# FOS Capacity & Border-to-Border Planning — Data Functions
# =============================================================================
# Static inventory / capacity data per FOS area for offseason 5-year planning:
#   - ground / air / prehatch sites + prehatch acres  (loc_breeding_sites)
#   - sections managed                                 (gis_sectcode)
#   - wet catch basins                                 (loc_catchbasin)
#   - techs (Inspectors) per FOS, FOS per facility     (employee_list)
#   - straight-line miles facility -> section centroid (PostGIS)
# All grouped by facility / fosarea / zone (P1 = '1', P2 = '2').
# =============================================================================

if (!exists("get_db_connection", mode = "function")) {
  source("../../shared/db_helpers.R")
}

# Prehatch site types (matches apps/ground_prehatch_progress/data_functions.R)
CAPACITY_PREHATCH_TYPES <- c("PREHATCH", "BRIQUET", "PELLET", "PRE1ONLY")
.prehatch_in_sql <- "('PREHATCH','BRIQUET','PELLET','PRE1ONLY')"

# 2001 Workload Study defaults (Ch. 11). The 5 larval-control job types, the
# district-mean hours per job (Table 11.3), and the avg hours a crew works
# air+ground insp/trt per season (Table 11.6). Hours are NOT in any database, so
# these are editable UI defaults. Job types map to dblarv_insptrt action codes.
JOB_TYPE_LABELS <- c(
  air_trt      = "Air treatment",       # actions A, D
  air_insp     = "Air inspection",      # action 4
  gnd_insp_trt = "Ground insp & treat", # action 1
  gnd_trt      = "Ground treatment",    # action 3
  gnd_insp     = "Ground inspection"    # action 2
)
STUDY_HOURS_PER_JOB <- c(air_trt = 1.35, air_insp = 0.66,
                         gnd_insp_trt = 1.08, gnd_trt = 0.55, gnd_insp = 0.17)
STUDY_CREW_HOURS_SEASON <- 2184
# Which site-count a job type draws from ("air" vs "ground" sites)
JOB_TYPE_SITEKIND <- c(air_trt = "air", air_insp = "air",
                       gnd_insp_trt = "ground", gnd_trt = "ground",
                       gnd_insp = "ground")

#' Jobs-per-distinct-site multipliers from ACTUAL treatment data, by
#' facility x zone x job_type, plus a DISTRICT roll-up. rate = jobs / distinct
#' sites serviced (matches Workload Study Table 11.4). Computed live so it beats
#' the stale 2001 numbers.
#'
#' @param year  Season to measure (default = last complete year)
#' @return data.frame: facility ("DISTRICT" for the roll-up), zone, job_type,
#'   jobs, distinct_sites, rate
load_jobs_per_site <- function(year = as.integer(format(Sys.Date(), "%Y")) - 1) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  year <- as.integer(year)

  q <- sprintf("
    WITH ins AS (
      SELECT i.sitecode, i.action FROM dblarv_insptrt_current i
      WHERE EXTRACT(YEAR FROM i.inspdate) = %d
        AND i.action IN ('1','2','3','4','A','D')
      UNION ALL
      SELECT i.sitecode, i.action FROM dblarv_insptrt_archive i
      WHERE EXTRACT(YEAR FROM i.inspdate) = %d
        AND i.action IN ('1','2','3','4','A','D')
    ),
    j AS (
      SELECT sc.facility, sc.zone,
             CASE ins.action
               WHEN '1' THEN 'gnd_insp_trt' WHEN '2' THEN 'gnd_insp'
               WHEN '3' THEN 'gnd_trt'      WHEN '4' THEN 'air_insp'
               WHEN 'A' THEN 'air_trt'      WHEN 'D' THEN 'air_trt'
             END AS job_type,
             ins.sitecode
      FROM ins
      JOIN gis_sectcode sc ON left(ins.sitecode, 7) = sc.sectcode
      WHERE sc.fosarea IS NOT NULL AND sc.fosarea <> '' AND sc.zone IN ('1','2')
    )
    SELECT COALESCE(facility, 'DISTRICT') AS facility, zone, job_type,
           COUNT(*)::int AS jobs,
           COUNT(DISTINCT sitecode)::int AS distinct_sites
    FROM j
    GROUP BY GROUPING SETS ((facility, zone, job_type), (zone, job_type))
  ", year, year)

  df <- tryCatch(dbGetQuery(con, q), error = function(e) {
    warning(paste("[load_jobs_per_site]", e$message)); data.frame()
  })
  if (nrow(df) > 0) {
    df$rate <- round(df$jobs / pmax(df$distinct_sites, 1), 2)
    df$year <- year
  }
  df
}

#' Section-grain capacity: one row per section with its site/catch-basin loads and
#' straight-line miles from its facility. This is the base grain for the capacity
#' table (aggregated up) AND the what-if section picker (used as-is).
#'
#' @return data.frame: facility, fosarea, zone, sectcode, ground_sites, air_sites,
#'   prehatch_sites, prehatch_acres, wet_cb, miles
load_capacity_sections <- function() {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)

  q <- sprintf("
    SELECT sc.facility, sc.fosarea, sc.zone, sc.sectcode,
           COALESCE(bs.ground_sites, 0)   AS ground_sites,
           COALESCE(bs.air_sites, 0)      AS air_sites,
           COALESCE(bs.prehatch_sites, 0) AS prehatch_sites,
           COALESCE(bs.prehatch_acres, 0) AS prehatch_acres,
           COALESCE(cb.wet_cb, 0)         AS wet_cb,
           CASE WHEN f.the_geom IS NOT NULL AND sc.latloncentroid IS NOT NULL
                THEN ST_Distance(
                       ST_Transform(ST_Centroid(f.the_geom), 4326)::geography,
                       ST_Transform(sc.latloncentroid, 4326)::geography) / 1609.34
                ELSE NULL END AS miles
    FROM gis_sectcode sc
    LEFT JOIN gis_facility f ON f.abbrv = sc.facility
    LEFT JOIN (
      SELECT left(b.sitecode, 7) AS sectcode,
             (COUNT(*) FILTER (WHERE b.air_gnd = 'G'))::int AS ground_sites,
             (COUNT(*) FILTER (WHERE b.air_gnd = 'A'))::int AS air_sites,
             (COUNT(*) FILTER (WHERE b.prehatch IN %s))::int AS prehatch_sites,
             COALESCE(SUM(b.acres) FILTER (WHERE b.prehatch IN %s), 0)::numeric AS prehatch_acres
      FROM loc_breeding_sites b
      WHERE b.enddate IS NULL
      GROUP BY left(b.sitecode, 7)
    ) bs ON bs.sectcode = sc.sectcode
    LEFT JOIN (
      SELECT left(cb.sitecode, 7) AS sectcode, COUNT(*)::int AS wet_cb
      FROM loc_catchbasin cb
      WHERE (cb.enddate IS NULL OR cb.enddate > CURRENT_DATE)
        AND cb.lettergrp::text <> 'Z'
        AND cb.status_udw = 'W'
      GROUP BY left(cb.sitecode, 7)
    ) cb ON cb.sectcode = sc.sectcode
    WHERE sc.fosarea IS NOT NULL AND sc.fosarea <> ''
      AND sc.zone IN ('1', '2')
    ORDER BY sc.facility, sc.fosarea, sc.zone, sc.sectcode
  ", .prehatch_in_sql, .prehatch_in_sql)

  df <- tryCatch(dbGetQuery(con, q), error = function(e) {
    warning(paste("[load_capacity_sections]", e$message)); data.frame()
  })
  if (nrow(df) > 0) {
    df$prehatch_acres <- round(as.numeric(df$prehatch_acres), 1)
    df$miles <- round(as.numeric(df$miles), 1)
  }
  df
}

#' Site-grain capacity broken down by priority (for the priority-breakdown view).
#'
#' @return data.frame: facility, fosarea, zone, priority, ground_sites, air_sites,
#'   prehatch_sites, prehatch_acres
load_capacity_by_priority <- function() {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)

  q <- sprintf("
    SELECT sc.facility, sc.fosarea, sc.zone,
           COALESCE(NULLIF(b.priority, ''), '(none)') AS priority,
           (COUNT(*) FILTER (WHERE b.air_gnd = 'G'))::int AS ground_sites,
           (COUNT(*) FILTER (WHERE b.air_gnd = 'A'))::int AS air_sites,
           (COUNT(*) FILTER (WHERE b.prehatch IN %s))::int AS prehatch_sites,
           COALESCE(SUM(b.acres) FILTER (WHERE b.prehatch IN %s), 0)::numeric AS prehatch_acres
    FROM loc_breeding_sites b
    JOIN gis_sectcode sc ON left(b.sitecode, 7) = sc.sectcode
    WHERE b.enddate IS NULL AND sc.fosarea IS NOT NULL AND sc.fosarea <> ''
      AND sc.zone IN ('1', '2')
    GROUP BY sc.facility, sc.fosarea, sc.zone,
             COALESCE(NULLIF(b.priority, ''), '(none)')
    ORDER BY sc.facility, sc.fosarea, sc.zone, priority
  ", .prehatch_in_sql, .prehatch_in_sql)

  df <- tryCatch(dbGetQuery(con, q), error = function(e) {
    warning(paste("[load_capacity_by_priority]", e$message)); data.frame()
  })
  if (nrow(df) > 0) df$prehatch_acres <- round(as.numeric(df$prehatch_acres), 1)
  df
}

#' Section polygons + centroids for one facility, for the interactive map.
#'
#' @param facility facility abbreviation
#' @return data.frame: sectcode, zone, fosarea, lat, lon, wkt (WGS84 polygon WKT)
load_section_geo <- function(facility) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  q <- sprintf("
    SELECT sc.sectcode, sc.zone, sc.fosarea,
           ST_Y(ST_Transform(sc.latloncentroid, 4326)) AS lat,
           ST_X(ST_Transform(sc.latloncentroid, 4326)) AS lon,
           ST_AsText(ST_Transform(sc.the_geom, 4326)) AS wkt
    FROM gis_sectcode sc
    WHERE sc.facility = '%s' AND sc.the_geom IS NOT NULL
      AND sc.fosarea IS NOT NULL AND sc.fosarea <> '' AND sc.zone IN ('1','2')
    ORDER BY sc.sectcode
  ", gsub("'", "''", facility))
  tryCatch(dbGetQuery(con, q), error = function(e) {
    warning(paste("[load_section_geo]", e$message)); data.frame()
  })
}

#' Staffing structure from employee_list.
#'
#' @return list(techs = df[fosarea, n_techs], fos_per_fac = df[facility, n_fos])
load_capacity_staffing <- function() {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)

  techs <- tryCatch(dbGetQuery(con, "
    SELECT fieldsuper AS fosarea, COUNT(*)::int AS n_techs
    FROM employee_list
    WHERE active = true AND emp_type = 'Inspector'
      AND fieldsuper IS NOT NULL AND fieldsuper <> ''
    GROUP BY fieldsuper
  "), error = function(e) data.frame(fosarea = character(), n_techs = integer()))

  fos_per_fac <- tryCatch(dbGetQuery(con, "
    SELECT facility, COUNT(*)::int AS n_fos
    FROM employee_list
    WHERE active = true AND emp_type = 'FieldSuper' AND facility IS NOT NULL
    GROUP BY facility
  "), error = function(e) data.frame(facility = character(), n_fos = integer()))

  list(techs = techs, fos_per_fac = fos_per_fac)
}

# -----------------------------------------------------------------------------
# Aggregation helpers (pure R — no DB)
# -----------------------------------------------------------------------------

#' Roll section-grain data up to one row per facility x fosarea x zone, add FOS
#' shortname, facility full name, and tech counts.
#'
#' @param sections  Output of load_capacity_sections()
#' @param staffing  Output of load_capacity_staffing()
#' @return data.frame per facility/fosarea/zone with the capacity columns
aggregate_capacity_by_fos <- function(sections, staffing) {
  if (is.null(sections) || nrow(sections) == 0) return(data.frame())

  agg <- sections %>%
    dplyr::group_by(facility, fosarea, zone) %>%
    dplyr::summarise(
      sections       = dplyr::n_distinct(sectcode),
      ground_sites   = sum(ground_sites, na.rm = TRUE),
      air_sites      = sum(air_sites, na.rm = TRUE),
      prehatch_sites = sum(prehatch_sites, na.rm = TRUE),
      prehatch_acres = round(sum(prehatch_acres, na.rm = TRUE), 1),
      wet_cb         = sum(wet_cb, na.rm = TRUE),
      avg_miles      = round(mean(miles, na.rm = TRUE), 1),
      max_miles      = round(max(miles, na.rm = TRUE), 1),
      .groups = "drop"
    )

  # Names + facility full name + techs
  foremen <- tryCatch(get_foremen_lookup(), error = function(e) data.frame())
  fac_lkp <- tryCatch(get_facility_lookup(), error = function(e) data.frame())
  fos_name <- if (nrow(foremen) > 0) {
    setNames(foremen$shortname, as.character(foremen$emp_num))
  } else character(0)
  fac_name <- if (nrow(fac_lkp) > 0) {
    setNames(fac_lkp$full_name, fac_lkp$short_name)
  } else character(0)

  agg$fos_name <- ifelse(as.character(agg$fosarea) %in% names(fos_name),
                         fos_name[as.character(agg$fosarea)],
                         paste0("FOS ", agg$fosarea))
  agg$facility_name <- ifelse(agg$facility %in% names(fac_name),
                              fac_name[agg$facility], agg$facility)

  techs <- staffing$techs
  tech_map <- if (!is.null(techs) && nrow(techs) > 0) {
    setNames(techs$n_techs, as.character(techs$fosarea))
  } else integer(0)
  agg$n_techs <- ifelse(as.character(agg$fosarea) %in% names(tech_map),
                        as.integer(tech_map[as.character(agg$fosarea)]), 0L)

  agg %>% dplyr::arrange(facility, fos_name, zone)
}

#' Averages of the capacity columns across FOS areas, for a given zone.
#' Returns per-facility means and a district-wide mean.
#'
#' @param cap  Output of aggregate_capacity_by_fos()
#' @param zone_val  "1" or "2"
#' @return data.frame: scope ("District" or facility), then mean of each metric
capacity_averages <- function(cap, zone_val) {
  if (is.null(cap) || nrow(cap) == 0) return(data.frame())
  z <- cap[cap$zone == zone_val, , drop = FALSE]
  if (nrow(z) == 0) return(data.frame())

  metric_cols <- c("sections", "ground_sites", "air_sites", "prehatch_sites",
                   "prehatch_acres", "wet_cb", "avg_miles", "n_techs")

  per_fac <- z %>%
    dplyr::group_by(scope = facility) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(metric_cols),
                                   ~ round(mean(.x, na.rm = TRUE), 1)),
                     n_fos = dplyr::n(), .groups = "drop")

  district <- z %>%
    dplyr::summarise(scope = "DISTRICT",
                     dplyr::across(dplyr::all_of(metric_cols),
                                   ~ round(mean(.x, na.rm = TRUE), 1)),
                     n_fos = dplyr::n())

  dplyr::bind_rows(district, per_fac)
}
