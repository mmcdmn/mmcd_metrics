# FOS Capacity & Border-to-Border Planning — Data Functions
# =============================================================================
# Static inventory / capacity data per FOS area for offseason 5-year planning:
#   - ground / air / prehatch sites + prehatch acres  (loc_breeding_sites)
#   - sections managed                                 (gis_sectcode)
#   - wet catch basins                                 (loc_catchbasin)
#   - techs per crew / crews / drone techs per facility (config defaults, user-editable)
#   - straight-line miles facility office -> section centroid
# All grouped by facility / fosarea / zone (P1 = '1', P2 = '2').
# =============================================================================

if (!exists("get_db_connection", mode = "function")) {
  source("../../shared/db_helpers.R")
}

# Prehatch site types (matches apps/ground_prehatch_progress/data_functions.R)
CAPACITY_PREHATCH_TYPES <- c("PREHATCH", "BRIQUET", "PELLET", "PRE1ONLY")
.prehatch_in_sql <- "('PREHATCH','BRIQUET','PELLET','PRE1ONLY')"

# Facility office locations used by routed and straight-line drive estimates.
FACILITY_LOCATION_FILE <- "data/facility_locations.csv"

load_facility_locations <- function(path = FACILITY_LOCATION_FILE) {
  if (!file.exists(path)) stop("Missing facility locations file: ", path)
  locations <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- c("facility", "lon", "lat")
  if (!all(required %in% names(locations)) || anyDuplicated(locations$facility) ||
      any(!is.finite(locations$lon)) || any(!is.finite(locations$lat)) ||
      any(locations$lon < -180 | locations$lon > 180 |
          locations$lat < -90 | locations$lat > 90)) {
    stop("Invalid facility locations in ", path)
  }
  locations
}

.haversine_miles <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 3958.7613 * asin(pmin(1, sqrt(a)))
}

# 2001 Workload Study defaults (Ch. 11). The 5 larval-control job types, the
# district-mean hours per job (Table 11.3), and the avg hours a crew works
# air+ground insp/trt per season (Table 11.6). Hours are NOT in any database, so
# these are editable UI defaults. Job types map to dblarv_insptrt action codes.
JOB_TYPE_LABELS <- c(
  air_trt      = "Air treatment",       # action A only (drone 'D' is a GROUND-site treatment, not air)
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

# Planning assumption (NOT a 2001-study value): how many round trips a crew
# drives to a given section over a season. Used by the What-If crew estimate to
# turn straight-line facility->section miles into seasonal travel hours. Editable
# in the UI; set to 0 to drop travel entirely (e.g. if the study hours-per-job
# already bake in travel).
DEFAULT_TRIPS_PER_SECTION <- 8

# ROM-confirmed district staffing (used if config/app_config.yaml can't be read;
# keep in sync with capacity_planner.staffing there). Crew = 1 FOS + ground techs.
CAPACITY_STAFFING_FALLBACK <- data.frame(
  facility          = c("Sr", "Sj", "Wp", "N", "E", "Wm"),
  crews             = c(4, 4, 5, 6, 7, 5),
  techs_per_crew    = c(5, 6, 5, 40 / 6, 40 / 7, 5),
  drone_techs       = c(2, 2, 4, 4, 3, 4),
  drone_offload_pct = c(0, 0, 0, 0, 0, 0),
  drone_air_offload_pct = c(0, 0, 0, 0, 0, 0),
  stringsAsFactors = FALSE
)

#' Jobs-per-distinct-site multipliers from ACTUAL treatment data, by
#' facility x zone x job_type, plus a DISTRICT roll-up. rate = jobs / distinct
#' sites serviced (matches Workload Study Table 11.4). Drone treatments (action
#' 'D') are deliberately excluded: they are done on GROUND sites by drone techs,
#' not air treatments, so they must not inflate the air-treatment rate. Computed live so it beats
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
        AND i.action IN ('1','2','3','4','A')
      UNION ALL
      SELECT i.sitecode, i.action FROM dblarv_insptrt_archive i
      WHERE EXTRACT(YEAR FROM i.inspdate) = %d
        AND i.action IN ('1','2','3','4','A')
    ),
    j AS (
      SELECT sc.facility, sc.zone,
             CASE ins.action
               WHEN '1' THEN 'gnd_insp_trt' WHEN '2' THEN 'gnd_insp'
               WHEN '3' THEN 'gnd_trt'      WHEN '4' THEN 'air_insp'
               WHEN 'A' THEN 'air_trt'
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

# Sites inside Restricted Areas with monitor_ok = FALSE (no work there), built offline by
# scripts/build_ra_exclusions.R from extra/qgis/ra_view.gpkg. Dropped from all site/acre loads.
RA_EXCLUSION_FILE <- "data/ra_excluded_sites.csv"

# Values read from a file are kept per R process (workers outlive sessions), keyed by the file's
# content hash, so only the first session after a start or an edit pays for the read / parse.
.capacity_memo <- new.env(parent = emptyenv())
.memo_file <- function(tag, path, build) {
  key <- tryCatch(unname(tools::md5sum(path)), error = function(e) NA_character_)
  if (is.na(key)) return(build())
  hit <- .capacity_memo[[tag]]
  if (!is.null(hit) && identical(hit$key, key)) return(hit$value)
  v <- build()
  assign(tag, list(key = key, value = v), envir = .capacity_memo)
  v
}

#' SQL fragment (" AND NOT (...)") that removes RA monitor_ok = FALSE sites; "" when the
#' lookup file is missing. Sites stay with their section's FOS (gis_sectcode.fosarea); the
#' RA layer's e_restrctspec employee numbers are not used.
#'
#' @param alias table alias of loc_breeding_sites in the query
ra_exclusion_sql <- function(alias = "b", path = RA_EXCLUSION_FILE) {
  if (!file.exists(path)) return("")
  codes <- .memo_file(paste0("ra:", path), path, function() {
    d <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character"),
                  error = function(e) NULL)
    if (is.null(d) || !"sitecode" %in% names(d)) return("")
    cs <- unique(d$sitecode[grepl("^[0-9A-Za-z-]+$", d$sitecode)])
    paste(cs, collapse = ",")
  })
  if (!nzchar(codes)) return("")
  sprintf(" AND NOT (%s.sitecode = ANY(string_to_array('%s', ',')))", alias, codes)
}

# ---- Shared cache for the heavy section / priority queries -------------------------------
# Redis (shared by every worker and session, see shared/redis_cache.R) holds the results for
# CAPACITY_CACHE_TTL (config cache.ttl.capacity_planner, default 24 h), so a visit does not re-run
# the two aggregates over loc_breeding_sites / loc_catchbasin. Bump CAPACITY_CACHE_VERSION when a
# query's columns change; the key also carries a fingerprint of the RA exclusion file and the
# prehatch types, so editing either never serves stale numbers.
CAPACITY_CACHE_VERSION <- 1L

capacity_cache_ttl <- function() {
  if (!exists("get_app_config", mode = "function") && file.exists("../../shared/config.R")) {
    source("../../shared/config.R")
  }
  v <- tryCatch(get_app_config()$cache$ttl$capacity_planner, error = function(e) NULL)
  if (is.null(v) || !is.numeric(v) || length(v) != 1 || !is.finite(v) || v <= 0) 86400L else as.integer(v)
}

#' list(get, set) backed by Redis, or NULL when Redis is not available (then nothing is cached).
capacity_cache_store <- function() {
  ok <- exists("redis_is_active", mode = "function") && exists("redis_get", mode = "function") &&
    exists("redis_set", mode = "function") &&
    isTRUE(tryCatch(redis_is_active(), error = function(e) FALSE))
  if (!ok) return(NULL)
  list(get = redis_get, set = redis_set)
}

capacity_cache_fingerprint <- function(path = RA_EXCLUSION_FILE) {
  md5 <- if (file.exists(path)) tryCatch(unname(tools::md5sum(path)), error = function(e) "unreadable") else "none"
  substr(rlang::hash(list(md5, .prehatch_in_sql)), 1, 8)
}

#' Run `loader` through the shared cache.
#'
#' A hit returns the stored value without touching the database. A miss (or `refresh = TRUE`)
#' calls the loader and stores the result for everyone, except an empty / NULL result, which is
#' returned but never stored (the loaders return an empty table when the database is
#' unreachable, and that must not stick for a day). No store (Redis down) just loads.
#'
#' @param name   short name of the data set ("sections", "priority", ...)
#' @param loader function() -> data.frame
#' @param store  list(get = function(key), set = function(key, value, ttl)) or NULL
#' @return list(data, at = POSIXct the data was read, cached = TRUE for a hit)
capacity_cached <- function(name, loader, refresh = FALSE, ttl = capacity_cache_ttl(),
                            store = capacity_cache_store(),
                            fingerprint = capacity_cache_fingerprint()) {
  key <- sprintf("capacity:%s:v%d:%s", name, CAPACITY_CACHE_VERSION, fingerprint)
  t0 <- Sys.time()
  if (!is.null(store) && !isTRUE(refresh)) {
    hit <- tryCatch(store$get(key), error = function(e) NULL)
    if (is.list(hit) && !is.null(hit$data) && inherits(hit$at, "POSIXct")) {
      message(sprintf("[capacity_planner] cache HIT  %-9s (%.0f min old, %.2fs)", name,
                      as.numeric(difftime(Sys.time(), hit$at, units = "mins")),
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      return(list(data = hit$data, at = hit$at, cached = TRUE))
    }
  }
  data <- loader()
  at <- Sys.time()
  stored <- FALSE
  if (!is.null(store) && !is.null(data) && NROW(data) > 0) {
    stored <- isTRUE(tryCatch({ store$set(key, list(data = data, at = at), ttl); TRUE },
                              error = function(e) FALSE))
  }
  message(sprintf("[capacity_planner] cache MISS %-9s (%s, %.2fs)", name,
                  if (stored) "stored" else "not stored",
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  list(data = data, at = at, cached = FALSE)
}

#' Fill (or refresh) the shared cache; run at server start so the first visitor gets a hit.
warm_capacity_cache <- function(refresh = FALSE) {
  s <- capacity_cached("sections", load_capacity_sections, refresh = refresh)
  p <- capacity_cached("priority", load_capacity_by_priority, refresh = refresh)
  capacity_cached("geo", load_section_geo, ttl = 7L * 86400L, refresh = refresh)   # What-If map shapes
  invisible(list(sections = NROW(s$data), priority = NROW(p$data)))
}

#' Section-grain capacity: one row per section with its site/catch-basin loads and
#' straight-line miles from its facility office. This is the base grain for the capacity
#' table (aggregated up) AND the what-if section picker (used as-is).
#'
#' @return data.frame: facility, fosarea, zone, sectcode, ground_sites, air_sites,
#'   ground_acres, air_acres, prehatch_sites, prehatch_acres, wet_cb, miles
load_capacity_sections <- function() {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)

  q <- sprintf("
    SELECT sc.facility, sc.fosarea, sc.zone, sc.sectcode,
           COALESCE(bs.ground_sites, 0)   AS ground_sites,
           COALESCE(bs.air_sites, 0)      AS air_sites,
           COALESCE(bs.ground_acres, 0)   AS ground_acres,
           COALESCE(bs.air_acres, 0)      AS air_acres,
           COALESCE(bs.prehatch_sites, 0) AS prehatch_sites,
           COALESCE(bs.prehatch_acres, 0) AS prehatch_acres,
           COALESCE(cb.wet_cb, 0)         AS wet_cb,
           ST_X(ST_Transform(sc.latloncentroid, 4326)) AS section_lon,
           ST_Y(ST_Transform(sc.latloncentroid, 4326)) AS section_lat
    FROM gis_sectcode sc
    LEFT JOIN (
      SELECT left(b.sitecode, 7) AS sectcode,
             (COUNT(*) FILTER (WHERE b.air_gnd = 'G'))::int AS ground_sites,
             (COUNT(*) FILTER (WHERE b.air_gnd = 'A'))::int AS air_sites,
             COALESCE(SUM(b.acres) FILTER (WHERE b.air_gnd = 'G'), 0)::numeric AS ground_acres,
             COALESCE(SUM(b.acres) FILTER (WHERE b.air_gnd = 'A'), 0)::numeric AS air_acres,
             (COUNT(*) FILTER (WHERE b.prehatch IN %s))::int AS prehatch_sites,
             COALESCE(SUM(b.acres) FILTER (WHERE b.prehatch IN %s), 0)::numeric AS prehatch_acres
      FROM loc_breeding_sites b
      WHERE b.enddate IS NULL %s
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
  ", .prehatch_in_sql, .prehatch_in_sql, ra_exclusion_sql("b"))

  df <- tryCatch(dbGetQuery(con, q), error = function(e) {
    warning(paste("[load_capacity_sections]", e$message)); data.frame()
  })
  if (nrow(df) > 0) {
    locations <- load_facility_locations()
    location_idx <- match(df$facility, locations$facility)
    if (anyNA(location_idx)) {
      stop("No office coordinates for facility: ",
           paste(unique(df$facility[is.na(location_idx)]), collapse = ", "))
    }
    df$miles <- .haversine_miles(
      locations$lon[location_idx], locations$lat[location_idx],
      df$section_lon, df$section_lat
    )
    df$section_lon <- NULL
    df$section_lat <- NULL
    for (col in c("ground_acres", "air_acres", "prehatch_acres")) {
      df[[col]] <- round(as.numeric(df[[col]]), 3)   # round after summing, not per section
    }
    df$miles <- round(as.numeric(df$miles), 1)
  }
  df
}

#' Site-grain capacity broken down by priority (for the priority-breakdown view).
#'
#' @return data.frame: facility, fosarea, zone, priority, ground_sites, air_sites,
#'   ground_acres, air_acres, prehatch_sites, prehatch_acres
load_capacity_by_priority <- function() {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)

  q <- sprintf("
    SELECT sc.facility, sc.fosarea, sc.zone,
           COALESCE(NULLIF(b.priority, ''), '(none)') AS priority,
           (COUNT(*) FILTER (WHERE b.air_gnd = 'G'))::int AS ground_sites,
           (COUNT(*) FILTER (WHERE b.air_gnd = 'A'))::int AS air_sites,
           COALESCE(SUM(b.acres) FILTER (WHERE b.air_gnd = 'G'), 0)::numeric AS ground_acres,
           COALESCE(SUM(b.acres) FILTER (WHERE b.air_gnd = 'A'), 0)::numeric AS air_acres,
           (COUNT(*) FILTER (WHERE b.prehatch IN %s))::int AS prehatch_sites,
           COALESCE(SUM(b.acres) FILTER (WHERE b.prehatch IN %s), 0)::numeric AS prehatch_acres
    FROM loc_breeding_sites b
    JOIN gis_sectcode sc ON left(b.sitecode, 7) = sc.sectcode
    WHERE b.enddate IS NULL AND sc.fosarea IS NOT NULL AND sc.fosarea <> ''
      AND sc.zone IN ('1', '2') %s
    GROUP BY sc.facility, sc.fosarea, sc.zone,
             COALESCE(NULLIF(b.priority, ''), '(none)')
    ORDER BY sc.facility, sc.fosarea, sc.zone, priority
  ", .prehatch_in_sql, .prehatch_in_sql, ra_exclusion_sql("b"))

  df <- tryCatch(dbGetQuery(con, q), error = function(e) {
    warning(paste("[load_capacity_by_priority]", e$message)); data.frame()
  })
  if (nrow(df) > 0) {
    for (col in c("ground_acres", "air_acres", "prehatch_acres")) {
      df[[col]] <- round(as.numeric(df[[col]]), 3)   # round after summing, not per section
    }
  }
  df
}

# Routed drive-time lookup built offline by scripts/build_drivetimes.R (OSRM on
# OpenStreetMap roads). Free-flow times, facility office -> section centroid.
ROUTED_DRIVETIME_FILE <- "data/facility_section_drivetime.csv"

#' Read the routed facility -> section drive-time lookup.
#'
#' @param path CSV written by scripts/build_drivetimes.R
#' The file holds EVERY facility x section pair (so a section moved to another facility
#' in the What-If planner has a time from its new facility), plus the straight-line miles.
#'
#' @return data.frame(facility, sectcode, drive_min_routed, drive_miles_routed,
#'   snap_m, snap_flag, straight_miles), or NULL when the file is missing/unreadable (the
#'   app then falls back to the straight-line estimate)
load_routed_drivetimes <- function(path = ROUTED_DRIVETIME_FILE) {
  if (!file.exists(path)) return(NULL)
  .memo_file(paste0("routed:", path), path, function() {   # parsed once per R process
    d <- tryCatch(suppressWarnings(utils::read.csv(path, stringsAsFactors = FALSE,
                                                   colClasses = c(sectcode = "character"))),
                  error = function(e) { warning(paste("[load_routed_drivetimes]", e$message)); NULL })
    need <- c("facility", "sectcode", "drive_min", "drive_miles")
    if (is.null(d) || !all(need %in% names(d))) return(NULL)
    if (!"snap_m" %in% names(d)) d$snap_m <- NA_real_
    if (!"snap_flag" %in% names(d)) d$snap_flag <- !is.na(d$snap_m) & d$snap_m > 1000
    if (!"straight_miles" %in% names(d)) d$straight_miles <- NA_real_
    names(d)[names(d) == "drive_min"]   <- "drive_min_routed"
    names(d)[names(d) == "drive_miles"] <- "drive_miles_routed"
    d[, c("facility", "sectcode", "drive_min_routed", "drive_miles_routed", "snap_m",
          "snap_flag", "straight_miles")]
  })
}

#' Add the routed drive-time columns to the section-grain table. Sections with no
#' routed row get NA (section_drive_minutes() falls back to the estimate for them).
attach_routed_drivetimes <- function(sections, routed = load_routed_drivetimes()) {
  if (is.null(sections) || nrow(sections) == 0) return(sections)
  if (is.null(routed)) {
    sections$drive_min_routed <- NA_real_; sections$drive_miles_routed <- NA_real_
    sections$snap_m <- NA_real_; sections$snap_flag <- NA
    return(sections)
  }
  idx <- match(paste(sections$facility, sections$sectcode), paste(routed$facility, routed$sectcode))
  sections$drive_min_routed   <- routed$drive_min_routed[idx]
  sections$drive_miles_routed <- routed$drive_miles_routed[idx]
  sections$snap_m             <- routed$snap_m[idx]
  sections$snap_flag          <- routed$snap_flag[idx]
  sections
}

#' Section polygons + centroids for the interactive map: one facility, or every
#' facility when `facility` is NULL (the What-If planner moves sections between
#' facilities). The all-facilities polygons are simplified (about 30 m) to keep the
#' payload and phone rendering light.
#'
#' @param facility facility abbreviation, or NULL for all
#' @return data.frame: sectcode, facility, zone, fosarea, lat, lon, wkt (WGS84 polygon WKT)
load_section_geo <- function(facility = NULL) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  where_fac <- if (is.null(facility)) "" else
    sprintf("sc.facility = '%s' AND ", gsub("'", "''", facility))
  geom <- if (is.null(facility)) {
    "ST_SimplifyPreserveTopology(ST_Transform(sc.the_geom, 4326), 0.0003)"
  } else "ST_Transform(sc.the_geom, 4326)"
  q <- sprintf("
    SELECT sc.sectcode, sc.facility, sc.zone, sc.fosarea,
           ST_Y(ST_Transform(sc.latloncentroid, 4326)) AS lat,
           ST_X(ST_Transform(sc.latloncentroid, 4326)) AS lon,
           ST_AsText(%s) AS wkt
    FROM gis_sectcode sc
    WHERE %ssc.the_geom IS NOT NULL
      AND sc.fosarea IS NOT NULL AND sc.fosarea <> '' AND sc.zone IN ('1','2')
    ORDER BY sc.sectcode
  ", geom, where_fac)
  tryCatch(dbGetQuery(con, q), error = function(e) {
    warning(paste("[load_section_geo]", e$message)); data.frame()
  })
}

#' District staffing defaults (replaces the unreliable employee_list.active
#' count). Read from config/app_config.yaml (capacity_planner.staffing); falls
#' back to CAPACITY_STAFFING_FALLBACK if the config is missing or unreadable.
#' One crew = 1 FOS area + its ground techs. Drone techs are facility-level and
#' are NOT counted in techs_per_crew.
#'
#' @return data.frame: facility, crews, techs_per_crew, drone_techs, drone_offload_pct,
#'   drone_air_offload_pct
default_staffing <- function() {
  if (!exists("get_app_config", mode = "function") &&
      file.exists("../../shared/config.R")) {
    source("../../shared/config.R")
  }
  cfg <- tryCatch(get_app_config()$capacity_planner$staffing, error = function(e) NULL)
  if (is.null(cfg) || length(cfg) == 0) return(CAPACITY_STAFFING_FALLBACK)

  num <- function(x, d) if (is.null(x) || is.na(suppressWarnings(as.numeric(x)))) d else as.numeric(x)
  df <- data.frame(
    facility          = names(cfg),
    crews             = vapply(cfg, function(x) num(x$crews, 0), numeric(1)),
    techs_per_crew    = vapply(cfg, function(x) num(x$techs_per_crew, 0), numeric(1)),
    drone_techs       = vapply(cfg, function(x) num(x$drone_techs, 0), numeric(1)),
    drone_offload_pct = vapply(cfg, function(x) num(x$drone_offload_pct, 0), numeric(1)),
    drone_air_offload_pct = vapply(cfg, function(x) num(x$drone_air_offload_pct, 0), numeric(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
  df
}

#' Number of FOS areas the DB has per facility (from section-grain data). Used
#' only as a cross-check against the configured crew counts (1 crew = 1 FOS area).
#'
#' @param sections Output of load_capacity_sections()
#' @return data.frame: facility, db_fos_areas
db_fos_area_counts <- function(sections) {
  if (is.null(sections) || nrow(sections) == 0) {
    return(data.frame(facility = character(), db_fos_areas = integer()))
  }
  agg <- stats::aggregate(fosarea ~ facility, data = sections,
                          FUN = function(x) length(unique(x)))
  names(agg)[2] <- "db_fos_areas"
  agg
}

# -----------------------------------------------------------------------------
# Aggregation helpers (pure R — no DB)
# -----------------------------------------------------------------------------

#' Roll section-grain data up to one row per facility x fosarea x zone, add FOS
#' shortname, facility full name, and tech counts.
#'
#' @param sections  Output of load_capacity_sections()
#' @param staffing  Staffing table (default_staffing() or the user's edited copy)
#' @return data.frame per facility/fosarea/zone with the capacity columns
aggregate_capacity_by_fos <- function(sections, staffing) {
  if (is.null(sections) || nrow(sections) == 0) return(data.frame())

  agg <- sections %>%
    dplyr::group_by(facility, fosarea, zone) %>%
    dplyr::summarise(
      sections       = dplyr::n_distinct(sectcode),
      ground_sites   = sum(ground_sites, na.rm = TRUE),
      air_sites      = sum(air_sites, na.rm = TRUE),
      ground_acres   = round(sum(ground_acres, na.rm = TRUE), 1),
      air_acres      = round(sum(air_acres, na.rm = TRUE), 1),
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

  # Staffing is set per facility (1 crew = 1 FOS area), so every FOS area in a
  # facility gets that facility's techs per crew. Drone techs/offload stay
  # facility-level (repeated on each FOS row).
  idx <- match(agg$facility, staffing$facility)
  agg$n_techs           <- ifelse(is.na(idx), 0, staffing$techs_per_crew[idx])
  agg$drone_techs       <- ifelse(is.na(idx), 0, staffing$drone_techs[idx])
  agg$drone_offload_pct <- ifelse(is.na(idx), 0, staffing$drone_offload_pct[idx])
  agg$drone_air_offload_pct <- ifelse(is.na(idx), 0, staffing$drone_air_offload_pct[idx])

  agg %>% dplyr::arrange(facility, fos_name, zone)
}

#' Mean AND median of the capacity columns across FOS areas, for a given zone.
#' Returns per-facility rows and a district-wide row (DISTRICT is first).
#'
#' @param cap  Output of aggregate_capacity_by_fos()
#' @param zone_val  "1" or "2"
#' @param metrics  metric columns to summarise (default CAPACITY_ZONE_METRICS);
#'   n_techs is always added
#' @return data.frame: scope, n_fos, then <metric>_mean and <metric>_median
capacity_averages <- function(cap, zone_val, metrics = NULL) {
  if (is.null(cap) || nrow(cap) == 0) return(data.frame())
  z <- cap[cap$zone == zone_val, , drop = FALSE]
  if (nrow(z) == 0) return(data.frame())

  if (is.null(metrics)) metrics <- CAPACITY_ZONE_METRICS
  metric_cols <- c(metrics, "n_techs")
  fns <- list(mean   = ~ round(mean(.x, na.rm = TRUE), 1),
              median = ~ round(stats::median(.x, na.rm = TRUE), 1))

  per_fac <- z %>%
    dplyr::group_by(scope = facility) %>%
    dplyr::summarise(n_fos = dplyr::n(),
                     dplyr::across(dplyr::all_of(metric_cols), fns,
                                   .names = "{.col}_{.fn}"),
                     .groups = "drop")

  district <- z %>%
    dplyr::summarise(scope = "DISTRICT", n_fos = dplyr::n(),
                     dplyr::across(dplyr::all_of(metric_cols), fns,
                                   .names = "{.col}_{.fn}"))

  dplyr::bind_rows(district, per_fac)
}

#' One row per FOS area for a box plot: a DISTRICT group (all FOS areas) plus one
#' group per facility. Each point is a FOS area.
#'
#' @param cap  Output of aggregate_capacity_by_fos() (already facility-filtered)
#' @param zone_val  "1" or "2"
#' @param metric  a column of `cap` (e.g. "ground_sites", "air_acres")
#' @return data.frame: group (factor, DISTRICT first), value, fos_name
capacity_boxplot_data <- function(cap, zone_val, metric) {
  if (is.null(cap) || nrow(cap) == 0 || !metric %in% names(cap)) return(data.frame())
  z <- cap[cap$zone == zone_val, , drop = FALSE]
  if (nrow(z) == 0) return(data.frame())
  fac <- data.frame(group = z$facility, value = as.numeric(z[[metric]]),
                    fos_name = z$fos_name, stringsAsFactors = FALSE)
  df <- rbind(transform(fac, group = "DISTRICT"), fac)
  df$group <- factor(df$group, levels = c("DISTRICT", sort(unique(z$facility))))
  df[!is.na(df$value), , drop = FALSE]
}
