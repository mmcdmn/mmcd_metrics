# FOS Capacity & Border-to-Border Planning — Display & What-If logic
# =============================================================================
# Pure functions (no Shiny, no DB) that shape the aggregated capacity data for
# display and run the what-if P2 -> P1 promotion / staffing math. Kept separate
# so they can be unit-tested against the live data loaders.
# =============================================================================

# Every site count is paired with an acres figure (ground, air, prehatch).
# Sections and wet catch basins are not sites with acres, so they are count-only.
CAPACITY_ZONE_METRICS <- c("sections", "ground_sites", "ground_acres",
                           "air_sites", "air_acres",
                           "prehatch_sites", "prehatch_acres", "wet_cb",
                           "avg_miles")
CAPACITY_SITE_METRICS <- c("ground_sites", "air_sites", "prehatch_sites")
CAPACITY_ACRE_METRICS <- c("ground_acres", "air_acres", "prehatch_acres")

CAPACITY_METRIC_LABELS <- c(
  sections = "Sections", ground_sites = "Ground sites", ground_acres = "Ground acres",
  air_sites = "Air sites", air_acres = "Air acres",
  prehatch_sites = "Prehatch sites", prehatch_acres = "Prehatch acres",
  wet_cb = "Wet catch basins", avg_miles = "Avg miles", n_techs = "Techs",
  max_miles = "Max miles")

#' Metrics shown for the sites / acres / both toggle (count-only metrics always show).
capacity_visible_metrics <- function(measure = "both") {
  m <- CAPACITY_ZONE_METRICS
  if (identical(measure, "sites")) return(setdiff(m, CAPACITY_ACRE_METRICS))
  if (identical(measure, "acres")) return(setdiff(m, CAPACITY_SITE_METRICS))
  m
}

#' Drop the hidden-measure columns from a table whose columns are metric names,
#' optionally with a _P1/_P2/_mean/_median suffix.
select_measure_cols <- function(df, measure = "both") {
  base_nm <- sub("(_P[12]|_mean|_median)$", "", names(df))
  hide <- setdiff(CAPACITY_ZONE_METRICS, capacity_visible_metrics(measure))
  df[, !(base_nm %in% hide), drop = FALSE]
}

#' Human-readable column label, e.g. "ground_acres_P1" -> "Ground acres P1".
capacity_col_label <- function(nm) {
  special <- c(facility_name = "Facility", facility = "Facility", fos_name = "FOS",
               fosarea = "FOS area", scope = "Scope", n_fos = "FOS areas",
               drive_min = "Drive min")
  if (nm %in% names(special)) return(unname(special[nm]))
  suffix <- ""
  for (sfx in c("_P1", "_P2", "_mean", "_median")) {
    if (endsWith(nm, sfx)) {
      suffix <- paste0(" ", substring(sfx, 2))
      nm <- substr(nm, 1, nchar(nm) - nchar(sfx))
      break
    }
  }
  lab <- if (nm %in% names(CAPACITY_METRIC_LABELS)) CAPACITY_METRIC_LABELS[[nm]] else
         if (nm %in% names(special)) special[[nm]] else nm
  paste0(lab, suffix)
}

#' Workload-study crew estimate (2001 Ch. 11 method): convert the promoted
#' sections' site counts into seasonal jobs, then on-site hours, ADD seasonal
#' travel (drive-time) hours, then crews.
#'   jobs     = site_count(kind) x jobs-per-site rate
#'   on-site  = jobs x hours-per-job
#'   travel   = sum_section( round trips/season x 2 x one-way drive hours )
#'   crews    = (on-site + travel) hours / hours-per-crew-season
#'
#' @param promoted_sections section-grain df (ground_sites, air_sites, miles) being added
#' @param rates          named vector job_type -> jobs-per-distinct-site (P1)
#' @param hours_per_job  named vector job_type -> hours (editable; default study)
#' @param hrs_per_crew   hours a crew works air+gnd insp/trt per season (def 2184)
#' @param trips_per_section round trips a crew drives to each section per season
#'   (editable planning assumption; 0 disables the travel term)
#' @param avg_mph, circuity  drive-time estimate params (see est_drive_minutes)
#' @return list(by_job = df, totals = list) — totals$hours is the FULL pool
#'   (on-site + travel); onsite_hours and travel_hours are broken out.
estimate_crews <- function(promoted_sections, rates,
                           hours_per_job = STUDY_HOURS_PER_JOB,
                           hrs_per_crew = STUDY_CREW_HOURS_SEASON,
                           trips_per_section = DEFAULT_TRIPS_PER_SECTION,
                           avg_mph = 45, circuity = 1.3) {
  jt <- names(JOB_TYPE_LABELS)
  n_sec   <- if (!is.null(promoted_sections)) nrow(promoted_sections) else 0
  g_sites <- if (n_sec > 0) sum(promoted_sections$ground_sites, na.rm = TRUE) else 0
  a_sites <- if (n_sec > 0) sum(promoted_sections$air_sites, na.rm = TRUE) else 0

  rate_v <- ifelse(jt %in% names(rates), as.numeric(rates[jt]), 0)
  hpj_v  <- ifelse(jt %in% names(hours_per_job), as.numeric(hours_per_job[jt]), 0)
  base_v <- ifelse(JOB_TYPE_SITEKIND[jt] == "air", a_sites, g_sites)

  jobs  <- round(base_v * rate_v)
  hours <- round(jobs * hpj_v, 1)

  by_job <- data.frame(
    Job        = unname(JOB_TYPE_LABELS[jt]),
    Sites      = base_v,
    `Jobs/site` = round(rate_v, 2),
    Jobs       = jobs,
    `Hrs/job`  = hpj_v,
    Hours      = hours,
    check.names = FALSE, stringsAsFactors = FALSE
  )

  onsite_hours <- sum(hours, na.rm = TRUE)

  # Seasonal travel: each section is visited `trips_per_section` times (round
  # trips), so travel hrs = trips x 2 x one-way drive hours, summed over sections.
  travel_hours <- 0
  if (n_sec > 0 && !is.null(promoted_sections$miles) && trips_per_section > 0) {
    one_way_hrs  <- est_drive_minutes(promoted_sections$miles, avg_mph, circuity) / 60
    travel_hours <- sum(trips_per_section * 2 * one_way_hrs, na.rm = TRUE)
  }

  total_hours <- onsite_hours + travel_hours
  crews <- if (hrs_per_crew > 0) total_hours / hrs_per_crew else NA_real_

  list(
    by_job = by_job,
    totals = list(
      ground_sites = g_sites, air_sites = a_sites,
      n_sections   = n_sec,
      jobs         = sum(jobs, na.rm = TRUE),
      onsite_hours = round(onsite_hours, 1),
      travel_hours = round(travel_hours, 1),
      hours        = round(total_hours, 1),
      hrs_per_crew = hrs_per_crew,
      crews        = round(crews, 2)
    )
  )
}

#' Estimated one-way drive minutes from straight-line miles.
#' minutes = miles * circuity / avg_mph * 60
est_drive_minutes <- function(miles, avg_mph = 45, circuity = 1.3) {
  round(miles * circuity / avg_mph * 60, 1)
}

#' Pivot the per-FOS x zone capacity to one wide row per FOS area, with P1/P2
#' columns for each zone-specific metric. Techs are per-FOS (not zone-specific).
#'
#' @param cap  Output of aggregate_capacity_by_fos()
#' @return wide data.frame, one row per facility/fosarea
capacity_wide <- function(cap) {
  if (is.null(cap) || nrow(cap) == 0) return(data.frame())

  long <- cap[, c("facility", "facility_name", "fos_name", "fosarea",
                  "n_techs", "zone", CAPACITY_ZONE_METRICS)]

  w <- tidyr::pivot_wider(
    long,
    id_cols     = c("facility", "facility_name", "fos_name", "fosarea", "n_techs"),
    names_from  = "zone",
    values_from = dplyr::all_of(CAPACITY_ZONE_METRICS),
    names_glue  = "{.value}_P{zone}",
    values_fill = 0
  )

  # avg_miles filled with 0 for a zone the FOS doesn't cover is misleading -> NA
  for (col in c("avg_miles_P1", "avg_miles_P2")) {
    if (col %in% names(w)) w[[col]][w[[col]] == 0] <- NA
  }
  as.data.frame(w) %>% dplyr::arrange(facility, fos_name)
}
# What drone techs can take off the ground techs: ground treatments (ground sites,
# prehatch sites/acres; drone treatments 'D' are on GROUND sites) use drone_offload_pct,
# and air-site inspections use drone_air_offload_pct. ALL ground techs inspect air
# sites; drones only help. Sections and wet catch basins are not offloaded.
DRONE_OFFSET_METRICS <- c("ground_sites", "ground_acres", "prehatch_sites", "prehatch_acres")
DRONE_AIR_OFFSET_METRICS <- c("air_sites", "air_acres")

#' Staffing baseline per facility + district: what each tech / crew carries (P1
#' load) using the configured/edited staffing, not the DB active flag. Every site
#' count is paired with its acres. Air sites are checked by ALL ground techs (not
#' just drone techs), so air load is per ground tech; drone techs are reported
#' separately.
#'
#' @param cap    aggregate_capacity_by_fos() output (P1/P2 rows per FOS area)
#' @param staff  staffing table (facility, crews, techs_per_crew, drone_techs,
#'               drone_offload_pct, drone_air_offload_pct)
#' @return data.frame, DISTRICT row first: crews, ground_techs, drone_techs, P1
#'   loads (p1_<kind>_sites / p1_<kind>_acres for ground, air, ph) and per-tech
#'   (<kind>_sites_per_tech, <kind>_acres_per_tech) and per-crew
#'   (<kind>_acres_per_crew) ratios
staffing_baseline <- function(cap, staff) {
  if (is.null(cap) || nrow(cap) == 0 || is.null(staff) || nrow(staff) == 0) {
    return(data.frame())
  }
  p1 <- cap[cap$zone == "1", , drop = FALSE]
  load <- p1 %>%
    dplyr::group_by(facility) %>%
    dplyr::summarise(
      p1_ground_sites = sum(ground_sites, na.rm = TRUE),
      p1_ground_acres = round(sum(ground_acres, na.rm = TRUE), 1),
      p1_air_sites    = sum(air_sites, na.rm = TRUE),
      p1_air_acres    = round(sum(air_acres, na.rm = TRUE), 1),
      p1_ph_sites     = sum(prehatch_sites, na.rm = TRUE),
      p1_ph_acres     = round(sum(prehatch_acres, na.rm = TRUE), 1),
      .groups = "drop")
  load_cols <- setdiff(names(load), "facility")
  b <- merge(staff, load, by = "facility", all.x = TRUE)
  for (col in load_cols) b[[col]][is.na(b[[col]])] <- 0
  b$ground_techs <- b$crews * b$techs_per_crew

  keep <- c("facility", "crews", "techs_per_crew", "drone_techs", "drone_offload_pct",
            "drone_air_offload_pct", load_cols, "ground_techs")
  b <- b[, keep]
  tot <- b[1, , drop = FALSE]
  tot[1, ] <- NA
  tot$facility <- "DISTRICT"
  for (col in c("crews", "drone_techs", load_cols, "ground_techs")) tot[[col]] <- sum(b[[col]])
  b <- rbind(tot, b)

  div <- function(n, d) ifelse(d > 0, round(n / d, 1), NA_real_)
  for (m in load_cols) {
    key <- sub("^p1_", "", m)                      # e.g. ground_acres
    b[[paste0(key, "_per_tech")]] <- div(b[[m]], b$ground_techs)
    if (grepl("acres$", key)) b[[paste0(key, "_per_crew")]] <- div(b[[m]], b$crews)
  }
  b
}

#' What-if: promote a set of P2 sections into P1 and estimate added techs.
#'
#' For each affected FOS, moves the promoted sections' loads from P2 to P1, then
#' compares the resulting P1 load (on the chosen metric) to a per-tech capacity to
#' estimate techs needed vs. currently assigned.
#'
#' @param sections  section-grain df (load_capacity_sections())
#' @param promote_sectcodes character vector of sectcodes to move to P1
#' @param cap       aggregate_capacity_by_fos() output (for names + current techs)
#' @param metric    which load drives staffing: one of CAPACITY_ZONE_METRICS
#'                  (excluding avg_miles) e.g. "ground_sites", "sections", "wet_cb"
#' @param per_tech  manageable load of `metric` per tech (editable baseline)
#'                  Ground-treated metrics (DRONE_OFFSET_METRICS) are first reduced
#'                  by each facility's drone_offload_pct, and air sites by its
#'                  drone_air_offload_pct (both carried in `cap`).
#' @return list(by_fos = df, totals = list)
whatif_compute <- function(sections, promote_sectcodes, cap,
                           metric = "ground_sites", per_tech = NULL) {
  empty <- list(by_fos = data.frame(), totals = list(added_techs = 0))
  if (is.null(sections) || nrow(sections) == 0) return(empty)
  if (!metric %in% setdiff(CAPACITY_ZONE_METRICS, "avg_miles")) {
    metric <- "ground_sites"
  }

  sec <- sections
  sec$promoted <- sec$sectcode %in% promote_sectcodes &
    as.character(sec$zone) == "2"

  # Effective zone after promotion
  sec$eff_zone <- ifelse(sec$promoted, "1", as.character(sec$zone))

  # Current + projected P1 load of `metric` per FOS
  cur <- sec[sec$zone == "1", ] %>%
    dplyr::group_by(fosarea) %>%
    dplyr::summarise(cur_p1 = sum(.data[[metric]], na.rm = TRUE), .groups = "drop")
  proj <- sec[sec$eff_zone == "1", ] %>%
    dplyr::group_by(fosarea) %>%
    dplyr::summarise(new_p1 = sum(.data[[metric]], na.rm = TRUE), .groups = "drop")

  by <- merge(cur, proj, by = "fosarea", all = TRUE)
  by$cur_p1[is.na(by$cur_p1)] <- 0
  by$new_p1[is.na(by$new_p1)] <- 0
  by$added_load <- by$new_p1 - by$cur_p1

  # Names + current techs from cap
  if (!"drone_offload_pct" %in% names(cap)) cap$drone_offload_pct <- 0
  if (!"drone_air_offload_pct" %in% names(cap)) cap$drone_air_offload_pct <- 0
  fos_info <- unique(cap[, c("fosarea", "fos_name", "facility", "facility_name",
                             "n_techs", "drone_offload_pct", "drone_air_offload_pct")])
  by <- merge(by, fos_info, by = "fosarea", all.x = TRUE)

  # Drones take a share of the ground-treated load (or of air inspections);
  # staff the techs only for the remainder.
  off_col <- if (metric %in% DRONE_OFFSET_METRICS) "drone_offload_pct" else
             if (metric %in% DRONE_AIR_OFFSET_METRICS) "drone_air_offload_pct" else NULL
  if (!is.null(off_col)) {
    keep <- 1 - pmin(pmax(ifelse(is.na(by[[off_col]]), 0, by[[off_col]]), 0), 100) / 100
    by$cur_p1 <- round(by$cur_p1 * keep, 1)
    by$new_p1 <- round(by$new_p1 * keep, 1)
    by$added_load <- by$new_p1 - by$cur_p1
  }

  # Per-tech capacity: default = current district average P1 load / tech
  if (is.null(per_tech) || is.na(per_tech) || per_tech <= 0) {
    tot_p1 <- sum(by$cur_p1, na.rm = TRUE)
    tot_tech <- sum(by$n_techs, na.rm = TRUE)
    per_tech <- if (tot_tech > 0) tot_p1 / tot_tech else NA_real_
  }

  if (!is.na(per_tech) && per_tech > 0) {
    # Techs a load requires at the per-tech baseline
    by$needed_now <- ceiling(by$cur_p1 / per_tech)   # for current P1 load
    by$needed_new <- ceiling(by$new_p1 / per_tech)   # after promotion
    # Additional techs the PROMOTION creates (0 for unaffected FOS)
    by$added_techs <- pmax(0, by$needed_new - by$needed_now)
    # Current over(-)/under(+) staffing vs the baseline (informational)
    by$gap_now <- by$needed_now - by$n_techs
  } else {
    by$needed_now <- NA_integer_
    by$needed_new <- NA_integer_
    by$added_techs <- NA_integer_
    by$gap_now <- NA_integer_
  }

  # Show FOS affected by the promotion first
  by <- by[order(-by$added_load, -by$added_techs), ]

  list(
    by_fos = by[, c("facility_name", "fos_name", "fosarea", "n_techs",
                    "cur_p1", "new_p1", "added_load", "needed_new",
                    "added_techs", "gap_now")],
    totals = list(
      per_tech    = round(per_tech, 1),
      metric      = metric,
      promoted_n  = length(promote_sectcodes),
      added_load  = sum(by$added_load, na.rm = TRUE),
      added_techs = sum(by$added_techs, na.rm = TRUE)
    )
  )
}
