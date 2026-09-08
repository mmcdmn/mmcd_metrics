# FOS Capacity & Border-to-Border Planning — Display & What-If logic
# =============================================================================
# Pure functions (no Shiny, no DB) that shape the aggregated capacity data for
# display and run the what-if P2 -> P1 promotion / staffing math. Kept separate
# so they can be unit-tested against the live data loaders.
# =============================================================================

CAPACITY_ZONE_METRICS <- c("sections", "ground_sites", "air_sites",
                           "prehatch_sites", "prehatch_acres", "wet_cb",
                           "avg_miles")

#' Workload-study crew estimate (2001 Ch. 11 method): convert the promoted
#' sections' site counts into seasonal jobs, then hours, then crews.
#'   jobs  = site_count(kind) x jobs-per-site rate
#'   hours = jobs x hours-per-job
#'   crews = total hours / hours-per-crew-season
#'
#' @param promoted_sections section-grain df (ground_sites, air_sites) being added
#' @param rates          named vector job_type -> jobs-per-distinct-site (P1)
#' @param hours_per_job  named vector job_type -> hours (editable; default study)
#' @param hrs_per_crew   hours a crew works air+gnd insp/trt per season (def 2184)
#' @return list(by_job = df, totals = list)
estimate_crews <- function(promoted_sections, rates,
                           hours_per_job = STUDY_HOURS_PER_JOB,
                           hrs_per_crew = STUDY_CREW_HOURS_SEASON) {
  jt <- names(JOB_TYPE_LABELS)
  g_sites <- if (nrow(promoted_sections) > 0) sum(promoted_sections$ground_sites, na.rm = TRUE) else 0
  a_sites <- if (nrow(promoted_sections) > 0) sum(promoted_sections$air_sites, na.rm = TRUE) else 0

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

  total_hours <- sum(hours, na.rm = TRUE)
  crews <- if (hrs_per_crew > 0) total_hours / hrs_per_crew else NA_real_

  list(
    by_job = by_job,
    totals = list(
      ground_sites = g_sites, air_sites = a_sites,
      jobs  = sum(jobs, na.rm = TRUE),
      hours = round(total_hours, 1),
      hrs_per_crew = hrs_per_crew,
      crews = round(crews, 2)
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
  fos_info <- unique(cap[, c("fosarea", "fos_name", "facility", "facility_name",
                             "n_techs")])
  by <- merge(by, fos_info, by = "fosarea", all.x = TRUE)

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
