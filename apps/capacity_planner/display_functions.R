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

#'
#' @param promoted_sections section-grain df (ground_sites, air_sites, miles) being added
#' @param rates          named vector job_type -> jobs-per-distinct-site (P1)
#' @param hours_per_job  named vector job_type -> hours (editable; default study)
#' @param hrs_per_crew   hours a crew works air+gnd insp/trt per season (def 2184)
#' @param trips_per_section round trips a crew drives to each section
#' @param avg_mph, circuity  drive-time estimate params (see est_drive_minutes)
#' @param drive_minutes optional one-way drive minutes per promoted section (e.g.
#'   routed times from section_drive_minutes()); when given it replaces the
#'   straight-line estimate
#' @return list(by_job = df, totals = list) — totals$hours is the FULL pool
#'   (on-site + travel); onsite_hours and travel_hours are broken out.
estimate_crews <- function(promoted_sections, rates,
                           hours_per_job = STUDY_HOURS_PER_JOB,
                           hrs_per_crew = STUDY_CREW_HOURS_SEASON,
                           trips_per_section = DEFAULT_TRIPS_PER_SECTION,
                           avg_mph = 45, circuity = 1.3, drive_minutes = NULL) {
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
  if (n_sec > 0 && trips_per_section > 0) {
    one_way_hrs <- if (!is.null(drive_minutes)) {
      drive_minutes / 60
    } else if (!is.null(promoted_sections$miles)) {
      est_drive_minutes(promoted_sections$miles, avg_mph, circuity) / 60
    } else NULL
    if (!is.null(one_way_hrs)) {
      travel_hours <- sum(trips_per_section * 2 * one_way_hrs, na.rm = TRUE)
    }
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
#' @param sections section-grain df (miles, optionally drive_min_routed)
#' @param mode "routed" or "estimate"
#' @param avg_mph,circuity straight-line estimate parameters (estimate mode / fallback)
#' @param speed_pct percent of map speed crews actually travel at (routed mode)
section_drive_minutes <- function(sections, mode = "routed", avg_mph = 45,
                                  circuity = 1.3, speed_pct = 100) {
  if (is.null(sections) || nrow(sections) == 0) return(numeric(0))
  est <- est_drive_minutes(sections$miles, avg_mph, circuity)
  if (!identical(mode, "routed") || is.null(sections$drive_min_routed)) return(est)
  pct <- if (is.null(speed_pct) || is.na(speed_pct) || speed_pct <= 0) 100 else speed_pct
  routed <- round(sections$drive_min_routed * 100 / pct, 1)
  ifelse(is.na(routed), est, routed)
}

#' Per-facility comparison of routed vs straight-line distance/time, to show how
#' realistic the circuity factor and average speed are.
#'
#' @param sections section-grain df with miles, drive_min_routed, drive_miles_routed
#' @param avg_mph,circuity the straight-line estimate being compared against
#' @return data.frame: facility (DISTRICT first), sections, median straight / road
#'   miles, circuity (road / straight), implied mph, routed min, estimate min
routed_summary <- function(sections, avg_mph = 45, circuity = 1.3) {
  if (is.null(sections) || is.null(sections$drive_min_routed)) return(data.frame())
  s <- sections[!is.na(sections$drive_min_routed) & !is.na(sections$miles) & sections$miles > 1, , drop = FALSE]
  if (nrow(s) == 0) return(data.frame())
  one <- function(d, label) data.frame(
    Facility = label, Sections = nrow(d),
    `Straight-line mi` = round(stats::median(d$miles), 1),
    `Road mi` = round(stats::median(d$drive_miles_routed), 1),
    Circuity = round(stats::median(d$drive_miles_routed / d$miles), 2),
    `Avg mph (routed)` = round(stats::median(d$drive_miles_routed / (d$drive_min_routed / 60)), 1),
    `Routed min` = round(stats::median(d$drive_min_routed), 1),
    `Estimate min` = round(stats::median(est_drive_minutes(d$miles, avg_mph, circuity)), 1),
    check.names = FALSE)
  rbind(one(s, "DISTRICT"), do.call(rbind, lapply(split(s, s$facility), function(d) one(d, d$facility[1]))))
}

#' Pivot the per-FOS x zone capacity to one wide row per FOS area
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
DRONE_OFFSET_METRICS <- c("ground_sites", "ground_acres", "prehatch_sites", "prehatch_acres")
DRONE_AIR_OFFSET_METRICS <- c("air_sites", "air_acres")
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

# -----------------------------------------------------------------------------
# What-if scenarios: promote P2 -> P1 and/or move sections between facilities
# -----------------------------------------------------------------------------
# Metrics a scenario reports (avg_miles is a per-FOS-area average, not a load)
SCENARIO_METRICS <- setdiff(CAPACITY_ZONE_METRICS, "avg_miles")

#' Apply per-section changes to the section-grain table.
#' @param sections section-grain df (facility, zone, sectcode, loads, miles, ...)
#' @param changes  data.frame(sectcode, to_facility, to_zone); NULL/empty = no change
#' @param times    routed lookup incl. straight_miles, or NULL
#' @param facilities the valid destination facilities (default: those in `sections`; pass the
#'   full list when `sections` is only a subset of the rows)
#' @return `sections` plus facility_from, zone_from, fos_new (NA once moved to another
#'   facility), moved, promoted, time_missing;
#'   `facility` and `zone` hold the NEW values
scenario_apply <- function(sections, changes = NULL, times = NULL, facilities = NULL) {
  s <- sections
  s$zone <- as.character(s$zone)
  s$facility_from <- s$facility
  s$zone_from <- s$zone
  s$fos_new <- as.character(s$fosarea)
  s$moved <- FALSE
  s$promoted <- FALSE
  s$time_missing <- FALSE
  if (is.null(changes) || nrow(changes) == 0) return(s)

  ch <- changes[!duplicated(as.character(changes$sectcode), fromLast = TRUE), , drop = FALSE]
  i <- match(as.character(s$sectcode), as.character(ch$sectcode))
  has <- !is.na(i)

  want_fac <- ifelse(has, as.character(ch$to_facility[i]), NA_character_)
  known <- if (is.null(facilities)) unique(s$facility) else facilities
  to_fac <- ifelse(!is.na(want_fac) & want_fac %in% known, want_fac, s$facility)
  want_zone <- ifelse(has, as.character(ch$to_zone[i]), NA_character_)
  promote <- !is.na(want_zone) & want_zone == "1" & s$zone_from == "2"
  to_zone <- ifelse(promote, "1", s$zone_from)

  s$moved <- to_fac != s$facility_from
  s$promoted <- to_zone != s$zone_from
  s$facility <- to_fac
  s$zone <- to_zone
  # A section moved to another facility belongs to none of that facility's FOS areas
  s$fos_new <- ifelse(s$moved, NA_character_, as.character(s$fosarea))

  mv <- which(s$moved)
  if (length(mv) > 0) {
    idx <- if (is.null(times)) rep(NA_integer_, length(mv)) else
      match(paste(s$facility[mv], s$sectcode[mv]), paste(times$facility, times$sectcode))
    pick <- function(col) if (is.null(times) || is.null(times[[col]])) NA else times[[col]][idx]
    if ("miles" %in% names(s)) s$miles[mv] <- pick("straight_miles")
    if ("drive_min_routed" %in% names(s)) s$drive_min_routed[mv] <- pick("drive_min_routed")
    if ("drive_miles_routed" %in% names(s)) s$drive_miles_routed[mv] <- pick("drive_miles_routed")
    if ("snap_m" %in% names(s)) s$snap_m[mv] <- pick("snap_m")
    if ("snap_flag" %in% names(s)) s$snap_flag[mv] <- pick("snap_flag")
    s$time_missing[mv] <- is.na(idx)
  }
  s
}

#' Select / deselect sections in a scenario (what a click, a box or a checkbox does).
#' @param sections section-grain df (facility, zone, sectcode) as it is today
#' @param current  the scenario so far: data.frame(sectcode, to_facility, to_zone)
#' @param codes    sections acted on
#' @param dest     "keep" or the facility to move them to
#' @param mode     "toggle", "set" or "off"
#' @return list(changes = the new scenario, skipped = number of P1 sections that nothing
#'   could be applied to, and that were not in the scenario)
scenario_toggle <- function(sections, current, codes, dest = "keep", mode = "toggle") {
  eq_na <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  cur <- current
  codes <- unique(as.character(codes)); codes <- codes[!is.na(codes) & nzchar(codes)]
  sec <- sections[match(codes, as.character(sections$sectcode)), , drop = FALSE]
  sec <- sec[!is.na(sec$sectcode), , drop = FALSE]
  if (nrow(sec) == 0) return(list(changes = cur, skipped = 0L))
  mv <- if (is.null(dest) || identical(dest, "keep")) rep(NA_character_, nrow(sec)) else
    ifelse(as.character(sec$facility) == dest, NA_character_, dest)
  pr <- ifelse(as.character(sec$zone) == "2", "1", NA_character_)
  tgt <- data.frame(sectcode = as.character(sec$sectcode), to_facility = mv, to_zone = pr,
                    stringsAsFactors = FALSE)
  can <- !is.na(tgt$to_facility) | !is.na(tgt$to_zone)
  old <- cur[match(tgt$sectcode, cur$sectcode), , drop = FALSE]
  has <- !is.na(old$sectcode)
  same <- has & can & eq_na(old$to_facility, tgt$to_facility) & eq_na(old$to_zone, tgt$to_zone)

  drop <- character(0)
  add <- tgt[0, , drop = FALSE]
  if (identical(mode, "off")) {
    drop <- tgt$sectcode[has]
  } else if (identical(mode, "set")) {
    add <- tgt[can, , drop = FALSE]
  } else if (any(can)) {
    if (all(same[can])) drop <- tgt$sectcode[can] else add <- tgt[can & !same, , drop = FALSE]
  } else {
    drop <- tgt$sectcode[has]
  }
  out <- rbind(cur[!cur$sectcode %in% c(drop, add$sectcode), , drop = FALSE], add)
  rownames(out) <- NULL
  list(changes = out, skipped = as.integer(sum(!can & !has)))
}

#' Facility totals of the scenario metrics
#' @param sections section-grain df with facility and zone (new values)
scenario_cap <- function(sections) {
  if (is.null(sections) || nrow(sections) == 0) return(data.frame())
  s <- sections
  if (!"sections" %in% names(s)) s$sections <- 1L
  s$zone <- as.character(s$zone)
  g <- stats::aggregate(s[, SCENARIO_METRICS], by = list(facility = s$facility, zone = s$zone),
                        FUN = function(x) sum(x, na.rm = TRUE))
  g
}

#' Workload change per facility 
#'
#' @param after   scenario_apply() output (before = facility_from / zone_from)
#' @param zone    "1" (default, what promotion adds to), "2" or "both"
#' @param metrics which SCENARIO_METRICS to report
#' @param facility NULL for one row per facility, or a facility to break down by FOS area
#' @param dm_
#' @return data.frame: key, label, <metric>_before/_after/_chg, p1_min_before, added_min
scenario_compare <- function(after, zone = "1", metrics = SCENARIO_METRICS, facility = NULL,
                             dm_before = NULL, dm_after = NULL) {
  if (is.null(after) || nrow(after) == 0) return(data.frame())
  s <- after
  if (!"sections" %in% names(s)) s$sections <- 1L
  all_z <- identical(zone, "both")
  keep_b <- if (all_z) rep(TRUE, nrow(s)) else s$zone_from == zone
  keep_a <- if (all_z) rep(TRUE, nrow(s)) else s$zone == zone

  if (is.null(facility)) {
    key_b <- s$facility_from
    key_a <- s$facility
    keys <- sort(unique(c(key_b, key_a)))
    labels <- keys
    total <- list(key = "DISTRICT", label = "DISTRICT")
    in_scope <- rep(TRUE, nrow(s))
  } else {
    key_b <- ifelse(s$facility_from == facility, as.character(s$fosarea), NA_character_)
    key_a <- ifelse(s$facility == facility,
                    ifelse(is.na(s$fos_new), "unassigned", as.character(s$fos_new)),
                    NA_character_)
    keys <- unique(c(key_b, key_a)); keys <- keys[!is.na(keys)]
    keys <- c(sort(setdiff(keys, "unassigned")), intersect(keys, "unassigned"))
    nm <- if ("fos_name" %in% names(s)) tapply(as.character(s$fos_name), s$fosarea, function(v) v[1]) else NULL
    labels <- vapply(keys, function(k) {
      if (identical(k, "unassigned")) "Unassigned (moved in)" else
        if (!is.null(nm) && !is.na(nm[k])) unname(nm[k]) else paste0("FOS ", k)
    }, "")
    total <- list(key = "ALL", label = paste("All of", facility))
    in_scope <- s$facility_from == facility | s$facility == facility
  }

  tot <- function(m, key, keep, k) sum(s[[m]][keep & !is.na(key) & key == k], na.rm = TRUE)
  out <- data.frame(key = c(total$key, keys), label = c(total$label, labels),
                    stringsAsFactors = FALSE)
  for (m in metrics) {
    b <- c(sum(s[[m]][keep_b & in_scope & !is.na(key_b)], na.rm = TRUE),
           vapply(keys, function(k) tot(m, key_b, keep_b, k), numeric(1)))
    a <- c(sum(s[[m]][keep_a & in_scope & !is.na(key_a)], na.rm = TRUE),
           vapply(keys, function(k) tot(m, key_a, keep_a, k), numeric(1)))
    out[[paste0(m, "_before")]] <- round(b, 3)
    out[[paste0(m, "_after")]]  <- round(a, 3)
    out[[paste0(m, "_chg")]]    <- round(a - b, 3)
  }

  if (!is.null(dm_before) && !is.null(dm_after) &&
      length(dm_before) == nrow(s) && length(dm_after) == nrow(s)) {
    med <- function(v) if (length(v) == 0 || all(is.na(v))) NA_real_ else round(stats::median(v, na.rm = TRUE), 1)
    was <- s$zone_from == "1"
    now <- s$zone == "1"
    one <- function(k) {
      in_b <- if (is.na(k)) !is.na(key_b) else !is.na(key_b) & key_b == k
      in_a <- if (is.na(k)) !is.na(key_a) else !is.na(key_a) & key_a == k
      w <- in_b & was
      c(med(dm_before[w]), med(dm_after[in_a & now & !w]))
    }
    m <- cbind(one(NA_character_), vapply(keys, one, numeric(2)))
    out$p1_min_before <- m[1, ]
    out$added_min <- m[2, ]
    if (!is.null(facility) && "unassigned" %in% keys) {
      # no P1 of its own yet: compare the moved-in sections with the facility's median P1 time
      out$p1_min_before[out$key == "unassigned"] <- out$p1_min_before[1]
    }
  }
  rownames(out) <- NULL
  out
}

#' Median one-way drive minutes of the P1 sections of each facility, before and after.
#'
#' @param after scenario_apply() output
#' @param dm_before,dm_after one-way minutes per row for the BEFORE facility and the
#'   AFTER facility (section_drive_minutes() on the before / after tables)
scenario_drive_summary <- function(after, dm_before, dm_after) {
  if (is.null(after) || nrow(after) == 0) return(data.frame())
  facs <- sort(unique(c(after$facility_from, after$facility)))
  med <- function(x) if (all(is.na(x))) NA_real_ else round(stats::median(x, na.rm = TRUE), 1)
  data.frame(
    facility = c("DISTRICT", facs),
    p1_median_min_before = c(
      med(dm_before[after$zone_from == "1"]),
      vapply(facs, function(f) med(dm_before[after$facility_from == f & after$zone_from == "1"]),
             numeric(1))),
    p1_median_min_after = c(
      med(dm_after[after$zone == "1"]),
      vapply(facs, function(f) med(dm_after[after$facility == f & after$zone == "1"]),
             numeric(1))),
    stringsAsFactors = FALSE, row.names = NULL)
}

#' Staffing impact (Estimate A) per FOS area (one FOS area = one crew): the P1 load of
#' `metric` each FOS area gains (sections newly in its P1: promoted) and loses (its P1
#' sections that were moved away), converted to techs at a per-tech capacity. A section
#' moved INTO a facility belongs to none of its FOS areas, so it lands in that facility's
#' "unassigned" row (no current crew or techs, so every tech it needs is new).
#'
#'   techs change = ceil((curP1 - removed + added / capacity factor) / per_tech)
#'                  - ceil(curP1 / per_tech)        (negative = techs freed)
#'
#' Ground-treated metrics (DRONE_OFFSET_METRICS) are reduced by the facility's
#' drone_offload_pct and air sites by its drone_air_offload_pct first. With
#' `travel_adjust`, only the ADDED load is scaled by the capacity factor
#' 1 / ((1 - s) + s * ratio), s = travel_share / 100, ratio = median drive minutes to the
#' FOS area's added sections / median to its current P1 sections (timed from the facility;
#' the unassigned row uses the facility's median P1 time as its baseline). A ratio above 1
#' means the added sections are farther away (more driving), so a tech carries less of them;
#' below 1 they are closer. Removed load frees techs at the baseline capacity.
#'
#' @param after scenario_apply() output
#' @param staff staffing table (facility, crews, techs_per_crew, drone_techs,
#'              drone_offload_pct, drone_air_offload_pct)
#' @param metric one of SCENARIO_METRICS
#' @param per_tech manageable load per tech (NULL/NA = district average P1 load per tech)
#' @param dm_before,dm_after one-way minutes per row of `after` for the before / after
#'   facility; needed for `travel_adjust`
#' @param travel_adjust apply the travel capacity factor
#' @param travel_share percent of a tech's workday spent driving (default 20)
#' @return list(by_fos = df per FOS area (+ unassigned), by_facility = df summed per
#'   facility, totals = list)
scenario_staffing <- function(after, staff, metric = "ground_sites", per_tech = NULL,
                              dm_before = NULL, dm_after = NULL,
                              travel_adjust = FALSE, travel_share = 20) {
  empty <- list(by_fos = data.frame(), by_facility = data.frame(),
                totals = list(techs_change = 0))
  if (is.null(after) || nrow(after) == 0 || is.null(staff) || nrow(staff) == 0) return(empty)
  if (!metric %in% SCENARIO_METRICS) metric <- "ground_sites"
  s <- after
  if (!"sections" %in% names(s)) s$sections <- 1L
  x <- as.numeric(s[[metric]])
  p1_before <- s$zone_from == "1"
  p1_after <- s$zone == "1"
  use_travel <- isTRUE(travel_adjust) && !is.null(dm_before) && !is.null(dm_after) &&
    length(dm_before) == nrow(s) && length(dm_after) == nrow(s)
  sh <- if (is.null(travel_share) || is.na(travel_share)) 20 else travel_share
  sh <- min(max(sh, 0), 99) / 100
  med <- function(v) if (length(v) == 0 || all(is.na(v))) NA_real_ else stats::median(v, na.rm = TRUE)

  fos_b <- paste(s$facility_from, as.character(s$fosarea), sep = "|")
  fos_a <- paste(s$facility, ifelse(is.na(s$fos_new), "unassigned", as.character(s$fos_new)),
                 sep = "|")
  keys <- sort(unique(c(fos_b, fos_a)))
  parts <- strsplit(keys, "|", fixed = TRUE)
  kf <- vapply(parts, `[`, "", 1)
  kk <- vapply(parts, `[`, "", 2)
  ord <- order(kf, kk == "unassigned", kk)
  keys <- keys[ord]; kf <- kf[ord]; kk <- kk[ord]
  nm <- if ("fos_name" %in% names(s)) tapply(as.character(s$fos_name), s$fosarea, function(v) v[1]) else NULL

  rows <- lapply(seq_along(keys), function(i) {
    f <- kf[i]; k <- kk[i]; un <- identical(k, "unassigned")
    was <- fos_b == keys[i] & p1_before
    now <- fos_a == keys[i] & p1_after
    add <- now & !was
    rem <- was & !now
    cur <- sum(x[was], na.rm = TRUE)
    gain <- sum(x[add], na.rm = TRUE)
    loss <- sum(x[rem], na.rm = TRUE)
    st <- staff[staff$facility == f, , drop = FALSE]
    techs <- if (un || nrow(st) == 0) 0 else st$techs_per_crew[1]
    off <- if (metric %in% DRONE_OFFSET_METRICS) "drone_offload_pct" else
           if (metric %in% DRONE_AIR_OFFSET_METRICS) "drone_air_offload_pct" else NULL
    keep <- 1
    if (!is.null(off) && nrow(st) > 0 && !is.na(st[[off]][1])) {
      keep <- 1 - min(max(st[[off]][1], 0), 100) / 100
    }
    m1 <- m2 <- NA_real_
    ratio <- 1
    if (use_travel) {
      m1 <- med(dm_before[was])
      if (is.na(m1)) m1 <- med(dm_before[s$facility_from == f & p1_before])  # facility baseline
      m2 <- med(dm_after[add])
      ratio <- m2 / m1                           # added / current: above 1 = more driving
      if (is.na(ratio) || !is.finite(ratio) || ratio < 0) ratio <- 1
    }
    label <- if (un) "Unassigned (moved in)" else
      if (!is.null(nm) && !is.na(nm[k])) unname(nm[k]) else paste0("FOS ", k)
    data.frame(facility = f, fosarea = k, fos_name = label, n_techs = techs,
               cur_p1 = round(cur * keep, 1), added_load = round(gain * keep, 1),
               removed_load = round(loss * keep, 1),
               med_p1_min = round(m1, 1), med_added_min = round(m2, 1),
               travel_ratio = ratio, capacity_factor = 1 / ((1 - sh) + sh * ratio),
               stringsAsFactors = FALSE)
  })
  by <- do.call(rbind, rows)
  by$new_p1 <- round(by$cur_p1 - by$removed_load + by$added_load, 1)

  if (is.null(per_tech) || is.na(per_tech) || per_tech <= 0) {
    tot_tech <- sum(by$n_techs, na.rm = TRUE)
    per_tech <- if (tot_tech > 0) sum(by$cur_p1, na.rm = TRUE) / tot_tech else NA_real_
  }

  if (!is.na(per_tech) && per_tech > 0) {
    by$needed_now <- ceiling(round(by$cur_p1 / per_tech, 9))
    eff_new <- ifelse(by$capacity_factor == 1, by$new_p1,
                      by$cur_p1 - by$removed_load + by$added_load / by$capacity_factor)
    by$needed_new <- ceiling(round(eff_new / per_tech, 9))
    by$techs_change <- by$needed_new - by$needed_now
    by$gap_now <- round(by$needed_now - by$n_techs, 1)
  } else {
    by$needed_now <- by$needed_new <- by$techs_change <- by$gap_now <- NA_integer_
  }
  rownames(by) <- NULL

  sum_cols <- c("n_techs", "cur_p1", "removed_load", "added_load", "new_p1",
                "needed_now", "needed_new", "techs_change", "gap_now")
  fac <- stats::aggregate(by[, sum_cols], by = list(facility = by$facility),
                          FUN = function(v) sum(v, na.rm = TRUE))
  fac$gap_now <- round(fac$gap_now, 1)
  fac$n_techs <- round(fac$n_techs, 1)
  fac <- fac[order(-abs(fac$added_load - fac$removed_load), fac$facility), ]
  rownames(fac) <- NULL

  list(
    by_fos = by, by_facility = fac,
    totals = list(
      per_tech = round(per_tech, 1), metric = metric, travel_adjusted = use_travel,
      added_load = sum(by$added_load, na.rm = TRUE),
      removed_load = sum(by$removed_load, na.rm = TRUE),
      techs_change = sum(by$techs_change, na.rm = TRUE),
      techs_added = sum(pmax(0, by$techs_change), na.rm = TRUE),
      techs_freed = sum(pmax(0, -by$techs_change), na.rm = TRUE)
    )
  )
}

#' Staffing impact (Estimate B, 2001 Workload Study method) for a scenario: hours of
#' work for the sections NEWLY in each facility's P1 (added), minus the hours of the
#' sections that left its P1 (removed), over hours per crew-season. Reuses
#' estimate_crews() per facility, so each facility can use its own job rates.
#'
#' @param after scenario_apply() output
#' @param rates list: facility (or "DISTRICT") -> named job-type rate vector
#' @param dm_before,dm_after one-way minutes per row (before / after facility), or NULL
#'   to use the straight-line estimate from `miles`
#' @inheritParams estimate_crews
#' @return list(by_facility = df, by_job = df (net), totals = list)
scenario_crews <- function(after, rates, hours_per_job = STUDY_HOURS_PER_JOB,
                           hrs_per_crew = STUDY_CREW_HOURS_SEASON,
                           trips_per_section = DEFAULT_TRIPS_PER_SECTION,
                           avg_mph = 45, circuity = 1.3,
                           dm_before = NULL, dm_after = NULL) {
  if (is.null(after) || nrow(after) == 0) {
    return(list(by_facility = data.frame(), by_job = data.frame(),
                totals = list(crews = 0, hours = 0, onsite_hours = 0, travel_hours = 0,
                              hrs_per_crew = hrs_per_crew, n_added = 0, n_removed = 0,
                              ground_sites = 0, air_sites = 0, jobs = 0)))
  }
  s <- after
  was <- s$zone_from == "1"
  now <- s$zone == "1"
  facs <- sort(unique(c(s$facility_from, s$facility)))
  rates_for <- function(f) {
    r <- rates[[f]]
    if (is.null(r)) r <- rates[["DISTRICT"]]
    if (is.null(r)) r <- numeric(0)
    r
  }
  est <- function(rows, fac, dm) {
    p <- s[rows, , drop = FALSE]
    estimate_crews(p, rates_for(fac), hours_per_job, hrs_per_crew, trips_per_section,
                   avg_mph, circuity, drive_minutes = if (is.null(dm)) NULL else dm[rows])
  }
  per_fac <- lapply(facs, function(f) {
    add <- which(s$facility == f & now & !(s$facility_from == f & was))
    rem <- which(s$facility_from == f & was & !(s$facility == f & now))
    a <- est(add, f, dm_after)
    r <- est(rem, f, dm_before)
    list(f = f, a = a, r = r, n_add = length(add), n_rem = length(rem))
  })
  by_fac <- do.call(rbind, lapply(per_fac, function(p) data.frame(
    facility = p$f, added_sections = p$n_add, removed_sections = p$n_rem,
    added_hours = p$a$totals$hours, removed_hours = p$r$totals$hours,
    net_hours = round(p$a$totals$hours - p$r$totals$hours, 1),
    net_crews = if (hrs_per_crew > 0) round((p$a$totals$hours - p$r$totals$hours) / hrs_per_crew, 2) else NA_real_,
    stringsAsFactors = FALSE)))
  by_fac <- by_fac[by_fac$added_sections > 0 | by_fac$removed_sections > 0, , drop = FALSE]
  rownames(by_fac) <- NULL

  net_job <- Reduce(function(acc, p) {
    d <- p$a$by_job
    num <- c("Sites", "Jobs", "Hours")
    d[num] <- p$a$by_job[num] - p$r$by_job[num]
    if (is.null(acc)) d else { acc[num] <- acc[num] + d[num]; acc }
  }, per_fac, NULL)
  if (!is.null(net_job)) {                       # Jobs/site differs by facility: show jobs / sites
    net_job[["Jobs/site"]] <- ifelse(net_job$Sites != 0, round(net_job$Jobs / net_job$Sites, 2), NA)
    net_job$Hours <- round(net_job$Hours, 1)
  }
  tsum <- function(k) sum(vapply(per_fac, function(p) p$a$totals[[k]] - p$r$totals[[k]], numeric(1)))
  hours <- tsum("hours")
  list(
    by_facility = by_fac, by_job = net_job,
    totals = list(
      ground_sites = tsum("ground_sites"), air_sites = tsum("air_sites"),
      n_added = sum(vapply(per_fac, function(p) p$n_add, numeric(1))),
      n_removed = sum(vapply(per_fac, function(p) p$n_rem, numeric(1))),
      jobs = tsum("jobs"), onsite_hours = round(tsum("onsite_hours"), 1),
      travel_hours = round(tsum("travel_hours"), 1), hours = round(hours, 1),
      hrs_per_crew = hrs_per_crew,
      crews = if (hrs_per_crew > 0) round(hours / hrs_per_crew, 2) else NA_real_
    )
  )
}

# ---- CSV exports of the What-If results (plain numbers, snake_case headers) -------------
# The tables on the page show formatted text ("+4,443", "319.3 -> 320.9"); the downloads
# hold the numbers themselves so they open cleanly in a spreadsheet.

#' What changed as numbers: for every metric the P1 value before, after and the change,
#' plus the median drive minutes to the current P1 sections and to the added sections and
#' their ratio (added / current: above 1 = more driving).
#'
#' @param cmp   scenario_compare() output
#' @param ids   name of the key column, or of the key and label columns (FOS mode)
scenario_export_compare <- function(cmp, ids = "facility") {
  if (is.null(cmp) || nrow(cmp) == 0) return(data.frame())
  out <- data.frame(cmp$key, stringsAsFactors = FALSE)
  if (length(ids) > 1) out[[2]] <- cmp$label
  names(out) <- ids
  for (m in SCENARIO_METRICS) {
    if (!paste0(m, "_before") %in% names(cmp)) next
    out[[paste0(m, "_p1_before")]] <- cmp[[paste0(m, "_before")]]
    out[[paste0(m, "_p1_after")]] <- cmp[[paste0(m, "_after")]]
    out[[paste0(m, "_p1_change")]] <- cmp[[paste0(m, "_chg")]]
  }
  if ("added_min" %in% names(cmp)) {
    out$median_drive_min_current_p1 <- cmp$p1_min_before
    out$median_drive_min_added <- cmp$added_min
    ratio <- cmp$added_min / cmp$p1_min_before
    out$travel_ratio <- ifelse(is.finite(ratio), round(ratio, 3), NA_real_)
  }
  out
}

#' What changed by FOS area, for every facility in one table (a facility's own total row is
#' left out; sections moved in from another facility are the "unassigned" rows).
scenario_export_fos <- function(after, dm_before = NULL, dm_after = NULL) {
  if (is.null(after) || nrow(after) == 0) return(data.frame())
  facs <- sort(unique(c(after$facility_from, after$facility)))
  parts <- lapply(facs, function(f) {
    cmp <- scenario_compare(after, "1", SCENARIO_METRICS, facility = f,
                            dm_before = dm_before, dm_after = dm_after)
    cmp <- cmp[cmp$key != "ALL", , drop = FALSE]
    if (nrow(cmp) == 0) return(NULL)
    cbind(facility = f, scenario_export_compare(cmp, c("fos_area", "fos_name")),
          stringsAsFactors = FALSE)
  })
  do.call(rbind, parts[!vapply(parts, is.null, logical(1))])
}

#' Load per tech and per crew, now and after (staffing_baseline() of each side).
scenario_export_load <- function(b0, b1) {
  if (is.null(b1) || nrow(b1) == 0) return(data.frame())
  cols <- c("ground_sites_per_tech", "ground_acres_per_tech", "air_sites_per_tech",
            "air_acres_per_tech", "ph_acres_per_tech", "ground_acres_per_crew",
            "air_acres_per_crew", "ph_acres_per_crew")
  i0 <- match(b1$facility, b0$facility)
  out <- data.frame(facility = b1$facility, ground_techs = round(b1$ground_techs, 1),
                    stringsAsFactors = FALSE)
  for (k in cols) {
    out[[paste0(k, "_now")]] <- b0[[k]][i0]
    out[[paste0(k, "_after")]] <- b1[[k]]
  }
  out
}

#' Estimate A per FOS area as numbers, with the driver and load per tech it used.
#'
#' @param res scenario_staffing() output
scenario_export_staffing <- function(res) {
  by <- res$by_fos
  if (is.null(by) || nrow(by) == 0) return(data.frame())
  data.frame(
    facility = by$facility, fos_area = by$fosarea, fos_name = by$fos_name,
    driver = res$totals$metric, load_per_tech = res$totals$per_tech,
    techs_now = round(by$n_techs, 2),
    p1_load_now = by$cur_p1, load_removed = by$removed_load, load_added = by$added_load,
    p1_load_after = by$new_p1,
    median_drive_min_current_p1 = by$med_p1_min, median_drive_min_added = by$med_added_min,
    travel_ratio = round(by$travel_ratio, 3), capacity_factor = round(by$capacity_factor, 3),
    techs_needed_now = by$needed_now, techs_needed_after = by$needed_new,
    techs_change = by$techs_change, techs_gap_now = by$gap_now,
    stringsAsFactors = FALSE)
}

#' The sections a scenario changes (promoted to P1 and/or moved), with where they were and
#' are, their loads and the one-way drive minutes from the old and the new facility.
#' Header-only when nothing is changed.
scenario_export_changes <- function(after, dm_before = NULL, dm_after = NULL) {
  cols <- c("sectcode", "facility_from", "zone_from", "facility_to", "zone_to", "promoted",
            "moved", "fos_area_from", "ground_sites", "ground_acres", "air_sites", "air_acres",
            "prehatch_sites", "prehatch_acres", "wet_cb", "drive_min_from", "drive_min_to")
  empty <- as.data.frame(stats::setNames(rep(list(character(0)), length(cols)), cols),
                         stringsAsFactors = FALSE)
  if (is.null(after) || nrow(after) == 0) return(empty)
  ch <- which(after$moved | after$promoted)
  if (length(ch) == 0) return(empty)
  d <- after[ch, , drop = FALSE]
  pick <- function(v) if (is.null(v) || length(v) != nrow(after)) rep(NA_real_, length(ch)) else v[ch]
  out <- data.frame(
    sectcode = d$sectcode, facility_from = d$facility_from, zone_from = paste0("P", d$zone_from),
    facility_to = d$facility, zone_to = paste0("P", d$zone), promoted = d$promoted,
    moved = d$moved, fos_area_from = as.character(d$fosarea),
    ground_sites = d$ground_sites, ground_acres = round(d$ground_acres, 2),
    air_sites = d$air_sites, air_acres = round(d$air_acres, 2),
    prehatch_sites = d$prehatch_sites, prehatch_acres = round(d$prehatch_acres, 2),
    wet_cb = d$wet_cb, drive_min_from = pick(dm_before), drive_min_to = pick(dm_after),
    stringsAsFactors = FALSE)
  out <- out[order(out$facility_from, out$sectcode), ]
  rownames(out) <- NULL
  out
}

#' DT table for the capacity planner with the first `left` column(s) pinned while
#' the rest swipe sideways (phone friendly). Extra arguments go to DT::datatable();
#' `options` is merged over the defaults so callers keep dom / pageLength / ordering.
#'
#' @param data     data.frame to show; the label column(s) must come first
#' @param left     number of leading columns to freeze (0 = none)
#' @param options  DT options list (e.g. pageLength, dom, ordering)
capacity_dt <- function(data, left = 1, options = list(), ...) {
  opts <- utils::modifyList(list(scrollX = TRUE), options)
  extra <- list(...)
  if (left > 0 && ncol(data) > left) {
    opts$fixedColumns <- list(leftColumns = left)
    extra$extensions <- unique(c(extra$extensions, "FixedColumns"))
  }
  do.call(DT::datatable, c(list(data = data, options = opts), extra))
}
