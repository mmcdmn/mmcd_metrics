# Inspections App - Coverage Functions
# =============================================================================
# THE 51% RULE
#
#   At least 51% of ground PREHATCH sites should have a red-bug sample in the
#   last 5 years OR have been sampled at all in the current year.
#
# A site "counts" toward the goal when EITHER:
#   - red_5yr           a sample with redblue = 'R' on an inspection dated in
#                       [as_of - 5 years, as_of]
#   - sampled_this_year any inspection in as_of's calendar year that produced a
#                       sample number, whatever the result (red, blue, no bugs,
#                       or not yet entered). Temporary: resets on Jan 1.
#
# Everything is derived from ONE per-site table (load_prehatch_site_coverage)
# so the app, the Overview tile and the API cannot disagree.
#
# Denominator: ground sites (air_gnd = 'G') still active on as_of with a real
# prehatch type. BTI-only sites are not prehatch sites. Drone sites stay in.
#
# Sample-bearing visits are action 1/2/4. Action 'P' (bioassay pupae) also counts,
# but ONLY when it has a sample number AND numdip (larvae per dip) > 0: a bioassay
# that found nothing says nothing about the site.
#
# Sourced by data_functions.R (so the Overview, API and embeds get it too).
# =============================================================================

COVERAGE_WINDOW_YEARS <- 5L
COVERAGE_PREHATCH_TYPES <- c("PREHATCH", "PELLET", "BRIQUET", "PRE1ONLY")
COVERAGE_VISIT_ACTIONS <- c("1", "2", "4")          # inspections (can carry a sample)
COVERAGE_BIOASSAY_ACTION <- "P"                      # pupae bioassay: a sample only if numdip > 0
COVERAGE_WET_CODES <- c("0", as.character(1:9), "A") # 0 = dry, 1-9 = % wet, A = flooded

# The two sides of the goal, as staff see them. Every label the app builds comes
# from these two words, so renaming them here renames them everywhere in the app.
# (The Overview tile's strings are plain text in apps/overview/metric_registry.R
# and need the same edit.)
COVERAGE_LABEL_COUNTING <- "Sampled"
COVERAGE_LABEL_NOT_COUNTING <- "Missing"

# A sample taken this year whose result (red/blue) is not entered yet. Once the
# ID is in the site is no longer ID pending - it is just Sampled or Missing.
COVERAGE_LABEL_ID_PENDING <- "ID pending"

# Statuses in display order. First two count toward the goal.
COVERAGE_STATUS_LEVELS <- c("red_5yr", "sampled_this_year", "red_expired",
                            "no_red_sampled", "inspected_no_sample", "not_inspected")
COVERAGE_STATUS_LABELS <- c(
  red_5yr             = "Red bug in last 5 yrs",
  sampled_this_year   = "Sampled this year (temporary)",
  red_expired         = "Last red bug over 5 yrs ago",
  no_red_sampled      = "Sampled before this year, never red",
  inspected_no_sample = "Inspected, no sample taken",
  not_inspected       = "Not inspected in 5 yrs"
)

#' SQL condition: this visit can carry a sample. Inspections (action 1/2/4), or a
#' pupae bioassay that found something (numdip > 0). A NULL numdip is not > 0.
#' @param prefix table alias including the dot, e.g. "i." (empty for a CTE column)
.sample_action_sql <- function(prefix = "") {
  sprintf("(%1$saction IN (%2$s) OR (%1$saction = '%3$s' AND %1$snumdip > 0))",
          prefix, build_sql_in_list(COVERAGE_VISIT_ACTIONS), COVERAGE_BIOASSAY_ACTION)
}

# Thresholds live in config/app_config.yaml (thresholds.fixed_pct.prehatch_coverage)
.coverage_threshold <- function(key, default) {
  thr <- if (exists("get_config_threshold", mode = "function")) {
    tryCatch(get_config_threshold("fixed_pct", "prehatch_coverage"), error = function(e) NULL)
  } else NULL
  value <- suppressWarnings(as.numeric(thr[[key]]))
  if (length(value) != 1 || is.na(value)) default else value
}

#' Goal percentage (51)
coverage_goal_pct <- function() .coverage_threshold("good", 51)

#' Below this the tile turns red (41)
coverage_warning_pct <- function() .coverage_threshold("warning", 41)

#' Percent truncated to 1 decimal so a miss is never rounded up to the goal
#' (50.96 -> 50.9, not 51.0). Same formula as the Overview tile (pct_exact).
coverage_pct_label <- function(pct) floor(pct * 10 + 1e-9) / 10

#' First day of the 5-year window. lubridate rolls Feb 29 back to Feb 28,
#' matching Postgres interval arithmetic (and avoiding NA).
coverage_window_start <- function(as_of) {
  lubridate::`%m-%`(as.Date(as_of), lubridate::years(COVERAGE_WINDOW_YEARS))
}

empty_site_coverage <- function() {
  data.frame(
    sitecode = character(), facility = character(), fosarea = character(), zone = character(),
    priority = character(), drone = character(), acres = numeric(), prehatch = character(),
    insp_5y = integer(), insp_this_year = integer(), wet_n = integer(), wet_count = integer(),
    last_insp_date = as.Date(character()), last_wet_date = as.Date(character()),
    samples_5y = integer(), samples_this_year = integer(), pending_this_year = integer(),
    last_sample_date = as.Date(character()), last_sample_result = character(),
    last_red_date_5y = as.Date(character()), last_red_older = as.Date(character()),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# DATA LOAD
# =============================================================================

#' One row per denominator site with the raw coverage facts (no judgement yet)
#'
#' All facilities/zones are returned; filter in R afterwards so one cached copy
#' serves every user and the Overview.
#'
#' @param as_of Date to evaluate the rule on (default today)
#' @param prehatch_types prehatch values in the denominator; NULL = any non-NULL
#'   value (the pre-rewrite dashboard definition - only used for parity checks)
#' @return data.frame, then pass through classify_site_coverage()
load_prehatch_site_coverage <- function(as_of = Sys.Date(),
                                        prehatch_types = COVERAGE_PREHATCH_TYPES) {
  as_of <- as.Date(as_of)
  window_start <- coverage_window_start(as_of)

  con <- get_db_connection()
  if (is.null(con)) return(empty_site_coverage())

  tryCatch({
    prehatch_sql <- if (is.null(prehatch_types)) {
      "b.prehatch IS NOT NULL"
    } else {
      paste0("b.prehatch IN (", build_sql_in_list(prehatch_types), ")")
    }
    visit_sql <- build_sql_in_list(COVERAGE_VISIT_ACTIONS)
    wet_sql <- build_sql_in_list(COVERAGE_WET_CODES)
    sample_sql <- .sample_action_sql()          # flagged CTE (bare column names)
    older_sample_sql <- .sample_action_sql("i.") # older_red CTE (aliased)

    qry <- sprintf("
      WITH den AS (
        SELECT b.sitecode, sc.facility, sc.fosarea, sc.zone,
               b.priority, b.drone, b.acres, b.prehatch
        FROM loc_breeding_sites b
        INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
        WHERE (b.enddate IS NULL OR b.enddate > '%1$s'::date)
          AND b.air_gnd = 'G'
          AND %3$s
      ),
      raw_visits AS (
        SELECT i.sitecode, i.inspdate, i.action, i.wet, i.numdip, i.sampnum_yr, s.redblue
        FROM dblarv_insptrt_current i
        LEFT JOIN dblarv_sample_current s ON s.sampnum_yr = i.sampnum_yr
        WHERE i.inspdate BETWEEN '%2$s'::date AND '%1$s'::date
          AND i.sitecode IN (SELECT sitecode FROM den)
        UNION ALL
        SELECT i.sitecode, i.inspdate, i.action, i.wet, i.numdip, i.sampnum_yr, s.redblue
        FROM dblarv_insptrt_archive i
        LEFT JOIN dblarv_sample_archive s ON s.sampnum_yr = i.sampnum_yr
        WHERE i.inspdate BETWEEN '%2$s'::date AND '%1$s'::date
          AND i.sitecode IN (SELECT sitecode FROM den)
      ),
      flagged AS (
        SELECT sitecode, inspdate, sampnum_yr, redblue,
               (action IN (%4$s) OR (action = '3' AND wet = '0'))      AS is_insp,
               (action IN (%4$s) AND wet IN (%5$s))                    AS is_visit,
               (action IN (%4$s) AND wet IN (%5$s) AND wet <> '0')     AS is_wet,
               (%6$s AND sampnum_yr IS NOT NULL)                       AS is_sample,
               (inspdate >= date_trunc('year', '%1$s'::date)::date)    AS this_year
        FROM raw_visits
      ),
      agg AS (
        SELECT sitecode,
          COUNT(*) FILTER (WHERE is_insp)::int                                   AS insp_5y,
          COUNT(*) FILTER (WHERE is_insp AND this_year)::int                     AS insp_this_year,
          COUNT(*) FILTER (WHERE is_visit)::int                                  AS wet_n,
          COUNT(*) FILTER (WHERE is_wet)::int                                    AS wet_count,
          MAX(inspdate) FILTER (WHERE is_insp)                                   AS last_insp_date,
          MAX(inspdate) FILTER (WHERE is_wet)                                    AS last_wet_date,
          COUNT(DISTINCT sampnum_yr) FILTER (WHERE is_sample)::int               AS samples_5y,
          COUNT(DISTINCT sampnum_yr) FILTER (WHERE is_sample AND this_year)::int AS samples_this_year,
          COUNT(DISTINCT sampnum_yr) FILTER (WHERE is_sample AND this_year
                                             AND COALESCE(redblue,'') = '')::int AS pending_this_year,
          MAX(inspdate) FILTER (WHERE is_sample)                                 AS last_sample_date,
          (array_agg(redblue ORDER BY inspdate DESC, sampnum_yr DESC)
             FILTER (WHERE is_sample))[1]                                        AS last_sample_result,
          MAX(inspdate) FILTER (WHERE is_sample AND redblue = 'R')               AS last_red_date_5y
        FROM flagged
        GROUP BY sitecode
      ),
      older_red AS (
        SELECT i.sitecode, MAX(i.inspdate) AS last_red_older
        FROM dblarv_insptrt_archive i
        INNER JOIN dblarv_sample_archive s ON s.sampnum_yr = i.sampnum_yr
        WHERE s.redblue = 'R'
          AND %7$s
          AND i.inspdate < '%2$s'::date
          AND i.sitecode IN (SELECT sitecode FROM den)
        GROUP BY i.sitecode
      )
      SELECT d.sitecode, d.facility, d.fosarea, d.zone, d.priority, d.drone, d.acres, d.prehatch,
             COALESCE(a.insp_5y, 0)           AS insp_5y,
             COALESCE(a.insp_this_year, 0)    AS insp_this_year,
             COALESCE(a.wet_n, 0)             AS wet_n,
             COALESCE(a.wet_count, 0)         AS wet_count,
             a.last_insp_date, a.last_wet_date,
             COALESCE(a.samples_5y, 0)        AS samples_5y,
             COALESCE(a.samples_this_year, 0) AS samples_this_year,
             COALESCE(a.pending_this_year, 0) AS pending_this_year,
             a.last_sample_date, a.last_sample_result,
             a.last_red_date_5y, o.last_red_older
      FROM den d
      LEFT JOIN agg a ON a.sitecode = d.sitecode
      LEFT JOIN older_red o ON o.sitecode = d.sitecode
    ", as.character(as_of), as.character(window_start), prehatch_sql, visit_sql, wet_sql,
    sample_sql, older_sample_sql)

    result <- dbGetQuery(con, qry)
    safe_disconnect(con)

    if (nrow(result) == 0) return(empty_site_coverage())

    for (col in c("last_insp_date", "last_wet_date", "last_sample_date",
                  "last_red_date_5y", "last_red_older")) {
      result[[col]] <- as.Date(result[[col]])
    }
    for (col in c("insp_5y", "insp_this_year", "wet_n", "wet_count",
                  "samples_5y", "samples_this_year", "pending_this_year")) {
      result[[col]] <- as.integer(result[[col]])
    }
    result
  }, error = function(e) {
    warning(paste("Error in load_prehatch_site_coverage:", e$message))
    if (!is.null(con)) tryCatch(safe_disconnect(con), error = function(e2) NULL)
    empty_site_coverage()
  })
}

#' Add fos_name (foreman shortname) from the 4-digit gis_sectcode.fosarea code
add_fos_names <- function(df) {
  if (nrow(df) == 0) { df$fos_name <- character(0); return(df) }
  foremen <- get_foremen_lookup()
  if (nrow(foremen) > 0) {
    fosarea_map <- setNames(foremen$shortname, sprintf("%04d", as.numeric(foremen$emp_num)))
    df$fos_name <- ifelse(df$fosarea %in% names(fosarea_map), fosarea_map[df$fosarea], df$fosarea)
  } else {
    df$fos_name <- df$fosarea
  }
  df
}

# =============================================================================
# CLASSIFICATION (pure - no database)
# =============================================================================

#' Decide, per site, whether it counts toward the goal and why / why not
#'
#' @param df output of load_prehatch_site_coverage()
#' @param as_of date the rule is evaluated on
#' @return df plus: last_red_date, has_red_5yr, counts, id_pending, status,
#'   status_label, counts_through, days_left, drops_because, wet_pct,
#'   likely_to_yield, reason
classify_site_coverage <- function(df, as_of = Sys.Date()) {
  as_of <- as.Date(as_of)
  window_start <- coverage_window_start(as_of)

  for (col in c("last_red_date_5y", "last_red_older", "last_sample_date", "last_insp_date", "last_wet_date")) {
    df[[col]] <- if (col %in% names(df)) as.Date(df[[col]]) else rep(as.Date(NA), nrow(df))
  }
  if (!"last_sample_result" %in% names(df)) df$last_sample_result <- rep(NA_character_, nrow(df))

  iso <- function(d) ifelse(is.na(d), "never", format(d, "%Y-%m-%d"))
  year_end <- as.Date(sprintf("%d-12-31", as.integer(format(as_of, "%Y"))))

  # Dates are handled as plain numbers for pmax/ifelse so the Date class survives
  num <- function(d) as.numeric(d)
  as_date <- function(x) as.Date(x, origin = "1970-01-01")

  df <- df %>%
    mutate(
      last_red_date = as_date(pmax(num(last_red_date_5y), num(last_red_older), na.rm = TRUE)),
      has_red_5yr = !is.na(last_red_date) & last_red_date >= window_start,
      sampled_this_year = samples_this_year > 0,
      status = case_when(
        has_red_5yr                  ~ "red_5yr",
        sampled_this_year            ~ "sampled_this_year",
        !is.na(last_red_date)        ~ "red_expired",
        samples_5y > 0               ~ "no_red_sampled",
        insp_5y > 0                  ~ "inspected_no_sample",
        TRUE                         ~ "not_inspected"
      ),
      counts = status %in% c("red_5yr", "sampled_this_year"),
      # A sample taken this year whose result is not entered yet. Says nothing about
      # the status: a site that already has a red bug can have one pending too.
      id_pending = coalesce(sampled_this_year, FALSE) & coalesce(pending_this_year, 0L) > 0,

      # Last day this site keeps counting, and what ends it
      red_through = lubridate::`%m+%`(last_red_date, lubridate::years(COVERAGE_WINDOW_YEARS)),
      red_through = if_else(has_red_5yr, red_through, as.Date(NA)),
      sample_through = if_else(sampled_this_year, year_end, as.Date(NA)),
      counts_through = as_date(pmax(num(red_through), num(sample_through), na.rm = TRUE)),
      drops_because = case_when(
        !counts                                                      ~ NA_character_,
        is.na(red_through)                                           ~ "This year's sample (resets Jan 1)",
        is.na(sample_through) | num(red_through) >= num(sample_through) ~ "Red bug ages out of 5-yr window",
        TRUE                                                         ~ "This year's sample (resets Jan 1)"
      ),
      days_left = as.integer(counts_through - as_of),

      wet_pct = ifelse(wet_n > 0, round(100 * wet_count / wet_n), NA_real_),
      likely_to_yield = case_when(
        wet_n < 2     ~ "Unknown",
        wet_pct >= 50 ~ "High",
        wet_pct >= 20 ~ "Medium",
        TRUE          ~ "Low"
      ),

      result_label = case_when(
        last_sample_result %in% "R" ~ "red",
        last_sample_result %in% "B" ~ "blue",
        TRUE                        ~ "result not entered yet"
      ),
      wet_text = ifelse(wet_n > 0, sprintf("wet %d of %d visits", wet_count, wet_n), "no wet readings"),
      reason = case_when(
        status == "red_5yr" ~
          sprintf("Red bug %s - stays %s through %s%s", iso(last_red_date),
                  COVERAGE_LABEL_COUNTING, iso(counts_through),
                  ifelse(id_pending,
                         sprintf(" (%s for %d sample(s) taken this year)", COVERAGE_LABEL_ID_PENDING, pending_this_year),
                         "")),
        id_pending ~
          sprintf("Sampled %s - %s for %d of %d sample(s) this year - drops to %s on Jan 1 unless a red bug is found",
                  iso(last_sample_date), COVERAGE_LABEL_ID_PENDING, pending_this_year, samples_this_year,
                  COVERAGE_LABEL_NOT_COUNTING),
        status == "sampled_this_year" ~
          sprintf("Sampled %s (%s) - drops to %s on Jan 1 unless a red bug is found",
                  iso(last_sample_date), result_label, COVERAGE_LABEL_NOT_COUNTING),
        status == "red_expired" ~
          sprintf("Last red bug %s is over 5 years ago", iso(last_red_date)),
        status == "no_red_sampled" ~
          sprintf("Sampled %d time(s) in 5 yrs, last %s (%s) - never red, and not sampled this year",
                  samples_5y, iso(last_sample_date), result_label),
        status == "inspected_no_sample" ~
          sprintf("Inspected %d time(s), last %s, but no sample taken (%s)",
                  insp_5y, iso(last_insp_date), wet_text),
        TRUE ~ "No inspection in the last 5 years"
      ),
      status = factor(status, levels = COVERAGE_STATUS_LEVELS),
      status_label = factor(COVERAGE_STATUS_LABELS[as.character(status)],
                            levels = unname(COVERAGE_STATUS_LABELS))
    ) %>%
    select(-red_through, -sample_through, -result_label, -wet_text)

  as.data.frame(df, stringsAsFactors = FALSE)
}

# =============================================================================
# SUMMARIES
# =============================================================================

#' Counting / not counting, exact percent, and how many more sites reach the goal
#'
#' @param df classified sites (classify_site_coverage output), already filtered
#' @param by grouping columns, e.g. "facility" or c("facility","fosarea","zone");
#'   character(0) gives one overall row
#' @param goal goal percent (default from config, 51)
summarise_coverage <- function(df, by = character(0), goal = coverage_goal_pct()) {
  grouped <- if (length(by) > 0) group_by(df, across(all_of(by))) else df
  out <- grouped %>%
    summarise(
      total = n(),
      counting = sum(counts),
      via_red = sum(status == "red_5yr"),
      via_sample_only = sum(status == "sampled_this_year"),
      .groups = "drop"
    )
  out %>%
    mutate(
      not_counting = total - counting,
      pct = ifelse(total > 0, 100 * counting / total, 0),
      # small epsilon: goal * total / 100 can land a hair above an integer
      sites_for_goal = as.integer(ceiling(goal * total / 100 - 1e-9)),
      needed = pmax(0L, sites_for_goal - counting),
      cushion = pmax(0L, counting - sites_for_goal),
      goal_met = counting >= sites_for_goal
    ) %>%
    as.data.frame()
}

#' Sites that will stop counting, by month, for the next `months` months
#'
#' @return data.frame(month = first of month, drops_because, n), zero-filled
aging_out_by_month <- function(df, as_of = Sys.Date(), months = 12) {
  as_of <- as.Date(as_of)
  first_month <- as.Date(format(as_of, "%Y-%m-01"))
  grid <- expand.grid(
    month = seq(first_month, by = "month", length.out = months),
    drops_because = c("Red bug ages out of 5-yr window", "This year's sample (resets Jan 1)"),
    stringsAsFactors = FALSE
  )
  # A site stops counting the day AFTER counts_through
  dropping <- df[df$counts & !is.na(df$counts_through), , drop = FALSE]
  if (nrow(dropping) > 0) {
    dropping$month <- as.Date(format(dropping$counts_through + 1, "%Y-%m-01"))
    counts <- dropping %>% count(month, drops_because, name = "n")
    grid <- merge(grid, counts, by = c("month", "drops_because"), all.x = TRUE)
  } else {
    grid$n <- 0L
  }
  grid$n[is.na(grid$n)] <- 0L
  grid[order(grid$month, grid$drops_because), ]
}

# =============================================================================
# OVERVIEW REGISTRY CONTRACT
# =============================================================================

#' facility x fosarea x zone counts in the shape the Overview expects
#'
#' total_count = prehatch sites, active_count = sites counting toward the goal,
#' expiring_count = sites not counting. Raw codes ("Sr", "1904", "1") - the
#' Overview filters zones and maps FOS areas itself.
get_coverage_overview_counts <- function(analysis_date = NULL) {
  analysis_date <- if (is.null(analysis_date)) Sys.Date() else as.Date(analysis_date)

  empty <- list(
    sites = data.frame(
      facility = character(), fosarea = character(), zone = character(),
      total_count = integer(), active_count = integer(), expiring_count = integer()
    ),
    treatments = data.frame(),
    pre_aggregated = TRUE
  )

  tryCatch({
    sites <- classify_site_coverage(load_prehatch_site_coverage(analysis_date), analysis_date)
    if (nrow(sites) == 0) return(empty)

    agg <- sites %>%
      group_by(facility, fosarea, zone) %>%
      summarise(
        total_count = n(),
        active_count = sum(counts),
        expiring_count = sum(!counts),
        .groups = "drop"
      ) %>%
      as.data.frame()

    message(sprintf("[prehatch_coverage] Overview: %s groups, %s sites, %s counting (%.1f%%), %s only via this year's sample",
                    nrow(agg), sum(agg$total_count), sum(agg$active_count),
                    100 * sum(agg$active_count) / max(1, sum(agg$total_count)),
                    sum(sites$status == "sampled_this_year")))

    list(sites = agg, treatments = data.frame(), pre_aggregated = TRUE)
  }, error = function(e) {
    warning(paste("[prehatch_coverage] Overview counts failed:", e$message))
    empty
  })
}
