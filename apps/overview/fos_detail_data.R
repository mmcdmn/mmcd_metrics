# FOS Detail Dashboard — Data Functions
# =============================================================================
# All functions take fos_emp_num (numeric ID string) and analysis_date.
# SUCO and catch_basin are loaded fresh here (not from pre-filtered data[[]]).
# =============================================================================

#' Ground prehatch TREATMENT status by township for one FOS area.
#'
#' Uses ground_prehatch_progress app's load_raw_data() — same source as the
#' main ground_prehatch overview metric — so numbers match exactly.
#'
#' @param fos_emp_num  Character emp_num (e.g., "1905")
#' @param analysis_date Date
#' @param zone_filter  Character vector of zones, or NULL for all
#' @return List with $summary (one row per township) and $sites (site-level detail)
load_fos_prehatch_township <- function(fos_emp_num, analysis_date,
                                       zone_filter = NULL) {
  analysis_date <- as.Date(analysis_date)

  app_envs <- tryCatch(get_app_envs(), error = function(e) NULL)
  gp_env <- if (!is.null(app_envs)) app_envs[["ground_prehatch"]] else NULL
  if (is.null(gp_env)) {
    warning("[load_fos_prehatch_township] ground_prehatch env not loaded")
    return(list(summary = data.frame(), sites = data.frame()))
  }

  raw <- tryCatch(
    gp_env$load_raw_data(analysis_date = analysis_date, include_archive = FALSE),
    error = function(e) {
      warning(paste("[load_fos_prehatch_township] load_raw_data:", e$message))
      list(sites = data.frame(), treatments = data.frame())
    }
  )

  sites <- raw$sites
  if (is.null(sites) || nrow(sites) == 0) {
    return(list(summary = data.frame(), sites = data.frame()))
  }

  # Filter to this FOS
  sites <- sites[as.character(sites$fosarea) == fos_emp_num, ]
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    sites <- sites[sites$zone %in% zone_filter, ]
  }
  if (nrow(sites) == 0) {
    return(list(summary = data.frame(), sites = data.frame()))
  }

  # Get township names
  town_lkp <- tryCatch({
    con <- get_db_connection()
    lkp <- dbGetQuery(con, "SELECT towncode, city FROM lookup_towncode_name")
    safe_disconnect(con)
    lkp
  }, error = function(e) data.frame(towncode = character(), city = character()))

  sites$towncode <- substr(sites$sitecode, 1, 4)
  sites <- merge(sites, town_lkp, by = "towncode", all.x = TRUE)
  sites$city <- ifelse(is.na(sites$city), sites$towncode, sites$city)

  summary_df <- sites %>%
    dplyr::group_by(towncode, city) %>%
    dplyr::summarise(
      total   = dplyr::n(),
      treated = sum(is_active, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(pct = round(100 * treated / pmax(total, 1), 1)) %>%
    dplyr::arrange(city)

  list(summary = summary_df, sites = sites)
}

# -----------------------------------------------------------------------------

#' SUCO counts for the whole facility this week (no FOS restriction).
#'
#' @param facility     Facility code (e.g., "Sr")
#' @param analysis_date Date
#' @param zone_filter  Zone filter vector or NULL
#' @return Data frame: fos, display_name, active (one row per FOS with activity)
load_fos_suco <- function(facility, analysis_date, zone_filter = NULL) {
  tryCatch(
    load_data_by_fos("suco",
                     analysis_date    = analysis_date,
                     facility_filter  = facility,
                     fos_filter       = NULL,
                     zone_filter      = if (!is.null(zone_filter)) zone_filter else c("1","2")),
    error = function(e) {
      warning(paste("[load_fos_suco]", e$message))
      data.frame(fos = character(), display_name = character(), active = integer())
    }
  )
}

# -----------------------------------------------------------------------------

#' Catch basin status for one FOS area.
#'
#' @param fos_emp_num  Character emp_num
#' @param facility     Facility code
#' @param analysis_date Date
#' @param zone_filter  Zone filter vector or NULL
#' @return Data frame: fos, total, active, expiring (one row, the current FOS)
load_fos_catch_basin <- function(fos_emp_num, facility, analysis_date,
                                 zone_filter = NULL) {
  all_cb <- tryCatch(
    load_data_by_fos("catch_basin",
                     analysis_date   = analysis_date,
                     facility_filter = facility,
                     fos_filter      = fos_emp_num,
                     zone_filter     = if (!is.null(zone_filter)) zone_filter else c("1","2")),
    error = function(e) {
      warning(paste("[load_fos_catch_basin]", e$message))
      data.frame()
    }
  )
  if (is.null(all_cb) || nrow(all_cb) == 0) return(data.frame())
  all_cb[as.character(all_cb$fos) == fos_emp_num, ]
}

# -----------------------------------------------------------------------------

#' Air work season-to-date status for one FOS area.
#'
#' @param fos_emp_num Character emp_num
#' @param analysis_date Date
#' @return List with $summary (named list) and $sites (data frame, site-level)
load_fos_air_work <- function(fos_emp_num, analysis_date) {
  analysis_date    <- as.Date(analysis_date)
  analysis_date_str <- as.character(analysis_date)
  year_val <- as.integer(format(analysis_date, "%Y"))

  con <- get_db_connection()
  if (is.null(con)) {
    return(list(
      summary = list(total_checked_ac = 0, red_ac = 0, blue_ac = 0,
                     red_total_ac = 0, red_treated_ac = 0,
                     pct_red_done = 0, is_complete = FALSE),
      sites = data.frame()
    ))
  }

  tryCatch({
    site_status <- dbGetQuery(con, sprintf("
      WITH last_insp AS (
        SELECT i.sitecode, MAX(i.inspdate) AS last_insp_date
        FROM dblarv_insptrt_current i
        JOIN gis_sectcode sc ON LEFT(i.sitecode, 7) = sc.sectcode
        WHERE i.action = '4'
          AND sc.fosarea = '%s'
          AND EXTRACT(YEAR FROM i.inspdate) = %d
        GROUP BY i.sitecode
      ),
      last_trt AS (
        SELECT i.sitecode, MAX(i.inspdate) AS last_trt_date
        FROM dblarv_insptrt_current i
        JOIN gis_sectcode sc ON LEFT(i.sitecode, 7) = sc.sectcode
        WHERE i.action IN ('A', 'D')
          AND sc.fosarea = '%s'
          AND EXTRACT(YEAR FROM i.inspdate) = %d
        GROUP BY i.sitecode
      ),
      insp_detail AS (
        SELECT DISTINCT ON (i.sitecode)
               i.sitecode, i.numdip, s.redblue, li.last_insp_date
        FROM last_insp li
        JOIN dblarv_insptrt_current i
             ON i.sitecode = li.sitecode AND i.inspdate = li.last_insp_date
             AND i.action = '4'
        JOIN gis_sectcode sc ON LEFT(i.sitecode, 7) = sc.sectcode
        LEFT JOIN dblarv_sample_current s ON i.sampnum_yr = s.sampnum_yr
        WHERE sc.fosarea = '%s'
        ORDER BY i.sitecode, i.inspdate DESC
      )
      SELECT
        d.sitecode,
        d.redblue,
        d.numdip,
        d.last_insp_date,
        l.acres,
        lt.last_trt_date,
        (lt.last_trt_date IS NOT NULL
         AND lt.last_trt_date >= d.last_insp_date) AS is_treated
      FROM insp_detail d
      JOIN loc_breeding_sites l ON d.sitecode = l.sitecode
      LEFT JOIN last_trt lt ON d.sitecode = lt.sitecode
      WHERE (l.enddate IS NULL OR l.enddate > '%s'::date)
    ", fos_emp_num, year_val, fos_emp_num, year_val, fos_emp_num, analysis_date_str))

    safe_disconnect(con)

    if (nrow(site_status) == 0) {
      return(list(
        summary = list(total_checked_ac = 0, red_ac = 0, blue_ac = 0,
                       red_total_ac = 0, red_treated_ac = 0,
                       pct_red_done = 0, is_complete = FALSE),
        sites = data.frame()
      ))
    }

    site_status$acres      <- as.numeric(site_status$acres)
    site_status$is_treated <- as.logical(site_status$is_treated)
    site_status$is_treated[is.na(site_status$is_treated)] <- FALSE

    total_ac   <- sum(site_status$acres, na.rm = TRUE)
    red_rows   <- site_status[!is.na(site_status$redblue) & site_status$redblue == "R", ]
    blue_rows  <- site_status[!is.na(site_status$redblue) & site_status$redblue == "B", ]

    red_total_ac   <- sum(red_rows$acres, na.rm = TRUE)
    red_treated_ac <- sum(red_rows$acres[red_rows$is_treated], na.rm = TRUE)
    red_untreated  <- sum(red_rows$acres[!red_rows$is_treated], na.rm = TRUE)
    blue_untreated <- sum(blue_rows$acres[!blue_rows$is_treated], na.rm = TRUE)

    pct_red_done <- if (red_total_ac > 0) red_treated_ac / red_total_ac else 0
    is_complete  <- pct_red_done >= 0.90

    list(
      summary = list(
        total_checked_ac = round(total_ac, 1),
        red_ac           = round(if (is_complete) 0 else red_untreated, 1),
        blue_ac          = round(if (is_complete) 0 else blue_untreated, 1),
        red_total_ac     = round(red_total_ac, 1),
        red_treated_ac   = round(red_treated_ac, 1),
        pct_red_done     = round(100 * pct_red_done, 1),
        is_complete      = is_complete
      ),
      sites = site_status[order(site_status$redblue, site_status$sitecode), ]
    )

  }, error = function(e) {
    safe_disconnect(con)
    warning(paste("[load_fos_air_work] Error:", e$message))
    list(
      summary = list(total_checked_ac = 0, red_ac = 0, blue_ac = 0,
                     red_total_ac = 0, red_treated_ac = 0,
                     pct_red_done = 0, is_complete = FALSE),
      sites = data.frame()
    )
  })
}

# -----------------------------------------------------------------------------

#' Bioassay (action='P') counts by FOS for the current week.
#'
#' @param facility     Facility code
#' @param analysis_date Date — week start computed as Monday
#' @return Data frame: fosarea, shortname, n
load_fos_bioassays <- function(facility, analysis_date) {
  analysis_date <- as.Date(analysis_date)
  week_start    <- analysis_date - as.integer(format(analysis_date, "%u")) + 1L
  week_start_str <- as.character(week_start)

  con <- get_db_connection()
  if (is.null(con)) {
    return(data.frame(fosarea = character(), shortname = character(), n = integer()))
  }

  tryCatch({
    result <- dbGetQuery(con, sprintf("
      SELECT sc.fosarea, el.shortname,
             COUNT(*) AS n,
             SUM(CASE WHEN i.numdip > 0 THEN 1 ELSE 0 END) AS n_with_pupae
      FROM dblarv_insptrt_current i
      JOIN gis_sectcode sc ON LEFT(i.sitecode, 7) = sc.sectcode
      LEFT JOIN employee_list el
             ON el.emp_num = sc.fosarea
            AND el.facility = sc.facility
            AND el.active = true
      WHERE i.action = 'P'
        AND sc.facility = '%s'
        AND i.inspdate >= '%s'::date
      GROUP BY sc.fosarea, el.shortname
      ORDER BY n DESC, sc.fosarea
    ", facility, week_start_str))

    safe_disconnect(con)
    result$n <- as.integer(result$n)
    result

  }, error = function(e) {
    safe_disconnect(con)
    warning(paste("[load_fos_bioassays] Error:", e$message))
    data.frame(fosarea = character(), shortname = character(), n = integer())
  })
}
