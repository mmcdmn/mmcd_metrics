# Air Inspection Checklist - Data Functions
# Retrieves RED air sites and recent inspection data for checklist display

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(DBI)
})

# Source shared helpers (only if not already loaded)
if (!exists("get_db_connection", mode = "function")) {
  source("../../shared/db_helpers.R")
}

#' Get RED air sites with recent inspection status for checklist
#'
#' @param facility_filter Character vector of facility short codes (or NULL for all)
#' @param foreman_filter Character - single FOS emp_num (or NULL for all)
#' @param lookback_days Integer, number of days to look back for inspections (default 2)
#' @param analysis_date Date to use as "today" (default Sys.Date())
#' @param zone_filter Character vector of zones to include (default "1")
#' @param include_active_treatment Logical, include prehatch treatment sites (default FALSE)
#' @return Data frame with one row per RED air site and inspection status
get_checklist_data <- function(facility_filter = NULL,
                               foreman_filter = NULL,
                               lookback_days = 2,
                               analysis_date = Sys.Date(),
                               zone_filter = "1",
                               priority_filter = "RED",
                               include_active_treatment = FALSE) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())

  tryCatch({
    # Build filter conditions
    facility_condition <- ""
    if (is_valid_filter(facility_filter)) {
      facility_list <- build_sql_in_list(facility_filter)
      facility_condition <- sprintf("AND sc.facility IN (%s)", facility_list)
    }

    foreman_condition <- ""
    if (is_valid_filter(foreman_filter)) {
      foreman_list <- build_sql_in_list(foreman_filter)
      foreman_condition <- sprintf("AND sc.fosarea IN (%s)", foreman_list)
    }

    # Zone filter
    zone_condition <- ""
    if (!is.null(zone_filter) && length(zone_filter) > 0) {
      zone_list <- build_sql_in_list(zone_filter)
      zone_condition <- sprintf("AND sc.zone IN (%s)", zone_list)
    }

    # Priority filter
    priority_condition <- "AND b.priority = 'RED'"  # safe default
    if (!is.null(priority_filter) && length(priority_filter) > 0) {
      priority_list <- build_sql_in_list(priority_filter)
      priority_condition <- sprintf("AND b.priority IN (%s)", priority_list)
    }

    # Determine which inspection table to use
    table_info <- get_table_strategy(analysis_date)
    inspection_table <- if (table_info$query_archive) "dblarv_insptrt_archive" else "dblarv_insptrt_current"
    sample_table <- if (table_info$query_archive) "dblarv_sample_archive" else "dblarv_sample_current"

    lookback_start <- as.character(as.Date(analysis_date) - lookback_days)

    query <- sprintf("
      WITH RedAirSites AS (
        SELECT DISTINCT ON (b.sitecode)
          b.sitecode,
          b.acres,
          sc.facility,
          sc.zone,
          sc.fosarea,
          sc.sectcode
        FROM loc_breeding_sites b
        LEFT JOIN gis_sectcode sc ON LEFT(b.sitecode, 7) = sc.sectcode
        WHERE (b.enddate IS NULL OR b.enddate > '%s')
          AND b.air_gnd = 'A'
          %s
          %s
          %s
          %s
        ORDER BY b.sitecode, b.enddate NULLS LAST
      ),

      -- Get inspections within the lookback window (actions 2, 4 = inspections)
      RecentInspections AS (
        SELECT
          i.sitecode,
          i.inspdate,
          i.numdip,
          i.emp1,
          i.sampnum_yr,
          ROW_NUMBER() OVER (PARTITION BY i.sitecode ORDER BY i.inspdate DESC) AS rn
        FROM %s i
        WHERE i.inspdate BETWEEN '%s'::date AND '%s'::date
          AND i.action IN ('2', '4')
          AND i.sitecode IN (SELECT sitecode FROM RedAirSites)
      ),

      -- Get most recent treatment per site to determine active treatment
      RecentTreatments AS (
        SELECT
          t.sitecode,
          t.inspdate AS last_treatment_date,
          t.matcode,
          t.mattype,
          COALESCE(mt.effect_days, 14) AS effect_days,
          t.inspdate + INTERVAL '1 day' * COALESCE(mt.effect_days, 14) AS treatment_expiry,
          COALESCE(mt.prehatch, FALSE) AS is_prehatch,
          ROW_NUMBER() OVER (PARTITION BY t.sitecode ORDER BY t.inspdate DESC) AS rn
        FROM %s t
        LEFT JOIN mattype_list_targetdose mt ON t.matcode = mt.matcode
        WHERE t.inspdate <= '%s'::date
          AND t.action IN ('3', 'A', 'D')
          AND t.matcode IS NOT NULL AND t.matcode != ''
          AND t.sitecode IN (SELECT sitecode FROM RedAirSites)
      ),
      ActiveTreatmentSites AS (
        SELECT sitecode, mattype AS active_material,
               last_treatment_date AS active_trt_date,
               treatment_expiry AS active_trt_expiry,
               is_prehatch
        FROM RecentTreatments
        WHERE rn = 1 AND treatment_expiry > '%s'::date
      ),

      -- Get lab sample results for red/blue bug determination
      LabResults AS (
        SELECT
          ls.sampnum_yr,
          ls.redblue,
          ls.missing
        FROM %s ls
        WHERE ls.sampnum_yr IS NOT NULL
      ),

      -- Deduplicated employee lookup (one row per emp_num, prefer active)
      EmployeeLookup AS (
        SELECT DISTINCT ON (emp_num) emp_num, shortname
        FROM employee_list
        WHERE active = true
        ORDER BY emp_num, pkey DESC
      )

      SELECT
        s.sitecode,
        s.acres,
        s.facility,
        s.zone,
        s.fosarea,
        s.sectcode,
        cards.airmap_num,
        i.inspdate AS last_insp_date,
        i.numdip AS dip_count,
        i.emp1 AS inspector_emp,
        emp.shortname AS inspector_name,
        i.sampnum_yr,
        lr.redblue,
        lr.missing AS lab_missing,
        CASE WHEN i.sitecode IS NOT NULL THEN TRUE ELSE FALSE END AS was_inspected,
        CASE WHEN ats.sitecode IS NOT NULL THEN TRUE ELSE FALSE END AS has_active_treatment,
        ats.active_material,
        ats.active_trt_date,
        ats.active_trt_expiry,
        COALESCE(ats.is_prehatch, FALSE) AS is_prehatch
      FROM RedAirSites s
      LEFT JOIN RecentInspections i ON s.sitecode = i.sitecode AND i.rn = 1
      LEFT JOIN LabResults lr ON i.sampnum_yr = lr.sampnum_yr
      LEFT JOIN ActiveTreatmentSites ats ON s.sitecode = ats.sitecode
      LEFT JOIN \"loc_breeding_site_cards_sjsreast2\" cards ON s.sitecode = cards.sitecode
      LEFT JOIN EmployeeLookup emp ON i.emp1 = emp.emp_num::text
      ORDER BY s.fosarea, cards.airmap_num NULLS LAST, s.sectcode, s.sitecode
    ",
      as.character(analysis_date),
      priority_condition,
      facility_condition,
      foreman_condition,
      zone_condition,
      inspection_table,
      lookback_start,
      as.character(analysis_date),
      inspection_table,
      as.character(analysis_date),
      as.character(analysis_date),
      sample_table
    )

    result <- dbGetQuery(con, query)
    safe_disconnect(con)

    if (nrow(result) == 0) return(data.frame())

    # Filter out prehatch treatment sites unless toggle is on
    if (!include_active_treatment) {
      result <- result[!(result$has_active_treatment & result$is_prehatch), ]
      if (nrow(result) == 0) return(data.frame())
    }

    # Add FOS display name
    foremen_lookup <- get_foremen_lookup()
    result$fos_display <- get_foreman_display_names(result$fosarea, lookup = foremen_lookup)

    # Add AirMap display label (for sub-grouping)
    result$airmap_display <- ifelse(
      is.na(result$airmap_num) | result$airmap_num == "",
      "No AirMap",
      paste("AirMap", result$airmap_num)
    )

    # Inspector display: use employee name if available, fall back to emp number
    result$inspector_display <- ifelse(
      !is.na(result$inspector_name) & result$inspector_name != "",
      result$inspector_name,
      ifelse(
        !is.na(result$inspector_emp) & result$inspector_emp != "",
        paste0("Emp #", result$inspector_emp),
        ""
      )
    )

    # Add bug status column
    result$bug_status <- ifelse(
      !result$was_inspected | is.na(result$sampnum_yr) | result$sampnum_yr == "",
      "No Sample",
      ifelse(
        is.na(result$lab_missing) | result$lab_missing == TRUE,
        "Pending Lab",
        ifelse(
          !is.na(result$redblue) & result$redblue == "R",
          "Red Bugs",
          ifelse(
            !is.na(result$redblue) & result$redblue == "B",
            "Blue Bugs",
            "No Bugs"
          )
        )
      )
    )

    return(result)
  }, error = function(e) {
    cat("ERROR in get_checklist_data:", e$message, "\n")
    if (!is.null(con)) safe_disconnect(con)
    return(data.frame())
  })
}


#' Summarize checklist data for display
#' @param data Data frame from get_checklist_data()
#' @return List of summary statistics
summarize_checklist <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      total_sites = 0,
      inspected = 0,
      not_inspected = 0,
      pct_complete = "0%",
      red_bugs = 0,
      blue_bugs = 0
    ))
  }

  total <- nrow(data)
  inspected <- sum(data$was_inspected, na.rm = TRUE)
  not_inspected <- total - inspected

  pct <- if (total > 0) round((inspected / total) * 100, 1) else 0

  red_bugs <- sum(data$bug_status == "Red Bugs", na.rm = TRUE)
  blue_bugs <- sum(data$bug_status == "Blue Bugs", na.rm = TRUE)

  list(
    total_sites = total,
    inspected = inspected,
    not_inspected = not_inspected,
    pct_complete = paste0(pct, "%"),
    red_bugs = red_bugs,
    blue_bugs = blue_bugs
  )
}


#' Get active field employees grouped by facility and FOS
#' Used to generate employees.json for the index page employee picker
#' @return Data frame with emp_num, shortname, facility, fieldsuper, fos_name
get_field_employees <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())

  tryCatch({
    employees <- dbGetQuery(con, "
      SELECT e.emp_num, e.shortname, e.emp_type, e.facility, e.fieldsuper,
             f.shortname AS fos_name
      FROM employee_list e
      LEFT JOIN employee_list f ON e.fieldsuper = f.emp_num
        AND f.active = true AND f.emp_type = 'FieldSuper'
      WHERE e.active = true
        AND e.fieldsuper IS NOT NULL
        AND e.emp_type NOT IN ('Pilot', 'Insp-Recpt', 'Insp-Lab')
      ORDER BY e.facility, e.shortname
    ")
    safe_disconnect(con)
    return(employees)
  }, error = function(e) {
    cat("ERROR in get_field_employees:", e$message, "\n")
    if (!is.null(con)) safe_disconnect(con)
    return(data.frame())
  })
}


# =============================================================================
# CLAIM FUNCTIONS (Redis-backed, shared across all workers)
# =============================================================================

CLAIM_HASH_PREFIX <- "claim"
CLAIM_TTL <- 172800L  # 2 days

#' Build Redis hash key for claims on a given date
#' @param date Character date "YYYY-MM-DD"
#' @return Key like "claim:2026-03-13"
claim_hash_key <- function(date) {
  paste0(CLAIM_HASH_PREFIX, ":", date)
}

#' Set a claim for a sitecode on a given date
#' @param sitecode Character sitecode
#' @param emp_num Character employee number
#' @param emp_name Character employee display name
#' @param date Character date "YYYY-MM-DD" (default today)
#' @return TRUE/FALSE
set_claim <- function(sitecode, emp_num, emp_name, date = format(Sys.Date(), "%Y-%m-%d")) {
  claim_data <- list(
    emp_num  = emp_num,
    emp_name = emp_name,
    time     = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  redis_hset(claim_hash_key(date), sitecode, claim_data, ttl = CLAIM_TTL)
}

#' Remove a claim for a sitecode on a given date
#' @param sitecode Character sitecode
#' @param date Character date "YYYY-MM-DD" (default today)
#' @return Number deleted
remove_claim <- function(sitecode, date = format(Sys.Date(), "%Y-%m-%d")) {
  redis_hdel(claim_hash_key(date), sitecode)
}

#' Get all claims within a lookback window
#' @param lookback_days Integer number of days to look back
#' @param analysis_date Date to use as reference (default Sys.Date())
#' @return Named list: sitecode -> list(emp_num, emp_name, time, claim_date)
get_claims <- function(lookback_days = 2, analysis_date = Sys.Date()) {
  all_claims <- list()
  for (d in 0:lookback_days) {
    date_str <- format(as.Date(analysis_date) - d, "%Y-%m-%d")
    day_claims <- redis_hgetall(claim_hash_key(date_str))
    if (length(day_claims) > 0) {
      for (sc in names(day_claims)) {
        # Newer claims (closer to analysis_date) take precedence
        if (is.null(all_claims[[sc]])) {
          claim <- day_claims[[sc]]
          claim$claim_date <- date_str
          all_claims[[sc]] <- claim
        }
      }
    }
  }
  all_claims
}

#' Get count of active claims (for admin/test-app display)
#' @return List with total count and per-date breakdown
get_claim_stats <- function() {
  today <- Sys.Date()
  dates <- format(today - 0:6, "%Y-%m-%d")
  counts <- list()
  total <- 0L
  for (d in dates) {
    n <- length(redis_hkeys(claim_hash_key(d)))
    if (n > 0) counts[[d]] <- n
    total <- total + n
  }
  list(total = total, by_date = counts)
}
