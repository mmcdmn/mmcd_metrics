# =============================================================================
# DATA FUNCTIONS: Cattail Inspections App
# =============================================================================
# Database connections and data retrieval for cattail inspection progress
# Compatible with overview metric registry
# =============================================================================

# Source shared helpers when loaded outside the app (overview registry)
if (!exists("get_db_connection", mode = "function")) {
  source("../../shared/db_helpers.R")
}

library(dplyr)
library(lubridate)

# =============================================================================
# MAIN DATA LOADING FUNCTION (Registry Compatible)
# =============================================================================

#' Load cattail inspection data
#' Compatible with overview metric registry pattern
#' 
#' @param analysis_date Date for progress calculation (default: today)
#' @param expiring_days Not used for cattail inspections (kept for compatibility)
#' @param zone_filter Vector of zones to include (e.g., c("1", "2"))
#' @param status_types Not used for cattail inspections (kept for compatibility)
#' @param include_archive Whether to include archive data (default: TRUE)
#' @param start_year Start year for historical data
#' @param end_year End year for historical data
#' @param ... Additional parameters (ignored)
#' @return List with sites, treatments, total_count, and goal_count
#' @export
load_raw_data <- function(analysis_date = NULL,
                          expiring_days = NULL,
                          zone_filter = NULL,
                          status_types = NULL,
                          include_archive = TRUE,
                          start_year = NULL,
                          end_year = NULL,
                          ...) {
  con <- get_db_connection()
  if (is.null(con)) {
    stop("Failed to connect to database")
  }
  
  # Default dates
  if (is.null(analysis_date)) analysis_date <- Sys.Date()
  analysis_date <- as.Date(analysis_date)
  
  # Default to current year if not specified
  year <- if (!is.null(end_year)) end_year else year(analysis_date)
  
  # Cattail season is August through December
  # We filter by year and up to the analysis date
  
  # Query for unique sites with action='9' (cattail inspection)
  query <- sprintf("
    WITH valid_sites AS (
      SELECT sitecode 
      FROM public.loc_breeding_sites 
      WHERE (enddate IS NULL OR enddate > '%s')
    ),
    all_inspections AS (
      SELECT a.facility, a.sitecode, a.inspdate,
             g.zone
      FROM (
        SELECT facility, sitecode, inspdate FROM public.dblarv_insptrt_archive
        WHERE action = '9'
          AND EXTRACT(YEAR FROM inspdate) = %d
          AND inspdate <= '%s'
        UNION ALL
        SELECT facility, sitecode, inspdate FROM public.dblarv_insptrt_current
        WHERE action = '9'
          AND EXTRACT(YEAR FROM inspdate) = %d
          AND inspdate <= '%s'
      ) a
      LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 7) = g.sectcode
      WHERE a.sitecode IN (SELECT sitecode FROM valid_sites)
    )
    SELECT facility, zone, COUNT(DISTINCT sitecode) AS inspections
    FROM all_inspections
    GROUP BY facility, zone
    ORDER BY facility, zone
  ", analysis_date, year, analysis_date, year, analysis_date)
  
  inspections <- dbGetQuery(con, query)
  inspections <- inspections %>%
    mutate(
      facility = trimws(facility),
      zone = as.character(zone),
      inspections = as.integer(inspections)
    )
  
  # Get goals from base table
  goals <- dbGetQuery(con, "SELECT facility, p1_totsitecount, p2_totsitecount FROM public.cattail_pctcomplete_base")
  goals <- goals %>% mutate(facility = trimws(facility))
  
  safe_disconnect(con)
  
  # Apply zone filter if provided
  if (!is.null(zone_filter)) {
    inspections <- inspections %>% filter(zone %in% zone_filter)
  }
  
  # Build sites data frame for registry compatibility
  # For each facility, combine P1 and P2 data
  facilities <- unique(c(inspections$facility, goals$facility))
  
  sites_list <- lapply(facilities, function(f) {
    insp_f <- inspections %>% filter(facility == f)
    goal_f <- goals %>% filter(facility == f)
    
    p1_actual <- insp_f %>% filter(zone == "1") %>% pull(inspections)
    p2_actual <- insp_f %>% filter(zone == "2") %>% pull(inspections)
    if (length(p1_actual) == 0) p1_actual <- 0
    if (length(p2_actual) == 0) p2_actual <- 0
    
    p1_goal <- if (nrow(goal_f) > 0) goal_f$p1_totsitecount else 0
    p2_goal <- if (nrow(goal_f) > 0) goal_f$p2_totsitecount else 0
    
    # For zone_filter, return data in overview-compatible format
    if (is.null(zone_filter) || all(c("1", "2") %in% zone_filter)) {
      # Both zones - return P1 and P2 as separate rows for overview compatibility
      list(
        data.frame(
          facility = f,
          zone = "1",
          total_count = p1_goal,
          active_count = p1_actual,
          expiring_count = 0,
          p1_goal = p1_goal,
          p1_actual = p1_actual,
          p2_goal = 0,
          p2_actual = 0,
          stringsAsFactors = FALSE
        ),
        data.frame(
          facility = f,
          zone = "2",
          total_count = p2_goal,
          active_count = p2_actual,
          expiring_count = 0,
          p1_goal = 0,
          p1_actual = 0,
          p2_goal = p2_goal,
          p2_actual = p2_actual,
          stringsAsFactors = FALSE
        )
      )
    } else if ("1" %in% zone_filter) {
      # P1 only
      data.frame(
        facility = f,
        zone = "1",
        total_count = p1_goal,
        active_count = p1_actual,
        expiring_count = 0,
        p1_goal = p1_goal,
        p1_actual = p1_actual,
        p2_goal = 0,
        p2_actual = 0,
        stringsAsFactors = FALSE
      )
    } else {
      # P2 only
      data.frame(
        facility = f,
        zone = "2",
        total_count = p2_goal,
        active_count = p2_actual,
        expiring_count = 0,
        p1_goal = 0,
        p1_actual = 0,
        p2_goal = p2_goal,
        p2_actual = p2_actual,
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Flatten the sites_list in case some entries return multiple data frames
  sites_flat <- list()
  for (item in sites_list) {
    if (is.list(item) && !is.data.frame(item)) {
      # Multiple data frames (from both zones case)
      sites_flat <- c(sites_flat, item)
    } else {
      # Single data frame
      sites_flat <- c(sites_flat, list(item))
    }
  }
  
  sites <- bind_rows(sites_flat)
  
  # Build treatments data frame (for historical charts)
  # Convert to daily counts for historical trending
  treatments <- inspections %>%
    rename(value = inspections) %>%
    mutate(inspdate = analysis_date)  # Use analysis date for current data
  
  # If requesting historical range, load detailed data
  if (!is.null(start_year) && !is.null(end_year)) {
    treatments <- load_historical_treatments(start_year, end_year, analysis_date, zone_filter)
  }
  
  return(list(
    sites = sites,
    treatments = treatments,
    total_count = sum(sites$active_count, na.rm = TRUE),
    goal_count = sum(sites$total_count, na.rm = TRUE),
    pre_aggregated = TRUE  # Data is already aggregated by facility/zone
  ))
}

#' Load historical treatment data for trending
#' @param start_year Start year
#' @param end_year End year
#' @param analysis_date Analysis date (for filtering future dates)
#' @param zone_filter Zone filter
#' @return Data frame with inspdate, facility, zone, value columns
load_historical_treatments <- function(start_year, end_year, analysis_date, zone_filter = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  query <- sprintf("
    WITH valid_sites AS (
      SELECT sitecode 
      FROM public.loc_breeding_sites 
      WHERE (enddate IS NULL OR enddate > '%s')
    ),
    all_inspections AS (
      SELECT a.facility, a.sitecode, a.inspdate,
             g.zone
      FROM (
        SELECT facility, sitecode, inspdate FROM public.dblarv_insptrt_archive
        WHERE action = '9'
          AND EXTRACT(YEAR FROM inspdate) BETWEEN %d AND %d
          AND inspdate <= '%s'
        UNION ALL
        SELECT facility, sitecode, inspdate FROM public.dblarv_insptrt_current
        WHERE action = '9'
          AND EXTRACT(YEAR FROM inspdate) BETWEEN %d AND %d
          AND inspdate <= '%s'
      ) a
      LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 7) = g.sectcode
      WHERE a.sitecode IN (SELECT sitecode FROM valid_sites)
    )
    SELECT facility, zone, inspdate, COUNT(DISTINCT sitecode) AS value
    FROM all_inspections
    GROUP BY facility, zone, inspdate
    ORDER BY inspdate
  ", analysis_date, start_year, end_year, analysis_date, start_year, end_year, analysis_date)
  
  result <- dbGetQuery(con, query)
  safe_disconnect(con)
  
  if (nrow(result) > 0) {
    result <- result %>%
      mutate(
        facility = trimws(facility),
        zone = as.character(zone),
        inspdate = as.Date(inspdate),
        value = as.integer(value)
      )
    
    if (!is.null(zone_filter)) {
      result <- result %>% filter(zone %in% zone_filter)
    }
  }
  
  return(result)
}

#' Get cattail inspection goals
#' @return Data frame with facility, p1_totsitecount, p2_totsitecount
#' @export
get_cattail_goals <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  goals <- dbGetQuery(con, "SELECT facility, p1_totsitecount, p2_totsitecount FROM public.cattail_pctcomplete_base")
  goals <- goals %>% mutate(facility = trimws(facility))
  
  safe_disconnect(con)
  return(goals)
}

#' Get progress summary by facility and zone
#' @param year Year to get progress for
#' @param analysis_date Date to calculate progress up to
#' @param zone_filter Optional zone filter
#' @return Data frame with facility, zone, goal, actual, pct columns
#' @export
get_progress_summary <- function(year, analysis_date = Sys.Date(), zone_filter = NULL) {
  data <- load_raw_data(
    analysis_date = analysis_date,
    zone_filter = zone_filter,
    end_year = year
  )
  
  sites <- data$sites
  
  # Calculate percentages
  summary <- sites %>%
    mutate(
      p1_pct = ifelse(p1_goal > 0, round(100 * p1_actual / p1_goal), 0),
      p2_pct = ifelse(p2_goal > 0, round(100 * p2_actual / p2_goal), 0),
      total_pct = ifelse(total_count > 0, round(100 * active_count / total_count), 0)
    ) %>%
    select(facility, zone, 
           p1_goal, p1_actual, p1_pct,
           p2_goal, p2_actual, p2_pct,
           total_goal = total_count, total_actual = active_count, total_pct)
  
  return(summary)
}

message("✓ cattail_inspections/data_functions.R loaded")
