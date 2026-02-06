# =============================================================================
# DATA FUNCTIONS: Mosquito Monitoring App
# =============================================================================
# Database connections and data retrieval
# =============================================================================

# Source shared helpers when loaded outside the app (overview registry)
if (!exists("get_db_connection", mode = "function")) {
  source("../../shared/db_helpers.R")
}

library(dplyr)
library(lubridate)

# Mosquito database connection (uses centralized configuration only)
get_mosquito_db_connection <- function() {
  con <- get_db_connection()
  if (is.null(con)) {
    warning("Mosquito database connection failed: centralized config not available")
  }
  con
}

# Function to handle integer64 conversion
convert_integer64_columns <- function(data) {
  # Convert any integer64 columns to regular integers/numerics
  for (col in names(data)) {
    if (inherits(data[[col]], "integer64")) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }
  return(data)
}

# Load mosquito data from database
# Compatible with overview metric registry (optional parameters ignored by default)
load_raw_data <- function(analysis_date = NULL,
                          expiring_days = NULL,
                          zone_filter = NULL,
                          status_types = NULL,
                          species_filter = "Total_Ae_+_Cq",
                          include_archive = FALSE,
                          start_year = NULL,
                          end_year = NULL,
                          ...) {
  con <- get_mosquito_db_connection()
  if (is.null(con)) {
    stop("Failed to connect to mosquito database")
  }
  
  mosquito0NAS <- dbReadTable(con, "dbadult_mon_nt_co2_tall2_forr")
  mosquito0NAS <- convert_integer64_columns(mosquito0NAS)
  mosquito0 <- na.omit(mosquito0NAS)
  
  safe_disconnect(con)
  
  mosquito0$Year <- year(mosquito0$inspdate)

  # Default behavior for app usage (no registry params supplied)
  registry_call <- !is.null(analysis_date) || !is.null(expiring_days) ||
    !is.null(zone_filter) || !is.null(status_types) || !is.null(start_year) || 
    !is.null(end_year) || length(list(...)) > 0
  if (!registry_call) {
    return(mosquito0)
  }

  # Registry-compatible output: list(sites, treatments, total_count, pre_aggregated)
  analysis_date <- if (is.null(analysis_date)) Sys.Date() else as.Date(analysis_date)
  expiring_days <- if (is.null(expiring_days)) 7 else as.numeric(expiring_days)
  
  # Get the day-of-year and month-day for consistent comparison across years
  current_month <- month(analysis_date)
  current_day <- day(analysis_date)
  current_year <- year(analysis_date)
  
  # For registry mode, compare SAME CALENDAR PERIOD across years (±3 days)
  if (!is.null(start_year) && !is.null(end_year)) {
    mosquito0 <- mosquito0 %>%
      filter(Year >= start_year, Year <= end_year)
    
    # HISTORICAL CHART MODE: Return data for historical trending
    # This is called by overview historical chart to get the raw treatment data
    
    # Filter to species FIRST if specified  
    if (!is.null(species_filter) && species_filter != "all") {
      mosquito0 <- mosquito0 %>% filter(spp_name == species_filter)
    }
    
    # Apply zone filter if provided, otherwise use zone 1 for consistency
    if (!is.null(zone_filter)) {
      mosquito0 <- mosquito0 %>% filter(zone %in% zone_filter)
    }
    
    treatments <- mosquito0 %>%
      group_by(inspdate, facility, zone) %>%
      summarise(
        value = round(mean(as.numeric(mosqcount)), 1),  # mean per date/facility
        .groups = "drop"
      ) %>%
      select(inspdate, facility, zone, value)
    
    # Sites data (empty for historical)
    sites <- data.frame(
      facility = character(),
      zone = character(), 
      total_count = integer(),
      active_count = integer(),
      expiring_count = integer()
    )
    
    return(list(
      sites = sites,
      treatments = treatments,
      total_count = sum(treatments$value, na.rm = TRUE),
      pre_aggregated = TRUE
    ))
  } else {
    # KEEP full data for historical comparison
    mosquito0_full <- mosquito0
    
    # Get current week data (within ±3 days of analysis date)
    start_date <- analysis_date - 3
    end_date <- analysis_date + 3
    mosquito0 <- mosquito0 %>%
      filter(inspdate >= start_date, inspdate <= end_date)
  }

  if (!is.null(zone_filter)) {
    mosquito0 <- mosquito0 %>% filter(zone %in% zone_filter)
    if (exists("mosquito0_full")) {
      mosquito0_full <- mosquito0_full %>% filter(zone %in% zone_filter)
    }
  }
  
  # Filter to species if specified
  if (!is.null(species_filter) && species_filter != "all") {
    mosquito0 <- mosquito0 %>% filter(spp_name == species_filter)
    if (exists("mosquito0_full")) {
      mosquito0_full <- mosquito0_full %>% filter(spp_name == species_filter)
    }
  }

  # Get REAL mosquito counts - PROPER weekly comparison
  # Current week data (specific analysis date week)
  current_week_start <- floor_date(analysis_date, "week", week_start = 1)
  current_week_end <- current_week_start + days(6)
  
  # Return SUM and TRAP_COUNT for proper weighted averaging in overview
  # This allows: weighted_avg = sum(total_mosquitoes) / sum(trap_count)
  current_data <- mosquito0 %>%
    filter(
      inspdate >= current_week_start,
      inspdate <= current_week_end
    ) %>%
    group_by(facility) %>%
    summarize(
      current_total = sum(as.numeric(mosqcount)),      # TOTAL mosquitoes (not avg)
      current_trap_count = n(),                         # Number of traps
      .groups = "drop"
    )
  
  # Historical average - same week across PRIOR years only (exclude current year)
  # This gives the AVERAGE for this specific week across previous years
  current_week_num <- week(analysis_date)
  current_year <- year(analysis_date)
  
  # Use mosquito0_full for historical data (not the filtered mosquito0)
  historical_source <- if (exists("mosquito0_full")) mosquito0_full else mosquito0
  
  historical_data <- historical_source %>%
    filter(
      week(inspdate) == current_week_num,  # Same week across all years
      year(inspdate) < current_year        # EXCLUDE current year from historical
    ) %>%
    group_by(facility) %>%
    summarize(
      historical_total = sum(as.numeric(mosqcount)),    # TOTAL mosquitoes (not avg)
      historical_trap_count = n(),                       # Total traps over all years
      .groups = "drop"
    )
  
  # Combine current week vs overall average for that week
  sites <- current_data %>%
    left_join(historical_data, by = "facility") %>%
    mutate(
      # Use zone from filter or default to "1"
      zone = if (!is.null(zone_filter) && length(zone_filter) == 1) zone_filter else "1",
      # Handle missing historical data
      historical_total = ifelse(is.na(historical_total), current_total, historical_total),
      historical_trap_count = ifelse(is.na(historical_trap_count) | historical_trap_count < 2, 
                                     current_trap_count, historical_trap_count),
      # Store TOTALS and TRAP COUNTS for proper weighted averaging
      total_count = historical_total,           # TOTAL mosquitoes historically
      active_count = current_total,             # TOTAL mosquitoes this week
      trap_count = current_trap_count,          # Current week trap count
      historical_trap_count = historical_trap_count,  # Historical trap count
      # Above average calculation (using proper per-trap averages)
      current_avg = current_total / current_trap_count,
      historical_avg = historical_total / historical_trap_count,
      expiring_count = pmax(0, round(current_avg - historical_avg, 1))
    ) %>%
    select(facility, zone, total_count, active_count, trap_count, 
           historical_trap_count, expiring_count)

  if (nrow(sites) == 0) {
    sites <- data.frame(
      facility = character(),
      zone = character(),
      total_count = integer(),
      active_count = integer(),
      expiring_count = integer()
    )
  }

  treatments <- mosquito0 %>%
    transmute(
      inspdate = as.Date(inspdate),
      facility = facility,
      zone = zone,
      value = as.numeric(mosqcount)
    )

  return(list(
    sites = sites,
    treatments = treatments,
    total_count = sum(sites$total_count, na.rm = TRUE),
    pre_aggregated = TRUE
  ))
}

message("✓ data_functions.R loaded")
