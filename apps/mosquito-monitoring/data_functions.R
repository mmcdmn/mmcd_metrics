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
    
    # HISTORICAL CHART MODE: Return EXACT SAME data as mosquito monitoring app
    # This is called by overview historical chart to get the raw treatment data
    
    # Filter to species FIRST if specified  
    if (!is.null(species_filter) && species_filter != "all") {
      mosquito0 <- mosquito0 %>% filter(spp_name == species_filter)
    }
    
    treatments <- mosquito0 %>%
      filter(zone == "1") %>%  # App uses zone 1 for main chart
      group_by(inspdate, facility) %>%
      summarise(
        value = round(mean(as.numeric(mosqcount)), 1),  # SAME as app: mean per date
        .groups = "drop"
      ) %>%
      # Add zone column for compatibility
      mutate(zone = "1") %>%
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
    # Get current week data (within ±3 days of analysis date)
    start_date <- analysis_date - 3
    end_date <- analysis_date + 3
    mosquito0 <- mosquito0 %>%
      filter(inspdate >= start_date, inspdate <= end_date)
  }

  if (!is.null(zone_filter)) {
    mosquito0 <- mosquito0 %>% filter(zone %in% zone_filter)
  }
  
  # Filter to species if specified
  if (!is.null(species_filter) && species_filter != "all") {
    mosquito0 <- mosquito0 %>% filter(spp_name == species_filter)
  }

  # Get REAL mosquito counts - weekly averages like the app shows
  # Current week data
  current_week_start <- floor_date(analysis_date, "week", week_start = 1)
  current_week_end <- current_week_start + days(6)
  
  current_data <- mosquito0 %>%
    filter(
      zone == "1",
      inspdate >= current_week_start,
      inspdate <= current_week_end
    ) %>%
    group_by(facility) %>%
    summarize(
      current_avg = round(mean(as.numeric(mosqcount)), 1),
      .groups = "drop"
    )
  
  # Historical data - same week in previous years (10-year average)
  current_year <- year(analysis_date)
  current_week_num <- week(analysis_date)
  
  historical_data <- mosquito0 %>%
    filter(
      zone == "1",
      year(inspdate) >= (current_year - 10),
      year(inspdate) < current_year,
      week(inspdate) == current_week_num
    ) %>%
    group_by(facility) %>%
    summarize(
      historical_avg = round(mean(as.numeric(mosqcount)), 1),
      .groups = "drop"
    )
  
  # Combine current and historical for each facility  
  sites <- current_data %>%
    left_join(historical_data, by = "facility") %>%
    mutate(
      zone = "1",
      historical_avg = ifelse(is.na(historical_avg) | historical_avg == 0, 
                             current_avg, historical_avg),
      total_count = round(historical_avg),    # 10-year average for this week
      active_count = current_avg,            # Current week average
      expiring_count = 0
    ) %>%
    select(facility, zone, total_count, active_count, expiring_count)

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
