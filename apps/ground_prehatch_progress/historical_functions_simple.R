# Simplified historical functions for Ground Prehatch Progress app
# These functions provide the interface needed by the app server logic

library(dplyr)
library(lubridate)
source("data_functions.R")

# Main function to get ground prehatch historical data
get_ground_historical_data <- function(time_period = "weekly", display_metric = "treatments", 
                                     zone_filter = c("1", "2"), 
                                     start_year = NULL, end_year = NULL) {
  
  # Determine year range - always use start_year and end_year
  if (is.null(start_year)) start_year <- as.numeric(format(Sys.Date(), "%Y")) - 4
  if (is.null(end_year)) end_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  use_start_year <- start_year
  use_end_year <- end_year
  
  # Use unified load_raw_data function
  tryCatch({
    # Load raw data with archive
    raw_data <- load_raw_data(
      analysis_date = as.Date(paste0(use_start_year, "-01-01")),
      include_archive = TRUE,
      start_year = use_start_year,
      end_year = use_end_year
    )
    
    if (is.null(raw_data$ground_treatments) || nrow(raw_data$ground_treatments) == 0) {
      return(data.frame())
    }
    
    # Merge treatments with site data
    data <- raw_data$ground_treatments %>%
      left_join(raw_data$ground_sites, by = "sitecode") %>%
      filter(!is.na(facility))
    
    # Filter by zones
    if (!is.null(zone_filter) && length(zone_filter) > 0) {
      data <- data %>% filter(zone %in% zone_filter)
    }
    
    # Add time period column
    if (time_period == "weekly") {
      data <- data %>%
        mutate(time_period = paste0(year(inspdate), "-W", sprintf("%02d", week(inspdate))))
    } else {
      data <- data %>%
        mutate(time_period = year(inspdate))
    }
    
    return(data)
    
  }, error = function(e) {
    warning(paste("Error in get_ground_historical_data:", e$message))
    return(data.frame())
  })
}

# Function to filter historical data
filter_historical_data <- function(data, zone_filter, facility_filter, foreman_filter) {
  if (nrow(data) == 0) return(data)
  
  # Filter by facility
  if (!"all" %in% facility_filter && !is.null(facility_filter)) {
    data <- data %>% filter(facility %in% facility_filter)
  }
  
  # Filter by foreman
  if (!"all" %in% foreman_filter && !is.null(foreman_filter)) {
    # Convert foreman names to emp_nums
    foremen_lookup <- get_foremen_lookup()
    if (nrow(foremen_lookup) > 0) {
      selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% foreman_filter]
      data <- data %>% filter(foreman %in% selected_emp_nums)
    }
  }
  
  return(data)
}

# Function to aggregate historical data by group
aggregate_historical_data_by_group <- function(data, group_by, time_period, display_metric, combine_zones = FALSE) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Create metric calculation
  if (display_metric == "treatments") {
    metric_func <- function(x) length(x)
  } else if (display_metric == "sites") {
    metric_func <- function(x) length(unique(x))
  } else { # acres
    metric_func <- function(x) sum(as.numeric(x), na.rm = TRUE)
  }
  
  # Group the data based on group_by parameter
  if (group_by == "mmcd_all") {
    # Check if we need to separate zones
    if (combine_zones) {
      # Combine all zones
      aggregated <- data %>%
        group_by(time_period) %>%
        summarise(
          count = if(display_metric == "treatments") n() 
                  else if(display_metric == "sites") n_distinct(sitecode)
                  else sum(acres, na.rm = TRUE), 
          .groups = "drop"
        ) %>%
        mutate(display_name = "MMCD Total")
    } else {
      # Separate by zone
      aggregated <- data %>%
        group_by(time_period, zone) %>%
        summarise(
          count = if(display_metric == "treatments") n() 
                  else if(display_metric == "sites") n_distinct(sitecode)
                  else sum(acres, na.rm = TRUE), 
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0("Zone P", zone))
    }
  } else if (group_by == "facility") {
    if (combine_zones) {
      aggregated <- data %>%
        group_by(time_period, facility) %>%
        summarise(
          count = if(display_metric == "treatments") n() 
                  else if(display_metric == "sites") n_distinct(sitecode)
                  else sum(acres, na.rm = TRUE), 
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0(facility, " (Facility)"))
    } else {
      aggregated <- data %>%
        group_by(time_period, facility, zone) %>%
        summarise(
          count = if(display_metric == "treatments") n() 
                  else if(display_metric == "sites") n_distinct(sitecode)
                  else sum(acres, na.rm = TRUE), 
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0(facility, " P", zone))
    }
  } else if (group_by == "foreman") {
    # Map foreman numbers to names
    foremen_lookup <- get_foremen_lookup()
    if (nrow(foremen_lookup) > 0) {
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      
      if (combine_zones) {
        aggregated <- data %>%
          mutate(foreman_name = ifelse(
            foreman %in% names(foreman_map),
            foreman_map[as.character(foreman)],
            paste("FOS", foreman)
          )) %>%
          group_by(time_period, foreman_name) %>%
          summarise(
            count = if(display_metric == "treatments") n() 
                    else if(display_metric == "sites") n_distinct(sitecode)
                    else sum(acres, na.rm = TRUE), 
            .groups = "drop"
          ) %>%
          mutate(display_name = paste0(foreman_name, " (FOS)"))
      } else {
        aggregated <- data %>%
          mutate(foreman_name = ifelse(
            foreman %in% names(foreman_map),
            foreman_map[as.character(foreman)],
            paste("FOS", foreman)
          )) %>%
          group_by(time_period, foreman_name, zone) %>%
          summarise(
            count = if(display_metric == "treatments") n() 
                    else if(display_metric == "sites") n_distinct(sitecode)
                    else sum(acres, na.rm = TRUE), 
            .groups = "drop"
          ) %>%
          mutate(display_name = paste0(foreman_name, " P", zone))
      }
    } else {
      # Fallback if no lookup available
      if (combine_zones) {
        aggregated <- data %>%
          group_by(time_period, foreman) %>%
          summarise(
            count = if(display_metric == "treatments") n() 
                    else if(display_metric == "sites") n_distinct(sitecode)
                    else sum(acres, na.rm = TRUE), 
            .groups = "drop"
          ) %>%
          mutate(display_name = paste0("FOS ", foreman))
      } else {
        aggregated <- data %>%
          group_by(time_period, foreman, zone) %>%
          summarise(
            count = if(display_metric == "treatments") n() 
                    else if(display_metric == "sites") n_distinct(sitecode)
                    else sum(acres, na.rm = TRUE), 
            .groups = "drop"
          ) %>%
          mutate(display_name = paste0("FOS ", foreman, " P", zone))
      }
    }
  } else if (group_by == "sectcode") {
    if (combine_zones) {
      aggregated <- data %>%
        group_by(time_period, sectcode) %>%
        summarise(
          count = if(display_metric == "treatments") n() 
                  else if(display_metric == "sites") n_distinct(sitecode)
                  else sum(acres, na.rm = TRUE), 
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0(sectcode, " (Section)"))
    } else {
      aggregated <- data %>%
        group_by(time_period, sectcode, zone) %>%
        summarise(
          count = if(display_metric == "treatments") n() 
                  else if(display_metric == "sites") n_distinct(sitecode)
                  else sum(acres, na.rm = TRUE), 
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0(sectcode, " P", zone))
    }
  } else {
    return(data.frame())
  }
  
  return(aggregated)
}

# Function to create historical details table
create_historical_details_table <- function(data) {
  if (nrow(data) == 0) {
    return(DT::datatable(data.frame(Message = "No historical data available")))
  }
  
  # Get foremen lookup for mapping
  foremen_lookup <- get_foremen_lookup()
  
  # Prepare data for table display
  table_data <- data %>%
    mutate(
      foreman_name = ifelse(
        treatment_foreman %in% foremen_lookup$emp_num,
        foremen_lookup$shortname[match(treatment_foreman, foremen_lookup$emp_num)],
        paste("FOS", treatment_foreman)
      )
    ) %>%
    select(inspdate, sitecode, facility, foreman_name, sectcode, zone, matcode) %>%
    arrange(desc(inspdate), facility, sitecode)
  
  # Rename columns for display
  colnames(table_data) <- c("Treatment Date", "Site Code", "Facility", "Foreman", "Section", "Zone", "Material")
  
  # Create the data table
  DT::datatable(table_data,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ),
    rownames = FALSE
  ) %>%
    DT::formatStyle(columns = 1:ncol(table_data), fontSize = '12px') %>%
    DT::formatDate("Treatment Date", method = "toDateString")
}