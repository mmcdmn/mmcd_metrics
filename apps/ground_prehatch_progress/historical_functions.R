# Historical data functions for Ground Prehatch Progress
# Pattern based on drone app - clean and simple

library(dplyr)
library(lubridate)
library(plotly)
library(DT)

source("data_functions.R")
source("display_functions.R")

# Main function to create historical data for charts
# Handles both yearly (sites/acres treated) and weekly (active sites/acres)
create_historical_data <- function(start_year, end_year, hist_time_period, hist_display_metric, 
                                  hist_group_by, hist_zone_display, 
                                  facility_filter = NULL, zone_filter = NULL, foreman_filter = NULL) {
  
  # Add NULL/default checks for all parameters
  if (is.null(hist_time_period)) hist_time_period <- "yearly"
  if (is.null(hist_display_metric)) hist_display_metric <- "treatments"
  if (is.null(hist_group_by)) hist_group_by <- "mmcd_all"
  if (is.null(hist_zone_display)) hist_zone_display <- "combined"
  if (is.null(zone_filter)) zone_filter <- c("1", "2")
  if (is.null(start_year)) start_year <- as.numeric(format(Sys.Date(), "%Y"))
  if (is.null(end_year)) end_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Normalize metric names (UI sends "weekly_active_count", we use "active_count" internally)
  hist_display_metric <- gsub("weekly_", "", hist_display_metric)
  
  # Set date range
  start_date <- as.Date(paste0(start_year, "-01-01"))
  end_date <- as.Date(paste0(end_year, "-12-31"))
  
  # Load raw data with archive
  raw_data <- load_raw_data(
    analysis_date = start_date, 
    include_archive = TRUE, 
    start_year = start_year, 
    end_year = end_year
  )
  
  if (is.null(raw_data) || is.null(raw_data$ground_treatments) || nrow(raw_data$ground_treatments) == 0) {
    return(data.frame())
  }
  
  ground_sites <- raw_data$ground_sites
  ground_treatments <- raw_data$ground_treatments %>%
    filter(inspdate >= start_date & inspdate <= end_date) %>%
    # Treatments already have facility, zone, fosarea from gis_sectcode join in the query
    rename(acres = treated_acres) %>%  # Rename for consistency
    filter(!is.na(facility))  # Remove any treatments without facility info
  
  # Apply filters using shared function from data_functions.R
  filtered_data <- apply_data_filters(
    list(ground_sites = ground_sites, ground_treatments = ground_treatments),
    facility_filter = facility_filter,
    foreman_filter = foreman_filter,
    zone_filter = zone_filter
  )
  ground_sites <- filtered_data$ground_sites
  ground_treatments <- filtered_data$ground_treatments
  
  # Determine if zones should be shown separately
  show_zones_separately <- !is.null(hist_zone_display) && 
                           hist_zone_display == "show-both" && 
                           !is.null(zone_filter) && 
                           length(zone_filter) > 1
  
  # Special logic for weekly active treatments (active sites and active acres)
  if (hist_time_period == "weekly" && hist_display_metric %in% c("active_count", "active_acres")) {
    # Generate all weeks in the range
    all_weeks <- seq.Date(start_date, end_date, by = "week")
    week_data <- data.frame()
    
    for (week_start in all_weeks) {
      week_friday <- as.Date(week_start) + 4  # Friday of that week
      week_label <- paste0(year(week_friday), "-W", sprintf("%02d", week(week_friday)))
      
      # Find sites/acres with active treatment on that Friday
      active_treatments <- ground_treatments %>%
        mutate(
          treatment_end = as.Date(inspdate) + ifelse(is.na(effect_days), 14, effect_days)
        ) %>%
        filter(
          as.Date(inspdate) <= week_friday,
          treatment_end >= week_friday
        )
      
      if (nrow(active_treatments) > 0) {
        # For weekly active, we need sitecode, facility, zone, fosarea, time_period
        # and acres if doing active_acres metric
        if (hist_display_metric == "active_acres") {
          # Active treatments already have acres (renamed from treated_acres)
          active_data <- active_treatments %>%
            mutate(time_period = week_label) %>%
            select(sitecode, facility, zone, fosarea, acres, time_period)
        } else {
          # For sites metric, just use what we have
          active_data <- active_treatments %>%
            mutate(time_period = week_label) %>%
            select(sitecode, facility, zone, fosarea, time_period)
        }
        
        week_data <- bind_rows(week_data, active_data)
      }
    }
    
    data_source <- week_data
  } else {
    # Yearly logic: treatments or sites treated
    if (hist_display_metric == "treatments") {
      # For treatments, use treatment data directly
      if (hist_time_period == "weekly") {
        data_source <- ground_treatments %>%
          mutate(time_period = paste0(year(inspdate), "-W", sprintf("%02d", week(inspdate))))
      } else {
        data_source <- ground_treatments %>%
          mutate(time_period = as.character(year(inspdate)))
      }
    } else if (hist_display_metric == "treatment_acres") {
      # For treatment acres, use acres already joined from ground_sites
      # Each treatment record already has its site's acres from the initial join
      if (hist_time_period == "weekly") {
        data_source <- ground_treatments %>%
          mutate(time_period = paste0(year(inspdate), "-W", sprintf("%02d", week(inspdate))))
      } else {
        data_source <- ground_treatments %>%
          mutate(time_period = as.character(year(inspdate)))
      }
    } else if (hist_display_metric %in% c("sites", "acres", "site_acres")) {
      # For sites/acres, join sites with treatments to get time periods
      if (hist_time_period == "weekly") {
        treatment_periods <- ground_treatments %>%
          mutate(time_period = paste0(year(inspdate), "-W", sprintf("%02d", week(inspdate)))) %>%
          select(sitecode, time_period) %>%
          distinct()
      } else {
        treatment_periods <- ground_treatments %>%
          mutate(time_period = as.character(year(inspdate))) %>%
          select(sitecode, time_period) %>%
          distinct()
      }
      
      # Join sites with their treatment periods
      data_source <- ground_sites %>%
        inner_join(treatment_periods, by = "sitecode", relationship = "many-to-many")
    } else {
      return(data.frame())
    }
  }
  
  # Ensure data_source has required columns for grouping
  if (nrow(data_source) == 0) {
    return(data.frame())
  }
  
  # Apply group labels using shared function (uses fosarea for foreman column)
  data_source <- apply_historical_group_labels(
    data_source, 
    group_by = hist_group_by, 
    show_zones_separately = show_zones_separately,
    foreman_col = "fosarea"
  )
  
  if (!"group_label" %in% names(data_source)) {
    return(data.frame())
  }
  
  # Summarize based on metric (ground uses unique site acres for site_acres metric)
  if (hist_display_metric %in% c("acres", "site_acres", "active_acres")) {
    # For site acres calculations, sum unique site acres only once
    summary_data <- data_source %>%
      group_by(group_label, time_period) %>%
      summarize(value = sum(acres[!duplicated(sitecode)], na.rm = TRUE), .groups = "drop")
  } else {
    summary_data <- summarize_historical_data(data_source, hist_display_metric)
  }
  
  return(summary_data)
}

# Wrapper functions to match existing app.R calls
get_historical_yearly_data <- function(zone_filter = c("1", "2"), 
                                     facility_filter = "all", 
                                     foreman_filter = "all",
                                     start_year = NULL, 
                                     end_year = NULL) {
  # Deprecated - app.R now calls create_historical_data directly
  return(data.frame())
}

get_historical_weekly_data <- function(zone_filter = c("1", "2"), 
                                     facility_filter = "all", 
                                     foreman_filter = "all",
                                     start_year = NULL, 
                                     end_year = NULL) {
  # Deprecated - app.R now calls create_historical_data directly 
  return(data.frame())
}

# Aggregate function - deprecated, kept for compatibility
aggregate_historical_data <- function(data, group_by = "facility", 
                                    time_period = "yearly", 
                                    display_metric = "treatments",
                                    combine_zones = FALSE) {
  # Deprecated - app.R now calls create_historical_data directly
  return(data)
}

# Create historical chart
create_historical_chart <- function(data, chart_type = "stacked_bar", 
                                  display_metric = "treatments",
                                  time_period = "yearly",
                                  group_by = "facility",
                                  theme = "MMCD",
                                  show_zones_separately = FALSE) {
  
  if (nrow(data) == 0) {
    return(plotly_empty() %>% layout(title = "No data available"))
  }
  
  # Build labels
  metric_label <- switch(display_metric,
    "treatments" = "Number of Treatments",
    "sites" = "Number of Sites Treated",
    "acres" = "Site Acres (Unique Sites)",
    "site_acres" = "Site Acres (Unique Sites)",
    "treatment_acres" = "Treatment Acres (Total)",
    "weekly_active_count" = "Number of Active Sites",
    "weekly_active_acres" = "Active Site Acres",
    "treatments"
  )
  time_label <- if (time_period == "weekly") "Week" else "Year"
  chart_title <- paste("Historical", metric_label)
  
  # Get colors
  colors <- if (group_by == "facility") {
    map_facility_display_names_to_colors(unique(data$group_label), theme, handle_zones = show_zones_separately)
  } else if (group_by == "foreman") {
    map_foreman_display_names_to_colors(unique(data$group_label), theme, handle_zones = show_zones_separately)
  } else {
    setNames(rep(get_status_colors(theme = theme)["completed"], length(unique(data$group_label))), unique(data$group_label))
  }
  
  # Use shared chart function
  create_trend_chart(data, chart_type, chart_title, time_label, metric_label, colors)
}

# Create historical details table
create_historical_details_table <- function(data) {
  if (nrow(data) == 0 || ncol(data) == 0) {
    return(DT::datatable(data.frame(Message = "No historical data available")))
  }
  
  DT::datatable(data,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'font-size': '16px'});",
        "$(this.api().table().body()).css({'font-size': '16px'});",
        "}"
      )
    ),
    rownames = FALSE
  )
}
