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
  
  # Normalize metric names (UI sends "weekly_active_sites", we use "active_sites" internally)
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
  
  # Apply filters to both sites and treatments
  if (!is.null(facility_filter) && length(facility_filter) > 0 && !"all" %in% facility_filter) {
    ground_sites <- ground_sites %>% filter(facility %in% facility_filter)
    ground_treatments <- ground_treatments %>% filter(facility %in% facility_filter)
  }
  
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    ground_sites <- ground_sites %>% filter(zone %in% zone_filter)
    ground_treatments <- ground_treatments %>% filter(zone %in% zone_filter)
  }
  
  if (!is.null(foreman_filter) && length(foreman_filter) > 0 && !"all" %in% foreman_filter) {
    # Convert foreman names to emp_nums
    foremen_lookup <- get_foremen_lookup()
    selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% foreman_filter]
    
    ground_sites <- ground_sites %>% filter(fosarea %in% selected_emp_nums)
    ground_treatments <- ground_treatments %>% filter(fosarea %in% selected_emp_nums)
  }
  
  # Determine if zones should be shown separately
  show_zones_separately <- hist_zone_display == "show-both" && length(zone_filter) > 1
  
  # Special logic for weekly active treatments (active sites and active acres)
  if (hist_time_period == "weekly" && hist_display_metric %in% c("active_sites", "active_acres")) {
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
  
  # Handle zone separation and grouping
  if (show_zones_separately) {
    # Create combined group labels with zones
    if (hist_group_by == "facility" && "facility" %in% names(data_source)) {
      # Map facility names first, then add zones
      facilities <- get_facility_lookup()
      facility_map <- setNames(facilities$full_name, facilities$short_name)
      
      data_source <- data_source %>%
        mutate(
          facility_display = ifelse(
            facility %in% names(facility_map),
            facility_map[facility],
            facility
          ),
          group_label = paste0(facility_display, " (P", zone, ")")
        )
      grouped_data <- data_source %>%
        group_by(group_label, time_period)
    } else if (hist_group_by == "foreman" && "fosarea" %in% names(data_source)) {
      # Map foreman numbers to names first, then add zone
      foremen_lookup <- get_foremen_lookup()
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      
      data_source <- data_source %>%
        mutate(
          foreman_name = ifelse(
            fosarea %in% names(foreman_map),
            foreman_map[as.character(fosarea)],
            paste("FOS", fosarea)
          ),
          group_label = paste0(foreman_name, " (P", zone, ")")
        )
      grouped_data <- data_source %>%
        group_by(group_label, time_period)
    } else if (hist_group_by == "mmcd_all") {
      data_source <- data_source %>%
        mutate(group_label = paste0("All MMCD (P", zone, ")"))
      grouped_data <- data_source %>%
        group_by(group_label, time_period)
    } else {
      return(data.frame())
    }
  } else {
    # Standard grouping without zone separation
    if (hist_group_by == "facility" && "facility" %in% names(data_source)) {
      # Map facility names for display
      facilities <- get_facility_lookup()
      facility_map <- setNames(facilities$full_name, facilities$short_name)
      
      data_source <- data_source %>%
        mutate(
          group_label = ifelse(
            facility %in% names(facility_map),
            facility_map[facility],
            facility
          )
        )
      grouped_data <- data_source %>%
        group_by(group_label, time_period)
    } else if (hist_group_by == "foreman" && "fosarea" %in% names(data_source)) {
      # Map foreman numbers to names for display
      foremen_lookup <- get_foremen_lookup()
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      
      data_source <- data_source %>%
        mutate(
          group_label = ifelse(
            fosarea %in% names(foreman_map),
            foreman_map[as.character(fosarea)],
            paste("FOS", fosarea)
          )
        )
      grouped_data <- data_source %>%
        group_by(group_label, time_period)
    } else if (hist_group_by == "mmcd_all") {
      data_source$group_label <- "All MMCD"
      grouped_data <- data_source %>%
        group_by(group_label, time_period)
    } else {
      return(data.frame())
    }
  }
  
  # Summarize based on metric
  if (hist_display_metric == "treatments") {
    summary_data <- grouped_data %>%
      summarize(value = n(), .groups = "drop")
  } else if (hist_display_metric == "sites" || hist_display_metric == "active_sites") {
    summary_data <- grouped_data %>%
      summarize(value = n_distinct(sitecode), .groups = "drop")
  } else if (hist_display_metric == "treatment_acres") {
    # For treatment acres, sum site acres for ALL treatments (not unique)
    # Each treatment counts the full site acres
    summary_data <- grouped_data %>%
      summarize(value = sum(acres, na.rm = TRUE), .groups = "drop")
  } else if (hist_display_metric == "acres" || hist_display_metric == "site_acres" || hist_display_metric == "active_acres") {
    # For site acres calculations, sum unique site acres only once
    summary_data <- grouped_data %>%
      summarize(value = sum(acres[!duplicated(sitecode)], na.rm = TRUE), .groups = "drop")
  } else {
    summary_data <- data.frame()
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
                                  theme = "MMCD") {
  
  if (nrow(data) == 0) {
    return(plotly_empty() %>% 
           layout(title = "No data available for the selected filters"))
  }
  
  # Create metric label
  metric_label <- switch(display_metric,
    "treatments" = "Number of Treatments",
    "sites" = "Number of Sites Treated",
    "acres" = "Site Acres (Unique Sites)",
    "site_acres" = "Site Acres (Unique Sites)",
    "treatment_acres" = "Treatment Acres (Total)",
    "weekly_active_sites" = "Number of Active Sites",
    "weekly_active_acres" = "Active Site Acres",
    "treatments"
  )
  
  time_label <- if (time_period == "weekly") "Week" else "Year"
  
  # Get colors based on grouping
  colors <- character()
  
  if (group_by == "facility") {
    # Facility colors are keyed by short_name
    facility_colors <- get_facility_base_colors(theme = theme)
    
    # Extract facility short name from group_label (e.g., "East" or "East (P1)")
    for (label in unique(data$group_label)) {
      # Remove zone suffix if present
      base_name <- gsub(" \\(P[12]\\)$", "", label)
      
      # Try to find matching facility short_name
      facilities <- get_facility_lookup()
      matching_facility <- facilities[facilities$full_name == base_name, ]
      
      if (nrow(matching_facility) > 0) {
        short_name <- matching_facility$short_name[1]
        if (short_name %in% names(facility_colors)) {
          colors[label] <- facility_colors[short_name]
        } else {
          colors[label] <- "#999999"  # Fallback
        }
      } else {
        colors[label] <- "#999999"  # Fallback
      }
    }
  } else if (group_by == "foreman") {
    # Foreman colors are keyed by shortname
    foreman_colors <- get_themed_foreman_colors(theme = theme)
    foremen_lookup <- get_foremen_lookup()
    
    # Extract foreman shortname from group_label (e.g., "Smith J" or "Smith J (P1)")
    for (label in unique(data$group_label)) {
      # Remove zone suffix if present
      base_name <- gsub(" \\(P[12]\\)$", "", label)
      
      # The group_label should already be the shortname from our earlier mapping
      if (base_name %in% names(foreman_colors)) {
        colors[label] <- foreman_colors[base_name]
      } else {
        colors[label] <- "#3498db"  # Fallback blue
      }
    }
  } else {
    # For "mmcd_all" or other groupings, use default color
    colors <- setNames(rep("#3498db", length(unique(data$group_label))), unique(data$group_label))
  }
  
  # Create chart
  if (chart_type == "line") {
    p <- plot_ly(data, x = ~time_period, y = ~value, color = ~group_label,
                type = 'scatter', mode = 'lines+markers', colors = colors) %>%
      layout(
        title = list(text = paste("Historical", metric_label), font = list(size = 20)),
        xaxis = list(title = list(text = time_label, font = list(size = 18)), 
                    tickfont = list(size = 16)),
        yaxis = list(title = list(text = metric_label, font = list(size = 18)), 
                    tickfont = list(size = 16)),
        legend = list(font = list(size = 16)),
        font = list(size = 16)
      )
  } else {
    p <- plot_ly(data, x = ~time_period, y = ~value, color = ~group_label,
                type = 'bar', colors = colors) %>%
      layout(
        title = list(text = paste("Historical", metric_label), font = list(size = 20)),
        xaxis = list(title = list(text = time_label, font = list(size = 18)), 
                    tickfont = list(size = 16)),
        yaxis = list(title = list(text = metric_label, font = list(size = 18)), 
                    tickfont = list(size = 16)),
        legend = list(font = list(size = 16)),
        font = list(size = 16),
        barmode = if(chart_type == "grouped_bar") 'group' else 'stack'
      )
  }
  
  return(p)
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
