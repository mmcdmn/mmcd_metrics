# Historical functions for Ground Prehatch Progress app
# All functions needed for historical analysis

library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(purrr)

source("data_functions.R")
source("display_functions.R")

# Main function to get ground prehatch historical data
get_ground_historical_data <- function(time_period = "weekly", display_metric = "treatments", 
                                     zone_filter = c("1", "2"), 
                                     start_year = NULL, end_year = NULL) {
  
  # Determine year range
  if (is.null(start_year)) start_year <- as.numeric(format(Sys.Date(), "%Y")) - 4
  if (is.null(end_year)) end_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Set date range
  start_date <- as.Date(paste0(start_year, "-01-01"))
  end_date <- as.Date(paste0(end_year, "-12-31"))
  
  # Load raw data using standard function
  raw_data <- load_raw_data(
    analysis_date = start_date,
    include_archive = TRUE,
    start_year = start_year,
    end_year = end_year
  )
  
  if (is.null(raw_data) || nrow(raw_data$ground_treatments) == 0) {
    return(data.frame())
  }
  
  ground_sites <- raw_data$ground_sites
  ground_treatments <- raw_data$ground_treatments %>%
    filter(inspdate >= start_date & inspdate <= end_date)
  
  # Add zone/facility/fosarea info to treatments by joining with sites
  ground_treatments <- ground_treatments %>%
    left_join(ground_sites %>% select(sitecode, facility, zone, fosarea), by = "sitecode", relationship = "many-to-many") %>%
    filter(!is.na(facility))  # Remove treatments without matching site data
    # Note: effect_days comes from database via mattype_list_targetdose join - do NOT override
  
  # Apply zone filter
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    # Filter sites by zone
    ground_sites <- ground_sites %>% filter(zone %in% zone_filter)
    
    # Filter treatments by joining with filtered sites to get zone information
    filtered_sitecodes <- ground_sites$sitecode
    ground_treatments <- ground_treatments %>% filter(sitecode %in% filtered_sitecodes)
  }
  
  # Special logic for weekly active treatments (active sites and active acres)
  if (time_period == "weekly" && display_metric %in% c("weekly_active_sites", "weekly_active_acres")) {
    # Generate all weeks in the range
    all_weeks <- seq.Date(start_date, end_date, by = "week")
    week_data <- data.frame()
    
    for (week_start in all_weeks) {
      week_friday <- as.Date(week_start) + 4  # Friday of that week
      week_label <- paste0("wk:", sprintf("%02d", week(week_friday)), "-", substr(year(week_friday), 3, 4))
      
      # Find sites/acres with active treatment on that Friday
      active_treatments <- ground_treatments %>%
        mutate(
          treatment_end = as.Date(inspdate) + effect_days  # Use database effect_days directly
        ) %>%
        filter(
          as.Date(inspdate) <= week_friday,
          treatment_end >= week_friday
        )
      
      if (nrow(active_treatments) > 0) {
        # For weekly active analysis, the treatment data already has facility, zone, fosarea from load_raw_data
        if (display_metric == "weekly_active_acres") {
          # Need to get acres from sites data
          active_data <- active_treatments %>%
            inner_join(ground_sites %>% select(sitecode, acres), 
                      by = "sitecode", relationship = "many-to-many") %>%
            mutate(time_period = week_label) %>%
            select(sitecode, facility, zone, fosarea, acres, time_period)
        } else {
          # For sites metric, still need acres column for aggregation logic
          active_data <- active_treatments %>%
            inner_join(ground_sites %>% select(sitecode, acres), 
                      by = "sitecode", relationship = "many-to-many") %>%
            mutate(time_period = week_label) %>%
            select(sitecode, facility, zone, fosarea, acres, time_period)
        }
        
        week_data <- bind_rows(week_data, active_data)
      }
    }
    
    return(week_data)
  } else {
    # Original logic for yearly or treatments
    if (display_metric == "treatments") {
      # For treatments, use treatment data directly
      if (time_period == "weekly") {
        data_source <- ground_treatments %>%
          mutate(time_period = paste0("wk:", sprintf("%02d", week(inspdate)), "-", substr(year(inspdate), 3, 4)))
      } else {
        data_source <- ground_treatments %>%
          mutate(time_period = as.character(year(inspdate)))
      }
    } else {
      # For sites/acres, join sites with treatments to get time periods
      if (time_period == "weekly") {
        treatment_periods <- ground_treatments %>%
          mutate(time_period = paste0("wk:", sprintf("%02d", week(inspdate)), "-", substr(year(inspdate), 3, 4))) %>%
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
    }
    
    return(data_source)
  }
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
      data <- data %>% filter(fosarea %in% selected_emp_nums)
    }
  }
  
  return(data)
}

# Historical yearly data function
get_historical_yearly_data <- function(zone_filter = c("1", "2"), 
                                     facility_filter = "all", 
                                     foreman_filter = "all",
                                     start_year = NULL, 
                                     end_year = NULL) {
  
  # Get the raw historical data
  data <- get_ground_historical_data(
    time_period = "yearly",
    display_metric = "treatments",  # Default metric
    zone_filter = zone_filter,
    start_year = start_year,
    end_year = end_year
  )
  
  # Apply filters
  filtered_data <- filter_historical_data(data, zone_filter, facility_filter, foreman_filter)
  
  return(filtered_data)
}

# Historical weekly data function  
get_historical_weekly_data <- function(zone_filter = c("1", "2"), 
                                     facility_filter = "all", 
                                     foreman_filter = "all",
                                     start_year = NULL, 
                                     end_year = NULL) {
  
  # Get the raw historical data
  data <- get_ground_historical_data(
    time_period = "weekly",
    display_metric = "weekly_active_sites",  # Default metric
    zone_filter = zone_filter,
    start_year = start_year,
    end_year = end_year
  )
  
  # Apply filters
  filtered_data <- filter_historical_data(data, zone_filter, facility_filter, foreman_filter)
  
  return(filtered_data)
}

# Function to aggregate historical data by group
aggregate_historical_data_by_group <- function(data, group_by, time_period, display_metric, combine_zones = FALSE) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Create metric calculation based on time period and metric type
  if (time_period == "weekly") {
    # For weekly analysis, data already contains only sites with active treatment
    if (display_metric == "weekly_active_sites") {
      # Count unique active sites per week
      metric_calculation <- function(data) n_distinct(data$sitecode)
    } else if (display_metric == "weekly_active_acres") {
      # Sum site acres for unique active sites per week
      metric_calculation <- function(data) sum(data$acres[!duplicated(data$sitecode)], na.rm = TRUE)
    } else {
      metric_calculation <- function(data) 0
    }
  } else {
    # For yearly analysis, use existing logic
    if (display_metric == "treatments") {
      metric_calculation <- function(data) nrow(data)
    } else if (display_metric == "sites") {
      metric_calculation <- function(data) n_distinct(data$sitecode)
    } else if (display_metric == "acres") {
      metric_calculation <- function(data) sum(data$treated_acres, na.rm = TRUE)
    } else if (display_metric == "site_acres") {
      metric_calculation <- function(data) sum(data$acres[!duplicated(data$sitecode)], na.rm = TRUE)
    } else {
      metric_calculation <- function(data) 0
    }
  }
  
  # Group the data based on group_by parameter
  if (group_by == "mmcd_all") {
    # Check if we need to separate zones
    if (combine_zones) {
      # Combine all zones
      aggregated <- data %>%
        group_by(time_period) %>%
        summarise(
          count = case_when(
            display_metric == "treatments" ~ n(),
            display_metric == "sites" ~ n_distinct(sitecode),
            display_metric == "acres" ~ ifelse("treated_acres" %in% names(pick(everything())), sum(treated_acres, na.rm = TRUE), ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0)),
            display_metric == "site_acres" ~ ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
            display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
            display_metric == "weekly_active_acres" ~ ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
            TRUE ~ 0
          ),
          .groups = "drop"
        ) %>%
        mutate(display_name = "MMCD Total")
    } else {
      # Separate by zone
      aggregated <- data %>%
        group_by(time_period, zone) %>%
        summarise(
          count = case_when(
            display_metric == "treatments" ~ n(),
            display_metric == "sites" ~ n_distinct(sitecode),
            display_metric == "acres" ~ ifelse("treated_acres" %in% names(pick(everything())), sum(treated_acres, na.rm = TRUE), ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0)),
            display_metric == "site_acres" ~ ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
            display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
            display_metric == "weekly_active_acres" ~ ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
            TRUE ~ 0
          ),
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0("Zone P", zone))
    }
  } else if (group_by == "facility") {
    if (combine_zones) {
      aggregated <- data %>%
        group_by(time_period, facility) %>%
        summarise(
          count = case_when(
            display_metric == "treatments" ~ n(),
            display_metric == "sites" ~ n_distinct(sitecode),
            display_metric == "acres" ~ sum(acres[!duplicated(sitecode)], na.rm = TRUE),
            display_metric == "site_acres" ~ sum(acres[!duplicated(sitecode)], na.rm = TRUE),
            display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
            display_metric == "weekly_active_acres" ~ sum(acres[!duplicated(sitecode)], na.rm = TRUE),
            TRUE ~ 0
          ),
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0(facility, " (Facility)"))
    } else {
      aggregated <- data %>%
        group_by(time_period, facility, zone) %>%
        summarise(
          count = case_when(
            display_metric == "treatments" ~ n(),
            display_metric == "sites" ~ n_distinct(sitecode),
            display_metric == "acres" ~ sum(acres[!duplicated(sitecode)], na.rm = TRUE),
            display_metric == "site_acres" ~ sum(acres[!duplicated(sitecode)], na.rm = TRUE),
            display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
            display_metric == "weekly_active_acres" ~ sum(acres[!duplicated(sitecode)], na.rm = TRUE),
            TRUE ~ 0
          ),
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0(facility, " P", zone, " (Facility)"))
    }
  } else if (group_by == "foreman") {
    # Map foreman numbers to names
    foremen_lookup <- get_foremen_lookup()
    if (nrow(foremen_lookup) > 0) {
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      
      if (combine_zones) {
        aggregated <- data %>%
          mutate(foreman_name = ifelse(
            fosarea %in% names(foreman_map),
            foreman_map[as.character(fosarea)],
            paste("FOS", fosarea)
          )) %>%
          group_by(time_period, foreman_name) %>%
          summarise(
            count = case_when(
              display_metric == "treatments" ~ n(),
              display_metric == "sites" ~ n_distinct(sitecode),
              display_metric == "acres" ~ ifelse("treated_acres" %in% names(pick(everything())), sum(treated_acres, na.rm = TRUE), ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0)),
              display_metric == "site_acres" ~ ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
              display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
              display_metric == "weekly_active_acres" ~ ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
              TRUE ~ 0
            ),
            .groups = "drop"
          ) %>%
          mutate(display_name = paste0(foreman_name, " (FOS)"))
      } else {
        aggregated <- data %>%
          mutate(foreman_name = ifelse(
            fosarea %in% names(foreman_map),
            foreman_map[as.character(fosarea)],
            paste("FOS", fosarea)
          )) %>%
          group_by(time_period, foreman_name, zone) %>%
          summarise(
            count = case_when(
              display_metric == "treatments" ~ n(),
              display_metric == "sites" ~ n_distinct(sitecode),
              display_metric == "acres" ~ ifelse("treated_acres" %in% names(pick(everything())), sum(treated_acres, na.rm = TRUE), ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0)),
              display_metric == "site_acres" ~ ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
              display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
              display_metric == "weekly_active_acres" ~ ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
              TRUE ~ 0
            ),
            .groups = "drop"
          ) %>%
          mutate(display_name = paste0(foreman_name, " P", zone))
      }
    } else {
      # Fallback if no lookup available
      if (combine_zones) {
        aggregated <- data %>%
          group_by(time_period, fosarea) %>%
          summarise(
            count = case_when(
              display_metric == "treatments" ~ n(),
              display_metric == "sites" ~ n_distinct(sitecode),
              display_metric == "acres" ~ ifelse("treated_acres" %in% names(pick(everything())), sum(treated_acres, na.rm = TRUE), ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0)),
              display_metric == "site_acres" ~ ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
              display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
              display_metric == "weekly_active_acres" ~ ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
              TRUE ~ 0
            ),
            .groups = "drop"
          ) %>%
          mutate(display_name = paste0("FOS ", fosarea))
      } else {
        aggregated <- data %>%
          group_by(time_period, fosarea, zone) %>%
          summarise(
            count = case_when(
              display_metric == "treatments" ~ n(),
              display_metric == "sites" ~ n_distinct(sitecode),
              display_metric == "acres" ~ ifelse("treated_acres" %in% names(pick(everything())), sum(treated_acres, na.rm = TRUE), ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0)),
              display_metric == "site_acres" ~ ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
              display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
              display_metric == "weekly_active_acres" ~ ifelse("acres" %in% names(pick(everything())), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
              TRUE ~ 0
            ),
            .groups = "drop"
          ) %>%
          mutate(display_name = paste0("FOS ", fosarea, " P", zone))
      }
    }
  } else if (group_by == "sectcode") {
    if (combine_zones) {
      aggregated <- data %>%
        group_by(time_period, sectcode) %>%
        summarise(
          count = case_when(
            display_metric == "treatments" ~ n(),
            display_metric == "sites" ~ n_distinct(sitecode),
            display_metric == "acres" ~ ifelse("treated_acres" %in% colnames(data), sum(treated_acres, na.rm = TRUE), ifelse("acres" %in% colnames(data), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0)),
            display_metric == "site_acres" ~ ifelse("acres" %in% colnames(data), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
            display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
            display_metric == "weekly_active_acres" ~ ifelse("acres" %in% colnames(data), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
            TRUE ~ 0
          ),
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0(sectcode, " (Section)"))
    } else {
      aggregated <- data %>%
        group_by(time_period, sectcode, zone) %>%
        summarise(
          count = case_when(
            display_metric == "treatments" ~ n(),
            display_metric == "sites" ~ n_distinct(sitecode),
            display_metric == "acres" ~ ifelse("treated_acres" %in% colnames(data), sum(treated_acres, na.rm = TRUE), ifelse("acres" %in% colnames(data), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0)),
            display_metric == "site_acres" ~ ifelse("acres" %in% colnames(data), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
            display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
            display_metric == "weekly_active_acres" ~ ifelse("acres" %in% colnames(data), sum(acres[!duplicated(sitecode)], na.rm = TRUE), 0),
            TRUE ~ 0
          ),
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0(sectcode, " P", zone, " (Section)"))
    }
  } else {
    cat("DEBUG - Aggregation: No data to aggregate\n")
    return(data.frame())
  }
  
  return(aggregated)
}

# Aggregate historical data (wrapper)
aggregate_historical_data <- function(data, group_by = "facility", 
                                    time_period = "yearly", 
                                    display_metric = "treatments",
                                    combine_zones = FALSE) {
  
  # Use the aggregation function
  return(aggregate_historical_data_by_group(
    data, group_by, time_period, display_metric, combine_zones
  ))
}

# Create historical chart
create_historical_chart <- function(data, chart_type = "stacked_bar", 
                                  display_metric = "treatments",
                                  time_period = "yearly",
                                  group_by = "facility") {
  
  if (nrow(data) == 0) {
    return(plotly_empty() %>% 
           layout(title = "No data available for the selected filters"))
  }
  
  # Sort data properly for time series (especially important for weekly line charts)
  if (time_period == "weekly") {
    # Convert week strings to dates for proper chronological sorting AND plotting
    data <- data %>%
      mutate(
        week_sort_date = sapply(time_period, function(week_str) {
          # Parse "wk:##-YY" format
          parts <- strsplit(week_str, "[:-]")[[1]]
          week_num <- as.numeric(parts[2])
          year <- 2000 + as.numeric(parts[3])
          
          # Convert to date (first day of that week)
          jan1 <- as.Date(paste0(year, "-01-01"))
          week_start <- jan1 + (week_num - 1) * 7 - as.numeric(format(jan1, "%w"))
          return(as.numeric(week_start))
        }),
        # Create actual date for x-axis (plotly works better with real dates)
        x_axis_date = as.Date(week_sort_date, origin = "1970-01-01"),
        # Keep original time_period for hover text
        original_time_period = time_period
      ) %>%
      arrange(week_sort_date, display_name) %>%
      select(-week_sort_date)
  } else {
    # For yearly data, sort numerically
    data <- data %>%
      mutate(time_period_num = as.numeric(time_period)) %>%
      arrange(time_period_num, display_name) %>%
      select(-time_period_num) %>%
      mutate(
        x_axis_date = as.Date(paste0(time_period, "-01-01")),
        original_time_period = time_period
      )
  }
  
  # Create the metric label for display
  metric_label <- switch(display_metric,
    "treatments" = "Number of Treatments",
    "sites" = "Number of Sites Treated",
    "acres" = "Acres Treated",
    "site_acres" = "Site Acres (Unique Sites)",
    "weekly_active_sites" = "Number of Active Sites",
    "weekly_active_acres" = "Active Site Acres",
    "treatments"
  )
  
  # Create time label for x-axis
  time_label <- if (time_period == "weekly") "Week" else "Year"
  
  # Get colors for groups
  if (group_by == "facility") {
    facility_colors <- get_facility_base_colors()
    # Map facility names from display_name to get proper colors (same as display_functions.R)
    colors <- character(0)
    for (display_name in unique(data$display_name)) {
      # Extract facility code from display names - handle multiple formats
      facility_code <- display_name
      # Try removing facility suffix patterns
      facility_code <- gsub(" \\(Facility\\)", "", facility_code)
      facility_code <- gsub(" P[12]", "", facility_code)  # Remove zone suffixes
      
      # Try direct mapping first
      if (facility_code %in% names(facility_colors)) {
        colors[display_name] <- facility_colors[facility_code]
      } else {
        # Try finding by facility lookup for full names
        facilities <- get_facility_lookup()
        matching_facility <- facilities[facilities$full_name == facility_code, ]
        if (nrow(matching_facility) > 0) {
          short_name <- matching_facility$short_name[1]
          if (short_name %in% names(facility_colors)) {
            colors[display_name] <- facility_colors[short_name]
          }
        }
      }
    }
    # Ensure all display names have colors
    for (display_name in unique(data$display_name)) {
      if (!display_name %in% names(colors)) {
        colors[display_name] <- "#999999"  # Fallback color
      }
    }
  } else if (group_by == "foreman") {
    # Map foreman employee numbers to facility-based colors (same as display_functions.R)
    foreman_colors <- get_foreman_colors()
    foremen_lookup <- get_foremen_lookup()
    colors <- character(0)
    
    # Extract foreman info from display names
    for (display_name in unique(data$display_name)) {
      # Extract foreman name from display_name patterns like "Smith P1" or "Smith (FOS)"
      foreman_name <- gsub(" P[12]$| \\(FOS\\)$", "", display_name)
      
      # Find matching foreman in lookup
      matches <- which(trimws(foremen_lookup$shortname) == trimws(foreman_name))
      if (length(matches) > 0) {
        shortname <- foremen_lookup$shortname[matches[1]]
        if (shortname %in% names(foreman_colors)) {
          colors[display_name] <- foreman_colors[shortname]
        }
      }
    }
    # Ensure all display names have colors
    for (display_name in unique(data$display_name)) {
      if (!display_name %in% names(colors)) {
        colors[display_name] <- "#3498db"  # Fallback blue
      }
    }
  } else {
    # Default colors for other groupings
    colors <- setNames(rep("#3498db", length(unique(data$display_name))), unique(data$display_name))
  }
  
  # Create the chart based on chart type
  if (chart_type == "line") {
    # Use actual dates for x-axis, original time period for hover
    p <- plot_ly(data, x = ~x_axis_date, y = ~count, color = ~display_name,
                type = 'scatter', mode = 'lines+markers',
                colors = colors,
                text = ~paste("Week:", original_time_period, "<br>", metric_label, ":", count),
                hovertemplate = "%{text}<extra></extra>") %>%
      layout(
        title = list(text = paste("Historical", metric_label, "by", time_label), 
                    font = list(size = 20, family = "Arial, sans-serif", weight = "bold")),
        xaxis = list(title = time_label, 
                    font = list(size = 18, family = "Arial, sans-serif", weight = "bold"),
                    tickfont = list(size = 18, family = "Arial, sans-serif", weight = "bold"),
                    type = if (time_period == "weekly") "date" else "category"),
        yaxis = list(title = metric_label,
                    font = list(size = 18, family = "Arial, sans-serif", weight = "bold"),
                    tickfont = list(size = 18, family = "Arial, sans-serif", weight = "bold")),
        legend = list(title = list(text = group_by, font = list(size = 18, family = "Arial, sans-serif", weight = "bold")),
                     font = list(size = 18, family = "Arial, sans-serif", weight = "bold")),
        font = list(size = 18, family = "Arial, sans-serif", weight = "bold")
      )
  } else if (chart_type == "area") {
    p <- plot_ly(data, x = ~x_axis_date, y = ~count, color = ~display_name,
                type = 'scatter', mode = 'lines', fill = 'tonexty',
                colors = colors,
                text = ~paste("Week:", original_time_period, "<br>", metric_label, ":", count),
                hovertemplate = "%{text}<extra></extra>") %>%
      layout(
        title = list(text = paste("Historical", metric_label, "by", time_label),
                    font = list(size = 20, family = "Arial, sans-serif", weight = "bold")),
        xaxis = list(title = time_label,
                    font = list(size = 18, family = "Arial, sans-serif", weight = "bold"),
                    tickfont = list(size = 18, family = "Arial, sans-serif", weight = "bold"),
                    type = if (time_period == "weekly") "date" else "category"),
        yaxis = list(title = metric_label,
                    font = list(size = 18, family = "Arial, sans-serif", weight = "bold"),
                    tickfont = list(size = 18, family = "Arial, sans-serif", weight = "bold")),
        legend = list(title = list(text = group_by, font = list(size = 18, family = "Arial, sans-serif", weight = "bold")),
                     font = list(size = 18, family = "Arial, sans-serif", weight = "bold")),
        font = list(size = 18, family = "Arial, sans-serif", weight = "bold")
      )
  } else if (chart_type == "grouped_bar") {
    p <- plot_ly(data, x = ~x_axis_date, y = ~count, color = ~display_name,
                type = 'bar', colors = colors,
                text = ~paste("Week:", original_time_period, "<br>", metric_label, ":", count),
                hovertemplate = "%{text}<extra></extra>") %>%
      layout(
        title = list(text = paste("Historical", metric_label, "by", time_label),
                    font = list(size = 20, family = "Arial, sans-serif", weight = "bold")),
        xaxis = list(title = time_label,
                    font = list(size = 18, family = "Arial, sans-serif", weight = "bold"),
                    tickfont = list(size = 18, family = "Arial, sans-serif", weight = "bold"),
                    type = if (time_period == "weekly") "date" else "category"),
        yaxis = list(title = metric_label,
                    font = list(size = 18, family = "Arial, sans-serif", weight = "bold"),
                    tickfont = list(size = 18, family = "Arial, sans-serif", weight = "bold")),
        barmode = 'group',
        legend = list(title = list(text = group_by, font = list(size = 18, family = "Arial, sans-serif", weight = "bold")),
                     font = list(size = 18, family = "Arial, sans-serif", weight = "bold")),
        font = list(size = 18, family = "Arial, sans-serif", weight = "bold")
      )
  } else {  # Default to stacked_bar
    p <- plot_ly(data, x = ~x_axis_date, y = ~count, color = ~display_name,
                type = 'bar', colors = colors,
                text = ~paste("Week:", original_time_period, "<br>", metric_label, ":", count),
                hovertemplate = "%{text}<extra></extra>") %>%
      layout(
        title = list(text = paste("Historical", metric_label, "by", time_label),
                    font = list(size = 20, family = "Arial, sans-serif", weight = "bold")),
        xaxis = list(title = time_label,
                    font = list(size = 18, family = "Arial, sans-serif", weight = "bold"),
                    tickfont = list(size = 18, family = "Arial, sans-serif", weight = "bold"),
                    type = if (time_period == "weekly") "date" else "category"),
        yaxis = list(title = metric_label,
                    font = list(size = 18, family = "Arial, sans-serif", weight = "bold"),
                    tickfont = list(size = 18, family = "Arial, sans-serif", weight = "bold")),
        barmode = 'stack',
        legend = list(title = list(text = group_by, font = list(size = 18, family = "Arial, sans-serif", weight = "bold")),
                     font = list(size = 18, family = "Arial, sans-serif", weight = "bold")),
        font = list(size = 18, family = "Arial, sans-serif", weight = "bold")
      )
  }
  
  return(p)
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
        fosarea %in% foremen_lookup$emp_num,
        foremen_lookup$shortname[match(fosarea, foremen_lookup$emp_num)],
        paste("FOS", fosarea)
      )
    )
  
  # Handle different data structures for weekly vs yearly data
  if ("inspdate" %in% colnames(table_data)) {
    # Yearly/treatments data has inspdate and other treatment details
    # Check which columns are available and select only existing ones
    available_cols <- colnames(table_data)
    
    base_cols <- c("inspdate", "sitecode", "facility", "foreman_name", "zone")
    optional_cols <- c("sectcode", "matcode")
    
    # Select base columns that exist, plus any optional columns that exist
    select_cols <- base_cols[base_cols %in% available_cols]
    optional_existing <- optional_cols[optional_cols %in% available_cols]
    
    table_data <- table_data %>%
      select(all_of(c(select_cols, optional_existing))) %>%
      arrange(desc(inspdate), facility, sitecode)
    
    # Rename columns for display
    col_names <- c("Treatment Date", "Site Code", "Facility", "Foreman", "Zone")
    if ("sectcode" %in% optional_existing) col_names <- c(col_names, "Section")
    if ("matcode" %in% optional_existing) col_names <- c(col_names, "Material")
    
    colnames(table_data) <- col_names
    
  } else {
    # Weekly active data doesn't have treatment details - check if acres column exists
    if ("acres" %in% colnames(table_data)) {
      table_data <- table_data %>%
        select(time_period, sitecode, facility, foreman_name, zone, acres) %>%
        arrange(desc(time_period), facility, sitecode)
      
      # Rename columns for display
      colnames(table_data) <- c("Week", "Site Code", "Facility", "Foreman", "Zone", "Acres")
    } else {
      # No acres column available for weekly data
      table_data <- table_data %>%
        select(time_period, sitecode, facility, foreman_name, zone) %>%
        arrange(desc(time_period), facility, sitecode)
      
      # Rename columns for display
      colnames(table_data) <- c("Week", "Site Code", "Facility", "Foreman", "Zone")
    }
  }
  
  # Create the data table
  dt_result <- DT::datatable(table_data,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = '_all')),
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({",
        "'font-weight': 'bold',", 
        "'font-size': '18px',",
        "'font-family': 'Arial, sans-serif'",
        "});",
        "$(this.api().table().container()).find('tbody td').css({",
        "'font-weight': 'bold',",
        "'font-size': '18px',", 
        "'font-family': 'Arial, sans-serif'",
        "});",
        "}"
      )
    ),
    rownames = FALSE
  )
  
  # Only format date if the first column is actually a date
  if ("Treatment Date" %in% colnames(table_data)) {
    dt_result <- dt_result %>% DT::formatDate("Treatment Date", method = "toDateString")
  }
  
  return(dt_result)
}
