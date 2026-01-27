# Overview - Historical Data Functions
# =============================================================================
# COMPLETELY DYNAMIC historical data loading.
# NO SQL HERE - calls app functions through the registry.
# NO metric-specific logic - iterates through registry.
# =============================================================================

library(dplyr)
library(lubridate)

# =============================================================================
# CONFIGURATION
# =============================================================================

#' Get the year range for historical data
#' @param n_years Number of years to include (default 5, includes current year)
#' @param analysis_date Date to use as "current" (default Sys.Date())
#' @return List with start_year and end_year
get_historical_year_range <- function(n_years = 5, analysis_date = NULL) {
  if (is.null(analysis_date)) analysis_date <- Sys.Date()
  current_year <- as.numeric(format(as.Date(analysis_date), "%Y"))
  list(
    start_year = current_year - n_years + 1,
    end_year = current_year
  )
}

# =============================================================================
# UNIFIED HISTORICAL DATA LOADER - COMPLETELY DYNAMIC
# =============================================================================

#' Load raw historical treatments from an app's data_functions.R
#' 
#' This is THE ONLY place that loads data. It dynamically sources the app's
#' data_functions.R and calls either load_raw_data (with include_archive=TRUE)
#' or load_historical_treatments if available.
#' 
#' @param metric_id The metric ID from registry
#' @param start_year Start year
#' @param end_year End year
#' @param zone_filter Zones to include
#' @return List with $treatments data frame containing inspdate column
load_app_historical_data <- function(metric_id, start_year, end_year, zone_filter = c("1", "2")) {
  
  registry <- get_metric_registry()
  config <- registry[[metric_id]]
  
  if (is.null(config)) {
    cat("ERROR: Metric", metric_id, "not found in registry\n")
    return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
  }
  
  apps_base <- get_apps_base_path()
  app_folder <- config$app_folder
  data_file <- file.path(apps_base, app_folder, "data_functions.R")
  
  if (!file.exists(data_file)) {
    cat("ERROR: data_functions.R not found for", metric_id, "\n")
    return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
  }
  
  env <- new.env(parent = globalenv())
  source(data_file, local = env, chdir = TRUE)
  
  tryCatch({
    # Try load_historical_treatments first (returns raw treatment data with inspdate)
    if ("load_historical_treatments" %in% names(env)) {
      result <- env$load_historical_treatments(
        start_year = start_year,
        end_year = end_year,
        zone_filter = zone_filter
      )
    } else if ("load_raw_data" %in% names(env)) {
      # Use standard load_raw_data with include_archive
      end_date <- as.Date(paste0(end_year, "-12-31"))
      result <- env$load_raw_data(
        analysis_date = end_date,
        include_archive = TRUE,
        start_year = start_year,
        end_year = end_year
      )
    } else {
      cat("ERROR: No historical loading function found for", metric_id, "\n")
      return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
    }
    
    # Filter by zone if treatments have zone column
    if (!is.null(result$treatments) && nrow(result$treatments) > 0) {
      if ("zone" %in% names(result$treatments) && !is.null(zone_filter)) {
        result$treatments <- result$treatments %>% filter(zone %in% zone_filter)
      }
    }
    
    result
    
  }, error = function(e) {
    cat("ERROR loading historical data for", metric_id, ":", e$message, "\n")
    list(sites = data.frame(), treatments = data.frame(), total_count = 0)
  })
}

# =============================================================================
# 5-YEAR AVERAGE VS CURRENT YEAR COMPARISON LOADER
# =============================================================================

#' Load historical comparison data: 5-year weekly average + current year
#' 
#' This function calculates ACTIVE treatments per week (not treatments done that week).
#' A treatment is ACTIVE on a given week if: inspdate <= week_friday AND inspdate + effect_days >= week_friday
#' 
#' @param metric Metric ID (from registry)
#' @param start_year Start year for 5-year average calculation
#' @param end_year End year (current year)
#' @param display_metric Metric to display: "treatment_acres", "treatments", "sites"
#' @param zone_filter Vector of zones to include
#' @return List with $average (5-yr avg by week) and $current (current year by week)
#' @export
load_historical_comparison_data <- function(metric,
                                             start_year = NULL,
                                             end_year = NULL,
                                             display_metric = "treatment_acres",
                                             zone_filter = c("1", "2"),
                                             analysis_date = NULL) {
  
  # Get default year range if not specified
  if (is.null(start_year) || is.null(end_year)) {
    years <- get_historical_year_range(5, analysis_date)
    start_year <- years$start_year
    end_year <- years$end_year
  }
  
  # Use analysis_date to filter out future data
  if (is.null(analysis_date)) analysis_date <- Sys.Date()
  analysis_date <- as.Date(analysis_date)
  
  # Load raw data from app
  raw_data <- load_app_historical_data(metric, start_year, end_year, zone_filter)
  
  if (is.null(raw_data$treatments) || nrow(raw_data$treatments) == 0) {
    return(list(average = data.frame(), current = data.frame(), total_count = raw_data$total_count %||% 0))
  }
  
  treatments <- raw_data$treatments
  
  # CRITICAL: Filter out treatments after analysis_date (don't show future data)
  treatments <- treatments %>% filter(inspdate <= analysis_date)
  
  # Ensure inspdate is Date type
  if (!"inspdate" %in% names(treatments)) {
    cat("ERROR: treatments missing inspdate column for", metric, "\n")
    return(list(average = data.frame(), current = data.frame()))
  }
  treatments$inspdate <- as.Date(treatments$inspdate)
  
  # Get registry config for metric
  registry <- get_metric_registry()
  config <- registry[[metric]]
  has_acres <- isTRUE(config$has_acres)
  use_active <- isTRUE(config$use_active_calculation)
  
  # Determine value column based on metric type
  if (has_acres && display_metric == "treatment_acres") {
    # Use acres if available
    acres_col <- if ("treated_acres" %in% names(treatments)) "treated_acres" 
                 else if ("acres" %in% names(treatments)) "acres"
                 else NULL
    
    if (!is.null(acres_col)) {
      treatments$value <- treatments[[acres_col]]
    } else {
      treatments$value <- 1
    }
  } else {
    treatments$value <- 1
  }
  
  current_year <- end_year
  n_prior_years <- current_year - start_year + 1
  
  # =========================================================================
  # ACTIVE TREATMENT CALCULATION (if use_active_calculation = TRUE)
  # For each week, count what's ACTIVE on that week's Friday
  # =========================================================================
  if (use_active) {
    # Get effect_days column (default to 14 for ground/drone, 28 for catch basin)
    if ("effect_days" %in% names(treatments)) {
      treatments$effect_days <- ifelse(is.na(treatments$effect_days), 14, treatments$effect_days)
    } else {
      # Default effect_days based on metric type
      treatments$effect_days <- if (metric == "catch_basin") 28 else 14
    }
    
    # Calculate treatment end date
    treatments <- treatments %>%
      mutate(treatment_end = inspdate + effect_days)
    
    # Get a unique ID for each treatment row (to avoid double-counting)
    # Use sitecode if available, otherwise row number
    if ("sitecode" %in% names(treatments)) {
      treatments$treatment_id <- treatments$sitecode
    } else if ("catchbasin_id" %in% names(treatments)) {
      treatments$treatment_id <- treatments$catchbasin_id
    } else {
      treatments$treatment_id <- seq_len(nrow(treatments))
    }
    
    # Generate weeks for all years in range
    weekly_data <- data.frame()
    
    for (yr in start_year:end_year) {
      start_date <- as.Date(paste0(yr, "-01-01"))
      end_date <- as.Date(paste0(yr, "-12-31"))
      weeks_in_year <- seq.Date(start_date, end_date, by = "week")
      
      for (week_start in weeks_in_year) {
        week_friday <- as.Date(week_start) + 4
        week_num <- week(week_friday)
        
        # Find treatments ACTIVE on this Friday
        active_on_friday <- treatments %>%
          filter(
            inspdate <= week_friday,
            treatment_end >= week_friday
          )
        
        if (nrow(active_on_friday) > 0) {
          # Sum value (acres for has_acres, count otherwise)
          # For unique sites, only count each site once per week
          if ("treatment_id" %in% names(active_on_friday)) {
            week_value <- active_on_friday %>%
              distinct(treatment_id, .keep_all = TRUE) %>%
              summarize(value = sum(value, na.rm = TRUE)) %>%
              pull(value)
          } else {
            week_value <- sum(active_on_friday$value, na.rm = TRUE)
          }
          
          weekly_data <- bind_rows(weekly_data, data.frame(
            year = yr,
            week_num = week_num,
            value = week_value
          ))
        }
      }
    }
  } else {
    # =========================================================================
    # SIMPLE TREATMENT COUNT (by week of treatment date)
    # =========================================================================
    treatments <- treatments %>%
      mutate(
        year = year(inspdate),
        week_num = week(inspdate)
      )
    
    weekly_data <- treatments %>%
      group_by(year, week_num) %>%
      summarize(value = sum(value, na.rm = TRUE), .groups = "drop")
  }
  
  # =========================================================================
  # SPLIT INTO PREVIOUS YEARS (for average) AND CURRENT YEAR
  # =========================================================================
  
  cat("DEBUG", metric, ": weekly_data rows =", nrow(weekly_data), "\n")
  if (nrow(weekly_data) > 0) {
    cat("DEBUG", metric, ": years in data =", paste(unique(weekly_data$year), collapse = ", "), "\n")
  }
  cat("DEBUG", metric, ": current_year =", current_year, "\n")
  
  previous_data <- weekly_data %>% filter(year < current_year)
  current_data <- weekly_data %>% filter(year == current_year)
  
  cat("DEBUG", metric, ": previous_data rows =", nrow(previous_data), ", current_data rows =", nrow(current_data), "\n")

  # Calculate average by week (previous years only)
  average_data <- previous_data %>%
    group_by(week_num) %>%
    summarize(
      value = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      time_period = sprintf("W%02d", week_num),
      group_label = paste0(n_prior_years, "-Year Avg")
    ) %>%
    select(time_period, week_num, value, group_label)
  
  # Format current year data
  current_formatted <- current_data %>%
    mutate(
      time_period = sprintf("W%02d", week_num),
      group_label = as.character(current_year)
    ) %>%
    select(time_period, week_num, value, group_label)
  
  cat("DEBUG", metric, ": FINAL average rows =", nrow(average_data), ", current rows =", nrow(current_formatted), "\n")
  
  list(
    average = average_data,
    current = current_formatted
  )
}

# =============================================================================
# LEGACY SUPPORT - load_historical_metric_data (wraps new function)
# =============================================================================

#' Load historical data for any metric (legacy interface)
#' 
#' @param metric Metric ID
#' @param start_year Start year
#' @param end_year End year
#' @param group_by Grouping: "mmcd_all", "zone", "facility"
#' @param time_period Aggregation: "yearly" or "weekly"
#' @param display_metric Metric to display
#' @param zone_filter Vector of zones
#' @return Data frame with time_period, group_label, value columns
#' @export
load_historical_metric_data <- function(metric,
                                        start_year = NULL,
                                        end_year = NULL,
                                        group_by = "mmcd_all",
                                        time_period = "weekly",
                                        display_metric = "treatment_acres",
                                        zone_filter = c("1", "2")) {
  
  # Get default year range if not specified
  if (is.null(start_year) || is.null(end_year)) {
    years <- get_historical_year_range(5)
    start_year <- years$start_year
    end_year <- years$end_year
  }
  
  # Load raw data from app
  raw_data <- load_app_historical_data(metric, start_year, end_year, zone_filter)
  
  if (is.null(raw_data$treatments) || nrow(raw_data$treatments) == 0) {
    return(data.frame())
  }
  
  treatments <- raw_data$treatments
  treatments$inspdate <- as.Date(treatments$inspdate)
  
  # Get registry config
  registry <- get_metric_registry()
  config <- registry[[metric]]
  has_acres <- isTRUE(config$has_acres)
  
  # Determine value
  if (has_acres && display_metric == "treatment_acres") {
    acres_col <- if ("treated_acres" %in% names(treatments)) "treated_acres" 
                 else if ("acres" %in% names(treatments)) "acres"
                 else NULL
    if (!is.null(acres_col)) {
      treatments$value <- treatments[[acres_col]]
    } else {
      treatments$value <- 1
    }
  } else {
    treatments$value <- 1
  }
  
  # Add time_period column
  if (time_period == "yearly") {
    treatments <- treatments %>%
      mutate(time_period = as.character(year(inspdate)))
  } else {
    treatments <- treatments %>%
      mutate(time_period = paste0(year(inspdate), "-W", sprintf("%02d", week(inspdate))))
  }
  
  # Apply grouping
  if (group_by == "mmcd_all") {
    result <- treatments %>%
      group_by(time_period) %>%
      summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(group_label = "All MMCD")
  } else if (group_by == "zone" && "zone" %in% names(treatments)) {
    result <- treatments %>%
      group_by(time_period, zone) %>%
      summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(group_label = paste0("P", zone))
  } else if (group_by == "facility" && "facility" %in% names(treatments)) {
    result <- treatments %>%
      group_by(time_period, facility) %>%
      summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(group_label = facility)
  } else {
    result <- treatments %>%
      group_by(time_period) %>%
      summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(group_label = "All MMCD")
  }
  
  result %>% select(time_period, group_label, value)
}

# =============================================================================
# CHART CREATION
# =============================================================================

#' Create a historical trend line chart
#' 
#' @param data Data frame with time_period, group_label, value columns
#' @param title Chart title
#' @param y_label Y-axis label
#' @param theme Color theme
#' @return Plotly chart
#' @export
create_historical_line_chart <- function(data, title = "Historical Trend", 
                                         y_label = "Acres", theme = "MMCD") {
  if (is.null(data) || nrow(data) == 0) {
    return(plotly_empty() %>% layout(title = list(text = title)))
  }
  
  # Get colors - use consistent historical comparison colors
  hist_colors <- get_historical_comparison_colors(theme = theme)
  
  # Create line chart with consistent colors
  p <- plot_ly() %>%
    layout(
      xaxis = list(title = "", tickangle = -45),
      yaxis = list(title = y_label),
      legend = list(orientation = "h", y = -0.2),
      margin = list(l = 60, r = 20, t = 30, b = 80),
      hovermode = "closest"
    ) %>%
    config(displayModeBar = FALSE)
  
  # Add traces with consistent colors
  for (group in unique(data$group_label)) {
    group_data <- data[data$group_label == group, ]
    
    # Determine color based on group label
    color <- if (grepl("Year Avg", group)) {
      hist_colors[["5-Year Avg"]]
    } else {
      hist_colors[["current_year"]]
    }
    
    p <- p %>% add_trace(
      data = group_data,
      x = ~time_period, 
      y = ~value,
      name = group,
      type = 'scatter', 
      mode = 'lines+markers',
      line = list(color = color),
      marker = list(color = color),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "%{y:,.0f}<br>",
        "<extra>%{fullData.name}</extra>"
      )
    )
  }
  
  return(p)
}

#' Create a historical stacked bar chart
#' 
#' @param data Data frame with time_period, group_label, value columns
#' @param title Chart title
#' @param y_label Y-axis label
#' @param theme Color theme
#' @return Plotly chart
#' @export
create_historical_bar_chart <- function(data, title = "Historical Trend",
                                        y_label = "Acres", theme = "MMCD") {
  if (is.null(data) || nrow(data) == 0) {
    return(plotly_empty() %>% layout(title = list(text = title)))
  }
  
  # Get colors - use consistent historical comparison colors  
  hist_colors <- get_historical_comparison_colors(theme = theme)
  
  # Create stacked bar chart with consistent colors
  p <- plot_ly() %>%
    layout(
      barmode = 'stack',
      xaxis = list(title = ""),
      yaxis = list(title = y_label),
      legend = list(orientation = "h", y = -0.2),
      margin = list(l = 60, r = 20, t = 30, b = 80)
    ) %>%
    config(displayModeBar = FALSE)
  
  # Add traces with consistent colors
  for (group in unique(data$group_label)) {
    group_data <- data[data$group_label == group, ]
    
    # Determine color based on group label
    color <- if (grepl("Year Avg", group)) {
      hist_colors[["5-Year Avg"]]
    } else {
      hist_colors[["current_year"]]
    }
    
    p <- p %>% add_trace(
      data = group_data,
      x = ~time_period, 
      y = ~value,
      name = group,
      type = 'bar',
      marker = list(color = color),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "%{y:,.0f}<br>",
        "<extra>%{fullData.name}</extra>"
      )
    )
  }
    config(displayModeBar = FALSE)
  
  return(p)
}
