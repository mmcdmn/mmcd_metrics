# Air Sites Historical Functions
# Functions for historical analysis including archive data

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(plotly)
})

# Get historical air sites data including archive tables
get_historical_air_sites_data <- function(analysis_date = Sys.Date(), start_year = NULL, end_year = NULL, 
                                         facility_filter = NULL, priority_filter = NULL, zone_filter = NULL, 
                                         larvae_threshold = 2, bti_effect_days_override = NULL) {
  # Call the main data function with archive flag
  get_air_sites_data(
    analysis_date = analysis_date,
    facility_filter = facility_filter,
    priority_filter = priority_filter, 
    zone_filter = zone_filter,
    larvae_threshold = larvae_threshold,
    bti_effect_days_override = bti_effect_days_override,
    include_archive = TRUE,
    start_year = start_year,
    end_year = end_year
  )
}

# Main historical data processing function - this is what the app calls
get_historical_processed_data <- function(hist_start_year = NULL, hist_end_year = NULL, hist_year = NULL,
                                         time_period = "yearly", group_by = "facility", 
                                         facility_filter = NULL, priority_filter = NULL, zone_filter = NULL,
                                         larvae_threshold = 2) {
  
  # Determine year range
  start_yr <- if (!is.null(hist_year)) hist_year else hist_start_year
  end_yr <- if (!is.null(hist_year)) hist_year else hist_end_year
  
  # Get historical data
  data <- get_historical_air_sites_data(
    analysis_date = Sys.Date(),
    start_year = start_yr,
    end_year = end_yr,
    facility_filter = facility_filter,
    priority_filter = priority_filter,
    zone_filter = zone_filter,
    larvae_threshold = larvae_threshold
  )
  
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Add time period grouping
  if (time_period == "weekly") {
    data$time_period <- paste0(lubridate::year(as.Date(data$last_inspection_date)), 
                               "-W", lubridate::week(as.Date(data$last_inspection_date)))
  } else if (time_period == "monthly") {
    data$time_period <- format(as.Date(data$last_inspection_date), "%Y-%m")
  } else {
    data$time_period <- lubridate::year(as.Date(data$last_inspection_date))
  }
  
  # Add grouping variable
  if (group_by == "facility") {
    data$group_var <- data$facility
  } else if (group_by == "priority") {
    data$group_var <- data$priority
  } else if (group_by == "zone") {
    data$group_var <- data$zone
  } else {
    data$group_var <- "All"
  }
  
  # Calculate red bug inspection ratios and other metrics
  data <- data %>%
    mutate(
      has_inspection = !is.na(last_inspection_date),
      has_sample = !is.na(sampnum_yr) & sampnum_yr != "",
      has_lab_result = !is.na(lab_id_timestamp),
      is_red_bug_site = has_red_bugs == 1 & has_lab_result,
      inspection_year = lubridate::year(as.Date(last_inspection_date)),
      treatment_year = lubridate::year(as.Date(last_treatment_date))
    )
  
  return(data)
}

# Create historical summary metrics for value boxes
create_historical_summary_metrics <- function(data) {
  if (nrow(data) == 0) {
    return(list(
      total_inspections = 0,
      red_bug_inspections = 0,
      avg_red_bug_ratio = "0%",
      groups_with_data = 0,
      sites_with_samples = 0,
      completed_lab_samples = 0
    ))
  }
  
  # Calculate key historical metrics
  total_inspections <- sum(data$has_inspection, na.rm = TRUE)
  sites_with_samples <- sum(data$has_sample, na.rm = TRUE)
  completed_lab_samples <- sum(data$has_lab_result, na.rm = TRUE)
  red_bug_inspections <- sum(data$is_red_bug_site, na.rm = TRUE)
  
  # Calculate red bug ratio - red bug sites / sites with completed lab results
  red_bug_ratio <- if (completed_lab_samples > 0) {
    round((red_bug_inspections / completed_lab_samples) * 100, 1)
  } else {
    0
  }
  
  # Count groups (facilities/zones/priorities) that have data
  groups_with_data <- length(unique(data$group_var[data$has_inspection]))
  
  return(list(
    total_inspections = total_inspections,
    red_bug_inspections = red_bug_inspections,
    avg_red_bug_ratio = paste0(red_bug_ratio, "%"),
    groups_with_data = groups_with_data,
    sites_with_samples = sites_with_samples,
    completed_lab_samples = completed_lab_samples
  ))
}

# Create historical summary chart showing red bug detection trends
create_historical_summary_chart <- function(data, time_period = "monthly") {
  if (nrow(data) == 0) {
    return(plot_ly() %>%
      add_annotations(
        text = "No historical data available",
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE
      ))
  }
  
  # Group data by time period and calculate red bug detection rates
  chart_data <- data %>%
    group_by(time_period_label) %>%
    summarise(
      inspections = sum(has_inspection, na.rm = TRUE),
      samples = sum(has_sample, na.rm = TRUE),
      lab_results = sum(has_lab_result, na.rm = TRUE),
      red_bug_sites = sum(is_red_bug_site, na.rm = TRUE),
      detection_rate = if_else(lab_results > 0, 
                              round((red_bug_sites / lab_results) * 100, 1), 
                              0),
      .groups = 'drop'
    ) %>%
    filter(inspections > 0) %>%  # Only include periods with actual data
    arrange(time_period_label)
  
  if (nrow(chart_data) == 0) {
    return(plot_ly() %>%
      add_annotations(
        text = "No data for selected time period",
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE
      ))
  }
  
  # Create dual-axis chart: bar chart for inspections, line for detection rate
  p <- plot_ly(chart_data) %>%
    
    # Add bar chart for number of inspections
    add_bars(
      x = ~time_period_label,
      y = ~inspections,
      name = "Total Inspections",
      marker = list(color = "#3498db"),
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Inspections: %{y}<br>",
        "Samples: ", chart_data$samples, "<br>",
        "Lab Results: ", chart_data$lab_results, "<br>",
        "<extra></extra>"
      )
    ) %>%
    
    # Add line chart for red bug detection rate
    add_lines(
      x = ~time_period_label,
      y = ~detection_rate,
      yaxis = "y2",
      name = "Red Bug Detection Rate (%)",
      line = list(color = "#e74c3c", width = 3),
      marker = list(color = "#e74c3c", size = 6),
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Detection Rate: %{y}%<br>",
        "Red Bug Sites: ", chart_data$red_bug_sites, "<br>",
        "<extra></extra>"
      )
    ) %>%
    
    # Configure layout with dual y-axes
    layout(
      title = list(
        text = paste0("Red Bug Detection Trends (", stringr::str_to_title(time_period), ")"),
        font = list(size = 16, color = "#2c3e50")
      ),
      xaxis = list(
        title = list(text = stringr::str_to_title(time_period), font = list(size = 12)),
        tickangle = if(nrow(chart_data) > 8) -45 else 0
      ),
      yaxis = list(
        title = list(text = "Number of Inspections", font = list(size = 12)),
        side = "left",
        color = "#3498db"
      ),
      yaxis2 = list(
        title = list(text = "Red Bug Detection Rate (%)", font = list(size = 12)),
        side = "right",
        overlaying = "y",
        color = "#e74c3c",
        range = c(0, max(c(chart_data$detection_rate, 10), na.rm = TRUE) * 1.1)
      ),
      hovermode = "x unified",
      legend = list(
        x = 0.02, y = 0.98,
        bgcolor = "rgba(255,255,255,0.8)",
        bordercolor = "rgba(0,0,0,0.2)",
        borderwidth = 1
      ),
      margin = list(t = 60, r = 60, b = 60, l = 60)
    )
  
  return(p)
}

# Create facility-level historical analysis
create_facility_historical_analysis <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Extract years and create facility summary
  data$year <- lubridate::year(as.Date(data$last_inspection_date))
  data$year[is.na(data$year)] <- lubridate::year(as.Date(data$last_treatment_date))[is.na(data$year)]
  
  # Remove rows without valid years
  data <- data[!is.na(data$year), ]
  
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Create facility-level summary
  facility_summary <- data %>%
    group_by(facility, year) %>%
    summarise(
      total_sites = n(),
      active_treatments = sum(site_status == "Active Treatment", na.rm = TRUE),
      needs_treatment = sum(site_status == "Needs Treatment", na.rm = TRUE),
      in_lab = sum(site_status == "In Lab", na.rm = TRUE),
      inspected = sum(site_status == "Inspected", na.rm = TRUE),
      unknown = sum(site_status == "Unknown", na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      treatment_rate = round((active_treatments / (active_treatments + needs_treatment + 0.01)) * 100, 1),
      inspection_rate = round(((inspected + needs_treatment + active_treatments + in_lab) / total_sites) * 100, 1)
    ) %>%
    arrange(facility, year)
  
  return(facility_summary)
}

# Create red bug historical analysis
create_red_bug_historical_analysis <- function(data) {
  if (nrow(data) == 0) {
    return(list(
      red_bug_trends = data.frame(),
      detection_rates = data.frame()
    ))
  }
  
  # Extract years
  data$year <- lubridate::year(as.Date(data$last_inspection_date))
  
  # Filter for samples with lab results
  lab_data <- data[!is.na(data$sampnum_yr) & !is.na(data$redblue) & !is.na(data$year), ]
  
  if (nrow(lab_data) == 0) {
    return(list(
      red_bug_trends = data.frame(),
      detection_rates = data.frame()
    ))
  }
  
  # Calculate red bug trends by year
  red_bug_trends <- lab_data %>%
    group_by(year) %>%
    summarise(
      total_samples = n(),
      red_bugs = sum(has_red_bugs == 1, na.rm = TRUE),
      blue_bugs = sum(redblue == 'B', na.rm = TRUE),
      red_bug_rate = round((red_bugs / total_samples) * 100, 1),
      .groups = 'drop'
    ) %>%
    arrange(year)
  
  # Calculate detection rates by facility and year
  detection_rates <- lab_data %>%
    group_by(facility, year) %>%
    summarise(
      samples = n(),
      red_detections = sum(has_red_bugs == 1, na.rm = TRUE),
      detection_rate = round((red_detections / samples) * 100, 1),
      .groups = 'drop'
    ) %>%
    filter(samples >= 3) %>%  # Only include facilities with at least 3 samples
    arrange(facility, year)
  
  return(list(
    red_bug_trends = red_bug_trends,
    detection_rates = detection_rates
  ))
}

# Create year-over-year comparison chart
create_year_comparison_chart <- function(data, comparison_metric = "treatment_rate") {
  if (nrow(data) == 0) {
    return(plot_ly() %>%
      add_annotations(
        text = "No data available for comparison",
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper", 
        showarrow = FALSE
      ))
  }
  
  # Get facility historical analysis
  facility_data <- create_facility_historical_analysis(data)
  
  if (nrow(facility_data) == 0) {
    return(plot_ly() %>%
      add_annotations(
        text = "No facility data available",
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE
      ))
  }
  
  # Create comparison chart based on selected metric
  if (comparison_metric == "treatment_rate") {
    p <- plot_ly(facility_data, x = ~year, y = ~treatment_rate, color = ~facility,
                 type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = "Treatment Rate Trends by Facility",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Treatment Rate (%)"),
        hovermode = 'x unified'
      )
  } else if (comparison_metric == "inspection_rate") {
    p <- plot_ly(facility_data, x = ~year, y = ~inspection_rate, color = ~facility,
                 type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = "Inspection Coverage Trends by Facility", 
        xaxis = list(title = "Year"),
        yaxis = list(title = "Inspection Coverage (%)"),
        hovermode = 'x unified'
      )
  } else {
    p <- plot_ly(facility_data, x = ~year, y = ~total_sites, color = ~facility,
                 type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = "Total Sites by Facility Over Time",
        xaxis = list(title = "Year"), 
        yaxis = list(title = "Number of Sites"),
        hovermode = 'x unified'
      )
  }
  
  return(p)
}