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
# Get simplified historical air sites inspection summary
get_historical_inspection_summary <- function(start_year = NULL, end_year = NULL, 
                                            facility_filter = NULL, priority_filter = NULL, 
                                            zone_filter = NULL, larvae_threshold = 2) {
  
  # Determine year range
  if (is.null(start_year)) start_year <- as.numeric(format(Sys.Date(), "%Y")) - 4
  if (is.null(end_year)) end_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Debug logging
  cat("Historical inspection summary - Start Year:", start_year, "End Year:", end_year, "\n")
  cat("Filters - Facility:", facility_filter, "Priority:", priority_filter, "Zone:", zone_filter, "\n")
  
  # Get all air sites data for the period
  data <- get_air_sites_data(
    analysis_date = paste0(end_year, "-12-31"),  # Use end of year range
    facility_filter = facility_filter,
    priority_filter = priority_filter,
    zone_filter = zone_filter,
    larvae_threshold = larvae_threshold,
    include_archive = TRUE,
    start_year = start_year,
    end_year = end_year
  )
  
  cat("Raw historical data returned:", nrow(data), "rows\n")
  
  if (nrow(data) == 0) {
    return(data.frame(
      sitecode = character(0),
      facility = character(0),
      priority = character(0),
      zone = character(0),
      acres = numeric(0),
      total_inspections = integer(0),
      red_bug_inspections = integer(0),
      red_bug_ratio = numeric(0),
      years_active = character(0)
    ))
  }
  
  # Get inspection details from archive for this period
  con <- get_db_connection()
  if (is.null(con)) {
    cat("Could not connect to database for detailed inspection counts\n")
    return(data.frame())
  }
  
  tryCatch({
    # Build filter conditions for SQL
    facility_condition <- ""
    if (!is.null(facility_filter) && length(facility_filter) > 0) {
      facility_list <- paste0("'", paste(facility_filter, collapse = "', '"), "'")
      # Filter on gis_sectcode facility, with fallback to site facility
      facility_condition <- sprintf("AND (g.facility IN (%s) OR (g.facility IS NULL AND b.facility IN (%s)))", facility_list, facility_list)
    }
    
    priority_condition <- ""
    if (!is.null(priority_filter) && length(priority_filter) > 0) {
      priority_list <- paste0("'", paste(priority_filter, collapse = "', '"), "'")
      priority_condition <- sprintf("AND b.priority IN (%s)", priority_list)
    }
    
    zone_condition <- ""
    if (!is.null(zone_filter) && zone_filter != "All") {
      if (zone_filter == "P1 + P2 Combined") {
        zone_condition <- "AND g.zone IN ('1', '2')"
      } else if (zone_filter == "P3 + P4 Combined") {
        zone_condition <- "AND g.zone IN ('3', '4')"
      } else {
        # Extract just the number for single zones (P1 -> 1, P2 -> 2, etc.)
        zone_num <- gsub("P", "", zone_filter)
        zone_condition <- sprintf("AND g.zone = '%s'", zone_num)
      }
    }
    
    # Get detailed inspection data - corrected red bug ratio calculation
    # Total inspections = ALL inspections (including those with numdip=0 or no samples)
    # Red bug inspections = Only those with samples showing redblue='R'
    # Inspections above threshold = Those with numdip >= threshold
    # Handle multiple site records by matching inspection date with site active period
    # Include BOTH archive AND current tables for complete historical data
    inspection_query <- sprintf("
      WITH combined_inspections AS (
        -- Archive inspection data
        SELECT 
          sitecode,
          inspdate,
          sampnum_yr,
          numdip,
          action,
          'archive' as source_table
        FROM dblarv_insptrt_archive
        WHERE action IN ('2', '4')
          AND EXTRACT(YEAR FROM inspdate) BETWEEN %d AND %d
        
        UNION ALL
        
        -- Current inspection data
        SELECT 
          sitecode,
          inspdate,
          sampnum_yr,
          numdip,
          action,
          'current' as source_table
        FROM dblarv_insptrt_current
        WHERE action IN ('2', '4')
          AND EXTRACT(YEAR FROM inspdate) BETWEEN %d AND %d
      ),
      all_inspections AS (
        SELECT 
          b.sitecode,
          COALESCE(g.facility, b.facility) as facility,  -- Use gis_sectcode facility first, fallback to site facility
          b.priority,
          g.zone,
          b.acres,
          i.inspdate,
          i.sampnum_yr,
          i.numdip,
          i.source_table,
          ROW_NUMBER() OVER (PARTITION BY i.sitecode, i.inspdate ORDER BY 
            CASE WHEN b.enddate IS NULL THEN 1 ELSE 0 END DESC,
            b.enddate DESC
          ) as rn
        FROM loc_breeding_sites b
        LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(b.sitecode, 7)
        INNER JOIN combined_inspections i ON b.sitecode = i.sitecode
        WHERE (b.enddate IS NULL OR b.enddate >= i.inspdate)
          AND b.air_gnd = 'A'
          AND b.geom IS NOT NULL
          %s
          %s
          %s
      ),
      filtered_inspections AS (
        SELECT * FROM all_inspections WHERE rn = 1
      ),
      combined_samples AS (
        -- Archive sample data
        SELECT 
          sampnum_yr,
          redblue,
          missing,
          form_type,
          'archive' as source_table
        FROM dblarv_sample_archive
        WHERE form_type = 'AIR'
          AND (missing = FALSE OR missing IS NULL)
        
        UNION ALL
        
        -- Current sample data  
        SELECT 
          sampnum_yr,
          redblue,
          missing,
          form_type,
          'current' as source_table
        FROM dblarv_sample_current
        WHERE form_type = 'AIR'
          AND (missing = FALSE OR missing IS NULL)
      ),
      red_bug_samples AS (
        SELECT 
          fi.sitecode,
          COUNT(CASE WHEN cs.redblue = 'R' THEN 1 END) as red_bug_count,
          COUNT(CASE WHEN cs.redblue IS NOT NULL THEN 1 END) as total_samples
        FROM filtered_inspections fi
        LEFT JOIN combined_samples cs ON fi.sampnum_yr = cs.sampnum_yr
        GROUP BY fi.sitecode
      )
      SELECT 
        fi.sitecode,
        fi.facility,
        fi.priority,
        fi.zone,
        fi.acres,
        COUNT(*)::INTEGER as total_inspections,
        SUM(CASE WHEN fi.numdip >= %d THEN 1 ELSE 0 END)::INTEGER as inspections_above_threshold,
        COALESCE(rbs.red_bug_count, 0)::INTEGER as red_bug_inspections,
        COALESCE(rbs.total_samples, 0)::INTEGER as samples_with_results,
        COUNT(CASE WHEN fi.source_table = 'archive' THEN 1 END)::INTEGER as archive_inspections,
        COUNT(CASE WHEN fi.source_table = 'current' THEN 1 END)::INTEGER as current_inspections,
        ARRAY_AGG(DISTINCT EXTRACT(YEAR FROM fi.inspdate) ORDER BY EXTRACT(YEAR FROM fi.inspdate)) as years_with_inspections
      FROM filtered_inspections fi
      LEFT JOIN red_bug_samples rbs ON fi.sitecode = rbs.sitecode
      GROUP BY fi.sitecode, fi.facility, fi.priority, fi.zone, fi.acres, rbs.red_bug_count, rbs.total_samples
      ORDER BY fi.sitecode
    ", start_year, end_year, start_year, end_year, 
       facility_condition, priority_condition, zone_condition, larvae_threshold)
    
    cat("Executing inspection summary query...\n")
    cat("Hope this will give the correct results.\n")
    result <- dbGetQuery(con, inspection_query)
    
    # Debug: Check data types and sample values
    if (nrow(result) > 0) {
      cat("Data types in result:\n")
      print(sapply(result, class))
      cat("\nSample of sites with potential red bugs:\n")
      red_candidates <- result[result$red_bug_inspections > 0, ]
      if (nrow(red_candidates) > 0) {
        print(red_candidates[1:min(3, nrow(red_candidates)), c("sitecode", "total_inspections", "red_bug_inspections", "archive_inspections", "current_inspections")])
      }
      
      # Show summary of archive vs current data
      cat("\nData source summary:\n")
      cat("Total archive inspections:", sum(result$archive_inspections), "\n")
      cat("Total current inspections:", sum(result$current_inspections), "\n")
      cat("Total combined inspections:", sum(result$total_inspections), "\n")
    }
    
    cat("Inspection summary query returned:", nrow(result), "rows\n")
    
    if (nrow(result) > 0) {
      # Ensure numeric types and calculate red bug ratio
      result$total_inspections <- as.numeric(result$total_inspections)
      result$red_bug_inspections <- as.numeric(result$red_bug_inspections)
      result$samples_with_results <- as.numeric(result$samples_with_results)
      result$archive_inspections <- as.numeric(result$archive_inspections)
      result$current_inspections <- as.numeric(result$current_inspections)
      
      # Calculate red bug ratio
      result$red_bug_ratio <- ifelse(result$total_inspections > 0,
                                    round((result$red_bug_inspections / result$total_inspections) * 100, 1),
                                    0)
      
      # Format years as readable string
      result$years_active <- sapply(result$years_with_inspections, function(x) {
        if (is.null(x) || length(x) == 0) return("None")
        years <- sort(as.numeric(unlist(strsplit(gsub("[{}]", "", x), ","))))
        if (length(years) <= 3) {
          return(paste(years, collapse = ", "))
        } else {
          return(paste(min(years), "-", max(years), "(", length(years), "years)"))
        }
      })
      
      # Clean up zone display
      result$zone <- ifelse(is.na(result$zone), "Unknown", paste0("P", result$zone))
      
      # Select final columns
      result <- result[, c("sitecode", "facility", "priority", "zone", "acres", 
                          "total_inspections", "red_bug_inspections", "red_bug_ratio", "years_active")]
    }
    
    return(result)
    
  }, error = function(e) {
    cat("Error in historical inspection summary:", e$message, "\n")
    return(data.frame())
  }, finally = {
    dbDisconnect(con)
  })
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
    group_by(time_period) %>%
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
    arrange(time_period)
  
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
      x = ~time_period,
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
      x = ~time_period,
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

# Create historical red bug chart - wrapper function for backward compatibility
create_historical_red_bug_chart <- function(data, time_period = "monthly", group_by = "facility") {
  return(create_historical_summary_chart(data, time_period))
}

# Create historical details table
create_historical_details_table <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame(
      sitecode = character(0),
      facility = character(0),
      priority = character(0),
      zone = character(0),
      last_inspection = character(0),
      last_treatment = character(0),
      has_samples = character(0),
      red_bugs = character(0)
    ))
  }
  
  # Create summary table
  table_data <- data %>%
    mutate(
      last_inspection = if_else(!is.na(last_inspection_date), 
                               format(as.Date(last_inspection_date), "%Y-%m-%d"), 
                               "None"),
      last_treatment = if_else(!is.na(last_treatment_date), 
                              format(as.Date(last_treatment_date), "%Y-%m-%d"), 
                              "None"),
      has_samples = if_else(has_sample, "Yes", "No"),
      red_bugs = if_else(is_red_bug_site, "Yes", if_else(has_lab_result, "No", "No Sample"))
    ) %>%
    select(sitecode, facility, priority, zone, last_inspection, last_treatment, has_samples, red_bugs) %>%
    arrange(sitecode)
  
  return(table_data)
}