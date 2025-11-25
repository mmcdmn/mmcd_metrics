# Air Sites Historical Functions
# Functions for historical analysis including archive data

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
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

# Get comprehensive historical data in a single optimized query
# Returns both inspection summary and treatment volume data
get_comprehensive_historical_data <- function(start_year = NULL, end_year = NULL,
                                            facility_filter = NULL, priority_filter = NULL, 
                                            zone_filter = NULL, larvae_threshold = 2) {
  
  # Determine year range
  if (is.null(start_year)) start_year <- as.numeric(format(Sys.Date(), "%Y")) - 4
  if (is.null(end_year)) end_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Debug logging
  cat("Comprehensive historical data - Start Year:", start_year, "End Year:", end_year, "\n")
  cat("Filters - Facility:", ifelse(is.null(facility_filter), "ALL", paste(facility_filter, collapse=",")), 
      "Priority:", ifelse(is.null(priority_filter), "ALL", paste(priority_filter, collapse=",")), 
      "Zone:", ifelse(is.null(zone_filter) || zone_filter == "All", "ALL", zone_filter), "\n")
  
  con <- get_db_connection()
  if (is.null(con)) {
    cat("Could not connect to database for comprehensive historical data\n")
    return(list(
      inspection_summary = data.frame(),
      treatment_volumes = data.frame()
    ))
  }
  
  tryCatch({
    # Build filter conditions for SQL - improved for better performance
    facility_condition <- ""
    if (!is.null(facility_filter) && length(facility_filter) > 0) {
      facility_list <- paste0("'", paste(facility_filter, collapse = "', '"), "'")
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
        zone_num <- gsub("P", "", zone_filter)
        zone_condition <- sprintf("AND g.zone = '%s'", zone_num)
      }
    }
    
    # Single comprehensive query for all historical data
    comprehensive_query <- sprintf("
      WITH combined_operations AS (
        -- Archive inspection data (actions 2, 4)
        SELECT 
          i.inspdate,
          i.sitecode,
          i.action,
          i.numdip,
          i.sampnum_yr,
          b.acres,  -- Site acres for inspections
          'inspection' as operation_type,
          'archive' as source_table,
          COALESCE(g.facility, b.facility) as facility,
          b.priority,
          g.zone
        FROM dblarv_insptrt_archive i
        INNER JOIN loc_breeding_sites b ON i.sitecode = b.sitecode
        LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(i.sitecode, 7)
        WHERE i.action IN ('2', '4')
          AND b.air_gnd = 'A'
          AND b.geom IS NOT NULL
          -- CRITICAL: Only include sites that were active during the inspection date
          AND (b.startdate IS NULL OR b.startdate <= i.inspdate)
          AND (b.enddate IS NULL OR b.enddate > i.inspdate)
          AND EXTRACT(YEAR FROM i.inspdate) BETWEEN %d AND %d
          %s
          %s
          %s
        
        UNION ALL
        
        -- Current inspection data (actions 2, 4)  
        SELECT 
          i.inspdate,
          i.sitecode,
          i.action,
          i.numdip,
          i.sampnum_yr,
          b.acres,  -- Site acres for inspections
          'inspection' as operation_type,
          'current' as source_table,
          COALESCE(g.facility, b.facility) as facility,
          b.priority,
          g.zone
        FROM dblarv_insptrt_current i
        INNER JOIN loc_breeding_sites b ON i.sitecode = b.sitecode
        LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(i.sitecode, 7)
        WHERE i.action IN ('2', '4')
          AND b.air_gnd = 'A'
          AND b.geom IS NOT NULL
          -- CRITICAL: Only include sites that were active during the inspection date
          AND (b.startdate IS NULL OR b.startdate <= i.inspdate)
          AND (b.enddate IS NULL OR b.enddate > i.inspdate)
          AND EXTRACT(YEAR FROM i.inspdate) BETWEEN %d AND %d
          %s
          %s
          %s
          
        UNION ALL
        
        -- Archive treatment data (actions 3, A, D)
        SELECT 
          t.inspdate,
          t.sitecode,
          t.action,
          NULL::numeric as numdip,
          NULL as sampnum_yr,
          t.acres,  -- Treatment acres
          'treatment' as operation_type,
          'archive' as source_table,
          COALESCE(g.facility, b.facility) as facility,
          b.priority,
          g.zone
        FROM dblarv_insptrt_archive t
        INNER JOIN loc_breeding_sites b ON t.sitecode = b.sitecode
        LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(t.sitecode, 7)
        WHERE t.action IN ('3', 'A', 'D')
          AND t.acres > 0
          AND b.air_gnd = 'A'
          -- CRITICAL: Only include sites that were active during the treatment date
          AND (b.startdate IS NULL OR b.startdate <= t.inspdate)
          AND (b.enddate IS NULL OR b.enddate > t.inspdate)
          AND EXTRACT(YEAR FROM t.inspdate) BETWEEN %d AND %d
          %s
          %s
          %s
        
        UNION ALL
        
        -- Current treatment data (actions 3, A, D)
        SELECT 
          t.inspdate,
          t.sitecode,
          t.action,
          NULL::numeric as numdip,
          NULL as sampnum_yr,
          t.acres,  -- Treatment acres
          'treatment' as operation_type,
          'current' as source_table,
          COALESCE(g.facility, b.facility) as facility,
          b.priority,
          g.zone
        FROM dblarv_insptrt_current t
        INNER JOIN loc_breeding_sites b ON t.sitecode = b.sitecode
        LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(t.sitecode, 7)
        WHERE t.action IN ('3', 'A', 'D')
          AND t.acres > 0
          AND b.air_gnd = 'A'
          -- CRITICAL: Only include sites that were active during the treatment date
          AND (b.startdate IS NULL OR b.startdate <= t.inspdate)
          AND (b.enddate IS NULL OR b.enddate > t.inspdate)
          AND EXTRACT(YEAR FROM t.inspdate) BETWEEN %d AND %d
          %s
          %s
          %s
      ),
      
      -- Get sample data
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
      
      -- Site inspection summary - now properly filtered by active periods
      site_inspection_summary AS (
        SELECT 
          o.sitecode,
          o.facility,
          o.priority,
          o.zone,
          MAX(o.acres) as acres,  -- Use MAX since inspection acres = site acres
          COUNT(CASE WHEN o.operation_type = 'inspection' THEN 1 END)::INTEGER as total_inspections,
          COUNT(CASE WHEN o.operation_type = 'inspection' AND o.numdip >= %d THEN 1 END)::INTEGER as inspections_above_threshold,
          COUNT(CASE WHEN o.operation_type = 'inspection' AND cs.redblue = 'R' THEN 1 END)::INTEGER as red_bug_inspections,
          COUNT(CASE WHEN o.operation_type = 'inspection' AND cs.redblue IS NOT NULL THEN 1 END)::INTEGER as samples_with_results,
          ARRAY_AGG(DISTINCT EXTRACT(YEAR FROM o.inspdate) ORDER BY EXTRACT(YEAR FROM o.inspdate)) FILTER (WHERE o.operation_type = 'inspection') as years_with_inspections
        FROM combined_operations o
        LEFT JOIN combined_samples cs ON o.sampnum_yr = cs.sampnum_yr
        WHERE o.operation_type = 'inspection'
        GROUP BY o.sitecode, o.facility, o.priority, o.zone
      ),
      
      -- Treatment volume summary by date/facility - now properly filtered
      treatment_volume_summary AS (
        SELECT 
          o.inspdate,
          o.operation_type,
          o.facility,
          o.priority,
          o.zone,
          COUNT(*) as total_operations,
          SUM(o.acres) as total_acres,
          EXTRACT(YEAR FROM o.inspdate) as year,
          EXTRACT(WEEK FROM o.inspdate) as week_of_year,
          DATE_TRUNC('week', o.inspdate)::date as week_start_date,
          o.source_table
        FROM combined_operations o
        GROUP BY o.inspdate, o.operation_type, o.facility, o.priority, o.zone, 
                 EXTRACT(YEAR FROM o.inspdate), EXTRACT(WEEK FROM o.inspdate), 
                 DATE_TRUNC('week', o.inspdate)::date, o.source_table
      )
      
      -- Return both datasets
      SELECT 'inspection_summary' as dataset_type, 
             sitecode, facility, priority, zone, acres,
             total_inspections, red_bug_inspections, 
             samples_with_results, years_with_inspections::text,
             NULL::date as inspdate, NULL as operation_type, 
             NULL::integer as total_operations, NULL::numeric as year,
             NULL::numeric as week_of_year, NULL::date as week_start_date,
             NULL as source_table, NULL::integer as inspections_above_threshold
      FROM site_inspection_summary
      
      UNION ALL
      
      SELECT 'treatment_volumes' as dataset_type,
             NULL as sitecode, facility, priority, zone, total_acres as acres,
             NULL::integer as total_inspections, NULL::integer as red_bug_inspections,
             NULL::integer as samples_with_results, NULL as years_with_inspections,
             inspdate, operation_type, total_operations, year,
             week_of_year, week_start_date, source_table, NULL::integer as inspections_above_threshold
      FROM treatment_volume_summary
      
      ORDER BY dataset_type, sitecode, inspdate
    ", start_year, end_year, facility_condition, priority_condition, zone_condition,
       start_year, end_year, facility_condition, priority_condition, zone_condition,
       start_year, end_year, facility_condition, priority_condition, zone_condition,
       start_year, end_year, facility_condition, priority_condition, zone_condition,
       larvae_threshold)
    
    cat("Executing comprehensive historical query...\n")
    result <- dbGetQuery(con, comprehensive_query)
    
    cat("Comprehensive query returned:", nrow(result), "rows\n")
    
    if (nrow(result) > 0) {
      # Split the results into two datasets
      inspection_data <- result[result$dataset_type == 'inspection_summary', ]
      volume_data <- result[result$dataset_type == 'treatment_volumes', ]
      
      # Process inspection summary
      if (nrow(inspection_data) > 0) {
        # Calculate red bug ratio
        inspection_data$red_bug_ratio <- ifelse(inspection_data$total_inspections > 0,
                                              round((inspection_data$red_bug_inspections / inspection_data$total_inspections) * 100, 1),
                                              0)
        
        # Format years as readable string
        inspection_data$years_active <- sapply(inspection_data$years_with_inspections, function(x) {
          if (is.null(x) || length(x) == 0 || x == "") return("None")
          years <- sort(as.numeric(unlist(strsplit(gsub("[{}]", "", x), ","))))
          if (length(years) <= 3) {
            return(paste(years, collapse = ", "))
          } else {
            return(paste(min(years), "-", max(years), "(", length(years), "years)"))
          }
        })
        
        # Clean up zone display
        inspection_data$zone <- ifelse(is.na(inspection_data$zone), "Unknown", paste0("P", inspection_data$zone))
        
        # Select final columns for inspection summary
        inspection_data <- inspection_data[, c("sitecode", "facility", "priority", "zone", "acres", 
                                             "total_inspections", "red_bug_inspections", "red_bug_ratio", "years_active")]
      }
      
      # Process volume data
      if (nrow(volume_data) > 0) {
        # Clean up zone display
        volume_data$zone <- ifelse(is.na(volume_data$zone), "Unknown", paste0("P", volume_data$zone))
        
        # Select relevant columns for volume data
        volume_data <- volume_data[, c("inspdate", "operation_type", "facility", "priority", "zone", 
                                     "total_operations", "acres", "year", "week_of_year", "week_start_date", "source_table")]
        colnames(volume_data)[colnames(volume_data) == "acres"] <- "total_acres"
      }
      
      cat("Processed data - Inspection sites:", nrow(inspection_data), 
          "Volume records:", nrow(volume_data), "\n")
      
      return(list(
        inspection_summary = inspection_data,
        treatment_volumes = volume_data
      ))
      
    } else {
      return(list(
        inspection_summary = data.frame(),
        treatment_volumes = data.frame()
      ))
    }
    
  }, error = function(e) {
    cat("Error in comprehensive historical query:", e$message, "\n")
    return(list(
      inspection_summary = data.frame(),
      treatment_volumes = data.frame()
    ))
  }, finally = {
    dbDisconnect(con)
  })
}
  
# Optimized wrapper functions using the comprehensive data source

# Get simplified historical air sites inspection summary
get_historical_inspection_summary <- function(start_year = NULL, end_year = NULL, 
                                            facility_filter = NULL, priority_filter = NULL, 
                                            zone_filter = NULL, larvae_threshold = 2) {
  
  # Use comprehensive function and return just the inspection summary
  comprehensive_data <- get_comprehensive_historical_data(
    start_year = start_year,
    end_year = end_year,
    facility_filter = facility_filter,
    priority_filter = priority_filter,
    zone_filter = zone_filter,
    larvae_threshold = larvae_threshold
  )
  
  return(comprehensive_data$inspection_summary)
}

# Get historical treatment volume data (now includes priority filter)
get_historical_treatment_volumes <- function(start_year = NULL, end_year = NULL, 
                                           facility_filter = NULL, priority_filter = NULL, zone_filter = NULL) {
  
  # Use comprehensive function and return just the treatment volumes
  comprehensive_data <- get_comprehensive_historical_data(
    start_year = start_year,
    end_year = end_year,
    facility_filter = facility_filter,
    priority_filter = priority_filter,  # Now includes priority filter!
    zone_filter = zone_filter,
    larvae_threshold = 2  # Default threshold for consistency
  )
  
  return(comprehensive_data$treatment_volumes)
}

# Create weekly treatment volume summary
create_weekly_treatment_summary <- function(volume_data) {
  if (nrow(volume_data) == 0) {
    return(data.frame(
      week_start_date = as.Date(character(0)),
      year = integer(0),
      week_of_year = integer(0),
      treatment_operations = integer(0),
      treatment_acres = numeric(0),
      inspection_operations = integer(0),
      inspection_acres = numeric(0)
    ))
  }
  
  # Aggregate by week
  weekly_summary <- volume_data %>%
    group_by(week_start_date, year, week_of_year) %>%
    summarise(
      treatment_operations = sum(total_operations[operation_type == 'treatment'], na.rm = TRUE),
      treatment_acres = sum(total_acres[operation_type == 'treatment'], na.rm = TRUE),
      inspection_operations = sum(total_operations[operation_type == 'inspection'], na.rm = TRUE),
      inspection_acres = sum(total_acres[operation_type == 'inspection'], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(week_start_date)
  
  return(weekly_summary)
}

# Create yearly treatment volume summary
create_yearly_treatment_summary <- function(volume_data) {
  if (nrow(volume_data) == 0) {
    return(data.frame(
      year = integer(0),
      treatment_operations = integer(0),
      treatment_acres = numeric(0),
      inspection_operations = integer(0),
      inspection_acres = numeric(0)
    ))
  }
  
  # Aggregate by year
  yearly_summary <- volume_data %>%
    group_by(year) %>%
    summarise(
      treatment_operations = sum(total_operations[operation_type == 'treatment'], na.rm = TRUE),
      treatment_acres = sum(total_acres[operation_type == 'treatment'], na.rm = TRUE),
      inspection_operations = sum(total_operations[operation_type == 'inspection'], na.rm = TRUE),
      inspection_acres = sum(total_acres[operation_type == 'inspection'], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(year)
  
  return(yearly_summary)
}

# Create treatment volume chart
create_treatment_volume_chart <- function(volume_data, time_period = "weekly") {
  if (nrow(volume_data) == 0) {
    return(plot_ly() %>%
      add_annotations(
        text = "No treatment volume data available",
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE
      ))
  }
  
  if (time_period == "weekly") {
    chart_data <- create_weekly_treatment_summary(volume_data)
    x_var <- ~week_start_date
    x_title <- "Week"
  } else {
    chart_data <- create_yearly_treatment_summary(volume_data)
    x_var <- ~year
    x_title <- "Year"
  }
  
  if (nrow(chart_data) == 0) {
    return(plot_ly() %>%
      add_annotations(
        text = "No data available for selected time period",
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE
      ))
  }
  
  # Create dual-axis chart: bars for operations, line for acres
  p <- plot_ly(chart_data) %>%
    
    # Add bar chart for treatment operations
    add_bars(
      x = x_var,
      y = ~treatment_operations,
      name = "Treatments",
      marker = list(color = "#2ecc71"),
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Treatments: %{y}<br>",
        "Treatment Acres: ", round(chart_data$treatment_acres, 1), "<br>",
        "<extra></extra>"
      )
    ) %>%
    
    # Add bar chart for inspection operations
    add_bars(
      x = x_var,
      y = ~inspection_operations,
      name = "Inspections",
      marker = list(color = "#3498db"),
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Inspections: %{y}<br>",
        "Inspection Acres: ", round(chart_data$inspection_acres, 1), "<br>",
        "<extra></extra>"
      )
    ) %>%
    
    # Add line chart for treatment acres
    add_lines(
      x = x_var,
      y = ~treatment_acres,
      yaxis = "y2",
      name = "Treatment Acres",
      line = list(color = "#e74c3c", width = 3),
      marker = list(color = "#e74c3c", size = 6),
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Treatment Acres: %{y}<br>",
        "<extra></extra>"
      )
    ) %>%
    
    # Configure layout with dual y-axes
    layout(
      title = list(
        text = paste0("Treatment and Inspection Volume (", stringr::str_to_title(time_period), ")"),
        font = list(size = 20, color = "#2c3e50")
      ),
      xaxis = list(
        title = list(text = x_title, font = list(size = 18)),
        tickfont = list(size = 16),
        tickangle = if(time_period == "weekly" && nrow(chart_data) > 8) -45 else 0
      ),
      yaxis = list(
        title = list(text = "Number of Operations", font = list(size = 18)),
        tickfont = list(size = 16),
        side = "left",
        color = "#3498db"
      ),
      yaxis2 = list(
        title = list(text = "Acres Treated", font = list(size = 18)),
        tickfont = list(size = 16),
        side = "right",
        overlaying = "y",
        color = "#e74c3c"
      ),
      hovermode = "x unified",
      legend = list(
        x = 0.02, y = 0.98,
        bgcolor = "rgba(255,255,255,0.8)",
        bordercolor = "rgba(0,0,0,0.2)",
        borderwidth = 1,
        font = list(size = 16)
      ),
      margin = list(t = 60, r = 80, b = 60, l = 60),
      barmode = 'group'
    )
  
  return(p)
}