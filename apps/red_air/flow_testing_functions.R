# Flow Testing Functions for Red Air Pipeline App
# Functions to handle the flow testing and validation functionality

# Function to get flow test data
get_flow_test_data <- function(start_date, end_date, data_type, synth_params = NULL) {
  if (data_type == "synthetic") {
    return(generate_synthetic_test_data(synth_params))
  }
  
  # Real database data
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Get real flow data for the date range
    query <- sprintf("
      WITH DateRange AS (
        SELECT generate_series('%s'::date, '%s'::date, '1 day'::interval) as test_date
      ),
      
      ActiveSites AS (
        SELECT DISTINCT
          b.sitecode,
          b.facility,
          b.priority
        FROM loc_breeding_sites b
        WHERE (b.enddate IS NULL OR b.enddate > '%s')
          AND b.air_gnd = 'A'
      ),
      
      DailyRainfall AS (
        SELECT 
          dr.test_date,
          a.sitecode,
          a.facility,
          a.priority,
          COALESCE(r.rainfall, 0) as rainfall
        FROM DateRange dr
        CROSS JOIN ActiveSites a
        LEFT JOIN public.rainfall r ON r.sitecode = a.sitecode 
          AND r.rdate = dr.test_date::date
      ),
      
      DailyInspections AS (
        SELECT 
          i.inspdate::date as test_date,
          i.sitecode,
          i.mosqcount,
          'inspection' as event_type
        FROM (
          SELECT sitecode, inspdate, mosqcount FROM public.dblarv_insptrt_archive WHERE action IN ('9', '8')
          UNION ALL
          SELECT sitecode, inspdate, mosqcount FROM public.dblarv_insptrt_current WHERE action IN ('9', '8')
        ) i
        WHERE i.inspdate::date BETWEEN '%s' AND '%s'
      ),
      
      DailyTreatments AS (
        SELECT 
          t.inspdate::date as test_date,
          t.sitecode,
          'treatment' as event_type
        FROM (
          SELECT sitecode, inspdate FROM public.dblarv_insptrt_archive WHERE action IN ('3', '4', '5', '6', '7')
          UNION ALL
          SELECT sitecode, inspdate FROM public.dblarv_insptrt_current WHERE action IN ('3', '4', '5', '6', '7')
        ) t
        WHERE t.inspdate::date BETWEEN '%s' AND '%s'
      )
      
      SELECT 
        dr.test_date,
        dr.sitecode,
        dr.facility,
        dr.priority,
        dr.rainfall,
        CASE WHEN di.sitecode IS NOT NULL THEN 1 ELSE 0 END as was_inspected,
        COALESCE(di.mosqcount, 0) as larvae_count,
        CASE WHEN dt.sitecode IS NOT NULL THEN 1 ELSE 0 END as was_treated,
        CASE 
          WHEN dr.rainfall < 1.0 THEN 'Under Threshold'
          WHEN di.sitecode IS NULL THEN 'Needs Inspection' 
          WHEN COALESCE(di.mosqcount, 0) < 1 THEN 'Under Threshold'
          WHEN dt.sitecode IS NULL THEN 'Needs Treatment'
          ELSE 'Treated'
        END as status
      FROM DailyRainfall dr
      LEFT JOIN DailyInspections di ON dr.test_date = di.test_date AND dr.sitecode = di.sitecode
      LEFT JOIN DailyTreatments dt ON dr.test_date = dt.test_date AND dr.sitecode = dt.sitecode
      WHERE dr.rainfall > 0 OR di.sitecode IS NOT NULL OR dt.sitecode IS NOT NULL
      ORDER BY dr.test_date, dr.sitecode
    ", start_date, end_date, end_date, start_date, end_date, start_date, end_date)
    
    result <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    return(result)
    
  }, error = function(e) {
    if (exists("con") && !is.null(con)) {
      dbDisconnect(con)
    }
    warning(paste("Error getting flow test data:", e$message))
    return(data.frame())
  })
}

# Function to generate synthetic test data
generate_synthetic_test_data <- function(synth_params) {
  if (is.null(synth_params)) {
    # Default parameters
    total_sites <- 100
    rain_pct <- 60
    inspect_pct <- 80
    above_thresh_pct <- 30
  } else {
    total_sites <- synth_params$total_sites
    rain_pct <- synth_params$rain_pct
    inspect_pct <- synth_params$inspect_pct
    above_thresh_pct <- synth_params$above_thresh_pct
  }
  
  # Generate test dates (last 7 days)
  test_dates <- seq(from = Sys.Date() - 6, to = Sys.Date(), by = "day")
  
  # Generate synthetic sites
  sites <- paste0("TEST", sprintf("%03d", 1:total_sites))
  facilities <- sample(c("BLAINE", "RAMSEY", "ANOKA", "METRO"), total_sites, replace = TRUE)
  priorities <- sample(c("RED", "YELLOW", "GREEN"), total_sites, replace = TRUE, prob = c(0.3, 0.5, 0.2))
  
  # Create synthetic data
  synthetic_data <- expand.grid(
    test_date = test_dates,
    sitecode = sites,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      facility = rep(facilities, each = length(test_dates)),
      priority = rep(priorities, each = length(test_dates)),
      # Simulate rainfall
      rainfall = ifelse(runif(n()) < (rain_pct/100), runif(n(), 0.5, 3.0), 0),
      # Simulate inspections (only for sites with rain)
      was_inspected = ifelse(rainfall > 0, 
                            ifelse(runif(n()) < (inspect_pct/100), 1, 0), 0),
      # Simulate larvae counts
      larvae_count = ifelse(was_inspected == 1,
                           ifelse(runif(n()) < (above_thresh_pct/100), 
                                 sample(1:20, n(), replace = TRUE), 0), 0),
      # Simulate treatments (for sites above threshold)
      was_treated = ifelse(larvae_count > 0, 
                          ifelse(runif(n()) < 0.9, 1, 0), 0),
      # Calculate status
      status = case_when(
        rainfall < 1.0 ~ "Under Threshold",
        was_inspected == 0 ~ "Needs Inspection",
        larvae_count == 0 ~ "Under Threshold", 
        was_treated == 0 ~ "Needs Treatment",
        TRUE ~ "Treated"
      )
    ) %>%
    filter(rainfall > 0 | was_inspected == 1 | was_treated == 1)  # Only include relevant records
  
  return(synthetic_data)
}

# Function to create daily counts chart
create_daily_counts_chart <- function(flow_data) {
  if (nrow(flow_data) == 0) {
    return(plot_ly() %>% layout(title = "No data available"))
  }
  
  # Aggregate by date and status
  daily_counts <- flow_data %>%
    group_by(test_date, status) %>%
    summarise(count = n(), .groups = "drop")
  
  # Get colors
  source_colors <- get_status_colors()
  
  plot_ly(daily_counts, x = ~test_date, y = ~count, color = ~status, type = "bar") %>%
    layout(title = "Daily Status Counts",
           xaxis = list(title = "Date"),
           yaxis = list(title = "Number of Sites"),
           barmode = "stack")
}

# Function to create flow summary
create_flow_summary <- function(flow_data) {
  if (nrow(flow_data) == 0) {
    return("No data available for analysis.")
  }
  
  # Calculate summary statistics
  total_events <- nrow(flow_data)
  sites_with_rain <- sum(flow_data$rainfall > 0)
  sites_inspected <- sum(flow_data$was_inspected)
  sites_above_threshold <- sum(flow_data$larvae_count > 0)
  sites_treated <- sum(flow_data$was_treated)
  
  inspection_rate <- if(sites_with_rain > 0) round((sites_inspected / sites_with_rain) * 100, 1) else 0
  treatment_rate <- if(sites_above_threshold > 0) round((sites_treated / sites_above_threshold) * 100, 1) else 0
  
  # Status breakdown
  status_summary <- flow_data %>%
    group_by(status) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(percentage = round((count / total_events) * 100, 1))
  
  summary_text <- paste(
    "FLOW SUMMARY",
    "=============",
    sprintf("Total Events: %d", total_events),
    sprintf("Sites with Rain: %d", sites_with_rain),
    sprintf("Sites Inspected: %d (%s%% of rain sites)", sites_inspected, inspection_rate),
    sprintf("Sites Above Threshold: %d", sites_above_threshold),
    sprintf("Sites Treated: %d (%s%% of above threshold)", sites_treated, treatment_rate),
    "",
    "STATUS BREAKDOWN:",
    paste(sprintf("%s: %d (%s%%)", status_summary$status, status_summary$count, status_summary$percentage), collapse = "\n"),
    sep = "\n"
  )
  
  return(summary_text)
}

# Function to create validation summary
create_validation_summary <- function(flow_data) {
  if (nrow(flow_data) == 0) {
    return("No data available for validation.")
  }
  
  # Validation checks
  validation_issues <- c()
  
  # Check for sites with rainfall but no inspection
  sites_rain_no_inspect <- flow_data %>%
    filter(rainfall > 1.0, was_inspected == 0) %>%
    nrow()
  
  if (sites_rain_no_inspect > 0) {
    validation_issues <- c(validation_issues, 
                          sprintf("⚠ %d sites with significant rainfall (>1 inch) but no inspection", sites_rain_no_inspect))
  }
  
  # Check for sites above threshold but not treated
  sites_above_no_treat <- flow_data %>%
    filter(larvae_count > 0, was_treated == 0) %>%
    nrow()
  
  if (sites_above_no_treat > 0) {
    validation_issues <- c(validation_issues,
                          sprintf("⚠ %d sites above treatment threshold but not treated", sites_above_no_treat))
  }
  
  # Check for treatments without inspections
  sites_treat_no_inspect <- flow_data %>%
    filter(was_treated == 1, was_inspected == 0) %>%
    nrow()
  
  if (sites_treat_no_inspect > 0) {
    validation_issues <- c(validation_issues,
                          sprintf("⚠ %d sites treated without inspection record", sites_treat_no_inspect))
  }
  
  # Check data quality
  na_rainfall <- sum(is.na(flow_data$rainfall))
  if (na_rainfall > 0) {
    validation_issues <- c(validation_issues,
                          sprintf("⚠ %d records with missing rainfall data", na_rainfall))
  }
  
  if (length(validation_issues) == 0) {
    validation_text <- "✓ DATA VALIDATION PASSED\n\nNo issues found in the flow logic."
  } else {
    validation_text <- paste(
      "DATA VALIDATION ISSUES",
      "======================",
      paste(validation_issues, collapse = "\n"),
      sep = "\n"
    )
  }
  
  return(validation_text)
}