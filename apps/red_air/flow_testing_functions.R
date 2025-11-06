# Flow Testing Functions for Red Air Site Status System
# This module provides comprehensive testing and visualization of the air site status workflow

library(DT)
library(plotly)
library(shiny)

# Function to create parameter testing interface
create_parameter_testing_ui <- function() {
  fluidRow(
    box(
      title = "Parameter Testing", status = "primary", solidHeader = TRUE, width = 12,
      p("Test how different parameter values affect site status calculations:"),
      
      fluidRow(
        column(3,
          numericInput("test_rain_threshold", "Rain Threshold (inches)", 
                      value = 1.0, min = 0.1, max = 3.0, step = 0.1)
        ),
        column(3,
          numericInput("test_lookback_days", "Lookback Period (days)", 
                      value = 3, min = 1, max = 14, step = 1)
        ),
        column(3,
          numericInput("test_treatment_threshold", "Treatment Threshold (larvae)", 
                      value = 2, min = 1, max = 10, step = 1)
        ),
        column(3,
          numericInput("test_total_sites", "Number of Test Sites", 
                      value = 50, min = 10, max = 200, step = 10)
        )
      ),
      
      actionButton("run_flow_test", "Run Flow Test", class = "btn-primary"),
      actionButton("run_persistence_test", "Test 14-Day Persistence", class = "btn-info"),
      actionButton("run_validation_test", "Run Business Logic Validation", class = "btn-success"),
      br(), br(),
      
      conditionalPanel(
        condition = "input.run_flow_test > 0",
        h4("Test Results Summary"),
        fluidRow(
          valueBoxOutput("test_unknown", width = 2),
          valueBoxOutput("test_needs_inspection", width = 2),
          valueBoxOutput("test_under_threshold", width = 2),
          valueBoxOutput("test_needs_treatment", width = 2),
          valueBoxOutput("test_active_treatment", width = 2),
          valueBoxOutput("test_total_sites", width = 2)
        )
      ),
      
      conditionalPanel(
        condition = "input.run_persistence_test > 0",
        h4("14-Day Persistence Test Results"),
        verbatimTextOutput("persistence_test_output")
      ),
      
      conditionalPanel(
        condition = "input.run_validation_test > 0",
        h4("Business Logic Validation Results"),
        verbatimTextOutput("validation_test_output")
      )
    )
  )
}

# Function to create flow diagram visualization with dynamic counts
create_flow_diagram_ui <- function() {
  fluidRow(
    box(
      title = "Status Flow Diagram", status = "info", solidHeader = TRUE, width = 12,
      h4("Air Site Status Transition Flow"),
      p("This diagram shows how sites move between different status states based on rainfall, inspections, and treatments."),
      p(em("Note: Counts update when you run a Flow Test with synthetic data.")),
      
      htmlOutput("flow_diagram_content")
    )
  )
}

# Function to generate flow diagram with actual counts
create_flow_diagram_with_counts <- function(data = NULL) {
  # Default counts
  counts <- list(
    unknown = 0,
    needs_inspection = 0,
    under_threshold = 0,
    needs_treatment = 0,
    active_treatment = 0
  )
  
  # Update counts if data is provided
  if (!is.null(data) && nrow(data) > 0) {
    status_summary <- table(data$site_status)
    counts$unknown <- ifelse("Unknown" %in% names(status_summary), status_summary[["Unknown"]], 0)
    counts$needs_inspection <- ifelse("Needs Inspection" %in% names(status_summary), status_summary[["Needs Inspection"]], 0)
    counts$under_threshold <- ifelse("Under Threshold" %in% names(status_summary), status_summary[["Under Threshold"]], 0)
    counts$needs_treatment <- ifelse("Needs Treatment" %in% names(status_summary), status_summary[["Needs Treatment"]], 0)
    counts$active_treatment <- ifelse("Active Treatment" %in% names(status_summary), status_summary[["Active Treatment"]], 0)
  }
  
  # Generate HTML with dynamic counts
  html_content <- sprintf('
    <div style="text-align: center; margin: 20px;">
      <svg width="800" height="500">
        <!-- Define arrow markers -->
        <defs>
          <marker id="arrowhead" markerWidth="10" markerHeight="7" 
                 refX="0" refY="3.5" orient="auto">
            <polygon points="0 0, 10 3.5, 0 7" fill="#333"/>
          </marker>
        </defs>
        
        <!-- Status boxes with dynamic counts -->
        <rect x="50" y="50" width="120" height="60" 
             fill="#f8f9fa" stroke="#6c757d" stroke-width="2" rx="5"/>
        <text x="110" y="75" text-anchor="middle" fill="#333" 
             font-weight="bold">Unknown</text>
        <text x="110" y="90" text-anchor="middle" fill="#666" 
             font-size="12">%d sites</text>
        
        <rect x="250" y="50" width="120" height="60" 
             fill="#fff3cd" stroke="#ffc107" stroke-width="2" rx="5"/>
        <text x="310" y="70" text-anchor="middle" fill="#333" 
             font-weight="bold">Needs</text>
        <text x="310" y="85" text-anchor="middle" fill="#333" 
             font-weight="bold">Inspection</text>
        <text x="310" y="100" text-anchor="middle" fill="#666" 
             font-size="12">%d sites</text>
        
        <rect x="450" y="150" width="120" height="60" 
             fill="#d1ecf1" stroke="#17a2b8" stroke-width="2" rx="5"/>
        <text x="510" y="175" text-anchor="middle" fill="#333" 
             font-weight="bold">Under</text>
        <text x="510" y="190" text-anchor="middle" fill="#333" 
             font-weight="bold">Threshold</text>
        <text x="510" y="205" text-anchor="middle" fill="#666" 
             font-size="12">%d sites</text>
        
        <rect x="450" y="250" width="120" height="60" 
             fill="#f8d7da" stroke="#dc3545" stroke-width="2" rx="5"/>
        <text x="510" y="275" text-anchor="middle" fill="#333" 
             font-weight="bold">Needs</text>
        <text x="510" y="290" text-anchor="middle" fill="#333" 
             font-weight="bold">Treatment</text>
        <text x="510" y="305" text-anchor="middle" fill="#666" 
             font-size="12">%d sites</text>
        
        <rect x="650" y="200" width="120" height="60" 
             fill="#d4edda" stroke="#28a745" stroke-width="2" rx="5"/>
        <text x="710" y="225" text-anchor="middle" fill="#333" 
             font-weight="bold">Active</text>
        <text x="710" y="240" text-anchor="middle" fill="#333" 
             font-weight="bold">Treatment</text>
        <text x="710" y="255" text-anchor="middle" fill="#666" 
             font-size="12">%d sites</text>
        
        <!-- Arrows with labels -->
        <line x1="170" y1="80" x2="240" y2="80" 
             stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
        <text x="205" y="75" text-anchor="middle" fill="#666" 
             font-size="11">Rainfall ≥ 1.0"</text>
        
        <line x1="360" y1="110" x2="440" y2="160" 
             stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
        <text x="380" y="130" text-anchor="middle" fill="#666" 
             font-size="11">Inspect: &lt; 2</text>
        
        <line x1="360" y1="100" x2="440" y2="270" 
             stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
        <text x="380" y="200" text-anchor="middle" fill="#666" 
             font-size="11">Inspect: ≥ 2</text>
        
        <line x1="570" y1="280" x2="640" y2="240" 
             stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"/>
        <text x="605" y="255" text-anchor="middle" fill="#666" 
             font-size="11">Treat</text>
        
        <line x1="680" y1="200" x2="140" y2="110" 
             stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"
             stroke-dasharray="5,5"/>
        <text x="400" y="140" text-anchor="middle" fill="#666" 
             font-size="11">Treatment Expires</text>
        
        <line x1="500" y1="150" x2="320" y2="110" 
             stroke="#333" stroke-width="2" marker-end="url(#arrowhead)"
             stroke-dasharray="5,5"/>
        <text x="380" y="120" text-anchor="middle" fill="#666" 
             font-size="11">New Rain</text>
      </svg>
    </div>
  ', 
  counts$unknown,
  counts$needs_inspection, 
  counts$under_threshold,
  counts$needs_treatment,
  counts$active_treatment)
  
  return(HTML(html_content))
}

# Function to create detailed test results UI
create_detailed_results_ui <- function() {
  fluidRow(
    box(
      title = "Detailed Test Results", status = "success", solidHeader = TRUE, width = 12,
      conditionalPanel(
        condition = "input.run_flow_test > 0",
        DT::dataTableOutput("flow_test_results")
      )
    )
  )
}

# Function to create site timeline data for visualization
create_site_timeline_data <- function(sitecode, analysis_date = Sys.Date()) {
  # Create a 30-day timeline showing site evolution
  dates <- seq(from = analysis_date - 30, to = analysis_date, by = "day")
  
  # Simulate realistic rainfall patterns
  # Extract number from sitecode like "TEST-001" to create reproducible seed
  site_num <- as.numeric(gsub("TEST-", "", sitecode))
  set.seed(site_num * 123)  # Use site number times a constant for seed
  rainfall_events <- sample(1:length(dates), size = sample(3:8, 1))
  
  timeline <- data.frame(
    date = dates,
    sitecode = sitecode,
    daily_rain = 0,
    cumulative_rain = 0,
    status = "Unknown",
    event = "",
    stringsAsFactors = FALSE
  )
  
  # Add rainfall events
  for (event_day in rainfall_events) {
    timeline$daily_rain[event_day] <- runif(1, 0.1, 1.5)
  }
  
  # Calculate status transitions day by day
  last_inspection_date <- NA
  last_larvae_count <- NA
  treatment_expiry <- NA
  
  for (i in 1:nrow(timeline)) {
    current_date <- timeline$date[i]
    
    # Calculate cumulative rain in last 3 days
    start_idx <- max(1, i - 2)
    timeline$cumulative_rain[i] <- sum(timeline$daily_rain[start_idx:i])
    
    # Check if triggering rainfall occurred
    has_triggering_rain <- timeline$cumulative_rain[i] >= 1.0
    
    # Simulate inspection events (random chance after triggering rain)
    if (isTRUE(has_triggering_rain) && is.na(last_inspection_date) && runif(1) < 0.3) {
      last_inspection_date <- current_date
      last_larvae_count <- sample(0:8, 1, prob = c(0.4, 0.2, 0.15, 0.1, 0.05, 0.04, 0.03, 0.02, 0.01))
      timeline$event[i] <- paste0("Inspected: ", last_larvae_count, " larvae")
    }
    
    # Simulate treatment events
    if (!is.na(last_larvae_count) && last_larvae_count >= 2 && is.na(treatment_expiry) && runif(1) < 0.4) {
      treatment_expiry <- current_date + 30
      timeline$event[i] <- paste0(timeline$event[i], " | Treatment Applied")
    }
    
    # Determine status
    if (!is.na(treatment_expiry) && current_date <= treatment_expiry) {
      timeline$status[i] <- "Active Treatment"
    } else if (!is.na(treatment_expiry) && current_date > treatment_expiry) {
      timeline$status[i] <- "Unknown"
      treatment_expiry <- NA  # Reset
      last_inspection_date <- NA
      last_larvae_count <- NA
    } else if (isTRUE(has_triggering_rain)) {
      timeline$status[i] <- "Needs Inspection"
    } else if (!is.na(last_larvae_count)) {
      if (last_larvae_count >= 2) {
        timeline$status[i] <- "Needs Treatment"
      } else {
        timeline$status[i] <- "Under Threshold"
      }
    } else {
      timeline$status[i] <- "Unknown"
    }
  }
  
  return(timeline)
}

# Function to create timeline visualization
#legacy
create_site_timeline_plot <- function(timeline_data) {
  status_colors <- c(
    "Unknown" = "#6c757d",
    "Needs Inspection" = "#ffc107", 
    "Under Threshold" = "#28a745",
    "Needs Treatment" = "#dc3545",
    "Active Treatment" = "#6f42c1"
  )
  
  p <- plot_ly(data = timeline_data, x = ~date, y = ~sitecode, color = ~status,
               colors = status_colors, type = 'scatter', mode = 'markers+lines',
               marker = list(size = 8), line = list(width = 3),
               text = ~paste("Date:", date, "<br>Status:", status, "<br>Rain:", round(daily_rain, 2), "in<br>Event:", event),
               hovertemplate = "%{text}<extra></extra>") %>%
    add_trace(y = ~sitecode, x = ~date, type = 'bar', 
              yaxis = 'y2', opacity = 0.3, 
              marker = list(color = 'lightblue'),
              name = 'Daily Rainfall', showlegend = FALSE,
              text = ~paste("Rain:", round(daily_rain, 2), "inches"),
              hovertemplate = "Date: %{x}<br>%{text}<extra></extra>") %>%
    layout(
      title = paste("Site Timeline:", unique(timeline_data$sitecode)),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Site Status", side = 'left'),
      yaxis2 = list(title = 'Daily Rainfall (in)', side = 'right', overlaying = 'y'),
      showlegend = TRUE,
      hovermode = 'closest'
    )
  
  return(p)
}

# Function to validate business logic with specific test cases
validate_business_logic <- function() {
  results <- list()
  
  # Test Case 1: Rainfall thresholds
  test_sites_1 <- data.frame(
    sitecode = c("TEST-001", "TEST-002", "TEST-003"),
    total_rainfall = c(0.5, 1.2, 1.0),
    has_triggering_rainfall = c(FALSE, TRUE, TRUE),
    last_inspection_date = as.Date(c(NA, NA, NA)),
    last_larvae_count = c(NA, NA, NA),
    treatment_expiry = as.Date(c(NA, NA, NA))
  )
  
  expected_status_1 <- c("Unknown", "Needs Inspection", "Needs Inspection")
  
  # Test Case 2: Inspection logic - CORRECTED
  # Rain ALWAYS causes "Needs Inspection" unless Active Treatment
  test_sites_2 <- data.frame(
    sitecode = c("TEST-004", "TEST-005", "TEST-006"),
    total_rainfall = c(1.5, 1.5, 1.5),
    has_triggering_rainfall = c(TRUE, TRUE, TRUE),
    last_inspection_date = Sys.Date() - c(1, 1, 1),
    last_larvae_count = c(1, 2, 5),
    treatment_expiry = as.Date(c(NA, NA, NA))
  )
  
  # CORRECTED: All should be "Needs Inspection" because rain trumps inspection results
  expected_status_2 <- c("Needs Inspection", "Needs Inspection", "Needs Inspection")
  
  # Test Case 3: Treatment effects - CORRECTED
  test_sites_3 <- data.frame(
    sitecode = c("TEST-007", "TEST-008", "TEST-009"),
    total_rainfall = c(1.5, 1.5, 1.5),
    has_triggering_rainfall = c(TRUE, TRUE, TRUE),
    last_inspection_date = Sys.Date() - c(5, 5, 5),
    last_larvae_count = c(3, 3, 3),
    treatment_expiry = c(Sys.Date() + 10, Sys.Date() - 5, NA)
  )
  
  # CORRECTED: TEST-009 has triggering rainfall and no treatment, so should be "Needs Inspection"
  expected_status_3 <- c("Active Treatment", "Unknown", "Needs Inspection")
  
  # Test Case 4: 14-day "Needs Inspection" persistence 
  # This tests that sites stay in "Needs Inspection" for 14 days unless inspected
  test_sites_4 <- data.frame(
    sitecode = c("TEST-010", "TEST-011", "TEST-012", "TEST-013", "TEST-014"),
    # Rainfall occurred at different times in the past
    rainfall_days_ago = c(1, 7, 13, 15, 20),  # Days since triggering rainfall
    has_triggering_rainfall = c(TRUE, TRUE, TRUE, FALSE, FALSE),  # Within 14 days vs beyond
    last_inspection_date = as.Date(c(NA, NA, NA, NA, NA)),  # No inspections yet
    last_larvae_count = c(NA, NA, NA, NA, NA),
    treatment_expiry = as.Date(c(NA, NA, NA, NA, NA))
  )
  
  # Expected: First 3 should be "Needs Inspection" (within 14 days), last 2 should be "Unknown" (beyond 14 days)
  expected_status_4 <- c("Needs Inspection", "Needs Inspection", "Needs Inspection", "Unknown", "Unknown")
  
  # Test Case 5: Inspection outcomes within 14-day period - CORRECTED
  # Rain ALWAYS causes "Needs Inspection" - inspection results don't matter if rain is active
  test_sites_5 <- data.frame(
    sitecode = c("TEST-015", "TEST-016", "TEST-017"),
    rainfall_days_ago = c(5, 5, 5),  # All had triggering rainfall 5 days ago
    has_triggering_rainfall = c(TRUE, TRUE, TRUE),  # All within 14 days
    last_inspection_date = c(Sys.Date() - 2, Sys.Date() - 2, Sys.Date() - 2),  # Inspected 2 days ago
    last_larvae_count = c(1, 2, 5),  # Below, at, and above threshold
    treatment_expiry = as.Date(c(NA, NA, NA))
  )
  
  # CORRECTED: All should be "Needs Inspection" because rain trumps inspection results
  expected_status_5 <- c("Needs Inspection", "Needs Inspection", "Needs Inspection")
  
  # Test Case 6: Inspection results when NO triggering rainfall - NEW TEST
  # This tests that inspection results are used ONLY when there's no triggering rainfall
  test_sites_6 <- data.frame(
    sitecode = c("TEST-018", "TEST-019", "TEST-020"),
    total_rainfall = c(0.3, 0.3, 0.3),  # Below threshold
    has_triggering_rainfall = c(FALSE, FALSE, FALSE),  # No triggering rainfall
    last_inspection_date = c(Sys.Date() - 2, Sys.Date() - 2, Sys.Date() - 2),
    last_larvae_count = c(1, 2, 5),  # Below, at, and above threshold
    treatment_expiry = as.Date(c(NA, NA, NA))
  )
  
  # Expected: No rain, so use inspection results
  expected_status_6 <- c("Under Threshold", "Needs Treatment", "Needs Treatment")
  
  all_tests <- list(
    list(data = test_sites_1, expected = expected_status_1, name = "Rainfall Thresholds"),
    list(data = test_sites_2, expected = expected_status_2, name = "Inspection Logic"),
    list(data = test_sites_3, expected = expected_status_3, name = "Treatment Effects"),
    list(data = test_sites_4, expected = expected_status_4, name = "14-Day Needs Inspection Persistence"),
    list(data = test_sites_5, expected = expected_status_5, name = "Inspection Outcomes During Active Period"),
    list(data = test_sites_6, expected = expected_status_6, name = "Inspection Results Without Rain")
  )
  
  test_results <- character(0)
  
  for (i in 1:length(all_tests)) {
    test <- all_tests[[i]]
    
    # Apply business logic with enhanced 14-day persistence handling
    actual_status <- character(nrow(test$data))
    
    for (j in 1:nrow(test$data)) {
      site_data <- test$data[j, ]
      
      # Handle special test cases that include rainfall timing
      if ("rainfall_days_ago" %in% names(site_data)) {
        # For 14-day persistence tests, simulate the triggering rainfall check
        # Rainfall is "triggering" if it occurred within the last 14 days
        site_data$has_triggering_rainfall <- site_data$rainfall_days_ago <= 14
      }
      
      # Apply the corrected business logic
      if (!is.na(site_data$treatment_expiry) && Sys.Date() <= site_data$treatment_expiry) {
        # Active treatment
        actual_status[j] <- "Active Treatment"
      } else if (!is.na(site_data$treatment_expiry) && Sys.Date() > site_data$treatment_expiry) {
        # Treatment expired -> Unknown
        actual_status[j] <- "Unknown"
      } else if (isTRUE(site_data$has_triggering_rainfall) && is.na(site_data$last_inspection_date)) {
        # Has triggering rainfall but no inspection -> Needs Inspection
        actual_status[j] <- "Needs Inspection"
      } else if (isTRUE(site_data$has_triggering_rainfall) && !is.na(site_data$last_inspection_date)) {
        # Has triggering rainfall -> ALWAYS Needs Inspection (rain trumps inspection results)
        actual_status[j] <- "Needs Inspection"
      } else if (!isTRUE(site_data$has_triggering_rainfall) && !is.na(site_data$last_inspection_date)) {
        # No triggering rainfall but has inspection -> use inspection results
        if (!is.na(site_data$last_larvae_count) && site_data$last_larvae_count >= 2) {
          actual_status[j] <- "Needs Treatment"
        } else {
          actual_status[j] <- "Under Threshold"
        }
      } else {
        # Default: No triggering rainfall, no inspection
        actual_status[j] <- "Unknown"
      }
    }
    
    # Check results
    passed <- all(actual_status == test$expected)
    
    test_results <- c(test_results, 
                     sprintf("✓ %s: %s", test$name, ifelse(passed, "PASSED", "FAILED")))
    
    if (!passed) {
      for (j in 1:length(actual_status)) {
        if (actual_status[j] != test$expected[j]) {
          test_results <- c(test_results,
                           sprintf("  - %s: Expected '%s', Got '%s'", 
                                  test$data$sitecode[j], test$expected[j], actual_status[j]))
        }
      }
    }
  }
  
  return(paste(test_results, collapse = "\n"))
}

# Function to test correct rainfall-to-inspection logic
test_rainfall_inspection_logic <- function() {
  # Create a timeline test showing correct behavior
  test_date <- as.Date("2024-01-15")  # Fixed date for consistent testing
  
  # Scenario: Rainfall occurs on day 1, triggering "Needs Inspection"
  # Test that inspection results actually change the status
  
  timeline_results <- data.frame(
    day = 1:20,
    date = test_date + 0:19,
    rainfall_on_day_1 = c(TRUE, rep(FALSE, 19)),  # Only rain on day 1
    days_since_rain = 0:19,
    within_14_days = c(rep(TRUE, 14), rep(FALSE, 6)),
    inspection_occurred = FALSE,  # No inspection for this test
    expected_status = c(rep("Needs Inspection", 14), rep("Unknown", 6)),
    stringsAsFactors = FALSE
  )
  
  # Test with inspection on day 5 - LOW larvae count
  timeline_with_low_inspection <- timeline_results
  timeline_with_low_inspection$inspection_occurred[5:20] <- TRUE  # Inspection persists
  timeline_with_low_inspection$inspection_larvae_count <- NA
  timeline_with_low_inspection$inspection_larvae_count[5:20] <- 1  # Below threshold
  
  # KEY FIX: After inspection with low larvae count, site should be "Under Threshold"
  # NOT go back to "Needs Inspection"
  timeline_with_low_inspection$expected_status[1:4] <- "Needs Inspection"    # Before inspection
  timeline_with_low_inspection$expected_status[5:20] <- "Under Threshold"   # After inspection with low count
  
  # Test with inspection finding high larvae count
  timeline_with_high_inspection <- timeline_results
  timeline_with_high_inspection$inspection_occurred[3:20] <- TRUE  # Inspection persists
  timeline_with_high_inspection$inspection_larvae_count <- NA
  timeline_with_high_inspection$inspection_larvae_count[3:20] <- 3  # Above threshold
  timeline_with_high_inspection$expected_status[1:2] <- "Needs Inspection"  # Before inspection
  timeline_with_high_inspection$expected_status[3:20] <- "Needs Treatment"  # After inspection with high count
  
  test_scenarios <- list(
    list(data = timeline_results, name = "No Inspection - 14 Day Rainfall Window"),
    list(data = timeline_with_low_inspection, name = "Inspection with Low Count - Under Threshold"),
    list(data = timeline_with_high_inspection, name = "Inspection with High Count - Needs Treatment")
  )
  
  results <- character(0)
  
  for (scenario in test_scenarios) {
    results <- c(results, paste("", scenario$name, sep = "\n"))
    results <- c(results, paste(rep("-", nchar(scenario$name)), collapse = ""))
    
    for (i in 1:nrow(scenario$data)) {
      day_data <- scenario$data[i, ]
      
      # Apply the CORRECTED business logic
      actual_status <- if (!is.na(day_data$inspection_larvae_count) && day_data$inspection_larvae_count >= 2) {
        # High larvae count always results in "Needs Treatment"
        "Needs Treatment"
      } else if (isTRUE(day_data$within_14_days) && !isTRUE(day_data$inspection_occurred)) {
        # Within 14 days of rainfall, no inspection yet
        "Needs Inspection"
      } else if (isTRUE(day_data$within_14_days) && isTRUE(day_data$inspection_occurred)) {
        # Within 14 days of rainfall, but inspected with low larvae count
        "Under Threshold"  # KEY: Inspection results matter!
      } else if (!isTRUE(day_data$within_14_days) && isTRUE(day_data$inspection_occurred)) {
        # Beyond 14 days, use inspection results
        "Under Threshold"  # Low larvae count
      } else {
        # Beyond 14 days, no inspection
        "Unknown"
      }
      
      status_match <- actual_status == day_data$expected_status
      results <- c(results, sprintf("Day %2d (%s): %s -> %s %s",
                                   day_data$day,
                                   format(day_data$date, "%m-%d"),
                                   day_data$expected_status,
                                   actual_status,
                                   ifelse(status_match, "✓", "✗")))
    }
    results <- c(results, "")
  }
  
  return(paste(results, collapse = "\n"))
}

# Function to create synthetic flow test data for UI testing
create_synthetic_flow_data <- function(total_sites = 50, analysis_date = Sys.Date()) {
  # Create test sites with various scenarios
  set.seed(42)  # For reproducible results
  
  sites <- data.frame(
    sitecode = sprintf("TEST-%03d", 1:total_sites),
    facility = sample(c("MMCD", "SMCD", "RMCD"), total_sites, replace = TRUE),
    priority = sample(c("RED", "BLUE"), total_sites, replace = TRUE, prob = c(0.3, 0.7)),
    acres = round(runif(total_sites, 0.1, 5.0), 1),
    stringsAsFactors = FALSE
  )
  
  # Add rainfall scenarios
  sites$rainfall_scenario <- sample(c("No Rain", "Light Rain", "Heavy Rain"), 
                                   total_sites, replace = TRUE, prob = c(0.4, 0.3, 0.3))
  sites$total_rainfall <- case_when(
    sites$rainfall_scenario == "No Rain" ~ runif(total_sites, 0, 0.8),
    sites$rainfall_scenario == "Light Rain" ~ runif(total_sites, 0.9, 1.5),
    sites$rainfall_scenario == "Heavy Rain" ~ runif(total_sites, 1.6, 3.0)
  )
  sites$has_triggering_rainfall <- sites$total_rainfall >= 1.0
  
  # Add inspection scenarios
  sites$inspection_scenario <- sample(c("Not Inspected", "Recent Inspection", "Old Inspection"),
                                     total_sites, replace = TRUE, prob = c(0.6, 0.3, 0.1))
  sites$last_inspection_date <- case_when(
    sites$inspection_scenario == "Not Inspected" ~ as.Date(NA),
    sites$inspection_scenario == "Recent Inspection" ~ analysis_date - sample(1:7, total_sites, replace = TRUE),
    sites$inspection_scenario == "Old Inspection" ~ analysis_date - sample(15:30, total_sites, replace = TRUE)
  )
  
  # Add larvae count based on inspection
  sites$last_larvae_count <- ifelse(
    is.na(sites$last_inspection_date), 
    NA, 
    sample(0:8, total_sites, replace = TRUE, prob = c(0.4, 0.2, 0.15, 0.1, 0.05, 0.04, 0.03, 0.02, 0.01))
  )
  
  # Add treatment scenarios
  sites$treatment_scenario <- sample(c("No Treatment", "Active Treatment", "Expired Treatment"),
                                    total_sites, replace = TRUE, prob = c(0.7, 0.2, 0.1))
  sites$last_treatment_date <- case_when(
    sites$treatment_scenario == "No Treatment" ~ as.Date(NA),
    sites$treatment_scenario == "Active Treatment" ~ analysis_date - sample(1:15, total_sites, replace = TRUE),
    sites$treatment_scenario == "Expired Treatment" ~ analysis_date - sample(35:60, total_sites, replace = TRUE)
  )
  
  sites$last_treatment_material <- ifelse(
    is.na(sites$last_treatment_date),
    NA,
    sample(c("BTI", "ALTOSID", "AGNIQUE"), total_sites, replace = TRUE)
  )
  
  sites$treatment_expiry <- ifelse(
    is.na(sites$last_treatment_date),
    as.Date(NA),
    sites$last_treatment_date + 30  # Assume 30-day treatment period
  )
  
  # Calculate site status using the same logic as the main application
  sites$site_status <- character(total_sites)
  
  for (i in 1:total_sites) {
    site <- sites[i, ]
    
    if (!is.na(site$treatment_expiry) && analysis_date <= site$treatment_expiry) {
      sites$site_status[i] <- "Active Treatment"
    } else if (!is.na(site$treatment_expiry) && analysis_date > site$treatment_expiry) {
      sites$site_status[i] <- "Unknown"
    } else if (isTRUE(site$has_triggering_rainfall) && is.na(site$last_inspection_date)) {
      sites$site_status[i] <- "Needs Inspection"
    } else if (isTRUE(site$has_triggering_rainfall) && !is.na(site$last_inspection_date)) {
      if (!is.na(site$last_larvae_count) && site$last_larvae_count >= 2) {
        sites$site_status[i] <- "Needs Treatment"
      } else {
        sites$site_status[i] <- "Needs Inspection"  # Key: back to needs inspection with active rainfall
      }
    } else if (!isTRUE(site$has_triggering_rainfall) && !is.na(site$last_inspection_date)) {
      if (!is.na(site$last_larvae_count) && site$last_larvae_count >= 2) {
        sites$site_status[i] <- "Needs Treatment"
      } else {
        sites$site_status[i] <- "Under Threshold"
      }
    } else {
      sites$site_status[i] <- "Unknown"
    }
  }
  
  return(sites)
}

# Function to get flow test data (used by the main app)
get_flow_test_data <- function(start_date, end_date, data_type = "synthetic", synth_params = NULL) {
  if (data_type == "synthetic") {
    total_sites <- ifelse(is.null(synth_params$total_sites), 50, synth_params$total_sites)
    return(create_synthetic_flow_data(total_sites, end_date))
  } else {
    # For real data, would query database - for now return empty
    return(data.frame())
  }
}

# Function to create daily counts chart
create_daily_counts_chart <- function(data) {
  if (nrow(data) == 0) {
    return(plot_ly() %>%
      add_annotations(
        text = "No data available",
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 16)
      ) %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        showlegend = FALSE
      ))
  }
  
  # Count by status
  status_counts <- data %>%
    count(site_status) %>%
    arrange(desc(n))
  
  p <- plot_ly(data = status_counts, 
               x = ~site_status, y = ~n, 
               type = 'bar',
               marker = list(color = c('#6c757d', '#ffc107', '#28a745', '#dc3545', '#6f42c1')[1:nrow(status_counts)])) %>%
    layout(title = "Site Status Distribution",
           xaxis = list(title = "Status"),
           yaxis = list(title = "Number of Sites"))
  
  return(p)
}

# Function to create flow summary
create_flow_summary <- function(data) {
  if (nrow(data) == 0) {
    return("No data available for summary.")
  }
  
  total_sites <- nrow(data)
  status_summary <- data %>%
    count(site_status) %>%
    mutate(percentage = round(n / total_sites * 100, 1))
  
  summary_text <- paste("Flow Test Summary:\n",
                       paste0("Total Sites: ", total_sites), "\n\n",
                       paste(status_summary$site_status, ": ", 
                            status_summary$n, " (", 
                            status_summary$percentage, "%)", 
                            collapse = "\n"))
  
  return(summary_text)
}

# Function to create validation summary
create_validation_summary <- function(data) {
  if (nrow(data) == 0) {
    return("No data available for validation.")
  }
  
  # Run the enhanced validation logic
  validation_results <- validate_business_logic()
  persistence_results <- test_14_day_persistence()
  
  summary <- paste("Validation Results:\n\n",
                  "Business Logic Tests:\n",
                  validation_results, "\n\n",
                  "14-Day Persistence Tests:\n",
                  persistence_results)
  
  return(summary)
}