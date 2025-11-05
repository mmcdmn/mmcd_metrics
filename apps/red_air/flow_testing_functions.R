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
      )
    )
  )
}

# Function to create flow diagram visualization
create_flow_diagram_ui <- function() {
  fluidRow(
    box(
      title = "Status Flow Diagram", status = "info", solidHeader = TRUE, width = 12,
      h4("Air Site Status Transition Flow"),
      p("This diagram shows how sites move between different status states based on rainfall, inspections, and treatments."),
      
      tags$div(style = "text-align: center; margin: 20px;",
        tags$svg(width = "800", height = "500",
          # Define arrow markers
          tags$defs(
            tags$marker(id = "arrowhead", markerWidth = "10", markerHeight = "7", 
                       refX = "0", refY = "3.5", orient = "auto",
              tags$polygon(points = "0 0, 10 3.5, 0 7", fill = "#333")
            )
          ),
          
          # Status boxes
          tags$rect(x = "50", y = "50", width = "120", height = "60", 
                   fill = "#f8f9fa", stroke = "#6c757d", `stroke-width` = "2", rx = "5"),
          tags$text(x = "110", y = "75", `text-anchor` = "middle", fill = "#333", 
                   `font-weight` = "bold", "Unknown"),
          tags$text(x = "110", y = "90", `text-anchor` = "middle", fill = "#666", 
                   `font-size` = "12", id = "unknown-count", "0 sites"),
          
          tags$rect(x = "250", y = "50", width = "120", height = "60", 
                   fill = "#fff3cd", stroke = "#ffc107", `stroke-width` = "2", rx = "5"),
          tags$text(x = "310", y = "70", `text-anchor` = "middle", fill = "#333", 
                   `font-weight` = "bold", "Needs"),
          tags$text(x = "310", y = "85", `text-anchor` = "middle", fill = "#333", 
                   `font-weight` = "bold", "Inspection"),
          tags$text(x = "310", y = "100", `text-anchor` = "middle", fill = "#666", 
                   `font-size` = "12", id = "inspection-count", "0 sites"),
          
          tags$rect(x = "450", y = "150", width = "120", height = "60", 
                   fill = "#d1ecf1", stroke = "#17a2b8", `stroke-width` = "2", rx = "5"),
          tags$text(x = "510", y = "175", `text-anchor` = "middle", fill = "#333", 
                   `font-weight` = "bold", "Under"),
          tags$text(x = "510", y = "190", `text-anchor` = "middle", fill = "#333", 
                   `font-weight` = "bold", "Threshold"),
          tags$text(x = "510", y = "205", `text-anchor` = "middle", fill = "#666", 
                   `font-size` = "12", id = "threshold-count", "0 sites"),
          
          tags$rect(x = "450", y = "250", width = "120", height = "60", 
                   fill = "#f8d7da", stroke = "#dc3545", `stroke-width` = "2", rx = "5"),
          tags$text(x = "510", y = "275", `text-anchor` = "middle", fill = "#333", 
                   `font-weight` = "bold", "Needs"),
          tags$text(x = "510", y = "290", `text-anchor` = "middle", fill = "#333", 
                   `font-weight` = "bold", "Treatment"),
          tags$text(x = "510", y = "305", `text-anchor` = "middle", fill = "#666", 
                   `font-size` = "12", id = "treatment-count", "0 sites"),
          
          tags$rect(x = "650", y = "200", width = "120", height = "60", 
                   fill = "#d4edda", stroke = "#28a745", `stroke-width` = "2", rx = "5"),
          tags$text(x = "710", y = "225", `text-anchor` = "middle", fill = "#333", 
                   `font-weight` = "bold", "Active"),
          tags$text(x = "710", y = "240", `text-anchor` = "middle", fill = "#333", 
                   `font-weight` = "bold", "Treatment"),
          tags$text(x = "710", y = "255", `text-anchor` = "middle", fill = "#666", 
                   `font-size` = "12", id = "active-count", "0 sites"),
          
          # Arrows
          tags$line(x1 = "170", y1 = "80", x2 = "240", y2 = "80", 
                   stroke = "#333", `stroke-width` = "2", `marker-end` = "url(#arrowhead)"),
          tags$text(x = "205", y = "75", `text-anchor` = "middle", fill = "#666", 
                   `font-size` = "11", "Rainfall ≥ 1.0\""),
          
          tags$line(x1 = "360", y1 = "110", x2 = "440", y2 = "160", 
                   stroke = "#333", `stroke-width` = "2", `marker-end` = "url(#arrowhead)"),
          tags$text(x = "380", y = "130", `text-anchor` = "middle", fill = "#666", 
                   `font-size` = "11", "Inspect: < 2"),
          
          tags$line(x1 = "360", y1 = "100", x2 = "440", y2 = "270", 
                   stroke = "#333", `stroke-width` = "2", `marker-end` = "url(#arrowhead)"),
          tags$text(x = "380", y = "200", `text-anchor` = "middle", fill = "#666", 
                   `font-size` = "11", "Inspect: ≥ 2"),
          
          tags$line(x1 = "570", y1 = "280", x2 = "640", y2 = "240", 
                   stroke = "#333", `stroke-width` = "2", `marker-end` = "url(#arrowhead)"),
          tags$text(x = "605", y = "255", `text-anchor` = "middle", fill = "#666", 
                   `font-size` = "11", "Treat"),
          
          tags$line(x1 = "680", y1 = "200", x2 = "140", y2 = "110", 
                   stroke = "#333", `stroke-width` = "2", `marker-end` = "url(#arrowhead)",
                   `stroke-dasharray` = "5,5"),
          tags$text(x = "400", y = "145", `text-anchor` = "middle", fill = "#666", 
                   `font-size` = "11", "Treatment Expires"),
          
          tags$line(x1 = "500", y1 = "150", x2 = "320", y2 = "110", 
                   stroke = "#333", `stroke-width` = "2", `marker-end` = "url(#arrowhead)",
                   `stroke-dasharray` = "3,3"),
          tags$text(x = "410", y = "125", `text-anchor` = "middle", fill = "#666", 
                   `font-size` = "11", "New Rain")
        )
      ),
      
      br(),
      h5("Status Definitions:"),
      tags$ul(
        tags$li(tags$strong("Unknown:"), " Default status or no triggering rainfall detected"),
        tags$li(tags$strong("Needs Inspection:"), " Rainfall threshold met, site requires inspection"),
        tags$li(tags$strong("Under Threshold:"), " Inspected with larvae count below treatment threshold"),
        tags$li(tags$strong("Needs Treatment:"), " Inspected with larvae count at or above treatment threshold"),
        tags$li(tags$strong("Active Treatment:"), " Treatment applied and still within effective period")
      )
    )
  )
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
  set.seed(as.numeric(as.Date(sitecode, format = "TEST-%03d")))
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
    if (has_triggering_rain && is.na(last_inspection_date) && runif(1) < 0.3) {
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
    } else if (has_triggering_rain) {
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
  
  # Test Case 2: Inspection logic
  test_sites_2 <- data.frame(
    sitecode = c("TEST-004", "TEST-005", "TEST-006"),
    total_rainfall = c(1.5, 1.5, 1.5),
    has_triggering_rainfall = c(TRUE, TRUE, TRUE),
    last_inspection_date = Sys.Date() - c(1, 1, 1),
    last_larvae_count = c(1, 2, 5),
    treatment_expiry = as.Date(c(NA, NA, NA))
  )
  
  expected_status_2 <- c("Needs Inspection", "Needs Treatment", "Needs Treatment")
  
  # Test Case 3: Treatment effects
  test_sites_3 <- data.frame(
    sitecode = c("TEST-007", "TEST-008", "TEST-009"),
    total_rainfall = c(1.5, 1.5, 1.5),
    has_triggering_rainfall = c(TRUE, TRUE, TRUE),
    last_inspection_date = Sys.Date() - c(5, 5, 5),
    last_larvae_count = c(3, 3, 3),
    treatment_expiry = c(Sys.Date() + 10, Sys.Date() - 5, NA)
  )
  
  expected_status_3 <- c("Active Treatment", "Unknown", "Needs Treatment")
  
  all_tests <- list(
    list(data = test_sites_1, expected = expected_status_1, name = "Rainfall Thresholds"),
    list(data = test_sites_2, expected = expected_status_2, name = "Inspection Logic"),
    list(data = test_sites_3, expected = expected_status_3, name = "Treatment Effects")
  )
  
  test_results <- character(0)
  
  for (i in 1:length(all_tests)) {
    test <- all_tests[[i]]
    
    # Apply business logic
    actual_status <- case_when(
      # Active treatment
      !is.na(test$data$treatment_expiry) & Sys.Date() <= test$data$treatment_expiry ~ "Active Treatment",
      # Treatment expired -> Unknown
      !is.na(test$data$treatment_expiry) & Sys.Date() > test$data$treatment_expiry ~ "Unknown",
      # Has triggering rainfall but no inspection -> Needs Inspection
      test$data$has_triggering_rainfall & is.na(test$data$last_inspection_date) ~ "Needs Inspection",
      # Has triggering rainfall and inspected
      test$data$has_triggering_rainfall & !is.na(test$data$last_inspection_date) ~ case_when(
        test$data$last_larvae_count >= 2 ~ "Needs Treatment",
        TRUE ~ "Needs Inspection"  # Back to needs inspection after new rain
      ),
      # No triggering rainfall but inspected
      !test$data$has_triggering_rainfall & !is.na(test$data$last_inspection_date) ~ case_when(
        test$data$last_larvae_count >= 2 ~ "Needs Treatment",
        TRUE ~ "Under Threshold"
      ),
      # Default
      TRUE ~ "Unknown"
    )
    
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