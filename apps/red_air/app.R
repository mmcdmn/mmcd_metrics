# Air Work Pipeline Application - Modular Version
# Based on requirements: Track all air sites with proper status lifecycle

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(leaflet)
  library(DT)
  library(plotly)
  library(tidyr)
})

# Source the shared database helper functions
suppressWarnings({
  source("../../shared/db_helpers.R")
})

# Source the external function files
source("air_status_functions.R")
source("flow_testing_functions.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Air Work Pipeline"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Air Site Status", tabName = "status", icon = icon("helicopter")),
      menuItem("Flow Testing", tabName = "testing", icon = icon("flask"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "status",
        fluidRow(
          box(title = "Controls", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(3,
                dateInput("analysis_date", "Analysis Date:",
                  value = Sys.Date(),
                  max = Sys.Date()
                )
              ),
              column(3,
                selectInput("lookback_period", "Rainfall Lookback Period:",
                  choices = list(
                    "24 hours" = 1,
                    "48 hours" = 2, 
                    "72 hours" = 3,
                    "4 days" = 4
                  ),
                  selected = 3
                )
              ),
              column(3,
                numericInput("rain_threshold", "Rain Threshold (inches):",
                  value = 1.0,
                  min = 0.1,
                  max = 5.0,
                  step = 0.1
                )
              ),
              column(3,
                selectInput("facility_filter", "Facility:",
                  choices = c("All Facilities" = "all"),
                  selected = "all"
                )
              )
            ),
            fluidRow(
              column(3,
                numericInput("treatment_threshold", "Treatment Threshold (larvae count):",
                  value = 1,
                  min = 0,
                  max = 100,
                  step = 1
                )
              ),
              column(9, "")
            ),
            fluidRow(
              column(4,
                selectInput("status_filter", "Status Filter:",
                  choices = c("All Statuses" = "all",
                             "Unknown" = "Unknown",
                             "Needs Inspection" = "Needs Inspection", 
                             "Under Threshold" = "Under Threshold",
                             "Needs Treatment" = "Needs Treatment",
                             "Active Treatment" = "Active Treatment"),
                  selected = "all"
                )
              ),
              column(4,
                selectInput("priority_filter", "Priority:",
                  choices = c("All Priorities" = "all", "RED" = "RED"),
                  selected = "RED"
                )
              ),
              column(4,
                checkboxGroupInput("zone_filter", "Filter by Zone:",
                  choices = c("P1" = "1", "P2" = "2"),
                  selected = c("1", "2"))
              )
            ),
            fluidRow(
              column(4,
                actionButton("refresh_data", "Refresh Data", class = "btn-primary")
              ),
              column(8, "")
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("total_air_sites", width = 2),
          valueBoxOutput("sites_unknown", width = 2),
          valueBoxOutput("sites_needs_inspection", width = 2),
          valueBoxOutput("sites_under_threshold", width = 2),
          valueBoxOutput("sites_needs_treatment", width = 2),
          valueBoxOutput("sites_active_treatment", width = 2)
        ),
        
        fluidRow(
          box(title = "Air Site Status Map", status = "primary", solidHeader = TRUE, width = 8,
            leafletOutput("status_map", height = "500px")
          ),
          box(title = "Status Summary", status = "info", solidHeader = TRUE, width = 4,
            plotlyOutput("status_chart", height = "500px")
          )
        ),
        
        fluidRow(
          box(title = "Site Details", status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("site_details_table")
          )
        )
      ),
      
      tabItem(tabName = "testing",
        # Parameter Testing Section
        create_parameter_testing_ui(),
        
        # Flow Diagram Section
        create_flow_diagram_ui(),
        
        # Detailed Test Results Section
        fluidRow(
          box(
            title = "Detailed Test Results", status = "warning", solidHeader = TRUE, width = 12,
            collapsible = TRUE, collapsed = FALSE,
            
            tabsetPanel(
              tabPanel("Site Details",
                br(),
                DT::dataTableOutput("flow_test_sites_table")
              ),
              
              tabPanel("Site Timelines",
                br(),
                p("Visual timeline showing how individual sites transition through statuses over time."),
                fluidRow(
                  column(6,
                    selectInput("timeline_site1", "Site 1:", 
                               choices = c("TEST-001", "TEST-002", "TEST-003", "TEST-004", "TEST-005"),
                               selected = "TEST-001")
                  ),
                  column(6,
                    selectInput("timeline_site2", "Site 2:", 
                               choices = c("TEST-001", "TEST-002", "TEST-003", "TEST-004", "TEST-005"),
                               selected = "TEST-003")
                  )
                ),
                fluidRow(
                  column(6, plotlyOutput("timeline_plot1", height = "400px")),
                  column(6, plotlyOutput("timeline_plot2", height = "400px"))
                )
              ),
              
              tabPanel("Status Transitions",
                br(),
                h5("Expected Transitions"),
                p("Based on the business logic, here are the possible status transitions:"),
                tags$ul(
                  tags$li("Unknown → Needs Inspection: When rainfall ≥ threshold is detected"),
                  tags$li("Needs Inspection → Under Threshold: When inspection shows larvae < threshold"),
                  tags$li("Needs Inspection → Needs Treatment: When inspection shows larvae ≥ threshold"),
                  tags$li("Under Threshold → Needs Inspection: When new rainfall ≥ threshold occurs"),
                  tags$li("Needs Treatment → Active Treatment: When treatment is applied"),
                  tags$li("Active Treatment → Unknown: When treatment expires")
                ),
                br(),
                h5("Synthetic Data Status Breakdown"),
                DT::dataTableOutput("flow_test_transitions_table")
              ),
                
                tabPanel("Business Logic Validation",
                  br(),
                  h5("Business Logic Test Cases"),
                  p("Verify that the system correctly handles edge cases and business rules:"),
                  
                  fluidRow(
                    column(6,
                      h6("Test Case 1: Rainfall Detection"),
                      tags$ul(
                        tags$li("Site with 0.5\" rain → Should remain Unknown"),
                        tags$li("Site with 1.2\" rain → Should become Needs Inspection"),
                        tags$li("Site with consecutive days totaling 1.0\" → Should trigger")
                      )
                    ),
                    column(6,
                      h6("Test Case 2: Inspection Logic"),
                      tags$ul(
                        tags$li("Larvae count 0-1 → Under Threshold"),
                        tags$li("Larvae count 2+ → Needs Treatment"),
                        tags$li("No inspection after rain → Needs Inspection")
                      )
                    )
                  ),
                  
                  br(),
                  fluidRow(
                    column(6,
                      h6("Test Case 3: Treatment Effects"),
                      tags$ul(
                        tags$li("Active treatment → Overrides other status"),
                        tags$li("Expired treatment → Returns to Unknown"),
                        tags$li("Treatment duration varies by material type")
                      )
                    ),
                    column(6,
                      h6("Test Case 4: State Persistence"),
                      tags$ul(
                        tags$li("Needs Inspection persists until inspected"),
                        tags$li("Under Threshold changes with new rain"),
                        tags$li("Active Treatment blocks other status changes")
                      )
                    )
                  ),
                  
                  br(),
                  actionButton("validate_business_logic", "Run Business Logic Validation", 
                              class = "btn-warning"),
                  br(), br(),
                  conditionalPanel(
                    condition = "input.validate_business_logic > 0",
                    verbatimTextOutput("business_logic_results")
                  )
                )
              )
            )
          )
        )
      )
    )
  )

# Define Server
server <- function(input, output, session) {
  
  # Load facility choices
  observe({
    tryCatch({
      # Use the shared function to get facility choices
      fac_choices <- get_facility_choices(include_all = TRUE)
      updateSelectInput(session, "facility_filter", choices = fac_choices)
    }, error = function(e) {
      showNotification(paste("Error loading facilities:", e$message), type = "error")
    })
  })
  
  # Main data reactive - gets all air sites with calculated status using external function
  air_sites_data <- reactive({
    input$refresh_data  # Trigger on refresh button
    req(input$analysis_date, input$lookback_period, input$rain_threshold, input$treatment_threshold)
    
    get_air_sites_data(
      analysis_date = input$analysis_date,
      lookback_period = input$lookback_period,
      rain_threshold = input$rain_threshold,
      treatment_threshold = input$treatment_threshold,
      facility_filter = input$facility_filter,
      priority_filter = input$priority_filter,
      zone_filter = input$zone_filter
    )
  })
  
  # Value boxes using external data
  output$total_air_sites <- renderValueBox({
    data <- air_sites_data()
    valueBox(
      value = nrow(data),
      subtitle = "Total Air Sites",
      icon = icon("helicopter"),
      color = "blue"
    )
  })
  
  output$sites_needs_inspection <- renderValueBox({
    data <- air_sites_data()
    count <- sum(data$site_status == "Needs Inspection", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Needs Inspection",
      icon = icon("search"),
      color = "yellow"
    )
  })
  
  output$sites_unknown <- renderValueBox({
    data <- air_sites_data()
    count <- sum(data$site_status == "Unknown", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Unknown",
      icon = icon("question-circle"),
      color = "light-blue"
    )
  })
  
  output$sites_active_treatment <- renderValueBox({
    data <- air_sites_data()
    count <- sum(data$site_status == "Active Treatment", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Active Treatment",
      icon = icon("spray-can"),
      color = "purple"
    )
  })
  
  output$sites_under_threshold <- renderValueBox({
    data <- air_sites_data()
    count <- sum(data$site_status == "Under Threshold", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Under Threshold",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$sites_needs_treatment <- renderValueBox({
    data <- air_sites_data()
    count <- sum(data$site_status == "Needs Treatment", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Needs Treatment",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  # Status map using external function
  output$status_map <- renderLeaflet({
    data <- air_sites_data()
    create_status_map(data, input$status_filter)
  })
  
  # Status chart using external function
  output$status_chart <- renderPlotly({
    data <- air_sites_data()
    create_status_chart(data, input$status_filter)
  })
  
  # Site details table using external function
  output$site_details_table <- renderDT({
    data <- air_sites_data()
    create_site_details_table(data, input$status_filter)
  })
  
  # Flow Testing Functionality
  # Reactive for synthetic flow test data
  synthetic_flow_data <- eventReactive(input$run_flow_test, {
    req(input$test_total_sites, input$test_rain_threshold, input$test_treatment_threshold)
    
    create_synthetic_flow_data(
      total_sites = input$test_total_sites,
      analysis_date = Sys.Date()
    )
  })
  
  # Value boxes for flow test results
  output$test_unknown <- renderValueBox({
    data <- synthetic_flow_data()
    count <- sum(data$site_status == "Unknown", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Unknown",
      icon = icon("question-circle"),
      color = "light-blue"
    )
  })
  
  output$test_needs_inspection <- renderValueBox({
    data <- synthetic_flow_data()
    count <- sum(data$site_status == "Needs Inspection", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Needs Inspection",
      icon = icon("search"),
      color = "yellow"
    )
  })
  
  output$test_under_threshold <- renderValueBox({
    data <- synthetic_flow_data()
    count <- sum(data$site_status == "Under Threshold", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Under Threshold",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$test_needs_treatment <- renderValueBox({
    data <- synthetic_flow_data()
    count <- sum(data$site_status == "Needs Treatment", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Needs Treatment",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$test_active_treatment <- renderValueBox({
    data <- synthetic_flow_data()
    count <- sum(data$site_status == "Active Treatment", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Active Treatment",
      icon = icon("spray-can"),
      color = "purple"
    )
  })
  
  output$test_total_sites <- renderValueBox({
    data <- synthetic_flow_data()
    count <- nrow(data)
    valueBox(
      value = count,
      subtitle = "Total Sites",
      icon = icon("map-marker"),
      color = "blue"
    )
  })
  
  # Flow test sites table
  output$flow_test_sites_table <- renderDT({
    if (input$run_flow_test == 0) {
      return(DT::datatable(data.frame(Message = "Click 'Run Flow Test' to generate data"), 
                          options = list(dom = 't'), rownames = FALSE))
    }
    
    data <- synthetic_flow_data()
    # Prepare data for display
    display_data <- data %>%
      select(sitecode, facility, priority, site_status, total_rainfall, 
             has_triggering_rainfall, last_inspection_date, last_larvae_count,
             last_treatment_date, last_treatment_material, treatment_expiry) %>%
      mutate(
        total_rainfall = round(total_rainfall, 3),
        last_inspection_date = as.character(last_inspection_date),
        last_treatment_date = as.character(last_treatment_date),
        treatment_expiry = as.character(treatment_expiry)
      )
    
    DT::datatable(display_data, 
                  options = list(pageLength = 15, scrollX = TRUE), 
                  rownames = FALSE,
                  caption = "Synthetic Test Site Data with Status Calculations")
  })
  
  # Flow test transitions table
  output$flow_test_transitions_table <- renderDT({
    if (input$run_flow_test == 0) {
      return(DT::datatable(data.frame(Message = "Click 'Run Flow Test' to generate data"), 
                          options = list(dom = 't'), rownames = FALSE))
    }
    
    data <- synthetic_flow_data()
    
    # Create summary by status and scenario
    summary_data <- data %>%
      group_by(site_status, rainfall_scenario, inspection_scenario, treatment_scenario) %>%
      summarise(
        count = n(),
        avg_rainfall = round(mean(total_rainfall, na.rm = TRUE), 3),
        avg_larvae = round(mean(last_larvae_count, na.rm = TRUE), 1),
        .groups = 'drop'
      ) %>%
      arrange(site_status, desc(count))
    
    DT::datatable(summary_data, 
                  options = list(pageLength = 10, scrollX = TRUE), 
                  rownames = FALSE,
                  caption = "Status Distribution by Scenario Combinations")
  })
  
  # Business logic validation
  output$business_logic_results <- renderText({
    input$validate_business_logic
    validate_business_logic()
  })
  
  # Flow testing functionality using external functions
  flow_test_data <- eventReactive(input$run_flow_test, {
    synth_params <- if (input$test_data_type == "synthetic") {
      list(
        total_sites = input$synth_total_sites,
        rain_pct = input$synth_rain_pct,
        inspect_pct = input$synth_inspect_pct,
        above_thresh_pct = input$synth_above_thresh_pct
      )
    } else {
      NULL
    }
    
    get_flow_test_data(
      start_date = input$test_start_date,
      end_date = input$test_end_date,
      data_type = input$test_data_type,
      synth_params = synth_params
    )
  })
  
  output$flow_test_results <- DT::renderDataTable({
    data <- flow_test_data()
    DT::datatable(data, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  output$daily_counts_chart <- renderPlotly({
    data <- flow_test_data()
    create_daily_counts_chart(data)
  })
  
  output$flow_summary <- renderText({
    data <- flow_test_data()
    create_flow_summary(data)
  })
  
  output$validation_summary <- renderText({
    data <- flow_test_data()
    create_validation_summary(data)
  })
  
  # Timeline plots for individual sites
  output$timeline_plot1 <- renderPlotly({
    timeline_data <- create_site_timeline_data(input$timeline_site1, Sys.Date())
    create_site_timeline_plot(timeline_data)
  })
  
  output$timeline_plot2 <- renderPlotly({
    timeline_data <- create_site_timeline_data(input$timeline_site2, Sys.Date())
    create_site_timeline_plot(timeline_data)
  })
}

# Run the application
shinyApp(ui = ui, server = server)