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
                             "Needs Treatment" = "Needs Treatment"),
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
          valueBoxOutput("total_air_sites", width = 3),
          valueBoxOutput("sites_needs_inspection", width = 3),
          valueBoxOutput("sites_under_threshold", width = 3),
          valueBoxOutput("sites_needs_treatment", width = 3)
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
        fluidRow(
          box(title = "Pipeline Flow Testing", status = "primary", solidHeader = TRUE, width = 12,
            h4("Test the Rainfall → Inspection → Treatment Pipeline"),
            p("This tool helps verify the flow logic by showing status transitions over time."),
            
            fluidRow(
              column(3,
                dateInput("test_start_date", "Test Start Date:",
                  value = Sys.Date() - 7,
                  max = Sys.Date()
                )
              ),
              column(3,
                dateInput("test_end_date", "Test End Date:",
                  value = Sys.Date(),
                  max = Sys.Date()
                )
              ),
              column(3,
                selectInput("test_data_type", "Data Type:",
                  choices = list(
                    "Real Database Data" = "real",
                    "Synthetic Test Data" = "synthetic"
                  ),
                  selected = "real"
                )
              ),
              column(3,
                actionButton("run_flow_test", "Run Flow Test", class = "btn-success")
              )
            ),
            
            conditionalPanel(
              condition = "input.test_data_type == 'synthetic'",
              fluidRow(
                column(12,
                  h5("Synthetic Data Parameters:"),
                  fluidRow(
                    column(3,
                      numericInput("synth_total_sites", "Total Sites:", value = 100, min = 10, max = 1000)
                    ),
                    column(3,
                      numericInput("synth_rain_pct", "% Sites with Rain:", value = 60, min = 0, max = 100)
                    ),
                    column(3,
                      numericInput("synth_inspect_pct", "% Rain Sites Inspected:", value = 80, min = 0, max = 100)
                    ),
                    column(3,
                      numericInput("synth_above_thresh_pct", "% Above Threshold:", value = 30, min = 0, max = 100)
                    )
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(title = "Flow Test Results", status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("flow_test_results")
          )
        ),
        
        fluidRow(
          box(title = "Daily Status Counts", status = "success", solidHeader = TRUE, width = 6,
            plotlyOutput("daily_counts_chart")
          ),
          box(title = "Status Summary", status = "warning", solidHeader = TRUE, width = 6,
            verbatimTextOutput("flow_summary")
          )
        ),
        
        fluidRow(
          box(title = "Data Validation", status = "danger", solidHeader = TRUE, width = 12,
            verbatimTextOutput("validation_summary")
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
  output$site_details_table <- DT::renderDataTable({
    data <- air_sites_data()
    display_data <- create_site_details_table(data, input$status_filter)
    
    DT::datatable(display_data,
                  options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatStyle("site_status",
                      backgroundColor = DT::styleEqual(
                        c("Unknown", "Needs Inspection", "Under Threshold", "Needs Treatment"),
                        c("#6c757d", "#ffc107", "#28a745", "#dc3545")
                      ),
                      color = "white")
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
}

# Run the application
shinyApp(ui = ui, server = server)