# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(DT)
  library(plotly)
  library(tidyr)
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")

# Source external function files
source("data_functions.R")
source("display_functions.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "Ground Prehatch Progress",
    # Add filters to the header using tags$li - moved expiry controls to the right
    tags$li(
      style = "padding: 5px 8px; margin: 2px;",
      selectizeInput("facility_filter", "Facility:",
                    choices = c("All" = "all"),
                    selected = "all", 
                    multiple = TRUE,
                    options = list(placeholder = "Select facilities...")),
      class = "dropdown"
    ),
    tags$li(
      style = "padding: 5px 8px; margin: 2px;",
      selectizeInput("foreman_filter", "FOS:",
                    choices = c("All" = "all"),
                    selected = "all",
                    multiple = TRUE,
                    options = list(placeholder = "Select FOS...")),
      class = "dropdown"
    ),
    tags$li(
      style = "padding: 5px 8px; margin: 2px;",
      checkboxGroupInput("zone_filter", "Filter by Zone:",
                        choices = c("P1" = "1", "P2" = "2"),
                        selected = c("1", "2"),
                        inline = TRUE),
      class = "dropdown"
    ),
    tags$li(
      style = "padding: 5px 8px; margin: 2px;",
      radioButtons("group_by", "Group by:",
                  choices = c("All MMCD" = "mmcd_all",
                             "Facility" = "facility", 
                             "FOS" = "foreman",
                             "Section" = "sectcode"),
                  selected = "facility",
                  inline = TRUE),
      class = "dropdown"
    ),
    tags$li(
      style = "padding: 5px 8px; margin: 2px;",
      dateInput("custom_today", "Pretend Today is:",
               value = Sys.Date(), 
               format = "yyyy-mm-dd"),
      class = "dropdown"
    ),
    tags$li(
      style = "padding: 5px 8px; margin: 2px; float: right;",
      checkboxInput("show_expiring_only", "Expiring Only", value = FALSE),
      class = "dropdown"
    ),
    tags$li(
      style = "padding: 5px 8px; margin: 2px; float: right;",
      sliderInput("expiring_days", "Days Until Expiring:",
                 min = 1, max = 60, value = 14, step = 1),
      class = "dropdown"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Progress Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Detailed View", tabName = "details", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    # Add minimal CSS to prevent header overlap with sidebar tabs
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          margin-top: 160px !important;
        }
        .main-header {
          height: 250px !important;
          background-color: #3c8dbc !important;
        }
        .main-header .navbar {
          height: 250px !important;
          background-color: #3c8dbc !important;
        }
        .main-sidebar {
          margin-top: 150px !important;
        }
        .skin-blue .main-header .navbar {
          background-color: #3c8dbc !important;
        }
        .skin-blue .main-header .logo {
          background-color: #367fa9 !important;
        }
      "))
    ),
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
        fluidRow(
          box(
            title = "Summary Statistics",
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              valueBoxOutput("total_sites", width = 2),
              valueBoxOutput("prehatch_sites", width = 2),
              valueBoxOutput("treated_sites", width = 2),
              valueBoxOutput("needs_treatment", width = 2),
              valueBoxOutput("treated_pct", width = 2),
              valueBoxOutput("expiring_pct", width = 2)
            ),
            conditionalPanel(
              condition = "input.group_by == 'sectcode'",
              div(
                style = "background-color: #f8f9fa; padding: 8px; border-radius: 4px; margin-top: 10px;",
                HTML("<i class='fa fa-info-circle' style='color: #17a2b8;'></i> 
                     <strong>Note:</strong> Section filtering is only available when a specific facility is selected (not 'All').")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Ground Prehatch Treatment Progress",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("progress_chart")
          )
        )
      ),
      
      # Details tab  
      tabItem(tabName = "details",
        fluidRow(
          box(
            title = "Site Details",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            downloadButton("download_details_data", "Download CSV", class = "btn-primary"),
            br(), br(),
            DT::dataTableOutput("details_table")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Initialize facility choices from db_helpers
  observe({
    facility_choices <- get_facility_choices()
    updateSelectizeInput(session, "facility_filter", choices = facility_choices, selected = "all")
  })
  
  # Initialize foreman choices from db_helpers  
  observe({
    foremen_lookup <- get_foremen_lookup()
    foremen_choices <- c("All" = "all")
    foremen_choices <- c(
      foremen_choices,
      setNames(foremen_lookup$emp_num, foremen_lookup$shortname)
    )
    updateSelectizeInput(session, "foreman_filter", choices = foremen_choices, selected = "all")
  })

  # Fetch ground prehatch data
  ground_data <- reactive({
    req(input$zone_filter, input$group_by)  # Ensure required inputs are available
    
    # Use custom date if provided, otherwise use current date
    simulation_date <- if (!is.null(input$custom_today)) input$custom_today else Sys.Date()
    
    get_ground_prehatch_data(input$zone_filter, simulation_date)
  })
  
  # Fetch site details data
  site_details <- reactive({
    # Use custom date if provided, otherwise use current date
    simulation_date <- if (!is.null(input$custom_today)) input$custom_today else Sys.Date()
    
    get_site_details_data(input$expiring_days, simulation_date)
  })

  # Update foreman filter based on facility selection
  observe({
    data <- ground_data()
    
    # Filter by facility if not 'all'
    if (!is.null(input$facility_filter) && !("all" %in% input$facility_filter)) {
      data <- data %>% filter(facility %in% input$facility_filter)
    }
    
    # Get foremen lookup to map empnum to names
    foremen_lookup <- get_foremen_lookup()
    
    # Get foreman choices using helper function
    foreman_choices <- get_foreman_choices(data, foremen_lookup)
    
    updateSelectizeInput(session, "foreman_filter", choices = foreman_choices, selected = "all")
  })
  
  # Filter data based on user selections
  filtered_data <- reactive({
    req(input$zone_filter, input$group_by, input$facility_filter)
    # Don't require foreman_filter since it starts as NULL
    
    data <- ground_data()
    
    filter_ground_data(data, input$zone_filter, input$facility_filter, input$foreman_filter)
  })
  
  # Aggregate data based on grouping level  
  aggregated_data <- reactive({
    data <- filtered_data()
    site_data <- site_details()
    
    aggregate_data_by_group(
      data, 
      input$group_by, 
      input$zone_filter, 
      input$show_expiring_only, 
      site_data
    )
  })
  
  # Details data for the table
  details_data <- reactive({
    site_data <- site_details()
    
    filter_ground_data(site_data, input$zone_filter, input$facility_filter, input$foreman_filter)
  })
  
  # Create value boxes
  value_boxes <- reactive({
    data <- aggregated_data()
    create_value_boxes(data)
  })
  
  # Render value boxes
  output$total_sites <- renderValueBox({ value_boxes()$total_sites })
  output$prehatch_sites <- renderValueBox({ value_boxes()$prehatch_sites })
  output$treated_sites <- renderValueBox({ value_boxes()$treated_sites })
  output$needs_treatment <- renderValueBox({ value_boxes()$needs_treatment })
  output$treated_pct <- renderValueBox({ value_boxes()$treated_pct })
  output$expiring_pct <- renderValueBox({ value_boxes()$expiring_pct })
  
  # Render progress chart
  output$progress_chart <- renderPlotly({
    data <- aggregated_data()
    create_progress_chart(data, input$group_by, input$show_expiring_only, input$expiring_days)
  })
  
  # Render details table
  output$details_table <- DT::renderDataTable({
    data <- details_data()
    foremen_lookup <- get_foremen_lookup()
    create_details_table(data, foremen_lookup)
  })
  
  # Download handler for details data
  output$download_details_data <- downloadHandler(
    filename = function() {
      paste("ground_prehatch_details_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- details_data()
      foremen_lookup <- get_foremen_lookup()
      download_data <- prepare_download_data(data, foremen_lookup)
      write.csv(download_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)