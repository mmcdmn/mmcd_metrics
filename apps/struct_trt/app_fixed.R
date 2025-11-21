# struct status App

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(rlang)
  library(purrr)  # For map_dfr function
  library(tibble) # For deframe function
  library(scales) # For percentage and number formatting
  library(DT)     # For data tables
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")

# Source external function files
source("data_functions.R")
source("display_functions.R")

# Load environment variables from .env file
env_paths <- c(
  "../../.env",
  "../../../.env",
  "/srv/shiny-server/.env"
)

env_loaded <- FALSE
for (path in env_paths) {
  if (file.exists(path)) {
    readRenviron(path)
    env_loaded <- TRUE
    break
  }
}

ui <- fluidPage(
  titlePanel("Structures with Active and Expiring Treatments"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("expiring_days", "Days Until Expiration:",
                  min = 1, max = 30, value = 7, step = 1),
      
      dateInput("custom_today", "Pretend Today is:",
                value = Sys.Date(), 
                format = "yyyy-mm-dd"),
      
      checkboxGroupInput("status_types", "Include Structure Status:",
                         choices = c("Dry (D)" = "D",
                                     "Wet (W)" = "W", 
                                     "Unknown (U)" = "U"),
                         selected = c("D", "W", "U")),
      
      selectizeInput("facility_filter", "Facility:",
                    choices = get_facility_choices(),
                    selected = "all", multiple = TRUE),
      
      selectInput("group_by", "Group by:",
                  choices = c("Facility" = "facility",
                              "FOS" = "foreman", 
                              "All MMCD" = "mmcd_all"),
                  selected = "facility"),
      
      radioButtons("zone_filter", "Zone Display:",
                   choices = c("P1 Only" = "1", 
                              "P2 Only" = "2", 
                              "P1 and P2 Separate" = "1,2", 
                              "Combined P1+P2" = "combined"),
                   selected = "1,2"),
      
      selectInput("structure_type_filter", "Structure Type:",
                  choices = get_structure_type_choices(include_all = TRUE),
                  selected = "all"),
      # we removed priority filter for now because data is incomplete
      # not all facilities have priorities for the structures
      
      helpText(tags$b("Structure Status:"),
               tags$br(),
               tags$ul(
                 tags$li(tags$b("D:"), "Dry - Structure is dry"),
                 tags$li(tags$b("W:"), "Wet - Structure has water"),
                 tags$li(tags$b("U:"), "Unknown - Status not determined")
               )),
      
      actionButton("refresh", "Refresh Data", 
                   icon = icon("refresh"),
                   class = "btn-primary btn-lg",
                   style = "width: 100%; margin-top: 20px;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Current Progress", 
                 plotOutput("structureGraph", height = "800px"),
                 br(),
                 fluidRow(
                   column(10, h4("Current Structure Details")),
                   column(2, downloadButton("download_current_data", "Download CSV", 
                                           class = "btn-success btn-sm", 
                                           style = "margin-top: 20px; float: right;"))
                 ),
                 DT::dataTableOutput("currentStructureTable")
        ),
        tabPanel("Historical Trends",
                 fluidRow(
                   column(3,
                          selectInput("start_year", "Start Year:",
                                      choices = seq(2010, 2025),
                                      selected = 2024)
                   ),
                   column(3,
                          selectInput("end_year", "End Year:",
                                      choices = seq(2010, 2025),
                                      selected = 2025)
                   ),
                   column(3,
                          radioButtons("hist_display_metric", "Display Metric:",
                                      choices = c("Proportion (%)" = "proportion",
                                                  "Raw Numbers" = "raw_numbers"),
                                      selected = "proportion",
                                      inline = TRUE)
                   ),
                   column(3,
                          selectInput("hist_chart_type", "Chart Type:",
                                      choices = c("Line Chart" = "line",
                                                  "Area Chart" = "area",
                                                  "Step Chart" = "step",
                                                  "Stacked Bar" = "stacked_bar",
                                                  "Grouped Bar" = "grouped_bar"),
                                      selected = "line")
                   )
                 ),
                 fluidRow(
                   column(6,
                          checkboxGroupInput("hist_average_lines", "Show Average Lines:",
                                            choices = c("5-Year Average" = "avg_5yr",
                                                        "10-Year Average" = "avg_10yr"),
                                            selected = NULL,
                                            inline = TRUE)
                   ),
                   column(6,
                          helpText(tags$small("Note: Average lines use all available historical data (5 or 10 years) regardless of selected date range"))
                   )
                 ),
                 plotOutput("historicalGraph", height = "600px"),
                 br(),
                 fluidRow(
                   column(10, h4("Historical Treatment Data")),
                   column(2, downloadButton("download_historical_data", "Download CSV", 
                                           class = "btn-success btn-sm", 
                                           style = "margin-top: 20px; float: right;"))
                 ),
                 DT::dataTableOutput("historicalStructureTable")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # =============================================================================
  # REFRESH BUTTON PATTERN - Capture all inputs when refresh clicked
  # =============================================================================
  
  refresh_inputs <- eventReactive(input$refresh, {
    zone_value <- isolate(input$zone_filter)
    
    # Parse zone filter
    parsed_zones <- if (zone_value == "combined") {
      c("1", "2")  # Include both zones but will be combined
    } else if (zone_value == "1,2") {
      c("1", "2")  # Include both zones separately
    } else {
      zone_value  # Single zone
    }
    
    list(
      zone_filter_raw = zone_value,
      zone_filter = parsed_zones,
      combine_zones = (zone_value == "combined"),
      expiring_days = isolate(input$expiring_days),
      custom_today = isolate(input$custom_today),
      status_types = isolate(input$status_types),
      facility_filter = isolate(input$facility_filter),
      group_by = isolate(input$group_by),
      structure_type_filter = isolate(input$structure_type_filter),
      priority_filter = "all",  # Default value since priority filter was removed from UI
      start_year = isolate(input$start_year),
      end_year = isolate(input$end_year),
      hist_display_metric = isolate(input$hist_display_metric),
      hist_chart_type = isolate(input$hist_chart_type),
      hist_average_lines = isolate(input$hist_average_lines)
    )
  })
  
  # Load current data - ONLY when refresh button clicked
  current_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    get_current_structure_data(
      inputs$custom_today,
      inputs$expiring_days,
      inputs$facility_filter,
      inputs$structure_type_filter,
      inputs$priority_filter,
      inputs$status_types,
      inputs$zone_filter
    )
  })
  
  # Load all structures - ONLY when refresh button clicked
  all_structures <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    get_all_structures(
      inputs$facility_filter,
      inputs$structure_type_filter,
      inputs$priority_filter,
      inputs$status_types,
      inputs$zone_filter
    )
  })
  
  # Load historical data - ONLY when refresh button clicked
  historical_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    get_historical_structure_data(
      inputs$start_year,
      inputs$end_year,
      inputs$facility_filter,
      inputs$structure_type_filter,
      inputs$priority_filter,
      inputs$status_types,
      inputs$zone_filter
    )
  })
  
  # Aggregate current data
  aggregated_current <- reactive({
    req(input$refresh)  # Require refresh button click
    inputs <- refresh_inputs()
    
    structures <- all_structures()
    treatments <- current_data()$treatments
    
    aggregate_structure_data(
      structures,
      treatments,
      inputs$group_by,
      inputs$zone_filter,
      inputs$combine_zones
    )
  })
  
  # Render current progress chart
  output$structureGraph <- renderPlot({
    req(input$refresh)  # Only render after refresh button clicked
    inputs <- refresh_inputs()
    
    data <- aggregated_current()
    
    create_current_progress_chart(
      data,
      inputs$group_by,
      inputs$facility_filter,
      inputs$status_types,
      inputs$zone_filter,
      inputs$combine_zones
    )
  })
  
  # Render historical trends chart
  output$historicalGraph <- renderPlot({
    req(input$refresh)  # Only render after refresh button clicked
    inputs <- refresh_inputs()
    
    hist_data <- historical_data()
    
    create_historical_trends_chart(
      hist_data$treatments,
      hist_data$total_structures,
      inputs$start_year,
      inputs$end_year,
      inputs$group_by,
      inputs$facility_filter,
      inputs$structure_type_filter,
      inputs$priority_filter,
      inputs$status_types,
      inputs$zone_filter,
      inputs$combine_zones,
      inputs$hist_display_metric,
      inputs$hist_chart_type,
      inputs$hist_average_lines
    )
  })
  
  # Current structure details table - simplified version
  output$currentStructureTable <- DT::renderDataTable({
    req(input$refresh)  # Only render after refresh button clicked
    
    all_structs <- all_structures()
    
    if (nrow(all_structs) == 0) {
      return(DT::datatable(
        data.frame("No data available" = character(0)),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ))
    }
    
    # Show structure data with better column names
    display_data <- all_structs %>%
      select(sitecode, facility, zone, s_type, foreman, status) %>%
      rename(
        "Sitecode" = sitecode,
        "Facility" = facility,
        "Zone" = zone,
        "Structure Type" = s_type,
        "FOS" = foreman,
        "Status" = status
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Historical structure data table
  output$historicalStructureTable <- DT::renderDataTable({
    req(input$refresh)  # Only render after refresh button clicked
    
    hist_data <- historical_data()
    
    if (is.null(hist_data) || nrow(hist_data$treatments) == 0) {
      return(DT::datatable(
        data.frame("No data available" = character(0)),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ))
    }
    
    # Process historical data for table display
    table_data <- hist_data$treatments %>%
      mutate(
        year = year(inspdate),
        month = month(inspdate, label = TRUE)
      ) %>%
      arrange(desc(inspdate)) %>%
      select(sitecode, facility, zone, year, month, inspdate, s_type, foreman) %>%
      rename(
        "Sitecode" = sitecode,
        "Facility" = facility,
        "Zone" = zone,
        "Year" = year,
        "Month" = month,
        "Treatment Date" = inspdate,
        "Structure Type" = s_type,
        "FOS" = foreman
      )
    
    DT::datatable(
      table_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Download handlers for CSV exports
  output$download_current_data <- downloadHandler(
    filename = function() {
      paste0("structure_current_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      all_structs <- all_structures()
      
      if (nrow(all_structs) > 0) {
        # Export structure data
        export_csv_safe(all_structs, file)
      } else {
        export_csv_safe(data.frame("No data available" = character(0)), file)
      }
    }
  )
  
  output$download_historical_data <- downloadHandler(
    filename = function() {
      paste0("structure_historical_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      hist_data <- historical_data()
      
      if (!is.null(hist_data) && nrow(hist_data$treatments) > 0) {
        # Process historical data for CSV export
        table_data <- hist_data$treatments %>%
          mutate(
            year = year(inspdate),
            month = month(inspdate, label = TRUE)
          ) %>%
          arrange(desc(inspdate))
        
        export_csv_safe(table_data, file)
      } else {
        export_csv_safe(data.frame("No data available" = character(0)), file)
      }
    }
  )
}

shinyApp(ui = ui, server = server)