# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(rlang)
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
      
      checkboxGroupInput("zone_filter", "Zones:",
                         choices = c("P1" = "1", "P2" = "2"),
                         selected = c("1", "2")),
      
      selectInput("structure_type_filter", "Structure Type:",
                  choices = c("All" = "all", "AP", "CB", "CG", "cv", "CV", "CV/PR", "CV/RR", "DR", "PC", "Pool", "PR", "RG", "RR", "SP", "SS", "US", "W", "wo", "WO", "XX"),
                  selected = "all"),
      
      selectInput("priority_filter", "Priority:",
                  choices = c("All" = "all", "BLUE", "GREEN", "RED", "YELLOW"),
                  selected = "all"),
      
      helpText("This visualization shows structures by facility with three categories:",
               tags$br(),
               tags$ul(
                 tags$li(tags$span(style = "color:gray", "Gray: Total structures")),
                 tags$li(tags$span(style = paste0("color:", get_status_colors()["active"]), "Green: Structures with active treatments")),
                 tags$li(tags$span(style = paste0("color:", get_status_colors()["planned"]), "Orange: Structures with treatments expiring within the selected days"))
               )),
      
      helpText(tags$b("Date Simulation:"),
               tags$br(),
               "Use 'Pretend Today is' to see what treatments would be active/expiring on any specific date."),
      
      helpText(tags$b("Structure Status:"),
               tags$br(),
               tags$ul(
                 tags$li(tags$b("D:"), "Dry - Structure is dry"),
                 tags$li(tags$b("W:"), "Wet - Structure has water"),
                 tags$li(tags$b("U:"), "Unknown - Status not determined")
               ))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Current Progress", 
                 plotOutput("structureGraph", height = "600px")
        ),
        tabPanel("Historical Trends",
                 fluidRow(
                   column(3,
                          selectInput("start_year", "Start Year:",
                                      choices = seq(2010, 2025),
                                      selected = 2018)
                   ),
                   column(3,
                          selectInput("end_year", "End Year:",
                                      choices = seq(2010, 2025),
                                      selected = 2025)
                   )
                 ),
                 plotOutput("historicalGraph", height = "600px")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  current_data <- reactive({
    get_current_structure_data(
      input$custom_today,
      input$expiring_days,
      input$facility_filter,
      input$structure_type_filter,
      input$priority_filter,
      input$status_types,
      input$zone_filter
    )
  })
  
  all_structures <- reactive({
    get_all_structures(
      input$facility_filter,
      input$structure_type_filter,
      input$priority_filter,
      input$status_types,
      input$zone_filter
    )
  })
  
  historical_data <- reactive({
    get_historical_structure_data(
      input$start_year,
      input$end_year,
      input$facility_filter,
      input$structure_type_filter,
      input$priority_filter,
      input$status_types
    )
  })
  
  aggregated_current <- reactive({
    structures <- all_structures()
    treatments <- current_data()$treatments
    
    aggregate_structure_data(
      structures,
      treatments,
      input$group_by,
      input$zone_filter
    )
  })
  
  output$structureGraph <- renderPlot({
    data <- aggregated_current()
    
    create_current_progress_chart(
      data,
      input$group_by,
      input$facility_filter,
      input$status_types,
      input$zone_filter
    )
  })
  
  output$historicalGraph <- renderPlot({
    hist_data <- historical_data()
    
    create_historical_trends_chart(
      hist_data$treatments,
      hist_data$total_structures,
      input$start_year,
      input$end_year,
      input$group_by,
      input$facility_filter,
      input$structure_type_filter,
      input$priority_filter,
      input$status_types,
      input$zone_filter
    )
  })
}

shinyApp(ui = ui, server = server)
