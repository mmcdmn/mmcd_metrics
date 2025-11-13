# Inspections App - Site Inspection Coverage Gaps

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(lubridate)

# Source shared database helpers and local functions
source("../../shared/db_helpers.R")
source("data_functions.R")
source("display_functions.R")
source("ui_helper.R")

# Define UI
ui <- create_main_ui()

# Define server logic
server <- function(input, output, session) {
  
  # Reactive data - ONLY loads when refresh button is clicked
  gap_data <- eventReactive(input$refresh, {
    get_inspection_gaps(
      air_gnd_filter = input$air_gnd,
      facility_filter = if (length(input$facility) == 0 || "all" %in% input$facility) NULL else input$facility,
      fosarea_filter = if (length(input$fosarea) == 0 || "all" %in% input$fosarea) NULL else input$fosarea,
      zone_filter = if (length(input$zone) == 0 || "all" %in% input$zone) NULL else input$zone,
      priority_filter = if (length(input$priority) == 0 || "all" %in% input$priority) NULL else input$priority,
      years_gap = input$years_gap,
      ref_date = Sys.Date()
    )
  })
  
  # Summary output - only shows after refresh
  output$summary <- renderText({
    if (input$refresh == 0) {
      " Click 'Load Inspection Gaps' to start analysis"
    } else {
      data <- gap_data()
      if (nrow(data) == 0) {
        " No sites found with inspection gaps."
      } else {
        never_inspected <- sum(data$inspection_status == "Never Inspected", na.rm = TRUE)
        gap_sites <- sum(data$inspection_status == "Inspection Gap", na.rm = TRUE)
        paste0(" Found ", format(nrow(data), big.mark=","), " sites with gaps: ",
               format(never_inspected, big.mark=","), " never inspected, ",
               format(gap_sites, big.mark=","), " with ", input$years_gap, "+ year gaps")
      }
    }
  })
  
  # Data table output - only shows after refresh
  output$gaps_table <- DT::renderDataTable({
    if (input$refresh == 0) {
      DT::datatable(
        data.frame(Message = "Click 'Load Inspection Gaps' button to load data"),
        rownames = FALSE,
        options = list(dom = 't', ordering = FALSE)
      )
    } else {
      render_gap_table(gap_data())
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)