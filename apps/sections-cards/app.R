# sections-cards DEMO App

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(DT)
  library(leaflet)
  library(sf)
  library(RColorBrewer)
})

# Source shared helper functions
source("../../shared/db_helpers.R")

# Source external function files
source("ui_helper.R")
source("data_functions.R")
source("display_functions.R")

# Load environment variables
load_env_vars()

# =============================================================================
# UI Definition
# =============================================================================

ui <- fluidPage(
  
  # Custom CSS for print button and layout
  tags$head(
    tags$style(HTML("
      .print-button {
        position: sticky;
        top: 10px;
        z-index: 1000;
        margin-bottom: 20px;
      }
      @media print {
        .sidebar, .print-button, .no-print {
          display: none !important;
        }
        .main-panel {
          width: 100% !important;
          margin: 0 !important;
          padding: 0 !important;
        }
      }
    "))
  ),
  
  titlePanel("Sections Cards DEMO"),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      create_field_selector(),
      
      hr(),
      
      actionButton(
        "generate_cards",
        "Generate Cards",
        class = "btn-primary btn-block",
        icon = icon("layer-group")
      ),
      
      br(), br(),
      
      downloadButton(
        "download_html",
        "Download as HTML",
        class = "btn-success btn-block"
      ),
      
      tags$p(
        class = "help-block",
        style = "margin-top: 10px; font-size: 12px;",
        HTML("<strong>How to Print/Save as PDF:</strong><br/>
             1. Click 'Generate Cards' to create the cards<br/>
             2. Use the browser's print function (Ctrl+P / Cmd+P)<br/>
             3. Select 'Save as PDF' as the destination<br/>
             4. Or download HTML file and open in browser to print<br/>
             <br/>
             <em>The layout is optimized for Letter size paper with 6 cards per page.</em>")
      )
    ),
    
    mainPanel(
      class = "main-panel",
      
      # Print button at top of cards
      div(
        class = "print-button no-print",
        actionButton(
          "print_cards",
          "Print Cards (Ctrl+P / Cmd+P)",
          class = "btn-info btn-lg",
          icon = icon("print"),
          onclick = "window.print();"
        )
      ),
      
      # Loading message
      conditionalPanel(
        condition = "!output.section_cards",
        div(
          style = "text-align: center; padding: 50px;",
          h4("Configure your card fields and click 'Generate Cards' to begin"),
          icon("layer-group", class = "fa-3x", style = "color: #ccc;")
        )
      ),
      
      # Cards display area
      uiOutput("section_cards")
    )
  )
)

# =============================================================================
# Server Logic
# =============================================================================

server <- function(input, output, session) {
  
  # Reactive values
  cards_data <- reactiveVal(NULL)
  
  # Load ONLY filter options on startup (lightweight query)
  observe({
    tryCatch({
      filter_opts <- get_filter_options()
      
      # Update facility filter choices
      updateSelectInput(
        session,
        "filter_facility",
        choices = c("All" = "all", setNames(filter_opts$facilities, filter_opts$facilities))
      )
      
      # Update FOS area filter choices
      updateSelectInput(
        session,
        "filter_fosarea",
        choices = c("All" = "all", setNames(filter_opts$fosarea_list, filter_opts$fosarea_list))
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error loading filter options:", e$message),
        type = "error",
        duration = NULL
      )
    })
  })
  
  # Generate cards when button is clicked
  observeEvent(input$generate_cards, {
    
    # Show loading notification
    loading_id <- showNotification(
      "Loading breeding sites data...",
      duration = NULL,
      closeButton = FALSE,
      type = "message"
    )
    
    # Load full dataset only when Generate Cards is clicked
    tryCatch({
      data <- get_breeding_sites_with_sections()
      cards_data(data)
      
      removeNotification(loading_id)
      
      showNotification(
        paste("Loaded", nrow(data), "breeding sites"),
        duration = 2,
        type = "message"
      )
      
    }, error = function(e) {
      removeNotification(loading_id)
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = NULL
      )
      return()
    })
    
    req(cards_data())
    
    # Apply filters
    filtered <- cards_data()
    
    # Apply facility filter
    if (input$filter_facility != "all") {
      filtered <- filtered %>% filter(facility == input$filter_facility)
    }
    
    # Apply air/ground filter
    if (input$filter_air_gnd != "all") {
      filtered <- filtered %>% filter(air_gnd == input$filter_air_gnd)
    }
    
    # Apply drone filter
    if (input$filter_drone == "include") {
      # All sites (no filter)
    } else if (input$filter_drone == "exclude") {
      filtered <- filtered %>% filter(is.na(drone) | drone == "")
    } else if (input$filter_drone == "only") {
      filtered <- filtered %>% filter(!is.na(drone) & drone != "")
    }
    
    # Apply zone filter
    if (input$filter_zone != "all") {
      filtered <- filtered %>% filter(zone == input$filter_zone)
    }
    
    # Apply FOS area filter
    if (input$filter_fosarea != "all") {
      filtered <- filtered %>% filter(fosarea == input$filter_fosarea)
    }
    
    # Apply priority filter
    if (input$filter_priority != "all") {
      filtered <- filtered %>% filter(priority == input$filter_priority)
    }
    
    if (nrow(filtered) == 0) {
      showNotification(
        "No sites match the selected filters.",
        type = "warning"
      )
      output$section_cards <- renderUI({
        div(
          style = "text-align: center; padding: 50px;",
          h4("No sites match the selected filters"),
          p("Try adjusting your filter settings")
        )
      })
      return()
    }
    
    if (nrow(filtered) == 0) {
      showNotification(
        "No sites match the selected filters.",
        type = "warning"
      )
      output$section_cards <- renderUI({
        div(
          style = "text-align: center; padding: 50px;",
          h4("No sites match the selected filters"),
          p("Try adjusting your filter settings")
        )
      })
      return()
    }
    
    # Ensure sitecode is always in title fields
    title_fields <- c("sitecode", input$title_fields)
    
    if (length(input$table_fields) == 0) {
      showNotification(
        "Please select at least one table column.",
        type = "warning"
      )
      return()
    }
    
    # Generate the HTML
    cards_html <- generate_section_cards_html(
      filtered,
      title_fields,
      input$table_fields,
      input$num_rows
    )
    
    # Render the cards
    output$section_cards <- renderUI({
      HTML(cards_html)
    })
    
    showNotification(
      paste("Generated", nrow(filtered), "section cards"),
      type = "message",
      duration = 3
    )
  })
  
  # Download handler for HTML file
  output$download_html <- downloadHandler(
    filename = function() {
      paste0("section-cards-", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      req(cards_data())
      
      # Apply same filters as generate cards
      filtered <- cards_data()
      
      if (input$filter_facility != "all") {
        filtered <- filtered %>% filter(facility == input$filter_facility)
      }
      if (input$filter_air_gnd != "all") {
        filtered <- filtered %>% filter(air_gnd == input$filter_air_gnd)
      }
      if (input$filter_drone == "exclude") {
        filtered <- filtered %>% filter(is.na(drone) | drone == "")
      } else if (input$filter_drone == "only") {
        filtered <- filtered %>% filter(!is.na(drone) & drone != "")
      }
      if (input$filter_zone != "all") {
        filtered <- filtered %>% filter(zone == input$filter_zone)
      }
      if (input$filter_fosarea != "all") {
        filtered <- filtered %>% filter(fosarea == input$filter_fosarea)
      }
      if (input$filter_priority != "all") {
        filtered <- filtered %>% filter(priority == input$filter_priority)
      }
      
      title_fields <- c("sitecode", input$title_fields)
      
      # Create complete HTML document
      html_content <- paste0(
        '<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>Section Cards - ', format(Sys.Date(), "%Y-%m-%d"), '</title>
</head>
<body>
  <h1 style="text-align: center; font-family: Arial;">MMCD Section Cards</h1>
  <p style="text-align: center; font-family: Arial; color: #666;">
    Generated: ', format(Sys.time(), "%Y-%m-%d %H:%M"), '
  </p>
  ',
        generate_section_cards_html(
          filtered,
          title_fields,
          input$table_fields,
          input$num_rows
        ),
        '
</body>
</html>'
      )
      
      writeLines(html_content, file)
      
      showNotification(
        "HTML file downloaded. Open in browser and use Ctrl+P to print or save as PDF.",
        type = "message",
        duration = 5
      )
    }
  )
}

# =============================================================================
# Run the application
# =============================================================================

shinyApp(ui = ui, server = server)
