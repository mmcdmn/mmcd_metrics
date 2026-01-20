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
      body, p, div, span, td, th, label, .help-block {
        font-size: 16px !important;
      }
      h1 { font-size: 32px !important; }
      h2 { font-size: 28px !important; }
      h3 { font-size: 24px !important; }
      h4 { font-size: 20px !important; }
      .btn { font-size: 16px !important; }
      .print-button {
        position: sticky;
        top: 10px;
        z-index: 1000;
        margin-bottom: 20px;
      }
      .loading-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(255, 255, 255, 0.9);
        z-index: 9999;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        padding: 40px;
        box-sizing: border-box;
      }
      .modal-dialog {
        max-width: none !important;
        margin: 0 !important;
        height: 100vh !important;
        width: 100vw !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
      }
      .modal-content {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
      }
      .loading-spinner {
        border: 8px solid #f3f3f3;
        border-top: 8px solid #3498db;
        border-radius: 50%;
        width: 60px;
        height: 60px;
        animation: spin 1s linear infinite;
      }
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      .loading-text {
        margin-top: 20px;
        font-size: 18px !important;
        color: #333;
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
    ")),
    
    # JavaScript for scroll position
    tags$script(HTML("
      Shiny.addCustomMessageHandler('restoreScroll', function(message) {
        setTimeout(function() {
          var element = document.getElementById('column_order_ui');
          if (element) {
            element.scrollTop = message.position;
          }
        }, 100);
      });
    "))
  ),

  
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
             <strong>Option 1 - Quick Print:</strong> Click 'Generate Cards' then use browser print (Ctrl+P / Cmd+P)<br/>
             <strong>Option 2 - Download First:</strong> Use 'Download as HTML' button (shows progress), then open file and print<br/>
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
  
  # Load facility choices on startup using db_helpers
  observe({
    tryCatch({
      # Get facility choices with full names for display
      facility_lookup <- get_facility_lookup()
      facility_choices <- setNames(facility_lookup$short_name, facility_lookup$full_name)
      facility_choices <- c("All Facilities" = "all", facility_choices)
      
      updateSelectInput(
        session,
        "filter_facility",
        choices = facility_choices
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error loading facility options:", e$message),
        type = "error",
        duration = NULL
      )
    })
  })
  
  # Update FOS and Section filters when facility changes (cascading)
  observeEvent(input$filter_facility, {
    tryCatch({
      # Get foreman lookup  
      fos_lookup <- get_foremen_lookup()
      
      # Filter FOS by selected facility
      if (input$filter_facility != "all") {
        fos_lookup <- fos_lookup %>% filter(facility == input$filter_facility)
      }
      
      # Create FOS choices with full names
      fos_choices <- setNames(sprintf("%04d", as.numeric(fos_lookup$emp_num)), fos_lookup$shortname)
      fos_choices <- c("All FOS Areas" = "all", fos_choices)
      
      updateSelectInput(
        session,
        "filter_fosarea",
        choices = fos_choices
      )
      
      # Update sections based on facility
      filter_opts <- get_filter_options(
        facility_filter = input$filter_facility,
        fosarea_filter = NULL
      )
      
      updateSelectInput(
        session,
        "filter_section",
        choices = c("All Sections" = "all", setNames(filter_opts$sections, filter_opts$sections))
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error updating filters:", e$message),
        type = "error"
      )
    })
  })
  
  # Update Section filter when FOS area changes (cascading)
  observeEvent(input$filter_fosarea, {
    tryCatch({
      # Update sections based on facility AND fosarea
      filter_opts <- get_filter_options(
        facility_filter = input$filter_facility,
        fosarea_filter = input$filter_fosarea
      )
      
      updateSelectInput(
        session,
        "filter_section",
        choices = c("All Sections" = "all", setNames(filter_opts$sections, filter_opts$sections))
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error updating section filter:", e$message),
        type = "error"
      )
    })
  })
  
  # Column ordering system
  column_choices <- reactiveVal(list(
    list(id = "date", label = "Date", selected = TRUE),
    list(id = "wet_pct", label = "Wet %", selected = TRUE),
    list(id = "emp_num", label = "Emp #", selected = TRUE),
    list(id = "num_dip", label = "#/Dip", selected = TRUE),
    list(id = "sample_num", label = "Sample #", selected = TRUE),
    list(id = "amt", label = "Amt", selected = TRUE),
    list(id = "mat", label = "Mat", selected = TRUE)
  ))
  
  # Render column ordering UI
  output$column_order_ui <- renderUI({
    cols <- column_choices()
    
    tags$div(
      id = "column_order_ui",
      style = "max-height: 300px; overflow-y: auto;",
      onscroll = "Shiny.setInputValue('column_scroll_position', this.scrollTop);",
      lapply(seq_along(cols), function(i) {
        col <- cols[[i]]
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 5px; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
          checkboxInput(
            paste0("col_check_", col$id),
            NULL,
            value = col$selected,
            width = "30px"
          ),
          tags$span(col$label, style = "flex-grow: 1; padding-left: 5px;"),
          actionButton(
            paste0("col_up_", col$id),
            "↑",
            style = "padding: 2px 8px; margin-right: 2px;",
            onclick = sprintf("Shiny.setInputValue('move_col_up', '%s', {priority: 'event'})", col$id)
          ),
          actionButton(
            paste0("col_down_", col$id),
            "↓",
            style = "padding: 2px 8px;",
            onclick = sprintf("Shiny.setInputValue('move_col_down', '%s', {priority: 'event'})", col$id)
          )
        )
      })
    )
  })
  
  # Store scroll position
  scroll_position <- reactiveVal(0)
  
  # Update column selected state
  observe({
    cols <- column_choices()
    changed <- FALSE
    for (i in seq_along(cols)) {
      col <- cols[[i]]
      check_val <- input[[paste0("col_check_", col$id)]]
      if (!is.null(check_val) && check_val != col$selected) {
        cols[[i]]$selected <- check_val
        changed <- TRUE
      }
    }
    if (changed) {
      column_choices(cols)
      # Restore scroll position after checkbox changes using JavaScript injection
      session$sendCustomMessage("restoreScroll", list(position = scroll_position()))
    }
  })
  
  # Capture scroll position before changes
  observeEvent(input$column_scroll_position, {
    scroll_position(input$column_scroll_position)
  })
  
  # Move column up
  observeEvent(input$move_col_up, {
    cols <- column_choices()
    col_id <- input$move_col_up
    idx <- which(sapply(cols, function(x) x$id == col_id))
    if (length(idx) > 0 && idx > 1) {
      cols[c(idx-1, idx)] <- cols[c(idx, idx-1)]
      column_choices(cols)
      # Restore scroll position after a brief delay
      session$sendCustomMessage("restoreScroll", list(position = scroll_position()))
    }
  })
  
  # Move column down
  observeEvent(input$move_col_down, {
    cols <- column_choices()
    col_id <- input$move_col_down
    idx <- which(sapply(cols, function(x) x$id == col_id))
    if (length(idx) > 0 && idx < length(cols)) {
      cols[c(idx, idx+1)] <- cols[c(idx+1, idx)]
      column_choices(cols)
      # Restore scroll position after a brief delay
      session$sendCustomMessage("restoreScroll", list(position = scroll_position()))
    }
  })
  
  # Generate cards when button is clicked
  observeEvent(input$generate_cards, {
    
    # Show loading overlay
    showModal(modalDialog(
      div(
        class = "loading-overlay",
        div(class = "loading-spinner"),
        div(class = "loading-text", "Generating section cards, please wait...")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Load full dataset only when Generate Cards is clicked
    tryCatch({
      data <- get_breeding_sites_with_sections()
      cards_data(data)
      
    }, error = function(e) {
      removeModal()
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
    
    # Apply section filter
    if (input$filter_section != "all") {
      filtered <- filtered %>% filter(section == input$filter_section)
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
      removeModal()
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
    
    # Get table fields from column ordering system (in order, selected only)
    cols <- column_choices()
    table_fields <- sapply(cols[sapply(cols, function(x) x$selected)], function(x) x$id)
    
    if (length(table_fields) == 0) {
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
      table_fields,
      input$num_rows,
      input$split_by_section,
      input$split_by_priority
    )
    
    # Render the cards
    output$section_cards <- renderUI({
      HTML(cards_html)
    })
    
    # Remove loading overlay
    removeModal()
    
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
      
      # Show progress modal
      showModal(modalDialog(
        div(
          class = "loading-overlay",
          div(class = "loading-spinner"),
          div(class = "loading-text", "Preparing download, please wait...")
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      
      # Small delay to show progress indicator
      Sys.sleep(0.5)
      
      # Apply same filters as generate cards
      filtered <- cards_data()
      
      if (input$filter_facility != "all") {
        filtered <- filtered %>% filter(facility == input$filter_facility)
      }
      if (input$filter_section != "all") {
        filtered <- filtered %>% filter(section == input$filter_section)
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
      
      # Get table fields from column ordering system (same as main generation)
      cols <- column_choices()
      table_fields <- sapply(cols[sapply(cols, function(x) x$selected)], function(x) x$id)
      
      if (length(table_fields) == 0) {
        removeModal()
        showNotification(
          "Please select at least one table column.",
          type = "warning"
        )
        return()
      }
      
      # Update progress
      showModal(modalDialog(
        div(
          class = "loading-overlay",
          div(class = "loading-spinner"),
          div(class = "loading-text", "Generating HTML content...")
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      
      # Generate HTML with progress tracking
      html_cards <- generate_section_cards_html(
        filtered,
        title_fields,
        table_fields,
        input$num_rows,
        input$split_by_section,
        input$split_by_priority
      )
      
      # Update progress
      showModal(modalDialog(
        div(
          class = "loading-overlay",
          div(class = "loading-spinner"),
          div(class = "loading-text", "Creating download file...")
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      
      # Create complete HTML document
      html_content <- paste0(
        '<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>Section Cards - ', format(Sys.Date(), "%Y-%m-%d"), '</title>
</head>
<body>
  ',
        html_cards,
        '
</body>
</html>'
      )
      
      writeLines(html_content, file)
      
      # Remove progress modal
      removeModal()
      
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
