# Section Cards App

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
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

# Set application name for AWS RDS monitoring
set_app_name("section_cards")

# =============================================================================
# STARTUP OPTIMIZATION: Preload lookup tables into cache
# =============================================================================
message("[section_cards] Preloading lookup tables...")
tryCatch({
  get_facility_lookup()
  get_foremen_lookup()
  message("[section_cards] Lookup tables preloaded")
}, error = function(e) message("[section_cards] Preload warning: ", e$message))

# Load environment variables
load_env_vars()

# =============================================================================
# UI Definition
# =============================================================================

ui <- fluidPage(
  
  # Initialize shinyjs
  useShinyjs(),
  
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
      Shiny.addCustomMessageHandler('moveUp', function(message) {
        var element = $('[id$=\"_' + message.id + '\"]').closest('div[style*=\"display: flex\"]');
        var prev = element.prev();
        if (prev.length) {
          var container = $('#column_order_ui')[0];
          var scroll = container.scrollTop;
          element.insertBefore(prev);
          container.scrollTop = scroll;
        }
      });
      
      Shiny.addCustomMessageHandler('moveDown', function(message) {
        var element = $('[id$=\"_' + message.id + '\"]').closest('div[style*=\"display: flex\"]');
        var next = element.next();
        if (next.length) {
          var container = $('#column_order_ui')[0];
          var scroll = container.scrollTop;
          element.insertAfter(next);
          container.scrollTop = scroll;
        }
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
  
  # ==========================================================================
  # DYNAMIC COLUMN DISCOVERY from JK table
  # Discover extra columns beyond core at startup
  # ==========================================================================
  jk_dynamic_cols <- reactiveVal(character(0))
  
  observe({
    tryCatch({
      dyn_cols <- get_dynamic_columns()
      jk_dynamic_cols(dyn_cols)
      message(sprintf("[section_cards] Discovered %d dynamic columns: %s",
                      length(dyn_cols), paste(dyn_cols, collapse = ", ")))
    }, error = function(e) {
      message("[section_cards] Dynamic column discovery failed: ", e$message)
    })
  })
  
  # Dynamic title fields panel based on site type
  output$title_fields_panel <- renderUI({
    site_type <- input$site_type
    
    if (is.null(site_type) || site_type == "breeding") {
      # Air/Ground site fields - includes dynamic columns from JK table
      # [DB] options are filtered to only show columns that have data
      # for the currently selected facility/foreman filters
      
      # Static core choices
      static_choices <- list(
        "Priority" = "priority",
        "Acres" = "acres",
        "Type" = "type",
        "Culex" = "culex",
        "Spring Aedes" = "spr_aedes",
        "Perturbans" = "perturbans",
        "Prehatch" = "prehatch",
        "Prehatch Calculation" = "prehatch_calc",
        "Sample Site" = "sample",
        "Drone" = "drone",
        "Section" = "section",
        "Facility" = "facility",
        "Zone" = "zone",
        "Foreman" = "foreman",
        "Remarks" = "remarks"
      )
      
      # Add dynamic columns from JK table with [DB] prefix
      # Only show columns that have data for current facility/foreman filter
      dyn_cols <- jk_dynamic_cols()
      if (length(dyn_cols) > 0) {
        fac <- input$filter_facility
        fos <- input$filter_fosarea
        cols_with_data <- tryCatch(
          get_dynamic_cols_with_data(dyn_cols, facility_filter = fac, fosarea_filter = fos),
          error = function(e) dyn_cols  # fallback: show all on error
        )
        if (length(cols_with_data) > 0) {
          dyn_choices <- setNames(
            as.list(cols_with_data),
            paste0("[DB] ", sapply(cols_with_data, humanize_column_name))
          )
          all_choices <- c(static_choices, dyn_choices)
        } else {
          all_choices <- static_choices
        }
      } else {
        all_choices <- static_choices
      }
      
      # Preserve user's previous title_fields selections if they exist
      # Only use defaults on first render
      prev_selected <- isolate(input$title_fields)
      if (is.null(prev_selected)) {
        sel <- c("priority", "acres", "type", "remarks")
      } else {
        # Keep previous selections that are still valid choices
        sel <- intersect(prev_selected, unlist(all_choices))
      }
      
      wellPanel(
        h5("Title Section Fields"),
        p(class = "help-block", "Select fields to display in the card header (sitecode is always included)"),
        checkboxGroupInput(
          "title_fields",
          NULL,
          choices = all_choices,
          selected = sel
        )
      )
    } else {
      # Structure site fields
      wellPanel(
        h5("Title Section Fields"),
        p(class = "help-block", "Select fields to display in the card header (sitecode is always included)"),
        checkboxGroupInput(
          "title_fields",
          NULL,
          choices = list(
            "Priority" = "priority",
            "Structure Type" = "s_type",
            "Status (D/W/U)" = "status_udw",
            "Sq Ft" = "sqft",
            "Culex" = "culex",
            "Chambers (PR only)" = "chambers",
            "Section" = "section",
            "Remarks" = "remarks"
          ),
          selected = c("priority", "s_type", "status_udw", "sqft", "remarks")
        )
      )
    }
  })
  
  # Reset filters when site type changes
  observeEvent(input$site_type, {
    # Reset to defaults when switching site types
    updateSelectInput(session, "filter_facility", selected = "all")
    updateSelectInput(session, "filter_zone", selected = "all")
    updateSelectInput(session, "filter_fosarea", selected = "all")
    updateSelectInput(session, "filter_towncode", choices = c("All Town Codes" = "all"), selected = "all")
    updateSelectInput(session, "filter_section", choices = c("All Sections" = "all"), selected = "all")
    updateSelectInput(session, "filter_priority", selected = "all")
    
    # Clear generated cards when switching types
    cards_data(NULL)
  })
  
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
      
      # Initialize town code choices as empty - will be populated when facility is selected
      updateSelectInput(
        session,
        "filter_towncode",
        choices = c("All Town Codes" = "all")
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
      
      # Use site type specific functions for town codes and sections
      if (!is.null(input$site_type) && input$site_type == "structures") {
        # Structure-specific cascading
        town_codes <- get_structure_town_codes(facility_filter = input$filter_facility)
        sections <- get_structure_sections_by_towncode(
          towncode_filter = input$filter_towncode,
          facility_filter = input$filter_facility,
          fosarea_filter = NULL
        )
      } else {
        # Breeding site cascading
        town_codes <- get_town_codes(facility_filter = input$filter_facility)
        sections <- get_sections_by_towncode(
          towncode_filter = input$filter_towncode,
          facility_filter = input$filter_facility,
          fosarea_filter = NULL
        )
      }
      
      town_choices <- c("All Town Codes" = "all", setNames(town_codes, town_codes))
      updateSelectInput(session, "filter_towncode", choices = town_choices)
      
      updateSelectInput(
        session,
        "filter_section",
        choices = c("All Sections" = "all", setNames(sections, sections))
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error updating filters:", e$message),
        type = "error"
      )
    })
  })
  
  # Update Town Code and Section filters when FOS area changes (cascading)
  observeEvent(input$filter_fosarea, {
    tryCatch({
      # Use site type specific functions
      if (!is.null(input$site_type) && input$site_type == "structures") {
        town_codes <- get_structure_town_codes(
          facility_filter = input$filter_facility,
          fosarea_filter = input$filter_fosarea
        )
        sections <- get_structure_sections_by_towncode(
          towncode_filter = input$filter_towncode,
          facility_filter = input$filter_facility,
          fosarea_filter = input$filter_fosarea
        )
      } else {
        town_codes <- get_town_codes(
          facility_filter = input$filter_facility,
          fosarea_filter = input$filter_fosarea
        )
        sections <- get_sections_by_towncode(
          towncode_filter = input$filter_towncode,
          facility_filter = input$filter_facility,
          fosarea_filter = input$filter_fosarea
        )
      }
      
      town_choices <- c("All Town Codes" = "all", setNames(town_codes, town_codes))
      updateSelectInput(session, "filter_towncode", choices = town_choices)
      
      updateSelectInput(
        session,
        "filter_section",
        choices = c("All Sections" = "all", setNames(sections, sections))
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error updating section filter:", e$message),
        type = "error"
      )
    })
  })
  
  # Update Section filter when town code changes (cascading)
  observeEvent(input$filter_towncode, {
    tryCatch({
      # Use site type specific functions
      if (!is.null(input$site_type) && input$site_type == "structures") {
        sections <- get_structure_sections_by_towncode(
          towncode_filter = input$filter_towncode,
          facility_filter = input$filter_facility,
          fosarea_filter = input$filter_fosarea
        )
      } else {
        sections <- get_sections_by_towncode(
          towncode_filter = input$filter_towncode,
          facility_filter = input$filter_facility,
          fosarea_filter = input$filter_fosarea
        )
      }
      
      updateSelectInput(
        session,
        "filter_section",
        choices = c("All Sections" = "all", setNames(sections, sections))
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error updating section filter:", e$message),
        type = "error"
      )
    })
  })
  
  # Track column order - includes static table cols + dynamic DB cols
  # Dynamic cols are appended after discovery
  static_table_cols <- c("date", "wet_pct", "emp_num", "num_dip", "sample_num", "amt", "mat")
  column_order <- reactiveVal(static_table_cols)
  
  # Store checkbox states to preserve during re-rendering
  # Static cols default ON, dynamic cols default OFF
  checkbox_states <- reactiveVal(list(
    date = TRUE, wet_pct = TRUE, emp_num = TRUE, 
    num_dip = TRUE, sample_num = TRUE, amt = TRUE, mat = TRUE
  ))
  
  # When dynamic columns are discovered, add them to the column order and checkbox states
  observeEvent(jk_dynamic_cols(), {
    dyn_cols <- jk_dynamic_cols()
    if (length(dyn_cols) > 0) {
      current_order <- column_order()
      # Only add dynamic cols that aren't already in the order
      new_cols <- setdiff(dyn_cols, current_order)
      if (length(new_cols) > 0) {
        column_order(c(current_order, new_cols))
        # Add checkbox states for new dynamic cols (default OFF)
        current_states <- checkbox_states()
        for (col in new_cols) {
          current_states[[col]] <- FALSE
        }
        checkbox_states(current_states)
        message(sprintf("[section_cards] Added %d dynamic cols to table column order: %s",
                        length(new_cols), paste(new_cols, collapse = ", ")))
      }
    }
  })
  
  # Build column labels: static + dynamic with [DB] prefix
  get_col_labels <- function() {
    labels <- list(
      date = "Date", wet_pct = "Wet %", emp_num = "Emp #", 
      num_dip = "#/Dip", sample_num = "Sample #", amt = "Amt", mat = "Mat"
    )
    dyn_cols <- jk_dynamic_cols()
    for (col in dyn_cols) {
      labels[[col]] <- paste0("[DB] ", humanize_column_name(col))
    }
    return(labels)
  }
  
  # Render column ordering UI - updates when order changes
  # Dynamic [DB] cols are filtered to only show ones with data for current facility/foreman
  output$column_order_ui <- renderUI({
    col_order <- column_order()
    current_states <- checkbox_states()
    col_labels <- get_col_labels()
    dyn_cols <- jk_dynamic_cols()
    
    # Filter dynamic cols by data presence for current filters
    cols_with_data <- character(0)
    if (length(dyn_cols) > 0) {
      fac <- input$filter_facility
      fos <- input$filter_fosarea
      cols_with_data <- tryCatch(
        get_dynamic_cols_with_data(dyn_cols, facility_filter = fac, fosarea_filter = fos),
        error = function(e) dyn_cols  # fallback: show all on error
      )
    }
    
    # Only show static cols + dynamic cols that have data
    visible_order <- col_order[col_order %in% c(static_table_cols, cols_with_data)]
    
    tags$div(
      id = "column_order_ui",
      style = "max-height: 300px; overflow-y: auto;",
      lapply(seq_along(visible_order), function(i) {
        col_id <- visible_order[[i]]
        col_label <- if (col_id %in% names(col_labels)) col_labels[[col_id]] else humanize_column_name(col_id)
        # Alternating background colors
        bg_color <- if (i %% 2 == 1) "#f8f9fa" else "#e9ecef"
        
        tags$div(
          style = paste0("display: flex; align-items: center; margin-bottom: 2px; padding: 3px; ",
                        "border: 1px solid #ddd; border-radius: 3px; background-color: ", bg_color, ";"),
          checkboxInput(
            paste0("col_check_", col_id),
            NULL,
            value = if (col_id %in% names(current_states)) current_states[[col_id]] else FALSE,
            width = "30px"
          ),
          tags$span(col_label, style = "flex-grow: 1; padding-left: 3px; font-weight: 500;"),
          actionButton(
            paste0("col_up_", col_id),
            "↑",
            style = "padding: 1px 6px; margin-right: 2px; font-size: 12px;",
            onclick = sprintf("Shiny.setInputValue('move_col_up', '%s', {priority: 'event'})", col_id)
          ),
          actionButton(
            paste0("col_down_", col_id),
            "↓",
            style = "padding: 1px 6px; font-size: 12px;",
            onclick = sprintf("Shiny.setInputValue('move_col_down', '%s', {priority: 'event'})", col_id)
          )
        )
      })
    )
  })
  
  # Helper function to get current column selections in current order
  get_selected_columns <- function() {
    # Get current order
    current_order <- column_order()
    
    # Read current checkbox values and return selected IDs in current order
    selected_ids <- c()
    for (col_id in current_order) {
      checkbox_id <- paste0("col_check_", col_id)
      if (isTruthy(input[[checkbox_id]])) {
        selected_ids <- c(selected_ids, col_id)
      }
    }
    return(selected_ids)
  }
  
  # Update stored checkbox states when they change (covers all cols in current order)
  observe({
    current_states <- checkbox_states()
    col_ids <- column_order()
    
    for (col_id in col_ids) {
      checkbox_id <- paste0("col_check_", col_id)
      if (!is.null(input[[checkbox_id]])) {
        current_states[[col_id]] <- input[[checkbox_id]]
      }
    }
    checkbox_states(current_states)
  })
  
  # Move column up - change actual order
  observeEvent(input$move_col_up, {
    current_order <- column_order()
    col_id <- input$move_col_up
    col_index <- which(current_order == col_id)
    
    if (col_index > 1) {
      # Swap with previous item
      new_order <- current_order
      new_order[col_index - 1] <- current_order[col_index]
      new_order[col_index] <- current_order[col_index - 1]
      column_order(new_order)
    }
  })
  
  # Move column down - change actual order
  observeEvent(input$move_col_down, {
    current_order <- column_order()
    col_id <- input$move_col_down
    col_index <- which(current_order == col_id)
    
    if (col_index < length(current_order)) {
      # Swap with next item
      new_order <- current_order
      new_order[col_index + 1] <- current_order[col_index]
      new_order[col_index] <- current_order[col_index + 1]
      column_order(new_order)
    }
  })
  
  # Generate cards when button is clicked
  observeEvent(input$generate_cards, {
    
    # Use withProgress for real progress tracking
    withProgress(message = "Generating section cards...", value = 0, {
      
      setProgress(value = 0.05, detail = "Loading data from database...")
      
      # Load data based on site type
      tryCatch({
        if (!is.null(input$site_type) && input$site_type == "structures") {
          data <- get_structures_with_sections()
        } else {
          data <- get_breeding_sites_with_sections()
        }
        cards_data(data)
        
      }, error = function(e) {
        showNotification(
          paste("Error loading data:", e$message),
          type = "error",
          duration = NULL
        )
        return()
      })
      
      req(cards_data())
      
      setProgress(value = 0.15, detail = "Applying filters...")
      
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
    
    # Site type specific filters
    if (!is.null(input$site_type) && input$site_type == "structures") {
      # Structure-specific filters
      
      # Apply structure type filter
      if (!is.null(input$filter_structure_type) && input$filter_structure_type != "all") {
        st <- toupper(input$filter_structure_type)
        if (st == "CV") {
          filtered <- filtered %>% filter(toupper(s_type) == "CV" | grepl("CV/", toupper(s_type)) | grepl("/CV", toupper(s_type)))
        } else if (st == "PR") {
          filtered <- filtered %>% filter(toupper(s_type) == "PR" | grepl("PR/", toupper(s_type)) | grepl("/PR", toupper(s_type)))
        } else {
          filtered <- filtered %>% filter(toupper(s_type) == st)
        }
      }
      
      # Apply status (D/W/U) filter
      if (!is.null(input$filter_status_udw) && input$filter_status_udw != "all") {
        filtered <- filtered %>% filter(status_udw == input$filter_status_udw)
      }
      
    } else {
      # Breeding site specific filters
      
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
    
    # Apply town code filter (first 4 digits of sitecode)
    if (input$filter_towncode != "all") {
      filtered <- filtered %>% filter(substr(sitecode, 1, 4) == input$filter_towncode)
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
    
    setProgress(value = 0.25, detail = sprintf("Building %d cards...", nrow(filtered)))
    
    # Ensure sitecode is always in title fields
    title_fields <- c("sitecode", input$title_fields)
    
    # Get table fields from column ordering system (in order, selected only)
    table_fields <- get_selected_columns()
    
    if (length(table_fields) == 0) {
      showNotification(
        "Please select at least one table column.",
        type = "warning"
      )
      return()
    }
    
    # Generate the HTML with progress callback
    # split_by_type only applies to structures
    split_type <- if (!is.null(input$site_type) && input$site_type == "structures") {
      isTRUE(input$split_by_type)
    } else {
      FALSE
    }
    
    cards_html <- generate_section_cards_html(
      filtered,
      title_fields,
      table_fields,
      input$num_rows,
      input$split_by_section,
      input$split_by_priority,
      split_type,
      progress_fn = function(pct, detail) {
        setProgress(value = 0.25 + pct * 0.65, detail = detail)
      }
    )
    
    setProgress(value = 0.95, detail = "Rendering cards...")
    
    # Render the cards
    output$section_cards <- renderUI({
      HTML(cards_html)
    })
    
    setProgress(value = 1, detail = "Done!")
    
    }) # end withProgress
    
    showNotification(
      paste("Generated", nrow(cards_data()), "section cards"),
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
      
      withProgress(message = "Preparing download...", value = 0, {
      
      setProgress(value = 0.05, detail = "Applying filters...")
      
      # Apply same filters as generate cards
      filtered <- cards_data()
      
      if (input$filter_facility != "all") {
        filtered <- filtered %>% filter(facility == input$filter_facility)
      }
      if (input$filter_section != "all") {
        filtered <- filtered %>% filter(section == input$filter_section)
      }
      
      # Site type specific filters
      if (!is.null(input$site_type) && input$site_type == "structures") {
        # Structure-specific filters
        if (!is.null(input$filter_structure_type) && input$filter_structure_type != "all") {
          st <- toupper(input$filter_structure_type)
          if (st == "CV") {
            filtered <- filtered %>% filter(toupper(s_type) == "CV" | grepl("CV/", toupper(s_type)) | grepl("/CV", toupper(s_type)))
          } else if (st == "PR") {
            filtered <- filtered %>% filter(toupper(s_type) == "PR" | grepl("PR/", toupper(s_type)) | grepl("/PR", toupper(s_type)))
          } else {
            filtered <- filtered %>% filter(toupper(s_type) == st)
          }
        }
        if (!is.null(input$filter_status_udw) && input$filter_status_udw != "all") {
          filtered <- filtered %>% filter(status_udw == input$filter_status_udw)
        }
      } else {
        # Breeding site filters
        if (input$filter_air_gnd != "all") {
          filtered <- filtered %>% filter(air_gnd == input$filter_air_gnd)
        }
        if (input$filter_drone == "exclude") {
          filtered <- filtered %>% filter(is.na(drone) | drone == "")
        } else if (input$filter_drone == "only") {
          filtered <- filtered %>% filter(!is.na(drone) & drone != "")
        }
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
      if (input$filter_towncode != "all") {
        filtered <- filtered %>% filter(substr(sitecode, 1, 4) == input$filter_towncode)
      }
      
      title_fields <- c("sitecode", input$title_fields)
      
      # Get table fields from column ordering system (same as main generation)
      table_fields <- get_selected_columns()
      
      if (length(table_fields) == 0) {
        showNotification(
          "Please select at least one table column.",
          type = "warning"
        )
        return()
      }
      
      setProgress(value = 0.15, detail = sprintf("Generating HTML for %d cards...", nrow(filtered)))
      
      # Generate HTML with progress tracking
      # split_by_type only applies to structures
      split_type <- if (!is.null(input$site_type) && input$site_type == "structures") {
        isTRUE(input$split_by_type)
      } else {
        FALSE
      }
      
      html_cards <- generate_section_cards_html(
        filtered,
        title_fields,
        table_fields,
        input$num_rows,
        input$split_by_section,
        input$split_by_priority,
        split_type,
        progress_fn = function(pct, detail) {
          setProgress(value = 0.15 + pct * 0.70, detail = detail)
        }
      )
      
      setProgress(value = 0.90, detail = "Writing file...")
      
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
      
      setProgress(value = 1, detail = "Done!")
      
      }) # end withProgress
      
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
