# Facilities Overview Dashboard
# Shows current progress from multiple apps in a single view, grouped by FACILITY
# Displays: Catch Basin, Drone, Ground Prehatch, and Structure Treatment progress
# Drill-down from District Overview - receives zone filter from URL parameter

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(tidyr)
  library(lubridate)
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")
source("../../shared/stat_box_helpers.R")

# Source external function files
source("data_functions.R")
source("display_functions.R")
source("ui_helper.R")

# Set application name for AWS RDS monitoring
set_app_name("facilities_overview")

# Load environment variables
load_env_vars()

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- district_overview_ui()

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # =============================================================================
  # URL PARAMETER HANDLING - For drill-down from District Overview
  # =============================================================================
  
  # Check for URL parameters on session start
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$zone)) {
      # Update zone filter based on URL parameter
      zone_value <- query$zone
      if (zone_value %in% c("1", "2")) {
        updateSelectInput(session, "zone_filter", selected = zone_value)
      }
    }
    
    if (!is.null(query$date)) {
      # Update analysis date based on URL parameter
      tryCatch({
        date_value <- as.Date(query$date)
        updateDateInput(session, "custom_today", value = date_value)
      }, error = function(e) {
        cat("Invalid date parameter:", query$date, "\n")
      })
    }
    
    if (!is.null(query$expiring)) {
      # Update expiring days based on URL parameter
      exp_value <- as.integer(query$expiring)
      if (!is.na(exp_value) && exp_value >= 1 && exp_value <= 30) {
        updateSliderInput(session, "expiring_days", value = exp_value)
      }
    }
  })
  
  # =============================================================================
  # THEME SUPPORT
  # =============================================================================
  
  current_theme <- reactive({
    input$color_theme
  })
  
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
  
  # =============================================================================
  # REFRESH BUTTON PATTERN - Capture all inputs when refresh clicked
  # =============================================================================
  
  refresh_inputs <- eventReactive(input$refresh, {
    zone_value <- isolate(input$zone_filter)
    
    # Parse zone filter and determine if zones should be shown separately
    # "separate" = show P1 and P2 as separate bars (e.g., "East (P1)", "East (P2)")
    # "1,2" = show both zones combined into single facility bars
    # "1" or "2" = single zone only
    
    separate_zones <- (zone_value == "separate")
    
    parsed_zones <- if (zone_value == "1,2" || zone_value == "separate") {
      c("1", "2")
    } else {
      zone_value
    }
    
    list(
      zone_filter_raw = zone_value,
      zone_filter = parsed_zones,
      combine_zones = (zone_value == "1,2"),
      separate_zones = separate_zones,
      custom_today = isolate(input$custom_today),
      expiring_days = isolate(input$expiring_days)
    )
  })
  
  # =============================================================================
  # DATA LOADING - Load data from all apps
  # =============================================================================
  
  # Load all facility data when refresh is clicked
  district_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    # Debug logging
    cat("Loading facilities data for date:", as.character(inputs$custom_today), 
        "separate_zones:", inputs$separate_zones, "\n")
    
    withProgress(message = "Loading facilities overview...", value = 0, {
      
      # Load Catch Basin data
      setProgress(value = 0.1, detail = "Loading catch basin data...")
      cb_data <- tryCatch({
        result <- load_data_by_facility(
          "catch_basin",
          analysis_date = inputs$custom_today,
          expiring_days = inputs$expiring_days,
          zone_filter = inputs$zone_filter,
          separate_zones = inputs$separate_zones
        )
        cat("Catch basin data rows:", ifelse(is.null(result), 0, nrow(result)), "\n")
        result
      }, error = function(e) {
        cat("ERROR in catch basin data:", e$message, "\n")
        warning(paste("Error loading catch basin data:", e$message))
        data.frame() # Return empty df instead of NULL
      })
      
      # Load Drone data
      setProgress(value = 0.3, detail = "Loading drone data...")
      drone_data <- tryCatch({
        result <- load_data_by_facility(
          "drone",
          analysis_date = inputs$custom_today,
          expiring_days = inputs$expiring_days,
          zone_filter = inputs$zone_filter,
          separate_zones = inputs$separate_zones
        )
        cat("Drone data rows:", ifelse(is.null(result), 0, nrow(result)), "\n")
        result
      }, error = function(e) {
        cat("ERROR in drone data:", e$message, "\n")
        warning(paste("Error loading drone data:", e$message))
        data.frame() # Return empty df instead of NULL
      })
      
      # Load Ground Prehatch data
      setProgress(value = 0.5, detail = "Loading ground prehatch data...")
      ground_data <- tryCatch({
        result <- load_data_by_facility(
          "ground_prehatch",
          analysis_date = inputs$custom_today,
          expiring_days = inputs$expiring_days,
          zone_filter = inputs$zone_filter,
          separate_zones = inputs$separate_zones
        )
        cat("Ground prehatch data rows:", ifelse(is.null(result), 0, nrow(result)), "\n")
        result
      }, error = function(e) {
        cat("ERROR in ground prehatch data:", e$message, "\n")
        warning(paste("Error loading ground prehatch data:", e$message))
        data.frame() # Return empty df instead of NULL
      })
      
      # Load Structure Treatment data
      setProgress(value = 0.7, detail = "Loading structure data...")
      struct_data <- tryCatch({
        result <- load_data_by_facility(
          "structure",
          analysis_date = inputs$custom_today,
          expiring_days = inputs$expiring_days,
          zone_filter = inputs$zone_filter,
          separate_zones = inputs$separate_zones
        )
        cat("Structure data rows:", ifelse(is.null(result), 0, nrow(result)), "\n")
        result
      }, error = function(e) {
        cat("ERROR in structure data:", e$message, "\n")
        warning(paste("Error loading structure data:", e$message))
        data.frame() # Return empty df instead of NULL
      })
      
      setProgress(value = 1.0, detail = "Complete!")
      
      list(
        catch_basin = cb_data,
        drone = drone_data,
        ground_prehatch = ground_data,
        structure = struct_data
      )
    })
  })
  
  # =============================================================================
  # OUTPUTS - Render charts
  # =============================================================================
  
  # Catch Basin Progress Chart
  output$catch_basin_chart <- renderPlotly({
    req(district_data())
    data <- district_data()$catch_basin
    inputs <- refresh_inputs()
    
    if (is.null(data) || nrow(data) == 0) {
      return(create_empty_chart("Catch Basin Progress", "No data available"))
    }
    
    create_overview_chart(
      data = data,
      title = "Catch Basin Progress",
      y_label = "Wet Catch Basins",
      theme = current_theme(),
      metric_type = "catch_basin"
    )
  })
  
  # Drone Progress Chart
  output$drone_chart <- renderPlotly({
    req(district_data())
    data <- district_data()$drone
    inputs <- refresh_inputs()
    
    if (is.null(data) || nrow(data) == 0) {
      return(create_empty_chart("Drone Progress", "No data available"))
    }
    
    create_overview_chart(
      data = data,
      title = "Drone Progress",
      y_label = "Drone Sites",
      theme = current_theme(),
      metric_type = "drone"
    )
  })
  
  # Ground Prehatch Progress Chart  
  output$ground_prehatch_chart <- renderPlotly({
    req(district_data())
    data <- district_data()$ground_prehatch
    inputs <- refresh_inputs()
    
    if (is.null(data) || nrow(data) == 0) {
      return(create_empty_chart("Ground Prehatch Progress", "No data available"))
    }
    
    create_overview_chart(
      data = data,
      title = "Ground Prehatch Progress",
      y_label = "Prehatch Sites",
      theme = current_theme(),
      metric_type = "ground_prehatch"
    )
  })
  
  # Structure Treatment Progress Chart
  output$structure_chart <- renderPlotly({
    req(district_data())
    data <- district_data()$structure
    inputs <- refresh_inputs()
    
    if (is.null(data) || nrow(data) == 0) {
      return(create_empty_chart("Structure Progress", "No data available"))
    }
    
    create_overview_chart(
      data = data,
      title = "Structure Progress",
      y_label = "Structures",
      theme = current_theme(),
      metric_type = "structure"
    )
  })
  
  # =============================================================================
  # SUMMARY STATS - District-wide totals
  # =============================================================================
  
  output$summary_stats <- renderUI({
    req(district_data())
    data <- district_data()
    
    # Calculate totals from each dataset
    cb_stats <- if (!is.null(data$catch_basin) && nrow(data$catch_basin) > 0) {
      list(
        total = sum(data$catch_basin$total, na.rm = TRUE),
        active = sum(data$catch_basin$active, na.rm = TRUE),
        pct = round(100 * sum(data$catch_basin$active, na.rm = TRUE) / 
                    max(1, sum(data$catch_basin$total, na.rm = TRUE)), 1)
      )
    } else {
      list(total = 0, active = 0, pct = 0)
    }
    
    drone_stats <- if (!is.null(data$drone) && nrow(data$drone) > 0) {
      list(
        total = sum(data$drone$total, na.rm = TRUE),
        active = sum(data$drone$active, na.rm = TRUE),
        pct = round(100 * sum(data$drone$active, na.rm = TRUE) / 
                    max(1, sum(data$drone$total, na.rm = TRUE)), 1)
      )
    } else {
      list(total = 0, active = 0, pct = 0)
    }
    
    ground_stats <- if (!is.null(data$ground_prehatch) && nrow(data$ground_prehatch) > 0) {
      list(
        total = sum(data$ground_prehatch$total, na.rm = TRUE),
        active = sum(data$ground_prehatch$active, na.rm = TRUE),
        pct = round(100 * sum(data$ground_prehatch$active, na.rm = TRUE) / 
                    max(1, sum(data$ground_prehatch$total, na.rm = TRUE)), 1)
      )
    } else {
      list(total = 0, active = 0, pct = 0)
    }
    
    struct_stats <- if (!is.null(data$structure) && nrow(data$structure) > 0) {
      list(
        total = sum(data$structure$total, na.rm = TRUE),
        active = sum(data$structure$active, na.rm = TRUE),
        pct = round(100 * sum(data$structure$active, na.rm = TRUE) / 
                    max(1, sum(data$structure$total, na.rm = TRUE)), 1)
      )
    } else {
      list(total = 0, active = 0, pct = 0)
    }
    
    # Create summary boxes using custom stat boxes from shared
    fluidRow(
      column(3,
        create_stat_box(
          value = paste0(cb_stats$pct, "%"),
          title = paste0("Catch Basins: ", format(cb_stats$active, big.mark = ","), 
                        " / ", format(cb_stats$total, big.mark = ","), " treated"),
          bg_color = "#667eea",
          icon = "tint"
        )
      ),
      column(3,
        create_stat_box(
          value = paste0(drone_stats$pct, "%"),
          title = paste0("Drone Sites: ", format(drone_stats$active, big.mark = ","), 
                        " / ", format(drone_stats$total, big.mark = ","), " treated"),
          bg_color = "#f5576c",
          icon = "helicopter"
        )
      ),
      column(3,
        create_stat_box(
          value = paste0(ground_stats$pct, "%"),
          title = paste0("Ground Prehatch: ", format(ground_stats$active, big.mark = ","), 
                        " / ", format(ground_stats$total, big.mark = ","), " treated"),
          bg_color = "#4facfe",
          icon = "seedling"
        )
      ),
      column(3,
        create_stat_box(
          value = paste0(struct_stats$pct, "%"),
          title = paste0("Structures: ", format(struct_stats$active, big.mark = ","), 
                        " / ", format(struct_stats$total, big.mark = ","), " treated"),
          bg_color = "#43e97b",
          icon = "building"
        )
      )
    )
  })
  
  # Last updated timestamp
  output$last_updated <- renderText({
    req(district_data())
    paste("Last updated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
