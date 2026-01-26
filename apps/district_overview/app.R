# District Overview Dashboard
# Top-level dashboard showing ALL MMCD aggregated by zone (P1 vs P2)
# Click on a zone bar to drill-down to Facilities Overview
# Displays: Catch Basin, Drone, Ground Prehatch, and Structure Treatment progress

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
set_app_name("district_overview")

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
    # "separate" = show P1 and P2 as separate bars
    # "1,2" = combine P1 and P2 into single totals
    # "1" or "2" = filter to that zone only
    if (zone_value == "separate") {
      zone_filter <- c("1", "2")
      separate_zones <- TRUE
    } else if (zone_value == "1,2") {
      zone_filter <- c("1", "2")
      separate_zones <- FALSE
    } else {
      zone_filter <- zone_value
      separate_zones <- FALSE
    }
    
    list(
      custom_today = isolate(input$custom_today),
      expiring_days = isolate(input$expiring_days),
      zone_filter = zone_filter,
      separate_zones = separate_zones
    )
  })
  
  # =============================================================================
  # DATA LOADING - Load data aggregated by ZONE (P1 vs P2)
  # =============================================================================
  
  district_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    cat("Loading district data for date:", as.character(inputs$custom_today), 
        ", zone_filter:", paste(inputs$zone_filter, collapse=","),
        ", separate:", inputs$separate_zones, "\n")
    
    withProgress(message = "Loading district overview...", value = 0, {
      
      # Load Catch Basin data aggregated by zone
      setProgress(value = 0.1, detail = "Loading catch basin data...")
      cb_data <- tryCatch({
        result <- load_data_by_zone(
          "catch_basin",
          analysis_date = inputs$custom_today,
          expiring_days = inputs$expiring_days,
          zone_filter = inputs$zone_filter,
          separate_zones = inputs$separate_zones
        )
        cat("Catch basin by zone rows:", ifelse(is.null(result), 0, nrow(result)), "\n")
        result
      }, error = function(e) {
        cat("ERROR in catch basin by zone:", e$message, "\n")
        warning(paste("Error loading catch basin data:", e$message))
        data.frame()
      })
      
      # Load Drone data aggregated by zone
      setProgress(value = 0.3, detail = "Loading drone data...")
      drone_data <- tryCatch({
        result <- load_data_by_zone(
          "drone",
          analysis_date = inputs$custom_today,
          expiring_days = inputs$expiring_days,
          zone_filter = inputs$zone_filter,
          separate_zones = inputs$separate_zones
        )
        cat("Drone by zone rows:", ifelse(is.null(result), 0, nrow(result)), "\n")
        result
      }, error = function(e) {
        cat("ERROR in drone by zone:", e$message, "\n")
        warning(paste("Error loading drone data:", e$message))
        data.frame()
      })
      
      # Load Ground Prehatch data aggregated by zone
      setProgress(value = 0.5, detail = "Loading ground prehatch data...")
      ground_data <- tryCatch({
        result <- load_data_by_zone(
          "ground_prehatch",
          analysis_date = inputs$custom_today,
          expiring_days = inputs$expiring_days,
          zone_filter = inputs$zone_filter,
          separate_zones = inputs$separate_zones
        )
        cat("Ground prehatch by zone rows:", ifelse(is.null(result), 0, nrow(result)), "\n")
        result
      }, error = function(e) {
        cat("ERROR in ground prehatch by zone:", e$message, "\n")
        warning(paste("Error loading ground prehatch data:", e$message))
        data.frame()
      })
      
      # Load Structure Treatment data aggregated by zone
      setProgress(value = 0.7, detail = "Loading structure data...")
      struct_data <- tryCatch({
        result <- load_data_by_zone(
          "structure",
          analysis_date = inputs$custom_today,
          expiring_days = inputs$expiring_days,
          zone_filter = inputs$zone_filter,
          separate_zones = inputs$separate_zones
        )
        cat("Structure by zone rows:", ifelse(is.null(result), 0, nrow(result)), "\n")
        result
      }, error = function(e) {
        cat("ERROR in structure by zone:", e$message, "\n")
        warning(paste("Error loading structure data:", e$message))
        data.frame()
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
  # OUTPUTS - Render charts with click handlers for drill-down
  # =============================================================================
  
  # Catch Basin Progress Chart
  output$catch_basin_chart <- renderPlotly({
    req(district_data())
    data <- district_data()$catch_basin
    
    if (is.null(data) || nrow(data) == 0) {
      return(create_empty_chart("Catch Basin Progress", "No data available"))
    }
    
    create_zone_chart(
      data = data,
      title = "Catch Basin Progress",
      y_label = "Wet Catch Basins",
      theme = current_theme(),
      chart_id = "catch_basin"
    )
  })
  
  # Drone Progress Chart
  output$drone_chart <- renderPlotly({
    req(district_data())
    data <- district_data()$drone
    
    if (is.null(data) || nrow(data) == 0) {
      return(create_empty_chart("Drone Progress", "No data available"))
    }
    
    create_zone_chart(
      data = data,
      title = "Drone Progress",
      y_label = "Drone Sites",
      theme = current_theme(),
      chart_id = "drone"
    )
  })
  
  # Ground Prehatch Progress Chart  
  output$ground_prehatch_chart <- renderPlotly({
    req(district_data())
    data <- district_data()$ground_prehatch
    
    if (is.null(data) || nrow(data) == 0) {
      return(create_empty_chart("Ground Prehatch Progress", "No data available"))
    }
    
    create_zone_chart(
      data = data,
      title = "Ground Prehatch Progress",
      y_label = "Prehatch Sites",
      theme = current_theme(),
      chart_id = "ground_prehatch"
    )
  })
  
  # Structure Treatment Progress Chart
  output$structure_chart <- renderPlotly({
    req(district_data())
    data <- district_data()$structure
    
    if (is.null(data) || nrow(data) == 0) {
      return(create_empty_chart("Structure Progress", "No data available"))
    }
    
    create_zone_chart(
      data = data,
      title = "Structure Progress",
      y_label = "Structures",
      theme = current_theme(),
      chart_id = "structure"
    )
  })
  
  # =============================================================================
  # DRILL-DOWN CLICK HANDLERS
  # =============================================================================
  
  # Handle clicks on any chart - navigate to facilities overview with zone filter
  observeEvent(event_data("plotly_click", source = "catch_basin"), {
    click_data <- event_data("plotly_click", source = "catch_basin")
    if (!is.null(click_data)) {
      zone_clicked <- click_data$x
      navigate_to_facilities(session, zone_clicked, input$custom_today, input$expiring_days)
    }
  })
  
  observeEvent(event_data("plotly_click", source = "drone"), {
    click_data <- event_data("plotly_click", source = "drone")
    if (!is.null(click_data)) {
      zone_clicked <- click_data$x
      navigate_to_facilities(session, zone_clicked, input$custom_today, input$expiring_days)
    }
  })
  
  observeEvent(event_data("plotly_click", source = "ground_prehatch"), {
    click_data <- event_data("plotly_click", source = "ground_prehatch")
    if (!is.null(click_data)) {
      zone_clicked <- click_data$x
      navigate_to_facilities(session, zone_clicked, input$custom_today, input$expiring_days)
    }
  })
  
  observeEvent(event_data("plotly_click", source = "structure"), {
    click_data <- event_data("plotly_click", source = "structure")
    if (!is.null(click_data)) {
      zone_clicked <- click_data$x
      navigate_to_facilities(session, zone_clicked, input$custom_today, input$expiring_days)
    }
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

#' Navigate to facilities overview with zone filter
#' @param session Shiny session
#' @param zone_clicked The zone that was clicked ("P1" or "P2")
#' @param analysis_date The current analysis date
#' @param expiring_days The current expiring days setting
navigate_to_facilities <- function(session, zone_clicked, analysis_date, expiring_days) {
  # Extract zone number from display name (e.g., "P1" -> "1")
  zone_num <- gsub("P", "", zone_clicked)
  
  # Build URL with parameters
  url <- paste0(
    "../facilities_overview/?zone=", zone_num,
    "&date=", as.character(analysis_date),
    "&expiring=", expiring_days
  )
  
  # Navigate using JavaScript
  session$sendCustomMessage("navigate", url)
}

# Run the application
shinyApp(ui = ui, server = server)
