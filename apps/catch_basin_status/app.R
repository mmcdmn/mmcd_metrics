# Catch Basin Status Dashboard
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
source("ui_helper.R")
source("historical_functions.R")

ui <- dashboardPage(
  dashboardHeader(title = "Catch Basin Status"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Status Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Detailed View", tabName = "details", icon = icon("table")),
      menuItem("Historical Analysis", tabName = "historical", icon = icon("history"))
    )
  ),
  
  dashboardBody(
    # Use universal CSS from db_helpers for consistent text sizing
    get_universal_text_css(),
    
    # Filter panel - always visible
    create_filter_panel(),
    
    # Help text (collapsible)
    div(id = "help-section",
      tags$a(href = "#", onclick = "$(this).next().toggle(); return false;", 
             style = "color: #17a2b8; text-decoration: none; font-size: 14px;",
             HTML("<i class='fa fa-question-circle'></i> Show/Hide Help")),
      div(style = "display: none;",
        create_help_text()
      )
    ),
    
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
        br(),
        
        # Summary statistics
        div(
          h4("Summary Statistics", style = "color: #3c8dbc; margin-bottom: 15px;"),
          create_overview_value_boxes()
        ),
        
        br(),
        
        # Status chart
        create_status_chart_box()
      ),
      
      # Details tab  
      tabItem(tabName = "details",
        br(),
        
        # Details table
        create_details_table_box()
      ),
      
      # Historical tab
      tabItem(tabName = "historical",
        br(),
        
        # Historical filters
        create_historical_filter_panel(),
        
        # Historical chart
        create_historical_chart_box(),
        
        br(),
        
        # Historical details table
        create_historical_details_table_box()
      )
    )
  )
)

server <- function(input, output, session) {
  
  # =============================================================================
  # THEME SUPPORT - Reactive theme handling
  # =============================================================================
  
  # Reactive value for current theme
  current_theme <- reactive({
    input$color_theme
  })
  
  # Update global theme option when theme changes
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
  
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
    
    # Handle empty foreman_filter (NULL or empty vector should be treated as NULL)
    foreman_val <- isolate(input$foreman_filter)
    if (is.null(foreman_val) || length(foreman_val) == 0) {
      foreman_val <- NULL
    }
    
    list(
      zone_filter_raw = zone_value,
      zone_filter = parsed_zones,
      combine_zones = (zone_value == "combined"),
      facility_filter = isolate(input$facility_filter),
      foreman_filter = foreman_val,
      group_by = isolate(input$group_by),
      custom_today = isolate(input$custom_today),
      expiring_days = isolate(input$expiring_days),
      expiring_filter = isolate(input$expiring_filter)
    )
  })
  
  # Historical refresh inputs - capture when historical refresh clicked
  hist_refresh_inputs <- eventReactive(input$hist_refresh, {
    zone_value <- isolate(input$zone_filter)
    
    # Parse zone filter
    parsed_zones <- if (zone_value == "combined") {
      c("1", "2")
    } else if (zone_value == "1,2") {
      c("1", "2")
    } else {
      zone_value
    }
    
    # Get the display metric based on time period
    hist_time_period_val <- isolate(input$hist_time_period)
    hist_display_metric_val <- if (hist_time_period_val == "yearly") {
      if (!is.null(input$hist_display_metric_yearly)) {
        isolate(input$hist_display_metric_yearly)
      } else {
        "treatments"  # default for yearly
      }
    } else {
      if (!is.null(input$hist_display_metric_weekly)) {
        isolate(input$hist_display_metric_weekly)
      } else {
        "weekly_active_treatments"  # default for weekly
      }
    }
    
    # Handle empty foreman_filter (NULL or empty vector should be treated as NULL)
    foreman_val <- isolate(input$foreman_filter)
    if (is.null(foreman_val) || length(foreman_val) == 0) {
      foreman_val <- NULL
    }
    
    list(
      zone_filter_raw = zone_value,
      zone_filter = parsed_zones,
      combine_zones = (zone_value == "combined"),
      facility_filter = isolate(input$facility_filter),
      foreman_filter = foreman_val,
      group_by = isolate(input$group_by),
      hist_time_period = hist_time_period_val,
      hist_display_metric = hist_display_metric_val,
      hist_chart_type = isolate(input$hist_chart_type),
      hist_year_range = isolate(input$hist_year_range)
    )
  })
  
  # =============================================================================
  # DYNAMIC UI UPDATES - Populate filter dropdowns
  # =============================================================================
  
  # Initialize facility choices from db_helpers - runs immediately on app load
  observe({
    tryCatch({
      facility_choices <- get_facility_choices()
      facilities <- get_facility_lookup()
      if (!is.null(facilities) && nrow(facilities) > 0) {
        facility_choices <- c("All" = "all", setNames(facilities$short_name, facilities$full_name))
      }
      updateSelectInput(session, "facility_filter", choices = facility_choices, selected = "all")
    }, error = function(e) {
      warning(paste("Error initializing facility choices:", e$message))
      updateSelectInput(session, "facility_filter", choices = c("All" = "all"), selected = "all")
    })
  })
  
  # Update foreman choices based on selected facility
  observe({
    req(input$facility_filter)
    tryCatch({
      foremen_lookup <- get_foremen_lookup()
      foremen_choices <- c()  # Remove "All" option
      if (!is.null(foremen_lookup) && nrow(foremen_lookup) > 0) {
        if (input$facility_filter != "all") {
          filtered_foremen <- foremen_lookup[foremen_lookup$facility == input$facility_filter, ]
        } else {
          filtered_foremen <- foremen_lookup
        }
        if (nrow(filtered_foremen) > 0) {
          foremen_choices <- setNames(filtered_foremen$emp_num, filtered_foremen$shortname)
        }
      }
      updateSelectizeInput(session, "foreman_filter", choices = foremen_choices, selected = NULL)
    }, error = function(e) {
      warning(paste("Error initializing foreman choices:", e$message))
      updateSelectizeInput(session, "foreman_filter", choices = c(), selected = NULL)
    })
  })
  
  # =============================================================================
  # DATA LOADING - Reactive data sources
  # =============================================================================
  
  # Load catch basin status data
  catch_basin_data <- eventReactive(refresh_inputs(), {
    inputs <- refresh_inputs()
    
    withProgress(message = "Loading catch basin data...", value = 0.5, {
      data <- load_catch_basin_data(
        facility_filter = inputs$facility_filter,
        foreman_filter = inputs$foreman_filter,
        zone_filter = inputs$zone_filter,
        custom_today = inputs$custom_today,
        expiring_days = inputs$expiring_days
      )
    })
    
    return(data)
  })
  
  # Process data for display
  processed_data <- reactive({
    req(catch_basin_data())
    inputs <- refresh_inputs()
    
    data <- catch_basin_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame())
    }
    
    # Apply grouping and filtering
    processed <- process_catch_basin_data(
      data = data,
      group_by = inputs$group_by,
      combine_zones = inputs$combine_zones,
      expiring_filter = inputs$expiring_filter
    )
    
    return(processed)
  })
  
  # =============================================================================
  # OVERVIEW TAB - Value boxes and charts
  # =============================================================================
  
  # Summary value boxes
  output$total_wet_cb <- renderValueBox({
    data <- catch_basin_data()
    
    if (is.null(data) || nrow(data) == 0) {
      total <- 0
    } else {
      total <- sum(data$wet_cb_count, na.rm = TRUE)
    }
    
    valueBox(
      format(total, big.mark = ","),
      "Total Wet Catch Basins",
      icon = icon("tint"),
      color = "blue"
    )
  })
  
  output$total_treated <- renderValueBox({
    data <- catch_basin_data()
    
    if (is.null(data) || nrow(data) == 0) {
      total <- 0
    } else {
      total <- sum(data$count_wet_activetrt, na.rm = TRUE)
    }
    
    valueBox(
      format(total, big.mark = ","),
      "Wet CB with Active Treatment",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$percent_treated <- renderValueBox({
    data <- catch_basin_data()
    
    if (is.null(data) || nrow(data) == 0) {
      pct <- 0
    } else {
      total_wet <- sum(data$wet_cb_count, na.rm = TRUE)
      total_treated <- sum(data$count_wet_activetrt, na.rm = TRUE)
      pct <- if (total_wet > 0) (total_treated / total_wet) * 100 else 0
    }
    
    valueBox(
      paste0(round(pct, 1), "%"),
      "Treatment Coverage",
      icon = icon("percent"),
      color = if (pct >= 75) "green" else if (pct >= 50) "yellow" else "red"
    )
  })
  
  output$total_expiring <- renderValueBox({
    data <- catch_basin_data()
    
    if (is.null(data) || nrow(data) == 0) {
      total <- 0
    } else {
      total <- sum(data$count_wet_expiring, na.rm = TRUE)
    }
    
    valueBox(
      format(total, big.mark = ","),
      "Expiring",
      icon = icon("clock"),
      color = "orange"
    )
  })
  
  output$total_expired <- renderValueBox({
    data <- catch_basin_data()
    
    if (is.null(data) || nrow(data) == 0) {
      total <- 0
    } else {
      total <- sum(data$count_wet_expired, na.rm = TRUE)
    }
    
    valueBox(
      format(total, big.mark = ","),
      "Expired",
      icon = icon("times-circle"),
      color = "red"
    )
  })
  
  # DUPLICATE OUTPUTS REMOVED - these were causing the app to crash!
  # output$total_expiring and output$total_expired were defined twice
  
  # Status chart
  output$status_chart <- renderPlotly({
    data <- processed_data()
    inputs <- refresh_inputs()
    
    create_status_chart(data, inputs$group_by, inputs$expiring_filter, theme = current_theme())
  })
  
  # Dynamic chart height
  output$chart_ui <- renderUI({
    data <- processed_data()
    
    # Calculate height based on number of groups
    n_groups <- if (is.null(data) || nrow(data) == 0) 1 else nrow(data)
    height <- max(400, n_groups * 80)
    
    plotlyOutput("status_chart", height = paste0(height, "px"))
  })
  
  # =============================================================================
  # DETAILS TAB - Data table
  # =============================================================================
  
  output$details_table <- renderDT({
    data <- processed_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No data available"))
    }
    
    # Use the format_details_table function
    display_data <- format_details_table(data)
    
    datatable(
      display_data,
      options = list(
        pageLength = 25,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        scrollX = TRUE
      ),
      class = 'cell-border stripe',
      rownames = FALSE
    )
  })
  
  # =============================================================================
  # HISTORICAL TAB OUTPUTS
  # =============================================================================
  
  # Load historical data when hist_refresh clicked
  historical_data <- eventReactive(input$hist_refresh, {
    inputs <- hist_refresh_inputs()
    
    withProgress(message = 'Loading historical data...', value = 0, {
      incProgress(0.3, detail = "Querying database...")
      
      hist_data <- create_historical_cb_data(
        start_year = inputs$hist_year_range[1],
        end_year = inputs$hist_year_range[2],
        hist_time_period = inputs$hist_time_period,
        hist_display_metric = inputs$hist_display_metric,
        hist_group_by = inputs$group_by,
        hist_zone_display = if (inputs$combine_zones) "combined" else "show-both",
        facility_filter = inputs$facility_filter,
        zone_filter = inputs$zone_filter,
        foreman_filter = inputs$foreman_filter
      )
      
      incProgress(0.7, detail = "Processing data...")
      
      return(hist_data)
    })
  })
  
  # Historical chart
  output$historical_chart <- renderPlotly({
    data <- historical_data()
    inputs <- hist_refresh_inputs()
    
    create_historical_cb_chart(
      data = data,
      hist_time_period = inputs$hist_time_period,
      hist_display_metric = inputs$hist_display_metric,
      hist_group_by = inputs$group_by,
      chart_type = inputs$hist_chart_type,
      theme = current_theme()
    )
  })
  
  # Historical table
  output$historical_table <- renderDT({
    data <- historical_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(
        data.frame(Message = "No historical data available"),
        options = list(pageLength = 10),
        rownames = FALSE
      ))
    }
    
    display_data <- format_historical_cb_table(data)
    
    datatable(
      display_data,
      options = list(
        pageLength = 25,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        scrollX = TRUE
      ),
      class = 'cell-border stripe',
      rownames = FALSE
    )
  })
  

  
}

shinyApp(ui = ui, server = server)
