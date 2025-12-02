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
      facility_filter = isolate(input$facility_filter),
      foreman_filter = isolate(input$foreman_filter),
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
    
    list(
      zone_filter_raw = zone_value,
      zone_filter = parsed_zones,
      combine_zones = (zone_value == "combined"),
      facility_filter = isolate(input$facility_filter),
      foreman_filter = isolate(input$foreman_filter),
      group_by = isolate(input$group_by),
      hist_time_period = isolate(input$hist_time_period),
      hist_display_metric = isolate(input$hist_display_metric),
      hist_year_range = isolate(input$hist_year_range)
    )
  })
  
  # =============================================================================
  # DYNAMIC UI UPDATES - Populate filter dropdowns
  # =============================================================================
  
  # Initialize facility choices from db_helpers - runs immediately on app load
  observe({
    facility_choices <- get_facility_choices()
    # Filter to only N, E, MO, Sr, Sj, Wm, Wp as specified in query
    facilities <- get_facility_lookup()
    if (!is.null(facilities) && nrow(facilities) > 0) {
      facilities <- facilities %>%
        filter(short_name %in% c("N", "E", "MO", "Sr", "Sj", "Wm", "Wp"))
      facility_choices <- c("All" = "all", setNames(facilities$short_name, facilities$full_name))
    }
    updateSelectizeInput(session, "facility_filter", choices = facility_choices, selected = "all")
  })
  
  # Initialize foreman choices from db_helpers - runs immediately on app load
  observe({
    foremen_lookup <- get_foremen_lookup()
    foremen_choices <- c("All" = "all")
    foremen_choices <- c(
      foremen_choices,
      setNames(foremen_lookup$emp_num, foremen_lookup$shortname)
    )
    updateSelectizeInput(session, "foreman_filter", choices = foremen_choices, selected = "all")
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
      icon = icon("exclamation-triangle"),
      color = "yellow"
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
  
  # Status chart
  output$status_chart <- renderPlotly({
    data <- processed_data()
    inputs <- refresh_inputs()
    
    create_status_chart(data, inputs$group_by, inputs$expiring_filter)
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
  
  # Historical chart
  output$historical_chart <- renderPlotly({
    # Placeholder for now - will be implemented in historical_functions.R
    plot_ly() %>%
      layout(
        title = "Historical Analysis - Coming Soon",
        xaxis = list(title = "Time Period"),
        yaxis = list(title = "Count")
      )
  })
  
  # Historical table
  output$historical_table <- renderDT({
    # Placeholder for now
    datatable(
      data.frame(Message = "Historical data will appear here after implementation"),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # =============================================================================
  # INITIAL DATA LOAD
  # =============================================================================
  
  # Trigger initial data load
  observe({
    # Simulate refresh button click on startup
    updateActionButton(session, "refresh", label = "Refresh Data")
  })
  
  # Auto-refresh on startup (after UI is ready)
  observeEvent(session$clientData, {
    if (!is.null(session$clientData$url_hostname)) {
      shinyjs::delay(500, click("refresh"))
    }
  }, once = TRUE, ignoreInit = FALSE)
  
}

shinyApp(ui = ui, server = server)
