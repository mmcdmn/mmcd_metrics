# Ground Prehatch Treatment Progress
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
source("ui_helpers.R")
source("historical_functions.R")

ui <- dashboardPage(
  dashboardHeader(title = "Ground Prehatch Treatment Progress"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Progress Overview", tabName = "overview", icon = icon("chart-bar")),
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
        
        # Section info panel
        create_section_info_panel(),
        
        # Summary statistics
        div(
          h4("Summary Statistics", style = "color: #3c8dbc; margin-bottom: 15px;"),
          create_overview_value_boxes()
        ),
        
        br(),
        
        # Progress chart
        create_progress_chart_box()
      ),
      
      # Details tab  
      tabItem(tabName = "details",
        br(),
        
        # Section info panel
        create_section_info_panel(),
        
        # Details table
        create_details_table_box()
      ),
      
      # Historical Analysis tab
      tabItem(tabName = "historical",
        br(),
        
        # Section info panel
        create_section_info_panel(),
        
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
  
  # Update hist_display_metric when hist_time_period changes
  observeEvent(input$hist_time_period, {
    if (input$hist_time_period == "yearly") {
      updateRadioButtons(session, "hist_display_metric", selected = "sites")
    } else if (input$hist_time_period == "weekly") {
      updateRadioButtons(session, "hist_display_metric", selected = "weekly_active_sites")
    }
  })
  
  # Historical refresh inputs - capture when historical refresh clicked
  hist_refresh_inputs <- eventReactive(input$hist_refresh, {
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
      hist_time_period = isolate(input$hist_time_period),
      hist_display_metric = isolate(input$hist_display_metric),
      hist_year_range = isolate(input$hist_year_range),
      hist_chart_type = isolate(input$hist_chart_type)
    )
  })
  
  # Initialize facility choices from db_helpers - runs immediately on app load
  observe({
    facility_choices <- get_facility_choices()
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
  
  # Update chart type default when zone filter changes to P1 and P2 separate
  observeEvent(input$zone_filter, {
    # Only update if on historical tab and switching to P1 and P2 separate
    if (input$sidebar_tabs == "historical" && input$zone_filter == "1,2") {
      # Default to grouped_bar but user can still change it
      updateSelectInput(session, "hist_chart_type", selected = "grouped_bar")
    }
  })

  # Fetch ground prehatch data, ONLY when refresh button clicked
  ground_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    # Use custom date if provided, otherwise use current date
    simulation_date <- if (!is.null(inputs$custom_today)) inputs$custom_today else Sys.Date()
    
    withProgress(message = "Loading ground prehatch data...", value = 0.5, {
      get_ground_prehatch_data(inputs$zone_filter, simulation_date)
    })
  })
  
  # Fetch site details data - ONLY when refresh button clicked
  site_details <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    # Use custom date if provided, otherwise use current date
    simulation_date <- if (!is.null(inputs$custom_today)) inputs$custom_today else Sys.Date()
    
    withProgress(message = "Loading site details...", value = 0.5, {
      get_site_details_data(inputs$expiring_days, simulation_date)
    })
  })
  
  # Filter data based on user selections
  filtered_data <- reactive({
    req(input$refresh)  # Require refresh button click
    inputs <- refresh_inputs()
    
    data <- ground_data()
    
    filter_ground_data(data, inputs$zone_filter, inputs$facility_filter, inputs$foreman_filter)
  })
  
  # Aggregate data based on grouping level  
  aggregated_data <- reactive({
    req(input$refresh)  # Require refresh button click
    inputs <- refresh_inputs()
    
    data <- filtered_data()
    site_data <- site_details()
    
    aggregate_data_by_group(
      data, 
      inputs$group_by, 
      inputs$zone_filter, 
      inputs$expiring_filter, 
      site_data,
      inputs$combine_zones
    )
  })
  
  # Details data for the table
  details_data <- reactive({
    req(input$refresh)  # Require refresh button click
    inputs <- refresh_inputs()
    
    site_data <- site_details()
    
    # Apply geographic filters
    filtered_data <- filter_ground_data(site_data, inputs$zone_filter, inputs$facility_filter, inputs$foreman_filter)
    
    # Apply expiring filter to site details
    if (inputs$expiring_filter == "expiring") {
      filtered_data <- filtered_data %>% filter(prehatch_status == "expiring")
    } else if (inputs$expiring_filter == "expiring_expired") {
      filtered_data <- filtered_data %>% filter(prehatch_status %in% c("expiring", "expired"))
    }
    
    return(filtered_data)
  })
  
  # Create value boxes
  value_boxes <- reactive({
    req(input$refresh)  # Require refresh button click
    
    data <- aggregated_data()
    create_value_boxes(data)
  })
  
  # Render value boxes using colors from db_helpers
  output$total_sites <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = data$total_ground,
      subtitle = "Total Ground Sites",
      icon = icon("map-marker"),
      color = shiny_colors["completed"]
    )
  })
  
  output$prehatch_sites <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = data$total_prehatch,
      subtitle = "Prehatch Sites",
      icon = icon("egg"),
      color = shiny_colors["planned"]
    )
  })
  
  output$treated_sites <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = data$total_active,
      subtitle = "Active Sites",
      icon = icon("check-circle"),
      color = shiny_colors["active"]
    )
  })
  
  output$sites_expiring <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = data$total_expiring,
      subtitle = "Sites Expiring",
      icon = icon("exclamation-triangle"),
      color = shiny_colors["needs_action"]
    )
  })
  
  output$expired_sites <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = data$total_expired,
      subtitle = "Expired Sites",
      icon = icon("clock"),
      color = shiny_colors["somthing_else"]
    )
  })
  
  output$skipped_sites <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = data$total_skipped,
      subtitle = "Skipped Sites",
      icon = icon("ban"),
      color = shiny_colors["needs_treatment"]
    )
  })
  
  output$treated_pct <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = paste0(data$treated_pct, "%"),
      subtitle = "Treated %",
      icon = icon("percent"),
      color = shiny_colors["active"]
    )
  })
  
  output$expiring_pct <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = paste0(data$expiring_pct, "%"),
      subtitle = "Expiring %",
      icon = icon("clock"),
      color = shiny_colors["needs_action"]
    )
  })
  
  # Store chart height reactively
  chart_info <- reactive({
    req(input$refresh)  # Only calculate after refresh button clicked
    inputs <- refresh_inputs()
    data <- aggregated_data()
    create_progress_chart(data, inputs$group_by, inputs$expiring_filter, inputs$expiring_days, return_height_info = TRUE)
  })
  
  # Render progress chart with dynamic height
  output$progress_chart <- renderPlotly({
    chart_result <- chart_info()
    if (is.list(chart_result) && "plot" %in% names(chart_result)) {
      chart_result$plot
    } else {
      chart_result
    }
  })
  
  # Render the chart UI with dynamic height
  output$dynamic_chart_ui <- renderUI({
    chart_result <- chart_info()
    height <- if (is.list(chart_result) && "height" %in% names(chart_result)) {
      paste0(chart_result$height, "px")
    } else {
      "600px"  # default fallback
    }
    
    plotlyOutput("progress_chart", height = height)
  })
  
  # Render details table
  output$details_table <- DT::renderDataTable({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- details_data()
    foremen_lookup <- get_foremen_lookup()
    create_details_table(data, foremen_lookup)
  })
  
  # Download handler for details data
  output$download_details_data <- downloadHandler(
    filename = function() {
      paste("ground_prehatch_details_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(input$refresh)  # Only allow download after refresh
      
      data <- details_data()
      foremen_lookup <- get_foremen_lookup()
      download_data <- prepare_download_data(data, foremen_lookup)
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  # =============================================================================
  # HISTORICAL ANALYSIS TAB
  # =============================================================================
  
  # Aggregate historical data for visualization - ONLY when hist_refresh clicked
  historical_aggregated <- eventReactive(input$hist_refresh, {
    inputs <- hist_refresh_inputs()
    
    # Determine zone display mode from zone_filter_raw
    hist_zone_display <- if(inputs$zone_filter_raw == "1,2") "show-both" else "combined"
    
    # Call create_historical_data directly
    create_historical_data(
      start_year = inputs$hist_year_range[1],
      end_year = inputs$hist_year_range[2],
      hist_time_period = inputs$hist_time_period,
      hist_display_metric = inputs$hist_display_metric,
      hist_group_by = inputs$group_by,
      hist_zone_display = hist_zone_display,
      facility_filter = inputs$facility_filter,
      zone_filter = inputs$zone_filter,
      foreman_filter = inputs$foreman_filter
    )
  })
  
  # Render historical chart
  output$historical_chart <- renderPlotly({
    req(input$hist_refresh)
    inputs <- hist_refresh_inputs()
    data <- historical_aggregated()
    
    create_historical_chart(
      data,
      chart_type = inputs$hist_chart_type,
      display_metric = inputs$hist_display_metric,
      time_period = inputs$hist_time_period,
      group_by = inputs$group_by
    )
  })
  
  # Render historical details table
  output$historical_details_table <- DT::renderDataTable({
    req(input$hist_refresh)
    data <- historical_aggregated()
    create_historical_details_table(data)
  })
  
  # Download handler for historical data
  output$download_historical_data <- downloadHandler(
    filename = function() {
      paste("ground_prehatch_historical_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(input$hist_refresh)
      data <- historical_aggregated()
      export_csv_safe(data, file)
    }
  )
}

shinyApp(ui = ui, server = server)