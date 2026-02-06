# Ground Prehatch Treatment Progress

# Load shared libraries and utilities
source("../../shared/app_libraries.R")
source("../../shared/server_utilities.R")
source("../../shared/db_helpers.R")
source("../../shared/stat_box_helpers.R")

# Source external function files
source("data_functions.R")
source("display_functions.R")
source("ui_helper.R")
source("historical_functions.R")

# Set application name for AWS RDS monitoring
set_app_name("ground_prehatch_progress")

# =============================================================================
# STARTUP OPTIMIZATION: Preload lookup tables into cache
# =============================================================================
message("[ground_prehatch_progress] Preloading lookup tables...")
tryCatch({
  get_facility_lookup()
  get_foremen_lookup()
  message("[ground_prehatch_progress] Lookup tables preloaded")
}, error = function(e) message("[ground_prehatch_progress] Preload warning: ", e$message))

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- ground_prehatch_ui()

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # Theme handling
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
    
    # Parse zone filter
    parsed_zones <- if (zone_value == "combined") {
      c("1", "2")  # Include both zones but will be combined
    } else if (zone_value == "1,2") {
      c("1", "2")  # Include both zones separately
    } else {
      zone_value  # Single zone
    }
    
    # Safely handle foreman_filter (can be NULL or empty with multiple=TRUE)
    foreman_val <- isolate(input$foreman_filter)
    if (is.null(foreman_val) || length(foreman_val) == 0) {
      foreman_val <- "all"
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
  
  # Update hist_display_metric when hist_time_period changes
  observeEvent(input$hist_time_period, {
    if (input$hist_time_period == "yearly") {
      updateRadioButtons(session, "hist_display_metric", selected = "sites")
    } else if (input$hist_time_period == "weekly") {
      updateRadioButtons(session, "hist_display_metric", selected = "weekly_active_count")
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
    
    # Safely handle foreman_filter (can be NULL or empty with multiple=TRUE)
    foreman_val <- isolate(input$foreman_filter)
    if (is.null(foreman_val) || length(foreman_val) == 0) {
      foreman_val <- "all"
    }
    
    list(
      zone_filter_raw = zone_value,
      zone_filter = parsed_zones,
      combine_zones = (zone_value == "combined"),
      facility_filter = isolate(input$facility_filter),
      foreman_filter = foreman_val,
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
    updateSelectInput(session, "facility_filter", choices = facility_choices, selected = "all")
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
  
  # Update FOS choices when facility changes
  observeEvent(input$facility_filter, {
    foremen_lookup <- get_foremen_lookup()
    
    if (input$facility_filter == "all") {
      # Show all FOS when "All Facilities" is selected
      foremen_choices <- c("All" = "all")
      foremen_choices <- c(
        foremen_choices,
        setNames(foremen_lookup$emp_num, foremen_lookup$shortname)
      )
    } else {
      # Filter FOS by selected facility
      filtered_foremen <- foremen_lookup[foremen_lookup$facility == input$facility_filter, ]
      foremen_choices <- c("All" = "all")
      if (nrow(filtered_foremen) > 0) {
        foremen_choices <- c(
          foremen_choices,
          setNames(filtered_foremen$emp_num, filtered_foremen$shortname)
        )
      }
    }
    
    updateSelectizeInput(session, "foreman_filter", choices = foremen_choices, selected = "all")
  })
  
  # Update chart type default when zone filter changes to P1 and P2 separate
  observeEvent(input$zone_filter, {
    # Only update if on historical tab and switching to P1 and P2 separate
    if (input$tabs == "historical" && input$zone_filter == "1,2") {
      # Default to grouped_bar but user can still change it
      updateSelectInput(session, "hist_chart_type", selected = "grouped_bar")
    }
  })

  # Fetch ground prehatch data, ONLY when refresh button clicked
  ground_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    # Use custom date if provided, otherwise use current date
    analysis_date <- if (!is.null(inputs$custom_today)) inputs$custom_today else Sys.Date()
    
    withProgress(message = "Loading ground prehatch data...", value = 0.5, {
      get_ground_prehatch_data(inputs$zone_filter, analysis_date, inputs$expiring_days)
    })
  })
  
  # Fetch site details data - ONLY when refresh button clicked
  site_details <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    # Use custom date if provided, otherwise use current date
    analysis_date <- if (!is.null(inputs$custom_today)) inputs$custom_today else Sys.Date()
    
    withProgress(message = "Loading site details...", value = 0.5, {
      get_site_details_data(inputs$expiring_days, analysis_date)
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
    display_metric <- input$display_metric  # Not isolated - changes trigger update
    create_value_boxes(data, display_metric = display_metric)
  })
  
  # Render metric boxes
  output$prehatch_sites <- renderUI({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    status_colors <- get_status_colors(theme = current_theme())
    metric_label <- if (input$display_metric == "acres") "Prehatch Acres" else "Prehatch Sites"
    create_stat_box(
      value = data$total_prehatch,
      title = metric_label,
      bg_color = status_colors["completed"],
      icon = icon("egg")
    )
  })
  
  output$treated_sites <- renderUI({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    status_colors <- get_status_colors(theme = current_theme())
    metric_label <- if (input$display_metric == "acres") "Active Acres" else "Active Sites"
    create_stat_box(
      value = data$total_active,
      title = metric_label,
      bg_color = status_colors["active"],
      icon = icon("check-circle")
    )
  })
  
  output$sites_expiring <- renderUI({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    status_colors <- get_status_colors(theme = current_theme())
    metric_label <- if (input$display_metric == "acres") "Acres Expiring" else "Sites Expiring"
    create_stat_box(
      value = data$total_expiring,
      title = metric_label,
      bg_color = status_colors["planned"],
      icon = icon("exclamation-triangle")
    )
  })
  
  output$expired_sites <- renderUI({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    status_colors <- get_status_colors(theme = current_theme())
    metric_label <- if (input$display_metric == "acres") "Expired Acres" else "Expired Sites"
    create_stat_box(
      value = data$total_expired,
      title = metric_label,
      bg_color = status_colors["unknown"],
      icon = icon("clock")
    )
  })
  
  output$skipped_sites <- renderUI({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    status_colors <- get_status_colors(theme = current_theme())
    metric_label <- if (input$display_metric == "acres") "Skipped Acres" else "Skipped Sites"
    create_stat_box(
      value = data$total_skipped,
      title = metric_label,
      bg_color = status_colors["needs_treatment"],
      icon = icon("ban")
    )
  })
  
  output$treated_pct <- renderUI({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    status_colors <- get_status_colors(theme = current_theme())
    create_stat_box(
      value = paste0(data$treated_pct, "%"),
      title = "Treated %",
      bg_color = status_colors["active"],
      icon = icon("percent")
    )
  })
  
  # Store chart height reactively
  chart_info <- reactive({
    req(input$refresh)  # Only calculate after refresh button clicked
    inputs <- refresh_inputs()
    data <- aggregated_data()
    display_metric <- input$display_metric  # Not isolated - changes trigger update
    create_progress_chart(data, inputs$group_by, inputs$expiring_filter, inputs$expiring_days, 
                         return_height_info = TRUE, theme = current_theme(), display_metric = display_metric)
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
    
    # Call create_historical_data with progress indicators
    withProgress(message = "Loading historical data...", value = 0, {
      incProgress(0.1, detail = "Connecting to database...")
      Sys.sleep(0.1)  # Brief pause for user to see progress
      
      incProgress(0.2, detail = "Loading ground site data...")
      Sys.sleep(0.1)
      
      incProgress(0.3, detail = "Loading treatment records...")
      Sys.sleep(0.1)
      
      incProgress(0.2, detail = "Applying filters...")
      Sys.sleep(0.1)
      
      incProgress(0.1, detail = "Aggregating data...")
      
      result <- create_historical_data(
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
      
      incProgress(0.1, detail = "Finalizing...")
      result
    })
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
      group_by = inputs$group_by,
      theme = current_theme(),
      show_zones_separately = !inputs$combine_zones
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