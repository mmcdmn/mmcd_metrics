# Catch Basin Status Dashboard
# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
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
source("../../shared/stat_box_helpers.R")

# Source external function files
source("data_functions.R")
source("display_functions.R")
source("ui_helper.R")
source("historical_functions.R")

ui <- fluidPage(
  # Use universal CSS from db_helpers for consistent text sizing
  get_universal_text_css(),
  
  # Add custom CSS for sidebar toggle and positioning
  tags$head(
    tags$style(HTML("
      .sidebar-toggle {
        position: fixed;
        top: 60px;
        left: 10px;
        z-index: 1000;
        background-color: #3c8dbc;
        color: white;
        border: none;
        padding: 10px 15px;
        cursor: pointer;
        border-radius: 4px;
        font-size: 18px;
      }
      .sidebar-toggle:hover {
        background-color: #357ca5;
      }
      .sidebar-collapsed {
        display: none !important;
      }
      /* Adjust main content when sidebar is hidden */
      .col-sm-9 {
        transition: width 0.3s;
      }
      .sidebar-collapsed ~ .col-sm-9 {
        width: 100%;
      }
      /* Move tabs to the right to avoid overlap with sidebar toggle button */
      .nav-tabs {
        margin-left: 50px;
      }
      /* Ensure stat boxes are responsive and don't get cut off */
      .col-sm-2 {
        min-width: 160px;
        overflow: hidden;
      }
      /* Responsive columns - adjust on smaller screens */
      @media (max-width: 1200px) {
        .col-sm-2 {
          flex: 0 0 calc(50% - 10px);
          max-width: calc(50% - 10px);
        }
      }
      @media (max-width: 768px) {
        .col-sm-2 {
          flex: 0 0 100%;
          max-width: 100%;
        }
      }
    "))
  ),
  
  # Sidebar toggle button
  tags$button(
    class = "sidebar-toggle",
    onclick = "$('.col-sm-3').toggleClass('sidebar-collapsed');",
    HTML("&#9776;")
  ),
  
  # Application title
  titlePanel("Catch Basin Status Dashboard"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      # Refresh button at top
      actionButton("refresh", "Refresh Data", 
                   icon = icon("refresh"),
                   class = "btn-primary btn-lg",
                   style = "width: 100%;"),
      p(style = "text-align: center; margin-top: 10px; font-size: 0.9em; color: #666;",
        "Click refresh to get data for selected filters"),
      
      hr(),
      
      # Color theme selector - dynamically populated from available themes
      selectInput("color_theme", "Color Theme:",
                  choices = get_available_themes(),
                  selected = "MMCD"),
      
      hr(),
      
      # Analysis Options
      h4("Analysis Options"),
      radioButtons("group_by", "Group by:",
                   choices = c("All MMCD" = "mmcd_all",
                              "Facility" = "facility", 
                              "FOS" = "foreman",
                              "Section" = "sectcode"),
                   selected = "facility"),
      
      hr(),
      
      # Filters section
      h4("Filters"),
      
      # Zone filter
      radioButtons("zone_filter", "Zone Display:",
                   choices = c("P1 Only" = "1", 
                              "P2 Only" = "2", 
                              "P1 and P2 Separate" = "1,2", 
                              "Combined P1+P2" = "combined"),
                   selected = "1,2"),
      
      hr(),
      
      # Facility filter (single-select)
      selectInput("facility_filter", "Facility:",
                  choices = c("All" = "all"),
                  selected = "all"),
      
      # FOS filter (multi-select)
      selectizeInput("foreman_filter", "FOS:",
                    choices = c("Loading..." = "LOADING"),
                    selected = NULL,
                    multiple = TRUE,
                    options = list(
                      placeholder = "Select FOS (empty = all)",
                      plugins = list('remove_button')
                    )),
      
      hr(),
      
      # Date and expiring options
      h4("Site Options"),
      dateInput("custom_today", "Pretend Today is:",
               value = Sys.Date(), 
               format = "yyyy-mm-dd"),
      
      sliderInput("expiring_days", "Days Until Expiring:",
                 min = 1, max = 60, value = 14, step = 1),
      
      radioButtons("expiring_filter", "Site Filter:",
                  choices = c("All Sites" = "all",
                             "Expiring Only" = "expiring", 
                             "Expiring + Expired" = "expiring_expired"),
                  selected = "all"),
      
      width = 3
    ),
    
    # Main panel with tabs
    mainPanel(
      tabsetPanel(id = "main_tabset",
        tabPanel("Status Overview",
          br(),
          # Summary statistics - uses create_catch_basin_overview_boxes() from stat_box_helpers
          div(
            h4("Summary Statistics", style = "color: #3c8dbc; margin-bottom: 15px;"),
            create_catch_basin_overview_boxes()
          ),
          br(),
          # Status chart
          create_status_chart_box()
        ),
        
        tabPanel("Detailed View",
          br(),
          # Details table
          create_details_table_box()
        ),
        
        tabPanel("Historical Analysis",
          br(),
          # Historical filters
          create_historical_filter_panel(),
          # Historical chart
          create_historical_chart_box(),
          br(),
          # Historical details table
          create_historical_details_table_box()
        )
      ),
      
      width = 9
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
  # SUMMARY STATISTICS - Custom colored boxes based on theme
  # =============================================================================
  
  output$stat_wet_cb <- renderUI({
    data <- catch_basin_data()
    colors <- get_status_colors(theme = current_theme())
    
    if (is.null(data) || nrow(data) == 0) {
      total <- 0
    } else {
      total <- sum(data$wet_cb_count, na.rm = TRUE)
    }
    
    create_stat_box(
      title = "Total Wet Catch Basins",
      value = format(total, big.mark = ","),
      bg_color = colors[["completed"]],  # Use completed/blue color from db_helpers
      icon = icon("tint")
    )
  })
  
  output$stat_treated <- renderUI({
    data <- catch_basin_data()
    colors <- get_status_colors(theme = current_theme())
    
    if (is.null(data) || nrow(data) == 0) {
      total <- 0
    } else {
      total <- sum(data$count_wet_activetrt, na.rm = TRUE)
    }
    
    create_stat_box(
      title = "Active Treatment",
      value = format(total, big.mark = ","),
      bg_color = colors[["active"]],  # Use active/green color from db_helpers
      icon = icon("check-circle")
    )
  })
  
  output$stat_percent <- renderUI({
    data <- catch_basin_data()
    colors <- get_status_colors(theme = current_theme())
    
    if (is.null(data) || nrow(data) == 0) {
      pct <- 0
    } else {
      total_wet <- sum(data$wet_cb_count, na.rm = TRUE)
      total_treated <- sum(data$count_wet_activetrt, na.rm = TRUE)
      pct <- if (total_wet > 0) (total_treated / total_wet) * 100 else 0
    }
    
    create_stat_box(
      title = "Coverage %",
      value = paste0(round(pct, 1), "%"),
      bg_color = colors[["active"]],  # Use active/green color from db_helpers
      icon = icon("percent")
    )
  })
  
  output$stat_expiring <- renderUI({
    data <- catch_basin_data()
    colors <- get_status_colors(theme = current_theme())
    
    if (is.null(data) || nrow(data) == 0) {
      total <- 0
    } else {
      total <- sum(data$count_wet_expiring, na.rm = TRUE)
    }
    
    create_stat_box(
      title = "Expiring",
      value = format(total, big.mark = ","),
      bg_color = colors[["planned"]],  # Use planned/orange color from db_helpers
      icon = icon("clock")
    )
  })
  
  output$stat_expired <- renderUI({
    data <- catch_basin_data()
    colors <- get_status_colors(theme = current_theme())
    
    if (is.null(data) || nrow(data) == 0) {
      total <- 0
    } else {
      total <- sum(data$count_wet_expired, na.rm = TRUE)
    }
    
    create_stat_box(
      title = "Expired",
      value = format(total, big.mark = ","),
      bg_color = colors[["needs_treatment"]],  # Use needs_treatment/red color from db_helpers
      icon = icon("times-circle")
    )
  })
  
  output$stat_needs_treatment <- renderUI({
    data <- catch_basin_data()
    colors <- get_status_colors(theme = current_theme())
    
    if (is.null(data) || nrow(data) == 0) {
      total <- 0
    } else {
      # Needs treatment = total - treated
      total_wet <- sum(data$wet_cb_count, na.rm = TRUE)
      total_treated <- sum(data$count_wet_activetrt, na.rm = TRUE)
      total <- total_wet - total_treated
    }
    
    create_stat_box(
      title = "Needs Treatment",
      value = format(total, big.mark = ","),
      bg_color = colors[["needs_action"]],  # Use needs_action color from db_helpers
      icon = icon("exclamation-circle")
    )
  })
  
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
