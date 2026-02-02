# Unified Overview Dashboard
# =============================================================================
# SINGLE APP that handles all overview views based on URL parameters.
# Replaces separate district/ and facilities/ apps with one unified app.
#
# URL Scheme:
#   /overview/?view=district               - District overview (default)
#   /overview/?view=facility&zone=1        - Facility view for P1
#   /overview/?view=metric_detail&metric=drone&zone=1
#
# Drill-down Flow:
#   District (all metrics, by zone)
#     → Click P1 bar → Facility (all metrics, P1 only)
#       → Click drone bar → Metric Detail (drone only, P1, all facilities)
#         → Click SLP bar → Metric Detail (drone only, P1, SLP only)
#
# Back Navigation:
#   Breadcrumb trail at top allows navigation back up the hierarchy
# =============================================================================

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

# Source shared database helpers (adjust path since we're in unified/ subfolder)
source("../../../shared/db_helpers.R")
source("../../../shared/stat_box_helpers.R")

# Source overview framework - THE REGISTRY IS THE SINGLE SOURCE OF TRUTH
# (files are in parent directory)
source("../metric_registry.R")
source("../data_functions.R")
source("../display_functions.R")
source("../historical_functions.R")
source("../ui_helper.R")
source("../dynamic_ui.R")
source("../dynamic_server.R")
source("../url_router.R")  # NEW: URL-based navigation

# Set application name for AWS RDS monitoring
set_app_name("overview")

# =============================================================================
# STARTUP OPTIMIZATION: Preload lookup tables into cache
# =============================================================================
message("[overview] Preloading lookup tables...")
tryCatch({
  get_facility_lookup()
  get_foremen_lookup()
  message("[overview] Lookup tables preloaded")
}, error = function(e) message("[overview] Preload warning: ", e$message))

# Load environment variables
load_env_vars()

# =============================================================================
# USER INTERFACE - Dynamic based on URL parameters
# =============================================================================

ui <- function(request) {
  # Parse URL params from request (initial load)
  # Note: This runs on initial page load only
  # Dynamic updates are handled in server via input$url_changed
  
  fluidPage(
    # Include URL navigation JavaScript
    get_url_navigation_js(),
    get_breadcrumb_css(),
    get_overview_css(),
    
    # Dynamic title - will be updated by server
    div(id = "dynamic_header",
      div(class = "page-header",
        h1(id = "page_title", textOutput("page_title_text", inline = TRUE)),
        p(id = "page_subtitle", textOutput("page_subtitle_text", inline = TRUE))
      )
    ),
    
    # Breadcrumb navigation
    uiOutput("breadcrumb"),
    
    # Control panel
    div(class = "control-panel",
      style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
      fluidRow(
        column(2,
          dateInput("custom_today", "Analysis Date:",
                    value = Sys.Date(),
                    max = Sys.Date())
        ),
        column(2,
          sliderInput("expiring_days", "Expiring Days:",
                      min = 1, max = 30, value = 7, step = 1)
        ),
        column(2,
          selectInput("zone_filter", "Zone Filter:",
                      choices = c("All Zones" = "1,2",
                                  "P1 Only" = "1",
                                  "P2 Only" = "2",
                                  "Separate P1/P2" = "separate"),
                      selected = "1,2")
        ),
        column(2,
          selectInput("color_theme", "Color Theme:",
                      choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis"),
                      selected = "MMCD")
        ),
        column(2,
          # Metric filter - populated dynamically
          uiOutput("metric_filter_ui")
        ),
        column(2,
          actionButton("refresh", "Refresh Data",
                       class = "btn-primary",
                       style = "margin-top: 25px; width: 100%;"),
          # Back button - shown when drilled down
          uiOutput("back_button_ui")
        )
      )
    ),
    
    # Summary stats row
    uiOutput("summary_stats"),
    
    # Current progress charts (dynamic)
    div(id = "current_charts_container",
      uiOutput("current_charts")
    ),
    
    # Historical charts (dynamic)
    div(id = "historical_charts_container",
      uiOutput("historical_charts")
    ),
    
    # Footer with last updated
    div(class = "text-muted text-center",
      style = "margin-top: 20px; padding: 10px;",
      textOutput("last_updated")
    )
  )
}

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # =========================================================================
  # URL PARAMETER HANDLING
  # =========================================================================
  
  # Reactive to store current URL params
  url_params <- reactiveVal(get_default_url_params())
  
  # Process initial URL params
  observeEvent(input$initial_url_params, {
    params <- parse_url_params(input$initial_url_params)
    url_params(params)
    
    # Update UI to match URL params
    update_ui_from_params(session, params, input)
  }, ignoreNULL = TRUE, once = TRUE)
  
  # Handle URL changes (from navigation or browser back/forward)
  observeEvent(input$url_changed, {
    params <- parse_url_params(input$url_changed$url)
    url_params(params)
    
    # Update UI to match URL params
    update_ui_from_params(session, params, input)
    
    # Trigger data refresh using JavaScript (avoids shinyjs dependency)
    session$sendCustomMessage("triggerRefresh", list())
  }, ignoreNULL = TRUE)
  
  # Handle breadcrumb navigation
  observeEvent(input$nav_to, {
    navigate_to_url(session, input$nav_to)
  }, ignoreNULL = TRUE)
  
  # =========================================================================
  # HELPER: Update UI from URL params
  # =========================================================================
  
  update_ui_from_params <- function(session, params, input) {
    # Update zone filter
    updateSelectInput(session, "zone_filter", selected = params$zone)
    
    # Update date
    updateDateInput(session, "custom_today", value = as.Date(params$date))
    
    # Update expiring days
    updateSliderInput(session, "expiring_days", value = params$expiring)
    
    # Update theme
    updateSelectInput(session, "color_theme", selected = params$theme)
  }
  
  # =========================================================================
  # DYNAMIC UI ELEMENTS
  # =========================================================================
  
  # Page title based on view
  output$page_title_text <- renderText({
    get_view_title(url_params())
  })
  
  # Page subtitle
  output$page_subtitle_text <- renderText({
    get_view_subtitle(url_params())
  })
  
  # Breadcrumb navigation
  output$breadcrumb <- renderUI({
    generate_breadcrumb_ui(url_params(), session)
  })
  
  # Metric filter dropdown
  output$metric_filter_ui <- renderUI({
    registry <- get_metric_registry()
    choices <- c("All Metrics" = "all")
    for (id in names(registry)) {
      choices[registry[[id]]$display_name] <- id
    }
    
    current_metric <- url_params()$metric
    selected <- if (identical(current_metric, "all")) "all" else current_metric[1]
    
    selectInput("metric_filter", "Metric Filter:",
                choices = choices,
                selected = selected)
  })
  
  # Back button (shown when drilled down)
  output$back_button_ui <- renderUI({
    params <- url_params()
    if (params$view != "district") {
      back_url <- build_back_url(params)
      actionButton("back_btn", "← Back",
                   class = "btn-secondary",
                   style = "margin-top: 5px; width: 100%;",
                   onclick = sprintf(
                     "Shiny.setInputValue('nav_to', '%s', {priority: 'event'});",
                     back_url
                   ))
    }
  })
  
  # Handle metric filter changes
  observeEvent(input$metric_filter, {
    current <- url_params()
    if (input$metric_filter != current$metric[1]) {
      new_url <- build_drill_down_url(
        view = current$view,
        metric = input$metric_filter,
        zone = input$zone_filter,
        facility = current$facility,
        date = input$custom_today,
        expiring = input$expiring_days,
        theme = input$color_theme
      )
      navigate_to_url(session, new_url)
    }
  }, ignoreInit = TRUE)
  
  # =========================================================================
  # THEME SUPPORT
  # =========================================================================
  
  current_theme <- reactive({
    input$color_theme
  })
  
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
  
  # =========================================================================
  # DATA LOADING
  # =========================================================================
  
  # Get current overview config based on URL params
  current_overview_config <- reactive({
    get_overview_config(url_params()$view)
  })
  
  # Get metrics to display based on URL params
  display_metrics <- reactive({
    get_display_metrics(url_params())
  })
  
  # Refresh inputs
  refresh_inputs <- eventReactive(input$refresh, {
    zone_value <- isolate(input$zone_filter)
    
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
  
  # Current data loading
  current_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    params <- url_params()
    metrics <- display_metrics()
    registry <- get_metric_registry()
    overview_config <- current_overview_config()
    
    n_metrics <- length(metrics)
    
    withProgress(message = "Loading current data...", value = 0, {
      results <- list()
      for (i in seq_along(metrics)) {
        metric_id <- metrics[i]
        config <- registry[[metric_id]]
        if (is.null(config)) next
        
        setProgress(
          value = (i - 0.5) / n_metrics,
          detail = paste("Loading", config$display_name, "...")
        )
        
        # Use the correct load function based on view
        load_function <- overview_config$load_function
        
        results[[metric_id]] <- tryCatch({
          if (load_function == "load_data_by_zone") {
            load_data_by_zone(
              metric = metric_id,
              analysis_date = inputs$custom_today,
              expiring_days = inputs$expiring_days,
              zone_filter = inputs$zone_filter,
              separate_zones = inputs$separate_zones
            )
          } else if (load_function == "load_data_by_facility") {
            load_data_by_facility(
              metric = metric_id,
              analysis_date = inputs$custom_today,
              expiring_days = inputs$expiring_days,
              zone_filter = inputs$zone_filter,
              separate_zones = inputs$separate_zones,
              facility_filter = if (params$facility != "all") params$facility else NULL
            )
          } else {
            load_data_by_zone(
              metric = metric_id,
              analysis_date = inputs$custom_today,
              expiring_days = inputs$expiring_days,
              zone_filter = inputs$zone_filter,
              separate_zones = inputs$separate_zones
            )
          }
        }, error = function(e) {
          cat("ERROR loading current", metric_id, ":", e$message, "\n")
          data.frame()
        })
      }
      setProgress(value = 1, detail = "Complete!")
      results
    })
  })
  
  # Historical data loading
  historical_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    params <- url_params()
    metrics <- display_metrics()
    registry <- get_metric_registry()
    
    # Filter to only historical-enabled metrics
    hist_metrics <- metrics[metrics %in% get_historical_metrics()]
    n_metrics <- length(hist_metrics)
    
    if (n_metrics == 0) return(list())
    
    years <- get_historical_year_range(10, inputs$custom_today)
    
    withProgress(message = "Loading historical data...", value = 0, {
      results <- list()
      for (i in seq_along(hist_metrics)) {
        metric_id <- hist_metrics[i]
        config <- registry[[metric_id]]
        if (is.null(config)) next
        
        setProgress(
          value = (i - 0.5) / n_metrics,
          detail = paste("Loading", config$display_name, "history...")
        )
        
        results[[metric_id]] <- tryCatch({
          load_historical_comparison_data(
            metric = metric_id,
            start_year = years$start_year,
            end_year = years$end_year,
            display_metric = config$display_metric,
            zone_filter = inputs$zone_filter,
            analysis_date = inputs$custom_today,
            overview_type = params$view
          )
        }, error = function(e) {
          cat("ERROR loading historical", metric_id, ":", e$message, "\n")
          list(average = data.frame(), current = data.frame(), yearly_data = data.frame())
        })
      }
      setProgress(value = 1, detail = "Complete!")
      results
    })
  })
  
  # =========================================================================
  # DYNAMIC CHART GENERATION
  # =========================================================================
  
  # Generate summary stats
  output$summary_stats <- renderUI({
    req(current_data())
    generate_summary_stats(current_data())
  })
  
  # Generate current charts dynamically
  output$current_charts <- renderUI({
    req(current_data())
    metrics <- display_metrics()
    registry <- get_metric_registry()
    
    # Create chart panels for displayed metrics
    chart_panels <- lapply(metrics, function(metric_id) {
      config <- registry[[metric_id]]
      if (is.null(config)) return(NULL)
      
      column(if (length(metrics) == 1) 12 else 6,
        div(class = "chart-panel",
          h4(config$display_name),
          plotlyOutput(paste0(metric_id, "_chart"), height = "350px"),
          uiOutput(paste0(metric_id, "_legend"))
        )
      )
    })
    
    # Arrange in rows
    chart_rows <- split(chart_panels, ceiling(seq_along(chart_panels) / 2))
    tagList(lapply(chart_rows, function(row) do.call(fluidRow, row)))
  })
  
  # Generate historical charts dynamically
  output$historical_charts <- renderUI({
    req(historical_data())
    params <- url_params()
    
    if (!isTRUE(params$show_historical)) return(NULL)
    
    metrics <- display_metrics()
    hist_metrics <- metrics[metrics %in% get_historical_metrics()]
    registry <- get_metric_registry()
    
    if (length(hist_metrics) == 0) return(NULL)
    
    # Create chart panels for historical metrics
    chart_panels <- lapply(hist_metrics, function(metric_id) {
      config <- registry[[metric_id]]
      if (is.null(config)) return(NULL)
      
      column(if (length(hist_metrics) == 1) 12 else 6,
        div(class = "chart-panel historical-chart",
          h5(paste(config$display_name, "- Historical")),
          plotlyOutput(paste0(metric_id, "_historical_chart"), height = "300px")
        )
      )
    })
    
    # Arrange with header
    tagList(
      fluidRow(
        column(12,
          h3("Historical Trends", style = "margin-top: 30px; border-bottom: 2px solid #eee;")
        )
      ),
      lapply(split(chart_panels, ceiling(seq_along(chart_panels) / 2)), function(row) {
        do.call(fluidRow, row)
      })
    )
  })
  
  # =========================================================================
  # SETUP CHART OUTPUTS FOR EACH METRIC
  # =========================================================================
  
  # We need to create outputs for ALL possible metrics (they may be shown/hidden)
  registry <- get_metric_registry()
  all_metrics <- names(registry)
  
  # Setup current chart outputs
  lapply(all_metrics, function(metric_id) {
    local({
      local_metric_id <- metric_id
      local_config <- registry[[metric_id]]
      
      # Current chart
      output[[paste0(local_metric_id, "_chart")]] <- renderPlotly({
        req(current_data())
        data <- current_data()[[local_metric_id]]
        
        if (is.null(data) || nrow(data) == 0) {
          return(create_empty_chart(local_config$display_name, "No data available"))
        }
        
        params <- url_params()
        overview_config <- current_overview_config()
        
        # Choose chart function based on view
        chart_function <- if (params$view == "district") {
          create_zone_chart
        } else {
          create_overview_chart
        }
        
        # Create chart with click source for drill-down
        p <- chart_function(
          data = data,
          title = paste(local_config$display_name, "Progress"),
          y_label = local_config$y_label,
          theme = current_theme(),
          metric_type = local_metric_id
        )
        
        # Enable drill-down if configured
        if (isTRUE(overview_config$enable_drill_down)) {
          p <- p %>% event_register("plotly_click")
        }
        
        p
      })
      
      # Legend
      output[[paste0(local_metric_id, "_legend")]] <- renderUI({
        create_overview_legend(theme = current_theme(), metric_id = local_metric_id)
      })
      
      # Historical chart
      output[[paste0(local_metric_id, "_historical_chart")]] <- renderPlotly({
        req(historical_data())
        hist_data <- historical_data()[[local_metric_id]]
        
        if (is.null(hist_data)) {
          return(create_empty_chart(paste(local_config$display_name, "Historical"), "Loading..."))
        }
        
        params <- url_params()
        
        # Check if this metric uses yearly grouped chart
        if (isTRUE(local_config$historical_type == "yearly_grouped")) {
          if (is.null(hist_data$yearly_data) || nrow(hist_data$yearly_data) == 0) {
            return(create_empty_chart(
              paste(local_config$display_name, "Historical"),
              "No historical data available"
            ))
          }
          
          y_label <- if (local_config$has_acres) {
            paste(local_config$short_name, "Acres")
          } else {
            local_config$y_label
          }
          
          return(create_yearly_grouped_chart(
            data = hist_data$yearly_data,
            title = paste(local_config$display_name, "- Yearly Totals"),
            y_label = y_label,
            theme = current_theme(),
            overview_type = params$view
          ))
        }
        
        # Standard comparison chart
        if ((is.null(hist_data$average) || nrow(hist_data$average) == 0) &&
            (is.null(hist_data$current) || nrow(hist_data$current) == 0)) {
          return(create_empty_chart(
            paste(local_config$display_name, "Historical"),
            "No historical data available"
          ))
        }
        
        y_label <- if (local_config$has_acres) {
          paste(local_config$short_name, "Acres")
        } else {
          local_config$y_label
        }
        
        create_comparison_chart(
          avg_data = hist_data$average,
          current_data = hist_data$current,
          title = paste(local_config$display_name, "- Historical"),
          y_label = y_label,
          bar_color = local_config$bg_color,
          theme = current_theme(),
          ten_year_avg_data = hist_data$ten_year_average
        )
      })
    })
  })
  
  # =========================================================================
  # DRILL-DOWN CLICK HANDLERS
  # =========================================================================
  
  # Setup click handlers for each metric
  lapply(all_metrics, function(metric_id) {
    local({
      local_metric_id <- metric_id
      
      observeEvent(event_data("plotly_click", source = local_metric_id), {
        click_data <- event_data("plotly_click", source = local_metric_id)
        
        if (!is.null(click_data)) {
          params <- url_params()
          overview_config <- current_overview_config()
          
          if (isTRUE(overview_config$enable_drill_down)) {
            # Build drill-down URL
            drill_url <- build_click_drill_down_url(
              session = session,
              click_data = click_data,
              current_params = params,
              metric_id = local_metric_id,
              overview_config = overview_config
            )
            
            cat("[Drill-Down] Navigating to:", drill_url, "\n")
            navigate_to_url(session, drill_url)
          }
        }
      })
    })
  })
  
  # =========================================================================
  # LAST UPDATED
  # =========================================================================
  
  output$last_updated <- renderText({
    req(current_data())
    paste("Last updated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  })
}

# =============================================================================
# RUN THE APPLICATION
# =============================================================================

shinyApp(ui = ui, server = server)
