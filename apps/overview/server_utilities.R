# Overview - Server Utilities
# =============================================================================
# Shared server functions for all overview dashboards.
# Handles data loading, chart rendering, and drill-down navigation.
# =============================================================================

#' Create a generic overview server
#' This is the main server function that can be customized for any overview type
#' 
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param overview_type Type of overview: "district", "facilities", "fos"
#' @param metrics Vector of metric IDs to load (NULL = all)
#' @export
create_overview_server <- function(input, output, session, 
                                   overview_type = "facilities",
                                   metrics = NULL) {
  
  # Get configuration
  config <- get_overview_config(overview_type)
  registry <- get_metric_registry()
  
  if (is.null(metrics)) {
    metrics <- get_active_metrics()
  }
  
  # ==========================================================================
  # THEME SUPPORT
  # ==========================================================================
  
  current_theme <- reactive({
    input$color_theme
  })
  
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
  
  # ==========================================================================
  # REFRESH BUTTON PATTERN
  # ==========================================================================
  
  refresh_inputs <- eventReactive(input$refresh, {
    zone_value <- isolate(input$zone_filter)
    
    # Parse zone filter
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
  
  # ==========================================================================
  # DATA LOADING
  # ==========================================================================
  
  overview_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    cat("Loading", overview_type, "data for date:", as.character(inputs$custom_today), 
        "separate_zones:", inputs$separate_zones, "\n")
    
    withProgress(message = paste("Loading", config$title, "..."), value = 0, {
      
      n_metrics <- length(metrics)
      results <- list()
      
      for (i in seq_along(metrics)) {
        metric_id <- metrics[i]
        metric_config <- registry[[metric_id]]
        
        setProgress(
          value = i / n_metrics, 
          detail = paste("Loading", metric_config$display_name, "...")
        )
        
        results[[metric_id]] <- tryCatch({
          if (config$group_by == "zone") {
            load_data_by_zone(
              metric = metric_id,
              analysis_date = inputs$custom_today,
              expiring_days = inputs$expiring_days,
              zone_filter = inputs$zone_filter,
              separate_zones = inputs$separate_zones
            )
          } else if (config$group_by == "facility") {
            load_data_by_facility(
              metric = metric_id,
              analysis_date = inputs$custom_today,
              expiring_days = inputs$expiring_days,
              zone_filter = inputs$zone_filter,
              separate_zones = inputs$separate_zones
            )
          } else if (config$group_by == "fos") {
            load_data_by_fos(
              metric = metric_id,
              analysis_date = inputs$custom_today,
              expiring_days = inputs$expiring_days,
              zone_filter = inputs$zone_filter,
              separate_zones = inputs$separate_zones
            )
          }
        }, error = function(e) {
          cat("ERROR loading", metric_id, ":", e$message, "\n")
          data.frame()
        })
        
        cat(metric_id, "rows:", 
            ifelse(is.null(results[[metric_id]]), 0, nrow(results[[metric_id]])), "\n")
      }
      
      setProgress(value = 1.0, detail = "Complete!")
      results
    })
  })
  
  # ==========================================================================
  # CHART OUTPUTS - Dynamically create outputs for each metric
  # ==========================================================================
  
  lapply(metrics, function(metric_id) {
    metric_config <- registry[[metric_id]]
    output_id <- paste0(metric_id, "_chart")
    
    output[[output_id]] <- renderPlotly({
      req(overview_data())
      data <- overview_data()[[metric_id]]
      
      if (is.null(data) || nrow(data) == 0) {
        return(create_empty_chart(
          paste(metric_config$display_name, "Progress"), 
          "No data available"
        ))
      }
      
      render_metric_chart(
        data = data,
        metric_config = metric_config,
        overview_type = overview_type,
        theme = current_theme()
      )
    })
  })
  
  # ==========================================================================
  # DRILL-DOWN CLICK HANDLERS (for district overview)
  # ==========================================================================
  
  if (config$enable_drill_down) {
    lapply(metrics, function(metric_id) {
      observeEvent(event_data("plotly_click", source = metric_id), {
        click_data <- event_data("plotly_click", source = metric_id)
        if (!is.null(click_data)) {
          zone_clicked <- click_data$x
          navigate_to_overview(
            session, 
            config$drill_down_target,
            zone_clicked, 
            input$custom_today, 
            input$expiring_days
          )
        }
      })
    })
  }
  
  # ==========================================================================
  # SUMMARY STATS
  # ==========================================================================
  
  # DISABLED: This conflicts with the main summary_stats in dynamic_server.R
  # output$summary_stats <- renderUI({
  #   req(overview_data())
  #   create_summary_stats_ui(overview_data(), registry)
  # })
  
  # ==========================================================================
  # LAST UPDATED
  # ==========================================================================
  
  output$last_updated <- renderText({
    req(overview_data())
    paste("Last updated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  })
  
  # Return the data reactive for use by specific apps if needed
  invisible(overview_data)
}


#' Handle URL parameters for drill-down navigation
#' Call this in your server function to process incoming URL parameters
#' 
#' @param session Shiny session
#' @param input Shiny input
#' @export
handle_url_parameters <- function(session, input) {
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$zone)) {
      zone_value <- query$zone
      if (zone_value %in% c("1", "2")) {
        updateSelectInput(session, "zone_filter", selected = zone_value)
      }
    }
    
    if (!is.null(query$date)) {
      tryCatch({
        date_value <- as.Date(query$date)
        updateDateInput(session, "custom_today", value = date_value)
      }, error = function(e) {
        cat("Invalid date parameter:", query$date, "\n")
      })
    }
    
    if (!is.null(query$expiring)) {
      exp_value <- as.integer(query$expiring)
      if (!is.na(exp_value) && exp_value >= 1 && exp_value <= 30) {
        updateSliderInput(session, "expiring_days", value = exp_value)
      }
    }
  })
}
