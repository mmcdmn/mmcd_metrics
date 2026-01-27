# Dynamic Server Logic for Overview Apps
# =============================================================================
# Generates all server logic dynamically from the metric registry.
# NO hardcoding of specific metrics - everything driven by get_metric_registry()
# =============================================================================

# =============================================================================
# DYNAMIC DATA LOADERS - Iterate through registry
# =============================================================================

#' Load current data for all metrics dynamically
#' @param inputs List with: custom_today, expiring_days, zone_filter, separate_zones
#' @param group_by One of: "zone", "facility", "fos"
#' @param progress_callback Optional function(metric_name, i, n) to update progress
#' @return Named list of data frames keyed by metric_id
#' @export
load_all_current_data <- function(inputs, group_by = "zone", progress_callback = NULL) {
  metrics <- get_active_metrics()
  n_metrics <- length(metrics)
  registry <- get_metric_registry()
  
  # Determine which load function to use
  load_fn <- switch(group_by,
    "zone" = load_data_by_zone,
    "facility" = load_data_by_facility,
    load_data_by_zone
  )
  
  results <- list()
  for (i in seq_along(metrics)) {
    metric_id <- metrics[i]
    config <- registry[[metric_id]]
    
    # Update progress if callback provided
    if (!is.null(progress_callback)) {
      progress_callback(config$display_name, i, n_metrics)
    }
    
    cat("Loading", metric_id, "for", group_by, "...\n")
    results[[metric_id]] <- tryCatch({
      load_fn(
        metric = metric_id,
        analysis_date = inputs$custom_today,
        expiring_days = inputs$expiring_days,
        zone_filter = inputs$zone_filter,
        separate_zones = if (!is.null(inputs$separate_zones)) inputs$separate_zones else FALSE
      )
    }, error = function(e) {
      cat("ERROR loading", metric_id, ":", e$message, "\n")
      data.frame()
    })
  }
  
  results
}

#' Load historical data for all metrics dynamically
#' @param overview_type One of: "district", "facilities", "fos"
#' @param zone_filter Vector of zones to include
#' @param progress_callback Optional function(metric_name, i, n) to update progress
#' @param analysis_date Date to use as "current year" for comparison
#' @return Named list of data frames keyed by metric_id
#' @export
load_all_historical_data <- function(overview_type, zone_filter = c("1", "2"), progress_callback = NULL, analysis_date = NULL) {
  metrics <- get_historical_metrics()
  n_metrics <- length(metrics)
  registry <- get_metric_registry()
  overview_config <- get_overview_config(overview_type)
  
  years <- get_historical_year_range(5, analysis_date)
  
  results <- list()
  for (i in seq_along(metrics)) {
    metric_id <- metrics[i]
    config <- registry[[metric_id]]
    
    # Update progress if callback provided
    if (!is.null(progress_callback)) {
      progress_callback(config$display_name, i, n_metrics)
    }
    
    cat("Loading historical", metric_id, "...\n")
    
    results[[metric_id]] <- tryCatch({
      load_historical_comparison_data(
        metric = metric_id,
        start_year = years$start_year,
        end_year = years$end_year,
        display_metric = config$display_metric,
        zone_filter = zone_filter,
        analysis_date = analysis_date
      )
    }, error = function(e) {
      cat("ERROR loading historical", metric_id, ":", e$message, "\n")
      list(average = data.frame(), current = data.frame(), total_count = 0)
    })
  }
  
  results
}

# =============================================================================
# DYNAMIC OUTPUT SETUP - Create renderPlotly for each metric
# =============================================================================

#' Setup current chart outputs for all metrics
#' @param output Shiny output object
#' @param data_reactive Reactive returning named list of data
#' @param theme_reactive Reactive returning current theme
#' @param chart_function Function to create charts (create_zone_chart or create_overview_chart)
#' @export
setup_current_chart_outputs <- function(output, data_reactive, theme_reactive, chart_function) {
  metrics <- get_active_metrics()
  registry <- get_metric_registry()
  
  lapply(metrics, function(metric_id) {
    config <- registry[[metric_id]]
    output_id <- paste0(metric_id, "_chart")
    
    # Use local() to capture the correct metric_id for each iteration
    local({
      local_metric_id <- metric_id
      local_config <- config
      
      output[[output_id]] <- renderPlotly({
        req(data_reactive())
        data <- data_reactive()[[local_metric_id]]
        
        if (is.null(data) || nrow(data) == 0) {
          return(create_empty_chart(local_config$display_name, "No data available"))
        }
        
        chart_function(
          data = data,
          title = paste(local_config$display_name, "Progress"),
          y_label = local_config$y_label,
          theme = theme_reactive(),
          metric_type = local_metric_id
        )
      })
    })
  })
  
  invisible(NULL)
}

#' Setup historical chart outputs for all metrics
#' @param output Shiny output object
#' @param data_reactive Reactive returning named list of historical data (each with $average and $current)
#' @param overview_type One of: "district", "facilities", "fos"
#' @export
setup_historical_chart_outputs <- function(output, data_reactive, overview_type) {
  metrics <- get_historical_metrics()
  registry <- get_metric_registry()
  
  lapply(metrics, function(metric_id) {
    config <- registry[[metric_id]]
    output_id <- paste0(metric_id, "_historical_chart")
    
    # Use local() to capture correct values
    local({
      local_metric_id <- metric_id
      local_config <- config
      
      output[[output_id]] <- renderPlotly({
        req(data_reactive())
        hist_data <- data_reactive()[[local_metric_id]]
        
        # Expect hist_data to be a list with $average and $current
        if (is.null(hist_data) || 
            (is.null(hist_data$average) || nrow(hist_data$average) == 0) &&
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
          title = paste(local_config$display_name, "- 5 Year Avg vs Current"),
          y_label = y_label,
          bar_color = local_config$bg_color,
          theme = current_theme()
        )
      })
    })
  })
  
  invisible(NULL)
}

# =============================================================================
# SUMMARY STATS GENERATOR
# =============================================================================

#' Generate summary stats UI from data
#' @param data Named list of data frames keyed by metric_id
#' @return fluidRow with stat boxes
#' @export
generate_summary_stats <- function(data) {
  metrics <- get_active_metrics()
  registry <- get_metric_registry()
  n_metrics <- length(metrics)
  col_width <- floor(12 / n_metrics)
  
  stat_boxes <- lapply(metrics, function(metric_id) {
    config <- registry[[metric_id]]
    metric_data <- data[[metric_id]]
    
    # Calculate stats
    if (!is.null(metric_data) && nrow(metric_data) > 0) {
      total <- sum(metric_data$total, na.rm = TRUE)
      active <- sum(metric_data$active, na.rm = TRUE)
      pct <- ceiling(100 * active / max(1, total))
    } else {
      total <- 0
      active <- 0
      pct <- 0
    }
    
    column(col_width,
      create_stat_box(
        value = paste0(pct, "%"),
        title = paste0(config$display_name, ": ", format(active, big.mark = ","),
                      " / ", format(total, big.mark = ","), " treated"),
        bg_color = config$bg_color,
        icon = config$icon
      )
    )
  })
  
  do.call(fluidRow, stat_boxes)
}

# =============================================================================
# COMPLETE SERVER BUILDER
# =============================================================================

#' Build a complete overview dashboard server
#' Call this function inside your server function to set up all outputs
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param overview_type One of: "district", "facilities", "fos"
#' @param include_historical Whether to include historical charts
#' @export
build_overview_server <- function(input, output, session, 
                                   overview_type = "district",
                                   include_historical = TRUE) {
  
  overview_config <- get_overview_config(overview_type)
  metrics <- get_active_metrics()
  registry <- get_metric_registry()
  
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
  # REFRESH INPUTS - Capture all inputs when refresh clicked
  # =========================================================================
  
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
  
  # =========================================================================
  # DATA LOADING WITH PROGRESS INDICATORS
  # =========================================================================
  
  # Current data loading with progress bar
  current_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    n_metrics <- length(metrics)
    
    withProgress(message = "Loading current data...", value = 0, {
      results <- list()
      for (i in seq_along(metrics)) {
        metric_id <- metrics[i]
        config <- registry[[metric_id]]
        
        setProgress(
          value = (i - 0.5) / n_metrics,
          detail = paste("Loading", config$display_name, "...")
        )
        
        # Use the correct load function based on overview type
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
              separate_zones = inputs$separate_zones
            )
          } else if (load_function == "load_data_by_fos") {
            load_data_by_fos(
              metric = metric_id,
              analysis_date = inputs$custom_today,
              expiring_days = inputs$expiring_days,
              zone_filter = inputs$zone_filter,
              separate_zones = inputs$separate_zones
            )
          } else {
            # Fallback
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
  
  # Historical data loading with progress bar
  historical_data <- if (include_historical) {
    eventReactive(input$refresh, {
      inputs <- refresh_inputs()
      years <- get_historical_year_range(5, inputs$custom_today)
      hist_metrics <- get_historical_metrics()
      n_metrics <- length(hist_metrics)
      
      withProgress(message = "Loading historical data...", value = 0, {
        results <- list()
        for (i in seq_along(hist_metrics)) {
          metric_id <- hist_metrics[i]
          config <- registry[[metric_id]]
          
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
              zone_filter = inputs$zone_filter
            )
          }, error = function(e) {
            cat("ERROR loading historical", metric_id, ":", e$message, "\n")
            list(average = data.frame(), current = data.frame())
          })
        }
        setProgress(value = 1, detail = "Complete!")
        results
      })
    })
  } else {
    reactive({ list() })
  }
  
  # =========================================================================
  # SETUP ALL CHART OUTPUTS
  # =========================================================================
  
  # Determine chart function based on overview type
  chart_function <- if (overview_type == "district") {
    create_zone_chart
  } else {
    create_overview_chart
  }
  
  # Setup current charts - each watches the current_data reactive
  lapply(metrics, function(metric_id) {
    local({
      local_metric_id <- metric_id
      local_config <- registry[[metric_id]]
      output_id <- paste0(local_metric_id, "_chart")
      
      output[[output_id]] <- renderPlotly({
        req(current_data())
        data <- current_data()[[local_metric_id]]
        
        if (is.null(data) || nrow(data) == 0) {
          return(create_empty_chart(local_config$display_name, "No data available"))
        }
        
        chart_function(
          data = data,
          title = paste(local_config$display_name, "Progress"),
          y_label = local_config$y_label,
          theme = current_theme(),
          metric_type = local_metric_id
        )
      })
    })
  })
  
  # Setup historical charts
  if (include_historical) {
    lapply(metrics, function(metric_id) {
      local({
        local_metric_id <- metric_id
        local_config <- registry[[metric_id]]
        output_id <- paste0(local_metric_id, "_historical_chart")
        
        output[[output_id]] <- renderPlotly({
          req(historical_data())
          hist_data <- historical_data()[[local_metric_id]]
          
          # DEBUG: Log what data we have
          cat("DEBUG Historical", local_metric_id, ":\n")
          cat("  - Average rows:", if (!is.null(hist_data$average)) nrow(hist_data$average) else "NULL", "\n")
          cat("  - Current rows:", if (!is.null(hist_data$current)) nrow(hist_data$current) else "NULL", "\n")
          if (!is.null(hist_data$average) && nrow(hist_data$average) > 0) {
            cat("  - Average label:", hist_data$average$group_label[1], "\n")
          }
          if (!is.null(hist_data$current) && nrow(hist_data$current) > 0) {
            cat("  - Current label:", hist_data$current$group_label[1], "\n")
          }
          
          if (is.null(hist_data) || 
              (is.null(hist_data$average) || nrow(hist_data$average) == 0) &&
              (is.null(hist_data$current) || nrow(hist_data$current) == 0)) {
            return(create_empty_chart(
              paste(local_config$display_name, "Historical"),
              "Loading..."
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
            title = paste(local_config$display_name, "- 5 Year Avg vs Current"),
            y_label = y_label,
            bar_color = local_config$bg_color,
            theme = current_theme()
          )
        })
      })
    })
  }
  
  # =========================================================================
  # DRILL-DOWN CLICK HANDLERS (for district overview)
  # =========================================================================
  
  if (overview_config$enable_drill_down) {
    lapply(metrics, function(metric_id) {
      observeEvent(event_data("plotly_click", source = metric_id), {
        click_data <- event_data("plotly_click", source = metric_id)
        if (!is.null(click_data)) {
          # For zone charts with flipped coordinates, use pointNumber to determine zone
          # pointNumber 0 = first bar = P1, pointNumber 1 = second bar = P2
          point_num <- click_data$pointNumber
          
          # Map pointNumber to zone
          if (point_num == 0) {
            zone_clicked <- "P1"
          } else if (point_num == 1) {
            zone_clicked <- "P2"  
          } else {
            # Fallback to original method
            zone_clicked <- click_data$y  # For flipped coordinates
          }
          
          cat("DEBUG: Click data - x:", click_data$x, "y:", click_data$y, "pointNumber:", point_num, "\n")
          cat("DEBUG: Determined zone_clicked:", zone_clicked, "\n")
          
          navigate_to_overview(
            session, 
            overview_config$drill_down_target,
            zone_clicked, 
            input$custom_today, 
            input$expiring_days,
            current_theme()
          )
        }
      })
    })
  }
  # =========================================================================
  # SUMMARY STATS OUTPUT
  # =========================================================================
  
  output$summary_stats <- renderUI({
    req(current_data())
    generate_summary_stats(current_data())
  })
  
  # =========================================================================
  # LAST UPDATED TIMESTAMP
  # =========================================================================
  
  output$last_updated <- renderText({
    req(current_data())
    paste("Last updated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  })
  
  # Return the reactives for external use if needed
  list(
    current_data = current_data,
    historical_data = historical_data,
    refresh_inputs = refresh_inputs
  )
}
