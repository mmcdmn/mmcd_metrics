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
load_all_historical_data <- function(overview_type, zone_filter = c("1", "2"), progress_callback = NULL, analysis_date = NULL, metrics_filter = NULL) {
  # Use filtered metrics if provided, otherwise get all historical metrics
  all_historical <- get_historical_metrics()
  metrics <- if (!is.null(metrics_filter)) {
    intersect(metrics_filter, all_historical)
  } else {
    all_historical
  }
  n_metrics <- length(metrics)
  registry <- get_metric_registry()
  overview_config <- get_overview_config(overview_type)
  
  # Use 10 years to calculate 10-year averages
  years <- get_historical_year_range(10, analysis_date)
  
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
        analysis_date = analysis_date,
        overview_type = overview_type
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
#' @param input Shiny input object for chart type selection
#' @export
setup_current_chart_outputs <- function(output, data_reactive, theme_reactive, chart_function, input = NULL) {
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
        
        # Check if this metric supports chart type selection
        chart_type <- "bar"  # default
        if (!is.null(input) && !is.null(local_config$chart_types) && length(local_config$chart_types) > 1) {
          chart_type_input <- input[[paste0(local_metric_id, "_chart_type")]]
          if (!is.null(chart_type_input)) {
            chart_type <- chart_type_input
          }
        }
        
        # Choose chart function based on chart type
        if (chart_type == "pie" && "pie" %in% local_config$chart_types) {
          create_overview_pie_chart(
            data = data,
            title = paste(local_config$display_name, "Progress"),
            theme = theme_reactive(),
            metric_type = local_metric_id
          )
        } else {
          chart_function(
            data = data,
            title = paste(local_config$display_name, "Progress"),
            y_label = local_config$y_label,
            theme = theme_reactive(),
            metric_type = local_metric_id
          )
        }
      })
    })
  })
  
  invisible(NULL)
}

#' Setup legend outputs for all current metrics
#' @param output Shiny output object
#' @param theme_reactive Reactive returning current theme
#' @export
setup_legend_outputs <- function(output, theme_reactive) {
  metrics <- get_active_metrics()
  registry <- get_metric_registry()
  
  lapply(metrics, function(metric_id) {
    config <- registry[[metric_id]]
    legend_id <- paste0(metric_id, "_legend")
    
    # Use local() to capture correct values
    local({
      local_metric_id <- metric_id
      
      output[[legend_id]] <- renderUI({
        create_overview_legend(theme = theme_reactive(), metric_id = local_metric_id)
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
      local_overview_type <- overview_type
      
      output[[output_id]] <- renderPlotly({
        req(data_reactive())
        hist_data <- data_reactive()[[local_metric_id]]
        
        # Expect hist_data to be a list with $average and $current OR $yearly_data
        if (is.null(hist_data) || 
            (is.null(hist_data$average) || nrow(hist_data$average) == 0) &&
            (is.null(hist_data$current) || nrow(hist_data$current) == 0) &&
            (is.null(hist_data$yearly_data) || nrow(hist_data$yearly_data) == 0)) {
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
        
        # Check if this metric uses yearly grouped chart
        if (isTRUE(local_config$historical_type == "yearly_grouped")) {
          # Use yearly grouped bar chart
          create_yearly_grouped_chart(
            data = hist_data$yearly_data,
            title = paste(local_config$display_name, "- Yearly Totals"),
            y_label = y_label,
            theme = current_theme(),
            overview_type = local_overview_type
          )
        } else {
          # Use standard comparison chart with 5-year and 10-year averages
          create_comparison_chart(
            avg_data = hist_data$average,
            current_data = hist_data$current,
            title = paste(local_config$display_name, "- Historical"),
            y_label = y_label,
            bar_color = local_config$bg_color,
            theme = current_theme(),
            ten_year_avg_data = hist_data$ten_year_average
          )
        }
      })
    })
  })
  
  invisible(NULL)
}

# =============================================================================
# SUMMARY STATS GENERATOR
# =============================================================================

#' Generate summary stats UI from data with clickable value boxes
#' @param data Named list of data frames keyed by metric_id
#' @param metrics_filter Optional filter for which metrics to display
#' @param overview_type Type of overview (district, facilities, fos)
#' @return fluidRow with clickable stat boxes that toggle chart visibility
#' @export
generate_summary_stats <- function(data, metrics_filter = NULL, overview_type = "district") {
  # Use filtered metrics if provided, otherwise get all active metrics
  metrics <- if (!is.null(metrics_filter)) metrics_filter else get_active_metrics()
  registry <- get_metric_registry()
  
  # For facilities view, generate one value box per facility instead of per metric
  if (overview_type == "facilities" && length(metrics) > 0) {
    # Get unique facilities from the data
    all_facilities <- unique(unlist(lapply(metrics, function(metric_id) {
      metric_data <- data[[metric_id]]
      if (!is.null(metric_data) && "facility" %in% names(metric_data)) {
        unique(metric_data$facility)
      } else {
        NULL
      }
    })))
    
    if (length(all_facilities) == 0) return(fluidRow())
    
    n_facilities <- length(all_facilities)
    # For 7 facilities, use 4 columns in first row, 3 in second (col_width = 3)
    # For other counts, distribute evenly
    col_width <- if (n_facilities == 7) 3 else floor(12 / min(n_facilities, 6))
    
    stat_boxes <- lapply(all_facilities, function(fac) {
      # For this facility, aggregate across all metrics
      total_all <- 0
      active_all <- 0
      expiring_all <- 0
      
      for (metric_id in metrics) {
        metric_data <- data[[metric_id]]
        if (!is.null(metric_data) && "facility" %in% names(metric_data)) {
          fac_data <- metric_data[metric_data$facility == fac, ]
          if (nrow(fac_data) > 0) {
            total_all <- total_all + sum(fac_data$total, na.rm = TRUE)
            active_all <- active_all + sum(fac_data$active, na.rm = TRUE)
            expiring_all <- expiring_all + sum(fac_data$expiring, na.rm = TRUE)
          }
        }
      }
      
      # For cattail, calculate % treated out of (treated + needs treatment)
      # treated = active - expiring (active includes expiring)
      # For metrics with display_as_average, show percentage: current vs avg
      # For other metrics, use active / total
      config <- registry[[metrics[1]]]
      
      if (metrics[1] == "cattail_treatments") {
        treated_all <- active_all - expiring_all
        workload <- treated_all + expiring_all
        pct <- if (workload > 0) ceiling(100 * treated_all / workload) else 0
      } else if (isTRUE(config$display_as_average)) {
        # For display_as_average metrics: current / avg * 100
        pct <- if (total_all > 0) round(100 * active_all / total_all, 1) else 0
      } else {
        pct <- if (total_all > 0) ceiling(100 * active_all / total_all) else 0
      }
      
      # Create stat box with facility short name
      column(col_width,
        div(
          class = "stat-box-clickable",
          `data-facility` = fac,
          create_stat_box(
            value = paste0(pct, "%"),
            title = fac,  # Just show facility short name
            bg_color = config$bg_color,
            icon = NULL,  # No icon for facility view
            icon_type = "fontawesome"
          )
        )
      )
    })
    
    fluidRow(stat_boxes)
    
  } else {
    # District view: metrics grouped by category
    categories <- get_metric_categories()
    metrics_by_cat <- get_metrics_grouped_by_category()
    
    # Build category sections
    category_sections <- lapply(categories, function(cat) {
      cat_metrics <- intersect(metrics_by_cat[[cat]], metrics)
      if (length(cat_metrics) == 0) return(NULL)
      
      # Calculate column width based on number of metrics in this category
      n_metrics <- length(cat_metrics)
      col_width <- floor(12 / n_metrics)
      
      stat_boxes <- lapply(cat_metrics, function(metric_id) {
        config <- registry[[metric_id]]
        metric_data <- data[[metric_id]]
        
        # Calculate stats
        if (!is.null(metric_data) && nrow(metric_data) > 0) {
          total <- sum(metric_data$total, na.rm = TRUE)
          active <- sum(metric_data$active, na.rm = TRUE)
          expiring <- sum(metric_data$expiring, na.rm = TRUE)
          
          # For cattail_treatments:
          # treated = active - expiring (active includes expiring in this dataset)
          # needs_treatment = expiring
          # Percentage = treated / (treated + needs_treatment)
          # For display_as_average metrics: show percentage (current / avg * 100)
          if (metric_id == "cattail_treatments") {
            treated <- active - expiring
            needs_treatment <- expiring
            workload <- treated + needs_treatment
            pct <- if (workload > 0) round(100 * treated / workload, 1) else 0
          } else if (isTRUE(config$display_as_average)) {
            # For display_as_average metrics: current / avg * 100
            pct <- if (total > 0) round(100 * active / total, 1) else 0
          } else {
            pct <- ceiling(100 * active / max(1, total))
          }
        } else {
          pct <- 0
        }
        
        # Create clickable stat box with data-metric-id attribute
        column(col_width,
          div(
            class = "stat-box-clickable",
            `data-metric-id` = metric_id,
            create_stat_box(
              value = paste0(pct, "%"),
              title = config$display_name,
              bg_color = config$bg_color,
              icon = if (!is.null(config$image_path)) config$image_path else config$icon,
              icon_type = if (!is.null(config$image_path)) "image" else "fontawesome"
            )
          )
        )
      })
      
      # Return category section with header and metrics row
      div(class = "category-section",
        style = "margin-bottom: 15px;",
        div(class = "category-header",
          style = "font-size: 14px; font-weight: bold; color: #666; margin-bottom: 8px; padding-left: 5px; border-left: 3px solid #2c5aa0;",
          cat
        ),
        fluidRow(stat_boxes)
      )
    })
    
    # Filter out NULL sections and wrap in a container
    category_sections <- Filter(Negate(is.null), category_sections)
    div(class = "metrics-by-category", category_sections)
  }
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
                                   include_historical = TRUE,
                                   metrics_filter = NULL) {
  
  overview_config <- get_overview_config(overview_type)
  # Use filtered metrics if provided, otherwise get all active metrics
  metrics <- if (!is.null(metrics_filter)) metrics_filter else get_active_metrics()
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
      years <- get_historical_year_range(10, inputs$custom_today)
      
      # Use filtered metrics if provided, otherwise get all historical metrics
      all_historical <- get_historical_metrics()
      hist_metrics <- if (!is.null(metrics_filter)) {
        intersect(metrics_filter, all_historical)
      } else {
        all_historical
      }
      
      # If no historical metrics to load, return empty list
      if (length(hist_metrics) == 0) {
        return(list())
      }
      
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
              zone_filter = inputs$zone_filter,
              analysis_date = inputs$custom_today,  # CRITICAL: Pass the analysis date!
              overview_type = overview_type
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
  
  # Setup legend outputs for all current metrics
  lapply(metrics, function(metric_id) {
    local({
      local_metric_id <- metric_id
      legend_id <- paste0(local_metric_id, "_legend")
      
      output[[legend_id]] <- renderUI({
        create_overview_legend(theme = current_theme(), metric_id = local_metric_id)
      })
    })
  })
  
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
        
        # Check if this metric supports chart type selection
        chart_type <- "bar"  # default
        if (!is.null(local_config$chart_types) && length(local_config$chart_types) > 1) {
          chart_type_input <- input[[paste0(local_metric_id, "_chart_type")]]
          if (!is.null(chart_type_input)) {
            chart_type <- chart_type_input
          }
        }
        
        # Choose chart function based on chart type
        if (chart_type == "pie" && "pie" %in% local_config$chart_types) {
          create_overview_pie_chart(
            data = data,
            title = paste(local_config$display_name, "Progress"),
            theme = current_theme(),
            metric_type = local_metric_id
          )
        } else {
          chart_function(
            data = data,
            title = paste(local_config$display_name, "Progress"),
            y_label = local_config$y_label,
            theme = current_theme(),
            metric_type = local_metric_id
          )
        }
      })
    })
  })
  
  # Setup historical charts
  if (include_historical) {
    lapply(metrics, function(metric_id) {
      local({
        local_metric_id <- metric_id
        local_config <- registry[[metric_id]]
        local_overview_type <- overview_type
        output_id <- paste0(local_metric_id, "_historical_chart")
        
        output[[output_id]] <- renderPlotly({
          req(historical_data())
          hist_data <- historical_data()[[local_metric_id]]
          
          # DEBUG: Log what data we have
          cat("DEBUG Historical", local_metric_id, ":\n")
          cat("  - Average rows:", if (!is.null(hist_data$average)) nrow(hist_data$average) else "NULL", "\n")
          cat("  - Current rows:", if (!is.null(hist_data$current)) nrow(hist_data$current) else "NULL", "\n")
          cat("  - Yearly data rows:", if (!is.null(hist_data$yearly_data)) nrow(hist_data$yearly_data) else "NULL", "\n")
          
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
              overview_type = local_overview_type
            ))
          }
          
          # Standard comparison chart
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
            title = paste(local_config$display_name, "- Historical"),
            y_label = y_label,
            bar_color = local_config$bg_color,
            theme = current_theme(),
            ten_year_avg_data = hist_data$ten_year_average
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
          # Debug the click data
          cat("DEBUG: Click event for", metric_id, "- x:", click_data$x, "y:", click_data$y, "pointNumber:", click_data$pointNumber, "\n")
          
          # Get current zone filter to understand data structure
          current_zone_filter <- input$zone_filter
          cat("DEBUG: Current zone filter:", current_zone_filter, "\n")
          
          # Zone determination logic based on current filter and click position
          zone_clicked <- NULL
          
          if (current_zone_filter == "1,2") {
            # Combined P1+P2 data - check if we have 1 or 2 bars
            if (!is.null(click_data$pointNumber)) {
              # If pointNumber 0 and there are 2 bars, it's P1; if 1 bar total, it's combined P1+P2
              # Check by looking at the y value (display_name)
              display_name <- as.character(click_data$y)
              if (grepl("P1", display_name, ignore.case = TRUE)) {
                zone_clicked <- "P1"
              } else if (grepl("P2", display_name, ignore.case = TRUE)) {
                zone_clicked <- "P2"  
              } else {
                # Combined bar - pass the combined filter
                zone_clicked <- "1,2"
              }
            }
          } else if (current_zone_filter %in% c("1", "2")) {
            # Single zone selected - pass through the current filter
            zone_clicked <- if (current_zone_filter == "1") "P1" else "P2"
          } else if (current_zone_filter == "separate") {
            # Separate zones - use pointNumber or y value to determine
            if (!is.null(click_data$pointNumber)) {
              zone_clicked <- if (click_data$pointNumber == 0) "P1" else "P2"
            }
          }
          
          cat("DEBUG: Determined zone_clicked:", zone_clicked, "\n")
          
          # Navigate with the determined zone and clicked metric
          navigate_to_overview(
            session, 
            overview_config$drill_down_target,
            zone_clicked, 
            input$custom_today, 
            input$expiring_days,
            current_theme(),
            metric_id  # Pass the clicked metric
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
    generate_summary_stats(current_data(), metrics_filter, overview_type)
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
