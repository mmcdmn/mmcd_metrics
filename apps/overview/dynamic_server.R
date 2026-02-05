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

# =============================================================================
# DETAIL VALUE BOXES GENERATOR
# =============================================================================

#' Generate detail value boxes for a specific facility drill-down
#' Shows detailed metrics (active, expiring, expired, etc.) for a single facility
#' 
#' @param metric_id The metric ID
#' @param facility The facility name to filter by
#' @param zone_filter Vector of zones to include
#' @param analysis_date Date for analysis
#' @param expiring_days Days until expiring
#' @param theme Current color theme
#' @param detail_data Optional preloaded detail data frame (avoid reloading)
#' @return UI element with detail value boxes or NULL if not supported
#' @export
generate_facility_detail_boxes <- function(metric_id, facility, zone_filter, 
                                           analysis_date, expiring_days, theme = "MMCD",
                                           detail_data = NULL) {
  # Get metric config
  config <- get_metric_config(metric_id)
  if (is.null(config)) return(NULL)
  
  # Check if this metric has detail boxes defined
  detail_boxes <- get_metric_detail_boxes(metric_id)
  if (is.null(detail_boxes)) return(NULL)
  
  # Load detailed data for this specific facility (unless provided)
  tryCatch({
    if (is.null(detail_data)) {
      # Use load_data_by_facility with the facility filter
      detail_data <- load_data_by_facility(
        metric = metric_id,
        analysis_date = analysis_date,
        expiring_days = expiring_days,
        zone_filter = zone_filter,
        separate_zones = FALSE,
        facility_filter = facility
      )
    }
    if (is.null(detail_data) || nrow(detail_data) == 0) {
      return(div(
        class = "alert alert-warning",
        style = "margin: 10px 0;",
        icon("exclamation-triangle"), " ",
        "No data available for ", facility
      ))
    }
    
    # Normalize column names if needed (fallbacks for *_count or *_acres)
    if (!"total" %in% names(detail_data)) {
      if ("total_count" %in% names(detail_data)) {
        detail_data$total <- detail_data$total_count
      } else if ("total_acres" %in% names(detail_data)) {
        detail_data$total <- detail_data$total_acres
      }
    }
    if (!"active" %in% names(detail_data)) {
      if ("active_count" %in% names(detail_data)) {
        detail_data$active <- detail_data$active_count
      } else if ("active_acres" %in% names(detail_data)) {
        detail_data$active <- detail_data$active_acres
      }
    }
    if (!"expiring" %in% names(detail_data)) {
      if ("expiring_count" %in% names(detail_data)) {
        detail_data$expiring <- detail_data$expiring_count
      } else if ("expiring_acres" %in% names(detail_data)) {
        detail_data$expiring <- detail_data$expiring_acres
      }
    }

    # Get status colors
    status_colors <- get_status_colors(theme = theme)
    
    # Generate detail boxes from config
    n_boxes <- length(detail_boxes)
    col_width <- max(2, floor(12 / n_boxes))  # At least 2 columns wide
    
    box_elements <- lapply(detail_boxes, function(box_def) {
      # Get value from data - column names should match (total, active, expiring)
      value <- if (box_def$column %in% names(detail_data)) {
        sum(detail_data[[box_def$column]], na.rm = TRUE)
      } else {
        NA
      }
      
      # Skip this box if no data
      if (is.na(value)) return(NULL)
      
      # Round if it's a decimal (acres)
      display_value <- if (is.numeric(value) && value %% 1 != 0) {
        format(round(value, 1), big.mark = ",")
      } else {
        format(value, big.mark = ",")
      }
      
      # Get color based on status
      bg_color <- if (!is.null(box_def$status) && box_def$status %in% names(status_colors)) {
        unname(status_colors[box_def$status])
      } else {
        config$bg_color
      }
      
      column(col_width,
        create_stat_box(
          value = display_value,
          title = box_def$title,
          bg_color = bg_color,
          icon = icon(box_def$icon),
          icon_type = "fontawesome"
        )
      )
    })
    
    # Remove NULL elements
    box_elements <- Filter(Negate(is.null), box_elements)
    
    if (length(box_elements) == 0) {
      # Log columns for debugging if detail boxes failed
      cat("Detail boxes missing columns for", metric_id, "facility", facility, "\n")
      cat("Available columns:", paste(names(detail_data), collapse = ", "), "\n")
      return(div(
        class = "alert alert-info",
        style = "margin: 10px 0;",
        icon("info-circle"), " ",
        "Detail breakdown not available for ", facility
      ))
    }
    
    # Return container with facility header and detail boxes
    div(class = "facility-detail-boxes-container",
      div(class = "facility-detail-header",
        icon("building"), " ", facility, " - ", config$display_name, " Details"
      ),
      fluidRow(class = "facility-detail-boxes", box_elements)
    )
    
  }, error = function(e) {
    warning(paste("Error generating detail boxes for", metric_id, ":", e$message))
    return(div(
      class = "alert alert-danger",
      style = "margin: 10px 0;",
      icon("exclamation-circle"), " ",
      "Error loading details: ", e$message
    ))
  })
}

# =============================================================================
# DYNAMIC COLOR LOGIC
# =============================================================================

# Cache for historical averages (loaded once per session for performance)
.hist_cache <- new.env(parent = emptyenv())

#' Load historical cache data once per session
load_hist_cache <- function() {
  if (is.null(.hist_cache$data)) {
    tryCatch({
      cache_file <- file.path(get_cache_dir(), "historical_averages_cache.rds")
      if (file.exists(cache_file)) {
        .hist_cache$data <- readRDS(cache_file)
      }
    }, error = function(e) NULL)
  }
  .hist_cache$data
}

#' Get historical average for a metric and week (cached for performance)
#' @param metric_id Metric ID
#' @param week_num ISO week number
#' @return Historical average value or NULL if not available
get_historical_week_avg <- function(metric_id, week_num) {
  cache <- load_hist_cache()
  if (is.null(cache)) return(NULL)
  
  cache_key <- paste0(metric_id, "_10yr")
  if (!cache_key %in% names(cache$averages)) return(NULL)
  
  hist_data <- cache$averages[[cache_key]]
  week_data <- hist_data[hist_data$week_num == week_num, ]
  if (nrow(week_data) == 0) return(NULL)
  
  mean(week_data$value, na.rm = TRUE)
}

#' Get current week's value for a metric from database
#' Uses the SAME logic as load_current_year_for_cache to match chart values
#' @param metric_id Metric ID
#' @param analysis_date Date for analysis
#' @param zone_filter Zones to include
#' @return Current week's value or NULL
get_current_week_value <- function(metric_id, analysis_date, zone_filter = c("1", "2")) {
  tryCatch({
    registry <- get_metric_registry()
    config <- registry[[metric_id]]
    if (is.null(config)) return(NULL)
    
    current_year <- lubridate::year(analysis_date)
    
    # Get the week's Friday (that's what the chart uses)
    week_start <- lubridate::floor_date(analysis_date, "week", week_start = 1)
    week_friday <- week_start + 4
    week_num <- lubridate::isoweek(week_friday)
    
    # Load raw data
    raw_data <- load_app_historical_data(metric_id, current_year, current_year, zone_filter)
    if (is.null(raw_data$treatments) || nrow(raw_data$treatments) == 0) return(NULL)
    
    treatments <- raw_data$treatments
    treatments$inspdate <- as.Date(treatments$inspdate)
    
    # Create value column (same logic as load_current_year_for_cache)
    has_acres <- isTRUE(config$has_acres)
    if (has_acres) {
      acres_col <- if ("treated_acres" %in% names(treatments)) "treated_acres" 
                   else if ("acres" %in% names(treatments)) "acres" else NULL
      treatments$value <- if (!is.null(acres_col)) treatments[[acres_col]] else 1
    } else {
      treatments$value <- 1
    }
    
    is_active <- isTRUE(config$is_active_treatment) || isTRUE(config$use_active_calculation)
    
    if (is_active) {
      # For active treatments: count what's ACTIVE on that week's Friday
      if (!"effect_days" %in% names(treatments)) {
        treatments$effect_days <- if (metric_id == "catch_basin") 28 else 14
      }
      treatments$treatment_end <- treatments$inspdate + treatments$effect_days
      
      # Filter to treatments active on Friday
      active <- treatments[treatments$inspdate <= week_friday & treatments$treatment_end >= week_friday, ]
      
      if (nrow(active) == 0) return(0)
      sum(active$value, na.rm = TRUE)
    } else {
      # For simple counts: sum treatments from that week
      week_data <- treatments[treatments$inspdate >= week_start & treatments$inspdate <= week_start + 6, ]
      if (nrow(week_data) == 0) return(0)
      sum(week_data$value, na.rm = TRUE)
    }
  }, error = function(e) {
    warning(paste("Error getting current week value:", e$message))
    NULL
  })
}

#' Determine value box color and get comparison data
#' Uses weekly comparison: current week value vs 10yr weekly average
#' OPTIMIZED: Accepts optional weekly_value to avoid duplicate DB loads
#' @param metric_id Metric ID
#' @param current_value Current active value (for fallback)
#' @param analysis_date Date for analysis
#' @param config Metric configuration
#' @param zone_filter Zones to filter
#' @param weekly_value Optional: pre-loaded weekly value from historical data (avoids DB call)
#' @return List with color, historical_avg, current_week, pct_diff, and status
#' @export
get_dynamic_value_box_info <- function(metric_id, current_value, analysis_date, config, zone_filter = c("1", "2"), weekly_value = NULL) {
  default_color <- config$bg_color
  result <- list(
    color = default_color,
    historical_avg = NULL,
    current_week = NULL,
    pct_diff = NULL,
    status = "default"
  )
  
  # Only apply dynamic colors to specific metrics
  dynamic_metrics <- c("drone", "ground_prehatch", "catch_basin", "structure", 
                       "mosquito_monitoring", "suco")
  
  if (!metric_id %in% dynamic_metrics) return(result)
  
  # SUCO has hardcoded capacity logic
  if (metric_id == "suco") {
    if (current_value >= 72) {
      result$color <- "#dc2626"
      result$status <- "at_capacity"
    } else if (current_value >= 60) {
      result$color <- "#eab308"
      result$status <- "near_capacity"
    } else {
      result$color <- "#16a34a"
      result$status <- "good"
    }
    result$historical_avg <- 72  # Capacity threshold for reference
    return(result)
  }
  
  # Get WEEKLY comparison: current week value vs 10yr weekly average
  week_num <- lubridate::isoweek(analysis_date)
  
  # Get 10-year weekly average from cache (fast - uses file cache)
  historical_avg <- get_historical_week_avg(metric_id, week_num)
  if (is.null(historical_avg) || historical_avg == 0) return(result)
  
  # Use pre-loaded weekly value if provided, otherwise load from DB (slower)
  current_week <- if (!is.null(weekly_value)) {
    weekly_value
  } else {
    get_current_week_value(metric_id, analysis_date, zone_filter)
  }
  if (is.null(current_week)) {
    # Fallback: can't get weekly value, use default color
    return(result)
  }
  
  result$historical_avg <- round(historical_avg, 0)
  result$current_week <- round(current_week, 0)
  result$pct_diff <- round(100 * (current_week - historical_avg) / historical_avg, 1)
  
  # Mosquito monitoring uses inverse logic (lower is better)
  if (metric_id == "mosquito_monitoring") {
    if (current_week <= historical_avg * 1.1) {
      result$color <- "#16a34a"
      result$status <- "good"
    } else if (current_week <= historical_avg * 1.2) {
      result$color <- "#eab308"
      result$status <- "warning"
    } else {
      result$color <- "#dc2626"
      result$status <- "alert"
    }
  } else {
    # Standard metrics (higher is better)
    if (current_week >= historical_avg * 0.9) {
      result$color <- "#16a34a"
      result$status <- "good"
    } else if (current_week >= historical_avg * 0.8) {
      result$color <- "#eab308"
      result$status <- "warning"
    } else {
      result$color <- "#dc2626"
      result$status <- "alert"
    }
  }
  
  result
}

#' Simple color-only wrapper for backward compatibility
get_dynamic_value_box_color <- function(metric_id, current_value, analysis_date, config) {
  get_dynamic_value_box_info(metric_id, current_value, analysis_date, config)$color
}

# =============================================================================
# SUMMARY STATS GENERATOR
# =============================================================================

#' Generate summary stats UI from data with clickable value boxes
#' @param data Named list of data frames keyed by metric_id
#' @param metrics_filter Optional filter for which metrics to display
#' @param overview_type Type of overview (district, facilities, fos)
#' @param analysis_date Date for analysis (for dynamic color logic)
#' @param historical_data Optional: pre-loaded historical data to extract weekly values (avoids duplicate DB loads)
#' @return fluidRow with clickable stat boxes that toggle chart visibility
#' @export
generate_summary_stats <- function(data, metrics_filter = NULL, overview_type = "district", analysis_date = Sys.Date(), historical_data = NULL) {
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
      
      # Get dynamic color for this facility based on aggregated active value
      box_color <- get_dynamic_value_box_color(metrics[1], active_all, analysis_date, config)
      
      # Create stat box with facility short name
      column(col_width,
        div(
          class = "stat-box-clickable",
          `data-facility` = fac,
          create_stat_box(
            value = paste0(pct, "%"),
            title = fac,  # Just show facility short name
            bg_color = box_color,
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
    
    # Pre-extract weekly values from historical data to avoid duplicate DB loads
    week_num <- lubridate::isoweek(analysis_date)
    weekly_values <- list()
    if (!is.null(historical_data)) {
      for (metric_id in metrics) {
        hist <- historical_data[[metric_id]]
        if (!is.null(hist) && !is.null(hist$current) && nrow(hist$current) > 0) {
          week_row <- hist$current[hist$current$week_num == week_num, ]
          if (nrow(week_row) > 0) {
            weekly_values[[metric_id]] <- sum(week_row$value, na.rm = TRUE)
          }
        }
      }
    }
    
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
          active <- 0
        }
        
        # Get dynamic color and comparison info
        # Use pre-loaded weekly value if available (optimization)
        weekly_val <- weekly_values[[metric_id]]
        box_info <- get_dynamic_value_box_info(metric_id, active, analysis_date, config, weekly_value = weekly_val)
        week_num <- lubridate::isoweek(analysis_date)
        
        # Create clickable stat box with comparison data attributes
        column(col_width,
          div(
            class = "stat-box-clickable",
            `data-metric-id` = metric_id,
            `data-current-week` = if (!is.null(box_info$current_week)) box_info$current_week else "",
            `data-historical-avg` = if (!is.null(box_info$historical_avg)) box_info$historical_avg else "",
            `data-pct-diff` = if (!is.null(box_info$pct_diff)) box_info$pct_diff else "",
            `data-week-num` = week_num,
            create_stat_box(
              value = paste0(pct, "%"),
              title = config$display_name,
              bg_color = box_info$color,
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

  # Cache last refresh inputs so downstream UI can reuse without reloading
  last_refresh_inputs <- reactiveVal(NULL)
  observeEvent(input$refresh, {
    last_refresh_inputs(refresh_inputs())
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
    # Get analysis date from refresh inputs (captured at refresh time)
    inputs <- refresh_inputs()
    analysis_date <- if (!is.null(inputs$custom_today)) inputs$custom_today else Sys.Date()
    
    # Pass historical data to avoid duplicate DB loads
    hist_data <- tryCatch(historical_data(), error = function(e) NULL)
    generate_summary_stats(current_data(), metrics_filter, overview_type, analysis_date, hist_data)
  })
  
  # =========================================================================
  # FACILITY DETAIL BOXES (for facilities view drill-down)
  # =========================================================================
  
  output$facility_detail_boxes <- renderUI({
    tryCatch({
      req(input$selected_facility)
      req(current_data())
      
      facility <- input$selected_facility

      inputs <- last_refresh_inputs()
      if (is.null(inputs)) {
        return(div(
          class = "alert alert-info",
          style = "margin: 10px 0;",
          icon("sync"), " ",
          "Press Refresh to load facility details."
        ))
      }
      
      if (is.null(metrics_filter) || length(metrics_filter) == 0) {
        return(div(
          class = "alert alert-info",
          "Select a metric category to see facility details."
        ))
      }
      
      # For now, use the first (should be only) metric in the filter
      metric_id <- metrics_filter[1]
      
      # Check if this metric supports detail boxes
      if (!has_detail_boxes(metric_id)) {
        return(div(
          class = "alert alert-info",
          style = "margin: 10px 0;",
          icon("info-circle"), " ",
          "Detail breakdown not yet available for this metric."
        ))
      }

      # Use already-loaded data (avoid reloading)
      preloaded_detail <- current_data()[[metric_id]]
      if (!is.null(preloaded_detail) && nrow(preloaded_detail) > 0) {
        facility_key <- trimws(tolower(facility))
        
        # Create facility matching vectors
        facility_match <- if ("facility" %in% names(preloaded_detail)) {
          trimws(tolower(as.character(preloaded_detail$facility))) == facility_key
        } else {
          rep(FALSE, nrow(preloaded_detail))
        }
        
        display_match <- if ("display_name" %in% names(preloaded_detail)) {
          trimws(tolower(as.character(preloaded_detail$display_name))) == facility_key
        } else {
          rep(FALSE, nrow(preloaded_detail))
        }
        
        # Apply the filter
        preloaded_detail <- preloaded_detail[facility_match | display_match, , drop = FALSE]
      }

      # Generate the detail boxes
      generate_facility_detail_boxes(
        metric_id = metric_id,
        facility = facility,
        zone_filter = inputs$zone_filter,
        analysis_date = inputs$custom_today,
        expiring_days = inputs$expiring_days,
        theme = current_theme(),
        detail_data = preloaded_detail
      )
    }, error = function(e) {
      # Capture and display actual error message in UI
      div(
        class = "alert alert-danger",
        style = "margin: 10px 0; word-break: break-word;",
        icon("exclamation-circle"), " ",
        strong("Error loading facility details:"), br(),
        code(e$message)
      )
    })
  })

  # Ensure facility detail boxes render even when container is hidden
  outputOptions(output, "facility_detail_boxes", suspendWhenHidden = FALSE)
  
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
