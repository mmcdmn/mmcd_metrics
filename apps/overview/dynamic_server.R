# Dynamic Server Logic for Overview Apps
# =============================================================================
# Generates all server logic dynamically from the metric registry.
# NO hardcoding of specific metrics - everything driven by get_metric_registry()
# =============================================================================

# =============================================================================
# SHORT-TERM CACHE FOR CURRENT WEEK VALUES
# Reduces DB load under concurrent multi-user access
# Cache expires after 120 seconds - data rarely changes within two minutes
# =============================================================================

# In-memory cache (per R process)
.current_week_cache <- new.env(parent = emptyenv())

#' Get cached current week value, or compute and cache it
#' @param metric_id Metric ID
#' @param analysis_date Date for analysis  
#' @param zone_filter Zones to include
#' @param ttl_seconds Cache time-to-live in seconds (default 120)
#' @return Current week's value or NULL
get_cached_current_week_value <- function(metric_id, analysis_date, zone_filter = c("1", "2"), ttl_seconds = 120) {
  cache_key <- paste(metric_id, as.character(analysis_date), paste(zone_filter, collapse = "_"), sep = "|")
  
  # Check if cached and not expired
  if (exists(cache_key, envir = .current_week_cache)) {
    cached <- get(cache_key, envir = .current_week_cache)
    if (difftime(Sys.time(), cached$timestamp, units = "secs") < ttl_seconds) {
      return(cached$value)
    }
  }
  
  # Not cached or expired - compute fresh value
  value <- get_current_week_value(metric_id, analysis_date, zone_filter)
  
  # Store in cache
  assign(cache_key, list(value = value, timestamp = Sys.time()), envir = .current_week_cache)
  
  value
}

#' Clear the current week cache (call when data might have changed)
clear_current_week_cache <- function() {
  rm(list = ls(envir = .current_week_cache), envir = .current_week_cache)
}

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
        
        current_theme <- theme_reactive()
        
        # Try Redis chart cache (2-min TTL)
        chart <- NULL
        if (exists("get_cached_chart", mode = "function")) {
          data_hash <- digest::digest(data, algo = "xxhash64")
          chart <- tryCatch({
            get_cached_chart(
              paste0("current:", local_metric_id),
              render_func = NULL,  # will build manually below if miss
              local_metric_id, chart_type, current_theme, data_hash
            )
          }, error = function(e) NULL)
        }
        
        if (is.null(chart)) {
          # Build chart
          chart <- if (chart_type == "pie" && "pie" %in% local_config$chart_types) {
            create_overview_pie_chart(
              data = data,
              title = paste(local_config$display_name, "Progress"),
              theme = current_theme,
              metric_type = local_metric_id
            )
          } else {
            chart_function(
              data = data,
              title = paste(local_config$display_name, "Progress"),
              y_label = local_config$y_label,
              theme = current_theme,
              metric_type = local_metric_id
            )
          }
          
          # Cache the built chart
          if (exists("set_app_cached_redis", mode = "function")) {
            cache_key <- build_cache_key("chart", paste0("current:", local_metric_id),
                                          local_metric_id, chart_type, current_theme,
                                          digest::digest(data, algo = "xxhash64"))
            tryCatch(set_app_cached_redis(cache_key, chart, ttl = TTL_2_MIN), error = function(e) NULL)
          }
        }
        
        chart
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

    # For cattail_treatments and air_sites: compute 'treated' = active - expiring
    # active = treated + need_treatment, expiring = need_treatment only
    if (metric_id %in% c("cattail_treatments", "air_sites")) {
      detail_data$treated <- detail_data$active - detail_data$expiring
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
# SHARED HELPERS - Reduce duplication across views
# =============================================================================

#' Calculate display percentage/value for a metric
#' Used in facilities, FOS, and district views to avoid triple duplication
#' @param metric_id The metric ID
#' @param config The metric config from registry
#' @param total Total count/acres
#' @param active Active count/acres
#' @param expiring Expiring count/acres
#' @param metric_data Optional full data frame (for raw_value_column)
#' @return Named list: pct (display value), display_value (formatted string)
calculate_display_pct <- function(metric_id, config, total, active, expiring, metric_data = NULL) {
  if (metric_id %in% c("cattail_treatments", "air_sites")) {
    treated <- active - expiring
    workload <- treated + expiring
    pct <- if (workload > 0) round(100 * treated / workload, 1) else 0
  } else if (isTRUE(config$display_raw_value)) {
    raw_col <- if (!is.null(config$raw_value_column)) config$raw_value_column else "max_vector_index"
    if (!is.null(metric_data) && raw_col %in% names(metric_data)) {
      pct <- max(metric_data[[raw_col]], na.rm = TRUE)
      if (is.infinite(pct)) pct <- 0
    } else {
      pct <- if (total > 0) round(active / total * 100, 1) else 0
    }
  } else if (isTRUE(config$display_as_average)) {
    pct <- if (total > 0) round(100 * active / total, 1) else 0
  } else {
    pct <- if (total > 0) ceiling(100 * active / total) else 0
  }
  
  # Format the display value
  display_value <- if (isTRUE(config$display_raw_value)) {
    suffix <- if (!is.null(config$raw_value_suffix)) config$raw_value_suffix else ""
    paste0(round(pct, 2), suffix)
  } else {
    paste0(pct, "%")
  }
  
  list(pct = pct, display_value = display_value)
}

#' Extract weekly values from historical data for a set of metrics
#' Used in facilities and district views
#' @param metrics Vector of metric IDs
#' @param historical_data Historical data list
#' @param week_num Week number
#' @param registry Metric registry
#' @return Named list of weekly values by metric_id
extract_weekly_values <- function(metrics, historical_data, week_num, registry) {
  vals <- list()
  if (is.null(historical_data)) return(vals)
  for (metric_id in metrics) {
    hist <- historical_data[[metric_id]]
    if (!is.null(hist) && !is.null(hist$current) && nrow(hist$current) > 0) {
      week_row <- hist$current[hist$current$week_num == week_num, ]
      if (nrow(week_row) > 0) {
        config_m <- registry[[metric_id]]
        vals[[metric_id]] <- if (isTRUE(config_m$aggregate_as_average)) {
          mean(week_row$value, na.rm = TRUE)
        } else {
          sum(week_row$value, na.rm = TRUE)
        }
      }
    }
  }
  vals
}

#' Extract weekly values grouped by an identifier (e.g., facility or fos)
#' @param metrics Vector of metric IDs
#' @param historical_data Historical data list
#' @param week_num Week number
#' @param registry Metric registry
#' @param group_col Column name to group by (default: "facility")
#' @return Named list of metric_id -> named list(group -> value)
extract_weekly_values_by_group <- function(metrics, historical_data, week_num, registry, group_col = "facility") {
  vals <- list()
  if (is.null(historical_data)) return(vals)

  for (metric_id in metrics) {
    hist <- historical_data[[metric_id]]
    if (is.null(hist) || is.null(hist$current) || nrow(hist$current) == 0) next

    week_row <- hist$current[hist$current$week_num == week_num, ]
    if (nrow(week_row) == 0 || !group_col %in% names(week_row)) next

    config_m <- registry[[metric_id]]
    group_vals <- list()

    for (grp in unique(week_row[[group_col]])) {
      grp_row <- week_row[week_row[[group_col]] == grp, ]
      if (nrow(grp_row) == 0) next

      group_vals[[as.character(grp)]] <- if (isTRUE(config_m$aggregate_as_average)) {
        mean(grp_row$value, na.rm = TRUE)
      } else {
        sum(grp_row$value, na.rm = TRUE)
      }
    }

    if (length(group_vals) > 0) {
      vals[[metric_id]] <- group_vals
    }
  }

  vals
}

# =============================================================================
# DYNAMIC COLOR LOGIC
# =============================================================================

# Cache for historical averages (loaded once per session for performance)
.hist_cache <- new.env(parent = emptyenv())

#' Load historical cache data once per session
#' Loads from Redis (single source of truth)
load_hist_cache <- function() {
  if (is.null(.hist_cache$data)) {
    # Redis is the single source of truth
    if (exists("load_historical_cache_redis", mode = "function") && exists("redis_is_active", mode = "function") && redis_is_active()) {
      tryCatch({
        redis_cache <- load_historical_cache_redis()
        if (!is.null(redis_cache) && length(redis_cache$averages) > 0) {
          .hist_cache$data <- redis_cache
          cat("[CACHE] Historical cache loaded from Redis (", length(redis_cache$averages), " metrics)\n")
          return(.hist_cache$data)
        } else {
          cat("[CACHE] WARNING: Redis historical cache is empty\n")
        }
      }, error = function(e) {
        cat("[CACHE] Redis load failed:", e$message, "\n")
      })
    } else {
      cat("[CACHE] WARNING: Redis not available for historical cache\n")
    }
  }
  .hist_cache$data
}

#' Get historical average for a metric and week (cached for performance)
#' @param metric_id Metric ID
#' @param week_num Week number (from lubridate::week(), matching cache data)
#' @return Historical average value or NULL if not available
get_historical_week_avg <- function(metric_id, week_num, zone_filter = c("1", "2")) {
  cache <- load_hist_cache()
  if (is.null(cache)) return(NULL)
  
  cache_key <- paste0(metric_id, "_10yr")
  if (!cache_key %in% names(cache$averages)) return(NULL)
  
  hist_data <- cache$averages[[cache_key]]
  week_data <- hist_data[hist_data$week_num == week_num, ]
  if (nrow(week_data) == 0) return(NULL)
  
  # Filter by zone when cache has zone-level data
  if ("zone" %in% names(week_data) && length(zone_filter) > 0) {
    week_data <- week_data[week_data$zone %in% zone_filter, ]
    if (nrow(week_data) == 0) return(NULL)
  }
  
  # For average-based metrics (e.g., mosquito monitoring avg/trap), average across zones.
  # For count/acres-based metrics, sum across zones.
  registry <- get_metric_registry()
  config <- registry[[metric_id]]
  if (isTRUE(config$aggregate_as_average)) {
    mean(week_data$value, na.rm = TRUE)
  } else {
    sum(week_data$value, na.rm = TRUE)
  }
}

#' Get historical average for a SPECIFIC FACILITY for a metric and week
#' Computes on-the-fly by loading 10-year historical data for that facility
#' @param metric_id Metric ID
#' @param week_num Week number
#' @param facility Facility short name to filter
#' @param zone_filter Zone filter
#' @return Historical average value or NULL if not available
get_historical_week_avg_by_facility <- function(metric_id, week_num, facility, zone_filter = c("1", "2")) {
  tryCatch({
    registry <- get_metric_registry()
    config <- registry[[metric_id]]
    if (is.null(config)) return(NULL)
    
    # Get year range for 10-year average
    current_year <- lubridate::year(Sys.Date())
    start_year <- current_year - 9
    
    # Load historical data with facility filter
    # Use load_app_historical_data which loads from the app's data_functions
    raw_data <- load_app_historical_data(metric_id, start_year, current_year - 1, zone_filter)
    
    if (is.null(raw_data$treatments) || nrow(raw_data$treatments) == 0) return(NULL)
    
    treatments <- raw_data$treatments
    
    # Filter to specific facility
    if (!"facility" %in% names(treatments)) return(NULL)
    treatments <- treatments[treatments$facility == facility, ]
    if (nrow(treatments) == 0) return(NULL)
    
    # Add week number
    treatments$week_num <- lubridate::week(treatments$inspdate)
    
    # Filter to target week
    week_treatments <- treatments[treatments$week_num == week_num, ]
    if (nrow(week_treatments) == 0) return(NULL)
    
    # Assign value column (acres or count)
    if (isTRUE(config$has_acres)) {
      acres_col <- if ("treated_acres" %in% names(week_treatments)) "treated_acres"
                   else if ("acres" %in% names(week_treatments)) "acres" else NULL
      week_treatments$value <- if (!is.null(acres_col)) week_treatments[[acres_col]] else 1
    } else if ("value" %in% names(week_treatments)) {
      week_treatments$value <- as.numeric(week_treatments$value)
    } else {
      week_treatments$value <- 1
    }
    
    # Calculate average across years for this week
    yearly_values <- week_treatments %>%
      dplyr::mutate(year = lubridate::year(inspdate)) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(yearly_value = sum(value, na.rm = TRUE), .groups = "drop")
    
    if (nrow(yearly_values) == 0) return(NULL)
    
    # Return average across years
    mean(yearly_values$yearly_value, na.rm = TRUE)
    
  }, error = function(e) {
    cat("[DEBUG] Error in get_historical_week_avg_by_facility:", e$message, "\n")
    NULL
  })
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
    week_num <- lubridate::week(analysis_date)
    
    # Reuse load_current_year_for_cache to avoid duplicating value column logic
    current_data <- load_current_year_for_cache(metric_id, current_year, zone_filter, config)
    
    if (is.null(current_data) || nrow(current_data) == 0) return(NULL)
    
    # Find the matching week
    week_row <- current_data[current_data$week_num == week_num, ]
    if (nrow(week_row) == 0) return(0)
    
    # Average across zones for avg-based metrics, sum for count/acres
    if (isTRUE(config$aggregate_as_average)) {
      mean(week_row$value, na.rm = TRUE)
    } else {
      sum(week_row$value, na.rm = TRUE)
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
#' @param facility_filter Optional: specific facility to compare against (uses facility historical avg)
#' @return List with color, historical_avg, current_week, pct_diff, and status
#' @export
get_dynamic_value_box_info <- function(metric_id, current_value, analysis_date, config, zone_filter = c("1", "2"), weekly_value = NULL, facility_filter = NULL) {
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
  
  # Fixed percentage-based coloring (e.g., air_sites coverage %)
  if (isTRUE(config$color_mode == "fixed_pct")) {
    thresholds <- config$color_thresholds
    # current_value is passed as the treatment coverage percentage
    pct_val <- current_value
    if (pct_val >= thresholds$good) {
      result$color <- "#16a34a"
      result$status <- "good"
    } else if (pct_val >= thresholds$warning) {
      result$color <- "#eab308"
      result$status <- "warning"
    } else {
      result$color <- "#dc2626"
      result$status <- "alert"
    }
    return(result)
  }
  
  # Percent-of-average coloring (e.g., mosquito monitoring current vs historical)
  # current_value = pct from value box (e.g., 76.5 means current is 76.5% of historical)
  if (isTRUE(config$color_mode == "pct_of_average")) {
    thresholds <- config$color_thresholds
    pct_val <- current_value
    pct_diff_val <- round(pct_val - 100, 1)
    
    if (isTRUE(config$inverse_color)) {
      # Inverse: lower is better (mosquitoes - fewer = good)
      if (pct_val <= thresholds$good) {
        result$color <- "#16a34a"
        result$status <- "good"
      } else if (pct_val <= thresholds$warning) {
        result$color <- "#eab308"
        result$status <- "warning"
      } else {
        result$color <- "#dc2626"
        result$status <- "alert"
      }
    } else {
      # Standard: higher is better
      if (pct_val >= (200 - thresholds$good)) {
        result$color <- "#16a34a"
        result$status <- "good"
      } else if (pct_val >= (200 - thresholds$warning)) {
        result$color <- "#eab308"
        result$status <- "warning"
      } else {
        result$color <- "#dc2626"
        result$status <- "alert"
      }
    }
    
    result$pct_diff <- pct_diff_val
    return(result)
  }
  
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
  week_num <- lubridate::week(analysis_date)
  
  # Get 10-year weekly average - use facility-specific if facility_filter provided
  historical_avg <- if (!is.null(facility_filter)) {
    # Facility-specific: compare against THIS facility's historical average
    get_historical_week_avg_by_facility(metric_id, week_num, facility_filter, zone_filter)
  } else {
    # Zone-wide: compare against zone average from cache (fast)
    get_historical_week_avg(metric_id, week_num, zone_filter)
  }
  if (is.null(historical_avg) || historical_avg == 0) return(result)
  
  # Use pre-loaded weekly value if provided, otherwise use short-term cache
  current_week <- if (!is.null(weekly_value)) {
    weekly_value
  } else {
    # Use cached value (120s TTL) to reduce DB load under concurrent access
    get_cached_current_week_value(metric_id, analysis_date, zone_filter)
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
generate_summary_stats <- function(data, metrics_filter = NULL, overview_type = "district", analysis_date = Sys.Date(), historical_data = NULL, zone_filter = c("1", "2"), fos_filter = NULL) {
  
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
    # Max 3 facilities per row for more spacious boxes
    max_fac_per_row <- 3
    col_width <- max(4, floor(12 / min(n_facilities, max_fac_per_row)))
    
    # Pre-extract FACILITY-SPECIFIC weekly values from historical data
    # This ensures each facility is compared against its own current week data
    week_num <- lubridate::week(analysis_date)
    fac_weekly_values <- extract_weekly_values_by_group(
      metrics,
      historical_data,
      week_num,
      registry,
      group_col = "facility"
    )
    
    stat_boxes <- lapply(all_facilities, function(fac) {
      # For this facility, aggregate across all metrics
      total_all <- 0
      active_all <- 0
      expiring_all <- 0
      fac_zone <- NULL
      
      for (metric_id in metrics) {
        metric_data <- data[[metric_id]]
        if (!is.null(metric_data) && "facility" %in% names(metric_data)) {
          fac_data <- metric_data[metric_data$facility == fac, ]
          if (nrow(fac_data) > 0) {
            total_all <- total_all + sum(fac_data$total, na.rm = TRUE)
            active_all <- active_all + sum(fac_data$active, na.rm = TRUE)
            expiring_all <- expiring_all + sum(fac_data$expiring, na.rm = TRUE)
            # Determine this facility's zone (first non-NA value)
            if (is.null(fac_zone) && "zone" %in% names(fac_data)) {
              z <- unique(fac_data$zone[!is.na(fac_data$zone)])
              if (length(z) > 0) fac_zone <- as.character(z[1])
            }
          }
        }
      }
      
      config <- registry[[metrics[1]]]
      pct_info <- calculate_display_pct(metrics[1], config, total_all, active_all, expiring_all)
      pct <- pct_info$pct
      
      # Use FACILITY-SPECIFIC historical data for dynamic coloring
      # Each facility is compared against ITS OWN historical average
      box_color <- config$bg_color
      box_info <- list(current_week = NULL, historical_avg = NULL, pct_diff = NULL)
      
      if (!is.null(fac_zone)) {
        fac_zone_filter <- fac_zone
        for (metric_id in metrics) {
          # Get facility-specific weekly value (nested: fac_weekly_values[[metric_id]][[fac]])
          weekly_val <- if (!is.null(fac_weekly_values[[metric_id]])) {
            fac_weekly_values[[metric_id]][[fac]]
          } else {
            NULL
          }
          fac_config <- registry[[metric_id]]
          fac_cm <- if (!is.null(fac_config$color_mode)) fac_config$color_mode else ""
          color_value <- if (fac_cm %in% c("fixed_pct", "pct_of_average")) pct else active_all
          info <- tryCatch(
            get_dynamic_value_box_info(metric_id, color_value, analysis_date, 
                                       fac_config, 
                                       zone_filter = fac_zone_filter, 
                                       weekly_value = weekly_val,
                                       facility_filter = fac),
            error = function(e) NULL
          )
          if (!is.null(info) && info$status != "default") {
            box_color <- info$color
            box_info <- info
            break
          }
        }
      }
      
      # Create stat box with facility short name
      column(col_width,
        div(
          class = "stat-box-clickable",
          `data-facility` = fac,
          `data-current-week` = if (!is.null(box_info$current_week)) box_info$current_week else "",
          `data-historical-avg` = if (!is.null(box_info$historical_avg)) box_info$historical_avg else "",
          `data-pct-diff` = if (!is.null(box_info$pct_diff)) box_info$pct_diff else "",
          `data-week-num` = week_num,
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
    
    # Split facility boxes into rows
    fac_rows <- split(stat_boxes, ceiling(seq_along(stat_boxes) / max_fac_per_row))
    row_elements <- lapply(fac_rows, function(row_boxes) fluidRow(row_boxes))
    div(class = "facility-stat-boxes", row_elements)
    
  } else if (overview_type == "fos" && length(metrics) > 0 && !is.null(fos_filter) && fos_filter != "all") {
    # FOS overview with specific FOS selected (from index.html):
    # Show per-METRIC value boxes with hidden charts, same pattern as district view
    week_num <- lubridate::week(analysis_date)
    weekly_values <- extract_weekly_values(metrics, historical_data, week_num, registry)
    
    n_metrics <- length(metrics)
    max_per_row <- 3
    col_width <- max(4, floor(12 / min(n_metrics, max_per_row)))
    
    stat_boxes <- lapply(metrics, function(metric_id) {
      config <- registry[[metric_id]]
      if (is.null(config)) return(NULL)
      
      metric_data <- data[[metric_id]]
      
      if (!is.null(metric_data) && nrow(metric_data) > 0) {
        total <- sum(metric_data$total, na.rm = TRUE)
        active <- sum(metric_data$active, na.rm = TRUE)
        expiring <- sum(metric_data$expiring, na.rm = TRUE)
        
        pct_info <- calculate_display_pct(metric_id, config, total, active, expiring, metric_data)
        pct <- pct_info$pct
        display_value <- pct_info$display_value
      } else {
        pct <- 0
        active <- 0
        display_value <- "0%"
      }
      
      # Get dynamic color
      weekly_val <- weekly_values[[metric_id]]
      cm <- if (!is.null(config$color_mode)) config$color_mode else ""
      color_value <- if (cm %in% c("fixed_pct", "pct_of_average")) pct else active
      box_info <- tryCatch(
        get_dynamic_value_box_info(metric_id, color_value, analysis_date, config, zone_filter = zone_filter, weekly_value = weekly_val),
        error = function(e) list(color = config$bg_color, current_week = NULL, historical_avg = NULL, pct_diff = NULL, status = "default")
      )
      
      column(col_width,
        div(
          class = "stat-box-clickable",
          `data-metric-id` = metric_id,
          `data-current-week` = if (!is.null(box_info$current_week)) box_info$current_week else "",
          `data-historical-avg` = if (!is.null(box_info$historical_avg)) box_info$historical_avg else "",
          `data-pct-diff` = if (!is.null(box_info$pct_diff)) box_info$pct_diff else "",
          `data-week-num` = week_num,
          create_stat_box(
            value = display_value,
            title = config$display_name,
            bg_color = box_info$color,
            icon = if (!is.null(config$image_path)) config$image_path else config$icon,
            icon_type = if (!is.null(config$image_path)) "image" else "fontawesome"
          )
        )
      )
    })
    
    stat_boxes <- Filter(Negate(is.null), stat_boxes)
    if (length(stat_boxes) == 0) return(fluidRow())
    
    # Split into rows
    box_rows <- split(stat_boxes, ceiling(seq_along(stat_boxes) / max_per_row))
    row_elements <- lapply(box_rows, function(row_boxes) fluidRow(row_boxes))
    
    # Build hidden chart panels for each metric (current progress + historical side by side)
    chart_panels <- lapply(metrics, function(metric_id) {
      config <- registry[[metric_id]]
      
      # Current progress chart
      current_panel <- create_chart_panel(metric_id, config, chart_height = "250px", is_historical = FALSE)
      
      # Historical chart (if available)
      all_historical <- get_historical_metrics()
      has_hist <- metric_id %in% all_historical
      
      if (has_hist) {
        hist_panel <- create_chart_panel(metric_id, config, chart_height = "250px", is_historical = TRUE)
        chart_content <- fluidRow(
          column(6, div(style = "padding-right: 5px;", current_panel)),
          column(6, div(style = "padding-left: 5px;", hist_panel))
        )
      } else {
        chart_content <- current_panel
      }
      
      div(
        id = paste0("chart_wrapper_", metric_id),
        class = "chart-panel-wrapper category-chart",
        chart_content
      )
    })
    
    charts_row <- div(class = "charts-grid-row", do.call(tagList, chart_panels))
    
    div(class = "fos-metric-boxes",
      do.call(tagList, row_elements),
      charts_row
    )
    
  } else if (overview_type == "fos" && length(metrics) > 0) {
    # FOS view: one value box per FOS person
    # Get unique FOS from the data
    all_fos <- unique(unlist(lapply(metrics, function(metric_id) {
      metric_data <- data[[metric_id]]
      if (!is.null(metric_data) && "fos" %in% names(metric_data)) {
        unique(metric_data$fos)
      } else if (!is.null(metric_data) && "display_name" %in% names(metric_data)) {
        unique(as.character(metric_data$display_name))
      } else {
        NULL
      }
    })))
    
    if (length(all_fos) == 0) {
      return(div(
        class = "alert alert-warning",
        icon("exclamation-triangle"), " ",
        "No FOS data available for this view."
      ))
    }
    
    n_fos <- length(all_fos)
    max_per_row <- 4
    col_width <- max(3, floor(12 / min(n_fos, max_per_row)))
    
    stat_boxes <- lapply(all_fos, function(fos_id) {
      total_all <- 0
      active_all <- 0
      expiring_all <- 0
      
      for (metric_id in metrics) {
        metric_data <- data[[metric_id]]
        if (!is.null(metric_data) && nrow(metric_data) > 0) {
          # Match by fos column or display_name
          if ("fos" %in% names(metric_data)) {
            fos_data <- metric_data[metric_data$fos == fos_id, ]
          } else if ("display_name" %in% names(metric_data)) {
            fos_data <- metric_data[as.character(metric_data$display_name) == fos_id, ]
          } else {
            fos_data <- data.frame()
          }
          if (nrow(fos_data) > 0) {
            total_all <- total_all + sum(fos_data$total, na.rm = TRUE)
            active_all <- active_all + sum(fos_data$active, na.rm = TRUE)
            expiring_all <- expiring_all + sum(fos_data$expiring, na.rm = TRUE)
          }
        }
      }
      
      config <- registry[[metrics[1]]]
      pct_info <- calculate_display_pct(metrics[1], config, total_all, active_all, expiring_all)
      pct <- pct_info$pct
      
      # Get the display name (FOS shortname)
      fos_display <- fos_id
      # Try to get display_name from data
      for (metric_id in metrics) {
        metric_data <- data[[metric_id]]
        if (!is.null(metric_data) && "fos" %in% names(metric_data) && "display_name" %in% names(metric_data)) {
          match_row <- metric_data[metric_data$fos == fos_id, ]
          if (nrow(match_row) > 0) {
            fos_display <- as.character(match_row$display_name[1])
            break
          }
        }
      }
      
      box_color <- config$bg_color
      
      column(col_width,
        div(
          class = "stat-box-clickable",
          `data-fos` = fos_id,
          create_stat_box(
            value = paste0(pct, "%"),
            title = fos_display,
            bg_color = box_color,
            icon = icon("user-tie"),
            icon_type = "fontawesome"
          )
        )
      )
    })
    
    fos_rows <- split(stat_boxes, ceiling(seq_along(stat_boxes) / max_per_row))
    row_elements <- lapply(fos_rows, function(row_boxes) fluidRow(row_boxes))
    div(class = "fos-stat-boxes", row_elements)
    
  } else {
    # District view: metrics grouped by category
    categories <- get_metric_categories()
    metrics_by_cat <- get_metrics_grouped_by_category()
    
    # Pre-extract weekly values from historical data to avoid duplicate DB loads
    week_num <- lubridate::week(analysis_date)
    weekly_values <- extract_weekly_values(metrics, historical_data, week_num, registry)
    
    # Build category sections
    category_sections <- lapply(categories, function(cat) {
      cat_metrics <- intersect(metrics_by_cat[[cat]], metrics)
      if (length(cat_metrics) == 0) return(NULL)
      
      # Calculate column width
      n_metrics <- length(cat_metrics)
      max_per_row <- 3
      col_width <- max(4, floor(12 / min(n_metrics, max_per_row)))
      
      stat_boxes <- lapply(cat_metrics, function(metric_id) {
        config <- registry[[metric_id]]
        if (is.null(config)) return(NULL)
        
        metric_data <- data[[metric_id]]
        
        # Calculate stats
        if (!is.null(metric_data) && nrow(metric_data) > 0) {
          total <- sum(metric_data$total, na.rm = TRUE)
          active <- sum(metric_data$active, na.rm = TRUE)
          expiring <- sum(metric_data$expiring, na.rm = TRUE)
          
          pct_info <- calculate_display_pct(metric_id, config, total, active, expiring, metric_data)
          pct <- pct_info$pct
          display_value <- pct_info$display_value
        } else {
          pct <- 0
          active <- 0
          display_value <- "0%"
        }
        
        # Get dynamic color and comparison info
        weekly_val <- weekly_values[[metric_id]]
        cm <- if (!is.null(config$color_mode)) config$color_mode else ""
        color_value <- if (cm %in% c("fixed_pct", "pct_of_average")) pct else active
        box_info <- tryCatch(
          get_dynamic_value_box_info(metric_id, color_value, analysis_date, config, zone_filter = zone_filter, weekly_value = weekly_val),
          error = function(e) {
            cat("WARNING: get_dynamic_value_box_info failed for", metric_id, ":", e$message, "\n")
            list(color = config$bg_color, current_week = NULL, historical_avg = NULL, pct_diff = NULL, status = "default")
          }
        )
        week_num <- tryCatch(lubridate::week(analysis_date), error = function(e) NA)
        
        # Create clickable stat box with comparison data attributes
        redirect_url <- if (!is.null(config$click_redirect)) config$click_redirect else ""
        result_box <- tryCatch(
          column(col_width,
            div(
              class = "stat-box-clickable",
              `data-metric-id` = metric_id,
              `data-redirect` = redirect_url,
              `data-current-week` = if (!is.null(box_info$current_week)) box_info$current_week else "",
              `data-historical-avg` = if (!is.null(box_info$historical_avg)) box_info$historical_avg else "",
              `data-pct-diff` = if (!is.null(box_info$pct_diff)) box_info$pct_diff else "",
              `data-week-num` = week_num,
              create_stat_box(
                value = display_value,
                title = config$display_name,
                bg_color = box_info$color,
                icon = if (!is.null(config$image_path)) config$image_path else config$icon,
                icon_type = if (!is.null(config$image_path)) "image" else "fontawesome"
              )
            )
          ),
          error = function(e) {
            cat("[DEBUG] ERROR creating stat box for", metric_id, ":", e$message, "\n")
            return(NULL)
          }
        )
        
        return(result_box)
      })
      
      stat_boxes <- Filter(Negate(is.null), stat_boxes)
      if (length(stat_boxes) == 0) return(NULL)
      
      # Split stat boxes into chunks for multiple rows
      box_rows <- split(stat_boxes, ceiling(seq_along(stat_boxes) / max_per_row))
      row_elements <- lapply(box_rows, function(row_boxes) fluidRow(row_boxes))
      
      # Build charts for this category's metrics
      cat_charts <- lapply(cat_metrics, function(metric_id) {
        config <- registry[[metric_id]]
        div(
          id = paste0("chart_wrapper_", metric_id),
          class = "chart-panel-wrapper category-chart",
          tryCatch(
            # Compact chart height for category grid - no scrolling
            create_chart_panel(metric_id, config, chart_height = "150px", is_historical = FALSE),
            error = function(e) {
              cat("[DEBUG] ERROR creating chart for", metric_id, ":", e$message, "\n")
              div(class = "alert alert-warning", "Error loading chart")
            }
          )
        )
      })
      
      # Wrap charts in a row for grid layout (3 per row)
      charts_row <- if (length(cat_charts) > 0) {
        div(class = "charts-grid-row", do.call(tagList, cat_charts))
      } else {
        NULL
      }
      
      section_result <- tryCatch(
        div(class = "category-section",
          style = "margin-bottom: 25px;",
          div(class = "category-header",
            style = "font-size: 14px; font-weight: bold; color: #666; margin-bottom: 8px; padding-left: 5px; border-left: 3px solid #2c5aa0;",
            cat
          ),
          do.call(tagList, row_elements),
          charts_row
        ),
        error = function(e) NULL
      )
      return(section_result)
    })
    
    # Filter out NULL sections and wrap in a container
    category_sections <- Filter(Negate(is.null), category_sections)
    
    if (length(category_sections) == 0) {
      return(div(
        class = "alert alert-warning",
        "No metrics data available to display."
      ))
    }
    
    final_result <- tryCatch(
      div(class = "metrics-by-category", 
        do.call(tagList, category_sections)
      ),
      error = function(e) {
        cat("[DEBUG] ERROR creating final metrics container:", e$message, "\n")
        return(div(
          class = "alert alert-danger",
          "Error assembling metrics display: ", e$message
        ))
      }
    )
    
    cat("[DEBUG] Final result created successfully\n")
    return(final_result)
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
                                   metrics_filter = NULL,
                                   facility_filter = NULL,
                                   fos_filter = NULL) {
  
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
              separate_zones = inputs$separate_zones,
              facility_filter = facility_filter,
              fos_filter = fos_filter
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
              analysis_date = inputs$custom_today,
              overview_type = overview_type,
              facility_filter = facility_filter  # Pass for FOS view facility-specific historical
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
  # District and facilities views need clickable charts for drill-down
  chart_function <- if (overview_type == "district") {
    create_zone_chart  # always clickable
  } else if (overview_type == "facilities" && overview_config$enable_drill_down) {
    # Facilities view with drill-down: make bars clickable for FOS navigation
    function(data, title, y_label, theme, metric_type, metric_config = NULL, ...) {
      create_overview_chart(data, title, y_label, theme, metric_type, metric_config, clickable = TRUE)
    }
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
  
  # Enable legend rendering even when hidden (charts start hidden)
  lapply(metrics, function(metric_id) {
    legend_id <- paste0(metric_id, "_legend")
    outputOptions(output, legend_id, suspendWhenHidden = FALSE)
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
            metric_type = local_metric_id,
            metric_config = local_config
          )
        }
      })
      
      # CRITICAL: Don't suspend rendering when chart is hidden
      # This ensures charts in hidden category sections still render
      outputOptions(output, output_id, suspendWhenHidden = FALSE)
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
        
        # CRITICAL: Don't suspend rendering when chart is hidden
        outputOptions(output, output_id, suspendWhenHidden = FALSE)
      })
    })
  }
  
  # =========================================================================
  # DRILL-DOWN CLICK HANDLERS
  # =========================================================================
  
  # District view: bar chart clicks drill down to facilities
  if (overview_config$enable_drill_down && overview_type == "district") {
    lapply(metrics, function(metric_id) {
      observeEvent(event_data("plotly_click", source = metric_id), {
        click_data <- event_data("plotly_click", source = metric_id)
        if (!is.null(click_data)) {
          # Debug the click data (use paste to safely handle list-type fields)
          cat("DEBUG: Click event for", metric_id, "- x:", paste(click_data$x, collapse=","), "y:", paste(click_data$y, collapse=","), "key:", paste(click_data$key, collapse=","), "pointNumber:", paste(click_data$pointNumber, collapse=","), "\n")
          
          # Get current zone filter to understand data structure
          current_zone_filter <- input$zone_filter
          cat("DEBUG: Current zone filter:", current_zone_filter, "\n")
          
          # Zone determination logic based on current filter and click position
          zone_clicked <- NULL
          
          # Use key aesthetic for reliable label (y returns factor level after coord_flip)
          display_name <- as.character(click_data$key)
          # Guard against NA key (happens if click lands on a bar trace without key aesthetic)
          if (is.na(display_name)) display_name <- NULL
          
          if (current_zone_filter == "1,2") {
            if (!is.null(display_name) && display_name != "NULL") {
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
            # Separate zones - use key to determine which zone was clicked
            if (!is.null(display_name) && display_name != "NULL") {
              if (grepl("P1", display_name, ignore.case = TRUE)) {
                zone_clicked <- "P1"
              } else if (grepl("P2", display_name, ignore.case = TRUE)) {
                zone_clicked <- "P2"
              }
            }
            # Fallback to pointNumber if key didn't help
            if (is.null(zone_clicked) && !is.null(click_data$pointNumber)) {
              zone_clicked <- if (click_data$pointNumber == 0) "P1" else "P2"
            }
          }
          
          cat("DEBUG: Determined zone_clicked:", zone_clicked, "\n")
          
          # Navigate with the determined zone and clicked metric
          # Pass zone_filter_raw to preserve 'separate' mode in the drill-down URL
          navigate_to_overview(
            session, 
            overview_config$drill_down_target,
            zone_clicked, 
            input$custom_today, 
            input$expiring_days,
            current_theme(),
            metric_id,  # Pass the clicked metric
            zone_filter_raw = current_zone_filter
          )
        }
      })
    })
  }
  
  # Facilities view: stat box click shows hidden detail boxes (JS toggles the container)
  # Bar chart click drills down to FOS
  if (overview_type == "facilities") {
    # Stat box click just triggers detail boxes (handled by renderUI below + JS toggle)
    observeEvent(input$selected_facility, {
      facility <- input$selected_facility
      if (!is.null(facility) && facility != "") {
        cat("DEBUG: Facility stat box clicked - showing detail boxes for:", facility, "\n")
        # The detail_boxes renderUI reacts to input$selected_facility automatically
      }
    })
    
    # Bar chart click navigates to FOS drill-down
    if (overview_config$enable_drill_down) {
      lapply(metrics, function(metric_id) {
        local({
          local_metric_id <- metric_id
          observeEvent(event_data("plotly_click", source = local_metric_id), {
            click_data <- event_data("plotly_click", source = local_metric_id)
            if (!is.null(click_data)) {
              cat("DEBUG: Facilities chart bar clicked for", local_metric_id, "\n")
              cat("DEBUG: Click data - x:", paste(click_data$x, collapse=","), "y:", paste(click_data$y, collapse=","), "key:", paste(click_data$key, collapse=","), "\n")
              
              # Use key aesthetic for reliable facility name (y returns factor level after coord_flip)
              facility_clicked <- as.character(click_data$key)
              # Guard against NA key (happens if click lands on a bar trace without key aesthetic)
              if (is.na(facility_clicked)) facility_clicked <- as.character(click_data$x)
              
              # Determine zone and preserve raw filter for 'separate' mode
              zone_value <- isolate(input$zone_filter)
              zone_clicked <- if (zone_value %in% c("1", "2")) {
                paste0("P", zone_value)
              } else {
                "1,2"
              }
              
              navigate_to_overview(
                session,
                "fos_overview",
                zone_clicked,
                isolate(input$custom_today),
                isolate(input$expiring_days),
                current_theme(),
                metric_id = local_metric_id,
                facility_clicked = facility_clicked,
                zone_filter_raw = zone_value
              )
            }
          })
        })
      })
    }
  }
  
  # FOS view: FOS stat box click to show detail value boxes
  if (overview_type == "fos") {
    observeEvent(input$selected_fos, {
      fos <- input$selected_fos
      if (!is.null(fos) && fos != "") {
        cat("DEBUG: FOS clicked:", fos, "\n")
      }
    })
  }
  
  # =========================================================================
  # SUMMARY STATS OUTPUT
  # =========================================================================
  
  # When current data is ready, tell the client to swap skeleton for real content
  # NOTE: hideLoadingSkeleton is now sent from renderUI below so the swap
  # only happens AFTER value boxes are generated (prevents empty wrapper).
  
  cat("\\n=== [DEBUG] ABOUT TO DEFINE summary_stats renderUI ===\\n")
  flush.console()

  output$summary_stats <- renderUI({
    message("[RENDER-UI] === summary_stats renderUI ENTERED ===")
    
    # Try to get current_data
    data_result <- tryCatch({
      cd <- current_data()
      message("[RENDER-UI] current_data() returned ", length(cd), " items")
      cd
    }, error = function(e) {
      message("[RENDER-UI] current_data() ERROR: ", e$message)
      NULL
    })
    
    if (is.null(data_result) || length(data_result) == 0) {
      message("[RENDER-UI] No data - showing fallback")
      session$sendCustomMessage("hideLoadingSkeleton", TRUE)
      return(div(class = "alert alert-warning", "No data loaded yet. Click Refresh to load data."))
    }
    
    # Get analysis date
    inputs <- tryCatch(refresh_inputs(), error = function(e) {
      message("[RENDER-UI] refresh_inputs() ERROR: ", e$message)
      list(custom_today = Sys.Date())
    })
    analysis_date <- if (!is.null(inputs$custom_today)) inputs$custom_today else Sys.Date()
    
    # Get historical data
    hist_data <- tryCatch(historical_data(), error = function(e) {
      message("[RENDER-UI] historical_data() ERROR: ", e$message)
      NULL
    })
    
    message("[RENDER-UI] About to call generate_summary_stats")
    
    # Generate value boxes
    result <- tryCatch({
      stats <- generate_summary_stats(data_result, metrics_filter, overview_type, analysis_date, hist_data, zone_filter = inputs$zone_filter, fos_filter = fos_filter)
      message("[RENDER-UI] generate_summary_stats SUCCESS")
      stats
    }, error = function(e) {
      message("[RENDER-UI] generate_summary_stats ERROR: ", e$message)
      div(
        class = "alert alert-danger",
        style = "margin: 10px 0;",
        icon("exclamation-circle"),
        strong(" Dashboard rendering error: "),
        e$message
      )
    })
    
    # Hide skeleton and show content
    session$sendCustomMessage("hideLoadingSkeleton", TRUE)
    message("[RENDER-UI] hideLoadingSkeleton sent, returning result")
    
    result
  })
  
  # CRITICAL: The summary_stats_wrapper div starts with display:none,
  # which causes Shiny to suspend this output and never render it.
  # This tells Shiny to always render it regardless of visibility.
  outputOptions(output, "summary_stats", suspendWhenHidden = FALSE)
  
  # Fallback: if summary_stats renderUI fails before sending hideLoadingSkeleton
  # (e.g., req(current_data()) throws because the reactive errored),
  # this observer ensures the skeleton is eventually hidden so the user isn't stuck.
  observeEvent(current_data(), {
    message("[FALLBACK] current_data() has fired! Data has ", length(current_data()), " metrics")
    session$sendCustomMessage("hideLoadingSkeleton", TRUE)
  })
  
  # =========================================================================
  # FACILITY DETAIL BOXES (for facilities view drill-down)
  # =========================================================================
  
  output$facility_detail_boxes <- renderUI({
    tryCatch({
      req(current_data())
      
      # Determine what was selected: FOS or facility
      selected_entity <- NULL
      entity_type <- NULL
      
      if (overview_type == "fos" && !is.null(input$selected_fos) && input$selected_fos != "") {
        selected_entity <- input$selected_fos
        entity_type <- "fos"
      } else if (!is.null(input$selected_facility) && input$selected_facility != "") {
        selected_entity <- input$selected_facility
        entity_type <- "facility"
      }
      
      req(selected_entity)

      inputs <- last_refresh_inputs()
      if (is.null(inputs)) {
        return(div(
          class = "alert alert-info",
          style = "margin: 10px 0;",
          icon("sync"), " ",
          "Press Refresh to load details."
        ))
      }
      
      if (is.null(metrics_filter) || length(metrics_filter) == 0) {
        return(div(
          class = "alert alert-info",
          "Select a metric category to see details."
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
        entity_key <- trimws(tolower(selected_entity))
        
        if (entity_type == "fos") {
          # Match by fos column or display_name
          fos_match <- if ("fos" %in% names(preloaded_detail)) {
            trimws(tolower(as.character(preloaded_detail$fos))) == entity_key
          } else {
            rep(FALSE, nrow(preloaded_detail))
          }
          display_match <- if ("display_name" %in% names(preloaded_detail)) {
            trimws(tolower(as.character(preloaded_detail$display_name))) == entity_key
          } else {
            rep(FALSE, nrow(preloaded_detail))
          }
          preloaded_detail <- preloaded_detail[fos_match | display_match, , drop = FALSE]
        } else {
          # Match by facility
          facility_match <- if ("facility" %in% names(preloaded_detail)) {
            trimws(tolower(as.character(preloaded_detail$facility))) == entity_key
          } else {
            rep(FALSE, nrow(preloaded_detail))
          }
          display_match <- if ("display_name" %in% names(preloaded_detail)) {
            trimws(tolower(as.character(preloaded_detail$display_name))) == entity_key
          } else {
            rep(FALSE, nrow(preloaded_detail))
          }
          preloaded_detail <- preloaded_detail[facility_match | display_match, , drop = FALSE]
        }
      }

      # Get display name for header
      header_label <- selected_entity
      if (entity_type == "fos") {
        # Try to get FOS shortname from data
        for (mid in metrics) {
          md <- current_data()[[mid]]
          if (!is.null(md) && "fos" %in% names(md) && "display_name" %in% names(md)) {
            match_row <- md[md$fos == selected_entity, ]
            if (nrow(match_row) > 0) {
              header_label <- as.character(match_row$display_name[1])
              break
            }
          }
        }
      }

      # Generate the detail boxes (reuse existing generator)
      generate_facility_detail_boxes(
        metric_id = metric_id,
        facility = header_label,
        zone_filter = inputs$zone_filter,
        analysis_date = inputs$custom_today,
        expiring_days = inputs$expiring_days,
        theme = current_theme(),
        detail_data = preloaded_detail
      )
    }, error = function(e) {
      div(
        class = "alert alert-danger",
        style = "margin: 10px 0; word-break: break-word;",
        icon("exclamation-circle"), " ",
        strong("Error loading details:"), br(),
        code(e$message)
      )
    })
  })

  # Ensure detail boxes render even when container is hidden
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
