# Shared server utilities for mmcd_metrics apps
# Minimal shared code - prefer inline patterns over wrapper functions

# =============================================================================
# CACHED DATA LOADING FOR INDIVIDUAL APPS
# =============================================================================
# Wraps any app's load_raw_data() with Redis DB query caching (2-min TTL).
# Call this instead of load_raw_data() directly to get automatic caching.

#' Load data with Redis caching (2-min TTL for DB query results)
#' @param app_name Short name for the app (e.g., "catch_basin_status")
#' @param ... All arguments to pass through to load_raw_data()
#' @return The result of load_raw_data(), cached if possible
cached_load_raw_data <- function(app_name, ...) {
  if (exists("get_cached_db_query", mode = "function")) {
    args <- list(...)
    cached <- tryCatch({
      get_cached_db_query(
        paste0("raw:", app_name),
        load_func = function() load_raw_data(...),
        app_name, args
      )
    }, error = function(e) NULL)
    if (!is.null(cached)) return(cached)
  }
  load_raw_data(...)
}

#' Load cached stat box data (2-min TTL)
#' @param app_name Short name for the app
#' @param data The data to compute stats from
#' @param calc_func A function that computes stats from data
#' @return Cached or freshly computed stat values
cached_stat_calculation <- function(app_name, data, calc_func) {
  if (exists("get_cached_stat_box", mode = "function")) {
    data_hash <- digest::digest(data, algo = "xxhash64")
    cached <- tryCatch({
      get_cached_stat_box(
        paste0("app_stat:", app_name),
        calc_func = calc_func,
        app_name, data_hash
      )
    }, error = function(e) NULL)
    if (!is.null(cached)) return(cached)
  }
  calc_func()
}

#' Load cached chart object (2-min TTL)
#' @param app_name Short name for the app
#' @param chart_id Chart identifier within the app
#' @param data Data feeding the chart (used for cache key)
#' @param render_func Function that creates the plotly chart
#' @param ... Additional key parameters (theme, chart_type, etc.)
#' @return Cached or freshly rendered plotly object
cached_chart <- function(app_name, chart_id, data, render_func, ...) {
  if (exists("get_cached_chart", mode = "function")) {
    data_hash <- digest::digest(data, algo = "xxhash64")
    cached <- tryCatch({
      get_cached_chart(
        paste0("app_chart:", app_name, ":", chart_id),
        render_func = NULL,
        app_name, chart_id, data_hash, ...
      )
    }, error = function(e) NULL)
    if (!is.null(cached)) {
      return(cached)
    }
  }
  
  chart <- render_func()
  
  # Store in cache
  if (exists("set_app_cached_redis", mode = "function") && exists("build_cache_key", mode = "function")) {
    data_hash <- digest::digest(data, algo = "xxhash64")
    cache_key <- build_cache_key("chart", paste0("app_chart:", app_name, ":", chart_id),
                                  app_name, chart_id, data_hash, ...)
    tryCatch(set_app_cached_redis(cache_key, chart, ttl = TTL_2_MIN), error = function(e) NULL)
  }
  
  chart
}

# =============================================================================
# COLOR MAPPING UTILITIES (shared because they're complex)
# =============================================================================

#' Map facility display names to theme colors
#' Handles zone suffixes: "Name P1" format (standard)
map_facility_display_names_to_colors <- function(display_names, theme = "MMCD", handle_zones = FALSE) {
  if (length(display_names) == 0) return(character(0))
  
  facility_colors <- get_facility_base_colors(theme = theme)
  facility_lookup <- get_facility_lookup()
  color_mapping <- character(0)
  
  for (display_name in display_names) {
    base_name <- sub(" P[12]$", "", display_name)  # Remove zone suffix
    
    matching_facility <- facility_lookup[facility_lookup$full_name == base_name, ]
    if (nrow(matching_facility) > 0) {
      short_name <- matching_facility$short_name[1]
      if (short_name %in% names(facility_colors)) {
        base_color <- facility_colors[short_name]
        
        # Darken P2 by 20% if zone handling enabled
        if (handle_zones && grepl(" P2$", display_name)) {
          rgb_vals <- col2rgb(base_color) / 255
          color_mapping[display_name] <- rgb(rgb_vals[1] * 0.8, rgb_vals[2] * 0.8, rgb_vals[3] * 0.8)
        } else {
          color_mapping[display_name] <- base_color
        }
      } else {
        color_mapping[display_name] <- get_status_colors(theme = theme)["unknown"]
      }
    } else {
      color_mapping[display_name] <- get_status_colors(theme = theme)["unknown"]
    }
  }
  
  return(color_mapping)
}

#' Map foreman display names to theme colors
#' Handles zone suffixes: "Name P1" format (standard)
map_foreman_display_names_to_colors <- function(display_names, theme = "MMCD", handle_zones = FALSE) {
  if (length(display_names) == 0) return(character(0))
  
  foreman_colors <- get_themed_foreman_colors(theme = theme)
  foremen_lookup <- get_foremen_lookup()
  color_mapping <- character(0)
  
  for (display_name in display_names) {
    base_name <- sub(" P[12]$", "", display_name)  # Remove zone suffix
    
    matching_foreman <- foremen_lookup[
      foremen_lookup$shortname == base_name |
      grepl(paste0("^", gsub("\\s+.*", "", base_name)), foremen_lookup$shortname), 
    ]
    
    if (nrow(matching_foreman) > 0) {
      shortname <- matching_foreman$shortname[1]
      if (shortname %in% names(foreman_colors)) {
        base_color <- foreman_colors[shortname]
        
        # Darken P2 by 20% if zone handling enabled
        if (handle_zones && grepl(" P2$", display_name)) {
          rgb_vals <- col2rgb(base_color) / 255
          color_mapping[display_name] <- rgb(rgb_vals[1] * 0.8, rgb_vals[2] * 0.8, rgb_vals[3] * 0.8)
        } else {
          color_mapping[display_name] <- base_color
        }
      } else {
        color_mapping[display_name] <- get_status_colors(theme = theme)["completed"]
      }
    } else {
      color_mapping[display_name] <- get_status_colors(theme = theme)["completed"]
    }
  }
  
  return(color_mapping)
}

#' Map foreman emp_num to theme colors
#' Used when data uses emp_num instead of shortname
map_foreman_emp_to_colors <- function(theme = "MMCD") {
  foreman_colors <- get_themed_foreman_colors(theme = theme)
  foremen_lookup <- get_foremen_lookup()
  emp_colors <- character(0)
  
  for (i in 1:nrow(foremen_lookup)) {
    shortname <- trimws(foremen_lookup$shortname[i])
    emp_num <- trimws(as.character(foremen_lookup$emp_num[i]))
    
    if (shortname %in% names(foreman_colors)) {
      emp_colors[emp_num] <- foreman_colors[shortname]
    }
  }
  
  return(emp_colors)
}

# =============================================================================
# HISTORICAL CHART CREATION 
# =============================================================================

#' Create a historical trend chart (plotly)
#' Supports stacked_bar, grouped_bar, line, area, step chart types
#' @param data Data frame with time_period, value, and group_label columns
#' @param chart_type One of: stacked_bar, grouped_bar, line, area, step
#' @param title Chart title
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @param colors Named vector of colors (names match group_label values)
create_trend_chart <- function(data, chart_type, title, x_label, y_label, colors = NULL) {
  if (nrow(data) == 0) {
    return(plotly_empty() %>% layout(title = "No data available"))
  }
  
  # Ensure required columns exist
  if (!"time_period" %in% names(data)) return(plotly_empty())
  value_col <- if ("value" %in% names(data)) "value" else if ("count" %in% names(data)) "count" else NULL
  group_col <- if ("group_label" %in% names(data)) "group_label" else if ("group_name" %in% names(data)) "group_name" else NULL
  
  if (is.null(value_col) || is.null(group_col)) return(plotly_empty())
  
  # Build plot based on chart type
  if (chart_type %in% c("stacked_bar", "grouped_bar", "bar")) {
    p <- plot_ly(data, x = ~time_period, y = as.formula(paste0("~", value_col)), 
                 color = as.formula(paste0("~", group_col)),
                 type = "bar", colors = colors) %>%
      layout(barmode = if (chart_type == "grouped_bar") "group" else "stack")
  } else if (chart_type == "line") {
    p <- plot_ly(data, x = ~time_period, y = as.formula(paste0("~", value_col)),
                 color = as.formula(paste0("~", group_col)),
                 type = "scatter", mode = "lines+markers", colors = colors)
  } else if (chart_type == "step") {
    p <- plot_ly(data, x = ~time_period, y = as.formula(paste0("~", value_col)),
                 color = as.formula(paste0("~", group_col)),
                 type = "scatter", mode = "lines+markers", line = list(shape = "hv"), colors = colors)
  } else if (chart_type == "area") {
    p <- plot_ly(data, x = ~time_period, y = as.formula(paste0("~", value_col)),
                 color = as.formula(paste0("~", group_col)),
                 type = "scatter", mode = "lines", fill = "tonexty", colors = colors)
  } else {
    # Default to stacked bar
    p <- plot_ly(data, x = ~time_period, y = as.formula(paste0("~", value_col)),
                 color = as.formula(paste0("~", group_col)),
                 type = "bar", colors = colors) %>%
      layout(barmode = "stack")
  }
  
  # Apply standard layout
  p %>% layout(
    title = list(text = title, font = list(size = 20)),
    xaxis = list(title = list(text = x_label, font = list(size = 18)), tickfont = list(size = 16)),
    yaxis = list(title = list(text = y_label, font = list(size = 18)), tickfont = list(size = 16)),
    legend = list(font = list(size = 16)),
    font = list(size = 16),
    hovermode = "x unified"
  )
}

# =============================================================================
# STANDARD PLOTLY LAYOUT
# =============================================================================

#' Apply standard plotly layout styling
#' Reduces repeated layout code across chart functions
#' @param p plotly object
#' @param title Chart title
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @param ... Additional layout parameters to override defaults
standard_plotly_layout <- function(p, title, x_label, y_label, ...) {
  p %>% layout(
    title = list(text = title, font = list(size = 20, family = "Arial, sans-serif", color = "#333"), x = 0.5),
    xaxis = list(
      title = list(text = x_label, font = list(size = 18, family = "Arial, sans-serif", color = "#333")),
      tickfont = list(size = 16, family = "Arial, sans-serif", color = "#333")
    ),
    yaxis = list(
      title = list(text = y_label, font = list(size = 18, family = "Arial, sans-serif", color = "#333")),
      tickfont = list(size = 16, family = "Arial, sans-serif", color = "#333")
    ),
    legend = list(font = list(size = 16, family = "Arial, sans-serif")),
    font = list(size = 16, family = "Arial, sans-serif"),
    margin = list(l = 70, r = 30, t = 100, b = 70),
    ...
  )
}

#' Create a simple distribution bar chart
#' @param x_values Vector of category labels
#' @param y_values Vector of counts
#' @param title Chart title
#' @param x_label X-axis label
#' @param y_label Y-axis label (default: "Number of Sites")
#' @param bar_color Color for bars (default: "lightblue")
#' @param border_color Color for bar borders (default: "darkblue")
create_distribution_chart <- function(x_values, y_values, title, x_label, 
                                      y_label = "Number of Sites",
                                      bar_color = "lightblue", 
                                      border_color = "darkblue") {
  plot_ly(
    x = x_values,
    y = y_values,
    type = 'bar',
    marker = list(color = bar_color, line = list(color = border_color, width = 1))
  ) %>%
    standard_plotly_layout(title, x_label, y_label)
}

# =============================================================================
# SITECODE LINK HELPER
# =============================================================================

#' Convert sitecode values to clickable HTML links to data.mmcd.org map
#' @param sitecode Character vector of sitecode values
#' @return Character vector with HTML anchor tags linking to the map
make_sitecode_link <- function(sitecode) {
  ifelse(
    is.na(sitecode) | nchar(trimws(as.character(sitecode))) == 0,
    as.character(sitecode),
    paste0(
      '<a href="https://data.mmcd.org/m1/map?search=', sitecode,
      '" target="_blank" style="color: #1a73e8; text-decoration: none; font-weight: 500;">',
      sitecode, '</a>'
    )
  )
}

cat(" Common server utilities loaded successfully\n")