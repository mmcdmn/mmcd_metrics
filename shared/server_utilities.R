# Shared server utilities for mmcd_metrics apps
# Minimal shared code - prefer inline patterns over wrapper functions

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

cat(" Common server utilities loaded successfully\n")