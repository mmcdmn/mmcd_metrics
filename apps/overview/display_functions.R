# Overview - Display Functions
# =============================================================================
# Shared functions for creating charts and visualizations for overview dashboards.
# These work with any group_by type (zone, facility, fos).
# =============================================================================

#' Create an overview chart showing treatment progress
#' @param data Data frame with columns: display_name, total, active, expiring
#' @param title Chart title
#' @param y_label Y-axis label
#' @param theme Color theme name
#' @param metric_type Type of metric for specific styling
#' @return Plotly chart object
#' @export
create_overview_chart <- function(data, title, y_label, theme = "MMCD", metric_type = "default") {
  if (is.null(data) || nrow(data) == 0) {
    return(create_empty_chart(title, "No data available"))
  }
  
  # Get status colors from db_helpers with theme support
  status_colors <- get_status_colors(theme = theme)
  
  # Prepare data for layered bars
  # Gray background: Total (all sites)
  # Green: Active treatments (not expiring)
  # Orange: Expiring treatments
  data <- data %>%
    mutate(
      y_total = total,
      y_active = pmax(0, active - expiring),  # Active only (not including expiring)
      y_expiring = expiring,
      # Calculate percentage rounded UP with no decimals
      pct = ceiling(100 * active / pmax(1, total)),
      # Tooltips - also show percentage rounded up with no decimals
      tooltip_text = paste0(
        display_name, "\n",
        "Total: ", format(total, big.mark = ","), "\n",
        "Active: ", format(active, big.mark = ","), 
        " (", pct, "%)\n",
        "Expiring: ", format(expiring, big.mark = ",")
      )
    )
  
  # Ensure display_name is ordered correctly (if it's a factor, use its levels; otherwise sort by total)
  if (!is.factor(data$display_name)) {
    data$display_name <- reorder(data$display_name, data$y_total)
  }
  
  # Create base ggplot with layered bars
  p <- ggplot(data, aes(x = display_name)) +
    # Gray background - total/untreated
    geom_bar(aes(y = y_total, text = tooltip_text), 
             stat = "identity", fill = "gray70", alpha = 0.4) +
    # Green layer - active treatments (stacked from bottom)
    geom_bar(aes(y = y_active + y_expiring), 
             stat = "identity", fill = unname(status_colors["active"]), alpha = 0.9) +
    # Orange layer - expiring (at the bottom of the active portion)
    geom_bar(aes(y = y_expiring), 
             stat = "identity", fill = unname(status_colors["planned"]), alpha = 1) +
    coord_flip() +
    labs(
      x = NULL,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 11),
      axis.text.y = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  # Convert to plotly with custom tooltip
  plotly_chart <- ggplotly(p, tooltip = "text") %>%
    layout(
      margin = list(l = 80, r = 20, t = 10, b = 50),
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12),
        bordercolor = "black"
      )
    ) %>%
    config(displayModeBar = FALSE)
  
  return(plotly_chart)
}


#' Create a zone-level chart showing P1 vs P2 with click capability
#' Used by the top-level District Overview dashboard
#' @param data Data frame with columns: zone, display_name, total, active, expiring
#' @param title Chart title
#' @param y_label Y-axis label
#' @param theme Color theme name
#' @param metric_type Unique ID for this chart (used for plotly source)
#' @return Plotly chart object with click event support
#' @export
create_zone_chart <- function(data, title, y_label, theme = "MMCD", metric_type = "chart") {
  if (is.null(data) || nrow(data) == 0) {
    return(create_empty_chart(title, "No data available"))
  }
  
  # Get status colors from db_helpers with theme support
  status_colors <- get_status_colors(theme = theme)
  
  # Prepare data for layered bars - percentage rounded UP with no decimals
  data <- data %>%
    mutate(
      y_total = total,
      y_active = pmax(0, active - expiring),  # Active only (not including expiring)
      y_expiring = expiring,
      pct = ceiling(100 * active / pmax(1, total)),
      # Tooltips
      tooltip_text = paste0(
        "<b>", display_name, "</b><br>",
        "Total: ", format(total, big.mark = ","), "<br>",
        "Active: ", format(active, big.mark = ","), 
        " (", pct, "%)<br>",
        "Expiring: ", format(expiring, big.mark = ","), "<br>",
        "<i>Click to drill down</i>"
      )
    )
  
  # Ensure display_name is ordered (P1, P2)
  data$display_name <- factor(data$display_name, levels = c("P1", "P2"))
  
  # Create base ggplot with layered bars (horizontal for consistency with facilities)
  p <- ggplot(data, aes(x = display_name)) +
    # Gray background - total/untreated
    geom_bar(aes(y = y_total, text = tooltip_text), 
             stat = "identity", fill = "gray70", alpha = 0.4) +
    # Green layer - active treatments (stacked from bottom)
    geom_bar(aes(y = y_active + y_expiring), 
             stat = "identity", fill = unname(status_colors["active"]), alpha = 0.9) +
    # Orange layer - expiring (at the bottom of the active portion)
    geom_bar(aes(y = y_expiring), 
             stat = "identity", fill = unname(status_colors["planned"]), alpha = 1) +
    coord_flip() +
    labs(
      title = NULL,
      x = NULL,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  # Convert to plotly with custom tooltip and click event source
  plotly_chart <- ggplotly(p, tooltip = "text", source = metric_type) %>%
    layout(
      margin = list(l = 50, r = 20, t = 20, b = 50),
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12),
        bordercolor = "black"
      )
    ) %>%
    config(displayModeBar = FALSE) %>%
    event_register("plotly_click")
  
  return(plotly_chart)
}


#' Create an empty chart with a message
#' @param title Chart title
#' @param message Message to display
#' @return Plotly chart object
#' @export
create_empty_chart <- function(title, message) {
  p <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = message, size = 6, color = "gray50") +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
    ) +
    labs(title = title)
  
  ggplotly(p) %>%
    config(displayModeBar = FALSE)
}


#' Create a combined district-wide summary chart
#' @param data List containing data from all programs
#' @param theme Color theme name
#' @return Plotly chart object
#' @export
create_district_summary_chart <- function(data, theme = "MMCD") {
  # Get metric registry for display names
  registry <- get_metric_registry()
  
  # Build summary from available metrics
  metrics <- names(data)
  
  summary_data <- data.frame(
    program = sapply(metrics, function(m) registry[[m]]$display_name),
    total = sapply(metrics, function(m) sum(data[[m]]$total, na.rm = TRUE)),
    active = sapply(metrics, function(m) sum(data[[m]]$active, na.rm = TRUE)),
    expiring = sapply(metrics, function(m) sum(data[[m]]$expiring, na.rm = TRUE))
  )
  
  # Calculate percentages rounded UP with no decimals
  summary_data <- summary_data %>%
    mutate(
      pct_active = ceiling(100 * active / pmax(1, total)),
      display_name = program
    )
  
  create_overview_chart(
    data = summary_data,
    title = "District-Wide Treatment Progress",
    y_label = "Count",
    theme = theme,
    metric_type = "summary"
  )
}


#' Create a percentage-based progress bar chart
#' @param data Data frame with program data
#' @param theme Color theme name
#' @return Plotly chart object
#' @export
create_percentage_chart <- function(data, theme = "MMCD") {
  if (is.null(data) || nrow(data) == 0) {
    return(create_empty_chart("Progress", "No data available"))
  }
  
  # Calculate percentages rounded UP with no decimals
  data <- data %>%
    mutate(
      pct_active = ceiling(100 * active / pmax(1, total)),
      pct_expiring = ceiling(100 * expiring / pmax(1, active)),
      tooltip_text = paste0(
        display_name, "\n",
        "Coverage: ", pct_active, "%\n",
        "Active: ", format(active, big.mark = ","), " / ", format(total, big.mark = ",")
      )
    )
  
  # Get theme colors
  status_colors <- get_status_colors(theme = theme)
  
  # Create percentage bar chart
  p <- ggplot(data, aes(x = reorder(display_name, pct_active), y = pct_active, text = tooltip_text)) +
    geom_bar(stat = "identity", fill = unname(status_colors["active"]), alpha = 0.9) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
    coord_flip() +
    scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25)) +
    labs(
      x = NULL,
      y = "% Coverage"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(face = "bold", size = 11),
      axis.title = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank()
    )
  
  ggplotly(p, tooltip = "text") %>%
    layout(margin = list(l = 80, r = 20, t = 10, b = 50)) %>%
    config(displayModeBar = FALSE)
}


#' Render a metric chart based on overview type
#' @param data The metric data
#' @param metric_config The metric configuration from registry
#' @param overview_type One of "district", "facilities", "fos"
#' @param theme Color theme
#' @return Plotly chart
#' @export
render_metric_chart <- function(data, metric_config, overview_type = "facilities", theme = "MMCD") {
  if (is.null(data) || nrow(data) == 0) {
    return(create_empty_chart(metric_config$display_name, "No data available"))
  }
  
  if (overview_type == "district") {
    create_zone_chart(
      data = data,
      title = paste(metric_config$display_name, "Progress"),
      y_label = metric_config$y_label,
      theme = theme,
      chart_id = metric_config$id
    )
  } else {
    create_overview_chart(
      data = data,
      title = paste(metric_config$display_name, "Progress"),
      y_label = metric_config$y_label,
      theme = theme,
      metric_type = metric_config$id
    )
  }
}

# =============================================================================
# HISTORICAL COMPARISON CHART - 5 Year Avg Bar + Current Year Line
# =============================================================================

#' Create a historical comparison chart with 5-year average bars and current year line
#' 
#' @param avg_data Data frame with columns: time_period, week_num, value, group_label (5-year avg)
#' @param current_data Data frame with columns: time_period, week_num, value, group_label (current year)
#' @param title Chart title
#' @param y_label Y-axis label
#' @param bar_color Color for the average bars (deprecated - use theme instead)
#' @param theme Color theme name (default: "MMCD")
#' @return Plotly chart object
#' @export
create_comparison_chart <- function(avg_data, current_data, title, y_label, bar_color = NULL, theme = "MMCD") {
  
  # Handle empty data
  has_avg <- !is.null(avg_data) && nrow(avg_data) > 0
  has_current <- !is.null(current_data) && nrow(current_data) > 0
  
  if (!has_avg && !has_current) {
    return(create_empty_chart(title, "No historical data available"))
  }
  
  # Get colors from theme
  historical_colors <- get_historical_comparison_colors(theme)
  line_color_avg <- historical_colors[["5-Year Avg"]]
  line_color_current <- historical_colors[["current_year"]]
  
  # Start with empty plot
  p <- plot_ly()
  
  # Add 5-year average as LINE (not bars)
  if (has_avg) {
    avg_label <- if (nrow(avg_data) > 0) avg_data$group_label[1] else "5-Year Avg"
    
    p <- p %>% add_trace(
      data = avg_data,
      x = ~time_period,
      y = ~value,
      name = avg_label,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = line_color_avg, width = 2, dash = "dash"),
      marker = list(color = line_color_avg, size = 6),
      hovertemplate = paste0(
        "<b>Week %{x}</b><br>",
        avg_label, ": %{y:,.0f}<br>",
        "<extra></extra>"
      )
    )
  }
  
  # Add current year as line
  if (has_current) {
    current_label <- if (nrow(current_data) > 0) current_data$group_label[1] else "Current Year"
    
    p <- p %>% add_trace(
      data = current_data,
      x = ~time_period,
      y = ~value,
      name = current_label,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = line_color_current, width = 3),
      marker = list(color = line_color_current, size = 8),
      hovertemplate = paste0(
        "<b>Week %{x}</b><br>",
        current_label, ": %{y:,.0f}<br>",
        "<extra></extra>"
      )
    )
  }
  
  # Layout - always show legend
  p %>% layout(
    xaxis = list(
      title = "",
      tickangle = -45,
      tickfont = list(size = 10)
    ),
    yaxis = list(
      title = y_label,
      rangemode = "tozero"
    ),
    legend = list(
      orientation = "h",
      y = -0.15,
      x = 0.5,
      xanchor = "center"
    ),
    showlegend = TRUE,  # Always show legend
    margin = list(l = 60, r = 20, t = 30, b = 80),
    hovermode = "x unified"
  ) %>%
    config(displayModeBar = FALSE)
}
