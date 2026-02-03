# Overview - Display Functions
# =============================================================================
# Shared functions for creating charts and visualizations for overview dashboards.
# These work with any group_by type (zone, facility, fos).
# =============================================================================

#' Create a legend HTML element for overview charts
#' Shows what the colors mean based on current theme
#' @param theme Color theme name
#' @param metric_id Optional metric ID for custom labels
#' @return HTML string for legend
#' @export
create_overview_legend <- function(theme = "MMCD", metric_id = NULL) {
  status_colors <- get_status_colors(theme = theme)
  
  # Default labels - can be customized per metric
  total_label <- "Total Sites"
  active_label <- "Treated"
  expiring_label <- "Expiring"
  
  # Customize labels for specific metrics
  if (!is.null(metric_id)) {
    if (metric_id == "cattail_treatments") {
      total_label <- "Total Sites Inspected"
      active_label <- "Treated"
      expiring_label <- "Needs Treatment"
    } else if (metric_id == "catch_basin_status") {
      active_label <- "Treated"
      expiring_label <- "Expiring"
    } else if (metric_id == "drone") {
      active_label <- "Active Treatments"
      expiring_label <- "Expiring"
      total_label <- "Total acres"
    }else if (metric_id == "ground_prehatch") {
      total_label <- "Total acres"
      active_label <- "Active Treatments"
      expiring_label <- "Expiring"
    }else if (metric_id == "mosquito_monitoring") {
      total_label <- "10-Year Average"
      active_label <- "Current Week"
      expiring_label <- "Above Average"
    }
  }
  
  HTML(paste0(
    '<div style="display: flex; gap: 20px; justify-content: center; padding: 8px 0; font-size: 16px; font-weight: 600;">',
    '<span><span style="display: inline-block; width: 16px; height: 16px; background: #D32F2F; opacity: 0.3; margin-right: 8px; border: 1px solid #999;"></span>', total_label, '</span>',
    '<span><span style="display: inline-block; width: 16px; height: 16px; background: ', unname(status_colors["active"]), '; margin-right: 8px; border: 1px solid #999;"></span>', active_label, '</span>',
    '<span><span style="display: inline-block; width: 16px; height: 16px; background: ', unname(status_colors["planned"]), '; margin-right: 8px; border: 1px solid #999;"></span>', expiring_label, '</span>',
    '</div>'
  ))
}

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
  
  # Order by total descending (highest total at top) - matches original apps
  data$display_name <- reorder(data$display_name, -data$y_total)
  
  # Create base ggplot with layered bars
  # For cattail_treatments: Green (treated) at bottom, showing progress "filling up"
  # For all other metrics: standard layered bars (red total, orange expiring, green active)
  if (metric_type == "cattail_treatments") {
    # Cattail: Show treated (green at bottom) + needs treatment (orange at top) on red background (visible around edges)
    # The red background should show the full total height with less opacity so layers are visible on top
    p <- ggplot(data, aes(x = display_name)) +
      # Red background - total sites that need/needed treatment (very light, visible behind)
      geom_bar(aes(y = y_total, text = tooltip_text), 
               stat = "identity", fill = "#D32F2F", alpha = 0.15) +
      # Orange layer - needs treatment (visible above treated)
      geom_bar(aes(y = y_expiring + y_active), 
               stat = "identity", fill = unname(status_colors["planned"]), alpha = 1) +
      # Green layer - treated sites (at bottom, covers orange up to treated level)
      geom_bar(aes(y = y_active), 
               stat = "identity", fill = unname(status_colors["active"]), alpha = 0.9) +
      coord_flip() +
      labs(x = NULL, y = y_label)
  } else {
    # Standard: Red total -> Green active on top -> Orange expiring at bottom
    p <- ggplot(data, aes(x = display_name)) +
      # Red background - total/untreated
      geom_bar(aes(y = y_total, text = tooltip_text), 
               stat = "identity", fill = "#D32F2F", alpha = 0.3) +
      # Orange layer - expiring (full height including active)
      geom_bar(aes(y = y_expiring + y_active), 
               stat = "identity", fill = unname(status_colors["planned"]), alpha = 1) +
      # Green layer - active (at bottom/left of expiring)
      geom_bar(aes(y = y_active), 
               stat = "identity", fill = unname(status_colors["active"]), alpha = 0.9) +
      coord_flip() +
      labs(x = NULL, y = y_label)
  }
  
  p <- p +
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
    # Red background - total/untreated
    geom_bar(aes(y = y_total, text = tooltip_text), 
             stat = "identity", fill = "#D32F2F", alpha = 0.3) +
    # Orange layer - expiring (full height including active)
    geom_bar(aes(y = y_expiring + y_active), 
             stat = "identity", fill = unname(status_colors["planned"]), alpha = 1) +
    # Green layer - active (at the bottom of the expiring portion)
    geom_bar(aes(y = y_active), 
             stat = "identity", fill = unname(status_colors["active"]), alpha = 0.9) +
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

#' Create pie charts showing treatment progress (Total → Treated → Expiring)
#' Creates separate pie charts for each zone/facility group
#' @param data Data frame with columns: display_name, total, active, expiring 
#' @param title Chart title
#' @param theme Color theme name
#' @param metric_type Type of metric for specific styling
#' @return Plotly chart object with multiple pie charts
#' @export
create_overview_pie_chart <- function(data, title, theme = "MMCD", metric_type = "default") {
  if (is.null(data) || nrow(data) == 0) {
    return(create_empty_chart(title, "No data available"))
  }
  
  status_colors <- get_status_colors(theme = theme)
  
  # Use the data as-is - respect whether it's combined or separate zones
  # If data has 1 row (combined), show 1 pie
  # If data has 2 rows (P1/P2), show 2 pies
  complete_data <- data
  
  # Create pie chart data for each group (zone/facility)
  pie_data_list <- list()
  
  for (i in 1:nrow(complete_data)) {
    row <- complete_data[i, ]
    group_name <- row$display_name
    
    # Use same logic as bar charts for segment calculation
    # Total gray background, active treatments (green), expiring treatments (orange)
    
    untreated <- max(0, row$total - row$active)  # Gray - sites not treated
    active_stable <- max(0, row$active - row$expiring)  # Green - active but not expiring
    expiring <- row$expiring  # Orange - expiring treatments
    
    # Create segments (include zero values for empty zones)
    segments <- list()
    if (untreated > 0) {
      segments[["Untreated"]] <- list(value = untreated, color = "lightgray")
    }
    if (active_stable > 0) {
      segments[["Treated"]] <- list(value = active_stable, color = status_colors["active"])
    }
    if (expiring > 0) {
      segments[["Expiring"]] <- list(value = expiring, color = status_colors["planned"])
    }
    
    # If no segments (all zero), create a "No Data" segment that's visible
    if (length(segments) == 0) {
      segments[["No Data"]] <- list(value = 100, color = "#f0f0f0")
    }
    
    pie_segments <- data.frame(
      category = names(segments),
      values = sapply(segments, function(x) x$value),
      colors = sapply(segments, function(x) x$color),
      stringsAsFactors = FALSE
    )
    pie_data_list[[group_name]] <- pie_segments
  }
  
  if (length(pie_data_list) == 0) {
    return(create_empty_chart(title, "No data to display"))
  }
  
  # Create pie charts
  n_pies <- length(pie_data_list)
  group_names <- names(pie_data_list)
  
  if (n_pies == 1) {
    # Single pie chart
    pie_data <- pie_data_list[[1]]
    
    p <- plot_ly(
      pie_data,
      labels = ~category,
      values = ~values,
      type = 'pie',
      textinfo = 'label+percent',
      textposition = 'auto',
      marker = list(colors = pie_data$colors),
      hovertemplate = paste0(
        "<b>%{label}</b><br>",
        "Count: %{value}<br>",
        "Percentage: %{percent}<br>",
        "<extra></extra>"
      )
    ) %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      showlegend = TRUE,
      margin = list(t = 50, b = 20, l = 20, r = 20)
    ) %>%
    config(displayModeBar = FALSE)
    
  } else {
    # Multiple pie charts - need to manually set domain for each pie
    # Plotly subplot doesn't work well with pie charts, so we create one plot with multiple traces
    
    p <- plot_ly()
    
    for (i in 1:n_pies) {
      pie_data <- pie_data_list[[i]]
      group_name <- group_names[i]
      
      # Calculate domain for this pie (side by side)
      x_start <- (i - 1) / n_pies + 0.02
      x_end <- i / n_pies - 0.02
      
      p <- p %>% add_pie(
        data = pie_data,
        labels = ~category,
        values = ~values,
        textinfo = 'percent',
        textposition = 'auto',
        marker = list(colors = pie_data$colors),
        domain = list(x = c(x_start, x_end), y = c(0.1, 0.9)),
        name = group_name,
        hovertemplate = paste0(
          "<b>", group_name, " - %{label}</b><br>",
          "Value: %{value:,.1f}<br>",
          "Percentage: %{percent}<br>",
          "<extra></extra>"
        )
      )
    }
    
    # Add annotations for P1, P2 labels
    p <- p %>%
      layout(
        title = list(text = title, font = list(size = 16)),
        showlegend = TRUE,
        annotations = lapply(1:n_pies, function(i) {
          list(
            x = (i - 0.5) / n_pies,
            y = 0,
            text = paste0("<b>", group_names[i], "</b>"),
            showarrow = FALSE,
            font = list(size = 14),
            xref = "paper",
            yref = "paper"
          )
        }),
        margin = list(t = 70, b = 50, l = 20, r = 20)
      ) %>%
      config(displayModeBar = FALSE)
  }
  
  return(p)
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
#' @param ten_year_avg_data Optional: Data frame for 10-year average line
#' @param title Chart title
#' @param y_label Y-axis label
#' @param bar_color Color for the average bars (deprecated - use theme instead)
#' @param theme Color theme name (default: "MMCD")
#' @return Plotly chart object
#' @export
create_comparison_chart <- function(avg_data, current_data, title, y_label, bar_color = NULL, theme = "MMCD", ten_year_avg_data = NULL) {
  
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
  
  # Add 10-year average as line (if provided)
  has_ten_year <- !is.null(ten_year_avg_data) && nrow(ten_year_avg_data) > 0
  if (has_ten_year) {
    ten_year_label <- if (nrow(ten_year_avg_data) > 0) ten_year_avg_data$group_label[1] else "10-Year Avg"
    
    p <- p %>% add_trace(
      data = ten_year_avg_data,
      x = ~time_period,
      y = ~value,
      name = ten_year_label,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#9C27B0", width = 2, dash = "dot"),
      marker = list(color = "#9C27B0", size = 5),
      hovertemplate = paste0(
        "<b>Week %{x}</b><br>",
        ten_year_label, ": %{y:,.0f}<br>",
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

# =============================================================================
# YEARLY GROUPED BAR CHART - For cattail treatments and similar metrics
# =============================================================================

#' Create a yearly grouped bar chart showing yearly totals by group
#' With 5-year and 10-year average lines
#' 
#' @param data Data frame with columns: year, group_label, value
#' @param title Chart title
#' @param y_label Y-axis label
#' @param theme Color theme name
#' @param overview_type "district" (MMCD total) or "facilities" (by facility)
#' @return Plotly chart object
#' @export
create_yearly_grouped_chart <- function(data, title, y_label, theme = "MMCD", overview_type = "facilities") {
  
  if (is.null(data) || nrow(data) == 0) {
    return(create_empty_chart(title, "No historical data available"))
  }
  
  # Get facility colors from theme
  facility_colors <- get_facility_base_colors(theme = theme)
  
  # For district overview, aggregate to MMCD total
  if (overview_type == "district") {
    data <- data %>%
      group_by(year) %>%
      summarise(
        value = sum(value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(group_label = "MMCD Total")
  }
  
  # Ensure year is numeric for calculations
  data <- data %>%
    mutate(year_num = as.numeric(year))
  
  # Calculate averages using YEARLY TOTALS (not per-facility averages)
  current_year <- max(data$year_num, na.rm = TRUE)
  
  # First, aggregate to yearly totals across all groups
  yearly_totals <- data %>%
    group_by(year_num) %>%
    summarise(yearly_total = sum(value, na.rm = TRUE), .groups = "drop")
  
  # 5-year average (last 5 years including current) - average of yearly totals
  five_year_data <- yearly_totals %>% 
    filter(year_num >= (current_year - 4))
  five_year_avg <- mean(five_year_data$yearly_total, na.rm = TRUE)
  
  # 10-year average (last 10 years including current) - average of yearly totals
  ten_year_data <- yearly_totals %>% 
    filter(year_num >= (current_year - 9))
  ten_year_avg <- mean(ten_year_data$yearly_total, na.rm = TRUE)
  
  # Convert year to character for x-axis display
  data <- data %>%
    mutate(year = as.character(year))
  
  # Get unique groups and assign colors
  groups <- unique(data$group_label)
  n_groups <- length(groups)
  
  if (n_groups == 1) {
    # Single group - use theme primary color
    group_colors <- setNames("#ff9500", groups)  # Cattail orange
  } else {
    # Multiple groups - use facility colors
    group_colors <- setNames(
      facility_colors[1:min(n_groups, length(facility_colors))],
      groups[1:min(n_groups, length(facility_colors))]
    )
    # Fill any remaining with defaults
    if (n_groups > length(facility_colors)) {
      extra_colors <- scales::hue_pal()(n_groups - length(facility_colors))
      for (i in (length(facility_colors) + 1):n_groups) {
        group_colors[groups[i]] <- extra_colors[i - length(facility_colors)]
      }
    }
  }
  
  # Create plotly grouped bar chart
  p <- plot_ly()
  
  # Add bars for each group
  for (grp in groups) {
    grp_data <- data %>% filter(group_label == grp)
    p <- p %>% add_trace(
      data = grp_data,
      x = ~year,
      y = ~value,
      name = grp,
      type = "bar",
      marker = list(color = unname(group_colors[grp])),
      hovertemplate = paste0(
        "<b>", grp, " - %{x}</b><br>",
        y_label, ": %{y:,.0f}<br>",
        "<extra></extra>"
      )
    )
  }
  
  # Get x-axis range for average lines
  years <- unique(data$year)
  
  # Add 5-year average line (dashed blue)
  p <- p %>% add_trace(
    x = years,
    y = rep(five_year_avg, length(years)),
    name = paste0("5-Yr Avg (", format(round(five_year_avg, 0), big.mark = ","), ")"),
    type = "scatter",
    mode = "lines",
    line = list(color = "#2196F3", width = 2, dash = "dash"),
    hovertemplate = paste0("5-Year Avg: ", format(round(five_year_avg, 0), big.mark = ","), "<extra></extra>")
  )
  
  # Add 10-year average line (dotted purple)
  p <- p %>% add_trace(
    x = years,
    y = rep(ten_year_avg, length(years)),
    name = paste0("10-Yr Avg (", format(round(ten_year_avg, 0), big.mark = ","), ")"),
    type = "scatter",
    mode = "lines",
    line = list(color = "#9C27B0", width = 2, dash = "dot"),
    hovertemplate = paste0("10-Year Avg: ", format(round(ten_year_avg, 0), big.mark = ","), "<extra></extra>")
  )
  
  # Layout with legend - remove "Year" title from x-axis
  p %>% layout(
    barmode = "group",
    xaxis = list(
      title = "",  # No "Year" label - just show years
      tickfont = list(size = 12),
      categoryorder = "category ascending"
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
    showlegend = TRUE,
    margin = list(l = 60, r = 20, t = 30, b = 80)
  ) %>%
    config(displayModeBar = FALSE)
}
