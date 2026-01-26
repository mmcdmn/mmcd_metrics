# District Overview - Display Functions
# Functions for creating charts and visualizations for the overview dashboard

#' Create an overview chart showing treatment progress
#' @param data Data frame with columns: facility, total, active, expiring, display_name
#' @param title Chart title
#' @param y_label Y-axis label
#' @param theme Color theme name
#' @param metric_type Type of metric for specific styling
#' @return Plotly chart object
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
      # Tooltips
      tooltip_text = paste0(
        display_name, "\n",
        "Total: ", format(total, big.mark = ","), "\n",
        "Active: ", format(active, big.mark = ","), 
        " (", round(100 * active / pmax(1, total), 1), "%)\n",
        "Expiring: ", format(expiring, big.mark = ",")
      )
    )
  
  # Ensure display_name is ordered correctly (if it's a factor, use its levels; otherwise sort by total)
  if (!is.factor(data$display_name)) {
    data$display_name <- reorder(data$display_name, data$y_total)
  }
  # If it IS a factor (from order_facilities), keep the predefined order
  
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


#' Create an empty chart with a message
#' @param title Chart title
#' @param message Message to display
#' @return Plotly chart object
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
create_district_summary_chart <- function(data, theme = "MMCD") {
  # Aggregate totals from each program
  summary_data <- data.frame(
    program = c("Catch Basins", "Drone Sites", "Ground Prehatch", "Structures"),
    total = c(
      sum(data$catch_basin$total, na.rm = TRUE),
      sum(data$drone$total, na.rm = TRUE),
      sum(data$ground_prehatch$total, na.rm = TRUE),
      sum(data$structure$total, na.rm = TRUE)
    ),
    active = c(
      sum(data$catch_basin$active, na.rm = TRUE),
      sum(data$drone$active, na.rm = TRUE),
      sum(data$ground_prehatch$active, na.rm = TRUE),
      sum(data$structure$active, na.rm = TRUE)
    ),
    expiring = c(
      sum(data$catch_basin$expiring, na.rm = TRUE),
      sum(data$drone$expiring, na.rm = TRUE),
      sum(data$ground_prehatch$expiring, na.rm = TRUE),
      sum(data$structure$expiring, na.rm = TRUE)
    )
  )
  
  # Calculate percentages
  summary_data <- summary_data %>%
    mutate(
      pct_active = round(100 * active / pmax(1, total), 1),
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
create_percentage_chart <- function(data, theme = "MMCD") {
  if (is.null(data) || nrow(data) == 0) {
    return(create_empty_chart("Progress", "No data available"))
  }
  
  # Calculate percentages
  data <- data %>%
    mutate(
      pct_active = round(100 * active / pmax(1, total), 1),
      pct_expiring = round(100 * expiring / pmax(1, active), 1),
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

#' Create a zone-level chart showing P1 vs P2 with click capability
#' Used by the top-level District Overview dashboard
#' @param data Data frame with columns: zone, display_name, total, active, expiring
#' @param title Chart title
#' @param y_label Y-axis label
#' @param theme Color theme name
#' @param chart_id Unique ID for this chart (used for plotly source)
#' @return Plotly chart object with click event support
create_zone_chart <- function(data, title, y_label, theme = "MMCD", chart_id = "chart") {
  if (is.null(data) || nrow(data) == 0) {
    return(create_empty_chart(title, "No data available"))
  }
  
  # Get status colors from db_helpers with theme support
  status_colors <- get_status_colors(theme = theme)
  
  # Prepare data for layered bars
  data <- data %>%
    mutate(
      y_total = total,
      y_active = pmax(0, active - expiring),  # Active only (not including expiring)
      y_expiring = expiring,
      pct = round(100 * active / pmax(1, total), 1),
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
  plotly_chart <- ggplotly(p, tooltip = "text", source = chart_id) %>%
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
