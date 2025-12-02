# Catch Basin Status - Display Functions
# Functions for creating charts, tables, and visualizations

library(stringr)

# Function to create status chart 
create_status_chart <- function(data, group_by = "facility", expiring_filter = "all") {
  if (is.null(data) || nrow(data) == 0) {
    plot <- ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No data available"), size = 6) +
           theme_void()
    return(plot)
  }
  
  # Get status colors from db_helpers
  status_colors <- get_status_colors()
  
  # Prepare y variables for layered bars with status-based colors
  # Gray layer: ALL wet catch basins (total) - shows all untreated/expired
  # Green layer: Active treatments (not expiring)
  # Orange layer: Expiring treatments
  data <- data %>%
    mutate(
      y_total = wet_cb_count,                                      # ALL wet catch basins (background)
      y_active = count_wet_activetrt - count_wet_expiring,         # Treated only (not expiring)
      y_expiring = count_wet_expiring                              # Just expiring sites
    )
  
  # Create layered plot with status-based colors (not facility colors)
  # Gray background shows total, green shows active treatment, orange shows expiring
  p <- ggplot(data, aes(x = reorder(display_name, y_total))) +
    geom_bar(aes(y = y_total), stat = "identity", fill = "gray70", alpha = 0.4) +                         # Gray background
    geom_bar(aes(y = y_active), stat = "identity", fill = unname(status_colors["active"]), alpha = 0.9) + # Green active
    geom_bar(aes(y = y_expiring), stat = "identity", fill = unname(status_colors["planned"]), alpha = 1)  # Orange expiring
  
  p <- p +
    coord_flip() +
    labs(
      title = "Catch Basin Treatment Status",
      x = case_when(
        group_by == "mmcd_all" ~ "MMCD",
        group_by == "facility" ~ "Facility",
        group_by == "foreman" ~ "FOS",
        group_by == "sectcode" ~ "Section"
      ),
      y = "Number of Wet Catch Basins"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      axis.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 13),
      legend.title = element_blank(),
      legend.text = element_text(size = 16),
      legend.position = "bottom"
    )
  
  # Add status color legend
  legend_data <- data.frame(
    status = factor(
      c("Expired/Untreated", "Active Treatment", "Expiring Soon"),
      levels = c("Expired/Untreated", "Active Treatment", "Expiring Soon")
    ),
    color = c("gray70", unname(status_colors["active"]), unname(status_colors["planned"]))
  )
  
  p <- p +
    geom_point(
      data = legend_data,
      aes(x = -Inf, y = -Inf, fill = status),
      shape = 21,
      size = 10,
      alpha = 1,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      name = NULL,
      values = setNames(legend_data$color, legend_data$status),
      breaks = legend_data$status,
      limits = legend_data$status,
      drop = FALSE
    )
  
  # Convert to plotly
  plot <- ggplotly(p, tooltip = "text")
  
  return(plot)
}

# Function to format data table for display
format_details_table <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame(Message = "No data available"))
  }
  
  # Calculate percentages and format - format strings AFTER calculating percentages
  display_data <- data %>%
    mutate(
      pct_coverage = round(pct_treated, 1)
    ) %>%
    select(
      Group = display_name,
      `Total Wet CB` = wet_cb_count,
      `Treated` = count_wet_activetrt,
      `Expiring` = count_wet_expiring,
      `Expired` = count_wet_expired,
      `Never Treated` = untreated_count,
      `% Coverage` = pct_coverage
    ) %>%
    mutate(
      `Total Wet CB` = format(`Total Wet CB`, big.mark = ","),
      `Treated` = format(`Treated`, big.mark = ","),
      `Expiring` = format(`Expiring`, big.mark = ","),
      `Expired` = format(`Expired`, big.mark = ","),
      `Never Treated` = format(`Never Treated`, big.mark = ",")
    )
  
  return(display_data)
}

# Function to calculate summary statistics
calculate_summary_stats <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      total_wet = 0,
      total_treated = 0,
      pct_treated = 0
    ))
  }
  
  total_wet <- sum(data$wet_cb_count, na.rm = TRUE)
  total_treated <- sum(data$count_wet_activetrt, na.rm = TRUE)
  pct_treated <- if (total_wet > 0) (total_treated / total_wet) * 100 else 0
  
  return(list(
    total_wet = total_wet,
    total_treated = total_treated,
    pct_treated = pct_treated
  ))
}
