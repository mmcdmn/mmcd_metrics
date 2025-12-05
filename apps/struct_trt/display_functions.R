# Structure Treatment - Display Functions
# Functions for creating charts and visualizations

# Function to create current progress chart
create_current_progress_chart <- function(data, group_by, facility_filter, status_types, zone_filter, combine_zones = FALSE, theme = "MMCD") {
  if (nrow(data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No data available"), size = 6) +
           theme_void())
  }
  
  # Get status colors from db_helpers with theme
  status_colors <- get_status_colors(theme = theme)
  
  # Prepare data for plotting with status-based colors
  data <- data %>%
    mutate(
      y_total = total_structures,
      y_active = active_structures - expiring_structures,  # Active only (not expiring)
      y_expiring = expiring_structures
    )
  
  # Determine plot group column - always use display_name for consistency
  plot_group_col <- "display_name"
  
  # Create a new column to determine which labels to show (avoiding overplot)
  data$show_active_label <- data$y_active != data$y_expiring
  
  # Calculate y-axis maximum for proper positioning
  y_max <- max(data$y_total) * 1.1
  
  # Set up the title with appropriate filters
  status_types_text <- paste(status_types, collapse = ", ")
  facility_text <- if (is.null(facility_filter) || ("all" %in% facility_filter)) {
    "All Facilities"
  } else {
    paste("Facilities:", paste(facility_filter, collapse = ", "))
  }
  zone_text <- if (length(zone_filter) == 0) {
    "No Zones"
  } else if (length(zone_filter) == 1) {
    paste("Zone: P", zone_filter)
  } else {
    paste("Zone:", paste0("P", zone_filter))
  }
  
  
  # Create the plot with status-based colors (not facility colors)
  # Gray background shows total, green shows active treatment, orange shows expiring
  p <- ggplot(data, aes(x = display_name)) +
    geom_bar(aes(y = y_total), stat = "identity", fill = "gray70", alpha = 0.4) +                         # Gray background
    geom_bar(aes(y = y_active), stat = "identity", fill = unname(status_colors["active"]), alpha = 0.9) + # Green active
    geom_bar(aes(y = y_expiring), stat = "identity", fill = unname(status_colors["planned"]), alpha = 1)  # Orange expiring
  
  # Add text labels with larger size
  p <- p +
    # Label total structures
    geom_text(aes(x = display_name, y = y_total, label = y_total), 
              vjust = -0.5, size = 7, fontface = "bold") +
    coord_flip() +
    labs(
      title = sprintf("Structures with Active and Expiring Treatments (%s, Status: %s, %s)",
                     facility_text, status_types_text, zone_text),
      x = case_when(
        group_by == "facility" ~ "Facility",
        group_by == "foreman" ~ "FOS",
        group_by == "mmcd_all" ~ "MMCD",
        TRUE ~ "Group"
      ),
      y = "Number of Structures"
    ) +
    scale_y_continuous(limits = c(0, y_max)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 20),
      axis.title = element_text(face = "bold", size = 18),
      axis.title.x = element_text(face = "bold", size = 20),
      axis.text = element_text(size = 18, face = "bold"),
      axis.text.x = element_text(size = 18, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      legend.title = element_text(size = 18, face = "bold"),
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
      shape = 22,
      size = 6,
      alpha = 1,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      name = "Treatment Status",
      values = setNames(legend_data$color, legend_data$status),
      breaks = legend_data$status,
      limits = legend_data$status,
      drop = FALSE
    ) +
    guides(fill = guide_legend(
      override.aes = list(size = 8, shape = 22),
      title.position = "top",
      title.hjust = 0.5
    ))
  
  return(p)
}