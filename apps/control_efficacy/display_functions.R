# display_functions.R
# Chart and visualization functions for control efficacy app
#
# Functions:
# - create_checkback_progress_chart() - Progress bars for checkback completion by brood
# - create_reduction_boxplot() - Box plots of % reduction by genus/season/year

library(ggplot2)
library(plotly)
library(dplyr)

# Source shared helper functions
source("../../shared/db_helpers.R", local = TRUE)

#' Create checkback progress chart (NEW - similar to struct_trt current progress)
#'
#' Shows one bar per brood with total checkbacks needed and completed portion filled.
#' X-axis: Brood name (Facility-Date)
#' Y-axis: Number of checkbacks
#' Bar colors: Gray background (total needed), green fill (completed)
#' Height: Dynamically calculated as 20px per bar (minimum)
#'
#' @param checkback_status Data frame from calculate_checkback_status()
#' @param theme Character. Color theme ("MMCD", "IBM", "Wong", etc.)
#'
#' @return List with plotly object and recommended height in pixels
create_checkback_progress_chart <- function(checkback_status, theme = "MMCD") {
  
  if (is.null(checkback_status) || nrow(checkback_status) == 0) {
    empty_plot <- ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No checkback data available"), size = 6) +
           theme_void()
    return(list(plot = empty_plot, height = 300))
  }
  
  # Get status colors with theme
  status_colors <- get_status_colors(theme = theme)
  
  # Prepare data - sort by date (most recent on top) and create ordered factor
  data <- checkback_status %>%
    mutate(
      total_needed = checkbacks_needed,
      completed = checkbacks_completed
    ) %>%
    arrange(desc(start_date)) %>%  # Most recent first
    mutate(round_name = factor(round_name, levels = round_name))  # Lock in order
  
  # Create the plot with plotly for hover tooltips
  p <- ggplot(data, aes(x = round_name)) +
    # Gray background shows total needed
    geom_bar(aes(y = total_needed, 
                 text = paste0("Brood: ", round_name, 
                               "<br>Total Needed: ", total_needed)), 
             stat = "identity", fill = "gray70", alpha = 0.4, width = 0.7) +
    # Green shows completed
    geom_bar(aes(y = completed,
                 text = paste0("Brood: ", round_name,
                               "<br>Completed: ", completed,
                               "<br>Total Needed: ", total_needed,
                               "<br>Percent: ", round(100 * completed / total_needed, 1), "%")), 
             stat = "identity", fill = unname(status_colors["active"]), alpha = 0.9, width = 0.7) +
    coord_flip() +
    labs(
      title = "Checkback Progress by Brood",
      x = "Brood (Facility - Date)",
      y = "Number of Checkbacks"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 18, face = "bold"),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 15),
      panel.grid.major.y = element_blank()  # Remove horizontal gridlines for cleaner look
    )
  
  # Convert to plotly for interactive hover tooltips
  p_interactive <- ggplotly(p, tooltip = "text") %>%
    layout(hovermode = "closest")
  
  # Calculate recommended height: 30px per bar for spacing, minimum 300px
  recommended_height <- max(300, nrow(data) * 30)
  
  return(list(
    plot = p_interactive,
    height = recommended_height
  ))
}

# =============================================================================
# EFFICACY TAB: Box plots for % Reduction by Genus
# =============================================================================

#' Create box plots of % reduction grouped by Year, Season, and Genus
#'
#' Each combination of Year × Season × Genus gets its own box.
#' X-axis: "Year Season" category
#' Y-axis: % Reduction (can be negative if post > pre)
#' Fill color: Genus (Aedes = red/warm, Culex = blue/cool)
#'
#' @param efficacy_data Data frame from load_efficacy_data() (long format)
#' @param theme Character. Color theme ("MMCD", "IBM", "Wong", etc.)
#'
#' @return plotly object
create_reduction_boxplot <- function(efficacy_data, theme = "MMCD", comparison_mode = "genus") {
  
  if (is.null(efficacy_data) || nrow(efficacy_data) == 0) {
    p <- ggplot() +
      geom_text(aes(x = 1, y = 1, label = "No efficacy data available"), size = 6) +
      theme_void()
    return(ggplotly(p))
  }
  
  # Filter to rows with valid % reduction and season
  plot_data <- efficacy_data %>%
    filter(!is.na(pct_reduction), !is.na(season)) %>%
    # Clamp negative reductions to 0 FOR GRAPH ONLY (tables keep original values)
    mutate(pct_reduction = pmax(pct_reduction, 0))
  
  if (nrow(plot_data) == 0) {
    p <- ggplot() +
      geom_text(aes(x = 1, y = 1, label = "No valid reduction data\n(need species data for both pre and post samples)"), size = 5) +
      theme_void()
    return(ggplotly(p))
  }
  
  # Determine fill variable and colors based on comparison mode
  fill_colors <- NULL
  if (comparison_mode == "material" && "active_ingredient" %in% names(plot_data)) {
    plot_data$fill_var <- ifelse(is.na(plot_data$active_ingredient), "Unknown", plot_data$active_ingredient)
    fill_label <- "Material"
  } else if (comparison_mode == "dosage" && "dosage_label" %in% names(plot_data)) {
    plot_data$fill_var <- ifelse(is.na(plot_data$dosage_label), "Unknown", plot_data$dosage_label)
    fill_label <- "Dosage"
  } else {
    plot_data$fill_var <- plot_data$genus
    fill_label <- "Genus"
    fill_colors <- c("Aedes" = "#D32F2F", "Culex" = "#1976D2")
  }
  
  # Build x-axis categories with sample counts
  plot_data$base_cat <- paste(plot_data$year, plot_data$season)
  cat_counts <- plot_data %>%
    group_by(base_cat) %>%
    summarise(cat_n = dplyr::n(), .groups = "drop")
  sorted_cats <- sort(unique(plot_data$base_cat))
  label_map <- setNames(
    paste0(sorted_cats, "\n(n=", cat_counts$cat_n[match(sorted_cats, cat_counts$base_cat)], ")"),
    sorted_cats
  )
  plot_data$category <- factor(label_map[plot_data$base_cat], levels = label_map)
  
  # Smart y-axis limits
  y_lo <- -5
  y_hi <- 110
  n_outside <- sum(plot_data$pct_reduction < y_lo | plot_data$pct_reduction > y_hi, na.rm = TRUE)
  
  subtitle_text <- "Dashed = 0% (no change) | Dotted green = 80% target"
  if (n_outside > 0) {
    subtitle_text <- paste0(subtitle_text, "  |  ", n_outside, " outlier(s) beyond axis limits")
  }
  
  p <- ggplot(plot_data, aes(x = category, y = pct_reduction, fill = fill_var)) +
    geom_boxplot(
      position = position_dodge(width = 0.8),
      alpha = 0.7,
      outlier.shape = 16,
      outlier.alpha = 0.4,
      outlier.size = 1.5,
      width = 0.7
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_hline(yintercept = 80, linetype = "dotted", color = "forestgreen", linewidth = 0.7, alpha = 0.7) +
    scale_y_continuous(
      breaks = seq(-10, 100, by = 10),
      labels = function(x) paste0(x, "%")
    ) +
    coord_cartesian(ylim = c(y_lo, y_hi)) +
    labs(
      title = paste("% Reduction by", fill_label, "/ Season / Year"),
      subtitle = subtitle_text,
      x = "",
      y = "% Reduction"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14, angle = 30, hjust = 1),
      axis.text.y = element_text(size = 13),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13),
      panel.grid.minor = element_blank()
    )
  
  # Apply fill colors
  if (!is.null(fill_colors)) {
    p <- p + scale_fill_manual(values = fill_colors, name = fill_label)
  } else {
    p <- p + labs(fill = fill_label)
  }
  
  p_plotly <- ggplotly(p) %>%
    layout(
      hovermode = "closest",
      boxmode = "group"
    )
  
  return(p_plotly)
}

