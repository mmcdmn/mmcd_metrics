# display_functions.R
# Chart and visualization functions for control efficacy app
#
# Functions:
# - create_checkback_progress_chart() - NEW: Progress bars for checkback completion by brood
# - create_treatment_timeline_chart() - Timeline of treatment activity
# - create_checkback_timing_chart() - Distribution of days to checkback
# - create_efficacy_scatter_chart() - Pre vs post treatment dip counts
# - create_dip_changes_chart() - Interactive pre/post comparison

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

#' Create treatment timeline chart
#'
#' Shows daily treatment activity by facility over time.
#'
#' @param treatments Data frame from load_treatment_data()
#' @param theme Character. Color theme
#'
#' @return ggplot object
create_treatment_timeline_chart <- function(treatments, theme = "MMCD") {
  
  if (is.null(treatments) || nrow(treatments) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 0, y = 0, label = "No treatment data available"), size = 6) +
           theme_void())
  }
  
  # Daily site counts by facility
  daily_summary <- treatments %>%
    group_by(inspdate, facility) %>%
    summarise(sites_treated = n(), .groups = "drop") %>%
    map_facility_names()
  
  # Get facility colors with theme
  facility_colors <- get_facility_base_colors(theme = theme)
  facilities <- get_facility_lookup()
  facility_map <- setNames(facilities$full_name, facilities$short_name)
  display_colors <- setNames(facility_colors, facility_map[names(facility_colors)])
  
  p <- ggplot(daily_summary, aes(x = inspdate, y = sites_treated, fill = facility_display)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = display_colors) +
    labs(
      title = "Air Treatment Activity Over Time",
      x = "Date",
      y = "Sites Treated",
      fill = "Facility"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 18, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
      axis.text.y = element_text(size = 15),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 15)
    )
  
  return(p)
}

#' Create checkback timing distribution chart
#'
#' Histogram showing days between treatment and first checkback.
#'
#' @param treatments Treatment data frame
#' @param checkbacks Checkback data frame
#' @param theme Character. Color theme
#'
#' @return ggplot object
create_checkback_timing_chart <- function(treatments, checkbacks, theme = "MMCD") {
  
  if (is.null(treatments) || is.null(checkbacks) || nrow(treatments) == 0 || nrow(checkbacks) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 0, y = 0, label = "No checkback data"), size = 4) +
           theme_void())
  }
  
  # Calculate days between treatment and checkback
  timing_data <- list()
  
  for (site in unique(treatments$sitecode)) {
    site_treatments <- treatments[treatments$sitecode == site, ]
    site_checkbacks <- checkbacks[checkbacks$sitecode == site, ]
    
    if (nrow(site_checkbacks) > 0) {
      for (i in 1:nrow(site_treatments)) {
        treatment_date <- site_treatments$inspdate[i]
        future_checkbacks <- site_checkbacks[site_checkbacks$inspdate > treatment_date, ]
        
        if (nrow(future_checkbacks) > 0) {
          first_checkback <- future_checkbacks[which.min(future_checkbacks$inspdate), ]
          days_diff <- as.numeric(first_checkback$inspdate - treatment_date)
          
          timing_data[[length(timing_data) + 1]] <- data.frame(
            sitecode = site,
            facility = site_treatments$facility[i],
            days_to_checkback = days_diff
          )
        }
      }
    }
  }
  
  if (length(timing_data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 0, y = 0, label = "No checkbacks found"), size = 4) +
           theme_void())
  }
  
  timing_df <- do.call(rbind, timing_data)
  
  # Get colors with theme
  status_colors <- get_status_colors(theme = theme)
  
  p <- ggplot(timing_df, aes(x = days_to_checkback)) +
    geom_histogram(binwidth = 2, fill = status_colors["active"], alpha = 0.7) +
    labs(
      title = "Days to Checkback",
      x = "Days After Treatment",
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 15)
    )
  
  return(p)
}

#' Create efficacy scatter plot
#'
#' Scatter plot of pre-treatment vs post-treatment dip counts.
#'
#' @param site_details Data frame from create_site_details()
#' @param theme Character. Color theme
#'
#' @return ggplot object
create_efficacy_scatter_chart <- function(site_details, theme = "MMCD") {
  
  if (is.null(site_details) || nrow(site_details) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 0, y = 0, label = "No data available"), size = 6) +
           theme_void())
  }
  
  # Filter to sites with both pre and post treatment data
  efficacy_df <- site_details %>%
    filter(!is.na(pre_treatment_dips) & !is.na(post_treatment_dips))
  
  if (nrow(efficacy_df) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 0, y = 0, label = "No sites with complete data"), size = 5) +
           theme_void())
  }
  
  # Get facility colors with theme
  facility_colors <- get_facility_base_colors(theme = theme)
  facilities <- get_facility_lookup()
  facility_map <- setNames(facilities$full_name, facilities$short_name)
  display_colors <- setNames(facility_colors, facility_map[names(facility_colors)])
  
  # Map facility names
  efficacy_df <- efficacy_df %>%
    map_facility_names()
  
  # Add reference line data
  max_val <- max(c(efficacy_df$pre_treatment_dips, efficacy_df$post_treatment_dips), na.rm = TRUE)
  
  p <- ggplot(efficacy_df, aes(x = pre_treatment_dips, y = post_treatment_dips, 
                                color = facility_display, text = sitecode)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = display_colors) +
    labs(
      title = "Treatment Efficacy: Pre vs Post Dip Counts",
      x = "Pre-Treatment Dip Count",
      y = "Post-Treatment Dip Count",
      color = "Facility"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 15),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 15)
    )
  
  return(p)
}

#' Create interactive dip changes chart (plotly)
#'
#' Shows pre/post treatment dip counts for each site with connecting lines.
#'
#' @param site_details Data frame from create_site_details()
#' @param theme Character. Color theme
#'
#' @return plotly object
#' Create dip changes chart (Enhanced)
#'
#' Shows pre-treatment vs post-treatment dip counts with:
#' - Boxplot overlays for distribution
#' - Dumbbell plot connecting paired observations
#' - Color gradient by days to checkback
#' - Interactive tooltips with site details
#'
#' @param site_details Data frame from create_site_details()
#' @param theme Character. Color theme
#'
#' @return plotly object
create_dip_changes_chart <- function(site_details, theme = "MMCD") {
  
  if (is.null(site_details) || nrow(site_details) == 0) {
    p <- ggplot() + 
      geom_text(aes(x = 0, y = 0, label = "No data available"), size = 5) +
      theme_void()
    return(ggplotly(p))
  }
  
  # Filter to sites with valid dip counts (all rows already have checkbacks)
  plot_data <- site_details %>%
    filter(!is.na(pre_treatment_dips), 
           !is.na(post_treatment_dips))
  
  if (nrow(plot_data) == 0) {
    p <- ggplot() + 
      geom_text(aes(x = 0, y = 0, label = "No sites with complete checkback data"), size = 5) +
      theme_void()
    return(ggplotly(p))
  }
  
  # Reshape for pre/post comparison (dumbbell plot needs long format)
  plot_df <- plot_data %>%
    select(sitecode, facility, pre_treatment_dips, post_treatment_dips, 
           days_to_checkback, acres) %>%
    tidyr::pivot_longer(
      cols = c(pre_treatment_dips, post_treatment_dips),
      names_to = "type",
      values_to = "dip"
    ) %>%
    mutate(
      type = ifelse(type == "pre_treatment_dips", "Pre-Treatment", "First Checkback"),
      type = factor(type, levels = c("Pre-Treatment", "First Checkback")),
      # Create tooltip text
      tooltip_text = paste0(
        "Site: ", sitecode, "<br>",
        "Facility: ", facility, "<br>",
        "Dips: ", dip, "<br>",
        ifelse(type == "First Checkback", 
               paste0("Days to Checkback: ", round(days_to_checkback, 1), "<br>"),
               ""),
        "Acres: ", round(acres, 1)
      )
    )
  
  # Calculate quartiles for smart y-axis limiting
  q1_pre <- quantile(plot_df$dip[plot_df$type == "Pre-Treatment"], 0.25, na.rm = TRUE)
  q3_pre <- quantile(plot_df$dip[plot_df$type == "Pre-Treatment"], 0.75, na.rm = TRUE)
  iqr_pre <- q3_pre - q1_pre
  
  q1_post <- quantile(plot_df$dip[plot_df$type == "First Checkback"], 0.25, na.rm = TRUE)
  q3_post <- quantile(plot_df$dip[plot_df$type == "First Checkback"], 0.75, na.rm = TRUE)
  iqr_post <- q3_post - q1_post
  
  # Use the higher Q3 and IQR for upper limit
  y_top <- max(q3_pre, q3_post, na.rm = TRUE)
  iqr_top <- max(iqr_pre, iqr_post, na.rm = TRUE)
  y_lim_top <- ceiling(y_top + max(0.25 * iqr_top, 2))
  
  # Get theme colors
  status_colors <- get_status_colors(theme = theme)
  
  # Create dumbbell plot with boxplot overlay
  p <- ggplot(plot_df, aes(x = type, y = dip, group = sitecode)) +
    # Boxplot overlay (nudged to the left, no outliers shown)
    geom_boxplot(aes(group = type), 
                 color = "gray40", 
                 fill = NA, 
                 width = 0.3, 
                 position = position_nudge(x = -0.30),
                 outlier.shape = NA,
                 coef = Inf) +
    # Connecting lines (colored by days to checkback)
    geom_line(aes(color = days_to_checkback), 
              alpha = 0.5, 
              linewidth = 1) +
    # Points with tooltips
    geom_point(aes(color = days_to_checkback, text = tooltip_text), 
               size = 3, 
               alpha = 0.8) +
    # Color gradient: red (soon) to blue (later) to purple (much later)
    scale_color_gradientn(
      colors = c("red", "orange", "yellow", "green", "blue", "purple"),
      na.value = "gray50",
      name = "Days to\nCheckback"
    ) +
    scale_y_continuous(limits = c(-2, y_lim_top), oob = scales::oob_keep) +
    labs(
      title = "Dip Count Change: Pre-Treatment vs First Checkback",
      x = "",
      y = "Dip Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12)
    )
  
  # Convert to plotly for interactivity
  p_plotly <- ggplotly(p, tooltip = "text") %>%
    layout(
      hovermode = "closest",
      showlegend = TRUE
    )
  
  return(p_plotly)
}
