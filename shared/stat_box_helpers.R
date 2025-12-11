# Statistical Box Helper Functions
# Functions for creating themed value boxes in Shiny apps

#' Create a custom stat box with specified colors
#'
#' @param value The main value to display
#' @param title The title/label for the value box
#' @param bg_color Background color (hex code)
#' @param text_color Text color (hex code, default white)
#' @param icon Shiny icon for the value box
#' @return A Shiny value box UI element
create_stat_box <- function(value, title, bg_color, text_color = "#ffffff", icon = NULL) {
  # Create a custom styled div that mimics a shinydashboard value box
  div(
    style = paste0(
      "background-color: ", bg_color, "; ",
      "color: ", text_color, "; ",
      "padding: 20px; ",
      "border-radius: 5px; ",
      "margin-bottom: 15px; ",
      "box-shadow: 0 2px 4px rgba(0,0,0,0.1); ",
      "display: flex; ",
      "align-items: center; ",
      "justify-content: space-between;"
    ),
    div(
      style = "flex: 1;",
      div(
        style = "font-size: 24px; font-weight: bold; margin-bottom: 5px;",
        value
      ),
      div(
        style = "font-size: 14px; opacity: 0.9;",
        title
      )
    ),
    if (!is.null(icon)) {
      div(
        style = "font-size: 36px; opacity: 0.8;",
        icon
      )
    }
  )
}

#' Create a stat box using status colors from theme
#'
#' @param value The main value to display
#' @param title The title/label for the value box
#' @param status Status type (e.g., "unknown", "completed", "needs_inspection", etc.)
#' @param icon Shiny icon for the value box
#' @param theme Color theme name (default "default")
#' @return A Shiny value box UI element
create_status_stat_box <- function(value, title, status, icon = NULL, theme = "default") {
  # Get status colors for the theme
  colors <- get_status_colors(theme = theme)
  
  # Get the background color for this status
  bg_color <- colors[[status]]
  if (is.null(bg_color) || length(bg_color) == 0 || is.na(bg_color)) {
    # Fallback to a default color if status not found
    bg_color <- "#3c8dbc"  # Default blue
  }
  
  # Use the create_stat_box function with theme colors
  create_stat_box(
    value = value,
    title = title,
    bg_color = bg_color,
    text_color = "#ffffff",
    icon = icon
  )
}

#' Format metric values for display
#'
#' @param value Numeric value to format
#' @param metric_type Type of metric ("acres", "sites", etc.)
#' @return Formatted string
format_metric <- function(value, metric_type = "sites") {
  if (is.na(value) || is.null(value)) {
    return("0")
  }
  
  if (metric_type == "acres") {
    # Format acres with commas and 1 decimal place
    return(format(round(value, 1), big.mark = ",", nsmall = 1))
  } else {
    # Format counts with commas, no decimals
    return(format(round(value, 0), big.mark = ","))
  }
}

#' Calculate metric value based on type
#'
#' @param data Data frame with site data
#' @param metric_type Type of metric to calculate ("acres" or "sites")
#' @return Calculated metric value
calc_metric <- function(data, metric_type = "sites") {
  if (is.null(data) || nrow(data) == 0) {
    return(0)
  }
  
  if (metric_type == "acres") {
    # Sum acres column if it exists
    if ("acres" %in% names(data)) {
      return(sum(data$acres, na.rm = TRUE))
    } else {
      # Fallback: assume 1 acre per site
      return(nrow(data))
    }
  } else {
    # Count number of sites
    return(nrow(data))
  }
}