# Statistical Box Helper Functions
# Functions for creating themed value boxes in Shiny apps

#' Create a custom stat box with specified colors
#'
#' @param value The main value to display
#' @param title The title/label for the value box
#' @param bg_color Background color (hex code)
#' @param text_color Text color (hex code, default white)
#' @param icon Icon name (without "fa-" prefix) or Shiny icon object
#' @return A Shiny value box UI element
create_stat_box <- function(value, title, bg_color, text_color = "#ffffff", icon = NULL) {
  # Convert icon name to icon object if it's a string
  icon_element <- NULL
  if (!is.null(icon)) {
    if (is.character(icon)) {
      icon_element <- shiny::icon(icon)
    } else {
      icon_element <- icon
    }
  }
  
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
    if (!is.null(icon_element)) {
      div(
        style = "font-size: 36px; opacity: 0.8;",
        icon_element
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
  
  # Safely get the background color for this status
  # Use named vector access with fallback for unknown statuses
  bg_color <- if (status %in% names(colors)) {
    colors[[status]]
  } else {
    "#3c8dbc"  # Default blue fallback
  }
  
  # Additional safety check
  if (is.null(bg_color) || length(bg_color) == 0 || is.na(bg_color)) {
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