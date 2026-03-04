# Statistical Box Helper Functions
# Functions for creating themed value boxes in Shiny apps

#' Create a custom stat box with specified colors
#'
#' @param value The main value to display
#' @param title The title/label for the value box
#' @param bg_color Background color (hex code)
#' @param text_color Text color (hex code, default white)
#' @param icon Icon name (without "fa-" prefix), Shiny icon object, or path to image
#' @param icon_type Type of icon: "fontawesome" (default) or "image"
#' @param metric_id Optional metric ID for info button (shows description + wiki link)
#' @return A Shiny value box UI element
create_stat_box <- function(value, title, bg_color, text_color = "#ffffff", icon = NULL, icon_type = "fontawesome", metric_id = NULL) {
  # Convert icon name to icon object or image element
  icon_element <- NULL
  if (!is.null(icon)) {
    if (icon_type == "image") {
      icon_element <- tags$img(
        src = icon,
        style = "width: 48px; height: 48px; opacity: 0.9;"
      )
    } else if (is.character(icon)) {
      icon_element <- shiny::icon(icon)
    } else {
      icon_element <- icon
    }
  }
  
  # Build small info button (top-right corner) if metric_id is provided
  info_btn <- NULL
  if (!is.null(metric_id)) {
    description <- tryCatch(get_metric_description(metric_id), error = function(e) "")
    wiki_link   <- tryCatch(get_wiki_link(metric_id), error = function(e) "")
    
    if (nzchar(description) || nzchar(wiki_link)) {
      info_btn <- tags$button(
        class = "stat-box-info-btn",
        `data-metric-id` = metric_id,
        `data-description` = description,
        `data-wiki-link` = wiki_link,
        style = paste0(
          "position: absolute; top: 6px; right: 6px; ",
          "background: rgba(255,255,255,0.25); border: none; ",
          "color: ", text_color, "; font-size: 14px; ",
          "width: 24px; height: 24px; border-radius: 50%; ",
          "cursor: pointer; display: flex; align-items: center; ",
          "justify-content: center; padding: 0; ",
          "transition: background 0.2s; z-index: 10;"
        ),
        shiny::icon("info-circle")
      )
    }
  }
  
  # Create a custom styled div that mimics a shinydashboard value box
  div(
    style = paste0(
      "position: relative; ",
      "background-color: ", bg_color, "; ",
      "color: ", text_color, "; ",
      "padding: 20px 24px; ",
      "border-radius: 8px; ",
      "margin-bottom: 15px; ",
      "min-height: 100px; ",
      "box-shadow: 0 2px 4px rgba(0,0,0,0.1); ",
      "display: flex; ",
      "align-items: center; ",
      "justify-content: space-between;"
    ),
    info_btn,
    div(
      style = "flex: 1;",
      div(
        style = "font-size: 28px; font-weight: bold; margin-bottom: 5px;",
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