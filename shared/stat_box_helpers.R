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
#' @param theme Color theme name; used to resolve `bg_color` when a status
#'   keyword ("good"/"warning"/"alert" or a theme status name) is passed instead
#'   of a hex code.
#' @return A Shiny value box UI element
create_stat_box <- function(value, title, bg_color, text_color = NULL, icon = NULL,
                            icon_type = "fontawesome", metric_id = NULL,
                            theme = getOption("mmcd.color.theme", "MMCD")) {
  # Accept a status keyword in place of a raw hex, so callers can stop
  # hardcoding colors. Anything already starting with "#" passes through.
  bg_color <- resolve_box_color(bg_color, theme = theme)

  # text_color defaults to NULL = auto-contrast. Callers passing an explicit
  # color still win. White text was previously hardcoded, which is unreadable
  # on light indicator colors (e.g. Viridis warning #FDE724).
  if (is.null(text_color)) {
    text_color <- if (exists("contrast_text_color", mode = "function", inherits = TRUE)) {
      contrast_text_color(bg_color)
    } else {
      "#ffffff"
    }
  }

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
#' @param theme Color theme name. Defaults to the active theme; the previous
#'   default of "default" was not a valid theme name and made every call emit
#'   "Theme 'default' not found" while ignoring the configured theme.
#' @return A Shiny value box UI element
create_status_stat_box <- function(value, title, status, icon = NULL,
                                   theme = getOption("mmcd.color.theme", "MMCD")) {
  create_stat_box(
    value = value,
    title = title,
    bg_color = resolve_box_color(status, theme = theme),
    icon = icon,
    theme = theme
  )
}

#' Resolve a stat box background color from a hex, status name, or indicator name
#'
#' Accepts, in order: a literal hex ("#1f77b4"), one of the indicator keywords
#' ("good"/"warning"/"alert"), or one of the theme status names ("active",
#' "completed", "planned", "needs_action", "in_lab", "needs_treatment",
#' "unknown"). Anything unrecognised falls back to a neutral blue.
#'
#' @param x Hex color, indicator keyword, or status name
#' @param theme Color theme name
#' @return Hex color string
resolve_box_color <- function(x, theme = getOption("mmcd.color.theme", "MMCD")) {
  fallback <- "#3c8dbc"

  # A typo'd or missing lookup upstream can produce NA; don't emit "NA" into CSS.
  if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(as.character(x))) {
    return(fallback)
  }
  x <- as.character(x)
  if (startsWith(x, "#")) return(x)

  if (x %in% c("good", "warning", "alert")) {
    ind <- tryCatch(get_indicator_colors(theme = theme), error = function(e) NULL)
    if (!is.null(ind) && !is.na(ind[x])) return(unname(ind[x]))
    return(fallback)
  }

  colors <- tryCatch(get_status_colors(theme = theme), error = function(e) NULL)
  if (!is.null(colors) && x %in% names(colors)) {
    v <- colors[[x]]
    if (!is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)) return(unname(v))
  }

  fallback
}