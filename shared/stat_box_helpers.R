# =============================================================================
# STAT BOX HELPERS - Customizable value box components
# =============================================================================
# Generic, reusable functions for creating styled stat boxes across all apps
# These functions provide flexibility for different use cases while maintaining
# consistent styling and theme integration
#
# Usage:
#   source("shared/stat_box_helpers.R")
#   
#   # Basic usage with custom colors
#   create_stat_box(title = "Total Items", value = "1,234", 
#                   bg_color = "#187018", text_color = "#fff")
#   
#   # Advanced usage with all options
#   create_stat_box(
#     title = "Active Treatments",
#     value = "456",
#     bg_color = "#2ecc71",
#     text_color = "#fff",
#     icon = icon("check-circle"),
#     icon_size = "2.5em",
#     border_color = "#27ae60",
#     border_width = "3px",
#     additional_style = "font-weight: bold;"
#   )
# =============================================================================

#' Create a Customizable Stat Box
#'
#' Generates a styled div-based stat box with flexible customization options.
#' Unlike shinydashboard valueBox, this uses pure HTML/CSS for full control.
#'
#' @param title Character. The title/label for the stat box
#' @param value Character. The main value to display (usually a metric)
#' @param bg_color Character. Background color (hex code or color name). Default: "#4169E1"
#' @param text_color Character. Text color (hex code or color name). Default: "#ffffff"
#' @param icon Icon object (from \code{icon()} function) or NULL. Default: NULL
#' @param icon_size Character. Size of icon in CSS units. Default: "2em"
#' @param icon_color Character. Icon color (overrides text_color for icon only). Default: NULL
#' @param border_color Character. Border color for the box. Default: NULL (no border)
#' @param border_width Character. Border width in CSS units. Default: "2px"
#' @param border_style Character. Border style (solid, dashed, dotted). Default: "solid"
#' @param padding Character. Interior padding. Default: "20px"
#' @param border_radius Character. Corner rounding radius. Default: "4px"
#' @param margin Character. Outer margin. Default: "15px"
#' @param box_shadow Character. Box shadow for depth effect. Default: "0 2px 4px rgba(0,0,0,0.1)"
#' @param title_size Character. Font size for title. Default: "0.9em"
#' @param value_size Character. Font size for value. Default: "2em"
#' @param additional_style Character. Additional CSS style properties. Default: ""
#' @param width Character. Box width (CSS units or bootstrap grid). Default: NULL (auto)
#' @param height Character. Box height (CSS units). Default: NULL (auto)
#' @param min_height Character. Minimum height. Default: NULL
#' @param hover_effect Logical. Add subtle hover effect. Default: FALSE
#' @param class Character. Additional CSS classes. Default: ""
#' @param id Character. HTML id attribute. Default: NULL
#'
#' @return A Shiny HTML div element
#'
#' @examples
#' \dontrun{
#' # Simple stat box
#' create_stat_box(title = "Total", value = "42", bg_color = "#4169E1")
#'
#' # With icon and border
#' create_stat_box(
#'   title = "Active",
#'   value = "28",
#'   bg_color = "#27ae60",
#'   icon = icon("check-circle"),
#'   border_color = "#229954",
#'   border_width = "3px"
#' )
#'
#' # Gradient-ready (set bg_color to gradient)
#' create_stat_box(
#'   title = "Status",
#'   value = "Good",
#'   bg_color = "linear-gradient(135deg, #667eea 0%, #764ba2 100%)"
#' )
#' }
#'
#' @export
create_stat_box <- function(
  title,
  value,
  bg_color = "#4169E1",
  text_color = "#ffffff",
  icon = NULL,
  icon_size = "2em",
  icon_color = NULL,
  border_color = NULL,
  border_width = "2px",
  border_style = "solid",
  padding = "20px",
  border_radius = "4px",
  margin = "15px",
  box_shadow = "0 2px 4px rgba(0,0,0,0.1)",
  title_size = "0.9em",
  value_size = "2em",
  additional_style = "",
  width = NULL,
  height = NULL,
  min_height = NULL,
  hover_effect = FALSE,
  class = "",
  id = NULL
) {
  
  # Build border style
  border_style_str <- ""
  if (!is.null(border_color)) {
    border_style_str <- paste0(
      "border: ", border_width, " ", border_style, " ", border_color, "; "
    )
  }
  
  # Build width/height styles
  size_style <- ""
  if (!is.null(width)) {
    size_style <- paste0(size_style, "width: ", width, "; ")
  }
  if (!is.null(height)) {
    size_style <- paste0(size_style, "height: ", height, "; ")
  }
  if (!is.null(min_height)) {
    size_style <- paste0(size_style, "min-height: ", min_height, "; ")
  }
  
  # Build hover effect
  hover_class <- ""
  if (hover_effect) {
    hover_class <- "stat-box-hover"
  }
  
  # Main container style
  main_style <- paste0(
    "background: ", bg_color, "; ",
    "color: ", text_color, "; ",
    "padding: ", padding, "; ",
    "border-radius: ", border_radius, "; ",
    "margin: ", margin, "; ",
    "text-align: center; ",
    "box-shadow: ", box_shadow, "; ",
    "display: flex; ",
    "flex-direction: column; ",
    "justify-content: center; ",
    size_style,
    border_style_str,
    additional_style
  )
  
  # Icon style
  icon_style <- paste0(
    "font-size: ", icon_size, "; ",
    "margin-bottom: 10px; ",
    if (!is.null(icon_color)) paste0("color: ", icon_color, "; ") else ""
  )
  
  # Title style
  title_style <- paste0(
    "font-size: ", title_size, "; ",
    "opacity: 0.9; ",
    "margin-bottom: 5px;"
  )
  
  # Value style
  value_style <- paste0(
    "font-size: ", value_size, "; ",
    "font-weight: bold;"
  )
  
  # Add hover CSS if needed
  if (hover_effect) {
    tags$head(tags$style(HTML("
      .stat-box-hover {
        transition: all 0.3s ease;
      }
      .stat-box-hover:hover {
        transform: translateY(-4px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.2) !important;
      }
    ")))
  }
  
  # Build the div
  div(
    id = id,
    class = paste0("stat-box ", hover_class, " ", class),
    style = main_style,
    
    # Icon (if provided)
    if (!is.null(icon)) {
      div(style = icon_style, icon)
    },
    
    # Title
    div(style = title_style, title),
    
    # Value
    div(style = value_style, value)
  )
}


#' Create Multiple Stat Boxes in a Grid Layout
#'
#' Convenience function to create a responsive grid of stat boxes.
#' Automatically handles layout and responsive sizing.
#'
#' @param ... List of stat box specifications. Each element should be a named list
#'        with parameters matching \code{create_stat_box()}.
#'        Can also pass stat box outputs directly.
#' @param cols_lg Integer. Number of columns on large screens. Default: 6
#' @param cols_md Integer. Number of columns on medium screens. Default: 3
#' @param cols_sm Integer. Number of columns on small screens. Default: 2
#' @param cols_xs Integer. Number of columns on extra small screens. Default: 1
#'
#' @return A Shiny fluidRow containing stat boxes in responsive columns
#'
#' @examples
#' \dontrun{
#' # Create a grid with 6 stat boxes
#' create_stat_box_grid(
#'   list(title = "Box 1", value = "100", bg_color = "#2ecc71"),
#'   list(title = "Box 2", value = "200", bg_color = "#3498db"),
#'   list(title = "Box 3", value = "300", bg_color = "#e74c3c"),
#'   cols_lg = 2  # 2 columns on large screens = 3 boxes per row
#' )
#' }
#'
#' @export
create_stat_box_grid <- function(
  ...,
  cols_lg = 6,
  cols_md = 3,
  cols_sm = 2,
  cols_xs = 1
) {
  
  boxes <- list(...)
  
  # Build the grid
  box_divs <- lapply(boxes, function(box_spec) {
    # If it's already a div/tag object, wrap it; if it's a list, create a stat box from it
    if (is.list(box_spec) && !inherits(box_spec, "shiny.tag")) {
      # It's a parameter list
      box_obj <- do.call(create_stat_box, box_spec)
    } else {
      # It's already a rendered box
      box_obj <- box_spec
    }
    
    # Wrap in responsive column
    column(
      width = cols_lg,
      class = paste0(
        "col-md-", cols_md, " ",
        "col-sm-", cols_sm, " ",
        "col-xs-", cols_xs
      ),
      box_obj
    )
  })
  
  # Return as fluidRow
  do.call(fluidRow, box_divs)
}


#' Create a Stat Box from Status Colors
#'
#' Convenience function that automatically pulls colors from \code{get_status_colors()}
#' in db_helpers.R. Use this when you want theme-aware stat boxes.
#'
#' @param title Character. The title/label for the stat box
#' @param value Character. The main value to display
#' @param status Character. Status key (e.g., "active", "completed", "planned").
#'        Must match a key in \code{get_status_colors()} output
#' @param icon Icon object (from \code{icon()} function) or NULL
#' @param theme Character. Theme to use. If NULL, uses current theme option.
#' @param ... Additional arguments passed to \code{create_stat_box()}
#'
#' @return A Shiny HTML div element styled with colors from the theme
#'
#' @examples
#' \dontrun{
#' # In server function:
#' output$status_box <- renderUI({
#'   create_status_stat_box(
#'     title = "Active Treatments",
#'     value = "123",
#'     status = "active",
#'     icon = icon("check-circle"),
#'     theme = input$color_theme
#'   )
#' })
#' }
#'
#' @export
create_status_stat_box <- function(
  title,
  value,
  status,
  icon = NULL,
  theme = NULL,
  ...
) {
  
  # Get the theme to use
  if (is.null(theme)) {
    theme <- getOption("mmcd.color.theme", "MMCD")
  }
  
  # Get status colors
  if (!exists("get_status_colors", mode = "function", inherits = TRUE)) {
    stop("get_status_colors() not found. Make sure db_helpers.R is sourced.")
  }
  
  colors <- get_status_colors(theme = theme)
  
  # Check if the status exists in colors
  if (!status %in% names(colors)) {
    warning(
      sprintf(
        "Status '%s' not found in theme colors. Available: %s",
        status,
        paste(names(colors), collapse = ", ")
      )
    )
    bg_color <- "#999999"  # Fallback gray
  } else {
    bg_color <- colors[[status]]
  }
  
  # Create the stat box with theme color
  create_stat_box(
    title = title,
    value = value,
    bg_color = bg_color,
    icon = icon,
    ...
  )
}


#' Create a Metric Stat Box (For Percentage/Score Display)
#'
#' Specialized stat box for displaying metrics like percentages, scores, or ratios.
#' Includes optional comparison/trend indicators.
#'
#' @param title Character. The metric title
#' @param value Numeric. The metric value
#' @param unit Character. Unit suffix (e.g., "%", "°C", "kmh"). Default: ""
#' @param bg_color Character. Background color. Default: "#4169E1"
#' @param text_color Character. Text color. Default: "#ffffff"
#' @param comparison Numeric. Comparison value for trend. Default: NULL
#' @param comparison_label Character. Label for comparison (e.g., "vs last month"). Default: NULL
#' @param comparison_direction Character. "up" or "down" for trend arrow. Default: NULL
#' @param icon Icon object or NULL
#' @param ... Additional arguments passed to \code{create_stat_box()}
#'
#' @return A Shiny HTML div element
#'
#' @examples
#' \dontrun{
#' # Simple percentage
#' create_metric_stat_box(
#'   title = "Coverage",
#'   value = 85.5,
#'   unit = "%",
#'   bg_color = "#27ae60"
#' )
#'
#' # With comparison/trend
#' create_metric_stat_box(
#'   title = "Conversion Rate",
#'   value = 12.3,
#'   unit = "%",
#'   comparison = 10.5,
#'   comparison_label = "vs last month",
#'   comparison_direction = "up",
#'   bg_color = "#2ecc71"
#' )
#' }
#'
#' @export
create_metric_stat_box <- function(
  title,
  value,
  unit = "",
  bg_color = "#4169E1",
  text_color = "#ffffff",
  comparison = NULL,
  comparison_label = NULL,
  comparison_direction = NULL,
  icon = NULL,
  ...
) {
  
  # Format the value with unit
  value_display <- if (is.numeric(value)) {
    paste0(round(value, 2), unit)
  } else {
    paste0(value, unit)
  }
  
  # Build comparison indicator
  comparison_html <- ""
  if (!is.null(comparison) && !is.null(comparison_direction)) {
    arrow <- if (comparison_direction == "up") "↑" else "↓"
    color <- if (comparison_direction == "up") "#2ecc71" else "#e74c3c"
    comparison_html <- paste0(
      "<div style='font-size: 0.8em; margin-top: 8px; color: ", color, ";'>",
      arrow, " ", round(value - comparison, 2), unit
    )
    if (!is.null(comparison_label)) {
      comparison_html <- paste0(comparison_html, " ", comparison_label)
    }
    comparison_html <- paste0(comparison_html, "</div>")
  }
  
  # Create stat box with comparison
  div(
    create_stat_box(
      title = title,
      value = value_display,
      bg_color = bg_color,
      text_color = text_color,
      icon = icon,
      ...
    ),
    if (comparison_html != "") HTML(comparison_html)
  )
}


#' Batch Create Multiple Status Stat Boxes
#'
#' Create multiple theme-aware stat boxes at once from status data.
#' Useful for summary dashboards with many status indicators.
#'
#' @param data Data frame with columns: title, value, status (optional: icon_name)
#' @param theme Character. Theme to use. Default: current theme option
#' @param icon_col Character. Column name for icon names. Default: "icon_name"
#' @param cols_lg Integer. Grid columns for large screens. Default: 4 (3 boxes per row)
#' @param ... Additional arguments for layout
#'
#' @return A Shiny fluidRow with stat boxes
#'
#' @examples
#' \dontrun{
#' # Create from data
#' status_data <- data.frame(
#'   title = c("Active", "Completed", "Planned"),
#'   value = c("45", "120", "30"),
#'   status = c("active", "completed", "planned"),
#'   icon_name = c("check-circle", "flag", "clock")
#' )
#'
#' create_batch_status_boxes(status_data)
#' }
#'
#' @export
create_batch_status_boxes <- function(
  data,
  theme = NULL,
  icon_col = "icon_name",
  cols_lg = 4,
  ...
) {
  
  if (nrow(data) == 0) return(NULL)
  
  # Default status to "unknown" if not provided
  if (!"status" %in% names(data)) {
    data$status <- "unknown"
  }
  
  # Create boxes
  boxes <- apply(data, 1, function(row) {
    icon_obj <- NULL
    if (icon_col %in% names(data) && !is.na(row[icon_col])) {
      icon_obj <- tryCatch(
        icon(row[icon_col]),
        error = function(e) NULL
      )
    }
    
    create_status_stat_box(
      title = row["title"],
      value = row["value"],
      status = row["status"],
      icon = icon_obj,
      theme = theme
    )
  })
  
  # Layout in grid
  box_divs <- lapply(boxes, function(box) {
    column(width = cols_lg, box)
  })
  
  do.call(fluidRow, box_divs)
}


#' Create Overview Stat Boxes Grid
#'
#' Creates a responsive grid of uiOutput placeholders for stat boxes.
#' Useful for dashboard overview sections where you render multiple stat boxes
#' dynamically in the server function.
#'
#' @param box_ids Character vector. IDs for the uiOutput placeholders.
#'        Default: c("stat_box_1", "stat_box_2", ..., "stat_box_6")
#' @param num_boxes Integer. Number of boxes to create. Default: 6
#' @param col_width Integer. Bootstrap column width (1-12). Default: 2 (6 boxes per row)
#'
#' @return A Shiny fluidRow with responsive columns containing uiOutput placeholders
#'
#' @details
#' This is a layout helper that creates empty placeholder divs for stat boxes.
#' You render the actual stat box content in the server function using renderUI().
#'
#' Example usage:
#' ```r
#' # In UI:
#' create_overview_stat_boxes_grid(
#'   box_ids = c("stat_total", "stat_active", "stat_pending"),
#'   num_boxes = 3,
#'   col_width = 4  # 3 boxes per row
#' )
#'
#' # In server:
#' output$stat_total <- renderUI({
#'   create_stat_box(
#'     title = "Total",
#'     value = "1,234",
#'     bg_color = "#4169E1"
#'   )
#' })
#' ```
#'
#' @export
create_overview_stat_boxes_grid <- function(
  box_ids = NULL,
  num_boxes = 6,
  col_width = 2
) {
  
  # Generate default IDs if not provided
  if (is.null(box_ids)) {
    box_ids <- paste0("stat_box_", seq_len(num_boxes))
  } else {
    num_boxes <- length(box_ids)
  }
  
  # Create column divs with uiOutput placeholders
  box_cols <- lapply(box_ids, function(id) {
    column(width = col_width, uiOutput(id))
  })
  
  # Combine into fluidRow
  do.call(fluidRow, box_cols)
}


#' Create Catch Basin Status Overview Boxes
#'
#' Specialized helper for creating the catch basin status overview stat boxes.
#' Creates 6 default boxes with proper layout for catch basin apps.
#'
#' This is a convenience wrapper around create_overview_stat_boxes_grid()
#' pre-configured for catch basin status dashboards.
#'
#' @return A Shiny fluidRow with 6 stat box placeholders
#'
#' @details
#' Creates boxes with IDs:
#' - stat_wet_cb
#' - stat_treated
#' - stat_percent
#' - stat_expiring
#' - stat_expired
#' - stat_needs_treatment
#'
#' Each ID corresponds to a renderUI() call in the server function.
#'
#' @examples
#' \dontrun{
#' # In UI:
#' create_catch_basin_overview_boxes()
#'
#' # In server:
#' output$stat_wet_cb <- renderUI({
#'   data <- catch_basin_data()
#'   colors <- get_status_colors(theme = current_theme())
#'
#'   create_stat_box(
#'     title = "Total Wet Catch Basins",
#'     value = format(sum(data$wet_cb_count), big.mark = ","),
#'     bg_color = colors[["completed"]],
#'     icon = icon("tint")
#'   )
#' })
#'
#' output$stat_treated <- renderUI({
#'   # Similar implementation...
#' })
#' # ... etc for remaining boxes
#' }
#'
#' @export
create_catch_basin_overview_boxes <- function() {
  create_overview_stat_boxes_grid(
    box_ids = c("stat_wet_cb", "stat_treated", "stat_percent",
                "stat_expiring", "stat_expired", "stat_needs_treatment"),
    num_boxes = 6,
    col_width = 2
  )
}

