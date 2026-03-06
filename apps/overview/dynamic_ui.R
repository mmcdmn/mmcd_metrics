# Dynamic UI Generation for Overview Apps
# =============================================================================
# Generates all UI elements dynamically from the metric registry.
# NO hardcoding of specific metrics - everything driven by get_metric_registry()
# =============================================================================

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Create a metric icon (image if available, FontAwesome icon as fallback)
#' @param config Metric configuration from registry
#' @return HTML tag for the icon
get_metric_icon <- function(config) {
  if (!is.null(config$image_path)) {
    # Use image file (Shiny will serve from www directory)
    tags$img(src = config$image_path, 
             alt = config$display_name,
             style = "width: 20px; height: 20px; margin-right: 5px; vertical-align: middle;")
  } else {
    # Fallback to FontAwesome icon
    icon(config$icon, style = "margin-right: 5px;")
  }
}

#' Create loading skeleton boxes
#' @param overview_type Type of overview ("district", "facilities")
#' @param n_boxes Number of boxes to show (defaults based on overview type)
#' @param show_loading_text Whether to show "Loading..." text above skeleton
#' @return UI elements for skeleton loading state
create_loading_skeleton <- function(overview_type = "district", n_boxes = NULL, show_loading_text = TRUE) {
  
  loading_indicator <- if (show_loading_text) {
    div(class = "loading-indicator",
      icon("sync", class = "fa-spin"),
      "Loading dashboard data..."
    )
  } else {
    NULL
  }
  
  if (overview_type == "facilities") {
    # Facilities view - show 6 skeleton boxes (typical facility count)
    if (is.null(n_boxes)) n_boxes <- 6
    
    skeleton_boxes <- lapply(1:n_boxes, function(i) {
      column(3,
        div(class = "skeleton-box",
          div(class = "skeleton-content",
            div(class = "skeleton-icon"),
            div(class = "skeleton-value"),
            div(class = "skeleton-title")
          )
        )
      )
    })
    
    div(
      loading_indicator,
      fluidRow(skeleton_boxes)
    )
    
  } else {
    # District view - show skeleton categories with boxes
    categories <- c("Treatment Progress", "Site Monitoring", "Quality Control")
    
    category_sections <- lapply(categories, function(cat) {
      # Each category has 2-3 skeleton boxes
      n_cat_boxes <- sample(2:3, 1)
      col_width <- floor(12 / n_cat_boxes)
      
      skeleton_boxes <- lapply(1:n_cat_boxes, function(i) {
        column(col_width,
          div(class = "skeleton-box",
            div(class = "skeleton-content",
              div(class = "skeleton-icon"),
              div(class = "skeleton-value"),
              div(class = "skeleton-title")
            )
          )
        )
      })
      
      div(class = "category-section skeleton-category",
        div(class = "skeleton-category-header"),
        fluidRow(skeleton_boxes)
      )
    })
    
    div(
      loading_indicator,
      div(class = "metrics-by-category", category_sections)
    )
  }
}

# =============================================================================
# CSS AND JAVASCRIPT (shared across all overview apps)
# =============================================================================

#' Get the standard CSS for overview dashboards
#' @return tags$link element referencing external CSS (cached by browser)
get_overview_css <- function() {
  tags$link(rel = "stylesheet", type = "text/css", href = "overview.css")
}

#' Get standard JavaScript for overview dashboards
#' @return tags$script element referencing external JS (cached by browser)
get_overview_js <- function() {
  tags$script(src = "overview.js")
}

# =============================================================================
# DYNAMIC CHART GENERATORS - Iterate through registry
# =============================================================================

#' Generate a single chart panel with title and filter info
#' @param metric_id The metric ID from registry
#' @param config The metric configuration
#' @param chart_height Height of the chart
#' @param is_historical Whether this is a historical chart
#' @return Shiny column element
create_chart_panel <- function(metric_id, config, chart_height = "300px", is_historical = FALSE) {
  
  output_id <- if (is_historical) {
    paste0(metric_id, "_historical_chart")
  } else {
    paste0(metric_id, "_chart")
  }
  
  title_text <- if (is_historical) {
    # For cattail treatments historical, use "Yearly History"
    if (isTRUE(config$historical_type == "yearly_grouped")) {
      paste0(config$display_name, " (Yearly History)")
    } else {
      y_suffix <- if (isTRUE(config$has_acres)) "Acres" else " " # leave else blank for now
      paste0(config$display_name, " ", y_suffix, " (Historical)")
    }
  } else {
    # For current progress charts
    # Metrics with display_as_average don't show "Progress" - they show status/comparison
    if (isTRUE(config$display_as_average)) {
      config$display_name  # Just the metric name (e.g., "avg mosquitoes per trap")
    } else {
      paste0(config$display_name, " Progress")  # Traditional progress metrics
    }
  }
  
  legend_id <- paste0(metric_id, "_legend")
  
  # Build info button for chart title (shown on ALL charts, current + historical)
  metric_description <- tryCatch(get_metric_description(metric_id), error = function(e) "")
  wiki_link <- tryCatch(get_wiki_link(metric_id), error = function(e) "")
  if (is_historical && nzchar(metric_description)) {
    metric_description <- paste0(metric_description, " (by week)")
  }
  chart_info_btn <- if (nzchar(metric_description) || !is.null(config$filter_info) || nzchar(wiki_link)) {
    # Combine description + filter_info into one data attribute
    combined_desc <- paste0(
      if (nzchar(metric_description)) metric_description else "",
      if (!is.null(config$filter_info) && !is_historical) {
        paste0("<hr style='margin:6px 0;border-color:#ddd'>", as.character(config$filter_info))
      } else ""
    )
    span(
      class = "chart-info-btn",
      `data-description` = combined_desc,
      `data-wiki-link` = wiki_link,
      shiny::icon("info-circle")
    )
  }

  div(class = "chart-panel",
    div(class = "chart-title",
      get_metric_icon(config), title_text,
      chart_info_btn
    ),
    # Add legend for current progress charts (not historical)
    if (!is_historical) {
      uiOutput(legend_id)
    },
    plotlyOutput(output_id, height = chart_height)
  )
}

#' Generate all current metric chart panels dynamically
#' Charts are hidden by default and shown when value boxes are clicked
#' @param chart_height Height of each chart
#' @return List of fluidRow elements with collapsible chart containers
#' @export
generate_current_charts_ui <- function(chart_height = "150px", metrics_filter = NULL) {
  # When drilling down (metrics_filter provided), use larger charts to match historical
  if (!is.null(metrics_filter)) {
    chart_height <- "300px"
  }
  
  # Use filtered metrics if provided, otherwise get all active metrics
  metrics <- if (!is.null(metrics_filter)) metrics_filter else get_active_metrics()
  
  # If no metrics to show, return nothing
  if (length(metrics) == 0) {
    return(div(class = "charts-flex-container", NULL))
  }
  
  registry <- get_metric_registry()
  
  # Create collapsible panels for each metric
  # If metrics_filter is provided (drill-down view), show charts without column wrapper (parent handles layout)
  # Otherwise, charts are hidden and shown when value boxes are clicked
  if (!is.null(metrics_filter)) {
    # Drill-down view: just the chart panels, no column wrapper
    panels <- lapply(metrics, function(metric_id) {
      config <- registry[[metric_id]]
      create_chart_panel(metric_id, config, chart_height, is_historical = FALSE)
    })
    tagList(panels)
  } else {
    # District view: compact flex layout with collapsible charts
    chart_class <- "chart-panel-wrapper inline-chart"
    panels <- lapply(metrics, function(metric_id) {
      config <- registry[[metric_id]]
      div(
        id = paste0("chart_wrapper_", metric_id),
        class = chart_class,
        create_chart_panel(metric_id, config, chart_height, is_historical = FALSE)
      )
    })
    # Wrap in flex container for side-by-side display
    div(class = "charts-flex-container", panels)
  }
}

#' Generate all historical chart panels dynamically
#' Reads from registry and creates UI for each metric with historical enabled
#' @param overview_type One of: "district", "facilities"
#' @param chart_height Height of each chart
#' @return List of UI elements including section header
#' @export
generate_historical_charts_ui <- function(overview_type = "district", chart_height = "300px", metrics_filter = NULL) {
  # Use filtered metrics if provided (intersected with historical), otherwise get historical
  all_historical <- get_historical_metrics()
  metrics <- if (!is.null(metrics_filter)) {
    intersect(metrics_filter, all_historical)
  } else {
    all_historical
  }
  
  # If no metrics to show, return nothing
  if (length(metrics) == 0) {
    return(NULL)
  }
  
  registry <- get_metric_registry()
  overview_config <- get_overview_config(overview_type)
  
  time_label <- if (overview_config$historical_type == "yearly") "Yearly" else "Weekly"
  
  # Section header - only show when NOT drilling down
  header <- if (is.null(metrics_filter)) {
    div(class = "historical-section",
      h3(icon("chart-line"), " Historical Trends (Last 5 Years)"),
      p(style = "color: #666; margin-bottom: 20px;",
        paste0(time_label, " treatment data - ",
               if (overview_config$historical_group_by == "mmcd_all") "All MMCD combined" 
               else paste0("By ", overview_config$historical_group_by)))
    )
  } else {
    NULL
  }
  
  # Create panels for each metric
  panels <- lapply(metrics, function(metric_id) {
    config <- registry[[metric_id]]
    create_chart_panel(metric_id, config, chart_height, is_historical = TRUE)
  })
  
  # When drilling down (metrics_filter not null), don't add header or column wrappers
  # Parent will handle side-by-side layout
  if (!is.null(metrics_filter)) {
    tagList(panels)
  } else {
    # Normal view: use column wrappers and rows
    panels_with_cols <- lapply(metrics, function(metric_id) {
      config <- registry[[metric_id]]
      column(6, create_chart_panel(metric_id, config, chart_height, is_historical = TRUE))
    })
    
    # Arrange in rows of 2
    rows <- split(panels_with_cols, ceiling(seq_along(panels_with_cols) / 2))
    chart_rows <- lapply(rows, function(row_panels) do.call(fluidRow, row_panels))
    
    # Combine header and chart rows
    c(list(header), chart_rows)
  }
}

# =============================================================================
# COMPLETE UI BUILDER
# =============================================================================

#' Build a complete overview dashboard UI
#' @param overview_type One of: "district", "facilities", "fos"
#' @param include_historical Whether to include historical charts section
#' @return Complete fluidPage UI
#' @export
build_overview_ui <- function(overview_type = "district", include_historical = TRUE, metrics_filter = NULL, 
                               initial_zone = "separate", initial_date = Sys.Date(), 
                               initial_expiring = 3, initial_theme = "MMCD",
                               facility_filter = NULL, fos_filter = NULL) {
  
  overview_config <- get_overview_config(overview_type)
  
  # Build dynamic title - include FOS name for FOS view
  page_title <- overview_config$title
  page_subtitle <- overview_config$subtitle
  if (overview_type == "fos" && !is.null(facility_filter)) {
    # Map facility short name to full name
    facilities <- tryCatch(get_facility_lookup(), error = function(e) data.frame())
    fac_display <- facility_filter
    if (nrow(facilities) > 0) {
      match <- facilities[facilities$short_name == facility_filter, ]
      if (nrow(match) > 0) fac_display <- match$full_name[1]
    }
    
    # If a specific FOS is selected, show their name in the title
    if (!is.null(fos_filter) && fos_filter != "all") {
      # Resolve FOS emp_num/shortname to display name
      fos_display <- fos_filter
      tryCatch({
        foremen <- get_foremen_lookup()
        # Try matching by shortname first, then by emp_num
        match_by_short <- foremen[foremen$shortname == fos_filter, ]
        match_by_emp <- foremen[as.character(foremen$emp_num) == as.character(fos_filter), ]
        if (nrow(match_by_short) > 0) {
          fos_display <- match_by_short$shortname[1]
        } else if (nrow(match_by_emp) > 0) {
          fos_display <- match_by_emp$shortname[1]
        }
      }, error = function(e) NULL)
      page_title <- paste0(fos_display, " - FOS Overview (" , fac_display, ")")
      page_subtitle <- paste0("Field Operations Supervisor progress for ", fos_display, " at ", fac_display)
    } else {
      page_title <- paste0(fac_display, " - FOS Overview")
      page_subtitle <- paste0("Field Operations Supervisor progress for ", fac_display)
    }
  }
  
  fluidPage(
    # Include universal CSS from db_helpers
    get_universal_text_css(),
    
    # Include overview-specific CSS and JS
    tags$head(
      get_overview_css(),
      get_overview_js()
    ),
    
    # Page Header with Back Button for drill-down views
    div(class = "page-header",
      if (overview_type %in% c("fos", "facilities") || !is.null(metrics_filter)) {
        div(style = "margin-bottom: 8px;",
          tags$a(
            href = "javascript:void(0)",
            onclick = "window.history.back();",
            class = "btn btn-default btn-sm",
            style = "font-size: 13px; padding: 4px 12px; color: #555; border-color: #ccc;",
            icon("arrow-left"),
            " Back to Overview"
          )
        )
      },
      h1(page_title),
      div(class = "subtitle", page_subtitle),
      # Color explanation note — appears in the blue header for all views
      div(style = "margin-top: 8px; font-size: 12px; opacity: 0.85; display: flex; align-items: center; gap: 6px;",
        icon("info-circle"),
        span("Value box colors reflect current week vs. 10-year weekly average for treatment metrics")
      )
    ),
    
    # Controls Panel
    div(class = "controls-panel",
      fluidRow(
        column(2,
          actionButton("refresh", "Refresh All Data",
                       icon = icon("sync"),
                       class = "btn-primary btn-lg",
                       style = "width: 100%;")
        ),
        column(2,
          dateInput("custom_today", "Analysis Date:",
                   value = initial_date,
                   max = Sys.Date(),
                   format = "yyyy-mm-dd")
        ),
        tags$input(type = "hidden", id = "expiring_days", name = "expiring_days", value = "3"),
        column(4,
          selectInput("zone_filter", "Zone:",
                     choices = c("P1 Only" = "1",
                                "P2 Only" = "2",
                                "P1 and P2" = "1,2",
                                "P1 and P2 SEPARATE" = "separate"),
                     selected = initial_zone)
        ),
        column(2,
          div(class = "last-updated",
            textOutput("last_updated")
          )
        )
      )
    ),
    
    # Summary Statistics Row with Loading Skeleton
    div(id = "summary_stats_container",
      # Initial prompt - shown before first Refresh click
      div(id = "initial_prompt_static",
        div(class = "initial-prompt",
          icon("mouse-pointer"),
          span("Select a date and click "),
          tags$strong("Refresh Data"),
          span(" to load the dashboard.")
        )
      ),
      # Loading skeleton - hidden initially, shown after Refresh click while data loads
      div(id = "loading_skeleton_static", style = "display: none;",
        create_loading_skeleton(overview_type)
      ),
      # Actual stats - hidden until data is ready
      div(id = "summary_stats_wrapper", style = "display: none;",
        uiOutput("summary_stats")
      )
    ),
    
    # Main Charts Container (only for drill-down views - district charts are now in category sections)
    # Placed BEFORE detail boxes so charts appear right after value boxes, not at page bottom
    div(class = "dashboard-container",
      # For drill-down view, show current progress and historical side-by-side
      # EXCEPT for FOS overview with specific FOS (those use hidden charts behind value boxes)
      if (include_historical && !is.null(metrics_filter) && !(overview_type == "fos" && !is.null(fos_filter))) {
        fluidRow(
          column(6,
            div(style = "padding-right: 10px;",
              generate_current_charts_ui(metrics_filter = metrics_filter)
            )
          ),
          column(6,
            div(style = "padding-left: 10px;",
              generate_historical_charts_ui(overview_type, metrics_filter = metrics_filter)
            )
          )
        )
      }
      # District view charts are now embedded in category sections
      # FOS overview charts are embedded in generate_summary_stats per-metric sections
    ),

    # Detail Boxes Container (for facilities AND FOS view click-to-expand)
    div(id = "facility_detail_container",
      class = "facility-detail-section",
      style = "margin-top: 20px;",
      uiOutput("facility_detail_boxes")
    ),

    # Color Theme selector at the bottom
    div(style = "text-align: right; padding: 10px 20px; margin-top: 10px;",
      div(style = "display: inline-block; width: 180px;",
        selectInput("color_theme", "Color Theme:",
                   choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis"),
                   selected = initial_theme)
      )
    )
  )
}
