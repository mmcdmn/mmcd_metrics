# Overview - UI Helper Functions
# =============================================================================
# Shared UI components for all overview dashboards (district, facilities, fos).
# Dynamically generates UI based on metric registry.
# =============================================================================

#' Create the main UI for any overview app
#' @param overview_type Type of overview: "district", "facilities", or "fos"
#' @param metrics Vector of metric IDs to display (NULL = all)
#' @return Shiny UI object
#' @export
create_overview_ui <- function(overview_type = "facilities", metrics = NULL) {
  
  # Get configuration for this overview type
  config <- get_overview_config(overview_type)
  
  # Get metrics to display
  if (is.null(metrics)) {
    metrics <- get_active_metrics()
  }
  registry <- get_metric_registry()
  
  fluidPage(
    # Use universal CSS from db_helpers for consistent text sizing
    get_universal_text_css(),
    
    # JavaScript for filter info toggle and navigation
    tags$head(
      tags$script(HTML("
        function toggleFilterInfo(id) {
          var tooltip = document.getElementById(id);
          // Close all other tooltips first
          var allTooltips = document.querySelectorAll('.filter-tooltip');
          allTooltips.forEach(function(t) {
            if (t.id !== id) {
              t.classList.remove('show');
            }
          });
          // Toggle this tooltip
          tooltip.classList.toggle('show');
        }
        // Close tooltips when clicking elsewhere
        document.addEventListener('click', function(e) {
          if (!e.target.classList.contains('filter-info-btn')) {
            var allTooltips = document.querySelectorAll('.filter-tooltip');
            allTooltips.forEach(function(t) {
              t.classList.remove('show');
            });
          }
        });
        // Handle navigation from Shiny
        Shiny.addCustomMessageHandler('navigate', function(url) {
          window.location.href = url;
        });
      "))
    ),
    
    # Custom CSS for the dashboard layout
    tags$head(
      tags$style(HTML(get_overview_css()))
    ),
    
    # Page Header
    div(class = "page-header",
      h1(config$title),
      div(class = "subtitle", config$subtitle)
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
                   value = Sys.Date(), 
                   max = Sys.Date(),
                   format = "yyyy-mm-dd")
        ),
        column(2,
          sliderInput("expiring_days", "Expiring Window (days):",
                     min = 1, max = 30, value = 3, step = 1)
        ),
        column(2,
          selectInput("zone_filter", "Zone:",
                     choices = c("P1 Only" = "1",
                                "P2 Only" = "2", 
                                "P1 and P2" = "1,2",
                                "P1 and P2 SEPARATE" = "separate"),
                     selected = "1,2")
        ),
        column(2,
          selectInput("color_theme", "Color Theme:",
                     choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis"),
                     selected = "MMCD")
        ),
        column(2,
          div(class = "last-updated",
            textOutput("last_updated")
          )
        )
      )
    ),
    
    # Summary Statistics Row (dynamic)
    uiOutput("summary_stats"),
    
    # Charts Grid - 2x2 layout (dynamic based on metrics)
    div(class = "dashboard-container",
      create_chart_grid(metrics, registry)
    )
  )
}


#' Create a 2x2 grid of chart panels based on metrics
#' @param metrics Vector of metric IDs
#' @param registry Metric registry
#' @return Shiny UI element
create_chart_grid <- function(metrics, registry) {
  # Split metrics into rows of 2
  n_metrics <- length(metrics)
  rows <- ceiling(n_metrics / 2)
  
  row_list <- lapply(seq_len(rows), function(row_idx) {
    start_idx <- (row_idx - 1) * 2 + 1
    end_idx <- min(start_idx + 1, n_metrics)
    row_metrics <- metrics[start_idx:end_idx]
    
    cols <- lapply(row_metrics, function(metric_id) {
      config <- registry[[metric_id]]
      column(6,
        create_chart_panel(config)
      )
    })
    
    # If only one metric in this row, add an empty column
    if (length(row_metrics) == 1) {
      cols[[2]] <- column(6)
    }
    
    do.call(fluidRow, cols)
  })
  
  do.call(tagList, row_list)
}


#' Create a single chart panel for a metric
#' @param metric_config Metric configuration from registry
#' @return Shiny UI element
create_chart_panel <- function(metric_config) {
  div(class = "chart-panel",
    div(class = "chart-title", 
        icon(metric_config$icon), 
        paste0(" ", metric_config$display_name, " Progress"),
        span(class = "filter-info-btn", 
             onclick = paste0("toggleFilterInfo('", metric_config$id, "_filters')"), "+"),
        div(id = paste0(metric_config$id, "_filters"), 
            class = "filter-tooltip",
            metric_config$filter_info)
    ),
    plotlyOutput(paste0(metric_config$id, "_chart"), height = "300px")
  )
}


#' Create summary stat boxes for all metrics
#' Uses ceiling() for percentages (no decimals, rounded up)
#' @param data Named list of metric data
#' @param registry Metric registry
#' @return Shiny UI element
#' @export
create_summary_stats_ui <- function(data, registry) {
  metrics <- names(data)
  n_metrics <- length(metrics)
  col_width <- max(2, floor(12 / n_metrics))
  
  cols <- lapply(metrics, function(metric_id) {
    config <- registry[[metric_id]]
    stats <- calculate_metric_stats(data[[metric_id]])
    
    column(col_width,
      create_stat_box(
        value = paste0(stats$pct, "%"),
        title = paste0(config$display_name, ": ", format(stats$active, big.mark = ","), 
                      " / ", format(stats$total, big.mark = ","), " treated"),
        bg_color = config$bg_color,
        icon = config$icon
      )
    )
  })
  
  do.call(fluidRow, cols)
}


#' Get CSS styles for overview dashboards
#' @return CSS string
get_overview_css <- function() {
  "
    .dashboard-container {
      padding: 10px;
    }
    .chart-panel {
      background: #fff;
      border: 1px solid #ddd;
      border-radius: 8px;
      padding: 15px;
      margin-bottom: 20px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      min-height: 350px;
    }
    .chart-title {
      font-size: 18px;
      font-weight: bold;
      margin-bottom: 10px;
      color: #333;
      border-bottom: 2px solid #007bff;
      padding-bottom: 5px;
    }
    .summary-box {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      border-radius: 10px;
      padding: 20px;
      text-align: center;
      margin-bottom: 20px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    }
    .summary-box h4 {
      margin: 0 0 10px 0;
      font-size: 16px;
      font-weight: 600;
      opacity: 0.9;
    }
    .summary-box .stat-value {
      font-size: 36px;
      font-weight: bold;
      margin: 5px 0;
    }
    .summary-box .stat-label {
      font-size: 14px;
      opacity: 0.85;
      margin: 0;
    }
    .page-header {
      background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
      color: white;
      padding: 20px 30px;
      margin: -15px -15px 20px -15px;
      border-radius: 0;
    }
    .page-header h1 {
      margin: 0;
      font-size: 28px;
      font-weight: 600;
    }
    .page-header .subtitle {
      opacity: 0.8;
      font-size: 14px;
      margin-top: 5px;
    }
    .controls-panel {
      background: #f8f9fa;
      border-radius: 8px;
      padding: 20px;
      margin-bottom: 20px;
    }
    .last-updated {
      text-align: right;
      color: #666;
      font-size: 12px;
      font-style: italic;
    }
    /* Different colors for each summary box */
    .col-sm-3:nth-child(1) .summary-box {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    }
    .col-sm-3:nth-child(2) .summary-box {
      background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
    }
    .col-sm-3:nth-child(3) .summary-box {
      background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
    }
    .col-sm-3:nth-child(4) .summary-box {
      background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%);
    }
    .filter-info-btn {
      display: inline-block;
      width: 20px;
      height: 20px;
      line-height: 18px;
      text-align: center;
      border-radius: 50%;
      background: #e0e0e0;
      color: #666;
      font-size: 14px;
      font-weight: bold;
      cursor: pointer;
      margin-left: 8px;
      border: 1px solid #ccc;
      transition: all 0.2s;
    }
    .filter-info-btn:hover {
      background: #007bff;
      color: white;
      border-color: #007bff;
    }
    .filter-tooltip {
      display: none;
      position: absolute;
      background: #333;
      color: white;
      padding: 10px 15px;
      border-radius: 6px;
      font-size: 12px;
      z-index: 1000;
      max-width: 300px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.3);
      line-height: 1.5;
    }
    .filter-tooltip.show {
      display: block;
    }
  "
}


#' Navigate to another overview with filters
#' @param session Shiny session
#' @param target Target overview type or URL
#' @param zone_clicked The zone that was clicked (e.g., "P1")
#' @param analysis_date The current analysis date
#' @param expiring_days The current expiring days setting
#' @export
navigate_to_overview <- function(session, target, zone_clicked, analysis_date, expiring_days) {
  # Extract zone number from display name (e.g., "P1" -> "1")
  zone_num <- gsub("P", "", zone_clicked)
  
  # Build URL with parameters
  url <- paste0(
    "../", target, "/?zone=", zone_num,
    "&date=", as.character(analysis_date),
    "&expiring=", expiring_days
  )
  
  # Navigate using JavaScript
  session$sendCustomMessage("navigate", url)
}
