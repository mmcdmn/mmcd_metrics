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

# =============================================================================
# CSS AND JAVASCRIPT (shared across all overview apps)
# =============================================================================

#' Get the standard CSS for overview dashboards
#' @return tags$style element
get_overview_css <- function() {
  tags$style(HTML("
    .dashboard-container {
      padding: 10px;
    }
    .chart-panel {
      background: #fff;
      border: 1px solid #ddd;
      border-radius: 8px;
      padding: 10px;
      margin-bottom: 15px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      min-height: 200px;
    }
    .chart-panel-wrapper {
      height: 0;
      overflow: hidden;
      transition: height 0.3s ease-out;
      max-width: 400px;
      margin: 0 auto 20px auto;
    }
    .chart-panel-wrapper.visible {
      height: auto;
      overflow: visible;
    }
    .charts-flex-container {
      display: flex;
      flex-wrap: wrap;
      gap: 15px;
      justify-content: flex-start;
      align-items: flex-start;
    }
    .inline-chart {
      flex: 0 1 calc(50% - 8px);
      min-width: 350px;
    }
    .drilldown-chart {
      max-width: 100% !important;
      width: 100%;
      margin: 20px 0;
    }
    @media (max-width: 768px) {
      .inline-chart {
        flex: 1 1 100%;
        min-width: 300px;
      }
    }
    @keyframes slideDown {
      from {
        opacity: 0;
        transform: translateY(-10px);
      }
      to {
        opacity: 1;
        transform: translateY(0);
      }
    }
    .stat-box-clickable {
      cursor: pointer;
      transition: transform 0.2s ease, box-shadow 0.2s ease;
      position: relative;
    }
    .stat-box-clickable:hover {
      transform: translateY(-3px);
      box-shadow: 0 6px 12px rgba(0,0,0,0.2);
    }
    .stat-box-clickable.active {
      outline: 3px solid #333;
      outline-offset: 2px;
    }
    .stat-box-clickable::after {
      content: 'Click to view chart';
      position: absolute;
      bottom: 5px;
      right: 10px;
      font-size: 10px;
      opacity: 0.7;
    }
    .stat-box-clickable.active::after {
      content: 'Click to hide chart';
    }
    /* Category grouping styles */
    .metrics-by-category {
      display: flex;
      flex-wrap: wrap;
      gap: 10px;
    }
    .category-section {
      flex: 1 1 auto;
      min-width: 280px;
    }
    .category-header {
      font-size: 13px;
      font-weight: 600;
      color: #555;
      margin-bottom: 8px;
      padding-left: 8px;
      border-left: 3px solid #2c5aa0;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    /* Facility detail section styles */
    .facility-detail-section {
      background: linear-gradient(135deg, #f5f7fa 0%, #e4e8eb 100%);
      border-radius: 10px;
      padding: 20px;
      margin: 15px 0;
      border: 1px solid #ddd;
      animation: fadeIn 0.3s ease-in-out;
      display: none;
    }
    .facility-detail-section.visible {
      display: block !important;
    }
    .facility-detail-header {
      font-size: 16px;
      font-weight: 600;
      color: #333;
      margin-bottom: 15px;
      padding-bottom: 10px;
      border-bottom: 2px solid #2c5aa0;
    }
    .facility-detail-boxes {
      display: flex;
      flex-wrap: wrap;
      gap: 15px;
    }
    .facility-detail-boxes .stat-box {
      flex: 1 1 150px;
      min-width: 140px;
      max-width: 200px;
    }
    /* Facility clickable hints */
    .stat-box-clickable[data-facility]::after {
      content: 'Click for details';
    }
    .stat-box-clickable[data-facility].active::after {
      content: 'Click to hide details';
    }
    @media (min-width: 1200px) {
      .category-section {
        flex: 0 1 48%;
      }
    }
    @media (min-width: 1600px) {
      .category-section {
        flex: 0 1 24%;
      }
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
    .historical-section {
      margin-top: 30px;
      padding-top: 20px;
      border-top: 3px solid #eee;
    }
    .historical-section h3 {
      color: #1e3c72;
      margin-bottom: 20px;
    }
    .chart-toggle-btn {
      font-size: 11px;
      padding: 4px 8px;
      border: 1px solid #ccc;
      background: #f8f9fa;
      color: #333;
      border-radius: 3px;
      cursor: pointer;
      transition: all 0.2s ease;
    }
    .chart-toggle-btn:hover {
      background: #e9ecef;
      border-color: #999;
    }
    .chart-toggle-btn.active {
      background: #007bff;
      color: white;
      border-color: #007bff;
    }
  "))
}

#' Get standard JavaScript for overview dashboards
#' @return tags$script element
get_overview_js <- function() {
  tags$script(HTML("
    function toggleFilterInfo(id) {
      var tooltip = document.getElementById(id);
      var allTooltips = document.querySelectorAll('.filter-tooltip');
      allTooltips.forEach(function(t) {
        if (t.id !== id) t.classList.remove('show');
      });
      tooltip.classList.toggle('show');
    }
    document.addEventListener('click', function(e) {
      if (!e.target.classList.contains('filter-info-btn')) {
        var allTooltips = document.querySelectorAll('.filter-tooltip');
        allTooltips.forEach(function(t) { t.classList.remove('show'); });
      }
    });
    Shiny.addCustomMessageHandler('navigate', function(url) {
      window.location.href = url;
    });
    
    // Value box click to toggle chart visibility (for district view - metric boxes)
    $(document).on('click', '.stat-box-clickable[data-metric-id]', function() {
      var metricId = $(this).data('metric-id');
      var chartWrapper = $('#chart_wrapper_' + metricId);
      var statBox = $(this);
      
      // Toggle visibility
      chartWrapper.toggleClass('visible');
      statBox.toggleClass('active');
      
      // Scroll to chart if now visible and resize Plotly
      if (chartWrapper.hasClass('visible')) {
        // Wait for CSS transition to complete, then resize plotly
        setTimeout(function() {
          // Find all plotly charts in this wrapper and resize them
          chartWrapper.find('.plotly').each(function() {
            if (window.Plotly && this.layout) {
              window.Plotly.Plots.resize(this);
            }
          });
          // Scroll to chart after resize
          $('html, body').animate({
            scrollTop: chartWrapper.offset().top - 100
          }, 300);
        }, 350); // Wait slightly longer than CSS transition
      }
      
      // Send visibility state to server
      Shiny.setInputValue(metricId + '_chart_visible', chartWrapper.hasClass('visible'), {priority: 'event'});
    });
    
    // Facility box click to show detail boxes (for facilities view)
    $(document).on('click', '.stat-box-clickable[data-facility]', function() {
      var facility = $(this).data('facility');
      var statBox = $(this);
      var detailContainer = $('#facility_detail_container');
      
      // Toggle active state on facility boxes
      $('.stat-box-clickable[data-facility]').removeClass('active');
      
      // Check if this facility is already selected
      var currentFacility = detailContainer.data('current-facility');
      if (currentFacility === facility) {
        // Clicking same facility - toggle off
        detailContainer.data('current-facility', null);
        detailContainer.removeClass('visible');
        Shiny.setInputValue('selected_facility', null, {priority: 'event'});
      } else {
        // Clicking new facility - show details
        statBox.addClass('active');
        detailContainer.data('current-facility', facility);
        detailContainer.addClass('visible');
        Shiny.setInputValue('selected_facility', facility, {priority: 'event'});
        
        // Scroll to detail container
        setTimeout(function() {
          if (detailContainer.length && detailContainer.hasClass('visible')) {
            $('html, body').animate({
              scrollTop: detailContainer.offset().top - 100
            }, 300);
          }
        }, 100);
      }
    });
    
    // Legacy: Chart type toggle functionality (kept for backwards compatibility)
    $(document).on('click', '.chart-toggle-btn', function() {
      var btn = $(this);
      var metric = btn.attr('id').replace('_toggle_bar', '').replace('_toggle_pie', '');
      var chartType = btn.attr('id').includes('_toggle_pie') ? 'pie' : 'bar';
      
      // Update button states
      $('#' + metric + '_toggle_bar, #' + metric + '_toggle_pie').removeClass('active');
      btn.addClass('active');
      
      // Send chart type change to server
      Shiny.setInputValue(metric + '_chart_type', chartType, {priority: 'event'});
    });
  "))
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
    paste0(config$display_name, " Progress")
  }
  
  filter_id <- paste0(metric_id, "_filters")
  legend_id <- paste0(metric_id, "_legend")
  chart_type_id <- paste0(metric_id, "_chart_type")
  
  # Check if metric supports multiple chart types (legacy - kept but not shown)
  has_chart_toggle <- !is.null(config$chart_types) && length(config$chart_types) > 1
  
  div(class = "chart-panel",
    div(class = "chart-title",
      get_metric_icon(config), title_text,
      # Pie chart toggle buttons removed - now using value box click to show/hide
      if (!is_historical && !is.null(config$filter_info)) {
        tagList(
          span(class = "filter-info-btn",
               onclick = paste0("toggleFilterInfo('", filter_id, "')"), "+"),
          div(id = filter_id, class = "filter-tooltip", config$filter_info)
        )
      }
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
                               initial_zone = "1,2", initial_date = Sys.Date(), 
                               initial_expiring = 3, initial_theme = "MMCD") {
  
  overview_config <- get_overview_config(overview_type)
  
  fluidPage(
    # Include universal CSS from db_helpers
    get_universal_text_css(),
    
    # Include overview-specific CSS and JS
    tags$head(
      get_overview_css(),
      get_overview_js()
    ),
    
    # Page Header
    div(class = "page-header",
      h1(overview_config$title),
      div(class = "subtitle", overview_config$subtitle)
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
        column(2,
          sliderInput("expiring_days", "Expiring Window (days):",
                     min = 1, max = 30, value = initial_expiring, step = 1)
        ),
        column(2,
          selectInput("zone_filter", "Zone:",
                     choices = c("P1 Only" = "1",
                                "P2 Only" = "2",
                                "P1 and P2" = "1,2",
                                "P1 and P2 SEPARATE" = "separate"),
                     selected = initial_zone)
        ),
        column(2,
          selectInput("color_theme", "Color Theme:",
                     choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis"),
                     selected = initial_theme)
        ),
        column(2,
          div(class = "last-updated",
            textOutput("last_updated")
          )
        )
      )
    ),
    
    # Summary Statistics Row
    uiOutput("summary_stats"),
    
    # Facility Detail Boxes Container (for facilities view drill-down)
    div(id = "facility_detail_container",
      class = "facility-detail-section",
      style = "margin-top: 20px;",
      uiOutput("facility_detail_boxes")
    ),
    
    # Main Charts Container
    div(class = "dashboard-container",
      # For drill-down view, show current progress and historical side-by-side
      if (include_historical && !is.null(metrics_filter)) {
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
      } else {
        # For district view, just show current progress charts
        div(
          generate_current_charts_ui(metrics_filter = metrics_filter)
        )
      }
    )
  )
}
