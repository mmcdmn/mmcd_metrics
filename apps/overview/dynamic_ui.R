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
      transition: all 0.3s ease-out;
      opacity: 0;
      max-width: 0;
      flex: 0 0 0;
      margin: 0;
      padding: 0;
    }
    .chart-panel-wrapper.visible {
      height: auto;
      overflow: visible;
      opacity: 1;
      max-width: none;
      flex: 1 1 400px;
      margin: 0 0 20px 0;
      padding: 10px;
    }
    .charts-flex-container {
      display: flex;
      flex-wrap: wrap;
      gap: 15px;
      justify-content: flex-start;
      align-items: stretch;
    }
    /* Dynamic sizing based on visible chart count */
    .charts-flex-container .chart-panel-wrapper.visible:only-child {
      flex: 1 1 100%;
      max-width: 100%;
    }
    .inline-chart {
      min-width: 350px;
    }
    .inline-chart.visible {
      flex: 1 1 calc(50% - 8px);
    }
    .drilldown-chart {
      max-width: 100% !important;
      width: 100%;
      margin: 20px 0;
    }
    /* Comparison banner shown when chart is expanded */
    .comparison-banner {
      display: flex;
      justify-content: center;
      align-items: center;
      gap: 15px;
      padding: 10px 15px;
      margin-bottom: 10px;
      border-radius: 6px;
      font-size: 14px;
      font-weight: 500;
      background: #f0f4f8;
      border: 1px solid #d0d7de;
    }
    .comparison-banner .separator { color: #999; }
    .comparison-banner .current { color: #333; }
    .comparison-banner .historical { color: #666; }
    .comparison-banner .diff.positive { color: #16a34a; font-weight: 600; }
    .comparison-banner .diff.negative { color: #dc2626; font-weight: 600; }
    .comparison-banner.positive { background: #dcfce7; border-color: #86efac; }
    .comparison-banner.negative { background: #fee2e2; border-color: #fca5a5; }
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
      width: 100%;
      max-width: 100%;
      display: block;
      overflow-x: hidden;
    }
    .category-section {
      width: 100%;
      max-width: 100%;
      margin-bottom: 20px;
      overflow-x: hidden;
    }
    .category-section .row {
      display: flex;
      flex-wrap: wrap;
      margin-left: -15px;
      margin-right: -15px;
    }
    .category-section .col-md-4 {
      display: flex;
      flex-direction: column;
      padding-left: 15px;
      padding-right: 15px;
      margin-bottom: 15px;
    }
    /* Charts within category sections - use specific class to avoid Bootstrap .row conflict */
    .category-section .charts-grid-row {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 15px;
      margin-top: 15px;
      width: 100%;
      max-width: 100%;
      box-sizing: border-box;
    }
    .category-section .category-chart {
      background: #f9f9f9;
      border: 1px solid #ddd;
      border-radius: 5px;
      padding: 8px;
      display: none;
      min-width: 0;
      max-width: 100%;
      box-sizing: border-box;
    }
    .category-section .category-chart.visible {
      display: block !important;
      animation: fadeIn 0.3s ease-in;
    }
    /* Remove all size classes - let content determine height */
    .category-section .category-chart .plotly {
      width: 100% !important;
      max-width: 100% !important;
    }
    .category-section .category-chart .chart-panel {
      width: 100%;
      max-width: 100%;
    }
    /* Force plotly to not scroll */
    .category-chart .js-plotly-plot,
    .category-chart .plot-container,
    .category-chart .svg-container {
      overflow: visible !important;
    }
    /* Ensure legend is visible - uiOutput creates shiny-html-output wrapper */
    .category-chart .shiny-html-output {
      display: block !important;
      visibility: visible !important;
      min-height: 20px;
    }
    .category-chart .chart-legend {
      display: flex !important;
      visibility: visible !important;
      font-size: 11px !important;
      padding: 4px 0 !important;
      gap: 12px !important;
      justify-content: center;
    }
    /* Compact chart title for category charts */
    .category-chart .chart-title {
      font-size: 12px;
      margin-bottom: 4px;
      padding-bottom: 3px;
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
    
    /* Loading Skeleton Styles */
    .skeleton-box {
      background: linear-gradient(90deg, #f0f0f0 25%, transparent 37%, transparent 63%, #f0f0f0 75%);
      background-size: 400% 100%;
      animation: skeleton-shimmer 1.5s ease-in-out infinite;
      border-radius: 8px;
      border: 1px solid #e0e0e0;
      height: 120px; /* Match typical stat-box height */
      margin-bottom: 15px;
      position: relative;
    }
    @keyframes skeleton-shimmer {
      0% { background-position: 100% 50%; }
      100% { background-position: -100% 50%; }
    }
    .skeleton-content {
      padding: 20px;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      height: 100%;
    }
    .skeleton-title {
      width: 80%;
      height: 16px;
      background: #e0e0e0;
      border-radius: 4px;
      margin-bottom: 10px;
    }
    .skeleton-value {
      width: 60%;
      height: 28px;
      background: #e0e0e0;
      border-radius: 4px;
      margin-bottom: 8px;
    }
    .skeleton-icon {
      width: 24px;
      height: 24px;
      background: #e0e0e0;
      border-radius: 50%;
      margin-bottom: 10px;
    }
    /* Category skeleton */
    .skeleton-category {
      margin-bottom: 25px;
    }
    .skeleton-category-header {
      width: 150px;
      height: 14px;
      background: #e0e0e0;
      border-radius: 4px;
      margin-bottom: 12px;
    }
    
    /* Smooth transition between skeleton and real content */
    #summary_stats_container {
      min-height: 140px; /* Prevent layout jump */
      transition: opacity 0.3s ease-in-out;
    }
    
    /* Initial prompt before first Refresh */
    .initial-prompt {
      display: flex;
      align-items: center;
      justify-content: center;
      padding: 30px 20px;
      color: #888;
      font-size: 16px;
      border: 2px dashed #ddd;
      border-radius: 8px;
      background: #fafafa;
      margin: 10px 0;
    }
    .initial-prompt i {
      margin-right: 10px;
      font-size: 20px;
      color: #aaa;
    }
    
    /* Loading state visual feedback */
    .loading-indicator {
      display: flex;
      align-items: center;
      justify-content: center;
      padding: 10px;
      color: #666;
      font-size: 14px;
      margin-bottom: 15px;
    }
    .loading-indicator i {
      margin-right: 8px;
      animation: spin 1s linear infinite;
    }
    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
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
    
    // Clear banners and reset active states when refresh is clicked
    $(document).on('click', '#refresh', function() {
      // Remove all comparison banners (they will be stale after refresh)
      $('.comparison-banner').remove();
      // Close all open charts and reset active states
      $('.chart-panel-wrapper.visible').removeClass('visible');
      $('.stat-box-clickable.active').removeClass('active');
      // Hide initial prompt (first load) and stats, show skeleton
      $('#initial_prompt_static').hide();
      $('#summary_stats_wrapper').hide();
      $('#loading_skeleton_static').fadeIn(150);
    });
    
    // Handler to swap skeleton for real content when data is ready
    Shiny.addCustomMessageHandler('hideLoadingSkeleton', function(msg) {
      $('#initial_prompt_static').hide();
      $('#loading_skeleton_static').fadeOut(200, function() {
        $('#summary_stats_wrapper').fadeIn(300);
        
        // Force all pre-rendered Plotly charts to resize after layout is applied
        setTimeout(function() {
          $('.category-chart .plotly').each(function() {
            if (window.Plotly && this.layout) {
              window.Plotly.Plots.resize(this);
              window.Plotly.redraw(this);
            }
          });
        }, 250);
      });
    });
    
    // Value box click to toggle chart visibility (for district view - metric boxes)
    $(document).on('click', '.stat-box-clickable[data-metric-id]', function() {
      var metricId = $(this).data('metric-id');
      
      // Check if this metric has a redirect URL (e.g., vector_index -> trap surveillance map)
      var redirectUrl = $(this).data('redirect');
      if (redirectUrl && redirectUrl !== '') {
        window.location.href = redirectUrl;
        return;
      }
      
      var chartWrapper = $('#chart_wrapper_' + metricId);
      var statBox = $(this);
      
      // Get comparison data from data attributes
      var currentWeek = statBox.data('current-week');
      var historicalAvg = statBox.data('historical-avg');
      var pctDiff = statBox.data('pct-diff');
      var weekNum = statBox.data('week-num');
      
      // Toggle visibility
      chartWrapper.toggleClass('visible');
      statBox.toggleClass('active');
      
      // Update comparison banner in chart wrapper
      var comparisonBanner = chartWrapper.find('.comparison-banner');
      if (chartWrapper.hasClass('visible') && historicalAvg && currentWeek !== undefined && currentWeek !== '') {
        var diffClass = pctDiff >= 0 ? 'positive' : 'negative';
        var diffSign = pctDiff >= 0 ? '+' : '';
        var bannerHtml = '<div class=\"comparison-banner ' + diffClass + '\">' +
          '<span class=\"current\">Current: ' + Math.round(currentWeek).toLocaleString() + '</span>' +
          '<span class=\"separator\">|</span>' +
          '<span class=\"historical\">10yr Week Avg: ' + Math.round(historicalAvg).toLocaleString() + '</span>' +
          '<span class=\"separator\">|</span>' +
          '<span class=\"diff ' + diffClass + '\">' + diffSign + pctDiff + '%</span>' +
          '</div>';
        if (comparisonBanner.length) {
          comparisonBanner.replaceWith(bannerHtml);
        } else {
          chartWrapper.prepend(bannerHtml);
        }
      } else {
        comparisonBanner.remove();
      }
      
      // Scroll to chart if now visible and resize Plotly
      if (chartWrapper.hasClass('visible')) {
        setTimeout(function() {
          // Force the chart to fully show first
          chartWrapper.show();
          
          // Find and resize plotly charts in this specific wrapper
          chartWrapper.find('.plotly').each(function() {
            var plotElement = this;
            
            if (window.Plotly) {
              // Force plotly to detect the new dimensions
              if (plotElement.layout) {
                window.Plotly.Plots.resize(plotElement);
              }
              
              // If chart still not visible, try to redraw
              setTimeout(function() {
                if (window.Plotly && plotElement.layout) {
                  window.Plotly.redraw(plotElement);
                }
              }, 200);
            }
          });
          
          // Scroll to the visible chart
          $('html, body').animate({
            scrollTop: chartWrapper.offset().top - 80
          }, 300);
        }, 100);
      } else {
        // Also resize remaining charts when one is hidden
        setTimeout(function() {
          $('.chart-panel-wrapper.visible .plotly').each(function() {
            if (window.Plotly && this.layout) {
              window.Plotly.Plots.resize(this);
            }
          });
        }, 350);
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
    # For current progress charts
    # Metrics with display_as_average don't show "Progress" - they show status/comparison
    if (isTRUE(config$display_as_average)) {
      config$display_name  # Just the metric name (e.g., "avg mosquitoes per trap")
    } else {
      paste0(config$display_name, " Progress")  # Traditional progress metrics
    }
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
    
    # Facility Detail Boxes Container (for facilities view drill-down)
    div(id = "facility_detail_container",
      class = "facility-detail-section",
      style = "margin-top: 20px;",
      uiOutput("facility_detail_boxes")
    ),
    
    # Main Charts Container (only for drill-down views - district charts are now in category sections)
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
      }
      # District view charts are now embedded in category sections
    )
  )
}
