# District Overview - UI Helper Functions
# Creates the user interface for the district overview dashboard
# Top-level view showing ALL MMCD aggregated by zone (P1 vs P2)
# Click on a zone bar to drill down to Facilities Overview

#' Create the main UI for the district overview app
#' @return Shiny UI object
district_overview_ui <- function() {
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
      tags$style(HTML("
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
        .summary-box:nth-child(2) .summary-box {
          background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%);
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
      "))
    ),
    
    # Page Header
    div(class = "page-header",
      h1("District Overview Dashboard"),
      div(class = "subtitle", "All MMCD treatment progress by zone - Click a bar to drill down to Facilities")
    ),
    
    # Controls Panel - Same filters as facilities_overview for consistency
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
    
    # Summary Statistics Row
    uiOutput("summary_stats"),
    
    # Charts Grid - 2x2 layout
    div(class = "dashboard-container",
      fluidRow(
        # Row 1: Catch Basin and Drone
        column(6,
          div(class = "chart-panel",
            div(class = "chart-title", 
                icon("tint"), " Catch Basin Treatment Progress",
                span(class = "filter-info-btn", 
                     onclick = "toggleFilterInfo('cb_filters')", "+"),
                div(id = "cb_filters", class = "filter-tooltip",
                    HTML("<b>Filters Applied:</b><br>
                          • Wet catch basins only (status_udw = 'W')<br>
                          • All facilities<br>
                          • Zone filter from dropdown"))
            ),
            plotlyOutput("catch_basin_chart", height = "300px")
          )
        ),
        column(6,
          div(class = "chart-panel",
            div(class = "chart-title", 
                icon("helicopter"), " Drone Site Progress",
                span(class = "filter-info-btn", 
                     onclick = "toggleFilterInfo('drone_filters')", "+"),
                div(id = "drone_filters", class = "filter-tooltip",
                    HTML("<b>Filters Applied:</b><br>
                          • Drone types: Y, M, C<br>
                          • All facilities, all foremen<br>
                          • Prehatch only: YES<br>
                          • Zone filter from dropdown"))
            ),
            plotlyOutput("drone_chart", height = "300px")
          )
        )
      ),
      fluidRow(
        # Row 2: Ground Prehatch and Structure
        column(6,
          div(class = "chart-panel",
            div(class = "chart-title", 
                icon("seedling"), " Ground Prehatch Progress",
                span(class = "filter-info-btn", 
                     onclick = "toggleFilterInfo('ground_filters')", "+"),
                div(id = "ground_filters", class = "filter-tooltip",
                    HTML("<b>Filters Applied:</b><br>
                          • Prehatch sites only<br>
                          • All facilities<br>
                          • Zone filter from dropdown"))
            ),
            plotlyOutput("ground_prehatch_chart", height = "300px")
          )
        ),
        column(6,
          div(class = "chart-panel",
            div(class = "chart-title", 
                icon("building"), " Structure Treatment Progress",
                span(class = "filter-info-btn", 
                     onclick = "toggleFilterInfo('struct_filters')", "+"),
                div(id = "struct_filters", class = "filter-tooltip",
                    HTML("<b>Filters Applied:</b><br>
                          • Structure types: All<br>
                          • Status: W, U (Wet, Unknown)<br>
                          • Priority: All<br>
                          • Zone filter from dropdown"))
            ),
            plotlyOutput("structure_chart", height = "300px")
          )
        )
      )
    )
  )
}
