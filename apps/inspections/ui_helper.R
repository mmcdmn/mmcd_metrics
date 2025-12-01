# Inspections App - UI Helper Functions

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)

# Source shared helpers
source("../../shared/db_helpers.R")

# Get facility choices with full names for display
get_facility_display_choices <- function() {
  facilities <- get_facility_lookup()
  if (nrow(facilities) == 0) {
    return(c("All Facilities" = "all"))
  }
  
  # Use full names as labels, short names as values for SQL
  choices <- setNames(facilities$short_name, facilities$full_name)
  choices <- c("All Facilities" = "all", choices)
  return(choices)
}

# Get FOS area choices with full names from foremen lookup
get_fosarea_display_choices <- function() {
  return(get_foreman_choices(include_all = TRUE))
}

# Create the main UI panel with shared filters
create_main_ui <- function() {
  dashboardPage(
    dashboardHeader(title = "Site Inspection Analysis"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Inspection Gaps", tabName = "gaps", icon = icon("search")),
        menuItem("Wet Analysis", tabName = "analytics", icon = icon("tint")),
        menuItem("Larvae Threshold", tabName = "larvae", icon = icon("bug"))
      )
    ),
    dashboardBody(
      # Use universal CSS from db_helpers for consistent text sizing
      get_universal_text_css(),
      tags$head(
        tags$style(HTML("
          .content-wrapper, .right-side {
            background-color: #f4f4f4;
          }
          .box {
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          }
          .btn-refresh {
            background: linear-gradient(45deg, #2196F3, #21CBF3);
            border: none;
            color: white;
            font-weight: bold;
            transition: all 0.3s ease;
            border-radius: 6px;
          }
          .btn-refresh:hover {
            background: linear-gradient(45deg, #1976D2, #1E88E5);
            transform: translateY(-1px);
            box-shadow: 0 4px 8px rgba(0,0,0,0.2);
          }
          .control-label {
            font-weight: 600;
            color: #333;
          }
          .summary-box {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 15px;
            border-radius: 8px;
            margin: 10px 0;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          }
          .summary-text {
            font-size: 16px;
            font-weight: 500;
          }
          .stat-box {
            background: linear-gradient(135deg, #4CAF50 0%, #45a049 100%);
            color: white;
            padding: 20px;
            border-radius: 8px;
            margin: 10px 0;
            text-align: center;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          }
          .stat-number {
            font-size: 32px;
            font-weight: bold;
            margin-bottom: 5px;
          }
          .stat-label {
            font-size: 14px;
            opacity: 0.9;
          }
          .filters-box {
            margin-bottom: 20px;
            background: #ffffff;
          }
        "))
      ),
      
      # SHARED FILTER CONTROLS (appears above all tabs)
      box(
        title = icon("filter", "Global Filters - Apply to All Tabs"), 
        status = "primary", 
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        class = "filters-box",
        fluidRow(
          column(2,
            selectInput("air_gnd", "Site Type:", 
              choices = c("🚁 Air Sites" = "A", "🚶 Ground Sites" = "G", "All Sites" = "both"), 
              selected = "A")
          ),
          column(2,
            selectizeInput("facility", "Facility:", 
              choices = get_facility_display_choices(), 
              selected = "all", multiple = TRUE)
          ),
          column(2,
            selectizeInput("fosarea", "FOS Area:", 
              choices = get_fosarea_display_choices(), 
              selected = "all", multiple = TRUE)
          ),
          column(3,
            radioButtons("zone", "Zone Display:",
                        choices = c("P1 Only" = "1", 
                                   "P2 Only" = "2", 
                                   "P1 and P2" = "1,2"),
                        selected = "1,2",
                        inline = TRUE)
          ),
          column(2,
            selectizeInput("priority", "Priority:", 
              choices = get_priority_choices(include_all = TRUE), 
              selected = "all", multiple = TRUE)
          ),
          column(2,
            numericInput("years_back", "Years Back:", 
              value = 5, min = 1, max = 20, step = 1)
          )
        ),
        fluidRow(
          column(3,
            radioButtons("drone_filter", "Drone Sites:", 
              choices = c(" Drone Sites Only" = "drone_only", 
                         " Non-Drone Sites Only" = "no_drone",
                         " Include Drone Sites" = "include_drone"), 
              selected = "include_drone", inline = TRUE)
          ),
          column(2,
            br(),
            checkboxInput("spring_only", "Spring Sites Only", 
              value = FALSE),
              tags$small("(Inspections before spring cutoff date)", 
              style = "color: #666; font-style: italic;")
            ),
            column(2,
              br(),
              checkboxInput("prehatch_only", "Prehatch Sites Only",
                value = FALSE),
            ),
          column(3,
            br(),
            actionButton("load_data", 
              " Load New Data", 
              class = "btn-refresh", 
              style = "width: 100%; padding: 12px; font-size: 16px;")
          ),
          column(4,
            div(class = "summary-box",
              div(class = "summary-text", 
                textOutput("data_summary"),
                style = "text-align: center;")
            )
          )
        ),
        fluidRow(
          column(12,
            div(style = "padding: 10px; text-align: center; margin-top: 5px;",
              tags$small(
                tags$i(class = "fa fa-info-circle", style = "color: #17a2b8;"),
                " Note: This button will load in new data. To run an analysis on this data, click the analysis buttons below.",
                style = "color: #666; font-style: italic;"
              )
            )
          )
        )
      ),
      
      tabItems(
        # Inspection Gaps Tab
        tabItem(tabName = "gaps",
          fluidRow(
            column(4,
              numericInput("years_gap", "Years Since Last Inspection:", 
                value = 3, min = 1, max = 10, step = 1)
            ),
            column(4,
              br(),
              uiOutput("gaps_button_ui")
            ),
            column(4,
              div(style = "margin-top: 25px;",
                textOutput("gaps_summary")
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Gap Analysis by Facility", 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              plotlyOutput("facility_gap_chart", height = "800px")
            )
          ),
          
          box(
            title = icon("table", "Sites with Inspection Gaps"), 
            status = "warning", 
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            div(style = "margin-bottom: 10px; text-align: right;",
                downloadButton("download_gaps_data", "Download CSV", 
                              class = "btn-success btn-sm")),
            DT::dataTableOutput("gaps_table")
          )
        ),
        
        # Site Analytics Tab
        tabItem(tabName = "analytics",
          fluidRow(
            column(6,
              numericInput("min_inspections", "Minimum Inspections for Wet Analysis:", 
                value = 5, min = 1, max = 50, step = 1)
            ),
            column(6,
              br(),
              uiOutput("wet_button_ui")
            )
          ),
          
          fluidRow(
            valueBoxOutput("total_active_sites", width = 6),
            valueBoxOutput("overall_wet_percentage", width = 6)
          ),
          
          fluidRow(
            box(
              title = "Wet Frequency Distribution", 
              status = "info", 
              solidHeader = TRUE,
              width = 6,
              plotlyOutput("wet_frequency_chart", height = "300px")
            ),
            box(
              title = "Sites by Priority", 
              status = "warning", 
              solidHeader = TRUE,
              width = 6,
              plotlyOutput("priority_chart", height = "300px")
            )
          ),
          
          box(
            title = icon("tint", "Wet Analysis - All Records with Wet Values"), 
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            div(style = "margin-bottom: 10px; text-align: right;",
                downloadButton("download_wet_frequency_data", "Download CSV", 
                              class = "btn-success btn-sm")),
            DT::dataTableOutput("wet_frequency_table")
          )
        ),
        
        # Larvae Threshold Tab
        tabItem(tabName = "larvae",
          fluidRow(
            column(4,
              numericInput("larvae_threshold", "Sites with at least this num dip:", 
                value = 2, min = 0, max = 100, step = 1)
            ),
            column(4,
              br(),
              uiOutput("larvae_button_ui")
            ),
            column(4,
              div(style = "margin-top: 25px;",
                textOutput("larvae_summary")
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Exceedance Frequency Distribution", 
              status = "danger", 
              solidHeader = TRUE,
              width = 6,
              plotlyOutput("exceedance_frequency_chart", height = "300px")
            ),
            box(
              title = "Avg dip Count Distribution", 
              status = "warning", 
              solidHeader = TRUE,
              width = 6,
              plotlyOutput("larvae_distribution_chart", height = "300px")
            )
          ),
          
          box(
            title = icon("bug", "High dip count Sites Analysis"), 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            div(style = "margin-bottom: 10px; text-align: right;",
                downloadButton("download_larvae_data", "Download CSV", 
                              class = "btn-success btn-sm")),
            DT::dataTableOutput("larvae_table")
          )
        )
      )
    )
  )
}