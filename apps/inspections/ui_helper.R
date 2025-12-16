# Inspections App - UI Helper Functions

library(shiny)
library(DT)
library(plotly)
library(ggplot2)

# Source shared helpers
source("../../shared/db_helpers.R")
source("../../shared/stat_box_helpers.R")

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
  fluidPage(
    # Use universal CSS from db_helpers for consistent text sizing
    get_universal_text_css(),
    
    # Application title
    titlePanel("Site Inspection Analysis"),
    
    # Sidebar with controls
    sidebarLayout(
      sidebarPanel(
        # Load Data button at the top
        actionButton("load_data", "Load New Data", icon = icon("refresh"), class = "btn-success", style = "width: 100%;"),
        hr(),
        
        # Data summary
        div(class = "summary-box",
          div(class = "summary-text", 
            textOutput("data_summary"),
            style = "text-align: center;")
        ),
        
        tags$small(
          tags$i(class = "fa fa-info-circle", style = "color: #17a2b8;"),
          " Note: This button will load in new data. To run an analysis on this data, click the analysis buttons below.",
          style = "color: #666; font-style: italic; display: block; margin-bottom: 15px;"
        ),
        hr(),
        
        # Global filters
        selectInput("air_gnd", "Site Type:", 
          choices = c("🚁 Air Sites" = "A", "🚶 Ground Sites" = "G", "All Sites" = "both"), 
          selected = "A"),
        
        selectInput("facility", "Facility:", 
          choices = get_facility_display_choices(), 
          selected = "all"),
        
        selectizeInput("fosarea", "FOS Area:", 
          choices = get_fosarea_display_choices(), 
          selected = "all", multiple = TRUE),
        
        radioButtons("zone", "Zone Display:",
          choices = c("P1 Only" = "1", 
                     "P2 Only" = "2", 
                     "P1 and P2" = "1,2"),
          selected = "1,2",
          inline = FALSE),
        
        selectizeInput("priority", "Priority:", 
          choices = get_priority_choices(include_all = TRUE), 
          selected = "all", multiple = TRUE),
        
        numericInput("years_back", "Years Back:", 
          value = 5, min = 1, max = 20, step = 1),
        
        radioButtons("drone_filter", "Drone Sites:", 
          choices = c("Drone Sites Only" = "drone_only", 
                     "Non-Drone Sites Only" = "no_drone",
                     "Include Drone Sites" = "include_drone"), 
          selected = "include_drone", inline = FALSE),
        
        checkboxInput("spring_only", "Spring Sites Only", 
          value = FALSE),
        tags$small("(Inspections before spring cutoff date)", 
          style = "color: #666; font-style: italic; display: block; margin-bottom: 10px;"),
        
        checkboxInput("prehatch_only", "Prehatch Sites Only",
          value = FALSE),
        
        hr(),
        
        selectInput("color_theme", "Color Theme:",
          choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
          selected = "MMCD")
      ),
      
      # Main panel with tabs
      mainPanel(
        tabsetPanel(
          id = "tabs",
          
          # Inspection Gaps Tab
          tabPanel("Inspection Gaps", value = "gaps",
            fluidRow(
              column(4,
                numericInput("years_gap", "Years Since Last Inspection:", 
                  value = 3, min = 1, max = 10, step = 1)
              ),
              column(4,
                br(),
                div(style = "position: relative;",
                  uiOutput("gaps_button_ui"),
                  div(
                    style = "position: absolute; left: -60px; top: 5px; font-size: 40px; color: #FF5722; animation: bounce 1s infinite;",
                    HTML("&#x27A4;")
                  ),
                  tags$small("← Click here after loading data!", 
                    style = "position: absolute; left: -60px; top: 50px; color: #FF5722; font-weight: bold; white-space: nowrap;")
                )
              ),
              column(4,
                div(style = "margin-top: 25px;",
                  textOutput("gaps_summary")
                )
              )
            ),
            hr(),
            
            fluidRow(
              column(12,
                h3("Gap Analysis by Facility"),
                plotlyOutput("facility_gap_chart", height = "800px")
              )
            ),
            
            fluidRow(
              column(12,
                h3("Sites with Inspection Gaps"),
                div(style = "margin-bottom: 10px; text-align: right;",
                  downloadButton("download_gaps_data", "Download CSV", 
                    class = "btn-success btn-sm")),
                DT::dataTableOutput("gaps_table")
              )
            )
          ),
          
          # Site Analytics Tab
          tabPanel("Wet Analysis", value = "analytics",
            fluidRow(
              column(6,
                numericInput("min_inspections", "Minimum Inspections for Wet Analysis:", 
                  value = 5, min = 1, max = 50, step = 1)
              ),
              column(6,
                br(),
                div(style = "position: relative;",
                  uiOutput("wet_button_ui"),
                  div(
                    style = "position: absolute; left: -60px; top: 5px; font-size: 40px; color: #FF5722; animation: bounce 1s infinite;",
                    HTML("&#x27A4;")
                  ),
                  tags$small("← Click here after loading data!", 
                    style = "position: absolute; left: -60px; top: 50px; color: #FF5722; font-weight: bold; white-space: nowrap;")
                )
              )
            ),
            hr(),
            
            fluidRow(
              column(6,
                uiOutput("total_active_sites_box")
              ),
              column(6,
                uiOutput("overall_wet_percentage_box")
              )
            ),
            
            fluidRow(
              column(6,
                h3("Wet Frequency Distribution"),
                plotlyOutput("wet_frequency_chart", height = "300px")
              ),
              column(6,
                h3("Sites by Priority"),
                plotlyOutput("priority_chart", height = "300px")
              )
            ),
            
            fluidRow(
              column(12,
                h3("Wet Analysis - All Records with Wet Values"),
                div(style = "margin-bottom: 10px; text-align: right;",
                  downloadButton("download_wet_frequency_data", "Download CSV", 
                    class = "btn-success btn-sm")),
                DT::dataTableOutput("wet_frequency_table")
              )
            )
          ),
          
          # Larvae Threshold Tab
          tabPanel("Larvae Threshold", value = "larvae",
            fluidRow(
              column(4,
                numericInput("larvae_threshold", "Sites with at least this num dip:", 
                  value = 2, min = 0, max = 100, step = 1)
              ),
              column(4,
                br(),
                div(style = "position: relative;",
                  uiOutput("larvae_button_ui"),
                  div(
                    style = "position: absolute; left: -60px; top: 5px; font-size: 40px; color: #FF5722; animation: bounce 1s infinite;",
                    HTML("&#x27A4;")
                  ),
                  tags$small("← Click here after loading data!", 
                    style = "position: absolute; left: -60px; top: 50px; color: #FF5722; font-weight: bold; white-space: nowrap;")
                )
              ),
              column(4,
                div(style = "margin-top: 25px;",
                  textOutput("larvae_summary")
                )
              )
            ),
            hr(),
            fluidRow(
              column(6,
                h3("Exceedance Frequency Distribution"),
                plotlyOutput("exceedance_frequency_chart", height = "300px")
              ),
              column(6,
                h3("Avg dip Count Distribution"),
                plotlyOutput("larvae_distribution_chart", height = "300px")
              )
            ),
            
            fluidRow(
              column(12,
                h3("High dip count Sites Analysis"),
                div(style = "margin-bottom: 10px; text-align: right;",
                  downloadButton("download_larvae_data", "Download CSV", 
                    class = "btn-success btn-sm")),
                DT::dataTableOutput("larvae_table")
              )
            )
          )
        )
      )
    ),
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        @keyframes bounce {
          0%, 20%, 50%, 80%, 100% {
            transform: translateX(0);
          }
          40% {
            transform: translateX(-10px);
          }
          60% {
            transform: translateX(-5px);
          }
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
        .btn-success {
          background: linear-gradient(45deg, #2196F3, #21CBF3);
          border: none;
          color: white;
          font-weight: bold;
          transition: all 0.3s ease;
          border-radius: 6px;
        }
        .btn-success:hover {
          background: linear-gradient(45deg, #1976D2, #1E88E5);
          transform: translateY(-1px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
      "))
    )
  )
}