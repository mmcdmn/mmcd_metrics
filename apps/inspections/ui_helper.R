# Inspections App - UI Helper Functions

library(shiny)
library(shinydashboard)
library(DT)

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

# Create the main UI panel with better styling
create_main_ui <- function() {
  dashboardPage(
    dashboardHeader(title = "Site Inspection Coverage Gaps"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Inspection Gaps", tabName = "gaps", icon = icon("search"))
      )
    ),
    dashboardBody(
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
        "))
      ),
      
      box(
        title = icon("filter", "Filters"), 
        status = "primary", 
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        fluidRow(
          column(2,
            selectInput("air_gnd", "Site Type:", 
              choices = c("🚁 Air Sites" = "A", "🚶 Ground Sites" = "G"), 
              selected = "A")
          ),
          column(3,
            selectizeInput("facility", "Facility:", 
              choices = get_facility_display_choices(), 
              selected = "all", multiple = TRUE)
          ),
          column(3,
            selectizeInput("fosarea", "FOS Area:", 
              choices = get_fosarea_display_choices(), 
              selected = "all", multiple = TRUE)
          ),
          column(2,
            selectizeInput("zone", "Zone:", 
              choices = c("All Zones" = "all", "Zone 1" = "1", "Zone 2" = "2"), 
              selected = "all", multiple = TRUE)
          ),
          column(2,
            selectizeInput("priority", "Priority:", 
              choices = c("All" = "all", " RED" = "RED", " YELLOW" = "YELLOW", " GREEN" = "GREEN"), 
              selected = "all", multiple = TRUE)
          )
        ),
        fluidRow(
          column(4,
            radioButtons("drone_filter", "Drone Sites:", 
              choices = c(" Drone Sites Only" = "drone_only", 
                         " Non-Drone Sites Only" = "no_drone",
                         " Include Drone Sites" = "include_drone"), 
              selected = "include_drone")
          ),
          column(8, "")
        ),
        hr(),
        fluidRow(
          column(3,
            numericInput("years_gap", "Years Since Last Inspection:", 
              value = 3, min = 1, max = 10, step = 1)
          ),
          column(3,
            br(),
            actionButton("refresh", 
              "Load Inspection Gaps", 
              class = "btn-refresh", 
              style = "width: 100%; padding: 12px; font-size: 16px;")
          ),
          column(6,
            div(class = "summary-box",
              div(class = "summary-text", textOutput("summary"))
            )
          )
        )
      ),
      
      box(
        title = icon("table", "Sites with Inspection Gaps"), 
        status = "warning", 
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        DT::dataTableOutput("gaps_table")
      )
    )
  )
}