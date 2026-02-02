# UI Helper Functions for Control Efficacy App
# Reusable UI components for consistent layout

#' Create the main UI for the control efficacy app
#' @return Shiny UI object
control_efficacy_ui <- function() {
  fluidPage(
    # Use universal CSS from db_helpers for consistent text sizing
    get_universal_text_css(),
    
    tags$head(
      tags$style(HTML("
        .sidebar {
          background-color: #f4f4f4;
        }
        .btn-help {
          margin-top: 10px;
          width: 100%;
        }
      "))
    ),
    
    # Application title
    titlePanel("Control Efficacy - Air Treatment Checkbacks"),
    
    # Sidebar with controls
    sidebarLayout(
      sidebarPanel(
        # Refresh button at the top
        actionButton("refresh_data_progress", "Refresh Data", 
          icon = icon("refresh"), 
          class = "btn-success", 
          style = "width: 100%;"),
        hr(),
        
        # Date range - shared control
        dateRangeInput("date_range_progress", "Date Range:",
          start = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
          end = Sys.Date(),
          max = Sys.Date()
        ),
        
        # Facility filter
        selectInput("facility_filter_progress", "Facility:",
          choices = NULL,
          selected = "all"
        ),
        
        # Material code filter
        selectInput("matcode_filter_progress", "Material Code:",
          choices = NULL,
          selected = "all"
        ),
        
        # Species filter
        selectInput("species_filter_progress", "Species Filter:",
          choices = c("All Species" = "all"),
          selected = "all"
        ),
        
        # Checkback target type
        radioButtons("checkback_type_progress", "Checkback Target:",
          choices = list("Percentage" = "percent", "Fixed Number" = "number"),
          selected = "percent"
        ),
        
        # Conditional checkback inputs
        conditionalPanel(
          condition = "input.checkback_type_progress == 'percent'",
          numericInput("checkback_percent_progress", "Required %:",
            value = 10, min = 0, max = 100, step = 5
          )
        ),
        conditionalPanel(
          condition = "input.checkback_type_progress == 'number'",
          numericInput("checkback_number_progress", "Required #:",
            value = 10, min = 1, step = 1
          )
        ),
        
        # Color theme
        selectInput("color_theme_progress", "Color Theme:",
          choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
          selected = "MMCD"
        ),
        
        # Status Tables tab specific controls
        conditionalPanel(
          condition = "input.tabs == 'status_tables'",
          hr(),
          h5("Checkback Details Filters"),
          selectInput("site_facility_filter", "Facility:",
            choices = NULL,
            selected = "all"
          ),
          numericInput("site_min_acres", "Min Acres:",
            value = 0, min = 0, step = 0.1
          ),
          numericInput("site_min_days", "Min Days to Checkback:",
            value = 0, min = 0, step = 1
          )
        ),
        
        # Control Efficacy tab specific controls
        conditionalPanel(
          condition = "input.tabs == 'status'",
          hr(),
          actionButton("show_recent_only", "Show Most Recent Brood",
            icon = icon("filter"),
            class = "btn-info",
            style = "width: 100%; margin-bottom: 5px;"),
          actionButton("show_all_broods", "Show All Broods",
            icon = icon("list"),
            class = "btn-default",
            style = "width: 100%;")
        ),
        
        # Help button at bottom
        hr(),
        actionButton("show_help", "Show Help", 
          icon = icon("question-circle"),
          class = "btn-info btn-help")
      ),
      
      # Main panel with tabs
      mainPanel(
        tabsetPanel(
          id = "tabs",
          tabPanel("Checkback Progress", value = "progress", 
            br(),
            # Summary Statistics
            fluidRow(
              column(3, uiOutput("total_checkbacks_needed")),
              column(3, uiOutput("total_checkbacks_completed")),
              column(3, uiOutput("overall_completion_rate")),
              column(3, uiOutput("avg_days_to_checkback"))
            ),
            br(),
            h4("Checkback Progress by Brood"),
            uiOutput("checkback_progress_chart_ui")
          ),
          tabPanel("Status Tables", value = "status_tables",
            br(),
            h4("Brood Status Table"),
            DT::dataTableOutput("checkback_status_table"),
            br(),
            h4("Site-Level Treatment and Checkback Details"),
            DT::dataTableOutput("site_details")
          ),
          tabPanel("Control Efficacy", value = "status",
            br(),
            div(style = "color: #856404; background-color: #fff3cd; padding: 10px; border-radius: 4px; margin-bottom: 10px;",
              tags$strong("Note:"),
              " Chart looks too busy? Use the sidebar filters to show only the most recent brood or a single facility."
            ),
            h4("Dip Count Changes (Pre/Post Treatment)"),
            plotly::plotlyOutput("dip_changes_plot", height = "800px"),
            br(),
            h4("All Sites with Checkbacks"),
            DT::dataTableOutput("all_checkbacks_table")
          )
        )
      )
    )
  )
}
