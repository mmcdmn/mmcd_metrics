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

# DEPRECATED FUNCTIONS - kept for compatibility

#' Create the main filter panel for control efficacy analysis
#'
#' @return A box containing all filter controls
create_filter_panel <- function() {
  box(
    title = "Analysis Controls",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    
    fluidRow(
      column(3,
        dateRangeInput("date_range", "Select Date Range:",
          start = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
          end = Sys.Date(),
          max = Sys.Date()
        )
      ),
      column(2,
        selectInput("facility_filter", "Facility:",
          choices = get_facility_choices(),
          selected = "all"
        )
      ),
      column(2,
        selectInput("matcode_filter", "Material Code:",
          choices = get_treatment_material_choices(),
          selected = "all"
        )
      ),
      column(2,
        selectInput("color_theme", "Color Theme:",
          choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
          selected = "MMCD"
        )
      ),
      column(3,
        radioButtons("checkback_type", "Checkback Target:",
          choices = list("Percentage" = "percent", "Fixed Number" = "number"),
          selected = "percent",
          inline = TRUE
        )
      )
    ),
    
    fluidRow(
      column(3,
        conditionalPanel(
          condition = "input.checkback_type == 'percent'",
          numericInput("checkback_percent", "Required Checkback %:",
            value = 10, min = 0, max = 100, step = 5
          )
        ),
        conditionalPanel(
          condition = "input.checkback_type == 'number'",
          numericInput("checkback_number", "Number of Checkbacks Required:",
            value = 10, min = 1, step = 1
          )
        )
      )
    )
  )
}

#' Create filter panel specific for progress tab
#'
#' @return A box containing progress tab filter controls
create_progress_filter_panel <- function() {
  box(
    title = "Controls",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    
    fluidRow(
      column(3,
        dateRangeInput("date_range_progress", "Select Date Range:",
          start = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
          end = Sys.Date(),
          max = Sys.Date()
        )
      ),
      column(2,
        selectInput("facility_filter_progress", "Facility:",
          choices = get_facility_choices(),
          selected = "all"
        )
      ),
      column(2,
        selectInput("matcode_filter_progress", "Material Code:",
          choices = get_treatment_material_choices(),
          selected = "all"
        )
      ),
      column(2,
        selectInput("color_theme_progress", "Color Theme:",
          choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
          selected = "MMCD"
        )
      ),
      column(3,
        radioButtons("checkback_type_progress", "Checkback Target:",
          choices = list("Percentage" = "percent", "Fixed Number" = "number"),
          selected = "percent",
          inline = TRUE
        )
      )
    ),
    
    fluidRow(
      column(3,
        conditionalPanel(
          condition = "input.checkback_type_progress == 'percent'",
          numericInput("checkback_percent_progress", "Required Checkback %:",
            value = 10, min = 0, max = 100, step = 5
          )
        ),
        conditionalPanel(
          condition = "input.checkback_type_progress == 'number'",
          numericInput("checkback_number_progress", "Number of Checkbacks Required:",
            value = 10, min = 1, step = 1
          )
        )
      )
    )
  )
}

#' Create value boxes row for summary statistics
#'
#' @param output_ids Vector of output IDs for the value boxes
#' @param width Width for each box (default 3 for 4 boxes in a row)
#'
#' @return A fluidRow with value boxes
create_value_boxes_row <- function(output_ids, width = 3) {
  do.call(fluidRow, lapply(output_ids, function(id) {
    valueBoxOutput(id, width = width)
  }))
}

#' Create a chart box with consistent styling
#'
#' @param title Box title
#' @param output_id Output ID for the plot
#' @param height Plot height (default "400px")
#' @param status Box color status (default "info")
#' @param width Box width (default 12)
#' @param plot_type Either "plot" for renderPlot or "plotly" for renderPlotly
#'
#' @return A box containing a plot output
create_chart_box <- function(title, output_id, height = "400px", 
                             status = "info", width = 12, plot_type = "plot") {
  output_func <- if (plot_type == "plotly") {
    plotly::plotlyOutput(output_id, height = height)
  } else {
    plotOutput(output_id, height = height)
  }
  
  box(
    title = title,
    status = status,
    solidHeader = TRUE,
    width = width,
    output_func
  )
}

#' Create a table box with consistent styling
#'
#' @param title Box title
#' @param output_id Output ID for the table
#' @param status Box color status (default "primary")
#' @param width Box width (default 12)
#'
#' @return A box containing a DataTable output
create_table_box <- function(title, output_id, status = "primary", width = 12) {
  box(
    title = title,
    status = status,
    solidHeader = TRUE,
    width = width,
    DT::dataTableOutput(output_id)
  )
}

#' Create help text box explaining checkback concepts
#'
#' @return A box with help text
create_help_box <- function() {
  box(
    title = "Understanding Checkbacks",
    status = "info",
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    collapsed = TRUE,
    
    div(
      style = "font-size: 16px; line-height: 1.6;",
      
      h4("What is a Checkback?", style = "font-size: 18px;"),
      p("A checkback is a post-treatment inspection (action='4') conducted after an air treatment 
         to verify efficacy. The checkback must have ", code("posttrt_p IS NOT NULL"), " to be valid."),
      
      hr(),
      
      h4("Timeline", style = "font-size: 18px;"),
      tags$ol(
        tags$li(strong("Initial Inspection"), " - Larval survey shows mosquito presence (action='1' or '2')"),
        tags$li(strong("Air Treatment"), " - Site is treated (action='A')"),
        tags$li(strong("Checkback"), " - Post-treatment inspection (action='4', posttrt_p not null)"),
        tags$li(strong("Next Treatment/Inspection"), " - Invalidates previous checkbacks for that site")
      ),
      
      hr(),
      
      h4("Broods", style = "font-size: 18px;"),
      p("A brood is a multi-day air treatment event. Consecutive treatment days (max 1-day gap) 
         by the same facility are grouped into a single brood. Each brood requires a certain 
         number or percentage of sites to receive checkbacks."),
      
      hr(),
      
      h4("Dip Count Comparison", style = "font-size: 18px;"),
      p(strong("Pre-treatment dip count:"), " From the air treatment record (action='A'). 
         This represents the larval density when treatment occurred."),
      p(strong("Post-treatment dip count:"), " From the checkback inspection (action='4'). 
         This shows larval density after treatment."),
      p(strong("Efficacy:"), " Measured by reduction in dip counts (pre - post).")
    )
  )
}
