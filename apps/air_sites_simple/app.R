# Simple Air Sites Status App
# Tracks Active Treatment vs Unknown status based on material effect days and Lab results
# No rainfall data involved

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(leaflet)
  library(dplyr)
  library(plotly)
  library(tidyr)
})

# Source shared database helpers
# Check if we're running from the app directory or root directory
if (file.exists("../../shared/db_helpers.R")) {
  source("../../shared/db_helpers.R")
} else if (file.exists("shared/db_helpers.R")) {
  source("shared/db_helpers.R")
} else {
  stop("Cannot find db_helpers.R file")
}

# Source stat box helpers
if (file.exists("../../shared/stat_box_helpers.R")) {
  source("../../shared/stat_box_helpers.R")
} else if (file.exists("shared/stat_box_helpers.R")) {
  source("shared/stat_box_helpers.R")
} else {
  stop("Cannot find stat_box_helpers.R file")
}

# Source app-specific functions
source("data_functions.R")
source("display_functions.R")
source("historical_functions.R")

# Functions are now loaded from data_functions.R and display_functions.R

# Define UI
ui <- fluidPage(
  titlePanel("Air Work Pipeline - Simple"),
  
  # Use universal CSS from db_helpers for consistent text sizing
  get_universal_text_css(),
  
  sidebarLayout(
    sidebarPanel(
      
      # Color theme selector (global, shown on all tabs)
      selectInput("color_theme", "Color Theme:",
                  choices = c("MMCD (Default)" = "MMCD",
                             "IBM Design" = "IBM",
                             "Color-Blind Friendly" = "Wong",
                             "Scientific" = "Tol",
                             "Viridis" = "Viridis",
                             "ColorBrewer" = "ColorBrewer"),
                  selected = "MMCD"),
      tags$small(style = "color: #666;", "Changes map and chart colors"),
      
      hr(),
      
      # === Status Tab Filters ===
      conditionalPanel(
        condition = "input.main_tabset == 'Air Site Status'",
        
        # Refresh button for Status tab
        actionButton("refresh_data", "Refresh Data", 
                     icon = icon("refresh"),
                     class = "btn-primary btn-lg",
                     style = "width: 100%;"),
        p(style = "text-align: center; margin-top: 10px; margin-bottom: 10px; font-size: 0.9em; color: #666;",
          "Click to refresh status data"),
        
        hr(),
        
        h4("Status Filters"),
        
        dateInput("analysis_date", "Analysis Date:",
                  value = Sys.Date(),
                  max = Sys.Date()),
        
        selectizeInput("facility_filter", "Facilities:",
                      choices = c("Loading..." = "LOADING"),
                      selected = "LOADING",
                      multiple = TRUE),
        
        selectizeInput("priority_filter", "Priorities:",
                      choices = c("Loading..." = "LOADING"),
                      selected = "LOADING",
                      multiple = TRUE),
        
        radioButtons("zone_filter", "Zones:",
                    choices = c("Loading..." = "LOADING"),
                    selected = "LOADING"),
        
        numericInput("larvae_threshold", "Larvae Threshold:",
                    value = 2,
                    min = 0,
                    max = 10,
                    step = 1),
        
        radioButtons("metric_type", "Display Metric:",
                    choices = c("Number of Sites" = "sites", "Acres" = "acres"),
                    selected = "sites",
                    inline = TRUE),
        
        selectInput("status_filter", "Status Filter:",
                   choices = c("All Statuses" = "all",
                              "Not Insp" = "Unknown",
                              "Insp" = "Inspected", 
                              "Needs ID" = "Needs ID",
                              "Needs Treatment" = "Needs Treatment",
                              "Active Treatment" = "Active Treatment"),
                   selected = "all"),
        
        selectizeInput("material_filter", "Treatment Materials:",
                      choices = c("Loading..." = "LOADING"),
                      selected = "LOADING",
                      multiple = TRUE),
        
        div(style = "margin-top: -10px; margin-bottom: 10px;",
          actionButton("btn_prehatch", "Prehatch Only", class = "btn-sm btn-primary", style = "margin-right: 5px;"),
          actionButton("btn_bti", "BTI Only", class = "btn-sm btn-success")
        ),
        
        numericInput("bti_effect_days_override", "BTI Effect Days Override:",
                    value = NA,
                    min = 1,
                    max = 60,
                    step = 1),
        tags$small(class = "text-muted", "Leave empty to use default from database")
      ),
      
      # === Process Tab Filters ===
      conditionalPanel(
        condition = "input.main_tabset == 'Pipeline Snapshot'",
        
        actionButton("refresh_process_data", "Refresh Process Data", 
                     icon = icon("refresh"),
                     class = "btn-primary btn-lg",
                     style = "width: 100%;"),
        p(style = "text-align: center; margin-top: 10px; margin-bottom: 10px; font-size: 0.9em; color: #666;",
          "Click to refresh process data"),
        
        hr(),
        
        h4("Process Filters"),
        
        dateInput("process_analysis_date", "Analysis Date:",
                  value = Sys.Date(),
                  max = Sys.Date()),
        
        selectizeInput("process_facility_filter", "Facilities:",
                      choices = c("Loading..." = "LOADING"),
                      selected = "LOADING",
                      multiple = TRUE),
        
        radioButtons("process_zone_filter", "Zones:",
                    choices = c("Loading..." = "LOADING"),
                    selected = "LOADING"),
        
        numericInput("process_larvae_threshold", "Larvae Threshold:",
                    value = 2,
                    min = 0,
                    max = 10,
                    step = 1),
        
        radioButtons("process_metric_type", "Display Metric:",
                    choices = c("Number of Sites" = "sites", "Acres" = "acres"),
                    selected = "sites",
                    inline = TRUE),
        
        selectizeInput("process_material_filter", "Treatment Materials:",
                      choices = c("Loading..." = "LOADING"),
                      selected = "LOADING",
                      multiple = TRUE),
        
        div(style = "margin-top: -10px; margin-bottom: 10px;",
          actionButton("btn_process_prehatch", "Prehatch Only", class = "btn-sm btn-primary", style = "margin-right: 5px;"),
          actionButton("btn_process_bti", "BTI Only", class = "btn-sm btn-success")
        ),
        
        numericInput("process_bti_effect_days_override", "BTI Effect Days Override:",
                    value = NA,
                    min = 1,
                    max = 60,
                    step = 1),
        tags$small(class = "text-muted", "Leave empty to use default"),
        
        checkboxGroupInput("process_status_filter", "Status Filter:",
                          choices = c("Not Insp" = "Unknown",
                                     "Insp" = "Inspected", 
                                     "Needs ID" = "Needs ID",
                                     "Needs Treatment" = "Needs Treatment",
                                     "Active Treatment" = "Active Treatment"),
                          selected = c("Unknown", "Inspected", "Needs ID", "Needs Treatment", "Active Treatment"))
      ),
      
      # === Historical Tab Filters ===
      conditionalPanel(
        condition = "input.main_tabset == 'Historical Analysis'",
        
        actionButton("refresh_historical_data", "Refresh Historical Data", 
                     icon = icon("refresh"),
                     class = "btn-primary btn-lg",
                     style = "width: 100%;"),
        p(style = "text-align: center; margin-top: 10px; margin-bottom: 10px; font-size: 0.9em; color: #666;",
          "Click to refresh historical data"),
        
        hr(),
        
        h4("Historical Filters"),
        
        selectizeInput("hist_facility_filter", "Facilities:",
                      choices = c("Loading..." = "LOADING"),
                      selected = "LOADING",
                      multiple = TRUE),
        
        selectizeInput("hist_priority_filter", "Priorities:",
                      choices = c("Loading..." = "LOADING"),
                      selected = "LOADING",
                      multiple = TRUE),
        
        radioButtons("hist_zone_filter", "Zones:",
                    choices = c("Loading..." = "LOADING"),
                    selected = "LOADING"),
        
        numericInput("hist_larvae_threshold", "Larvae Threshold:",
                    value = 2,
                    min = 0,
                    max = 10,
                    step = 1),
        
        radioButtons("hist_metric_type", "Display Metric:",
                    choices = c("Number of Sites" = "sites", "Acres" = "acres"),
                    selected = "sites",
                    inline = TRUE),
        
        dateInput("hist_start_date", "Start Date:", 
                 value = Sys.Date() - (365 * 4),
                 max = Sys.Date()),
        
        dateInput("hist_end_date", "End Date:", 
                 value = Sys.Date(),
                 max = Sys.Date()),
        
        selectInput("hist_chart_type", "Chart Type:",
                   choices = list(
                     "Line Chart" = "line",
                     "Grouped Bar Chart" = "bar",
                     "Stacked Bar Chart" = "stacked"
                   ),
                   selected = "line"),
        
        radioButtons("volume_time_period", "Volume Time Period:",
                    choices = c("Weekly" = "weekly", "Yearly" = "yearly"),
                    selected = "yearly",
                    inline = TRUE)
      ),
      
      width = 3
    ),
    
    # Main panel with tabs
    mainPanel(
      tabsetPanel(id = "main_tabset",
        tabPanel("Air Site Status",
          br(),
          
          # Info note
          div(style = "padding: 10px; text-align: center; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; margin-bottom: 15px;",
            tags$i(class = "fa fa-info-circle", style = "color: #17a2b8;"),
            tags$small(" Note: 'Needs Inspection' status is currently under review. This dashboard shows the process from inspection to treatment.")
          ),
          # Summary statistics
          fluidRow(
            column(2, uiOutput("total_air_sites")),
            column(2, uiOutput("sites_unknown")),
            column(2, uiOutput("sites_inspected")),
            column(2, uiOutput("sites_in_lab")),
            column(2, uiOutput("sites_needs_treatment")),
            column(2, uiOutput("sites_active_treatment"))
          ),
          
          fluidRow(
          column(8,
            wellPanel(
              h4("Air Site Status Map", style = "margin-top: 0;"),
              leafletOutput("status_map", height = "500px")
            )
          ),
          column(4,
            wellPanel(
              h4("Status Summary", style = "margin-top: 0;"),
              plotlyOutput("status_chart", height = "500px")
            )
          )
        ),
        
        fluidRow(
          column(12,
            wellPanel(
              tags$details(
                tags$summary(tags$strong("Status Definitions"), style = "cursor: pointer;"),
                tags$div(
                  tags$h5("Air Site Status Definitions:"),
                  tags$ul(
                    tags$li(tags$strong("Not Insp:"), " Sites that have not been inspected or have no recent inspection data"),
                    tags$li(tags$strong("Insp:"), " Sites inspected with larvae count below threshold (no treatment needed)"),
                    tags$li(tags$strong("Needs ID:"), " Sites with Dip ≥ threshold, samples sent to lab for red/blue bug identification"),
                    tags$li(tags$strong("Needs Treatment:"), " Sites with red bugs found in lab analysis (require treatment)"),
                    tags$li(tags$strong("Active Treatment:"), " Sites who recived treatment < effect_days ago acording to material type (see override for BTI)")
                  ),
                  tags$h5("Treatment Logic:"),
                  tags$ul(
                    tags$li("Sites with Dip ≥ threshold → lab for species identification"),
                    tags$li("Red bugs found → treatment required (species that bite humans)"),
                    tags$li("Blue bugs found → no treatment needed (species that don't bite humans)"),
                    tags$li("Active treatments last for BTI effect days (default varies by material)")
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(12,
            wellPanel(
              h4("Site Details", style = "margin-top: 0;"),
              div(style = "text-align: right; margin-bottom: 10px;",
                downloadButton("download_site_details", "Download CSV", class = "btn-success")
              ),
              DT::dataTableOutput("site_details_table")
            )
          )
        )
        ),
        
        tabPanel("Pipeline Snapshot",
          br(),
          
          # Info note
          div(style = "padding: 10px; text-align: center; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; margin-bottom: 15px;",
            tags$i(class = "fa fa-info-circle", style = "color: #17a2b8;"),
            tags$small(" Status filter only affects the flow chart visualization.")
          ),
          
          # Summary statistics
          fluidRow(
            column(3, uiOutput("sites_receiving_treatment")),
            column(3, uiOutput("treatment_rate")),
            column(3, uiOutput("treatment_efficiency")),
            column(3, uiOutput("inspection_coverage"))
          ),
          
          fluidRow(
            column(4, uiOutput("total_inspected_samples")),
            column(4, uiOutput("red_bug_detection_rate")),
            column(4, uiOutput("lab_completion_rate"))
          ),
        
        fluidRow(
          column(12,
            wellPanel(
              tags$details(
                tags$summary(tags$strong("Calculation Notes"), style = "cursor: pointer;"),
                tags$div(
                  tags$h5("Treatment Process Metrics:"),
                  tags$ul(
                    tags$li(tags$strong("Treatment Rate:"), " Active Treatments ÷ (Needs Treatment + Active Treatment) × 100%"),
                    tags$li(tags$strong("Treatment Completion:"), " Active Treatments ÷ (Needs Treatment + Active Treatment) × 100%"),
                    tags$li(tags$strong("Inspection Coverage:"), " (Insp + In Lab + Needs Treatment + Active Treatment) ÷ Total Sites × 100%")
                  ),
                  tags$h5("Lab Processing Metrics:"),
                  tags$ul(
                    tags$li(tags$strong("Total Insp Samples:"), " Number of inspected sites with completed lab results (timestamp data)"),
                    tags$li(tags$strong("Red Bug Detection Rate:"), " (Red Bugs Found ÷ Total Samples) × 100% - Percentage needing treatment")
                  ),
                  tags$p(tags$strong("Note:"), " Total samples = only completed lab samples with timestamps from current analysis date. Pending samples without timestamps are excluded.")
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(8,
            wellPanel(
              h4("Treatment Flow by Facility", style = "margin-top: 0;"),
              plotlyOutput("treatment_flow_chart", height = "400px")
            )
          ),
          column(4,
            wellPanel(
              h4("Process Summary", style = "margin-top: 0;"),
              div(style = "text-align: right; margin-bottom: 10px;",
                downloadButton("download_process_summary", "Download CSV", class = "btn-info")
              ),
              DT::dataTableOutput("process_summary_table")
            )
          )
        ),
        
        fluidRow(
          column(12,
            wellPanel(
              h4("Facility Treatment Process Details", style = "margin-top: 0;"),
              div(style = "text-align: right; margin-bottom: 10px;",
                downloadButton("download_facility_process", "Download CSV", class = "btn-success")
              ),
              DT::dataTableOutput("facility_process_table")
            )
          )
        )
        ),
        
        tabPanel("Historical Analysis",
          br(),
          
          # Info note
          div(style = "padding: 10px; text-align: center; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; margin-bottom: 15px;",
            tags$i(class = "fa fa-info-circle", style = "color: #17a2b8;"),
            tags$small(" This analysis shows inspection history and treatment volumes for each air site over the selected date range.")
          ),
          
          # Summary statistics
          fluidRow(
            column(3, uiOutput("hist_total_sites")),
            column(3, uiOutput("hist_total_inspections")),
            column(3, uiOutput("hist_total_red_bug_inspections")),
            column(3, uiOutput("hist_overall_red_bug_ratio"))
          ),
          
          fluidRow(
            column(4, uiOutput("hist_total_treatments")),
            column(4, uiOutput("hist_total_treatment_acres")),
            column(4, uiOutput("hist_total_inspection_acres"))
          ),
        
        fluidRow(
          column(12,
            wellPanel(
              tags$details(
                tags$summary(tags$strong("Calculation Notes"), style = "cursor: pointer;"),
                tags$div(
                  tags$h5("What is Counted:"),
                  tags$ul(
                    tags$li(tags$strong("Inspections:"), " Action code 4 (AIR inspection) from dblarv_insptrt tables"),
                    tags$li(tags$strong("Treatments:"), " Action codes 3 (Ground treatment), A (AIR treatment), and D (Drone treatment) from dblarv_insptrt tables"),
                    tags$li(tags$strong("Red Bug Inspections:"), " Inspections where lab samples show has_red_bugs = TRUE (species requiring treatment)")
                  ),
                  tags$h5("Metric Calculations:"),
                  tags$ul(
                    tags$li(tags$strong("Total Inspections:"), " Count of all AIR inspection records (action 4) within year range"),
                    tags$li(tags$strong("Total Treatments:"), " Count of all treatment records (actions 3, A, D) within year range"),
                    tags$li(tags$strong("Red Bug Ratio:"), " (Red Bug Inspections ÷ Total Inspections) × 100%"),
                    tags$li(tags$strong("Total Treatment Acres:"), " Sum of site acres for all treatment records"),
                    tags$li(tags$strong("Total Inspection Acres:"), " Sum of site acres for all inspection records")
                  ),
                  tags$p(tags$strong("Note:"), " Only includes air sites (air_gnd = 'A') that were active during the operation date (between startdate and enddate).")
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(8,
            wellPanel(
              h4("Treatment Volume Trends", style = "margin-top: 0;"),
              plotlyOutput("treatment_volume_chart", height = "400px")
            )
          ),
          column(4,
            wellPanel(
              h4("Red Bug Detection Over Time", style = "margin-top: 0;"),
              plotlyOutput("red_bug_trend_chart", height = "400px")
            )
          )
        ),
        
        fluidRow(
          column(6,
            wellPanel(
              h4("Site Inspection History", style = "margin-top: 0;"),
              div(style = "text-align: center; padding-bottom: 10px;",
                tags$small("Red Bug Ratio = (Red Bug Inspections ÷ Total Inspections) × 100%")
              ),
              div(style = "text-align: right; margin-bottom: 10px;",
                downloadButton("download_inspection_history", "Download CSV", class = "btn-success")
              ),
              DT::dataTableOutput("historical_inspection_table")
            )
          ),
          column(6,
            wellPanel(
              h4("Treatment Volume Summary", style = "margin-top: 0;"),
              div(style = "text-align: center; padding-bottom: 10px;",
                tags$small("Weekly and yearly treatment volumes with acres treated")
              ),
              div(style = "text-align: right; margin-bottom: 10px;",
                downloadButton("download_treatment_volume", "Download CSV", class = "btn-warning")
              ),
              DT::dataTableOutput("treatment_volume_table")
            )
          )
        )
      )
    )
  )
)
)


# Server Logic
server <- function(input, output, session) {
  
  # Reactive value for current theme
  current_theme <- reactive({
    input$color_theme
  })
  
  # Update global theme option when theme changes
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
  
  # Initialize filters ONCE on startup - do not make reactive
  # This prevents constant updating and "loading" states
  
  # Load facility choices (static, load once)
  facility_lookup <- get_facility_lookup()
  if (nrow(facility_lookup) > 0) {
    facility_choices <- c("All Facilities" = "all", setNames(facility_lookup$short_name, facility_lookup$full_name))
  } else {
    facility_choices <- c("Failed to load facilities", "error")
  }
  
  # Load priority choices (static, load once)
  priority_choices_raw <- get_priority_choices(include_all = FALSE)
  priority_choices <- c("All Priorities" = "all", priority_choices_raw)
  
  # Load zone choices (static, load once)
  zone_choices <- get_available_zones()
  zone_display <- setNames(zone_choices, zone_choices)
  
  # Load treatment material choices (static, load once)
  material_choices <- get_material_choices(include_all = TRUE)
  if (length(material_choices) == 0) {
    material_choices <- c("All Materials" = "all")
  }
  material_display <- material_choices
  
  # Update all filter inputs ONCE on startup
  observe({
    # Update facility filters - start empty
    updateSelectizeInput(session, "facility_filter", 
                        choices = facility_choices,
                        selected = NULL)
    updateSelectizeInput(session, "process_facility_filter", 
                        choices = facility_choices,
                        selected = NULL)
    
    # Update priority filter - start empty
    updateSelectizeInput(session, "priority_filter", 
                        choices = priority_choices,
                        selected = NULL)
    
    # Update zone filter
    updateRadioButtons(session, "zone_filter", 
                       choices = zone_display,
                       selected = zone_choices[1])
    
    # Update process zone filter
    updateRadioButtons(session, "process_zone_filter", 
                       choices = zone_display,
                       selected = zone_choices[1])
    
    # Update material filters
    updateSelectizeInput(session, "material_filter", 
                        choices = material_display,
                        selected = "all")
    updateSelectizeInput(session, "process_material_filter", 
                        choices = material_display,
                        selected = "all")
  })
  
##============Synchronization Logic============

  # Synchronize analysis date between tabs
  observeEvent(input$analysis_date, {
    updateDateInput(session, "process_analysis_date", value = input$analysis_date)
  })
  
  observeEvent(input$process_analysis_date, {
    updateDateInput(session, "analysis_date", value = input$process_analysis_date)
  })
  
  # Synchronize larvae threshold between tabs  
  observeEvent(input$larvae_threshold, {
    updateNumericInput(session, "process_larvae_threshold", value = input$larvae_threshold)
  })
  
  observeEvent(input$process_larvae_threshold, {
    updateNumericInput(session, "larvae_threshold", value = input$process_larvae_threshold)
  })
  
  # Synchronize BTI effect days override between tabs
  observeEvent(input$bti_effect_days_override, {
    updateNumericInput(session, "process_bti_effect_days_override", value = input$bti_effect_days_override)
  })
  
  observeEvent(input$process_bti_effect_days_override, {
    updateNumericInput(session, "bti_effect_days_override", value = input$process_bti_effect_days_override)
  })
  
  # Synchronize zone filters between tabs
  observeEvent(input$zone_filter, {
    updateRadioButtons(session, "process_zone_filter", selected = input$zone_filter)
    updateRadioButtons(session, "hist_zone_filter", selected = input$zone_filter)
  })
  
  observeEvent(input$process_zone_filter, {
    updateRadioButtons(session, "zone_filter", selected = input$process_zone_filter)
    updateRadioButtons(session, "hist_zone_filter", selected = input$process_zone_filter)
  })
  
  observeEvent(input$hist_zone_filter, {
    updateRadioButtons(session, "zone_filter", selected = input$hist_zone_filter)
    updateRadioButtons(session, "process_zone_filter", selected = input$hist_zone_filter)
  })
  
  # Synchronize priority filters between Status and Historical tabs (Process doesn't have priority filter)
  observeEvent(input$priority_filter, {
    updateSelectizeInput(session, "hist_priority_filter", selected = input$priority_filter)
  })
  
  observeEvent(input$hist_priority_filter, {
    updateSelectizeInput(session, "priority_filter", selected = input$hist_priority_filter)
  })
  
  # Synchronize material filters between Status and Process tabs
  observeEvent(input$material_filter, {
    updateSelectizeInput(session, "process_material_filter", selected = input$material_filter)
  })
  
  observeEvent(input$process_material_filter, {
    updateSelectizeInput(session, "material_filter", selected = input$process_material_filter)
  })
  
  # Quick filter buttons for Status Overview tab
  observeEvent(input$btn_prehatch, {
    prehatch_mats <- get_material_choices(include_all = FALSE, filter_type = "prehatch")
    if (length(prehatch_mats) > 0) {
      updateSelectizeInput(session, "material_filter", selected = prehatch_mats)
    }
  })
  
  observeEvent(input$btn_bti, {
    bti_mats <- get_material_choices(include_all = FALSE, filter_type = "bti")
    if (length(bti_mats) > 0) {
      updateSelectizeInput(session, "material_filter", selected = bti_mats)
    }
  })
  
  # Quick filter buttons for Treatment Process tab
  observeEvent(input$btn_process_prehatch, {
    prehatch_mats <- get_material_choices(include_all = FALSE, filter_type = "prehatch")
    if (length(prehatch_mats) > 0) {
      updateSelectizeInput(session, "process_material_filter", selected = prehatch_mats)
    }
  })
  
  observeEvent(input$btn_process_bti, {
    bti_mats <- get_material_choices(include_all = FALSE, filter_type = "bti")
    if (length(bti_mats) > 0) {
      updateSelectizeInput(session, "process_material_filter", selected = bti_mats)
    }
  })
  
  # Synchronize facility filters between Status and Process tabs
  observeEvent(input$facility_filter, {
    updateSelectizeInput(session, "process_facility_filter", selected = input$facility_filter)
    updateSelectizeInput(session, "hist_facility_filter", selected = input$facility_filter)
  })
  
  observeEvent(input$process_facility_filter, {
    updateSelectizeInput(session, "facility_filter", selected = input$process_facility_filter)
    updateSelectizeInput(session, "hist_facility_filter", selected = input$process_facility_filter)
  })
  
  observeEvent(input$hist_facility_filter, {
    updateSelectizeInput(session, "facility_filter", selected = input$hist_facility_filter)
    updateSelectizeInput(session, "process_facility_filter", selected = input$hist_facility_filter)
  })
  
  # ============ AIR SITE STATUS TAB LOGIC ============
  # Reactive data - only loads when refresh button is clicked
  air_sites_data <- eventReactive(input$refresh_data, {
    withProgress(message = "Loading air sites data...", value = 0.5, {
      get_air_sites_data(
        analysis_date = input$analysis_date,
        facility_filter = input$facility_filter,
        priority_filter = input$priority_filter,
        zone_filter = input$zone_filter,
        larvae_threshold = input$larvae_threshold,
        bti_effect_days_override = input$bti_effect_days_override
      )
    })
  })
  
  # Filter data by status and material if needed
  filtered_data <- reactive({
    if (input$refresh_data == 0) return(data.frame())
    
    data <- air_sites_data()
    if (nrow(data) == 0) return(data)
    
    # Apply status filter
    if (input$status_filter != "all") {
      data <- data[data$site_status == input$status_filter, ]
    }
    
    # Apply material filter for active treatments
    if (!is.null(input$material_filter) && length(input$material_filter) > 0 && !"all" %in% input$material_filter) {
      # Filter active treatments by material using matcode, keep all other statuses
      active_treatments <- data[data$site_status == "Active Treatment" & 
                               (!is.na(data$matcode) & 
                                data$matcode %in% input$material_filter), ]
      other_statuses <- data[data$site_status != "Active Treatment", ]
      data <- rbind(active_treatments, other_statuses)
    }
    
    return(data)
  })
  
  # Helper function to calculate metric (sites or acres)
  calc_metric <- function(data, metric_type = "sites") {
    if (nrow(data) == 0) return(0)
    if (metric_type == "acres") {
      return(round(sum(data$acres, na.rm = TRUE), 1))
    } else {
      return(nrow(data))
    }
  }
  
  # Helper function to format metric value
  format_metric <- function(value, metric_type = "sites") {
    if (metric_type == "acres") {
      return(paste0(format(value, big.mark = ","), " ac"))
    } else {
      return(format(value, big.mark = ","))
    }
  }
  
  # Value boxes using stat_box_helper for theme colors
  output$total_air_sites <- renderUI({
    data <- filtered_data()
    metric_val <- calc_metric(data, input$metric_type)
    label <- if (input$metric_type == "acres") "Total Air Site Acres" else "Total Air Sites"
    
    # Use completed status color for total (informational, not status-specific)
    colors <- get_status_colors(theme = input$color_theme)
    create_stat_box(
      value = format_metric(metric_val, input$metric_type),
      title = label,
      bg_color = colors[["completed"]],
      text_color = "#ffffff",
      icon = icon("helicopter")
    )
  })
  
  output$sites_unknown <- renderUI({
    data <- filtered_data()
    data_subset <- data[data$site_status == "Unknown", ]
    metric_val <- calc_metric(data_subset, input$metric_type)
    label <- if (input$metric_type == "acres") "Not Insp Acres (in last 7 days)" else "Not Insp (in last 7 days)"
    create_status_stat_box(
      value = format_metric(metric_val, input$metric_type),
      title = label,
      status = "unknown",
      icon = icon("question-circle"),
      theme = input$color_theme
    )
  })
  
  output$sites_inspected <- renderUI({
    data <- filtered_data()
    data_subset <- data[data$site_status == "Inspected", ]
    metric_val <- calc_metric(data_subset, input$metric_type)
    label <- if (input$metric_type == "acres") "Insp Acres (under threshold)" else "Insp (under threshold, last 7 days)"
    create_status_stat_box(
      value = format_metric(metric_val, input$metric_type),
      title = label,
      status = "completed",
      icon = icon("magnifying-glass"),
      theme = input$color_theme
    )
  })
  
  output$sites_in_lab <- renderUI({
    data <- filtered_data()
    data_subset <- data[data$site_status == "Needs ID", ]
    metric_val <- calc_metric(data_subset, input$metric_type)
    label <- if (input$metric_type == "acres") "Needs ID Acres" else "Needs ID"
    create_status_stat_box(
      value = format_metric(metric_val, input$metric_type),
      title = label,
      status = "in_lab",
      icon = icon("microscope"),
      theme = input$color_theme
    )
  })

  output$sites_needs_treatment <- renderUI({
    data <- filtered_data()
    data_subset <- data[data$site_status == "Needs Treatment", ]
    metric_val <- calc_metric(data_subset, input$metric_type)
    label <- if (input$metric_type == "acres") "Needs Treatment Acres" else "Needs Treatment"
    create_status_stat_box(
      value = format_metric(metric_val, input$metric_type),
      title = label,
      status = "needs_treatment",
      icon = icon("exclamation-triangle"),
      theme = input$color_theme
    )
  })

  output$sites_active_treatment <- renderUI({
    data <- filtered_data()
    data_subset <- data[data$site_status == "Active Treatment", ]
    metric_val <- calc_metric(data_subset, input$metric_type)
    label <- if (input$metric_type == "acres") "Active Treatment Acres" else "Active Treatment"
    create_status_stat_box(
      value = format_metric(metric_val, input$metric_type),
      title = label,
      status = "active",
      icon = icon("check-circle"),
      theme = input$color_theme
    )
  })
  
  
  # Site Map
  output$status_map <- renderLeaflet({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(leaflet() %>% 
        addTiles() %>%
        setView(lng = -93.2, lat = 44.9, zoom = 10))
    }
    
    create_site_map(data, theme = current_theme())
  })
  
  # Status Chart
  output$status_chart <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(plot_ly() %>%
        add_annotations(
          text = "No data available",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE
        ))
    }
    
    # Create status summary chart (sites or acres)
    if (input$metric_type == "acres") {
      status_summary <- data %>%
        group_by(site_status) %>%
        summarise(value = sum(acres, na.rm = TRUE), .groups = 'drop')
      y_label <- "Acres"
    } else {
      status_summary <- data %>%
        group_by(site_status) %>%
        summarise(value = n(), .groups = 'drop')
      y_label <- "Number of Sites"
    }
    
    # Map internal status names to display labels
    status_display_map <- c(
      "Unknown" = "Not Insp",
      "Inspected" = "Insp",
      "Needs ID" = "Needs ID",
      "Needs Treatment" = "Needs Treatment",
      "Active Treatment" = "Active Treatment"
    )
    status_summary$display_status <- status_display_map[status_summary$site_status]
    
    # Define desired order
    status_order <- c("Not Insp", "Insp", "Needs ID", "Needs Treatment", "Active Treatment")
    status_summary$display_status <- factor(status_summary$display_status, levels = status_order)
    
    # Get colors from db_helpers to match the map - use reactive theme
    status_color_map <- get_status_color_map(theme = current_theme())
    
    # Map colors to each status using the original site_status value
    status_summary$color <- sapply(status_summary$site_status, function(s) {
      color <- as.character(status_color_map[[s]])
      if (is.null(color) || is.na(color) || color == "" || color == "NA") {
        warning(paste("Missing color for status:", s, "- using gray"))
        return("#808080")
      }
      return(color)
    })
    
    plot_ly(status_summary, 
            x = ~display_status, 
            y = ~value,
            type = 'bar',
            marker = list(color = ~color)) %>%
      layout(
        title = list(text = "Site Status Distribution", font = list(size = 20)),
        xaxis = list(title = list(text = "Status", font = list(size = 18)), tickfont = list(size = 16)),
        yaxis = list(title = list(text = y_label, font = list(size = 18)), tickfont = list(size = 16)),
        showlegend = FALSE
      )
  })
  
  # Sites Table
  output$site_details_table <- DT::renderDataTable({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    }
    
    # Use create_site_details_panel function (returns formatted datatable)
    create_site_details_panel(data)
  })
  
  # ============ TREATMENT PROCESS TAB LOGIC ============
  
  # Reactive data for process tab
  process_data <- eventReactive(input$refresh_process_data, {
    data <- get_air_sites_data(
      analysis_date = input$process_analysis_date,
      facility_filter = input$process_facility_filter,
      priority_filter = NULL,  # Include all priorities for process tracking
      zone_filter = input$process_zone_filter,
      larvae_threshold = input$process_larvae_threshold,
      bti_effect_days_override = input$process_bti_effect_days_override
    )
    
    # Apply material filter for active treatments
    if (!is.null(input$process_material_filter) && length(input$process_material_filter) > 0 && !"all" %in% input$process_material_filter) {
      # Filter active treatments by material using matcode, keep all other statuses
      active_treatments <- data[data$site_status == "Active Treatment" & 
                               (!is.na(data$matcode) & 
                                data$matcode %in% input$process_material_filter), ]
      other_statuses <- data[data$site_status != "Active Treatment", ]
      data <- rbind(active_treatments, other_statuses)
    }
    
    return(data)
  })
  
  # Filtered data for flow chart (status filter applied)
  process_chart_data <- reactive({
    data <- process_data()
    if (input$refresh_process_data == 0 || nrow(data) == 0) return(data.frame())
    
    # Apply status filter for chart display
    if (!is.null(input$process_status_filter) && length(input$process_status_filter) > 0) {
      data <- data[data$site_status %in% input$process_status_filter, ]
    }
    
    return(data)
  })
  
  # Process metrics value boxes
  output$sites_receiving_treatment <- renderUI({
    data <- process_data()
    metric_type <- input$process_metric_type
    subtitle_text <- if (metric_type == "acres") "Active Treatment (Acres)" else "Active Treatments"
    if (input$refresh_process_data == 0) {
      colors <- get_status_colors(theme = input$color_theme)
      return(create_stat_box(
        value = "0",
        title = subtitle_text,
        bg_color = colors[["active"]],
        text_color = "#ffffff",
        icon = icon("check-circle")
      ))
    }
    
    metrics <- create_treatment_efficiency_metrics(data, metric_type = metric_type)
    create_status_stat_box(
      value = metrics$sites_receiving_treatment,
      title = subtitle_text,
      status = "active",
      icon = icon("check-circle"),
      theme = input$color_theme
    )
  })
  
  output$treatment_efficiency <- renderUI({
    data <- process_data()
    metric_type <- input$process_metric_type
    if (input$refresh_process_data == 0) {
      colors <- get_status_colors(theme = input$color_theme)
      return(create_stat_box(
        value = "0%",
        title = "Treatment completion",
        bg_color = colors[["completed"]],
        text_color = "#ffffff",
        icon = icon("percent")
      ))
    }
    
    metrics <- create_treatment_efficiency_metrics(data, metric_type = metric_type)
    create_status_stat_box(
      value = metrics$treatment_efficiency,
      title = "Treatment completion",
      status = "completed",
      icon = icon("percent"),
      theme = input$color_theme
    )
  })
  
  output$treatment_rate <- renderUI({
    data <- process_data()
    metric_type <- input$process_metric_type
    if (input$refresh_process_data == 0) {
      colors <- get_status_colors(theme = input$color_theme)
      return(create_stat_box(
        value = "0%",
        title = "% Need Treatment",
        bg_color = colors[["needs_treatment"]],
        text_color = "#ffffff",
        icon = icon("exclamation-triangle")
      ))
    }
    
    metrics <- create_treatment_efficiency_metrics(data, metric_type = metric_type)
    create_status_stat_box(
      value = metrics$treatment_rate,
      title = "% Need Treatment",
      status = "needs_treatment",
      icon = icon("exclamation-triangle"),
      theme = input$color_theme
    )
  })
  
  output$inspection_coverage <- renderUI({
    data <- process_data()
    metric_type <- input$process_metric_type
    if (input$refresh_process_data == 0) {
      colors <- get_status_colors(theme = input$color_theme)
      return(create_stat_box(
        value = "0%",
        title = "Inspection Coverage",
        bg_color = colors[["in_lab"]],
        text_color = "#ffffff",
        icon = icon("search")
      ))
    }
    
    metrics <- create_treatment_efficiency_metrics(data, metric_type = metric_type)
    create_status_stat_box(
      value = metrics$inspection_coverage,
      title = "Inspection Coverage",
      status = "in_lab",
      icon = icon("search"),
      theme = input$color_theme
    )
  })
  
  # Lab processing metrics value boxes
  output$total_inspected_samples <- renderUI({
    data <- process_data()
    if (input$refresh_process_data == 0) {
      colors <- get_status_colors(theme = input$color_theme)
      return(create_stat_box(
        value = "0",
        title = "Insp Samples",
        bg_color = colors[["planned"]],
        text_color = "#ffffff",
        icon = icon("vial")
      ))
    }
    
    lab_metrics <- analyze_lab_processing_metrics(data)
    create_status_stat_box(
      value = lab_metrics$total_inspected_with_samples,
      title = "Insp Samples",
      status = "planned",
      icon = icon("vial"),
      theme = input$color_theme
    )
  })
  
  output$red_bug_detection_rate <- renderUI({
    data <- process_data()
    if (input$refresh_process_data == 0) {
      colors <- get_status_colors(theme = input$color_theme)
      return(create_stat_box(
        value = "0%",
        title = "Red Bug Detection",
        bg_color = colors[["needs_treatment"]],
        text_color = "#ffffff",
        icon = icon("bug")
      ))
    }
    
    lab_metrics <- analyze_lab_processing_metrics(data)
    create_status_stat_box(
      value = lab_metrics$red_bug_detection_rate,
      title = "Red Bug Detection",
      status = "needs_treatment",
      icon = icon("bug"),
      theme = input$color_theme
    )
  })
  
  
  # Treatment flow chart
  output$treatment_flow_chart <- renderPlotly({
    data <- process_chart_data()  # Use filtered data for chart
    metric_type <- input$process_metric_type
    if (input$refresh_process_data == 0 || nrow(data) == 0) {
      return(plot_ly() %>%
        add_annotations(
          text = "No data available - Click 'Refresh Data'",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE
        ))
    }
    
    create_treatment_flow_chart(data, metric_type = metric_type, theme = current_theme())
  })
  
  # Process summary table
  output$process_summary_table <- DT::renderDataTable({
    data <- process_data()
    metric_type <- input$process_metric_type
    if (input$refresh_process_data == 0 || nrow(data) == 0) {
      return(DT::datatable(data.frame(), options = list(pageLength = 10)))
    }
    
    summary_data <- create_treatment_process_summary(data, metric_type = metric_type)
    
    DT::datatable(
      summary_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Treatment Rate",
        backgroundColor = DT::styleInterval(
          cuts = c(50, 75),
          values = c("#f8d7da", "#fff3cd", "#d4edda")
        )
      )
  })
  
  # Facility process details table
  output$facility_process_table <- DT::renderDataTable({
    data <- process_data()
    if (input$refresh_process_data == 0 || nrow(data) == 0) {
      return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    }
    
    # Show all sites with their status for process tracking
    process_details <- data %>%
      select(sitecode, facility, priority, site_status, 
             last_inspection_date_display, last_larvae_count,
             last_treatment_date_display, last_treatment_material) %>%
      arrange(facility, site_status, sitecode)
    
    colnames(process_details) <- c(
      "Site Code", "Facility", "Priority", "Status", 
      "Last Inspection", "Larvae Count", "Last Treatment", "Material"
    )
    
    DT::datatable(
      process_details,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(1, 2, 3, 5))
        )
      ),
      rownames = FALSE,
      filter = 'top'
    ) %>%
      DT::formatStyle(
        "Status",
        backgroundColor = DT::styleEqual(
          c("Active Treatment", "Needs Treatment", "Inspected", "Needs ID", "Unknown"),
          c("#d4edda", "#f8d7da", "#d1ecf1", "#fff3cd", "#f8f9fa")
        )
      )
  })
  
  # ============ HISTORICAL ANALYSIS TAB LOGIC ============
  
  # Load filter choices for historical tab - ONCE only
  observe({
    tryCatch({
      # Load facility choices for historical tab
      facility_lookup <- get_facility_lookup()
      if (nrow(facility_lookup) > 0) {
        facility_choices_hist <- c("All Facilities" = "all", setNames(facility_lookup$short_name, facility_lookup$full_name))
        updateSelectizeInput(session, "hist_facility_filter", 
                            choices = facility_choices_hist,
                            selected = NULL)
      }
      
      # Load priority choices for historical tab
      priority_choices_hist <- get_priority_choices(include_all = FALSE)
      priority_choices_hist <- priority_choices_hist[names(priority_choices_hist) != "All Priorities"]
      updateSelectizeInput(session, "hist_priority_filter", 
                          choices = priority_choices_hist,
                          selected = priority_choices_hist)
      
      # Load zone choices for historical tab
      zone_choices_hist <- get_available_zones()
      updateRadioButtons(session, "hist_zone_filter", 
                              choices = setNames(zone_choices_hist, zone_choices_hist),
                              selected = zone_choices_hist[1])
      
    }, error = function(e) {
      warning(paste("Error loading historical filter choices:", e$message))
    })
  })
  
  # Synchronize larvae threshold between tabs
  observeEvent(input$larvae_threshold, {
    updateNumericInput(session, "hist_larvae_threshold", value = input$larvae_threshold)
  })
  
  observeEvent(input$hist_larvae_threshold, {
    updateNumericInput(session, "larvae_threshold", value = input$hist_larvae_threshold)
  })
  
  # Synchronize metric type between tabs (Status, Pipeline Snapshot, Historical Analysis)
  observeEvent(input$metric_type, {
    updateRadioButtons(session, "hist_metric_type", selected = input$metric_type)
    updateRadioButtons(session, "process_metric_type", selected = input$metric_type)
  })
  
  observeEvent(input$hist_metric_type, {
    updateRadioButtons(session, "metric_type", selected = input$hist_metric_type)
    updateRadioButtons(session, "process_metric_type", selected = input$hist_metric_type)
  })
  
  observeEvent(input$process_metric_type, {
    updateRadioButtons(session, "metric_type", selected = input$process_metric_type)
    updateRadioButtons(session, "hist_metric_type", selected = input$process_metric_type)
  })
  
  # Reactive data for historical analysis - OPTIMIZED: Single comprehensive query
  comprehensive_historical_data <- eventReactive(input$refresh_historical_data, {
    withProgress(message = "Loading historical data...", value = 0.5, {
      cat("Historical refresh button clicked - Getting comprehensive data\n")
      get_comprehensive_historical_data(
        start_date = input$hist_start_date,
        end_date = input$hist_end_date,
        facility_filter = input$hist_facility_filter,
        priority_filter = input$hist_priority_filter,
        zone_filter = input$hist_zone_filter,
        larvae_threshold = input$hist_larvae_threshold
      )
    })
  })
  
  # Extract inspection summary from comprehensive data
  historical_data <- reactive({
    comprehensive_data <- comprehensive_historical_data()
    return(comprehensive_data$inspection_summary)
  })
  
  # Extract treatment volume data from comprehensive data
  treatment_volume_data <- reactive({
    comprehensive_data <- comprehensive_historical_data()
    return(comprehensive_data$treatment_volumes)
  })
  
  # Historical value boxes
  output$hist_total_sites <- renderUI({
    data <- historical_data()
    if (input$refresh_historical_data == 0) {
      colors <- get_status_colors(theme = input$color_theme)
      return(create_stat_box(
        value = "0",
        title = "Total Sites",
        bg_color = colors[["completed"]],
        text_color = "#ffffff",
        icon = icon("map-marker")
      ))
    }
    
    create_stat_box(
      value = nrow(data),
      title = "Total Sites",
      bg_color = get_status_colors(theme = input$color_theme)[["completed"]],
      text_color = "#ffffff",
      icon = icon("map-marker")
    )
  })
  
  output$hist_total_inspections <- renderUI({
    data <- historical_data()
    if (input$refresh_historical_data == 0) {
      colors <- get_status_colors(theme = input$color_theme)
      return(create_stat_box(
        value = "0",
        title = "Total Inspections",
        bg_color = colors[["completed"]],
        text_color = "#ffffff",
        icon = icon("search")
      ))
    }
    
    total_inspections <- sum(data$total_inspections, na.rm = TRUE)
    create_stat_box(
      value = format(total_inspections, big.mark = ","),
      title = "Total Inspections",
      bg_color = get_status_colors(theme = input$color_theme)[["completed"]],
      text_color = "#ffffff",
      icon = icon("search")
    )
  })
  
  output$hist_total_red_bug_inspections <- renderUI({
    data <- historical_data()
    if (input$refresh_historical_data == 0) {
      colors <- get_status_colors(theme = input$color_theme)
      return(create_stat_box(
        value = "0",
        title = "Red Bug Inspections",
        bg_color = colors[["needs_treatment"]],
        text_color = "#ffffff",
        icon = icon("bug")
      ))
    }
    
    red_bug_inspections <- sum(data$red_bug_inspections, na.rm = TRUE)
    create_stat_box(
      value = format(red_bug_inspections, big.mark = ","),
      title = "Red Bug Inspections",
      bg_color = get_status_colors(theme = input$color_theme)[["needs_treatment"]],
      text_color = "#ffffff",
      icon = icon("bug")
    )
  })
  
  output$hist_overall_red_bug_ratio <- renderUI({
    data <- historical_data()
    if (input$refresh_historical_data == 0) {
      colors <- get_status_colors(theme = input$color_theme)
      return(create_stat_box(
        value = "0%",
        title = "% Inspections with Red Bugs",
        bg_color = colors[["planned"]],
        text_color = "#ffffff",
        icon = icon("percent")
      ))
    }
    
    total_inspections <- sum(data$total_inspections, na.rm = TRUE)
    total_red_bug <- sum(data$red_bug_inspections, na.rm = TRUE)
    overall_ratio <- if (total_inspections > 0) round((total_red_bug / total_inspections) * 100, 1) else 0
    
    create_stat_box(
      value = paste0(overall_ratio, "%"),
      title = "% Inspections with Red Bugs",
      bg_color = get_status_colors(theme = input$color_theme)[["planned"]],
      text_color = "#ffffff",
      icon = icon("percent")
    )
  })
  
  # Treatment volume value boxes
  output$hist_total_treatments <- renderUI({
    data <- treatment_volume_data()
    if (input$refresh_historical_data == 0) {
      colors <- get_status_colors(theme = input$color_theme)
      return(create_stat_box(
        value = "0",
        title = "Total Treatments",
        bg_color = colors[["active"]],
        text_color = "#ffffff",
        icon = icon("spray-can")
      ))
    }
    
    treatments <- sum(data$total_operations[data$operation_type == 'treatment'], na.rm = TRUE)
    create_stat_box(
      value = format(treatments, big.mark = ","),
      title = "Total Treatments",
      bg_color = get_status_colors(theme = input$color_theme)[["active"]],
      text_color = "#ffffff",
      icon = icon("spray-can")
    )
  })
  
  output$hist_total_treatment_acres <- renderUI({
    data <- treatment_volume_data()
    if (input$refresh_historical_data == 0) {
      colors <- get_status_colors(theme = input$color_theme)
      return(create_stat_box(
        value = "0",
        title = "Treatment Acres",
        bg_color = colors[["active"]],
        text_color = "#ffffff",
        icon = icon("area-chart")
      ))
    }
    
    acres <- sum(data$total_acres[data$operation_type == 'treatment'], na.rm = TRUE)
    create_stat_box(
      value = format(round(acres, 1), big.mark = ","),
      title = "Treatment Acres",
      bg_color = get_status_colors(theme = input$color_theme)[["active"]],
      text_color = "#ffffff",
      icon = icon("area-chart")
    )
  })
  
  output$hist_total_inspection_acres <- renderUI({
    data <- treatment_volume_data()
    if (input$refresh_historical_data == 0) {
      colors <- get_status_colors(theme = input$color_theme)
      return(create_stat_box(
        value = "0",
        title = "Inspection Acres",
        bg_color = colors[["completed"]],
        text_color = "#ffffff",
        icon = icon("search")
      ))
    }
    
    acres <- sum(data$total_acres[data$operation_type == 'inspection'], na.rm = TRUE)
    create_stat_box(
      value = format(round(acres, 1), big.mark = ","),
      title = "Inspection Acres",
      bg_color = get_status_colors(theme = input$color_theme)[["completed"]],
      text_color = "#ffffff",
      icon = icon("search")
    )
  })
  
  # Historical inspection table
  output$historical_inspection_table <- DT::renderDataTable({
    data <- historical_data()
    if (input$refresh_historical_data == 0 || nrow(data) == 0) {
      return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    }
    
    # Format the table for display
    display_data <- data
    colnames(display_data) <- c(
      "Site Code", "Facility", "Priority", "Zone", "Acres", 
      "Total Inspections", "Red Bug Inspections", "Red Bug Ratio (%)", "Years Active"
    )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(1, 2, 3, 5, 6, 7)),
          list(className = 'dt-right', targets = 4)
        ),
        order = list(list(7, 'desc'))  # Sort by Red Bug Ratio descending
      ),
      rownames = FALSE,
      filter = 'top'
    ) %>%
      DT::formatRound('Acres', 1) %>%
      DT::formatStyle(
        "Red Bug Ratio (%)",
        backgroundColor = DT::styleInterval(
          cuts = c(10, 25, 50),
          values = c("#d4edda", "#fff3cd", "#ffeeba", "#f8d7da")
        )
      ) %>%
      DT::formatStyle(
        "Total Inspections",
        backgroundColor = DT::styleInterval(
          cuts = c(5, 20, 50),
          values = c("#f8f9fa", "#e2e3e5", "#d1ecf1", "#b8daff")
        )
      )
  })
  
  # Treatment volume chart
  output$treatment_volume_chart <- renderPlotly({
    data <- treatment_volume_data()
    if (input$refresh_historical_data == 0 || nrow(data) == 0) {
      return(plot_ly() %>%
        add_annotations(
          text = "No data available - Click 'Refresh Historical Data'",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE
        ))
    }
    
    create_treatment_volume_chart(data, input$volume_time_period, chart_type = input$hist_chart_type, 
                                 metric_type = input$hist_metric_type, theme = current_theme())
  })
  
  # Red bug trend chart
  output$red_bug_trend_chart <- renderPlotly({
    data <- historical_data()
    if (input$refresh_historical_data == 0 || nrow(data) == 0) {
      return(plot_ly() %>%
        add_annotations(
          text = "No historical data",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE
        ))
    }
    
    # Create simple red bug ratio trend by facility
    trend_data <- data %>%
      mutate(
        # Extract primary year from years_active string
        primary_year = sapply(years_active, function(x) {
          if (grepl("-", x)) {
            # Range format, take start year
            as.numeric(sub("-.*", "", x))
          } else if (grepl(",", x)) {
            # List format, take first year
            as.numeric(sub(",.*", "", x))
          } else {
            # Single year or other format
            as.numeric(gsub("[^0-9]", "", x))
          }
        })
      ) %>%
      filter(!is.na(primary_year) & as.Date(paste0(primary_year, "-01-01")) >= input$hist_start_date) %>%
      group_by(facility, primary_year) %>%
      summarise(
        sites = n(),
        avg_red_bug_ratio = round(mean(red_bug_ratio, na.rm = TRUE), 1),
        .groups = 'drop'
      ) %>%
      filter(sites >= 3)  # Only show facilities with at least 3 sites
    
    if (nrow(trend_data) == 0) {
      return(plot_ly() %>%
        add_annotations(
          text = "Insufficient data for trends",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE
        ))
    }
    
    # Get facility colors from theme
    facility_colors <- get_facility_base_colors(theme = current_theme())
    
    plot_ly(trend_data, x = ~primary_year, y = ~avg_red_bug_ratio, color = ~facility,
            colors = unname(facility_colors),
            type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = list(text = "Red Bug Detection Trends", font = list(size = 20)),
        xaxis = list(title = list(text = "Year", font = list(size = 18)), tickfont = list(size = 16)),
        yaxis = list(title = list(text = "Avg Red Bug Ratio (%)", font = list(size = 18)), tickfont = list(size = 16)),
        legend = list(font = list(size = 16)),
        hovermode = 'x unified'
      )
  })
  
  # Treatment volume table
  output$treatment_volume_table <- DT::renderDataTable({
    data <- treatment_volume_data()
    if (input$refresh_historical_data == 0 || nrow(data) == 0) {
      return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    }
    
    # Create summary by year and facility
    summary_data <- data %>%
      group_by(facility, priority, year, operation_type) %>%
      summarise(
        operations = sum(total_operations, na.rm = TRUE),
        acres = round(sum(total_acres, na.rm = TRUE), 1),
        .groups = 'drop'
      ) %>%
      pivot_wider(
        names_from = operation_type,
        values_from = c(operations, acres),
        values_fill = 0
      ) %>%
      select(
        Facility = facility,
        Priority = priority,
        Year = year,
        Treatments = operations_treatment,
        `Treatment Acres` = acres_treatment,
        Inspections = operations_inspection,
        `Inspection Acres` = acres_inspection
      ) %>%
      arrange(desc(Year), Facility, Priority)
    
    DT::datatable(
      summary_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(0, 1, 2, 3, 5)),
          list(className = 'dt-right', targets = c(4, 6))
        )
      ),
      rownames = FALSE,
      filter = 'top'
    ) %>%
      DT::formatStyle(
        "Treatment Acres",
        backgroundColor = DT::styleInterval(
          cuts = c(50, 200, 500),
          values = c("#f8f9fa", "#d1ecf1", "#b8daff", "#7fb3d3")
        )
      )
  })
  
  # Download handlers for CSV exports
  output$download_site_details <- downloadHandler(
    filename = function() {
      paste("site_details_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      tryCatch({
        # Get the same data that the site details table uses
        site_data <- filtered_data()
        
        # Debug: Check if we have data
        cat("Debug: Site data rows:", nrow(site_data), "\n")
        cat("Debug: File path:", file, "\n")
        
        if (is.null(site_data) || nrow(site_data) == 0) {
          # Create empty file with message
          write.csv(data.frame(Message = "No data available for selected filters"), 
                    file, row.names = FALSE)
          cat("Debug: Created empty file due to no data\n")
        } else {
          # Use the shared CSV export function
          result <- export_csv_safe(site_data, file, clean_data = TRUE)
          
          cat("Debug: Export result - Success:", result$success, "\n")
          cat("Debug: Export message:", result$message, "\n")
          
          # If export failed, try simple write.csv as fallback
          if (!result$success) {
            cat("Debug: Falling back to simple write.csv\n")
            write.csv(site_data, file, row.names = FALSE, na = "")
          }
        }
        
        # Verify file was created
        if (file.exists(file)) {
          cat("Debug: File successfully created at:", file, "\n")
          cat("Debug: File size:", file.size(file), "bytes\n")
        } else {
          cat("Debug: ERROR - File was not created!\n")
        }
        
      }, error = function(e) {
        cat("Debug: Download error:", e$message, "\n")
        # Create error file
        write.csv(data.frame(Error = paste("Download failed:", e$message)), 
                  file, row.names = FALSE)
      })
    }
  )
  
  output$download_process_summary <- downloadHandler(
    filename = function() {
      paste("process_summary_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      tryCatch({
        # Get the same data that the process summary table uses
        data <- process_data()
        
        if (input$refresh_process_data == 0 || nrow(data) == 0) {
          write.csv(data.frame(Message = "No process data available"), file, row.names = FALSE)
          return()
        }
        
        summary_data <- create_treatment_process_summary(data)
        
        # Use the shared CSV export function
        result <- export_csv_safe(summary_data, file, clean_data = TRUE)
        
        if (!result$success) {
          write.csv(summary_data, file, row.names = FALSE, na = "")
        }
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )
  
  output$download_facility_process <- downloadHandler(
    filename = function() {
      paste("facility_process_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      tryCatch({
        # Get the same data that the facility process table uses
        data <- process_data()
        
        if (input$refresh_process_data == 0 || nrow(data) == 0) {
          write.csv(data.frame(Message = "No process data available"), file, row.names = FALSE)
          return()
        }
        
        # Create the same process details data as the table
        process_details <- data %>%
          select(sitecode, facility, priority, site_status, 
                 last_inspection_date_display, last_larvae_count,
                 last_treatment_date_display, last_treatment_material) %>%
          arrange(facility, site_status, sitecode)
        
        colnames(process_details) <- c(
          "Site Code", "Facility", "Priority", "Status", 
          "Last Inspection", "Larvae Count", "Last Treatment", "Material"
        )
        
        # Use the shared CSV export function
        result <- export_csv_safe(process_details, file, clean_data = TRUE)
        
        if (!result$success) {
          write.csv(process_details, file, row.names = FALSE, na = "")
        }
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )
  
  output$download_inspection_history <- downloadHandler(
    filename = function() {
      paste("inspection_history_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      tryCatch({
        # Get the same data that the historical inspection table uses
        data <- historical_data()
        
        if (input$refresh_historical_data == 0 || nrow(data) == 0) {
          write.csv(data.frame(Message = "No historical data available"), file, row.names = FALSE)
          return()
        }
        
        # Format the same way as the table
        display_data <- data
        colnames(display_data) <- c(
          "Site Code", "Facility", "Priority", "Zone", "Acres", 
          "Total Inspections", "Red Bug Inspections", "Red Bug Ratio (%)", "Years Active"
        )
        
        # Use the shared CSV export function
        result <- export_csv_safe(display_data, file, clean_data = TRUE)
        
        if (!result$success) {
          write.csv(display_data, file, row.names = FALSE, na = "")
        }
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )
  
  output$download_treatment_volume <- downloadHandler(
    filename = function() {
      paste("treatment_volume_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      tryCatch({
        # Get the same data that the treatment volume table uses
        data <- treatment_volume_data()
        
        if (input$refresh_historical_data == 0 || nrow(data) == 0) {
          write.csv(data.frame(Message = "No treatment volume data available"), file, row.names = FALSE)
          return()
        }
        
        # Create the same summary as the table
        summary_data <- data %>%
          group_by(facility, priority, year, operation_type) %>%
          summarise(
            operations = sum(total_operations, na.rm = TRUE),
            acres = round(sum(total_acres, na.rm = TRUE), 1),
            .groups = 'drop'
          ) %>%
          pivot_wider(
            names_from = operation_type,
            values_from = c(operations, acres),
            values_fill = 0
          ) %>%
          select(
            Facility = facility,
            Priority = priority,
            Year = year,
            Treatments = operations_treatment,
            `Treatment Acres` = acres_treatment,
            Inspections = operations_inspection,
            `Inspection Acres` = acres_inspection
          ) %>%
          arrange(desc(Year), Facility, Priority)
        
        # Use the shared CSV export function
        result <- export_csv_safe(summary_data, file, clean_data = TRUE)
        
        if (!result$success) {
          write.csv(summary_data, file, row.names = FALSE, na = "")
        }
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )
    }

# Run the application
shinyApp(ui = ui, server = server)