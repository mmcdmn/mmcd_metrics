# Simple Air Sites Status App
# Only tracks Active Treatment vs Unknown status based on material effect days
# No rainfall data involved

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(leaflet)
  library(dplyr)
  library(plotly)
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

# Source app-specific functions
# Using working version while debugging enhanced version
source("air_status_functions_working.R")
source("historical_functions.R")

# Alias for compatibility
get_air_sites_data <- get_air_sites_data_working

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Air Work Pipeline - Simple"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Air Site Status", tabName = "status", icon = icon("helicopter")),
      menuItem("Treatment Process", tabName = "process", icon = icon("chart-line")),
      menuItem("Historical Analysis", tabName = "historical", icon = icon("chart-area"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "status",
        fluidRow(
          box(title = "Controls", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(3,
                dateInput("analysis_date", "Analysis Date:",
                  value = Sys.Date(),
                  max = Sys.Date()
                )
              ),
              column(3,
                selectizeInput("facility_filter", "Facilities:",
                  choices = c("Loading..." = "LOADING"),
                  selected = "LOADING",
                  multiple = TRUE
                )
              ),
              column(3,
                selectizeInput("priority_filter", "Priorities:",
                  choices = c("Loading..." = "LOADING"),
                  selected = "LOADING",
                  multiple = TRUE
                )
              ),
              column(3,
                radioButtons("zone_filter", "Zones:",
                  choices = c("Loading..." = "LOADING"),
                  selected = "LOADING"
                )
              )
            ),
            fluidRow(
              column(3,
                numericInput("larvae_threshold", "Larvae Threshold:",
                  value = 2,
                  min = 0,
                  max = 10,
                  step = 1
                )
              ),
              column(3,
                selectInput("status_filter", "Status Filter:",
                  choices = c("All Statuses" = "all",
                             "Unknown" = "Unknown",
                             "Inspected" = "Inspected", 
                             "In Lab" = "In Lab",
                             "Needs Treatment" = "Needs Treatment",
                             "Active Treatment" = "Active Treatment"),
                  selected = "all"
                )
              ),
              column(3,
                selectizeInput("material_filter", "Treatment Materials:",
                  choices = c("Loading..." = "LOADING"),
                  selected = "LOADING",
                  multiple = TRUE
                )
              ),
              column(3,
                numericInput("bti_effect_days_override", "BTI Effect Days Override:",
                  value = NA,
                  min = 1,
                  max = 60,
                  step = 1
                ),
                tags$small(class = "text-muted", "Leave empty to use default from database")
              )
            ),
            fluidRow(
              column(12, style = "text-align: center; padding-top: 10px;",
                actionButton("refresh_data", "Refresh Data", class = "btn-primary btn-lg", 
                           style = "width: 30%;")
              )
            ),
            fluidRow(
              column(12,
                div(style = "padding-top: 10px; text-align: center;",
                  tags$small(
                    tags$i(class = "fa fa-info-circle", style = "color: #17a2b8;"),
                    " Note: 'Needs Inspection' status is currently under review. This dashboard shows the process from inspection to treatment."
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("total_air_sites", width = 2),
          valueBoxOutput("sites_unknown", width = 2),
          valueBoxOutput("sites_inspected", width = 2),
          valueBoxOutput("sites_in_lab", width = 2),
          valueBoxOutput("sites_needs_treatment", width = 2),
          valueBoxOutput("sites_active_treatment", width = 2)
        ),
        
        fluidRow(
          box(title = "Air Site Status Map", status = "primary", solidHeader = TRUE, width = 8,
            leafletOutput("status_map", height = "500px")
          ),
          box(title = "Status Summary", status = "info", solidHeader = TRUE, width = 4,
            plotlyOutput("status_chart", height = "500px")
          )
        ),
        
        fluidRow(
          box(title = "Status Definitions", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
            tags$div(
              tags$h5("Air Site Status Definitions:"),
              tags$ul(
                tags$li(tags$strong("Unknown:"), " Sites that have not been inspected or have no recent inspection data"),
                tags$li(tags$strong("Inspected:"), " Sites inspected with larvae count below threshold (no treatment needed)"),
                tags$li(tags$strong("In Lab:"), " Sites with larvae ≥ threshold, samples sent to lab for red/blue bug identification"),
                tags$li(tags$strong("Needs Treatment:"), " Sites with red bugs found in lab analysis (require treatment)"),
                tags$li(tags$strong("Active Treatment:"), " Sites who recived treatment < effect_days ago acording to material type (see override for BTI)")
              ),
              tags$h5("Treatment Logic:"),
              tags$ul(
                tags$li("Sites with larvae ≥ threshold → lab for species identification"),
                tags$li("Red bugs found → treatment required (species that bite humans)"),
                tags$li("Blue bugs found → no treatment needed (species that don't bite humans)"),
                tags$li("Active treatments last for BTI effect days (default varies by material)")
              )
            )
          )
        ),
        
        fluidRow(
          box(title = "Site Details", status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("site_details_table")
          )
        )
      ),
      
      # Treatment Process Tab
      tabItem(tabName = "process",
        fluidRow(
          box(title = "Process Controls", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(3,
                dateInput("process_analysis_date", "Analysis Date:",
                  value = Sys.Date(),
                  max = Sys.Date()
                )
              ),
              column(3,
                selectizeInput("process_facility_filter", "Facilities:",
                  choices = c("Loading..." = "LOADING"),
                  selected = "LOADING",
                  multiple = TRUE
                )
              ),
              column(3,
                radioButtons("process_zone_filter", "Zones:",
                  choices = c("Loading..." = "LOADING"),
                  selected = "LOADING"
                )
              )
            ),
            fluidRow(
              column(3,
                numericInput("process_larvae_threshold", "Larvae Threshold:",
                  value = 2,
                  min = 0,
                  max = 10,
                  step = 1
                )
              ),
              column(3,
                selectizeInput("process_material_filter", "Treatment Materials:",
                  choices = c("Loading..." = "LOADING"),
                  selected = "LOADING",
                  multiple = TRUE
                )
              )
            ),
            fluidRow(
              column(3,
                numericInput("process_bti_effect_days_override", "BTI Effect Days Override:",
                  value = NA,
                  min = 1,
                  max = 60,
                  step = 1
                ),
                tags$small(class = "text-muted", "Leave empty to use default")
              ),
              column(6,
                checkboxGroupInput("process_status_filter", "Status Filter (for Flow Chart):",
                  choices = c("Unknown" = "Unknown",
                             "Inspected" = "Inspected", 
                             "In Lab" = "In Lab",
                             "Needs Treatment" = "Needs Treatment",
                             "Active Treatment" = "Active Treatment"),
                  selected = c("Unknown", "Inspected", "In Lab", "Needs Treatment", "Active Treatment"),
                  inline = TRUE
                )
              ),
              column(3,
                actionButton("refresh_process_data", "Refresh Data", class = "btn-primary btn-lg", 
                           style = "width: 100%; margin-top: 25px;")
              ),
              column(3,
                div(style = "padding-top: 10px;",
                  tags$small(
                    tags$i(class = "fa fa-info-circle", style = "color: #17a2b8;"),
                    " Status filter only affects the flow chart visualization."
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("sites_receiving_treatment", width = 4),
          valueBoxOutput("treatment_efficiency", width = 4),
          valueBoxOutput("inspection_coverage", width = 4)
        ),
        
        fluidRow(
          valueBoxOutput("total_inspected_samples", width = 4),
          valueBoxOutput("red_bug_detection_rate", width = 4),
          valueBoxOutput("lab_completion_rate", width = 4)
        ),
        
        fluidRow(
          box(title = "Calculation Notes", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
            tags$div(
              tags$h5("Treatment Process Metrics:"),
              tags$ul(
                tags$li(tags$strong("Treatment Efficiency:"), " Active Treatments ÷ (Needs Treatment + Active Treatment) × 100%"),
                tags$li(tags$strong("Inspection Coverage:"), " (Inspected + Needs Treatment + Active Treatment) ÷ Total Sites × 100%")
              ),
              tags$h5("Lab Processing Metrics:"),
              tags$ul(
                tags$li(tags$strong("Total Inspected Samples:"), " Number of inspected sites with completed lab results (timestamp data)"),
                tags$li(tags$strong("Red Bug Detection Rate:"), " (Red Bugs Found ÷ Total Samples) × 100% - Percentage needing treatment"),
                tags$li(tags$strong("Lab Completion Rate:"), " (Completed Samples ÷ All Samples) × 100% - Shows <100% when sites are in lab")
              ),
              tags$p(tags$strong("Note:"), " Total samples = only completed lab samples with timestamps from current analysis date. Pending samples without timestamps are excluded.")
            )
          )
        ),
        
        fluidRow(
          box(title = "Treatment Flow by Facility", status = "primary", solidHeader = TRUE, width = 8,
            plotlyOutput("treatment_flow_chart", height = "400px")
          ),
          box(title = "Process Summary", status = "info", solidHeader = TRUE, width = 4,
            DT::dataTableOutput("process_summary_table")
          )
        ),
        
        fluidRow(
          box(title = "Facility Treatment Process Details", status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("facility_process_table")
          )
        )
      ),
      
      # Historical Analysis Tab
      tabItem(tabName = "historical",
        fluidRow(
          box(title = "Historical Controls", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(3,
                selectizeInput("hist_facility_filter", "Facilities:",
                  choices = c("Loading..." = "LOADING"),
                  selected = "LOADING",
                  multiple = TRUE
                )
              ),
              column(3,
                selectizeInput("hist_priority_filter", "Priorities:",
                  choices = c("Loading..." = "LOADING"),
                  selected = "LOADING",
                  multiple = TRUE
                )
              ),
              column(3,
                radioButtons("hist_zone_filter", "Zones:",
                  choices = c("Loading..." = "LOADING"),
                  selected = "LOADING"
                )
              ),
              column(3,
                numericInput("hist_larvae_threshold", "Larvae Threshold:",
                  value = 2,
                  min = 0,
                  max = 10,
                  step = 1
                )
              )
            ),
            fluidRow(
              column(3,
                radioButtons("hist_time_period", "Time Period:",
                  choices = c("Yearly" = "yearly", 
                              "Weekly" = "weekly"),
                  selected = "yearly",
                  inline = TRUE
                )
              ),
              column(3,
                radioButtons("hist_group_by", "Group by:",
                  choices = c("All MMCD" = "mmcd_all",
                             "Facility" = "facility", 
                             "Priority" = "priority",
                             "Zone" = "zone"),
                  selected = "mmcd_all",
                  inline = TRUE
                )
              ),
              column(3,
                conditionalPanel(
                  condition = "input.hist_time_period == 'weekly'",
                  numericInput("hist_year", "Year:", 
                              value = as.numeric(format(Sys.Date(), "%Y")), 
                              min = as.numeric(format(Sys.Date(), "%Y")) - 10, 
                              max = as.numeric(format(Sys.Date(), "%Y")), 
                              step = 1)
                ),
                conditionalPanel(
                  condition = "input.hist_time_period == 'yearly'",
                  div(
                    numericInput("hist_start_year", "Start Year:", 
                                value = as.numeric(format(Sys.Date(), "%Y")) - 4, 
                                min = as.numeric(format(Sys.Date(), "%Y")) - 10, 
                                max = as.numeric(format(Sys.Date(), "%Y")), 
                                step = 1),
                    numericInput("hist_end_year", "End Year:", 
                                value = as.numeric(format(Sys.Date(), "%Y")), 
                                min = as.numeric(format(Sys.Date(), "%Y")) - 10, 
                                max = as.numeric(format(Sys.Date(), "%Y")), 
                                step = 1)
                  )
                )
              ),
              column(3,
                div(style = "margin-top: 25px;",
                  actionButton("refresh_historical_data", "Refresh Historical Data", class = "btn-primary btn-lg", 
                             style = "width: 100%;")
                )
              )
            ),
            fluidRow(
              column(12,
                div(style = "padding-top: 10px; text-align: center;",
                  tags$small(
                    tags$i(class = "fa fa-info-circle", style = "color: #17a2b8;"),
                    " Red Bug Ratio = Times site had larvae >= threshold ÷ Total inspections for site"
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("hist_total_inspections", width = 3),
          valueBoxOutput("hist_red_bug_inspections", width = 3),
          valueBoxOutput("hist_avg_red_bug_ratio", width = 3),
          valueBoxOutput("hist_sites_with_data", width = 3)
        ),
        
        fluidRow(
          box(title = "Historical Red Bug Ratio Trends", status = "primary", solidHeader = TRUE, width = 8,
            plotlyOutput("historical_chart", height = "500px")
          ),
          box(title = "Red Bug Summary", status = "info", solidHeader = TRUE, width = 4,
            plotlyOutput("hist_summary_chart", height = "500px")
          )
        ),
        
        fluidRow(
          box(title = "Historical Data Details", status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("historical_details_table")
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Initialize filters ONCE on startup - do not make reactive
  # This prevents constant updating and "loading" states
  
  # Load facility choices (static, load once)
  facility_lookup <- get_facility_lookup()
  if (nrow(facility_lookup) > 0) {
    facility_choices <- setNames(facility_lookup$short_name, facility_lookup$full_name)
  } else {
    facility_choices <- c("MMCD" = "MMCD", "SMCD" = "SMCD", "RMCD" = "RMCD")
  }
  
  # Load priority choices (static, load once)
  priority_choices <- get_priority_choices(include_all = FALSE)
  priority_choices <- priority_choices[names(priority_choices) != "All Priorities"]
  
  # Load zone choices (static, load once)
  zone_choices <- get_available_zones()
  zone_display <- setNames(zone_choices, zone_choices)
  
  # Load treatment material choices (static, load once)
  material_choices <- get_treatment_materials(include_all = TRUE)
  if (length(material_choices) == 0) {
    material_choices <- c("All")
  }
  material_display <- setNames(material_choices, material_choices)
  
  # Update all filter inputs ONCE on startup
  observe({
    # Update facility filters
    updateSelectizeInput(session, "facility_filter", 
                        choices = facility_choices,
                        selected = unname(facility_choices))
    updateSelectizeInput(session, "process_facility_filter", 
                        choices = facility_choices,
                        selected = unname(facility_choices))
    
    # Update priority filter
    updateSelectizeInput(session, "priority_filter", 
                        choices = priority_choices,
                        selected = unname(priority_choices))
    
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
                        selected = "All")
    updateSelectizeInput(session, "process_material_filter", 
                        choices = material_display,
                        selected = "All")
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
    get_air_sites_data_working(
      analysis_date = input$analysis_date,
      facility_filter = input$facility_filter,
      priority_filter = input$priority_filter,
      zone_filter = input$zone_filter,
      larvae_threshold = input$larvae_threshold,
      bti_effect_days_override = input$bti_effect_days_override
    )
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
    if (!is.null(input$material_filter) && length(input$material_filter) > 0 && !"All" %in% input$material_filter) {
      # Filter active treatments by material, keep all other statuses
      active_treatments <- data[data$site_status == "Active Treatment" & 
                               (!is.na(data$last_treatment_material) & 
                                data$last_treatment_material %in% input$material_filter), ]
      other_statuses <- data[data$site_status != "Active Treatment", ]
      data <- rbind(active_treatments, other_statuses)
    }
    
    return(data)
  })
  
  # Value boxes
  output$total_air_sites <- renderValueBox({
    data <- filtered_data()
    valueBox(
      value = nrow(data),
      subtitle = "Total Air Sites",
      icon = icon("helicopter"),
      color = "blue"
    )
  })
  
  output$sites_unknown <- renderValueBox({
    data <- filtered_data()
    count <- sum(data$site_status == "Unknown", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Unknown Status",
      icon = icon("question-circle"),
      color = "yellow"
    )
  })
  
  output$sites_inspected <- renderValueBox({
    data <- filtered_data()
    count <- sum(data$site_status == "Inspected", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Inspected (Under Threshold)",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$sites_in_lab <- renderValueBox({
    data <- filtered_data()
    count <- sum(data$site_status == "In Lab", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "In Lab",
      icon = icon("flask"),
      color = "blue"
    )
  })

  output$sites_needs_treatment <- renderValueBox({
    data <- filtered_data()
    count <- sum(data$site_status == "Needs Treatment", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Needs Treatment",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })

  output$sites_active_treatment <- renderValueBox({
    data <- filtered_data()
    count <- sum(data$site_status == "Active Treatment", na.rm = TRUE)
    valueBox(
      value = count,
      subtitle = "Active Treatment",
      icon = icon("check-circle"),
      color = "green"
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
    
    create_site_map_working(data)
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
    
    # Create status summary chart
    status_counts <- data %>%
      group_by(site_status) %>%
      summarise(count = n(), .groups = 'drop')
    
    # Get colors from db_helpers to match the map
    status_color_map <- get_status_color_map()
    chart_colors <- c(
      "Active Treatment" = as.character(status_color_map[["Active Treatment"]]),
      "Needs Treatment" = as.character(status_color_map[["Needs Treatment"]]),
      "Inspected" = as.character(status_color_map[["Under Threshold"]]),  # Use green for inspected
      "In Lab" = "#ff9800",  # Orange for lab processing
      "Unknown" = as.character(status_color_map[["Unknown"]])
    )
    
    plot_ly(status_counts, 
            x = ~site_status, 
            y = ~count,
            type = 'bar',
            marker = list(color = ~chart_colors[site_status])) %>%
      layout(
        title = "Site Status Distribution",
        xaxis = list(title = "Status"),
        yaxis = list(title = "Number of Sites"),
        showlegend = FALSE
      )
  })
  
  # Sites Table
  output$site_details_table <- DT::renderDataTable({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    }
    
    # Use enhanced create_site_details_panel function
    table_data <- create_site_details_panel(data)
    
    DT::datatable(
      table_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(1, 2, 3, 5, 6))
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Status",  # Updated to match new column name
        backgroundColor = DT::styleEqual(
          c("Active Treatment", "Needs Treatment", "Inspected", "In Lab", "Unknown"),
          c("#d4edda", "#f8d7da", "#d1ecf1", "#fff3cd", "#f8f9fa")
        )
      )
  })
  
  # ============ TREATMENT PROCESS TAB LOGIC ============
  
  # Reactive data for process tab
  process_data <- eventReactive(input$refresh_process_data, {
    data <- get_air_sites_data_working(
      analysis_date = input$process_analysis_date,
      facility_filter = input$process_facility_filter,
      priority_filter = NULL,  # Include all priorities for process tracking
      zone_filter = input$process_zone_filter,
      larvae_threshold = input$process_larvae_threshold,
      bti_effect_days_override = input$process_bti_effect_days_override
    )
    
    # Apply material filter for active treatments
    if (!is.null(input$process_material_filter) && length(input$process_material_filter) > 0 && !"All" %in% input$process_material_filter) {
      # Filter active treatments by material, keep all other statuses
      active_treatments <- data[data$site_status == "Active Treatment" & 
                               (!is.na(data$last_treatment_material) & 
                                data$last_treatment_material %in% input$process_material_filter), ]
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
  output$sites_receiving_treatment <- renderValueBox({
    data <- process_data()
    if (input$refresh_process_data == 0) return(valueBox(0, "Active Treatments", icon = icon("check-circle"), color = "green"))
    
    metrics <- create_treatment_efficiency_metrics(data)
    valueBox(
      value = metrics$sites_receiving_treatment,
      subtitle = "Active Treatments",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$treatment_efficiency <- renderValueBox({
    data <- process_data()
    if (input$refresh_process_data == 0) return(valueBox("0%", "Treatment Efficiency", icon = icon("percent"), color = "blue"))
    
    metrics <- create_treatment_efficiency_metrics(data)
    valueBox(
      value = metrics$treatment_efficiency,
      subtitle = "Treatment Efficiency",
      icon = icon("percent"),
      color = "blue"
    )
  })
  
  output$inspection_coverage <- renderValueBox({
    data <- process_data()
    if (input$refresh_process_data == 0) return(valueBox("0%", "Inspection Coverage", icon = icon("search"), color = "purple"))
    
    metrics <- create_treatment_efficiency_metrics(data)
    valueBox(
      value = metrics$inspection_coverage,
      subtitle = "Inspection Coverage",
      icon = icon("search"),
      color = "purple"
    )
  })
  
  # Lab processing metrics value boxes
  output$total_inspected_samples <- renderValueBox({
    data <- process_data()
    if (input$refresh_process_data == 0) return(valueBox(0, "Inspected Samples", icon = icon("vial"), color = "orange"))
    
    lab_metrics <- analyze_lab_processing_metrics(data)
    valueBox(
      value = lab_metrics$total_inspected_with_samples,
      subtitle = "Inspected Samples",
      icon = icon("vial"),
      color = "orange"
    )
  })
  
  output$red_bug_detection_rate <- renderValueBox({
    data <- process_data()
    if (input$refresh_process_data == 0) return(valueBox("0%", "Red Bug Detection", icon = icon("bug"), color = "red"))
    
    lab_metrics <- analyze_lab_processing_metrics(data)
    valueBox(
      value = lab_metrics$red_bug_detection_rate,
      subtitle = "Red Bug Detection",
      icon = icon("bug"),
      color = "red"
    )
  })
  
  output$lab_completion_rate <- renderValueBox({
    data <- process_data()
    if (input$refresh_process_data == 0) return(valueBox("100%", "Lab Completion", icon = icon("flask"), color = "teal"))
    
    lab_metrics <- analyze_lab_processing_metrics(data)
    valueBox(
      value = lab_metrics$lab_completion_rate,
      subtitle = "Lab Completion",
      icon = icon("flask"),
      color = "teal"
    )
  })
  
  # Treatment flow chart
  output$treatment_flow_chart <- renderPlotly({
    data <- process_chart_data()  # Use filtered data for chart
    if (input$refresh_process_data == 0 || nrow(data) == 0) {
      return(plot_ly() %>%
        add_annotations(
          text = "No data available - Click 'Refresh Data'",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE
        ))
    }
    
    create_treatment_flow_chart(data)
  })
  
  # Process summary table
  output$process_summary_table <- DT::renderDataTable({
    data <- process_data()
    if (input$refresh_process_data == 0 || nrow(data) == 0) {
      return(DT::datatable(data.frame(), options = list(pageLength = 10)))
    }
    
    summary_data <- create_treatment_process_summary(data)
    
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
          c("Active Treatment", "Needs Treatment", "Inspected", "In Lab", "Unknown"),
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
        facility_choices_hist <- setNames(facility_lookup$short_name, facility_lookup$full_name)
        updateSelectizeInput(session, "hist_facility_filter", 
                            choices = facility_choices_hist,
                            selected = facility_lookup$short_name)
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
  
  # Reactive data for historical analysis
  historical_data <- eventReactive(input$refresh_historical_data, {
    get_historical_processed_data(
      hist_start_year = if (input$hist_time_period == "yearly") input$hist_start_year else input$hist_year,
      hist_end_year = if (input$hist_time_period == "yearly") input$hist_end_year else input$hist_year,
      hist_year = if (input$hist_time_period == "weekly") input$hist_year else NULL,
      time_period = input$hist_time_period,
      group_by = input$hist_group_by,
      facility_filter = input$hist_facility_filter,
      priority_filter = input$hist_priority_filter,
      zone_filter = input$hist_zone_filter,
      larvae_threshold = input$hist_larvae_threshold
    )
  })
  
  # Historical value boxes
  output$hist_total_inspections <- renderValueBox({
    data <- historical_data()
    if (input$refresh_historical_data == 0) return(valueBox(0, "Total Inspections", icon = icon("search"), color = "blue"))
    
    metrics <- create_historical_summary_metrics(data)
    valueBox(
      value = metrics$total_inspections,
      subtitle = "Total Inspections",
      icon = icon("search"),
      color = "blue"
    )
  })
  
  output$hist_red_bug_inspections <- renderValueBox({
    data <- historical_data()
    if (input$refresh_historical_data == 0) return(valueBox(0, "Red Bug Inspections", icon = icon("bug"), color = "red"))
    
    metrics <- create_historical_summary_metrics(data)
    valueBox(
      value = metrics$red_bug_inspections,
      subtitle = "Red Bug Inspections",
      icon = icon("bug"),
      color = "red"
    )
  })
  
  output$hist_avg_red_bug_ratio <- renderValueBox({
    data <- historical_data()
    if (input$refresh_historical_data == 0) return(valueBox("0%", "Avg Red Bug Ratio", icon = icon("percent"), color = "yellow"))
    
    metrics <- create_historical_summary_metrics(data)
    valueBox(
      value = metrics$avg_red_bug_ratio,
      subtitle = "Avg Red Bug Ratio",
      icon = icon("percent"),
      color = "yellow"
    )
  })
  
  output$hist_sites_with_data <- renderValueBox({
    data <- historical_data()
    if (input$refresh_historical_data == 0) return(valueBox(0, "Groups with Data", icon = icon("map-marker"), color = "green"))
    
    metrics <- create_historical_summary_metrics(data)
    valueBox(
      value = metrics$sites_with_data,
      subtitle = "Groups with Data",
      icon = icon("map-marker"),
      color = "green"
    )
  })
  
  # Historical trend chart
  output$historical_chart <- renderPlotly({
    data <- historical_data()
    if (input$refresh_historical_data == 0 || nrow(data) == 0) {
      return(plot_ly() %>%
        add_annotations(
          text = "No data available - Click 'Refresh Historical Data'",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE
        ))
    }
    
    create_historical_red_bug_chart(data, input$hist_time_period, input$hist_group_by)
  })
  
  # Historical summary chart
  output$hist_summary_chart <- renderPlotly({
    data <- historical_data()
    if (input$refresh_historical_data == 0 || nrow(data) == 0) {
      return(plot_ly() %>%
        add_annotations(
          text = "No data available",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE
        ))
    }
    
    create_historical_summary_chart(data)
  })
  
  # Historical details table
  output$historical_details_table <- DT::renderDataTable({
    data <- historical_data()
    if (input$refresh_historical_data == 0 || nrow(data) == 0) {
      return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    }
    
    table_data <- create_historical_details_table(data)
    
    DT::datatable(
      table_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)