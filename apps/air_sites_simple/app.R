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
source("../../shared/db_helpers.R")

# Source app-specific functions
source("air_status_functions.R")
source("air_status_functions_enhanced.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Air Work Pipeline - Simple"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Air Site Status", tabName = "status", icon = icon("helicopter")),
      menuItem("Treatment Process", tabName = "process", icon = icon("chart-line"))
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
                checkboxGroupInput("zone_filter", "Zones:",
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
                numericInput("bit_effect_days_override", "BIT Effect Days Override:",
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
          valueBoxOutput("total_air_sites", width = 3),
          valueBoxOutput("sites_unknown", width = 3),
          valueBoxOutput("sites_needs_treatment", width = 3),
          valueBoxOutput("sites_active_treatment", width = 3)
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
                numericInput("process_bit_effect_days_override", "BIT Effect Days Override:",
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
                             "Needs Treatment" = "Needs Treatment",
                             "Active Treatment" = "Active Treatment"),
                  selected = c("Unknown", "Inspected", "Needs Treatment", "Active Treatment"),
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
          valueBoxOutput("total_sites_needing_action", width = 3),
          valueBoxOutput("sites_receiving_treatment", width = 3),
          valueBoxOutput("treatment_efficiency", width = 3),
          valueBoxOutput("inspection_coverage", width = 3)
        ),
        
        fluidRow(
          box(title = "Calculation Notes", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
            tags$ul(
              tags$li(tags$strong("Treatment Efficiency:"), " Active Treatments ÷ (Needs Treatment + Active Treatment) × 100%"),
              tags$li(tags$strong("Inspection Coverage:"), " (Inspected + Needs Treatment + Active Treatment) ÷ Total Sites × 100%"),
              tags$li(tags$strong("Sites Needing Action:"), " Total sites that need treatment (Needs Treatment + Active Treatment)"),
              tags$li(tags$strong("Note:"), " Higher treatment efficiency indicates better follow-through from identification to treatment")
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
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Load filter choices on startup
  observe({
    tryCatch({
      # Load facility choices using shared function
      facility_lookup <- get_facility_lookup()
      if (nrow(facility_lookup) > 0) {
        facility_choices <- setNames(facility_lookup$short_name, facility_lookup$full_name)
        updateSelectizeInput(session, "facility_filter", 
                            choices = facility_choices,
                            selected = facility_lookup$short_name)
        # Also update process tab facility filter
        updateSelectizeInput(session, "process_facility_filter", 
                            choices = facility_choices,
                            selected = facility_lookup$short_name)
      } else {
        # Fallback if database lookup fails
        facility_choices <- c("MMCD", "SMCD", "RMCD")
        updateSelectizeInput(session, "facility_filter", 
                            choices = facility_choices,
                            selected = facility_choices)
        updateSelectizeInput(session, "process_facility_filter", 
                            choices = facility_choices,
                            selected = facility_choices)
      }
      
      # Load priority choices using shared function
      priority_choices <- get_priority_choices(include_all = FALSE)
      priority_choices <- priority_choices[names(priority_choices) != "All Priorities"]
      updateSelectizeInput(session, "priority_filter", 
                          choices = priority_choices,
                          selected = priority_choices)
      
      # Load zone choices
      zone_choices <- get_available_zones()
      updateCheckboxGroupInput(session, "zone_filter", 
                              choices = setNames(zone_choices, paste0("P", zone_choices)),
                              selected = zone_choices)
      
      # Load treatment material choices
      material_choices <- get_treatment_materials(include_all = TRUE)
      if (length(material_choices) > 0) {
        updateSelectizeInput(session, "material_filter", 
                            choices = setNames(material_choices, material_choices),
                            selected = "All")
        updateSelectizeInput(session, "process_material_filter", 
                            choices = setNames(material_choices, material_choices),
                            selected = "All")
      }
      
    }, error = function(e) {
      warning(paste("Error loading filter choices:", e$message))
    })
  })
  
##============Synchronization Logic============

  # Synchronize analysis date between tabs
  observeEvent(input$analysis_date, {
    updateDateInput(session, "process_analysis_date", value = input$analysis_date)
  })
  
  observeEvent(input$process_analysis_date, {
    updateDateInput(session, "analysis_date", value = input$process_analysis_date)
  })
  
  # Synchronize facility filter between tabs
  observeEvent(input$facility_filter, {
    updateSelectizeInput(session, "process_facility_filter", selected = input$facility_filter)
  }, ignoreInit = TRUE)
  
  observeEvent(input$process_facility_filter, {
    updateSelectizeInput(session, "facility_filter", selected = input$process_facility_filter)
  }, ignoreInit = TRUE)
  
  # Synchronize larvae threshold between tabs
  observeEvent(input$larvae_threshold, {
    updateNumericInput(session, "process_larvae_threshold", value = input$larvae_threshold)
  })
  
  observeEvent(input$process_larvae_threshold, {
    updateNumericInput(session, "larvae_threshold", value = input$process_larvae_threshold)
  })
  
  # Synchronize material filter between tabs
  observeEvent(input$material_filter, {
    updateSelectizeInput(session, "process_material_filter", selected = input$material_filter)
  }, ignoreInit = TRUE)
  
  observeEvent(input$process_material_filter, {
    updateSelectizeInput(session, "material_filter", selected = input$process_material_filter)
  }, ignoreInit = TRUE)
  
  # Synchronize BIT effect days override between tabs
  observeEvent(input$bit_effect_days_override, {
    updateNumericInput(session, "process_bit_effect_days_override", value = input$bit_effect_days_override)
  })
  
  observeEvent(input$process_bit_effect_days_override, {
    updateNumericInput(session, "bit_effect_days_override", value = input$process_bit_effect_days_override)
  })
  
  # ============ AIR SITE STATUS TAB LOGIC ============
  # Reactive data
  air_sites_data <- eventReactive(input$refresh_data, {
    get_air_sites_data_enhanced(
      analysis_date = input$analysis_date,
      facility_filter = input$facility_filter,
      priority_filter = input$priority_filter,
      zone_filter = input$zone_filter,
      larvae_threshold = input$larvae_threshold,
      bit_effect_days_override = input$bit_effect_days_override
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
    
    create_site_map_enhanced(data)
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
          c("Active Treatment", "Needs Treatment", "Inspected", "Unknown"),
          c("#d4edda", "#f8d7da", "#d1ecf1", "#f8f9fa")
        )
      )
  })
  
  # ============ TREATMENT PROCESS TAB LOGIC ============
  
  # Reactive data for process tab
  process_data <- eventReactive(input$refresh_process_data, {
    data <- get_air_sites_data_enhanced(
      analysis_date = input$process_analysis_date,
      facility_filter = input$process_facility_filter,
      priority_filter = NULL,  # Include all priorities for process tracking
      zone_filter = NULL,      # Include all zones for process tracking
      larvae_threshold = input$process_larvae_threshold,
      bit_effect_days_override = input$process_bit_effect_days_override
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
  output$total_sites_needing_action <- renderValueBox({
    data <- process_data()
    if (input$refresh_process_data == 0) return(valueBox(0, "Sites Needing Action", icon = icon("exclamation-triangle"), color = "yellow"))
    
    metrics <- create_treatment_efficiency_metrics(data)
    valueBox(
      value = metrics$total_sites_needing_action,
      subtitle = "Sites Needing Action",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
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
          c("Active Treatment", "Needs Treatment", "Inspected", "Unknown"),
          c("#d4edda", "#f8d7da", "#d1ecf1", "#f8f9fa")
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)