# Cattail Treatments Dashboard
# MMCD Cattail Treatment Planning and Management System

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(leaflet)
library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)

# Source the shared database helper functions
source("../../shared/db_helpers.R")

# Source external function files
source("data_functions.R")
source("display_functions.R")
source("ui_helper.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "MMCD Cattail Treatments",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Treatment Progress", tabName = "progress", icon = icon("tasks")),
      menuItem("Planning", tabName = "planning", icon = icon("calendar-alt")),
      menuItem("Details", tabName = "details", icon = icon("table")),
      menuItem("Map", tabName = "map", icon = icon("map"))
    ),
    
    # Filters
    h4("Filters", style = "color: white; margin: 15px;"),
    
    div(style = "margin: 15px;",
      create_zone_filter(),
      br(),
      create_grouping_selector(),
      br(),
      create_facility_filter(),
      br(), 
      create_foreman_filter(),
      br(),
      create_section_filter(),
      br(),
      create_treatment_type_filter(),
      br(),
      create_status_filter(),
      br(),
      create_date_range_filter(),
      br(),
      create_refresh_button()
    )
  ),
  
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .value-box-text > p {
          font-size: 16px !important;
        }
        .small-box h3 {
          font-size: 28px !important;
        }
        .small-box p {
          font-size: 14px !important;
        }
        .box.box-solid.box-primary > .box-header {
          color: #fff;
          background: #3c8dbc;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3c8dbc;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("total_sites_box", width = 2),
          valueBoxOutput("total_acres_box", width = 2), 
          valueBoxOutput("total_treatments_box", width = 2),
          valueBoxOutput("active_treatments_box", width = 2),
          valueBoxOutput("treatment_coverage_box", width = 2),
          valueBoxOutput("upcoming_plans_box", width = 2)
        ),
        
        fluidRow(
          box(
            title = "Treatment Timeline", status = "primary", solidHeader = TRUE,
            width = 8, height = "500px",
            plotlyOutput("timeline_chart", height = "450px")
          ),
          box(
            title = "Summary Statistics", status = "primary", solidHeader = TRUE,
            width = 4, height = "500px",
            tableOutput("summary_stats")
          )
        ),
        
        fluidRow(
          box(
            title = "Recent Activity", status = "primary", solidHeader = TRUE,
            width = 12, 
            DTOutput("recent_activity_table")
          )
        )
      ),
      
      # Treatment Progress Tab
      tabItem(tabName = "progress",
        fluidRow(
          box(
            title = "Treatment Progress by Group", status = "primary", solidHeader = TRUE,
            width = 12, height = "600px",
            plotlyOutput("progress_chart", height = "550px")
          )
        ),
        
        fluidRow(
          box(
            title = "Efficacy Analysis", status = "primary", solidHeader = TRUE,
            width = 6, height = "500px",
            plotlyOutput("efficacy_chart", height = "450px")
          ),
          box(
            title = "Treatment Methods", status = "primary", solidHeader = TRUE,
            width = 6, height = "500px", 
            plotlyOutput("methods_chart", height = "450px")
          )
        )
      ),
      
      # Planning Tab
      tabItem(tabName = "planning",
        fluidRow(
          valueBoxOutput("total_plans_box", width = 3),
          valueBoxOutput("overdue_plans_box", width = 3),
          valueBoxOutput("upcoming_week_box", width = 3),
          valueBoxOutput("upcoming_month_box", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Planning Calendar", status = "primary", solidHeader = TRUE,
            width = 12, height = "500px",
            plotlyOutput("planning_calendar", height = "450px")
          )
        ),
        
        fluidRow(
          box(
            title = "Treatment Plans", status = "primary", solidHeader = TRUE,
            width = 12,
            div(style = "margin-bottom: 10px;",
              create_download_button("download_plans", "Download Plans")
            ),
            DTOutput("plans_table")
          )
        )
      ),
      
      # Details Tab
      tabItem(tabName = "details",
        fluidRow(
          box(
            title = "Treatment Details", status = "primary", solidHeader = TRUE,
            width = 12,
            div(style = "margin-bottom: 10px;",
              create_download_button("download_treatments", "Download Treatments")
            ),
            DTOutput("treatments_table")
          )
        )
      ),
      
      # Map Tab
      tabItem(tabName = "map",
        fluidRow(
          box(
            title = "Cattail Treatment Sites", status = "primary", solidHeader = TRUE,
            width = 12, height = "700px",
            div(style = "margin-bottom: 10px;",
              radioButtons("basemap", "Base Map:",
                          choices = list("Street" = "carto", "Satellite" = "satellite", 
                                       "Terrain" = "terrain", "OpenStreetMap" = "osm"),
                          selected = "carto", inline = TRUE)
            ),
            leafletOutput("treatment_map", height = "600px")
          )
        ),
        
        fluidRow(
          box(
            title = "Site Details", status = "primary", solidHeader = TRUE,
            width = 12,
            DTOutput("map_details_table")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    raw_data = NULL,
    filtered_data = NULL,
    aggregated_data = NULL,
    filter_choices = NULL
  )
  
  # Load initial data
  observe({
    values$raw_data <- load_cattail_data(
      analysis_date = Sys.Date(),
      include_archive = TRUE,
      start_year = year(input$date_range[1]), 
      end_year = year(input$date_range[2])
    )
    
    if (!is.null(values$raw_data)) {
      values$filter_choices <- get_filter_choices(values$raw_data)
      
      # Update filter choices
      updateCheckboxGroupInput(session, "facility_filter",
                              choices = setNames(values$filter_choices$facilities, values$filter_choices$facilities),
                              selected = values$filter_choices$facilities)
      
      updateCheckboxGroupInput(session, "foreman_filter", 
                              choices = setNames(values$filter_choices$foremen, values$filter_choices$foremen),
                              selected = values$filter_choices$foremen)
      
      updateCheckboxGroupInput(session, "section_filter",
                              choices = setNames(values$filter_choices$sections, values$filter_choices$sections),
                              selected = values$filter_choices$sections)
      
      updateCheckboxGroupInput(session, "treatment_type_filter",
                              choices = setNames(values$filter_choices$treatment_types, values$filter_choices$treatment_types),
                              selected = values$filter_choices$treatment_types)
    }
  })
  
  # Refresh data when button clicked
  observeEvent(input$refresh_data, {
    values$raw_data <- load_cattail_data(
      analysis_date = Sys.Date(),
      include_archive = TRUE,
      start_year = year(input$date_range[1]),
      end_year = year(input$date_range[2])
    )
  })
  
  # Filter and aggregate data reactively
  observe({
    if (!is.null(values$raw_data)) {
      values$filtered_data <- filter_cattail_data(
        values$raw_data,
        zone_filter = input$zone_filter,
        facility_filter = if("all" %in% input$facility_filter || is.null(input$facility_filter)) "all" else input$facility_filter,
        foreman_filter = if("all" %in% input$foreman_filter || is.null(input$foreman_filter)) "all" else input$foreman_filter,
        section_filter = if("all" %in% input$section_filter || is.null(input$section_filter)) "all" else input$section_filter,
        treatment_type_filter = if("all" %in% input$treatment_type_filter || is.null(input$treatment_type_filter)) "all" else input$treatment_type_filter,
        status_filter = if("all" %in% input$status_filter || is.null(input$status_filter)) "all" else input$status_filter,
        date_range = input$date_range
      )
      
      if (!is.null(values$filtered_data)) {
        values$aggregated_data <- aggregate_cattail_data(values$filtered_data, input$group_by)
      }
    }
  })
  
  # Create value boxes
  cattail_values <- reactive({
    if (is.null(values$aggregated_data) || nrow(values$aggregated_data) == 0) {
      return(list(
        total_sites = 0, total_acres = 0, total_treatments = 0,
        active_treatments = 0, treatment_coverage = 0, upcoming_plans = 0,
        total_plans = 0, overdue_plans = 0, active_percentage = 0
      ))
    }
    
    treatments_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatments else data.frame()
    plans_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatment_plans else data.frame()
    
    create_cattail_value_boxes(values$aggregated_data, treatments_data, plans_data)
  })
  
  # Value box outputs
  output$total_sites_box <- renderValueBox({
    valueBox(
      value = cattail_values()$total_sites,
      subtitle = "Total Sites",
      icon = icon("map-marker-alt"),
      color = "blue"
    )
  })
  
  output$total_acres_box <- renderValueBox({
    valueBox(
      value = paste(cattail_values()$total_acres, "ac"),
      subtitle = "Total Acres", 
      icon = icon("area-chart"),
      color = "green"
    )
  })
  
  output$total_treatments_box <- renderValueBox({
    valueBox(
      value = cattail_values()$total_treatments,
      subtitle = "Treatments Applied",
      icon = icon("spray-can"),
      color = "yellow"
    )
  })
  
  output$active_treatments_box <- renderValueBox({
    valueBox(
      value = cattail_values()$active_treatments,
      subtitle = "Active Treatments",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$treatment_coverage_box <- renderValueBox({
    valueBox(
      value = paste0(cattail_values()$treatment_coverage, "%"),
      subtitle = "Treatment Coverage",
      icon = icon("percentage"),
      color = "purple"
    )
  })
  
  output$upcoming_plans_box <- renderValueBox({
    valueBox(
      value = cattail_values()$upcoming_plans,
      subtitle = "Upcoming Plans",
      icon = icon("calendar-check"),
      color = "orange"
    )
  })
  
  output$total_plans_box <- renderValueBox({
    valueBox(
      value = cattail_values()$total_plans,
      subtitle = "Total Plans",
      icon = icon("list-alt"),
      color = "blue"
    )
  })
  
  output$overdue_plans_box <- renderValueBox({
    valueBox(
      value = cattail_values()$overdue_plans,
      subtitle = "Overdue Plans",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$upcoming_week_box <- renderValueBox({
    plans_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatment_plans else data.frame()
    upcoming_week <- if (nrow(plans_data) > 0) sum(plans_data$plan_status == "Due This Week", na.rm = TRUE) else 0
    
    valueBox(
      value = upcoming_week,
      subtitle = "Due This Week",
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  output$upcoming_month_box <- renderValueBox({
    plans_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatment_plans else data.frame()
    upcoming_month <- if (nrow(plans_data) > 0) sum(plans_data$plan_status == "Due This Month", na.rm = TRUE) else 0
    
    valueBox(
      value = upcoming_month,
      subtitle = "Due This Month",
      icon = icon("calendar"),
      color = "orange"
    )
  })
  
  # Chart outputs
  output$progress_chart <- renderPlotly({
    if (is.null(values$aggregated_data) || nrow(values$aggregated_data) == 0) {
      return(ggplot() + geom_text(aes(x = 1, y = 1, label = "No data available"), size = 6) + theme_void())
    }
    create_treatment_progress_chart(values$aggregated_data, input$group_by)
  })
  
  output$timeline_chart <- renderPlotly({
    treatments_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatments else data.frame()
    create_treatment_timeline(treatments_data, input$group_by)
  })
  
  output$efficacy_chart <- renderPlotly({
    efficacy_data <- if (!is.null(values$filtered_data)) values$filtered_data$efficacy_data else data.frame()
    treatments_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatments else data.frame()
    create_efficacy_chart(efficacy_data, treatments_data)
  })
  
  output$planning_calendar <- renderPlotly({
    plans_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatment_plans else data.frame()
    create_planning_calendar(plans_data)
  })
  
  # Table outputs
  output$treatments_table <- renderDT({
    treatments_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatments else data.frame()
    foremen_lookup <- if (!is.null(values$filtered_data)) values$filtered_data$foremen_lookup else data.frame()
    create_treatments_table(treatments_data, foremen_lookup)
  })
  
  output$plans_table <- renderDT({
    plans_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatment_plans else data.frame()
    foremen_lookup <- if (!is.null(values$filtered_data)) values$filtered_data$foremen_lookup else data.frame()
    create_plans_table(plans_data, foremen_lookup)
  })
  
  output$recent_activity_table <- renderDT({
    treatments_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatments else data.frame()
    
    if (nrow(treatments_data) > 0) {
      recent_treatments <- treatments_data %>%
        arrange(desc(as.Date(inspdate))) %>%
        head(10) %>%
        select(
          Date = inspdate,
          Sitecode = sitecode,
          Facility = facility_display,
          Material = material_name,
          "Acres Treated" = treated_acres,
          Status = treatment_status
        )
      
      datatable(recent_treatments, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
    } else {
      datatable(data.frame(Message = "No recent activity"), options = list(dom = 't'), rownames = FALSE)
    }
  })
  
  # Map output
  output$treatment_map <- renderLeaflet({
    sites_data <- if (!is.null(values$filtered_data)) values$filtered_data$cattail_sites else data.frame()
    treatments_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatments else data.frame()
    create_cattail_map(sites_data, treatments_data, input$basemap)
  })
  
  output$map_details_table <- renderDT({
    sites_data <- if (!is.null(values$filtered_data)) values$filtered_data$cattail_sites else data.frame()
    
    if (nrow(sites_data) > 0 && "sf" %in% class(sites_data)) {
      table_data <- st_drop_geometry(sites_data) %>%
        select(
          Sitecode = sitecode,
          Facility = facility_display, 
          Zone = zone,
          Section = sectcode,
          Acres = acres,
          Priority = priority
        ) %>%
        arrange(Facility, Sitecode)
      
      datatable(table_data, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
    } else {
      datatable(data.frame(Message = "No site data available"), options = list(dom = 't'), rownames = FALSE)
    }
  })
  
  # Summary statistics
  output$summary_stats <- renderTable({
    if (is.null(values$aggregated_data) || nrow(values$aggregated_data) == 0) {
      return(data.frame(Metric = "No data available", Value = ""))
    }
    
    stats <- cattail_values()
    data.frame(
      Metric = c("Total Sites", "Total Acres", "Total Treatments", "Active Treatments", 
                "Treatment Coverage", "Upcoming Plans"),
      Value = c(stats$total_sites, paste(stats$total_acres, "ac"), stats$total_treatments,
               stats$active_treatments, paste0(stats$treatment_coverage, "%"), stats$upcoming_plans)
    )
  }, bordered = TRUE, spacing = "xs")
  
  # Download handlers
  output$download_treatments <- downloadHandler(
    filename = function() {
      paste0("cattail_treatments_", Sys.Date(), ".csv")
    },
    content = function(file) {
      treatments_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatments else data.frame()
      foremen_lookup <- if (!is.null(values$filtered_data)) values$filtered_data$foremen_lookup else data.frame()
      
      if (nrow(treatments_data) > 0) {
        download_data <- prepare_cattail_download_data(treatments_data, data.frame(), data.frame(), foremen_lookup)
        write.csv(download_data$treatments, file, row.names = FALSE)
      } else {
        write.csv(data.frame(Message = "No treatment data available"), file, row.names = FALSE)
      }
    }
  )
  
  output$download_plans <- downloadHandler(
    filename = function() {
      paste0("cattail_plans_", Sys.Date(), ".csv")
    },
    content = function() {
      plans_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatment_plans else data.frame()
      foremen_lookup <- if (!is.null(values$filtered_data)) values$filtered_data$foremen_lookup else data.frame()
      
      if (nrow(plans_data) > 0) {
        download_data <- prepare_cattail_download_data(data.frame(), plans_data, data.frame(), foremen_lookup)
        write.csv(download_data$plans, file, row.names = FALSE)
      } else {
        write.csv(data.frame(Message = "No planning data available"), file, row.names = FALSE)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)