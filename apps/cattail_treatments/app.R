# Cattail Treatments Dashboard
# MMCD Cattail Treatment Planning and Management System

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(leaflet)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sf)

# Source the shared database helper functions
source("../../shared/db_helpers.R")

# Source external function files
source("data_functions.R")
source("display_functions.R")
source("historical_functions.R")
source("ui_helper.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "MMCD Cattail Treatments",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Progress", tabName = "progress", icon = icon("chart-line")),
      menuItem("Historical", tabName = "historical", icon = icon("chart-line")),
      menuItem("Map", tabName = "map", icon = icon("map"))
    )
  ),
  
  dashboardBody(
    # Filter panel - always visible (matching ground prehatch style)
    create_filter_panel(),
    
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
      # Progress Tab (matching ground prehatch layout)
      tabItem(tabName = "progress",
        fluidRow(
          valueBoxOutput("sites_inspected_box", width = 2), 
          valueBoxOutput("under_threshold_box", width = 2),
          valueBoxOutput("active_treatments_box", width = 2),
          valueBoxOutput("treated_sites_box", width = 2),
          valueBoxOutput("treatment_coverage_box", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Current Inspection Progress", status = "primary", solidHeader = TRUE,
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
      
      # Historical Tab
      tabItem(tabName = "historical",
        fluidRow(
          box(
            title = "Historical Analysis Settings", status = "primary", solidHeader = TRUE,
            width = 12, collapsible = TRUE,
            fluidRow(
              column(2, create_time_period_selector()),
              column(2, selectInput("hist_display_metric", "Metric:",
                                   choices = list(
                                     "Sites Inspected" = "sites",
                                     "Sites Need Treatment" = "need_treatment",
                                     "Number of Inspections" = "inspections"
                                   ),
                                   selected = "sites")),
              column(2, create_year_range_selector()[[1]]),
              column(2, create_year_range_selector()[[2]]),
              column(2, create_chart_type_selector()),
              column(2, dateRangeInput("hist_date_range", "Date Range:",
                                     start = Sys.Date() - 365,
                                     end = Sys.Date(),
                                     format = "yyyy-mm-dd",
                                     width = "100%"))
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Historical Cattail Treatment Data", status = "primary", solidHeader = TRUE,
            width = 12, height = "600px",
            plotlyOutput("historical_chart", height = "550px")
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
  
  # Initialize facility choices from db_helpers - runs immediately on app load
  observe({
    facility_choices <- get_facility_choices()
    updateSelectizeInput(session, "facility_filter", choices = facility_choices, selected = "all")
  })
  
  # Initialize foreman choices from db_helpers - runs immediately on app load
  observe({
    foremen_lookup <- get_foremen_lookup()
    foremen_choices <- c("All" = "all")
    foremen_choices <- c(
      foremen_choices,
      setNames(foremen_lookup$emp_num, foremen_lookup$shortname)
    )
    updateSelectizeInput(session, "foreman_filter", choices = foremen_choices, selected = "all")
  })
  
  # Refresh data when button clicked - ONLY source of data loading
  observeEvent(input$refresh_data, {
    analysis_date <- input$analysis_date
    if (is.null(analysis_date)) analysis_date <- Sys.Date()
    
    current_year <- year(analysis_date)
    values$raw_data <- load_cattail_data(
      analysis_date = analysis_date,
      include_archive = TRUE,
      start_year = current_year - 2,
      end_year = current_year
    )
    
    # Show notification
    if (!is.null(values$raw_data)) {
      showNotification("Cattail data refreshed successfully", type = "message")
    } else {
      showNotification("Failed to load cattail data", type = "error")
    }
  })
  
  # Filter and aggregate data reactively
  observe({
    if (!is.null(values$raw_data)) {
      # Determine zone filter based on zone_display setting
      zone_filter <- switch(input$zone_display,
        "p1" = "1",
        "p2" = "2", 
        "separate" = c("1", "2"),
        "combined" = c("1", "2")
      )
      
      # Determine if zones should be combined for aggregation
      combine_zones <- input$zone_display %in% c("combined", "p1", "p2")
      
      values$filtered_data <- filter_cattail_data(
        values$raw_data,
        zone_filter = zone_filter,
        facility_filter = if("all" %in% input$facility_filter || is.null(input$facility_filter)) "all" else input$facility_filter,
        foreman_filter = if("all" %in% input$foreman_filter || is.null(input$foreman_filter)) "all" else input$foreman_filter
      )
      
      if (!is.null(values$filtered_data)) {
        values$aggregated_data <- aggregate_cattail_data(values$filtered_data, input$analysis_date)
      }
    }
  })
  
  # Create value boxes
  cattail_values <- reactive({
    if (is.null(values$filtered_data) || is.null(values$filtered_data$cattail_sites)) {
      return(list(
        total_sites = 0, total_acres = 0, sites_inspected = 0,
        sites_need_treatment = 0, inspection_coverage = 0, percent_need_treatment = 0
      ))
    }
    
    cattail_sites <- values$filtered_data$cattail_sites
    
    if (nrow(cattail_sites) == 0) {
      return(list(
        total_sites = 0, total_acres = 0, sites_inspected = 0,
        sites_need_treatment = 0, inspection_coverage = 0, percent_need_treatment = 0
      ))
    }
    
    # Calculate the actual metrics - only inspected sites (action 9) this year
    sites_under_threshold <- sum(cattail_sites$state == "under_threshold", na.rm = TRUE)
    sites_need_treatment <- sum(cattail_sites$state == "need_treatment", na.rm = TRUE)
    sites_treated <- sum(cattail_sites$state == "treated", na.rm = TRUE)
    sites_inspected <- sites_under_threshold + sites_need_treatment + sites_treated
    
    list(
      sites_inspected = sites_inspected,  # This is the "total" 
      total_acres = round(sum(cattail_sites$acres, na.rm = TRUE), 1),
      sites_under_threshold = sites_under_threshold,
      sites_need_treatment = sites_need_treatment,
      sites_treated = sites_treated,
      percent_need_treatment = if(sites_inspected > 0) round(sites_need_treatment / sites_inspected * 100, 1) else 0,
      percent_treated = if(sites_inspected > 0) round(sites_treated / sites_inspected * 100, 1) else 0
    )
  })
  
  # Value box outputs
  
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
      value = cattail_values()$sites_need_treatment,
      subtitle = "Need Treatment",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$treatment_coverage_box <- renderValueBox({
    valueBox(
      value = paste0(cattail_values()$percent_treated, "% Treated"),
      subtitle = "Treatment Progress", 
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$sites_inspected_box <- renderValueBox({
    valueBox(
      value = cattail_values()$sites_inspected,
      subtitle = "Total Sites Inspected",
      icon = icon("clipboard-check"),
      color = "blue"
    )
  })
  
  output$under_threshold_box <- renderValueBox({
    valueBox(
      value = cattail_values()$sites_under_threshold,
      subtitle = "Under Threshold",
      icon = icon("check-circle"),
      color = "light-blue"
    )
  })
  
  output$treated_sites_box <- renderValueBox({
    valueBox(
      value = cattail_values()$sites_treated,
      subtitle = "Treated Sites",
      icon = icon("check-double"),
      color = "green"
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
    if (is.null(values$filtered_data) || is.null(values$filtered_data$cattail_sites)) {
      return(ggplot() + geom_text(aes(x = 1, y = 1, label = "No data available"), size = 6) + theme_void())
    }
    
    # Create current progress stacked bar chart
    cattail_sites <- values$filtered_data$cattail_sites
    
    # Group by facility (or other grouping) for the stacked bars
    group_by <- input$group_by
    if (is.null(group_by)) group_by <- "facility"
    
    progress_data <- cattail_sites %>%
      group_by(
        group_name = case_when(
          group_by == "facility" ~ facility,
          group_by == "foreman" ~ paste("FOS", fosarea),
          group_by == "sectcode" ~ paste("Section", sectcode),
          TRUE ~ "All"
        )
      ) %>%
      summarise(
        inspected_under_threshold = sum(state == "under_threshold", na.rm = TRUE),
        need_treatment = sum(state == "need_treatment", na.rm = TRUE), 
        treated = sum(state == "treated", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Reshape for stacked bar chart
      pivot_longer(
        cols = c("inspected_under_threshold", "need_treatment", "treated"),
        names_to = "status",
        values_to = "count"
      ) %>%
      mutate(
        status = factor(status, 
                       levels = c("inspected_under_threshold", "need_treatment", "treated"),
                       labels = c("Inspected (Under Threshold)", "Need Treatment", "Treated"))
      )
    
    # Create stacked bar chart
    p <- ggplot(progress_data, aes(x = group_name, y = count, fill = status)) +
      geom_col(position = "stack", width = 0.7) +
      scale_fill_manual(
        values = c("Inspected (Under Threshold)" = "lightgrey", 
                   "Need Treatment" = "firebrick",
                   "Treated" = "forestgreen")
      ) +
      labs(
        title = "Current Cattail Inspection Progress",
        x = stringr::str_to_title(group_by),
        y = "Number of Sites",
        fill = "Status"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$efficacy_chart <- renderPlotly({
    efficacy_data <- data.frame() # No efficacy data yet
    treatments_data <- if (!is.null(values$filtered_data)) {
      values$filtered_data$cattail_sites %>% filter(state == "treated")
    } else {
      data.frame()
    }
    create_efficacy_chart(efficacy_data, treatments_data)
  })
  
  output$planning_calendar <- renderPlotly({
    plans_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatment_plans else data.frame()
    create_planning_calendar(plans_data)
  })
  
  output$historical_chart <- renderPlotly({
    if (is.null(values$raw_data)) {
      return(ggplot() + geom_text(aes(x = 1, y = 1, label = "No data available"), size = 6) + theme_void())
    }
    
    time_period <- input$time_period
    chart_type <- input$chart_type
    hist_display_metric <- input$hist_display_metric
    start_year <- input$start_year
    end_year <- input$end_year
    hist_date_range <- input$hist_date_range
    
    # Use defaults if inputs are NULL
    if (is.null(time_period)) time_period <- "monthly"
    if (is.null(chart_type)) chart_type <- "line"
    if (is.null(hist_display_metric)) hist_display_metric <- "sites"
    if (is.null(start_year)) start_year <- 2022
    if (is.null(end_year)) end_year <- year(Sys.Date())
    if (is.null(hist_date_range)) {
      start_date <- as.Date("2022-01-01")
      end_date <- Sys.Date()
    } else {
      start_date <- hist_date_range[1]
      end_date <- hist_date_range[2]
    }
    
    create_historical_analysis_chart(
      values$raw_data, 
      group_by = input$group_by,
      time_period = time_period,
      chart_type = chart_type,
      display_metric = hist_display_metric,
      start_date = start_date,
      end_date = end_date,
      combine_zones = input$zone_display %in% c("combined", "p1", "p2")
    )
  })
  
  # Table outputs
  output$treatments_table <- renderDT({
    treatments_data <- if (!is.null(values$filtered_data) && !is.null(values$filtered_data$cattail_sites)) {
      values$filtered_data$cattail_sites %>% filter(state == "treated")
    } else {
      data.frame()
    }
    foremen_lookup <- if (!is.null(values$filtered_data)) values$filtered_data$foremen_lookup else data.frame()
    create_treatments_table(treatments_data, foremen_lookup)
  })
  
  output$plans_table <- renderDT({
    plans_data <- data.frame() # No plans data yet
    foremen_lookup <- if (!is.null(values$filtered_data)) values$filtered_data$foremen_lookup else data.frame()
    create_plans_table(plans_data, foremen_lookup)
  })
  
  output$recent_activity_table <- renderDT({
    treatments_data <- if (!is.null(values$filtered_data) && !is.null(values$filtered_data$cattail_sites)) {
      values$filtered_data$cattail_sites %>% filter(state == "treated")
    } else {
      data.frame()
    }
    
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
    treatments_data <- if (!is.null(values$filtered_data) && !is.null(values$filtered_data$cattail_sites)) {
      values$filtered_data$cattail_sites %>% filter(state == "treated")
    } else {
      data.frame()
    }
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
    if (is.null(values$filtered_data) || is.null(values$filtered_data$cattail_sites) || nrow(values$filtered_data$cattail_sites) == 0) {
      return(data.frame(Metric = "No data available", Value = ""))
    }
    
    stats <- cattail_values()
    data.frame(
      Metric = c("Sites Inspected", "Total Acres", "Under Threshold", "Need Treatment", 
                "% Need Treatment", "% Treated"),
      Value = c(stats$sites_inspected, paste(stats$total_acres, "ac"), stats$sites_under_threshold,
               stats$sites_need_treatment, paste0(stats$percent_need_treatment, "%"), paste0(stats$percent_treated, "%"))
    )
  }, bordered = TRUE, spacing = "xs")
  
  # Download handlers
  output$download_treatments <- downloadHandler(
    filename = function() {
      paste0("cattail_treatments_", Sys.Date(), ".csv")
    },
    content = function(file) {
      treatments_data <- if (!is.null(values$filtered_data) && !is.null(values$filtered_data$cattail_sites)) {
        values$filtered_data$cattail_sites %>% filter(state == "treated")
      } else {
        data.frame()
      }
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
    content = function(file) {
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