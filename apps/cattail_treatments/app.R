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
      menuItem("Map", tabName = "map", icon = icon("map")),
      
      # Theme selector
      selectInput(
        "color_theme",
        "Color Theme:",
        choices = c(
          "MMCD (Default)" = "MMCD",
          "IBM Design" = "IBM",
          "Color-Blind Friendly" = "Wong",
          "Scientific" = "Tol",
          "Viridis" = "Viridis",
          "ColorBrewer" = "ColorBrewer"
        ),
        selected = "MMCD"
      ),
      tags$small(style = "color: #999; padding-left: 15px;", "Changes chart colors")
    )
  ),
  
  dashboardBody(
    # Use universal CSS from db_helpers for consistent text sizing
    get_universal_text_css(),
    
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
            title = "Current Cattail Progress", status = "primary", solidHeader = TRUE,
            width = 12,
            plotlyOutput("timeline_chart", height = "450px")
          )
        ),
        
        fluidRow(
          box(
            title = "Status table", status = "primary", solidHeader = TRUE,
            width = 12, 
            DTOutput("status_table")
          )
        )
      ),
      
      # Treatment Progress Tab
      tabItem(tabName = "progress",
        fluidRow(
          box(
            title = "Treatment Progress by Group", status = "primary", solidHeader = TRUE,
            width = 12, height = "500",
            plotlyOutput("progress_chart", height = "900px")
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
              column(4, create_year_range_selector()),
              column(4, selectInput("hist_status_metric", "Status Metric:",
                                   choices = list(
                                     "Need Treatment (up to December 1)" = "need_treatment",
                                     "Treated (up to Aug 1)" = "treated",
                                     "% Treated of Need Treatment (up to Aug 1)" = "pct_treated"
                                   ),
                                   selected = "need_treatment")),
              column(2, create_chart_type_selector()),
              column(2, actionButton("refresh_historical", "Refresh Data", 
                                   icon = icon("refresh"),
                                   class = "btn-primary",
                                   style = "margin-top: 25px; width: 100%;"))
            ),
            fluidRow(
              column(12, 
                tags$p(
                  style = "margin-top: 10px; font-style: italic; color: #666;",
                  "Note: cattail Year runs from Fall (Sept-Dec) to Summer (May-Aug). Year 2022 = Fall 2022 through Summer 2023. Use Analysis Settings above to toggle between Sites and Acres."
                )
              )
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
  
  # Reactive theme value
  current_theme <- reactive({
    input$color_theme
  })
  
  # Update global theme option when theme changes
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
  
  # Reactive values
  values <- reactiveValues(
    raw_data = NULL,
    filtered_data = NULL,
    aggregated_data = NULL,
    filter_choices = NULL
  )
  
  # Initialize filter choices on app startup
  observe({
    basic_choices <- get_basic_filter_choices()
    
    # Initialize facility choices
    if (length(basic_choices$facilities) > 0) {
      facility_choices <- c("All" = "all")
      facility_choices <- c(facility_choices, setNames(basic_choices$facility_codes, basic_choices$facilities))
      updateSelectInput(session, "facility_filter", choices = facility_choices, selected = "all")
    }
  })
  
  # Refresh data when button clicked - ONLY source of data loading
  observeEvent(input$refresh_data, {
    analysis_date <- input$analysis_date
    if (is.null(analysis_date)) analysis_date <- Sys.Date()
    
    current_year <- year(analysis_date)
    
    withProgress(message = "Loading cattail treatment data...", value = 0.5, {
      values$raw_data <- load_cattail_data(
        analysis_date = analysis_date,
        include_archive = TRUE,
        start_year = current_year - 2,
        end_year = current_year
      )
    })
    
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
        facility_filter = if("all" %in% input$facility_filter || is.null(input$facility_filter)) "all" else input$facility_filter
      )
      
      if (!is.null(values$filtered_data)) {
        values$aggregated_data <- aggregate_cattail_data(values$filtered_data, input$analysis_date)
      }
    }
  })
  
  # Create value boxes using aggregated data
  cattail_values <- reactive({
    if (is.null(values$aggregated_data) || is.null(values$aggregated_data$total_summary)) {
      return(list(
        total_sites = 0, total_acres = 0, sites_inspected = 0,
        sites_need_treatment = 0, sites_treated = 0, 
        percent_need_treatment = 0, percent_treated = 0
      ))
    }
    
    summary <- values$aggregated_data$total_summary
    
    list(
      sites_inspected = summary$inspected_sites,
      total_acres = round(summary$total_acres, 1),
      sites_under_threshold = summary$under_threshold_sites,
      sites_need_treatment = summary$need_treatment_sites,
      sites_treated = summary$treated_sites,  # This should now work!
      percent_need_treatment = summary$treatment_need_rate,
      percent_treated = summary$treatment_completion_rate
    )
  })
  
  # Value box outputs
  
  # Value box outputs for Progress tab
  
  output$active_treatments_box <- renderValueBox({
    metric_type <- if (is.null(input$display_metric_type)) "sites" else input$display_metric_type
    
    if (metric_type == "acres") {
      # Calculate acres needing treatment from aggregated data
      acres_val <- if (!is.null(values$aggregated_data$total_summary)) {
        values$aggregated_data$total_summary$need_treatment_acres
      } else {
        0
      }
      valueBox(
        value = format(round(acres_val, 1), big.mark = ",", nsmall = 1),
        subtitle = "Acres Need Treatment",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    } else {
      valueBox(
        value = cattail_values()$sites_need_treatment,
        subtitle = "Sites Need Treatment",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  output$treatment_coverage_box <- renderValueBox({
    metric_type <- if (is.null(input$display_metric_type)) "sites" else input$display_metric_type
    
    if (metric_type == "acres") {
      # Calculate % acres treated
      pct_val <- if (!is.null(values$aggregated_data$total_summary)) {
        need_acres <- values$aggregated_data$total_summary$need_treatment_acres
        treated_acres <- values$aggregated_data$total_summary$treated_acres
        if (need_acres > 0) {
          round(100 * treated_acres / need_acres, 1)
        } else {
          0
        }
      } else {
        0
      }
      valueBox(
        value = paste0(pct_val, "% Treated"),
        subtitle = "% Acres Treated", 
        icon = icon("chart-line"),
        color = "green"
      )
    } else {
      valueBox(
        value = paste0(cattail_values()$percent_treated, "% Treated"),
        subtitle = "% Sites Treated", 
        icon = icon("chart-line"),
        color = "green"
      )
    }
  })
  
  output$sites_inspected_box <- renderValueBox({
    metric_type <- if (is.null(input$display_metric_type)) "sites" else input$display_metric_type
    
    if (metric_type == "acres") {
      valueBox(
        value = format(cattail_values()$total_acres, big.mark = ",", nsmall = 1),
        subtitle = "Total Acres Inspected",
        icon = icon("clipboard-check"),
        color = "blue"
      )
    } else {
      valueBox(
        value = cattail_values()$sites_inspected,
        subtitle = "Total Sites Inspected",
        icon = icon("clipboard-check"),
        color = "blue"
      )
    }
  })
  
  output$under_threshold_box <- renderValueBox({
    metric_type <- if (is.null(input$display_metric_type)) "sites" else input$display_metric_type
    
    if (metric_type == "acres") {
      # Calculate acres under threshold from aggregated data
      acres_val <- if (!is.null(values$aggregated_data$total_summary)) {
        values$aggregated_data$total_summary$under_threshold_acres
      } else {
        0
      }
      valueBox(
        value = format(round(acres_val, 1), big.mark = ",", nsmall = 1),
        subtitle = "Acres Under Threshold",
        icon = icon("check-circle"),
        color = "light-blue"
      )
    } else {
      valueBox(
        value = cattail_values()$sites_under_threshold,
        subtitle = "Sites Under Threshold",
        icon = icon("check-circle"),
        color = "light-blue"
      )
    }
  })
  
  output$treated_sites_box <- renderValueBox({
    metric_type <- if (is.null(input$display_metric_type)) "sites" else input$display_metric_type
    
    if (metric_type == "acres") {
      # Calculate acres treated from aggregated data
      acres_val <- if (!is.null(values$aggregated_data$total_summary)) {
        values$aggregated_data$total_summary$treated_acres
      } else {
        0
      }
      valueBox(
        value = format(round(acres_val, 1), big.mark = ",", nsmall = 1),
        subtitle = "Acres Treated",
        icon = icon("check-double"),
        color = "green"
      )
    } else {
      valueBox(
        value = cattail_values()$sites_treated,
        subtitle = "Sites Treated",
        icon = icon("check-double"),
        color = "green"
      )
    }
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
    combine_zones <- input$zone_display %in% c("combined", "p1", "p2")
    create_treatment_progress_chart(values$aggregated_data, input$group_by, input$progress_chart_type, combine_zones)
  })
  output$timeline_chart <- renderPlotly({
    sites_data <- if (!is.null(values$aggregated_data) && !is.null(values$aggregated_data$sites_data)) {
      values$aggregated_data$sites_data
    } else {
      data.frame()
    }

    combine_zones <- input$zone_display %in% c("combined", "p1", "p2")
    metric_type <- if (is.null(input$display_metric_type)) "sites" else input$display_metric_type
    create_current_progress_chart(sites_data, input$group_by, input$progress_chart_type, combine_zones, metric_type, theme = current_theme())
  })
  
  # Charts that need implementation
  output$efficacy_chart <- renderPlotly({
    # Placeholder for future efficacy analysis
    ggplot() + geom_text(aes(x = 1, y = 1, label = "Efficacy Analysis - Coming Soon"), size = 6) + theme_void()
  })
  
  output$planning_calendar <- renderPlotly({
    # Placeholder for future planning calendar
    ggplot() + geom_text(aes(x = 1, y = 1, label = "Planning Calendar - Coming Soon"), size = 6) + theme_void()
  })
  
  # Reactive value to track if historical data should be loaded
  historical_data_ready <- reactiveVal(FALSE)
  
  # Load historical data only when refresh button is clicked
  observeEvent(input$refresh_historical, {
    historical_data_ready(TRUE)
  })
  
  output$historical_chart <- renderPlotly({
    # Don't load until refresh button is clicked at least once
    if (!historical_data_ready()) {
      return(ggplot() + 
             geom_text(aes(x = 1, y = 1, label = "Click 'Refresh Data' to load historical analysis"), size = 6) + 
             theme_void())
    }
    
    # Wrap in progress indicator
    withProgress(message = 'Loading historical data...', value = 0, {
      
      # After first refresh, chart will update automatically when inputs change
      chart_type <- input$chart_type
      hist_status_metric <- input$hist_status_metric
      year_range <- input$year_range
      
      incProgress(0.2, detail = "Reading parameters")
      
      # Debug: Log the input values to console
      cat("Historical Chart Rendering - Chart Type:", chart_type, "| Metric:", hist_status_metric, 
          "| Year Range:", if(!is.null(year_range)) paste(year_range, collapse="-") else "NULL", "\n")
      
      # Use defaults if inputs are NULL
      if (is.null(chart_type)) chart_type <- "line"
      if (is.null(hist_status_metric)) hist_status_metric <- "need_treatment"
      if (is.null(year_range)) {
        start_year <- year(Sys.Date()) - 4
        end_year <- year(Sys.Date())
      } else {
        start_year <- year_range[1]
        end_year <- year_range[2]
      }
      
      cat("  Using Years:", start_year, "to", end_year, "| Chart:", chart_type, "\n")
      
      incProgress(0.3, detail = "Querying database")
      
      # Convert years to dates - need to include fall of start_year through summer of end_year+1
      # Fall start_year is Sept 1 of start_year
      # Summer end_year ends Aug 1 of end_year+1
      start_date <- as.Date(paste0(start_year, "-09-01"))
      end_date <- as.Date(paste0(end_year + 1, "-08-01"))
      
      metric_type <- if (is.null(input$display_metric_type)) "sites" else input$display_metric_type
      
      incProgress(0.5, detail = "Creating chart")
      
      result <- create_historical_analysis_chart(
        values$raw_data, 
        group_by = input$group_by,
        time_period = "yearly",  # Always yearly for inspection year grouping
        chart_type = chart_type,
        display_metric = hist_status_metric,
        start_date = start_date,
        end_date = end_date,
        combine_zones = input$zone_display %in% c("combined", "p1", "p2"),
        metric_type = metric_type,
        theme = current_theme(),
        facility_filter = if("all" %in% input$facility_filter || is.null(input$facility_filter)) "all" else input$facility_filter,
        foreman_filter = if("all" %in% input$foreman_filter || is.null(input$foreman_filter)) "all" else input$foreman_filter
      )
      
      incProgress(1.0, detail = "Done")
      result
    })
  })
  
  # Table outputs
  # Tables that need implementation 
  output$treatments_table <- renderDT({
    # Placeholder - will be implemented when treatment tracking is added
    datatable(data.frame(Message = "Treatment tracking - Coming Soon"), 
              options = list(dom = 't'), rownames = FALSE)
  })
  
  output$plans_table <- renderDT({
    # Placeholder - will be implemented when planning module is added  
    datatable(data.frame(Message = "Treatment planning - Coming Soon"), 
              options = list(dom = 't'), rownames = FALSE)
  })
  
  output$status_table <- renderDT({
    sites_data <- if (!is.null(values$aggregated_data) && !is.null(values$aggregated_data$sites_data)) {
      values$aggregated_data$sites_data
    } else {
      data.frame()
    }
    
    treatments_data <- if (!is.null(values$filtered_data) && !is.null(values$filtered_data$treatments)) {
      values$filtered_data$treatments
    } else {
      data.frame()
    }
    
    create_status_table(sites_data, treatments_data, input$analysis_date)
  })
  
  # Map output
  output$treatment_map <- renderLeaflet({
    sites_data <- if (!is.null(values$filtered_data)) values$filtered_data$cattail_sites else data.frame()
    treatments_data <- if (!is.null(values$filtered_data) && !is.null(values$filtered_data$cattail_sites)) {
      values$filtered_data$cattail_sites %>% filter(state == "treated")
    } else {
      data.frame()
    }
    create_cattail_map(sites_data, treatments_data, input$basemap, theme = current_theme())
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
          Acres = acres
        ) %>%
        arrange(Facility, Sitecode)
      
      datatable(table_data, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
    } else {
      datatable(data.frame(Message = "No site data available"), options = list(dom = 't'), rownames = FALSE)
    }
  })
  
  # Summary statistics value boxes
  output$total_inspected_stat <- renderValueBox({
    stats <- cattail_values()
    valueBox(
      value = stats$sites_inspected,
      subtitle = "Sites Inspected",
      icon = icon("clipboard-check"),
      color = "light-blue"
    )
  })
  
  output$total_acres_stat <- renderValueBox({
    stats <- cattail_values()
    valueBox(
      value = paste(stats$total_acres, "ac"),
      subtitle = "Total Acres",
      icon = icon("ruler-combined"),
      color = "aqua"
    )
  })
  
  output$under_threshold_stat <- renderValueBox({
    stats <- cattail_values()
    valueBox(
      value = stats$sites_under_threshold,
      subtitle = "Under Threshold",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$need_treatment_stat <- renderValueBox({
    stats <- cattail_values()
    valueBox(
      value = stats$sites_need_treatment,
      subtitle = "Need Treatment",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$pct_need_treatment_stat <- renderValueBox({
    stats <- cattail_values()
    total_inspected <- stats$sites_inspected
    sites_needing_treatment <- stats$sites_need_treatment
    pct_need_treatment <- if (total_inspected > 0) round(100 * sites_needing_treatment / total_inspected, 1) else 0
    
    valueBox(
      value = paste0(pct_need_treatment, "%"),
      subtitle = "% Need Treatment (of inspected)",
      icon = icon("percentage"),
      color = "orange"
    )
  })
  
  output$pct_treated_stat <- renderValueBox({
    stats <- cattail_values()
    sites_needing_treatment <- stats$sites_need_treatment
    sites_treated <- stats$sites_treated
    sites_requiring_treatment <- sites_needing_treatment + sites_treated
    pct_treated_of_requiring <- if (sites_requiring_treatment > 0) round(100 * sites_treated / sites_requiring_treatment, 1) else 0
    
    valueBox(
      value = paste0(pct_treated_of_requiring, "%"),
      subtitle = "% Treated (of need treatment)",
      icon = icon("chart-pie"),
      color = "teal"
    )
  })
  
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