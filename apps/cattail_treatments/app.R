# Cattail Treatments Dashboard
# MMCD Cattail Treatment Planning and Management System

library(shiny)
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
source("../../shared/stat_box_helpers.R")
source("../../shared/server_utilities.R")

# Source external function files
source("data_functions.R")
source("display_functions.R")
source("historical_functions.R")
source("ui_helper.R")

# Define UI
ui <- cattail_treatments_ui()

# Define Server
server <- function(input, output, session) {
  
  # =============================================================================
  # INITIALIZE FILTERS
  # =============================================================================
  
  # Initialize facility filter choices
  observe({
    facility_choices <- c("All Facilities" = "all", get_facility_choices())
    updateSelectInput(session, "facility_filter",
                      choices = facility_choices,
                      selected = "all")
  })
  
  # =============================================================================
  # THEME HANDLING
  # =============================================================================
  
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
      updateSelectizeInput(session, "facility_filter", choices = facility_choices, selected = "all")
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
    req(input$refresh_data)  # Require refresh button click
    if (is.null(values$aggregated_data) || is.null(values$aggregated_data$total_summary)) {
      return(list(
        total_count = 0, total_acres = 0, sites_inspected = 0,
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
  
  output$active_treatments_box <- renderUI({
    req(input$refresh_data)
    metric_type <- if (is.null(input$display_metric_type)) "sites" else input$display_metric_type
    status_colors <- get_status_colors(theme = current_theme())
    
    if (metric_type == "acres") {
      # Calculate acres needing treatment from aggregated data
      acres_val <- if (!is.null(values$aggregated_data$total_summary)) {
        values$aggregated_data$total_summary$need_treatment_acres
      } else {
        0
      }
      create_stat_box(
        value = format(round(acres_val, 1), big.mark = ",", nsmall = 1),
        title = "Acres Need Treatment",
        bg_color = status_colors["planned"],
        icon = "exclamation-triangle"
      )
    } else {
      create_stat_box(
        value = format(cattail_values()$sites_need_treatment, big.mark = ","),
        title = "Sites Need Treatment",
        bg_color = status_colors["planned"],
        icon = "exclamation-triangle"
      )
    }
  })
  
  output$treatment_coverage_box <- renderUI({
    req(input$refresh_data)
    metric_type <- if (is.null(input$display_metric_type)) "sites" else input$display_metric_type
    status_colors <- get_status_colors(theme = current_theme())
    
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
      create_stat_box(
        value = paste0(pct_val, "%"),
        title = "% Acres Treated",
        bg_color = status_colors["active"],
        icon = "percent"
      )
    } else {
      create_stat_box(
        value = paste0(cattail_values()$percent_treated, "%"),
        title = "% Sites Treated",
        bg_color = status_colors["active"],
        icon = "percent"
      )
    }
  })
  
  output$sites_inspected_box <- renderUI({
    req(input$refresh_data)
    metric_type <- if (is.null(input$display_metric_type)) "sites" else input$display_metric_type
    status_colors <- get_status_colors(theme = current_theme())
    
    if (metric_type == "acres") {
      create_stat_box(
        value = format(cattail_values()$total_acres, big.mark = ",", nsmall = 1),
        title = "Total Acres Inspected",
        bg_color = status_colors["completed"],
        icon = "clipboard-check"
      )
    } else {
      create_stat_box(
        value = format(cattail_values()$sites_inspected, big.mark = ","),
        title = "Total Sites Inspected",
        bg_color = status_colors["completed"],
        icon = "clipboard-check"
      )
    }
  })
  
  output$under_threshold_box <- renderUI({
    req(input$refresh_data)
    metric_type <- if (is.null(input$display_metric_type)) "sites" else input$display_metric_type
    status_colors <- get_status_colors(theme = current_theme())
    
    if (metric_type == "acres") {
      # Calculate acres under threshold from aggregated data
      acres_val <- if (!is.null(values$aggregated_data$total_summary)) {
        values$aggregated_data$total_summary$under_threshold_acres
      } else {
        0
      }
      create_stat_box(
        value = format(round(acres_val, 1), big.mark = ",", nsmall = 1),
        title = "Acres Under Threshold",
        bg_color = status_colors["unknonwn"],
        icon = "check-circle"
      )
    } else {
      create_stat_box(
        value = format(cattail_values()$sites_under_threshold, big.mark = ","),
        title = "Sites Under Threshold",
        bg_color = status_colors["unknown"],
        icon = "check-circle"
      )
    }
  })
  
  output$treated_sites_box <- renderUI({
    req(input$refresh_data)
    metric_type <- if (is.null(input$display_metric_type)) "sites" else input$display_metric_type
    status_colors <- get_status_colors(theme = current_theme())
    
    if (metric_type == "acres") {
      # Calculate acres treated from aggregated data
      acres_val <- if (!is.null(values$aggregated_data$total_summary)) {
        values$aggregated_data$total_summary$treated_acres
      } else {
        0
      }
      create_stat_box(
        value = format(round(acres_val, 1), big.mark = ",", nsmall = 1),
        title = "Acres Treated",
        bg_color = status_colors["active"],
        icon = "check-double"
      )
    } else {
      create_stat_box(
        value = format(cattail_values()$sites_treated, big.mark = ","),
        title = "Sites Treated",
        bg_color = status_colors["active"],
        icon = "check-double"
      )
    }
  })
  
  output$upcoming_plans_box <- renderUI({
    req(input$refresh_data)
    status_colors <- get_status_colors(theme = current_theme())
    # Calculate upcoming plans from filtered data
    plans_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatment_plans else data.frame()
    upcoming_plans <- if (nrow(plans_data) > 0) {
      sum(plans_data$plan_status %in% c("Due This Week", "Due This Month"), na.rm = TRUE)
    } else {
      0
    }
    
    create_stat_box(
      value = format(upcoming_plans, big.mark = ","),
      title = "Upcoming Plans",
      bg_color = status_colors["planned"],
      icon = "calendar-check"
    )
  })
  
  output$total_plans_box <- renderUI({
    req(input$refresh_data)
    status_colors <- get_status_colors(theme = current_theme())
    # Calculate total plans from filtered data
    plans_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatment_plans else data.frame()
    total_plans <- nrow(plans_data)
    
    create_stat_box(
      value = format(total_plans, big.mark = ","),
      title = "Total Plans",
      bg_color = status_colors["unknown"],
      icon = "list-alt"
    )
  })
  
  output$overdue_plans_box <- renderUI({
    req(input$refresh_data)
    status_colors <- get_status_colors(theme = current_theme())
    # Calculate overdue plans from filtered data
    plans_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatment_plans else data.frame()
    overdue_plans <- if (nrow(plans_data) > 0) {
      sum(plans_data$plan_status == "Overdue", na.rm = TRUE)
    } else {
      0
    }
    
    create_stat_box(
      value = format(overdue_plans, big.mark = ","),
      title = "Overdue Plans",
      bg_color = status_colors["needs_treatment"],
      icon = "exclamation-triangle"
    )
  })
  
  output$upcoming_week_box <- renderUI({
    req(input$refresh_data)
    status_colors <- get_status_colors(theme = current_theme())
    plans_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatment_plans else data.frame()
    upcoming_week <- if (nrow(plans_data) > 0) sum(plans_data$plan_status == "Due This Week", na.rm = TRUE) else 0
    
    create_stat_box(
      value = format(upcoming_week, big.mark = ","),
      title = "Due This Week",
      bg_color = status_colors["planned"],
      icon = "clock"
    )
  })
  
  output$upcoming_month_box <- renderUI({
    req(input$refresh_data)
    status_colors <- get_status_colors(theme = current_theme())
    plans_data <- if (!is.null(values$filtered_data)) values$filtered_data$treatment_plans else data.frame()
    upcoming_month <- if (nrow(plans_data) > 0) sum(plans_data$plan_status == "Due This Month", na.rm = TRUE) else 0
    
    create_stat_box(
      value = format(upcoming_month, big.mark = ","),
      title = "Due This Month",
      bg_color = status_colors["planned"],
      icon = "calendar"
    )
  })
  
  # Chart outputs
  output$progress_chart <- renderPlotly({
    req(input$refresh_data)  # Require refresh button click
    sites_data <- if (!is.null(values$aggregated_data) && !is.null(values$aggregated_data$sites_data)) {
      values$aggregated_data$sites_data
    } else {
      data.frame()
    }
    combine_zones <- input$zone_display %in% c("combined", "p1", "p2")
    metric_type <- if (is.null(input$display_metric_type)) "sites" else input$display_metric_type
    create_current_progress_chart(sites_data, input$group_by, input$progress_chart_type, combine_zones, metric_type, theme = current_theme())
  })
  output$timeline_chart <- renderPlotly({
    req(input$refresh_data)  # Require refresh button click
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
    req(input$refresh_data)  # Require refresh button click
    # Placeholder for future efficacy analysis
    ggplot() + geom_text(aes(x = 1, y = 1, label = "Efficacy Analysis - Coming Soon"), size = 6) + theme_void()
  })
  
  # Reactive value to track if historical data should be loaded
  historical_data_ready <- reactiveVal(FALSE)
  
  # Load historical data only when refresh button is clicked
  observeEvent(input$refresh_historical, {
    historical_data_ready(TRUE)
  })
  
  output$historical_chart <- renderPlotly({
    # Only trigger when button is clicked
    req(input$refresh_historical)
    
    # Don't load until refresh button is clicked at least once
    if (!isolate(historical_data_ready())) {
      return(ggplot() + 
             geom_text(aes(x = 1, y = 1, label = "Click 'Refresh Data' to load historical analysis"), size = 6) + 
             theme_void())
    }
    
    # Wrap in progress indicator
    withProgress(message = 'Loading historical data...', value = 0, {
      
      # Isolate ALL inputs so they don't trigger auto-refresh
      chart_type_raw <- isolate(input$hist_chart_type)
      hist_status_metric <- isolate(input$hist_status_metric)
      year_range <- isolate(input$year_range)
      display_metric_type <- isolate(input$display_metric_type)
      group_by <- isolate(input$group_by)
      zone_display <- isolate(input$zone_display)
      facility_filter <- isolate(input$facility_filter)
      foreman_filter <- isolate(input$foreman_filter)
      color_theme <- isolate(input$color_theme)
      
      # Map UI chart type values to function expected values
      chart_type <- switch(chart_type_raw,
        "stacked_bar" = "stacked",
        "grouped_bar" = "bar",
        "line" = "line",
        "area" = "area",
        "line"  # default
      )
      
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
      
      metric_type <- if (is.null(display_metric_type)) "sites" else display_metric_type
      
      incProgress(0.5, detail = "Creating chart")
      
      result <- create_historical_analysis_chart(
        values$raw_data, 
        group_by = group_by,
        time_period = "yearly",  # Always yearly for inspection year grouping
        chart_type = chart_type,
        display_metric = hist_status_metric,
        start_date = start_date,
        end_date = end_date,
        combine_zones = zone_display %in% c("combined", "p1", "p2"),
        metric_type = metric_type,
        theme = color_theme,
        facility_filter = if("all" %in% facility_filter || is.null(facility_filter)) "all" else facility_filter,
        foreman_filter = if("all" %in% foreman_filter || is.null(foreman_filter)) "all" else foreman_filter
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
  output$total_inspected_stat <- renderUI({
    stats <- cattail_values()
    create_metric_box(
      value = stats$sites_inspected,
      subtitle = "Sites Inspected",
      icon = "clipboard-check",
      color = "primary"
    )
  })
  
  output$total_acres_stat <- renderUI({
    stats <- cattail_values()
    create_metric_box(
      value = paste(stats$total_acres, "ac"),
      subtitle = "Total Acres",
      icon = "ruler-combined",
      color = "info"
    )
  })
  
  output$under_threshold_stat <- renderUI({
    stats <- cattail_values()
    create_metric_box(
      value = stats$sites_under_threshold,
      subtitle = "Under Threshold",
      icon = "check-circle",
      color = "success"
    )
  })
  
  output$need_treatment_stat <- renderUI({
    stats <- cattail_values()
    create_metric_box(
      value = stats$sites_need_treatment,
      subtitle = "Need Treatment",
      icon = "exclamation-triangle",
      color = "danger"
    )
  })
  
  output$pct_need_treatment_stat <- renderUI({
    stats <- cattail_values()
    total_inspected <- stats$sites_inspected
    sites_needing_treatment <- stats$sites_need_treatment
    pct_need_treatment <- if (total_inspected > 0) round(100 * sites_needing_treatment / total_inspected, 1) else 0
    
    create_metric_box(
      value = paste0(pct_need_treatment, "%"),
      subtitle = "% Need Treatment (of inspected)",
      icon = "percentage",
      color = "warning"
    )
  })
  
  output$pct_treated_stat <- renderUI({
    stats <- cattail_values()
    sites_needing_treatment <- stats$sites_need_treatment
    sites_treated <- stats$sites_treated
    sites_requiring_treatment <- sites_needing_treatment + sites_treated
    pct_treated_of_requiring <- if (sites_requiring_treatment > 0) round(100 * sites_treated / sites_requiring_treatment, 1) else 0
    
    create_metric_box(
      value = paste0(pct_treated_of_requiring, "%"),
      subtitle = "% Treated (of need treatment)",
      icon = "chart-pie",
      color = "info"
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