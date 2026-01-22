# struct status App

# Load shared libraries and utilities
source("../../shared/app_libraries.R")
source("../../shared/server_utilities.R")
source("../../shared/db_helpers.R")
source("../../shared/stat_box_helpers.R")

# Source external function files
source("ui_helper.R")
source("data_functions.R")
source("display_functions.R")
source("historical_functions.R")

# Set application name for AWS RDS monitoring
set_app_name("struct_trt")

# Load environment variables from .env file
env_paths <- c(
  "../../.env",
  "../../../.env",
  "/srv/shiny-server/.env"
)

env_loaded <- FALSE
for (path in env_paths) {
  if (file.exists(path)) {
    readRenviron(path)
    env_loaded <- TRUE
    break
  }
}

ui <- struct_trt_ui()

server <- function(input, output, session) {
  
  # =============================================================================
  # INITIALIZE FILTERS
  # =============================================================================
  
  # Load foremen lookup table
  foremen_lookup <- get_foremen_lookup()
  
  # Initialize facility filter choices
  observe({
    facility_choices <- c("All Facilities" = "all", get_facility_choices())
    updateSelectInput(session, "facility_filter", 
                      choices = facility_choices,
                      selected = "all")
  })
  
  # Initialize FOS filter choices using existing function
  observe({
    fos_choices <- get_foreman_choices(include_all = TRUE)
    updateSelectizeInput(session, "foreman_filter",
                        choices = fos_choices,
                        selected = NULL)
  })
  
  # Update FOS choices when facility changes
  observeEvent(input$facility_filter, {
    if (!is.null(input$facility_filter)) {
      if (input$facility_filter == "all") {
        # Show all FOS using the existing function
        fos_choices <- get_foreman_choices(include_all = TRUE)
      } else {
        # Filter FOS by facility
        filtered_foremen <- foremen_lookup %>%
          filter(facility == input$facility_filter) %>%
          pull(shortname) %>%
          unique()
        
        # Create choices with shortname (facility) format
        filtered_display <- foremen_lookup %>%
          filter(facility == input$facility_filter) %>%
          mutate(display = paste0(shortname, " (", facility, ")")) %>%
          select(shortname, display) %>%
          distinct()
        
        fos_choices <- c("All FOS" = "all", setNames(filtered_display$shortname, filtered_display$display))
      }
      
      # Preserve current selection if it's still valid
      current_selection <- input$foreman_filter
      if (!is.null(current_selection)) {
        valid_selection <- current_selection[current_selection %in% c("all", fos_choices)]
        if (length(valid_selection) == 0) {
          valid_selection <- NULL
        }
      } else {
        valid_selection <- NULL
      }
      
      updateSelectizeInput(session, "foreman_filter",
                          choices = fos_choices,
                          selected = valid_selection)
    }
  })
  
  # =============================================================================
  # THEME HANDLING
  # =============================================================================
  
  current_theme <- reactive({
    input$color_theme
  })
  
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
  
  # =============================================================================
  # REFRESH BUTTON PATTERN - Capture all inputs when refresh clicked
  # =============================================================================
  
  refresh_inputs <- eventReactive(input$refresh, {
    zone_value <- isolate(input$zone_filter)
    
    # Parse zone filter
    parsed_zones <- if (zone_value == "combined") {
      c("1", "2")  # Include both zones but will be combined
    } else if (zone_value == "1,2") {
      c("1", "2")  # Include both zones separately
    } else {
      zone_value  # Single zone
    }
    
    # Get facility filter value (handle "all" case)
    facility_val <- isolate(input$facility_filter)
    if (is.null(facility_val) || facility_val == "all") {
      facility_val <- "all"
    }
    
    # Get foreman filter value with defensive handling
    foreman_val <- isolate(input$foreman_filter)
    if (is.null(foreman_val) || length(foreman_val) == 0 || "all" %in% foreman_val) {
      foreman_val <- "all"
    }
    
    list(
      zone_filter_raw = zone_value,
      zone_filter = parsed_zones,
      combine_zones = (zone_value == "combined"),
      expiring_days = isolate(input$expiring_days),
      custom_today = isolate(input$custom_today),
      status_types = isolate(input$status_types),
      facility_filter = facility_val,
      foreman_filter = foreman_val,
      group_by = isolate(input$group_by),
      structure_type_filter = isolate(input$structure_type_filter),
      priority_filter = "all",  # Default value since priority filter was removed from UI
      # Historical inputs
      hist_time_period = isolate(input$hist_time_period),
      hist_display_metric = if (isolate(input$hist_time_period) == "yearly") {
        if (!is.null(input$hist_display_metric_yearly)) {
          isolate(input$hist_display_metric_yearly)
        } else {
          "treatments"
        }
      } else {
        if (!is.null(input$hist_display_metric_weekly)) {
          isolate(input$hist_display_metric_weekly)
        } else {
          "weekly_active_treatments"
        }
      },
      hist_chart_type = if (isolate(input$hist_time_period) == "yearly" && 
                             !is.null(isolate(input$hist_display_metric_yearly)) &&
                             isolate(input$hist_display_metric_yearly) == "proportion") {
        if (!is.null(input$hist_chart_type_prop)) {
          isolate(input$hist_chart_type_prop)
        } else {
          "grouped_bar"
        }
      } else {
        if (!is.null(input$hist_chart_type_regular)) {
          isolate(input$hist_chart_type_regular)
        } else {
          "stacked_bar"
        }
      },
      hist_year_range = isolate(input$hist_year_range)
    )
  })
  
  # Load current data - ONLY when refresh button clicked
  current_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    withProgress(message = "Loading structure treatment data...", value = 0.5, {
      get_current_structure_data(
        analysis_date = inputs$custom_today,
        expiring_days = inputs$expiring_days,
        facility_filter = inputs$facility_filter,
        foreman_filter = inputs$foreman_filter,
        structure_type_filter = inputs$structure_type_filter,
        priority_filter = inputs$priority_filter,
        status_types = inputs$status_types,
        zone_filter = inputs$zone_filter
      )
    })
  })
  
  # Load all structures - ONLY when refresh button clicked
  all_structures <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    withProgress(message = "Loading structures...", value = 0.5, {
      get_all_structures(
        inputs$facility_filter,
        inputs$foreman_filter,
        inputs$structure_type_filter,
        inputs$priority_filter,
        inputs$status_types,
        inputs$zone_filter
      )
    })
  })
  
  # Load historical data - ONLY when refresh button clicked
  historical_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    withProgress(message = 'Loading historical data...', value = 0, {
      incProgress(0.3, detail = "Querying database...")
      
      hist_data <- create_historical_struct_data(
        start_year = inputs$hist_year_range[1],
        end_year = inputs$hist_year_range[2],
        hist_time_period = inputs$hist_time_period,
        hist_display_metric = inputs$hist_display_metric,
        hist_group_by = inputs$group_by,
        hist_zone_display = if (inputs$combine_zones) "combined" else "show-both",
        facility_filter = inputs$facility_filter,
        foreman_filter = inputs$foreman_filter,
        zone_filter = inputs$zone_filter,
        structure_type_filter = inputs$structure_type_filter,
        status_types = inputs$status_types
      )
      
      incProgress(0.7, detail = "Processing data...")
      
      return(hist_data)
    })
  })
  
  # Aggregate current data
  aggregated_current <- reactive({
    req(input$refresh)  # Require refresh button click
    inputs <- refresh_inputs()
    
    structures <- all_structures()
    treatments <- current_data()$treatments
    
    aggregate_structure_data(
      structures,
      treatments,
      inputs$group_by,
      inputs$zone_filter,
      inputs$combine_zones
    )
  })
  
  # Create value boxes for summary statistics
  value_boxes <- reactive({
    req(input$refresh)
    data <- aggregated_current()
    
    if (is.null(data) || nrow(data) == 0) {
      return(list(
        total_count = 0,
        active_count = 0,
        expiring_count = 0,
        active_pct = 0
      ))
    }
    
    total <- sum(data$total_count, na.rm = TRUE)
    active <- sum(data$active_count, na.rm = TRUE)
    expiring <- sum(data$expiring_count, na.rm = TRUE)
    active_pct <- if (total > 0) round(100 * active / total, 1) else 0
    
    list(
      total_count = total,
      active_count = active,
      expiring_count = expiring,
      active_pct = active_pct
    )
  })
  
  # Render stat boxes
  output$total_count_box <- renderUI({
    req(input$refresh)
    data <- value_boxes()
    status_colors <- get_status_colors(theme = current_theme())
    create_stat_box(
      value = format(data$total_count, big.mark = ","),
      title = "Total Structures",
      bg_color = status_colors["unknown"],
      icon = icon("building")
    )
  })
  
  output$active_count_box <- renderUI({
    req(input$refresh)
    data <- value_boxes()
    status_colors <- get_status_colors(theme = current_theme())
    create_stat_box(
      value = format(data$active_count, big.mark = ","),
      title = "Active Treatments",
      bg_color = status_colors["active"],
      icon = icon("check-circle")
    )
  })
  
  output$expiring_count_box <- renderUI({
    req(input$refresh)
    data <- value_boxes()
    status_colors <- get_status_colors(theme = current_theme())
    create_stat_box(
      value = format(data$expiring_count, big.mark = ","),
      title = "Expiring Soon",
      bg_color = status_colors["planned"],
      icon = icon("clock")
    )
  })
  
  output$active_pct_box <- renderUI({
    req(input$refresh)
    data <- value_boxes()
    status_colors <- get_status_colors(theme = current_theme())
    create_stat_box(
      value = paste0(data$active_pct, "%"),
      title = "Active %",
      bg_color = status_colors["active"],
      icon = icon("percent")
    )
  })
  
  # Render current progress chart
  output$structureGraph <- renderPlot({
    req(aggregated_current())  # Require data exists
    inputs <- refresh_inputs()
    
    # CRITICAL: Read theme to create reactive dependency
    current_theme_value <- input$color_theme
    
    cat("Current progress chart rendering with theme:", current_theme_value, "\n")
    
    data <- aggregated_current()
    
    create_current_progress_chart(
      data,
      inputs$group_by,
      inputs$facility_filter,
      inputs$status_types,
      inputs$zone_filter,
      inputs$combine_zones,
      theme = current_theme_value
    )
  })
  
  # Render historical trends chart
  output$historicalGraph <- renderPlotly({
    req(historical_data())  # Require data exists
    inputs <- refresh_inputs()
    
    # CRITICAL: Read theme to create reactive dependency
    current_theme_value <- input$color_theme
    
    cat("Historical chart rendering with theme:", current_theme_value, "\n")
    
    create_historical_struct_chart(
      data = historical_data(),
      hist_time_period = inputs$hist_time_period,
      hist_display_metric = inputs$hist_display_metric,
      hist_group_by = inputs$group_by,
      chart_type = inputs$hist_chart_type,
      theme = current_theme_value,
      show_zones_separately = !inputs$combine_zones
    )
  })
  
  # Current structure details table - simplified version
  output$currentStructureTable <- DT::renderDataTable({
    req(input$refresh)  # Only render after refresh button clicked
    
    all_structs <- all_structures()
    
    if (nrow(all_structs) == 0) {
      return(DT::datatable(
        data.frame("No data available" = character(0)),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ))
    }
    
    # Show structure data with better column names
    display_data <- all_structs %>%
      select(sitecode, facility, zone, s_type, foreman, status) %>%
      rename(
        "Sitecode" = sitecode,
        "Facility" = facility,
        "Zone" = zone,
        "Structure Type" = s_type,
        "FOS" = foreman,
        "Status" = status
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Historical structure data table
  output$historicalStructureTable <- DT::renderDataTable({
    req(input$refresh)  # Require refresh button click
    data <- historical_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(DT::datatable(
        data.frame("No data available" = character(0)),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ))
    }
    
    # Format the aggregated data for display
    table_data <- data %>%
      arrange(desc(time_period)) %>%
      select(time_period, group_name, count) %>%
      rename(
        "Time Period" = time_period,
        "Group" = group_name,
        "Count" = count
      )
    
    DT::datatable(
      table_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Download handlers for CSV exports
  output$download_current_data <- downloadHandler(
    filename = function() {
      paste0("structure_current_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      all_structs <- all_structures()
      
      if (nrow(all_structs) > 0) {
        # Export structure data
        export_csv_safe(all_structs, file)
      } else {
        export_csv_safe(data.frame("No data available" = character(0)), file)
      }
    }
  )
  
  output$download_historical_data <- downloadHandler(
    filename = function() {
      paste0("structure_historical_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- historical_data()
      
      if (!is.null(data) && nrow(data) > 0) {
        export_csv_safe(data, file)
      } else {
        export_csv_safe(data.frame("No data available" = character(0)), file)
      }
    }
  )
}

shinyApp(ui = ui, server = server)