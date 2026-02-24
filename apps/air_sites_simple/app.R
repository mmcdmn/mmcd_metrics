# Air Sites Simple App
# Tracks Active Treatment vs Unknown status based on material effect days and Lab results

# Load shared libraries and utilities
source("../../shared/app_libraries.R")
source("../../shared/server_utilities.R")
source("../../shared/db_helpers.R")
source("../../shared/stat_box_helpers.R")

# Source app-specific function files
source("ui_helper.R")
source("data_functions.R")
source("display_functions.R")
source("historical_functions.R")

# Set application name for AWS RDS monitoring
set_app_name("air_sites_simple")

# =============================================================================
# STARTUP OPTIMIZATION: Preload lookup tables into cache
# =============================================================================
message("[air_sites_simple] Preloading lookup tables...")
tryCatch({
  get_facility_lookup()
  get_foremen_lookup()
  message("[air_sites_simple] Lookup tables preloaded")
}, error = function(e) message("[air_sites_simple] Preload warning: ", e$message))

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- air_sites_simple_ui()

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {

  # ===========================================================================
  # THEME SUPPORT
  # ===========================================================================

  current_theme <- reactive({ input$color_theme })

  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })

  # ===========================================================================
  # INITIALIZE FILTER CHOICES (once on startup)
  # ===========================================================================

  observe({
    facility_lookup <- get_facility_lookup()
    if (nrow(facility_lookup) > 0) {
      facility_choices <- c("All Facilities" = "all",
                            setNames(facility_lookup$short_name, facility_lookup$full_name))
    } else {
      facility_choices <- c("All Facilities" = "all")
    }
    updateSelectInput(session, "facility_filter", choices = facility_choices, selected = "all")

    priority_choices_raw <- get_priority_choices(include_all = FALSE)
    updateSelectizeInput(session, "priority_filter",
                         choices = c("All Priorities" = "all", priority_choices_raw),
                         selected = NULL)

    zone_choices <- get_available_zones()
    updateSelectInput(session, "zone_filter",
                      choices = setNames(zone_choices, zone_choices),
                      selected = zone_choices[1])

    material_choices <- get_material_choices(include_all = TRUE)
    if (length(material_choices) == 0) material_choices <- c("All Materials" = "all")
    updateSelectizeInput(session, "material_filter", choices = material_choices, selected = "all")
  })

  # Quick filter buttons
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

  # ===========================================================================
  # REFRESH BUTTON PATTERN - Capture all inputs when refresh clicked
  # ===========================================================================

  refresh_inputs <- eventReactive(input$refresh, {
    list(
      analysis_date = isolate(input$analysis_date),
      facility_filter = isolate(input$facility_filter),
      priority_filter = isolate(input$priority_filter),
      zone_filter = isolate(input$zone_filter),
      larvae_threshold = isolate(input$larvae_threshold),
      bti_effect_days_override = isolate(input$bti_effect_days_override),
      metric_type = isolate(input$metric_type),
      material_filter = isolate(input$material_filter),
      status_filter = isolate(input$status_filter),
      process_status_filter = isolate(input$process_status_filter),
      load_air_site_polygons = isolate(input$load_air_site_polygons)
    )
  })

  # Historical refresh - captures shared + historical-specific inputs
  hist_refresh_inputs <- eventReactive(input$hist_refresh, {
    list(
      facility_filter = isolate(input$facility_filter),
      priority_filter = isolate(input$priority_filter),
      zone_filter = isolate(input$zone_filter),
      larvae_threshold = isolate(input$larvae_threshold),
      metric_type = isolate(input$metric_type),
      hist_start_date = isolate(input$hist_start_date),
      hist_end_date = isolate(input$hist_end_date),
      hist_chart_type = isolate(input$hist_chart_type),
      volume_time_period = isolate(input$volume_time_period)
    )
  })

  # ===========================================================================
  # DATA LOADING AND PROCESSING
  # ===========================================================================

  # Raw data - single load when refresh clicked (no priority filter - shared by all)
  raw_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    withProgress(message = "Loading air sites data...", value = 0.5, {
      get_air_sites_data(
        analysis_date = inputs$analysis_date,
        facility_filter = inputs$facility_filter,
        priority_filter = NULL,
        zone_filter = inputs$zone_filter,
        larvae_threshold = inputs$larvae_threshold,
        bti_effect_days_override = inputs$bti_effect_days_override
      )
    })
  })

  # Status tab data - priority + status + material filters applied
  status_data <- reactive({
    req(input$refresh)
    data <- raw_data()
    if (nrow(data) == 0) return(data)
    inputs <- refresh_inputs()

    # Apply priority filter
    if (is_valid_filter(inputs$priority_filter)) {
      data <- data[data$priority %in% inputs$priority_filter, ]
    }

    # Apply status filter
    if (inputs$status_filter != "all") {
      data <- data[data$site_status == inputs$status_filter, ]
    }

    # Apply material filter for active treatments
    if (is_valid_filter(inputs$material_filter)) {
      active <- data[data$site_status == "Active Treatment" &
                     !is.na(data$matcode) &
                     data$matcode %in% inputs$material_filter, ]
      others <- data[data$site_status != "Active Treatment", ]
      data <- rbind(active, others)
    }

    data
  })

  # Process tab data - material filter only (all priorities included)
  process_data <- reactive({
    req(input$refresh)
    data <- raw_data()
    if (nrow(data) == 0) return(data)
    inputs <- refresh_inputs()

    # Apply material filter for active treatments
    if (is_valid_filter(inputs$material_filter)) {
      active <- data[data$site_status == "Active Treatment" &
                     !is.na(data$matcode) &
                     data$matcode %in% inputs$material_filter, ]
      others <- data[data$site_status != "Active Treatment", ]
      data <- rbind(active, others)
    }

    data
  })

  # Process chart data (status checkbox filter applied to flow chart)
  process_chart_data <- reactive({
    data <- process_data()
    if (nrow(data) == 0) return(data)
    inputs <- refresh_inputs()

    if (!is.null(inputs$process_status_filter) && length(inputs$process_status_filter) > 0) {
      data <- data[data$site_status %in% inputs$process_status_filter, ]
    }
    data
  })

  # ===========================================================================
  # AIR SITE STATUS TAB
  # ===========================================================================

  # Value boxes
  output$total_air_sites <- renderUI({
    req(input$refresh)
    data <- status_data()
    inputs <- refresh_inputs()
    metric_val <- if (nrow(data) == 0) 0 else if (inputs$metric_type == "acres") round(sum(data$acres, na.rm = TRUE), 1) else nrow(data)
    label <- if (inputs$metric_type == "acres") "Total Air Site Acres" else "Total Air Sites"
    formatted <- if (inputs$metric_type == "acres") paste0(format(metric_val, big.mark = ","), " ac") else format(metric_val, big.mark = ",")
    colors <- get_status_colors(theme = current_theme())
    create_stat_box(value = formatted, title = label, bg_color = colors[["completed"]], text_color = "#ffffff", icon = icon("helicopter"))
  })

  output$sites_unknown <- renderUI({
    req(input$refresh)
    data <- status_data()
    inputs <- refresh_inputs()
    subset <- data[data$site_status == "Unknown", ]
    metric_val <- if (nrow(subset) == 0) 0 else if (inputs$metric_type == "acres") round(sum(subset$acres, na.rm = TRUE), 1) else nrow(subset)
    label <- if (inputs$metric_type == "acres") "Not Insp Acres (in last 7 days)" else "Not Insp (in last 7 days)"
    formatted <- if (inputs$metric_type == "acres") paste0(format(metric_val, big.mark = ","), " ac") else format(metric_val, big.mark = ",")
    create_status_stat_box(value = formatted, title = label, status = "unknown", icon = icon("question-circle"), theme = current_theme())
  })

  output$sites_inspected <- renderUI({
    req(input$refresh)
    data <- status_data()
    inputs <- refresh_inputs()
    subset <- data[data$site_status == "Inspected", ]
    metric_val <- if (nrow(subset) == 0) 0 else if (inputs$metric_type == "acres") round(sum(subset$acres, na.rm = TRUE), 1) else nrow(subset)
    label <- if (inputs$metric_type == "acres") "Insp Acres (under threshold)" else "Insp (under threshold, last 7 days)"
    formatted <- if (inputs$metric_type == "acres") paste0(format(metric_val, big.mark = ","), " ac") else format(metric_val, big.mark = ",")
    create_status_stat_box(value = formatted, title = label, status = "completed", icon = icon("magnifying-glass"), theme = current_theme())
  })

  output$sites_in_lab <- renderUI({
    req(input$refresh)
    data <- status_data()
    inputs <- refresh_inputs()
    subset <- data[data$site_status == "Needs ID", ]
    metric_val <- if (nrow(subset) == 0) 0 else if (inputs$metric_type == "acres") round(sum(subset$acres, na.rm = TRUE), 1) else nrow(subset)
    label <- if (inputs$metric_type == "acres") "Needs ID Acres" else "Needs ID"
    formatted <- if (inputs$metric_type == "acres") paste0(format(metric_val, big.mark = ","), " ac") else format(metric_val, big.mark = ",")
    create_status_stat_box(value = formatted, title = label, status = "in_lab", icon = icon("microscope"), theme = current_theme())
  })

  output$sites_needs_treatment <- renderUI({
    req(input$refresh)
    data <- status_data()
    inputs <- refresh_inputs()
    subset <- data[data$site_status == "Needs Treatment", ]
    metric_val <- if (nrow(subset) == 0) 0 else if (inputs$metric_type == "acres") round(sum(subset$acres, na.rm = TRUE), 1) else nrow(subset)
    label <- if (inputs$metric_type == "acres") "Needs Treatment Acres" else "Needs Treatment"
    formatted <- if (inputs$metric_type == "acres") paste0(format(metric_val, big.mark = ","), " ac") else format(metric_val, big.mark = ",")
    create_status_stat_box(value = formatted, title = label, status = "needs_treatment", icon = icon("exclamation-triangle"), theme = current_theme())
  })

  output$sites_active_treatment <- renderUI({
    req(input$refresh)
    data <- status_data()
    inputs <- refresh_inputs()
    subset <- data[data$site_status == "Active Treatment", ]
    metric_val <- if (nrow(subset) == 0) 0 else if (inputs$metric_type == "acres") round(sum(subset$acres, na.rm = TRUE), 1) else nrow(subset)
    label <- if (inputs$metric_type == "acres") "Active Treatment Acres" else "Active Treatment"
    formatted <- if (inputs$metric_type == "acres") paste0(format(metric_val, big.mark = ","), " ac") else format(metric_val, big.mark = ",")
    create_status_stat_box(value = formatted, title = label, status = "active", icon = icon("check-circle"), theme = current_theme())
  })

  # Site Map
  output$status_map <- renderLeaflet({
    req(input$refresh)
    data <- status_data()
    inputs <- refresh_inputs()
    if (nrow(data) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = -93.2, lat = 44.9, zoom = 10))
    }
    withProgress(message = "Building map...", value = 0, {
      create_site_map(data, theme = current_theme(),
                      facility_filter = inputs$facility_filter,
                      load_air_site_polygons = inputs$load_air_site_polygons)
    })
  })

  # Zoom observer for air site polygons - show/hide based on zoom level
  observe({
    zoom <- input$status_map_zoom
    if (!is.null(zoom)) {
      proxy <- leafletProxy("status_map")
      if (zoom >= 13) {
        proxy %>% showGroup("Air Sites")
      } else {
        proxy %>% hideGroup("Air Sites")
      }
    }
  })

  # Status Chart
  output$status_chart <- renderPlotly({
    req(input$refresh)
    data <- status_data()
    inputs <- refresh_inputs()

    if (nrow(data) == 0) {
      return(plot_ly() %>%
        add_annotations(text = "No data available", x = 0.5, y = 0.5,
                        xref = "paper", yref = "paper", showarrow = FALSE))
    }

    # Create status summary
    if (inputs$metric_type == "acres") {
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

    # Map to display labels
    status_display_map <- c(
      "Unknown" = "Not Insp",
      "Inspected" = "Insp - Under Threshold",
      "Needs ID" = "Needs ID",
      "Needs Treatment" = "Needs Treatment",
      "Active Treatment" = "Active Treatment"
    )
    status_summary$display_status <- status_display_map[status_summary$site_status]

    status_order <- c("Not Insp", "Insp - Under Threshold", "Needs ID", "Needs Treatment", "Active Treatment")
    status_summary$display_status <- factor(status_summary$display_status, levels = status_order)

    status_color_map <- get_status_color_map(theme = current_theme())
    status_summary$color <- sapply(status_summary$site_status, function(s) {
      color <- as.character(status_color_map[[s]])
      if (is.null(color) || is.na(color) || color == "" || color == "NA") return("#808080")
      return(color)
    })

    plot_ly(status_summary, x = ~display_status, y = ~value, type = 'bar',
            marker = list(color = ~color)) %>%
      layout(
        title = list(text = "Site Status Distribution", font = list(size = 20)),
        xaxis = list(title = list(text = "Status", font = list(size = 18)), tickfont = list(size = 16)),
        yaxis = list(title = list(text = y_label, font = list(size = 18)), tickfont = list(size = 16)),
        showlegend = FALSE
      )
  })

  # Site Details Table
  output$site_details_table <- DT::renderDataTable({
    req(input$refresh)
    data <- status_data()
    if (nrow(data) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15)))

    display_data <- create_site_details_panel(data)

    DT::datatable(
      display_data,
      escape = FALSE,
      options = list(
        pageLength = 15,
        columnDefs = list(
          list(type = "num", targets = which(colnames(display_data) == "Larvae Count") - 1)
        )
      )
    ) %>%
      DT::formatStyle("Larvae Count", color = DT::styleInterval(c(0), c("black", "red")))
  }, server = FALSE)

  # ===========================================================================
  # PIPELINE SNAPSHOT TAB
  # ===========================================================================

  # Value boxes
  output$sites_receiving_treatment <- renderUI({
    req(input$refresh)
    data <- process_data()
    inputs <- refresh_inputs()
    subtitle_text <- if (inputs$metric_type == "acres") "Active Treatment (Acres)" else "Active Treatments"
    metrics <- create_treatment_efficiency_metrics(data, metric_type = inputs$metric_type)
    create_status_stat_box(value = metrics$sites_receiving_treatment, title = subtitle_text,
                           status = "active", icon = icon("check-circle"), theme = current_theme())
  })

  output$treatment_efficiency <- renderUI({
    req(input$refresh)
    data <- process_data()
    inputs <- refresh_inputs()
    metrics <- create_treatment_efficiency_metrics(data, metric_type = inputs$metric_type)
    create_status_stat_box(value = metrics$treatment_efficiency, title = "Treatment completion",
                           status = "completed", icon = icon("percent"), theme = current_theme())
  })

  output$treatment_rate <- renderUI({
    req(input$refresh)
    data <- process_data()
    inputs <- refresh_inputs()
    metrics <- create_treatment_efficiency_metrics(data, metric_type = inputs$metric_type)
    create_status_stat_box(value = metrics$treatment_rate, title = "% Need Treatment",
                           status = "needs_treatment", icon = icon("exclamation-triangle"), theme = current_theme())
  })

  output$inspection_coverage <- renderUI({
    req(input$refresh)
    data <- process_data()
    inputs <- refresh_inputs()
    metrics <- create_treatment_efficiency_metrics(data, metric_type = inputs$metric_type)
    create_status_stat_box(value = metrics$inspection_coverage, title = "Inspection Coverage",
                           status = "in_lab", icon = icon("search"), theme = current_theme())
  })

  output$total_inspected_samples <- renderUI({
    req(input$refresh)
    data <- process_data()
    lab_metrics <- analyze_lab_processing_metrics(data)
    create_status_stat_box(value = lab_metrics$total_inspected_with_samples, title = "Insp Samples",
                           status = "planned", icon = icon("vial"), theme = current_theme())
  })

  output$red_bug_detection_rate <- renderUI({
    req(input$refresh)
    data <- process_data()
    lab_metrics <- analyze_lab_processing_metrics(data)
    create_status_stat_box(value = lab_metrics$red_bug_detection_rate, title = "Red Bug Detection",
                           status = "needs_treatment", icon = icon("bug"), theme = current_theme())
  })

  # Treatment flow chart
  output$treatment_flow_chart <- renderPlotly({
    req(input$refresh)
    data <- process_chart_data()
    inputs <- refresh_inputs()

    if (nrow(data) == 0) {
      return(plot_ly() %>%
        add_annotations(text = "No data available - Click 'Refresh Data'",
                        x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE))
    }

    create_treatment_flow_chart(data, metric_type = inputs$metric_type, theme = current_theme())
  })

  # Process summary table
  output$process_summary_table <- DT::renderDataTable({
    req(input$refresh)
    data <- process_data()
    inputs <- refresh_inputs()
    if (nrow(data) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 10)))

    summary_data <- create_treatment_process_summary(data, metric_type = inputs$metric_type)

    DT::datatable(
      summary_data,
      options = list(pageLength = 10, scrollX = TRUE, searching = FALSE, paging = FALSE, info = FALSE),
      rownames = FALSE
    ) %>%
      DT::formatStyle("Treatment Rate",
                       backgroundColor = DT::styleInterval(cuts = c(50, 75), values = c("#f8d7da", "#fff3cd", "#d4edda")))
  }, server = FALSE)

  # Shared reactive for facility process details (used by table + download)
  facility_process_details <- reactive({
    data <- process_data()
    if (nrow(data) == 0) return(data.frame())
    details <- data %>%
      select(sitecode, facility, priority, site_status,
             last_inspection_date_display, last_larvae_count,
             last_treatment_date_display, last_treatment_material) %>%
      arrange(facility, site_status, sitecode)
    colnames(details) <- c("Site Code", "Facility", "Priority", "Status",
                            "Last Inspection", "Larvae Count", "Last Treatment", "Material")
    details
  })

  # Facility process details table
  output$facility_process_table <- DT::renderDataTable({
    req(input$refresh)
    process_details <- facility_process_details()
    if (nrow(process_details) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15)))

    DT::datatable(
      process_details,
      options = list(
        pageLength = 15, scrollX = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = c(1, 2, 3, 5)))
      ),
      rownames = FALSE, filter = 'top'
    ) %>%
      DT::formatStyle("Status",
                       backgroundColor = DT::styleEqual(
                         c("Active Treatment", "Needs Treatment", "Inspected", "Needs ID", "Unknown"),
                         c("#d4edda", "#f8d7da", "#d1ecf1", "#fff3cd", "#f8f9fa")
                       ))
  }, server = FALSE)

  # Shared reactive for treatment volume summary (used by table + download)
  treatment_volume_summary <- reactive({
    data <- treatment_volume_data()
    if (nrow(data) == 0) return(data.frame())
    data %>%
      group_by(facility, priority, year, operation_type) %>%
      summarise(operations = sum(total_operations, na.rm = TRUE),
                acres = round(sum(total_acres, na.rm = TRUE), 1), .groups = 'drop') %>%
      pivot_wider(names_from = operation_type, values_from = c(operations, acres), values_fill = 0) %>%
      select(Facility = facility, Priority = priority, Year = year,
             Treatments = operations_treatment, `Treatment Acres` = acres_treatment,
             Inspections = operations_inspection, `Inspection Acres` = acres_inspection) %>%
      arrange(desc(Year), Facility, Priority)
  })

  # ===========================================================================
  # HISTORICAL ANALYSIS TAB
  # ===========================================================================

  # Historical data (separate refresh button)
  comprehensive_historical_data <- eventReactive(input$hist_refresh, {
    inputs <- hist_refresh_inputs()
    withProgress(message = "Loading historical data...", value = 0.5, {
      get_comprehensive_historical_data(
        start_date = inputs$hist_start_date,
        end_date = inputs$hist_end_date,
        facility_filter = inputs$facility_filter,
        priority_filter = inputs$priority_filter,
        zone_filter = inputs$zone_filter,
        larvae_threshold = inputs$larvae_threshold
      )
    })
  })

  historical_data <- reactive({ comprehensive_historical_data()$inspection_summary })
  treatment_volume_data <- reactive({ comprehensive_historical_data()$treatment_volumes })

  # Historical value boxes
  output$hist_total_sites <- renderUI({
    req(input$hist_refresh)
    data <- historical_data()
    colors <- get_status_colors(theme = current_theme())
    create_stat_box(value = nrow(data), title = "Total Sites",
                    bg_color = colors[["completed"]], text_color = "#ffffff", icon = icon("map-marker"))
  })

  output$hist_total_inspections <- renderUI({
    req(input$hist_refresh)
    data <- historical_data()
    total <- sum(data$total_inspections, na.rm = TRUE)
    colors <- get_status_colors(theme = current_theme())
    create_stat_box(value = format(total, big.mark = ","), title = "Total Inspections",
                    bg_color = colors[["completed"]], text_color = "#ffffff", icon = icon("search"))
  })

  output$hist_total_red_bug_inspections <- renderUI({
    req(input$hist_refresh)
    data <- historical_data()
    total <- sum(data$red_bug_inspections, na.rm = TRUE)
    colors <- get_status_colors(theme = current_theme())
    create_stat_box(value = format(total, big.mark = ","), title = "Red Bug Inspections",
                    bg_color = colors[["needs_treatment"]], text_color = "#ffffff", icon = icon("bug"))
  })

  output$hist_overall_red_bug_ratio <- renderUI({
    req(input$hist_refresh)
    data <- historical_data()
    total_insp <- sum(data$total_inspections, na.rm = TRUE)
    total_red <- sum(data$red_bug_inspections, na.rm = TRUE)
    ratio <- if (total_insp > 0) round((total_red / total_insp) * 100, 1) else 0
    colors <- get_status_colors(theme = current_theme())
    create_stat_box(value = paste0(ratio, "%"), title = "% Inspections with Red Bugs",
                    bg_color = colors[["planned"]], text_color = "#ffffff", icon = icon("percent"))
  })

  output$hist_total_treatments <- renderUI({
    req(input$hist_refresh)
    data <- treatment_volume_data()
    treatments <- sum(data$total_operations[data$operation_type == 'treatment'], na.rm = TRUE)
    colors <- get_status_colors(theme = current_theme())
    create_stat_box(value = format(treatments, big.mark = ","), title = "Total Treatments",
                    bg_color = colors[["active"]], text_color = "#ffffff", icon = icon("spray-can"))
  })

  output$hist_total_treatment_acres <- renderUI({
    req(input$hist_refresh)
    data <- treatment_volume_data()
    acres <- sum(data$total_acres[data$operation_type == 'treatment'], na.rm = TRUE)
    colors <- get_status_colors(theme = current_theme())
    create_stat_box(value = format(round(acres, 1), big.mark = ","), title = "Treatment Acres",
                    bg_color = colors[["active"]], text_color = "#ffffff", icon = icon("area-chart"))
  })

  output$hist_total_inspection_acres <- renderUI({
    req(input$hist_refresh)
    data <- treatment_volume_data()
    acres <- sum(data$total_acres[data$operation_type == 'inspection'], na.rm = TRUE)
    colors <- get_status_colors(theme = current_theme())
    create_stat_box(value = format(round(acres, 1), big.mark = ","), title = "Inspection Acres",
                    bg_color = colors[["completed"]], text_color = "#ffffff", icon = icon("search"))
  })

  # Historical inspection table
  output$historical_inspection_table <- DT::renderDataTable({
    req(input$hist_refresh)
    data <- historical_data()
    if (nrow(data) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15)))

    display_data <- data
    colnames(display_data) <- c("Site Code", "Facility", "Priority", "Zone", "Acres",
                                 "Total Inspections", "Red Bug Inspections", "Red Bug Ratio (%)", "Years Active")

    # Link sitecodes to data.mmcd.org map
    display_data$`Site Code` <- make_sitecode_link(display_data$`Site Code`)

    DT::datatable(
      display_data,
      escape = FALSE,
      options = list(
        pageLength = 15, scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(1, 2, 3, 5, 6, 7)),
          list(className = 'dt-right', targets = 4)
        ),
        order = list(list(7, 'desc'))
      ),
      rownames = FALSE, filter = 'top'
    ) %>%
      DT::formatRound('Acres', 1) %>%
      DT::formatStyle("Red Bug Ratio (%)",
                       backgroundColor = DT::styleInterval(cuts = c(10, 25, 50), values = c("#d4edda", "#fff3cd", "#ffeeba", "#f8d7da"))) %>%
      DT::formatStyle("Total Inspections",
                       backgroundColor = DT::styleInterval(cuts = c(5, 20, 50), values = c("#f8f9fa", "#e2e3e5", "#d1ecf1", "#b8daff")))
  }, server = FALSE)

  # Treatment volume chart
  output$treatment_volume_chart <- renderPlotly({
    req(input$hist_refresh)
    data <- treatment_volume_data()
    inputs <- hist_refresh_inputs()

    if (nrow(data) == 0) {
      return(plot_ly() %>%
        add_annotations(text = "No data available - Click 'Refresh Historical Data'",
                        x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE))
    }

    create_treatment_volume_chart(data, inputs$volume_time_period,
                                  chart_type = inputs$hist_chart_type,
                                  metric_type = inputs$metric_type,
                                  theme = current_theme())
  })

  # Red bug trend chart
  output$red_bug_trend_chart <- renderPlotly({
    req(input$hist_refresh)
    data <- historical_data()
    inputs <- hist_refresh_inputs()

    if (nrow(data) == 0) {
      return(plot_ly() %>%
        add_annotations(text = "No historical data", x = 0.5, y = 0.5,
                        xref = "paper", yref = "paper", showarrow = FALSE))
    }

    trend_data <- data %>%
      mutate(
        primary_year = sapply(years_active, function(x) {
          if (grepl("-", x)) as.numeric(sub("-.*", "", x))
          else if (grepl(",", x)) as.numeric(sub(",.*", "", x))
          else as.numeric(gsub("[^0-9]", "", x))
        })
      ) %>%
      filter(!is.na(primary_year) & as.Date(paste0(primary_year, "-01-01")) >= inputs$hist_start_date) %>%
      group_by(facility, primary_year) %>%
      summarise(sites = n(), avg_red_bug_ratio = round(mean(red_bug_ratio, na.rm = TRUE), 1), .groups = 'drop') %>%
      filter(sites >= 3)

    if (nrow(trend_data) == 0) {
      return(plot_ly() %>%
        add_annotations(text = "Insufficient data for trends", x = 0.5, y = 0.5,
                        xref = "paper", yref = "paper", showarrow = FALSE))
    }

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
    req(input$hist_refresh)
    summary_data <- treatment_volume_summary()
    if (nrow(summary_data) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15)))

    DT::datatable(
      summary_data,
      options = list(
        pageLength = 15, scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(0, 1, 2, 3, 5)),
          list(className = 'dt-right', targets = c(4, 6))
        )
      ),
      rownames = FALSE, filter = 'top'
    ) %>%
      DT::formatStyle("Treatment Acres",
                       backgroundColor = DT::styleInterval(cuts = c(50, 200, 500), values = c("#f8f9fa", "#d1ecf1", "#b8daff", "#7fb3d3")))
  }, server = FALSE)

  # ===========================================================================
  # DOWNLOAD HANDLERS
  # ===========================================================================

  output$download_site_details <- downloadHandler(
    filename = function() { paste0("site_details_", Sys.Date(), ".csv") },
    content = function(file) {
      tryCatch({
        site_data <- status_data()
        if (is.null(site_data) || nrow(site_data) == 0) {
          write.csv(data.frame(Message = "No data available for selected filters"), file, row.names = FALSE)
        } else {
          result <- export_csv_safe(site_data, file, clean_data = TRUE)
          if (!result$success) write.csv(site_data, file, row.names = FALSE, na = "")
        }
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )

  output$download_process_summary <- downloadHandler(
    filename = function() { paste0("process_summary_", Sys.Date(), ".csv") },
    content = function(file) {
      tryCatch({
        data <- process_data()
        if (nrow(data) == 0) {
          write.csv(data.frame(Message = "No process data available"), file, row.names = FALSE)
          return()
        }
        inputs <- refresh_inputs()
        summary_data <- create_treatment_process_summary(data, metric_type = inputs$metric_type)
        result <- export_csv_safe(summary_data, file, clean_data = TRUE)
        if (!result$success) write.csv(summary_data, file, row.names = FALSE, na = "")
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )

  output$download_facility_process <- downloadHandler(
    filename = function() { paste0("facility_process_", Sys.Date(), ".csv") },
    content = function(file) {
      tryCatch({
        process_details <- facility_process_details()
        if (nrow(process_details) == 0) {
          write.csv(data.frame(Message = "No process data available"), file, row.names = FALSE)
          return()
        }
        result <- export_csv_safe(process_details, file, clean_data = TRUE)
        if (!result$success) write.csv(process_details, file, row.names = FALSE, na = "")
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )

  output$download_inspection_history <- downloadHandler(
    filename = function() { paste0("inspection_history_", Sys.Date(), ".csv") },
    content = function(file) {
      tryCatch({
        data <- historical_data()
        if (nrow(data) == 0) {
          write.csv(data.frame(Message = "No historical data available"), file, row.names = FALSE)
          return()
        }
        display_data <- data
        colnames(display_data) <- c("Site Code", "Facility", "Priority", "Zone", "Acres",
                                     "Total Inspections", "Red Bug Inspections", "Red Bug Ratio (%)", "Years Active")
        result <- export_csv_safe(display_data, file, clean_data = TRUE)
        if (!result$success) write.csv(display_data, file, row.names = FALSE, na = "")
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )

  output$download_treatment_volume <- downloadHandler(
    filename = function() { paste0("treatment_volume_", Sys.Date(), ".csv") },
    content = function(file) {
      tryCatch({
        summary_data <- treatment_volume_summary()
        if (nrow(summary_data) == 0) {
          write.csv(data.frame(Message = "No treatment volume data available"), file, row.names = FALSE)
          return()
        }
        result <- export_csv_safe(summary_data, file, clean_data = TRUE)
        if (!result$success) write.csv(summary_data, file, row.names = FALSE, na = "")
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )
}

# =============================================================================
# APPLICATION LAUNCH
# =============================================================================

shinyApp(ui = ui, server = server)
