# drone site Status App

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(DT)
  library(leaflet)
  library(sf)
  library(RColorBrewer)
})

# Source shared helper functions
source("../../shared/db_helpers.R")

# Source external function files
source("ui_helper.R")
source("data_functions.R")
source("display_functions.R")
source("historical_functions.R")

# Load environment variables
load_env_vars()

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- drone_ui()

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # Initialize UI options (facility and FOS choices) on app startup - NO DATA QUERIES
  observe({
    # Load facility choices from db_helpers
    facility_choices <- get_facility_choices()
    updateSelectizeInput(session, "facility_filter", choices = facility_choices, selected = "all")
    
    # Load foreman/FOS choices from db_helpers
    foreman_choices <- get_foreman_choices()
    updateSelectizeInput(session, "foreman_filter", choices = foreman_choices, selected = "all")
  })
  
  # Update group by options when switching to historical tabs
  observeEvent(input$tabs, {
    if (input$tabs %in% c("historical", "site_stats")) {
      if (input$group_by %in% c("sectcode", "mmcd_all")) {
        updateSelectInput(session, "group_by",
                         choices = c("Facility" = "facility", 
                                    "FOS" = "foreman",
                                    "All MMCD" = "mmcd_all"),
                         selected = "facility")
      } else {
        updateSelectInput(session, "group_by",
                         choices = c("Facility" = "facility", 
                                    "FOS" = "foreman",
                                    "All MMCD" = "mmcd_all"),
                         selected = input$group_by)
      }
    }
  })
  
  # =============================================================================
  # DATA LOADING AND PROCESSING
  # =============================================================================
  
  # Capture all input values when refresh button is clicked
  refresh_inputs <- eventReactive(input$refresh, {
    # Convert zone_option to zone_filter
    zone_filter <- switch(isolate(input$zone_option),
                          "p1_only" = "1",
                          "p2_only" = "2", 
                          "p1_p2_separate" = c("1", "2"),
                          "p1_p2_combined" = c("1", "2"))
    
    # Determine if we're combining P1 and P2
    combine_zones <- isolate(input$zone_option) == "p1_p2_combined"
    
    list(
      analysis_date = isolate(input$analysis_date),
      zone_filter = zone_filter,
      combine_zones = combine_zones,
      zone_option = isolate(input$zone_option),
      facility_filter = isolate(input$facility_filter),
      foreman_filter = isolate(input$foreman_filter),
      prehatch_only = isolate(input$prehatch_only),
      group_by = isolate(input$group_by),
      expiring_days = isolate(input$expiring_days),
      current_display_metric = isolate(input$current_display_metric),
      hist_time_period = isolate(input$hist_time_period),
      hist_chart_type = isolate(input$hist_chart_type),
      hist_display_metric = isolate(input$hist_display_metric),
      hist_start_year = isolate(input$hist_year_range[1]),
      hist_end_year = isolate(input$hist_year_range[2]),
      site_stat_type = isolate(input$site_stat_type),
      site_start_year = isolate(input$site_year_range[1]),
      site_end_year = isolate(input$site_year_range[2])
    )
  })
  
  # Raw data reactive - loads base data from database ONLY when refresh button is clicked
  # All inputs are captured at the moment of refresh button click
  raw_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    drone_types <- c("Y", "M", "C")  # Default drone types
    
    # Load raw data
    data <- load_raw_data(drone_types, analysis_date = inputs$analysis_date)
    
    # Apply filters immediately using captured input values
    filtered <- apply_data_filters(
      data = data,
      facility_filter = inputs$facility_filter,
      foreman_filter = inputs$foreman_filter,
      prehatch_only = inputs$prehatch_only
    )
    
    # Process current progress data using captured input values
    processed <- process_current_data(
      drone_sites = filtered$drone_sites,
      drone_treatments = filtered$drone_treatments,
      zone_filter = inputs$zone_filter,
      combine_zones = inputs$combine_zones,
      expiring_days = inputs$expiring_days,
      group_by = inputs$group_by,
      analysis_date = inputs$analysis_date
    )
    
    # Return everything needed
    list(
      raw = data,
      filtered = filtered,
      processed = processed
    )
  })
  
  # =============================================================================
  # CURRENT PROGRESS SECTION
  # =============================================================================
  
  # Access processed data from raw_data result
  processed_data <- reactive({
    raw_data()$processed
  })
  
  # Current progress plot
  output$currentPlot <- renderPlot({
    req(input$refresh)  # Only render after refresh button is clicked
    inputs <- refresh_inputs()
    result <- processed_data()
    data <- result$data
    sectcode_facility_mapping <- result$sectcode_facility_mapping
    
    if (nrow(data) == 0) {
      return(ggplot() + 
             geom_text(aes(x = 0.5, y = 0.5, label = "No data available"), size = 6) +
             theme_void())
    }
    
    # Determine variables and colors
    show_zones_separately <- inputs$zone_option == "p1_p2_separate"
    combine_zones <- inputs$combine_zones
    
    # Set x-axis and fill variables
    if (inputs$group_by == "mmcd_all") {
      x_var <- "display_name"
      fill_var <- "display_name"
    } else if (show_zones_separately) {
      x_var <- "display_name"
      if (inputs$group_by == "facility") {
        fill_var <- "facility_zone_key"
      } else {
        fill_var <- "combined_group"
      }
    } else {
      x_var <- "display_name"
      fill_var <- inputs$group_by
    }
    
    # Get colors
    custom_colors <- get_visualization_colors(
      group_by = inputs$group_by,
      data = data,
      show_zones_separately = show_zones_separately,
      zone_filter = inputs$zone_filter,
      for_historical = FALSE,
      sectcode_facility_mapping = sectcode_facility_mapping
    )
    
    # For zone-separated display, use modified colors to differentiate P1 vs P2
    # but keep the base colors recognizable
    if (show_zones_separately && !is.null(custom_colors)) {
      # Create zone-aware colors by adjusting brightness for P1 vs P2
      if (inputs$group_by == "facility") {
        # Get base facility colors
        base_facility_colors <- get_facility_base_colors()
        
        # Create colors for P1 (normal) and P2 (darker)
        zone_colors <- character()
        for (group_name in unique(data$combined_group)) {
          facility_code <- gsub(" \\(P[12]\\)", "", group_name)
          zone <- gsub(".*\\((P[12])\\)", "\\1", group_name)
          
          if (facility_code %in% names(base_facility_colors)) {
            base_color <- base_facility_colors[facility_code]
            # P1 = normal color, P2 = darker version (multiply RGB by 0.7)
            if (zone == "P1") {
              zone_colors[group_name] <- base_color
            } else {
              # Make P2 darker
              rgb_vals <- col2rgb(base_color)
              darker_color <- rgb(rgb_vals[1,1] * 0.7, rgb_vals[2,1] * 0.7, rgb_vals[3,1] * 0.7, maxColorValue = 255)
              zone_colors[group_name] <- darker_color
            }
          }
        }
        custom_colors <- zone_colors
      } else if (inputs$group_by == "foreman") {
        # Similar logic for foreman colors
        base_foreman_colors <- get_foreman_colors()
        foremen_lookup <- get_foremen_lookup()
        
        zone_colors <- character()
        for (group_name in unique(data$combined_group)) {
          foreman_part <- gsub(" \\(P[12]\\)", "", group_name)
          zone <- gsub(".*\\((P[12])\\)", "\\1", group_name)
          
          # Find foreman shortname from lookup
          if (foreman_part %in% foremen_lookup$shortname) {
            if (foreman_part %in% names(base_foreman_colors)) {
              base_color <- base_foreman_colors[foreman_part]
              # P1 = normal color, P2 = darker version
              if (zone == "P1") {
                zone_colors[group_name] <- base_color
              } else {
                rgb_vals <- col2rgb(base_color)
                darker_color <- rgb(rgb_vals[1,1] * 0.7, rgb_vals[2,1] * 0.7, rgb_vals[3,1] * 0.7, maxColorValue = 255)
                zone_colors[group_name] <- darker_color
              }
            }
          }
        }
        custom_colors <- zone_colors
      }
    }
    
    # Get status colors
    status_colors <- get_status_colors()
    
    # Add labels and formatting (BEFORE creating plot)
    metric_label <- case_when(
      inputs$current_display_metric == "sites" ~ "Number of Sites",
      inputs$current_display_metric == "treated_acres" ~ "Treated Acres",
      TRUE ~ "Count"
    )
    zone_text <- switch(inputs$zone_option,
                        "p1_only" = " (P1 Only)",
                        "p2_only" = " (P2 Only)",
                        "p1_p2_separate" = "",
                        "p1_p2_combined" = " (P1+P2 Combined)")
    prehatch_text <- ifelse(inputs$prehatch_only, " - Prehatch Only", "")
    
    group_label <- case_when(
      inputs$group_by == "facility" ~ "Facility",
      inputs$group_by == "foreman" ~ "FOS", 
      inputs$group_by == "sectcode" ~ "Section",
      inputs$group_by == "mmcd_all" ~ "All MMCD",
      TRUE ~ "Group"
    )
    
    # Add y variables based on metric
    if (inputs$current_display_metric == "sites") {
      data$y_total <- data$total_sites
      data$y_active <- data$active_sites
      data$y_expiring <- data$expiring_sites
    } else {  # treated_acres
      data$y_total <- data$total_treated_acres
      data$y_active <- data$active_acres
      data$y_expiring <- data$expiring_acres
    }
    
    # Create plot
    if (!is.null(custom_colors) && length(custom_colors) > 0 && inputs$group_by != "mmcd_all") {
      p <- ggplot(data, aes(x = .data[[x_var]], fill = !!sym(fill_var))) +
        geom_bar(aes(y = y_total), stat = "identity", alpha = 0.3) +
        geom_bar(aes(y = y_active), stat = "identity", alpha = 0.8) +
        geom_bar(aes(y = y_expiring), stat = "identity", fill = status_colors["planned"]) +
        scale_fill_manual(values = custom_colors, guide = "none")
    } else {
      # For MMCD grouping or when no specific colors available, use status colors only
      p <- ggplot(data, aes(x = .data[[x_var]])) +
        geom_bar(aes(y = y_total), stat = "identity", fill = "gray80", alpha = 0.7) +
        geom_bar(aes(y = y_active), stat = "identity", fill = status_colors["active"]) +
        geom_bar(aes(y = y_expiring), stat = "identity", fill = status_colors["planned"])
    }
    
    # Add formatting
    p <- p +
      labs(
        title = paste0("Drone Sites Progress by ", group_label, zone_text, prehatch_text),
        subtitle = paste0("Gray: Total sites, Blue: Active treatments, Orange: Expiring in ", inputs$expiring_days, " days"),
        x = group_label,
        y = metric_label
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
        plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(size = 13),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 11)
      )
    print(p)
  }, height = 900)
  
  # Current progress data table - show sitecode details
  # Current progress data table with dynamic sizing
  output$currentDataTable <- DT::renderDataTable({
    req(input$refresh)  # Only render after refresh button is clicked
    inputs <- refresh_inputs()
    
    # Load and process data like the plot does
    data <- load_raw_data(c("Y", "M", "C"), analysis_date = inputs$analysis_date)
    filtered <- apply_data_filters(
      data = data,
      facility_filter = inputs$facility_filter,
      foreman_filter = inputs$foreman_filter,
      prehatch_only = inputs$prehatch_only
    )
    
    if (nrow(filtered$drone_treatments) == 0) {
      return(DT::datatable(
        data.frame("No data available" = character(0)),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ))
    }
    
    # Get current date for active/expiring calculations
    current_date <- as.Date(inputs$analysis_date)
    expiring_start_date <- current_date
    expiring_end_date <- current_date + inputs$expiring_days
    
    # Calculate treatment status
    treatments_with_status <- filtered$drone_treatments %>%
      mutate(
        treatment_end_date = as.Date(inspdate) + ifelse(is.na(effect_days), 0, effect_days),
        is_active = treatment_end_date >= current_date,
        is_expiring = is_active & treatment_end_date >= expiring_start_date & treatment_end_date <= expiring_end_date,
        status = case_when(
          is_expiring ~ "Expiring",
          is_active ~ "Active", 
          TRUE ~ "Expired"
        )
      )
    
    # Create sitecode summary table - include ALL sites (active, expiring, AND expired)
    all_sites <- filtered$drone_sites
    
    # Get latest treatment for each site
    latest_treatments <- filtered$drone_treatments %>%
      group_by(sitecode) %>%
      arrange(desc(inspdate)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        treatment_end_date = as.Date(inspdate) + ifelse(is.na(effect_days), 0, effect_days),
        is_active = treatment_end_date >= current_date,
        is_expiring = is_active & treatment_end_date >= expiring_start_date & treatment_end_date <= expiring_end_date,
        status = case_when(
          is_expiring ~ "Expiring",
          is_active ~ "Active", 
          TRUE ~ "Expired"
        )
      ) %>%
      select(sitecode, inspdate, effect_days, status)
    
    # Join sites with their treatment status (include sites without treatments as expired)
    sitecode_table <- all_sites %>%
      left_join(latest_treatments, by = "sitecode") %>%
      mutate(
        status = ifelse(is.na(status), "No Treatment", status),
        last_treatment = ifelse(is.na(inspdate), "Never", as.character(as.Date(inspdate)))
      ) %>%
      # Count treatments per site
      left_join(
        filtered$drone_treatments %>% 
          group_by(sitecode) %>% 
          summarise(
            treatments = n(),
            treated_acres = sum(treated_acres, na.rm = TRUE),
            .groups = "drop"
          ), 
        by = "sitecode"
      ) %>%
      mutate(
        treatments = ifelse(is.na(treatments), 0, treatments),
        treated_acres = ifelse(is.na(treated_acres), 0, treated_acres)
      ) %>%
      arrange(desc(treated_acres), facility, sitecode) %>%
      head(100) %>%  # Increase limit to show more sites including expired ones
      rename(
        "Sitecode" = sitecode,
        "Facility" = facility,
        "Zone" = zone,
        "Treatments" = treatments,
        "Treated Acres" = treated_acres,
        "Last Treatment" = last_treatment,
        "Status" = status
      )
    
    DT::datatable(
      sitecode_table,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 1:5)
        )
      ),
      rownames = FALSE
    )
  })
  
  # =============================================================================
  # HISTORICAL TRENDS SECTION  
  # =============================================================================
  
  # Historical description
  output$historicalDescription <- renderText({
    req(input$refresh)
    inputs <- refresh_inputs()
    
    # Determine what's being shown
    metric_text <- case_when(
      inputs$hist_display_metric == "sites" ~ "sites",
      inputs$hist_display_metric == "treatments" ~ "treatments", 
      inputs$hist_display_metric == "acres" ~ "acres"
    )
    
    time_text <- if(inputs$hist_time_period == "weekly") "weekly" else "yearly"
    
    group_text <- case_when(
      inputs$group_by == "facility" ~ "by facility",
      inputs$group_by == "foreman" ~ "by Field Operations Supervisor (FOS)",
      inputs$group_by == "mmcd_all" ~ "district-wide"
    )
    
    chart_text <- case_when(
      inputs$hist_chart_type == "stacked_bar" ~ "as a stacked bar chart",
      inputs$hist_chart_type == "grouped_bar" ~ "as a grouped bar chart",
      inputs$hist_chart_type == "line" ~ "as a line chart",
      inputs$hist_chart_type == "area" ~ "as an area chart", 
      inputs$hist_chart_type == "step" ~ "as a step chart",
      TRUE ~ "as a chart"
    )
    
    # Filter information
    filter_parts <- c()
    if (!is.null(inputs$facility_filter) && !"all" %in% inputs$facility_filter) {
      filter_parts <- c(filter_parts, paste("facilities:", paste(inputs$facility_filter, collapse = ", ")))
    }
    if (!is.null(inputs$foreman_filter) && !"all" %in% inputs$foreman_filter) {
      filter_parts <- c(filter_parts, paste("FOS:", paste(inputs$foreman_filter, collapse = ", ")))
    }
    if (inputs$prehatch_only) {
      filter_parts <- c(filter_parts, "prehatch sites only")
    }
    
    filter_text <- if (length(filter_parts) > 0) {
      paste0(" (filtered by ", paste(filter_parts, collapse = "; "), ")")
    } else {
      ""
    }
    
    # Special description for weekly active treatments
    if (inputs$hist_time_period == "weekly" && inputs$hist_display_metric != "treatments") {
      paste0("Showing ", time_text, " drone ", metric_text, " with active treatments ", group_text, 
             " from ", inputs$hist_start_year, "-", inputs$hist_end_year, " ", chart_text,
             " (active on Fridays of each week)", filter_text, ".")
    } else {
      paste0("Showing ", time_text, " drone ", metric_text, " ", group_text, 
             " from ", inputs$hist_start_year, "-", inputs$hist_end_year, " ", chart_text, 
             filter_text, ".")
    }
  })
  
  # Historical plot output - uses external function
  output$historicalPlot <- renderPlot({
    req(input$refresh)  # Only render after refresh button is clicked
    inputs <- refresh_inputs()
    
    p <- create_historical_plot(
      zone_filter = inputs$zone_filter,
      combine_zones = inputs$combine_zones,
      zone_option = inputs$zone_option,
      group_by = inputs$group_by,
      hist_time_period = inputs$hist_time_period,
      hist_chart_type = inputs$hist_chart_type,
      hist_display_metric = inputs$hist_display_metric,
      prehatch_only = inputs$prehatch_only,
      hist_start_year = inputs$hist_start_year,
      hist_end_year = inputs$hist_end_year,
      drone_types = c("Y", "M", "C"),
      facility_filter = inputs$facility_filter,
      foreman_filter = inputs$foreman_filter,
      analysis_date = inputs$analysis_date
    ) +
      theme(
        axis.text.x = element_text(size = 14, face = "bold")
      )
    print(p)
  }, height = 900)
  
  # Historical data table
  output$historicalDataTable <- DT::renderDataTable({
    req(input$refresh)  # Only render after refresh button is clicked
    inputs <- refresh_inputs()
    
    historical_data <- get_historical_processed_data(
      hist_start_year = inputs$hist_start_year,
      hist_end_year = inputs$hist_end_year,
      drone_types = c("Y", "M", "C"),
      zone_filter = inputs$zone_filter,
      facility_filter = inputs$facility_filter,
      foreman_filter = inputs$foreman_filter,
      prehatch_only = inputs$prehatch_only,
      group_by = inputs$group_by,
      hist_time_period = inputs$hist_time_period,
      hist_display_metric = inputs$hist_display_metric,
      combine_zones = inputs$combine_zones,
      analysis_date = inputs$analysis_date
    )
    
    if (is.null(historical_data) || nrow(historical_data) == 0) {
      return(DT::datatable(
        data.frame("No data available" = character(0)),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ))
    }
    
    # Create a clean summary table
    table_data <- historical_data %>%
      rename(
        "Group" = display_name,
        "Time Period" = time_period,
        "Count" = count
      )
      
    DT::datatable(
      table_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )
      ),
      rownames = FALSE
    )
  })
  
  # =============================================================================
  # SITE STATISTICS SECTION
  # =============================================================================
  
  # Individual sitecode data for tables and stats - ONLY loads when refresh button is clicked
  # Capture all input values at moment of refresh
  sitecode_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    # Get input values with defaults
    start_year <- if(is.null(inputs$site_start_year)) 2018 else as.integer(inputs$site_start_year)
    end_year <- if(is.null(inputs$site_end_year)) 2025 else as.integer(inputs$site_end_year)
    
    # Get sitecode data
    sitecode_raw <- get_sitecode_data(
      start_year = start_year,
      end_year = end_year,
      zone_filter = inputs$zone_filter,
      facility_filter = inputs$facility_filter,
      foreman_filter = inputs$foreman_filter,
      prehatch_only = inputs$prehatch_only,
      analysis_date = inputs$analysis_date
    )
    
    # Get aggregated stats data immediately
    stats_data <- get_site_stats_data(
      sitecode_data_raw = sitecode_raw,
      zone_filter = inputs$zone_filter,
      combine_zones = inputs$combine_zones,
      group_by = inputs$group_by
    )
    
    # Return both
    list(
      raw = sitecode_raw,
      stats = stats_data
    )
  })
  
  # Site statistics data for plotting
  site_stats_data <- reactive({
    sitecode_data()$stats
  })
  
  # Site statistics plot
  output$siteStatsPlot <- renderPlot({
    req(input$refresh)  # Only render after refresh button is clicked
    inputs <- refresh_inputs()
    data <- site_stats_data()
    
    if (nrow(data) == 0) {
      return(ggplot() + 
             geom_text(aes(x = 0.5, y = 0.5, label = "No data available"), size = 6) +
             theme_void())
    }
    
    # Determine which metric to show and filter appropriately
    if (inputs$site_stat_type == "smallest") {
      # For smallest, filter out groups with zero minimum values
      data <- data %>% filter(min_site_acres > 0)
      y_var <- "min_site_acres"
      y_label <- "Minimum Treated Acres per Site"
      title_text <- "Smallest Drone Sites by Treated Acres"
    } else if (inputs$site_stat_type == "largest") {
      y_var <- "max_site_acres"
      y_label <- "Maximum Treated Acres per Site"
      title_text <- "Largest Drone Sites by Treated Acres"
    } else {
      y_var <- "avg_site_acres"
      y_label <- "Average Treated Acres per Site"
      title_text <- "Average Drone Site Size by Treated Acres"
    }
    
    # Check if we still have data after filtering
    if (nrow(data) == 0) {
      return(ggplot() + 
             geom_text(aes(x = 0.5, y = 0.5, label = "No data available"), size = 6) +
             theme_void())
    }
    
    # Get colors
    if (inputs$group_by == "mmcd_all") {
      # No special colors for MMCD grouping
      custom_colors <- NULL
    } else {
      group_col <- if(!inputs$combine_zones && length(inputs$zone_filter) > 1) "combined_group" else inputs$group_by
      custom_colors <- get_visualization_colors(
        group_by = inputs$group_by,
        data = data,
        show_zones_separately = !inputs$combine_zones && length(inputs$zone_filter) > 1,
        zone_filter = inputs$zone_filter,
        for_historical = FALSE,
        sectcode_facility_mapping = NULL
      )
    }
    
    # Create plot
    p <- ggplot(data, aes(x = reorder(display_name, !!sym(y_var)), y = !!sym(y_var))) +
      geom_bar(stat = "identity", aes(fill = display_name), show.legend = FALSE) +
      coord_flip() +
      labs(
        title = title_text,
        x = case_when(
          inputs$group_by == "facility" ~ "Facility",
          inputs$group_by == "foreman" ~ "FOS",
          inputs$group_by == "mmcd_all" ~ "All MMCD",
          TRUE ~ "Group"
        ),
        y = y_label
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 14)
      )
    
    # Add colors if available
    if (!is.null(custom_colors) && length(custom_colors) > 0) {
      p <- p + scale_fill_manual(values = custom_colors)
    }
    
    print(p)
  })
  
  # Largest sites table (individual treatments)
  output$largestSitesTable <- renderTable({
    req(input$refresh)  # Only render after refresh button is clicked
    data <- sitecode_data()$raw  # Use raw data from the list
    
    if (nrow(data) == 0) {
      return(data.frame("No data available" = character(0)))
    }
    
    # Add facility display names
    facilities <- get_facility_lookup()
    facility_map <- setNames(facilities$full_name, facilities$short_name)
    
    data %>%
      arrange(desc(acres)) %>%
      head(10) %>%
      mutate(
        facility_display = ifelse(
          facility %in% names(facility_map),
          facility_map[facility],
          facility
        ),
        zone_display = paste0("P", zone)
      ) %>%
      select(sitecode, acres, facility_display, zone_display, matcode, year) %>%
      rename("Sitecode" = sitecode, "Treated Acres" = acres, "Facility" = facility_display, "Zone" = zone_display, "Material" = matcode, "Year" = year)
  }, striped = TRUE, spacing = "xs")
  
  # Smallest sites table (individual treatments)
  output$smallestSitesTable <- renderTable({
    req(input$refresh)  # Only render after refresh button is clicked
    data <- sitecode_data()$raw  # Use raw data from the list
    
    if (nrow(data) == 0) {
      return(data.frame("No data available" = character(0)))
    }
    
    # Add facility display names
    facilities <- get_facility_lookup()
    facility_map <- setNames(facilities$full_name, facilities$short_name)
    
    data %>%
      arrange(acres) %>%
      head(10) %>%
      mutate(
        facility_display = ifelse(
          facility %in% names(facility_map),
          facility_map[facility],
          facility
        ),
        zone_display = paste0("P", zone)
      ) %>%
      select(sitecode, acres, facility_display, zone_display, matcode, year) %>%
      rename("Sitecode" = sitecode, "Treated Acres" = acres, "Facility" = facility_display, "Zone" = zone_display, "Material" = matcode, "Year" = year)
  }, striped = TRUE, spacing = "xs")
  
  # =============================================================================
  # MAP SECTION
  # =============================================================================
  
  # Map spatial data - loads when refresh button is clicked
  map_spatial_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    load_spatial_data(
      analysis_date = inputs$analysis_date,
      zone_filter = inputs$zone_filter,
      facility_filter = inputs$facility_filter,
      foreman_filter = inputs$foreman_filter,
      prehatch_only = inputs$prehatch_only,
      expiring_days = inputs$expiring_days
    )
  })
  
  # Map description
  output$mapDescription <- renderText({
    req(input$refresh)
    inputs <- refresh_inputs()
    
    # Filter information
    filter_parts <- c()
    if (!is.null(inputs$facility_filter) && !"all" %in% inputs$facility_filter) {
      filter_parts <- c(filter_parts, paste("facilities:", paste(inputs$facility_filter, collapse = ", ")))
    }
    if (!is.null(inputs$foreman_filter) && !"all" %in% inputs$foreman_filter) {
      filter_parts <- c(filter_parts, paste("FOS:", paste(inputs$foreman_filter, collapse = ", ")))
    }
    if (inputs$prehatch_only) {
      filter_parts <- c(filter_parts, "prehatch sites only")
    }
    
    filter_text <- if (length(filter_parts) > 0) {
      paste0(" (filtered by ", paste(filter_parts, collapse = "; "), ")")
    } else {
      ""
    }
    
    zone_text <- switch(inputs$zone_option,
                        "p1_only" = " in P1 zones",
                        "p2_only" = " in P2 zones", 
                        "p1_p2_separate" = " in P1 and P2 zones",
                        "p1_p2_combined" = " in P1 and P2 zones")
    
    paste0("Interactive map showing drone sites", zone_text, 
           ". Markers are colored by treatment status: Active (blue), Expiring within ", 
           inputs$expiring_days, " days (orange), Expired (red), No Treatment (gray)", 
           filter_text, ".")
  })
  
  # Leaflet map output
  output$droneMap <- renderLeaflet({
    req(input$refresh)
    spatial_data <- map_spatial_data()
    
    if (is.null(spatial_data) || nrow(spatial_data) == 0) {
      # Return empty map
      return(leaflet() %>%
        addTiles() %>%
        setView(lng = -93.2, lat = 44.9, zoom = 8))
    }
    
    # Define colors for treatment status
    status_colors <- c(
      "Active" = "#1f77b4",      # Blue
      "Expiring" = "#ff7f0e",    # Orange  
      "Expired" = "#d62728",     # Red
      "No Treatment" = "#7f7f7f" # Gray
    )
    
    # Create color palette function
    pal <- colorFactor(
      palette = status_colors,
      domain = spatial_data$treatment_status
    )
    
    # Get coordinates for map bounds
    coords <- st_coordinates(spatial_data)
    
    # Create popup content
    spatial_data$popup_text <- sprintf(
      "<strong>%s</strong><br/>
      Facility: %s<br/>
      Zone: P%s<br/>
      Acres: %.1f<br/>
      Status: %s<br/>
      %s",
      spatial_data$sitecode,
      spatial_data$facility,
      spatial_data$zone,
      spatial_data$acres,
      spatial_data$treatment_status,
      ifelse(is.na(spatial_data$last_treatment_date), 
             "No treatments recorded",
             sprintf("Last treated: %s<br/>Material: %s<br/>Treated acres: %.1f", 
                    spatial_data$last_treatment_date, 
                    spatial_data$last_material,
                    spatial_data$treated_acres))
    )
    
    # Create map
    leaflet(spatial_data) %>%
      addProviderTiles("OpenStreetMap") %>%
      addCircleMarkers(
        radius = 8,
        color = "#000000",
        weight = 1,
        opacity = 0.8,
        fillColor = ~pal(treatment_status),
        fillOpacity = 0.8,
        popup = ~popup_text
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~treatment_status,
        title = "Treatment Status",
        opacity = 0.8
      ) %>%
      fitBounds(
        lng1 = min(coords[,1]) - 0.01, lat1 = min(coords[,2]) - 0.01,
        lng2 = max(coords[,1]) + 0.01, lat2 = max(coords[,2]) + 0.01
      )
  })
  
  # Map data table
  output$mapDataTable <- DT::renderDataTable({
    req(input$refresh)
    spatial_data <- map_spatial_data()
    
    if (is.null(spatial_data) || nrow(spatial_data) == 0) {
      return(DT::datatable(
        data.frame("No data available" = character(0)),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ))
    }
    
    # Create clean table without geometry
    table_data <- spatial_data %>%
      st_drop_geometry() %>%
      arrange(desc(acres)) %>%
      select(sitecode, facility, zone, acres, treatment_status, last_treatment_date, last_material, treated_acres) %>%
      rename(
        "Sitecode" = sitecode,
        "Facility" = facility,
        "Zone" = zone,
        "Site Acres" = acres,
        "Status" = treatment_status,
        "Last Treatment" = last_treatment_date,
        "Material" = last_material,
        "Treated Acres" = treated_acres
      ) %>%
      mutate(
        Zone = paste0("P", Zone),
        `Last Treatment` = ifelse(is.na(`Last Treatment`), "Never", as.character(`Last Treatment`)),
        Material = ifelse(is.na(Material), "None", Material),
        `Treated Acres` = ifelse(is.na(`Treated Acres`), 0, `Treated Acres`)
      )
    
    DT::datatable(
      table_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 1:7)
        )
      ),
      rownames = FALSE
    )
  })
  
  # Download handlers for CSV exports
  output$download_current_data <- downloadHandler(
    filename = function() {
      paste0("drone_current_sites_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Get the same data used for the current data table
      inputs <- refresh_inputs()
      data <- load_raw_data(c("Y", "M", "C"), analysis_date = inputs$analysis_date)
      filtered <- apply_data_filters(
        data = data,
        facility_filter = inputs$facility_filter,
        foreman_filter = inputs$foreman_filter,
        prehatch_only = inputs$prehatch_only
      )
      
      if (nrow(filtered$drone_treatments) > 0) {
        # Get current date for active/expiring calculations
        current_date <- as.Date(inputs$analysis_date)
        expiring_start_date <- current_date
        expiring_end_date <- current_date + inputs$expiring_days
        
        # Calculate treatment status
        treatments_with_status <- filtered$drone_treatments %>%
          mutate(
            treatment_end_date = as.Date(inspdate) + ifelse(is.na(effect_days), 0, effect_days),
            is_active = treatment_end_date >= current_date,
            is_expiring = is_active & treatment_end_date >= expiring_start_date & treatment_end_date <= expiring_end_date,
            status = case_when(
              is_expiring ~ "Expiring",
              is_active ~ "Active", 
              TRUE ~ "Expired"
            )
          )
        
        # Create sitecode summary table
        all_sites <- filtered$drone_sites
        latest_treatments <- treatments_with_status %>%
          group_by(sitecode) %>%
          slice_max(inspdate, with_ties = FALSE) %>%
          ungroup()
        
        # Combine sites with treatments
        combined_data <- all_sites %>%
          left_join(latest_treatments %>%
                      select(sitecode, material, inspdate, status, treatment_end_date, treated_acres),
                    by = "sitecode") %>%
          mutate(
            status = ifelse(is.na(status), "Never Treated", status),
            material = ifelse(is.na(material), "None", material),
            inspdate = ifelse(is.na(inspdate), "Never", as.character(inspdate)),
            treatment_end_date = ifelse(is.na(treatment_end_date), "N/A", as.character(treatment_end_date)),
            treated_acres = ifelse(is.na(treated_acres), 0, treated_acres)
          )
        
        export_csv_safe(combined_data, file)
      } else {
        export_csv_safe(data.frame("No data available" = character(0)), file)
      }
    }
  )
  
  output$download_map_data <- downloadHandler(
    filename = function() {
      paste0("drone_map_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Get the same spatial data used for the map
      spatial_data <- map_spatial_data()
      
      if (!is.null(spatial_data) && nrow(spatial_data) > 0) {
        # Create clean table without geometry
        table_data <- spatial_data %>%
          st_drop_geometry() %>%
          arrange(desc(acres)) %>%
          select(sitecode, facility, zone, acres, treatment_status, last_treatment_date, last_material, treated_acres) %>%
          mutate(
            zone = paste0("P", zone),
            last_treatment_date = ifelse(is.na(last_treatment_date), "Never", as.character(last_treatment_date)),
            last_material = ifelse(is.na(last_material), "None", last_material),
            treated_acres = ifelse(is.na(treated_acres), 0, treated_acres)
          )
        
        export_csv_safe(table_data, file)
      } else {
        export_csv_safe(data.frame("No data available" = character(0)), file)
      }
    }
  )
  
  output$download_historical_data <- downloadHandler(
    filename = function() {
      paste0("drone_historical_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Get the same historical data used for the historical table
      inputs <- refresh_inputs()
      
      historical_data <- get_historical_processed_data(
        hist_start_year = inputs$hist_start_year,
        hist_end_year = inputs$hist_end_year,
        drone_types = c("Y", "M", "C"),
        zone_filter = inputs$zone_filter,
        facility_filter = inputs$facility_filter,
        foreman_filter = inputs$foreman_filter,
        prehatch_only = inputs$prehatch_only,
        group_by = inputs$group_by,
        hist_time_period = inputs$hist_time_period,
        hist_display_metric = inputs$hist_display_metric,
        combine_zones = inputs$combine_zones,
        analysis_date = inputs$analysis_date
      )
      
      if (!is.null(historical_data) && nrow(historical_data) > 0) {
        export_csv_safe(historical_data, file)
      } else {
        export_csv_safe(data.frame("No data available" = character(0)), file)
      }
    }
  )
}

# =============================================================================
# APPLICATION LAUNCH
# =============================================================================

# Run the application
shinyApp(ui = ui, server = server)
