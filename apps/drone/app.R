# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
})

# Source shared helper functions
source("../../shared/db_helpers.R")

# Source external function files
source("ui_helper.R")
source("data_functions.R")
source("display_functions.R")
source("historical_functions.R")
source("site_average_functions.R")

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
      if (input$group_by == "sectcode") {
        updateRadioButtons(session, "group_by",
                          choices = c("Facility" = "facility", 
                                     "FOS" = "foreman"),
                          selected = "facility")
      } else {
        updateRadioButtons(session, "group_by",
                          choices = c("Facility" = "facility", 
                                     "FOS" = "foreman"),
                          selected = input$group_by)
      }
    }
  })
  
  # =============================================================================
  # DATA LOADING AND PROCESSING
  # =============================================================================
  
  # Capture all input values when refresh button is clicked
  refresh_inputs <- eventReactive(input$refresh, {
    list(
      zone_filter = isolate(input$zone_filter),
      facility_filter = isolate(input$facility_filter),
      foreman_filter = isolate(input$foreman_filter),
      prehatch_only = isolate(input$prehatch_only),
      group_by = isolate(input$group_by),
      expiring_days = isolate(input$expiring_days),
      current_display_metric = isolate(input$current_display_metric),
      hist_display_metric = isolate(input$hist_display_metric),
      hist_start_year = isolate(input$hist_start_year),
      hist_end_year = isolate(input$hist_end_year),
      hist_show_percentages = isolate(input$hist_show_percentages),
      site_stat_type = isolate(input$site_stat_type),
      site_start_year = isolate(input$site_start_year),
      site_end_year = isolate(input$site_end_year)
    )
  })
  
  # Raw data reactive - loads base data from database ONLY when refresh button is clicked
  # All inputs are captured at the moment of refresh button click
  raw_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    drone_types <- c("Y", "M", "C")  # Default drone types
    
    # Load raw data
    data <- load_raw_data(drone_types)
    
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
      expiring_days = inputs$expiring_days,
      group_by = inputs$group_by
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
    show_zones_separately <- length(inputs$zone_filter) > 1
    
    # Set x-axis and fill variables
    if (show_zones_separately) {
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
    
    # Handle zone-separated color mapping for current progress
    if (show_zones_separately && !is.null(custom_colors)) {
      if (inputs$group_by == "facility") {
        # Use facility_zone_key for color mapping
        facility_result <- get_facility_base_colors(
          alpha_zones = inputs$zone_filter,
          combined_groups = unique(data$combined_group)
        )
        custom_colors <- facility_result$colors
        alpha_values <- facility_result$alpha_values
        
        # Create the plot with alpha mapping
        p_base <- ggplot(data, aes(x = .data[[x_var]], fill = .data[[fill_var]], alpha = zone_factor))
      } else if (inputs$group_by == "foreman") {
        # Use foreman colors with zone awareness
        foreman_result <- get_foreman_colors(
          alpha_zones = inputs$zone_filter,
          combined_groups = unique(data$combined_group)
        )
        custom_colors <- foreman_result$colors
        alpha_values <- foreman_result$alpha_values
        
        p_base <- ggplot(data, aes(x = .data[[x_var]], fill = .data[[fill_var]], alpha = zone_factor))
      } else {
        p_base <- ggplot(data, aes(x = .data[[x_var]], fill = .data[[fill_var]]))
      }
    } else {
      p_base <- ggplot(data, aes(x = .data[[x_var]], fill = .data[[fill_var]]))
    }
    
    # Get status colors
    status_colors <- get_status_colors()
    
    # Add labels and formatting (BEFORE creating plot)
    metric_label <- ifelse(inputs$current_display_metric == "sites", "Number of Sites", "Total Acres")
    zone_text <- if (length(inputs$zone_filter) == 2) "" else ifelse("1" %in% inputs$zone_filter, " (P1 Only)", " (P2 Only)")
    prehatch_text <- ifelse(inputs$prehatch_only, " - Prehatch Only", "")
    
    group_label <- case_when(
      inputs$group_by == "facility" ~ "Facility",
      inputs$group_by == "foreman" ~ "FOS", 
      inputs$group_by == "sectcode" ~ "Section",
      TRUE ~ "Group"
    )
    
    # Add y variables based on metric
    if (inputs$current_display_metric == "sites") {
      data$y_total <- data$total_sites
      data$y_active <- data$active_sites
      data$y_expiring <- data$expiring_sites
    } else {
      data$y_total <- data$total_acres
      data$y_active <- data$active_acres
      data$y_expiring <- data$expiring_acres
    }
    
    # Create plot
    if (!is.null(custom_colors) && inputs$group_by != "mmcd_all") {
      p <- ggplot(data, aes(x = .data[[x_var]], fill = !!sym(fill_var))) +
        geom_bar(aes(y = y_total), stat = "identity", alpha = 0.3) +
        geom_bar(aes(y = y_active), stat = "identity", alpha = 0.8) +
        geom_bar(aes(y = y_expiring), stat = "identity", fill = status_colors["planned"]) +
        scale_fill_manual(values = custom_colors, guide = "none")
    } else {
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
  
  # =============================================================================
  # HISTORICAL TRENDS SECTION  
  # =============================================================================
  
  # Historical plot output - uses external function
  output$historicalPlot <- renderPlot({
    req(input$refresh)  # Only render after refresh button is clicked
    inputs <- refresh_inputs()
    
    p <- create_historical_plot(
      zone_filter = inputs$zone_filter,
      group_by = inputs$group_by,
      hist_display_metric = inputs$hist_display_metric,
      prehatch_only = inputs$prehatch_only,
      hist_show_percentages = inputs$hist_show_percentages,
      hist_start_year = inputs$hist_start_year,
      hist_end_year = inputs$hist_end_year,
      drone_types = c("Y", "M", "C"),
      facility_filter = inputs$facility_filter,
      foreman_filter = inputs$foreman_filter
    ) +
      theme(
        axis.text.x = element_text(size = 14, face = "bold")
      )
    print(p)
  }, height = 900)
  
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
      prehatch_only = inputs$prehatch_only
    )
    
    # Get aggregated stats data immediately
    stats_data <- get_site_stats_data(
      sitecode_data_raw = sitecode_raw,
      zone_filter = inputs$zone_filter,
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
    group_col <- if(length(inputs$zone_filter) > 1) "combined_group" else inputs$group_by
    custom_colors <- get_visualization_colors(
      group_by = inputs$group_by,
      data = data,
      show_zones_separately = length(inputs$zone_filter) > 1,
      zone_filter = inputs$zone_filter,
      for_historical = FALSE,
      sectcode_facility_mapping = NULL
    )
    
    # Create plot
    p <- ggplot(data, aes(x = reorder(display_name, !!sym(y_var)), y = !!sym(y_var))) +
      geom_bar(stat = "identity", aes(fill = display_name), show.legend = FALSE) +
      coord_flip() +
      labs(
        title = title_text,
        x = case_when(
          inputs$group_by == "facility" ~ "Facility",
          inputs$group_by == "foreman" ~ "FOS",
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
        )
      ) %>%
      select(sitecode, acres, facility_display, matcode, year) %>%
      rename("Sitecode" = sitecode, "Treated Acres" = acres, "Facility" = facility_display, "Material" = matcode, "Year" = year)
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
      filter(acres > 0) %>%
      head(10) %>%
      mutate(
        facility_display = ifelse(
          facility %in% names(facility_map),
          facility_map[facility],
          facility
        )
      ) %>%
      select(sitecode, acres, facility_display, matcode, year) %>%
      rename("Sitecode" = sitecode, "Treated Acres" = acres, "Facility" = facility_display, "Material" = matcode, "Year" = year)
  }, striped = TRUE, spacing = "xs")
}

# =============================================================================
# APPLICATION LAUNCH
# =============================================================================

# Run the application
shinyApp(ui = ui, server = server)
