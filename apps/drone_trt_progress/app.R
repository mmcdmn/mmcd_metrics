# =============================================================================
# DRONE TREATMENT PROGRESS DASHBOARD
# =============================================================================
# This application shows current progress and historical trends for drone 
# treatment sites with filtering and grouping capabilities.
# 
# Features:
# - Current progress view with active/expiring treatments
# - Historical trends analysis
# - Site average historical analysis
# - Multi-level filtering (zone, facility, FOS, prehatch)
# - Dynamic grouping options
# - Zone-aware color coding and alpha transparency
# =============================================================================

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

# Source historical functions (EXACT copy from backup)
source("historical_functions.R")

# Load environment variables
load_env_vars()

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Load raw data from database
#' @param drone_types Vector of drone type selections
#' @return List with drone_sites and drone_treatments data frames
load_raw_data <- function(drone_types = c("Y", "M", "C")) {
  con <- get_db_connection()
  if (is.null(con)) return(list(drone_sites = data.frame(), drone_treatments = data.frame()))
  
  # Build the drone designation filter based on user selection
    drone_types_str <- paste0("'", paste(drone_types, collapse = "','"), "'")
    
    # Query to get drone sites from loc_breeding_sites
    drone_sites_query <- sprintf("
    SELECT b.sitecode, b.facility, b.acres, b.prehatch, b.drone, 
           CASE 
             WHEN e.emp_num IS NOT NULL AND e.active = true THEN sc.fosarea
             ELSE NULL
           END as foreman, 
           sc.zone,
           left(b.sitecode,7) as sectcode
    FROM public.loc_breeding_sites b
    LEFT JOIN public.gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
    LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
      AND e.emp_type = 'FieldSuper' 
      AND e.active = true
    WHERE (b.drone IN (%s) OR b.air_gnd = 'D')
    AND b.enddate IS NULL
    ", drone_types_str)
    
    drone_sites <- dbGetQuery(con, drone_sites_query)
    
    # Query to get treatment information
    treatments_query <- "
    SELECT t.sitecode, t.facility, t.inspdate, t.matcode, t.acres, t.foreman, m.effect_days
    FROM public.dblarv_insptrt_current t
    LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
    WHERE (t.airgrnd_plan = 'D' OR t.action = 'D')
    "
    
    treatments <- dbGetQuery(con, treatments_query)
    
    dbDisconnect(con)
    
    # Process the data
    # Identify drone sites with treatments
    drone_treatments <- treatments %>%
      inner_join(drone_sites, by = c("sitecode", "facility"), suffix = c("_trt", "_site")) %>%
      mutate(
        # Use treatment acres if available, otherwise use site acres
        acres = ifelse(is.na(acres_trt), acres_site, acres_trt),
        # Use site foreman (jurisdictional assignment). Ignore treatment foreman
        foreman = foreman_site,
        prehatch = prehatch,
        drone = drone,
        zone = zone,
        sectcode = sectcode
      )
    
    # Calculate treatment status (active)
    current_date <- Sys.Date()
    drone_treatments <- drone_treatments %>%
      mutate(
        inspdate = as.Date(inspdate),
        effect_days = ifelse(is.na(effect_days), 0, effect_days),
        treatment_end_date = inspdate + effect_days,
        is_active = treatment_end_date >= current_date
      )
    
    # Return all the data needed for filtering later
    return(list(
      drone_sites = drone_sites,
      drone_treatments = drone_treatments
    ))
}

#' Apply filters to drone data
#' @param data List containing drone_sites and drone_treatments
#' @param facility_filter Vector of selected facilities  
#' @param foreman_filter Vector of selected foremen
#' @param prehatch_only Boolean for prehatch filter
#' @return Filtered data list
apply_data_filters <- function(data, facility_filter = NULL, 
                               foreman_filter = NULL, prehatch_only = FALSE) {
  
  drone_sites <- data$drone_sites
  drone_treatments <- data$drone_treatments
  
  # Note: Zone filtering is handled in processed_data() after database joins
  
  # Apply facility filter
  if (!is.null(facility_filter) && length(facility_filter) > 0 && !("all" %in% facility_filter)) {
    drone_sites <- drone_sites %>% filter(facility %in% facility_filter)
    drone_treatments <- drone_treatments %>% filter(facility %in% facility_filter)
  }
  
  # Apply foreman/FOS filter
  if (!is.null(foreman_filter) && length(foreman_filter) > 0 && !("all" %in% foreman_filter)) {
    # Convert shortnames to employee numbers for filtering
    foremen_lookup <- get_foremen_lookup()
    selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% foreman_filter]
    
    drone_sites <- drone_sites %>% filter(foreman %in% selected_emp_nums)
    drone_treatments <- drone_treatments %>% filter(foreman %in% selected_emp_nums)
  }
  
  # Apply prehatch filter
  if (prehatch_only) {
    drone_sites <- drone_sites %>% filter(prehatch == 'PREHATCH')
    drone_treatments <- drone_treatments %>% filter(prehatch == 'PREHATCH')
  }
  
  return(list(
    drone_sites = drone_sites,
    drone_treatments = drone_treatments
  ))
}

#' Create zone-separated combined groups for display
#' @param data Data frame with grouping column and zone
#' @param group_col Column name for grouping
#' @return Data frame with combined_group and zone keys added
create_zone_groups <- function(data, group_col) {
  if (group_col == "facility") {
    # Map facility names BEFORE creating combined_group
    data <- data %>% map_facility_names(facility_col = "facility")
    # Use facility_display for combined_group display, keep facility for color mapping
    data$combined_group <- paste0(data$facility_display, " (P", data$zone, ")")
    data$facility_zone_key <- paste0(data$facility, " (P", data$zone, ")")
  } else {
    # For other groupings, use the raw column values
    data$combined_group <- paste0(data[[group_col]], " (P", data$zone, ")")
  }
  
  return(data)
}

#' Get colors for visualization based on grouping type
#' @param group_by Grouping column name
#' @param data Data frame for color mapping
#' @param show_zones_separately Boolean for zone separation
#' @param zone_filter Vector of selected zones
#' @return Named vector of colors or list with colors and alpha values
get_visualization_colors <- function(group_by, data, show_zones_separately = FALSE, 
                                     zone_filter = NULL, for_historical = FALSE) {
  
  if (group_by == "facility") {
    if (show_zones_separately && for_historical) {
      # For HISTORICAL: Use zone-aware facility colors with alpha differentiation
      facility_result <- get_facility_base_colors(
        alpha_zones = zone_filter,
        combined_groups = unique(data$combined_group)
      )
      return(facility_result$colors)
    } else {
      # For CURRENT: Use standard facility colors (no zone differentiation)
      return(get_facility_base_colors())
    }
  } else if (group_by == "foreman") {
    if (show_zones_separately && for_historical) {
      # Get zone-aware foreman colors for historical
      foreman_result <- get_foreman_colors(
        alpha_zones = zone_filter,
        combined_groups = unique(data$combined_group)
      )
      return(foreman_result$colors)
    } else {
      # Standard foreman colors - map employee numbers to facility-based colors
      foreman_colors <- get_foreman_colors()
      foremen_lookup <- get_foremen_lookup()
      
      # Create mapping from foreman NUMBER to facility-based colors
      foremen_in_data <- unique(na.omit(data[[group_by]]))
      emp_colors <- character(0)
      
      for (foreman_num in foremen_in_data) {
        foreman_num_str <- trimws(as.character(foreman_num))
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
        
        if (length(matches) > 0) {
          shortname <- foremen_lookup$shortname[matches[1]]
          if (shortname %in% names(foreman_colors)) {
            emp_colors[foreman_num_str] <- foreman_colors[shortname]
          }
        }
      }
      return(emp_colors)
    }
  } else if (group_by == "sectcode") {
    return(get_foreman_colors())
  } else {
    return(get_facility_base_colors())
  }
}

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- fluidPage(
  # Application title
  titlePanel("Drone Sites with Active and Expiring Treatments"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      # Current Progress tab controls
      conditionalPanel(
        condition = "input.tabs == 'current'",
        radioButtons("current_display_metric", "Display Metric:",
                     choices = c("Number of Sites" = "sites",
                                 "Total Acres" = "acres"),
                     selected = "sites"),
        
        sliderInput("expiring_days", "Days Until Expiration:",
                    min = 1, max = 30, value = 7, step = 1)
      ),
      
      # Historical Trends tab controls
      conditionalPanel(
        condition = "input.tabs == 'historical'",
        radioButtons("hist_display_metric", "Display Metric:",
                     choices = c("Number of Sites" = "sites",
                                 "Number of Treatments" = "treatments"),
                     selected = "sites")
      ),
      
      # Shared controls
      checkboxInput("prehatch_only", "Show Only Prehatch Sites", value = FALSE),
      
      checkboxGroupInput("zone_filter", "Select Zones:",
                         choices = c("P1" = "1", "P2" = "2"),
                         selected = c("1", "2")),
      
      checkboxGroupInput("facility_filter", "Select Facilities:",
                         choices = get_facility_choices(),
                         selected = "all"),
      
      checkboxGroupInput("foreman_filter", "Select FOS:",
                         choices = get_foreman_choices(),
                         selected = "all"),
      
      # Group by controls (dynamic based on tab)
      conditionalPanel(
        condition = "input.tabs == 'current'",
        radioButtons("group_by", "Group By:",
                     choices = c("Facility" = "facility",
                                 "FOS" = "foreman",
                                 "Section" = "sectcode"),
                     selected = "facility")
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'historical' || input.tabs == 'historical_site_avg'",
        radioButtons("group_by", "Group By:",
                     choices = c("Facility" = "facility",
                                 "FOS" = "foreman"),
                     selected = "facility")
      )
    ),
    
    # Main panel with tabs
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Current Progress", value = "current", plotOutput("currentPlot")),
        tabPanel("Historical Trends", value = "historical",
                 # Historical controls row
                 fluidRow(
                   column(3,
                          selectInput("hist_start_year", "Start Year:",
                                      choices = seq(2010, 2025),
                                      selected = 2018)
                   ),
                   column(3,
                          selectInput("hist_end_year", "End Year:",
                                      choices = seq(2010, 2025),
                                      selected = 2025)
                   ),
                   column(3,
                          selectInput("hist_display_metric", "Display:",
                                      choices = c("Sites" = "sites", "Treatments" = "treatments"),
                                      selected = "sites")
                   ),
                   column(3,
                          checkboxInput("hist_show_percentages", "Show Percentages", value = FALSE)
                   )
                 ),
                 plotOutput("historicalPlot")
        ),
        tabPanel("Site Average", value = "historical_site_avg", plotOutput("siteAvgPlot"))
      )
    )
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # Update group by options when switching to historical tabs
  observeEvent(input$tabs, {
    if (input$tabs %in% c("historical", "historical_site_avg")) {
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
  
  # Raw data reactive - loads base data from database
  raw_data <- reactive({
    drone_types <- c("Y", "M", "C")  # Default drone types
    load_raw_data(drone_types)
  })
  
  # Apply filters to raw data
  filtered_data <- reactive({
    data <- raw_data()
    apply_data_filters(
      data = data,
      facility_filter = input$facility_filter,
      foreman_filter = input$foreman_filter,
      prehatch_only = input$prehatch_only
    )
  })
  
  # Continue with the rest of the functions...
  
  # =============================================================================
  # CURRENT PROGRESS SECTION
  # =============================================================================
  
  # Process current progress data
  processed_data <- reactive({
    # Get filtered data
    data <- filtered_data()
    drone_sites <- data$drone_sites
    drone_treatments <- data$drone_treatments
    
    # Apply zone filter if selected (zone column exists after database joins)
    if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
      drone_sites <- drone_sites %>% filter(zone %in% input$zone_filter)
      drone_treatments <- drone_treatments %>% filter(zone %in% input$zone_filter)
    }
    
    # Check if we have any data after filtering
    if (nrow(drone_sites) == 0) {
      return(data.frame())
    }
    
    # Calculate expiring window
    current_date <- Sys.Date()
    expiring_start_date <- current_date
    expiring_end_date <- current_date + input$expiring_days
    
    # Update treatment status to include expiring
    drone_treatments <- drone_treatments %>%
      mutate(
        treatment_start_date = as.Date(inspdate),
        treatment_end_date = treatment_start_date + days(ifelse(is.na(effect_days), 14, effect_days)),
        is_active = treatment_end_date >= current_date,
        is_expiring = is_active & treatment_end_date >= expiring_start_date & treatment_end_date <= expiring_end_date
      )
    
    # Determine grouping and zone separation
    group_col <- input$group_by
    show_zones_separately <- length(input$zone_filter) > 1
    
    # Filter by foreman assignment if grouping by foreman
    if (group_col == "foreman") {
      drone_sites <- drone_sites %>% filter(!is.na(foreman) & foreman != "")
      drone_treatments <- drone_treatments %>% filter(!is.na(foreman) & foreman != "")
    }
    
    # Create zone-separated groups if needed
    if (show_zones_separately) {
      drone_sites <- create_zone_groups(drone_sites, group_col)
      drone_treatments <- create_zone_groups(drone_treatments, group_col)
    }
    
    # Aggregate data by grouping
    if (show_zones_separately) {
      # Use combined_group for aggregation when zones are separated
      total_drone_sites <- drone_sites %>%
        group_by(combined_group) %>%
        summarize(total_sites = n(), total_acres = sum(acres, na.rm = TRUE), .groups = 'drop')
      
      active_drone_sites <- drone_treatments %>%
        filter(is_active == TRUE) %>%
        distinct(sitecode, combined_group, acres) %>%
        group_by(combined_group) %>%
        summarize(active_sites = n(), active_acres = sum(acres, na.rm = TRUE), .groups = 'drop')
      
      expiring_drone_sites <- drone_treatments %>%
        filter(is_expiring == TRUE) %>%
        distinct(sitecode, combined_group, acres) %>%
        group_by(combined_group) %>%
        summarize(expiring_sites = n(), expiring_acres = sum(acres, na.rm = TRUE), .groups = 'drop')
    } else {
      # Use original grouping column
      total_drone_sites <- drone_sites %>%
        group_by(!!sym(group_col)) %>%
        summarize(total_sites = n(), total_acres = sum(acres, na.rm = TRUE), .groups = 'drop')
      
      active_drone_sites <- drone_treatments %>%
        filter(is_active == TRUE) %>%
        distinct(sitecode, !!sym(group_col), acres) %>%
        group_by(!!sym(group_col)) %>%
        summarize(active_sites = n(), active_acres = sum(acres, na.rm = TRUE), .groups = 'drop')
      
      expiring_drone_sites <- drone_treatments %>%
        filter(is_expiring == TRUE) %>%
        distinct(sitecode, !!sym(group_col), acres) %>%
        group_by(!!sym(group_col)) %>%
        summarize(expiring_sites = n(), expiring_acres = sum(acres, na.rm = TRUE), .groups = 'drop')
    }
    
    # Combine all data
    if (show_zones_separately) {
      combined_data <- total_drone_sites %>%
        left_join(active_drone_sites, by = "combined_group") %>%
        left_join(expiring_drone_sites, by = "combined_group")
    } else {
      combined_data <- total_drone_sites %>%
        left_join(active_drone_sites, by = group_col) %>%
        left_join(expiring_drone_sites, by = group_col)
    }
    
    # Fill missing values and round acres
    combined_data <- combined_data %>%
      mutate(
        active_sites = ifelse(is.na(active_sites), 0, active_sites),
        active_acres = ifelse(is.na(active_acres), 0, active_acres),
        expiring_sites = ifelse(is.na(expiring_sites), 0, expiring_sites),
        expiring_acres = ifelse(is.na(expiring_acres), 0, expiring_acres),
        total_acres = round(total_acres),
        active_acres = round(active_acres),
        expiring_acres = round(expiring_acres)
      )
    
    # Add display names and color mapping keys
    if (show_zones_separately) {
      if (group_col == "facility") {
        # Extract facility display names and create color keys
        facility_display_from_combined <- gsub(" \\(P[12]\\)", "", combined_data$combined_group)
        zone_factor_from_combined <- gsub(".*\\(P([12])\\).*", "\\1", combined_data$combined_group)
        
        # Create reverse mapping for color keys
        facilities_lookup <- get_facility_lookup()
        facility_reverse_map <- setNames(facilities_lookup$short_name, facilities_lookup$full_name)
        
        combined_data <- combined_data %>%
          mutate(
            facility_display_temp = facility_display_from_combined,
            zone_factor = zone_factor_from_combined,
            facility_short = facility_reverse_map[facility_display_temp],
            facility_zone_key = paste0(facility_short, " (P", zone_factor, ")"),
            display_name = combined_group
          ) %>%
          select(-facility_display_temp)
      } else if (group_col == "foreman") {
        # Extract foreman info and map to display names
        foremen_lookup <- get_foremen_lookup()
        foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
        
        combined_data <- combined_data %>%
          mutate(
            foreman = gsub(" \\(P[12]\\)", "", combined_group),
            zone_factor = gsub(".*\\(P([12])\\).*", "\\1", combined_group),
            foreman_display = ifelse(
              is.na(foreman) | foreman == "",
              "Unassigned FOS",
              foreman_map[as.character(trimws(foreman))]
            ),
            foreman_display = ifelse(
              is.na(foreman_display),
              paste0("FOS #", foreman),
              foreman_display
            ),
            display_name = paste0(foreman_display, " (P", zone_factor, ")")
          )
      }
    } else {
      # Add display names for non-zone-separated data
      if (group_col == "facility") {
        combined_data <- combined_data %>% 
          mutate(facility = !!sym(group_col)) %>%
          map_facility_names(facility_col = "facility")
        combined_data$display_name <- combined_data$facility_display
      } else if (group_col == "foreman") {
        foremen_lookup <- get_foremen_lookup()
        foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
        
        combined_data$display_name <- ifelse(
          is.na(combined_data[[group_col]]) | combined_data[[group_col]] == "",
          "Unassigned FOS",
          foreman_map[as.character(trimws(combined_data[[group_col]]))]
        )
        combined_data$display_name <- ifelse(
          is.na(combined_data$display_name),
          paste0("FOS #", combined_data[[group_col]]),
          combined_data$display_name
        )
      } else if (group_col == "sectcode") {
        # Add sectcode display formatting
        sectcode_facility_map <- drone_sites %>%
          select(sectcode, facility) %>%
          distinct()
        
        combined_data <- combined_data %>%
          left_join(sectcode_facility_map, by = "sectcode") %>%
          map_facility_names(facility_col = "facility") %>%
          mutate(sectcode_display = paste0(facility_display, " - ", sectcode))
        combined_data$display_name <- combined_data$sectcode_display
      }
    }
    
    # Add y-axis values based on selected metric
    if (input$current_display_metric == "sites") {
      combined_data <- combined_data %>%
        mutate(
          y_total = total_sites,
          y_active = active_sites,
          y_expiring = expiring_sites,
          show_active_label = y_active > 0
        )
    } else {
      combined_data <- combined_data %>%
        mutate(
          y_total = total_acres,
          y_active = active_acres,
          y_expiring = expiring_acres,
          show_active_label = y_active > 0
        )
    }
    
    return(combined_data)
  })
  
  # Current progress plot
  output$currentPlot <- renderPlot({
    data <- processed_data()
    
    if (nrow(data) == 0) {
      return(ggplot() + 
             geom_text(aes(x = 0.5, y = 0.5, label = "No data available"), size = 6) +
             theme_void())
    }
    
    # Determine variables and colors
    show_zones_separately <- length(input$zone_filter) > 1
    
    # Set x-axis and fill variables
    if (show_zones_separately) {
      x_var <- "display_name"
      if (input$group_by == "facility") {
        fill_var <- "facility_zone_key"
      } else {
        fill_var <- "combined_group"
      }
    } else {
      x_var <- "display_name"
      fill_var <- input$group_by
    }
    
    # Get colors
    custom_colors <- get_visualization_colors(
      group_by = input$group_by,
      data = data,
      show_zones_separately = show_zones_separately,
      zone_filter = input$zone_filter,
      for_historical = FALSE
    )
    
    # Handle zone-separated color mapping for current progress
    if (show_zones_separately && !is.null(custom_colors)) {
      if (input$group_by == "facility") {
        # For facilities, create colors mapping for facility_zone_key using base facility colors
        facility_zone_keys <- unique(data$facility_zone_key)
        combined_colors <- character(0)
        
        for (zone_key in facility_zone_keys) {
          base_facility <- gsub("\\s*\\([^)]+\\)$", "", zone_key)
          base_facility <- trimws(base_facility)
          
          if (base_facility %in% names(custom_colors)) {
            combined_colors[zone_key] <- custom_colors[base_facility]
          }
        }
      } else {
        # For other groupings, extract base names from combined groups
        base_names <- unique(data$combined_group)
        combined_colors <- character(0)
        
        for (combined_name in base_names) {
          base_name <- gsub("\\s*\\([^)]+\\)$", "", combined_name)
          base_name <- trimws(base_name)
          
          if (base_name %in% names(custom_colors)) {
            combined_colors[combined_name] <- custom_colors[base_name]
          }
        }
      }
      
      if (length(combined_colors) > 0) {
        custom_colors <- combined_colors
      }
    }
    
    # Get status colors
    status_colors <- get_status_colors()
    
    # Create plot
    if (!is.null(custom_colors) && input$group_by != "mmcd_all") {
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
    
    # Add labels and formatting
    metric_label <- ifelse(input$current_display_metric == "sites", "Number of Sites", "Total Acres")
    zone_text <- if (length(input$zone_filter) == 2) "" else 
      if (length(input$zone_filter) == 1) 
        ifelse("1" %in% input$zone_filter, " (P1 Only)", " (P2 Only)") else " (No Zones)"
    prehatch_text <- ifelse(input$prehatch_only, " - Prehatch Only", "")
    
    group_label <- case_when(
      input$group_by == "facility" ~ "Facility",
      input$group_by == "foreman" ~ "FOS", 
      input$group_by == "sectcode" ~ "Section",
      TRUE ~ "Group"
    )
    
    p <- p +
      labs(
        title = paste0("Drone Sites Progress by ", group_label, zone_text, prehatch_text),
        subtitle = paste0("Gray: Total sites, Blue: Active treatments, Orange: Expiring in ", input$expiring_days, " days"),
        x = group_label,
        y = metric_label
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    print(p)
  })
  
  # =============================================================================
  # HISTORICAL TRENDS SECTION  
  # =============================================================================
  # Note: Historical functions are now sourced from historical_functions.R
  # This file contains EXACT copies from the working backup version
  
  # Historical plot output - uses external function
  output$historicalPlot <- renderPlot({
    p <- create_historical_plot(
      zone_filter = input$zone_filter,
      group_by = input$group_by,
      hist_display_metric = input$hist_display_metric,
      prehatch_only = input$prehatch_only,
      hist_show_percentages = input$hist_show_percentages,
      hist_start_year = input$hist_start_year,
      hist_end_year = input$hist_end_year,
      drone_types = input$drone_types,
      facility_filter = input$facility_filter,
      foreman_filter = input$foreman_filter
    )
    print(p)
  })
  
  # =============================================================================
  # SITE AVERAGE SECTION
  # =============================================================================
  
  # Historical raw data for site averages
  historical_raw_data <- reactive({
    # Implementation for site average data processing
    # [This would contain the site average processing logic]
    return(data.frame())  # Placeholder
  })
  
  # Site average plot  
  output$siteAvgPlot <- renderPlot({
    # Implementation for site average plotting
    # [This would contain the site average plotting logic]
    ggplot() + 
      geom_text(aes(x = 0.5, y = 0.5, label = "Site Average plot - To be implemented"), size = 6) +
      theme_void()
  })
}

# =============================================================================
# APPLICATION LAUNCH
# =============================================================================

# Run the application
shinyApp(ui = ui, server = server)