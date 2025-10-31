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

# Load environment variables
load_env_vars()

# Define UI for the application
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
        
        # Slider to control the expiration window
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
      
      # Toggle for prehatch filter
      checkboxInput("prehatch_only", "Show Only Prehatch Sites", value = FALSE),
      
      # Zone filter
      checkboxGroupInput("zone_filter", "Filter by Zone:",
                        choices = c("P1" = "1", "P2" = "2"),
                        selected = c("1", "2"),
                        inline = TRUE),
      
      # Dropdown for facility filter - using db_helpers to get full names with multiple selection
      selectizeInput("facility_filter", "Facility:",
                    choices = get_facility_choices(),
                    selected = "all", multiple = TRUE),
      
      # Dropdown for FOS filter - multiple selection
      selectizeInput("foreman_filter", "FOS:",
                    choices = get_foreman_choices(),
                    selected = "all", multiple = TRUE),
      
      # Group by option - show section only on current tab
      radioButtons("group_by", "Group by:",
                  choices = if(FALSE) {  # This will be updated dynamically
                    c("Facility" = "facility", "FOS" = "foreman", "Section" = "sectcode")
                  } else {
                    c("Facility" = "facility", "FOS" = "foreman")
                  },
                  selected = "facility",
                  inline = TRUE),
      
      # Toggle for drone designation types
      checkboxGroupInput("drone_types", "Include Drone Designations:",
                         choices = c("Yes (Y)" = "Y",
                                     "Maybe (M)" = "M",
                                     "Considering (C)" = "C"),
                         selected = "Y"),
      
      helpText("This visualization shows drone sites by facility with three categories:",
               tags$br(),
               tags$ul(
                 tags$li(tags$span(style = paste0("color:", get_status_colors()["unknown"]), "Gray: Total sites/acres")),
                 tags$li(tags$span(style = paste0("color:", get_status_colors()["active"]), "Green: Sites/acres with active treatments")),
                 tags$li(tags$span(style = paste0("color:", get_status_colors()["planned"]), "Orange: Sites/acres with treatments expiring within the selected days"))
               )),
      
      helpText(tags$b("Drone Designations:"),
               tags$br(),
               tags$ul(
                 tags$li(tags$b("Y:"), "Yes - Confirmed drone site"),
                 tags$li(tags$b("M:"), "Maybe - Potential drone site"),
                 tags$li(tags$b("C:"), "Considering (?) - Under evaluation"),
                 tags$li(tags$b("N:"), "No - Not suitable for drone (excluded)")
               ))
    ),
    
    # Main panel for displaying the graphs with tabs
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Current Progress", value = "current",
                 plotOutput("droneGraph", height = "600px")
        ),
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
                          checkboxInput("hist_show_percentages", "Show Percentages", value = FALSE)
                   )
                 ),
                 plotOutput("historicalGraph", height = "600px"),
                 plotOutput("siteAvgSizeGraph", height = "400px"),
                 tableOutput("summaryTable"),
                 h4("5 Smallest and 5 Largest Drone Sites per Year"),
                 tableOutput("siteExtremesTable")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Update group_by choices based on current tab
  observeEvent(input$tabs, {
    if (input$tabs == "current") {
      updateRadioButtons(session, "group_by",
                        choices = c("Facility" = "facility", 
                                   "FOS" = "foreman",
                                   "Section" = "sectcode"),
                        selected = input$group_by)
    } else {
      # Historical tab - no section option
      if (input$group_by == "sectcode") {
        # Switch to facility if currently on section
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
  
  # Current date
  current_date <- Sys.Date() 
  
  # Fetch data from database
  raw_data <- reactive({
    con <- get_db_connection()
    if (is.null(con)) return(data.frame())
    
    # Build the drone designation filter based on user selection
    drone_types <- paste0("'", paste(input$drone_types, collapse = "','"), "'")
    
    # Query to get drone sites from loc_breeding_sites including acres, prehatch status, foreman and sectcode
    # Include both drone='Y','M','C'? and air_gnd='D' as indicators
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
", drone_types)
    
    drone_sites <- dbGetQuery(con, drone_sites_query)
    
    # Query to get treatment information and material types with foreman
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
        # Use site foreman (jurisdictional assignment) - ignore treatment foreman
        foreman = foreman_site,
        # Keep other site info
        prehatch = prehatch,
        drone = drone,
        zone = zone,
        sectcode = sectcode
      )
    
    #Calculate treatment status (active)
    drone_treatments <- drone_treatments %>%
      mutate(
        inspdate = as.Date(inspdate),
        effect_days = ifelse(is.na(effect_days), 0, effect_days),
        treatment_end_date = inspdate + effect_days,
        is_active = treatment_end_date >= current_date
      )
    
    # Return all the data needed for filtering later
    list(
      drone_sites = drone_sites,
      drone_treatments = drone_treatments
    )
  })
  
  # Process data based on user inputs
  processed_data <- reactive({
    # Get raw data
    data <- raw_data()
    drone_sites <- data$drone_sites
    drone_treatments <- data$drone_treatments
    
    # Apply zone filter if selected
    if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
      drone_sites <- drone_sites %>%
        filter(zone %in% input$zone_filter)
      
      drone_treatments <- drone_treatments %>%
        filter(zone %in% input$zone_filter)
    }
    
    # Apply facility filter if selected
    if (!is.null(input$facility_filter) && length(input$facility_filter) > 0 && 
        !("all" %in% input$facility_filter)) {
      drone_sites <- drone_sites %>%
        filter(facility %in% input$facility_filter)
      
      drone_treatments <- drone_treatments %>%
        filter(facility %in% input$facility_filter)
    }
    
    # Apply foreman/FOS filter if selected
    if (!is.null(input$foreman_filter) && length(input$foreman_filter) > 0 && 
        !("all" %in% input$foreman_filter)) {
      # Convert shortnames to employee numbers for filtering
      foremen_lookup <- get_foremen_lookup()
      selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% input$foreman_filter]
      
      drone_sites <- drone_sites %>%
        filter(foreman %in% selected_emp_nums)
      
      drone_treatments <- drone_treatments %>%
        filter(foreman %in% selected_emp_nums)
    }
    
    # Apply prehatch filter if selected
    if (input$prehatch_only) {
      # Filter to only prehatch sites
      drone_sites <- drone_sites %>%
        filter(prehatch == 'PREHATCH')
      
      # Update treatments to only include prehatch sites
      drone_treatments <- drone_treatments %>%
        filter(prehatch == 'PREHATCH')
    }
    
    # Calculate expiring window
    expiring_start_date <- current_date
    expiring_end_date <- current_date + input$expiring_days
    
    # Update treatment status to include expiring
    drone_treatments <- drone_treatments %>%
      mutate(
        is_expiring = is_active & treatment_end_date >= expiring_start_date &
          treatment_end_date <= expiring_end_date
      )
    
    # Determine grouping column
    group_col <- input$group_by
    if (group_col == "foreman") {
      # Only include sites that have a foreman assigned (jurisdictional assignment)
      drone_sites <- drone_sites %>% filter(!is.na(foreman) & foreman != "")
      drone_treatments <- drone_treatments %>% filter(!is.na(foreman) & foreman != "")
    }
    
    # Check if we have any data after filtering
    if (nrow(drone_sites) == 0) {
      return(data.frame())
    }
    
    # Add combined group column for zone differentiation when both zones selected
    show_zones_separately <- length(input$zone_filter) > 1
    if (show_zones_separately) {
      if (group_col == "facility") {
        # Map facility names BEFORE creating combined_group
        drone_sites <- drone_sites %>% map_facility_names(facility_col = "facility")
        drone_treatments <- drone_treatments %>% map_facility_names(facility_col = "facility")
        # Use facility_display for combined_group display, but keep facility for color mapping
        drone_sites$combined_group <- paste0(drone_sites$facility_display, " (P", drone_sites$zone, ")")
        drone_treatments$combined_group <- paste0(drone_treatments$facility_display, " (P", drone_treatments$zone, ")")
        # Keep the short codes for color mapping
        drone_sites$facility_zone_key <- paste0(drone_sites$facility, " (P", drone_sites$zone, ")")
        drone_treatments$facility_zone_key <- paste0(drone_treatments$facility, " (P", drone_treatments$zone, ")")
      } else {
        # For other groupings, use the raw column values
        drone_sites$combined_group <- paste0(drone_sites[[group_col]], " (P", drone_sites$zone, ")")
        drone_treatments$combined_group <- paste0(drone_treatments[[group_col]], " (P", drone_treatments$zone, ")")
      }
    }
    
    # Count total drone sites by group
    total_drone_sites <- drone_sites %>%
      group_by(!!sym(group_col)) %>%
      summarize(
        total_sites = n(),
        total_acres = sum(acres, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Count active drone sites by group
    # Need to handle overlapping treatments by taking distinct sitecodes only
    active_drone_sites <- drone_treatments %>%
      filter(is_active == TRUE) %>%
      distinct(sitecode, !!sym(group_col), acres) %>%
      group_by(!!sym(group_col)) %>%
      summarize(
        active_sites = n(),
        active_acres = sum(acres, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Count expiring drone sites by group
    expiring_drone_sites <- drone_treatments %>%
      filter(is_expiring == TRUE) %>%
      distinct(sitecode, !!sym(group_col), acres) %>%
      group_by(!!sym(group_col)) %>%
      summarize(
        expiring_sites = n(),
        expiring_acres = sum(acres, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Combine all counts by group
    combined_data <- total_drone_sites %>%
      left_join(active_drone_sites, by = group_col) %>%
      left_join(expiring_drone_sites, by = group_col) %>%
      mutate(
        active_sites = ifelse(is.na(active_sites), 0, active_sites),
        active_acres = ifelse(is.na(active_acres), 0, active_acres),
        expiring_sites = ifelse(is.na(expiring_sites), 0, expiring_sites),
        expiring_acres = ifelse(is.na(expiring_acres), 0, expiring_acres),
        # Round acres to whole numbers
        total_acres = round(total_acres),
        active_acres = round(active_acres),
        expiring_acres = round(expiring_acres)
      )
    
    # Add zone information when both zones are selected
    if (show_zones_separately) {
      # Recalculate with combined groups for zone differentiation
      total_by_combined <- drone_sites %>%
        group_by(combined_group) %>%
        summarize(
          total_sites = n(),
          total_acres = sum(acres, na.rm = TRUE),
          .groups = 'drop'
        )
      
      active_by_combined <- drone_treatments %>%
        filter(is_active == TRUE) %>%
        distinct(sitecode, combined_group, acres) %>%
        group_by(combined_group) %>%
        summarize(
          active_sites = n(),
          active_acres = sum(acres, na.rm = TRUE),
          .groups = 'drop'
        )
      
      expiring_by_combined <- drone_treatments %>%
        filter(is_expiring == TRUE) %>%
        distinct(sitecode, combined_group, acres) %>%
        group_by(combined_group) %>%
        summarize(
          expiring_sites = n(),
          expiring_acres = sum(acres, na.rm = TRUE),
          .groups = 'drop'
        )
      
      combined_data <- total_by_combined %>%
        left_join(active_by_combined, by = "combined_group") %>%
        left_join(expiring_by_combined, by = "combined_group") %>%
        mutate(
          active_sites = ifelse(is.na(active_sites), 0, active_sites),
          active_acres = ifelse(is.na(active_acres), 0, active_acres),
          expiring_sites = ifelse(is.na(expiring_sites), 0, expiring_sites),
          expiring_acres = ifelse(is.na(expiring_acres), 0, expiring_acres),
          # Round acres to whole numbers
          total_acres = round(total_acres),
          active_acres = round(active_acres),
          expiring_acres = round(expiring_acres)
        )
      
      # Add the original group column for color mapping, zone factor, and display names
      if (group_col == "facility") {
        # Extract facility display names and zone from combined_group
        facility_display_from_combined <- gsub(" \\(P[12]\\)", "", combined_data$combined_group)
        zone_factor_from_combined <- gsub(".*\\(P([12])\\).*", "\\1", combined_data$combined_group)
        
        # Create reverse mapping from full names back to short codes for color mapping
        facilities_lookup <- get_facility_lookup()
        facility_reverse_map <- setNames(facilities_lookup$short_name, facilities_lookup$full_name)
        
        combined_data <- combined_data %>%
          mutate(
            facility_display_temp = facility_display_from_combined,
            zone_factor = zone_factor_from_combined,
            # Map back to short codes for color mapping
            facility_short = facility_reverse_map[facility_display_temp],
            # Create facility_zone_key using SHORT codes for color mapping
            facility_zone_key = paste0(facility_short, " (P", zone_factor, ")")
          ) %>%
          select(-facility_display_temp)
        # For zone-separated facility grouping, display_name is the combined_group itself
        combined_data$display_name <- combined_data$combined_group
      } else if (group_col == "foreman") {
        # Extract foreman numbers and map to display names
        foremen_lookup <- get_foremen_lookup()
        foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
        
        combined_data <- combined_data %>%
          mutate(
            foreman = gsub(" \\(P[12]\\)", "", combined_group),
            zone_factor = gsub(".*\\(P([12])\\).*", "\\1", combined_group)
          )
        
        # Create display names for foreman with zones
        combined_data$foreman_display <- ifelse(
          is.na(combined_data$foreman) | combined_data$foreman == "",
          "Unassigned FOS",
          foreman_map[as.character(trimws(combined_data$foreman))]
        )
        # Handle any remaining NAs
        combined_data$foreman_display <- ifelse(
          is.na(combined_data$foreman_display),
          paste0("FOS #", combined_data$foreman),
          combined_data$foreman_display
        )
        
        # Create combined display name with zone
        combined_data$display_name <- paste0(combined_data$foreman_display, " (P", combined_data$zone_factor, ")")
      }
    } else {
      # Add display names to the aggregated data
      if (group_col == "facility") {
        combined_data <- combined_data %>% 
          mutate(facility = !!sym(group_col)) %>%
          map_facility_names(facility_col = "facility")
        combined_data$display_name <- combined_data$facility_display
      } else if (group_col == "foreman") {
        # Map foreman employee numbers to shortnames using db_helpers - EXACTLY like struct_trt
        foremen_lookup <- get_foremen_lookup()
        foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
        
        # Handle NA values properly - make sure we convert to character and trim
        combined_data$display_name <- ifelse(
          is.na(combined_data[[group_col]]) | combined_data[[group_col]] == "",
          "Unassigned FOS",
          foreman_map[as.character(trimws(combined_data[[group_col]]))]
        )
        # Handle any remaining NAs from missing foreman numbers in lookup
        combined_data$display_name <- ifelse(
          is.na(combined_data$display_name),
          paste0("FOS #", combined_data[[group_col]]),
          combined_data$display_name
        )
        
        # Keep foreman as employee numbers for color mapping, display_name as shortnames for UI
      } else if (group_col == "sectcode") {
        # Add sectcode display formatting
        # Need to get facility info for each sectcode
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
    
    return(combined_data)
  })
  
  # Generate the plot
  output$droneGraph <- renderPlot({
    # Get the processed data
    data <- processed_data()
    
    # Handle case when no data is available (e.g., no prehatch sites)
    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No data available with the selected filters", size = 6) +
          theme_void()
      )
    }
    
    # Determine if we're showing zones separately
    show_zones_separately <- length(input$zone_filter) > 1
    
    # Choose which metric to display based on user input
    if (input$current_display_metric == "sites") {
      # Display number of sites
      data$y_total <- data$total_sites
      data$y_active <- data$active_sites
      data$y_expiring <- data$expiring_sites
      y_label <- "Number of Sites"
      title_metric <- "Number of Sites"
    } else {
      # Display acres
      data$y_total <- data$total_acres
      data$y_active <- data$active_acres
      data$y_expiring <- data$expiring_acres
      y_label <- "Total Acres"
      title_metric <- "Acres"
    }
    
    # Create a new column to determine which labels to show (avoiding overplot)
    data$show_active_label <- data$y_active != data$y_expiring
    
    # Calculate y-axis maximum for proper positioning
    y_max <- max(data$y_total) * 1.1
    
    # Set up the title with appropriate filters
    drone_types_text <- paste(input$drone_types, collapse = ", ")
    prehatch_filter_text <- ifelse(input$prehatch_only, "Prehatch Sites Only", "All Sites")
    zone_filter_text <- ifelse(length(input$zone_filter) == 2, "P1+P2", 
                               ifelse(length(input$zone_filter) == 1, 
                                      ifelse("1" %in% input$zone_filter, "P1 Only", "P2 Only"), 
                                      "No Zones"))
    
    # Get colors from shared helper functions
    # Get colors based on grouping - following struct_trt pattern EXACTLY
    custom_colors <- if(input$group_by == "facility") {
      get_facility_base_colors()
    } else if(input$group_by == "foreman") {
      # Follow struct_trt pattern exactly - map foreman NUMBERS to facility-based colors
      foreman_colors <- get_foreman_colors()  # These are keyed by shortname
      foremen_lookup <- get_foremen_lookup()
      
      # Create mapping from foreman NUMBER to facility-based colors
      foremen_in_data <- unique(na.omit(data[[input$group_by]]))
      emp_colors <- character(0)
      
      for (foreman_num in foremen_in_data) {
        foreman_num_str <- trimws(as.character(foreman_num))
        
        # Find the shortname for this foreman number
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
        
        if(length(matches) > 0) {
          shortname <- foremen_lookup$shortname[matches[1]]
          
          # Get the facility-based color for this shortname
          if(shortname %in% names(foreman_colors)) {
            emp_colors[foreman_num_str] <- foreman_colors[shortname]
          }
        }
      }
      
      # Return emp_colors keyed by foreman numbers
      emp_colors
    } else if(input$group_by == "sectcode") {
      # Use foreman colors for sectcode grouping too
      get_foreman_colors()
    } else {
      get_facility_base_colors()  # Default fallback
    }
    
    # Handle color mapping for combined groups (zone separation)
    # For CURRENT PROGRESS: Use same colors as single zone (no zone differentiation)
    # The bars themselves will show zone separation, but colors remain consistent
    if (show_zones_separately && !is.null(custom_colors)) {
      if (input$group_by == "facility") {
        # For facilities, create colors mapping for facility_zone_key using base facility colors
        # NO ZONE DIFFERENTIATION - use same colors as single zone
        facility_zone_keys <- unique(data$facility_zone_key)
        combined_colors <- character(0)
        
        for (zone_key in facility_zone_keys) {
          # Extract base facility short code by removing zone info like " (P1)" or " (P2)"
          base_facility <- gsub("\\s*\\([^)]+\\)$", "", zone_key)
          base_facility <- trimws(base_facility)
          
          # Map to existing facility color if available - SAME COLOR FOR BOTH ZONES
          if (base_facility %in% names(custom_colors)) {
            combined_colors[zone_key] <- custom_colors[base_facility]
          }
        }
      } else {
        # For other groupings, extract base names from combined groups
        base_names <- unique(data$combined_group)
        combined_colors <- character(0)
        
        for (combined_name in base_names) {
          # Extract base name by removing zone info like " (P1)" or " (P2)"
          base_name <- gsub("\\s*\\([^)]+\\)$", "", combined_name)
          base_name <- trimws(base_name)
          
          # Map to existing color if available - SAME COLOR FOR BOTH ZONES
          if (base_name %in% names(custom_colors)) {
            combined_colors[combined_name] <- custom_colors[base_name]
          }
        }
      }
      
      # Update custom_colors to use combined group mapping
      if (length(combined_colors) > 0) {
        custom_colors <- combined_colors
      }
    }
    
    status_colors <- get_status_colors()
    
    # Map colors for this application's specific needs
    total_color <- status_colors["unknown"]        # Gray for total sites
    active_color <- status_colors["active"]        # Green for active treatments
    expiring_color <- status_colors["planned"]     # Orange for expiring treatments
    
    # Determine x-axis and fill variables - following struct_trt pattern
    if (show_zones_separately) {
      x_var <- "display_name"  
      if (input$group_by == "facility") {
        fill_var <- "facility_zone_key"  # Use short codes for facility color mapping
      } else {
        fill_var <- "combined_group"
      }
    } else {
      x_var <- "display_name"  
      fill_var <- input$group_by  
    }

    # Final fallback to prevent "insufficient values" error
    if (is.null(custom_colors) || length(custom_colors) == 0) {
      # Generate basic colors for all unique values in the data
      unique_values <- unique(data[[x_var]])
      if (length(unique_values) > 0) {
        basic_colors <- rainbow(length(unique_values))
        names(basic_colors) <- unique_values
        custom_colors <- basic_colors
      }
    }

    # Create the plot - following struct_trt pattern EXACTLY
    if (!is.null(custom_colors) && input$group_by != "mmcd_all") {
      p <- ggplot(data, aes(x = .data[[x_var]], fill = !!sym(fill_var))) +
        # First draw total bars with reduced alpha
        geom_bar(aes(y = y_total), stat = "identity", alpha = 0.3) +
        # Then overlay active bars with zone-based alpha if zones separated
        geom_bar(aes(y = y_active), stat = "identity", alpha = 0.8) +
        # Finally overlay expiring bars
        geom_bar(aes(y = y_expiring), stat = "identity", fill = status_colors["planned"]) +
        scale_fill_manual(values = custom_colors, guide = "none")
    } else {
      # Default colors when no custom scheme available
      p <- ggplot(data, aes(x = .data[[x_var]])) +
        # First draw total bars
        geom_bar(aes(y = y_total), stat = "identity", fill = "gray80", alpha = 0.7) +
        # Then overlay active bars
        geom_bar(aes(y = y_active), stat = "identity", fill = status_colors["active"]) +
        # Finally overlay expiring bars
        geom_bar(aes(y = y_expiring), stat = "identity", fill = status_colors["planned"])
    }
    
    # Add labels and formatting
    p <- p +
      # Add labels on top of each bar
      geom_text(aes(x = .data[[x_var]], y = y_total, label = y_total), vjust = -0.5, color = "black") +
      # Add expiring labels
      geom_text(aes(x = .data[[x_var]], y = y_expiring, label = y_expiring), vjust = 1.5, color = "black", fontface = "bold")
    
    # Determine group label for title
    group_label <- case_when(
      input$group_by == "facility" ~ "Facility",
      input$group_by == "foreman" ~ "FOS", 
      input$group_by == "sectcode" ~ "Section",
      TRUE ~ "Group"
    )
    
    # Add labels and title
    p <- p +
      labs(
        title = paste("Drone Sites by", group_label, "-", title_metric),
        subtitle = paste("Zones:", zone_filter_text, "- Drone types:", drone_types_text, "-", prehatch_filter_text,
                         "- Expiring within", input$expiring_days, "days"),
        x = group_label,
        y = y_label
      ) +
      # Customize appearance
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold", size = 16, color = "black", angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    # Add active labels only where they differ from expiring
    if (any(data$show_active_label)) {
      p <- p + geom_text(data = data[data$show_active_label,],
                         aes(y = y_active, label = y_active),
                         vjust = -0.5, color = active_color, fontface = "bold")
    }
    
    print(p)
  })
  
  # Historical data reactive
  historical_raw_data <- reactive({
    con <- get_db_connection()
    if (is.null(con)) {
      return(NULL)
    }
    
    # Get archive data
    archive_query <- sprintf("
      SELECT facility, sitecode, inspdate, action
      FROM public.dblarv_insptrt_archive
      WHERE action = 'D'
      AND EXTRACT(YEAR FROM inspdate) BETWEEN %d AND %d
    ", as.integer(input$hist_start_year), as.integer(input$hist_end_year))
    
    archive_data <- dbGetQuery(con, archive_query)
    
    # Get current data  
    current_query <- sprintf("
      SELECT facility, sitecode, inspdate, action, airgrnd_plan
      FROM public.dblarv_insptrt_current
      WHERE (airgrnd_plan = 'D' OR action = 'D')
      AND EXTRACT(YEAR FROM inspdate) BETWEEN %d AND %d
    ", as.integer(input$hist_start_year), as.integer(input$hist_end_year))
    
    current_data <- dbGetQuery(con, current_query)
    
    # Get site size info from loc_breeding_sites
    # Build drone_types filter from UI (Y/M/C) to apply the same designation filtering used by current data
    drone_types <- paste0("'", paste(input$drone_types, collapse = "','"), "'")

    sitecodes <- unique(c(archive_data$sitecode, current_data$sitecode))
    if (length(sitecodes) > 0) {
      sitecodes_str <- paste(sprintf("'%s'", sitecodes), collapse = ",")
      lbs_query <- sprintf(
        "SELECT b.sitecode, b.acres, b.facility, b.prehatch, b.drone, 
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
        WHERE b.sitecode IN (%s)
        AND (b.drone IN (%s) OR b.air_gnd = 'D')
        AND b.enddate IS NULL",
        sitecodes_str, drone_types)
      lbs_data <- dbGetQuery(con, lbs_query)
    } else {
      lbs_data <- data.frame(sitecode=character(), acres=numeric(), facility=character(), prehatch=character(), drone=character(), foreman=character(), zone=character(), sectcode=character())
    }
    
    dbDisconnect(con)
    
    list(
      archive = archive_data,
      current = current_data,
      lbs = lbs_data
    )
  })
  
  # Process historical data based on user selections
  historical_processed_data <- reactive({
    # Get raw data
    data_list <- historical_raw_data()
    if (is.null(data_list)) return(data.frame())
    
    # Get LBS data for prehatch information
    lbs_data <- data_list$lbs
    
    # Process archive data
    archive_data <- data_list$archive %>%
      mutate(
        source = "Archive",
        year = year(inspdate)
      )
    
    # Process current data
    current_data <- data_list$current %>%
      mutate(
        source = "Current", 
        year = year(inspdate)
      )
    
    # Combine the data
    all_data <- bind_rows(archive_data, current_data)
    
    # Join with lbs_data to get prehatch information and extract sectcode
    all_data <- all_data %>%
      left_join(lbs_data, by = c("sitecode", "facility")) %>%
      mutate(
        # Extract sectcode from sitecode (first 7 characters) - use from lbs_data if available
        sectcode = ifelse(is.na(sectcode), substr(sitecode, 1, 7), sectcode)
      )
    
    # Apply zone filter if selected
    if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
      all_data <- all_data %>%
        filter(zone %in% input$zone_filter)
    }
    
    # Apply facility filter if selected
    if (!is.null(input$facility_filter) && length(input$facility_filter) > 0 && 
        !("all" %in% input$facility_filter)) {
      all_data <- all_data %>%
        filter(facility %in% input$facility_filter)
    }
    
    # Apply foreman/FOS filter if selected
    if (!is.null(input$foreman_filter) && length(input$foreman_filter) > 0 && 
        !("all" %in% input$foreman_filter)) {
      # Convert shortnames to employee numbers for filtering
      foremen_lookup <- get_foremen_lookup()
      selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% input$foreman_filter]
      
      all_data <- all_data %>%
        filter(foreman %in% selected_emp_nums)
    }
    
    # Apply prehatch filter if selected
    if (input$prehatch_only) {
      all_data <- all_data %>%
        filter(prehatch == 'PREHATCH')
    }
    
    # Determine grouping column and filter for foreman if needed
    group_col <- input$group_by
    if (group_col == "foreman") {
      # Only include sites that have a foreman assigned
      all_data <- all_data %>% filter(!is.na(foreman) & foreman != "")
    }
    
    # Process based on count type and grouping selection
    show_zones_separately <- length(input$zone_filter) > 1
    
    # Add combined group column for zone differentiation when both zones selected
    if (show_zones_separately) {
      all_data$combined_group <- paste0(all_data[[group_col]], " (P", all_data$zone, ")")
      group_var <- sym("combined_group")
    } else {
      group_var <- sym(group_col)
    }
    
    # Calculate counts based on display metric
    if (input$hist_display_metric == "treatments") {
      # Count all treatments
      results <- all_data %>%
        group_by(!!group_var, year) %>%
        summarize(count = n(), .groups = "drop")
    } else {
      # Count unique sites
      results <- all_data %>%
        group_by(!!group_var, year) %>%
        summarize(count = n_distinct(sitecode), .groups = "drop")
    }
    
    # Apply facility name mapping if grouping by facility or sectcode
    if (group_col %in% c("facility", "sectcode")) {
      # Add facility column for mapping if not present
      if (group_col == "facility") {
        if (show_zones_separately) {
          # Extract facility and zone info from combined_group before mapping
          results <- results %>%
            mutate(
              facility = gsub("\\s*\\(P[12]\\).*", "", combined_group),
              zone_part = gsub(".*\\((P[12])\\).*", "\\1", combined_group)
            ) %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(combined_group = paste0(facility_display, " (", zone_part, ")")) %>%
            select(-zone_part, -facility, -facility_display)
        } else {
          results <- results %>%
            mutate(facility = !!sym(group_col)) %>%
            map_facility_names(facility_col = "facility")
        }
      } else if (group_col == "sectcode") {
        # For sectcode grouping, we need to get facility info back
        sectcode_facility_map <- all_data %>%
          select(sectcode, facility) %>%
          distinct()
        
        if (show_zones_separately) {
          # Extract sectcode info from combined_group
          results <- results %>%
            mutate(
              sectcode_part = gsub("\\s*\\(P[12]\\).*", "", combined_group),
              zone_part = gsub(".*\\((P[12])\\).*", "\\1", combined_group)
            ) %>%
            separate(sectcode_part, into = c("facility", "sectcode"), sep = " - ", extra = "merge") %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(
              sectcode_display = paste0(facility_display, " - ", sectcode),
              combined_group = paste0(sectcode_display, " (", zone_part, ")")
            ) %>%
            select(-zone_part, -facility, -facility_display, -sectcode, -sectcode_display)
        } else {
          results <- results %>%
            rename(sectcode_temp = !!group_var) %>%
            left_join(sectcode_facility_map, by = c("sectcode_temp" = "sectcode")) %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(sectcode_display = paste0(facility_display, " - ", sectcode_temp)) %>%
            select(-sectcode_temp, -facility, -facility_display) %>%
            rename(!!group_col := sectcode_display)
        }
      }
    } else if (group_col == "foreman") {
      # Add foreman display formatting using db_helpers
      foremen_lookup <- get_foremen_lookup()
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      
      if (show_zones_separately) {
        results <- results %>%
          mutate(
            foreman_part = gsub("\\s*\\(P[12]\\).*", "", combined_group),
            zone_part = gsub(".*\\((P[12])\\).*", "\\1", combined_group),
            foreman_display = ifelse(
              foreman_part %in% names(foreman_map),
              foreman_map[foreman_part],
              paste("Unknown FOS", foreman_part)
            ),
            combined_group = paste0(foreman_display, " (", zone_part, ")")
          ) %>%
          select(-foreman_part, -zone_part, -foreman_display)
      } else {
        results <- results %>%
          mutate(foreman_display = ifelse(
            foreman %in% names(foreman_map),
            foreman_map[foreman],
            paste("Unknown FOS", foreman)
          ))
        # Keep foreman as employee numbers, add foreman_display as shortnames
      }
    }
    
    # Make sure we have entries for all years in the range
    all_years <- seq(from = as.integer(input$hist_start_year), to = as.integer(input$hist_end_year))
    
    # Create complete grid based on zone separation and grouping
    if (show_zones_separately) {
      all_combined_groups <- unique(results$combined_group)
      if (length(all_combined_groups) > 0) {
        expanded_grid <- expand.grid(
          combined_group = all_combined_groups,
          year = all_years
        )
        
        # Join with actual data
        results <- expanded_grid %>%
          left_join(results, by = c("combined_group", "year")) %>%
          mutate(
            count = ifelse(is.na(count), 0, count),
            # Extract zone from combined_group for alpha mapping
            zone_factor = gsub(".*\\(P([12])\\).*", "\\1", combined_group)
          )
      } else {
        # Handle case with no data
        results <- data.frame(combined_group = character(), year = integer(), count = integer(), zone_factor = character())
      }
    } else {
      # Create expanded grid based on grouping type
      unique_groups <- unique(results[[group_col]])
      if (length(unique_groups) > 0) {
        expanded_grid <- data.frame(
          group_val = unique_groups,
          year = rep(all_years, each = length(unique_groups))
        )
        names(expanded_grid)[1] <- group_col
        
        results <- expanded_grid %>%
          left_join(results, by = c(group_col, "year")) %>%
          mutate(count = ifelse(is.na(count), 0, count))
      } else {
        # Create empty data frame with proper column structure
        empty_df <- data.frame(year = integer(), count = integer())
        empty_df[[group_col]] <- character()
        results <- empty_df
      }
    }

    return(results)
  })
  
  # Historical plot output
  output$historicalGraph <- renderPlot({
    data <- historical_processed_data()
    if (nrow(data) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No data available for the selected criteria", cex = 1.5)
      return()
    }
    
    # Determine if we're showing zones separately
    show_zones_separately <- length(input$zone_filter) > 1
    
    # Create filter text for title
    prehatch_filter_text <- ifelse(input$prehatch_only, " (Prehatch Sites Only)", "")
    zone_filter_text <- ifelse(length(input$zone_filter) == 2, "", 
                               ifelse(length(input$zone_filter) == 1, 
                                      ifelse("1" %in% input$zone_filter, " (P1 Only)", " (P2 Only)"), 
                                      " (No Zones)"))
    
    # Get colors from shared helper functions - EXACTLY like current progress (struct_trt pattern)
    custom_colors <- if(input$group_by == "facility") {
      get_facility_base_colors()
    } else if(input$group_by == "foreman") {
      # Follow struct_trt pattern exactly - map foreman NUMBERS to facility-based colors
      foreman_colors <- get_foreman_colors()  # These are keyed by shortname
      foremen_lookup <- get_foremen_lookup()
      
      # Create mapping from foreman NUMBER to facility-based colors
      foremen_in_data <- unique(na.omit(data[[input$group_by]]))
      emp_colors <- character(0)
      
      for (foreman_num in foremen_in_data) {
        foreman_num_str <- trimws(as.character(foreman_num))
        
        # Find the shortname for this foreman number
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
        
        if(length(matches) > 0) {
          shortname <- foremen_lookup$shortname[matches[1]]
          
          # Get the facility-based color for this shortname
          if(shortname %in% names(foreman_colors)) {
            emp_colors[foreman_num_str] <- foreman_colors[shortname]
          }
        }
      }
      
      # Return emp_colors keyed by foreman numbers
      emp_colors
    } else if(input$group_by == "sectcode") {
      # Use foreman colors for sectcode grouping too
      get_foreman_colors()
    } else {
      get_facility_base_colors()  # Default fallback
    }

    # Handle color mapping for combined groups (zone separation)
    if (show_zones_separately && !is.null(custom_colors)) {
      # Extract base names from combined groups
      base_names <- unique(data$combined_group)
      combined_colors <- character(0)
      
      for (combined_name in base_names) {
        # Extract base name by removing zone info like " (P1)" or " (P2)"
        base_name <- gsub("\\s*\\([^)]+\\)$", "", combined_name)
        base_name <- trimws(base_name)
        
        # Map to existing color if available
        if (base_name %in% names(custom_colors)) {
          combined_colors[combined_name] <- custom_colors[base_name]
        }
      }
      
      # Update custom_colors to use combined group mapping
      if (length(combined_colors) > 0) {
        custom_colors <- combined_colors
      }
    }

    # Set fill variable and display names - EXACTLY like struct_trt
    if (show_zones_separately) {
      fill_var <- "combined_group"
      data$display_name <- data$combined_group
    } else {
      if (input$group_by == "facility") {
        fill_var <- "facility"
        # Add facility display names
        facilities <- get_facility_lookup()
        facility_map <- setNames(facilities$full_name, facilities$short_name)
        data$display_name <- ifelse(
          data$facility %in% names(facility_map),
          facility_map[data$facility],
          data$facility
        )
      } else if (input$group_by == "foreman") {
        fill_var <- "foreman"  # Use employee numbers for fill (to match color keys)
        # Create display names from employee numbers
        foremen_lookup <- get_foremen_lookup()
        foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
        data$display_name <- ifelse(
          is.na(data$foreman) | data$foreman == "",
          "Unassigned FOS",
          foreman_map[as.character(trimws(data$foreman))]
        )
        # Handle any remaining NAs
        data$display_name <- ifelse(
          is.na(data$display_name),
          paste0("FOS #", data$foreman),
          data$display_name
        )
      } else {
        fill_var <- input$group_by
        data$display_name <- data[[input$group_by]]
      }
    }
    
    # Validate colors and add fallback
    if (is.null(custom_colors) || length(custom_colors) == 0) {
      # Fallback to basic ggplot2 colors if no custom colors available
      unique_values <- unique(data[[fill_var]])
      if (length(unique_values) > 0) {
        # Generate basic colors
        basic_colors <- rainbow(length(unique_values))
        names(basic_colors) <- unique_values
        custom_colors <- basic_colors
      } else {
        # If still no data, skip color mapping entirely
        custom_colors <- NULL
      }
    }

    if (input$hist_show_percentages) {
      # Show as percentages
      data <- data %>%
        group_by(year) %>%
        mutate(percentage = round(count / sum(count) * 100, 1)) %>%
        ungroup()
      
      p <- ggplot(data, aes(x = year, y = percentage, fill = .data[[fill_var]])) +
        geom_col(position = "stack")
      
      # Add colors if available
      if (!is.null(custom_colors) && length(custom_colors) > 0) {
        p <- p + scale_fill_manual(values = custom_colors)
      }
      
      p <- p + labs(
        title = paste0("Drone Treatments by ", case_when(
          input$group_by == "facility" ~ "Facility",
          input$group_by == "foreman" ~ "FOS",
          input$group_by == "sectcode" ~ "Section",
          TRUE ~ "Group"
        ), " (Percentage)", zone_filter_text, prehatch_filter_text),
        x = "Year",
        y = "Percentage (%)",
        fill = case_when(
          input$group_by == "facility" ~ "Facility",
          input$group_by == "foreman" ~ "FOS",
          input$group_by == "sectcode" ~ "Section",
          TRUE ~ "Group"
        )
      ) +
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
        scale_x_continuous(breaks = seq(min(data$year), max(data$year), 1)) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    } else {
      # Show raw counts
      p <- ggplot(data, aes(x = year, y = count, fill = .data[[fill_var]])) +
        geom_col(position = "stack")
      
      # Add colors if available
      if (!is.null(custom_colors) && length(custom_colors) > 0) {
        p <- p + scale_fill_manual(values = custom_colors)
      }
      
      p <- p + labs(
        title = paste0("Drone ", ifelse(input$hist_display_metric == "treatments", "Treatments", "Sites Treated"), " by ", case_when(
          input$group_by == "facility" ~ "Facility",
          input$group_by == "foreman" ~ "FOS", 
          input$group_by == "sectcode" ~ "Section",
          TRUE ~ "Group"
        ), zone_filter_text, prehatch_filter_text),
        x = "Year", 
        y = ifelse(input$hist_display_metric == "treatments", "Number of Treatments", "Number of Sites"),
        fill = case_when(
          input$group_by == "facility" ~ "Facility",
          input$group_by == "foreman" ~ "FOS",
          input$group_by == "sectcode" ~ "Section",
          TRUE ~ "Group"
        )
      ) +
        scale_x_continuous(breaks = seq(min(data$year), max(data$year), 1)) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }
    
    print(p)
  })
  
  # Site average size graph output
  output$siteAvgSizeGraph <- renderPlot({
    data_list <- historical_raw_data()
    if (is.null(data_list)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No data available", cex = 1.5)
      return()
    }
    
    # Combine archive and current data
    archive_data <- data_list$archive %>% mutate(source = "Archive")
    current_data <- data_list$current %>% mutate(source = "Current")
    all_data <- bind_rows(archive_data, current_data)
    
    if (nrow(all_data) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No treatment data available", cex = 1.5)
      return()
    }
    
    # Join with site size data (including prehatch info)
    site_data <- all_data %>%
      inner_join(data_list$lbs, by = "sitecode") %>%
      mutate(year = year(inspdate))
    
    # Handle potential duplicate facility columns from join
    if ("facility.x" %in% names(site_data)) {
      site_data <- site_data %>%
        mutate(facility = facility.x) %>%
        select(-facility.x)
    }
    if ("facility.y" %in% names(site_data)) {
      site_data <- site_data %>%
        select(-facility.y)
    }
    
    # Apply zone filter if selected
    if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
      site_data <- site_data %>%
        filter(zone %in% input$zone_filter)
    }
    
    # Apply facility filter if selected
    if (!is.null(input$facility_filter) && length(input$facility_filter) > 0 && 
        !("all" %in% input$facility_filter)) {
      site_data <- site_data %>%
        filter(facility %in% input$facility_filter)
    }
    
    # Apply foreman/FOS filter if selected
    if (!is.null(input$foreman_filter) && length(input$foreman_filter) > 0 && 
        !("all" %in% input$foreman_filter)) {
      # Convert shortnames to employee numbers for filtering
      foremen_lookup <- get_foremen_lookup()
      selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% input$foreman_filter]
      
      site_data <- site_data %>%
        filter(foreman %in% selected_emp_nums)
    }
    
    # Apply prehatch filter if selected
    if (input$prehatch_only) {
      site_data <- site_data %>%
        filter(prehatch == 'PREHATCH')
    }
    
    # Calculate average acres by grouping and year
    show_zones_separately <- length(input$zone_filter) > 1
    
    # Determine grouping column
    group_col <- input$group_by
    if (group_col == "foreman") {
      # Only include sites that have a foreman assigned
      site_data <- site_data %>% filter(!is.na(foreman) & foreman != "")
    }
    
    # Apply grouping logic consistent with main historical processing
    if (show_zones_separately) {
      if (group_col == "facility") {
        # Create reverse mapping from full names back to short codes for color mapping
        facilities_lookup <- get_facility_lookup()
        facility_reverse_map <- setNames(facilities_lookup$short_name, facilities_lookup$full_name)
        
        site_data <- site_data %>%
          group_by(facility, zone, year) %>%
          summarize(avg_acres = mean(acres, na.rm = TRUE), .groups = "drop") %>%
          map_facility_names(facility_col = "facility") %>%
          mutate(
            combined_group = paste0(facility_display, " (P", zone, ")"),
            zone_factor = as.character(zone),
            # Create facility_zone_key using SHORT codes for color mapping
            facility_zone_key = paste0(facility, " (P", zone, ")")
          )
      } else if (group_col == "foreman") {
        # Map foreman numbers to shortnames for display
        foremen_lookup <- get_foremen_lookup()
        foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
        
        site_data <- site_data %>%
          group_by(foreman, zone, year) %>%
          summarize(avg_acres = mean(acres, na.rm = TRUE), .groups = "drop") %>%
          mutate(
            foreman_display = ifelse(
              foreman %in% names(foreman_map),
              foreman_map[foreman],
              paste("Unknown FOS", foreman)
            ),
            combined_group = paste0(foreman_display, " (P", zone, ")"),
            zone_factor = as.character(zone)
          ) %>%
          select(-foreman_display)
      } else if (group_col == "sectcode") {
        site_data <- site_data %>%
          group_by(facility, sectcode, zone, year) %>%
          summarize(avg_acres = mean(acres, na.rm = TRUE), .groups = "drop") %>%
          map_facility_names(facility_col = "facility") %>%
          mutate(
            combined_group = paste0(facility_display, " - ", sectcode, " (P", zone, ")"),
            zone_factor = as.character(zone)
          )
      }
    } else {
      if (group_col == "facility") {
        site_data <- site_data %>%
          group_by(facility, year) %>%
          summarize(avg_acres = mean(acres, na.rm = TRUE), .groups = "drop")
      } else if (group_col == "foreman") {
        # Map foreman numbers to shortnames for display
        foremen_lookup <- get_foremen_lookup()
        foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
        
        site_data <- site_data %>%
          group_by(foreman, year) %>%
          summarize(avg_acres = mean(acres, na.rm = TRUE), .groups = "drop") %>%
          mutate(foreman_display = ifelse(
            foreman %in% names(foreman_map),
            foreman_map[foreman],
            paste("Unknown FOS", foreman)
          )) %>%
          rename(foreman_original = foreman, foreman = foreman_display)
      } else if (group_col == "sectcode") {
        site_data <- site_data %>%
          group_by(facility, sectcode, year) %>%
          summarize(avg_acres = mean(acres, na.rm = TRUE), .groups = "drop") %>%
          map_facility_names(facility_col = "facility") %>%
          mutate(sectcode_display = paste0(facility_display, " - ", sectcode))
      }
    }
    
    if (nrow(site_data) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No site size data available", cex = 1.5)
      return()
    }
    
    # Create filter text for title
    prehatch_filter_text <- ifelse(input$prehatch_only, " (Prehatch Sites Only)", "")
    zone_filter_text <- ifelse(length(input$zone_filter) == 2, "", 
                               ifelse(length(input$zone_filter) == 1, 
                                      ifelse("1" %in% input$zone_filter, " (P1 Only)", " (P2 Only)"), 
                                      " (No Zones)"))
    
    # Get colors from shared helper functions - EXACTLY like current and historical (struct_trt pattern)
    custom_colors <- if(input$group_by == "facility") {
      if (show_zones_separately) {
        # For HISTORICAL: Use zone-aware facility colors with alpha differentiation
        facility_result <- get_facility_base_colors(
          alpha_zones = input$zone_filter,
          combined_groups = unique(site_data$combined_group)
        )
        facility_result$colors
      } else {
        get_facility_base_colors()
      }
    } else if(input$group_by == "foreman") {
      # Get foreman colors - use db_helpers with zone support if needed
      if (show_zones_separately) {
        # Get zone-aware foreman colors
        foreman_result <- get_foreman_colors(
          alpha_zones = input$zone_filter,
          combined_groups = unique(site_data$combined_group)
        )
        foreman_result$colors
      } else {
        # Follow struct_trt pattern exactly - map foreman NUMBERS to facility-based colors
        foreman_colors <- get_foreman_colors()  # These are keyed by shortname
        foremen_lookup <- get_foremen_lookup()
        
        # Create mapping from foreman NUMBER to facility-based colors
        foremen_in_data <- unique(na.omit(site_data$foreman))
        emp_colors <- character(0)
        
        for (foreman_num in foremen_in_data) {
          foreman_num_str <- trimws(as.character(foreman_num))
          
          # Find the shortname for this foreman number
          matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
          
          if(length(matches) > 0) {
            shortname <- foremen_lookup$shortname[matches[1]]
            
            # Get the facility-based color for this shortname
            if(shortname %in% names(foreman_colors)) {
              emp_colors[foreman_num_str] <- foreman_colors[shortname]
            }
          }
        }
        
        # Return emp_colors keyed by foreman numbers
        emp_colors
      }
    } else if(input$group_by == "sectcode") {
      # Use foreman colors for sectcode grouping too
      get_foreman_colors()
    } else {
      get_facility_base_colors()  # Default fallback
    }

    # Handle color mapping for combined groups (zone separation)
    if (show_zones_separately && !is.null(custom_colors)) {
      # Extract base names from combined groups
      base_names <- unique(site_data$combined_group)
      combined_colors <- character(0)
      
      for (combined_name in base_names) {
        # Extract base name by removing zone info like " (P1)" or " (P2)"
        base_name <- gsub("\\s*\\([^)]+\\)$", "", combined_name)
        base_name <- trimws(base_name)
        
        # Map to existing color if available
        if (base_name %in% names(custom_colors)) {
          combined_colors[combined_name] <- custom_colors[base_name]
        }
      }
      
      # Update custom_colors to use combined group mapping
      if (length(combined_colors) > 0) {
        custom_colors <- combined_colors
      }
      if (input$group_by == "facility") {
        color_var <- "facility_zone_key"  # Use short codes for color mapping
      } else {
        color_var <- "combined_group"
      }
    } else {
      # Set color variable based on grouping
      if (input$group_by == "facility") {
        color_var <- "facility"
      } else if (input$group_by == "foreman") {
        color_var <- "foreman"  # Use employee numbers for color mapping (to match struct_trt pattern)
      } else {
        color_var <- "facility"  # Use facility for sectcode grouping colors
      }
    }

    # Validate colors and add fallback for site avg graph
    if (is.null(custom_colors) || length(custom_colors) == 0) {
      # Fallback to basic ggplot2 colors if no custom colors available
      unique_values <- unique(site_data[[color_var]])
      if (length(unique_values) > 0) {
        # Generate basic colors
        basic_colors <- rainbow(length(unique_values))
        names(basic_colors) <- unique_values
        custom_colors <- basic_colors
      } else {
        # If still no data, skip color mapping entirely
        custom_colors <- NULL
      }
    }
    
    # Determine legend label based on grouping
    legend_label <- case_when(
      input$group_by == "facility" ~ "Facility",
      input$group_by == "foreman" ~ "FOS",
      input$group_by == "sectcode" ~ "Section",
      TRUE ~ "Group"
    )
    
    p <- ggplot(site_data, aes(x = year, y = avg_acres, color = .data[[color_var]])) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3)
    
    # Add colors if available
    if (!is.null(custom_colors) && length(custom_colors) > 0) {
      p <- p + scale_color_manual(values = custom_colors)
    }
    
    # Add alpha transparency for zone differentiation in historical trends
    if (show_zones_separately && input$group_by == "facility") {
      # Get alpha values for zone differentiation
      facility_result <- get_facility_base_colors(
        alpha_zones = input$zone_filter,
        combined_groups = unique(site_data$combined_group)
      )
      if (!is.null(facility_result$alpha_values)) {
        p <- p + 
          aes(alpha = zone_factor) +
          scale_alpha_manual(
            name = "Zone",
            values = facility_result$alpha_values,
            labels = c("1" = "P1 (Solid)", "2" = "P2 (Faded)"),
            drop = FALSE
          )
      }
    }
    
    p <- p + labs(
        title = paste0("Average Site Size Treated by Drone (Acres) by ", legend_label, zone_filter_text, prehatch_filter_text),
        x = "Year",
        y = "Average Acres",
        color = legend_label
      ) +
      scale_x_continuous(breaks = seq(min(site_data$year), max(site_data$year), 1)) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    print(p)
  })
  
  # Summary table output
  output$summaryTable <- renderTable({
    data <- historical_processed_data()
    if (nrow(data) == 0) {
      return(data.frame(Message = "No data available for the selected criteria"))
    }
    
    # Determine grouping column based on zone separation and grouping selection
    show_zones_separately <- length(input$zone_filter) > 1
    group_col <- input$group_by
    
    if (show_zones_separately) {
      summary_col <- "combined_group"
      summary_label <- "Group"
    } else {
      summary_col <- group_col
      summary_label <- case_when(
        group_col == "facility" ~ "Facility",
        group_col == "foreman" ~ "FOS",
        group_col == "sectcode" ~ "Section",
        TRUE ~ "Group"
      )
    }
    
    # Create summary by grouping column
    if (summary_col %in% colnames(data)) {
      summary_by_group <- data %>%
        group_by(.data[[summary_col]]) %>%
        summarize(
          Total = sum(count),
          Average = round(mean(count), 1),
          Min = min(count),
          Max = max(count),
          .groups = "drop"
        )
      names(summary_by_group)[1] <- summary_label
    } else {
      # Fallback if column doesn't exist
      summary_by_group <- data.frame()
    }
    
    # Add overall totals
    overall <- data %>%
      group_by(year) %>%
      summarize(total_year = sum(count), .groups = "drop") %>%
      summarize(
        group_name = "OVERALL",
        Total = sum(total_year),
        Average = round(mean(total_year), 1),
        Min = min(total_year),
        Max = max(total_year)
      )
    names(overall)[1] <- summary_label
    
    # Combine
    result <- bind_rows(summary_by_group, overall)
    
    # Rename for display
    result <- result %>%
      rename(
        `Total Count` = Total,
        `Avg per Year` = Average,
        `Min Year` = Min,
        `Max Year` = Max
      )
    
    return(result)
  }, striped = TRUE, hover = TRUE, spacing = "m")
  
  # Site extremes table output  
  output$siteExtremesTable <- renderTable({
    data_list <- historical_raw_data()
    if (is.null(data_list)) {
      return(data.frame(Message = "No data available"))
    }
    
    # Combine archive and current data
    archive_data <- data_list$archive %>% mutate(source = "Archive")
    current_data <- data_list$current %>% mutate(source = "Current")
    all_data <- bind_rows(archive_data, current_data)
    
    if (nrow(all_data) == 0) {
      return(data.frame(Message = "No treatment data available"))
    }
    
    # Join with site size data (including prehatch info)
    site_data <- all_data %>%
      inner_join(data_list$lbs, by = "sitecode")
    
    # Handle potential duplicate facility columns from join
    if ("facility.x" %in% names(site_data) && "facility.y" %in% names(site_data)) {
      site_data <- site_data %>%
        mutate(facility = coalesce(facility.x, facility.y)) %>%
        select(-facility.x, -facility.y)
    } else if ("facility.x" %in% names(site_data)) {
      site_data <- site_data %>%
        rename(facility = facility.x)
    } else if ("facility.y" %in% names(site_data)) {
      site_data <- site_data %>%
        rename(facility = facility.y)
    }
    
    # Select the needed columns, handling cases where some might not exist
    site_data <- site_data %>%
      select(any_of(c("sitecode", "acres", "facility", "prehatch", "zone", "foreman"))) %>%
      distinct()
    
    # Apply zone filter if selected
    if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
      site_data <- site_data %>%
        filter(zone %in% input$zone_filter)
    }
    
    # Apply facility filter if selected
    if (!is.null(input$facility_filter) && length(input$facility_filter) > 0 && 
        !("all" %in% input$facility_filter)) {
      site_data <- site_data %>%
        filter(facility %in% input$facility_filter)
    }
    
    # Apply foreman/FOS filter if selected
    if (!is.null(input$foreman_filter) && length(input$foreman_filter) > 0 && 
        !("all" %in% input$foreman_filter)) {
      # Convert shortnames to employee numbers for filtering
      foremen_lookup <- get_foremen_lookup()
      selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% input$foreman_filter]
      
      site_data <- site_data %>%
        filter(foreman %in% selected_emp_nums)
    }
    
    # Apply prehatch filter if selected
    if (input$prehatch_only) {
      site_data <- site_data %>%
        filter(prehatch == 'PREHATCH')
    }
    
    # Remove prehatch, zone, and foreman columns for final output
    site_data <- site_data %>%
      select(-prehatch, -zone, -foreman)
    
    if (nrow(site_data) == 0) {
      return(data.frame(Message = "No site size data available"))
    }
    
    # Find extremes
    largest_sites <- site_data %>%
      arrange(desc(acres)) %>%
      head(5) %>%
      mutate(Category = "Largest Sites") %>%
      select(Category, sitecode, acres, facility)
    
    smallest_sites <- site_data %>%
      arrange(acres) %>%
      head(5) %>%
      mutate(Category = "Smallest Sites") %>%
      select(Category, sitecode, acres, facility)
    
    # Combine
    result <- bind_rows(largest_sites, smallest_sites) %>%
      rename(
        Type = Category,
        `Site Code` = sitecode,
        `Acres` = acres,
        `Facility` = facility
      )
    
    return(result)
  }, striped = TRUE, hover = TRUE, spacing = "m")
}

# Run the application
shinyApp(ui = ui, server = server)

