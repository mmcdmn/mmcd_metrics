# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
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
      
      # Group by option
      radioButtons("group_by", "Group by:",
                  choices = c("Facility" = "facility", 
                             "FOS" = "foreman",
                             "Section" = "sectcode"),
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
server <- function(input, output) {
  
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
    
    # Add combined group column for zone differentiation when both zones selected
    show_zones_separately <- length(input$zone_filter) > 1
    if (show_zones_separately) {
      drone_sites$combined_group <- paste0(drone_sites[[group_col]], " (P", drone_sites$zone, ")")
      drone_treatments$combined_group <- paste0(drone_treatments[[group_col]], " (P", drone_treatments$zone, ")")
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
      
      # Add the original group column for color mapping and zone factor
      if (group_col == "facility") {
        combined_data <- combined_data %>%
          mutate(
            facility = gsub(" \\(P[12]\\)", "", combined_group),
            zone_factor = gsub(".*\\(P([12])\\).*", "\\1", combined_group)
          ) %>%
          map_facility_names(facility_col = "facility")
      } else if (group_col == "foreman") {
        combined_data <- combined_data %>%
          mutate(
            foreman = gsub(" \\(P[12]\\)", "", combined_group),
            zone_factor = gsub(".*\\(P([12])\\).*", "\\1", combined_group)
          )
      }
    } else {
      # Add facility display names when not showing zones separately
      if (group_col == "facility") {
        combined_data <- combined_data %>% map_facility_names(facility_col = "facility")
      } else if (group_col == "foreman") {
        combined_data <- combined_data %>% mutate(foreman_display = paste0("FOS #", foreman))
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
    status_colors <- get_status_colors()
    
    # Map colors for this application's specific needs
    total_color <- status_colors["unknown"]        # Gray for total sites
    active_color <- status_colors["active"]        # Green for active treatments
    expiring_color <- status_colors["planned"]     # Orange for expiring treatments
    
    # Determine x-axis variable and get facility colors
    if (show_zones_separately) {
      x_var <- "combined_group"
      # Get zone-aware facility colors
      facility_colors <- get_facility_base_colors(
        alpha_zones = input$zone_filter,
        combined_groups = unique(data$combined_group)
      )
      
      # For the stacked bars, we need to apply alpha to the fill colors manually
      # since geom_bar doesn't work well with alpha aesthetics for this use case
      total_color <- status_colors["unknown"]
      active_color <- status_colors["active"] 
      expiring_color <- status_colors["planned"]
    } else {
      # Determine x-axis variable based on grouping
      if (input$group_by == "facility") {
        x_var <- "facility_display"
      } else if (input$group_by == "foreman") {
        x_var <- "foreman_display"
      } else if (input$group_by == "sectcode") {
        x_var <- "sectcode_display"
      }
      
      # Get standard facility colors
      facility_colors <- get_facility_base_colors()
      
      total_color <- status_colors["unknown"]
      active_color <- status_colors["active"]
      expiring_color <- status_colors["planned"]
    }
    
    # Create the plot
    p <- ggplot(data, aes_string(x = x_var)) +
      # First draw total bars
      geom_bar(aes(y = y_total), stat = "identity", fill = total_color, alpha = 0.7) +
      # Then overlay active bars
      geom_bar(aes(y = y_active), stat = "identity", fill = active_color) +
      # Finally overlay expiring bars
      geom_bar(aes(y = y_expiring), stat = "identity", fill = expiring_color) +
      
      # Add labels on top of each bar
      geom_text(aes(y = y_total, label = y_total), vjust = -0.5, color = "black") +
      # Add expiring labels
      geom_text(aes(y = y_expiring, label = y_expiring), vjust = 1.5, color = "black", fontface = "bold")
    
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
    
    # Add active labels only where they differ from expiring (safer approach)
    if (any(data$show_active_label)) {
      p <- p + geom_text(data = data[data$show_active_label,],
                         aes(y = y_active, label = y_active),
                         vjust = -0.5, color = active_color, fontface = "bold")
    }
    
    # Add legend manually using mapped colors
    p + annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.9, ymax = y_max * 0.95,
                 fill = total_color, alpha = 0.7) +
      annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.8, ymax = y_max * 0.85,
               fill = active_color) +
      annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.7, ymax = y_max * 0.75,
               fill = expiring_color) +
      annotate("text", x = 0.1, y = y_max * 0.925,
               label = "Total", hjust = 0) +
      annotate("text", x = 0.1, y = y_max * 0.825,
               label = "Active", hjust = 0) +
      annotate("text", x = 0.1, y = y_max * 0.725,
               label = "Expiring", hjust = 0)
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
        "SELECT b.sitecode, b.acres, b.facility, b.prehatch, b.drone, sc.zone
        FROM public.loc_breeding_sites b
        LEFT JOIN public.gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
        WHERE b.sitecode IN (%s)
        AND (b.drone IN (%s) OR b.air_gnd = 'D')
        AND b.enddate IS NULL",
        sitecodes_str, drone_types)
      lbs_data <- dbGetQuery(con, lbs_query)
    } else {
      lbs_data <- data.frame(sitecode=character(), acres=numeric(), facility=character(), prehatch=character(), drone=character(), zone=character())
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
        # Extract sectcode from sitecode (first 7 characters)
        sectcode = substr(sitecode, 1, 7)
      )
    
    # Apply zone filter if selected
    if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
      all_data <- all_data %>%
        filter(zone %in% input$zone_filter)
    }
    
    # Apply prehatch filter if selected
    if (input$prehatch_only) {
      all_data <- all_data %>%
        filter(prehatch == 'PREHATCH')
    }
    
    # Process based on count type and grouping selection
    show_zones_separately <- length(input$zone_filter) > 1
    
    # Define grouping based on group_by selection and zone separation
    if (input$group_by == "facility") {
      if (show_zones_separately) {
        # Group by facility, zone, and year when showing multiple zones
        if (input$hist_display_metric == "treatments") {
          # Count all treatments
          results <- all_data %>%
            group_by(facility, zone, year) %>%
            summarize(count = n(), .groups = "drop") %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(combined_group = paste0(facility_display, " (P", zone, ")"))
        } else {
          # Count unique sites
          results <- all_data %>%
            group_by(facility, zone, year) %>%
            summarize(count = n_distinct(sitecode), .groups = "drop") %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(combined_group = paste0(facility_display, " (P", zone, ")"))
        }
      } else {
        # Group only by facility and year when showing single zone
        if (input$hist_display_metric == "treatments") {
          # Count all treatments
          results <- all_data %>%
            group_by(facility, year) %>%
            summarize(count = n(), .groups = "drop") %>%
            map_facility_names(facility_col = "facility")
        } else {
          # Count unique sites
          results <- all_data %>%
            group_by(facility, year) %>%
            summarize(count = n_distinct(sitecode), .groups = "drop") %>%
            map_facility_names(facility_col = "facility")
        }
      }
    } else if (input$group_by == "foreman") {
      # Note: Foreman data is not available in historical data, fallback to facility grouping
      if (show_zones_separately) {
        # Group by facility and zone when foreman not available
        if (input$hist_display_metric == "treatments") {
          results <- all_data %>%
            group_by(facility, zone, year) %>%
            summarize(count = n(), .groups = "drop") %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(combined_group = paste0(facility_display, " (P", zone, ") [FOS data not available]"))
        } else {
          results <- all_data %>%
            group_by(facility, zone, year) %>%
            summarize(count = n_distinct(sitecode), .groups = "drop") %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(combined_group = paste0(facility_display, " (P", zone, ") [FOS data not available]"))
        }
      } else {
        # Group only by facility when foreman not available
        if (input$hist_display_metric == "treatments") {
          results <- all_data %>%
            group_by(facility, year) %>%
            summarize(count = n(), .groups = "drop") %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(facility_display = paste0(facility_display, " [FOS data not available]"))
        } else {
          results <- all_data %>%
            group_by(facility, year) %>%
            summarize(count = n_distinct(sitecode), .groups = "drop") %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(facility_display = paste0(facility_display, " [FOS data not available]"))
        }
      }
    } else if (input$group_by == "sectcode") {
      if (show_zones_separately) {
        # Group by facility, sectcode, zone, and year when showing multiple zones
        if (input$hist_display_metric == "treatments") {
          # Count all treatments
          results <- all_data %>%
            group_by(facility, sectcode, zone, year) %>%
            summarize(count = n(), .groups = "drop") %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(combined_group = paste0(facility_display, " - ", sectcode, " (P", zone, ")"))
        } else {
          # Count unique sites
          results <- all_data %>%
            group_by(facility, sectcode, zone, year) %>%
            summarize(count = n_distinct(sitecode), .groups = "drop") %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(combined_group = paste0(facility_display, " - ", sectcode, " (P", zone, ")"))
        }
      } else {
        # Group by facility, sectcode and year when showing single zone
        if (input$hist_display_metric == "treatments") {
          # Count all treatments
          results <- all_data %>%
            group_by(facility, sectcode, year) %>%
            summarize(count = n(), .groups = "drop") %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(sectcode_display = paste0(facility_display, " - ", sectcode))
        } else {
          # Count unique sites
          results <- all_data %>%
            group_by(facility, sectcode, year) %>%
            summarize(count = n_distinct(sitecode), .groups = "drop") %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(sectcode_display = paste0(facility_display, " - ", sectcode))
        }
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
      if (input$group_by == "facility") {
        unique_groups <- unique(results$facility)
        if (length(unique_groups) > 0) {
          expanded_grid <- expand.grid(
            facility = unique_groups,
            year = all_years
          )
          results <- expanded_grid %>%
            left_join(results, by = c("facility", "year")) %>%
            mutate(count = ifelse(is.na(count), 0, count))
        } else {
          results <- data.frame(facility = character(), year = integer(), count = integer())
        }
      } else if (input$group_by == "foreman") {
        # Since foreman data isn't available in historical data, we fallback to facility grouping
        unique_groups <- unique(results$facility)
        if (length(unique_groups) > 0) {
          expanded_grid <- expand.grid(
            facility = unique_groups,
            year = all_years
          )
          results <- expanded_grid %>%
            left_join(results, by = c("facility", "year")) %>%
            mutate(count = ifelse(is.na(count), 0, count)) %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(facility_display = paste0(facility_display, " [FOS data not available]"))
        } else {
          results <- data.frame(facility = character(), year = integer(), count = integer())
        }
      } else if (input$group_by == "sectcode") {
        unique_groups <- unique(results[c("facility", "sectcode")])
        if (nrow(unique_groups) > 0) {
          expanded_grid <- expand.grid(
            facility = unique(unique_groups$facility),
            sectcode = unique(unique_groups$sectcode),
            year = all_years
          )
          results <- expanded_grid %>%
            left_join(results, by = c("facility", "sectcode", "year")) %>%
            mutate(count = ifelse(is.na(count), 0, count)) %>%
            map_facility_names(facility_col = "facility") %>%
            mutate(sectcode_display = paste0(facility_display, " - ", sectcode))
        } else {
          results <- data.frame(facility = character(), sectcode = character(), year = integer(), count = integer())
        }
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
    
    # Get facility colors
    if (show_zones_separately) {
      facility_colors <- get_facility_base_colors(
        alpha_zones = input$zone_filter,
        combined_groups = unique(data$combined_group)
      )
      fill_var <- "combined_group"
      color_values <- facility_colors$colors
      alpha_var <- "zone_factor"
      alpha_values <- facility_colors$alpha_values
    } else {
      # Determine fill variable and colors based on grouping
      if (input$group_by == "facility") {
        facility_colors <- get_facility_base_colors()
        fill_var <- "facility"
        color_values <- facility_colors
      } else if (input$group_by == "foreman") {
        # Since foreman data isn't available in historical data, fallback to facility colors
        facility_colors <- get_facility_base_colors()
        fill_var <- "facility"
        color_values <- facility_colors
      } else if (input$group_by == "sectcode") {
        facility_colors <- get_facility_base_colors()
        fill_var <- "facility"  # Use facility for sectcode grouping colors
        color_values <- facility_colors
      }
      alpha_var <- NULL
      alpha_values <- NULL
    }
    
    # Check if data exists and has the required columns
    if (nrow(data) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No historical data available for the selected criteria", cex = 1.5)
      return()
    }
    
    # Validate that color_values has the right structure
    if (is.null(color_values) || length(color_values) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Color mapping error - please try a different grouping", cex = 1.5)
      return()
    }
    
    if (input$hist_show_percentages) {
      # Show as percentages
      data <- data %>%
        group_by(year) %>%
        mutate(percentage = round(count / sum(count) * 100, 1)) %>%
        ungroup()
      
      if (show_zones_separately) {
        p <- ggplot(data, aes_string(x = "year", y = "percentage", fill = fill_var, alpha = alpha_var)) +
          geom_col(position = "stack") +
          scale_fill_manual(values = color_values) +
          scale_alpha_manual(values = alpha_values) +
          guides(alpha = "none") +  # Hide alpha legend
          labs(
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
          )
      } else {
        p <- ggplot(data, aes_string(x = "year", y = "percentage", fill = fill_var)) +
          geom_col(position = "stack") +
          scale_fill_manual(values = color_values) +
          labs(
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
          )
      }
      
      p <- p +
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
        scale_x_continuous(breaks = seq(min(data$year), max(data$year), 1)) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    } else {
      # Show raw counts
      if (show_zones_separately) {
        p <- ggplot(data, aes_string(x = "year", y = "count", fill = fill_var, alpha = alpha_var)) +
          geom_col(position = "stack") +
          scale_fill_manual(values = color_values) +
          scale_alpha_manual(values = alpha_values) +
          guides(alpha = "none") +  # Hide alpha legend
          labs(
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
          )
      } else {
        p <- ggplot(data, aes_string(x = "year", y = "count", fill = fill_var)) +
          geom_col(position = "stack") +
          scale_fill_manual(values = color_values) +
          labs(
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
          )
      }
      
      p <- p +
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
    
    # Apply zone filter if selected
    if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
      site_data <- site_data %>%
        filter(zone %in% input$zone_filter)
    }
    
    # Apply prehatch filter if selected
    if (input$prehatch_only) {
      site_data <- site_data %>%
        filter(prehatch == 'PREHATCH')
    }
    
    # Calculate average acres by facility and year
    show_zones_separately <- length(input$zone_filter) > 1
    
    if (show_zones_separately) {
      site_data <- site_data %>%
        group_by(facility.x, zone, year) %>%
        summarize(avg_acres = mean(acres, na.rm = TRUE), .groups = "drop") %>%
        rename(facility = facility.x) %>%
        mutate(
          combined_group = paste0(facility, " (P", zone, ")"),
          zone_factor = as.character(zone)
        )
    } else {
      site_data <- site_data %>%
        group_by(facility.x, year) %>%
        summarize(avg_acres = mean(acres, na.rm = TRUE), .groups = "drop") %>%
        rename(facility = facility.x)
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
    
    # Get facility colors for site data plot
    if (show_zones_separately) {
      facility_colors <- get_facility_base_colors(
        alpha_zones = input$zone_filter,
        combined_groups = unique(site_data$combined_group)
      )
      color_var <- "combined_group"
      color_values <- facility_colors$colors
      alpha_var <- "zone_factor"
      alpha_values <- facility_colors$alpha_values
    } else {
      # Determine color variable and colors based on grouping
      if (input$group_by == "facility") {
        facility_colors <- get_facility_base_colors()
        color_var <- "facility"
        color_values <- facility_colors
      } else if (input$group_by == "foreman") {
        # Check if foreman column exists and has data
        if ("foreman" %in% colnames(site_data) && length(unique(site_data$foreman)) > 0) {
          color_var <- "foreman"
          # Create a simple color palette for foremen
          unique_foremen <- unique(site_data$foreman)
          color_values <- rainbow(length(unique_foremen))
          names(color_values) <- unique_foremen
        } else {
          # Fallback to facility colors if foreman data not available
          facility_colors <- get_facility_base_colors()
          color_var <- "facility"
          color_values <- facility_colors
        }
      } else if (input$group_by == "sectcode") {
        facility_colors <- get_facility_base_colors()
        color_var <- "facility"  # Use facility for sectcode grouping colors
        color_values <- facility_colors
      }
      alpha_var <- NULL
      alpha_values <- NULL
    }
    
    if (show_zones_separately) {
      p <- ggplot(site_data, aes_string(x = "year", y = "avg_acres", color = color_var, alpha = alpha_var)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = color_values) +
        scale_alpha_manual(values = alpha_values) +
        guides(alpha = "none") +  # Hide alpha legend
        labs(
          title = paste0("Average Site Size Treated by Drone (Acres)", zone_filter_text, prehatch_filter_text),
          x = "Year",
          y = "Average Acres",
          color = "Facility"
        )
    } else {
      p <- ggplot(site_data, aes_string(x = "year", y = "avg_acres", color = color_var)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = color_values) +
        labs(
          title = paste0("Average Site Size Treated by Drone (Acres)", zone_filter_text, prehatch_filter_text),
          x = "Year",
          y = "Average Acres",
          color = "Facility"
        )
    }
    
    p <- p +
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
    
    # Create summary by facility and overall
    summary_by_facility <- data %>%
      group_by(facility) %>%
      summarize(
        Total = sum(count),
        Average = round(mean(count), 1),
        Min = min(count),
        Max = max(count),
        .groups = "drop"
      )
    
    # Add overall totals
    overall <- data %>%
      group_by(year) %>%
      summarize(total_year = sum(count), .groups = "drop") %>%
      summarize(
        facility = "OVERALL",
        Total = sum(total_year),
        Average = round(mean(total_year), 1),
        Min = min(total_year),
        Max = max(total_year)
      )
    
    # Combine
    result <- bind_rows(summary_by_facility, overall)
    
    # Rename for display
    result <- result %>%
      rename(
        Facility = facility,
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
      inner_join(data_list$lbs, by = "sitecode") %>%
      select(sitecode, acres, facility.x, prehatch, zone) %>%
      distinct() %>%
      rename(facility = facility.x)
    
    # Apply zone filter if selected
    if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
      site_data <- site_data %>%
        filter(zone %in% input$zone_filter)
    }
    
    # Apply prehatch filter if selected
    if (input$prehatch_only) {
      site_data <- site_data %>%
        filter(prehatch == 'PREHATCH')
    }
    
    # Remove prehatch and zone columns for final output
    site_data <- site_data %>%
      select(-prehatch, -zone)
    
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

