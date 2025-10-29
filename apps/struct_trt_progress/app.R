# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(scales)  # For percent_format
  library(rlang)  # For sym() function
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")

# Load environment variables from .env file (for local development)
# or from Docker environment variables (for production)
env_paths <- c(
  "../../.env",           # For local development
  "../../../.env",        # Alternative local path
  "/srv/shiny-server/.env" # Docker path
)

# Try to load from .env file first
env_loaded <- FALSE
for (path in env_paths) {
  if (file.exists(path)) {
    readRenviron(path)
    env_loaded <- TRUE
    break
  }
}

# If no .env file found, environment variables should already be set by Docker

# Database configuration using environment variables
db_host <- Sys.getenv("DB_HOST")
db_port <- Sys.getenv("DB_PORT")
db_user <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")
db_name <- Sys.getenv("DB_NAME")

# Define UI for the application
ui <- fluidPage(
  # Application title
  titlePanel("Structure Treatment Progress and History"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      # Progress-specific controls
      conditionalPanel(
        condition = "input.graph_tabs == 'Current Progress'",
        # Slider to control the expiration window
        sliderInput("expiring_days", "Days Until Expiration:",
                    min = 1, max = 30, value = 7, step = 1),
        
        # Date input to simulate "today"
        dateInput("custom_today", "Pretend Today is:",
                  value = Sys.Date(), 
                  format = "yyyy-mm-dd")
      ),
      
      # Toggle for structure status types
      checkboxGroupInput("status_types", "Include Structure Status:",
                         choices = c("Dry (D)" = "D",
                                     "Wet (W)" = "W", 
                                     "Unknown (U)" = "U"),
                         selected = c("D", "W", "U")),
      
      # Dropdown for facility filter - using db_helpers to get full names (multi-select)
      selectizeInput("facility_filter", "Filter by Facility:",
                  choices = c("All"),  # Will be populated from get_facility_lookup()
                  selected = "All", multiple = TRUE),
      
      # Group by selection
      selectInput("group_by", "Group by:",
                  choices = c("Facility" = "facility",
                              "FOS" = "foreman", 
                              "All MMCD" = "mmcd_all"),
                  selected = "facility"),
      
      # Zone filter checkboxes
      checkboxGroupInput("zone_filter", "Zones:",
                         choices = c("P1" = "1", "P2" = "2"),
                         selected = c("1", "2")),
      
      # Dropdown for structure type filter  
      selectInput("structure_type_filter", "Structure Type:",
                  choices = c("All" = "all", "AP", "CB", "CG", "cv", "CV", "CV/PR", "CV/RR", "DR", "PC", "Pool", "PR", "RG", "RR", "SP", "SS", "US", "W", "wo", "WO", "XX"),
                  selected = "all"),
      
      # Dropdown for priority filter
      selectInput("priority_filter", "Priority:",
                  choices = c("All" = "all", "BLUE", "GREEN", "RED", "YELLOW"),
                  selected = "all"),
      
      # Conditional help text based on active tab
      conditionalPanel(
        condition = "input.graph_tabs == 'Current Progress'",
        helpText("This visualization shows structures by facility with three categories:",
                 tags$br(),
                 tags$ul(
                   tags$li(tags$span(style = "color:gray", "Gray: Total structures")),
                   tags$li(tags$span(style = paste0("color:", get_status_colors()["active"]), "Green: Structures with active treatments")),
                   tags$li(tags$span(style = paste0("color:", get_status_colors()["planned"]), "Orange: Structures with treatments expiring within the selected days"))
                 )),
        
        helpText(tags$b("Date Simulation:"),
                 tags$br(),
                 "Use 'Pretend Today is' to see what treatments would be active/expiring on any specific date. Only treatments that would still be active on that date are considered.")
      ),
      
      conditionalPanel(
        condition = "input.graph_tabs == 'Historical Trends'",
        helpText("This visualization shows the historical proportion of structures with active treatment over time.",
                 tags$br(),
                 "Blue line: actual proportion | Red dashed line: seasonal average")
      ),
      
      helpText(tags$b("Structure Status:"),
               tags$br(),
               tags$ul(
                 tags$li(tags$b("D:"), "Dry - Structure is dry"),
                 tags$li(tags$b("W:"), "Wet - Structure has water"),
                 tags$li(tags$b("U:"), "Unknown - Status not determined")
               ))
    ),
    
    # Main panel for displaying the graph
    mainPanel(
      tabsetPanel(
        id = "graph_tabs",
        tabPanel("Current Progress", 
          plotOutput("structureGraph", height = "600px")
        ),
        tabPanel("Historical Trends", 
          # Add controls specific to historical view
          fluidRow(
            column(6,
              selectInput("start_year", "Start Year:",
                         choices = seq(2010, 2025), selected = 2018)
            ),
            column(6,
              selectInput("end_year", "End Year:",
                         choices = seq(2010, 2025), selected = 2025)
            )
          ),
          plotOutput("historyGraph", height = "600px")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Initialize facility choices from db_helpers
  observe({
    facilities <- get_facility_lookup()
    # Create named vector with full names as labels and short names as values
    facility_choices <- c("All" = "All")
    facility_choices <- c(
      facility_choices,
      setNames(facilities$short_name, facilities$full_name)
    )
    updateSelectizeInput(session, "facility_filter", choices = facility_choices)
  })
  
  # Fetch data from database
  raw_data <- reactive({
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = db_name,
      host = db_host,
      port = as.numeric(db_port),
      user = db_user,
      password = db_password
    )
    
    # Build the status filter based on user selection
    status_types <- paste0("'", paste(input$status_types, collapse = "','"), "'")
    
    # Build structure type filter condition  
    struct_type_condition <- if (input$structure_type_filter == "all") "" else sprintf("AND s_type = '%s'", input$structure_type_filter)
    
    # Build priority filter condition
    priority_condition <- if (input$priority_filter == "all") "" else sprintf("AND priority = '%s'", input$priority_filter)
    
    # Query to get structures from loc_cxstruct with zone and foreman information
    structures_query <- sprintf("
SELECT s.sitecode, s.facility, s.status_udw, s.s_type, s.priority,
CASE 
  WHEN h.foreman IS NOT NULL AND h.foreman != '' THEN h.foreman 
  ELSE NULL
END as foreman,
g.zone
FROM public.loc_cxstruct s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND CURRENT_DATE >= h.startdate 
  AND (h.enddate IS NULL OR CURRENT_DATE <= h.enddate)
LEFT JOIN public.gis_sectcode g ON LEFT(s.sitecode, 6) || '-' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'N' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'S' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'E' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'W' = g.sectcode
WHERE (s.status_udw IN (%s) OR s.status_udw IS NULL)
AND (s.enddate IS NULL OR s.enddate > CURRENT_DATE)
%s
%s
", status_types, struct_type_condition, priority_condition)
    
    structures <- dbGetQuery(con, structures_query)
    
    # Query to get treatment information from current treatments
    treatments_query <- "
SELECT t.sitecode, t.facility, t.inspdate, t.matcode, m.effect_days
FROM public.dblarv_insptrt_current t
LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
WHERE t.list_type = 'STR'
"
    treatments <- dbGetQuery(con, treatments_query)
    
    dbDisconnect(con)
    
    # Process the data
    # Identify structures with treatments
    structure_treatments <- treatments %>%
      inner_join(structures, by = c("sitecode", "facility"))
    
    # Calculate treatment status (active) - will be recalculated in processed_data with custom date
    structure_treatments <- structure_treatments %>%
      mutate(
        inspdate = as.Date(inspdate),
        effect_days = ifelse(is.na(effect_days), 0, effect_days),
        treatment_end_date = inspdate + effect_days
      )
    
    # Return all the data needed for filtering later
    list(
      structures = structures,
      structure_treatments = structure_treatments
    )
  })
  
  # Process data based on user inputs
  processed_data <- reactive({
    # Get raw data
    data <- raw_data()
    structures <- data$structures
    structure_treatments <- data$structure_treatments
    
    # Filter by zone if selected
    if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
      structures <- structures %>% filter(zone %in% input$zone_filter)
      structure_treatments <- structure_treatments %>% filter(zone %in% input$zone_filter)
    }
    
    # Filter by facility if selected (like suco_history)
    if (!is.null(input$facility_filter) && !("All" %in% input$facility_filter)) {
      structures <- structures %>% filter(facility %in% input$facility_filter)
      structure_treatments <- structure_treatments %>% filter(facility %in% input$facility_filter)
    }
    
    # Use custom date from input
    current_date <- as.Date(input$custom_today)
    
    # Calculate expiring window
    expiring_start_date <- current_date
    expiring_end_date <- current_date + input$expiring_days
    
    # Update treatment status based on custom date
    structure_treatments <- structure_treatments %>%
      mutate(
        is_active = treatment_end_date >= current_date,
        is_expiring = treatment_end_date >= expiring_start_date &
          treatment_end_date <= expiring_end_date &
          treatment_end_date >= current_date
      )
    
    # Determine grouping column
    group_col <- input$group_by
    if (group_col == "mmcd_all") {
      # Add a constant column for "All MMCD" grouping
      structures$mmcd_all <- "All MMCD"
      structure_treatments$mmcd_all <- "All MMCD"
    }
    
    # Add combined group column for zone differentiation when both zones selected
    if (length(input$zone_filter) > 1) {
      structures$combined_group <- paste0(structures[[group_col]], " (P", structures$zone, ")")
      structure_treatments$combined_group <- paste0(structure_treatments[[group_col]], " (P", structure_treatments$zone, ")")
    }
    
    # Count total structures by group
    total_structures <- structures %>%
      group_by(!!sym(group_col)) %>%
      summarize(
        total_structures = n(),
        .groups = 'drop'
      )
    
    # Count active structures by group
    # Need to handle overlapping treatments by taking distinct sitecodes only
    active_structures <- structure_treatments %>%
      filter(is_active == TRUE) %>%
      distinct(sitecode, !!sym(group_col)) %>%
      group_by(!!sym(group_col)) %>%
      summarize(
        active_structures = n(),
        .groups = 'drop'
      )
    
    # Count expiring structures by group
    expiring_structures <- structure_treatments %>%
      filter(is_expiring == TRUE) %>%
      distinct(sitecode, !!sym(group_col)) %>%
      group_by(!!sym(group_col)) %>%
      summarize(
        expiring_structures = n(),
        .groups = 'drop'
      )
    
    # Combine all counts by group
    combined_data <- total_structures %>%
      left_join(active_structures, by = group_col) %>%
      left_join(expiring_structures, by = group_col) %>%
      mutate(
        active_structures = ifelse(is.na(active_structures), 0, active_structures),
        expiring_structures = ifelse(is.na(expiring_structures), 0, expiring_structures)
      )
    
    # Add zone information when both zones are selected
    if (length(input$zone_filter) > 1) {
      # Recalculate with combined groups for zone differentiation
      total_by_combined <- structures %>%
        group_by(combined_group) %>%
        summarize(total_structures = n(), .groups = 'drop')
      
      active_by_combined <- structure_treatments %>%
        filter(is_active == TRUE) %>%
        distinct(sitecode, combined_group) %>%
        group_by(combined_group) %>%
        summarize(active_structures = n(), .groups = 'drop')
      
      expiring_by_combined <- structure_treatments %>%
        filter(is_expiring == TRUE) %>%
        distinct(sitecode, combined_group) %>%
        group_by(combined_group) %>%
        summarize(expiring_structures = n(), .groups = 'drop')
      
      combined_data <- total_by_combined %>%
        left_join(active_by_combined, by = "combined_group") %>%
        left_join(expiring_by_combined, by = "combined_group") %>%
        mutate(
          active_structures = ifelse(is.na(active_structures), 0, active_structures),
          expiring_structures = ifelse(is.na(expiring_structures), 0, expiring_structures)
        )
      
      # Add the original group column for color mapping
      combined_data[[group_col]] <- gsub("\\s*\\([^)]+\\)$", "", combined_data$combined_group)
    }
    
    return(combined_data)
  })
  
  # Historical data reactive for the Historical Trends tab
  historical_data <- reactive({
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = db_name,
      host = db_host,
      port = as.numeric(db_port),
      user = db_user,
      password = db_password
    )
    
    # Build the status filter based on user selection
    status_types <- paste0("'", paste(input$status_types, collapse = "','"), "'")
    
    # Build structure type filter condition  
    struct_type_condition <- if (input$structure_type_filter == "all") "" else sprintf("AND s.s_type = '%s'", input$structure_type_filter)
    
    # Build priority filter condition
    priority_condition <- if (input$priority_filter == "all") "" else sprintf("AND s.priority = '%s'", input$priority_filter)
    
    # Fetch archive data
    query_archive <- sprintf("
SELECT
  t.sitecode,
  t.inspdate,
  COALESCE(m.effect_days, 0) AS effect_days,
  s.facility,
  CASE 
    WHEN h.foreman IS NOT NULL AND h.foreman != '' THEN h.foreman 
    ELSE NULL
  END as foreman,
  g.zone
FROM public.dblarv_insptrt_archive t
LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
LEFT JOIN public.loc_cxstruct s ON t.sitecode = s.sitecode
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND t.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR t.inspdate <= h.enddate)
LEFT JOIN public.gis_sectcode g ON LEFT(s.sitecode, 6) || '-' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'N' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'S' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'E' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'W' = g.sectcode
WHERE t.inspdate >= date '%d-01-01'
AND t.inspdate < date '%d-01-01'
AND t.list_type = 'STR'
AND (s.status_udw IN (%s) OR s.status_udw IS NULL)
%s
%s
", as.numeric(input$start_year), as.numeric(input$end_year) + 1, status_types, struct_type_condition, priority_condition)
    
    archive_data <- dbGetQuery(con, query_archive)
    
    # Fetch current data
    query_current <- sprintf("
SELECT
  t.sitecode,
  t.inspdate,
  COALESCE(m.effect_days, 0) AS effect_days,
  s.facility,
  CASE 
    WHEN h.foreman IS NOT NULL AND h.foreman != '' THEN h.foreman 
    ELSE NULL
  END as foreman,
  g.zone
FROM public.dblarv_insptrt_current t
LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
LEFT JOIN public.loc_cxstruct s ON t.sitecode = s.sitecode
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND t.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR t.inspdate <= h.enddate)
LEFT JOIN public.gis_sectcode g ON LEFT(s.sitecode, 6) || '-' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'N' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'S' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'E' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'W' = g.sectcode
WHERE t.inspdate >= date '%d-01-01'
AND t.inspdate < date '%d-01-01'
AND t.list_type = 'STR'
AND (s.status_udw IN (%s) OR s.status_udw IS NULL)
%s
%s
", as.numeric(input$start_year), as.numeric(input$end_year) + 1, status_types, struct_type_condition, priority_condition)
    
    current_data <- dbGetQuery(con, query_current)
    
    # Combine archive and current data
    all_treatments <- rbind(archive_data, current_data)
    
    # Get total structures for proportion calculation
    total_structures_query <- sprintf("
SELECT 
  s.sitecode,
  s.facility,
  CASE 
    WHEN h.foreman IS NOT NULL AND h.foreman != '' THEN h.foreman 
    ELSE NULL
  END as foreman,
  g.zone
FROM public.loc_cxstruct s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND CURRENT_DATE >= h.startdate 
  AND (h.enddate IS NULL OR CURRENT_DATE <= h.enddate)
LEFT JOIN public.gis_sectcode g ON LEFT(s.sitecode, 6) || '-' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'N' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'S' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'E' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'W' = g.sectcode
WHERE (s.status_udw IN (%s) OR s.status_udw IS NULL)
AND (s.enddate IS NULL OR s.enddate > CURRENT_DATE)
%s
%s
", status_types, struct_type_condition, priority_condition)
    
    total_structures <- dbGetQuery(con, total_structures_query)
    
    dbDisconnect(con)
    
    return(list(treatments = all_treatments, total_structures = total_structures))
  })
  
  # Process historical data based on filters
  processed_historical_data <- reactive({
    data_list <- historical_data()
    treatments <- data_list$treatments
    total_structures <- data_list$total_structures
    
    # Filter by facility if selected (same logic as current progress)
    if (!is.null(input$facility_filter) && length(input$facility_filter) > 0 && !"All" %in% input$facility_filter) {
      treatments <- treatments %>% filter(facility %in% input$facility_filter)
      total_structures <- total_structures %>% filter(facility %in% input$facility_filter)
    }
    
    # Filter by zone if selected
    if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
      treatments <- treatments %>% filter(zone %in% input$zone_filter)
      total_structures <- total_structures %>% filter(zone %in% input$zone_filter)
    }
    
    # Calculate treatment end dates and determine which treatments were active each month
    treatments <- treatments %>%
      mutate(
        inspdate = as.Date(inspdate),
        treatment_end_date = inspdate + effect_days
      )
    
    # Create monthly sequence
    start_date <- as.Date(paste0(input$start_year, "-01-01"))
    end_date <- as.Date(paste0(input$end_year, "-12-31"))
    monthly_dates <- seq(start_date, end_date, by = "month")
    
    # For each month, calculate how many structures had active treatment
    monthly_data <- data.frame()
    
    for (month_date in monthly_dates) {
      month_date <- as.Date(month_date)
      
      # Find treatments that were active during this month
      active_treatments <- treatments %>%
        filter(
          inspdate <= month_date,
          treatment_end_date >= month_date
        ) %>%
        distinct(sitecode)
      
      # Count total structures for this time period
      total_count <- nrow(total_structures)
      
      # Count treated structures for this month
      treated_count <- nrow(active_treatments)
      
      monthly_data <- rbind(monthly_data, data.frame(
        date = month_date,
        total_structures = total_count,
        treated_structures = treated_count,
        proportion_active_treatment = if(total_count > 0) treated_count / total_count else 0
      ))
    }
    
    return(monthly_data)
  })
  
  # Generate the plot
  output$structureGraph <- renderPlot({
    # Get the processed data
    data <- processed_data()
    
    # Handle case when no data is available (e.g., no structures with selected filters)
    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No data available with the selected filters", size = 6) +
          theme_void()
      )
    }
    
    # Set display values (only structures, no acres)
    data$y_total <- data$total_structures
    data$y_active <- data$active_structures
    data$y_expiring <- data$expiring_structures
    y_label <- "Number of Structures"
    title_metric <- "Number of Structures"
    
    # Determine the plotting group column and handle display names
    group_col <- input$group_by
    plot_group_col <- if (length(input$zone_filter) > 1 && "combined_group" %in% names(data)) {
      "combined_group"
    } else {
      group_col
    }
    
    # Create display names based on grouping
    if (group_col == "facility") {
      # Map facility codes to full names for display
      facilities <- get_facility_lookup()
      facility_map <- setNames(facilities$full_name, facilities$short_name)
      data$display_name <- facility_map[data[[group_col]]]
      if (plot_group_col == "combined_group") {
        # Update combined group with full names
        data$display_name <- paste0(facility_map[data[[group_col]]], " (P", gsub(".*\\(P([12])\\).*", "\\1", data$combined_group), ")")
      }
    } else if (group_col == "foreman") {
      # Map foreman numbers to names
      foremen_lookup <- get_foremen_lookup()
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      data$display_name <- foreman_map[as.character(data[[group_col]])]
      if (plot_group_col == "combined_group") {
        # Update combined group with foreman names
        data$display_name <- paste0(foreman_map[as.character(data[[group_col]])], " (P", gsub(".*\\(P([12])\\).*", "\\1", data$combined_group), ")")
      }
    } else {
      # For "All MMCD" or other cases
      data$display_name <- data[[plot_group_col]]
    }
    
    # Get colors based on grouping - use new consolidated system with zone support
    use_zones <- length(input$zone_filter) > 1
    combined_groups <- plot_group_col == "combined_group"
    
    if(group_col == "facility") {
      custom_colors <- get_facility_base_colors(alpha_zones = use_zones, combined_groups = combined_groups)
    } else if(group_col == "foreman") {
      custom_colors <- get_foreman_colors(alpha_zones = use_zones, combined_groups = combined_groups)
    } else {
      custom_colors <- NULL
    }
    
    # If using zones, apply alpha manually to P2 colors
    if (use_zones && !is.null(custom_colors)) {
      # Apply 60% alpha to P2 zone colors
      for (name in names(custom_colors)) {
        if (grepl("\\(P2\\)", name)) {
          # Convert to rgba with alpha
          rgb_vals <- col2rgb(custom_colors[name])
          custom_colors[name] <- sprintf("rgba(%d,%d,%d,0.6)", rgb_vals[1], rgb_vals[2], rgb_vals[3])
        }
      }
    }
    
    # Create a new column to determine which labels to show (avoiding overplot)
    data$show_active_label <- data$y_active != data$y_expiring
    
    # Calculate y-axis maximum for proper positioning
    y_max <- max(data$y_total) * 1.1
    
    # Set up the title with appropriate filters
    status_types_text <- paste(input$status_types, collapse = ", ")
    
    # Handle facility text for multi-select
    facility_text <- if (is.null(input$facility_filter) || "All" %in% input$facility_filter) {
      "All Facilities"
    } else {
      # Get facility names for display
      facilities <- get_facility_lookup()
      facility_names <- setNames(facilities$full_name, facilities$short_name)
      display_names <- sapply(input$facility_filter, function(f) facility_names[f] %||% f)
      paste("Facility:", paste(display_names, collapse=", "))
    }
    zone_text <- if (length(input$zone_filter) == 0) {
      "No Zones"
    } else if (length(input$zone_filter) == 1) {
      paste("Zone: P", input$zone_filter)
    } else {
      paste("Zone:", paste0("P", input$zone_filter))
    }
    
    # Get status colors from db_helpers before creating the plot
    status_colors <- get_status_colors()
    
    # Create the plot - use colors when available
    if (!is.null(custom_colors) && group_col != "mmcd_all") {
      p <- ggplot(data, aes(x = display_name, fill = !!sym(plot_group_col))) +
        # First draw total bars
        geom_bar(aes(y = y_total), stat = "identity", alpha = 0.3) +
        # Then overlay active bars  
        geom_bar(aes(y = y_active), stat = "identity") +
        # Finally overlay expiring bars
        geom_bar(aes(y = y_expiring), stat = "identity", fill = status_colors["planned"]) +
        scale_fill_manual(values = custom_colors, guide = "none")
    } else {
      # Default colors when no custom scheme available
      p <- ggplot(data, aes(x = display_name)) +
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
      geom_text(aes(x = display_name, y = y_total, label = y_total), vjust = -0.5, color = "black") +
      # Add expiring labels
      geom_text(aes(x = display_name, y = y_expiring, label = y_expiring), vjust = 1.5, color = "black", fontface = "bold") +
      
      # Add labels and title
      labs(
        title = paste("Structures by", 
                      case_when(
                        group_col == "facility" ~ "Facility",
                        group_col == "foreman" ~ "FOS",
                        group_col == "mmcd_all" ~ "MMCD (All)",
                        TRUE ~ group_col
                      ), "-", title_metric),
        subtitle = paste("Status types:", status_types_text, "-", zone_text, "-", facility_text,
                         "- Expiring within", input$expiring_days, "days"),
        x = case_when(
          group_col == "facility" ~ "Facility",
          group_col == "foreman" ~ "FOS",
          group_col == "mmcd_all" ~ "MMCD (All)",
          TRUE ~ group_col
        ),
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
                         vjust = -0.5, color = status_colors["active"], fontface = "bold")
    }
    
    # Add legend manually
    p + annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.9, ymax = y_max * 0.95,
                 fill = "gray80", alpha = 0.7) +
      annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.8, ymax = y_max * 0.85,
               fill = status_colors["active"]) +
      annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.7, ymax = y_max * 0.75,
               fill = status_colors["planned"]) +
      annotate("text", x = 0.1, y = y_max * 0.925,
               label = "Total", hjust = 0) +
      annotate("text", x = 0.1, y = y_max * 0.825,
               label = "Active", hjust = 0) +
      annotate("text", x = 0.1, y = y_max * 0.725,
               label = "Expiring", hjust = 0)
  })
  
    # Generate the historical trends plot
  output$historicalGraph <- renderPlot({
    data <- processed_historical_data()
    
    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No data available with the selected filters", size = 6) +
          theme_void()
      )
    }
    
    # Calculate seasonal averages
    treatment_trends <- data %>%
      mutate(
        month = month(date)
      ) %>%
      group_by(month) %>%
      mutate(seasonal_avg = mean(proportion_active_treatment, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(date)
    
    # Build filter text for subtitle
    filter_parts <- c()
    
    if (!is.null(input$facility_filter) && length(input$facility_filter) > 0 && !"All" %in% input$facility_filter) {
      if (length(input$facility_filter) == 1) {
        filter_parts <- c(filter_parts, paste("Facility:", input$facility_filter[1]))
      } else {
        filter_parts <- c(filter_parts, paste("Facilities:", paste(input$facility_filter, collapse = ", ")))
      }
    }
    
    if (input$structure_type_filter != "all") {
      filter_parts <- c(filter_parts, paste("Structure Type:", input$structure_type_filter))
    }
    
    if (input$priority_filter != "all") {
      filter_parts <- c(filter_parts, paste("Priority:", input$priority_filter))
    }
    
    status_types_text <- paste(input$status_types, collapse = ", ")
    filter_parts <- c(filter_parts, paste("Status:", status_types_text))
    
    if (length(input$zone_filter) == 1) {
      filter_parts <- c(filter_parts, paste("Zone: P", input$zone_filter))
    } else if (length(input$zone_filter) == 2) {
      filter_parts <- c(filter_parts, paste("Zones: P", paste(input$zone_filter, collapse = ", P")))
    }
    
    filter_text <- if (length(filter_parts) > 0) {
      paste0(" (", paste(filter_parts, collapse = " | "), ")")
    } else {
      ""
    }
    
    # Create the plot
    ggplot(treatment_trends, aes(x = date)) +
      geom_line(aes(y = proportion_active_treatment), color = "blue", size = 1) +
      geom_line(aes(y = seasonal_avg), color = "red", linetype = "dashed", size = 1.2) +
      labs(
        title = sprintf(
          "Proportion of Structures with Active Treatment (%d to %d)%s",
          as.numeric(input$start_year),
          as.numeric(input$end_year),
          filter_text
        ),
        subtitle = "Blue: actual | Red dashed: seasonal average",
        x = "Date",
        y = "Proportion of Active Treatment"
      ) +
      scale_y_continuous(
        labels = scales::percent_format(),
        breaks = seq(0, 1, by = 0.1)
      ) +
      scale_x_date(
        date_breaks = "6 months",
        date_labels = "%b %Y",
        expand = expansion(mult = c(0.02, 0.02))
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
