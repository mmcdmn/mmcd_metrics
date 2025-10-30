# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
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
  titlePanel("Structures with Active and Expiring Treatments"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      # Slider to control the expiration window
      sliderInput("expiring_days", "Days Until Expiration:",
                  min = 1, max = 30, value = 7, step = 1),
      
      # Date input to simulate "today"
      dateInput("custom_today", "Pretend Today is:",
                value = Sys.Date(), 
                format = "yyyy-mm-dd"),
      
      # Toggle for structure status types
      checkboxGroupInput("status_types", "Include Structure Status:",
                         choices = c("Dry (D)" = "D",
                                     "Wet (W)" = "W", 
                                     "Unknown (U)" = "U"),
                         selected = c("D", "W", "U")),
      
      # Dropdown for facility filter - using db_helpers to get full names with multiple selection
      selectizeInput("facility_filter", "Facility:",
                    choices = get_facility_choices(),
                    selected = "all", multiple = TRUE),
      
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
      
      helpText("This visualization shows structures by facility with three categories:",
               tags$br(),
               tags$ul(
                 tags$li(tags$span(style = "color:gray", "Gray: Total structures")),
                 tags$li(tags$span(style = paste0("color:", get_status_colors()["active"]), "Green: Structures with active treatments")),
                 tags$li(tags$span(style = paste0("color:", get_status_colors()["planned"]), "Orange: Structures with treatments expiring within the selected days"))
               )),
      
      helpText(tags$b("Date Simulation:"),
               tags$br(),
               "Use 'Pretend Today is' to see what treatments would be active/expiring on any specific date. Only treatments that would still be active on that date are considered."),
      
      helpText(tags$b("Structure Status:"),
               tags$br(),
               tags$ul(
                 tags$li(tags$b("D:"), "Dry - Structure is dry"),
                 tags$li(tags$b("W:"), "Wet - Structure has water"),
                 tags$li(tags$b("U:"), "Unknown - Status not determined")
               ))
    ),
    
    # Main panel for displaying the graphs with tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Current Progress", 
                 plotOutput("structureGraph", height = "600px")
        ),
        tabPanel("Historical Trends",
                 fluidRow(
                   column(3,
                          selectInput("start_year", "Start Year:",
                                      choices = seq(2010, 2025),
                                      selected = 2018)
                   ),
                   column(3,
                          selectInput("end_year", "End Year:",
                                      choices = seq(2010, 2025),
                                      selected = 2025)
                   )
                 ),
                 plotOutput("historicalGraph", height = "600px")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Helper functions 
  get_facility_condition <- function(facility_filter) {
    if (is.null(facility_filter) || ("all" %in% facility_filter)) {
      return("") # No filtering
    } else {
      facility_list <- paste0("'", paste(facility_filter, collapse = "','"), "'")
      return(sprintf("AND trt.facility IN (%s)", facility_list))
    }
  }
  
  # Function to construct structure type filter condition for SQL
  get_structure_type_condition <- function(structure_type) {
    if (structure_type == "all") {
      return("")
    } else {
      # Handle compound types (e.g., CV/PR matches both CV and PR)
      # Use case-insensitive matching with UPPER()
      return(sprintf("AND (UPPER(loc.s_type) = '%s' OR UPPER(loc.s_type) LIKE '%%/%s' OR UPPER(loc.s_type) LIKE '%s/%%' OR UPPER(loc.s_type) LIKE '%%/%s/%%')", 
                     toupper(structure_type), toupper(structure_type), toupper(structure_type), toupper(structure_type)))
    }
  }
  
  # Function to construct priority filter condition for SQL
  get_priority_condition <- function(priority) {
    if (priority == "all") {
      return("")
    } else {
      return(sprintf("AND loc.priority = '%s'", priority))
    }
  }
  
  # Function to construct status_udw filter condition for SQL
  get_status_condition <- function(include_status_values) {
    if (is.null(include_status_values) || length(include_status_values) == 0) {
      # If no status values selected, exclude all structures with status_udw
      return("AND loc.status_udw IS NULL")
    } else if (length(include_status_values) == 3) {
      # If all status values selected, include all structures
      return("")
    } else {
      # Include only selected status values (case-insensitive)
      status_list <- paste0("'", include_status_values, "'", collapse = ", ")
      return(sprintf("AND (loc.status_udw IS NULL OR UPPER(loc.status_udw) IN (%s))", status_list))
    }
  }
  
  get_facility_condition_total <- function(facility_filter, structure_type, priority, include_status_values) {
    conditions <- c()
    if (!is.null(facility_filter) && !("all" %in% facility_filter)) {
      facility_list <- paste0("'", paste(facility_filter, collapse = "','"), "'")
      conditions <- c(conditions, sprintf("facility IN (%s)", facility_list))
    }
    if (structure_type != "all") {
      # Handle compound types and case-insensitivity for total structure count
      conditions <- c(conditions, sprintf("(UPPER(s_type) = '%s' OR UPPER(s_type) LIKE '%%/%s' OR UPPER(s_type) LIKE '%s/%%' OR UPPER(s_type) LIKE '%%/%s/%%')", 
                                        toupper(structure_type), toupper(structure_type), toupper(structure_type), toupper(structure_type)))
    }
    if (priority != "all") {
      conditions <- c(conditions, sprintf("priority = '%s'", priority))
    }
    
    # Handle status filter
    if (is.null(include_status_values) || length(include_status_values) == 0) {
      conditions <- c(conditions, "status_udw IS NULL")
    } else if (length(include_status_values) < 3) {
      # If not all status values selected, filter accordingly
      status_list <- paste0("'", include_status_values, "'", collapse = ", ")
      conditions <- c(conditions, sprintf("(status_udw IS NULL OR UPPER(status_udw) IN (%s))", status_list))
    }
    # If all 3 status values selected, no additional condition needed
    
    if (length(conditions) > 0) {
      return(paste("AND", paste(conditions, collapse = " AND ")))
    } else {
      return("")
    }
  }
  
  # Treatment data reactive function - copied from struct_trt_history
  treatment_data <- reactive({
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = db_name,
      host = db_host,
      port = as.numeric(db_port),
      user = db_user,
      password = db_password
    )
    
    # Fetch archive data with structure info
    query_archive <- sprintf(
      "
SELECT
trt.sitecode,
trt.inspdate,
COALESCE(mat.effect_days, 0) AS effect_days,
loc.s_type,
loc.priority,
loc.facility
FROM public.dblarv_insptrt_archive trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
WHERE trt.inspdate >= date '%d-01-01'
AND trt.inspdate < date '%d-01-01'
AND trt.list_type = 'STR'
%s
%s
%s
%s
",
      as.numeric(input$start_year),
      as.numeric(input$end_year) + 1,
      get_facility_condition(input$facility_filter),
      get_structure_type_condition(input$structure_type_filter),
      get_priority_condition(input$priority_filter),
      get_status_condition(input$status_types)
    )
    archive_data <- dbGetQuery(con, query_archive)
    
    # Fetch current data with structure info
    query_current <- sprintf(
      "
SELECT
trt.sitecode,
trt.inspdate,
COALESCE(mat.effect_days, 0) AS effect_days,
loc.s_type,
loc.priority,
loc.facility
FROM public.dblarv_insptrt_current trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
WHERE trt.inspdate >= date '%d-01-01'
AND trt.inspdate < date '%d-01-01'
AND trt.list_type = 'STR'
%s
%s
%s
%s
",
      as.numeric(input$start_year),
      as.numeric(input$end_year) + 1,
      get_facility_condition(input$facility_filter),
      get_structure_type_condition(input$structure_type_filter),
      get_priority_condition(input$priority_filter),
      get_status_condition(input$status_types)
    )
    current_data <- dbGetQuery(con, query_current)
    
    #Fetch Total Structures (active structures only)
    query_total_structures <- sprintf(
      "
SELECT COUNT(DISTINCT sitecode) AS total_structures
FROM loc_cxstruct
WHERE 1=1
AND (enddate IS NULL OR enddate > CURRENT_DATE)
%s
",
      get_facility_condition_total(input$facility_filter, input$structure_type_filter, input$priority_filter, input$status_types)
    )
    
    total_structures <- dbGetQuery(con, query_total_structures)$total_structures
    dbDisconnect(con)
    
    # Combine and process data
    combined_data <- bind_rows(archive_data, current_data) %>%
      mutate(inspdate = as.Date(inspdate),
             enddate = inspdate + effect_days)
    
    list(data = combined_data, total_structures = total_structures)
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
    
    # Build facility filter condition - handle multiple selection with table alias
    facility_condition <- if (is.null(input$facility_filter) || ("all" %in% input$facility_filter)) {
      ""
    } else {
      facility_list <- paste0("'", paste(input$facility_filter, collapse = "','"), "'")
      sprintf("AND s.facility IN (%s)", facility_list)
    }
    
    # Build structure type filter condition  
    struct_type_condition <- if (input$structure_type_filter == "all") "" else sprintf("AND s.s_type = '%s'", input$structure_type_filter)
    
    # Build priority filter condition
    priority_condition <- if (input$priority_filter == "all") "" else sprintf("AND s.priority = '%s'", input$priority_filter)
    
    # Query to get structures from loc_cxstruct with zone and foreman information
    # Only include foremen who are currently active in employee_list
    structures_query <- sprintf("
SELECT s.sitecode, s.facility, s.status_udw, s.s_type, s.priority, 
       CASE 
         WHEN e.emp_num IS NOT NULL AND e.active = true THEN s.foreman
         ELSE NULL
       END as foreman, 
       g.zone
FROM public.loc_cxstruct s
LEFT JOIN public.employee_list e ON s.foreman = e.emp_num 
  AND e.emp_type = 'FieldSuper' 
  AND e.active = true
LEFT JOIN public.gis_sectcode g ON LEFT(s.sitecode, 6) || '-' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'N' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'S' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'E' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'W' = g.sectcode
WHERE (s.status_udw IN (%s) OR s.status_udw IS NULL)
AND (s.enddate IS NULL OR s.enddate > CURRENT_DATE)
%s
%s
%s
", status_types, facility_condition, struct_type_condition, priority_condition)
    
    structures <- dbGetQuery(con, structures_query)
    
    # Query to get treatment information from current treatments
    treatments_query <- "
SELECT t.sitecode, t.facility, t.inspdate, t.matcode, t.foreman, m.effect_days
FROM public.dblarv_insptrt_current t
LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
WHERE t.list_type = 'STR'
"
    treatments <- dbGetQuery(con, treatments_query)
    
    dbDisconnect(con)
    
    # Process the data
    # Identify structures with treatments
    structure_treatments <- treatments %>%
      inner_join(structures, by = c("sitecode", "facility"), suffix = c("_trt", "_struct")) %>%
      mutate(
        # ONLY use structure foreman (jurisdictional assignment) - ignore treatment foreman completely
        foreman = foreman_struct
      )    # Calculate treatment status (active) - will be recalculated in processed_data with custom date
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
    } else if (group_col == "foreman") {
      # Only include structures that have a foreman assigned (jurisdictional assignment)
      structures <- structures %>% filter(!is.na(foreman) & foreman != "")
      structure_treatments <- structure_treatments %>% filter(!is.na(foreman) & foreman != "")
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
      # Map foreman numbers to names and set proper ordering by facility
      foremen_lookup <- get_foremen_lookup()
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      
      # Handle NA values properly
      data$display_name <- ifelse(
        is.na(data[[group_col]]) | data[[group_col]] == "",
        "Unassigned FOS",
        foreman_map[as.character(data[[group_col]])]
      )
      # Handle any remaining NAs from missing foreman numbers in lookup
      data$display_name <- ifelse(
        is.na(data$display_name),
        paste0("FOS #", data[[group_col]]),
        data$display_name
      )
      
      # Set factor levels to order by facility grouping (foremen_lookup is already ordered by facility, shortname)
      ordered_foreman_names <- c("Unassigned FOS", foremen_lookup$shortname)
      # Add any FOS # names that might exist
      fos_pattern_names <- unique(data$display_name[grepl("^FOS #", data$display_name)])
      all_foreman_levels <- c(ordered_foreman_names, fos_pattern_names)
      data$display_name <- factor(data$display_name, levels = all_foreman_levels)
      
      if (plot_group_col == "combined_group") {
        # Update combined group with foreman names
        base_foreman_name <- ifelse(
          is.na(data[[group_col]]) | data[[group_col]] == "",
          "Unassigned FOS",
          foreman_map[as.character(data[[group_col]])]
        )
        base_foreman_name <- ifelse(
          is.na(base_foreman_name),
          paste0("FOS #", data[[group_col]]),
          base_foreman_name
        )
        data$display_name <- paste0(base_foreman_name, " (P", gsub(".*\\(P([12])\\).*", "\\1", data$combined_group), ")")
        
        # Set factor levels for combined groups with proper facility ordering
        combined_levels <- c()
        for (foreman_name in ordered_foreman_names) {
          combined_levels <- c(combined_levels, paste0(foreman_name, " (P1)"), paste0(foreman_name, " (P2)"))
        }
        # Add any FOS # combined names
        fos_combined_names <- unique(data$display_name[grepl("^FOS #.*\\(P[12]\\)", data$display_name)])
        all_combined_levels <- c(combined_levels, fos_combined_names)
        data$display_name <- factor(data$display_name, levels = all_combined_levels)
      }
    } else {
      # For "All MMCD" or other cases
      data$display_name <- data[[plot_group_col]]
    }
    
    # Get colors based on grouping
    custom_colors <- if(group_col == "facility") {
      get_facility_base_colors()
    } else if(group_col == "foreman") {
      # Follow suco_history pattern exactly - map foreman NUMBERS to facility-based colors
      foreman_colors <- get_foreman_colors()  # These are keyed by shortname
      foremen_lookup <- get_foremen_lookup()
      
      # Create mapping from foreman NUMBER to facility-based colors
      foremen_in_data <- unique(na.omit(data[[group_col]]))
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
      
      # Return emp_colors keyed by foreman numbers (what plot_group_col contains)
      emp_colors
    } else {
      NULL
    }
    
    # Handle color mapping for combined groups
    if (plot_group_col == "combined_group" && !is.null(custom_colors)) {
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
    
    # Create a new column to determine which labels to show (avoiding overplot)
    data$show_active_label <- data$y_active != data$y_expiring
    
    # Calculate y-axis maximum for proper positioning
    y_max <- max(data$y_total) * 1.1
    
    # Set up the title with appropriate filters
    status_types_text <- paste(input$status_types, collapse = ", ")
    facility_text <- if (is.null(input$facility_filter) || ("all" %in% input$facility_filter)) {
      "All Facilities"
    } else {
      paste("Facilities:", paste(input$facility_filter, collapse = ", "))
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
        geom_bar(aes(y = y_active), stat = "identity", alpha = 0.8) +
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

  # Historical plot - Modified to support grouping by facility and FOS
  output$historicalGraph <- renderPlot({
    td <- treatment_data()
    data <- td$data
    total_structures <- td$total_structures
    
    # Handle cases where no data is available
    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "No data available for the selected range.",
            size = 6
          ) +
          theme_void()
      )
    }
    
    # Generate the full date range for time series
    start_date <- as.Date(sprintf("%d-03-01", as.numeric(input$start_year)))
    end_date <- as.Date(sprintf("%d-12-31", as.numeric(input$end_year)))
    date_range <- seq.Date(start_date, end_date, by = "day")
    
    # Determine grouping column
    group_col <- input$group_by
    
    if (group_col == "mmcd_all") {
      # Original aggregated approach for "All MMCD"
      # Get structure start/end dates to calculate dynamic totals
      con <- get_db_connection()
      if (is.null(con)) {
        return(
          ggplot() +
            annotate(
              "text",
              x = 0.5,
              y = 0.5,
              label = "Database connection failed.",
              size = 6
            ) +
            theme_void()
        )
      }
      
      query_structure_dates <- sprintf(
        "
SELECT sitecode, startdate, enddate
FROM loc_cxstruct
WHERE 1=1
%s
",
        get_facility_condition_total(input$facility_filter, input$structure_type_filter, input$priority_filter, input$status_types)
      )
      structure_dates <- dbGetQuery(con, query_structure_dates)
      dbDisconnect(con)
      
      # Create a dataframe of daily changes for treatments (by sitecode to avoid double-counting)
      # First, get unique treatment periods by sitecode
      treatment_periods <- data %>%
        select(sitecode, inspdate, effect_days) %>%
        mutate(enddate = inspdate + effect_days) %>%
        arrange(sitecode, inspdate)
      
      # For each sitecode, merge overlapping treatment periods
      merged_treatments <- treatment_periods %>%
        group_by(sitecode) %>%
        arrange(inspdate) %>%
        mutate(
          # Create groups for overlapping treatments
          lag_enddate = lag(inspdate + effect_days, default = as.Date("1900-01-01")),
          new_period = inspdate > lag_enddate,
          period_id = cumsum(new_period)
        ) %>%
        group_by(sitecode, period_id) %>%
        summarize(
          start_date = min(inspdate),
          end_date = max(inspdate + effect_days),
          .groups = "drop"
        )
      
      daily_treatment_changes <- data.frame(
        date = c(merged_treatments$start_date, merged_treatments$end_date),
        change = c(rep(1, nrow(merged_treatments)), rep(-1, nrow(merged_treatments))) # +1 for start, -1 for end
      ) %>%
        group_by(date) %>%
        summarize(treatment_change = sum(change)) %>%
        arrange(date)
      
      # Create a dataframe of daily changes for structure counts
      structure_changes <- data.frame()
      if (nrow(structure_dates) > 0) {
        start_changes <- structure_dates %>%
          filter(!is.na(startdate)) %>%
          mutate(date = as.Date(startdate), structure_change = 1) %>%
          select(date, structure_change)
        
        end_changes <- structure_dates %>%
          filter(!is.na(enddate)) %>%
          mutate(date = as.Date(enddate), structure_change = -1) %>%
          select(date, structure_change)
        
        structure_changes <- bind_rows(start_changes, end_changes) %>%
          group_by(date) %>%
          summarize(structure_change = sum(structure_change)) %>%
          arrange(date)
      }
      
      # Calculate cumulative active treatments and total structures over time
      all_dates <- data.frame(date = date_range)
      treatment_trends <- all_dates %>%
        left_join(daily_treatment_changes, by = "date") %>%
        left_join(structure_changes, by = "date") %>%
        mutate(
          treatment_change = ifelse(is.na(treatment_change), 0, treatment_change),
          structure_change = ifelse(is.na(structure_change), 0, structure_change)
        ) %>%
        arrange(date) %>%
        mutate(
          active_treatments = cumsum(treatment_change),
          total_structures_dynamic = total_structures + cumsum(structure_change)
        ) %>%
        # Avoid division by zero
        mutate(
          proportion_active_treatment = ifelse(
            total_structures_dynamic > 0, 
            active_treatments / total_structures_dynamic, 
            0
          ),
          group_name = "All MMCD"
        )

      # Calculate seasonal average curve (average proportion for each calendar day across all years)
      seasonal_curve <- treatment_trends %>%
        mutate(day_of_year = format(date, "%m-%d")) %>%
        group_by(day_of_year) %>%
        summarize(
          seasonal_avg = mean(proportion_active_treatment, na.rm = TRUE),
          .groups = "drop"
        )

      # Join seasonal average back to treatment_trends
      treatment_trends <- treatment_trends %>%
        mutate(day_of_year = format(date, "%m-%d")) %>%
        left_join(seasonal_curve, by = "day_of_year")
        
    } else {
      # Grouping by facility or foreman - not yet implemented
      return(
        ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "This feature is not yet working.\nTry MMCD (All) in the group by to see a preview\nof the data without the grouping by facility or FOS.",
            size = 5,
            hjust = 0.5,
            vjust = 0.5
          ) +
          theme_void() +
          labs(title = "Historical Trends - Feature Under Development")
      )
    }

    # Create filter description for title
    filters <- c()
    if (!is.null(input$facility_filter) && !("all" %in% input$facility_filter)) {
      if (length(input$facility_filter) == 1) {
        filters <- c(filters, paste("Facility:", input$facility_filter))
      } else {
        filters <- c(filters, paste("Facilities:", paste(input$facility_filter, collapse = ", ")))
      }
    }
    if (input$structure_type_filter != "all") filters <- c(filters, paste("Type:", input$structure_type_filter))
    if (input$priority_filter != "all") filters <- c(filters, paste("Priority:", input$priority_filter))
    
    # Map facility codes to full names if specific facilities are selected
    if (!is.null(input$facility_filter) && !("all" %in% input$facility_filter)) {
      facilities <- get_facility_lookup()
      facility_full_names <- sapply(input$facility_filter, function(f) {
        full_name <- facilities$full_name[facilities$short_name == f]
        if (length(full_name) > 0) full_name else f
      })
      # Replace facility filter with full names
      if (length(input$facility_filter) == 1) {
        filters[1] <- paste("Facility:", facility_full_names)
      } else {
        filters[1] <- paste("Facilities:", paste(facility_full_names, collapse = ", "))
      }
    }
    
    filter_text <- if (length(filters) > 0) paste(" -", paste(filters, collapse = ", ")) else ""
    
    # Get colors for groups
    if (group_col == "facility") {
      group_colors <- get_facility_base_colors()
      # Map display names if needed
      if (group_col == "facility") {
        facilities <- get_facility_lookup()
        facility_map <- setNames(facilities$full_name, facilities$short_name)
        treatment_trends$display_name <- ifelse(
          treatment_trends$group_name %in% names(facility_map),
          facility_map[treatment_trends$group_name],
          treatment_trends$group_name
        )
      } else {
        treatment_trends$display_name <- treatment_trends$group_name
      }
    } else if (group_col == "foreman") {
      # Get foreman colors following suco_history pattern
      foreman_colors <- get_foreman_colors()
      foremen_lookup <- get_foremen_lookup()
      
      # Map foreman numbers to names and colors
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      group_colors <- character(0)
      
      unique_groups <- unique(treatment_trends$group_name)
      for (foreman_num in unique_groups) {
        foreman_num_str <- trimws(as.character(foreman_num))
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
        
        if(length(matches) > 0) {
          shortname <- foremen_lookup$shortname[matches[1]]
          if(shortname %in% names(foreman_colors)) {
            group_colors[foreman_num_str] <- foreman_colors[shortname]
          }
        }
      }
      
      # Set display names
      treatment_trends$display_name <- ifelse(
        treatment_trends$group_name %in% names(foreman_map),
        foreman_map[treatment_trends$group_name],
        paste0("FOS #", treatment_trends$group_name)
      )
    } else {
      group_colors <- NULL
      treatment_trends$display_name <- treatment_trends$group_name
    }
    
    # Create the plot
    group_title <- case_when(
      group_col == "facility" ~ "by Facility",
      group_col == "foreman" ~ "by FOS", 
      group_col == "mmcd_all" ~ "(All MMCD)",
      TRUE ~ paste("by", group_col)
    )
    
    p <- ggplot(treatment_trends, aes(x = date, y = proportion_active_treatment)) +
      geom_line(aes(color = display_name), size = 1.2) +
      # Add seasonal average line for MMCD (All) grouping
      {if (group_col == "mmcd_all") geom_line(aes(y = seasonal_avg), color = "red", linetype = "dashed", size = 1.2)} +
      labs(
        title = sprintf(
          "Proportion of Structures with Active Treatment %s (%d to %d)%s",
          group_title,
          as.numeric(input$start_year),
          as.numeric(input$end_year),
          filter_text
        ),
        subtitle = if (group_col == "mmcd_all") "Blue: actual | Red dashed: seasonal average" else NULL,
        x = "Date",
        y = "Proportion of Active Treatment",
        color = case_when(
          group_col == "facility" ~ "Facility",
          group_col == "foreman" ~ "FOS",
          TRUE ~ "Group"
        )
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
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      )
    
    # Apply custom colors if available
    if (!is.null(group_colors) && length(group_colors) > 0) {
      # Create color mapping for display names
      display_colors <- character(0)
      for (i in seq_len(nrow(treatment_trends))) {
        group_val <- treatment_trends$group_name[i]
        display_val <- treatment_trends$display_name[i]
        if (group_val %in% names(group_colors)) {
          display_colors[display_val] <- group_colors[group_val]
        }
      }
      display_colors <- display_colors[!duplicated(names(display_colors))]
      
      if (length(display_colors) > 0) {
        p <- p + scale_color_manual(values = display_colors)
      }
    }
    
    return(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
