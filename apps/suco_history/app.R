# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(scales)
  library(leaflet) # For interactive maps
  library(sf) # For handling spatial data
  library(stringr) # For string manipulation
  library(plotly)
  library(later) # For delayed execution to prevent infinite loops
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

# Define UI for the application
ui <- fluidPage(
  # Application title
  titlePanel("SUCO Analysis Dashboard"),

  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      # Group by selection
      h4("Analysis Options"),
      radioButtons("group_by", "Group By:",
                   choices = c("MMCD (All)" = "mmcd_all",
                               "Facility" = "facility", 
                               "FOS" = "foreman"),
                   selected = "mmcd_all"),
      
      hr(),
      
      # Date shortcuts
      h4("Date Selection"),
      fluidRow(
        column(4, actionButton("this_year", "This Year", class = "btn-primary btn-sm", style = "width: 100%;")),
        column(4, actionButton("this_month", "This Month", class = "btn-info btn-sm", style = "width: 100%;")),
        column(4, actionButton("this_week", "This Week", class = "btn-success btn-sm", style = "width: 100%;"))
      ),
      br(),
      
      # Year slider (only visible for All Data tab with multi-year capability)
      conditionalPanel(
        condition = "input.main_tabset == 'All Data (Current + Archive)'",
        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}",
                   ".irs-grid-text {font-size: 8pt;}"),
        sliderInput("year_range", "Year Range:",
                    min = 2015, max = as.numeric(format(Sys.Date(), "%Y")),
                    value = c(as.numeric(format(Sys.Date(), "%Y")), as.numeric(format(Sys.Date(), "%Y"))),
                    sep = "", step = 1),
        br()
      ),
      
      # Date range selection - behavior depends on which tab is active
      dateRangeInput("date_range", "Custom Date Range:",
                     start = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                     end = Sys.Date(),
                     format = "yyyy-mm-dd"),
      
      hr(),
      
      # Filter by zone (P1/P2)
      h4("Location Filters"),
      checkboxGroupInput("zone_filter", "Filter by Zone:",
                        choices = c("P1" = "1", "P2" = "2"),
                        selected = c("1", "2")),
      
      # Filter by facility (multi-select)
      selectizeInput("facility_filter", "Filter by Facility:",
                  choices = c("All"),  # Will be populated from get_facility_lookup()
                  selected = "All", multiple = TRUE),
      
      # Filter by foreman (multi-select, populated dynamically)
      selectizeInput("foreman_filter", "Filter by FOS:",
                  choices = c("All"),
                  selected = "All", multiple = TRUE),
      
      # Add species filter to sidebarPanel
      selectInput("species_filter", "Filter by Species:", choices = c("All"), selected = "All"),
      
      hr(),
      
      # Display and visualization options
      h4("Display Options"),
      # Graph type selector
      selectInput("graph_type", "Graph Type:",
                  choices = c("Bar" = "bar", "Line" = "line", "Point" = "point", "Area" = "area"),
                  selected = "bar"),
      # Map Controls
      conditionalPanel(
        condition = "input.tabset == 'Map'",
        hr(),
        h4("Map Options"),
        
        # Base map selection
        selectInput("basemap", "Base Map:",
                    choices = c("OpenStreetMap" = "osm",
                                "Carto Light" = "carto",
                                "Esri Satellite" = "satellite"),
                    selected = "carto"),
        
        # Marker size adjustment
        sliderInput("marker_size", "Marker Size Multiplier:",
                    min = 1, max = 5, value = 3, step = 0.5)
      )
    ),
    
    # Main panel for displaying the graphs
    mainPanel(
      tabsetPanel(id = "main_tabset",
        tabPanel("Current Data", 
          tabsetPanel(id = "current_tabset",
            tabPanel("Graph", value = "CurrentGraph", plotOutput("current_trend_plot", height = "500px")),
            tabPanel("Map", value = "CurrentMap", leafletOutput("current_map", height = "600px")),
            tabPanel("Summary Table", value = "CurrentTable", dataTableOutput("current_summary_table")),
            tabPanel("Top Locations", value = "CurrentTopLoc", plotlyOutput("current_location_plotly", height = "500px"))
          )
        ),
        tabPanel("All Data (Current + Archive)",
          tabsetPanel(id = "all_tabset",
            tabPanel("Graph", value = "AllGraph", plotOutput("trend_plot", height = "500px")),
            tabPanel("Map", value = "AllMap", leafletOutput("map", height = "600px")),
            tabPanel("Summary Table", value = "AllTable", dataTableOutput("summary_table")),
            tabPanel("Top Locations", value = "AllTopLoc", plotlyOutput("location_plotly", height = "500px"))
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to prevent infinite loops between date controls
  updating_date_range <- reactiveVal(FALSE)
  updating_year_range <- reactiveVal(FALSE)
  
  # Date shortcut handlers - behavior depends on active tab
  observeEvent(input$this_year, {
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    start_date <- as.Date(paste0(current_year, "-01-01"))
    end_date <- Sys.Date()
    
    # Set flag to prevent infinite loop
    updating_date_range(TRUE)
    updating_year_range(TRUE)
    
    # Always update date range to current year
    updateDateRangeInput(session, "date_range", start = start_date, end = end_date)
    
    # Only update year slider if on All Data tab
    if (!is.null(input$main_tabset) && input$main_tabset == "All Data (Current + Archive)") {
      updateSliderInput(session, "year_range", value = c(current_year, current_year))
    }
    
    # Reset flags after a short delay
    later::later(function() {
      updating_date_range(FALSE)
      updating_year_range(FALSE)
    }, 0.1)
  })
  
  observeEvent(input$this_month, {
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    current_month <- as.numeric(format(Sys.Date(), "%m"))
    start_date <- as.Date(paste0(current_year, "-", sprintf("%02d", current_month), "-01"))
    end_date <- Sys.Date()
    
    # Set flag to prevent infinite loop
    updating_date_range(TRUE)
    updating_year_range(TRUE)
    
    # Always update date range to current month
    updateDateRangeInput(session, "date_range", start = start_date, end = end_date)
    
    # Only update year slider if on All Data tab
    if (!is.null(input$main_tabset) && input$main_tabset == "All Data (Current + Archive)") {
      updateSliderInput(session, "year_range", value = c(current_year, current_year))
    }
    
    # Reset flags after a short delay
    later::later(function() {
      updating_date_range(FALSE)
      updating_year_range(FALSE)
    }, 0.1)
  })
  
  observeEvent(input$this_week, {
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    # Get start of current week (Monday)
    start_date <- floor_date(Sys.Date(), "week", week_start = 1)
    end_date <- Sys.Date()
    
    # Set flag to prevent infinite loop
    updating_date_range(TRUE)
    updating_year_range(TRUE)
    
    # Always update date range to current week
    updateDateRangeInput(session, "date_range", start = start_date, end = end_date)
    
    # Only update year slider if on All Data tab
    if (!is.null(input$main_tabset) && input$main_tabset == "All Data (Current + Archive)") {
      updateSliderInput(session, "year_range", value = c(current_year, current_year))
    }
    
    # Reset flags after a short delay
    later::later(function() {
      updating_date_range(FALSE)
      updating_year_range(FALSE)
    }, 0.1)
  })
  
  # Year slider handler - only works when on All Data tab and not updating
  observeEvent(input$year_range, {
    # Only process if we're on the All Data tab and not already updating
    if (!updating_year_range() && 
        !is.null(input$main_tabset) && 
        input$main_tabset == "All Data (Current + Archive)") {
      
      start_year <- input$year_range[1]
      end_year <- input$year_range[2]
      
      # Create date range from year selection
      start_date <- as.Date(paste0(start_year, "-01-01"))
      end_date <- if (end_year == as.numeric(format(Sys.Date(), "%Y"))) {
        # If current year is selected as end, use today's date
        Sys.Date()
      } else {
        # Otherwise use end of that year
        as.Date(paste0(end_year, "-12-31"))
      }
      
      # Set flag and update date range input
      updating_date_range(TRUE)
      updateDateRangeInput(session, "date_range", start = start_date, end = end_date)
      
      # Reset flag after a short delay
      later::later(function() {
        updating_date_range(FALSE)
      }, 0.1)
    }
  }, ignoreInit = TRUE)  # Ignore initial trigger
  
  # Custom date range handler - behavior depends on active tab and not updating
  observeEvent(input$date_range, {
    if (!updating_date_range() && 
        !is.null(input$date_range) && 
        length(input$date_range) == 2) {
      
      start_year <- as.numeric(format(input$date_range[1], "%Y"))
      end_year <- as.numeric(format(input$date_range[2], "%Y"))
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      
      # If on Current Data tab, restrict to current year only
      if (!is.null(input$main_tabset) && input$main_tabset == "Current Data") {
        if (start_year != current_year || end_year != current_year) {
          # Force dates back to current year
          new_start <- as.Date(paste0(current_year, "-01-01"))
          new_end <- Sys.Date()
          
          updating_date_range(TRUE)
          updateDateRangeInput(session, "date_range", start = new_start, end = new_end)
          
          # Show a notification
          showNotification("Current Data tab is limited to current year data only. Use 'All Data' tab for multi-year analysis.", 
                          type = "warning", duration = 3)
          
          # Reset flag after a short delay
          later::later(function() {
            updating_date_range(FALSE)
          }, 0.1)
        }
      } else if (!is.null(input$main_tabset) && input$main_tabset == "All Data (Current + Archive)") {
        # On All Data tab, update year slider to match
        if (!identical(c(start_year, end_year), input$year_range)) {
          updating_year_range(TRUE)
          updateSliderInput(session, "year_range", value = c(start_year, end_year))
          
          # Reset flag after a short delay
          later::later(function() {
            updating_year_range(FALSE)
          }, 0.1)
        }
      }
    }
  }, ignoreInit = TRUE)  # Ignore initial trigger
  
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
  
  # Function to convert well-known binary (WKB) to sf object
  wkb_to_sf <- function(wkb) {
    if (is.na(wkb) || is.null(wkb) || wkb == "") {
      return(NULL)
    }
    
    tryCatch({
      # First, try to parse as hex WKB
      point <- sf::st_as_sfc(wkb, hex = TRUE, crs = 4326)
      return(point)
    }, error = function(e) {
      tryCatch({
        # If that fails, try parsing as WKT
        if (substr(wkb, 1, 5) == "POINT" ||
            substr(wkb, 1, 10) == "MULTIPOINT" ||
            substr(wkb, 1, 10) == "LINESTRING" ||
            substr(wkb, 1, 7) == "POLYGON") {
          point <- sf::st_as_sfc(wkb, crs = 4326)
          return(point)
        }
        return(NULL)
      }, error = function(e) {
        return(NULL)
      })
    })
  }
  
  # Fetch and join SUCO and species data
  suco_data <- reactive({
    con <- get_db_connection()
    if (is.null(con)) {
      return(data.frame())  # Return empty data frame if connection fails
    }
    
    # Date range for query
    start_date <- format(input$date_range[1], "%Y-%m-%d")
    end_date <- format(input$date_range[2], "%Y-%m-%d")
    
    # Query current data for SUCOs (survtype = 7) with harborage lookup for current foreman assignments
    current_query <- sprintf("
SELECT
s.id, s.ainspecnum, s.facility, 
CASE 
  WHEN h.foreman IS NOT NULL AND h.foreman != '' THEN h.foreman 
  WHEN s.foreman IS NOT NULL AND s.foreman != '' THEN s.foreman
  ELSE NULL
END as foreman,
s.inspdate, s.sitecode,
s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
s.x, s.y, ST_AsText(s.geometry) as geometry_text,
COALESCE(h.facility, '') as harborage_facility,
g.zone
FROM public.dbadult_insp_current s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND s.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR s.inspdate <= h.enddate)
LEFT JOIN public.gis_sectcode g ON LEFT(s.sitecode, 6) || '-' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'N' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'S' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'E' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'W' = g.sectcode
WHERE s.survtype = '7'
AND s.inspdate BETWEEN '%s' AND '%s'
", start_date, end_date)
    
    # Query archive data for SUCOs with harborage lookup for current foreman assignments
    archive_query <- sprintf("
SELECT
s.id, s.ainspecnum, s.facility,
CASE 
  WHEN h.foreman IS NOT NULL AND h.foreman != '' THEN h.foreman 
  WHEN s.foreman IS NOT NULL AND s.foreman != '' THEN s.foreman
  ELSE NULL
END as foreman,
s.inspdate, s.sitecode,
s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
s.x, s.y, ST_AsText(s.geometry) as geometry_text,
COALESCE(h.facility, '') as harborage_facility,
g.zone
FROM public.dbadult_insp_archive s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND s.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR s.inspdate <= h.enddate)
LEFT JOIN public.gis_sectcode g ON LEFT(s.sitecode, 6) || '-' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'N' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'S' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'E' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'W' = g.sectcode
WHERE s.survtype = '7'
AND s.inspdate BETWEEN '%s' AND '%s'
", start_date, end_date)
    
    # Execute queries
    current_data <- dbGetQuery(con, current_query)
    archive_data <- dbGetQuery(con, archive_query)
    
    # Query species tables (current and archive)
    species_current_query <- sprintf("
SELECT ainspecnum, spp, cnt
FROM public.dbadult_species_current
WHERE ainspecnum IS NOT NULL
")
    species_archive_query <- sprintf("
SELECT ainspecnum, spp, cnt
FROM public.dbadult_species_archive
WHERE ainspecnum IS NOT NULL
")
    species_current <- dbGetQuery(con, species_current_query)
    species_archive <- dbGetQuery(con, species_archive_query)

    # Query species lookup table using centralized function
    species_lookup <- get_species_lookup()

    # Close connection
    dbDisconnect(con)

    # Combine current and archive data
    all_data <- bind_rows(
      mutate(current_data, source = "Current"),
      mutate(archive_data, source = "Archive")
    ) %>%
      mutate(
        inspdate = as.Date(inspdate),
        year = year(inspdate),
        month = month(inspdate),
        week_start = floor_date(inspdate, "week", week_start = 1),
        month_label = format(inspdate, "%b %Y"),
        location = ifelse(!is.na(park_name) & park_name != "", park_name,
                          ifelse(!is.na(address1) & address1 != "", address1, sitecode)),
        # Use harborage facility when harborage foreman is used
        # This ensures foreman colors match the correct facility
        facility = ifelse(!is.na(harborage_facility) & harborage_facility != "", 
                         harborage_facility, facility)
      )
    
    # Combine species data
    all_species <- bind_rows(species_current, species_archive)

    # Join SUCO data with species data and lookup for names
    joined_data <- all_data %>%
      left_join(all_species, by = "ainspecnum", relationship = "many-to-many") %>%
      left_join(species_lookup, by = c("spp" = "sppcode")) %>%
      mutate(
        species_name = dplyr::case_when(
          !is.na(genus) & !is.na(species) ~ paste(genus, species),
          !is.na(spp) ~ as.character(spp),
          TRUE ~ NA_character_
        ),
        # Ensure foreman values are consistent strings
        foreman = trimws(as.character(foreman))
      )
    return(joined_data)
  })
  
  # Current data only (no archive) - more efficient for fast loading
  suco_data_current <- reactive({
    # Simple checks only
    req(input$date_range, input$main_tabset)
    
    # Only run when actually on Current Data tab
    if (input$main_tabset != "Current Data") {
      return(data.frame())
    }
    
    tryCatch({
      con <- get_db_connection()
      if (is.null(con)) {
        return(data.frame())
      }
      
      # Date range for query
      start_date <- format(input$date_range[1], "%Y-%m-%d")
      end_date <- format(input$date_range[2], "%Y-%m-%d")
      
      # Query with essential fields and proper zone detection, including harborage lookup
      current_query <- sprintf("
SELECT
s.id, s.ainspecnum, s.facility, 
CASE 
  WHEN h.foreman IS NOT NULL AND h.foreman != '' THEN h.foreman 
  WHEN s.foreman IS NOT NULL AND s.foreman != '' THEN s.foreman
  ELSE NULL
END as foreman,
s.inspdate, s.sitecode,
s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
s.x, s.y,
g.zone
FROM public.dbadult_insp_current s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND s.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR s.inspdate <= h.enddate)
LEFT JOIN public.gis_sectcode g ON LEFT(s.sitecode, 6) || '-' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'N' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'S' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'E' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'W' = g.sectcode
WHERE s.survtype = '7'
AND s.inspdate BETWEEN '%s' AND '%s'
", start_date, end_date)
      
      current_data <- dbGetQuery(con, current_query)
      
      # Get species data
      species_current_query <- "
SELECT ainspecnum, spp, cnt
FROM public.dbadult_species_current
WHERE ainspecnum IS NOT NULL
"
      species_current <- dbGetQuery(con, species_current_query)
      
      # Get species lookup
      species_lookup <- get_species_lookup()
      
      dbDisconnect(con)
      
      if (nrow(current_data) == 0) {
        return(data.frame())
      }
      
      # Process data with all required fields
      processed_data <- current_data %>%
        mutate(
          inspdate = as.Date(inspdate),
          year = year(inspdate),
          month = month(inspdate),
          week_start = floor_date(inspdate, "week", week_start = 1),
          month_label = format(inspdate, "%b %Y"),
          location = ifelse(!is.na(park_name) & park_name != "", park_name,
                            ifelse(!is.na(address1) & address1 != "", address1, sitecode)),
          foreman = trimws(as.character(foreman)),
          zone = as.character(zone)
        ) %>%
        left_join(species_current, by = "ainspecnum", relationship = "many-to-many") %>%
        left_join(species_lookup, by = c("spp" = "sppcode")) %>%
        mutate(
          species_name = case_when(
            !is.na(genus) & !is.na(species) ~ paste(genus, species),
            !is.na(spp) ~ as.character(spp),
            TRUE ~ NA_character_
          )
        )
      
      return(processed_data)
      
    }, error = function(e) {
      cat("Error in suco_data_current:", e$message, "\n")
      return(data.frame())
    })
  })
  
  # Helper function to create trend plots - eliminates code duplication
  create_trend_plot <- function(data_source) {
    # Get aggregated data based on data source
    if (data_source == "current") {
      data <- aggregated_data_current()
    } else {
      data <- aggregated_data()
    }
    
    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste("No SUCO data available with the selected filters", 
                                if(data_source == "current") "(Current Data)" else ""), size = 6) +
          theme_void()
      )
    }
    
    # Use weekly labels (MM/DD format)
    data$time_label <- format(data$time_group, "%m/%d")
    
    group_col <- input$group_by
    if (group_col == "mmcd_all") {
      group_col <- "mmcd_all"
    }
    
    # Get facility and foreman lookups for labels
    facilities <- get_facility_lookup()
    foremen_lookup <- get_foremen_lookup()
    
    # Create facility name mapping
    facility_names <- setNames(facilities$full_name, facilities$short_name)
    
    # Create foreman name mapping
    foreman_names <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
    
    title_interval <- "Weekly"
    title_group <- case_when(
      input$group_by == "facility" ~ "Facility",
      input$group_by == "foreman" ~ "Foreman", 
      input$group_by == "mmcd_all" ~ "MMCD (All)",
      TRUE ~ "Group"
    )
    
    # Create filter text for subtitle
    facility_text <- if ("All" %in% input$facility_filter) {
      "All Facilities"
    } else {
      display_names <- sapply(input$facility_filter, function(f) facility_names[f] %||% f)
      paste("Facility:", paste(display_names, collapse=", "))
    }
    
    foreman_text <- if ("All" %in% input$foreman_filter) {
      "All Foremen"
    } else {
      display_names <- sapply(input$foreman_filter, function(f) foreman_names[f] %||% f)
      paste("Foreman:", paste(display_names, collapse=", "))
    }
    
    # Zone filter text
    zone_text <- if (length(input$zone_filter) == 2) {
      "Zones: P1, P2"
    } else if (length(input$zone_filter) == 1) {
      paste("Zone:", paste0("P", input$zone_filter))
    } else {
      "No Zones"
    }
    
    # Get color scales from db_helpers based on grouping
    custom_colors <- if(group_col == "facility") {
      get_facility_base_colors()
    } else if(group_col == "foreman") {
      # Get the foreman colors from db_helpers
      foreman_colors <- get_foreman_colors()
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
          facility <- foremen_lookup$facility[matches[1]]
          
          # Get the facility-based color for this shortname
          if(shortname %in% names(foreman_colors)) {
            emp_colors[foreman_num_str] <- foreman_colors[shortname]
          }
        }
      }
      
      # Remove any NA colors
      emp_colors <- emp_colors[!is.na(emp_colors)]
      
      # Return the mapped colors
      emp_colors
    } else {
      NULL
    }
    
    # Determine the plotting group column and handle color mapping for combined zones
    plot_group_col <- if (length(input$zone_filter) > 1 && "combined_group" %in% names(data)) {
      "combined_group"
    } else if (length(input$zone_filter) > 1 && "zone_label" %in% names(data)) {
      "zone_label"
    } else {
      group_col
    }
    
    # If using combined groups, create color mapping based on base names
    if (plot_group_col %in% c("combined_group", "zone_label") && !is.null(custom_colors)) {
      # Extract base names from combined groups (remove zone info)
      base_names <- unique(data[[plot_group_col]])
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
      
      # Add zone column for visual differentiation
      if ("combined_group" %in% names(data)) {
        data$zone_type <- ifelse(grepl("\\(P1\\)", data$combined_group), "P1", 
                                ifelse(grepl("\\(P2\\)", data$combined_group), "P2", "Unknown"))
      }
    }
    
    # Create the plot using the appropriate group column for color mapping
    plot_aes <- if (plot_group_col %in% c("combined_group", "zone_label") && "zone_type" %in% names(data)) {
      # Use color and fill for facility/foreman, alpha will be added separately
      aes(x = time_group, y = count, 
          color = !!sym(plot_group_col), 
          fill = !!sym(plot_group_col), 
          group = !!sym(plot_group_col))
    } else {
      # Standard mapping when single zone or no zone info
      aes(x = time_group, y = count, 
          color = !!sym(plot_group_col), 
          fill = !!sym(plot_group_col), 
          group = !!sym(plot_group_col))
    }
    
    p <- ggplot(data, plot_aes)
    
    
    # Add color scales based on grouping
    if(!is.null(custom_colors)) {
      # Add color scales with labels for the legend
      if(group_col == "facility" && plot_group_col != "combined_group") {
        # For facilities, map short names to full names in legend
        p <- p + scale_color_manual(
          values = custom_colors,
          labels = function(x) sapply(x, function(v) facility_names[v] %||% v),
          drop = FALSE
        ) + scale_fill_manual(
          values = custom_colors,
          labels = function(x) sapply(x, function(v) facility_names[v] %||% v),
          drop = FALSE
        )
      } else if(group_col == "foreman" && plot_group_col != "combined_group") {
        # For foremen, map employee numbers to names in legend
        p <- p + scale_color_manual(
          values = custom_colors,
          labels = function(x) sapply(x, function(v) foreman_names[v] %||% v),
          drop = FALSE
        ) + scale_fill_manual(
          values = custom_colors,
          labels = function(x) sapply(x, function(v) foreman_names[v] %||% v),
          drop = FALSE
        )
      } else if(plot_group_col == "combined_group") {
        # For combined groups, use the mapped colors directly
        p <- p + scale_color_manual(
          values = custom_colors,
          drop = FALSE
        ) + scale_fill_manual(
          values = custom_colors,
          drop = FALSE
        )
        
        # Add discrete alpha scale for zone differentiation that shows in legend
        if ("zone_type" %in% names(data)) {
          # Create a factor for zone_type to ensure proper legend
          data$zone_factor <- factor(data$zone_type, levels = c("P1", "P2"))
          
          # Rebuild plot with alpha aesthetic
          p <- ggplot(data, aes(x = time_group, y = count, 
                               color = !!sym(plot_group_col), 
                               fill = !!sym(plot_group_col),
                               alpha = zone_factor,
                               group = !!sym(plot_group_col))) +
            scale_color_manual(
              values = custom_colors,
              drop = FALSE
            ) + scale_fill_manual(
              values = custom_colors,
              drop = FALSE
            ) +
            scale_alpha_manual(
              name = "Zone",
              values = c("P1" = 1.0, "P2" = 0.6),
              labels = c("P1" = "P1 (Solid)", "P2" = "P2 (Faded)"),
              drop = FALSE
            )
        }
      } else {
        # Default case
        p <- p + scale_color_manual(values = custom_colors, drop = FALSE) + 
                 scale_fill_manual(values = custom_colors, drop = FALSE)
      }
    } else {
      p <- p + scale_color_discrete() + scale_fill_discrete()
    }
    
    if (input$graph_type == "bar") {
      p <- p + geom_bar(stat = "identity", position = "dodge")
    } else if (input$graph_type == "line") {
      p <- p + geom_line(size = 1.2)
    } else if (input$graph_type == "point") {
      p <- p + geom_point(size = 3)
    } else if (input$graph_type == "area") {
      p <- p + geom_area(position = "stack", alpha = 0.6)
    }
    
    subtitle_text <- paste(zone_text, "-", facility_text, "-", foreman_text)
    if (data_source == "current") {
      subtitle_text <- paste(subtitle_text, "(Current Data Only)")
    }
    
    p <- p + labs(
      title = paste(title_interval, "SUCO Counts by", ifelse(input$group_by == "mmcd_all", "MMCD (All)", title_group)),
      subtitle = subtitle_text,
      x = "Week Starting",
      y = "Number of SUCOs",
      fill = ifelse(input$group_by == "mmcd_all", "MMCD (All)", title_group),
      color = ifelse(input$group_by == "mmcd_all", "MMCD (All)", title_group)
    ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")
      )
    # Use weekly scale
    p <- p + scale_x_date(
      date_breaks = "2 weeks",
      date_labels = "%m/%d",
      limits = c(min(data$time_group), max(data$time_group))
    )
    return(p)
  }
  
  # Update species filter choices based on available data (use species_name)
  observe({
    data <- suco_data()
    
    # Get unique species
    spp_choices <- sort(unique(na.omit(data$species_name)))
    spp_choices <- c("All", spp_choices)
    
    # Update select input
    updateSelectInput(session, "species_filter", choices = spp_choices)
  })
  
  # Update foreman filter choices based on available data (multi-select aware)
  observe({
    data <- suco_data()
    foremen_lookup <- get_foremen_lookup()
    
    # Filter by facility if not 'All' and not empty
    if (!is.null(input$facility_filter) && !("All" %in% input$facility_filter)) {
      data <- data %>% filter(facility %in% input$facility_filter)
    }
    
    # Get unique foremen numbers from data
    foremen_nums <- sort(unique(na.omit(data$foreman)))
    
    # Create mapping from emp_num to shortname for display with robust matching
    foremen_names <- sapply(foremen_nums, function(num) {
      # Ensure both sides are strings and trimmed
      num_str <- trimws(as.character(num))
      matches <- which(trimws(as.character(foremen_lookup$emp_num)) == num_str)
      if(length(matches) > 0) {
        foremen_lookup$shortname[matches[1]]
      } else {
        paste0("FOS #", num_str)  # fallback to formatted number
      }
    })
    
    # Create choices with names as labels and numbers as values
    foremen_choices <- setNames(
      c("All", foremen_nums),
      c("All", foremen_names)
    )
    
    updateSelectInput(session, "foreman_filter", choices = foremen_choices)
  })

  # Filter data based on user selections, now including multi-select for facility/foreman
  filtered_data <- reactive({
    data <- suco_data()
    # Filter by zone if selected
    if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
      data <- data %>% filter(zone %in% input$zone_filter)
    }
    # Filter by facility if not 'All' and not empty
    if (!is.null(input$facility_filter) && !("All" %in% input$facility_filter)) {
      data <- data %>% filter(facility %in% input$facility_filter)
    }
    # Filter by foreman if not 'All' and not empty
    if (!is.null(input$foreman_filter) && !("All" %in% input$foreman_filter)) {
      data <- data %>% filter(foreman %in% input$foreman_filter)
    }
    # Filter by species if selected
    if (input$species_filter != "All") {
      data <- data %>% filter(species_name == input$species_filter)
    }
    return(data)
  })
  
  # Filter current data based on user selections
  filtered_data_current <- reactive({
    data <- suco_data_current()
    # Filter by zone if selected
    if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
      data <- data %>% filter(zone %in% input$zone_filter)
    }
    # Filter by facility if not 'All' and not empty
    if (!is.null(input$facility_filter) && !("All" %in% input$facility_filter)) {
      data <- data %>% filter(facility %in% input$facility_filter)
    }
    # Filter by foreman if not 'All' and not empty
    if (!is.null(input$foreman_filter) && !("All" %in% input$foreman_filter)) {
      data <- data %>% filter(foreman %in% input$foreman_filter)
    }
    # Filter by species if selected
    if (input$species_filter != "All") {
      data <- data %>% filter(species_name == input$species_filter)
    }
    return(data)
  })
  
  # Process spatial data for mapping
  spatial_data <- reactive({
    data <- filtered_data()
    
    # Create sf object for mapping
    sf_data <- data %>%
      # Use x and y coordinates if available, otherwise try to parse geometry
      mutate(
        longitude = as.numeric(x),
        latitude = as.numeric(y),
        has_coords = !is.na(longitude) & !is.na(latitude) &
          longitude > -180 & longitude < 180 &
          latitude > -90 & latitude < 90
      ) %>%
      filter(has_coords) %>%
      # Create sf object
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    return(sf_data)
  })
  
  # Process spatial data for mapping (current data only)
  spatial_data_current <- reactive({
    data <- filtered_data_current()
    
    # Create sf object for mapping
    sf_data <- data %>%
      # Use x and y coordinates if available, otherwise try to parse geometry
      mutate(
        longitude = as.numeric(x),
        latitude = as.numeric(y),
        has_coords = !is.na(longitude) & !is.na(latitude) &
          longitude > -180 & longitude < 180 &
          latitude > -90 & latitude < 90
      ) %>%
      filter(has_coords) %>%
      # Create sf object
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    return(sf_data)
  })
  
  # Aggregate data by selected time interval and grouping
  aggregated_data <- reactive({
    data <- filtered_data()
    
    # Use weekly intervals (Monday start date of each week)
    data <- data %>%
      mutate(time_group = week_start)
    
    # Define the grouping column
    group_col <- input$group_by
    
    # Check if we should show zones as separate entities
    show_zones_separately <- length(input$zone_filter) > 1
    
    if (group_col == "mmcd_all") {
      if (show_zones_separately) {
        # Group by zone when both P1 and P2 are selected
        result <- data %>%
          group_by(time_group, zone) %>%
          summarize(
            count = n(),
            total_fieldcount = sum(fieldcount, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(time_group) %>%
          mutate(zone_label = paste0("P", zone))
      } else {
        # Aggregate for the whole MMCD (no grouping by facility or foreman)
        result <- data %>%
          group_by(time_group) %>%
          summarize(
            count = n(),
            total_fieldcount = sum(fieldcount, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(time_group)
        # Add a dummy column for plotting
        result$mmcd_all <- "MMCD (All)"
      }
    } else {
      if (show_zones_separately) {
        # Group by both the selected grouping column AND zone
        result <- data %>%
          group_by(time_group, !!sym(group_col), zone) %>%
          summarize(
            count = n(),
            total_fieldcount = sum(fieldcount, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(time_group) %>%
          mutate(
            combined_group = paste0(!!sym(group_col), " (P", zone, ")"),
            zone_label = paste0("P", zone)
          )
      } else {
        # Group and summarize data by the selected column only
        result <- data %>%
          group_by(time_group, !!sym(group_col)) %>%
          summarize(
            count = n(),
            total_fieldcount = sum(fieldcount, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(time_group)
      }
    }
    
    return(result)
  })
  
  # Aggregate current data by selected time interval and grouping
  aggregated_data_current <- reactive({
    data <- filtered_data_current()
    
    # Use weekly intervals (Monday start date of each week)
    data <- data %>%
      mutate(time_group = week_start)
    
    # Define the grouping column
    group_col <- input$group_by
    
    # Check if we should show zones as separate entities
    show_zones_separately <- length(input$zone_filter) > 1
    
    if (group_col == "mmcd_all") {
      if (show_zones_separately) {
        # Group by zone when both P1 and P2 are selected
        result <- data %>%
          group_by(time_group, zone) %>%
          summarize(
            count = n(),
            total_fieldcount = sum(fieldcount, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(time_group) %>%
          mutate(
            mmcd_all = "MMCD (All)",
            zone_label = paste0("P", zone),
            combined_group = paste0("MMCD (All) (P", zone, ")")
          )
      } else {
        # Group all data together for MMCD (All)
        result <- data %>%
          group_by(time_group) %>%
          summarize(
            count = n(),
            total_fieldcount = sum(fieldcount, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(time_group) %>%
          mutate(mmcd_all = "MMCD (All)")
      }
    } else {
      if (show_zones_separately) {
        # Group by both the selected column and zone
        result <- data %>%
          group_by(time_group, !!sym(group_col), zone) %>%
          summarize(
            count = n(),
            total_fieldcount = sum(fieldcount, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(time_group) %>%
          mutate(
            combined_group = paste0(!!sym(group_col), " (P", zone, ")"),
            zone_label = paste0("P", zone)
          )
      } else {
        # Group and summarize data by the selected column only
        result <- data %>%
          group_by(time_group, !!sym(group_col)) %>%
          summarize(
            count = n(),
            total_fieldcount = sum(fieldcount, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(time_group)
      }
    }
    
    return(result)
  })
  
  # Generate trend plot
  output$trend_plot <- renderPlot({
    create_trend_plot("all")
  })
  
  # Generate current trend plot (for current data tab)
  output$current_trend_plot <- renderPlot({
    create_trend_plot("current")
  })
  
  # Generate map
  output$map <- renderLeaflet({
    # Get spatial data
    data <- spatial_data()
    
    # Get marker size multiplier
    size_multiplier <- input$marker_size
    
    # Get facility and foremen lookups for display names
    facilities <- get_facility_lookup()
    
    # Set up basemap provider
    basemap <- switch(input$basemap,
                      "osm" = providers$OpenStreetMap,
                      "carto" = providers$CartoDB.Positron,
                      "terrain" = providers$Stamen.Terrain,
                      "satellite" = providers$Esri.WorldImagery,
                      providers$CartoDB.Positron)
    
    # Handle case when no data is available
    if (nrow(data) == 0) {
      # Return empty map with message
      return(
        leaflet() %>%
          addProviderTiles(basemap) %>%
          setView(lng = -93.2, lat = 45.0, zoom = 9) %>%
          addControl(html = "<div style='background-color: white; padding: 10px;'><h4>No SUCO locations available with the selected filters</h4></div>",
                     position = "topleft")
      )
    }
    
    # Create color palette based on field count or facility
    if (input$group_by == "facility") {
      # Get facility colors and lookup from db_helpers
      facility_colors <- get_facility_base_colors()
      facilities <- get_facility_lookup()
      foremen_lookup <- get_foremen_lookup()  # Add foremen lookup for popups
      foremen_lookup$emp_num <- as.character(foremen_lookup$emp_num)  # Ensure string format
      
      # Create facility name mapping FIRST
      facility_names <- setNames(facilities$full_name, facilities$short_name)
      
      # Create popup text beforehand to avoid scope issues
      data <- data %>%
        mutate(
          popup_text = {
            # Get proper foreman names with robust matching
            foreman_names <- sapply(foreman, function(f) {
              if (!is.na(f) && f != "" && !is.null(f)) {
                # Ensure both foreman and emp_num are strings for comparison
                foreman_str <- trimws(as.character(f))
                
                # Find matching foreman in lookup table
                matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_str)
                
                if(length(matches) > 0) {
                  foremen_lookup$shortname[matches[1]]
                } else {
                  # Fallback: show the raw foreman ID if no match found
                  paste0("FOS #", foreman_str)
                }
              } else {
                "No FOS assigned"
              }
            })
            
            facility_names_vec <- sapply(facility, function(f) {
              fname <- facility_names[f]
              if(length(fname) > 0) fname[1] else f
            })
            
            paste0("<b>Date:</b> ", inspdate, "<br>",
                   "<b>Facility:</b> ", facility_names_vec, "<br>",
                   "<b>FOS:</b> ", foreman_names, "<br>",
                   "<b>Location:</b> ", location, "<br>",
                   "<b>Field Count:</b> ", fieldcount)
          }
        )
      
      # Create color palette function
      pal <- colorFactor(
        palette = facility_colors,
        domain = names(facility_colors))
      
      # Create a named vector for legend labels
      legend_labels <- sapply(names(facility_colors), function(code) facility_names[code] %||% code)
      
      # Create map with facility coloring
      leaflet(data) %>%
        addProviderTiles(basemap) %>%
        fitBounds(
          lng1 = min(st_coordinates(data)[,1]),
          lat1 = min(st_coordinates(data)[,2]),
          lng2 = max(st_coordinates(data)[,1]),
          lat2 = max(st_coordinates(data)[,2])
        ) %>%
        addCircleMarkers(
          radius = ~pmin(15, (3 * size_multiplier)),
          color = "black",
          weight = 1.5,
          fillColor = ~pal(facility),
          fillOpacity = 0.8,
          popup = ~popup_text
        ) %>%
        addLegend(
          position = "bottomright",
          title = "Facility",
          colors = facility_colors,
          labels = legend_labels,
          opacity = 0.8
        )
    } else if (input$group_by == "foreman") {
      # Filter out records with NA foreman before processing
      data <- data %>% filter(!is.na(foreman) & foreman != "")
      
      # Check if we have any data left after filtering
      if (nrow(data) == 0) {
        return(
          leaflet() %>%
            addProviderTiles(basemap) %>%
            setView(lng = -93.2, lat = 45.0, zoom = 9) %>%
            addControl(html = "<div style='background-color: white; padding: 10px;'><h4>No SUCO locations with valid FOS data available</h4></div>",
                       position = "topleft")
        )
      }
      
      # Get both colors and lookup exactly as documented
      foreman_colors <- get_foreman_colors()
      foremen_lookup <- get_foremen_lookup()
      
      # Ensure employee numbers are consistently formatted
      # foremen_lookup$emp_num is already properly formatted
      
      # Create mapping from foreman NUMBER to facility-based colors (same logic as plot)
      foremen_in_data <- unique(na.omit(data$foreman))
      emp_colors <- character(0)
      
      for (foreman_num in foremen_in_data) {
        foreman_num_str <- trimws(as.character(foreman_num))
        
        # Find the shortname for this foreman number
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
        
        if(length(matches) > 0) {
          shortname <- foremen_lookup$shortname[matches[1]]
          facility <- foremen_lookup$facility[matches[1]]
          
          
          # Get the facility-based color for this shortname
          if(shortname %in% names(foreman_colors)) {
            emp_colors[foreman_num_str] <- foreman_colors[shortname]
          }
        }
      }
      
      # Remove any NA colors
      emp_colors <- emp_colors[!is.na(emp_colors)]
      
      # Create ORDERED colors to ensure legend and map match exactly
      # Order by facility, then by foreman within facility (same as legend)
      ordered_foremen <- foremen_lookup[order(foremen_lookup$facility, foremen_lookup$shortname), ]
      ordered_emp_colors <- character(0)
      ordered_emp_numbers <- character(0)
      
      for (i in 1:nrow(ordered_foremen)) {
        emp_num <- trimws(as.character(ordered_foremen$emp_num[i]))
        if (emp_num %in% names(emp_colors)) {
          ordered_emp_colors <- c(ordered_emp_colors, emp_colors[emp_num])
          ordered_emp_numbers <- c(ordered_emp_numbers, emp_num)
        }
      }
      names(ordered_emp_colors) <- ordered_emp_numbers
      
      # Create color palette function for leaflet using ORDERED colors
      pal <- colorFactor(
        palette = ordered_emp_colors,
        domain = names(ordered_emp_colors),
        ordered = TRUE  # Maintain order
      )
      
      # Create popup text beforehand for foreman map too
      data <- data %>%
        mutate(
          popup_text_foreman = {
            # Get proper foreman names with robust matching
            foreman_names <- sapply(foreman, function(f) {
              if (!is.na(f) && f != "" && !is.null(f)) {
                # Ensure both foreman and emp_num are strings for comparison
                foreman_str <- trimws(as.character(f))
                
                # Find matching foreman in lookup table
                matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_str)
                
                if(length(matches) > 0) {
                  foremen_lookup$shortname[matches[1]]
                } else {
                  # Fallback: show the raw foreman ID if no match found
                  paste0("FOS #", foreman_str)
                }
              } else {
                "No FOS assigned"
              }
            })
            
            facility_names_vec <- sapply(facility, function(f) {
              fname <- facilities$full_name[facilities$short_name == f]
              if(length(fname) > 0) fname[1] else f
            })
            
            paste0("<b>Date:</b> ", inspdate, "<br>",
                   "<b>Facility:</b> ", facility_names_vec, "<br>",
                   "<b>FOS:</b> ", foreman_names, "<br>",
                   "<b>Location:</b> ", location, "<br>",
                   "<b>Field Count:</b> ", fieldcount)
          }
        )
      
      # Create map with foreman coloring
      leaflet(data) %>%
        addProviderTiles(basemap) %>%
        fitBounds(
          lng1 = min(st_coordinates(data)[,1]),
          lat1 = min(st_coordinates(data)[,2]),
          lng2 = max(st_coordinates(data)[,1]),
          lat2 = max(st_coordinates(data)[,2])
        ) %>%
        addCircleMarkers(
          radius = ~pmin(15, (3 * size_multiplier)),
          color = "black",
          weight = 1.5,
          fillColor = ~pal(foreman),
          fillOpacity = 0.8,
          popup = ~popup_text_foreman
        ) %>%
        addLegend(
          position = "bottomright",
          title = "FOS",
          colors = ordered_emp_colors,  # Use same ordered colors as map palette
          labels = {
            # Use same ordering as the map palette
            ordered_labels <- character(0)
            for (i in 1:length(ordered_emp_numbers)) {
              emp_num <- ordered_emp_numbers[i]
              foreman_info <- foremen_lookup[trimws(as.character(foremen_lookup$emp_num)) == emp_num, ]
              if(nrow(foreman_info) > 0) {
                ordered_labels <- c(ordered_labels, foreman_info$shortname[1])
              }
            }
            ordered_labels
          },
          opacity = 0.8
        )
    } else {
      # For MMCD (All) case, use a single color
      leaflet(data) %>%
        addProviderTiles(basemap) %>%
        fitBounds(
          lng1 = min(st_coordinates(data)[,1]),
          lat1 = min(st_coordinates(data)[,2]),
          lng2 = max(st_coordinates(data)[,1]),
          lat2 = max(st_coordinates(data)[,2])
        ) %>%
        addCircleMarkers(
          radius = ~pmin(15, (3 * size_multiplier)),
          color = "black",
          weight = 1.5,
          fillColor = "#1f77b4", # Standard blue color
          fillOpacity = 0.8,
          popup = ~paste0("<b>Date:</b> ", inspdate, "<br>",
                          "<b>Facility:</b> ", facility, "<br>",
                          "<b>Foreman:</b> ", foreman, "<br>",
                          "<b>Location:</b> ", location, "<br>",
                          "<b>Field Count:</b> ", fieldcount)
        ) %>%
        addLegend(
          position = "bottomright",
          title = "MMCD",
          colors = "#1f77b4",
          labels = "All",
          opacity = 0.8
        )
    }
  })
  
  # Generate summary table
  output$summary_table <- renderDataTable({
    data <- filtered_data()
    group_col <- input$group_by
    
    if (group_col == "mmcd_all") {
      # Summarize for the whole MMCD (no grouping)
      summary_data <- data %>%
        summarize(
          Total_SUCOs = n(),
          Total_Locations = n_distinct(sitecode),
          Total_Species_Count = sum(cnt, na.rm = TRUE),
          First_SUCO = min(inspdate),
          Last_SUCO = max(inspdate)
        )
      rownames(summary_data) <- "MMCD (All)"
    } else if (group_col == "facility") {
      # Get all facilities from lookup
      facilities <- get_facility_lookup()
      
      # Create base data frame with all facilities
      all_facilities <- data.frame(
        facility = facilities$short_name,
        full_name = facilities$full_name
      )
      
      # Summarize actual data
      summary_stats <- data %>%
        group_by(facility) %>%
        summarize(
          Total_SUCOs = n(),
          Total_Locations = n_distinct(sitecode),
          Total_Species_Count = sum(cnt, na.rm = TRUE),
          First_SUCO = min(inspdate),
          Last_SUCO = max(inspdate)
        )
      
      # Join with all facilities to include zeros
      summary_data <- all_facilities %>%
        left_join(summary_stats, by = "facility") %>%
        mutate(
          Total_SUCOs = replace_na(Total_SUCOs, 0),
          Total_Locations = replace_na(Total_Locations, 0),
          Total_Species_Count = replace_na(Total_Species_Count, 0),
          First_SUCO = as.Date(First_SUCO),
          Last_SUCO = as.Date(Last_SUCO)
        ) %>%
        arrange(desc(Total_SUCOs), facility)
      
      # Rename columns
      colnames(summary_data)[1:2] <- c("Facility", "Facility_Name")
      
    } else {  # foreman grouping
      # Get foreman lookup
      foremen_lookup <- get_foremen_lookup()
      
      # Note: both foremen_lookup$emp_num and data$foreman are already properly formatted
      
      # Create base data frame with all foremen
      all_foremen <- data.frame(
        foreman = foremen_lookup$emp_num,
        shortname = foremen_lookup$shortname,
        facility = foremen_lookup$facility
      )
      
      # Summarize actual data
      summary_stats <- data %>%
        group_by(foreman) %>%
        summarize(
          Total_SUCOs = n(),
          Total_Locations = n_distinct(sitecode),
          Total_Species_Count = sum(cnt, na.rm = TRUE),
          First_SUCO = min(inspdate),
          Last_SUCO = max(inspdate)
        )
      
      # Join with all foremen to include zeros
      summary_data <- all_foremen %>%
        left_join(summary_stats, by = c("foreman")) %>%
        mutate(
          Total_SUCOs = replace_na(Total_SUCOs, 0),
          Total_Locations = replace_na(Total_Locations, 0),
          Total_Species_Count = replace_na(Total_Species_Count, 0),
          First_SUCO = as.Date(First_SUCO),
          Last_SUCO = as.Date(Last_SUCO)
        ) %>%
        arrange(desc(Total_SUCOs), facility, shortname)
      
      # Rename columns
      colnames(summary_data)[1:3] <- c("Foreman", "Name", "Facility")
    }
    return(summary_data)
  }, options = list(pageLength = 15, 
                   searching = TRUE,
                   columnDefs = list(list(
                     targets = c("First_SUCO", "Last_SUCO"),
                     render = JS("function(data, type, row) {
                       return data ? data : 'N/A';
                     }")
                   ))))
  
  # Generate location plot (top locations with most SUCOs)
  output$location_plotly <- plotly::renderPlotly({
    data <- filtered_data()
    # Always group by location for top locations
    top_locations <- data %>%
      group_by(location) %>%
      summarize(visits = n(), .groups = "drop") %>%
      filter(!is.na(location)) %>%  # Remove NA locations
      arrange(desc(visits)) %>%
      head(15)
    if (nrow(top_locations) == 0) {
      return(plotly::ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No SUCO data available with the selected filters", size = 6) + theme_void()))
    }
    p <- ggplot(top_locations, aes(x = reorder(location, visits), y = visits, text = location)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = visits), hjust = 1.3, color = "black") +  # Move number further right
      coord_flip() +
      labs(title = "Top SUCO Locations", x = "Location", y = "Number of Visits") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16), axis.title = element_text(face = "bold"))
    p <- plotly::ggplotly(p, tooltip = c("x", "y", "text"), source = "location_plotly")
    # Register the click event
    plotly::event_register(p, 'plotly_click')
    p
  })
  
  # React to plotly click and update map and tab
  observe({
    click <- plotly::event_data("plotly_click", source = "location_plotly")
    if (!is.null(click)) {
      idx <- click$pointNumber + 1  # R is 1-based
      data <- filtered_data()
      top_locations <- data %>%
        group_by(location) %>%
        summarize(visits = n(), .groups = "drop") %>%
        arrange(desc(visits)) %>%
        head(15)
      if (idx > 0 && idx <= nrow(top_locations)) {
        loc <- top_locations$location[idx]
        # Use spatial_data() for accurate coordinates (same as map)
        spatial <- spatial_data()
        # Find the first point in spatial_data with this location
        if (nrow(spatial) > 0 && loc %in% spatial$location) {
          point <- spatial[spatial$location == loc, ][1, ]
          coords <- sf::st_coordinates(point)
          lng <- coords[1]
          lat <- coords[2]
          if (!is.na(lng) && !is.na(lat)) {
            updateTabsetPanel(session, "all_tabset", selected = "AllMap")
            leafletProxy("map") %>%
              setView(lng = lng, lat = lat, zoom = 15) %>%
              addCircleMarkers(lng = lng, lat = lat, radius = 15, color = "red", fill = TRUE, fillOpacity = 0.7, layerId = "highlighted_location")
          }
        }
      }
    } else {
      leafletProxy("map") %>% clearGroup("highlighted_location")
    }
  })
  
  # ========== CURRENT DATA OUTPUTS (simplified to avoid duplication) ==========
  
  # Generate current map (uses same logic as main map but with current data)
  output$current_map <- renderLeaflet({
    # Use spatial_data_current() instead of spatial_data()
    data <- spatial_data_current()
    
    # Get marker size multiplier
    size_multiplier <- input$marker_size
    
    # Get facility and foremen lookups for display names
    facilities <- get_facility_lookup()
    
    # Set up basemap provider
    basemap <- switch(input$basemap,
                      "osm" = providers$OpenStreetMap,
                      "carto" = providers$CartoDB.Positron,
                      "terrain" = providers$Stamen.Terrain,
                      "satellite" = providers$Esri.WorldImagery,
                      providers$CartoDB.Positron)
    
    # Handle case when no data is available
    if (nrow(data) == 0) {
      return(
        leaflet() %>%
          addProviderTiles(basemap) %>%
          setView(lng = -93.2, lat = 45.0, zoom = 9) %>%
          addControl(html = "<div style='background-color: white; padding: 10px;'><h4>No SUCO locations available with the selected filters (Current Data)</h4></div>",
                     position = "topleft")
      )
    }
    
    # Create color palette based on field count or facility (SAME LOGIC AS MAIN MAP)
    if (input$group_by == "facility") {
      # Get facility colors and lookup from db_helpers
      facility_colors <- get_facility_base_colors()
      facilities <- get_facility_lookup()
      foremen_lookup <- get_foremen_lookup()  # Add foremen lookup for popups
      foremen_lookup$emp_num <- as.character(foremen_lookup$emp_num)  # Ensure string format
      
      # Create facility name mapping FIRST
      facility_names <- setNames(facilities$full_name, facilities$short_name)
      
      # Create popup text beforehand to avoid scope issues
      data <- data %>%
        mutate(
          popup_text = {
            # Get proper foreman names with robust matching
            foreman_names <- sapply(foreman, function(f) {
              if (!is.na(f) && f != "" && !is.null(f)) {
                # Ensure both foreman and emp_num are strings for comparison
                foreman_str <- trimws(as.character(f))
                
                # Find matching foreman in lookup table
                matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_str)
                
                if(length(matches) > 0) {
                  foremen_lookup$shortname[matches[1]]
                } else {
                  # Fallback: show the raw foreman ID if no match found
                  paste0("FOS #", foreman_str)
                }
              } else {
                "No FOS assigned"
              }
            })
            
            facility_names_vec <- sapply(facility, function(f) {
              fname <- facility_names[f]
              if(length(fname) > 0) fname[1] else f
            })
            
            paste0("<b>Date:</b> ", inspdate, "<br>",
                   "<b>Facility:</b> ", facility_names_vec, "<br>",
                   "<b>FOS:</b> ", foreman_names, "<br>",
                   "<b>Location:</b> ", location, "<br>",
                   "<b>Field Count:</b> ", fieldcount, "<br>",
                   "<b>Data Source:</b> Current Only")
          }
        )
      
      # Create color palette function
      pal <- colorFactor(
        palette = facility_colors,
        domain = names(facility_colors))
      
      # Create a named vector for legend labels
      legend_labels <- sapply(names(facility_colors), function(code) facility_names[code] %||% code)
      
      # Create map with facility coloring
      leaflet(data) %>%
        addProviderTiles(basemap) %>%
        fitBounds(
          lng1 = min(st_coordinates(data)[,1]),
          lat1 = min(st_coordinates(data)[,2]),
          lng2 = max(st_coordinates(data)[,1]),
          lat2 = max(st_coordinates(data)[,2])
        ) %>%
        addCircleMarkers(
          radius = ~pmin(15, (3 * size_multiplier)),
          color = "black",
          weight = 1.5,
          fillColor = ~pal(facility),
          fillOpacity = 0.8,
          popup = ~popup_text
        ) %>%
        addLegend(
          position = "bottomright",
          title = "Facility (Current Data)",
          colors = facility_colors,
          labels = legend_labels,
          opacity = 0.8
        )
    } else if (input$group_by == "foreman") {
      # Filter out records with NA foreman before processing
      data <- data %>% filter(!is.na(foreman) & foreman != "")
      
      # Check if we have any data left after filtering
      if (nrow(data) == 0) {
        return(
          leaflet() %>%
            addProviderTiles(basemap) %>%
            setView(lng = -93.2, lat = 45.0, zoom = 9) %>%
            addControl(html = "<div style='background-color: white; padding: 10px;'><h4>No SUCO locations with valid FOS data available (Current Data)</h4></div>",
                       position = "topleft")
        )
      }
      
      # Get both colors and lookup exactly as documented
      foreman_colors <- get_foreman_colors()
      foremen_lookup <- get_foremen_lookup()
      
      # Create mapping from foreman NUMBER to facility-based colors (same logic as plot)
      foremen_in_data <- unique(na.omit(data$foreman))
      emp_colors <- character(0)
      
      for (foreman_num in foremen_in_data) {
        foreman_num_str <- trimws(as.character(foreman_num))
        
        # Find the shortname for this foreman number
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
        
        if(length(matches) > 0) {
          shortname <- foremen_lookup$shortname[matches[1]]
          facility <- foremen_lookup$facility[matches[1]]
          
          # Get the facility-based color for this shortname
          if(shortname %in% names(foreman_colors)) {
            emp_colors[foreman_num_str] <- foreman_colors[shortname]
          }
        }
      }
      
      # Remove any NA colors
      emp_colors <- emp_colors[!is.na(emp_colors)]
      
      # Create ORDERED colors to ensure legend and map match exactly
      # Order by facility, then by foreman within facility (same as legend)
      ordered_foremen <- foremen_lookup[order(foremen_lookup$facility, foremen_lookup$shortname), ]
      ordered_emp_colors <- character(0)
      ordered_emp_numbers <- character(0)
      
      for (i in 1:nrow(ordered_foremen)) {
        emp_num <- trimws(as.character(ordered_foremen$emp_num[i]))
        if (emp_num %in% names(emp_colors)) {
          ordered_emp_colors <- c(ordered_emp_colors, emp_colors[emp_num])
          ordered_emp_numbers <- c(ordered_emp_numbers, emp_num)
        }
      }
      names(ordered_emp_colors) <- ordered_emp_numbers
      
      # Create color palette function for leaflet using ORDERED colors
      pal <- colorFactor(
        palette = ordered_emp_colors,
        domain = names(ordered_emp_colors),
        ordered = TRUE  # Maintain order
      )
      
      # Create popup text beforehand for foreman map too
      data <- data %>%
        mutate(
          popup_text_foreman = {
            # Get proper foreman names with robust matching
            foreman_names <- sapply(foreman, function(f) {
              if (!is.na(f) && f != "" && !is.null(f)) {
                # Ensure both foreman and emp_num are strings for comparison
                foreman_str <- trimws(as.character(f))
                
                # Find matching foreman in lookup table
                matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_str)
                
                if(length(matches) > 0) {
                  foremen_lookup$shortname[matches[1]]
                } else {
                  # Fallback: show the raw foreman ID if no match found
                  paste0("FOS #", foreman_str)
                }
              } else {
                "No FOS assigned"
              }
            })
            
            facility_names_vec <- sapply(facility, function(f) {
              fname <- facilities$full_name[facilities$short_name == f]
              if(length(fname) > 0) fname[1] else f
            })
            
            paste0("<b>Date:</b> ", inspdate, "<br>",
                   "<b>Facility:</b> ", facility_names_vec, "<br>",
                   "<b>FOS:</b> ", foreman_names, "<br>",
                   "<b>Location:</b> ", location, "<br>",
                   "<b>Field Count:</b> ", fieldcount, "<br>",
                   "<b>Data Source:</b> Current Only")
          }
        )
      
      # Create map with foreman coloring
      leaflet(data) %>%
        addProviderTiles(basemap) %>%
        fitBounds(
          lng1 = min(st_coordinates(data)[,1]),
          lat1 = min(st_coordinates(data)[,2]),
          lng2 = max(st_coordinates(data)[,1]),
          lat2 = max(st_coordinates(data)[,2])
        ) %>%
        addCircleMarkers(
          radius = ~pmin(15, (3 * size_multiplier)),
          color = "black",
          weight = 1.5,
          fillColor = ~pal(foreman),
          fillOpacity = 0.8,
          popup = ~popup_text_foreman
        ) %>%
        addLegend(
          position = "bottomright",
          title = "FOS (Current Data)",
          colors = ordered_emp_colors,  # Use same ordered colors as map palette
          labels = {
            # Use same ordering as the map palette
            ordered_labels <- character(0)
            for (i in 1:length(ordered_emp_numbers)) {
              emp_num <- ordered_emp_numbers[i]
              foreman_info <- foremen_lookup[trimws(as.character(foremen_lookup$emp_num)) == emp_num, ]
              if(nrow(foreman_info) > 0) {
                ordered_labels <- c(ordered_labels, foreman_info$shortname[1])
              }
            }
            ordered_labels
          },
          opacity = 0.8
        )
    } else {
      # For MMCD (All) case, use a single color
      leaflet(data) %>%
        addProviderTiles(basemap) %>%
        fitBounds(
          lng1 = min(st_coordinates(data)[,1]),
          lat1 = min(st_coordinates(data)[,2]),
          lng2 = max(st_coordinates(data)[,1]),
          lat2 = max(st_coordinates(data)[,2])
        ) %>%
        addCircleMarkers(
          radius = ~pmin(15, (3 * size_multiplier)),
          color = "black",
          weight = 1.5,
          fillColor = "#1f77b4", # Standard blue color
          fillOpacity = 0.8,
          popup = ~paste0("<b>Date:</b> ", inspdate, "<br>",
                          "<b>Facility:</b> ", facility, "<br>",
                          "<b>Foreman:</b> ", foreman, "<br>",
                          "<b>Location:</b> ", location, "<br>",
                          "<b>Field Count:</b> ", fieldcount, "<br>",
                          "<b>Data Source:</b> Current Only")
        ) %>%
        addLegend(
          position = "bottomright",
          title = "MMCD (Current Data)",
          colors = "#1f77b4",
          labels = "All",
          opacity = 0.8
        )
    }
  })
  
  # Generate current summary table
  output$current_summary_table <- renderDataTable({
    # Use filtered_data_current() instead of filtered_data()
    data <- filtered_data_current()
    group_col <- input$group_by
    
    if (group_col == "mmcd_all") {
      summary_data <- data %>%
        summarize(
          Total_SUCOs = n(),
          Total_Locations = n_distinct(sitecode),
          Total_Species_Count = sum(cnt, na.rm = TRUE),
          First_SUCO = min(inspdate),
          Last_SUCO = max(inspdate)
        )
      rownames(summary_data) <- "MMCD (All) - Current Data"
    } else if (group_col == "facility") {
      facilities <- get_facility_lookup()
      all_facilities <- data.frame(
        facility = facilities$short_name,
        full_name = facilities$full_name
      )
      
      summary_stats <- data %>%
        group_by(facility) %>%
        summarize(
          Total_SUCOs = n(),
          Total_Locations = n_distinct(sitecode),
          Total_Species_Count = sum(cnt, na.rm = TRUE),
          First_SUCO = min(inspdate),
          Last_SUCO = max(inspdate)
        )
      
      summary_data <- all_facilities %>%
        left_join(summary_stats, by = "facility") %>%
        mutate(
          Total_SUCOs = replace_na(Total_SUCOs, 0),
          Total_Locations = replace_na(Total_Locations, 0),
          Total_Species_Count = replace_na(Total_Species_Count, 0),
          First_SUCO = as.Date(First_SUCO),
          Last_SUCO = as.Date(Last_SUCO)
        ) %>%
        arrange(desc(Total_SUCOs), facility)
      
      colnames(summary_data)[1:2] <- c("Facility", "Facility_Name")
      
    } else {  # foreman grouping
      foremen_lookup <- get_foremen_lookup()
      
      all_foremen <- data.frame(
        foreman = foremen_lookup$emp_num,
        shortname = foremen_lookup$shortname,
        facility = foremen_lookup$facility
      )
      
      summary_stats <- data %>%
        group_by(foreman) %>%
        summarize(
          Total_SUCOs = n(),
          Total_Locations = n_distinct(sitecode),
          Total_Species_Count = sum(cnt, na.rm = TRUE),
          First_SUCO = min(inspdate),
          Last_SUCO = max(inspdate)
        )
      
      summary_data <- all_foremen %>%
        left_join(summary_stats, by = c("foreman")) %>%
        mutate(
          Total_SUCOs = replace_na(Total_SUCOs, 0),
          Total_Locations = replace_na(Total_Locations, 0),
          Total_Species_Count = replace_na(Total_Species_Count, 0),
          First_SUCO = as.Date(First_SUCO),
          Last_SUCO = as.Date(Last_SUCO)
        ) %>%
        arrange(desc(Total_SUCOs), facility, shortname)
      
      colnames(summary_data)[1:3] <- c("Foreman", "Name", "Facility")
    }
    return(summary_data)
  }, options = list(pageLength = 15, 
                   searching = TRUE,
                   columnDefs = list(list(
                     targets = c("First_SUCO", "Last_SUCO"),
                     render = JS("function(data, type, row) {
                       return data ? data : 'N/A';
                     }")
                   ))))
  
  # Generate current location plot
  output$current_location_plotly <- plotly::renderPlotly({
    # Use filtered_data_current() instead of filtered_data()
    data <- filtered_data_current()
    top_locations <- data %>%
      group_by(location) %>%
      summarize(visits = n(), .groups = "drop") %>%
      filter(!is.na(location)) %>%
      arrange(desc(visits)) %>%
      head(15)
    if (nrow(top_locations) == 0) {
      return(plotly::ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No SUCO data available with the selected filters (Current Data)", size = 6) + theme_void()))
    }
    p <- ggplot(top_locations, aes(x = reorder(location, visits), y = visits, text = location)) +
      geom_bar(stat = "identity", fill = "#1f77b4") +  # Blue to match current data theme
      geom_text(aes(label = visits), hjust = 1.3, color = "black") +
      coord_flip() +
      labs(title = "Top SUCO Locations (Current Data Only)", x = "Location", y = "Number of Visits") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16), axis.title = element_text(face = "bold"))
    p <- plotly::ggplotly(p, tooltip = c("x", "y", "text"), source = "current_location_plotly")
    plotly::event_register(p, 'plotly_click')
    p
  })
  
  # Handle current location plot clicks
  observe({
    click <- plotly::event_data("plotly_click", source = "current_location_plotly")
    if (!is.null(click)) {
      idx <- click$pointNumber + 1
      data <- filtered_data_current()
      top_locations <- data %>%
        group_by(location) %>%
        summarize(visits = n(), .groups = "drop") %>%
        arrange(desc(visits)) %>%
        head(15)
      if (idx > 0 && idx <= nrow(top_locations)) {
        loc <- top_locations$location[idx]
        spatial <- spatial_data_current()
        if (nrow(spatial) > 0 && loc %in% spatial$location) {
          point <- spatial[spatial$location == loc, ][1, ]
          coords <- sf::st_coordinates(point)
          lng <- coords[1]
          lat <- coords[2]
          if (!is.na(lng) && !is.na(lat)) {
            updateTabsetPanel(session, "current_tabset", selected = "CurrentMap")
            leafletProxy("current_map") %>%
              setView(lng = lng, lat = lat, zoom = 15) %>%
              addCircleMarkers(lng = lng, lat = lat, radius = 15, color = "red", fill = TRUE, fillOpacity = 0.7, layerId = "highlighted_location_current")
          }
        }
      }
    } else {
      leafletProxy("current_map") %>% clearGroup("highlighted_location_current")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)