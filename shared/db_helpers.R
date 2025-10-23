# Shared Database Helper Functions and Common Utilities
# This file contains common database queries and utility functions used across multiple apps

# Load required libraries (if not already loaded)
if (!require(DBI, quietly = TRUE)) library(DBI)
if (!require(RPostgres, quietly = TRUE)) library(RPostgres)
if (!require(dplyr, quietly = TRUE)) library(dplyr)

# Load environment variables helper
load_env_variables <- function() {
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
  
  return(env_loaded)
}

# Database Helper Functions for MMCD Metrics Applications
# This file contains shared functions for database connections and common lookups

# Load required libraries
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
})

# Load environment variables function
load_env_vars <- function() {
  # Load environment variables from .env file (for local development)
  # or from Docker environment variables (for production)
  env_paths <- c(
    "../../.env",           # For local development from apps/*/
    "../../../.env",        # Alternative local path
    "/srv/shiny-server/.env", # Docker path
    ".env"                  # Root directory (when called from shared/)
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
  return(env_loaded)
}

# Database connection function
get_db_connection <- function() {
  tryCatch({
    # Load environment variables
    load_env_vars()
    
    # Database configuration using environment variables
    db_host <- Sys.getenv("DB_HOST")
    db_port <- Sys.getenv("DB_PORT")
    db_user <- Sys.getenv("DB_USER")
    db_password <- Sys.getenv("DB_PASSWORD")
    db_name <- Sys.getenv("DB_NAME")
    
    # Check if all required environment variables are set
    if (any(c(db_host, db_port, db_user, db_password, db_name) == "")) {
      warning("Missing required database environment variables")
      return(NULL)
    }
    
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = db_name,
      host = db_host,
      port = as.numeric(db_port),
      user = db_user,
      password = db_password
    )
    
    return(con)
  }, error = function(e) {
    warning(paste("Database connection failed:", e$message))
    return(NULL)
  })
}

# Facility lookup functions
get_facility_lookup <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Get facility lookup from gis_facility table
    facilities <- dbGetQuery(con, "
      SELECT DISTINCT 
        abbrv as short_name,
        city as full_name
      FROM gis_facility 
      WHERE abbrv IS NOT NULL AND city IS NOT NULL
      ORDER BY abbrv
    ")
    
    dbDisconnect(con)
    return(facilities)
    
  }, error = function(e) {
    warning(paste("Error loading facility lookup:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}

# Get facility choices for selectInput widgets
get_facility_choices <- function(include_all = TRUE) {
  facilities <- get_facility_lookup()
  
  if (nrow(facilities) == 0) {
    return(c("All Facilities" = "all"))
  }
  
  choices <- setNames(facilities$short_name, facilities$full_name)
  
  if (include_all) {
    choices <- c("All Facilities" = "all", choices)
  }
  
  return(choices)
}

# Map facility short codes to full names for display
map_facility_names <- function(data, facility_col = "facility") {
  facilities <- get_facility_lookup()
  
  if (nrow(facilities) == 0 || !facility_col %in% names(data)) {
    return(data)
  }
  
  # Create a named vector for mapping
  facility_map <- setNames(facilities$full_name, facilities$short_name)
  
  # Map the facility names, keeping original if no mapping found
  data[[paste0(facility_col, "_display")]] <- ifelse(
    data[[facility_col]] %in% names(facility_map),
    facility_map[data[[facility_col]]],
    data[[facility_col]]
  )
  
  return(data)
}

# Priority lookup
get_priority_choices <- function(include_all = TRUE) {
  choices <- c("HIGH" = "RED", "MEDIUM" = "YELLOW", "LOW" = "GREEN")
  
  if (include_all) {
    choices <- c("All Priorities" = "all", choices)
  }
  
  return(choices)
}

# Material type lookup
get_material_types <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    materials <- dbGetQuery(con, "
      SELECT 
        list_type,
        matcode,
        display_text,
        order_num,
        heading,
        startdate,
        enddate,
        tdose,
        unit,
        area,
        tdosedisplay,
        mattype
      FROM lookup_matcode_entrylist 
      WHERE matcode IS NOT NULL 
      ORDER BY order_num, matcode
    ")
    
    dbDisconnect(con)
    return(materials)
    
  }, error = function(e) {
    warning(paste("Error loading material types:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}

# Get material choices for selectInput widgets
get_material_choices <- function(include_all = TRUE) {
  materials <- get_material_types()
  
  if (nrow(materials) == 0) {
    return(c("All Materials" = "all"))
  }
  
  # Use display_text if available, otherwise fall back to matcode
  labels <- ifelse(!is.na(materials$display_text) & materials$display_text != "", 
                   materials$display_text, 
                   materials$matcode)
  
  choices <- setNames(materials$matcode, labels)
  
  if (include_all) {
    choices <- c("All Materials" = "all", choices)
  }
  
  return(choices)
}

# Get foremen (field supervisors) lookup table
get_foremen_lookup <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Get active field supervisors from employee_list
    foremen <- dbGetQuery(con, "
      SELECT 
        emp_num,
        lastname,
        firstname,
        shortname,
        facility
      FROM employee_list 
      WHERE emp_type = 'FieldSuper' 
      AND date_end IS NULL 
      AND active = TRUE
      ORDER BY facility, shortname
    ")
    
    dbDisconnect(con)
    return(foremen)
    
  }, error = function(e) {
    warning(paste("Error loading foremen lookup:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}

# Get facility base colors (hardcoded for consistency)
get_facility_base_colors <- function() {
  return(c(
    "N" = "blue",
    "E" = "red", 
    "S" = "green",
    "Sr" = "green",
    "Sj" = "orange",
    "W" = "purple",
    "Wp" = "purple",
    "Wm" = "purple", 
    "W2" = "purple",
    "MO" = "brown"
  ))
}

# Generate foreman colors (shades of facility color)
get_foreman_colors <- function() {
  facility_colors <- get_facility_base_colors()
  foremen <- get_foremen_lookup()
  
  if (nrow(foremen) == 0) return(c())
  
  foreman_colors <- c()
  
  # Group foremen by facility and assign shades
  for (facility in unique(foremen$facility)) {
    facility_foremen <- foremen[foremen$facility == facility, ]
    base_color <- facility_colors[facility]
    
    if (is.na(base_color)) base_color <- "gray"
    
    # Convert base color to HSV for better control
    rgb_base <- col2rgb(base_color) / 255
    hsv_base <- rgb2hsv(rgb_base[1], rgb_base[2], rgb_base[3])
    h <- hsv_base[1]  # Hue
    s <- hsv_base[2]  # Saturation
    v <- hsv_base[3]  # Value
    
    # Create shades for each foreman in the facility
    n_foremen <- nrow(facility_foremen)
    
    # Generate distinct shades based on facility's base color
    if (n_foremen == 1) {
      colors <- base_color
    } else {
      shades <- character(n_foremen)
      
      # Create distinct variations that stay within the facility's color family
      for (i in 1:n_foremen) {
        # Calculate variations that maximize distinction while keeping color family
        new_s <- max(0.4, min(1, s + (i - n_foremen/2) * 0.15))  # Vary saturation
        new_v <- max(0.4, min(0.95, v + cos(i/n_foremen * pi) * 0.3))  # Vary brightness in wave pattern
        
        # Small hue variation to create more distinction while staying in color family
        hue_shift <- (i - n_foremen/2) * 0.05  # Small hue shifts
        new_h <- (h + hue_shift) %% 1  # Keep hue in valid range
        
        shades[i] <- hsv(new_h, new_s, new_v)
      }
      
      # Ensure middle shade is close to facility base color
      mid_point <- ceiling(n_foremen/2)
      shades[mid_point] <- base_color
      colors <- shades
    }
    
    # Map colors to foreman numbers
    foreman_colors <- c(foreman_colors, 
                       setNames(colors, as.character(facility_foremen$foreman)))
  }
  
  return(foreman_colors)
}

# Common date range options
get_date_range_choices <- function() {
  return(list(
    "Last 7 days" = 7,
    "Last 14 days" = 14,
    "Last 30 days" = 30,
    "Last 60 days" = 60,
    "Last 90 days" = 90,
    "Current year" = as.numeric(Sys.Date() - as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")))
  ))
}

# Format date for display
format_display_date <- function(date_col) {
  ifelse(is.na(date_col) | date_col == "", 
         "None", 
         tryCatch(format(as.Date(date_col), "%m/%d/%y"), 
                error = function(e) as.character(date_col)))
}

# Comprehensive status and treatment color mappings
get_status_colors <- function() {
  return(c(
    # General status colors
    "U" = "gray",
    "Unknown" = "gray",
    "Needs Inspection" = "yellow", 
    "Under Threshold" = "blue",
    "Needs Treatment" = "red",
    "Active Treatment" = "green",  # Combined with "active" - both mean active treatment
    "Completed" = "lightgreen",
    "Pending" = "orange",
    "In Progress" = "lightblue",
    
    # Treatment progress specific colors
    "total" = "gray80",
    
    # Priority colors
    "RED" = "red",
    "YELLOW" = "yellow", 
    "GREEN" = "lightgreen",
    "HIGH" = "red",
    "MEDIUM" = "yellow",
    "LOW" = "blue",
    
    # Special status colors
    "PREHATCH" = "green"
  ))
}

# Get color descriptions for different status types
get_status_descriptions <- function() {
  return(c(
    # General status descriptions
    "U" = "Unknown status",
    "Unknown" = "Unknown status",
    "Needs Inspection" = "Requires inspection", 
    "Under Threshold" = "Below treatment threshold",
    "Needs Treatment" = "Requires treatment",
    "Active Treatment" = "Currently being treated",
    "Completed" = "Treatment completed",
    "Pending" = "Treatment pending",
    "In Progress" = "Work in progress",
    
    # Treatment progress descriptions
    "total" = "Total sites/acres available",
    
    # Priority descriptions
    "RED" = "High priority",
    "YELLOW" = "Medium priority", 
    "GREEN" = "Low priority",
    "HIGH" = "High priority",
    "MEDIUM" = "Medium priority",
    "LOW" = "Low priority",
    
    # Special status descriptions
    "PREHATCH" = "Prehatch site status"
  ))
}