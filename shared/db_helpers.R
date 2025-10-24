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
      WHERE abbrv IS NOT NULL 
        AND city IS NOT NULL
        AND abbrv NOT IN ('AW', 'MF', 'OT', 'OW', 'RW')
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



# Get facility base colors - each facility gets a unique, maximally distinct color
get_facility_base_colors <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(c())
  
  tryCatch({
    facilities <- dbGetQuery(con, "
      SELECT DISTINCT 
        abbrv,
        city
      FROM gis_facility 
      WHERE abbrv IS NOT NULL 
        AND abbrv NOT IN ('AW', 'MF', 'OT', 'OW', 'RW')
      ORDER BY abbrv
    ")
    
    dbDisconnect(con)
    
    if (nrow(facilities) == 0) return(c())
    
    # Maximally distinct colors - completely unique for each facility
    # These colors have been chosen to be as distinct as possible 
    # while maintaining good visibility
    base_colors <- c(
      "#FF0000",  # Pure Red
      "#0000FF",  # Pure Blue
      "#00CC00",  # Bright Green
      "#FF6600",  # Orange
      "#9933CC",  # Purple
      "#FF3399",  # Pink
      "#00CCFF",  # Sky Blue
      "#FFCC00",  # Gold
      "#33CC33",  # Lime Green
      "#FF99CC",  # Light Pink
      "#6666FF",  # Cornflower Blue
      "#FF9900"   # Dark Orange
    )
    
    n_facilities <- nrow(facilities)
    if (n_facilities > length(base_colors)) {
      warning("More facilities than predefined colors - generating additional colors")
      # Generate additional maximally distinct colors
      h_values <- seq(0, 0.9, length.out = n_facilities)  # Spread hues evenly
      s_values <- rep(0.8, n_facilities)  # Keep saturation constant
      v_values <- rep(0.9, n_facilities)  # Keep value constant
      
      base_colors <- sapply(h_values, function(h) {
        hsv(h = h, s = 0.8, v = 0.9)
      })
    }
    
    # Create named vector mapping facility abbreviations to colors
    result <- setNames(base_colors[1:n_facilities], facilities$abbrv)
    return(result)
  }, error = function(e) {
    warning(paste("Error loading facility colors:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(c())
  })
}# Generate foreman colors dynamically based on facility colors
get_foreman_colors <- function() {
  # Get current facilities and their colors
  facility_colors <- get_facility_base_colors()
  
  # Get current active foremen
  foremen <- get_foremen_lookup()
  if (nrow(foremen) == 0) return(c())
  
  foreman_colors <- c()
  
  # Process each facility's foremen
  for (facility in unique(foremen$facility)) {
    # Get foremen for this facility
    facility_foremen <- foremen[foremen$facility == facility, ]
    base_color <- facility_colors[facility]
    
    if (is.na(base_color)) {
      warning(paste("No base color found for facility:", facility))
      next
    }
    
    # Convert base color to HSV for better shade generation
    rgb_base <- col2rgb(base_color) / 255
    hsv_base <- rgb2hsv(rgb_base[1], rgb_base[2], rgb_base[3])
    
    n_foremen <- nrow(facility_foremen)
    
    if (n_foremen == 1) {
      # Single foreman gets the facility base color
      foreman_colors[as.character(facility_foremen$foreman)] <- base_color
    } else {
      # For multiple foremen, generate distinct shades
      # Use golden ratio to spread colors evenly
      golden_ratio <- 0.618033988749895
      
      # Generate shades for each foreman
      shades <- character(n_foremen)
      for (i in 1:n_foremen) {
        # Use golden ratio to generate well-distributed values
        sat_position <- (i * golden_ratio) %% 1
        val_position <- ((i * golden_ratio * golden_ratio) %% 1)
        
        # Calculate new saturation and value
        # Keep saturation between 0.5 and 1.0 for visibility
        new_sat <- 0.5 + (sat_position * 0.5)
        # Keep value between 0.4 and 0.9 for visibility
        new_val <- 0.4 + (val_position * 0.5)
        
        # Create color
        shades[i] <- hsv(h = hsv_base[1],     # Keep facility hue
                        s = new_sat, 
                        v = new_val)
      }
      
      # Assign the base facility color to the middle foreman
      mid_point <- ceiling(n_foremen/2)
      shades[mid_point] <- base_color
      
      # Map colors to foremen
      foreman_colors <- c(foreman_colors,
                         setNames(shades,
                                as.character(facility_foremen$foreman)))
    }
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

# Core status colors - single source of truth for all status indicators
get_status_colors <- function() {
  return(c(
    # Core status colors - no duplicates or aliases
    "active" = "#00CC00",      # Bright green for active/in-progress/treatment
    "completed" = "#4169E1",   # Royal blue for completed
    "planned" = "#FFA500",     # Orange for planned/pending
    "needs_action" = "#FF4500", # Red-orange for needs inspection/treatment
    "unknown" = "#A9A9A9"      # Dark gray for unknown status
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