# =============================================================================
# MMCD METRICS - SHARED HELPER FUNCTIONS
# =============================================================================
# Central utility library for all MMCD dashboard applications.
# Source this file: source("../../shared/db_helpers.R")
#
# TABLE OF CONTENTS (Line numbers approximate):
# -----------------------------------------------------------------------------
# 1. INITIALIZATION & DEPENDENCIES.............. Lines ~10-90
#    - Library loading, color themes, connection pool
#
# 2. DATABASE CONNECTION......................... Lines ~90-190
#    - get_db_connection(), safe_disconnect(), load_env_vars()
#
# 3. SQL FILTER HELPERS.......................... Lines ~190-270
#    - is_valid_filter(), build_sql_in_clause(), etc.
#
# 4. LOOKUP TABLE FUNCTIONS...................... Lines ~270-600
#    - get_facility_lookup(), get_foreman_choices(), get_species_choices()
#
# 5. COLOR FUNCTIONS............................. Lines ~600-1150
#    - get_facility_base_colors(), get_status_colors(), get_foreman_colors()
#
# 6. SPECIES DATA FUNCTIONS...................... Lines ~1150-1200
#    - get_mosquito_species_colors() (test-app only)
#
# 7. TREATMENT PLAN FUNCTIONS.................... Lines ~1200-1380
#    - get_treatment_plan_types(), get_treatment_plan_colors()
#
# 8. CSV EXPORT HELPERS.......................... Lines ~1380-1460
#    - export_csv_safe()
#
# 9. UI/CSS HELPERS.............................. Lines ~1460-1600
#    - get_universal_text_css()
#
# 10. HISTORICAL DATA HELPERS.................... Lines ~1600-1700
#    - apply_historical_group_labels(), summarize_historical_data()
# =============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
})

# Source color themes - try multiple paths to find color_themes.R
# Source color themes directly (will attempt from multiple paths)
color_themes_paths <- c(
  "color_themes.R",
  "../../shared/color_themes.R",
  "../shared/color_themes.R",
  "../../../shared/color_themes.R",
  "shared/color_themes.R"
)

color_themes_loaded <- FALSE
for (ct_path in color_themes_paths) {
  if (file.exists(ct_path)) {
    tryCatch({
      source(ct_path, local = FALSE)
      color_themes_loaded <- TRUE
      break
    }, error = function(e) {
      # Continue to next path
    })
  }
}

if (!color_themes_loaded) {
  message("Note: color_themes.R not found. Using default MMCD colors only.")
}

# =============================================================================
# LOAD CONNECTION POOL MODULE
# =============================================================================
# Source the connection pooling module for improved performance
# This provides get_pool() which maintains persistent database connections
tryCatch({
  # Try to source from same directory
  source_paths <- c(
    file.path(dirname(sys.frame(1)$ofile %||% ""), "db_pool.R"),
    "shared/db_pool.R",
    "../../shared/db_pool.R",
    "../../../shared/db_pool.R",
    "db_pool.R",
    "../shared/db_pool.R"
  )
  
  pool_loaded <- FALSE
  for (path in source_paths) {
    if (!is.null(path) && file.exists(path)) {
      source(path, local = FALSE)
      pool_loaded <- TRUE
      break
    }
  }
  
  if (!pool_loaded) {
    message("Note: db_pool.R not found. Connection pooling not available.")
    message("      Using traditional connections (create/disconnect per query)")
  }
}, error = function(e) {
  message("Note: Could not load db_pool.R - ", e$message)
  message("      Using traditional connections")
})

# Set default color theme to MMCD if not already set
#########################################################
## HERE IS WHERE WE SET THE COLOR THEME
##########################################################
# This ensures all apps use MMCD theme unless explicitly changed
if (is.null(getOption("mmcd.color.theme"))) {
  options(mmcd.color.theme = "MMCD")
}

# Define NULL coalescing operator if not already defined (from color_themes.R)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

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
  
  # Environment variables might already be set (Docker)
  required_vars <- c("DB_HOST", "DB_PORT", "DB_NAME", 
                     "DB_USER", "DB_PASSWORD")
  
  if (!env_loaded && !all(sapply(required_vars, function(var) Sys.getenv(var) != ""))) {
    warning("Could not load environment variables from .env file and required variables are not set")
    return(FALSE)
  }
  
  return(TRUE)
}

# =============================================================================
# DATABASE CONNECTION FUNCTIONS
# =============================================================================

# Centralized database connection function
# NOW WITH CONNECTION POOLING SUPPORT!
# 
# If db_pool.R is loaded, this function will use the connection pool
# for better performance. Otherwise, falls back to traditional connections.
# 
# Usage:
#   conn <- get_db_connection()
#   data <- dbGetQuery(conn, "SELECT ...")
#   dbDisconnect(conn)  # Optional with pooling (no-op), required without
#
# For best performance, update your code to use get_pool() directly:
#   conn <- get_pool()
#   data <- dbGetQuery(conn, "SELECT ...")
#   # No disconnect needed!
get_db_connection <- function() {
  # Check if connection pool is available (from db_pool.R)
  if (exists("get_pool", mode = "function")) {
    # Use connection pool - much faster!
    return(get_pool())
  }
  
  # Fallback to traditional connection if pool not available
  if (!load_env_vars()) {
    warning("Failed to load environment variables")
    return(NULL)
  }
  
  tryCatch({
    con <- dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("DB_HOST"),
      port = as.numeric(Sys.getenv("DB_PORT")),
      dbname = Sys.getenv("DB_NAME"),
      user = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASSWORD")
    )
    return(con)
  }, error = function(e) {
    warning(paste("Database connection failed:", e$message))
    return(NULL)
  })
}

# Safe disconnect function - only disconnects traditional connections, not pools
safe_disconnect <- function(con) {
  if (is.null(con)) return(invisible(NULL))
  
  # Check if this is a pool object - if so, don't disconnect
  if (inherits(con, "Pool")) {
    return(invisible(NULL))
  }
  
  # For traditional connections, disconnect normally
  tryCatch({
    dbDisconnect(con)
  }, error = function(e) {
    # Silently ignore disconnect errors
    invisible(NULL)
  })
}

# =============================================================================
# SQL FILTER BUILDING HELPERS
# =============================================================================
# Standardized functions for building SQL WHERE clauses from filter inputs.
# These replace the repetitive pattern of checking for NULL/"all" and building
# IN clauses that was duplicated 30+ times across apps.

#' Check if a filter should be applied (not null, not empty, not "all")
#' @param filter_values Vector of filter values from user input
#' @param case_sensitive If FALSE (default), checks for "all" case-insensitively
#' @return TRUE if filter should be applied, FALSE otherwise
is_valid_filter <- function(filter_values, case_sensitive = FALSE) {
  if (is.null(filter_values) || length(filter_values) == 0) {
    return(FALSE)
  }
  
  # Check for "all" value (case-insensitive by default)
  all_check <- if (case_sensitive) {
    "all" %in% filter_values || "All" %in% filter_values
  } else {
    "all" %in% tolower(filter_values)
  }
  
  return(!all_check)
}

#' Build a SQL IN clause from a vector of values
#' @param values Vector of values to include in the IN clause
#' @return String like "'val1', 'val2', 'val3'" ready for SQL IN ()
build_sql_in_list <- function(values) {
  if (is.null(values) || length(values) == 0) {
    return("")
  }
  paste0("'", values, "'", collapse = ", ")
}

#' Build a complete SQL WHERE clause for a column IN filter
#' @param column_name The SQL column name (e.g., "facility", "gis.facility")
#' @param filter_values Vector of filter values
#' @param prefix What to prepend (default "AND ") - use "" for first clause
#' @return String like "AND facility IN ('E', 'W')" or "" if filter not valid
build_sql_in_clause <- function(column_name, filter_values, prefix = "AND ") {
  if (!is_valid_filter(filter_values)) {
    return("")
  }
  
  in_list <- build_sql_in_list(filter_values)
  paste0(prefix, column_name, " IN (", in_list, ")")
}

#' Build SQL WHERE clause for a single value (equals)
#' @param column_name The SQL column name
#' @param value Single value to match
#' @param prefix What to prepend (default "AND ")
#' @return String like "AND zone = '1'" or "" if value is null
build_sql_equals_clause <- function(column_name, value, prefix = "AND ") {
  if (is.null(value) || length(value) == 0 || (is.character(value) && value == "")) {
    return("")
  }
  paste0(prefix, column_name, " = '", value, "'")
}

#' Extract filter value from input, returning NULL if "all" is selected
#' Useful for reactive filter processing
#' @param input_value The raw input value (potentially containing "all")
#' @return The filter values, or NULL if "all" was selected or empty
normalize_filter_input <- function(input_value) {
  if (is.null(input_value) || length(input_value) == 0) {
    return(NULL)
  }
  if ("all" %in% tolower(input_value)) {
    return(NULL)
  }
  return(input_value)
}

# Get year ranges from current and archive tables for historical data
# This handles March transitions when data moves between tables
get_historical_year_ranges <- function(con, current_table, archive_table, date_column = "inspdate") {
  if (is.null(con)) {
    return(list(current_years = c(), archive_years = c()))
  }
  
  tryCatch({
    # Get year ranges from each table to determine data availability
    current_year_query <- sprintf("SELECT MIN(EXTRACT(YEAR FROM %s)) as min_year, MAX(EXTRACT(YEAR FROM %s)) as max_year FROM %s WHERE %s IS NOT NULL", 
                                   date_column, date_column, current_table, date_column)
    archive_year_query <- sprintf("SELECT MIN(EXTRACT(YEAR FROM %s)) as min_year, MAX(EXTRACT(YEAR FROM %s)) as max_year FROM %s WHERE %s IS NOT NULL", 
                                   date_column, date_column, archive_table, date_column)
    
    current_year_range <- dbGetQuery(con, current_year_query)
    archive_year_range <- dbGetQuery(con, archive_year_query)
    
    # Determine which years to get from which table based on actual data availability
    current_years <- c()
    archive_years <- c()
    
    if (!is.na(current_year_range$min_year) && !is.na(current_year_range$max_year)) {
      current_years <- seq(current_year_range$min_year, current_year_range$max_year, 1)
    }
    
    if (!is.na(archive_year_range$min_year) && !is.na(archive_year_range$max_year)) {
      archive_years <- seq(archive_year_range$min_year, archive_year_range$max_year, 1)
    }
    
    return(list(current_years = current_years, archive_years = archive_years))
    
  }, error = function(e) {
    warning(paste("Error getting historical year ranges:", e$message))
    return(list(current_years = c(), archive_years = c()))
  })
}

# Facility lookup functions
get_facility_lookup <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Get facility lookup from gis_facility table, excluding special facilities
    facilities <- dbGetQuery(con, "
      SELECT DISTINCT 
        abbrv as short_name,
        city as full_name
      FROM public.gis_facility
      WHERE abbrv NOT IN ('OT', 'MF', 'AW', 'RW', 'OW')
      ORDER BY abbrv
    ")
    
    safe_disconnect(con)
    return(facilities)
    
  }, error = function(e) {
    warning(paste("Error loading facility lookup:", e$message))
    safe_disconnect(con)
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

# Get foreman (FOS) choices for selectInput widgets
get_foreman_choices <- function(include_all = TRUE) {
  foremen <- get_foremen_lookup()
  
  if (nrow(foremen) == 0) {
    return(c("All FOS" = "all"))
  }
  
  # Create choices with shortname as value and "shortname (facility)" as display
  display_names <- paste0(foremen$shortname, " (", foremen$facility, ")")
  choices <- setNames(foremen$shortname, display_names)
  
  if (include_all) {
    choices <- c("All FOS" = "all", choices)
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
  choices <- c("RED" = "RED", "YELLOW" = "YELLOW", "BLUE" = "BLUE", "GREEN" = "GREEN", "PURPLE" = "PURPLE")
  
  if (include_all) {
    choices <- c("All Priorities" = "all", choices)
  }
  
  return(choices)
}

# Get spring date thresholds from ACT4-P1 lookup table
get_spring_date_thresholds <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Get ACT4-P1 thresholds, excluding January 1st start dates
    qry <- "
    SELECT EXTRACT(year FROM date_start) as year, date_start
    FROM public.lookup_threshold_larv
    WHERE description = 'ACT4-P1'
      AND EXTRACT(month FROM date_start) != 1
      AND EXTRACT(day FROM date_start) != 1
    ORDER BY year, date_start
    "
    
    result <- dbGetQuery(con, qry)
    safe_disconnect(con)
    
    if (nrow(result) > 0) {
      result$date_start <- as.Date(result$date_start)
    }
    
    return(result)
    
  }, error = function(e) {
    warning(paste("Error in get_spring_date_thresholds:", e$message))
    safe_disconnect(con)
    return(data.frame())
  })
}

# Structure type lookup from database
get_structure_type_choices <- function(include_all = TRUE) {
  con <- get_db_connection()
  if (is.null(con)) {
    # Fallback to hardcoded values if database connection fails
    choices <- c("AP" = "AP", "CB" = "CB", "CG" = "CG", "CV" = "CV", "DR" = "DR", 
                 "PC" = "PC", "Pool" = "Pool", "PR" = "PR", "RG" = "RG", "RR" = "RR", 
                 "SP" = "SP", "SS" = "SS", "US" = "US", "W" = "W", "WO" = "WO", "XX" = "XX")
  } else {
    tryCatch({
      # Get structure types from lookup table
      structure_types <- dbGetQuery(con, "
        SELECT DISTINCT 
          code,
          definition
        FROM public.lookup_cx_stype
        ORDER BY code ASC
      ")
      
      safe_disconnect(con)
      
      if (nrow(structure_types) > 0) {
        # Create named vector with full names as labels and codes as values
        choices <- setNames(structure_types$code, 
                           paste0(structure_types$definition, " (", structure_types$code, ")"))
      } else {
        # Fallback if no data returned
        choices <- c("AP" = "AP", "CB" = "CB", "CG" = "CG", "CV" = "CV", "DR" = "DR", 
                     "PC" = "PC", "Pool" = "Pool", "PR" = "PR", "RG" = "RG", "RR" = "RR", 
                     "SP" = "SP", "SS" = "SS", "US" = "US", "W" = "W", "WO" = "WO", "XX" = "XX")
      }
      
    }, error = function(e) {
      warning(paste("Error loading structure types:", e$message))
      safe_disconnect(con)
      # Fallback to hardcoded values
      choices <- c("AP" = "AP", "CB" = "CB", "CG" = "CG", "CV" = "CV", "DR" = "DR", 
                   "PC" = "PC", "Pool" = "Pool", "PR" = "PR", "RG" = "RG", "RR" = "RR", 
                   "SP" = "SP", "SS" = "SS", "US" = "US", "W" = "W", "WO" = "WO", "XX" = "XX")
    })
  }
  
  if (include_all) {
    choices <- c("All Types" = "all", choices)
  }
  
  return(choices)
}

# Get material choices with optional filtering for prehatch or BTI materials
# filter_type: NULL (all materials), "prehatch" (prehatch=TRUE only), "bti" (BTI/Bs materials only)
get_material_choices <- function(include_all = TRUE, filter_type = NULL) {
  con <- get_db_connection()
  if (is.null(con)) {
    if (is.null(filter_type)) {
      return(c("All Materials" = "all"))
    } else {
      return(character(0))
    }
  }
  
  tryCatch({
    # Build WHERE clause based on filter type
    where_clause <- "WHERE mattype IS NOT NULL"
    
    if (!is.null(filter_type)) {
      if (filter_type == "prehatch") {
        where_clause <- paste(where_clause, "AND prehatch = TRUE")
      } else if (filter_type == "bti") {
        where_clause <- paste(where_clause, "AND (mattype ILIKE '%bti%' OR mattype ILIKE '%bs%')")
      }
    }
    
    # Get matcode and enhanced material names from mattype_list_targetdose
    # Return matcode as value (for filtering) and enhanced name as label (for display)
    query <- sprintf("
      SELECT DISTINCT
        matcode,
        CASE 
          WHEN tdose IS NOT NULL AND unit IS NOT NULL AND area IS NOT NULL
          THEN CONCAT(mattype, ' ', tdose, ' ', unit, ' per ', area)
          ELSE mattype
        END AS enhanced_name
      FROM mattype_list_targetdose
      %s
      ORDER BY enhanced_name
    ", where_clause)
    
    materials <- dbGetQuery(con, query)
    
    safe_disconnect(con)
    
    if (nrow(materials) == 0) {
      if (is.null(filter_type) && include_all) {
        return(c("All Materials" = "all"))
      } else {
        return(character(0))
      }
    }
    
    # Create choices with enhanced names as labels and matcode as values
    choices <- setNames(materials$matcode, materials$enhanced_name)
    
    if (include_all && is.null(filter_type)) {
      choices <- c("All Materials" = "all", choices)
    }
    
    return(choices)
    
  }, error = function(e) {
    warning(paste("Error loading material choices:", e$message))
    safe_disconnect(con)
    if (is.null(filter_type) && include_all) {
      return(c("All Materials" = "all"))
    } else {
      return(character(0))
    }
  })
}

# Alias function for consistency with some apps
get_treatment_material_choices <- function(include_all = TRUE, filter_type = NULL) {
  return(get_material_choices(include_all = include_all, filter_type = filter_type))
}

# Get foremen (field supervisors) lookup table
get_foremen_lookup <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
        # Get active field supervisors basic info
    foremen <- dbGetQuery(con, "
      SELECT 
        emp_num,
        shortname,
        facility
      FROM employee_list 
      WHERE emp_type = 'FieldSuper'
        AND active = true 
        AND facility IS NOT NULL
      ORDER BY facility, shortname
    ")
    
    safe_disconnect(con)
    return(foremen)
    
  }, error = function(e) {
    warning(paste("Error loading foremen lookup:", e$message))
    safe_disconnect(con)
    return(data.frame())
  })
}

# =============================================================================
# VIRUS TARGET CONFIGURATION
# =============================================================================

# Get virus target choices for surveillance applications
# These are the standardized virus targets used across MMCD mosquito testing
get_virus_target_choices <- function(include_all = FALSE) {
  choices <- c(
    "West Nile Virus" = "WNV",
    "La Crosse Encephalitis" = "LAC",
    "Eastern Equine Encephalitis" = "EEE"
  )
  
  if (include_all) {
    choices <- c("All Viruses" = "all", choices)
  }
  
  return(choices)
}

# Get virus target display names


# =============================================================================
# SPECIES LOOKUP FUNCTIONS
# =============================================================================

# Get species lookup table
# Get available zones with P1/P2 options
get_available_zones <- function(include_all = TRUE, include_combined = TRUE) {
  # Return clean zone options as specified
  result <- c(
    "P1",
    "P2", 
    "P1 + P2 Combined",
    "P1 and P2 Separate"
  )
  
  return(result)
}

get_species_lookup <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Get species lookup from lookup_specieslist table
    species_lookup <- dbGetQuery(con, "
      SELECT 
        sppcode,
        genus,
        species
      FROM lookup_specieslist 
      ORDER BY genus, species
    ")
    
    safe_disconnect(con)
    return(species_lookup)
    
  }, error = function(e) {
    warning(paste("Error loading species lookup:", e$message))
    safe_disconnect(con)
    return(data.frame())
  })
}

#' Enhanced Species Name Mapping for SUCO Applications
#' 
#' This function provides enhanced species name mapping with flexible formatting options
#' specifically designed for SUCO surveillance applications. It builds on the existing
#' get_species_lookup() and get_species_code_map() functions to provide consistent
#' species name formatting across all visualizations.
#' 
#' @param format_style Character. Options: "full" (Genus species), "abbreviated" (Ae. albopictus), 
#'   "code_only" (spp code), "display" (smart formatting for UI)
#' @param include_code Logical. If TRUE, includes species code in parentheses
#' 
#' Usage:
#' ```r
#' # Get full scientific names
#' full_names <- get_enhanced_species_mapping(format_style = "full")
#' # "52" = "Aedes albopictus"
#' 
#' # Get abbreviated names for space-constrained displays
#' abbrev_names <- get_enhanced_species_mapping(format_style = "abbreviated")
#' # "52" = "Ae. albopictus"
#' 
#' # Get display names with codes for clarity
#' display_names <- get_enhanced_species_mapping(format_style = "display", include_code = TRUE)
#' # "52" = "Aedes albopictus (52)"
#' ```
#' 
#' Returns:
#'   Named vector where names are species codes and values are formatted species names
get_enhanced_species_mapping <- function(format_style = "display", include_code = FALSE) {
  species_lookup <- get_species_lookup()
  
  if (nrow(species_lookup) == 0) {
    return(character(0))
  }
  
  # Create mapping
  species_map <- character(0)
  
  for (i in 1:nrow(species_lookup)) {
    code <- as.character(species_lookup$sppcode[i])
    genus <- species_lookup$genus[i]
    species <- species_lookup$species[i]
    
    # Format based on style
    formatted_name <- switch(format_style,
      "full" = {
        if (!is.na(genus) && !is.na(species) && genus != "" && species != "") {
          paste(genus, species)
        } else if (!is.na(genus) && genus != "") {
          genus
        } else if (!is.na(species) && species != "") {
          species
        } else {
          paste0("Species ", code)
        }
      },
      "abbreviated" = {
        if (!is.na(genus) && !is.na(species) && genus != "" && species != "") {
          paste0(substr(genus, 1, 2), ". ", species)
        } else if (!is.na(genus) && genus != "") {
          genus
        } else if (!is.na(species) && species != "") {
          species
        } else {
          paste0("Species ", code)
        }
      },
      "code_only" = {
        code
      },
      "display" = {
        # Smart formatting: use full name for clarity
        if (!is.na(genus) && !is.na(species) && genus != "" && species != "") {
          paste(genus, species)
        } else if (!is.na(genus) && genus != "") {
          genus
        } else if (!is.na(species) && species != "") {
          species
        } else {
          paste0("Species ", code)
        }
      },
      # Default to display format
      {
        if (!is.na(genus) && !is.na(species) && genus != "" && species != "") {
          paste(genus, species)
        } else if (!is.na(genus) && genus != "") {
          genus
        } else if (!is.na(species) && species != "") {
          species
        } else {
          paste0("Species ", code)
        }
      }
    )
    
    # Add code if requested
    if (include_code && format_style != "code_only") {
      formatted_name <- paste0(formatted_name, " (", code, ")")
    }
    
    species_map[code] <- formatted_name
  }
  
  return(species_map)
}

#' Get Species Choices for Shiny Select Input
#' 
#' Returns a named vector of species codes suitable for use in Shiny selectInput/selectizeInput.
#' The names are formatted display names (e.g., "Aedes albopictus (52)") and values are species codes.
#' 
#' @param include_all Logical. If TRUE, includes "All" as the first option.
#' 
#' @return Named character vector where names are display names and values are species codes
#' 
#' @examples
#' choices <- get_species_choices()
#' # Returns: c("Aedes albopictus (52)" = "52", "Culex pipiens (5)" = "5", ...)
#' 
#' choices_with_all <- get_species_choices(include_all = TRUE)
#' # Returns: c("All Species" = "all", "Aedes albopictus (52)" = "52", ...)
get_species_choices <- function(include_all = TRUE) {
  # Get species mapping with display format and codes
  species_map <- get_enhanced_species_mapping(format_style = "display", include_code = TRUE)
  
  if (length(species_map) == 0) {
    warning("No species data available from database")
    return(if (include_all) c("All Species" = "all") else character(0))
  }
  
  # species_map is returned as: names=codes, values=display_names
  # We need to flip it for Shiny: names=display_names, values=codes
  choices <- setNames(names(species_map), species_map)
  
  # Sort alphabetically by display name
  choices <- choices[order(names(choices))]
  
  # Add "All" option if requested
  if (include_all) {
    choices <- c("All Species" = "all", choices)
  }
  
  return(choices)
}

# Internal helper function to generate visually distinct colors
# This function creates a set of unique colors that are visually distinct from each other
# Now supports theme-based color generation
# Parameters:
#   n: Number of colors to generate
#   theme: Color theme to use (default: getOption("mmcd.color.theme", "MMCD"))
# Returns: Vector of hex color codes
generate_distinct_colors_internal <- function(n, theme = getOption("mmcd.color.theme", "MMCD")) {
  if (n <= 0) return(character(0))
  
  # Try to use theme-based colors if color_themes.R is loaded
  if (exists("generate_distinct_colors", mode = "function", inherits = TRUE)) {
    tryCatch({
      return(generate_distinct_colors(n, theme))
    }, error = function(e) {
      # Fall through to default generation
    })
  }
  
  # Fallback to HSV generation if theme system not available
  hues <- seq(0, 1, length.out = n + 1)[1:n]
  colors <- sapply(hues, function(h) {
    hsv(h = h, s = 0.8, v = 0.9)
  })
  
  return(colors)
}

#' Get Consistent Facility Colors
#' 
#' This function generates and returns a consistent color mapping for facilities.
#' Each facility gets assigned a unique color that remains consistent across all visualizations.
#' 
#' @param alpha_zones Optional. Vector of zone identifiers (e.g., c("1", "2")) to apply 
#'   alpha transparency for zone differentiation. P1 zones get full opacity (1.0), 
#'   P2 zones get reduced opacity (0.6). If NULL, returns standard colors.
#' @param combined_groups Optional. Vector of combined group names (e.g., "AP (P1)", "NM (P2)")
#'   to extract base facility names from for color mapping. Used with alpha_zones.
#' @param theme Character. Color theme to use (default: getOption("mmcd.color.theme", "MMCD"))
#' 
#' Usage:
#' ```r
#' # Standard facility colors:
#' facility_colors <- get_facility_base_colors()
#' ggplot(data, aes(x = x, y = y, color = facility)) +
#'   scale_color_manual(values = facility_colors)
#'
#' # Zone-aware colors with alpha:
#' zone_colors <- get_facility_base_colors(
#'   alpha_zones = c("1", "2"), 
#'   combined_groups = unique(data$combined_group)
#' )
#' ggplot(data, aes(x = x, y = y, color = combined_group, alpha = zone_factor)) +
#'   scale_color_manual(values = zone_colors$colors) +
#'   scale_alpha_manual(values = zone_colors$alpha_values)
#' ```
#' 
#' Returns:
#'   If alpha_zones is NULL: Named vector where names are facility short names and values are hex colors.
#'   If alpha_zones provided: List with $colors (named vector) and $alpha_values (named vector for zones).
get_facility_base_colors <- function(alpha_zones = NULL, combined_groups = NULL, theme = getOption("mmcd.color.theme", "MMCD")) {
  facilities <- get_facility_lookup()
  if (nrow(facilities) == 0) return(c())
  
  # Try to use theme-specific facility colors
  if (exists("get_theme_palette", mode = "function", inherits = TRUE)) {
    tryCatch({
      palette <- get_theme_palette(theme)
      if (!is.null(palette$facilities) && all(facilities$short_name %in% names(palette$facilities))) {
        colors <- palette$facilities[facilities$short_name]
        result <- setNames(as.character(colors), facilities$short_name)
        
        # Handle zone differentiation if requested
        if (!is.null(alpha_zones) && length(alpha_zones) > 1 && !is.null(combined_groups)) {
          combined_colors <- character(0)
          for (combined_name in combined_groups) {
            base_name <- gsub("\\\\s*\\\\([^)]+\\\\)$", "", combined_name)
            base_name <- trimws(base_name)
            if (base_name %in% names(result)) {
              combined_colors[combined_name] <- result[base_name]
            }
          }
          return(list(
            colors = combined_colors,
            alpha_values = c("1" = 1.0, "2" = 0.6)
          ))
        }
        
        return(result)
      }
    }, error = function(e) {
      # Fall through to default generation
    })
  }
  
  # Fallback: Generate one color per facility
  colors <- generate_distinct_colors_internal(nrow(facilities), theme)
  
  # Map colors to facility short names
  result <- setNames(colors, facilities$short_name)
  
  # Handle zone differentiation if requested
  if (!is.null(alpha_zones) && length(alpha_zones) > 1 && !is.null(combined_groups)) {
    # Extract base names from combined groups and map to colors
    combined_colors <- character(0)
    for (combined_name in combined_groups) {
      base_name <- gsub("\\s*\\([^)]+\\)$", "", combined_name)
      base_name <- trimws(base_name)
      if (base_name %in% names(result)) {
        combined_colors[combined_name] <- result[base_name]
      }
    }
    
    # Return zone-aware result
    return(list(
      colors = combined_colors,
      alpha_values = c("1" = 1.0, "2" = 0.6)
    ))
  }
  
  return(result)
}

#' Get Consistent Foreman Colors Based on Facility
#' 
#' This function generates and returns a color mapping for foremen where each foreman's
#' color is a variation of their facility's base color. This ensures that foremen from
#' the same facility have similar but distinguishable colors.
#' 
#' @param alpha_zones Optional. Vector of zone identifiers (e.g., c("1", "2")) to apply 
#'   alpha transparency for zone differentiation. P1 zones get full opacity (1.0), 
#'   P2 zones get reduced opacity (0.6). If NULL, returns standard colors.
#' @param combined_groups Optional. Vector of combined group names (e.g., "John S. (P1)", "Jane D. (P2)")
#'   to extract base foreman names from for color mapping. Used with alpha_zones.
#' 
#' Important Notes:
#' 1. The foreman colors are based on employee numbers (e.g., "7002", "8203")
#' 2. You must use get_foremen_lookup() to map between employee numbers and names
#' 
#' Usage:
#' ```r
#' # Standard foreman colors:
#' foreman_colors <- get_foreman_colors()
#' foremen_lookup <- get_foremen_lookup()
#' emp_colors <- setNames(foreman_colors[foremen_lookup$shortname], foremen_lookup$emp_num)
#' ggplot(data, aes(x = x, y = y, color = foreman)) +
#'   scale_color_manual(values = emp_colors)
#' 
#' # Zone-aware colors:
#' zone_colors <- get_foreman_colors(
#'   alpha_zones = c("1", "2"), 
#'   combined_groups = unique(data$combined_group)
#' )
#' ggplot(data, aes(x = x, y = y, color = combined_group, alpha = zone_factor)) +
#'   scale_color_manual(values = zone_colors$colors) +
#'   scale_alpha_manual(values = zone_colors$alpha_values)
#' ```
#' 
#' Returns:
#'   If alpha_zones is NULL: Named vector where names are foreman shortnames and values are hex colors.
#'   If alpha_zones provided: List with $colors (named vector) and $alpha_values (named vector for zones).
get_foreman_colors <- function(alpha_zones = NULL, combined_groups = NULL, theme = getOption("mmcd.color.theme", "MMCD")) {
  foremen <- get_foremen_lookup()
  if (nrow(foremen) == 0) return(c())
  
  facility_colors <- get_facility_base_colors(theme = theme)
  foreman_colors <- character(nrow(foremen))
  
  # For each facility
  for (facility in unique(foremen$facility)) {
    # Get foremen for this facility
    facility_foremen <- foremen[foremen$facility == facility, ]
    n_foremen <- nrow(facility_foremen)
    
    # Get base color for facility
    base_color <- facility_colors[facility]
    if (is.na(base_color)) next
    
    # Convert base color to HSV for manipulation
    rgb_base <- col2rgb(base_color) / 255
    hsv_base <- rgb2hsv(rgb_base[1], rgb_base[2], rgb_base[3])
    
    # Generate variations of the base color using enhanced logic
    if (n_foremen == 1) {
      # Single foreman gets the facility base color
      foreman_colors[foremen$shortname == facility_foremen$shortname] <- base_color
    } else {
      # Sort foremen by shortname for consistent ordering
      facility_foremen <- facility_foremen[order(facility_foremen$shortname), ]
      base_hue <- hsv_base[1]
      
      # Use a smaller hue range for better facility color grouping
      hue_range <- 0.08  # ±8% variation around facility color
      
      # Create distinct but related colors for foremen in this facility
      for (i in seq_len(n_foremen)) {
        foreman_name <- facility_foremen$shortname[i]
        
        # Calculate hue offset based on position
        if (n_foremen == 2) {
          # For 2 foremen: one slightly lighter, one slightly darker
          hue_offset <- ifelse(i == 1, -hue_range/2, hue_range/2)
        } else {
          # For 3+ foremen: spread across the hue range
          hue_offset <- -hue_range + (2 * hue_range * (i - 1) / (n_foremen - 1))
        }
        
        # Calculate foreman hue (keep within facility color family)
        foreman_hue <- (base_hue + hue_offset) %% 1
        
        # Vary saturation and brightness more distinctly
        if (n_foremen <= 3) {
          # For small groups, use more pronounced saturation/value differences
          saturation <- 0.6 + (0.35 * (i - 1) / max(1, n_foremen - 1))  # 0.6 to 0.95
          value <- 0.65 + (0.3 * (i - 1) / max(1, n_foremen - 1))       # 0.65 to 0.95
        } else {
          # For larger groups, use smaller but still distinct differences
          saturation <- 0.65 + (0.3 * (i - 1) / max(1, n_foremen - 1))  # 0.65 to 0.95
          value <- 0.7 + (0.25 * (i - 1) / max(1, n_foremen - 1))       # 0.7 to 0.95
        }
        
        # Assign the color for this foreman
        foreman_idx <- which(foremen$shortname == foreman_name)
        foreman_colors[foreman_idx] <- hsv(h = foreman_hue, s = saturation, v = value)
      }
    }
    hsv_base <- rgb2hsv(rgb_base[1], rgb_base[2], rgb_base[3])
    
    # Generate variations of the base color
    if (n_foremen == 1) {
      # Single foreman gets the facility base color
      foreman_colors[foremen$shortname == facility_foremen$shortname] <- base_color
    } else {
      # Sort foremen by shortname for consistent ordering
      facility_foremen <- facility_foremen[order(facility_foremen$shortname), ]
      base_hue <- hsv_base[1]
      
      # Use a smaller hue range for better facility color grouping
      hue_range <- 0.08  # ±8% variation around facility color (was 15%)
      
      # Create distinct but related colors for foremen in this facility
      for (i in 1:n_foremen) {
        foreman_name <- facility_foremen$shortname[i]
        
        # Calculate hue offset based on position
        if (n_foremen == 2) {
          # For 2 foremen: one slightly lighter, one slightly darker
          hue_offset <- ifelse(i == 1, -hue_range/2, hue_range/2)
        } else {
          # For 3+ foremen: spread across the hue range
          hue_offset <- -hue_range + (2 * hue_range * (i - 1) / (n_foremen - 1))
        }
        
        # Calculate foreman hue (keep within facility color family)
        foreman_hue <- (base_hue + hue_offset) %% 1
        
        # Vary saturation and brightness more distinctly
        if (n_foremen <= 3) {
          # For small groups, use more pronounced saturation/value differences
          saturation <- 0.6 + (0.35 * (i - 1) / max(1, n_foremen - 1))  # 0.6 to 0.95
          value <- 0.65 + (0.3 * (i - 1) / max(1, n_foremen - 1))       # 0.65 to 0.95
        } else {
          # For larger groups, use smaller but still distinct differences
          saturation <- 0.65 + (0.3 * (i - 1) / max(1, n_foremen - 1))  # 0.65 to 0.95
          value <- 0.7 + (0.25 * (i - 1) / max(1, n_foremen - 1))       # 0.7 to 0.95
        }
        
        # Assign the color for this foreman
        foreman_idx <- which(foremen$shortname == foreman_name)
        foreman_colors[foreman_idx] <- hsv(h = foreman_hue, s = saturation, v = value)
      }
    }
  }
  
  # Create named vector
  names(foreman_colors) <- foremen$shortname
  
  # Handle zone differentiation if requested
  if (!is.null(alpha_zones) && length(alpha_zones) > 1 && !is.null(combined_groups)) {
    # Extract base names from combined groups and map to colors
    combined_colors <- character(0)
    for (combined_name in combined_groups) {
      base_name <- gsub("\\s*\\([^)]+\\)$", "", combined_name)
      base_name <- trimws(base_name)
      if (base_name %in% names(foreman_colors)) {
        combined_colors[combined_name] <- foreman_colors[base_name]
      }
    }
    
    # Return zone-aware result
    return(list(
      colors = combined_colors,
      alpha_values = c("1" = 1.0, "2" = 0.6)
    ))
  }
  
  return(foreman_colors)
}

# Get theme-aware foreman colors based on facility colors
# This version accepts a theme parameter and generates foreman colors as variations
# of their facility's base color from the specified theme
# Alias for backwards compatibility - get_themed_foreman_colors is the same as get_foreman_colors
# Both generate theme-aware foreman colors based on facility colors
get_themed_foreman_colors <- function(theme = getOption("mmcd.color.theme", "MMCD")) {
  # Simply call get_foreman_colors with theme support (no zone handling)
  return(get_foreman_colors(alpha_zones = NULL, combined_groups = NULL, theme = theme))
}

# Common date range options




# Core status colors - single source of truth for all status indicators
# Now supports theme-based colors
get_status_colors <- function(theme = getOption("mmcd.color.theme", "MMCD")) {
  # Default MMCD colors
  default_colors <- c(
    "active" = "#187018",      # forest green for active/in-progress/treatment
    "completed" = "#4169E1",   # Royal blue for completed
    "planned" = "#fdb73e",     # Orange for planned/pending
    "needs_action" = "#FF4500", # Red-orange for needs inspection
    "in_lab" = "#5841c0",        # Purple for lab processing
    "needs_treatment" = "#FF0000", # Pure red for needs treatment
    "unknown" = "#A9A9A9"      # Dark gray for unknown status
  )
  
  # For MMCD theme or if theme system not available, use default colors
  if (theme == "MMCD" || !exists("get_theme_palette", mode = "function", inherits = TRUE)) {
    return(default_colors)
  }
  
  # Try to use theme-specific status colors for non-MMCD themes
  tryCatch({
    palette <- get_theme_palette(theme)
    if (!is.null(palette$status)) {
      # All themes now have the required keys directly
      return(palette$status)
    }
  }, error = function(e) {
    warning(paste("Error getting theme palette:", e$message))
  })
  
  # Final fallback to default MMCD colors
  return(default_colors)
}

# Map hex colors to Shiny named colors for valueBox and dashboard elements
# This converts db_helpers hex colors to Shiny's accepted named colors
get_shiny_colors <- function() {
  return(c(
    "active" = "olive",          # #187018 → olive (closest to forest green)
    "completed" = "blue",      # #4169E1 → blue
    "planned" = "orange",      # #FFA500 → orange
    "needs_action" = "yellow", # #FF4500 → yellow (closest to red-orange)
    "needs_treatment" = "red", # #FF0000 → red
    "unknown" = "gray",         # #A9A9A9 → gray (NOTE: Shiny uses "gray" not "grey")
    "somthing_else" = "aqua"   # #00FFFF → aqua
  ))
}

# Map status names to hex colors for visualizations (maps, charts, tables)
get_status_color_map <- function(theme = getOption("mmcd.color.theme", "MMCD")) {
  status_colors <- get_status_colors(theme = theme)
  color_map <- c(
    "Unknown" = as.character(status_colors[["unknown"]]),
    "Needs Inspection" = as.character(status_colors[["planned"]]),      # Orange/yellow for needs inspection
    "Under Threshold" = as.character(status_colors[["completed"]]),
    "Inspected" = as.character(status_colors[["completed"]]),           # Reuse completed color for inspected
    "Needs ID" = as.character(status_colors[["in_lab"]]),               # Purple for needs ID (formerly In Lab)
    "In Lab" = as.character(status_colors[["in_lab"]]),                 # Keep for backwards compatibility              
    "Needs Treatment" = as.character(status_colors[["needs_treatment"]]),
    "Active Treatment" = as.character(status_colors[["active"]])
  )
  return(color_map)
}

# Get color descriptions for different status types
get_status_descriptions <- function() {
  return(c(
    # General status descriptions
    "U" = "Unknown status",
    "Unknown" = "Unknown status",
    "Needs Inspection" = "Requires inspection", 
    "Under Threshold" = "Below treatment threshold",
    "In Lab" = "Awaiting species identification",
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
    "GREEN" = "PREHATCH",
    "HIGH" = "High priority",
    "MEDIUM" = "Medium priority",
    "LOW" = "Low priority",
    
    # Special status descriptions
    "PREHATCH" = "Prehatch site status"
  ))
}

# Mosquito Species Visualization Functions
# Centralized color and shape mappings for mosquito species in surveillance data

# Get color mappings for mosquito species
get_mosquito_species_colors <- function() {
  return(list(
    "Total_Ae_+_Cq" = "#000000", Total_Ae_springs = "#008000", Total_Ae_summers = "#ffa500",
    Cq_perturbans_42 = "#800080", Total_Cx_vectors = "#FF0000", Cx_erraticus_32 = "#000000",
    Cx_pipiens_33 = "#0000FF", Cx_restuans_34 = "#008000", Cx_salinarius_35 = "#87cefa",
    Cx_tarsalis_36 = "#a52a2a", Cx_territans_37 = "#00ff7f", "Cx_restuans/pipiens_372" = "#40e0d0",
    Cx_unknown_371 = "#ffa500", An_barberi_27 = "#FFFF00", An_earlei_28 = "#ffc0cb",
    An_punctipennis_29 = "#0000FF", An_quadrimaculatus_30 = "#FF0000", An_walkeri_31 = "#ffa500",
    sp311an_un = "#800080", Total_Anopheles = "#87cefa", sp01_abser = "#FF0000", sp03_aurif = "#FFFF00",
    sp04_euedes = "#f08080", sp05_campest = "#adff2f", sp08_commun = "#483d8b", sp09_diant = "#00FFFF",
    sp118abpun = "#800080", sp11_excru = "#ffa500", sp12_fitch = "#a52a2a", sp13_flave = "#800000",
    sp14_imple = "#7fff00", sp15_intrud = "#ffd700", sp17_pioni = "#FF00FF", sp18_punct = "#0000FF",
    sp19_ripar = "#008000", sp20_spenc = "#ff1493", sp22_stimu = "#708090", sp23_provo = "#ff6347",
    Ae_cinereus_7 = "#006400", Ae_triseriatus_24 = "#0000FF", Ae_vexans_26 = "#FF0000",
    sp02_atrop = "#ff1493", Ae_canadensis_6 = "#000000", Ae_dorsalis_10 = "#808080", sp16_nigro = "#ffd700",
    sp21_stict = "#FF00FF", sp25_trivi = "#800080", sp261ae_unid = "#000000", sp262spr_unid = "#008000",
    sp264summ_unid = "#ffa500", sp50_hende = "#7fff00", Ae_albopictus_51 = "#FF0000",
    Ae_japonicus_52 = "#008000", Ps_ciliata_44 = "#a52a2a", Ps_columbiae_45 = "#008000",
    Ps_ferox_46 = "#000000", sp471ps_un = "#808080", Ps_horrida_47 = "#FF0000", sp38_inorn = "#0000FF",
    Total_Psorophora = "#00FFFF", Culiseta_melanura = "#FF0000", sp40_minne = "#ffa500", sp41_morsi = "#a52a2a",
    sp411cs_un = "#808080", Or_signifera_43 = "#87cefa", Ur_sapphirina_48 = "#00008b", sp49_smith = "#0000FF"
  ))
}

# Get shape mappings for mosquito species (ggplot shape numbers)

# Get species color mapping for display names to color keys
# Maps human-readable species names to the color key names used in get_mosquito_species_colors()
get_species_display_colors <- function() {
  # Get all available species colors
  all_species_colors <- get_mosquito_species_colors()
  
  # Create mapping from display names to color keys
  species_color_mapping <- c(
    "Aedes triseriatus" = "Ae_triseriatus_24",
    "Aedes japonicus" = "Ae_japonicus_52", 
    "Culex tarsalis" = "Cx_tarsalis_36",
    "Culiseta melanura" = "Culiseta_melanura",
    "Aedes albopictus" = "Ae_albopictus_51",
    "Aedes cinereus" = "Ae_cinereus_7",
    "Aedes canadensis" = "Ae_canadensis_6",
    "Aedes dorsalis" = "Ae_dorsalis_10", 
    "Aedes vexans" = "Ae_vexans_26",
    "No species" = "#999999",      # Gray for no species
    "Multiple species" = "#800080"  # Purple for multiple species at same location
  )
  
  # Extract colors for the mapped species
  display_colors <- sapply(names(species_color_mapping), function(display_name) {
    color_key <- species_color_mapping[display_name]
    if (color_key %in% names(all_species_colors)) {
      return(all_species_colors[[color_key]])
    } else if (startsWith(color_key, "#")) {
      return(color_key)  # Direct hex color
    } else {
      return("#666666")  # Default gray
    }
  })
  names(display_colors) <- names(species_color_mapping)
  
  return(display_colors)
}


# Treatment Plan Type Colors
# Dynamic function to get treatment plan types and assign consistent colors

#' Get Treatment Plan Type Lookup
#' 
#' This function dynamically fetches the available treatment plan types from the database
#' and returns them with their full names for display purposes.
#' 
#' Returns:
#'   Data frame with columns: plan_code, plan_name, description
get_treatment_plan_types <- function() {
  con <- get_db_connection()
  if (is.null(con)) {
    # Return default mapping if database is unavailable
    return(data.frame(
      plan_code = c("A", "D", "G", "N", "U"),
      plan_name = c("Air", "Drone", "Ground", "None", "Unknown"),
      description = c("Air treatment", "Drone treatment", "Ground treatment", "No treatment planned", "Unknown treatment type"),
      stringsAsFactors = FALSE
    ))
  }
  
  tryCatch({
    # Get distinct treatment plan types from the current treatments table
    plan_types <- dbGetQuery(con, "
      SELECT DISTINCT 
        airgrnd_plan as plan_code,
        CASE 
          WHEN airgrnd_plan = 'A' THEN 'Air'
          WHEN airgrnd_plan = 'D' THEN 'Drone' 
          WHEN airgrnd_plan = 'G' THEN 'Ground'
          WHEN airgrnd_plan = 'N' THEN 'None'
          WHEN airgrnd_plan = 'U' THEN 'Unknown'
          ELSE airgrnd_plan
        END as plan_name,
        CASE 
          WHEN airgrnd_plan = 'A' THEN 'Air treatment'
          WHEN airgrnd_plan = 'D' THEN 'Drone treatment' 
          WHEN airgrnd_plan = 'G' THEN 'Ground treatment'
          WHEN airgrnd_plan = 'N' THEN 'No treatment planned'
          WHEN airgrnd_plan = 'U' THEN 'Unknown treatment type'
          ELSE 'Other treatment type'
        END as description,
        CASE airgrnd_plan 
          WHEN 'A' THEN 1
          WHEN 'D' THEN 2
          WHEN 'G' THEN 3
          WHEN 'N' THEN 4
          WHEN 'U' THEN 5
          ELSE 6
        END as sort_order
      FROM public.dblarv_insptrt_current 
      WHERE airgrnd_plan IS NOT NULL
      ORDER BY sort_order
    ")
    
    safe_disconnect(con)
    return(plan_types)
    
  }, error = function(e) {
    warning(paste("Error loading treatment plan types:", e$message))
    safe_disconnect(con)
    
    # Return default mapping if query fails
    return(data.frame(
      plan_code = c("A", "D", "G", "N", "U"),
      plan_name = c("Air", "Drone", "Ground", "None", "Unknown"),
      description = c("Air treatment", "Drone treatment", "Ground treatment", "No treatment planned", "Unknown treatment type"),
      stringsAsFactors = FALSE
    ))
  })
}

#' Get Consistent Colors for Treatment Plan Types
#' 
#' This function generates and returns a consistent color mapping for treatment plan types.
#' Each plan type gets assigned a unique, visually distinct color that remains consistent 
#' across all visualizations.
#' 
#' Usage:
#' ```r
#' # Get colors for treatment plan types
#' plan_colors <- get_treatment_plan_colors()
#' 
#' # In ggplot2 using plan codes (A, D, G, N, U):
#' ggplot(data, aes(x = plan_type, y = acres, fill = airgrnd_plan)) +
#'   scale_fill_manual(values = plan_colors)
#' 
#' # In ggplot2 using plan names (Air, Drone, Ground, None, Unknown):
#' plan_name_colors <- get_treatment_plan_colors(use_names = TRUE)
#' ggplot(data, aes(x = plan_name, y = acres, fill = plan_name)) +
#'   scale_fill_manual(values = plan_name_colors)
#' ```
#' 
#' Parameters:
#'   use_names: If TRUE, returns colors mapped to plan names (Air, Drone, etc.)
#'              If FALSE (default), returns colors mapped to plan codes (A, D, etc.)
#'   theme: Color theme to use for generating additional colors (default: getOption("mmcd.color.theme", "MMCD"))
#' 
#' Returns:
#'   Named vector where names are either plan codes or plan names, and values are hex colors
get_treatment_plan_colors <- function(use_names = FALSE, theme = getOption("mmcd.color.theme", "MMCD")) {
  plan_types <- get_treatment_plan_types()
  if (nrow(plan_types) == 0) return(c())
  
  # Define specific colors for common treatment plan types for consistency
  predefined_colors <- c(
    "A" = "#E41A1C",    # Red for Air
    "D" = "#377EB8",    # Blue for Drone  
    "G" = "#4DAF4A",    # Green for Ground
    "N" = "#984EA3",    # Purple for None
    "U" = "#FF7F00"     # Orange for Unknown
  )
  
  # Start with predefined colors
  colors <- character(nrow(plan_types))
  names(colors) <- plan_types$plan_code
  
  # Assign predefined colors where available
  for (i in seq_len(nrow(plan_types))) {
    code <- plan_types$plan_code[i]
    if (code %in% names(predefined_colors)) {
      colors[code] <- predefined_colors[code]
    }
  }
  
  # For any codes not in predefined list, generate distinct colors
  missing_codes <- plan_types$plan_code[!plan_types$plan_code %in% names(predefined_colors)]
  if (length(missing_codes) > 0) {
    additional_colors <- generate_distinct_colors_internal(length(missing_codes), theme)
    names(additional_colors) <- missing_codes
    colors[missing_codes] <- additional_colors
  }
  
  # If use_names is TRUE, convert keys from codes to names
  if (use_names) {
    name_map <- setNames(plan_types$plan_name, plan_types$plan_code)
    names(colors) <- name_map[names(colors)]
  }
  
  return(colors)
}

#' Get Treatment Plan Choices for Select Inputs
#' 
#' Returns properly formatted choices for selectInput widgets with full names as labels
# =============================================================================
# CSV EXPORT HELPER FUNCTIONS
# =============================================================================

#' Clean Data for CSV Export
#' 
#' This function cleans data to prevent CSV formatting issues by handling
#' line breaks, quotes, and other problematic characters in text fields.
#' 
#' @param data Data frame to clean
#' @return Cleaned data frame safe for CSV export
#' @export
clean_data_for_csv <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }
  
  # Process each character column
  for (col in names(data)) {
    if (is.character(data[[col]])) {
      # Replace newlines, tabs, and carriage returns with spaces
      data[[col]] <- gsub("[\r\n\t]+", " ", data[[col]])
      # Replace multiple spaces with single space
      data[[col]] <- gsub(" +", " ", data[[col]])
      # Trim leading/trailing whitespace
      data[[col]] <- trimws(data[[col]])
    }
  }
  
  return(data)
}

#' Export Data to CSV with Error Handling
#' 
#' This function provides a robust way to export data to CSV with proper
#' error handling, data cleaning, and informative error messages.
#' 
#' @param data Data frame to export
#' @param file File path where CSV should be saved
#' @param clean_data Logical. If TRUE, cleans data before export using clean_data_for_csv()
#' @param row_names Logical. Include row names in CSV? (default: FALSE)
#' @param na_string String to use for NA values (default: empty string)
#' 
#' @return List with success status and message
#' @export
export_csv_safe <- function(data,
                            file,
                            clean_data = TRUE,
                            row_names = FALSE,
                            na_string = "") {
  
  tryCatch({
    # Validate inputs
    if (is.null(data)) {
      return(list(
        success = FALSE,
        message = "No data provided for export",
        rows_exported = 0
      ))
    }
    
    if (nrow(data) == 0) {
      # Create empty CSV with headers
      write.csv(data, file, row.names = row_names, na = na_string)
      return(list(
        success = TRUE,
        message = "Empty CSV file created with headers",
        rows_exported = 0
      ))
    }
    
    # Clean data if requested
    if (clean_data) {
      data <- clean_data_for_csv(data)
    }
    
    # Export to CSV
    write.csv(data, file, row.names = row_names, na = na_string)
    
    return(list(
      success = TRUE,
      message = paste("Successfully exported", nrow(data), "rows to", file),
      rows_exported = nrow(data),
      file_path = file
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("CSV export failed:", e$message),
      rows_exported = 0,
      error_details = e$message
    ))
  })
}

# =============================================================================
# UNIVERSAL CSS HELPER FOR SHINY DASHBOARDS
# =============================================================================

#' Get Universal CSS for Text Size Increase
#' 
#' Returns HTML tags with CSS styling to increase all text sizes by 4px
#' across a Shiny dashboard. This provides consistent text sizing across
#' all MMCD dashboard applications.
#' 
#' Usage:
#' ```r
#' # In your Shiny UI:
#' ui <- dashboardPage(
#'   dashboardHeader(...),
#'   dashboardSidebar(...),
#'   dashboardBody(
#'     get_universal_text_css(),  # Add this line
#'     tabItems(...)
#'   )
#' )
#' ```
#' 
#' @param base_increase Numeric. Base font size increase in pixels (default: 4)
#' @return HTML tags containing CSS styles
#' @export
get_universal_text_css <- function(base_increase = 4) {
  
  # Calculate all font sizes based on standard defaults + increase
  base_size <- 14 + base_increase      # 18px
  label_size <- 14 + base_increase     # 18px
  box_title_size <- 18 + base_increase # 22px
  header_size <- 16 + base_increase    # 20px
  table_size <- 14 + base_increase     # 18px
  table_header_size <- 14 + base_increase # 18px
  valuebox_h3 <- 18 + base_increase    # 22px
  valuebox_p <- 14 + base_increase     # 18px
  infobox_size <- 14 + base_increase   # 18px
  
  shiny::tags$head(
    shiny::tags$style(shiny::HTML(paste0("
      /* Universal MMCD Dashboard Text Size Increase - Base +", base_increase, "px */
      
      /* Increase base font size */
      body, .content-wrapper, .main-sidebar, .sidebar {
        font-size: ", base_size, "px !important;
      }
      
      /* Increase input labels and text */
      label, .control-label {
        font-size: ", label_size, "px !important;
      }
      
      /* Increase select input text */
      .selectize-input, .selectize-dropdown {
        font-size: ", label_size, "px !important;
      }
      
      /* Increase radio button and checkbox text */
      .radio label, .checkbox label {
        font-size: ", label_size, "px !important;
      }
      
      /* Increase box titles */
      .box-title {
        font-size: ", box_title_size, "px !important;
      }
      
      /* Increase button text */
      .btn {
        font-size: ", label_size, "px !important;
      }
      
      /* Increase input field text */
      input[type='text'], input[type='number'], input[type='date'], textarea, select {
        font-size: ", label_size, "px !important;
      }
      
      /* Increase tab text */
      .nav-tabs > li > a {
        font-size: ", label_size, "px !important;
      }
      
      /* Increase sidebar menu text */
      .sidebar-menu > li > a {
        font-size: ", label_size, "px !important;
      }
      
      /* Increase value box text */
      .small-box h3 {
        font-size: ", valuebox_h3, "px !important;
      }
      
      .small-box p {
        font-size: ", valuebox_p, "px !important;
      }
      
      /* Increase info box text */
      .info-box-text, .info-box-number {
        font-size: ", infobox_size, "px !important;
      }
      
      /* Increase DT table text */
      table.dataTable {
        font-size: ", table_size, "px !important;
      }
      
      table.dataTable thead th {
        font-size: ", table_header_size, "px !important;
      }
      
      /* Increase dashboard header text */
      .main-header .logo, .main-header .navbar {
        font-size: ", header_size, "px !important;
      }
      
      /* Increase help text and small text */
      .text-muted, small, .small {
        font-size: ", base_size - 2, "px !important;
      }
      
      /* Increase modal text */
      .modal-body, .modal-title {
        font-size: ", label_size, "px !important;
      }
      
      /* Reset datepicker calendar to normal size to prevent overflow */
      .datepicker, .datepicker table {
        font-size: 14px !important;
      }
      
      .datepicker td, .datepicker th {
        padding: 4px 5px !important;
        font-size: 14px !important;
      }
      
      .datepicker-dropdown {
        font-size: 14px !important;
      }
    ")))
  )
}

#' Apply historical data grouping labels
#'
#' Adds group_label column to data based on grouping type and zone separation
#' @param data Data frame with facility, foreman/fosarea, and zone columns
#' @param group_by Grouping type: "facility", "foreman", or "mmcd_all"
#' @param show_zones_separately Whether to add zone suffix to labels
#' @param foreman_col Name of the foreman column (default "foreman", some apps use "fosarea")
#' @return Data frame with group_label column added
apply_historical_group_labels <- function(data, group_by, show_zones_separately = FALSE, foreman_col = "foreman") {
  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }
  
  # Get lookup tables
  facilities <- get_facility_lookup()
  facility_map <- setNames(facilities$full_name, facilities$short_name)
  foremen_lookup <- get_foremen_lookup()
  foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
  
  # Build group labels based on grouping type
  if (group_by == "facility" && "facility" %in% names(data)) {
    data <- data %>%
      mutate(
        facility_display = ifelse(
          facility %in% names(facility_map),
          facility_map[facility],
          facility
        ),
        group_label = if (show_zones_separately && "zone" %in% names(data)) {
          paste0(facility_display, " P", zone)
        } else {
          facility_display
        }
      )
  } else if (group_by == "foreman" && foreman_col %in% names(data)) {
    foreman_values <- data[[foreman_col]]
    data <- data %>%
      mutate(
        foreman_name = ifelse(
          foreman_values %in% names(foreman_map),
          foreman_map[as.character(foreman_values)],
          paste("FOS", foreman_values)
        ),
        group_label = if (show_zones_separately && "zone" %in% names(data)) {
          paste0(foreman_name, " P", zone)
        } else {
          foreman_name
        }
      )
  } else if (group_by == "mmcd_all") {
    data <- data %>%
      mutate(
        group_label = if (show_zones_separately && "zone" %in% names(data)) {
          paste0("All MMCD P", zone)
        } else {
          "All MMCD"
        }
      )
  }
  
  return(data)
}

#' Summarize historical data by group and time period
#'
#' @param data Data frame with group_label and time_period columns
#' @param metric Display metric: "treatments", "sites", "acres", "treatment_acres", "active_count", "active_acres"
#' @return Summarized data frame with group_label, time_period, value columns
summarize_historical_data <- function(data, metric) {
  if (is.null(data) || nrow(data) == 0 || !"group_label" %in% names(data)) {
    return(data.frame())
  }
  
  grouped_data <- data %>%
    group_by(group_label, time_period)
  
  if (metric == "treatments") {
    summary_data <- grouped_data %>%
      summarize(value = n(), .groups = "drop")
  } else if (metric == "treatment_acres") {
    summary_data <- grouped_data %>%
      summarize(value = sum(acres, na.rm = TRUE), .groups = "drop")
  } else if (metric %in% c("sites", "active_count")) {
    summary_data <- grouped_data %>%
      summarize(value = n_distinct(sitecode), .groups = "drop")
  } else if (metric %in% c("site_acres", "acres", "active_acres")) {
    summary_data <- grouped_data %>%
      summarize(value = sum(acres, na.rm = TRUE), .groups = "drop")
  } else {
    summary_data <- data.frame()
  }
  
  return(summary_data)
}
