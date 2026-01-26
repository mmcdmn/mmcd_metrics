# =============================================================================
# TEST STUBS AND MOCKS
# =============================================================================
# Provides stub implementations for database-dependent functions
# so tests can run in isolation without a database connection.
#
# DATA SOURCE: These stubs use REAL sample data from the MMCD database
# Last updated: 2026-01-22
#
# Usage: Source this file AFTER sourcing the real shared modules.
#        The stubs will override the database-dependent functions.
# =============================================================================

# Check if we're in isolated testing mode
if (exists("TESTING_MODE_ISOLATED") && TESTING_MODE_ISOLATED) {
  
  cat(" Loading test stubs for isolated testing...\n")
  
  # ===========================================================================
  # CONNECTION STUBS
  # ===========================================================================
  
  # Stub: get_pool - returns NULL in isolated mode
  get_pool <- function() {
    return(NULL)
  }
  
  # Stub: get_db_connection - returns NULL in isolated mode
  get_db_connection <- function() {
    return(NULL)
  }
  
  # Stub: safe_disconnect - no-op in isolated mode
  safe_disconnect <- function(conn) {
    invisible(NULL)
  }
  
  # ===========================================================================
  # LOOKUP FUNCTION STUBS - REAL DATA FROM MMCD DATABASE
  # ===========================================================================
  
  # Stub: get_facility_lookup
  # Real data from: SELECT DISTINCT abbrv, city FROM public.gis_facility
  get_facility_lookup <- function() {
    data.frame(
      short_name = c("E", "MO", "N", "Sj", "Sr", "Wm", "Wp"),
      full_name = c("East", "Main Office", "North", "South Jordan", 
                    "South Rosemount", "West Maple Grove", "West Plymouth"),
      stringsAsFactors = FALSE
    )
  }
  
  # Stub: get_facility_choices
  get_facility_choices <- function(include_all = TRUE) {
    choices <- c(
      "East" = "E", 
      "Main Office" = "MO", 
      "North" = "N", 
      "South Jordan" = "Sj", 
      "South Rosemount" = "Sr", 
      "West Maple Grove" = "Wm", 
      "West Plymouth" = "Wp"
    )
    if (include_all) {
      choices <- c("All Facilities" = "all", choices)
    }
    return(choices)
  }
  
  # Stub: get_foremen_lookup  
  # Real data from: SELECT emp_num, shortname, facility FROM employee_list WHERE emp_type = 'FieldSuper'
  get_foremen_lookup <- function() {
    data.frame(
      emp_num = c("8202", "8206", "8203", "8208", "8205", "8207", "8209", "2127", "0205"),
      shortname = c("Andrew M.", "Gabi G.", "John K.", "Kathy B.", "S. Grant", 
                    "Shawn M.", "Vanessa", "Colette", "Abbey"),
      facility = c("E", "E", "E", "E", "E", "E", "E", "MO", "N"),
      stringsAsFactors = FALSE
    )
  }
  
  # Stub: get_foreman_choices
  get_foreman_choices <- function(include_all = TRUE) {
    choices <- c(
      "Andrew M. (E)" = "Andrew M.",
      "Gabi G. (E)" = "Gabi G.",
      "John K. (E)" = "John K.",
      "Kathy B. (E)" = "Kathy B.",
      "S. Grant (E)" = "S. Grant",
      "Shawn M. (E)" = "Shawn M.",
      "Vanessa (E)" = "Vanessa",
      "Colette (MO)" = "Colette",
      "Abbey (N)" = "Abbey"
    )
    if (include_all) {
      choices <- c("All FOS" = "all", choices)
    }
    return(choices)
  }
  
  # Stub: get_priority_choices (from db_helpers - already hardcoded, but stub for consistency)
  get_priority_choices <- function(include_all = TRUE) {
    choices <- c(
      "RED" = "RED", 
      "YELLOW" = "YELLOW", 
      "BLUE" = "BLUE", 
      "GREEN" = "GREEN", 
      "PURPLE" = "PURPLE"
    )
    if (include_all) {
      choices <- c("All Priorities" = "all", choices)
    }
    return(choices)
  }
  
  # Stub: get_species_choices (from color_themes - common species)
  get_species_choices <- function(include_all = TRUE) {
    choices <- c(
      "Aedes vexans" = "vex",
      "Culex tarsalis" = "tar", 
      "Culex pipiens/restuans" = "pip/res",
      "Aedes dorsalis" = "dor",
      "Ochlerotatus trivittatus" = "tri"
    )
    if (include_all) {
      choices <- c("All Species" = "all", choices)
    }
    return(choices)
  }
  
  # Stub: get_virus_target_choices (from db_helpers)
  get_virus_target_choices <- function(include_all = FALSE) {
    choices <- c(
      "West Nile Virus" = "WNV",
      "La Crosse Encephalitis" = "LAC",
      "Jamestown Canyon" = "JC",
      "Eastern Equine Encephalitis" = "EEE",
      "Western Equine Encephalitis" = "WEE"
    )
    if (include_all) {
      choices <- c("All Targets" = "all", choices)
    }
    return(choices)
  }
  
  # Stub: is_valid_filter (from db_helpers)
  # Checks if a filter has valid selections (not NULL, not empty, not just "all")
  is_valid_filter <- function(filter_values, case_sensitive = FALSE) {
    if (is.null(filter_values)) return(FALSE)
    if (length(filter_values) == 0) return(FALSE)
    
    # Check for "all" option
    check_values <- if (case_sensitive) filter_values else tolower(filter_values)
    if (length(check_values) == 1 && check_values[1] == "all") return(FALSE)
    
    return(TRUE)
  }
  
  cat("✓ Test stubs loaded - database functions mocked with real sample data\n")
  
} else {
  cat("ℹ Test stubs not loaded - using real database connections\n")
}
