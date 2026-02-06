# Test Stub Data Index
# 
# This file loads all stub data files for use in testing.
# Stubs are based on REAL database schema from mmcd_data PostgreSQL database.
#
# Usage in tests:
#   source("tests/stubs/stubs.R")
#   my_data <- get_stub_loc_cxstruct()
#
# Available stub functions:
#
# From stub_loc_cxstruct.R:
#   - get_stub_loc_cxstruct()          # Structure locations
#
# From stub_treatments.R:
#   - get_stub_dblarv_insptrt_current() # Full treatment data
#   - get_stub_treatments_minimal()     # Minimal treatment data
#
# From stub_breeding_sites.R:
#   - get_stub_loc_breeding_sites()     # Breeding site locations
#   - get_stub_locpt_air_site_dip_spots() # Dip spot locations
#   - get_stub_drone_sites()            # Drone-designated sites
#   - get_stub_standard_sites()         # Standard sites for cross-app testing
#   - get_stub_standard_treatments()    # Standard treatments for cross-app testing
#   - get_stub_raw_data_for_app(app)    # Get raw data structure for specific app
#   - get_stub_expected_facility_n_sites() # Expected sitecodes for facility N
#   - get_stub_expected_zone1_sites()   # Expected sitecodes for zone 1
#
# From stub_catchbasin.R:
#   - get_stub_catchbasin_status()      # CB status view
#   - get_stub_catchbasin_status_varied() # CB status with varied states
#   - get_stub_loc_catchbasin()         # CB locations
#   - get_stub_cbtreatment_status()     # CB treatment status
#
# From stub_employees.R:
#   - get_stub_employee_list()          # Full employee list
#   - get_stub_field_supervisors()      # Active field supervisors
#   - get_stub_gis_sectcode()           # Section code geography
#   - get_stub_facilities()             # Facility code lookup
#
# Get the directory where this script lives
# First try to get the path from sys.frame (works when sourced directly)
stub_dir <- tryCatch({
  script_path <- sys.frame(1)$ofile
  if (!is.null(script_path) && script_path != "") {
    dirname(normalizePath(script_path, mustWork = FALSE))
  } else {
    NULL
  }
}, error = function(e) {
  NULL
})

if (is.null(stub_dir) || stub_dir == "" || stub_dir == ".") {
  # Fallback for sourcing from different contexts - check multiple possible paths
  possible_paths <- c(
    "tests/stubs",           # From project root
    "../stubs",              # From tests/apps or tests/shared
    "stubs",                 # From tests/
    ".",                     # Already in stubs directory
    "../../tests/stubs"      # From apps/*/
  )
  
  for (path in possible_paths) {
    if (file.exists(file.path(path, "stub_treatments.R"))) {
      stub_dir <- normalizePath(path, mustWork = FALSE)
      break
    }
  }
  
  # Final fallback - use absolute path from working directory
  if (is.null(stub_dir) || stub_dir == "" || stub_dir == ".") {
    # Try to find project root and build path
    if (file.exists("tests/stubs/stub_treatments.R")) {
      stub_dir <- normalizePath("tests/stubs", mustWork = FALSE)
    } else if (file.exists("../stubs/stub_treatments.R")) {
      stub_dir <- normalizePath("../stubs", mustWork = FALSE)
    } else {
      stub_dir <- "tests/stubs"
    }
  }
}

# Source all stub files
tryCatch({
  source(file.path(stub_dir, "stub_loc_cxstruct.R"))
}, error = function(e) {
  warning("Failed to load stub_loc_cxstruct.R from ", stub_dir, ": ", e$message)
})

tryCatch({
  source(file.path(stub_dir, "stub_treatments.R"))
}, error = function(e) {
  warning("Failed to load stub_treatments.R: ", e$message)
})

tryCatch({
  source(file.path(stub_dir, "stub_breeding_sites.R"))
}, error = function(e) {
  warning("Failed to load stub_breeding_sites.R: ", e$message)
})

tryCatch({
  source(file.path(stub_dir, "stub_catchbasin.R"))
}, error = function(e) {
  warning("Failed to load stub_catchbasin.R: ", e$message)
})

tryCatch({
  source(file.path(stub_dir, "stub_employees.R"))
}, error = function(e) {
  warning("Failed to load stub_employees.R: ", e$message)
})

tryCatch({
  source(file.path(stub_dir, "stub_suco_data.R"))
}, error = function(e) {
  warning("Failed to load stub_suco_data.R: ", e$message)
})

message("Stub data loaded. Use get_stub_*() functions to access test data.")
