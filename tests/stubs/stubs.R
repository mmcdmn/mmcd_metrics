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
# Use a more robust approach that works from different calling contexts
get_stub_directory <- function() {
  # Multiple fallback strategies for finding the stub directory
  possible_dirs <- c(
    # If called from project root
    file.path(getwd(), "tests/stubs"),
    # If called from tests/ directory  
    file.path(getwd(), "stubs"),
    # If called from tests/apps/ or tests/shared/
    file.path(getwd(), "../stubs"),
    file.path(getwd(), "../../tests/stubs"),
    # Absolute path based on this file's location
    tryCatch({
      script_path <- sys.frame(1)$ofile
      if (!is.null(script_path) && script_path != "") {
        file.path(dirname(script_path), ".")
      } else {
        NULL
      }
    }, error = function(e) NULL),
    # Last resort - try to find any tests directory
    file.path(find.package("base"), "..", "..", "..", "tests/stubs")
  )
  
  for (dir in possible_dirs) {
    if (!is.null(dir) && dir != "" && file.exists(file.path(dir, "stub_treatments.R"))) {
      return(normalizePath(dir, mustWork = FALSE))
    }
  }
  
  # Ultimate fallback - return a reasonable default
  return("tests/stubs")
}

stub_dir <- get_stub_directory()

# Source all stub files with better error reporting
cat("Loading stubs from directory:", stub_dir, "\n")

stub_files <- c("stub_loc_cxstruct.R", "stub_treatments.R", "stub_breeding_sites.R", 
                "stub_catchbasin.R", "stub_employees.R", "stub_suco_data.R")

for (stub_file in stub_files) {
  stub_path <- file.path(stub_dir, stub_file)
  if (file.exists(stub_path)) {
    tryCatch({
      source(stub_path)
      cat("✓ Loaded", stub_file, "\n")
    }, error = function(e) {
      cat("✗ Failed to load", stub_file, ":", e$message, "\n")
    })
  } else {
    cat("✗ File not found:", stub_path, "\n")
  }
}

message("Stub data loaded. Use get_stub_*() functions to access test data.")
