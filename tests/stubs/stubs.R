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

# Get the directory where this script lives
stub_dir <- dirname(sys.frame(1)$ofile)
if (is.null(stub_dir) || stub_dir == "") {
  # Fallback for sourcing from different contexts
  stub_dir <- "tests/stubs"
}

# Source all stub files
source(file.path(stub_dir, "stub_loc_cxstruct.R"))
source(file.path(stub_dir, "stub_treatments.R"))
source(file.path(stub_dir, "stub_breeding_sites.R"))
source(file.path(stub_dir, "stub_catchbasin.R"))
source(file.path(stub_dir, "stub_employees.R"))

message("Stub data loaded. Use get_stub_*() functions to access test data.")
