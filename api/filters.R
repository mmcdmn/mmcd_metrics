# Filter API Server
# Provides JSON endpoints for facility and FOS filter options
# This is a standalone Plumber API that serves filter data

library(plumber)
library(jsonlite)
library(dplyr)

# Load shared database helpers
source("../shared/db_helpers.R")
source("../shared/app_libraries.R")

# Load environment variables
load_env_vars()

# Pre-cache the lookup tables
get_facility_lookup()
get_foremen_lookup()

# =============================================================================
# API ENDPOINTS
# =============================================================================

#' @get /facilities
#' @json
function() {
  tryCatch({
    facility_choices <- get_facility_choices(include_all = FALSE)
    
    # Convert to list of objects
    facilities <- lapply(names(facility_choices), function(name) {
      list(
        label = name,
        code = unname(facility_choices[[name]])
      )
    })
    
    # Add "All" option at the beginning
    facilities <- c(
      list(list(label = "All Facilities", code = "all")),
      facilities
    )
    
    return(facilities)
  }, error = function(e) {
    list(error = paste("Error loading facilities:", e$message))
  })
}

#' @get /foremen
#' @json
function() {
  tryCatch({
    foreman_choices <- get_foreman_choices(include_all = FALSE)
    
    # Convert to list of objects
    foremen <- lapply(names(foreman_choices), function(name) {
      list(
        label = name,
        code = unname(foreman_choices[[name]])
      )
    })
    
    # Add "All" option at the beginning
    foremen <- c(
      list(list(label = "All FOS Areas", code = "all")),
      foremen
    )
    
    return(foremen)
  }, error = function(e) {
    list(error = paste("Error loading foremen:", e$message))
  })
}

#' @get /health
#' @json
function() {
  list(status = "ok", timestamp = Sys.time())
}
