# =============================================================================
# DATABASE CONNECTION STUBS
# =============================================================================
# Provides stub/mock implementations for database connection functions
# so tests don't need real database access.

# Mock get_pool function that doesn't require database credentials
get_pool <- function() {
  stop("Cannot create connection pool: Database environment variables not available")
}

# Mock get_db_connection function
get_db_connection <- function() {
  stop("Cannot create database connection: Database environment variables not available")
}

# Mock database result functions
mock_db_result <- function(...) {
  data.frame(
    code = character(0),
    name = character(0),
    stringsAsFactors = FALSE
  )
}

# Mock leaflet functions for testing
if (!requireNamespace("leaflet", quietly = TRUE)) {
  # Create mock leaflet objects
  leaflet <- function(...) {
    structure(list(type = "mock_leaflet", calls = list()), class = "mock_leaflet")
  }
  
  addTiles <- function(map, ...) {
    map$calls <- c(map$calls, "addTiles")
    map
  }
  
  addProviderTiles <- function(map, provider, ...) {
    map$calls <- c(map$calls, paste("addProviderTiles", provider))
    map
  }
  
  addCircleMarkers <- function(map, ...) {
    map$calls <- c(map$calls, "addCircleMarkers")
    map
  }
  
  setView <- function(map, ...) {
    map$calls <- c(map$calls, "setView")
    map
  }
  
  fitBounds <- function(map, ...) {
    map$calls <- c(map$calls, "fitBounds")
    map
  }
  
  # Mock providers object
  providers <- list(
    OpenStreetMap = "OpenStreetMap",
    CartoDB.Positron = "CartoDB.Positron",
    Esri.WorldImagery = "Esri.WorldImagery"
  )
  
  # Make these available globally
  assign("leaflet", leaflet, envir = .GlobalEnv)
  assign("addTiles", addTiles, envir = .GlobalEnv)
  assign("addProviderTiles", addProviderTiles, envir = .GlobalEnv)
  assign("addCircleMarkers", addCircleMarkers, envir = .GlobalEnv)
  assign("setView", setView, envir = .GlobalEnv)
  assign("fitBounds", fitBounds, envir = .GlobalEnv)
  assign("providers", providers, envir = .GlobalEnv)
  
  cat("Mock leaflet functions created\n")
}

# Mock lookup functions
get_foremen_lookup <- function() {
  data.frame(
    foreman = c("8201", "8202", "8203", "8207", "0204", "7002"),
    name = c("John Doe", "Jane Smith", "Bob Johnson", "Alice Brown", "Mike Wilson", "Sarah Davis"),
    stringsAsFactors = FALSE
  )
}

get_facility_lookup <- function() {
  data.frame(
    facility = c("E", "N", "W", "Wm", "Sj"),
    name = c("East", "North", "West", "West plymouth", "South Jordan"),
    stringsAsFactors = FALSE
  )
}

# Mock display name functions that use the lookups
get_foreman_display_names <- function(codes) {
  if (length(codes) == 0) return(character(0))
  
  lookup <- get_foremen_lookup()
  result <- character(length(codes))
  
  for (i in seq_along(codes)) {
    if (is.na(codes[i])) {
      result[i] <- "Unknown"
    } else {
      match_row <- lookup[lookup$foreman == codes[i], ]
      if (nrow(match_row) > 0) {
        result[i] <- match_row$name[1]
      } else {
        result[i] <- codes[i]  # Return original if not found
      }
    }
  }
  
  result
}

get_facility_display_names <- function(codes) {
  if (length(codes) == 0) return(character(0))
  
  lookup <- get_facility_lookup()
  result <- character(length(codes))
  
  for (i in seq_along(codes)) {
    if (is.na(codes[i])) {
      result[i] <- "Unknown"
    } else {
      match_row <- lookup[lookup$facility == codes[i], ]
      if (nrow(match_row) > 0) {
        result[i] <- match_row$name[1]
      } else {
        result[i] <- codes[i]  # Return original if not found
      }
    }
  }
  
  result
}

message("Database stubs loaded - database functions will fail gracefully in tests")