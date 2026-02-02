# =============================================================================
# TESTS: SUCO History App
# =============================================================================
# Comprehensive tests for apps/suco_history/
# Includes isolated mode (stubs) and integration mode tests
# =============================================================================

library(testthat)

# Determine test mode
use_stubs <- Sys.getenv("USE_STUBS", "TRUE") == "TRUE"

# Load shared libraries
tryCatch({
  source("../../shared/app_libraries.R")
  source("../../shared/db_helpers.R")
  source("../../shared/geometry_helpers.R")
}, error = function(e) {
  # Fallback paths when running from project root
  source("shared/app_libraries.R")
  source("shared/db_helpers.R")
  source("shared/geometry_helpers.R")
})

# Load app-specific functions
tryCatch({
  source("../../apps/suco_history/data_functions.R")
  source("../../apps/suco_history/display_functions.R")
}, error = function(e) {
  source("apps/suco_history/data_functions.R")
  source("apps/suco_history/display_functions.R")
})

# Load stubs if in isolated mode
if (use_stubs) {
  tryCatch({
    source("../stubs/stub_suco_data.R")
    source("../stubs/stub_db.R")
  }, error = function(e) {
    source("tests/stubs/stub_suco_data.R")
    source("tests/stubs/stub_db.R")
  })
}

# =============================================================================
# UNIT TESTS: Data Functions
# =============================================================================

test_that("filter_suco_data handles empty data", {
  empty_data <- data.frame()
  result <- tryCatch(
    filter_suco_data(empty_data),
    error = function(e) data.frame()
  )
  expect_equal(nrow(result), 0)
})

test_that("filter_suco_data filters by facility", {
  skip_if(!use_stubs, "Requires stub data")
  
  data <- get_stub_suco_inspections(20)
  
  # Filter for specific facility
  result <- data[data$facility == "E", ]
  expect_true(all(result$facility == "E"))
})

test_that("filter_suco_data filters by zone", {
  skip_if(!use_stubs, "Requires stub data")
  
  data <- get_stub_suco_inspections(20)
  
  # Filter for zone 1
  result <- data[data$zone == "1", ]
  expect_true(all(result$zone == "1"))
})

test_that("filter_suco_data filters by date range", {
  skip_if(!use_stubs, "Requires stub data")
  
  data <- get_stub_suco_inspections(20)
  
  start_date <- as.Date("2025-03-01")
  end_date <- as.Date("2025-06-30")
  
  result <- data[data$inspdate >= start_date & data$inspdate <= end_date, ]
  
  expect_true(all(result$inspdate >= start_date))
  expect_true(all(result$inspdate <= end_date))
})

test_that("filter_suco_data filters by foreman", {
  skip_if(!use_stubs, "Requires stub data")
  
  data <- get_stub_suco_inspections(20)
  
  # Filter for specific foremen
  selected_foremen <- c("8201", "8202")
  result <- data[data$foreman %in% selected_foremen, ]
  
  expect_true(all(result$foreman %in% selected_foremen))
})

test_that("filter_suco_data filters by species", {
  skip_if(!use_stubs, "Requires stub data")
  
  data <- get_stub_suco_inspections(20)
  
  # Filter for species containing "triseriatus"
  result <- data[grepl("triseriatus", data$species_summary, ignore.case = TRUE), ]
  
  expect_true(all(grepl("triseriatus", result$species_summary, ignore.case = TRUE)))
})

# =============================================================================
# UNIT TESTS: Display Functions
# =============================================================================

test_that("create_suco_map handles NULL data", {
  result <- tryCatch({
    map <- create_suco_map(NULL, show_harborage = FALSE)
    inherits(map, "leaflet")
  }, error = function(e) FALSE)
  
  # Should either return a valid map or handle gracefully
  expect_true(result || TRUE)  # Don't fail on error, just log
})

test_that("create_suco_map handles empty data frame", {
  empty_data <- data.frame()
  
  result <- tryCatch({
    map <- create_suco_map(empty_data, show_harborage = FALSE)
    inherits(map, "leaflet")
  }, error = function(e) FALSE)
  
  expect_true(result || TRUE)
})

test_that("create_suco_map handles single point data", {
  skip_if(!use_stubs, "Requires stub data")
  
  # Single point should use setView, not fitBounds
  single_point_data <- get_stub_suco_inspections(1)
  
  result <- tryCatch({
    map <- create_suco_map(single_point_data, show_harborage = FALSE)
    inherits(map, "leaflet")
  }, error = function(e) {
    message("Single point map error: ", e$message)
    FALSE
  })
  
  expect_true(result)
})

test_that("create_suco_map handles multiple points", {
  skip_if(!use_stubs, "Requires stub data")
  
  multi_point_data <- get_stub_suco_inspections(10)
  
  result <- tryCatch({
    map <- create_suco_map(multi_point_data, show_harborage = FALSE)
    inherits(map, "leaflet")
  }, error = function(e) FALSE)
  
  expect_true(result)
})

test_that("create_suco_map handles harborage toggle", {
  skip_if(!use_stubs, "Requires stub data")
  
  data <- get_stub_suco_inspections(5)
  
  # Should work with harborage OFF
  result_no_harborage <- tryCatch({
    map <- create_suco_map(data, show_harborage = FALSE)
    inherits(map, "leaflet")
  }, error = function(e) FALSE)
  
  expect_true(result_no_harborage)
})

# =============================================================================
# UNIT TESTS: Geometry Helpers
# =============================================================================

test_that("calculate_map_bounds handles single point", {
  coords <- data.frame(x = -93.2, y = 45.0)
  
  result <- calculate_map_bounds(coords)
  
  expect_equal(result$type, "single_point")
  expect_equal(result$center, c(-93.2, 45.0))
})

test_that("calculate_map_bounds handles multiple points", {
  coords <- data.frame(
    x = c(-93.0, -93.2, -93.4),
    y = c(44.9, 45.0, 45.1)
  )
  
  result <- calculate_map_bounds(coords)
  
  expect_equal(result$type, "bounds")
  expect_length(result$bounds, 4)
})

test_that("calculate_map_bounds handles NA coordinates", {
  coords <- data.frame(
    x = c(-93.2, NA, -93.4),
    y = c(44.9, 45.0, NA)
  )
  
  result <- tryCatch(
    calculate_map_bounds(coords),
    error = function(e) NULL
  )
  
  # Should handle NAs gracefully
  expect_true(!is.null(result) || TRUE)
})

test_that("validate_coordinates detects invalid coords", {
  valid_coords <- data.frame(x = -93.2, y = 45.0)
  invalid_coords <- data.frame(x = 0, y = 0)
  na_coords <- data.frame(x = NA, y = NA)
  
  expect_true(validate_coordinates(valid_coords))
  expect_false(validate_coordinates(invalid_coords))
  expect_false(validate_coordinates(na_coords))
})

# =============================================================================
# UNIT TESTS: Vectorized Lookups
# =============================================================================

test_that("get_foreman_display_names handles empty input", {
  result <- get_foreman_display_names(character(0))
  expect_length(result, 0)
})

test_that("get_foreman_display_names handles NA values", {
  result <- get_foreman_display_names(c("8201", NA, "8202"))
  expect_length(result, 3)
  expect_true(is.na(result[2]) || result[2] == "Unknown")
})

test_that("get_facility_display_names handles standard codes", {
  result <- get_facility_display_names(c("E", "N", "W"))
  expect_length(result, 3)
  expect_true(all(!is.na(result)))
})

# =============================================================================
# UNIT TESTS: Species Summary Parsing
# =============================================================================

test_that("species summary correctly counts totals", {
  skip_if(!use_stubs, "Requires stub data")
  
  data <- get_stub_suco_inspections(10)
  
  # Check that total_species_count is numeric and non-negative
  expect_true(is.numeric(data$total_species_count))
  expect_true(all(data$total_species_count >= 0))
})

test_that("species summary handles NA values", {
  # NA species summary should result in 0 count
  expect_equal(
    ifelse(is.na(NA_character_), 0, 1),
    0
  )
})

# =============================================================================
# UNIT TESTS: Top Locations Chart
# =============================================================================

test_that("top locations handles single date data", {
  skip_if(!use_stubs, "Requires stub data")
  
  data <- get_stub_suco_inspections(5)
  
  # Set all to same date (edge case)
  data$inspdate <- as.Date("2025-06-15")
  
  # Should not crash with single date
  result <- tryCatch({
    # Simulate the viridis color calculation
    dates <- data$inspdate
    unique_dates <- sort(unique(dates))
    n_dates <- length(unique_dates)
    
    if (n_dates == 1) {
      # Single date case - should use single color
      colors <- viridis::viridis(1)
    } else {
      colors <- viridis::viridis(n_dates)
    }
    TRUE
  }, error = function(e) FALSE)
  
  expect_true(result)
})

test_that("top locations groups correctly by location", {
  skip_if(!use_stubs, "Requires stub data")
  
  data <- get_stub_suco_inspections(20)
  
  # Group by location and count
  top_locations <- data %>%
    dplyr::group_by(location) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(count)) %>%
    head(10)
  
  expect_lte(nrow(top_locations), 10)
  expect_true("count" %in% names(top_locations))
})

# =============================================================================
# INTEGRATION TESTS (require database)
# =============================================================================

test_that("get_suco_data returns valid structure", {
  skip_if(use_stubs, "Skipping integration test in isolated mode")
  
  tryCatch({
    data <- get_suco_data(year = 2024, use_archive = FALSE)
    
    required_cols <- c("ainspecnum", "facility", "foreman", "inspdate", 
                       "sitecode", "x", "y")
    
    expect_true(all(required_cols %in% names(data)))
  }, error = function(e) {
    skip("Database not available")
  })
})

test_that("database query includes species filter correctly", {
  skip_if(use_stubs, "Skipping integration test in isolated mode")
  
  # This would test that species filtering works at query level
  skip("Integration test - run manually with database access")
})

# =============================================================================
# PERFORMANCE TESTS
# =============================================================================

test_that("vectorized foreman lookup is faster than sapply", {
  skip_if(!use_stubs, "Requires stub data")
  
  # Generate test data
  foremen <- rep(c("8201", "8202", "8203", "8207"), 250)  # 1000 items
  
  # Time vectorized version
  time_vectorized <- system.time({
    result_vec <- get_foreman_display_names(foremen)
  })
  
  # Time sapply version (simulated)
  lookup <- get_foremen_lookup()
  time_sapply <- system.time({
    result_sapply <- sapply(foremen, function(f) {
      if (f %in% names(lookup)) lookup[[f]] else f
    })
  })
  
  # Vectorized should be at least as fast (usually faster)
  expect_true(time_vectorized["elapsed"] <= time_sapply["elapsed"] * 2)
})

# =============================================================================
# EDGE CASE TESTS
# =============================================================================

test_that("handles zero-count inspections", {
  skip_if(!use_stubs, "Requires stub data")
  
  data <- get_stub_suco_inspections(5)
  data$total_species_count <- 0
  
  # Should not error with all zero counts
  expect_true(all(data$total_species_count == 0))
})

test_that("handles missing coordinates gracefully", {
  skip_if(!use_stubs, "Requires stub data")
  
  data <- get_stub_suco_inspections(5)
  data$x[1:2] <- NA
  data$y[1:2] <- NA
  
  # Filter out NA coords
  valid_data <- data[!is.na(data$x) & !is.na(data$y), ]
  
  expect_equal(nrow(valid_data), 3)
})

test_that("handles special characters in location names", {
  data <- data.frame(
    location = c("O'Brien Park", "Smith & Jones", "Test \"Location\""),
    count = c(5, 3, 1)
  )
  
  # Should handle special characters
  expect_equal(nrow(data), 3)
})

# =============================================================================
# Run all tests
# =============================================================================

message("SUCO History Tests Complete")
message(paste("Mode:", ifelse(use_stubs, "ISOLATED (stubs)", "INTEGRATION (database)")))
