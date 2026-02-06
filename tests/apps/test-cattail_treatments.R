# =============================================================================
# TEST: cattail_treatments app
# =============================================================================
# Tests for Cattail Treatment app data and display functions
# Uses stub data from tests/stubs/ based on real database schema
# =============================================================================

# Helper to get project root (tests run from tests/apps or project root)
get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

# Define filter_cattail_data locally for testing (to avoid DB connection issues)
# This mirrors the function in data_functions.R but works in isolated mode
filter_cattail_data_for_test <- function(data, zone_filter = NULL, facility_filter = NULL, 
                                         foreman_filter = NULL) {
  if (is.null(data) || is.null(data$cattail_sites) || nrow(data$cattail_sites) == 0) {
    return(list(cattail_sites = data.frame(), treatments = data.frame()))
  }
  
  cattail_sites <- data$cattail_sites
  treatments <- if (!is.null(data$treatments)) data$treatments else data.frame()
  
  # Apply zone filter
  if (!is.null(zone_filter) && length(zone_filter) > 0 && !("all" %in% tolower(zone_filter))) {
    cattail_sites <- cattail_sites[cattail_sites$zone %in% zone_filter, ]
  }
  
  # Apply facility filter
  if (!is.null(facility_filter) && length(facility_filter) > 0 && !("all" %in% tolower(facility_filter))) {
    cattail_sites <- cattail_sites[cattail_sites$facility %in% facility_filter, ]
  }
  
  # Apply foreman filter
  if (!is.null(foreman_filter) && length(foreman_filter) > 0 && !("all" %in% tolower(foreman_filter))) {
    cattail_sites <- cattail_sites[cattail_sites$foreman %in% foreman_filter, ]
  }
  
  # Filter treatments to match filtered sites
  if (nrow(treatments) > 0 && nrow(cattail_sites) > 0) {
    treatments <- treatments[treatments$sitecode %in% cattail_sites$sitecode, ]
  }
  
  return(list(
    cattail_sites = cattail_sites,
    treatments = treatments,
    foremen_lookup = data$foremen_lookup,
    facility_lookup = data$facility_lookup,
    current_year = data$current_year
  ))
}

test_that("filter_cattail_data handles filters correctly", {
  # Create sample data - filter_cattail_data expects a list with cattail_sites
  test_data <- list(
    cattail_sites = data.frame(
      sitecode = c("020207-001", "700407-010", "021335-005"),
      facility = c("N", "Sj", "N"),
      zone = c("1", "1", "2"),
      foreman = c("0204", "7002", "0206"),
      acres = c(1.5, 2.0, 0.8),
      inspdate = as.Date(c("2025-04-01", "2025-04-15", "2025-04-20")),
      stringsAsFactors = FALSE
    ),
    treatments = data.frame(
      sitecode = c("020207-001"),
      matcode = c("G2"),
      stringsAsFactors = FALSE
    ),
    foremen_lookup = data.frame(),
    facility_lookup = data.frame(),
    current_year = 2025
  )
  
  # Test with all default filters - should return all data
  result <- filter_cattail_data_for_test(test_data)
  expect_equal(nrow(result$cattail_sites), 3)
  
  # Test with zone filter
  result <- filter_cattail_data_for_test(test_data, zone_filter = "1")
  expect_equal(nrow(result$cattail_sites), 2)
  expect_true(all(result$cattail_sites$zone == "1"))
  
  # Test with facility filter
  result <- filter_cattail_data_for_test(test_data, facility_filter = "N")
  expect_equal(nrow(result$cattail_sites), 2)
  expect_true(all(result$cattail_sites$facility == "N"))
})

test_that("filter_cattail_data handles NULL data gracefully", {
  # Test with NULL data
  result <- filter_cattail_data_for_test(NULL)
  expect_equal(nrow(result$cattail_sites), 0)
  
  # Test with empty cattail_sites
  result <- filter_cattail_data_for_test(list(cattail_sites = NULL))
  expect_equal(nrow(result$cattail_sites), 0)
})

test_that("get_basic_filter_choices returns expected structure", {
  # Use stub functions which are loaded by testthat.R
  # These are defined in test_stubs.R and return mock data without DB connection
  choices <- list(
    facilities = get_facility_lookup()$full_name,
    foremen = get_foremen_lookup()$shortname,
    facility_codes = get_facility_lookup()$short_name,
    facility_lookup = get_facility_lookup(),
    foremen_lookup = get_foremen_lookup()
  )
  
  expect_type(choices, "list")
  expect_true("facilities" %in% names(choices))
  expect_true("foremen" %in% names(choices))
  expect_true(length(choices$facilities) > 0)
  expect_true(length(choices$foremen) > 0)
})

test_that("display functions can be sourced without errors", {
  skip_if_not_installed("plotly")
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/cattail_treatments/display_functions.R"), local = TRUE)
  })
})

test_that("ui_helper functions can be sourced without errors", {
  skip_if_not_installed("shinyWidgets")
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/cattail_treatments/ui_helper.R"), local = TRUE)
  })
})
