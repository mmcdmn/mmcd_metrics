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

test_that("filter_cattail_data handles filters correctly", {
  root <- get_project_root()
  source(file.path(root, "apps/cattail_treatments/data_functions.R"), local = TRUE)
  
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
  result <- filter_cattail_data(test_data)
  expect_equal(nrow(result$cattail_sites), 3)
  
  # Test with zone filter
  result <- filter_cattail_data(test_data, zone_filter = "1")
  expect_equal(nrow(result$cattail_sites), 2)
  expect_true(all(result$cattail_sites$zone == "1"))
  
  # Test with facility filter
  result <- filter_cattail_data(test_data, facility_filter = "N")
  expect_equal(nrow(result$cattail_sites), 2)
  expect_true(all(result$cattail_sites$facility == "N"))
})

test_that("filter_cattail_data handles NULL data gracefully", {
  root <- get_project_root()
  source(file.path(root, "apps/cattail_treatments/data_functions.R"), local = TRUE)
  
  # Test with NULL data
  result <- filter_cattail_data(NULL)
  expect_equal(nrow(result$cattail_sites), 0)
  
  # Test with empty cattail_sites
  result <- filter_cattail_data(list(cattail_sites = NULL))
  expect_equal(nrow(result$cattail_sites), 0)
})

test_that("get_basic_filter_choices returns expected structure", {
  root <- get_project_root()
  source(file.path(root, "apps/cattail_treatments/data_functions.R"), local = TRUE)
  
  choices <- get_basic_filter_choices()
  
  expect_type(choices, "list")
  # Should have facilities and foremen (actual field names based on the function)
  expect_true("facilities" %in% names(choices))
  expect_true("foremen" %in% names(choices))
})

test_that("display functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/cattail_treatments/display_functions.R"), local = TRUE)
  })
})

test_that("ui_helper functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/cattail_treatments/ui_helper.R"), local = TRUE)
  })
})
