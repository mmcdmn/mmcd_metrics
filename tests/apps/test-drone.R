# =============================================================================
# TEST: drone app
# =============================================================================
# Tests for Drone Treatment app data and display functions
# Uses stub data from tests/stubs/ based on real database schema
# =============================================================================

# Helper to get project root (tests run from tests/apps or project root)
get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

test_that("apply_data_filters works with facility filter", {
  root <- get_project_root()
  source(file.path(root, "apps/drone/data_functions.R"), local = TRUE)
  
  # Create sample data similar to what load_raw_data returns (STANDARDIZED format)
  test_data <- list(
    sites = data.frame(
      sitecode = c("020207-001", "700407-010", "021335-005"),
      facility = c("N", "Sj", "N"),
      acres = c(0.50, 0.25, 1.20),
      prehatch = c(NA, "PREHATCH", NA),
      drone = c("Y", "M", "C"),
      foreman = c("0204", "7002", "0206"),
      zone = c("1", "1", "2"),
      stringsAsFactors = FALSE
    ),
    treatments = data.frame(
      sitecode = c("020207-001", "700407-010"),
      facility = c("N", "Sj"),
      inspdate = as.Date(c("2025-04-16", "2025-05-07")),
      treated_acres = c(0.50, 0.25),
      foreman = c("0204", "7002"),
      zone = c("1", "1"),
      prehatch = c(NA, "PREHATCH"),
      stringsAsFactors = FALSE
    ),
    total_count = 3
  )
  
  # Test filtering by facility
  result <- apply_data_filters(test_data, facility_filter = "N")
  expect_equal(nrow(result$sites), 2)
  expect_true(all(result$sites$facility == "N"))
  
  # Test filtering by zone
  result <- apply_data_filters(test_data, zone_filter = "1")
  expect_equal(nrow(result$sites), 2)
  expect_true(all(result$sites$zone == "1"))
  
  # Test prehatch only filter
  result <- apply_data_filters(test_data, prehatch_only = TRUE)
  expect_equal(nrow(result$sites), 1)
  expect_equal(result$sites$prehatch[1], "PREHATCH")
})

test_that("apply_data_filters handles NULL filters correctly", {
  root <- get_project_root()
  source(file.path(root, "apps/drone/data_functions.R"), local = TRUE)
  
  test_data <- list(
    sites = data.frame(
      sitecode = c("020207-001", "700407-010"),
      facility = c("N", "Sj"),
      acres = c(0.50, 0.25),
      prehatch = c(NA, "PREHATCH"),
      drone = c("Y", "M"),
      foreman = c("0204", "7002"),
      zone = c("1", "2"),
      stringsAsFactors = FALSE
    ),
    treatments = data.frame(
      sitecode = c("020207-001"),
      facility = c("N"),
      inspdate = as.Date(c("2025-04-16")),
      treated_acres = c(0.50),
      foreman = c("0204"),
      zone = c("1"),
      prehatch = c(NA),
      stringsAsFactors = FALSE
    ),
    total_count = 2
  )
  
  # With NULL filters, all data should be returned
  result <- apply_data_filters(test_data, 
                               facility_filter = NULL, 
                               foreman_filter = NULL, 
                               zone_filter = NULL, 
                               prehatch_only = FALSE)
  
  expect_equal(nrow(result$sites), 2)
  expect_equal(nrow(result$treatments), 1)
})

test_that("display functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/drone/display_functions.R"), local = TRUE)
  })
})

test_that("ui_helper functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/drone/ui_helper.R"), local = TRUE)
  })
})

test_that("historical_functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/drone/historical_functions.R"), local = TRUE)
  })
})
