# =============================================================================
# TEST: ground_prehatch_progress app
# =============================================================================
# Tests for Ground Prehatch Progress app data and display functions
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
  source(file.path(root, "apps/ground_prehatch_progress/data_functions.R"), local = TRUE)
  
  # Create sample data similar to what load_raw_data returns (STANDARDIZED format)
  test_data <- list(
    sites = data.frame(
      sitecode = c("020207-001", "700407-010", "021335-005"),
      facility = c("N", "Sj", "N"),
      acres = c(0.50, 0.25, 1.20),
      prehatch = c("PREHATCH", "PREHATCH", "BRIQUET"),
      fosarea = c("0204", "7002", "0206"),
      zone = c("1", "1", "2"),
      stringsAsFactors = FALSE
    ),
    treatments = data.frame(
      sitecode = c("020207-001", "700407-010"),
      facility = c("N", "Sj"),
      inspdate = as.Date(c("2025-04-16", "2025-05-07")),
      acres = c(0.50, 0.25),
      fosarea = c("0204", "7002"),
      zone = c("1", "1"),
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
})

test_that("apply_data_filters handles NULL filters correctly", {
  root <- get_project_root()
  source(file.path(root, "apps/ground_prehatch_progress/data_functions.R"), local = TRUE)
  
  test_data <- list(
    sites = data.frame(
      sitecode = c("020207-001", "700407-010"),
      facility = c("N", "Sj"),
      acres = c(0.50, 0.25),
      prehatch = c("PREHATCH", "PREHATCH"),
      fosarea = c("0204", "7002"),
      zone = c("1", "2"),
      stringsAsFactors = FALSE
    ),
    treatments = data.frame(
      sitecode = c("020207-001"),
      facility = c("N"),
      inspdate = as.Date(c("2025-04-16")),
      acres = c(0.50),
      fosarea = c("0204"),
      zone = c("1"),
      stringsAsFactors = FALSE
    ),
    total_count = 2
  )
  
  # With NULL filters, all data should be returned
  result <- apply_data_filters(test_data, 
                               facility_filter = NULL, 
                               foreman_filter = NULL, 
                               zone_filter = NULL)
  
  expect_equal(nrow(result$sites), 2)
  expect_equal(nrow(result$treatments), 1)
})

test_that("filter_ground_data handles filters", {
  root <- get_project_root()
  source(file.path(root, "apps/ground_prehatch_progress/data_functions.R"), local = TRUE)
  
  test_data <- data.frame(
    sitecode = c("020207-001", "700407-010", "021335-005"),
    facility = c("N", "Sj", "N"),
    zone = c("1", "1", "2"),
    fosarea = c("0204", "7002", "0206"),
    total_count = c(10, 20, 15),
    stringsAsFactors = FALSE
  )
  
  # Test zone filter
  result <- filter_ground_data(test_data, zone_filter = "1")
  expect_equal(nrow(result), 2)
  
  # Test facility filter
  result <- filter_ground_data(test_data, facility_filter = "N")
  expect_equal(nrow(result), 2)
})

test_that("display_functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/ground_prehatch_progress/display_functions.R"), local = TRUE)
  })
})

test_that("ui_helper can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/ground_prehatch_progress/ui_helper.R"), local = TRUE)
  })
})

