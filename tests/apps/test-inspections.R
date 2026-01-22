# =============================================================================
# TEST: inspections app
# =============================================================================
# Tests for Inspections app data and display functions
# Uses stub data from tests/stubs/ based on real database schema
# =============================================================================

# Helper to get project root (tests run from tests/apps or project root)
get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

test_that("is_spring_inspection works correctly", {
  root <- get_project_root()
  source(file.path(root, "apps/inspections/data_functions.R"), local = TRUE)
  
  # Create sample spring thresholds
  spring_thresholds <- data.frame(
    year = c(2024, 2025),
    date_start = as.Date(c("2024-04-15", "2025-04-15")),
    stringsAsFactors = FALSE
  )
  
  # Test dates before threshold (should be spring)
  expect_true(is_spring_inspection(as.Date("2024-03-15"), spring_thresholds))
  expect_true(is_spring_inspection(as.Date("2025-02-20"), spring_thresholds))
  
  # Test dates after threshold (should not be spring)
  expect_false(is_spring_inspection(as.Date("2024-05-15"), spring_thresholds))
  expect_false(is_spring_inspection(as.Date("2025-06-01"), spring_thresholds))
  
  # Test NA date
  expect_false(is_spring_inspection(NA, spring_thresholds))
  
  # Test with empty thresholds
  expect_false(is_spring_inspection(as.Date("2024-03-15"), data.frame()))
})

test_that("get_total_sites_count_from_data handles filters", {
  root <- get_project_root()
  source(file.path(root, "apps/inspections/data_functions.R"), local = TRUE)
  
  # Create sample comprehensive data
  test_data <- data.frame(
    sitecode = c("020207-001", "700407-010", "021335-005", "020207-002"),
    air_gnd = c("G", "A", "G", "A"),
    facility = c("N", "Sj", "N", "N"),
    stringsAsFactors = FALSE
  )
  
  # Test "both" filter (all sites)
  result <- get_total_sites_count_from_data(test_data, air_gnd_filter = "both")
  expect_equal(result, 4)
  
  # Test ground only
  result <- get_total_sites_count_from_data(test_data, air_gnd_filter = "G")
  expect_equal(result, 2)
  
  # Test air only
  result <- get_total_sites_count_from_data(test_data, air_gnd_filter = "A")
  expect_equal(result, 2)
  
  # Test empty data
  result <- get_total_sites_count_from_data(data.frame(), air_gnd_filter = "both")
  expect_equal(result, 0)
})

test_that("get_high_larvae_sites_from_data works correctly", {
  root <- get_project_root()
  source(file.path(root, "apps/inspections/data_functions.R"), local = TRUE)
  
  # Create sample comprehensive data with inspection details
  test_data <- data.frame(
    sitecode = c("020207-001", "020207-001", "700407-010", "700407-010"),
    air_gnd = c("G", "G", "G", "G"),
    facility = c("N", "N", "Sj", "Sj"),
    fosarea = c("0204", "0204", "7002", "7002"),
    zone = c("1", "1", "1", "1"),
    priority = c("RED", "RED", "GREEN", "GREEN"),
    acres = c(0.5, 0.5, 0.25, 0.25),
    years_with_data = c(3, 3, 2, 2),
    prehatch = c(NA, NA, "PREHATCH", "PREHATCH"),
    numdip = c(5, 3, 1, 0),  # First site has high larvae
    inspdate = as.Date(c("2024-06-15", "2024-07-15", "2024-06-20", "2024-07-20")),
    action = c("1", "1", "1", "1"),
    wet = c("1", "1", "0", "0"),
    stringsAsFactors = FALSE
  )
  
  # Get sites with high larvae (threshold = 2)
  result <- get_high_larvae_sites_from_data(test_data, threshold = 2, years_back = 5)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$sitecode[1], "020207-001")
  expect_equal(result$threshold_exceedances[1], 2)  # Both counts >= 2
})

test_that("display_functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/inspections/display_functions.R"), local = TRUE)
  })
})

test_that("ui_helper functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/inspections/ui_helper.R"), local = TRUE)
  })
})
