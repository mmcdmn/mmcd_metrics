# =============================================================================
# TEST: struct_trt app
# =============================================================================
# Tests for Structure Treatment app data and display functions
# Uses stub data from tests/stubs/ based on real database schema
# =============================================================================

# These tests run in isolated mode - no database required
# Data functions are tested by mocking database calls

# Helper to get project root (tests run from tests/apps or project root)
get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

test_that("get_facility_condition returns correct SQL", {
  root <- get_project_root()
  # Source the data_functions to test
  source(file.path(root, "apps/struct_trt/data_functions.R"), local = TRUE)
  
  # Test with "all" filter
  result <- get_facility_condition("all")
  expect_equal(result, "")
  
  # Test with NULL filter  
  result <- get_facility_condition(NULL)
  expect_equal(result, "")
  
  # Test with single facility
  result <- get_facility_condition("N")
  expect_match(result, "AND gis.facility IN")
  expect_match(result, "'N'")
  
  # Test with multiple facilities
  result <- get_facility_condition(c("N", "Sj"))
  expect_match(result, "'N'")
  expect_match(result, "'Sj'")
})

test_that("get_structure_type_condition handles CV and PR types", {
  root <- get_project_root()
  source(file.path(root, "apps/struct_trt/data_functions.R"), local = TRUE)
  
  # Test with "all"
  result <- get_structure_type_condition("all")
  expect_equal(result, "")
  
  # Test with CV - should match CV and CV/PR combinations
  result <- get_structure_type_condition("CV")
  expect_match(result, "UPPER.*CV")
  expect_match(result, "LIKE")
  
  # Test with PR - should match PR and CV/PR combinations
  result <- get_structure_type_condition("PR")
  expect_match(result, "UPPER.*PR")
  
  # Test with other type
  result <- get_structure_type_condition("WO")
  expect_match(result, "UPPER.*WO")
})

test_that("get_priority_condition builds correct SQL", {
  root <- get_project_root()
  source(file.path(root, "apps/struct_trt/data_functions.R"), local = TRUE)
  
  # Test with "all"
  result <- get_priority_condition("all")
  expect_equal(result, "")
  
  # Test with specific priority
  result <- get_priority_condition("RED")
  expect_match(result, "priority = 'RED'")
})

test_that("get_status_condition handles status types", {
  root <- get_project_root()
  source(file.path(root, "apps/struct_trt/data_functions.R"), local = TRUE)
  
  # Test with NULL - should return no results condition

  result <- get_status_condition(NULL)
  expect_match(result, "FALSE")
  
  # Test with empty vector
  result <- get_status_condition(character(0))
  expect_match(result, "FALSE")
  
  # Test with status types
  result <- get_status_condition(c("D", "W", "U"))
  expect_match(result, "status_udw IN")
  expect_match(result, "'D'")
  expect_match(result, "'W'")
  expect_match(result, "'U'")
})

test_that("display functions can be sourced without errors", {
  root <- get_project_root()
  # Ensure display functions module loads successfully
  expect_no_error({
    source(file.path(root, "apps/struct_trt/display_functions.R"), local = TRUE)
  })
})

test_that("create_current_progress_chart handles empty data", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)  # Need to load ggplot2 for functions to work
  root <- get_project_root()
  source(file.path(root, "apps/struct_trt/display_functions.R"), local = TRUE)
  
  # Test with empty data frame
  empty_df <- data.frame()
  result <- create_current_progress_chart(
    data = empty_df, 
    group_by = "facility",
    facility_filter = "all",
    status_types = c("D", "W", "U"),
    zone_filter = c("1", "2")
  )
  
  # Should return a ggplot object
  expect_s3_class(result, "ggplot")
})

test_that("ui_helper functions can be sourced without errors", {
  root <- get_project_root()
  # Ensure UI helper module loads successfully
  expect_no_error({
    source(file.path(root, "apps/struct_trt/ui_helper.R"), local = TRUE)
  })
})
