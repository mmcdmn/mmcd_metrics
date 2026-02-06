# =============================================================================
# TEST: cattail_inspections app
# =============================================================================
# Tests for Cattail Inspections app data and display functions
# Uses stub data from tests/stubs/ based on real database schema
# =============================================================================

# Helper to get project root (tests run from tests/apps or project root)
get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

test_that("progress_functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/cattail_inspections/progress_functions.R"), local = TRUE)
  })
})

test_that("planned_treatment_functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/cattail_inspections/planned_treatment_functions.R"), local = TRUE)
  })
})

test_that("historical_functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/cattail_inspections/historical_functions.R"), local = TRUE)
  })
})

test_that("create_progress_plot handles empty data", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)  # Need to load ggplot2 for functions to work
  root <- get_project_root()
  source(file.path(root, "apps/cattail_inspections/progress_functions.R"), local = TRUE)
  
  # Empty data should still return a ggplot
  empty_data <- data.frame(
    display_name = character(0),
    total_sites = integer(0),
    inspected_count = integer(0),
    goal = integer(0),
    stringsAsFactors = FALSE
  )
  
  # Should not error with empty data
  result <- create_progress_plot(empty_data)
  expect_s3_class(result, "ggplot")
})

test_that("create_treatment_plan_plot_with_data handles empty data", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)  # Need to load ggplot2 for functions to work
  root <- get_project_root()
  source(file.path(root, "apps/cattail_inspections/planned_treatment_functions.R"), local = TRUE)
  
  # Create empty data with expected columns
  empty_data <- data.frame(
    facility = character(0),
    airgrnd_plan = character(0),
    acres = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Should not error with empty data
  result <- create_treatment_plan_plot_with_data(
    data = empty_data,
    facility_filter = "all",
    plan_types_filter = c("Full", "Partial"),
    view_type = "acres"
  )
  expect_s3_class(result, "ggplot")
})

test_that("create_historical_progress_plot handles empty data", {
  skip_if_not_installed("plotly")
  root <- get_project_root()
  source(file.path(root, "apps/cattail_inspections/historical_functions.R"), local = TRUE)
  
  empty_data <- data.frame()
  
  # Should not error with empty data - returns plotly or ggplot
  result <- create_historical_progress_plot(
    data = empty_data,
    hist_years = c(2024, 2025),
    metric = "sites"
  )
  # Check it's either ggplot or plotly (both are valid plot objects)
  expect_true(inherits(result, "ggplot") || inherits(result, "plotly"))
})
