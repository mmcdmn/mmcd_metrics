# Test: Facility Detail Boxes Data Flow
# This test verifies that the data loading and rendering pipeline works correctly
# for facility drill-down detail boxes

library(testthat)
library(shiny)

# Set working directory to apps/overview so data environment loading works
project_root <- normalizePath(file.path(getwd(), "../../.."), winslash = "/")
setwd(file.path(project_root, "apps/overview"))

# Source required files
source(file.path(project_root, "shared", "db_pool.R"))
source(file.path(project_root, "shared", "db_helpers.R"))
source(file.path(project_root, "apps", "overview", "metric_registry.R"))
source(file.path(project_root, "apps", "overview", "data_functions.R"))

test_that("load_data_by_facility returns data for ground_prehatch", {
  skip_if_not(exists("get_pool"), "Database connection not available")
  
  # Test parameters (matching what would be used in the app)
  analysis_date <- Sys.Date()
  expiring_days <- 5
  zone_filter <- c("1", "2")
  facility_filter <- "Sr"  # South Rosemount - the facility clicked in screenshot
  
  # Load data
  data <- load_data_by_facility(
    metric = "ground_prehatch",
    analysis_date = analysis_date,
    expiring_days = expiring_days,
    zone_filter = zone_filter,
    separate_zones = FALSE,
    facility_filter = facility_filter
  )
  
  # Verify data structure
  expect_false(is.null(data))
  expect_true(is.data.frame(data))
  expect_gt(nrow(data), 0)
  
  # Check for expected columns based on ground_prehatch detail boxes
  expected_cols <- c("total", "active", "expiring")
  
  cat("\nActual columns returned:", paste(names(data), collapse = ", "), "\n")
  cat("Expected columns:", paste(expected_cols, collapse = ", "), "\n")
  
  # At least some of these columns should exist
  has_expected_cols <- any(expected_cols %in% names(data))
  expect_true(has_expected_cols)
  
  # Print sample data for debugging
  cat("\nSample data (first few rows):\n")
  print(head(data))
  
  # Print column values if they exist
  for (col in expected_cols) {
    if (col %in% names(data)) {
      cat("\n", col, "value:", data[[col]][1], "\n")
    }
  }
})

test_that("get_metric_detail_boxes returns correct configuration for ground_prehatch", {
  detail_boxes <- get_metric_detail_boxes("ground_prehatch")
  
  expect_false(is.null(detail_boxes))
  expect_true(is.list(detail_boxes))
  expect_gt(length(detail_boxes), 0)
  
  cat("\nDetail boxes configuration:\n")
  for (i in seq_along(detail_boxes)) {
    box <- detail_boxes[[i]]
    cat("  Box", i, ":\n")
    cat("    id:", box$id, "\n")
    cat("    title:", box$title, "\n")
    cat("    column:", box$column, "\n")
    cat("    icon:", box$icon, "\n")
    cat("    status:", box$status, "\n")
  }
  
  # Check first box structure
  first_box <- detail_boxes[[1]]
  expect_true("id" %in% names(first_box))
  expect_true("title" %in% names(first_box))
  expect_true("column" %in% names(first_box))
})

test_that("detail box columns exist in facility data", {
  skip_if_not(exists("get_pool"), "Database connection not available")
  
  analysis_date <- Sys.Date()
  expiring_days <- 5
  zone_filter <- c("1", "2")
  facility_filter <- "Sr"
  
  data <- load_data_by_facility(
    metric = "ground_prehatch",
    analysis_date = analysis_date,
    expiring_days = expiring_days,
    zone_filter = zone_filter,
    separate_zones = FALSE,
    facility_filter = facility_filter
  )
  
  detail_boxes <- get_metric_detail_boxes("ground_prehatch")
  expected_cols <- unique(vapply(detail_boxes, function(x) x$column, character(1)))
  missing_cols <- setdiff(expected_cols, names(data))
  
  cat("\nExpected detail columns:", paste(expected_cols, collapse = ", "), "\n")
  cat("Missing columns:", paste(missing_cols, collapse = ", "), "\n")
  
  expect_equal(length(missing_cols), 0)
})

test_that("facility detail data matches facility summary data", {
  skip_if_not(exists("get_pool"), "Database connection not available")
  
  analysis_date <- Sys.Date()
  expiring_days <- 5
  zone_filter <- c("1", "2")
  
  # Load summary data (what shows in the facility boxes)
  summary_data <- load_data_by_facility(
    metric = "ground_prehatch",
    analysis_date = analysis_date,
    expiring_days = expiring_days,
    zone_filter = zone_filter,
    separate_zones = FALSE,
    facility_filter = NULL  # All facilities
  )
  
  # Load detail data for specific facility
  detail_data <- load_data_by_facility(
    metric = "ground_prehatch",
    analysis_date = analysis_date,
    expiring_days = expiring_days,
    zone_filter = zone_filter,
    separate_zones = FALSE,
    facility_filter = "Sr"
  )
  
  expect_false(is.null(summary_data))
  expect_false(is.null(detail_data))
  
  cat("\nSummary data rows:", nrow(summary_data), "\n")
  cat("Detail data rows:", nrow(detail_data), "\n")
  
  # Detail data should be a subset of summary data
  expect_lte(nrow(detail_data), nrow(summary_data))
})
