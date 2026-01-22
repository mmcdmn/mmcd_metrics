# =============================================================================
# Tests for Historical Data Helper Functions (db_helpers.R)
# =============================================================================

library(testthat)
library(dplyr)

context("Historical Data Helpers")

# =============================================================================
# apply_historical_group_labels() tests
# =============================================================================

test_that("apply_historical_group_labels returns input for NULL data", {
  result <- apply_historical_group_labels(NULL, "facility")
  expect_null(result)
})

test_that("apply_historical_group_labels returns input for empty data", {
  empty_df <- data.frame()
  result <- apply_historical_group_labels(empty_df, "facility")
  expect_equal(nrow(result), 0)
})

test_that("apply_historical_group_labels adds group_label for mmcd_all", {
  test_data <- data.frame(
    sitecode = c("A1", "A2", "A3"),
    facility = c("E", "N", "MO"),
    zone = c("1", "2", "1"),
    time_period = c("2024", "2024", "2024")
  )
  
  result <- apply_historical_group_labels(test_data, "mmcd_all")
  
  expect_true("group_label" %in% names(result))
  expect_true(all(grepl("All MMCD", result$group_label)))
})

test_that("apply_historical_group_labels adds group_label for facility", {
  skip_if_not(exists("get_facility_lookup", mode = "function"))
  
  # Skip if database not available
  lookup <- tryCatch(get_facility_lookup(), error = function(e) NULL)
  skip_if(is.null(lookup), "Database connection not available")
  
  test_data <- data.frame(
    sitecode = c("A1", "A2"),
    facility = c("E", "N"),
    zone = c("1", "1"),
    time_period = c("2024", "2024")
  )
  
  result <- apply_historical_group_labels(test_data, "facility")
  
  expect_true("group_label" %in% names(result))
  # Group labels should be full facility names
  expect_false(all(result$group_label %in% c("E", "N")))
})

test_that("apply_historical_group_labels handles zone separation", {
  test_data <- data.frame(
    sitecode = c("A1", "A2", "A3", "A4"),
    facility = c("E", "E", "N", "N"),
    zone = c("1", "2", "1", "2"),
    time_period = c("2024", "2024", "2024", "2024")
  )
  
  result <- apply_historical_group_labels(test_data, "mmcd_all", show_zones_separately = TRUE)
  
  expect_true("group_label" %in% names(result))
  # Should have zone suffixes
  expect_true(any(grepl("P1", result$group_label)))
  expect_true(any(grepl("P2", result$group_label)))
})

test_that("apply_historical_group_labels uses foreman_col parameter", {
  skip_if_not(exists("get_foremen_lookup", mode = "function"))
  
  # Skip if database not available
  lookup <- tryCatch(get_foremen_lookup(), error = function(e) NULL)
  skip_if(is.null(lookup) || nrow(lookup) == 0, "Database connection not available")
  
  # Get a real foreman ID from the lookup
  test_foreman <- lookup$emp_num[1]
  
  test_data <- data.frame(
    sitecode = c("A1", "A2"),
    fosarea = c(test_foreman, test_foreman),
    zone = c("1", "1"),
    time_period = c("2024", "2024")
  )
  
  result <- apply_historical_group_labels(test_data, "foreman", foreman_col = "fosarea")
  
  expect_true("group_label" %in% names(result))
})

# =============================================================================
# summarize_historical_data() tests
# =============================================================================

test_that("summarize_historical_data returns empty for NULL data", {
  result <- summarize_historical_data(NULL, "treatments")
  expect_equal(nrow(result), 0)
})

test_that("summarize_historical_data returns empty for data without group_label", {
  test_data <- data.frame(
    sitecode = c("A1", "A2"),
    time_period = c("2024", "2024")
  )
  
  result <- summarize_historical_data(test_data, "treatments")
  expect_equal(nrow(result), 0)
})

test_that("summarize_historical_data counts treatments correctly", {
  test_data <- data.frame(
    sitecode = c("A1", "A2", "A3", "A4"),
    group_label = c("Group1", "Group1", "Group2", "Group2"),
    time_period = c("2024", "2024", "2024", "2024")
  )
  
  result <- summarize_historical_data(test_data, "treatments")
  
  expect_true("value" %in% names(result))
  expect_equal(nrow(result), 2)  # 2 groups
  expect_equal(sum(result$value), 4)  # 4 total treatments
})

test_that("summarize_historical_data counts distinct sites correctly", {
  test_data <- data.frame(
    sitecode = c("A1", "A1", "A2", "A2"),  # Same sites treated multiple times
    group_label = c("Group1", "Group1", "Group1", "Group1"),
    time_period = c("2024", "2024", "2024", "2024")
  )
  
  result <- summarize_historical_data(test_data, "sites")
  
  expect_equal(result$value[1], 2)  # Only 2 distinct sites
})

test_that("summarize_historical_data sums acres correctly", {
  test_data <- data.frame(
    sitecode = c("A1", "A2", "A3"),
    group_label = c("Group1", "Group1", "Group1"),
    time_period = c("2024", "2024", "2024"),
    acres = c(10.5, 20.0, 15.5)
  )
  
  result <- summarize_historical_data(test_data, "acres")
  
  expect_equal(result$value[1], 46.0)
})

test_that("summarize_historical_data handles NA acres", {
  test_data <- data.frame(
    sitecode = c("A1", "A2", "A3"),
    group_label = c("Group1", "Group1", "Group1"),
    time_period = c("2024", "2024", "2024"),
    acres = c(10.0, NA, 20.0)
  )
  
  result <- summarize_historical_data(test_data, "acres")
  
  expect_equal(result$value[1], 30.0)  # NA should be ignored
})

test_that("summarize_historical_data groups by time_period", {
  test_data <- data.frame(
    sitecode = c("A1", "A2", "A3", "A4"),
    group_label = c("Group1", "Group1", "Group1", "Group1"),
    time_period = c("2023", "2023", "2024", "2024")
  )
  
  result <- summarize_historical_data(test_data, "treatments")
  
  expect_equal(nrow(result), 2)  # 2 time periods
  expect_true(all(c("2023", "2024") %in% result$time_period))
})

test_that("summarize_historical_data handles active_count metric", {
  test_data <- data.frame(
    sitecode = c("A1", "A1", "A2"),
    group_label = c("Group1", "Group1", "Group1"),
    time_period = c("2024-W01", "2024-W01", "2024-W01")
  )
  
  result <- summarize_historical_data(test_data, "active_count")
  
  expect_equal(result$value[1], 2)  # 2 distinct sites
})
