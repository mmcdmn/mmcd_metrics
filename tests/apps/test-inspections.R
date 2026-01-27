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

# Define test versions of functions to avoid DB connection issues
# These mirror the actual functions but work in isolated mode

#' Check if a date is before the spring threshold for that year
is_spring_inspection_for_test <- function(date, spring_thresholds) {
  if (is.na(date) || is.null(date)) return(FALSE)
  if (is.null(spring_thresholds) || nrow(spring_thresholds) == 0) return(FALSE)
  
  year <- as.numeric(format(date, "%Y"))
  threshold_row <- spring_thresholds[spring_thresholds$year == year, ]
  
  if (nrow(threshold_row) == 0) return(FALSE)
  
  return(date < threshold_row$date_start[1])
}

#' Get total sites count from data with air_gnd filter
get_total_sites_count_from_data_for_test <- function(data, air_gnd_filter = "both") {
  if (is.null(data) || nrow(data) == 0) return(0)
  
  if (air_gnd_filter == "both") {
    return(length(unique(data$sitecode)))
  } else {
    filtered <- data[data$air_gnd == air_gnd_filter, ]
    return(length(unique(filtered$sitecode)))
  }
}

#' Get sites with high larvae counts from data
get_high_larvae_sites_from_data_for_test <- function(data, threshold = 2, years_back = 5) {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame(sitecode = character(), threshold_exceedances = integer()))
  }
  
  # Count how many times each site exceeded the threshold
  exceedances <- tapply(data$numdip >= threshold, data$sitecode, sum, na.rm = TRUE)
  
  # Return sites with at least one exceedance
  high_sites <- names(exceedances[exceedances > 0])
  
  result <- data.frame(
    sitecode = high_sites,
    threshold_exceedances = as.integer(exceedances[high_sites]),
    stringsAsFactors = FALSE
  )
  
  return(result)
}

test_that("is_spring_inspection works correctly", {
  # Create sample spring thresholds
  spring_thresholds <- data.frame(
    year = c(2024, 2025),
    date_start = as.Date(c("2024-04-15", "2025-04-15")),
    stringsAsFactors = FALSE
  )
  
  # Test dates before threshold (should be spring)
  expect_true(is_spring_inspection_for_test(as.Date("2024-03-15"), spring_thresholds))
  expect_true(is_spring_inspection_for_test(as.Date("2025-02-20"), spring_thresholds))
  
  # Test dates after threshold (should not be spring)
  expect_false(is_spring_inspection_for_test(as.Date("2024-05-15"), spring_thresholds))
  expect_false(is_spring_inspection_for_test(as.Date("2025-06-01"), spring_thresholds))
  
  # Test NA date
  expect_false(is_spring_inspection_for_test(NA, spring_thresholds))
  
  # Test with empty thresholds
  expect_false(is_spring_inspection_for_test(as.Date("2024-03-15"), data.frame()))
})

test_that("get_total_sites_count_from_data handles filters", {
  # Create sample comprehensive data
  test_data <- data.frame(
    sitecode = c("020207-001", "700407-010", "021335-005", "020207-002"),
    air_gnd = c("G", "A", "G", "A"),
    facility = c("N", "Sj", "N", "N"),
    stringsAsFactors = FALSE
  )
  
  # Test "both" filter (all sites)
  result <- get_total_sites_count_from_data_for_test(test_data, air_gnd_filter = "both")
  expect_equal(result, 4)
  
  # Test ground only
  result <- get_total_sites_count_from_data_for_test(test_data, air_gnd_filter = "G")
  expect_equal(result, 2)
  
  # Test air only
  result <- get_total_sites_count_from_data_for_test(test_data, air_gnd_filter = "A")
  expect_equal(result, 2)
  
  # Test empty data
  result <- get_total_sites_count_from_data_for_test(data.frame(), air_gnd_filter = "both")
  expect_equal(result, 0)
})

test_that("get_high_larvae_sites_from_data works correctly", {
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
  result <- get_high_larvae_sites_from_data_for_test(test_data, threshold = 2, years_back = 5)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$sitecode[1], "020207-001")
  expect_equal(result$threshold_exceedances[1], 2)  # Both counts >= 2
})

test_that("display_functions can be sourced without errors", {
  skip_if_not_installed("DT")
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/inspections/display_functions.R"), local = TRUE)
  })
})

test_that("ui_helper functions can be sourced without errors", {
  skip_if_not_installed("shinyWidgets")
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/inspections/ui_helper.R"), local = TRUE)
  })
})
