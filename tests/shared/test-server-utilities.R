# =============================================================================
# Tests for Server Utilities (server_utilities.R)
# =============================================================================

library(testthat)
library(plotly)

context("Server Utilities")

# =============================================================================
# map_facility_display_names_to_colors() tests
# =============================================================================

test_that("map_facility_display_names_to_colors returns empty for empty input", {
  result <- map_facility_display_names_to_colors(character(0))
  expect_length(result, 0)
})

test_that("map_facility_display_names_to_colors returns named vector", {
  skip_if_not(exists("get_facility_lookup", mode = "function"))
  
  # Skip if database not available
  lookup <- tryCatch(get_facility_lookup(), error = function(e) NULL)
  skip_if(is.null(lookup), "Database connection not available")
  
  # Get a real facility name
  test_name <- lookup$full_name[1]
  
  result <- map_facility_display_names_to_colors(test_name)
  
  expect_type(result, "character")
  expect_true(test_name %in% names(result))
})

test_that("map_facility_display_names_to_colors handles zone suffixes", {
  skip_if_not(exists("get_facility_lookup", mode = "function"))
  
  lookup <- tryCatch(get_facility_lookup(), error = function(e) NULL)
  skip_if(is.null(lookup), "Database connection not available")
  
  # Get a real facility name and add zone suffix
  test_name <- paste(lookup$full_name[1], "P1")
  
  result <- map_facility_display_names_to_colors(test_name, handle_zones = TRUE)
  
  expect_type(result, "character")
  expect_true(test_name %in% names(result))
})

test_that("map_facility_display_names_to_colors returns valid hex colors", {
  skip_if_not(exists("get_facility_lookup", mode = "function"))
  
  lookup <- tryCatch(get_facility_lookup(), error = function(e) NULL)
  skip_if(is.null(lookup), "Database connection not available")
  
  test_names <- lookup$full_name[1:min(3, nrow(lookup))]
  result <- map_facility_display_names_to_colors(test_names)
  
  for (color in result) {
    expect_match(color, "^#[0-9A-Fa-f]{6}$",
                 info = paste("Invalid hex color:", color))
  }
})

# =============================================================================
# map_foreman_display_names_to_colors() tests
# =============================================================================

test_that("map_foreman_display_names_to_colors returns empty for empty input", {
  result <- map_foreman_display_names_to_colors(character(0))
  expect_length(result, 0)
})

test_that("map_foreman_display_names_to_colors returns named vector", {
  skip_if_not(exists("get_foremen_lookup", mode = "function"))
  
  lookup <- tryCatch(get_foremen_lookup(), error = function(e) NULL)
  skip_if(is.null(lookup) || nrow(lookup) == 0, "Database connection not available")
  
  # Get a real foreman name
  test_name <- lookup$shortname[1]
  
  result <- map_foreman_display_names_to_colors(test_name)
  
  expect_type(result, "character")
})

# =============================================================================
# create_trend_chart() tests
# =============================================================================

test_that("create_trend_chart returns plotly object for empty data", {
  empty_data <- data.frame()
  result <- create_trend_chart(empty_data, "stacked_bar", "Test", "X", "Y")
  
  expect_s3_class(result, "plotly")
})

test_that("create_trend_chart creates stacked bar chart", {
  test_data <- data.frame(
    time_period = c("2023", "2023", "2024", "2024"),
    value = c(10, 20, 15, 25),
    group_label = c("A", "B", "A", "B")
  )
  
  result <- create_trend_chart(test_data, "stacked_bar", "Test Chart", "Year", "Count")
  
  expect_s3_class(result, "plotly")
})

test_that("create_trend_chart creates grouped bar chart", {
  test_data <- data.frame(
    time_period = c("2023", "2023", "2024", "2024"),
    value = c(10, 20, 15, 25),
    group_label = c("A", "B", "A", "B")
  )
  
  result <- create_trend_chart(test_data, "grouped_bar", "Test Chart", "Year", "Count")
  
  expect_s3_class(result, "plotly")
})

test_that("create_trend_chart creates line chart", {
  test_data <- data.frame(
    time_period = c("2023", "2024", "2025"),
    value = c(10, 15, 20),
    group_label = c("A", "A", "A")
  )
  
  result <- create_trend_chart(test_data, "line", "Test Chart", "Year", "Count")
  
  expect_s3_class(result, "plotly")
})

test_that("create_trend_chart applies custom colors", {
  test_data <- data.frame(
    time_period = c("2023", "2024"),
    value = c(10, 20),
    group_label = c("A", "A")
  )
  
  custom_colors <- c("A" = "#ff0000")
  result <- create_trend_chart(
    test_data, "stacked_bar", "Test", "X", "Y", 
    colors = custom_colors
  )
  
  expect_s3_class(result, "plotly")
})

# =============================================================================
# create_distribution_chart() tests (if available)
# =============================================================================

test_that("create_distribution_chart returns plotly object", {
  skip_if_not(exists("create_distribution_chart", mode = "function"))
  
  result <- create_distribution_chart(
    x_values = c("A", "B", "C"),
    y_values = c(10, 20, 30),
    title = "Test Distribution",
    x_label = "Category"
  )
  
  expect_s3_class(result, "plotly")
})
