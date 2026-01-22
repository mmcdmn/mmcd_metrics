# =============================================================================
# Tests for Export Helper Functions (db_helpers.R)
# =============================================================================

library(testthat)

context("Export Helpers")

# =============================================================================
# export_csv_safe() tests
# =============================================================================

test_that("export_csv_safe returns error result for NULL data", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))
  
  result <- export_csv_safe(NULL, temp_file)
  
  expect_type(result, "list")
  expect_false(result$success)
  expect_equal(result$rows_exported, 0)
})

test_that("export_csv_safe handles empty data frame", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))
  
  empty_df <- data.frame(col1 = character(), col2 = numeric())
  result <- export_csv_safe(empty_df, temp_file)
  
  expect_true(result$success)
  expect_equal(result$rows_exported, 0)
  expect_true(file.exists(temp_file))
})

test_that("export_csv_safe writes data to file", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))
  
  test_data <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    value = c(1, 2, 3)
  )
  
  result <- export_csv_safe(test_data, temp_file)
  
  expect_true(result$success)
  expect_equal(result$rows_exported, 3)
  expect_true(file.exists(temp_file))
  
  # Verify content
  read_back <- read.csv(temp_file)
  expect_equal(nrow(read_back), 3)
})

test_that("export_csv_safe cleans problematic characters", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))
  
  test_data <- data.frame(
    name = c("Line\nBreak", "Tab\there", "Quote\"test"),
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  
  result <- export_csv_safe(test_data, temp_file, clean_data = TRUE)
  
  expect_true(result$success)
  
  # File should be readable
  read_back <- read.csv(temp_file, stringsAsFactors = FALSE)
  expect_equal(nrow(read_back), 3)
})

test_that("export_csv_safe respects row_names parameter", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))
  
  test_data <- data.frame(
    name = c("A", "B"),
    value = c(1, 2)
  )
  
  # Without row names
  export_csv_safe(test_data, temp_file, row_names = FALSE)
  lines_without <- readLines(temp_file)
  
  # With row names
  export_csv_safe(test_data, temp_file, row_names = TRUE)
  lines_with <- readLines(temp_file)
  
  # Row names version should have extra column
  expect_true(nchar(lines_with[1]) > nchar(lines_without[1]))
})

test_that("export_csv_safe handles NA values", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))
  
  test_data <- data.frame(
    name = c("A", NA, "C"),
    value = c(1, NA, 3)
  )
  
  result <- export_csv_safe(test_data, temp_file, na_string = "")
  
  expect_true(result$success)
  
  # Check that NAs are handled
  content <- readLines(temp_file)
  expect_true(length(content) > 0)
})

# =============================================================================
# get_universal_text_css() tests
# =============================================================================

test_that("get_universal_text_css returns shiny tag", {
  result <- get_universal_text_css()
  
  expect_s3_class(result, "shiny.tag")
})

test_that("get_universal_text_css includes style element", {
  result <- get_universal_text_css()
  
  # Check structure of the shiny tag - should be a head tag containing style
  expect_equal(result$name, "head")
  # Should have children (the style tag)
  expect_true(length(result$children) > 0)
})

test_that("get_universal_text_css respects base_increase parameter", {
  small_css <- get_universal_text_css(base_increase = 2)
  large_css <- get_universal_text_css(base_increase = 8)
  
  # Both should be valid shiny tags
  expect_s3_class(small_css, "shiny.tag")
  expect_s3_class(large_css, "shiny.tag")
  
  # Render to HTML strings for comparison
  small_html <- as.character(htmltools::renderTags(small_css)$head)
  large_html <- as.character(htmltools::renderTags(large_css)$head)
  
  # They should be different (different font sizes)
  expect_false(identical(small_html, large_html))
})

test_that("get_universal_text_css includes font-size rules", {
  result <- get_universal_text_css()
  
  # Render to HTML string
  html_output <- as.character(htmltools::renderTags(result)$head)
  expect_true(grepl("font-size", html_output))
})
