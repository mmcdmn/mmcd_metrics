# =============================================================================
# Tests for Stat Box Helper Functions (stat_box_helpers.R)
# =============================================================================

library(testthat)
library(shiny)

context("Stat Box Helpers")

# Helper function to render shiny tag to HTML string
render_to_html <- function(tag) {
  htmltools::doRenderTags(tag)
}

# =============================================================================
# create_stat_box() tests
# =============================================================================

test_that("create_stat_box returns shiny tag", {
  result <- create_stat_box(
    value = "100",
    title = "Test Box",
    bg_color = "#3c8dbc"
  )
  
  expect_true("shiny.tag" %in% class(result))
})

test_that("create_stat_box includes value in output", {
  result <- create_stat_box(
    value = "12345",
    title = "Test",
    bg_color = "#ffffff"
  )
  
  # Convert to HTML and check for value
  html_output <- render_to_html(result)
  expect_true(grepl("12345", html_output))
})

test_that("create_stat_box includes title in output", {
  result <- create_stat_box(
    value = "100",
    title = "My Custom Title",
    bg_color = "#ffffff"
  )
  
  html_output <- render_to_html(result)
  expect_true(grepl("My Custom Title", html_output))
})

test_that("create_stat_box applies background color", {
  result <- create_stat_box(
    value = "100",
    title = "Test",
    bg_color = "#ff5733"
  )
  
  html_output <- render_to_html(result)
  expect_true(grepl("#ff5733", html_output, ignore.case = TRUE))
})

test_that("create_stat_box handles text color parameter", {
  result <- create_stat_box(
    value = "100",
    title = "Test",
    bg_color = "#ffffff",
    text_color = "#000000"
  )
  
  html_output <- render_to_html(result)
  expect_true(grepl("#000000", html_output))
})

test_that("create_stat_box handles icon parameter as string", {
  result <- create_stat_box(
    value = "100",
    title = "Test",
    bg_color = "#ffffff",
    icon = "check"
  )
  
  html_output <- render_to_html(result)
  expect_true(grepl("fa-check", html_output) || grepl("check", html_output))
})

test_that("create_stat_box handles icon parameter as icon object", {
  result <- create_stat_box(
    value = "100",
    title = "Test",
    bg_color = "#ffffff",
    icon = icon("star")
  )
  
  expect_true("shiny.tag" %in% class(result))
})

test_that("create_stat_box handles NULL icon", {
  result <- create_stat_box(
    value = "100",
    title = "Test",
    bg_color = "#ffffff",
    icon = NULL
  )
  
  expect_true("shiny.tag" %in% class(result))
})

# =============================================================================
# create_status_stat_box() tests
# =============================================================================

test_that("create_status_stat_box returns shiny tag", {
  result <- create_status_stat_box(
    value = "100",
    title = "Test",
    status = "completed"
  )
  
  expect_true("shiny.tag" %in% class(result))
})

test_that("create_status_stat_box uses status colors", {
  # Get expected color for "completed" status - use MMCD theme since default falls back to it
  status_colors <- get_status_colors(theme = "MMCD")
  expected_color <- status_colors[["completed"]]
  
  result <- create_status_stat_box(
    value = "100",
    title = "Test",
    status = "completed",
    theme = "MMCD"
  )
  
  html_output <- render_to_html(result)
  expect_true(grepl(expected_color, html_output, ignore.case = TRUE))
})

test_that("create_status_stat_box handles unknown status gracefully", {
  # Should not error, should use fallback color
  result <- create_status_stat_box(
    value = "100",
    title = "Test",
    status = "nonexistent_status"
  )
  
  expect_true("shiny.tag" %in% class(result))
  # Should contain the fallback blue color
  html_output <- render_to_html(result)
  expect_true(grepl("#3c8dbc", html_output, ignore.case = TRUE))
})

test_that("create_status_stat_box respects theme parameter", {
  mmcd_result <- create_status_stat_box(
    value = "100",
    title = "Test",
    status = "completed",  # Use a status that exists in both themes
    theme = "MMCD"
  )
  
  ibm_result <- create_status_stat_box(
    value = "100",
    title = "Test",
    status = "completed",
    theme = "IBM"
  )
  
  # Both should render successfully
  mmcd_html <- render_to_html(mmcd_result)
  ibm_html <- render_to_html(ibm_result)
  
  # Both should be non-empty strings
  expect_true(nchar(mmcd_html) > 0)
  expect_true(nchar(ibm_html) > 0)
})
