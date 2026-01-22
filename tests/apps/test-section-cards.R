# =============================================================================
# TEST: section-cards app
# =============================================================================
# Tests for Section Cards app data and display functions
# Uses stub data from tests/stubs/ based on real database schema
# =============================================================================

# Helper to get project root (tests run from tests/apps or project root)
get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

test_that("data_functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/section-cards/data_functions.R"), local = TRUE)
  })
})

test_that("display_functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/section-cards/display_functions.R"), local = TRUE)
  })
})

test_that("ui_helper functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/section-cards/ui_helper.R"), local = TRUE)
  })
})
