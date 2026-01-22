# =============================================================================
# TEST: control_efficacy app
# =============================================================================
# Tests for Control Efficacy app data and display functions
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
    source(file.path(root, "apps/control_efficacy/data_functions.R"), local = TRUE)
  })
})

test_that("checkback_functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/control_efficacy/checkback_functions.R"), local = TRUE)
  })
})

test_that("display_functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/control_efficacy/display_functions.R"), local = TRUE)
  })
})

test_that("ui_helper functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/control_efficacy/ui_helper.R"), local = TRUE)
  })
})
