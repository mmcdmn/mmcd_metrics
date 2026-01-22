# =============================================================================
# Tests for Lookup Table Functions (db_helpers.R)
# =============================================================================
# These tests work with stubs in isolated mode or real data in integration mode

library(testthat)

context("Lookup Table Functions")

# =============================================================================
# get_facility_lookup() tests
# =============================================================================

test_that("get_facility_lookup returns data frame", {
  result <- get_facility_lookup()
  
  expect_s3_class(result, "data.frame")
  expect_true("short_name" %in% names(result))
  expect_true("full_name" %in% names(result))
})

test_that("get_facility_lookup includes required facilities", {
  result <- get_facility_lookup()
  
  # These facilities should exist in real DB and in stubs
  required <- c("E", "MO", "N")
  for (fac in required) {
    expect_true(fac %in% result$short_name,
                info = paste("Missing facility:", fac))
  }
})

test_that("get_facility_lookup has consistent column types", {
  result <- get_facility_lookup()
  
  expect_type(result$short_name, "character")
  expect_type(result$full_name, "character")
})

# =============================================================================
# get_facility_choices() tests
# =============================================================================

test_that("get_facility_choices returns named vector", {
  result <- get_facility_choices()
  
  expect_type(result, "character")
  expect_true(length(names(result)) > 0)
})

test_that("get_facility_choices includes 'All' when requested", {
  result <- get_facility_choices(include_all = TRUE)
  
  expect_true("all" %in% result)
})

test_that("get_facility_choices excludes 'All' when requested", {
  result <- get_facility_choices(include_all = FALSE)
  
  expect_false("all" %in% result)
})

test_that("get_facility_choices values match lookup short_names", {
  lookup <- get_facility_lookup()
  choices <- get_facility_choices(include_all = FALSE)
  
  # All choice values should be in lookup short_names
  for (val in choices) {
    expect_true(val %in% lookup$short_name,
                info = paste("Choice value not in lookup:", val))
  }
})

# =============================================================================
# get_foremen_lookup() tests
# =============================================================================

test_that("get_foremen_lookup returns data frame", {
  result <- get_foremen_lookup()
  
  expect_s3_class(result, "data.frame")
  expect_true("emp_num" %in% names(result))
  expect_true("shortname" %in% names(result))
  expect_true("facility" %in% names(result))
})

test_that("get_foremen_lookup has valid facility references", {
  foremen <- get_foremen_lookup()
  facilities <- get_facility_lookup()
  
  # All foreman facilities should be valid
  for (fac in unique(foremen$facility)) {
    expect_true(fac %in% facilities$short_name,
                info = paste("Invalid facility in foremen:", fac))
  }
})

# =============================================================================
# get_foreman_choices() tests
# =============================================================================

test_that("get_foreman_choices returns named vector", {
  result <- get_foreman_choices()
  
  expect_type(result, "character")
  expect_true(length(names(result)) > 0)
})

test_that("get_foreman_choices includes 'All FOS' when requested", {
  result <- get_foreman_choices(include_all = TRUE)
  
  expect_true("all" %in% result)
})

test_that("get_foreman_choices excludes 'All FOS' when requested", {
  result <- get_foreman_choices(include_all = FALSE)
  
  expect_false("all" %in% result)
})

# =============================================================================
# get_priority_choices() tests
# =============================================================================

test_that("get_priority_choices returns expected priorities", {
  result <- get_priority_choices(include_all = FALSE)
  
  expected <- c("RED", "YELLOW", "BLUE", "GREEN", "PURPLE")
  for (p in expected) {
    expect_true(p %in% result,
                info = paste("Missing priority:", p))
  }
})

test_that("get_priority_choices includes 'All' when requested", {
  result <- get_priority_choices(include_all = TRUE)
  
  expect_true("all" %in% result)
})

# =============================================================================
# get_virus_target_choices() tests
# =============================================================================

test_that("get_virus_target_choices returns expected targets", {
  result <- get_virus_target_choices(include_all = FALSE)
  
  # WNV should always be included
  expect_true("WNV" %in% result)
})

test_that("get_virus_target_choices is named vector", {
  result <- get_virus_target_choices()
  
  expect_type(result, "character")
  expect_true(length(names(result)) > 0)
  # Names should be descriptive (like "West Nile Virus")
  expect_true(any(grepl("Virus|Encephalitis", names(result))))
})
