# =============================================================================
# Tests for SQL Filter Helper Functions (db_helpers.R)
# =============================================================================

library(testthat)

context("SQL Filter Helpers")

# =============================================================================
# is_valid_filter() tests
# =============================================================================

test_that("is_valid_filter returns FALSE for NULL input", {
  expect_false(is_valid_filter(NULL))
})

test_that("is_valid_filter returns FALSE for empty vector", {
  expect_false(is_valid_filter(character(0)))
  expect_false(is_valid_filter(c()))
})

test_that("is_valid_filter returns FALSE when 'all' is selected", {
  expect_false(is_valid_filter("all"))
  expect_false(is_valid_filter(c("all")))
  expect_false(is_valid_filter(c("all", "other")))  # "all" anywhere means no filter
})

test_that("is_valid_filter handles case-insensitive 'all' check", {
  expect_false(is_valid_filter("ALL"))
  expect_false(is_valid_filter("All"))
  expect_false(is_valid_filter("aLl"))
})

test_that("is_valid_filter returns TRUE for valid filter values", {
  expect_true(is_valid_filter("E"))
  expect_true(is_valid_filter(c("E", "W")))
  expect_true(is_valid_filter(c("1", "2")))
})

test_that("is_valid_filter case_sensitive option works", {
  # With case_sensitive = TRUE, only exact "all" or "All" should be excluded
  expect_false(is_valid_filter("all", case_sensitive = TRUE))
  expect_false(is_valid_filter("All", case_sensitive = TRUE))
  # "ALL" would need to be checked differently with case_sensitive
})

# =============================================================================
# build_sql_in_list() tests
# =============================================================================

test_that("build_sql_in_list returns empty string for NULL", {
  expect_equal(build_sql_in_list(NULL), "")
})
  
test_that("build_sql_in_list returns empty string for empty vector", {
  expect_equal(build_sql_in_list(character(0)), "")
})

test_that("build_sql_in_list formats single value correctly", {
  expect_equal(build_sql_in_list("E"), "'E'")
})

test_that("build_sql_in_list formats multiple values correctly", {
  expect_equal(build_sql_in_list(c("E", "W")), "'E', 'W'")
  expect_equal(build_sql_in_list(c("1", "2", "3")), "'1', '2', '3'")
})

test_that("build_sql_in_list handles numeric values", {
  expect_equal(build_sql_in_list(c(1, 2, 3)), "'1', '2', '3'")
})

# =============================================================================
# build_sql_in_clause() tests
# =============================================================================

test_that("build_sql_in_clause returns empty for invalid filter", {
  expect_equal(build_sql_in_clause("facility", NULL), "")
  expect_equal(build_sql_in_clause("facility", "all"), "")
  expect_equal(build_sql_in_clause("facility", character(0)), "")
})

test_that("build_sql_in_clause builds correct clause", {
  expect_equal(
    build_sql_in_clause("facility", c("E", "W")),
    "AND facility IN ('E', 'W')"
  )
})

test_that("build_sql_in_clause respects prefix parameter", {
  expect_equal(
    build_sql_in_clause("facility", c("E"), prefix = "WHERE "),
    "WHERE facility IN ('E')"
  )
  expect_equal(
    build_sql_in_clause("facility", c("E"), prefix = ""),
    "facility IN ('E')"
  )
})

test_that("build_sql_in_clause handles qualified column names", {
  expect_equal(
    build_sql_in_clause("gis.facility", c("E", "W")),
    "AND gis.facility IN ('E', 'W')"
  )
})

# =============================================================================
# build_sql_equals_clause() tests
# =============================================================================

test_that("build_sql_equals_clause returns empty for NULL", {
  expect_equal(build_sql_equals_clause("zone", NULL), "")
})

test_that("build_sql_equals_clause returns empty for empty string", {
  expect_equal(build_sql_equals_clause("zone", ""), "")
})

test_that("build_sql_equals_clause builds correct clause", {
  expect_equal(
    build_sql_equals_clause("zone", "1"),
    "AND zone = '1'"
  )
})

test_that("build_sql_equals_clause respects prefix", {
  expect_equal(
    build_sql_equals_clause("zone", "1", prefix = "WHERE "),
    "WHERE zone = '1'"
  )
})

# =============================================================================
# normalize_filter_input() tests
# =============================================================================

test_that("normalize_filter_input returns NULL for NULL input", {
  expect_null(normalize_filter_input(NULL))
})

test_that("normalize_filter_input returns NULL for empty input", {
  expect_null(normalize_filter_input(character(0)))
})

test_that("normalize_filter_input returns NULL when 'all' selected", {
  expect_null(normalize_filter_input("all"))
  expect_null(normalize_filter_input("All"))
  expect_null(normalize_filter_input(c("all", "E")))
})

test_that("normalize_filter_input returns values when valid", {
  expect_equal(normalize_filter_input("E"), "E")
  expect_equal(normalize_filter_input(c("E", "W")), c("E", "W"))
})
