# =============================================================================
# TEST: catch_basin_status app
# =============================================================================
# Tests for Catch Basin Status app data and display functions
# Uses stub data from tests/stubs/ based on real database schema
# =============================================================================

# Helper to get project root (tests run from tests/apps or project root)
get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

test_that("process_catch_basin_data handles empty data", {
  root <- get_project_root()
  source(file.path(root, "apps/catch_basin_status/data_functions.R"), local = TRUE)
  
  result <- process_catch_basin_data(NULL)
  expect_equal(nrow(result), 0)
  
  result <- process_catch_basin_data(data.frame())
  expect_equal(nrow(result), 0)
})

test_that("summary-by-facility route rolls up load_raw_data sections by facility", {
  # Regression: the route passed load_raw_data()'s list to a data-frame function and
  # returned 400 on every call. Evaluates the real route with the loader stubbed.
  route_file <- file.path(get_project_root(), "api/routes/catch_basins.R")
  exprs <- parse(route_file, keep.source = TRUE)
  first_lines <- vapply(attr(exprs, "srcref"), function(s) as.integer(s[1]), integer(1))
  route_line <- grep("@get /summary-by-facility", readLines(route_file))
  helper <- Filter(function(e) length(e) > 1 && identical(e[[2]], as.name(".cb_group_rows")), as.list(exprs))[[1]]

  env <- new.env(parent = globalenv())
  eval(helper, env)
  env$validate_zone <- function(zone) c("1", "2")
  env$validate_date <- function(d) as.Date("2026-07-01")
  env$CB_EXPIRING_DEFAULT <- 7L
  env$api_error <- function(res, status, message) stop(message)
  env$cb_env <- list(load_raw_data = function(expiring_days, ...) {
    expect_equal(expiring_days, 7L)
    list(treatments = data.frame(), total_count = 3, sites = data.frame(
      facility = c("E", "E", "N"), facility_full = c("East", "East", "North"),
      sectcode = c("1904A", "1904B", "2001A"), fosarea = c("1", "1", "2"),
      foreman_name = c("A", "A", "B"),
      total_count = c(10L, 10L, 5L), active_count = c(5L, 5L, 0L),
      expiring_count = c(1L, 1L, 0L), expired_count = c(2L, 2L, 5L)
    ))
  })
  route <- eval(exprs[[which(first_lines > route_line)[1]]], env)

  result <- route(req = list(), res = list())
  expect_equal(result$analysis_date, "2026-07-01")
  expect_equal(vapply(result$facility_summaries, `[[`, "", "facility"), c("East", "North"))
  east <- result$facility_summaries[[1]]
  expect_equal(c(east$total_count, east$active_count, east$expiring_count, east$expired_count), c(20L, 10L, 2L, 4L))
  expect_equal(east$pct_treated, 50)
})

test_that("process_catch_basin_data aggregates by mmcd_all", {
  root <- get_project_root()
  source(file.path(root, "apps/catch_basin_status/data_functions.R"), local = TRUE)
  
  # Sample data resembling load_catch_basin_data output
  test_data <- data.frame(
    facility = c("N", "N", "Sj"),
    facility_full = c("North", "North", "St. James"),
    zone = c("1", "2", "1"),
    fosarea = c("0204", "0206", "7002"),
    foreman_name = c("John", "Jane", "Mike"),
    sectcode = c("020125-", "020207-", "700407-"),
    total_count = c(100L, 150L, 80L),
    active_count = c(80L, 120L, 60L),
    expiring_count = c(10L, 15L, 8L),
    expired_count = c(10L, 15L, 12L),
    stringsAsFactors = FALSE
  )
  
  result <- process_catch_basin_data(test_data, group_by = "mmcd_all")
  
  expect_equal(nrow(result), 1)
  expect_equal(result$display_name, "All MMCD")
  expect_equal(result$total_count, 330)  # 100 + 150 + 80
  expect_equal(result$active_count, 260)  # 80 + 120 + 60
  expect_equal(result$expiring_count, 33)  # 10 + 15 + 8
  expect_equal(result$expired_count, 37)  # 10 + 15 + 12
})

test_that("process_catch_basin_data aggregates by facility with zones separate", {
  root <- get_project_root()
  source(file.path(root, "apps/catch_basin_status/data_functions.R"), local = TRUE)
  
  test_data <- data.frame(
    facility = c("N", "N", "Sj"),
    facility_full = c("North", "North", "St. James"),
    zone = c("1", "2", "1"),
    fosarea = c("0204", "0206", "7002"),
    foreman_name = c("John", "Jane", "Mike"),
    sectcode = c("020125-", "020207-", "700407-"),
    total_count = c(100L, 150L, 80L),
    active_count = c(80L, 120L, 60L),
    expiring_count = c(10L, 15L, 8L),
    expired_count = c(10L, 15L, 12L),
    stringsAsFactors = FALSE
  )
  
  result <- process_catch_basin_data(test_data, group_by = "facility", combine_zones = FALSE)
  
  # Should have 3 rows (N P1, N P2, Sj P1)
  expect_equal(nrow(result), 3)
  expect_true("display_name" %in% names(result))
  expect_true(any(grepl("North P1", result$display_name)))
  expect_true(any(grepl("North P2", result$display_name)))
})

test_that("process_catch_basin_data combines zones when requested", {
  root <- get_project_root()
  source(file.path(root, "apps/catch_basin_status/data_functions.R"), local = TRUE)
  
  test_data <- data.frame(
    facility = c("N", "N", "Sj"),
    facility_full = c("North", "North", "St. James"),
    zone = c("1", "2", "1"),
    fosarea = c("0204", "0206", "7002"),
    foreman_name = c("John", "Jane", "Mike"),
    sectcode = c("020125-", "020207-", "700407-"),
    total_count = c(100L, 150L, 80L),
    active_count = c(80L, 120L, 60L),
    expiring_count = c(10L, 15L, 8L),
    expired_count = c(10L, 15L, 12L),
    stringsAsFactors = FALSE
  )
  
  result <- process_catch_basin_data(test_data, group_by = "facility", combine_zones = TRUE)
  
  # Should have 2 rows (N, Sj) with zones combined
  expect_equal(nrow(result), 2)
  
  north_row <- result[result$facility == "N", ]
  expect_equal(north_row$total_count, 250)  # 100 + 150
  expect_equal(north_row$active_count, 200)  # 80 + 120
})

test_that("process_catch_basin_data filters by expiring status", {
  root <- get_project_root()
  source(file.path(root, "apps/catch_basin_status/data_functions.R"), local = TRUE)
  
  test_data <- data.frame(
    facility = c("N", "N", "Sj"),
    facility_full = c("North", "North", "St. James"),
    zone = c("1", "2", "1"),
    fosarea = c("0204", "0206", "7002"),
    foreman_name = c("John", "Jane", "Mike"),
    sectcode = c("020125-", "020207-", "700407-"),
    total_count = c(100L, 150L, 80L),
    active_count = c(80L, 120L, 60L),
    expiring_count = c(0L, 15L, 8L),  # First row has no expiring
    expired_count = c(10L, 0L, 12L),  # Second row has no expired
    stringsAsFactors = FALSE
  )
  
  # Filter for expiring only
  result <- process_catch_basin_data(test_data, expiring_filter = "expiring")
  expect_equal(nrow(result), 2)  # Rows 2 and 3 have expiring_count > 0
  
  # Filter for expiring or expired
  result <- process_catch_basin_data(test_data, expiring_filter = "expiring_expired")
  expect_equal(nrow(result), 3)  # All rows have either expiring or expired
})

test_that("display functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/catch_basin_status/display_functions.R"), local = TRUE)
  })
})

test_that("ui_helper functions can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/catch_basin_status/ui_helper.R"), local = TRUE)
  })
})

test_that("historical_functions can be sourced without errors", {
  skip_if_not_installed("plotly")
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/catch_basin_status/historical_functions.R"), local = TRUE)
  })
})
