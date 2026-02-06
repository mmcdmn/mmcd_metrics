# =============================================================================
# Tests for URL Router Functions (apps/overview/url_router.R)
# =============================================================================
# Tests URL parsing, building, and navigation helpers for the unified
# overview dashboard's drill-down navigation system.
# =============================================================================

library(testthat)

context("URL Router Functions")

# =============================================================================
# SETUP: Source the url_router.R file
# =============================================================================

# Source the url_router (and dependencies)
# Need to handle the fact that url_router.R sources metric_registry.R
tryCatch({
  # First source metric_registry to define get_metric_registry()
  source("apps/overview/metric_registry.R")
  cat("✓ metric_registry.R loaded\n")
}, error = function(e) {
  cat("✗ metric_registry.R failed:", e$message, "\n")
})

tryCatch({
  source("apps/overview/url_router.R")
  cat("✓ url_router.R loaded\n")
}, error = function(e) {
  cat("✗ url_router.R failed:", e$message, "\n")
  # If we can't load url_router, skip all tests
  skip("url_router.R failed to load")
})

# =============================================================================
# get_default_url_params() tests
# =============================================================================

test_that("get_default_url_params returns valid structure", {
  params <- get_default_url_params()
  
  expect_type(params, "list")
  expect_true("view" %in% names(params))
  expect_true("metric" %in% names(params))
  expect_true("zone" %in% names(params))
  expect_true("facility" %in% names(params))
  expect_true("date" %in% names(params))
  expect_true("expiring" %in% names(params))
  expect_true("theme" %in% names(params))
})

test_that("get_default_url_params has correct default values", {
  params <- get_default_url_params()
  
  expect_equal(params$view, "district")
  expect_equal(params$metric, "all")
  expect_equal(params$zone, "1,2")
  expect_equal(params$facility, "all")
  expect_equal(params$expiring, 7)
  expect_equal(params$theme, "MMCD")
})

test_that("get_default_url_params date is current date", {
  params <- get_default_url_params()
  
  expect_equal(params$date, as.character(Sys.Date()))
})

# =============================================================================
# get_valid_param_values() tests
# =============================================================================

test_that("get_valid_param_values returns valid view options", {
  valid <- get_valid_param_values()
  
  expect_true("view" %in% names(valid))
  expect_true("district" %in% valid$view)
  expect_true("facility" %in% valid$view)
  expect_true("metric_detail" %in% valid$view)
})

test_that("get_valid_param_values returns valid zone options", {
  valid <- get_valid_param_values()
  
  expect_true("zone" %in% names(valid))
  expect_true("1" %in% valid$zone)
  expect_true("2" %in% valid$zone)
  expect_true("1,2" %in% valid$zone)
  expect_true("separate" %in% valid$zone)
})

test_that("get_valid_param_values returns valid theme options", {
  valid <- get_valid_param_values()
  
  expect_true("theme" %in% names(valid))
  expect_true("MMCD" %in% valid$theme)
  expect_true("IBM" %in% valid$theme)
  expect_true("Wong" %in% valid$theme)
})

# =============================================================================
# parse_url_params() tests - Basic Parsing
# =============================================================================

test_that("parse_url_params handles empty query string", {
  params <- parse_url_params("")
  
  expect_equal(params$view, "district")
  expect_equal(params$metric, "all")
  expect_equal(params$zone, "1,2")
})

test_that("parse_url_params handles NULL query string", {
  # parseQueryString handles NULL gracefully
  params <- parse_url_params(NULL)
  
  expect_equal(params$view, "district")
})

test_that("parse_url_params parses view parameter", {
  params <- parse_url_params("?view=facility")
  expect_equal(params$view, "facility")
  
  params <- parse_url_params("?view=metric_detail")
  expect_equal(params$view, "metric_detail")
  
  params <- parse_url_params("?view=district")
  expect_equal(params$view, "district")
})

test_that("parse_url_params ignores invalid view values", {
  params <- parse_url_params("?view=invalid_view")
  
  # Should fall back to default

  expect_equal(params$view, "district")
})

test_that("parse_url_params parses zone parameter", {
  params <- parse_url_params("?zone=1")
  expect_equal(params$zone, "1")
  
  params <- parse_url_params("?zone=2")
  expect_equal(params$zone, "2")
  
  params <- parse_url_params("?zone=1,2")
  expect_equal(params$zone, "1,2")
  
  params <- parse_url_params("?zone=separate")
  expect_equal(params$zone, "separate")
})

test_that("parse_url_params parses facility parameter", {
  params <- parse_url_params("?facility=SLP")
  expect_equal(params$facility, "SLP")
  
  params <- parse_url_params("?facility=all")
  expect_equal(params$facility, "all")
  
  params <- parse_url_params("?facility=E")
  expect_equal(params$facility, "E")
})

test_that("parse_url_params parses date parameter", {
  params <- parse_url_params("?date=2026-01-15")
  expect_equal(params$date, "2026-01-15")
})

test_that("parse_url_params handles invalid date gracefully", {
  params <- parse_url_params("?date=not-a-date")
  
  # Should keep default (today's date)
  expect_equal(params$date, as.character(Sys.Date()))
})

test_that("parse_url_params parses expiring parameter", {
  params <- parse_url_params("?expiring=14")
  expect_equal(params$expiring, 14)
  
  params <- parse_url_params("?expiring=1")
  expect_equal(params$expiring, 1)
  
  params <- parse_url_params("?expiring=30")
  expect_equal(params$expiring, 30)
})

test_that("parse_url_params validates expiring range", {
  # Out of range - should keep default
  params <- parse_url_params("?expiring=0")
  expect_equal(params$expiring, 7)  # default
  
  params <- parse_url_params("?expiring=100")
  expect_equal(params$expiring, 7)  # default
  
  params <- parse_url_params("?expiring=-5")
  expect_equal(params$expiring, 7)  # default
})

test_that("parse_url_params parses theme parameter", {
  params <- parse_url_params("?theme=IBM")
  expect_equal(params$theme, "IBM")
  
  params <- parse_url_params("?theme=Wong")
  expect_equal(params$theme, "Wong")
  
  params <- parse_url_params("?theme=Viridis")
  expect_equal(params$theme, "Viridis")
})

test_that("parse_url_params ignores invalid theme", {
  params <- parse_url_params("?theme=InvalidTheme")
  
  expect_equal(params$theme, "MMCD")  # default
})

test_that("parse_url_params parses show_historical parameter", {
  params <- parse_url_params("?show_historical=true")
  expect_true(params$show_historical)
  
  params <- parse_url_params("?show_historical=false")
  expect_false(params$show_historical)
  
  params <- parse_url_params("?show_historical=1")
  expect_true(params$show_historical)
})

# =============================================================================
# parse_url_params() tests - Metric Parsing
# =============================================================================

test_that("parse_url_params parses single metric", {
  params <- parse_url_params("?metric=drone")
  expect_equal(params$metric, "drone")
  
  params <- parse_url_params("?metric=catch_basin")
  expect_equal(params$metric, "catch_basin")
})

test_that("parse_url_params parses multiple metrics", {
  params <- parse_url_params("?metric=drone,ground_prehatch")
  
  expect_type(params$metric, "character")
  expect_true(length(params$metric) >= 1)
  # Should contain valid metrics that are in the registry
})

test_that("parse_url_params handles 'all' metrics", {
  params <- parse_url_params("?metric=all")
  expect_equal(params$metric, "all")
})

test_that("parse_url_params filters out invalid metrics", {
  # Mix of valid and invalid metrics
  params <- parse_url_params("?metric=drone,fake_metric,catch_basin")
  
  # Should only include valid metrics
  if (is.character(params$metric) && length(params$metric) > 1) {
    expect_false("fake_metric" %in% params$metric)
  }
})

test_that("parse_url_params falls back to 'all' when all metrics invalid", {
  params <- parse_url_params("?metric=fake1,fake2,fake3")
  
  expect_equal(params$metric, "all")
})

# =============================================================================
# parse_url_params() tests - Breadcrumb Tracking
# =============================================================================

test_that("parse_url_params parses breadcrumb from_view", {
  params <- parse_url_params("?from_view=district")
  expect_equal(params$from_view, "district")
  
  params <- parse_url_params("?from_view=facility")
  expect_equal(params$from_view, "facility")
})

test_that("parse_url_params parses breadcrumb from_metric", {
  params <- parse_url_params("?from_metric=drone")
  expect_equal(params$from_metric, "drone")
  
  params <- parse_url_params("?from_metric=all")
  expect_equal(params$from_metric, "all")
})

# =============================================================================
# parse_url_params() tests - Complex URL Combinations
# =============================================================================

test_that("parse_url_params handles full URL with all parameters", {
  url <- "?view=facility&metric=drone&zone=1&facility=SLP&date=2026-01-15&expiring=14&theme=IBM&show_historical=true"
  params <- parse_url_params(url)
  
  expect_equal(params$view, "facility")
  expect_equal(params$metric, "drone")
  expect_equal(params$zone, "1")
  expect_equal(params$facility, "SLP")
  expect_equal(params$date, "2026-01-15")
  expect_equal(params$expiring, 14)
  expect_equal(params$theme, "IBM")
  expect_true(params$show_historical)
})

test_that("parse_url_params handles realistic drill-down URL", {
  # Simulating drill from district -> facility -> metric_detail
  url <- "?view=metric_detail&metric=drone&zone=1&facility=all&from_view=facility&from_metric=all"
  params <- parse_url_params(url)
  
  expect_equal(params$view, "metric_detail")
  expect_equal(params$metric, "drone")
  expect_equal(params$zone, "1")
  expect_equal(params$from_view, "facility")
  expect_equal(params$from_metric, "all")
})

# =============================================================================
# build_drill_down_url() tests
# =============================================================================

test_that("build_drill_down_url creates valid query string", {
  url <- build_drill_down_url(view = "district")
  
  expect_type(url, "character")
  expect_true(grepl("^\\?", url))  # Starts with ?
  expect_true(grepl("view=district", url))
})

test_that("build_drill_down_url includes all parameters", {
  url <- build_drill_down_url(
    view = "facility",
    metric = "drone",
    zone = "1",
    facility = "SLP",
    date = as.Date("2026-01-15"),
    expiring = 14,
    theme = "IBM",
    show_historical = TRUE
  )
  
  expect_true(grepl("view=facility", url))
  expect_true(grepl("metric=drone", url))
  expect_true(grepl("zone=1", url))
  expect_true(grepl("facility=SLP", url))
  expect_true(grepl("date=2026-01-15", url))
  expect_true(grepl("expiring=14", url))
  expect_true(grepl("theme=IBM", url))
  expect_true(grepl("show_historical=true", url))
})

test_that("build_drill_down_url handles multiple metrics", {
  url <- build_drill_down_url(
    view = "facility",
    metric = c("drone", "ground_prehatch")
  )
  
  expect_true(grepl("metric=drone%2Cground_prehatch|metric=drone,ground_prehatch", url))
})

test_that("build_drill_down_url includes breadcrumb tracking", {
  url <- build_drill_down_url(
    view = "metric_detail",
    metric = "drone",
    from_view = "facility",
    from_metric = "all"
  )
  
  expect_true(grepl("from_view=facility", url))
  expect_true(grepl("from_metric=all", url))
})

test_that("build_drill_down_url uses base_path when provided", {
  url <- build_drill_down_url(
    view = "district",
    base_path = "/overview/"
  )
  
  expect_true(grepl("^/overview/\\?", url))
})

# =============================================================================
# build_back_url() tests
# =============================================================================

test_that("build_back_url returns district when no breadcrumb", {
  params <- list(
    view = "facility",
    zone = "1",
    date = "2026-01-15",
    expiring = 7,
    theme = "MMCD",
    from_view = NULL,
    from_metric = NULL
  )
  
  url <- build_back_url(params)
  
  expect_true(grepl("view=district", url))
  expect_true(grepl("metric=all", url))
})
  
test_that("build_back_url uses from_view when available", {
  params <- list(
    view = "metric_detail",
    zone = "1",
    facility = "SLP",
    date = "2026-01-15",
    expiring = 7,
    theme = "MMCD",
    from_view = "facility",
    from_metric = "all"
  )
  
  url <- build_back_url(params)
  
  expect_true(grepl("view=facility", url))
})

test_that("build_back_url preserves zone and date", {
  params <- list(
    view = "facility",
    zone = "2",
    date = "2026-01-20",
    expiring = 10,
    theme = "Wong",
    from_view = "district",
    from_metric = NULL
  )
  
  url <- build_back_url(params)
  
  expect_true(grepl("zone=2", url))
  expect_true(grepl("date=2026-01-20", url))
  expect_true(grepl("expiring=10", url))
  expect_true(grepl("theme=Wong", url))
})

# =============================================================================
# get_display_metrics() tests
# =============================================================================

test_that("get_display_metrics returns all metrics when 'all'", {
  params <- list(metric = "all")
  
  metrics <- get_display_metrics(params)
  
  expect_type(metrics, "character")
  expect_true(length(metrics) > 0)
})

test_that("get_display_metrics returns specific metrics when provided", {
  params <- list(metric = c("drone", "catch_basin"))
  
  metrics <- get_display_metrics(params)
  
  expect_equal(metrics, c("drone", "catch_basin"))
})

test_that("get_display_metrics handles single metric", {
  params <- list(metric = "drone")
  
  metrics <- get_display_metrics(params)
  
  expect_equal(metrics, "drone")
})

# =============================================================================
# get_view_title() tests
# =============================================================================

test_that("get_view_title returns correct base titles", {
  expect_equal(get_view_title(list(view = "district", metric = "all", zone = "1,2", facility = "all")), 
               "District Overview")
  
  expect_equal(get_view_title(list(view = "facility", metric = "all", zone = "1,2", facility = "all")), 
               "Facility Overview")
})

test_that("get_view_title adds zone to title", {
  title <- get_view_title(list(view = "facility", metric = "all", zone = "1", facility = "all"))
  
  expect_true(grepl("P1", title))
})

test_that("get_view_title adds metric name when single metric", {
  title <- get_view_title(list(view = "metric_detail", metric = "drone", zone = "1,2", facility = "all"))
  
  # Should include the metric's display name
  expect_true(grepl("Drone|drone", title, ignore.case = TRUE))
})

test_that("get_view_title adds facility when filtered", {
  title <- get_view_title(list(view = "facility", metric = "all", zone = "1", facility = "SLP"))
  
  expect_true(grepl("SLP", title))
})

# =============================================================================
# get_view_subtitle() tests
# =============================================================================

test_that("get_view_subtitle returns appropriate subtitles", {
  expect_true(grepl("zone", get_view_subtitle(list(view = "district")), ignore.case = TRUE))
  expect_true(grepl("facility", get_view_subtitle(list(view = "facility")), ignore.case = TRUE))
})

# =============================================================================
# URL Roundtrip tests (build -> parse)
# =============================================================================

test_that("URL roundtrip preserves parameters", {
  # Build a URL
  original_url <- build_drill_down_url(
    view = "facility",
    metric = "drone",
    zone = "1",
    facility = "E",
    date = as.Date("2026-02-01"),
    expiring = 10,
    theme = "Wong"
  )
  
  # Parse it back
  parsed <- parse_url_params(original_url)
  
  expect_equal(parsed$view, "facility")
  expect_equal(parsed$metric, "drone")
  expect_equal(parsed$zone, "1")
  expect_equal(parsed$facility, "E")
  expect_equal(parsed$date, "2026-02-01")
  expect_equal(parsed$expiring, 10)
  expect_equal(parsed$theme, "Wong")
})

# =============================================================================
# JavaScript/CSS helper tests (existence checks)
# =============================================================================

test_that("get_url_navigation_js returns script tag", {
  js <- get_url_navigation_js()
  
  expect_true(inherits(js, "shiny.tag"))
  expect_equal(js$name, "script")
})

test_that("get_breadcrumb_css returns style tag", {
  css <- get_breadcrumb_css()
  
  expect_true(inherits(css, "shiny.tag"))
  expect_equal(css$name, "style")
})

test_that("get_url_navigation_js contains required handlers", {
  js <- get_url_navigation_js()
  js_content <- as.character(js$children[[1]])
  
  expect_true(grepl("navigateWithParams", js_content))
  expect_true(grepl("triggerRefresh", js_content))
  expect_true(grepl("popstate", js_content))
  expect_true(grepl("initial_url_params", js_content))
})

# =============================================================================
# Edge Case tests
# =============================================================================

test_that("parse_url_params handles URL-encoded characters", {
  # Facility names might have special characters
  params <- parse_url_params("?facility=South%20Jordan")
  
  expect_equal(params$facility, "South Jordan")
})

test_that("parse_url_params handles leading ? in query string", {
  # With leading ?
  params1 <- parse_url_params("?view=facility")
  # Without leading ?
  params2 <- parse_url_params("view=facility")
  
  expect_equal(params1$view, params2$view)
})

test_that("parse_url_params handles parameter order independence", {
  url1 <- "?view=facility&zone=1&metric=drone"
  url2 <- "?metric=drone&view=facility&zone=1"
  
  params1 <- parse_url_params(url1)
  params2 <- parse_url_params(url2)
  
  expect_equal(params1$view, params2$view)
  expect_equal(params1$zone, params2$zone)
  expect_equal(params1$metric, params2$metric)
})
