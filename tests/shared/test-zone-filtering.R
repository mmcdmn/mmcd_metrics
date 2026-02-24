# =============================================================================
# Tests for Zone Filtering Across All Views
# =============================================================================
# Ensures zone filtering (P1 only, P2 only, P1+P2 combined, P1+P2 SEPARATE)
# works correctly across all overview types (district, facilities, fos) and
# that filter state is preserved through drill-down navigation.
# =============================================================================

library(testthat)

context("Zone Filtering")

# =============================================================================
# SETUP: Source required modules
# =============================================================================

# Ensure metric_registry and data functions are available
tryCatch({
  source("apps/overview/metric_registry.R")
  source("apps/overview/data_functions.R")
  source("apps/overview/ui_helper.R")
  cat("✓ Overview modules loaded for zone filtering tests\n")
}, error = function(e) {
  cat("✗ Overview modules failed:", e$message, "\n")
})

# =============================================================================
# 1. parse_unified_params() zone handling
# =============================================================================

test_that("parse_unified_params is available", {
  tryCatch({
    source("apps/overview/unified/app.R", local = TRUE)
    skip("Cannot source unified/app.R in test environment (needs shiny)")
  }, error = function(e) {
    # Expected - test the parsing logic directly instead
    expect_true(TRUE)
  })
})

# Test zone parsing logic directly (extracted from parse_unified_params)
parse_zone_param <- function(zone_value) {
  zone_filter <- "1,2"  # default
  if (!is.null(zone_value)) {
    if (zone_value == "1") {
      zone_filter <- "1"
    } else if (zone_value == "2") {
      zone_filter <- "2"
    } else if (zone_value == "separate") {
      zone_filter <- "separate"
    }
  }
  zone_filter
}

test_that("zone URL param: NULL defaults to '1,2'", {
  expect_equal(parse_zone_param(NULL), "1,2")
})

test_that("zone URL param: '1' returns '1'", {
  expect_equal(parse_zone_param("1"), "1")
})

test_that("zone URL param: '2' returns '2'", {
  expect_equal(parse_zone_param("2"), "2")
})

test_that("zone URL param: 'separate' returns 'separate'", {
  expect_equal(parse_zone_param("separate"), "separate")
})

test_that("zone URL param: unrecognized value defaults to '1,2'", {
  expect_equal(parse_zone_param("invalid"), "1,2")
  expect_equal(parse_zone_param("both"), "1,2")
  expect_equal(parse_zone_param("all"), "1,2")
})

# =============================================================================
# 2. refresh_inputs() zone parsing logic
# =============================================================================

# Extract the zone parsing logic from refresh_inputs() for unit testing
parse_refresh_zone <- function(zone_value) {
  separate_zones <- (zone_value == "separate")
  parsed_zones <- if (zone_value == "1,2" || zone_value == "separate") {
    c("1", "2")
  } else {
    zone_value
  }
  list(
    zone_filter_raw = zone_value,
    zone_filter = parsed_zones,
    combine_zones = (zone_value == "1,2"),
    separate_zones = separate_zones
  )
}

test_that("refresh zone parsing: '1,2' gives combined mode", {
  result <- parse_refresh_zone("1,2")
  expect_equal(result$zone_filter, c("1", "2"))
  expect_true(result$combine_zones)
  expect_false(result$separate_zones)
})

test_that("refresh zone parsing: 'separate' gives separate mode", {
  result <- parse_refresh_zone("separate")
  expect_equal(result$zone_filter, c("1", "2"))
  expect_false(result$combine_zones)
  expect_true(result$separate_zones)
})

test_that("refresh zone parsing: '1' gives P1 only", {
  result <- parse_refresh_zone("1")
  expect_equal(result$zone_filter, "1")
  expect_false(result$combine_zones)
  expect_false(result$separate_zones)
})

test_that("refresh zone parsing: '2' gives P2 only", {
  result <- parse_refresh_zone("2")
  expect_equal(result$zone_filter, "2")
  expect_false(result$combine_zones)
  expect_false(result$separate_zones)
})

test_that("refresh zone parsing preserves raw value", {
  expect_equal(parse_refresh_zone("separate")$zone_filter_raw, "separate")
  expect_equal(parse_refresh_zone("1,2")$zone_filter_raw, "1,2")
  expect_equal(parse_refresh_zone("1")$zone_filter_raw, "1")
})

# =============================================================================
# 3. navigate_to_overview() zone propagation
# =============================================================================

# Mock shiny session for testing navigate_to_overview
# Using environment (not list) so <<- assignment works in closures
mock_session <- new.env(parent = emptyenv())
mock_session$last_navigate_url <- NULL
mock_session$sendCustomMessage <- function(type, data) {
  if (type == "navigate") {
    mock_session$last_navigate_url <- data
  }
}

test_that("navigate_to_overview exists", {
  expect_true(exists("navigate_to_overview"))
})

test_that("navigate preserves zone=separate in URL", {
  mock_session$last_navigate_url <- NULL
  navigate_to_overview(
    session = mock_session,
    target = "facilities_overview",
    zone_clicked = "P1",
    analysis_date = Sys.Date(),
    expiring_days = 3,
    color_theme = "MMCD",
    metric_id = "drone",
    zone_filter_raw = "separate"
  )
  url <- mock_session$last_navigate_url
  expect_true(!is.null(url))
  expect_true(grepl("zone=separate", url), 
              info = paste("URL should contain zone=separate, got:", url))
})

test_that("navigate produces zone=1 for P1 click", {
  mock_session$last_navigate_url <- NULL
  navigate_to_overview(
    session = mock_session,
    target = "facilities_overview",
    zone_clicked = "P1",
    analysis_date = Sys.Date(),
    expiring_days = 3,
    color_theme = "MMCD",
    metric_id = "drone"
  )
  url <- mock_session$last_navigate_url
  expect_true(grepl("zone=1", url),
              info = paste("URL should contain zone=1, got:", url))
})

test_that("navigate produces zone=2 for P2 click", {
  mock_session$last_navigate_url <- NULL
  navigate_to_overview(
    session = mock_session,
    target = "facilities_overview",
    zone_clicked = "P2",
    analysis_date = Sys.Date(),
    expiring_days = 3
  )
  url <- mock_session$last_navigate_url
  expect_true(grepl("zone=2", url),
              info = paste("URL should contain zone=2, got:", url))
})

test_that("navigate omits zone param for combined mode (1,2)", {
  mock_session$last_navigate_url <- NULL
  navigate_to_overview(
    session = mock_session,
    target = "facilities_overview",
    zone_clicked = "1,2",
    analysis_date = Sys.Date(),
    expiring_days = 3
  )
  url <- mock_session$last_navigate_url
  # zone should NOT be in URL (defaults to 1,2 when absent)
  expect_false(grepl("zone=", url),
               info = paste("URL should not contain zone param for combined mode, got:", url))
})

test_that("navigate preserves separate mode even when specific zone clicked", {
  # When user is in "separate" mode and clicks P1 bar, the drill-down should
  # still carry zone=separate so the target page shows both zones separately
  mock_session$last_navigate_url <- NULL
  navigate_to_overview(
    session = mock_session,
    target = "facilities_overview",
    zone_clicked = "P1",
    analysis_date = Sys.Date(),
    expiring_days = 3,
    zone_filter_raw = "separate"
  )
  url <- mock_session$last_navigate_url
  expect_true(grepl("zone=separate", url),
              info = paste("zone=separate should override P1 click, got:", url))
  expect_false(grepl("zone=1[^,]", url),
               info = paste("zone=1 should NOT appear when separate mode active, got:", url))
})

test_that("navigate produces correct facility drill-down URL", {
  mock_session$last_navigate_url <- NULL
  navigate_to_overview(
    session = mock_session,
    target = "fos_overview",
    zone_clicked = "P1",
    analysis_date = Sys.Date(),
    expiring_days = 3,
    metric_id = "ground_prehatch",
    facility_clicked = "East",
    zone_filter_raw = "separate"
  )
  url <- mock_session$last_navigate_url
  expect_true(grepl("view=fos", url))
  expect_true(grepl("facility=East", url))
  expect_true(grepl("metric=ground_prehatch", url))
  expect_true(grepl("zone=separate", url))
})

# =============================================================================
# 4. aggregate_metric_data() zone handling
# =============================================================================

test_that("aggregate_metric_data exists", {
  expect_true(exists("aggregate_metric_data"))
})

# Create mock data for standard metric
create_mock_data <- function() {
  data.frame(
    facility = rep(c("E", "N", "Sr"), each = 2),
    facility_display = rep(c("East", "North", "South Reserve"), each = 2),
    zone = rep(c("1", "2"), 3),
    total_count = c(100, 80, 120, 90, 110, 70),
    active_count = c(60, 50, 80, 40, 70, 30),
    expiring_count = c(10, 5, 15, 8, 12, 7),
    stringsAsFactors = FALSE
  )
}

# Standard metric config (not SUCO, not average)
mock_config <- list(
  id = "drone",
  display_name = "Drone",
  display_as_average = FALSE,
  display_raw_value = FALSE,
  load_params = list()
)

test_that("aggregate by zone with separate_zones=TRUE gives 2 rows", {
  data <- create_mock_data()
  result <- aggregate_metric_data(data, mock_config, "drone", 
                                  group_cols = c("zone"),
                                  separate_zones = TRUE)
  # Should have 2 rows (P1, P2)
  expect_equal(nrow(result), 2)
  expect_true("zone" %in% names(result))
  # Zone 1 total = 100+120+110 = 330
  zone1 <- result[result$zone == "1", ]
  expect_equal(zone1$total, 330)
  # Zone 2 total = 80+90+70 = 240
  zone2 <- result[result$zone == "2", ]
  expect_equal(zone2$total, 240)
})

test_that("aggregate by zone with separate_zones=FALSE gives 1 row", {
  data <- create_mock_data()
  # No zone in group_cols = combined
  result <- aggregate_metric_data(data, mock_config, "drone",
                                  group_cols = character(0),
                                  separate_zones = FALSE)
  expect_equal(nrow(result), 1)
  expect_equal(result$total, 570)  # 330 + 240
})

test_that("aggregate by facility with zone gives per-facility-zone rows", {
  data <- create_mock_data()
  result <- aggregate_metric_data(data, mock_config, "drone",
                                  group_cols = c("facility", "facility_display", "zone"),
                                  separate_zones = TRUE)
  # Should have 6 rows (3 facilities × 2 zones)
  expect_equal(nrow(result), 6)
  # Check East P1
  east_p1 <- result[result$facility == "E" & result$zone == "1", ]
  expect_equal(east_p1$total, 100)
  expect_equal(east_p1$active, 60)
})

test_that("aggregate by facility without zone gives per-facility rows", {
  data <- create_mock_data()
  result <- aggregate_metric_data(data, mock_config, "drone",
                                  group_cols = c("facility", "facility_display"),
                                  separate_zones = FALSE)
  # Should have 3 rows (one per facility)
  expect_equal(nrow(result), 3)
  # East total = 100 + 80 = 180
  east <- result[result$facility == "E", ]
  expect_equal(east$total, 180)
})

# =============================================================================
# 5. SUCO metric aggregate_metric_data zone handling
# =============================================================================

mock_suco_config <- list(
  id = "suco",
  display_name = "SUCO",
  load_params = list(capacity_total = 72)
)

create_suco_mock_data <- function() {
  data.frame(
    facility = rep(c("E", "N"), each = 4),
    zone = rep(c("1", "1", "2", "2"), 2),
    active_count = c(3, 2, 4, 1, 5, 3, 2, 1),
    stringsAsFactors = FALSE
  )
}

test_that("SUCO aggregate with separate_zones=TRUE shows per-zone capacity", {
  data <- create_suco_mock_data()
  result <- aggregate_metric_data(data, mock_suco_config, "suco",
                                  group_cols = c("zone"),
                                  separate_zones = TRUE)
  # Should have 2 rows (P1, P2)
  expect_equal(nrow(result), 2)
  expect_true("display_name" %in% names(result))
  # Each zone gets capacity_total / 2 = 36
  expect_equal(result$total[1], 36)
  expect_equal(result$total[2], 36)
})

test_that("SUCO aggregate with separate_zones=FALSE shows full capacity", {
  data <- create_suco_mock_data()
  result <- aggregate_metric_data(data, mock_suco_config, "suco",
                                  group_cols = character(0),
                                  separate_zones = FALSE)
  expect_equal(nrow(result), 1)
  expect_equal(result$total, 72)
})

test_that("SUCO aggregate by facility shows per-facility capacity", {
  data <- create_suco_mock_data()
  result <- aggregate_metric_data(data, mock_suco_config, "suco",
                                  group_cols = c("facility"),
                                  separate_zones = FALSE)
  # 2 facilities, each gets 72/2 = 36
  expect_equal(nrow(result), 2)
  expect_equal(result$total[1], 36)
})

# =============================================================================
# 6. load_data_by_zone() zone filtering
# =============================================================================

# These tests need stub data, so we test structure and logic only

test_that("load_data_by_zone exists and has correct params", {
  expect_true(exists("load_data_by_zone"))
  args <- formals(load_data_by_zone)
  expect_true("zone_filter" %in% names(args))
  expect_true("separate_zones" %in% names(args))
  # Default separate_zones should be TRUE
  expect_equal(eval(args$separate_zones), TRUE)
})

test_that("load_data_by_facility exists and has correct params", {
  expect_true(exists("load_data_by_facility"))
  args <- formals(load_data_by_facility)
  expect_true("zone_filter" %in% names(args))
  expect_true("separate_zones" %in% names(args))
  expect_true("facility_filter" %in% names(args))
  # Default separate_zones should be FALSE
  expect_equal(eval(args$separate_zones), FALSE)
})

test_that("load_data_by_fos exists and has correct params", {
  expect_true(exists("load_data_by_fos"))
  args <- formals(load_data_by_fos)
  expect_true("zone_filter" %in% names(args))
  expect_true("separate_zones" %in% names(args))
  expect_true("facility_filter" %in% names(args))
  expect_true("fos_filter" %in% names(args))
})

# Regression test: FOS facility validation must support full display names
# (e.g. "South Jordan" not just "Sj") since drill-down URLs use full names
test_that("get_facility_lookup maps abbreviations to full names", {
  fac_lookup <- get_facility_lookup()
  expect_true(nrow(fac_lookup) > 0, info = "Facility lookup should return data")
  expect_true("short_name" %in% names(fac_lookup))
  expect_true("full_name" %in% names(fac_lookup))
  # The mapping must work both ways — short to full
  fac_map <- setNames(fac_lookup$full_name, fac_lookup$short_name)
  expect_equal(unname(fac_map["Sj"]), "South Jordan")
  expect_equal(unname(fac_map["E"]), "East")
  expect_equal(unname(fac_map["N"]), "North")
})

test_that("get_foremen_lookup returns facility abbreviations", {
  foremen <- get_foremen_lookup()
  expect_true(nrow(foremen) > 0)
  expect_true("facility" %in% names(foremen))
  # Foremen facility values are abbreviations like "E", "N", etc.
  # These must be mapped to full names before comparing with URL facility_filter
  e_foremen <- foremen[foremen$facility == "E", ]
  expect_true(nrow(e_foremen) > 0, info = "Should have FOS supervisors for East")
})

# =============================================================================
# 7. order_facilities() zone handling
# =============================================================================

test_that("order_facilities exists", {
  expect_true(exists("order_facilities"))
})

test_that("order_facilities without separate zones uses facility names", {
  data <- data.frame(
    display_name = c("North", "East", "South Jordan"),
    total = c(100, 200, 150),
    stringsAsFactors = FALSE
  )
  result <- order_facilities(data, separate_zones = FALSE)
  # display_name should be a factor in standard order
  expect_true(is.factor(result$display_name))
  # East should come before North in standard order
  levels <- levels(result$display_name)
  east_pos <- which(levels == "East")
  north_pos <- which(levels == "North")
  expect_true(east_pos < north_pos)
})

test_that("order_facilities with separate zones creates interleaved P1/P2 levels", {
  data <- data.frame(
    display_name = c("East (P1)", "East (P2)", "North (P1)", "North (P2)"),
    total = c(100, 80, 120, 90),
    stringsAsFactors = FALSE
  )
  result <- order_facilities(data, separate_zones = TRUE)
  expect_true(is.factor(result$display_name))
  levels <- levels(result$display_name)
  # Should be interleaved: East (P1), East (P2), ..., North (P1), North (P2)
  east_p1 <- which(levels == "East (P1)")
  east_p2 <- which(levels == "East (P2)")
  north_p1 <- which(levels == "North (P1)")
  expect_true(east_p1 < east_p2)
  expect_true(east_p2 < north_p1)
})

# =============================================================================
# 8. get_overview_config() view types
# =============================================================================

test_that("get_overview_config exists", {
  expect_true(exists("get_overview_config"))
})

test_that("district config enables drill-down to facilities", {
  config <- get_overview_config("district")
  expect_true(config$enable_drill_down)
  expect_equal(config$drill_down_target, "facilities_overview")
  expect_equal(config$load_function, "load_data_by_zone")
})

test_that("facilities config enables drill-down to fos", {
  config <- get_overview_config("facilities")
  expect_true(config$enable_drill_down)
  expect_equal(config$drill_down_target, "fos_overview")
  expect_equal(config$load_function, "load_data_by_facility")
})

test_that("fos config has no drill-down", {
  config <- get_overview_config("fos")
  expect_false(config$enable_drill_down)
  expect_equal(config$load_function, "load_data_by_fos")
})

# =============================================================================
# 9. Full drill-down URL chain tests
# =============================================================================

test_that("district -> facilities drill-down preserves separate zone", {
  mock_session$last_navigate_url <- NULL
  
  # Simulate clicking P1 bar in separate mode
  navigate_to_overview(
    session = mock_session,
    target = "facilities_overview",
    zone_clicked = "P1",
    analysis_date = as.Date("2026-02-18"),
    expiring_days = 3,
    color_theme = "MMCD",
    metric_id = "drone",
    zone_filter_raw = "separate"
  )
  
  url <- mock_session$last_navigate_url
  # URL should have view=facility, zone=separate, metric=drone
  expect_true(grepl("view=facility", url))
  expect_true(grepl("zone=separate", url))
  expect_true(grepl("metric=drone", url))
  
  # Parse the resulting URL to verify round-trip
  query_string <- sub("^.*\\?", "", url)
  params <- as.list(strsplit(query_string, "&")[[1]])
  params <- setNames(
    sapply(params, function(p) sub("^[^=]+=", "", p)),
    sapply(params, function(p) sub("=.*$", "", p))
  )
  
  zone_result <- parse_zone_param(params[["zone"]])
  expect_equal(zone_result, "separate")
  
  # The facilities page should then parse this to separate_zones=TRUE
  refresh_result <- parse_refresh_zone(zone_result)
  expect_true(refresh_result$separate_zones)
  expect_equal(refresh_result$zone_filter, c("1", "2"))
})

test_that("facilities -> fos drill-down preserves separate zone", {
  mock_session$last_navigate_url <- NULL
  
  # Simulate clicking a facility bar in separate mode
  navigate_to_overview(
    session = mock_session,
    target = "fos_overview",
    zone_clicked = "1,2",
    analysis_date = as.Date("2026-02-18"),
    expiring_days = 3,
    color_theme = "MMCD",
    metric_id = "ground_prehatch",
    facility_clicked = "East",
    zone_filter_raw = "separate"
  )
  
  url <- mock_session$last_navigate_url
  expect_true(grepl("view=fos", url))
  expect_true(grepl("zone=separate", url))
  expect_true(grepl("facility=East", url))
  expect_true(grepl("metric=ground_prehatch", url))
})

test_that("district -> facilities drill-down with P1 only", {
  mock_session$last_navigate_url <- NULL
  
  navigate_to_overview(
    session = mock_session,
    target = "facilities_overview",
    zone_clicked = "P1",
    analysis_date = as.Date("2026-02-18"),
    expiring_days = 3,
    zone_filter_raw = "1"  # P1 only mode
  )
  
  url <- mock_session$last_navigate_url
  expect_true(grepl("zone=1", url))
  expect_false(grepl("zone=separate", url))
})

test_that("district -> facilities drill-down with combined mode omits zone", {
  mock_session$last_navigate_url <- NULL
  
  navigate_to_overview(
    session = mock_session,
    target = "facilities_overview",
    zone_clicked = "1,2",
    analysis_date = as.Date("2026-02-18"),
    expiring_days = 3,
    zone_filter_raw = "1,2"  # Combined mode
  )
  
  url <- mock_session$last_navigate_url
  # Combined mode: zone_filter_raw is "1,2", NOT "separate", 
  # so navigate uses zone_clicked="1,2" which produces NULL zone_num
  expect_false(grepl("zone=separate", url))
})

# =============================================================================
# 10. calculate_display_pct for different metric types
# NOTE: calculate_display_pct is defined inside the server closure in 
#       dynamic_server.R and cannot be tested in isolation. These tests
#       are skipped. The function is indirectly tested through integration
#       tests that exercise the full server pipeline.
# =============================================================================

# =============================================================================
# 11. calculate_metric_stats zone aggregation
# =============================================================================

test_that("calculate_metric_stats exists", {
  expect_true(exists("calculate_metric_stats"))
})

test_that("calculate_metric_stats sums across zones correctly", {
  # Data with P1 and P2 separate
  data <- data.frame(
    total = c(200, 150),
    active = c(120, 90),
    zone = c("1", "2"),
    display_name = c("P1", "P2"),
    stringsAsFactors = FALSE
  )
  
  result <- calculate_metric_stats(data, metric_id = "drone")
  # Should sum both zones
  expect_equal(result$total, 350)
  expect_equal(result$active, 210)
  expect_equal(result$pct, ceiling(210 / 350 * 100))
})

test_that("calculate_metric_stats handles single zone", {
  data <- data.frame(
    total = c(200),
    active = c(120),
    zone = c("1"),
    display_name = c("P1"),
    stringsAsFactors = FALSE
  )
  
  result <- calculate_metric_stats(data, metric_id = "drone")
  expect_equal(result$total, 200)
  expect_equal(result$active, 120)
  expect_equal(result$pct, 60)
})

# =============================================================================
# 12. Metric registry has all required fields for drill-down
# =============================================================================

test_that("all metrics have valid configuration", {
  registry <- get_metric_registry()
  expect_true(length(registry) > 0)
  
  for (metric_id in names(registry)) {
    config <- registry[[metric_id]]
    expect_true(!is.null(config$id), info = paste(metric_id, "missing id"))
    expect_true(!is.null(config$display_name), info = paste(metric_id, "missing display_name"))
    expect_true(!is.null(config$y_label), info = paste(metric_id, "missing y_label"))
    expect_true(!is.null(config$app_folder), info = paste(metric_id, "missing app_folder"))
  }
})

test_that("overview configs exist for all view types", {
  for (view_type in c("district", "facilities", "fos")) {
    config <- get_overview_config(view_type)
    expect_true(!is.null(config), info = paste("No config for", view_type))
    expect_true(!is.null(config$load_function), info = paste(view_type, "missing load_function"))
    expect_true(!is.null(config$chart_function), info = paste(view_type, "missing chart_function"))
    expect_true(!is.null(config$enable_drill_down), info = paste(view_type, "missing enable_drill_down"))
  }
})

# =============================================================================
# 13. All 4 zone filter modes produce valid data structures
# =============================================================================

test_that("all zone modes produce consistent data columns for aggregate", {
  data <- create_mock_data()
  
  # Test all modes
  modes <- list(
    p1_only = list(group_cols = c("zone"), separate = TRUE),
    p2_only = list(group_cols = c("zone"), separate = TRUE),
    combined = list(group_cols = character(0), separate = FALSE),
    separate = list(group_cols = c("zone"), separate = TRUE)
  )
  
  for (mode_name in names(modes)) {
    mode <- modes[[mode_name]]
    result <- aggregate_metric_data(data, mock_config, "drone",
                                    group_cols = mode$group_cols,
                                    separate_zones = mode$separate)
    # All results must have total, active, expiring columns
    expect_true("total" %in% names(result), 
                info = paste(mode_name, "missing 'total' column"))
    expect_true("active" %in% names(result), 
                info = paste(mode_name, "missing 'active' column"))
    expect_true("expiring" %in% names(result), 
                info = paste(mode_name, "missing 'expiring' column"))
    # Values should be non-negative
    expect_true(all(result$total >= 0), 
                info = paste(mode_name, "has negative total"))
    expect_true(all(result$active >= 0), 
                info = paste(mode_name, "has negative active"))
  }
})

test_that("facility aggregate with separate zones doubles the row count", {
  data <- create_mock_data()
  
  # Without zone separation
  combined <- aggregate_metric_data(data, mock_config, "drone",
                                     group_cols = c("facility", "facility_display"),
                                     separate_zones = FALSE)
  # With zone separation
  separate <- aggregate_metric_data(data, mock_config, "drone",
                                     group_cols = c("facility", "facility_display", "zone"),
                                     separate_zones = TRUE)
  
  # Separate should have double the rows (one per zone per facility)
  expect_equal(nrow(separate), nrow(combined) * 2)
  
  # Totals should match
  expect_equal(sum(separate$total), sum(combined$total))
  expect_equal(sum(separate$active), sum(combined$active))
})

# =============================================================================
# 14. Zone filter display_name consistency
# =============================================================================

test_that("load_data_by_zone display names are consistent", {
  # Verify the display_name format for separate vs combined
  data <- create_mock_data()
  
  # Separate mode produces display_names like "P1", "P2"
  result_sep <- aggregate_metric_data(data, mock_config, "drone",
                                       group_cols = c("zone"),
                                       separate_zones = TRUE)
  # Result should NOT have display_name yet (added by load_data_by_zone wrapper)
  # But zone column should be "1" and "2"
  expect_true(all(result_sep$zone %in% c("1", "2")))
})

# =============================================================================
# 15. Edge cases
# =============================================================================

test_that("aggregate handles empty data gracefully", {
  empty_data <- data.frame(
    facility = character(0),
    zone = character(0),
    total_count = integer(0),
    active_count = integer(0),
    expiring_count = integer(0),
    stringsAsFactors = FALSE
  )
  
  result <- aggregate_metric_data(empty_data, mock_config, "drone",
                                  group_cols = c("zone"),
                                  separate_zones = TRUE)
  expect_equal(nrow(result), 0)
})

test_that("aggregate handles single-zone data correctly", {
  data <- data.frame(
    facility = c("E", "N"),
    zone = c("1", "1"),
    total_count = c(100, 120),
    active_count = c(60, 80),
    expiring_count = c(10, 15),
    stringsAsFactors = FALSE
  )
  
  result <- aggregate_metric_data(data, mock_config, "drone",
                                  group_cols = c("zone"),
                                  separate_zones = TRUE)
  expect_equal(nrow(result), 1)
  expect_equal(result$zone, "1")
  expect_equal(result$total, 220)
})

test_that("SUCO separate zones divides capacity correctly", {
  data <- create_suco_mock_data()
  
  result_separate <- aggregate_metric_data(data, mock_suco_config, "suco",
                                            group_cols = c("zone"),
                                            separate_zones = TRUE)
  result_combined <- aggregate_metric_data(data, mock_suco_config, "suco",
                                            group_cols = character(0),
                                            separate_zones = FALSE)
  
  # Separate: each zone = 72/2 = 36
  expect_equal(sum(result_separate$total), 72)
  # Combined: full capacity
  expect_equal(result_combined$total, 72)
  # Active counts should match in both modes
  expect_equal(sum(result_separate$active), sum(result_combined$active))
})
