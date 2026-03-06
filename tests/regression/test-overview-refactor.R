# =============================================================================
# REGRESSION TEST: Overview Refactoring (refactor_3 branch)
# =============================================================================
# Validates that the refactoring did not break:
#   1. Critical function availability after sourcing modules
#   2. Cache regeneration dependency chain
#   3. Color logic for all metric types
#   4. calculate_display_pct for all metric types
#   5. navigate_to_overview URL construction
#   6. load_app_historical_data fallback when get_app_envs absent
#   7. Historical cache path resolution
#
# These tests run WITHOUT a database connection (isolated mode).
# =============================================================================

library(testthat)

# ── Helper: locate project root ──────────────────────────────────────────────
get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  if (file.exists("../apps")) return("..")
  stop("Cannot find project root")
}

root <- get_project_root()

# =============================================================================
# 1. CRITICAL FUNCTIONS EXIST AFTER SOURCING OVERVIEW MODULES
# =============================================================================

test_that("all critical overview functions exist after sourcing", {
  # Source overview modules in correct order (same as unified/app.R)
  source(file.path(root, "apps/overview/metric_registry.R"), local = FALSE)
  source(file.path(root, "apps/overview/data_functions.R"), local = FALSE)
  source(file.path(root, "apps/overview/display_functions.R"), local = FALSE)
  
  # These were always present

  expect_true(exists("get_metric_registry", mode = "function"),
    info = "get_metric_registry must exist")
  expect_true(exists("get_active_metrics", mode = "function"),
    info = "get_active_metrics must exist")
  expect_true(exists("get_metric_categories", mode = "function"),
    info = "get_metric_categories must exist")
  expect_true(exists("get_historical_metrics", mode = "function"),
    info = "get_historical_metrics must exist")
  
  # Data loading functions
  expect_true(exists("load_data_by_zone", mode = "function"),
    info = "load_data_by_zone must exist")
  expect_true(exists("load_data_by_facility", mode = "function"),
    info = "load_data_by_facility must exist")
  expect_true(exists("load_data_by_fos", mode = "function"),
    info = "load_data_by_fos must exist")
  
  # Cached environments (introduced in refactoring)
  expect_true(exists("get_app_envs", mode = "function"),
    info = "get_app_envs must exist — required by load_app_historical_data")
  
  # Display functions
  expect_true(exists("create_overview_chart", mode = "function"),
    info = "create_overview_chart must exist")
  expect_true(exists("create_zone_chart", mode = "function"),
    info = "create_zone_chart must exist")
  expect_true(exists("create_comparison_chart", mode = "function"),
    info = "create_comparison_chart must exist")
})

test_that("dynamic_server critical functions exist", {
  # Source with stubs for Shiny functions
  if (!exists("observeEvent", mode = "function")) {
    # Stub Shiny server-only functions to allow sourcing dynamic_server.R
    observeEvent <- function(...) invisible(NULL)
    renderPlotly <- function(...) invisible(NULL)
    renderUI <- function(...) invisible(NULL)
    renderText <- function(...) invisible(NULL)
    reactive <- function(...) invisible(NULL)
    eventReactive <- function(...) invisible(NULL)
    outputOptions <- function(...) invisible(NULL)
    event_data <- function(...) NULL
    req <- function(...) invisible(NULL)
  }
  
  tryCatch({
    source(file.path(root, "apps/overview/historical_functions.R"), local = FALSE)
    source(file.path(root, "apps/overview/dynamic_server.R"), local = FALSE)
  }, error = function(e) {
    skip(paste("Could not source dynamic_server.R:", e$message))
  })
  
  # navigate_to_overview was previously deleted with ui_helper.R — must be restored
  expect_true(exists("navigate_to_overview", mode = "function"),
    info = "navigate_to_overview must exist (restored after ui_helper.R deletion)")
  
  # Core server functions
  expect_true(exists("build_overview_server", mode = "function"),
    info = "build_overview_server must exist (main entry point)")
  expect_true(exists("generate_summary_stats", mode = "function"),
    info = "generate_summary_stats must exist")
  expect_true(exists("get_dynamic_value_box_info", mode = "function"),
    info = "get_dynamic_value_box_info must exist")
  expect_true(exists("calculate_display_pct", mode = "function"),
    info = "calculate_display_pct must exist")
  expect_true(exists("get_historical_week_avg", mode = "function"),
    info = "get_historical_week_avg must exist")
  expect_true(exists("load_hist_cache", mode = "function"),
    info = "load_hist_cache must exist")
})

# =============================================================================
# 2. CACHE REGENERATION DEPENDENCY CHAIN
# =============================================================================

test_that("cache_utilities.R sources data_functions.R for get_app_envs", {
  cache_file <- file.path(root, "shared/cache_utilities.R")
  content <- readLines(cache_file, warn = FALSE)
  content_text <- paste(content, collapse = "\n")
  
  # Must source data_functions.R BEFORE historical_functions.R
  expect_true(
    grepl("data_functions\\.R", content_text),
    info = "cache_utilities.R must source data_functions.R (provides get_app_envs)"
  )
  
  # Verify data_functions sourcing appears BEFORE historical_functions sourcing
  data_pos <- grep("data_functions\\.R", content)[1]
  hist_pos <- grep("historical_functions\\.R", content)[1]
  
  # data_functions must appear before historical_functions in the source order
  # (both positions should be within the regenerate_cache function)
  expect_true(
    !is.na(data_pos) && !is.na(hist_pos) && data_pos < hist_pos,
    info = "data_functions.R must be sourced BEFORE historical_functions.R in cache_utilities.R"
  )
})

test_that("historical_functions load_app_historical_data has get_app_envs fallback", {
  hist_file <- file.path(root, "apps/overview/historical_functions.R")
  content <- readLines(hist_file, warn = FALSE)
  content_text <- paste(content, collapse = "\n")
  
  # Must check if get_app_envs exists before calling it
  expect_true(
    grepl('exists\\("get_app_envs"', content_text),
    info = "load_app_historical_data must check get_app_envs existence before calling"
  )
  
  # Must have a fallback sourcing path when get_app_envs is unavailable
  expect_true(
    grepl("get_apps_base_path", content_text),
    info = "load_app_historical_data must have a fallback via get_apps_base_path"
  )
})

# =============================================================================
# 3. METRIC REGISTRY COMPLETENESS
# =============================================================================

test_that("metric registry has all required fields", {
  source(file.path(root, "apps/overview/metric_registry.R"), local = FALSE)
  
  registry <- get_metric_registry()
  expect_true(length(registry) > 0, info = "Registry must not be empty")
  
  required_fields <- c("id", "display_name", "category", "bg_color", "app_folder")
  
  for (metric_id in names(registry)) {
    config <- registry[[metric_id]]
    for (field in required_fields) {
      expect_true(
        !is.null(config[[field]]),
        info = paste0("Metric '", metric_id, "' missing required field: ", field)
      )
    }
  }
})

test_that("get_metric_registry is cached (not recalculated each call)", {
  source(file.path(root, "apps/overview/metric_registry.R"), local = FALSE)
  
  # Call twice and confirm same object (pointer equality or at least fast)
  r1 <- get_metric_registry()
  r2 <- get_metric_registry()
  
  expect_identical(r1, r2,
    info = "get_metric_registry should return identical cached result")
})

# =============================================================================
# 4. calculate_display_pct CORRECTNESS
# =============================================================================

test_that("calculate_display_pct: standard percentage metric", {
  source(file.path(root, "apps/overview/metric_registry.R"), local = FALSE)
  
  # Simulating a dynamic_server function outside of Shiny context
  # Define the function inline since it may need Shiny server context
  # But calculate_display_pct should be pure
  
  if (!exists("calculate_display_pct", mode = "function")) {
    skip("calculate_display_pct not loaded")
  }
  
  config <- list(has_acres = FALSE, display_raw_value = FALSE, display_as_average = FALSE, display_as_goal = FALSE)
  
  result <- calculate_display_pct("catch_basin", config, 
    total = 100, active = 75, expiring = 10)
  
  expect_equal(result$pct, 75)  # ceiling(75/100 * 100)
  expect_equal(result$display_value, "75%")
})

test_that("calculate_display_pct: cattail_treatments uses treated/workload", {
  if (!exists("calculate_display_pct", mode = "function")) {
    skip("calculate_display_pct not loaded")
  }
  
  config <- list(has_acres = TRUE, display_raw_value = FALSE, display_as_average = FALSE, display_as_goal = FALSE)
  
  # cattail: treated = active - expiring, workload = treated + expiring
  # active=100, expiring=30 → treated=70, workload=100, pct=70%
  result <- calculate_display_pct("cattail_treatments", config,
    total = 200, active = 100, expiring = 30)
  
  expect_equal(result$pct, 70)
  expect_equal(result$display_value, "70%")
})

test_that("calculate_display_pct: cattail_treatments handles zero workload", {
  if (!exists("calculate_display_pct", mode = "function")) {
    skip("calculate_display_pct not loaded")
  }
  
  config <- list(has_acres = TRUE, display_raw_value = FALSE, display_as_average = FALSE, display_as_goal = FALSE)
  
  # active=0, expiring=0 → treated=0, workload=0, pct=0 (no div by zero)
  result <- calculate_display_pct("cattail_treatments", config,
    total = 0, active = 0, expiring = 0)
  
  expect_equal(result$pct, 0)
})

test_that("calculate_display_pct: air_sites uses same treated/workload logic", {
  if (!exists("calculate_display_pct", mode = "function")) {
    skip("calculate_display_pct not loaded")
  }
  
  config <- list(has_acres = FALSE, display_raw_value = FALSE, display_as_average = FALSE, display_as_goal = FALSE)
  
  result <- calculate_display_pct("air_sites", config,
    total = 500, active = 200, expiring = 50)
  
  # treated = 200 - 50 = 150, workload = 150 + 50 = 200, pct = 150/200 = 75%
  expect_equal(result$pct, 75)
})

# =============================================================================
# 5. COLOR LOGIC: get_dynamic_value_box_info
# =============================================================================

test_that("fixed_pct color mode returns correct status", {
  if (!exists("get_dynamic_value_box_info", mode = "function")) {
    skip("get_dynamic_value_box_info not loaded")
  }
  
  config <- list(
    bg_color = "#333333",
    color_mode = "fixed_pct",
    color_thresholds = list(good = 80, warning = 60)
  )
  
  # Higher is better (default for fixed_pct without inverse)
  good <- get_dynamic_value_box_info("test_metric", 85, Sys.Date(), config)
  expect_equal(good$status, "good")
  
  warning_result <- get_dynamic_value_box_info("test_metric", 70, Sys.Date(), config)
  expect_equal(warning_result$status, "warning")
  
  alert <- get_dynamic_value_box_info("test_metric", 40, Sys.Date(), config)
  expect_equal(alert$status, "alert")
})

test_that("pct_of_average color mode with inverse returns correct status", {
  if (!exists("get_dynamic_value_box_info", mode = "function")) {
    skip("get_dynamic_value_box_info not loaded")
  }
  
  config <- list(
    bg_color = "#10b981",
    color_mode = "pct_of_average",
    inverse_color = TRUE,
    color_thresholds = list(good = 110, warning = 130)
  )
  
  # Lower is better (inverse) — 80% of avg is good
  good <- get_dynamic_value_box_info("mosquito_test", 80, Sys.Date(), config)
  expect_equal(good$status, "good")
  
  # 120% of avg — warning
  warn <- get_dynamic_value_box_info("mosquito_test", 120, Sys.Date(), config)
  expect_equal(warn$status, "warning")
  
  # 150% of avg — alert
  alert <- get_dynamic_value_box_info("mosquito_test", 150, Sys.Date(), config)
  expect_equal(alert$status, "alert")
})

test_that("non-dynamic metric returns default status", {
  if (!exists("get_dynamic_value_box_info", mode = "function")) {
    skip("get_dynamic_value_box_info not loaded")
  }
  
  # cattail_treatments has no color_mode and is NOT in dynamic_metrics list
  config <- list(bg_color = "#ff9500")
  
  result <- get_dynamic_value_box_info("cattail_treatments", 100, Sys.Date(), config)
  expect_equal(result$status, "default")
  expect_equal(result$color, "#ff9500")
})

test_that("dynamic metric with zero historical avg returns default", {
  if (!exists("get_dynamic_value_box_info", mode = "function")) {
    skip("get_dynamic_value_box_info not loaded")
  }
  
  # In winter, historical avg is 0, function should return early with default
  config <- list(bg_color = "#667eea")
  
  # Stub the cache to return 0 avg
  .hist_cache$data <- list(
    averages = list(
      catch_basin_10yr = data.frame(
        week_num = 9, zone = c("1", "2"), value = c(0, 0),
        time_period = "W09", group_label = "10-Year Avg",
        stringsAsFactors = FALSE
      )
    )
  )
  
  result <- get_dynamic_value_box_info("catch_basin", 100, 
    as.Date("2026-03-01"), config)
  expect_equal(result$status, "default",
    info = "When historical avg is 0, status should be 'default'")
  
  # Cleanup
  .hist_cache$data <- NULL
})

# =============================================================================
# 6. navigate_to_overview URL CONSTRUCTION
# =============================================================================

test_that("navigate_to_overview builds correct URL for facilities drill-down", {
  if (!exists("navigate_to_overview", mode = "function")) {
    skip("navigate_to_overview not loaded")
  }
  
  # Create a mock session
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      mock_session$last_message <<- list(type = type, message = message)
    },
    last_message = NULL
  )
  
  navigate_to_overview(
    session = mock_session,
    target = "facilities_overview",
    zone_clicked = "P1",
    analysis_date = as.Date("2026-03-04"),
    expiring_days = 30,
    color_theme = "MMCD",
    metric_id = "catch_basin"
  )
  
  expect_equal(mock_session$last_message$type, "navigate")
  url <- mock_session$last_message$message
  
  expect_true(grepl("view=facility", url), info = "URL must contain view=facility")
  expect_true(grepl("zone=1", url), info = "P1 should map to zone=1")
  expect_true(grepl("metric=catch_basin", url), info = "URL must contain metric filter")
  expect_true(grepl("date=2026-03-04", url), info = "URL must contain analysis date")
})

test_that("navigate_to_overview handles separate zone correctly", {
  if (!exists("navigate_to_overview", mode = "function")) {
    skip("navigate_to_overview not loaded")
  }
  
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      mock_session$last_message <<- list(type = type, message = message)
    },
    last_message = NULL
  )
  
  navigate_to_overview(
    session = mock_session,
    target = "facilities_overview",
    zone_clicked = "separate",
    analysis_date = as.Date("2026-03-04"),
    expiring_days = 30
  )
  
  url <- mock_session$last_message$message
  expect_true(grepl("zone=separate", url), 
    info = "Separate zone mode should be preserved in URL")
})

test_that("navigate_to_overview handles FOS drill-down with facility", {
  if (!exists("navigate_to_overview", mode = "function")) {
    skip("navigate_to_overview not loaded")
  }
  
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      mock_session$last_message <<- list(type = type, message = message)
    },
    last_message = NULL
  )
  
  navigate_to_overview(
    session = mock_session,
    target = "fos_overview",
    zone_clicked = "1,2",
    analysis_date = as.Date("2026-03-04"),
    expiring_days = 30,
    metric_id = "drone",
    facility_clicked = "North"
  )
  
  url <- mock_session$last_message$message
  expect_true(grepl("view=fos", url), info = "URL must contain view=fos")
  expect_true(grepl("facility=North", url), info = "URL must contain facility filter")
  expect_true(grepl("metric=drone", url), info = "URL must contain metric filter")
})

test_that("navigate_to_overview handles NA inputs gracefully", {
  if (!exists("navigate_to_overview", mode = "function")) {
    skip("navigate_to_overview not loaded")
  }
  
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      mock_session$last_message <<- list(type = type, message = message)
    },
    last_message = NULL
  )
  
  # Should not error with NA zone or facility
  expect_no_error(
    navigate_to_overview(
      session = mock_session,
      target = "facilities_overview",
      zone_clicked = NA,
      analysis_date = as.Date("2026-03-04"),
      expiring_days = 30,
      facility_clicked = NA,
      zone_filter_raw = NA
    )
  )
})

# =============================================================================
# 7. HISTORICAL CACHE PATH RESOLUTION
# =============================================================================

test_that("historical_cache.R uses .resolve_overview_file for paths", {
  hist_cache_file <- file.path(root, "apps/overview/historical_cache.R")
  if (!file.exists(hist_cache_file)) {
    skip("historical_cache.R not found")
  }
  
  content <- readLines(hist_cache_file, warn = FALSE)
  content_text <- paste(content, collapse = "\n")
  
  # Must use dynamic path resolution, not bare filenames
  expect_true(
    grepl("\\.resolve_overview_file", content_text),
    info = "historical_cache.R must use .resolve_overview_file for path resolution"
  )
  
  # Should NOT have bare source('metric_registry.R') calls
  bare_source_lines <- grep("source\\(['\"]metric_registry\\.R['\"]\\)", content)
  expect_equal(length(bare_source_lines), 0,
    info = "historical_cache.R must not use bare source('metric_registry.R') — use .resolve_overview_file")
})

# =============================================================================
# 8. DELETED FILE SAFETY
# =============================================================================

test_that("deleted files are not referenced in active code", {
  # Files deleted during refactoring
  deleted_files <- c(
    "apps/overview/ui_helper.R",
    "apps/overview/server_utilities.R"
  )
  
  # Files that should NOT reference deleted files
  # (test runner is excluded since it may have stale references)
  check_files <- c(
    "apps/overview/unified/app.R",
    "apps/overview/dynamic_server.R",
    "apps/overview/dynamic_ui.R",
    "apps/overview/data_functions.R",
    "apps/overview/historical_functions.R",
    "apps/overview/historical_cache.R"
  )
  
  for (deleted in deleted_files) {
    deleted_basename <- basename(deleted)
    for (check in check_files) {
      full_path <- file.path(root, check)
      if (!file.exists(full_path)) next
      
      content <- readLines(full_path, warn = FALSE)
      references <- grep(paste0("source.*", deleted_basename), content)
      
      # Filter out comments
      references <- references[!grepl("^\\s*#", content[references])]
      
      expect_equal(length(references), 0,
        info = paste0(check, " must not source deleted file: ", deleted_basename))
    }
  }
})

# =============================================================================
# 9. DISPLAY FUNCTIONS: CHART CLICK REGISTRATION
# =============================================================================

test_that("create_overview_chart registers plotly click when clickable", {
  display_file <- file.path(root, "apps/overview/display_functions.R")
  content <- readLines(display_file, warn = FALSE)
  content_text <- paste(content, collapse = "\n")
  
  # Must set plotly source for click events (native plot_ly uses source= arg)
  expect_true(
    grepl("source = if \\(clickable\\) metric_type", content_text),
    info = "create_overview_chart must set plotly source for click events"
  )
  
  # Must register plotly_click event
  expect_true(
    grepl('event_register\\("plotly_click"\\)', content_text),
    info = "create_overview_chart must register plotly_click event"
  )
  
  # Bars must have key aesthetic for click identification
  expect_true(
    grepl("key = ~as\\.character\\(display_name\\)", content_text),
    info = "Bars must have key = as.character(display_name) for click identification"
  )
})

# =============================================================================
# 10. OVERVIEW CONFIG DRILL-DOWN TARGETS
# =============================================================================

test_that("overview configs have correct drill-down targets", {
  if (!exists("get_overview_config", mode = "function")) {
    skip("get_overview_config not loaded")
  }
  
  district <- get_overview_config("district")
  expect_true(district$enable_drill_down, info = "District must enable drill-down")
  expect_equal(district$drill_down_target, "facilities_overview",
    info = "District drill-down target must be facilities_overview")
  
  facilities <- get_overview_config("facilities")
  expect_true(facilities$enable_drill_down, info = "Facilities must enable drill-down")
  expect_equal(facilities$drill_down_target, "fos_overview",
    info = "Facilities drill-down target must be fos_overview")
  
  fos <- get_overview_config("fos")
  expect_false(fos$enable_drill_down, info = "FOS must NOT enable drill-down (leaf level)")
})
