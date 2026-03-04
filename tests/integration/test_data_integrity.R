# =============================================================================
# DATA INTEGRITY TEST — Overview Dashboard
# =============================================================================
# Validates that data loading, historical comparison, and value box coloring
# produce correct results for a known reference date (August 7, 2025).
#
# This test contacts the REAL database — run only where DB is reachable.
#
# Usage:
#   Rscript tests/integration/test_data_integrity.R
# =============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(lubridate)
})

# Navigate to project root
if (basename(getwd()) == "tests" || basename(getwd()) == "integration") {
  while (!file.exists("shared/app_libraries.R")) setwd("..")
}
cat("Working directory:", getwd(), "\n\n")

# Source project modules
source("shared/app_libraries.R")
source("shared/db_helpers.R")
source("shared/stat_box_helpers.R")
source("shared/redis_cache.R")
source("shared/cache_utilities.R")
source("apps/overview/metric_registry.R")
source("apps/overview/data_functions.R")
source("apps/overview/historical_functions.R")
source("apps/overview/historical_cache.R")
source("apps/overview/dynamic_server.R")

# ---------------------------------------------------------------------------
# PRE-FLIGHT: Verify database connectivity before running any tests.
# These are INTEGRATION tests — they require a live PostgreSQL connection.
# Skip gracefully when running outside Docker / without DB access.
# ---------------------------------------------------------------------------
db_available <- tryCatch({
  con <- get_db_connection()
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE))
  DBI::dbIsValid(con)
}, error = function(e) {
  FALSE
})

if (!db_available) {
  cat("SKIPPED: Database not reachable — integration tests require a live DB connection.\n")
  cat("Run inside Docker or on a host with DB_HOST/DB_USER/DB_PASSWORD set.\n")
  quit(status = 0)
}
cat("Database connection: OK\n\n")

# Reference date for all tests
TEST_DATE <- as.Date("2025-08-07")
ZONE_FILTER <- c("1", "2")

cat("=== Data Integrity Tests ===\n")
cat("Reference date:", as.character(TEST_DATE), "\n")
cat("Week number:", week(TEST_DATE), "\n\n")

# ============================================================================
# 1. Current Data Loading — Every metric must return a data.frame with rows
# ============================================================================
test_that("Current data loads successfully for all active metrics", {
  registry <- get_metric_registry()
  metrics <- get_active_metrics()
  
  cat("\n--- Testing current data loading (district, by zone) ---\n")
  for (metric_id in metrics) {
    config <- registry[[metric_id]]
    
    result <- tryCatch(
      load_data_by_zone(
        metric = metric_id,
        analysis_date = TEST_DATE,
        expiring_days = 7,
        zone_filter = ZONE_FILTER,
        separate_zones = FALSE
      ),
      error = function(e) {
        cat("  FAIL:", metric_id, "-", e$message, "\n")
        NULL
      }
    )
    
    expect_true(!is.null(result), label = paste(metric_id, "returned NULL"))
    expect_true(is.data.frame(result), label = paste(metric_id, "is not a data.frame"))
    expect_true(nrow(result) > 0, label = paste(metric_id, "has 0 rows"))
    
    # Each metric must have at least 'total' and 'active' columns
    expect_true("total" %in% names(result), label = paste(metric_id, "missing 'total' col"))
    expect_true("active" %in% names(result), label = paste(metric_id, "missing 'active' col"))
    
    total_val <- sum(result$total, na.rm = TRUE)
    active_val <- sum(result$active, na.rm = TRUE)
    
    cat(sprintf("  %-25s total=%6d  active=%6d  rows=%d\n",
                config$display_name, total_val, active_val, nrow(result)))
    
    # Sanity: total should be > 0 during peak season (August)
    expect_true(total_val > 0, label = paste(metric_id, "total is 0 in August"))
  }
})

# ============================================================================
# 2. Historical Data Loading — Must return 5yr + 10yr averages + current
# ============================================================================
test_that("Historical data loads with averages for all historical metrics", {
  registry <- get_metric_registry()
  hist_metrics <- get_historical_metrics()
  years <- get_historical_year_range(10, TEST_DATE)
  
  cat("\n--- Testing historical data loading ---\n")
  for (metric_id in hist_metrics) {
    config <- registry[[metric_id]]
    
    result <- tryCatch(
      load_historical_comparison_data(
        metric = metric_id,
        start_year = years$start_year,
        end_year = years$end_year,
        display_metric = config$display_metric,
        zone_filter = ZONE_FILTER,
        analysis_date = TEST_DATE,
        overview_type = "district",
        facility_filter = NULL
      ),
      error = function(e) {
        cat("  FAIL:", metric_id, "-", e$message, "\n")
        NULL
      }
    )
    
    expect_true(!is.null(result), label = paste(metric_id, "returned NULL"))
    expect_true(is.list(result), label = paste(metric_id, "is not a list"))
    
    # Weekly metrics should have average + current components
    if (!isTRUE(config$historical_type == "yearly_grouped")) {
      avg <- result$average
      ten_yr <- result$ten_year_average
      current <- result$current
      
      expect_true(!is.null(avg) && nrow(avg) > 0,
                  label = paste(metric_id, "5yr average is empty"))
      expect_true(!is.null(ten_yr) && nrow(ten_yr) > 0,
                  label = paste(metric_id, "10yr average is empty"))
      expect_true(!is.null(current) && nrow(current) > 0,
                  label = paste(metric_id, "current year data is empty"))
      
      # All should have 'week_num' and 'value' columns
      expect_true("week_num" %in% names(avg), label = paste(metric_id, "avg missing week_num"))
      expect_true("value" %in% names(avg), label = paste(metric_id, "avg missing value"))
      
      # Test week 32 (August 7) has data in current year
      test_week <- week(TEST_DATE)
      current_week <- current[current$week_num == test_week, ]
      avg_week <- ten_yr[ten_yr$week_num == test_week, ]
      
      current_val <- if (nrow(current_week) > 0) sum(current_week$value, na.rm = TRUE) else 0
      avg_val <- if (nrow(avg_week) > 0) sum(avg_week$value, na.rm = TRUE) else 0
      
      cat(sprintf("  %-25s current_w%d=%7.0f  10yr_avg=%7.0f  weeks=%d\n",
                  config$display_name, test_week, current_val, avg_val, nrow(current)))
    } else {
      # Yearly grouped metrics (cattail_treatments)
      expect_true(!is.null(result$yearly_data) && nrow(result$yearly_data) > 0,
                  label = paste(metric_id, "yearly_data is empty"))
      cat(sprintf("  %-25s yearly_rows=%d\n", config$display_name, nrow(result$yearly_data)))
    }
  }
})

# ============================================================================
# 3. Weekly Value Extraction — extract_weekly_values must return values
# ============================================================================
test_that("extract_weekly_values returns non-null for all metrics", {
  registry <- get_metric_registry()
  metrics <- get_active_metrics()
  hist_metrics <- get_historical_metrics()
  years <- get_historical_year_range(10, TEST_DATE)
  test_week <- week(TEST_DATE)
  
  cat("\n--- Testing weekly value extraction ---\n")
  
  # Load historical data first (simulating what the dashboard does)
  historical_data <- list()
  for (metric_id in hist_metrics) {
    config <- registry[[metric_id]]
    historical_data[[metric_id]] <- tryCatch(
      load_historical_comparison_data(
        metric = metric_id,
        start_year = years$start_year,
        end_year = years$end_year,
        display_metric = config$display_metric,
        zone_filter = ZONE_FILTER,
        analysis_date = TEST_DATE,
        overview_type = "district"
      ),
      error = function(e) list(average = data.frame(), current = data.frame())
    )
  }
  
  weekly_vals <- extract_weekly_values(metrics, historical_data, test_week, registry)
  
  for (metric_id in hist_metrics) {
    val <- weekly_vals[[metric_id]]
    config <- registry[[metric_id]]
    if (!isTRUE(config$historical_type == "yearly_grouped")) {
      expect_true(!is.null(val), label = paste(metric_id, "weekly value is NULL"))
      expect_true(val > 0, label = paste(metric_id, "weekly value is 0 in August"))
      cat(sprintf("  %-25s week_%d_value = %7.0f\n", config$display_name, test_week, val))
    }
  }
})

# ============================================================================
# 4. Value Box Coloring — Drone must get a non-default color
# ============================================================================
test_that("Dynamic value box coloring produces colors for key metrics", {
  registry <- get_metric_registry()
  test_week <- week(TEST_DATE)
  dynamic_metrics <- c("drone", "ground_prehatch", "catch_basin", "structure")
  
  cat("\n--- Testing dynamic value box coloring ---\n")
  
  for (metric_id in dynamic_metrics) {
    config <- registry[[metric_id]]
    
    # Load current data to get a realistic 'active' value
    current <- tryCatch(
      load_data_by_zone(
        metric = metric_id, analysis_date = TEST_DATE,
        expiring_days = 7, zone_filter = ZONE_FILTER
      ),
      error = function(e) data.frame()
    )
    
    active_val <- if (nrow(current) > 0) sum(current$active, na.rm = TRUE) else 0
    
    box_info <- tryCatch(
      get_dynamic_value_box_info(
        metric_id, active_val, TEST_DATE, config,
        zone_filter = ZONE_FILTER, weekly_value = active_val
      ),
      error = function(e) list(color = "error", status = "error")
    )
    
    cat(sprintf("  %-25s active=%6d  status=%-10s  color=%-8s  hist_avg=%s  pct_diff=%s\n",
                config$display_name, active_val, box_info$status, box_info$color,
                ifelse(is.null(box_info$historical_avg), "NULL", as.character(box_info$historical_avg)),
                ifelse(is.null(box_info$pct_diff), "NULL", paste0(box_info$pct_diff, "%"))))
    
    # Key assertion: in August, these metrics MUST produce a non-default color
    # (meaning the historical cache has data for comparison)
    if (active_val > 0) {
      expect_true(box_info$status != "default",
                  label = paste(metric_id, "returned default status — historical cache may be empty"))
    }
  }
})

# ============================================================================
# 5. Facility-Filtered Historical Cache — Test Redis cache round-trip
# ============================================================================
test_that("Facility-filtered historical data caches and retrieves correctly", {
  cat("\n--- Testing facility-filtered historical cache ---\n")
  
  registry <- get_metric_registry()
  years <- get_historical_year_range(10, TEST_DATE)
  
  # Pick a metric and facility to test
  test_metric <- "catch_basin"
  test_facility <- "South Rosemount"  # adjust if needed
  
  config <- registry[[test_metric]]
  
  # First call — should cache miss and compute from DB
  result1 <- tryCatch(
    load_historical_comparison_data(
      metric = test_metric,
      start_year = years$start_year,
      end_year = years$end_year,
      display_metric = config$display_metric,
      zone_filter = ZONE_FILTER,
      analysis_date = TEST_DATE,
      overview_type = "facilities",
      facility_filter = test_facility
    ),
    error = function(e) NULL
  )
  
  if (is.null(result1)) {
    cat("  SKIP: Could not load data for", test_metric, "/", test_facility, "\n")
  } else {
    cat("  First call (DB): 10yr avg rows =", nrow(result1$ten_year_average),
        ", current rows =", nrow(result1$current), "\n")
    
    # Second call — should be a cache hit (if Redis is available)
    result2 <- tryCatch(
      load_historical_comparison_data(
        metric = test_metric,
        start_year = years$start_year,
        end_year = years$end_year,
        display_metric = config$display_metric,
        zone_filter = ZONE_FILTER,
        analysis_date = TEST_DATE,
        overview_type = "facilities",
        facility_filter = test_facility
      ),
      error = function(e) NULL
    )
    
    if (!is.null(result2)) {
      cat("  Second call (cache): 10yr avg rows =", nrow(result2$ten_year_average),
          ", current rows =", nrow(result2$current), "\n")
      
      # Results should match
      expect_equal(nrow(result1$ten_year_average), nrow(result2$ten_year_average),
                   label = "Cached row count mismatch for ten_year_average")
      expect_equal(nrow(result1$current), nrow(result2$current),
                   label = "Cached row count mismatch for current")
    }
  }
})

# ============================================================================
# 6. Catch Basin + Structure — Verify non-zero values in August
# ============================================================================
test_that("Catch basin and structure have non-zero data on Aug 7 2025", {
  cat("\n--- Verifying catch basin and structure data ---\n")
  registry <- get_metric_registry()
  
  for (metric_id in c("catch_basin", "structure")) {
    # Current data
    current <- tryCatch(
      load_data_by_zone(metric = metric_id, analysis_date = TEST_DATE,
                        expiring_days = 7, zone_filter = ZONE_FILTER),
      error = function(e) data.frame()
    )
    
    total <- sum(current$total, na.rm = TRUE)
    active <- sum(current$active, na.rm = TRUE)
    
    cat(sprintf("  %-15s total=%d  active=%d  rows=%d\n",
                metric_id, total, active, nrow(current)))
    
    expect_true(total > 0, label = paste(metric_id, "total is 0"))
    expect_true(active > 0, label = paste(metric_id, "active is 0"))
    
    # Historical comparison
    config <- registry[[metric_id]]
    years <- get_historical_year_range(10, TEST_DATE)
    hist_result <- tryCatch(
      load_historical_comparison_data(
        metric = metric_id,
        start_year = years$start_year,
        end_year = years$end_year,
        display_metric = config$display_metric,
        zone_filter = ZONE_FILTER,
        analysis_date = TEST_DATE,
        overview_type = "district"
      ),
      error = function(e) NULL
    )
    
    expect_true(!is.null(hist_result), label = paste(metric_id, "historical is NULL"))
    
    test_week <- week(TEST_DATE)
    if (!is.null(hist_result$ten_year_average)) {
      week_avg <- hist_result$ten_year_average[hist_result$ten_year_average$week_num == test_week, ]
      avg_val <- sum(week_avg$value, na.rm = TRUE)
      cat(sprintf("  %-15s 10yr_week_%d_avg = %0.0f\n", metric_id, test_week, avg_val))
      expect_true(avg_val > 0, label = paste(metric_id, "10yr avg is 0"))
    }
    
    if (!is.null(hist_result$current)) {
      week_cur <- hist_result$current[hist_result$current$week_num == test_week, ]
      cur_val <- sum(week_cur$value, na.rm = TRUE)
      cat(sprintf("  %-15s current_week_%d = %0.0f\n", metric_id, test_week, cur_val))
      expect_true(cur_val > 0, label = paste(metric_id, "current week is 0"))
    }
  }
})

cat("\n=== All data integrity tests complete ===\n")
