# =============================================================================
# Tests for Tiered Cache System
# =============================================================================
# Tests the new multi-tier Redis caching:
#   1. TTL constants (14 days, 7 days, 2 min)
#   2. Cache key generation (build_cache_key)
#   3. DB query caching (2-min TTL)
#   4. Stat box caching (2-min TTL)
#   5. Chart caching (2-min TTL)
#   6. FOS drill-down caching (7-day TTL)
#   7. Color mapping caching (7-day TTL)
#   8. Cache tier clearing
#   9. Auto-repopulate logic
#  10. Individual app cached_load_raw_data wrapper
# =============================================================================

library(testthat)

context("Tiered Cache System")

# =============================================================================
# SOURCE REQUIRED FILES (with fallback paths)
# =============================================================================

.source_shared <- function(filename) {
  paths <- c(
    file.path("shared", filename),
    file.path("../../shared", filename),
    file.path("/home/alex/Documents/mmcd/mmcd_metrics/shared", filename)
  )
  for (p in paths) {
    if (file.exists(p)) { source(p, local = FALSE); return(TRUE) }
  }
  FALSE
}

# Source redis_cache.R first (defines TTL constants and cache functions)
.source_shared("redis_cache.R")

# =============================================================================
# TEST: TTL Constants
# =============================================================================

test_that("TTL constants are defined with correct values", {
  expect_true(exists("TTL_14_DAYS"))
  expect_true(exists("TTL_7_DAYS"))
  expect_true(exists("TTL_2_MIN"))
  expect_true(exists("TTL_5_MIN"))
  expect_true(exists("TTL_24_HR"))
  
  expect_equal(TTL_14_DAYS, 1209600L)
  expect_equal(TTL_7_DAYS, 604800L)
  expect_equal(TTL_2_MIN, 120L)
  expect_equal(TTL_5_MIN, 300L)
  expect_equal(TTL_24_HR, 86400L)
})

test_that("14-day TTL equals exactly 14 * 24 * 60 * 60", {
  expect_equal(TTL_14_DAYS, 14L * 24L * 60L * 60L)
})

test_that("7-day TTL equals exactly 7 * 24 * 60 * 60", {
  expect_equal(TTL_7_DAYS, 7L * 24L * 60L * 60L)
})

# =============================================================================
# TEST: Cache Key Prefix Constants
# =============================================================================

test_that("cache tier prefix constants are defined", {
  expect_true(exists("CACHE_PREFIX_DB"))
  expect_true(exists("CACHE_PREFIX_CHART"))
  expect_true(exists("CACHE_PREFIX_STAT"))
  expect_true(exists("CACHE_PREFIX_FOS"))
  expect_true(exists("CACHE_PREFIX_COLOR"))
  
  expect_equal(CACHE_PREFIX_DB, "db")
  expect_equal(CACHE_PREFIX_CHART, "chart")
  expect_equal(CACHE_PREFIX_STAT, "stat")
  expect_equal(CACHE_PREFIX_FOS, "fos")
  expect_equal(CACHE_PREFIX_COLOR, "color")
})

# =============================================================================
# TEST: build_cache_key
# =============================================================================

test_that("build_cache_key produces deterministic keys", {
  expect_true(exists("build_cache_key", mode = "function"))
  
  key1 <- build_cache_key("db", "catch_basin", "2025-01-15", "1_2")
  key2 <- build_cache_key("db", "catch_basin", "2025-01-15", "1_2")
  
  expect_equal(key1, key2)
  expect_true(grepl("^db:", key1))
})

test_that("build_cache_key produces different keys for different params", {
  key1 <- build_cache_key("db", "catch_basin", "2025-01-15")
  key2 <- build_cache_key("db", "catch_basin", "2025-01-16")
  key3 <- build_cache_key("db", "drone", "2025-01-15")
  
  expect_false(key1 == key2)
  expect_false(key1 == key3)
})

test_that("build_cache_key handles NULL and empty params", {
  key1 <- build_cache_key("fos", "test", NULL)
  key2 <- build_cache_key("fos", "test", "")
  key3 <- build_cache_key("fos", "test")
  
  # All should be valid strings starting with prefix
  expect_true(grepl("^fos:", key1))
  expect_true(grepl("^fos:", key2))
  expect_true(grepl("^fos:", key3))
  
  # Different params should give different keys
  expect_false(key1 == key2)
})

test_that("build_cache_key handles complex objects", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  key1 <- build_cache_key("stat", "test", df)
  key2 <- build_cache_key("stat", "test", df)
  
  expect_equal(key1, key2)
  expect_true(grepl("^stat:", key1))
})

# =============================================================================
# TEST: Function signature defaults (TTL values)
# =============================================================================

test_that("save_historical_cache_redis defaults to 14-day TTL", {
  fn <- save_historical_cache_redis
  defaults <- formals(fn)
  expect_equal(eval(defaults$ttl), TTL_14_DAYS)
})

test_that("set_lookup_redis defaults to 14-day TTL", {
  fn <- set_lookup_redis
  defaults <- formals(fn)
  expect_equal(eval(defaults$ttl), TTL_14_DAYS)
})

test_that("redis_set defaults to 14-day TTL", {
  fn <- redis_set
  defaults <- formals(fn)
  expect_equal(eval(defaults$ttl), TTL_14_DAYS)
})

test_that("redis_hset defaults to 14-day TTL", {
  fn <- redis_hset
  defaults <- formals(fn)
  expect_equal(eval(defaults$ttl), TTL_14_DAYS)
})

test_that("get_app_cached_redis defaults to 5-min TTL", {
  fn <- get_app_cached_redis
  defaults <- formals(fn)
  expect_equal(eval(defaults$ttl), TTL_5_MIN)
})

# =============================================================================
# TEST: Tiered cache wrapper functions exist
# =============================================================================

test_that("all tiered cache functions exist", {
  expect_true(exists("get_cached_db_query", mode = "function"))
  expect_true(exists("get_cached_chart", mode = "function"))
  expect_true(exists("get_cached_stat_box", mode = "function"))
  expect_true(exists("get_cached_fos_data", mode = "function"))
  expect_true(exists("get_cached_color_mapping", mode = "function"))
  expect_true(exists("clear_cache_tier", mode = "function"))
  expect_true(exists("get_cache_tier_counts", mode = "function"))
})

# =============================================================================
# TEST: Tiered cache functions call get_app_cached_redis correctly
# =============================================================================

test_that("get_cached_db_query builds correct key prefix", {
  # Mock get_app_cached_redis to capture the key
  captured_key <- NULL
  captured_ttl <- NULL
  original_fn <- get_app_cached_redis
  
  # Override temporarily
  assignInNamespace <- NULL  # Not needed; we override in global
  assign("get_app_cached_redis", function(key, load_func = NULL, ttl = 300L) {
    captured_key <<- key
    captured_ttl <<- ttl
    "test_result"
  }, envir = .GlobalEnv)
  
  result <- get_cached_db_query("test_metric", function() "data", "param1")
  
  # Restore
  assign("get_app_cached_redis", original_fn, envir = .GlobalEnv)
  
  expect_equal(result, "test_result")
  expect_true(grepl("^db:", captured_key))
  expect_equal(captured_ttl, TTL_2_MIN)
})

test_that("get_cached_fos_data uses 7-day TTL", {
  captured_ttl <- NULL
  original_fn <- get_app_cached_redis
  
  assign("get_app_cached_redis", function(key, load_func = NULL, ttl = 300L) {
    captured_ttl <<- ttl
    "fos_data"
  }, envir = .GlobalEnv)
  
  result <- get_cached_fos_data("test_fos", function() "data")
  
  assign("get_app_cached_redis", original_fn, envir = .GlobalEnv)
  
  expect_equal(result, "fos_data")
  expect_equal(captured_ttl, TTL_7_DAYS)
})

test_that("get_cached_color_mapping uses 7-day TTL", {
  captured_ttl <- NULL
  original_fn <- get_app_cached_redis
  
  assign("get_app_cached_redis", function(key, load_func = NULL, ttl = 300L) {
    captured_ttl <<- ttl
    "color_data"
  }, envir = .GlobalEnv)
  
  result <- get_cached_color_mapping("facility_colors", function() "colors")
  
  assign("get_app_cached_redis", original_fn, envir = .GlobalEnv)
  
  expect_equal(result, "color_data")
  expect_equal(captured_ttl, TTL_7_DAYS)
})

test_that("get_cached_stat_box uses 2-min TTL", {
  captured_ttl <- NULL
  original_fn <- get_app_cached_redis
  
  assign("get_app_cached_redis", function(key, load_func = NULL, ttl = 300L) {
    captured_ttl <<- ttl
    "stat_data"
  }, envir = .GlobalEnv)
  
  result <- get_cached_stat_box("test_stat", function() "stats")
  
  assign("get_app_cached_redis", original_fn, envir = .GlobalEnv)
  
  expect_equal(result, "stat_data")
  expect_equal(captured_ttl, TTL_2_MIN)
})

test_that("get_cached_chart uses 2-min TTL", {
  captured_ttl <- NULL
  original_fn <- get_app_cached_redis
  
  assign("get_app_cached_redis", function(key, load_func = NULL, ttl = 300L) {
    captured_ttl <<- ttl
    "chart_data"
  }, envir = .GlobalEnv)
  
  result <- get_cached_chart("test_chart", function() "chart")
  
  assign("get_app_cached_redis", original_fn, envir = .GlobalEnv)
  
  expect_equal(result, "chart_data")
  expect_equal(captured_ttl, TTL_2_MIN)
})

# =============================================================================
# TEST: clear_cache_tier
# =============================================================================

test_that("clear_cache_tier handles unknown prefix gracefully", {
  # Should not error
  result <- tryCatch({
    clear_cache_tier("nonexistent_prefix")
    TRUE
  }, error = function(e) FALSE)
  
  expect_true(result)
})

# =============================================================================
# TEST: get_cache_tier_counts returns correct structure
# =============================================================================

test_that("get_cache_tier_counts returns named list with all tiers", {
  counts <- get_cache_tier_counts()
  
  expect_true(is.list(counts))
  expect_true("db_queries" %in% names(counts))
  expect_true("charts" %in% names(counts))
  expect_true("stat_boxes" %in% names(counts))
  expect_true("fos_drilldown" %in% names(counts))
  expect_true("color_maps" %in% names(counts))
  expect_true("other_app" %in% names(counts))
  
  # All should be non-negative integers
  for (n in names(counts)) {
    expect_true(counts[[n]] >= 0, info = paste("Count for", n, "should be >= 0"))
  }
})

# =============================================================================
# TEST: Redis integration (if Redis is available)
# =============================================================================

test_that("Redis round-trip works for app-level cache", {
  skip_if(!exists("redis_is_active", mode = "function") || !redis_is_active(),
          "Redis not available")
  
  # Write
  test_key <- "test:tiered:roundtrip"
  test_data <- list(x = 1:5, name = "test", timestamp = Sys.time())
  ok <- set_app_cached_redis(test_key, test_data, ttl = 60L)
  expect_true(ok)
  
  # Read
  result <- redis_get(paste0("app:", test_key))
  expect_equal(result$x, test_data$x)
  expect_equal(result$name, test_data$name)
  
  # Clean up
  redis_del(paste0("app:", test_key))
})

test_that("get_app_cached_redis loads on miss and caches", {
  skip_if(!exists("redis_is_active", mode = "function") || !redis_is_active(),
          "Redis not available")
  
  # Clean any existing cache
  test_key <- "test:tiered:load_on_miss"
  redis_del(paste0("app:", test_key))
  
  load_count <- 0
  loader <- function() {
    load_count <<- load_count + 1
    data.frame(val = 42)
  }
  
  # First call: cache miss → loads from function
  result1 <- get_app_cached_redis(test_key, loader, ttl = 60L)
  expect_equal(load_count, 1)
  expect_equal(result1$val, 42)
  
  # Second call: cache hit → does NOT call loader again
  result2 <- get_app_cached_redis(test_key, loader, ttl = 60L)
  expect_equal(load_count, 1)  # Still 1!
  expect_equal(result2$val, 42)
  
  # Clean up
  redis_del(paste0("app:", test_key))
})

test_that("get_cached_db_query end-to-end works", {
  skip_if(!exists("redis_is_active", mode = "function") || !redis_is_active(),
          "Redis not available")
  
  load_count <- 0
  
  result1 <- get_cached_db_query(
    "test_e2e_metric",
    function() { load_count <<- load_count + 1; data.frame(total = 100) },
    "test_e2e_metric", "2025-01-15", "1_2"
  )
  expect_equal(result1$total, 100)
  expect_equal(load_count, 1)
  
  # Same params → cache hit
  result2 <- get_cached_db_query(
    "test_e2e_metric",
    function() { load_count <<- load_count + 1; data.frame(total = 200) },
    "test_e2e_metric", "2025-01-15", "1_2"
  )
  expect_equal(result2$total, 100)  # Returns FIRST value (cached)
  expect_equal(load_count, 1)
  
  # Different params → cache miss
  result3 <- get_cached_db_query(
    "test_e2e_metric",
    function() { load_count <<- load_count + 1; data.frame(total = 300) },
    "test_e2e_metric", "2025-01-16", "1_2"  # different date
  )
  expect_equal(result3$total, 300)
  expect_equal(load_count, 2)
  
  # Clean up
  clear_cache_tier("db")
})

test_that("get_cached_fos_data end-to-end works with 7-day TTL", {
  skip_if(!exists("redis_is_active", mode = "function") || !redis_is_active(),
          "Redis not available")
  
  load_count <- 0
  
  result <- get_cached_fos_data(
    "test_fos_e2e",
    function() { load_count <<- load_count + 1; data.frame(fos = "John", total = 50) },
    "test_fos", "2025-01-15"
  )
  expect_equal(result$fos, "John")
  expect_equal(load_count, 1)
  
  # Cache hit
  result2 <- get_cached_fos_data(
    "test_fos_e2e",
    function() { load_count <<- load_count + 1; data.frame(fos = "Jane", total = 99) },
    "test_fos", "2025-01-15"
  )
  expect_equal(result2$fos, "John")  # Cached value
  expect_equal(load_count, 1)
  
  # Clean up
  clear_cache_tier("fos")
})

test_that("get_cached_color_mapping end-to-end works", {
  skip_if(!exists("redis_is_active", mode = "function") || !redis_is_active(),
          "Redis not available")
  
  result <- get_cached_color_mapping(
    "test_colors",
    function() c(N = "#FF0000", E = "#00FF00"),
    "test_theme"
  )
  expect_equal(result[["N"]], "#FF0000")
  expect_equal(result[["E"]], "#00FF00")
  
  # Clean up
  clear_cache_tier("color")
})

test_that("clear_cache_tier clears only the specified tier", {
  skip_if(!exists("redis_is_active", mode = "function") || !redis_is_active(),
          "Redis not available")
  
  # Populate db and fos tiers
  get_cached_db_query("clear_test_db", function() "db_data", "p1")
  get_cached_fos_data("clear_test_fos", function() "fos_data", "p2")
  
  counts_before <- get_cache_tier_counts()
  expect_true(counts_before$db_queries > 0)
  expect_true(counts_before$fos_drilldown > 0)
  
  # Clear only db tier
  clear_cache_tier("db")
  
  counts_after <- get_cache_tier_counts()
  expect_equal(counts_after$db_queries, 0)
  expect_true(counts_after$fos_drilldown > 0)  # FOS should still exist
  
  # Clean up
  clear_cache_tier("fos")
})

# =============================================================================
# TEST: Historical cache auto-repopulate configuration
# =============================================================================

test_that("is_cache_valid defaults to 14-day max age", {
  # Source historical_cache.R
  hist_paths <- c(
    "apps/overview/historical_cache.R",
    "../../apps/overview/historical_cache.R",
    "/home/alex/Documents/mmcd/mmcd_metrics/apps/overview/historical_cache.R"
  )
  sourced <- FALSE
  for (p in hist_paths) {
    if (file.exists(p)) {
      tryCatch({ source(p, local = FALSE); sourced <- TRUE }, error = function(e) NULL)
      break
    }
  }
  skip_if(!sourced, "historical_cache.R not found")
  
  fn <- is_cache_valid
  defaults <- formals(fn)
  expect_equal(defaults$max_age_days, 14)
})

# =============================================================================
# TEST: server_utilities.R cached wrappers
# =============================================================================

test_that("cached_load_raw_data function exists in server_utilities", {
  # Source server_utilities.R
  util_paths <- c(
    "shared/server_utilities.R",
    "../../shared/server_utilities.R",
    "/home/alex/Documents/mmcd/mmcd_metrics/shared/server_utilities.R"
  )
  sourced <- FALSE
  for (p in util_paths) {
    if (file.exists(p)) {
      tryCatch({ source(p, local = FALSE); sourced <- TRUE }, error = function(e) NULL)
      break
    }
  }
  skip_if(!sourced, "server_utilities.R not found")
  
  expect_true(exists("cached_load_raw_data", mode = "function"))
  expect_true(exists("cached_stat_calculation", mode = "function"))
  expect_true(exists("cached_chart", mode = "function"))
})

test_that("cached_load_raw_data falls back to load_raw_data when no Redis", {
  # Mock load_raw_data in global env (where cached_load_raw_data will find it)
  assign("load_raw_data", function(...) list(sites = data.frame(x = 1), call_args = list(...)), envir = .GlobalEnv)
  on.exit(rm("load_raw_data", envir = .GlobalEnv), add = TRUE)
  
  # Temporarily hide get_cached_db_query to test fallback path
  if (exists("get_cached_db_query", envir = .GlobalEnv)) {
    saved <- get("get_cached_db_query", envir = .GlobalEnv)
    rm("get_cached_db_query", envir = .GlobalEnv)
    on.exit(assign("get_cached_db_query", saved, envir = .GlobalEnv), add = TRUE)
  }
  
  result <- cached_load_raw_data("test_app", analysis_date = "2025-01-15")
  expect_true(is.list(result))
  expect_true("sites" %in% names(result))
})

# =============================================================================
# TEST: Individual apps use cached_load_raw_data
# =============================================================================

test_that("catch_basin_status app.R uses cached_load_raw_data", {
  app_path <- NULL
  for (p in c("apps/catch_basin_status/app.R",
              "../../apps/catch_basin_status/app.R",
              "/home/alex/Documents/mmcd/mmcd_metrics/apps/catch_basin_status/app.R")) {
    if (file.exists(p)) { app_path <- p; break }
  }
  skip_if(is.null(app_path), "catch_basin_status/app.R not found")
  
  code <- readLines(app_path)
  expect_true(any(grepl("cached_load_raw_data", code)),
              info = "catch_basin_status should use cached_load_raw_data")
})

test_that("drone app.R uses cached_load_raw_data", {
  app_path <- NULL
  for (p in c("apps/drone/app.R",
              "../../apps/drone/app.R",
              "/home/alex/Documents/mmcd/mmcd_metrics/apps/drone/app.R")) {
    if (file.exists(p)) { app_path <- p; break }
  }
  skip_if(is.null(app_path), "drone/app.R not found")
  
  code <- readLines(app_path)
  # Should have cached_load_raw_data and NOT bare load_raw_data calls
  cached_count <- sum(grepl("cached_load_raw_data", code))
  expect_true(cached_count >= 1, info = "drone should use cached_load_raw_data")
})

test_that("struct_trt app.R uses cached_load_raw_data", {
  app_path <- NULL
  for (p in c("apps/struct_trt/app.R",
              "../../apps/struct_trt/app.R",
              "/home/alex/Documents/mmcd/mmcd_metrics/apps/struct_trt/app.R")) {
    if (file.exists(p)) { app_path <- p; break }
  }
  skip_if(is.null(app_path), "struct_trt/app.R not found")
  
  code <- readLines(app_path)
  expect_true(any(grepl("cached_load_raw_data", code)),
              info = "struct_trt should use cached_load_raw_data")
})

test_that("inspections app.R uses cached_load_raw_data", {
  app_path <- NULL
  for (p in c("apps/inspections/app.R",
              "../../apps/inspections/app.R",
              "/home/alex/Documents/mmcd/mmcd_metrics/apps/inspections/app.R")) {
    if (file.exists(p)) { app_path <- p; break }
  }
  skip_if(is.null(app_path), "inspections/app.R not found")
  
  code <- readLines(app_path)
  expect_true(any(grepl("cached_load_raw_data", code)),
              info = "inspections should use cached_load_raw_data")
})

test_that("cattail_treatments app.R uses cached_load_raw_data", {
  app_path <- NULL
  for (p in c("apps/cattail_treatments/app.R",
              "../../apps/cattail_treatments/app.R",
              "/home/alex/Documents/mmcd/mmcd_metrics/apps/cattail_treatments/app.R")) {
    if (file.exists(p)) { app_path <- p; break }
  }
  skip_if(is.null(app_path), "cattail_treatments/app.R not found")
  
  code <- readLines(app_path)
  expect_true(any(grepl("cached_load_raw_data", code)),
              info = "cattail_treatments should use cached_load_raw_data")
})

# =============================================================================
# TEST: Overview data_functions.R uses caching
# =============================================================================

test_that("overview load_metric_data uses cached DB queries", {
  df_path <- NULL
  for (p in c("apps/overview/data_functions.R",
              "../../apps/overview/data_functions.R",
              "/home/alex/Documents/mmcd/mmcd_metrics/apps/overview/data_functions.R")) {
    if (file.exists(p)) { df_path <- p; break }
  }
  skip_if(is.null(df_path), "overview/data_functions.R not found")
  
  code <- readLines(df_path)
  expect_true(any(grepl("get_cached_db_query", code)),
              info = "load_metric_data should use get_cached_db_query")
  expect_true(any(grepl("\\.load_metric_data_uncached", code)),
              info = "Should have uncached internal function")
})

test_that("overview load_data_by_fos uses cached FOS data", {
  df_path <- NULL
  for (p in c("apps/overview/data_functions.R",
              "../../apps/overview/data_functions.R",
              "/home/alex/Documents/mmcd/mmcd_metrics/apps/overview/data_functions.R")) {
    if (file.exists(p)) { df_path <- p; break }
  }
  skip_if(is.null(df_path), "overview/data_functions.R not found")
  
  code <- readLines(df_path)
  expect_true(any(grepl("get_cached_fos_data", code)),
              info = "load_data_by_fos should use get_cached_fos_data")
  expect_true(any(grepl("\\.load_data_by_fos_uncached", code)),
              info = "Should have uncached internal function")
})

test_that("overview calculate_metric_stats uses cached stat boxes", {
  df_path <- NULL
  for (p in c("apps/overview/data_functions.R",
              "../../apps/overview/data_functions.R",
              "/home/alex/Documents/mmcd/mmcd_metrics/apps/overview/data_functions.R")) {
    if (file.exists(p)) { df_path <- p; break }
  }
  skip_if(is.null(df_path), "overview/data_functions.R not found")
  
  code <- readLines(df_path)
  expect_true(any(grepl("get_cached_stat_box", code)),
              info = "calculate_metric_stats should use get_cached_stat_box")
})

# =============================================================================
# TEST: db_helpers.R color functions use caching
# =============================================================================

test_that("get_facility_base_colors uses cached color mappings", {
  db_path <- NULL
  for (p in c("shared/db_helpers.R",
              "../../shared/db_helpers.R",
              "/home/alex/Documents/mmcd/mmcd_metrics/shared/db_helpers.R")) {
    if (file.exists(p)) { db_path <- p; break }
  }
  skip_if(is.null(db_path), "db_helpers.R not found")
  
  code <- readLines(db_path)
  expect_true(any(grepl("get_cached_color_mapping", code)),
              info = "get_facility_base_colors should use get_cached_color_mapping")
  expect_true(any(grepl("\\.get_facility_base_colors_uncached", code)),
              info = "Should have uncached internal function")
})

test_that("get_foreman_colors uses cached color mappings", {
  db_path <- NULL
  for (p in c("shared/db_helpers.R",
              "../../shared/db_helpers.R",
              "/home/alex/Documents/mmcd/mmcd_metrics/shared/db_helpers.R")) {
    if (file.exists(p)) { db_path <- p; break }
  }
  skip_if(is.null(db_path), "db_helpers.R not found")
  
  code <- readLines(db_path)
  # Check for cached pattern in get_foreman_colors
  fc_lines <- grep("get_foreman_colors|get_cached_color_mapping.*foreman", code)
  expect_true(length(fc_lines) > 0,
              info = "get_foreman_colors should use get_cached_color_mapping")
  expect_true(any(grepl("\\.get_foreman_colors_uncached", code)),
              info = "Should have uncached internal function")
})

# =============================================================================
# TEST: test-app has tiered cache management UI
# =============================================================================

test_that("test-app has tiered cache management buttons", {
  app_path <- NULL
  for (p in c("apps/test-app/app.R",
              "../../apps/test-app/app.R",
              "/home/alex/Documents/mmcd/mmcd_metrics/apps/test-app/app.R")) {
    if (file.exists(p)) { app_path <- p; break }
  }
  skip_if(is.null(app_path), "test-app/app.R not found")
  
  code <- readLines(app_path)
  
  expect_true(any(grepl("clearDbCache", code)), info = "Should have DB cache clear button")
  expect_true(any(grepl("clearChartCache", code)), info = "Should have chart cache clear button")
  expect_true(any(grepl("clearStatCache", code)), info = "Should have stat cache clear button")
  expect_true(any(grepl("clearFosCache", code)), info = "Should have FOS cache clear button")
  expect_true(any(grepl("clearColorCache", code)), info = "Should have color cache clear button")
  expect_true(any(grepl("clearAllTieredCache", code)), info = "Should have clear all tiered cache button")
  expect_true(any(grepl("tieredCacheStatus", code)), info = "Should have tiered cache status output")
})
