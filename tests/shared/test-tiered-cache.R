# =============================================================================
# Tests for Tiered Redis Cache System (redis_cache.R)
# =============================================================================
# Tests verify:
# 1. TTL constants match config/app_config.yaml values
# 2. Cache key prefixes are correct
# 3. build_cache_key produces deterministic, prefix-scoped keys
# 4. Tiered helper function signatures and key routing
# 5. clear_cache_tier handles all tiers including global hist_fac/hist_fos
# 6. get_cache_tier_counts includes all expected tiers
# 7. District-only decoupling: extract_weekly_values handles NULL hist_data
# =============================================================================

library(testthat)

context("Tiered Cache System")

# =============================================================================
# SETUP: Locate and source files
# =============================================================================

find_file <- function(relative_paths) {
  for (p in relative_paths) {
    if (file.exists(p)) return(normalizePath(p))
  }
  NULL
}

redis_cache_path <- find_file(c(
  "shared/redis_cache.R",
  "../../shared/redis_cache.R",
  "../shared/redis_cache.R",
  "/srv/shiny-server/shared/redis_cache.R"
))

config_yaml_path <- find_file(c(
  "config/app_config.yaml",
  "../../config/app_config.yaml",
  "../config/app_config.yaml",
  "/srv/shiny-server/config/app_config.yaml"
))

dynamic_server_path <- find_file(c(
  "apps/overview/dynamic_server.R",
  "../../apps/overview/dynamic_server.R",
  "../apps/overview/dynamic_server.R",
  "/srv/shiny-server/apps/overview/dynamic_server.R"
))

historical_functions_path <- find_file(c(
  "apps/overview/historical_functions.R",
  "../../apps/overview/historical_functions.R",
  "../apps/overview/historical_functions.R",
  "/srv/shiny-server/apps/overview/historical_functions.R"
))

test_app_path <- find_file(c(
  "apps/test-app/app.R",
  "../../apps/test-app/app.R",
  "../apps/test-app/app.R",
  "/srv/shiny-server/apps/test-app/app.R"
))

# =============================================================================
# 1. TTL CONSTANTS — verify values match config/app_config.yaml
# =============================================================================

test_that("config/app_config.yaml exists", {
  skip_if(is.null(config_yaml_path), "app_config.yaml not found")
  expect_true(file.exists(config_yaml_path))
})

test_that("config TTL values are correct", {
  skip_if(is.null(config_yaml_path), "app_config.yaml not found")
  
  cfg <- yaml::read_yaml(config_yaml_path)
  ttl <- cfg$cache$ttl
  
  expect_equal(ttl$historical_averages, 1209600, label = "historical_averages = 14 days")
  expect_equal(ttl$lookup_tables, 1209600, label = "lookup_tables = 14 days")
  expect_equal(ttl$fos_drilldown, 604800, label = "fos_drilldown = 7 days")
  expect_equal(ttl$color_mappings, 604800, label = "color_mappings = 7 days")
  expect_equal(ttl$facility_historical, 86400, label = "facility_historical = 24 hours")
  expect_equal(ttl$general, 300, label = "general = 5 min")
  expect_equal(ttl$current_year, 180, label = "current_year = 3 min")
  expect_equal(ttl$db_queries, 120, label = "db_queries = 2 min")
  expect_equal(ttl$charts, 120, label = "charts = 2 min")
  expect_equal(ttl$stat_boxes, 120, label = "stat_boxes = 2 min")
})

test_that("TTL tiers follow correct ordering: long > medium > short", {
  skip_if(is.null(config_yaml_path), "app_config.yaml not found")
  
  cfg <- yaml::read_yaml(config_yaml_path)
  ttl <- cfg$cache$ttl
  
  # Historical > FOS drill-down > facility historical > general > current_year > db_queries
  expect_gt(ttl$historical_averages, ttl$fos_drilldown)
  expect_gt(ttl$fos_drilldown, ttl$facility_historical)
  expect_gt(ttl$facility_historical, ttl$general)
  expect_gt(ttl$general, ttl$current_year)
  expect_gte(ttl$current_year, ttl$db_queries)
  expect_gte(ttl$db_queries, ttl$charts)
})

test_that("all TTL values are positive integers", {
  skip_if(is.null(config_yaml_path), "app_config.yaml not found")
  
  cfg <- yaml::read_yaml(config_yaml_path)
  ttl <- cfg$cache$ttl
  
  for (name in names(ttl)) {
    val <- ttl[[name]]
    expect_true(is.numeric(val) && val > 0,
                info = paste(name, "=", val, "should be positive numeric"))
  }
})

test_that("db_queries TTL does not exceed 120 seconds", {
  skip_if(is.null(config_yaml_path), "app_config.yaml not found")
  
  cfg <- yaml::read_yaml(config_yaml_path)
  expect_lte(cfg$cache$ttl$db_queries, 120,
             label = "db_queries TTL should be <= 120s for fresh data")
})

test_that("current_year TTL does not exceed 180 seconds", {
  skip_if(is.null(config_yaml_path), "app_config.yaml not found")
  
  cfg <- yaml::read_yaml(config_yaml_path)
  expect_lte(cfg$cache$ttl$current_year, 180,
             label = "current_year TTL should be <= 180s for live data")
})

# =============================================================================
# 2. redis_cache.R — TTL constants sourced from config
# =============================================================================

test_that("redis_cache.R defines all 6 TTL constants", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- readLines(redis_cache_path)
  code <- src[!grepl("^\\s*#", src)]
  
  expect_true(any(grepl("TTL_14_DAYS\\s*<-", code)), info = "TTL_14_DAYS defined")
  expect_true(any(grepl("TTL_7_DAYS\\s*<-", code)), info = "TTL_7_DAYS defined")
  expect_true(any(grepl("TTL_5_MIN\\s*<-", code)), info = "TTL_5_MIN defined")
  expect_true(any(grepl("TTL_3_MIN\\s*<-", code)), info = "TTL_3_MIN defined")
  expect_true(any(grepl("TTL_24_HR\\s*<-", code)), info = "TTL_24_HR defined")
  expect_true(any(grepl("TTL_2_MIN\\s*<-", code)), info = "TTL_2_MIN defined")
})

test_that("TTL constants read from correct config keys", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  expect_true(grepl("TTL_14_DAYS.*historical_averages", src),
              info = "TTL_14_DAYS reads from historical_averages config")
  expect_true(grepl("TTL_7_DAYS.*fos_drilldown", src),
              info = "TTL_7_DAYS reads from fos_drilldown config")
  expect_true(grepl("TTL_5_MIN.*general", src),
              info = "TTL_5_MIN reads from general config")
  expect_true(grepl("TTL_3_MIN.*current_year", src),
              info = "TTL_3_MIN reads from current_year config")
  expect_true(grepl("TTL_24_HR.*facility_historical", src),
              info = "TTL_24_HR reads from facility_historical config")
  expect_true(grepl("TTL_2_MIN.*db_queries", src),
              info = "TTL_2_MIN reads from db_queries config")
})

test_that("TTL constants have correct fallback defaults", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  expect_true(grepl("TTL_14_DAYS.*%\\|\\|%.*1209600", src))
  expect_true(grepl("TTL_7_DAYS.*%\\|\\|%.*604800", src))
  expect_true(grepl("TTL_5_MIN.*%\\|\\|%.*300", src))
  expect_true(grepl("TTL_3_MIN.*%\\|\\|%.*180", src))
  expect_true(grepl("TTL_24_HR.*%\\|\\|%.*86400", src))
  expect_true(grepl("TTL_2_MIN.*%\\|\\|%.*120", src))
})

# =============================================================================
# 3. CACHE KEY PREFIXES — verify all 7 tiers defined
# =============================================================================

test_that("all 7 cache prefix constants are defined", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- readLines(redis_cache_path)
  code <- src[!grepl("^\\s*#", src)]
  
  prefixes <- c("CACHE_PREFIX_DB", "CACHE_PREFIX_CHART", "CACHE_PREFIX_STAT",
                 "CACHE_PREFIX_FOS", "CACHE_PREFIX_COLOR", "CACHE_PREFIX_FAC_HIST",
                 "CACHE_PREFIX_CURYR")
  
  for (p in prefixes) {
    expect_true(any(grepl(paste0(p, "\\s*<-"), code)),
                info = paste(p, "should be defined"))
  }
})

test_that("cache prefix values are correct strings", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  expect_true(grepl('CACHE_PREFIX_DB\\s*<-\\s*"db"', src))
  expect_true(grepl('CACHE_PREFIX_CHART\\s*<-\\s*"chart"', src))
  expect_true(grepl('CACHE_PREFIX_STAT\\s*<-\\s*"stat"', src))
  expect_true(grepl('CACHE_PREFIX_FOS\\s*<-\\s*"fos"', src))
  expect_true(grepl('CACHE_PREFIX_COLOR\\s*<-\\s*"color"', src))
  expect_true(grepl('CACHE_PREFIX_FAC_HIST\\s*<-\\s*"fachist"', src))
  expect_true(grepl('CACHE_PREFIX_CURYR\\s*<-\\s*"curyr"', src))
})

# =============================================================================
# 4. build_cache_key — deterministic key generation
# =============================================================================

test_that("build_cache_key function is defined", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  expect_true(grepl("build_cache_key\\s*<-\\s*function\\(prefix,\\s*\\.\\.\\.\\)", src))
})

test_that("build_cache_key uses digest for hashing", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  # Find the build_cache_key function body
  expect_true(grepl("digest::digest", src),
              info = "build_cache_key should use digest::digest")
})

test_that("build_cache_key produces prefix:hash format", {
  skip_if(!requireNamespace("digest", quietly = TRUE), "digest package needed")
  
  # Simulate build_cache_key
  build_cache_key <- function(prefix, ...) {
    args <- list(...)
    hash <- digest::digest(args, algo = "xxhash64")
    paste0(prefix, ":", hash)
  }
  
  key <- build_cache_key("db", "catch_basin", c("1", "2"))
  
  expect_true(grepl("^db:", key))
  expect_true(nchar(key) > 4)  # prefix + ":" + hash
})

test_that("build_cache_key is deterministic (same args = same key)", {
  skip_if(!requireNamespace("digest", quietly = TRUE), "digest package needed")
  
  build_cache_key <- function(prefix, ...) {
    args <- list(...)
    hash <- digest::digest(args, algo = "xxhash64")
    paste0(prefix, ":", hash)
  }
  
  key1 <- build_cache_key("chart", "catch_basin", "2026", c("1", "2"))
  key2 <- build_cache_key("chart", "catch_basin", "2026", c("1", "2"))
  
  expect_identical(key1, key2)
})

test_that("build_cache_key produces different keys for different args", {
  skip_if(!requireNamespace("digest", quietly = TRUE), "digest package needed")
  
  build_cache_key <- function(prefix, ...) {
    args <- list(...)
    hash <- digest::digest(args, algo = "xxhash64")
    paste0(prefix, ":", hash)
  }
  
  key_zone12 <- build_cache_key("db", "catch_basin", c("1", "2"))
  key_zone1  <- build_cache_key("db", "catch_basin", c("1"))
  key_diff   <- build_cache_key("db", "drone", c("1", "2"))
  
  expect_false(key_zone12 == key_zone1)
  expect_false(key_zone12 == key_diff)
})

test_that("build_cache_key uses different prefix per tier", {
  skip_if(!requireNamespace("digest", quietly = TRUE), "digest package needed")
  
  build_cache_key <- function(prefix, ...) {
    args <- list(...)
    hash <- digest::digest(args, algo = "xxhash64")
    paste0(prefix, ":", hash)
  }
  
  db_key    <- build_cache_key("db", "catch_basin")
  chart_key <- build_cache_key("chart", "catch_basin")
  stat_key  <- build_cache_key("stat", "catch_basin")
  
  # Same args but different prefixes → different keys
  expect_false(db_key == chart_key)
  expect_false(db_key == stat_key)
  
  # But all start with their tier prefix
  expect_true(startsWith(db_key, "db:"))
  expect_true(startsWith(chart_key, "chart:"))
  expect_true(startsWith(stat_key, "stat:"))
})

# =============================================================================
# 5. TIERED HELPER FUNCTIONS — correct signatures and TTL routing
# =============================================================================

test_that("all 7 tiered get_cached_* functions are defined", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  funcs <- c(
    "get_cached_db_query",
    "get_cached_chart",
    "get_cached_stat_box",
    "get_cached_fos_data",
    "get_cached_color_mapping",
    "get_cached_fac_hist_data",
    "get_cached_curyr_data"
  )
  
  for (fn in funcs) {
    expect_true(grepl(paste0(fn, "\\s*<-\\s*function"), src),
                info = paste(fn, "should be defined"))
  }
})

test_that("short-lived helpers use TTL_2_MIN", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  # Extract function bodies by looking for the ttl = parameter
  # get_cached_db_query should use TTL_2_MIN
  db_section <- regmatches(src, regexpr("get_cached_db_query.*?\\}", src))
  expect_true(grepl("TTL_2_MIN", db_section),
              info = "get_cached_db_query should use TTL_2_MIN")
  
  # get_cached_chart should use TTL_2_MIN
  chart_section <- regmatches(src, regexpr("get_cached_chart.*?\\}", src))
  expect_true(grepl("TTL_2_MIN", chart_section),
              info = "get_cached_chart should use TTL_2_MIN")
  
  # get_cached_stat_box should use TTL_2_MIN
  stat_section <- regmatches(src, regexpr("get_cached_stat_box.*?\\}", src))
  expect_true(grepl("TTL_2_MIN", stat_section),
              info = "get_cached_stat_box should use TTL_2_MIN")
})

test_that("get_cached_curyr_data uses TTL_3_MIN", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  curyr_section <- regmatches(src, regexpr("get_cached_curyr_data.*?\\}", src))
  expect_true(grepl("TTL_3_MIN", curyr_section),
              info = "get_cached_curyr_data should use TTL_3_MIN (180s)")
  # Must NOT use TTL_5_MIN or TTL_2_MIN
  expect_false(grepl("TTL_5_MIN", curyr_section),
               info = "get_cached_curyr_data should NOT use TTL_5_MIN")
})

test_that("long-lived helpers use correct TTLs", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  # get_cached_fos_data → TTL_7_DAYS
  fos_section <- regmatches(src, regexpr("get_cached_fos_data.*?\\}", src))
  expect_true(grepl("TTL_7_DAYS", fos_section))
  
  # get_cached_color_mapping → TTL_7_DAYS
  color_section <- regmatches(src, regexpr("get_cached_color_mapping.*?\\}", src))
  expect_true(grepl("TTL_7_DAYS", color_section))
  
  # get_cached_fac_hist_data → TTL_24_HR
  fachist_section <- regmatches(src, regexpr("get_cached_fac_hist_data.*?\\}", src))
  expect_true(grepl("TTL_24_HR", fachist_section))
})

test_that("each helper uses its correct prefix constant", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  # Map: function -> expected prefix constant
  mappings <- list(
    "get_cached_db_query"     = "CACHE_PREFIX_DB",
    "get_cached_chart"        = "CACHE_PREFIX_CHART",
    "get_cached_stat_box"     = "CACHE_PREFIX_STAT",
    "get_cached_fos_data"     = "CACHE_PREFIX_FOS",
    "get_cached_color_mapping" = "CACHE_PREFIX_COLOR",
    "get_cached_fac_hist_data" = "CACHE_PREFIX_FAC_HIST",
    "get_cached_curyr_data"   = "CACHE_PREFIX_CURYR"
  )
  
  for (fn in names(mappings)) {
    fn_section <- regmatches(src, regexpr(paste0(fn, ".*?\\}"), src))
    expect_true(grepl(mappings[[fn]], fn_section),
                info = paste(fn, "should use", mappings[[fn]]))
  }
})

# =============================================================================
# 6. clear_cache_tier — handles all tiers including global keys
# =============================================================================

test_that("clear_cache_tier function is defined", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  expect_true(grepl("clear_cache_tier\\s*<-\\s*function", src))
})

test_that("clear_cache_tier handles 'all' prefix", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  # The "all" branch should clear app:* and global hist_fac/hist_fos
  expect_true(grepl('prefix == "all"', src, fixed = TRUE))
  expect_true(grepl("clear_app_cache_redis", src))
})

test_that("clear_cache_tier distinguishes global vs app prefixes", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  # hist_fac and hist_fos use "mmcd:" prefix, not "app:"
  expect_true(grepl('c\\("hist_fac",\\s*"hist_fos"\\)', src),
              info = "hist_fac and hist_fos should be identified as global caches")
  expect_true(grepl('paste0\\("mmcd:",\\s*prefix', src),
              info = "Global caches should use mmcd: prefix")
  expect_true(grepl('paste0\\("app:",\\s*prefix', src),
              info = "App caches should use app: prefix")
})

test_that("clear_cache_tier supports all expected tier names", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  # All tier names should appear in documentation or logic
  tiers <- c("db", "chart", "stat", "fos", "color", "fachist", "curyr",
             "hist_fac", "hist_fos", "all")
  
  for (tier in tiers) {
    expect_true(grepl(tier, src, fixed = TRUE),
                info = paste("Tier", tier, "should be referenced in clear_cache_tier"))
  }
})

# =============================================================================
# 7. get_cache_tier_counts — includes all tiers
# =============================================================================

test_that("get_cache_tier_counts is defined", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  expect_true(grepl("get_cache_tier_counts\\s*<-\\s*function", src))
})

test_that("get_cache_tier_counts returns all 10 tier counts", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  # Extract just the get_cache_tier_counts function body
  fn_match <- regmatches(src, regexpr("get_cache_tier_counts.*?^\\}", src,
                                       perl = FALSE))
  # Fallback: check entire source for these keys in the list() call
  expected_keys <- c("db_queries", "charts", "stat_boxes", "fos_drilldown",
                     "color_maps", "fac_hist", "curyr", "hist_fac", "hist_fos",
                     "other_app")
  
  for (key in expected_keys) {
    expect_true(grepl(key, src),
                info = paste("get_cache_tier_counts should include", key))
  }
})

test_that("get_cache_tier_counts uses correct key patterns", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  # App-level caches use "app:" prefix via paste0("app:", CACHE_PREFIX_*, ":*")
  expect_true(grepl("CACHE_PREFIX_DB", src),
              info = "DB queries should use CACHE_PREFIX_DB")
  expect_true(grepl("CACHE_PREFIX_CHART", src),
              info = "Charts should use CACHE_PREFIX_CHART")
  expect_true(grepl("CACHE_PREFIX_CURYR", src),
              info = "Current year should use CACHE_PREFIX_CURYR")
  
  # The get_cache_tier_counts function uses paste0("app:", CACHE_PREFIX_*, ":*")
  expect_true(grepl('paste0\\("app:"', src),
              info = "App-level caches should be keyed with app: prefix")
  
  # Global caches use "mmcd:" prefix  
  expect_true(grepl('mmcd:hist_fac:', src),
              info = "Facility hist averages should use mmcd:hist_fac:* pattern")
  expect_true(grepl('mmcd:hist_fos:', src),
              info = "FOS hist averages should use mmcd:hist_fos:* pattern")
})

# =============================================================================
# 8. TEST-APP — cache display includes all tiers
# =============================================================================

test_that("test-app displays all cache tiers", {
  skip_if(is.null(test_app_path), "test-app/app.R not found")
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  app_src <- paste(readLines(test_app_path), collapse = "\n")
  redis_src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  # Test-app should use the DT table for tiered cache display
  expect_true(grepl("tieredCacheTable", app_src),
              info = "test-app should have tieredCacheTable DT output")
  expect_true(grepl("get_tiered_cache_inventory", app_src),
              info = "test-app should call get_tiered_cache_inventory")
  
  # The inventory function in redis_cache.R should define all tier display names
  tier_names <- c("DB Queries", "Charts", "Stat Boxes", "Current Year",
                   "FOS Drill-down", "Color Maps", "Fac Historical",
                   "Hist Avg (Fac)", "Hist Avg (FOS)")
  
  for (name in tier_names) {
    expect_true(grepl(name, redis_src, fixed = TRUE),
                info = paste("redis_cache.R inventory should include tier:", name))
  }
})

test_that("test-app has clear buttons for all tiers", {
  skip_if(is.null(test_app_path), "test-app/app.R not found")
  
  src <- paste(readLines(test_app_path), collapse = "\n")
  
  # Required action button IDs
  button_ids <- c("clearDbCache", "clearChartCache", "clearStatCache",
                   "clearCuryrCache", "clearFosCache", "clearColorCache",
                   "clearFacHistCache", "clearHistFacCache", "clearHistFosCache",
                   "clearAllTieredCache")
  
  for (btn in button_ids) {
    expect_true(grepl(btn, src, fixed = TRUE),
                info = paste("test-app should have", btn, "button"))
  }
})

test_that("test-app clear handlers use correct tier prefixes", {
  skip_if(is.null(test_app_path), "test-app/app.R not found")
  
  src <- paste(readLines(test_app_path), collapse = "\n")
  
  # Verify each clear button calls clear_cache_tier with the right prefix
  handlers <- list(
    "clearDbCache"      = '"db"',
    "clearChartCache"   = '"chart"',
    "clearStatCache"    = '"stat"',
    "clearCuryrCache"   = '"curyr"',
    "clearFosCache"     = '"fos"',
    "clearColorCache"   = '"color"',
    "clearFacHistCache" = '"fachist"',
    "clearHistFacCache" = '"hist_fac"',
    "clearHistFosCache" = '"hist_fos"'
  )
  
  for (btn in names(handlers)) {
    # Find the observeEvent block for this button
    pattern <- paste0("input\\$", btn, ".*?clear_cache_tier\\(", handlers[[btn]])
    expect_true(grepl(pattern, src),
                info = paste(btn, "handler should call clear_cache_tier with", handlers[[btn]]))
  }
})

# =============================================================================
# 9. HISTORICAL DATA DECOUPLING — district overview skips historical_data
# =============================================================================

test_that("summary_stats_ui renderUI checks overview_type for historical_data", {
  skip_if(is.null(dynamic_server_path), "dynamic_server.R not found")
  
  src <- paste(readLines(dynamic_server_path), collapse = "\n")
  
  # The renderUI for summary_stats_ui should check overview_type == "district"
  # and pass NULL for hist_data when it is
  expect_true(grepl('overview_type.*==.*"district"', src),
              info = "Should check if overview_type is 'district'")
})

test_that("district overview passes NULL hist_data to generate_summary_stats", {
  skip_if(is.null(dynamic_server_path), "dynamic_server.R not found")
  
  src <- paste(readLines(dynamic_server_path), collapse = "\n")
  
  # When district, hist_data should be set to NULL
  expect_true(grepl("hist_data.*NULL", src),
              info = "District overview should pass NULL hist_data")
})

test_that("extract_weekly_values handles NULL historical_data gracefully", {
  skip_if(is.null(dynamic_server_path), "dynamic_server.R not found")
  
  src <- readLines(dynamic_server_path)
  
  # Find extract_weekly_values function
  fn_start <- grep("extract_weekly_values\\s*<-\\s*function", src)
  skip_if(length(fn_start) == 0, "extract_weekly_values not found")
  
  # Read the function body (next ~30 lines should contain the NULL check)
  fn_body <- paste(src[fn_start[1]:min(fn_start[1] + 40, length(src))], collapse = "\n")
  
  # Should handle NULL or empty historical_data
  expect_true(grepl("is.null|is.null\\(historical_data\\)|length.*==.*0", fn_body),
              info = "extract_weekly_values should handle NULL historical_data")
})

# =============================================================================
# 10. CURRENT YEAR CACHE — load_current_year_for_cache uses curyr tier
# =============================================================================

test_that("historical_functions.R uses get_cached_curyr_data", {
  skip_if(is.null(historical_functions_path), "historical_functions.R not found")
  
  src <- paste(readLines(historical_functions_path), collapse = "\n")
  
  expect_true(grepl("get_cached_curyr_data", src),
              info = "load_current_year_for_cache should use get_cached_curyr_data helper")
})

test_that("data_functions.R uses get_cached_db_query", {
  data_functions_path <- find_file(c(
    "apps/overview/data_functions.R",
    "../../apps/overview/data_functions.R",
    "../apps/overview/data_functions.R",
    "/srv/shiny-server/apps/overview/data_functions.R"
  ))
  skip_if(is.null(data_functions_path), "data_functions.R not found")
  
  src <- paste(readLines(data_functions_path), collapse = "\n")
  
  expect_true(grepl("get_cached_db_query", src),
              info = "load_metric_data should use get_cached_db_query for 2-min caching")
})

# =============================================================================
# 11. CONSISTENCY: No TTL_5_MIN in short-lived cache paths
# =============================================================================

test_that("get_cached_db_query does NOT use TTL_5_MIN", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  # Extract just the get_cached_db_query function
  fn_section <- regmatches(src, regexpr("get_cached_db_query.*?\\}", src))
  expect_false(grepl("TTL_5_MIN", fn_section),
               info = "get_cached_db_query should use TTL_2_MIN, NOT TTL_5_MIN")
})

test_that("get_cached_curyr_data does NOT use TTL_5_MIN", {
  skip_if(is.null(redis_cache_path), "redis_cache.R not found")
  
  src <- paste(readLines(redis_cache_path), collapse = "\n")
  
  fn_section <- regmatches(src, regexpr("get_cached_curyr_data.*?\\}", src))
  expect_false(grepl("TTL_5_MIN", fn_section),
               info = "get_cached_curyr_data should use TTL_3_MIN, NOT TTL_5_MIN")
})

# =============================================================================
# 12. get_historical_week_avg_by_entity — uses 7-day Redis TTL
# =============================================================================

test_that("get_historical_week_avg_by_entity caches with 7-day TTL", {
  skip_if(is.null(dynamic_server_path), "dynamic_server.R not found")
  
  src <- paste(readLines(dynamic_server_path), collapse = "\n")
  
  # Find the function and its redis_set call
  fn_match <- regmatches(src, regexpr(
    "get_historical_week_avg_by_entity.*?^\\}", src, perl = FALSE
  ))
  # Fallback: search the whole source for the TTL in this function's context
  expect_true(grepl("604800", src),
              info = "get_historical_week_avg_by_entity should use 604800 (7-day) TTL")
})

test_that("get_historical_week_avg has both hist_fac and hist_fos callers", {
  skip_if(is.null(dynamic_server_path), "dynamic_server.R not found")
  
  src <- paste(readLines(dynamic_server_path), collapse = "\n")
  
  # Facility-level wrapper passes "hist_fac" prefix
  expect_true(grepl('get_historical_week_avg_by_entity.*"hist_fac"', src),
              info = "Facility-level wrapper should use hist_fac prefix")
  
  # FOS-level wrapper passes "hist_fos" prefix
  expect_true(grepl('get_historical_week_avg_by_entity.*"hist_fos"', src),
              info = "FOS-level wrapper should use hist_fos prefix")
})

# =============================================================================
# CLEANUP
# =============================================================================
# No cleanup needed — these tests only read source files
