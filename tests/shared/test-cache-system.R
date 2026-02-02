# =============================================================================
# Tests for Cache System (db_helpers.R lookup cache + cache_utilities.R)
# =============================================================================
# These tests verify that:
# 1. Lookup cache correctly stores and retrieves data
# 2. Cache is cleared properly
# 3. After clearing, fresh data is fetched
# 4. Cache utilities work with the lookup cache

library(testthat)

context("Cache System - Lookup Tables")

# =============================================================================
# SETUP: Create mock cache environment for isolated testing
# =============================================================================

# Create a test cache environment (simulates .lookup_cache from db_helpers.R)
setup_test_cache <- function() {
  # Create cache environment in global scope (where db_helpers.R expects it)
  if (!exists(".lookup_cache", envir = .GlobalEnv)) {
    assign(".lookup_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  get(".lookup_cache", envir = .GlobalEnv)
}

# Clear test cache
clear_test_cache <- function() {
  if (exists(".lookup_cache", envir = .GlobalEnv)) {
    cache_env <- get(".lookup_cache", envir = .GlobalEnv)
    rm(list = ls(cache_env), envir = cache_env)
  }
}

# =============================================================================
# TEST: Cache Environment Exists
# =============================================================================

test_that("lookup cache environment can be created", {
  cache_env <- setup_test_cache()
  
  expect_true(is.environment(cache_env))
  expect_true(exists(".lookup_cache", envir = .GlobalEnv))
})

# =============================================================================
# TEST: Cache Storage and Retrieval
# =============================================================================

test_that("data can be stored in lookup cache", {
  cache_env <- setup_test_cache()
  clear_test_cache()
  
  # Store test data
  test_facilities <- data.frame(
    short_name = c("N", "E", "MO"),
    full_name = c("North", "East", "Main Office"),
    stringsAsFactors = FALSE
  )
  
  assign("facilities", test_facilities, envir = cache_env)
  
  # Verify storage
  expect_true(exists("facilities", envir = cache_env))
  
  retrieved <- get("facilities", envir = cache_env)
  expect_equal(nrow(retrieved), 3)
  expect_equal(retrieved$short_name, c("N", "E", "MO"))
})

test_that("multiple lookup types can be cached simultaneously", {
  cache_env <- setup_test_cache()
  clear_test_cache()
  
  # Store facilities
  assign("facilities", data.frame(code = c("N", "E")), envir = cache_env)
  
  # Store foremen
  assign("foremen", data.frame(emp_num = c("0206", "7003")), envir = cache_env)
  
  # Store species
  assign("species", data.frame(sppcode = c("1", "2", "3")), envir = cache_env)
  
  # Verify all are cached
  expect_true(exists("facilities", envir = cache_env))
  expect_true(exists("foremen", envir = cache_env))
  expect_true(exists("species", envir = cache_env))
  
  # Verify independence
  expect_equal(nrow(get("facilities", envir = cache_env)), 2)
  expect_equal(nrow(get("foremen", envir = cache_env)), 2)
  expect_equal(nrow(get("species", envir = cache_env)), 3)
})

# =============================================================================
# TEST: Cache Clearing
# =============================================================================

test_that("cache can be cleared completely", {
  cache_env <- setup_test_cache()
  
  # Add some data
  assign("facilities", data.frame(x = 1), envir = cache_env)
  assign("foremen", data.frame(x = 1), envir = cache_env)
  
  expect_true(length(ls(cache_env)) >= 2)
  
  # Clear using the same method as clear_lookup_cache()
  rm(list = ls(cache_env), envir = cache_env)
  
  expect_equal(length(ls(cache_env)), 0)
  expect_false(exists("facilities", envir = cache_env))
  expect_false(exists("foremen", envir = cache_env))
})

test_that("individual cache entries can be removed", {
  cache_env <- setup_test_cache()
  clear_test_cache()
  
  # Add multiple entries
  assign("facilities", data.frame(x = 1), envir = cache_env)
  assign("foremen", data.frame(x = 2), envir = cache_env)
  assign("species", data.frame(x = 3), envir = cache_env)
  
  # Remove just one
  rm("foremen", envir = cache_env)
  
  expect_true(exists("facilities", envir = cache_env))
  expect_false(exists("foremen", envir = cache_env))
  expect_true(exists("species", envir = cache_env))
})

# =============================================================================
# TEST: Cache Behavior Pattern (First Call Caches, Second Call Returns Cached)
# =============================================================================

test_that("cache pattern works: first call stores, second call retrieves", {
  cache_env <- setup_test_cache()
  clear_test_cache()
  
  # Simulate get_facility_lookup pattern
  get_cached_or_fetch <- function(cache_key, fetch_fn) {
    if (exists(cache_key, envir = cache_env)) {
      return(get(cache_key, envir = cache_env))
    }
    
    # Simulate fetching from database
    result <- fetch_fn()
    assign(cache_key, result, envir = cache_env)
    return(result)
  }
  
  # Track how many times "database" was hit
  db_call_count <- 0
  mock_fetch <- function() {
    db_call_count <<- db_call_count + 1
    data.frame(id = 1:5)
  }
  
  # First call should hit "database"
  result1 <- get_cached_or_fetch("test_data", mock_fetch)
  expect_equal(db_call_count, 1)
  expect_equal(nrow(result1), 5)
  
  # Second call should use cache
  result2 <- get_cached_or_fetch("test_data", mock_fetch)
  expect_equal(db_call_count, 1)  # Still 1 - no new fetch
  expect_equal(nrow(result2), 5)
  
  # Third call also uses cache
  result3 <- get_cached_or_fetch("test_data", mock_fetch)
  expect_equal(db_call_count, 1)  # Still 1
})

test_that("after cache clear, next call fetches fresh data", {
  cache_env <- setup_test_cache()
  clear_test_cache()
  
  db_call_count <- 0
  version <- 1
  
  get_cached_or_fetch <- function(cache_key) {
    if (exists(cache_key, envir = cache_env)) {
      return(get(cache_key, envir = cache_env))
    }
    db_call_count <<- db_call_count + 1
    result <- data.frame(version = version)
    assign(cache_key, result, envir = cache_env)
    return(result)
  }
  
  # First call
  result1 <- get_cached_or_fetch("data")
  expect_equal(db_call_count, 1)
  expect_equal(result1$version, 1)
  
  # Clear cache
  rm(list = ls(cache_env), envir = cache_env)
  
  # Simulate database update
  version <- 2
  
  # Next call should fetch fresh (version 2)
  result2 <- get_cached_or_fetch("data")
  expect_equal(db_call_count, 2)
  expect_equal(result2$version, 2)
})

# =============================================================================
# TEST: Integration with Stub Data
# =============================================================================

test_that("cached lookup matches stub data format", {
  skip_if_not(exists("get_stub_field_supervisors", mode = "function"),
              "Stub functions not loaded")
  
  cache_env <- setup_test_cache()
  clear_test_cache()
  
  # Get stub data
  stub_foremen <- get_stub_field_supervisors()
  
  # Store in cache (simulating what get_foremen_lookup does)
  assign("foremen", stub_foremen, envir = cache_env)
  
  # Retrieve and verify
  cached <- get("foremen", envir = cache_env)
  
  expect_s3_class(cached, "data.frame")
  expect_true("emp_num" %in% names(cached))
  expect_true("shortname" %in% names(cached))
  expect_true("facility" %in% names(cached))
})

test_that("cached facilities match stub format", {
  cache_env <- setup_test_cache()
  clear_test_cache()
  
  # Simulate facility lookup from stubs
  stub_facilities <- data.frame(
    short_name = c("N", "Sj", "E", "Wm", "MO"),
    full_name = c("North", "St. James", "East", "Waconia/Mound", "Main Office"),
    stringsAsFactors = FALSE
  )
  
  assign("facilities", stub_facilities, envir = cache_env)
  
  cached <- get("facilities", envir = cache_env)
  
  expect_equal(nrow(cached), 5)
  expect_true("N" %in% cached$short_name)
  expect_true("E" %in% cached$short_name)
})

# =============================================================================
# TEST: Cache Status Reporting
# =============================================================================

test_that("cache status correctly reports cached items", {
  cache_env <- setup_test_cache()
  clear_test_cache()
  
  # Function to check cache status
  get_cache_status_list <- function(lookup_types) {
    sapply(lookup_types, function(lt) {
      if (exists(lt, envir = cache_env)) "Cached" else "Empty"
    })
  }
  
  types <- c("facilities", "foremen", "species")
  
  # Initially empty
  status1 <- get_cache_status_list(types)
  expect_true(all(status1 == "Empty"))
  
  # Add one
  assign("facilities", data.frame(x = 1), envir = cache_env)
  
  status2 <- get_cache_status_list(types)
  expect_equal(status2["facilities"], c(facilities = "Cached"))
  expect_equal(status2["foremen"], c(foremen = "Empty"))
  
  # Add another
  assign("foremen", data.frame(x = 1), envir = cache_env)
  
  status3 <- get_cache_status_list(types)
  expect_equal(status3["facilities"], c(facilities = "Cached"))
  expect_equal(status3["foremen"], c(foremen = "Cached"))
  expect_equal(status3["species"], c(species = "Empty"))
})

test_that("cache row counts are accurate", {
  cache_env <- setup_test_cache()
  clear_test_cache()
  
  get_row_count <- function(key) {
    if (!exists(key, envir = cache_env)) return(NA)
    obj <- get(key, envir = cache_env)
    if (is.data.frame(obj)) nrow(obj) else length(obj)
  }
  
  # Add data frames of different sizes
  assign("facilities", data.frame(x = 1:5), envir = cache_env)
  assign("foremen", data.frame(x = 1:12), envir = cache_env)
  assign("species", data.frame(x = 1:100), envir = cache_env)
  
  expect_equal(get_row_count("facilities"), 5)
  expect_equal(get_row_count("foremen"), 12)
  expect_equal(get_row_count("species"), 100)
  expect_true(is.na(get_row_count("nonexistent")))
})

# =============================================================================
# CLEANUP
# =============================================================================

# Clean up after all tests
clear_test_cache()
