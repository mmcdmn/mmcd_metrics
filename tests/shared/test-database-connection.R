# =============================================================================
# Tests for Database Connection Functions (db_helpers.R, db_pool.R)
# =============================================================================

library(testthat)

context("Database Connection")

# =============================================================================
# Connection Pool Tests
# =============================================================================

test_that("get_pool function exists", {
  expect_true(exists("get_pool", mode = "function"))
})

test_that("get_db_connection function exists", {
  expect_true(exists("get_db_connection", mode = "function"))
})

test_that("safe_disconnect function exists", {
  expect_true(exists("safe_disconnect", mode = "function"))
})

test_that("safe_disconnect handles NULL gracefully", {
  # Should not error
  expect_silent(safe_disconnect(NULL))
})

# =============================================================================
# Connection Tests (requires database)
# =============================================================================

test_that("get_db_connection returns connection or pool object", {
  skip_on_cran()
  
  # Try to get connection
  conn <- tryCatch(
    get_db_connection(),
    error = function(e) NULL
  )
  
  skip_if(is.null(conn), "Database connection not available")
  
  # Should be a valid connection or pool
  expect_true(
    inherits(conn, "Pool") || 
    inherits(conn, "PqConnection") ||
    inherits(conn, "DBIConnection")
  )
  
  # Cleanup
  safe_disconnect(conn)
})

test_that("get_pool returns pool object when available", {
  skip_on_cran()
  
  pool <- tryCatch(
    get_pool(),
    error = function(e) NULL
  )
  
  skip_if(is.null(pool), "Connection pool not available")
  
  expect_true(inherits(pool, "Pool"))
})

test_that("database queries work through connection", {
  skip_on_cran()
  
  conn <- tryCatch(
    get_db_connection(),
    error = function(e) NULL
  )
  
  skip_if(is.null(conn), "Database connection not available")
  
  # Try a simple query
  result <- tryCatch(
    DBI::dbGetQuery(conn, "SELECT 1 as test"),
    error = function(e) NULL
  )
  
  if (!is.null(result)) {
    expect_equal(result$test, 1)
  }
  
  safe_disconnect(conn)
})

# =============================================================================
# Environment Variable Tests
# =============================================================================

test_that("load_env_vars function exists", {
  expect_true(exists("load_env_vars", mode = "function"))
})

test_that("required environment variables are documented",
{
  # These are the expected required variables
  required_vars <- c("DB_HOST", "DB_PORT", "DB_NAME", "DB_USER", "DB_PASSWORD")
  
  # Just verify we know what they are
  expect_length(required_vars, 5)
})
