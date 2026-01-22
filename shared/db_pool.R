# =============================================================================
# MMCD METRICS - DATABASE CONNECTION POOL MANAGER
# =============================================================================
# This file manages a persistent connection pool for all MMCD applications
# Uses the {pool} package to maintain a pool of reusable database connections
# 
# Benefits:
# - Eliminates connection overhead (no create/destroy on every query)
# - Prevents connection exhaustion under load
# - Automatic connection health checking and reconnection
# - Thread-safe for parallel operations
# 
# Usage in apps:
#   source("../../shared/db_pool.R")
#   conn <- get_pool()
#   data <- dbGetQuery(conn, "SELECT ...")  # No disconnect needed!
# =============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(pool)
  library(DBI)
  library(RPostgres)
})

# Global pool object (initialized once per R session)
.mmcd_connection_pool <- NULL

# Flag to track if we've already tried to load environment variables
.env_loaded <- FALSE

# Application name for database connection tracking (visible in AWS RDS monitoring)
.app_name <- "mmcd_metrics"

#' Load environment variables for database connection
#' 
#' Tries multiple paths to find .env file, or uses system environment variables
#' Only runs once per session to avoid repeated file I/O
#' 
#' @return TRUE if environment variables are available, FALSE otherwise
load_db_env_vars <- function() {
  if (.env_loaded) {
    return(TRUE)
  }
  
  # Possible paths to .env file
  env_paths <- c(
    "../../.env",              # From apps/*/
    "../../../.env",           # Alternative local path
    "/srv/shiny-server/.env",  # Docker path
    ".env",                    # Root directory
    "../.env"                  # One level up
  )
  
  # Try to load from .env file
  for (path in env_paths) {
    if (file.exists(path)) {
      readRenviron(path)
      .env_loaded <<- TRUE
      return(TRUE)
    }
  }
  
  # Check if environment variables are already set (Docker/system)
  required_vars <- c("DB_HOST", "DB_PORT", "DB_NAME", 
                     "DB_USER", "DB_PASSWORD")
  
  if (all(sapply(required_vars, function(var) Sys.getenv(var) != ""))) {
    .env_loaded <<- TRUE
    return(TRUE)
  }
  
  warning("Could not load database environment variables from .env file or system")
  return(FALSE)
}

#' Set the application name for database connection tracking
#' 
#' Sets the application name that will be visible in AWS RDS Performance Insights
#' and PostgreSQL's pg_stat_activity view. Call this BEFORE calling get_pool().
#' 
#' @param name The application name (e.g., "struct_trt", "drone", "catch_basin_status")
#' @export
#' 
#' @examples
#' # In your app.R, call this before any database queries:
#' set_app_name("struct_trt")
#' conn <- get_pool()
set_app_name <- function(name) {
  .app_name <<- paste0("mmcd_", name)
  message(sprintf(" Application name set to: %s", .app_name))
}

#' Get the current application name
#' 
#' @return The current application name string
#' @export
get_app_name <- function() {
  return(.app_name)
}

#' Initialize or retrieve the database connection pool
#' 
#' Creates a connection pool on first call, returns existing pool on subsequent calls.
#' The pool maintains 1-15 connections that are reused across queries.
#' 
#' Pool configuration:
#' - minSize: 1 (at least 1 connection always ready)
#' - maxSize: 15 (up to 15 concurrent connections)
#' - idleTimeout: 3600 seconds (1 hour - close idle connections)
#' - validationInterval: 60 seconds (check connection health every minute)
#' 
#' @param force_reconnect If TRUE, closes existing pool and creates new one (default: FALSE)
#' @return A pool object that can be used like a regular DBI connection
#' @export
#' 
#' @examples
#' # Get the pool
#' conn <- get_pool()
#' 
#' # Use it like a regular connection (no disconnect needed!)
#' data <- dbGetQuery(conn, "SELECT * FROM my_table LIMIT 10")
#' 
#' # Pool automatically manages connections
get_pool <- function(force_reconnect = FALSE) {
  # If pool exists and we're not forcing reconnection, return it
  if (!is.null(.mmcd_connection_pool) && !force_reconnect) {
    # Verify pool is still valid
    tryCatch({
      pool::dbGetInfo(.mmcd_connection_pool)
      return(.mmcd_connection_pool)
    }, error = function(e) {
      # Pool is invalid, will recreate below
      message("Connection pool invalid, recreating...")
    })
  }
  
  # Close existing pool if forcing reconnection
  if (!is.null(.mmcd_connection_pool) && force_reconnect) {
    close_pool()
  }
  
  # Load environment variables
  if (!load_db_env_vars()) {
    stop("Cannot create connection pool: Database environment variables not available")
  }
  
  # Validate required environment variables
  required_vars <- c("DB_HOST", "DB_PORT", "DB_NAME", 
                     "DB_USER", "DB_PASSWORD")
  missing_vars <- required_vars[sapply(required_vars, function(v) Sys.getenv(v) == "")]
  
  if (length(missing_vars) > 0) {
    stop(sprintf("Missing required environment variables: %s", 
                paste(missing_vars, collapse = ", ")))
  }
  
  # Create the connection pool
  tryCatch({
    .mmcd_connection_pool <<- pool::dbPool(
      drv = RPostgres::Postgres(),
      host = Sys.getenv("DB_HOST"),
      port = as.integer(Sys.getenv("DB_PORT")),
      dbname = Sys.getenv("DB_NAME"),
      user = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASSWORD"),
      # Application name for AWS RDS monitoring
      application_name = .app_name,
      # Pool configuration
      minSize = 1,               # Keep at least 1 connection ready
      maxSize = 15,              # Allow up to 15 concurrent connections
      idleTimeout = 3600,        # Close idle connections after 1 hour
      validationInterval = 60    # Check connection health every minute
    )
    
    message("✓ Database connection pool initialized successfully")
    message(sprintf("  Host: %s:%s", Sys.getenv("DB_HOST"), Sys.getenv("DB_PORT")))
    message(sprintf("  Database: %s", Sys.getenv("DB_NAME")))
    message(sprintf("  App Name: %s", .app_name))
    message("  Pool size: 1-15 connections")
    
    return(.mmcd_connection_pool)
    
  }, error = function(e) {
    stop(sprintf("Failed to create connection pool: %s", e$message))
  })
}

#' Close the connection pool
#' 
#' Gracefully closes all connections in the pool.
#' Should only be called when shutting down the application.
#' The pool will be automatically recreated on next get_pool() call.
#' 
#' @export
close_pool <- function() {
  if (!is.null(.mmcd_connection_pool)) {
    tryCatch({
      pool::poolClose(.mmcd_connection_pool)
      .mmcd_connection_pool <<- NULL
      message("✓ Connection pool closed")
    }, error = function(e) {
      warning(sprintf("Error closing connection pool: %s", e$message))
      .mmcd_connection_pool <<- NULL
    })
  }
}

#' Get pool statistics for monitoring
#' 
#' Returns information about current pool state, useful for debugging
#' and monitoring connection usage.
#' 
#' @return List with pool statistics or NULL if pool not initialized
#' @export
get_pool_stats <- function() {
  if (is.null(.mmcd_connection_pool)) {
    return(list(
      initialized = FALSE,
      message = "Connection pool not initialized"
    ))
  }
  
  tryCatch({
    # Get pool information
    info <- pool::dbGetInfo(.mmcd_connection_pool)
    
    # Return formatted statistics
    list(
      initialized = TRUE,
      host = Sys.getenv("POSTGRES_HOST"),
      database = Sys.getenv("POSTGRES_DB"),
      min_size = 1,
      max_size = 15,
      info = info
    )
  }, error = function(e) {
    list(
      initialized = TRUE,
      error = e$message
    )
  })
}

#' Register cleanup on session end (for Shiny apps)
#' 
#' Call this in your Shiny server function to ensure proper cleanup
#' when the user's session ends. This prevents connection leaks.
#' 
#' @param session Shiny session object
#' @export
#' 
#' @examples
#' # In Shiny server function:
#' server <- function(input, output, session) {
#'   register_pool_cleanup(session)
#'   # ... rest of server code
#' }
register_pool_cleanup <- function(session) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    warning("shiny package not available, cannot register cleanup")
    return()
  }
  
  shiny::onSessionEnded(function() {
    # Note: We don't close the pool here because other sessions might be using it
    # The pool will handle its own connection lifecycle
    message("Session ended - pool remains active for other sessions")
  })
}

# =============================================================================
# BACKWARD COMPATIBILITY WRAPPER
# =============================================================================
# For apps that currently use get_db_connection() + dbDisconnect()
# This allows gradual migration to the pool

#' Get a pooled connection (backward compatibility wrapper)
#' 
#' This function provides backward compatibility for apps using the old pattern:
#'   con <- get_db_connection()
#'   data <- dbGetQuery(con, query)
#'   dbDisconnect(con)  # <-- This becomes a no-op with pools
#' 
#' @return A pool object that behaves like a connection
#' @export
get_pooled_connection <- function() {
  return(get_pool())
}

# Message on load
message(" db_pool.R loaded - Connection pooling available")
message("   Use get_pool() to access the connection pool")
message("   No disconnect needed - connections are automatically managed!")
