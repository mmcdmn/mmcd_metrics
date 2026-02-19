# =============================================================================
# MMCD METRICS - APP-LEVEL CACHING MODULE
# =============================================================================
# This module provides intelligent caching for Shiny applications to reduce
# database load and improve response times under concurrent user load.
#
# KEY FEATURES:
# 1. Global data cache - Share data across all sessions of an app
# 2. Reactive caching - bindCache() wrappers for Shiny reactives  
# 3. Auto-refresh - Background refresh of cached data
# 4. Memory management - LRU eviction and size limits
#
# USAGE:
#   source("../../shared/app_cache.R")
#   
#   # In server function:
#   cached_data <- create_cached_data_loader(
#     load_func = function() dbGetQuery(conn, "SELECT ..."),
#     cache_key = "my_data",
#     ttl_minutes = 5
#   )
# =============================================================================

library(digest)

# =============================================================================
# SOURCE REDIS CACHE LAYER
# =============================================================================
# Provides redis_is_active(), get_app_cached_redis(), set_app_cached_redis()
.redis_app_paths <- c(
  "/srv/shiny-server/shared/redis_cache.R",
  "redis_cache.R",
  "../../shared/redis_cache.R",
  "../shared/redis_cache.R",
  "../../../shared/redis_cache.R",
  "shared/redis_cache.R"
)
for (.rp in .redis_app_paths) {
  if (file.exists(.rp) && !exists("redis_is_active", mode = "function")) {
    tryCatch(source(.rp, local = FALSE), error = function(e) NULL)
    break
  }
}
if (exists(".redis_app_paths")) rm(.redis_app_paths)
if (exists(".rp")) rm(.rp)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Cache configuration (can be overridden per-app)
CACHE_CONFIG <- list(
  # Enable/disable caching globally
  enabled = TRUE,
  
  # Default cache time-to-live in minutes
  default_ttl_minutes = 5,
  
  # Maximum cache entries per app (LRU eviction)
  max_entries = 100,
  
  # Maximum memory per cache in MB (approximate)
  max_memory_mb = 100,
  
  # Enable debug logging
  debug = FALSE,
  
  # Background refresh threshold (refresh when X% of TTL remaining)
  refresh_threshold = 0.2
)

# =============================================================================
# GLOBAL CACHE STORAGE
# =============================================================================

# Global cache environment (survives across sessions within same R process)
if (!exists(".mmcd_app_cache", envir = globalenv())) {
  assign(".mmcd_app_cache", new.env(parent = emptyenv()), envir = globalenv())
}

#' Get the global cache environment
get_cache_env <- function() {
  get(".mmcd_app_cache", envir = globalenv())
}

#' Clear all cached data
clear_app_cache <- function() {
  cache_env <- get_cache_env()
  rm(list = ls(cache_env), envir = cache_env)
  if (CACHE_CONFIG$debug) message("[cache] App cache cleared")
}

#' Get cache statistics
get_cache_stats <- function() {
  cache_env <- get_cache_env()
  keys <- ls(cache_env)
  
  if (length(keys) == 0) {
    return(list(
      entries = 0,
      total_size_mb = 0,
      keys = character(0)
    ))
  }
  
  # Calculate approximate size
  total_size <- sum(sapply(keys, function(k) {
    obj <- get(k, envir = cache_env)
    if (is.list(obj) && "data" %in% names(obj)) {
      object.size(obj$data)
    } else {
      object.size(obj)
    }
  }))
  
  list(
    entries = length(keys),
    total_size_mb = round(total_size / 1024 / 1024, 2),
    keys = keys
  )
}

# =============================================================================
# CACHE ENTRY MANAGEMENT
# =============================================================================

#' Create a cache entry with metadata
create_cache_entry <- function(data, ttl_minutes, key) {
  list(
    data = data,
    created_at = Sys.time(),
    expires_at = Sys.time() + (ttl_minutes * 60),
    ttl_minutes = ttl_minutes,
    key = key,
    access_count = 0,
    last_accessed = Sys.time()
  )
}

#' Check if cache entry is valid (not expired)
is_cache_valid <- function(entry) {
  if (is.null(entry)) return(FALSE)
  if (!is.list(entry)) return(FALSE)
  if (!"expires_at" %in% names(entry)) return(FALSE)
  
  Sys.time() < entry$expires_at
}

#' Check if cache entry should be refreshed (approaching expiration)
should_refresh <- function(entry) {
  if (!is_cache_valid(entry)) return(TRUE)
  
  remaining <- as.numeric(difftime(entry$expires_at, Sys.time(), units = "secs"))
  total <- entry$ttl_minutes * 60
  
  (remaining / total) < CACHE_CONFIG$refresh_threshold
}

#' Get cached data with optional refresh
#' Checks: in-memory → Redis → load_func
get_cached <- function(key, load_func = NULL, ttl_minutes = CACHE_CONFIG$default_ttl_minutes) {
  cache_env <- get_cache_env()
  
  # 1. Check in-memory cache (fastest, same R process)
  if (exists(key, envir = cache_env)) {
    entry <- get(key, envir = cache_env)
    
    if (is_cache_valid(entry)) {
      # Update access stats
      entry$access_count <- entry$access_count + 1
      entry$last_accessed <- Sys.time()
      assign(key, entry, envir = cache_env)
      
      if (CACHE_CONFIG$debug) {
        message(sprintf("[cache] HIT (memory): %s (accessed %d times)", key, entry$access_count))
      }
      
      return(entry$data)
    }
  }
  
  # 2. Check Redis (shared across containers)
  if (exists("redis_is_active", mode = "function") && redis_is_active()) {
    redis_data <- get_app_cached_redis(key)
    if (!is.null(redis_data)) {
      # Populate in-memory for subsequent calls in this process
      set_cached(key, redis_data, ttl_minutes, redis_write = FALSE)
      if (CACHE_CONFIG$debug) {
        message(sprintf("[cache] HIT (redis): %s", key))
      }
      return(redis_data)
    }
  }
  
  # 3. Cache miss - need to load data
  if (is.null(load_func)) {
    if (CACHE_CONFIG$debug) message(sprintf("[cache] MISS: %s (no load function)", key))
    return(NULL)
  }
  
  if (CACHE_CONFIG$debug) message(sprintf("[cache] MISS: %s (loading...)", key))
  
  # Load fresh data
  data <- tryCatch({
    load_func()
  }, error = function(e) {
    warning(sprintf("Cache load error for %s: %s", key, e$message))
    NULL
  })
  
  if (!is.null(data)) {
    set_cached(key, data, ttl_minutes)
  }
  
  data
}

#' Set cached data
#' Writes to in-memory + Redis (if active)
#' @param redis_write If FALSE, skip Redis write (used when populating from Redis hit)
set_cached <- function(key, data, ttl_minutes = CACHE_CONFIG$default_ttl_minutes, redis_write = TRUE) {
  if (!CACHE_CONFIG$enabled) return(invisible(NULL))
  
  cache_env <- get_cache_env()
  
  # Create entry for in-memory cache
  entry <- create_cache_entry(data, ttl_minutes, key)
  assign(key, entry, envir = cache_env)
  
  # Write to Redis for cross-container sharing
  if (redis_write && exists("redis_is_active", mode = "function") && redis_is_active()) {
    tryCatch({
      set_app_cached_redis(key, data, ttl = ttl_minutes * 60)
      if (CACHE_CONFIG$debug) {
        message(sprintf("[cache] SET (memory+redis): %s (TTL: %d min)", key, ttl_minutes))
      }
    }, error = function(e) {
      if (CACHE_CONFIG$debug) {
        message(sprintf("[cache] SET (memory only, redis error): %s - %s", key, e$message))
      }
    })
  } else if (CACHE_CONFIG$debug) {
    message(sprintf("[cache] SET (memory): %s (TTL: %d min)", key, ttl_minutes))
  }
  
  # Check memory limits and evict if needed
  enforce_cache_limits()
  
  invisible(NULL)
}

#' Remove specific cache entry from memory and Redis
remove_cached <- function(key) {
  cache_env <- get_cache_env()
  if (exists(key, envir = cache_env)) {
    rm(list = key, envir = cache_env)
  }
  
  # Also remove from Redis
  if (exists("redis_is_active", mode = "function") && redis_is_active()) {
    tryCatch({
      redis_del(paste0("app:", key))
    }, error = function(e) {
      if (CACHE_CONFIG$debug) message(sprintf("[cache] Redis remove error: %s", e$message))
    })
  }
  
  if (CACHE_CONFIG$debug) message(sprintf("[cache] REMOVE: %s", key))
}

#' Enforce cache size limits using LRU eviction
enforce_cache_limits <- function() {
  cache_env <- get_cache_env()
  keys <- ls(cache_env)
  
  if (length(keys) <= CACHE_CONFIG$max_entries) {
    return(invisible(NULL))
  }
  
  # Get access times for LRU
  access_times <- sapply(keys, function(k) {
    entry <- get(k, envir = cache_env)
    if (is.list(entry) && "last_accessed" %in% names(entry)) {
      as.numeric(entry$last_accessed)
    } else {
      0
    }
  })
  
  # Remove oldest entries
  to_remove <- names(sort(access_times))[1:(length(keys) - CACHE_CONFIG$max_entries + 1)]
  for (k in to_remove) {
    rm(list = k, envir = cache_env)
  }
  
  if (CACHE_CONFIG$debug) {
    message(sprintf("[cache] Evicted %d entries", length(to_remove)))
  }
}

# =============================================================================
# SHINY-SPECIFIC CACHING HELPERS
# =============================================================================

#' Create a cached data loader reactive
#' 
#' Returns a reactive that loads data with caching. Multiple sessions
#' will share the same cached data, reducing database load.
#'
#' @param load_func Function that loads the data (called when cache miss)
#' @param cache_key Unique key for this cache entry
#' @param ttl_minutes How long to cache the data (default 5 minutes)
#' @param depends_on Optional reactive to invalidate cache when changed
#' @return A reactive expression that returns the cached data
#'
#' @examples
#' # In server function:
#' my_data <- create_cached_data_loader(
#'   load_func = function() load_raw_data(analysis_date = Sys.Date()),
#'   cache_key = "drone_data_today",
#'   ttl_minutes = 5
#' )
create_cached_data_loader <- function(load_func, cache_key, 
                                       ttl_minutes = CACHE_CONFIG$default_ttl_minutes,
                                       depends_on = NULL) {
  # Return a reactive that uses the cache
  shiny::reactive({
    # If there's a dependency, include it in the cache key
    full_key <- if (!is.null(depends_on)) {
      dep_value <- if (is.reactive(depends_on)) depends_on() else depends_on
      paste0(cache_key, "_", digest::digest(dep_value, algo = "xxhash32"))
    } else {
      cache_key
    }
    
    get_cached(full_key, load_func, ttl_minutes)
  })
}

#' Create a filtered data reactive with caching
#' 
#' Caches data for specific filter combinations. Each unique filter
#' combination gets its own cache entry.
#'
#' @param load_func Function that loads data, receives filter values as args
#' @param cache_prefix Prefix for cache keys
#' @param filter_inputs List of reactive inputs to use as cache key parts
#' @param ttl_minutes Cache TTL
#' @return Reactive expression
create_cached_filtered_loader <- function(load_func, cache_prefix,
                                           filter_inputs,
                                           ttl_minutes = 3) {
  shiny::reactive({
    # Build cache key from filter values
    filter_values <- lapply(filter_inputs, function(inp) {
      if (is.reactive(inp)) inp() else inp
    })
    
    key_hash <- digest::digest(filter_values, algo = "xxhash32")
    cache_key <- paste0(cache_prefix, "_", key_hash)
    
    get_cached(cache_key, function() do.call(load_func, filter_values), ttl_minutes)
  })
}

#' Generate cache key from input values
#' 
#' Creates a consistent cache key from a list of input values.
#' Useful for caching filtered query results.
#'
#' @param prefix Prefix string for the key
#' @param ... Values to include in the key
#' @return Character string cache key
make_cache_key <- function(prefix, ...) {
  values <- list(...)
  hash <- digest::digest(values, algo = "xxhash32")
  paste0(prefix, "_", hash)
}

# =============================================================================
# GLOBAL DATA SHARING (ACROSS SESSIONS)
# =============================================================================

#' Create a shared data store for an app
#' 
#' Creates a global data store that is shared across all sessions of an app.
#' This is ideal for reference data that doesn't change often.
#'
#' @param store_name Name of the shared store
#' @return List with get/set/has functions
create_shared_store <- function(store_name) {
  store_key <- paste0("_store_", store_name)
  cache_env <- get_cache_env()
  
  # Initialize store if needed
  if (!exists(store_key, envir = cache_env)) {
    assign(store_key, new.env(parent = emptyenv()), envir = cache_env)
  }
  
  store_env <- get(store_key, envir = cache_env)
  
  list(
    get = function(key, default = NULL) {
      if (exists(key, envir = store_env)) {
        get(key, envir = store_env)
      } else {
        default
      }
    },
    
    set = function(key, value) {
      assign(key, value, envir = store_env)
      invisible(value)
    },
    
    has = function(key) {
      exists(key, envir = store_env)
    },
    
    keys = function() {
      ls(store_env)
    },
    
    clear = function() {
      rm(list = ls(store_env), envir = store_env)
    }
  )
}

# =============================================================================
# LOOKUP TABLE CACHING (OPTIMIZED)
# =============================================================================

#' Get cached lookup table with auto-refresh
#' 
#' Wrapper specifically for lookup tables that change infrequently.
#' Uses longer TTL and supports background refresh.
#'
#' @param lookup_name Name of the lookup (e.g., "facilities", "foremen")
#' @param load_func Function to load the lookup from database
#' @param ttl_hours How long to cache (default 1 hour)
#' @return The lookup data
get_cached_lookup_v2 <- function(lookup_name, load_func, ttl_hours = 1) {
  cache_key <- paste0("lookup_", lookup_name)
  get_cached(cache_key, load_func, ttl_minutes = ttl_hours * 60)
}

# =============================================================================
# PRELOAD FUNCTIONS
# =============================================================================

#' Preload common data on app startup
#' 
#' Call this at the start of app.R (outside server function) to
#' preload frequently-used data into the cache. This ensures the
#' first user doesn't experience a slow load.
#'
#' @param lookups List of lookup names to preload
preload_lookups <- function(lookups = c("facilities", "foremen", "species")) {
  message("[cache] Preloading lookup tables...")
  
  # These functions are defined in db_helpers.R
  preload_funcs <- list(
    facilities = function() {
      if (exists("get_facility_lookup")) get_facility_lookup() else NULL
    },
    foremen = function() {
      if (exists("get_foremen_lookup")) get_foremen_lookup() else NULL
    },
    species = function() {
      if (exists("get_species_lookup")) get_species_lookup() else NULL
    }
  )
  
  for (name in lookups) {
    if (name %in% names(preload_funcs)) {
      tryCatch({
        data <- preload_funcs[[name]]()
        if (!is.null(data)) {
          set_cached(paste0("lookup_", name), data, ttl_minutes = 60)
          message(sprintf("  - %s preloaded", name))
        }
      }, error = function(e) {
        message(sprintf("  - %s failed: %s", name, e$message))
      })
    }
  }
  
  message("[cache] Preload complete")
}

# =============================================================================
# CACHE WARMING (for scheduled refresh)
# =============================================================================

#' Warm the cache for a specific app
#' 
#' Pre-populates cache with common query results. Can be called
#' from a scheduled task to keep cache fresh.
#'
#' @param app_name Name of the app
#' @param warm_func Function that generates the data to cache
warm_cache <- function(app_name, warm_func) {
  cache_key <- paste0("warm_", app_name, "_", Sys.Date())
  
  message(sprintf("[cache] Warming cache for %s...", app_name))
  
  tryCatch({
    data <- warm_func()
    set_cached(cache_key, data, ttl_minutes = 60)
    message(sprintf("[cache] Cache warmed for %s", app_name))
  }, error = function(e) {
    warning(sprintf("Cache warming failed for %s: %s", app_name, e$message))
  })
}

# =============================================================================
# DEBUG AND MONITORING
# =============================================================================

#' Print cache debug info
print_cache_info <- function() {
  stats <- get_cache_stats()
  cache_env <- get_cache_env()
  
  # Determine Redis status
  redis_status <- "not configured"
  if (exists("redis_is_active", mode = "function")) {
    redis_status <- if (redis_is_active()) "connected" else "disconnected"
  }
  
  cat("\n╔═══════════════════════════════════════╗\n")
  cat("║       MMCD App Cache Status           ║\n")
  cat("╠═══════════════════════════════════════╣\n")
  cat(sprintf("║ In-Memory Entries: %-18d ║\n", stats$entries))
  cat(sprintf("║ In-Memory Size:    %-16.2f MB ║\n", stats$total_size_mb))
  cat(sprintf("║ Redis Backend:     %-18s ║\n", redis_status))
  cat("╠═══════════════════════════════════════╣\n")
  
  if (stats$entries > 0) {
    cat("║ In-Memory Entries:                    ║\n")
    for (key in stats$keys) {
      entry <- get(key, envir = cache_env)
      if (is.list(entry) && "expires_at" %in% names(entry)) {
        remaining <- difftime(entry$expires_at, Sys.time(), units = "mins")
        status <- if (remaining > 0) sprintf("%.1fm", remaining) else "EXPIRED"
        cat(sprintf("║   %-32s %s ║\n", 
                    substr(key, 1, 32),
                    status))
      }
    }
  }
  
  # Show Redis cache info if available
  if (exists("redis_is_active", mode = "function") && redis_is_active()) {
    tryCatch({
      redis_info <- redis_cache_status()
      if (!is.null(redis_info)) {
        cat("╠═══════════════════════════════════════╣\n")
        cat(sprintf("║ Redis Historical Avg: %-15s ║\n",
                    if (redis_info$historical_fields > 0) paste0(redis_info$historical_fields, " metrics") else "empty"))
        cat(sprintf("║ Redis Lookups:        %-15s ║\n",
                    if (redis_info$lookup_fields > 0) paste0(redis_info$lookup_fields, " tables") else "empty"))
      }
    }, error = function(e) NULL)
  }
  
  cat("╚═══════════════════════════════════════╝\n")
}

# Log on load
message("[app_cache] Module loaded (shared/app_cache.R)")
