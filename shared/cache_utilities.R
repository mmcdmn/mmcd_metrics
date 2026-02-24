# Cache Utilities for Historical Data
# =============================================================================
# Shared functions for managing the cache system across metrics.
# FULLY DYNAMIC - reads from metric_registry.R to determine cacheable metrics.
#
# BACKEND: Uses embedded Redis as the single source of truth.
#          Redis is sourced from shared/redis_cache.R
#          NO .rds file fallback — all data lives in Redis.
# =============================================================================

# =============================================================================
# REDIS INTEGRATION — source the Redis layer
# =============================================================================

# Source redis_cache.R (provides redis_is_active, redis_hget, etc.)
.redis_cache_paths <- c(
  "/srv/shiny-server/shared/redis_cache.R",
  "../../../shared/redis_cache.R",
  "../../shared/redis_cache.R",
  "../shared/redis_cache.R",
  "./shared/redis_cache.R"
)
for (.p in .redis_cache_paths) {
  if (file.exists(.p)) {
    source(.p, local = FALSE)
    break
  }
}
rm(.redis_cache_paths, .p)

# =============================================================================
# CACHE CONFIGURATION
# =============================================================================

CACHE_ENABLED <- TRUE

#' Get cache directory path (works from any location)
#' @return Path to cache directory
get_cache_dir <- function() {
  paths <- c(
    "/srv/shiny-server/shared/cache",
    "../../../shared/cache",
    "../../shared/cache",
    "../shared/cache", 
    "./shared/cache"
  )
  for (p in paths) {
    if (dir.exists(p)) return(normalizePath(p))
  }
  # Fallback - create if needed
  if (dir.exists("/srv/shiny-server/shared")) {
    dir.create("/srv/shiny-server/shared/cache", showWarnings = FALSE)
    return("/srv/shiny-server/shared/cache")
  }
  "."
}

#' Get cache file path (legacy compat stub — no longer used for reads/writes)
#' @return Path to legacy cache RDS file (for cleanup only)
get_cache_file <- function() {
  file.path(get_cache_dir(), "historical_averages_cache.rds")
}

# =============================================================================
# DYNAMIC METRIC DISCOVERY
# =============================================================================

#' Source metric_registry.R dynamically
#' @return TRUE if sourced successfully
ensure_registry_loaded <- function() {
  if (exists("get_metric_registry", mode = "function")) return(TRUE)
  
  registry_paths <- c(
    "/srv/shiny-server/apps/overview/metric_registry.R",
    "../overview/metric_registry.R",
    "../../apps/overview/metric_registry.R",
    "./apps/overview/metric_registry.R"
  )
  
  for (p in registry_paths) {
    if (file.exists(p)) {
      source(p, local = FALSE)
      return(TRUE)
    }
  }
  FALSE
}

#' Get list of metrics that CAN be cached (from registry)
#' Metrics with historical_enabled = TRUE and use_active_calculation = TRUE are cacheable
#' @return Character vector of metric IDs
get_cacheable_metrics <- function() {
  if (!ensure_registry_loaded()) {
    # Fallback to hardcoded list if registry unavailable
    return(c("catch_basin", "drone", "ground_prehatch", "structure", "cattail_treatments", "air_sites"))
  }
  
  registry <- get_metric_registry()
  
  # Filter to metrics that support caching (have historical charts)
  cacheable <- sapply(names(registry), function(id) {
    config <- registry[[id]]
    isTRUE(config$historical_enabled)
  })
  
  names(cacheable)[cacheable]
}

#' Get metrics that ARE currently cached
#' @return Character vector of metric IDs that have cache entries
get_cached_metrics <- function() {
  # Redis is the single source
  if (exists("redis_is_active", mode = "function") && redis_is_active()) {
    keys <- tryCatch(redis_hkeys(HISTORICAL_HASH_KEY), error = function(e) character(0))
    if (length(keys) > 0) {
      return(unique(gsub("_(5yr|10yr|yearly_facilities|yearly_district)$", "", keys)))
    }
  }

  character(0)
}

#' Check if a metric supports caching
#' @param metric The metric name to check
#' @return TRUE if metric can be cached
is_metric_cacheable <- function(metric) {
  metric %in% get_cacheable_metrics()
}

#' Check if a metric IS currently cached
#' @param metric The metric name to check  
#' @return TRUE if metric has cache data
is_metric_cached <- function(metric) {
  metric %in% get_cached_metrics()
}

# =============================================================================
# CACHE MANAGEMENT FUNCTIONS
# =============================================================================

#' Get comprehensive cache status for all metrics
#' @return Data frame with metric status information
get_cache_status <- function() {
  all_metrics <- get_cacheable_metrics()
  cached_metrics <- get_cached_metrics()
  
  # Load cache for dates/stats from Redis (single source)
  cache_info <- if (exists("redis_is_active", mode = "function") && redis_is_active()) {
    meta <- tryCatch(redis_get(HISTORICAL_META_KEY), error = function(e) NULL)
    avgs <- tryCatch(redis_hgetall(HISTORICAL_HASH_KEY), error = function(e) list())
    list(
      generated_date = if (!is.null(meta)) meta$generated_date else NA,
      averages = avgs,
      source = "redis"
    )
  } else {
    list(generated_date = NA, averages = list(), source = "redis-unavailable")
  }
  
  # Build status for each metric
  registry <- if (ensure_registry_loaded()) get_metric_registry() else list()
  
  do.call(rbind, lapply(all_metrics, function(metric_id) {
    config <- registry[[metric_id]]
    is_yearly <- isTRUE(config$historical_type == "yearly_grouped")
    
    if (is_yearly) {
      key_fac <- paste0(metric_id, "_yearly_facilities")
      key_dist <- paste0(metric_id, "_yearly_district")
      
      has_fac <- key_fac %in% names(cache_info$averages)
      has_dist <- key_dist %in% names(cache_info$averages)
      
      rows_fac <- if (has_fac) nrow(cache_info$averages[[key_fac]]) else 0
      rows_dist <- if (has_dist) nrow(cache_info$averages[[key_dist]]) else 0
      
      data.frame(
        metric_id = metric_id,
        cacheable = TRUE,
        has_5yr = has_fac,
        has_10yr = has_dist,
        rows_5yr = rows_fac,
        rows_10yr = rows_dist,
        status = if (has_fac && has_dist) "Complete" else if (has_fac || has_dist) "Partial" else "Missing",
        last_updated = as.character(cache_info$generated_date),
        stringsAsFactors = FALSE
      )
    } else {
      key_5yr <- paste0(metric_id, "_5yr")
      key_10yr <- paste0(metric_id, "_10yr")
      
      has_5yr <- key_5yr %in% names(cache_info$averages)
      has_10yr <- key_10yr %in% names(cache_info$averages)
      
      rows_5yr <- if (has_5yr) nrow(cache_info$averages[[key_5yr]]) else 0
      rows_10yr <- if (has_10yr) nrow(cache_info$averages[[key_10yr]]) else 0
      
      data.frame(
        metric_id = metric_id,
        cacheable = TRUE,
        has_5yr = has_5yr,
        has_10yr = has_10yr,
        rows_5yr = rows_5yr,
        rows_10yr = rows_10yr,
        status = if (has_5yr && has_10yr) "Complete" else if (has_5yr || has_10yr) "Partial" else "Missing",
        last_updated = as.character(cache_info$generated_date),
        stringsAsFactors = FALSE
      )
    }
  }))
}

#' Regenerate cache for specific metrics
#' @param metrics Vector of metric IDs to regenerate, or NULL for all
#' @param zone_filter Zone filter to use
#' @return Updated cache object
regenerate_cache <- function(metrics = NULL, zone_filter = c("1", "2")) {
  ensure_registry_loaded()
  
  # Source historical functions
  hist_paths <- c(
    "/srv/shiny-server/apps/overview/historical_functions.R",
    "../overview/historical_functions.R", 
    "../../apps/overview/historical_functions.R"
  )
  for (p in hist_paths) {
    if (file.exists(p)) {
      source(p, local = FALSE)
      break
    }
  }
  
  if (is.null(metrics)) {
    metrics <- get_cacheable_metrics()
  }
  
  cache_file <- get_cache_file()
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Load existing cache from Redis or create new
  existing_avgs <- list()
  if (exists("redis_is_active", mode = "function") && redis_is_active()) {
    existing_avgs <- tryCatch(redis_hgetall(HISTORICAL_HASH_KEY), error = function(e) list())
  }
  cache <- list(
    generated_date = Sys.Date(),
    generated_by = Sys.info()["user"],
    zone_filter = zone_filter,
    averages = if (length(existing_avgs) > 0) existing_avgs else list()
  )
  
  for (metric_id in metrics) {
    cat("Regenerating cache for:", metric_id, "...\n")
    
    tryCatch({
      # Temporarily disable cache to force DB load
      old_cache_setting <- if (exists("USE_CACHED_AVERAGES")) USE_CACHED_AVERAGES else FALSE
      assign("USE_CACHED_AVERAGES", FALSE, envir = .GlobalEnv)
      
      # Check metric type from registry
      registry <- get_metric_registry()
      config <- registry[[metric_id]]
      is_yearly_grouped <- isTRUE(config$historical_type == "yearly_grouped")
      
      if (is_yearly_grouped) {
        # Yearly grouped metrics: cache yearly_data for both overview types
        for (ov_type in c("facilities", "district")) {
          result <- load_historical_comparison_data(
            metric = metric_id,
            start_year = current_year - 9,
            end_year = current_year,
            zone_filter = zone_filter,
            overview_type = ov_type
          )
          
          if (!is.null(result$yearly_data) && nrow(result$yearly_data) > 0) {
            cache_key <- paste0(metric_id, "_yearly_", ov_type)
            cache$averages[[cache_key]] <- result$yearly_data
            cat("  ", ov_type, ":", nrow(result$yearly_data), "rows,",
                length(unique(result$yearly_data$year)), "years\n")
          }
        }
      } else {
        # Standard metrics: cache 5yr and 10yr averages
        result <- load_historical_comparison_data(
          metric = metric_id,
          start_year = current_year - 9,
          end_year = current_year,
          zone_filter = zone_filter
        )
        
        # Store 5-year and 10-year averages
        # Store zone-level data if available (for zone-specific retrieval)
        # Otherwise fall back to aggregated data
        avg_to_store <- if (!is.null(result$average_by_zone)) result$average_by_zone else result$average
        ten_to_store <- if (!is.null(result$ten_year_by_zone)) result$ten_year_by_zone else result$ten_year_average
        
        if (!is.null(avg_to_store) && nrow(avg_to_store) > 0) {
          cache$averages[[paste0(metric_id, "_5yr")]] <- avg_to_store
          zone_info <- if ("zone" %in% names(avg_to_store)) paste0(" (", nrow(avg_to_store), " zone-level rows)") else ""
          cat("  5yr:", nrow(avg_to_store), "rows, avg:", round(mean(avg_to_store$value, na.rm = TRUE), 1), zone_info, "\n")
        }
        
        if (!is.null(ten_to_store) && nrow(ten_to_store) > 0) {
          cache$averages[[paste0(metric_id, "_10yr")]] <- ten_to_store
          zone_info <- if ("zone" %in% names(ten_to_store)) paste0(" (", nrow(ten_to_store), " zone-level rows)") else ""
          cat("  10yr:", nrow(ten_to_store), "rows, avg:", round(mean(ten_to_store$value, na.rm = TRUE), 1), zone_info, "\n")
        }
      }
      
      # Restore cache setting
      assign("USE_CACHED_AVERAGES", old_cache_setting, envir = .GlobalEnv)
      
    }, error = function(e) {
      cat("  ERROR:", e$message, "\n")
    })
  }
  
  # Update metadata and save
  cache$generated_date <- Sys.Date()
  cache$zone_filter <- zone_filter
  
  # Save to Redis (primary / only backend)
  if (exists("redis_is_active", mode = "function") && redis_is_active()) {
    save_historical_cache_redis(cache)
    cat("Cache saved to Redis\n")
  } else {
    cat("WARNING: Redis not available — cache NOT persisted!\n")
  }
  
  invisible(cache)
}

#' Clear cache for specific metrics or all
#' @param metrics Vector of metric IDs to clear, or NULL for all
clear_cache <- function(metrics = NULL) {
  if (is.null(metrics)) {
    # Clear Redis
    if (exists("redis_is_active", mode = "function") && redis_is_active()) {
      clear_historical_cache_redis()
      cat("Redis historical cache cleared\n")
    }
    # Also remove legacy RDS file if it still exists
    cache_file <- get_cache_file()
    if (file.exists(cache_file)) {
      file.remove(cache_file)
      cat("Legacy RDS cache file removed\n")
    }
  } else {
    # Clear specific metrics from Redis
    if (exists("redis_is_active", mode = "function") && redis_is_active()) {
      for (metric_id in metrics) {
        fields <- c(
          paste0(metric_id, "_5yr"), paste0(metric_id, "_10yr"),
          paste0(metric_id, "_yearly_facilities"), paste0(metric_id, "_yearly_district")
        )
        redis_hdel(HISTORICAL_HASH_KEY, fields)
        cat("Cleared Redis cache for:", metric_id, "\n")
      }
    }
  }
  
  invisible()
}

# =============================================================================
# LOOKUP CACHE MANAGEMENT
# =============================================================================
# These functions provide access to the lookup cache (Redis or file-based)
# from db_helpers.R, enabling test-app to clear/refresh lookup data.

#' Get list of cached lookup types
#' @return Character vector of lookup names that can be cached
get_lookup_cache_types <- function() {
  c("facilities", "foremen", "species", "structure_types", "spring_thresholds")
}

#' Get status of lookup cache
#' @return Data frame with cache status for each lookup type
get_lookup_cache_status <- function() {
  # Load cache from Redis (single source)
  cache <- if (exists("redis_is_active", mode = "function") && redis_is_active()) {
    tryCatch(load_lookup_cache_redis(), error = function(e) list())
  } else {
    load_lookup_cache()
  }
  
  lookup_types <- get_lookup_cache_types()
  
  status_df <- data.frame(
    lookup_type = lookup_types,
    status = sapply(lookup_types, function(lt) {
      if (lt %in% names(cache)) "Cached" else "Empty"
    }),
    row_count = sapply(lookup_types, function(lt) {
      if (lt %in% names(cache)) {
        obj <- cache[[lt]]
        if (is.data.frame(obj)) nrow(obj) else length(obj)
      } else {
        NA_integer_
      }
    }),
    cached_at = sapply(lookup_types, function(lt) {
      ts_key <- paste0(lt, "_timestamp")
      if (ts_key %in% names(cache)) {
        as.character(cache[[ts_key]])
      } else {
        NA_character_
      }
    }),
    stringsAsFactors = FALSE
  )
  
  return(status_df)
}

#' Clear lookup cache (all or specific types) - Redis-backed
#' @param lookup_types Character vector of lookup types to clear, or NULL for all
#' @return Invisible NULL
clear_lookup_cache_types <- function(lookup_types = NULL) {
  use_redis <- exists("redis_is_active", mode = "function") && redis_is_active()

  if (is.null(lookup_types)) {
    # Clear all
    clear_lookup_cache()                        # memory + Redis (from db_helpers.R)
    cat("All lookup caches cleared\n")
  } else {
    # Clear specific types from memory
    for (lt in lookup_types) {
      if (exists(lt, envir = .lookup_memory_cache)) {
        rm(list = lt, envir = .lookup_memory_cache)
      }
      ts_key <- paste0(lt, "_timestamp")
      if (exists(ts_key, envir = .lookup_memory_cache)) {
        rm(list = ts_key, envir = .lookup_memory_cache)
      }
      cat("Cleared lookup cache:", lt, "\n")
    }

    # Clear specific types from Redis
    if (use_redis) {
      for (lt in lookup_types) {
        tryCatch(
          redis_hdel(LOOKUP_HASH_KEY, c(lt, paste0(lt, "_timestamp"))),
          error = function(e) warning(paste("[lookup cache] Redis hdel error:", e$message))
        )
      }
    }
  }
  
  invisible()
}

#' Refresh all lookup caches by clearing and re-fetching
#' @return Invisible list of refreshed data
refresh_lookup_caches <- function() {
  # Clear all cached lookups (memory + Redis)
  clear_lookup_cache()
  
  # Re-fetch by calling the lookup functions
  # These will populate the cache on first call
  cat("Refreshing lookup caches from database...\n")
  
  results <- list()
  
  tryCatch({
    results$facilities <- get_facility_lookup()
    cat("  facilities:", nrow(results$facilities), "rows\n")
  }, error = function(e) cat("  facilities: ERROR -", e$message, "\n"))
  
  tryCatch({
    results$foremen <- get_foremen_lookup()
    cat("  foremen:", nrow(results$foremen), "rows\n")
  }, error = function(e) cat("  foremen: ERROR -", e$message, "\n"))
  
  tryCatch({
    results$species <- get_species_lookup()
    cat("  species:", nrow(results$species), "rows\n")
  }, error = function(e) cat("  species: ERROR -", e$message, "\n"))
  
  tryCatch({
    results$structure_types <- get_structure_type_choices(include_all = FALSE)
    cat("  structure_types:", length(results$structure_types), "items\n")
  }, error = function(e) cat("  structure_types: ERROR -", e$message, "\n"))
  
  tryCatch({
    results$spring_thresholds <- get_spring_date_thresholds()
    cat("  spring_thresholds:", if (is.data.frame(results$spring_thresholds)) nrow(results$spring_thresholds) else length(results$spring_thresholds), "items\n")
  }, error = function(e) cat("  spring_thresholds: ERROR -", e$message, "\n"))
  
  cat("Lookup cache refresh complete\n")
  invisible(results)
}

#' Get lookup cache file path for display (legacy compat)
#' @return Description of cache backend
get_lookup_cache_path <- function() {
  "Redis (mmcd:lookup_cache)"
}