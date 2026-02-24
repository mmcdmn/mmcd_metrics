# Historical Average Cache System
# =============================================================================
# Pre-calculated historical averages for faster dashboard loading
# Toggle USE_CACHED_AVERAGES to enable/disable caching
# 
# To regenerate cache: source this file and call regenerate_historical_cache()
# =============================================================================

library(dplyr)
library(lubridate)

# Source Redis cache layer (if available)
.redis_hist_paths <- c(
  "/srv/shiny-server/shared/redis_cache.R",
  "../../shared/redis_cache.R",
  "../../../shared/redis_cache.R",
  file.path(getwd(), "shared/redis_cache.R")
)
for (.rp in .redis_hist_paths) {
  if (file.exists(.rp)) {
    tryCatch(source(.rp, local = FALSE), error = function(e) {
      message("[historical_cache] Redis source error: ", e$message)
    })
    break
  }
}

# =============================================================================
# CONFIGURATION
# =============================================================================

# Toggle this to enable/disable caching
USE_CACHED_AVERAGES <- TRUE

# Cache file location (in the shared/cache directory for all apps)
CACHE_DIR <- tryCatch({
  # Try shared/cache directory first (actual location)
  if (dir.exists("/srv/shiny-server/shared/cache")) {
    "/srv/shiny-server/shared/cache"
  } else if (dir.exists("../../../shared/cache")) {
    "../../../shared/cache"  # Relative path from unified/ subfolder
  } else if (dir.exists("../../shared/cache")) {
    "../../shared/cache"  # Relative path from overview app  
  } else if (dir.exists("/srv/shiny-server/shared")) {
    "/srv/shiny-server/shared"
  } else if (dir.exists("../../../shared")) {
    "../../../shared"  # Relative path from unified/ subfolder
  } else if (dir.exists("../../shared")) {
    "../../shared"  # Relative path from overview app
  } else {
    dirname(sys.frame(1)$ofile)  # Fallback to current file directory
  }
}, error = function(e) {
  getwd()  # Final fallback to current working directory
})
CACHE_FILE <- file.path(CACHE_DIR, "historical_averages_cache.rds")

# =============================================================================
# CACHE FUNCTIONS
# =============================================================================

#' Get cached historical averages for a metric
#' Checks: Redis → .rds file
#' @param metric_id The metric ID (e.g., "catch_basin", "drone")
#' @param avg_type "5yr" or "10yr"
#' @return Data frame with cached averages by week, or NULL if not cached
#' @export
get_cached_average <- function(metric_id, avg_type = "10yr") {
  if (!USE_CACHED_AVERAGES) {
    return(NULL)
  }
  
  cache_key <- paste0(metric_id, "_", avg_type)
  
  # 1. Try Redis first (shared across containers)
  if (exists("get_cached_average_redis", mode = "function") && exists("redis_is_active", mode = "function") && redis_is_active()) {
    redis_result <- tryCatch({
      get_cached_average_redis(metric_id, avg_type)
    }, error = function(e) NULL)
    
    if (!is.null(redis_result)) {
      return(redis_result)
    }
  }
  
  # 2. Fall back to .rds file
  cache_file <- CACHE_FILE
  if (!file.exists(cache_file)) {
    # Try alternative paths
    alt_paths <- c(
      "/srv/shiny-server/shared/cache/historical_averages_cache.rds",
      "/srv/shiny-server/shared/historical_averages_cache.rds",
      "/srv/shiny-server/apps/overview/historical_averages_cache.rds"
    )
    
    cache_file <- NULL
    for (alt_path in alt_paths) {
      if (file.exists(alt_path)) {
        cache_file <- alt_path
        break
      }
    }
    
    if (is.null(cache_file)) {
      return(NULL)
    }
  }
  
  cache <- readRDS(cache_file)
  
  if (!cache_key %in% names(cache$averages)) {
    return(NULL)
  }
  
  return(cache$averages[[cache_key]])
}

#' Check if cache is available and recent
#' @param max_age_days Maximum age of cache in days before it's considered stale
#' @return TRUE if cache is valid and recent
#' @export
is_cache_valid <- function(max_age_days = 7) {
  if (!USE_CACHED_AVERAGES) return(FALSE)
  if (!file.exists(CACHE_FILE)) return(FALSE)
  
  cache <- readRDS(CACHE_FILE)
  cache_age <- as.numeric(Sys.Date() - as.Date(cache$generated_date))
  
  return(cache_age <= max_age_days)
}

#' Regenerate the historical averages cache
#' This function calculates averages for all metrics and saves to cache file
#' @param zone_filter Zone filter to use (default both zones)
#' @export
regenerate_historical_cache <- function(zone_filter = c("1", "2")) {
  cat("=== REGENERATING HISTORICAL AVERAGES CACHE ===\n")
  cat("This may take several minutes...\n\n")
  
  # Source required files
  source('metric_registry.R')
  source('historical_functions.R')
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  registry <- get_metric_registry()
  
  cache <- list(
    generated_date = Sys.Date(),
    generated_by = Sys.info()["user"],
    zone_filter = zone_filter,
    averages = list()
  )
  
  # CRITICAL: Disable cache reads during regeneration to avoid circular dependency
  # (load_historical_comparison_data would return stale cached values otherwise)
  old_cache_setting <- USE_CACHED_AVERAGES
  USE_CACHED_AVERAGES <<- FALSE
  on.exit(USE_CACHED_AVERAGES <<- old_cache_setting)
  
  # Metrics to cache - weekly average metrics
  metrics_to_cache <- c("catch_basin", "drone", "ground_prehatch", "structure", "mosquito_monitoring", "air_sites")
  
  # Yearly grouped metrics (cached differently - store yearly_data per overview_type)
  yearly_grouped_metrics <- c("cattail_treatments")
  
  for (metric_id in metrics_to_cache) {
    if (!metric_id %in% names(registry)) {
      cat("Skipping unknown metric:", metric_id, "\n")
      next
    }
    
    cat("Calculating averages for:", metric_id, "...\n")
    
    tryCatch({
      # Load 10 years of data
      result <- load_historical_comparison_data(
        metric = metric_id,
        start_year = current_year - 9,
        end_year = current_year,
        zone_filter = zone_filter
      )
      
      # Store zone-level data if available (for zone-specific retrieval)
      avg_to_store <- if (!is.null(result$average_by_zone)) result$average_by_zone else result$average
      ten_to_store <- if (!is.null(result$ten_year_by_zone)) result$ten_year_by_zone else result$ten_year_average
      
      # Store 5-year average
      if (!is.null(avg_to_store) && nrow(avg_to_store) > 0) {
        cache$averages[[paste0(metric_id, "_5yr")]] <- avg_to_store
        zone_info <- if ("zone" %in% names(avg_to_store)) paste0(" (", nrow(avg_to_store), " zone-level rows)") else ""
        cat("  - 5-year average: ", round(mean(avg_to_store$value, na.rm = TRUE), 1), zone_info, "\n")
      }
      
      # Store 10-year average
      if (!is.null(ten_to_store) && nrow(ten_to_store) > 0) {
        cache$averages[[paste0(metric_id, "_10yr")]] <- ten_to_store
        zone_info <- if ("zone" %in% names(ten_to_store)) paste0(" (", nrow(ten_to_store), " zone-level rows)") else ""
        cat("  - 10-year average:", round(mean(ten_to_store$value, na.rm = TRUE), 1), zone_info, "\n")
      }
      
    }, error = function(e) {
      cat("  ERROR:", e$message, "\n")
    })
  }
  
  # Cache yearly grouped metrics (cattail_treatments, etc.)
  for (metric_id in yearly_grouped_metrics) {
    if (!metric_id %in% names(registry)) {
      cat("Skipping unknown yearly grouped metric:", metric_id, "\n")
      next
    }
    
    cat("Caching yearly grouped data for:", metric_id, "...\n")
    
    tryCatch({
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
          cat("  -", ov_type, ":", nrow(result$yearly_data), "rows,",
              n_distinct(result$yearly_data$year), "years\n")
        }
      }
    }, error = function(e) {
      cat("  ERROR:", e$message, "\n")
    })
  }
  
  # Save cache to .rds file
  saveRDS(cache, CACHE_FILE)
  cat("\n=== CACHE SAVED ===\n")
  cat("File:", CACHE_FILE, "\n")
  
  # Also save to Redis for cross-container sharing
  if (exists("save_historical_cache_redis", mode = "function") && exists("redis_is_active", mode = "function") && redis_is_active()) {
    tryCatch({
      save_historical_cache_redis(cache)
      cat("Redis: saved\n")
    }, error = function(e) {
      cat("Redis: save failed -", e$message, "\n")
    })
  }
  
  cat("Metrics cached:", length(cache$averages), "\n")
  cat("Generated:", as.character(cache$generated_date), "\n")
  
  return(invisible(cache))
}

#' View cache status and contents
#' @export
view_cache_status <- function() {
  cat("=== HISTORICAL AVERAGES CACHE STATUS ===\n")
  cat("Caching enabled:", USE_CACHED_AVERAGES, "\n")
  cat("Cache file:", CACHE_FILE, "\n")
  
  # Redis status
  if (exists("redis_is_active", mode = "function")) {
    cat("Redis backend:", if (redis_is_active()) "connected" else "disconnected", "\n")
    if (redis_is_active()) {
      tryCatch({
        info <- redis_cache_status()
        cat("Redis historical metrics:", info$historical_count, "\n")
      }, error = function(e) NULL)
    }
  } else {
    cat("Redis backend: not configured\n")
  }
  
  if (!file.exists(CACHE_FILE)) {
    cat("File status: NO CACHE FILE EXISTS\n")
    cat("Run regenerate_historical_cache() to create cache\n")
    return(invisible(NULL))
  }
  
  cache <- readRDS(CACHE_FILE)
  cat("Generated:", as.character(cache$generated_date), "\n")
  cat("Age:", as.numeric(Sys.Date() - as.Date(cache$generated_date)), "days\n")
  cat("Zone filter:", paste(cache$zone_filter, collapse = ", "), "\n")
  cat("\nCached averages:\n")
  
  for (key in names(cache$averages)) {
    avg_val <- mean(cache$averages[[key]]$value, na.rm = TRUE)
    cat("  -", key, ":", round(avg_val, 1), "(", nrow(cache$averages[[key]]), "weeks )\n")
  }
  
  return(invisible(cache))
}

#' Clear the cache file and Redis
#' @export
clear_cache <- function() {
  if (file.exists(CACHE_FILE)) {
    file.remove(CACHE_FILE)
    cat("Cache file cleared:", CACHE_FILE, "\n")
  } else {
    cat("No cache file to clear\n")
  }
  
  # Also clear Redis historical cache
  if (exists("redis_is_active", mode = "function") && redis_is_active()) {
    tryCatch({
      r <- get_redis()
      prefix <- REDIS_CONFIG$prefix
      r$DEL(paste0(prefix, HISTORICAL_HASH_KEY))
      r$DEL(paste0(prefix, HISTORICAL_META_KEY))
      cat("Redis historical cache cleared\n")
    }, error = function(e) {
      cat("Redis clear failed:", e$message, "\n")
    })
  }
}

cat("Historical cache system loaded.\n")
cat("  USE_CACHED_AVERAGES =", USE_CACHED_AVERAGES, "\n")
cat("  To view status: view_cache_status()\n")
cat("  To regenerate:  regenerate_historical_cache()\n")
cat("  To clear:       clear_cache()\n")