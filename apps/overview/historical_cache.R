# Historical Average Cache System
# =============================================================================
# Pre-calculated historical averages for faster dashboard loading
# Toggle USE_CACHED_AVERAGES to enable/disable caching
# 
# To regenerate cache: source this file and call regenerate_historical_cache()
# =============================================================================

library(dplyr)
library(lubridate)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Toggle this to enable/disable caching
USE_CACHED_AVERAGES <- TRUE

# Cache file location (in the same directory as this file)
CACHE_DIR <- tryCatch({
  dirname(sys.frame(1)$ofile)
}, error = function(e) {
  getwd()  # Fallback to current working directory
})
CACHE_FILE <- file.path(CACHE_DIR, "historical_averages_cache.rds")

# =============================================================================
# CACHE FUNCTIONS
# =============================================================================

#' Get cached historical averages for a metric
#' @param metric_id The metric ID (e.g., "catch_basin", "drone")
#' @param avg_type "5yr" or "10yr"
#' @return Data frame with cached averages by week, or NULL if not cached
#' @export
get_cached_average <- function(metric_id, avg_type = "10yr") {
  if (!USE_CACHED_AVERAGES) return(NULL)
  
  if (!file.exists(CACHE_FILE)) {
    cat("Cache file not found:", CACHE_FILE, "\n")
    return(NULL)
  }
  
  cache <- readRDS(CACHE_FILE)
  
  cache_key <- paste0(metric_id, "_", avg_type)
  if (!cache_key %in% names(cache$averages)) {
    cat("No cached data for:", cache_key, "\n")
    return(NULL)
  }
  
  cat("Using cached", avg_type, "average for", metric_id, "(generated:", cache$generated_date, ")\n")
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
  
  # Metrics to cache
  metrics_to_cache <- c("catch_basin", "drone", "ground_prehatch", "structure")
  
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
      
      # Store 5-year average
      if (!is.null(result$average) && nrow(result$average) > 0) {
        cache$averages[[paste0(metric_id, "_5yr")]] <- result$average
        cat("  - 5-year average: ", round(mean(result$average$value, na.rm = TRUE), 1), "\n")
      }
      
      # Store 10-year average
      if (!is.null(result$ten_year_average) && nrow(result$ten_year_average) > 0) {
        cache$averages[[paste0(metric_id, "_10yr")]] <- result$ten_year_average
        cat("  - 10-year average:", round(mean(result$ten_year_average$value, na.rm = TRUE), 1), "\n")
      }
      
    }, error = function(e) {
      cat("  ERROR:", e$message, "\n")
    })
  }
  
  # Save cache
  saveRDS(cache, CACHE_FILE)
  cat("\n=== CACHE SAVED ===\n")
  cat("File:", CACHE_FILE, "\n")
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
  
  if (!file.exists(CACHE_FILE)) {
    cat("Status: NO CACHE FILE EXISTS\n")
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

#' Clear the cache file
#' @export
clear_cache <- function() {
  if (file.exists(CACHE_FILE)) {
    file.remove(CACHE_FILE)
    cat("Cache cleared:", CACHE_FILE, "\n")
  } else {
    cat("No cache file to clear\n")
  }
}

cat("Historical cache system loaded.\n")
cat("  USE_CACHED_AVERAGES =", USE_CACHED_AVERAGES, "\n")
cat("  To view status: view_cache_status()\n")
cat("  To regenerate:  regenerate_historical_cache()\n")
cat("  To clear:       clear_cache()\n")