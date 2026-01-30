# Cache Utilities for Historical Data
# Shared functions for managing the cache system across metrics

#' Get list of metrics that should be cached
#' @return Character vector of metric names with cache support
get_cacheable_metrics <- function() {
  # These metrics have pre-calculated cache data available
  c("catch_basin", "drone", "ground_prehatch", "structure")
}

#' Check if a metric supports caching
#' @param metric The metric name to check
#' @return TRUE if metric has cached data available
is_metric_cacheable <- function(metric) {
  metric %in% get_cacheable_metrics()
}

#' Add a new metric to the cache system
#' This is a template function showing what's needed to cache a new metric
#' @param metric_name Name of the metric to cache
#' @param cache_file_path Path to the cache RDS file
add_metric_to_cache <- function(metric_name, cache_file_path) {
  cat("INFO: To add", metric_name, "to cache system:\n")
  cat("1. Run cache generation script with metric included\n")
  cat("2. Add", metric_name, "to get_cacheable_metrics() list above\n")
  cat("3. Verify cache keys:", paste0(metric_name, "_5yr"), "and", paste0(metric_name, "_10yr"), "exist\n")
  
  # Check current cache contents
  if (file.exists(cache_file_path)) {
    cache_data <- readRDS(cache_file_path)
    existing_keys <- names(cache_data$averages)
    metric_keys <- grep(paste0("^", metric_name), existing_keys, value = TRUE)
    
    if (length(metric_keys) > 0) {
      cat("✅ Found existing cache keys for", metric_name, ":", paste(metric_keys, collapse = ", "), "\n")
    } else {
      cat("❌ No cache keys found for", metric_name, "in current cache\n")
      cat("   Available keys:", paste(head(existing_keys, 10), collapse = ", "), "...\n")
    }
  } else {
    cat("❌ Cache file not found at", cache_file_path, "\n")
  }
}

#' Show cache statistics and recommendations
#' @param cache_file_path Path to the cache RDS file  
show_cache_info <- function(cache_file_path) {
  if (!file.exists(cache_file_path)) {
    cat("❌ Cache file not found at", cache_file_path, "\n")
    return(invisible())
  }
  
  cache_data <- readRDS(cache_file_path)
  
  cat("=== CACHE SYSTEM INFO ===\n")
  cat("Cache file:", cache_file_path, "\n")
  cat("Generated:", cache_data$generated_date, "\n")
  cat("Total cached averages:", length(cache_data$averages), "\n")
  
  # Group by metric
  all_keys <- names(cache_data$averages)
  metrics <- unique(gsub("_(5yr|10yr)$", "", all_keys))
  
  cat("\nCached metrics:\n")
  for (metric in metrics) {
    metric_keys <- grep(paste0("^", metric), all_keys, value = TRUE)
    has_5yr <- any(grepl("_5yr$", metric_keys))
    has_10yr <- any(grepl("_10yr$", metric_keys))
    
    status <- if (has_5yr && has_10yr) "✅ Complete" else "⚠️ Partial"
    cat("  -", metric, ":", status, "(", paste(metric_keys, collapse = ", "), ")\n")
  }
  
  # Check for missing common metrics
  common_metrics <- c("cattail_treatments", "control_efficacy", "air_sites")
  cat("\nCommon metrics not cached:\n")
  for (metric in common_metrics) {
    if (!metric %in% metrics) {
      cat("  -", metric, "(consider adding if slow)\n")
    }
  }
}

#' Clean up debug notifications for production
#' @param keep_cache_hits Whether to keep cache hit notifications (useful for monitoring)
cleanup_cache_notifications <- function(keep_cache_hits = TRUE) {
  cat("INFO: To clean up debug notifications for production:\n")
  cat("1. Remove/comment out detailed cache debugging notifications\n")
  cat("2. Keep cache hit notifications:", ifelse(keep_cache_hits, "YES", "NO"), "\n")
  cat("3. Reduce notification duration for remaining messages\n")
  cat("4. Consider adding cache statistics to a hidden admin panel\n")
}

# Example usage:
# show_cache_info("../../cache/historical_averages_cache.rds")  
# add_metric_to_cache("cattail_treatments", "../../cache/historical_averages_cache.rds")
# is_metric_cacheable("catch_basin")  # Returns TRUE
# is_metric_cacheable("cattail_treatments")  # Returns FALSE (not cached yet)