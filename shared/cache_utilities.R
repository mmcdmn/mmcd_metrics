# Cache Utilities for Historical Data
# =============================================================================
# Shared functions for managing the cache system across metrics.
# FULLY DYNAMIC - reads from metric_registry.R to determine cacheable metrics.
# =============================================================================

# =============================================================================
# CACHE CONFIGURATION
# =============================================================================

CACHE_ENABLED <- TRUE

#' Get cache directory path (works from any location)
#' @return Path to cache directory
get_cache_dir <- function() {
  paths <- c(
    "/srv/shiny-server/shared/cache",
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

#' Get cache file path
#' @return Path to cache RDS file
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
    return(c("catch_basin", "drone", "ground_prehatch", "structure"))
  }
  
  registry <- get_metric_registry()
  
  # Filter to metrics that support caching (have historical charts)
  cacheable <- sapply(names(registry), function(id) {
    config <- registry[[id]]
    isTRUE(config$historical_enabled) && 
      !isTRUE(config$historical_type == "yearly_grouped")  # yearly_grouped doesn't need cache
  })
  
  names(cacheable)[cacheable]
}

#' Get metrics that ARE currently cached
#' @return Character vector of metric IDs that have cache entries
get_cached_metrics <- function() {
  cache_file <- get_cache_file()
  if (!file.exists(cache_file)) return(character(0))
  
  cache <- readRDS(cache_file)
  keys <- names(cache$averages)
  unique(gsub("_(5yr|10yr)$", "", keys))
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
  cache_file <- get_cache_file()
  all_metrics <- get_cacheable_metrics()
  cached_metrics <- get_cached_metrics()
  
  # Load cache for dates/stats
  cache_info <- if (file.exists(cache_file)) {
    cache <- readRDS(cache_file)
    list(
      generated_date = cache$generated_date,
      averages = cache$averages
    )
  } else {
    list(generated_date = NA, averages = list())
  }
  
  # Build status for each metric
  do.call(rbind, lapply(all_metrics, function(metric_id) {
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
  
  # Load existing cache or create new
  if (file.exists(cache_file)) {
    cache <- readRDS(cache_file)
  } else {
    cache <- list(
      generated_date = Sys.Date(),
      generated_by = Sys.info()["user"],
      zone_filter = zone_filter,
      averages = list()
    )
  }
  
  for (metric_id in metrics) {
    cat("Regenerating cache for:", metric_id, "...\n")
    
    tryCatch({
      # Temporarily disable cache to force DB load
      old_cache_setting <- if (exists("USE_CACHED_AVERAGES")) USE_CACHED_AVERAGES else FALSE
      assign("USE_CACHED_AVERAGES", FALSE, envir = .GlobalEnv)
      
      result <- load_historical_comparison_data(
        metric = metric_id,
        start_year = current_year - 9,
        end_year = current_year,
        zone_filter = zone_filter
      )
      
      # Restore cache setting
      assign("USE_CACHED_AVERAGES", old_cache_setting, envir = .GlobalEnv)
      
      # Store 5-year and 10-year averages
      if (!is.null(result$average) && nrow(result$average) > 0) {
        cache$averages[[paste0(metric_id, "_5yr")]] <- result$average
        cat("  5yr:", nrow(result$average), "rows, avg:", round(mean(result$average$value, na.rm = TRUE), 1), "\n")
      }
      
      if (!is.null(result$ten_year_average) && nrow(result$ten_year_average) > 0) {
        cache$averages[[paste0(metric_id, "_10yr")]] <- result$ten_year_average
        cat("  10yr:", nrow(result$ten_year_average), "rows, avg:", round(mean(result$ten_year_average$value, na.rm = TRUE), 1), "\n")
      }
      
    }, error = function(e) {
      cat("  ERROR:", e$message, "\n")
    })
  }
  
  # Update metadata and save
  cache$generated_date <- Sys.Date()
  cache$zone_filter <- zone_filter
  
  saveRDS(cache, cache_file)
  cat("\nCache saved to:", cache_file, "\n")
  
  invisible(cache)
}

#' Clear cache for specific metrics or all
#' @param metrics Vector of metric IDs to clear, or NULL for all
clear_cache <- function(metrics = NULL) {
  cache_file <- get_cache_file()
  
  if (!file.exists(cache_file)) {
    cat("No cache file exists\n")
    return(invisible())
  }
  
  if (is.null(metrics)) {
    file.remove(cache_file)
    cat("Entire cache cleared\n")
  } else {
    cache <- readRDS(cache_file)
    for (metric_id in metrics) {
      cache$averages[[paste0(metric_id, "_5yr")]] <- NULL
      cache$averages[[paste0(metric_id, "_10yr")]] <- NULL
      cat("Cleared cache for:", metric_id, "\n")
    }
    saveRDS(cache, cache_file)
  }
  
  invisible()
}

# =============================================================================
# LOOKUP CACHE MANAGEMENT
# =============================================================================
# These functions provide access to the file-based lookup cache from db_helpers.R
# enabling test-app to clear/refresh lookup data without restarting the server.

#' Get list of cached lookup types
#' @return Character vector of lookup names that can be cached
get_lookup_cache_types <- function() {
  c("facilities", "foremen", "species", "structure_types", "spring_thresholds")
}

#' Get status of lookup cache (file-based)
#' @return Data frame with cache status for each lookup type
get_lookup_cache_status <- function() {
  # Load cache from file
  cache <- load_lookup_cache()
  
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

#' Clear lookup cache (all or specific types) - file-based
#' @param lookup_types Character vector of lookup types to clear, or NULL for all
#' @return Invisible NULL
clear_lookup_cache_types <- function(lookup_types = NULL) {
  if (is.null(lookup_types)) {
    # Clear all by deleting the file
    clear_lookup_cache()
    cat("All lookup caches cleared\n")
  } else {
    # Clear specific types by removing from the list
    cache <- load_lookup_cache()
    for (lt in lookup_types) {
      if (lt %in% names(cache)) {
        cache[[lt]] <- NULL
        cache[[paste0(lt, "_timestamp")]] <- NULL
        cat("Cleared lookup cache:", lt, "\n")
      }
    }
    save_lookup_cache(cache)
  }
  
  invisible()
}

#' Refresh all lookup caches by clearing and re-fetching (file-based)
#' @return Invisible list of refreshed data
refresh_lookup_caches <- function() {
  # Clear all cached lookups
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
  
  cat("Lookup cache refresh complete\n")
  invisible(results)
}

#' Get lookup cache file path for display
#' @return Path to cache file
get_lookup_cache_path <- function() {
  get_lookup_cache_file()
}