# Overview - Historical Data Functions
# =============================================================================
# COMPLETELY DYNAMIC historical data loading.
# NO SQL HERE - calls app functions through the registry.
# NO metric-specific logic - iterates through registry.
# 
# CACHING: Source historical_cache.R and set USE_CACHED_AVERAGES = TRUE
#          to use pre-calculated averages instead of live queries.
# =============================================================================

library(dplyr)
library(lubridate)

# Source cache utilities (shared across all apps)
cache_utilities_file <- tryCatch({
  if (file.exists("/srv/shiny-server/shared/cache_utilities.R")) {
    "/srv/shiny-server/shared/cache_utilities.R"
  } else if (file.exists("../../../shared/cache_utilities.R")) {
    "../../../shared/cache_utilities.R"
  } else if (file.exists("../../shared/cache_utilities.R")) {
    "../../shared/cache_utilities.R"
  } else {
    "../shared/cache_utilities.R"
  }
}, error = function(e) {
  "cache_utilities.R"
})

if (file.exists(cache_utilities_file)) {
  source(cache_utilities_file, local = FALSE)
}

# Source cache system if available
cache_file <- tryCatch({
  if (file.exists("/srv/shiny-server/shared/historical_cache.R")) {
    "/srv/shiny-server/shared/historical_cache.R"
  } else if (file.exists("../../shared/historical_cache.R")) {
    "../../shared/historical_cache.R"
  } else if (file.exists("../historical_cache.R")) {
    "../historical_cache.R"
  } else if (file.exists("historical_cache.R")) {
    "historical_cache.R"
  } else {
    "/srv/shiny-server/apps/overview/historical_cache.R"
  }
}, error = function(e) {
  "historical_cache.R"
})

if (file.exists(cache_file)) {
  source(cache_file, local = FALSE)  # Source into global environment
} else {
  USE_CACHED_AVERAGES <- FALSE
}

# =============================================================================
# CONFIGURATION
# =============================================================================

# =============================================================================
# SHARED VALUE COLUMN HELPER
# =============================================================================
#' Assign the 'value' column to a treatments data frame based on metric config.
#' Used by load_current_year_for_cache, load_yearly_grouped_data, and the main
#' historical calculation. Centralizes the logic to avoid drift between callers.
#' @param treatments Data frame of treatment records
#' @param config Metric config from registry
#' @return treatments with a 'value' column added/updated
assign_value_column <- function(treatments, config) {
  aggregate_as_avg <- isTRUE(config$aggregate_as_average)
  has_acres <- isTRUE(config$has_acres)
  
  if (aggregate_as_avg && "value" %in% names(treatments)) {
    # Keep existing pre-aggregated values (e.g., avg mosquitoes per trap)
    treatments$value <- as.numeric(treatments$value)
  } else if (has_acres) {
    acres_col <- if ("treated_acres" %in% names(treatments)) "treated_acres"
                 else if ("acres" %in% names(treatments)) "acres" else NULL
    treatments$value <- if (!is.null(acres_col)) treatments[[acres_col]] else 1
  } else {
    treatments$value <- 1
  }
  treatments
}

#' Load current year data for cache path (helper function)
#' Handles both active treatment metrics and simple count metrics
#' Results are cached via get_cached_curyr_data (3-min TTL) to avoid redundant
#' DB queries when multiple views/metrics request the same current year data.
#' @param metric Metric ID
#' @param analysis_year Year to load
#' @param zone_filter Zone filter
#' @param config Metric config from registry
#' @return Data frame with time_period, week_num, value, group_label columns
load_current_year_for_cache <- function(metric, analysis_year, zone_filter, config) {
  # Use tiered cache helper (3-min TTL) if available
  cache_id <- paste0(metric, ":", analysis_year, ":", paste(sort(zone_filter), collapse = "_"))
  if (exists("get_cached_curyr_data", mode = "function")) {
    return(get_cached_curyr_data(cache_id, function() {
      .load_current_year_for_cache_uncached(metric, analysis_year, zone_filter, config)
    }))
  }

  # Fallback: direct call without caching
  .load_current_year_for_cache_uncached(metric, analysis_year, zone_filter, config)
}

#' Uncached implementation of load_current_year_for_cache
.load_current_year_for_cache_uncached <- function(metric, analysis_year, zone_filter, config) {
  tryCatch({
    raw_data <- load_app_historical_data(metric, analysis_year, analysis_year, zone_filter)
    
    if (is.null(raw_data$treatments) || nrow(raw_data$treatments) == 0) {
      return(data.frame(time_period = character(), week_num = integer(), 
                        value = numeric(), group_label = character()))
    }
    
    treatments <- raw_data$treatments
    treatments$inspdate <- as.Date(treatments$inspdate)
    
    has_acres <- isTRUE(config$has_acres)
    is_active <- isTRUE(config$is_active_treatment) || isTRUE(config$use_active_calculation)
    aggregate_as_avg <- isTRUE(config$aggregate_as_average)
    
    # Assign value column using shared helper
    treatments <- assign_value_column(treatments, config)
    
    # Calculate weekly data
    if (is_active) {
      if (!"effect_days" %in% names(treatments)) {
        treatments$effect_days <- if (metric == "catch_basin") 28 else 14
      }
      treatments$treatment_end <- treatments$inspdate + treatments$effect_days
      
      weekly_data <- data.frame()
      start_date <- as.Date(paste0(analysis_year, "-01-01"))
      end_date <- min(as.Date(paste0(analysis_year, "-12-31")), Sys.Date())
      
      # Use sitecode/catchbasin_id for dedup (same logic as historical calculation)
      if ("sitecode" %in% names(treatments)) {
        treatments$treatment_id <- treatments$sitecode
      } else if ("catchbasin_id" %in% names(treatments)) {
        treatments$treatment_id <- treatments$catchbasin_id
      }
      
      weekly_list <- vector("list", 53L)
      widx <- 0L
      for (week_start in seq.Date(start_date, end_date, by = "week")) {
        # Convert back to Date (R for loop strips Date class)
        week_start <- as.Date(week_start, origin = "1970-01-01")
        week_friday <- week_start + 4
        week_num <- week(week_friday)
        active <- treatments %>% filter(inspdate <= week_friday, treatment_end >= week_friday)
        
        if (nrow(active) > 0) {
          # Dedup: keep treatment lasting longest per site (consistent with historical calc)
          if ("treatment_id" %in% names(active)) {
            active <- active %>%
              arrange(desc(treatment_end)) %>%
              distinct(treatment_id, .keep_all = TRUE)
          }
          week_value <- sum(active$value, na.rm = TRUE)
          widx <- widx + 1L
          weekly_list[[widx]] <- data.frame(
            year = analysis_year, week_num = week_num, value = week_value
          )
        }
      }
      weekly_data <- if (widx > 0) do.call(rbind, weekly_list[1:widx]) else data.frame()
    } else {
      weekly_data <- treatments %>%
        mutate(year = year(inspdate), week_num = week(inspdate)) %>%
        group_by(year, week_num) %>%
        summarize(value = if (aggregate_as_avg) mean(value, na.rm = TRUE) else sum(value, na.rm = TRUE), .groups = "drop")
    }
    
    # Format output
    if (nrow(weekly_data) > 0) {
      weekly_data %>%
        mutate(time_period = sprintf("W%02d", week_num), 
               group_label = as.character(analysis_year)) %>%
        select(time_period, week_num, value, group_label)
    } else {
      data.frame(time_period = character(), week_num = integer(), 
                 value = numeric(), group_label = character())
    }
  }, error = function(e) {
    cat("ERROR loading current year for", metric, ":", e$message, "\n")
    data.frame(time_period = character(), week_num = integer(), 
               value = numeric(), group_label = character())
  })
}

#' Get the year range for historical data
#' @param n_years Number of years to include (default 5, includes current year)
#' @param analysis_date Date to use as "current" (default Sys.Date())
#' @return List with start_year and end_year
get_historical_year_range <- function(n_years = 5, analysis_date = NULL) {
  if (is.null(analysis_date)) analysis_date <- Sys.Date()
  current_year <- as.numeric(format(as.Date(analysis_date), "%Y"))
  list(
    start_year = current_year - n_years + 1,
    end_year = current_year
  )
}

# =============================================================================
# UNIFIED HISTORICAL DATA LOADER - COMPLETELY DYNAMIC
# =============================================================================

#' Load raw historical treatments from an app's data_functions.R
#' 
#' This is THE ONLY place that loads data. It dynamically sources the app's
#' data_functions.R and calls load_raw_data (with include_archive=TRUE).
#' 
#' @param metric_id The metric ID from registry
#' @param start_year Start year
#' @param end_year End year
#' @param zone_filter Zones to include
#' @return List with $treatments data frame containing inspdate column
load_app_historical_data <- function(metric_id, start_year, end_year, zone_filter = c("1", "2")) {
  
  registry <- get_metric_registry()
  config <- registry[[metric_id]]
  
  if (is.null(config)) {
    cat("ERROR: Metric", metric_id, "not found in registry\n")
    return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
  }
  
  # Use cached app environments if available (normal app context)
  # Fallback to direct sourcing when called from cache_utilities.R or standalone scripts
  env <- NULL
  if (exists("get_app_envs", mode = "function")) {
    app_envs <- get_app_envs()
    env <- app_envs[[metric_id]]
  }
  
  # Fallback: source the app's data_functions.R directly into a fresh environment
  if (is.null(env)) {
    apps_base <- get_apps_base_path()
    app_folder <- config$app_folder
    data_file <- file.path(apps_base, app_folder, "data_functions.R")
    if (file.exists(data_file)) {
      env <- new.env(parent = globalenv())
      tryCatch(
        source(data_file, local = env, chdir = TRUE),
        error = function(e) cat("ERROR sourcing", data_file, ":", e$message, "\n")
      )
    }
  }
  
  if (is.null(env) || length(ls(env)) == 0) {
    cat("ERROR: No app environment loaded for", metric_id, "\n")
    return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
  }
  
  tryCatch({
    if ("load_raw_data" %in% names(env)) {
      end_date <- as.Date(paste0(end_year, "-12-31"))
      result <- env$load_raw_data(
        analysis_date = end_date,
        include_archive = TRUE,
        start_year = start_year,
        end_year = end_year
      )
    } else {
      cat("ERROR: load_raw_data not found for", metric_id, "\n")
      return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
    }
    
    # Filter by zone if treatments have zone column
    if (!is.null(result$treatments) && nrow(result$treatments) > 0) {
      if ("zone" %in% names(result$treatments) && !is.null(zone_filter)) {
        result$treatments <- result$treatments %>% filter(zone %in% zone_filter)
      }
    }
    
    result
    
  }, error = function(e) {
    cat("ERROR loading historical data for", metric_id, ":", e$message, "\n")
    list(sites = data.frame(), treatments = data.frame(), total_count = 0)
  })
}

# =============================================================================
# YEARLY GROUPED DATA LOADER - For cattail treatments and similar metrics
# =============================================================================

#' Load yearly grouped data for metrics with historical_type = "yearly_grouped"
#' 
#' @param metric Metric ID
#' @param treatments Treatment data frame with inspdate column
#' @param config Metric configuration from registry
#' @param overview_type One of: "district", "facilities", "fos"
#' @param start_year First year to include
#' @param end_year Last year to include
#' @return List with $yearly_data, $average, $current (last two empty for compatibility)
load_yearly_grouped_data <- function(metric, treatments, config, overview_type = "facilities",
                                    start_year = NULL, end_year = NULL) {
  
  # Assign value column using shared helper
  treatments <- assign_value_column(treatments, config)
  
  # Add year column - use config-specified column if available, otherwise inspdate year
  year_col <- config$historical_year_column
  if (!is.null(year_col) && year_col %in% names(treatments)) {
    treatments$year <- treatments[[year_col]]
  } else {
    treatments$year <- year(treatments$inspdate)
  }
  
  # Filter to requested year range
  if (!is.null(start_year)) {
    treatments <- treatments %>% filter(year >= start_year)
  }
  if (!is.null(end_year)) {
    treatments <- treatments %>% filter(year <= end_year)
  }
  
  # Determine group_by based on config and overview_type
  group_by_col <- config$historical_group_by %||% "facility"
  
  if (overview_type == "district") {
    # District overview: aggregate to MMCD total
    yearly_data <- treatments %>%
      group_by(year) %>%
      summarise(
        value = sum(value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(group_label = "MMCD Total")
  } else if (group_by_col == "facility" && "facility" %in% names(treatments)) {
    # Facilities overview: group by facility
    yearly_data <- treatments %>%
      group_by(year, facility) %>%
      summarise(
        value = sum(value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(group_label = facility) %>%
      select(year, group_label, value)
  } else {
    # Fallback: aggregate to MMCD total
    yearly_data <- treatments %>%
      group_by(year) %>%
      summarise(
        value = sum(value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(group_label = "MMCD Total")
  }
  
  # Return with empty average/current for compatibility
  list(
    yearly_data = yearly_data,
    average = data.frame(),
    current = data.frame(),
    total_count = nrow(treatments)
  )
}

# =============================================================================
# 5-YEAR AVERAGE VS CURRENT YEAR COMPARISON LOADER
# =============================================================================

#' Load historical comparison data: 5-year weekly average + current year
#' 
#' This function calculates ACTIVE treatments per week (not treatments done that week).
#' A treatment is ACTIVE on a given week if: inspdate <= week_friday AND inspdate + effect_days >= week_friday
#' 
#' For metrics with historical_type = "yearly_grouped", returns yearly totals instead.
#' 
#' @param metric Metric ID (from registry)
#' @param start_year Start year for 5-year average calculation
#' @param end_year End year (current year)
#' @param display_metric Metric to display: "treatment_acres", "treatments", "sites"
#' @param zone_filter Vector of zones to include
#' @param overview_type One of: "district", "facilities", "fos" (for yearly_grouped)
#' @return List with $average (5-yr avg by week) and $current (current year by week)
#'         OR $yearly_data for yearly_grouped metrics
#' @export
load_historical_comparison_data <- function(metric,
                                             start_year = NULL,
                                             end_year = NULL,
                                             display_metric = "treatment_acres",
                                             zone_filter = c("1", "2"),
                                             analysis_date = NULL,
                                             overview_type = "facilities",
                                             facility_filter = NULL) {
  
  # Get registry config for metric
  registry <- get_metric_registry()
  config <- registry[[metric]]
  
  # CHECK FOR RAW DATA ONLY MODE (mosquito monitoring)
  if (isTRUE(config$raw_data_only)) {
    # For mosquito monitoring: return ONLY current year raw data, no averages
    raw_data <- load_app_historical_data(metric, end_year, end_year, zone_filter)
    
    if (is.null(raw_data$treatments) || nrow(raw_data$treatments) == 0) {
      return(list(average = data.frame(), current = data.frame(), yearly_data = data.frame(), total_count = 0))
    }
    
    treatments <- raw_data$treatments %>% 
      filter(inspdate <= (if(is.null(analysis_date)) Sys.Date() else analysis_date))
    treatments$inspdate <- as.Date(treatments$inspdate)
    
    # Return ONLY current year data as a simple time series (no averages)
    return(list(
      average = data.frame(),  # NO 5-year average
      current = treatments %>% 
        group_by(inspdate) %>% 
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(group_label = as.character(end_year)),
      yearly_data = data.frame(),  # NO yearly data
      total_count = sum(treatments$value, na.rm = TRUE)
    ))
  }
  
  # Initialize cache variables (may be set by cache path below)
  cached_5yr <- NULL
  cached_10yr <- NULL
  
  # ==========================================================================
  # CACHE PATH: Try to use cached averages first (much faster)
  # Skip cache when facility_filter is set — use Redis facility-specific cache instead
  # ==========================================================================
  if (is.null(facility_filter) &&
      exists("get_cached_average") && exists("USE_CACHED_AVERAGES") && USE_CACHED_AVERAGES &&
      exists("is_metric_cacheable") && is_metric_cacheable(metric)) {
    
    cached_5yr <- tryCatch(get_cached_average(metric, "5yr"), error = function(e) NULL)
    cached_10yr <- tryCatch(get_cached_average(metric, "10yr"), error = function(e) NULL)
    
    if (!is.null(cached_10yr) && !is.null(cached_5yr)) {
      # Aggregate cached zone-level data for the requested zone_filter
      # For average-based metrics (e.g., avg/trap), take mean across zones
      # For count/acres-based metrics, sum across zones
      agg_fn <- if (isTRUE(config$aggregate_as_average)) "mean" else "sum"
      
      if ("zone" %in% names(cached_5yr)) {
        cached_5yr <- cached_5yr %>%
          filter(zone %in% zone_filter) %>%
          group_by(week_num, time_period, group_label) %>%
          summarize(value = if (agg_fn == "mean") mean(value, na.rm = TRUE) 
                           else sum(value, na.rm = TRUE), .groups = "drop")
      }
      if ("zone" %in% names(cached_10yr)) {
        cached_10yr <- cached_10yr %>%
          filter(zone %in% zone_filter) %>%
          group_by(week_num, time_period, group_label) %>%
          summarize(value = if (agg_fn == "mean") mean(value, na.rm = TRUE) 
                           else sum(value, na.rm = TRUE), .groups = "drop")
      }
      
      # Load current year data fresh (already filtered by zone_filter)
      years <- get_historical_year_range(10, analysis_date)
      analysis_year <- years$end_year
      current_formatted <- load_current_year_for_cache(metric, analysis_year, zone_filter, config)
      
      return(list(
        average = cached_5yr,
        ten_year_average = cached_10yr,
        current = current_formatted
      ))
    }
  }
  
  # ==========================================================================
  # CACHE PATH: Yearly grouped metrics (e.g., cattail_treatments)
  # ==========================================================================
  if (is.null(facility_filter) &&
      isTRUE(config$historical_type == "yearly_grouped") &&
      exists("get_cached_average") && exists("USE_CACHED_AVERAGES") && USE_CACHED_AVERAGES) {
    cache_key <- paste0(metric, "_yearly_", overview_type)
    cached_yearly <- tryCatch(get_cached_average(metric, paste0("yearly_", overview_type)), error = function(e) NULL)
    
    if (!is.null(cached_yearly) && nrow(cached_yearly) > 0) {
      cat("Using cached yearly grouped data for", metric, "(", overview_type, ")\n")
      return(list(
        yearly_data = cached_yearly,
        average = data.frame(),
        current = data.frame(),
        total_count = nrow(cached_yearly)
      ))
    }
  }
  
  # ==========================================================================
  # CACHE PATH: Facility-filtered historical (Redis, 24-hr TTL)
  # When viewing FOS overview, cache the full result per facility + metric
  # ==========================================================================
  if (!is.null(facility_filter) && facility_filter != "all" &&
      exists("get_cached_fac_hist_data")) {
    
    # Build a stable cache key from metric + facility + zones + analysis week
    analysis_week <- if (!is.null(analysis_date)) {
      paste0(lubridate::year(analysis_date), "_W", lubridate::week(analysis_date))
    } else {
      paste0(lubridate::year(Sys.Date()), "_W", lubridate::week(Sys.Date()))
    }
    
    fac_hist_id <- paste0(metric, "_", facility_filter)
    cached_result <- get_cached_fac_hist_data(
      fac_hist_id, NULL,
      metric, facility_filter, sort(zone_filter), overview_type, analysis_week
    )
    
    if (!is.null(cached_result)) {
      cat("[fachist] Cache HIT for", metric, "facility:", facility_filter, "\n")
      return(cached_result)
    }
    
    cat("[fachist] Cache MISS for", metric, "facility:", facility_filter, "- loading from DB\n")
  }
  
  # ==========================================================================
  # DATABASE FALLBACK: Load full historical data from database
  # ==========================================================================
  
  # Get default year range if not specified
  # Always use 10 years to have data for 10-year average calculations
  if (is.null(start_year) || is.null(end_year)) {
    n_years <- 10  # Always load 10 years for 10-year average
    years <- get_historical_year_range(n_years, analysis_date)
    start_year <- years$start_year
    end_year <- years$end_year
  }
  
  # Use analysis_date to filter out future data
  if (is.null(analysis_date)) analysis_date <- Sys.Date()
  analysis_date <- as.Date(analysis_date)
  
  # Load raw data from app
  raw_data <- load_app_historical_data(metric, start_year, end_year, zone_filter)
  
  if (is.null(raw_data$treatments) || nrow(raw_data$treatments) == 0) {
    return(list(average = data.frame(), current = data.frame(), yearly_data = data.frame(), total_count = raw_data$total_count %||% 0))
  }
  
  treatments <- raw_data$treatments
  
  # CRITICAL: Filter out treatments after analysis_date (don't show future data)
  treatments <- treatments %>% filter(inspdate <= analysis_date)
  
  # Apply facility filter for FOS view (show facility-specific historical)
  if (!is.null(facility_filter) && facility_filter != "all" && "facility" %in% names(treatments)) {
    treatments <- map_facility_names(treatments)
    treatments <- treatments %>%
      filter(facility == facility_filter | facility_display == facility_filter)
    if ("facility_display" %in% names(treatments)) {
      treatments <- treatments %>% select(-facility_display)
    }
    if (nrow(treatments) == 0) {
      return(list(average = data.frame(), current = data.frame(), yearly_data = data.frame(), total_count = 0))
    }
  }
  
  # Ensure inspdate is Date type
  if (!"inspdate" %in% names(treatments)) {
    cat("ERROR: treatments missing inspdate column for", metric, "\n")
    return(list(average = data.frame(), current = data.frame(), yearly_data = data.frame()))
  }
  treatments$inspdate <- as.Date(treatments$inspdate)
  
  # Config already loaded above - extract what we need
  has_acres <- isTRUE(config$has_acres)
  use_active <- isTRUE(config$use_active_calculation)
  
  # Check if this metric uses yearly grouped chart
  if (isTRUE(config$historical_type == "yearly_grouped")) {
    yearly_result <- load_yearly_grouped_data(metric, treatments, config, overview_type, start_year, end_year)
    
    # Cache facility-filtered yearly grouped result before returning
    if (!is.null(facility_filter) && facility_filter != "all" &&
        exists("set_app_cached_redis") && exists("build_cache_key") && exists("CACHE_PREFIX_FAC_HIST")) {
      analysis_week <- if (!is.null(analysis_date)) {
        paste0(lubridate::year(analysis_date), "_W", lubridate::week(analysis_date))
      } else {
        paste0(lubridate::year(Sys.Date()), "_W", lubridate::week(Sys.Date()))
      }
      fac_hist_id <- paste0(metric, "_", facility_filter)
      cache_key <- build_cache_key(CACHE_PREFIX_FAC_HIST, fac_hist_id,
                                    metric, facility_filter, sort(zone_filter), overview_type, analysis_week)
      set_app_cached_redis(cache_key, yearly_result, ttl = TTL_24_HR)
      cat("[fachist] Cached yearly grouped result for", metric, "facility:", facility_filter, "\n")
    }
    
    return(yearly_result)
  }
  
  # Assign value column using shared helper (reduces duplication)
  treatments <- assign_value_column(treatments, config)
  
  current_year <- end_year
  n_prior_years <- current_year - start_year + 1
  
  # =========================================================================
  # ACTIVE TREATMENT CALCULATION (if use_active_calculation = TRUE)
  # For each week, count what's ACTIVE on that week's Friday
  # =========================================================================
  if (use_active) {
    # Get effect_days column (default to 14 for ground/drone, 28 for catch basin)
    if ("effect_days" %in% names(treatments)) {
      treatments$effect_days <- ifelse(is.na(treatments$effect_days), 14, treatments$effect_days)
    } else {
      # Default effect_days based on metric type
      treatments$effect_days <- if (metric == "catch_basin") 28 else 14
    }
    
    # Calculate treatment end date
    treatments <- treatments %>%
      mutate(treatment_end = inspdate + effect_days)
    
    # Get a unique ID for each treatment row (to avoid double-counting)
    # Use sitecode if available, otherwise row number
    if ("sitecode" %in% names(treatments)) {
      treatments$treatment_id <- treatments$sitecode
    } else if ("catchbasin_id" %in% names(treatments)) {
      treatments$treatment_id <- treatments$catchbasin_id
    } else {
      treatments$treatment_id <- seq_len(nrow(treatments))
    }
    
    # Determine if zone-level reporting is possible
    has_zone_data <- "zone" %in% names(treatments)
    
    # Generate weeks for all years in range — collect in list, bind once at end
    weekly_list <- vector("list", (end_year - start_year + 1) * 53L)
    widx <- 0L
    
    for (yr in start_year:end_year) {
      start_date <- as.Date(paste0(yr, "-01-01"))
      end_date <- as.Date(paste0(yr, "-12-31"))
      weeks_in_year <- seq.Date(start_date, end_date, by = "week")
      
      for (week_start in weeks_in_year) {
        # Convert back to Date (R for loop strips Date class)
        week_start <- as.Date(week_start, origin = "1970-01-01")
        week_friday <- week_start + 4
        
        # Skip if Friday spills into next year (avoids duplicate week_num=1)
        if (year(week_friday) != yr) next
        
        week_num <- week(week_friday)
        
        # Find treatments ACTIVE on this Friday
        active_on_friday <- treatments %>%
          filter(
            inspdate <= week_friday,
            treatment_end >= week_friday
          )
        
        # Always emit a row — zero-activity weeks count toward the average
        if (nrow(active_on_friday) > 0) {
          # Dedup: keep treatment lasting longest per site (most future coverage)
          if ("treatment_id" %in% names(active_on_friday)) {
            active_on_friday <- active_on_friday %>%
              arrange(desc(treatment_end)) %>%
              distinct(treatment_id, .keep_all = TRUE)
          }
          
          if (has_zone_data) {
            # Aggregate by zone for zone-level cache
            zone_values <- active_on_friday %>%
              group_by(zone) %>%
              summarize(value = sum(value, na.rm = TRUE), .groups = "drop")
            
            # Ensure all zones in zone_filter have a row (zero-fill missing zones)
            all_zones <- data.frame(zone = zone_filter, stringsAsFactors = FALSE)
            zone_row <- all_zones %>%
              left_join(zone_values, by = "zone") %>%
              mutate(value = ifelse(is.na(value), 0, value),
                     year = yr, week_num = week_num)
            widx <- widx + 1L
            weekly_list[[widx]] <- zone_row
          } else {
            week_value <- sum(active_on_friday$value, na.rm = TRUE)
            widx <- widx + 1L
            weekly_list[[widx]] <- data.frame(
              year = yr, week_num = week_num, value = week_value
            )
          }
        } else {
          # No active treatments — emit zeros (zone-aware or single row)
          if (has_zone_data) {
            widx <- widx + 1L
            weekly_list[[widx]] <- data.frame(
              zone = zone_filter, year = yr, week_num = week_num, value = 0,
              stringsAsFactors = FALSE
            )
          } else {
            widx <- widx + 1L
            weekly_list[[widx]] <- data.frame(
              year = yr, week_num = week_num, value = 0
            )
          }
        }
      }
    }
    weekly_data <- if (widx > 0) do.call(rbind, weekly_list[1:widx]) else data.frame()
  } else {
    # =========================================================================
    # SIMPLE TREATMENT COUNT (by week of treatment date)
    # =========================================================================
    treatments <- treatments %>%
      mutate(
        year = year(inspdate),
        week_num = week(inspdate)
      )
    
    has_zone_data <- "zone" %in% names(treatments)
    
    # Use mean for metrics where values are already averages (e.g., mosquito monitoring)
    # Use sum for metrics counting treatments or acres
    if (isTRUE(config$aggregate_as_average)) {
      # Averaged metrics (mosquito monitoring): keep MMCD-wide, no zone split
      # Zone-level means can't be simply summed for correct combined average
      weekly_data <- treatments %>%
        group_by(year, week_num) %>%
        summarize(value = mean(value, na.rm = TRUE), .groups = "drop")
      
      # Zero-fill: include all year-week combos so zero-activity weeks
      # count toward the average (consistent with active-treatment metrics)
      all_year_weeks <- expand.grid(
        year = start_year:end_year,
        week_num = sort(unique(weekly_data$week_num))
      )
      weekly_data <- merge(all_year_weeks, weekly_data, by = c("year", "week_num"), all.x = TRUE)
      weekly_data$value[is.na(weekly_data$value)] <- 0
      weekly_data <- tibble::as_tibble(weekly_data)
    } else if (has_zone_data) {
      # Treatment count/acres metrics: aggregate by zone for zone-level cache
      weekly_data <- treatments %>%
        group_by(year, week_num, zone) %>%
        summarize(value = sum(value, na.rm = TRUE), .groups = "drop")
    } else {
      weekly_data <- treatments %>%
        group_by(year, week_num) %>%
        summarize(value = sum(value, na.rm = TRUE), .groups = "drop")
    }
  }
  
  # =========================================================================
  # SPLIT INTO PREVIOUS YEARS (for average) AND CURRENT YEAR
  # =========================================================================
  previous_data <- weekly_data %>% filter(year < current_year)
  current_data <- weekly_data %>% filter(year == current_year)
  
  # Split previous data into 5-year and 10-year windows
  five_year_cutoff <- current_year - 5  # Years >= this are in 5-year window
  ten_year_cutoff <- current_year - 10  # Years >= this are in 10-year window
  
  previous_5yr <- previous_data %>% filter(year >= five_year_cutoff)
  previous_10yr <- previous_data  # Use ALL available historical data for 10-year average

  has_zone_col <- "zone" %in% names(weekly_data)

  # Calculate averages — zone-aware when zone data exists
  if (!is.null(cached_5yr)) {
    average_data <- cached_5yr
  } else if (has_zone_col) {
    average_data <- previous_5yr %>%
      group_by(week_num, zone) %>%
      summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(time_period = sprintf("W%02d", week_num), group_label = "5-Year Avg")
  } else {
    average_data <- previous_5yr %>%
      group_by(week_num) %>%
      summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(time_period = sprintf("W%02d", week_num), group_label = "5-Year Avg") %>%
      select(time_period, week_num, value, group_label)
  }
  
  if (!is.null(cached_10yr)) {
    ten_year_avg_data <- cached_10yr
  } else if (has_zone_col) {
    ten_year_avg_data <- previous_data %>%
      group_by(week_num, zone) %>%
      summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(time_period = sprintf("W%02d", week_num), group_label = "10-Year Avg")
  } else {
    ten_year_avg_data <- previous_data %>%
      group_by(week_num) %>%
      summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(time_period = sprintf("W%02d", week_num), group_label = "10-Year Avg") %>%
      select(time_period, week_num, value, group_label)
  }
  
  # Save zone-level data for cache storage (before aggregation)
  average_by_zone <- if ("zone" %in% names(average_data)) average_data else NULL
  ten_year_by_zone <- if ("zone" %in% names(ten_year_avg_data)) ten_year_avg_data else NULL
  
  # Aggregate zone-level data for display (sum across zones in filter)
  if ("zone" %in% names(average_data)) {
    display_5yr <- average_data %>%
      group_by(week_num, time_period, group_label) %>%
      summarize(value = sum(value, na.rm = TRUE), .groups = "drop")
  } else {
    display_5yr <- average_data
  }
  
  if ("zone" %in% names(ten_year_avg_data)) {
    display_10yr <- ten_year_avg_data %>%
      group_by(week_num, time_period, group_label) %>%
      summarize(value = sum(value, na.rm = TRUE), .groups = "drop")
  } else {
    display_10yr <- ten_year_avg_data
  }
  
  # Format current year data — aggregate zones for display
  if (has_zone_col && nrow(current_data) > 0) {
    current_formatted <- current_data %>%
      group_by(week_num) %>%
      summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(time_period = sprintf("W%02d", week_num),
             group_label = as.character(current_year))
  } else {
    current_formatted <- current_data %>%
      mutate(time_period = sprintf("W%02d", week_num),
             group_label = as.character(current_year))
  }
  if (nrow(current_formatted) > 0) {
    current_formatted <- current_formatted %>%
      select(time_period, week_num, value, group_label)
  }
  
  result <- list(
    average = display_5yr,
    ten_year_average = display_10yr,
    current = current_formatted,
    average_by_zone = average_by_zone,
    ten_year_by_zone = ten_year_by_zone
  )
  
  # Store facility-filtered result in Redis for next time
  if (!is.null(facility_filter) && facility_filter != "all" &&
      exists("set_app_cached_redis") && exists("build_cache_key") && exists("CACHE_PREFIX_FAC_HIST")) {
    
    analysis_week <- if (!is.null(analysis_date)) {
      paste0(lubridate::year(analysis_date), "_W", lubridate::week(analysis_date))
    } else {
      paste0(lubridate::year(Sys.Date()), "_W", lubridate::week(Sys.Date()))
    }
    
    fac_hist_id <- paste0(metric, "_", facility_filter)
    cache_key <- build_cache_key(CACHE_PREFIX_FAC_HIST, fac_hist_id,
                                  metric, facility_filter, sort(zone_filter), overview_type, analysis_week)
    set_app_cached_redis(cache_key, result, ttl = TTL_24_HR)
    cat("[fachist] Cached result for", metric, "facility:", facility_filter, "\n")
  }
  
  result
}
