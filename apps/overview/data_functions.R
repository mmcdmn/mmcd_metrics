# Overview - UNIFIED Data Functions
# =============================================================================
# ONE function for ALL metrics. All apps now return standardized format:
#   list(sites = df, treatments = df, total_count = int)
# 
# Where sites has: sitecode, facility, zone, is_active, is_expiring
# 
# NO HARDCODED METRIC NAMES - Everything comes from the registry!
# =============================================================================

# Source the registry (must exist before this file is sourced)
# Registry is sourced in app.R before this file

# =============================================================================
# ENVIRONMENT LOADING - Uses registry for app folders
# =============================================================================

# Get apps base path - checks relative paths
get_apps_base_path_df <- function() {
  # When running from apps/overview/district or apps/overview/facilities
  if (file.exists("../../drone/data_functions.R")) return("../..")
  # When running from apps/overview
  if (file.exists("../drone/data_functions.R")) return("..")
  # When running on server
  if (file.exists("/srv/shiny-server/apps/drone/data_functions.R")) return("/srv/shiny-server/apps")
  # Fallback
  return("../..")
}

# Load each app's data_functions into isolated environment
# Uses registry to get app folders dynamically
.load_app_environments <- function() {
  apps_base <- get_apps_base_path_df()
  registry <- get_metric_registry()
  
  app_envs <- list()
  for (metric_id in names(registry)) {
    config <- registry[[metric_id]]
    app_folder <- config$app_folder
    data_file <- file.path(apps_base, app_folder, "data_functions.R")
    
    if (file.exists(data_file)) {
      env <- new.env(parent = globalenv())
      tryCatch({
        source(data_file, local = env, chdir = TRUE)
        app_envs[[metric_id]] <- env
      }, error = function(e) {
        cat("Warning: Could not load", data_file, ":", e$message, "\n")
      })
    } else {
      cat("Warning: Data file not found:", data_file, "\n")
    }
  }
  return(app_envs)
}

# Initialize app environments (lazy loading)
.app_envs <- NULL
get_app_envs <- function() {
  if (is.null(.app_envs)) {
    .app_envs <<- .load_app_environments()
  }
  return(.app_envs)
}

# =============================================================================
# THE UNIFIED FUNCTION - COMPLETELY DYNAMIC
# =============================================================================

#' Load data for ANY metric, aggregated by facility+zone
#' 
#' Returns: facility, zone, total_count, active_count, expiring_count
#' 
#' This ONE function works for ALL metrics dynamically.
#' Uses the registry to get configuration, no hardcoded metric names.
#'
#' @param metric Metric ID from registry
#' @param analysis_date Date for analysis
#' @param expiring_days Days until expiring (defaults to registry value)
#' @param zone_filter Vector of zones to include
#' @export
load_metric_data <- function(metric, 
                             analysis_date = Sys.Date(),
                             expiring_days = NULL,
                             zone_filter = c("1", "2")) {
  
  # Get config from registry
  registry <- get_metric_registry()
  if (!metric %in% names(registry)) {
    stop(paste("Invalid metric:", metric, "- not found in registry"))
  }
  
  config <- registry[[metric]]
  
  # Get expiring_days from registry if not specified
  if (is.null(expiring_days)) {
    expiring_days <- if (!is.null(config$load_params$expiring_days)) {
      config$load_params$expiring_days
    } else {
      7  # default fallback
    }
  }
  
  app_envs <- get_app_envs()
  env <- app_envs[[metric]]
  
  if (is.null(env)) {
    warning(paste("Environment not loaded for metric:", metric))
    return(data.frame(
      facility = character(),
      zone = character(),
      total_count = integer(),
      active_count = integer(),
      expiring_count = integer()
    ))
  }
  
  # Dynamically load raw data - try standard interface first
  raw_data <- tryCatch({
    # All apps should have load_raw_data with at minimum analysis_date
    if ("load_raw_data" %in% names(env)) {
      # Try with common parameters - apps will ignore unknown params
      # Pass empty status_types list to get FULL universe (not filtered by status)
      env$load_raw_data(
        analysis_date = analysis_date,
        expiring_days = expiring_days,
        zone_filter = zone_filter,
        status_types = character(0)
      )
    } else {
      warning(paste("load_raw_data not found for metric:", metric))
      list(sites = data.frame(), treatments = data.frame())
    }
  }, error = function(e) {
    # If the call fails, try with just analysis_date
    tryCatch({
      env$load_raw_data(analysis_date = analysis_date)
    }, error = function(e2) {
      warning(paste("Error loading", metric, ":", e2$message))
      list(sites = data.frame(), treatments = data.frame())
    })
  })
  
  # Apply filters if the app has apply_data_filters function
  if ("apply_data_filters" %in% names(env) && !is.null(raw_data$sites) && nrow(raw_data$sites) > 0) {
    tryCatch({
      raw_data <- env$apply_data_filters(
        data = raw_data,
        zone_filter = zone_filter
      )
    }, error = function(e) {
      # Filter application failed, continue with unfiltered data
      cat("Note: apply_data_filters failed for", metric, "\n")
    })
  }
  
  sites <- raw_data$sites
  if (is.null(sites) || nrow(sites) == 0) {
    return(data.frame(
      facility = character(),
      zone = character(),
      total_count = integer(),
      active_count = integer(),
      expiring_count = integer()
    ))
  }
  
  # Special handling for SUCO - capacity-based instead of active/expiring
  if (metric == "suco") {
    # For SUCOs: 
    # - total = capacity (from config)
    # - active = actual SUCOs completed this week
    # - expiring = 0 (not applicable)
    
    # Get capacity from config
    capacity_total <- config$load_params$capacity_total  # 72
    capacity_per_facility <- config$load_params$capacity_per_facility  # 72/7
    
    # Count SUCOs by facility and zone (each row in sites is one SUCO)
    suco_counts <- sites %>%
      filter(zone %in% zone_filter) %>%
      group_by(facility, zone) %>%
      summarize(
        active_count = n(),  # Count of SUCOs done this week
        .groups = "drop"
      )
    
    # Get all facilities and zones to show capacity even if no SUCOs
    all_facilities <- get_facility_lookup()
    all_combinations <- expand.grid(
      facility = all_facilities$short_name,
      zone = zone_filter,
      stringsAsFactors = FALSE
    )
    
    result <- all_combinations %>%
      left_join(suco_counts, by = c("facility", "zone")) %>%
      mutate(
        total_count = capacity_per_facility,  # Capacity per facility
        active_count = ifelse(is.na(active_count), 0, active_count),  # Actual SUCOs
        # "Above Capacity" = how many SUCOs exceed the capacity line per facility
        expiring_count = pmax(0, active_count - total_count)
      )
    
    return(result)
  }
  
  # Aggregate to facility+zone level
  # Check if pre-aggregated or site-level data
  if (isTRUE(raw_data$pre_aggregated)) {
    # Get metric config for display_as_average check
    config <- registry[[metric]]
    
    # For metrics with display_as_average (like mosquito_monitoring) - DON'T aggregate, use as-is
    # Also preserve trap_count columns for proper weighted averaging
    # For display_raw_value metrics (like vector_index) - preserve extra columns too
    if (isTRUE(config$display_as_average) || isTRUE(config$display_raw_value)) {
      # Select all available columns including extra metric-specific columns
      standard_cols <- c("facility", "zone", "total_count", "active_count", "expiring_count", 
                         "trap_count", "historical_trap_count")
      # Also preserve raw_value_column if defined
      if (!is.null(config$raw_value_column)) {
        standard_cols <- c(standard_cols, config$raw_value_column)
      }
      available_cols <- intersect(standard_cols, names(sites))
      result <- sites %>%
        filter(zone %in% zone_filter) %>%
        select(all_of(available_cols))
    } else {
      # Already has counts - just sum them
      base_result <- sites %>%
        filter(zone %in% zone_filter) %>%
        group_by(facility, zone) %>%
        summarize(
          total_count = sum(total_count, na.rm = TRUE),
          active_count = sum(active_count, na.rm = TRUE),
          expiring_count = sum(expiring_count, na.rm = TRUE),
          .groups = "drop"
        )
    
      # Add acres columns if they exist
      if ("total_acres" %in% names(sites)) {
        acres_data <- sites %>%
          filter(zone %in% zone_filter) %>%
          group_by(facility, zone) %>%
          summarize(
            total_acres = sum(total_acres, na.rm = TRUE),
            active_acres = sum(active_acres, na.rm = TRUE),
            expiring_acres = sum(expiring_acres, na.rm = TRUE),
            .groups = "drop"
          )
        result <- base_result %>%
          left_join(acres_data, by = c("facility", "zone"))
      } else {
        result <- base_result
      }
    }
  } else {
    # Site-level data with is_active/is_expiring - count them
    base_result <- sites %>%
      filter(zone %in% zone_filter) %>%
      group_by(facility, zone) %>%
      summarize(
        total_count = n(),
        active_count = sum(is_active, na.rm = TRUE),
        expiring_count = sum(is_expiring, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Add acres columns - prefer treated_acres from latest treatment over site.acres
    # treated_acres = the actual acres treated (from the LATEST treatment per site)
    # site.acres = the site's defined size (may differ from treatment acres)
    if ("treated_acres" %in% names(sites)) {
      # Use treated_acres from latest treatment (correct for ground_prehatch, drone)
      acres_data <- sites %>%
        filter(zone %in% zone_filter) %>%
        group_by(facility, zone) %>%
        summarize(
          total_acres = sum(acres, na.rm = TRUE),  # Site acres for total
          active_acres = sum(ifelse(is_active, treated_acres, 0), na.rm = TRUE),
          expiring_acres = sum(ifelse(is_expiring, treated_acres, 0), na.rm = TRUE),
          .groups = "drop"
        )
      result <- base_result %>%
        left_join(acres_data, by = c("facility", "zone"))
    } else if ("acres" %in% names(sites)) {
      # Fallback to site.acres for metrics without treated_acres (cattail)
      acres_data <- sites %>%
        filter(zone %in% zone_filter) %>%
        group_by(facility, zone) %>%
        summarize(
          total_acres = sum(acres, na.rm = TRUE),
          active_acres = sum(ifelse(is_active, acres, 0), na.rm = TRUE),
          expiring_acres = sum(ifelse(is_expiring, acres, 0), na.rm = TRUE),
          .groups = "drop"
        )
      result <- base_result %>%
        left_join(acres_data, by = c("facility", "zone"))
    } else {
      result <- base_result
    }
  }
  
  return(result)
}

# =============================================================================
# SHARED AGGREGATION HELPER
# =============================================================================

# Metrics that use acres instead of counts for total/active/expiring
ACRES_METRICS <- c("drone", "ground_prehatch", "cattail_treatments", "air_sites")

#' Aggregate metric data by grouping columns into total/active/expiring
#' Handles all metric-type dispatch: SUCO capacity, raw_value, weighted average, acres vs counts
#' @param data Pre-loaded metric data from load_metric_data
#' @param config Registry config for this metric
#' @param metric Metric ID string
#' @param group_cols Character vector of columns to group by (e.g. c("zone") or c("facility","facility_display","zone"))
#' @param separate_zones Whether zones should be separate rows
#' @return Data frame with total, active, expiring, display_name columns
aggregate_metric_data <- function(data, config, metric, group_cols, separate_zones = TRUE) {
  
  # --- SUCO: capacity-based ---
  if (metric == "suco") {
    capacity_total <- config$load_params$capacity_total  # 72
    if (separate_zones) {
      return(data %>%
        group_by(zone) %>%
        summarize(total = capacity_total / 2, active = sum(active_count, na.rm = TRUE), .groups = "drop") %>%
        mutate(expiring = pmax(0, active - total), display_name = paste0("P", zone)))
    } else {
      return(data %>%
        summarize(total = capacity_total, active = sum(active_count, na.rm = TRUE), .groups = "drop") %>%
        mutate(expiring = pmax(0, active - total), display_name = "MMCD (All)", zone = "1,2"))
    }
  }
  
  # --- display_raw_value (vector_index): district-wide passthrough ---
  if (isTRUE(config$display_raw_value)) {
    raw_col <- config$raw_value_column
    dn <- if (separate_zones) "District" else "District Total"
    zn <- if (separate_zones) "1" else "all"
    result <- data %>%
      summarize(total = sum(total_count, na.rm = TRUE), active = sum(active_count, na.rm = TRUE),
                expiring = sum(expiring_count, na.rm = TRUE), .groups = "drop") %>%
      mutate(zone = zn, display_name = dn)
    if (!is.null(raw_col) && raw_col %in% names(data)) result[[raw_col]] <- max(data[[raw_col]], na.rm = TRUE)
    return(result)
  }
  
  # --- Weighted average metrics (mosquito_monitoring) ---
  if (isTRUE(config$display_as_average) && "trap_count" %in% names(data)) {
    weighted_avg_summarize <- function(df) {
      df %>% summarize(
        total = { d <- sum(historical_trap_count, na.rm = TRUE); if (d > 0) round(sum(total_count, na.rm = TRUE) / d, 1) else 0 },
        active = { d <- sum(trap_count, na.rm = TRUE); if (d > 0) round(sum(active_count, na.rm = TRUE) / d, 1) else 0 },
        expiring = round(mean(expiring_count, na.rm = TRUE), 1),
        .groups = "drop"
      )
    }
    if (separate_zones && "zone" %in% group_cols) {
      grp <- intersect(group_cols, names(data))
      result <- data %>% group_by(across(all_of(grp))) %>% weighted_avg_summarize()
    } else {
      grp <- setdiff(group_cols, c("zone"))
      grp <- intersect(grp, names(data))
      if (length(grp) > 0) {
        result <- data %>% group_by(across(all_of(grp))) %>% weighted_avg_summarize()
      } else {
        result <- weighted_avg_summarize(data) %>% mutate(zone = "all")
      }
    }
    return(result)
  }
  
  # --- Standard metrics: use acres or counts ---
  use_acres <- metric %in% ACRES_METRICS && "total_acres" %in% names(data)
  total_col <- if (use_acres) "total_acres" else "total_count"
  active_col <- if (use_acres) "active_acres" else "active_count"
  expiring_col <- if (use_acres) "expiring_acres" else "expiring_count"
  
  grp <- intersect(group_cols, names(data))
  if (length(grp) > 0) {
    result <- data %>%
      group_by(across(all_of(grp))) %>%
      summarize(total = sum(.data[[total_col]], na.rm = TRUE),
                active = sum(.data[[active_col]], na.rm = TRUE),
                expiring = sum(.data[[expiring_col]], na.rm = TRUE), .groups = "drop")
  } else {
    result <- data %>%
      summarize(total = sum(.data[[total_col]], na.rm = TRUE),
                active = sum(.data[[active_col]], na.rm = TRUE),
                expiring = sum(.data[[expiring_col]], na.rm = TRUE))
  }
  
  result
}

# =============================================================================
# AGGREGATION FUNCTIONS BY GROUP TYPE
# =============================================================================

#' Load ANY metric aggregated by zone (P1/P2)
#' @export
load_data_by_zone <- function(metric, 
                              analysis_date = Sys.Date(),
                              expiring_days = NULL,
                              zone_filter = c("1", "2"),
                              separate_zones = TRUE) {
  
  data <- load_metric_data(metric, analysis_date, expiring_days, zone_filter)
  if (nrow(data) == 0) return(data.frame())
  
  registry <- get_metric_registry()
  config <- registry[[metric]]
  
  result <- aggregate_metric_data(data, config, metric,
                                  group_cols = c("zone"),
                                  separate_zones = separate_zones)
  
  # Add display_name if not already set by aggregate_metric_data
  if (!"display_name" %in% names(result)) {
    if (separate_zones && "zone" %in% names(result)) {
      result <- result %>% mutate(display_name = paste0("P", zone)) %>% arrange(zone)
    } else {
      result <- result %>% mutate(zone = "all", display_name = "District Total")
    }
  }
  
  return(result)
}

#' Load ANY metric aggregated by facility
#' @param metric Metric ID from registry
#' @param analysis_date Date for analysis
#' @param expiring_days Days until expiring
#' @param zone_filter Vector of zones to include
#' @param separate_zones Whether to show P1/P2 separately
#' @param facility_filter Optional: filter to specific facility (NULL = all)
#' @export
load_data_by_facility <- function(metric,
                                  analysis_date = Sys.Date(),
                                  expiring_days = NULL,
                                  zone_filter = c("1", "2"),
                                  separate_zones = FALSE,
                                  facility_filter = NULL) {
  
  data <- load_metric_data(metric, analysis_date, expiring_days, zone_filter)
  if (nrow(data) == 0) return(data.frame())
  
  data <- map_facility_names(data)
  
  # Apply facility filter if specified
  if (!is.null(facility_filter) && facility_filter != "all") {
    data <- data %>%
      filter(facility == facility_filter | facility_display == facility_filter)
  }
  
  registry <- get_metric_registry()
  config <- registry[[metric]]
  
  # Determine grouping columns
  grp <- if (separate_zones && length(zone_filter) == 2) {
    c("facility", "facility_display", "zone")
  } else {
    c("facility", "facility_display")
  }
  
  result <- aggregate_metric_data(data, config, metric, group_cols = grp, separate_zones = separate_zones)
  
  # Add display_name from facility_display
  if (!"display_name" %in% names(result)) {
    if ("facility_display" %in% names(result) && "zone" %in% names(result) && separate_zones) {
      result <- result %>% mutate(display_name = paste0(facility_display, " (P", zone, ")")) %>% select(-facility_display)
    } else if ("facility_display" %in% names(result)) {
      result <- result %>% mutate(display_name = facility_display) %>% select(-facility_display)
    }
  } else if ("facility_display" %in% names(result)) {
    result <- result %>% select(-facility_display)
  }
  
  result <- order_facilities(result, separate_zones)
  return(result)
}

#' Load ANY metric aggregated by FOS (placeholder for future)
#' @export
load_data_by_fos <- function(metric,
                             analysis_date = Sys.Date(),
                             expiring_days = NULL,
                             zone_filter = c("1", "2"),
                             separate_zones = FALSE) {
  # TODO: Implement FOS aggregation when FOS overview is needed
  # For now, return empty data frame with correct structure
  data.frame(
    fos = character(),
    display_name = character(),
    total = integer(),
    active = integer(),
    expiring = integer()
  )
}

# =============================================================================
# FACILITY MAPPING AND ORDERING
# =============================================================================

# NOTE: map_facility_names is now provided by shared/db_helpers.R
# Do NOT define it here as it will override the shared version!

#' Get the standard facility order
get_facility_order <- function() {
  facilities <- tryCatch({
    get_facility_lookup()
  }, error = function(e) {
    data.frame(full_name = c("East", "North", "South Jordan", "South Reserve", "West Metro"))
  })
  
  if (nrow(facilities) == 0) {
    return(c("East", "North", "South Jordan", "South Reserve", "West Metro"))
  }
  return(facilities$full_name)
}

#' Order facilities in standard order
order_facilities <- function(data, separate_zones = FALSE) {
  if (!"display_name" %in% names(data)) return(data)
  
  facility_order <- get_facility_order()
  
  if (separate_zones) {
    zone_levels <- unlist(lapply(facility_order, function(f) {
      c(paste0(f, " (P1)"), paste0(f, " (P2)"))
    }))
    data <- data %>% mutate(display_name = factor(display_name, levels = zone_levels))
  } else {
    data <- data %>% mutate(display_name = factor(display_name, levels = facility_order))
  }
  
  data %>% arrange(display_name)
}

# =============================================================================
# GENERIC LOADER FOR ALL METRICS
# =============================================================================

#' Load all metrics for an overview dashboard
#' 
#' @param metrics Vector of metric IDs to load
#' @param group_by Aggregation type: "zone", "facility", or "fos"
#' @param analysis_date Date for analysis
#' @param expiring_days Days until expiring
#' @param zone_filter Vector of zones to include
#' @param separate_zones Whether to show zones separately
#' @return Named list of data frames, one per metric
#' @export
load_all_metrics <- function(metrics = NULL,
                             group_by = "zone",
                             analysis_date = Sys.Date(),
                             expiring_days = 7,
                             zone_filter = c("1", "2"),
                             separate_zones = TRUE) {
  
  if (is.null(metrics)) {
    metrics <- VALID_METRICS
  }
  
  # Choose the right loader function
  loader <- switch(group_by,
    "zone" = load_data_by_zone,
    "facility" = load_data_by_facility,
    "fos" = load_data_by_fos,
    load_data_by_zone  # default
  )
  
  results <- list()
  for (metric in metrics) {
    results[[metric]] <- tryCatch({
      loader(
        metric = metric,
        analysis_date = analysis_date,
        expiring_days = expiring_days,
        zone_filter = zone_filter,
        separate_zones = separate_zones
      )
    }, error = function(e) {
      cat("ERROR loading", metric, ":", e$message, "\n")
      data.frame()
    })
  }
  
  return(results)
}

# =============================================================================
# STATS CALCULATION
# =============================================================================

#' Calculate stats for a single metric (for stat boxes)
#' Uses ceiling() to round percentages UP with no decimal places
#' 
#' @param data Data frame with total, active, expiring columns
#' @param metric_id Optional metric ID for special handling
#' @param metric_config Optional registry config (avoids lookup)
#' @return List with total, active, pct (ceiling rounded, no decimals)
#' @export
calculate_metric_stats <- function(data, metric_id = NULL, metric_config = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(total = 0, active = 0, pct = 0))
  }
  
  total <- sum(data$total, na.rm = TRUE)
  active <- sum(data$active, na.rm = TRUE)
  
  # Get config from registry if not provided
  if (!is.null(metric_id) && is.null(metric_config)) {
    registry <- get_metric_registry()
    metric_config <- registry[[metric_id]]
  }
  
  # For metrics with display_as_average, show percentage: current vs avg
  if (isTRUE(metric_config$display_as_average)) {
    pct <- if (total > 0) round(100 * active / total, 1) else 0  # Show percentage
  } else {
    # Use ceiling() to round UP with no decimal places for percentages
    pct <- ceiling(100 * active / max(1, total))
  }
  
  list(
    total = total,
    active = active,
    pct = pct
  )
}
