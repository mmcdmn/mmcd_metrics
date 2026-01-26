# District Overview - UNIFIED Data Functions
# =============================================================================
# ONE function for ALL metrics. All apps now return standardized format:
#   list(sites = df, treatments = df, total_count = int)
# 
# Where sites has: sitecode, facility, zone, is_active, is_expiring
# (Exception: catch_basin is pre-aggregated for performance, has pre_aggregated=TRUE)
# =============================================================================

# =============================================================================
# CONFIGURATION
# =============================================================================

METRIC_DEFAULTS <- list(
  catch_basin = list(expiring_days = 7),
  drone = list(expiring_days = 7),
  ground_prehatch = list(expiring_days = 7),
  structure = list(expiring_days = 7)
)

VALID_METRICS <- c("catch_basin", "drone", "ground_prehatch", "structure")

METRIC_TO_APP <- list(
  catch_basin = "catch_basin_status",
  drone = "drone",
  ground_prehatch = "ground_prehatch_progress",
  structure = "struct_trt"
)

# =============================================================================
# ENVIRONMENT LOADING
# =============================================================================

get_apps_base_path <- function() {
  if (file.exists("../catch_basin_status/data_functions.R")) return("..")
  if (file.exists("/srv/shiny-server/apps/catch_basin_status/data_functions.R")) return("/srv/shiny-server/apps")
  return("..")
}

apps_base <- get_apps_base_path()

# Load each app into isolated environment
app_envs <- list()
for (metric in names(METRIC_TO_APP)) {
  app_folder <- METRIC_TO_APP[[metric]]
  env <- new.env(parent = globalenv())
  source(file.path(apps_base, app_folder, "data_functions.R"), local = env)
  app_envs[[metric]] <- env
}

# =============================================================================
# THE UNIFIED FUNCTION
# =============================================================================

#' Load data for ANY metric, aggregated by facility+zone
#' 
#' Returns: facility, zone, total_count, active_count, expiring_count
#' 
#' This ONE function replaces all the separate load_*_by_zone functions.
#' All apps now return sites with is_active/is_expiring, so we just aggregate.
#'
#' @param metric One of: "catch_basin", "drone", "ground_prehatch", "structure"
#' @param analysis_date Date for analysis
#' @param expiring_days Days until expiring
#' @param zone_filter Vector of zones to include
load_metric_data <- function(metric, 
                             analysis_date = Sys.Date(),
                             expiring_days = NULL,
                             zone_filter = c("1", "2")) {
  
  if (!metric %in% VALID_METRICS) {
    stop(paste("Invalid metric:", metric))
  }
  
  if (is.null(expiring_days)) {
    expiring_days <- METRIC_DEFAULTS[[metric]]$expiring_days
  }
  
  env <- app_envs[[metric]]
  
  # Load data - each app has load_raw_data with slightly different params
  raw_data <- switch(metric,
    "catch_basin" = env$load_raw_data(
      analysis_date = analysis_date,
      expiring_days = expiring_days,
      zone_filter = zone_filter
    ),
    "drone" = env$load_raw_data(
      drone_types = c("Y", "M", "C"),
      analysis_date = analysis_date
    ),
    "ground_prehatch" = env$load_raw_data(
      analysis_date = analysis_date
    ),
    "structure" = env$load_raw_data(
      analysis_date = analysis_date,
      expiring_days = expiring_days,
      zone_filter = zone_filter
    )
  )
  
  # Apply filters for apps that need them
  if (metric == "drone") {
    raw_data <- env$apply_data_filters(
      data = raw_data,
      zone_filter = zone_filter,
      prehatch_only = TRUE
    )
  } else if (metric == "ground_prehatch") {
    raw_data <- env$apply_data_filters(
      data = raw_data,
      zone_filter = zone_filter
    )
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
  
  # Aggregate to facility+zone level
  # Check if pre-aggregated (catch_basin) or site-level (others)
  if (isTRUE(raw_data$pre_aggregated)) {
    # Already has counts - just sum them
    result <- sites %>%
      filter(zone %in% zone_filter) %>%
      group_by(facility, zone) %>%
      summarize(
        total_count = sum(total_count, na.rm = TRUE),
        active_count = sum(active_count, na.rm = TRUE),
        expiring_count = sum(expiring_count, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    # Site-level data with is_active/is_expiring - count them
    result <- sites %>%
      filter(zone %in% zone_filter) %>%
      group_by(facility, zone) %>%
      summarize(
        total_count = n(),
        active_count = sum(is_active, na.rm = TRUE),
        expiring_count = sum(is_expiring, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  return(result)
}

# =============================================================================
# AGGREGATION FUNCTIONS
# =============================================================================

#' Load ANY metric aggregated by zone (P1/P2)
load_data_by_zone <- function(metric, 
                              analysis_date = Sys.Date(),
                              expiring_days = NULL,
                              zone_filter = c("1", "2"),
                              separate_zones = TRUE) {
  
  data <- load_metric_data(metric, analysis_date, expiring_days, zone_filter)
  if (nrow(data) == 0) return(data.frame())
  
  if (separate_zones) {
    result <- data %>%
      group_by(zone) %>%
      summarize(
        total = sum(total_count, na.rm = TRUE),
        active = sum(active_count, na.rm = TRUE),
        expiring = sum(expiring_count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(display_name = paste0("P", zone)) %>%
      arrange(zone)
  } else {
    result <- data %>%
      summarize(
        total = sum(total_count, na.rm = TRUE),
        active = sum(active_count, na.rm = TRUE),
        expiring = sum(expiring_count, na.rm = TRUE)
      ) %>%
      mutate(zone = "all", display_name = "District Total")
  }
  
  return(result)
}

#' Load ANY metric aggregated by facility
load_data_by_facility <- function(metric,
                                  analysis_date = Sys.Date(),
                                  expiring_days = NULL,
                                  zone_filter = c("1", "2"),
                                  separate_zones = FALSE) {
  
  data <- load_metric_data(metric, analysis_date, expiring_days, zone_filter)
  if (nrow(data) == 0) return(data.frame())
  
  data <- map_facility_names(data)
  
  if (separate_zones && length(zone_filter) == 2) {
    result <- data %>%
      group_by(facility, facility_display, zone) %>%
      summarize(
        total = sum(total_count, na.rm = TRUE),
        active = sum(active_count, na.rm = TRUE),
        expiring = sum(expiring_count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(display_name = paste0(facility_display, " (P", zone, ")")) %>%
      select(-facility_display)
  } else {
    result <- data %>%
      group_by(facility, facility_display) %>%
      summarize(
        total = sum(total_count, na.rm = TRUE),
        active = sum(active_count, na.rm = TRUE),
        expiring = sum(expiring_count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(display_name = facility_display) %>%
      select(-facility_display)
  }
  
  result <- order_facilities(result, separate_zones)
  return(result)
}

# =============================================================================
# FACILITY ORDERING
# =============================================================================

get_facility_order <- function() {
  facilities <- get_facility_lookup()
  if (nrow(facilities) == 0) {
    return(c("East", "North", "South Jordan", "South Reserve", "West Metro"))
  }
  return(facilities$full_name)
}

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
