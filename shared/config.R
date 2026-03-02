# =============================================================================
# MMCD METRICS - CENTRALIZED CONFIGURATION LOADER
# =============================================================================
# Loads and caches the YAML configuration from config/app_config.yaml.
# All other modules should use get_app_config() to access settings.
#
# USAGE:
#   source("../../shared/config.R")      # or appropriate relative path
#   cfg <- get_app_config()
#   cfg$cache$ttl$db_queries             # => 120
#   cfg$thresholds$colors$good           # => "#16a34a"
#   get_config_threshold("fixed_pct", "air_sites")  # => list(good=85, warning=60, direction="higher_is_better")
# =============================================================================

# Internal cache for the parsed config (avoids re-reading YAML on every call)
.config_env <- new.env(parent = emptyenv())

#' Find the config file from any working directory
#' @return Absolute path to app_config.yaml, or NULL
find_config_path <- function() {
  candidates <- c(
    # From apps/overview, apps/overview/district, apps/overview/facilities, apps/test-app
    "../../config/app_config.yaml",
    "../config/app_config.yaml",
    # From workspace root
    "config/app_config.yaml",
    # Docker container paths
    "/srv/shiny-server/config/app_config.yaml",
    # From shared/ itself
    "../config/app_config.yaml"
  )
  for (path in candidates) {
    if (file.exists(path)) return(normalizePath(path))
  }
  NULL
}

#' Load and return the application configuration
#' Reads config/app_config.yaml once, caches in memory for the R session.
#' Falls back to hardcoded defaults if the file is missing.
#'
#' @param force_reload If TRUE, re-read from disk (default FALSE)
#' @return Named list of configuration values
#' @export
get_app_config <- function(force_reload = FALSE) {
  if (!force_reload && exists("config", envir = .config_env)) {
    return(get("config", envir = .config_env))
  }

  config_path <- find_config_path()

  if (!is.null(config_path) && requireNamespace("yaml", quietly = TRUE)) {
    tryCatch({
      cfg <- yaml::read_yaml(config_path)
      assign("config", cfg, envir = .config_env)
      assign("config_path", config_path, envir = .config_env)
      cat("[config] Loaded from", config_path, "\n")
      return(cfg)
    }, error = function(e) {
      warning("[config] Failed to parse ", config_path, ": ", e$message,
              " — using defaults")
    })
  } else if (is.null(config_path)) {
    message("[config] app_config.yaml not found — using hardcoded defaults")
  } else {
    message("[config] yaml package not available — using hardcoded defaults")
  }

  # Hardcoded defaults (mirrors the YAML structure exactly)
  cfg <- list(
    cache = list(
      ttl = list(
        historical_averages = 1209600L,
        lookup_tables = 1209600L,
        fos_drilldown = 604800L,
        color_mappings = 604800L,
        facility_historical = 86400L,
        general = 300L,
        db_queries = 120L,
        charts = 120L,
        stat_boxes = 120L
      )
    ),
    thresholds = list(
      colors = list(good = "#16a34a", warning = "#eab308", alert = "#dc2626"),
      historical = list(
        default = list(direction = "higher_is_better", good = 0.9, warning = 0.8),
        mosquito_monitoring = list(direction = "lower_is_better", good = 1.1, warning = 1.2)
      ),
      fixed_pct = list(
        air_sites = list(direction = "higher_is_better", good = 85, warning = 60),
        prehatch_coverage = list(direction = "higher_is_better", good = 85, warning = 60),
        vector_index = list(direction = "lower_is_better", good = 30, warning = 60)
      ),
      pct_of_average = list(
        mosquito_monitoring = list(direction = "lower_is_better", good = 110, warning = 130)
      ),
      capacity = list(
        suco = list(direction = "lower_is_better", at_capacity = 72, near_capacity = 60)
      )
    ),
    wiki_links = list(
      catch_basin = "", drone = "", ground_prehatch = "", air_sites = "",
      structure = "", vector_index = "", cattail_treatments = "",
      mosquito_monitoring = "", suco = "", cattail_inspections = "",
      prehatch_coverage = ""
    ),
    display = list(
      default_zone_filter = "P1 and P2 SEPARATE",
      expiring_days_default = 3,
      historical_year_range = 5
    ),
    runtime = list(
      route_ttl_seconds = 600,
      max_workers = 3
    )
  )
  assign("config", cfg, envir = .config_env)
  assign("config_path", "(defaults)", envir = .config_env)
  cfg
}

#' Get the file path the config was loaded from
#' @return Character string path or "(defaults)" if using fallbacks
#' @export
get_config_source <- function() {
  get_app_config()  # ensure loaded
  if (exists("config_path", envir = .config_env)) {
    get("config_path", envir = .config_env)
  } else {
    "(unknown)"
  }
}

# =============================================================================
# CONVENIENCE ACCESSORS
# =============================================================================

#' Get a cache TTL value by name
#' @param name One of: historical_averages, lookup_tables, fos_drilldown,
#'   color_mappings, facility_historical, general, db_queries, charts, stat_boxes
#' @return Integer TTL in seconds
#' @export
get_cache_ttl <- function(name) {
  cfg <- get_app_config()
  val <- cfg$cache$ttl[[name]]
  if (is.null(val)) {
    warning("[config] Unknown cache TTL name: ", name, " — returning 300")
    return(300L)
  }
  as.integer(val)
}

#' Get status indicator colors from config
#' @return Named list with good, warning, alert hex colors
#' @export
get_status_indicator_colors <- function() {
  cfg <- get_app_config()
  cfg$thresholds$colors
}

#' Get threshold config for a specific color mode and metric
#' @param color_mode One of: "historical", "fixed_pct", "pct_of_average", "capacity"
#' @param metric_id The metric ID (e.g., "air_sites", "suco")
#' @return Named list with direction, good, warning (and potentially other fields)
#' @export
get_config_threshold <- function(color_mode, metric_id) {
  cfg <- get_app_config()
  section <- cfg$thresholds[[color_mode]]
  if (is.null(section)) return(NULL)

  # Try metric-specific first, then fall back to "default" (for historical mode)
  thresh <- section[[metric_id]]
  if (is.null(thresh) && !is.null(section$default)) {
    thresh <- section$default
  }
  thresh
}

#' Get the wiki link URL for a metric
#' @param metric_id The metric ID
#' @return Character URL string (empty string if not configured)
#' @export
get_wiki_link <- function(metric_id) {
  cfg <- get_app_config()
  link <- cfg$wiki_links[[metric_id]]
  if (is.null(link)) "" else as.character(link)
}

#' Get a display setting
#' @param name Setting name (default_zone_filter, expiring_days_default, historical_year_range)
#' @return The setting value
#' @export
get_display_setting <- function(name) {
  cfg <- get_app_config()
  cfg$display[[name]]
}

#' Pretty-print the current config for diagnostics
#' @export
print_config_summary <- function() {
  cfg <- get_app_config()
  source_path <- get_config_source()

  cat("=== MMCD App Configuration ===\n")
  cat("Source:", source_path, "\n\n")

  cat("Cache TTLs:\n")
  for (nm in names(cfg$cache$ttl)) {
    val <- cfg$cache$ttl[[nm]]
    human <- if (val >= 86400) paste0(val / 86400, " days")
             else if (val >= 3600) paste0(val / 3600, " hours")
             else if (val >= 60) paste0(val / 60, " min")
             else paste0(val, " sec")
    cat(sprintf("  %-25s %8d sec  (%s)\n", nm, val, human))
  }

  cat("\nThreshold Colors:\n")
  for (nm in names(cfg$thresholds$colors)) {
    cat(sprintf("  %-10s %s\n", nm, cfg$thresholds$colors[[nm]]))
  }

  cat("\nWiki Links:\n")
  for (nm in names(cfg$wiki_links)) {
    link <- cfg$wiki_links[[nm]]
    status <- if (nchar(link) > 0) link else "(not set)"
    cat(sprintf("  %-25s %s\n", nm, status))
  }

  cat("\nDisplay Settings:\n")
  for (nm in names(cfg$display)) {
    cat(sprintf("  %-25s %s\n", nm, cfg$display[[nm]]))
  }

  cat("\nRuntime Settings:\n")
  for (nm in names(cfg$runtime)) {
    cat(sprintf("  %-25s %s\n", nm, cfg$runtime[[nm]]))
  }
}
