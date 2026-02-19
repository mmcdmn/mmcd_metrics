# Unified Overview Dashboard
# =============================================================================
# SINGLE APP that handles all overview views based on URL parameters.
# Uses the SAME build_overview_ui/server functions as district/facilities apps.
#
# URL Scheme:
#   /overview/                             - District overview (default)
#   /overview/?view=district               - District overview
#   /overview/?view=facility               - Facility view
#   /overview/?view=facility&zone=1        - Facility view for P1
#   /overview/?metric=drone                - Show only drone metric
#   /overview/?metric=drone,catch_basin    - Show drone and catch basin
#
# This app looks EXACTLY like district_overview or facilities_overview.
# Metric filtering is done PURELY through URL parameters.
# =============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(tidyr)
  library(lubridate)
})

# Source shared database helpers (adjust path since we're in unified/ subfolder)
source("../../../shared/db_helpers.R")
source("../../../shared/stat_box_helpers.R")

# Source overview framework - THE REGISTRY IS THE SINGLE SOURCE OF TRUTH
source("../metric_registry.R")
source("../data_functions.R")
source("../display_functions.R")
source("../historical_functions.R")
source("../ui_helper.R")
source("../dynamic_ui.R")
source("../dynamic_server.R")

# Set application name for AWS RDS monitoring
set_app_name("overview")

# =============================================================================
# STARTUP OPTIMIZATION: Preload lookup tables into cache
# =============================================================================
message("[overview] Preloading lookup tables...")
tryCatch({
  get_facility_lookup()
  get_foremen_lookup()
  message("[overview] Lookup tables preloaded")
}, error = function(e) message("[overview] Preload warning: ", e$message))

# Load environment variables
load_env_vars()

# =============================================================================
# URL PARAMETER PARSING
# =============================================================================

#' Parse URL parameters for the unified app
#' @param query_string The query string from request
#' @return List with all parsed parameters
parse_unified_params <- function(query_string) {
  query <- parseQueryString(query_string)
  
  # Determine view type (default: district)
  view_type <- "district"
  if (!is.null(query$view) && query$view %in% c("district", "facility", "facilities", "fos")) {
    view_type <- if (query$view == "facility") "facilities" else query$view
  }
  
  # Determine metrics filter (default: NULL = all metrics)
  metrics_filter <- NULL
  if (!is.null(query$metric) && query$metric != "" && query$metric != "all") {
    # Split comma-separated metrics
    requested <- strsplit(query$metric, ",")[[1]]
    # Validate against registry
    registry <- get_metric_registry()
    valid_metrics <- names(registry)
    metrics_filter <- requested[requested %in% valid_metrics]
    if (length(metrics_filter) == 0) metrics_filter <- NULL
  }
  
  # Parse zone filter
  zone_filter <- "1,2"  # default
  if (!is.null(query$zone)) {
    if (query$zone == "1") {
      zone_filter <- "1"
    } else if (query$zone == "2") {
      zone_filter <- "2"
    } else if (query$zone == "separate") {
      zone_filter <- "separate"
    }
  }
  
  # Parse facility filter (for FOS drilldown from a specific facility)
  facility_filter <- NULL
  if (!is.null(query$facility) && query$facility != "" && query$facility != "all") {
    facility_filter <- query$facility
  }
  
  # Parse FOS filter (for further drilldown to a specific FOS)
  fos_filter <- NULL
  if (!is.null(query$fos) && query$fos != "" && query$fos != "all") {
    fos_filter <- query$fos
  }
  
  # Parse date
  analysis_date <- Sys.Date()
  if (!is.null(query$date)) {
    tryCatch({
      analysis_date <- as.Date(query$date)
    }, error = function(e) {
      cat("Invalid date parameter:", query$date, "\n")
    })
  }
  
  # Parse expiring days
  expiring_days <- 3  # default
  if (!is.null(query$expiring)) {
    tryCatch({
      expiring_days <- as.numeric(query$expiring)
    }, error = function(e) {
      cat("Invalid expiring parameter:", query$expiring, "\n")
    })
  }
  
  # Parse theme
  color_theme <- "MMCD"  # default
  if (!is.null(query$theme) && query$theme %in% c("MMCD", "IBM", "Wong", "Tol", "Viridis")) {
    color_theme <- query$theme
  }
  
  list(
    view_type = view_type,
    metrics_filter = metrics_filter,
    zone_filter = zone_filter,
    facility_filter = facility_filter,
    fos_filter = fos_filter,
    analysis_date = analysis_date,
    expiring_days = expiring_days,
    color_theme = color_theme
  )
}

# =============================================================================
# USER INTERFACE - Built dynamically from metric_registry.R
# =============================================================================

ui <- function(request) {
  # Parse URL parameters
  params <- parse_unified_params(request$QUERY_STRING)
  
  # Build the standard overview UI (EXACTLY like district_overview)
  # Pass metrics_filter and URL parameters to set initial values
  # Only show historical charts when drilling down to specific metrics
  build_overview_ui(
    overview_type = params$view_type,
    include_historical = !is.null(params$metrics_filter),  # Historical for drill-down views
    metrics_filter = params$metrics_filter,
    initial_zone = params$zone_filter,
    initial_date = params$analysis_date,
    initial_expiring = params$expiring_days,
    initial_theme = params$color_theme,
    facility_filter = params$facility_filter,
    fos_filter = params$fos_filter
  )
}

# =============================================================================
# SERVER LOGIC - Built dynamically from metric_registry.R
# =============================================================================

server <- function(input, output, session) {
  # Parse URL parameters - use isolate() since we only need the initial value
  params <- isolate(parse_unified_params(session$clientData$url_search))
  
  # Build the standard overview server (EXACTLY like district_overview)
  build_overview_server(
    input = input,
    output = output,
    session = session,
    overview_type = params$view_type,
    include_historical = !is.null(params$metrics_filter),  # Historical for drill-down views
    metrics_filter = params$metrics_filter,
    facility_filter = params$facility_filter,
    fos_filter = params$fos_filter
  )
}

# Run the application
shinyApp(ui = ui, server = server)
