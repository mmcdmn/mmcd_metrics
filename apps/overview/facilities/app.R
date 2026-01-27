# Facilities Overview Dashboard
# =============================================================================
# Shows current progress from multiple apps in a single view, grouped by FACILITY
# Displays: All metrics defined in metric_registry.R (no hardcoding here!)
# Drill-down from District Overview - receives zone filter from URL parameter
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

# Source shared database helpers
source("../../../shared/db_helpers.R")
source("../../../shared/stat_box_helpers.R")

# Source shared overview functions - THE REGISTRY IS THE SINGLE SOURCE OF TRUTH
source("../metric_registry.R")
source("../data_functions.R")
source("../display_functions.R")
source("../historical_functions.R")
source("../dynamic_ui.R")
source("../dynamic_server.R")

# Set application name for AWS RDS monitoring
set_app_name("facilities_overview")

# Load environment variables
load_env_vars()

# =============================================================================
# USER INTERFACE - Built dynamically from metric_registry.R
# =============================================================================

ui <- build_overview_ui(overview_type = "facilities", include_historical = TRUE)

# =============================================================================
# SERVER LOGIC - Built dynamically from metric_registry.R
# =============================================================================

server <- function(input, output, session) {
  # Handle URL parameters for drill-down from district overview
  # (This is the only overview-specific code needed)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$zone)) {
      zone_value <- query$zone
      if (zone_value %in% c("1", "2")) {
        updateSelectInput(session, "zone_filter", selected = zone_value)
      }
    }
    
    if (!is.null(query$date)) {
      tryCatch({
        date_value <- as.Date(query$date)
        updateDateInput(session, "custom_today", value = date_value)
      }, error = function(e) {
        cat("Invalid date parameter:", query$date, "\n")
      })
    }
  })
  
  # All chart outputs and data loading are set up dynamically
  # based on the metrics defined in metric_registry.R
  build_overview_server(
    input = input, 
    output = output, 
    session = session,
    overview_type = "facilities",
    include_historical = TRUE
  )
}

# Run the application
shinyApp(ui = ui, server = server)
