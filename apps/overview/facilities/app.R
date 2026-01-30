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
source("../ui_helper.R")
source("../dynamic_ui.R")
source("../dynamic_server.R")

# Set application name for AWS RDS monitoring
set_app_name("facilities_overview")

# =============================================================================
# STARTUP OPTIMIZATION: Preload lookup tables into cache
# =============================================================================
message("[facilities_overview] Preloading lookup tables...")
tryCatch({
  get_facility_lookup()
  get_foremen_lookup()
  message("[facilities_overview] Lookup tables preloaded")
}, error = function(e) message("[facilities_overview] Preload warning: ", e$message))

# Load environment variables
load_env_vars()

# =============================================================================
# USER INTERFACE - Built dynamically from metric_registry.R
# =============================================================================

ui <- build_overview_ui(overview_type = "facilities", include_historical = TRUE)

# Add client-side JavaScript to handle URL parameters
ui <- tagAppendChild(ui, tags$script(HTML("
$(document).ready(function() {
  // Parse URL parameters
  var urlParams = new URLSearchParams(window.location.search);
  var zone = urlParams.get('zone');
  var date = urlParams.get('date');
  var expiring = urlParams.get('expiring');
  var theme = urlParams.get('theme');
  
  console.log('URL parameters:', {zone: zone, date: date, expiring: expiring, theme: theme});
  
  // Update zone filter if specified
  if (zone && (zone === '1' || zone === '2')) {
    console.log('Setting zone filter to:', zone);
    setTimeout(function() {
      $('#zone_filter').val(zone);
      $('#zone_filter').trigger('change');
    }, 500); // Small delay to ensure Shiny is ready
  }
  
  // Update date if specified
  if (date) {
    console.log('Setting date to:', date);
    setTimeout(function() {
      $('#custom_today').val(date);
      $('#custom_today').trigger('change');
    }, 500);
  }
  
  // Update expiring days if specified
  if (expiring) {
    console.log('Setting expiring days to:', expiring);
    setTimeout(function() {
      $('#expiring_days').data('ionRangeSlider').update({from: parseInt(expiring)});
    }, 500);
  }
  
  // Update theme if specified
  if (theme) {
    console.log('Setting theme to:', theme);
    setTimeout(function() {
      $('#color_theme').val(theme);
      $('#color_theme').trigger('change');
    }, 500);
  }
});
")))

# =============================================================================
# SERVER LOGIC - Built dynamically from metric_registry.R
# =============================================================================

server <- function(input, output, session) {
  # Handle URL parameters for drill-down from district overview - using delayed execution
  url_params_processed <- FALSE
  
  # Create a delayed observer that runs after the UI is ready
  observe({
    if (!url_params_processed) {
      invalidateLater(2000, session) # Wait 2 seconds for UI to be ready
      
      query <- isolate(parseQueryString(session$clientData$url_search))
      
      if (length(query) > 0) {
        url_params_processed <<- TRUE # Mark as processed to prevent re-execution
        cat("DEBUG: Processing URL parameters after delay:", paste(names(query), "=", unlist(query), collapse=", "), "\n")
        
        if (!is.null(query$zone)) {
          zone_value <- query$zone
          cat("DEBUG: Updating zone filter to:", zone_value, "\n")
          if (zone_value %in% c("1", "2")) {
            updateSelectInput(session, "zone_filter", selected = zone_value)
          }
        }
        
        if (!is.null(query$date)) {
          tryCatch({
            date_value <- as.Date(query$date)
            cat("DEBUG: Updating date to:", as.character(date_value), "\n")
            updateDateInput(session, "custom_today", value = date_value)
          }, error = function(e) {
            cat("Invalid date parameter:", query$date, "\n")
          })
        }
        
        if (!is.null(query$expiring)) {
          tryCatch({
            expiring_value <- as.numeric(query$expiring)
            cat("DEBUG: Updating expiring days to:", expiring_value, "\n")
            updateNumericInput(session, "expiring_days", value = expiring_value)
          }, error = function(e) {
            cat("Invalid expiring parameter:", query$expiring, "\n")
          })
        }
        
        if (!is.null(query$theme)) {
          theme_value <- query$theme
          cat("DEBUG: Updating theme to:", theme_value, "\n")
          valid_themes <- c("MMCD", "IBM", "Wong", "Tol", "Viridis")
          if (theme_value %in% valid_themes) {
            updateSelectInput(session, "color_theme", selected = theme_value)
          }
        }
      }
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
