# District Overview Dashboard
# =============================================================================
# Top-level dashboard showing ALL MMCD aggregated by zone (P1 vs P2)
# Click on a zone bar to drill-down to Facilities Overview
# Displays: All metrics defined in metric_registry.R (no hardcoding here!)
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
set_app_name("district_overview")

# Load environment variables
load_env_vars()

# =============================================================================
# USER INTERFACE - Built dynamically from metric_registry.R
# =============================================================================

ui <- build_overview_ui(overview_type = "district", include_historical = TRUE)

# =============================================================================
# SERVER LOGIC - Built dynamically from metric_registry.R
# =============================================================================

server <- function(input, output, session) {
  # All chart outputs and data loading are set up dynamically
  # based on the metrics defined in metric_registry.R
  build_overview_server(
    input = input, 
    output = output, 
    session = session,
    overview_type = "district"
  )
}

# Run the application
shinyApp(ui = ui, server = server)
