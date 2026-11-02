# Inspections App - Data Functions
# =============================================================================
# Entry point sourced by the Inspections app, the Overview registry
# (apps/overview/data_functions.R), the API (api/routes/inspections.R) and the
# embed producers (shared/embed_producers.R). All of them source this file with
# chdir = TRUE into a private environment, so sibling files are sourced with
# local = TRUE (a plain source() would leak into the global environment).
#
#   coverage_functions.R  the 51% rule: per-site classification + summaries
#                         (used by the web app and the Overview tile)
#   legacy_functions.R    pre-rewrite analyses still called by embeds/API/LLM
# =============================================================================

library(dplyr)
library(DBI)
library(RPostgres)
library(lubridate)

# Source shared helpers when loaded outside the app (overview registry)
if (!exists("get_db_connection", mode = "function")) {
  source("../../shared/db_helpers.R")
}

source("coverage_functions.R", local = TRUE)
source("legacy_functions.R", local = TRUE)

# =============================================================================
# LOAD_RAW_DATA - dispatcher for external callers
# =============================================================================
# The web app does NOT call this; it uses load_prehatch_site_coverage().
# Keep the formals: the Overview passes named args and checks formals() for
# facility_filter, and the embed producers call it with the filter arguments.

#' @param status_types character(0) is the Overview registry's signal for
#'   "give me facility/FOS/zone counts" - answered from the coverage rule.
#'   Anything else gets the legacy per-inspection data frame.
#' @return Overview call: list(sites, treatments, pre_aggregated = TRUE).
#'   Otherwise a plain data frame (embed producers check is.data.frame()).
load_raw_data <- function(analysis_date = NULL,
                          facility_filter = NULL,
                          fosarea_filter = NULL,
                          zone_filter = NULL,
                          priority_filter = NULL,
                          drone_filter = "all",
                          spring_only = FALSE,
                          prehatch_only = FALSE,
                          expiring_days = NULL,
                          status_types = NULL,
                          start_year = NULL,
                          end_year = NULL,
                          ...) {

  if (!is.null(status_types) && is.character(status_types) && length(status_types) == 0) {
    return(get_coverage_overview_counts(analysis_date))
  }

  load_legacy_inspection_data(
    analysis_date = analysis_date,
    facility_filter = facility_filter,
    fosarea_filter = fosarea_filter,
    zone_filter = zone_filter,
    priority_filter = priority_filter,
    drone_filter = drone_filter,
    spring_only = spring_only,
    prehatch_only = prehatch_only,
    start_year = start_year,
    end_year = end_year,
    ...
  )
}
