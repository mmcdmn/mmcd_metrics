# Manual check: Facility detail boxes (no testthat)
# Usage: Rscript tests/manual/check_facility_detail_boxes.R [metric_id] [facility_code]

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
})

metric_id <- if (length(commandArgs(trailingOnly = TRUE)) >= 1) commandArgs(trailingOnly = TRUE)[1] else "ground_prehatch"
facility_filter <- if (length(commandArgs(trailingOnly = TRUE)) >= 2) commandArgs(trailingOnly = TRUE)[2] else NULL

project_root <- normalizePath(getwd(), winslash = "/")
if (!dir.exists(file.path(project_root, "apps", "overview"))) {
  project_root <- normalizePath(file.path(getwd(), ".."), winslash = "/")
}
if (!dir.exists(file.path(project_root, "apps", "overview"))) {
  project_root <- normalizePath(file.path(getwd(), "..", ".."), winslash = "/")
}
if (!dir.exists(file.path(project_root, "apps", "overview"))) {
  stop("FAIL: Could not locate apps/overview. Run from repo root.")
}
setwd(file.path(project_root, "apps", "overview"))

source(file.path(project_root, "shared", "db_pool.R"))
source(file.path(project_root, "shared", "db_helpers.R"))
source(file.path(project_root, "apps", "overview", "metric_registry.R"))
source(file.path(project_root, "apps", "overview", "data_functions.R"))
source(file.path(project_root, "apps", "overview", "dynamic_server.R"))

cat("Manual check for metric:", metric_id, "\n")

analysis_date <- Sys.Date()
expiring_days <- 5
zone_filter <- c("1", "2")

# Load facility data once (simulate preloaded current_data)
facility_data <- load_data_by_facility(
  metric = metric_id,
  analysis_date = analysis_date,
  expiring_days = expiring_days,
  zone_filter = zone_filter,
  separate_zones = FALSE,
  facility_filter = facility_filter
)

if (is.null(facility_data) || nrow(facility_data) == 0) {
  stop("FAIL: No facility data returned.")
}

cat("Loaded rows:", nrow(facility_data), "\n")
cat("Columns:", paste(names(facility_data), collapse = ", "), "\n")

# Pick a facility if not provided
if (is.null(facility_filter)) {
  if (!"facility" %in% names(facility_data)) stop("FAIL: facility column not found.")
  facility_filter <- facility_data$facility[1]
}

# Subset detail data (preloaded)
preloaded_detail <- facility_data[
  facility_data$facility == facility_filter | facility_data$display_name == facility_filter,
  ,
  drop = FALSE
]

if (nrow(preloaded_detail) == 0) {
  stop("FAIL: No rows for selected facility.")
}

# Ensure expected columns are present
expected_cols <- c("total", "active", "expiring")
missing_cols <- setdiff(expected_cols, names(preloaded_detail))
if (length(missing_cols) > 0) {
  stop(paste("FAIL: Missing expected columns:", paste(missing_cols, collapse = ", ")))
}

# Generate detail boxes without reloading data
ui <- generate_facility_detail_boxes(
  metric_id = metric_id,
  facility = facility_filter,
  zone_filter = zone_filter,
  analysis_date = analysis_date,
  expiring_days = expiring_days,
  theme = "MMCD",
  detail_data = preloaded_detail
)

if (is.null(ui)) {
  stop("FAIL: Detail box UI is NULL.")
}

cat("PASS: Detail boxes generated for facility:", facility_filter, "\n")
