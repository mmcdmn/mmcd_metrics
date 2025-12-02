# Test historical catch basin data loading

library(dplyr)
library(lubridate)

# Source required files
source("../../shared/db_helpers.R")
source("data_functions.R")
source("historical_functions.R")

cat("Testing historical catch basin data loading...\n\n")

# Test 1: Load historical data for 2023-2024
cat("Test 1: Loading historical data for 2023-2024...\n")
hist_data <- load_historical_cb_data(
  start_year = 2023,
  end_year = 2024,
  facility_filter = NULL,
  zone_filter = c("1", "2"),
  foreman_filter = NULL
)

cat("Rows returned:", nrow(hist_data), "\n")
if (nrow(hist_data) > 0) {
  cat("Columns:", paste(names(hist_data), collapse = ", "), "\n")
  cat("Years in data:", paste(unique(hist_data$treatment_year), collapse = ", "), "\n")
  cat("Facilities in data:", paste(unique(hist_data$facility), collapse = ", "), "\n")
  cat("\nFirst few rows:\n")
  print(head(hist_data, 10))
}

cat("\n\nTest 2: Create yearly historical data...\n")
yearly_data <- create_historical_cb_data(
  start_year = 2023,
  end_year = 2024,
  hist_time_period = "yearly",
  hist_display_metric = "treatments",
  hist_group_by = "facility",
  hist_zone_display = "combined",
  facility_filter = NULL,
  zone_filter = c("1", "2"),
  foreman_filter = NULL
)

cat("Rows returned:", nrow(yearly_data), "\n")
if (nrow(yearly_data) > 0) {
  cat("Columns:", paste(names(yearly_data), collapse = ", "), "\n")
  cat("\nYearly summary:\n")
  print(yearly_data)
}

cat("\n\nTest 3: Create yearly wet CB count...\n")
yearly_cb <- create_historical_cb_data(
  start_year = 2023,
  end_year = 2024,
  hist_time_period = "yearly",
  hist_display_metric = "wet_cb_count",
  hist_group_by = "mmcd_all",
  hist_zone_display = "combined",
  facility_filter = NULL,
  zone_filter = c("1", "2"),
  foreman_filter = NULL
)

cat("Rows returned:", nrow(yearly_cb), "\n")
if (nrow(yearly_cb) > 0) {
  print(yearly_cb)
}

cat("\n\n=== Tests Complete ===\n")
