# URGENT TEST - Historical data not loading after UI changes
# Testing if historical data is actually being retrieved

library(dplyr)
library(lubridate)

setwd("c:/Users/datatech/Documents/mmcd_metrics/apps/ground_prehatch_progress")
source("../../shared/db_helpers.R")
source("data_functions.R")
source("historical_functions.R")

cat("========================================\n")
cat("HISTORICAL DATA TEST (Post-UI Changes)\n")
cat("========================================\n\n")

# Test 1: Basic data loading
cat("TEST 1: Load raw data\n")
cat("----------------------\n")
raw_data <- load_raw_data(
  analysis_date = as.Date("2025-01-01"),
  include_archive = TRUE,
  start_year = 2024,
  end_year = 2025
)

cat("Ground sites loaded:", nrow(raw_data$ground_sites), "\n")
cat("Ground treatments loaded:", nrow(raw_data$ground_treatments), "\n")

if (nrow(raw_data$ground_treatments) > 0) {
  cat("Treatment date range:", 
      min(raw_data$ground_treatments$inspdate), "to",
      max(raw_data$ground_treatments$inspdate), "\n")
  cat("Facilities in data:", paste(unique(raw_data$ground_treatments$facility), collapse = ", "), "\n")
}

cat("\n\nTEST 2: Simple scenario (All facilities, All FOS, Combined)\n")
cat("-------------------------------------------------------------\n")
result1 <- create_historical_data(
  start_year = 2024,
  end_year = 2025,
  hist_time_period = "yearly",
  hist_display_metric = "sites",
  hist_group_by = "mmcd_all",
  hist_zone_display = "combined",
  facility_filter = "all",
  zone_filter = c("1", "2"),
  foreman_filter = "all"
)

cat("Rows returned:", nrow(result1), "\n")
if (nrow(result1) > 0) {
  cat("SUCCESS - Data returned!\n")
  print(result1)
} else {
  cat("FAILURE - No data returned!\n")
}

cat("\n\nTEST 3: Group by Facility (Combined zones)\n")
cat("-------------------------------------------\n")
result2 <- create_historical_data(
  start_year = 2024,
  end_year = 2025,
  hist_time_period = "yearly",
  hist_display_metric = "sites",
  hist_group_by = "facility",
  hist_zone_display = "combined",
  facility_filter = "all",
  zone_filter = c("1", "2"),
  foreman_filter = "all"
)

cat("Rows returned:", nrow(result2), "\n")
if (nrow(result2) > 0) {
  cat("SUCCESS - Data returned!\n")
  print(head(result2, 10))
} else {
  cat("FAILURE - No data returned!\n")
}

cat("\n\nTEST 4: Group by FOS (Combined zones)\n")
cat("--------------------------------------\n")
result3 <- create_historical_data(
  start_year = 2024,
  end_year = 2025,
  hist_time_period = "yearly",
  hist_display_metric = "sites",
  hist_group_by = "foreman",
  hist_zone_display = "combined",
  facility_filter = "all",
  zone_filter = c("1", "2"),
  foreman_filter = "all"
)

cat("Rows returned:", nrow(result3), "\n")
if (nrow(result3) > 0) {
  cat("SUCCESS - Data returned!\n")
  print(head(result3, 10))
} else {
  cat("FAILURE - No data returned!\n")
}

cat("\n\nTEST 5: Single Facility + FOS grouping (Combined zones)\n")
cat("--------------------------------------------------------\n")
result4 <- create_historical_data(
  start_year = 2024,
  end_year = 2025,
  hist_time_period = "yearly",
  hist_display_metric = "sites",
  hist_group_by = "foreman",
  hist_zone_display = "combined",
  facility_filter = "E",
  zone_filter = c("1", "2"),
  foreman_filter = "all"
)

cat("Rows returned:", nrow(result4), "\n")
if (nrow(result4) > 0) {
  cat("SUCCESS - Data returned!\n")
  print(result4)
} else {
  cat("FAILURE - No data returned!\n")
}

cat("\n========================================\n")
cat("TEST COMPLETE\n")
cat("========================================\n")
