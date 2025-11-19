# Test script for historical functionality

# Load required libraries
library(dplyr)
library(lubridate)

# Source functions
source("data_functions.R")
source("historical_functions_simple.R")
source("../../shared/db_helpers.R")

cat("=== TESTING HISTORICAL FUNCTIONALITY ===\n\n")

# Test 1: Basic data loading
cat("1. Testing historical data loading...\n")
historical_data <- tryCatch({
  get_ground_historical_data(
    time_period = "yearly",
    display_metric = "treatments",
    zone_filter = c("1", "2"),
    start_year = 2021,
    end_year = 2025
  )
}, error = function(e) {
  cat("Error in historical data loading:", e$message, "\n")
  data.frame()
})

if (nrow(historical_data) > 0) {
  cat("✓ Historical data loaded successfully - got", nrow(historical_data), "rows\n")
  cat("Columns:", paste(names(historical_data), collapse = ", "), "\n")
  cat("Sample data:\n")
  print(head(historical_data, 3))
} else {
  cat("✗ No historical data returned\n")
}

# Test 2: Weekly active sites
cat("\n2. Testing weekly active sites...\n")
weekly_data <- tryCatch({
  get_ground_historical_data(
    time_period = "weekly",
    display_metric = "weekly_active_sites",
    zone_filter = c("1", "2"),
    start_year = 2025,
    end_year = 2025
  )
}, error = function(e) {
  cat("Error in weekly data loading:", e$message, "\n")
  data.frame()
})

if (nrow(weekly_data) > 0) {
  cat("✓ Weekly active data loaded successfully - got", nrow(weekly_data), "rows\n")
  cat("Columns:", paste(names(weekly_data), collapse = ", "), "\n")
  cat("Sample data:\n")
  print(head(weekly_data, 3))
} else {
  cat("✗ No weekly data returned\n")
}

# Test 3: Aggregation
cat("\n3. Testing aggregation...\n")
if (nrow(historical_data) > 0) {
  aggregated <- tryCatch({
    aggregate_historical_data_by_group(
      historical_data, 
      "mmcd_all", 
      "yearly", 
      "treatments", 
      FALSE
    )
  }, error = function(e) {
    cat("Error in aggregation:", e$message, "\n")
    data.frame()
  })
  
  if (nrow(aggregated) > 0) {
    cat("✓ Aggregation successful - got", nrow(aggregated), "rows\n")
    print(aggregated)
  } else {
    cat("✗ Aggregation failed\n")
  }
} else {
  cat("Skipping aggregation test - no data\n")
}

cat("\n=== HISTORICAL TEST COMPLETE ===\n")