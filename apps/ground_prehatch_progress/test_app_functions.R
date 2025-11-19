#!/usr/bin/env Rscript

# Minimal test that replicates the app's exact function calls
cat("=== GROUND PREHATCH FUNCTION TEST ===\n")

# Load the functions first
cat("Loading functions...\n")
setwd("c:/Users/datatech/Documents/mmcd_metrics/apps/ground_prehatch_progress")

tryCatch({
  source("data_functions.R")
  source("historical_functions_simple.R")
  cat("✓ Functions loaded\n")
}, error = function(e) {
  cat("✗ ERROR loading functions:", e$message, "\n")
  quit(save = "no", status = 1)
})

# Test 1: Weekly active sites (this should replicate the error)
cat("\n--- Test 1: Weekly Active Sites ---\n")
tryCatch({
  result <- get_ground_historical_data(
    time_period = "weekly",
    display_metric = "active_sites", 
    zone_filter = c("1", "2"),
    start_year = 2024,
    end_year = 2024
  )
  cat("✓ get_ground_historical_data succeeded, rows:", nrow(result), "\n")
  if (nrow(result) > 0) {
    cat("Columns:", paste(colnames(result), collapse = ", "), "\n")
    cat("Sample data:\n")
    print(head(result, 2))
  }
}, error = function(e) {
  cat("✗ ERROR in get_ground_historical_data:", e$message, "\n")
  cat("Full error details:\n")
  print(e)
})

# Test 2: Aggregation (this should replicate the treated_acres error)
cat("\n--- Test 2: Aggregation ---\n")
tryCatch({
  # Create sample weekly active data that matches what the function should return
  test_data <- data.frame(
    time_period = c("2024-W10", "2024-W10", "2024-W11"),
    sitecode = c("SITE001", "SITE002", "SITE001"), 
    facility = c("A", "A", "A"),
    zone = c("1", "1", "1"),
    fosarea = c(100, 100, 100),
    acres = c(5.0, 3.0, 5.0),
    stringsAsFactors = FALSE
  )
  
  cat("Test data structure:\n")
  str(test_data)
  
  agg_result <- aggregate_historical_data_by_group(
    test_data,
    group_by = "foreman", 
    time_period = "weekly",
    display_metric = "active_sites",
    combine_zones = FALSE
  )
  cat("✓ Aggregation succeeded, rows:", nrow(agg_result), "\n")
  print(agg_result)
}, error = function(e) {
  cat("✗ ERROR in aggregation:", e$message, "\n")
  print(e)
})

# Test 3: Details table (this should replicate the inspdate error) 
cat("\n--- Test 3: Details Table ---\n")
tryCatch({
  test_data <- data.frame(
    time_period = c("2024-W10", "2024-W11"),
    sitecode = c("SITE001", "SITE002"),
    facility = c("A", "A"), 
    zone = c("1", "1"),
    fosarea = c(100, 100),
    acres = c(5.0, 3.0),
    stringsAsFactors = FALSE
  )
  
  details_result <- create_historical_details_table(test_data)
  cat("✓ Details table succeeded\n")
}, error = function(e) {
  cat("✗ ERROR in details table:", e$message, "\n")
  print(e)
})

cat("\n=== TEST COMPLETE ===\n")