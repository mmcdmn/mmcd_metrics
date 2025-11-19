# Test script to reproduce the exact errors reported
library(dplyr)
library(lubridate)

setwd("c:/Users/datatech/Documents/mmcd_metrics/apps/ground_prehatch_progress")
source("data_functions.R") 
source("historical_functions_simple.R")

cat("=== TESTING EXACT ERROR SCENARIOS ===\n\n")

# Test 1: Reproduce the zone filter error
cat("1. Testing zone filter issue...\n")
tryCatch({
  result1 <- get_ground_historical_data(
    time_period = "weekly",
    display_metric = "active_sites", 
    zone_filter = c("1", "2"),
    start_year = 2021,
    end_year = 2021
  )
  cat("✓ Zone filter test passed - got", nrow(result1), "rows\n")
  if(nrow(result1) > 0) {
    cat("Columns:", paste(colnames(result1), collapse=", "), "\n")
  }
}, error = function(e) {
  cat("✗ Zone filter ERROR:", e$message, "\n")
})

# Test 2: Reproduce the treated_acres aggregation error  
cat("\n2. Testing aggregation with sample weekly data...\n")
tryCatch({
  # Create weekly active data structure
  sample_data <- data.frame(
    time_period = rep("2021-W09", 3),
    sitecode = c("SITE001", "SITE002", "SITE003"),
    facility = rep("A", 3),
    zone = rep("1", 3), 
    fosarea = rep(123, 3),
    acres = c(5.0, 3.2, 4.1),
    stringsAsFactors = FALSE
  )
  
  result2 <- aggregate_historical_data_by_group(
    sample_data,
    group_by = "foreman",
    time_period = "weekly", 
    display_metric = "active_sites",
    combine_zones = FALSE
  )
  cat("✓ Aggregation test passed - got", nrow(result2), "rows\n")
  print(result2)
}, error = function(e) {
  cat("✗ Aggregation ERROR:", e$message, "\n")
})

# Test 3: Reproduce the inspdate details table error
cat("\n3. Testing details table with weekly data...\n") 
tryCatch({
  sample_data <- data.frame(
    time_period = c("2021-W09", "2021-W10"),
    sitecode = c("SITE001", "SITE002"),
    facility = c("A", "A"),
    zone = c("1", "1"),
    fosarea = c(123, 123), 
    acres = c(5.0, 3.2),
    stringsAsFactors = FALSE
  )
  
  result3 <- create_historical_details_table(sample_data)
  cat("✓ Details table test passed\n")
}, error = function(e) {
  cat("✗ Details table ERROR:", e$message, "\n")
})

cat("\n=== TEST COMPLETE ===\n")