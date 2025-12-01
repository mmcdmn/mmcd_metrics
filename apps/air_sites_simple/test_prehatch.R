# Test script to verify prehatch field in loc_breeding_sites for air sites
# This tests the query before implementing in the app

# Load required libraries
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
})

# Source shared database helpers and data functions
source("../../shared/db_helpers.R")
source("data_functions.R")

cat("\n=== Testing Prehatch Filter in Air Sites Simple App ===\n\n")

# Test 1: Load data WITHOUT prehatch filter
cat("Test 1: Loading all air sites (no prehatch filter)\n")
all_data <- get_air_sites_data(
  analysis_date = Sys.Date(),
  facility_filter = NULL,
  priority_filter = NULL,
  zone_filter = NULL,
  larvae_threshold = 2,
  prehatch_only = FALSE
)

cat("Total sites loaded:", nrow(all_data), "\n")
if (nrow(all_data) > 0) {
  prehatch_count <- sum(!is.na(all_data$prehatch), na.rm = TRUE)
  cat("Sites with prehatch NOT NULL:", prehatch_count, "\n")
  cat("Percentage with prehatch:", round((prehatch_count / nrow(all_data)) * 100, 2), "%\n")
}

# Test 2: Load data WITH prehatch filter
cat("\nTest 2: Loading ONLY prehatch sites (prehatch_only = TRUE)\n")
prehatch_data <- get_air_sites_data(
  analysis_date = Sys.Date(),
  facility_filter = NULL,
  priority_filter = NULL,
  zone_filter = NULL,
  larvae_threshold = 2,
  prehatch_only = TRUE
)

cat("Total prehatch sites loaded:", nrow(prehatch_data), "\n")
if (nrow(prehatch_data) > 0) {
  sites_without_prehatch <- sum(is.na(prehatch_data$prehatch), na.rm = TRUE)
  cat("Sites WITHOUT prehatch (should be 0):", sites_without_prehatch, "\n")
  
  if (sites_without_prehatch == 0) {
    cat("✓ SUCCESS: All returned sites have prehatch values\n")
  } else {
    cat("✗ FAILURE: Some sites without prehatch were included\n")
  }
  
  # Show sample
  cat("\nSample of prehatch sites:\n")
  print(head(prehatch_data[, c("sitecode", "facility", "prehatch", "priority")], 10))
}

# Test 3: Test with facility filter combined
cat("\nTest 3: Testing prehatch filter combined with facility filter (Sr only)\n")
sr_prehatch_data <- get_air_sites_data(
  analysis_date = Sys.Date(),
  facility_filter = "Sr",
  priority_filter = NULL,
  zone_filter = NULL,
  larvae_threshold = 2,
  prehatch_only = TRUE
)

cat("Sr facility prehatch sites:", nrow(sr_prehatch_data), "\n")
if (nrow(sr_prehatch_data) > 0) {
  sites_without_prehatch <- sum(is.na(sr_prehatch_data$prehatch), na.rm = TRUE)
  wrong_facility <- sum(sr_prehatch_data$facility != "Sr", na.rm = TRUE)
  
  cat("Sites WITHOUT prehatch (should be 0):", sites_without_prehatch, "\n")
  cat("Sites NOT in Sr facility (should be 0):", wrong_facility, "\n")
  
  if (sites_without_prehatch == 0 && wrong_facility == 0) {
    cat("✓ SUCCESS: Combined filters work correctly\n")
  } else {
    cat("✗ FAILURE: Combined filters not working properly\n")
  }
}

cat("\n=== All Prehatch Tests Completed ===\n")
