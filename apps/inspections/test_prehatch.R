# Test script for prehatch_only functionality in inspections app
# This tests that the prehatch filter works correctly

library(dplyr)

# Set working directory
setwd("c:/Users/datatech/Documents/mmcd_metrics/apps/inspections")

# Source required files
source("../../shared/db_helpers.R")
source("data_functions.R")

cat("\n========================================\n")
cat("Testing Prehatch Filtering Functionality\n")
cat("========================================\n\n")

# Test 1: Get data WITHOUT prehatch filter
cat("Test 1: Loading data WITHOUT prehatch filter...\n")
data_all <- get_all_inspection_data(
  facility_filter = NULL,
  fosarea_filter = NULL,
  zone_filter = NULL,
  priority_filter = NULL,
  drone_filter = "include_drone",
  spring_only = FALSE,
  prehatch_only = FALSE
)

if (nrow(data_all) > 0) {
  total_sites_all <- data_all %>% distinct(sitecode) %>% nrow()
  sites_with_prehatch <- data_all %>% 
    distinct(sitecode, prehatch) %>% 
    filter(!is.na(prehatch)) %>% 
    nrow()
  sites_without_prehatch <- total_sites_all - sites_with_prehatch
  
  cat(sprintf("  ✓ Total sites: %d\n", total_sites_all))
  cat(sprintf("  ✓ Sites with prehatch: %d\n", sites_with_prehatch))
  cat(sprintf("  ✓ Sites without prehatch: %d\n", sites_without_prehatch))
  
  # Show some sample prehatch values
  prehatch_sample <- data_all %>% 
    distinct(sitecode, prehatch) %>% 
    filter(!is.na(prehatch)) %>% 
    head(5)
  
  if (nrow(prehatch_sample) > 0) {
    cat("\n  Sample prehatch values:\n")
    for (i in 1:nrow(prehatch_sample)) {
      cat(sprintf("    %s: %s\n", prehatch_sample$sitecode[i], prehatch_sample$prehatch[i]))
    }
  }
} else {
  cat("  ✗ ERROR: No data returned\n")
  quit(status = 1)
}

cat("\n")

# Test 2: Get data WITH prehatch filter
cat("Test 2: Loading data WITH prehatch filter (prehatch_only = TRUE)...\n")
data_prehatch <- get_all_inspection_data(
  facility_filter = NULL,
  fosarea_filter = NULL,
  zone_filter = NULL,
  priority_filter = NULL,
  drone_filter = "include_drone",
  spring_only = FALSE,
  prehatch_only = TRUE
)

if (nrow(data_prehatch) > 0) {
  total_sites_prehatch <- data_prehatch %>% distinct(sitecode) %>% nrow()
  
  # Verify ALL returned sites have non-null prehatch
  sites_without_prehatch_in_filtered <- data_prehatch %>% 
    distinct(sitecode, prehatch) %>% 
    filter(is.na(prehatch)) %>% 
    nrow()
  
  cat(sprintf("  ✓ Total sites with prehatch filter: %d\n", total_sites_prehatch))
  cat(sprintf("  ✓ Sites without prehatch in filtered data: %d\n", sites_without_prehatch_in_filtered))
  
  if (sites_without_prehatch_in_filtered > 0) {
    cat("  ✗ ERROR: Filter failed - found sites without prehatch!\n")
    quit(status = 1)
  } else {
    cat("  ✓ SUCCESS: All filtered sites have prehatch values\n")
  }
  
  # Show reduction percentage
  reduction_pct <- round(100 * (total_sites_all - total_sites_prehatch) / total_sites_all, 1)
  cat(sprintf("  ✓ Filter reduced dataset by %.1f%%\n", reduction_pct))
  
} else {
  cat("  ✗ WARNING: No prehatch sites found (this may be valid if no sites have prehatch)\n")
}

cat("\n")

# Test 3: Verify filter works with other filters combined
cat("Test 3: Testing prehatch filter combined with facility filter...\n")
data_combined <- get_all_inspection_data(
  facility_filter = c("Sr"),  # Test with one facility
  fosarea_filter = NULL,
  zone_filter = NULL,
  priority_filter = NULL,
  drone_filter = "include_drone",
  spring_only = FALSE,
  prehatch_only = TRUE
)

if (nrow(data_combined) > 0) {
  total_sites_combined <- data_combined %>% distinct(sitecode) %>% nrow()
  facilities <- data_combined %>% distinct(facility) %>% pull(facility)
  
  cat(sprintf("  ✓ Total Sr prehatch sites: %d\n", total_sites_combined))
  cat(sprintf("  ✓ Facilities in result: %s\n", paste(facilities, collapse = ", ")))
  
  # Verify only Sr facility
  if (length(facilities) == 1 && facilities[1] == "Sr") {
    cat("  ✓ SUCCESS: Facility filter working correctly\n")
  } else {
    cat("  ✗ ERROR: Facility filter failed\n")
    quit(status = 1)
  }
  
  # Verify all have prehatch
  sites_without_prehatch <- data_combined %>% 
    distinct(sitecode, prehatch) %>% 
    filter(is.na(prehatch)) %>% 
    nrow()
  
  if (sites_without_prehatch == 0) {
    cat("  ✓ SUCCESS: Prehatch filter working with facility filter\n")
  } else {
    cat("  ✗ ERROR: Some sites without prehatch in combined filter\n")
    quit(status = 1)
  }
  
} else {
  cat("  ✗ WARNING: No Sr prehatch sites found\n")
}

cat("\n========================================\n")
cat("All Tests Passed! ✓\n")
cat("========================================\n\n")

cat("Summary:\n")
cat(sprintf("  - Total sites (no filter): %d\n", total_sites_all))
cat(sprintf("  - Sites with prehatch: %d (%.1f%%)\n", 
            sites_with_prehatch, 
            100 * sites_with_prehatch / total_sites_all))
cat(sprintf("  - Prehatch filter correctly excludes %d sites\n", 
            total_sites_all - total_sites_prehatch))
cat("\nPrehatch filtering is working correctly!\n")
