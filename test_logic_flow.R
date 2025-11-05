#!/usr/bin/env Rscript

# Test the corrected status logic with different thresholds
source('shared/db_helpers.R')
source('apps/red_air/air_status_functions.R')

cat('=== TESTING CORRECTED STATUS LOGIC ===\n')

# Test with very low rain threshold to see more "Needs Inspection" status
result1 <- get_air_sites_data(
  analysis_date = Sys.Date(),
  lookback_period = "7",      # Longer lookback
  rain_threshold = "0.1",     # Very low threshold
  treatment_threshold = "5",  # Higher treatment threshold
  facility_filter = "all",
  priority_filter = "all",
  zone_filter = NULL
)

cat('Test 1 - Low rain threshold (0.1"), high treatment threshold (5):\n')
cat('Total rows:', nrow(result1), '\n')
if(nrow(result1) > 0) {
  status_counts1 <- table(result1$site_status)
  print(status_counts1)
}

# Test with higher rain threshold
result2 <- get_air_sites_data(
  analysis_date = Sys.Date(),
  lookback_period = "3",
  rain_threshold = "2.0",     # Higher threshold  
  treatment_threshold = "1",  # Lower treatment threshold
  facility_filter = "all",
  priority_filter = "all", 
  zone_filter = NULL
)

cat('\nTest 2 - High rain threshold (2.0"), low treatment threshold (1):\n')
cat('Total rows:', nrow(result2), '\n')
if(nrow(result2) > 0) {
  status_counts2 <- table(result2$site_status)
  print(status_counts2)
}

# Show some sample data
if(nrow(result1) > 0) {
  cat('\nSample data from Test 1:\n')
  sample_data <- result1[1:min(5, nrow(result1)), c('sitecode', 'site_status', 'total_rainfall', 'last_inspection_date', 'last_larvae_count')]
  print(sample_data)
}

cat('\n=== STATUS MEANINGS ===\n')
cat('Blue dots (Unknown): Default status, or low rainfall, or post-treatment expired\n')
cat('Orange dots (Needs Inspection): Rainfall >= threshold, needs inspection\n') 
cat('Green dots (Under Threshold): Inspected, larvae count < treatment threshold\n')
cat('Red dots (Needs Treatment): Inspected, larvae count >= treatment threshold\n')
cat('Purple dots (Active Treatment): Treatment applied and still effective\n')