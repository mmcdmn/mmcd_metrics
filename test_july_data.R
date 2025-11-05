#!/usr/bin/env Rscript

# Test red_air with July date for better test data
source('shared/db_helpers.R')
source('apps/red_air/air_status_functions.R')

# Use July date when there's more activity
analysis_date <- as.Date("2025-07-15")  # Mid-July
lookback_period <- 3
rain_threshold <- 1.0
treatment_threshold <- 1
facility_filter <- "all"
priority_filter <- "RED"
zone_filter <- NULL

cat('=== TESTING WITH JULY DATE ===\n')
cat('Parameters:\n')
cat('- Analysis date:', as.character(analysis_date), '\n')
cat('- Lookback period:', lookback_period, 'days\n')
cat('- Rain threshold:', rain_threshold, 'inches\n')
cat('- Treatment threshold:', treatment_threshold, '\n')
cat('- Facility filter:', facility_filter, '\n')
cat('- Priority filter:', priority_filter, '\n\n')

result <- get_air_sites_data(
  analysis_date = analysis_date,
  lookback_period = lookback_period,
  rain_threshold = rain_threshold,
  treatment_threshold = treatment_threshold,
  facility_filter = facility_filter,
  priority_filter = priority_filter,
  zone_filter = zone_filter
)

cat('Result rows:', nrow(result), '\n')
if(nrow(result) > 0) {
  # Check status distribution
  status_counts <- table(result$site_status)
  cat('Status distribution:\n')
  print(status_counts)
  cat('\n')
  
  # Show examples of each status
  for(status in names(status_counts)) {
    cat('=== Examples of', status, '===\n')
    examples <- result[result$site_status == status, c('sitecode', 'facility', 'total_rainfall', 'last_inspection_date', 'last_larvae_count', 'last_treatment_date', 'treatment_material', 'treatment_expiry')]
    print(head(examples, 3))
    cat('\n')
  }
  
  # Test value box calculations
  total_sites <- nrow(result)
  needs_inspection <- sum(result$site_status == "Needs Inspection", na.rm = TRUE)
  under_threshold <- sum(result$site_status == "Under Threshold", na.rm = TRUE)
  needs_treatment <- sum(result$site_status == "Needs Treatment", na.rm = TRUE)
  active_treatment <- sum(result$site_status == "Active Treatment", na.rm = TRUE)
  unknown <- sum(result$site_status == "Unknown", na.rm = TRUE)
  
  cat('Value box data for red_air dashboard:\n')
  cat('- Total air sites:', total_sites, '\n')
  cat('- Needs inspection:', needs_inspection, '\n')
  cat('- Under threshold:', under_threshold, '\n')
  cat('- Needs treatment:', needs_treatment, '\n')
  cat('- Active treatment:', active_treatment, '\n')
  cat('- Unknown:', unknown, '\n')
  
} else {
  cat('No data returned\n')
}

cat('\nTesting with lower rain threshold to see more variety...\n')

# Test with lower threshold
result2 <- get_air_sites_data(
  analysis_date = analysis_date,
  lookback_period = lookback_period,
  rain_threshold = 0.1,  # Lower threshold
  treatment_threshold = treatment_threshold,
  facility_filter = facility_filter,
  priority_filter = priority_filter,
  zone_filter = zone_filter
)

if(nrow(result2) > 0) {
  status_counts2 <- table(result2$site_status)
  cat('Status distribution with 0.1 inch threshold:\n')
  print(status_counts2)
}