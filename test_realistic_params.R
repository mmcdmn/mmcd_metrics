#!/usr/bin/env Rscript

# Test red_air with more realistic parameters to see different statuses
source('shared/db_helpers.R')
source('apps/red_air/air_status_functions.R')

# Test with lower rain threshold to trigger status changes
analysis_date <- Sys.Date()
lookback_period <- 7  # Longer lookback
rain_threshold <- 0.01  # Much lower threshold
treatment_threshold <- 1
facility_filter <- "all"
priority_filter <- "all"  # All priorities
zone_filter <- NULL

cat('Testing with realistic parameters...\n')
cat('Parameters:\n')
cat('- Analysis date:', as.character(analysis_date), '\n')
cat('- Lookback period:', lookback_period, 'days\n')
cat('- Rain threshold:', rain_threshold, 'inches (low to trigger statuses)\n')
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
    examples <- result[result$site_status == status, c('sitecode', 'facility', 'total_rainfall', 'last_inspection_date', 'last_larvae_count', 'treatment_expiry')]
    print(head(examples, 3))
    cat('\n')
  }
} else {
  cat('No data returned\n')
}