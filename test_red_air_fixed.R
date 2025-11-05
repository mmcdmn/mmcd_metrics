#!/usr/bin/env Rscript

# Test the updated red_air query with correct table
source('shared/db_helpers.R')
source('apps/red_air/air_status_functions.R')

# Test with typical red_air parameters
analysis_date <- Sys.Date()
lookback_period <- 3
rain_threshold <- 1.0
treatment_threshold <- 1
facility_filter <- "all"
priority_filter <- "RED"
zone_filter <- NULL

cat('Testing updated red_air query...\n')
cat('Parameters:\n')
cat('- Analysis date:', as.character(analysis_date), '\n')
cat('- Lookback period:', lookback_period, 'days\n')
cat('- Rain threshold:', rain_threshold, 'inches\n')
cat('- Treatment threshold:', treatment_threshold, '\n')
cat('- Facility filter:', facility_filter, '\n')
cat('- Priority filter:', priority_filter, '\n')

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
  cat('Sample data:\n')
  print(head(result, 3))
  
  # Check status distribution
  status_counts <- table(result$site_status)
  cat('Status distribution:\n')
  print(status_counts)
} else {
  cat('No data returned - checking individual components...\n')
}