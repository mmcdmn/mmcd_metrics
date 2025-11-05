#!/usr/bin/env Rscript

# Final test of the complete red_air functionality
source('shared/db_helpers.R')
source('apps/red_air/air_status_functions.R')

# Test with parameters that should show multiple status types
analysis_date <- Sys.Date()
lookback_period <- 3
rain_threshold <- 1.0  # Normal threshold
treatment_threshold <- 1
facility_filter <- "Wm"  # Specific facility
priority_filter <- "RED"
zone_filter <- NULL

cat('=== FINAL RED_AIR TEST ===\n')
cat('Testing air sites data function...\n')
cat('Parameters:\n')
cat('- Analysis date:', as.character(analysis_date), '\n')
cat('- Lookback period:', lookback_period, 'days\n')
cat('- Rain threshold:', rain_threshold, 'inches\n')
cat('- Treatment threshold:', treatment_threshold, '\n')
cat('- Facility filter:', facility_filter, '\n')
cat('- Priority filter:', priority_filter, '\n\n')

# Test the main function
result <- get_air_sites_data(
  analysis_date = analysis_date,
  lookback_period = lookback_period,
  rain_threshold = rain_threshold,
  treatment_threshold = treatment_threshold,
  facility_filter = facility_filter,
  priority_filter = priority_filter,
  zone_filter = zone_filter
)

cat('Air sites data result:\n')
cat('- Total rows:', nrow(result), '\n')

if(nrow(result) > 0) {
  # Status distribution
  status_counts <- table(result$site_status)
  cat('- Status distribution:\n')
  for(i in 1:length(status_counts)) {
    cat('  ', names(status_counts)[i], ':', status_counts[i], '\n')
  }
  
  # Sample data
  cat('\nSample data (first 3 rows):\n')
  print(result[1:min(3, nrow(result)), c('sitecode', 'facility', 'priority', 'site_status', 'total_rainfall')])
  
  # Test value box creation (like in the app)
  total_sites <- nrow(result)
  needs_inspection <- sum(result$site_status == "Needs Inspection", na.rm = TRUE)
  under_threshold <- sum(result$site_status == "Under Threshold", na.rm = TRUE)
  needs_treatment <- sum(result$site_status == "Needs Treatment", na.rm = TRUE)
  active_treatment <- sum(result$site_status == "Active Treatment", na.rm = TRUE)
  
  cat('\nValue box data:\n')
  cat('- Total sites:', total_sites, '\n')
  cat('- Needs inspection:', needs_inspection, '\n')
  cat('- Under threshold:', under_threshold, '\n')
  cat('- Needs treatment:', needs_treatment, '\n')
  cat('- Active treatment:', active_treatment, '\n')
  
} else {
  cat('- No data returned\n')
}

# Test color mapping
if(nrow(result) > 0) {
  cat('\nTesting color mapping...\n')
  source_colors <- get_status_colors()
  cat('Available colors:\n')
  print(source_colors)
  
  # Test the color assignment
  unique_statuses <- unique(result$site_status)
  cat('Status color mapping:\n')
  for(status in unique_statuses) {
    color <- switch(status,
           "Unknown" = source_colors["unknown"],
           "Needs Inspection" = source_colors["needs_action"], 
           "Under Threshold" = source_colors["active"],
           "Needs Treatment" = source_colors["needs_treatment"],
           "Active Treatment" = source_colors["completed"],
           source_colors["unknown"])
    cat('- ', status, ': ', color, '\n')
  }
}

cat('\n=== RED_AIR TEST COMPLETE ===\n')
cat('The red_air app should now be working properly!\n')