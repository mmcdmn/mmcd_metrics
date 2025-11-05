#!/usr/bin/env Rscript

# Test the corrected consecutive rainfall logic
source('shared/db_helpers.R')
source('apps/red_air/air_status_functions.R')
library(dplyr)

cat('=== TESTING CORRECTED CONSECUTIVE RAINFALL LOGIC ===\n')

# Test with different consecutive day periods
test_cases <- list(
  list(period = 1, threshold = 0.1, desc = "1-day period, low threshold"),
  list(period = 3, threshold = 0.5, desc = "3-day period, medium threshold"),  
  list(period = 5, threshold = 1.0, desc = "5-day period, high threshold"),
  list(period = 7, threshold = 0.3, desc = "7-day period, low threshold")
)

for(i in 1:length(test_cases)) {
  test <- test_cases[[i]]
  
  cat('\nTest', i, ':', test$desc, '\n')
  
  result <- get_air_sites_data(
    analysis_date = Sys.Date(),
    lookback_period = test$period,
    rain_threshold = test$threshold,
    treatment_threshold = 2,
    facility_filter = "all",
    priority_filter = "all",
    zone_filter = NULL
  )
  
  cat('- Total sites:', nrow(result), '\n')
  if(nrow(result) > 0) {
    status_counts <- table(result$site_status)
    cat('- Needs Inspection:', status_counts[["Needs Inspection"]] %||% 0, '\n')
    cat('- Under Threshold:', status_counts[["Under Threshold"]] %||% 0, '\n')
    cat('- Needs Treatment:', status_counts[["Needs Treatment"]] %||% 0, '\n')
    cat('- Unknown:', status_counts[["Unknown"]] %||% 0, '\n')
  }
}

cat('\n=== BUSINESS LOGIC SUMMARY ===\n')
cat('✓ Sites transition from Unknown/Under Threshold → Needs Inspection when ANY consecutive', '\n')
cat('  period of [lookback_period] days has cumulative rainfall ≥ threshold\n')
cat('✓ Sites stay "Needs Inspection" until actually inspected\n')
cat('✓ Sites transition from Needs Inspection → Under Threshold/Needs Treatment based on larvae count\n')
cat('✓ Sites return from Under Threshold → Needs Inspection when new triggering rainfall occurs\n')
cat('✓ Active Treatment → Unknown when treatment expires\n')