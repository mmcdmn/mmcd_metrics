#!/usr/bin/env Rscript

# Simple debug to find the exact error
source('shared/db_helpers.R')
source('apps/red_air/air_status_functions.R')

cat('Testing get_air_sites_data function call...\n')

# Test with exact same parameters as the debug script showed work
result <- tryCatch({
  get_air_sites_data(
    analysis_date = Sys.Date(),
    lookback_period = "3",
    rain_threshold = "1.0", 
    treatment_threshold = "1",
    facility_filter = "all",
    priority_filter = "all",
    zone_filter = NULL
  )
}, error = function(e) {
  cat('FULL ERROR:\n')
  cat('Message:', e$message, '\n')
  cat('Call:', deparse(e$call), '\n')
  print(e)
  return(NULL)
})

if(is.null(result)) {
  cat('Function returned NULL due to error\n')
} else {
  cat('Function succeeded - got', nrow(result), 'rows\n')
}