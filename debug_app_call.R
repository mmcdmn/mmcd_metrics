#!/usr/bin/env Rscript

# Debug the exact call that the app makes
source('shared/db_helpers.R')
source('apps/red_air/air_status_functions.R')

# Simulate exactly what the app does with default values
analysis_date <- Sys.Date()
lookback_period <- "3"  # String like from UI
rain_threshold <- "1.0"  # String like from UI 
treatment_threshold <- "1"  # String like from UI
facility_filter <- "all"
priority_filter <- "all"
zone_filter <- NULL

cat('=== DEBUGGING APP CALL ===\n')
cat('Simulating exact app call with these parameters:\n')
cat('- analysis_date:', class(analysis_date), '=', as.character(analysis_date), '\n')
cat('- lookback_period:', class(lookback_period), '=', lookback_period, '\n')
cat('- rain_threshold:', class(rain_threshold), '=', rain_threshold, '\n')
cat('- treatment_threshold:', class(treatment_threshold), '=', treatment_threshold, '\n')
cat('- facility_filter:', class(facility_filter), '=', facility_filter, '\n')
cat('- priority_filter:', class(priority_filter), '=', priority_filter, '\n')
cat('- zone_filter:', class(zone_filter), '=', zone_filter, '\n\n')

# Test database connection first
cat('Testing database connection...\n')
con <- get_db_connection()
if(is.null(con)) {
  cat('ERROR: Database connection failed!\n')
  stop('Cannot connect to database')
} else {
  cat('Database connection successful\n')
  dbDisconnect(con)
}

cat('\nCalling get_air_sites_data function...\n')

# Wrap in tryCatch to see any errors
result <- tryCatch({
  get_air_sites_data(
    analysis_date = analysis_date,
    lookback_period = lookback_period, 
    rain_threshold = rain_threshold,
    treatment_threshold = treatment_threshold,
    facility_filter = facility_filter,
    priority_filter = priority_filter,
    zone_filter = zone_filter
  )
}, error = function(e) {
  cat('ERROR in get_air_sites_data:\n')
  cat('Error message:', e$message, '\n')
  return(data.frame())
}, warning = function(w) {
  cat('WARNING in get_air_sites_data:\n')
  cat('Warning message:', w$message, '\n')
})

cat('Function completed.\n')
cat('Result type:', class(result), '\n')
cat('Result dimensions:', dim(result), '\n')

if(nrow(result) == 0) {
  cat('\nNO DATA RETURNED! Debugging...\n')
  
  # Test a simple query to see if we can get any air sites at all
  cat('Testing simple air sites query...\n')
  con <- get_db_connection()
  simple_result <- dbGetQuery(con, \"
    SELECT COUNT(*) as count 
    FROM loc_breeding_sites 
    WHERE air_gnd = 'A' AND enddate IS NULL
  \")
  cat('Total active air sites in database:', simple_result$count, '\n')
  
  if(simple_result$count > 0) {
    cat('Air sites exist, so the issue is in the complex query.\n')
    
    # Test each CTE step by step
    cat('Testing ActiveAirSites CTE...\n')
    active_sites <- dbGetQuery(con, \"
      SELECT COUNT(*) as count
      FROM loc_breeding_sites b
      WHERE b.enddate IS NULL
        AND b.air_gnd = 'A'
        AND b.geom IS NOT NULL
    \")
    cat('Active air sites with geometry:', active_sites$count, '\n')
    
  } else {
    cat('No active air sites found in database!\n')
  }
  
  dbDisconnect(con)
} else {
  cat('SUCCESS! Got', nrow(result), 'rows\n')
  if(nrow(result) > 0) {
    status_counts <- table(result$site_status)
    print(status_counts)
  }
}