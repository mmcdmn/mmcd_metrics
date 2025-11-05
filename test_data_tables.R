#!/usr/bin/env Rscript

# Test rainfall and inspection data availability
source('shared/db_helpers.R')

con <- get_db_connection()
if(!is.null(con)) {
  cat('Testing rainfall data...\n')
  
  # Check if rainfall table exists and has recent data
  result1 <- dbGetQuery(con, "SELECT COUNT(*) as count FROM public.rainfall WHERE rdate >= CURRENT_DATE - INTERVAL '7 days'")
  cat('Recent rainfall records (last 7 days):', result1$count, '\n')
  
  cat('Testing inspection data...\n')
  
  # Check current inspections
  result2 <- dbGetQuery(con, "SELECT COUNT(*) as count FROM public.dblarv_insptrt_current WHERE action IN ('9', '8')")
  cat('Current inspection records:', result2$count, '\n')
  
  # Check archive inspections
  result3 <- dbGetQuery(con, "SELECT COUNT(*) as count FROM public.dblarv_insptrt_archive WHERE action IN ('9', '8')")
  cat('Archive inspection records:', result3$count, '\n')
  
  # Test a simple version of the red_air function
  cat('Testing simplified red_air query...\n')
  
  analysis_date <- Sys.Date()
  lookback_days <- 3
  
  simple_query <- sprintf("
    WITH ActiveAirSites AS (
      SELECT 
        b.facility,
        b.sitecode,
        b.priority
      FROM loc_breeding_sites b
      WHERE (b.enddate IS NULL OR b.enddate > '%s')
        AND b.air_gnd = 'A'
      LIMIT 5
    )
    SELECT * FROM ActiveAirSites
  ", analysis_date)
  
  result4 <- dbGetQuery(con, simple_query)
  cat('Simple CTE result rows:', nrow(result4), '\n')
  print(result4)
  
  dbDisconnect(con)
} else {
  cat('Database connection failed\n')
}