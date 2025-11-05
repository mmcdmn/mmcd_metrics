#!/usr/bin/env Rscript

# Test the gis_sectcode join that might be causing issues
source('shared/db_helpers.R')

con <- get_db_connection()
if(!is.null(con)) {
  cat('Testing gis_sectcode join...\n')
  
  # Test if gis_sectcode table exists and has data
  result1 <- dbGetQuery(con, "SELECT COUNT(*) as count FROM public.gis_sectcode")
  cat('gis_sectcode rows:', result1$count, '\n')
  
  # Test the complex join condition
  query <- "
    SELECT 
      b.facility,
      b.sitecode,
      g.zone,
      g.sectcode
    FROM loc_breeding_sites b
    LEFT JOIN public.gis_sectcode g ON LEFT(b.sitecode, 6) || '-' = g.sectcode
      OR LEFT(b.sitecode, 6) || 'N' = g.sectcode
      OR LEFT(b.sitecode, 6) || 'S' = g.sectcode
      OR LEFT(b.sitecode, 6) || 'E' = g.sectcode
      OR LEFT(b.sitecode, 6) || 'W' = g.sectcode
    WHERE b.air_gnd = 'A'
      AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
    LIMIT 5
  "
  
  result2 <- dbGetQuery(con, query)
  cat('Join result rows:', nrow(result2), '\n')
  print(result2)
  
  dbDisconnect(con)
} else {
  cat('Database connection failed\n')
}