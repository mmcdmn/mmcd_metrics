#!/usr/bin/env Rscript

# Test red_air specific query
source('shared/db_helpers.R')

con <- get_db_connection()
if(!is.null(con)) {
  cat('Testing red_air query components...\n')
  
  # Test the main query structure that red_air uses
  analysis_date <- Sys.Date()
  
  # Simplified version of the red_air query
  query <- sprintf("
    SELECT 
      b.facility,
      b.sitecode,
      b.acres,
      b.priority,
      b.prehatch,
      ST_X(ST_Centroid(ST_Transform(b.geom, 4326))) as longitude,
      ST_Y(ST_Centroid(ST_Transform(b.geom, 4326))) as latitude
    FROM loc_breeding_sites b
    WHERE (b.enddate IS NULL OR b.enddate > '%s')
      AND b.air_gnd = 'A'
      AND b.geom IS NOT NULL
    LIMIT 10
  ", analysis_date)
  
  cat('Query:', query, '\n')
  
  result <- dbGetQuery(con, query)
  cat('Result rows:', nrow(result), '\n')
  print(head(result))
  
  dbDisconnect(con)
} else {
  cat('Database connection failed\n')
}