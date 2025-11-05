#!/usr/bin/env Rscript

# Test script to debug red_air data issues
source('shared/db_helpers.R')

con <- get_db_connection()
if(!is.null(con)) {
  cat('Database connection successful\n')
  
  # Test 1: Count total air sites
  cat('Testing air sites count...\n')
  result1 <- dbGetQuery(con, "SELECT COUNT(*) as count FROM loc_breeding_sites WHERE air_gnd = 'A'")
  cat('Total air sites:', result1$count, '\n')
  
  # Test 2: Get a sample of air sites
  cat('Getting sample air sites...\n')
  result2 <- dbGetQuery(con, "SELECT facility, sitecode, air_gnd FROM loc_breeding_sites WHERE air_gnd = 'A' LIMIT 5")
  print(result2)
  
  # Test 3: Check if there are recent active air sites
  cat('Testing active air sites (not ended)...\n')
  result3 <- dbGetQuery(con, "SELECT COUNT(*) as count FROM loc_breeding_sites WHERE air_gnd = 'A' AND (enddate IS NULL OR enddate > CURRENT_DATE)")
  cat('Active air sites:', result3$count, '\n')
  
  # Test 4: Check facilities with air sites
  cat('Testing facilities with air sites...\n')
  result4 <- dbGetQuery(con, "SELECT facility, COUNT(*) as count FROM loc_breeding_sites WHERE air_gnd = 'A' GROUP BY facility")
  print(result4)
  
  dbDisconnect(con)
} else {
  cat('Database connection failed\n')
}