#!/usr/bin/env Rscript

# Check weather table structure
source('shared/db_helpers.R')

con <- get_db_connection()
if(!is.null(con)) {
  cat('Checking tofly_weather table...\n')
  
  # Check column structure
  result1 <- dbGetQuery(con, "SELECT column_name, data_type FROM information_schema.columns WHERE table_name = 'tofly_weather' ORDER BY ordinal_position")
  cat('tofly_weather columns:\n')
  print(result1)
  
  # Check sample data
  result2 <- dbGetQuery(con, "SELECT * FROM tofly_weather ORDER BY date DESC LIMIT 5")
  cat('Sample weather data:\n')
  print(result2)
  
  # Check if rainfall column exists
  rainfall_cols <- dbGetQuery(con, "SELECT column_name FROM information_schema.columns WHERE table_name = 'tofly_weather' AND column_name LIKE '%rain%'")
  cat('Rainfall columns:\n')
  print(rainfall_cols)
  
  dbDisconnect(con)
} else {
  cat('Database connection failed\n')
}