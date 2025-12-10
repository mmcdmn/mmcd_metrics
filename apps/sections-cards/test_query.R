# Test script for sections-cards data query
# Run with: & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" test_query.R

# Load required libraries
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
})

# Source shared helpers and data functions
setwd("c:/Users/datatech/Documents/mmcd_metrics/apps/sections-cards")
source("../../shared/db_helpers.R")
source("data_functions.R")

# Load environment variables
load_env_vars()

cat("\n=== Testing Breeding Sites Query ===\n\n")

tryCatch({
  # Test the query
  data <- get_breeding_sites_with_sections()
  
  cat("✓ Query executed successfully!\n\n")
  cat("Number of rows returned:", nrow(data), "\n")
  cat("Number of columns:", ncol(data), "\n\n")
  
  cat("Column names:\n")
  print(names(data))
  cat("\n")
  
  cat("First 5 rows:\n")
  print(head(data, 5))
  cat("\n")
  
  cat("Summary statistics:\n")
  cat("- Unique sitecodes:", length(unique(data$sitecode)), "\n")
  cat("- Unique facilities:", length(unique(data$facility[!is.na(data$facility)])), "\n")
  cat("- Unique zones:", length(unique(data$zone[!is.na(data$zone)])), "\n")
  cat("- Unique priorities:", paste(unique(data$priority[!is.na(data$priority)]), collapse = ", "), "\n")
  cat("\n")
  
  # Check for any issues
  if (nrow(data) == 0) {
    cat("⚠ WARNING: No data returned from query!\n")
  }
  
  # Check for missing joins
  missing_section <- sum(is.na(data$section))
  if (missing_section > 0) {
    cat("⚠ WARNING:", missing_section, "sites have no section information\n")
    cat("Sample sitecodes without sections:\n")
    print(head(data$sitecode[is.na(data$section)], 10))
  }
  
  cat("\n✓ Test completed successfully!\n")
  
}, error = function(e) {
  cat("✗ ERROR:", e$message, "\n")
  cat("\nFull error details:\n")
  print(e)
})
