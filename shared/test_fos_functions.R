# Test file for FOS-related database functions
# Run with: & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" test_fos_functions.R

# Load required libraries
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
})

# Load .env file
env_paths <- c(
  "../.env",
  "../../.env",
  ".env"
)

for (path in env_paths) {
  if (file.exists(path)) {
    readRenviron(path)
    cat("Loaded .env from:", path, "\n\n")
    break
  }
}

# Source db_helpers
source("db_helpers.R")

cat("=== Testing FOS Functions ===\n\n")

# Test 1: get_foremen_lookup
cat("Test 1: get_foremen_lookup()\n")
cat("-------------------------------\n")
foremen <- get_foremen_lookup()
cat("Rows returned:", nrow(foremen), "\n")
if (nrow(foremen) > 0) {
  cat("Columns:", paste(names(foremen), collapse=", "), "\n")
  cat("Sample data:\n")
  print(head(foremen, 5))
} else {
  cat("WARNING: No data returned!\n")
}
cat("\n")

# Test 2: get_foreman_choices
cat("Test 2: get_foreman_choices()\n")
cat("-------------------------------\n")
choices <- get_foreman_choices()
cat("Number of choices:", length(choices), "\n")
cat("Sample choices:\n")
print(head(choices, 5))
cat("\n")

# Test 3: Check gis_sectcode for fosarea
cat("Test 3: Query gis_sectcode for fosarea\n")
cat("-------------------------------\n")
con <- get_db_connection()
if (!is.null(con)) {
  tryCatch({
    # Get unique fosarea values from gis_sectcode
    fosarea_data <- dbGetQuery(con, "
      SELECT DISTINCT
        fosarea,
        facility,
        zone
      FROM gis_sectcode
      WHERE fosarea IS NOT NULL
        AND facility IS NOT NULL
      ORDER BY facility, fosarea
      LIMIT 10
    ")
    
    cat("Rows returned:", nrow(fosarea_data), "\n")
    if (nrow(fosarea_data) > 0) {
      cat("Columns:", paste(names(fosarea_data), collapse=", "), "\n")
      cat("Sample data:\n")
      print(head(fosarea_data, 10))
    } else {
      cat("WARNING: No fosarea data found!\n")
    }
    
    dbDisconnect(con)
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    if (!is.null(con)) dbDisconnect(con)
  })
} else {
  cat("ERROR: Could not connect to database\n")
}
cat("\n")

# Test 4: Compare employee_list vs gis_sectcode
cat("Test 4: Compare FOS sources\n")
cat("-------------------------------\n")
con <- get_db_connection()
if (!is.null(con)) {
  tryCatch({
    # Get count from employee_list
    emp_count <- dbGetQuery(con, "
      SELECT COUNT(DISTINCT shortname) as count
      FROM employee_list
      WHERE emp_type = 'FieldSuper' AND active = true
    ")
    cat("Active FOS in employee_list:", emp_count$count, "\n")
    
    # Get count from gis_sectcode
    gis_count <- dbGetQuery(con, "
      SELECT COUNT(DISTINCT fosarea) as count
      FROM gis_sectcode
      WHERE fosarea IS NOT NULL
    ")
    cat("Unique fosarea in gis_sectcode:", gis_count$count, "\n")
    
    # Get sample of fosarea values
    fosarea_sample <- dbGetQuery(con, "
      SELECT DISTINCT fosarea
      FROM gis_sectcode
      WHERE fosarea IS NOT NULL
      ORDER BY fosarea
    ")
    cat("\nAll unique fosarea values:\n")
    print(fosarea_sample$fosarea)
    
    dbDisconnect(con)
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    if (!is.null(con)) dbDisconnect(con)
  })
} else {
  cat("ERROR: Could not connect to database\n")
}

cat("\n=== Tests Complete ===\n")
