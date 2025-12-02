# Test the archive table structure and verify the query approach

library(dplyr)

source("../../shared/db_helpers.R")

con <- get_db_connection()
if (is.null(con)) {
  cat("ERROR: Could not connect to database\n")
  quit(status = 1)
}

cat("=== Testing Archive Table Structure ===\n\n")

# Test 1: Check dblarv_treatment_cb_archive structure
cat("Test 1: Checking dblarv_treatment_cb_archive table...\n")
test_query1 <- "
  SELECT * 
  FROM dblarv_treatment_cb_archive
  LIMIT 5
"

tryCatch({
  result <- dbGetQuery(con, test_query1)
  cat("✓ dblarv_treatment_cb_archive exists!\n")
  cat("Columns:", paste(names(result), collapse = ", "), "\n")
  cat("Sample rows:\n")
  print(result)
}, error = function(e) {
  cat("✗ Query failed:", e$message, "\n")
})

cat("\n\nTest 2: Archive query matching current progress style...\n")
# This should mirror the current year query but use archive tables
test_query2 <- "
  SELECT 
    loc_catchbasin.gid as catchbasin_id,
    loc_catchbasin.sitecode,
    left(loc_catchbasin.sitecode, 7) as sectcode,
    loc_catchbasin.facility,
    sc.zone,
    sc.fosarea,
    dblarv_insptrt_archive.inspdate,
    dblarv_treatment_cb_archive.status,
    mattype_list_targetdose.effect_days,
    EXTRACT(YEAR FROM dblarv_insptrt_archive.inspdate) as treatment_year
  FROM dblarv_insptrt_archive
  JOIN dblarv_treatment_cb_archive ON dblarv_insptrt_archive.pkey_pg = dblarv_treatment_cb_archive.treatment_id
  JOIN loc_catchbasin ON dblarv_treatment_cb_archive.catchbasin_id = loc_catchbasin.gid
  JOIN mattype_list_targetdose USING (matcode)
  LEFT JOIN gis_sectcode sc ON left(loc_catchbasin.sitecode, 7) = sc.sectcode
  WHERE EXTRACT(YEAR FROM dblarv_insptrt_archive.inspdate) = 2023
    AND loc_catchbasin.status_udw = 'W'
    AND loc_catchbasin.lettergrp <> 'Z'
  LIMIT 10
"

tryCatch({
  result <- dbGetQuery(con, test_query2)
  cat("✓ Archive query using treatment_cb_archive works!\n")
  cat("Rows returned:", nrow(result), "\n")
  if (nrow(result) > 0) {
    cat("Columns:", paste(names(result), collapse = ", "), "\n")
    cat("\nFirst few rows:\n")
    print(head(result))
  }
}, error = function(e) {
  cat("✗ Archive query failed:", e$message, "\n")
})

cat("\n\nTest 3: Compare counts - Current vs Archive approach...\n")
# Count treatments in 2023 using archive
test_query3 <- "
  SELECT 
    EXTRACT(YEAR FROM dblarv_insptrt_archive.inspdate) as year,
    COUNT(*) as treatment_count,
    COUNT(DISTINCT loc_catchbasin.gid) as unique_cb_count
  FROM dblarv_insptrt_archive
  JOIN dblarv_treatment_cb_archive ON dblarv_insptrt_archive.pkey_pg = dblarv_treatment_cb_archive.treatment_id
  JOIN loc_catchbasin ON dblarv_treatment_cb_archive.catchbasin_id = loc_catchbasin.gid
  WHERE EXTRACT(YEAR FROM dblarv_insptrt_archive.inspdate) BETWEEN 2022 AND 2024
    AND loc_catchbasin.status_udw = 'W'
    AND loc_catchbasin.lettergrp <> 'Z'
  GROUP BY EXTRACT(YEAR FROM dblarv_insptrt_archive.inspdate)
  ORDER BY year
"

tryCatch({
  result <- dbGetQuery(con, test_query3)
  cat("✓ Summary by year:\n")
  print(result)
}, error = function(e) {
  cat("✗ Summary query failed:", e$message, "\n")
})

dbDisconnect(con)
cat("\n=== Test Complete ===\n")
