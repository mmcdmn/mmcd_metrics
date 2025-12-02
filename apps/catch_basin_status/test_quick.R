# Quick test - just verify queries are valid

library(dplyr)

source("../../shared/db_helpers.R")

con <- get_db_connection()
if (is.null(con)) {
  cat("ERROR: Could not connect to database\n")
  quit(status = 1)
}

cat("Testing archive query structure...\n")

# Test simple archive query
test_query <- "
  SELECT 
    dblarv_insptrt_archive.sitecode,
    dblarv_insptrt_archive.inspdate,
    EXTRACT(YEAR FROM dblarv_insptrt_archive.inspdate) as treatment_year
  FROM dblarv_insptrt_archive
  WHERE EXTRACT(YEAR FROM dblarv_insptrt_archive.inspdate) = 2023
  LIMIT 5
"

tryCatch({
  result <- dbGetQuery(con, test_query)
  cat("✓ Archive query works!\n")
  cat("Rows returned:", nrow(result), "\n")
  if (nrow(result) > 0) {
    print(head(result))
  }
}, error = function(e) {
  cat("✗ Archive query failed:", e$message, "\n")
})

cat("\nTesting current year query structure...\n")

# Test current year query with treatment_catchbasin join
test_query2 <- "
  SELECT 
    loc_catchbasin.gid as catchbasin_id,
    loc_catchbasin.sitecode,
    dblarv_insptrt_current.inspdate
  FROM dblarv_insptrt_current
  JOIN dblarv_treatment_catchbasin ON dblarv_insptrt_current.pkey_pg = dblarv_treatment_catchbasin.treatment_id
  JOIN loc_catchbasin ON dblarv_treatment_catchbasin.catchbasin_id = loc_catchbasin.gid
  WHERE EXTRACT(YEAR FROM dblarv_insptrt_current.inspdate) = 2025
  LIMIT 5
"

tryCatch({
  result <- dbGetQuery(con, test_query2)
  cat("✓ Current year query works!\n")
  cat("Rows returned:", nrow(result), "\n")
  if (nrow(result) > 0) {
    print(head(result))
  }
}, error = function(e) {
  cat("✗ Current year query failed:", e$message, "\n")
})

dbDisconnect(con)
cat("\nQuick test complete!\n")
