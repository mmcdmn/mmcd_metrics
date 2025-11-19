# Test script for ground prehatch historical functions
cat("Starting test...\n")

# Source the functions
tryCatch({
  source("historical_functions_simple.R")
  cat("Functions loaded successfully\n")
}, error = function(e) {
  cat("ERROR loading functions:", e$message, "\n")
  quit(status = 1)
})

# Test basic function call
cat("Testing get_ground_historical_data...\n")
tryCatch({
  result <- get_ground_historical_data(
    time_period = "weekly", 
    display_metric = "active_sites",
    zone_filter = c("1", "2"),
    start_year = 2024,
    end_year = 2024
  )
  cat("Success! Got", nrow(result), "rows\n")
  cat("Columns:", paste(colnames(result), collapse = ", "), "\n")
  
  if (nrow(result) > 0) {
    cat("First few rows:\n")
    print(head(result, 3))
  }
}, error = function(e) {
  cat("ERROR in get_ground_historical_data:", e$message, "\n")
})

# Test aggregation function
cat("Testing aggregation...\n")
tryCatch({
  # Create dummy data for testing
  test_data <- data.frame(
    time_period = c("2024-W10", "2024-W10", "2024-W11"),
    sitecode = c("SITE001", "SITE002", "SITE001"),
    facility = c("A", "A", "A"),
    zone = c("1", "1", "1"),
    fosarea = c(100, 100, 100),
    acres = c(5.0, 3.0, 5.0)
  )
  
  agg_result <- aggregate_historical_data_by_group(
    test_data, 
    group_by = "mmcd_all",
    time_period = "weekly",
    display_metric = "active_sites",
    combine_zones = TRUE
  )
  
  cat("Aggregation success! Got", nrow(agg_result), "rows\n")
  print(agg_result)
}, error = function(e) {
  cat("ERROR in aggregation:", e$message, "\n")
})

cat("Test completed\n")