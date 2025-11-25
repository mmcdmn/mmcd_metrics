# TEST-CHART.R
# Test the progress chart to identify the plotly error

library(dplyr)
library(ggplot2)
library(plotly)

# Source required files
source("../../shared/db_helpers.R")
source("display_functions.R")

cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("GROUND PREHATCH PROGRESS CHART TEST\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

# Create minimal test data
cat("Creating test data...\n")
test_data <- data.frame(
  display_name = c("East", "North", "South Jordan", "West Maple Grove"),
  prehatch_sites_cnt = c(100, 120, 90, 110),
  ph_treated_cnt = c(80, 90, 70, 85),
  ph_expiring_cnt = c(10, 15, 8, 12),
  ph_expired_cnt = c(5, 8, 6, 7),
  ph_skipped_cnt = c(5, 7, 6, 6),
  group_name = c("East", "North", "South Jordan", "West Maple Grove"),
  stringsAsFactors = FALSE
)

cat("Test data rows:", nrow(test_data), "\n")
cat("Columns:", paste(names(test_data), collapse=", "), "\n\n")

# Test chart creation - Facility grouping
cat("TEST 1: Create chart with facility grouping\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n")
tryCatch({
  chart_result <- create_progress_chart(
    data = test_data,
    group_by = "facility",
    expiring_filter = "all",
    expiring_days = 14,
    return_height_info = TRUE
  )
  
  cat("✓ Chart created successfully\n")
  cat("  Chart type:", class(chart_result$plot)[1], "\n")
  cat("  Height:", chart_result$height, "\n")
  
  # Try converting to plotly if it's a ggplot
  cat("  Testing plotly conversion...\n")
  if ("gg" %in% class(chart_result$plot)) {
    plotly_obj <- ggplotly(chart_result$plot)
    cat("  ✓ Plotly conversion successful\n")
    cat("  Plotly object class:", class(plotly_obj)[1], "\n")
  } else if ("plotly" %in% class(chart_result$plot)) {
    cat("  ✓ Already a plotly object\n")
  }
}, error = function(e) {
  cat("✗ ERROR:", e$message, "\n")
  cat("\nFull error details:\n")
  print(e)
  cat("\nTraceback:\n")
  traceback()
})
cat("\n")

# Test chart creation with MMCD grouping (simpler case)
cat("TEST 2: Create chart with MMCD grouping\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n")

test_data_mmcd <- data.frame(
  display_name = "All MMCD",
  prehatch_sites_cnt = 420,
  ph_treated_cnt = 325,
  ph_expiring_cnt = 45,
  ph_expired_cnt = 26,
  ph_skipped_cnt = 24,
  group_name = "All MMCD",
  stringsAsFactors = FALSE
)

tryCatch({
  chart_result <- create_progress_chart(
    data = test_data_mmcd,
    group_by = "mmcd_all",
    expiring_filter = "all",
    expiring_days = 14,
    return_height_info = TRUE
  )
  
  cat("✓ Chart created successfully\n")
  cat("  Chart type:", class(chart_result$plot)[1], "\n")
  
  # Try converting to plotly
  cat("  Testing plotly conversion...\n")
  if ("gg" %in% class(chart_result$plot)) {
    plotly_obj <- ggplotly(chart_result$plot)
    cat("  ✓ Plotly conversion successful\n")
  } else if ("plotly" %in% class(chart_result$plot)) {
    cat("  ✓ Already a plotly object\n")
  }
}, error = function(e) {
  cat("✗ ERROR:", e$message, "\n")
  cat("\nFull error details:\n")
  print(e)
})
cat("\n")

cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("GROUND PREHATCH CHART TEST COMPLETE\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
