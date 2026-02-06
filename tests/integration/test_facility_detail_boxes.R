# Integration test: Load real data and generate facility detail boxes

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
})

cat("Running facility detail boxes integration test with real data...\n\n")

# Source required modules
source(file.path("shared", "db_helpers.R"))
source(file.path("shared", "stat_box_helpers.R"))
source(file.path("apps", "overview", "metric_registry.R"))
source(file.path("apps", "overview", "data_functions.R"))
source(file.path("apps", "overview", "dynamic_server.R"))

# Test parameters
test_date <- as.Date("2025-08-14")
test_expiring <- 7
test_zones <- c("1", "2")
test_metrics <- c("ground_prehatch", "drone", "catch_basin")
test_facilities <- c("North", "East", "South Jordan")

cat("Test Configuration:\n")
cat("  Date: ", format(test_date), "\n")
cat("  Expiring Days: ", test_expiring, "\n")
cat("  Zones: ", paste(test_zones, collapse = ", "), "\n")
cat("  Metrics: ", paste(test_metrics, collapse = ", "), "\n")
cat("  Facilities: ", paste(test_facilities, collapse = ", "), "\n\n")

test_results <- list()

for (metric_id in test_metrics) {
  cat("Testing metric:", metric_id, "\n")
  
  # Check if metric has detail boxes
  if (!has_detail_boxes(metric_id)) {
    cat("  SKIP: No detail_boxes configured\n\n")
    test_results[[metric_id]] <- list(status = "skip", reason = "no config")
    next
  }
  
  # Load facility data
  cat("  Loading facility data...\n")
  facility_data <- tryCatch({
    load_data_by_facility(
      metric = metric_id,
      analysis_date = test_date,
      expiring_days = test_expiring,
      zone_filter = test_zones,
      separate_zones = FALSE
    )
  }, error = function(e) {
    cat("  ERROR loading data:", e$message, "\n")
    NULL
  })
  
  if (is.null(facility_data) || nrow(facility_data) == 0) {
    cat("  FAIL: No data returned\n\n")
    test_results[[metric_id]] <- list(status = "fail", reason = "no data")
    next
  }
  
  cat("  Data loaded:", nrow(facility_data), "facilities\n")
  cat("  Columns:", paste(names(facility_data), collapse = ", "), "\n")
  
  # Test generating detail boxes for each facility
  for (fac in test_facilities) {
    if (!fac %in% facility_data$facility) {
      cat("  SKIP facility:", fac, "(not in data)\n")
      next
    }
    
    cat("  Generating boxes for:", fac, "\n")
    boxes <- tryCatch({
      generate_facility_detail_boxes(
        metric_id = metric_id,
        facility = fac,
        zone_filter = test_zones,
        analysis_date = test_date,
        expiring_days = test_expiring,
        theme = "MMCD"
      )
    }, error = function(e) {
      cat("    ERROR:", e$message, "\n")
      NULL
    })
    
    if (is.null(boxes)) {
      cat("    FAIL: NULL returned\n")
      test_results[[paste0(metric_id, "_", fac)]] <- list(status = "fail", reason = "null output")
    } else if (inherits(boxes, "shiny.tag")) {
      cat("    PASS: Shiny tag generated\n")
      # Check if it contains stat boxes
      boxes_html <- as.character(boxes)
      has_values <- grepl("Total|Active|Expiring|Treated|Wet", boxes_html, ignore.case = TRUE)
      cat("    Contains expected content:", has_values, "\n")
      test_results[[paste0(metric_id, "_", fac)]] <- list(
        status = ifelse(has_values, "pass", "warn"),
        reason = ifelse(has_values, "ok", "missing content")
      )
    } else {
      cat("    WARN: Not a shiny tag, type:", class(boxes), "\n")
      test_results[[paste0(metric_id, "_", fac)]] <- list(status = "warn", reason = "wrong type")
    }
  }
  cat("\n")
}

# Summary
cat("=== TEST SUMMARY ===\n")
pass_count <- sum(sapply(test_results, function(x) x$status == "pass"))
fail_count <- sum(sapply(test_results, function(x) x$status == "fail"))
warn_count <- sum(sapply(test_results, function(x) x$status == "warn"))
skip_count <- sum(sapply(test_results, function(x) x$status == "skip"))

cat("PASS:", pass_count, "\n")
cat("FAIL:", fail_count, "\n")
cat("WARN:", warn_count, "\n")
cat("SKIP:", skip_count, "\n")

if (fail_count > 0) {
  cat("\nFailed tests:\n")
  for (name in names(test_results)) {
    if (test_results[[name]]$status == "fail") {
      cat("  ", name, ":", test_results[[name]]$reason, "\n")
    }
  }
  stop("FAIL: Some tests failed")
}

cat("\nPASS: Integration test completed\n")
