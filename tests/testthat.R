# =============================================================================
# MMCD METRICS - TEST RUNNER
# =============================================================================
# Run all unit tests for shared functions
# 
# Usage:
#   cd mmcd_metrics
#   Rscript tests/testthat.R
#
# Or in R console:
#   source("tests/testthat.R")
# =============================================================================

# Load required packages
if (!require("testthat", quietly = TRUE)) {
  install.packages("testthat")
  library(testthat)
}

# Set working directory to project root if needed
if (basename(getwd()) == "tests") {
  setwd("..")
}

# Source shared files (from project root)
cat("\n=== Loading shared modules ===\n")

# Load app libraries first (shiny, dplyr, etc.)
tryCatch({
  source("shared/app_libraries.R")
  cat("✓ app_libraries.R loaded\n")
}, error = function(e) cat("✗ app_libraries.R failed:", e$message, "\n"))

# Also load htmltools explicitly for tag rendering in tests
if (!require("htmltools", quietly = TRUE)) {
  install.packages("htmltools")
}
library(htmltools)
cat("✓ htmltools loaded for tag rendering\n")

# =============================================================================
# TESTING MODE FLAG
# =============================================================================
# Set this to TRUE to run tests WITHOUT database connection (uses stubs)
# Set to FALSE to run integration tests with real database
TESTING_MODE_ISOLATED <- TRUE

if (TESTING_MODE_ISOLATED) {
  cat(" Running in ISOLATED mode - no database connection required\n")
  
  # Load stubs FIRST to override DB connection functions BEFORE any module loads
  source("tests/test_stubs.R")
  
} else {
  cat("🔌 Running in INTEGRATION mode - database connection required\n")
  # Load actual .env file (same way apps do)
  if (file.exists(".env")) {
    env_lines <- readLines(".env", warn = FALSE)
    for (line in env_lines) {
      if (grepl("^[^#].*=", line)) {
        parts <- strsplit(line, "=", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          key <- trimws(parts[1])
          value <- trimws(paste(parts[-1], collapse = "="))
          value <- gsub("^['\"]|['\"]$", "", value)
          Sys.setenv(setNames(value, key))
        }
      }
    }
    cat("✓ .env file loaded\n")
  } else {
    cat("⚠ No .env file found - using existing environment variables\n")
  }
}

# Source the shared modules (stubs already loaded in isolated mode, will be overridden)
tryCatch({
  source("shared/color_themes.R")
  cat("✓ color_themes.R loaded\n")
}, error = function(e) cat("✗ color_themes.R failed:", e$message, "\n"))

# In isolated mode, skip db_pool.R since we have stubs
if (!TESTING_MODE_ISOLATED) {
  tryCatch({
    source("shared/db_pool.R")
    cat("✓ db_pool.R loaded\n")
  }, error = function(e) cat("✗ db_pool.R failed:", e$message, "\n"))
}

tryCatch({
  source("shared/db_helpers.R")
  cat("✓ db_helpers.R loaded\n")
}, error = function(e) cat("✗ db_helpers.R failed:", e$message, "\n"))

tryCatch({
  source("shared/stat_box_helpers.R")
  cat("✓ stat_box_helpers.R loaded\n")
}, error = function(e) cat("✗ stat_box_helpers.R failed:", e$message, "\n"))

tryCatch({
  source("shared/server_utilities.R")
  cat("✓ server_utilities.R loaded\n")
}, error = function(e) cat("✗ server_utilities.R failed:", e$message, "\n"))

# Re-load test stubs AFTER db_helpers to override DB-dependent functions
if (TESTING_MODE_ISOLATED) {
  source("tests/test_stubs.R")
}

# =============================================================================
# RUN TESTS
# =============================================================================

# Run shared module tests
cat("\n=== Running Shared Module Tests ===\n\n")

shared_results <- test_dir(
  "tests/shared",
  reporter = "summary",
  stop_on_failure = FALSE
)

# Run app tests if they exist
app_results <- NULL
if (dir.exists("tests/apps") && length(list.files("tests/apps", pattern = "^test-.*\\.R$")) > 0) {
  cat("\n=== Running App Tests ===\n\n")
  
  app_results <- test_dir(
    "tests/apps",
    reporter = "summary",
    stop_on_failure = FALSE
  )
}

# =============================================================================
# TEST SUMMARY
# =============================================================================
cat("\n")
cat("+----------------------------------------------------------------------+\n")
cat("|                         TEST SUMMARY                                 |\n")
cat("+----------------------------------------------------------------------+\n")

# Convert results to data frame for analysis
results_df <- as.data.frame(shared_results)

total_tests <- sum(results_df$passed) + sum(results_df$failed) + sum(results_df$skipped)
total_passed <- sum(results_df$passed)
total_failed <- sum(results_df$failed)
total_skipped <- sum(results_df$skipped)
total_warnings <- sum(results_df$warning)

# Add app results if they exist
if (!is.null(app_results)) {
  app_results_df <- as.data.frame(app_results)
  total_tests <- total_tests + sum(app_results_df$passed) + sum(app_results_df$failed) + sum(app_results_df$skipped)
  total_passed <- total_passed + sum(app_results_df$passed)
  total_failed <- total_failed + sum(app_results_df$failed)
  total_skipped <- total_skipped + sum(app_results_df$skipped)
  total_warnings <- total_warnings + sum(app_results_df$warning)
}

cat(sprintf("|  Total Tests:    %4d                                                |\n", total_tests))
cat(sprintf("|  [PASS] Passed:  %4d                                                |\n", total_passed))
cat(sprintf("|  [FAIL] Failed:  %4d                                                |\n", total_failed))
cat(sprintf("|  [SKIP] Skipped: %4d                                                |\n", total_skipped))
cat(sprintf("|  [WARN] Warnings:%4d                                                |\n", total_warnings))
cat("+----------------------------------------------------------------------+\n")

if (total_failed == 0) {
  cat("|  [PASS] ALL TESTS PASSED!                                          |\n")
} else {
  cat("|  [FAIL] SOME TESTS FAILED - See details above                      |\n")
}
cat("+----------------------------------------------------------------------+\n")

# Return exit code for CI/CD
if (total_failed > 0) {
  quit(status = 1)
}
