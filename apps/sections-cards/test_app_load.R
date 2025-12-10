# Quick test to verify app can load
# Run with: & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" test_app_load.R

cat("\n=== Testing Section Cards App ===\n\n")

# Test 1: Check if required packages are available
cat("1. Checking required packages...\n")
required_packages <- c("shiny", "DBI", "RPostgres", "dplyr", "tidyr", 
                       "ggplot2", "lubridate", "DT", "leaflet", "sf")

missing_packages <- c()
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  cat("✗ Missing packages:", paste(missing_packages, collapse = ", "), "\n")
  quit(status = 1)
} else {
  cat("✓ All required packages available\n\n")
}

# Test 2: Check if files exist
cat("2. Checking app files...\n")
setwd("c:/Users/datatech/Documents/mmcd_metrics/apps/sections-cards")

files_to_check <- c(
  "app.R",
  "ui_helper.R",
  "data_functions.R",
  "display_functions.R",
  "../../shared/db_helpers.R"
)

for (file in files_to_check) {
  if (!file.exists(file)) {
    cat("✗ Missing file:", file, "\n")
    quit(status = 1)
  }
}
cat("✓ All app files present\n\n")

# Test 3: Try to source the files
cat("3. Testing file syntax...\n")

tryCatch({
  source("../../shared/db_helpers.R")
  cat("✓ db_helpers.R loaded\n")
}, error = function(e) {
  cat("✗ Error loading db_helpers.R:", e$message, "\n")
  quit(status = 1)
})

tryCatch({
  source("ui_helper.R")
  cat("✓ ui_helper.R loaded\n")
}, error = function(e) {
  cat("✗ Error loading ui_helper.R:", e$message, "\n")
  quit(status = 1)
})

tryCatch({
  source("data_functions.R")
  cat("✓ data_functions.R loaded\n")
}, error = function(e) {
  cat("✗ Error loading data_functions.R:", e$message, "\n")
  quit(status = 1)
})

tryCatch({
  source("display_functions.R")
  cat("✓ display_functions.R loaded\n")
}, error = function(e) {
  cat("✗ Error loading display_functions.R:", e$message, "\n")
  quit(status = 1)
})

cat("\n✓ All tests passed!\n")
cat("\nTo run the app:\n")
cat("cd c:/Users/datatech/Documents/mmcd_metrics/apps/sections-cards\n")
cat("& \"C:\\Program Files\\R\\R-4.5.2\\bin\\R.exe\" -e \"shiny::runApp(port=3838, host='127.0.0.1')\"\n")
