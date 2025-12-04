# Test script to verify color functions
# Run with: Rscript test_colors.R

# Source the db_helpers
source("db_helpers.R")

cat("\n=== TESTING COLOR FUNCTIONS ===\n\n")

# Set theme to MMCD
options(mmcd.color.theme = "MMCD")

cat("Current theme:", getOption("mmcd.color.theme"), "\n\n")

# Test get_status_colors
cat("--- get_status_colors() ---\n")
status_colors <- get_status_colors()
print(status_colors)
cat("\n")

# Test get_status_color_map
cat("--- get_status_color_map() ---\n")
status_color_map <- get_status_color_map()
print(names(status_color_map))
cat("\n")

# Test each status individually
statuses_to_test <- c("Unknown", "Needs ID", "Inspected", "Needs Treatment", "Active Treatment")

cat("--- Testing each status color ---\n")
for (status in statuses_to_test) {
  color <- status_color_map[[status]]
  cat(sprintf("%-20s -> %s\n", status, 
              if(is.null(color) || is.na(color)) "NULL/NA" else color))
}

cat("\n--- All status_color_map entries ---\n")
for (name in names(status_color_map)) {
  color <- status_color_map[[name]]
  cat(sprintf("%-20s -> %s\n", name, 
              if(is.null(color) || is.na(color)) "NULL/NA" else color))
}

cat("\n=== TEST COMPLETE ===\n")
