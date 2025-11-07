# Simple test script to demonstrate 14-day persistence logic
# This can be run independently to test the new functionality

# Source the functions
source("flow_testing_functions.R")

# Run the 14-day persistence test
cat("Testing 14-Day 'Needs Inspection' Persistence Logic\n")
cat("=" %>% rep(50) %>% paste(collapse=""), "\n\n")

# Test the validate_business_logic function
cat("BUSINESS LOGIC VALIDATION RESULTS:\n")
cat("-" %>% rep(40) %>% paste(collapse=""), "\n")
validation_results <- validate_business_logic()
cat(validation_results)
cat("\n\n")

# Test the 14-day persistence function
cat("14-DAY PERSISTENCE TEST RESULTS:\n") 
cat("-" %>% rep(40) %>% paste(collapse=""), "\n")
persistence_results <- test_14_day_persistence()
cat(persistence_results)
cat("\n\n")

# Test synthetic data generation
cat("SYNTHETIC DATA GENERATION TEST:\n")
cat("-" %>% rep(40) %>% paste(collapse=""), "\n")
synthetic_data <- create_synthetic_flow_data(total_sites = 10, analysis_date = Sys.Date())
cat("Generated", nrow(synthetic_data), "test sites\n")
cat("Status distribution:\n")
status_summary <- table(synthetic_data$site_status)
for (status in names(status_summary)) {
  cat("  ", status, ": ", status_summary[status], "\n")
}

cat("\nSample sites:\n")
print(synthetic_data[1:5, c("sitecode", "site_status", "has_triggering_rainfall", 
                           "last_inspection_date", "last_larvae_count")])

cat("\n\nTest completed successfully! ✓\n")