# Test lab processing metrics calculations
# Focus on 6-27 N facility case and red bug threshold logic

setwd('c:/Users/datatech/Documents/mmcd_metrics')
source('shared/db_helpers.R')
source('apps/air_sites_simple/air_status_functions_working.R')

cat("=== TESTING LAB METRICS CALCULATIONS ===\n\n")

# Test the specific case: 6-27 N facility
test_date <- as.Date("2025-06-27")
cat("Testing date:", as.character(test_date), "\n")

# Get data for N facility only
data_all <- get_air_sites_data_working(
  analysis_date = test_date,
  facility_filter = "N",
  priority_filter = NULL,
  zone_filter = "All Zones",
  larvae_threshold = 15,
  bti_effect_days_override = NA
)

cat("Total N facility sites on", as.character(test_date), ":", nrow(data_all), "\n\n")

# Check status distribution
cat("=== STATUS DISTRIBUTION ===\n")
print(table(data_all$site_status, useNA = "ifany"))

# Check lab-related data
cat("\n=== LAB STATUS ANALYSIS ===\n")
cat("Sites with samples (sampnum_yr not NA/empty):", 
    sum(!is.na(data_all$sampnum_yr) & data_all$sampnum_yr != "", na.rm = TRUE), "\n")

cat("Sites with lab timestamps:", 
    sum(!is.na(data_all$lab_id_timestamp), na.rm = TRUE), "\n")

cat("Sites still in lab (have sample but no timestamp):", 
    sum(!is.na(data_all$sampnum_yr) & data_all$sampnum_yr != "" & is.na(data_all$lab_id_timestamp), na.rm = TRUE), "\n")

# Run the lab metrics function
lab_metrics <- analyze_lab_processing_metrics(data_all)

cat("\n=== LAB METRICS RESULTS (UPDATED) ===\n")
cat("Total inspected with completed samples:", lab_metrics$total_inspected_with_samples, "\n")
cat("Lab completion rate:", lab_metrics$lab_completion_rate, "\n")
cat("Red bugs found:", lab_metrics$red_bugs_found, "\n")
cat("Red bug detection rate:", lab_metrics$red_bug_detection_rate, "\n")
cat("Samples above threshold:", lab_metrics$samples_above_threshold, "\n")
cat("Samples below threshold:", lab_metrics$samples_below_threshold, "\n")
cat("Samples threshold rate:", lab_metrics$samples_threshold_rate, "\n")

# Detailed analysis of the completion issue - now only completed samples
cat("\n=== DETAILED ANALYSIS (COMPLETED SAMPLES ONLY) ===\n")

# Find sites with completed samples (have timestamps)
sites_with_completed_samples <- data_all[!is.na(data_all$sampnum_yr) & 
                                        data_all$sampnum_yr != "" & 
                                        !is.na(data_all$lab_id_timestamp), ]
cat("Sites with completed samples:", nrow(sites_with_completed_samples), "\n")

if (nrow(sites_with_completed_samples) > 0) {
  # Check inspection dates
  cat("Completed samples with inspection dates:", 
      sum(!is.na(sites_with_completed_samples$last_inspection_date), na.rm = TRUE), "\n")
  
  # This is what the function now uses
  inspected_with_completed <- sites_with_completed_samples[!is.na(sites_with_completed_samples$last_inspection_date), ]
  cat("Inspected sites with completed samples (function input):", nrow(inspected_with_completed), "\n")
  
  if (nrow(inspected_with_completed) > 0) {
    # Show some sample data
    cat("\nSample of completed samples:\n")
    sample_cols <- c("sitecode", "site_status", "sampnum_yr", "redblue", "last_inspection_date", "last_larvae_count")
    print(head(inspected_with_completed[, sample_cols], 5))
  }
}

# Test threshold calculation for ALL samples (not just red bugs)
cat("\n=== SAMPLES THRESHOLD TESTING (ALL SAMPLES) ===\n")
completed_samples <- data_all[!is.na(data_all$sampnum_yr) & 
                             data_all$sampnum_yr != "" & 
                             !is.na(data_all$lab_id_timestamp) &
                             !is.na(data_all$last_inspection_date), ]
cat("Total completed samples:", nrow(completed_samples), "\n")

if (nrow(completed_samples) > 0) {
  cat("Completed samples with larvae counts:\n")
  larvae_data <- completed_samples[!is.na(completed_samples$last_larvae_count), ]
  cat("  - Samples with larvae count data:", nrow(larvae_data), "\n")
  
  if (nrow(larvae_data) > 0) {
    threshold <- 15
    above_threshold <- sum(larvae_data$last_larvae_count >= threshold, na.rm = TRUE)
    below_threshold <- nrow(larvae_data) - above_threshold
    
    cat("  - Above threshold (>= 15):", above_threshold, "\n")
    cat("  - Below threshold (< 15):", below_threshold, "\n")
    cat("  - Threshold rate:", round((above_threshold / nrow(larvae_data)) * 100, 1), "%\n")
    
    # Show larvae count distribution
    cat("\nLarvae count distribution for all samples:\n")
    print(summary(larvae_data$last_larvae_count))
    
    # Show red bug detection rate
    red_samples <- sum(larvae_data$has_red_bugs == 1, na.rm = TRUE)
    cat("\nRed bug detection:\n")
    cat("  - Red bug samples:", red_samples, "\n") 
    cat("  - Total samples:", nrow(larvae_data), "\n")
    cat("  - Red bug detection rate:", round((red_samples / nrow(larvae_data)) * 100, 1), "%\n")
  }
}

# Test with different dates to see pattern
cat("\n=== TESTING MULTIPLE DATES ===\n")
test_dates <- c("2025-06-15", "2025-06-27", "2025-07-15", "2025-08-15")

for (test_d in test_dates) {
  test_date_obj <- as.Date(test_d)
  test_data <- get_air_sites_data_working(
    analysis_date = test_date_obj,
    facility_filter = "N",
    zone_filter = "All Zones"
  )
  
  test_metrics <- analyze_lab_processing_metrics(test_data)
  in_lab_count <- sum(test_data$site_status == "In Lab", na.rm = TRUE)
  
  cat(sprintf("%s: Lab completion=%s, In Lab sites=%d, Total samples=%d\n", 
              test_d, 
              test_metrics$lab_completion_rate,
              in_lab_count,
              test_metrics$total_inspected_with_samples))
}

cat("\n=== TEST COMPLETE ===\n")