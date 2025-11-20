# Investigate the 21-site mismatch between table (699) and chart total (678)
# Find which sites are missing from chart total

library(dplyr)
library(DBI)
library(RPostgres)
source("../../shared/db_helpers.R")
source("data_functions.R")

investigate_site_mismatch <- function() {
  cat("=== INVESTIGATING 21-SITE MISMATCH ===\n")
  cat("Table shows 699 sites, chart total shows 678 sites\n")
  cat("Missing: 699 - 678 = 21 sites\n\n")
  
  simulation_date <- as.Date("2025-06-11")
  
  # Get the raw site details data (used by table)
  table_data <- get_site_details_data(simulation_date = simulation_date) %>%
    filter(facility == "Sr", zone == "1", fosarea == "1904")
  
  cat("Table data analysis:\n")
  cat("Total sites in table:", nrow(table_data), "\n")
  
  # Analyze treatment status distribution
  status_breakdown <- table_data %>%
    mutate(
      prehatch_status_clean = ifelse(is.na(prehatch_status), "no_treatment", prehatch_status),
      has_treatment = !is.na(prehatch_status),
      has_inspdate = !is.na(inspdate)
    ) %>%
    count(prehatch_status_clean, has_treatment, has_inspdate, name = "count")
  
  cat("\nDetailed status breakdown:\n")
  print(status_breakdown)
  
  # Count sites that would be included in chart total
  sites_with_treatment_status <- table_data %>%
    filter(!is.na(prehatch_status)) %>%  # Only sites with treatment status
    nrow()
  
  sites_without_treatment <- table_data %>%
    filter(is.na(prehatch_status)) %>%  # Sites without treatment status
    nrow()
  
  cat("\nTreatment status analysis:\n")
  cat("Sites with treatment status (included in chart):", sites_with_treatment_status, "\n")
  cat("Sites without treatment status (excluded from chart):", sites_without_treatment, "\n")
  cat("Total:", sites_with_treatment_status + sites_without_treatment, "\n")
  
  # Show examples of sites without treatment
  if(sites_without_treatment > 0) {
    cat("\nSample sites without treatment (excluded from chart total):\n")
    no_treatment_sites <- table_data %>%
      filter(is.na(prehatch_status)) %>%
      select(sitecode, facility, zone, fosarea, prehatch, inspdate, prehatch_status) %>%
      head(10)
    
    print(no_treatment_sites)
  }
  
  # Check if all sites have proper gis_sectcode data
  cat("\n=== GIS_SECTCODE DATA ANALYSIS ===\n")
  
  missing_gis_data <- table_data %>%
    filter(is.na(facility) | is.na(zone) | is.na(fosarea)) %>%
    nrow()
  
  cat("Sites missing facility/zone/fosarea data:", missing_gis_data, "\n")
  
  if(missing_gis_data > 0) {
    cat("Sample sites with missing gis_sectcode data:\n")
    missing_samples <- table_data %>%
      filter(is.na(facility) | is.na(zone) | is.na(fosarea)) %>%
      select(sitecode, facility, zone, fosarea, sectcode) %>%
      head(10)
    print(missing_samples)
  }
  
  # Verify chart calculation matches our analysis
  cat("\n=== CHART TOTAL VERIFICATION ===\n")
  
  chart_data <- get_ground_prehatch_data(zone_filter = "1", simulation_date = simulation_date) %>%
    filter(facility == "Sr", zone == "1", fosarea == "1904")
  
  chart_total_calculated <- sum(chart_data$ph_treated_cnt + chart_data$ph_expiring_cnt + chart_data$ph_expired_cnt, na.rm = TRUE)
  
  cat("Chart total from aggregation:", chart_total_calculated, "\n")
  cat("Sites with treatment status:", sites_with_treatment_status, "\n")
  
  if(chart_total_calculated == sites_with_treatment_status) {
    cat("✅ Chart total matches sites with treatment status\n")
  } else {
    cat("❌ Chart total doesn't match - investigation needed\n")
  }
  
  # Root cause analysis
  cat("\n=== ROOT CAUSE ANALYSIS ===\n")
  if(sites_without_treatment == 21) {
    cat("✅ FOUND THE ISSUE: 21 sites have no treatment records\n")
    cat("These sites:\n")
    cat("- Are valid prehatch sites (included in table)\n") 
    cat("- Have no treatment records (excluded from chart total)\n")
    cat("- Still get facility/zone/fosarea from gis_sectcode correctly\n")
    cat("- Should be counted as 'untreated' sites, not ignored\n")
  } else {
    cat("❌ The number of untreated sites (", sites_without_treatment, ") doesn't match the discrepancy (21)\n")
  }
  
  # Recommendation
  cat("\n=== RECOMMENDATION ===\n")
  cat("The chart total should include ALL prehatch sites (699), not just treated ones (678)\n")
  cat("Gray bar should show: treated + expiring + expired + untreated = 699\n")
  cat("Blue bar should show: treated + expiring = 643\n")
  cat("This way the chart total matches the table total\n")
  
  return(list(
    table_total = nrow(table_data),
    treated_sites = sites_with_treatment_status,
    untreated_sites = sites_without_treatment,
    chart_total = chart_total_calculated
  ))
}

# Run the investigation
investigation_results <- investigate_site_mismatch()