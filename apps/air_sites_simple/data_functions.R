# Air Sites Data Functions
# Data retrieval, processing, and analysis functions

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(DBI)
})

# Get air sites data with filtering and status logic
get_air_sites_data <- function(analysis_date = Sys.Date(), facility_filter = NULL, priority_filter = NULL, zone_filter = NULL, larvae_threshold = 2, bti_effect_days_override = NULL, include_archive = FALSE, start_year = NULL, end_year = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Build filter conditions with safer checks
    # Treat NULL, empty, or "all" as no filter
    facility_condition <- ""
    if (!is.null(facility_filter) && length(facility_filter) > 0 && all(!is.na(facility_filter)) && !"all" %in% facility_filter) {
      facility_list <- paste(sprintf("'%s'", facility_filter), collapse=", ")
      facility_condition <- sprintf("AND b.facility IN (%s)", facility_list)
    }
    
    priority_condition <- ""
    if (!is.null(priority_filter) && length(priority_filter) > 0 && all(!is.na(priority_filter)) && !"all" %in% priority_filter) {
      priority_list <- paste(sprintf("'%s'", priority_filter), collapse=", ")
      priority_condition <- sprintf("AND b.priority IN (%s)", priority_list)
    }
    
    zone_condition <- ""
    if (!is.null(zone_filter) && length(zone_filter) > 0 && !is.na(zone_filter)) {
      # Handle the 4 clean zone filter options from UI (now single selection)
      actual_zones <- c()
      if (zone_filter == "P1") {
        actual_zones <- "1"
      } else if (zone_filter == "P2") {
        actual_zones <- "2"  
      } else if (zone_filter == "P1 + P2 Combined" || zone_filter == "P1 and P2 Separate") {
        # Include all zones - no filter needed
        actual_zones <- c("1", "2")
      }
      
      # Only add condition if we have specific zones (not combined options)
      if (length(actual_zones) == 1) {
        zone_condition <- sprintf("AND g.zone = '%s'", actual_zones)
      }
    }

    # Build BTI effect days override condition
    bti_effect_days_sql <- if (!is.null(bti_effect_days_override) && !is.na(bti_effect_days_override) && bti_effect_days_override > 0) {
      sprintf("CASE WHEN rt.mattype = 'Bti_gran' THEN %d WHEN mt.effect_days IS NOT NULL THEN mt.effect_days ELSE 14 END", bti_effect_days_override)
    } else {
      "COALESCE(mt.effect_days, 14)"
    }

    # Enhanced query with inspection, treatment, and lab sample data
    # Determine table names based on archive flag
    inspection_table <- if (include_archive) "dblarv_insptrt_archive" else "dblarv_insptrt_current"
    sample_table <- if (include_archive) "dblarv_sample_archive" else "dblarv_sample_current"
    
    # Debug logging for archive queries
    if (include_archive) {
      cat("Using archive tables - Inspection:", inspection_table, "Sample:", sample_table, "\n")
      cat("Year range:", start_year, "to", end_year, "\n")
    }
    
    # Add year filtering for historical analysis
    year_condition_inspection <- ""
    year_condition_treatment <- ""
    if (include_archive && !is.null(start_year) && !is.null(end_year)) {
      year_condition_inspection <- sprintf("AND EXTRACT(YEAR FROM i.inspdate) BETWEEN %d AND %d", start_year, end_year)
      year_condition_treatment <- sprintf("AND EXTRACT(YEAR FROM t.inspdate) BETWEEN %d AND %d", start_year, end_year)
    }
    
    query <- sprintf("
      WITH ActiveAirSites AS (
        SELECT 
          b.facility,
          b.sitecode,
          b.acres,
          b.priority,
          g.zone,
          ST_X(ST_Centroid(ST_Transform(b.geom, 4326))) as longitude,
          ST_Y(ST_Centroid(ST_Transform(b.geom, 4326))) as latitude
        FROM loc_breeding_sites b
        LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(b.sitecode, 7)
        WHERE (b.enddate IS NULL OR b.enddate > '%s')
          AND b.air_gnd = 'A'
          AND b.geom IS NOT NULL
          %s
          %s
          %s
      ),
      
      -- Get most recent inspections (actions 2, 4)
      RecentInspections AS (
        SELECT 
          i.sitecode,
          i.inspdate as last_inspection_date,
          i.numdip,
          i.sampnum_yr,
          ROW_NUMBER() OVER (PARTITION BY i.sitecode ORDER BY i.inspdate DESC) as rn
        FROM %s i
        WHERE i.inspdate <= '%s'::date
          AND i.action IN ('2', '4')  -- Inspection actions only
          AND i.sitecode IN (SELECT sitecode FROM ActiveAirSites)
          %s
      ),
      
      -- Get most recent treatments (actions 3, A, D)
      RecentTreatments AS (
        SELECT 
          t.sitecode,
          t.inspdate as last_treatment_date,
          t.matcode,
          t.mattype,
          ROW_NUMBER() OVER (PARTITION BY t.sitecode ORDER BY t.inspdate DESC) as rn
        FROM %s t
        WHERE t.inspdate <= '%s'::date
          AND t.action IN ('3', 'A', 'D')  -- Treatment actions only
          AND t.matcode IS NOT NULL
          AND t.matcode != ''
          AND t.sitecode IN (SELECT sitecode FROM ActiveAirSites)
          %s
      ),
      
      -- Get lab sample data for red/blue bug determination (AIR samples only)
      LabSampleData AS (
        SELECT 
          ls.sampnum_yr,
          ls.redblue,
          ls.missing,
          -- Count red bugs for this sample
          CASE 
            WHEN ls.redblue = 'R' THEN 1
            ELSE 0
          END as has_red_bugs
        FROM %s ls
        WHERE ls.sampnum_yr IS NOT NULL
          AND ls.missing = FALSE
          AND ls.form_type = 'AIR'
      ),
      
      -- Get treatment material details and effect days
      TreatmentInfo AS (
        SELECT 
          rt.sitecode,
          rt.last_treatment_date,
          rt.matcode,
          rt.mattype,
          -- Create better material name with dosage details
          CASE 
            WHEN mt.tdose IS NOT NULL AND mt.unit IS NOT NULL AND mt.area IS NOT NULL
            THEN CONCAT(rt.mattype, ' ', mt.tdose, ' ', mt.unit, ' per ', mt.area)
            ELSE rt.mattype
          END as last_treatment_material,
          -- Calculate treatment expiry based on material effect days
          CASE 
            WHEN (%s) IS NOT NULL
            THEN rt.last_treatment_date + INTERVAL '1 day' * (%s)
            ELSE NULL
          END as treatment_expiry
        FROM RecentTreatments rt
        LEFT JOIN mattype_list_targetdose mt ON rt.matcode = mt.matcode
        WHERE rt.rn = 1
      ),
      
      -- Get inspection data
      InspectionInfo AS (
        SELECT 
          ri.sitecode,
          ri.last_inspection_date,
          ri.numdip,
          ri.sampnum_yr,
          ls.redblue,
          ls.has_red_bugs,
          ls.missing
        FROM RecentInspections ri
        LEFT JOIN LabSampleData ls ON ri.sampnum_yr = ls.sampnum_yr
        WHERE ri.rn = 1
      )
      
      SELECT 
        a.sitecode,
        a.facility,
        a.priority,
        a.zone,
        a.acres,
        a.longitude,
        a.latitude,
        t.last_treatment_date,
        t.matcode,
        t.last_treatment_material,
        t.treatment_expiry,
        i.last_inspection_date,
        i.numdip as last_larvae_count,
        i.sampnum_yr,
        i.redblue,
        i.has_red_bugs,
        i.missing
      FROM ActiveAirSites a
      LEFT JOIN TreatmentInfo t ON a.sitecode = t.sitecode
      LEFT JOIN InspectionInfo i ON a.sitecode = i.sitecode
      ORDER BY a.sitecode
    ", analysis_date, facility_condition, priority_condition, zone_condition,
       inspection_table, analysis_date, year_condition_inspection,
       inspection_table, analysis_date, year_condition_treatment,
       sample_table, bti_effect_days_sql, bti_effect_days_sql)
    
    result <- dbGetQuery(con, query)
    
    # Debug logging for archive queries
    if (include_archive) {
      cat("Archive query returned", nrow(result), "rows\n")
      if (nrow(result) > 0) {
        cat("Date range in results:", min(result$last_inspection_date, na.rm = TRUE), "to", 
            max(result$last_inspection_date, na.rm = TRUE), "\n")
      }
    }
    
    # Apply enhanced status logic in R
    if (nrow(result) > 0) {
      result <- apply_site_status_logic(result, analysis_date, larvae_threshold)
    }
    
    return(result)
    
  }, error = function(e) {
    cat("Error in get_air_sites_data_enhanced:", e$message, "\n")
    return(data.frame())
  }, finally = {
    if (!is.null(con)) dbDisconnect(con)
  })
}

# Apply site status logic with larvae threshold and lab integration
apply_site_status_logic <- function(data, analysis_date, larvae_threshold = 2) {
    analysis_date <- as.Date(analysis_date)
  
  # Initialize status as Unknown
  data$site_status <- "Unknown"
  
  for (i in seq_len(nrow(data))) {
    site <- data[i, , drop = FALSE]
    
    last_treatment_date <- NULL
    if (!is.null(site$last_treatment_date) && length(site$last_treatment_date) > 0) {
      if (!is.na(site$last_treatment_date[1])) {
        tryCatch({
          last_treatment_date <- as.Date(site$last_treatment_date[1])
        }, error = function(e) {
          last_treatment_date <<- NULL
        })
      }
    }
    
    treatment_expiry <- NULL
    if (!is.null(site$treatment_expiry) && length(site$treatment_expiry) > 0) {
      if (!is.na(site$treatment_expiry[1])) {
        tryCatch({
          treatment_expiry <- as.Date(site$treatment_expiry[1])
        }, error = function(e) {
          treatment_expiry <<- NULL
        })
      }
    }
    
    last_inspection_date <- NULL
    if (!is.null(site$last_inspection_date) && length(site$last_inspection_date) > 0) {
      if (!is.na(site$last_inspection_date[1])) {
        tryCatch({
          last_inspection_date <- as.Date(site$last_inspection_date[1])
        }, error = function(e) {
          last_inspection_date <<- NULL
        })
      }
    }
    
    # Check if treatment is currently active
    treatment_active <- FALSE
    if (!is.null(treatment_expiry) && length(treatment_expiry) > 0) {
      tryCatch({
        treatment_active <- !is.na(treatment_expiry) && treatment_expiry > analysis_date
      }, error = function(e) {
        treatment_active <<- FALSE
      })
    }
    
    # Priority 1: Active Treatment (cannot be overridden by re-inspection)
    if (treatment_active) {
      data$site_status[i] <- "Active Treatment"
      next
    }
    
    # Check inspection status if we have inspection data
    if (!is.null(last_inspection_date)) {
      # Check if there was a treatment after the last inspection
      treatment_after_inspection <- FALSE
      last_treatment_date <- NULL
      if (!is.null(site$last_treatment_date) && length(site$last_treatment_date) > 0) {
        if (!is.na(site$last_treatment_date[1])) {
          tryCatch({
            last_treatment_date <- as.Date(site$last_treatment_date[1])
            if (!is.null(last_treatment_date) && !is.null(last_inspection_date)) {
              treatment_after_inspection <- last_treatment_date > last_inspection_date
            }
          }, error = function(e) {
            last_treatment_date <<- NULL
          })
        }
      }
      
      # If treatment occurred after inspection and has now expired, 
      # ignore the old inspection data and default to Unknown
      if (treatment_after_inspection && !treatment_active) {
        data$site_status[i] <- "Unknown"
        next
      }
      
      larvae_count <- 0
      if (!is.null(site$last_larvae_count) && length(site$last_larvae_count) > 0) {
        if (!is.na(site$last_larvae_count[1])) {
          tryCatch({
            larvae_count <- as.numeric(site$last_larvae_count[1])
            if (is.na(larvae_count)) larvae_count <- 0
          }, error = function(e) {
            larvae_count <<- 0
          })
        }
      }
      days_since_inspection <- as.numeric(analysis_date - last_inspection_date)
      
      # If inspection is within 7 days
      if (days_since_inspection <= 7) {
        if (larvae_count >= larvae_threshold) {
          # Check for lab sample
          has_sample <- FALSE
          if (!is.null(site$sampnum_yr) && length(site$sampnum_yr) > 0) {
            if (!is.na(site$sampnum_yr[1]) && site$sampnum_yr[1] != "") {
              has_sample <- TRUE
            }
          }
          
          if (has_sample) {
            # Sample was sent to lab - check if missing = FALSE (results available)
            has_lab_results <- FALSE
            if (!is.null(site$missing) && length(site$missing) > 0) {
              if (!is.na(site$missing[1])) {
                tryCatch({
                  has_lab_results <- !site$missing[1]  # missing = FALSE means results available
                }, error = function(e) {
                  has_lab_results <<- FALSE
                })
              }
            }
            
            if (has_lab_results) {
              # Lab results are available, check for red bugs
              has_red_bugs <- FALSE
              if (!is.null(site$has_red_bugs) && length(site$has_red_bugs) > 0) {
                if (!is.na(site$has_red_bugs[1])) {
                  tryCatch({
                    red_bug_value <- as.numeric(site$has_red_bugs[1])
                    has_red_bugs <- !is.na(red_bug_value) && red_bug_value == 1
                  }, error = function(e) {
                    has_red_bugs <<- FALSE
                  })
                }
              }
              
              if (has_red_bugs) {
                # Red bugs found - needs treatment
                data$site_status[i] <- "Needs Treatment"
              } else {
                # No red bugs or only blue bugs - considered inspected (under threshold)
                data$site_status[i] <- "Inspected"
              }
            } else {
              # Sample sent but no lab results yet (missing = TRUE) - needs ID
              data$site_status[i] <- "Needs ID"
            }
          } else {
            # High larvae count but no sample sent - direct to needs treatment (fallback)
            data$site_status[i] <- "Needs Treatment"
          }
        } else {
          # Low larvae count - under threshold
          data$site_status[i] <- "Inspected"
        }
      }
      # If inspection is older than 7 days, status expires back to Unknown
    }
  }
  
  # Format dates for display
  data$last_treatment_date_display <- ifelse(
    is.na(data$last_treatment_date), 
    "None", 
    format(as.Date(data$last_treatment_date), "%Y-%m-%d")
  )
  
  data$last_inspection_date_display <- ifelse(
    is.na(data$last_inspection_date), 
    "None", 
    format(as.Date(data$last_inspection_date), "%Y-%m-%d")
  )
  
  data$lab_status_display <- ifelse(
    is.na(data$sampnum_yr), 
    "No Sample",
    ifelse(
      is.na(data$missing) | data$missing == TRUE,
      "Needs ID",
      ifelse(
        !is.na(data$has_red_bugs) & data$has_red_bugs == 1,
        "Red Bugs Found",
        "No Red Bugs"
      )
    )
  )
  
  return(data)
}

# Material filter function is now provided by db_helpers.R
# Use get_material_choices() instead of get_treatment_materials()

create_treatment_efficiency_metrics <- function(data, metric_type = "sites") {
  if (nrow(data) == 0) {
    return(list(
      total_sites_needing_action = 0,
      sites_receiving_treatment = 0,
      treatment_efficiency = "0%",
      inspection_coverage = "0%"
    ))
  }
  
  # Calculate efficiency metrics based on metric type (sites or acres)
  if (metric_type == "acres") {
    # Acres-based calculations
    total_acres <- sum(data$acres, na.rm = TRUE)
    acres_needing_treatment <- sum(data$acres[data$site_status == "Needs Treatment"], na.rm = TRUE)
    acres_receiving_treatment <- sum(data$acres[data$site_status == "Active Treatment"], na.rm = TRUE)
    acres_inspected <- sum(data$acres[data$site_status == "Inspected"], na.rm = TRUE)
    acres_in_lab <- sum(data$acres[data$site_status == "Needs ID"], na.rm = TRUE)
    
    # Acres that have been inspected, in lab, or treated
    acres_with_action <- acres_inspected + acres_in_lab + acres_needing_treatment + acres_receiving_treatment
    
    # Treatment efficiency: active treatment acres / (needs treatment + active treatment)
    acres_requiring_treatment <- acres_needing_treatment + acres_receiving_treatment
    treatment_efficiency <- if (acres_requiring_treatment > 0) {
      round((acres_receiving_treatment / acres_requiring_treatment) * 100, 1)
    } else {
      0
    }
    
    # Treatment rate: needs treatment / (needs treatment + active treatment) - shows % still needing treatment
    treatment_rate <- if (acres_requiring_treatment > 0) {
      round((acres_needing_treatment / acres_requiring_treatment) * 100, 1)
    } else {
      0
    }
    
    # Inspection coverage: (inspected + in lab + needs treatment + active treatment) / total acres
    inspection_coverage <- if (total_acres > 0) {
      round((acres_with_action / total_acres) * 100, 1)
    } else {
      0
    }
    
    return(list(
      total_sites_needing_action = round(acres_requiring_treatment, 1),
      sites_receiving_treatment = round(acres_receiving_treatment, 1),
      treatment_efficiency = paste0(treatment_efficiency, "%"),
      treatment_rate = paste0(treatment_rate, "%"),
      inspection_coverage = paste0(inspection_coverage, "%")
    ))
  } else {
    # Sites-based calculations (original logic)
    total_sites <- nrow(data)
    sites_needing_treatment <- sum(data$site_status == "Needs Treatment", na.rm = TRUE)
    sites_receiving_treatment <- sum(data$site_status == "Active Treatment", na.rm = TRUE)
    sites_inspected <- sum(data$site_status == "Inspected", na.rm = TRUE)
    sites_in_lab <- sum(data$site_status == "Needs ID", na.rm = TRUE)
    
    # Sites that have been inspected, in lab, or treated
    sites_with_action <- sites_inspected + sites_in_lab + sites_needing_treatment + sites_receiving_treatment
    
    # Treatment efficiency: active treatments / (needs treatment + active treatment)
    sites_requiring_treatment <- sites_needing_treatment + sites_receiving_treatment
    treatment_efficiency <- if (sites_requiring_treatment > 0) {
      round((sites_receiving_treatment / sites_requiring_treatment) * 100, 1)
    } else {
      0
    }
    
    # Treatment rate: needs treatment / (needs treatment + active treatment) - shows % still needing treatment
    treatment_rate <- if (sites_requiring_treatment > 0) {
      round((sites_needing_treatment / sites_requiring_treatment) * 100, 1)
    } else {
      0
    }
    
    # Inspection coverage: (inspected + in lab + needs treatment + active treatment) / total sites
    inspection_coverage <- if (total_sites > 0) {
      round((sites_with_action / total_sites) * 100, 1)
    } else {
      0
    }
    
    return(list(
      total_sites_needing_action = sites_requiring_treatment,
      sites_receiving_treatment = sites_receiving_treatment,
      treatment_efficiency = paste0(treatment_efficiency, "%"),
      treatment_rate = paste0(treatment_rate, "%"),
      inspection_coverage = paste0(inspection_coverage, "%")
    ))
  }
}


# Analyze lab processing metrics for inspected sites (AIR samples only)
analyze_lab_processing_metrics <- function(site_data) {
  if (nrow(site_data) == 0) {
    return(list(
      total_inspected_with_samples = 0,
      sites_in_lab = 0,
      red_bugs_found = 0,
      samples_above_threshold = 0,
      samples_below_threshold = 0,
      red_bug_detection_rate = "0%"
    ))
  }
  
  # Find all sites that have been inspected and have AIR samples (including both completed and pending)
  all_inspected_with_samples <- site_data[
    !is.na(site_data$sampnum_yr) & 
    site_data$sampnum_yr != "" & 
    !is.na(site_data$last_inspection_date), 
  ]
  
  total_all_samples <- nrow(all_inspected_with_samples)
  
  if (total_all_samples == 0) {
    return(list(
      total_inspected_with_samples = 0,
      sites_in_lab = 0,
      red_bugs_found = 0,
      samples_above_threshold = 0,
      samples_below_threshold = 0,
      red_bug_detection_rate = "0%"
    ))
  }
  
  # Find completed samples (missing = FALSE means results available)
  completed_samples <- all_inspected_with_samples[
    !is.na(all_inspected_with_samples$missing) & 
    all_inspected_with_samples$missing == FALSE, 
  ]
  total_completed_samples <- nrow(completed_samples)
  
  # Sites needing ID: samples without completed timestamps (missing == TRUE)
  sites_in_lab <- total_all_samples - total_completed_samples

  # For other metrics, only use completed samples (those with timestamps)
  if (total_completed_samples == 0) {
    return(list(
      total_inspected_with_samples = total_all_samples,
      sites_in_lab = sites_in_lab,
      red_bugs_found = 0,
      samples_above_threshold = 0,
      samples_below_threshold = 0,
      red_bug_detection_rate = "0%"
    ))
  }
  
  # Count sites with red bugs (has_red_bugs == 1) - only from completed samples
  red_bugs_found <- sum(completed_samples$has_red_bugs == 1, na.rm = TRUE)
  
  # Red bug detection rate: red bugs found / total completed samples
  red_bug_detection_rate <- if (total_completed_samples > 0) {
    round((red_bugs_found / total_completed_samples) * 100, 1)
  } else {
    0
  }
  
  # Count ALL completed samples above vs below larvae threshold (regardless of red/blue bugs)
  larvae_threshold <- 15  # This should match the threshold used in status logic
  samples_above_threshold <- sum(
    !is.na(completed_samples$last_larvae_count) & 
    completed_samples$last_larvae_count >= larvae_threshold, 
    na.rm = TRUE
  )
  samples_below_threshold <- total_completed_samples - samples_above_threshold
  
  return(list(
    total_inspected_with_samples = total_completed_samples,  # Only completed samples with timestamps
    sites_in_lab = sites_in_lab,
    red_bugs_found = red_bugs_found,
    samples_above_threshold = samples_above_threshold,
    samples_below_threshold = samples_below_threshold,
    red_bug_detection_rate = paste0(red_bug_detection_rate, "%")
  ))
}
