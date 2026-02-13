# =============================================================================
# STUB DATA FOR CONTROL EFFICACY TESTING
# =============================================================================
# Stub data for control_efficacy app testing
# Provides realistic control checkback scenarios with known outcomes
# Based on actual database schema from mmcd_data
# =============================================================================

#' Create stub treatment data with known control scenarios
#' Includes cases where treatment date <= pre-inspection date (controls)
#' and cases where treatment date > pre-inspection date (valid checkbacks)
get_stub_control_treatments <- function() {
  data.frame(
    pkey_pg = 1001:1010,
    sitecode = paste0("TEST-", sprintf("%03d", 1:10)),
    inspdate = as.Date(c(
      "2024-05-15", "2024-05-20", "2024-06-01", "2024-06-10", "2024-06-15",  # Valid treatments
      "2024-05-10", "2024-05-25", "2024-06-05", "2024-06-12", "2024-06-20"   # Control treatments (before pre-inspections)
    )),
    insptime = as.POSIXct(paste(c(
      "2024-05-15", "2024-05-20", "2024-06-01", "2024-06-10", "2024-06-15",
      "2024-05-10", "2024-05-25", "2024-06-05", "2024-06-12", "2024-06-20"
    ), "10:00:00")),
    facility = rep("N", 10),
    action = rep("A", 10),  # Air treatments
    matcode = rep("19", 10),  # BTI
    mattype = rep("Bti_Granule", 10),
    numdip = rep(5, 10),
    sampnum_yr = NA_character_,
    presamp_yr = paste0("24-", 1001:1010),
    posttrt_p = NA_character_,
    acres = rep(1.0, 10),
    stringsAsFactors = FALSE
  )
}

#' Create stub checkback data that corresponds to treatment data
get_stub_control_checkbacks <- function() {
  data.frame(
    pkey_pg = 2001:2010,
    sitecode = paste0("TEST-", sprintf("%03d", 1:10)),
    inspdate = as.Date(c(
      "2024-05-25", "2024-05-30", "2024-06-11", "2024-06-20", "2024-06-25",  # Valid checkbacks (after treatments)
      "2024-05-20", "2024-06-04", "2024-06-15", "2024-06-22", "2024-06-30"   # Control checkbacks (treatments before pre-inspection)
    )),
    insptime = as.POSIXct(paste(c(
      "2024-05-25", "2024-05-30", "2024-06-11", "2024-06-20", "2024-06-25",
      "2024-05-20", "2024-06-04", "2024-06-15", "2024-06-22", "2024-06-30"
    ), "14:00:00")),
    facility = rep("N", 10),
    action = rep("4", 10),  # Post-treatment inspections
    numdip = c(2, 3, 1, 4, 2, 8, 7, 5, 6, 9),  # Post-treatment dip counts
    sampnum_yr = paste0("24-", 2001:2010),
    posttrt_p = rep("t", 10),  # Marked as post-treatment
    stringsAsFactors = FALSE
  )
}

#' Create stub pre-inspection data
get_stub_control_pre_inspections <- function() {
  data.frame(
    pkey_pg = 3001:3010,
    sitecode = paste0("TEST-", sprintf("%03d", 1:10)),
    inspdate = as.Date(c(
      "2024-05-12", "2024-05-17", "2024-05-29", "2024-06-07", "2024-06-12",  # Pre-inspections for valid treatments
      "2024-05-15", "2024-05-28", "2024-06-08", "2024-06-15", "2024-06-23"   # Pre-inspections for control treatments
    )),
    insptime = as.POSIXct(paste(c(
      "2024-05-12", "2024-05-17", "2024-05-29", "2024-06-07", "2024-06-12",
      "2024-05-15", "2024-05-28", "2024-06-08", "2024-06-15", "2024-06-23"
    ), "08:00:00")),
    facility = rep("N", 10),
    action = rep("4", 10),  # Pre-treatment inspections
    numdip = c(10, 15, 8, 12, 9, 20, 18, 14, 16, 22),  # Pre-treatment dip counts
    sampnum_yr = paste0("24-", 1001:1010),
    posttrt_p = NA_character_,  # NOT marked as post-treatment
    stringsAsFactors = FALSE
  )
}

#' Create stub species data for genus percentage testing
get_stub_control_species <- function() {
  # Create genus data for all samples (pre and post)
  samples <- c(paste0("24-", 1001:1010), paste0("24-", 2001:2010))
  species_list <- c()
  
  for (i in 1:length(samples)) {
    # Alternate between Aedes-dominant and Culex-dominant samples
    if (i %% 2 == 1) {
      # Aedes dominant
      species_list <- rbind(species_list, data.frame(
        sampnum_yr = samples[i],
        genus = "Aedes",
        total_pct = 80,
        stringsAsFactors = FALSE
      ))
      species_list <- rbind(species_list, data.frame(
        sampnum_yr = samples[i], 
        genus = "Culex",
        total_pct = 20,
        stringsAsFactors = FALSE
      ))
    } else {
      # Culex dominant
      species_list <- rbind(species_list, data.frame(
        sampnum_yr = samples[i],
        genus = "Aedes", 
        total_pct = 30,
        stringsAsFactors = FALSE
      ))
      species_list <- rbind(species_list, data.frame(
        sampnum_yr = samples[i],
        genus = "Culex",
        total_pct = 70,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(species_list)
}

#' Create stub material filter data
get_stub_material_codes <- function() {
  list(
    bti = c("19", "20", "21"),
    methoprene = c("08", "10", "12"), 
    spinosad = c("N1", "N3", "N4")
  )
}

#' Create stub efficacy data for testing percent reduction calculations
get_stub_efficacy_data <- function() {
  # Create matched treatment-checkback data for efficacy testing
  data.frame(
    sitecode = paste0("TEST-", sprintf("%03d", 1:10)),
    facility = rep("N", 10),
    year = rep(2024, 10),
    season = rep(c("Spring", "Summer"), 5),
    genus = rep(c("Aedes", "Culex"), 5),
    trt_type = rep("Air", 10),
    trt_matcode = rep("19", 10),
    trt_date = as.Date(c(
      "2024-05-15", "2024-05-20", "2024-06-01", "2024-06-10", "2024-06-15",
      "2024-05-10", "2024-05-25", "2024-06-05", "2024-06-12", "2024-06-20"
    )),
    pre_date = as.Date(c(
      "2024-05-12", "2024-05-17", "2024-05-29", "2024-06-07", "2024-06-12",
      "2024-05-15", "2024-05-28", "2024-06-08", "2024-06-15", "2024-06-23"
    )),
    post_date = as.Date(c(
      "2024-05-25", "2024-05-30", "2024-06-11", "2024-06-20", "2024-06-25",
      "2024-05-20", "2024-06-04", "2024-06-15", "2024-06-22", "2024-06-30"
    )),
    days_from_trt = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10),
    pre_numdip = c(10, 15, 8, 12, 9, 20, 18, 14, 16, 22),
    post_numdip = c(2, 3, 1, 4, 2, 8, 7, 5, 6, 9),
    pre_genus_pct = c(80, 30, 80, 30, 80, 30, 80, 30, 80, 30),
    post_genus_pct = c(75, 25, 85, 35, 70, 40, 90, 20, 65, 45),
    pre_genus_dips = c(8.0, 4.5, 6.4, 3.6, 7.2, 6.0, 14.4, 4.2, 12.8, 6.6),
    post_genus_dips = c(1.5, 0.75, 0.85, 1.4, 1.4, 3.2, 6.3, 1.0, 3.9, 4.05),
    acres = rep(1.0, 10),
    is_control = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}