# Stub data for breeding sites and drone-related tables
# Based on actual database schema from mmcd_data
# Tables: loc_breeding_sites, locpt_air_site_dip_spots

#' Get stub data for loc_breeding_sites
#' @return data.frame with realistic breeding site data
get_stub_loc_breeding_sites <- function() {
  data.frame(
    gid = c(1064256L, 893905L, 893907L, 893908L, 893909L),
    sitecode = c("021423-014", "272431-096", "273221-020", "020207-001", "700407-010"),
    acres = c(0.00, 0.16, 0.17, 0.50, 0.25),
    type = c("1.1", "1.2", "2.1", "1.1", "2.2"),
    air_gnd = c("G", "G", "G", "D", "D"),
    coq_pert = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    culex = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    spr_aedes = c(NA_character_, "Y", NA_character_, NA_character_, "Y"),
    priority = c(NA_character_, "GREEN", "GREEN", "RED", "YELLOW"),
    prehatch = c(NA_character_, "PREHATCH", "PREHATCH", NA_character_, "PREHATCH"),
    sample = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    remarks = c(NA_character_, "", "", "Main site", ""),
    startdate = as.POSIXct(c("2025-07-23 14:13:42", "2024-04-09 05:00:01", "2024-04-09 05:00:01",
                             "2023-05-15 10:00:00", "2024-03-20 08:30:00")),
    startby = c("0237", "2710", "2708", "0240", "7016"),
    enddate = as.POSIXct(c(NA, "2025-04-15 05:00:00", "2025-04-15 05:00:00", NA, NA)),
    endby = c(NA_character_, "2401", "2401", NA_character_, NA_character_),
    facility = c("N", "Wm", "Wm", "N", "Sj"),
    elev_thresh = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    partialtrt = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    perimacres = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
    photo = c("breedingsites/1000000128.jpg", NA_character_, NA_character_, NA_character_, NA_character_),
    drone = c("N", NA_character_, NA_character_, "Y", "M"),
    stringsAsFactors = FALSE
  )
}

#' Get stub data for locpt_air_site_dip_spots
#' @return data.frame with realistic dip spot data
get_stub_locpt_air_site_dip_spots <- function() {
  data.frame(
    pkey = c(34L, 67L, 100L, 101L, 133L),
    emp_num = c("6258", "6258", "0245", "0245", "0245"),
    facility = c("E", "E", "N", "N", "N"),
    fos = c("8202", "8206", "0203", "0203", "0203"),
    drawnby = c("6258", "6258", "0245", "0245", "0245"),
    datedrawn = as.POSIXct(c("2022-04-19 17:53:00", "2022-04-21 16:19:21", "2022-04-25 13:12:27",
                             "2022-04-25 13:13:52", "2022-04-25 13:46:40")),
    label = c("Canary grass pockets", "Dip spot", "Air Site #50 DipSpot", 
              "Air Site #50 DipSpot", "Air Site #002 DipSpo"),
    comment = c("", "Maybe make ground.", "Easy access from Amazon road and Driveway 23136",
                "Easy access from Ganges Road.", "Easily Accessed off 235th Culdesac."),
    photo = c(NA_character_, NA_character_, "flagpoints/16508922584467118789500536331992.jpg",
              "flagpoints/16508905579734860955479543733314.jpg", NA_character_),
    enddate = as.POSIXct(c(NA, NA, NA, NA, NA)),
    endby = c("", "", "", "", ""),
    stringsAsFactors = FALSE
  )
}

#' Get stub data for drone sites (sites where drone = Y, M, or C)
#' @return data.frame with realistic drone site data
get_stub_drone_sites <- function() {
  data.frame(
    sitecode = c("020207-001", "700407-010", "021335-005", "020125-003", "700201-015"),
    facility = c("N", "Sj", "N", "N", "Sj"),
    acres = c(0.50, 0.25, 1.20, 0.80, 0.45),
    prehatch = c(NA_character_, "PREHATCH", NA_character_, "PREHATCH", NA_character_),
    drone = c("Y", "M", "C", "Y", "M"),
    foreman = c("0204", "7002", "0206", "0208", "7003"),
    zone = c("1", "1", "2", "1", "2"),
    sectcode = c("020207-", "700407-", "021335-", "020125-", "700201-"),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# STANDARDIZED STUB DATA HELPERS (for cross-app testing)
# =============================================================================

#' Get standard sites data - SINGLE SOURCE OF TRUTH for all app tests
#' Uses consistent sitecodes/facilities/zones/foremen across all tests
#' @return data.frame with standard site columns
get_stub_standard_sites <- function() {
  sites <- get_stub_drone_sites()
  # Ensure fosarea column exists for apps that expect it
  sites$fosarea <- sites$foreman
  sites
}

#' Get standard treatments data - SINGLE SOURCE OF TRUTH for all app tests
#' @return data.frame with standard treatment columns
get_stub_standard_treatments <- function() {
  data.frame(
    sitecode = c("020207-001", "020207-001", "700407-010"),
    facility = c("N", "N", "Sj"),
    foreman = c("0204", "0204", "7002"),
    fosarea = c("0204", "0204", "7002"),
    zone = c("1", "1", "1"),
    inspdate = as.Date(c("2025-04-16", "2025-04-10", "2025-05-07")),
    matcode = c("G2", "G2", "G2"),
    effect_days = c(30L, 30L, 30L),
    stringsAsFactors = FALSE
  )
}

#' Get raw data in STANDARDIZED format for all apps
#' ALL apps now use the SAME format: list(sites, treatments, total_count)
#' @param app_name One of: drone, ground_prehatch_progress, struct_trt, catch_basin_status
#' @return List with standardized format: list(sites, treatments, total_count)
get_stub_raw_data_for_app <- function(app_name) {
  # All apps return the SAME standardized format
  if (app_name == "catch_basin_status") {
    # Catch basin uses aggregated section data
    agg_data <- create_stub_catch_basin_aggregated_data()
    return(list(
      sites = agg_data,
      treatments = agg_data,
      total_count = sum(agg_data$total_count)
    ))
  } else {
    # All other apps use individual site/treatment records
    sites <- get_stub_standard_sites()
    treatments <- get_stub_standard_treatments()
    return(list(
      sites = sites,
      treatments = treatments,
      total_count = nrow(sites)
    ))
  }
}

#' Create stub catch basin aggregated data (matches load_raw_data output format)
#' @return data.frame with aggregated catch basin status data
create_stub_catch_basin_aggregated_data <- function() {
  data.frame(
    facility = c("N", "N", "Sj", "E", "Sr"),
    zone = c("1", "2", "1", "1", "2"),
    fosarea = c("0204", "0204", "7002", "0301", "0501"),
    sectcode = c("020125-", "020207-", "700407-", "620123-", "500123-"),
    total_count = c(50L, 30L, 40L, 25L, 35L),
    active_count = c(40L, 25L, 35L, 20L, 30L),
    expiring_count = c(5L, 3L, 4L, 2L, 3L),
    expired_count = c(5L, 2L, 1L, 3L, 2L),
    facility_full = c("Navarre", "Navarre", "St. Joseph", "East", "Shoreview"),
    foreman_name = c("FOS1", "FOS1", "FOS2", "FOS3", "FOS4"),
    stringsAsFactors = FALSE
  )
}

#' Get expected sitecodes when filtering by facility "N"
#' @return Character vector of sitecodes
get_stub_expected_facility_n_sites <- function() {
  sort(c("020125-003", "020207-001", "021335-005"))
}

#' Get expected sitecodes when filtering by zone "1"
#' @return Character vector of sitecodes
get_stub_expected_zone1_sites <- function() {
  sort(c("020125-003", "020207-001", "700407-010"))
}
