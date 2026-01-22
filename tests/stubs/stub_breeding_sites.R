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
