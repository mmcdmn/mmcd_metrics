# Stub data for section-cards app testing
# Based on REAL data from mmcd_data PostgreSQL database (extracted 2026-03-13)
# Tables: loc_breeding_site_cards_sjsreast2, loc_breeding_sites + gis_sectcode, loc_cxstruct + gis_sectcode

# =============================================================================
# JK TABLE (loc_breeding_site_cards_sjsreast2) STUBS
# =============================================================================

#' Get stub data for breeding sites from JK table (get_breeding_sites_with_sections output)
#' Matches the SELECT columns from data_functions.R including dynamic columns
#' Data sourced from real database records across E and Sj facilities
#' @return data.frame with realistic breeding site card data
get_stub_breeding_site_cards <- function() {
  data.frame(
    sitecode = c("620205-001", "620205-006", "620205-046",
                 "620205-050", "620205-060", "100201-001", "100201-002"),
    priority = c("RED", "GREEN", "GREEN", "GREEN", "RED", "BLUE", "BLUE"),
    acres = c(0.02, 0.20, 0.10, 0.95, 0.11, 0.23, 0.90),
    type = c(2.1, 2.1, 3.2, 1.2, 4.3, 4.3, 4.2),
    air_gnd = c("G", "G", "G", "G", "G", "G", "G"),
    culex = c(NA_character_, "", "", "", "Y", "", ""),
    spr_aedes = c(NA_character_, "", "", "Y", "", "", ""),
    perturbans = c(NA_character_, "", "Y", "", "", "", ""),
    prehatch = c(NA_character_, "PREHATCH", "PREHATCH", "PREHATCH",
                 "", "", ""),
    remarks = c("New 2024", "",  "",
                "If north bit is bone dry, don't treat that area. Adjust material if needed. Patchy.",
                "Bred 2021 culex and vexans, 2022 culex", "", ""),
    drone = c(NA_character_, NA_character_, NA_character_,
              NA_character_, NA_character_, NA_character_, NA_character_),
    sample = c("", "", "", "", "", "", ""),
    facility = c("E", "E", "E", "E", "E", "Sj", "Sj"),
    zone = c("1", "1", "1", "1", "1", "1", "1"),
    foreman = c("8208", "8208", "8208", "8208", "8208", "7006", "7006"),
    fosarea = c("8208", "8208", "8208", "8208", "8208", "7006", "7006"),
    ra = c(NA_character_, "", "", "", "", "", ""),
    section = c("620205-", "620205-", "620205-",
                "620205-", "620205-", "100201-", "100201-"),
    airmap_num = c(0L, -3L, 0L, 0L, 0L, 0L, 0L),
    elevthresh = c("", "", "", "", "", "", ""),
    partialtrt = c("", "", "", "", "", "", ""),
    perimacres = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    mapped = c("", "", "", "", "", "", ""),
    catinsp = c("", "", "", "", "", "", ""),
    dip = c("", "", "", "", "", "", ""),
    percent = c("0", "0", "0", "0", "0", "0", "0"),
    air_foreman = c(NA_character_, NA_character_, NA_character_,
                    NA_character_, NA_character_, NA_character_, NA_character_),
    blue = c(NA_character_, NA_character_, NA_character_,
             NA_character_, NA_character_, NA_character_, NA_character_),
    cattail_inspected = c(NA_character_, NA_character_, NA_character_,
                          NA_character_, NA_character_, NA_character_, NA_character_),
    red = c(NA_character_, NA_character_, NA_character_,
            NA_character_, NA_character_, NA_character_, NA_character_),
    fast = c(NA_character_, NA_character_, NA_character_,
             NA_character_, NA_character_, NA_character_, NA_character_),
    favorite_site = c(NA_character_, NA_character_, NA_character_,
                      NA_character_, NA_character_, NA_character_, NA_character_),
    fos_comments = c("", "Homeowner says at 100% wet it spills into the yards",
                     "", "", "", "", ""),
    good_ct_site = c("", "", "", "", "", "", ""),
    medium_ct = c("", "", "", "", "", "", ""),
    no_longer_ct = c("", "", "", "", "", "", ""),
    partner = c(NA_character_, NA_character_, NA_character_,
                NA_character_, NA_character_, NA_character_, NA_character_),
    possible_ct = c("", "", "Y", "", "", "", ""),
    potentialdronesite = c("", "", "", "", "", "", ""),
    towncode = c(NA_character_, NA_character_, NA_character_,
                 NA_character_, NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
}

#' Get stub dynamic columns list (output of get_dynamic_columns)
#' Based on real information_schema query of loc_breeding_site_cards_sjsreast2
#' @return Character vector of dynamic column names
get_stub_dynamic_columns <- function() {
  c("airmap_num", "elevthresh", "partialtrt", "perimacres", "mapped",
    "catinsp", "dip", "percent", "air_foreman", "blue",
    "cattail_inspected", "red", "fast", "favorite_site",
    "fos_comments", "good_ct_site", "medium_ct", "no_longer_ct",
    "partner", "possible_ct", "potentialdronesite", "towncode")
}

#' Get stub filter options for JK table (output of get_filter_options)
#' Based on real DISTINCT query results
#' @return list with facilities, sections, fosarea_list
get_stub_jk_filter_options <- function() {
  list(
    facilities = c("E", "Sj"),
    sections = c("100201-", "620205-", "620206-", "620207-",
                 "620208-", "620403-"),
    fosarea_list = c("7006", "8208")
  )
}

#' Get stub town codes for JK table (output of get_town_codes)
#' @return Character vector of town codes
get_stub_jk_town_codes <- function() {
  c("1002", "6202", "6203", "6204")
}

# =============================================================================
# WEBSTER TABLE (loc_breeding_sites + gis_sectcode) STUBS
# =============================================================================

#' Get stub data for Webster breeding sites (get_webster_breeding_sites output)
#' Data sourced from real database records across N, Sj, E, Wm facilities
#' @return data.frame matching the Webster query columns
get_stub_webster_breeding_sites <- function() {
  data.frame(
    sitecode = c("020129-001", "020129-008", "100101-001",
                 "100101-002", "020912-001", "020912-002", "021701-001"),
    priority = c("RED", "RED", "RED",
                 NA_character_, "RED", "RED", "RED"),
    acres = c(9.94, 2.41, 11.00, 0.23, 24.54, 1.47, 2.54),
    type = c("5.2", "3.1", "2.1", "2.1", "3.2", "1.2", "3.1"),
    air_gnd = c("A", "A", "A", "G", "A", "G", "G"),
    culex = c("Y", "Y", NA_character_,
              NA_character_, NA_character_, NA_character_, NA_character_),
    spr_aedes = c("Y", NA_character_, "Y",
                  NA_character_, NA_character_, NA_character_, NA_character_),
    perturbans = c("Y", NA_character_, NA_character_,
                   NA_character_, NA_character_, NA_character_, NA_character_),
    prehatch = c(NA_character_, NA_character_, NA_character_,
                 NA_character_, NA_character_, NA_character_, NA_character_),
    remarks = c(" Can Follow Ditch Into East Side", NA_character_,
                "PARK ON 110TH ST OR CO RD 140", "",
                "", "", "Coould be switched to air or drone?"),
    drone = c(NA_character_, NA_character_, NA_character_,
              NA_character_, NA_character_, NA_character_, NA_character_),
    sample = c(NA_character_, NA_character_, NA_character_,
               NA_character_, NA_character_, NA_character_, NA_character_),
    facility = c("N", "N", "Sj", "Sj", "E", "E", "Wm"),
    zone = c("1", "1", "2", "2", "1", "1", "1"),
    fosarea = c("0204", "0204", "7009", "7009", "8209", "8209", "2707"),
    section = c("020129-", "020129-", "100101-",
                "100101-", "020912-", "020912-", "021701-"),
    stringsAsFactors = FALSE
  )
}

#' Get stub filter options for Webster table (output of get_webster_filter_options)
#' @return list with facilities, sections, fosarea_list
get_stub_webster_filter_options <- function() {
  list(
    facilities = c("E", "N", "Sj", "Wm"),
    sections = c("020129-", "020912-", "021701-", "100101-"),
    fosarea_list = c("0204", "2707", "7009", "8209")
  )
}

# =============================================================================
# STRUCTURES (loc_cxstruct + gis_sectcode) STUBS
# =============================================================================

#' Get stub data for structures with sections (get_structures_with_sections output)
#' Data from real loc_cxstruct records across N, Sj, E facilities
#' @return data.frame matching the structures query columns
get_stub_structures_with_sections <- function() {
  data.frame(
    sitecode = c("020125-301", "020125-302", "100111-301",
                 "100111-302", "620109-301"),
    s_type = c("PR", "CV", "PR", "PR", "XX"),
    status_udw = c("W", "W", "W", "W", "D"),
    sqft = c(0.0, 0.0, 0.0, NA_real_, 1.0),
    priority = c("GREEN", "", "GREEN", "GREEN", "BLUE"),
    culex = c("Y", "", "", NA_character_, ""),
    chambers = c("", "", "1", "1", "1"),
    remarks = c("B-JUL 2X 21", "", "NEW SITE 2021", "NEW SITE 2022", ""),
    section = c("020125-", "020125-", "100111-", "100111-", "620109-"),
    zone = c("2", "2", "1", "1", "1"),
    facility = c("N", "N", "Sj", "Sj", "E"),
    fosarea = c("0204", "0204", "7009", "7009", "8202"),
    stringsAsFactors = FALSE
  )
}

#' Get stub filter options for structures (get_structure_filter_options output)
#' Based on real DISTINCT query results
#' @return list with facilities, sections, fosarea_list, structure_types
get_stub_structure_filter_options <- function() {
  list(
    facilities = c("E", "N", "Sj"),
    sections = c("020125-", "100111-", "620109-"),
    fosarea_list = c("0204", "7009", "8202"),
    structure_types = c("CV", "PR", "XX")
  )
}

#' Get stub town codes for structures (get_structure_town_codes output)
#' @return Character vector of town codes
get_stub_structure_town_codes <- function() {
  c("0201", "1001", "6201")
}
