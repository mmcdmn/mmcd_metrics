# Stub data for air_inspection_checklist app testing
# Based on REAL data from mmcd_data PostgreSQL database (extracted 2026-03-13)
# Tables: loc_breeding_sites, gis_sectcode, dblarv_insptrt_current/archive,
#         mattype_list_targetdose, dblarv_sample_current/archive,
#         loc_breeding_site_cards_sjsreast2, employee_list

# =============================================================================
# CHECKLIST DATA (final enriched output from get_checklist_data)
# =============================================================================

#' Get stub checklist data (output of get_checklist_data after R post-processing)
#' Includes all SQL columns plus computed R columns (fos_display, airmap_display, etc.)
#' Built from real RED air sites (facility N + Sj), real employee names,
#' and realistic inspection/treatment states
#' @return data.frame with realistic RED air site checklist data
get_stub_checklist_data <- function() {
  data.frame(
    sitecode = c("020129-001", "020129-008", "020129-010",
                 "020130-002", "020130-005", "100101-001", "100102-018"),
    acres = c(9.94, 2.41, 2.84, 99.93, 3.96, 11.00, 5.00),
    facility = c("N", "N", "N", "N", "N", "Sj", "Sj"),
    zone = c("1", "1", "1", "1", "1", "2", "2"),
    fosarea = c("0204", "0204", "0204", "0204", "0204", "7009", "7009"),
    sectcode = c("020129-", "020129-", "020129-",
                 "020130-", "020130-", "100101-", "100102-"),
    airmap_num = c(NA_integer_, NA_integer_, NA_integer_,
                   NA_integer_, NA_integer_, NA_integer_, NA_integer_),
    last_insp_date = as.Date(c("2026-03-12", "2026-03-12", NA,
                               "2026-03-11", NA, NA, NA)),
    dip_count = c(0, 0, NA_real_, 0, NA_real_, NA_real_, NA_real_),
    inspector_emp = c("1905", "1905", NA_character_,
                      "1905", NA_character_, NA_character_, NA_character_),
    inspector_name = c(NA_character_, NA_character_, NA_character_,
                       NA_character_, NA_character_, NA_character_, NA_character_),
    sampnum_yr = c(NA_character_, NA_character_, NA_character_,
                   NA_character_, NA_character_, NA_character_, NA_character_),
    redblue = c(NA_character_, NA_character_, NA_character_,
                NA_character_, NA_character_, NA_character_, NA_character_),
    lab_missing = c(NA, NA, NA, NA, NA, NA, NA),
    was_inspected = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE),
    has_active_treatment = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    active_material = c(NA_character_, NA_character_, NA_character_,
                        NA_character_, NA_character_, NA_character_, NA_character_),
    active_trt_date = as.Date(c(NA, NA, NA, NA, NA, NA, NA)),
    active_trt_expiry = as.Date(c(NA, NA, NA, NA, NA, NA, NA)),
    is_prehatch = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    fos_display = c("Rosa", "Rosa", "Rosa",
                    "Rosa", "Rosa", "Matt G.", "Matt G."),
    airmap_display = c("No AirMap", "No AirMap", "No AirMap",
                       "No AirMap", "No AirMap", "No AirMap", "No AirMap"),
    inspector_display = c("Emp #1905", "Emp #1905", "",
                          "Emp #1905", "", "", ""),
    bug_status = c("No Sample", "No Sample", "No Sample",
                   "No Sample", "No Sample", "No Sample", "No Sample"),
    stringsAsFactors = FALSE
  )
}

#' Get stub checklist data with only uninspected sites
#' Useful for testing empty inspection states
#' @return data.frame with sites that have no recent inspections
get_stub_checklist_data_uninspected <- function() {
  data.frame(
    sitecode = c("020129-010", "020130-005", "100101-001"),
    acres = c(2.84, 3.96, 11.00),
    facility = c("N", "N", "Sj"),
    zone = c("1", "1", "2"),
    fosarea = c("0204", "0204", "7009"),
    sectcode = c("020129-", "020130-", "100101-"),
    airmap_num = c(NA_integer_, NA_integer_, NA_integer_),
    last_insp_date = as.Date(c(NA, NA, NA)),
    dip_count = c(NA_real_, NA_real_, NA_real_),
    inspector_emp = c(NA_character_, NA_character_, NA_character_),
    inspector_name = c(NA_character_, NA_character_, NA_character_),
    sampnum_yr = c(NA_character_, NA_character_, NA_character_),
    redblue = c(NA_character_, NA_character_, NA_character_),
    lab_missing = c(NA, NA, NA),
    was_inspected = c(FALSE, FALSE, FALSE),
    has_active_treatment = c(FALSE, FALSE, FALSE),
    active_material = c(NA_character_, NA_character_, NA_character_),
    active_trt_date = as.Date(c(NA, NA, NA)),
    active_trt_expiry = as.Date(c(NA, NA, NA)),
    is_prehatch = c(FALSE, FALSE, FALSE),
    fos_display = c("Rosa", "Rosa", "Matt G."),
    airmap_display = c("No AirMap", "No AirMap", "No AirMap"),
    inspector_display = c("", "", ""),
    bug_status = c("No Sample", "No Sample", "No Sample"),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# FIELD EMPLOYEES (output of get_field_employees)
# =============================================================================

#' Get stub field employees (output of get_field_employees)
#' Active employees with fieldsuper info, excluding Pilot/Lab/Recpt types
#' Data from real employee_list records
#' @return data.frame with field employee data
get_stub_air_field_employees <- function() {
  data.frame(
    emp_num = c("8202", "8206", "8203", "8208", "8205",
                "8207", "8209", "6272", "8299", "2002"),
    shortname = c("Andrew M.", "Gabi G.", "John K.", "Kathy B.", "S. Grant",
                  "Shawn M.", "Vanessa", "Katie", "Testing", "Tyler"),
    emp_type = c("FieldSuper", "FieldSuper", "FieldSuper", "FieldSuper", "FieldSuper",
                 "FieldSuper", "FieldSuper", "Inspector", "Inspector", "RFT-Tech"),
    facility = c("E", "E", "E", "E", "E",
                 "E", "E", "E", "E", "E"),
    fieldsuper = c("8202", "8206", "8203", "8208", "8205",
                   "8207", "8209", "8208", "8203", "8203"),
    fos_name = c("Andrew M.", "Gabi G.", "John K.", "Kathy B.", "S. Grant",
                 "Shawn M.", "Vanessa", "Kathy B.", "John K.", "John K."),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# SUPPORTING TABLE STUBS
# =============================================================================

#' Get stub mattype_list_targetdose data (material types with effect days)
#' Used in the treatment expiry CTE - from real mattype_list_targetdose table
#' @return data.frame with material type lookup data
get_stub_mattype_list <- function() {
  data.frame(
    matcode = c("14", "G2", "08", "12", "35",
                "19", "49", "S2"),
    mattype = c("Alt_Briquet", "Alt_P35", "Alt_Pellets", "Alt_SandXRG",
                "Bs_Vlexgran", "Bti_gran", "Bti_liquid", "Bti_VbacGS"),
    effect_days = c(150L, 30L, 30L, 20L, 30L, 6L, 1L, 6L),
    prehatch = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

#' Get stub lab sample results (dblarv_sample_current)
#' Used for red/blue bug determination - from real samples
#' @return data.frame with lab sample data
get_stub_dblarv_sample <- function() {
  data.frame(
    sampnum_yr = c("25-79085", "25-78218", "25-78215",
                   "25-78164", "25-78062", "25-77840", "25-75574"),
    redblue = c("R", "R", "B", "B", NA_character_, NA_character_, NA_character_),
    missing = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# CHECKLIST SUMMARY (output of summarize_checklist)
# =============================================================================

#' Get stub checklist summary for the default checklist data
#' Computed from get_stub_checklist_data() output
#' @return list of summary statistics matching summarize_checklist() output
get_stub_checklist_summary <- function() {
  list(
    total_sites = 7L,
    inspected = 3L,
    not_inspected = 4L,
    pct_complete = "42.9%",
    red_bugs = 0L,
    blue_bugs = 0L
  )
}
