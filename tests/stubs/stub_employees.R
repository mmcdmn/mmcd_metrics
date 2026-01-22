# Stub data for employee and section code lookup tables
# Based on actual database schema from mmcd_data
# Tables: employee_list, gis_sectcode

#' Get stub data for employee_list
#' @return data.frame with realistic employee data (passwords redacted)
get_stub_employee_list <- function() {
  data.frame(
    emp_num = c("0206", "7003", "7006", "7009", "2103", "0204", "0208"),
    date_start = as.Date(c("2018-05-20", "2009-04-06", "1996-12-02", "1998-08-03", 
                           "2004-02-09", "2015-03-01", "2017-06-15")),
    date_end = as.Date(c(NA, NA, NA, NA, NA, NA, NA)),
    active = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    lastname = c("Lundquist", "Frey", "Pexa", "Giesen", "LaMere", "Smith", "Johnson"),
    firstname = c("John", "Adam", "Tom", "Matt", "Carey", "Jane", "Bob"),
    shortname = c("John L.", "Adam F.", "Tom P.", "Matt G.", "Carey L.", "Jane S.", "Bob J."),
    emp_type = c("FieldSuper", "FieldSuper", "FieldSuper", "FieldSuper", "RFT-Tech", "FieldSuper", "FieldSuper"),
    facility = c("N", "Sj", "Sj", "Sj", "MO", "N", "N"),
    fieldsuper = c("0206", "7003", "7006", "7009", "2405", "0204", "0208"),
    email = c("jlundquist@mmcd.org", "adamfrey@mmcd.org", "tpexa@mmcd.org", 
              "mgiesen@mmcd.org", "clamere@mmcd.org", "jsmith@mmcd.org", "bjohnson@mmcd.org"),
    sec_email = c("ddirkswager@mmcd.org", "cherrmann@mmcd.org", "cherrmann@mmcd.org",
                  "cherrmann@mmcd.org", "nancread@mmcd.org", "ddirkswager@mmcd.org", "ddirkswager@mmcd.org"),
    comment = c(NA_character_, NA_character_, NA_character_, NA_character_, "", NA_character_, NA_character_),
    pkey = c(2966L, 306L, 311L, 309L, 326L, 100L, 101L),
    pest_app_license_num = c(NA_character_, "20054701", "20035606", "20035595", "", NA_character_, NA_character_),
    classb_hasmat_lic = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_),
    stringsAsFactors = FALSE
  )
}

#' Get stub data for active field supervisors only
#' @return data.frame with active field supervisors
get_stub_field_supervisors <- function() {
  data.frame(
    emp_num = c("0204", "0206", "0208", "7002", "7003", "7006", "7009"),
    shortname = c("Jane S.", "John L.", "Bob J.", "Mike T.", "Adam F.", "Tom P.", "Matt G."),
    facility = c("N", "N", "N", "Sj", "Sj", "Sj", "Sj"),
    active = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    emp_type = rep("FieldSuper", 7),
    stringsAsFactors = FALSE
  )
}

#' Get stub data for gis_sectcode (section code geography)
#' @return data.frame with realistic section code data
get_stub_gis_sectcode <- function() {
  data.frame(
    gid = c(2721L, 184L, 5L, 6L, 7L),
    sectcode = c("820107-", "021020-", "020129-", "020130-", "020131-"),
    area = c(0.98, 0.02, 1.01, 0.98, 1.00),
    plss_coun = c("82", "2", "2", "2", "2"),
    plss_town = c("028", "031", "034", "034", "034"),
    rdir = c("0", "0", "0", "0", "0"),
    plss_rang = c("20", "24", "23", "23", "23"),
    plss_sect = c("07", "20", "29", "30", "31"),
    geosect = c("8202802007", "203102420", "203402329", "203402330", "203402331"),
    georang = c("82028020", "2031024", "2034023", "2034023", "2034023"),
    zone = c("1", "1", "1", "1", "1"),
    facility = c("E", "N", "N", "N", "N"),
    fosarea = c("8206", "0206", "0204", "0204", "0204"),
    fac_for_air = c("E", "N", "N", "N", "N"),
    stringsAsFactors = FALSE
  )
}

#' Get stub data for facility zone lookup
#' @return data.frame mapping facility codes to names
get_stub_facilities <- function() {
  data.frame(
    code = c("N", "Sj", "E", "Wm", "MO"),
    name = c("North", "St. James", "East", "Waconia/Mound", "Main Office"),
    stringsAsFactors = FALSE
  )
}
