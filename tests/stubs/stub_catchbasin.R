# Stub data for catch basin tables
# Based on actual database schema from mmcd_data
# Tables: catchbasin_status, loc_catchbasin, cbtreatment_status

#' Get stub data for catchbasin_status view
#' @return data.frame with realistic catch basin status data
get_stub_catchbasin_status <- function() {
  data.frame(
    gid = c(613341L, 613342L, 613345L, 613346L, 613348L),
    sectcode = c("020125-", "020125-", "020125-", "020125-", "020125-"),
    status_udw = c("W", "W", "W", "W", "W"),
    active_status = c("expired", "expired", "expired", "expired", "expired"),
    insptrt_id = c(3371137L, 3371137L, 3371137L, 3371137L, 3371137L),
    stringsAsFactors = FALSE
  )
}

#' Get stub data for catchbasin_status with varied statuses
#' @return data.frame with diverse catch basin statuses for testing
get_stub_catchbasin_status_varied <- function() {
  data.frame(
    gid = c(613341L, 613342L, 613345L, 613346L, 613348L, 613350L, 613351L),
    sectcode = c("020125-", "020125-", "020207-", "020207-", "700407-", "700407-", "021335-"),
    status_udw = c("W", "D", "W", "U", "W", "D", "W"),
    active_status = c("expired", "active", "expired", "unknown", "active", "active", "expired"),
    insptrt_id = c(3371137L, 3371140L, 3371145L, NA_integer_, 3371150L, 3371151L, 3371160L),
    stringsAsFactors = FALSE
  )
}

#' Get stub data for loc_catchbasin (catch basin locations)
#' @return data.frame with realistic catch basin location data
get_stub_loc_catchbasin <- function() {
  data.frame(
    gid = c(613341L, 613342L, 613345L, 613346L, 613348L),
    sitecode = c("020125-CB001", "020125-CB002", "020125-CB003", "020125-CB004", "020125-CB005"),
    sectcode = c("020125-", "020125-", "020125-", "020125-", "020125-"),
    facility = c("N", "N", "N", "N", "N"),
    zone = c("1", "1", "1", "1", "1"),
    foreman = c("0204", "0204", "0204", "0204", "0204"),
    cb_type = c("Standard", "Standard", "Sump", "Standard", "Sump"),
    status = c("Active", "Active", "Active", "Inactive", "Active"),
    startdate = as.POSIXct(c("2020-01-15 00:00:00", "2020-01-15 00:00:00", "2021-03-20 00:00:00",
                             "2019-06-10 00:00:00", "2022-04-05 00:00:00")),
    enddate = as.POSIXct(c(NA, NA, NA, "2024-01-01 00:00:00", NA)),
    stringsAsFactors = FALSE
  )
}

#' Get stub data for cbtreatment_status (catch basin treatment status)
#' @return data.frame with realistic CB treatment status data
get_stub_cbtreatment_status <- function() {
  data.frame(
    gid = c(613341L, 613342L, 613345L),
    sectcode = c("020125-", "020125-", "020125-"),
    last_treatment_date = as.Date(c("2025-06-15", "2025-06-15", "2025-05-20")),
    matcode = c("VB", "VB", "VB"),
    days_since_treatment = c(30L, 30L, 56L),
    next_treatment_due = as.Date(c("2025-07-15", "2025-07-15", "2025-06-20")),
    stringsAsFactors = FALSE
  )
}
