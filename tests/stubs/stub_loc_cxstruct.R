# Stub data for loc_cxstruct table
# Based on actual database schema and sample data from mmcd_data.public.loc_cxstruct
# This stub provides realistic test data for structure-related app testing

#' Get stub data for loc_cxstruct (structure locations)
#' @return data.frame with realistic structure data
get_stub_loc_cxstruct <- function() {
  data.frame(
    gid = c(123456L, 123457L, 123458L, 123459L, 123460L),
    site = c(1L, 2L, 3L, 4L, 5L),
    sitecode = c("021723-304", "021723-305", "020207-002", "700407-032", "021335-112"),
    s_type = c("CV", "WO", "CV", "WO", "CV"),
    comments = c(NA_character_, "Test comment", NA_character_, NA_character_, "Notes here"),
    status_udw = c("W", "W", "D", "U", "W"),
    sqft = c(100.0, 250.5, 75.0, 300.0, 150.0),
    depth = c(2.0, 3.5, 1.5, 4.0, 2.5),
    ra = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    culex = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    date_drawn = as.POSIXct(c("2024-01-15 10:30:00", "2024-02-20 14:15:00", "2024-03-10 09:00:00", 
                              "2024-04-05 11:45:00", "2024-05-12 16:30:00")),
    who_drew = c("0240", "0239", "0270", "7016", "0245"),
    sectcode = c("021723-", "021723-", "020207-", "700407-", "021335-"),
    foreman = c("0204", "0204", "0208", "7002", "0206"),
    facility = c("N", "N", "N", "Sj", "N"),
    zone = c("1", "1", "2", "1", "1"),
    page = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    priority = c(NA_character_, "HIGH", NA_character_, NA_character_, NA_character_),
    chambers = c(1L, 2L, 1L, 3L, 1L),
    startdate = as.POSIXct(c("2020-01-01 00:00:00", "2021-06-15 00:00:00", "2022-03-20 00:00:00",
                             "2023-04-10 00:00:00", "2024-01-05 00:00:00")),
    startby = c("0240", "0239", "0270", "7016", "0245"),
    enddate = as.POSIXct(c(NA, NA, NA, NA, NA)),
    endby = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    photo = c(NA_character_, "structures/photo1.jpg", NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
}
