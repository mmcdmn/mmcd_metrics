# data_functions.R
# Data processing functions for sections-cards DEMO

#' Get filter options from database (lightweight query)
#' 
#' Retrieves unique values for filter dropdowns without loading full dataset
#' 
#' @return A list with facility and fosarea choices
#' @export
get_filter_options <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  
  query <- "
    SELECT DISTINCT
      g.facility,
      g.fosarea
    FROM public.loc_breeding_sites b
    LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode, 7)
    WHERE b.enddate IS NULL
      AND g.facility IS NOT NULL
      AND g.fosarea IS NOT NULL
    ORDER BY g.facility, g.fosarea
  "
  
  data <- dbGetQuery(con, query)
  
  return(list(
    facilities = sort(unique(data$facility)),
    fosarea_list = sort(unique(data$fosarea))
  ))
}

#' Get breeding sites data with section information
#' 
#' This function retrieves breeding site data and joins with section (gis_sectcode)
#' information using the correct JOIN logic to avoid ambiguous matches.
#' 
#' @return A data frame with breeding site and section information
#' @export
get_breeding_sites_with_sections <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  
  query <- "
    SELECT 
      b.sitecode,
      b.priority,
      b.acres,
      b.type,
      b.air_gnd,
      b.culex,
      b.spr_aedes,
      b.prehatch,
      b.remarks,
      b.drone,
      b.facility as site_facility,
      g.sectcode as section,
      g.zone,
      g.facility,
      g.fosarea
    FROM public.loc_breeding_sites b
    -- CRITICAL: Use exact sectcode match to avoid ambiguous joins
    -- This matches the first 7 characters of sitecode with sectcode
    -- Example: sitecode '191819-045' -> left(sitecode,7) = '191819-' matches sectcode '191819-'
    LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode, 7)
    WHERE b.enddate IS NULL
    ORDER BY b.sitecode
  "
  
  data <- dbGetQuery(con, query)
  
  return(data)
}
