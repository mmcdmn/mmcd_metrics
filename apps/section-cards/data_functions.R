# data_functions.R
# Data processing functions for Section Cards

# =============================================================================
# CONSTANTS: Core and excluded columns for loc_breeding_site_card_JK
# =============================================================================

# Core breeding site columns - always selected explicitly in queries
CORE_BREEDING_COLS <- c("sitecode", "priority", "acres", "type", "air_gnd",
                        "culex", "spr_aedes", "coq_pert", "prehatch",
                        "remarks", "drone", "sample", "facility")

# Internal/structural columns excluded from dynamic selection
EXCLUDED_INTERNAL_COLS <- c("id", "geom", "fid", "site", "sectcode", "zone","foreman")

# Combined exclusion list for dynamic column discovery
ALL_EXCLUDED_FROM_DYNAMIC <- c(CORE_BREEDING_COLS, EXCLUDED_INTERNAL_COLS)

#' Convert a column name to a human-readable label
#'
#' @param col_name The raw column name (e.g., "airmap_num")
#' @return A human-readable label (e.g., "Airmap Num")
#' @export
humanize_column_name <- function(col_name) {
  label <- gsub("_", " ", col_name)
  label <- tools::toTitleCase(label)
  return(label)
}

#' Get dynamic (non-core) column names from loc_breeding_site_card_JK
#'
#' Queries information_schema to discover columns that aren't part of the
#' core breeding site fields. These columns may change as users modify the table.
#'
#' @param con Optional database connection. If NULL, creates a new one.
#' @return Character vector of dynamic column names
#' @export
get_dynamic_columns <- function(con = NULL) {
  own_con <- is.null(con)
  if (own_con) {
    con <- get_db_connection()
    on.exit(safe_disconnect(con), add = TRUE)
  }

  all_cols <- dbGetQuery(con, "
    SELECT column_name
    FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'loc_breeding_site_card_JK'
    ORDER BY ordinal_position
  ")$column_name

  return(setdiff(all_cols, ALL_EXCLUDED_FROM_DYNAMIC))
}

#' Get filter options from database (lightweight query)
#' 
#' Retrieves unique values for filter dropdowns without loading full dataset
#' Uses db_helpers functions for facility and FOS lookups
#' 
#' @param facility_filter Optional facility filter to cascade to FOS and sections
#' @param fosarea_filter Optional FOS area filter to cascade to sections  
#' @return A list with facility, section, and fosarea choices
#' @export
get_filter_options <- function(facility_filter = NULL, fosarea_filter = NULL) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  # Build filter conditions (no enddate filter - JK table doesn't have enddate)
  where_conditions <- "g.facility IS NOT NULL AND g.sectcode IS NOT NULL AND g.fosarea IS NOT NULL"
  
  if (!is.null(facility_filter) && facility_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND g.facility = '", facility_filter, "'")
  }
  
  if (!is.null(fosarea_filter) && fosarea_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND g.fosarea = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT
      g.facility,
      g.sectcode as section,
      g.fosarea
    FROM public.\"loc_breeding_site_card_JK\" b
    LEFT JOIN public.gis_sectcode g ON g.sectcode = b.sectcode
    WHERE ", where_conditions, "
    ORDER BY g.facility, g.sectcode, g.fosarea
  ")
  
  data <- dbGetQuery(con, query)
  
  return(list(
    facilities = sort(unique(data$facility)),
    sections = sort(unique(data$section)),
    fosarea_list = sort(unique(data$fosarea))
  ))
}

#' Get unique town codes from gis_sectcode with optional filtering
#' 
#' Extracts the first 4 digits from sectcodes to get unique town codes
#' Can be filtered by facility and/or fosarea for cascading dropdowns
#' 
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area filter  
#' @return A vector of unique town codes
#' @export
get_town_codes <- function(facility_filter = NULL, fosarea_filter = NULL) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  # Build filter conditions
  where_conditions <- "sectcode IS NOT NULL"
  
  if (!is.null(facility_filter) && facility_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND facility = '", facility_filter, "'")
  }
  
  if (!is.null(fosarea_filter) && fosarea_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND fosarea = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT left(sectcode, 4) as towncode
    FROM public.gis_sectcode
    WHERE ", where_conditions, "
    ORDER BY left(sectcode, 4)
  ")
  
  data <- dbGetQuery(con, query)
  return(data$towncode)
}

#' Get sections filtered by town code
#' 
#' @param towncode_filter Optional town code filter (first 4 digits)
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area filter
#' @return A vector of unique sections
#' @export
get_sections_by_towncode <- function(towncode_filter = NULL, facility_filter = NULL, fosarea_filter = NULL) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  # Build filter conditions
  where_conditions <- "sectcode IS NOT NULL"
  
  if (!is.null(towncode_filter) && towncode_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND left(sectcode, 4) = '", towncode_filter, "'")
  }
  
  if (!is.null(facility_filter) && facility_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND facility = '", facility_filter, "'")
  }
  
  if (!is.null(fosarea_filter) && fosarea_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND fosarea = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT sectcode
    FROM public.gis_sectcode
    WHERE ", where_conditions, "
    ORDER BY sectcode
  ")
  
  data <- dbGetQuery(con, query)
  return(data$sectcode)
}

#' Get breeding sites data with section information
#'
#' This function retrieves breeding site data from loc_breeding_site_card_JK
#' and joins with section (gis_sectcode) information.
#' Dynamically discovers extra columns in the JK table beyond the core fields.
#'
#' @return A data frame with breeding site, section, and dynamic column information
#' @export
get_breeding_sites_with_sections <- function() {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)

  # Discover dynamic columns from the JK table
  dynamic_cols <- get_dynamic_columns(con)

  # Build dynamic SELECT clause for extra columns
  dynamic_select <- ""
  if (length(dynamic_cols) > 0) {
    dynamic_select <- paste0(",\n      b.", dynamic_cols, collapse = "")
  }

  query <- paste0("
    SELECT
      b.sitecode,
      b.priority,
      b.acres,
      b.type,
      b.air_gnd,
      b.culex,
      b.spr_aedes,
      b.coq_pert as perturbans,
      b.prehatch,
      b.remarks,
      b.drone,
      b.sample,
      b.facility as site_facility,
      g.sectcode as section,
      g.zone,
      g.facility,
      g.fosarea", dynamic_select, "
    FROM public.\"loc_breeding_site_card_JK\" b
    LEFT JOIN public.gis_sectcode g ON g.sectcode = b.sectcode
    ORDER BY b.sitecode
  ")

  data <- dbGetQuery(con, query)

  return(data)
}

#' Get structure sites data with section information
#' 
#' This function retrieves structure site data from loc_cxstruct and joins with 
#' section (gis_sectcode) information for filtering.
#' 
#' @return A data frame with structure site and section information
#' @export
get_structures_with_sections <- function() {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  query <- "
    SELECT 
      loc.sitecode,
      loc.s_type,
      loc.status_udw,
      loc.sqft,
      loc.priority,
      loc.culex,
      loc.chambers,
      loc.comments as remarks,
      g.sectcode as section,
      g.zone,
      g.facility,
      g.fosarea
    FROM public.loc_cxstruct loc
    LEFT JOIN public.gis_sectcode g ON g.sectcode = left(loc.sitecode, 7)
    WHERE loc.enddate IS NULL
    ORDER BY loc.sitecode
  "
  
  data <- dbGetQuery(con, query)
  
  return(data)
}

#' Get town codes for structures
#' 
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area filter
#' @return A vector of unique town codes
#' @export
get_structure_town_codes <- function(facility_filter = NULL, fosarea_filter = NULL) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  # Build filter conditions
  where_conditions <- "loc.enddate IS NULL AND g.sectcode IS NOT NULL"
  
  if (!is.null(facility_filter) && facility_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND g.facility = '", facility_filter, "'")
  }
  
  if (!is.null(fosarea_filter) && fosarea_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND g.fosarea = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT left(g.sectcode, 4) as towncode
    FROM public.loc_cxstruct loc
    LEFT JOIN public.gis_sectcode g ON g.sectcode = left(loc.sitecode, 7)
    WHERE ", where_conditions, "
    ORDER BY left(g.sectcode, 4)
  ")
  
  data <- dbGetQuery(con, query)
  return(data$towncode)
}

#' Get sections for structures filtered by town code
#' 
#' @param towncode_filter Optional town code filter
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area filter
#' @return A vector of unique sections
#' @export
get_structure_sections_by_towncode <- function(towncode_filter = NULL, facility_filter = NULL, fosarea_filter = NULL) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  # Build filter conditions
  where_conditions <- "loc.enddate IS NULL AND g.sectcode IS NOT NULL"
  
  if (!is.null(towncode_filter) && towncode_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND left(g.sectcode, 4) = '", towncode_filter, "'")
  }
  
  if (!is.null(facility_filter) && facility_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND g.facility = '", facility_filter, "'")
  }
  
  if (!is.null(fosarea_filter) && fosarea_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND g.fosarea = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT g.sectcode
    FROM public.loc_cxstruct loc
    LEFT JOIN public.gis_sectcode g ON g.sectcode = left(loc.sitecode, 7)
    WHERE ", where_conditions, "
    ORDER BY g.sectcode
  ")
  
  data <- dbGetQuery(con, query)
  return(data$sectcode)
}

#' Get filter options for structures
#' 
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area filter
#' @return A list with structure type, priority and section choices
#' @export
get_structure_filter_options <- function(facility_filter = NULL, fosarea_filter = NULL) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  # Build filter conditions
  where_conditions <- "loc.enddate IS NULL AND g.facility IS NOT NULL"
  
  if (!is.null(facility_filter) && facility_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND g.facility = '", facility_filter, "'")
  }
  
  if (!is.null(fosarea_filter) && fosarea_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND g.fosarea = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT
      g.facility,
      g.sectcode as section,
      g.fosarea,
      loc.s_type
    FROM public.loc_cxstruct loc
    LEFT JOIN public.gis_sectcode g ON g.sectcode = left(loc.sitecode, 7)
    WHERE ", where_conditions, "
    ORDER BY g.facility, g.sectcode, g.fosarea
  ")
  
  data <- dbGetQuery(con, query)
  
  return(list(
    facilities = sort(unique(data$facility)),
    sections = sort(unique(data$section)),
    fosarea_list = sort(unique(data$fosarea)),
    structure_types = sort(unique(toupper(data$s_type)))
  ))
}
