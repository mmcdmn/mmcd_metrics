# data_functions.R
# Data processing functions for Section Cards
#
# BREEDING SITES: Uses loc_breeding_site_card_JK table directly
#   - facility, zone, foreman come from JK table (NOT gis_sectcode)
#   - foreman is aliased as fosarea for filter compatibility
#   - Dynamic column discovery for extra fields (ra, airmap_num, etc.)
# STRUCTURES: Still use gis_sectcode for facility/zone/fosarea

# =============================================================================
# JK TABLE CONSTANTS & DYNAMIC COLUMN DISCOVERY
# =============================================================================

# The JK table name (case-sensitive, needs quoting in SQL)
JK_TABLE <- '"loc_breeding_site_card_JK"'

# Core columns that are always expected in the JK table
CORE_BREEDING_COLS <- c("sitecode", "priority", "acres", "type", "air_gnd",
                        "culex", "spr_aedes", "coq_pert", "prehatch",
                        "remarks", "drone", "sample", "facility",
                        "zone", "foreman")

# Internal/structural columns excluded from dynamic discovery
EXCLUDED_INTERNAL_COLS <- c("id", "geom", "fid", "site", "sectcode", "page")

#' Discover dynamic (extra) columns in the JK table
#'
#' Queries information_schema to find columns beyond core + excluded sets.
#' These are extra fields like ra, airmap_num, partialtrt, etc.
#'
#' @param con Optional database connection (will create one if NULL)
#' @return Character vector of dynamic column names
#' @export
get_dynamic_columns <- function(con = NULL) {
  own_con <- is.null(con)
  if (own_con) {
    con <- get_db_connection()
    on.exit(safe_disconnect(con), add = TRUE)
  }
  
  query <- paste0("
    SELECT column_name
    FROM information_schema.columns
    WHERE table_schema = 'public'
      AND table_name = 'loc_breeding_site_card_JK'
    ORDER BY ordinal_position
  ")
  
  all_cols <- dbGetQuery(con, query)$column_name
  known_cols <- c(CORE_BREEDING_COLS, EXCLUDED_INTERNAL_COLS)
  dynamic <- setdiff(all_cols, known_cols)
  
  return(dynamic)
}

#' Convert a snake_case column name to a human-readable label
#'
#' @param col_name The column name (e.g. "airmap_num")
#' @return Human-readable label (e.g. "Airmap Num")
#' @export
humanize_column_name <- function(col_name) {
  label <- gsub("_", " ", col_name)
  label <- gsub("(^|\\s)(\\w)", "\\1\\U\\2", label, perl = TRUE)
  return(label)
}

# =============================================================================
# BREEDING SITE FUNCTIONS (from JK table — NO gis_sectcode)
# =============================================================================

#' Check which dynamic columns actually have data for given filters
#'
#' Queries the JK table to find which dynamic columns have at least one
#' non-null, non-empty value for the given facility/foreman filter.
#'
#' @param dynamic_cols Character vector of dynamic column names to check
#' @param facility_filter Optional facility filter (NULL or "all" = no filter)
#' @param fosarea_filter Optional foreman/fosarea filter (NULL or "all" = no filter)
#' @return Character vector of dynamic column names that have data
#' @export
get_dynamic_cols_with_data <- function(dynamic_cols, facility_filter = NULL, fosarea_filter = NULL) {
  if (length(dynamic_cols) == 0) return(character(0))
  
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  # Build WHERE clause
  where_parts <- c()
  if (!is.null(facility_filter) && facility_filter != "all") {
    where_parts <- c(where_parts, paste0("facility = '", facility_filter, "'"))
  }
  if (!is.null(fosarea_filter) && fosarea_filter != "all") {
    where_parts <- c(where_parts, paste0("foreman = '", fosarea_filter, "'"))
  }
  where_clause <- if (length(where_parts) > 0) paste0(" WHERE ", paste(where_parts, collapse = " AND ")) else ""
  
  # Build a single query: SELECT count(col) > 0 for each dynamic column
  count_exprs <- sapply(dynamic_cols, function(col) {
    sprintf("CASE WHEN COUNT(NULLIF(%s::text, '')) > 0 THEN 1 ELSE 0 END AS %s", col, col)
  })
  
  query <- paste0(
    "SELECT ", paste(count_exprs, collapse = ", "),
    " FROM public.", JK_TABLE, where_clause
  )
  
  result <- dbGetQuery(con, query)
  
  # Return column names where the value is 1
  cols_with_data <- dynamic_cols[as.logical(as.integer(result[1, ]))]
  return(cols_with_data)
}

#' Get filter options for breeding sites from JK table
#'
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area (foreman) filter
#' @return A list with facility, section, and fosarea choices
#' @export
get_filter_options <- function(facility_filter = NULL, fosarea_filter = NULL) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  # Build filter conditions
  where_conditions <- "facility IS NOT NULL AND sectcode IS NOT NULL"
  
  if (!is.null(facility_filter) && facility_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND facility = '", facility_filter, "'")
  }
  
  if (!is.null(fosarea_filter) && fosarea_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND foreman = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT
      facility,
      sectcode as section,
      foreman as fosarea
    FROM public.", JK_TABLE, "
    WHERE ", where_conditions, "
    ORDER BY facility, sectcode, foreman
  ")
  
  data <- dbGetQuery(con, query)
  
  return(list(
    facilities = sort(unique(data$facility)),
    sections = sort(unique(data$section)),
    fosarea_list = sort(unique(data$fosarea))
  ))
}

#' Get unique town codes from JK table with optional filtering
#'
#' Extracts the first 4 digits from sectcodes to get unique town codes
#'
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area (foreman) filter
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
    where_conditions <- paste0(where_conditions, " AND foreman = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT left(sectcode, 4) as towncode
    FROM public.", JK_TABLE, "
    WHERE ", where_conditions, "
    ORDER BY left(sectcode, 4)
  ")
  
  data <- dbGetQuery(con, query)
  return(data$towncode)
}

#' Get sections filtered by town code from JK table
#'
#' @param towncode_filter Optional town code filter (first 4 digits)
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area (foreman) filter
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
    where_conditions <- paste0(where_conditions, " AND foreman = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT sectcode
    FROM public.", JK_TABLE, "
    WHERE ", where_conditions, "
    ORDER BY sectcode
  ")
  
  data <- dbGetQuery(con, query)
  return(data$sectcode)
}

#' Get breeding sites data from JK table (NO gis_sectcode join)
#'
#' Retrieves all breeding site data directly from the JK table.
#' facility, zone, and foreman come from the JK table itself.
#' foreman is aliased as fosarea for filter compatibility.
#' Dynamic columns (ra, airmap_num, etc.) are included automatically.
#'
#' @return A data frame with breeding site information
#' @export
get_breeding_sites_with_sections <- function() {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  # Discover dynamic columns beyond core
  dynamic_cols <- get_dynamic_columns(con)
  
  # Build dynamic column SELECT list
  dynamic_select <- ""
  if (length(dynamic_cols) > 0) {
    dynamic_select <- paste0(",\n      ", paste(dynamic_cols, collapse = ",\n      "))
  }
  
  query <- paste0("
    SELECT
      sitecode,
      priority,
      acres,
      type,
      air_gnd,
      culex,
      spr_aedes,
      coq_pert as perturbans,
      prehatch,
      remarks,
      drone,
      sample,
      facility,
      zone,
      foreman,
      foreman as fosarea,
      sectcode as section", dynamic_select, "
    FROM public.", JK_TABLE, "
    ORDER BY sitecode
  ")
  
  message("[section_cards] Loading breeding sites from JK table...")
  data <- dbGetQuery(con, query)
  message(sprintf("[section_cards] Loaded %d breeding sites with %d columns", nrow(data), ncol(data)))
  
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
