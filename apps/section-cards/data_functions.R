# data_functions.R
# Data processing functions for Section Cards
#
# BREEDING SITES: Uses loc_breeding_site_cards_sjsreast2 table directly
#   - facility, zone, foreman all come from new table (NOT from gis_sectcode)
#   - FOS Area (fosarea) is derived from foreman field (now NOT NULL)
#   - Section is derived from sitecode left(7) based on available foreman data
#   - Dynamic column discovery for extra fields (ra, airmap_num, etc.)
# STRUCTURES: Still use gis_sectcode for facility/zone/fosarea (separate data source)

# =============================================================================
# JK TABLE CONSTANTS & DYNAMIC COLUMN DISCOVERY
# =============================================================================

# The production table name (case-sensitive, needs quoting in SQL)
JK_TABLE <- '"loc_breeding_site_cards_sjsreast2"'

# The original Webster table (public.loc_breeding_sites)
# Same core columns but no custom fields; uses gis_sectcode for zone/fosarea
WEBSTER_TABLE <- 'loc_breeding_sites'

# Core columns that are always expected in the JK table
CORE_BREEDING_COLS <- c("sitecode", "priority", "acres", "type", "air_gnd",
                        "culex", "spr_aedes", "coq_pert", "prehatch",
                        "remarks", "drone", "sample", "facility",
                        "zone", "foreman")

# Internal/structural columns excluded from dynamic discovery
EXCLUDED_INTERNAL_COLS <- c("id", "geom", "fid", "site", "page")

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
      AND table_name = 'loc_breeding_site_cards_sjsreast2'
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
# DATA SANITIZATION
# =============================================================================

#' Sanitize breeding site data after loading from database
#'
#' Normalizes inconsistencies between the new table (loc_breeding_site_cards_sjsreast2)
#' and what loc_breeding_sites enforces at the backend. The new table imported from
#' GeoPackage uses empty strings where loc_breeding_sites uses NULL, and has different
#' data type handling. This function brings the data into the same clean state.
#'
#' Key differences addressed:
#'   - culex/spr_aedes/coq_pert: Old uses NULL for "no", new has empty strings → normalize to NA
#'   - prehatch: Old has PREHATCH/PELLET/BRIQUET/BTI, new only has PREHATCH → keep as-is
#'   - drone: Old has Y/N/M/C, new has Y/M + empty strings → empty to NA
#'   - sample: Old has Y/F/year-codes, new is all empty → empty to NA
#'   - air_gnd: Old uses 1-char G/A (varchar(1)), new matches → enforce uppercase
#'   - type: Old is numeric, new is double precision → coerce to numeric
#'   - priority: Old uses varchar(6) uppercase, new matches → enforce uppercase
#'
#' @param data Data frame from get_breeding_sites_with_sections()
#' @return Cleaned data frame
#' @export
sanitize_breeding_data <- function(data) {
  if (nrow(data) == 0) return(data)
  
  # --- 1. Trim whitespace from all character columns ---
  char_cols <- names(data)[sapply(data, is.character)]
  for (col in char_cols) {
    data[[col]] <- trimws(data[[col]])
  }
  
  # --- 2. Normalize flag fields: empty string → NA ---
  # loc_breeding_sites uses NULL for "not set"; new table has empty strings
  # These fields can have Y and other values (drone: Y/N/M/C, sample: Y/F/codes)
  flag_fields <- intersect(c("culex", "spr_aedes", "drone", "sample"), names(data))
  for (col in flag_fields) {
    data[[col]][!is.na(data[[col]]) & data[[col]] == ""] <- NA
    data[[col]] <- toupper(data[[col]])
  }
  
  # --- 3. Normalize coq_pert/perturbans: empty → NA, uppercase ---
  if ("perturbans" %in% names(data)) {
    data$perturbans[!is.na(data$perturbans) & data$perturbans == ""] <- NA
    data$perturbans <- toupper(data$perturbans)
  }
  
  # --- 4. Normalize prehatch: empty → NA ---
  # loc_breeding_sites allows PREHATCH, PELLET, BRIQUET, BTI — keep whatever value exists
  if ("prehatch" %in% names(data)) {
    data$prehatch[!is.na(data$prehatch) & data$prehatch == ""] <- NA
    data$prehatch <- toupper(data$prehatch)
  }
  
  # --- 5. Priority: uppercase, enforce varchar(6) max like loc_breeding_sites ---
  if ("priority" %in% names(data)) {
    data$priority <- toupper(trimws(as.character(data$priority)))
    data$priority[data$priority == ""] <- NA
  }
  
  # --- 6. air_gnd: uppercase, enforce 1-char like loc_breeding_sites varchar(1) ---
  if ("air_gnd" %in% names(data)) {
    data$air_gnd <- toupper(trimws(as.character(data$air_gnd)))
    data$air_gnd[data$air_gnd == ""] <- NA
    # Truncate to 1 char if somehow longer (loc_breeding_sites is varchar(1))
    data$air_gnd <- substr(data$air_gnd, 1, 1)
  }
  
  # --- 7. Ensure acres and type are numeric (loc_breeding_sites: double precision / numeric) ---
  if ("acres" %in% names(data)) {
    data$acres <- suppressWarnings(as.numeric(data$acres))
  }
  if ("type" %in% names(data)) {
    data$type <- suppressWarnings(as.numeric(data$type))
  }
  
  # --- 8. Normalize remarks: NA → empty string (display expects non-NA) ---
  if ("remarks" %in% names(data)) {
    data$remarks[is.na(data$remarks)] <- ""
  }
  
  # --- 9. Normalize dynamic/extra columns: empty strings → NA ---
  # So card display can skip empty fields cleanly
  dynamic_cols <- setdiff(names(data), c("sitecode", "priority", "acres", "type",
    "air_gnd", "culex", "spr_aedes", "perturbans", "prehatch", "remarks",
    "drone", "sample", "facility", "zone", "foreman", "fosarea", "section"))
  for (col in dynamic_cols) {
    if (is.character(data[[col]])) {
      data[[col]][!is.na(data[[col]]) & data[[col]] == ""] <- NA
    }
  }
  
  return(data)
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
#' FOS Area is derived from the foreman field (now NOT NULL).
#' Section filtering should ideally exclude sections with no foreman-assigned sites.
#'
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area (foreman) filter
#' @return A list with facility, section, and fosarea choices
#' @export
get_filter_options <- function(facility_filter = NULL, fosarea_filter = NULL) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  # Build filter conditions
  where_conditions <- "facility IS NOT NULL AND sitecode IS NOT NULL AND foreman IS NOT NULL"
  
  if (!is.null(facility_filter) && facility_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND facility = '", facility_filter, "'")
  }
  
  # FOS Area filter uses foreman field
  if (!is.null(fosarea_filter) && fosarea_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND foreman = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT
      facility,
      left(sitecode, 7) as section,
      foreman as fosarea
    FROM public.", JK_TABLE, "
    WHERE ", where_conditions, "
    ORDER BY facility, left(sitecode, 7), foreman
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
#' Extracts the first 4 digits from sitecodes to get unique town codes.
#' FOS Area filtering is based on the foreman field.
#'
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area (foreman) filter
#' @return A vector of unique town codes
#' @export
get_town_codes <- function(facility_filter = NULL, fosarea_filter = NULL) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  # Build filter conditions
  where_conditions <- "sitecode IS NOT NULL AND foreman IS NOT NULL"
  
  if (!is.null(facility_filter) && facility_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND facility = '", facility_filter, "'")
  }
  
  # FOS Area filter uses foreman field
  if (!is.null(fosarea_filter) && fosarea_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND foreman = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT left(sitecode, 4) as towncode
    FROM public.", JK_TABLE, "
    WHERE ", where_conditions, "
    ORDER BY left(sitecode, 4)
  ")
  
  data <- dbGetQuery(con, query)
  return(data$towncode)
}

#' Get sections filtered by town code from JK table
#'
#' Returns sections (left 7 chars of sitecode) based on filters.
#' NOTE: Ideally, sections shown should be limited to those with data for the selected foreman.
#' This ensures users see sections where the foreman has assigned sites.
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
  # NOTE: foreman IS NOT NULL to match breeding data availability
  where_conditions <- "sitecode IS NOT NULL AND foreman IS NOT NULL"
  
  if (!is.null(towncode_filter) && towncode_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND left(sitecode, 4) = '", towncode_filter, "'")
  }
  
  if (!is.null(facility_filter) && facility_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND facility = '", facility_filter, "'")
  }
  
  # FOS Area filter uses foreman field
  if (!is.null(fosarea_filter) && fosarea_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND foreman = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT left(sitecode, 7) as sectcode
    FROM public.", JK_TABLE, "
    WHERE ", where_conditions, "
    ORDER BY left(sitecode, 7)
  ")
  
  data <- dbGetQuery(con, query)
  return(data$sectcode)
}

#' Get breeding sites data from JK table (NO gis_sectcode join)
#'
#' Retrieves all breeding site data directly from the JK table.
#' facility, zone, and foreman (FOS Area) all come from the JK table itself.
#' The foreman field is NOT NULL, providing the definitive FOS Area for each site.
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
      left(sitecode, 7) as section", dynamic_select, "
    FROM public.", JK_TABLE, "
    ORDER BY sitecode
  ")
  
  message("[section_cards] Loading breeding sites from JK table...")
  data <- dbGetQuery(con, query)
  message(sprintf("[section_cards] Loaded %d breeding sites with %d columns", nrow(data), ncol(data)))
  
  # Sanitize data: normalize NULLs, whitespace, case
  data <- sanitize_breeding_data(data)
  
  return(data)
}

# =============================================================================
# WEBSTER TABLE FUNCTIONS (public.loc_breeding_sites + gis_sectcode)
# =============================================================================

#' Get breeding sites from the original Webster table (loc_breeding_sites)
#'
#' Uses gis_sectcode for zone/fosarea (like structures do).
#' No dynamic columns — only the standard core fields.
#'
#' @return A data frame with breeding site information
#' @export
get_webster_breeding_sites <- function() {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
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
      COALESCE(g.facility, b.facility) as facility,
      g.zone,
      g.fosarea,
      g.sectcode as section
    FROM public.", WEBSTER_TABLE, " b
    LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode, 7)
    WHERE b.enddate IS NULL
    ORDER BY b.sitecode
  ")
  
  message("[section_cards] Loading breeding sites from Webster table (loc_breeding_sites)...")
  data <- dbGetQuery(con, query)
  message(sprintf("[section_cards] Loaded %d Webster breeding sites", nrow(data)))
  
  # Sanitize data
  data <- sanitize_breeding_data(data)
  
  return(data)
}

#' Get filter options from Webster table
#'
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area filter
#' @return A list with facility, section, and fosarea choices
#' @export
get_webster_filter_options <- function(facility_filter = NULL, fosarea_filter = NULL) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  where_conditions <- "b.enddate IS NULL AND g.facility IS NOT NULL AND b.sitecode IS NOT NULL"
  
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
    FROM public.", WEBSTER_TABLE, " b
    LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode, 7)
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

#' Get town codes from Webster table
#'
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area filter
#' @return A vector of unique town codes
#' @export
get_webster_town_codes <- function(facility_filter = NULL, fosarea_filter = NULL) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  where_conditions <- "b.enddate IS NULL AND b.sitecode IS NOT NULL"
  
  if (!is.null(facility_filter) && facility_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND COALESCE(g.facility, b.facility) = '", facility_filter, "'")
  }
  if (!is.null(fosarea_filter) && fosarea_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND g.fosarea = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT left(b.sitecode, 4) as towncode
    FROM public.", WEBSTER_TABLE, " b
    LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode, 7)
    WHERE ", where_conditions, "
    ORDER BY left(b.sitecode, 4)
  ")
  
  data <- dbGetQuery(con, query)
  return(data$towncode)
}

#' Get sections from Webster table filtered by town code
#'
#' @param towncode_filter Optional town code filter
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area filter
#' @return A vector of unique sections
#' @export
get_webster_sections_by_towncode <- function(towncode_filter = NULL, facility_filter = NULL, fosarea_filter = NULL) {
  con <- get_db_connection()
  on.exit(safe_disconnect(con), add = TRUE)
  
  where_conditions <- "b.enddate IS NULL AND b.sitecode IS NOT NULL"
  
  if (!is.null(towncode_filter) && towncode_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND left(b.sitecode, 4) = '", towncode_filter, "'")
  }
  if (!is.null(facility_filter) && facility_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND COALESCE(g.facility, b.facility) = '", facility_filter, "'")
  }
  if (!is.null(fosarea_filter) && fosarea_filter != "all") {
    where_conditions <- paste0(where_conditions, " AND g.fosarea = '", fosarea_filter, "'")
  }
  
  query <- paste0("
    SELECT DISTINCT g.sectcode
    FROM public.", WEBSTER_TABLE, " b
    LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode, 7)
    WHERE ", where_conditions, " AND g.sectcode IS NOT NULL
    ORDER BY g.sectcode
  ")
  
  data <- dbGetQuery(con, query)
  return(data$sectcode)
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
