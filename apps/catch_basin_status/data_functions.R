# Catch Basin Status - Data Functions
# Functions for fetching and processing catch basin status data

# Source shared helpers (only if not already loaded - allows use from overview apps)
if (!exists("get_db_connection", mode = "function")) {
  source("../../shared/db_helpers.R")
}

# Standard column names used across all apps:
# - total_count: Total items in this group
# - active_count: Items with active treatment (includes expiring)
# - expiring_count: Items expiring within expiring_days
# - expired_count: Items with expired treatment
# - display_name: Human-readable group name

# Load catch basin status data (standard function - renamed from load_catch_basin_data)
load_raw_data <- function(analysis_date = Sys.Date(), include_archive = FALSE,
                          start_year = NULL, end_year = NULL, include_geometry = FALSE,
                          facility_filter = "all", foreman_filter = "all", 
                          zone_filter = c("1", "2"), expiring_days = 14) {
  # Historical archive mode: return individual treatment rows for cache/charts
  if (isTRUE(include_archive) && !is.null(start_year) && !is.null(end_year)) {
    return(load_historical_treatments(start_year, end_year, zone_filter))
  }
  
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Build facility filter clause using shared helper
    facility_clause <- ""
    if (is_valid_filter(facility_filter)) {
      facility_list <- build_sql_in_list(facility_filter)
      facility_clause <- paste0("WHERE g.abbrv IN (", facility_list, ")")
    } else {
      facility_clause <- "WHERE g.abbrv IN ('N','E','MO','Sr','Sj','Wm','Wp')"
    }
    
    # Build foreman filter using shared helper
    foreman_where <- ""
    if (is_valid_filter(foreman_filter)) {
      foreman_list <- build_sql_in_list(foreman_filter)
      foreman_where <- paste0("AND sc.fosarea IN (", foreman_list, ")")
    }
    
    # Build zone filter
    zone_where <- ""
    if (length(zone_filter) == 1) {
      zone_where <- paste0("AND sc.zone = '", zone_filter, "'")
    } else if (length(zone_filter) == 2) {
      zone_where <- "AND sc.zone IN ('1', '2')"
    }
    
    # Determine which tables have data for this analysis_date
    table_info <- get_table_strategy(analysis_date)
    insptrt_source <- table_info$insptrt_source
    
    # Main query 
    query <- sprintf("
    SELECT o.facility,
           o.zone,
           o.fosarea,
           o.sectcode,
           COALESCE(wet.total_count, 0)::integer AS total_count,
           COALESCE(t.active_count, 0)::integer AS active_count,
           COALESCE(t.expiring_count, 0)::integer AS expiring_count,
           COALESCE(t.expired_count, 0)::integer AS expired_count
    FROM (SELECT DISTINCT g.abbrv AS facility, sc.zone, sc.fosarea, sc.sectcode 
          FROM gis_facility g
          CROSS JOIN gis_sectcode sc
          %s
          %s
          %s
         ) o 
    
    LEFT JOIN 
    (SELECT loc_catchbasin.facility,
            left(loc_catchbasin.sitecode,7) as sectcode,
            count(*) AS total_count
     FROM loc_catchbasin
     LEFT JOIN gis_sectcode sc ON left(loc_catchbasin.sitecode,7)=sc.sectcode
     WHERE (loc_catchbasin.enddate IS NULL OR loc_catchbasin.enddate > '%s'::date)
       AND loc_catchbasin.lettergrp::text <> 'Z'::text
       AND loc_catchbasin.status_udw='W'
       %s
       %s
     GROUP BY loc_catchbasin.facility, left(loc_catchbasin.sitecode,7)
    ) wet ON wet.facility=o.facility AND wet.sectcode=o.sectcode
    
    LEFT JOIN
    (
      SELECT cbstat.facility,
             cbstat.sectcode,
             count(*) FILTER (WHERE cbstat.active_status IN ('treated','expiring')) AS active_count,
             count(*) FILTER (WHERE cbstat.active_status = 'expiring') AS expiring_count,
             count(*) FILTER (WHERE cbstat.active_status = 'expired') AS expired_count
      FROM 		  
      -- Recreating catchbasin_status to include Facility
      ( SELECT s_tcb.gid,
               s_tcb.facility,
               s_tcb.sectcode,
               CASE
                   WHEN s_tcb.status_udw::text = 'W'::text AND s_tcb.enddate IS NOT NULL THEN 'X'::character varying(1)
                   WHEN s_tcb.status_udw::text = 'W'::text AND s_tcb.lettergrp::text = 'Z'::text THEN 'Z'::character varying(1)
                   ELSE COALESCE(s_tcb.status_udw, 'U'::character varying)::character varying(1)
               END AS status_udw,
               CASE
                   WHEN s_tcb.age > COALESCE(s_tcb.effect_days::integer, 150)::double precision THEN 'expired'::text
                   WHEN s_tcb.age > (COALESCE(s_tcb.effect_days::integer, 150) - %d)::double precision THEN
                   CASE
                       WHEN s_tcb.status::text = 'S'::text THEN 'skipped-expiring'::text
                       ELSE 'expiring'::text
                   END
                   WHEN s_tcb.status::text = 'S'::text THEN 'skipped'::text
                   WHEN s_tcb.status::text = 'P'::text THEN 'planned'::text
                   ELSE 'treated'::text
               END AS active_status,
               s_tcb.insptrt_id
        FROM ( SELECT DISTINCT ON (loc_catchbasin.gid) loc_catchbasin.gid,
                      loc_catchbasin.facility,
                      left(loc_catchbasin.sitecode,7) as sectcode,
                      loc_catchbasin.status_udw,
                      loc_catchbasin.lettergrp,
                      loc_catchbasin.enddate,
                      insptrt.pkey_pg AS insptrt_id,
                      date_part('days'::text, '%s'::timestamp - insptrt.inspdate::timestamp with time zone) AS age,
                      dblarv_treatment_catchbasin.status,
                      mattype_list_targetdose.effect_days,
                      mattype_list_targetdose.days_retrt_early
               FROM %s AS insptrt
               JOIN dblarv_treatment_catchbasin ON insptrt.pkey_pg = dblarv_treatment_catchbasin.treatment_id
               JOIN loc_catchbasin ON dblarv_treatment_catchbasin.catchbasin_id = loc_catchbasin.gid
               JOIN mattype_list_targetdose USING (matcode)
               LEFT JOIN gis_sectcode sc ON left(loc_catchbasin.sitecode,7)=sc.sectcode
               WHERE insptrt.inspdate <= '%s'::date
               %s
               %s
               ORDER BY loc_catchbasin.gid, insptrt.inspdate DESC, insptrt.insptime DESC) s_tcb
        ORDER BY s_tcb.sectcode, s_tcb.status_udw, s_tcb.gid
      ) cbstat
      WHERE cbstat.status_udw::text = 'W'::text
      GROUP BY cbstat.facility, cbstat.sectcode
    ) t ON t.facility=o.facility AND t.sectcode=o.sectcode
    
    ORDER BY o.facility, o.zone, o.fosarea, o.sectcode
    ", facility_clause, foreman_where, zone_where, 
       analysis_date, foreman_where, zone_where, 
       expiring_days, analysis_date, insptrt_source, analysis_date, foreman_where, zone_where)
    
    data <- dbGetQuery(con, query)
    safe_disconnect(con)
    
    # Ensure numeric types
    data$total_count <- as.integer(data$total_count)
    data$active_count <- as.integer(data$active_count)
    data$expiring_count <- as.integer(data$expiring_count)
    data$expired_count <- as.integer(data$expired_count)
    
    # Map facility short names to full names using db_helpers
    facilities <- get_facility_lookup()
    if (!is.null(facilities) && nrow(facilities) > 0) {
      facility_map <- setNames(facilities$full_name, facilities$short_name)
      data$facility_full <- ifelse(
        data$facility %in% names(facility_map),
        facility_map[data$facility],
        data$facility
      )
    } else {
      data$facility_full <- data$facility
    }
    
    # Map foreman numbers to names using db_helpers
    foremen <- get_foremen_lookup()
    if (!is.null(foremen) && nrow(foremen) > 0) {
      foreman_map <- setNames(foremen$shortname, foremen$emp_num)
      data$foreman_name <- ifelse(
        data$fosarea %in% names(foreman_map),
        foreman_map[data$fosarea],
        data$fosarea
      )
    } else {
      data$foreman_name <- data$fosarea
    }
    
    # Calculate total catch basins from aggregated section data
    total_count <- sum(data$total_count, na.rm = TRUE)
    
    # Return STANDARDIZED format
    # Note: catch_basin is PRE-AGGREGATED for performance (50K+ sites)
    # Instead of is_active/is_expiring per site, we have active_count/expiring_count per sectcode
    # Set pre_aggregated flag so unified functions know to sum counts instead of counting rows
    return(list(
      sites = data,
      treatments = data,
      total_count = total_count,
      pre_aggregated = TRUE  # Flag indicating counts are already aggregated
    ))
    
  }, error = function(e) {
    warning(paste("Error loading catch basin data:", e$message))
    if (exists("con") && !is.null(con)) {
      safe_disconnect(con)
    }
    return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
  })
}

#' Apply filters to catch basin aggregated data - STANDARDIZED FORMAT
#' Standard function to filter the results from load_raw_data
#' @param data The result from load_raw_data (list with sites, treatments, total_count)
#' @param facility_filter Vector of selected facilities  
#' @param foreman_filter Vector of selected foremen
#' @param zone_filter Vector of selected zones
#' @return Filtered data list with standardized keys
apply_data_filters <- function(data, facility_filter = NULL,
                              foreman_filter = NULL, zone_filter = NULL) {
  # Use standardized keys
  sites <- data$sites
  treatments <- data$treatments
  
  if (is.null(sites) || nrow(sites) == 0) {
    return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
  }
  
  # Apply facility filter
  if (!is.null(facility_filter) && !("all" %in% facility_filter) && length(facility_filter) > 0) {
    sites <- sites %>% filter(facility %in% facility_filter)
    treatments <- treatments %>% filter(facility %in% facility_filter)
  }
  
  # Apply foreman filter  
  if (!is.null(foreman_filter) && !("all" %in% foreman_filter) && length(foreman_filter) > 0) {
    sites <- sites %>% filter(fosarea %in% foreman_filter)
    treatments <- treatments %>% filter(fosarea %in% foreman_filter)
  }
  
  # Apply zone filter
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    sites <- sites %>% filter(zone %in% zone_filter)
    treatments <- treatments %>% filter(zone %in% zone_filter)
  }
  
  # Return STANDARDIZED format - preserve pre_aggregated flag
  return(list(
    sites = sites,
    treatments = treatments,
    total_count = data$total_count,  # Keep original total_count from universe
    pre_aggregated = data$pre_aggregated  # Preserve pre_aggregated flag
  ))
}

# Process catch basin data for display - aggregates by group_by with standard column names
process_catch_basin_data <- function(data, group_by = "facility", combine_zones = FALSE, expiring_filter = "all") {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  # Apply expiring filter first
  if (expiring_filter == "expiring") {
    # Keep only sites that are expiring
    data <- data %>%
      filter(expiring_count > 0)
  } else if (expiring_filter == "expiring_expired") {
    # Keep only sites that are expiring or expired
    data <- data %>%
      filter(expiring_count > 0 | expired_count > 0)
  }
  
  # Apply grouping
  if (group_by == "mmcd_all") {
    # Aggregate all data into single row
    processed <- data %>%
      summarise(
        display_name = "All MMCD",
        group_name = "mmcd_all",
        total_count = sum(total_count, na.rm = TRUE),
        active_count = sum(active_count, na.rm = TRUE),
        expiring_count = sum(expiring_count, na.rm = TRUE),
        expired_count = sum(expired_count, na.rm = TRUE)
      )
  } else if (group_by == "facility") {
    # Group by facility (and optionally zone)
    if (combine_zones) {
      # Combine zones within each facility
      processed <- data %>%
        group_by(facility, facility_full) %>%
        summarise(
          total_count = sum(total_count, na.rm = TRUE),
          active_count = sum(active_count, na.rm = TRUE),
          expiring_count = sum(expiring_count, na.rm = TRUE),
          expired_count = sum(expired_count, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          display_name = facility_full,
          group_name = facility
        )
    } else {
      # Keep zones separate
      processed <- data %>%
        group_by(facility, facility_full, zone) %>%
        summarise(
          total_count = sum(total_count, na.rm = TRUE),
          active_count = sum(active_count, na.rm = TRUE),
          expiring_count = sum(expiring_count, na.rm = TRUE),
          expired_count = sum(expired_count, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          display_name = paste0(facility_full, " P", zone),
          group_name = paste0(facility, "_", zone)
        )
    }
  } else if (group_by == "foreman") {
    # Group by foreman
    if (combine_zones) {
      processed <- data %>%
        group_by(fosarea, foreman_name) %>%
        summarise(
          total_count = sum(total_count, na.rm = TRUE),
          active_count = sum(active_count, na.rm = TRUE),
          expiring_count = sum(expiring_count, na.rm = TRUE),
          expired_count = sum(expired_count, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          display_name = foreman_name,
          group_name = fosarea
        )
    } else {
      processed <- data %>%
        group_by(fosarea, foreman_name, zone) %>%
        summarise(
          total_count = sum(total_count, na.rm = TRUE),
          active_count = sum(active_count, na.rm = TRUE),
          expiring_count = sum(expiring_count, na.rm = TRUE),
          expired_count = sum(expired_count, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          display_name = paste0(foreman_name, " P", zone),
          group_name = paste0(fosarea, "_", zone)
        )
    }
  } else if (group_by == "sectcode") {
    # Group by section
    if (combine_zones) {
      processed <- data %>%
        group_by(sectcode) %>%
        summarise(
          total_count = sum(total_count, na.rm = TRUE),
          active_count = sum(active_count, na.rm = TRUE),
          expiring_count = sum(expiring_count, na.rm = TRUE),
          expired_count = sum(expired_count, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          display_name = sectcode,
          group_name = sectcode
        )
    } else {
      processed <- data %>%
        group_by(sectcode, zone) %>%
        summarise(
          total_count = sum(total_count, na.rm = TRUE),
          active_count = sum(active_count, na.rm = TRUE),
          expiring_count = sum(expiring_count, na.rm = TRUE),
          expired_count = sum(expired_count, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          display_name = paste0(sectcode, " P", zone),
          group_name = paste0(sectcode, "_", zone)
        )
    }
  } else {
    # Default to facility grouping
    processed <- data %>%
      mutate(
        display_name = facility,
        group_name = facility
      )
  }
  
  # Add calculated fields
  processed <- processed %>%
    mutate(
      pct_treated = ifelse(total_count > 0, 
                          (active_count / total_count) * 100, 
                          0),
      untreated_count = total_count - active_count
    )
  
  # Remove rows with no data
  processed <- processed %>%
    filter(total_count > 0 | active_count > 0)
  
  return(processed)
}

# =============================================================================
# HISTORICAL TREATMENTS LOADER - For overview dashboards
# =============================================================================

#' Load raw historical treatments for catch basins
#' Returns individual treatments with inspdate for weekly aggregation
#' Standardized interface matching drone/ground_prehatch for overview use
#' 
#' @param start_year Start year for historical range
#' @param end_year End year for historical range  
#' @param zone_filter Vector of zones to include
#' @return List with $treatments data frame containing inspdate, facility, zone columns
#' @export
load_historical_treatments <- function(start_year, end_year, zone_filter = c("1", "2")) {
  con <- get_db_connection()
  if (is.null(con)) return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
  
  tryCatch({
    # Build zone filter
    zone_condition <- ""
    if (!is.null(zone_filter) && length(zone_filter) > 0) {
      zone_list <- paste0("'", paste(zone_filter, collapse = "','"), "'")
      zone_condition <- sprintf("AND sc.zone IN (%s)", zone_list)
    }
    
    # Use shared function to determine which years are in which table
    year_ranges <- get_historical_year_ranges(con, "dblarv_insptrt_current", "dblarv_insptrt_archive", "inspdate")
    current_years <- year_ranges$current_years
    archive_years <- year_ranges$archive_years
    
    cat("DEBUG load_historical_treatments: current_years =", paste(current_years, collapse = ","), 
        "archive_years =", paste(range(archive_years), collapse = "-"), "\n")
    
    treatments <- data.frame()
    
    # Get data from CURRENT table for recent years
    current_year_range <- start_year:end_year
    if (length(current_year_range) > 0) {
      query_current <- sprintf("
        SELECT 
          trt.inspdate,
          loc.facility,
          sc.zone,
          loc.gid as catchbasin_id,
          COALESCE(mat.effect_days, 28) as effect_days
        FROM dblarv_insptrt_current trt
        JOIN dblarv_treatment_catchbasin tcb ON trt.pkey_pg = tcb.treatment_id
        JOIN loc_catchbasin loc ON tcb.catchbasin_id = loc.gid
        JOIN mattype_list_targetdose mat USING (matcode)
        LEFT JOIN gis_sectcode sc ON left(loc.sitecode, 7) = sc.sectcode
        WHERE EXTRACT(YEAR FROM trt.inspdate) BETWEEN %d AND %d
          AND loc.status_udw = 'W'
          AND loc.lettergrp <> 'Z'
          %s
      ", min(current_year_range) - 1, max(current_year_range), zone_condition)
      
      cat("DEBUG: Getting current table data for years", min(current_year_range), "-", max(current_year_range), "\n")
      current_data <- dbGetQuery(con, query_current)
      cat("DEBUG: Current table returned", nrow(current_data), "rows\n")
      treatments <- bind_rows(treatments, current_data)
    }
    
    # Get data from ARCHIVE table for historical years
    # Query full requested range - database will return what exists
    if (start_year < min(current_years, na.rm = TRUE)) {
      query_archive <- sprintf("
        SELECT 
          trt.inspdate,
          loc.facility,
          sc.zone,
          loc.gid as catchbasin_id,
          COALESCE(mat.effect_days, 28) as effect_days
        FROM dblarv_insptrt_archive trt
        JOIN loc_catchbasin loc ON trt.sitecode = loc.sitecode
        JOIN mattype_list_targetdose mat USING (matcode)
        LEFT JOIN gis_sectcode sc ON left(loc.sitecode, 7) = sc.sectcode
        WHERE EXTRACT(YEAR FROM trt.inspdate) BETWEEN %d AND %d
          AND trt.action = '6'
          AND loc.status_udw = 'W'
          AND loc.lettergrp <> 'Z'
          %s
      ", start_year - 1, end_year, zone_condition)
      
      cat("DEBUG: Getting archive table data for years", start_year - 1, "-", end_year, "\n")
      archive_data <- dbGetQuery(con, query_archive)
      cat("DEBUG: Archive table returned", nrow(archive_data), "rows\n")
      treatments <- bind_rows(treatments, archive_data)
    }
    
    safe_disconnect(con)
    
    if (nrow(treatments) > 0) {
      treatments <- treatments %>% mutate(inspdate = as.Date(inspdate))
    }
    
    cat("DEBUG: Total treatments loaded:", nrow(treatments), "\n")
    
    # Return in standardized format matching other apps
    list(
      sites = data.frame(),
      treatments = treatments,
      total_count = nrow(treatments)
    )
    
  }, error = function(e) {
    warning(paste("Error loading catch basin historical treatments:", e$message))
    if (!is.null(con)) safe_disconnect(con)
    list(sites = data.frame(), treatments = data.frame(), total_count = 0)
  })
}
