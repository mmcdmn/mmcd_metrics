# Catch Basin Status - Data Functions
# Functions for fetching and processing catch basin status data
# Note: db_helpers.R is sourced by app.R before this file

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
     WHERE loc_catchbasin.enddate IS NULL 
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
                      dblarv_insptrt_current.pkey_pg AS insptrt_id,
                      date_part('days'::text, '%s'::timestamp - dblarv_insptrt_current.inspdate::timestamp with time zone) AS age,
                      dblarv_treatment_catchbasin.status,
                      mattype_list_targetdose.effect_days,
                      mattype_list_targetdose.days_retrt_early
               FROM dblarv_insptrt_current
               JOIN dblarv_treatment_catchbasin ON dblarv_insptrt_current.pkey_pg = dblarv_treatment_catchbasin.treatment_id
               JOIN loc_catchbasin ON dblarv_treatment_catchbasin.catchbasin_id = loc_catchbasin.gid
               JOIN mattype_list_targetdose USING (matcode)
               LEFT JOIN gis_sectcode sc ON left(loc_catchbasin.sitecode,7)=sc.sectcode
               WHERE dblarv_insptrt_current.inspdate <= '%s'::date
               %s
               %s
               ORDER BY loc_catchbasin.gid, dblarv_insptrt_current.inspdate DESC, dblarv_insptrt_current.insptime DESC) s_tcb
        ORDER BY s_tcb.sectcode, s_tcb.status_udw, s_tcb.gid
      ) cbstat
      WHERE cbstat.status_udw::text = 'W'::text
      GROUP BY cbstat.facility, cbstat.sectcode
    ) t ON t.facility=o.facility AND t.sectcode=o.sectcode
    
    ORDER BY o.facility, o.zone, o.fosarea, o.sectcode
    ", facility_clause, foreman_where, zone_where, foreman_where, zone_where, 
       expiring_days, analysis_date, analysis_date, foreman_where, zone_where)
    
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
    
    # Return STANDARDIZED format - sites/treatments are same for aggregated data
    return(list(
      sites = data,
      treatments = data,
      total_count = total_count
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
  
  # Return STANDARDIZED format
  return(list(
    sites = sites,
    treatments = treatments,
    total_count = data$total_count  # Keep original total_count from universe
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
