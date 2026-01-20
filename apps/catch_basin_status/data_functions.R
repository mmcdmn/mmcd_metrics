# Catch Basin Status - Data Functions
# Functions for fetching and processing catch basin status data
# Note: db_helpers.R is sourced by app.R before this file

# Standard column names used across all apps:
# - total_count: Total items in this group
# - active_count: Items with active treatment (includes expiring)
# - expiring_count: Items expiring within expiring_days
# - expired_count: Items with expired treatment
# - display_name: Human-readable group name

# Load catch basin status data
load_catch_basin_data <- function(facility_filter = "all", foreman_filter = "all", 
                                   zone_filter = c("1", "2"), analysis_date = Sys.Date(),
                                   expiring_days = 14) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Build facility filter clause
    facility_clause <- ""
    if (!("all" %in% facility_filter) && length(facility_filter) > 0) {
      facility_list <- paste0("'", facility_filter, "'", collapse = ", ")
      facility_clause <- paste0("WHERE g.abbrv IN (", facility_list, ")")
    } else {
      facility_clause <- "WHERE g.abbrv IN ('N','E','MO','Sr','Sj','Wm','Wp')"
    }
    
    # Build foreman filter
    foreman_where <- ""
    if (!("all" %in% foreman_filter) && length(foreman_filter) > 0) {
      foreman_list <- paste0("'", foreman_filter, "'", collapse = ", ")
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
           COALESCE(wet.wet_cb_count, 0)::integer AS wet_cb_count,
           COALESCE(t.count_wet_activetrt, 0)::integer AS count_wet_activetrt,
           COALESCE(t.count_wet_expiring, 0)::integer AS count_wet_expiring,
           COALESCE(t.count_wet_expired, 0)::integer AS count_wet_expired
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
            count(*) AS wet_cb_count
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
             count(*) FILTER (WHERE cbstat.active_status IN ('treated','expiring')) AS count_wet_activetrt,
             count(*) FILTER (WHERE cbstat.active_status = 'expiring') AS count_wet_expiring,
             count(*) FILTER (WHERE cbstat.active_status = 'expired') AS count_wet_expired
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
    
    # Ensure numeric types (should be handled by query, but double-check)
    data$wet_cb_count <- as.integer(data$wet_cb_count)
    data$count_wet_activetrt <- as.integer(data$count_wet_activetrt)
    data$count_wet_expiring <- as.integer(data$count_wet_expiring)
    data$count_wet_expired <- as.integer(data$count_wet_expired)
    
    # Add standardized column names (aliases for consistent API)
    data$total_count <- data$wet_cb_count
    data$active_count <- data$count_wet_activetrt
    data$expiring_count <- data$count_wet_expiring
    data$expired_count <- data$count_wet_expired
    
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
    
    return(data)
    
  }, error = function(e) {
    warning(paste("Error loading catch basin data:", e$message))
    if (exists("con") && !is.null(con)) {
      safe_disconnect(con)
    }
    return(data.frame())
  })
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
      filter(count_wet_expiring > 0)
  } else if (expiring_filter == "expiring_expired") {
    # Keep only sites that are expiring or expired
    data <- data %>%
      filter(count_wet_expiring > 0 | count_wet_expired > 0)
  }
  
  # Apply grouping
  if (group_by == "mmcd_all") {
    # Aggregate all data into single row
    processed <- data %>%
      summarise(
        display_name = "All MMCD",
        group_name = "mmcd_all",
        wet_cb_count = sum(wet_cb_count, na.rm = TRUE),
        count_wet_activetrt = sum(count_wet_activetrt, na.rm = TRUE),
        count_wet_expiring = sum(count_wet_expiring, na.rm = TRUE),
        count_wet_expired = sum(count_wet_expired, na.rm = TRUE)
      )
  } else if (group_by == "facility") {
    # Group by facility (and optionally zone)
    if (combine_zones) {
      # Combine zones within each facility
      processed <- data %>%
        group_by(facility, facility_full) %>%
        summarise(
          wet_cb_count = sum(wet_cb_count, na.rm = TRUE),
          count_wet_activetrt = sum(count_wet_activetrt, na.rm = TRUE),
          count_wet_expiring = sum(count_wet_expiring, na.rm = TRUE),
          count_wet_expired = sum(count_wet_expired, na.rm = TRUE),
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
          wet_cb_count = sum(wet_cb_count, na.rm = TRUE),
          count_wet_activetrt = sum(count_wet_activetrt, na.rm = TRUE),
          count_wet_expiring = sum(count_wet_expiring, na.rm = TRUE),
          count_wet_expired = sum(count_wet_expired, na.rm = TRUE),
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
          wet_cb_count = sum(wet_cb_count, na.rm = TRUE),
          count_wet_activetrt = sum(count_wet_activetrt, na.rm = TRUE),
          count_wet_expiring = sum(count_wet_expiring, na.rm = TRUE),
          count_wet_expired = sum(count_wet_expired, na.rm = TRUE),
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
          wet_cb_count = sum(wet_cb_count, na.rm = TRUE),
          count_wet_activetrt = sum(count_wet_activetrt, na.rm = TRUE),
          count_wet_expiring = sum(count_wet_expiring, na.rm = TRUE),
          count_wet_expired = sum(count_wet_expired, na.rm = TRUE),
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
          wet_cb_count = sum(wet_cb_count, na.rm = TRUE),
          count_wet_activetrt = sum(count_wet_activetrt, na.rm = TRUE),
          count_wet_expiring = sum(count_wet_expiring, na.rm = TRUE),
          count_wet_expired = sum(count_wet_expired, na.rm = TRUE),
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
          wet_cb_count = sum(wet_cb_count, na.rm = TRUE),
          count_wet_activetrt = sum(count_wet_activetrt, na.rm = TRUE),
          count_wet_expiring = sum(count_wet_expiring, na.rm = TRUE),
          count_wet_expired = sum(count_wet_expired, na.rm = TRUE),
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
      pct_treated = ifelse(wet_cb_count > 0, 
                          (count_wet_activetrt / wet_cb_count) * 100, 
                          0),
      untreated_count = wet_cb_count - count_wet_activetrt,
      # Add standardized column names
      total_count = wet_cb_count,
      active_count = count_wet_activetrt,
      expiring_count = count_wet_expiring,
      expired_count = count_wet_expired
    )
  
  # Remove rows with no data
  processed <- processed %>%
    filter(wet_cb_count > 0 | count_wet_activetrt > 0)
  
  return(processed)
}
