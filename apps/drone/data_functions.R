# Data functions for drone app - handles all database queries and data processing

#' Load raw data from database
#' @param drone_types Vector of drone type selections
#' @return List with drone_sites and drone_treatments data frames
load_raw_data <- function(drone_types = c("Y", "M", "C")) {
  con <- get_db_connection()
  if (is.null(con)) return(list(drone_sites = data.frame(), drone_treatments = data.frame()))
  
  # Build the drone designation filter based on user selection
  drone_types_str <- paste0("'", paste(drone_types, collapse = "','"), "'")
  
  # Query to get drone sites from loc_breeding_sites
  drone_sites_query <- sprintf("
  SELECT b.sitecode, b.facility, b.acres, b.prehatch, b.drone, 
         CASE 
           WHEN e.emp_num IS NOT NULL AND e.active = true THEN sc.fosarea
           ELSE NULL
         END as foreman, 
         sc.zone,
         left(b.sitecode,7) as sectcode
  FROM public.loc_breeding_sites b
  LEFT JOIN public.gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
  LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
    AND e.emp_type = 'FieldSuper' 
    AND e.active = true
  WHERE (b.drone IN (%s) OR b.air_gnd = 'D')
  AND b.enddate IS NULL
  ", drone_types_str)
  
  drone_sites <- dbGetQuery(con, drone_sites_query)
  
  # Query to get treatment information with recorded treated acres
  treatments_query <- "
  SELECT t.sitecode, t.facility, t.inspdate, t.matcode, t.acres as treated_acres, t.foreman, m.effect_days
  FROM public.dblarv_insptrt_current t
  LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
  WHERE (t.airgrnd_plan = 'D' OR t.action = 'D')
  "
  
  treatments <- dbGetQuery(con, treatments_query)
  
  dbDisconnect(con)
  
  # Process the data
  # Identify drone sites with treatments
  drone_treatments <- treatments %>%
    inner_join(drone_sites, by = c("sitecode", "facility"), suffix = c("_trt", "_site")) %>%
    mutate(
      # Use recorded treated acres from treatment records
      acres = treated_acres,
      # Use site foreman (jurisdictional assignment). Ignore treatment foreman
      foreman = foreman_site,
      prehatch = prehatch,
      drone = drone,
      zone = zone,
      sectcode = sectcode
    )
  
  # Calculate treatment status (active)
  current_date <- Sys.Date()
  drone_treatments <- drone_treatments %>%
    mutate(
      inspdate = as.Date(inspdate),
      effect_days = ifelse(is.na(effect_days), 0, effect_days),
      treatment_end_date = inspdate + effect_days,
      is_active = treatment_end_date >= current_date
    )
  
  # Return all the data needed for filtering later
  return(list(
    drone_sites = drone_sites,
    drone_treatments = drone_treatments
  ))
}

#' Apply filters to drone data
#' @param data List containing drone_sites and drone_treatments
#' @param facility_filter Vector of selected facilities  
#' @param foreman_filter Vector of selected foremen
#' @param prehatch_only Boolean for prehatch filter
#' @return Filtered data list
apply_data_filters <- function(data, facility_filter = NULL, 
                               foreman_filter = NULL, prehatch_only = FALSE) {
  
  drone_sites <- data$drone_sites
  drone_treatments <- data$drone_treatments
  
  # Note: Zone filtering is handled in processed_data() after database joins
  
  # Apply facility filter
  if (!is.null(facility_filter) && length(facility_filter) > 0 && !("all" %in% facility_filter)) {
    drone_sites <- drone_sites %>% filter(facility %in% facility_filter)
    drone_treatments <- drone_treatments %>% filter(facility %in% facility_filter)
  }
  
  # Apply foreman/FOS filter
  if (!is.null(foreman_filter) && length(foreman_filter) > 0 && !("all" %in% foreman_filter)) {
    # Convert shortnames to employee numbers for filtering
    foremen_lookup <- get_foremen_lookup()
    selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% foreman_filter]
    
    drone_sites <- drone_sites %>% filter(foreman %in% selected_emp_nums)
    drone_treatments <- drone_treatments %>% filter(foreman %in% selected_emp_nums)
  }
  
  # Apply prehatch filter
  if (prehatch_only) {
    drone_sites <- drone_sites %>% filter(prehatch == 'PREHATCH')
    drone_treatments <- drone_treatments %>% filter(prehatch == 'PREHATCH')
  }
  
  return(list(
    drone_sites = drone_sites,
    drone_treatments = drone_treatments
  ))
}

#' Get individual sitecode treatment data for site statistics
#' @param start_year Start year for query
#' @param end_year End year for query
#' @param zone_filter Vector of selected zones
#' @param facility_filter Vector of selected facilities
#' @param foreman_filter Vector of selected foremen
#' @param prehatch_only Boolean for prehatch filter
#' @return Data frame with individual treatment records
get_sitecode_data <- function(start_year, end_year, zone_filter, facility_filter, foreman_filter, prehatch_only) {
  # Get database connection and calculate directly in SQL to avoid duplication
  con <- get_db_connection()
  if (is.null(con)) {
    return(data.frame())
  }
  
  # Get individual treatment records - each treatment as a separate row
  query <- sprintf("
  WITH treatment_data AS (
      SELECT 
          t.facility, 
          t.sitecode, 
          t.inspdate, 
          t.matcode, 
          t.amts as amount_used,
          t.acres as recorded_acres,
          t.foreman as treatment_foreman,
          EXTRACT(YEAR FROM t.inspdate) as year
      FROM public.dblarv_insptrt_current t
      WHERE (t.airgrnd_plan = 'D' OR t.action = 'D')
          AND EXTRACT(YEAR FROM t.inspdate) BETWEEN %d AND %d
          AND t.matcode IS NOT NULL
          AND t.acres IS NOT NULL
          AND t.acres > 0
      
      UNION ALL
      
      SELECT 
          t.facility, 
          t.sitecode, 
          t.inspdate, 
          t.matcode, 
          t.amts as amount_used,
          t.acres as recorded_acres,
          t.foreman as treatment_foreman,
          EXTRACT(YEAR FROM t.inspdate) as year
      FROM public.dblarv_insptrt_archive t
      WHERE t.action = 'D'
          AND EXTRACT(YEAR FROM t.inspdate) BETWEEN %d AND %d
          AND t.matcode IS NOT NULL
          AND t.acres IS NOT NULL
          AND t.acres > 0
  ),
  -- Get deduplicated location data for prehatch info and zone
  location_data AS (
      SELECT DISTINCT ON (b.sitecode, b.facility)
          b.sitecode, 
          b.facility, 
          b.prehatch,
          CASE 
            WHEN e.emp_num IS NOT NULL AND e.active = true THEN sc.fosarea
            ELSE NULL
          END as foreman,
          sc.zone
      FROM public.loc_breeding_sites b
      LEFT JOIN public.gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
      LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
        AND e.emp_type = 'FieldSuper' 
        AND e.active = true
      ORDER BY b.sitecode, b.facility
  )
  -- Return individual treatment records, not aggregated
  SELECT DISTINCT
      t.sitecode,
      t.facility,
      t.recorded_acres as acres,
      t.inspdate,
      t.year,
      t.matcode,
      l.zone,
      l.foreman,
      l.prehatch,
      -- Add a unique treatment ID for identification
      ROW_NUMBER() OVER (ORDER BY t.sitecode, t.inspdate) as treatment_id
  FROM treatment_data t
  LEFT JOIN location_data l ON t.sitecode = l.sitecode AND t.facility = l.facility
  ORDER BY t.sitecode, t.inspdate DESC
  ", start_year, end_year, start_year, end_year)
  
  result <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  if (nrow(result) == 0) {
    return(data.frame())
  }
  
  # Apply filters
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    # Include NULL zones if both zones are selected (which is the default)
    if (length(zone_filter) >= 2) {
      # If both zones selected, don't filter by zone (show all including NULL)
    } else {
      result <- result %>% filter(zone %in% zone_filter)
    }
  }
  
  if (!is.null(facility_filter) && length(facility_filter) > 0 && 
      !("all" %in% facility_filter)) {
    result <- result %>% filter(facility %in% facility_filter)
  }
  
  if (!is.null(foreman_filter) && length(foreman_filter) > 0 && 
      !("all" %in% foreman_filter)) {
    # Convert shortnames to employee numbers for filtering
    foremen_lookup <- get_foremen_lookup()
    selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% foreman_filter]
    
    result <- result %>% filter(foreman %in% selected_emp_nums)
  }
  
  if (!is.null(prehatch_only) && prehatch_only) {
    result <- result %>% filter(prehatch == 'PREHATCH')
  }
  
  return(result)
}

#' Get site statistics data aggregated from treatment records
#' @param sitecode_data_raw Raw sitecode data from get_sitecode_data
#' @param zone_filter Vector of selected zones
#' @param group_by Grouping column name
#' @return Aggregated site statistics data frame
get_site_stats_data <- function(sitecode_data_raw, zone_filter, group_by) {
  if (nrow(sitecode_data_raw) == 0) {
    return(data.frame())
  }
  
  # Apply zone separation if needed
  show_zones_separately <- length(zone_filter) > 1
  
  if (show_zones_separately) {
    sitecode_data_raw <- create_zone_groups(sitecode_data_raw, group_by)
    group_col <- "combined_group"
  } else {
    group_col <- group_by
  }
  
  # Calculate statistics by group from individual treatment records
  site_stats <- sitecode_data_raw %>%
    group_by(!!sym(group_col)) %>%
    summarize(
      avg_site_acres = mean(acres, na.rm = TRUE),
      min_site_acres = min(acres, na.rm = TRUE),
      max_site_acres = max(acres, na.rm = TRUE),
      n_treatments = n(),
      .groups = "drop"
    )
  
  # Add display names based on grouping
  if (group_by == "facility" && !show_zones_separately) {
    facilities <- get_facility_lookup()
    facility_map <- setNames(facilities$full_name, facilities$short_name)
    site_stats <- site_stats %>%
      mutate(display_name = ifelse(
        facility %in% names(facility_map),
        facility_map[facility],
        facility
      ))
  } else if (group_by == "foreman" && !show_zones_separately) {
    foremen_lookup <- get_foremen_lookup()
    foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
    site_stats <- site_stats %>%
      mutate(display_name = ifelse(
        foreman %in% names(foreman_map),
        foreman_map[as.character(foreman)],
        paste("FOS", foreman)
      ))
  } else if (group_by == "facility" && show_zones_separately) {
    # Extract facility and zone for proper display
    site_stats <- site_stats %>%
      mutate(display_name = combined_group)
  } else if (group_by == "foreman" && show_zones_separately) {
    # Extract foreman and zone for proper display
    foremen_lookup <- get_foremen_lookup()
    foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
    
    site_stats <- site_stats %>%
      mutate(
        foreman_part = gsub(" \\(P[12]\\)", "", combined_group),
        display_name = combined_group
      ) %>%
      select(-foreman_part)
  } else {
    site_stats <- site_stats %>%
      mutate(display_name = !!sym(group_col))
  }
  
  return(site_stats)
}
