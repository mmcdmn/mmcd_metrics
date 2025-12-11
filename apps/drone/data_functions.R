# Data functions for drone app - handles all database queries and data processing

#' Load raw data from database
#' @param drone_types Vector of drone type selections
#' @param analysis_date Date to use as "current date" for analysis (defaults to today)
#' @param include_archive Boolean to include archive treatments (for historical analysis)
#' @param start_year Optional start year filter for treatments (for historical analysis)
#' @param end_year Optional end year filter for treatments (for historical analysis)
#' @param include_geometry Boolean to include spatial geometry data for mapping
#' @return List with drone_sites and drone_treatments data frames
load_raw_data <- function(drone_types = c("Y", "M", "C"), analysis_date = Sys.Date(), 
                         include_archive = FALSE, start_year = NULL, end_year = NULL,
                         include_geometry = FALSE) {
  con <- get_db_connection()
  if (is.null(con)) return(list(drone_sites = data.frame(), drone_treatments = data.frame()))
  
  # Build the drone designation filter based on user selection
  drone_types_str <- paste0("'", paste(drone_types, collapse = "','"), "'")
  
  # Query to get drone sites from loc_breeding_sites with facility/zone from gis_sectcode
  # Optionally include geometry for mapping
  geom_select <- if (include_geometry) {
    ", ST_X(ST_Transform(ST_Centroid(b.geom), 4326)) as lng, ST_Y(ST_Transform(ST_Centroid(b.geom), 4326)) as lat"
  } else {
    ""
  }
  
  geom_where <- if (include_geometry) {
    "AND b.geom IS NOT NULL"
  } else {
    ""
  }
  
  drone_sites_query <- sprintf("
  SELECT b.sitecode, g.facility, b.acres, b.prehatch, b.drone, 
         CASE 
           WHEN e.emp_num IS NOT NULL AND e.active = true THEN g.fosarea
           ELSE NULL
         END as foreman, 
         g.zone,
         left(b.sitecode,7) as sectcode%s
  FROM public.loc_breeding_sites b
  LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode,7)
  LEFT JOIN public.employee_list e ON g.fosarea = e.emp_num 
    AND e.emp_type = 'FieldSuper' 
    AND e.active = true
  WHERE (b.drone IN (%s) OR b.air_gnd = 'D')
  AND b.enddate IS NULL %s
  ", geom_select, drone_types_str, geom_where)
  
  drone_sites <- dbGetQuery(con, drone_sites_query)
  
  # Query to get treatment information with facility and zone from gis_sectcode
  treatments_query = "
  SELECT t.sitecode, t.inspdate, t.matcode, t.acres as treated_acres, t.foreman, m.effect_days,
         g.facility, g.zone, g.fosarea
  FROM public.dblarv_insptrt_current t
  LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
  LEFT JOIN public.gis_sectcode g ON g.sectcode = left(t.sitecode,7)
  WHERE (t.airgrnd_plan = 'D' OR t.action = 'D')
  "
  
  # Add year filtering for historical analysis
  if (!is.null(start_year) && !is.null(end_year)) {
    treatments_query <- paste0(treatments_query, sprintf(
      " AND EXTRACT(YEAR FROM t.inspdate) BETWEEN %d AND %d", start_year, end_year))
  }
  
  treatments <- dbGetQuery(con, treatments_query)
  
  # Get archive treatments if requested (for historical analysis)
  if (include_archive) {
    archive_query <- "
    SELECT t.sitecode, g.facility, t.inspdate, t.matcode, t.acres as treated_acres, 
           NULL::character as foreman, NULL::integer as effect_days,
           g.zone, g.fosarea
    FROM public.dblarv_insptrt_archive t
    LEFT JOIN public.gis_sectcode g ON g.sectcode = left(t.sitecode,7)
    WHERE t.action = 'D'
    "
    
    # Add same year filtering for archive
    if (!is.null(start_year) && !is.null(end_year)) {
      archive_query <- paste0(archive_query, sprintf(
        " AND EXTRACT(YEAR FROM t.inspdate) BETWEEN %d AND %d", start_year, end_year))
    }
    
    archive_treatments <- dbGetQuery(con, archive_query)
    
    # Combine current and archive treatments
    treatments <- bind_rows(treatments, archive_treatments)
  }
  
  dbDisconnect(con)
  
  # Process the data
  # Identify drone sites with treatments  
  drone_treatments <- treatments %>%
    inner_join(drone_sites, by = c("sitecode", "facility"), suffix = c("_trt", "_site"), relationship = "many-to-many") %>%
    mutate(
      # Use recorded treated acres from treatment records
      acres = treated_acres,
      # Use site foreman (jurisdictional assignment). Ignore treatment foreman  
      foreman = foreman_site,
      prehatch = prehatch,
      drone = drone,
      # Use zone from treatment record (should match site zone)
      zone = zone_trt,
      sectcode = sectcode
    )
  
  # Calculate treatment status (active)
  current_date <- as.Date(analysis_date)
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
#' @param analysis_date Date to use as cutoff - no data after this date (defaults to today)
#' @return Data frame with individual treatment records
get_sitecode_data <- function(start_year, end_year, zone_filter, facility_filter, foreman_filter, prehatch_only, analysis_date = Sys.Date()) {
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
          EXTRACT(YEAR FROM t.inspdate) as year
      FROM public.dblarv_insptrt_current t
      WHERE (t.airgrnd_plan = 'D' OR t.action = 'D')
          AND EXTRACT(YEAR FROM t.inspdate) BETWEEN %d AND %d
          AND t.inspdate <= '%s'
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
          EXTRACT(YEAR FROM t.inspdate) as year
      FROM public.dblarv_insptrt_archive t
      WHERE t.action = 'D'
          AND EXTRACT(YEAR FROM t.inspdate) BETWEEN %d AND %d
          AND t.inspdate <= '%s'
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
  ", start_year, end_year, as.character(analysis_date), start_year, end_year, as.character(analysis_date))
  
  result <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  if (nrow(result) == 0) {
    return(data.frame())
  }
  
  # Apply filters
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    # Always filter by the specified zones
    result <- result %>% filter(zone %in% zone_filter)
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
#' @param combine_zones Boolean whether to combine P1+P2
#' @param group_by Grouping column name
#' @return Aggregated site statistics data frame
get_site_stats_data <- function(sitecode_data_raw, zone_filter, combine_zones = FALSE, group_by) {
  if (nrow(sitecode_data_raw) == 0) {
    return(data.frame())
  }
  
  # Apply zone separation if needed
  show_zones_separately <- !combine_zones && length(zone_filter) > 1
  
  # Handle MMCD grouping and zone separation
  if (group_by == "mmcd_all") {
    if (show_zones_separately) {
      # Create zone-separated MMCD groups like "All MMCD (P1)", "All MMCD (P2)"
      sitecode_data_raw$mmcd_all <- "All MMCD"
      sitecode_data_raw <- create_zone_groups(sitecode_data_raw, "mmcd_all")
      group_col <- "combined_group"
    } else {
      # Single MMCD group
      sitecode_data_raw$mmcd_all <- "All MMCD"
      group_col <- "mmcd_all"
    }
  } else if (show_zones_separately) {
    sitecode_data_raw <- create_zone_groups(sitecode_data_raw, group_by)
    group_col <- "combined_group"
  } else {
    group_col <- group_by
  }
  
  # Calculate statistics by group from individual treatment records
  if (show_zones_separately) {
    # When separating zones, group by both combined_group AND zone
    site_stats <- sitecode_data_raw %>%
      group_by(!!sym(group_col), zone) %>%
      summarize(
        avg_site_acres = mean(acres, na.rm = TRUE),
        min_site_acres = min(acres, na.rm = TRUE),
        max_site_acres = max(acres, na.rm = TRUE),
        n = n_distinct(sitecode),  # Count unique sites, not treatments
        n_treatments = n(),
        .groups = "drop"
      )
  } else {
    # Standard grouping without zone separation
    site_stats <- sitecode_data_raw %>%
      group_by(!!sym(group_col)) %>%
      summarize(
        avg_site_acres = mean(acres, na.rm = TRUE),
        min_site_acres = min(acres, na.rm = TRUE),
        max_site_acres = max(acres, na.rm = TRUE),
        n = n_distinct(sitecode),  # Count unique sites, not treatments
        n_treatments = n(),
        .groups = "drop"
      )
  }
  
  # Add display names based on grouping
  if (group_by == "mmcd_all") {
    # For MMCD grouping, just use "All MMCD"
    site_stats <- site_stats %>%
      mutate(display_name = "All MMCD")
  } else if (group_by == "facility" && !show_zones_separately) {
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

#' Load spatial data for map visualization
#' @param analysis_date Date to use as "current date" for analysis
#' @param zone_filter Zone filter (1, 2, or c("1", "2"))
#' @param facility_filter Facility filter (can be "all" or specific facilities)
#' @param foreman_filter Foreman filter (can be "all" or specific foreman IDs)
#' @param prehatch_only Logical, whether to filter to prehatch sites only
#' @param expiring_days Number of days for expiring treatments
#' @return sf data frame with geometry and treatment status
load_spatial_data <- function(analysis_date = Sys.Date(), zone_filter = c("1", "2"), 
                             facility_filter = "all", foreman_filter = "all", 
                             prehatch_only = FALSE, expiring_days = 14) {
  
  # Load raw data with geometry
  raw_data <- load_raw_data(
    drone_types = c("Y", "M", "C"),
    analysis_date = analysis_date,
    include_geometry = TRUE
  )
  
  if (nrow(raw_data$drone_sites) == 0) {
    return(NULL)
  }
  
  # Apply filters
  filtered_data <- apply_data_filters(
    data = raw_data,
    facility_filter = facility_filter,
    foreman_filter = foreman_filter,
    prehatch_only = prehatch_only
  )
  
  # Filter by zones
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    filtered_data$drone_sites <- filtered_data$drone_sites %>% 
      filter(zone %in% zone_filter)
    filtered_data$drone_treatments <- filtered_data$drone_treatments %>% 
      filter(zone %in% zone_filter)
  }
  
  # Get current date for status calculations
  current_date <- as.Date(analysis_date)
  expiring_start_date <- current_date
  expiring_end_date <- current_date + expiring_days
  
  # Get latest treatment for each site
  latest_treatments <- filtered_data$drone_treatments %>%
    group_by(sitecode) %>%
    arrange(desc(inspdate)) %>%
    slice(1) %>%
    ungroup() %>%
    select(sitecode, inspdate, matcode, treated_acres, treatment_end_date) %>%
    mutate(
      treatment_status = case_when(
        treatment_end_date < current_date ~ "Expired",
        treatment_end_date >= expiring_start_date & treatment_end_date <= expiring_end_date ~ "Expiring",
        treatment_end_date > expiring_end_date ~ "Active",
        TRUE ~ "No Treatment"
      ),
      days_to_expiry = case_when(
        is.na(treatment_end_date) ~ 999,
        treatment_end_date < current_date ~ as.numeric(current_date - treatment_end_date),
        TRUE ~ as.numeric(treatment_end_date - current_date)
      )
    )
  
  # Join sites with treatment status
  spatial_data <- filtered_data$drone_sites %>%
    left_join(latest_treatments, by = "sitecode") %>%
    mutate(
      treatment_status = if_else(is.na(treatment_status), "No Treatment", treatment_status),
      last_treatment_date = if_else(is.na(inspdate), as.Date(NA), as.Date(inspdate)),
      last_material = if_else(is.na(matcode), as.character(NA), matcode),
      treated_acres = if_else(is.na(treated_acres), 0, treated_acres)
    ) %>%
    filter(!is.na(lng), !is.na(lat))  # Only keep sites with coordinates
  
  if (nrow(spatial_data) == 0) {
    return(NULL)
  }
  
  # Convert to sf object
  spatial_sf <- st_as_sf(spatial_data, 
                        coords = c("lng", "lat"), 
                        crs = 4326)
  
  return(spatial_sf)
}
