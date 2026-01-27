# Ground Prehatch Progress - Data Functions
# Functions for fetching and processing ground prehatch data

# Source shared helpers (only if not already loaded - allows use from district_overview)
if (!exists("get_db_connection", mode = "function")) {
  source("../../shared/db_helpers.R")
}

# Unified function to load raw ground prehatch data 
load_raw_data <- function(analysis_date = Sys.Date(), include_archive = FALSE, 
                         start_year = NULL, end_year = NULL, include_geometry = FALSE) {
  con <- get_db_connection()
  if (is.null(con)) return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
  
  tryCatch({
    analysis_year <- format(analysis_date, "%Y")
    
    # Load ground sites using the exact pattern from working app
    geom_select <- if (include_geometry) {
      ", ST_AsText(ST_Transform(b.geom, 4326)) as geometry"
    } else {
      ""
    }
    
    sites_query <- sprintf("
    SELECT sc.facility, sc.zone, sc.fosarea, left(b.sitecode,7) AS sectcode,
           b.sitecode, acres, air_gnd, priority, prehatch, drone, remarks,
           sc.fosarea as foreman%s
    FROM loc_breeding_sites b
    LEFT JOIN gis_sectcode sc ON left(b.sitecode,7)=sc.sectcode
    WHERE (b.enddate IS NULL OR b.enddate>'%s-05-01')
      AND b.air_gnd='G'
      AND b.prehatch IN ('PREHATCH','BRIQUET')
    ORDER BY sc.facility, sc.sectcode, b.sitecode, b.prehatch
    ", geom_select, analysis_year)
    
    ground_sites <- dbGetQuery(con, sites_query)
    
    # Load treatments based on include_archive flag
    if (include_archive && !is.null(start_year) && !is.null(end_year)) {
      # Historical mode: ALWAYS get current year data from current table, archive from archive table
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      
      # Build query parts
      query_parts <- c()
      
      # ALWAYS include current year data from dblarv_insptrt_current
      if (end_year >= current_year) {
        current_query <- sprintf("
        SELECT c.sitecode, c.inspdate, c.matcode, c.insptime,
               c.acres as treated_acres, sc.facility, sc.zone, sc.fosarea,
               p.effect_days, 'current' as data_source
        FROM dblarv_insptrt_current c
        JOIN gis_sectcode sc ON left(c.sitecode, 7) = sc.sectcode
        JOIN mattype_list_targetdose p ON c.matcode = p.matcode
        WHERE c.inspdate >= '%d-01-01' AND c.inspdate <= '%d-12-31'
          AND p.prehatch IS TRUE
        ", current_year, current_year)
        query_parts <- c(query_parts, current_query)
      }
      
      # Include archive data for any years before current year
      if (start_year < current_year) {
        archive_end_year <- min(end_year, current_year - 1)
        archive_query <- sprintf("
        SELECT c.sitecode, c.inspdate, c.matcode, c.insptime,
               c.acres as treated_acres, sc.facility, sc.zone, sc.fosarea,
               p.effect_days, 'archive' as data_source
        FROM dblarv_insptrt_archive c
        JOIN gis_sectcode sc ON left(c.sitecode, 7) = sc.sectcode
        JOIN mattype_list_targetdose p ON c.matcode = p.matcode
        WHERE c.inspdate >= '%d-01-01' AND c.inspdate <= '%d-12-31'
          AND p.prehatch IS TRUE
        ", start_year, archive_end_year)
        query_parts <- c(query_parts, archive_query)
      }
      
      # Combine query parts
      if (length(query_parts) > 0) {
        treatments_query <- paste(query_parts, collapse = " UNION ALL ")
        treatments_query <- paste(treatments_query, "ORDER BY inspdate DESC, sitecode")
      } else {
        # Fallback empty query
        treatments_query <- "SELECT NULL::text as sitecode, NULL::date as inspdate, NULL::text as matcode, NULL::time as insptime, NULL::numeric as treated_acres, NULL::text as facility, NULL::text as zone, NULL::text as fosarea, NULL::integer as effect_days, NULL::text as data_source WHERE FALSE"
      }
    } else {
      # Current mode: use exact working pattern with current year from analysis_date
      analysis_year <- format(analysis_date, "%Y")
      treatments_query <- sprintf("
      SELECT c.sitecode, c.inspdate, c.matcode, c.insptime,
             c.acres as treated_acres, p.effect_days, 'current' as data_source,
             -- Add inspection data for skipped status
             i.inspdate as last_inspection_date, i.action as inspection_action, i.wet as inspection_wet
      FROM (SELECT * FROM dblarv_insptrt_current WHERE inspdate>'%s-01-01' AND inspdate <= '%s') c
      JOIN (
        SELECT sc.facility, sc.zone, sc.fosarea, left(b.sitecode,7) AS sectcode,
               b.sitecode, acres, air_gnd, priority, prehatch, drone, remarks,
               sc.fosarea as foreman
        FROM loc_breeding_sites b
        LEFT JOIN gis_sectcode sc ON left(b.sitecode,7)=sc.sectcode
        WHERE (b.enddate IS NULL OR b.enddate>'%s-05-01')
          AND b.air_gnd='G'
          AND b.prehatch IN ('PREHATCH','BRIQUET')
      ) a ON c.sitecode = a.sitecode
      JOIN (SELECT * FROM mattype_list_targetdose WHERE prehatch IS TRUE) p USING (matcode)
      -- Left join to get the most recent ground inspection for skipped status
      LEFT JOIN LATERAL (
        SELECT inspdate, action, wet
        FROM dblarv_insptrt_current 
        WHERE sitecode = c.sitecode 
          AND action = '2'
          AND wet = '0'
          AND inspdate > c.inspdate
        ORDER BY inspdate DESC, insptime DESC
        LIMIT 1
      ) i ON true
      ORDER BY c.inspdate DESC, c.sitecode
      ", analysis_year, format(analysis_date, "%Y-%m-%d"), analysis_year)
    }
    
    ground_treatments <- dbGetQuery(con, treatments_query)
    
    safe_disconnect(con)
    
    # Convert data types
    if (nrow(ground_sites) > 0) {
      ground_sites <- map_facility_names(ground_sites)
    }
    
    # Calculate site-level status from latest treatment per site
    # STANDARDIZED: sites MUST have is_active and is_expiring columns
    if (nrow(ground_treatments) > 0) {
      current_date <- as.Date(analysis_date)
      expiring_days <- 7  # Default expiring window
      
      site_status <- ground_treatments %>%
        mutate(
          treatment_end = as.Date(inspdate) + ifelse(is.na(effect_days), 0, effect_days),
          is_active = treatment_end >= current_date,
          is_expiring = is_active & treatment_end <= (current_date + expiring_days)
        ) %>%
        group_by(sitecode) %>%
        arrange(desc(inspdate)) %>%
        slice(1) %>%
        ungroup() %>%
        select(sitecode, is_active, is_expiring)
      
      ground_sites <- ground_sites %>%
        left_join(site_status, by = "sitecode") %>%
        mutate(
          is_active = ifelse(is.na(is_active), FALSE, is_active),
          is_expiring = ifelse(is.na(is_expiring), FALSE, is_expiring)
        )
    } else {
      ground_sites$is_active <- FALSE
      ground_sites$is_expiring <- FALSE
    }
    
    # Return STANDARDIZED format - sites ALWAYS has is_active, is_expiring
    return(list(
      sites = ground_sites,
      treatments = ground_treatments,
      total_count = nrow(ground_sites)
    ))
    
  }, error = function(e) {
    warning(paste("Error loading raw data:", e$message))
    if (!is.null(con)) safe_disconnect(con)
    return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
  })
}

#' Apply filters to ground prehatch data - STANDARDIZED FORMAT
#' @param data List containing sites and treatments (standardized keys)
#' @param facility_filter Vector of selected facilities  
#' @param foreman_filter Vector of selected foremen (emp_num values)
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
  
  # Apply facility filter using shared helper
  if (is_valid_filter(facility_filter)) {
    sites <- sites %>% filter(facility %in% facility_filter)
  }
  
  # Apply zone filter (zones don't use "all" check)
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    sites <- sites %>% filter(zone %in% zone_filter)
  }
  
  # Apply foreman/FOS filter using shared helper
  # Note: foreman_filter is already emp_nums in this app, no conversion needed
  if (is_valid_filter(foreman_filter)) {
    sites <- sites %>% filter(fosarea %in% foreman_filter)
  }
  
  # Filter treatments to only include those for filtered sites
  if (!is.null(treatments) && nrow(treatments) > 0 && nrow(sites) > 0) {
    treatments <- treatments %>% filter(sitecode %in% sites$sitecode)
  }
  
  # Return STANDARDIZED format
  return(list(
    sites = sites,
    treatments = if(is.null(treatments)) data.frame() else treatments,
    total_count = nrow(sites)
  ))
}

# Function to get ground prehatch data from database
# Uses load_raw_data as single source of truth and processes status like get_site_details_data
# Standard column names added: total_count, active_count, expiring_count, expired_count
get_ground_prehatch_data <- function(zone_filter = c("1", "2"), analysis_date = Sys.Date(), 
                                      expiring_days = 14) {
  # Load raw data using the unified function
  raw_data <- load_raw_data(analysis_date = analysis_date, include_archive = FALSE)
  
  if (is.null(raw_data$sites) || nrow(raw_data$sites) == 0) {
    return(data.frame())
  }
  
  # Calculate treatment status for each site
  site_details <- raw_data$sites
  
  # Apply zone filter
  site_details <- site_details %>%
    filter(zone %in% zone_filter)
  
  if (nrow(site_details) == 0) {
    return(data.frame())
  }
  
  if (!is.null(raw_data$treatments) && nrow(raw_data$treatments) > 0) {
    # Get latest treatment for each site
    latest_treatments <- raw_data$treatments %>%
      group_by(sitecode) %>%
      arrange(desc(inspdate), desc(insptime)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        age = as.numeric(analysis_date - inspdate),
        # Calculate end of year date for skipped sites
        end_of_year = as.Date(paste0(format(analysis_date, "%Y"), "-12-30")),
        days_until_eoy = as.numeric(end_of_year - analysis_date),
        prehatch_status = case_when(
          # If there's a ground inspection with action=2 and wet=0 after treatment, it's skipped
          !is.na(last_inspection_date) & !is.na(inspection_action) & !is.na(inspection_wet) &
            inspection_action == '2' & inspection_wet == '0' & 
            last_inspection_date > inspdate ~ ifelse(days_until_eoy > 0, "skipped", "expired"),
          # Regular status calculation
          age > effect_days ~ "expired",
          age > (effect_days - expiring_days) ~ "expiring", 
          age <= effect_days ~ "treated",
          TRUE ~ "unknown"
        )
      ) %>%
      select(sitecode, prehatch_status, inspdate, matcode, age, effect_days)
    
    # Join sites with treatment status
    site_details <- site_details %>%
      left_join(latest_treatments, by = "sitecode")
    
    # Sites without treatments in the join are expired
    site_details <- site_details %>%
      mutate(prehatch_status = ifelse(is.na(prehatch_status), "expired", prehatch_status))
  } else {
    # No treatments at all (e.g., January before prehatch season)
    # All sites are counted as "expired" (untreated)
    site_details$prehatch_status <- "expired"
  }
  
  # Aggregate by sectcode and foreman to match expected structure
  result <- site_details %>%
    group_by(facility, zone, fosarea, sectcode) %>%
    summarise(
      foreman = first(fosarea),
      tot_ground = n(),  
      not_prehatch_sites = 0, 
      prehatch_sites_cnt = n(),
      prehatch_acres = sum(acres, na.rm = TRUE),
      drone_sites_cnt = 0, 
      ph_treated_cnt = sum(prehatch_status == "treated", na.rm = TRUE),
      ph_treated_acres = sum(ifelse(prehatch_status == "treated", acres, 0), na.rm = TRUE),
      ph_expiring_cnt = sum(prehatch_status == "expiring", na.rm = TRUE),
      ph_expiring_acres = sum(ifelse(prehatch_status == "expiring", acres, 0), na.rm = TRUE),
      ph_expired_cnt = sum(prehatch_status %in% c("expired", NA), na.rm = TRUE),
      ph_expired_acres = sum(ifelse(prehatch_status %in% c("expired", NA), acres, 0), na.rm = TRUE),
      ph_skipped_cnt = sum(prehatch_status == "skipped", na.rm = TRUE),
      ph_skipped_acres = sum(ifelse(prehatch_status == "skipped", acres, 0), na.rm = TRUE),
      ph_untreated_cnt = sum(is.na(prehatch_status), na.rm = TRUE),
      # Standard column names
      total_count = n(),
      active_count = sum(prehatch_status %in% c("treated", "expiring"), na.rm = TRUE),
      expiring_count = sum(prehatch_status == "expiring", na.rm = TRUE),
      expired_count = sum(prehatch_status %in% c("expired", NA), na.rm = TRUE),
      total_acres = sum(acres, na.rm = TRUE),
      active_acres = sum(ifelse(prehatch_status %in% c("treated", "expiring"), acres, 0), na.rm = TRUE),
      expiring_acres = sum(ifelse(prehatch_status == "expiring", acres, 0), na.rm = TRUE),
      .groups = "drop"
    )
  
  return(result)
}

# Function to get site details data - uses same source as charts
get_site_details_data <- function(expiring_days = 14, analysis_date = Sys.Date()) {
  # Load raw data using the unified function - same as charts use
  raw_data <- load_raw_data(analysis_date = analysis_date, include_archive = FALSE)
  
  if (is.null(raw_data$sites) || nrow(raw_data$sites) == 0) {
    return(data.frame())
  }
  
  # Start with all sites (gis_sectcode is source of truth)
  result <- raw_data$sites
  
  if (!is.null(raw_data$treatments) && nrow(raw_data$treatments) > 0) {
    # Get latest treatment for each site
    latest_treatments <- raw_data$treatments %>%
      group_by(sitecode) %>%
      arrange(desc(inspdate), desc(insptime)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        age = as.numeric(analysis_date - inspdate),
        # Calculate end of year date for skipped sites
        end_of_year = as.Date(paste0(format(analysis_date, "%Y"), "-12-30")),
        days_until_eoy = as.numeric(end_of_year - analysis_date),
        prehatch_status = case_when(
          # If there's a ground inspection with action=2 and wet=0 after treatment, it's skipped
          !is.na(last_inspection_date) & !is.na(inspection_action) & !is.na(inspection_wet) &
            inspection_action == '2' & inspection_wet == '0' & 
            last_inspection_date > inspdate ~ ifelse(days_until_eoy > 0, "skipped", "expired"),
          # Regular status calculation
          age > effect_days ~ "expired",
          age > (effect_days - expiring_days) ~ "expiring",
          age <= effect_days ~ "treated", 
          TRUE ~ "unknown"
        )
      ) %>%
      select(sitecode, prehatch_status, inspdate, matcode, age, effect_days)
    
    # Join sites with treatment status
    result <- result %>%
      left_join(latest_treatments, by = "sitecode")
  } else {
    # No treatments data - add empty columns
    result <- result %>%
      mutate(
        prehatch_status = NA_character_,
        inspdate = as.Date(NA),
        matcode = NA_character_,
        age = NA_real_,
        effect_days = NA_real_
      )
  }
  
  # Sites without treatments are expired/unknown (count as expired)
  result <- result %>%
    mutate(
      prehatch_status = ifelse(is.na(prehatch_status), "expired", prehatch_status)
    ) %>%
    # Filter to only prehatch sites
    filter(prehatch %in% c("PREHATCH", "BRIQUET")) %>%
    arrange(facility, sectcode, sitecode)
  
  return(result)
}

# Function to filter data based on user selections
filter_ground_data <- function(data, zone_filter = NULL, facility_filter = NULL, foreman_filter = NULL) {
  if (nrow(data) == 0) return(data)
  
  # Return empty data if "none" is selected for facility or foreman
  if (!is.null(facility_filter) && ("none" %in% facility_filter) && length(facility_filter) == 1) {
    return(data.frame())
  }
  if (!is.null(foreman_filter) && ("none" %in% foreman_filter) && length(foreman_filter) == 1) {
    return(data.frame())
  }
  
  # Filter by zone if selected
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    data <- data %>% filter(zone %in% zone_filter)
  }
  
  # Filter by facility using shared helper
  if (is_valid_filter(facility_filter)) {
    data <- data %>% filter(facility %in% facility_filter)
  }
  
  # Filter by foreman using shared helper
  if (is_valid_filter(foreman_filter)) {
    data <- data %>% filter(foreman %in% foreman_filter)
  }
  
  return(data)
}

# Function to aggregate data based on grouping level
aggregate_data_by_group <- function(data, group_by, zone_filter = NULL, expiring_filter = "all", site_details = NULL, combine_zones = FALSE) {
  if (nrow(data) == 0) return(data)
  
  # Map facility names for display
  data <- map_facility_names(data)
  
  # Apply expiring filter based on selection
  if (expiring_filter != "all" && !is.null(site_details)) {
    if (expiring_filter == "expiring") {
      # Show only expiring sites
      expiring_count_data <- site_details %>% filter(prehatch_status == "expiring")
      
      if (nrow(expiring_count_data) > 0) {
        # Filter data to only sections that have expiring sites
        expiring_sections <- unique(expiring_count_data$sectcode)
        data <- data %>% filter(sectcode %in% expiring_sections)
        
        # Zero out all counts except expiring
        data <- data %>%
          mutate(
            ph_treated_cnt = 0,
            ph_expired_cnt = 0
          )
      } else {
        # No expiring sites, return empty data
        return(data.frame())
      }
    } else if (expiring_filter == "expiring_expired") {
      # Show expiring AND expired sites
      expiring_expired_sites <- site_details %>% filter(prehatch_status %in% c("expiring", "expired"))
      
      if (nrow(expiring_expired_sites) > 0) {
        # Filter data to only sections that have expiring or expired sites
        expiring_expired_sections <- unique(expiring_expired_sites$sectcode)
        data <- data %>% filter(sectcode %in% expiring_expired_sections)
        
        # Zero out all counts except expiring and expired
        data <- data %>%
          mutate(
            ph_treated_cnt = 0
          )
      } else {
        # No expiring or expired sites, return empty data
        return(data.frame())
      }
    }
  }
  
  # Determine if zones should be shown separately
  # For single zone, never show separately (just show the zone)
  # For multiple zones, show separately only if combine_zones is FALSE
  show_zones_separately <- !is.null(zone_filter) && length(zone_filter) > 1 && !combine_zones
  
  if (group_by == "mmcd_all") {
    if (show_zones_separately) {
      # Group by zone only
      result <- data %>%
        group_by(zone) %>%
        summarise(
          tot_ground = sum(tot_ground, na.rm = TRUE),
          not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
          prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
          prehatch_acres = sum(prehatch_acres, na.rm = TRUE),
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_treated_acres = sum(ph_treated_acres, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expiring_acres = sum(ph_expiring_acres, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
          ph_expired_acres = sum(ph_expired_acres, na.rm = TRUE),
          ph_skipped_cnt = sum(ph_skipped_cnt, na.rm = TRUE),
          ph_skipped_acres = sum(ph_skipped_acres, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          display_name = paste0("MMCD P", zone),
          group_name = "MMCD"
        )
    } else {
      # Group all together
      result <- data %>%
        summarise(
          tot_ground = sum(tot_ground, na.rm = TRUE),
          not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
          prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
          prehatch_acres = sum(prehatch_acres, na.rm = TRUE),
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_treated_acres = sum(ph_treated_acres, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expiring_acres = sum(ph_expiring_acres, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
          ph_expired_acres = sum(ph_expired_acres, na.rm = TRUE),
          ph_skipped_cnt = sum(ph_skipped_cnt, na.rm = TRUE),
          ph_skipped_acres = sum(ph_skipped_acres, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          display_name = "MMCD",
          group_name = "MMCD"
        )
    }
  } else if (group_by == "sectcode") {
    # Group by section only - sections cannot be shown separately by zone
    # since each section inherently belongs to exactly one zone
    result <- data %>%
      group_by(sectcode) %>%
      summarise(
        tot_ground = sum(tot_ground, na.rm = TRUE),
        not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
        prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
        prehatch_acres = sum(prehatch_acres, na.rm = TRUE),
        drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
        ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
        ph_treated_acres = sum(ph_treated_acres, na.rm = TRUE),
        ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
        ph_expiring_acres = sum(ph_expiring_acres, na.rm = TRUE),
        ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
        ph_expired_acres = sum(ph_expired_acres, na.rm = TRUE),
        ph_skipped_cnt = sum(ph_skipped_cnt, na.rm = TRUE),
        ph_skipped_acres = sum(ph_skipped_acres, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        display_name = sectcode,
        group_name = sectcode
      )
  } else if (group_by == "facility") {
    if (show_zones_separately) {
      # Group by facility and zone
      result <- data %>%
        group_by(facility, zone) %>%
        summarise(
          tot_ground = sum(tot_ground, na.rm = TRUE),
          not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
          prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
          prehatch_acres = sum(prehatch_acres, na.rm = TRUE),
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_treated_acres = sum(ph_treated_acres, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expiring_acres = sum(ph_expiring_acres, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
          ph_expired_acres = sum(ph_expired_acres, na.rm = TRUE),
          ph_skipped_cnt = sum(ph_skipped_cnt, na.rm = TRUE),
          ph_skipped_acres = sum(ph_skipped_acres, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          facility_display = map_facility_names(data.frame(facility = facility))$facility_display,
          display_name = paste0(facility_display, " P", zone),
          group_name = facility
        )
    } else {
      # Group by facility only  
      result <- data %>%
        group_by(facility) %>%
        summarise(
          tot_ground = sum(tot_ground, na.rm = TRUE),
          not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
          prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
          prehatch_acres = sum(prehatch_acres, na.rm = TRUE),
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_treated_acres = sum(ph_treated_acres, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expiring_acres = sum(ph_expiring_acres, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
          ph_expired_acres = sum(ph_expired_acres, na.rm = TRUE),
          ph_skipped_cnt = sum(ph_skipped_cnt, na.rm = TRUE),
          ph_skipped_acres = sum(ph_skipped_acres, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          facility_display = map_facility_names(data.frame(facility = facility))$facility_display,
          display_name = facility_display,
          group_name = facility
        )
    }
  } else if (group_by == "foreman") {
    # Get foreman lookup for display names
    foremen_lookup <- get_foremen_lookup()
    
    if (show_zones_separately) {
      # Group by foreman and zone
      result <- data %>%
        group_by(foreman, zone) %>%
        summarise(
          tot_ground = sum(tot_ground, na.rm = TRUE),
          not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
          prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
          prehatch_acres = sum(prehatch_acres, na.rm = TRUE),
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_treated_acres = sum(ph_treated_acres, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expiring_acres = sum(ph_expiring_acres, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
          ph_expired_acres = sum(ph_expired_acres, na.rm = TRUE),
          ph_skipped_cnt = sum(ph_skipped_cnt, na.rm = TRUE),
          ph_skipped_acres = sum(ph_skipped_acres, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          foreman_name = sapply(foreman, function(f) {
            matches <- which(trimws(as.character(foremen_lookup$emp_num)) == trimws(as.character(f)))
            if(length(matches) > 0) foremen_lookup$shortname[matches[1]] else paste0("FOS #", f)
          }),
          display_name = paste0(foreman_name, " P", zone),
          group_name = foreman,
          combined_group = paste0(foreman, " P", zone)
        )
    } else {
      # Group by foreman only
      result <- data %>%
        group_by(foreman) %>%
        summarise(
          tot_ground = sum(tot_ground, na.rm = TRUE),
          not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
          prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
          prehatch_acres = sum(prehatch_acres, na.rm = TRUE),
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_treated_acres = sum(ph_treated_acres, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expiring_acres = sum(ph_expiring_acres, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
          ph_expired_acres = sum(ph_expired_acres, na.rm = TRUE),
          ph_skipped_cnt = sum(ph_skipped_cnt, na.rm = TRUE),
          ph_skipped_acres = sum(ph_skipped_acres, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          foreman_name = sapply(foreman, function(f) {
            matches <- which(trimws(as.character(foremen_lookup$emp_num)) == trimws(as.character(f)))
            if(length(matches) > 0) foremen_lookup$shortname[matches[1]] else paste0("FOS #", f)
          }),
          display_name = foreman_name,
          group_name = foreman
        )
    }
  }
  
  # Calculate percentages
  result <- result %>%
    mutate(
      treated_pct = ifelse(prehatch_sites_cnt > 0, 
                          round(100 * ph_treated_cnt / prehatch_sites_cnt, 1), 0),
      expiring_pct = ifelse(prehatch_sites_cnt > 0, 
                           round(100 * ph_expiring_cnt / prehatch_sites_cnt, 1), 0),
      # Add standardized column names for cross-app consistency
      total_count = prehatch_sites_cnt,
      active_count = ph_treated_cnt + ph_expiring_cnt,
      expiring_count = ph_expiring_cnt,
      expired_count = ph_expired_cnt,
      total_acres = prehatch_acres,
      active_acres = ph_treated_acres + ph_expiring_acres,
      expiring_acres = ph_expiring_acres
    )
  
  return(result)
}

# Function to get unique foreman choices for filter updates
get_foreman_choices <- function(data, foremen_lookup) {
  foreman_choices <- c("None (No Data)" = "none", "All" = "all")
  
  if (nrow(data) > 0 && "fosarea" %in% names(data) && nrow(foremen_lookup) > 0) {
    unique_foremen <- sort(unique(na.omit(data$fosarea)))
    
    # Map empnum to shortname for display
    for (empnum in unique_foremen) {
      foreman_row <- foremen_lookup[foremen_lookup$emp_num == empnum, ]
      if (nrow(foreman_row) > 0) {
        display_name <- foreman_row$shortname[1]
        foreman_choices <- c(foreman_choices, setNames(empnum, display_name))
      } else {
        # Fallback to empnum if not found in lookup
        foreman_choices <- c(foreman_choices, setNames(empnum, empnum))
      }
    }
  }
  
  return(foreman_choices)
}

# Load spatial data for map (similar to drone app)
load_spatial_data <- function(analysis_date = Sys.Date(), zone_filter = c("1", "2"), 
                             facility_filter = "all", foreman_filter = "all") {
  
  # Load raw data with geometry
  raw_data <- load_raw_data(
    analysis_date = analysis_date,
    include_archive = FALSE,
    include_geometry = TRUE
  )
  
  if (is.null(raw_data$sites) || nrow(raw_data$sites) == 0) {
    return(NULL)
  }
  
  # Apply zone filter
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    raw_data$sites <- raw_data$sites %>% 
      filter(zone %in% zone_filter)
  }
  
  # Apply facility filter using shared helper
  if (is_valid_filter(facility_filter)) {
    raw_data$sites <- raw_data$sites %>% 
      filter(facility %in% facility_filter)
  }
  
  # Apply foreman filter (using fosarea) using shared helper
  if (is_valid_filter(foreman_filter)) {
    raw_data$sites <- raw_data$sites %>% 
      filter(fosarea %in% foreman_filter)
  }
  
  # Get current date for status calculations
  current_date <- as.Date(analysis_date)
  
  # Get latest treatment for each site with proper effect_days from database
  if (!is.null(raw_data$treatments) && nrow(raw_data$treatments) > 0) {
    latest_treatments <- raw_data$treatments %>%
      group_by(sitecode) %>%
      arrange(desc(inspdate)) %>%
      slice(1) %>%
      ungroup() %>%
      select(sitecode, inspdate, matcode, effect_days) %>%
      mutate(
        days_since_treatment = as.numeric(current_date - inspdate),
        # Use database effect_days for each treatment, not hardcoded expiring_days
        treatment_status = case_when(
          is.na(inspdate) ~ "No Treatment",
          days_since_treatment <= effect_days ~ "Active",
          days_since_treatment <= effect_days + 14 ~ "Expiring", 
          TRUE ~ "Expired"
        )
      )
  } else {
    # Create empty treatment data
    latest_treatments <- data.frame(
      sitecode = character(0),
      inspdate = as.Date(character(0)),
      matcode = character(0),
      days_since_treatment = numeric(0),
      treatment_status = character(0)
    )
  }
  
  # Convert sites to sf object
  sites_sf <- tryCatch({
    if ("geometry" %in% names(raw_data$sites) && !all(is.na(raw_data$sites$geometry))) {
      # Convert to sf using WKT
      sf_result <- sf::st_as_sf(raw_data$sites, wkt = "geometry", crs = 4326)
      
      # Convert polygons to centroids for point markers
      if (any(sf::st_geometry_type(sf_result) %in% c("POLYGON", "MULTIPOLYGON"))) {
        sf_result <- sf::st_centroid(sf_result)
      }
      
      sf_result
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })
  
  if (is.null(sites_sf)) {
    return(NULL)
  }
  
  # Join sites with treatment status
  spatial_data <- sites_sf %>%
    left_join(latest_treatments, by = "sitecode") %>%
    mutate(
      treatment_status = ifelse(is.na(treatment_status), "No Treatment", treatment_status),
      last_treatment_date = inspdate,
      last_material = matcode
    ) %>%
    arrange(facility, zone, sitecode)
  
  return(spatial_data)
}
