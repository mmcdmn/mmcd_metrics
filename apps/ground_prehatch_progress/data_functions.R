# Ground Prehatch Progress - Data Functions
# Functions for fetching and processing ground prehatch data

# Source shared helpers
source("../../shared/db_helpers.R")

# Unified function to load raw ground prehatch data 
load_raw_data <- function(analysis_date = Sys.Date(), include_archive = FALSE, 
                         start_year = NULL, end_year = NULL, include_geometry = FALSE) {
  con <- get_db_connection()
  if (is.null(con)) return(list(ground_sites = data.frame(), ground_treatments = data.frame()))
  
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
      # Historical mode: use the working query pattern but with archive
      treatments_query <- sprintf("
      SELECT c.sitecode, c.inspdate, c.matcode, c.insptime,
             c.acres as treated_acres, p.effect_days, 'current' as data_source
      FROM (SELECT * FROM dblarv_insptrt_current WHERE inspdate>='%d-01-01' AND inspdate <= '%d-12-31') c
      JOIN (
        SELECT sc.facility, sc.zone, sc.fosarea, left(b.sitecode,7) AS sectcode,
               b.sitecode, acres, air_gnd, priority, prehatch, drone, remarks,
               sc.fosarea as foreman
        FROM loc_breeding_sites b
        LEFT JOIN gis_sectcode sc ON left(b.sitecode,7)=sc.sectcode
        WHERE (b.enddate IS NULL OR b.enddate>'%d-05-01')
          AND b.air_gnd='G'
          AND b.prehatch IN ('PREHATCH','BRIQUET')
      ) a ON c.sitecode = a.sitecode
      JOIN (SELECT * FROM mattype_list_targetdose WHERE prehatch IS TRUE) p USING (matcode)
      
      UNION ALL
      
      SELECT c.sitecode, c.inspdate, c.matcode, c.insptime,
             c.acres as treated_acres, p.effect_days, 'archive' as data_source
      FROM (SELECT * FROM dblarv_insptrt_archive WHERE inspdate>='%d-01-01' AND inspdate <= '%d-12-31') c
      JOIN (
        SELECT sc.facility, sc.zone, sc.fosarea, left(b.sitecode,7) AS sectcode,
               b.sitecode, acres, air_gnd, priority, prehatch, drone, remarks,
               sc.fosarea as foreman
        FROM loc_breeding_sites b
        LEFT JOIN gis_sectcode sc ON left(b.sitecode,7)=sc.sectcode
        WHERE (b.enddate IS NULL OR b.enddate>'%d-05-01')
          AND b.air_gnd='G'
          AND b.prehatch IN ('PREHATCH','BRIQUET')
      ) a ON c.sitecode = a.sitecode
      JOIN (SELECT * FROM mattype_list_targetdose WHERE prehatch IS TRUE) p USING (matcode)
      
      ORDER BY inspdate DESC, sitecode
      ", start_year, end_year, start_year, start_year, end_year, start_year)
    } else {
      # Current mode: use exact working pattern with current year from analysis_date
      analysis_year <- format(analysis_date, "%Y")
      treatments_query <- sprintf("
      SELECT c.sitecode, c.inspdate, c.matcode, c.insptime,
             c.acres as treated_acres, p.effect_days, 'current' as data_source
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
      ORDER BY c.inspdate DESC, c.sitecode
      ", analysis_year, format(analysis_date, "%Y-%m-%d"), analysis_year)
    }
    
    ground_treatments <- dbGetQuery(con, treatments_query)
    
    dbDisconnect(con)
    
    # Convert data types
    if (nrow(ground_sites) > 0) {
      ground_sites <- map_facility_names(ground_sites)
    }
    
    # No need to convert dates - they're already Date objects from PostgreSQL
    
    return(list(
      ground_sites = ground_sites,
      ground_treatments = ground_treatments
    ))
    
  }, error = function(e) {
    warning(paste("Error loading raw data:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(list(ground_sites = data.frame(), ground_treatments = data.frame()))
  })
}

# Function to get ground prehatch data from database
# Uses load_raw_data as single source of truth and processes status like get_site_details_data
get_ground_prehatch_data <- function(zone_filter, simulation_date = Sys.Date(), expiring_days = 14) {
  # Load raw data using the unified function
  raw_data <- load_raw_data(analysis_date = simulation_date, include_archive = FALSE)
  
  if (nrow(raw_data$ground_sites) == 0) {
    return(data.frame())
  }
  
  # Calculate treatment status for each site
  site_details <- raw_data$ground_sites
  
  if (nrow(raw_data$ground_treatments) > 0) {
    # Get latest treatment for each site
    latest_treatments <- raw_data$ground_treatments %>%
      group_by(sitecode) %>%
      arrange(desc(inspdate), desc(insptime)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        age = as.numeric(simulation_date - inspdate),
        prehatch_status = case_when(
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
  }
  
  # Sites without treatments are expired/unknown (count as expired)
  site_details <- site_details %>%
    mutate(
      prehatch_status = ifelse(is.na(prehatch_status), "expired", prehatch_status)
    )
  
  # Aggregate by sectcode and foreman to match expected structure
  result <- site_details %>%
    group_by(facility, zone, fosarea, sectcode) %>%
    summarise(
      foreman = first(fosarea),  # Use fosarea as foreman (gis_sectcode is source of truth)
      tot_ground = n(),  
      not_prehatch_sites = 0, 
      prehatch_sites_cnt = n(),  # All prehatch sites
      drone_sites_cnt = 0, 
      ph_treated_cnt = sum(prehatch_status == "treated", na.rm = TRUE),
      ph_expiring_cnt = sum(prehatch_status == "expiring", na.rm = TRUE),
      ph_expired_cnt = sum(prehatch_status == "expired" | is.na(prehatch_status), na.rm = TRUE),
      ph_untreated_cnt = sum(is.na(prehatch_status), na.rm = TRUE),
      .groups = "drop"
    )
  
  return(result)
}

# Function to get site details data - uses same source as charts
get_site_details_data <- function(expiring_days = 14, simulation_date = Sys.Date()) {
  # Load raw data using the unified function - same as charts use
  raw_data <- load_raw_data(analysis_date = simulation_date, include_archive = FALSE)
  
  if (nrow(raw_data$ground_sites) == 0) {
    return(data.frame())
  }
  
  # Start with all ground sites (gis_sectcode is source of truth)
  result <- raw_data$ground_sites
  
  if (nrow(raw_data$ground_treatments) > 0) {
    # Get latest treatment for each site
    latest_treatments <- raw_data$ground_treatments %>%
      group_by(sitecode) %>%
      arrange(desc(inspdate), desc(insptime)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        age = as.numeric(simulation_date - inspdate),
        prehatch_status = case_when(
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
  
  # Filter by facility
  if (!is.null(facility_filter) && !("all" %in% facility_filter)) {
    data <- data %>% filter(facility %in% facility_filter)
  }
  
  # Filter by foreman
  if (!is.null(foreman_filter) && !("all" %in% foreman_filter)) {
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
      expiring_sites <- site_details %>% filter(prehatch_status == "expiring")
      
      if (nrow(expiring_sites) > 0) {
        # Filter data to only sections that have expiring sites
        expiring_sections <- unique(expiring_sites$sectcode)
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
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          display_name = paste0("MMCD (P", zone, ")"),
          group_name = "MMCD"
        )
    } else {
      # Group all together
      result <- data %>%
        summarise(
          tot_ground = sum(tot_ground, na.rm = TRUE),
          not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
          prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
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
        drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
        ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
        ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
        ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
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
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          facility_display = map_facility_names(data.frame(facility = facility))$facility_display,
          display_name = paste0(facility_display, " (P", zone, ")"),
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
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
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
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          foreman_name = sapply(foreman, function(f) {
            matches <- which(trimws(as.character(foremen_lookup$emp_num)) == trimws(as.character(f)))
            if(length(matches) > 0) foremen_lookup$shortname[matches[1]] else paste0("FOS #", f)
          }),
          display_name = paste0(foreman_name, " (P", zone, ")"),
          group_name = foreman,
          combined_group = paste0(foreman, " (P", zone, ")")
        )
    } else {
      # Group by foreman only
      result <- data %>%
        group_by(foreman) %>%
        summarise(
          tot_ground = sum(tot_ground, na.rm = TRUE),
          not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
          prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
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
                           round(100 * ph_expiring_cnt / prehatch_sites_cnt, 1), 0)
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
  
  if (nrow(raw_data$ground_sites) == 0) {
    return(NULL)
  }
  
  # Apply zone filter
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    raw_data$ground_sites <- raw_data$ground_sites %>% 
      filter(zone %in% zone_filter)
  }
  
  # Apply facility filter
  if (!is.null(facility_filter) && !"all" %in% facility_filter) {
    raw_data$ground_sites <- raw_data$ground_sites %>% 
      filter(facility %in% facility_filter)
  }
  
  # Apply foreman filter (using fosarea)
  if (!is.null(foreman_filter) && !"all" %in% foreman_filter) {
    raw_data$ground_sites <- raw_data$ground_sites %>% 
      filter(fosarea %in% foreman_filter)
  }
  
  # Get current date for status calculations
  current_date <- as.Date(analysis_date)
  
  # Get latest treatment for each site with proper effect_days from database
  if (!is.null(raw_data$ground_treatments) && nrow(raw_data$ground_treatments) > 0) {
    latest_treatments <- raw_data$ground_treatments %>%
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
    if ("geometry" %in% names(raw_data$ground_sites) && !all(is.na(raw_data$ground_sites$geometry))) {
      # Convert to sf using WKT
      sf_result <- sf::st_as_sf(raw_data$ground_sites, wkt = "geometry", crs = 4326)
      
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