# Ground Prehatch Progress - Data Functions
# Functions for fetching and processing ground prehatch data

# Function to get ground prehatch data from database
get_ground_prehatch_data <- function(zone_filter, simulation_date = Sys.Date()) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Use the provided SQL query with slight modifications for current year
    current_year <- format(Sys.Date(), "%Y")
    
    query <- sprintf("
WITH ActiveSites_g AS (
  SELECT sc.facility, sc.zone, sc.fosarea, left(b.sitecode,7) AS sectcode,
         b.sitecode, acres, air_gnd, priority, prehatch, drone, remarks,
         sc.fosarea as foreman
  FROM loc_breeding_sites b
  LEFT JOIN gis_sectcode sc ON left(b.sitecode,7)=sc.sectcode
  WHERE (b.enddate IS NULL OR b.enddate>'%s-05-01')
    AND b.air_gnd='G'
  ORDER BY sc.facility, sc.sectcode, b.sitecode, b.prehatch
)
SELECT sitecnts.facility, sitecnts.zone, sitecnts.fosarea, sitecnts.sectcode, sitecnts.foreman,
       tot_ground, not_prehatch_sites, prehatch_sites_cnt, drone_sites_cnt,
       COALESCE(ph_treated_cnt, 0) AS ph_treated_cnt,
       COALESCE(ph_expiring_cnt, 0) AS ph_expiring_cnt,
       COALESCE(ph_expired_cnt, 0) AS ph_expired_cnt,
       prehatch_sites_cnt-(COALESCE(ph_treated_cnt,0)+COALESCE(ph_expiring_cnt, 0)) AS ph_notactivetrt_cnt
FROM
(SELECT facility, zone, fosarea, sectcode, foreman,
        COUNT(CASE WHEN (air_gnd='G') THEN 1 END) AS tot_ground,
        COUNT(CASE WHEN (air_gnd='G' AND prehatch IS NULL) THEN 1 END) AS not_prehatch_sites,
        COUNT(CASE WHEN (air_gnd='G' AND prehatch IN ('PREHATCH','BRIQUET')) THEN 1 END) AS prehatch_sites_cnt,
        COUNT(CASE WHEN (air_gnd='G' AND drone IN ('Y','M','C')) THEN 1 END) AS drone_sites_cnt
 FROM ActiveSites_g a
 GROUP BY facility, zone, fosarea, sectcode, foreman
 ORDER BY facility, zone, fosarea, sectcode, foreman) sitecnts
LEFT JOIN(
SELECT facility, zone, fosarea, sectcode, foreman,
       COUNT(CASE WHEN (prehatch_status='treated') THEN 1 END) AS ph_treated_cnt,
       COUNT(CASE WHEN (prehatch_status='expiring') THEN 1 END) AS ph_expiring_cnt,
       COUNT(CASE WHEN (prehatch_status='expired') THEN 1 END) AS ph_expired_cnt
FROM (  
  SELECT facility, zone, fosarea, sectcode, foreman, sitecode,
         CASE
           WHEN age > COALESCE(effect_days::integer, 150)::double precision THEN 'expired'::text
           WHEN days_retrt_early IS NOT NULL AND age > days_retrt_early::double precision THEN 'expiring'::text
           WHEN age<= effect_days::integer::double precision THEN 'treated'::text
           ELSE 'unknown'::text
         END AS prehatch_status,
         inspdate, matcode, age, effect_days, days_retrt_early
  FROM (  
    SELECT DISTINCT ON (a.sitecode)
           a.sitecode, a.sectcode, a.facility, a.fosarea, a.zone, a.foreman,
           c.pkey_pg AS insptrt_id,
           date_part('days'::text, '%s'::timestamp - c.inspdate::timestamp with time zone) AS age,
           c.matcode, c.inspdate, p.effect_days, p.days_retrt_early
    FROM (SELECT * FROM dblarv_insptrt_current WHERE inspdate>'%s-01-01' AND inspdate <= '%s') c
    JOIN activesites_g a ON c.sitecode = a.sitecode
    JOIN (SELECT * FROM mattype_list_targetdose WHERE prehatch IS TRUE) p USING (matcode)
    ORDER BY a.sitecode, c.inspdate DESC, c.insptime DESC
  ) s_grd
  ORDER BY sitecode
) list
GROUP BY facility, zone, fosarea, sectcode, foreman
ORDER BY facility, zone, fosarea, sectcode, foreman
) trtcnts ON trtcnts.sectcode = sitecnts.sectcode AND COALESCE(trtcnts.foreman, '') = COALESCE(sitecnts.foreman, '')
ORDER BY sectcode", current_year, format(simulation_date, "%Y-%m-%d"), current_year, format(simulation_date, "%Y-%m-%d"))
    
    result <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    # Map facility names for display
    result <- map_facility_names(result)
    
    return(result)
    
  }, error = function(e) {
    warning(paste("Error loading ground prehatch data:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}

# Function to get site details data
get_site_details_data <- function(expiring_days = 14, simulation_date = Sys.Date()) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    current_year <- format(Sys.Date(), "%Y")
    
    query <- sprintf("
WITH ActiveSites_g AS (
  SELECT sc.facility, sc.zone, sc.fosarea, left(b.sitecode,7) AS sectcode,
         b.sitecode, acres, air_gnd, priority, prehatch, drone, remarks,
         sc.fosarea as foreman
  FROM loc_breeding_sites b
  LEFT JOIN gis_sectcode sc ON left(b.sitecode,7)=sc.sectcode
  WHERE (b.enddate IS NULL OR b.enddate>'%s-05-01')
    AND b.air_gnd='G'
  ORDER BY sc.facility, sc.sectcode, b.sitecode, b.prehatch
)
SELECT a.sitecode, a.sectcode, a.facility, a.fosarea, a.zone, a.foreman, a.acres, a.priority, a.prehatch,
       s.prehatch_status, s.inspdate, s.matcode, s.age, s.effect_days
FROM activesites_g a 
LEFT JOIN (
  SELECT sitecode, sectcode, facility, fosarea, zone, foreman,
         CASE
           WHEN age > COALESCE(effect_days::integer, 150)::double precision THEN 'expired'::text
           WHEN days_retrt_early IS NOT NULL AND age > days_retrt_early::double precision THEN 'expiring'::text
           WHEN age<= effect_days::integer::double precision THEN 'treated'::text
           ELSE 'unknown'::text
         END AS prehatch_status,
         inspdate, matcode, age, effect_days, days_retrt_early
  FROM (  
    SELECT DISTINCT ON (a.sitecode)
           a.sitecode, a.sectcode, a.facility, a.fosarea, a.zone, a.foreman,
           c.pkey_pg AS insptrt_id,
           date_part('days'::text, '%s'::timestamp - c.inspdate::timestamp with time zone) AS age,
           c.matcode, c.inspdate, p.effect_days, p.days_retrt_early
    FROM (SELECT * FROM dblarv_insptrt_current WHERE inspdate>'%s-01-01' AND inspdate <= '%s') c
    JOIN activesites_g a ON c.sitecode = a.sitecode
    JOIN (SELECT * FROM mattype_list_targetdose WHERE prehatch IS TRUE) p USING (matcode)
    ORDER BY a.sitecode, c.inspdate DESC, c.insptime DESC
  ) s_grd
  ORDER BY sitecode
) s USING (sitecode, sectcode, facility, fosarea, zone, foreman)
WHERE a.prehatch IN ('PREHATCH','BRIQUET')
ORDER BY a.facility, a.sectcode, a.sitecode", current_year, format(simulation_date, "%Y-%m-%d"), current_year, format(simulation_date, "%Y-%m-%d"))
    
    result <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    # Map facility names for display
    result <- map_facility_names(result)
    
    return(result)
    
  }, error = function(e) {
    warning(paste("Error loading site details:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
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
aggregate_data_by_group <- function(data, group_by, zone_filter = NULL, show_expiring_only = FALSE, site_details = NULL, combine_zones = FALSE) {
  if (nrow(data) == 0) return(data)
  
  # Map facility names for display
  data <- map_facility_names(data)
  
  # If "Show Only Expiring Sites" is checked, filter to only expiring sites before aggregation
  if (show_expiring_only && !is.null(site_details)) {
    expiring_sites <- site_details %>% filter(prehatch_status == "expiring")
    
    if (nrow(expiring_sites) > 0) {
      # Filter data to only sections that have expiring sites
      expiring_sections <- unique(expiring_sites$sectcode)
      data <- data %>% filter(sectcode %in% expiring_sections)
      
      # Zero out all counts except expiring
      data <- data %>%
        mutate(
          ph_treated_cnt = 0,
          ph_expired_cnt = 0,
          ph_notactivetrt_cnt = 0
        )
    } else {
      # No expiring sites, return empty data
      return(data.frame())
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
          ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
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
          ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          display_name = "MMCD",
          group_name = "MMCD"
        )
    }
  } else if (group_by == "sectcode") {
    if (show_zones_separately) {
      # Group by section and zone
      result <- data %>%
        group_by(sectcode, zone) %>%
        summarise(
          tot_ground = sum(tot_ground, na.rm = TRUE),
          not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
          prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
          ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          display_name = paste0(sectcode, " (P", zone, ")"),
          group_name = sectcode
        )
    } else {
      # Group by section only
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
          ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          display_name = sectcode,
          group_name = sectcode
        )
    }
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
          ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
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
          ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
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
          ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
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
          ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
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