# Inspections App - Data Functions

library(dplyr)
library(DBI)
library(RPostgres)
library(lubridate)

# Get site choices from gis_sectcode
get_site_choices <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(list(facility = NULL, fosarea = NULL, zone = NULL))
  df <- dbGetQuery(con, "SELECT DISTINCT facility, fosarea, zone FROM gis_sectcode ORDER BY facility, fosarea, zone")
  dbDisconnect(con)
  list(
    facility = sort(unique(df$facility)),
    fosarea = sort(unique(df$fosarea)),
    zone = sort(unique(df$zone))
  )
}

# SINGLE UNIFIED DATA RETRIEVAL - Get ALL inspection data in one query
get_all_inspection_data <- function(facility_filter = NULL, fosarea_filter = NULL, zone_filter = NULL, priority_filter = NULL, drone_filter = "all") {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Build filter conditions
    site_filters <- c()
    
    if (!is.null(facility_filter) && length(facility_filter) > 0 && !"all" %in% facility_filter) {
      facilities_str <- paste0("'", facility_filter, "'", collapse = ",")
      site_filters <- c(site_filters, paste0("sc.facility IN (", facilities_str, ")"))
    }
    
    if (!is.null(fosarea_filter) && length(fosarea_filter) > 0 && !"all" %in% fosarea_filter) {
      # Map foreman shortnames to their emp_num (which corresponds to fosarea codes in gis_sectcode)
      foreman_lookup <- get_foremen_lookup()
      if (nrow(foreman_lookup) > 0) {
        # Convert shortnames to emp_num values (fosarea codes)
        fosarea_codes <- character(0)
        for (fosarea in fosarea_filter) {
          matching_foreman <- foreman_lookup[foreman_lookup$shortname == fosarea, ]
          if (nrow(matching_foreman) > 0) {
            fosarea_codes <- c(fosarea_codes, matching_foreman$emp_num)
          }
        }
        if (length(fosarea_codes) > 0) {
          # Format fosarea codes as 4-digit strings to match gis_sectcode format
          fosarea_codes_formatted <- sprintf("%04d", as.numeric(fosarea_codes))
          fosareas_str <- paste0("'", fosarea_codes_formatted, "'", collapse = ",")
          site_filters <- c(site_filters, paste0("sc.fosarea IN (", fosareas_str, ")"))
        }
      }
    }

    if (!is.null(zone_filter) && length(zone_filter) > 0) {
      zones_str <- paste0("'", zone_filter, "'", collapse = ",")
      site_filters <- c(site_filters, paste0("sc.zone IN (", zones_str, ")"))
    }
    
    if (!is.null(priority_filter) && length(priority_filter) > 0 && !"all" %in% priority_filter) {
      priorities_str <- paste0("'", priority_filter, "'", collapse = ",")
      site_filters <- c(site_filters, paste0("b.priority IN (", priorities_str, ")"))
    }
    
    # Add drone filtering
    if (!is.null(drone_filter) && drone_filter != "all") {
      if (drone_filter == "drone_only") {
        site_filters <- c(site_filters, "b.drone = 'Y'")
      } else if (drone_filter == "no_drone") {
        site_filters <- c(site_filters, "(b.drone IS NULL OR b.drone != 'Y')")
      }
    }
    
    # Combine filters
    where_clause <- if (length(site_filters) > 0) {
      paste0(" AND ", paste(site_filters, collapse = " AND "))
    } else {
      ""
    }
    
    # SINGLE COMPREHENSIVE QUERY - gets ALL data we need
    qry <- sprintf("
    WITH filtered_sites AS (
      SELECT 
        b.sitecode,
        sc.facility,
        sc.fosarea,
        sc.zone,
        b.air_gnd,
        b.priority,
        b.drone,
        b.acres,
        CASE 
          WHEN b.startdate IS NOT NULL THEN CURRENT_DATE - b.startdate::date
          ELSE NULL
        END as days_active
      FROM loc_breeding_sites b
      INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
      WHERE b.enddate IS NULL
      %s
    )
    SELECT 
      fs.sitecode,
      fs.facility,
      fs.fosarea,
      fs.zone,
      fs.air_gnd,
      fs.priority,
      fs.drone,
      fs.acres,
      fs.days_active,
      i.inspdate,
      i.action,
      i.numdip,
      i.wet
    FROM filtered_sites fs
    LEFT JOIN (
      SELECT sitecode, inspdate, action, numdip, wet
      FROM dblarv_insptrt_current
      UNION ALL
      SELECT sitecode, inspdate, action, numdip, wet
      FROM dblarv_insptrt_archive
    ) i ON fs.sitecode = i.sitecode
    ORDER BY fs.sitecode, i.inspdate DESC
    ", where_clause)
    
    result <- dbGetQuery(con, qry)
    dbDisconnect(con)
    
    # Convert dates
    if (nrow(result) > 0 && "inspdate" %in% names(result)) {
      result$inspdate <- as.Date(result$inspdate)
    }
    
    return(result)
    
  }, error = function(e) {
    warning(paste("Error in get_all_inspection_data:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}



# Get total number of active sites from comprehensive data
get_total_sites_count_from_data <- function(comprehensive_data, air_gnd_filter = "both") {
  if (nrow(comprehensive_data) == 0) return(0)
  
  sites_data <- comprehensive_data %>%
    distinct(sitecode, air_gnd)
  
  if (air_gnd_filter == "both") {
    return(nrow(sites_data))
  } else {
    filtered_sites <- sites_data %>%
      filter(air_gnd == air_gnd_filter)
    return(nrow(filtered_sites))
  }
}

# Get sites over larvae threshold from comprehensive data - with action filtering
get_high_larvae_sites_from_data <- function(comprehensive_data, threshold = 2, years_back = 5, air_gnd_filter = "both") {
  if (nrow(comprehensive_data) == 0) return(data.frame())
  
  cutoff_date <- Sys.Date() - years(years_back)
  
  # Filter data - apply action filtering for larvae analysis
  filtered_data <- comprehensive_data %>%
    filter(!is.na(numdip), 
           !is.na(inspdate),
           inspdate >= cutoff_date,
           action %in% c('1','2','4') | (action == '3' & wet == '0'))  # Action filtering for larvae analysis
  
  if (air_gnd_filter != "both") {
    filtered_data <- filtered_data %>%
      filter(air_gnd == air_gnd_filter)
  }
  
  # Find sites with high larvae counts - show frequency, not just max
  high_larvae_sites <- filtered_data %>%
    group_by(sitecode, facility, fosarea, zone, air_gnd, priority, acres, days_active) %>%
    summarise(
      total_inspections = n(),
      threshold_exceedances = sum(numdip >= threshold, na.rm = TRUE),
      max_dip_count = max(numdip, na.rm = TRUE),
      avg_dip_count = round(mean(numdip, na.rm = TRUE), 1),
      last_high_date = ifelse(
        any(numdip >= threshold, na.rm = TRUE),
        max(inspdate[numdip >= threshold], na.rm = TRUE),
        NA_real_
      ),
      first_high_date = ifelse(
        any(numdip >= threshold, na.rm = TRUE),
        min(inspdate[numdip >= threshold], na.rm = TRUE),
        NA_real_
      ),
      .groups = 'drop'
    ) %>%
    filter(threshold_exceedances > 0) %>%
    mutate(
      exceedance_frequency = round(100.0 * threshold_exceedances / total_inspections, 1),
      last_high_date = as.Date(last_high_date, origin = "1970-01-01"),
      first_high_date = as.Date(first_high_date, origin = "1970-01-01")
    ) %>%
    arrange(desc(exceedance_frequency), desc(threshold_exceedances), desc(max_dip_count))
  
  return(high_larvae_sites)
}

# Get wet frequency analysis from comprehensive data - NO ACTION FILTERING FOR WET DATA
get_wet_frequency_from_data <- function(comprehensive_data, air_gnd_filter = "both", min_inspections = 5, years_back = 5) {
  if (nrow(comprehensive_data) == 0) return(data.frame())
  
  # Filter data by years back
  cutoff_date <- Sys.Date() - years(years_back)
  
  # Filter data - include ALL records with wet data regardless of action
  filtered_data <- comprehensive_data %>%
    filter(!is.na(inspdate), inspdate >= cutoff_date, !is.na(wet))
  
  if (air_gnd_filter != "both") {
    filtered_data <- filtered_data %>%
      filter(air_gnd == air_gnd_filter)
  }
  
  # Calculate wet frequency by site - updated logic for numeric values
  # 0 = dry, 1-9 = various wet levels, A = flooded (>100%)
  wet_frequency <- filtered_data %>%
    group_by(sitecode, facility, fosarea, zone, air_gnd, priority, acres, days_active) %>%
    summarise(
      total_inspections = n(),
      wet_count = sum(wet %in% c('1', '2', '3', '4', '5', '6', '7', '8', '9', 'A'), na.rm = TRUE),
      flooded_count = sum(wet == 'A', na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(total_inspections >= min_inspections) %>%
    mutate(
      wet_percentage = round(100.0 * wet_count / total_inspections, 1),
      flooded_percentage = round(100.0 * flooded_count / total_inspections, 1)
    ) %>%
    arrange(desc(wet_percentage), desc(total_inspections))
  
  return(wet_frequency)
}

# Get summary statistics from comprehensive data
get_summary_stats_from_data <- function(comprehensive_data, air_gnd_filter = "both", years_back = 5) {
  if (nrow(comprehensive_data) == 0) return(list())
  
  # Filter data by years back
  cutoff_date <- Sys.Date() - years(years_back)
  
  # Filter data
  filtered_data <- comprehensive_data %>%
    filter(!is.na(inspdate), inspdate >= cutoff_date)
  
  if (air_gnd_filter != "both") {
    filtered_data <- filtered_data %>%
      filter(air_gnd == air_gnd_filter)
  }
  
  # Calculate wet statistics - NO action filtering, just need wet values
  wet_data <- filtered_data %>%
    filter(!is.na(wet))
  
  wet_stats <- list(
    sites_with_wet_data = wet_data %>% distinct(sitecode) %>% nrow(),
    total_wet_records = nrow(wet_data),
    total_wet_positive = sum(wet_data$wet %in% c('1', '2', '3', '4', '5', '6', '7', '8', '9', 'A'), na.rm = TRUE),
    total_flooded = sum(wet_data$wet == 'A', na.rm = TRUE)
  )
  
  wet_stats$overall_wet_percentage <- if (wet_stats$total_wet_records > 0) {
    round(100.0 * wet_stats$total_wet_positive / wet_stats$total_wet_records, 1)
  } else {
    0
  }
  
  # Calculate larvae statistics - WITH action filtering
  larvae_data <- filtered_data %>%
    filter(!is.na(numdip),
           action %in% c('1','2','4') | (action == '3' & wet == '0'))  # Action filtering for larvae stats
  
  larvae_stats <- list(
    sites_with_larvae_data = larvae_data %>% distinct(sitecode) %>% nrow(),
    total_larvae_records = nrow(larvae_data),
    avg_dip_count = if (nrow(larvae_data) > 0) round(mean(larvae_data$numdip, na.rm = TRUE), 2) else 0,
    max_dip_count = if (nrow(larvae_data) > 0) max(larvae_data$numdip, na.rm = TRUE) else 0
  )
  
  return(c(wet_stats, larvae_stats))
}

# Get inspection gaps from comprehensive data - with action filtering
get_inspection_gaps_from_data <- function(comprehensive_data, years_gap, ref_date = Sys.Date()) {
  if (nrow(comprehensive_data) == 0) return(data.frame())
  
  gap_cutoff <- ref_date - years(years_gap)
  
  # Get the most recent inspection per site - apply action filtering
  site_inspections <- comprehensive_data %>%
    filter(!is.na(inspdate),
           action %in% c('1','2','4') | (action == '3' & wet == '0')) %>%  # Action filtering for gaps
    group_by(sitecode, facility, fosarea, zone, air_gnd, priority, drone, acres, days_active) %>%
    arrange(desc(inspdate)) %>%
    slice(1) %>%
    ungroup()
  
  # Get sites that never had inspections
  all_sites <- comprehensive_data %>%
    distinct(sitecode, facility, fosarea, zone, air_gnd, priority, drone, acres, days_active)
  
  never_inspected <- all_sites %>%
    anti_join(site_inspections, by = "sitecode") %>%
    mutate(
      last_inspection_date = as.Date('1900-01-01'),
      numdip = NA_real_,
      days_since_inspection = 999999,
      inspection_status = "Never Inspected"
    )
  
  # Combine and filter for gaps
  gap_sites <- site_inspections %>%
    mutate(
      last_inspection_date = inspdate,
      last_numdip = numdip,
      days_since_inspection = as.numeric(ref_date - inspdate),
      inspection_status = case_when(
        inspdate < gap_cutoff ~ "Inspection Gap",
        TRUE ~ "Recently Inspected"
      )
    ) %>%
    filter(inspdate < gap_cutoff) %>%
    select(sitecode, facility, fosarea, zone, air_gnd, priority, drone, acres, days_active,
           last_inspection_date, last_numdip, days_since_inspection, inspection_status) %>%
    bind_rows(never_inspected) %>%
    arrange(last_inspection_date, sitecode)
  
  return(gap_sites)
}



