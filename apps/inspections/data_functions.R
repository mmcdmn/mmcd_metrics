# Inspections App - Data Functions
# =============================================================================
# Core data loading and analysis functions for inspection coverage tracking.
# Uses load_raw_data() as the primary data loading function.
# =============================================================================

library(dplyr)
library(DBI)
library(RPostgres)
library(lubridate)

# Source shared helpers when loaded outside the app (overview registry)
if (!exists("get_db_connection", mode = "function")) {
  source("../../shared/db_helpers.R")
}

# =============================================================================
# LOAD_RAW_DATA - PRIMARY DATA FUNCTION
# =============================================================================
# This is the main data loading function used by both the app and overview.
# Returns detailed inspection data with sites and their inspection records.

#' Load raw inspection data with all filters
#' 
#' @param analysis_date Date for analysis (default Sys.Date())
#' @param facility_filter Vector of facilities to include (NULL = all)
#' @param fosarea_filter Vector of FOS areas to include (NULL = all)
#' @param zone_filter Vector of zones to include (NULL = all)
#' @param priority_filter Vector of priorities to include (NULL = all)
#' @param drone_filter "all", "drone_only", "no_drone", or "include_drone"
#' @param spring_only Boolean - filter to spring inspections only
#' @param prehatch_only Boolean - filter to prehatch sites only
#' @param ... Additional parameters for compatibility (expiring_days, etc.)
#' @return Data frame with site info and inspection records
#' @export
load_raw_data <- function(analysis_date = NULL,
                          facility_filter = NULL,
                          fosarea_filter = NULL,
                          zone_filter = NULL,
                          priority_filter = NULL,
                          drone_filter = "all",
                          spring_only = FALSE,
                          prehatch_only = FALSE,
                          expiring_days = NULL,
                          status_types = NULL,
                          start_year = NULL,
                          end_year = NULL,
                          ...) {
  
  # Default analysis date
  analysis_date <- if (is.null(analysis_date)) Sys.Date() else as.Date(analysis_date)
  
  # =========================================================================
  # OVERVIEW REGISTRY MODE (fast path) - check BEFORE loading full data
  # =========================================================================
  # When called from overview registry, status_types is passed as character(0).
  # Uses a dedicated 3-query approach (~1-2s vs 60s+ for full data load).
  # Filters: Ground sites, prehatch only, include drone, 3-year gap threshold.
  # Action filtering for qualifying inspections: 1, 2, 4, or (3 with wet='0')
  if (!is.null(status_types) && is.character(status_types) && length(status_types) == 0) {
    con_overview <- get_db_connection()
    if (is.null(con_overview)) {
      return(list(
        sites = data.frame(
          facility = character(), zone = character(),
          total_count = integer(), active_count = integer(), expiring_count = integer()
        ),
        treatments = data.frame(),
        pre_aggregated = TRUE
      ))
    }
    
    tryCatch({
      years_gap <- 3
      gap_cutoff <- analysis_date - lubridate::years(years_gap)
      gap_cutoff_str <- as.character(gap_cutoff)
      analysis_date_str <- as.character(analysis_date)
      
      # FAST 3-QUERY APPROACH (~1-2 seconds vs 60+ seconds for single CTE)
      # 1) Get prehatch ground sites with facility+zone
      # 2) Get latest qualifying inspection from current table (covers current season)
      # 3) For sites not recently inspected in current, check archive with IN clause
      
      # Step 1: Prehatch ground sites
      prehatch <- dbGetQuery(con_overview, sprintf("
        SELECT b.sitecode, sc.facility, sc.zone
        FROM loc_breeding_sites b
        INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
        WHERE (b.enddate IS NULL OR b.enddate > '%s'::date)
          AND b.air_gnd = 'G'
          AND b.prehatch IS NOT NULL
      ", analysis_date_str))
      
      if (nrow(prehatch) == 0) {
        safe_disconnect(con_overview)
        return(list(
          sites = data.frame(
            facility = character(), zone = character(),
            total_count = integer(), active_count = integer(), expiring_count = integer()
          ),
          treatments = data.frame(),
          pre_aggregated = TRUE
        ))
      }
      
      # Step 2: Latest qualifying inspection from current table (fast - ~0.3s)
      current_max <- dbGetQuery(con_overview, "
        SELECT sitecode, MAX(inspdate) AS last_insp
        FROM dblarv_insptrt_current
        WHERE inspdate IS NOT NULL
          AND (action IN ('1','2','4') OR (action = '3' AND wet = '0'))
        GROUP BY sitecode
      ")
      
      # Merge current table results
      sites_with_current <- prehatch %>%
        left_join(current_max, by = "sitecode")
      
      # Step 3: Sites needing archive check (not recently inspected in current table)
      need_archive_codes <- sites_with_current %>%
        filter(is.na(last_insp) | last_insp < gap_cutoff) %>%
        pull(sitecode) %>%
        unique()
      
      # Query archive ONLY for sites that need it, using IN clause (fast - ~0.7s)
      archive_max <- data.frame(sitecode = character(0), archive_insp = as.Date(character(0)))
      if (length(need_archive_codes) > 0) {
        chunk_size <- 5000
        archive_parts <- list()
        for (i in seq(1, length(need_archive_codes), by = chunk_size)) {
          chunk <- need_archive_codes[i:min(i + chunk_size - 1, length(need_archive_codes))]
          in_clause <- paste0("'", chunk, "'", collapse = ",")
          q <- sprintf("
            SELECT sitecode, MAX(inspdate) AS archive_insp
            FROM dblarv_insptrt_archive
            WHERE sitecode IN (%s)
              AND inspdate IS NOT NULL
              AND inspdate >= '%s'::date - interval '6 years'
              AND (action IN ('1','2','4') OR (action = '3' AND wet = '0'))
            GROUP BY sitecode
          ", in_clause, analysis_date_str)
          archive_parts[[length(archive_parts) + 1]] <- dbGetQuery(con_overview, q)
        }
        archive_max <- do.call(rbind, archive_parts)
      }
      safe_disconnect(con_overview)
      
      # Combine: use the most recent inspection from either table
      sites_combined <- sites_with_current %>%
        left_join(archive_max, by = "sitecode") %>%
        mutate(
          final_insp = pmax(last_insp, archive_insp, na.rm = TRUE),
          is_gap = is.na(final_insp) | final_insp < gap_cutoff
        )
      
      # Aggregate to facility + zone
      sites <- sites_combined %>%
        group_by(facility, zone) %>%
        summarise(
          total_count = n(),
          active_count = sum(!is_gap),
          expiring_count = sum(is_gap),
          .groups = "drop"
        )
      
      message(sprintf("[inspection_gaps] Overview (fast): %s facilities, %s total sites, %s gaps (%.1f%%)",
                      nrow(sites), sum(sites$total_count), sum(sites$expiring_count),
                      100 * sum(sites$expiring_count) / max(1, sum(sites$total_count))))
      
      return(list(
        sites = sites,
        treatments = data.frame(),
        pre_aggregated = TRUE
      ))
      
    }, error = function(e) {
      warning(paste("[inspection_gaps] Overview fast query failed:", e$message))
      if (!is.null(con_overview)) tryCatch(safe_disconnect(con_overview), error = function(e2) NULL)
      return(list(
        sites = data.frame(
          facility = character(), zone = character(),
          total_count = integer(), active_count = integer(), expiring_count = integer()
        ),
        treatments = data.frame(),
        pre_aggregated = TRUE
      ))
    })
  }
  
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Build filter conditions using shared SQL helpers
    site_filters <- c()
    
    if (is_valid_filter(facility_filter)) {
      facilities_str <- build_sql_in_list(facility_filter)
      site_filters <- c(site_filters, paste0("sc.facility IN (", facilities_str, ")"))
    }
    
    if (is_valid_filter(fosarea_filter)) {
      # Map foreman shortnames to their emp_num (fosarea codes in gis_sectcode)
      foreman_lookup <- get_foremen_lookup()
      if (nrow(foreman_lookup) > 0) {
        fosarea_codes <- character(0)
        for (fosarea in fosarea_filter) {
          matching_foreman <- foreman_lookup[foreman_lookup$shortname == fosarea, ]
          if (nrow(matching_foreman) > 0) {
            fosarea_codes <- c(fosarea_codes, matching_foreman$emp_num)
          }
        }
        if (length(fosarea_codes) > 0) {
          fosarea_codes_formatted <- sprintf("%04d", as.numeric(fosarea_codes))
          fosareas_str <- build_sql_in_list(fosarea_codes_formatted)
          site_filters <- c(site_filters, paste0("sc.fosarea IN (", fosareas_str, ")"))
        }
      }
    }

    if (!is.null(zone_filter) && length(zone_filter) > 0) {
      zones_str <- build_sql_in_list(zone_filter)
      site_filters <- c(site_filters, paste0("sc.zone IN (", zones_str, ")"))
    }
    
    if (is_valid_filter(priority_filter)) {
      priorities_str <- build_sql_in_list(priority_filter)
      site_filters <- c(site_filters, paste0("b.priority IN (", priorities_str, ")"))
    }
    
    # Add drone filtering
    if (!is.null(drone_filter) && drone_filter != "all") {
      if (drone_filter == "drone_only") {
        site_filters <- c(site_filters, "b.drone = 'Y'")
      } else if (drone_filter == "no_drone") {
        site_filters <- c(site_filters, "(b.drone IS NULL OR b.drone != 'Y')")
      }
      # "include_drone" means no additional filter - include all
    }
    
    # Combine filters
    where_clause <- if (length(site_filters) > 0) {
      paste0(" AND ", paste(site_filters, collapse = " AND "))
    } else {
      ""
    }
    
    # COMPREHENSIVE QUERY - gets ALL data we need
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
        b.prehatch
      FROM loc_breeding_sites b
      INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
      WHERE (b.enddate IS NULL OR b.enddate > '%s'::date)
      %s
    ),
    site_years AS (
      SELECT 
        sitecode,
        array_to_string(array_agg(DISTINCT EXTRACT(year FROM inspdate)::text ORDER BY EXTRACT(year FROM inspdate)::text), ', ') as years_with_data
      FROM (
        SELECT sitecode, inspdate FROM dblarv_insptrt_current WHERE inspdate IS NOT NULL
        UNION ALL
        SELECT sitecode, inspdate FROM dblarv_insptrt_archive WHERE inspdate IS NOT NULL
      ) all_inspections
      GROUP BY sitecode
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
      fs.prehatch,
      COALESCE(sy.years_with_data, 'No data') as years_with_data,
      i.inspdate,
      i.action,
      i.numdip,
      i.wet
    FROM filtered_sites fs
    LEFT JOIN site_years sy ON fs.sitecode = sy.sitecode
    LEFT JOIN (
      SELECT sitecode, inspdate, action, numdip, wet
      FROM dblarv_insptrt_current
      UNION ALL
      SELECT sitecode, inspdate, action, numdip, wet
      FROM dblarv_insptrt_archive
    ) i ON fs.sitecode = i.sitecode
    ORDER BY fs.sitecode, i.inspdate DESC
    ", analysis_date, where_clause)
    
    result <- dbGetQuery(con, qry)
    safe_disconnect(con)
    
    # Convert dates
    if (nrow(result) > 0 && "inspdate" %in% names(result)) {
      result$inspdate <- as.Date(result$inspdate)
    }
    
    # Apply prehatch filtering if requested
    if (prehatch_only && nrow(result) > 0) {
      result <- result %>% filter(!is.na(prehatch))
    }
    
    # Apply spring filtering if requested - but keep all sites
    if (spring_only && nrow(result) > 0) {
      spring_thresholds <- get_spring_date_thresholds()
      if (nrow(spring_thresholds) > 0) {
        # Get all unique sites first to preserve them
        all_sites <- result %>%
          distinct(sitecode, facility, fosarea, zone, air_gnd, priority, drone, acres, prehatch)
        
        # Filter inspection records to only spring inspections
        spring_inspections <- result %>%
          filter(!is.na(inspdate)) %>%
          rowwise() %>%
          filter(is_spring_inspection(inspdate, spring_thresholds)) %>%
          ungroup()
        
        # Calculate spring-specific years_with_data
        spring_years <- spring_inspections %>%
          group_by(sitecode) %>%
          summarise(
            years_with_data = paste(sort(unique(year(inspdate))), collapse = ", "),
            .groups = 'drop'
          ) %>%
          mutate(
            years_with_data = ifelse(years_with_data == "", "No spring data", years_with_data)
          )
        
        # Join back to preserve all sites, even those without spring inspections
        result <- all_sites %>%
          left_join(spring_years, by = "sitecode") %>%
          mutate(
            years_with_data = ifelse(is.na(years_with_data), "No spring data", years_with_data)
          ) %>%
          left_join(spring_inspections %>%
                   select(sitecode, inspdate, action, numdip, wet), 
                   by = "sitecode")
      }
    }
    
    return(result)
    
  }, error = function(e) {
    warning(paste("Error in load_raw_data:", e$message))
    if (!is.null(con)) safe_disconnect(con)
    return(data.frame())
  })
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Get site choices from gis_sectcode
get_site_choices <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(list(facility = NULL, fosarea = NULL, zone = NULL))
  df <- dbGetQuery(con, "SELECT DISTINCT facility, fosarea, zone FROM gis_sectcode ORDER BY facility, fosarea, zone")
  safe_disconnect(con)
  list(
    facility = sort(unique(df$facility)),
    fosarea = sort(unique(df$fosarea)),
    zone = sort(unique(df$zone))
  )
}

# Check if an inspection date is a "spring inspection"
is_spring_inspection <- function(inspdate, spring_thresholds) {
  if (is.na(inspdate) || nrow(spring_thresholds) == 0) return(FALSE)
  
  insp_year <- year(inspdate)
  
  # Find the spring threshold for this year
  year_threshold <- spring_thresholds[spring_thresholds$year == insp_year, ]
  
  if (nrow(year_threshold) == 0) return(FALSE)
  
  # Spring inspection: after Jan 1st and before the threshold date
  spring_cutoff <- year_threshold$date_start[1]
  year_start <- as.Date(paste0(insp_year, "-01-01"))
  
  return(inspdate >= year_start && inspdate < spring_cutoff)
}

# =============================================================================
# ANALYSIS FUNCTIONS - Work with data from load_raw_data()
# =============================================================================

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
    group_by(sitecode, facility, fosarea, zone, air_gnd, priority, acres, years_with_data, prehatch) %>%
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
    group_by(sitecode, facility, fosarea, zone, air_gnd, priority, acres, years_with_data, prehatch) %>%
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

# Get comprehensive gap analysis by facility and FOS area
get_facility_gap_analysis <- function(comprehensive_data, gap_data) {
  if (nrow(comprehensive_data) == 0) return(data.frame())
  
  # Get total sites by facility and fosarea from comprehensive data
  total_sites <- comprehensive_data %>%
    distinct(sitecode, facility, fosarea) %>%
    count(facility, fosarea, name = "total_sites")
  
  # Get gap sites by facility and fosarea
  gap_sites <- gap_data %>%
    count(facility, fosarea, name = "gap_sites")
  
  # Combine and calculate percentages
  facility_analysis <- total_sites %>%
    left_join(gap_sites, by = c("facility", "fosarea")) %>%
    mutate(
      gap_sites = ifelse(is.na(gap_sites), 0, gap_sites),
      recently_inspected_sites = total_sites - gap_sites,
      gap_percentage = round(100 * gap_sites / total_sites, 1),
      recently_inspected_percentage = round(100 * recently_inspected_sites / total_sites, 1)
    ) %>%
    filter(total_sites > 0)  # Only include facilities with sites
  
  return(facility_analysis)
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
    group_by(sitecode, facility, fosarea, zone, air_gnd, priority, drone, acres, years_with_data, prehatch) %>%
    arrange(desc(inspdate)) %>%
    slice(1) %>%
    ungroup()
  
  # Get sites that never had inspections
  all_sites <- comprehensive_data %>%
    distinct(sitecode, facility, fosarea, zone, air_gnd, priority, drone, acres, years_with_data, prehatch)
  
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
    select(sitecode, facility, fosarea, zone, air_gnd, priority, drone, acres, years_with_data, prehatch,
           last_inspection_date, last_numdip, days_since_inspection, inspection_status) %>%
    bind_rows(never_inspected) %>%
    arrange(last_inspection_date, sitecode)
  
  return(gap_sites)
}

message("✓ inspections/data_functions.R loaded")

