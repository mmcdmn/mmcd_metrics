# Inspections App - Legacy Functions
# =============================================================================
# The pre-rewrite analysis functions, moved here UNCHANGED (except the loader
# rename below) because other code still calls them:
#   - shared/embed_producers.R   (embed views: gaps, analytics, larvae, red_bug_gaps)
#   - api/routes/inspections.R   (/red-bug-gaps, /red-bug-gaps/by-group, /larval)
#   - llm/tool_registry.py       (via the API endpoints above)
#
# They are NOT used by the Inspections web app, which is built on
# coverage_functions.R. Their definition of a "red bug gap" (no red bug in N
# years) is intentionally different from the 51% coverage rule (red bug in 5
# years OR sampled this year) - see coverage_functions.R.
#
# Sourced by data_functions.R; do not source this file directly.
# =============================================================================

#' Load every site with all of its inspection records (the old comprehensive query)
#'
#' load_raw_data() in data_functions.R dispatches here for everything except the
#' Overview registry call (status_types = character(0)).
#' Returns a plain data frame - embed producers check is.data.frame().
#'
#' @param analysis_date Date for analysis (default Sys.Date())
#' @param facility_filter Vector of facilities to include (NULL = all)
#' @param fosarea_filter Vector of FOS areas to include (NULL = all)
#' @param zone_filter Vector of zones to include (NULL = all)
#' @param priority_filter Vector of priorities to include (NULL = all)
#' @param drone_filter "all", "drone_only", "no_drone", or "include_drone"
#' @param spring_only Boolean - filter to spring inspections only
#' @param prehatch_only Boolean - filter to prehatch sites only
#' @param ... Absorbs arguments meant for other loaders (expiring_days, status_types, ...)
#' @return Data frame with site info and inspection records
load_legacy_inspection_data <- function(analysis_date = NULL,
                                        facility_filter = NULL,
                                        fosarea_filter = NULL,
                                        zone_filter = NULL,
                                        priority_filter = NULL,
                                        drone_filter = "all",
                                        spring_only = FALSE,
                                        prehatch_only = FALSE,
                                        start_year = NULL,
                                        end_year = NULL,
                                        ...) {

  # Default analysis date
  analysis_date <- if (is.null(analysis_date)) Sys.Date() else as.Date(analysis_date)

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
    warning(paste("Error in load_legacy_inspection_data:", e$message))
    if (!is.null(con)) safe_disconnect(con)
    return(data.frame())
  })
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
# ANALYSIS FUNCTIONS - Work with data from load_legacy_inspection_data()
# =============================================================================

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

# =============================================================================
# RED BUG GAP ANALYSIS
# =============================================================================
# Finds sites where red bugs haven't been found in inspections for N+ years.
# Queries dblarv_sample_current/archive for redblue = 'R' linked via sampnum_yr.

#' Get red bug gap data - sites without red bugs found for N+ years
#'
#' @param years_gap Number of years threshold
#' @param facility_filter Optional facility filter
#' @param fosarea_filter Optional FOS area filter
#' @param zone_filter Optional zone filter
#' @param priority_filter Optional priority filter
#' @param air_gnd_filter "A", "G", or "both"
#' @param drone_filter drone filter option
#' @param prehatch_only Boolean
#' @param ref_date Reference date for analysis
#' @return Data frame with site info and last red bug date
get_red_bug_gaps <- function(years_gap = 5,
                             facility_filter = NULL,
                             fosarea_filter = NULL,
                             zone_filter = NULL,
                             priority_filter = NULL,
                             air_gnd_filter = "both",
                             drone_filter = "all",
                             prehatch_only = FALSE,
                             ref_date = Sys.Date()) {
  
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    ref_date_str <- as.character(ref_date)
    gap_cutoff <- ref_date - lubridate::years(years_gap)
    gap_cutoff_str <- as.character(gap_cutoff)
    
    # Build site filter conditions
    site_filters <- c()
    
    if (is_valid_filter(facility_filter)) {
      facilities_str <- build_sql_in_list(facility_filter)
      site_filters <- c(site_filters, paste0("sc.facility IN (", facilities_str, ")"))
    }
    
    if (is_valid_filter(fosarea_filter)) {
      foreman_lookup <- get_foremen_lookup()
      if (nrow(foreman_lookup) > 0) {
        fosarea_codes <- character(0)
        for (fos in fosarea_filter) {
          matching <- foreman_lookup[foreman_lookup$shortname == fos, ]
          if (nrow(matching) > 0) {
            fosarea_codes <- c(fosarea_codes, matching$emp_num)
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
    
    if (!is.null(air_gnd_filter) && air_gnd_filter != "both") {
      site_filters <- c(site_filters, paste0("b.air_gnd = '", air_gnd_filter, "'"))
    }
    
    if (!is.null(drone_filter) && drone_filter != "all") {
      if (drone_filter == "drone_only") {
        site_filters <- c(site_filters, "b.drone = 'Y'")
      } else if (drone_filter == "no_drone") {
        site_filters <- c(site_filters, "(b.drone IS NULL OR b.drone != 'Y')")
      }
    }
    
    if (prehatch_only) {
      site_filters <- c(site_filters, "b.prehatch IS NOT NULL")
    }
    
    where_clause <- if (length(site_filters) > 0) {
      paste0(" AND ", paste(site_filters, collapse = " AND "))
    } else {
      ""
    }
    
    # Step 1: Get filtered sites
    sites_qry <- sprintf("
      SELECT b.sitecode, sc.facility, sc.fosarea, sc.zone, 
             b.air_gnd, b.priority, b.drone, b.acres, b.prehatch
      FROM loc_breeding_sites b
      INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
      WHERE (b.enddate IS NULL OR b.enddate > '%s'::date)
      %s
    ", ref_date_str, where_clause)
    
    sites <- dbGetQuery(con, sites_qry)
    
    if (nrow(sites) == 0) {
      safe_disconnect(con)
      return(data.frame())
    }
    
    # Step 2: Get latest red bug inspection from current table
    current_red <- dbGetQuery(con, "
      SELECT i.sitecode, MAX(i.inspdate) AS last_red_bug
      FROM dblarv_insptrt_current i
      INNER JOIN dblarv_sample_current s ON i.sampnum_yr = s.sampnum_yr
      WHERE i.inspdate IS NOT NULL
        AND i.sampnum_yr IS NOT NULL
        AND s.redblue = 'R'
      GROUP BY i.sitecode
    ")
    
    # Step 3: Merge with sites and find those needing archive check
    sites_merged <- sites %>%
      left_join(current_red, by = "sitecode")
    
    need_archive <- sites_merged %>%
      filter(is.na(last_red_bug) | last_red_bug < gap_cutoff) %>%
      pull(sitecode) %>%
      unique()
    
    # Step 4: Check archive for sites that need it
    archive_red <- data.frame(sitecode = character(0), archive_red = as.Date(character(0)))
    if (length(need_archive) > 0) {
      chunk_size <- 5000
      archive_parts <- list()
      for (i in seq(1, length(need_archive), by = chunk_size)) {
        chunk <- need_archive[i:min(i + chunk_size - 1, length(need_archive))]
        in_clause <- paste0("'", chunk, "'", collapse = ",")
        q <- sprintf("
          SELECT i.sitecode, MAX(i.inspdate) AS archive_red
          FROM dblarv_insptrt_archive i
          INNER JOIN dblarv_sample_archive s ON i.sampnum_yr = s.sampnum_yr
          WHERE i.sitecode IN (%s)
            AND i.inspdate IS NOT NULL
            AND i.sampnum_yr IS NOT NULL
            AND s.redblue = 'R'
          GROUP BY i.sitecode
        ", in_clause)
        archive_parts[[length(archive_parts) + 1]] <- dbGetQuery(con, q)
      }
      archive_red <- do.call(rbind, archive_parts)
    }
    
    safe_disconnect(con)
    
    # Step 5: Combine current and archive results
    result <- sites_merged %>%
      left_join(archive_red, by = "sitecode") %>%
      mutate(
        last_red_bug_date = pmax(last_red_bug, archive_red, na.rm = TRUE),
        days_since_red_bug = as.numeric(ref_date - last_red_bug_date),
        red_bug_status = case_when(
          is.na(last_red_bug_date) ~ "Never Found",
          last_red_bug_date < gap_cutoff ~ "Red Bug Gap",
          TRUE ~ "Recently Found"
        )
      ) %>%
      select(sitecode, facility, fosarea, zone, air_gnd, priority, drone, acres, prehatch,
             last_red_bug_date, days_since_red_bug, red_bug_status)
    
    # Map fosarea emp_num to foreman shortname for display
    foremen <- get_foremen_lookup()
    if (nrow(foremen) > 0) {
      fosarea_map <- setNames(foremen$shortname, sprintf("%04d", as.numeric(foremen$emp_num)))
      result$fos_name <- ifelse(
        result$fosarea %in% names(fosarea_map),
        fosarea_map[result$fosarea],
        result$fosarea
      )
    } else {
      result$fos_name <- result$fosarea
    }
    
    # Filter to only gap sites (never found + gap)
    gap_sites <- result %>%
      filter(red_bug_status != "Recently Found") %>%
      arrange(last_red_bug_date, sitecode)
    
    return(gap_sites)
    
  }, error = function(e) {
    warning(paste("Error in get_red_bug_gaps:", e$message))
    if (!is.null(con)) safe_disconnect(con)
    return(data.frame())
  })
}

#' Aggregate red bug gap data by facility (for bar chart)
get_red_bug_facility_analysis <- function(gap_data, all_sites_data) {
  if (nrow(all_sites_data) == 0) return(data.frame())
  
  total_sites <- all_sites_data %>%
    distinct(sitecode, facility) %>%
    count(facility, name = "total_sites")
  
  gap_sites <- gap_data %>%
    count(facility, name = "gap_sites")
  
  facility_analysis <- total_sites %>%
    left_join(gap_sites, by = "facility") %>%
    mutate(
      gap_sites = ifelse(is.na(gap_sites), 0, gap_sites),
      recently_found_sites = total_sites - gap_sites,
      gap_percentage = round(100 * gap_sites / total_sites, 1),
      recently_found_percentage = round(100 * recently_found_sites / total_sites, 1)
    ) %>%
    filter(total_sites > 0)
  
  return(facility_analysis)
}

#' Aggregate red bug gap data by FOS/foreman (for bar chart)
get_red_bug_fos_analysis <- function(gap_data, all_sites_data) {
  if (nrow(all_sites_data) == 0) return(data.frame())
  
  # Map fosarea to foreman names for all sites
  foremen <- get_foremen_lookup()
  if (nrow(foremen) > 0) {
    fosarea_map <- setNames(foremen$shortname, sprintf("%04d", as.numeric(foremen$emp_num)))
    all_sites_data$fos_name <- ifelse(
      all_sites_data$fosarea %in% names(fosarea_map),
      fosarea_map[all_sites_data$fosarea],
      all_sites_data$fosarea
    )
  } else {
    all_sites_data$fos_name <- all_sites_data$fosarea
  }
  
  total_sites <- all_sites_data %>%
    distinct(sitecode, fos_name) %>%
    count(fos_name, name = "total_sites")
  
  gap_sites <- gap_data %>%
    count(fos_name, name = "gap_sites")
  
  fos_analysis <- total_sites %>%
    left_join(gap_sites, by = "fos_name") %>%
    mutate(
      gap_sites = ifelse(is.na(gap_sites), 0, gap_sites),
      recently_found_sites = total_sites - gap_sites,
      gap_percentage = round(100 * gap_sites / total_sites, 1),
      recently_found_percentage = round(100 * recently_found_sites / total_sites, 1)
    ) %>%
    filter(total_sites > 0)
  
  return(fos_analysis)
}

#' Get all sites (not just gaps) for computing totals - same query as get_red_bug_gaps but returns all
get_red_bug_all_sites <- function(facility_filter = NULL,
                                  fosarea_filter = NULL,
                                  zone_filter = NULL,
                                  priority_filter = NULL,
                                  air_gnd_filter = "both",
                                  drone_filter = "all",
                                  prehatch_only = FALSE,
                                  ref_date = Sys.Date()) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    ref_date_str <- as.character(ref_date)
    
    site_filters <- c()
    
    if (is_valid_filter(facility_filter)) {
      facilities_str <- build_sql_in_list(facility_filter)
      site_filters <- c(site_filters, paste0("sc.facility IN (", facilities_str, ")"))
    }
    
    if (is_valid_filter(fosarea_filter)) {
      foreman_lookup <- get_foremen_lookup()
      if (nrow(foreman_lookup) > 0) {
        fosarea_codes <- character(0)
        for (fos in fosarea_filter) {
          matching <- foreman_lookup[foreman_lookup$shortname == fos, ]
          if (nrow(matching) > 0) {
            fosarea_codes <- c(fosarea_codes, matching$emp_num)
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
    
    if (!is.null(air_gnd_filter) && air_gnd_filter != "both") {
      site_filters <- c(site_filters, paste0("b.air_gnd = '", air_gnd_filter, "'"))
    }
    
    if (!is.null(drone_filter) && drone_filter != "all") {
      if (drone_filter == "drone_only") {
        site_filters <- c(site_filters, "b.drone = 'Y'")
      } else if (drone_filter == "no_drone") {
        site_filters <- c(site_filters, "(b.drone IS NULL OR b.drone != 'Y')")
      }
    }
    
    if (prehatch_only) {
      site_filters <- c(site_filters, "b.prehatch IS NOT NULL")
    }
    
    where_clause <- if (length(site_filters) > 0) {
      paste0(" AND ", paste(site_filters, collapse = " AND "))
    } else {
      ""
    }
    
    qry <- sprintf("
      SELECT b.sitecode, sc.facility, sc.fosarea, sc.zone,
             b.air_gnd, b.priority, b.drone, b.acres, b.prehatch
      FROM loc_breeding_sites b
      INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
      WHERE (b.enddate IS NULL OR b.enddate > '%s'::date)
      %s
    ", ref_date_str, where_clause)
    
    result <- dbGetQuery(con, qry)
    safe_disconnect(con)
    return(result)
    
  }, error = function(e) {
    warning(paste("Error in get_red_bug_all_sites:", e$message))
    if (!is.null(con)) safe_disconnect(con)
    return(data.frame())
  })
}

message("✓ inspections/data_functions.R loaded")

