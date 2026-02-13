# Cattail Treatments - Data Functions
# Functions for loading and processing cattail treatment data

# Source shared helpers
source("../../shared/db_helpers.R")

library(dplyr)
library(lubridate)
library(sf)

# NOTE: map_facility_names is defined in shared/db_helpers.R
# It creates a facility_display column with full names while keeping facility as short codes

# Function to load raw cattail treatment data (STANDARD FUNCTION)
load_raw_data <- function(analysis_date = Sys.Date(), include_archive = FALSE,
                          start_year = NULL, end_year = NULL, include_geometry = FALSE,
                          expiring_days = 7, facility_filter = "all", foreman_filter = "all", 
                          status_types = character(0), zone_filter = c("1", "2")) {
  
  # Handle NULL analysis_date
  if (is.null(analysis_date)) {
    analysis_date <- Sys.Date()
  } else if (is.character(analysis_date)) {
    analysis_date <- as.Date(analysis_date)
  }
  
  # Automatically include archive if analysis_date is before current year
  if (year(analysis_date) < year(Sys.Date())) {
    include_archive <- TRUE
  }
  
  # Determine year range
  if (is.null(start_year)) start_year <- as.numeric(format(analysis_date, "%Y")) - 2
  if (is.null(end_year)) end_year <- as.numeric(format(analysis_date, "%Y"))
  
  # Set date range
  start_date <- as.Date(paste0(start_year, "-01-01"))
  end_date <- as.Date(paste0(end_year, "-12-31"))
  
  tryCatch({
    # Get database connection
    con <- get_db_connection()
    if (is.null(con)) {
      warning("Could not connect to database")
      return(NULL)
    }
    
    # Main cattail inspections query - filter for sites that had cattail inspections
    cattail_treatments_query <- "
      WITH breeding_sites AS (
        SELECT 
          sc.facility, 
          sc.zone, 
          sc.fosarea, 
          LEFT(b.sitecode,7) AS sectcode,
          b.sitecode,
          b.acres,
          b.air_gnd,
          CASE WHEN b.drone IS NOT NULL THEN 'D' ELSE NULL END as drone,
          sc.fosarea as foreman
        FROM public.loc_breeding_sites b
        LEFT JOIN public.gis_sectcode sc ON LEFT(b.sitecode,7) = sc.sectcode
        WHERE (b.enddate IS NULL OR b.enddate > $1)
      ),
      inspection_data AS (
        -- Get the most recent cattail inspection per site
        WITH all_inspections AS (
          -- Current year inspections
          SELECT 
            i.sitecode,
            i.inspdate,
            i.action,
            i.numdip,
            COALESCE(i.acres_plan, b.acres) as acres_plan,
            EXTRACT(year FROM i.inspdate) as year
          FROM public.dblarv_insptrt_current i
          LEFT JOIN public.loc_breeding_sites b ON i.sitecode = b.sitecode
          WHERE i.inspdate BETWEEN $2 AND $1
            AND i.action = '9'  -- Action 9 = inspected
          
          UNION ALL
          
          -- Archive inspections (if include_archive is true)
          SELECT 
            a.sitecode,
            a.inspdate,
            a.action,
            a.numdip,
            COALESCE(a.acres_plan, b.acres) as acres_plan,
            EXTRACT(year FROM a.inspdate) as year
          FROM public.dblarv_insptrt_archive a
          LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
          WHERE $3 = TRUE
            AND a.inspdate BETWEEN $2 AND $1
            AND a.action = '9'  -- Action 9 = inspected
        )
        SELECT DISTINCT ON (sitecode)
          sitecode,
          inspdate,
          action,
          numdip,
          acres_plan,
          year
        FROM all_inspections
        ORDER BY sitecode, inspdate DESC  -- Most recent inspection per site
      )
      SELECT 
        b.sitecode,
        b.sectcode,
        b.acres,
        b.air_gnd,
        b.drone,
        b.facility,
        b.zone,
        b.fosarea,
        b.foreman,
        -- Add display and calculated fields
        b.facility as facility_display,  -- We'll map this later
        -- Determine treatment state - all sites are inspected since we use INNER JOIN
        CASE 
          WHEN i.sitecode IS NOT NULL THEN 'inspected'
          ELSE 'inspected'  -- This shouldn't happen with INNER JOIN
        END as inspection_status,
        CASE 
          WHEN i.numdip > 0 THEN 'need_treatment'
          WHEN i.numdip = 0 THEN 'under_threshold' 
          ELSE 'under_threshold'  -- Default to under_threshold for safety
        END as treatment_status,
        i.inspdate,
        i.numdip,
        i.acres_plan,
        i.year as inspection_year
      FROM breeding_sites b
      INNER JOIN inspection_data i ON b.sitecode = i.sitecode  -- Only inspected sites
      ORDER BY b.sitecode
    "
    
    # Execute query with parameters
    cattail_sites <- dbGetQuery(con, cattail_treatments_query, list(
      analysis_date,           # $1 - for site enddate filter AND inspection date filter (no future inspections!)
      start_date,              # $2 - start of inspection date range (3 years back)
      include_archive          # $3
    ))
    
    safe_disconnect(con)
    
    # Create the state column - all sites are inspected, just need treatment_status
    cattail_sites <- cattail_sites %>%
      mutate(
        state = case_when(
          treatment_status == "need_treatment" ~ "need_treatment",
          treatment_status == "under_threshold" ~ "under_threshold",
          TRUE ~ "under_threshold"  # Default
        ),
        # Initialize final_status to treatment_status - will be updated in aggregate_cattail_data
        final_status = treatment_status
      )
    
    # Add facility mapping - creates facility_display with full names, keeps facility as short codes
    cattail_sites <- map_facility_names(cattail_sites, "facility")
    
    # Add facility_code for filtering - same as short code facility
    cattail_sites$facility_code <- cattail_sites$facility
    
    # Load treatment data for current year (filtered by analysis_date)
    current_year <- if (!is.null(end_year)) end_year else year(analysis_date)
    treatments <- load_cattail_treatments(analysis_date = analysis_date, current_year = current_year)
    
    # NOTE: We do NOT add "treatment-only" sites anymore
    # Cattail treatments are ONLY valid if there was a prior fall inspection
    # Those sites should already be in cattail_sites from the inspection query
    # Any site with treatments but no inspection is incorrectly coded and should not appear
    
    # Add standardized is_active and is_expiring columns
    # For cattail treatments:
    # - is_active: Sites that need treatment (numdip > 0) but aren't treated yet
    # - is_expiring: Sites that were treated but might need retreatment (not really applicable to cattail)
    
    # Get treated sites to update status
    treated_sites <- character(0)
    if (!is.null(treatments) && nrow(treatments) > 0) {
      # Determine inspection year based on analysis_date
      analysis_year <- year(analysis_date)
      analysis_month <- month(analysis_date)
      
      if (analysis_month >= 1 && analysis_month <= 7) {
        inspection_year_filter <- analysis_year - 1
      } else {
        inspection_year_filter <- analysis_year
      }
      
      treated_sites <- treatments %>%
        filter(inspection_year == inspection_year_filter) %>%
        pull(sitecode) %>%
        unique()
    }
    
    cattail_sites <- cattail_sites %>%
      mutate(
        # Update final status based on treatments
        final_status = case_when(
          sitecode %in% treated_sites ~ "treated",
          treatment_status == "need_treatment" ~ "need_treatment", 
          treatment_status == "under_threshold" ~ "under_threshold",
          TRUE ~ "under_threshold"
        ),
        # Standard columns for overview compatibility
        is_active = case_when(
          final_status == "need_treatment" ~ TRUE,
          final_status == "treated" ~ TRUE,  # Count treated as active for metrics
          TRUE ~ FALSE
        ),
        is_expiring = case_when(
          final_status == "need_treatment" ~ TRUE,  # Sites needing treatment = "expiring"
          TRUE ~ FALSE
        )
      )
    
    # Use inspection acres_plan as primary acres source for overview display
    # b.acres from loc_breeding_sites is often NULL for active cattail sites,
    # while acres_plan from inspections is more reliably populated
    cattail_sites <- cattail_sites %>%
      mutate(acres = ifelse(!is.na(acres_plan) & acres_plan > 0, acres_plan,
                            ifelse(!is.na(acres) & acres > 0, acres, 0)))
    
    # Return STANDARDIZED format
    return(list(
      sites = cattail_sites,
      treatments = treatments,
      total_count = nrow(cattail_sites)
    ))
    
  }, error = function(e) {
    warning(paste("Error loading cattail data:", e$message))
    if (!is.null(con)) safe_disconnect(con)
    return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
  })
}

# Function to load cattail treatment data
load_cattail_treatments <- function(analysis_date = NULL, current_year = NULL) {
  
  # Handle NULL analysis_date
  if (is.null(analysis_date)) {
    analysis_date <- Sys.Date()
  } else if (is.character(analysis_date)) {
    analysis_date <- as.Date(analysis_date)
  }
  
  # Handle NULL current_year
  if (is.null(current_year)) {
    current_year <- year(analysis_date)
  }
  
  tryCatch({
    # Get database connection
    con <- get_db_connection()
    if (is.null(con)) {
      warning("Could not connect to database")
      return(data.frame())
    }
    
    # Treatments query - actions '3', 'A', and 'D' with improved cattail material codes
    # Filter by analysis_date to exclude future treatments
    treatments_query <- "
      SELECT 
        i.sitecode,
        LEFT(i.sitecode,7) AS sectcode,
        i.inspdate AS trtdate, 
        i.action, 
        i.mattype, 
        i.matcode, 
        i.amts, 
        i.acres, 
        sc.facility,
        sc.zone,
        sc.fosarea,
        EXTRACT(year FROM i.inspdate) as trt_year,
        EXTRACT(month FROM i.inspdate) as trt_month
      FROM public.dblarv_insptrt_current i
      LEFT JOIN public.gis_sectcode sc ON LEFT(i.sitecode,7) = sc.sectcode
      WHERE i.action IN ('3', 'A', 'D')  -- Actions 3, A, D = treatments
        AND i.inspdate <= $1  -- Only treatments up to analysis_date
        AND i.matcode IN (
          SELECT matcode 
          FROM public.mattype_list_targetdose
          WHERE prgassign_default = 'Cat' OR prg_alt1 = 'Cat'
        )
        AND (
          -- Fall/winter treatments using DOY (day 244-365: Sept 1 - Dec 31)
          EXTRACT(DOY FROM i.inspdate) BETWEEN 244 AND 365
          OR
          -- Spring/summer treatments using DOY (day 135-213: May 15 - Aug 1) 
          EXTRACT(DOY FROM i.inspdate) BETWEEN 135 AND 213
        )
      
      UNION ALL
      
      SELECT 
        a.sitecode,
        LEFT(a.sitecode,7) AS sectcode,
        a.inspdate AS trtdate, 
        a.action, 
        a.mattype, 
        a.matcode, 
        a.amts, 
        a.acres, 
        sc.facility,
        sc.zone,
        sc.fosarea,
        EXTRACT(year FROM a.inspdate) as trt_year,
        EXTRACT(month FROM a.inspdate) as trt_month
      FROM public.dblarv_insptrt_archive a
      LEFT JOIN public.gis_sectcode sc ON LEFT(a.sitecode,7) = sc.sectcode
      WHERE a.action IN ('3', 'A', 'D')  -- Actions 3, A, D = treatments
        AND a.inspdate <= $1  -- Only treatments up to analysis_date
        AND a.matcode IN (
          SELECT matcode 
          FROM public.mattype_list_targetdose
          WHERE prgassign_default = 'Cat' OR prg_alt1 = 'Cat'
        )
        AND (
          -- Fall/winter treatments using DOY (day 244-365: Sept 1 - Dec 31)
          EXTRACT(DOY FROM a.inspdate) BETWEEN 244 AND 365
          OR
          -- Spring/summer treatments using DOY (day 135-213: May 15 - Aug 1) 
          EXTRACT(DOY FROM a.inspdate) BETWEEN 135 AND 213
        )
      
      ORDER BY trtdate
    "
    
    # Execute query with analysis_date parameter to filter out future treatments
    treatments <- dbGetQuery(con, treatments_query, list(analysis_date))
    safe_disconnect(con)
    
    if (nrow(treatments) == 0) {
      return(data.frame())
    }
    
    # Add facility mapping
    treatments <- map_facility_names(treatments, "facility")
    
    # Add treatment season classification
    treatments <- treatments %>%
      mutate(
        trtdate = as.Date(trtdate),
        treatment_season = case_when(
          trt_month %in% c(9, 10, 11, 12) ~ "Fall/Winter",
          trt_month %in% c(5, 6, 7, 8) ~ "Spring/Summer", 
          TRUE ~ "Other"
        ),
        inspection_year = case_when(
          treatment_season == "Fall/Winter" ~ trt_year,
          treatment_season == "Spring/Summer" ~ trt_year - 1,
          TRUE ~ trt_year
        ),
        action_desc = case_when(
          action == '3' ~ "Treatment",
          action == 'A' ~ "Treatment",
          action == 'D' ~ "Treatment", 
          TRUE ~ paste("Action", action)
        ),
        # Add inspdate alias for overview system compatibility
        inspdate = trtdate
      )
    
    return(treatments)
    
  }, error = function(e) {
    warning(paste("Error loading cattail treatments:", e$message))
    if (exists("con") && !is.null(con)) safe_disconnect(con)
    return(data.frame())
  })
}

# Function to aggregate cattail treatment data with 3-state metrics
aggregate_cattail_data <- function(cattail_data, analysis_date = NULL) {
  
  # Check for data using new standardized structure
  sites_data <- NULL
  if (!is.null(cattail_data$sites) && nrow(cattail_data$sites) > 0) {
    sites_data <- cattail_data$sites
  } else if (!is.null(cattail_data$cattail_sites) && nrow(cattail_data$cattail_sites) > 0) {
    # Fallback to old structure for backwards compatibility during transition
    sites_data <- cattail_data$cattail_sites
  }
  
  if (is.null(sites_data) || nrow(sites_data) == 0) {
    warning("No cattail data to aggregate")
    return(NULL)
  }
  
  if (is.null(analysis_date)) {
    analysis_date <- Sys.Date()
  }
  
  # Determine which inspection year to use based on analysis_date
  analysis_year <- year(analysis_date)
  analysis_month <- month(analysis_date)
  
  # Cattail logic: Fall inspections (Aug-Dec) drive treatments in same fall OR following spring
  # If analysis_date is Jan-July, we want PREVIOUS YEAR's fall inspections
  # If analysis_date is Aug-Dec, we want CURRENT YEAR's fall inspections
  if (analysis_month >= 1 && analysis_month <= 7) {
    # Spring/early summer: use previous fall's inspections
    inspection_year_filter <- analysis_year - 1
  } else {
    # Fall/winter: use current year's inspections
    inspection_year_filter <- analysis_year
  }
  
  # Filter sites to relevant inspection year
  sites_data <- sites_data %>%
    filter(inspection_year == inspection_year_filter)
    
  treatments <- cattail_data$treatments
  current_year <- analysis_year
  
  cat("Aggregating data for year", analysis_year, "using inspection year", inspection_year_filter, "(", nrow(sites_data), "sites)\n")
  
  # Match treatments to sites - focus on current inspection cycle
  if (!is.null(treatments) && nrow(treatments) > 0) {
    # For the inspection cycle, we want treatments that map to the inspection year
    # Treatments can be in same fall OR following spring
    
    treated_sites <- treatments %>%
      filter(inspection_year == inspection_year_filter) %>%
      pull(sitecode) %>%
      unique()
    
    cat("Found", length(treated_sites), "treated sites\n")
    
    # Update treatment status to include 'treated' state
    sites_data <- sites_data %>%
      mutate(
        final_status = case_when(
          sitecode %in% treated_sites ~ "treated",
          treatment_status == "need_treatment" ~ "need_treatment", 
          treatment_status == "under_threshold" ~ "under_threshold",
          TRUE ~ "under_threshold"
        ),
        # Update state field to match final_status for UI compatibility
        state = final_status
      )
    
    cat("Sites marked as treated:", sum(sites_data$final_status == "treated"), "\n")
  } else {
    # No treatments data, use original status
    sites_data$final_status <- sites_data$treatment_status
    sites_data$state <- sites_data$treatment_status
    cat("No treatments found for year", analysis_year, "\n")
  }
  
  # Calculate 3-state metrics using final_status
  total_summary <- data.frame(
    # Basic counts
    total_count = nrow(sites_data),
    total_acres = sum(sites_data$acres, na.rm = TRUE),
    
    # Inspection status
    inspected_sites = sum(sites_data$inspection_status == 'inspected', na.rm = TRUE),
    not_inspected_sites = sum(sites_data$inspection_status == 'not_inspected', na.rm = TRUE),
    
    # Treatment status (for inspected sites only) - using final_status
    need_treatment_sites = sum(sites_data$final_status == 'need_treatment', na.rm = TRUE),
    under_threshold_sites = sum(sites_data$final_status == 'under_threshold', na.rm = TRUE),
    treated_sites = sum(sites_data$final_status == 'treated', na.rm = TRUE),
    
    # Acres by status
    inspected_acres = sum(sites_data$acres[sites_data$inspection_status == 'inspected'], na.rm = TRUE),
    under_threshold_acres = sum(sites_data$acres[sites_data$final_status == 'under_threshold'], na.rm = TRUE),
    need_treatment_acres = sum(sites_data$acres[sites_data$final_status == 'need_treatment'], na.rm = TRUE),
    treated_acres = sum(sites_data$acres[sites_data$final_status == 'treated'], na.rm = TRUE),
    
    # Inspection coverage percentage
    inspection_coverage = round(
      100 * sum(sites_data$inspection_status == 'inspected', na.rm = TRUE) / nrow(sites_data), 1
    ),
    
    # Treatment need percentage (of inspected sites)
    treatment_need_rate = ifelse(
      sum(sites_data$inspection_status == 'inspected', na.rm = TRUE) > 0,
      round(100 * sum(sites_data$final_status == 'need_treatment', na.rm = TRUE) / 
            sum(sites_data$inspection_status == 'inspected', na.rm = TRUE), 1),
      0
    ),
    # Treatment completion rate (of sites needing treatment)
    treatment_completion_rate = ifelse(
      sum(sites_data$final_status %in% c('need_treatment', 'treated'), na.rm = TRUE) > 0,
      round(100 * sum(sites_data$final_status == 'treated', na.rm = TRUE) / 
            sum(sites_data$final_status %in% c('need_treatment', 'treated'), na.rm = TRUE), 1),
      0
    )
  )
  
  # Facility summary with 3-state metrics
  facility_summary <- sites_data %>%
    group_by(facility) %>%
    summarise(
      facility_code = first(facility),
      total_count = n(),
      total_acres = sum(acres, na.rm = TRUE),
      
      # Inspection metrics
      inspected_sites = sum(inspection_status == 'inspected', na.rm = TRUE),
      not_inspected_sites = sum(inspection_status == 'not_inspected', na.rm = TRUE),
      inspection_coverage = round(100 * sum(inspection_status == 'inspected', na.rm = TRUE) / n(), 1),
      
      # Treatment metrics (for inspected sites)
      need_treatment_sites = sum(treatment_status == 'need_treatment', na.rm = TRUE),
      under_threshold_sites = sum(treatment_status == 'under_threshold', na.rm = TRUE),
      need_treatment_acres = sum(acres[treatment_status == 'need_treatment'], na.rm = TRUE),
      
      # Treatment need rate (of inspected sites)
      treatment_need_rate = ifelse(
        sum(inspection_status == 'inspected', na.rm = TRUE) > 0,
        round(100 * sum(treatment_status == 'need_treatment', na.rm = TRUE) / 
              sum(inspection_status == 'inspected', na.rm = TRUE), 1),
        0
      ),
      
      .groups = 'drop'
    ) %>%
    arrange(facility)
  
  # Zone summary with 3-state metrics
  zone_summary <- sites_data %>%
    filter(!is.na(zone)) %>%
    group_by(facility, zone) %>%
    summarise(
      facility_code = first(facility),
      total_count = n(),
      total_acres = sum(acres, na.rm = TRUE),
      
      # Inspection metrics
      inspected_sites = sum(inspection_status == 'inspected', na.rm = TRUE),
      not_inspected_sites = sum(inspection_status == 'not_inspected', na.rm = TRUE),
      inspection_coverage = round(100 * sum(inspection_status == 'inspected', na.rm = TRUE) / n(), 1),
      
      # Treatment metrics
      need_treatment_sites = sum(treatment_status == 'need_treatment', na.rm = TRUE),
      under_threshold_sites = sum(treatment_status == 'under_threshold', na.rm = TRUE),
      need_treatment_acres = sum(acres[treatment_status == 'need_treatment'], na.rm = TRUE),
      
      # Treatment need rate
      treatment_need_rate = ifelse(
        sum(inspection_status == 'inspected', na.rm = TRUE) > 0,
        round(100 * sum(treatment_status == 'need_treatment', na.rm = TRUE) / 
              sum(inspection_status == 'inspected', na.rm = TRUE), 1),
        0
      ),
      
      .groups = 'drop'
    ) %>%
    arrange(facility, zone)
  
  # Summary by foreman/FOS area
  fos_summary <- sites_data %>%
    filter(!is.na(fosarea)) %>%
    group_by(facility, fosarea) %>%
    summarise(
      facility_code = first(facility),
      total_count = n(),
      total_acres = sum(acres, na.rm = TRUE),
      
      # Inspection metrics
      inspected_sites = sum(inspection_status == 'inspected', na.rm = TRUE),
      inspection_coverage = round(100 * sum(inspection_status == 'inspected', na.rm = TRUE) / n(), 1),
      
      # Treatment metrics
      need_treatment_sites = sum(treatment_status == 'need_treatment', na.rm = TRUE),
      need_treatment_acres = sum(acres[treatment_status == 'need_treatment'], na.rm = TRUE),
      
      .groups = 'drop'
    ) %>%
    arrange(facility, fosarea)

  
  # Facility summary with 3-state metrics
  facility_summary <- sites_data %>%
    group_by(facility) %>%
    summarise(
      facility_code = first(facility),
      total_count = n(),
      total_acres = sum(acres, na.rm = TRUE),
      
      # Inspection metrics
      inspected_sites = sum(inspection_status == 'inspected', na.rm = TRUE),
      not_inspected_sites = sum(inspection_status == 'not_inspected', na.rm = TRUE),
      inspection_coverage = round(100 * sum(inspection_status == 'inspected', na.rm = TRUE) / n(), 1),
      
      # Treatment metrics (for inspected sites)
      need_treatment_sites = sum(treatment_status == 'need_treatment', na.rm = TRUE),
      under_threshold_sites = sum(treatment_status == 'under_threshold', na.rm = TRUE),
      need_treatment_acres = sum(acres[treatment_status == 'need_treatment'], na.rm = TRUE),
      
      # Treatment need rate (of inspected sites)
      treatment_need_rate = ifelse(
        sum(inspection_status == 'inspected', na.rm = TRUE) > 0,
        round(100 * sum(treatment_status == 'need_treatment', na.rm = TRUE) / 
              sum(inspection_status == 'inspected', na.rm = TRUE), 1),
        0
      ),
      
      .groups = 'drop'
    ) %>%
    arrange(facility)
  
  # Zone summary with 3-state metrics
  zone_summary <- sites_data %>%
    filter(!is.na(zone)) %>%
    group_by(facility, zone) %>%
    summarise(
      facility_code = first(facility),
      total_count = n(),
      total_acres = sum(acres, na.rm = TRUE),
      
      # Inspection metrics
      inspected_sites = sum(inspection_status == 'inspected', na.rm = TRUE),
      not_inspected_sites = sum(inspection_status == 'not_inspected', na.rm = TRUE),
      inspection_coverage = round(100 * sum(inspection_status == 'inspected', na.rm = TRUE) / n(), 1),
      
      # Treatment metrics
      need_treatment_sites = sum(treatment_status == 'need_treatment', na.rm = TRUE),
      under_threshold_sites = sum(treatment_status == 'under_threshold', na.rm = TRUE),
      need_treatment_acres = sum(acres[treatment_status == 'need_treatment'], na.rm = TRUE),
      
      # Treatment need rate
      treatment_need_rate = ifelse(
        sum(inspection_status == 'inspected', na.rm = TRUE) > 0,
        round(100 * sum(treatment_status == 'need_treatment', na.rm = TRUE) / 
              sum(inspection_status == 'inspected', na.rm = TRUE), 1),
        0
      ),
      
      .groups = 'drop'
    ) %>%
    arrange(facility, zone)
  
  # Summary by foreman/FOS area
  fos_summary <- sites_data %>%
    filter(!is.na(fosarea)) %>%
    group_by(facility, fosarea) %>%
    summarise(
      facility_code = first(facility),
      total_count = n(),
      total_acres = sum(acres, na.rm = TRUE),
      
      # Inspection metrics
      inspected_sites = sum(inspection_status == 'inspected', na.rm = TRUE),
      inspection_coverage = round(100 * sum(inspection_status == 'inspected', na.rm = TRUE) / n(), 1),
      
      # Treatment metrics
      need_treatment_sites = sum(treatment_status == 'need_treatment', na.rm = TRUE),
      need_treatment_acres = sum(acres[treatment_status == 'need_treatment'], na.rm = TRUE),
      
      .groups = 'drop'
    ) %>%
    arrange(facility, fosarea)
  
  return(list(
    sites_data = sites_data,  # Include the processed site data with final_status
    total_summary = total_summary,
    facility_summary = facility_summary, 
    zone_summary = zone_summary,
    fos_summary = fos_summary,
    analysis_date = analysis_date
  ))
}

# Function to get basic filter choices for startup (before main data load)
get_basic_filter_choices <- function() {
  
  tryCatch({
    # Get facility and foremen lookups directly
    facility_lookup <- get_facility_lookup()
    foremen_lookup <- get_foremen_lookup()
    
    # Get facility choices from lookup
    facilities <- character(0)
    if (!is.null(facility_lookup) && nrow(facility_lookup) > 0) {
      facilities <- sort(facility_lookup$full_name)
    }
    
    # Get foreman choices from lookup
    foremen <- character(0)
    if (!is.null(foremen_lookup) && nrow(foremen_lookup) > 0) {
      foremen <- sort(foremen_lookup$shortname)
    }
    
    return(list(
      facilities = facilities,
      foremen = foremen,
      facility_codes = if (!is.null(facility_lookup)) facility_lookup$short_name else character(0),
      facility_lookup = facility_lookup,
      foremen_lookup = foremen_lookup
    ))
    
  }, error = function(e) {
    warning(paste("Error getting basic filter choices:", e$message))
    return(list(
      facilities = character(0),
      foremen = character(0),
      facility_codes = character(0),
      facility_lookup = data.frame(),
      foremen_lookup = data.frame()
    ))
  })
}

# STANDARDIZED FUNCTION: Apply data filters to loaded data
#' Standard function to filter the results from load_raw_data
#' @param data The result from load_raw_data (list with sites, treatments, total_count)
#' @param facility_filter Vector of selected facilities (or "all")
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
    if (!is.null(treatments) && nrow(treatments) > 0) {
      treatments <- treatments %>% filter(facility %in% facility_filter)
    }
  }
  
  # Apply foreman filter (fosarea)
  if (!is.null(foreman_filter) && !("all" %in% foreman_filter) && length(foreman_filter) > 0) {
    sites <- sites %>% filter(fosarea %in% foreman_filter)
    if (!is.null(treatments) && nrow(treatments) > 0) {
      treatments <- treatments %>% filter(fosarea %in% foreman_filter)
    }
  }
  
  # Apply zone filter  
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    sites <- sites %>% filter(zone %in% zone_filter)
    if (!is.null(treatments) && nrow(treatments) > 0) {
      treatments <- treatments %>% filter(zone %in% zone_filter)
    }
  }
  
  # Return STANDARDIZED format
  return(list(
    sites = sites,
    treatments = treatments,
    total_count = data$total_count  # Keep original total_count from universe
  ))
}
