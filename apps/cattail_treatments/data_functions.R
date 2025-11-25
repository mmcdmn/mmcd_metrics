# Cattail Treatments - Data Functions
# Functions for loading and processing cattail treatment data

# Source shared helpers
source("../../shared/db_helpers.R")

library(dplyr)
library(lubridate)
library(sf)

# Helper function for facility name mapping
map_facility_names <- function(data, facility_column = "facility") {
  facility_lookup <- get_facility_lookup()
  if (nrow(facility_lookup) > 0) {
    facility_map <- setNames(facility_lookup$full_name, facility_lookup$short_name)
    matched_facilities <- facility_map[data[[facility_column]]]
    data[[facility_column]] <- ifelse(!is.na(matched_facilities), matched_facilities, data[[facility_column]])
  }
  return(data)
}

# Function to load raw cattail treatment data
load_cattail_data <- function(analysis_date = NULL, include_archive = FALSE,
                              start_year = NULL, end_year = NULL) {
  
  # Handle NULL analysis_date
  if (is.null(analysis_date)) {
    analysis_date <- Sys.Date()
  } else if (is.character(analysis_date)) {
    analysis_date <- as.Date(analysis_date)
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
        0 as treated_acres,  -- Placeholder for display functions
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
      analysis_date,           # $1
      start_date,              # $2  
      include_archive          # $3
    ))
    
    dbDisconnect(con)
    
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
    
    # Add facility mapping
    cattail_sites <- map_facility_names(cattail_sites, "facility")
    
    # Add facility_display for display function compatibility
    cattail_sites$facility_display <- cattail_sites$facility
    
    # Load treatment data for current year
    current_year <- if (!is.null(end_year)) end_year else year(analysis_date)
    treatments <- load_cattail_treatments(analysis_date = analysis_date, current_year = current_year)
    
    # Add sites that have cattail treatments but no cattail inspections
    if (!is.null(treatments) && nrow(treatments) > 0) {
      # Get sites with treatments that aren't already in cattail_sites
      treatment_only_sites <- treatments %>%
        filter(!sitecode %in% cattail_sites$sitecode) %>%
        select(sitecode) %>%
        distinct()
      
      if (nrow(treatment_only_sites) > 0) {
        cat("Found", nrow(treatment_only_sites), "sites with treatments but no cattail inspections\n")
        
        # Add these sites as "not inspected but treated"
        additional_sites <- treatment_only_sites %>%
          mutate(
            sectcode = substr(sitecode, 1, 7),
            acres = 0,  # We don't have site acres for these
            air_gnd = "G",
            drone = NA_character_,
            facility = "Unknown",
            zone = NA_character_,
            fosarea = NA_character_,
            foreman = NA_character_,
            facility_display = "Unknown",
            treated_acres = 0,
            inspection_status = 'not_inspected',
            treatment_status = 'treated',  # They have treatments
            final_status = 'treated',  # Initialize final_status
            inspdate = as.Date(NA),
            numdip = NA_real_,
            acres_plan = 0,
            inspection_year = current_year,
            state = 'treated',
            facility_code = "UNK"
          )
        
        # Combine with existing sites
        cattail_sites <- bind_rows(cattail_sites, additional_sites)
      }
    }
    
    # Convert to sf object if we have coordinates (we'll add this later for mapping)
    # For now, return as regular data frame
    
    # Load lookups for return
    facility_lookup <- get_facility_lookup()
    foremen_lookup <- get_foremen_lookup()
    
    return(list(
      cattail_sites = cattail_sites,
      treatments = treatments,
      facility_lookup = facility_lookup,
      foremen_lookup = foremen_lookup,
      analysis_date = analysis_date,
      date_range = c(start_date, end_date),
      current_year = current_year
    ))
    
  }, error = function(e) {
    warning(paste("Error loading cattail data:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(NULL)
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
    # Uses DOY (day-of-year) for better seasonal matching across years
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
    
    # Execute query - no date parameters needed since using DOY filtering
    # DOY ranges:
    # - Fall/winter: day 244-365 (Sept 1 - Dec 31)
    # - Spring/summer: day 135-213 (May 15 - Aug 1)
    treatments <- dbGetQuery(con, treatments_query)
    dbDisconnect(con)
    
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
        )
      )
    
    return(treatments)
    
  }, error = function(e) {
    warning(paste("Error loading cattail treatments:", e$message))
    if (exists("con") && !is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}

# Function to aggregate cattail treatment data with 3-state metrics
aggregate_cattail_data <- function(cattail_data, analysis_date = NULL) {
  
  if (is.null(cattail_data) || nrow(cattail_data$cattail_sites) == 0) {
    warning("No cattail data to aggregate")
    return(NULL)
  }
  
  if (is.null(analysis_date)) {
    analysis_date <- Sys.Date()
  }
  
  # Filter to current year analysis 
  analysis_year <- year(analysis_date)
  
  # Filter sites to current analysis year only
  sites_data <- cattail_data$cattail_sites %>%
    filter(inspection_year == analysis_year)
    
  treatments <- cattail_data$treatments
  current_year <- analysis_year
  
  cat("Aggregating data for year", analysis_year, "(", nrow(sites_data), "sites)\n")
  
  # Match treatments to sites - focus on current year cycle
  if (!is.null(treatments) && nrow(treatments) > 0) {
    # For current year analysis, we want treatments that map to current year inspections
    # This represents the complete treatment cycle for sites inspected in current year
    
    treated_sites <- treatments %>%
      filter(inspection_year == current_year) %>%
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
    total_sites = nrow(sites_data),
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
      total_sites = n(),
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
      total_sites = n(),
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
      total_sites = n(),
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
      total_sites = n(),
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
      total_sites = n(),
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
      total_sites = n(),
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

# Function to filter cattail data based on UI inputs
filter_cattail_data <- function(raw_data, zone_filter = c("1", "2"), facility_filter = "all",
                                date_range = NULL) {
  
  if (is.null(raw_data) || is.null(raw_data$cattail_sites)) {
    return(list(cattail_sites = data.frame()))
  }
  
  sites_data <- raw_data$cattail_sites
  
  # Apply zone filter
  if (!is.null(zone_filter) && !"all" %in% zone_filter) {
    sites_data <- sites_data %>% filter(zone %in% zone_filter)
  }
  
  # Apply facility filter - use facility_code (short codes) for filtering
  if (!"all" %in% facility_filter && !is.null(facility_filter)) {
    sites_data <- sites_data %>% filter(facility_code %in% facility_filter)
  }
  
  # Apply date range filter to inspection data
  if (!is.null(date_range) && length(date_range) == 2) {
    sites_data <- sites_data %>% 
      filter(is.na(inspdate) | (inspdate >= date_range[1] & inspdate <= date_range[2]))
  }
  
  # Filter treatments to match filtered sites
  treatments_data <- raw_data$treatments
  if (!is.null(treatments_data) && nrow(sites_data) > 0) {
    # Only include treatments for sites that passed the filtering
    site_codes <- unique(sites_data$sitecode)
    treatments_data <- treatments_data %>%
      filter(sitecode %in% site_codes)
  }

  return(list(
    cattail_sites = sites_data,
    treatments = treatments_data,  # Include filtered treatments
    foremen_lookup = raw_data$foremen_lookup,
    facility_lookup = raw_data$facility_lookup,
    current_year = raw_data$current_year
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
