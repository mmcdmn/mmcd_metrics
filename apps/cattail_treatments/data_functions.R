# Cattail Treatments - Data Functions
# Functions for loading and processing cattail treatment data

library(dplyr)
library(lubridate)
library(sf)

# Function to load raw cattail treatment data
load_cattail_data <- function(analysis_date = Sys.Date(), include_archive = FALSE, 
                              start_year = NULL, end_year = NULL) {
  
  # Determine year range
  if (is.null(start_year)) start_year <- as.numeric(format(analysis_date, "%Y")) - 2
  if (is.null(end_year)) end_year <- as.numeric(format(analysis_date, "%Y"))
  
  # Set date range
  start_date <- as.Date(paste0(start_year, "-01-01"))
  end_date <- as.Date(paste0(end_year, "-12-31"))
  
  tryCatch({
    # Get database connection
    con <- get_database_connection()
    if (is.null(con)) {
      warning("Could not connect to database")
      return(NULL)
    }
    
    # Load cattail sites data
    cattail_sites_query <- "
      SELECT 
        s.sitecode,
        s.facility,
        s.zone,
        s.fosarea,
        s.sectcode,
        s.acres,
        s.priority,
        s.active,
        ST_X(s.geom) as longitude,
        ST_Y(s.geom) as latitude,
        s.geom
      FROM sites s 
      WHERE s.sitetype = 'cattail' 
        AND s.active = true
      ORDER BY s.facility, s.sectcode, s.sitecode"
    
    cattail_sites <- dbGetQuery(con, cattail_sites_query)
    
    # Load cattail treatment plans
    treatment_plans_query <- "
      SELECT 
        tp.plan_id,
        tp.sitecode,
        tp.planned_date,
        tp.treatment_type,
        tp.material_code,
        tp.planned_acres,
        tp.status,
        tp.priority,
        tp.notes,
        tp.created_date,
        tp.created_by,
        tp.modified_date,
        tp.modified_by
      FROM treatment_plans tp
      WHERE tp.treatment_category = 'cattail'
        AND tp.planned_date BETWEEN ? AND ?
      ORDER BY tp.planned_date, tp.sitecode"
    
    treatment_plans <- dbGetQuery(con, treatment_plans_query, params = list(start_date, end_date))
    
    # Load actual cattail treatments (current year)
    current_treatments_query <- "
      SELECT 
        t.sitecode,
        t.inspdate,
        t.matcode,
        t.treated_acres,
        t.effect_days,
        t.weather,
        t.wind_speed,
        t.wind_direction,
        t.temperature,
        t.humidity,
        t.notes,
        t.inspector,
        t.crew_size,
        t.equipment_type,
        t.application_rate,
        m.material_name,
        m.category,
        m.target_species
      FROM db_insptrt_current t
      LEFT JOIN mattype_list_targetdose m ON t.matcode = m.matcode
      WHERE t.cattail = true
        AND t.inspdate BETWEEN ? AND ?
      ORDER BY t.inspdate DESC, t.sitecode"
    
    current_treatments <- dbGetQuery(con, current_treatments_query, params = list(start_date, end_date))
    
    # Load historical treatments if requested
    historical_treatments <- data.frame()
    if (include_archive && start_year < as.numeric(format(Sys.Date(), "%Y"))) {
      historical_query <- "
        SELECT 
          t.sitecode,
          t.inspdate,
          t.matcode,
          t.treated_acres,
          t.effect_days,
          t.weather,
          t.wind_speed,
          t.wind_direction, 
          t.temperature,
          t.humidity,
          t.notes,
          t.inspector,
          m.material_name,
          m.category,
          m.target_species
        FROM db_insptrt_archive t
        LEFT JOIN mattype_list_targetdose m ON t.matcode = m.matcode
        WHERE t.cattail = true
          AND t.inspdate BETWEEN ? AND ?
        ORDER BY t.inspdate DESC, t.sitecode"
      
      historical_treatments <- dbGetQuery(con, historical_query, params = list(start_date, end_date))
    }
    
    # Combine current and historical treatments
    all_treatments <- bind_rows(current_treatments, historical_treatments)
    
    # Load treatment efficacy data
    efficacy_query <- "
      SELECT 
        e.sitecode,
        e.treatment_date,
        e.evaluation_date,
        e.efficacy_rating,
        e.coverage_percent,
        e.regrowth_percent,
        e.notes as efficacy_notes,
        e.evaluator
      FROM treatment_efficacy e
      WHERE e.treatment_type = 'cattail'
        AND e.evaluation_date BETWEEN ? AND ?
      ORDER BY e.evaluation_date DESC"
    
    efficacy_data <- dbGetQuery(con, efficacy_query, params = list(start_date, end_date))
    
    # Get facility lookup for display names
    facility_lookup <- get_facility_lookup()
    
    # Get foremen lookup
    foremen_lookup <- get_foremen_lookup()
    
    # Close database connection
    dbDisconnect(con)
    
    # Process and enhance data
    if (nrow(cattail_sites) > 0) {
      # Add display names for facilities
      cattail_sites$facility_display <- cattail_sites$facility
      if (nrow(facility_lookup) > 0) {
        facility_map <- setNames(facility_lookup$full_name, facility_lookup$short_name)
        cattail_sites$facility_display <- ifelse(
          cattail_sites$facility %in% names(facility_map),
          facility_map[cattail_sites$facility],
          cattail_sites$facility
        )
      }
      
      # Convert to sf object for spatial operations
      if ("longitude" %in% names(cattail_sites) && "latitude" %in% names(cattail_sites)) {
        cattail_sites <- cattail_sites %>%
          filter(!is.na(longitude) & !is.na(latitude)) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
      }
    }
    
    # Enhance treatments with site information
    if (nrow(all_treatments) > 0 && nrow(cattail_sites) > 0) {
      # Convert sf to regular dataframe for joining
      sites_df <- cattail_sites
      if ("sf" %in% class(cattail_sites)) {
        sites_df <- st_drop_geometry(cattail_sites)
      }
      
      # Join treatments with site data
      all_treatments <- all_treatments %>%
        left_join(
          sites_df %>% select(sitecode, facility, zone, fosarea, sectcode, acres, facility_display),
          by = "sitecode",
          relationship = "many-to-many"
        )
      
      # Add treatment status based on effect days
      all_treatments <- all_treatments %>%
        mutate(
          treatment_end = as.Date(inspdate) + coalesce(effect_days, 30),
          days_since_treatment = as.numeric(analysis_date - as.Date(inspdate)),
          treatment_status = case_when(
            treatment_end >= analysis_date ~ "Active",
            days_since_treatment <= 7 ~ "Recently Expired",
            days_since_treatment <= 30 ~ "Expired",
            TRUE ~ "Long Expired"
          ),
          season = case_when(
            month(inspdate) %in% c(3, 4, 5) ~ "Spring",
            month(inspdate) %in% c(6, 7, 8) ~ "Summer", 
            month(inspdate) %in% c(9, 10, 11) ~ "Fall",
            TRUE ~ "Winter"
          )
        )
    }
    
    # Enhance treatment plans with site information
    if (nrow(treatment_plans) > 0 && nrow(cattail_sites) > 0) {
      sites_df <- cattail_sites
      if ("sf" %in% class(cattail_sites)) {
        sites_df <- st_drop_geometry(cattail_sites)
      }
      
      treatment_plans <- treatment_plans %>%
        left_join(
          sites_df %>% select(sitecode, facility, zone, fosarea, sectcode, acres, facility_display),
          by = "sitecode",
          relationship = "many-to-many"
        ) %>%
        mutate(
          days_until_planned = as.numeric(as.Date(planned_date) - analysis_date),
          plan_status = case_when(
            status == "cancelled" ~ "Cancelled",
            status == "completed" ~ "Completed", 
            as.Date(planned_date) < analysis_date ~ "Overdue",
            days_until_planned <= 7 ~ "Due This Week",
            days_until_planned <= 30 ~ "Due This Month",
            TRUE ~ "Future"
          )
        )
    }
    
    return(list(
      cattail_sites = cattail_sites,
      treatments = all_treatments,
      treatment_plans = treatment_plans,
      efficacy_data = efficacy_data,
      facility_lookup = facility_lookup,
      foremen_lookup = foremen_lookup
    ))
    
  }, error = function(e) {
    warning(paste("Error loading cattail data:", e$message))
    return(NULL)
  })
}

# Function to filter cattail data based on UI inputs
filter_cattail_data <- function(raw_data, zone_filter = c("1", "2"), facility_filter = "all",
                                foreman_filter = "all", section_filter = "all", 
                                treatment_type_filter = "all", status_filter = "all",
                                date_range = NULL) {
  
  if (is.null(raw_data)) return(NULL)
  
  # Extract data components
  sites <- raw_data$cattail_sites
  treatments <- raw_data$treatments
  plans <- raw_data$treatment_plans
  
  # Apply zone filter to sites
  if (!is.null(zone_filter) && !"all" %in% zone_filter) {
    sites <- sites %>% filter(zone %in% zone_filter)
    filtered_sitecodes <- sites$sitecode
    treatments <- treatments %>% filter(sitecode %in% filtered_sitecodes)
    plans <- plans %>% filter(sitecode %in% filtered_sitecodes)
  }
  
  # Apply facility filter
  if (!"all" %in% facility_filter && !is.null(facility_filter)) {
    sites <- sites %>% filter(facility %in% facility_filter)
    treatments <- treatments %>% filter(facility %in% facility_filter)
    plans <- plans %>% filter(facility %in% facility_filter)
  }
  
  # Apply foreman filter
  if (!"all" %in% foreman_filter && !is.null(foreman_filter)) {
    foremen_lookup <- raw_data$foremen_lookup
    if (nrow(foremen_lookup) > 0) {
      selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% foreman_filter]
      sites <- sites %>% filter(fosarea %in% selected_emp_nums)
      treatments <- treatments %>% filter(fosarea %in% selected_emp_nums)
      plans <- plans %>% filter(fosarea %in% selected_emp_nums)
    }
  }
  
  # Apply section filter
  if (!"all" %in% section_filter && !is.null(section_filter)) {
    sites <- sites %>% filter(sectcode %in% section_filter)
    treatments <- treatments %>% filter(sectcode %in% section_filter)
    plans <- plans %>% filter(sectcode %in% section_filter)
  }
  
  # Apply treatment type filter
  if (!"all" %in% treatment_type_filter && !is.null(treatment_type_filter) && nrow(treatments) > 0) {
    treatments <- treatments %>% filter(category %in% treatment_type_filter)
  }
  
  # Apply status filter to treatments
  if (!"all" %in% status_filter && !is.null(status_filter) && nrow(treatments) > 0) {
    treatments <- treatments %>% filter(treatment_status %in% status_filter)
  }
  
  # Apply date range filter
  if (!is.null(date_range) && length(date_range) == 2) {
    start_date <- date_range[1]
    end_date <- date_range[2]
    
    if (nrow(treatments) > 0) {
      treatments <- treatments %>% 
        filter(as.Date(inspdate) >= start_date & as.Date(inspdate) <= end_date)
    }
    
    if (nrow(plans) > 0) {
      plans <- plans %>% 
        filter(as.Date(planned_date) >= start_date & as.Date(planned_date) <= end_date)
    }
  }
  
  return(list(
    cattail_sites = sites,
    treatments = treatments,
    treatment_plans = plans,
    efficacy_data = raw_data$efficacy_data,
    facility_lookup = raw_data$facility_lookup,
    foremen_lookup = raw_data$foremen_lookup
  ))
}

# Function to aggregate cattail data by grouping
aggregate_cattail_data <- function(filtered_data, group_by = "facility") {
  
  if (is.null(filtered_data)) {
    return(data.frame())
  }
  
  sites <- filtered_data$cattail_sites
  treatments <- filtered_data$treatments
  plans <- filtered_data$treatment_plans
  
  if (nrow(sites) == 0) {
    return(data.frame())
  }
  
  # Convert sf to regular dataframe if needed
  if ("sf" %in% class(sites)) {
    sites_df <- st_drop_geometry(sites)
  } else {
    sites_df <- sites
  }
  
  # Prepare grouping variables and display names
  if (group_by == "facility") {
    aggregated <- sites_df %>%
      group_by(facility, facility_display) %>%
      summarise(
        total_sites = n(),
        total_acres = sum(acres, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(display_name = facility_display)
    
  } else if (group_by == "foreman") {
    foremen_lookup <- filtered_data$foremen_lookup
    
    # Map foreman numbers to names
    sites_with_foreman <- sites_df
    if (nrow(foremen_lookup) > 0) {
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      sites_with_foreman$foreman_name <- ifelse(
        sites_with_foreman$fosarea %in% names(foreman_map),
        foreman_map[as.character(sites_with_foreman$fosarea)],
        paste("FOS", sites_with_foreman$fosarea)
      )
    } else {
      sites_with_foreman$foreman_name <- paste("FOS", sites_with_foreman$fosarea)
    }
    
    aggregated <- sites_with_foreman %>%
      group_by(fosarea, foreman_name) %>%
      summarise(
        total_sites = n(),
        total_acres = sum(acres, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(display_name = foreman_name)
    
  } else if (group_by == "sectcode") {
    aggregated <- sites_df %>%
      group_by(sectcode) %>%
      summarise(
        total_sites = n(), 
        total_acres = sum(acres, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(display_name = paste0(sectcode, " (Section)"))
    
  } else {  # mmcd_all
    aggregated <- sites_df %>%
      summarise(
        total_sites = n(),
        total_acres = sum(acres, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(display_name = "MMCD Total")
  }
  
  # Add treatment statistics
  if (nrow(treatments) > 0) {
    treatment_stats <- treatments %>%
      group_by(across(any_of(c("facility", "fosarea", "sectcode")))) %>%
      summarise(
        treatments_applied = n(),
        acres_treated = sum(treated_acres, na.rm = TRUE),
        active_treatments = sum(treatment_status == "Active", na.rm = TRUE),
        recent_treatments = sum(days_since_treatment <= 30, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join with aggregated data based on grouping
    if (group_by == "facility") {
      aggregated <- aggregated %>%
        left_join(treatment_stats, by = "facility")
    } else if (group_by == "foreman") {
      aggregated <- aggregated %>%
        left_join(treatment_stats, by = "fosarea") 
    } else if (group_by == "sectcode") {
      aggregated <- aggregated %>%
        left_join(treatment_stats, by = "sectcode")
    } else {
      # For mmcd_all, sum all treatment stats
      total_stats <- treatment_stats %>%
        summarise(
          treatments_applied = sum(treatments_applied, na.rm = TRUE),
          acres_treated = sum(acres_treated, na.rm = TRUE),
          active_treatments = sum(active_treatments, na.rm = TRUE),
          recent_treatments = sum(recent_treatments, na.rm = TRUE),
          .groups = "drop"
        )
      aggregated <- bind_cols(aggregated, total_stats)
    }
  } else {
    # Add zero treatment columns if no treatments
    aggregated <- aggregated %>%
      mutate(
        treatments_applied = 0,
        acres_treated = 0,
        active_treatments = 0,
        recent_treatments = 0
      )
  }
  
  # Add treatment plan statistics
  if (nrow(plans) > 0) {
    plan_stats <- plans %>%
      group_by(across(any_of(c("facility", "fosarea", "sectcode")))) %>%
      summarise(
        planned_treatments = n(),
        planned_acres = sum(planned_acres, na.rm = TRUE),
        overdue_plans = sum(plan_status == "Overdue", na.rm = TRUE),
        upcoming_plans = sum(plan_status %in% c("Due This Week", "Due This Month"), na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join with aggregated data
    if (group_by == "facility") {
      aggregated <- aggregated %>%
        left_join(plan_stats, by = "facility")
    } else if (group_by == "foreman") {
      aggregated <- aggregated %>%
        left_join(plan_stats, by = "fosarea")
    } else if (group_by == "sectcode") {
      aggregated <- aggregated %>%
        left_join(plan_stats, by = "sectcode")
    } else {
      total_plan_stats <- plan_stats %>%
        summarise(
          planned_treatments = sum(planned_treatments, na.rm = TRUE),
          planned_acres = sum(planned_acres, na.rm = TRUE),
          overdue_plans = sum(overdue_plans, na.rm = TRUE),
          upcoming_plans = sum(upcoming_plans, na.rm = TRUE),
          .groups = "drop"
        )
      aggregated <- bind_cols(aggregated, total_plan_stats)
    }
  } else {
    # Add zero plan columns if no plans
    aggregated <- aggregated %>%
      mutate(
        planned_treatments = 0,
        planned_acres = 0,
        overdue_plans = 0,
        upcoming_plans = 0
      )
  }
  
  # Fill NA values with 0
  aggregated[is.na(aggregated)] <- 0
  
  return(aggregated)
}

# Function to get unique filter values
get_filter_choices <- function(raw_data) {
  
  if (is.null(raw_data)) {
    return(list(
      facilities = character(0),
      foremen = character(0), 
      sections = character(0),
      treatment_types = character(0)
    ))
  }
  
  sites <- raw_data$cattail_sites
  treatments <- raw_data$treatments
  foremen_lookup <- raw_data$foremen_lookup
  
  # Convert sf to regular dataframe if needed
  if ("sf" %in% class(sites)) {
    sites <- st_drop_geometry(sites)
  }
  
  # Get facility choices
  facilities <- sort(unique(sites$facility))
  
  # Get foreman choices
  foremen <- character(0)
  if (nrow(foremen_lookup) > 0) {
    foremen_in_sites <- unique(sites$fosarea)
    foremen <- foremen_lookup$shortname[foremen_lookup$emp_num %in% foremen_in_sites]
    foremen <- sort(foremen)
  }
  
  # Get section choices
  sections <- sort(unique(sites$sectcode))
  
  # Get treatment type choices
  treatment_types <- character(0)
  if (nrow(treatments) > 0) {
    treatment_types <- sort(unique(treatments$category[!is.na(treatments$category)]))
  }
  
  return(list(
    facilities = facilities,
    foremen = foremen,
    sections = sections,
    treatment_types = treatment_types
  ))
}