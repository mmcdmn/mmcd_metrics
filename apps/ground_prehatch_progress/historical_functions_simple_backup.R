# Simplified historical functions for Ground Prehatch Progress app
# These functions provide the interface needed by the app server logic

library(dplyr)
library(lubridate)
library(purrr)
source("data_functions.R")

# Main function to get ground prehatch historical data
get_ground_historical_data <- function(time_period = "weekly", display_metric = "treatments", 
                                     zone_filter = c("1", "2"), 
                                     start_year = NULL, end_year = NULL) {
  
  # Determine year range
  if (is.null(start_year)) start_year <- as.numeric(format(Sys.Date(), "%Y")) - 4
  if (is.null(end_year)) end_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Set date range
  start_date <- as.Date(paste0(start_year, "-01-01"))
  end_date <- as.Date(paste0(end_year, "-12-31"))
  
  # Load raw data using standard function
  raw_data <- load_raw_data(
    analysis_date = start_date,
    include_archive = TRUE,
    start_year = start_year,
    end_year = end_year
  )
  
  if (is.null(raw_data) || nrow(raw_data$ground_treatments) == 0) {
    return(data.frame())
  }
  
  ground_sites <- raw_data$ground_sites
  ground_treatments <- raw_data$ground_treatments %>%
    filter(inspdate >= start_date & inspdate <= end_date)
  
  # Apply zone filter
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    ground_sites <- ground_sites %>% filter(zone %in% zone_filter)
    ground_treatments <- ground_treatments %>% filter(zone %in% zone_filter)
  }
  
  # Special logic for weekly active treatments (active sites and active acres)
  if (time_period == "weekly" && display_metric %in% c("active_sites", "active_acres")) {
    # Generate all weeks in the range
    all_weeks <- seq.Date(start_date, end_date, by = "week")
    week_data <- data.frame()
    
    for (week_start in all_weeks) {
      week_friday <- as.Date(week_start) + 4  # Friday of that week
      week_label <- paste0(year(week_friday), "-W", sprintf("%02d", week(week_friday)))
      
      # Find sites/acres with active treatment on that Friday
      active_treatments <- ground_treatments %>%
        mutate(
          treatment_end = as.Date(inspdate) + ifelse(is.na(effect_days), 14, effect_days)
        ) %>%
        filter(
          as.Date(inspdate) <= week_friday,
          treatment_end >= week_friday
        )
      
      if (nrow(active_treatments) > 0) {
        # Join with sites for acres data if needed
        if (display_metric == "active_acres") {
          active_data <- active_treatments %>%
            inner_join(ground_sites %>% select(sitecode, facility, zone, fosarea, acres), 
                      by = "sitecode", relationship = "many-to-many") %>%
            mutate(time_period = week_label) %>%
            # Use site acres for weekly active view
            select(sitecode, facility = facility.y, zone = zone.y, fosarea = fosarea.y, 
                  acres = acres.y, time_period)
        } else {
          # For sites metric, use the treatment data
          active_data <- active_treatments %>%
            mutate(time_period = week_label) %>%
            select(sitecode, facility, zone, fosarea, time_period)
        }
        
        week_data <- bind_rows(week_data, active_data)
      }
    }
    
    return(week_data)
  } else {
    # Original logic for yearly or treatments
    if (display_metric == "treatments") {
      # For treatments, use treatment data directly
      if (time_period == "weekly") {
        data_source <- ground_treatments %>%
          mutate(time_period = paste0(year(inspdate), "-W", sprintf("%02d", week(inspdate))))
      } else {
        data_source <- ground_treatments %>%
          mutate(time_period = as.character(year(inspdate)))
      }
    } else {
      # For sites/acres, join sites with treatments to get time periods
      if (time_period == "weekly") {
        treatment_periods <- ground_treatments %>%
          mutate(time_period = paste0(year(inspdate), "-W", sprintf("%02d", week(inspdate)))) %>%
          select(sitecode, time_period) %>%
          distinct()
      } else {
        treatment_periods <- ground_treatments %>%
          mutate(time_period = as.character(year(inspdate))) %>%
          select(sitecode, time_period) %>%
          distinct()
      }
      
      # Join sites with their treatment periods
      data_source <- ground_sites %>%
        inner_join(treatment_periods, by = "sitecode", relationship = "many-to-many")
    }
    
    return(data_source)
  }
}

# Function for yearly treatment data 
get_yearly_treatment_data <- function(start_year, end_year, zone_filter) {
  # Load raw data with archive
  raw_data <- load_raw_data(
    analysis_date = as.Date(paste0(start_year, "-01-01")),
    include_archive = TRUE,
    start_year = start_year,
    end_year = end_year
  )
  
  if (is.null(raw_data$ground_treatments) || nrow(raw_data$ground_treatments) == 0) {
    return(data.frame())
  }
  
  # Merge treatments with site data
  data <- raw_data$ground_treatments %>%
    left_join(raw_data$ground_sites, by = "sitecode", relationship = "many-to-many") %>%
    filter(!is.na(facility)) %>%
    mutate(site_acres = acres)  # Create alias for consistency with weekly data
  
  # Filter by zones
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    data <- data %>% filter(zone %in% zone_filter)
  }
  
  # Add time period column for yearly
  data <- data %>%
    mutate(time_period = year(inspdate))
  
  return(data)
}

# Function for weekly active treatment data
get_weekly_active_treatment_data <- function(start_year, end_year, zone_filter) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Get all treatments with effect_days 
    treatments_query <- sprintf("
    SELECT c.sitecode, c.inspdate, c.matcode, c.insptime,
           c.acres as treated_acres, p.effect_days, 'current' as data_source,
           sc.facility, b.acres as site_acres, sc.zone, sc.fosarea, left(b.sitecode,7) as sectcode
    FROM (SELECT * FROM dblarv_insptrt_current WHERE inspdate>='%d-01-01' AND inspdate <= '%d-12-31') c
    JOIN loc_breeding_sites b ON c.sitecode = b.sitecode
    JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
    JOIN (SELECT * FROM mattype_list_targetdose WHERE prehatch IS TRUE) p USING (matcode)
    WHERE (b.enddate IS NULL OR b.enddate>'%d-05-01')
      AND b.air_gnd='G'
      AND b.prehatch IN ('PREHATCH','BRIQUET')
    
    UNION ALL
    
    SELECT c.sitecode, c.inspdate, c.matcode, c.insptime,
           c.acres as treated_acres, p.effect_days, 'archive' as data_source,
           sc.facility, b.acres as site_acres, sc.zone, sc.fosarea, left(b.sitecode,7) as sectcode
    FROM (SELECT * FROM dblarv_insptrt_archive WHERE inspdate>='%d-01-01' AND inspdate <= '%d-12-31') c
    JOIN loc_breeding_sites b ON c.sitecode = b.sitecode
    JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
    JOIN (SELECT * FROM mattype_list_targetdose WHERE prehatch IS TRUE) p USING (matcode)
    WHERE (b.enddate IS NULL OR b.enddate>'%d-05-01')
      AND b.air_gnd='G'
      AND b.prehatch IN ('PREHATCH','BRIQUET')
    
    ORDER BY sitecode, inspdate DESC
    ", start_year, end_year, start_year, start_year, end_year, start_year)
    
    all_treatments <- dbGetQuery(con, treatments_query)
    dbDisconnect(con)
    
    if (nrow(all_treatments) == 0) return(data.frame())
    
    # Map facility names
    all_treatments <- map_facility_names(all_treatments)
    
    # Filter by zones
    if (!is.null(zone_filter) && length(zone_filter) > 0) {
      all_treatments <- all_treatments %>% filter(zone %in% zone_filter)
    }
    
    # Convert dates and add effect_days default
    all_treatments <- all_treatments %>%
      mutate(
        inspdate = as.Date(inspdate),
        effect_days = ifelse(is.na(effect_days), 30, effect_days)  # Default 30 days for prehatch
      )
    
    # Generate all Fridays for the year range
    start_date <- as.Date(paste0(start_year, "-01-01"))
    end_date <- as.Date(paste0(end_year, "-12-31"))
    
    # Find first Friday of start_year
    first_friday <- start_date
    while (weekdays(first_friday) != "Friday") {
      first_friday <- first_friday + 1
    }
    
    # Generate all Fridays
    fridays <- seq(from = first_friday, to = end_date, by = 7)
    fridays <- fridays[fridays <= end_date]
    
    # For each Friday, determine which sites have active treatment
    weekly_data <- map_dfr(fridays, function(friday_date) {
      # For each site, find the most recent treatment before or on this Friday
      site_status <- all_treatments %>%
        filter(inspdate <= friday_date) %>%
        group_by(sitecode) %>%
        arrange(desc(inspdate), desc(insptime)) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(
          treatment_end_date = inspdate + effect_days,
          is_active = treatment_end_date >= friday_date,
          week_date = friday_date,
          year = year(friday_date),
          week = week(friday_date),
          time_period = paste0(year, "-W", sprintf("%02d", week))
        ) %>%
        filter(is_active)  # Only include sites with active treatment
      
      return(site_status)
    })
    
    return(weekly_data)
    
  }, error = function(e) {
    warning(paste("Error in get_weekly_active_treatment_data:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}

# Function to filter historical data
filter_historical_data <- function(data, zone_filter, facility_filter, foreman_filter) {
  if (nrow(data) == 0) return(data)
  
  # Filter by facility
  if (!"all" %in% facility_filter && !is.null(facility_filter)) {
    data <- data %>% filter(facility %in% facility_filter)
  }
  
  # Filter by foreman
  if (!"all" %in% foreman_filter && !is.null(foreman_filter)) {
    # Convert foreman names to emp_nums
    foremen_lookup <- get_foremen_lookup()
    if (nrow(foremen_lookup) > 0) {
      selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% foreman_filter]
      data <- data %>% filter(fosarea %in% selected_emp_nums)
    }
  }
  
  return(data)
}

# Function to aggregate historical data by group
aggregate_historical_data_by_group <- function(data, group_by, time_period, display_metric, combine_zones = FALSE) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Create metric calculation based on time period and metric type
  if (time_period == "weekly") {
    # For weekly analysis, data already contains only sites with active treatment
    if (display_metric == "weekly_active_sites") {
      # Count unique active sites per week
      metric_calculation <- function(data) n_distinct(data$sitecode)
    } else if (display_metric == "weekly_active_acres") {
      # Sum site acres for unique active sites per week
      metric_calculation <- function(data) sum(data$site_acres[!duplicated(data$sitecode)], na.rm = TRUE)
    } else {
      metric_calculation <- function(data) 0
    }
  } else {
    # For yearly analysis, use existing logic
    if (display_metric == "treatments") {
      metric_calculation <- function(data) nrow(data)
    } else if (display_metric == "sites") {
      metric_calculation <- function(data) n_distinct(data$sitecode)
    } else if (display_metric == "acres") {
      metric_calculation <- function(data) sum(data$treated_acres, na.rm = TRUE)
    } else if (display_metric == "site_acres") {
      metric_calculation <- function(data) sum(data$site_acres[!duplicated(data$sitecode)], na.rm = TRUE)
    } else {
      metric_calculation <- function(data) 0
    }
  }
  
  # Group the data based on group_by parameter
  if (group_by == "mmcd_all") {
    # Check if we need to separate zones
    if (combine_zones) {
      # Combine all zones
      aggregated <- data %>%
        group_by(time_period) %>%
        summarise(
          count = case_when(
            display_metric == "treatments" ~ n(),
            display_metric == "sites" ~ n_distinct(sitecode),
            display_metric == "acres" ~ sum(treated_acres, na.rm = TRUE),
            display_metric == "site_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
            display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
            display_metric == "weekly_active_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
            TRUE ~ 0
          ),
          .groups = "drop"
        ) %>%
        mutate(display_name = "MMCD Total")
    } else {
      # Separate by zone
      aggregated <- data %>%
        group_by(time_period, zone) %>%
        summarise(
          count = case_when(
            display_metric == "treatments" ~ n(),
            display_metric == "sites" ~ n_distinct(sitecode),
            display_metric == "acres" ~ sum(treated_acres, na.rm = TRUE),
            display_metric == "site_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
            display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
            display_metric == "weekly_active_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
            TRUE ~ 0
          ),
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0("Zone P", zone))
    }
  } else if (group_by == "facility") {
    if (combine_zones) {
      aggregated <- data %>%
        group_by(time_period, facility) %>%
        summarise(
          count = case_when(
            display_metric == "treatments" ~ n(),
            display_metric == "sites" ~ n_distinct(sitecode),
            display_metric == "acres" ~ sum(treated_acres, na.rm = TRUE),
            display_metric == "site_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
            display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
            display_metric == "weekly_active_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
            TRUE ~ 0
          ),
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0(facility, " (Facility)"))
    } else {
      aggregated <- data %>%
        group_by(time_period, facility, zone) %>%
        summarise(
          count = case_when(
            display_metric == "treatments" ~ n(),
            display_metric == "sites" ~ n_distinct(sitecode),
            display_metric == "acres" ~ sum(treated_acres, na.rm = TRUE),
            display_metric == "site_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
            display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
            display_metric == "weekly_active_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
            TRUE ~ 0
          ),
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0(facility, " P", zone, " (Facility)"))
    }
  } else if (group_by == "foreman") {
    # Map foreman numbers to names
    foremen_lookup <- get_foremen_lookup()
    if (nrow(foremen_lookup) > 0) {
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      
      if (combine_zones) {
        aggregated <- data %>%
          mutate(foreman_name = ifelse(
            fosarea %in% names(foreman_map),
            foreman_map[as.character(fosarea)],
            paste("FOS", fosarea)
          )) %>%
          group_by(time_period, foreman_name) %>%
          summarise(
            count = case_when(
              display_metric == "treatments" ~ n(),
              display_metric == "sites" ~ n_distinct(sitecode),
              display_metric == "acres" ~ sum(treated_acres, na.rm = TRUE),
              display_metric == "site_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
              display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
              display_metric == "weekly_active_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
              TRUE ~ 0
            ),
            .groups = "drop"
          ) %>%
          mutate(display_name = paste0(foreman_name, " (FOS)"))
      } else {
        aggregated <- data %>%
          mutate(foreman_name = ifelse(
            fosarea %in% names(foreman_map),
            foreman_map[as.character(fosarea)],
            paste("FOS", fosarea)
          )) %>%
          group_by(time_period, foreman_name, zone) %>%
          summarise(
            count = case_when(
              display_metric == "treatments" ~ n(),
              display_metric == "sites" ~ n_distinct(sitecode),
              display_metric == "acres" ~ sum(treated_acres, na.rm = TRUE),
              display_metric == "site_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
              display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
              display_metric == "weekly_active_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
              TRUE ~ 0
            ),
            .groups = "drop"
          ) %>%
          mutate(display_name = paste0(foreman_name, " P", zone))
      }
    } else {
      # Fallback if no lookup available
      if (combine_zones) {
        aggregated <- data %>%
          group_by(time_period, fosarea) %>%
          summarise(
            count = case_when(
              display_metric == "treatments" ~ n(),
              display_metric == "sites" ~ n_distinct(sitecode),
              display_metric == "acres" ~ sum(treated_acres, na.rm = TRUE),
              display_metric == "site_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
              display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
              display_metric == "weekly_active_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
              TRUE ~ 0
            ),
            .groups = "drop"
          ) %>%
          mutate(display_name = paste0("FOS ", fosarea))
      } else {
        aggregated <- data %>%
          group_by(time_period, fosarea, zone) %>%
          summarise(
            count = case_when(
              display_metric == "treatments" ~ n(),
              display_metric == "sites" ~ n_distinct(sitecode),
              display_metric == "acres" ~ sum(treated_acres, na.rm = TRUE),
              display_metric == "site_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
              display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
              display_metric == "weekly_active_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
              TRUE ~ 0
            ),
            .groups = "drop"
          ) %>%
          mutate(display_name = paste0("FOS ", fosarea, " P", zone))
      }
    }
  } else if (group_by == "sectcode") {
    if (combine_zones) {
      aggregated <- data %>%
        group_by(time_period, sectcode) %>%
        summarise(
          count = case_when(
            display_metric == "treatments" ~ n(),
            display_metric == "sites" ~ n_distinct(sitecode),
            display_metric == "acres" ~ sum(treated_acres, na.rm = TRUE),
            display_metric == "site_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
            display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
            display_metric == "weekly_active_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
            TRUE ~ 0
          ),
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0(sectcode, " (Section)"))
    } else {
      aggregated <- data %>%
        group_by(time_period, sectcode, zone) %>%
        summarise(
          count = case_when(
            display_metric == "treatments" ~ n(),
            display_metric == "sites" ~ n_distinct(sitecode),
            display_metric == "acres" ~ sum(treated_acres, na.rm = TRUE),
            display_metric == "site_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
            display_metric == "weekly_active_sites" ~ n_distinct(sitecode),
            display_metric == "weekly_active_acres" ~ sum(site_acres[!duplicated(sitecode)], na.rm = TRUE),
            TRUE ~ 0
          ),
          .groups = "drop"
        ) %>%
        mutate(display_name = paste0(sectcode, " P", zone, " (Section)"))
    }
  } else {
    return(data.frame())
  }
  
  return(aggregated)
}

# Function to create historical details table
create_historical_details_table <- function(data) {
  if (nrow(data) == 0) {
    return(DT::datatable(data.frame(Message = "No historical data available")))
  }
  
  # Get foremen lookup for mapping
  foremen_lookup <- get_foremen_lookup()
  
  # Prepare data for table display
  table_data <- data %>%
    mutate(
      foreman_name = ifelse(
        fosarea %in% foremen_lookup$emp_num,
        foremen_lookup$shortname[match(fosarea, foremen_lookup$emp_num)],
        paste("FOS", fosarea)
      )
    ) %>%
    select(inspdate, sitecode, facility, foreman_name, sectcode, zone, matcode) %>%
    arrange(desc(inspdate), facility, sitecode)
  
  # Rename columns for display
  colnames(table_data) <- c("Treatment Date", "Site Code", "Facility", "Foreman", "Section", "Zone", "Material")
  
  # Create the data table
  DT::datatable(table_data,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ),
    rownames = FALSE
  ) %>%
    DT::formatStyle(columns = 1:ncol(table_data), fontSize = '12px') %>%
    DT::formatDate("Treatment Date", method = "toDateString")
}