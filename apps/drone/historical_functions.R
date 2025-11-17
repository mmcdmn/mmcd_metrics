# Historical data functions - now using consolidated load_raw_data

# Simplified function to create historical data for charts
create_historical_data <- function(start_year, end_year, hist_time_period, hist_display_metric, hist_group_by, hist_zone_display, facility_filter = NULL, zone_filter = NULL, foreman_filter = NULL, prehatch_only = FALSE) {
  
  library(lubridate)
  source('data_functions.R')
  
  # Set date range
  start_date <- as.Date(paste0(start_year, "-01-01"))
  end_date <- as.Date(paste0(end_year, "-12-31"))
  
  # Load raw data with archive
  raw_data <- load_raw_data(analysis_date = start_date, include_archive = TRUE, 
                           start_year = start_year, end_year = end_year)
  
  if (is.null(raw_data) || is.null(raw_data$drone_treatments) || nrow(raw_data$drone_treatments) == 0) {
    return(data.frame())
  }
  
  drone_sites <- raw_data$drone_sites
  drone_treatments <- raw_data$drone_treatments %>%
    filter(inspdate >= start_date & inspdate <= end_date)
  
  # Apply filters to both sites and treatments
  if (!is.null(facility_filter) && length(facility_filter) > 0 && !"all" %in% facility_filter) {
    drone_sites <- drone_sites %>% filter(facility %in% facility_filter)
    drone_treatments <- drone_treatments %>% filter(facility %in% facility_filter)
  }
  
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    drone_sites <- drone_sites %>% filter(zone %in% zone_filter)
    drone_treatments <- drone_treatments %>% filter(zone %in% zone_filter)
  }
  
  if (!is.null(foreman_filter) && length(foreman_filter) > 0 && !"all" %in% foreman_filter) {
    # Convert foreman names to emp_nums
    foremen_lookup <- get_foremen_lookup()
    selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% foreman_filter]
    
    drone_sites <- drone_sites %>% filter(foreman %in% selected_emp_nums)
    drone_treatments <- drone_treatments %>% filter(foreman %in% selected_emp_nums)
  }
  
  # Apply prehatch filter
  if (!is.null(prehatch_only) && prehatch_only) {
    drone_sites <- drone_sites %>% filter(prehatch == 'PREHATCH')
    drone_treatments <- drone_treatments %>% filter(prehatch == 'PREHATCH')
  }
  
  # Determine if zones should be shown separately
  show_zones_separately <- hist_zone_display == "show-both" && length(zone_filter) > 1
  
  # Special logic for weekly active treatments
  if (hist_time_period == "weekly" && hist_display_metric != "treatments") {
    # Generate all weeks in the range
    all_weeks <- seq.Date(start_date, end_date, by = "week")
    week_data <- data.frame()
    
    for (week_start in all_weeks) {
      week_friday <- as.Date(week_start) + 4  # Friday of that week
      week_label <- paste0(year(week_friday), "-W", sprintf("%02d", week(week_friday)))
      
      # Find sites/acres with active treatment on that Friday
      active_treatments <- drone_treatments %>%
        mutate(
          treatment_end = as.Date(inspdate) + ifelse(is.na(effect_days), 14, effect_days)
        ) %>%
        filter(
          as.Date(inspdate) <= week_friday,
          treatment_end >= week_friday
        )
      
      if (nrow(active_treatments) > 0) {
        # Join with sites for acres data if needed, but keep all necessary columns
        if (hist_display_metric == "acres") {
          active_data <- active_treatments %>%
            inner_join(drone_sites %>% select(sitecode, facility, zone, foreman, acres), 
                      by = "sitecode", relationship = "many-to-many") %>%
            mutate(time_period = week_label) %>%
            # Use site acres instead of treated acres for weekly active view
            select(sitecode, facility = facility.y, zone = zone.y, foreman = foreman.y, 
                  acres = acres.y, time_period)
        } else {
          # For sites metric, use the treatment data but ensure we have facility/zone
          active_data <- active_treatments %>%
            mutate(time_period = week_label) %>%
            select(sitecode, facility, zone, foreman, time_period)
        }
        
        week_data <- bind_rows(week_data, active_data)
      }
    }
    
    data_source <- week_data
  } else {
    # Original logic for yearly or treatments
    # Prepare base data for each time period
    if (hist_display_metric == "treatments") {
      # For treatments, use treatment data directly
      if (hist_time_period == "weekly") {
        data_source <- drone_treatments %>%
          mutate(time_period = paste0(year(inspdate), "-W", sprintf("%02d", week(inspdate))))
      } else {
        data_source <- drone_treatments %>%
          mutate(time_period = as.character(year(inspdate)))
      }
    } else {
      # For sites/acres, join sites with treatments to get time periods
      if (hist_time_period == "weekly") {
        treatment_periods <- drone_treatments %>%
          mutate(time_period = paste0(year(inspdate), "-W", sprintf("%02d", week(inspdate)))) %>%
          select(sitecode, time_period) %>%
          distinct()
      } else {
        treatment_periods <- drone_treatments %>%
          mutate(time_period = as.character(year(inspdate))) %>%
          select(sitecode, time_period) %>%
          distinct()
      }
      
      # Join sites with their treatment periods
      data_source <- drone_sites %>%
        inner_join(treatment_periods, by = "sitecode", relationship = "many-to-many")
    }
  }
  
  # Ensure data_source has required columns for grouping
  if (nrow(data_source) == 0) {
    return(data.frame())
  }
  
  # Handle zone separation and grouping
  if (show_zones_separately) {
    # Create combined group labels with zones
    if (hist_group_by == "facility" && "facility" %in% names(data_source)) {
      # Map facility names first, then add zones
      facilities <- get_facility_lookup()
      facility_map <- setNames(facilities$full_name, facilities$short_name)
      
      data_source <- data_source %>%
        mutate(
          facility_display = ifelse(
            facility %in% names(facility_map),
            facility_map[facility],
            facility
          ),
          group_label = paste0(facility_display, " (P", zone, ")")
        )
      grouped_data <- data_source %>%
        group_by(group_label, time_period)
    } else if (hist_group_by == "foreman" && "foreman" %in% names(data_source)) {
      # Map foreman numbers to names first, then add zone
      foremen_lookup <- get_foremen_lookup()
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      
      data_source <- data_source %>%
        mutate(
          foreman_name = ifelse(
            foreman %in% names(foreman_map),
            foreman_map[as.character(foreman)],
            paste("FOS", foreman)
          ),
          group_label = paste0(foreman_name, " (P", zone, ")")
        )
      grouped_data <- data_source %>%
        group_by(group_label, time_period)
    } else if (hist_group_by == "mmcd_all") {
      data_source <- data_source %>%
        mutate(group_label = paste0("All MMCD (P", zone, ")"))
      grouped_data <- data_source %>%
        group_by(group_label, time_period)
    } else {
      # Fallback: if we can't group properly, return empty
      return(data.frame())
    }
  } else {
    # Standard grouping without zone separation
    if (hist_group_by == "facility" && "facility" %in% names(data_source)) {
      # Map facility names for display
      facilities <- get_facility_lookup()
      facility_map <- setNames(facilities$full_name, facilities$short_name)
      
      data_source <- data_source %>%
        mutate(
          group_label = ifelse(
            facility %in% names(facility_map),
            facility_map[facility],
            facility
          )
        )
      grouped_data <- data_source %>%
        group_by(group_label, time_period)
    } else if (hist_group_by == "foreman" && "foreman" %in% names(data_source)) {
      # Map foreman numbers to names for display
      foremen_lookup <- get_foremen_lookup()
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      
      data_source <- data_source %>%
        mutate(
          group_label = ifelse(
            foreman %in% names(foreman_map),
            foreman_map[as.character(foreman)],
            paste("FOS", foreman)
          )
        )
      grouped_data <- data_source %>%
        group_by(group_label, time_period)
    } else if (hist_group_by == "mmcd_all") {
      data_source$group_label <- "All MMCD"
      grouped_data <- data_source %>%
        group_by(group_label, time_period)
    } else {
      # Fallback: if we can't group properly, return empty
      return(data.frame())
    }
  }
  
  # Summarize based on metric
  if (hist_display_metric == "treatments") {
    summary_data <- grouped_data %>%
      summarize(value = n(), .groups = "drop")
  } else if (hist_display_metric == "sites") {
    summary_data <- grouped_data %>%
      summarize(value = n_distinct(sitecode), .groups = "drop")
  } else if (hist_display_metric == "acres") {
    summary_data <- grouped_data %>%
      summarize(value = sum(acres, na.rm = TRUE), .groups = "drop")
  }
  
  # Summary data already has group_label set from the grouping logic above
  return(summary_data)
}

# Process historical data - uses consolidated data loading with archive support
get_historical_processed_data <- function(hist_start_year, hist_end_year, drone_types, zone_filter, facility_filter, foreman_filter, prehatch_only, group_by, hist_time_period = "yearly", hist_display_metric, combine_zones = FALSE, analysis_date = Sys.Date()) {
  # Load data with archive included for historical analysis
  library(lubridate)
  source('data_functions.R')
  
  start_year <- as.integer(hist_start_year[1])
  end_year <- as.integer(hist_end_year[1])
  
  # Use consolidated load_raw_data with archive support
  raw_data <- load_raw_data(include_archive = TRUE, start_year = start_year, end_year = end_year)
  
  if (is.null(raw_data) || nrow(raw_data$drone_treatments) == 0) {
    return(data.frame())
  }
  
  drone_treatments <- raw_data$drone_treatments
  drone_sites <- raw_data$drone_sites
  
  # Filter drone_sites to only include sites that were treated in the time period
  treated_sitecodes <- unique(drone_treatments$sitecode)
  drone_sites <- drone_sites %>%
    filter(sitecode %in% treated_sitecodes)
  
  if (nrow(drone_treatments) == 0 || nrow(drone_sites) == 0) {
    return(data.frame())
  }
  
  # Apply filters to BOTH treatments and sites (to match current progress logic)
  # Apply facility filter  
  if (!is.null(facility_filter) && length(facility_filter) > 0 && !("all" %in% facility_filter)) {
    drone_treatments <- drone_treatments %>% filter(facility %in% facility_filter)
    drone_sites <- drone_sites %>% filter(facility %in% facility_filter)
  }
  
  # Apply foreman/FOS filter
  if (!is.null(foreman_filter) && length(foreman_filter) > 0 && !("all" %in% foreman_filter)) {
    foremen_lookup <- get_foremen_lookup()
    selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% foreman_filter]
    
    drone_treatments <- drone_treatments %>% filter(foreman %in% selected_emp_nums)
    drone_sites <- drone_sites %>% filter(foreman %in% selected_emp_nums)
  }
  
  # Apply prehatch filter
  if (prehatch_only) {
    drone_treatments <- drone_treatments %>% filter(prehatch == 'PREHATCH')
    drone_sites <- drone_sites %>% filter(prehatch == 'PREHATCH')
  }
  
  # Apply zone filter
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    drone_treatments <- drone_treatments %>% filter(zone %in% zone_filter)
    drone_sites <- drone_sites %>% filter(zone %in% zone_filter)
  }
  
  if (nrow(drone_treatments) == 0 || nrow(drone_sites) == 0) {
    return(data.frame())
  }
  
  # Handle grouping and zone separation
  show_zones_separately <- !combine_zones && length(zone_filter) > 1
  
  # Handle MMCD grouping
  if (group_by == "mmcd_all") {
    if (nrow(drone_sites) > 0) drone_sites$mmcd_all <- "All MMCD"
    if (nrow(drone_treatments) > 0) drone_treatments$mmcd_all <- "All MMCD"
    group_col <- "mmcd_all"
  } else {
    group_col <- group_by
  }
  
  # Use SITES for acres calculations, treatments for counts/dates
  if (hist_display_metric == "sites") {
    data_source <- drone_sites
    metric_col <- "sitecode"
    use_distinct <- TRUE
  } else if (hist_display_metric == "treatments") {
    data_source <- drone_treatments
    metric_col <- NULL
    use_distinct <- FALSE
  } else { # acres
    # Sum site acres (using sites table - SAME as current progress!)
    data_source <- drone_sites
    metric_col <- "acres"
    use_distinct <- FALSE
  }
  
  # Add time period column based on time period selection and available treatment data
  if (hist_time_period == "weekly") {
    # For weekly, use treatment dates to create weekly periods
    if (hist_display_metric == "treatments") {
      data_source <- drone_treatments %>%
        mutate(time_period = paste0(year(inspdate), "-W", sprintf("%02d", week(inspdate))))
    } else {
      # For sites/acres, use the treatments to determine which weeks had activity
      weekly_treatments <- drone_treatments %>%
        mutate(time_period = paste0(year(inspdate), "-W", sprintf("%02d", week(inspdate))))
      
      # Join sites with weekly treatment data
      data_source <- drone_sites %>%
        inner_join(weekly_treatments %>% select(sitecode, time_period) %>% distinct(), 
                   by = "sitecode", relationship = "many-to-many")
    }
  } else {
    # For yearly periods
    if (hist_display_metric == "treatments") {
      data_source <- drone_treatments %>%
        mutate(time_period = year(inspdate))
    } else {
      # For sites/acres, use treatment years to determine which years had activity
      yearly_treatments <- drone_treatments %>%
        mutate(time_period = year(inspdate))
      
      # Join sites with yearly treatment data
      data_source <- drone_sites %>%
        inner_join(yearly_treatments %>% select(sitecode, time_period) %>% distinct(), 
                   by = "sitecode", relationship = "many-to-many")
    }
  }
  
  # Handle zone separation logic AFTER data_source is finalized
  if (show_zones_separately) {
    if (group_by == "mmcd_all") {
      data_source <- data_source %>%
        mutate(combined_group = paste0(mmcd_all, " (P", zone, ")"))
    } else {
      data_source <- data_source %>%
        mutate(combined_group = paste0(!!sym(group_col), " (P", zone, ")"))
    }
    group_var <- sym("combined_group")
  } else {
    # Ensure the group column exists
    if (!group_col %in% names(data_source)) {
      warning(paste("Group column", group_col, "not found in data. Available columns:", paste(names(data_source), collapse = ", ")))
      return(data.frame())
    }
    group_var <- sym(group_col)
  }
  
  # Aggregate based on metric type
  if (hist_display_metric == "sites") {
    result <- data_source %>%
      group_by(!!group_var, time_period) %>%
      summarise(count = n_distinct(!!sym(metric_col)), .groups = 'drop')
  } else if (hist_display_metric == "treatments") {
    result <- data_source %>%
      group_by(!!group_var, time_period) %>%
      summarise(count = n(), .groups = 'drop')
  } else { # acres
    result <- data_source %>%
      group_by(!!group_var, time_period) %>%
      summarise(count = sum(!!sym(metric_col), na.rm = TRUE), .groups = 'drop')
  }
  
  # Add display_name column based on the grouping variable used
  if (show_zones_separately) {
    # For zone-separated data, the combined_group column contains the display names
    # The group_var is sym("combined_group"), so we rename combined_group to display_name
    result <- result %>%
      rename(display_name = combined_group)
  } else {
    # For non-zone separated data, map names appropriately
    group_var_name <- as.character(group_var)[length(as.character(group_var))]
    
    if (group_by == "facility") {
      facilities <- get_facility_lookup()
      facility_map <- setNames(facilities$full_name, facilities$short_name)
      result <- result %>%
        mutate(display_name = ifelse(
          !!sym(group_var_name) %in% names(facility_map),
          facility_map[as.character(!!sym(group_var_name))],
          as.character(!!sym(group_var_name))
        )) %>%
        select(-!!sym(group_var_name))
    } else if (group_by == "foreman") {
      foremen_lookup <- get_foremen_lookup()
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      result <- result %>%
        mutate(display_name = ifelse(
          as.character(!!sym(group_var_name)) %in% names(foreman_map),
          foreman_map[as.character(!!sym(group_var_name))],
          paste("FOS", !!sym(group_var_name))
        )) %>%
        select(-!!sym(group_var_name))
    } else {
      result <- result %>%
        rename(display_name = !!sym(group_var_name))
    }
  }
  
  return(result)
}

# Legacy function for compatibility - now uses consolidated approach
get_shared_historical_data <- function(hist_start_year, hist_end_year, drone_types) {
  start_year <- as.integer(hist_start_year[1])
  end_year <- as.integer(hist_end_year[1])
  
  # Use consolidated load_raw_data with archive support
  raw_data <- load_raw_data(include_archive = TRUE, start_year = start_year, end_year = end_year)
  
  if (is.null(raw_data)) {
    return(list(
      drone_sites = data.frame(),
      current_treatments = data.frame(),
      archive_treatments = data.frame()
    ))
  }
  
  return(list(
    drone_sites = raw_data$drone_sites,
    current_treatments = raw_data$drone_treatments,  # Combined current + archive
    archive_treatments = data.frame()  # Already combined in drone_treatments
  ))
}