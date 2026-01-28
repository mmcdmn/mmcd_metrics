# Catch Basin Status - Historical Functions
# Note: data_functions.R and display_functions.R are sourced by app.R before this file

library(dplyr)
library(lubridate)
library(plotly)
library(DT)

# Load historical catch basin treatment data
# Combines current year (dblarv_insptrt_current) and archive (dblarv_insptrt_archive)
load_historical_cb_data <- function(start_year, end_year, 
                                    facility_filter = NULL, 
                                    zone_filter = NULL, 
                                    foreman_filter = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  tryCatch({
    # Build filters using shared SQL helpers
    facility_where <- ""
    if (is_valid_filter(facility_filter)) {
      # Handle both facility codes ('E') and full names ('East')
      # Convert full names to codes if needed
      facility_lookup <- get_facility_lookup()
      if (!is.null(facility_lookup) && nrow(facility_lookup) > 0) {
        name_to_code_map <- setNames(facility_lookup$short_name, facility_lookup$full_name)
        
        # Convert any full names to codes
        converted_filters <- sapply(facility_filter, function(f) {
          if (f %in% names(name_to_code_map)) {
            return(name_to_code_map[[f]])  # Convert full name to code
          } else {
            return(f)  # Already a code
          }
        })
        
        facility_where <- build_sql_in_clause("loc_catchbasin.facility", converted_filters)
        cat("DEBUG: Facility filter converted:", paste(facility_filter, "->", converted_filters, collapse = ", "), "\n")
      } else {
        # Fallback: assume they're already codes
        facility_where <- build_sql_in_clause("loc_catchbasin.facility", facility_filter)
      }
    }
    
    zone_where <- ""
    if (!is.null(zone_filter) && length(zone_filter) > 0) {
      if (length(zone_filter) == 1) {
        zone_where <- build_sql_equals_clause("sc.zone", zone_filter)
      } else {
        zone_where <- build_sql_in_clause("sc.zone", zone_filter)
      }
    }
    
    foreman_where <- build_sql_in_clause("sc.fosarea", foreman_filter)
    # Use shared function to determine which years are in which table
    # This handles March transitions when current data moves to archive
    year_ranges <- get_historical_year_ranges(con, "dblarv_insptrt_current", "dblarv_insptrt_archive", "inspdate")
    current_years <- year_ranges$current_years
    archive_years <- year_ranges$archive_years
    
    data <- data.frame()
    
    # Get data from CURRENT table for recent years (2025+)
    current_year_range <- start_year:end_year
    if (length(current_year_range) > 0) {
      current_query <- sprintf("
        SELECT 
          loc_catchbasin.facility,
          sc.zone,
          sc.fosarea,
          left(loc_catchbasin.sitecode, 7) as sectcode,
          EXTRACT(YEAR FROM dblarv_insptrt_current.inspdate) as treatment_year,
          COUNT(*) as total_count,
          COUNT(*) FILTER (WHERE dblarv_treatment_catchbasin.status IN ('A', 'W')) as active_count,
          COUNT(*) FILTER (WHERE dblarv_treatment_catchbasin.status IN ('A', 'W') 
                           AND (CURRENT_DATE - dblarv_insptrt_current.inspdate) >= 21 
                           AND (CURRENT_DATE - dblarv_insptrt_current.inspdate) < 28) as expiring_count,
          COUNT(*) FILTER (WHERE dblarv_treatment_catchbasin.status IN ('A', 'W') 
                           AND (CURRENT_DATE - dblarv_insptrt_current.inspdate) >= 28) as expired_count
        FROM dblarv_insptrt_current
        JOIN dblarv_treatment_catchbasin ON dblarv_insptrt_current.pkey_pg = dblarv_treatment_catchbasin.treatment_id
        JOIN loc_catchbasin ON dblarv_treatment_catchbasin.catchbasin_id = loc_catchbasin.gid
        JOIN mattype_list_targetdose USING (matcode)
        LEFT JOIN gis_sectcode sc ON left(loc_catchbasin.sitecode, 7) = sc.sectcode
        WHERE EXTRACT(YEAR FROM dblarv_insptrt_current.inspdate) BETWEEN %d AND %d
          AND loc_catchbasin.status_udw = 'W'
          AND loc_catchbasin.lettergrp <> 'Z'
          %s
          %s
          %s
        GROUP BY loc_catchbasin.facility, sc.zone, sc.fosarea, left(loc_catchbasin.sitecode, 7), 
                 EXTRACT(YEAR FROM dblarv_insptrt_current.inspdate)
      ", min(current_year_range), max(current_year_range), facility_where, zone_where, foreman_where)
      
      cat("DEBUG: Getting current table data for years", min(current_year_range), "-", max(current_year_range), "\n")
      current_data <- dbGetQuery(con, current_query)
      cat("DEBUG: Current table returned", nrow(current_data), "rows\n")
      data <- bind_rows(data, current_data)
    }
    
    # Get data from ARCHIVE table for historical years (2006-2024)  
    archive_year_range <- start_year:end_year
    if (length(archive_year_range) > 0) {
      archive_query <- sprintf("
        SELECT 
          loc_catchbasin.facility,
          sc.zone,
          sc.fosarea,
          left(loc_catchbasin.sitecode, 7) as sectcode,
          EXTRACT(YEAR FROM dblarv_insptrt_archive.inspdate) as treatment_year,
          COUNT(*) as total_count,
          COUNT(*) FILTER (WHERE dblarv_treatment_cb_archive.status IN ('A', 'W')) as active_count,
          COUNT(*) FILTER (WHERE dblarv_treatment_cb_archive.status IN ('A', 'W') 
                           AND (CURRENT_DATE - dblarv_insptrt_archive.inspdate) >= 21 
                           AND (CURRENT_DATE - dblarv_insptrt_archive.inspdate) < 28) as expiring_count,
          COUNT(*) FILTER (WHERE dblarv_treatment_cb_archive.status IN ('A', 'W') 
                           AND (CURRENT_DATE - dblarv_insptrt_archive.inspdate) >= 28) as expired_count
        FROM dblarv_insptrt_archive
        JOIN dblarv_treatment_cb_archive ON dblarv_insptrt_archive.pkey_pg = dblarv_treatment_cb_archive.treatment_id
        JOIN loc_catchbasin ON dblarv_treatment_cb_archive.catchbasin_id = loc_catchbasin.gid
        JOIN mattype_list_targetdose USING (matcode)
        LEFT JOIN gis_sectcode sc ON left(loc_catchbasin.sitecode, 7) = sc.sectcode
        WHERE EXTRACT(YEAR FROM dblarv_insptrt_archive.inspdate) BETWEEN %d AND %d
          AND loc_catchbasin.status_udw = 'W'
          AND loc_catchbasin.lettergrp <> 'Z'
          %s
          %s
          %s
        GROUP BY loc_catchbasin.facility, sc.zone, sc.fosarea, left(loc_catchbasin.sitecode, 7), 
                 EXTRACT(YEAR FROM dblarv_insptrt_archive.inspdate)
      ", min(archive_year_range), max(archive_year_range), facility_where, zone_where, foreman_where)
      
      cat("DEBUG: Getting archive table data for years", min(archive_year_range), "-", max(archive_year_range), "\n")
      archive_data <- dbGetQuery(con, archive_query)
      cat("DEBUG: Archive table returned", nrow(archive_data), "rows\n")
      data <- bind_rows(data, archive_data)
    }
    
    cat("DEBUG: Combined data total:", nrow(data), "rows\n")
  
    safe_disconnect(con)
    
    # Map facility and foreman names
    facilities <- get_facility_lookup()
    if (!is.null(facilities) && nrow(facilities) > 0) {
      facility_map <- setNames(facilities$full_name, facilities$short_name)
      data$facility_full <- ifelse(
        data$facility %in% names(facility_map),
        facility_map[data$facility],
        data$facility
      )
    } else {
      data$facility_full <- data$facility
    }
    
    foremen <- get_foremen_lookup()
    if (!is.null(foremen) && nrow(foremen) > 0) {
      foreman_map <- setNames(foremen$shortname, foremen$emp_num)
      data$foreman_name <- ifelse(
        data$fosarea %in% names(foreman_map),
        foreman_map[data$fosarea],
        data$fosarea
      )
    } else {
      data$foreman_name <- data$fosarea
    }
    
    return(data)
    
  }, error = function(e) {
    warning(paste("Error loading historical catch basin data:", e$message))
    if (exists("con") && !is.null(con)) {
      safe_disconnect(con)
    }
    return(data.frame())
  })
}

# Main function to create historical data for charts
create_historical_cb_data <- function(start_year, end_year, 
                                      hist_time_period, 
                                      hist_display_metric, 
                                      hist_group_by, 
                                      hist_zone_display,
                                      facility_filter = NULL, 
                                      zone_filter = NULL, 
                                      foreman_filter = NULL) {
  
  cat("DEBUG CB create_historical_cb_data called!\n")
  cat("  - hist_time_period:", hist_time_period, "\n")
  cat("  - hist_display_metric:", hist_display_metric, "\n")
  cat("  - start_year:", start_year, "end_year:", end_year, "\n")
  
  # Normalize metric names
  hist_display_metric <- gsub("weekly_", "", hist_display_metric)
  
  # Determine if zones should be shown separately
  show_zones_separately <- hist_zone_display == "show-both" && length(zone_filter) > 1
  
  # Process based on time period
  if (hist_time_period == "yearly") {
    # For yearly: use the pre-aggregated function (faster)
    treatments <- load_historical_cb_data(
      start_year = start_year,
      end_year = end_year,
      facility_filter = facility_filter,
      zone_filter = zone_filter,
      foreman_filter = foreman_filter
    )
    
    if (is.null(treatments) || nrow(treatments) == 0) {
      return(data.frame())
    }
    
    result <- treatments %>%
      mutate(time_period = as.character(treatment_year)) %>%
      select(time_period, facility, facility_full, zone, fosarea, foreman_name, 
             count = total_count)
  } else {
    # For weekly ACTIVE treatments: count catch basins with ACTIVE treatment on each week's Friday
    # A treatment is active if: inspdate <= week_friday AND inspdate + effect_days >= week_friday
    
    raw_data <- load_historical_treatments(
      start_year = start_year,
      end_year = end_year,
      zone_filter = zone_filter
    )
    
    cat("DEBUG CB: raw_data$treatments rows =", 
        if (!is.null(raw_data$treatments)) nrow(raw_data$treatments) else "NULL", "\n")
    
    if (is.null(raw_data$treatments) || nrow(raw_data$treatments) == 0) {
      cat("DEBUG CB ERROR: No raw_data returned from load_historical_treatments\n")
      return(data.frame())
    }
    
    treatments <- raw_data$treatments %>%
      mutate(
        inspdate = as.Date(inspdate),
        effect_days = ifelse(is.na(effect_days), 28, effect_days),  # Default 28 days for CB
        treatment_end = inspdate + effect_days
      )
    
    cat("DEBUG CB: After mutate, treatments rows =", nrow(treatments), "\n")
    
    # Apply facility filter if provided (and not "all")
    if (!is.null(facility_filter) && length(facility_filter) > 0 && !("all" %in% facility_filter)) {
      cat("DEBUG CB: Applying facility filter:", paste(facility_filter, collapse = ","), "\n")
      treatments <- treatments %>% filter(facility %in% facility_filter)
      cat("DEBUG CB: After facility filter, treatments rows =", nrow(treatments), "\n")
    }
    
    # Build facility_full using lookup
    facilities <- get_facility_lookup()
    if (!is.null(facilities) && nrow(facilities) > 0) {
      facility_map <- setNames(facilities$full_name, facilities$short_name)
      treatments$facility_full <- ifelse(
        treatments$facility %in% names(facility_map),
        facility_map[treatments$facility],
        treatments$facility
      )
    } else {
      treatments$facility_full <- treatments$facility
    }
    
    # Generate all weeks in the date range
    start_date <- as.Date(paste0(start_year, "-01-01"))
    end_date <- as.Date(paste0(end_year, "-12-31"))
    all_weeks <- seq.Date(start_date, end_date, by = "week")
    
    cat("DEBUG CB: Loaded", nrow(treatments), "raw treatments\n")
    cat("DEBUG CB: Processing", length(all_weeks), "weeks\n")
    
    # For each week, count ACTIVE catch basins (treatments that are still active on that Friday)
    week_data <- data.frame()
    
    for (week_start in all_weeks) {
      week_friday <- as.Date(week_start) + 4  # Friday of that week
      week_label <- paste0(year(week_friday), "-W", sprintf("%02d", week(week_friday)))
      
      # Find catch basins with active treatment on that Friday
      # Active = treatment started on or before Friday AND treatment hasn't expired yet
      active_on_friday <- treatments %>%
        filter(
          inspdate <= week_friday,
          treatment_end >= week_friday
        )
      
      if (nrow(active_on_friday) > 0) {
        # Count UNIQUE catch basins with active treatment (not total treatments)
        active_data <- active_on_friday %>%
          mutate(time_period = week_label) %>%
          select(catchbasin_id, facility, facility_full, zone, time_period) %>%
          distinct(catchbasin_id, .keep_all = TRUE)  # One count per catch basin
        
        week_data <- bind_rows(week_data, active_data)
      }
    }
    
    if (nrow(week_data) == 0) {
      cat("DEBUG CB ERROR: week_data is empty after loop\n")
      return(data.frame())
    }
    
    # Each row is a unique active catch basin per week
    result <- week_data %>%
      mutate(count = 1)
  }
  
  # Apply grouping
  if (hist_group_by == "facility") {
    if (show_zones_separately) {
      result <- result %>%
        group_by(time_period, facility, facility_full, zone) %>%
        summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
        mutate(group_name = paste0(facility_full, " P", zone))
    } else {
      result <- result %>%
        group_by(time_period, facility, facility_full) %>%
        summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
        mutate(group_name = facility_full)
    }
  } else if (hist_group_by == "foreman") {
    if (show_zones_separately && "fosarea" %in% names(result)) {
      result <- result %>%
        group_by(time_period, fosarea, zone) %>%
        summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
        mutate(group_name = paste0(fosarea, " P", zone))
    } else if ("fosarea" %in% names(result)) {
      result <- result %>%
        group_by(time_period, fosarea) %>%
        summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
        mutate(group_name = fosarea)
    } else {
      # No foreman data, fall back to mmcd_all
      result <- result %>%
        group_by(time_period) %>%
        summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
        mutate(group_name = "All MMCD")
    }
  } else {
    # mmcd_all
    result <- result %>%
      group_by(time_period) %>%
      summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      mutate(group_name = "All MMCD")
  }
  cat(capture.output(head(result)), sep = "\n")
  cat("I am here\n")
  return(result)
}

# Create historical chart - uses shared create_trend_chart
create_historical_cb_chart <- function(data, hist_time_period, hist_display_metric, hist_group_by, chart_type = "stacked_bar", theme = "MMCD", show_zones_separately = FALSE) {
  if (is.null(data) || nrow(data) == 0) {
    return(plotly_empty() %>% layout(title = "No historical data available"))
  }
  
  # Build labels
  metric_label <- case_when(
    hist_display_metric %in% c("treatments", "weekly_active_treatments") ~ "Treatments",
    hist_display_metric %in% c("total_count", "weekly_active_wet_cb") ~ "Wet Catch Basins",
    TRUE ~ "Count"
  )
  period_label <- ifelse(hist_time_period == "yearly", "Year", "Week")
  chart_title <- paste("Historical Catch Basin", metric_label, "by", period_label)
  
  # Get colors based on grouping
  colors <- NULL
  if (hist_group_by == "facility" && "group_name" %in% names(data)) {
    colors <- map_facility_display_names_to_colors(unique(data$group_name), theme, handle_zones = show_zones_separately)
  } else if (hist_group_by == "foreman" && "group_name" %in% names(data)) {
    colors <- map_foreman_display_names_to_colors(unique(data$group_name), theme, handle_zones = show_zones_separately)
  }
  
  # Use shared chart function
  create_trend_chart(data, chart_type, chart_title, period_label, metric_label, colors)
}

# Format historical data table
format_historical_cb_table <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame(Message = "No historical data available"))
  }
  
  # Format for display
  display_data <- data %>%
    mutate(
      count = format(count, big.mark = ",")
    ) %>%
    select(
      `Time Period` = time_period,
      Group = group_name,
      Count = count
    )
    cat("I am here also\n")
  return(display_data)
} 





