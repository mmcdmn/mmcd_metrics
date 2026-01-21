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
    # Build filters
    facility_where <- ""
    if (!is.null(facility_filter) && length(facility_filter) > 0 && !"all" %in% facility_filter) {
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
        
        facility_list <- paste0("'", converted_filters, "'", collapse = ", ")
        facility_where <- paste0("AND loc_catchbasin.facility IN (", facility_list, ")")
        cat("DEBUG: Facility filter converted:", paste(facility_filter, "->", converted_filters, collapse = ", "), "\n")
      } else {
        # Fallback: assume they're already codes
        facility_list <- paste0("'", facility_filter, "'", collapse = ", ")
        facility_where <- paste0("AND loc_catchbasin.facility IN (", facility_list, ")")
      }
    }
    
    zone_where <- ""
    if (!is.null(zone_filter) && length(zone_filter) > 0) {
      if (length(zone_filter) == 1) {
        zone_where <- paste0("AND sc.zone = '", zone_filter, "'")
      } else {
        zone_where <- "AND sc.zone IN ('1', '2')"
      }
    }
    
    foreman_where <- ""
    if (!is.null(foreman_filter) && length(foreman_filter) > 0 && !"all" %in% foreman_filter) {
      foreman_list <- paste0("'", foreman_filter, "'", collapse = ", ")
      foreman_where <- paste0("AND sc.fosarea IN (", foreman_list, ")")
    }
    # Use shared function to determine which years are in which table
    # This handles March transitions when current data moves to archive
    year_ranges <- get_historical_year_ranges(con, "dblarv_insptrt_current", "dblarv_insptrt_archive", "inspdate")
    current_years <- year_ranges$current_years
    archive_years <- year_ranges$archive_years
    
    data <- data.frame()
    
    # Get data from CURRENT table for recent years (2025+)
    current_year_range <- intersect(start_year:end_year, current_years)
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
    archive_year_range <- intersect(start_year:end_year, archive_years)
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
  
  # Normalize metric names
  hist_display_metric <- gsub("weekly_", "", hist_display_metric)
  
  # Load historical treatment data
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
  
  # Determine if zones should be shown separately
  show_zones_separately <- hist_zone_display == "show-both" && length(zone_filter) > 1
  
  # Process based on time period
  if (hist_time_period == "yearly") {
    # Data is already aggregated by year from the query
    # Use the total_count which represents the number of treatments
    result <- treatments %>%
      mutate(time_period = as.character(treatment_year)) %>%
      select(time_period, facility, facility_full, zone, fosarea, foreman_name, 
             count = total_count)  # Use total_count as the count
  } else {
    # Weekly aggregation not supported with this aggregated data structure
    # Would need raw treatment data for weekly calculations
    warning("Weekly aggregation not supported with current data structure")
    result <- data.frame()
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
    if (show_zones_separately) {
      result <- result %>%
        group_by(time_period, fosarea, foreman_name, zone) %>%
        summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
        mutate(group_name = paste0(foreman_name, " P", zone))
    } else {
      result <- result %>%
        group_by(time_period, fosarea, foreman_name) %>%
        summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
        mutate(group_name = foreman_name)
    }
  } else {
    # mmcd_all
    result <- result %>%
      group_by(time_period) %>%
      summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      mutate(group_name = "All MMCD")
  }
  
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
  
  return(display_data)
} 





