# Catch Basin Status - Historical Functions


library(dplyr)
library(lubridate)
library(plotly)
library(DT)

source("data_functions.R")
source("display_functions.R")

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
      facility_list <- paste0("'", facility_filter, "'", collapse = ", ")
      facility_where <- paste0("AND loc_catchbasin.facility IN (", facility_list, ")")
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
    
    # Query for CURRENT YEAR treatments (from dblarv_insptrt_current)
    current_query <- sprintf("
      SELECT 
        loc_catchbasin.gid as catchbasin_id,
        loc_catchbasin.sitecode,
        left(loc_catchbasin.sitecode, 7) as sectcode,
        loc_catchbasin.facility,
        sc.zone,
        sc.fosarea,
        dblarv_insptrt_current.inspdate,
        dblarv_treatment_catchbasin.status,
        mattype_list_targetdose.effect_days,
        EXTRACT(YEAR FROM dblarv_insptrt_current.inspdate) as treatment_year
      FROM dblarv_insptrt_current
      JOIN dblarv_treatment_catchbasin ON dblarv_insptrt_current.pkey_pg = dblarv_treatment_catchbasin.treatment_id
      JOIN loc_catchbasin ON dblarv_treatment_catchbasin.catchbasin_id = loc_catchbasin.gid
      JOIN mattype_list_targetdose USING (matcode)
      LEFT JOIN gis_sectcode sc ON left(loc_catchbasin.sitecode, 7) = sc.sectcode
      WHERE EXTRACT(YEAR FROM dblarv_insptrt_current.inspdate) = %d
        AND loc_catchbasin.status_udw = 'W'
        AND loc_catchbasin.lettergrp <> 'Z'
        %s
        %s
        %s
    ", current_year, facility_where, zone_where, foreman_where)
    
    # Query for ARCHIVE treatments (from dblarv_insptrt_archive)
    # Use same structure as current year query with dblarv_treatment_cb_archive
    archive_query <- sprintf("
      SELECT 
        loc_catchbasin.gid as catchbasin_id,
        loc_catchbasin.sitecode,
        left(loc_catchbasin.sitecode, 7) as sectcode,
        loc_catchbasin.facility,
        sc.zone,
        sc.fosarea,
        dblarv_insptrt_archive.inspdate,
        dblarv_treatment_cb_archive.status,
        mattype_list_targetdose.effect_days,
        EXTRACT(YEAR FROM dblarv_insptrt_archive.inspdate) as treatment_year
      FROM dblarv_insptrt_archive
      JOIN dblarv_treatment_cb_archive ON dblarv_insptrt_archive.pkey_pg = dblarv_treatment_cb_archive.treatment_id
      JOIN loc_catchbasin ON dblarv_treatment_cb_archive.catchbasin_id = loc_catchbasin.gid
      JOIN mattype_list_targetdose USING (matcode)
      LEFT JOIN gis_sectcode sc ON left(loc_catchbasin.sitecode, 7) = sc.sectcode
      WHERE EXTRACT(YEAR FROM dblarv_insptrt_archive.inspdate) BETWEEN %d AND %d
        AND EXTRACT(YEAR FROM dblarv_insptrt_archive.inspdate) < %d
        AND loc_catchbasin.status_udw = 'W'
        AND loc_catchbasin.lettergrp <> 'Z'
        %s
        %s
        %s
    ", start_year, end_year, current_year, facility_where, zone_where, foreman_where)
    
    # Combine current and archive data
    if (current_year >= start_year && current_year <= end_year) {
      # Need both current and archive
      current_data <- dbGetQuery(con, current_query)
      archive_data <- dbGetQuery(con, archive_query)
      data <- bind_rows(current_data, archive_data)
    } else if (end_year < current_year) {
      # Only archive needed
      data <- dbGetQuery(con, archive_query)
    } else {
      # Only current needed
      data <- dbGetQuery(con, current_query)
    }
    
    dbDisconnect(con)
    
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
      dbDisconnect(con)
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
    # Yearly aggregation
    if (hist_display_metric == "treatments") {
      # Count total treatments per year
      result <- treatments %>%
        mutate(time_period = as.character(treatment_year)) %>%
        group_by(time_period, facility, facility_full, zone, fosarea, foreman_name) %>%
        summarise(count = n(), .groups = "drop")
    } else {
      # Count unique wet catch basins per year
      result <- treatments %>%
        mutate(time_period = as.character(treatment_year)) %>%
        group_by(time_period, facility, facility_full, zone, fosarea, foreman_name, catchbasin_id) %>%
        summarise(.groups = "drop") %>%
        group_by(time_period, facility, facility_full, zone, fosarea, foreman_name) %>%
        summarise(count = n(), .groups = "drop")
    }
  } else {
    # Weekly aggregation
    start_date <- as.Date(paste0(start_year, "-01-01"))
    end_date <- as.Date(paste0(end_year, "-12-31"))
    all_weeks <- seq.Date(start_date, end_date, by = "week")
    
    week_data <- data.frame()
    
    for (week_start in all_weeks) {
      week_friday <- as.Date(week_start) + 4
      week_label <- paste0(year(week_friday), "-W", sprintf("%02d", week(week_friday)))
      
      # Find active treatments on that Friday
      active_treatments <- treatments %>%
        mutate(
          treatment_end = as.Date(inspdate) + ifelse(is.na(effect_days), 150, effect_days)
        ) %>%
        filter(
          as.Date(inspdate) <= week_friday,
          treatment_end >= week_friday
        )
      
      if (nrow(active_treatments) > 0) {
        if (hist_display_metric == "active_treatments") {
          # Count active treatments
          week_result <- active_treatments %>%
            mutate(time_period = week_label) %>%
            group_by(time_period, facility, facility_full, zone, fosarea, foreman_name) %>%
            summarise(count = n(), .groups = "drop")
        } else {
          # Count unique active wet catch basins
          week_result <- active_treatments %>%
            mutate(time_period = week_label) %>%
            group_by(time_period, facility, facility_full, zone, fosarea, foreman_name, catchbasin_id) %>%
            summarise(.groups = "drop") %>%
            group_by(time_period, facility, facility_full, zone, fosarea, foreman_name) %>%
            summarise(count = n(), .groups = "drop")
        }
        
        week_data <- bind_rows(week_data, week_result)
      }
    }
    
    result <- week_data
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

# Create historical chart
create_historical_cb_chart <- function(data, hist_time_period, hist_display_metric, hist_group_by, chart_type = "stacked_bar") {
  if (is.null(data) || nrow(data) == 0) {
    return(plot_ly() %>%
      layout(
        title = "No historical data available",
        xaxis = list(title = "Time Period"),
        yaxis = list(title = "Count")
      ))
  }
  
  # Determine chart title
  metric_label <- case_when(
    hist_display_metric %in% c("treatments", "weekly_active_treatments") ~ "Treatments",
    hist_display_metric %in% c("wet_cb_count", "weekly_active_wet_cb") ~ "Wet Catch Basins",
    TRUE ~ "Count"
  )
  
  period_label <- ifelse(hist_time_period == "yearly", "Year", "Week")
  
  chart_title <- paste("Historical Catch Basin", metric_label, "by", period_label)
  
  # Get group colors if applicable
  group_colors <- NULL
  if (hist_group_by == "facility" && "facility" %in% names(data)) {
    facility_colors <- get_facility_base_colors()
    group_colors <- facility_colors
  } else if (hist_group_by == "foreman" && "fosarea" %in% names(data)) {
    foreman_colors <- get_foreman_colors()
    group_colors <- foreman_colors
  }
  
  # Create plotly chart based on chart_type
  if (chart_type == "stacked_bar") {
    # Stacked bar chart
    if (!is.null(group_colors) && length(group_colors) > 0 && hist_group_by != "mmcd_all") {
      p <- plot_ly(data, x = ~time_period, y = ~count, color = ~group_name,
                   type = 'bar',
                   colors = unname(group_colors)) %>%
        layout(barmode = 'stack')
    } else {
      p <- plot_ly(data, x = ~time_period, y = ~count, color = ~group_name,
                   type = 'bar') %>%
        layout(barmode = 'stack')
    }
  } else if (chart_type == "grouped_bar") {
    # Grouped bar chart
    if (!is.null(group_colors) && length(group_colors) > 0 && hist_group_by != "mmcd_all") {
      p <- plot_ly(data, x = ~time_period, y = ~count, color = ~group_name,
                   type = 'bar',
                   colors = unname(group_colors)) %>%
        layout(barmode = 'group')
    } else {
      p <- plot_ly(data, x = ~time_period, y = ~count, color = ~group_name,
                   type = 'bar') %>%
        layout(barmode = 'group')
    }
  } else if (chart_type == "area") {
    # Area chart
    if (!is.null(group_colors) && length(group_colors) > 0 && hist_group_by != "mmcd_all") {
      p <- plot_ly(data, x = ~time_period, y = ~count, color = ~group_name,
                   type = 'scatter', mode = 'lines', fill = 'tonexty',
                   colors = unname(group_colors)) %>%
        layout(hovermode = 'x unified')
    } else {
      p <- plot_ly(data, x = ~time_period, y = ~count, color = ~group_name,
                   type = 'scatter', mode = 'lines', fill = 'tonexty') %>%
        layout(hovermode = 'x unified')
    }
  } else {
    # Default: Line chart
    if (!is.null(group_colors) && length(group_colors) > 0 && hist_group_by != "mmcd_all") {
      p <- plot_ly(data, x = ~time_period, y = ~count, color = ~group_name,
                   type = 'scatter', mode = 'lines+markers',
                   colors = unname(group_colors))
    } else {
      p <- plot_ly(data, x = ~time_period, y = ~count, color = ~group_name,
                   type = 'scatter', mode = 'lines+markers')
    }
  }
  
  p <- p %>%
    layout(
      title = list(text = chart_title, font = list(size = 20)),
      xaxis = list(
        title = list(text = period_label, font = list(size = 18)),
        tickfont = list(size = 16)
      ),
      yaxis = list(
        title = list(text = metric_label, font = list(size = 18)),
        tickfont = list(size = 16)
      ),
      legend = list(
        font = list(size = 16)
      ),
      hovermode = "closest",
      font = list(size = 16)
    )
  
  return(p)
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





