# Structure Treatment - Historical Functions
# Functions for loading and processing historical structure treatment data

library(dplyr)
library(lubridate)
library(plotly)
library(DT)

# Load historical structure treatment data from archive and current tables
load_historical_struct_data <- function(start_year, end_year, 
                                        facility_filter = NULL, 
                                        zone_filter = NULL, 
                                        foreman_filter = NULL,
                                        structure_type_filter = NULL,
                                        status_types = c("D", "W", "U")) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  # Get dynamic year ranges to determine which tables to query
  year_ranges <- get_historical_year_ranges(con, "dblarv_insptrt_current", "dblarv_insptrt_archive", "inspdate")
  current_table_years <- year_ranges$current_years
  archive_table_years <- year_ranges$archive_years
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  tryCatch({
    # Build filters
    facility_where <- ""
    if (!is.null(facility_filter) && length(facility_filter) > 0 && !"all" %in% facility_filter) {
      facility_list <- paste0("'", facility_filter, "'", collapse = ", ")
      facility_where <- paste0("AND gis.facility IN (", facility_list, ")")
    }
    
    zone_where <- ""
    if (!is.null(zone_filter) && length(zone_filter) > 0) {
      if (length(zone_filter) == 1) {
        zone_where <- paste0("AND gis.zone = '", zone_filter, "'")
      } else {
        zone_where <- "AND gis.zone IN ('1', '2')"
      }
    }
    
    foreman_where <- ""
    if (!is.null(foreman_filter) && length(foreman_filter) > 0 && !"all" %in% foreman_filter) {
      # Convert shortnames to emp_num (foreman_filter contains shortnames, but gis.fosarea contains emp_num)
      shortname_list <- paste0("'", paste(foreman_filter, collapse = "','"), "'")
      emp_nums_query <- sprintf("
        SELECT emp_num 
        FROM employee_list 
        WHERE shortname IN (%s)
      ", shortname_list)
      
      emp_nums <- dbGetQuery(con, emp_nums_query)
      
      if (nrow(emp_nums) > 0) {
        emp_num_list <- paste0("'", paste(emp_nums$emp_num, collapse = "','"), "'")
        foreman_where <- paste0("AND gis.fosarea IN (", emp_num_list, ")")
      }
    }
    
    structure_type_where <- ""
    if (!is.null(structure_type_filter) && length(structure_type_filter) > 0 && !"all" %in% structure_type_filter) {
      # Use the same logic as data_functions.R
      if (length(structure_type_filter) == 1) {
        structure_type_where <- get_structure_type_condition(structure_type_filter)
      } else {
        # Multiple types selected
        type_conditions <- sapply(structure_type_filter, function(st) {
          if (toupper(st) == "CV") {
            "(UPPER(loc.s_type) = 'CV' OR UPPER(loc.s_type) LIKE 'CV/%' OR UPPER(loc.s_type) LIKE '%/CV')"
          } else if (toupper(st) == "PR") {
            "(UPPER(loc.s_type) = 'PR' OR UPPER(loc.s_type) LIKE 'PR/%' OR UPPER(loc.s_type) LIKE '%/PR')"
          } else {
            sprintf("UPPER(loc.s_type) = UPPER('%s')", st)
          }
        })
        structure_type_where <- paste0("AND (", paste(type_conditions, collapse = " OR "), ")")
      }
    }
    
    status_where <- ""
    if (!is.null(status_types) && length(status_types) > 0) {
      status_list <- paste0("'", paste(status_types, collapse = "','"), "'")
      status_where <- paste0("AND loc.status_udw IN (", status_list, ")")
    }
    
    # Determine which years need current table vs archive table
    current_years_needed <- intersect(start_year:end_year, current_table_years)
    archive_years_needed <- intersect(start_year:end_year, archive_table_years)
    
    all_data <- data.frame()
    
    # Query CURRENT table if needed
    if (length(current_years_needed) > 0) {
      current_query <- sprintf("
        SELECT DISTINCT
          trt.sitecode,
          trt.inspdate,
          COALESCE(mat.effect_days, 30) AS effect_days,
          loc.s_type,
          loc.priority,
          gis.facility,
          gis.fosarea,
          gis.zone,
          EXTRACT(YEAR FROM trt.inspdate) as treatment_year
        FROM public.dblarv_insptrt_current trt
        LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode
        LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
        LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
        WHERE EXTRACT(YEAR FROM trt.inspdate) IN (%s)
          AND trt.list_type = 'STR'
          AND (loc.enddate IS NULL OR loc.enddate > trt.inspdate)
          %s
          %s
          %s
          %s
          %s
      ", paste(current_years_needed, collapse = ","), facility_where, zone_where, foreman_where, structure_type_where, status_where)
      
      current_data <- dbGetQuery(con, current_query)
      all_data <- bind_rows(all_data, current_data)
    }
    
    # Query ARCHIVE table if needed
    if (length(archive_years_needed) > 0) {
      archive_query <- sprintf("
        SELECT DISTINCT
          trt.sitecode,
          trt.inspdate,
          COALESCE(mat.effect_days, 30) AS effect_days,
          loc.s_type,
          loc.priority,
          gis.facility,
          gis.fosarea,
          gis.zone,
          EXTRACT(YEAR FROM trt.inspdate) as treatment_year
        FROM public.dblarv_insptrt_archive trt
        LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode
        LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
        LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
        WHERE EXTRACT(YEAR FROM trt.inspdate) IN (%s)
          AND trt.list_type = 'STR'
          AND (loc.enddate IS NULL OR loc.enddate > trt.inspdate)
          %s
          %s
          %s
          %s
          %s
      ", paste(archive_years_needed, collapse = ","), facility_where, zone_where, foreman_where, structure_type_where, status_where)
      
      archive_data <- dbGetQuery(con, archive_query)
      all_data <- bind_rows(all_data, archive_data)
    }
    
    # Add facility and foreman name lookups
    if (nrow(all_data) > 0) {
      facility_lookup <- get_facility_lookup()
      foremen_lookup <- get_foremen_lookup()
      
      all_data <- all_data %>%
        left_join(facility_lookup %>% select(short_name, full_name), 
                  by = c("facility" = "short_name")) %>%
        rename(facility_full = full_name) %>%
        left_join(foremen_lookup %>% select(emp_num, shortname), 
                  by = c("fosarea" = "emp_num")) %>%
        rename(foreman_name = shortname)
    }
    
    return(all_data)
    
  }, error = function(e) {
    warning(paste("Error loading historical structure data:", e$message))
    return(data.frame())
  }, finally = {
    safe_disconnect(con)
  })
}

# Create aggregated historical structure data for charting
create_historical_struct_data <- function(start_year, end_year, 
                                          hist_time_period, 
                                          hist_display_metric, 
                                          hist_group_by, 
                                          hist_zone_display,
                                          facility_filter = NULL, 
                                          zone_filter = NULL, 
                                          foreman_filter = NULL,
                                          structure_type_filter = NULL,
                                          status_types = c("D", "W", "U")) {
  
  # Normalize metric names (remove weekly_ prefix if present)
  hist_display_metric <- gsub("weekly_", "", hist_display_metric)
  
  # Load historical treatment data
  treatments <- load_historical_struct_data(
    start_year = start_year,
    end_year = end_year,
    facility_filter = facility_filter,
    zone_filter = zone_filter,
    foreman_filter = foreman_filter,
    structure_type_filter = structure_type_filter,
    status_types = status_types
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
    } else if (hist_display_metric == "structures_count") {
      # Count unique structures treated per year
      result <- treatments %>%
        mutate(time_period = as.character(treatment_year)) %>%
        group_by(time_period, facility, facility_full, zone, fosarea, foreman_name, sitecode) %>%
        summarise(.groups = "drop") %>%
        group_by(time_period, facility, facility_full, zone, fosarea, foreman_name) %>%
        summarise(count = n(), .groups = "drop")
    } else {
      # proportion - Count treated structures, we'll calculate proportion AFTER getting totals
      # DO NOT calculate proportion yet - just count treated structures per group
      result <- treatments %>%
        mutate(time_period = as.character(treatment_year)) %>%
        group_by(time_period, facility, facility_full, zone, fosarea, foreman_name, sitecode) %>%
        summarise(.groups = "drop") %>%
        group_by(time_period, facility, facility_full, zone, fosarea, foreman_name) %>%
        summarise(treated_structures = n(), .groups = "drop")  # COUNT OF TREATED STRUCTURES
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
          treatment_end = as.Date(inspdate) + ifelse(is.na(effect_days), 30, effect_days)
        ) %>%
        filter(
          as.Date(inspdate) <= week_friday,
          treatment_end >= week_friday
        )
      
      if (nrow(active_treatments) > 0) {
        # Count active treatments
        week_result <- active_treatments %>%
          mutate(time_period = week_label) %>%
          group_by(time_period, facility, facility_full, zone, fosarea, foreman_name) %>%
          summarise(count = n(), .groups = "drop")
        
        week_data <- bind_rows(week_data, week_result)
      }
    }
    
    result <- week_data
  }
  
  # Apply grouping
  # Determine column name based on metric type
  value_col <- if (hist_display_metric == "proportion") "treated_structures" else "count"
  
  if (hist_group_by == "facility") {
    if (show_zones_separately) {
      result <- result %>%
        group_by(time_period, facility, facility_full, zone) %>%
        summarise(!!value_col := sum(!!sym(value_col), na.rm = TRUE), .groups = "drop") %>%
        mutate(group_name = paste0(facility_full, " P", zone))
    } else {
      result <- result %>%
        group_by(time_period, facility, facility_full) %>%
        summarise(!!value_col := sum(!!sym(value_col), na.rm = TRUE), .groups = "drop") %>%
        mutate(group_name = facility_full)
    }
  } else if (hist_group_by == "foreman") {
    if (show_zones_separately) {
      result <- result %>%
        group_by(time_period, fosarea, foreman_name, zone) %>%
        summarise(!!value_col := sum(!!sym(value_col), na.rm = TRUE), .groups = "drop") %>%
        mutate(group_name = paste0(foreman_name, " P", zone))
    } else {
      result <- result %>%
        group_by(time_period, fosarea, foreman_name) %>%
        summarise(!!value_col := sum(!!sym(value_col), na.rm = TRUE), .groups = "drop") %>%
        mutate(group_name = foreman_name)
    }
  } else if (hist_group_by == "mmcd_all") {
    result <- result %>%
      group_by(time_period) %>%
      summarise(!!value_col := sum(!!sym(value_col), na.rm = TRUE), .groups = "drop") %>%
      mutate(group_name = "MMCD")
  }
  
  # NOW calculate proportion if that's the metric requested
  if (hist_display_metric == "proportion") {
    # Get total structures per group (using current active structures)
    # IMPORTANT: Apply same filters as were used for treatments data
    con <- get_db_connection()
    
    # Build filter conditions
    facility_where <- ""
    if (!is.null(facility_filter) && length(facility_filter) > 0 && !"all" %in% facility_filter) {
      facility_list <- paste0("'", facility_filter, "'", collapse = ", ")
      facility_where <- paste0("AND gis.facility IN (", facility_list, ")")
    }
    
    zone_where <- ""
    if (!is.null(zone_filter) && length(zone_filter) > 0) {
      if (length(zone_filter) == 1) {
        zone_where <- paste0("AND gis.zone = '", zone_filter, "'")
      } else {
        zone_where <- "AND gis.zone IN ('1', '2')"
      }
    }
    
    structure_type_where <- ""
    if (!is.null(structure_type_filter) && length(structure_type_filter) > 0 && !"all" %in% structure_type_filter) {
      if (length(structure_type_filter) == 1) {
        structure_type_where <- get_structure_type_condition(structure_type_filter)
      } else {
        conditions <- sapply(structure_type_filter, get_structure_type_condition)
        structure_type_where <- paste0("AND (", paste(gsub("^AND ", "", conditions), collapse = " OR "), ")")
      }
    }
    
    status_where <- ""
    if (!is.null(status_types) && length(status_types) > 0) {
      status_list <- paste0("'", status_types, "'", collapse = ", ")
      status_where <- paste0("AND loc.status_udw IN (", status_list, ")")
    }
    
    # Build query based on grouping
    if (hist_group_by == "facility") {
      if (show_zones_separately) {
        query <- paste0("
          SELECT 
            gis.facility,
            gis.zone,
            COUNT(DISTINCT loc.sitecode)::INTEGER as total_count
          FROM loc_cxstruct loc
          INNER JOIN gis_sectcode gis ON loc.sectcode = gis.sectcode
          WHERE 1=1
          ", facility_where, " ", zone_where, " ", structure_type_where, " ", status_where, "
          GROUP BY gis.facility, gis.zone
        ")
        total_count <- dbGetQuery(con, query) %>% as_tibble()
        result <- result %>% left_join(total_count, by = c("facility", "zone"))
      } else {
        query <- paste0("
          SELECT 
            gis.facility,
            COUNT(DISTINCT loc.sitecode)::INTEGER as total_count
          FROM loc_cxstruct loc
          INNER JOIN gis_sectcode gis ON loc.sectcode = gis.sectcode
          WHERE 1=1
          ", facility_where, " ", zone_where, " ", structure_type_where, " ", status_where, "
          GROUP BY gis.facility
        ")
        total_count <- dbGetQuery(con, query) %>% as_tibble()
        result <- result %>% left_join(total_count, by = "facility")
      }
    } else if (hist_group_by == "foreman") {
      if (show_zones_separately) {
        query <- paste0("
          SELECT 
            gis.fosarea,
            gis.zone,
            COUNT(DISTINCT loc.sitecode)::INTEGER as total_count
          FROM loc_cxstruct loc
          INNER JOIN gis_sectcode gis ON loc.sectcode = gis.sectcode
          WHERE 1=1
          ", facility_where, " ", zone_where, " ", structure_type_where, " ", status_where, "
          GROUP BY gis.fosarea, gis.zone
        ")
        total_count <- dbGetQuery(con, query) %>% as_tibble()
        result <- result %>% left_join(total_count, by = c("fosarea", "zone"))
      } else {
        query <- paste0("
          SELECT 
            gis.fosarea,
            COUNT(DISTINCT loc.sitecode)::INTEGER as total_count
          FROM loc_cxstruct loc
          INNER JOIN gis_sectcode gis ON loc.sectcode = gis.sectcode
          WHERE 1=1
          ", facility_where, " ", zone_where, " ", structure_type_where, " ", status_where, "
          GROUP BY gis.fosarea
        ")
        total_count <- dbGetQuery(con, query) %>% as_tibble()
        result <- result %>% left_join(total_count, by = "fosarea")
      }
    } else if (hist_group_by == "mmcd_all") {
      query <- paste0("
        SELECT 
          COUNT(DISTINCT loc.sitecode)::INTEGER as total_count
        FROM loc_cxstruct loc
        INNER JOIN gis_sectcode gis ON loc.sectcode = gis.sectcode
        WHERE 1=1
        ", facility_where, " ", zone_where, " ", structure_type_where, " ", status_where
      )
      total_count_result <- dbGetQuery(con, query)[[1]]
      result <- result %>% mutate(total_count = total_count_result)
    }
    
    # Calculate proportion as percentage
    # treated_structures = count of structures that received treatment
    # total_count = count of ALL structures in that group (with filters applied)
    result <- result %>%
      mutate(
        total_count = ifelse(is.na(total_count) | total_count == 0, treated_structures, total_count),
        count = (treated_structures / total_count) * 100  # Convert to percentage
      ) %>%
      select(-treated_structures, -total_count)
  }
  
  return(result)
}

# Create Plotly historical chart for structure treatments
create_historical_struct_chart <- function(data, 
                                           hist_time_period, 
                                           hist_display_metric, 
                                           hist_group_by, 
                                           chart_type = "stacked_bar",
                                           theme = "MMCD") {
  
  if (is.null(data) || nrow(data) == 0) {
    # Return empty plot with message
    return(plot_ly() %>%
             add_text(x = 0.5, y = 0.5, text = "No historical data available", 
                     textfont = list(size = 16)) %>%
             layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
  }
  
  # Determine colors based on group_by
  if (hist_group_by == "facility") {
    # Check if we have zones in the data
    if (any(grepl(" P[12]$", data$group_name))) {
      # Zone-aware colors needed
      zones <- unique(sub(".* P([12])$", "\\1", data$group_name[grepl(" P[12]$", data$group_name)]))
      group_colors <- get_facility_base_colors(
        alpha_zones = zones,
        combined_groups = unique(data$group_name),
        theme = theme
      )
      if (is.list(group_colors)) group_colors <- group_colors$colors
      # Filter to only include colors for groups actually present in data
      group_colors <- group_colors[names(group_colors) %in% unique(data$group_name)]
    } else {
      # Simple facility colors - use shared utility function
      group_colors <- map_facility_display_names_to_colors(unique(data$group_name), theme)
    }
  } else if (hist_group_by == "foreman") {
    # Check if we have zones in the data
    if (any(grepl(" P[12]$", data$group_name))) {
      # Zone-aware foreman colors
      zones <- unique(sub(".* P([12])$", "\\1", data$group_name[grepl(" P[12]$", data$group_name)]))
      group_colors <- get_foreman_colors(
        alpha_zones = zones,
        combined_groups = unique(data$group_name)
      )
      if (is.list(group_colors)) group_colors <- group_colors$colors
      # Filter to only include colors for groups actually present in data
      group_colors <- group_colors[names(group_colors) %in% unique(data$group_name)]
    } else {
      # Simple foreman colors - use shared utility function
      group_colors <- map_foreman_display_names_to_colors(unique(data$group_name), theme)
    }
  } else {
    # mmcd_all - use single color
    group_colors <- c("MMCD" = get_facility_base_colors(theme = theme)["N"])
  }
  
  # Prepare y-axis title based on metric
  if (hist_time_period == "yearly") {
    y_title <- if (hist_display_metric == "treatments") {
      "Total Treatments"
    } else if (hist_display_metric == "structures_count") {
      "Unique Structures Treated"
    } else {
      "Proportion of Structures Treated (%)"
    }
  } else {
    y_title <- if (hist_display_metric == "proportion") {
      "Proportion of Structures (%)"
    } else {
      "Active Treatments"
    }
  }
  
  # Prepare x-axis title
  x_title <- if (hist_time_period == "yearly") "Year" else "Week"
  
  # VALIDATION: Block stacked bar for proportions
  if (hist_display_metric == "proportion" && chart_type == "stacked_bar") {
    chart_type <- "grouped_bar"  # Auto-switch to grouped bar
  }
  
  # Ensure colors are in correct format for Plotly (named vector matching group_name)
  if (is.null(names(group_colors))) {
    # If unnamed, try to match by position
    group_names <- unique(data$group_name)
    if (length(group_colors) >= length(group_names)) {
      names(group_colors) <- group_names[seq_along(group_colors)]
    }
  }
  
  # Create the plot based on chart type
  if (chart_type == "pie") {
    # Pie chart - aggregate across all time periods for proportion view
    pie_data <- data %>%
      group_by(group_name) %>%
      summarise(count = mean(count, na.rm = TRUE), .groups = "drop")  # Average proportion across years
    
    p <- plot_ly(pie_data, labels = ~group_name, values = ~count, 
                 type = "pie",
                 marker = list(colors = group_colors),
                 textinfo = "label+percent",
                 textfont = list(size = 16),
                 hovertemplate = "%{label}<br>%{value:.1f}%<extra></extra>")
  } else if (chart_type == "stacked_bar") {
    p <- plot_ly(data, x = ~time_period, y = ~count, color = ~group_name, 
                 colors = group_colors, type = "bar") %>%
      layout(barmode = "stack")
  } else if (chart_type == "grouped_bar") {
    p <- plot_ly(data, x = ~time_period, y = ~count, color = ~group_name, 
                 colors = group_colors, type = "bar") %>%
      layout(barmode = "group")
  } else if (chart_type == "line") {
    p <- plot_ly(data, x = ~time_period, y = ~count, color = ~group_name, 
                 colors = group_colors, type = "scatter", mode = "lines+markers")
  } else if (chart_type == "area") {
    p <- plot_ly(data, x = ~time_period, y = ~count, color = ~group_name, 
                 colors = group_colors, type = "scatter", mode = "lines", 
                 fill = "tonexty", stackgroup = "one")
  }
  
  # Layout with larger fonts
  if (chart_type == "pie") {
    # Pie chart layout - no axes
    p <- p %>%
      layout(
        title = list(
          text = paste("Average Proportion of Structures Treated (",
                      min(data$time_period), "-", max(data$time_period), ")"),
          font = list(size = 20, family = "Arial, sans-serif")
        ),
        showlegend = TRUE,
        legend = list(
          font = list(size = 16),
          orientation = "v",
          x = 1.02,
          xanchor = "left",
          y = 1,
          yanchor = "top"
        ),
        font = list(size = 16, family = "Arial, sans-serif"),
        margin = list(l = 80, r = 150, t = 80, b = 80)
      )
  } else {
    # Bar/line/area chart layout - with axes
    p <- p %>%
      layout(
        title = list(
          text = paste("Historical Structure Treatment Trends -", 
                      if (hist_time_period == "yearly") "Yearly" else "Weekly"),
          font = list(size = 20, family = "Arial, sans-serif")
        ),
        xaxis = list(
          title = list(text = x_title, font = list(size = 18)),
          tickfont = list(size = 16)
        ),
        yaxis = list(
          title = list(text = y_title, font = list(size = 18)),
          tickfont = list(size = 16),
          ticksuffix = if (hist_display_metric == "proportion") "%" else ""
        ),
        hovermode = "x unified",
        legend = list(
          title = list(text = "Group", font = list(size = 18)),
          font = list(size = 16),
          orientation = "v",
          x = 1.02,
          xanchor = "left",
          y = 1,
          yanchor = "top"
        ),
        font = list(size = 16, family = "Arial, sans-serif"),
        margin = list(l = 80, r = 150, t = 80, b = 80)
      )
  }
  
  return(p)
}
