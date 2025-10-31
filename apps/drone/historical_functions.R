# Historical data functions

# Shared function to get raw historical data - used by both historical and site average functions
get_shared_historical_data <- function(hist_start_year, hist_end_year, drone_types) {
  return(get_historical_raw_data(hist_start_year, hist_end_year, drone_types))
}

# Historical data function
get_historical_raw_data <- function(hist_start_year, hist_end_year, drone_types) {
  con <- get_db_connection()
  if (is.null(con)) {
    return(NULL)
  }
  
  # Get archive data
  archive_query <- sprintf("
    SELECT facility, sitecode, inspdate, action
    FROM public.dblarv_insptrt_archive
    WHERE action = 'D'
    AND EXTRACT(YEAR FROM inspdate) BETWEEN %d AND %d
  ", as.integer(hist_start_year), as.integer(hist_end_year))
  
  archive_data <- dbGetQuery(con, archive_query)
  
  # Get current data  
  current_query <- sprintf("
    SELECT facility, sitecode, inspdate, action, airgrnd_plan
    FROM public.dblarv_insptrt_current
    WHERE (airgrnd_plan = 'D' OR action = 'D')
    AND EXTRACT(YEAR FROM inspdate) BETWEEN %d AND %d
  ", as.integer(hist_start_year), as.integer(hist_end_year))
  
  current_data <- dbGetQuery(con, current_query)
  
  # Get site size info from loc_breeding_sites
  # Build drone_types filter from UI (Y/M/C) to apply the same designation filtering used by current data
  drone_types_str <- paste0("'", paste(drone_types, collapse = "','"), "'")

  sitecodes <- unique(c(archive_data$sitecode, current_data$sitecode))
  if (length(sitecodes) > 0) {
    sitecodes_str <- paste(sprintf("'%s'", sitecodes), collapse = ",")
    
    # First try a simple query to see if loc_breeding_sites exists and has data
    simple_lbs_query <- sprintf(
      "SELECT b.sitecode, b.facility 
      FROM public.loc_breeding_sites b
      WHERE b.sitecode IN (%s)
      LIMIT 10",
      sitecodes_str)
    
    simple_lbs_data <- dbGetQuery(con, simple_lbs_query)
    
    if (nrow(simple_lbs_data) > 0) {
      # The historical data should work regardless of the drone designation
      lbs_query <- sprintf(
        "SELECT b.sitecode, b.acres, b.facility, b.prehatch, b.drone, b.air_gnd,
               CASE 
                 WHEN e.emp_num IS NOT NULL AND e.active = true THEN sc.fosarea
                 ELSE NULL
               END as foreman, 
               sc.zone,
               left(b.sitecode,7) as sectcode
        FROM public.loc_breeding_sites b
        LEFT JOIN public.gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
        LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
          AND e.emp_type = 'FieldSuper' 
          AND e.active = true
        WHERE b.sitecode IN (%s)
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)",
        sitecodes_str)
      lbs_data <- dbGetQuery(con, lbs_query)
    } else {
      lbs_data <- data.frame(sitecode=character(), acres=numeric(), facility=character(), prehatch=character(), drone=character(), foreman=character(), zone=character(), sectcode=character())
    }
  } else {
    lbs_data <- data.frame(sitecode=character(), acres=numeric(), facility=character(), prehatch=character(), drone=character(), foreman=character(), zone=character(), sectcode=character())
  }
  
  dbDisconnect(con)
  
  list(
    archive = archive_data,
    current = current_data,
    lbs = lbs_data
  )
}

# Process historical data based on user selections
get_historical_processed_data <- function(hist_start_year, hist_end_year, drone_types, zone_filter, facility_filter, foreman_filter, prehatch_only, group_by, hist_display_metric) {
  # Get raw data
  data_list <- get_historical_raw_data(hist_start_year, hist_end_year, drone_types)
  if (is.null(data_list)) {
    return(data.frame())
  }
  
  # Get LBS data for prehatch information
  lbs_data <- data_list$lbs
  
  # Process archive data
  if (nrow(data_list$archive) > 0) {
    archive_data <- data_list$archive %>%
      mutate(
        source = "Archive",
        year = year(inspdate)
      )
  } else {
    archive_data <- data.frame()
  }
  
  # Process current data
  if (nrow(data_list$current) > 0) {
    current_data <- data_list$current %>%
      mutate(
        source = "Current", 
        year = year(inspdate)
      )
  } else {
    current_data <- data.frame()
  }
  
  # Combine the data
  all_data <- bind_rows(archive_data, current_data)
  
  # Check if we have any data before proceeding
  if (nrow(all_data) == 0) {
    return(data.frame())
  }
  
  # Join with lbs_data to get prehatch information and extract sectcode
  all_data <- all_data %>%
    left_join(lbs_data, by = c("sitecode", "facility")) %>%
    mutate(
      # Extract sectcode from sitecode (first 7 characters) - use from lbs_data if available
      sectcode = ifelse(is.na(sectcode), substr(sitecode, 1, 7), sectcode)
    )
  
  # Apply zone filter if selected
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    all_data <- all_data %>%
      filter(zone %in% zone_filter)
  }
  
  # Apply facility filter if selected
  if (!is.null(facility_filter) && length(facility_filter) > 0 && 
      !("all" %in% facility_filter)) {
    all_data <- all_data %>%
      filter(facility %in% facility_filter)
  }
  
  # Apply foreman/FOS filter if selected
  if (!is.null(foreman_filter) && length(foreman_filter) > 0 && 
      !("all" %in% foreman_filter)) {
    # Convert shortnames to employee numbers for filtering
    foremen_lookup <- get_foremen_lookup()
    selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% foreman_filter]
    
    all_data <- all_data %>%
      filter(foreman %in% selected_emp_nums)
  }
  
  # Apply prehatch filter if selected
  if (prehatch_only) {
    all_data <- all_data %>%
      filter(prehatch == 'PREHATCH')
  }
  
  # Determine grouping column and filter for foreman if needed
  group_col <- group_by
  if (group_col == "foreman") {
    # Only include sites that have a foreman assigned
    all_data <- all_data %>% filter(!is.na(foreman) & foreman != "")
  }
  
  # Process based on count type and grouping selection
  show_zones_separately <- length(zone_filter) > 1
  
  # Add combined group column for zone differentiation when both zones selected
  if (show_zones_separately) {
    all_data$combined_group <- paste0(all_data[[group_col]], " (P", all_data$zone, ")")
    group_var <- sym("combined_group")
  } else {
    group_var <- sym(group_col)
  }
  
  # Calculate counts based on display metric
  if (hist_display_metric == "treatments") {
    # Count all treatments
    results <- all_data %>%
      group_by(!!group_var, year) %>%
      summarize(count = n(), .groups = "drop")
  } else if (hist_display_metric == "acres") {
    # Sum acres treated
    results <- all_data %>%
      group_by(!!group_var, year) %>%
      summarize(count = sum(acres, na.rm = TRUE), .groups = "drop")
  } else {
    # Count unique sites (default for "sites")
    results <- all_data %>%
      group_by(!!group_var, year) %>%
      summarize(count = n_distinct(sitecode), .groups = "drop")
  }
  
  # Apply facility name mapping if grouping by facility or sectcode
  if (group_col %in% c("facility", "sectcode")) {
    # Add facility column for mapping if not present
    if (group_col == "facility") {
      if (show_zones_separately) {
        # Extract facility and zone info from combined_group before mapping
        results <- results %>%
          mutate(
            facility = gsub("\\s*\\(P[12]\\).*", "", combined_group),
            zone_part = gsub(".*\\((P[12])\\).*", "\\1", combined_group)
          ) %>%
          map_facility_names(facility_col = "facility") %>%
          mutate(combined_group = paste0(facility_display, " (", zone_part, ")")) %>%
          select(-zone_part, -facility, -facility_display)
      } else {
        results <- results %>%
          mutate(facility = !!sym(group_col)) %>%
          map_facility_names(facility_col = "facility")
      }
    } else if (group_col == "sectcode") {
      # For sectcode grouping, we need to get facility info back
      sectcode_facility_map <- all_data %>%
        select(sectcode, facility) %>%
        distinct()
      
      if (show_zones_separately) {
        # Extract sectcode info from combined_group
        results <- results %>%
          mutate(
            sectcode_part = gsub("\\s*\\(P[12]\\).*", "", combined_group),
            zone_part = gsub(".*\\((P[12])\\).*", "\\1", combined_group)
          ) %>%
          separate(sectcode_part, into = c("facility", "sectcode"), sep = " - ", extra = "merge") %>%
          map_facility_names(facility_col = "facility") %>%
          mutate(
            sectcode_display = paste0(facility_display, " - ", sectcode),
            combined_group = paste0(sectcode_display, " (", zone_part, ")")
          ) %>%
          select(-zone_part, -facility, -facility_display, -sectcode, -sectcode_display)
      } else {
        results <- results %>%
          rename(sectcode_temp = !!group_var) %>%
          left_join(sectcode_facility_map, by = c("sectcode_temp" = "sectcode")) %>%
          map_facility_names(facility_col = "facility") %>%
          mutate(sectcode_display = paste0(facility_display, " - ", sectcode_temp)) %>%
          select(-sectcode_temp, -facility, -facility_display) %>%
          rename(!!group_col := sectcode_display)
      }
    }
  } else if (group_col == "foreman") {
    # Add foreman display formatting using db_helpers
    foremen_lookup <- get_foremen_lookup()
    foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
    
    if (show_zones_separately) {
      results <- results %>%
        mutate(
          foreman_part = gsub("\\s*\\(P[12]\\).*", "", combined_group),
          zone_part = gsub(".*\\((P[12])\\).*", "\\1", combined_group),
          foreman_display = ifelse(
            foreman_part %in% names(foreman_map),
            foreman_map[foreman_part],
            paste("Unknown FOS", foreman_part)
          ),
          combined_group = paste0(foreman_display, " (", zone_part, ")")
        ) %>%
        select(-foreman_part, -zone_part, -foreman_display)
    } else {
      results <- results %>%
        mutate(foreman_display = ifelse(
          foreman %in% names(foreman_map),
          foreman_map[foreman],
          paste("Unknown FOS", foreman)
        ))
      # Keep foreman as employee numbers, add foreman_display as shortnames
    }
  }
  
  # Make sure we have entries for all years in the range
  all_years <- seq(from = as.integer(hist_start_year), to = as.integer(hist_end_year))
  
  # Create complete grid based on zone separation and grouping
  if (show_zones_separately) {
    all_combined_groups <- unique(results$combined_group)
    if (length(all_combined_groups) > 0) {
      expanded_grid <- expand.grid(
        combined_group = all_combined_groups,
        year = all_years
      )
      
      # Join with actual data
      results <- expanded_grid %>%
        left_join(results, by = c("combined_group", "year")) %>%
        mutate(
          count = ifelse(is.na(count), 0, count),
          # Extract zone from combined_group for alpha mapping
          zone_factor = gsub(".*\\(P([12])\\).*", "\\1", combined_group)
        )
    } else {
      # Handle case with no data
      results <- data.frame(combined_group = character(), year = integer(), count = integer(), zone_factor = character())
    }
  } else {
    # Create expanded grid based on grouping type
    unique_groups <- unique(results[[group_col]])
    if (length(unique_groups) > 0) {
      expanded_grid <- data.frame(
        group_val = unique_groups,
        year = rep(all_years, each = length(unique_groups))
      )
      names(expanded_grid)[1] <- group_col
      
      results <- expanded_grid %>%
        left_join(results, by = c(group_col, "year")) %>%
        mutate(count = ifelse(is.na(count), 0, count))
    } else {
      # Create empty data frame with proper column structure
      empty_df <- data.frame(year = integer(), count = integer())
      empty_df[[group_col]] <- character()
      results <- empty_df
    }
  }

  return(results)
}

# Historical plot output - EXACT copy from backup
create_historical_plot <- function(zone_filter, facility_filter, foreman_filter, prehatch_only, group_by, hist_display_metric, hist_show_percentages, hist_start_year, hist_end_year, drone_types) {
  data <- get_historical_processed_data(hist_start_year, hist_end_year, drone_types, zone_filter, facility_filter, foreman_filter, prehatch_only, group_by, hist_display_metric)
  if (nrow(data) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No data available for the selected criteria", cex = 1.5)
    return()
  }
  
  # Determine if we're showing zones separately
  show_zones_separately <- length(zone_filter) > 1
  
  # Create filter text for title
  prehatch_filter_text <- ifelse(prehatch_only, " (Prehatch Sites Only)", "")
  zone_filter_text <- ifelse(length(zone_filter) == 2, "", 
                             ifelse(length(zone_filter) == 1, 
                                    ifelse("1" %in% zone_filter, " (P1 Only)", " (P2 Only)"), 
                                    " (No Zones)"))
  
  # Get colors from shared helper functions using the zone-aware pattern
  if (show_zones_separately && group_by == "facility") {
    # Use zone-aware facility colors for proper P1/P2 differentiation
    facility_result <- get_facility_base_colors(
      alpha_zones = zone_filter,
      combined_groups = unique(data$combined_group)
    )
    custom_colors <- facility_result$colors
    alpha_values <- facility_result$alpha_values
  } else if (show_zones_separately && group_by == "foreman") {
    # Use zone-aware foreman colors
    foreman_result <- get_foreman_colors(
      alpha_zones = zone_filter,
      combined_groups = unique(data$combined_group)
    )
    custom_colors <- foreman_result$colors  
    alpha_values <- foreman_result$alpha_values
  } else {
    # Single zone or no zone differentiation
    custom_colors <- if(group_by == "facility") {
      get_facility_base_colors()
    } else if(group_by == "foreman") {
      # Follow struct_trt pattern exactly - map foreman NUMBERS to facility-based colors
      foreman_colors <- get_foreman_colors()  # These are keyed by shortname
      foremen_lookup <- get_foremen_lookup()
      
      # Create mapping from foreman NUMBER to facility-based colors
      foremen_in_data <- unique(na.omit(data[[group_by]]))
      emp_colors <- character(0)
      
      for (foreman_num in foremen_in_data) {
        foreman_num_str <- trimws(as.character(foreman_num))
        
        # Find the shortname for this foreman number
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
        
        if(length(matches) > 0) {
          shortname <- foremen_lookup$shortname[matches[1]]
          
          # Get the facility-based color for this shortname
          if(shortname %in% names(foreman_colors)) {
            emp_colors[foreman_num_str] <- foreman_colors[shortname]
          }
        }
      }
      
      # Return emp_colors keyed by foreman numbers
      emp_colors
    } else if(group_by == "sectcode") {
      # Use foreman colors for sectcode grouping too
      get_foreman_colors()
    } else {
      get_facility_base_colors()  # Default fallback
    }
    alpha_values <- NULL
  }

  # Set fill variable and display names - EXACTLY like struct_trt
  if (show_zones_separately) {
    fill_var <- "combined_group"
    data$display_name <- data$combined_group
  } else {
    if (group_by == "facility") {
      fill_var <- "facility"
      # Add facility display names
      facilities <- get_facility_lookup()
      facility_map <- setNames(facilities$full_name, facilities$short_name)
      data$display_name <- ifelse(
        data$facility %in% names(facility_map),
        facility_map[data$facility],
        data$facility
      )
    } else if (group_by == "foreman") {
      fill_var <- "foreman"  # Use employee numbers for fill (to match color keys)
      # Create display names from employee numbers
      foremen_lookup <- get_foremen_lookup()
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      data$display_name <- ifelse(
        is.na(data$foreman) | data$foreman == "",
        "Unassigned FOS",
        foreman_map[as.character(trimws(data$foreman))]
      )
      # Handle any remaining NAs
      data$display_name <- ifelse(
        is.na(data$display_name),
        paste0("FOS #", data$foreman),
        data$display_name
      )
    } else {
      fill_var <- group_by
      data$display_name <- data[[group_by]]
    }
  }
  
  # Validate colors and add fallback
  if (is.null(custom_colors) || length(custom_colors) == 0) {
    # Fallback to basic ggplot2 colors if no custom colors available
    unique_values <- unique(data[[fill_var]])
    if (length(unique_values) > 0) {
      # Generate basic colors
      basic_colors <- rainbow(length(unique_values))
      names(basic_colors) <- unique_values
      custom_colors <- basic_colors
    } else {
      # If still no data, skip color mapping entirely
      custom_colors <- NULL
    }
  }

  if (hist_show_percentages) {
    # Show as percentages
    data <- data %>%
      group_by(year) %>%
      mutate(percentage = ifelse(sum(count) == 0, 0, round(count / sum(count) * 100, 1))) %>%
      ungroup()
    
    # Build ggplot with optional alpha mapping
    if (show_zones_separately && !is.null(alpha_values)) {
      p <- ggplot(data, aes(x = year, y = percentage, fill = .data[[fill_var]], alpha = zone_factor)) +
        geom_col(position = "stack")
    } else {
      p <- ggplot(data, aes(x = year, y = percentage, fill = .data[[fill_var]])) +
        geom_col(position = "stack")
    }
    
    # Add colors if available
    if (!is.null(custom_colors) && length(custom_colors) > 0) {
      p <- p + scale_fill_manual(values = custom_colors)
    }
    
    # Add alpha scale if available
    if (show_zones_separately && !is.null(alpha_values)) {
      p <- p + scale_alpha_manual(values = alpha_values, guide = "none")
    }
    
    p <- p + labs(
      title = paste0("Drone ", case_when(
        hist_display_metric == "treatments" ~ "Treatments",
        hist_display_metric == "acres" ~ "Acres Treated", 
        TRUE ~ "Sites Treated"
      ), " by ", case_when(
        group_by == "facility" ~ "Facility",
        group_by == "foreman" ~ "FOS",
        group_by == "sectcode" ~ "Section",
        TRUE ~ "Group"
      ), " (Percentage)", zone_filter_text, prehatch_filter_text),
      x = "Year",
      y = "Percentage (%)",
      fill = case_when(
        group_by == "facility" ~ "Facility",
        group_by == "foreman" ~ "FOS",
        group_by == "sectcode" ~ "Section",
        TRUE ~ "Group"
      )
    ) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), 1)) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  } else {
    # Show raw counts
    # Build ggplot with optional alpha mapping
    if (show_zones_separately && !is.null(alpha_values)) {
      p <- ggplot(data, aes(x = year, y = count, fill = .data[[fill_var]], alpha = zone_factor)) +
        geom_col(position = "stack")
    } else {
      p <- ggplot(data, aes(x = year, y = count, fill = .data[[fill_var]])) +
        geom_col(position = "stack")
    }
    
    # Add colors if available
    if (!is.null(custom_colors) && length(custom_colors) > 0) {
      p <- p + scale_fill_manual(values = custom_colors)
    }
    
    # Add alpha scale if available
    if (show_zones_separately && !is.null(alpha_values)) {
      p <- p + scale_alpha_manual(values = alpha_values, guide = "none")
    }
    
    p <- p + labs(
      title = paste0("Drone ", case_when(
        hist_display_metric == "treatments" ~ "Treatments",
        hist_display_metric == "acres" ~ "Acres Treated",
        TRUE ~ "Sites Treated"
      ), " by ", case_when(
        group_by == "facility" ~ "Facility",
        group_by == "foreman" ~ "FOS", 
        group_by == "sectcode" ~ "Section",
        TRUE ~ "Group"
      ), zone_filter_text, prehatch_filter_text),
      x = "Year", 
      y = case_when(
        hist_display_metric == "treatments" ~ "Number of Treatments",
        hist_display_metric == "acres" ~ "Number of Acres",
        TRUE ~ "Number of Sites"
      ),
      fill = case_when(
        group_by == "facility" ~ "Facility",
        group_by == "foreman" ~ "FOS",
        group_by == "sectcode" ~ "Section",
        TRUE ~ "Group"
      )
    ) +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), 1)) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }
  
  return(p)
}