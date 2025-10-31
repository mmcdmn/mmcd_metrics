# Site average functions

# Source the historical functions to access shared data
source("historical_functions.R")

# Site average data processing
get_site_average_data <- function(hist_start_year, hist_end_year, drone_types, zone_filter, facility_filter, foreman_filter, prehatch_only, group_by) {
  # Get raw data using shared function
  data_list <- get_shared_historical_data(hist_start_year, hist_end_year, drone_types)
  if (is.null(data_list)) {
    return(data.frame())
  }
  
  # Combine archive and current data
  archive_data <- data_list$archive %>% mutate(source = "Archive")
  current_data <- data_list$current %>% mutate(source = "Current")
  all_data <- bind_rows(archive_data, current_data)
  
  if (nrow(all_data) == 0) {
    return(data.frame())
  }
  
  # Join with site size data (including prehatch info)
  site_data <- all_data %>%
    inner_join(data_list$lbs, by = "sitecode") %>%
    mutate(year = year(inspdate))
  
  # Handle potential duplicate facility columns from join
  if ("facility.x" %in% names(site_data)) {
    site_data <- site_data %>%
      mutate(facility = facility.x) %>%
      select(-facility.x)
  }
  if ("facility.y" %in% names(site_data)) {
    site_data <- site_data %>%
      select(-facility.y)
  }
  
  # Apply zone filter if selected
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    site_data <- site_data %>%
      filter(zone %in% zone_filter)
  }
  
  # Apply facility filter if selected
  if (!is.null(facility_filter) && length(facility_filter) > 0 && 
      !("all" %in% facility_filter)) {
    site_data <- site_data %>%
      filter(facility %in% facility_filter)
  }
  
  # Apply foreman/FOS filter if selected
  if (!is.null(foreman_filter) && length(foreman_filter) > 0 && 
      !("all" %in% foreman_filter)) {
    # Convert shortnames to employee numbers for filtering
    foremen_lookup <- get_foremen_lookup()
    selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% foreman_filter]
    
    site_data <- site_data %>%
      filter(foreman %in% selected_emp_nums)
  }
  
  # Apply prehatch filter if selected
  if (prehatch_only) {
    site_data <- site_data %>%
      filter(prehatch == 'PREHATCH')
  }
  
  # Calculate average acres by grouping and year
  show_zones_separately <- length(zone_filter) > 1
  
  # Determine grouping column
  group_col <- group_by
  if (group_col == "foreman") {
    # Only include sites that have a foreman assigned
    site_data <- site_data %>% filter(!is.na(foreman) & foreman != "")
  }
  
  # Apply grouping logic consistent with main historical processing
  if (show_zones_separately) {
    if (group_col == "facility") {
      # Create reverse mapping from full names back to short codes for color mapping
      facilities_lookup <- get_facility_lookup()
      facility_reverse_map <- setNames(facilities_lookup$short_name, facilities_lookup$full_name)
      
      site_data <- site_data %>%
        group_by(facility, zone, year) %>%
        summarize(avg_acres = ifelse(all(is.na(acres)), 0, mean(acres, na.rm = TRUE)), .groups = "drop") %>%
        map_facility_names(facility_col = "facility") %>%
        mutate(
          combined_group = paste0(facility_display, " (P", zone, ")"),
          zone_factor = as.character(zone),
          # Create facility_zone_key using SHORT codes for color mapping
          facility_zone_key = paste0(facility, " (P", zone, ")")
        )
    } else if (group_col == "foreman") {
      # Map foreman numbers to shortnames for display
      foremen_lookup <- get_foremen_lookup()
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      
      site_data <- site_data %>%
        group_by(foreman, zone, year) %>%
        summarize(avg_acres = ifelse(all(is.na(acres)), 0, mean(acres, na.rm = TRUE)), .groups = "drop") %>%
        mutate(
          foreman_display = ifelse(
            foreman %in% names(foreman_map),
            foreman_map[foreman],
            paste("Unknown FOS", foreman)
          ),
          combined_group = paste0(foreman_display, " (P", zone, ")"),
          zone_factor = as.character(zone)
        ) %>%
        select(-foreman_display)
    } else if (group_col == "sectcode") {
      site_data <- site_data %>%
        group_by(facility, sectcode, zone, year) %>%
        summarize(avg_acres = ifelse(all(is.na(acres)), 0, mean(acres, na.rm = TRUE)), .groups = "drop") %>%
        map_facility_names(facility_col = "facility") %>%
        mutate(
          sectcode_display = paste0(facility_display, " - ", sectcode),
          combined_group = paste0(sectcode_display, " (P", zone, ")"),
          zone_factor = as.character(zone)
        ) %>%
        select(-sectcode_display)
    }
  } else {
    # Single zone processing
    if (group_col == "facility") {
      site_data <- site_data %>%
        group_by(facility, year) %>%
        summarize(avg_acres = ifelse(all(is.na(acres)), 0, mean(acres, na.rm = TRUE)), .groups = "drop") %>%
        map_facility_names(facility_col = "facility")
    } else if (group_col == "foreman") {
      # Map foreman numbers to shortnames for display
      foremen_lookup <- get_foremen_lookup()
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      
      site_data <- site_data %>%
        group_by(foreman, year) %>%
        summarize(avg_acres = ifelse(all(is.na(acres)), 0, mean(acres, na.rm = TRUE)), .groups = "drop") %>%
        mutate(
          foreman_display = ifelse(
            foreman %in% names(foreman_map),
            foreman_map[foreman],
            paste("Unknown FOS", foreman)
          )
        )
    } else if (group_col == "sectcode") {
      site_data <- site_data %>%
        group_by(facility, sectcode, year) %>%
        summarize(avg_acres = ifelse(all(is.na(acres)), 0, mean(acres, na.rm = TRUE)), .groups = "drop") %>%
        map_facility_names(facility_col = "facility") %>%
        mutate(sectcode_display = paste0(facility_display, " - ", sectcode))
    }
  }
  
  return(site_data)
}

# Site average plot creation
create_site_average_plot <- function(zone_filter, facility_filter, foreman_filter, prehatch_only, group_by, hist_start_year, hist_end_year, drone_types) {
  site_data <- get_site_average_data(hist_start_year, hist_end_year, drone_types, zone_filter, facility_filter, foreman_filter, prehatch_only, group_by)
  
  if (nrow(site_data) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "No treatment data available", cex = 1.5)
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
  
  # Get colors from shared helper functions using the same pattern as historical
  if (show_zones_separately && group_by == "facility") {
    # Use zone-aware facility colors for proper P1/P2 differentiation
    facility_result <- get_facility_base_colors(
      alpha_zones = zone_filter,
      combined_groups = unique(site_data$combined_group)
    )
    custom_colors <- facility_result$colors
    alpha_values <- facility_result$alpha_values
  } else if (show_zones_separately && group_by == "foreman") {
    # Use zone-aware foreman colors
    foreman_result <- get_foreman_colors(
      alpha_zones = zone_filter,
      combined_groups = unique(site_data$combined_group)
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
      foremen_in_data <- unique(na.omit(site_data$foreman))
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
  
  # Determine the appropriate color variable based on zone separation and grouping
  if (show_zones_separately) {
    if (group_by == "facility") {
      color_var <- "facility_zone_key"  # Use short codes for color mapping
    } else {
      color_var <- "combined_group"
    }
  } else {
    # Set color variable based on grouping
    if (group_by == "facility") {
      color_var <- "facility"
    } else if (group_by == "foreman") {
      color_var <- "foreman"  # Use employee numbers for color mapping (to match struct_trt pattern)
    } else {
      color_var <- "facility"  # Use facility for sectcode grouping colors
    }
  }

  # Validate colors and add fallback for site avg graph
  if (is.null(custom_colors) || length(custom_colors) == 0) {
    # Fallback to basic ggplot2 colors if no custom colors available
    unique_values <- unique(site_data[[color_var]])
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
  
  # Determine legend label based on grouping
  legend_label <- case_when(
    group_by == "facility" ~ "Facility",
    group_by == "foreman" ~ "FOS",
    group_by == "sectcode" ~ "Section",
    TRUE ~ "Group"
  )
  
  # Build the plot with optional alpha mapping
  if (show_zones_separately && !is.null(alpha_values)) {
    p <- ggplot(site_data, aes(x = year, y = avg_acres, color = .data[[color_var]], alpha = zone_factor)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3)
  } else {
    p <- ggplot(site_data, aes(x = year, y = avg_acres, color = .data[[color_var]])) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3)
  }
  
  # Add colors if available
  if (!is.null(custom_colors) && length(custom_colors) > 0) {
    p <- p + scale_color_manual(values = custom_colors)
  }
  
  # Add alpha transparency for zone differentiation
  if (show_zones_separately && !is.null(alpha_values)) {
    p <- p + scale_alpha_manual(
      name = "Zone",
      values = alpha_values,
      labels = c("1" = "P1 (Solid)", "2" = "P2 (Faded)"),
      drop = FALSE
    )
  }
  
  p <- p + labs(
      title = paste0("Average Site Size Treated by Drone (Acres) by ", legend_label, zone_filter_text, prehatch_filter_text),
      x = "Year",
      y = "Average Acres",
      color = legend_label
    ) +
    scale_x_continuous(breaks = seq(min(site_data$year), max(site_data$year), 1)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

# Site extremes table data
get_site_extremes_data <- function(hist_start_year, hist_end_year, drone_types, zone_filter, facility_filter, foreman_filter, prehatch_only) {
  # Get raw data using shared function
  data_list <- get_shared_historical_data(hist_start_year, hist_end_year, drone_types)
  if (is.null(data_list)) {
    return(data.frame(Message = "No data available"))
  }
  
  # Combine archive and current data
  archive_data <- data_list$archive %>% mutate(source = "Archive")
  current_data <- data_list$current %>% mutate(source = "Current")
  all_data <- bind_rows(archive_data, current_data)
  
  if (nrow(all_data) == 0) {
    return(data.frame(Message = "No treatment data available"))
  }
  
  # Join with site size data (including prehatch info)
  site_data <- all_data %>%
    inner_join(data_list$lbs, by = "sitecode")
  
  # Handle potential duplicate facility columns from join
  if ("facility.x" %in% names(site_data) && "facility.y" %in% names(site_data)) {
    site_data <- site_data %>%
      mutate(facility = coalesce(facility.x, facility.y)) %>%
      select(-facility.x, -facility.y)
  } else if ("facility.x" %in% names(site_data)) {
    site_data <- site_data %>%
      rename(facility = facility.x)
  } else if ("facility.y" %in% names(site_data)) {
    site_data <- site_data %>%
      rename(facility = facility.y)
  }
  
  # Select the needed columns, handling cases where some might not exist
  site_data <- site_data %>%
    select(any_of(c("sitecode", "acres", "facility", "prehatch", "zone", "foreman"))) %>%
    distinct()
  
  # Apply zone filter if selected
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    site_data <- site_data %>%
      filter(zone %in% zone_filter)
  }
  
  # Apply facility filter if selected
  if (!is.null(facility_filter) && length(facility_filter) > 0 && 
      !("all" %in% facility_filter)) {
    site_data <- site_data %>%
      filter(facility %in% facility_filter)
  }
  
  # Apply foreman/FOS filter if selected
  if (!is.null(foreman_filter) && length(foreman_filter) > 0 && 
      !("all" %in% foreman_filter)) {
    # Convert shortnames to employee numbers for filtering
    foremen_lookup <- get_foremen_lookup()
    selected_emp_nums <- foremen_lookup$emp_num[foremen_lookup$shortname %in% foreman_filter]
    
    site_data <- site_data %>%
      filter(foreman %in% selected_emp_nums)
  }
  
  # Apply prehatch filter if selected
  if (prehatch_only) {
    site_data <- site_data %>%
      filter(prehatch == 'PREHATCH')
  }
  
  # Remove prehatch, zone, and foreman columns for final output
  site_data <- site_data %>%
    select(-prehatch, -zone, -foreman)
  
  if (nrow(site_data) == 0) {
    return(data.frame(Message = "No site size data available"))
  }
  
  # Find extremes
  largest_sites <- site_data %>%
    arrange(desc(acres)) %>%
    head(5) %>%
    mutate(Category = "Largest Sites") %>%
    select(Category, sitecode, acres, facility)
  
  smallest_sites <- site_data %>%
    arrange(acres) %>%
    head(5) %>%
    mutate(Category = "Smallest Sites") %>%
    select(Category, sitecode, acres, facility)
  
  # Combine
  result <- bind_rows(largest_sites, smallest_sites) %>%
    rename(
      Type = Category,
      `Site Code` = sitecode,
      `Acres` = acres,
      `Facility` = facility
    )
  
  return(result)
}