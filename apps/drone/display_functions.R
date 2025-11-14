# Display functions for drone app - handles all visualization and plot creation

#' Create zone-separated combined groups for display
#' @param data Data frame with grouping column and zone
#' @param group_col Column name for grouping
#' @return Data frame with combined_group and zone keys added
create_zone_groups <- function(data, group_col) {
  if (group_col == "facility") {
    # Map facility names BEFORE creating combined_group
    data <- data %>% map_facility_names(facility_col = "facility")
    # Use facility_display for combined_group display, keep facility for color mapping
    data$combined_group <- paste0(data$facility_display, " (P", data$zone, ")")
    data$facility_zone_key <- paste0(data$facility, " (P", data$zone, ")")
  } else {
    # For other groupings, use the raw column values
    data$combined_group <- paste0(data[[group_col]], " (P", data$zone, ")")
  }
  
  return(data)
}

#' Get colors for visualization based on grouping type
#' @param group_by Grouping column name
#' @param data Data frame for color mapping
#' @param show_zones_separately Boolean for zone separation
#' @param zone_filter Vector of selected zones
#' @return Named vector of colors or list with colors and alpha values
get_visualization_colors <- function(group_by, data, show_zones_separately = FALSE, 
                                     zone_filter = NULL, for_historical = FALSE,
                                     sectcode_facility_mapping = NULL) {
  
  # For MMCD grouping, return no colors (will use default single color)
  if (group_by == "mmcd_all") {
    return(NULL)
  }
  
  if (group_by == "facility") {
    if (show_zones_separately && for_historical) {
      # For HISTORICAL: Use zone-aware facility colors with alpha differentiation
      facility_result <- get_facility_base_colors(
        alpha_zones = zone_filter,
        combined_groups = unique(data$combined_group)
      )
      return(facility_result$colors)
    } else {
      # For CURRENT: Use standard facility colors (no zone differentiation)
      return(get_facility_base_colors())
    }
  } else if (group_by == "foreman") {
    if (show_zones_separately && for_historical) {
      # Get zone-aware foreman colors for historical
      foreman_result <- get_foreman_colors(
        alpha_zones = zone_filter,
        combined_groups = unique(data$combined_group)
      )
      return(foreman_result$colors)
    } else {
      # Standard foreman colors - map employee numbers to facility-based colors
      foreman_colors <- get_foreman_colors()
      foremen_lookup <- get_foremen_lookup()
      
      # Create mapping from foreman NUMBER to facility-based colors
      foremen_in_data <- unique(na.omit(data[[group_by]]))
      emp_colors <- character(0)
      
      for (foreman_num in foremen_in_data) {
        foreman_num_str <- trimws(as.character(foreman_num))
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
        
        if (length(matches) > 0) {
          shortname <- foremen_lookup$shortname[matches[1]]
          if (shortname %in% names(foreman_colors)) {
            emp_colors[foreman_num_str] <- foreman_colors[shortname]
          }
        }
      }
      return(emp_colors)
    }
  } else if (group_by == "sectcode") {
    # For sectcode, use the provided sectcode-to-facility mapping
    if (!is.null(sectcode_facility_mapping) && nrow(sectcode_facility_mapping) > 0) {
      facility_colors <- get_facility_base_colors()
      sectcode_colors <- character(0)
      
      # Map each sectcode to its facility's color
      for (i in 1:nrow(sectcode_facility_mapping)) {
        sectcode_val <- sectcode_facility_mapping$sectcode[i]
        facility_val <- sectcode_facility_mapping$facility[i]
        
        if (!is.na(sectcode_val) && !is.na(facility_val) && facility_val %in% names(facility_colors)) {
          sectcode_colors[as.character(sectcode_val)] <- facility_colors[facility_val]
        }
      }
      
      return(sectcode_colors)
    } else {
      # Fallback to facility colors
      return(get_facility_base_colors())
    }
  } else {
    return(get_facility_base_colors())
  }
}

#' Process current progress data for visualization
#' @param drone_sites Data frame of drone sites
#' @param drone_treatments Data frame of drone treatments
#' @param zone_filter Vector of selected zones
#' @param combine_zones Boolean whether to combine P1+P2 into single group
#' @param expiring_days Number of days for expiring window
#' @param group_by Grouping column name
#' @param analysis_date Date to use as "current date" for analysis (defaults to today)
#' @return List with processed data and sectcode_facility_mapping
process_current_data <- function(drone_sites, drone_treatments, zone_filter, combine_zones = FALSE, expiring_days, group_by, analysis_date = Sys.Date()) {
  # Apply zone filter if selected (zone column exists after database joins)
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    drone_sites <- drone_sites %>% filter(zone %in% zone_filter)
    drone_treatments <- drone_treatments %>% filter(zone %in% zone_filter)
  }
  
  # Check if we have any data after filtering
  if (nrow(drone_sites) == 0) {
    return(list(data = data.frame(), sectcode_facility_mapping = NULL))
  }
  
  # Calculate expiring window
  current_date <- as.Date(analysis_date)
  expiring_start_date <- current_date
  expiring_end_date <- current_date + expiring_days
  
  # Update treatment status to include expiring
  if (nrow(drone_treatments) > 0) {
    drone_treatments <- drone_treatments %>%
      mutate(
        treatment_start_date = as.Date(inspdate),
        treatment_end_date = treatment_start_date + ifelse(is.na(effect_days), 14, effect_days),
        is_active = treatment_end_date >= current_date,
        is_expiring = is_active & treatment_end_date >= expiring_start_date & treatment_end_date <= expiring_end_date
      )
  } else {
    # Ensure empty treatments have the required columns for consistency
    drone_treatments$treatment_start_date <- as.Date(character(0))
    drone_treatments$treatment_end_date <- as.Date(character(0))
    drone_treatments$is_active <- logical(0)
    drone_treatments$is_expiring <- logical(0)
  }
  
  # Determine grouping and zone separation
  group_col <- group_by
  show_zones_separately <- !combine_zones && length(zone_filter) > 1
  
  # Handle MMCD grouping - aggregate everything into a single "All MMCD" group
  if (group_col == "mmcd_all") {
    # For MMCD grouping, combine all data regardless of facility/foreman/sectcode
    # but preserve zone information for proper aggregation
    if (nrow(drone_sites) > 0) {
      drone_sites$mmcd_all <- "All MMCD"
    }
    if (nrow(drone_treatments) > 0) {
      drone_treatments$mmcd_all <- "All MMCD"
    }
    group_col <- "mmcd_all"
  }
  
  # Create sectcode-to-facility mapping for color mapping (if needed)
  sectcode_facility_mapping <- NULL
  if (group_col == "sectcode") {
    sectcode_facility_mapping <- drone_sites %>%
      select(sectcode, facility) %>%
      distinct() %>%
      filter(!is.na(sectcode), !is.na(facility))
  }
  
  # Filter by foreman assignment if grouping by foreman
  if (group_col == "foreman") {
    drone_sites <- drone_sites %>% filter(!is.na(foreman) & foreman != "")
    drone_treatments <- drone_treatments %>% filter(!is.na(foreman) & foreman != "")
  }
  
  # Create zone-separated groups if needed
  if (show_zones_separately) {
    if (nrow(drone_sites) > 0) {
      drone_sites <- create_zone_groups(drone_sites, group_col)
    }
    if (nrow(drone_treatments) > 0) {
      drone_treatments <- create_zone_groups(drone_treatments, group_col)
    }
    # Update group_col to use combined_group for aggregation
    group_col <- "combined_group"
  }
  
  # Helper function to safely aggregate treatments
  safe_aggregate_treatments <- function(treatments, group_cols, filter_col = NULL) {
    if (nrow(treatments) == 0) {
      # Create empty result with proper column structure
      result_cols <- c(group_cols, "active_sites", "active_acres")
      if (!is.null(filter_col)) {
        # For expiring treatments, use expiring column names
        result_cols <- c(group_cols, "expiring_sites", "expiring_acres")
      }
      empty_result <- setNames(data.frame(matrix(nrow = 0, ncol = length(result_cols))), result_cols)
      # Set proper column types
      for (col in group_cols) {
        empty_result[[col]] <- character(0)
      }
      if (!is.null(filter_col)) {
        empty_result$expiring_sites <- numeric(0)
        empty_result$expiring_acres <- numeric(0)
      } else {
        empty_result$active_sites <- numeric(0)
        empty_result$active_acres <- numeric(0)
      }
      return(empty_result)
    }
    
    # Normal aggregation
    if (!is.null(filter_col)) {
      treatments <- treatments %>% filter(!!sym(filter_col))
      treatments %>%
        group_by(across(all_of(group_cols))) %>%
        summarize(expiring_sites = n_distinct(sitecode), expiring_acres = sum(acres, na.rm = TRUE), .groups = "drop")
    } else {
      treatments %>%
        filter(is_active) %>%
        group_by(across(all_of(group_cols))) %>%
        summarize(active_sites = n_distinct(sitecode), active_acres = sum(acres, na.rm = TRUE), .groups = "drop")
    }
  }

  # Aggregate data by grouping
  if (show_zones_separately) {
    # P1 and P2 separate bars
    total_sites <- drone_sites %>%
      group_by(combined_group, zone) %>%
      summarize(total_sites = n(), total_acres = sum(acres, na.rm = TRUE), .groups = "drop")
    
    active_treatments <- safe_aggregate_treatments(drone_treatments, c("combined_group", "zone"))
    expiring_treatments <- safe_aggregate_treatments(drone_treatments, c("combined_group", "zone"), "is_expiring")
  } else if (combine_zones && group_col != "mmcd_all") {
    # P1+P2 combined but not MMCD - combine by group but not by zone
    total_sites <- drone_sites %>%
      group_by(!!sym(group_col)) %>%
      summarize(total_sites = n(), total_acres = sum(acres, na.rm = TRUE), .groups = "drop")
    
    active_treatments <- safe_aggregate_treatments(drone_treatments, group_col)
    expiring_treatments <- safe_aggregate_treatments(drone_treatments, group_col, "is_expiring")
  } else {
    # Single zone or MMCD all grouping
    total_sites <- drone_sites %>%
      group_by(!!sym(group_col)) %>%
      summarize(total_sites = n(), total_acres = sum(acres, na.rm = TRUE), .groups = "drop")
    
    active_treatments <- safe_aggregate_treatments(drone_treatments, group_col)
    expiring_treatments <- safe_aggregate_treatments(drone_treatments, group_col, "is_expiring")
  }
  
  # Combine all data
  if (show_zones_separately) {
    # P1 and P2 separate
    combined_data <- total_sites %>%
      left_join(active_treatments, by = c("combined_group", "zone")) %>%
      left_join(expiring_treatments, by = c("combined_group", "zone"))
  } else {
    # P1+P2 combined OR single zone OR MMCD grouping
    combined_data <- total_sites %>%
      left_join(active_treatments, by = group_col) %>%
      left_join(expiring_treatments, by = group_col)
  }
  
  # Fill missing values and round acres
  combined_data <- combined_data %>%
    mutate(
      active_sites = ifelse(is.na(active_sites), 0, active_sites),
      active_acres = ifelse(is.na(active_acres), 0, round(active_acres, 2)),
      expiring_sites = ifelse(is.na(expiring_sites), 0, expiring_sites),
      expiring_acres = ifelse(is.na(expiring_acres), 0, round(expiring_acres, 2)),
      total_acres = round(total_acres, 2)
    )
  
  # Add display names and color mapping keys
  if (group_by == "mmcd_all" && !show_zones_separately) {
    # For MMCD grouping without zone separation, just use "All MMCD" as display name
    combined_data <- combined_data %>%
      mutate(display_name = "All MMCD")
  } else if (show_zones_separately) {
    combined_data <- combined_data %>%
      mutate(
        display_name = combined_group,
        zone_factor = as.character(zone)
      )
    
    if (group_by == "mmcd_all") {
      # For MMCD zone separation, the combined_group already has the right format
      # No additional processing needed, display_name is already set to combined_group
    } else if (group_by == "facility") {
      combined_data <- combined_data %>%
        mutate(
          facility = gsub(" \\(P[12]\\)", "", combined_group),
          facility_zone_key = combined_group  # Use combined_group as the zone key
        )
    } else if (group_by == "foreman") {
      # Map foreman numbers to names in zone-separated display
      foremen_lookup <- get_foremen_lookup()
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      
      combined_data <- combined_data %>%
        mutate(
          foreman_num = gsub(" \\(P[12]\\)", "", combined_group),
          zone_part = gsub(".*\\((P[12])\\)", "\\1", combined_group),
          foreman_name = ifelse(
            foreman_num %in% names(foreman_map),
            foreman_map[foreman_num],
            paste("FOS", foreman_num)
          ),
          display_name = paste0(foreman_name, " (", zone_part, ")"),
          foreman = foreman_num
        ) %>%
        select(-foreman_num, -zone_part, -foreman_name)
    }
  } else {
    if (group_col == "facility") {
      facilities <- get_facility_lookup()
      facility_map <- setNames(facilities$full_name, facilities$short_name)
      combined_data <- combined_data %>%
        mutate(display_name = ifelse(
          facility %in% names(facility_map),
          facility_map[facility],
          facility
        ))
    } else if (group_col == "foreman") {
      foremen_lookup <- get_foremen_lookup()
      foreman_map <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
      combined_data <- combined_data %>%
        mutate(display_name = ifelse(
          foreman %in% names(foreman_map),
          foreman_map[as.character(foreman)],
          paste("FOS", foreman)
        ))
    } else if (group_col == "sectcode") {
      combined_data <- combined_data %>%
        left_join(sectcode_facility_mapping, by = "sectcode") %>%
        map_facility_names(facility_col = "facility") %>%
        mutate(display_name = paste0(facility_display, " - ", sectcode)) %>%
        select(-facility, -facility_display)
    } else {
      combined_data <- combined_data %>%
        mutate(display_name = !!sym(group_col))
    }
  }
  
  # Failsafe: Ensure display_name always exists
  if (!"display_name" %in% colnames(combined_data)) {
    if (show_zones_separately) {
      combined_data$display_name <- combined_data$combined_group
    } else {
      combined_data$display_name <- combined_data[[group_col]]
    }
  }
  
  return(list(
    data = combined_data,
    sectcode_facility_mapping = sectcode_facility_mapping
  ))
}
