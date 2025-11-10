# Structure Treatment - Display Functions
# Functions for creating charts and visualizations

# Function to create current progress chart
create_current_progress_chart <- function(data, group_by, facility_filter, status_types, zone_filter, combine_zones = FALSE) {
  if (nrow(data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No data available"), size = 6) +
           theme_void())
  }
  
  # Get status colors from db_helpers
  status_colors <- get_status_colors()
  
  # Prepare data for plotting
  data <- data %>%
    mutate(
      y_total = total_structures,
      y_active = active_structures,
      y_expiring = expiring_structures
    )
  
  # Determine plot group column - always use display_name for consistency
  plot_group_col <- "display_name"
  
  # Get zone and facility colors for custom coloring
  # Set up custom colors based on grouping
  custom_colors <- NULL
  if (group_by == "facility" && (length(zone_filter) == 1 || combine_zones)) {
    # Single zone OR combined zones - use basic facility colors mapped to display names
    facility_colors <- get_facility_base_colors()
    # Map facility short names to display names
    custom_colors <- character(0)
    for (i in 1:nrow(data)) {
      facility_short <- data$group_name[i]
      display_name <- data$display_name[i]
      if (facility_short %in% names(facility_colors)) {
        custom_colors[display_name] <- facility_colors[facility_short]
      }
    }
  } else if (group_by == "facility" && length(zone_filter) > 1 && !combine_zones) {
    # Multiple zones shown separately - need to map display names back to short names for colors
    facilities <- get_facility_lookup()
    facility_map <- setNames(facilities$short_name, facilities$full_name)  # reverse mapping
    
    # Extract short names from display names for color mapping
    short_groups <- sapply(unique(data$display_name), function(display_name) {
      # Extract facility name without zone, e.g., "North (P1)" -> "North"
      base_name <- gsub("\\s*\\([^)]+\\)$", "", display_name)
      base_name <- trimws(base_name)
      
      # Map back to short name, e.g., "North" -> "N"
      if (base_name %in% names(facility_map)) {
        short_name <- facility_map[base_name]
        # Recreate combined group with short name for color mapping
        zone_part <- gsub("^[^(]*", "", display_name)  # extract "(P1)" part
        return(paste0(short_name, zone_part))
      } else {
        return(display_name)  # fallback
      }
    })
    
    zone_result <- get_facility_base_colors(
      alpha_zones = zone_filter,
      combined_groups = short_groups
    )
    
    # Map the colors back to display names
    custom_colors <- character(0)
    for (i in 1:length(unique(data$display_name))) {
      display_name <- unique(data$display_name)[i]
      short_group <- short_groups[i]
      if (short_group %in% names(zone_result$colors)) {
        custom_colors[display_name] <- zone_result$colors[short_group]
      }
    }
  } else if (group_by == "foreman" && length(zone_filter) == 1) {
    # Single zone - use basic foreman colors mapped to display names
    foreman_colors <- get_foreman_colors()
    # Map foreman shortnames to display names directly
    custom_colors <- character(0)
    for (i in 1:nrow(data)) {
      display_name <- data$display_name[i]
      # The display_name is already the shortname, so use it directly
      if (display_name %in% names(foreman_colors)) {
        custom_colors[display_name] <- foreman_colors[display_name]
      }
    }
  } else if (group_by == "foreman" && length(zone_filter) > 1) {
    # Multiple zones - use zone-aware foreman colors
    zone_result <- get_foreman_colors(
      alpha_zones = zone_filter,
      combined_groups = unique(data$display_name)
    )
    custom_colors <- zone_result$colors
  }
  
  # Create a new column to determine which labels to show (avoiding overplot)
  data$show_active_label <- data$y_active != data$y_expiring
  
  # Calculate y-axis maximum for proper positioning
  y_max <- max(data$y_total) * 1.1
  
  # Set up the title with appropriate filters
  status_types_text <- paste(status_types, collapse = ", ")
  facility_text <- if (is.null(facility_filter) || ("all" %in% facility_filter)) {
    "All Facilities"
  } else {
    paste("Facilities:", paste(facility_filter, collapse = ", "))
  }
  zone_text <- if (length(zone_filter) == 0) {
    "No Zones"
  } else if (length(zone_filter) == 1) {
    paste("Zone: P", zone_filter)
  } else {
    paste("Zone:", paste0("P", zone_filter))
  }
  
  # Create the plot - use colors when available
  if (!is.null(custom_colors) && group_by != "mmcd_all") {
    p <- ggplot(data, aes(x = display_name, fill = !!sym(plot_group_col))) +
      # First draw total bars
      geom_bar(aes(y = y_total), stat = "identity", alpha = 0.3) +
      # Then overlay active bars  
      geom_bar(aes(y = y_active), stat = "identity", alpha = 0.8) +
      # Finally overlay expiring bars
      geom_bar(aes(y = y_expiring), stat = "identity", fill = status_colors["planned"]) +
      scale_fill_manual(values = custom_colors)  # Removed guide = "none" to show legend
  } else {
    # Default colors when no custom scheme available
    p <- ggplot(data, aes(x = display_name)) +
      # First draw total bars
      geom_bar(aes(y = y_total), stat = "identity", fill = "gray80", alpha = 0.7) +
      # Then overlay active bars
      geom_bar(aes(y = y_active), stat = "identity", fill = status_colors["active"]) +
      # Finally overlay expiring bars
      geom_bar(aes(y = y_expiring), stat = "identity", fill = status_colors["planned"])
  }
  
  # Add text labels with larger size
  p <- p +
    # Label total structures
    geom_text(aes(x = display_name, y = y_total, label = y_total), 
              vjust = -0.5, size = 6, fontface = "bold") +
    # Label active structures (only when different from expiring)
    geom_text(data = subset(data, show_active_label), 
              aes(x = display_name, y = y_active, label = y_active), 
              vjust = -0.5, size = 6, fontface = "bold", color = "white") +
    # Label expiring structures
    geom_text(aes(x = display_name, y = y_expiring, label = y_expiring), 
              vjust = -0.5, size = 6, fontface = "bold", color = "white") +
    coord_flip() +
    labs(
      title = sprintf("Structures with Active and Expiring Treatments (%s, Status: %s, %s)",
                     facility_text, status_types_text, zone_text),
      x = case_when(
        group_by == "facility" ~ "Facility",
        group_by == "foreman" ~ "FOS",
        group_by == "mmcd_all" ~ "MMCD",
        TRUE ~ "Group"
      ),
      y = "Number of Structures",
      fill = case_when(
        group_by == "facility" ~ "Facility",
        group_by == "foreman" ~ "FOS",
        group_by == "mmcd_all" ~ "MMCD",
        TRUE ~ "Group"
      )
    ) +
    scale_y_continuous(limits = c(0, y_max)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(face = "bold", size = 18),     # Increased from 14 to 18
      axis.title.x = element_text(face = "bold", size = 20),   # X-axis title even larger
      axis.text = element_text(size = 16, face = "bold"),      # Increased from 12 to 16
      axis.text.x = element_text(size = 18, face = "bold"),    # X-axis numbers larger (from 12 to 18)
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),  # Add some margin for better spacing
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 12)
    )
  
  return(p)
}

# Function to create historical trends chart
create_historical_trends_chart <- function(treatments_data, total_structures, start_year, end_year, group_by, facility_filter, structure_type_filter, priority_filter, status_types, zone_filter, combine_zones = FALSE) {
  if (nrow(treatments_data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No historical data available"), size = 6) +
           theme_void())
  }
  
  # Parse zone filter (same logic as in app.R)
  parsed_zones <- if (is.character(zone_filter) && length(zone_filter) == 1 && zone_filter == "combined") {
    c("1", "2")  # Include both zones but will be combined
  } else if (is.character(zone_filter) && length(zone_filter) == 1 && zone_filter == "1,2") {
    c("1", "2")  # Include both zones separately
  } else if (is.character(zone_filter)) {
    zone_filter  # Single zone
  } else {
    zone_filter  # Already parsed
  }
  
  # Create date range
  date_range <- seq(
    from = as.Date(paste0(start_year, "-01-01")),
    to = as.Date(paste0(end_year, "-12-31")),
    by = "day"
  )
  
  # Process treatments data
  treatments_data <- treatments_data %>%
    mutate(
      inspdate = as.Date(inspdate),
      # Handle NULL effect_days - use 30 days as default when NULL or 0
      effect_days = ifelse(is.na(effect_days) | effect_days == 0, 30, effect_days),
      enddate = as.Date(inspdate) + effect_days
    ) %>%
    {map_facility_names(.)}
  
  # Determine grouping based on user selection
  group_col <- case_when(
    group_by == "facility" ~ "facility",
    group_by == "foreman" ~ "foreman", 
    group_by == "mmcd_all" ~ "mmcd_all",
    TRUE ~ "mmcd_all"
  )
  
  # Add group column for mmcd_all case
  if (group_col == "mmcd_all") {
    treatments_data$mmcd_all <- "All MMCD"
  }
  
  # Handle zone-aware grouping
  if (!combine_zones && length(parsed_zones) > 1) {
    if (group_col == "mmcd_all") {
      # For mmcd_all, create zone-specific groups like "All MMCD (P1)", "All MMCD (P2)"
      treatments_data$combined_group <- paste0("All MMCD (P", treatments_data$zone, ")")
    } else {
      # Create zone-aware combined groups like "North (P1)", "North (P2)"
      treatments_data$combined_group <- paste0(treatments_data[[group_col]], " (P", treatments_data$zone, ")")
    }
    group_col_to_use <- "combined_group"
  } else {
    # Use regular grouping (single zone, combined zones, or mmcd_all)
    group_col_to_use <- group_col
  }
  
  # Get unique groups for processing
  if (group_col == "mmcd_all" && group_col_to_use == "mmcd_all") {
    # Regular mmcd_all (single zone or combined zones)
    groups_to_process <- c("All MMCD")
    group_title <- "(All MMCD)"
  } else if (group_col == "mmcd_all" && group_col_to_use == "combined_group") {
    # Zone-aware mmcd_all (P1 and P2 separate)
    groups_to_process <- unique(treatments_data[[group_col_to_use]])
    group_title <- "(All MMCD by Zone)"
  } else {
    groups_to_process <- unique(treatments_data[[group_col_to_use]])
    group_title <- case_when(
      group_col == "facility" ~ if(combine_zones) "by Facility (Combined P1+P2)" else "by Facility",
      group_col == "foreman" ~ if(combine_zones) "by FOS (Combined P1+P2)" else "by FOS",
      TRUE ~ paste("by", group_col)
    )
  }

  # Calculate unique sites with active treatments for each date and group
  treatment_trends <- map_dfr(groups_to_process, function(current_group) {
    # Filter data for current group
    group_treatments <- if (group_col == "mmcd_all" && group_col_to_use == "mmcd_all") {
      # Regular mmcd_all case
      treatments_data
    } else {
      # Zone-aware case or other groupings
      treatments_data %>% filter(!!sym(group_col_to_use) == current_group)
    }
    
    # Get total structures for this group
    group_total_structures <- if (group_col == "mmcd_all" && group_col_to_use == "mmcd_all") {
      # Regular mmcd_all case
      total_structures
    } else if (group_col == "mmcd_all" && group_col_to_use == "combined_group") {
      # Zone-aware mmcd_all case - use half the total structures for each zone
      total_structures / 2
    } else {
      # Calculate total structures for this specific group
      # This should ideally come from the get_historical_structure_data function
      # For now, use a proportional estimate based on the group's treatment data
      max(length(unique(group_treatments$sitecode)) * 2, 1)  # rough estimate, ensure > 0
    }
    
    # Calculate unique active sites for each date
    unique_active_sites_per_date <- map_dfr(date_range, function(current_date) {
      active_sites <- group_treatments %>%
        filter(
          inspdate <= current_date & 
          (is.na(enddate) | enddate > current_date)
        ) %>%
        distinct(sitecode) %>%
        nrow()
      
      data.frame(
        date = current_date,
        active_unique_sites = active_sites,
        group_name = current_group,
        group_col = group_col
      )
    })
    
    # Calculate proportions
    unique_active_sites_per_date %>%
      mutate(
        total_structures_dynamic = group_total_structures,
        proportion_active_treatment = ifelse(
          total_structures_dynamic > 0, 
          active_unique_sites / total_structures_dynamic, 
          0
        )
      )
  })

  # Ensure we have data before proceeding
  if (nrow(treatment_trends) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No data available for selected criteria"), 
                     size = 6, color = "gray50") +
           theme_void())
  }

  # Calculate seasonal average curve per group (average proportion for each calendar day across all years)
  seasonal_curve <- treatment_trends %>%
    mutate(day_of_year = format(date, "%m-%d")) %>%
    group_by(day_of_year, group_name) %>%
    summarize(
      seasonal_avg = mean(proportion_active_treatment, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Add seasonal average to main data
  treatment_trends <- treatment_trends %>%
    mutate(day_of_year = format(date, "%m-%d")) %>%
    left_join(seasonal_curve, by = c("day_of_year", "group_name"))
  
  # Set display names for different group types
  if (group_col == "mmcd_all") {
    treatment_trends$display_name <- treatment_trends$group_name
  } else if (group_col == "facility") {
    # Handle facility display names
    if ("facility_display" %in% names(treatments_data)) {
      # Create a lookup for facility display names
      facility_lookup_df <- treatments_data %>%
        distinct(facility, facility_display)
      facility_lookup <- setNames(facility_lookup_df$facility_display, facility_lookup_df$facility)
      
      if (group_col_to_use == "combined_group") {
        # Zone-aware groups like "North (P1)" -> map facility part to full name
        treatment_trends$display_name <- sapply(treatment_trends$group_name, function(combined_name) {
          # Extract facility short name from "N (P1)" -> "N"
          base_name <- gsub("\\s*\\([^)]+\\)$", "", combined_name)
          zone_part <- gsub("^[^(]*", "", combined_name)  # extract "(P1)" part
          
          if (base_name %in% names(facility_lookup)) {
            paste0(facility_lookup[base_name], zone_part)  # "North (P1)"
          } else {
            combined_name
          }
        })
      } else {
        # Regular facility grouping
        treatment_trends$display_name <- sapply(treatment_trends$group_name, function(facility) {
          if (facility %in% names(facility_lookup)) {
            facility_lookup[facility]
          } else {
            facility
          }
        })
      }
    } else {
      treatment_trends$display_name <- treatment_trends$group_name
    }
  } else if (group_col == "foreman") {
    # Handle foreman display names
    foremen_lookup <- get_foremen_lookup()
    if (group_col_to_use == "combined_group") {
      # Zone-aware FOS groups
      treatment_trends$display_name <- sapply(treatment_trends$group_name, function(combined_name) {
        # Extract emp_num from "1234 (P1)" -> "1234"
        base_name <- gsub("\\s*\\([^)]+\\)$", "", combined_name)
        zone_part <- gsub("^[^(]*", "", combined_name)  # extract "(P1)" part
        
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == trimws(as.character(base_name)))
        if(length(matches) > 0) {
          paste0(foremen_lookup$shortname[matches[1]], zone_part)
        } else {
          paste0("FOS #", combined_name)
        }
      })
    } else {
      # Regular foreman grouping
      treatment_trends$display_name <- sapply(treatment_trends$group_name, function(f) {
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == trimws(as.character(f)))
        if(length(matches) > 0) foremen_lookup$shortname[matches[1]] else paste0("FOS #", f)
      })
    }
  } else {
    treatment_trends$display_name <- treatment_trends$group_name
  }
  
  # Set up filter text
  filter_conditions <- character(0)
  if (!is.null(facility_filter) && !("all" %in% facility_filter)) {
    filter_conditions <- c(filter_conditions, paste("Facilities:", paste(facility_filter, collapse = ", ")))
  }
  if (structure_type_filter != "all") {
    filter_conditions <- c(filter_conditions, paste("Type:", structure_type_filter))
  }
  if (priority_filter != "all") {
    filter_conditions <- c(filter_conditions, paste("Priority:", priority_filter))
  }
  if (!is.null(status_types) && length(status_types) > 0) {
    filter_conditions <- c(filter_conditions, paste("Status:", paste(status_types, collapse = ", ")))
  }
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    # Handle the new zone filter format
    zone_text <- if (is.character(zone_filter)) {
      case_when(
        zone_filter == "1" ~ "Zone: P1 Only",
        zone_filter == "2" ~ "Zone: P2 Only", 
        zone_filter == "1,2" ~ "Zones: P1 and P2 Separate",
        zone_filter == "combined" ~ "Zones: Combined P1+P2",
        TRUE ~ paste("Zones:", paste0("P", zone_filter, collapse = ", "))
      )
    } else {
      if (length(zone_filter) == 1) {
        paste("Zone: P", zone_filter)
      } else {
        paste("Zones:", paste0("P", zone_filter, collapse = ", "))
      }
    }
    filter_conditions <- c(filter_conditions, zone_text)
  }
  
  filter_text <- if (length(filter_conditions) > 0) {
    paste0(" (", paste(filter_conditions, collapse = "; "), ")")
  } else {
    ""
  }
  
  # Get colors using the same logic as the current progress chart
  custom_colors <- NULL
  if (group_col == "facility" && (length(parsed_zones) == 1 || combine_zones)) {
    # Single zone OR combined zones - use basic facility colors mapped to display names
    facility_colors <- get_facility_base_colors()
    # Map facility short names to display names
    custom_colors <- character(0)
    for (i in 1:nrow(treatment_trends)) {
      facility_short <- treatment_trends$group_name[i]
      display_name <- treatment_trends$display_name[i]
      if (facility_short %in% names(facility_colors)) {
        custom_colors[display_name] <- facility_colors[facility_short]
      }
    }
  } else if (group_col == "facility" && length(parsed_zones) > 1 && !combine_zones) {
    # Multiple zones shown separately - need to map display names back to short names for colors
    facilities <- get_facility_lookup()
    facility_map <- setNames(facilities$short_name, facilities$full_name)  # reverse mapping
    
    # Extract short names from display names for color mapping
    short_groups <- sapply(unique(treatment_trends$display_name), function(display_name) {
      # Extract facility name without zone, e.g., "North (P1)" -> "North"
      base_name <- gsub("\\s*\\([^)]+\\)$", "", display_name)
      base_name <- trimws(base_name)
      
      # Map full name back to short name
      if (base_name %in% names(facility_map)) {
        facility_map[base_name]
      } else {
        base_name  # fallback
      }
    })
    
    # Get zone-aware facility colors
    zone_result <- get_facility_base_colors(
      alpha_zones = parsed_zones,
      combined_groups = unique(treatment_trends$display_name)
    )
    custom_colors <- zone_result$colors
  } else if (group_col == "foreman" && length(parsed_zones) == 1) {
    # Single zone - use basic foreman colors mapped to display names
    foreman_colors <- get_foreman_colors()
    # Map foreman shortnames to display names directly
    custom_colors <- character(0)
    for (i in 1:nrow(treatment_trends)) {
      display_name <- treatment_trends$display_name[i]
      # The display_name is already the shortname, so use it directly
      if (display_name %in% names(foreman_colors)) {
        custom_colors[display_name] <- foreman_colors[display_name]
      }
    }
  } else if (group_col == "foreman" && length(parsed_zones) > 1) {
    # Multiple zones - use zone-aware foreman colors
    zone_result <- get_foreman_colors(
      alpha_zones = parsed_zones,
      combined_groups = unique(treatment_trends$display_name)
    )
    custom_colors <- zone_result$colors
  }

  # Create the plot
  p <- ggplot(treatment_trends, aes(x = date, y = proportion_active_treatment)) +
    geom_line(aes(color = display_name), linewidth = 1.2) +
    # Add seasonal average line for each group (not just MMCD)
    {if ("seasonal_avg" %in% names(treatment_trends)) {
      geom_line(aes(y = seasonal_avg, color = display_name), linetype = "dashed", linewidth = 1.0, alpha = 0.7)
    }} +
    labs(
      title = sprintf(
        "Proportion of Structures with Active Treatment %s (%d to %d)%s",
        group_title,
        as.numeric(start_year),
        as.numeric(end_year),
        filter_text
      ),
      subtitle = "Solid: actual | Dashed: seasonal average",
      x = "Date",
      y = "Proportion of Struct with Active Treatment",
      color = case_when(
        group_col == "facility" ~ "Facility",
        group_col == "foreman" ~ "FOS",
        TRUE ~ "Group"
      )
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.01),
      breaks = seq(0, 1, by = 0.1)
    ) +
    scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b %Y",
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
      axis.text.y = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 16)
    )
  
  # Apply custom colors if available
  if (!is.null(custom_colors) && length(custom_colors) > 0) {
    p <- p + scale_color_manual(values = custom_colors)
  }
  
  return(p)
}