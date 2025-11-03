# Structure Treatment - Display Functions
# Functions for creating charts and visualizations

# Function to create current progress chart
create_current_progress_chart <- function(data, group_by, facility_filter, status_types, zone_filter) {
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
  if (group_by == "facility" && length(zone_filter) == 1) {
    # Single zone - use basic facility colors mapped to display names
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
  } else if (group_by == "facility" && length(zone_filter) > 1) {
    # Multiple zones - use zone-aware colors
    zone_result <- get_facility_base_colors(
      alpha_zones = zone_filter,
      combined_groups = unique(data$display_name)
    )
    custom_colors <- zone_result$colors
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
      scale_fill_manual(values = custom_colors, guide = "none")
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
  
  # Add text labels
  p <- p +
    # Label total structures
    geom_text(aes(x = display_name, y = y_total, label = y_total), 
              vjust = -0.5, size = 3, fontface = "bold") +
    # Label active structures (only when different from expiring)
    geom_text(data = subset(data, show_active_label), 
              aes(x = display_name, y = y_active, label = y_active), 
              vjust = -0.5, size = 3, fontface = "bold", color = "white") +
    # Label expiring structures
    geom_text(aes(x = display_name, y = y_expiring, label = y_expiring), 
              vjust = -0.5, size = 3, fontface = "bold", color = "white") +
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
      y = "Number of Structures"
    ) +
    scale_y_continuous(limits = c(0, y_max)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# Function to create historical trends chart
create_historical_trends_chart <- function(treatments_data, total_structures, start_year, end_year, group_by, facility_filter, structure_type_filter, priority_filter, status_types, zone_filter) {
  if (nrow(treatments_data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No historical data available"), size = 6) +
           theme_void())
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
      enddate = as.Date(inspdate) + effect_days
    ) %>%
    {map_facility_names(.)}
  
  # Determine grouping
  group_col <- case_when(
    group_by == "facility" ~ "facility",
    group_by == "foreman" ~ "foreman",
    group_by == "mmcd_all" ~ "mmcd_all",
    TRUE ~ "facility"
  )
  
  # Add group column for mmcd_all case
  if (group_col == "mmcd_all") {
    treatments_data$mmcd_all <- "All MMCD"
  }
  
  # Create treatment changes for each group
  if (group_col == "mmcd_all") {
    # Calculate daily treatment changes for all MMCD
    treatment_starts <- treatments_data %>%
      group_by(inspdate) %>%
      summarize(treatment_change = n(), .groups = 'drop') %>%
      rename(date = inspdate)
    
    treatment_ends <- treatments_data %>%
      group_by(enddate) %>%
      summarize(treatment_change = -n(), .groups = 'drop') %>%
      rename(date = enddate)
    
    daily_treatment_changes <- bind_rows(treatment_starts, treatment_ends) %>%
      group_by(date) %>%
      summarize(treatment_change = sum(treatment_change), .groups = 'drop')
    
    # Calculate structure changes (assuming relatively stable for this timeframe)
    structure_changes <- data.frame(date = as.Date(character(0)), structure_change = numeric(0))
    
    # Calculate cumulative active treatments and total structures over time
    all_dates <- data.frame(date = date_range)
    treatment_trends <- all_dates %>%
      left_join(daily_treatment_changes, by = "date") %>%
      left_join(structure_changes, by = "date") %>%
      mutate(
        treatment_change = ifelse(is.na(treatment_change), 0, treatment_change),
        structure_change = ifelse(is.na(structure_change), 0, structure_change)
      ) %>%
      arrange(date) %>%
      mutate(
        active_treatments = cumsum(treatment_change),
        total_structures_dynamic = total_structures + cumsum(structure_change)
      ) %>%
      # Avoid division by zero
      mutate(
        proportion_active_treatment = ifelse(
          total_structures_dynamic > 0, 
          active_treatments / total_structures_dynamic, 
          0
        ),
        group_name = "All MMCD"
      )

    # Calculate seasonal average curve (average proportion for each calendar day across all years)
    seasonal_curve <- treatment_trends %>%
      mutate(day_of_year = format(date, "%m-%d")) %>%
      group_by(day_of_year) %>%
      summarize(
        seasonal_avg = mean(proportion_active_treatment, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Add seasonal average to main data
    treatment_trends <- treatment_trends %>%
      mutate(day_of_year = format(date, "%m-%d")) %>%
      left_join(seasonal_curve, by = "day_of_year")
    
    # Set display name
    treatment_trends$display_name <- treatment_trends$group_name
  } else {
    # Handle facility or foreman grouping (simplified for now)
    treatment_trends <- data.frame(
      date = date_range,
      proportion_active_treatment = 0,
      group_name = "Data",
      display_name = "Data"
    )
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
  if (length(zone_filter) > 0) {
    zone_text <- if (length(zone_filter) == 1) {
      paste("Zone: P", zone_filter)
    } else {
      paste("Zones:", paste0("P", zone_filter, collapse = ", "))
    }
    filter_conditions <- c(filter_conditions, zone_text)
  }
  
  filter_text <- if (length(filter_conditions) > 0) {
    paste0(" (", paste(filter_conditions, collapse = "; "), ")")
  } else {
    ""
  }
  
  # Get group colors
  group_colors <- NULL
  if (group_col == "facility") {
    group_colors <- get_facility_base_colors()
  }
  
  # Set display names for non-MMCD groupings
  if (group_col != "mmcd_all") {
    if (group_col == "facility") {
      treatment_trends$display_name <- treatment_trends$group_name
    } else if (group_col == "foreman") {
      foremen_lookup <- get_foremen_lookup()
      treatment_trends$display_name <- sapply(treatment_trends$group_name, function(f) {
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == trimws(as.character(f)))
        if(length(matches) > 0) foremen_lookup$shortname[matches[1]] else paste0("FOS #", f)
      })
    } else {
      treatment_trends$display_name <- treatment_trends$group_name
    }
  }
  
  # Create the plot
  group_title <- case_when(
    group_col == "facility" ~ "by Facility",
    group_col == "foreman" ~ "by FOS", 
    group_col == "mmcd_all" ~ "(All MMCD)",
    TRUE ~ paste("by", group_col)
  )
  
  p <- ggplot(treatment_trends, aes(x = date, y = proportion_active_treatment)) +
    geom_line(aes(color = display_name), size = 1.2) +
    # Add seasonal average line for MMCD (All) grouping
    {if (group_col == "mmcd_all" && "seasonal_avg" %in% names(treatment_trends)) {
      geom_line(aes(y = seasonal_avg), color = "red", linetype = "dashed", size = 1.2)
    }} +
    labs(
      title = sprintf(
        "Proportion of Structures with Active Treatment %s (%d to %d)%s",
        group_title,
        as.numeric(start_year),
        as.numeric(end_year),
        filter_text
      ),
      subtitle = if (group_col == "mmcd_all") "Blue: actual | Red dashed: seasonal average" else NULL,
      x = "Date",
      y = "Proportion of Active Treatment",
      color = case_when(
        group_col == "facility" ~ "Facility",
        group_col == "foreman" ~ "FOS",
        TRUE ~ "Group"
      )
    ) +
    scale_y_continuous(
      labels = scales::percent_format(),
      breaks = seq(0, 1, by = 0.1)
    ) +
    scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b %Y",
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  # Apply custom colors if available
  if (!is.null(group_colors) && length(group_colors) > 0) {
    # Create color mapping for display names
    display_colors <- character(0)
    for (i in seq_len(nrow(treatment_trends))) {
      group_val <- treatment_trends$group_name[i]
      display_val <- treatment_trends$display_name[i]
      if (group_val %in% names(group_colors)) {
        display_colors[display_val] <- group_colors[group_val]
      }
    }
    display_colors <- display_colors[!duplicated(names(display_colors))]
    
    if (length(display_colors) > 0) {
      p <- p + scale_color_manual(values = display_colors)
    }
  }
  
  return(p)
}