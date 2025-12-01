# Catch Basin Status - Display Functions
# Functions for creating charts, tables, and visualizations

library(stringr)

# Function to create status chart 
create_status_chart <- function(data, group_by = "facility", expiring_filter = "all") {
  if (is.null(data) || nrow(data) == 0) {
    plot <- ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No data available"), size = 6) +
           theme_void()
    return(plot)
  }
  
  # Get status colors from db_helpers
  status_colors <- get_status_colors()
  
  # Get appropriate group colors (same logic as ground_prehatch)
  group_colors <- NULL
  if (group_by == "facility") {
    facility_colors <- get_facility_base_colors()
    # Map facility names from display_name to get proper colors
    group_colors <- character(0)
    for (display_name in unique(data$display_name)) {
      # Extract facility code from display names - handle multiple formats
      facility_code <- display_name
      # Try removing facility suffix patterns
      facility_code <- gsub(" \\(Facility\\)", "", facility_code)
      facility_code <- gsub(" P[12]", "", facility_code)  # Remove zone suffixes
      
      # Try direct mapping first
      if (facility_code %in% names(facility_colors)) {
        group_colors[display_name] <- facility_colors[facility_code]
      } else {
        # Try finding by facility lookup for full names
        facilities <- get_facility_lookup()
        matching_facility <- facilities[facilities$full_name == facility_code, ]
        if (nrow(matching_facility) > 0) {
          short_name <- matching_facility$short_name[1]
          if (short_name %in% names(facility_colors)) {
            group_colors[display_name] <- facility_colors[short_name]
          }
        }
      }
    }
  } else if (group_by == "foreman") {
    # Map foreman employee numbers to facility-based colors 
    foreman_colors <- get_foreman_colors()
    foremen_lookup <- get_foremen_lookup()
    group_colors <- character(0)
    
    # Get foreman numbers from data for mapping
    foremen_in_data <- unique(na.omit(data$group_name))  # group_name contains the foreman number
    
    for (foreman_num in foremen_in_data) {
      foreman_num_str <- trimws(as.character(foreman_num))
      matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
      
      if (length(matches) > 0) {
        shortname <- foremen_lookup$shortname[matches[1]]
        if (shortname %in% names(foreman_colors)) {
          # Find corresponding display_name for this foreman number
          matching_rows <- data[data$group_name == foreman_num, ]
          if (nrow(matching_rows) > 0) {
            display_name <- unique(matching_rows$display_name)[1]
            group_colors[display_name] <- foreman_colors[shortname]
          }
        }
      }
    }
  }
  
  # Prepare y variables for layered bars
  # Background layer (transparent group color): ALL wet catch basins (total)
  # Active layer (solid green): Treated sites (not expiring)
  # Expiring layer (solid orange): Expiring sites
  # The transparent background shows expired + never treated portions
  data <- data %>%
    mutate(
      y_total = wet_cb_count,                                      # ALL wet catch basins (background)
      y_active = count_wet_activetrt - count_wet_expiring,         # Treated only (not expiring)
      y_expiring = count_wet_expiring                              # Just expiring sites
    )
  
  # Create layered plot with updated color scheme
  if (!is.null(group_colors) && length(group_colors) > 0 && group_by != "mmcd_all") {
    # Use group colors for background (expired + untreated), status green for active, orange for expiring
    # Ensure group_colors are unnamed to avoid plotly issues
    group_colors_clean <- unname(group_colors)
    names(group_colors_clean) <- names(group_colors)
    
    p <- ggplot(data, aes(x = reorder(display_name, y_total))) +
      geom_bar(aes(y = y_total, fill = display_name), stat = "identity", alpha = 0.4) +  # Group colors background - shows expired+untreated
      geom_bar(aes(y = y_active), stat = "identity", fill = unname(status_colors["active"]), alpha = 0.9) +  # Green treated - solid
      geom_bar(aes(y = y_expiring), stat = "identity", fill = unname(status_colors["planned"]), alpha = 1) +  # Orange expiring - solid
      scale_fill_manual(values = group_colors_clean, na.value = "grey70", guide = "none")  # Hide legend for group colors
  } else {
    # For MMCD grouping or when no specific colors available, use gray background
    p <- ggplot(data, aes(x = reorder(display_name, y_total))) +
      geom_bar(aes(y = y_total), stat = "identity", fill = "gray80", alpha = 0.4) +     # Gray background - shows expired+untreated
      geom_bar(aes(y = y_active), stat = "identity", fill = unname(status_colors["active"]), alpha = 0.9) +   # Green treated - solid
      geom_bar(aes(y = y_expiring), stat = "identity", fill = unname(status_colors["planned"]), alpha = 1)  # Orange expiring - solid
  }
  
  p <- p +
    coord_flip() +
    labs(
      title = "Catch Basin Treatment Status",
      x = case_when(
        group_by == "mmcd_all" ~ "MMCD",
        group_by == "facility" ~ "Facility",
        group_by == "foreman" ~ "FOS",
        group_by == "sectcode" ~ "Section"
      ),
      y = "Number of Wet Catch Basins"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      axis.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 13),
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11)
    )
  
  # Convert to plotly
  plot <- ggplotly(p, tooltip = "text")
  
  return(plot)
}

# Function to format data table for display
format_details_table <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame(Message = "No data available"))
  }
  
  # Calculate percentages and format - format strings AFTER calculating percentages
  display_data <- data %>%
    mutate(
      pct_coverage = round(pct_treated, 1)
    ) %>%
    select(
      Group = display_name,
      `Total Wet CB` = wet_cb_count,
      `Treated` = count_wet_activetrt,
      `Expiring` = count_wet_expiring,
      `Expired` = count_wet_expired,
      `Never Treated` = untreated_count,
      `% Coverage` = pct_coverage
    ) %>%
    mutate(
      `Total Wet CB` = format(`Total Wet CB`, big.mark = ","),
      `Treated` = format(`Treated`, big.mark = ","),
      `Expiring` = format(`Expiring`, big.mark = ","),
      `Expired` = format(`Expired`, big.mark = ","),
      `Never Treated` = format(`Never Treated`, big.mark = ",")
    )
  
  return(display_data)
}

# Function to calculate summary statistics
calculate_summary_stats <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      total_wet = 0,
      total_treated = 0,
      pct_treated = 0
    ))
  }
  
  total_wet <- sum(data$wet_cb_count, na.rm = TRUE)
  total_treated <- sum(data$count_wet_activetrt, na.rm = TRUE)
  pct_treated <- if (total_wet > 0) (total_treated / total_wet) * 100 else 0
  
  return(list(
    total_wet = total_wet,
    total_treated = total_treated,
    pct_treated = pct_treated
  ))
}
