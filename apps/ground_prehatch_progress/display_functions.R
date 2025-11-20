# Ground Prehatch Progress - Display Functions
# Functions for creating charts, tables, and value boxes

library(stringr)  # For str_to_title function
library(leaflet)  # For map functionality
library(sf)       # For spatial data handling

# Function to create progress chart exactly like drone app with layered bars
create_progress_chart <- function(data, group_by, expiring_filter = "all", expiring_days = 14) {
  if (nrow(data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No data available"), size = 6) +
           theme_void())
  }
  
  # Get status colors from db_helpers
  status_colors <- get_status_colors()
  
  # Get appropriate group colors for active sites (same logic as drone app)
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
    # Map foreman employee numbers to facility-based colors (same as drone app)
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

  # Prepare y variables for layered bars - FIXED TO INCLUDE ALL PREHATCH SITES
  # Gray background: ALL prehatch sites (treated + expiring + expired + untreated)
  # Blue bar: Active + Expiring sites (fills up portion of gray background)  
  # Yellow overlay: Just expiring sites (on top of blue)
  data <- data %>%
    mutate(
      y_total = prehatch_sites_cnt,                                # ALL prehatch sites (gray background)
      y_active = ph_treated_cnt + ph_expiring_cnt,                 # Active + Expiring (blue bar)
      y_expiring = ph_expiring_cnt                                 # Just Expiring (yellow overlay)
    )

  # Apply filtering if needed - FIXED LOGIC
  if (expiring_filter == "expiring") {
    # Show only expiring sites
    data <- data %>%
      mutate(
        y_total = ph_expiring_cnt,     # Total = just expiring sites
        y_active = ph_expiring_cnt,    # Blue bar = expiring sites (fills total)
        y_expiring = ph_expiring_cnt   # Yellow overlay = same expiring sites
      )
  } else if (expiring_filter == "expiring_expired") {
    # Show expiring + expired sites
    data <- data %>%
      mutate(
        y_total = ph_expiring_cnt + ph_expired_cnt,   # Total = expiring + expired
        y_active = ph_expiring_cnt,                   # Blue bar = just expiring 
        y_expiring = ph_expiring_cnt                  # Yellow overlay = expiring
      )
  }

  # Create title based on filters
  chart_title <- if (expiring_filter == "expiring") {
    paste("Ground Prehatch Expiring Sites (Within", expiring_days, "Days)")
  } else if (expiring_filter == "expiring_expired") {
    paste("Ground Prehatch Expiring + Expired Sites (Expiring Within", expiring_days, "Days)")
  } else {
    "Ground Prehatch Treatment Progress"
  }

  # Create layered plot exactly like drone app
  if (!is.null(group_colors) && length(group_colors) > 0 && group_by != "mmcd_all") {
    # Use group colors for active sites (facility or foreman colors)
    p <- ggplot(data, aes(x = reorder(display_name, y_total))) +
      geom_bar(aes(y = y_total), stat = "identity", fill = "gray80", alpha = 0.7) +  # Gray background
      geom_bar(aes(y = y_active, fill = display_name), stat = "identity", alpha = 0.8) +  # Group colors
      geom_bar(aes(y = y_expiring), stat = "identity", fill = status_colors["planned"]) +  # Orange overlay
      scale_fill_manual(values = group_colors, na.value = "grey70", guide = "none")  # Hide legend for group colors
  } else {
    # For MMCD grouping or when no specific colors available, use status colors only
    p <- ggplot(data, aes(x = reorder(display_name, y_total))) +
      geom_bar(aes(y = y_total), stat = "identity", fill = "gray80", alpha = 0.7) +     # Gray background
      geom_bar(aes(y = y_active), stat = "identity", fill = status_colors["active"]) +   # Green active
      geom_bar(aes(y = y_expiring), stat = "identity", fill = status_colors["planned"])  # Orange expiring
  }
  
  p <- p +
    coord_flip() +
    labs(
      title = chart_title,
      x = case_when(
        group_by == "mmcd_all" ~ "MMCD",
        group_by == "facility" ~ "Facility",
        group_by == "foreman" ~ "FOS",
        group_by == "sectcode" ~ "Section"
      ),
      y = "Number of Sites"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      axis.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 13),
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11)
    )
  
  return(ggplotly(p, tooltip = c("x", "y", "fill")))
}

# Function to create historical chart with multiple chart types (matching drone app exactly)
create_historical_chart <- function(data, hist_time_period, hist_display_metric, hist_group_by, chart_type = "stacked_bar") {
  if (nrow(data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No historical data available"), size = 6) +
           theme_void())
  }
  
  # Create chart title
  metric_label <- switch(hist_display_metric,
    "treatments" = "Treatments",
    "sites" = "Sites Treated", 
    "acres" = "Acres Treated"
  )
  
  period_label <- switch(hist_time_period,
    "weekly" = "Weekly",
    "yearly" = "Yearly"
  )
  
  chart_title <- paste("Historical Ground Prehatch", metric_label, "-", period_label, "View")
  
  # Sort time periods properly and rename columns to match drone pattern
  if (hist_time_period == "weekly") {
    data <- data %>%
      arrange(time_period) %>%
      mutate(group_label = display_name, value = count)
  } else {
    data <- data %>%
      mutate(time_period = as.numeric(time_period)) %>%
      arrange(time_period) %>%
      mutate(group_label = display_name, value = count)
  }
  
  # Get colors based on grouping (same logic as drone app)
  colors <- NULL
  if (hist_group_by == "facility") {
    facility_colors <- get_facility_base_colors()
    # Map facility names to colors
    color_mapping <- character(0)
    for (display_name in unique(data$group_label)) {
      # Extract facility code from display names like "Sj P1", "Wm (Facility)", etc.
      facility_code <- gsub(" .*", "", display_name)  # Take first part before space
      if (facility_code %in% names(facility_colors)) {
        color_mapping[display_name] <- facility_colors[facility_code]
      }
    }
    colors <- color_mapping
  } else if (hist_group_by == "foreman") {
    foreman_colors <- get_foreman_colors()
    # Map foreman names to colors
    color_mapping <- character(0)
    for (display_name in unique(data$group_label)) {
      # Extract foreman name from display names like "Smith (FOS)", "Smith P1", etc.
      foreman_name <- gsub(" \\(FOS\\)| P[12]", "", display_name)  # Remove suffixes
      if (foreman_name %in% names(foreman_colors)) {
        color_mapping[display_name] <- foreman_colors[foreman_name]
      }
    }
    colors <- color_mapping
  }
  
  # Create the plot based on chart type (exactly like drone app)
  if (chart_type == "bar" || chart_type == "stacked_bar" || chart_type == "grouped_bar") {
    p <- ggplot(data, aes(x = as.factor(time_period), y = value, fill = group_label)) +
      geom_col(position = if(chart_type == "grouped_bar") "dodge" else "stack", alpha = 0.8) +
      labs(
        title = chart_title,
        x = "Time Period",
        y = paste(stringr::str_to_title(hist_display_metric)),
        fill = stringr::str_to_title(hist_group_by)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 11),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Apply custom colors if available
    if (!is.null(colors) && length(colors) > 0) {
      p <- p + scale_fill_manual(values = colors, na.value = "grey50")
    }
    
  } else if (chart_type == "area") {
    p <- ggplot(data, aes(x = as.numeric(as.factor(time_period)), y = value, fill = group_label)) +
      geom_area(alpha = 0.7, position = "stack") +
      scale_x_continuous(breaks = seq_along(unique(data$time_period)), 
                        labels = unique(data$time_period)) +
      labs(
        title = chart_title,
        x = "Time Period",
        y = paste(stringr::str_to_title(hist_display_metric)),
        fill = stringr::str_to_title(hist_group_by)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 11),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Apply custom colors if available
    if (!is.null(colors) && length(colors) > 0) {
      p <- p + scale_fill_manual(values = colors, na.value = "grey50")
    }
    
  } else if (chart_type == "step") {
    p <- ggplot(data, aes(x = as.numeric(as.factor(time_period)), y = value, color = group_label, group = group_label)) +
      geom_step(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = seq_along(unique(data$time_period)), 
                        labels = unique(data$time_period)) +
      labs(
        title = chart_title,
        x = "Time Period",
        y = paste(stringr::str_to_title(hist_display_metric)),
        color = stringr::str_to_title(hist_group_by)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 11),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Apply custom colors if available
    if (!is.null(colors) && length(colors) > 0) {
      p <- p + scale_color_manual(values = colors, na.value = "grey50")
    }
    
  } else {
    # Line chart (default)
    p <- ggplot(data, aes(x = time_period, y = value, color = group_label, group = group_label)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      labs(
        title = chart_title,
        x = ifelse(hist_time_period == "weekly", "Week", "Year"),
        y = paste(stringr::str_to_title(hist_display_metric)),
        color = stringr::str_to_title(hist_group_by)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 11),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Apply custom colors if available
    if (!is.null(colors) && length(colors) > 0) {
      p <- p + scale_color_manual(values = colors, na.value = "grey50")
    }
  }
  
  return(ggplotly(p, tooltip = c("x", "y", if(chart_type %in% c("line", "step")) "colour" else "fill")))
}

# Function to create all value boxes (like red_air does)
create_value_boxes <- function(data) {
  # Calculate totals
  total_ground <- sum(data$tot_ground, na.rm = TRUE)
  total_prehatch <- sum(data$prehatch_sites_cnt, na.rm = TRUE)
  total_treated <- sum(data$ph_treated_cnt, na.rm = TRUE)
  total_expired <- sum(data$ph_expired_cnt, na.rm = TRUE)
  total_expiring <- sum(data$ph_expiring_cnt, na.rm = TRUE)
  
  # Calculate percentages
  treated_pct <- if (total_prehatch > 0) round(100 * total_treated / total_prehatch, 1) else 0
  expiring_pct <- if (total_prehatch > 0) round(100 * total_expiring / total_prehatch, 1) else 0
  
  return(list(
    total_ground = total_ground,
    total_prehatch = total_prehatch,
    total_treated = total_treated,
    total_expired = total_expired,
    treated_pct = treated_pct,
    expiring_pct = expiring_pct
  ))
}

# Function to create details table
create_details_table <- function(data, foremen_lookup) {
  if (nrow(data) == 0) {
    return(datatable(
      data.frame(Message = "No data available with current filters"),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ))
  }
  
  # Map fosarea (empnum) to foreman names
  data$foreman_name <- NA
  for (i in 1:nrow(data)) {
    if (!is.na(data$fosarea[i])) {
      foreman_row <- foremen_lookup[foremen_lookup$emp_num == data$fosarea[i], ]
      if (nrow(foreman_row) > 0) {
        data$foreman_name[i] <- foreman_row$shortname[1]
      } else {
        data$foreman_name[i] <- data$fosarea[i]  # Fallback to empnum
      }
    }
  }
  
  # Format data for display
  display_data <- data %>%
    select(
      Facility = facility_display,
      `Priority Zone` = zone,
      Section = sectcode,
      Sitecode = sitecode,
      FOS = foreman_name,
      Acres = acres,
      Priority = priority,
      `Treatment Type` = prehatch,
      Status = prehatch_status,
      `Last Treatment` = inspdate,
      `Days Since Last Treatment` = age,
      Material = matcode,
      `Effect Days` = effect_days
    ) %>%
    arrange(Facility, Section, Sitecode)
  
  # Round numeric columns
  if ("Days Since Last Treatment" %in% names(display_data)) {
    display_data$`Days Since Last Treatment` <- round(as.numeric(display_data$`Days Since Last Treatment`), 1)
  }
  if ("Acres" %in% names(display_data)) {
    display_data$Acres <- round(as.numeric(display_data$Acres), 2)
  }
  
  datatable(
    display_data,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      columnDefs = list(
        list(className = 'dt-center', targets = 5:12)
      )
    ),
    rownames = FALSE
  )
}

# Function to prepare download data
prepare_download_data <- function(data, foremen_lookup) {
  if (nrow(data) == 0) {
    return(data.frame(Message = "No data available with current filters"))
  }
  
  # Map fosarea (empnum) to foreman names
  data$foreman_name <- NA
  for (i in 1:nrow(data)) {
    if (!is.na(data$fosarea[i])) {
      foreman_row <- foremen_lookup[foremen_lookup$emp_num == data$fosarea[i], ]
      if (nrow(foreman_row) > 0) {
        data$foreman_name[i] <- foreman_row$shortname[1]
      } else {
        data$foreman_name[i] <- data$fosarea[i]  # Fallback to empnum
      }
    }
  }
  
  # Format for download (same as displayed data)
  download_data <- data %>%
    select(
      Facility = facility_display,
      `Priority Zone` = zone,
      Section = sectcode,
      Sitecode = sitecode,
      FOS = foreman_name,
      Acres = acres,
      Priority = priority,
      `Treatment Type` = prehatch,
      Status = prehatch_status,
      `Last Treatment` = inspdate,
      `Days Since Last Treatment` = age,
      Material = matcode,
      `Effect Days` = effect_days
    ) %>%
    arrange(Facility, Section, Sitecode)
  
  # Round numeric columns
  if ("Days Since Last Treatment" %in% names(download_data)) {
    download_data$`Days Since Last Treatment` <- round(as.numeric(download_data$`Days Since Last Treatment`), 1)
  }
  if ("Acres" %in% names(download_data)) {
    download_data$Acres <- round(as.numeric(download_data$Acres), 2)
  }
  
  return(download_data)
}

# Function to create ground prehatch map (simplified like drone app)
create_ground_map <- function(spatial_data, basemap = "carto", site_filter = "all") {
  # Robust null/empty data checking
  if (is.null(spatial_data) || !inherits(spatial_data, "sf") || nrow(spatial_data) == 0) {
    # Return empty map with explanation
    return(leaflet() %>%
           addTiles() %>%
           setView(lng = -93.5, lat = 44.95, zoom = 10) %>%
           addPopups(lng = -93.5, lat = 44.95, 
                    popup = "No spatial data available for mapping"))
  }
  
  # Filter out rows with invalid coordinates
  tryCatch({
    coords <- sf::st_coordinates(spatial_data)
    valid_rows <- !is.na(coords[,1]) & !is.na(coords[,2]) & 
                  is.finite(coords[,1]) & is.finite(coords[,2]) &
                  abs(coords[,1]) <= 180 & abs(coords[,2]) <= 90
    
    if (sum(valid_rows) == 0) {
      return(leaflet() %>%
             addTiles() %>%
             setView(lng = -93.5, lat = 44.95, zoom = 10) %>%
             addPopups(lng = -93.5, lat = 44.95, 
                      popup = "No sites with valid coordinates found"))
    }
    
    # Filter to only valid coordinate data
    if (sum(valid_rows) < nrow(spatial_data)) {
      spatial_data <- spatial_data[valid_rows, ]
    }
  }, error = function(e) {
    return(leaflet() %>%
           addTiles() %>%
           setView(lng = -93.5, lat = 44.95, zoom = 10) %>%
           addPopups(lng = -93.5, lat = 44.95, 
                    popup = paste("Coordinate validation error:", e$message)))
  })
  
  # Define colors for treatment status using proper db_helpers colors
  status_colors <- get_status_colors()
  status_color_map <- c(
    "Active" = unname(status_colors["active"]),           # Forest green for active treatment
    "Expiring" = unname(status_colors["needs_action"]),    # Orange for expiring treatment  
    "Expired" = unname(status_colors["needs_treatment"]),  # Red for expired treatment
    "No Treatment" = unname(status_colors["unknown"])      # Gray for no treatment
  )
  
  # Ensure we have treatment status data
  if (!"treatment_status" %in% names(spatial_data)) {
    spatial_data$treatment_status <- "No Treatment"
  }
  
  # Apply site filtering based on treatment status
  if (site_filter != "all") {
    if (site_filter == "expiring") {
      spatial_data <- spatial_data %>% filter(treatment_status == "Expiring")
    } else if (site_filter == "expiring_expired") {
      spatial_data <- spatial_data %>% filter(treatment_status %in% c("Expiring", "Expired"))
    }
    
    # Check if we still have data after filtering
    if (nrow(spatial_data) == 0) {
      return(leaflet() %>%
             addTiles() %>%
             setView(lng = -93.5, lat = 44.95, zoom = 10) %>%
             addPopups(lng = -93.5, lat = 44.95, 
                      popup = paste("No sites found with filter:", site_filter)))
    }
  }
  
  # Create color palette function
  pal <- colorFactor(
    palette = status_color_map,
    domain = c("Active", "Expiring", "Expired", "No Treatment"),
    na.color = "#7f7f7f"
  )
  
  # Get coordinates for map bounds
  coords <- sf::st_coordinates(spatial_data)
  
  # Create popup content
  spatial_data$popup_text <- sprintf(
    "<strong>%s</strong><br/>
    Facility: %s<br/>
    Zone: P%s<br/>
    Acres: %.2f<br/>
    Priority: %s<br/>
    Status: %s<br/>
    %s",
    spatial_data$sitecode,
    spatial_data$facility,
    spatial_data$zone,
    spatial_data$acres,
    spatial_data$priority,
    spatial_data$treatment_status,
    ifelse(is.na(spatial_data$last_treatment_date), 
           "No prehatch treatment recorded",
           sprintf("Last treated: %s<br/>Material: %s", 
                  spatial_data$last_treatment_date, 
                  spatial_data$last_material))
  )
  
  # Choose base tiles
  if (basemap == "satellite") {
    tiles <- "Esri.WorldImagery"
  } else if (basemap == "terrain") {
    tiles <- "Esri.WorldTopoMap"  
  } else if (basemap == "osm") {
    tiles <- "OpenStreetMap"
  } else {
    tiles <- "CartoDB.Positron"  # carto
  }
  
  # Create map
  map <- leaflet(spatial_data) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(
      radius = 6,
      color = "#000000",
      weight = 1,
      opacity = 0.8,
      fillColor = ~pal(treatment_status),
      fillOpacity = 0.8,
      popup = ~popup_text
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~treatment_status,
      title = "Treatment Status",
      opacity = 0.8
    )
  
  # Fit bounds if we have coordinates
  if (nrow(coords) > 0) {
    map <- map %>%
      fitBounds(
        lng1 = min(coords[,1]) - 0.01, lat1 = min(coords[,2]) - 0.01,
        lng2 = max(coords[,1]) + 0.01, lat2 = max(coords[,2]) + 0.01
      )
  }
  
  return(map)
}

# Function to create map details table (simplified for spatial data)
create_map_details_table <- function(spatial_data) {
  if (is.null(spatial_data) || nrow(spatial_data) == 0) {
    return(DT::datatable(
      data.frame("No map data available" = character(0)),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ))
  }
  
  # Create clean table without geometry
  table_data <- spatial_data %>%
    sf::st_drop_geometry() %>%
    arrange(desc(acres)) %>%
    select(sitecode, facility, zone, acres, treatment_status, last_treatment_date, last_material) %>%
    rename(
      "Sitecode" = sitecode,
      "Facility" = facility,
      "Zone" = zone,
      "Acres" = acres,
      "Status" = treatment_status,
      "Last Treatment" = last_treatment_date,
      "Material" = last_material
    )
  
  # Round numeric columns
  table_data$Acres <- round(table_data$Acres, 2)
  
  DT::datatable(
    table_data,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      columnDefs = list(
        list(className = 'dt-center', targets = '_all')
      )
    ),
    rownames = FALSE
  )
}

# Function to create historical chart
create_historical_chart <- function(data, time_period, display_metric, group_by, chart_type) {
  if (nrow(data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No historical data available"), size = 6) +
           theme_void())
  }
  
  # Get status colors from db_helpers
  status_colors <- get_status_colors()
  
  # Create base plot
  p <- ggplot(data, aes(x = time_period, y = count, group = display_name, color = display_name, fill = display_name))
  
  # Add chart type specific geometry
  if (chart_type == "line") {
    p <- p + 
      geom_line(size = 1.2) +
      geom_point(size = 2.5)
  } else if (chart_type == "area") {
    p <- p + 
      geom_area(alpha = 0.7) +
      geom_line(size = 1)
  } else if (chart_type == "step") {
    p <- p + 
      geom_step(size = 1.2, direction = "hv") +
      geom_point(size = 2.5)
  } else if (chart_type == "grouped_bar") {
    p <- p + 
      geom_col(position = "dodge", alpha = 0.8)
  } else {
    # Default: stacked bar
    p <- p + 
      geom_col(position = "stack", alpha = 0.8)
  }
  
  # Set title based on metric and time period
  metric_title <- switch(display_metric,
    "treatments" = "Treatments",
    "sites" = "Sites Treated", 
    "acres" = "Acres Treated",
    "site_acres" = "Site Acres (Unique Sites)",
    "weekly_active_sites" = "Active Sites",
    "weekly_active_acres" = "Active Site Acres",
    "Weekly Active Sites"
  )
  
  period_title <- if (time_period == "weekly") "Weekly" else "Yearly"
  chart_title <- paste(period_title, metric_title, "by", str_to_title(gsub("_", " ", group_by)))
  
  # Apply theme and formatting
  p <- p + 
    labs(
      title = chart_title,
      x = if (time_period == "weekly") "Week" else "Year",
      y = metric_title,
      color = "Group",
      fill = "Group"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = if (time_period == "weekly") 45 else 0, hjust = 1),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank()
    )
  
  # Convert to plotly
  ggplotly(p, tooltip = c("x", "y", "group")) %>%
    layout(
      title = list(text = chart_title, x = 0.5, font = list(size = 16)),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1)
    )
}