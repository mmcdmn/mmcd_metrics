# Display functions for SUCO History app
# These functions handle visualization and rendering for both current and all data

source("../../shared/db_helpers.R")

# Helper function to convert zone UI selection to filter vector
convert_zone_selection <- function(zone_input) {
  if (is.null(zone_input)) {
    return(c("1", "2"))  # Default to all zones
  }
  
  if (zone_input == "all") {
    return(c("1", "2"))  # P1 + P2 means both zones
  } else {
    return(as.character(zone_input))  # Single zone selection
  }
}

# Create SUCO map (consolidates map and current_map functionality)
# Function to create SUCO map with flexible data source
create_suco_map <- function(data, input, data_source = "all") {
  # Get marker size multiplier
  size_multiplier <- input$marker_size
  
  # Get facility and foremen lookups for display names
  facilities <- get_facility_lookup()
  
  # Set up basemap provider
  basemap <- switch(input$basemap,
                    "osm" = providers$OpenStreetMap,
                    "carto" = providers$CartoDB.Positron,
                    "terrain" = providers$Stamen.Terrain,
                    "satellite" = providers$Esri.WorldImagery,
                    providers$CartoDB.Positron)
  
  # Handle case when no data is available
  if (nrow(data) == 0) {
    message_text <- if (data_source == "current") {
      "No SUCO locations available with the selected filters (Current Data)"
    } else {
      "No SUCO locations available with the selected filters"
    }
    
    return(
      leaflet() %>%
        addProviderTiles(basemap) %>%
        setView(lng = -93.2, lat = 45.0, zoom = 9) %>%
        addControl(html = paste0("<div style='background-color: white; padding: 10px;'><h4>", message_text, "</h4></div>"),
                   position = "topleft")
    )
  }
  
  # Create color palette based on field count or facility
  if (input$group_by == "facility") {
    # Get facility colors and lookup from db_helpers
    facility_colors <- get_facility_base_colors()
    facilities <- get_facility_lookup()
    foremen_lookup <- get_foremen_lookup()  # Add foremen lookup for popups
    foremen_lookup$emp_num <- as.character(foremen_lookup$emp_num)  # Ensure string format
    
    # Create facility name mapping FIRST
    facility_names <- setNames(facilities$full_name, facilities$short_name)
    
    # Create popup text beforehand to avoid scope issues
    data <- data %>%
      mutate(
        popup_text = {
          # Get proper foreman names with robust matching
          foreman_names <- sapply(foreman, function(f) {
            if (!is.na(f) && f != "" && !is.null(f)) {
              # Ensure both foreman and emp_num are strings for comparison
              foreman_str <- trimws(as.character(f))
              
              # Find matching foreman in lookup table
              matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_str)
              
              if(length(matches) > 0) {
                foremen_lookup$shortname[matches[1]]
              } else {
                # Fallback: show the raw foreman ID if no match found
                paste0("FOS #", foreman_str)
              }
            } else {
              "No FOS assigned"
            }
          })
          
          facility_names_vec <- sapply(facility, function(f) {
            fname <- facility_names[f]
            if(length(fname) > 0) fname[1] else f
          })
          
          paste0("<b>Date:</b> ", inspdate, "<br>",
                 "<b>Facility:</b> ", facility_names_vec, "<br>",
                 "<b>FOS:</b> ", foreman_names, "<br>",
                 "<b>Location:</b> ", location, "<br>",
                 "<b>Species Count:</b> ", display_species_count, "<br>",
                 "<b>Species Found:</b><br>", species_summary)
        }
      )
    
    # Create color palette function
    pal <- colorFactor(
      palette = facility_colors,
      domain = names(facility_colors))
    
    # Create a named vector for legend labels
    legend_labels <- sapply(names(facility_colors), function(code) facility_names[code] %||% code)
    
    # Create map with facility coloring
    leaflet(data) %>%
      addProviderTiles(basemap) %>%
      fitBounds(
        lng1 = min(st_coordinates(data)[,1]),
        lat1 = min(st_coordinates(data)[,2]),
        lng2 = max(st_coordinates(data)[,1]),
        lat2 = max(st_coordinates(data)[,2])
      ) %>%
      addCircleMarkers(
        radius = ~marker_size,
        color = "black",
        weight = 1.5,
        fillColor = ~pal(facility),
        fillOpacity = 0.8,
        popup = ~popup_text
      ) %>%
      addLegend(
        position = "bottomright",
        title = "Facility",
        colors = facility_colors,
        labels = legend_labels,
        opacity = 0.8
      )
  } else if (input$group_by == "foreman") {
    # Filter out records with NA foreman before processing
    data <- data %>% filter(!is.na(foreman) & foreman != "")
    
    # Check if we have any data left after filtering
    if (nrow(data) == 0) {
      return(
        leaflet() %>%
          addProviderTiles(basemap) %>%
          setView(lng = -93.2, lat = 45.0, zoom = 9) %>%
          addControl(html = "<div style='background-color: white; padding: 10px;'><h4>No SUCO locations with valid FOS data available</h4></div>",
                     position = "topleft")
      )
    }
    
    # Get both colors and lookup exactly as documented
    foreman_colors <- get_foreman_colors()
    foremen_lookup <- get_foremen_lookup()
    
    # Create mapping from foreman NUMBER to facility-based colors (same logic as plot)
    foremen_in_data <- unique(na.omit(data$foreman))
    emp_colors <- character(0)
    
    for (foreman_num in foremen_in_data) {
      foreman_num_str <- trimws(as.character(foreman_num))
      
      # Find the shortname for this foreman number
      matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
      
      if(length(matches) > 0) {
        shortname <- foremen_lookup$shortname[matches[1]]
        facility <- foremen_lookup$facility[matches[1]]
        
        # Get the facility-based color for this shortname
        if(shortname %in% names(foreman_colors)) {
          emp_colors[foreman_num_str] <- foreman_colors[shortname]
        }
      }
    }
    
    # Remove any NA colors
    emp_colors <- emp_colors[!is.na(emp_colors)]
    
    # Create ORDERED colors to ensure legend and map match exactly
    # Order by facility, then by foreman within facility (same as legend)
    ordered_foremen <- foremen_lookup[order(foremen_lookup$facility, foremen_lookup$shortname), ]
    ordered_emp_colors <- character(0)
    ordered_emp_numbers <- character(0)
    
    for (i in 1:nrow(ordered_foremen)) {
      emp_num <- trimws(as.character(ordered_foremen$emp_num[i]))
      if (emp_num %in% names(emp_colors)) {
        ordered_emp_colors <- c(ordered_emp_colors, emp_colors[emp_num])
        ordered_emp_numbers <- c(ordered_emp_numbers, emp_num)
      }
    }
    
    names(ordered_emp_colors) <- ordered_emp_numbers
    
    # Create color palette function for leaflet using ORDERED colors
    pal <- colorFactor(
      palette = ordered_emp_colors,
      domain = names(ordered_emp_colors),
      ordered = TRUE  # Maintain order
    )
    
    # Create popup text beforehand for foreman map too
    data <- data %>%
      mutate(
        popup_text_foreman = {
          # Get proper foreman names with robust matching
          foreman_names <- sapply(foreman, function(f) {
            if (!is.na(f) && f != "" && !is.null(f)) {
              # Ensure both foreman and emp_num are strings for comparison
              foreman_str <- trimws(as.character(f))
              
              # Find matching foreman in lookup table
              matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_str)
              
              if(length(matches) > 0) {
                foremen_lookup$shortname[matches[1]]
              } else {
                # Fallback: show the raw foreman ID if no match found
                paste0("FOS #", foreman_str)
              }
            } else {
              "No FOS assigned"
            }
          })
          
          facility_names_vec <- sapply(facility, function(f) {
            fname <- facilities$full_name[facilities$short_name == f]
            if(length(fname) > 0) fname[1] else f
          })
          
          paste0("<b>Date:</b> ", inspdate, "<br>",
                 "<b>Facility:</b> ", facility_names_vec, "<br>",
                 "<b>FOS:</b> ", foreman_names, "<br>",
                 "<b>Location:</b> ", location, "<br>",
                 "<b>Species Count:</b> ", display_species_count, "<br>",
                 "<b>Species Found:</b><br>", species_summary)
        }
      )
    
    # Create map with foreman coloring
    leaflet(data) %>%
      addProviderTiles(basemap) %>%
      fitBounds(
        lng1 = min(st_coordinates(data)[,1]),
        lat1 = min(st_coordinates(data)[,2]),
        lng2 = max(st_coordinates(data)[,1]),
        lat2 = max(st_coordinates(data)[,2])
      ) %>%
      addCircleMarkers(
        radius = ~marker_size,
        color = "black",
        weight = 1.5,
        fillColor = ~pal(foreman),
        fillOpacity = 0.8,
        popup = ~popup_text_foreman
      ) %>%
      addLegend(
        position = "bottomright",
        title = "FOS",
        colors = ordered_emp_colors,  # Use same ordered colors as map palette
        labels = {
          # Use same ordering as the map palette
          ordered_labels <- character(0)
          for (i in 1:length(ordered_emp_numbers)) {
            emp_num <- ordered_emp_numbers[i]
            foreman_info <- foremen_lookup[trimws(as.character(foremen_lookup$emp_num)) == emp_num, ]
            if(nrow(foreman_info) > 0) {
              ordered_labels <- c(ordered_labels, foreman_info$shortname[1])
            }
          }
          ordered_labels
        },
        opacity = 0.8
      )
  } else {
    # For MMCD (All) case, use a single color
    leaflet(data) %>%
      addProviderTiles(basemap) %>%
      fitBounds(
        lng1 = min(st_coordinates(data)[,1]),
        lat1 = min(st_coordinates(data)[,2]),
        lng2 = max(st_coordinates(data)[,1]),
        lat2 = max(st_coordinates(data)[,2])
      ) %>%
      addCircleMarkers(
        radius = ~marker_size,
        color = "black",
        weight = 1.5,
        fillColor = "#1f77b4", # Standard blue color
        fillOpacity = 0.8,
        popup = ~paste0("<b>Date:</b> ", inspdate, "<br>",
                        "<b>Facility:</b> ", facility, "<br>",
                        "<b>Foreman:</b> ", foreman, "<br>",
                        "<b>Location:</b> ", location, "<br>",
                        "<b>Species Count:</b> ", display_species_count, "<br>",
                        "<b>Species Found:</b><br>", species_summary)
      ) %>%
      addLegend(
        position = "bottomright",
        title = "MMCD",
        colors = "#1f77b4",
        labels = "All",
        opacity = 0.8
      )
  }
}

# Create location plotly chart (consolidates location_plotly and current_location_plotly)
create_location_plotly <- function(top_locations_data, data_source = "all", mode = "visits") {
  if (nrow(top_locations_data) == 0) {
    empty_plot <- plotly::plot_ly() %>%
      plotly::add_annotations(
        text = paste("No location data available", 
                    if(data_source == "current") "(Current Data)" else ""),
        xref = "paper", yref = "paper",
        x = 0.5, y = 0.5, xanchor = 'center', yanchor = 'middle',
        showarrow = FALSE, font = list(size = 16)
      ) %>%
      plotly::layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        showlegend = FALSE
      )
    return(empty_plot)
  }
  
  # Determine chart properties based on mode
  if (mode == "species") {
    chart_title <- if (data_source == "current") {
      "Top Locations by Species Count - Individual Samples (Current Data Only)"
    } else {
      "Top Locations by Species Count - Individual Samples"
    }
    chart_subtitle <- "Each segment represents one SUCO sample. Hover for details."
    y_label <- "Species Count per Sample"
    value_col <- "species_count"
    fill_colors <- if(data_source == "current") {
      scale_fill_viridis_c(option = "plasma", name = "Date")
    } else {
      scale_fill_viridis_c(option = "viridis", name = "Date")
    }
  } else {
    chart_title <- if (data_source == "current") {
      "Top SUCO Locations - Individual Samples (Current Data Only)"
    } else {
      "Top SUCO Locations - Individual Samples"
    }
    chart_subtitle <- "Each segment represents one SUCO sample. Colors show sample dates."
    y_label <- "Individual Samples"
    value_col <- "visits"
    fill_colors <- if(data_source == "current") {
      scale_fill_viridis_c(option = "plasma", name = "Date")
    } else {
      scale_fill_viridis_c(option = "viridis", name = "Date")
    }
  }
  
  # Calculate location totals for ordering
  location_totals <- top_locations_data %>%
    group_by(location) %>%
    summarize(total = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total))
  
  # Reorder data by location totals
  top_locations_data$location <- factor(top_locations_data$location, 
                                       levels = location_totals$location)
  
  # Create stacked bar chart
  p <- ggplot(top_locations_data, aes(x = location, y = .data[[value_col]], 
                                     fill = date_numeric, 
                                     text = paste("Location:", location, "<br>",
                                                 "Date:", format(inspdate, "%m/%d/%y"), "<br>",
                                                 "Species Found:<br>",
                                                 gsub("<br>", "<br>", species_summary)))) +
    geom_bar(stat = "identity", position = "stack") +
    fill_colors +
    coord_flip() +
    labs(title = chart_title, subtitle = chart_subtitle, x = "Location", y = y_label) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16), 
      plot.subtitle = element_text(size = 12, color = "gray40", margin = margin(b = 15)),
      axis.title = element_text(face = "bold"),
      legend.position = "right",  # Show legend to explain color scale
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 10)
    ) +
    guides(fill = guide_colorbar(
      title = "Sample Date\n(Newer → Lighter)",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 1,
      barheight = 8
    ))
  
  # Convert to plotly with appropriate source for click events
  source_name <- if (data_source == "current") "current_location_plotly" else "location_plotly"
  p <- plotly::ggplotly(p, tooltip = "text", source = source_name)
  
  # Register the click event
  plotly::event_register(p, 'plotly_click')
  
  return(p)
}

# Create trend plot (consolidates trend plotting logic)
create_trend_plot <- function(aggregated_data, aggregated_data_current, input, data_source = "all") {
  # Get aggregated data based on data source
  if (data_source == "current") {
    data <- aggregated_data_current()
  } else {
    data <- aggregated_data()
  }
  
  if (nrow(data) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("No SUCO data available with the selected filters", 
                              if(data_source == "current") "(Current Data)" else ""), size = 6) +
        theme_void()
    )
  }
  
  # Use weekly labels (MM/DD format)
  data$time_label <- format(data$time_group, "%m/%d")
  
  group_col <- input$group_by
  if (group_col == "mmcd_all") {
    group_col <- "mmcd_all"
  }
  
  # Get facility and foreman lookups for labels
  facilities <- get_facility_lookup()
  foremen_lookup <- get_foremen_lookup()
  
  # Create facility name mapping
  facility_names <- setNames(facilities$full_name, facilities$short_name)
  
  # Create foreman name mapping
  foreman_names <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
  
  title_interval <- "Weekly"
  title_group <- case_when(
    input$group_by == "facility" ~ "Facility",
    input$group_by == "foreman" ~ "FOS", 
    input$group_by == "mmcd_all" ~ "MMCD (All)",
    TRUE ~ "Group"
  )
  
  # Create filter text for subtitle
  facility_text <- if ("All" %in% input$facility_filter) {
    "All Facilities"
  } else {
    display_names <- sapply(input$facility_filter, function(f) facility_names[f] %||% f)
    paste("Facility:", paste(display_names, collapse=", "))
  }
  
  foreman_text <- if ("All" %in% input$foreman_filter) {
    "All Foremen"
  } else {
    display_names <- sapply(input$foreman_filter, function(f) foreman_names[f] %||% f)
    paste("FOS:", paste(display_names, collapse=", "))
  }
  
  # Zone filter text
  zone_text <- case_when(
    input$zone_filter == "all" ~ "Zones: P1 + P2",
    input$zone_filter == "1" ~ "Zone: P1",
    input$zone_filter == "2" ~ "Zone: P2",
    TRUE ~ "Zone: All"
  )
  
  # Get color scales from db_helpers based on grouping
  custom_colors <- if(group_col == "facility") {
    get_facility_base_colors()
  } else if(group_col == "foreman") {
    # Get the foreman colors from db_helpers
    foreman_colors <- get_foreman_colors()
    foremen_lookup <- get_foremen_lookup()
    
    # Create mapping from foreman NUMBER to facility-based colors
    foremen_in_data <- unique(na.omit(data[[group_col]]))
    emp_colors <- character(0)
    
    for (foreman_num in foremen_in_data) {
      foreman_num_str <- trimws(as.character(foreman_num))
      
      # Find the shortname for this foreman number
      matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
      
      if(length(matches) > 0) {
        shortname <- foremen_lookup$shortname[matches[1]]
        facility <- foremen_lookup$facility[matches[1]]
        
        # Get the facility-based color for this shortname
        if(shortname %in% names(foreman_colors)) {
          emp_colors[foreman_num_str] <- foreman_colors[shortname]
        }
      }
    }
    
    # Remove any NA colors
    emp_colors <- emp_colors[!is.na(emp_colors)]
    
    # Return the mapped colors
    emp_colors
  } else {
    NULL
  }
  
  # Determine the plotting group column and handle color mapping for combined zones
  # With the current implementation, zones are never shown separately
  # "P1 + P2" combines zones into single bars/lines
  zones_selected <- convert_zone_selection(input$zone_filter)
  show_zones_separately <- FALSE  # Never show zones separately in this version
  
  plot_group_col <- group_col
  
  # Get colors based on grouping - simplified since zones are never shown separately
  if (group_col == "facility") {
    # Standard facility colors
    custom_colors <- get_facility_base_colors()
    alpha_values <- NULL
  } else if (group_col == "foreman") {
    # Get foremen lookup for mapping emp_num to shortname
    foremen_lookup <- get_foremen_lookup()
    
    # Standard foreman colors - map from shortname to emp_num
    foreman_colors <- get_foreman_colors()
    emp_colors <- character(0)
    
    for (i in 1:nrow(foremen_lookup)) {
      shortname <- trimws(foremen_lookup$shortname[i])
      emp_num <- trimws(as.character(foremen_lookup$emp_num[i]))
      
      if (shortname %in% names(foreman_colors)) {
        emp_colors[emp_num] <- foreman_colors[shortname]
      }
    }
    
    custom_colors <- emp_colors
    alpha_values <- NULL
  } else {
    custom_colors <- NULL
    alpha_values <- NULL
  }
  
  # Create the plot using standard aesthetics (no alpha since zones aren't shown separately)
  p <- ggplot(data, aes(x = time_group, y = count, 
                       color = !!sym(plot_group_col), 
                       fill = !!sym(plot_group_col), 
                       group = !!sym(plot_group_col)))
  
  # Add color scales based on grouping
  if(!is.null(custom_colors)) {
    # Create label mapping for display (simplified since no combined groups)
    if (group_col == "facility") {
      # Map facility short_names to full_names for labels
      labels_mapping <- function(x) sapply(x, function(code) facility_names[code] %||% code)
    } else if (group_col == "foreman") {
      # Map emp_num to shortnames for labels
      labels_mapping <- function(x) sapply(x, function(num) foreman_names[as.character(num)] %||% paste0("FOS #", num))
    } else {
      labels_mapping <- NULL
    }
    
    p <- p + scale_color_manual(values = custom_colors, labels = labels_mapping, drop = FALSE) + 
             scale_fill_manual(values = custom_colors, labels = labels_mapping, drop = FALSE)
  } else {
    p <- p + scale_color_discrete() + scale_fill_discrete()
  }
  
  # Add zone alpha differentiation if available
  if (!is.null(alpha_values)) {
    p <- add_zone_alpha_to_plot(p, alpha_values, 
                                representative_color = if(!is.null(custom_colors)) custom_colors[1] else NULL)
  }
  
  if (input$graph_type == "bar") {
    p <- p + geom_bar(stat = "identity", position = "dodge")
  } else if (input$graph_type == "stacked_bar") {
    p <- p + geom_bar(stat = "identity", position = "stack")
  } else if (input$graph_type == "line") {
    p <- p + geom_line(size = 1.2)
  } else if (input$graph_type == "point") {
    p <- p + geom_point(size = 3)
  } else if (input$graph_type == "area") {
    p <- p + geom_area(position = "stack", alpha = 0.6)
  }
  
  # Add average line for "All Data (Current + Archive)" only
  if (data_source == "all") {
    # Calculate average SUCOs per week based on TOTAL SUCOs per week (not per group)
    # First, get the total SUCOs per week by summing across all groups
    weekly_totals <- data %>%
      group_by(time_group) %>%
      summarize(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      filter(total_count > 0)  # Only count weeks with actual SUCOs
    
    if (nrow(weekly_totals) > 0) {
      avg_sucos_per_week <- mean(weekly_totals$total_count, na.rm = TRUE)
      
      # For stacked bars, use the maximum total height for positioning
      max_height <- if (input$graph_type == "stacked_bar") {
        max(weekly_totals$total_count, na.rm = TRUE)
      } else {
        max(data$count, na.rm = TRUE)
      }
      
      p <- p + geom_hline(yintercept = avg_sucos_per_week, 
                         color = "red", 
                         linetype = "dashed", 
                         size = 1.2, 
                         alpha = 0.8) +
               annotate("text", 
                       x = max(data$time_group) - days(7), 
                       y = avg_sucos_per_week + max_height * 0.05, 
                       label = paste("Avg:", round(avg_sucos_per_week, 1), "SUCOs/week"), 
                       color = "red", 
                       size = 4.5, 
                       hjust = 1,
                       fontface = "bold")
    }
  }
  
  subtitle_text <- paste(zone_text, "-", facility_text, "-", foreman_text)
  if (data_source == "current") {
    subtitle_text <- paste(subtitle_text, "(Current Data Only)")
  }
  
  # Create y-axis label based on species filter
  y_axis_label <- if (!is.null(input$species_filter) && input$species_filter != "All") {
    paste0("Number of SUCOs with ", input$species_filter)
  } else {
    "Number of SUCOs"
  }
  
  p <- p + labs(
    title = paste(title_interval, "SUCO Counts by", ifelse(input$group_by == "mmcd_all", "MMCD (All)", title_group)),
    subtitle = subtitle_text,
    x = "Epi Week",
    y = y_axis_label,
    fill = ifelse(input$group_by == "mmcd_all", "MMCD (All)", title_group),
    color = ifelse(input$group_by == "mmcd_all", "MMCD (All)", title_group)
  ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(size = 14, face = "bold"),
      axis.title = element_text(face = "bold", size = 14),
      axis.text = element_text(face = "bold", size = 13),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(face = "bold", size = 14),
      axis.title.y = element_text(size = 15, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 14, face = "bold")
    )
  
  # Identify year boundaries for vertical separator lines
  year_boundaries <- data %>%
    mutate(year = epiyear(time_group)) %>%
    group_by(year) %>%
    summarize(min_time = min(time_group), max_time = max(time_group), .groups = "drop") %>%
    arrange(year)
  
  # Add vertical lines at year boundaries (except the first one)
  if (nrow(year_boundaries) > 1) {
    for (i in 2:nrow(year_boundaries)) {
      p <- p + geom_vline(xintercept = as.numeric(year_boundaries$min_time[i]), 
                         color = "gray60", 
                         linetype = "solid", 
                         size = 1.5, 
                         alpha = 0.7)
    }
  }
  
  # Use epi week scale instead of traditional date labels
  # Create a mapping of time_group to epi_week_label for x-axis
  epi_labels <- data %>%
    distinct(time_group, epi_week_label) %>%
    arrange(time_group)
  
  p <- p + scale_x_date(
    breaks = epi_labels$time_group,
    labels = epi_labels$epi_week_label,
    limits = c(min(data$time_group), max(data$time_group))
  )
  return(p)
}

# Helper function to add zone alpha differentiation to plots
add_zone_alpha_to_plot <- function(p, alpha_values, representative_color = NULL) {
  # Add alpha scale for zone differentiation
  p <- p + scale_alpha_manual(
    values = alpha_values,
    guide = guide_legend(title = "Zone Alpha", override.aes = list(color = representative_color))
  )
  return(p)
}