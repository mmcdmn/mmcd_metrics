# Display functions for SUCO History app
# These functions handle visualization and rendering for both current and all data

source("../../shared/db_helpers.R")

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
                 "<b>Field Count:</b> ", fieldcount)
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
        radius = ~pmin(15, (3 * size_multiplier)),
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
                 "<b>Field Count:</b> ", fieldcount)
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
        radius = ~pmin(15, (3 * size_multiplier)),
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
        radius = ~pmin(15, (3 * size_multiplier)),
        color = "black",
        weight = 1.5,
        fillColor = "#1f77b4", # Standard blue color
        fillOpacity = 0.8,
        popup = ~paste0("<b>Date:</b> ", inspdate, "<br>",
                        "<b>Facility:</b> ", facility, "<br>",
                        "<b>Foreman:</b> ", foreman, "<br>",
                        "<b>Location:</b> ", location, "<br>",
                        "<b>Field Count:</b> ", fieldcount)
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
create_location_plotly <- function(top_locations_data, data_source = "all") {
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
  
  # Create the plotly bar chart
  chart_title <- if (data_source == "current") {
    "Top SUCO Locations (Current Data Only)"
  } else {
    "Top SUCO Locations"
  }
  
  # Create ggplot
  p <- ggplot(top_locations_data, aes(x = reorder(location, visits), y = visits, text = location)) +
    geom_bar(stat = "identity", fill = if(data_source == "current") "#1f77b4" else "steelblue") +
    geom_text(aes(label = visits), hjust = 1.3, color = "black") +
    coord_flip() +
    labs(title = chart_title, x = "Location", y = "Number of Visits") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 16), 
          axis.title = element_text(face = "bold"))
  
  # Convert to plotly with appropriate source for click events
  source_name <- if (data_source == "current") "current_location_plotly" else "location_plotly"
  p <- plotly::ggplotly(p, tooltip = c("x", "y", "text"), source = source_name)
  
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
    paste(display_names, collapse = ", ")
  }
  
  foreman_text <- if ("All" %in% input$foreman_filter) {
    "All FOS"
  } else {
    display_names <- sapply(input$foreman_filter, function(f) foreman_names[f] %||% f)
    paste(display_names, collapse = ", ")
  }
  
  species_text <- if ("All" %in% input$species_filter) {
    "All Species"
  } else {
    paste(input$species_filter, collapse = ", ")
  }
  
  zone_text <- if ("All" %in% input$zone_filter) {
    "All Zones"
  } else {
    paste(input$zone_filter, collapse = ", ")
  }
  
  # Format date range properly
  date_from <- format(input$date_range[1], "%m/%d/%Y")
  date_to <- format(input$date_range[2], "%m/%d/%Y")
  date_text <- paste0(date_from, " to ", date_to)
  
  filter_subtitle <- paste0("Filters: ", facility_text, " | ", foreman_text, " | ", species_text, " | ", zone_text, " | ", date_text)
  
  # Create base plot
  base_title <- paste(title_interval, "SUCO Trend by", title_group)
  if(data_source == "current") {
    base_title <- paste(base_title, "(Current Data Only)")
  }
  
  if (group_col == "mmcd_all") {
    # Single line for MMCD (All)
    p <- ggplot(data, aes(x = time_group, y = total_sucoz)) +
      geom_line(color = "#1f77b4", size = 1.2) +
      geom_point(color = "#1f77b4", size = 2) +
      labs(title = base_title,
           subtitle = filter_subtitle,
           x = "Week",
           y = "Total SUCO Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 10, color = "gray50"),
        axis.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold")
      )
  } else if (group_col == "facility") {
    # Get facility colors
    facility_colors <- get_facility_base_colors()
    
    p <- ggplot(data, aes(x = time_group, y = total_sucoz, color = facility)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(
        name = "Facility",
        values = facility_colors,
        labels = function(x) sapply(x, function(code) facility_names[code] %||% code)
      ) +
      labs(title = base_title,
           subtitle = filter_subtitle,
           x = "Week",
           y = "Total SUCO Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 10, color = "gray50"),
        axis.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold")
      )
  } else if (group_col == "foreman") {
    # Get foreman colors (facility-based)
    foreman_colors <- get_foreman_colors()
    
    # Need to map from foreman numbers to colors
    unique_foremen <- unique(data$foreman)
    foreman_color_mapping <- character(0)
    
    for (foreman_num in unique_foremen) {
      foreman_num_str <- trimws(as.character(foreman_num))
      matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
      
      if(length(matches) > 0) {
        shortname <- foremen_lookup$shortname[matches[1]]
        if(shortname %in% names(foreman_colors)) {
          foreman_color_mapping[foreman_num_str] <- foreman_colors[shortname]
        }
      }
    }
    
    p <- ggplot(data, aes(x = time_group, y = total_sucoz, color = factor(foreman))) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(
        name = "FOS",
        values = foreman_color_mapping,
        labels = function(x) sapply(x, function(num) foreman_names[num] %||% paste0("FOS #", num))
      ) +
      labs(title = base_title,
           subtitle = filter_subtitle,
           x = "Week",
           y = "Total SUCO Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 10, color = "gray50"),
        axis.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold")
      )
  }
  # Use weekly scale
  p <- p + scale_x_date(
    date_breaks = "2 weeks",
    date_labels = "%m/%d",
    limits = c(min(data$time_group), max(data$time_group))
  )
  return(p)
}