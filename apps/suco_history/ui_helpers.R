# UI helper functions for SUCO History app
# These functions handle UI updates and interactions

# Helper function to set updating flags and reset them
set_updating_flags <- function(updating_date_range, updating_year_range) {
  updating_date_range(TRUE)
  updating_year_range(TRUE)
  
  # Reset flags after a short delay
  later::later(function() {
    updating_date_range(FALSE)
    updating_year_range(FALSE)
  }, 0.1)
}

# Helper function to handle date shortcut logic
handle_date_shortcut <- function(shortcut_type, session, input, updating_date_range, updating_year_range) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Calculate date range based on shortcut type
  if (shortcut_type == "year") {
    start_date <- as.Date(paste0(current_year, "-01-01"))
    end_date <- Sys.Date()
  } else if (shortcut_type == "month") {
    current_month <- as.numeric(format(Sys.Date(), "%m"))
    start_date <- as.Date(paste0(current_year, "-", sprintf("%02d", current_month), "-01"))
    end_date <- Sys.Date()
  } else if (shortcut_type == "week") {
    # Get start of current week (Monday)
    start_date <- floor_date(Sys.Date(), "week", week_start = 1)
    end_date <- Sys.Date()
  }
  
  # Set flags to prevent infinite loop
  set_updating_flags(updating_date_range, updating_year_range)
  
  # Always update date range
  updateDateRangeInput(session, "date_range", start = start_date, end = end_date)
  
  # Only update year slider if on All Data tab
  if (!is.null(input$main_tabset) && input$main_tabset == "All Data (Current + Archive)") {
    updateSliderInput(session, "year_range", value = c(current_year, current_year))
  }
}

# Helper function to handle year range changes
handle_year_range_change <- function(input, session, updating_date_range, updating_year_range) {
  # Only process if we're on the All Data tab and not already updating
  if (!updating_year_range() && 
      !is.null(input$main_tabset) && 
      input$main_tabset == "All Data (Current + Archive)") {
    
    start_year <- input$year_range[1]
    end_year <- input$year_range[2]
    
    # Create date range from year selection
    start_date <- as.Date(paste0(start_year, "-01-01"))
    end_date <- if (end_year == as.numeric(format(Sys.Date(), "%Y"))) {
      # If current year is selected as end, use today's date
      Sys.Date()
    } else {
      # Otherwise use end of that year
      as.Date(paste0(end_year, "-12-31"))
    }
    
    # Set flag and update date range input
    updating_date_range(TRUE)
    updateDateRangeInput(session, "date_range", start = start_date, end = end_date)
    
    # Reset flag after a short delay
    later::later(function() {
      updating_date_range(FALSE)
    }, 0.1)
  }
}

# Helper function to handle custom date range changes
handle_date_range_change <- function(input, session, updating_date_range, updating_year_range) {
  if (!updating_date_range() && 
      !is.null(input$date_range) && 
      length(input$date_range) == 2) {
    
    start_year <- as.numeric(format(input$date_range[1], "%Y"))
    end_year <- as.numeric(format(input$date_range[2], "%Y"))
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    
    # If on Current Data tab, restrict to current year only
    if (!is.null(input$main_tabset) && input$main_tabset == "Current Data") {
      if (start_year != current_year || end_year != current_year) {
        # Force dates back to current year
        new_start <- as.Date(paste0(current_year, "-01-01"))
        new_end <- Sys.Date()
        
        updating_date_range(TRUE)
        updateDateRangeInput(session, "date_range", start = new_start, end = new_end)
        
        # Show a notification
        showNotification("Current Data tab is limited to current year data only. Use 'All Data' tab for multi-year analysis.", 
                        type = "warning", duration = 3)
        
        # Reset flag after a short delay
        later::later(function() {
          updating_date_range(FALSE)
        }, 0.1)
      }
    } else if (!is.null(input$main_tabset) && input$main_tabset == "All Data (Current + Archive)") {
      # On All Data tab, update year slider to match
      if (!identical(c(start_year, end_year), input$year_range)) {
        updating_year_range(TRUE)
        updateSliderInput(session, "year_range", value = c(start_year, end_year))
        
        # Reset flag after a short delay
        later::later(function() {
          updating_year_range(FALSE)
        }, 0.1)
      }
    }
  }
}

# Helper function to handle plotly click events and map updates
handle_location_click <- function(click_data, data_function, spatial_data_function, session, map_id, tabset_id, tab_value) {
  if (!is.null(click_data)) {
    idx <- click_data$pointNumber + 1  # R is 1-based
    data <- data_function()
    top_locations <- data %>%
      group_by(location) %>%
      summarize(visits = n(), .groups = "drop") %>%
      arrange(desc(visits)) %>%
      head(15)
      
    if (idx > 0 && idx <= nrow(top_locations)) {
      loc <- top_locations$location[idx]
      spatial <- spatial_data_function()
      
      if (nrow(spatial) > 0 && loc %in% spatial$location) {
        point <- spatial[spatial$location == loc, ][1, ]
        coords <- sf::st_coordinates(point)
        lng <- coords[1]
        lat <- coords[2]
        
        if (!is.na(lng) && !is.na(lat)) {
          updateTabsetPanel(session, tabset_id, selected = tab_value)
          leafletProxy(map_id) %>%
            setView(lng = lng, lat = lat, zoom = 15) %>%
            addCircleMarkers(lng = lng, lat = lat, radius = 15, color = "red", 
                           fill = TRUE, fillOpacity = 0.7, layerId = paste0("highlighted_location_", map_id))
        }
      }
    }
  } else {
    leafletProxy(map_id) %>% clearGroup(paste0("highlighted_location_", map_id))
  }
}

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