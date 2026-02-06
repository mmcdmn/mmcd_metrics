# =============================================================================
# MMCD METRICS - SHARED GEOMETRY HELPERS
# =============================================================================
# Reusable functions for handling shapefiles, coordinate transformations,
# and map rendering. Designed to be used across all map-enabled apps.
#
# CONTENTS:
# 1. Shapefile Loading
# 2. Map Bounds Handling  
# 3. Coordinate Validation
# 4. Leaflet Map Helpers
# 5. Background Layer Helpers
# =============================================================================

suppressPackageStartupMessages({
  # SF (geospatial) - required for map functionality but optional for testing
  if (!requireNamespace("sf", quietly = TRUE)) {
    message("WARNING: sf package not available - spatial functions will be limited")
  } else {
    library(sf)
  }
  
  # Leaflet - required for map rendering but optional for testing  
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    message("WARNING: leaflet package not available - map rendering will fail")
  } else {
    library(leaflet)
  }
  
  library(dplyr)
})

# =============================================================================
# 1. SHAPEFILE LOADING
# =============================================================================

#' Load a shapefile with automatic CRS transformation
#' 
#' @param file_path Path to the shapefile (.shp)
#' @param target_crs Target CRS (default: 4326 for WGS84/leaflet)
#' @param quiet Suppress sf::st_read messages
#' @return sf object or NULL if file doesn't exist
#' @export
load_shapefile <- function(file_path, target_crs = 4326, quiet = TRUE) {
  if (!file.exists(file_path)) {
    message("Shapefile not found: ", file_path)
    return(NULL)
  }
  
  tryCatch({
    sf_data <- sf::st_read(file_path, quiet = quiet)
    
    # Transform to target CRS if needed
    current_crs <- sf::st_crs(sf_data)
    if (!is.na(current_crs) && !is.null(current_crs$epsg) && current_crs$epsg != target_crs) {
      sf_data <- sf::st_transform(sf_data, target_crs)
    }
    
    return(sf_data)
  }, error = function(e) {
    message("Error loading shapefile: ", file_path, " - ", e$message)
    return(NULL)
  })
}

#' Load MMCD background layers (facility and zone boundaries)
#' 
#' @param base_path Base path to Q_to_R/data directory (or NULL for default)
#' @return List with facilities and zones sf objects
#' @export
load_background_layers <- function(base_path = NULL) {
  # Default path - works from apps/{app_name}/ directory
  if (is.null(base_path)) {
    base_path <- file.path("..", "..", "shared", "Q_to_R", "data")
  }
  
  list(
    facilities = load_shapefile(file.path(base_path, "facility_boundaries.shp")),
    zones = load_shapefile(file.path(base_path, "zone_boundaries.shp"))
  )
}

#' Load harborage shapefiles with optional facility/foreman filtering
#' 
#' @param base_path Path to harborages directory
#' @param facility_filter Facility code to filter by (or NULL/"all" for all)
#' @param foreman_filter Foreman emp_num(s) to filter by (or NULL/"all" for all)
#' @return Combined sf object or NULL if no harborages found
#' @export
load_harborage_layers <- function(base_path = NULL, facility_filter = NULL, foreman_filter = NULL) {
  # Default path
  if (is.null(base_path)) {
    base_path <- file.path("..", "..", "shared", "Q_to_R", "data", "harborages")
  }
  
  if (!dir.exists(base_path)) {
    message("Harborage directory not found: ", base_path)
    return(NULL)
  }
  
  # Get facility folders
  facility_folders <- list.dirs(base_path, full.names = TRUE, recursive = FALSE)
  
  # Filter by facility if specified
  if (!is.null(facility_filter) && facility_filter != "all") {
    facility_folders <- facility_folders[tolower(basename(facility_folders)) == tolower(facility_filter)]
  }
  
  all_harborages <- list()
  
  for (folder in facility_folders) {
    facility_code <- basename(folder)
    shp_files <- list.files(folder, pattern = "_harborages\\.shp$", full.names = TRUE)
    
    for (shp_file in shp_files) {
      # Extract emp_num from filename (e.g., "8202_harborages.shp" -> "8202")
      emp_num <- gsub("_harborages\\.shp$", "", basename(shp_file))
      
      # Filter by foreman if specified
      if (!is.null(foreman_filter) && length(foreman_filter) > 0 && 
          !("all" %in% foreman_filter) && !(emp_num %in% foreman_filter)) {
        next
      }
      
      harb_sf <- load_shapefile(shp_file)
      
      if (!is.null(harb_sf)) {
        # Standardize columns
        harb_sf <- standardize_harborage_columns(harb_sf, facility_code)
        all_harborages[[paste0(facility_code, "_", emp_num)]] <- harb_sf
      }
    }
  }
  
  # Combine all harborages
  if (length(all_harborages) > 0) {
    return(do.call(rbind, all_harborages))
  }
  
  return(NULL)
}

#' Standardize harborage column names for consistent display
#' 
#' @param harb_sf sf object with harborage data
#' @param facility_code Facility code to add if missing
#' @return sf object with standardized columns
standardize_harborage_columns <- function(harb_sf, facility_code = NULL) {
  # Essential columns for harborage display
  essential_cols <- c("Sitecode", "Acres", "Foreman", "Facility", "Park_name", "HarbName")
  
  # Add facility code if not present
  if (!"Facility" %in% names(harb_sf) && !is.null(facility_code)) {
    harb_sf$Facility <- toupper(facility_code)
  }
  
  # Ensure all essential columns exist (add NA if missing)
  for (col in essential_cols) {
    if (!(col %in% names(harb_sf))) {
      harb_sf[[col]] <- NA_character_
    }
  }
  
  # Keep only essential columns + geometry
  geom_col <- attr(harb_sf, "sf_column")
  harb_sf[, c(essential_cols, geom_col)]
}

# =============================================================================
# 2. MAP BOUNDS HANDLING
# =============================================================================

#' Calculate safe map bounds for leaflet
#' Handles single point case by using setView instead of fitBounds
#' 
#' @param data sf object or data frame with coordinate columns
#' @param lng_col Name of longitude column (for data frames)
#' @param lat_col Name of latitude column (for data frames)
#' @param single_point_zoom Zoom level for single point (default: 14)
#' @param default_center Default center coordinates c(lng, lat)
#' @param default_zoom Default zoom level
#' @return List with type ("bounds", "center", or "default") and coordinates
#' @export
calculate_map_bounds <- function(data, lng_col = "longitude", lat_col = "latitude",
                                  single_point_zoom = 14, 
                                  default_center = c(-93.2, 45.0),
                                  default_zoom = 9) {
  # Handle NULL or empty data
  if (is.null(data) || (is.data.frame(data) && nrow(data) == 0)) {
    return(list(type = "default", lng = default_center[1], lat = default_center[2], zoom = default_zoom))
  }
  
  # Extract coordinates based on data type
  if (inherits(data, "sf")) {
    coords <- sf::st_coordinates(data)
    if (nrow(coords) == 0) {
      return(list(type = "default", lng = default_center[1], lat = default_center[2], zoom = default_zoom))
    }
    lngs <- coords[, 1]
    lats <- coords[, 2]
  } else {
    lngs <- data[[lng_col]]
    lats <- data[[lat_col]]
  }
  
  # Remove NAs
  valid <- !is.na(lngs) & !is.na(lats)
  lngs <- lngs[valid]
  lats <- lats[valid]
  
  if (length(lngs) == 0) {
    return(list(type = "default", lng = default_center[1], lat = default_center[2], zoom = default_zoom))
  }
  
  # Single point case (or all points at same location)
  lng_range <- max(lngs) - min(lngs)
  lat_range <- max(lats) - min(lats)
  
  if (length(lngs) == 1 || (lng_range < 0.0001 && lat_range < 0.0001)) {
    return(list(
      type = "center",
      lng = mean(lngs),
      lat = mean(lats),
      zoom = single_point_zoom
    ))
  }
  
  # Multiple points - return bounds
  list(
    type = "bounds",
    lng1 = min(lngs),
    lat1 = min(lats),
    lng2 = max(lngs),
    lat2 = max(lats)
  )
}

#' Apply calculated bounds to a leaflet map
#' 
#' @param map Leaflet map object
#' @param bounds Result from calculate_map_bounds()
#' @return Leaflet map with bounds applied
#' @export
apply_map_bounds <- function(map, bounds) {
  if (bounds$type == "center") {
    map %>% setView(lng = bounds$lng, lat = bounds$lat, zoom = bounds$zoom)
  } else if (bounds$type == "bounds") {
    map %>% fitBounds(
      lng1 = bounds$lng1, lat1 = bounds$lat1,
      lng2 = bounds$lng2, lat2 = bounds$lat2
    )
  } else {
    # Default view (MMCD district center)
    map %>% setView(lng = bounds$lng, lat = bounds$lat, zoom = bounds$zoom)
  }
}

# =============================================================================
# 3. COORDINATE VALIDATION
# =============================================================================

#' Validate coordinates and return logical vector
#' 
#' @param data Data frame with coordinate columns
#' @param x_col Name of longitude/x column
#' @param y_col Name of latitude/y column
#' @param lng_bounds Valid longitude range (default: c(-180, 180))
#' @param lat_bounds Valid latitude range (default: c(-90, 90))
#' @return Logical vector indicating valid coordinates
#' @export
validate_coordinates <- function(data, x_col = "x", y_col = "y",
                                  lng_bounds = c(-180, 180),
                                  lat_bounds = c(-90, 90)) {
  lng <- as.numeric(data[[x_col]])
  lat <- as.numeric(data[[y_col]])
  
  !is.na(lng) & !is.na(lat) &
    lng >= lng_bounds[1] & lng <= lng_bounds[2] &
    lat >= lat_bounds[1] & lat <= lat_bounds[2] &
    !(lng == 0 & lat == 0)  # Filter out 0,0 coordinates
}

#' Filter data to only include valid coordinates
#' 
#' @param data Data frame with coordinate columns
#' @param x_col Name of longitude/x column
#' @param y_col Name of latitude/y column
#' @return Filtered data frame
#' @export
filter_valid_coordinates <- function(data, x_col = "x", y_col = "y") {
  valid <- validate_coordinates(data, x_col, y_col)
  data[valid, ]
}

# =============================================================================
# 4. LEAFLET MAP HELPERS
# =============================================================================

#' Create a base leaflet map with standard options
#' 
#' @param data sf object or data frame with coordinate columns (or NULL)
#' @param basemap Basemap type: "osm", "carto", "satellite"
#' @param lng_col Longitude column name (for data frames)
#' @param lat_col Latitude column name (for data frames)
#' @return Leaflet map object with bounds set
#' @export
create_base_map <- function(data = NULL, basemap = "carto", 
                            lng_col = "longitude", lat_col = "latitude") {
  # Get basemap provider
  provider <- switch(basemap,
    "osm" = providers$OpenStreetMap,
    "carto" = providers$CartoDB.Positron,
    "satellite" = providers$Esri.WorldImagery,
    providers$CartoDB.Positron
  )
  
  # Create map
  m <- leaflet(data) %>%
    addProviderTiles(provider, group = "Base Map")
  
  # Calculate and apply bounds if data provided
  if (!is.null(data)) {
    bounds <- calculate_map_bounds(data, lng_col, lat_col)
    m <- apply_map_bounds(m, bounds)
  }
  
  m
}

#' Create an empty placeholder map with a message
#' 
#' @param message_text Message to display on the map
#' @param basemap Basemap type
#' @param center Center coordinates c(lng, lat)
#' @param zoom Zoom level
#' @return Leaflet map with message control
#' @export
create_empty_map <- function(message_text = "No data available", 
                              basemap = "carto",
                              center = c(-93.2, 45.0),
                              zoom = 9) {
  provider <- switch(basemap,
    "osm" = providers$OpenStreetMap,
    "carto" = providers$CartoDB.Positron,
    "satellite" = providers$Esri.WorldImagery,
    providers$CartoDB.Positron
  )
  
  leaflet() %>%
    addProviderTiles(provider) %>%
    setView(lng = center[1], lat = center[2], zoom = zoom) %>%
    addControl(
      html = paste0("<div style='background-color: white; padding: 10px; border-radius: 5px;'><h4>", 
                    message_text, "</h4></div>"),
      position = "topleft"
    )
}

# =============================================================================
# 5. BACKGROUND LAYER HELPERS
# =============================================================================

#' Add facility and zone boundary layers to a map
#' 
#' @param map Leaflet map object
#' @param layers List with facilities and zones sf objects (from load_background_layers)
#' @param show_facilities Whether to show facility boundaries
#' @param show_zones Whether to show zone boundaries
#' @param facility_color Color for facility boundaries
#' @param zone_color Color for zone boundaries
#' @return Leaflet map with layers added
#' @export
add_background_layers <- function(map, layers, 
                                   show_facilities = TRUE, 
                                   show_zones = TRUE,
                                   facility_color = "#2c3e50",
                                   zone_color = "#e74c3c") {
  # Add facility boundaries
  if (show_facilities && !is.null(layers$facilities)) {
    popup_text <- if ("facility" %in% names(layers$facilities)) {
      paste0("<b>Facility:</b> ", layers$facilities$facility)
    } else {
      rep("", nrow(layers$facilities))
    }
    
    map <- map %>%
      addPolylines(
        data = layers$facilities,
        color = facility_color,
        weight = 2.5,
        opacity = 0.8,
        group = "Facility Boundaries",
        popup = popup_text
      )
  }
  
  # Add zone boundaries
  if (show_zones && !is.null(layers$zones)) {
    map <- map %>%
      addPolylines(
        data = layers$zones,
        color = zone_color,
        weight = 2,
        opacity = 0.7,
        dashArray = "5,5",
        group = "Zone Boundaries"
      )
  }
  
  map
}

#' Add harborage polygons to a map
#' 
#' @param map Leaflet map object
#' @param harborages sf object with harborage polygons
#' @param color Fill/stroke color
#' @param group Layer group name
#' @return Leaflet map with harborages added
#' @export
add_harborage_layer <- function(map, harborages, color = "#27ae60", group = "Harborages") {
  if (is.null(harborages) || nrow(harborages) == 0) {
    return(map)
  }
  
  # Create popup text - handle missing columns gracefully
  harb_name <- if ("HarbName" %in% names(harborages)) {
    ifelse(!is.na(harborages$HarbName) & harborages$HarbName != "",
           harborages$HarbName,
           ifelse("Park_name" %in% names(harborages) & !is.na(harborages$Park_name),
                  harborages$Park_name, "Unnamed"))
  } else if ("Park_name" %in% names(harborages)) {
    ifelse(!is.na(harborages$Park_name), harborages$Park_name, "Unnamed")
  } else {
    "Unnamed"
  }
  
  # Build popup with available columns
  popup_parts <- list()
  if ("Sitecode" %in% names(harborages)) {
    popup_parts <- c(popup_parts, paste0("<b>Harborage:</b> ", harborages$Sitecode))
  }
  popup_parts <- c(popup_parts, paste0("<b>Name:</b> ", harb_name))
  if ("Acres" %in% names(harborages)) {
    popup_parts <- c(popup_parts, paste0("<b>Acres:</b> ", round(as.numeric(harborages$Acres), 2)))
  }
  if ("Foreman" %in% names(harborages)) {
    popup_parts <- c(popup_parts, paste0("<b>Foreman:</b> ", harborages$Foreman))
  }
  if ("Facility" %in% names(harborages)) {
    popup_parts <- c(popup_parts, paste0("<b>Facility:</b> ", harborages$Facility))
  }
  
  harb_popup <- do.call(paste, c(popup_parts, sep = "<br>"))
  
  map %>%
    addPolygons(
      data = harborages,
      color = color,
      weight = 1.5,
      opacity = 0.7,
      fillColor = color,
      fillOpacity = 0.15,
      popup = harb_popup,
      highlightOptions = highlightOptions(
        weight = 3,
        color = color,
        fillOpacity = 0.3,
        bringToFront = FALSE
      ),
      group = group
    )
}

#' Add standard layers control to a map
#' 
#' @param map Leaflet map object
#' @param overlay_groups Vector of overlay group names
#' @param collapsed Whether to collapse the control
#' @return Leaflet map with layers control
#' @export
add_layers_control <- function(map, overlay_groups, collapsed = FALSE) {
  map %>%
    addLayersControl(
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = collapsed)
    )
}

#' Add marker type legend (zero vs non-zero species)
#' 
#' @param map Leaflet map object
#' @param position Legend position
#' @param color_present Color for markers with species
#' @param color_zero Color for markers without species
#' @return Leaflet map with legend
#' @export
add_marker_type_legend <- function(map, position = "bottomleft",
                                    color_present = "#1f77b4",
                                    color_zero = "rgba(31, 119, 180, 0.35)") {
  map %>%
    addControl(
      html = sprintf('<div style="background: white; padding: 10px; border: 2px solid grey; border-radius: 5px;">
              <div style="font-weight: bold; margin-bottom: 5px;">Marker Type</div>
              <div style="margin-bottom: 3px;">
                <span style="display: inline-block; width: 16px; height: 16px; border-radius: 50%%; 
                            background-color: %s; border: 2px solid black; vertical-align: middle;"></span>
                <span style="margin-left: 5px;">Target species present</span>
              </div>
              <div>
                <span style="display: inline-block; width: 16px; height: 16px; border-radius: 50%%; 
                            background-color: %s; border: 4px solid black; vertical-align: middle;"></span>
                <span style="margin-left: 5px;">Zero target species</span>
              </div>
            </div>', color_present, color_zero),
      position = position
    )
}

message("geometry_helpers.R loaded - Shared geometry utilities available")
