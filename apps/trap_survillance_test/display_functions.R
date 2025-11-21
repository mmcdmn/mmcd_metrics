library(leaflet)
library(htmltools)
library(ggplot2)
library(sf)
library(viridis)
library(scales)
library(ggspatial)

# Load background layers for static maps
load_background_layers <- function() {
  # Try shared Q_to_R location first, then mosquito_surveillance_map
  q_to_r_path <- file.path("..", "..", "shared", "Q_to_R", "data")
  mosquito_map_path <- file.path("..", "mosquito_surveillance_map", "shp")
  
  layers <- list()
  
  # Try to load facilities and zones from Q_to_R first
  facilities_q_path <- file.path(q_to_r_path, "facility_boundaries.shp")
  zones_q_path <- file.path(q_to_r_path, "zone_boundaries.shp")
  
  if (file.exists(facilities_q_path)) {
    layers$facilities <- st_read(facilities_q_path, quiet = TRUE)
  }
  
  if (file.exists(zones_q_path)) {
    layers$zones <- st_read(zones_q_path, quiet = TRUE)
  }
  
  # Try mosquito_surveillance_map for counties if not found above
  counties_path <- file.path(mosquito_map_path, "Counties_4326.shp")
  if (file.exists(counties_path)) {
    layers$counties <- st_read(counties_path, quiet = TRUE)
  }
  
  # Fallback facility areas if not found in Q_to_R
  if (is.null(layers$facilities)) {
    facilities_path <- file.path(mosquito_map_path, "FacilityArea_4326.shp")
    if (file.exists(facilities_path)) {
      layers$facilities <- st_read(facilities_path, quiet = TRUE)
    }
  }
  
  # Fallback zone boundaries if not found in Q_to_R  
  if (is.null(layers$zones)) {
    zones_path <- file.path(mosquito_map_path, "P1zonebdry_4326.shp")
    if (file.exists(zones_path)) {
      layers$zones <- st_read(zones_path, quiet = TRUE)
    }
  }
  
  return(layers)
}

# Create interactive SF-based map with OpenStreetMap background
render_vector_map_sf <- function(sections_sf, trap_df = NULL, species_label = "selected species") {
  # Better error handling for empty/null data
  if (is.null(sections_sf)) {
    message("ERROR: sections_sf is NULL")
    return(ggplot() + 
           theme_void() + 
           geom_text(aes(x = 0.5, y = 0.5, label = "No section data available.\nCheck your filters and date selection."),
                    size = 6, color = "red") +
           theme(plot.title = element_text(hjust = 0.5)) +
           labs(title = "No Data Available"))
  }
  
  if (nrow(sections_sf) == 0) {
    message("ERROR: sections_sf has 0 rows")
    return(ggplot() + 
           theme_void() + 
           geom_text(aes(x = 0.5, y = 0.5, label = "No sections found with current filters.\nTry:\n- More recent date\n- Different facility\n- 'All' species"),
                    size = 5, color = "red") +
           theme(plot.title = element_text(hjust = 0.5)) +
           labs(title = "No Data Available"))
  }
  
  # Check if geometry column exists
  if (!"geometry" %in% names(sections_sf)) {
    message("ERROR: no geometry column in sections_sf")
    message("Columns:", paste(names(sections_sf), collapse=", "))
    return(ggplot() + 
           theme_void() + 
           geom_text(aes(x = 0.5, y = 0.5, label = "Error: Missing geometry data"),
                    size = 6, color = "red"))
  }
  
  # Log what we're rendering
  message(sprintf("Rendering map with %d sections and %d traps", 
                 nrow(sections_sf), 
                 if(is.null(trap_df)) 0 else nrow(trap_df)))
  message(sprintf("Bounding box: %s", paste(round(st_bbox(sections_sf), 2), collapse=", ")))
  message(sprintf("Population index range: %.2f to %.2f", 
                 min(sections_sf$vector_index, na.rm=TRUE),
                 max(sections_sf$vector_index, na.rm=TRUE)))

  # Load background layers - simplified for debugging
  bg_layers <- load_background_layers()
  
  # Get bounding box for sections to set map extent
  bbox <- st_bbox(sections_sf)
  
  # Create base plot and explicitly set coordinates first so tiles know the extent
  p <- ggplot() +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             crs = st_crs(4326),
             expand = FALSE)
  
  # Add basemap tiles FIRST (they render at the back)
  # Use cartodark for BLACK background with high contrast, zoomin=2 for more detail
  p <- p + annotation_map_tile(type = "cartodark", zoomin = 2, alpha = 1.0, progress = "none", quiet = TRUE)
  
  # Then add sections with population index coloring on TOP of tiles
  if (!is.null(sections_sf$vector_index)) {
    p <- p + geom_sf(data = sections_sf, 
                     aes(fill = vector_index), 
                     color = "gray40",  # Less prominent borders
                     size = 0.05,       # Thinner lines
                     alpha = 0.4,       # Much more transparent to see basemap
                     inherit.aes = FALSE) +  # Don't inherit coord_sf
             scale_fill_viridis_c(name = "Population\nIndex", 
                                option = "plasma", 
                                na.value = "lightgray",
                                trans = "sqrt",
                                labels = number_format(accuracy = 0.1))
  } else {
    p <- p + geom_sf(data = sections_sf, 
                     fill = "transparent", 
                     color = "black", 
                     size = 0.1, 
                     alpha = 0.55,
                     inherit.aes = FALSE)
  }
  
  # Add background county lines for context (very light)
  if (!is.null(bg_layers$counties)) {
    p <- p + geom_sf(data = bg_layers$counties, 
                     fill = "transparent", 
                     color = "gray80", 
                     size = 0.2, 
                     alpha = 0.3)
  }
  
  # Add facility boundaries for reference (very subtle)
  if (!is.null(bg_layers$facilities)) {
    p <- p + geom_sf(data = bg_layers$facilities, 
                     fill = "transparent", 
                     color = "gray40", 
                     size = 0.3, 
                     linetype = "dashed",
                     alpha = 0.6)
  }
  
  # Add traps if provided
  if (!is.null(trap_df) && nrow(trap_df) > 0) {
    traps_sf <- st_as_sf(trap_df, coords = c("lon", "lat"), crs = 4326)
    
    # Map survtype to colors and labels
    trap_colors <- c("Elevated CO2" = "#2166ac", "Gravid Trap" = "#762a83", "CO2 Overnight" = "#5aae61")
    trap_type_map <- c("4" = "Elevated CO2", "5" = "Gravid Trap", "6" = "CO2 Overnight")
    
    # Add trap type labels
    traps_sf$trap_label <- trap_type_map[as.character(traps_sf$survtype)]
    
    p <- p + geom_sf(data = traps_sf, 
                     aes(color = trap_label, size = species_count), 
                     alpha = 0.9,  # Keep traps highly visible
                     stroke = 1) +
             scale_color_manual(name = "Trap Type", 
                              values = trap_colors) +
             scale_size_continuous(name = paste(species_label, "\nCount"), 
                                 range = c(2, 5),  # Larger for better visibility
                                 guide = guide_legend(override.aes = list(alpha = 1)))
  }
  
  # Style the map
  p <- p + 
    theme_void() +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      legend.background = element_rect(fill = "white", color = "gray"),
      legend.margin = margin(5, 5, 5, 5),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.margin = margin(5, 5, 5, 5),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9)
    ) +
    labs(
      title = "Mosquito Population Index by Section (Live Data)",
      subtitle = paste("Based on", species_label, "- Updates on Refresh"),
      caption = paste("Analysis Date:", Sys.Date(), "| Click refresh to update data"),
      x = "Longitude", 
      y = "Latitude"
    ) +
    
    # Add scale bar and north arrow
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(location = "tr", which_north = "true", 
                          pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                          style = north_arrow_fancy_orienteering)
  
  return(p)
}

# Original leaflet function (keep for compatibility)
render_vector_map <- function(section_df, trap_df = NULL, species_label = "selected species") {
  if (is.null(section_df) || nrow(section_df) == 0) {
    return(leaflet() %>% addTiles() %>% setView(lng = -93.2, lat = 44.9, zoom = 9))
  }

  # Color palette for sections
  pal <- colorNumeric("YlOrRd", domain = section_df$vector_index, na.color = "#f0f0f0")
  
  # Start map with sections
  m <- leaflet() %>% 
    addTiles() %>%
    addCircleMarkers(data = section_df,
                     lng = ~lon, lat = ~lat,
                     radius = ~pmax(4, log1p(vector_index) * 4),
                     color = ~pal(vector_index), 
                     fillOpacity = 0.7,
                     stroke = TRUE,
                     weight = 1,
                     group = "Sections",
                     popup = ~paste0("<strong>Section: ", sectcode, "</strong><br/>",
                                   "Population Index: ", round(vector_index, 2), "<br/>",
                                   "Nearest Trap Total: ", nearest_trap_count, "<br/>",
                                   "Last Inspection: ", last_inspection)) %>%
    addLegend(position = "bottomright", pal = pal, values = section_df$vector_index, title = "Population Index")
  
  # Add traps if provided
  if (!is.null(trap_df) && nrow(trap_df) > 0) {
    # Add species label column to trap_df for popup
    trap_df$species_label <- species_label
    
    # Map survtype codes to names
    trap_type_names <- c("4" = "Elevated CO2", "5" = "Gravid Trap", "6" = "CO2 Overnight")
    trap_df$survtype_name <- trap_type_names[as.character(trap_df$survtype)]
    
    m <- m %>%
      addCircleMarkers(data = trap_df,
                       lng = ~lon, lat = ~lat,
                       radius = 3,
                       color = "#2166ac",
                       fillColor = "#2166ac",
                       fillOpacity = 0.6,
                       stroke = TRUE,
                       weight = 1,
                       group = "Traps",
                       popup = ~paste0("<strong>Trap: ", ainspecnum, "</strong><br/>",
                                     "Facility: ", facility, "<br/>",
                                     "Type: ", survtype_name, "<br/>",
                                     "Inspection Date: ", inspdate, "<br/>",
                                     species_label, " Count: ", species_count)) %>%
      addLayersControl(
        overlayGroups = c("Sections", "Traps"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  return(m)
}
