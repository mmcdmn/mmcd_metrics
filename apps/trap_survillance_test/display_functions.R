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

# Create static SF-based map with OpenStreetMap background
render_vector_map_sf <- function(sections_sf, trap_df = NULL, species_label = "selected species") {
  if (is.null(sections_sf) || nrow(sections_sf) == 0) {
    return(ggplot() + 
           theme_void() + 
           labs(title = "No data available") +
           theme(plot.title = element_text(hjust = 0.5)))
  }

  # Load background layers
  bg_layers <- load_background_layers()
  
  # Start with base map with OpenStreetMap background
  p <- ggplot()
  
  # Add OpenStreetMap background
  p <- p + annotation_map_tile(type = "osm", zoom = 10, alpha = 0.3)
  
  # Add background layers (lighter since we have OSM)
  if (!is.null(bg_layers$counties)) {
    p <- p + geom_sf(data = bg_layers$counties, 
                     fill = "transparent", 
                     color = "gray70", 
                     size = 0.3, 
                     alpha = 0.5)
  }
  
  if (!is.null(bg_layers$facilities)) {
    p <- p + geom_sf(data = bg_layers$facilities, 
                     fill = "transparent", 
                     color = "gray50", 
                     size = 0.4, 
                     linetype = "dashed",
                     alpha = 0.7)
  }
  
  # Add sections with vector index coloring
  if (!is.null(sections_sf$vector_index)) {
    p <- p + geom_sf(data = sections_sf, 
                     aes(fill = vector_index), 
                     color = "white", 
                     size = 0.1, 
                     alpha = 0.7) +
             scale_fill_viridis_c(name = "Vector\nIndex", 
                                option = "plasma", 
                                na.value = "lightgray",
                                trans = "sqrt",  # Better for skewed data
                                labels = number_format(accuracy = 0.1))
  } else {
    p <- p + geom_sf(data = sections_sf, 
                     fill = "lightblue", 
                     color = "white", 
                     size = 0.1, 
                     alpha = 0.6)
  }
  
  # Add traps if provided
  if (!is.null(trap_df) && nrow(trap_df) > 0) {
    traps_sf <- st_as_sf(trap_df, coords = c("lon", "lat"), crs = 4326)
    
    # Map survtype to colors
    trap_colors <- c("4" = "#2166ac", "5" = "#762a83", "6" = "#5aae61")
    trap_labels <- c("4" = "Elevated CO2", "5" = "Gravid Trap", "6" = "CO2 Overnight")
    
    # Add trap type colors
    traps_sf$trap_color <- trap_colors[as.character(traps_sf$survtype)]
    traps_sf$trap_label <- trap_labels[as.character(traps_sf$survtype)]
    
    p <- p + geom_sf(data = traps_sf, 
                     aes(color = trap_label, size = species_count), 
                     alpha = 0.8,
                     stroke = 1) +
             scale_color_manual(name = "Trap Type", 
                              values = trap_colors,
                              labels = trap_labels) +
             scale_size_continuous(name = paste(species_label, "\nCount"), 
                                 range = c(1.5, 4),
                                 guide = guide_legend(override.aes = list(alpha = 1)))
  }
  
  # Style the map for better zoom functionality
  p <- p + 
    theme_void() +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      legend.background = element_rect(fill = "white", color = "gray"),
      legend.margin = margin(5, 5, 5, 5),
      panel.background = element_rect(fill = "lightblue", color = NA),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.margin = margin(5, 5, 5, 5),
      axis.text = element_text(size = 8),  # Show coordinates for zoom reference
      axis.title = element_text(size = 9)
    ) +
    labs(
      title = "Mosquito Vector Index by Section",
      subtitle = paste("Based on", species_label, "- k-NN Inverse Distance Weighting"),
      caption = paste("Analysis Date:", Sys.Date(), "| Background: OpenStreetMap"),
      x = "Longitude", 
      y = "Latitude"
    ) +
    coord_sf(expand = FALSE, datum = st_crs(4326)) +
    
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
                                   "Vector Index: ", round(vector_index, 2), "<br/>",
                                   "Nearest Trap Total: ", nearest_trap_count, "<br/>",
                                   "Last Inspection: ", last_inspection)) %>%
    addLegend(position = "bottomright", pal = pal, values = section_df$vector_index, title = "Vector Index")
  
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
