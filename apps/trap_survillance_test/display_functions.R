library(leaflet)
library(htmltools)
library(ggplot2)
library(sf)
library(viridis)
library(scales)
library(ggspatial)
library(plotly)

# Source color themes
source("../../shared/color_themes.R")

# Helper to safely get column value handling .x/.y suffixes
get_col_safe <- function(df, col_name) {
  if (col_name %in% names(df)) {
    return(df[[col_name]])
  } else if (paste0(col_name, ".x") %in% names(df)) {
    return(df[[paste0(col_name, ".x")]])
  } else if (paste0(col_name, ".y") %in% names(df)) {
    return(df[[paste0(col_name, ".y")]])
  } else {
    return(rep(NA, nrow(df)))
  }
}

# Create interactive Leaflet map with basemap and tooltips
render_vector_map_leaflet <- function(sections_sf, trap_df = NULL, species_label = "selected species", metric_type = "popindex", color_theme = "MMCD") {
  if (is.null(sections_sf) || nrow(sections_sf) == 0) {
    return(leaflet() %>% 
           addTiles() %>% 
           setView(lng = -93.2, lat = 44.9, zoom = 9) %>%
           addControl(html = "<div style='background:white;padding:10px;'>No data available</div>", 
                     position = "topright"))
  }
  
  # Determine metric column and label
  if (metric_type == "mle") {
    metric_col <- "mle"
    metric_label <- "MLE (per 1000)"
  } else if (metric_type == "vector_index") {
    metric_col <- "vector_index_metric"
    metric_label <- "Vector Index (N × P)"
  } else {
    metric_col <- "vector_index"
    metric_label <- "Population Index"
  }
  
  # Check if metric column exists
  if (!metric_col %in% names(sections_sf)) {
    return(leaflet() %>% 
           addTiles() %>% 
           setView(lng = -93.2, lat = 44.9, zoom = 9) %>%
           addControl(html = sprintf("<div style='background:white;padding:10px;'>No %s data available</div>", metric_label), 
                     position = "topright"))
  }
  
  # Get selected color theme
  theme_palette <- get_theme_palette(color_theme)
  
  # Create color palette using theme's sequential colors
  # Use viridis-style for popindex, YlOrRd-style for MLE/Vector Index
  metric_values <- sections_sf[[metric_col]]
  
  if (metric_type == "mle" || metric_type == "vector_index") {
    # Use heat colors (YlOrRd) for MLE and Vector Index if available, otherwise fallback to sequential
    color_pal <- if (!is.null(theme_palette$sequential_heat)) theme_palette$sequential_heat else theme_palette$sequential
  } else {
    # Use regular sequential colors (viridis-style) for population index
    color_pal <- theme_palette$sequential
  }
  
  pal <- colorNumeric(
    palette = color_pal,
    domain = metric_values,
    na.color = "#808080"
  )
  
  # Create popups with detailed information - use a separate vector
  # Safely extract columns handling .x/.y suffixes from joins
  sectcode <- get_col_safe(sections_sf, "sectcode")
  facility <- get_col_safe(sections_sf, "facility")
  metric_val <- sections_sf[[metric_col]]
  
  if (metric_type == "mle") {
    mle_lower <- get_col_safe(sections_sf, "mle_lower")
    mle_upper <- get_col_safe(sections_sf, "mle_upper")
    num_pools <- get_col_safe(sections_sf, "num_pools")
    num_positive <- get_col_safe(sections_sf, "num_positive")
    total_tested <- get_col_safe(sections_sf, "total_tested")
    pool_date <- get_col_safe(sections_sf, "nearest_pool_date")
    
    popup_text <- sprintf(
      "<strong>Section: %s</strong><br/>Facility: %s<br/>MLE: <strong>%.2f</strong> per 1000<br/>95%% CI: (%.2f - %.2f)<br/><hr/>Based on %.0f pools at %.0f locations<br/>Nearest: %.0f meters<br/>Farthest: %.0f meters<br/><hr/>Total pools from these traps: %.0f<br/>Pools with positive results: %.0f<br/>Total mosquitoes tested: %.0f<br/>Most recent pool test: %s",
      ifelse(is.na(sectcode), "Unknown", sectcode),
      ifelse(is.na(facility) | facility == "", "Unknown", facility),
      ifelse(is.na(metric_val), 0, metric_val),
      ifelse(is.na(mle_lower), 0, mle_lower),
      ifelse(is.na(mle_upper), 0, mle_upper),
      ifelse(is.na(sections_sf$num_traps), 0, as.numeric(sections_sf$num_traps)),
      ifelse(is.na(sections_sf$num_locations), 0, as.numeric(sections_sf$num_locations)),
      ifelse(is.na(sections_sf$nearest_dist), 0, as.numeric(sections_sf$nearest_dist)),
      ifelse(is.na(sections_sf$farthest_dist), 0, as.numeric(sections_sf$farthest_dist)),
      ifelse(is.na(num_pools), 0, as.numeric(num_pools)),
      ifelse(is.na(num_positive), 0, as.numeric(num_positive)),
      ifelse(is.na(total_tested), 0, as.numeric(total_tested)),
      ifelse(is.na(pool_date), "N/A", as.character(pool_date))
    )
  } else if (metric_type == "vector_index") {
    # Vector Index combines both trap and pool data
    N <- get_col_safe(sections_sf, "N")
    P <- get_col_safe(sections_sf, "P")
    mle <- get_col_safe(sections_sf, "mle")
    nearest_trap_count <- get_col_safe(sections_sf, "nearest_trap_count")
    num_pools <- get_col_safe(sections_sf, "num_pools")
    num_positive <- get_col_safe(sections_sf, "num_positive")
    total_tested <- get_col_safe(sections_sf, "total_tested")
    last_inspection <- get_col_safe(sections_sf, "last_inspection")
    pool_date <- get_col_safe(sections_sf, "nearest_pool_date")
    
    popup_text <- sprintf(
      "<strong>Section: %s</strong><br/>Facility: %s<br/><strong>Vector Index: %.4f</strong><br/><hr/>N (avg mosquitoes/trap): %.2f<br/>P (infection rate): %.4f<br/>MLE: %.2f per 1000<br/><hr/>Traps: %.0f mosquitoes<br/>Pools: %.0f tested, %.0f positive<br/>Last inspection: %s<br/>Last pool: %s",
      ifelse(is.na(sectcode), "Unknown", sectcode),
      ifelse(is.na(facility) | facility == "", "Unknown", facility),
      ifelse(is.na(metric_val), 0, metric_val),
      ifelse(is.na(N), 0, N),
      ifelse(is.na(P), 0, P),
      ifelse(is.na(mle), 0, mle),
      ifelse(is.na(nearest_trap_count), 0, nearest_trap_count),
      ifelse(is.na(num_pools), 0, num_pools),
      ifelse(is.na(num_positive), 0, num_positive),
      ifelse(is.na(last_inspection), "N/A", as.character(last_inspection)),
      ifelse(is.na(pool_date), "N/A", as.character(pool_date))
    )
  } else {
    nearest_trap_count <- get_col_safe(sections_sf, "nearest_trap_count")
    last_inspection <- get_col_safe(sections_sf, "last_inspection")
    
    popup_text <- sprintf(
      "<strong>Section: %s</strong><br/>Facility: %s<br/>Population Index: <strong>%.2f</strong><br/>Nearest trap count: %d<br/>Last inspection: %s",
      ifelse(is.na(sectcode), "Unknown", sectcode),
      ifelse(is.na(facility) | facility == "", "Unknown", facility),
      ifelse(is.na(metric_val), 0, metric_val),
      ifelse(is.na(nearest_trap_count), 0, nearest_trap_count),
      ifelse(is.na(last_inspection), "N/A", as.character(last_inspection))
    )
  }
  
  # Load background layers
  bg_layers <- load_background_layers()
  
  # Create base map with OpenStreetMap tiles
  m <- leaflet(sections_sf) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB Light") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite")
  
  # Add section polygons with color and popup
  m <- m %>%
    addPolygons(
      fillColor = ~pal(get(metric_col)),
      fillOpacity = 0.7,
      color = "#444",
      weight = 1,
      popup = popup_text,
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#666",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      group = "Sections"
    )
  
  # Add facility boundaries - use theme's primary color
  if (!is.null(bg_layers$facilities)) {
    facility_color <- if (length(theme_palette$primary) > 0) theme_palette$primary[1] else "#2c3e50"
    m <- m %>%
      addPolylines(
        data = bg_layers$facilities,
        color = facility_color,
        weight = 2,
        opacity = 0.8,
        group = "Facilities"
      )
  }
  
  # Add county boundaries - bolder with theme's accent color
  if (!is.null(bg_layers$counties)) {
    # Use theme's red/accent color if available (4th color in primary palette typically)
    county_color <- if (length(theme_palette$primary) >= 4) theme_palette$primary[4] else "#d62728"
    m <- m %>%
      addPolylines(
        data = bg_layers$counties,
        color = county_color,
        weight = 3,
        opacity = 0.8,
        group = "Counties"
      )
  }
  
  # Add Vector Index Areas (2025) - distinct areas for surveillance
  if (!is.null(bg_layers$vector_index_areas)) {
    # Use purple/violet color for vector index areas
    vector_index_color <- "#9467bd"  # Purple from standard color palette
    
    # Create popup labels with area names and section counts
    popup_text <- sprintf(
      "<strong>Vector Index Area: %s</strong><br/>Sections: %d<br/>Total Area: %.2f sq mi",
      bg_layers$vector_index_areas$VIareaA,
      bg_layers$vector_index_areas$num_sections,
      bg_layers$vector_index_areas$total_area
    )
    
    m <- m %>%
      addPolygons(
        data = bg_layers$vector_index_areas,
        color = vector_index_color,
        weight = 3,
        opacity = 0.9,
        fillColor = vector_index_color,
        fillOpacity = 0.15,
        popup = popup_text,
        highlightOptions = highlightOptions(
          weight = 4,
          color = vector_index_color,
          fillOpacity = 0.3,
          bringToFront = FALSE
        ),
        group = "Vector Index Areas"
      )
  }
  
  # Add trap location markers
  if (!is.null(trap_df) && nrow(trap_df) > 0) {
    # Create trap popup text - check if MLE data is available
    if ("mle" %in% names(trap_df)) {
      # MLE trap markers - show detailed pool statistics
      trap_popup <- sprintf(
        "<strong>Trap: %s</strong><br/>Facility: %s<br/><hr/><strong>MLE: %.2f per 1000</strong><br/>95%% CI: (%.2f - %.2f)<br/><hr/>Total pools from this trap: %.0f<br/>Pools with positive results: %.0f<br/>Total mosquitoes tested: %.0f<br/>Inspection date: %s",
        trap_df$sampnum_yr,
        trap_df$facility,
        as.numeric(trap_df$mle),
        as.numeric(trap_df$mle_lower),
        as.numeric(trap_df$mle_upper),
        as.numeric(trap_df$num_pools),
        as.numeric(trap_df$num_positive),
        as.numeric(trap_df$total_mosquitoes),
        as.character(trap_df$inspdate)
      )
    } else {
      # Population index trap markers - show mosquito counts
      trap_popup <- sprintf(
        "<strong>Trap Location</strong><br/>Facility: %s<br/>Type: %s<br/>Count: %.0f<br/>Date: %s",
        trap_df$facility,
        trap_df$survtype,
        as.numeric(trap_df$species_count),
        trap_df$inspdate
      )
    }
    
    m <- m %>%
      addCircleMarkers(
        data = st_as_sf(trap_df, coords = c("lon", "lat"), crs = 4326),
        radius = 4,
        color = "#000000",
        fillColor = "#FFD700",  # Gold color for visibility
        fillOpacity = 0.8,
        weight = 1,
        popup = trap_popup,
        group = "Trap Locations"
      )
  }
  
  # Add legend
  m <- m %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~get(metric_col),
      title = metric_label,
      opacity = 0.7
    )
  
  # Add layer control
  m <- m %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB Light", "Satellite"),
      overlayGroups = c("Sections", "Trap Locations", "Facilities", "Counties", "Vector Index Areas"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(m)
}

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
  
  # Load Vector Index Areas (2025) - these are in UTM, need conversion to WGS84
  vector_index_areas_path <- file.path(q_to_r_path, "VectorIndexAreasA2025.shp")
  if (file.exists(vector_index_areas_path)) {
    layers$vector_index_areas <- st_read(vector_index_areas_path, quiet = TRUE)
    # Convert from UTM to WGS84 (EPSG:4326) for leaflet
    if (st_crs(layers$vector_index_areas)$epsg != 4326) {
      layers$vector_index_areas <- st_transform(layers$vector_index_areas, 4326)
    }
    # Dissolve individual sections into area groups by VIareaA attribute
    if ("VIareaA" %in% names(layers$vector_index_areas)) {
      # Fix invalid geometries before dissolving
      layers$vector_index_areas <- st_make_valid(layers$vector_index_areas)
      # Temporarily disable s2 for union operation to avoid strict geometry checks
      old_use_s2 <- sf_use_s2()
      sf_use_s2(FALSE)
      layers$vector_index_areas <- layers$vector_index_areas %>%
        group_by(VIareaA) %>%
        summarize(
          num_sections = n(),
          total_area = sum(AREA, na.rm = TRUE),
          .groups = "drop"
        )
      sf_use_s2(old_use_s2)  # Restore previous s2 setting
    }
  }
  
  return(layers)
}
