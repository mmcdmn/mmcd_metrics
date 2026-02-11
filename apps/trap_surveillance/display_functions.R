# =============================================================================
# DISPLAY FUNCTIONS - Trap Surveillance Map Rendering
# =============================================================================
# Renders leaflet choropleth maps by Vector Index Area using pre-calculated data.
# =============================================================================

library(leaflet)
library(htmltools)
library(sf)

# Source color themes
source("../../shared/color_themes.R")

# =============================================================================
# MAIN MAP RENDER
# =============================================================================
render_surveillance_map <- function(combined_data, areas_sf, 
                                     metric_type = "abundance",
                                     infection_metric = "mle",
                                     spp_label = "Total Culex Vectors",
                                     yrwk_label = "",
                                     color_theme = "MMCD") {
  
  if (is.null(areas_sf) || nrow(areas_sf) == 0) {
    return(leaflet() %>%
             addTiles() %>%
             setView(lng = -93.3, lat = 44.95, zoom = 9) %>%
             addControl(html = "<div style='background:white;padding:10px;font-size:14px;'>No area geometries available</div>",
                        position = "topright"))
  }
  
  # Merge data with geometries
  if (!is.null(combined_data) && nrow(combined_data) > 0) {
    map_sf <- areas_sf %>%
      left_join(combined_data, by = "viarea")
  } else {
    map_sf <- areas_sf
    map_sf$total_count <- NA_real_
    map_sf$avg_per_trap <- NA_real_
    map_sf$num_traps <- NA_integer_
    map_sf$infection_rate <- NA_real_
    map_sf$vector_index <- NA_real_
  }
  
  # Determine which metric to color by
  if (metric_type == "vector_index") {
    metric_col <- "vector_index"
    metric_label <- "Vector Index (N × P)"
    format_fn <- function(x) sprintf("%.4f", x)
  } else if (metric_type == "infection") {
    metric_col <- "infection_rate"
    if (infection_metric == "mle") {
      metric_label <- "MLE (Infection Rate)"
    } else {
      metric_label <- "MIR (per 1000)"
      metric_col <- "mir_raw"  # Use raw MIR for display
    }
    format_fn <- function(x) sprintf("%.6f", x)
  } else {
    # Default: abundance
    metric_col <- "avg_per_trap"
    metric_label <- "Avg Mosquitoes/Trap"
    format_fn <- function(x) sprintf("%.1f", x)
  }
  
  # Ensure metric column exists
  if (!metric_col %in% names(map_sf)) {
    map_sf[[metric_col]] <- NA_real_
  }
  
  metric_values <- map_sf[[metric_col]]
  has_data <- !all(is.na(metric_values))
  
  # Get theme palette
  theme_palette <- get_theme_palette(color_theme)
  
  # Color palette
  if (has_data) {
    valid_vals <- metric_values[!is.na(metric_values)]
    if (length(valid_vals) > 0 && max(valid_vals) > min(valid_vals)) {
      color_pal <- if (metric_type %in% c("infection", "vector_index") && !is.null(theme_palette$sequential_heat)) {
        theme_palette$sequential_heat
      } else {
        theme_palette$sequential
      }
      pal <- colorNumeric(palette = color_pal, domain = valid_vals, na.color = "#C0C0C0")
    } else {
      pal <- colorNumeric(palette = c("#ffffcc", "#fd8d3c"), domain = c(0, max(valid_vals, 1)), na.color = "#C0C0C0")
    }
  } else {
    pal <- colorNumeric(palette = c("#ffffcc", "#fd8d3c"), domain = c(0, 1), na.color = "#C0C0C0")
  }
  
  # Build popup text
  popup_text <- build_area_popups(map_sf, metric_type, infection_metric)
  
  # Load background layers
  bg_layers <- load_background_layers()
  
  # Build map
  m <- leaflet(map_sf) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB Light") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite")
  
  # Add area polygons
  m <- m %>%
    addPolygons(
      fillColor = ~pal(get(metric_col)),
      fillOpacity = 0.65,
      color = "#333",
      weight = 2,
      popup = popup_text,
      label = ~paste0(viarea, ": ", ifelse(is.na(get(metric_col)), "No data", format_fn(get(metric_col)))),
      highlightOptions = highlightOptions(
        weight = 4,
        color = "#000",
        fillOpacity = 0.85,
        bringToFront = TRUE
      ),
      group = "Vector Index Areas"
    )
  
  # Add facility boundaries
  if (!is.null(bg_layers$facilities)) {
    facility_color <- if (length(theme_palette$primary) > 0) theme_palette$primary[1] else "#2c3e50"
    m <- m %>%
      addPolylines(
        data = bg_layers$facilities,
        color = facility_color,
        weight = 1.5,
        opacity = 0.6,
        group = "Facilities"
      )
  }
  
  # Add county boundaries
  if (!is.null(bg_layers$counties)) {
    county_color <- if (length(theme_palette$primary) >= 4) theme_palette$primary[4] else "#d62728"
    m <- m %>%
      addPolylines(
        data = bg_layers$counties,
        color = county_color,
        weight = 2.5,
        opacity = 0.7,
        group = "Counties"
      )
  }
  
  # Add zone boundaries
  if (!is.null(bg_layers$zones)) {
    m <- m %>%
      addPolylines(
        data = bg_layers$zones,
        color = "#555",
        weight = 1,
        opacity = 0.4,
        group = "Zones"
      )
  }
  
  # Legend
  if (has_data) {
    m <- m %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~get(metric_col),
        title = metric_label,
        opacity = 0.7
      )
  }
  
  # Title
  title_html <- sprintf(
    "<div style='background:white;padding:8px 12px;border-radius:5px;font-size:13px;border:1px solid #ccc;'>
       <strong>%s</strong> | %s | Week %s
     </div>",
    metric_label, spp_label, yrwk_label
  )
  m <- m %>%
    addControl(html = title_html, position = "topleft")
  
  # Layer control
  m <- m %>%
    addLayersControl(
      baseGroups = c("CartoDB Light", "OpenStreetMap", "Satellite"),
      overlayGroups = c("Vector Index Areas", "Facilities", "Counties", "Zones"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    hideGroup("Zones")
  
  m
}

# =============================================================================
# POPUP BUILDER
# =============================================================================
build_area_popups <- function(map_sf, metric_type, infection_metric) {
  sapply(seq_len(nrow(map_sf)), function(i) {
    row <- map_sf[i, , drop = FALSE]
    viarea <- row$viarea
    
    # Base info
    html <- sprintf("<strong style='font-size:14px;'>%s</strong><br/>", viarea)
    html <- paste0(html, sprintf("Sections: %s<br/>", 
                                  ifelse("num_sections" %in% names(row), as.character(row$num_sections), "?")))
    html <- paste0(html, "<hr style='margin:5px 0;'/>")
    
    # Abundance
    if ("total_count" %in% names(row) && !is.na(row$total_count)) {
      html <- paste0(html, sprintf(
        "<strong>Abundance:</strong><br/>Total count: %s<br/>Traps: %s<br/>Avg/trap: <strong>%.1f</strong><br/>",
        format(as.numeric(row$total_count), big.mark = ","),
        as.character(row$num_traps),
        as.numeric(row$avg_per_trap)
      ))
    } else {
      html <- paste0(html, "<em>No abundance data this week</em><br/>")
    }
    
    html <- paste0(html, "<hr style='margin:5px 0;'/>")
    
    # Infection rate
    if ("infection_rate" %in% names(row) && !is.na(row$infection_rate)) {
      rate_val <- as.numeric(row$infection_rate)
      if (infection_metric == "mle") {
        html <- paste0(html, sprintf(
          "<strong>MLE:</strong> %.6f<br/>",
          rate_val
        ))
        if ("rate_lower" %in% names(row) && !is.na(row$rate_lower)) {
          html <- paste0(html, sprintf("95%% CI: (%.6f - %.6f)<br/>",
                                        as.numeric(row$rate_lower), as.numeric(row$rate_upper)))
        }
      } else {
        mir_val <- if ("mir_raw" %in% names(row) && !is.na(row$mir_raw)) as.numeric(row$mir_raw) else rate_val * 1000
        html <- paste0(html, sprintf("<strong>MIR:</strong> %.2f per 1000<br/>", mir_val))
        if ("positive" %in% names(row) && !is.na(row$positive)) {
          html <- paste0(html, sprintf("Positive: %s / %s pools (%s mosquitoes)<br/>",
                                        row$positive, row$total_pools, 
                                        format(as.numeric(row$total_mosquitoes), big.mark = ",")))
        }
      }
    } else {
      html <- paste0(html, "<em>No infection data this week</em><br/>")
    }
    
    # Vector Index
    if ("vector_index" %in% names(row) && !is.na(row$vector_index)) {
      html <- paste0(html, "<hr style='margin:5px 0;'/>")
      html <- paste0(html, sprintf(
        "<strong>Vector Index (N×P):</strong> <span style='font-size:14px;color:#d63031;'>%.4f</span>",
        as.numeric(row$vector_index)
      ))
    }
    
    html
  })
}
