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
                                     color_theme = "MMCD",
                                     all_traps = NULL) {
  
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
  
  # NON-LINEAR BREAKS — more color differentiation at low values where most data lives
  # Uses colorBin so each bin gets a distinct color step.
  if (metric_type == "abundance") {
    fixed_breaks <- c(0, 1, 3, 7, 12, 20, 30)
  } else if (metric_type == "infection") {
    if (infection_metric == "mle") {
      fixed_breaks <- c(0, 0.001, 0.005, 0.01, 0.02, 0.04, 0.06)
    } else {
      fixed_breaks <- c(0, 2, 5, 15, 30, 60, 100)
    }
  } else if (metric_type == "vector_index") {
    fixed_breaks <- c(0, 0.02, 0.08, 0.2, 0.5, 1.0, 2.0)
  } else {
    fixed_breaks <- c(0, 1, 3, 7, 12, 20, 30)
  }
  legend_max <- max(fixed_breaks)
  
  # Clamp metric values so anything beyond legend_max still gets the darkest color
  map_sf[[paste0(metric_col, "_clamped")]] <- pmin(
    ifelse(is.na(map_sf[[metric_col]]), NA_real_, map_sf[[metric_col]]),
    legend_max
  )
  
  # Yellow-to-red heat palette with colorBin for distinct steps
  heat_colors <- c("#ffffcc", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#800026")
  pal <- colorBin(palette = heat_colors, bins = fixed_breaks, na.color = "#C0C0C0")
  
  # Build popup text (uses real unclamped values)
  popup_text <- build_area_popups(map_sf, metric_type, infection_metric)
  
  # Use the clamped column name for coloring
  clamped_col <- paste0(metric_col, "_clamped")
  
  # Load background layers
  bg_layers <- load_background_layers()
  
  # Build map — custom panes keep traps always on top of polygons
  m <- leaflet(map_sf) %>%
    addMapPane("areas", zIndex = 410) %>%
    addMapPane("traps", zIndex = 450) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB Light") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite")
  
  # Add area polygons (in "areas" pane — below traps)
  m <- m %>%
    addPolygons(
      fillColor = ~pal(get(clamped_col)),
      fillOpacity = 0.65,
      color = "#333",
      weight = 2,
      popup = popup_text,
      label = ~paste0(viarea, ": ", ifelse(is.na(get(metric_col)), "No data available", format_fn(get(metric_col)))),
      highlightOptions = highlightOptions(
        weight = 4,
        color = "#000",
        fillOpacity = 0.85,
        bringToFront = FALSE
      ),
      options = pathOptions(pane = "areas"),
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
  
  # Legend — build from bin breaks (non-linear)
  if (has_data) {
    # Each bin needs a color swatch and label like "0 – 1", "1 – 3", etc.
    n_bins <- length(fixed_breaks) - 1
    legend_colors <- heat_colors[seq_len(n_bins)]
    
    # Format break labels based on metric precision
    fmt <- if (metric_type == "infection" && infection_metric != "mir") {
      function(x) sprintf("%.3f", x)
    } else if (metric_type == "vector_index") {
      function(x) sprintf("%.2f", x)
    } else {
      function(x) sprintf("%.0f", x)
    }
    
    legend_labels <- sapply(seq_len(n_bins), function(i) {
      lo <- fmt(fixed_breaks[i])
      hi <- fmt(fixed_breaks[i + 1])
      if (i == n_bins) paste0(lo, " – ", hi, "+") else paste0(lo, " – ", hi)
    })
    
    m <- m %>%
      addLegend(
        position = "bottomright",
        colors = rev(legend_colors),
        labels = rev(legend_labels),
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
  
  # =========================================================================
  # TRAP MARKERS — all trap locations from shapefile
  # =========================================================================
  
  overlay_groups <- c("Vector Index Areas", "Facilities", "Counties", "Zones")
  
  if (!is.null(all_traps) && nrow(all_traps) > 0) {
    trap_popup <- sprintf(
      "<strong>Trap Location</strong><br/>Facility: %s<br/>Type: %s<br/>Survey Type: %s<br/>Date: %s",
      all_traps$facility,
      all_traps$trap_type_label,
      all_traps$survtype,
      as.character(all_traps$inspdate)
    )
    
    m <- m %>%
      addCircleMarkers(
        data = all_traps,
        radius = 4,
        color = "#333",
        fillColor = "#FFD700",
        fillOpacity = 0.8,
        weight = 1,
        popup = trap_popup,
        options = pathOptions(pane = "traps"),
        group = "Trap Locations"
      )
    overlay_groups <- c(overlay_groups, "Trap Locations")
  }
  
  # Layer control
  m <- m %>%
    addLayersControl(
      baseGroups = c("CartoDB Light", "OpenStreetMap", "Satellite"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    hideGroup("Zones")
  
  m
}

# =============================================================================
# POPUP BUILDER
# =============================================================================
# =============================================================================
# COMPARISON MAP - shows delta between two weeks
# =============================================================================
render_comparison_map <- function(data_a, data_b, areas_sf,
                                  metric_type = "abundance",
                                  infection_metric = "mle",
                                  spp_label = "",
                                  yrwk_a = "", yrwk_b = "",
                                  all_traps = NULL) {
  
  if (is.null(areas_sf) || nrow(areas_sf) == 0 ||
      is.null(data_a)   || nrow(data_a) == 0 ||
      is.null(data_b)   || nrow(data_b) == 0) {
    return(leaflet() %>% addTiles() %>%
             setView(lng = -93.3, lat = 44.95, zoom = 9) %>%
             addControl(html = "<div style='background:white;padding:10px;'>Select two weeks with data</div>",
                        position = "topright"))
  }
  
  # Pick the metric column
  if (metric_type == "vector_index") {
    col <- "vector_index"
    label <- "Vector Index"
    fmt <- function(x) sprintf("%.4f", x)
  } else if (metric_type == "infection") {
    if (infection_metric == "mir") {
      col <- "mir_raw"; label <- "MIR (per 1000)"
    } else {
      col <- "infection_rate"; label <- "MLE"
    }
    fmt <- function(x) sprintf("%.4f", x)
  } else {
    col <- "avg_per_trap"; label <- "Avg/Trap"
    fmt <- function(x) sprintf("%.1f", x)
  }
  
  # Ensure column exists
  if (!col %in% names(data_a)) data_a[[col]] <- NA_real_
  if (!col %in% names(data_b)) data_b[[col]] <- NA_real_
  
  # Compute delta: week_b - week_a (positive = increase)
  delta <- data.frame(viarea = data_a$viarea, stringsAsFactors = FALSE)
  merged <- merge(delta,
                  data_a[, c("viarea", col)], by = "viarea", all.x = TRUE)
  names(merged)[ncol(merged)] <- "val_a"
  merged <- merge(merged,
                  data_b[, c("viarea", col)], by = "viarea", all.x = TRUE)
  names(merged)[ncol(merged)] <- "val_b"
  merged$delta <- merged$val_b - merged$val_a
  
  map_sf <- areas_sf %>% left_join(merged, by = "viarea")
  
  # Symmetric diverging domain
  max_abs <- max(abs(merged$delta), na.rm = TRUE)
  if (is.infinite(max_abs) || max_abs == 0) max_abs <- 1
  
  div_pal <- colorNumeric(
    palette = c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7",
                "#fddbc7", "#ef8a62", "#b2182b"),
    domain = c(-max_abs, max_abs), na.color = "#C0C0C0"
  )
  
  # Popup
  popup_text <- sapply(seq_len(nrow(map_sf)), function(i) {
    r <- map_sf[i, , drop = FALSE]
    sprintf(
      "<strong>%s</strong><br/><hr/>Wk %s: %s<br/>Wk %s: %s<br/><strong>Change: %s</strong>",
      r$viarea,
      yrwk_a, ifelse(is.na(r$val_a), "N/A", fmt(r$val_a)),
      yrwk_b, ifelse(is.na(r$val_b), "N/A", fmt(r$val_b)),
      ifelse(is.na(r$delta), "N/A",
             paste0(ifelse(r$delta > 0, "+", ""), fmt(r$delta)))
    )
  })
  
  bg_layers <- load_background_layers()
  
  m <- leaflet(map_sf) %>%
    addMapPane("areas", zIndex = 410) %>%
    addMapPane("traps", zIndex = 450) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB Light") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addPolygons(
      fillColor = ~div_pal(delta),
      fillOpacity = 0.7,
      color = "#333", weight = 2,
      popup = popup_text,
      label = ~paste0(viarea, ": ", ifelse(is.na(delta), "N/A",
                      paste0(ifelse(delta > 0, "+", ""), fmt(delta)))),
      highlightOptions = highlightOptions(weight = 4, color = "#000",
                                          fillOpacity = 0.9, bringToFront = FALSE),
      options = pathOptions(pane = "areas"),
      group = "Vector Index Areas"
    )
  
  # Background layers
  if (!is.null(bg_layers$facilities)) {
    m <- m %>% addPolylines(data = bg_layers$facilities, color = "#2c3e50",
                            weight = 1.5, opacity = 0.6, group = "Facilities")
  }
  if (!is.null(bg_layers$counties)) {
    m <- m %>% addPolylines(data = bg_layers$counties, color = "#d62728",
                            weight = 2.5, opacity = 0.7, group = "Counties")
  }
  
  # Diverging legend
  m <- m %>%
    addLegend(position = "bottomright", pal = div_pal,
              values = c(-max_abs, max_abs),
              title = paste("\u0394", label), opacity = 0.7)
  
  # Title
  m <- m %>%
    addControl(
      html = sprintf(
        "<div style='background:white;padding:8px 12px;border-radius:5px;font-size:13px;border:1px solid #ccc;'>
           <strong>Change in %s</strong> | %s | Wk %s \u2192 Wk %s
         </div>",
        label, spp_label, yrwk_a, yrwk_b),
      position = "topleft")
  
  # Trap markers
  overlay_groups <- c("Vector Index Areas", "Facilities", "Counties")
  if (!is.null(all_traps) && nrow(all_traps) > 0) {
    m <- m %>%
      addCircleMarkers(data = all_traps, radius = 4, color = "#333",
                       fillColor = "#FFD700", fillOpacity = 0.8, weight = 1,
                       popup = ~paste0("<strong>Trap</strong><br/>Facility: ", facility,
                                       "<br/>Type: ", trap_type_label),
                       options = pathOptions(pane = "traps"),
                       group = "Trap Locations")
    overlay_groups <- c(overlay_groups, "Trap Locations")
  }
  
  m %>%
    addLayersControl(
      baseGroups = c("CartoDB Light", "OpenStreetMap", "Satellite"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = TRUE))
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
        # Compute 95% CI for MIR using binomial SE
        if ("total_mosquitoes" %in% names(row) && !is.na(row$total_mosquitoes) &&
            as.numeric(row$total_mosquitoes) > 0) {
          n_mosq <- as.numeric(row$total_mosquitoes)
          p_hat <- as.numeric(row$positive) / n_mosq
          mir_se <- sqrt(p_hat * (1 - p_hat) / n_mosq) * 1000
          mir_lower <- max(mir_val - 1.96 * mir_se, 0)
          mir_upper <- mir_val + 1.96 * mir_se
          html <- paste0(html, sprintf("95%% CI: (%.2f - %.2f)<br/>", mir_lower, mir_upper))
        }
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
