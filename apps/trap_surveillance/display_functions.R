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

build_trap_status_legend_html <- function() {
  paste0(
    "<div style='background:white;padding:8px 10px;border-radius:5px;border:1px solid #ccc;font-size:12px;line-height:1.6;'>",
    "<strong>Pool Status</strong><br/>",
    "<span style='display:inline-block;width:10px;height:10px;border-radius:50%;background:#e74c3c;border:1px solid #333;margin-right:6px;'></span>Positive pool(s)<br/>",
    "<span style='display:inline-block;width:10px;height:10px;border-radius:50%;background:#3498db;border:1px solid #333;margin-right:6px;'></span>Tested, all negative<br/>",
    "<span style='display:inline-block;width:10px;height:10px;border-radius:50%;background:#FFD700;border:1px solid #333;margin-right:6px;'></span>No pools tested<br/>",
    "<hr style='margin:4px 0;'/>",
    "<strong>Trap Type</strong><br/>",
    "<span style='display:inline-block;width:10px;height:10px;border-radius:50%;background:#888;border:1px solid #333;margin-right:6px;'></span>CO2 Overnight<br/>",
    "<span style='display:inline-block;width:10px;height:10px;border-radius:50%;background:#888;border:2px solid #8e44ad;margin-right:6px;'></span>Elevated CO2<br/>",
    "<span style='display:inline-block;width:10px;height:10px;border-radius:50%;background:#888;border:2px solid #333;margin-right:6px;'></span>Gravid",
    "</div>"
  )
}

# =============================================================================
# MAIN MAP RENDER
# =============================================================================
render_surveillance_map <- function(combined_data, areas_sf, 
                                     metric_type = "abundance",
                                     infection_metric = "mle",
                                     spp_label = "Total Culex Vectors",
                                     yrwk_label = "",
                                     color_theme = "MMCD",
                                     all_traps = NULL,
                                     week_traps = NULL) {
  
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
  # TRAP MARKERS — prefer week-active traps with pool details over shapefile
  # =========================================================================
  
  overlay_groups <- c("Vector Index Areas", "Facilities", "Counties", "Zones")
  
  if (!is.null(week_traps) && nrow(week_traps) > 0) {
    # Ensure trap_type column exists (backwards compatible)
    if (!"trap_type" %in% names(week_traps)) {
      week_traps$trap_type <- "CO2"
    }
    
    # Week-active traps with pool details
    trap_popup <- sprintf(
      "<strong>Trap: %s</strong> <span style='color:#888;'>(%s)</span><br/>Facility: %s | Zone: %s<br/><hr/><strong>Cx Vector Count: %d</strong><br/>Inspection: %s<br/><hr/><strong>Pools (%d total, %d positive):</strong><br/>%s",
      week_traps$loc_code,
      week_traps$trap_type,
      week_traps$facility,
      ifelse(is.na(week_traps$zone), "N/A", week_traps$zone),
      as.integer(week_traps$cx_vector_count),
      as.character(week_traps$inspdate),
      week_traps$num_pools,
      week_traps$num_positive,
      week_traps$pool_details_html
    )
    
    # Color traps by pool status
    trap_fill <- ifelse(week_traps$num_positive > 0, "#e74c3c",  # Red = positive pools
                 ifelse(week_traps$num_pools > 0, "#3498db",      # Blue = tested, all negative
                        "#FFD700"))                                # Gold = no pools tested
    
    # Split by trap type for different marker styles
    co2_idx <- which(week_traps$trap_type == "CO2")
    eco2_idx <- which(week_traps$trap_type == "Elevated CO2")
    grav_idx <- which(week_traps$trap_type == "Gravid")
    
    if (length(co2_idx) > 0) {
      m <- m %>%
        addCircleMarkers(
          data = week_traps[co2_idx, ],
          radius = 5,
          color = "#333",
          fillColor = trap_fill[co2_idx],
          fillOpacity = 0.85,
          weight = 1,
          popup = trap_popup[co2_idx],
          options = pathOptions(pane = "traps"),
          group = "CO2 Traps"
        )
    }
    if (length(eco2_idx) > 0) {
      m <- m %>%
        addCircleMarkers(
          data = week_traps[eco2_idx, ],
          radius = 7,
          color = "#8e44ad",
          fillColor = trap_fill[eco2_idx],
          fillOpacity = 0.85,
          weight = 2,
          popup = trap_popup[eco2_idx],
          options = pathOptions(pane = "traps"),
          group = "Elevated CO2 Traps"
        )
    }
    if (length(grav_idx) > 0) {
      m <- m %>%
        addCircleMarkers(
          data = week_traps[grav_idx, ],
          radius = 6,
          color = "#333",
          fillColor = trap_fill[grav_idx],
          fillOpacity = 0.85,
          weight = 2,
          popup = trap_popup[grav_idx],
          options = pathOptions(pane = "traps"),
          group = "Gravid Traps"
        )
    }
    overlay_groups <- c(overlay_groups, "CO2 Traps", "Elevated CO2 Traps", "Gravid Traps")
    m <- m %>% addControl(html = build_trap_status_legend_html(), position = "topright")
  } else if (!is.null(all_traps) && nrow(all_traps) > 0) {
    # Fallback: shapefile traps (no pool details)
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
    m <- m %>% addControl(
      html = "<div style='background:white;padding:8px 10px;border-radius:5px;border:1px solid #ccc;font-size:12px;line-height:1.4;'><strong>Trap Status</strong><br/><span style='display:inline-block;width:10px;height:10px;border-radius:50%;background:#FFD700;border:1px solid #333;margin-right:6px;'></span>Trap location</div>",
      position = "topright"
    )
  }
  
  # Layer control
  m <- m %>%
    addLayersControl(
      baseGroups = c("CartoDB Light", "OpenStreetMap", "Satellite"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    hideGroup("Zones") %>%
    addScaleBar(position = "bottomleft")
  
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
                                  all_traps = NULL,
                                  week_traps = NULL) {
  
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
  if (!is.null(week_traps) && nrow(week_traps) > 0) {
    if (!"trap_type" %in% names(week_traps)) week_traps$trap_type <- "CO2"
    
    trap_popup <- sprintf(
      "<strong>Trap: %s</strong> <span style='color:#888;'>(%s)</span><br/>Facility: %s | Zone: %s<br/><hr/><strong>Cx Count: %d</strong> | Pools: %d (%d pos)<br/>%s",
      week_traps$loc_code,
      week_traps$trap_type,
      week_traps$facility,
      ifelse(is.na(week_traps$zone), "N/A", week_traps$zone),
      as.integer(week_traps$cx_vector_count),
      week_traps$num_pools,
      week_traps$num_positive,
      week_traps$pool_details_html
    )
    trap_fill <- ifelse(week_traps$num_positive > 0, "#e74c3c",
                 ifelse(week_traps$num_pools > 0, "#3498db", "#FFD700"))
    
    co2_idx <- which(week_traps$trap_type == "CO2")
    eco2_idx <- which(week_traps$trap_type == "Elevated CO2")
    grav_idx <- which(week_traps$trap_type == "Gravid")
    
    if (length(co2_idx) > 0) {
      m <- m %>%
        addCircleMarkers(data = week_traps[co2_idx, ], radius = 5, color = "#333",
                         fillColor = trap_fill[co2_idx], fillOpacity = 0.85, weight = 1,
                         popup = trap_popup[co2_idx],
                         options = pathOptions(pane = "traps"),
                         group = "CO2 Traps")
    }
    if (length(eco2_idx) > 0) {
      m <- m %>%
        addCircleMarkers(data = week_traps[eco2_idx, ], radius = 7, color = "#8e44ad",
                         fillColor = trap_fill[eco2_idx], fillOpacity = 0.85, weight = 2,
                         popup = trap_popup[eco2_idx],
                         options = pathOptions(pane = "traps"),
                         group = "Elevated CO2 Traps")
    }
    if (length(grav_idx) > 0) {
      m <- m %>%
        addCircleMarkers(data = week_traps[grav_idx, ], radius = 6, color = "#333",
                         fillColor = trap_fill[grav_idx], fillOpacity = 0.85, weight = 2,
                         popup = trap_popup[grav_idx],
                         options = pathOptions(pane = "traps"),
                         group = "Gravid Traps")
    }
    overlay_groups <- c(overlay_groups, "CO2 Traps", "Elevated CO2 Traps", "Gravid Traps")
    m <- m %>% addControl(html = build_trap_status_legend_html(), position = "topright")
  } else if (!is.null(all_traps) && nrow(all_traps) > 0) {
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
      options = layersControlOptions(collapsed = TRUE)) %>%
    addScaleBar(position = "bottomleft")
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

# =============================================================================
# TRAP PERFORMANCE MAP — Renders scored traps colored by composite score
# Based on Chakravarti et al. (2026) trap placement optimization approach
# =============================================================================

render_trap_performance_map <- function(perf_data, areas_sf = NULL) {
  
  if (is.null(perf_data) || nrow(perf_data) == 0) {
    return(leaflet() %>%
             addTiles() %>%
             setView(lng = -93.3, lat = 44.95, zoom = 9) %>%
             addControl(html = "<div style='background:white;padding:10px;font-size:14px;'>No trap performance data available</div>",
                        position = "topright"))
  }
  
  # Filter to traps with geometry
  perf_data <- perf_data[!is.na(perf_data$lon) & !is.na(perf_data$lat), ]
  if (nrow(perf_data) == 0) return(leaflet() %>% addTiles() %>%
                                     setView(lng = -93.3, lat = 44.95, zoom = 9))
  
  traps_sf <- st_as_sf(perf_data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  # Color scale: red (low score) → yellow (medium) → green (high)
  score_pal <- colorNumeric(
    palette = c("#e74c3c", "#f39c12", "#f1c40f", "#2ecc71", "#27ae60"),
    domain = c(0, 1),
    na.color = "#999"
  )
  
  # Size by yield (bigger markers = more mosquitoes captured)
  radius_scale <- 4 + 8 * (perf_data$yield_score)
  
  # Build popup
  trap_popup <- sprintf(
    paste0(
      "<strong style='font-size:14px;'>Trap: %s</strong><br/>",
      "<span style='color:#666;'>VI Area: %s</span><br/>",
      "<hr style='margin:4px 0;'/>",
      "<strong>Composite Score: <span style='font-size:16px;color:%s;'>%.2f</span></strong>",
      " <span style='padding:2px 6px;border-radius:3px;background:%s;color:white;font-size:11px;'>%s</span><br/>",
      "<hr style='margin:4px 0;'/>",
      "<table style='font-size:12px;width:100%%;'>",
      "<tr><td>Yield Score:</td><td><strong>%.2f</strong></td><td style='color:#888;'>(%.1f avg/wk)</td></tr>",
      "<tr><td>Testing Score:</td><td><strong>%.2f</strong></td><td style='color:#888;'>(%d pools)</td></tr>",
      "<tr><td>Detection Score:</td><td><strong>%.2f</strong></td><td style='color:#888;'>(%d positive, %.1f%%)</td></tr>",
      "<tr><td>Consistency Score:</td><td><strong>%.2f</strong></td><td style='color:#888;'>(%d weeks)</td></tr>",
      "</table>",
      "<hr style='margin:4px 0;'/>",
      "<span style='color:#888;font-size:11px;'>Years active: %d | Total mosq: %s | Tested: %s</span>"
    ),
    perf_data$loc_code,
    ifelse(is.na(perf_data$viarea), "N/A", perf_data$viarea),
    ifelse(perf_data$composite_score >= 0.6, "#27ae60",
           ifelse(perf_data$composite_score >= 0.3, "#f39c12", "#e74c3c")),
    perf_data$composite_score,
    ifelse(perf_data$performance_tier == "High", "#27ae60",
           ifelse(perf_data$performance_tier == "Medium", "#f39c12", "#e74c3c")),
    perf_data$performance_tier,
    perf_data$yield_score, as.numeric(perf_data$avg_per_week),
    perf_data$testing_score, as.integer(perf_data$total_pools),
    perf_data$detection_score, as.integer(perf_data$total_positive), as.numeric(perf_data$positivity_rate_pct),
    perf_data$consistency_score, as.integer(perf_data$weeks_active),
    as.integer(perf_data$years_active),
    format(as.numeric(perf_data$total_mosq), big.mark = ","),
    format(as.numeric(perf_data$total_tested), big.mark = ",")
  )
  
  bg_layers <- load_background_layers()
  
  m <- leaflet(traps_sf) %>%
    addMapPane("areas", zIndex = 410) %>%
    addMapPane("traps", zIndex = 450) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB Light") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite")
  
  # Add VI area polygons as background (if available)
  overlay_groups <- c()
  if (!is.null(areas_sf) && nrow(areas_sf) > 0) {
    m <- m %>%
      addPolygons(
        data = areas_sf,
        fillColor = "#f0f0f0", fillOpacity = 0.15,
        color = "#666", weight = 1.5, opacity = 0.5,
        label = ~viarea,
        options = pathOptions(pane = "areas"),
        group = "VI Areas"
      )
    overlay_groups <- c(overlay_groups, "VI Areas")
  }
  
  # Add facility/county boundaries
  if (!is.null(bg_layers$facilities)) {
    m <- m %>% addPolylines(data = bg_layers$facilities, color = "#2c3e50",
                            weight = 1.5, opacity = 0.6, group = "Facilities")
    overlay_groups <- c(overlay_groups, "Facilities")
  }
  if (!is.null(bg_layers$counties)) {
    m <- m %>% addPolylines(data = bg_layers$counties, color = "#d62728",
                            weight = 2.5, opacity = 0.7, group = "Counties")
    overlay_groups <- c(overlay_groups, "Counties")
  }
  
  # Add scored trap markers
  m <- m %>%
    addCircleMarkers(
      data = traps_sf,
      radius = radius_scale,
      color = "#333",
      fillColor = ~score_pal(composite_score),
      fillOpacity = 0.85,
      weight = 1,
      popup = trap_popup,
      label = ~paste0(loc_code, " - Score: ", sprintf("%.2f", composite_score),
                      " (", performance_tier, ")"),
      options = pathOptions(pane = "traps"),
      group = "Trap Scores"
    )
  overlay_groups <- c(overlay_groups, "Trap Scores")
  
  # Legend
  m <- m %>%
    addLegend(
      position = "bottomright",
      pal = score_pal,
      values = c(0, 1),
      title = "Performance Score",
      opacity = 0.85
    )
  
  # Score tier legend
  tier_legend_html <- paste0(
    "<div style='background:white;padding:8px 10px;border-radius:5px;border:1px solid #ccc;font-size:12px;'>",
    "<strong>Score Components</strong><br/>",
    "<span style='color:#27ae60;'>&#9679;</span> High (&ge; 0.6)<br/>",
    "<span style='color:#f39c12;'>&#9679;</span> Medium (0.3 - 0.6)<br/>",
    "<span style='color:#e74c3c;'>&#9679;</span> Low (&lt; 0.3)<br/>",
    "<hr style='margin:4px 0;'/>",
    "<strong>Marker Size</strong> = Yield<br/>",
    "<span style='font-size:10px;color:#888;'>Larger = more mosquitoes captured</span>",
    "</div>"
  )
  m <- m %>% addControl(html = tier_legend_html, position = "topright")
  
  # Title
  m <- m %>%
    addControl(
      html = "<div style='background:white;padding:8px 12px;border-radius:5px;font-size:13px;border:1px solid #ccc;'>
                <strong>Trap Performance Scores</strong> | Based on Chakravarti et al. (2026)
              </div>",
      position = "topleft"
    )
  
  m <- m %>%
    addLayersControl(
      baseGroups = c("CartoDB Light", "OpenStreetMap", "Satellite"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addScaleBar(position = "bottomleft")
  
  m
}

# =============================================================================
# PHASE 1 — Spatial Risk Map
# =============================================================================
# Renders a leaflet map showing spatial risk index per trap.
# Uses blue-to-red color scale representing low-to-high WNV risk.
# =============================================================================

render_spatial_risk_map <- function(risk_data, areas_sf = NULL) {
  # Deprecated — redirects to combined map
  render_trap_analysis_map(risk_data = risk_data, areas_sf = areas_sf)
}

# =============================================================================
# COMBINED CAUSAL ANALYSIS MAP
# =============================================================================
# Renders a leaflet map with:
#   1. Risk surface heatmap (interpolated grid) — shows area-wide WNV risk
#   2. Trap markers colored by composite score — shows individual performance
#   3. VI area polygons colored by coverage grade — shows coverage gaps
#   4. Coverage gap shading — areas far from any trap
# =============================================================================

render_trap_analysis_map <- function(risk_data, risk_surface = NULL,
                                      areas_sf = NULL, area_coverage = NULL) {
  
  if (is.null(risk_data) || nrow(risk_data) == 0) {
    return(leaflet() %>%
             addTiles() %>%
             setView(lng = -93.3, lat = 44.95, zoom = 9) %>%
             addControl(html = "<div style='background:white;padding:10px;font-size:14px;'>Click 'Run Analysis' to load data.</div>",
                        position = "topright"))
  }
  
  # Filter to traps with geometry
  geo_data <- risk_data[!is.na(risk_data$lon) & !is.na(risk_data$lat), ]
  if (nrow(geo_data) == 0) return(leaflet() %>% addTiles() %>%
                                    setView(lng = -93.3, lat = 44.95, zoom = 9))
  
  traps_sf <- st_as_sf(geo_data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  # Color palettes
  score_pal <- colorNumeric(
    palette = c("#e74c3c", "#f39c12", "#f1c40f", "#2ecc71", "#27ae60"),
    domain = c(0, 1), na.color = "#999"
  )
  
  # Marker size by yield
  radius_scale <- 4 + 8 * geo_data$yield_score
  
  # Build trap popup
  trap_popup <- sprintf(
    paste0(
      "<strong style='font-size:14px;'>%s</strong> ",
      "<span style='padding:2px 6px;border-radius:3px;background:%s;color:white;font-size:11px;'>%s</span><br/>",
      "<span style='color:#666;'>%s</span>",
      "<hr style='margin:4px 0;'/>",
      "<table style='font-size:12px;width:100%%;'>",
      "<tr><td>Score:</td><td><strong>%.3f</strong></td><td>Risk Index:</td><td><strong>%.3f</strong></td></tr>",
      "<tr><td>Yield:</td><td>%.2f</td><td>Spatial Risk:</td><td>%.3f</td></tr>",
      "<tr><td>Testing:</td><td>%.2f</td><td>Detection:</td><td>%.2f</td></tr>",
      "</table>",
      "<hr style='margin:4px 0;'/>",
      "<span style='font-size:11px;color:#666;'>%.1f avg/wk | %d pools | %d pos | %d wks active</span>"
    ),
    geo_data$loc_code,
    ifelse(geo_data$performance_tier == "High", "#27ae60",
           ifelse(geo_data$performance_tier == "Medium", "#f39c12", "#e74c3c")),
    geo_data$performance_tier,
    ifelse(is.na(geo_data$viarea), "", geo_data$viarea),
    geo_data$composite_score,
    geo_data$risk_index,
    geo_data$yield_score, geo_data$spatial_risk,
    geo_data$testing_score, geo_data$detection_score,
    as.numeric(geo_data$avg_per_week),
    as.integer(geo_data$total_pools),
    as.integer(geo_data$total_positive),
    as.integer(geo_data$weeks_active)
  )
  
  bg_layers <- load_background_layers()
  
  m <- leaflet(traps_sf) %>%
    addMapPane("heatmap", zIndex = 400) %>%
    addMapPane("areas", zIndex = 410) %>%
    addMapPane("traps", zIndex = 450) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB Light") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite")
  
  overlay_groups <- c()
  
  # --- Layer 1: Risk Surface Heatmap ---
  if (!is.null(risk_surface) && nrow(risk_surface) > 0) {
    # Filter out zero-risk cells and gap cells for cleaner rendering
    surf <- risk_surface[risk_surface$risk_value > 0.01 & !risk_surface$is_gap, ]
    if (nrow(surf) > 0) {
      heat_pal <- colorNumeric(
        palette = c("#3498db22", "#2ecc7144", "#f1c40f88", "#e67e22aa", "#e74c3ccc"),
        domain = c(0, max(surf$risk_value, na.rm = TRUE)),
        na.color = "transparent"
      )
      m <- m %>%
        addCircleMarkers(
          data = surf,
          lng = ~lon, lat = ~lat,
          radius = 4,
          stroke = FALSE,
          fillColor = ~heat_pal(risk_value),
          fillOpacity = 0.6,
          options = pathOptions(pane = "heatmap"),
          group = "Risk Surface"
        )
      overlay_groups <- c(overlay_groups, "Risk Surface")
    }
    
    # Show coverage gaps
    gaps <- risk_surface[risk_surface$is_gap & risk_surface$nearest_trap_km > 3, ]
    if (nrow(gaps) > 0) {
      m <- m %>%
        addCircleMarkers(
          data = gaps,
          lng = ~lon, lat = ~lat,
          radius = 3,
          stroke = FALSE,
          fillColor = "#95a5a6",
          fillOpacity = 0.3,
          options = pathOptions(pane = "heatmap"),
          group = "Coverage Gaps"
        )
      overlay_groups <- c(overlay_groups, "Coverage Gaps")
    }
  }
  
  # --- Layer 2: VI Area polygons with coverage grade ---
  if (!is.null(areas_sf) && nrow(areas_sf) > 0) {
    if (!is.null(area_coverage) && nrow(area_coverage) > 0) {
      areas_merged <- merge(areas_sf, area_coverage, by = "viarea", all.x = TRUE)
      
      grade_colors <- c("Good" = "#27ae60", "Adequate" = "#f39c12",
                         "Thin" = "#e67e22", "Gap" = "#e74c3c")
      areas_merged$fill_color <- ifelse(
        is.na(areas_merged$coverage_grade), "#cccccc",
        grade_colors[areas_merged$coverage_grade]
      )
      
      area_popup <- sprintf(
        paste0(
          "<strong>%s</strong><br/>",
          "<span style='padding:2px 6px;border-radius:3px;background:%s;color:white;'>%s coverage</span><br/>",
          "<hr style='margin:4px 0;'/>",
          "<table style='font-size:12px;'>",
          "<tr><td>Traps:</td><td><strong>%s</strong></td></tr>",
          "<tr><td>Avg Score:</td><td>%s</td></tr>",
          "<tr><td>Avg Risk:</td><td>%s</td></tr>",
          "<tr><td>Pools:</td><td>%s</td></tr>",
          "<tr><td>Positives:</td><td>%s (%s%%)</td></tr>",
          "<tr><td>Low Performers:</td><td>%s%%</td></tr>",
          "</table>"
        ),
        areas_merged$viarea,
        ifelse(is.na(areas_merged$coverage_grade), "#999",
               grade_colors[areas_merged$coverage_grade]),
        ifelse(is.na(areas_merged$coverage_grade), "No data", areas_merged$coverage_grade),
        ifelse(is.na(areas_merged$n_traps), "0", areas_merged$n_traps),
        ifelse(is.na(areas_merged$avg_score), "N/A", areas_merged$avg_score),
        ifelse(is.na(areas_merged$avg_risk), "N/A", areas_merged$avg_risk),
        ifelse(is.na(areas_merged$total_pools), "0", format(areas_merged$total_pools, big.mark = ",")),
        ifelse(is.na(areas_merged$total_positive), "0", areas_merged$total_positive),
        ifelse(is.na(areas_merged$positivity_pct), "0", areas_merged$positivity_pct),
        ifelse(is.na(areas_merged$pct_low), "0", areas_merged$pct_low)
      )
      
      m <- m %>%
        addPolygons(
          data = areas_merged,
          fillColor = ~fill_color, fillOpacity = 0.2,
          color = "#333", weight = 2, opacity = 0.7,
          popup = area_popup,
          label = ~paste0(viarea, " — ", ifelse(is.na(coverage_grade), "?", coverage_grade),
                          " (", ifelse(is.na(n_traps), 0, n_traps), " traps)"),
          options = pathOptions(pane = "areas"),
          group = "VI Areas (Coverage)"
        )
      overlay_groups <- c(overlay_groups, "VI Areas (Coverage)")
    } else {
      m <- m %>%
        addPolygons(
          data = areas_sf,
          fillColor = "#f0f0f0", fillOpacity = 0.15,
          color = "#666", weight = 1.5, opacity = 0.5,
          label = ~viarea,
          options = pathOptions(pane = "areas"),
          group = "VI Areas"
        )
      overlay_groups <- c(overlay_groups, "VI Areas")
    }
  }
  
  # --- Background layers ---
  if (!is.null(bg_layers$facilities)) {
    m <- m %>% addPolylines(data = bg_layers$facilities, color = "#2c3e50",
                            weight = 1.5, opacity = 0.6, group = "Facilities")
    overlay_groups <- c(overlay_groups, "Facilities")
  }
  if (!is.null(bg_layers$counties)) {
    m <- m %>% addPolylines(data = bg_layers$counties, color = "#d62728",
                            weight = 2.5, opacity = 0.7, group = "Counties")
    overlay_groups <- c(overlay_groups, "Counties")
  }
  
  # --- Layer 3: Trap markers ---
  m <- m %>%
    addCircleMarkers(
      data = traps_sf,
      radius = radius_scale,
      color = "#333", weight = 1,
      fillColor = ~score_pal(composite_score),
      fillOpacity = 0.9,
      popup = trap_popup,
      label = ~paste0(loc_code, " — Score: ", sprintf("%.2f", composite_score),
                      " | Risk: ", sprintf("%.2f", risk_index)),
      options = pathOptions(pane = "traps"),
      group = "Trap Scores"
    )
  overlay_groups <- c(overlay_groups, "Trap Scores")
  
  # --- Legends ---
  m <- m %>%
    addLegend(position = "bottomright", pal = score_pal,
              values = c(0, 1), title = "Trap Score (markers)", opacity = 0.85)
  
  legend_html <- paste0(
    "<div style='background:white;padding:8px 10px;border-radius:5px;border:1px solid #ccc;font-size:12px;'>",
    "<strong>Map Layers</strong><br/>",
    "<hr style='margin:4px 0;'/>",
    "<strong>&#9679; Markers</strong> = Individual Traps<br/>",
    "<span style='margin-left:12px;color:#27ae60;'>&#9679;</span> High score (&ge; 0.6)<br/>",
    "<span style='margin-left:12px;color:#f39c12;'>&#9679;</span> Medium (0.3&ndash;0.6)<br/>",
    "<span style='margin-left:12px;color:#e74c3c;'>&#9679;</span> Low (&lt; 0.3)<br/>",
    "<span style='margin-left:12px;font-size:10px;color:#888;'>Size = mosquito yield</span><br/>",
    "<hr style='margin:4px 0;'/>",
    "<strong style='color:#e67e22;'>&#11044;</strong> <strong>Heatmap</strong> = Interpolated WNV Risk<br/>",
    "<span style='margin-left:12px;font-size:10px;color:#888;'>Blue&rarr;Green&rarr;Yellow&rarr;Red = Low&rarr;High risk</span><br/>",
    "<span style='margin-left:12px;font-size:10px;color:#888;'>Estimated between traps via kernel smoothing</span><br/>",
    "<hr style='margin:4px 0;'/>",
    "<strong>&#9632; Area Outlines</strong> = VI Area Coverage<br/>",
    "<span style='margin-left:12px;color:#27ae60;'>&#9632;</span> Good<br/>",
    "<span style='margin-left:12px;color:#f39c12;'>&#9632;</span> Adequate<br/>",
    "<span style='margin-left:12px;color:#e67e22;'>&#9632;</span> Thin<br/>",
    "<span style='margin-left:12px;color:#e74c3c;'>&#9632;</span> Gap<br/>",
    "<hr style='margin:4px 0;'/>",
    "<span style='color:#95a5a6;'>&#9679;</span> <strong>Grey dots</strong> = No nearby traps (coverage gap)<br/>",
    "</div>"
  )
  m <- m %>% addControl(html = legend_html, position = "topright")
  
  m <- m %>%
    addControl(
      html = "<div style='background:white;padding:8px 12px;border-radius:5px;font-size:13px;border:1px solid #ccc;'>
                <strong>Causal Analysis</strong> | Risk Surface + Trap Scores + Coverage
              </div>",
      position = "topleft"
    )
  
  m <- m %>%
    addLayersControl(
      baseGroups = c("CartoDB Light", "OpenStreetMap", "Satellite"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addScaleBar(position = "bottomleft")
  
  m
}
