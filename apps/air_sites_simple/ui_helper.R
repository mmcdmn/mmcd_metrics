# Get available zone options for air sites only
get_available_zones <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(character(0))
  
  tryCatch({
    query <- "
      SELECT DISTINCT g.zone 
      FROM loc_breeding_sites b
      LEFT JOIN public.gis_sectcode g ON LEFT(b.sitecode, 6) || '-' = g.sectcode
        OR LEFT(b.sitecode, 6) || 'N' = g.sectcode
      WHERE g.zone IS NOT NULL 
        AND b.air_gnd = 'A'
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
      ORDER BY g.zone
    "
    result <- dbGetQuery(con, query)
    dbDisconnect(con)
    return(result$zone)
  }, error = function(e) {
    if (exists("con") && !is.null(con)) dbDisconnect(con)
    return(character(0))
  })
}

# Create site map
create_site_map <- function(data) {
  if (nrow(data) == 0) {
    return(leaflet() %>% 
      addTiles() %>%
      setView(lng = -93.2, lat = 44.9, zoom = 10))
  }
  
  # Define colors for status using shared color scheme
  status_color_map <- get_status_color_map()
  status_colors <- c(
    "Active Treatment" = status_color_map[["Active Treatment"]],
    "Unknown" = status_color_map[["Unknown"]]
  )
  
  # Create color palette
  data$color <- status_colors[data$site_status]
  
  # Create map
  map <- leaflet(data) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      color = ~color,
      fillColor = ~color,
      fillOpacity = 0.7,
      radius = 6,
      stroke = TRUE,
      weight = 2,
      popup = ~paste0(
        "<strong>", sitecode, "</strong><br/>",
        "Priority: ", priority, "<br/>",
        "Acres: ", acres, "<br/>",
        "Last Treatment: ", ifelse(is.na(last_treatment_date), "None", last_treatment_date), "<br/>",
        "Material Used: ", ifelse(is.na(last_treatment_material), "None", last_treatment_material)
      ),
      layerId = ~sitecode
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c(status_colors[["Active Treatment"]], status_colors[["Unknown"]]),
      labels = c("Active Treatment", "Unknown"),
      title = "Site Status"
    )
  
  # Fit bounds to data
  if (nrow(data) > 0) {
    map <- map %>% fitBounds(
      lng1 = min(data$longitude, na.rm = TRUE),
      lat1 = min(data$latitude, na.rm = TRUE),
      lng2 = max(data$longitude, na.rm = TRUE),
      lat2 = max(data$latitude, na.rm = TRUE)
    )
  }
  
  return(map)
}

# Create site details panel
create_site_details_panel <- function(site_data) {
  treatment_info <- ""
  if (!is.na(site_data$last_treatment_date)) {
    treatment_info <- paste0(
      "<strong>Last Treatment:</strong> ", site_data$last_treatment_date, "<br/>",
      "<strong>Material:</strong> ", ifelse(is.na(site_data$last_treatment_material), "Unknown", site_data$last_treatment_material), "<br/>",
      "<strong>Material Code:</strong> ", ifelse(is.na(site_data$matcode), "Unknown", site_data$matcode), "<br/>",
      "<strong>Expires:</strong> ", ifelse(is.na(site_data$treatment_expiry), "Unknown", site_data$treatment_expiry), "<br/>"
    )
  } else {
    treatment_info <- "<strong>No recent treatments found</strong><br/>"
  }
  
  HTML(paste0(
    "<h4>", site_data$sitecode, "</h4>",
    "<strong>Facility:</strong> ", site_data$facility, "<br/>",
    "<strong>Priority:</strong> ", site_data$priority, "<br/>",
    "<strong>Zone:</strong> ", ifelse(is.na(site_data$zone), "Unknown", site_data$zone), "<br/>",
    "<strong>Acres:</strong> ", site_data$acres, "<br/>",
    "<strong>Status:</strong> <span style='color: ", 
    ifelse(site_data$site_status == "Active Treatment", "green", "gray"), ";'>",
    site_data$site_status, "</span><br/><br/>",
    treatment_info
  ))
}