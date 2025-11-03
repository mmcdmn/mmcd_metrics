# Air Site Status Functions for Red Air Pipeline App
# Functions to handle the main air site status dashboard functionality

# Function to get air sites data with status calculation
get_air_sites_data <- function(analysis_date, lookback_period, rain_threshold, treatment_threshold, facility_filter, priority_filter, zone_filter) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    lookback_days <- as.numeric(lookback_period)
    rain_threshold <- as.numeric(rain_threshold)
    treatment_threshold <- as.numeric(treatment_threshold)
    
    # Build facility filter
    facility_condition <- if (facility_filter == "all") {
      ""
    } else {
      sprintf("AND b.facility = '%s'", facility_filter)
    }
    
    # Build priority filter  
    priority_condition <- if (priority_filter == "all") {
      ""
    } else {
      sprintf("AND b.priority = '%s'", priority_filter)
    }
    
    # Build zone filter
    zone_condition <- if (!is.null(zone_filter) && length(zone_filter) > 0) {
      zone_list <- paste0("'", zone_filter, "'", collapse = ",")
      sprintf("AND g.zone IN (%s)", zone_list)
    } else {
      ""
    }
    
    # Get all active air sites with rainfall data and status calculation
    query <- sprintf("
      WITH ActiveAirSites AS (
        SELECT 
          b.facility,
          b.sitecode,
          b.acres,
          b.priority,
          b.prehatch,
          g.zone,
          ST_X(ST_Centroid(ST_Transform(b.geom, 4326))) as longitude,
          ST_Y(ST_Centroid(ST_Transform(b.geom, 4326))) as latitude
        FROM loc_breeding_sites b
        LEFT JOIN public.gis_sectcode g ON LEFT(b.sitecode, 6) || '-' = g.sectcode
          OR LEFT(b.sitecode, 6) || 'N' = g.sectcode
          OR LEFT(b.sitecode, 6) || 'S' = g.sectcode
          OR LEFT(b.sitecode, 6) || 'E' = g.sectcode
          OR LEFT(b.sitecode, 6) || 'W' = g.sectcode
        WHERE (b.enddate IS NULL OR b.enddate > '%s')
          AND b.air_gnd = 'A'
          AND b.geom IS NOT NULL
          %s
          %s
          %s
      ),
      
      RainfallData AS (
        SELECT 
          r.sitecode,
          SUM(r.rainfall) as total_rainfall,
          MAX(r.rdate) as last_rain_date,
          COUNT(*) as rain_days
        FROM public.rainfall r
        WHERE r.rdate >= '%s'::date - INTERVAL '%d days'
          AND r.rdate <= '%s'::date
        GROUP BY r.sitecode
      ),
      
      LastInspections AS (
        SELECT 
          a.sitecode,
          a.inspdate as last_inspection_date,
          a.mosqcount as last_larvae_count,
          ROW_NUMBER() OVER (PARTITION BY a.sitecode ORDER BY a.inspdate DESC) as rn
        FROM public.dblarv_insptrt_archive a
        WHERE a.action IN ('9', '8')
        UNION ALL
        SELECT 
          c.sitecode,
          c.inspdate as last_inspection_date,
          c.mosqcount as last_larvae_count,
          ROW_NUMBER() OVER (PARTITION BY c.sitecode ORDER BY c.inspdate DESC) as rn
        FROM public.dblarv_insptrt_current c
        WHERE c.action IN ('9', '8')
      ),
      
      RecentInspections AS (
        SELECT 
          sitecode,
          last_inspection_date,
          last_larvae_count
        FROM LastInspections
        WHERE rn = 1
      ),
      
      LastTreatments AS (
        SELECT 
          a.sitecode,
          a.inspdate as last_treatment_date,
          ROW_NUMBER() OVER (PARTITION BY a.sitecode ORDER BY a.inspdate DESC) as rn
        FROM public.dblarv_insptrt_archive a
        WHERE a.action IN ('3', '4', '5', '6', '7')
        UNION ALL
        SELECT 
          c.sitecode,
          c.inspdate as last_treatment_date,
          ROW_NUMBER() OVER (PARTITION BY c.sitecode ORDER BY c.inspdate DESC) as rn
        FROM public.dblarv_insptrt_current c
        WHERE c.action IN ('3', '4', '5', '6', '7')
      ),
      
      RecentTreatments AS (
        SELECT 
          sitecode,
          last_treatment_date
        FROM LastTreatments
        WHERE rn = 1
      )
      
      SELECT 
        a.facility,
        a.sitecode,
        a.acres,
        a.priority,
        a.prehatch,
        a.zone,
        a.longitude,
        a.latitude,
        COALESCE(r.total_rainfall, 0) as total_rainfall,
        r.last_rain_date,
        r.rain_days,
        i.last_inspection_date,
        i.last_larvae_count,
        t.last_treatment_date,
        CASE 
          WHEN COALESCE(r.total_rainfall, 0) < %f THEN 'Under Threshold'
          WHEN i.last_inspection_date IS NULL OR i.last_inspection_date < r.last_rain_date THEN 'Needs Inspection'
          WHEN COALESCE(i.last_larvae_count, 0) < %d THEN 'Under Threshold' 
          ELSE 'Needs Treatment'
        END as site_status
      FROM ActiveAirSites a
      LEFT JOIN RainfallData r ON a.sitecode = r.sitecode
      LEFT JOIN RecentInspections i ON a.sitecode = i.sitecode
      LEFT JOIN RecentTreatments t ON a.sitecode = t.sitecode
      ORDER BY a.facility, a.sitecode
    ", 
    analysis_date,
    facility_condition,
    priority_condition, 
    zone_condition,
    analysis_date,
    lookback_days,
    analysis_date,
    rain_threshold,
    treatment_threshold
    )
    
    result <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    return(result)
    
  }, error = function(e) {
    if (exists("con") && !is.null(con)) {
      dbDisconnect(con)
    }
    warning(paste("Error getting air sites data:", e$message))
    return(data.frame())
  })
}

# Function to create status map
create_status_map <- function(data, status_filter) {
  if (nrow(data) == 0) {
    return(leaflet() %>% addTiles() %>% setView(lng = -93.2, lat = 44.95, zoom = 10))
  }
  
  # Filter data by status if specified
  if (status_filter != "all") {
    data <- data[data$site_status == status_filter, ]
  }
  
  # Get colors for each status
  source_colors <- get_status_colors()
  data$color <- sapply(data$site_status, function(status) {
    switch(status,
           "Unknown" = source_colors["unknown"],
           "Needs Inspection" = source_colors["needs_attention"], 
           "Under Threshold" = source_colors["active"],
           "Needs Treatment" = source_colors["critical"],
           source_colors["unknown"])
  })
  
  # Create popup text
  data$popup_text <- sprintf(
    "<b>%s</b><br/>Status: %s<br/>Facility: %s<br/>Priority: %s<br/>Zone: %s<br/>Rainfall: %.2f inches<br/>Last Inspection: %s<br/>Last Count: %s",
    data$sitecode,
    data$site_status,
    data$facility,
    data$priority,
    data$zone,
    data$total_rainfall,
    ifelse(is.na(data$last_inspection_date), "Never", 
           tryCatch(format(as.Date(data$last_inspection_date), "%Y-%m-%d"),
                   error = function(e) data$last_inspection_date)),
    ifelse(is.na(data$last_larvae_count), "N/A", data$last_larvae_count)
  )
  
  leaflet(data) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      color = ~color,
      fillColor = ~color,
      fillOpacity = 0.7,
      radius = 6,
      popup = ~popup_text,
      stroke = TRUE,
      weight = 2
    ) %>%
    setView(lng = -93.2, lat = 44.95, zoom = 10)
}

# Function to create status chart
create_status_chart <- function(data, status_filter) {
  if (nrow(data) == 0) {
    return(plot_ly() %>% layout(title = "No data available"))
  }
  
  # Filter data by status if specified
  if (status_filter != "all") {
    data <- data[data$site_status == status_filter, ]
  }
  
  # Count by status
  status_counts <- data %>%
    group_by(site_status) %>%
    summarise(count = n(), .groups = "drop")
  
  # Get colors for chart
  source_colors <- get_status_colors()
  colors_for_chart <- sapply(status_counts$site_status, function(status) {
    switch(status,
           "Unknown" = source_colors["unknown"],
           "Needs Inspection" = source_colors["needs_attention"],
           "Under Threshold" = source_colors["active"], 
           "Needs Treatment" = source_colors["critical"],
           source_colors["unknown"])
  })
  
  plot_ly(status_counts, x = ~site_status, y = ~count, type = "bar",
          marker = list(color = colors_for_chart)) %>%
    layout(title = "Air Sites by Status",
           xaxis = list(title = "Status"),
           yaxis = list(title = "Number of Sites"))
}

# Function to create site details table
create_site_details_table <- function(data, status_filter) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Filter data by status if specified  
  if (status_filter != "all") {
    data <- data[data$site_status == status_filter, ]
  }
  
  # Format the data for display
  display_data <- data %>%
    mutate(
      last_inspection_date = ifelse(is.na(last_inspection_date), "Never",
                                   tryCatch(format(as.Date(last_inspection_date), "%Y-%m-%d"),
                                           error = function(e) last_inspection_date)),
      last_treatment_date = ifelse(is.na(last_treatment_date), "Never", 
                                  tryCatch(format(as.Date(last_treatment_date), "%Y-%m-%d"),
                                          error = function(e) last_treatment_date)),
      last_larvae_count = ifelse(is.na(last_larvae_count), "N/A", last_larvae_count),
      total_rainfall = round(total_rainfall, 2)
    ) %>%
    select(sitecode, facility, site_status, priority, zone, total_rainfall, 
           last_inspection_date, last_larvae_count, last_treatment_date, acres)
  
  return(display_data)
}