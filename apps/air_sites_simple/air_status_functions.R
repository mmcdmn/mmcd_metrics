# Simple Air Status Functions
# Only considers treatment status - no rainfall data

# Load required libraries for this file
suppressPackageStartupMessages({
  library(leaflet)
})

get_air_sites_data_simple <- function(analysis_date, larvae_threshold = 2, facility_filter = NULL, priority_filter = NULL, zone_filter = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Convert threshold to numeric
    larvae_threshold <- as.numeric(larvae_threshold)
    
    # Build filter conditions
    facility_condition <- ""
    if (!is.null(facility_filter) && length(facility_filter) > 0) {
      facility_list <- paste(sprintf("'%s'", facility_filter), collapse=", ")
      facility_condition <- sprintf("AND b.facility IN (%s)", facility_list)
    }
    
    priority_condition <- ""
    if (!is.null(priority_filter) && length(priority_filter) > 0) {
      priority_list <- paste(sprintf("'%s'", priority_filter), collapse=", ")
      priority_condition <- sprintf("AND b.priority IN (%s)", priority_list)
    }
    
    zone_condition <- ""
    if (!is.null(zone_filter) && length(zone_filter) > 0) {
      zone_list <- paste(sprintf("'%s'", zone_filter), collapse=", ")
      zone_condition <- sprintf("AND g.zone IN (%s)", zone_list)
    }

    # Enhanced query with inspection logic added
    query <- sprintf("
      WITH ActiveAirSites AS (
        SELECT 
          b.facility,
          b.sitecode,
          b.acres,
          b.priority,
          g.zone,
          ST_X(ST_Centroid(ST_Transform(b.geom, 4326))) as longitude,
          ST_Y(ST_Centroid(ST_Transform(b.geom, 4326))) as latitude
        FROM loc_breeding_sites b
        LEFT JOIN public.gis_sectcode g ON LEFT(b.sitecode, 6) || '-' = g.sectcode
          OR LEFT(b.sitecode, 6) || 'N' = g.sectcode
        WHERE (b.enddate IS NULL OR b.enddate > '%s')
          AND b.air_gnd = 'A'
          AND b.geom IS NOT NULL
          %s
          %s
          %s
      ),
      
      -- Get last inspection for each site (actions 2,4 only)
      LastInspections AS (
        SELECT 
          sitecode,
          inspdate as last_inspection_date,
          numdip as last_larvae_count,
          ROW_NUMBER() OVER (PARTITION BY sitecode ORDER BY inspdate DESC) as rn
        FROM dblarv_insptrt_current
        WHERE action IN ('2', '4')  -- Inspection actions only
          AND inspdate <= '%s'::date
          AND sitecode IN (SELECT sitecode FROM ActiveAirSites)
      ),
      
      RecentInspections AS (
        SELECT 
          sitecode,
          last_inspection_date,
          last_larvae_count
        FROM LastInspections
        WHERE rn = 1
      ),
      
      -- Get the most recent treatment for each site from dblarv_insptrt_current
      RecentTreatments AS (
        SELECT 
          t.sitecode,
          MAX(t.inspdate) as last_treatment_date,
          t.matcode,
          t.mattype,
          ROW_NUMBER() OVER (PARTITION BY t.sitecode ORDER BY MAX(t.inspdate) DESC) as rn
        FROM dblarv_insptrt_current t
        WHERE t.inspdate <= '%s'::date
          AND t.action IN ('3', 'A', 'D')  -- Treatment actions only
          AND t.matcode IS NOT NULL
          AND t.matcode != ''
          AND t.sitecode IN (SELECT sitecode FROM ActiveAirSites)
        GROUP BY t.sitecode, t.matcode, t.mattype
      ),
      
      -- Get treatment material details and effect days
      TreatmentInfo AS (
        SELECT 
          rt.sitecode,
          rt.last_treatment_date,
          rt.matcode,
          rt.mattype as last_treatment_material,
          -- Calculate treatment expiry based on material effect days
          CASE 
            WHEN mt.effect_days IS NOT NULL AND mt.effect_days > 0
            THEN rt.last_treatment_date + INTERVAL '1 day' * mt.effect_days
            ELSE NULL
          END as treatment_expiry
        FROM RecentTreatments rt
        LEFT JOIN mattype_list mt ON rt.mattype = mt.mattype
        WHERE rt.rn = 1
      )
      
      SELECT 
        a.facility,
        a.sitecode,
        a.acres,
        a.priority,
        a.zone,
        a.longitude,
        a.latitude,
        i.last_inspection_date,
        COALESCE(i.last_larvae_count, 0) as last_larvae_count,
        t.last_treatment_date,
        t.last_treatment_material,
        t.treatment_expiry,
        t.matcode,
        
        -- Enhanced status logic based on inspections and treatments
        CASE 
          -- Priority 1: Active treatment (treatment hasn't expired yet)
          -- Note: Even re-inspection should not change active treatment status
          WHEN t.treatment_expiry IS NOT NULL AND t.treatment_expiry > '%s'::date THEN 'Active Treatment'
          
          -- Priority 2: Has inspection data
          WHEN i.last_inspection_date IS NOT NULL THEN
            CASE 
              -- If larvae count >= threshold, needs treatment
              WHEN COALESCE(i.last_larvae_count, 0) >= %.0f THEN 
                CASE 
                  -- But if inspection is more than 7 days old, revert to unknown
                  WHEN i.last_inspection_date < '%s'::date - INTERVAL '7 days' THEN 'Unknown'
                  ELSE 'Need Treatment'
                END
              -- If larvae count < threshold, inspected (under threshold)
              ELSE 
                CASE 
                  -- But if inspection is more than 7 days old, revert to unknown
                  WHEN i.last_inspection_date < '%s'::date - INTERVAL '7 days' THEN 'Unknown'
                  ELSE 'Inspected'
                END
            END
          
          -- Priority 3: No inspection or treatment data
          ELSE 'Unknown'
        END as site_status
        
      FROM ActiveAirSites a
      LEFT JOIN RecentInspections i ON a.sitecode = i.sitecode
      LEFT JOIN TreatmentInfo t ON a.sitecode = t.sitecode
      ORDER BY a.facility, a.sitecode
    ",
    analysis_date,          # For enddate filter
    facility_condition,
    priority_condition, 
    zone_condition,
    analysis_date,          # For inspection date filter
    analysis_date,          # For treatment date filter
    analysis_date,          # For treatment expiry check
    larvae_threshold,       # For larvae threshold check
    analysis_date,          # For inspection age check 1
    analysis_date           # For inspection age check 2
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
  
  # Define colors for all status types using shared color scheme
  status_color_map <- get_status_color_map()
  status_colors <- c(
    "Unknown" = as.character(status_color_map[["Unknown"]]),
    "Inspected" = as.character(status_color_map[["Under Threshold"]]),
    "Need Treatment" = as.character(status_color_map[["Needs Treatment"]]),
    "Active Treatment" = as.character(status_color_map[["Active Treatment"]])
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
        "Status: ", site_status, "<br/>",
        "Priority: ", priority, "<br/>",
        "Acres: ", acres, "<br/>",
        "Last Inspection: ", ifelse(is.na(last_inspection_date), "None", last_inspection_date), "<br/>",
        "Larvae Count: ", ifelse(is.na(last_larvae_count) | last_larvae_count == 0, "None", last_larvae_count), "<br/>",
        "Last Treatment: ", ifelse(is.na(last_treatment_date), "None", last_treatment_date), "<br/>",
        "Material Used: ", ifelse(is.na(last_treatment_material), "None", last_treatment_material)
      ),
      layerId = ~sitecode
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c(status_colors["Unknown"], status_colors["Inspected"], 
                status_colors["Need Treatment"], status_colors["Active Treatment"]),
      labels = c("Unknown", "Inspected", "Need Treatment", "Active Treatment"),
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
  # Inspection info
  inspection_info <- ""
  if (!is.na(site_data$last_inspection_date)) {
    inspection_info <- paste0(
      "<strong>Last Inspection:</strong> ", site_data$last_inspection_date, "<br/>",
      "<strong>Larvae Count:</strong> ", ifelse(is.na(site_data$last_larvae_count) | site_data$last_larvae_count == 0, "None", site_data$last_larvae_count), "<br/>"
    )
  } else {
    inspection_info <- "<strong>No recent inspections found</strong><br/>"
  }
  
  # Treatment info  
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
    if (site_data$site_status == "Active Treatment") "green" else
    if (site_data$site_status == "Need Treatment") "red" else
    if (site_data$site_status == "Inspected") "blue" else "gray", ";'>",
    site_data$site_status, "</span><br/><br/>",
    inspection_info, "<br/>",
    treatment_info
  ))
}

