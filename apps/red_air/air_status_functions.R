# Air Site Status Functions for Red Air Pipeline App

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
    zone_condition <- if (is.null(zone_filter) || length(zone_filter) == 0) {
      ""
    } else {
      zone_list <- paste(sprintf("'%s'", zone_filter), collapse=", ")
      sprintf("AND g.zone IN (%s)", zone_list)
    }
    
    # Simple but effective query for air site status calculation
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
        WHERE (b.enddate IS NULL OR b.enddate > '%s')
          AND b.air_gnd = 'A'
          AND b.geom IS NOT NULL
          %s
          %s
          %s
      ),
      
      RainfallData AS (
        SELECT 
          a.sitecode,
          COALESCE(SUM(r.rain_inches), 0) as total_rainfall,
          -- Check if any consecutive N-day period had enough rainfall
          CASE 
            WHEN EXISTS (
              SELECT 1 
              FROM generate_series(0, 14 - %d) AS start_offset
              WHERE (
                SELECT COALESCE(SUM(rain_inches), 0)
                FROM nws_precip_site_history 
                WHERE sitecode = a.sitecode 
                  AND date >= '%s'::date - INTERVAL '14 days' + start_offset * INTERVAL '1 day'
                  AND date < '%s'::date - INTERVAL '14 days' + start_offset * INTERVAL '1 day' + INTERVAL '%d days'
              ) >= %f
            ) THEN 
              json_build_object('triggered', true, 'total_rain', COALESCE(SUM(r.rain_inches), 0))::text
            ELSE 
              json_build_object('triggered', false)::text
          END as triggering_period,
          CASE 
            WHEN EXISTS (
              SELECT 1 
              FROM generate_series(0, 14 - %d) AS start_offset
              WHERE (
                SELECT COALESCE(SUM(rain_inches), 0)
                FROM nws_precip_site_history 
                WHERE sitecode = a.sitecode 
                  AND date >= '%s'::date - INTERVAL '14 days' + start_offset * INTERVAL '1 day'
                  AND date < '%s'::date - INTERVAL '14 days' + start_offset * INTERVAL '1 day' + INTERVAL '%d days'
              ) >= %f
            ) THEN true
            ELSE false
          END as has_triggering_rainfall
        FROM ActiveAirSites a
        LEFT JOIN nws_precip_site_history r ON a.sitecode = r.sitecode
          AND r.date >= '%s'::date - INTERVAL '14 days'
          AND r.date <= '%s'::date
        GROUP BY a.sitecode
      ),
      
      LastInspections AS (
        SELECT 
          sitecode,
          inspdate as last_inspection_date,
          numdip as last_larvae_count,
          ROW_NUMBER() OVER (PARTITION BY sitecode ORDER BY inspdate DESC) as rn
        FROM (
          SELECT sitecode, inspdate, numdip FROM dblarv_insptrt_current WHERE action IN ('1', '2', '4') AND inspdate <= '%s'
          UNION ALL
          SELECT sitecode, inspdate, numdip FROM dblarv_insptrt_archive WHERE action IN ('1', '2', '4') AND inspdate <= '%s'
        ) inspections
      ),
      
      RecentInspections AS (
        SELECT 
          sitecode,
          last_inspection_date,
          last_larvae_count
        FROM LastInspections
        WHERE rn = 1
      ),
      
      TreatmentEffects AS (
        SELECT 
          t.sitecode,
          MAX(t.inspdate) as last_treatment_date,
          FIRST_VALUE(t.mattype) OVER (PARTITION BY t.sitecode ORDER BY t.inspdate DESC) as last_treatment_material,
          CASE 
            WHEN MAX(m.effect_days) IS NOT NULL 
            THEN MAX(t.inspdate) + INTERVAL '1 day' * MAX(m.effect_days)
            ELSE NULL
          END as treatment_expiry
        FROM (
          SELECT sitecode, inspdate, mattype
          FROM public.dblarv_insptrt_archive
          WHERE action IN ('3', 'A', 'D')
            AND inspdate <= '%s'::date
          UNION ALL
          SELECT sitecode, inspdate, mattype
          FROM public.dblarv_insptrt_current
          WHERE action IN ('3', 'A', 'D')
            AND inspdate <= '%s'::date
        ) t
        LEFT JOIN public.mattype_list m ON t.mattype = m.mattype
        GROUP BY t.sitecode, t.mattype, t.inspdate
      ),
      
      LastTreatmentInfo AS (
        SELECT 
          sitecode,
          last_treatment_date,
          last_treatment_material,
          treatment_expiry,
          ROW_NUMBER() OVER (PARTITION BY sitecode ORDER BY last_treatment_date DESC) as rn
        FROM TreatmentEffects
      ),
      
      FinalTreatmentInfo AS (
        SELECT 
          sitecode,
          last_treatment_date,
          last_treatment_material,
          treatment_expiry
        FROM LastTreatmentInfo
        WHERE rn = 1
      )
      
      SELECT 
        a.sitecode,
        a.facility,
        a.priority,
        a.zone,
        a.acres,
        a.longitude,
        a.latitude,
        COALESCE(r.total_rainfall, 0) as total_rainfall,
        r.triggering_period,
        r.has_triggering_rainfall,
        i.last_inspection_date,
        i.last_larvae_count,
        t.last_treatment_date,
        t.last_treatment_material,
        t.treatment_expiry,
        CASE 
          -- Priority 1: Active Treatment (treatment applied and still effective)
          WHEN t.treatment_expiry IS NOT NULL AND '%s'::date <= t.treatment_expiry THEN 'Active Treatment'
          
          -- Priority 2: Treatment Expired -> Unknown (once treatment expires, go back to unknown)
          WHEN t.treatment_expiry IS NOT NULL AND '%s'::date > t.treatment_expiry THEN 'Unknown'
          
          -- Priority 3: Has triggering rainfall -> Needs Inspection
          WHEN r.has_triggering_rainfall = true THEN 'Needs Inspection'
          
          -- Priority 4: No triggering rainfall but has inspection -> check larvae count
          WHEN i.last_inspection_date IS NOT NULL AND r.has_triggering_rainfall = false THEN
            CASE 
              WHEN COALESCE(i.last_larvae_count, 0) >= %d THEN 'Needs Treatment'
              ELSE 'Under Threshold'
            END
          
          -- Default: Unknown (no triggering rainfall, no inspection)
          ELSE 'Unknown'
        END as site_status
      FROM ActiveAirSites a
      LEFT JOIN RainfallData r ON a.sitecode = r.sitecode
      LEFT JOIN RecentInspections i ON a.sitecode = i.sitecode
      LEFT JOIN FinalTreatmentInfo t ON a.sitecode = t.sitecode
      ORDER BY a.facility, a.sitecode
    ",
    analysis_date,          # For enddate filter
    facility_condition,
    priority_condition, 
    zone_condition,
    lookback_days,          # For generate_series limit 1
    analysis_date,          # For consecutive period start 1
    analysis_date,          # For consecutive period start 2
    lookback_days,          # For consecutive period length 1
    rain_threshold,         # For consecutive threshold check 1
    lookback_days,          # For generate_series limit 2
    analysis_date,          # For consecutive period start 3
    analysis_date,          # For consecutive period start 4
    lookback_days,          # For consecutive period length 2
    rain_threshold,         # For consecutive threshold check 2
    analysis_date,          # For rainfall period start
    analysis_date,          # For rainfall period end
    analysis_date,          # For inspection date filter 1
    analysis_date,          # For inspection date filter 2
    analysis_date,          # For treatment date filter 1
    analysis_date,          # For treatment date filter 2
    analysis_date,          # For treatment expiry check
    analysis_date,          # For treatment expired check
    treatment_threshold     # For larvae count threshold
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
  
  # Filter data if needed
  if (status_filter != "all") {
    data <- data[data$site_status == status_filter, ]
  }
  
  if (nrow(data) == 0) {
    return(leaflet() %>% addTiles() %>% setView(lng = -93.2, lat = 44.95, zoom = 10))
  }
  
  # Create popup text
  data$popup_text <- sprintf(
    "<b>%s</b><br/>Status: %s<br/>Facility: %s<br/>Priority: %s<br/>Zone: %s<br/>Rainfall: %.2f inches<br/>Last Inspection: %s<br/>Last Count: %s<br/>Last Treatment: %s<br/>Treatment Material: %s",
    data$sitecode,
    data$site_status,
    data$facility,
    data$priority,
    data$zone,
    data$total_rainfall,
    ifelse(is.na(data$last_inspection_date), "Never", 
           tryCatch(format(as.Date(data$last_inspection_date), "%Y-%m-%d"),
                   error = function(e) data$last_inspection_date)),
    ifelse(is.na(data$last_larvae_count), "N/A", data$last_larvae_count),
    ifelse(is.na(data$last_treatment_date), "Never", 
           tryCatch(format(as.Date(data$last_treatment_date), "%Y-%m-%d"),
                   error = function(e) data$last_treatment_date)),
    ifelse(is.na(data$last_treatment_material), "None", data$last_treatment_material)
  )
  
  # Color mapping for status
  status_colors <- c(
    "Unknown" = "#3498db",             # Blue - default status or post-treatment
    "Needs Inspection" = "#f39c12",    # Orange - rainfall triggered, needs inspection
    "Under Threshold" = "#2ecc71",     # Green - inspected, larvae count below threshold
    "Needs Treatment" = "#e74c3c",     # Red - inspected, larvae count above threshold
    "Active Treatment" = "#9b59b6"     # Purple - treatment is active/effective
  )
  
  data$color <- status_colors[data$site_status]
  
  # Create the map
  leaflet(data) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      color = ~color,
      fillColor = ~color,
      radius = 6,
      fillOpacity = 0.7,
      popup = ~popup_text,
      stroke = TRUE,
      weight = 2
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("#3498db", "#f39c12", "#2ecc71", "#e74c3c", "#9b59b6"),
      labels = c("Unknown", "Needs Inspection", "Under Threshold", "Needs Treatment", "Active Treatment"),
      title = "Site Status",
      opacity = 0.8
    ) %>%
    setView(lng = -93.2, lat = 44.95, zoom = 10)
}

# Function to create site details table
create_site_details_table <- function(data, status_filter = "all") {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Filter data if needed
  if (status_filter != "all") {
    data <- data[data$site_status == status_filter, ]
  }
  
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Format the data for display
  display_data <- data %>%
    mutate(
      # Format dates
      last_inspection_date = ifelse(is.na(last_inspection_date), "Never", 
                                   tryCatch(format(as.Date(last_inspection_date), "%Y-%m-%d"),
                                           error = function(e) as.character(last_inspection_date))),
      last_treatment_date = ifelse(is.na(last_treatment_date), "Never", 
                                  tryCatch(format(as.Date(last_treatment_date), "%Y-%m-%d"),
                                          error = function(e) as.character(last_treatment_date))),
      # Format larvae count
      last_larvae_count = ifelse(is.na(last_larvae_count), "N/A", as.character(last_larvae_count)),
      # Format treatment material
      last_treatment_material = ifelse(is.na(last_treatment_material), "None", as.character(last_treatment_material)),
      # Format rainfall
      total_rainfall = round(as.numeric(total_rainfall), 3),
      # Format acres
      acres = round(as.numeric(acres), 2)
    ) %>%
    select(
      "Site Code" = sitecode,
      "Facility" = facility,
      "Status" = site_status,
      "Priority" = priority,
      "Zone" = zone,
      "Rainfall (in)" = total_rainfall,
      "Last Inspection" = last_inspection_date,
      "Last Count" = last_larvae_count,
      "Last Treatment" = last_treatment_date,
      "Treatment Material" = last_treatment_material,
      "Acres" = acres
    )
  
  return(display_data)
}

# Function to create status chart
create_status_chart <- function(data, status_filter = "all") {
  if (nrow(data) == 0) {
    return(plotly::plot_ly() %>% 
           plotly::add_text(x = 0.5, y = 0.5, text = "No data available") %>%
           plotly::layout(showlegend = FALSE))
  }
  
  # Filter data if needed
  if (status_filter != "all") {
    data <- data[data$site_status == status_filter, ]
  }
  
  if (nrow(data) == 0) {
    return(plotly::plot_ly() %>% 
           plotly::add_text(x = 0.5, y = 0.5, text = "No data for selected filter") %>%
           plotly::layout(showlegend = FALSE))
  }
  
  # Create status summary
  status_counts <- table(data$site_status)
  status_df <- data.frame(
    Status = names(status_counts),
    Count = as.numeric(status_counts),
    stringsAsFactors = FALSE
  )
  
  # Color mapping
  status_colors <- c(
    "Unknown" = "#3498db",
    "Needs Inspection" = "#f39c12",
    "Under Threshold" = "#2ecc71",
    "Needs Treatment" = "#e74c3c",
    "Active Treatment" = "#9b59b6"
  )
  
  status_df$Color <- status_colors[status_df$Status]
  
  # Create bar chart
  p <- plotly::plot_ly(
    data = status_df,
    x = ~Status,
    y = ~Count,
    type = "bar",
    marker = list(color = ~Color),
    text = ~paste("Status:", Status, "<br>Count:", Count),
    textposition = "auto",
    hovertemplate = "%{text}<extra></extra>"
  ) %>%
  plotly::layout(
    title = list(text = "Air Sites Status Distribution", font = list(size = 16)),
    xaxis = list(title = "Status"),
    yaxis = list(title = "Number of Sites"),
    showlegend = FALSE,
    margin = list(t = 50, b = 100)
  )
  
  return(p)
}

# Function to create synthetic test data for flow testing
create_synthetic_flow_data <- function(total_sites = 50, analysis_date = Sys.Date()) {
  library(dplyr)
  set.seed(123) # For reproducible results
  
  # Generate site codes
  sitecodes <- sprintf("TEST-%03d", 1:total_sites)
  
  # Generate base site info
  sites <- data.frame(
    sitecode = sitecodes,
    facility = sample(c("Test_A", "Test_B", "Test_C"), total_sites, replace = TRUE),
    priority = sample(c("RED", "YELLOW"), total_sites, replace = TRUE, prob = c(0.7, 0.3)),
    zone = sample(1:3, total_sites, replace = TRUE),
    acres = round(runif(total_sites, 0.5, 5.0), 2),
    longitude = runif(total_sites, -93.5, -92.8),
    latitude = runif(total_sites, 44.8, 45.2),
    stringsAsFactors = FALSE
  )
  
  # Generate rainfall scenarios
  sites$rainfall_scenario <- sample(c("none", "low", "medium", "high"), total_sites, 
                                   replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2))
  
  # Generate inspection scenarios
  sites$inspection_scenario <- sample(c("never", "old", "recent_low", "recent_high"), 
                                     total_sites, replace = TRUE, prob = c(0.3, 0.2, 0.3, 0.2))
  
  # Generate treatment scenarios
  sites$treatment_scenario <- sample(c("never", "expired", "active"), total_sites, 
                                    replace = TRUE, prob = c(0.6, 0.2, 0.2))
  
  # Calculate actual values based on scenarios
  sites$total_rainfall <- 0
  for (i in 1:nrow(sites)) {
    if (sites$rainfall_scenario[i] == "none") {
      sites$total_rainfall[i] <- runif(1, 0, 0.3)
    } else if (sites$rainfall_scenario[i] == "low") {
      sites$total_rainfall[i] <- runif(1, 0.3, 0.8)
    } else if (sites$rainfall_scenario[i] == "medium") {
      sites$total_rainfall[i] <- runif(1, 0.8, 1.5)
    } else if (sites$rainfall_scenario[i] == "high") {
      sites$total_rainfall[i] <- runif(1, 1.5, 3.0)
    }
  }
  
  sites$has_triggering_rainfall <- sites$total_rainfall >= 1.0
  
  # Generate inspection data
  sites$last_inspection_date <- as.Date(NA)
  sites$last_larvae_count <- as.numeric(NA)
  
  for (i in 1:nrow(sites)) {
    if (sites$inspection_scenario[i] == "old") {
      sites$last_inspection_date[i] <- analysis_date - sample(60:200, 1)
      sites$last_larvae_count[i] <- round(runif(1, 0, 10))
    } else if (sites$inspection_scenario[i] == "recent_low") {
      sites$last_inspection_date[i] <- analysis_date - sample(1:30, 1)
      sites$last_larvae_count[i] <- round(runif(1, 0, 1))
    } else if (sites$inspection_scenario[i] == "recent_high") {
      sites$last_inspection_date[i] <- analysis_date - sample(1:30, 1)
      sites$last_larvae_count[i] <- round(runif(1, 2, 15))
    }
  }
  
  # Generate treatment data
  sites$last_treatment_date <- as.Date(NA)
  sites$last_treatment_material <- as.character(NA)
  sites$treatment_expiry <- as.Date(NA)
  
  for (i in 1:nrow(sites)) {
    if (sites$treatment_scenario[i] == "expired") {
      sites$last_treatment_date[i] <- analysis_date - sample(45:90, 1)
      sites$last_treatment_material[i] <- sample(c("Bti_gran", "Alt_P35", "VectoLex"), 1)
      sites$treatment_expiry[i] <- sites$last_treatment_date[i] + 30
    } else if (sites$treatment_scenario[i] == "active") {
      sites$last_treatment_date[i] <- analysis_date - sample(1:30, 1)
      sites$last_treatment_material[i] <- sample(c("Bti_gran", "Alt_P35", "VectoLex"), 1)
      sites$treatment_expiry[i] <- sites$last_treatment_date[i] + 45
    }
  }
  
  # Generate triggering period JSON
  sites$triggering_period <- ifelse(
    sites$has_triggering_rainfall,
    sprintf('{"start_date": "%s", "end_date": "%s", "total_rain": %.2f, "triggered": true}',
            analysis_date - 10, analysis_date - 8, sites$total_rainfall),
    '{"triggered": false}'
  )
  
  # Calculate site status based on business logic
  sites$site_status <- "Unknown"
  
  for (i in 1:nrow(sites)) {
    # Priority 1: Active treatment (treatment applied and still effective)
    if (!is.na(sites$treatment_expiry[i]) && analysis_date <= sites$treatment_expiry[i]) {
      sites$site_status[i] <- "Active Treatment"
    }
    # Priority 2: Treatment expired -> Unknown (once treatment expires, go back to unknown)
    else if (!is.na(sites$treatment_expiry[i]) && analysis_date > sites$treatment_expiry[i]) {
      sites$site_status[i] <- "Unknown"
    }
    # Priority 3: Has triggering rainfall -> Needs Inspection (rainfall always triggers inspection)
    else if (sites$has_triggering_rainfall[i]) {
      sites$site_status[i] <- "Needs Inspection"
    }
    # Priority 4: No triggering rainfall but has inspection -> check larvae count
    else if (!is.na(sites$last_inspection_date[i]) && !sites$has_triggering_rainfall[i]) {
      if (!is.na(sites$last_larvae_count[i]) && sites$last_larvae_count[i] >= 2) {
        sites$site_status[i] <- "Needs Treatment"
      } else {
        sites$site_status[i] <- "Under Threshold"
      }
    }
    # Default: Unknown (no triggering rainfall, no inspection)
  }
  
  return(sites)
}

# Function to create flow testing visualizations
create_flow_test_results <- function(synthetic_data, analysis_date = Sys.Date(), 
                                    lookback_period = 3, rain_threshold = 1.0, 
                                    treatment_threshold = 2) {
  
  # Status distribution
  status_dist <- table(synthetic_data$site_status)
  
  # Create flow diagram data
  flow_steps <- data.frame(
    step = 1:6,
    status = c("Unknown", "Needs Inspection", "Under Threshold", "Needs Treatment", "Active Treatment", "Unknown (Post-Treatment)"),
    count = c(
      sum(synthetic_data$site_status == "Unknown"),
      sum(synthetic_data$site_status == "Needs Inspection"),
      sum(synthetic_data$site_status == "Under Threshold"),
      sum(synthetic_data$site_status == "Needs Treatment"),
      sum(synthetic_data$site_status == "Active Treatment"),
      0  # Will be calculated based on expired treatments
    ),
    description = c(
      "Default status or no triggering rainfall",
      "Triggering rainfall occurred, awaiting inspection",
      "Inspected, larvae count below threshold",
      "Inspected, larvae count above threshold",
      "Treatment applied and still effective",
      "Treatment expired, returns to Unknown"
    )
  )
  
  # Create transition matrix
  transitions <- data.frame(
    from = c("Unknown", "Unknown", "Needs Inspection", "Needs Inspection", "Under Threshold", "Needs Treatment", "Active Treatment"),
    to = c("Needs Inspection", "Needs Inspection", "Under Threshold", "Needs Treatment", "Needs Inspection", "Active Treatment", "Unknown"),
    trigger = c(
      "Rainfall ≥ threshold",
      "New rainfall after treatment expires",
      "Inspection: count < threshold",
      "Inspection: count ≥ threshold", 
      "New rainfall ≥ threshold",
      "Treatment applied",
      "Treatment expires"
    ),
    count = c(
      sum(synthetic_data$site_status == "Needs Inspection"),
      0,
      sum(synthetic_data$site_status == "Under Threshold"),
      sum(synthetic_data$site_status == "Needs Treatment"),
      0,
      sum(synthetic_data$site_status == "Active Treatment"),
      0
    )
  )
  
  return(list(
    status_distribution = status_dist,
    flow_steps = flow_steps,
    transitions = transitions,
    sample_sites = synthetic_data,
    parameters = list(
      analysis_date = analysis_date,
      lookback_period = lookback_period,
      rain_threshold = rain_threshold,
      treatment_threshold = treatment_threshold
    )
  ))
}