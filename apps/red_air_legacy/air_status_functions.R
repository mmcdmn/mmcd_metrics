# Air Site Status Functions for Red Air Pipeline App

# Function to get available zones from the database
get_available_zones <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(c("1", "2"))
  
  tryCatch({
    query <- "
      SELECT DISTINCT g.zone 
      FROM public.gis_sectcode g
      WHERE g.zone IS NOT NULL
      ORDER BY g.zone
    "
    
    result <- dbGetQuery(con, query)
    safe_disconnect(con)
    
    if (nrow(result) > 0) {
      zones <- result$zone
      choices <- setNames(zones, paste0("P", zones))
      return(choices)
    } else {
      return(c("1", "2"))
    }
    
  }, error = function(e) {
    if (exists("con") && !is.null(con)) {
      safe_disconnect(con)
    }
    warning(paste("Error getting zones:", e$message))
    return(c("1", "2"))
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
