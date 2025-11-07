# Enhanced Air Status Functions
# Includes inspection logic with larvae threshold

# Load required libraries for this file
suppressPackageStartupMessages({
  library(leaflet)
})

get_air_sites_data_enhanced <- function(analysis_date, facility_filter = NULL, priority_filter = NULL, zone_filter = NULL, larvae_threshold = 2, bit_effect_days_override = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
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

    # Build BIT effect days override condition
    # If override is provided, use it for Bti_gran (BIT), otherwise use database value
    bit_override_sql <- ""
    if (!is.null(bit_effect_days_override) && !is.na(bit_effect_days_override) && bit_effect_days_override > 0) {
      bit_override_sql <- sprintf("
          -- Override effect days for BIT (Bti_gran)
          CASE 
            WHEN rt.mattype = 'Bti_gran' THEN %d
            WHEN mt.effect_days IS NOT NULL AND mt.effect_days > 0 THEN mt.effect_days
            ELSE NULL
          END", bit_effect_days_override)
    } else {
      bit_override_sql <- "
          -- Use database effect days
          CASE 
            WHEN mt.effect_days IS NOT NULL AND mt.effect_days > 0 THEN mt.effect_days
            ELSE NULL
          END"
    }

    # Enhanced query with inspection and treatment data
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
      
      -- Get most recent inspections (actions 2, 4)
      RecentInspections AS (
        SELECT 
          i.sitecode,
          MAX(i.inspdate) as last_inspection_date,
          i.numdip,
          ROW_NUMBER() OVER (PARTITION BY i.sitecode ORDER BY MAX(i.inspdate) DESC) as rn
        FROM dblarv_insptrt_current i
        WHERE i.inspdate <= '%s'::date
          AND i.action IN ('2', '4')  -- Inspection actions only
          AND i.sitecode IN (SELECT sitecode FROM ActiveAirSites)
        GROUP BY i.sitecode, i.numdip, i.inspdate
      ),
      
      -- Get most recent treatments (actions 3, A, D)
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
          -- Calculate treatment expiry based on material effect days (with BIT override if specified)
          CASE 
            WHEN %s IS NOT NULL
            THEN rt.last_treatment_date + INTERVAL '1 day' * (%s)
            ELSE NULL
          END as treatment_expiry
        FROM RecentTreatments rt
        LEFT JOIN mattype_list mt ON rt.mattype = mt.mattype
        WHERE rt.rn = 1
      ),
      
      -- Get inspection details
      InspectionInfo AS (
        SELECT 
          ri.sitecode,
          ri.last_inspection_date,
          ri.numdip
        FROM RecentInspections ri
        WHERE ri.rn = 1
      )
      
      SELECT 
        a.sitecode,
        a.facility,
        a.priority,
        a.zone,
        a.acres,
        a.longitude,
        a.latitude,
        t.last_treatment_date,
        t.last_treatment_material,
        t.treatment_expiry,
        i.last_inspection_date,
        i.numdip as last_larvae_count
      FROM ActiveAirSites a
      LEFT JOIN TreatmentInfo t ON a.sitecode = t.sitecode
      LEFT JOIN InspectionInfo i ON a.sitecode = i.sitecode
      ORDER BY a.sitecode
    ", analysis_date, facility_condition, priority_condition, zone_condition, 
       analysis_date, analysis_date, bit_override_sql, bit_override_sql)
    
    result <- dbGetQuery(con, query)
    
    # Apply status logic in R for better control
    if (nrow(result) > 0) {
      result <- apply_site_status_logic(result, analysis_date, larvae_threshold)
    }
    
    return(result)
    
  }, error = function(e) {
    cat("Error in get_air_sites_data_enhanced:", e$message, "\n")
    return(data.frame())
  }, finally = {
    if (!is.null(con)) dbDisconnect(con)
  })
}

# Apply site status logic based on inspections and treatments
apply_site_status_logic <- function(data, analysis_date, larvae_threshold = 2) {
  analysis_date <- as.Date(analysis_date)
  
  # Initialize status as Unknown
  data$site_status <- "Unknown"
  
  for (i in 1:nrow(data)) {
    site <- data[i, ]
    
    # Get dates as Date objects
    last_treatment_date <- if (!is.na(site$last_treatment_date)) as.Date(site$last_treatment_date) else NULL
    treatment_expiry <- if (!is.na(site$treatment_expiry)) as.Date(site$treatment_expiry) else NULL
    last_inspection_date <- if (!is.na(site$last_inspection_date)) as.Date(site$last_inspection_date) else NULL
    
    # Check if treatment is currently active
    treatment_active <- !is.null(treatment_expiry) && treatment_expiry > analysis_date
    
    # Priority 1: Active Treatment (cannot be overridden by re-inspection)
    if (treatment_active) {
      data$site_status[i] <- "Active Treatment"
      next
    }
    
    # Check inspection status if we have inspection data
    if (!is.null(last_inspection_date)) {
      larvae_count <- if (!is.na(site$last_larvae_count)) site$last_larvae_count else 0
      days_since_inspection <- as.numeric(analysis_date - last_inspection_date)
      
      # If inspection is within 7 days
      if (days_since_inspection <= 7) {
        if (larvae_count >= larvae_threshold) {
          data$site_status[i] <- "Needs Treatment"
        } else {
          data$site_status[i] <- "Inspected"
        }
      }
      # If inspection is older than 7 days, status expires back to Unknown
      # (this is already set as default)
    }
  }
  
  # Format dates for display
  data$last_treatment_date_display <- ifelse(
    is.na(data$last_treatment_date), 
    "None", 
    format(as.Date(data$last_treatment_date), "%Y-%m-%d")
  )
  
  data$last_inspection_date_display <- ifelse(
    is.na(data$last_inspection_date), 
    "None", 
    format(as.Date(data$last_inspection_date), "%Y-%m-%d")
  )
  
  return(data)
}

# Create enhanced site map with new status categories
create_site_map_enhanced <- function(data) {
  if (nrow(data) == 0) {
    return(leaflet() %>% 
      addTiles() %>%
      setView(lng = -93.2, lat = 44.9, zoom = 10))
  }
  
  # Define colors for all status types using shared color scheme
  status_color_map <- get_status_color_map()
  status_colors <- c(
    "Active Treatment" = as.character(status_color_map[["Active Treatment"]]),
    "Needs Treatment" = as.character(status_color_map[["Needs Treatment"]]),
    "Inspected" = as.character(status_color_map[["Under Threshold"]]),  # Reuse green color
    "Unknown" = as.character(status_color_map[["Unknown"]])
  )
  
  # Create color palette
  data$color <- status_colors[data$site_status]
  
  # Get unique statuses for legend
  unique_statuses <- unique(data$site_status)
  legend_colors <- status_colors[unique_statuses]
  
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
        "Status: ", site_status, "<br/>",
        "Last Inspection: ", last_inspection_date_display, "<br/>",
        "Larvae Count: ", ifelse(is.na(last_larvae_count), "N/A", last_larvae_count), "<br/>",
        "Last Treatment: ", last_treatment_date_display, "<br/>",
        "Material Used: ", ifelse(is.na(last_treatment_material), "None", last_treatment_material)
      ),
      layerId = ~sitecode
    ) %>%
    addLegend(
      position = "bottomright",
      colors = legend_colors,
      labels = unique_statuses,
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

# Create enhanced summary stats
create_summary_stats_enhanced <- function(data) {
  if (nrow(data) == 0) {
    return(list(
      total_sites = 0,
      active_treatment = 0,
      needs_treatment = 0,
      inspected = 0,
      unknown = 0
    ))
  }
  
  summary <- list(
    total_sites = nrow(data),
    active_treatment = sum(data$site_status == "Active Treatment", na.rm = TRUE),
    needs_treatment = sum(data$site_status == "Needs Treatment", na.rm = TRUE),
    inspected = sum(data$site_status == "Inspected", na.rm = TRUE),
    unknown = sum(data$site_status == "Unknown", na.rm = TRUE)
  )
  
  return(summary)
}

# Create site details panel
create_site_details_panel <- function(site_data) {
  if (nrow(site_data) == 0) {
    return(data.frame())
  }
  
  # Format larvae count for display
  site_data$larvae_count_display <- ifelse(
    is.na(site_data$last_larvae_count), 
    "N/A", 
    as.character(site_data$last_larvae_count)
  )
  
  # Select and rename columns for display (using pre-formatted date fields)
  display_data <- site_data[, c(
    "sitecode", "facility", "priority", "zone", "acres", 
    "site_status", "last_inspection_date_display", "larvae_count_display",
    "last_treatment_date_display", "last_treatment_material"
  )]
  
  colnames(display_data) <- c(
    "Site Code", "Facility", "Priority", "Zone", "Acres", 
    "Status", "Last Inspection", "Larvae Count", 
    "Last Treatment", "Treatment Material"
  )
  
  return(display_data)
}

# Create treatment process tracking summary
create_treatment_process_summary <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame(
      Facility = character(0),
      `Total Sites` = numeric(0),
      `Unknown` = numeric(0),
      `Needs Treatment` = numeric(0),
      `Active Treatment` = numeric(0),
      `Inspected` = numeric(0),
      `Treatment Rate` = character(0),
      check.names = FALSE
    ))
  }
  
  # Group by facility and calculate status counts
  process_summary <- data %>%
    group_by(facility) %>%
    summarise(
      total_sites = n(),
      unknown = sum(site_status == "Unknown", na.rm = TRUE),
      needs_treatment = sum(site_status == "Needs Treatment", na.rm = TRUE),
      active_treatment = sum(site_status == "Active Treatment", na.rm = TRUE),
      inspected = sum(site_status == "Inspected", na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      # Calculate treatment rate: active treatments / (needs treatment + active treatment)
      treatment_rate = ifelse(
        (needs_treatment + active_treatment) > 0,
        round((active_treatment / (needs_treatment + active_treatment)) * 100, 1),
        0
      ),
      treatment_rate_display = paste0(treatment_rate, "%")
    )
  
  # Rename columns for display
  process_summary_display <- process_summary %>%
    select(facility, total_sites, unknown, needs_treatment, active_treatment, inspected, treatment_rate_display)
  
  colnames(process_summary_display) <- c(
    "Facility", "Total Sites", "Unknown", "Needs Treatment", 
    "Active Treatment", "Inspected", "Treatment Rate"
  )
  
  return(process_summary_display)
}

# Create treatment process flow chart
create_treatment_flow_chart <- function(data) {
  if (nrow(data) == 0) {
    return(plot_ly() %>%
      add_annotations(
        text = "No data available",
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE
      ))
  }
  
  # Create facility-level summary
  facility_summary <- data %>%
    group_by(facility, site_status) %>%
    summarise(count = n(), .groups = 'drop') %>%
    tidyr::pivot_wider(names_from = site_status, values_from = count, values_fill = 0)
  
  # Ensure all columns exist
  if (!"Unknown" %in% colnames(facility_summary)) facility_summary$Unknown <- 0
  if (!"Needs Treatment" %in% colnames(facility_summary)) facility_summary$`Needs Treatment` <- 0
  if (!"Active Treatment" %in% colnames(facility_summary)) facility_summary$`Active Treatment` <- 0
  if (!"Inspected" %in% colnames(facility_summary)) facility_summary$Inspected <- 0
  
  # Get colors from db_helpers
  status_color_map <- get_status_color_map()
  colors <- list(
    "Unknown" = as.character(status_color_map[["Unknown"]]),
    "Needs Treatment" = as.character(status_color_map[["Needs Treatment"]]),
    "Active Treatment" = as.character(status_color_map[["Active Treatment"]]),
    "Inspected" = as.character(status_color_map[["Under Threshold"]])
  )
  
  # Create stacked bar chart
  p <- plot_ly(facility_summary, x = ~facility, y = ~Unknown, type = 'bar', 
               name = 'Unknown', marker = list(color = colors$Unknown)) %>%
    add_trace(y = ~`Needs Treatment`, name = 'Needs Treatment', 
              marker = list(color = colors$`Needs Treatment`)) %>%
    add_trace(y = ~`Active Treatment`, name = 'Active Treatment', 
              marker = list(color = colors$`Active Treatment`)) %>%
    add_trace(y = ~Inspected, name = 'Inspected', 
              marker = list(color = colors$Inspected)) %>%
    layout(
      title = "Treatment Process Flow by Facility",
      xaxis = list(title = "Facility"),
      yaxis = list(title = "Number of Sites"),
      barmode = 'stack',
      showlegend = TRUE
    )
  
  return(p)
}

# Get available treatment materials
get_treatment_materials <- function(include_all = TRUE) {
  con <- get_db_connection()
  if (is.null(con)) return(character(0))
  
  tryCatch({
    query <- "
      SELECT DISTINCT mattype 
      FROM mattype_list 
      WHERE mattype IS NOT NULL AND mattype != ''
      ORDER BY mattype
    "
    result <- dbGetQuery(con, query)
    materials <- result$mattype
    
    # Add "All" option at the beginning if requested
    if (include_all) {
      materials <- c("All", materials)
    }
    
    return(materials)
  }, error = function(e) {
    cat("Error getting treatment materials:", e$message, "\n")
    if (include_all) {
      return("All")
    } else {
      return(character(0))
    }
  }, finally = {
    if (!is.null(con)) dbDisconnect(con)
  })
}

# Create treatment efficiency metrics
create_treatment_efficiency_metrics <- function(data) {
  if (nrow(data) == 0) {
    return(list(
      total_sites_needing_action = 0,
      sites_receiving_treatment = 0,
      treatment_efficiency = "0%",
      inspection_coverage = "0%"
    ))
  }
  
  # Calculate efficiency metrics
  total_sites <- nrow(data)
  sites_needing_treatment <- sum(data$site_status == "Needs Treatment", na.rm = TRUE)
  sites_receiving_treatment <- sum(data$site_status == "Active Treatment", na.rm = TRUE)
  sites_inspected <- sum(data$site_status == "Inspected", na.rm = TRUE)
  sites_unknown <- sum(data$site_status == "Unknown", na.rm = TRUE)
  
  # Sites that have been inspected or treated
  sites_with_action <- sites_needing_treatment + sites_receiving_treatment + sites_inspected
  
  # Treatment efficiency: active treatments / (needs treatment + active treatment)
  sites_requiring_treatment <- sites_needing_treatment + sites_receiving_treatment
  treatment_efficiency <- if (sites_requiring_treatment > 0) {
    round((sites_receiving_treatment / sites_requiring_treatment) * 100, 1)
  } else {
    0
  }
  
  # Inspection coverage: (inspected + needs treatment + active treatment) / total sites
  inspection_coverage <- if (total_sites > 0) {
    round((sites_with_action / total_sites) * 100, 1)
  } else {
    0
  }
  
  return(list(
    total_sites_needing_action = sites_requiring_treatment,
    sites_receiving_treatment = sites_receiving_treatment,
    treatment_efficiency = paste0(treatment_efficiency, "%"),
    inspection_coverage = paste0(inspection_coverage, "%")
  ))
}
