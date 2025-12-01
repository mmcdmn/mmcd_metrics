# Air Sites Display Functions
# Functions for creating maps, charts, and UI elements

# Load required libraries
suppressPackageStartupMessages({
  library(leaflet)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(shiny)
})

# Create interactive site map with status colors
create_site_map <- function(data) {
  if (nrow(data) == 0) {
    return(leaflet() %>% 
      addTiles() %>%
      setView(lng = -93.2, lat = 44.9, zoom = 10))
  }
  
  # Define colors for all status types using shared color scheme with Needs ID
  status_color_map <- get_status_color_map()
  status_colors <- c(
    "Active Treatment" = as.character(status_color_map[["Active Treatment"]]),
    "Needs Treatment" = as.character(status_color_map[["Needs Treatment"]]),
    "Inspected" = as.character(status_color_map[["Inspected"]]),
    "Needs ID" = as.character(status_color_map[["Needs ID"]]),
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
        "Lab Status: ", lab_status_display, "<br/>",
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

# Create summary statistics panel
create_summary_stats <- function(data) {
   if (nrow(data) == 0) {
    return(list(
      total_sites = 0,
      active_treatment = 0,
      needs_treatment = 0,
      inspected = 0,
      in_lab = 0,
      unknown = 0
    ))
  }
  
  summary <- list(
    total_sites = nrow(data),
    active_treatment = sum(data$site_status == "Active Treatment", na.rm = TRUE),
    needs_treatment = sum(data$site_status == "Needs Treatment", na.rm = TRUE),
    inspected = sum(data$site_status == "Inspected", na.rm = TRUE),
    in_lab = sum(data$site_status == "Needs ID", na.rm = TRUE),
    unknown = sum(data$site_status == "Unknown", na.rm = TRUE)
  )
  
  return(summary)
}

# Create site details table
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
    "lab_status_display", "last_treatment_date_display", "last_treatment_material"
  )]
  
  colnames(display_data) <- c(
    "Site Code", "Facility", "Priority", "Zone", "Acres", 
    "Status", "Last Inspection", "Larvae Count", "Lab Status",
    "Last Treatment", "Treatment Material"
  )
  
  return(display_data)
}

# Create treatment process summary with value boxes
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
  
  # Map facility short names to full names
  process_summary <- map_facility_names(process_summary, "facility")
  
  # Rename columns for display (reorder to match status order)
  process_summary_display <- process_summary %>%
    select(facility_display, total_sites, unknown, inspected, needs_treatment, active_treatment, treatment_rate_display)
  
  colnames(process_summary_display) <- c(
    "Facility", "Total Sites", "Not Insp", "Insp", "Needs Treatment", 
    "Active Treatment", "Treatment Rate"
  )
  
  return(process_summary_display)
}

# Create treatment flow chart
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
  
  # Map facility short names to full names
  facility_summary <- map_facility_names(facility_summary, "facility")
  
  # Ensure all columns exist
  if (!"Unknown" %in% colnames(facility_summary)) facility_summary$Unknown <- 0
  if (!"Inspected" %in% colnames(facility_summary)) facility_summary$Inspected <- 0
  if (!"Needs ID" %in% colnames(facility_summary)) facility_summary$`Needs ID` <- 0
  if (!"Needs Treatment" %in% colnames(facility_summary)) facility_summary$`Needs Treatment` <- 0
  if (!"Active Treatment" %in% colnames(facility_summary)) facility_summary$`Active Treatment` <- 0
  
  # Get colors from db_helpers
  status_color_map <- get_status_color_map()
  colors <- list(
    "Unknown" = as.character(status_color_map[["Unknown"]]),
    "Inspected" = as.character(status_color_map[["Inspected"]]),
    "Needs ID" = as.character(status_color_map[["Needs ID"]]),
    "Needs Treatment" = as.character(status_color_map[["Needs Treatment"]]),
    "Active Treatment" = as.character(status_color_map[["Active Treatment"]])
  )
  
  # Create stacked bar chart (ordered: Not Insp, Insp, Needs ID, Needs Treatment, Active Treatment)
  p <- plot_ly(facility_summary, x = ~facility_display, y = ~Unknown, type = 'bar', 
               name = 'Not Insp', marker = list(color = colors$Unknown)) %>%
    add_trace(y = ~Inspected, name = 'Insp', 
              marker = list(color = colors$Inspected)) %>%
    add_trace(y = ~`Needs ID`, name = 'Needs ID', 
              marker = list(color = colors$`Needs ID`)) %>%
    add_trace(y = ~`Needs Treatment`, name = 'Needs Treatment', 
              marker = list(color = colors$`Needs Treatment`)) %>%
    add_trace(y = ~`Active Treatment`, name = 'Active Treatment', 
              marker = list(color = colors$`Active Treatment`)) %>%
    layout(
      title = list(text = "Treatment Process Flow by Facility", font = list(size = 20)),
      xaxis = list(title = list(text = "Facility", font = list(size = 18)), tickfont = list(size = 16)),
      yaxis = list(title = list(text = "Number of Sites", font = list(size = 18)), tickfont = list(size = 16)),
      barmode = 'stack',
      showlegend = TRUE,
      legend = list(font = list(size = 16))
    )
  
  return(p)
}

# Placeholder historical functions
create_historical_summary_metrics <- function(data) {
  return(list(
    total_historical = nrow(data),
    historical_rate = "0%"
  ))
}

create_historical_summary_chart <- function(data) {
  return(plot_ly() %>%
    add_annotations(
      text = "Historical analysis not implemented",
      x = 0.5, y = 0.5,
      xref = "paper", yref = "paper",
      showarrow = FALSE
    ))
}