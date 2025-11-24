# Cattail Treatments - Display Functions
# Functions for creating charts, tables, maps, and value boxes

library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(sf)
library(stringr)

# Function to create treatment progress chart
create_treatment_progress_chart <- function(data, group_by = "facility") {
  if (nrow(data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No data available"), size = 6) +
           theme_void())
  }
  
  # Calculate dynamic height: 80 pixels per y-axis item
  n_items <- nrow(data)
  dynamic_height <- max(400, n_items * 80)
  
  # Get status colors from db_helpers
  status_colors <- get_status_colors()
  
  # Prepare data for stacked bar chart
  chart_data <- data %>%
    select(display_name, total_sites, treatments_applied, active_treatments, planned_treatments) %>%
    mutate(
      untreated_sites = pmax(0, total_sites - treatments_applied),
      inactive_treatments = pmax(0, treatments_applied - active_treatments)
    ) %>%
    # Reshape for stacked chart
    tidyr::pivot_longer(
      cols = c(active_treatments, inactive_treatments, untreated_sites, planned_treatments),
      names_to = "category",
      values_to = "count"
    ) %>%
    mutate(
      category = factor(category, levels = c("active_treatments", "inactive_treatments", "untreated_sites", "planned_treatments"),
                       labels = c("Active Treatments", "Inactive Treatments", "Untreated Sites", "Planned Treatments"))
    )
  
  # Define colors for categories
  category_colors <- c(
    "Active Treatments" = unname(status_colors["active"]),
    "Inactive Treatments" = unname(status_colors["expired"]),
    "Untreated Sites" = "#95a5a6",
    "Planned Treatments" = unname(status_colors["planned"])
  )
  
  # Create the chart
  p <- ggplot(chart_data, aes(x = reorder(display_name, -count), y = count, fill = category)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    coord_flip() +
    scale_fill_manual(values = category_colors, name = "Status") +
    labs(
      title = "Cattail Treatment Progress",
      x = case_when(
        group_by == "facility" ~ "Facility",
        group_by == "foreman" ~ "FOS",
        group_by == "sectcode" ~ "Section",
        TRUE ~ "Group"
      ),
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 18, family = "Arial"),
      axis.title = element_text(face = "bold", size = 14, family = "Arial"),
      axis.text = element_text(size = 13, family = "Arial"),
      legend.title = element_text(face = "bold", size = 12, family = "Arial"),
      legend.text = element_text(size = 11, family = "Arial"),
      legend.position = "bottom"
    )
  
  return(ggplotly(p, tooltip = c("x", "y", "fill")) %>%
         layout(height = dynamic_height))
}

# Function to create treatment timeline chart
create_treatment_timeline <- function(treatments_data, group_by = "facility") {
  if (nrow(treatments_data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No treatment data available"), size = 6) +
           theme_void())
  }
  
  # Prepare data for timeline
  timeline_data <- treatments_data %>%
    mutate(
      treatment_date = as.Date(inspdate),
      treatment_month = floor_date(treatment_date, "month"),
      group_label = case_when(
        group_by == "facility" ~ facility_display,
        group_by == "foreman" ~ paste("FOS", fosarea),
        group_by == "sectcode" ~ paste("Section", sectcode),
        TRUE ~ "All"
      )
    ) %>%
    group_by(treatment_month, group_label) %>%
    summarise(
      treatments = n(),
      acres_treated = sum(treated_acres, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create timeline chart
  p <- ggplot(timeline_data, aes(x = treatment_month, y = treatments, color = group_label)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    labs(
      title = "Cattail Treatment Timeline",
      x = "Month",
      y = "Number of Treatments",
      color = str_to_title(group_by)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 18, family = "Arial"),
      axis.title = element_text(face = "bold", size = 14, family = "Arial"),
      axis.text = element_text(size = 13, family = "Arial"),
      legend.title = element_text(face = "bold", size = 12, family = "Arial"),
      legend.text = element_text(size = 11, family = "Arial"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  
  return(ggplotly(p, tooltip = c("x", "y", "colour")))
}

# Function to create efficacy analysis chart
create_efficacy_chart <- function(efficacy_data, treatments_data) {
  if (nrow(efficacy_data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No efficacy data available"), size = 6) +
           theme_void())
  }
  
  # Join efficacy with treatment data
  efficacy_with_treatment <- efficacy_data %>%
    left_join(treatments_data %>% select(sitecode, inspdate, matcode, material_name),
              by = c("sitecode", "treatment_date" = "inspdate"))
  
  # Create efficacy rating distribution
  p <- ggplot(efficacy_with_treatment, aes(x = efficacy_rating, fill = material_name)) +
    geom_bar(alpha = 0.8, position = "dodge") +
    labs(
      title = "Treatment Efficacy by Material",
      x = "Efficacy Rating",
      y = "Number of Evaluations",
      fill = "Material"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 18, family = "Arial"),
      axis.title = element_text(face = "bold", size = 14, family = "Arial"),
      axis.text = element_text(size = 13, family = "Arial"),
      legend.title = element_text(face = "bold", size = 12, family = "Arial"),
      legend.text = element_text(size = 11, family = "Arial"),
      legend.position = "bottom"
    )
  
  return(ggplotly(p, tooltip = c("x", "y", "fill")))
}

# Function to create treatment planning calendar
create_planning_calendar <- function(plans_data) {
  if (nrow(plans_data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No planning data available"), size = 6) +
           theme_void())
  }
  
  # Prepare calendar data
  calendar_data <- plans_data %>%
    mutate(
      plan_date = as.Date(planned_date),
      plan_week = floor_date(plan_date, "week"),
      plan_status_color = case_when(
        plan_status == "Overdue" ~ "#e74c3c",
        plan_status == "Due This Week" ~ "#f39c12", 
        plan_status == "Due This Month" ~ "#f1c40f",
        plan_status == "Completed" ~ "#27ae60",
        plan_status == "Cancelled" ~ "#95a5a6",
        TRUE ~ "#3498db"
      )
    ) %>%
    group_by(plan_week, plan_status) %>%
    summarise(
      planned_treatments = n(),
      planned_acres = sum(planned_acres, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create calendar view
  p <- ggplot(calendar_data, aes(x = plan_week, y = planned_treatments, fill = plan_status)) +
    geom_col(alpha = 0.8) +
    labs(
      title = "Treatment Planning Calendar",
      x = "Week",
      y = "Planned Treatments",
      fill = "Plan Status"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 18, family = "Arial"),
      axis.title = element_text(face = "bold", size = 14, family = "Arial"),
      axis.text = element_text(size = 13, family = "Arial"),
      legend.title = element_text(face = "bold", size = 12, family = "Arial"),
      legend.text = element_text(size = 11, family = "Arial"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")
  
  return(ggplotly(p, tooltip = c("x", "y", "fill")))
}

# Function to create value boxes
create_cattail_value_boxes <- function(data, treatments_data, plans_data) {
  
  # Calculate totals
  total_sites <- sum(data$total_sites, na.rm = TRUE)
  total_acres <- sum(data$total_acres, na.rm = TRUE)
  
  # Treatment statistics
  total_treatments <- sum(data$treatments_applied, na.rm = TRUE)
  active_treatments <- sum(data$active_treatments, na.rm = TRUE)
  acres_treated <- sum(data$acres_treated, na.rm = TRUE)
  
  # Planning statistics
  total_plans <- sum(data$planned_treatments, na.rm = TRUE)
  overdue_plans <- sum(data$overdue_plans, na.rm = TRUE)
  upcoming_plans <- sum(data$upcoming_plans, na.rm = TRUE)
  
  # Calculate percentages
  treatment_coverage <- if (total_sites > 0) round(100 * total_treatments / total_sites, 1) else 0
  active_percentage <- if (total_treatments > 0) round(100 * active_treatments / total_treatments, 1) else 0
  
  return(list(
    total_sites = total_sites,
    total_acres = round(total_acres, 1),
    total_treatments = total_treatments,
    active_treatments = active_treatments,
    acres_treated = round(acres_treated, 1),
    total_plans = total_plans,
    overdue_plans = overdue_plans,
    upcoming_plans = upcoming_plans,
    treatment_coverage = treatment_coverage,
    active_percentage = active_percentage
  ))
}

# Function to create detailed treatments table
create_treatments_table <- function(treatments_data, foremen_lookup) {
  if (nrow(treatments_data) == 0) {
    return(datatable(
      data.frame(Message = "No treatment data available"),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ))
  }
  
  # Add foreman names
  display_data <- treatments_data %>%
    left_join(foremen_lookup, by = c("fosarea" = "emp_num")) %>%
    select(
      Facility = facility_display,
      Zone = zone,
      Section = sectcode,
      Sitecode = sitecode,
      FOS = shortname,
      "Treatment Date" = inspdate,
      Material = material_name,
      "Acres Treated" = treated_acres,
      "Effect Days" = effect_days,
      Status = treatment_status,
      "Days Since" = days_since_treatment,
      Inspector = inspector,
      Weather = weather,
      Notes = notes
    ) %>%
    arrange(desc(`Treatment Date`), Facility, Sitecode)
  
  # Round numeric columns
  numeric_cols <- c("Acres Treated", "Effect Days", "Days Since")
  display_data[numeric_cols] <- lapply(display_data[numeric_cols], function(x) round(as.numeric(x), 1))
  
  datatable(
    display_data,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      columnDefs = list(
        list(className = 'dt-center', targets = 4:12)
      )
    ),
    rownames = FALSE,
    filter = "top"
  )
}

# Function to create treatment plans table
create_plans_table <- function(plans_data, foremen_lookup) {
  if (nrow(plans_data) == 0) {
    return(datatable(
      data.frame(Message = "No planning data available"),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ))
  }
  
  # Add foreman names
  display_data <- plans_data %>%
    left_join(foremen_lookup, by = c("fosarea" = "emp_num")) %>%
    select(
      "Plan ID" = plan_id,
      Facility = facility_display,
      Zone = zone,
      Section = sectcode,
      Sitecode = sitecode,
      FOS = shortname,
      "Planned Date" = planned_date,
      "Treatment Type" = treatment_type,
      Material = material_code,
      "Planned Acres" = planned_acres,
      Status = plan_status,
      Priority = priority,
      "Created Date" = created_date,
      "Created By" = created_by,
      Notes = notes
    ) %>%
    arrange(`Planned Date`, Facility, Sitecode)
  
  # Round numeric columns
  display_data$`Planned Acres` <- round(as.numeric(display_data$`Planned Acres`), 1)
  
  datatable(
    display_data,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      columnDefs = list(
        list(className = 'dt-center', targets = 5:13)
      )
    ),
    rownames = FALSE,
    filter = "top"
  )
}

# Function to create cattail treatments map
create_cattail_map <- function(spatial_data, treatments_data, basemap = "carto") {
  
  # Robust null/empty data checking
  if (is.null(spatial_data) || !inherits(spatial_data, "sf") || nrow(spatial_data) == 0) {
    return(leaflet() %>%
           addTiles() %>%
           setView(lng = -93.5, lat = 44.95, zoom = 10) %>%
           addPopups(lng = -93.5, lat = 44.95, 
                    popup = "No spatial data available for mapping"))
  }
  
  # Filter out rows with invalid coordinates
  tryCatch({
    coords <- sf::st_coordinates(spatial_data)
    valid_rows <- !is.na(coords[,1]) & !is.na(coords[,2]) & 
                  is.finite(coords[,1]) & is.finite(coords[,2]) &
                  abs(coords[,1]) <= 180 & abs(coords[,2]) <= 90
    
    if (sum(valid_rows) == 0) {
      return(leaflet() %>%
             addTiles() %>%
             setView(lng = -93.5, lat = 44.95, zoom = 10) %>%
             addPopups(lng = -93.5, lat = 44.95, 
                      popup = "No sites with valid coordinates found"))
    }
    
    if (sum(valid_rows) < nrow(spatial_data)) {
      spatial_data <- spatial_data[valid_rows, ]
    }
  }, error = function(e) {
    return(leaflet() %>%
           addTiles() %>%
           setView(lng = -93.5, lat = 44.95, zoom = 10) %>%
           addPopups(lng = -93.5, lat = 44.95, 
                    popup = paste("Coordinate validation error:", e$message)))
  })
  
  # Add treatment status to spatial data
  if (nrow(treatments_data) > 0) {
    # Get most recent treatment per site
    recent_treatments <- treatments_data %>%
      group_by(sitecode) %>%
      arrange(desc(as.Date(inspdate))) %>%
      slice(1) %>%
      ungroup() %>%
      select(sitecode, treatment_status, inspdate, material_name, treated_acres)
    
    # Join with spatial data
    spatial_data <- spatial_data %>%
      left_join(st_drop_geometry(recent_treatments), by = "sitecode") %>%
      mutate(
        map_status = coalesce(treatment_status, "No Treatment"),
        last_treatment_date = as.Date(inspdate),
        last_material = material_name
      )
  } else {
    spatial_data <- spatial_data %>%
      mutate(
        map_status = "No Treatment",
        last_treatment_date = as.Date(NA),
        last_material = NA
      )
  }
  
  # Define colors for treatment status
  status_colors <- get_status_colors()
  status_color_map <- c(
    "Active" = unname(status_colors["active"]),
    "Recently Expired" = "#f39c12",
    "Expired" = unname(status_colors["expired"]),
    "Long Expired" = "#c0392b",
    "No Treatment" = unname(status_colors["unknown"])
  )
  
  # Create color palette
  pal <- colorFactor(
    palette = status_color_map,
    domain = c("Active", "Recently Expired", "Expired", "Long Expired", "No Treatment"),
    na.color = "#7f7f7f"
  )
  
  # Create popup content
  spatial_data$popup_text <- sprintf(
    "<strong>%s</strong><br/>
    Facility: %s<br/>
    Zone: P%s<br/>
    Section: %s<br/>
    Acres: %.2f<br/>
    Status: %s<br/>
    %s",
    spatial_data$sitecode,
    spatial_data$facility_display,
    spatial_data$zone,
    spatial_data$sectcode,
    spatial_data$acres,
    spatial_data$map_status,
    ifelse(is.na(spatial_data$last_treatment_date), 
           "No treatment recorded",
           sprintf("Last treated: %s<br/>Material: %s<br/>Acres: %.1f", 
                  spatial_data$last_treatment_date, 
                  spatial_data$last_material,
                  spatial_data$treated_acres))
  )
  
  # Choose base tiles
  if (basemap == "satellite") {
    tiles <- "Esri.WorldImagery"
  } else if (basemap == "terrain") {
    tiles <- "Esri.WorldTopoMap"
  } else if (basemap == "osm") {
    tiles <- "OpenStreetMap"
  } else {
    tiles <- "CartoDB.Positron"  # carto
  }
  
  # Create map
  coords <- sf::st_coordinates(spatial_data)
  map <- leaflet(spatial_data) %>%
    addProviderTiles(tiles) %>%
    addCircleMarkers(
      radius = 8,
      color = "#000000",
      weight = 1,
      opacity = 0.8,
      fillColor = ~pal(map_status),
      fillOpacity = 0.8,
      popup = ~popup_text
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~map_status,
      title = "Treatment Status",
      opacity = 0.8
    )
  
  # Fit bounds if we have coordinates
  if (nrow(coords) > 0) {
    map <- map %>%
      fitBounds(
        lng1 = min(coords[,1]) - 0.01, lat1 = min(coords[,2]) - 0.01,
        lng2 = max(coords[,1]) + 0.01, lat2 = max(coords[,2]) + 0.01
      )
  }
  
  return(map)
}

# Function to prepare download data
prepare_cattail_download_data <- function(treatments_data, plans_data, sites_data, foremen_lookup) {
  
  # Combine all data for comprehensive download
  download_data <- list()
  
  # Sites data
  if (nrow(sites_data) > 0) {
    sites_df <- sites_data
    if ("sf" %in% class(sites_data)) {
      sites_df <- st_drop_geometry(sites_data)
    }
    
    download_data$sites <- sites_df %>%
      left_join(foremen_lookup, by = c("fosarea" = "emp_num")) %>%
      select(
        Sitecode = sitecode,
        Facility = facility_display,
        Zone = zone,
        Section = sectcode,
        FOS = shortname,
        Acres = acres,
        Priority = priority,
        Active = active
      )
  }
  
  # Treatments data
  if (nrow(treatments_data) > 0) {
    download_data$treatments <- treatments_data %>%
      left_join(foremen_lookup, by = c("fosarea" = "emp_num")) %>%
      select(
        Sitecode = sitecode,
        Facility = facility_display,
        Zone = zone,
        Section = sectcode,
        FOS = shortname,
        "Treatment Date" = inspdate,
        Material = material_name,
        "Material Code" = matcode,
        "Acres Treated" = treated_acres,
        "Effect Days" = effect_days,
        Status = treatment_status,
        Inspector = inspector,
        Weather = weather,
        "Wind Speed" = wind_speed,
        "Wind Direction" = wind_direction,
        Temperature = temperature,
        Humidity = humidity,
        Notes = notes
      )
  }
  
  # Plans data
  if (nrow(plans_data) > 0) {
    download_data$plans <- plans_data %>%
      left_join(foremen_lookup, by = c("fosarea" = "emp_num")) %>%
      select(
        "Plan ID" = plan_id,
        Sitecode = sitecode,
        Facility = facility_display,
        Zone = zone,
        Section = sectcode,
        FOS = shortname,
        "Planned Date" = planned_date,
        "Treatment Type" = treatment_type,
        "Material Code" = material_code,
        "Planned Acres" = planned_acres,
        Status = status,
        Priority = priority,
        "Created Date" = created_date,
        "Created By" = created_by,
        Notes = notes
      )
  }
  
  return(download_data)
}