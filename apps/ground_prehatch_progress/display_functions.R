# Ground Prehatch Progress - Display Functions
# Functions for creating charts, tables, and value boxes

# Function to create progress chart
create_progress_chart <- function(data, group_by, show_expiring_only = FALSE, expiring_days = 14) {
  if (nrow(data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No data available"), size = 6) +
           theme_void())
  }
  
  # Get status colors from db_helpers
  status_colors <- get_status_colors()
  
  # Prepare data for stacked bar chart
  chart_data <- data %>%
    select(display_name, ph_treated_cnt, ph_expiring_cnt, ph_expired_cnt, ph_notactivetrt_cnt) %>%
    tidyr::pivot_longer(cols = -display_name, names_to = "status", values_to = "count") %>%
    mutate(
      status_label = case_when(
        status == "ph_treated_cnt" ~ "Treated",
        status == "ph_expiring_cnt" ~ "Expiring",
        status == "ph_expired_cnt" ~ "Expired",
        status == "ph_notactivetrt_cnt" ~ "Needs Treatment"
      ),
      color = case_when(
        status == "ph_treated_cnt" ~ unname(status_colors["active"]),
        status == "ph_expiring_cnt" ~ unname(status_colors["planned"]),
        status == "ph_expired_cnt" ~ unname(status_colors["unknown"]),
        status == "ph_notactivetrt_cnt" ~ unname(status_colors["needs_treatment"])
      )
    )

  # If showing expiring only, filter chart data to only show expiring status
  if (show_expiring_only) {
    chart_data <- chart_data %>% filter(status == "ph_expiring_cnt")
  }

  # Create title based on filters
  chart_title <- if (show_expiring_only) {
    paste("Ground Prehatch Expiring Sites (Within", expiring_days, "Days)")
  } else {
    "Ground Prehatch Treatment Progress"
  }
  
  p <- ggplot(chart_data, aes(x = reorder(display_name, count), y = count, fill = status_label)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = setNames(chart_data$color, chart_data$status_label)) +
    coord_flip() +
    labs(
      title = chart_title,
      x = case_when(
        group_by == "mmcd_all" ~ "MMCD",
        group_by == "facility" ~ "Facility",
        group_by == "foreman" ~ "FOS",
        group_by == "sectcode" ~ "Section"
      ),
      y = "Number of Sites",
      fill = "Treatment Status"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      axis.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 13),
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11)
    )
  
  return(ggplotly(p, tooltip = c("x", "y", "fill")))
}

# Function to create value boxes
create_value_box <- function(value, subtitle, icon, color) {
  valueBox(
    value = value,
    subtitle = subtitle,
    icon = icon(icon),
    color = color
  )
}

# Function to create all value boxes
create_value_boxes <- function(data) {
  if (nrow(data) == 0) {
    return(list(
      total_sites = create_value_box(0, "Total Ground Sites", "map-marker", "blue"),
      prehatch_sites = create_value_box(0, "Prehatch Sites", "bug", "green"),
      treated_sites = create_value_box(0, "Treated Sites", "check-circle", "green"),
      needs_treatment = create_value_box(0, "Needs Treatment", "exclamation-triangle", "red"),
      treated_pct = create_value_box("0%", "Treated %", "percent", "green"),
      expiring_pct = create_value_box("0%", "Expiring %", "clock", "yellow")
    ))
  }
  
  # Calculate totals
  total_ground <- sum(data$tot_ground, na.rm = TRUE)
  total_prehatch <- sum(data$prehatch_sites_cnt, na.rm = TRUE)
  total_treated <- sum(data$ph_treated_cnt, na.rm = TRUE)
  total_needs_treatment <- sum(data$ph_notactivetrt_cnt, na.rm = TRUE)
  total_expiring <- sum(data$ph_expiring_cnt, na.rm = TRUE)
  
  # Calculate percentages
  treated_pct <- if (total_prehatch > 0) round(100 * total_treated / total_prehatch, 1) else 0
  expiring_pct <- if (total_prehatch > 0) round(100 * total_expiring / total_prehatch, 1) else 0
  
  return(list(
    total_sites = create_value_box(total_ground, "Total Ground Sites", "map-marker", "blue"),
    prehatch_sites = create_value_box(total_prehatch, "Prehatch Sites", "bug", "green"),
    treated_sites = create_value_box(total_treated, "Treated Sites", "check-circle", "green"),
    needs_treatment = create_value_box(total_needs_treatment, "Needs Treatment", "exclamation-triangle", "red"),
    treated_pct = create_value_box(paste0(treated_pct, "%"), "Treated %", "percent", "green"),
    expiring_pct = create_value_box(paste0(expiring_pct, "%"), "Expiring %", "clock", "yellow")
  ))
}

# Function to create details table
create_details_table <- function(data, foremen_lookup) {
  if (nrow(data) == 0) {
    return(datatable(
      data.frame(Message = "No data available with current filters"),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ))
  }
  
  # Map fosarea (empnum) to foreman names
  data$foreman_name <- NA
  for (i in 1:nrow(data)) {
    if (!is.na(data$fosarea[i])) {
      foreman_row <- foremen_lookup[foremen_lookup$emp_num == data$fosarea[i], ]
      if (nrow(foreman_row) > 0) {
        data$foreman_name[i] <- foreman_row$shortname[1]
      } else {
        data$foreman_name[i] <- data$fosarea[i]  # Fallback to empnum
      }
    }
  }
  
  # Format data for display
  display_data <- data %>%
    select(
      Facility = facility_display,
      `Priority Zone` = zone,
      Section = sectcode,
      Sitecode = sitecode,
      FOS = foreman_name,
      Acres = acres,
      Priority = priority,
      `Treatment Type` = prehatch,
      Status = prehatch_status,
      `Last Treatment` = inspdate,
      `Days Since Last Treatment` = age,
      Material = matcode,
      `Effect Days` = effect_days
    ) %>%
    arrange(Facility, Section, Sitecode)
  
  # Round numeric columns
  if ("Days Since Last Treatment" %in% names(display_data)) {
    display_data$`Days Since Last Treatment` <- round(as.numeric(display_data$`Days Since Last Treatment`), 1)
  }
  if ("Acres" %in% names(display_data)) {
    display_data$Acres <- round(as.numeric(display_data$Acres), 2)
  }
  
  datatable(
    display_data,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      columnDefs = list(
        list(className = 'dt-center', targets = 5:12)
      )
    ),
    rownames = FALSE
  )
}

# Function to prepare download data
prepare_download_data <- function(data, foremen_lookup) {
  if (nrow(data) == 0) {
    return(data.frame(Message = "No data available with current filters"))
  }
  
  # Map fosarea (empnum) to foreman names
  data$foreman_name <- NA
  for (i in 1:nrow(data)) {
    if (!is.na(data$fosarea[i])) {
      foreman_row <- foremen_lookup[foremen_lookup$emp_num == data$fosarea[i], ]
      if (nrow(foreman_row) > 0) {
        data$foreman_name[i] <- foreman_row$shortname[1]
      } else {
        data$foreman_name[i] <- data$fosarea[i]  # Fallback to empnum
      }
    }
  }
  
  # Format for download (same as displayed data)
  download_data <- data %>%
    select(
      Facility = facility_display,
      `Priority Zone` = zone,
      Section = sectcode,
      Sitecode = sitecode,
      FOS = foreman_name,
      Acres = acres,
      Priority = priority,
      `Treatment Type` = prehatch,
      Status = prehatch_status,
      `Last Treatment` = inspdate,
      `Days Since Last Treatment` = age,
      Material = matcode,
      `Effect Days` = effect_days
    ) %>%
    arrange(Facility, Section, Sitecode)
  
  # Round numeric columns
  if ("Days Since Last Treatment" %in% names(download_data)) {
    download_data$`Days Since Last Treatment` <- round(as.numeric(download_data$`Days Since Last Treatment`), 1)
  }
  if ("Acres" %in% names(download_data)) {
    download_data$Acres <- round(as.numeric(download_data$Acres), 2)
  }
  
  return(download_data)
}