# Inspections App - Display Functions

library(DT)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)

# Format inspection gaps data for display
format_inspection_gaps <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Format the data for display
  data %>%
    mutate(
      `Site Code` = sitecode,
      `Facility` = facility,
      `FOS Area` = fosarea,
      `Zone` = zone,
      `Air/Ground` = air_gnd,
      `Priority` = priority,
      `Drone Site` = case_when(
        is.na(drone) | drone != 'Y' ~ "No",
        drone == 'Y' ~ "Yes",
        TRUE ~ "No"
      ),
      `Last Inspection` = case_when(
        last_inspection_date == as.Date('1900-01-01') ~ "Never",
        is.na(last_inspection_date) ~ "Never",
        TRUE ~ format(last_inspection_date, "%Y-%m-%d")
      ),
      `Dip Count` = case_when(
        is.na(last_numdip) ~ "-",
        TRUE ~ as.character(last_numdip)
      ),
      `Days Since` = case_when(
        days_since_inspection == 999999 ~ "Never",
        TRUE ~ paste(format(days_since_inspection, big.mark=","), "days")
      ),
      `Status` = inspection_status
    ) %>%
    select(`Site Code`, `Facility`, `FOS Area`, `Zone`, `Air/Ground`, `Priority`, 
           `Drone Site`, `Last Inspection`, `Dip Count`, `Days Since`, `Status`)
}

# Render the inspection gap table
render_gap_table <- function(data) {
  formatted_data <- format_inspection_gaps(data)
  
  if (nrow(formatted_data) == 0) {
    return(DT::datatable(data.frame(Message = "No sites found with inspection gaps"), 
                        rownames = FALSE, options = list(dom = 't')))
  }
  
  DT::datatable(
    formatted_data,
    rownames = FALSE,
    options = list(
      pageLength = 25, 
      autoWidth = TRUE, 
      scrollX = TRUE,
      order = list(list(9, 'desc'))  # Sort by Days Since descending (column moved)
    ),
    filter = 'top',
    class = 'compact stripe hover'
  ) %>%
  DT::formatStyle(
    'Status',
    backgroundColor = DT::styleEqual(
      c('Never Inspected', 'Inspection Gap'),
      c('#ffebee', '#fff3e0')
    )
  ) %>%
  DT::formatStyle(
    'Drone Site',
    backgroundColor = DT::styleEqual(
      'Yes',
      '#e8f5e8'  # Light green for drone sites
    )
  )
}

# Format wet frequency data for display
format_wet_frequency_data <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  data %>%
    mutate(
      `Site Code` = sitecode,
      `Facility` = facility,
      `FOS Area` = fosarea,
      `Zone` = zone,
      `Air/Ground` = air_gnd,
      `Priority` = priority,
      `Acres` = format(acres, digits = 2, nsmall = 1),
      `Days Active` = format(days_active, big.mark = ","),
      `Total Inspections` = format(total_inspections, big.mark = ","),
      `Wet Count` = format(wet_count, big.mark = ","),
      `Water Present` = paste0(wet_percentage, "%"),
      `Flooded Count` = format(flooded_count, big.mark = ","),
      `Flooded %` = paste0(flooded_percentage, "%")
    ) %>%
    select(`Site Code`, `Facility`, `FOS Area`, `Zone`, `Air/Ground`, `Priority`, 
           `Acres`, `Days Active`, `Total Inspections`, `Wet Count`, `Water Present`, `Flooded Count`, `Flooded %`)
}

# Render wet frequency table
render_wet_frequency_table <- function(data) {
  formatted_data <- format_wet_frequency_data(data)
  
  if (nrow(formatted_data) == 0) {
    return(DT::datatable(data.frame(Message = "No sites found with minimum inspection threshold"), 
                        rownames = FALSE, options = list(dom = 't')))
  }
  
  DT::datatable(
    formatted_data,
    rownames = FALSE,
    options = list(
      pageLength = 25, 
      autoWidth = TRUE, 
      scrollX = TRUE,
      order = list(list(8, 'desc'))  # Sort by Water Present descending
    ),
    filter = 'top',
    class = 'compact stripe hover'
  ) %>%
  DT::formatStyle(
    'Water Present',
    background = DT::styleColorBar(c(0, 100), 'lightblue'),
    backgroundSize = '98% 88%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) %>%
  DT::formatStyle(
    'Flooded %',
    background = DT::styleColorBar(c(0, 100), 'orange'),
    backgroundSize = '98% 88%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  )
}

# Format high larvae sites data for display
format_high_larvae_data <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  data %>%
    mutate(
      `Site Code` = sitecode,
      `Facility` = facility,
      `FOS Area` = fosarea,
      `Zone` = zone,
      `Air/Ground` = air_gnd,
      `Priority` = priority,
      `Acres` = format(acres, digits = 2, nsmall = 1),
      `Days Active` = format(days_active, big.mark = ","),
      `Total Inspections` = total_inspections,
      `Exceedances` = threshold_exceedances,
      `Exceedance Frequency` = paste0(exceedance_frequency, "%"),
      `Max Dip Count` = format(max_dip_count, big.mark = ","),
      `Avg Dip Count` = avg_dip_count,
      `First High Date` = case_when(
        is.na(first_high_date) | is.infinite(first_high_date) ~ "-",
        TRUE ~ format(as.Date(first_high_date), "%Y-%m-%d")
      ),
      `Last High Date` = case_when(
        is.na(last_high_date) | is.infinite(last_high_date) ~ "-",
        TRUE ~ format(as.Date(last_high_date), "%Y-%m-%d")
      )
    ) %>%
    select(`Site Code`, `Facility`, `FOS Area`, `Zone`, `Air/Ground`, `Priority`, 
           `Acres`, `Days Active`, `Total Inspections`, `Exceedances`, `Exceedance Frequency`, 
           `Max Dip Count`, `Avg Dip Count`, `First High Date`, `Last High Date`)
}

# Render high larvae sites table
render_high_larvae_table <- function(data) {
  formatted_data <- format_high_larvae_data(data)
  
  if (nrow(formatted_data) == 0) {
    return(DT::datatable(data.frame(Message = "No sites found above larvae threshold"), 
                        rownames = FALSE, options = list(dom = 't')))
  }
  
  DT::datatable(
    formatted_data,
    rownames = FALSE,
    options = list(
      pageLength = 25, 
      autoWidth = TRUE, 
      scrollX = TRUE,
      order = list(list(8, 'desc'))  # Sort by Exceedance Frequency descending
    ),
    filter = 'top',
    class = 'compact stripe hover'
  ) %>%
  DT::formatStyle(
    'Exceedance Frequency',
    background = DT::styleColorBar(c(0, 100), 'lightcoral'),
    backgroundSize = '98% 88%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) %>%
  DT::formatStyle(
    'Max Dip Count',
    background = DT::styleColorBar(range(as.numeric(gsub(",", "", data$max_dip_count)), na.rm = TRUE), 'lightyellow'),
    backgroundSize = '98% 88%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  )
}

# =============================================================================
# VISUALIZATION FUNCTIONS
# =============================================================================

# Create wet frequency distribution chart
create_wet_frequency_chart <- function(data) {
  if (nrow(data) == 0) return(NULL)
  
  # Create frequency bins using the correct column name 'wet_percentage' from raw data
  wet_freq_bins <- cut(data$wet_percentage, 
                      breaks = c(0, 10, 25, 50, 75, 90, 100),
                      labels = c("0-10%", "10-25%", "25-50%", "50-75%", "75-90%", "90-100%"),
                      include.lowest = TRUE)
  
  freq_counts <- table(wet_freq_bins)
  
  p <- plot_ly(
    x = names(freq_counts),
    y = as.numeric(freq_counts),
    type = 'bar',
    marker = list(color = 'lightblue', line = list(color = 'darkblue', width = 1))
  ) %>%
  layout(
    title = list(text = "Distribution of Wet Frequencies", font = list(size = 14)),
    xaxis = list(title = "Wet Frequency Range"),
    yaxis = list(title = "Number of Sites"),
    margin = list(l = 50, r = 30, t = 50, b = 50)
  )
  
  return(p)
}

# Create priority distribution chart
create_priority_chart <- function(data) {
  if (nrow(data) == 0) return(NULL)
  
  priority_counts <- data %>%
    count(priority, name = "count") %>%
    mutate(priority = ifelse(is.na(priority), "Unassigned", priority))
  
  colors <- c("RED" = "#d32f2f", "YELLOW" = "#f57c00", "GREEN" = "#388e3c", 
              "BLUE" = "#1976d2", "Unassigned" = "#757575")
  
  p <- plot_ly(
    priority_counts,
    labels = ~priority,
    values = ~count,
    type = 'pie',
    marker = list(colors = colors[priority_counts$priority])
  ) %>%
  layout(
    title = list(text = "Sites by Priority Level", font = list(size = 14)),
    margin = list(l = 20, r = 20, t = 50, b = 20)
  )
  
  return(p)
}

# Create exceedance frequency distribution chart
create_exceedance_frequency_chart <- function(data) {
  if (nrow(data) == 0) return(NULL)
  
  # Create frequency bins for exceedance rates
  freq_bins <- cut(data$exceedance_frequency,
                  breaks = c(0, 5, 15, 30, 50, 75, 100),
                  labels = c("0-5%", "5-15%", "15-30%", "30-50%", "50-75%", "75-100%"),
                  include.lowest = TRUE)
  
  bin_counts <- table(freq_bins)
  
  p <- plot_ly(
    x = names(bin_counts),
    y = as.numeric(bin_counts),
    type = 'bar',
    marker = list(color = 'lightcoral', line = list(color = 'darkred', width = 1))
  ) %>%
  layout(
    title = list(text = "Distribution of Exceedance Frequencies", font = list(size = 14)),
    xaxis = list(title = "Exceedance Frequency Range"),
    yaxis = list(title = "Number of Sites"),
    margin = list(l = 50, r = 30, t = 50, b = 50)
  )
  
  return(p)
}

# Create larvae count distribution chart
create_larvae_distribution_chart <- function(data) {
  if (nrow(data) == 0) return(NULL)
  
  # Create bins for average dip count with ordered levels
  avg_bins <- cut(data$avg_dip_count, 
                 breaks = c(0, 1, 2, 5, 10, 20, Inf),
                 labels = c("0-1", "1-2", "2-5", "5-10", "10-20", "20+"),
                 include.lowest = TRUE)
  
  freq_counts <- table(avg_bins)
  
  # Ensure proper ordering of factor levels
  ordered_labels <- c("0-1", "1-2", "2-5", "5-10", "10-20", "20+")
  freq_df <- data.frame(
    bin = factor(names(freq_counts), levels = ordered_labels),
    count = as.numeric(freq_counts)
  )
  
  p <- plot_ly(
    data = freq_df,
    x = ~bin,
    y = ~count,
    type = 'bar',
    marker = list(color = 'orange', line = list(color = 'darkorange', width = 1))
  ) %>%
  layout(
    title = list(text = "Average Dip Count Distribution", font = list(size = 14)),
    xaxis = list(title = "Average Dip Count Range", categoryorder = "array", categoryarray = ordered_labels),
    yaxis = list(title = "Number of Sites"),
    margin = list(l = 50, r = 30, t = 50, b = 50)
  )
  
  return(p)
}

# Create gap distribution chart
create_gap_distribution_chart <- function(data) {
  if (nrow(data) == 0) return(NULL)
  
  # Create bins for days since last inspection
  gap_bins <- cut(data$days_since_inspection,
                 breaks = c(0, 365, 730, 1095, 1460, 2555, 999999),
                 labels = c("<1 year", "1-2 years", "2-3 years", "3-4 years", "4-7 years", "Never"),
                 include.lowest = TRUE)
  
  gap_counts <- table(gap_bins)
  
  colors <- c("<1 year" = "#4caf50", "1-2 years" = "#8bc34a", "2-3 years" = "#ffeb3b", 
              "3-4 years" = "#ff9800", "4-7 years" = "#f44336", "Never" = "#9e9e9e")
  
  p <- plot_ly(
    x = names(gap_counts),
    y = as.numeric(gap_counts),
    type = 'bar',
    marker = list(color = colors[names(gap_counts)], line = list(color = 'black', width = 1))
  ) %>%
  layout(
    title = list(text = "Gap Distribution by Time", font = list(size = 14)),
    xaxis = list(title = "Time Since Last Inspection"),
    yaxis = list(title = "Number of Sites"),
    margin = list(l = 50, r = 30, t = 50, b = 50)
  )
  
  return(p)
}

# Create facility gap chart
create_facility_gap_chart <- function(data) {
  if (nrow(data) == 0) return(NULL)
  
  # Summarize gaps by facility
  facility_summary <- data %>%
    count(facility, inspection_status) %>%
    tidyr::pivot_wider(names_from = inspection_status, values_from = n, values_fill = 0)
  
  # Ensure required columns exist
  if (!"Never Inspected" %in% names(facility_summary)) {
    facility_summary$`Never Inspected` <- 0
  }
  if (!"Inspection Gap" %in% names(facility_summary)) {
    facility_summary$`Inspection Gap` <- 0
  }
  
  # Create stacked bar chart
  p <- plot_ly(facility_summary) %>%
    add_trace(y = ~facility, x = ~`Never Inspected`, name = "Never Inspected", 
              type = 'bar', orientation = 'h', marker = list(color = '#f44336')) %>%
    add_trace(y = ~facility, x = ~`Inspection Gap`, name = "Inspection Gap", 
              type = 'bar', orientation = 'h', marker = list(color = '#ff9800')) %>%
    layout(
      title = list(text = "Gap Status by Facility", font = list(size = 14)),
      xaxis = list(title = "Number of Sites"),
      yaxis = list(title = "Facility"),
      barmode = 'stack',
      margin = list(l = 80, r = 30, t = 50, b = 50)
    )
  
  return(p)
}