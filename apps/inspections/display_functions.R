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
  result <- data %>%
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
      `Days Since` = days_since_inspection,  # Keep numeric for coloring
      `Status` = inspection_status
    ) %>%
    select(`Site Code`, `Facility`, `FOS Area`, `Zone`, `Air/Ground`, `Priority`, 
           `Drone Site`, `Last Inspection`, `Dip Count`, `Days Since`, `Status`)
  
  # Link sitecodes to data.mmcd.org map
  result$`Site Code` <- make_sitecode_link(result$`Site Code`)
  result
}

# Render the inspection gap table
render_gap_table <- function(data, theme = "MMCD") {
  formatted_data <- format_inspection_gaps(data)
  
  if (nrow(formatted_data) == 0) {
    return(DT::datatable(data.frame(Message = "No sites found with inspection gaps"), 
                        rownames = FALSE, options = list(dom = 't')))
  }
  
  # Get status colors from db_helpers for consistent coloring
  status_colors <- get_status_colors(theme = theme)

  DT::datatable(
    formatted_data,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      pageLength = 25, 
      autoWidth = TRUE, 
      scrollX = TRUE,
      order = list(list(9, 'desc')),  # Sort by Days Since descending 
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().container()).find('table').css('font-size', '14px');",
        "$(this.api().table().container()).find('thead').css('font-weight', 'bold');",
        "}"
      )
    ),
    filter = 'top',
    class = 'compact stripe hover'
  ) %>%
  DT::formatStyle(
    'Status',
    backgroundColor = DT::styleEqual(
      c('Never Inspected', 'Inspection Gap'),
      c('#ffebee', status_colors['planned'])  # Use status orange for planned/pending
    )
  ) %>%
  DT::formatStyle(
    'Drone Site',
    backgroundColor = DT::styleEqual(
      'Yes',
      '#e8f5e8'  # Light green for drone sites
    )
  ) %>%
  DT::formatStyle(
    'Days Since',
    backgroundColor = DT::styleInterval(
      cuts = c(365, 730, 1095),  # 3 cuts
      values = c("#e8f5e8", "#ffeb3b", "#ff9800", "#f44336")  # 4 values - green to yellow to orange to red
    )
  ) %>%
  DT::formatCurrency('Days Since', currency = "", digits = 0, mark = ",") %>%
  DT::formatString('Days Since', suffix = " days")
}

# Format wet frequency data for display
format_wet_frequency_data <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  result <- data %>%
    mutate(
      `Site Code` = sitecode,
      `Facility` = facility,
      `FOS Area` = fosarea,
      `Zone` = zone,
      `Air/Ground` = air_gnd,
      `Priority` = priority,
      `Acres` = format(acres, digits = 2, nsmall = 1),
      `Years with Data` = years_with_data,
      `Total Inspections` = format(total_inspections, big.mark = ","),
      `Wet Count` = format(wet_count, big.mark = ","),
      `Water Present` = wet_percentage / 100,  # Convert to decimal for formatPercentage
      `Flooded Count` = format(flooded_count, big.mark = ","),
      `Flooded %` = flooded_percentage / 100   # Convert to decimal for formatPercentage
    ) %>%
    select(`Site Code`, `Facility`, `FOS Area`, `Zone`, `Air/Ground`, `Priority`, 
           `Acres`, `Years with Data`, `Total Inspections`, `Wet Count`, `Water Present`, `Flooded Count`, `Flooded %`)
  
  # Link sitecodes to data.mmcd.org map
  result$`Site Code` <- make_sitecode_link(result$`Site Code`)
  result
}

# Render wet frequency table
render_wet_frequency_table <- function(data, theme = "MMCD") {
  formatted_data <- format_wet_frequency_data(data)
  
  if (nrow(formatted_data) == 0) {
    return(DT::datatable(data.frame(Message = "No sites found with minimum inspection threshold"), 
                        rownames = FALSE, options = list(dom = 't')))
  }
  
  # Get status colors from db_helpers for consistent coloring
  status_colors <- get_status_colors(theme = theme)
  
  DT::datatable(
    formatted_data,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      pageLength = 25, 
      autoWidth = TRUE, 
      scrollX = TRUE,
      order = list(list(8, 'desc')),  # Sort by Water Present descending
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().container()).find('table').css('font-size', '14px');",
        "$(this.api().table().container()).find('thead').css('font-weight', 'bold');",
        "}"
      )
    ),
    filter = 'top',
    class = 'compact stripe hover'
  ) %>%
  DT::formatStyle(
    'Water Present',
    backgroundColor = DT::styleInterval(
      cuts = c(0.10, 0.20, 0.40, 0.60, 0.80, 0.95),  # Decimal cuts for percentage values
      values = c("#f8f9fa", "#e3f2fd", "#bbdefb", "#64b5f6", "#42a5f5", "#2196f3", "#1e88e5")  # 7 values - light to dark blue
    )
  ) %>%
  DT::formatStyle(
    'Flooded %',
    backgroundColor = DT::styleInterval(
      cuts = c(0.05, 0.10, 0.20, 0.40, 0.60, 0.80),  # Decimal cuts for percentage values
      values = c("#f8f9fa", "#e8f5e8", "#c8e6c8", "#81c784", "#66bb6a", "#4caf50", "#388e3c")  # 7 values - light to dark green
    )
  ) %>%
  DT::formatPercentage(c('Water Present', 'Flooded %'), digits = 1)
}

# Format high larvae sites data for display
format_high_larvae_data <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  result <- data %>%
    mutate(
      `Site Code` = sitecode,
      `Facility` = facility,
      `FOS Area` = fosarea,
      `Zone` = zone,
      `Air/Ground` = air_gnd,
      `Priority` = priority,
      `Acres` = format(acres, digits = 2, nsmall = 1),
      `Years with Data` = years_with_data,
      `Total Inspections` = total_inspections,
      `Exceedances` = threshold_exceedances,
      `Exceedance Frequency` = exceedance_frequency / 100,  # Convert to decimal for formatPercentage
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
           `Acres`, `Years with Data`, `Total Inspections`, `Exceedances`, `Exceedance Frequency`, 
           `Max Dip Count`, `Avg Dip Count`, `First High Date`, `Last High Date`)
  
  # Link sitecodes to data.mmcd.org map
  result$`Site Code` <- make_sitecode_link(result$`Site Code`)
  result
}

# Render high larvae table
render_high_larvae_table <- function(data, theme = "MMCD") {
  formatted_data <- format_high_larvae_data(data)
  
  if (nrow(formatted_data) == 0) {
    return(DT::datatable(data.frame(Message = "No sites found above larvae threshold"), 
                        rownames = FALSE, options = list(dom = 't')))
  }
  
  # Get status colors from db_helpers for consistent coloring
  status_colors <- get_status_colors(theme = theme)
  
  DT::datatable(
    formatted_data,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      pageLength = 25, 
      autoWidth = TRUE, 
      scrollX = TRUE,
      order = list(list(8, 'desc')),  # Sort by Exceedance Frequency descending
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().container()).find('table').css('font-size', '14px');",
        "$(this.api().table().container()).find('thead').css('font-weight', 'bold');",
        "}"
      )
    ),
    filter = 'top',
    class = 'compact stripe hover'
  ) %>%
  DT::formatStyle(
    'Exceedance Frequency',
    backgroundColor = DT::styleInterval(
      cuts = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90),  # Decimal cuts for percentage values
      values = c("#f3e5f5", "#e1bee7", "#ce93d8", "#ba68c8", "#ab47bc", "#9c27b0", "#f44336")  # 7 values - light to dark (purple to red)
    )
  ) %>%
  DT::formatStyle(
    'Avg Dip Count',
    backgroundColor = DT::styleInterval(
      cuts = c(1, 3, 6, 12, 25),  # 5 cuts
      values = c("#e3f2fd", "#bbdefb", "#90caf9", "#64b5f6", "#ff8a65", "#ff6f00")  # 6 values - light blue to orange
    )
  ) %>%
  DT::formatPercentage('Exceedance Frequency', digits = 1)
}

# =============================================================================
# VISUALIZATION FUNCTIONS
# =============================================================================

# Create wet frequency distribution chart
create_wet_frequency_chart <- function(data, theme = "MMCD") {
  if (nrow(data) == 0) return(NULL)
  
  # Create frequency bins using the correct column name 'wet_percentage' from raw data
  wet_freq_bins <- cut(data$wet_percentage, 
                      breaks = c(0, 10, 25, 50, 75, 90, 100),
                      labels = c("0-10%", "10-25%", "25-50%", "50-75%", "75-90%", "90-100%"),
                      include.lowest = TRUE)
  
  freq_counts <- table(wet_freq_bins)
  
  create_distribution_chart(
    x_values = names(freq_counts),
    y_values = as.numeric(freq_counts),
    title = "Distribution of Wet Frequencies",
    x_label = "Wet Frequency Range",
    bar_color = "lightblue",
    border_color = "darkblue"
  )
}

# Create priority distribution chart
create_priority_chart <- function(data, theme = "MMCD") {
  if (nrow(data) == 0) return(NULL)
  
  priority_counts <- data %>%
    count(priority, name = "count") %>%
    mutate(priority = ifelse(is.na(priority), "Unassigned", priority))
  
  # Get dynamic priority choices from db_helpers
  priority_choices <- get_priority_choices(include_all = FALSE)
  status_colors <- get_status_colors(theme = theme)
  
  # Create color mapping that actually matches priority names
  colors <- c(
    "RED" = "red",                    # Literal red color for RED priority
    "YELLOW" = "yellow",              # Literal yellow color for YELLOW priority  
    "GREEN" = "green",                # Literal green color for GREEN priority
    "BLUE" = "blue",                  # Literal blue color for BLUE priority
    "PURPLE" = "purple",              # Literal purple color for PURPLE priority
    "Unassigned" = "#888888"          # Gray color for unassigned
  )
  
  p <- plot_ly(
    priority_counts,
    labels = ~priority,
    values = ~count,
    type = 'pie',
    marker = list(colors = colors[priority_counts$priority]),
    textfont = list(size = 18, family = "Arial, sans-serif", color = "white"),
    textinfo = "label+percent",
    textposition = "inside"
  ) %>%
  layout(
    title = list(text = "Sites by Priority Level", font = list(size = 20, family = "Arial, sans-serif", color = "#333"), x = 0.5),
    margin = list(l = 20, r = 20, t = 100, b = 20),
    font = list(size = 18, family = "Arial, sans-serif"),
    legend = list(font = list(size = 18, family = "Arial, sans-serif")),
    textfont = list(size = 18, family = "Arial, sans-serif", color = "white")
  )
  
  return(p)
}

# Create exceedance frequency distribution chart
create_exceedance_frequency_chart <- function(data, theme = "MMCD") {
  if (nrow(data) == 0) return(NULL)
  
  # Create frequency bins for exceedance rates with ordered levels
  ordered_labels <- c("0-5%", "5-15%", "15-30%", "30-50%", "50-75%", "75-100%")
  freq_bins <- cut(data$exceedance_frequency,
                  breaks = c(0, 5, 15, 30, 50, 75, 100),
                  labels = ordered_labels,
                  include.lowest = TRUE)
  
  bin_counts <- table(freq_bins)
  freq_df <- data.frame(
    bin = factor(names(bin_counts), levels = ordered_labels),
    count = as.numeric(bin_counts)
  )
  
  create_distribution_chart(
    x_values = freq_df$bin,
    y_values = freq_df$count,
    title = "Distribution of Exceedance Frequencies",
    x_label = "Exceedance Frequency Range",
    bar_color = "lightcoral",
    border_color = "darkred"
  ) %>% layout(xaxis = list(categoryorder = "array", categoryarray = ordered_labels))
}

# Create larvae count distribution chart
create_larvae_distribution_chart <- function(data, theme = "MMCD") {
  if (nrow(data) == 0) return(NULL)
  
  # Create bins for average dip count with ordered levels
  ordered_labels <- c("0-1", "1-2", "2-5", "5-10", "10-20", "20+")
  avg_bins <- cut(data$avg_dip_count, 
                 breaks = c(0, 1, 2, 5, 10, 20, Inf),
                 labels = ordered_labels,
                 include.lowest = TRUE)
  
  freq_counts <- table(avg_bins)
  freq_df <- data.frame(
    bin = factor(names(freq_counts), levels = ordered_labels),
    count = as.numeric(freq_counts)
  )
  
  create_distribution_chart(
    x_values = freq_df$bin,
    y_values = freq_df$count,
    title = "Average Dip Count Distribution",
    x_label = "Average Dip Count Range",
    bar_color = "orange",
    border_color = "darkorange"
  ) %>% layout(xaxis = list(categoryorder = "array", categoryarray = ordered_labels))
}

# Create facility gap chart - Stacked percentage comparison by facility only
create_facility_gap_chart <- function(facility_analysis, theme = "MMCD") {
  if (nrow(facility_analysis) == 0) return(NULL)
  
  # Aggregate by facility only (sum across FOS areas)
  facility_summary <- facility_analysis %>%
    group_by(facility) %>%
    summarise(
      total_sites = sum(total_sites),
      gap_sites = sum(gap_sites),
      recently_inspected_sites = sum(recently_inspected_sites),
      .groups = 'drop'
    ) %>%
    mutate(
      gap_percentage = round(100 * gap_sites / total_sites, 1),
      recently_inspected_percentage = round(100 * recently_inspected_sites / total_sites, 1)
    )
  
  # Map facility codes to display names
  facility_summary <- map_facility_names(facility_summary)
  
  # Get themed status colors (green for active/good, red/orange for gaps/issues)
  status_colors <- get_status_colors(theme = theme)
  
  # Calculate overall percentage of green sites across all facilities
  overall_green_pct <- round(100 * sum(facility_summary$recently_inspected_sites) / sum(facility_summary$total_sites), 1)
  
  # Create the stacked percentage bar chart (each bar = 100%)
  p <- plot_ly(facility_summary) %>%
    add_trace(
      x = ~facility_display, 
      y = ~recently_inspected_percentage, 
      type = 'bar',
      name = "Recently Inspected",
      text = ~paste("Recently Inspected:", recently_inspected_percentage, "%<br>", 
                   recently_inspected_sites, " of ", total_sites, " sites"),
      hovertemplate = "%{text}<extra></extra>",
      marker = list(color = status_colors["active"], line = list(color = 'white', width = 1))
    ) %>%
    add_trace(
      x = ~facility_display, 
      y = ~gap_percentage,
      type = 'bar', 
      name = "Has Inspection Gap",
      text = ~paste("Has Gap:", gap_percentage, "%<br>", 
                   gap_sites, " of ", total_sites, " sites"),
      hovertemplate = "%{text}<extra></extra>",
      hoverlabel = list(bgcolor = "white", bordercolor = "black", font = list(size = 12)),
      marker = list(color = status_colors["needs_treatment"])
    ) %>%
    layout(
      title = list(text = "Inspection Gap Analysis by Facility", font = list(size = 24, family = "Arial, sans-serif", color = "#333"), x = 0.5),
      xaxis = list(
        title = list(text = "Facility", font = list(size = 18, family = "Arial, sans-serif", color = "#333")),
        tickangle = -45,
        categoryorder = "array",
        categoryarray = unique(facility_summary$facility_display),
        tickfont = list(size = 18, family = "Arial, sans-serif", color = "#333")
      ),
      yaxis = list(
        title = list(text = "Percentage of Total Sites in Category", font = list(size = 18, family = "Arial, sans-serif", color = "#333")),
        tickfont = list(size = 18, family = "Arial, sans-serif", color = "#333"),
        range = c(0, 115)  # Extend range to 115% to provide space for hover text above bars
      ),
      barmode = 'stack',
      margin = list(l = 80, r = 30, t = 80, b = 200),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.25,
        font = list(size = 18, family = "Arial, sans-serif")
      ),
      font = list(size = 18, family = "Arial, sans-serif"),
      annotations = list(
        text = paste0("Each bar shows percentage of green (recently inspected) sites in all facilities. Overall: ", overall_green_pct, "% of sites are recently inspected."),
        showarrow = FALSE,
        x = 0.5,
        y = -0.98,
        xref = "paper",
        yref = "paper",
        font = list(size = 16, color = "#666", family = "Arial, sans-serif")
      )
    )
  
  return(p)
}