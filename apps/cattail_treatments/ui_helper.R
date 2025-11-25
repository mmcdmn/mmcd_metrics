# Cattail Treatments - UI Helper Functions
# Helper functions for creating UI components 

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)

# Function to create analysis date selector 
create_analysis_date_selector <- function() {
  dateInput("analysis_date", "Pretend today is:",
            value = Sys.Date(),
            max = Sys.Date(),
            format = "yyyy-mm-dd",
            width = "100%")
}

# Function to create grouping selector 
create_grouping_selector <- function() {
  radioButtons("group_by", "Group By:",
               choices = list(
                 "All MMCD" = "mmcd_all",
                 "FOS" = "foreman", 
                 "Facility" = "facility"
               ),
               selected = "facility",
               inline = FALSE)
}

# Function to create zone display selector 
create_zone_display_selector <- function() {
  radioButtons("zone_display", "Zone Display:",
               choices = list(
                 "P1" = "p1",
                 "P2" = "p2",
                 "P1 and P2 Separate" = "separate", 
                 "P1 and P2 Combined" = "combined"
               ),
               selected = "separate",
               inline = FALSE)
}

# Function to create metric toggle 
create_metric_toggle <- function() {
  radioButtons("display_metric", "Toggle Between:",
               choices = list(
                 "Number of Sites" = "sites",
                 "Acres" = "acres"
               ),
               selected = "sites",
               inline = FALSE)
}

# Function to create facility filter UI 
create_facility_filter <- function() {
  conditionalPanel(
    condition = "input.group_by == 'facility' || input.group_by == 'mmcd_all'",
    selectizeInput("facility_filter", "Facility:",
                   choices = c("All" = "all"),
                   selected = "all",
                   multiple = FALSE,
                   width = "100%")
  )
}

# Function to create foreman filter UI (populated dynamically from database)
create_foreman_filter <- function() {
  conditionalPanel(
    condition = "input.group_by == 'foreman' || input.group_by == 'mmcd_all'",
    selectizeInput("foreman_filter", "FOS:",
                   choices = c("All" = "all"),
                   selected = "all",
                   multiple = FALSE,
                   width = "100%")
  )
}

# Function to create filter panel (matching ground prehatch style)
create_filter_panel <- function() {
  fluidRow(
    box(
      title = "Analysis Settings", 
      status = "primary", 
      solidHeader = TRUE, 
      width = 12, 
      collapsible = TRUE,
      
      fluidRow(
        column(2, create_analysis_date_selector()),
        column(2, create_grouping_selector()),
        column(2, create_zone_display_selector()),
        column(2, create_facility_filter()),
        column(2, create_progress_chart_type_selector())
      ),
      
      fluidRow(
        column(12, 
          div(style = "text-align: center; margin-top: 10px;",
            create_refresh_button()
          )
        )
      )
    )
  )
}

# Function to create refresh button
create_refresh_button <- function() {
  actionButton("refresh_data", "Refresh Data", 
               class = "btn-primary btn-block",
               icon = icon("refresh"))
}

# Function to create progress chart type selector (for Progress tab)
create_progress_chart_type_selector <- function() {
  selectInput("progress_chart_type", "Progress Chart Type:",
              choices = list(
                "Line Chart" = "line",
                "Grouped Bar Chart" = "bar",
                "Stacked Bar Chart" = "stacked"
              ),
              selected = "stacked",
              width = "100%")
}

# Function to create download button
create_download_button <- function(output_id = "download_data", label = "Download Data") {
  downloadButton(output_id, label,
                 class = "btn-success",
                 icon = icon("download"))
}

# Historical analysis helper functions 
create_time_period_selector <- function() {
  selectInput("time_period", "Time Period:",
              choices = list(
                "Weekly" = "weekly",
                "Yearly" = "yearly"
              ),
              selected = "yearly",
              width = "100%")
}

create_chart_type_selector <- function() {
  selectInput("chart_type", "Chart Type:",
              choices = list(
                "Line Chart" = "line",
                "Stacked Bar" = "stacked_bar",
                "Grouped Bar" = "grouped_bar"
              ),
              selected = "line",
              width = "100%")
}

create_year_range_selector <- function() {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  list(
    numericInput("start_year", "Start Year:",
                 value = current_year - 2,
                 min = 2020,
                 max = current_year,
                 step = 1,
                 width = "100%"),
    numericInput("end_year", "End Year:",
                 value = current_year,
                 min = 2020,
                 max = current_year,
                 step = 1,
                 width = "100%")
  )
}

# Function to create value boxes with metric awareness
create_cattail_value_boxes <- function(aggregated_data, treatments_data = NULL, plans_data = NULL) {
  if (nrow(aggregated_data) == 0) {
    return(list(
      total_sites = 0, total_acres = 0, total_treatments = 0,
      active_treatments = 0, treatment_coverage = 0, 
      acres_treated = 0
    ))
  }
  
  # Calculate totals
  total_sites <- sum(aggregated_data$total_sites, na.rm = TRUE)
  total_acres <- sum(aggregated_data$total_acres, na.rm = TRUE)
  total_treatments <- sum(aggregated_data$treatments_applied, na.rm = TRUE)
  active_treatments <- sum(aggregated_data$active_treatments, na.rm = TRUE)
  acres_treated <- sum(aggregated_data$acres_treated, na.rm = TRUE)
  
  # Calculate coverage percentage
  treatment_coverage <- if (total_sites > 0) {
    round((total_treatments / total_sites) * 100, 1)
  } else {
    0
  }
  
  return(list(
    total_sites = total_sites,
    total_acres = total_acres,
    total_treatments = total_treatments,
    active_treatments = active_treatments,
    treatment_coverage = treatment_coverage,
    acres_treated = acres_treated
  ))
}