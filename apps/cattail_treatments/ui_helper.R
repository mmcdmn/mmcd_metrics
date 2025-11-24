# Cattail Treatments - UI Helper Functions
# Helper functions for creating UI components (matching ground prehatch style)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)

# Function to create zone filter UI
create_zone_filter <- function() {
  radioButtons("zone_filter", "Zone Display:",
               choices = list(
                 "P1 Only" = "1",
                 "P2 Only" = "2",
                 "P1 and P2 Separate" = "1,2",
                 "Combined P1+P2" = "combined"
               ),
               selected = "1,2",
               inline = TRUE)
}

# Function to create section filter UI
create_section_filter <- function() {
  selectizeInput("section_filter", "Section:",
                 choices = c("All" = "all"),
                 selected = "all",
                 multiple = TRUE,
                 options = list(placeholder = "Select sections..."))
}

# Function to create treatment type filter UI
create_treatment_type_filter <- function() {
  selectizeInput("treatment_type_filter", "Treatment Type:",
                 choices = c("All" = "all"),
                 selected = "all",
                 multiple = TRUE,
                 options = list(placeholder = "Select treatment types..."))
}

# Function to create status filter UI
create_status_filter <- function() {
  selectizeInput("status_filter", "Status:",
                 choices = c("All" = "all"),
                 selected = "all",
                 multiple = TRUE,
                 options = list(placeholder = "Select status..."))
}

# Function to create date range filter UI
create_date_range_filter <- function() {
  list(
    dateInput("start_date", "Start Date:",
              value = Sys.Date() - 90,
              format = "yyyy-mm-dd"),
    dateInput("end_date", "End Date:",
              value = Sys.Date(),
              format = "yyyy-mm-dd")
  )
}

# Function to create facility filter UI
create_facility_filter <- function() {
  conditionalPanel(
    condition = "input.group_by == 'facility' || input.group_by == 'mmcd_all'",
    checkboxGroupInput("facility_filter", "Facility:",
                       choices = list(),
                       selected = list(),
                       inline = TRUE)
  )
}

# Function to create foreman filter UI
create_foreman_filter <- function() {
  conditionalPanel(
    condition = "input.group_by == 'foreman' || input.group_by == 'mmcd_all'",
    checkboxGroupInput("foreman_filter", "FOS:",
                       choices = list(),
                       selected = list(),
                       inline = TRUE)
  )
}

# Function to create grouping selector UI (matching ground prehatch)
create_grouping_selector <- function() {
  radioButtons("group_by", "Group By:",
               choices = list(
                 "MMCD Total" = "mmcd_all",
                 "FOS" = "foreman", 
                 "Facility" = "facility"
               ),
               selected = "facility",
               inline = TRUE)
}

# Function to create zone display selector
create_zone_display_selector <- function() {
  radioButtons("zone_display", "Zone Display:",
               choices = list(
                 "P1 Only" = "p1",
                 "P2 Only" = "p2",
                 "P1 and P2 Separate" = "separate",
                 "P1 and P2 Combined" = "combined"
               ),
               selected = "separate",
               inline = TRUE)
}

# Function to create analysis date selector (pretend today is)
create_analysis_date_selector <- function() {
  dateInput("analysis_date", "Pretend Today Is:",
           value = Sys.Date(),
           min = "2020-01-01",
           max = Sys.Date() + 365)
}

# Function to create metric toggle (acres vs sites)
create_metric_toggle <- function() {
  radioButtons("display_metric", "Display Metric:",
               choices = list(
                 "Number of Sites" = "sites",
                 "Acres" = "acres"
               ),
               selected = "sites",
               inline = TRUE)
}

# Function to create refresh button UI
create_refresh_button <- function() {
  actionButton("refresh_data", "Refresh Data", 
               class = "btn-primary",
               icon = icon("refresh"))
}

# Function to create download button UI
create_download_button <- function(output_id = "download_data", label = "Download Data") {
  downloadButton(output_id, label,
                 class = "btn-success",
                 icon = icon("download"))
}

# Function to create chart type selector for historical views
create_chart_type_selector <- function() {
  selectInput("chart_type", "Chart Type:",
              choices = list(
                "Stacked Bar" = "stacked_bar",
                "Grouped Bar" = "grouped_bar", 
                "Line Chart" = "line",
                "Area Chart" = "area"
              ),
              selected = "stacked_bar")
}

# Function to create time period selector for historical views
create_time_period_selector <- function() {
  radioButtons("hist_time_period", "Time Period:",
               choices = list(
                 "Weekly" = "weekly",
                 "Monthly" = "monthly",
                 "Yearly" = "yearly"
               ),
               selected = "monthly",
               inline = TRUE)
}

# Function to create year range selector for historical views
create_year_range_selector <- function() {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  list(
    numericInput("start_year", "Start Year:",
                 value = current_year - 2,
                 min = 2020,
                 max = current_year + 5,
                 step = 1,
                 width = "120px"),
    numericInput("end_year", "End Year:",
                 value = current_year,
                 min = 2020,
                 max = current_year + 5, 
                 step = 1,
                 width = "120px")
  )
}

# Function to create info box UI
create_info_box <- function(title, value, subtitle = NULL, icon_name = "info", color = "blue", width = 3) {
  infoBox(
    title = title,
    value = value,
    subtitle = subtitle,
    icon = icon(icon_name),
    color = color,
    width = width,
    fill = TRUE
  )
}

# Function to create value box UI
create_value_box <- function(title, value, subtitle = NULL, icon_name = "chart-bar", color = "blue", width = 3) {
  valueBox(
    value = value,
    subtitle = title,
    icon = icon(icon_name),
    color = color,
    width = width
  )
}

# Function to create tabbed content UI
create_tab_content <- function(tab_id, title, content) {
  tabPanel(title, value = tab_id, content)
}

# Function to create collapsible panel UI  
create_collapsible_panel <- function(title, content, collapsed = FALSE) {
  box(
    title = title,
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = collapsed,
    width = 12,
    content
  )
}

# Function to create loading spinner UI
create_loading_spinner <- function(output_id, type = "html") {
  if (type == "plot") {
    withSpinner(plotlyOutput(output_id), type = 6, color = "#3498db")
  } else if (type == "table") {
    withSpinner(DTOutput(output_id), type = 6, color = "#3498db")
  } else {
    withSpinner(htmlOutput(output_id), type = 6, color = "#3498db")
  }
}

# Function to validate date inputs
validate_dates <- function(start_date, end_date) {
  errors <- c()
  
  if (is.na(start_date) || is.na(end_date)) {
    errors <- c(errors, "Both start and end dates must be provided")
  }
  
  if (start_date >= end_date) {
    errors <- c(errors, "Start date must be before end date")
  }
  
  if (end_date > Sys.Date() + 365) {
    errors <- c(errors, "End date cannot be more than 1 year in the future")
  }
  
  return(errors)
}

# Function to create help text UI
create_help_text <- function(text, placement = "top") {
  tags$span(
    icon("question-circle"),
    title = text,
    `data-toggle` = "tooltip",
    `data-placement` = placement,
    style = "color: #3498db; cursor: help; margin-left: 5px;"
  )
}

# Function to create refresh button UI
create_refresh_button <- function() {
  actionButton("refresh_data", "Refresh Data", 
               class = "btn-primary",
               icon = icon("refresh"))
}

# Function to create download button UI
create_download_button <- function(output_id = "download_data", label = "Download Data") {
  downloadButton(output_id, label,
                 class = "btn-success",
                 icon = icon("download"))
}

# Function to create chart type selector for historical views
create_chart_type_selector <- function() {
  selectInput("chart_type", "Chart Type:",
              choices = list(
                "Stacked Bar" = "stacked_bar",
                "Grouped Bar" = "grouped_bar", 
                "Line Chart" = "line",
                "Area Chart" = "area"
              ),
              selected = "stacked_bar")
}

# Function to create time period selector for historical views
create_time_period_selector <- function() {
  radioButtons("hist_time_period", "Time Period:",
               choices = list(
                 "Weekly" = "weekly",
                 "Monthly" = "monthly",
                 "Yearly" = "yearly"
               ),
               selected = "monthly",
               inline = TRUE)
}

# Function to create metric selector for historical views
create_metric_selector <- function() {
  selectInput("hist_display_metric", "Metric:",
              choices = list(
                "Treatments Applied" = "treatments",
                "Sites Treated" = "sites", 
                "Acres Treated" = "acres",
                "Treatment Plans" = "plans",
                "Active Treatments" = "active_treatments"
              ),
              selected = "treatments")
}

# Function to create year range selector for historical views
create_year_range_selector <- function() {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  list(
    numericInput("start_year", "Start Year:",
                 value = current_year - 2,
                 min = 2020,
                 max = current_year + 5,
                 step = 1,
                 width = "120px"),
    numericInput("end_year", "End Year:",
                 value = current_year,
                 min = 2020,
                 max = current_year + 5, 
                 step = 1,
                 width = "120px")
  )
}

# Function to create info box UI
create_info_box <- function(title, value, subtitle = NULL, icon_name = "info", color = "blue", width = 3) {
  infoBox(
    title = title,
    value = value,
    subtitle = subtitle,
    icon = icon(icon_name),
    color = color,
    width = width,
    fill = TRUE
  )
}

# Function to create value box UI
create_value_box <- function(title, value, subtitle = NULL, icon_name = "chart-bar", color = "blue", width = 3) {
  valueBox(
    value = value,
    subtitle = title,
    icon = icon(icon_name),
    color = color,
    width = width
  )
}

# Function to create tabbed content UI
create_tab_content <- function(tab_id, title, content) {
  tabPanel(title, value = tab_id, content)
}

# Function to create collapsible panel UI  
create_collapsible_panel <- function(title, content, collapsed = FALSE) {
  box(
    title = title,
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = collapsed,
    width = 12,
    content
  )
}

# Function to create loading spinner UI
create_loading_spinner <- function(output_id, type = "html") {
  if (type == "plot") {
    withSpinner(plotlyOutput(output_id), type = 6, color = "#3498db")
  } else if (type == "table") {
    withSpinner(DTOutput(output_id), type = 6, color = "#3498db")
  } else {
    withSpinner(htmlOutput(output_id), type = 6, color = "#3498db")
  }
}

# Function to validate date inputs
validate_dates <- function(start_date, end_date) {
  errors <- c()
  
  if (is.na(start_date) || is.na(end_date)) {
    errors <- c(errors, "Both start and end dates must be provided")
  }
  
  if (start_date >= end_date) {
    errors <- c(errors, "Start date must be before end date")
  }
  
  if (end_date > Sys.Date() + 365) {
    errors <- c(errors, "End date cannot be more than 1 year in the future")
  }
  
  return(errors)
}

# Function to create help text UI
create_help_text <- function(text, placement = "top") {
  tags$span(
    icon("question-circle"),
    title = text,
    `data-toggle` = "tooltip",
    `data-placement` = placement,
    style = "color: #3498db; cursor: help; margin-left: 5px;"
  )
}