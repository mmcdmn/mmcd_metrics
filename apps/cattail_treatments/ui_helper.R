# Cattail Treatments - UI Helper Functions
# Helper functions for creating UI components 

library(shiny)
library(shinyWidgets)
library(DT)
library(plotly)
library(leaflet)

#' Create the main UI for the cattail treatments app
#' @return Shiny UI object
cattail_treatments_ui <- function() {
  fluidPage(
    # Use universal CSS from db_helpers for consistent text sizing
    get_universal_text_css(),
    
    # Application title
    titlePanel("MMCD Cattail Treatments"),
    
    # Sidebar with controls
    sidebarLayout(
      sidebarPanel(
        # Refresh button at the top
        actionButton("refresh_data", "Refresh Data", 
          icon = icon("refresh"),
          class = "btn-primary btn-lg",
          style = "width: 100%; margin-bottom: 20px;"),
        
        # Important note about material codes
        div(style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 10px; margin-bottom: 15px;",
          icon("info-circle", style = "color: #856404; margin-right: 5px;"),
          tags$strong("Note:", style = "color: #856404;"),
          tags$span(style = "color: #856404; font-size: 12px;",
            "This dashboard only shows treatments entered with cattail-specific material codes (e.g., G3). If a site was treated for cattails but entered with a general larvicide code (e.g., G2, matcode 16), the treatment will not appear here and the site may incorrectly show as 'Need Treatment'."
          )
        ),
        
        # Progress tab controls
        conditionalPanel(
          condition = "input.tabs == 'progress'",
          dateInput("analysis_date", "Pretend today is:",
                    value = Sys.Date(),
                    max = Sys.Date(),
                    format = "yyyy-mm-dd"),
          
          selectInput("progress_chart_type", "Progress Chart Type:",
                      choices = c("Line Chart" = "line",
                                  "Grouped Bar Chart" = "bar",
                                  "Stacked Bar Chart" = "stacked"),
                      selected = "stacked")
        ),
        
        # Historical tab controls
        conditionalPanel(
          condition = "input.tabs == 'historical'",
          sliderInput("year_range", "Year Range:",
                      min = 2010, max = as.numeric(format(Sys.Date(), "%Y")),
                      value = c(2020, as.numeric(format(Sys.Date(), "%Y"))),
                      step = 1, sep = ""),
          
          selectInput("hist_status_metric", "Status Metric:",
                      choices = c("Need Treatment (up to December 1)" = "need_treatment",
                                  "Treated (up to Aug 1)" = "treated",
                                  "% Treated of Need Treatment (up to Aug 1)" = "pct_treated"),
                      selected = "need_treatment"),
          
          selectInput("hist_chart_type", "Chart Type:",
                      choices = c("Stacked Bar" = "stacked_bar",
                                  "Grouped Bar" = "grouped_bar",
                                  "Line Chart" = "line",
                                  "Area Chart" = "area"),
                      selected = "stacked_bar"),
          
          actionButton("refresh_historical", "Refresh Historical",
                      icon = icon("refresh"),
                      class = "btn-success",
                      style = "width: 100%; margin-top: 10px;")
        ),
        
        # Map tab controls
        conditionalPanel(
          condition = "input.tabs == 'map'",
          radioButtons("basemap", "Base Map:",
                      choices = c("Street" = "carto", "Satellite" = "satellite",
                                  "OpenStreetMap" = "osm"),
                      selected = "carto")
        ),
        
        # Shared controls
        hr(),
        
        radioButtons("group_by", "Group By:",
                     choices = c("All MMCD" = "mmcd_all",
                                 "FOS" = "foreman",
                                 "Facility" = "facility"),
                     selected = "facility"),
        
        radioButtons("zone_display", "Zone Display:",
                     choices = c("P1" = "p1",
                                 "P2" = "p2",
                                 "P1 and P2 Separate" = "separate",
                                 "P1 and P2 Combined" = "combined"),
                     selected = "separate"),
        
        selectInput("facility_filter", "Facility:",
                    choices = NULL),
        
        radioButtons("display_metric_type", "Display Metric:",
                     choices = c("Number of Sites" = "sites",
                                 "Acres" = "acres"),
                     selected = "sites"),
        
        selectInput("color_theme", "Color Theme:",
                    choices = c("MMCD (Default)" = "MMCD",
                                "IBM Design" = "IBM",
                                "Color-Blind Friendly" = "Wong",
                                "Scientific" = "Tol",
                                "Viridis" = "Viridis",
                                "ColorBrewer" = "ColorBrewer"),
                    selected = "MMCD"),
        
        tags$small(style = "color: #999;", "Note: cattail Year runs from Fall (Sept-Dec) to Summer (May-Aug).")
      ),
      
      # Main panel with tabs
      mainPanel(
        tabsetPanel(id = "tabs",
          tabPanel("Progress", value = "progress",
                   fluidRow(
                     column(2, uiOutput("sites_inspected_box")),
                     column(2, uiOutput("under_threshold_box")),
                     column(2, uiOutput("active_treatments_box")),
                     column(2, uiOutput("treated_sites_box")),
                     column(4, uiOutput("treatment_coverage_box"))
                   ),
                   br(),
                   fluidRow(
                     column(12, 
                       h4("Cattail Treatment Progress"),
                       plotlyOutput("progress_chart", height = "500px")
                     )
                   ),
                   br(),
                   fluidRow(
                     column(12,
                       h4("Site Status Details"),
                       DTOutput("status_table")
                     )
                   ),
                   br(),
          ),
          tabPanel("Historical", value = "historical",
                   br(),
                   fluidRow(
                     column(12,
                       h4("Historical Cattail Treatment Data"),
                       plotlyOutput("historical_chart", height = "550px")
                     )
                   )
          ),
          tabPanel("Map", value = "map",
                   br(),
                   fluidRow(
                     column(12,
                       h4("Cattail Treatment Sites"),
                       leafletOutput("treatment_map", height = "600px")
                     )
                   ),
                   br(),
                   fluidRow(
                     column(12,
                       h4("Site Details"),
                       DTOutput("map_details_table")
                     )
                   )
          )
        )
      )
    )
  )
}

#' Create a custom metric box (replacement for valueBox)
#' @param value The main value to display
#' @param subtitle The subtitle text
#' @param icon The icon name (without "fa-" prefix)
#' @param color The color theme ("primary", "success", "warning", "danger", "info")
#' @return HTML div
create_metric_box <- function(value, subtitle, icon = "chart-line", color = "primary") {
  color_map <- list(
    primary = "#3c8dbc",
    success = "#00a65a",
    warning = "#f39c12",
    danger = "#dd4b39",
    info = "#00c0ef"
  )
  
  bg_color <- color_map[[color]] %||% color_map$primary
  
  div(
    style = sprintf(
      "background-color: %s; color: white; padding: 15px; border-radius: 3px; margin-bottom: 10px; min-height: 90px;",
      bg_color
    ),
    div(
      style = "font-size: 28px; font-weight: bold;",
      value
    ),
    div(
      style = "font-size: 14px; margin-top: 5px;",
      icon(icon, style = "margin-right: 5px;"),
      subtitle
    )
  )
}

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
  selectizeInput("facility_filter", "Facility:",
                 choices = c("All" = "all"),
                 selected = "all",
                 multiple = TRUE,
                 width = "100%")
}

# Function to create foreman filter UI (populated dynamically from database)
create_foreman_filter <- function() {
  conditionalPanel(
    condition = "input.group_by == 'foreman' || input.group_by == 'mmcd_all'",
    selectizeInput("foreman_filter", "FOS:",
                   choices = c("All" = "all"),
                   selected = "all",
                   multiple = TRUE,
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
      
      # Important note about material codes
      fluidRow(
        column(12,
          div(style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 10px; margin-bottom: 15px;",
            icon("info-circle", style = "color: #856404; margin-right: 5px;"),
            tags$strong("Note:", style = "color: #856404;"),
            tags$span(style = "color: #856404;",
              "This dashboard only shows treatments entered with cattail-specific material codes (e.g., G3). ",
              "If a site was treated for cattails but entered with a general larvicide code (e.g., G2, matcode 16), ",
              "the treatment will not appear here and the site may incorrectly show as 'Need Treatment'."
            )
          )
        )
      ),
      
      fluidRow(
        column(2, create_analysis_date_selector()),
        column(2, create_grouping_selector()),
        column(2, create_zone_display_selector()),
        column(2, create_facility_filter()),
        column(2, create_progress_chart_type_selector()),
        column(2, create_metric_selector())
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

# Function to create metric selector (sites vs acres)
create_metric_selector <- function() {
  radioButtons("display_metric_type", "Display Metric:",
              choices = list(
                "Number of Sites" = "sites",
                "Acres" = "acres"
              ),
              selected = "sites",
              inline = TRUE,
              width = "100%")
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

# create_chart_type_selector moved to historical_functions.R to avoid duplication

# create_year_range_selector moved to historical_functions.R to avoid duplication

# Function to create value boxes with metric awareness
create_cattail_value_boxes <- function(aggregated_data, treatments_data = NULL, plans_data = NULL) {
  if (nrow(aggregated_data) == 0) {
    return(list(
      total_count = 0, total_acres = 0, total_treatments = 0,
      active_treatments = 0, treatment_coverage = 0, 
      acres_treated = 0
    ))
  }
  
  # Calculate totals
  total_count <- sum(aggregated_data$total_count, na.rm = TRUE)
  total_acres <- sum(aggregated_data$total_acres, na.rm = TRUE)
  total_treatments <- sum(aggregated_data$treatments_applied, na.rm = TRUE)
  active_treatments <- sum(aggregated_data$active_treatments, na.rm = TRUE)
  acres_treated <- sum(aggregated_data$acres_treated, na.rm = TRUE)
  
  # Calculate coverage percentage
  treatment_coverage <- if (total_count > 0) {
    round((total_treatments / total_count) * 100, 1)
  } else {
    0
  }
  
  return(list(
    total_count = total_count,
    total_acres = total_acres,
    total_treatments = total_treatments,
    active_treatments = active_treatments,
    treatment_coverage = treatment_coverage,
    acres_treated = acres_treated
  ))
}