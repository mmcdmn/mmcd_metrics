# UI helper functions for drone app

#' Create the main UI for the drone app
#' @return Shiny UI object
drone_ui <- function() {
  fluidPage(
    # Use universal CSS from db_helpers for consistent text sizing
    get_universal_text_css(),
    # Application title
    titlePanel("Drone Sites with Active and Expiring Treatments"),
    
    # Sidebar with controls
    sidebarLayout(
      sidebarPanel(
        # Refresh button at the top
        actionButton("refresh", "Refresh Data", icon = icon("refresh"), class = "btn-success", style = "width: 100%;"),
        hr(),
        
        # Current Progress tab controls
        conditionalPanel(
          condition = "input.tabs == 'current'",
          radioButtons("current_display_metric", "Display Metric:",
                       choices = c("Number of Sites" = "sites",
                                   "Treated Acres" = "treated_acres"),
                       selected = "sites"),
          
          sliderInput("expiring_days", "Days Until Expiration:",
                      min = 1, max = 30, value = 7, step = 1)
        ),
        
        # Historical Trends tab controls
        conditionalPanel(
          condition = "input.tabs == 'historical'",
          radioButtons("hist_time_period", "Time Period:",
                       choices = c("Yearly" = "yearly",
                                   "Weekly" = "weekly"),
                       selected = "yearly"),
          selectInput("hist_chart_type", "Chart Type:",
                      choices = c("Stacked Bar" = "stacked_bar",
                                  "Grouped Bar" = "grouped_bar", 
                                  "Line Chart" = "line",
                                  "Area Chart" = "area",
                                  "Step Chart" = "step"),
                      selected = "stacked_bar"),
          # Conditional metric choices based on time period
          conditionalPanel(
            condition = "input.hist_time_period == 'yearly'",
            radioButtons("hist_display_metric", "Display Metric:",
                         choices = c("Sites Treated" = "sites",
                                     "Site Acres (Unique)" = "site_acres",
                                     "Treatment Acres (Total)" = "treatment_acres"),
                         selected = "sites")
          ),
          conditionalPanel(
            condition = "input.hist_time_period == 'weekly'",
            radioButtons("hist_display_metric", "Display Metric:",
                         choices = c("Number of Active Sites" = "active_sites",
                                     "Number of Active Acres" = "active_acres"),
                         selected = "active_sites")
          ),
          sliderInput("hist_year_range", "Year Range:",
                      min = 2010, max = 2025, value = c(2018, 2025), step = 1)
        ),
        
        # Map tab controls
        conditionalPanel(
          condition = "input.tabs == 'map'",
          selectInput("map_basemap", "Base Map:",
                      choices = c("Streets" = "carto",
                                  "Satellite" = "satellite", 
                                  "Terrain" = "terrain",
                                  "OpenStreetMap" = "osm"),
                      selected = "carto"),
          sliderInput("expiring_days", "Days Until Expiration:",
                      min = 1, max = 30, value = 7, step = 1)
        ),
        
        # Site Statistics tab controls
        conditionalPanel(
          condition = "input.tabs == 'site_stats'",
          radioButtons("site_stat_type", "Show:",
                      choices = c("Average" = "average",
                                "Largest" = "largest", 
                                "Smallest" = "smallest"),
                      selected = "average"),
          sliderInput("site_year_range", "Year Range:",
                      min = 2010, max = 2025, value = c(2018, 2025), step = 1)
        ),
        
        # Shared controls
        dateInput("analysis_date", "Analysis Date (Pretend Today Is):",
                 value = Sys.Date(), 
                 max = Sys.Date(),
                 format = "yyyy-mm-dd"),
        
        selectInput("color_theme", "Color Theme:",
                    choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
                    selected = "MMCD"),
        
        checkboxInput("prehatch_only", "Show Only Prehatch Sites", value = TRUE),
        
        selectInput("zone_option", "Zone Display:",
                    choices = c("P1 Only" = "p1_only",
                                "P2 Only" = "p2_only", 
                                "P1 and P2 Separate" = "p1_p2_separate",
                                "P1+P2 Combined" = "p1_p2_combined"),
                    selected = "p1_p2_combined"),
        
        selectInput("facility_filter", "Facility:",
                      choices = NULL),
        
        selectizeInput("foreman_filter", "FOS:",
                      choices = NULL, multiple = TRUE),
        
        # Group by controls (dynamic based on tab)
        conditionalPanel(
          condition = "input.tabs == 'current'",
          selectInput("group_by", "Group By:",
                      choices = c("Facility" = "facility",
                                  "FOS" = "foreman",
                                  "Section" = "sectcode",
                                  "All MMCD" = "mmcd_all"),
                      selected = "facility")
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'historical' || input.tabs == 'site_stats'",
          selectInput("group_by", "Group By:",
                      choices = c("Facility" = "facility",
                                  "FOS" = "foreman",
                                  "All MMCD" = "mmcd_all"),
                      selected = "facility")
        ),
        
        # Help text for historical metrics (collapsible)
        hr(),
        div(id = "help-section",
          tags$a(href = "#", onclick = "$(this).next().toggle(); return false;", 
                 style = "color: #17a2b8; text-decoration: none; font-size: 14px;",
                 HTML("<i class='fa fa-question-circle'></i> Show/Hide Help")),
          div(style = "display: none;",
            create_help_text()
          )
        )
      ),
      
      # Main panel with tabs
      mainPanel(
        tabsetPanel(
          id = "tabs",
          tabPanel("Current Progress", value = "current", 
                   br(),
                   textOutput("currentDescription"),
                   br(),
                   plotOutput("currentPlot", height = "auto"),
                   br(),
                   fluidRow(
                     column(10, h4("Sitecode Details")),
                     column(2, downloadButton("download_current_data", "Download CSV", 
                                             class = "btn-success btn-sm", 
                                             style = "margin-top: 20px; float: right;"))
                   ),
                   DT::dataTableOutput("currentDataTable")
          ),
          tabPanel("Map", value = "map",
                   br(),
                   textOutput("mapDescription"),
                   br(),
                   leafletOutput("droneMap", height = "600px"),
                   br(),
                   fluidRow(
                     column(10, h4("Site Details")),
                     column(2, downloadButton("download_map_data", "Download CSV", 
                                             class = "btn-success btn-sm", 
                                             style = "margin-top: 20px; float: right;"))
                   ),
                   DT::dataTableOutput("mapDataTable")
          ),
          tabPanel("Historical Trends", value = "historical", 
                   plotOutput("historicalPlot", height = "auto"),
                   br(),
                   fluidRow(
                     column(10, h4("Historical Data")),
                     column(2, downloadButton("download_historical_data", "Download CSV", 
                                             class = "btn-success btn-sm", 
                                             style = "margin-top: 20px; float: right;"))
                   ),
                   DT::dataTableOutput("historicalDataTable")
          ),
          tabPanel("Site Statistics", value = "site_stats",
                   # Plot and table layout
                   fluidRow(
                     column(8, plotOutput("siteStatsPlot")),
                     column(4, 
                            h4("Site Rankings"),
                            h5("5 Largest Sites"),
                            tableOutput("largestSitesTable"),
                            h5("5 Smallest Sites"),
                            tableOutput("smallestSitesTable")
                     )
                   )
          )
        )
      )
    )
  )
}

# Create help text for historical metrics
create_help_text <- function() {
  div(
    style = "margin: 10px; padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; font-size: 16px;",
    h4("Understanding Historical Metrics", style = "margin-top: 0; font-size: 20px;"),
    
    p(style = "margin-bottom: 15px; font-size: 16px;",
      strong("Time Period Differences:"),
      " Historical metrics can be viewed ", strong("yearly"), " or ", strong("weekly"), ":"
    ),
    
    tags$ul(style = "font-size: 16px;",
      tags$li(strong("Yearly metrics"), " show totals for each calendar year"),
      tags$li(strong("Weekly metrics"), " show which sites had active treatments on Fridays (tracking persistence over time)")
    ),
    
    hr(style = "margin: 15px 0;"),
    
    h5("Yearly Metric Definitions", style = "font-size: 18px;"),
    
    p(style = "margin-bottom: 10px; font-size: 16px;",
      strong("Sites Treated:"), " The number of unique sites that received at least one drone treatment during the time period."
    ),
    
    p(style = "margin-bottom: 10px; font-size: 16px;",
      strong("Site Acres (Unique):"), "Site acres treated one or more times during the year, counted only once per site.",
      br(),
      em("Example: If Site A (20 acres) is treated 3 times, it contributes 20 acres to this metric.")
    ),
    
    p(style = "margin-bottom: 10px; font-size: 16px;",
      strong("Treatment Acres (Total):"), " Total acres of treatment applied, summing all treatments even if on the same site.",
      br(),
      em("Example: If Site A (20 acres) is treated 3 times with 10, 15, and 12 acres of treatment respectively, this metric shows 37 acres (10+15+12).")
    ),
    
    hr(style = "margin: 15px 0;"),
    
    h5("Weekly Metric Definitions", style = "font-size: 18px;"),
    
    p(style = "margin-bottom: 10px; font-size: 16px;",
      strong("Active Sites:"), " The number of sites with active drone treatments on each Friday, based on the treatment date plus the material's effect days."
    ),
    
    p(style = "margin-bottom: 10px; font-size: 16px;",
      strong("Active Acres:"), " The total acres of sites with active treatments on each Friday."
    )
  )
}
