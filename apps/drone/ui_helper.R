# UI helper functions for drone app

#' Create the main UI for the drone app
#' @return Shiny UI object
drone_ui <- function() {
  fluidPage(
    # Application title
    titlePanel("Drone Sites with Active and Expiring Treatments"),
    
    # Sidebar with controls
    sidebarLayout(
      sidebarPanel(
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
                         choices = c("Number of Sites" = "sites",
                                     "Number of Treatments" = "treatments", 
                                     "Number of Acres" = "acres"),
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
        
        checkboxInput("prehatch_only", "Show Only Prehatch Sites", value = FALSE),
        
        selectInput("zone_option", "Zone Display:",
                    choices = c("P1 Only" = "p1_only",
                                "P2 Only" = "p2_only", 
                                "P1 and P2 Separate" = "p1_p2_separate",
                                "P1+P2 Combined" = "p1_p2_combined"),
                    selected = "p1_p2_combined"),
        
        selectizeInput("facility_filter", "Facility:",
                      choices = NULL, multiple = TRUE),
        
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
        
        # Refresh button
        hr(),
        actionButton("refresh", "Refresh Data", icon = icon("refresh"), class = "btn-success", style = "width: 100%;")
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
