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
                                   "Total Acres" = "acres"),
                       selected = "sites"),
          
          sliderInput("expiring_days", "Days Until Expiration:",
                      min = 1, max = 30, value = 7, step = 1)
        ),
        
        # Historical Trends tab controls
        conditionalPanel(
          condition = "input.tabs == 'historical'",
          radioButtons("hist_display_metric", "Display Metric:",
                       choices = c("Number of Sites" = "sites",
                                   "Number of Treatments" = "treatments",
                                   "Number of Acres" = "acres"),
                       selected = "sites"),
          selectInput("hist_start_year", "Start Year:",
                     choices = seq(2010, 2025),
                     selected = 2018),
          selectInput("hist_end_year", "End Year:",
                     choices = seq(2010, 2025),
                     selected = 2025),
          checkboxInput("hist_show_percentages", "Show Percentages", value = FALSE)
        ),
        
        # Site Statistics tab controls
        conditionalPanel(
          condition = "input.tabs == 'site_stats'",
          radioButtons("site_stat_type", "Show:",
                      choices = c("Average" = "average",
                                "Largest" = "largest", 
                                "Smallest" = "smallest"),
                      selected = "average"),
          selectInput("site_start_year", "Start Year:",
                     choices = seq(2010, 2025),
                     selected = 2018),
          selectInput("site_end_year", "End Year:",
                     choices = seq(2010, 2025),
                     selected = 2025)
        ),
        
        # Shared controls
        dateInput("analysis_date", "Analysis Date (Pretend Today Is):",
                 value = Sys.Date(), 
                 max = Sys.Date(),
                 format = "yyyy-mm-dd"),
        
        checkboxInput("prehatch_only", "Show Only Prehatch Sites", value = FALSE),
        
        radioButtons("zone_option", "Zone Display:",
                     choices = c("P1 Only" = "p1_only",
                                 "P2 Only" = "p2_only", 
                                 "P1 and P2 Separate" = "p1_p2_separate",
                                 "P1+P2 Combined" = "p1_p2_combined"),
                     selected = "p1_p2_separate"),
        
        selectizeInput("facility_filter", "Facility:",
                      choices = NULL, multiple = TRUE),
        
        selectizeInput("foreman_filter", "FOS:",
                      choices = NULL, multiple = TRUE),
        
        # Group by controls (dynamic based on tab)
        conditionalPanel(
          condition = "input.tabs == 'current'",
          radioButtons("group_by", "Group By:",
                       choices = c("Facility" = "facility",
                                   "FOS" = "foreman",
                                   "Section" = "sectcode",
                                   "All MMCD" = "mmcd_all"),
                       selected = "facility")
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'historical' || input.tabs == 'site_stats'",
          radioButtons("group_by", "Group By:",
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
          tabPanel("Current Progress", value = "current", plotOutput("currentPlot")),
          tabPanel("Historical Trends", value = "historical", plotOutput("historicalPlot")),
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
