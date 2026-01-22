# UI helper functions for Ground Prehatch Progress app
# These functions create reusable UI components and improve code organization

#' Create the main UI for the ground prehatch progress app
#' @return Shiny UI object
ground_prehatch_ui <- function() {
  fluidPage(
    # Use universal CSS from db_helpers for consistent text sizing
    get_universal_text_css(),
    # Application title
    titlePanel("Ground Prehatch Treatment Progress"),
    
    # Sidebar with controls
    sidebarLayout(
      sidebarPanel(
        # Refresh button at the top
        actionButton("refresh", "Refresh Data", icon = icon("refresh"), class = "btn-success", style = "width: 100%;"),
        hr(),
        
        # Current Progress and Detailed View tab controls
        conditionalPanel(
          condition = "input.tabs == 'overview' || input.tabs == 'details'",
          radioButtons("display_metric", "Display Metric:",
                      choices = c("Sites" = "sites",
                                  "Acres" = "acres"),
                      selected = "sites"),
          
          dateInput("custom_today", "Pretend Today is:",
                   value = Sys.Date(), 
                   max = Sys.Date(),
                   format = "yyyy-mm-dd"),
          
          sliderInput("expiring_days", "Days Until Expiring:",
                     min = 1, max = 60, value = 14, step = 1),
          
          radioButtons("expiring_filter", "Site Filter:",
                      choices = c("All Sites" = "all",
                                 "Expiring Only" = "expiring", 
                                 "Expiring + Expired" = "expiring_expired"),
                      selected = "all")
        ),
        
        # Historical Analysis tab controls
        conditionalPanel(
          condition = "input.tabs == 'historical'",
          actionButton("hist_refresh", "Refresh Historical", icon = icon("refresh"), class = "btn-success", style = "width: 100%; margin-bottom: 10px;"),
          
          radioButtons("hist_time_period", "Time Period:",
                      choices = c("Yearly" = "yearly",
                                  "Weekly" = "weekly"),
                      selected = "yearly"),
          
          selectInput("hist_chart_type", "Chart Type:",
                      choices = c("Stacked Bar" = "stacked_bar",
                                  "Grouped Bar" = "grouped_bar", 
                                  "Line Chart" = "line",
                                  "Area Chart" = "area"),
                      selected = "stacked_bar"),
          
          # Conditional metric choices based on time period
          conditionalPanel(
            condition = "input.hist_time_period == 'yearly'",
            radioButtons("hist_display_metric", "Display Metric:",
                        choices = c("Sites Treated" = "sites",
                                   "Site Acres (Unique)" = "acres",
                                   "Treatment Acres (Total)" = "treatment_acres"),
                        selected = "sites")
          ),
          conditionalPanel(
            condition = "input.hist_time_period == 'weekly'",
            radioButtons("hist_display_metric", "Display Metric:",
                        choices = c("Active Sites" = "weekly_active_count",
                                   "Active Acres" = "weekly_active_acres"),
                        selected = "weekly_active_count")
          ),
          
          sliderInput("hist_year_range", "Year Range:",
                      min = 2010, max = 2025, 
                      value = c(2018, 2025), step = 1)
        ),
        
        # Shared controls
        selectInput("color_theme", "Color Theme:",
                    choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
                    selected = "MMCD"),
        
        selectInput("zone_filter", "Zone Display:",
                    choices = c("P1 Only" = "1",
                                "P2 Only" = "2", 
                                "P1 and P2 Separate" = "1,2",
                                "Combined P1+P2" = "combined"),
                    selected = "1,2"),
        
        selectInput("facility_filter", "Facility:",
                      choices = NULL),
        
        selectizeInput("foreman_filter", "FOS:",
                      choices = NULL, multiple = TRUE),
        
        # Group by controls (dynamic based on tab)
        conditionalPanel(
          condition = "input.tabs == 'overview' || input.tabs == 'details'",
          selectInput("group_by", "Group By:",
                      choices = c("Facility" = "facility",
                                  "FOS" = "foreman",
                                  "Section" = "sectcode",
                                  "All MMCD" = "mmcd_all"),
                      selected = "mmcd_all")
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'historical'",
          selectInput("group_by", "Group By:",
                      choices = c("Facility" = "facility",
                                  "FOS" = "foreman",
                                  "All MMCD" = "mmcd_all"),
                      selected = "mmcd_all")
        ),
        
        # Help text (collapsible)
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
          tabPanel("Progress Overview", value = "overview", 
                   br(),
                   create_important_note(),
                   # Summary statistics
                   div(
                     h4("Summary Statistics", style = "color: #3c8dbc; margin-bottom: 15px;"),
                     create_overview_value_boxes()
                   ),
                   br(),
                   # Progress chart
                   uiOutput("dynamic_chart_ui")
          ),
          tabPanel("Detailed View", value = "details",
                   br(),
                   create_important_note(),
                   h4("Site Details"),
                   div(
                     style = "margin-bottom: 15px;",
                     downloadButton("download_details_data", "Download CSV", class = "btn-primary btn-sm"),
                     tags$span(style = "margin-left: 15px; color: #666;", 
                               "Download filtered data as CSV file")
                   ),
                   DT::dataTableOutput("details_table")
          ),
          tabPanel("Historical Analysis", value = "historical", 
                   br(),
                   plotlyOutput("historical_chart", height = "600px"),
                   br(),
                   fluidRow(
                     column(10, h4("Historical Data")),
                     column(2, downloadButton("download_historical_data", "Download CSV", 
                                             class = "btn-success btn-sm", 
                                             style = "margin-top: 20px; float: right;"))
                   ),
                   DT::dataTableOutput("historical_details_table")
          )
        )
      )
    )
  )
}

# Create the main filter panel for dashboard layout (DEPRECATED - kept for compatibility)
create_filter_panel <- function() {
  box(
    title = "Filters & Controls",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    
    fluidRow(
      column(3,
        selectInput("facility_filter", "Facility:",
                      choices = c("All Facilities" = "all"),
                      selected = "all")
      ),
      column(3,
        selectizeInput("foreman_filter", "FOS:",
                      choices = c("All" = "all"),
                      selected = "all",
                      multiple = TRUE,
                      options = list(placeholder = "Select FOS..."))
      ),
      column(3,
        radioButtons("zone_filter", "Zone Display:",
                     choices = c("P1 Only" = "1", 
                                "P2 Only" = "2", 
                                "P1 and P2 Separate" = "1,2", 
                                "Combined P1+P2" = "combined"),
                     selected = "1,2",
                     inline = TRUE)
      ),
      column(2,
        radioButtons("group_by", "Group by:",
                    choices = c("All MMCD" = "mmcd_all",
                               "Facility" = "facility", 
                               "FOS" = "foreman",
                               "Section" = "sectcode"),
                    selected = "mmcd_all",
                    inline = TRUE)
      ),
      column(2,
        selectInput("color_theme", "Color Theme:",
                    choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
                    selected = "MMCD")
      )
    ),
    
    # Current analysis controls (hidden on historical tab)
    conditionalPanel(
      condition = "input.sidebar_tabs != 'historical'",
      fluidRow(
        column(3,
          div(style = "padding-top: 200px;",
            dateInput("custom_today", "Pretend Today is:",
                     value = Sys.Date(), 
                     format = "yyyy-mm-dd")
          )
        ),
        column(3,
          sliderInput("expiring_days", "Days Until Expiring:",
                     min = 1, max = 60, value = 14, step = 1)
        ),
        column(3,
          div(style = "margin-top: 5px;",
            radioButtons("expiring_filter", "Site Filter:",
                        choices = c("All Sites" = "all",
                                   "Expiring Only" = "expiring", 
                                   "Expiring + Expired" = "expiring_expired"),
                        selected = "all",
                        inline = FALSE)
          )
        ),
        column(3,
          div(style = "margin-top: 25px;",
            actionButton("refresh", "Refresh Data", 
                        icon = icon("refresh"),
                        class = "btn-primary btn-lg",
                        style = "width: 100%;")
          )
        )
      )
    ),
    
    # Historical analysis controls (shown only on historical tab)
    conditionalPanel(
      condition = "input.sidebar_tabs == 'historical'",
      fluidRow(
        column(3,
          radioButtons("hist_time_period", "Time Period:",
                      choices = c("Yearly" = "yearly", 
                                  "Weekly" = "weekly"),
                      selected = "yearly",
                      inline = TRUE)
        ),
        column(3,
          selectInput("hist_chart_type", "Chart Type:",
                      choices = c("Stacked Bar" = "stacked_bar",
                                  "Grouped Bar" = "grouped_bar", 
                                  "Line Chart" = "line",
                                  "Area Chart" = "area"),
                      selected = "stacked_bar")
        ),
        column(3,
          # Conditional metric selector based on time period
          conditionalPanel(
            condition = "input.hist_time_period == 'yearly'",
            radioButtons("hist_display_metric", "Display Metric:",
                        choices = c("Sites Treated" = "sites",
                                   "Site Acres (Unique)" = "acres",
                                   "Treatment Acres (Total)" = "treatment_acres"),
                        selected = "sites",
                        inline = TRUE)
          ),
          conditionalPanel(
            condition = "input.hist_time_period == 'weekly'",
            radioButtons("hist_display_metric", "Display Metric:",
                        choices = c("Active Sites" = "weekly_active_count",
                                   "Active Acres" = "weekly_active_acres"),
                        selected = "weekly_active_count",
                        inline = TRUE)
          )
        ),
        column(3,
          sliderInput("hist_year_range", "Year Range:",
                      min = as.numeric(format(Sys.Date(), "%Y")) - 20, 
                      max = as.numeric(format(Sys.Date(), "%Y")), 
                      value = c(as.numeric(format(Sys.Date(), "%Y")) - 4, as.numeric(format(Sys.Date(), "%Y"))), 
                      step = 1)
        )
      ),
      fluidRow(
        column(12,
          div(style = "margin-top: 15px;",
            actionButton("hist_refresh", "Refresh Historical Data", 
                        icon = icon("refresh"),
                        class = "btn-primary btn-lg",
                        style = "width: 220px;")
          )
        )
      )
    ),
    
    # Map controls (shown only on map tab)
    conditionalPanel(
      condition = "input.sidebar_menu == 'map'",
      fluidRow(
        column(6,
          selectInput("map_basemap", "Base Map:",
                      choices = c("Streets" = "carto",
                                  "Satellite" = "satellite",
                                  "OpenStreetMap" = "osm"),
                      selected = "carto")
        ),
        column(6,
          div(style = "margin-top: 5px;",
            p("Map uses the same filters and analysis date as other tabs.")
          )
        )
      )
    )
  )
}

# Create the overview value boxes (like red_air does)
create_overview_value_boxes <- function() {
  fluidRow(
    column(2, uiOutput("prehatch_sites")),
    column(2, uiOutput("treated_sites")),
    column(2, uiOutput("sites_expiring")),
    column(2, uiOutput("expired_sites")),
    column(2, uiOutput("skipped_sites")),
    column(2, uiOutput("treated_pct"))
  )
}

# Create the section filtering info panel
create_section_info_panel <- function() {
  conditionalPanel(
    condition = "input.group_by == 'All MMCD'",
    div(
      style = "background-color: #d1ecf1; padding: 12px; border-radius: 4px; margin-bottom: 15px; border: 1px solid #bee5eb;",
      HTML("<i class='fa fa-info-circle' style='color: #0c5460;'></i> 
           <strong>Section Grouping:</strong> Section filtering is only available when a specific facility is selected (not 'All').")
    )
  )
}

# Create the progress chart box
create_progress_chart_box <- function() {
  # Dynamic chart height will be set in server
  tagList(
    create_important_note(),
    box(
      title = "Ground Prehatch Treatment Progress",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      uiOutput("dynamic_chart_ui")
    )
  )
}

# Create the details table box
create_details_table_box <- function() {
  tagList(
    create_important_note(),
    box(
      title = "Site Details",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      div(
        style = "margin-bottom: 15px;",
        downloadButton("download_details_data", "Download CSV", class = "btn-primary btn-sm"),
        tags$span(style = "margin-left: 15px; color: #666;", 
                  "Download filtered data as CSV file")
      ),
      DT::dataTableOutput("details_table")
    )
  )
}

# Create universal important note about expired sites
create_important_note <- function() {
  div(
    style = "margin-bottom: 15px; padding: 12px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px; color: #856404;",
    HTML("<strong><i class='fa fa-info-circle'></i> Important Note:</strong> Some prehatch sites may be expired for good operational reasons. For example:<br>
         • Sites that dry up later in summer and become unsuitable for treatment<br>
         • Sites that have been consistently dry for months and are intentionally skipped<br>
         • Sites with environmental or access restrictions that prevent routine treatment<br>
         Expired status does not necessarily indicate a problem - it may reflect sound operational decisions.")
  )
}

# Create help text for the app
create_help_text <- function() {
  div(
    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px; border-left: 4px solid #17a2b8;",
    h5("How to Use This Dashboard", style = "color: #17a2b8; margin-top: 0;"),
    tags$ul(
      tags$li(tags$strong("Overview Tab:"), "View summary statistics and progress charts"),
      tags$li(tags$strong("Detailed View Tab:"), "Examine individual site details and download data"),
      tags$li(tags$strong("Historical Tab:"), "Analyze treatment trends over time"),
      tags$li(tags$strong("Zone Options:"), "Choose how to display P1/P2 zones - separate or combined"),
      tags$li(tags$strong("Grouping:"), "Group data by All MMCD, Facility, FOS, or Section"),
      tags$li(tags$strong("Date Simulation:"), "Use 'Pretend Today is' to see status on any date"),
      br(),
      tags$li(tags$strong("Ground Site Definition:"), " Sites labeled as 'Ground' in the data are included in this analysis IF they also have an non null Prehatch field. Regardless of Drone status")
    ),
    br(),
    h5("Historical Analysis Display Metrics", style = "color: #17a2b8; margin-top: 0;"),
    div(
      style = "background-color: #fff; padding: 10px; border-radius: 3px; border: 1px solid #ddd;",
      
      # Time Period Explanation
      tags$div(
        style = "background-color: #f8f9fa; padding: 8px; border-radius: 3px; margin-bottom: 10px; border-left: 3px solid #28a745;",
        tags$strong("Time Period Differences:"),
        tags$ul(
          tags$li(tags$strong("Yearly Analysis:"), " Shows treatment activity - when treatments were applied"),
          tags$li(tags$strong("Weekly Analysis:"), " Shows active treatment coverage - sites with active treatments on that week's Friday. NOTE: This takes a long time to run")
        )
      ),
      
      # Metric Definitions
      tags$strong("Yearly Analysis Metrics:"),
      tags$ul(
        tags$li(
          tags$strong("Sites Treated:"), 
          " Number of unique sites that received treatment. ",
          tags$em("Each site is counted only once per time period"),
          " regardless of how many times it was treated."
        ),
        tags$li(
          tags$strong("Site Acres (Unique):"), 
          " Total site capacity (acres) of unique sites that received treatment. ",
          tags$em("Each site's acres are counted only once per time period"),
          " - if a site is treated multiple times, its acres count only once. This shows the total acreage capacity that received any treatment."
        ),
        tags$li(
          tags$strong("Treatment Acres (Total):"), 
          " Total acres covered by all treatment applications. ",
          tags$em("Sums actual treated acres from each treatment record"),
          " - if a site is treated multiple times, all treated acres are included. This shows total treatment effort/coverage."
        )
      ),
      
      tags$strong("Weekly Analysis Metrics:"),
      tags$ul(
        tags$li(
          tags$strong("Active Sites:"), 
          " Number of sites with active treatment coverage on each Friday. ",
          tags$em("Counts unique sites where inspdate <= Friday AND (inspdate + effect_days) >= Friday"),
          " - shows how many sites are currently protected."
        ),
        tags$li(
          tags$strong("Active Acres:"), 
          " Total site capacity (acres) covered by active treatments on each Friday. ",
          tags$em("Sums unique site acres for sites with active treatment"),
          " - shows total acreage capacity under active protection."
        )
      ),
      
      tags$p(
        style = "margin-top: 10px; padding: 8px; background-color: #e7f3ff; border-left: 3px solid #007bff; margin-bottom: 0;",
        tags$strong("Example:"), " If Site A (20 acres) is treated 3 times with 10, 15, and 12 acres respectively: ",
        tags$strong("Sites Treated = 1"), ", ",
        tags$strong("Site Acres = 20"), " (site capacity counted once), ",
        tags$strong("Treatment Acres = 37"), " (10+15+12 from all treatments)."
      ),
      tags$p(
        style = "margin-top: 10px; padding: 8px; background-color: #fff3cd; border-left: 3px solid #ffc107; margin-bottom: 0;",
        tags$strong("Weekly Analysis:"), " Uses each Friday to check treatment effectiveness. ",
        "A site counts as 'active' if its most recent treatment + effect_days >= that Friday. ",
        "Shows operational coverage rather than treatment activity."
      )
    )
  )
}

# Create the historical filter panel
create_historical_filter_panel <- function() {
  box(
    title = "Historical Analysis Filters",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    
    fluidRow(
      column(3,
        selectInput("hist_facility_filter", "Facility:",
                      choices = c("All Facilities" = "all"),
                      selected = "all")
      ),
      column(3,
        selectizeInput("hist_foreman_filter", "FOS:",
                      choices = c("All" = "all"),
                      selected = "all",
                      multiple = TRUE,
                      options = list(placeholder = "Select FOS..."))
      ),
      column(3,
        radioButtons("hist_zone_filter", "Zone Display:",
                     choices = c("P1 Only" = "1", 
                                "P2 Only" = "2", 
                                "P1 and P2 Separate" = "1,2", 
                                "Combined P1+P2" = "combined"),
                     selected = "1,2",
                     inline = TRUE)
      ),
      column(3,
        radioButtons("hist_group_by", "Group by:",
                    choices = c("All MMCD" = "mmcd_all",
                               "Facility" = "facility", 
                               "FOS" = "foreman",
                               "Section" = "sectcode"),
                    selected = "mmcd_all",
                    inline = TRUE)
      )
    ),
    
    fluidRow(
      column(3,
        radioButtons("hist_time_period", "Time Period:",
                    choices = c("Weekly" = "weekly", "Yearly" = "yearly"),
                    selected = "weekly",
                    inline = TRUE)
      ),
      column(3,
        # Conditional metric selector based on time period
        conditionalPanel(
          condition = "input.hist_time_period == 'yearly'",
          radioButtons("hist_display_metric", "Display Metric:",
                      choices = c("Treatments" = "treatments",
                                 "Sites Treated" = "sites",
                                 "Acres Treated" = "acres",
                                 "Site Acres (Unique Sites)" = "site_acres"),
                      selected = "treatments",
                      inline = TRUE)
        ),
        conditionalPanel(
          condition = "input.hist_time_period == 'weekly'",
          radioButtons("hist_display_metric", "Display Metric:",
                      choices = c("Active Sites" = "weekly_active_count",
                                 "Active Site Acres" = "weekly_active_acres"),
                      selected = "weekly_active_count",
                      inline = TRUE)
        )
      ),
      column(3,
        conditionalPanel(
          condition = "input.hist_time_period == 'weekly'",
          numericInput("hist_year", "Year:", 
                      value = as.numeric(format(Sys.Date(), "%Y")), 
                      min = as.numeric(format(Sys.Date(), "%Y")) - 20, 
                      max = as.numeric(format(Sys.Date(), "%Y")) + 1, 
                      step = 1)
        ),
        conditionalPanel(
          condition = "input.hist_time_period == 'yearly'",
          div(
            style = "margin-top: 25px;",
            numericInput("hist_start_year", "Start Year:", 
                        value = as.numeric(format(Sys.Date(), "%Y")) - 4, 
                        min = as.numeric(format(Sys.Date(), "%Y")) - 20, 
                        max = as.numeric(format(Sys.Date(), "%Y")), 
                        step = 1),
            numericInput("hist_end_year", "End Year:", 
                        value = as.numeric(format(Sys.Date(), "%Y")), 
                        min = as.numeric(format(Sys.Date(), "%Y")) - 20, 
                        max = as.numeric(format(Sys.Date(), "%Y")), 
                        step = 1)
          )
        )
      ),
      column(3,
        div(style = "margin-top: 25px;",
          actionButton("hist_refresh", "Refresh Historical Data", 
                      icon = icon("refresh"),
                      class = "btn-primary btn-lg",
                      style = "width: 100%;")
        )
      )
    )
  )
}

# Create the historical chart box
create_historical_chart_box <- function() {
  tagList(
    create_important_note(),
    box(
      title = "Historical Ground Prehatch Trends",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      plotlyOutput("historical_chart", height = "500px")
    )
  )
}

# Create the historical details table box
create_historical_details_table_box <- function() {
  box(
    title = "Historical Data Details",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    div(
      style = "margin-bottom: 15px;",
      downloadButton("download_historical_data", "Download Historical CSV", class = "btn-primary btn-sm"),
      tags$span(style = "margin-left: 15px; color: #666;", 
                "Download historical data as CSV file")
    ),
    DT::dataTableOutput("historical_details_table")
  )
}

# Create the map view box
create_map_box <- function() {
  tagList(
    create_important_note(),
    box(
      title = "Ground Prehatch Site Map",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      # Add map description
      div(
        style = "margin-bottom: 10px; padding: 8px; background-color: #f8f9fa; border-radius: 4px;",
        textOutput("mapDescription")
      ),
      leafletOutput("ground_map", height = "600px")
    )
  )
}

# Create the map details table box
create_map_details_table_box <- function() {
  box(
    title = "Map Site Details",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    div(
      style = "margin-bottom: 15px;",
      downloadButton("download_map_data", "Download Map CSV", class = "btn-primary btn-sm"),
      tags$span(style = "margin-left: 15px; color: #666;", 
                "Download map data as CSV file")
    ),
    DT::dataTableOutput("map_details_table")
  )
}

create_app_css <- function() {
  # Empty function - all styling is done via inline styles
  NULL
}