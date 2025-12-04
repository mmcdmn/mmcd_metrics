# UI helper functions for Catch Basin Status app
# These functions create reusable UI components and improve code organization

# Create the main filter panel for dashboard layout
create_filter_panel <- function() {
  box(
    title = "Filters & Controls",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    
    fluidRow(
      column(3,
        selectizeInput("facility_filter", "Facility:",
                      choices = c("All" = "all"),
                      selected = "all", 
                      multiple = TRUE,
                      options = list(placeholder = "Select facilities..."))
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
      column(3,
        radioButtons("group_by", "Group by:",
                    choices = c("All MMCD" = "mmcd_all",
                               "Facility" = "facility", 
                               "FOS" = "foreman",
                               "Section" = "sectcode"),
                    selected = "facility",
                    inline = TRUE)
      )
    ),
    
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
  )
}

# Create help text section
create_help_text <- function() {
  div(
    style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px; margin-bottom: 20px; border-left: 4px solid #17a2b8;",
    h4("How to Use This Dashboard", style = "color: #17a2b8; margin-top: 0;"),
    tags$ul(
      tags$li(strong("Facility:"), " Filter data by specific facility or view all facilities combined."),
      tags$li(strong("FOS:"), " Filter by Field Operations Supervisor area."),
      tags$li(strong("Zone Display:"), " View zones separately, combined, or filter to a single priority zone."),
      tags$li(strong("Group By:"), " View data aggregated across all MMCD or broken down by facility, FOS, or section."),
      tags$li(strong("Pretend Today is:"), " Set a custom analysis date to view historical treatment status."),
      tags$li(strong("Days Until Expiring:"), " Adjust the threshold for identifying catch basins nearing treatment expiration."),
      tags$li(strong("Site Filter:"), " Filter to show all sites, only expiring sites, or expiring plus expired sites."),
      tags$li(strong("Status Overview:"), " View summary statistics and visual charts of catch basin treatment status."),
      tags$li(strong("Detailed View:"), " Browse detailed data table with all catch basin counts and percentages."),
      tags$li(strong("Historical Analysis:"), " Analyze catch basin treatment trends over time.")
    ),
    h4("Key Metrics", style = "color: #17a2b8;"),
    tags$ul(
      tags$li(strong("Total Wet Catch Basins:"), " Count of all catch basins currently classified as wet (status_udw='W')."),
      tags$li(strong("Wet CB with Active Treatment:"), " Count of wet catch basins with current active or expiring treatment."),
      tags$li(strong("Treatment Coverage:"), " Percentage of wet catch basins that have active treatment. ", 
              tags$em("Calculation: (Active Treatment Count / Total Wet CB Count) × 100")),
      tags$li(strong("Expiring:"), " Catch basins with treatments nearing expiration (within specified days threshold)."),
      tags$li(strong("Expired:"), " Catch basins with treatments that have passed their effective treatment period."),
      tags$li(strong("Never Treated:"), " Wet catch basins that have never received any treatment this year.")
    ),
    h4("Historical Metrics Explained", style = "color: #17a2b8;"),
    tags$ul(
      tags$li(strong("Yearly - Total Treatments:"), " Counts every treatment applied. Since catch basins are treated multiple times per year, a single catch basin treated 3 times will contribute 3 to this count. Use this to track treatment workload."),
      tags$li(strong("Yearly - Unique Wet CB Treated:"), " Counts each catch basin only once, regardless of how many times it was treated. Use this to track coverage of the wet catch basin inventory."),
      tags$li(strong("Weekly - Active Treatments:"), " For each week, counts how many treatments were still active (not expired) on Friday of that week. Shows treatment effectiveness over time.")
    )
  )
}

# Create value boxes container for overview tab
create_overview_value_boxes <- function() {
  fluidRow(
    column(3, valueBoxOutput("total_wet_cb", width = 12)),
    column(3, valueBoxOutput("total_treated", width = 12)),
    column(2, valueBoxOutput("percent_treated", width = 12)),
    column(2, valueBoxOutput("total_expiring", width = 12)),
    column(2, valueBoxOutput("total_expired", width = 12))
  )
}

# Create status chart box
create_status_chart_box <- function() {
  box(
    title = "Catch Basin Status by Group",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    uiOutput("chart_ui")
  )
}

# Create details table box
create_details_table_box <- function() {
  box(
    title = "Detailed Catch Basin Status Data",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    DTOutput("details_table")
  )
}

# Historical tab UI components
create_historical_filter_panel <- function() {
  box(
    title = "Historical Analysis Filters",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    
    fluidRow(
      column(3,
        radioButtons("hist_time_period", "Time Period:",
                    choices = c("Yearly" = "yearly", "Weekly" = "weekly"),
                    selected = "yearly")
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
        conditionalPanel(
          condition = "input.hist_time_period == 'yearly'",
          radioButtons("hist_display_metric_yearly", "Display Metric:",
                     choices = c("Total Treatments" = "treatments",
                                "Unique Wet CB Treated" = "wet_cb_count"),
                     selected = "treatments",
                     inline = TRUE)
        ),
        conditionalPanel(
          condition = "input.hist_time_period == 'weekly'",
          radioButtons("hist_display_metric_weekly", "Display Metric:",
                     choices = c("Active Treatments" = "weekly_active_treatments"),
                     selected = "weekly_active_treatments",
                     inline = TRUE)
        )
      ),
      column(3,
        sliderInput("hist_year_range", "Year Range:",
                   min = 2010, max = as.numeric(format(Sys.Date(), "%Y")),
                   value = c(2000, as.numeric(format(Sys.Date(), "%Y"))),
                   step = 1, sep = "")
      ),
      column(3,
        div(style = "margin-top: 25px;",
          actionButton("hist_refresh", "Refresh Historical Data", 
                      icon = icon("sync"), 
                      class = "btn-primary btn-block")
        )
      )
    )
  )
}

create_historical_chart_box <- function() {
  box(
    title = "Historical Trends",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    plotlyOutput("historical_chart", height = "500px")
  )
}

create_historical_details_table_box <- function() {
  box(
    title = "Historical Data Table",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    DTOutput("historical_table")
  )
}
