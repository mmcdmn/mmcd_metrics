# UI helper functions for Ground Prehatch Progress app
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
        dateInput("custom_today", "Pretend Today is:",
                 value = Sys.Date(), 
                 format = "yyyy-mm-dd")
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

# Create the overview value boxes (like red_air does)
create_overview_value_boxes <- function() {
  fluidRow(
    valueBoxOutput("total_sites", width = 2),
    valueBoxOutput("prehatch_sites", width = 2),
    valueBoxOutput("treated_sites", width = 2),
    valueBoxOutput("expired_sites", width = 2),
    valueBoxOutput("treated_pct", width = 2),
    valueBoxOutput("expiring_pct", width = 2)
  )
}

# Create the section filtering info panel
create_section_info_panel <- function() {
  conditionalPanel(
    condition = "input.group_by == 'sectcode'",
    div(
      style = "background-color: #d1ecf1; padding: 12px; border-radius: 4px; margin-bottom: 15px; border: 1px solid #bee5eb;",
      HTML("<i class='fa fa-info-circle' style='color: #0c5460;'></i> 
           <strong>Section Grouping:</strong> Section filtering is only available when a specific facility is selected (not 'All').")
    )
  )
}

# Create the progress chart box
create_progress_chart_box <- function() {
  # Dynamically set chart height based on number of y-axis categories
  n_bars <- isolate({
    # Try to get the number of bars from the aggregated data if available
    if (exists("aggregated_data", envir = .GlobalEnv)) {
      nrow(get("aggregated_data", envir = .GlobalEnv))
    } else {
      10 # fallback default
    }
  })
  chart_height <- max(100, min(66000, n_bars * 50)) # 50px per bar, clamp between 400 and 66000px
  box(
    title = "Ground Prehatch Treatment Progress",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    plotlyOutput("progress_chart", height = paste0(chart_height, "px"))
  )
}

# Create the details table box
create_details_table_box <- function() {
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
}

# Create help text for the app
create_help_text <- function() {
  div(
    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px; border-left: 4px solid #17a2b8;",
    h5("How to Use This Dashboard", style = "color: #17a2b8; margin-top: 0;"),
    tags$ul(
      tags$li(tags$strong("Overview Tab:"), "View summary statistics and progress charts"),
      tags$li(tags$strong("Detailed View Tab:"), "Examine individual site details and download data"),
      tags$li(tags$strong("Zone Options:"), "Choose how to display P1/P2 zones - separate or combined"),
      tags$li(tags$strong("Grouping:"), "Group data by All MMCD, Facility, FOS, or Section"),
      tags$li(tags$strong("Date Simulation:"), "Use 'Pretend Today is' to see status on any date")
    )
  )
}

# No CSS needed - using inline styles and Shiny built-in components
create_app_css <- function() {
  # Empty function - all styling is done via inline styles
  NULL
}