# Catttail Status App

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(tibble)
  library(DT)
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")

# Source the planned treatment functions
source("planned_treatment_functions.R")

# Source the progress functions
source("progress_functions.R")

# Source the historical comparison functions
source("historical_functions.R")

ui <- fluidPage(
  titlePanel("Cattail Inspection Progress and Treatment Planning"),
  
  tabsetPanel(
    # First tab: Progress vs Goal
    tabPanel("Progress vs Goal",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "goal_year",
            "Year:",
            choices = 2010:2025,
            selected = as.numeric(format(Sys.Date(), "%Y"))
          ),
          selectInput(
            "goal_column",
            "Goal Type:",
            choices = c(
              "P1 Goal (set in current year)" = "p1_totsitecount",
              "P2 Goal (set in current year)" = "p2_totsitecount"
            ),
            selected = "p1_totsitecount"
          ),
          dateInput(
            "custom_today",
            "Pretend Today is:",
            value = Sys.Date(),
            format = "yyyy-mm-dd"
          ),
          
          # Refresh button
          actionButton(
            "refresh_goal_progress",
            "Refresh Data",
            icon = icon("refresh"),
            style = "color: #fff; background-color: #28a745; border-color: #28a745; width: 100%; font-weight: bold; margin-top: 10px;"
          )
        ),
        mainPanel(
          plotOutput("progressPlot", height = "600px")
        )
      )
    ),
    
    # Second tab: Historical Comparison
    tabPanel("Historical Comparison",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "hist_zone",
            "Zone:",
            choices = c(
              "P1 (Zone 1)" = "1",
              "P2 (Zone 2)" = "2"
            ),
            selected = "1"
          ),
          
          numericInput(
            "hist_years",
            "Compare to previous X years:",
            value = 3,
            min = 1,
            max = 10,
            step = 1
          ),
          
          selectizeInput(
            "hist_facility_filter",
            "Filter Sites by Facility:",
            choices = get_facility_choices(),
            selected = "all",
            multiple = TRUE
          ),
          
          radioButtons(
            "sites_view_type",
            "Sites Table View:",
            choices = c(
              "All sites in last X years" = "all",
              "Sites NOT checked this year" = "unchecked"
            ),
            selected = "all"
          ),
          
          # Refresh button
          actionButton(
            "refresh_historical",
            "Refresh Data",
            icon = icon("refresh"),
            style = "color: #fff; background-color: #28a745; border-color: #28a745; width: 100%; font-weight: bold; margin-top: 10px;"
          )
        ),
        mainPanel(
          h4("Historical Progress Comparison", style = "font-weight: bold; margin-top: 20px;"),
          plotOutput("historicalProgressPlot", height = "500px"),
          hr(),
          h4("Site Inspection Details", style = "font-weight: bold; margin-top: 20px;"),
          div(style = "margin-bottom: 10px;",
            downloadButton("download_sites_data", "Download CSV", class = "btn-success btn-sm")
          ),
          DT::dataTableOutput("sitesTable")
        )
      )
    ),
    
    # Third tab: Treatment Planning
    tabPanel("Treatment Planning",
      sidebarLayout(
        sidebarPanel(
          # Radio buttons to select view type
          radioButtons(
            "view_type",
            "View By:",
            choices = c("Acres" = "acres", "Number of Sites" = "sites"),
            selected = "acres"
          ),
          
          # Dropdown to select facility
          selectInput(
            "facility",
            "Select Facility:",
            choices = get_facility_choices(),
            selected = "all"
          ),
          
          # Checkboxes to select treatment plan types
          checkboxGroupInput(
            "plan_types",
            "Select Treatment Plan Types:",
            choices = get_treatment_plan_choices(),
            selected = c("A", "D", "G", "N", "U")
          ),
          
          # Refresh button
          actionButton(
            "refresh_treatment",
            "Refresh Data",
            icon = icon("refresh"),
            style = "color: #fff; background-color: #28a745; border-color: #28a745; width: 100%; font-weight: bold; margin-top: 10px;"
          )
        ),
        
        # Main panel for displaying the graph and table
        mainPanel(
          plotOutput("treatmentGraph", height = "600px"),
          hr(),
          h4("Site Details", style = "font-weight: bold; margin-top: 20px;"),
          div(style = "margin-bottom: 10px;",
            downloadButton("download_treatment_details", "Download CSV", class = "btn-warning btn-sm")
          ),
          DT::dataTableOutput("siteDetailsTable")
        )
      )
    )
  )
)

server <- function(input, output) {
  # Progress vs Goal tab - data loads on refresh button
  goal_progress_data <- eventReactive(input$refresh_goal_progress, {
    get_progress_data(input$goal_year, input$goal_column, input$custom_today)
  })
  
  output$progressPlot <- renderPlot({
    data <- goal_progress_data()
    create_progress_plot(data)
  })
  
  # Historical Comparison tab - data loads on refresh button
  historical_progress_data <- eventReactive(input$refresh_historical, {
    get_historical_progress_data(input$hist_years, input$hist_zone, input$hist_facility_filter)
  })
  
  # Historical progress plot - overlaid bars like drone app
  output$historicalProgressPlot <- renderPlot({
    data <- historical_progress_data()
    create_historical_progress_plot(data, input$hist_years)
  })
  
  # Sites table data - sites inspected in last X years (with toggle for unchecked this year)
  sites_table_data <- eventReactive(input$refresh_historical, {
    get_sites_table_data(input$hist_years, input$hist_zone, input$hist_facility_filter, input$sites_view_type)
  })
  
  # Sites table output
  output$sitesTable <- DT::renderDataTable({
    site_data <- sites_table_data()
    
    if (nrow(site_data) == 0) {
      return(data.frame(Message = "No site data available."))
    }
    
    # Rename columns for display
    display_data <- site_data %>%
      select(
        `Site Code` = sitecode,
        Facility = facility,
        `Last Inspection` = last_inspection,
        Wet = wet,
        `Num Dip` = numdip,
        Acres = acres,
        `Acres Plan` = acres_plan
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(1, 'asc'))
      ),
      rownames = FALSE
    )
  })
  
  # Treatment Planning - fetch data only on refresh
  treatment_data <- eventReactive(input$refresh_treatment, {
    get_treatment_plan_data()
  })
  
  output$treatmentGraph <- renderPlot({
    data <- treatment_data()
    create_treatment_plan_plot_with_data(data, input$facility, input$plan_types, input$view_type)
  })
  
  # Site details table
  output$siteDetailsTable <- DT::renderDataTable({
    # Fetch site data only when refresh is clicked
    req(input$refresh_treatment)
    
    site_data <- get_site_details_data(input$facility, input$plan_types)
    
    if (nrow(site_data) == 0) {
      return(data.frame(Message = "No site data available for the selected filters."))
    }
    
    # Select and rename columns for display - only relevant columns
    display_data <- site_data %>%
      select(
        `Site Code` = sitecode,
        Facility = facility,
        `Plan Type` = plan_name,
        `Inspection Date` = inspdate,
        Wet = wet,
        `Num Dip` = numdip,
        Acres = acres,
        `Acres Plan` = acres_plan
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(1, 'asc'), list(2, 'asc'))
      ),
      rownames = FALSE
    )
  })
  
  # Download handlers for CSV exports
  output$download_sites_data <- downloadHandler(
    filename = function() {
      paste("cattail_sites_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      tryCatch({
        site_data <- get_sites_table_data(input$hist_years, input$hist_zone, input$hist_facility_filter, input$sites_view_type)
        
        if (is.null(site_data) || nrow(site_data) == 0) {
          write.csv(data.frame(Message = "No sites data available"), file, row.names = FALSE)
        } else {
          result <- export_csv_safe(site_data, file, clean_data = TRUE)
          if (!result$success) {
            write.csv(site_data, file, row.names = FALSE, na = "")
          }
        }
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )
  
  output$download_treatment_details <- downloadHandler(
    filename = function() {
      paste("cattail_treatment_details_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      tryCatch({
        site_data <- get_site_details_data(input$facility, input$plan_types)
        
        if (is.null(site_data) || nrow(site_data) == 0) {
          write.csv(data.frame(Message = "No treatment details available"), file, row.names = FALSE)
        } else {
          result <- export_csv_safe(site_data, file, clean_data = TRUE)
          if (!result$success) {
            write.csv(site_data, file, row.names = FALSE, na = "")
          }
        }
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )
}

shinyApp(ui = ui, server = server)
