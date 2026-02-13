# Catttail inspections App

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(purrr)
  library(tibble)
  library(DT)
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")
source("../../shared/server_utilities.R")

# Source the planned treatment functions
source("planned_treatment_functions.R")

# Source the progress functions
source("progress_functions.R")

# Source the historical comparison functions
source("historical_functions.R")

# Set application name for AWS RDS monitoring
set_app_name("cattail_inspections")

# =============================================================================
# STARTUP OPTIMIZATION: Preload lookup tables into cache
# =============================================================================
message("[cattail_inspections] Preloading lookup tables...")
tryCatch({
  get_facility_lookup()
  get_foremen_lookup()
  message("[cattail_inspections] Lookup tables preloaded")
}, error = function(e) message("[cattail_inspections] Preload warning: ", e$message))

ui <- fluidPage(
  # Use universal CSS from db_helpers for consistent text sizing
  get_universal_text_css(),
  titlePanel("Cattail Inspection Progress and Treatment Planning"),
  
  tabsetPanel(
    # First tab: Progress vs Goal
    tabPanel("Progress vs Goal",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "goal_year",
            "Year:",
            choices = 2010:2026,
            selected = as.numeric(format(Sys.Date(), "%Y"))
          ),
          selectInput(
            "goal_column",
            "Zone Filter:",
            choices = c(
              "P1 + P2 Combined (Total)" = "total",
              "P1 Only (Zone 1)" = "p1",
              "P2 Only (Zone 2)" = "p2",
              "P1 and P2 Separate" = "separate"
            ),
            selected = "total"
          ),
          dateInput(
            "custom_today",
            "Pretend Today is:",
            value = Sys.Date(),
            format = "yyyy-mm-dd"
          ),
          
          selectInput(
            "color_theme_progress",
            "Color Theme:",
            choices = c(
              "MMCD (Default)" = "MMCD",
              "IBM Design" = "IBM",
              "Color-Blind Friendly" = "Wong",
              "Scientific" = "Tol",
              "Viridis" = "Viridis",
              "ColorBrewer" = "ColorBrewer"
            ),
            selected = "MMCD"
          ),
          tags$small(style = "color: #666;", "Changes chart colors"),
          
          # Refresh button
          actionButton(
            "refresh_goal_progress",
            "Refresh Data",
            icon = icon("refresh"),
            style = "color: #fff; background-color: #28a745; border-color: #28a745; width: 100%; font-weight: bold; margin-top: 10px;"
          )
        ),
        mainPanel(
          h4("Progress Summary", style = "font-weight: bold; margin-bottom: 15px;"),
          uiOutput("progressValueBoxes"),
          hr(),
          plotlyOutput("progressPlot", height = "600px"),
          hr(),
          h4("Site Details", style = "font-weight: bold; margin-top: 20px;"),
          div(style = "margin-bottom: 10px;",
            downloadButton("download_progress_sites", "Download CSV", class = "btn-success btn-sm")
          ),
          DT::dataTableOutput("progressSitesTable")
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
              "P1 (Zone 1)" = "p1",
              "P2 (Zone 2)" = "p2",
              "P1 + P2 Combined" = "combined",
              "P1 and P2 Separate" = "separate"
            ),
            selected = "p1"
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
            "hist_metric",
            "Historical Progress Metric:",
            choices = c(
              "Unique Sites Inspected" = "sites",
              "Unique Acres Inspected" = "acres"
            ),
            selected = "sites"
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
          
          selectInput(
            "color_theme_historical",
            "Color Theme:",
            choices = c(
              "MMCD (Default)" = "MMCD",
              "IBM Design" = "IBM",
              "Color-Blind Friendly" = "Wong",
              "Scientific" = "Tol",
              "Viridis" = "Viridis",
              "ColorBrewer" = "ColorBrewer"
            ),
            selected = "MMCD"
          ),
          tags$small(style = "color: #666;", "Changes chart colors"),
          
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
          plotlyOutput("historicalProgressPlot", height = "500px"),
          hr(),
          h4("Site Inspection Details", style = "font-weight: bold; margin-top: 20px;"),
          div(style = "margin-bottom: 10px;",
            downloadButton("download_sites_data", "Download CSV", class = "btn-success btn-sm")
          ),
          DT::dataTableOutput("sitesTable")
        )
      )
    )
    
    # NOTE: Treatment Planning tab commented out - this functionality will be moved to the treatment progress app
    # Third tab: Treatment Planning
    # tabPanel("Treatment Planning",
    #   sidebarLayout(
    #     sidebarPanel(
    #       # Radio buttons to select view type
    #       radioButtons(
    #         "view_type",
    #         "View By:",
    #         choices = c("Acres" = "acres", "Number of Sites" = "sites"),
    #         selected = "acres"
    #       ),
    #       
    #       # Dropdown to select facility
    #       selectInput(
    #         "facility",
    #         "Select Facility:",
    #         choices = get_facility_choices(),
    #         selected = "all"
    #       ),
    #       
    #       # Checkboxes to select treatment plan types
    #       checkboxGroupInput(
    #         "plan_types",
    #         "Select Treatment Plan Types:",
    #         choices = get_treatment_plan_choices(),
    #         selected = c("A", "D", "G", "N", "U")
    #       ),
    #       
    #       # Refresh button
    #       actionButton(
    #         "refresh_treatment",
    #         "Refresh Data",
    #         icon = icon("refresh"),
    #         style = "color: #fff; background-color: #28a745; border-color: #28a745; width: 100%; font-weight: bold; margin-top: 10px;"
    #       )
    #     ),
    #     
    #     # Main panel for displaying the graph and table
    #     mainPanel(
    #       plotOutput("treatmentGraph", height = "600px"),
    #       hr(),
    #       h4("Site Details", style = "font-weight: bold; margin-top: 20px;"),
    #       div(style = "margin-bottom: 10px;",
    #         downloadButton("download_treatment_details", "Download CSV", class = "btn-warning btn-sm")
    #       ),
    #       DT::dataTableOutput("siteDetailsTable")
    #     )
    #   )
    # )
  )
)

server <- function(input, output) {
  # Reactive theme values for each tab
  current_theme_progress <- reactive({
    input$color_theme_progress
  })
  
  current_theme_historical <- reactive({
    input$color_theme_historical
  })
  
  # Update global theme option when theme changes
  observeEvent(input$color_theme_progress, {
    options(mmcd.color.theme = input$color_theme_progress)
  })
  
  observeEvent(input$color_theme_historical, {
    options(mmcd.color.theme = input$color_theme_historical)
  })
  
  # Progress vs Goal tab - data loads on refresh button
  goal_progress_data <- eventReactive(input$refresh_goal_progress, {
    withProgress(message = "Loading progress data...", value = 0.5, {
      get_progress_data(input$goal_year, input$goal_column, input$custom_today)
    })
  })
  
  output$progressPlot <- renderPlotly({
    data <- goal_progress_data()
    create_progress_plot(data, zone_option = input$goal_column, theme = current_theme_progress())
  })
  
  # Get site details for progress data
  progress_sites_data <- eventReactive(input$refresh_goal_progress, {
    withProgress(message = "Loading site details...", value = 0.5, {
      get_progress_sites_detail(input$goal_year, input$goal_column, input$custom_today)
    })
  })
  
  # Progress sites table
  output$progressSitesTable <- DT::renderDataTable({
    site_data <- progress_sites_data()
    
    if (nrow(site_data) == 0) {
      return(data.frame(Message = "No site data available."))
    }
    
    # Rename columns for display - include zone when viewing both or separate
    if (input$goal_column %in% c("total", "separate") && "zone_label" %in% names(site_data)) {
      display_data <- site_data %>%
        select(
          `Site Code` = sitecode,
          Facility = facility,
          Zone = zone_label,
          `Inspection Date` = inspdate,
          Wet = wet,
          `Num Dip` = numdip,
          Acres = acres
        )
    } else {
      display_data <- site_data %>%
        select(
          `Site Code` = sitecode,
          Facility = facility,
          `Inspection Date` = inspdate,
          Wet = wet,
          `Num Dip` = numdip,
          Acres = acres
        )
    }
    
    # Link sitecodes to data.mmcd.org map
    display_data$`Site Code` <- make_sitecode_link(display_data$`Site Code`)
    
    DT::datatable(
      display_data,
      escape = FALSE,
      options = list(
        pageLength = 25,
        order = list(list(1, 'asc'), list(2, 'desc')),
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  # Download handler for progress sites
  output$download_progress_sites <- downloadHandler(
    filename = function() {
      sprintf("cattail_progress_sites_%s.csv", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      site_data <- get_progress_sites_detail(input$goal_year, input$goal_column, input$custom_today)
      if (nrow(site_data) > 0) {
        result <- export_csv_safe(site_data, file, clean_data = TRUE)
        if (!result$success) {
          warning(result$message)
        }
      }
    }
  )
  
  # Progress value boxes - compact display
  output$progressValueBoxes <- renderUI({
    data <- goal_progress_data()
    zone_option <- input$goal_column
    
    if (nrow(data) == 0) {
      return(div(style = "text-align: center; padding: 10px;", "No data available"))
    }
    
    # Helper function to get status color
    get_status_color <- function(pct) {
      if (pct >= 100) "#28a745"       # green
      else if (pct >= 75) "#ffc107"   # yellow
      else if (pct >= 50) "#fd7e14"   # orange
      else "#dc3545"                   # red
    }
    
    # Zone label for display
    zone_label <- switch(zone_option, 
      "total" = "Total",
      "p1" = "P1",
      "p2" = "P2",
      "separate" = NULL,  # Will show P1/P2 in box
      "Total"
    )
    
    if (zone_option == "separate") {
      # For "separate" - show P1 | P2 in each box
      summary_data <- data %>%
        group_by(facility_display) %>%
        summarise(
          p1_goal = sum(goal[zone == "P1"], na.rm = TRUE),
          p1_actual = sum(actual[zone == "P1"], na.rm = TRUE),
          p2_goal = sum(goal[zone == "P2"], na.rm = TRUE),
          p2_actual = sum(actual[zone == "P2"], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          p1_pct = ifelse(p1_goal > 0, round((p1_actual / p1_goal) * 100, 0), 0),
          p2_pct = ifelse(p2_goal > 0, round((p2_actual / p2_goal) * 100, 0), 0),
          total_pct = ifelse(p1_goal + p2_goal > 0, 
                             round(((p1_actual + p2_actual) / (p1_goal + p2_goal)) * 100, 0), 0)
        )
      
      # Create compact two-line boxes
      value_boxes <- lapply(1:nrow(summary_data), function(i) {
        row <- summary_data[i, ]
        div(
          class = "col-sm-4 col-md-3 col-lg-2",
          style = "padding: 3px;",
          div(
            style = sprintf(
              "background-color: %s; color: white; padding: 8px; border-radius: 4px; text-align: center;",
              get_status_color(row$total_pct)
            ),
            div(style = "font-size: 11px; font-weight: bold; margin-bottom: 2px;", row$facility_display),
            div(style = "font-size: 10px;",
              sprintf("P1: %d%% | P2: %d%%", row$p1_pct, row$p2_pct)
            )
          )
        )
      })
    } else {
      # Single metric (total, p1, or p2) - compact boxes
      summary_data <- data %>%
        tidyr::pivot_wider(names_from = type, values_from = count) %>%
        mutate(
          pct_complete = ifelse(Goal > 0, round((`Actual Inspections` / Goal) * 100, 0), 0)
        )
      
      value_boxes <- lapply(1:nrow(summary_data), function(i) {
        row <- summary_data[i, ]
        div(
          class = "col-sm-4 col-md-3 col-lg-2",
          style = "padding: 3px;",
          div(
            style = sprintf(
              "background-color: %s; color: white; padding: 8px; border-radius: 4px; text-align: center;",
              get_status_color(row$pct_complete)
            ),
            div(style = "font-size: 11px; font-weight: bold; margin-bottom: 2px;", row$facility_display),
            div(style = "font-size: 14px; font-weight: bold;", sprintf("%d%%", row$pct_complete)),
            div(style = "font-size: 9px; opacity: 0.9;", 
                sprintf("%d/%d %s", row$`Actual Inspections`, row$Goal, zone_label))
          )
        )
      })
    }
    
    # All facilities in a single row
    div(
      class = "row",
      style = "margin-bottom: 10px;",
      value_boxes
    )
  })
  
  # Historical Comparison tab - data loads on refresh button
  historical_progress_data <- eventReactive(input$refresh_historical, {
    withProgress(message = "Loading historical data...", value = 0.5, {
      get_historical_progress_data(input$hist_years, input$hist_zone, input$hist_facility_filter)
    })
  })
  
  # Historical progress plot - uses shared create_trend_chart
  output$historicalProgressPlot <- renderPlotly({
    data <- historical_progress_data()
    create_historical_progress_plot(data, input$hist_years, input$hist_metric, theme = current_theme_historical())
  })
  
  # Sites table data - sites inspected in last X years (with toggle for unchecked this year)
  sites_table_data <- eventReactive(input$refresh_historical, {
    withProgress(message = "Loading sites data...", value = 0.5, {
      get_sites_table_data(input$hist_years, input$hist_zone, input$hist_facility_filter, input$sites_view_type)
    })
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
        Acres = acres
      )
    
    # Link sitecodes to data.mmcd.org map
    display_data$`Site Code` <- make_sitecode_link(display_data$`Site Code`)
    
    DT::datatable(
      display_data,
      escape = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(1, 'asc'))
      ),
      rownames = FALSE
    )
  })
  
  # NOTE: Treatment Planning server code commented out - functionality to be moved to treatment progress app
  # Treatment Planning - fetch data only on refresh
  # treatment_data <- eventReactive(input$refresh_treatment, {
  #   get_treatment_plan_data()
  # })
  # 
  # output$treatmentGraph <- renderPlot({
  #   data <- treatment_data()
  #   create_treatment_plan_plot_with_data(data, input$facility, input$plan_types, input$view_type)
  # })
  # 
  # # Site details table
  # output$siteDetailsTable <- DT::renderDataTable({
  #   # Fetch site data only when refresh is clicked
  #   req(input$refresh_treatment)
  #   
  #   site_data <- get_site_details_data(input$facility, input$plan_types)
  #   
  #   if (nrow(site_data) == 0) {
  #     return(data.frame(Message = "No site data available for the selected filters."))
  #   }
  #   
  #   # Select and rename columns for display - only relevant columns
  #   display_data <- site_data %>%
  #     select(
  #       `Site Code` = sitecode,
  #       Facility = facility,
  #       `Plan Type` = plan_name,
  #       `Inspection Date` = inspdate,
  #       Wet = wet,
  #       `Num Dip` = numdip,
  #       Acres = acres,
  #       `Acres Plan` = acres_plan
  #     )
  #   
  #   DT::datatable(
  #     display_data,
  #     options = list(
  #       pageLength = 15,
  #       scrollX = TRUE,
  #       order = list(list(1, 'asc'), list(2, 'asc'))
  #     ),
  #     rownames = FALSE
  #   )
  # })
  
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
  
  # NOTE: Treatment Planning download handler commented out - functionality to be moved to treatment progress app
  # output$download_treatment_details <- downloadHandler(
  #   filename = function() {
  #     paste("cattail_treatment_details_", Sys.Date(), ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     tryCatch({
  #       site_data <- get_site_details_data(input$facility, input$plan_types)
  #       
  #       if (is.null(site_data) || nrow(site_data) == 0) {
  #         write.csv(data.frame(Message = "No treatment details available"), file, row.names = FALSE)
  #       } else {
  #         result <- export_csv_safe(site_data, file, clean_data = TRUE)
  #         if (!result$success) {
  #           write.csv(site_data, file, row.names = FALSE, na = "")
  #         }
  #       }
  #     }, error = function(e) {
  #       write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
  #     })
  #   }
  # )
}

shinyApp(ui = ui, server = server)
