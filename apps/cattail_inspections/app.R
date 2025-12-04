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

# Source the planned treatment functions
source("planned_treatment_functions.R")

# Source the progress functions
source("progress_functions.R")

# Source the historical comparison functions
source("historical_functions.R")

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
          h4("Progress Summary", style = "font-weight: bold; margin-bottom: 15px;"),
          uiOutput("progressValueBoxes"),
          hr(),
          plotlyOutput("progressPlot", height = "600px")
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
          
          # Refresh button
          actionButton(
            "refresh_historical",
            "Refresh Data",
            icon = icon("refresh"),
            style = "color: #fff; background-color: #28a745; border-color: #28a745; width: 100%; font-weight: bold; margin-top: 10px;"
          )
        ),
        mainPanel(
          h4("Current Year Progress", style = "font-weight: bold; margin-top: 20px; margin-bottom: 15px;"),
          uiOutput("historicalValueBoxes"),
          hr(),
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
  # Progress vs Goal tab - data loads on refresh button
  goal_progress_data <- eventReactive(input$refresh_goal_progress, {
    withProgress(message = "Loading progress data...", value = 0.5, {
      get_progress_data(input$goal_year, input$goal_column, input$custom_today)
    })
  })
  
  output$progressPlot <- renderPlotly({
    data <- goal_progress_data()
    create_progress_plot(data)
  })
  
  # Progress value boxes
  output$progressValueBoxes <- renderUI({
    data <- goal_progress_data()
    
    if (nrow(data) == 0) {
      return(div(style = "text-align: center; padding: 20px;", "No data available"))
    }
    
    # Calculate % complete for each facility
    summary_data <- data %>%
      tidyr::pivot_wider(names_from = type, values_from = count) %>%
      mutate(
        pct_complete = ifelse(Goal > 0, round((`Actual Inspections` / Goal) * 100, 1), 0),
        status_color = case_when(
          pct_complete >= 100 ~ "#28a745",  # green
          pct_complete >= 75 ~ "#ffc107",   # yellow
          pct_complete >= 50 ~ "#fd7e14",   # orange
          TRUE ~ "#dc3545"                   # red
        )
      )
    
    # Create value boxes dynamically
    value_boxes <- lapply(1:nrow(summary_data), function(i) {
      row <- summary_data[i, ]
      
      div(
        class = "col-sm-6 col-md-4 col-lg-3",
        style = "padding: 5px;",
        div(
          style = sprintf(
            "background-color: %s; color: white; padding: 15px; border-radius: 5px; text-align: center; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            row$status_color
          ),
          div(
            style = "font-size: 28px; font-weight: bold; margin-bottom: 5px;",
            sprintf("%.1f%%", row$pct_complete)
          ),
          div(
            style = "font-size: 16px; font-weight: bold; margin-bottom: 3px;",
            row$facility_display
          ),
          div(
            style = "font-size: 12px; opacity: 0.9;",
            sprintf("%d / %d sites", row$`Actual Inspections`, row$Goal)
          )
        )
      )
    })
    
    # Wrap in a row
    div(
      class = "row",
      style = "margin-bottom: 20px;",
      value_boxes
    )
  })
  
  # Historical Comparison tab - data loads on refresh button
  historical_progress_data <- eventReactive(input$refresh_historical, {
    withProgress(message = "Loading historical data...", value = 0.5, {
      get_historical_progress_data(input$hist_years, input$hist_zone, input$hist_facility_filter)
    })
  })
  
  # Historical progress plot - overlaid bars like drone app
  output$historicalProgressPlot <- renderPlot({
    data <- historical_progress_data()
    create_historical_progress_plot(data, input$hist_years, input$hist_metric)
  })
  
  # Historical value boxes showing current year progress with goals
  output$historicalValueBoxes <- renderUI({
    data <- historical_progress_data()
    
    if (nrow(data) == 0) {
      return(div(style = "text-align: center; padding: 20px;", "No data available"))
    }
    
    # Get current year data and calculate % progress vs goals
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    current_data <- data %>%
      filter(type == "Current Year")
    
    # Get goals from database
    con <- get_db_connection()
    if (is.null(con)) {
      return(div(style = "text-align: center; padding: 20px;", "Database connection error"))
    }
    
    goals <- tryCatch({
      dbGetQuery(con, "SELECT facility, p1_totsitecount, p2_totsitecount FROM public.cattail_pctcomplete_base") %>%
        mutate(facility = trimws(facility))
    }, error = function(e) {
      data.frame()
    })
    
    dbDisconnect(con)
    
    if (nrow(goals) == 0) {
      return(div(style = "text-align: center; padding: 20px;", "Goals not available"))
    }
    
    # Determine which zone(s) to show based on hist_zone selection
    if (input$hist_zone == "p1") {
      zone_filter <- "1"
      goal_col <- "p1_totsitecount"
    } else if (input$hist_zone == "p2") {
      zone_filter <- "2"
      goal_col <- "p2_totsitecount"
    } else {
      zone_filter <- c("1", "2")
      goal_col <- NULL  # Will use both
    }
    
    # Filter current data by zone if needed
    if ("zone" %in% names(current_data)) {
      current_data <- current_data %>% filter(zone %in% zone_filter)
    }
    
    # Calculate progress with goals
    if (input$hist_zone == "separate" && "zone" %in% names(current_data)) {
      # Show separate boxes for each facility-zone combination
      summary_data <- current_data %>%
        left_join(goals, by = "facility") %>%
        mutate(
          goal = ifelse(zone == "1", p1_totsitecount, p2_totsitecount),
          pct_complete = ifelse(goal > 0, 
                               round((site_count / goal) * 100, 1), 0),
          display_label = paste0(facility_display, " - P", zone),
          status_color = case_when(
            pct_complete >= 100 ~ "#28a745",  # green
            pct_complete >= 75 ~ "#ffc107",   # yellow
            pct_complete >= 50 ~ "#fd7e14",   # orange
            TRUE ~ "#dc3545"                   # red
          )
        )
    } else if (input$hist_zone == "combined") {
      # Combine P1 and P2 for each facility
      summary_data <- current_data %>%
        group_by(facility, facility_display) %>%
        summarize(
          site_count = sum(site_count, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        left_join(goals, by = "facility") %>%
        mutate(
          goal = p1_totsitecount + p2_totsitecount,
          pct_complete = ifelse(goal > 0, 
                               round((site_count / goal) * 100, 1), 0),
          display_label = facility_display,
          status_color = case_when(
            pct_complete >= 100 ~ "#28a745",
            pct_complete >= 75 ~ "#ffc107",
            pct_complete >= 50 ~ "#fd7e14",
            TRUE ~ "#dc3545"
          )
        )
    } else {
      # Single zone (P1 or P2)
      summary_data <- current_data %>%
        left_join(goals, by = "facility") %>%
        mutate(
          goal = if (input$hist_zone == "p1") p1_totsitecount else p2_totsitecount,
          pct_complete = ifelse(goal > 0, 
                               round((site_count / goal) * 100, 1), 0),
          display_label = facility_display,
          status_color = case_when(
            pct_complete >= 100 ~ "#28a745",
            pct_complete >= 75 ~ "#ffc107",
            pct_complete >= 50 ~ "#fd7e14",
            TRUE ~ "#dc3545"
          )
        )
    }
    
    # Create value boxes
    value_boxes <- lapply(seq_len(nrow(summary_data)), function(i) {
      row <- summary_data[i, ]
      
      div(
        class = "col-sm-6 col-md-4 col-lg-3",
        style = "padding: 5px;",
        div(
          style = sprintf(
            "background-color: %s; color: white; padding: 15px; border-radius: 5px; text-align: center; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            row$status_color
          ),
          div(
            style = "font-size: 28px; font-weight: bold; margin-bottom: 5px;",
            sprintf("%.1f%%", row$pct_complete)
          ),
          div(
            style = "font-size: 16px; font-weight: bold; margin-bottom: 3px;",
            row$display_label
          ),
          div(
            style = "font-size: 12px; opacity: 0.9;",
            sprintf("%d / %d sites", row$site_count, row$goal)
          )
        )
      )
    })
    
    # Wrap in a row
    div(
      class = "row",
      style = "margin-bottom: 20px;",
      value_boxes
    )
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
