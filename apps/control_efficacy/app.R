# Control Efficacy App
# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(DT)
  library(plotly)
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")
source("../../shared/stat_box_helpers.R")

# Source external function files
source("ui_helper.R")
source("data_functions.R")
source("checkback_functions.R")
source("display_functions.R")

# Suppress R CMD check notes for dplyr/ggplot2 NSE variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "facility", "inspdate", "date_diff", "round_start", "round_id", "sitecode", 
    "acres", "start_date", "checkback_count", "last_treatment_date", 
    "next_treatment_date", "checkback", "last_treatment", "daily_summary", 
    "sites_treated", "timing_df", "days_to_checkback", "facility_display"
  ))
}

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- control_efficacy_ui()

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # Show help modal when help link is clicked
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Control Efficacy App - Help",
      size = "l",
      easyClose = TRUE,
      
      h3("Key Definitions"),
      tags$dl(
        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Brood (Treatment Round)"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;", 
          "A group of consecutive treatment days at the same facility. Consecutive means a maximum of 1 day gap between treatments. Each brood is identified by facility and start date (e.g., 'BBF-06/14')."
        ),
        
        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Air Treatment"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;", 
          "A treatment record with action code 'A' from the database. These are the treatments that require checkback inspections."
        ),
        
        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Checkback Inspection"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;", 
          "An inspection conducted after a treatment to assess its effectiveness. Valid checkbacks have:",
          tags$ul(
            tags$li("Action code '4' in the database"),
            tags$li("Non-null posttrt_p value (post-treatment dip count recorded)"),
            tags$li("Occurred after the treatment date"),
            tags$li("Not invalidated by a subsequent treatment at the same site")
          )
        ),
        
        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Valid vs Invalid Checkbacks"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;", 
          "A checkback is only valid if no new treatment occurred at that site between the original treatment and the checkback. If a site is re-treated, any checkbacks before the next treatment are counted for the first brood, but checkbacks after the re-treatment belong to the new brood."
        )
      ),
      
      h3(style = "margin-top: 25px;", "Calculations"),
      tags$dl(
        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Checkbacks Needed"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;", 
          tags$b("Percentage Mode:"), " ceiling(unique_sites_treated × percentage / 100)",
          tags$br(),
          "Example: 105 sites treated × 10% = 10.5, rounded up = 11 checkbacks needed",
          tags$br(), tags$br(),
          tags$b("Fixed Number Mode:"), " min(fixed_number, unique_sites_treated)",
          tags$br(),
          "Example: Fixed number = 10, but only 8 sites treated = 8 checkbacks needed"
        ),
        
        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Completion Rate"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;", 
          "(checkbacks_completed / checkbacks_needed) × 100",
          tags$br(),
          "Example: 7 completed / 11 needed = 63.6%"
        ),
        
        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Days to Checkback"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;", 
          "Number of days between the last treatment date at a site and its checkback inspection date. Averaged across all checkbacks for a brood."
        ),
        
        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Brood Duration"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;", 
          "(end_date - start_date) + 1",
          tags$br(),
          "Example: Treatments from June 14-16 = 3 days duration"
        ),
        
        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Control Efficacy (% Reduction)"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;", 
          "((pre_treatment_dips - post_treatment_dips) / pre_treatment_dips) × 100",
          tags$br(),
          "Example: Pre = 50 dips, Post = 10 dips → (50-10)/50 = 80% reduction"
        )
      ),
      
      h3(style = "margin-top: 25px;", "Data Sources"),
      tags$dl(
        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Database Tables"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;", 
          tags$ul(
            tags$li(tags$b("dblarv_insptrt_current:"), " Current year treatments and inspections"),
            tags$li(tags$b("dblarv_insptrt_archive:"), " Historical data from previous years"),
            tags$li(tags$b("gis_sectcode:"), " Facility and zone information")
          ),
          "The app intelligently queries only the needed table(s) based on your selected date range."
        )
      ),
      
      footer = modalButton("Close")
    ))
  })
  
  # Initialize UI choices on app load - NO DATA QUERIES, ONLY FILTER CHOICES
  observe({
    facility_choices <- get_facility_choices()
    updateSelectInput(session, "facility_filter_progress", choices = facility_choices, selected = "all")
    updateSelectInput(session, "site_facility_filter", choices = facility_choices, selected = "all")
  })
  
  # Update material code choices on app load
  observe({
    matcodes <- get_treatment_material_choices()
    updateSelectInput(session, "matcode_filter_progress", choices = matcodes, selected = "all")
  })
  
  # Update species filter choices on app load from db_helpers
  observe({
    species_choices <- get_species_choices(include_all = TRUE)
    # Add special options for missing data
    species_choices <- c(species_choices, 
                        "Sample Missing" = "[Sample Missing]",
                        "No Species Data" = "[No Species Data]")
    updateSelectInput(session, "species_filter_progress", choices = species_choices, selected = "all")
  })
  
  # ===== PROGRESS TAB REACTIVES =====
  treatment_data_progress <- eventReactive(input$refresh_data_progress, {
    req(input$date_range_progress, input$facility_filter_progress, input$matcode_filter_progress)
    
    withProgress(message = "Loading treatment data...", value = 0.5, {
      load_treatment_data(
        start_date = as.character(input$date_range_progress[1]),
        end_date = as.character(input$date_range_progress[2]),
        facility_filter = input$facility_filter_progress,
        matcode_filter = input$matcode_filter_progress
      )
    })
  })
  
  checkback_data_progress <- eventReactive(input$refresh_data_progress, {
    req(treatment_data_progress())
    
    withProgress(message = "Loading checkback data...", value = 0.5, {
      treatments <- treatment_data_progress()
      if (is.null(treatments) || nrow(treatments) == 0) return(NULL)
      
      treated_sites <- unique(treatments$sitecode)
      load_checkback_data(
        treated_sites = treated_sites,
        start_date = as.character(input$date_range_progress[1]),
        end_date = as.character(input$date_range_progress[2])
      )
    })
  })
  
  treatment_rounds_progress <- eventReactive(input$refresh_data_progress, {
    req(treatment_data_progress())
    
    withProgress(message = "Calculating treatment rounds...", value = 0.5, {
      calculate_treatment_rounds(
        treatments = treatment_data_progress(),
        checkback_type = input$checkback_type_progress,
        checkback_percent = input$checkback_percent_progress,
        checkback_number = input$checkback_number_progress
      )
    })
  })
  
  checkback_status_progress <- eventReactive(input$refresh_data_progress, {
    req(treatment_rounds_progress())
    
    withProgress(message = "Calculating checkback status...", value = 0.5, {
      calculate_checkback_status(
        rounds = treatment_rounds_progress(),
        checkbacks = checkback_data_progress(),
        treatments = treatment_data_progress()
      )
    })
  })
  
  # Progress Tab Outputs
  output$checkback_progress_chart <- renderPlotly({
    input$color_theme_progress  # Reactive dependency
    status <- checkback_status_progress()
    
    result <- create_checkback_progress_chart(
      checkback_status = status,
      theme = input$color_theme_progress
    )
    
    # Return the plot (result is now a list with $plot and $height)
    if (is.list(result)) result$plot else result
  })
  
  # Dynamic height for checkback progress chart
  output$checkback_progress_chart_ui <- renderUI({
    status <- checkback_status_progress()
    result <- create_checkback_progress_chart(
      checkback_status = status,
      theme = input$color_theme_progress
    )
    
    # Get height from result (30px per bar for spacing, min 300px)
    height <- if (is.list(result)) result$height else 400
    
    plotlyOutput("checkback_progress_chart", height = paste0(height, "px"))
  })
  
  output$total_checkbacks_needed <- renderUI({
    status <- checkback_status_progress()
    total <- if (!is.null(status)) sum(status$checkbacks_needed, na.rm = TRUE) else 0
    
    status_colors <- get_status_colors(theme = input$color_theme_progress)
    create_stat_box(
      value = total,
      title = "Total Checkbacks Needed",
      bg_color = status_colors["unknown"],
      icon = icon("clipboard-list")
    )
  })
  
  output$total_checkbacks_completed <- renderUI({
    status <- checkback_status_progress()
    completed <- if (!is.null(status)) sum(status$checkbacks_completed, na.rm = TRUE) else 0
    
    status_colors <- get_status_colors(theme = input$color_theme_progress)
    create_stat_box(
      value = completed,
      title = "Total Checkbacks Completed",
      bg_color = status_colors["active"],
      icon = icon("check-circle")
    )
  })
  
  output$overall_completion_rate <- renderUI({
    status <- checkback_status_progress()
    status_colors <- get_status_colors(theme = input$color_theme_progress)
    
    if (!is.null(status)) {
      needed <- sum(status$checkbacks_needed, na.rm = TRUE)
      completed <- sum(status$checkbacks_completed, na.rm = TRUE)
      rate <- if (needed > 0) round(completed / needed * 100, 1) else 0
      # Use theme-aware colors based on completion rate
      color <- if (rate >= 80) {
        status_colors["active"]          # Green for good completion
      } else if (rate >= 50) {
        status_colors["needs_action"]    # Orange for medium completion
      } else {
        status_colors["needs_treatment"] # Red for poor completion
      }
    } else {
      rate <- 0
      color <- status_colors["needs_treatment"]
    }
    
    create_stat_box(
      value = paste0(rate, "%"),
      title = "Overall Completion Rate",
      bg_color = color,
      icon = icon("percentage")
    )
  })
  
  output$avg_days_to_checkback <- renderUI({
    status <- checkback_status_progress()
    avg <- if (!is.null(status)) round(mean(status$avg_days_to_checkback, na.rm = TRUE), 1) else 0
    
    status_colors <- get_status_colors(theme = input$color_theme_progress)
    create_stat_box(
      value = avg,
      title = "Avg Days to Checkback",
      bg_color = status_colors["planned"],
      icon = icon("calendar-day")
    )
  })
  
  # ===== SHARED DATA (used by Status and Details tabs) =====
  # These use the Progress tab filters
  
  # Load species data for checkbacks
  species_data_for_checkbacks <- eventReactive(input$refresh_data_progress, {
    req(checkback_data_progress())
    
    withProgress(message = 'Loading species data...', value = 0, {
      load_species_data_for_checkbacks(
        checkbacks = checkback_data_progress(),
        start_date = input$date_range_progress[1],
        end_date = input$date_range_progress[2]
      )
    })
  })
  
  site_details <- eventReactive(input$refresh_data_progress, {
    req(treatment_data_progress())
    
    create_site_details(
      treatments = treatment_data_progress(),
      checkbacks = checkback_data_progress(),
      species_data = species_data_for_checkbacks()
    )
  })
  
  # Update facility choices for site details filter
  observe({
    details <- site_details()
    if (!is.null(details)) {
      facilities <- c("all", sort(unique(details$facility)))
      updateSelectInput(session, "site_facility_filter", choices = facilities, selected = "all")
    }
  })
  

  
  # Filtered site details based on user selections
  filtered_site_details <- reactive({
    details <- site_details()
    
    if (is.null(details)) return(NULL)
    
    # Apply filters
    filtered <- details
    
    # Facility filter
    if (!is.null(input$site_facility_filter) && input$site_facility_filter != "all") {
      filtered <- filtered %>% filter(facility == input$site_facility_filter)
    }
    
    # Min acres filter
    if (!is.null(input$site_min_acres) && input$site_min_acres > 0) {
      filtered <- filtered %>% filter(acres >= input$site_min_acres)
    }
    
    # Min days to checkback filter
    if (!is.null(input$site_min_days) && input$site_min_days > 0) {
      filtered <- filtered %>% filter(days_to_checkback >= input$site_min_days)
    }
    
    # Species filter
    if (!is.null(input$species_filter_progress) && input$species_filter_progress != "all") {
      filtered <- filtered %>% filter(grepl(input$species_filter_progress, species_composition, fixed = TRUE))
    }
    
    return(filtered)
  })
  
  # ===== STATUS TAB OUTPUTS =====
  # Brood Status Table (used in Status Tables tab)
  output$checkback_status_table <- DT::renderDataTable({
    status <- checkback_status_progress()
    
    if (is.null(status)) {
      return(data.frame(Message = "No data available"))
    }
    
    display_data <- status %>%
      select(
        Brood = round_name,
        Facility = facility,
        `Start Date` = start_date,
        `End Date` = end_date,
        `Sites Treated` = sites_treated,
        `Checkbacks Needed` = checkbacks_needed,
        `Checkbacks Completed` = checkbacks_completed,
        `Completion Rate (%)` = completion_rate,
        `Avg Days to Checkback` = avg_days_to_checkback
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Completion Rate (%)",
        backgroundColor = styleInterval(
          c(10, 20, 30, 40, 50, 60, 70, 80, 90),
          c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850", "#006837")
        )
      )
  })
  
  # Brood Status Table (legacy output for backward compatibility)
  output$facility_status <- DT::renderDataTable({
    status <- checkback_status_progress()
    
    if (is.null(status)) {
      return(data.frame(Message = "No data available"))
    }
    
    display_data <- status %>%
      select(
        Brood = round_name,
        Facility = facility,
        `Start Date` = start_date,
        `End Date` = end_date,
        `Sites Treated` = sites_treated,
        `Checkbacks Needed` = checkbacks_needed,
        `Checkbacks Completed` = checkbacks_completed,
        `Completion Rate (%)` = completion_rate,
        `Avg Days to Checkback` = avg_days_to_checkback
      )
    
    datatable(display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Completion Rate (%)",
        backgroundColor = styleInterval(
          c(10, 20, 30, 40, 50, 60, 70, 80, 90),
          c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850", "#006837")
        )
      )
  })
  
  # All checkbacks table (uses progress tab data)
  output$all_checkbacks_table <- DT::renderDataTable({
    details <- site_details()
    
    if (is.null(details)) {
      return(data.frame(Message = "No checkback data available"))
    }
    
    display_data <- details %>%
      mutate(
        `% Reduction` = ifelse(
          is.na(pre_treatment_dips) | is.na(post_treatment_dips) | pre_treatment_dips == 0,
          NA,
          round(((pre_treatment_dips - post_treatment_dips) / pre_treatment_dips) * 100, 1)
        )
      ) %>%
      select(
        Site = sitecode,
        Facility = facility,
        `Inspection Date` = inspection_date,
        `Treatment Date` = treatment_date,
        `Checkback Date` = checkback_date,
        `Days to Checkback` = days_to_checkback,
        `Pre Dips` = pre_treatment_dips,
        `Post Dips` = post_treatment_dips,
        `% Reduction`,
        Acres = acres
      )
    
    datatable(display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        "% Reduction",
        backgroundColor = styleInterval(
          c(-50, 0, 25, 50, 75, 90, 95),
          c("#67001f", "#d73027", "#f46d43", "#fee08b", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850")
        )
      )
  })
  
  # Reactive value to track filter state for dip changes chart
  dip_filter_mode <- reactiveVal("all")
  
  observeEvent(input$show_recent_only, {
    dip_filter_mode("recent")
  })
  
  observeEvent(input$show_all_broods, {
    dip_filter_mode("all")
  })
  
  # Filtered site details for dip changes chart
  filtered_site_details_for_dip <- reactive({
    details <- site_details()
    
    if (is.null(details) || nrow(details) == 0) return(NULL)
    
    # All rows in site_details already have checkbacks (it's per-checkback now)
    details_with_checkback <- details
    
    if (dip_filter_mode() == "recent") {
      # Get the most recent treatment date from the data itself
      if (nrow(details_with_checkback) > 0) {
        most_recent_date <- max(details_with_checkback$treatment_date, na.rm = TRUE)
        
        # Filter to only checkbacks from the most recent treatment date
        details_with_checkback <- details_with_checkback %>%
          filter(treatment_date >= most_recent_date)
      }
    }
    
    return(details_with_checkback)
  })
  
  output$dip_changes_plot <- renderPlotly({
    input$color_theme_progress  # Reactive dependency
    input$show_recent_only  # Reactive dependency
    input$show_all_broods  # Reactive dependency
    
    withProgress(message = "Rendering dip changes chart...", value = 0.5, {
      create_dip_changes_chart(
        site_details = filtered_site_details_for_dip(),
        theme = input$color_theme_progress
      )
    })
  })
  
  # ===== SITE DETAILS TAB OUTPUTS =====
  output$site_details <- DT::renderDataTable({
    details <- filtered_site_details()
    
    if (is.null(details) || nrow(details) == 0) {
      return(data.frame(Message = "No data available with current filters"))
    }
    
    display_data <- details %>%
      select(
        Site = sitecode,
        Facility = facility,
        `Inspection Date` = inspection_date,
        `Treatment Date` = treatment_date,
        `Checkback Date` = checkback_date,
        `Pre-Trt Dips` = pre_treatment_dips,
        `Post-Trt Dips` = post_treatment_dips,
        Acres = acres,
        `Mat Code` = matcode,
        `Material Type` = mattype,
        `Effect Days` = effect_days,
        `Days to Checkback` = days_to_checkback,
        `Red/Blue` = redblue,
        `Species Composition` = species_composition
      ) %>%
      mutate(
        Acres = round(Acres, 1)
      )
    
    datatable(display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
