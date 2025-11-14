# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(DT)
  library(plotly)
  library(tidyr)
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")

# Source external function files
source("data_functions.R")
source("display_functions.R")
source("ui_helpers.R")

ui <- dashboardPage(
  dashboardHeader(title = "Ground Prehatch Treatment Progress"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Progress Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Detailed View", tabName = "details", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    # Filter panel - always visible
    create_filter_panel(),
    
    # Help text (collapsible)
    div(id = "help-section",
      tags$a(href = "#", onclick = "$(this).next().toggle(); return false;", 
             style = "color: #17a2b8; text-decoration: none; font-size: 14px;",
             HTML("<i class='fa fa-question-circle'></i> Show/Hide Help")),
      div(style = "display: none;",
        create_help_text()
      )
    ),
    
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
        br(),
        
        # Section info panel
        create_section_info_panel(),
        
        # Summary statistics
        div(
          h4("Summary Statistics", style = "color: #3c8dbc; margin-bottom: 15px;"),
          create_overview_value_boxes()
        ),
        
        br(),
        
        # Progress chart
        create_progress_chart_box()
      ),
      
      # Details tab  
      tabItem(tabName = "details",
        br(),
        
        # Section info panel
        create_section_info_panel(),
        
        # Details table
        create_details_table_box()
      )
    )
  )
)

server <- function(input, output, session) {
  
  # =============================================================================
  # REFRESH BUTTON PATTERN - Capture all inputs when refresh clicked
  # =============================================================================
  
  refresh_inputs <- eventReactive(input$refresh, {
    zone_value <- isolate(input$zone_filter)
    
    # Parse zone filter
    parsed_zones <- if (zone_value == "combined") {
      c("1", "2")  # Include both zones but will be combined
    } else if (zone_value == "1,2") {
      c("1", "2")  # Include both zones separately
    } else {
      zone_value  # Single zone
    }
    
    list(
      zone_filter_raw = zone_value,
      zone_filter = parsed_zones,
      combine_zones = (zone_value == "combined"),
      facility_filter = isolate(input$facility_filter),
      foreman_filter = isolate(input$foreman_filter),
      group_by = isolate(input$group_by),
      custom_today = isolate(input$custom_today),
      expiring_days = isolate(input$expiring_days),
      expiring_filter = isolate(input$expiring_filter)
    )
  })
  
  # Initialize facility choices from db_helpers
  observe({
    facility_choices <- get_facility_choices()
    updateSelectizeInput(session, "facility_filter", choices = facility_choices, selected = "all")
  })
  
  # Initialize foreman choices from db_helpers  
  observe({
    foremen_lookup <- get_foremen_lookup()
    foremen_choices <- c("All" = "all")
    foremen_choices <- c(
      foremen_choices,
      setNames(foremen_lookup$emp_num, foremen_lookup$shortname)
    )
    updateSelectizeInput(session, "foreman_filter", choices = foremen_choices, selected = "all")
  })

  # Fetch ground prehatch data - ONLY when refresh button clicked
  ground_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    # Use custom date if provided, otherwise use current date
    simulation_date <- if (!is.null(inputs$custom_today)) inputs$custom_today else Sys.Date()
    
    get_ground_prehatch_data(inputs$zone_filter, simulation_date)
  })
  
  # Fetch site details data - ONLY when refresh button clicked
  site_details <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    # Use custom date if provided, otherwise use current date
    simulation_date <- if (!is.null(inputs$custom_today)) inputs$custom_today else Sys.Date()
    
    get_site_details_data(inputs$expiring_days, simulation_date)
  })

  # Update foreman filter based on facility selection
  observe({
    req(input$refresh)  # Only update after refresh
    inputs <- refresh_inputs()
    data <- ground_data()
    
    # Filter by facility if not 'all'
    if (!is.null(inputs$facility_filter) && !("all" %in% inputs$facility_filter)) {
      data <- data %>% filter(facility %in% inputs$facility_filter)
    }
    
    # Get foremen lookup to map empnum to names
    foremen_lookup <- get_foremen_lookup()
    
    # Get foreman choices using helper function
    foreman_choices <- get_foreman_choices(data, foremen_lookup)
    
    updateSelectizeInput(session, "foreman_filter", choices = foreman_choices, selected = "all")
  })
  
  # Filter data based on user selections
  filtered_data <- reactive({
    req(input$refresh)  # Require refresh button click
    inputs <- refresh_inputs()
    
    data <- ground_data()
    
    filter_ground_data(data, inputs$zone_filter, inputs$facility_filter, inputs$foreman_filter)
  })
  
  # Aggregate data based on grouping level  
  aggregated_data <- reactive({
    req(input$refresh)  # Require refresh button click
    inputs <- refresh_inputs()
    
    data <- filtered_data()
    site_data <- site_details()
  
    aggregate_data_by_group(
      data, 
      inputs$group_by, 
      inputs$zone_filter, 
      inputs$expiring_filter, 
      site_data,
      inputs$combine_zones
    )
  })
  
  # Details data for the table
  details_data <- reactive({
    req(input$refresh)  # Require refresh button click
    inputs <- refresh_inputs()
    
    site_data <- site_details()
    
    # Apply geographic filters
    filtered_data <- filter_ground_data(site_data, inputs$zone_filter, inputs$facility_filter, inputs$foreman_filter)
    
    # Apply expiring filter to site details
    if (inputs$expiring_filter == "expiring") {
      filtered_data <- filtered_data %>% filter(prehatch_status == "expiring")
    } else if (inputs$expiring_filter == "expiring_expired") {
      filtered_data <- filtered_data %>% filter(prehatch_status %in% c("expiring", "expired"))
    }
    
    return(filtered_data)
  })
  
  # Create value boxes
  value_boxes <- reactive({
    req(input$refresh)  # Require refresh button click
    
    data <- aggregated_data()
    create_value_boxes(data)
  })
  
  # Render value boxes using colors from db_helpers
  output$total_sites <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = data$total_ground,
      subtitle = "Total Ground Sites",
      icon = icon("map-marker"),
      color = shiny_colors["completed"]
    )
  })
  
  output$prehatch_sites <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = data$total_prehatch,
      subtitle = "Prehatch Sites",
      icon = icon("egg"),
      color = shiny_colors["planned"]
    )
  })
  
  output$treated_sites <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = data$total_treated,
      subtitle = "Treated Sites",
      icon = icon("check-circle"),
      color = shiny_colors["active"]
    )
  })
  
  output$expired_sites <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = data$total_expired,
      subtitle = "Expired Sites",
      icon = icon("clock"),
      color = shiny_colors["somthing_else"]
    )
  })
  
  output$treated_pct <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = paste0(data$treated_pct, "%"),
      subtitle = "Treated %",
      icon = icon("percent"),
      color = shiny_colors["active"]
    )
  })
  
  output$expiring_pct <- renderValueBox({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = paste0(data$expiring_pct, "%"),
      subtitle = "Expiring %",
      icon = icon("clock"),
      color = shiny_colors["needs_action"]
    )
  })
  
  # Render progress chart
  output$progress_chart <- renderPlotly({
    req(input$refresh)  # Only render after refresh button clicked
    inputs <- refresh_inputs()
    
    data <- aggregated_data()
    create_progress_chart(data, inputs$group_by, inputs$expiring_filter, inputs$expiring_days)
  })
  
  # Render details table
  output$details_table <- DT::renderDataTable({
    req(input$refresh)  # Only render after refresh button clicked
    
    data <- details_data()
    foremen_lookup <- get_foremen_lookup()
    create_details_table(data, foremen_lookup)
  })
  
  # Download handler for details data
  output$download_details_data <- downloadHandler(
    filename = function() {
      paste("ground_prehatch_details_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(input$refresh)  # Only allow download after refresh
      
      data <- details_data()
      foremen_lookup <- get_foremen_lookup()
      download_data <- prepare_download_data(data, foremen_lookup)
      write.csv(download_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)