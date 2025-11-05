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
  
  # Parse zone filter input
  parsed_zone_filter <- reactive({
    if (input$zone_filter == "combined") {
      return(c("1", "2"))  # Include both zones but will be combined
    } else if (input$zone_filter == "1,2") {
      return(c("1", "2"))  # Include both zones separately
    } else {
      return(input$zone_filter)  # Single zone
    }
  })
  
  # Flag for whether to combine zones
  combine_zones <- reactive({
    input$zone_filter == "combined"
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

  # Fetch ground prehatch data
  ground_data <- reactive({
    req(parsed_zone_filter(), input$group_by)  # Ensure required inputs are available
    
    # Use custom date if provided, otherwise use current date
    simulation_date <- if (!is.null(input$custom_today)) input$custom_today else Sys.Date()
    
    get_ground_prehatch_data(parsed_zone_filter(), simulation_date)
  })
  
  # Fetch site details data
  site_details <- reactive({
    # Use custom date if provided, otherwise use current date
    simulation_date <- if (!is.null(input$custom_today)) input$custom_today else Sys.Date()
    
    get_site_details_data(input$expiring_days, simulation_date)
  })

  # Update foreman filter based on facility selection
  observe({
    data <- ground_data()
    
    # Filter by facility if not 'all'
    if (!is.null(input$facility_filter) && !("all" %in% input$facility_filter)) {
      data <- data %>% filter(facility %in% input$facility_filter)
    }
    
    # Get foremen lookup to map empnum to names
    foremen_lookup <- get_foremen_lookup()
    
    # Get foreman choices using helper function
    foreman_choices <- get_foreman_choices(data, foremen_lookup)
    
    updateSelectizeInput(session, "foreman_filter", choices = foreman_choices, selected = "all")
  })
  
  # Filter data based on user selections
  filtered_data <- reactive({
    req(parsed_zone_filter(), input$group_by, input$facility_filter)
    # Don't require foreman_filter since it starts as NULL
    
    data <- ground_data()
    
    filter_ground_data(data, parsed_zone_filter(), input$facility_filter, input$foreman_filter)
  })
  
  # Aggregate data based on grouping level  
  aggregated_data <- reactive({
    data <- filtered_data()
    site_data <- site_details()
    
    aggregate_data_by_group(
      data, 
      input$group_by, 
      parsed_zone_filter(), 
      input$show_expiring_only, 
      site_data,
      combine_zones()
    )
  })
  
  # Details data for the table
  details_data <- reactive({
    site_data <- site_details()
    
    filter_ground_data(site_data, parsed_zone_filter(), input$facility_filter, input$foreman_filter)
  })
  
  # Create value boxes
  value_boxes <- reactive({
    data <- aggregated_data()
    create_value_boxes(data)
  })
  
  # Render value boxes using colors from db_helpers
  output$total_sites <- renderValueBox({
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
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = data$total_treated,
      subtitle = "Treated Sites",
      icon = icon("check-circle"),
      color = shiny_colors["active"]
    )
  })
  
  output$needs_treatment <- renderValueBox({
    data <- value_boxes()
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = data$total_needs_treatment,
      subtitle = "Needs Treatment",
      icon = icon("exclamation-triangle"),
      color = shiny_colors["needs_treatment"]
    )
  })
  
  output$treated_pct <- renderValueBox({
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
    data <- aggregated_data()
    create_progress_chart(data, input$group_by, input$show_expiring_only, input$expiring_days)
  })
  
  # Render details table
  output$details_table <- DT::renderDataTable({
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
      data <- details_data()
      foremen_lookup <- get_foremen_lookup()
      download_data <- prepare_download_data(data, foremen_lookup)
      write.csv(download_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)