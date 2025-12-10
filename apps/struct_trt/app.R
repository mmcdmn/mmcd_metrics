# struct status App

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(rlang)
  library(purrr)  # For map_dfr function
  library(tibble) # For deframe function
  library(scales) # For percentage and number formatting
  library(DT)     # For data tables
  library(plotly) # For interactive plots
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")

# Source external function files
source("data_functions.R")
source("display_functions.R")
source("historical_functions.R")

# Load environment variables from .env file
env_paths <- c(
  "../../.env",
  "../../../.env",
  "/srv/shiny-server/.env"
)

env_loaded <- FALSE
for (path in env_paths) {
  if (file.exists(path)) {
    readRenviron(path)
    env_loaded <- TRUE
    break
  }
}

# Create help text for historical metrics
create_help_text <- function() {
  div(
    style = "margin: 10px; padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; font-size: 16px;",
    h4("Understanding Historical Metrics", style = "margin-top: 0; font-size: 20px;"),
    
    p(style = "margin-bottom: 15px; font-size: 16px;",
      "The historical trends chart shows structure treatment coverage over time. You can view the data in two different ways:"
    ),
    
    hr(style = "margin: 15px 0;"),
    
    h5("Metric Definitions", style = "font-size: 18px;"),
    
    p(style = "margin-bottom: 10px; font-size: 16px;",
      strong("Proportion (%):"), " The percentage of all structures (within your selected filters) that had active treatments during each time period.",
      br(),
      em("Example: If there are 1,000 structures total and 250 had active treatments in a given week, the proportion is 25%.")
    ),
    
    p(style = "margin-bottom: 10px; font-size: 16px;",
      strong("Number of Structures:"), " The absolute count of structures that had active treatments during each time period.",
      br(),
      em("Example: If 250 structures had active treatments in a given week, this metric shows 250.")
    ),
    
    hr(style = "margin: 15px 0;"),
    
    h5("Chart Options", style = "font-size: 18px;"),
    
    p(style = "margin-bottom: 10px; font-size: 16px;",
      strong("Average Lines:"), " You can overlay 5-year or 10-year average lines to compare current performance against historical trends. These averages use all available historical data, regardless of the selected date range."
    ),
    
    p(style = "margin-bottom: 10px; font-size: 16px;",
      strong("Chart Types:"), " Choose from line, area, step, stacked bar, or grouped bar charts to visualize the data in different ways."
    )
  )
}

ui <- fluidPage(
  # Use universal CSS from db_helpers for consistent text sizing
  get_universal_text_css(),
  
  # Add custom CSS for collapsible sidebar
  tags$head(
    tags$style(HTML("
      .sidebar-toggle {
        position: fixed;
        top: 60px;
        left: 10px;
        z-index: 1000;
        background-color: #3c8dbc;
        color: white;
        border: none;
        padding: 10px 15px;
        cursor: pointer;
        border-radius: 4px;
        font-size: 18px;
      }
      .sidebar-toggle:hover {
        background-color: #357ca5;
      }
      .sidebar-collapsed {
        display: none !important;
      }
      /* Move tabs to the right to avoid overlap with sidebar toggle button */
      .nav-tabs {
        margin-left: 50px;
      }
    "))
  ),
  
  # Add toggle button
  tags$button(
    class = "sidebar-toggle",
    onclick = "$('.col-sm-4').toggleClass('sidebar-collapsed');",
    HTML("&#9776;")
  ),
  
  titlePanel("Structures with Active and Expiring Treatments"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("refresh", "Refresh Data", 
                   icon = icon("refresh"),
                   class = "btn-primary btn-lg",
                   style = "width: 100%; margin-bottom: 20px;"),
      
      sliderInput("expiring_days", "Days Until Expiration:",
                  min = 1, max = 30, value = 7, step = 1),
      
      dateInput("custom_today", "Pretend Today is:",
                value = Sys.Date(), 
                format = "yyyy-mm-dd"),
      
      checkboxGroupInput("status_types", "Include Structure Status:",
                         choices = c("Dry (D)" = "D",
                                     "Wet (W)" = "W", 
                                     "Unknown (U)" = "U"),
                         selected = c("D", "W", "U")),
      
      selectInput("facility_filter", "Facility:",
                  choices = get_facility_choices(),
                  selected = "all"),
      
      selectizeInput("foreman_filter", "FOS:",
                    choices = c("Loading..." = "LOADING"),
                    selected = NULL,
                    multiple = TRUE,
                    options = list(
                      placeholder = "Select FOS (empty = all)",
                      plugins = list('remove_button')
                    )),
      
      selectInput("group_by", "Group by:",
                  choices = c("Facility" = "facility",
                              "FOS" = "foreman", 
                              "All MMCD" = "mmcd_all"),
                  selected = "facility"),
      
      radioButtons("zone_filter", "Zone Display:",
                   choices = c("P1 Only" = "1", 
                              "P2 Only" = "2", 
                              "P1 and P2 Separate" = "1,2", 
                              "Combined P1+P2" = "combined"),
                   selected = "1,2"),
      
      selectizeInput("structure_type_filter", "Structure Type:",
                  choices = get_structure_type_choices(include_all = TRUE),
                  selected = "all", multiple = TRUE),
      # we removed priority filter for now because data is incomplete
      # not all facilities have priorities for the structures
      
      selectInput("color_theme", "Color Theme:",
                  choices = c("MMCD" = "MMCD",
                              "IBM Design" = "IBM",
                              "Wong (Color Blind Safe)" = "Wong",
                              "Tol (Color Blind Safe)" = "Tol",
                              "Viridis" = "Viridis",
                              "ColorBrewer Set2" = "ColorBrewer"),
                  selected = "MMCD"),
      
      helpText(tags$b("Structure Status:"),
               tags$br(),
               tags$ul(
                 tags$li(tags$b("D:"), "Dry - Structure is dry"),
                 tags$li(tags$b("W:"), "Wet - Structure has water"),
                 tags$li(tags$b("U:"), "Unknown - Status not determined")
               )),
      
      # Help text for historical metrics (collapsible)
      hr(),
      div(id = "help-section",
        tags$a(href = "#", onclick = "$(this).next().toggle(); return false;", 
               style = "color: #17a2b8; text-decoration: none; font-size: 15px;",
               HTML("<i class='fa fa-question-circle'></i> Show/Hide Help")),
        div(style = "display: none;",
          create_help_text()
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Current Progress", 
                 plotOutput("structureGraph", height = "800px"),
                 br(),
                 fluidRow(
                   column(10, h4("Current Structure Details")),
                   column(2, downloadButton("download_current_data", "Download CSV", 
                                           class = "btn-success btn-sm", 
                                           style = "margin-top: 20px; float: right;"))
                 ),
                 DT::dataTableOutput("currentStructureTable")
        ),
        tabPanel("Historical Trends",
                 br(),
                 fluidRow(
                   column(3,
                          radioButtons("hist_time_period", "Time Period:",
                                      choices = c("Yearly" = "yearly", "Weekly" = "weekly"),
                                      selected = "yearly")
                   ),
                   column(3,
                          conditionalPanel(
                            condition = "input.hist_time_period == 'yearly' && input.hist_display_metric_yearly == 'proportion'",
                            selectInput("hist_chart_type_prop", "Chart Type:",
                                        choices = c("Grouped Bar" = "grouped_bar",
                                                    "Line Chart" = "line",
                                                    "Pie Chart" = "pie"),
                                        selected = "grouped_bar")
                          ),
                          conditionalPanel(
                            condition = "!(input.hist_time_period == 'yearly' && input.hist_display_metric_yearly == 'proportion')",
                            selectInput("hist_chart_type_regular", "Chart Type:",
                                        choices = c("Stacked Bar" = "stacked_bar",
                                                    "Grouped Bar" = "grouped_bar",
                                                    "Line Chart" = "line",
                                                    "Area Chart" = "area"),
                                        selected = "stacked_bar")
                          )
                   ),
                   column(3,
                          conditionalPanel(
                            condition = "input.hist_time_period == 'yearly'",
                            radioButtons("hist_display_metric_yearly", "Display Metric:",
                                       choices = c("Total Treatments" = "treatments",
                                                  "Unique Structures Treated" = "structures_count",
                                                  "Proportion of Structures (%)" = "proportion"),
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
                                     value = c(2020, as.numeric(format(Sys.Date(), "%Y"))),
                                     step = 1, sep = "")
                   )
                 ),
                 plotlyOutput("historicalGraph", height = "600px"),
                 br(),
                 fluidRow(
                   column(10, h4("Historical Treatment Data")),
                   column(2, downloadButton("download_historical_data", "Download CSV", 
                                           class = "btn-success btn-sm", 
                                           style = "margin-top: 20px; float: right;"))
                 ),
                 DT::dataTableOutput("historicalStructureTable")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Initialize facility choices on app startup
  observe({
    facility_choices <- get_facility_choices()
    updateSelectInput(session, "facility_filter", choices = facility_choices, selected = "all")
  })

  # Dynamically update foreman choices based on selected facility
  observe({
    selected_facility <- input$facility_filter
    foremen_lookup <- get_foremen_lookup()
    foreman_choices <- c("All" = "all")
    if (!is.null(selected_facility) && selected_facility != "all" && nrow(foremen_lookup) > 0) {
      filtered_foremen <- foremen_lookup[foremen_lookup$facility == selected_facility, ]
      if (nrow(filtered_foremen) > 0) {
        foreman_choices <- c(
          foreman_choices,
          setNames(filtered_foremen$emp_num, filtered_foremen$shortname)
        )
      }
    } else if (nrow(foremen_lookup) > 0) {
      foreman_choices <- c(
        foreman_choices,
        setNames(foremen_lookup$emp_num, foremen_lookup$shortname)
      )
    }
    # Start empty, where empty means all
    updateSelectizeInput(session, "foreman_filter", choices = foreman_choices, selected = NULL)
  })
  
  # =============================================================================
  # THEME HANDLING
  # =============================================================================
  
  # Reactive for current theme
  current_theme <- reactive({
    input$color_theme
  })
  
  # Update global option when theme changes
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
  
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
      expiring_days = isolate(input$expiring_days),
      custom_today = isolate(input$custom_today),
      status_types = isolate(input$status_types),
      facility_filter = isolate(input$facility_filter),
      foreman_filter = isolate(input$foreman_filter),
      group_by = isolate(input$group_by),
      structure_type_filter = isolate(input$structure_type_filter),
      priority_filter = "all",  # Default value since priority filter was removed from UI
      # Historical inputs
      hist_time_period = isolate(input$hist_time_period),
      hist_display_metric = if (isolate(input$hist_time_period) == "yearly") {
        if (!is.null(input$hist_display_metric_yearly)) {
          isolate(input$hist_display_metric_yearly)
        } else {
          "treatments"
        }
      } else {
        if (!is.null(input$hist_display_metric_weekly)) {
          isolate(input$hist_display_metric_weekly)
        } else {
          "weekly_active_treatments"
        }
      },
      hist_chart_type = if (isolate(input$hist_time_period) == "yearly" && 
                             !is.null(isolate(input$hist_display_metric_yearly)) &&
                             isolate(input$hist_display_metric_yearly) == "proportion") {
        if (!is.null(input$hist_chart_type_prop)) {
          isolate(input$hist_chart_type_prop)
        } else {
          "grouped_bar"
        }
      } else {
        if (!is.null(input$hist_chart_type_regular)) {
          isolate(input$hist_chart_type_regular)
        } else {
          "stacked_bar"
        }
      },
      hist_year_range = isolate(input$hist_year_range)
    )
  })
  
  # Load current data - ONLY when refresh button clicked
  current_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    withProgress(message = "Loading structure treatment data...", value = 0.5, {
      get_current_structure_data(
        inputs$custom_today,
        inputs$expiring_days,
        inputs$facility_filter,
        inputs$structure_type_filter,
        inputs$priority_filter,
        inputs$status_types,
        inputs$zone_filter,
        inputs$foreman_filter
      )
    })
  })
  
  # Load all structures - ONLY when refresh button clicked
  all_structures <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    withProgress(message = "Loading structures...", value = 0.5, {
      get_all_structures(
        inputs$facility_filter,
        inputs$structure_type_filter,
        inputs$priority_filter,
        inputs$status_types,
        inputs$zone_filter,
        inputs$foreman_filter
      )
    })
  })
  
  # Load historical data - ONLY when refresh button clicked
  historical_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    
    withProgress(message = 'Loading historical data...', value = 0, {
      incProgress(0.3, detail = "Querying database...")
      
      hist_data <- create_historical_struct_data(
        start_year = inputs$hist_year_range[1],
        end_year = inputs$hist_year_range[2],
        hist_time_period = inputs$hist_time_period,
        hist_display_metric = inputs$hist_display_metric,
        hist_group_by = inputs$group_by,
        hist_zone_display = if (inputs$combine_zones) "combined" else "show-both",
        facility_filter = inputs$facility_filter,
        foreman_filter = inputs$foreman_filter,
        zone_filter = inputs$zone_filter,
        structure_type_filter = inputs$structure_type_filter,
        status_types = inputs$status_types
      )
      
      incProgress(0.7, detail = "Processing data...")
      
      return(hist_data)
    })
  })
  
  # Aggregate current data
  aggregated_current <- reactive({
    req(input$refresh)  # Require refresh button click
    inputs <- refresh_inputs()
    
    structures <- all_structures()
    treatments <- current_data()$treatments
    
    aggregate_structure_data(
      structures,
      treatments,
      inputs$group_by,
      inputs$zone_filter,
      inputs$combine_zones
    )
  })
  
  # Render current progress chart
  output$structureGraph <- renderPlot({
    req(aggregated_current())  # Require data exists
    inputs <- refresh_inputs()
    
    # CRITICAL: Read theme to create reactive dependency
    current_theme_value <- input$color_theme
    
    cat("Current progress chart rendering with theme:", current_theme_value, "\n")
    
    data <- aggregated_current()
    
    create_current_progress_chart(
      data,
      inputs$group_by,
      inputs$facility_filter,
      inputs$status_types,
      inputs$zone_filter,
      inputs$combine_zones,
      theme = current_theme_value
    )
  })
  
  # Render historical trends chart
  output$historicalGraph <- renderPlotly({
    req(historical_data())  # Require data exists
    inputs <- refresh_inputs()
    
    # CRITICAL: Read theme to create reactive dependency
    current_theme_value <- input$color_theme
    
    cat("Historical chart rendering with theme:", current_theme_value, "\n")
    
    create_historical_struct_chart(
      data = historical_data(),
      hist_time_period = inputs$hist_time_period,
      hist_display_metric = inputs$hist_display_metric,
      hist_group_by = inputs$group_by,
      chart_type = inputs$hist_chart_type,
      theme = current_theme_value
    )
  })
  
  # Current structure details table - simplified version
  output$currentStructureTable <- DT::renderDataTable({
    req(input$refresh)  # Only render after refresh button clicked
    
    all_structs <- all_structures()
    
    if (nrow(all_structs) == 0) {
      return(DT::datatable(
        data.frame("No data available" = character(0)),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ))
    }
    
    # Show structure data with better column names
    display_data <- all_structs %>%
      select(sitecode, facility, zone, s_type, foreman, status) %>%
      rename(
        "Sitecode" = sitecode,
        "Facility" = facility,
        "Zone" = zone,
        "Structure Type" = s_type,
        "FOS" = foreman,
        "Status" = status
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Historical structure data table
  output$historicalStructureTable <- DT::renderDataTable({
    req(input$refresh)  # Require refresh button click
    data <- historical_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(DT::datatable(
        data.frame("No data available" = character(0)),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ))
    }
    
    # Format the aggregated data for display
    table_data <- data %>%
      arrange(desc(time_period)) %>%
      select(time_period, group_name, count) %>%
      rename(
        "Time Period" = time_period,
        "Group" = group_name,
        "Count" = count
      )
    
    DT::datatable(
      table_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Download handlers for CSV exports
  output$download_current_data <- downloadHandler(
    filename = function() {
      paste0("structure_current_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      all_structs <- all_structures()
      
      if (nrow(all_structs) > 0) {
        # Export structure data
        export_csv_safe(all_structs, file)
      } else {
        export_csv_safe(data.frame("No data available" = character(0)), file)
      }
    }
  )
  
  output$download_historical_data <- downloadHandler(
    filename = function() {
      paste0("structure_historical_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- historical_data()
      
      if (!is.null(data) && nrow(data) > 0) {
        export_csv_safe(data, file)
      } else {
        export_csv_safe(data.frame("No data available" = character(0)), file)
      }
    }
  )
}

shinyApp(ui = ui, server = server)