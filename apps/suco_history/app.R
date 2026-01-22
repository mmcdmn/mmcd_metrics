# Suco History Analysis App

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(scales)
  library(leaflet) 
  library(sf) 
  library(stringr) 
  library(plotly)
  library(later) 
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")
source("../../shared/server_utilities.R")
source("data_functions.R")
source("display_functions.R")
source("ui_helpers.R")

# Set application name for AWS RDS monitoring
set_app_name("suco_history")

# Load environment variables from .env file (for local development)
# or from Docker environment variables (for production)
env_paths <- c(
  "../../.env",           # For local development
  "../../../.env",        # Alternative local path
  "/srv/shiny-server/.env" # Docker path
)

# Try to load from .env file first
env_loaded <- FALSE
for (path in env_paths) {
  if (file.exists(path)) {
    readRenviron(path)
    env_loaded <- TRUE
    break
  }
}

# If no .env file found, environment variables should already be set by Docker

# Define UI for the application
ui <- fluidPage(
  # Use universal CSS from db_helpers for consistent text sizing
  get_universal_text_css(),
  # Application title
  titlePanel("SUCO Analysis Dashboard"),

  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      # Refresh button at top with instruction
      actionButton("refresh", "Refresh Data", 
                   icon = icon("refresh"),
                   class = "btn-primary btn-lg",
                   style = "width: 100%;"),
      p(style = "text-align: center; margin-top: 10px; font-size: 0.9em; color: #666;",
        "Click refresh to get data for selected filters"),
      
      hr(),
      
      # Color theme selector
      selectInput("color_theme", "Color Theme:",
                  choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
                  selected = "MMCD"),
      
      hr(),
      
      # Group by selection
      h4("Analysis Options"),
      radioButtons("group_by", "Group By:",
                   choices = c("MMCD (All)" = "mmcd_all",
                               "Facility" = "facility", 
                               "FOS" = "foreman"),
                   selected = "mmcd_all"),
      
      hr(),
      
      # Date range selection
      h4("Date Selection"),
      dateRangeInput("date_range", "Custom Date Range:",
                     start = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                     end = Sys.Date(),
                     format = "yyyy-mm-dd"),
      
      hr(),
      
      # Filters section - more compact
      h4("Filters"),
      
      # Filter by zone (P1/P2)
      selectInput("zone_filter", "Zone:",
                  choices = c("P1 + P2" = "all", "P1" = "1", "P2" = "2"),
                  selected = "all"),
      
      # Filter by facility (single-select)
      selectInput("facility_filter", "Facility:",
                  choices = NULL),
      
      # Filter by foreman (multi-select, populated dynamically based on facility)
      selectizeInput("foreman_filter", "FOS:",
                  choices = NULL, multiple = TRUE),
      
      # Add species filter to sidebarPanel
      selectInput("species_filter", "Species:", choices = c("All"), selected = "All"),
      
      hr(),
      
      # Display and visualization options
      h4("Display Options"),
      # Graph type selector
      selectInput("graph_type", "Graph Type:",
                  choices = c("Bar" = "bar", "Stacked Bar" = "stacked_bar", "Line" = "line", "Point" = "point", "Area" = "area"),
                  selected = "stacked_bar"),
      
      # Top locations mode toggle
      conditionalPanel(
        condition = "input.current_tabset == 'CurrentTopLoc' || input.all_tabset == 'AllTopLoc'",
        selectInput("top_locations_mode", "Top Locations:",
                    choices = c("Most Visited" = "visits", "Most Species" = "species"),
                    selected = "visits")
      ),
      # Map Controls
      conditionalPanel(
        condition = "input.current_tabset == 'CurrentMap' || input.all_tabset == 'AllMap'",
        selectInput("basemap", "Base Map:",
                    choices = c("OpenStreetMap" = "osm",
                                "Carto Light" = "carto",
                                "Esri Satellite" = "satellite"),
                    selected = "carto")
      )
    ),
    
    # Main panel for displaying the graphs
    mainPanel(
      tabsetPanel(id = "main_tabset",
        tabPanel("Current Data", 
          tabsetPanel(id = "current_tabset",
            tabPanel("Graph", value = "CurrentGraph", plotOutput("current_trend_plot", height = "500px")),
            tabPanel("Map", value = "CurrentMap", leafletOutput("current_map", height = "600px")),
            tabPanel("Summary Table", value = "CurrentTable", 
                     br(),
                     fluidRow(
                       column(10, h4("Summary Table")),
                       column(2, downloadButton("download_current_summary", "Download CSV", 
                                               class = "btn-success btn-sm", 
                                               style = "float: right;"))
                     ),
                     dataTableOutput("current_summary_table")),
            tabPanel("Detailed Samples", value = "CurrentDetailed", 
                     br(),
                     fluidRow(
                       column(10, h4("Detailed Samples")),
                       column(2, downloadButton("download_current_detailed", "Download CSV", 
                                               class = "btn-success btn-sm", 
                                               style = "float: right;"))
                     ),
                     dataTableOutput("current_detailed_table")),
            tabPanel("Top Locations", value = "CurrentTopLoc", plotlyOutput("current_location_plotly", height = "700px"))
          )
        ),
        tabPanel("All Data (Current + Archive)",
          tabsetPanel(id = "all_tabset",
            tabPanel("Graph", value = "AllGraph", plotOutput("trend_plot", height = "500px")),
            tabPanel("Map", value = "AllMap", leafletOutput("map", height = "600px")),
            tabPanel("Summary Table", value = "AllTable", 
                     br(),
                     fluidRow(
                       column(10, h4("Summary Table")),
                       column(2, downloadButton("download_summary", "Download CSV", 
                                               class = "btn-success btn-sm", 
                                               style = "float: right;"))
                     ),
                     dataTableOutput("summary_table")),
            tabPanel("Detailed Samples", value = "AllDetailed", 
                     br(),
                     fluidRow(
                       column(10, h4("Detailed Samples")),
                       column(2, downloadButton("download_detailed", "Download CSV", 
                                               class = "btn-success btn-sm", 
                                               style = "float: right;"))
                     ),
                     dataTableOutput("detailed_table")),
            tabPanel("Top Locations", value = "AllTopLoc", plotlyOutput("location_plotly", height = "700px"))
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive theme handling
  current_theme <- reactive({
    input$color_theme
  })
  
  # Set global theme option when changed
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
  # Reactive values to prevent infinite loops between date controls
  updating_date_range <- reactiveVal(FALSE)
  updating_year_range <- reactiveVal(FALSE)
  
  # =============================================================================
  # REFRESH BUTTON PATTERN - Capture all inputs when refresh clicked
  # =============================================================================
  
  refresh_inputs <- eventReactive(input$refresh, {
    list(
      group_by = isolate(input$group_by),
      date_range = isolate(input$date_range),
      zone_filter = isolate(input$zone_filter),
      facility_filter = isolate(input$facility_filter),
      foreman_filter = isolate(input$foreman_filter),
      species_filter = isolate(input$species_filter),
      graph_type = isolate(input$graph_type),
      top_locations_mode = isolate(input$top_locations_mode),
      basemap = isolate(input$basemap)
    )
  })
  
  # Initialize facility choices from db_helpers - runs immediately on app load
  observe({
    facility_choices <- get_facility_choices()
    updateSelectInput(session, "facility_filter", choices = facility_choices, selected = "all")
  })
  
  # Initialize foreman choices from db_helpers - runs immediately on app load
  observe({
    foremen_lookup <- get_foremen_lookup()
    foremen_choices <- c("All" = "all")
    foremen_choices <- c(
      foremen_choices,
      setNames(foremen_lookup$emp_num, foremen_lookup$shortname)
    )
    updateSelectizeInput(session, "foreman_filter", choices = foremen_choices, selected = "all")
  })
  
  # Update FOS choices when facility changes
  observeEvent(input$facility_filter, {
    foremen_lookup <- get_foremen_lookup()
    
    if (input$facility_filter == "all") {
      # Show all FOS when "All Facilities" is selected
      foremen_choices <- c("All" = "all")
      foremen_choices <- c(
        foremen_choices,
        setNames(foremen_lookup$emp_num, foremen_lookup$shortname)
      )
    } else {
      # Filter FOS by selected facility
      filtered_foremen <- foremen_lookup[foremen_lookup$facility == input$facility_filter, ]
      foremen_choices <- c("All" = "all")
      if (nrow(filtered_foremen) > 0) {
        foremen_choices <- c(
          foremen_choices,
          setNames(filtered_foremen$emp_num, filtered_foremen$shortname)
        )
      }
    }
    
    updateSelectizeInput(session, "foreman_filter", choices = foremen_choices, selected = "all")
  })
  
  # Initialize species choices from db_helpers
  observe({
    species_lookup <- get_species_lookup()
    if (nrow(species_lookup) > 0) {
      # Create species display names
      species_names <- paste(species_lookup$genus, species_lookup$species)
      species_choices <- c("All", unique(species_names))
      updateSelectInput(session, "species_filter", choices = species_choices, selected = "All")
    }
  })
  
  # Fetch SUCO data - all data (current + archive) - ONLY when refresh button clicked
  suco_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    withProgress(message = "Loading SUCO data...", value = 0.5, {
      get_suco_data("all", inputs$date_range)
    })
  })
  
  # Fetch SUCO data - current only (more efficient for Current Data tab) - ONLY when refresh button clicked
  suco_data_current <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    withProgress(message = "Loading current SUCO data...", value = 0.5, {
      get_suco_data("current", inputs$date_range)
    })
  })

  # Filter data based on user selections
  filtered_data <- reactive({
    data <- suco_data()
    filter_suco_data(data, input$facility_filter, input$foreman_filter, input$zone_filter, input$date_range, input$species_filter)
  })
  
  # Filter current data based on user selections
  filtered_data_current <- reactive({
    data <- suco_data_current()
    filter_suco_data(data, input$facility_filter, input$foreman_filter, input$zone_filter, input$date_range, input$species_filter)
  })
  
  # Process spatial data for mapping
  spatial_data <- reactive({
    data <- filtered_data()
    create_spatial_data(data, input$species_filter)
  })
  
  # Process spatial data for mapping (current data only)
  spatial_data_current <- reactive({
    data <- filtered_data_current()
    create_spatial_data(data, input$species_filter)
  })
  
  # Aggregate data by selected time interval and grouping
  aggregated_data <- reactive({
    req(input$refresh)  # Require refresh button click
    inputs <- refresh_inputs()
    data <- filtered_data()
    aggregate_suco_data(data, inputs$group_by, inputs$zone_filter)
  })
  
  # Aggregate current data by selected time interval and grouping
  aggregated_data_current <- reactive({
    req(input$refresh)  # Require refresh button click
    inputs <- refresh_inputs()
    data <- filtered_data_current()
    aggregate_suco_data(data, inputs$group_by, inputs$zone_filter)
  })
  
  # Generate trend plot
  output$trend_plot <- renderPlot({
    req(input$refresh)  # Only render after refresh button clicked
    inputs <- refresh_inputs()
    create_trend_plot(aggregated_data, aggregated_data_current, inputs, "all", theme = current_theme())
  })
  
  # Generate current trend plot (for current data tab)
  output$current_trend_plot <- renderPlot({
    req(input$refresh)  # Only render after refresh button clicked
    inputs <- refresh_inputs()
    create_trend_plot(aggregated_data, aggregated_data_current, inputs, "current", theme = current_theme())
  })
  
  # Generate map
  output$map <- renderLeaflet({
    req(input$refresh)  # Only render after refresh button clicked
    inputs <- refresh_inputs()
    create_suco_map(spatial_data(), inputs, "all", theme = current_theme())
  })


  # Generate summary table
  output$summary_table <- renderDataTable({
    req(input$refresh)  # Only render after refresh button clicked
    inputs <- refresh_inputs()
    data <- filtered_data()
    summary_data <- create_summary_stats(data, inputs$group_by, "all")
    return(summary_data)
  }, options = list(pageLength = 15, 
                   searching = TRUE,
                   columnDefs = list(list(
                     targets = c("First_SUCO", "Last_SUCO"),
                     render = JS("function(data, type, row) {
                       return data ? data : 'N/A';
                     }")
                   ))))
  
  # Generate detailed samples table
  output$detailed_table <- renderDataTable({
    req(input$refresh)  # Only render after refresh button clicked
    inputs <- refresh_inputs()
    data <- filtered_data()
    detailed_data <- create_detailed_samples_table(data, inputs$species_filter)
    return(detailed_data)
  }, options = list(pageLength = 25, 
                   searching = TRUE,
                   scrollX = TRUE,
                   columnDefs = list(list(
                     targets = c("Species_Found"),
                     render = JS("function(data, type, row) {
                       return data ? data.substring(0, 100) + (data.length > 100 ? '...' : '') : 'N/A';
                     }")
                   ))))
  
  # Generate location plot (top locations with most SUCOs)
  output$location_plotly <- plotly::renderPlotly({
    data <- filtered_data()
    top_locations <- get_top_locations(data, input$top_locations_mode, input$species_filter)
    create_location_plotly(top_locations, "all", input$top_locations_mode, theme = current_theme())
  })
  
  # React to plotly click and update map and tab
  observe({
    click <- plotly::event_data("plotly_click", source = "location_plotly")
    if (!is.null(click)) {
      idx <- click$pointNumber + 1  # R is 1-based
      data <- filtered_data()
      top_locations <- get_top_locations(data, input$top_locations_mode, input$species_filter)
      if (idx > 0 && idx <= nrow(top_locations)) {
        loc <- top_locations$location[idx]
        # Use spatial_data() for accurate coordinates (same as map)
        spatial <- spatial_data()
        # Find the first point in spatial_data with this location
        if (nrow(spatial) > 0 && loc %in% spatial$location) {
          point <- spatial[spatial$location == loc, ][1, ]
          coords <- sf::st_coordinates(point)
          lng <- coords[1]
          lat <- coords[2]
          if (!is.na(lng) && !is.na(lat)) {
            updateTabsetPanel(session, "all_tabset", selected = "AllMap")
            leafletProxy("map") %>%
              setView(lng = lng, lat = lat, zoom = 15) %>%
              addCircleMarkers(lng = lng, lat = lat, radius = 15, color = "red", fill = TRUE, fillOpacity = 0.7, layerId = "highlighted_location")
          }
        }
      }
    } else {
      leafletProxy("map") %>% clearGroup("highlighted_location")
    }
  })
  
  # ========== CURRENT DATA OUTPUTS ==========
  
  # Generate current map (uses same logic as main map but with current data)
  output$current_map <- renderLeaflet({
    req(input$refresh)  # Only render after refresh button clicked
    inputs <- refresh_inputs()
    # Use spatial_data_current() instead of spatial_data()
    data <- spatial_data_current()
    
    # Get facility and foremen lookups for display names
    facilities <- get_facility_lookup()
    
    # Set up basemap provider
    basemap <- switch(inputs$basemap,
                      "osm" = providers$OpenStreetMap,
                      "carto" = providers$CartoDB.Positron,
                      "satellite" = providers$Esri.WorldImagery,
                      providers$CartoDB.Positron)
    
    # Handle case when no data is available
    if (nrow(data) == 0) {
      return(
        leaflet() %>%
          addProviderTiles(basemap) %>%
          setView(lng = -93.2, lat = 45.0, zoom = 9) %>%
          addControl(html = "<div style='background-color: white; padding: 10px;'><h4>No SUCO locations available with the selected filters (Current Data)</h4></div>",
                     position = "topleft")
      )
    }
    
    # Create color palette based on field count or facility (SAME LOGIC AS MAIN MAP)
    if (input$group_by == "facility") {
      # Get facility colors and lookup from db_helpers
      facility_colors <- get_facility_base_colors(theme = current_theme())
      facilities <- get_facility_lookup()
      foremen_lookup <- get_foremen_lookup()  # Add foremen lookup for popups
      foremen_lookup$emp_num <- as.character(foremen_lookup$emp_num)  # Ensure string format
      
      # Create facility name mapping FIRST
      facility_names <- setNames(facilities$full_name, facilities$short_name)
      
      # Create popup text beforehand to avoid scope issues
      data <- data %>%
        mutate(
          popup_text = {
            # Get proper foreman names with robust matching
            foreman_names <- sapply(foreman, function(f) {
              if (!is.na(f) && f != "" && !is.null(f)) {
                # Ensure both foreman and emp_num are strings for comparison
                foreman_str <- trimws(as.character(f))
                
                # Find matching foreman in lookup table
                matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_str)
                
                if(length(matches) > 0) {
                  foremen_lookup$shortname[matches[1]]
                } else {
                  # Fallback: show the raw foreman ID if no match found
                  paste0("FOS #", foreman_str)
                }
              } else {
                "No FOS assigned"
              }
            })
            
            facility_names_vec <- sapply(facility, function(f) {
              fname <- facility_names[f]
              if(length(fname) > 0) fname[1] else f
            })
            
            paste0("<b>Date:</b> ", inspdate, "<br>",
                   "<b>Facility:</b> ", facility_names_vec, "<br>",
                   "<b>FOS:</b> ", foreman_names, "<br>",
                   "<b>Location:</b> ", location, "<br>",
                   "<b>Species Count:</b> ", display_species_count, "<br>",
                   "<b>Species Found:</b><br>", species_summary)
          },
          marker_fill_opacity = ifelse(display_species_count == 0, 0.35, 0.8),
          marker_weight = ifelse(display_species_count == 0, 4, 1.5)
        )
      
      # Create color palette function
      pal <- colorFactor(
        palette = facility_colors,
        domain = names(facility_colors))
      
      # Create a named vector for legend labels
      legend_labels <- sapply(names(facility_colors), function(code) facility_names[code] %||% code)
      
      # Create map with facility coloring
      leaflet(data) %>%
        addProviderTiles(basemap) %>%
        fitBounds(
          lng1 = min(st_coordinates(data)[,1]),
          lat1 = min(st_coordinates(data)[,2]),
          lng2 = max(st_coordinates(data)[,1]),
          lat2 = max(st_coordinates(data)[,2])
        ) %>%
        addCircleMarkers(
          radius = ~marker_size,
          color = "black",
          weight = ~marker_weight,
          fillColor = ~pal(facility),
          fillOpacity = ~marker_fill_opacity,
          popup = ~popup_text
        ) %>%
        addLegend(
          position = "bottomright",
          title = "Facility (Current Data)",
          colors = facility_colors,
          labels = legend_labels,
          opacity = 0.8
        ) %>%
        addControl(
          html = '<div style="background: white; padding: 10px; border: 2px solid grey; border-radius: 5px;">
                  <div style="font-weight: bold; margin-bottom: 5px;">Marker Type</div>
                  <div style="margin-bottom: 3px;">
                    <span style="display: inline-block; width: 16px; height: 16px; border-radius: 50%; 
                                background-color: #1f77b4; border: 2px solid black; vertical-align: middle;"></span>
                    <span style="margin-left: 5px;">Target species present</span>
                  </div>
                  <div>
                    <span style="display: inline-block; width: 16px; height: 16px; border-radius: 50%; 
                                background-color: rgba(31, 119, 180, 0.35); border: 4px solid black; vertical-align: middle;"></span>
                    <span style="margin-left: 5px;">Zero target species</span>
                  </div>
                </div>',
          position = "bottomleft"
        )
    } else if (input$group_by == "foreman") {
      # Filter out records with NA foreman before processing
      data <- data %>% filter(!is.na(foreman) & foreman != "")
      
      # Check if we have any data left after filtering
      if (nrow(data) == 0) {
        return(
          leaflet() %>%
            addProviderTiles(basemap) %>%
            setView(lng = -93.2, lat = 45.0, zoom = 9) %>%
            addControl(html = "<div style='background-color: white; padding: 10px;'><h4>No SUCO locations with valid FOS data available (Current Data)</h4></div>",
                       position = "topleft")
        )
      }
      
      # Get both colors and lookup exactly as documented
      foreman_colors <- get_themed_foreman_colors(theme = current_theme())
      foremen_lookup <- get_foremen_lookup()
      
      # Create mapping from foreman NUMBER to facility-based colors (same logic as plot)
      foremen_in_data <- unique(na.omit(data$foreman))
      emp_colors <- character(0)
      
      for (foreman_num in foremen_in_data) {
        foreman_num_str <- trimws(as.character(foreman_num))
        
        # Find the shortname for this foreman number
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
        
        if(length(matches) > 0) {
          shortname <- foremen_lookup$shortname[matches[1]]
          facility <- foremen_lookup$facility[matches[1]]
          
          # Get the facility-based color for this shortname
          if(shortname %in% names(foreman_colors)) {
            emp_colors[foreman_num_str] <- foreman_colors[shortname]
          }
        }
      }
      
      # Remove any NA colors
      emp_colors <- emp_colors[!is.na(emp_colors)]
      
      # Create ORDERED colors to ensure legend and map match exactly
      # Order by facility, then by foreman within facility (same as legend)
      ordered_foremen <- foremen_lookup[order(foremen_lookup$facility, foremen_lookup$shortname), ]
      ordered_emp_colors <- character(0)
      ordered_emp_numbers <- character(0)
      
      for (i in 1:nrow(ordered_foremen)) {
        emp_num <- trimws(as.character(ordered_foremen$emp_num[i]))
        if (emp_num %in% names(emp_colors)) {
          ordered_emp_colors <- c(ordered_emp_colors, emp_colors[emp_num])
          ordered_emp_numbers <- c(ordered_emp_numbers, emp_num)
        }
      }
      names(ordered_emp_colors) <- ordered_emp_numbers
      
      # Create color palette function for leaflet using ORDERED colors
      pal <- colorFactor(
        palette = ordered_emp_colors,
        domain = names(ordered_emp_colors),
        ordered = TRUE  # Maintain order
      )
      
      # Create popup text beforehand for foreman map too
      data <- data %>%
        mutate(
          popup_text_foreman = {
            # Get proper foreman names with robust matching
            foreman_names <- sapply(foreman, function(f) {
              if (!is.na(f) && f != "" && !is.null(f)) {
                # Ensure both foreman and emp_num are strings for comparison
                foreman_str <- trimws(as.character(f))
                
                # Find matching foreman in lookup table
                matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_str)
                
                if(length(matches) > 0) {
                  foremen_lookup$shortname[matches[1]]
                } else {
                  # Fallback: show the raw foreman ID if no match found
                  paste0("FOS #", foreman_str)
                }
              } else {
                "No FOS assigned"
              }
            })
            
            facility_names_vec <- sapply(facility, function(f) {
              fname <- facilities$full_name[facilities$short_name == f]
              if(length(fname) > 0) fname[1] else f
            })
            
            paste0("<b>Date:</b> ", inspdate, "<br>",
                   "<b>Facility:</b> ", facility_names_vec, "<br>",
                   "<b>FOS:</b> ", foreman_names, "<br>",
                   "<b>Location:</b> ", location, "<br>",
                   "<b>Species Count:</b> ", display_species_count, "<br>",
                   "<b>Species Found:</b><br>", species_summary)
          },
          marker_fill_opacity = ifelse(display_species_count == 0, 0.35, 0.8),
          marker_weight = ifelse(display_species_count == 0, 4, 1.5)
        )
      
      # Create map with foreman coloring
      leaflet(data) %>%
        addProviderTiles(basemap) %>%
        fitBounds(
          lng1 = min(st_coordinates(data)[,1]),
          lat1 = min(st_coordinates(data)[,2]),
          lng2 = max(st_coordinates(data)[,1]),
          lat2 = max(st_coordinates(data)[,2])
        ) %>%
        addCircleMarkers(
          radius = ~marker_size,
          color = "black",
          weight = ~marker_weight,
          fillColor = ~pal(foreman),
          fillOpacity = ~marker_fill_opacity,
          popup = ~popup_text_foreman
        ) %>%
        addLegend(
          position = "bottomright",
          title = "FOS (Current Data)",
          colors = ordered_emp_colors,  # Use same ordered colors as map palette
          labels = {
            # Use same ordering as the map palette
            ordered_labels <- character(0)
            for (i in 1:length(ordered_emp_numbers)) {
              emp_num <- ordered_emp_numbers[i]
              foreman_info <- foremen_lookup[trimws(as.character(foremen_lookup$emp_num)) == emp_num, ]
              if(nrow(foreman_info) > 0) {
                ordered_labels <- c(ordered_labels, foreman_info$shortname[1])
              }
            }
            ordered_labels
          },
          opacity = 0.8
        ) %>%
        addControl(
          html = '<div style="background: white; padding: 10px; border: 2px solid grey; border-radius: 5px;">
                  <div style="font-weight: bold; margin-bottom: 5px;">Marker Type</div>
                  <div style="margin-bottom: 3px;">
                    <span style="display: inline-block; width: 16px; height: 16px; border-radius: 50%; 
                                background-color: #1f77b4; border: 2px solid black; vertical-align: middle;"></span>
                    <span style="margin-left: 5px;">Target species present</span>
                  </div>
                  <div>
                    <span style="display: inline-block; width: 16px; height: 16px; border-radius: 50%; 
                                background-color: rgba(31, 119, 180, 0.35); border: 4px solid black; vertical-align: middle;"></span>
                    <span style="margin-left: 5px;">Zero target species</span>
                  </div>
                </div>',
          position = "bottomleft"
        )
    } else {
      # For MMCD (All) case, use a single color
      # Add marker styling for zero counts
      data <- data %>%
        mutate(
          marker_fill_opacity = ifelse(display_species_count == 0, 0.35, 0.8),
          marker_weight = ifelse(display_species_count == 0, 4, 1.5)
        )
      
      leaflet(data) %>%
        addProviderTiles(basemap) %>%
        fitBounds(
          lng1 = min(st_coordinates(data)[,1]),
          lat1 = min(st_coordinates(data)[,2]),
          lng2 = max(st_coordinates(data)[,1]),
          lat2 = max(st_coordinates(data)[,2])
        ) %>%
        addCircleMarkers(
          radius = ~marker_size,
          color = "black",
          weight = ~marker_weight,
          fillColor = "#1f77b4", # Standard blue color
          fillOpacity = ~marker_fill_opacity,
          popup = ~paste0("<b>Date:</b> ", inspdate, "<br>",
                          "<b>Facility:</b> ", facility, "<br>",
                          "<b>Foreman:</b> ", foreman, "<br>",
                          "<b>Location:</b> ", location, "<br>",
                          "<b>Species Count:</b> ", display_species_count, "<br>",
                          "<b>Species Found:</b><br>", species_summary)
        ) %>%
        addLegend(
          position = "bottomright",
          title = "MMCD (Current Data)",
          colors = "#1f77b4",
          labels = "All",
          opacity = 0.8
        ) %>%
        addControl(
          html = '<div style="background: white; padding: 10px; border: 2px solid grey; border-radius: 5px;">
                  <div style="font-weight: bold; margin-bottom: 5px;">Marker Type</div>
                  <div style="margin-bottom: 3px;">
                    <span style="display: inline-block; width: 16px; height: 16px; border-radius: 50%; 
                                background-color: #1f77b4; border: 2px solid black; vertical-align: middle;"></span>
                    <span style="margin-left: 5px;">Target species present</span>
                  </div>
                  <div>
                    <span style="display: inline-block; width: 16px; height: 16px; border-radius: 50%; 
                                background-color: rgba(31, 119, 180, 0.35); border: 4px solid black; vertical-align: middle;"></span>
                    <span style="margin-left: 5px;">Zero target species</span>
                  </div>
                </div>',
          position = "bottomleft"
        )
    }
  })
  
  # Generate current summary table
  output$current_summary_table <- renderDataTable({
    req(input$refresh)  # Only render after refresh button clicked
    inputs <- refresh_inputs()
    data <- filtered_data_current()
    summary_data <- create_summary_stats(data, inputs$group_by, "current")
    return(summary_data)
  }, options = list(pageLength = 15, 
                   searching = TRUE,
                   columnDefs = list(list(
                     targets = c("First_SUCO", "Last_SUCO"),
                     render = JS("function(data, type, row) {
                       return data ? data : 'N/A';
                     }")
                   ))))
  
  # Generate current detailed samples table
  output$current_detailed_table <- renderDataTable({
    req(input$refresh)  # Only render after refresh button clicked
    inputs <- refresh_inputs()
    data <- filtered_data_current()
    detailed_data <- create_detailed_samples_table(data, inputs$species_filter)
    return(detailed_data)
  }, options = list(pageLength = 25, 
                   searching = TRUE,
                   scrollX = TRUE,
                   columnDefs = list(list(
                     targets = c("Species_Found"),
                     render = JS("function(data, type, row) {
                       return data ? data.substring(0, 100) + (data.length > 100 ? '...' : '') : 'N/A';
                     }")
                   ))))
  
  # Generate current location plot
  output$current_location_plotly <- plotly::renderPlotly({
    req(input$refresh)  # Only render after refresh button clicked
    inputs <- refresh_inputs()
    data <- filtered_data_current()
    top_locations <- get_top_locations(data, inputs$top_locations_mode, inputs$species_filter)
    create_location_plotly(top_locations, "current", inputs$top_locations_mode, theme = current_theme())
  })
  
  # Handle current location plot clicks
  observe({
    click <- plotly::event_data("plotly_click", source = "current_location_plotly")
    if (!is.null(click)) {
      idx <- click$pointNumber + 1
      data <- filtered_data_current()
      top_locations <- get_top_locations(data, input$top_locations_mode, input$species_filter)
      if (idx > 0 && idx <= nrow(top_locations)) {
        loc <- top_locations$location[idx]
        spatial <- spatial_data_current()
        if (nrow(spatial) > 0 && loc %in% spatial$location) {
          point <- spatial[spatial$location == loc, ][1, ]
          coords <- sf::st_coordinates(point)
          lng <- coords[1]
          lat <- coords[2]
          if (!is.na(lng) && !is.na(lat)) {
            updateTabsetPanel(session, "current_tabset", selected = "CurrentMap")
            leafletProxy("current_map") %>%
              setView(lng = lng, lat = lat, zoom = 15) %>%
              addCircleMarkers(lng = lng, lat = lat, radius = 15, color = "red", fill = TRUE, fillOpacity = 0.7, layerId = "highlighted_location_current")
          }
        }
      }
    } else {
      leafletProxy("current_map") %>% clearGroup("highlighted_location_current")
    }
  })
  
  # Download handlers for CSV exports
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("suco_summary_all_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      inputs <- refresh_inputs()
      data <- filtered_data()
      summary_data <- create_summary_stats(data, inputs$group_by, "all")
      export_csv_safe(summary_data, file)
    }
  )
  
  output$download_detailed <- downloadHandler(
    filename = function() {
      paste0("suco_detailed_all_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      inputs <- refresh_inputs()
      data <- filtered_data()
      detailed_data <- create_detailed_samples_table(data, inputs$species_filter)
      export_csv_safe(detailed_data, file)
    }
  )
  
  output$download_current_summary <- downloadHandler(
    filename = function() {
      paste0("suco_summary_current_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      inputs <- refresh_inputs()
      data <- filtered_data_current()
      summary_data <- create_summary_stats(data, inputs$group_by, "current")
      export_csv_safe(summary_data, file)
    }
  )
  
  output$download_current_detailed <- downloadHandler(
    filename = function() {
      paste0("suco_detailed_current_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      inputs <- refresh_inputs()
      data <- filtered_data_current()
      detailed_data <- create_detailed_samples_table(data, inputs$species_filter)
      export_csv_safe(detailed_data, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)