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
source("data_functions.R")
source("display_functions.R")
source("ui_helpers.R")

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
  # Application title
  titlePanel("SUCO Analysis Dashboard"),

  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      # Group by selection
      h4("Analysis Options"),
      radioButtons("group_by", "Group By:",
                   choices = c("MMCD (All)" = "mmcd_all",
                               "Facility" = "facility", 
                               "FOS" = "foreman"),
                   selected = "mmcd_all"),
      
      hr(),
      
      # Date shortcuts
      h4("Date Selection"),
      fluidRow(
        column(4, actionButton("this_year", "This Year", class = "btn-primary btn-sm", style = "width: 100%;")),
        column(4, actionButton("this_month", "This Month", class = "btn-info btn-sm", style = "width: 100%;")),
        column(4, actionButton("this_week", "This Week", class = "btn-success btn-sm", style = "width: 100%;"))
      ),
      br(),
      
      # Year slider (only visible for All Data tab with multi-year capability)
      conditionalPanel(
        condition = "input.main_tabset == 'All Data (Current + Archive)'",
        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}",
                   ".irs-grid-text {font-size: 8pt;}"),
        sliderInput("year_range", "Year Range:",
                    min = 2015, max = as.numeric(format(Sys.Date(), "%Y")),
                    value = c(as.numeric(format(Sys.Date(), "%Y")), as.numeric(format(Sys.Date(), "%Y"))),
                    sep = "", step = 1),
        br()
      ),
      
      # Date range selection - behavior depends on which tab is active
      dateRangeInput("date_range", "Custom Date Range:",
                     start = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                     end = Sys.Date(),
                     format = "yyyy-mm-dd"),
      
      hr(),
      
      # Filter by zone (P1/P2)
      h4("Location Filters"),
      checkboxGroupInput("zone_filter", "Filter by Zone:",
                        choices = c("P1" = "1", "P2" = "2"),
                        selected = c("1", "2")),
      
      # Filter by facility (multi-select)
      selectizeInput("facility_filter", "Filter by Facility:",
                  choices = c("All"),  # Will be populated from get_facility_lookup()
                  selected = "All", multiple = TRUE),
      
      # Filter by foreman (multi-select, populated dynamically)
      selectizeInput("foreman_filter", "Filter by FOS:",
                  choices = c("All"),
                  selected = "All", multiple = TRUE),
      
      # Add species filter to sidebarPanel
      selectInput("species_filter", "Filter by Species:", choices = c("All"), selected = "All"),
      
      hr(),
      
      # Display and visualization options
      h4("Display Options"),
      # Graph type selector
      selectInput("graph_type", "Graph Type:",
                  choices = c("Bar" = "bar", "Line" = "line", "Point" = "point", "Area" = "area"),
                  selected = "bar"),
      # Map Controls
      conditionalPanel(
        condition = "input.tabset == 'Map'",
        hr(),
        h4("Map Options"),
        
        # Base map selection
        selectInput("basemap", "Base Map:",
                    choices = c("OpenStreetMap" = "osm",
                                "Carto Light" = "carto",
                                "Esri Satellite" = "satellite"),
                    selected = "carto"),
        
        # Marker size adjustment
        sliderInput("marker_size", "Marker Size Multiplier:",
                    min = 1, max = 5, value = 3, step = 0.5)
      )
    ),
    
    # Main panel for displaying the graphs
    mainPanel(
      tabsetPanel(id = "main_tabset",
        tabPanel("Current Data", 
          tabsetPanel(id = "current_tabset",
            tabPanel("Graph", value = "CurrentGraph", plotOutput("current_trend_plot", height = "500px")),
            tabPanel("Map", value = "CurrentMap", leafletOutput("current_map", height = "600px")),
            tabPanel("Summary Table", value = "CurrentTable", dataTableOutput("current_summary_table")),
            tabPanel("Top Locations", value = "CurrentTopLoc", plotlyOutput("current_location_plotly", height = "500px"))
          )
        ),
        tabPanel("All Data (Current + Archive)",
          tabsetPanel(id = "all_tabset",
            tabPanel("Graph", value = "AllGraph", plotOutput("trend_plot", height = "500px")),
            tabPanel("Map", value = "AllMap", leafletOutput("map", height = "600px")),
            tabPanel("Summary Table", value = "AllTable", dataTableOutput("summary_table")),
            tabPanel("Top Locations", value = "AllTopLoc", plotlyOutput("location_plotly", height = "500px"))
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to prevent infinite loops between date controls
  updating_date_range <- reactiveVal(FALSE)
  updating_year_range <- reactiveVal(FALSE)
  
  # Date shortcut handlers - behavior depends on active tab
  observeEvent(input$this_year, {
    handle_date_shortcut("year", session, input, updating_date_range, updating_year_range)
  })
  
  observeEvent(input$this_month, {
    handle_date_shortcut("month", session, input, updating_date_range, updating_year_range)
  })
  
  observeEvent(input$this_week, {
    handle_date_shortcut("week", session, input, updating_date_range, updating_year_range)
  })
  
  # Year slider handler - only works when on All Data tab and not updating
  observeEvent(input$year_range, {
    handle_year_range_change(input, session, updating_date_range, updating_year_range)
  }, ignoreInit = TRUE)  # Ignore initial trigger
  
  # Custom date range handler - behavior depends on active tab and not updating
  observeEvent(input$date_range, {
    handle_date_range_change(input, session, updating_date_range, updating_year_range)
  }, ignoreInit = TRUE)  # Ignore initial trigger
  
  # Initialize facility choices from db_helpers
  observe({
    facilities <- get_facility_lookup()
    # Create named vector with full names as labels and short names as values
    facility_choices <- c("All" = "All")
    facility_choices <- c(
      facility_choices,
      setNames(facilities$short_name, facilities$full_name)
    )
    updateSelectizeInput(session, "facility_filter", choices = facility_choices)
  })
  
  # Fetch SUCO data - all data (current + archive)
  suco_data <- reactive({
    get_suco_data("all", input$date_range)
  })
  
  # Fetch SUCO data - current only (more efficient for Current Data tab)
  suco_data_current <- reactive({
    # Simple checks only
    req(input$date_range, input$main_tabset)
    
    # Only run when actually on Current Data tab
    if (input$main_tabset != "Current Data") {
      return(data.frame())
    }
    
    tryCatch({
      get_suco_data("current", input$date_range)
    }, error = function(e) {
      cat("Error in suco_data_current:", e$message, "\n")
      return(data.frame())
    })
  })
  
  # Helper function to create trend plots - eliminates code duplication
  # Update species filter choices based on available data (use species_name)
  observe({
    data <- suco_data()
    
    # Get unique species
    spp_choices <- sort(unique(na.omit(data$species_name)))
    spp_choices <- c("All", spp_choices)
    
    # Update select input
    updateSelectInput(session, "species_filter", choices = spp_choices)
  })
  
  # Update foreman filter choices based on available data (multi-select aware)
  observe({
    data <- suco_data()
    foremen_lookup <- get_foremen_lookup()
    
    # Filter by facility if not 'All' and not empty
    if (!is.null(input$facility_filter) && !("All" %in% input$facility_filter)) {
      data <- data %>% filter(facility %in% input$facility_filter)
    }
    
    # Get unique foremen numbers from data
    foremen_nums <- sort(unique(na.omit(data$foreman)))
    
    # Create mapping from emp_num to shortname for display with robust matching
    foremen_names <- sapply(foremen_nums, function(num) {
      # Ensure both sides are strings and trimmed
      num_str <- trimws(as.character(num))
      matches <- which(trimws(as.character(foremen_lookup$emp_num)) == num_str)
      if(length(matches) > 0) {
        foremen_lookup$shortname[matches[1]]
      } else {
        paste0("FOS #", num_str)  # fallback to formatted number
      }
    })
    
    # Create choices with names as labels and numbers as values
    foremen_choices <- setNames(
      c("All", foremen_nums),
      c("All", foremen_names)
    )
    
    updateSelectInput(session, "foreman_filter", choices = foremen_choices)
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
    create_spatial_data(data)
  })
  
  # Process spatial data for mapping (current data only)
  spatial_data_current <- reactive({
    data <- filtered_data_current()
    create_spatial_data(data)
  })
  
  # Aggregate data by selected time interval and grouping
  aggregated_data <- reactive({
    data <- filtered_data()
    aggregate_suco_data(data, input$group_by, input$zone_filter)
  })
  
  # Aggregate current data by selected time interval and grouping
  aggregated_data_current <- reactive({
    data <- filtered_data_current()
    aggregate_suco_data(data, input$group_by, input$zone_filter)
  })
  
  # Generate trend plot
  output$trend_plot <- renderPlot({
    create_trend_plot(aggregated_data, aggregated_data_current, input, "all")
  })
  
  # Generate current trend plot (for current data tab)
  output$current_trend_plot <- renderPlot({
    create_trend_plot(aggregated_data, aggregated_data_current, input, "current")
  })
  
  # Generate map
  output$map <- renderLeaflet({
    create_suco_map(spatial_data(), input, "all")
  })
  
  # Generate current map (for current data tab)
  output$current_map <- renderLeaflet({
    create_suco_map(spatial_data_current(), input, "current")
  })


  # Generate summary table
  output$summary_table <- renderDataTable({
    data <- filtered_data()
    summary_data <- create_summary_stats(data, input$group_by, "all")
    return(summary_data)
  }, options = list(pageLength = 15, 
                   searching = TRUE,
                   columnDefs = list(list(
                     targets = c("First_SUCO", "Last_SUCO"),
                     render = JS("function(data, type, row) {
                       return data ? data : 'N/A';
                     }")
                   ))))
  
  # Generate location plot (top locations with most SUCOs)
  output$location_plotly <- plotly::renderPlotly({
    data <- filtered_data()
    top_locations <- get_top_locations(data)
    create_location_plotly(top_locations, "all")
  })
  
  # React to plotly click and update map and tab
  observe({
    click <- plotly::event_data("plotly_click", source = "location_plotly")
    if (!is.null(click)) {
      idx <- click$pointNumber + 1  # R is 1-based
      data <- filtered_data()
      top_locations <- data %>%
        group_by(location) %>%
        summarize(visits = n(), .groups = "drop") %>%
        arrange(desc(visits)) %>%
        head(15)
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
    # Use spatial_data_current() instead of spatial_data()
    data <- spatial_data_current()
    
    # Get marker size multiplier
    size_multiplier <- input$marker_size
    
    # Get facility and foremen lookups for display names
    facilities <- get_facility_lookup()
    
    # Set up basemap provider
    basemap <- switch(input$basemap,
                      "osm" = providers$OpenStreetMap,
                      "carto" = providers$CartoDB.Positron,
                      "terrain" = providers$Stamen.Terrain,
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
      facility_colors <- get_facility_base_colors()
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
                   "<b>Field Count:</b> ", fieldcount, "<br>",
                   "<b>Data Source:</b> Current Only")
          }
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
          radius = ~pmin(15, (3 * size_multiplier)),
          color = "black",
          weight = 1.5,
          fillColor = ~pal(facility),
          fillOpacity = 0.8,
          popup = ~popup_text
        ) %>%
        addLegend(
          position = "bottomright",
          title = "Facility (Current Data)",
          colors = facility_colors,
          labels = legend_labels,
          opacity = 0.8
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
      foreman_colors <- get_foreman_colors()
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
                   "<b>Field Count:</b> ", fieldcount, "<br>",
                   "<b>Data Source:</b> Current Only")
          }
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
          radius = ~pmin(15, (3 * size_multiplier)),
          color = "black",
          weight = 1.5,
          fillColor = ~pal(foreman),
          fillOpacity = 0.8,
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
        )
    } else {
      # For MMCD (All) case, use a single color
      leaflet(data) %>%
        addProviderTiles(basemap) %>%
        fitBounds(
          lng1 = min(st_coordinates(data)[,1]),
          lat1 = min(st_coordinates(data)[,2]),
          lng2 = max(st_coordinates(data)[,1]),
          lat2 = max(st_coordinates(data)[,2])
        ) %>%
        addCircleMarkers(
          radius = ~pmin(15, (3 * size_multiplier)),
          color = "black",
          weight = 1.5,
          fillColor = "#1f77b4", # Standard blue color
          fillOpacity = 0.8,
          popup = ~paste0("<b>Date:</b> ", inspdate, "<br>",
                          "<b>Facility:</b> ", facility, "<br>",
                          "<b>Foreman:</b> ", foreman, "<br>",
                          "<b>Location:</b> ", location, "<br>",
                          "<b>Field Count:</b> ", fieldcount, "<br>",
                          "<b>Data Source:</b> Current Only")
        ) %>%
        addLegend(
          position = "bottomright",
          title = "MMCD (Current Data)",
          colors = "#1f77b4",
          labels = "All",
          opacity = 0.8
        )
    }
  })
  
  # Generate current summary table
  output$current_summary_table <- renderDataTable({
    data <- filtered_data_current()
    summary_data <- create_summary_stats(data, input$group_by, "current")
    return(summary_data)
  }, options = list(pageLength = 15, 
                   searching = TRUE,
                   columnDefs = list(list(
                     targets = c("First_SUCO", "Last_SUCO"),
                     render = JS("function(data, type, row) {
                       return data ? data : 'N/A';
                     }")
                   ))))
  
  # Generate current location plot
  output$current_location_plotly <- plotly::renderPlotly({
    data <- filtered_data_current()
    top_locations <- get_top_locations(data)
    create_location_plotly(top_locations, "current")
  })
  
  # Handle current location plot clicks
  observe({
    click <- plotly::event_data("plotly_click", source = "current_location_plotly")
    if (!is.null(click)) {
      idx <- click$pointNumber + 1
      data <- filtered_data_current()
      top_locations <- data %>%
        group_by(location) %>%
        summarize(visits = n(), .groups = "drop") %>%
        arrange(desc(visits)) %>%
        head(15)
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
}

# Run the application
shinyApp(ui = ui, server = server)