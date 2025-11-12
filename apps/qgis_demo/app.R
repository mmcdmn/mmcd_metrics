# =============================================================================
# MMCD METRICS - QGIS Server Integration Demo
# =============================================================================
# This app demonstrates how to integrate QGIS Server with Shiny for dynamic
# map rendering. Users can filter data and refresh the map to see updates.
# =============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(DT)
  library(leaflet)
  library(dplyr)
  library(lubridate)
})

# Source shared database and utility functions
source("../../shared/db_helpers.R")

# =============================================================================
# QGIS HELPER FUNCTIONS
# =============================================================================

#' Generate WMS URL for QGIS Server
#' 
#' Creates a WMS GetMap URL for embedding QGIS maps in the Shiny app
#' 
#' @param project_name Name of the QGIS project file (without .qgs extension)
#' @param layers Comma-separated list of layer names to display
#' @param bbox Bounding box as c(xmin, ymin, xmax, ymax)
#' @param width Image width in pixels
#' @param height Image height in pixels
#' @param crs Coordinate reference system (default: EPSG:4326)
#' @return Complete WMS GetMap URL
generate_wms_url <- function(project_name, layers, bbox = NULL, 
                            width = 800, height = 600, 
                            crs = "EPSG:4326") {
  # Base QGIS Server URL (accessible from Apache proxy)
  # Use http://localhost:8080/qgis/ when accessing from host browser
  base_url <- "http://localhost:8080/qgis/"
  
  # Default bbox for Minnesota if not specified (rough bounds)
  if (is.null(bbox)) {
    bbox <- c(-97.5, 43.0, -89.5, 49.5)
  }
  
  bbox_str <- paste(bbox, collapse = ",")
  
  # Build WMS GetMap request
  params <- list(
    SERVICE = "WMS",
    VERSION = "1.3.0",
    REQUEST = "GetMap",
    MAP = paste0("/qgis/projects/", project_name, ".qgs"),
    LAYERS = layers,
    STYLES = "",
    FORMAT = "image/png",
    TRANSPARENT = "TRUE",
    CRS = crs,
    BBOX = bbox_str,
    WIDTH = width,
    HEIGHT = height
  )
  
  # Build query string
  query_string <- paste(
    names(params), 
    sapply(params, utils::URLencode, reserved = TRUE), 
    sep = "=", 
    collapse = "&"
  )
  
  return(paste0(base_url, "?", query_string))
}

#' Generate a basic QGIS project XML for PostGIS layer
#' 
#' Creates a simple .qgs project file that connects to PostGIS
#' 
#' @param project_path Full path where to save the .qgs file
#' @param layer_name Name for the layer in QGIS
#' @param table_name PostGIS table name
#' @param geometry_column Name of geometry column (default: geom)
#' @param sql_filter Optional SQL WHERE clause for filtering
#' @return TRUE if successful, FALSE otherwise
create_qgis_project <- function(project_path, layer_name, table_name, 
                                geometry_column = "geom", sql_filter = NULL) {
  
  # Get database connection parameters
  load_env_vars()
  db_host <- Sys.getenv("DB_HOST")
  db_port <- Sys.getenv("DB_PORT", "5432")
  db_user <- Sys.getenv("DB_USER")
  db_password <- Sys.getenv("DB_PASSWORD")
  db_name <- Sys.getenv("DB_NAME")
  
  # Build PostGIS connection string
  # Note: Password in connection string is OK for internal QGIS project files
  postgis_uri <- sprintf(
    "dbname='%s' host=%s port=%s user='%s' password='%s' sslmode=disable key='gid' srid=4326 type=MultiPolygon table=\"public\".\"%s\" (%s) sql=%s",
    db_name, db_host, db_port, db_user, db_password, 
    table_name, geometry_column, 
    ifelse(is.null(sql_filter), "", sql_filter)
  )
  
  # Basic QGIS project XML template
  # This is a simplified version - in production you'd want more complete XML
  qgs_xml <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<qgis projectname="%s" version="3.28.0">
  <properties>
    <WMSServiceTitle type="QString">%s</WMSServiceTitle>
    <WMSExtent type="QgsRectangle">
      <xmin>-97.5</xmin>
      <ymin>43.0</ymin>
      <xmax>-89.5</xmax>
      <ymax>49.5</ymax>
    </WMSExtent>
  </properties>
  <projectlayers>
    <maplayer>
      <id>layer1</id>
      <datasource>%s</datasource>
      <layername>%s</layername>
      <provider>postgres</provider>
      <srs>
        <spatialrefsys>
          <proj4>+proj=longlat +datum=WGS84 +no_defs</proj4>
          <srsid>3452</srsid>
          <srid>4326</srid>
          <authid>EPSG:4326</authid>
        </spatialrefsys>
      </srs>
    </maplayer>
  </projectlayers>
</qgis>', layer_name, layer_name, postgis_uri, layer_name)
  
  # Write to file
  tryCatch({
    writeLines(qgs_xml, project_path)
    return(TRUE)
  }, error = function(e) {
    warning(paste("Failed to create QGIS project:", e$message))
    return(FALSE)
  })
}

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "QGIS Demo - Live Maps",
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Map View", tabName = "map", icon = icon("map")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    
    # Filters
    hr(),
    h4("Filter Options", style = "padding-left: 15px;"),
    
    selectInput(
      "facility_filter",
      "Facility:",
      choices = c("All" = "all"),
      selected = "all"
    ),
    
    dateRangeInput(
      "date_range",
      "Date Range:",
      start = Sys.Date() - 30,
      end = Sys.Date(),
      max = Sys.Date()
    ),
    
    # Action button to refresh map
    actionButton(
      "refresh_map",
      "Refresh Map",
      icon = icon("refresh"),
      class = "btn-primary btn-block",
      style = "margin: 15px;"
    ),
    
    # Info box
    box(
      width = 12,
      background = "light-blue",
      p(strong("How it works:")),
      p("1. Adjust filters above"),
      p("2. Click 'Refresh Map'"),
      p("3. QGIS Server renders filtered data"),
      p("4. Map updates dynamically")
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #ecf0f5; }
        .qgis-map-container { 
          border: 2px solid #3c8dbc; 
          border-radius: 4px;
          overflow: hidden;
          background: white;
        }
        .info-box { margin-bottom: 15px; }
      "))
    ),
    
    tabItems(
      # Map tab
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            width = 12,
            title = "QGIS Server Map Viewer",
            status = "primary",
            solidHeader = TRUE,
            
            # Info text
            p("This demonstrates QGIS Server integration. The map below is rendered by QGIS Server based on your filter selections."),
            
            # Leaflet map with WMS overlay option
            leafletOutput("qgis_leaflet_map", height = 600)
          )
        ),
        
        fluidRow(
          valueBoxOutput("total_records", width = 4),
          valueBoxOutput("date_range_display", width = 4),
          valueBoxOutput("last_updated", width = 4)
        )
      ),
      
      # Table tab
      tabItem(
        tabName = "table",
        fluidRow(
          box(
            width = 12,
            title = "Filtered Data",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("data_table")
          )
        )
      ),
      
      # About tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12,
            title = "About QGIS Server Integration",
            status = "info",
            solidHeader = TRUE,
            
            h3("What is this demo?"),
            p("This application demonstrates the integration of QGIS Server with R Shiny. 
              It shows how you can create dynamic, filterable maps using QGIS Server's 
              rendering capabilities combined with Shiny's interactive UI."),
            
            h3("How does it work?"),
            tags$ol(
              tags$li("User selects filters in the Shiny UI (facility, date range, etc.)"),
              tags$li("When 'Refresh Map' is clicked, a QGIS project file (.qgs) is generated dynamically"),
              tags$li("The .qgs file contains SQL filters based on user selections"),
              tags$li("QGIS Server renders the filtered PostGIS data as WMS tiles"),
              tags$li("The WMS layer is displayed in the Leaflet map viewer")
            ),
            
            h3("Architecture"),
            p(strong("Technology Stack:")),
            tags$ul(
              tags$li("R Shiny: UI and filter controls"),
              tags$li("QGIS Server: Map rendering engine"),
              tags$li("PostGIS: Spatial database"),
              tags$li("Leaflet: Map display library"),
              tags$li("Apache: Web server proxy for QGIS Server")
            ),
            
            h3("Advantages of QGIS Server"),
            tags$ul(
              tags$li("Professional cartographic rendering"),
              tags$li("Complex symbology and styling"),
              tags$li("Server-side rendering reduces client load"),
              tags$li("Standard WMS/WFS protocols"),
              tags$li("Can handle large datasets efficiently")
            ),
            
            h3("Next Steps"),
            p("To use this in production:"),
            tags$ol(
              tags$li("Create properly styled QGIS projects with your data layers"),
              tags$li("Implement dynamic SQL filtering based on UI controls"),
              tags$li("Add legend and layer controls"),
              tags$li("Optimize QGIS Server configuration for performance"),
              tags$li("Add caching strategies for frequently accessed maps")
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # Load facility choices on startup
  observe({
    facilities <- get_facility_choices(include_all = TRUE)
    updateSelectInput(session, "facility_filter", choices = facilities)
  })
  
  # Reactive values for map state
  map_state <- reactiveValues(
    last_refresh = NULL,
    current_project = "default",
    wms_url = NULL
  )
  
  # Get filtered data based on selections
  filtered_data <- reactive({
    # Connect to database
    con <- get_db_connection()
    if (is.null(con)) {
      return(data.frame())
    }
    
    # Build query based on filters
    # Using gis_facility as a demo table - replace with your actual table
    query <- "SELECT 
                abbrv as facility,
                city as city,
                ST_X(ST_Centroid(geom)) as lon,
                ST_Y(ST_Centroid(geom)) as lat,
                ST_AsText(geom) as geometry
              FROM public.gis_facility
              WHERE 1=1"
    
    # Apply facility filter
    if (!is.null(input$facility_filter) && input$facility_filter != "all") {
      query <- paste0(query, sprintf(" AND abbrv = '%s'", input$facility_filter))
    }
    
    query <- paste0(query, " ORDER BY abbrv")
    
    tryCatch({
      data <- dbGetQuery(con, query)
      dbDisconnect(con)
      return(data)
    }, error = function(e) {
      if (!is.null(con)) dbDisconnect(con)
      warning(paste("Error querying data:", e$message))
      return(data.frame())
    })
  })
  
  # Generate QGIS project when refresh is clicked
  observeEvent(input$refresh_map, {
    # Build SQL filter based on current selections
    sql_filter <- "1=1"
    
    if (!is.null(input$facility_filter) && input$facility_filter != "all") {
      sql_filter <- sprintf("abbrv = '%s'", input$facility_filter)
    }
    
    # Generate project name based on filters (to cache different filter combinations)
    project_name <- paste0("facilities_", 
                          ifelse(input$facility_filter == "all", "all", input$facility_filter),
                          "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    
    project_path <- file.path("/qgis/projects", paste0(project_name, ".qgs"))
    
    # Create QGIS project
    success <- create_qgis_project(
      project_path = project_path,
      layer_name = "Facilities",
      table_name = "gis_facility",
      geometry_column = "geom",
      sql_filter = sql_filter
    )
    
    if (success) {
      map_state$current_project <- project_name
      map_state$last_refresh <- Sys.time()
      
      # Generate WMS URL for this project
      map_state$wms_url <- generate_wms_url(
        project_name = project_name,
        layers = "Facilities",
        bbox = c(-97.5, 43.0, -89.5, 49.5),
        width = 1200,
        height = 800
      )
      
      showNotification("Map refreshed successfully!", type = "message")
    } else {
      showNotification("Failed to create QGIS project", type = "error")
    }
  })
  
  # Render Leaflet map with WMS overlay
  output$qgis_leaflet_map <- renderLeaflet({
    # Start with a base leaflet map
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = -93.5, lat = 46.0, zoom = 7)
    
    # Add WMS layer if available
    if (!is.null(map_state$wms_url)) {
      # For WMS in Leaflet, we use addWMSTiles
      map <- map %>%
        addWMSTiles(
          baseUrl = "http://localhost:8080/qgis/",
          layers = "Facilities",
          options = WMSTileOptions(
            format = "image/png",
            transparent = TRUE,
            version = "1.3.0",
            crs = "EPSG:4326"
          ),
          attribution = "QGIS Server | MMCD"
        )
    }
    
    # Add markers from filtered data
    data <- filtered_data()
    if (nrow(data) > 0) {
      map <- map %>%
        addMarkers(
          lng = data$lon,
          lat = data$lat,
          popup = paste0("<b>", data$facility, "</b><br>", data$city)
        )
    }
    
    map
  })
  
  # Data table output
  output$data_table <- renderDT({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(datatable(data.frame(Message = "No data available")))
    }
    
    # Remove geometry column for display
    display_data <- data %>%
      select(-geometry) %>%
      rename(
        Facility = facility,
        City = city,
        Longitude = lon,
        Latitude = lat
      )
    
    datatable(
      display_data,
      options = list(
        pageLength = 25,
        searching = TRUE,
        ordering = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Value boxes
  output$total_records <- renderValueBox({
    data <- filtered_data()
    valueBox(
      nrow(data),
      "Total Records",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$date_range_display <- renderValueBox({
    valueBox(
      paste(format(input$date_range[1], "%m/%d/%y"), "-", 
            format(input$date_range[2], "%m/%d/%y")),
      "Date Range",
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$last_updated <- renderValueBox({
    last_refresh <- map_state$last_refresh
    display_time <- if (is.null(last_refresh)) {
      "Not yet refreshed"
    } else {
      format(last_refresh, "%H:%M:%S")
    }
    
    valueBox(
      display_time,
      "Last Map Refresh",
      icon = icon("clock"),
      color = "yellow"
    )
  })
}

# =============================================================================
# RUN APPLICATION
# =============================================================================

shinyApp(ui = ui, server = server)
