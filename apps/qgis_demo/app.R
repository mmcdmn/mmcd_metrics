# =============================================================================
# MMCD METRICS - QGIS Server Trap Surveillance Heat Map
# =============================================================================
# This app creates a heat map visualization of trap surveillance data using
# QGIS Server for cartographic rendering. It uses the same SQL
# and concept as trap_survillance_test but renders through QGIS.
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
  library(sf)
})

# Source shared database and utility functions
source("../../shared/db_helpers.R")
source("data_functions.R")
source("qgis_helpers.R")



# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "Trap Surveillance Heat Map (QGIS)",
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Heat Map", tabName = "map", icon = icon("fire")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    
    # Filters
    hr(),
    h4("Filter Options", style = "padding-left: 15px;"),
    
    dateInput(
      "analysis_date",
      "Analysis Date:",
      value = Sys.Date(),
      max = Sys.Date()
    ),
    
    selectizeInput(
      "species",
      "Species Filter:",
      choices = c("All Species" = "all"),
      selected = "all",
      multiple = TRUE,
      options = list(placeholder = "Select species...")
    ),
    
    checkboxGroupInput(
      "trap_types",
      "Trap Types:",
      choices = c(
        "Elevated CO2" = "4",
        "Gravid Trap" = "5",
        "CO2 Overnight" = "6"
      ),
      selected = c("4", "5", "6")
    ),
    
    sliderInput(
      "k_neighbors",
      "k-Nearest Neighbors:",
      min = 1,
      max = 10,
      value = 4,
      step = 1
    ),
    
    selectizeInput(
      "facility",
      "Facility Filter:",
      choices = c("All" = "all"),
      selected = "all",
      multiple = TRUE
    ),
    
    # Action button to refresh map
    actionButton(
      "refresh",
      "Refresh Heat Map",
      icon = icon("refresh"),
      class = "btn-primary btn-block",
      style = "margin: 15px;"
    ),
    
    # Info box
    box(
      width = 12,
      background = "light-blue",
      p(strong("QGIS Rendering:")),
      p("This map uses QGIS Server for professional cartographic rendering with heat map styling."),
      p(strong("Vector Index:")),
      p("k-NN inverse-distance weighted mosquito counts.")
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #ecf0f5; }
        .info-box { margin-bottom: 15px; }
        .legend-box {
          background: white;
          padding: 10px;
          border-radius: 4px;
          border: 2px solid #3c8dbc;
          margin-top: 10px;
        }
      "))
    ),
    
    tabItems(
      # Map tab
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            width = 12,
            title = "Trap Surveillance Vector Index Heat Map",
            status = "primary",
            solidHeader = TRUE,
            
            # Info text
            p("Heat map showing mosquito activity based on trap surveillance data. 
              The vector index represents a distance-weighted estimate of mosquito abundance 
              for each section based on nearby trap counts."),
            
            # Leaflet map with QGIS WMS layer
            leafletOutput("heat_map", height = 600),
            
            # Legend
            div(
              class = "legend-box",
              h4("Legend", style = "margin-top: 0;"),
              p(strong("Heat Map Colors:"), " Yellow (low) → Orange → Red (high) mosquito activity"),
              p(strong("Circle Size:"), " Proportional to log(vector index + 1)"),
              p(strong("Blue Markers:"), " Individual trap locations with species counts")
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("total_sections", width = 3),
          valueBoxOutput("total_traps", width = 3),
          valueBoxOutput("avg_vector_index", width = 3),
          valueBoxOutput("last_updated", width = 3)
        )
      ),
      
      # Table tab
      tabItem(
        tabName = "table",
        fluidRow(
          box(
            width = 12,
            title = "Section Vector Index Data",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("section_table")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Trap Data",
            status = "info",
            solidHeader = TRUE,
            DTOutput("trap_table")
          )
        )
      ),
      
      # About tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12,
            title = "About This Application",
            status = "info",
            solidHeader = TRUE,
            
            h3("Trap Surveillance Heat Map with QGIS"),
            p("This application combines the analytical power of R with the cartographic 
              capabilities of QGIS Server to create professional heat map visualizations 
              of mosquito trap surveillance data."),
            
            h3("How It Works"),
            tags$ol(
              tags$li("R queries PostgreSQL for trap surveillance and species count data"),
              tags$li("Vector index is calculated using k-NN inverse-distance weighting"),
              tags$li("Results are written to a temporary PostGIS table or loaded into GPKG"),
              tags$li("QGIS project (.qgs) is generated dynamically with heat map styling"),
              tags$li("QGIS Server renders the styled data as WMS tiles"),
              tags$li("Leaflet displays the WMS layer along with interactive markers")
            ),
            
            h3("Vector Index Calculation"),
            p(strong("Algorithm:"), " k-Nearest Neighbors with Inverse Distance Weighting"),
            p("For each section:"),
            tags$ul(
              tags$li("Find k nearest trap locations (default k=4)"),
              tags$li("Calculate inverse distance weights: weight = 1/distance"),
              tags$li("Compute weighted average: Σ(weight × count) / Σ(weight)")
            ),
            p("This provides a spatial interpolation of mosquito activity across all sections, 
              even those without traps, based on nearby trap surveillance data."),
            
            h3("Data Sources"),
            tags$ul(
              tags$li(strong("dbadult_insp_current:"), " Trap inspection records"),
              tags$li(strong("dbadult_species_current:"), " Species counts per inspection"),
              tags$li(strong("gis_sectcode:"), " Section polygons (from PostGIS)"),
              tags$li(strong("GPKG files:"), " Additional spatial layers for context")
            ),
            
            h3("GPKG Layers Available"),
            tags$ul(
              tags$li(strong("MMCD_Sections_2025.gpkg:"), " Latest section boundaries"),
              tags$li(strong("7counties.gpkg:"), " Seven-county metro area boundaries"),
              tags$li(strong("MMCD_FacilityArea.gpkg:"), " MMCD facility boundaries"),
              tags$li(strong("2020CensusBlockAnalysis.gpkg:"), " Census data for analysis")
            ),
            
            h3("Technology Stack"),
            tags$ul(
              tags$li("R Shiny: Interactive UI and data processing"),
              tags$li("QGIS Server: Professional map rendering with heat map styling"),
              tags$li("PostGIS: Spatial database for trap and section data"),
              tags$li("GeoPackage: Portable spatial data layers"),
              tags$li("Leaflet: Web map display library"),
              tags$li("Apache: Web server proxy for QGIS Server")
            ),
            
            h3("Advantages of QGIS Server Approach"),
            tags$ul(
              tags$li("Professional cartographic rendering with advanced symbology"),
              tags$li("True heat map interpolation (not just colored circles)"),
              tags$li("Server-side rendering reduces client load"),
              tags$li("Can handle very large datasets efficiently"),
              tags$li("Standardized WMS/WFS protocols for interoperability"),
              tags$li("Consistent styling across platforms")
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
  
  # Initialize selectors - load species and facility choices
  observe({
    species_choices <- get_species_choices()
    updateSelectizeInput(session, "species", choices = species_choices, selected = "all")
    
    facility_choices <- get_facility_choices(include_all = TRUE)
    updateSelectizeInput(session, "facility", choices = facility_choices, selected = "all")
  })
  
  # Reactive values for map state
  map_state <- reactiveValues(
    last_refresh = NULL,
    current_project = NULL,
    wms_url = NULL,
    data = NULL
  )
  
  # Compute vector index and generate QGIS project when refresh is clicked
  observeEvent(input$refresh, {
    req(input$species)
    req(input$trap_types)
    
    withProgress(message = 'Processing data...', value = 0, {
      tryCatch({
        # Step 1: Compute vector index (25%)
        incProgress(0.25, detail = "Computing vector index...")
        
        result <- compute_section_vector_index(
          species_codes = input$species,
          analysis_date = input$analysis_date,
          k = input$k_neighbors,
          facility_filter = input$facility,
          trap_types = input$trap_types
        )
        
        if (nrow(result$sections) == 0) {
          showNotification("No data found with current filters", type = "warning", duration = 5)
          return()
        }
        
        # Store data for display
        map_state$data <- result
        
        # Step 2: Generate QGIS project (50%)
        incProgress(0.25, detail = "Generating QGIS project...")
        
        project_name <- generate_qgis_heatmap_project(
          sections_data = result$sections,
          traps_data = result$traps,
          analysis_date = as.character(input$analysis_date),
          species_label = if ("all" %in% tolower(input$species)) {
            "All Species"
          } else {
            paste(length(input$species), "selected species")
          }
        )
        
        if (!is.null(project_name)) {
          map_state$current_project <- project_name
          map_state$last_refresh <- Sys.time()
          
          # Step 3: Generate WMS URL (75%)
          incProgress(0.25, detail = "Preparing map tiles...")
          
          # Calculate bounding box from sections data
          bbox <- c(
            min(result$sections$lon) - 0.5,
            min(result$sections$lat) - 0.5,
            max(result$sections$lon) + 0.5,
            max(result$sections$lat) + 0.5
          )
          
          map_state$wms_url <- generate_wms_url(
            project_name = project_name,
            layers = "sections_heatmap",
            bbox = bbox,
            width = 1200,
            height = 800
          )
          
          incProgress(0.25, detail = "Complete!")
          showNotification("Heat map generated successfully!", type = "message", duration = 3)
        } else {
          showNotification("Failed to create QGIS project", type = "error", duration = 5)
        }
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        message("Error in heat map generation: ", e$message)
      })
    })
  })
  
  # Render heat map with QGIS WMS layer
  output$heat_map <- renderLeaflet({
    # Trigger re-render when refresh is clicked
    map_state$last_refresh
    
    data <- map_state$data
    
    # Start with a base leaflet map
    map <- leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      setView(lng = -93.5, lat = 45.0, zoom = 9)
    
    # Add QGIS WMS layer if project exists
    if (!is.null(map_state$current_project) && nchar(map_state$current_project) > 0) {
      project_file <- paste0("/qgis/projects/", map_state$current_project, ".qgs")
      
      tryCatch({
        map <- map %>%
          addWMSTiles(
            baseUrl = "http://localhost:8080/qgis/",
            layers = "sections_heatmap",
            options = WMSTileOptions(
              format = "image/png",
              transparent = TRUE,
              version = "1.3.0",
              crs = "EPSG:4326",
              map = project_file
            ),
            attribution = "QGIS Server | MMCD",
            group = "Heat Map"
          )
      }, error = function(e) {
        message("WMS layer error: ", e$message)
      })
    }
    
    # Add section markers (circles) if data is available
    if (!is.null(data) && nrow(data$sections) > 0) {
      sections <- data$sections
      
      # Color palette for vector index
      pal <- colorNumeric(
        palette = c("#FFFF00", "#FFA500", "#FF4500", "#FF0000"),
        domain = sections$vector_index,
        na.color = "#808080"
      )
      
      map <- map %>%
        addCircleMarkers(
          lng = sections$lon,
          lat = sections$lat,
          radius = log1p(sections$vector_index) * 4,
          color = "#000000",
          weight = 1,
          fillColor = ~pal(vector_index),
          fillOpacity = 0.6,
          popup = paste0(
            "<b>Section:</b> ", sections$sectcode, "<br>",
            "<b>Facility:</b> ", sections$facility, "<br>",
            "<b>Vector Index:</b> ", round(sections$vector_index, 2), "<br>",
            "<b>Nearest Trap Count:</b> ", sections$nearest_trap_count, "<br>",
            "<b>Last Inspection:</b> ", sections$last_inspection
          ),
          group = "Sections"
        )
    }
    
    # Add trap markers if data is available
    if (!is.null(data) && nrow(data$traps) > 0) {
      traps <- data$traps
      
      # Map trap type codes to names
      trap_type_names <- c(
        "4" = "Elevated CO2",
        "5" = "Gravid Trap",
        "6" = "CO2 Overnight"
      )
      
      map <- map %>%
        addCircleMarkers(
          lng = traps$lon,
          lat = traps$lat,
          radius = 5,
          color = "#0000FF",
          weight = 2,
          fillColor = "#0000FF",
          fillOpacity = 0.7,
          popup = paste0(
            "<b>Trap ID:</b> ", traps$ainspecnum, "<br>",
            "<b>Facility:</b> ", traps$facility, "<br>",
            "<b>Type:</b> ", trap_type_names[as.character(traps$survtype)], "<br>",
            "<b>Inspection Date:</b> ", traps$inspdate, "<br>",
            "<b>Species Count:</b> ", traps$species_count
          ),
          group = "Traps"
        )
    }
    
    # Add layer controls
    map <- map %>%
      addLayersControl(
        baseGroups = c("OSM", "CartoDB"),
        overlayGroups = c("Heat Map", "Sections", "Traps"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    map
  })
  
  # Data tables
  output$section_table <- renderDT({
    data <- map_state$data
    
    if (is.null(data) || nrow(data$sections) == 0) {
      return(datatable(data.frame(Message = "Click 'Refresh Heat Map' to load data")))
    }
    
    display_data <- data$sections %>%
      select(sectcode, zone, facility, vector_index, nearest_trap_count, last_inspection) %>%
      rename(
        Section = sectcode,
        Zone = zone,
        Facility = facility,
        `Vector Index` = vector_index,
        `Nearest Trap Total` = nearest_trap_count,
        `Last Inspection` = last_inspection
      ) %>%
      arrange(desc(`Vector Index`))
    
    datatable(
      display_data,
      options = list(
        pageLength = 25,
        searching = TRUE,
        ordering = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = "Vector Index", digits = 2)
  })
  
  output$trap_table <- renderDT({
    data <- map_state$data
    
    if (is.null(data) || nrow(data$traps) == 0) {
      return(datatable(data.frame(Message = "No trap data available")))
    }
    
    # Map trap type codes to names
    trap_type_names <- c(
      "4" = "Elevated CO2",
      "5" = "Gravid Trap",
      "6" = "CO2 Overnight"
    )
    
    display_data <- data$traps %>%
      mutate(trap_type_name = trap_type_names[as.character(survtype)]) %>%
      select(ainspecnum, facility, trap_type_name, inspdate, species_count) %>%
      rename(
        `Trap ID` = ainspecnum,
        Facility = facility,
        `Trap Type` = trap_type_name,
        `Inspection Date` = inspdate,
        `Species Count` = species_count
      ) %>%
      arrange(desc(`Species Count`))
    
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
  output$total_sections <- renderValueBox({
    data <- map_state$data
    count <- if (is.null(data)) 0 else nrow(data$sections)
    
    valueBox(
      count,
      "Total Sections",
      icon = icon("map"),
      color = "blue"
    )
  })
  
  output$total_traps <- renderValueBox({
    data <- map_state$data
    count <- if (is.null(data)) 0 else nrow(data$traps)
    
    valueBox(
      count,
      "Total Traps",
      icon = icon("bug"),
      color = "green"
    )
  })
  
  output$avg_vector_index <- renderValueBox({
    data <- map_state$data
    avg <- if (is.null(data) || nrow(data$sections) == 0) {
      0
    } else {
      round(mean(data$sections$vector_index, na.rm = TRUE), 2)
    }
    
    valueBox(
      avg,
      "Avg Vector Index",
      icon = icon("chart-line"),
      color = "orange"
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
      "Last Update",
      icon = icon("clock"),
      color = "yellow"
    )
  })
}

# =============================================================================
# RUN APPLICATION
# =============================================================================

shinyApp(ui = ui, server = server)
