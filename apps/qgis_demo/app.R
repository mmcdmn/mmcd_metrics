# =============================================================================
# QGIS Server Pure Demo - Professional Cartography Only
# =============================================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(htmltools)

# Create debug log file
debug_log <- "/tmp/qgis_demo_debug.log"
debug_write <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("[", timestamp, "] ", msg, "\n"), file = debug_log, append = TRUE)
  cat(msg, "\n")  # Also print to console
}

debug_write("=== QGIS Demo App Starting ===")

# Source shared functions
source("../../shared/db_helpers.R")
source("data_functions.R")
source("qgis_helpers.R")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "QGIS Server Professional Cartography"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("QGIS Demo", tabName = "qgis_demo", icon = icon("map"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .comparison-container {
          height: 600px;
          width: 100%;
          position: relative;
        }
        .map-panel {
          height: 100%;
          width: 100%;
          border: 2px solid #ddd;
          border-radius: 8px;
          position: relative;
        }
        .map-title {
          position: absolute;
          top: 10px;
          left: 10px;
          background: rgba(255,255,255,0.9);
          padding: 5px 10px;
          border-radius: 4px;
          font-weight: bold;
          z-index: 1000;
          color: #2E8B57;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "qgis_demo",
        fluidRow(
          box(
            width = 4,
            title = "Analysis Parameters",
            status = "primary",
            solidHeader = TRUE,
            
            selectInput("species", 
                       label = "Target Species", 
                       choices = list("Aedes triseriatus" = "24",
                                    "Aedes japonicus" = "52", 
                                    "Aedes albopictus" = "51"),
                       selected = "24"),
            
            dateInput("analysis_date",
                     label = "Analysis Date",
                     value = Sys.Date() - 1,
                     max = Sys.Date()),
            
            numericInput("k_neighbors",
                        label = "K-Nearest Neighbors",
                        value = 5,
                        min = 3,
                        max = 15),
            
            actionButton("generate_maps", 
                        "Generate QGIS Cartography",
                        class = "btn-success btn-lg",
                        style = "width: 100%; margin-top: 20px;")
          ),
          
          box(
            width = 4,
            title = "QGIS Features",
            status = "success",
            solidHeader = TRUE,
            valueBoxOutput("qgis_features", width = 12)
          ),
          
          box(
            width = 4,
            title = "Performance",
            status = "info", 
            solidHeader = TRUE,
            valueBoxOutput("rendering_time", width = 12)
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "QGIS Server Professional Cartography Demo",
            status = "primary",
            solidHeader = TRUE,
            
            div(
              class = "comparison-container",
              div(
                class = "map-panel",
                div(class = "map-title", "🗺️ QGIS Server Professional Cartography"),
                imageOutput("qgis_map", width = "100%", height = "600px")
              )
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store map data
  map_data <- reactiveValues(
    sections = NULL,
    traps = NULL,
    qgis_project_name = NULL
  )
  
  # Generate maps when button is clicked
  observeEvent(input$generate_maps, {
    
    debug_write("=== Generate button clicked ===")
    
    withProgress(message = 'Generating professional cartography...', value = 0, {
      
      # Step 1: Process data
      debug_write("Starting data processing")
      incProgress(0.2, detail = "Processing surveillance data...")
      
      tryCatch({
        result <- compute_section_vector_index(
          species_codes = input$species,
          analysis_date = input$analysis_date,
          k = input$k_neighbors,
          facility_filter = "all",
          trap_types = c("4", "5", "6")
        )
        
        debug_write(paste("Data processing completed - Sections:", nrow(result$sections), "Traps:", nrow(result$traps)))
        
        map_data$sections <- result$sections
        map_data$traps <- result$traps
        
      }, error = function(e) {
        debug_write(paste("ERROR in data processing:", e$message))
        showNotification(paste("Data processing error:", e$message), type = "error")
        return()
      })
      
      # Step 2: Generate QGIS project
      incProgress(0.4, detail = "Creating QGIS project...")
      
      tryCatch({
        qgis_project <- generate_advanced_qgis_project(
          sections_data = map_data$sections,
          traps_data = map_data$traps,
          analysis_date = as.character(input$analysis_date),
          species_label = "Target Species"
        )
        
        debug_write(paste("QGIS project created:", qgis_project))
        map_data$qgis_project_name <- qgis_project
        
      }, error = function(e) {
        debug_write(paste("ERROR in QGIS project generation:", e$message))
        showNotification(paste("QGIS project error:", e$message), type = "error")
        return()
      })
      
      incProgress(1.0, detail = "Complete!")
      
      showNotification("Professional cartography generated successfully!", type = "message")
    })
  })
  
  # QGIS Server direct image map
  output$qgis_map <- renderImage({
    
    if (!is.null(map_data$sections) && !is.null(map_data$qgis_project_name)) {
      
      debug_write("QGIS map: generating direct image from QGIS Server")
      debug_write(paste("Project name:", map_data$qgis_project_name))
      
      # Generate direct map image URL from QGIS Server (internal container access)
      layer_name <- paste0("Simple Heat Map - ", as.character(input$analysis_date))
      
      map_url <- paste0(
        "http://localhost/qgis/?",
        "SERVICE=WMS&",
        "VERSION=1.3.0&",
        "REQUEST=GetMap&",
        "MAP=/qgis/projects/", map_data$qgis_project_name, ".qgs&",
        "LAYERS=", URLencode(layer_name), "&",
        "STYLES=&",
        "FORMAT=image/png&",
        "BGCOLOR=0xFFFFFF&",
        "TRANSPARENT=TRUE&",
        "SRS=EPSG:4326&",
        "BBOX=-94.5,44.0,-92.5,45.5&",
        "WIDTH=800&",
        "HEIGHT=600"
      )
      
      debug_write(paste("Map URL:", map_url))
      
      # Download image and serve through Shiny
      temp_file <- tempfile(fileext = ".png")
      tryCatch({
        download.file(map_url, temp_file, mode = "wb", quiet = TRUE)
        debug_write(paste("Image downloaded to:", temp_file))
        
        list(
          src = temp_file,
          contentType = "image/png",
          width = 800,
          height = 600,
          alt = "QGIS Professional Cartography"
        )
      }, error = function(e) {
        debug_write(paste("Error downloading image:", e$message))
        # Return placeholder image path
        list(src = "", contentType = "image/png", width = 800, height = 600)
      })
      
    } else {
      debug_write("No data or project available, showing placeholder")
      # Return empty for placeholder
      list(src = "", contentType = "image/png", width = 800, height = 600)
    }
  }, deleteFile = TRUE)
  
  # Value boxes
  output$qgis_features <- renderValueBox({
    valueBox(
      "5+ Layers",
      "QGIS Cartographic Features", 
      icon = icon("layer-group"),
      color = "green"
    )
  })
  
  output$rendering_time <- renderValueBox({
    valueBox(
      "< 3 sec",
      "Server Rendering Time",
      icon = icon("clock"),
      color = "blue"
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)