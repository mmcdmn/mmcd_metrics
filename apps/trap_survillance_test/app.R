#KNN traps surveillance app

library(shiny)
library(shinydashboard)
library(ggplot2)

source("../../shared/db_helpers.R")
source("ui_helper.R")
source("data_functions.R")
source("display_functions.R")

ui <- trap_ui()

server <- function(input, output, session) {
  # Initialize selectors - load species and facility choices (UI options only)
  observe({
    species_choices <- get_species_choices()
    updateSelectizeInput(session, "species", choices = species_choices, selected = "all")
    updateSelectizeInput(session, "species_sf", choices = species_choices, selected = "all")
    
    facility_choices <- get_facility_choices(include_all = TRUE)
    updateSelectizeInput(session, "facility", choices = facility_choices, selected = "all")
    updateSelectizeInput(session, "facility_sf", choices = facility_choices, selected = "all")
  })

  # Reactive: compute population index ONLY when refresh button is clicked (leaflet version)
  vector_data <- eventReactive(input$refresh, {
    req(input$species)
    req(input$trap_types)
    
    result <- compute_section_vector_index(
      species_codes = input$species,
      analysis_date = input$analysis_date,
      k = input$k_neighbors,
      facility_filter = input$facility,
      trap_types = input$trap_types
    )
    result
  })
  
  # Reactive: compute population index ONLY when refresh button is clicked (sf version)
  vector_data_sf <- eventReactive(input$refresh_sf, {
    req(input$species_sf)
    req(input$trap_types_sf)
    
    result <- compute_section_vector_index(
      species_codes = input$species_sf,
      analysis_date = input$analysis_date_sf,
      k = input$k_neighbors_sf,
      facility_filter = input$facility_sf,
      trap_types = input$trap_types_sf
    )
    result
  })

  # Leaflet map output
  output$map <- renderLeaflet({
    data <- vector_data()
    species_label <- if ("all" %in% tolower(input$species)) {
      "All Species"
    } else {
      paste(length(input$species), "selected species")
    }
    render_vector_map(data$sections, data$traps, species_label)
  })
  
  # SF static map output (no plotly - it strips the OSM basemap tiles)
  output$map_sf <- renderPlot({
    data <- vector_data_sf()
    species_label <- if ("all" %in% tolower(input$species_sf)) {
      "All Species"
    } else {
      paste(length(input$species_sf), "selected species")
    }
    
    # Create and return the ggplot (static map with ggspatial tiles)
    render_vector_map_sf(data$sections_sf, data$traps, species_label)
  }, height = 800, width = 1000)

  output$table <- DT::renderDT({
    # Use data from either source (they should be the same)
    data <- if (exists("vector_data") && !is.null(vector_data())) {
      vector_data()
    } else if (exists("vector_data_sf") && !is.null(vector_data_sf())) {
      vector_data_sf()
    } else {
      return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    }
    
    # Show sections with trap type info would require joining, so just show sections
    # Add trap type names for display
    sections_display <- data$sections
    DT::datatable(sections_display, options = list(pageLength = 15), 
                  caption = "Section Population Index Results")
  })
  
  # Download handler for vector data CSV export
  output$download_vector_data <- downloadHandler(
    filename = function() {
      paste("trap_vector_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      tryCatch({
        # Use data from either source
        data <- if (exists("vector_data") && !is.null(vector_data())) {
          vector_data()
        } else if (exists("vector_data_sf") && !is.null(vector_data_sf())) {
          vector_data_sf()
        } else {
          write.csv(data.frame(Message = "No vector data available"), file, row.names = FALSE)
          return()
        }
        
        # Export the sections data
        if (!is.null(data$sections) && nrow(data$sections) > 0) {
          result <- export_csv_safe(data$sections, file, clean_data = TRUE)
          if (!result$success) {
            write.csv(data$sections, file, row.names = FALSE, na = "")
          }
        } else {
          write.csv(data.frame(Message = "No sections data available"), file, row.names = FALSE)
        }
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )
}

shinyApp(ui = ui, server = server)
