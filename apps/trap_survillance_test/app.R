library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

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

  # Reactive: compute vector index ONLY when refresh button is clicked (leaflet version)
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
  
  # Reactive: compute vector index ONLY when refresh button is clicked (sf version)
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
  
  # SF static map output with zoom functionality
  output$map_sf <- renderPlotly({
    data <- vector_data_sf()
    species_label <- if ("all" %in% tolower(input$species_sf)) {
      "All Species"
    } else {
      paste(length(input$species_sf), "selected species")
    }
    
    # Create the ggplot
    p <- render_vector_map_sf(data$sections_sf, data$traps, species_label)
    
    # Convert to plotly for zoom/pan functionality
    ggplotly(p, tooltip = c("fill", "colour")) %>%
      config(displayModeBar = TRUE, 
             modeBarButtonsToAdd = list("pan2d", "select2d", "lasso2d"),
             modeBarButtonsToRemove = list("sendDataToCloud", "editInChartStudio")) %>%
      layout(legend = list(orientation = "v", x = 1.02, y = 1))
  })

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
                  caption = "Section Vector Index Results")
  })
}

shinyApp(ui = ui, server = server)
