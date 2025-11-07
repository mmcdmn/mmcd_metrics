library(shiny)
library(shinydashboard)

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
    
    facility_choices <- get_facility_choices(include_all = TRUE)
    updateSelectizeInput(session, "facility", choices = facility_choices, selected = "all")
  })

  # Reactive: compute vector index ONLY when refresh button is clicked
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

  output$map <- renderLeaflet({
    data <- vector_data()
    species_label <- if ("all" %in% tolower(input$species)) {
      "All Species"
    } else {
      paste(length(input$species), "selected species")
    }
    render_vector_map(data$sections, data$traps, species_label)
  })

  output$table <- DT::renderDT({
    data <- vector_data()
    # Show sections with trap type info would require joining, so just show sections
    # Add trap type names for display
    sections_display <- data$sections
    DT::datatable(sections_display, options = list(pageLength = 15), 
                  caption = "Section Vector Index Results")
  })
}

shinyApp(ui = ui, server = server)
