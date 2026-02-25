#KNN traps surveillance app

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

source("../../shared/db_helpers.R")
source("ui_helper.R")
source("data_functions.R")
source("mle_trap_based.R")
source("display_functions.R")

ui <- trap_ui()

server <- function(input, output, session) {
  # Initialize selectors - load species choices on app start
  species_choices <- get_species_choices()
  
  observe({
    updateSelectizeInput(session, "species_sf", choices = species_choices, selected = "all")
  })
  
  # Species shortcut buttons - filter by genus
  observeEvent(input$select_aedes, {
    species_choices <- get_species_choices()
    # species_choices is a NAMED vector where names are labels "Genus species (code)" and values are codes
    aedes_codes <- species_choices[grepl("^Aedes ", names(species_choices))]
    updateSelectizeInput(session, "species_sf", selected = aedes_codes)
  })
  
  observeEvent(input$select_culex, {
    species_choices <- get_species_choices()
    culex_codes <- species_choices[grepl("^Culex ", names(species_choices))]
    updateSelectizeInput(session, "species_sf", selected = culex_codes)
  })
  
  observeEvent(input$select_anopheles, {
    species_choices <- get_species_choices()
    anopheles_codes <- species_choices[grepl("^Anopheles ", names(species_choices))]
    updateSelectizeInput(session, "species_sf", selected = anopheles_codes)
  })
  observeEvent(input$select_all_species, {
    species_choices <- get_species_choices()
    # Select all codes (the values, not the names)
    updateSelectizeInput(session, "species_sf", selected = as.character(species_choices))
  })

  # Reactive: compute population index OR MLE OR Vector Index based on metric selection
  vector_data_sf <- eventReactive(input$refresh_sf, {
    req(input$species_sf)
    req(input$trap_types_sf)
    req(input$metric_type)
    
    # Show progress indicator
    withProgress(message = 'Calculating metrics...', value = 0, {
      # Create progress updater function
      progress_updater <- list(
        set = function(value, detail = NULL) {
          setProgress(value, detail = detail)
        }
      )
      
      if (input$metric_type == "popindex") {
        setProgress(0.3, detail = "Computing population index...")
        result <- compute_section_vector_index(
          species_codes = input$species_sf,
          analysis_date = input$analysis_date_sf,
          k = input$k_neighbors_sf,
          trap_types = input$trap_types_sf
        )
      } else if (input$metric_type == "mle") {
        setProgress(0.2, detail = "Stage 1: Calculating trap MLEs...")
        result <- compute_section_mle_trap_based(
          species_codes = input$species_sf,
          analysis_date = input$analysis_date_sf,
          k = input$k_neighbors_sf,
          trap_types = input$trap_types_sf,
          virus_target = input$virus_target,
          pt_method = input$mle_method,
          scale = input$mle_scale,
          group_by = input$trap_grouping,
          progress = progress_updater  # Pass progress object
        )
        setProgress(0.9, detail = "Stage 2: Computing section averages...")
      } else {
        setProgress(0.3, detail = "Computing vector index...")
        result <- compute_section_vector_index_metric(
          species_codes = input$species_sf,
          analysis_date = input$analysis_date_sf,
          k = input$k_neighbors_sf,
          trap_types = input$trap_types_sf,
          virus_target = input$virus_target,
          pt_method = input$mle_method,
          scale = input$mle_scale,
          group_by = input$trap_grouping,
          progress = progress_updater  # Pass progress object
        )
      }
      
      setProgress(1, detail = "Complete!")
    })
    
    result
  })

  # Leaflet map output with basemap and interactive tooltips
  output$map_sf <- leaflet::renderLeaflet({
    data <- vector_data_sf()
    species_label <- if ("all" %in% tolower(input$species_sf)) {
      "All Species"
    } else {
      paste(length(input$species_sf), "selected species")
    }
    
    # Determine which metric to display
    metric_type <- input$metric_type
    
    # Get color theme
    color_theme <- input$color_theme
    
    # Pass traps data to display function
    # For MLE and Vector Index: show trap locations with their MLE values and pool statistics
    # For Population Index: show trap locations with mosquito counts
    traps_data <- if (metric_type %in% c("mle", "vector_index") && !is.null(data$trap_mles) && nrow(data$trap_mles) > 0) {
      # For MLE/Vector Index: use trap_mles directly - keep original column names
      # display_functions.R expects: sampnum_yr, facility, lon, lat, inspdate, mle, 
      # mle_lower, mle_upper, num_pools, num_positive, total_mosquitoes
      data$trap_mles
    } else if (!is.null(data$traps) && nrow(data$traps) > 0) {
      # For Population Index: show trap counts
      data$traps
    } else {
      NULL
    }
    
    # Create the leaflet map with basemap, popups, and trap markers
    render_vector_map_leaflet(data$sections_sf, traps_data, species_label, metric_type, color_theme)
  })

  output$table <- DT::renderDT({
    # Use data from sf version
    data <- if (exists("vector_data_sf") && !is.null(vector_data_sf())) {
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
        # Use data from sf version
        data <- if (exists("vector_data_sf") && !is.null(vector_data_sf())) {
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
