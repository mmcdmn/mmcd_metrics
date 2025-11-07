library(leaflet)
library(htmltools)

render_vector_map <- function(section_df, trap_df = NULL, species_label = "selected species") {
  if (is.null(section_df) || nrow(section_df) == 0) {
    return(leaflet() %>% addTiles() %>% setView(lng = -93.2, lat = 44.9, zoom = 9))
  }

  # Color palette for sections
  pal <- colorNumeric("YlOrRd", domain = section_df$vector_index, na.color = "#f0f0f0")
  
  # Start map with sections
  m <- leaflet() %>% 
    addTiles() %>%
    addCircleMarkers(data = section_df,
                     lng = ~lon, lat = ~lat,
                     radius = ~pmax(4, log1p(vector_index) * 4),
                     color = ~pal(vector_index), 
                     fillOpacity = 0.7,
                     stroke = TRUE,
                     weight = 1,
                     group = "Sections",
                     popup = ~paste0("<strong>Section: ", sectcode, "</strong><br/>",
                                   "Vector Index: ", round(vector_index, 2), "<br/>",
                                   "Nearest Trap Total: ", nearest_trap_count, "<br/>",
                                   "Last Inspection: ", last_inspection)) %>%
    addLegend(position = "bottomright", pal = pal, values = section_df$vector_index, title = "Vector Index")
  
  # Add traps if provided
  if (!is.null(trap_df) && nrow(trap_df) > 0) {
    # Add species label column to trap_df for popup
    trap_df$species_label <- species_label
    
    # Map survtype codes to names
    trap_type_names <- c("4" = "Elevated CO2", "5" = "Gravid Trap", "6" = "CO2 Overnight")
    trap_df$survtype_name <- trap_type_names[as.character(trap_df$survtype)]
    
    m <- m %>%
      addCircleMarkers(data = trap_df,
                       lng = ~lon, lat = ~lat,
                       radius = 3,
                       color = "#2166ac",
                       fillColor = "#2166ac",
                       fillOpacity = 0.6,
                       stroke = TRUE,
                       weight = 1,
                       group = "Traps",
                       popup = ~paste0("<strong>Trap: ", ainspecnum, "</strong><br/>",
                                     "Facility: ", facility, "<br/>",
                                     "Type: ", survtype_name, "<br/>",
                                     "Inspection Date: ", inspdate, "<br/>",
                                     species_label, " Count: ", species_count)) %>%
      addLayersControl(
        overlayGroups = c("Sections", "Traps"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  return(m)
}
