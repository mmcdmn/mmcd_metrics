# MMCD Mosquito Surveillance Map Application
# Adapted from mmcdmapprototype to use shared db_helpers

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)  # Updated to use RPostgres instead of RPostgreSQL
  library(ggplot2)
  library(sf)
  library(tidyverse)
  library(plotly)
  library(bslib)
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")

# Load shapefiles
UTMshapeCOUNTYdata <- sf::st_read("shp/Counties_UTM.shp")
UTMshapeCITYdata <- sf::st_read("shp/CTUs.shp")
# UTMshapeLAKEdata <- sf::st_read("shp/Lakes.shp")
# UTMshapeZONEdata <- sf::st_read("shp/P1zonebdry_UTM.shp")

# Load mosquito surveillance data using shared connection
load_mosquito_data <- function() {
  con <- get_db_connection()
  if (is.null(con)) {
    warning("Could not connect to database")
    return(list(dfmap = data.frame(), dfmapMISS = data.frame()))
  }
  
  tryCatch({
    dfmap <- dbReadTable(con, "dbadult_mapdata_forr_calclat")
    dfmapMISS <- dbReadTable(con, "dbadult_mapdata_forr_missing")
    dbDisconnect(con)
    
    return(list(dfmap = dfmap, dfmapMISS = dfmapMISS))
  }, error = function(e) {
    warning(paste("Error loading mosquito data:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(list(dfmap = data.frame(), dfmapMISS = data.frame()))
  })
}

# Load data
mosquito_data <- load_mosquito_data()
dfmap <- mosquito_data$dfmap
dfmapMISS <- mosquito_data$dfmapMISS

# Transform coordinates to spatial format
if (nrow(dfmap) > 0) {
  dfmap4326 <- dfmap %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)
  dfmap2163UTM <- st_transform(dfmap4326, crs = 2163)
} else {
  dfmap2163UTM <- st_sf(geometry = st_sfc())
}

if (nrow(dfmapMISS) > 0) {
  dfmapMISS4326 <- dfmapMISS %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)
  dfmapMISS2163UTM <- st_transform(dfmapMISS4326, crs = 2163)
} else {
  dfmapMISS2163UTM <- st_sf(geometry = st_sfc())
}

# UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  
  # Add title
  titlePanel("MMCD Mosquito Surveillance Map"),
  
  plotlyOutput("MAP", height = "1000px"), 
  
  absolutePanel(
    id = "controls", 
    class = "panel panel-default", 
    fixed = TRUE,
    draggable = FALSE, 
    top = 100, 
    left = 20, 
    right = "auto", 
    bottom = "auto",
    width = 330, 
    height = "auto",
    
    h4("Map Controls"),
    
    selectInput("species", "Species Selection", 
                choices = if(nrow(dfmap) > 0) unique(dfmap$spp_name) else c("No data available"),
                selected = if(nrow(dfmap) > 0) "Total_Ae_+_Cq" else NULL,
                multiple = FALSE),
    
    dateRangeInput("daterange", "Inspection Date Range",
                   start = if(nrow(dfmap) > 0) min(dfmap$inspdate, na.rm = TRUE) else Sys.Date() - 30,
                   end = if(nrow(dfmap) > 0) max(dfmap$inspdate, na.rm = TRUE) else Sys.Date(),
                   min = if(nrow(dfmap) > 0) min(dfmap$inspdate, na.rm = TRUE) else Sys.Date() - 365,
                   max = if(nrow(dfmap) > 0) max(dfmap$inspdate, na.rm = TRUE) else Sys.Date()),
    
    selectInput("survtype", "Surveillance Type", 
                choices = c("Sweep", "CO2(reg)", "Gravid", "CO2(elev)", "All"),
                selected = "All",
                multiple = FALSE),
    
    div(style = "margin: 10px 0;",
        checkboxInput("show_avg", "Show Average (vs Sum)", value = FALSE)
    ),
    
    checkboxInput("labels", "Show Labels", value = FALSE),
    
    hr(),
    
    # Data status
    div(
      style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
      h5("Data Status"),
      if(nrow(dfmap) > 0) {
        paste("Loaded", nrow(dfmap), "surveillance records")
      } else {
        "No surveillance data available"
      }
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  whichplot <- reactiveVal(FALSE)  # FALSE = Sum, TRUE = Average
  
  # Update whichplot when checkbox changes
  observeEvent(input$show_avg, {
    whichplot(input$show_avg)
  })
  
  # Title strings
  TitleString <- reactive({
    paste(input$species, "in", input$survtype, "from", input$daterange[1], "to", input$daterange[2])
  })
  
  TitleStringAVG <- reactive({
    paste("Average", input$species, "in", input$survtype, "from", input$daterange[1], "to", input$daterange[2])
  })
  
  # Data processing reactive functions
  mosquito <- reactive({
    if (nrow(dfmap2163UTM) == 0) return(dfmap2163UTM)
    
    if (isTRUE(input$survtype == "All")) {
      dfmap2163UTM
    } else {
      dfmap2163UTM %>%
        filter(survtypename %in% input$survtype)
    }
  })
  
  mosqsppFilter <- reactive({
    data <- mosquito()
    if (nrow(data) == 0) return(data)
    
    data %>%
      filter(spp_name %in% input$species)
  })
  
  mosqdateFilter <- reactive({
    data <- mosqsppFilter()
    if (nrow(data) == 0) return(data)
    
    data %>% 
      filter(between(inspdate, input$daterange[1], input$daterange[2]))
  })
  
  MAPdata0 <- reactive({
    data <- mosqdateFilter()
    if (nrow(data) == 0) return(data)
    
    data %>%
      group_by(loc_code) %>%
      summarise(
        Sum = sum(mosqcount, na.rm = TRUE), 
        Avg = round(mean(mosqcount, na.rm = TRUE), 2), 
        geometry = first(geometry), 
        spp_name = first(spp_name), 
        loc_code = first(loc_code),
        .groups = "drop"
      )
  })
  
  # Category assignment based on survey type and species
  get_category_breaks <- function(survey_type, species, is_avg = FALSE) {
    if (survey_type == "All") {
      list(
        breaks = c(-Inf, 0, 2, 4, 49, 129, 299, 999, Inf),
        labels = c("0", "1-2", "3-4", "5-49", "50-129 ", "130-299 ", "300-999 ", "1000+ ")
      )
    } else if (survey_type == "Sweep") {
      list(
        breaks = c(-Inf, 0, 2, 10, 20, 50, Inf),
        labels = c("0", "1-2", "3-10", "11-20", "21-50", "51+")
      )
    } else if (survey_type == "Gravid") {
      list(
        breaks = c(-Inf, 0, 4, 9, 29, 49, Inf),
        labels = c("0", "1-4", "5-9", "10-29", "30-49", "50+")
      )
    } else if ((survey_type %in% c("CO2(reg)", "CO2(elev)")) && 
               (species %in% c("Cx_pipiens_33", "Cx_restuans_34", "Cx_restuans/pipiens_372", "Cx_salinarius_35", "Cx_tarsalis_36"))) {
      list(
        breaks = c(-Inf, 0, 4, 9, 29, 49, Inf),
        labels = c("0", "1-4", "5-9", "10-29", "30-49", "50+")
      )
    } else if ((survey_type %in% c("CO2(reg)", "CO2(elev)")) && (species == "Cs_melanura_39")) {
      list(
        breaks = c(-Inf, 0, 4, 9, 49, Inf),
        labels = c("0", "1-4", "5-9", "10-49", "50+ ")
      )
    } else {
      list(
        breaks = c(-Inf, 0, 49, 129, 299, 999, Inf),
        labels = c("0", "1-49", "50-129", "130-299", "300-999", "1000+")
      )
    }
  }
  
  MAPdata <- reactive({
    data <- MAPdata0()
    if (nrow(data) == 0) return(data)
    
    breaks_info <- get_category_breaks(input$survtype, input$species, FALSE)
    
    data %>%
      mutate(mosq_category = cut(Sum, 
                                breaks = breaks_info$breaks,
                                labels = breaks_info$labels,
                                include.lowest = TRUE))
  })
  
  MAPdataAVG <- reactive({
    data <- MAPdata0()
    if (nrow(data) == 0) return(data)
    
    breaks_info <- get_category_breaks(input$survtype, input$species, TRUE)
    
    data %>%
      mutate(mosq_category = cut(Avg, 
                                breaks = breaks_info$breaks,
                                labels = breaks_info$labels,
                                include.lowest = TRUE))
  })
  
  # Missing data processing
  mosquitoMISS <- reactive({
    if (nrow(dfmapMISS2163UTM) == 0) return(dfmapMISS2163UTM)
    
    if (isTRUE(input$survtype == "All")) {
      dfmapMISS2163UTM
    } else {
      dfmapMISS2163UTM %>%
        filter(survtypename %in% input$survtype)
    }
  })
  
  mosqsppFilterMISS <- reactive({
    data <- mosquitoMISS()
    if (nrow(data) == 0) return(data)
    
    data %>%
      filter(spp_name %in% input$species)
  })
  
  mosqdateFilterMISS <- reactive({
    data <- mosqsppFilterMISS()
    if (nrow(data) == 0) return(data)
    
    data %>% 
      filter(between(inspdate, input$daterange[1], input$daterange[2]))
  })
  
  MAPdataMISS0 <- reactive({
    data <- mosqdateFilterMISS()
    if (nrow(data) == 0) return(data)
    
    data %>%
      group_by(loc_code) %>%
      summarise(
        Missing = "Missing", 
        geometry = first(geometry), 
        spp_name = first(spp_name), 
        loc_code = first(loc_code),
        .groups = "drop"
      )
  })  
  
  MAPdataMISS <- reactive({
    data <- MAPdataMISS0()
    if (nrow(data) == 0) return(data)
    
    data %>%
      mutate(mosq_category = factor("Missing"))
  })
  
  # Label data
  LABELdataSum <- reactive({
    data <- MAPdata()
    if (nrow(data) == 0) return(data)
    
    data %>%
      filter(Sum > 0)
  })
  
  LABELdataAvg <- reactive({
    data <- MAPdataAVG()
    if (nrow(data) == 0) return(data)
    
    data %>%
      filter(Avg > 0) %>%
      mutate(Avg = round(Avg, 0))
  })
  
  # Base map
  BASEmap <- ggplot() + 
    geom_sf(data = UTMshapeCOUNTYdata, linewidth = .8) +
    theme_void()
  
  # Color and shape scales
  get_scales <- function() {
    list(
      colors = c("0" = "brown", 
                "1-2" = "#ffffb2", 
                "3-10" = "#fecc5c", 
                "11-20" = "#fd8d3c", 
                "21-50" = "#f03b20", 
                "51+" = "#b10026",
                "1-4" = "#ffffb2", 
                "5-9" = "#fecc5c", 
                "10-29" = "#fd8d3c", 
                "30-49" = "#f03b20", 
                "50+" = "#b10026",
                "1-49" = "#ffffb2", 
                "50-129" = "#fecc5c", 
                "130-299" = "#fd8d3c", 
                "300-999" = "#f03b20", 
                "1000+" = "#b10026",
                "10-49" = "#fd8d3c",
                "50+ " = "#f03b20",
                "3-4" = "#fed976",
                "5-49" = "#feb24c",
                "50-129 " = "#fd8d3c",
                "130-299 " = "#fc4e2a",
                "300-999 " = "#e31a1c",
                "1000+ " = "#b10026",
                "Missing" = "black"),
      
      shapes = c("0" = 1, 
                "1-2" = 17, 
                "3-10" = 16, 
                "11-20" = 18, 
                "21-50" = 15, 
                "51+" = 15,
                "1-4" = 17, 
                "5-9" = 16, 
                "10-29" = 18, 
                "30-49" = 15, 
                "50+" = 15,
                "1-49" = 17, 
                "50-129" = 16, 
                "130-299" = 18, 
                "300-999" = 15, 
                "1000+" = 15,
                "10-49" = 18,
                "50+ " = 15,
                "3-4" = 17,
                "5-49" = 16,
                "50-129 " = 18,
                "130-299 " = 15,
                "300-999 " = 15,
                "1000+ " = 15,
                "Missing" = 4),
      
      sizes = c("0" = 2, 
               "1-2" = 3, 
               "3-10" = 4, 
               "11-20" = 5, 
               "21-50" = 6, 
               "51+" = 7,
               "1-4" = 3, 
               "5-9" = 4, 
               "10-29" = 5, 
               "30-49" = 6, 
               "50+" = 7,
               "1-49" = 3, 
               "50-129" = 4, 
               "130-299" = 5, 
               "300-999" = 6, 
               "1000+" = 7,
               "10-49" = 5,
               "50+ " = 6,
               "3-4" = 4,
               "5-49" = 5,
               "50-129 " = 6,
               "130-299 " = 6,
               "300-999 " = 7,
               "1000+ " = 8,
               "Missing" = 2)
    )
  }
  
  # Main map reactive
  whichmap <- reactive({
    scales <- get_scales()
    
    if (isTRUE(whichplot())) {
      # Average map
      map_data <- MAPdataAVG()
      miss_data <- MAPdataMISS()
      
      if (nrow(map_data) == 0 && nrow(miss_data) == 0) {
        return(BASEmap + ggtitle("No data available for selected filters"))
      }
      
      p <- BASEmap
      
      if (nrow(miss_data) > 0) {
        p <- p + geom_sf(data = miss_data,
                        aes(color = mosq_category,
                            shape = mosq_category,
                            size = mosq_category,
                            text = sprintf("%s<br>Missing", loc_code)))
      }
      
      if (nrow(map_data) > 0) {
        p <- p + geom_sf(mapping = aes(color = mosq_category, 
                                      shape = mosq_category, 
                                      size = mosq_category, 
                                      text = sprintf("%s<br>%.1f", loc_code, Avg)), 
                        data = map_data)
      }
      
      p <- p + 
        ggtitle(TitleStringAVG()) +
        scale_color_manual(name = "Mosq Avg", values = scales$colors) +
        scale_shape_manual(name = "Mosq Avg", values = scales$shapes) +
        scale_size_manual(name = "Mosq Avg", values = scales$sizes) +
        guides(
          color = guide_legend(override.aes = list(size = 9)),
          shape = guide_legend(override.aes = list(size = 9)),
          size = guide_legend(override.aes = list(size = 9))
        ) +
        theme(plot.title = element_text(hjust = 0.5))
        
    } else {
      # Sum map  
      map_data <- MAPdata()
      miss_data <- MAPdataMISS()
      
      if (nrow(map_data) == 0 && nrow(miss_data) == 0) {
        return(BASEmap + ggtitle("No data available for selected filters"))
      }
      
      p <- BASEmap
      
      if (nrow(miss_data) > 0) {
        p <- p + geom_sf(data = miss_data,
                        aes(color = mosq_category,
                            shape = mosq_category,
                            size = mosq_category,
                            text = sprintf("%s<br>Missing", loc_code)))
      }
      
      if (nrow(map_data) > 0) {
        p <- p + geom_sf(mapping = aes(color = mosq_category, 
                                      shape = mosq_category, 
                                      size = mosq_category, 
                                      text = sprintf("%s<br>%d", loc_code, Sum)), 
                        data = map_data)
      }
      
      p <- p + 
        ggtitle(TitleString()) +
        scale_color_manual(name = "Mosq Sum", values = scales$colors) +
        scale_shape_manual(name = "Mosq Sum", values = scales$shapes) +
        scale_size_manual(name = "Mosq Sum", values = scales$sizes) +
        guides(
          color = guide_legend(override.aes = list(size = 7)),
          shape = guide_legend(override.aes = list(size = 7)),
          size = guide_legend(override.aes = list(size = 7))
        ) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    return(p)
  })
  
  # Final map with optional labels
  whichmapfinal <- reactive({
    base_map <- whichmap()
    
    if (input$labels == TRUE) {
      if (isTRUE(whichplot())) {
        # Add average labels
        label_data <- LABELdataAvg()
        if (nrow(label_data) > 0) {
          base_map <- base_map + 
            geom_sf_text(aes(label = Avg), data = label_data, size = 3, fontface = "bold")
        }
      } else {
        # Add sum labels
        label_data <- LABELdataSum()
        if (nrow(label_data) > 0) {
          base_map <- base_map + 
            geom_sf_text(aes(label = Sum), data = label_data, size = 3, fontface = "bold")
        }
      }
    }
    
    return(base_map)
  })
  
  # Render the map
  output$MAP <- renderPlotly({
    p <- whichmapfinal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        legend = list(x = 1.1, y = 0.5),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })    
}

# Run the application 
shinyApp(ui = ui, server = server)