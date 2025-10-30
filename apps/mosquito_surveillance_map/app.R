#
# 
# 
#
# 
#
#   
# ggplot(UTMshapeCOUNTYdata) + geom_sf() + geom_sf( data = UTMshapeZONEdata, fill = "grey") + geom_sf( data = UTMshapeTOWNdata,  alpha = 0) + geom_sf( data = UTMshapeFACILITYdata, alpha = 0, linewidth = 1)

library(shiny)
library(DBI)
library(RPostgreSQL)
library(ggplot2)
library(sf)
library(tidyverse)
library(plotly)
library(bslib)
   
#  ggplot() + geom_sf(data= UTMshapeZONEdata, fill = "grey") + geom_sf( data = UTMshapeCOUNTYdata, alpha = 0) + geom_sf( data = UTMshapeTOWNdata,  alpha = 0) + geom_sf( data = UTMshapeFACILITYdata,color = 'blue', alpha = 0, linewidth = .5) + geom_sf(mapping = aes(size = mosqcount/10), data = dfmapUTMhead)
UTMshapeCOUNTYdata <- sf::st_read("shp/Counties_UTM.shp")
UTMshapeCITYdata <- sf::st_read("shp/CTUs.shp")
#UTMshapeLAKEdata <- sf::st_read("shp/Lakes.shp")
#UTMshapeZONEdata <- sf::st_read("shp/P1zonebdry_UTM.shp")

# Source shared helper functions
suppressWarnings({
  source("../../shared/db_helpers.R")
})

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

con <- get_db_connection()
if (is.null(con)) {
  stop("Database connection failed")
}

dfmap <- dbReadTable(con, "dbadult_mapdata_forr_calclat")

# Debug: Check data range
cat("Data loaded - Date range:", min(dfmap$inspdate), "to", max(dfmap$inspdate), "\n")
cat("Data loaded - Mosquito count range:", min(dfmap$mosqcount, na.rm = TRUE), "to", max(dfmap$mosqcount, na.rm = TRUE), "\n")
cat("Data loaded - Total records:", nrow(dfmap), "\n")

dfmapMISS <- dbReadTable(con, "dbadult_mapdata_forr_missing")

dbDisconnect(con)

dfmap4326 = dfmap %>%
  st_as_sf(coords = c("long", "lat"), crs=4326)

dfmap2163UTM <- st_transform(dfmap4326, crs=2163)

dfmapMISS4326 = dfmapMISS %>%
  st_as_sf(coords = c("long", "lat"), crs=4326)

dfmapMISS2163UTM <- st_transform(dfmapMISS4326, crs=2163)


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  
  # Add header with data last updated info
  fluidRow(
    column(12,
      div(style = "background-color: #f8f9fa; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
        h4("Mosquito Surveillance Map", style = "margin: 0; display: inline-block;"),
        div(style = "float: right; margin-top: 5px;",
          textOutput("lastUpdated", inline = TRUE)
        )
      )
    )
  ),
  
  plotlyOutput("MAP", height = "1000px"), 
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = FALSE, top = 120, left = 20, right = "auto", bottom = "auto",
                width = 330, height = "auto",
                
                selectInput("species", "Species Selection", choices = unique(dfmap$spp_name),
                                      selected = "Total_Ae_+_Cq",
                                      multiple = FALSE),
                dateRangeInput("daterange", "Insp Date Range",
                                   min(dfmap$inspdate), max(dfmap$inspdate), start = "2025-09-01", end = "2025-10-01"),
                selectInput("survtype", "Surveillance Selection", choices = c("Sweep", "CO2(reg)", "Gravid", "CO2(elev)", "All"),
                                      selected = "All",
                                      multiple = FALSE),
                input_switch("button", "Avg/Sum"),
                checkboxInput("labels", "Labels")
                # downloadButton("report", "Generate report")
              
            
               
              )
  )


server <- function(input, output) {

  # Display when data was last updated
  output$lastUpdated <- renderText({
    last_update_date <- max(dfmap$inspdate, na.rm = TRUE)
    paste("Data last updated:", format(last_update_date, "%B %d, %Y"))
  })

  # output$report <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename = "report.html",
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(n = input$species)
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )  
  
whichplot <- reactiveVal(TRUE)

#Title String
TitleString <- reactive({
  paste(input$species, "in", input$survtype, "from", input$daterange[1], "to", input$daterange[2])
})

TitleStringAVG <- reactive({
  paste("Average",input$species, "in", input$survtype, "from", input$daterange[1], "to", input$daterange[2])
})
# Main table Processing
mosquito <- reactive({
    if (isTRUE(input$survtype == "All")) {
      result <- dfmap2163UTM
      cat("mosquito() - All survtype: ", nrow(result), "records\n")
      result
    } else {
      result <- dfmap2163UTM %>%
        filter(survtypename %in% input$survtype)
      cat("mosquito() - Filtered survtype:", input$survtype, ":", nrow(result), "records\n")
      result
    }
  })

mosqsppFilter <- reactive({
  result <- mosquito() %>%
    filter(spp_name %in% input$species)
  cat("mosqsppFilter() - Species:", input$species, ":", nrow(result), "records\n")
  result
  })

mosqdateFilter <- reactive({
  result <- mosqsppFilter() %>% 
    filter(between(inspdate, input$daterange[1], input$daterange[2]))
  cat("mosqdateFilter() - Date range:", input$daterange[1], "to", input$daterange[2], ":", nrow(result), "records\n")
  if(nrow(result) > 0) {
    cat("mosqdateFilter() - Mosquito count range:", min(result$mosqcount, na.rm = TRUE), "to", max(result$mosqcount, na.rm = TRUE), "\n")
  }
  result
  })

MAPdata0 <- reactive({
  result <- mosqdateFilter() %>%
    group_by(loc_code) %>%
    summarise(
      Sum = sum(mosqcount, na.rm = TRUE), 
      Avg = round(mean(mosqcount, na.rm = TRUE), 2), 
      geometry = first(geometry), 
      spp_name = first(spp_name),
      .groups = 'drop'
    )
  
  # Debug output - print summary to console
  cat("MAPdata0 debug:\n")
  cat("Number of records:", nrow(result), "\n")
  cat("Sum range:", min(result$Sum, na.rm = TRUE), "to", max(result$Sum, na.rm = TRUE), "\n")
  cat("Sum distribution:\n")
  print(table(cut(result$Sum, breaks = c(-Inf, 0, 2.5, 4.5, 49.5, 129.5, 299.5, 999.5, Inf))))
  
  result
})

# Add debug reactive to show what categories are being created
MAPdataDebug <- reactive({
  data <- MAPdata()
  cat("MAPdata categories created:\n")
  print(table(data$mosq_category))
  data
})

LABELdataSum <- reactive({
  MAPdata() %>%
    filter(Sum > 0)
  
})

LABELdataAvg0 <- reactive({
  MAPdataAVG() %>%
    filter(Avg > 0)
  
})

LABELdataAvg <- reactive({
  LABELdataAvg0() %>%
    summarise(Avg = round(Avg, 0),  geometry = geometry)
  
})

MAPdata <- reactive({
    if(input$survtype == "All"){
  MAPdata0() %>%
    mutate(mosq_category = cut(Sum, 
                               breaks = c(-Inf, 0, 2.5, 4.5, 49.5, 129.5, 299.5, 999.5, Inf),
                               labels = c("0", 
                                          "1-2", 
                                          "3-4", 
                                          "5-49", 
                                          "50-129", 
                                          "130-299",
                                          "300-999",
                                          "1000+"),
                               include.lowest = TRUE)) } else {
    if(input$survtype == "Sweep"){          
    MAPdata0() %>%
    mutate(mosq_category = cut(Sum, 
                               breaks = c(-Inf, 0, 2.5, 10.5, 20.5, 50.5, Inf),
                               labels = c("0", 
                                          "1-2", 
                                          "3-10", 
                                          "11-20", 
                                          "21-50", 
                                          "51+"),
                               include.lowest = TRUE)) } else {                              
    
    if(input$survtype == "Gravid"){
    MAPdata0() %>%
      mutate(mosq_category = cut(Sum, 
                               breaks = c(-Inf, 0, 4.5, 9.5, 29.5, 49.5, Inf),
                               labels = c("0", 
                               "1-4", 
                               "5-9", 
                               "10-29", 
                               "30-49", 
                               "50+"),
                               include.lowest = TRUE))
                               } else {
    if((input$survtype == "CO2(reg)" | input$survtype == "CO2(elev)") && (input$species == "Cx_pipiens_33" | input$species == "Cx_restuans_34" | input$species == "Cx_restuans/pipiens_372" | input$species == "Cx_salinarius_35" | input$species == "Cx_tarsalis_36")){
      MAPdata0() %>%
                                   mutate(mosq_category = cut(Sum, 
                                                              breaks = c(-Inf, 0, 4.5, 9.5, 29.5, 49.5, Inf),
                                                              labels = c("0", 
                                                                         "1-4", 
                                                                         "5-9", 
                                                                         "10-29", 
                                                                         "30-49", 
                                                                         "50+"),
                                                              include.lowest = TRUE)) } else {
      if((input$survtype == "CO2(reg)" | input$survtype == "CO2(elev)") && (input$species == "Cs_melanura_39")){
        MAPdata0() %>%
          mutate(mosq_category = cut(Sum, 
                                     breaks = c(-Inf, 0, 4.5, 9.5, 49.5, Inf),
                                     labels = c("0", 
                                                "1-4", 
                                                "5-9", 
                                                "10-49", 
                                                "50+ "),
                                     include.lowest = TRUE)) 
      } else {                                                          
      MAPdata0() %>%
      mutate(mosq_category = cut(Sum, 
                               breaks = c(-Inf, 0, 49.5, 129.5, 299.5, 999.5, Inf),
                               labels = c("0", 
                               "1-49", 
                               "50-129", 
                               "130-299", 
                               "300-999", 
                               "1000+"),
                                include.lowest = TRUE))  
                               }}}}}
})

MAPdataAVG <- reactive({ if(input$survtype == "All"){
  MAPdata0() %>%
    mutate(mosq_category = cut(Avg, 
                               breaks = c(-Inf, 0, 2.5, 4.5, 49.5, 129.5, 299.5, 999.5, Inf),
                               labels = c("0", 
                                          "1-2", 
                                          "3-4", 
                                          "5-49", 
                                          "50-129", 
                                          "130-299",
                                          "300-999",
                                          "1000+"),
                               include.lowest = TRUE)) } else {
                                 if(input$survtype == "Sweep"){          
                                   MAPdata0() %>%
                                     mutate(mosq_category = cut(Avg, 
                                                                breaks = c(-Inf, 0, 2.5, 10.5, 20.5, 50.5, Inf),
                                                                labels = c("0", 
                                                                           "1-2", 
                                                                           "3-10", 
                                                                           "11-20", 
                                                                           "21-50", 
                                                                           "51+"),
                                                                include.lowest = TRUE)) } else {                              
                                                                  
                                                                  if(input$survtype == "Gravid"){
                                                                    MAPdata0() %>%
                                                                      mutate(mosq_category = cut(Avg, 
                                                                                                 breaks = c(-Inf, 0, 4.5, 9.5, 29.5, 49.5, Inf),
                                                                                                 labels = c("0", 
                                                                                                            "1-4", 
                                                                                                            "5-9", 
                                                                                                            "10-29", 
                                                                                                            "30-49", 
                                                                                                            "50+"),
                                                                                                 include.lowest = TRUE))
                                                                  } else {
                                                                    if((input$survtype == "CO2(reg)" | input$survtype == "CO2(elev)") && (input$species == "Cx_pipiens_33" | input$species == "Cx_restuans_34" | input$species == "Cx_restuans/pipiens_372" | input$species == "Cx_salinarius_35" | input$species == "Cx_tarsalis_36")){
                                                                      MAPdata0() %>%
                                                                        mutate(mosq_category = cut(Avg, 
                                                                                                   breaks = c(-Inf, 0, 4.5, 9.5, 29.5, 49.5, Inf),
                                                                                                   labels = c("0", 
                                                                                                              "1-4", 
                                                                                                              "5-9", 
                                                                                                              "10-29", 
                                                                                                              "30-49", 
                                                                                                              "50+"),
                                                                                                   include.lowest = TRUE)) } else {
                                                                                                     if((input$survtype == "CO2(reg)" | input$survtype == "CO2(elev)") && (input$species == "Cs_melanura_39")){
                                                                                                       MAPdata0() %>%
                                                                                                         mutate(mosq_category = cut(Avg, 
                                                                                                                                    breaks = c(-Inf, 0, 4.5, 9.5, 49.5, Inf),
                                                                                                                                    labels = c("0", 
                                                                                                                                               "1-4", 
                                                                                                                                               "5-9", 
                                                                                                                                               "10-49", 
                                                                                                                                               "50+ "),
                                                                                                                                    include.lowest = TRUE)) 
                                                                                                     } else {                                                          
                                                                                                       MAPdata0() %>%
                                                                                                         mutate(mosq_category = cut(Avg, 
                                                                                                                                    breaks = c(-Inf, 0, 49.5, 129.5, 299.5, 999.5, Inf),
                                                                                                                                    labels = c("0", 
                                                                                                                                               "1-49", 
                                                                                                                                               "50-129", 
                                                                                                                                               "130-299", 
                                                                                                                                               "300-999", 
                                                                                                                                               "1000+"),
                                                                                                                                    include.lowest = TRUE))  
                                                                                                     }}}}}
})

#Missing table Filtering

mosquitoMISS <- reactive({
  if (isTRUE(input$survtype == "All")) {dfmapMISS2163UTM}
  else {
    dfmapMISS2163UTM %>%
      filter(survtypename %in% input$survtype)
  }
})

mosqsppFilterMISS <- reactive({
  mosquitoMISS() %>%
    filter(spp_name %in% input$species)
})

mosqdateFilterMISS <- reactive({
  mosqsppFilterMISS() %>% 
    filter(between(inspdate, input$daterange[1], input$daterange[2]))
})

MAPdataMISS0 <- reactive({
  mosqdateFilterMISS() %>%
    group_by(loc_code) %>%
    summarise(Missing = "Missing", geometry = geometry, spp_name = spp_name, loc_code = loc_code)
})  

MAPdataMISS <- reactive({
  MAPdataMISS0() %>%
    mutate(mosq_category = factor("Missing"))
})

observeEvent(input$button, {
  whichplot(!whichplot())
})


BASEmap <- ggplot() + 
           #geom_sf(data= UTMshapeZONEdata, fill = "grey") + 
           # geom_sf( data = UTMshapeLAKEdata, fill = "lightblue") + 
           # geom_sf( data = UTMshapeCITYdata,  alpha = 0) + 
           geom_sf( data = UTMshapeCOUNTYdata, linewidth = .8) +
           theme_void()


   whichmap <- reactive({
      if(isTRUE(whichplot())) {
        BASEmap +
        geom_sf(data = MAPdataMISS(),
                  aes(color = mosq_category,
                      shape = mosq_category,
                      size = mosq_category,
                      text = sprintf(
                        "%s<br>Missing",
                        loc_code
                      ))) +
        geom_sf(mapping = aes(color = mosq_category, shape = mosq_category, size = mosq_category, 
                              text = sprintf(
                                "%s<br>%d",
                                loc_code, Sum
                              )), data = MAPdata()) +
        ggtitle(req(TitleString())) +
          scale_color_manual(name = "Mosq Sum",
                             values = c("0" = "brown", 
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
                                        "50-129" = "#fd8d3c",
                                        "130-299" = "#fc4e2a",
                                        "300-999" = "#e31a1c",
                                        "1000+" = "#b10026",
                                        "Missing" = "black")) +
          scale_shape_manual(name = "Mosq Sum",
                             values = c("0" = 1, 
                                        "1-2" = 17, 
                                        "3-4" = 17,
                                        "5-49" = 16,
                                        "50-129" = 18,
                                        "130-299" = 15,
                                        "300-999" = 15,
                                        "1000+" = 15,
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
                                        "10-49" = 18,
                                        "Missing" = 4)) +
          scale_size_manual(name = "Mosq Sum",
                             values = c("0" = 2, 
                                        "1-2" = 3, 
                                        "3-4" = 4,
                                        "5-49" = 5,
                                        "50-129" = 6,
                                        "130-299" = 6,
                                        "300-999" = 7,
                                        "1000+" = 8,
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
                                        "10-49" = 5,
                                        "Missing" = 2)) +
          guides(
            color = guide_legend(override.aes = list(size = 7)),
            shape = guide_legend(override.aes = list(size = 7)),
            size = guide_legend(override.aes = list(size = 7))
          ) +
          theme(plot.title = element_text(hjust = .5)) 
          
        } else {
          BASEmap +
            geom_sf(data = MAPdataMISS(),
                    aes(color = mosq_category,
                        shape = mosq_category,
                        size = mosq_category,
                        text = sprintf(
                          "%s<br>Missing",
                          loc_code
                        ))) +
            geom_sf(mapping = aes(color = mosq_category, shape = mosq_category, size = mosq_category, 
                                  text = sprintf(
                                    "%s<br>%.1f",
                                    loc_code, Avg
                                  )), data = MAPdataAVG()) +
            ggtitle(req(TitleStringAVG())) +
            scale_color_manual(name = "Mosq Avg",
                               values = c("0" = "brown", 
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
                                          "50-129" = "#fd8d3c",
                                          "130-299" = "#fc4e2a",
                                          "300-999" = "#e31a1c",
                                          "1000+" = "#b10026",
                                          "Missing" = "black")) +
            scale_shape_manual(name = "Mosq Avg",
                               values = c("0" = 1, 
                                          "1-2" = 17, 
                                          "3-4" = 17,
                                          "5-49" = 16,
                                          "50-129" = 18,
                                          "130-299" = 15,
                                          "300-999" = 15,
                                          "1000+" = 15,
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
                                          "10-49" = 18,
                                          "Missing" = 4)) +
            scale_size_manual(name = "Mosq Avg",
                              values = c("0" = 2, 
                                         "1-2" = 3, 
                                         "3-4" = 4,
                                         "5-49" = 5,
                                         "50-129" = 6,
                                         "130-299" = 6,
                                         "300-999" = 7,
                                         "1000+" = 8,
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
                                         "10-49" = 5,
                                         "Missing" = 2)) +
            guides(
              color = guide_legend(override.aes = list(size = 9)),
              shape = guide_legend(override.aes = list(size = 9)),
              size = guide_legend(override.aes = list(size = 9))
            ) +
            theme(plot.title = element_text(hjust = 1)) 
          
        }
       
    })
   
   whichmapfinal <- reactive({
     if(input$labels == TRUE & isTRUE(whichplot())) {
       whichmap() + geom_sf_text(aes(label = Sum), data = LABELdataSum(), size = 3, fontface = "bold") }
     else {
       if(input$labels == TRUE & isFALSE(whichplot())){
         whichmap() + geom_sf_text(aes(label = Avg), data = LABELdataAvg(), size = 3, fontface = "bold")
       } else {
       whichmap()}
     }
     
   })
    
    output$MAP <- renderPlotly({ggplotly(whichmapfinal(), tooltip = "text") %>%
        layout(
          hoverlabel = list(bgcolor = "white"),
          legend = list(x = 1.1, y = 0.5),
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        )})    

   
}

# Run the application 
shinyApp(ui = ui, server = server)
