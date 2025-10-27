library(shiny)
library(vroom)
library(tidyverse)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(dtplyr)
library(shinyWidgets)
library(plotrix)
library(DBI)
library(RPostgreSQL)

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

# Database configuration using environment variables
host_db <- Sys.getenv("DB_HOST")
db_port <- Sys.getenv("DB_PORT")
db_user <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")
db_name <- Sys.getenv("DB_NAME")
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname = db_name, host=host_db, port=db_port, user=db_user, password=db_password )

mosquito0NAS <- dbReadTable(con, "dbadult_mon_nt_co2_tall2_forr")

mosquito0 <- na.omit(mosquito0NAS)

dbDisconnect(con)
#mosquito0 <- vroom::vroom("Mosquito0Test.csv")

#mosquito1 <- mosquito0 %>% 
  #mutate(inspdate = mdy(inspdate))

#mosquito <- mosquito0 %>% 
  #group_by(inspdate, spp_name) %>% 
  #summarise(avg = round(mean(mosqcount), 1), sd = std.error(mosqcount), sum = sum(mosqcount))

mosquito0$Year<- year(mosquito0$inspdate)

colorspecieslist <- list("Total_Ae_+_Cq" = "#000000", Total_Ae_springs = "#008000", Total_Ae_summers = "#ffa500",
                         Cq_perturbans_42 = "#800080", Total_Cx_vectors = "#FF0000", Cx_erraticus_32 = "#000000",
                         Cx_pipiens_33 = "#0000FF", Cx_restuans_34 = "#008000", Cx_salinarius_35 = "#87cefa",
                         Cx_tarsalis_36 = "#a52a2a", Cx_territans_37 = "#00ff7f", "Cx_restuans/pipiens_372" = "#40e0d0",
                         Cx_unknown_371 = "#ffa500", An_barberi_27 = "#FFFF00", An_earlei_28 = "#ffc0cb",
                         An_punctipennis_29 = "#0000FF", An_quadrimaculatus_30 = "#FF0000", An_walkeri_31 = "#ffa500",
                         sp311an_un = "#800080", Total_Anopheles = "#87cefa", sp01_abser = "#FF0000", sp03_aurif = "#FFFF00",
                         sp04_euedes = "#f08080", sp05_campest = "#adff2f", sp08_commun = "#483d8b", sp09_diant = "#00FFFF",
                         sp118abpun = "#800080", sp11_excru = "#ffa500", sp12_fitch = "#a52a2a", sp13_flave = "#800000",
                         sp14_imple = "#7fff00", sp15_intrud = "#ffd700", sp17_pioni = "#FF00FF", sp18_punct = "#0000FF",
                         sp19_ripar = "#008000", sp20_spenc = "#ff1493", sp22_stimu = "#708090", sp23_provo = "#ff6347",
                         Ae_cinereus_7 = "#006400", Ae_triseriatus_24 = "#0000FF", Ae_vexans_26 = "#FF0000",
                         sp02_atrop = "#ff1493", Ae_canadensis_6 = "#000000", Ae_dorsalis_10 = "#808080", sp16_nigro = "#ffd700",
                         sp21_stict = "#FF00FF", sp25_trivi = "#800080", sp261ae_unid = "#000000", sp262spr_unid = "#008000",
                         sp264summ_unid = "#ffa500", sp50_hende = "#7fff00", Ae_albopictus_51 = "#FF0000",
                         Ae_japonicus_52 = "#008000", Ps_ciliata_44 = "#a52a2a", Ps_columbiae_45 = "#008000",
                         Ps_ferox_46 = "#000000", sp471ps_un = "#808080", Ps_horrida_47 = "#FF0000", sp38_inorn = "#0000FF",
                         Total_Psorophora = "#00FFFF", Culiseta_melanura = "#FF0000", sp40_minne = "#ffa500", sp41_morsi = "#a52a2a",
                         sp411cs_un = "#808080", Or_signifera_43 = "#87cefa", Ur_sapphirina_48 = "#00008b", sp49_smith = "#0000FF" )

shapespecieslist <- list("Total_Ae_+_Cq" = 1, Total_Ae_springs = 1, Total_Ae_summers = 1, Cq_perturbans_42 = 1,
                         Total_Cx_vectors = 1, Cx_erraticus_32 = 15, Cx_pipiens_33 = 15, Cx_restuans_34 = 15,
                         Cx_salinarius_35 = 15, Cx_tarsalis_36 = 15, Cx_territans_37 = 15, "Cx_restuans/pipiens_372" = 15,
                         Cx_unknown_371 = 15, An_barberi_27 = 4, An_earlei_28 = 4, An_punctipennis_29 = 4,
                         An_quadrimaculatus_30 = 4, An_walkeri_31 = 4, sp311an_un = 4, Total_Anopheles = 4,
                         sp01_abser = 19, sp03_aurif = 19, sp04_euedes = 19, sp05_campest = 19, sp08_commun = 19,
                         sp09_diant = 19, sp118abpun = 19, sp11_excru = 19, sp12_fitch = 19, sp13_flave = 19,
                         sp14_imple = 19, sp15_intrud = 19, sp17_pioni = 19, sp18_punct = 19, sp19_ripar = 19,
                         sp20_spenc = 19, sp22_stimu = 19, sp23_provo = 19, Ae_cinereus_7 = 19, Ae_triseriatus_24 = 19,
                         Ae_vexans_26 = 19, sp02_atrop = 19, Ae_canadensis_6 = 19, Ae_dorsalis_10 = 19, sp16_nigro = 19,
                         sp21_stict = 19, sp25_trivi = 19, sp261ae_unid = 19, sp262spr_unid = 19, sp264summ_unid = 19,
                         sp50_hende = 19, Ae_albopictus_51 = 19, Ae_japonicus_52 = 19, Ps_ciliata_44 = 3,
                         Ps_columbiae_45 = 3, Ps_ferox_46 = 3, sp471ps_un = 3, Ps_horrida_47 = 3, sp38_inorn = 3,
                         Total_Psorophora = 3, Culiseta_melanura = 18, sp40_minne = 18, sp41_morsi = 18, sp411cs_un = 18,
                         Or_signifera_43 = 18, Ur_sapphirina_48 = 18, sp49_smith = 18)
              

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  tabsetPanel(
    tabPanel( "Compare",
              
              titlePanel("CO2 Traps"),
              fluidRow(
                column(3, selectInput("facility", "Facility", choices = c("All Facilities" = "all"),
                                      selected = "all")),
                column(4, selectInput("species", "Species Selection", choices = unique(mosquito0$spp_name),
                                      selected = "Total_Ae_+_Cq",
                                      multiple = TRUE)),
                column(3, tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}",
                                     ".irs-grid-text {font-size: 8pt;}"),
                       sliderInput("years", "Years",
                                   min(mosquito0$Year), max(mosquito0$Year),
                                   value = c(min(mosquito0$Year), max(mosquito0$Year)),
                                   sep = "")),
                column(1, actionButton("logbut", "Y-Scale")),
                column(1, actionButton("button", "Standard Error"))
              ),
              wellPanel( fluidRow(
                div( style = "position:relative",
                     plotOutput("chart", 
                                hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                     uiOutput("hover_info",  style = "pointer-events: none")))
              ),
              wellPanel(fluidRow(
                div( style = "position:relative",
                     plotOutput("chart1", 
                                hover = hoverOpts("plot_hover1", delay = 100, delayType = "debounce")),
                     uiOutput("hover_info1",  style = "pointer-events: none"))
              ))),
   #ALL Tab W/ Zone Selector
    tabPanel( "All",
              fluidRow(
                column(3, selectInput("facilityONE", "Facility", choices = c("All Facilities" = "all"),
                                      selected = "all")),
                column(3, selectInput("speciesONE", "Species Selection", choices = unique(mosquito0$spp_name),
                                      selected = "Total_Ae_+_Cq",
                                      multiple = TRUE)),
                column(1, selectInput("zoneONE", "Zone", choices = c(1, "2+X", "All"),
                                      selected = "All")),
                column(3, tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}",
                                     ".irs-grid-text {font-size: 8pt;}"),
                       sliderInput("yearsONE", "Years",
                                   min(mosquito0$Year), max(mosquito0$Year),
                                   value = c(min(mosquito0$Year), max(mosquito0$Year)),
                                   sep = "")),
                column(1, actionButton("logbutONE", "Y-Scale")),
                column(1, actionButton("buttonONE", "Standard Error"))
              ),
              wellPanel( fluidRow(
                div( style = "position:relative",
                     plotOutput("chartONE", 
                                hover = hoverOpts("plot_hoverONE", delay = 100, delayType = "debounce")),
                     uiOutput("hover_infoONE",  style = "pointer-events: none")))
              )     
      
    )
  )
)


server <- function(input, output, session) {
  
  # Update facility choices with full names from db_helpers
  observe({
    tryCatch({
      facility_choices <- get_facility_choices(include_all = TRUE)
      updateSelectInput(session, "facility", choices = facility_choices)
      updateSelectInput(session, "facilityONE", choices = facility_choices)
    }, error = function(e) {
      # Fallback to current data if db_helpers fails
      facility_fallback <- c("All Facilities" = "all", setNames(unique(mosquito0$facility), unique(mosquito0$facility)))
      updateSelectInput(session, "facility", choices = facility_fallback)
      updateSelectInput(session, "facilityONE", choices = facility_fallback)
    })
  })
  
  logscale <- reactiveVal(TRUE)
  whichplot <- reactiveVal(TRUE)
  #Filter will have to include facility for both and then seperate filtersfor each graph for year, sp, zone in one then do calculations
  mosquito <- reactive({
    if (isTRUE(input$facility == "all")) {mosquito0}
    else {
    mosquito0 %>%
    filter(facility %in% input$facility)
    }
  })
  
  datafilter <- reactive({
    mosquito() %>% 
      filter(between(Year, input$years[1], input$years[2]) & spp_name %in% input$species & zone %in% 1)
  })
  
  data <- reactive({
    datafilter() %>% 
    group_by(inspdate, spp_name) %>% 
    summarise(avg = round(mean(mosqcount), 1), sd = std.error(mosqcount), sum = sum(mosqcount), Year = Year)
  })
  
  # Looping through each group of year and species
  #for (i in 1:nrow(dfsdtest)) {
  unique_species <- reactive({unique(data()$spp_name)})
  unique_year <- reactive({unique(data()$Year)})
  new_data = data.frame()
  
  Dummy <- reactive({
  lapply(unique_year(), function(i) {
    
      new_row <- data.frame(Year = i, spp_name = input$species, avg = NA, sd = NA, inspdate = as.Date(paste(i, "-05-01", sep = "")))
      new_data <- rbind(new_data, new_row)
    })
  })
  # Combining the original data with the new rows
  RealData <- reactive({
    rbind(data(), Dummy())
  })
  #Second set
  datafilter1 <- reactive({
    mosquito() %>% 
      filter(between(Year, input$years[1], input$years[2]) & spp_name %in% input$species & zone %in% c(2, "X"))
  })
  
  data1 <- reactive({
    datafilter1() %>% 
      group_by(inspdate, spp_name) %>% 
      summarise(avg = round(mean(mosqcount), 1), sd = std.error(mosqcount), sum = sum(mosqcount), Year = Year)
  })
  
  unique_year1 <- reactive({unique(datafilter1()$Year)})
  new_data1 = data.frame()
  
  Dummy1 <- reactive({
    lapply(unique_year1(), function(i) {
      
      new_row1 <- data.frame(Year = i, spp_name = input$species, avg = NA, sd = NA, inspdate = as.Date(paste(i, "-05-01", sep = "")))
      new_data1 <- rbind(new_data1, new_row1)
    })
  })
  
  RealData1 <- reactive({
    rbind(data1(), Dummy1())
  })
  #Ylims Compare Graph
  heightlim <- reactive({
                   if (isTRUE(max(data()$avg) >= max(data1()$avg))) 
                        {max(data()$avg)} 
                   else {max(data1()$avg)} 
                  })
  
  heightlimsd <- reactive({
                        if (isTRUE((max(data()$avg) + max(data()$sd)) >= (max(data1()$avg)) + max(data1()$sd))) 
                          {max(data()$avg) + max(data()$sd)} 
                        else {max(data1()$avg) + max(data1()$sd)} 
                  })
  #Date Breaks
  breaks <- reactive({
    switch(input$years[2] - input$years[1] + 1, "1 week", "1 month", "1 month", "6 months",
           "6 months", "6 months", "6 months", "6 months", "6 months", "6 months", "6 months", "6 months", "6 months", "6 months")
  })
  
  breaks1 <- reactive({
    switch(input$years[2] - input$years[1] + 1, "1 week", "1 month", "1 month", "6 months",
           "6 months", "6 months", "6 months", "6 months", "6 months", "6 months", "6 months", "6 months", "6 months", "6 months")
  })
  
  observeEvent(input$button, {
     whichplot(!whichplot())
  })
  
  observeEvent(input$logbut, {
    logscale(!logscale())
  })
  
  whichscale <- reactive({
    if (logscale()) {"identity"} else {scales::pseudo_log_trans(base = 10, sigma = 1)}
  })
  
  hoverdata <- reactive({
    data() %>%
      mutate(obs = sum/avg)
  })
  
  logdata <- reactive({
    hoverdata() %>%
      mutate(logavg = log10(avg))
  })
  
  hoverlogswitch <- reactive({
    if (logscale()) {hoverdata()} else {logdata()}
  })
  
  hoverdata1 <- reactive({
    data1() %>%
      mutate(obs = sum/avg)
  })
  
  logdata1 <- reactive({
    hoverdata1() %>%
      mutate(logavg = log10(avg))
  })
  
  hoverlogswitch1 <- reactive({
    if (logscale()) {hoverdata1()} else {logdata1()}
  })
  
  yvarhoverswitch <- reactive({
    if (logscale()) {"avg"} else {"logavg"}
  })
  
  
  
  whichchart <- reactive({
    if (isTRUE(whichplot())) {
      ggplot( RealData(), aes(x=inspdate, y=avg)) + 
        scale_x_date(position = "top", date_breaks = breaks(), date_labels = "%Y/%m/%d") +
        scale_y_continuous(limits = c(0, heightlim()), trans = whichscale()) +
        theme(axis.text.x = element_text(size=15, angle = -45, hjust = 1),
              axis.title.x = element_text(size=15, margin=margin(t=20, r=0, b=5, l=0, unit = "pt")),
              axis.text.y = element_text(size=20),
              axis.title.y = element_text(size=15, margin=margin(t=0, r=15, b=0, l=0, unit = "pt")),
              legend.text = element_text(size = 20), legend.title = element_text(size = 25),
              legend.position = "right"
              )+
        xlab("Inspection Date") +
        ylab("Mosquitos (Average/Trap) Zone 1") +
        #ggtitle("Mosquito Avg per CO2 Trap") +
        geom_line(aes(color = spp_name)) + scale_color_manual(name = "Species", values=c(
          "Total_Ae_+_Cq" = "#000000", Total_Ae_springs = "#008000", Total_Ae_summers = "#ffa500",
          Cq_perturbans_42 = "#800080", Total_Cx_vectors = "#FF0000", Cx_erraticus_32 = "#000000",
          Cx_pipiens_33 = "#0000FF", Cx_restuans_34 = "#008000", Cx_salinarius_35 = "#87cefa",
          Cx_tarsalis_36 = "#a52a2a", Cx_territans_37 = "#00ff7f", "Cx_restuans/pipiens_372" = "#40e0d0",
          Cx_unknown_371 = "#ffa500", An_barberi_27 = "#FFFF00", An_earlei_28 = "#ffc0cb",
          An_punctipennis_29 = "#0000FF", An_quadrimaculatus_30 = "#FF0000", An_walkeri_31 = "#ffa500",
          sp311an_un = "#800080", Total_Anopheles = "#87cefa", sp01_abser = "#FF0000", sp03_aurif = "#FFFF00",
          sp04_euedes = "#f08080", sp05_campest = "#adff2f", sp08_commun = "#483d8b", sp09_diant = "#00FFFF",
          sp118abpun = "#800080", sp11_excru = "#ffa500", sp12_fitch = "#a52a2a", sp13_flave = "#800000",
          sp14_imple = "#7fff00", sp15_intrud = "#ffd700", sp17_pioni = "#FF00FF", sp18_punct = "#0000FF",
          sp19_ripar = "#008000", sp20_spenc = "#ff1493", sp22_stimu = "#708090", sp23_provo = "#ff6347",
          Ae_cinereus_7 = "#006400", Ae_triseriatus_24 = "#0000FF", Ae_vexans_26 = "#FF0000",
          sp02_atrop = "#ff1493", Ae_canadensis_6 = "#000000", Ae_dorsalis_10 = "#808080", sp16_nigro = "#ffd700",
          sp21_stict = "#FF00FF", sp25_trivi = "#800080", sp261ae_unid = "#000000", sp262spr_unid = "#008000",
          sp264summ_unid = "#ffa500", sp50_hende = "#7fff00", Ae_albopictus_51 = "#FF0000",
          Ae_japonicus_52 = "#008000", Ps_ciliata_44 = "#a52a2a", Ps_columbiae_45 = "#008000",
          Ps_ferox_46 = "#000000", sp471ps_un = "#808080", Ps_horrida_47 = "#FF0000", sp38_inorn = "#0000FF",
          Total_Psorophora = "#00FFFF", Culiseta_melanura = "#FF0000", sp40_minne = "#ffa500", sp41_morsi = "#a52a2a",
          sp411cs_un = "#808080", Or_signifera_43 = "#87cefa", Ur_sapphirina_48 = "#00008b", sp49_smith = "#0000FF"
        )) +
        scale_shape_manual(name = "Species",  values = c(
          "Total_Ae_+_Cq" = 1, Total_Ae_springs = 1, Total_Ae_summers = 1, Cq_perturbans_42 = 1,
          Total_Cx_vectors = 1, Cx_erraticus_32 = 15, Cx_pipiens_33 = 15, Cx_restuans_34 = 15,
          Cx_salinarius_35 = 15, Cx_tarsalis_36 = 15, Cx_territans_37 = 15, "Cx_restuans/pipiens_372" = 15,
          Cx_unknown_371 = 15, An_barberi_27 = 4, An_earlei_28 = 4, An_punctipennis_29 = 4,
          An_quadrimaculatus_30 = 4, An_walkeri_31 = 4, sp311an_un = 4, Total_Anopheles = 4,
          sp01_abser = 19, sp03_aurif = 19, sp04_euedes = 19, sp05_campest = 19, sp08_commun = 19,
          sp09_diant = 19, sp118abpun = 19, sp11_excru = 19, sp12_fitch = 19, sp13_flave = 19,
          sp14_imple = 19, sp15_intrud = 19, sp17_pioni = 19, sp18_punct = 19, sp19_ripar = 19,
          sp20_spenc = 19, sp22_stimu = 19, sp23_provo = 19, Ae_cinereus_7 = 19, Ae_triseriatus_24 = 19,
          Ae_vexans_26 = 19, sp02_atrop = 19, Ae_canadensis_6 = 19, Ae_dorsalis_10 = 19, sp16_nigro = 19,
          sp21_stict = 19, sp25_trivi = 19, sp261ae_unid = 19, sp262spr_unid = 19, sp264summ_unid = 19,
          sp50_hende = 19, Ae_albopictus_51 = 19, Ae_japonicus_52 = 19, Ps_ciliata_44 = 3,
          Ps_columbiae_45 = 3, Ps_ferox_46 = 3, sp471ps_un = 3, Ps_horrida_47 = 3, sp38_inorn = 3,
          Total_Psorophora = 3, Culiseta_melanura = 18, sp40_minne = 18, sp41_morsi = 18, sp411cs_un = 18,
          Or_signifera_43 = 18, Ur_sapphirina_48 = 18, sp49_smith = 18
        )) +
        geom_point(aes(color = spp_name, shape = spp_name), size=3)
      
    } else {
      ggplot( RealData(), aes(x=inspdate, y=avg)) + 
        scale_x_date(position = "top", date_breaks = breaks(), date_labels = "%Y/%m/%d") +
        scale_y_continuous(limits = c(0, heightlimsd()), trans = whichscale()) +
        theme(axis.text.x = element_text(size=15, angle = -45, hjust = 1),
              axis.title.x = element_text(size=15, margin=margin(t=20, r=0, b=5, l=0, unit = "pt")),
              axis.text.y = element_text(size=20),
              axis.title.y = element_text(size=15, margin=margin(t=0, r=15, b=0, l=0, unit = "pt")),
              legend.text = element_text(size = 20), legend.title = element_text(size = 25),
              legend.position = "right"
        )+
        xlab("Inspection Date") +
        ylab("Mosquitos (Average/Trap) Zone 1") +
        #ggtitle("Mosquito Avg per CO2 Trap") +
        geom_line(aes(color = spp_name)) + scale_color_manual(name = "Species", values=c(
          "Total_Ae_+_Cq" = "#000000", Total_Ae_springs = "#008000", Total_Ae_summers = "#ffa500",
          Cq_perturbans_42 = "#800080", Total_Cx_vectors = "#FF0000", Cx_erraticus_32 = "#000000",
          Cx_pipiens_33 = "#0000FF", Cx_restuans_34 = "#008000", Cx_salinarius_35 = "#87cefa",
          Cx_tarsalis_36 = "#a52a2a", Cx_territans_37 = "#00ff7f", "Cx_restuans/pipiens_372" = "#40e0d0",
          Cx_unknown_371 = "#ffa500", An_barberi_27 = "#FFFF00", An_earlei_28 = "#ffc0cb",
          An_punctipennis_29 = "#0000FF", An_quadrimaculatus_30 = "#FF0000", An_walkeri_31 = "#ffa500",
          sp311an_un = "#800080", Total_Anopheles = "#87cefa", sp01_abser = "#FF0000", sp03_aurif = "#FFFF00",
          sp04_euedes = "#f08080", sp05_campest = "#adff2f", sp08_commun = "#483d8b", sp09_diant = "#00FFFF",
          sp118abpun = "#800080", sp11_excru = "#ffa500", sp12_fitch = "#a52a2a", sp13_flave = "#800000",
          sp14_imple = "#7fff00", sp15_intrud = "#ffd700", sp17_pioni = "#FF00FF", sp18_punct = "#0000FF",
          sp19_ripar = "#008000", sp20_spenc = "#ff1493", sp22_stimu = "#708090", sp23_provo = "#ff6347",
          Ae_cinereus_7 = "#006400", Ae_triseriatus_24 = "#0000FF", Ae_vexans_26 = "#FF0000",
          sp02_atrop = "#ff1493", Ae_canadensis_6 = "#000000", Ae_dorsalis_10 = "#808080", sp16_nigro = "#ffd700",
          sp21_stict = "#FF00FF", sp25_trivi = "#800080", sp261ae_unid = "#000000", sp262spr_unid = "#008000",
          sp264summ_unid = "#ffa500", sp50_hende = "#7fff00", Ae_albopictus_51 = "#FF0000",
          Ae_japonicus_52 = "#008000", Ps_ciliata_44 = "#a52a2a", Ps_columbiae_45 = "#008000",
          Ps_ferox_46 = "#000000", sp471ps_un = "#808080", Ps_horrida_47 = "#FF0000", sp38_inorn = "#0000FF",
          Total_Psorophora = "#00FFFF", Culiseta_melanura = "#FF0000", sp40_minne = "#ffa500", sp41_morsi = "#a52a2a",
          sp411cs_un = "#808080", Or_signifera_43 = "#87cefa", Ur_sapphirina_48 = "#00008b", sp49_smith = "#0000FF"
        )) +
        scale_shape_manual(name = "Species",  values = c(
          "Total_Ae_+_Cq" = 1, Total_Ae_springs = 1, Total_Ae_summers = 1, Cq_perturbans_42 = 1,
          Total_Cx_vectors = 1, Cx_erraticus_32 = 15, Cx_pipiens_33 = 15, Cx_restuans_34 = 15,
          Cx_salinarius_35 = 15, Cx_tarsalis_36 = 15, Cx_territans_37 = 15, "Cx_restuans/pipiens_372" = 15,
          Cx_unknown_371 = 15, An_barberi_27 = 4, An_earlei_28 = 4, An_punctipennis_29 = 4,
          An_quadrimaculatus_30 = 4, An_walkeri_31 = 4, sp311an_un = 4, Total_Anopheles = 4,
          sp01_abser = 19, sp03_aurif = 19, sp04_euedes = 19, sp05_campest = 19, sp08_commun = 19,
          sp09_diant = 19, sp118abpun = 19, sp11_excru = 19, sp12_fitch = 19, sp13_flave = 19,
          sp14_imple = 19, sp15_intrud = 19, sp17_pioni = 19, sp18_punct = 19, sp19_ripar = 19,
          sp20_spenc = 19, sp22_stimu = 19, sp23_provo = 19, Ae_cinereus_7 = 19, Ae_triseriatus_24 = 19,
          Ae_vexans_26 = 19, sp02_atrop = 19, Ae_canadensis_6 = 19, Ae_dorsalis_10 = 19, sp16_nigro = 19,
          sp21_stict = 19, sp25_trivi = 19, sp261ae_unid = 19, sp262spr_unid = 19, sp264summ_unid = 19,
          sp50_hende = 19, Ae_albopictus_51 = 19, Ae_japonicus_52 = 19, Ps_ciliata_44 = 3,
          Ps_columbiae_45 = 3, Ps_ferox_46 = 3, sp471ps_un = 3, Ps_horrida_47 = 3, sp38_inorn = 3,
          Total_Psorophora = 3, Culiseta_melanura = 18, sp40_minne = 18, sp41_morsi = 18, sp411cs_un = 18,
          Or_signifera_43 = 18, Ur_sapphirina_48 = 18, sp49_smith = 18
        )) +
        #geom_point(aes(color = spp_name)) +
        geom_pointrange(aes(ymin=avg-sd, ymax=avg+sd, color = spp_name, shape = spp_name))
    }
  })
  
  output$chart <- renderPlot({   
    whichchart()
  })
  
  whichchart1 <- reactive({
    if (isTRUE(whichplot())) {
      ggplot( RealData1(), aes(x=inspdate, y=avg)) + 
        scale_x_date(date_breaks = breaks1(), date_labels = "%Y/%m/%d") +
        scale_y_continuous(limits = c(0, heightlim()), trans = whichscale()) +
        theme(axis.text.x = element_text(size=15, angle = 45, hjust = 1),
              axis.title.x = element_text(size=15, margin=margin(t=20, r=0, b=5, l=0, unit = "pt")),
              axis.text.y = element_text(size=20),
              axis.title.y = element_text(size=15, margin=margin(t=0, r=15, b=0, l=0, unit = "pt")),
              legend.text = element_text(size = 20), legend.title = element_text(size = 25),
              legend.position = "right")+
        xlab("Inspection Date") +
        ylab("Mosquitos (Average/Trap) Zone 2+X") +
        #ggtitle("Mosquito Avg per CO2 Trap") +
        geom_line(aes(color = spp_name)) + scale_color_manual(name = "Species", values=c(
          "Total_Ae_+_Cq" = "#000000", Total_Ae_springs = "#008000", Total_Ae_summers = "#ffa500",
          Cq_perturbans_42 = "#800080", Total_Cx_vectors = "#FF0000", Cx_erraticus_32 = "#000000",
          Cx_pipiens_33 = "#0000FF", Cx_restuans_34 = "#008000", Cx_salinarius_35 = "#87cefa",
          Cx_tarsalis_36 = "#a52a2a", Cx_territans_37 = "#00ff7f", "Cx_restuans/pipiens_372" = "#40e0d0",
          Cx_unknown_371 = "#ffa500", An_barberi_27 = "#FFFF00", An_earlei_28 = "#ffc0cb",
          An_punctipennis_29 = "#0000FF", An_quadrimaculatus_30 = "#FF0000", An_walkeri_31 = "#ffa500",
          sp311an_un = "#800080", Total_Anopheles = "#87cefa", sp01_abser = "#FF0000", sp03_aurif = "#FFFF00",
          sp04_euedes = "#f08080", sp05_campest = "#adff2f", sp08_commun = "#483d8b", sp09_diant = "#00FFFF",
          sp118abpun = "#800080", sp11_excru = "#ffa500", sp12_fitch = "#a52a2a", sp13_flave = "#800000",
          sp14_imple = "#7fff00", sp15_intrud = "#ffd700", sp17_pioni = "#FF00FF", sp18_punct = "#0000FF",
          sp19_ripar = "#008000", sp20_spenc = "#ff1493", sp22_stimu = "#708090", sp23_provo = "#ff6347",
          Ae_cinereus_7 = "#006400", Ae_triseriatus_24 = "#0000FF", Ae_vexans_26 = "#FF0000",
          sp02_atrop = "#ff1493", Ae_canadensis_6 = "#000000", Ae_dorsalis_10 = "#808080", sp16_nigro = "#ffd700",
          sp21_stict = "#FF00FF", sp25_trivi = "#800080", sp261ae_unid = "#000000", sp262spr_unid = "#008000",
          sp264summ_unid = "#ffa500", sp50_hende = "#7fff00", Ae_albopictus_51 = "#FF0000",
          Ae_japonicus_52 = "#008000", Ps_ciliata_44 = "#a52a2a", Ps_columbiae_45 = "#008000",
          Ps_ferox_46 = "#000000", sp471ps_un = "#808080", Ps_horrida_47 = "#FF0000", sp38_inorn = "#0000FF",
          Total_Psorophora = "#00FFFF", Culiseta_melanura = "#FF0000", sp40_minne = "#ffa500", sp41_morsi = "#a52a2a",
          sp411cs_un = "#808080", Or_signifera_43 = "#87cefa", Ur_sapphirina_48 = "#00008b", sp49_smith = "#0000FF"
        )) +
        scale_shape_manual(name = "Species",  values = c(
          "Total_Ae_+_Cq" = 1, Total_Ae_springs = 1, Total_Ae_summers = 1, Cq_perturbans_42 = 1,
          Total_Cx_vectors = 1, Cx_erraticus_32 = 15, Cx_pipiens_33 = 15, Cx_restuans_34 = 15,
          Cx_salinarius_35 = 15, Cx_tarsalis_36 = 15, Cx_territans_37 = 15, "Cx_restuans/pipiens_372" = 15,
          Cx_unknown_371 = 15, An_barberi_27 = 4, An_earlei_28 = 4, An_punctipennis_29 = 4,
          An_quadrimaculatus_30 = 4, An_walkeri_31 = 4, sp311an_un = 4, Total_Anopheles = 4,
          sp01_abser = 19, sp03_aurif = 19, sp04_euedes = 19, sp05_campest = 19, sp08_commun = 19,
          sp09_diant = 19, sp118abpun = 19, sp11_excru = 19, sp12_fitch = 19, sp13_flave = 19,
          sp14_imple = 19, sp15_intrud = 19, sp17_pioni = 19, sp18_punct = 19, sp19_ripar = 19,
          sp20_spenc = 19, sp22_stimu = 19, sp23_provo = 19, Ae_cinereus_7 = 19, Ae_triseriatus_24 = 19,
          Ae_vexans_26 = 19, sp02_atrop = 19, Ae_canadensis_6 = 19, Ae_dorsalis_10 = 19, sp16_nigro = 19,
          sp21_stict = 19, sp25_trivi = 19, sp261ae_unid = 19, sp262spr_unid = 19, sp264summ_unid = 19,
          sp50_hende = 19, Ae_albopictus_51 = 19, Ae_japonicus_52 = 19, Ps_ciliata_44 = 3,
          Ps_columbiae_45 = 3, Ps_ferox_46 = 3, sp471ps_un = 3, Ps_horrida_47 = 3, sp38_inorn = 3,
          Total_Psorophora = 3, Culiseta_melanura = 18, sp40_minne = 18, sp41_morsi = 18, sp411cs_un = 18,
          Or_signifera_43 = 18, Ur_sapphirina_48 = 18, sp49_smith = 18
        )) +
        geom_point(aes(color = spp_name, shape = spp_name), size=3)
      
    } else {
      ggplot( RealData1(), aes(x=inspdate, y=avg)) + 
        scale_x_date(date_breaks = breaks1(), date_labels = "%Y/%m/%d") +
        scale_y_continuous(limits = c(0, heightlimsd()), trans = whichscale()) +
        theme(axis.text.x = element_text(size=15, angle = 45, hjust = 1),
              axis.title.x = element_text(size=15, margin=margin(t=20, r=0, b=5, l=0, unit = "pt")),
              axis.text.y = element_text(size=20),
              axis.title.y = element_text(size=15, margin=margin(t=0, r=15, b=0, l=0, unit = "pt")),
              legend.text = element_text(size = 20), legend.title = element_text(size = 25),
              legend.position = "right")+
        xlab("Inspection Date") +
        ylab("Mosquitos (Average/Trap) Zone 2+X") +
        #ggtitle("Mosquito Avg per CO2 Trap") +
        geom_line(aes(color = spp_name)) + scale_color_manual(name = "Species", values=c(
          "Total_Ae_+_Cq" = "#000000", Total_Ae_springs = "#008000", Total_Ae_summers = "#ffa500",
          Cq_perturbans_42 = "#800080", Total_Cx_vectors = "#FF0000", Cx_erraticus_32 = "#000000",
          Cx_pipiens_33 = "#0000FF", Cx_restuans_34 = "#008000", Cx_salinarius_35 = "#87cefa",
          Cx_tarsalis_36 = "#a52a2a", Cx_territans_37 = "#00ff7f", "Cx_restuans/pipiens_372" = "#40e0d0",
          Cx_unknown_371 = "#ffa500", An_barberi_27 = "#FFFF00", An_earlei_28 = "#ffc0cb",
          An_punctipennis_29 = "#0000FF", An_quadrimaculatus_30 = "#FF0000", An_walkeri_31 = "#ffa500",
          sp311an_un = "#800080", Total_Anopheles = "#87cefa", sp01_abser = "#FF0000", sp03_aurif = "#FFFF00",
          sp04_euedes = "#f08080", sp05_campest = "#adff2f", sp08_commun = "#483d8b", sp09_diant = "#00FFFF",
          sp118abpun = "#800080", sp11_excru = "#ffa500", sp12_fitch = "#a52a2a", sp13_flave = "#800000",
          sp14_imple = "#7fff00", sp15_intrud = "#ffd700", sp17_pioni = "#FF00FF", sp18_punct = "#0000FF",
          sp19_ripar = "#008000", sp20_spenc = "#ff1493", sp22_stimu = "#708090", sp23_provo = "#ff6347",
          Ae_cinereus_7 = "#006400", Ae_triseriatus_24 = "#0000FF", Ae_vexans_26 = "#FF0000",
          sp02_atrop = "#ff1493", Ae_canadensis_6 = "#000000", Ae_dorsalis_10 = "#808080", sp16_nigro = "#ffd700",
          sp21_stict = "#FF00FF", sp25_trivi = "#800080", sp261ae_unid = "#000000", sp262spr_unid = "#008000",
          sp264summ_unid = "#ffa500", sp50_hende = "#7fff00", Ae_albopictus_51 = "#FF0000",
          Ae_japonicus_52 = "#008000", Ps_ciliata_44 = "#a52a2a", Ps_columbiae_45 = "#008000",
          Ps_ferox_46 = "#000000", sp471ps_un = "#808080", Ps_horrida_47 = "#FF0000", sp38_inorn = "#0000FF",
          Total_Psorophora = "#00FFFF", Culiseta_melanura = "#FF0000", sp40_minne = "#ffa500", sp41_morsi = "#a52a2a",
          sp411cs_un = "#808080", Or_signifera_43 = "#87cefa", Ur_sapphirina_48 = "#00008b", sp49_smith = "#0000FF"
        )) +
        scale_shape_manual(name = "Species",  values = c(
          "Total_Ae_+_Cq" = 1, Total_Ae_springs = 1, Total_Ae_summers = 1, Cq_perturbans_42 = 1,
          Total_Cx_vectors = 1, Cx_erraticus_32 = 15, Cx_pipiens_33 = 15, Cx_restuans_34 = 15,
          Cx_salinarius_35 = 15, Cx_tarsalis_36 = 15, Cx_territans_37 = 15, "Cx_restuans/pipiens_372" = 15,
          Cx_unknown_371 = 15, An_barberi_27 = 4, An_earlei_28 = 4, An_punctipennis_29 = 4,
          An_quadrimaculatus_30 = 4, An_walkeri_31 = 4, sp311an_un = 4, Total_Anopheles = 4,
          sp01_abser = 19, sp03_aurif = 19, sp04_euedes = 19, sp05_campest = 19, sp08_commun = 19,
          sp09_diant = 19, sp118abpun = 19, sp11_excru = 19, sp12_fitch = 19, sp13_flave = 19,
          sp14_imple = 19, sp15_intrud = 19, sp17_pioni = 19, sp18_punct = 19, sp19_ripar = 19,
          sp20_spenc = 19, sp22_stimu = 19, sp23_provo = 19, Ae_cinereus_7 = 19, Ae_triseriatus_24 = 19,
          Ae_vexans_26 = 19, sp02_atrop = 19, Ae_canadensis_6 = 19, Ae_dorsalis_10 = 19, sp16_nigro = 19,
          sp21_stict = 19, sp25_trivi = 19, sp261ae_unid = 19, sp262spr_unid = 19, sp264summ_unid = 19,
          sp50_hende = 19, Ae_albopictus_51 = 19, Ae_japonicus_52 = 19, Ps_ciliata_44 = 3,
          Ps_columbiae_45 = 3, Ps_ferox_46 = 3, sp471ps_un = 3, Ps_horrida_47 = 3, sp38_inorn = 3,
          Total_Psorophora = 3, Culiseta_melanura = 18, sp40_minne = 18, sp41_morsi = 18, sp411cs_un = 18,
          Or_signifera_43 = 18, Ur_sapphirina_48 = 18, sp49_smith = 18
        )) +
        #geom_point(aes(color = species)) +
        geom_pointrange(aes(ymin=avg-sd, ymax=avg+sd, color = spp_name, shape = spp_name))
    }
  })
  
  output$chart1 <- renderPlot({   
    whichchart1()
  }) 
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(hoverlogswitch(), hover, yvar = yvarhoverswitch(), threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$coords_css$x
    top_px <- hover$coords_css$y
    
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;","padding: 5px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Date: </b>", point$inspdate, "<br/>",
                    "<b> Average: </b>", point$avg, "<br/>",
                    "<b> Standard Error: </b>", round(point$sd, 1), "<br/>",
                    "<b> Total: </b>", point$sum, "<br/>",
                    "<b> Observations: </b>", round(point$obs, 0), "<br/>",
                    "<b> Species: </b>", point$spp_name, "<br/>")))
    )
  })
  
  output$hover_info1 <- renderUI({
    hover1 <- input$plot_hover1
    point1 <- nearPoints(hoverlogswitch1(), hover1, yvar = yvarhoverswitch(), threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point1) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct1 <- (hover1$x - hover1$domain$left) / (hover1$domain$right - hover1$domain$left)
    top_pct1 <- (hover1$domain$top - hover1$y) / (hover1$domain$top - hover1$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px1 <- hover1$coords_css$x
    top_px1 <- hover1$coords_css$y
    
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px1 + 2, "px; top:", top_px1 + 2, "px;","padding: 5px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Date: </b>", point1$inspdate, "<br/>",
                    "<b> Average: </b>", point1$avg, "<br/>",
                    "<b> Standard Error: </b>", round(point1$sd, 1), "<br/>",
                    "<b> Total: </b>", point1$sum, "<br/>",
                    "<b> Observations: </b>", round(point1$obs, 0), "<br/>",
                    "<b> Species: </b>", point1$spp_name, "<br/>")))
    )
  })
  
  #ALL Tab Backend
  logscaleONE <- reactiveVal(TRUE)
  whichplotONE <- reactiveVal(TRUE)
  
  mosquitoONE <- reactive({
    if(isTRUE(input$facilityONE == "all")) {mosquito0}
    else {
      mosquito0 %>%
        filter(facility %in% input$facilityONE)
    }
  }) 
  mosquitoONEZONE <- reactive({if(input$zoneONE == "All") {mosquitoONE()}
      else {if(input$zoneONE == "2+X")
        {mosquitoONE() %>%
          filter(zone %in% c(2, "X"))}
        else {mosquitoONE() %>%
            filter(zone %in% 1)}
      }
    })
  datafilterONE <- reactive({
    mosquitoONEZONE() %>% 
      filter(between(Year, input$yearsONE[1], input$yearsONE[2]) & spp_name %in% input$speciesONE)
  })
  dataONE <- reactive({
    datafilterONE() %>% 
      group_by(inspdate, spp_name) %>% 
      summarise(avg = round(mean(mosqcount), 1), sd = std.error(mosqcount), sum = sum(mosqcount), Year = Year)
  })
  
  # Looping through each group of year and species
  #for (i in 1:nrow(dfsdtest)) {
  unique_speciesONE <- reactive({unique(dataONE()$spp_name)})
  unique_yearONE <- reactive({unique(dataONE()$Year)})
  new_dataONE = data.frame()
  
  DummyONE <- reactive({
    lapply(unique_yearONE(), function(i) {
      
      new_rowONE <- data.frame(Year = i, spp_name = input$speciesONE, avg = NA, sd = NA, inspdate = as.Date(paste(i, "-05-01", sep = "")))
      new_dataONE <- rbind(new_dataONE, new_rowONE)
    })
  })
  # Combining the original data with the new rows
  RealDataONE <- reactive({
    rbind(dataONE(), DummyONE())
  })
  
  breaksONE <- reactive({
    switch(input$yearsONE[2] - input$yearsONE[1] + 1, "1 week", "1 month", "1 month", "6 months",
           "6 months", "6 months", "6 months", "6 months", "6 months", "6 months", "6 months", "6 months",
           "6 months", "6 months", "6 months", "6 months", "6 months")
  })
  
  observeEvent(input$buttonONE, {
    whichplotONE(!whichplotONE())
  })
  
  observeEvent(input$logbutONE, {
    logscaleONE(!logscaleONE())
  })
  
  whichscaleONE <- reactive({
    if (logscaleONE()) {"identity"} else {scales::pseudo_log_trans(base = 10, sigma = 1)}
  })
  
  hoverdataONE <- reactive({
    dataONE() %>%
      mutate(obs = sum/avg)
  })
  
  logdataONE <- reactive({
    hoverdataONE() %>%
      mutate(logavg = log10(avg))
  })
  
  hoverlogswitchONE <- reactive({
    if (logscaleONE()) {hoverdataONE()} else {logdataONE()}
  })
  
  yvarhoverswitchONE <- reactive({
    if (logscaleONE()) {"avg"} else {"logavg"}
  })
  
  whichchartONE <- reactive({
    if (isTRUE(whichplotONE())) {
      ggplot( RealDataONE(), aes(x=inspdate, y=avg)) + 
        scale_x_date(date_breaks = breaksONE(), date_labels = "%Y/%m/%d") +
        scale_y_continuous(trans = whichscaleONE()) +
        theme(axis.text.x = element_text(size=15, angle = 45, hjust = 1),
              axis.title.x = element_text(size=15, margin=margin(t=20, r=0, b=5, l=0, unit = "pt")),
              axis.text.y = element_text(size=20),
              axis.title.y = element_text(size=15, margin=margin(t=0, r=15, b=0, l=0, unit = "pt")),
              legend.text = element_text(size = 20), legend.title = element_text(size = 25),
              legend.position = "right")+
        xlab("Inspection Date") +
        ylab("Mosquitos (Average/Trap)") +
        #ggtitle("Mosquito Avg per CO2 Trap") +
        geom_line(aes(color = spp_name)) + scale_color_manual(name = "Species", values=c(
          "Total_Ae_+_Cq" = "#000000", Total_Ae_springs = "#008000", Total_Ae_summers = "#ffa500",
          Cq_perturbans_42 = "#800080", Total_Cx_vectors = "#FF0000", Cx_erraticus_32 = "#000000",
          Cx_pipiens_33 = "#0000FF", Cx_restuans_34 = "#008000", Cx_salinarius_35 = "#87cefa",
          Cx_tarsalis_36 = "#a52a2a", Cx_territans_37 = "#00ff7f", "Cx_restuans/pipiens_372" = "#40e0d0",
          Cx_unknown_371 = "#ffa500", An_barberi_27 = "#FFFF00", An_earlei_28 = "#ffc0cb",
          An_punctipennis_29 = "#0000FF", An_quadrimaculatus_30 = "#FF0000", An_walkeri_31 = "#ffa500",
          sp311an_un = "#800080", Total_Anopheles = "#87cefa", sp01_abser = "#FF0000", sp03_aurif = "#FFFF00",
          sp04_euedes = "#f08080", sp05_campest = "#adff2f", sp08_commun = "#483d8b", sp09_diant = "#00FFFF",
          sp118abpun = "#800080", sp11_excru = "#ffa500", sp12_fitch = "#a52a2a", sp13_flave = "#800000",
          sp14_imple = "#7fff00", sp15_intrud = "#ffd700", sp17_pioni = "#FF00FF", sp18_punct = "#0000FF",
          sp19_ripar = "#008000", sp20_spenc = "#ff1493", sp22_stimu = "#708090", sp23_provo = "#ff6347",
          Ae_cinereus_7 = "#006400", Ae_triseriatus_24 = "#0000FF", Ae_vexans_26 = "#FF0000",
          sp02_atrop = "#ff1493", Ae_canadensis_6 = "#000000", Ae_dorsalis_10 = "#808080", sp16_nigro = "#ffd700",
          sp21_stict = "#FF00FF", sp25_trivi = "#800080", sp261ae_unid = "#000000", sp262spr_unid = "#008000",
          sp264summ_unid = "#ffa500", sp50_hende = "#7fff00", Ae_albopictus_51 = "#FF0000",
          Ae_japonicus_52 = "#008000", Ps_ciliata_44 = "#a52a2a", Ps_columbiae_45 = "#008000",
          Ps_ferox_46 = "#000000", sp471ps_un = "#808080", Ps_horrida_47 = "#FF0000", sp38_inorn = "#0000FF",
          Total_Psorophora = "#00FFFF", Culiseta_melanura = "#FF0000", sp40_minne = "#ffa500", sp41_morsi = "#a52a2a",
          sp411cs_un = "#808080", Or_signifera_43 = "#87cefa", Ur_sapphirina_48 = "#00008b", sp49_smith = "#0000FF" 
        )) +
        scale_shape_manual(name = "Species",  values = c(
          "Total_Ae_+_Cq" = 1, Total_Ae_springs = 1, Total_Ae_summers = 1, Cq_perturbans_42 = 1,
          Total_Cx_vectors = 1, Cx_erraticus_32 = 15, Cx_pipiens_33 = 15, Cx_restuans_34 = 15,
          Cx_salinarius_35 = 15, Cx_tarsalis_36 = 15, Cx_territans_37 = 15, "Cx_restuans/pipiens_372" = 15,
          Cx_unknown_371 = 15, An_barberi_27 = 4, An_earlei_28 = 4, An_punctipennis_29 = 4,
          An_quadrimaculatus_30 = 4, An_walkeri_31 = 4, sp311an_un = 4, Total_Anopheles = 4,
          sp01_abser = 19, sp03_aurif = 19, sp04_euedes = 19, sp05_campest = 19, sp08_commun = 19,
          sp09_diant = 19, sp118abpun = 19, sp11_excru = 19, sp12_fitch = 19, sp13_flave = 19,
          sp14_imple = 19, sp15_intrud = 19, sp17_pioni = 19, sp18_punct = 19, sp19_ripar = 19,
          sp20_spenc = 19, sp22_stimu = 19, sp23_provo = 19, Ae_cinereus_7 = 19, Ae_triseriatus_24 = 19,
          Ae_vexans_26 = 19, sp02_atrop = 19, Ae_canadensis_6 = 19, Ae_dorsalis_10 = 19, sp16_nigro = 19,
          sp21_stict = 19, sp25_trivi = 19, sp261ae_unid = 19, sp262spr_unid = 19, sp264summ_unid = 19,
          sp50_hende = 19, Ae_albopictus_51 = 19, Ae_japonicus_52 = 19, Ps_ciliata_44 = 3,
          Ps_columbiae_45 = 3, Ps_ferox_46 = 3, sp471ps_un = 3, Ps_horrida_47 = 3, sp38_inorn = 3,
          Total_Psorophora = 3, Culiseta_melanura = 18, sp40_minne = 18, sp41_morsi = 18, sp411cs_un = 18,
          Or_signifera_43 = 18, Ur_sapphirina_48 = 18, sp49_smith = 18
        )) +
        geom_point(aes(color = spp_name, shape = spp_name), size=3)
      
    } else {
      ggplot( RealDataONE(), aes(x=inspdate, y=avg)) + 
        scale_x_date(date_breaks = breaksONE(), date_labels = "%Y/%m/%d") +
        scale_y_continuous(trans = whichscaleONE()) +
        theme(axis.text.x = element_text(size=15, angle = 45, hjust = 1),
              axis.title.x = element_text(size=15, margin=margin(t=20, r=0, b=5, l=0, unit = "pt")),
              axis.text.y = element_text(size=20),
              axis.title.y = element_text(size=15, margin=margin(t=0, r=15, b=0, l=0, unit = "pt")),
              legend.text = element_text(size = 20), legend.title = element_text(size = 25),
              legend.position = "right")+
        xlab("Inspection Date") +
        ylab("Mosquitos (Average/Trap) Zone 2+X") +
        #ggtitle("Mosquito Avg per CO2 Trap") +
        geom_line(aes(color = spp_name)) + scale_color_manual(name = "Species", values=c(
          "Total_Ae_+_Cq" = "#000000", Total_Ae_springs = "#008000", Total_Ae_summers = "#ffa500",
          Cq_perturbans_42 = "#800080", Total_Cx_vectors = "#FF0000", Cx_erraticus_32 = "#000000",
          Cx_pipiens_33 = "#0000FF", Cx_restuans_34 = "#008000", Cx_salinarius_35 = "#87cefa",
          Cx_tarsalis_36 = "#a52a2a", Cx_territans_37 = "#00ff7f", "Cx_restuans/pipiens_372" = "#40e0d0",
          Cx_unknown_371 = "#ffa500", An_barberi_27 = "#FFFF00", An_earlei_28 = "#ffc0cb",
          An_punctipennis_29 = "#0000FF", An_quadrimaculatus_30 = "#FF0000", An_walkeri_31 = "#ffa500",
          sp311an_un = "#800080", Total_Anopheles = "#87cefa", sp01_abser = "#FF0000", sp03_aurif = "#FFFF00",
          sp04_euedes = "#f08080", sp05_campest = "#adff2f", sp08_commun = "#483d8b", sp09_diant = "#00FFFF",
          sp118abpun = "#800080", sp11_excru = "#ffa500", sp12_fitch = "#a52a2a", sp13_flave = "#800000",
          sp14_imple = "#7fff00", sp15_intrud = "#ffd700", sp17_pioni = "#FF00FF", sp18_punct = "#0000FF",
          sp19_ripar = "#008000", sp20_spenc = "#ff1493", sp22_stimu = "#708090", sp23_provo = "#ff6347",
          Ae_cinereus_7 = "#006400", Ae_triseriatus_24 = "#0000FF", Ae_vexans_26 = "#FF0000",
          sp02_atrop = "#ff1493", Ae_canadensis_6 = "#000000", Ae_dorsalis_10 = "#808080", sp16_nigro = "#ffd700",
          sp21_stict = "#FF00FF", sp25_trivi = "#800080", sp261ae_unid = "#000000", sp262spr_unid = "#008000",
          sp264summ_unid = "#ffa500", sp50_hende = "#7fff00", Ae_albopictus_51 = "#FF0000",
          Ae_japonicus_52 = "#008000", Ps_ciliata_44 = "#a52a2a", Ps_columbiae_45 = "#008000",
          Ps_ferox_46 = "#000000", sp471ps_un = "#808080", Ps_horrida_47 = "#FF0000", sp38_inorn = "#0000FF",
          Total_Psorophora = "#00FFFF", Culiseta_melanura = "#FF0000", sp40_minne = "#ffa500", sp41_morsi = "#a52a2a",
          sp411cs_un = "#808080", Or_signifera_43 = "#87cefa", Ur_sapphirina_48 = "#00008b", sp49_smith = "#0000FF"
        )) +
        scale_shape_manual(name = "Species",  values = c(
          "Total_Ae_+_Cq" = 1, Total_Ae_springs = 1, Total_Ae_summers = 1, Cq_perturbans_42 = 1,
          Total_Cx_vectors = 1, Cx_erraticus_32 = 15, Cx_pipiens_33 = 15, Cx_restuans_34 = 15,
          Cx_salinarius_35 = 15, Cx_tarsalis_36 = 15, Cx_territans_37 = 15, "Cx_restuans/pipiens_372" = 15,
          Cx_unknown_371 = 15, An_barberi_27 = 4, An_earlei_28 = 4, An_punctipennis_29 = 4,
          An_quadrimaculatus_30 = 4, An_walkeri_31 = 4, sp311an_un = 4, Total_Anopheles = 4,
          sp01_abser = 19, sp03_aurif = 19, sp04_euedes = 19, sp05_campest = 19, sp08_commun = 19,
          sp09_diant = 19, sp118abpun = 19, sp11_excru = 19, sp12_fitch = 19, sp13_flave = 19,
          sp14_imple = 19, sp15_intrud = 19, sp17_pioni = 19, sp18_punct = 19, sp19_ripar = 19,
          sp20_spenc = 19, sp22_stimu = 19, sp23_provo = 19, Ae_cinereus_7 = 19, Ae_triseriatus_24 = 19,
          Ae_vexans_26 = 19, sp02_atrop = 19, Ae_canadensis_6 = 19, Ae_dorsalis_10 = 19, sp16_nigro = 19,
          sp21_stict = 19, sp25_trivi = 19, sp261ae_unid = 19, sp262spr_unid = 19, sp264summ_unid = 19,
          sp50_hende = 19, Ae_albopictus_51 = 19, Ae_japonicus_52 = 19, Ps_ciliata_44 = 3,
          Ps_columbiae_45 = 3, Ps_ferox_46 = 3, sp471ps_un = 3, Ps_horrida_47 = 3, sp38_inorn = 3,
          Total_Psorophora = 3, Culiseta_melanura = 18, sp40_minne = 18, sp41_morsi = 18, sp411cs_un = 18,
          Or_signifera_43 = 18, Ur_sapphirina_48 = 18, sp49_smith = 18 
        )) +
        #geom_point(aes(color = species)) +
        geom_pointrange(aes(ymin=avg-sd, ymax=avg+sd, color = spp_name, shape = spp_name))
    }
  })
  
  output$chartONE <- renderPlot({   
    whichchartONE()
  }) 
  
  output$hover_infoONE <- renderUI({
    hoverONE <- input$plot_hoverONE
    pointONE <- nearPoints(hoverlogswitchONE(), hoverONE, yvar = yvarhoverswitchONE(), threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(pointONE) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pctONE <- (hoverONE$x - hoverONE$domain$left) / (hoverONE$domain$right - hoverONE$domain$left)
    top_pctONE <- (hoverONE$domain$top - hoverONE$y) / (hoverONE$domain$top - hoverONE$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_pxONE <- hoverONE$coords_css$x
    top_pxONE <- hoverONE$coords_css$y
    
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_pxONE + 2, "px; top:", top_pxONE + 2, "px;","padding: 5px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Date: </b>", pointONE$inspdate, "<br/>",
                    "<b> Average: </b>", pointONE$avg, "<br/>",
                    "<b> Standard Error: </b>", round(pointONE$sd, 1), "<br/>",
                    "<b> Total: </b>", pointONE$sum, "<br/>",
                    "<b> Observations: </b>", round(pointONE$obs, 0), "<br/>",
                    "<b> Species: </b>", pointONE$spp_name, "<br/>")))
    )
  })
  
}

shinyApp(ui, server)