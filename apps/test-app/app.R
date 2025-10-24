library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)

source("../../shared/db_helpers.R")

ui <- dashboardPage(
  dashboardHeader(title = "DB Helpers Info"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Facilities", tabName = "facilities", icon = icon("building")),
      menuItem("Foremen", tabName = "foremen", icon = icon("users")),
      menuItem("Statuses", tabName = "statuses", icon = icon("info-circle")),
      menuItem("Color Preview", tabName = "preview", icon = icon("palette"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Facilities tab
      tabItem(tabName = "facilities",
        fluidRow(
          box(
            title = "Facility Information",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("facilityInfo")
          )
        ),
        fluidRow(
          box(
            title = "Facility Base Colors",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("facilityColors")
          )
        )
      ),
      
      # Foremen tab
      tabItem(tabName = "foremen",
        fluidRow(
          box(
            title = "Foreman Information",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("foremanInfo")
          )
        ),
        fluidRow(
          box(
            title = "Foreman Colors",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("foremanColors")
          )
        )
      ),
      
      # Statuses tab
      tabItem(tabName = "statuses",
        fluidRow(
          box(
            title = "Status Colors",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            DTOutput("statusColors")
          ),
          box(
            title = "Status Descriptions",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            DTOutput("statusDescriptions")
          )
        )
      ),
      
      # Color Preview tab
      tabItem(tabName = "preview",
        fluidRow(
          box(
            title = "Color Visualization",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            uiOutput("colorPreview")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  # Facility Information
  output$facilityInfo <- renderDT({
    con <- get_db_connection()
    if (!is.null(con)) {
      facilities <- dbGetQuery(con, "
        SELECT 
          abbrv,
          city,
          address,
          zip
        FROM gis_facility 
        WHERE abbrv IS NOT NULL 
        ORDER BY abbrv
      ")
      dbDisconnect(con)
      facilities
    }
  }, options = list(pageLength = 10))
  
  # Facility Colors
  output$facilityColors <- renderDT({
    facilities <- get_facility_base_colors()
    df <- data.frame(
      Abbreviation = names(facilities),
      Color = facilities,
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        facilities
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Foreman Information
  output$foremanInfo <- renderDT({
    con <- get_db_connection()
    if (!is.null(con)) {
      foremen <- dbGetQuery(con, "
        SELECT DISTINCT
          foreman,
          facility_id
        FROM gis_employees
        WHERE foreman IS NOT NULL
        ORDER BY facility_id, foreman
      ")
      dbDisconnect(con)
      foremen
    }
  }, options = list(pageLength = 10))
  
  # Foreman Colors
  output$foremanColors <- renderDT({
    foremen <- get_foreman_colors()
    df <- data.frame(
      Foreman = names(foremen),
      Color = foremen,
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        foremen
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Status Colors
  output$statusColors <- renderDT({
    statuses <- get_status_colors()
    df <- data.frame(
      Status = names(statuses),
      Color = statuses,
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        statuses
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Status Descriptions
  output$statusDescriptions <- renderDT({
    descriptions <- get_status_descriptions()
    data.frame(
      Status = names(descriptions),
      Description = descriptions,
      stringsAsFactors = FALSE
    )
  }, options = list(pageLength = 10))
  
  # Color Preview
  output$colorPreview <- renderUI({
    facilities <- get_facility_base_colors()
    foremen <- get_foreman_colors()
    statuses <- get_status_colors()
    
    tagList(
      h3("Facility Colors"),
      div(style = "display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 20px;",
        lapply(seq_along(facilities), function(i) {
          div(
            style = sprintf("
              width: 120px; 
              height: 60px; 
              background-color: %s; 
              display: flex; 
              align-items: center; 
              justify-content: center;
              color: %s;
              border: 1px solid #ddd;
              border-radius: 4px;
              padding: 5px;
              text-align: center;
            ", 
            facilities[i],
            ifelse(sum(col2rgb(facilities[i])) < 382, "white", "black")
            ),
            names(facilities)[i]
          )
        })
      ),
      
      h3("Foreman Colors"),
      div(style = "display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 20px;",
        lapply(seq_along(foremen), function(i) {
          div(
            style = sprintf("
              width: 120px; 
              height: 60px; 
              background-color: %s; 
              display: flex; 
              align-items: center; 
              justify-content: center;
              color: %s;
              border: 1px solid #ddd;
              border-radius: 4px;
              padding: 5px;
              text-align: center;
            ", 
            foremen[i],
            ifelse(sum(col2rgb(foremen[i])) < 382, "white", "black")
            ),
            names(foremen)[i]
          )
        })
      ),
      
      h3("Status Colors"),
      div(style = "display: flex; flex-wrap: wrap; gap: 10px;",
        lapply(seq_along(statuses), function(i) {
          div(
            style = sprintf("
              width: 120px; 
              height: 60px; 
              background-color: %s; 
              display: flex; 
              align-items: center; 
              justify-content: center;
              color: %s;
              border: 1px solid #ddd;
              border-radius: 4px;
              padding: 5px;
              text-align: center;
            ", 
            statuses[i],
            ifelse(sum(col2rgb(statuses[i])) < 382, "white", "black")
            ),
            names(statuses)[i]
          )
        })
      )
    )
  })
}

shinyApp(ui = ui, server = server)
