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
      menuItem("Foremen", tabName = "foremen", icon = icon("users"))
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
        )
      ),
      
      # Foremen tab
      tabItem(tabName = "foremen",
        fluidRow(
          box(
            title = "Foreman Information",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("foremanInfo")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  # Facility Information (with colors)
  output$facilityInfo <- renderDT({
    facilities <- get_facility_lookup()
    if (nrow(facilities) == 0) return(NULL)
    
    colors <- get_facility_base_colors()
    
    df <- data.frame(
      Facility = facilities$short_name,
      City = facilities$full_name,
      Color = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        colors[facilities$short_name]
      ),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Foreman Information (with colors)
  output$foremanInfo <- renderDT({
    foremen <- get_foremen_lookup()
    if (nrow(foremen) == 0) return(NULL)
    
    colors <- get_foreman_colors()
    
    df <- data.frame(
      "Emp Num" = foremen$emp_num,
      "Shortname" = foremen$shortname,
      Facility = foremen$facility,
      Color = unname(colors[foremen$shortname]),
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        colors[foremen$shortname]
      ),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  
  

}

shinyApp(ui = ui, server = server)
