library(shiny)
library(shinydashboard)

trap_ui <- function() {
  dashboardPage(
    dashboardHeader(title = "Trap Surveillance Test"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Map", tabName = "map", icon = icon("map")),
        menuItem("Table", tabName = "table", icon = icon("table"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "map",
          fluidRow(
            box(width = 4, title = "Controls", status = "primary", solidHeader = TRUE,
              dateInput("analysis_date", "Analysis Date:", value = Sys.Date(), max = Sys.Date()),
              selectizeInput("species", "Species (select one or more):", choices = NULL, multiple = TRUE),
              checkboxGroupInput("trap_types", "Trap Types:", 
                                 choices = c("Elevated CO2" = "4", 
                                            "Gravid Trap" = "5", 
                                            "CO2 Overnight" = "6"),
                                 selected = c("4", "5", "6")),
              numericInput("k_neighbors", "k (nearest neighbors):", value = 4, min = 1, max = 10, step = 1),
              selectizeInput("facility", "Facility (optional):", choices = c("All" = "all"), multiple = TRUE),
              hr(),
              actionButton("refresh", "Refresh Data", icon = icon("refresh"), class = "btn-success", width = "100%")
            ),
            box(width = 8, title = "Sections Vector Index Map", status = "info", solidHeader = TRUE,
                leaflet::leafletOutput("map", height = 700)
            )
          )
        ),
        tabItem(tabName = "table",
          fluidRow(
            box(width = 12, title = "Section Vector Index Table", status = "primary", solidHeader = TRUE,
                dataTableOutput("table")
            )
          )
        )
      )
    )
  )
}
