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
      menuItem("Status Colors", tabName = "status_colors", icon = icon("palette")),
      menuItem("Treatment Plan Colors", tabName = "treatment_colors", icon = icon("spray-can")),
      menuItem("Mosquito Species Colors", tabName = "mosquito_colors", icon = icon("bug"))
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
      ),
      
      # Status Colors tab
      tabItem(tabName = "status_colors",
        fluidRow(
          box(
            title = "Status Colors (Hex for Charts)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("statusColors")
          ),
          box(
            title = "Shiny Colors (Named for ValueBoxes)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("shinyColors")
          )
        ),
        fluidRow(
          box(
            title = "Status Color Mapping (for Visualizations)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("statusColorMap")
          )
        )
      ),
      
      # Treatment Plan Colors tab
      tabItem(tabName = "treatment_colors",
        fluidRow(
          box(
            title = "Treatment Plan Colors (by Code)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("treatmentColorsCodes")
          ),
          box(
            title = "Treatment Plan Colors (by Name)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("treatmentColorsNames")
          )
        ),
        fluidRow(
          box(
            title = "Treatment Plan Types Information",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("treatmentPlanTypes")
          )
        )
      ),
      
      # Mosquito Species Colors tab
      tabItem(tabName = "mosquito_colors",
        fluidRow(
          box(
            title = "Mosquito Species Colors",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "600px",
            style = "overflow-y: auto;",
            DTOutput("mosquitoColors")
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
  
  # Status Colors (Hex)
  output$statusColors <- renderDT({
    colors <- get_status_colors()
    descriptions <- get_status_descriptions()
    
    df <- data.frame(
      Status = names(colors),
      "Hex Color" = as.character(colors),
      Description = descriptions[names(colors)],
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        colors
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Shiny Colors (Named)
  output$shinyColors <- renderDT({
    colors <- get_shiny_colors()
    
    df <- data.frame(
      Status = names(colors),
      "Shiny Color" = as.character(colors),
      "Use Case" = "ValueBox elements",
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Status Color Mapping
  output$statusColorMap <- renderDT({
    color_map <- get_status_color_map()
    
    df <- data.frame(
      "Status Name" = names(color_map),
      "Hex Color" = as.character(unlist(color_map)),
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        unlist(color_map)
      ),
      "Use Case" = "Maps, charts, tables",
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Treatment Plan Colors (by Code)
  output$treatmentColorsCodes <- renderDT({
    colors <- get_treatment_plan_colors(use_names = FALSE)
    
    if (length(colors) == 0) {
      df <- data.frame(
        Message = "No treatment plan colors available",
        stringsAsFactors = FALSE
      )
      return(df)
    }
    
    df <- data.frame(
      "Plan Code" = names(colors),
      "Hex Color" = as.character(colors),
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        colors
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Treatment Plan Colors (by Name)
  output$treatmentColorsNames <- renderDT({
    colors <- get_treatment_plan_colors(use_names = TRUE)
    
    if (length(colors) == 0) {
      df <- data.frame(
        Message = "No treatment plan colors available",
        stringsAsFactors = FALSE
      )
      return(df)
    }
    
    df <- data.frame(
      "Plan Name" = names(colors),
      "Hex Color" = as.character(colors),
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        colors
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Treatment Plan Types
  output$treatmentPlanTypes <- renderDT({
    plan_types <- get_treatment_plan_types()
    
    if (nrow(plan_types) == 0) {
      df <- data.frame(
        Message = "No treatment plan types available",
        stringsAsFactors = FALSE
      )
      return(df)
    }
    
    df <- data.frame(
      "Plan Code" = plan_types$plan_code,
      "Plan Name" = plan_types$plan_name,
      Description = plan_types$description,
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Mosquito Species Colors
  output$mosquitoColors <- renderDT({
    colors <- get_mosquito_species_colors()
    
    if (length(colors) == 0) {
      df <- data.frame(
        Message = "No mosquito species colors available",
        stringsAsFactors = FALSE
      )
      return(df)
    }
    
    df <- data.frame(
      "Species Code" = names(colors),
      "Hex Color" = as.character(unlist(colors)),
      Preview = sprintf(
        '<div style="background-color: %s; width: 80px; height: 15px; border: 1px solid #ddd;"></div>',
        unlist(colors)
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 20, scrollY = "400px", scrollCollapse = TRUE))
  
  
  

}

shinyApp(ui = ui, server = server)
