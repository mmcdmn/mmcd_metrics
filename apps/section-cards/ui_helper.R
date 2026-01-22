# ui_helper.R
# UI helper functions for sections-cards DEMO

#' Create field selection panel for section cards
#' 
#' @return Shiny UI element with field selection controls
create_field_selector <- function() {
  tagList(
    h4("Card Configuration"),
    
    wellPanel(
      h5("Filters"),
      selectInput(
        "filter_facility",
        "Facility:",
        choices = c("All Facilities" = "all"),
        selected = "all"
      ),
      selectInput(
        "filter_zone",
        "Zone:",
        choices = c("All" = "all", "Zone 1" = "1", "Zone 2" = "2"),
        selected = "all"
      ),
      selectInput(
        "filter_fosarea",
        "FOS Area:",
        choices = c("All FOS Areas" = "all"),
        selected = "all"
      ),
      selectInput(
        "filter_towncode",
        "Town Code (first 4 digits):",
        choices = c("All Town Codes" = "all"),
        selected = "all"
      ),
      selectInput(
        "filter_section",
        "Section:",
        choices = c("All Sections" = "all"),
        selected = "all"
      ),
      checkboxInput(
        "split_by_section",
        "Split by section (each section on separate pages)",
        value = FALSE
      ),
      checkboxInput(
        "split_by_priority",
        "Split by priority (each priority on separate pages)",
        value = FALSE
      ),
      selectInput(
        "filter_air_gnd",
        "Air/Ground:",
        choices = c("All" = "all", "Air" = "A", "Ground" = "G"),
        selected = "all"
      ),
      selectInput(
        "filter_drone",
        "Drone Sites:",
        choices = c("All" = "all", "Include Drone" = "include", "Exclude Drone" = "exclude", "Drone Only" = "only"),
        selected = "all"
      ),
      selectInput(
        "filter_priority",
        "Priority:",
        choices = c("All" = "all", "RED" = "RED", "YELLOW" = "YELLOW", "GREEN" = "GREEN", "BLUE" = "BLUE", "ORANGE" = "ORANGE", "PURPLE" = "PURPLE"),
        selected = "all"
      )
    ),
    
    hr(),
    
    wellPanel(
      h5("Title Section Fields"),
      p(class = "help-block", "Select fields to display in the card header (sitecode is always included)"),
      checkboxGroupInput(
        "title_fields",
        NULL,
        choices = list(
          "Priority" = "priority",
          "Acres" = "acres",
          "Type" = "type",
          "Culex" = "culex",
          "Spring Aedes" = "spr_aedes",
          "Perturbans" = "perturbans",
          "Prehatch" = "prehatch",
          "Prehatch Calculation" = "prehatch_calc",
          "Sample Site" = "sample",
          "Section" = "section",
          "Remarks" = "remarks"
        ),
        selected = c("priority", "acres", "type", "remarks")
      )
    ),
    
    wellPanel(
      h5("Data Table Columns"),
      p(class = "help-block", "Select columns and reorder them using the buttons"),
      uiOutput("column_order_ui"),
      
      br(),
      
      numericInput(
        "num_rows",
        "Number of empty rows per card:",
        value = 5,
        min = 1,
        max = 20,
        step = 1
      )
    )
  )
}
