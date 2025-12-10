# ui_helper.R
# UI helper functions for sections-cards DEMO

#' Create field selection panel for section cards
#' 
#' @return Shiny UI element with field selection controls
create_field_selector <- function() {
  tagList(
    h4("Card Configuration"),
    
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
          "Prehatch" = "prehatch",
          "Section" = "section",
          "Remarks" = "remarks"
        ),
        selected = c("priority", "acres", "section")
      )
    ),
    
    wellPanel(
      h5("Data Table Columns"),
      p(class = "help-block", "Select columns for the data entry table (empty rows for manual entry)"),
      checkboxGroupInput(
        "table_fields",
        NULL,
        choices = list(
          "Date" = "date",
          "Wet %" = "wet_pct",
          "Emp #" = "emp_num",
          "#/Dip" = "num_dip",
          "Sample Number" = "sample_num",
          "Amt" = "amt",
          "Mat" = "mat"
        ),
        selected = c("date", "wet_pct", "emp_num", "num_dip")
      ),
      
      br(),
      
      numericInput(
        "num_rows",
        "Number of empty rows per card:",
        value = 5,
        min = 1,
        max = 20,
        step = 1
      )
    ),
    
    hr(),
    
    wellPanel(
      h5("Filters"),
      selectInput(
        "filter_facility",
        "Facility:",
        choices = c("All" = "all"),
        selected = "all"
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
        "filter_zone",
        "Zone:",
        choices = c("All" = "all", "Zone 1" = "1", "Zone 2" = "2"),
        selected = "all"
      ),
      selectInput(
        "filter_fosarea",
        "FOS Area:",
        choices = c("All" = "all"),
        selected = "all"
      ),
      selectInput(
        "filter_priority",
        "Priority:",
        choices = c("All" = "all", "RED" = "RED", "YELLOW" = "YELLOW", "GREEN" = "GREEN", "BLUE" = "BLUE", "ORANGE" = "ORANGE", "PURPLE" = "PURPLE"),
        selected = "all"
      )
    )
  )
}
