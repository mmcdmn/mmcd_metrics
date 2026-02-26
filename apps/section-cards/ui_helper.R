# ui_helper.R
# UI helper functions for Section Cards

#' Create field selection panel for section cards
#' 
#' @return Shiny UI element with field selection controls
create_field_selector <- function() {
  tagList(
    h4("Card Configuration"),
    
    wellPanel(
      h5("Site Type"),
      radioButtons(
        "site_type",
        NULL,
        choices = c("Air/Ground Sites" = "breeding", "Structures" = "structures"),
        selected = "breeding",
        inline = TRUE
      ),
      # Data source toggle - only visible for breeding sites
      conditionalPanel(
        condition = "input.site_type == 'breeding'",
        hr(),
        checkboxInput(
          "use_webster",
          "Use Webster data (no custom columns)",
          value = FALSE
        ),
        tags$small(class = "help-block", "Webster uses the original loc_breeding_sites table")
      )
    ),
    
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
      checkboxInput(
        "double_sided",
        "Double-sided printing (pad sections to even pages)",
        value = FALSE
      ),
      
      # Air/Ground specific filters (shown when site_type == "breeding")
      conditionalPanel(
        condition = "input.site_type == 'breeding'",
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
        )
      ),
      
      # Structure specific filters (shown when site_type == "structures")
      conditionalPanel(
        condition = "input.site_type == 'structures'",
        selectInput(
          "filter_structure_type",
          "Structure Type:",
          choices = get_structure_type_choices(include_all = TRUE),
          selected = "all"
        ),
        selectInput(
          "filter_status_udw",
          "Status:",
          choices = c("All" = "all", "Dry" = "D", "Wet" = "W", "Unknown" = "U"),
          selected = "all"
        ),
        checkboxInput(
          "split_by_type",
          "Split by Type (each type on separate page)",
          value = FALSE
        )
      ),
      
      selectInput(
        "filter_priority",
        "Priority:",
        choices = c("All" = "all", "RED" = "RED", "YELLOW" = "YELLOW", "GREEN" = "GREEN", "BLUE" = "BLUE", "ORANGE" = "ORANGE", "PURPLE" = "PURPLE"),
        selected = "all"
      )
    ),
    
    hr(),
    
    # Title fields section - conditionally rendered based on site type
    uiOutput("title_fields_panel"),
    
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
