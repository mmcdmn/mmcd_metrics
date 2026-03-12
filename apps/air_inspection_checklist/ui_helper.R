# Air Inspection Checklist - UI Helper
# Builds the Shiny UI for the air inspection checklist app

#' Create the main UI for the air inspection checklist app
#' @return Shiny UI object
air_inspection_checklist_ui <- function() {
  fluidPage(
    get_universal_text_css(),
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=5"),
      tags$link(rel = "stylesheet", type = "text/css", href = "checklist.css"),
      tags$script(src = "checklist.js")
    ),

    # Mobile sidebar toggle button (hidden on desktop via CSS)
    tags$button(id = "sidebar-toggle", HTML("&#9776; Filters")),
    # Overlay behind mobile drawer
    div(class = "sidebar-overlay"),

    titlePanel("Air Inspection Checklist"),

    sidebarLayout(
      sidebarPanel(
        width = 3,
        actionButton("refresh", "Refresh Data",
                     icon = icon("refresh"),
                     class = "btn-success",
                     style = "width: 100%;"),
        hr(),

        selectInput("facility_filter", "Facility:", choices = NULL),

        selectInput("foreman_filter", "FOS:",
                    choices = NULL),

        selectInput("zone_filter", "Zone Display:",
                    choices = c("P1 Only" = "1",
                                "P2 Only" = "2",
                                "P1 and P2 Combined" = "1,2"),
                    selected = "1"),

        sliderInput("lookback_days", "Lookback Days:",
                    min = 1, max = 7, value = 2, step = 1),

        checkboxInput("show_unfinished_only", "Show Only Unfinished",
                      value = FALSE),

        checkboxInput("show_active_treatment", "Show Prehatch Treatment Sites",
                      value = FALSE),

        hr(),
        dateInput("analysis_date", "Analysis Date:",
                  value = Sys.Date(),
                  max = Sys.Date()),
        
        hr(),
        div(style = "font-size: 12px; color: #888; padding: 5px;",
            p(icon("info-circle"), " RED air sites checked for inspections in the last N days."),
            p(icon("check-circle", class = "item-done"), " = Inspected (with employee # and dip count)"),
            p(icon("times-circle", class = "item-not-done"), " = Not inspected")
        )
      ),

      mainPanel(
        width = 9,
        uiOutput("checklist_display")
      )
    )
  )
}
