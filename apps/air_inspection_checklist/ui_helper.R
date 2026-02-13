# Air Inspection Checklist - UI Helper
# Builds the Shiny UI for the air inspection checklist app

#' Create the main UI for the air inspection checklist app
#' @return Shiny UI object
air_inspection_checklist_ui <- function() {
  fluidPage(
    get_universal_text_css(),
    tags$head(
      tags$style(HTML("
        .checklist-section {
          margin-bottom: 20px;
        }
        .checklist-header {
          background-color: #2c5aa0;
          color: white;
          padding: 8px 12px;
          border-radius: 4px 4px 0 0;
          font-size: 16px;
          font-weight: bold;
        }
        .section-subheader {
          background-color: #e8eef5;
          padding: 6px 12px;
          font-weight: bold;
          font-size: 14px;
          border-bottom: 1px solid #ccc;
        }
        .checklist-item {
          padding: 6px 12px;
          border-bottom: 1px solid #eee;
          display: flex;
          align-items: center;
          font-size: 13px;
        }
        .checklist-item:hover {
          background-color: #f8f9fa;
        }
        .item-done {
          color: #28a745;
        }
        .item-not-done {
          color: #dc3545;
        }
        .item-sitecode {
          width: 140px;
          font-weight: bold;
        }
        .item-status-icon {
          width: 30px;
          text-align: center;
          font-size: 16px;
        }
        .item-details {
          flex: 1;
          color: #666;
          font-size: 12px;
        }
        .item-bug-badge {
          display: inline-block;
          padding: 2px 8px;
          border-radius: 10px;
          font-size: 11px;
          font-weight: bold;
          margin-left: 8px;
        }
        .bug-red {
          background-color: #dc3545;
          color: white;
        }
        .bug-blue {
          background-color: #007bff;
          color: white;
        }
        .bug-pending {
          background-color: #ffc107;
          color: #333;
        }
        .bug-none {
          background-color: #6c757d;
          color: white;
        }
        .badge-treatment {
          padding: 2px 8px;
          border-radius: 10px;
          font-size: 11px;
          font-weight: bold;
          margin-left: 8px;
          background-color: #6f42c1;
          color: white;
        }
        .item-active-trt {
          background-color: #f3eefb;
        }
        .summary-box {
          text-align: center;
          padding: 15px;
          border-radius: 8px;
          margin-bottom: 10px;
        }
        .summary-number {
          font-size: 28px;
          font-weight: bold;
        }
        .summary-label {
          font-size: 12px;
          color: #666;
        }
      "))
    ),

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

        checkboxInput("show_active_treatment", "Show Active Treatment Sites",
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
