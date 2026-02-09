# UI helper for Air Sites Simple app

#' Create the main UI for the air sites simple app
#' @return Shiny UI object
air_sites_simple_ui <- function() {
  fluidPage(
    get_universal_text_css(),
    titlePanel("Air Work Pipeline - Simple"),

    sidebarLayout(
      sidebarPanel(
        # Main refresh button (Status + Pipeline tabs)
        actionButton("refresh", "Refresh Data",
                     icon = icon("refresh"),
                     class = "btn-success",
                     style = "width: 100%;"),
        p(style = "text-align: center; margin-top: 5px; font-size: 0.9em; color: #666;",
          "Refreshes Status and Pipeline tabs"),
        hr(),

        # === Tab-specific controls ===

        # Status tab: single status filter dropdown
        conditionalPanel(
          condition = "input.main_tabset == 'Air Site Status'",
          h4("Status Filters"),
          selectInput("status_filter", "Status Filter:",
                      choices = c("All Statuses" = "all",
                                  "Not Insp" = "Unknown",
                                  "Insp - Under Threshold" = "Inspected",
                                  "Needs ID" = "Needs ID",
                                  "Needs Treatment" = "Needs Treatment",
                                  "Active Treatment" = "Active Treatment"),
                      selected = "all")
        ),

        # Process tab: checkbox status filter
        conditionalPanel(
          condition = "input.main_tabset == 'Pipeline Snapshot'",
          h4("Pipeline Filters"),
          checkboxGroupInput("process_status_filter", "Status Filter (chart only):",
                             choices = c("Not Insp" = "Unknown",
                                         "Insp - Under Threshold" = "Inspected",
                                         "Needs ID" = "Needs ID",
                                         "Needs Treatment" = "Needs Treatment",
                                         "Active Treatment" = "Active Treatment"),
                             selected = c("Unknown", "Inspected", "Needs ID",
                                          "Needs Treatment", "Active Treatment"))
        ),

        # Historical tab: separate refresh + date range + chart controls
        conditionalPanel(
          condition = "input.main_tabset == 'Historical Analysis'",
          h4("Historical Filters"),
          actionButton("hist_refresh", "Refresh Historical Data",
                       icon = icon("refresh"),
                       class = "btn-info",
                       style = "width: 100%;"),
          p(style = "text-align: center; margin-top: 5px; font-size: 0.9em; color: #666;",
            "Loads historical trends separately"),
          hr(),
          dateInput("hist_start_date", "Start Date:",
                    value = Sys.Date() - (365 * 4),
                    max = Sys.Date()),
          dateInput("hist_end_date", "End Date:",
                    value = Sys.Date(),
                    max = Sys.Date()),
          selectInput("hist_chart_type", "Chart Type:",
                      choices = list("Line Chart" = "line",
                                     "Grouped Bar Chart" = "bar",
                                     "Stacked Bar Chart" = "stacked"),
                      selected = "line"),
          radioButtons("volume_time_period", "Volume Time Period:",
                       choices = c("Weekly" = "weekly", "Yearly" = "yearly"),
                       selected = "yearly",
                       inline = TRUE)
        ),

        # === Shared controls (visible on all tabs) ===
        hr(),
        dateInput("analysis_date", "Analysis Date:",
                  value = Sys.Date(),
                  max = Sys.Date()),

        selectInput("color_theme", "Color Theme:",
                    choices = c("MMCD (Default)" = "MMCD",
                                "IBM Design" = "IBM",
                                "Color-Blind Friendly" = "Wong",
                                "Scientific" = "Tol",
                                "Viridis" = "Viridis",
                                "ColorBrewer" = "ColorBrewer"),
                    selected = "MMCD"),
        tags$small(style = "color: #666;", "Changes map and chart colors"),

        selectInput("facility_filter", "Facility:", choices = NULL),

        selectizeInput("priority_filter", "Priorities:",
                       choices = NULL, multiple = TRUE),

        selectInput("zone_filter", "Zones:", choices = NULL),

        numericInput("larvae_threshold", "Larvae Threshold:",
                     value = 2, min = 0, max = 10, step = 1),

        radioButtons("metric_type", "Display Metric:",
                     choices = c("Number of Sites" = "sites", "Acres" = "acres"),
                     selected = "sites",
                     inline = TRUE),

        selectizeInput("material_filter", "Treatment Materials:",
                       choices = NULL, multiple = TRUE),

        div(style = "margin-top: -10px; margin-bottom: 10px;",
            actionButton("btn_prehatch", "Prehatch Only",
                         class = "btn-sm btn-primary",
                         style = "margin-right: 5px;"),
            actionButton("btn_bti", "BTI Only",
                         class = "btn-sm btn-success")
        ),

        numericInput("bti_effect_days_override", "BTI Effect Days Override:",
                     value = NA, min = 1, max = 60, step = 1),
        tags$small(class = "text-muted", "Leave empty to use default from database"),

        hr(),
        checkboxInput("load_air_site_polygons", 
                      tags$span(icon("map"), "Load Air Site Polygons"),
                      value = FALSE),
        tags$small(class = "text-muted", "Shows air site boundary polygons on the map (zoom in to see)"),

        # Help section
        hr(),
        div(id = "help-section",
            tags$a(href = "#", onclick = "$(this).next().toggle(); return false;",
                   style = "color: #17a2b8; text-decoration: none; font-size: 14px;",
                   HTML("<i class='fa fa-question-circle'></i> Show/Hide Help")),
            div(style = "display: none;",
                create_help_text()
            )
        ),

        width = 3
      ),

      # =====================================================================
      # MAIN PANEL WITH TABS
      # =====================================================================
      mainPanel(
        tabsetPanel(id = "main_tabset",

          # =================================================================
          # TAB 1: Air Site Status
          # =================================================================
          tabPanel("Air Site Status",
            br(),
            div(style = "padding: 10px; text-align: center; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; margin-bottom: 15px;",
                tags$i(class = "fa fa-info-circle", style = "color: #17a2b8;"),
                tags$small(" Note: 'Needs Inspection' status is currently under review. This dashboard shows the process from inspection to treatment.")
            ),
            fluidRow(
              column(2, uiOutput("total_air_sites")),
              column(2, uiOutput("sites_unknown")),
              column(2, uiOutput("sites_inspected")),
              column(2, uiOutput("sites_in_lab")),
              column(2, uiOutput("sites_needs_treatment")),
              column(2, uiOutput("sites_active_treatment"))
            ),
            fluidRow(
              column(8,
                wellPanel(
                  h4("Air Site Status Map", style = "margin-top: 0;"),
                  leafletOutput("status_map", height = "500px")
                )
              ),
              column(4,
                wellPanel(
                  h4("Status Summary", style = "margin-top: 0;"),
                  plotlyOutput("status_chart", height = "500px")
                )
              )
            ),
            fluidRow(
              column(12,
                wellPanel(
                  tags$details(
                    tags$summary(tags$strong("Status Definitions"), style = "cursor: pointer;"),
                    tags$div(
                      tags$h5("Air Site Status Definitions:"),
                      tags$ul(
                        tags$li(tags$strong("Not Insp:"), " Sites that have not been inspected or have no recent inspection data"),
                        tags$li(tags$strong("Insp - Under Threshold:"), " Sites inspected with larvae count below threshold (no treatment needed)"),
                        tags$li(tags$strong("Needs ID:"), " Sites with Dip >= threshold, samples sent to lab for red/blue bug identification"),
                        tags$li(tags$strong("Needs Treatment:"), " Sites with red bugs found in lab analysis (require treatment)"),
                        tags$li(tags$strong("Active Treatment:"), " Sites who received treatment < effect_days ago according to material type")
                      ),
                      tags$h5("Treatment Logic:"),
                      tags$ul(
                        tags$li("Sites with Dip >= threshold -> lab for species identification"),
                        tags$li("Red bugs found -> treatment required (species that bite humans)"),
                        tags$li("Blue bugs found -> no treatment needed (species that don't bite humans)"),
                        tags$li("Active treatments last for BTI effect days (default varies by material)")
                      )
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(12,
                wellPanel(
                  h4("Site Details", style = "margin-top: 0;"),
                  div(style = "text-align: right; margin-bottom: 10px;",
                      downloadButton("download_site_details", "Download CSV", class = "btn-success")
                  ),
                  DT::dataTableOutput("site_details_table")
                )
              )
            )
          ),

          # =================================================================
          # TAB 2: Pipeline Snapshot
          # =================================================================
          tabPanel("Pipeline Snapshot",
            br(),
            div(style = "padding: 10px; text-align: center; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; margin-bottom: 15px;",
                tags$i(class = "fa fa-info-circle", style = "color: #17a2b8;"),
                tags$small(" Status filter only affects the flow chart visualization.")
            ),
            fluidRow(
              column(3, uiOutput("sites_receiving_treatment")),
              column(3, uiOutput("treatment_rate")),
              column(3, uiOutput("treatment_efficiency")),
              column(3, uiOutput("inspection_coverage"))
            ),
            fluidRow(
              column(6, uiOutput("total_inspected_samples")),
              column(6, uiOutput("red_bug_detection_rate"))
            ),
            fluidRow(
              column(12,
                wellPanel(
                  tags$details(
                    tags$summary(tags$strong("Calculation Notes"), style = "cursor: pointer;"),
                    tags$div(
                      tags$h5("Treatment Process Metrics:"),
                      tags$ul(
                        tags$li(tags$strong("Treatment Rate:"), " Active Treatments / (Needs Treatment + Active Treatment) x 100%"),
                        tags$li(tags$strong("Treatment Completion:"), " Active Treatments / (Needs Treatment + Active Treatment) x 100%"),
                        tags$li(tags$strong("Inspection Coverage:"), " (Insp + In Lab + Needs Treatment + Active Treatment) / Total Sites x 100%")
                      ),
                      tags$h5("Lab Processing Metrics:"),
                      tags$ul(
                        tags$li(tags$strong("Total Insp Samples:"), " Number of inspected sites with completed lab results"),
                        tags$li(tags$strong("Red Bug Detection Rate:"), " (Red Bugs Found / Total Samples) x 100%")
                      ),
                      tags$p(tags$strong("Note:"), " Total samples = only completed lab samples with timestamps. Pending samples are excluded.")
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(8,
                wellPanel(
                  h4("Treatment Flow by Facility", style = "margin-top: 0;"),
                  plotlyOutput("treatment_flow_chart", height = "400px")
                )
              ),
              column(4,
                wellPanel(
                  h4("Process Summary", style = "margin-top: 0;"),
                  div(style = "text-align: right; margin-bottom: 10px;",
                      downloadButton("download_process_summary", "Download CSV", class = "btn-info")
                  ),
                  DT::dataTableOutput("process_summary_table")
                )
              )
            ),
            fluidRow(
              column(12,
                wellPanel(
                  h4("Facility Treatment Process Details", style = "margin-top: 0;"),
                  div(style = "text-align: right; margin-bottom: 10px;",
                      downloadButton("download_facility_process", "Download CSV", class = "btn-success")
                  ),
                  DT::dataTableOutput("facility_process_table")
                )
              )
            )
          ),

          # =================================================================
          # TAB 3: Historical Analysis
          # =================================================================
          tabPanel("Historical Analysis",
            br(),
            div(style = "padding: 10px; text-align: center; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; margin-bottom: 15px;",
                tags$i(class = "fa fa-info-circle", style = "color: #17a2b8;"),
                tags$small(" This analysis shows inspection history and treatment volumes over the selected date range.")
            ),
            fluidRow(
              column(3, uiOutput("hist_total_sites")),
              column(3, uiOutput("hist_total_inspections")),
              column(3, uiOutput("hist_total_red_bug_inspections")),
              column(3, uiOutput("hist_overall_red_bug_ratio"))
            ),
            fluidRow(
              column(4, uiOutput("hist_total_treatments")),
              column(4, uiOutput("hist_total_treatment_acres")),
              column(4, uiOutput("hist_total_inspection_acres"))
            ),
            fluidRow(
              column(12,
                wellPanel(
                  tags$details(
                    tags$summary(tags$strong("Calculation Notes"), style = "cursor: pointer;"),
                    tags$div(
                      tags$h5("What is Counted:"),
                      tags$ul(
                        tags$li(tags$strong("Inspections:"), " Action code 4 (AIR inspection) from dblarv_insptrt tables"),
                        tags$li(tags$strong("Treatments:"), " Action codes 3 (Ground), A (AIR), D (Drone) from dblarv_insptrt tables"),
                        tags$li(tags$strong("Red Bug Inspections:"), " Inspections where lab samples show has_red_bugs = TRUE")
                      ),
                      tags$h5("Metric Calculations:"),
                      tags$ul(
                        tags$li(tags$strong("Total Inspections:"), " Count of all AIR inspection records within date range"),
                        tags$li(tags$strong("Total Treatments:"), " Count of all treatment records within date range"),
                        tags$li(tags$strong("Red Bug Ratio:"), " (Red Bug Inspections / Total Inspections) x 100%"),
                        tags$li(tags$strong("Treatment/Inspection Acres:"), " Sum of site acres for respective records")
                      ),
                      tags$p(tags$strong("Note:"), " Only includes air sites (air_gnd = 'A') that were active during the operation date.")
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(8,
                wellPanel(
                  h4("Treatment Volume Trends", style = "margin-top: 0;"),
                  plotlyOutput("treatment_volume_chart", height = "400px")
                )
              ),
              column(4,
                wellPanel(
                  h4("Red Bug Detection Over Time", style = "margin-top: 0;"),
                  plotlyOutput("red_bug_trend_chart", height = "400px")
                )
              )
            ),
            fluidRow(
              column(6,
                wellPanel(
                  h4("Site Inspection History", style = "margin-top: 0;"),
                  div(style = "text-align: center; padding-bottom: 10px;",
                      tags$small("Red Bug Ratio = (Red Bug Inspections / Total Inspections) x 100%")
                  ),
                  div(style = "text-align: right; margin-bottom: 10px;",
                      downloadButton("download_inspection_history", "Download CSV", class = "btn-success")
                  ),
                  DT::dataTableOutput("historical_inspection_table")
                )
              ),
              column(6,
                wellPanel(
                  h4("Treatment Volume Summary", style = "margin-top: 0;"),
                  div(style = "text-align: center; padding-bottom: 10px;",
                      tags$small("Weekly and yearly treatment volumes with acres treated")
                  ),
                  div(style = "text-align: right; margin-bottom: 10px;",
                      downloadButton("download_treatment_volume", "Download CSV", class = "btn-warning")
                  ),
                  DT::dataTableOutput("treatment_volume_table")
                )
              )
            )
          )
        )
      )
    )
  )
}

# Help text for the air sites app
create_help_text <- function() {
  div(
    style = "margin: 10px; padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; font-size: 16px;",
    h4("Understanding Air Site Status", style = "margin-top: 0; font-size: 20px;"),
    tags$ul(style = "font-size: 16px;",
      tags$li(tags$strong("Not Insp:"), " No inspection in the last 7 days"),
      tags$li(tags$strong("Insp - Under Threshold:"), " Inspected, larvae below threshold"),
      tags$li(tags$strong("Needs ID:"), " High larvae, sample sent to lab, awaiting results"),
      tags$li(tags$strong("Needs Treatment:"), " Red bugs confirmed by lab"),
      tags$li(tags$strong("Active Treatment:"), " Treatment applied within effect period")
    ),
    hr(style = "margin: 15px 0;"),
    h5("Filter Options", style = "font-size: 18px;"),
    tags$ul(style = "font-size: 16px;",
      tags$li(tags$strong("Analysis Date:"), " Set a custom date to view historical status snapshots"),
      tags$li(tags$strong("Larvae Threshold:"), " Minimum dip count that triggers lab sampling"),
      tags$li(tags$strong("BTI Effect Days:"), " Override the default treatment duration for BTI materials"),
      tags$li(tags$strong("Prehatch/BTI buttons:"), " Quick-select treatment material categories"),
      tags$li(tags$strong("Priority Filter:"), " Filter sites by priority level (applies to Status and Historical tabs)"),
      tags$li(tags$strong("Material Filter:"), " Filter active treatments by specific treatment materials")
    )
  )
}
