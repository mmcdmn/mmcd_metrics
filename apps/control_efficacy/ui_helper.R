# UI Helper Functions for Control Efficacy App
# Reusable UI components for consistent layout

#' Create the main UI for the control efficacy app
#' @return Shiny UI object
control_efficacy_ui <- function() {
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  fluidPage(
    shinyjs::useShinyjs(),
    get_universal_text_css(),
    
    tags$head(
      tags$style(HTML("
        .sidebar { background-color: #f4f4f4; }
        .btn-help { margin-top: 10px; width: 100%; }
        .btn-toggle-filters { width: 100%; margin-bottom: 10px; }
        .collapsible-filters { transition: all 0.3s ease; }
        .sidebar-column { transition: all 0.3s ease; }
        .main-content { transition: all 0.3s ease; }
        .sidebar-hidden .sidebar-column { display: none; }
        .sidebar-hidden .main-content { width: 100% !important; }
        
        /* Floating toggle button when sidebar is hidden */
        .floating-toggle { 
          display: none; 
          position: fixed; 
          top: 80px; 
          left: 20px; 
          z-index: 9999; 
          background: #007bff !important; 
          color: white !important; 
          border: 2px solid #0056b3 !important; 
          border-radius: 6px !important; 
          padding: 10px 15px !important; 
          cursor: pointer !important;
          font-weight: bold !important;
          box-shadow: 0 4px 8px rgba(0,0,0,0.3) !important;
        }
        .floating-toggle:hover {
          background: #0056b3 !important;
        }
        .sidebar-hidden .floating-toggle { display: block !important; }
      "))
    ),
    
    titlePanel("Control Efficacy - Air Treatment Checkbacks"),
    
    # Toggle button OUTSIDE the layout so it's always visible
    div(style = "margin-bottom: 15px;",
      actionButton(
        "toggle_filters", "Hide Filters",
        icon = icon("eye-slash"),
        class = "btn-secondary",
        style = "margin-right: 10px;"
      )
    ),
    
    # Floating toggle button (visible when sidebar is hidden)
    div(
      actionButton(
        "toggle_filters_floating", "Show Filters",
        icon = icon("eye"),
        class = "floating-toggle btn-primary"
      )
    ),
    
    div(id = "main_layout",
      fluidRow(
        
        # ============================
        # Sidebar
        # ============================
        column(
          3, id = "sidebar_col", class = "sidebar-column",
          div(
            class = "sidebar",
            style = "padding: 15px; border-radius: 5px;",
            
            actionButton(
              "refresh_data_progress", "Refresh Data",
              icon = icon("refresh"),
              class = "btn-success",
              style = "width: 100%;"
            ),
            br(),
            
            # Collapsible Filters
            div(
              id = "filter_section",
              class = "collapsible-filters",
              
              # Shared Filters
              sliderInput(
                "year_range", "Year Range:",
                min = 2015, max = current_year,
                value = c(current_year, current_year),
                step = 1, sep = "", ticks = FALSE
              ),
              
              selectInput(
                "facility_filter", "Facility:",
                choices = NULL,
                selected = "all"
              ),
              
              selectInput(
                "color_theme", "Color Theme:",
                choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
                selected = "MMCD"
              ),
              
              # Progress Tab Filters
              conditionalPanel(
                condition = "input.tabs == 'progress'",
                hr(),
                h5("Progress Filters"),
                
                selectInput(
                  "matcode_filter_progress", "Material Code:",
                  choices = NULL,
                  selected = "all"
                ),
                
                radioButtons(
                  "checkback_type_progress", "Checkback Target:",
                  choices = list("Percentage" = "percent", "Fixed Number" = "number"),
                  selected = "percent"
                ),
                
                conditionalPanel(
                  condition = "input.checkback_type_progress == 'percent'",
                  numericInput(
                    "checkback_percent_progress", "Required %:",
                    value = 10, min = 0, max = 100, step = 5
                  )
                ),
                
                conditionalPanel(
                  condition = "input.checkback_type_progress == 'number'",
                  numericInput(
                    "checkback_number_progress", "Required #:",
                    value = 10, min = 1, step = 1
                  )
                )
              ),
              
              # Efficacy Filters
              conditionalPanel(
                condition = "input.tabs == 'efficacy'",
                hr(),
                h5("Efficacy Filters"),
                
                selectInput(
                  "comparison_mode", "Compare By:",
                  choices = c("By Genus" = "genus", "By Material" = "material", "By Dosage" = "dosage"),
                  selected = "genus"
                ),
                
                selectInput(
                  "genus_filter", "Genus:",
                  choices = c("Both" = "Both", "Aedes" = "Aedes", "Culex" = "Culex"),
                  selected = "Both"
                ),
                
                checkboxGroupInput(
                  "season_filter", "Season:",
                  choices = c("Spring" = "Spring", "Summer" = "Summer"),
                  selected = c("Spring", "Summer"),
                  inline = TRUE
                ),
                
                checkboxGroupInput(
                  "trt_type_filter", "Treatment Type:",
                  choices = c("Air" = "Air", "Ground" = "Ground", "Drone" = "Drone"),
                  selected = c("Air", "Ground", "Drone"),
                  inline = TRUE
                ),
                
                selectInput(
                  "material_type_filter", "Material Type:",
                  choices = c(
                    "All Materials" = "all",
                    "Bti Only" = "bti",
                    "Methoprene Only" = "methoprene",
                    "Spinosad Only" = "spinosad"
                  ),
                  selected = "all"
                ),
                
                selectInput(
                  "dosage_filter", "Target Dosage:",
                  choices = c("All Dosages" = "all"),
                  selected = "all"
                ),
                
                checkboxInput("use_mullas", "Use Mulla's formula", value = FALSE)
              )
            ), # end filter_section
            
            hr(),
            actionButton(
              "show_help", "Show Help",
              icon = icon("question-circle"),
              class = "btn-info btn-help"
            )
          ) # end sidebar div
        ), # end sidebar column
        
        # ============================
        # Main Content
        # ============================
        column(
          9, class = "main-content",
          
          tabsetPanel(
            id = "tabs",
            
            # --- Control Efficacy ---
            tabPanel(
              "Control Efficacy", value = "efficacy",
              br(),
              fluidRow(
                column(3, uiOutput("efficacy_valid_count")),
                column(3, uiOutput("efficacy_median_reduction")),
                column(3, uiOutput("efficacy_mean_reduction")),
                column(3, uiOutput("efficacy_pct_above_80"))
              ),
              br(),
              h4("% Reduction by Genus, Season, and Year"),
              plotly::plotlyOutput("reduction_boxplot", height = "600px"),
              br(),
              h4("Efficacy Data Table"),
              DT::dataTableOutput("efficacy_table")
            ),
            
            # --- Checkback Progress ---
            tabPanel(
              "Checkback Progress", value = "progress",
              br(),
              fluidRow(
                column(3, uiOutput("total_checkbacks_needed")),
                column(3, uiOutput("total_checkbacks_completed")),
                column(3, uiOutput("overall_completion_rate")),
                column(3, uiOutput("avg_days_to_checkback"))
              ),
              br(),
              h4("Checkback Progress by Brood"),
              uiOutput("checkback_progress_chart_ui")
            ),
            
            # --- Status Tables ---
            tabPanel(
              "Status Tables", value = "status_tables",
              br(),
              h4("Brood Status Table"),
              DT::dataTableOutput("checkback_status_table"),
              br(),
              h4("Control Checkback Details", style = "color: #856404;"),
              div(style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffeeba; border-radius: 4px; margin-bottom: 15px;",
                tags$i(class = "fa fa-info-circle", style = "color: #856404;"),
                tags$span(style = "font-size: 0.9em;",
                  " Control checkbacks: treatment happened BEFORE the pre-inspection,",
                  " so both pre and post measurements occurred after treatment.",
                  " These show natural population change and are used for",
                  " Mulla's formula correction when enabled in the Efficacy tab."
                )
              ),
              DT::dataTableOutput("control_details")
            )
          ) # end tabsetPanel
        ) # end main content column
      ) # end fluidRow
    ) # end main_layout div
  ) # end fluidPage
}
