# UI helper functions for Structure Treatment app
# These functions create reusable UI components and improve code organization

#' Create help text for historical metrics
#' @return Shiny div with help text
create_help_text <- function() {
  div(
    style = "margin: 10px; padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; font-size: 16px;",
    h4("Understanding Historical Metrics", style = "margin-top: 0; font-size: 20px;"),
    
    p(style = "margin-bottom: 15px; font-size: 16px;",
      "The historical trends chart shows structure treatment coverage over time. You can view the data in two different ways:"
    ),
    
    hr(style = "margin: 15px 0;"),
    
    h5("Metric Definitions", style = "font-size: 18px;"),
    
    p(style = "margin-bottom: 10px; font-size: 16px;",
      strong("Proportion (%):"), " The percentage of all structures (within your selected filters) that had active treatments during each time period.",
      br(),
      em("Example: If there are 1,000 structures total and 250 had active treatments in a given week, the proportion is 25%.")
    ),
    
    p(style = "margin-bottom: 10px; font-size: 16px;",
      strong("Number of Structures:"), " The absolute count of structures that had active treatments during each time period.",
      br(),
      em("Example: If 250 structures had active treatments in a given week, this metric shows 250.")
    ),
    
    hr(style = "margin: 15px 0;"),
    
    h5("Chart Options", style = "font-size: 18px;"),
    
    p(style = "margin-bottom: 10px; font-size: 16px;",
      strong("Average Lines:"), " You can overlay 5-year or 10-year average lines to compare current performance against historical trends. These averages use all available historical data, regardless of the selected date range."
    ),
    
    p(style = "margin-bottom: 10px; font-size: 16px;",
      strong("Chart Types:"), " Choose from line, area, step, stacked bar, or grouped bar charts to visualize the data in different ways."
    )
  )
}

#' Create the main UI for the structure treatment app
#' @return Shiny UI object
struct_trt_ui <- function() {
  fluidPage(
    # Use universal CSS from db_helpers for consistent text sizing
    get_universal_text_css(),
    
    # Application title
    titlePanel("Structures with Active and Expiring Treatments"),
    
    # Sidebar with controls
    sidebarLayout(
      sidebarPanel(
        # Refresh button at the top
        actionButton("refresh", "Refresh Data", 
          icon = icon("refresh"),
          class = "btn-primary btn-lg",
          style = "width: 100%; margin-bottom: 20px;"),
        
        # Current Progress tab controls
        conditionalPanel(
          condition = "input.tabs == 'current'",
          sliderInput("expiring_days", "Days Until Expiration:",
                      min = 1, max = 30, value = 7, step = 1),
          
          dateInput("custom_today", "Pretend Today is:",
                    value = Sys.Date(), 
                    format = "yyyy-mm-dd"),
          
          checkboxGroupInput("status_types", "Include Structure Status:",
                             choices = c("Dry (D)" = "D",
                                         "Wet (W)" = "W", 
                                         "Unknown (U)" = "U"),
                             selected = c("D", "W", "U"))
        ),
        
        # Historical tab controls
        conditionalPanel(
          condition = "input.tabs == 'historical'",
          radioButtons("hist_time_period", "Time Period:",
                      choices = c("Yearly" = "yearly", "Weekly" = "weekly"),
                      selected = "yearly"),
          
          conditionalPanel(
            condition = "input.hist_time_period == 'yearly' && input.hist_display_metric_yearly == 'proportion'",
            selectInput("hist_chart_type_prop", "Chart Type:",
                        choices = c("Grouped Bar" = "grouped_bar",
                                    "Line Chart" = "line",
                                    "Pie Chart" = "pie"),
                        selected = "grouped_bar")
          ),
          
          conditionalPanel(
            condition = "!(input.hist_time_period == 'yearly' && input.hist_display_metric_yearly == 'proportion')",
            selectInput("hist_chart_type_regular", "Chart Type:",
                        choices = c("Stacked Bar" = "stacked_bar",
                                    "Grouped Bar" = "grouped_bar",
                                    "Line Chart" = "line",
                                    "Area Chart" = "area"),
                        selected = "stacked_bar")
          ),
          
          conditionalPanel(
            condition = "input.hist_time_period == 'yearly'",
            radioButtons("hist_display_metric_yearly", "Display Metric:",
                       choices = c("Total Treatments" = "treatments",
                                  "Unique Structures Treated" = "structures_count",
                                  "Proportion of Structures (%)" = "proportion"),
                       selected = "treatments",
                       inline = TRUE)
          ),
          
          conditionalPanel(
            condition = "input.hist_time_period == 'weekly'",
            radioButtons("hist_display_metric_weekly", "Display Metric:",
                       choices = c("Active Treatments" = "weekly_active_treatments"),
                       selected = "weekly_active_treatments",
                       inline = TRUE)
          ),
          
          sliderInput("hist_year_range", "Year Range:",
                     min = 2010, max = as.numeric(format(Sys.Date(), "%Y")),
                     value = c(2020, as.numeric(format(Sys.Date(), "%Y"))),
                     step = 1, sep = "")
        ),
        
        # Shared controls
        hr(),
        
        selectInput("facility_filter", "Facility:",
                    choices = NULL),
        
        selectizeInput("foreman_filter", "FOS Area:",
                      choices = NULL, multiple = TRUE),
        
        selectInput("group_by", "Group by:",
                    choices = c("Facility" = "facility",
                                "FOS" = "foreman", 
                                "All MMCD" = "mmcd_all"),
                    selected = "facility"),
        
        radioButtons("zone_filter", "Zone Display:",
                     choices = c("P1 Only" = "1", 
                                "P2 Only" = "2", 
                                "P1 and P2 Separate" = "1,2", 
                                "Combined P1+P2" = "combined"),
                     selected = "1,2"),
        
        selectizeInput("structure_type_filter", "Structure Type:",
                    choices = get_structure_type_choices(include_all = TRUE),
                    selected = "all", multiple = TRUE),
        
        selectInput("color_theme", "Color Theme:",
                    choices = c("MMCD" = "MMCD",
                                "IBM Design" = "IBM",
                                "Wong (Color Blind Safe)" = "Wong",
                                "Tol (Color Blind Safe)" = "Tol",
                                "Viridis" = "Viridis",
                                "ColorBrewer Set2" = "ColorBrewer"),
                    selected = "MMCD"),
        
        helpText(tags$b("Structure Status:"),
                 tags$br(),
                 tags$ul(
                   tags$li(tags$b("D:"), "Dry - Structure is dry"),
                   tags$li(tags$b("W:"), "Wet - Structure has water"),
                   tags$li(tags$b("U:"), "Unknown - Status not determined")
                 )),
        
        # Help text for historical metrics (collapsible)
        hr(),
        div(id = "help-section",
          tags$a(href = "#", onclick = "$(this).next().toggle(); return false;", 
                 style = "color: #17a2b8; text-decoration: none; font-size: 15px;",
                 HTML("<i class='fa fa-question-circle'></i> Show/Hide Help")),
          div(style = "display: none;",
            create_help_text()
          )
        )
      ),
      
      # Main panel with tabs
      mainPanel(
        tabsetPanel(id = "tabs",
          tabPanel("Current Progress", value = "current",
                   plotOutput("structureGraph", height = "800px"),
                   br(),
                   fluidRow(
                     column(10, h4("Current Structure Details")),
                     column(2, downloadButton("download_current_data", "Download CSV", 
                                             class = "btn-success btn-sm", 
                                             style = "margin-top: 20px; float: right;"))
                   ),
                   DT::dataTableOutput("currentStructureTable")
          ),
          tabPanel("Historical Trends", value = "historical",
                   br(),
                   plotlyOutput("historicalGraph", height = "600px"),
                   br(),
                   fluidRow(
                     column(10, h4("Historical Treatment Data")),
                     column(2, downloadButton("download_historical_data", "Download CSV", 
                                             class = "btn-success btn-sm", 
                                             style = "margin-top: 20px; float: right;"))
                   ),
                   DT::dataTableOutput("historicalStructureTable")
          )
        )
      )
    )
  )
}
