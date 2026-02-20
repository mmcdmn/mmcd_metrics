library(shiny)
library(shinydashboard)

trap_surveillance_ui <- function() {
  dashboardPage(
    dashboardHeader(title = "Trap Surveillance"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Map", tabName = "map_tab", icon = icon("map")),
        menuItem("Trend", tabName = "trend_tab", icon = icon("chart-line")),
        menuItem("Table", tabName = "table_tab", icon = icon("table")),
        menuItem("Methods", tabName = "methods_tab", icon = icon("info-circle"))
      )
    ),
    dashboardBody(
      get_universal_text_css(),
      tabItems(
        # =====================================================================
        # MAP TAB
        # =====================================================================
        tabItem(tabName = "map_tab",
          fluidRow(
            box(width = 3, title = "Controls", status = "primary", solidHeader = TRUE,
              selectInput("year", "Year:",
                         choices = NULL,
                         selected = NULL),
              selectInput("yrwk", "Week:",
                         choices = NULL,
                         selected = NULL),
              selectInput("species", "Species:",
                         choices = NULL,
                         selected = NULL),
              hr(),
              selectInput("metric_type", "Map Metric:",
                         choices = c("Abundance (avg/trap)" = "abundance",
                                    "Infection Rate" = "infection",
                                    "Vector Index (N × P)" = "vector_index"),
                         selected = "abundance"),
              conditionalPanel(
                condition = "input.metric_type == 'infection' || input.metric_type == 'vector_index'",
                selectInput("infection_metric", "Infection Method:",
                           choices = c("MLE (Maximum Likelihood)" = "mle",
                                      "MIR (Minimum Infection Rate)" = "mir"),
                           selected = "mle")
              ),
              hr(),
              selectInput("color_theme", "Color Theme:",
                         choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
                         selected = "MMCD"),
              hr(),
              checkboxInput("compare_mode", "Compare Two Weeks", value = FALSE),
              conditionalPanel(
                condition = "input.compare_mode",
                selectInput("yrwk_b", "Compare To Week:",
                           choices = NULL, selected = NULL)
              ),
              hr(),
              actionButton("refresh", "Refresh Data", icon = icon("refresh"), 
                          class = "btn-success", width = "100%")
            ),
            box(width = 9, title = "Vector Index Areas", status = "warning", solidHeader = TRUE,
                leaflet::leafletOutput("map", height = "750px")
            )
          )
        ),
        
        # =====================================================================
        # TREND TAB
        # =====================================================================
        tabItem(tabName = "trend_tab",
          fluidRow(
            box(width = 12, title = "District-Wide MLE Trend", status = "primary", solidHeader = TRUE,
                plotly::plotlyOutput("mle_trend_plot", height = "400px")
            )
          ),
          fluidRow(
            box(width = 12, title = "District-Wide MIR Trend", status = "warning", solidHeader = TRUE,
                plotly::plotlyOutput("mir_trend_plot", height = "400px")
            )
          ),
          fluidRow(
            box(width = 12, title = "Abundance by Area Over Time", status = "info", solidHeader = TRUE,
                plotly::plotlyOutput("abundance_trend_plot", height = "400px")
            )
          ),
          fluidRow(
            box(width = 12, title = "Vector Index by Area Over Time", status = "danger", solidHeader = TRUE,
                plotly::plotlyOutput("vi_trend_plot", height = "400px")
            )
          )
        ),
        
        # =====================================================================
        # TABLE TAB
        # =====================================================================
        tabItem(tabName = "table_tab",
          fluidRow(
            box(width = 12, title = "Area Data Table", status = "primary", solidHeader = TRUE,
                div(style = "margin-bottom: 10px;",
                  downloadButton("download_data", "Download CSV", class = "btn-primary btn-sm")
                ),
                DT::dataTableOutput("data_table")
            )
          )
        ),
        
        # =====================================================================
        # METHODS TAB
        # =====================================================================
        tabItem(tabName = "methods_tab",
          fluidRow(
            box(width = 12, title = "Data Sources & Methods", status = "info", solidHeader = TRUE,
              h4("About This Dashboard"),
              p("This dashboard displays mosquito surveillance metrics aggregated by ",
                strong("Vector Index Area (VI Area)"), 
                " — geographic zones used for surveillance analysis across the MMCD district."),
              hr(),
              
              h5(icon("database"), " Data Sources"),
              tags$ul(
                tags$li(strong("Abundance:"), " dbadult_mon_nt_co2_forvectorabundance — Monday night CO2 trap counts by species, location, and week."),
                tags$li(strong("MLE:"), " dbvirus_mle_yrwk_area / dbvirus_mle_yrwk_area_spp — Pre-calculated Maximum Likelihood Estimates of infection rate."),
                tags$li(strong("MIR:"), " dbvirus_mir_yrwk_area / dbvirus_mir_yrwk_area_spp — Pre-calculated Minimum Infection Rates."),
                tags$li(strong("Geometry:"), " loc_vectorindexareas_sections_a — Section polygons dissolved into 12 VI areas.")
              ),
              hr(),
              
              h5(icon("calculator"), " Metrics"),
              div(style = "background-color:#f5f5f5; padding:12px; border-radius:5px; margin-bottom:10px;",
                h6(strong("Abundance (N)")),
                p("Average mosquitoes per trap per night for the selected week and species."),
                tags$code("N = Total Count / Number of Traps")
              ),
              div(style = "background-color:#f5f5f5; padding:12px; border-radius:5px; margin-bottom:10px;",
                h6(strong("MLE (Maximum Likelihood Estimate)")),
                p("Statistical estimate of the probability a single mosquito is infected, calculated from pooled testing data using bias-reduced methods."),
                p("Pre-calculated in the database using the Firth method (bias-reduced MLE).")
              ),
              div(style = "background-color:#f5f5f5; padding:12px; border-radius:5px; margin-bottom:10px;",
                h6(strong("MIR (Minimum Infection Rate)")),
                p("Simple proportion: (positive pools / total mosquitoes tested) × 1000"),
                tags$code("MIR = (Positives / Total Mosquitoes) × 1000")
              ),
              div(style = "background-color:#fff3cd; padding:12px; border-radius:5px; border-left:4px solid #ffc107;",
                h6(strong("Vector Index (VI = N × P)")),
                p("Combined measure of mosquito abundance and infection risk."),
                tags$code("VI = N × P"),
                p("Where N = average mosquitoes/trap, P = infection rate (MLE or MIR/1000)")
              ),
              hr(),
              
              h5(icon("map"), " Species"),
              tags$ul(
                tags$li(strong("Total Cx Vectors:"), " Sum of all Culex species — uses area-level MLE (all species pooled together)."),
                tags$li(strong("Cx. pipiens (33):"), " Primary WNV vector in urban areas."),
                tags$li(strong("Cx. restuans (34):"), " Early-season WNV vector."),
                tags$li(strong("Cx. tarsalis (36):"), " Efficient WNV vector, more rural."),
                tags$li(strong("Cx. restuans/pipiens (372):"), " Unresolved Culex identification.")
              ),
              hr(),
              
              h5(icon("clock"), " Temporal Resolution"),
              p("Data is organized by ", strong("epiweek (yrwk)"), " — epidemiological weeks. ",
                "Each yrwk value encodes year and week number (e.g., 202537 = year 2025, week 37).")
            )
          )
        )
      )
    )
  )
}
