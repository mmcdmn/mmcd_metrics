library(shiny)
library(shinydashboard)

trap_ui <- function() {
  dashboardPage(
    dashboardHeader(title = "Trap Surveillance Test"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Polygon Map", tabName = "map_sf", icon = icon("chart-area")),
        menuItem("Table", tabName = "table", icon = icon("table"))
      )
    ),
    dashboardBody(
      # Use universal CSS from db_helpers for consistent text sizing
      get_universal_text_css(),
      tabItems(
        tabItem(tabName = "map_sf",
          fluidRow(
            box(width = 4, title = "Controls", status = "primary", solidHeader = TRUE,
              selectInput("metric_type", "Metric:", 
                         choices = c("Population Index" = "popindex", 
                                   "MLE (Infection Rate)" = "mle",
                                   "Vector Index (N × P)" = "vector_index"),
                         selected = "popindex"),
              dateInput("analysis_date_sf", "Analysis Date:", value = Sys.Date(), max = Sys.Date()),
              selectizeInput("species_sf", "Species (select one or more):", choices = NULL, multiple = TRUE),
              div(style="margin-top: -10px; margin-bottom: 10px;",
                h6("Quick Select by Genus:", style="margin-bottom: 5px; font-weight: bold;"),
                actionButton("select_aedes", "Aedes", class = "btn-sm", style="margin-right: 5px;"),
                actionButton("select_culex", "Culex", class = "btn-sm", style="margin-right: 5px;"),
                actionButton("select_anopheles", "Anopheles", class = "btn-sm", style="margin-right: 5px;"),
                actionButton("select_all_species", "All", class = "btn-sm")
              ),
              checkboxGroupInput("trap_types_sf", "Trap Types:", 
                                 choices = c("Elevated CO2" = "4", 
                                            "Gravid Trap" = "5", 
                                            "CO2 Overnight" = "6"),
                                 selected = c("4", "5", "6")),
              conditionalPanel(
                condition = "input.metric_type == 'mle' || input.metric_type == 'vector_index'",
                selectInput("mle_method", "MLE Method:", 
                           choices = c("Firth (bias-reduced)" = "firth",
                                     "Gart (bias-corrected)" = "gart", 
                                     "MLE (standard)" = "mle",
                                     "MIR (minimum infection rate)" = "mir"),
                           selected = "firth"),
                numericInput("mle_scale", "Scale (per X mosquitoes):", value = 1000, min = 1, step = 100),
                selectInput("virus_target", "Virus Target:",
                           choices = get_virus_target_choices(),
                           selected = "WNV"),
                sliderInput("lookback_days", "Historical Data (days):",
                           min = 30, max = 180, value = 90, step = 10,
                           post = " days"),
                helpText("Select which virus to test for in surveillance data"),
                selectInput("trap_grouping", "Trap Grouping:", 
                           choices = c("By Location (recommended)" = "location",
                                     "By Individual Trap" = "individual"),
                           selected = "location"),
                helpText("By Location: Groups traps at same coordinates (prevents duplicate distance issue)")
              ),
              numericInput("k_neighbors_sf", "k (nearest neighbors):", value = 4, min = 1, max = 10, step = 1),
              hr(),
              selectInput("color_theme", "Color Theme:",
                         choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
                         selected = "MMCD"),
              hr(),
              actionButton("refresh_sf", "Refresh Data", icon = icon("refresh"), class = "btn-success", width = "100%")
            ),
            box(width = 8, title = "Silent Cartographer", status = "warning", solidHeader = TRUE,
                leaflet::leafletOutput("map_sf", height = "800px")
            )
          ),
          fluidRow(
            box(width = 12, title = "Calculation Methods", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              h4("How Metrics Are Calculated"),
              hr(),
              
              h5(icon("calculator"), " Population Index (N)"),
              p(strong("Purpose:"), "Average mosquito density per trap, weighted by proximity."),
              p(strong("Method:"), "k-Nearest Neighbor (k-NN) with Inverse Distance Weighting (IDW)"),
              div(style="background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
                "N = Σ(w", tags$sub("i"), " × count", tags$sub("i"), ") / Σ(w", tags$sub("i"), ")",
                br(),
                "where w", tags$sub("i"), " = 1 / distance", tags$sub("i"), " (inverse distance weight)"
              ),
              p(style="margin-top:10px;", 
                strong("Example:"), "For a section with 4 nearest traps at distances 7642m, 8216m, 8373m, 9626m",
                "with counts 1272, 1242, 35, 1575:",
                br(),
                "Weights = 1/7642, 1/8216, 1/8373, 1/9626 = 0.000131, 0.000122, 0.000119, 0.000104",
                br(),
                "N = (0.000131×1272 + 0.000122×1242 + 0.000119×35 + 0.000104×1575) / 0.000476 = 1020.02 mosquitoes/trap"
              ),
              hr(),
              
              h5(icon("virus"), " MLE (Maximum Likelihood Estimate)"),
              p(strong("Purpose:"), "Statistical estimate of infection rate from pooled mosquito samples."),
              p(strong("Method:"), "Two-stage trap-based calculation with k-NN spatial averaging"),
              div(style="background-color:#f5f5f5; padding:10px; border-radius:5px; margin-top:10px;",
                strong("Stage 1: Per-Trap MLE"),
                br(),
                "• Query pool tests matching filters (date, virus, species, trap types)",
                br(),
                "• Group by trap ID (sampnum_yr)",
                br(),
                "• Calculate MLE per trap using PooledInfRate::pooledBin()",
                br(),
                br(),
                strong("Stage 2: Section k-NN Averaging"),
                br(),
                "• Find k nearest trap locations to each section centroid",
                br(),
                "• Apply inverse distance squared weighting: w", tags$sub("i"), " = 1 / distance", tags$sub("i"), tags$sup("2"),
                br(),
                "• Calculate weighted average: Section MLE = Σ(trap_MLE × weight)"
              ),
              div(style="background-color:#fff3cd; padding:10px; border-radius:5px; margin-top:10px; border-left: 4px solid #ffc107;",
                strong("Input variables per trap:"),
                br(),
                "x = [0, 0, 1, 0] ", tags$em("(test results: 0=negative, 1=positive)"),
                br(),
                "m = [50, 48, 45, 52] ", tags$em("(mosquitoes per pool)"),
                br(),
                br(),
                strong("Example:"),
                br(),
                "Pool 1: 50 mosquitoes, NEGATIVE (x=0)",
                br(),
                "Pool 2: 48 mosquitoes, NEGATIVE (x=0)",
                br(),
                "Pool 3: 45 mosquitoes, POSITIVE (x=1)",
                br(),
                "Pool 4: 52 mosquitoes, NEGATIVE (x=0)",
                br(),
                br(),
                "→ Trap MLE + 95% CI calculated via PooledInfRate"
              ),
              p(style="margin-top:10px;",
                strong("Methods Available:"),
                tags$ul(
                  tags$li(strong("Firth:"), "Bias-reduced MLE (default) - best for small samples"),
                  tags$li(strong("Gart:"), "Score-based estimator - classic pooled testing method"),
                  tags$li(strong("MLE:"), "Standard maximum likelihood - large samples only"),
                  tags$li(strong("MIR:"), "Minimum Infection Rate - simple proportion (positives/total)")
                )
              ),
              p(style="margin-top:10px;",
                tags$em("Package: PooledInfRate (CDC GitHub: CDCgov/PooledInfRate)")
              ),
              hr(),
              
              h5(icon("chart-line"), " Vector Index (VI = N × P)"),
              p(strong("Purpose:"), "Combined metric of mosquito abundance and infection risk."),
              p(strong("Method:"), "Multiply Population Index by infection rate proportion"),
              div(style="background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
                "VI = N × P",
                br(),
                "where:",
                br(),
                "  N = Population Index (avg mosquitoes/trap)",
                br(),
                "  P = MLE / 1000 (infection rate as proportion)"
              ),
              p(style="margin-top:10px;",
                strong("Example:"), "Section 700101W:",
                br(),
                "N = 9584.15 mosquitoes/trap",
                br(),
                "MLE = 12.16 per 1000",
                br(),
                "P = 12.16 / 1000 = 0.01216",
                br(),
                "VI = 9584.15 × 0.01216 = ", strong("116.51")
              ),
              p(style="margin-top:10px;",
                em("Reference: Dallas County Mosquito Surveillance 2013 WNV Guide")
              ),
              hr(),
              
              h5(icon("info-circle"), " Additional Information"),
              p(strong("k parameter:"), "Number of nearest neighbors to consider (default: 4)"),
              p(strong("Distance calculation:"), "Geodesic distance (great circle / haversine formula) in meters"),
              p(strong("Weight function (MLE):"), "1 / distance² (inverse distance squared)"),
              p(strong("Weight function (Population):"), "1 / distance (inverse distance)"),
              p(strong("Coordinate system:"), "WGS84 (EPSG:4326) for display"),
              p(strong("Projection:"), "Web Mercator (EPSG:3857) for distance calculations"),
              p(strong("Spatial library:"), "sf package for R with GDAL/GEOS backends"),
              p(strong("Filters applied:"), "Date range (90 days), Virus target (WNV/LAC/EEE), Species, Trap types (4/5/6)")
            )
          )
        ),
        tabItem(tabName = "table",
          fluidRow(
            box(width = 12, title = "Section Population Index Table", status = "primary", solidHeader = TRUE,
                div(style = "margin-bottom: 10px;",
                  downloadButton("download_vector_data", "Download CSV", class = "btn-primary btn-sm")
                ),
                dataTableOutput("table")
            )
          )
        )
      )
    )
  )
}
