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
        menuItem("Causal Analysis", tabName = "trap_analysis_tab", icon = icon("flask")),
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
            box(width = 12, title = "District-Wide Vector Index Trend", status = "danger", solidHeader = TRUE,
                plotly::plotlyOutput("vi_trend_plot", height = "400px")
            )
          ),
          fluidRow(
            box(width = 12, title = "Vector Index by Area Over Time", status = "danger", solidHeader = TRUE,
                plotly::plotlyOutput("vi_area_trend_plot", height = "400px")
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
        # CAUSAL ANALYSIS TAB
        # =====================================================================
        tabItem(tabName = "trap_analysis_tab",
          fluidRow(
            box(width = 3, title = "Analysis Controls", status = "primary", solidHeader = TRUE,
              selectInput("analysis_year", "Year (or All Years):",
                         choices = NULL,
                         selected = NULL),
              hr(),
              sliderInput("analysis_bandwidth", "Spatial Bandwidth (km):",
                         min = 1, max = 10, value = 3, step = 0.5),
              sliderInput("analysis_radius", "Max Neighbor Radius (km):",
                         min = 3, max = 20, value = 10, step = 1),
              hr(),
              div(style = "background-color:#f5f5f5; padding:10px; border-radius:5px; margin-bottom:10px; font-size:12px;",
                h6(strong("What This Does"), style = "margin-top:0;"),
                p(style = "margin-bottom:4px;", strong("Scoring:"), " Rates each trap's WNV detection value (yield, pools, positivity, consistency)."),
                p(style = "margin-bottom:4px;", strong("Risk Surface:"), " Interpolates trap scores into an area-wide WNV risk heatmap."),
                p(style = "margin-bottom:4px;", strong("Coverage:"), " Grades each VI area's surveillance coverage (Good/Adequate/Thin/Gap)."),
                p(style = "margin-bottom:0;", strong("Causal Factors:"), " Identifies which trap features drive performance.")
              ),
              hr(),
              actionButton("analysis_refresh", "Run Analysis", icon = icon("play"),
                          class = "btn-warning", width = "100%"),
              hr(),
              div(style = "font-size:11px; color:#888;",
                p("Based on Chakravarti et al. (2026):"),
                p(em("'A novel approach to determine mosquito trap placement for WNV surveillance'")),
                p("J. Med. Entomol. 63(2) ")
              )
            ),
            box(width = 9, title = "Spatial Risk & Trap Scores", status = "warning", solidHeader = TRUE,
                leaflet::leafletOutput("analysis_map", height = "600px")
            )
          ),
          fluidRow(
            box(width = 4, title = "Area Coverage Summary", status = "success", solidHeader = TRUE,
                div(style = "max-height:320px; overflow-y:auto;",
                  DT::dataTableOutput("coverage_table")
                )
            ),
            box(width = 4, title = "Factor Importance", status = "info", solidHeader = TRUE,
                plotly::plotlyOutput("importance_plot", height = "300px")
            ),
            box(width = 4, title = "Score Distribution", status = "info", solidHeader = TRUE,
                plotly::plotlyOutput("score_histogram", height = "300px")
            )
          ),
          fluidRow(
            box(width = 3, title = "Summary", status = "primary", solidHeader = TRUE,
                uiOutput("analysis_summary_boxes")
            ),
            box(width = 4, title = "Dose-Response: Pools \u2192 Score", status = "info", solidHeader = TRUE,
                plotly::plotlyOutput("dr_pools_plot", height = "280px")
            ),
            box(width = 5, title = "Dose-Response: Detection Rate \u2192 Score", status = "info", solidHeader = TRUE,
                plotly::plotlyOutput("dr_positives_plot", height = "280px")
            )
          ),
          fluidRow(
            box(width = 12, title = "Trap Scorecard & Recommendations", status = "primary", solidHeader = TRUE,
                div(style = "margin-bottom: 10px;",
                  downloadButton("download_analysis_data", "Download Analysis CSV", class = "btn-primary btn-sm")
                ),
                DT::dataTableOutput("analysis_table")
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
              p("The ", strong("Causal Analysis"), " tab implements an analytical framework inspired by ",
                strong("Chakravarti et al. (2026)"), ": ",
                em("'A novel approach to determine mosquito trap placement for WNV surveillance'"),
                " — J. Med. Entomol. 63(2). ",
                "The implementation adapts the paper's concepts to MMCD's data and operational context."),
              hr(),
              
              h5(icon("database"), " Data Sources"),
              tags$ul(
                tags$li(strong("Abundance:"), " dbadult_mon_nt_co2_forvectorabundance — Monday night CO2 trap counts by species, location, and week."),
                tags$li(strong("MLE:"), " dbvirus_mle_yrwk_area / dbvirus_mle_yrwk_area_spp — Pre-calculated Maximum Likelihood Estimates of infection rate."),
                tags$li(strong("MIR:"), " dbvirus_mir_yrwk_area / dbvirus_mir_yrwk_area_spp — Pre-calculated Minimum Infection Rates."),
                tags$li(strong("Virus Pools:"), " dbadult_insp_current, dbvirus_pool, dbvirus_pool_test — Pool collection and WNV testing results per trap."),
                tags$li(strong("Trap Locations:"), " loc_mondaynight — Trap coordinates (PostGIS geometry)."),
                tags$li(strong("Geometry:"), " loc_vectorindexareas_sections_a — Section polygons dissolved into 12 VI areas.")
              ),
              hr(),
              
              h5(icon("calculator"), " Core Metrics (Map & Trend Tabs)"),
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
                p("Simple proportion: (positive pools / total mosquitoes tested) \u00d7 1000"),
                tags$code("MIR = (Positives / Total Mosquitoes) \u00d7 1000")
              ),
              div(style = "background-color:#fff3cd; padding:12px; border-radius:5px; border-left:4px solid #ffc107;",
                h6(strong("Vector Index (VI = N \u00d7 P)")),
                p("Combined measure of mosquito abundance and infection risk."),
                tags$code("VI = N \u00d7 P"),
                p("Where N = average mosquitoes/trap, P = infection rate (MLE or MIR/1000)")
              ),
              hr(),

              h4("Causal Analysis Methods"),
              p("The Causal Analysis tab runs four linked analyses in sequence. Full equations with derivations are in the ",
                strong("TRAP_ANALYSIS_EQUATIONS.md"), " document."),
              hr(),

              h5(icon("crosshairs"), " 1. Trap Performance Scoring"),
              div(style = "background-color:#eaf2f8; padding:12px; border-radius:5px; border-left:4px solid #2c5aa0; margin-bottom:10px;",
                p("Each trap is scored on a ", strong("composite performance metric (0\u20131)"), " based on four weighted factors:"),
                tags$table(style = "width:100%; font-size:13px; margin:8px 0;",
                  tags$tr(tags$th("Component"), tags$th("Weight"), tags$th("Description")),
                  tags$tr(tags$td("Yield Score"), tags$td("25%"), tags$td("Avg mosquitoes per week, normalized by max across all traps")),
                  tags$tr(tags$td("Testing Score"), tags$td("30%"), tags$td("Total WNV pools collected, normalized by max")),
                  tags$tr(tags$td("Detection Score"), tags$td("30%"), tags$td("Pool positivity rate (% positive), normalized by max")),
                  tags$tr(tags$td("Consistency Score"), tags$td("15%"), tags$td("Weeks active, normalized by max"))
                ),
                tags$code("composite = 0.25\u00d7yield + 0.30\u00d7testing + 0.30\u00d7detection + 0.15\u00d7consistency"),
                p(style = "margin-top:8px;", strong("Tiers:"), " High (\u2265 0.6) | Medium (0.3\u20130.6) | Low (< 0.3)"),
                p(style = "color:#666; font-size:12px;", "Weights reflect the paper's finding that testing volume and detection are the strongest predictors of trap utility.")
              ),
              hr(),

              h5(icon("globe"), " 2. Spatial Risk Smoothing"),
              div(style = "background-color:#fdedec; padding:12px; border-radius:5px; border-left:4px solid #e74c3c; margin-bottom:10px;",
                p("Computes a spatial risk score for each trap using exponential kernel smoothing of neighboring trap signals. ",
                  "This approximates the spatial correlation structure (Mat\u00e9rn covariance) used in the paper's GLMM, using Haversine distances between traps."),
                p("For each trap ", em("i"), ", the spatial risk is a weighted average of neighbor signals:"),
                tags$code("spatial_risk_i = \u03a3(w_j \u00d7 signal_j) / \u03a3(w_j)"),
                p(style = "margin-top:6px;", "Where:"),
                tags$ul(
                  tags$li(tags$code("w_j = exp(-dist_ij / bandwidth)"), " — Exponential decay kernel (bandwidth default 3 km)"),
                  tags$li(tags$code("signal_j = 0.6\u00d7detection + 0.4\u00d7testing"), " — Neighbor's detection signal"),
                  tags$li(tags$code("dist_ij"), " — Haversine (great-circle) distance in km between traps i and j")
                ),
                p("The combined ", strong("Risk Index"), " integrates the trap's own metrics with its spatial context:"),
                tags$code("risk_index = 0.35\u00d7yield + 0.30\u00d7spatial_risk + 0.35\u00d7detection"),
                p(style = "margin-top:8px;", strong("Tiers:"), " High Risk (\u2265 0.5) | Moderate (0.2\u20130.5) | Low (< 0.2)")
              ),
              hr(),

              h5(icon("th"), " 3. Risk Surface & Coverage Assessment"),
              div(style = "background-color:#e8f8f5; padding:12px; border-radius:5px; border-left:4px solid #1abc9c; margin-bottom:10px;",
                p("The ", strong("Risk Surface"), " interpolates trap-level risk scores onto a regular grid (~500m resolution) spanning the district, visualizing WNV risk ", em("between"), " trap locations."),
                tags$code("grid_risk(x,y) = \u03a3(w_j \u00d7 risk_index_j) / \u03a3(w_j)"),
                p(style = "margin-top:6px;", "Grid cells farther than 2\u00d7 bandwidth from any trap are flagged as ", strong("coverage gaps"), "."),
                hr(),
                p(strong("Area Coverage Grades"), " aggregate trap metrics per VI area:"),
                tags$table(style = "width:100%; font-size:13px; margin:8px 0;",
                  tags$tr(tags$th("Grade"), tags$th("Criteria")),
                  tags$tr(tags$td("\u2705 Good"), tags$td("\u2265 10 traps AND avg score \u2265 0.3")),
                  tags$tr(tags$td("\u26a0\ufe0f Adequate"), tags$td("\u2265 5 traps AND avg score \u2265 0.2")),
                  tags$tr(tags$td("\u26a0\ufe0f Thin"), tags$td("\u2265 3 traps (any avg score)")),
                  tags$tr(tags$td("\u274c Gap"), tags$td("Fewer than 3 traps"))
                )
              ),
              hr(),

              h5(icon("flask"), " 4. Causal Factor Analysis"),
              div(style = "background-color:#fef9e7; padding:12px; border-radius:5px; border-left:4px solid #f39c12; margin-bottom:10px;",
                p("Identifies which trap-level variables have the strongest association with performance using ",
                  strong("Spearman rank correlations"), " and binned ", strong("dose-response curves"), " (approximating ADRFs from the paper)."),
                p(strong("Factors analyzed:")),
                tags$ul(
                  tags$li("Pools Collected — total WNV pools submitted"),
                  tags$li("Positive Pools — number of pools testing positive"),
                  tags$li("Avg Yield/Week — average mosquitoes captured per collection week"),
                  tags$li("Weeks Active — number of distinct collection weeks"),
                  tags$li("Total Mosquitoes — total specimens collected")
                ),
                p(strong("Key finding from the paper:"), " Three variables showed significant causal effects: ",
                  em("(1) total population in 1,500m buffer"), " (not available in MMCD data), ",
                  em("(2) number of pools collected"), ", and ",
                  em("(3) number of positive pools"), 
                  ". Landcover and socioeconomic variables did NOT show significant direct causal effects."),
                p("Per-trap recommendations are generated based on score thresholds and factor values.")
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
                "Each yrwk value encodes year and week number (e.g., 202537 = year 2025, week 37)."),
              hr(),

              h5(icon("book"), " Reference"),
              p("Chakravarti, Li, Bartlett, Irwin, Smith (2026). ",
                em("'A novel approach to determine mosquito trap placement for West Nile virus surveillance.'"),
                " Journal of Medical Entomology, 63(2).")
            )
          )
        )
      )
    )
  )
}
