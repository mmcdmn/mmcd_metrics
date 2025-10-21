library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(DT)
library(sf)
library(plotly)
library(shinyWidgets)
library(tidyr)

# Load environment variables from .env file (for local development)
# or from Docker environment variables (for production)
env_paths <- c(
  "../../.env",           # For local development
  "../../../.env",        # Alternative local path
  "/srv/shiny-server/.env" # Docker path
)

# Try to load from .env file first
env_loaded <- FALSE
for (path in env_paths) {
  if (file.exists(path)) {
    readRenviron(path)
    env_loaded <- TRUE
    break
  }
}

# Database configuration using environment variables
db_host <- Sys.getenv("DB_HOST")
db_port <- Sys.getenv("DB_PORT")
db_user <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")
db_name <- Sys.getenv("DB_NAME")

# Database connection function
get_db_connection <- function() {
  tryCatch({
    dbConnect(RPostgres::Postgres(),
              host = db_host,
              port = as.numeric(db_port),
              user = db_user,
              password = db_password,
              dbname = db_name)
  }, error = function(e) {
    showNotification(paste("Database connection failed:", e$message), type = "error", duration = 10)
    NULL
  })
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Air Site Treatment Pipeline Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Air Site Pipeline", tabName = "pipeline", icon = icon("plane")),
      menuItem("Inspection Status", tabName = "inspection", icon = icon("search")),
      menuItem("Treatment Queue", tabName = "treatment_queue", icon = icon("list-alt")),
      menuItem("Performance Metrics", tabName = "metrics", icon = icon("chart-bar")),
      menuItem("Site Details", tabName = "details", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .leaflet-container {
          background-color: white;
        }
      "))
    ),
    
    tabItems(
      # Air Site Pipeline Tab
      tabItem(tabName = "pipeline",
        fluidRow(
          box(title = "Air Site Pipeline Controls", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(3,
                dateInput("pretend_today", "Pretend Today Is:",
                  value = Sys.Date(),
                  max = Sys.Date()
                )
              ),
              column(3,
                selectInput("data_source", "Data Source:",
                  choices = list(
                    "Current Year" = "current",
                    "Archive" = "archive"
                  ),
                  selected = "current"
                )
              ),
              column(3,
                numericInput("rainfall_threshold", "Rainfall Threshold (inches):",
                  value = 1.0,
                  min = 0.1,
                  max = 10,
                  step = 0.1
                )
              ),
              column(3,
                selectInput("priority_focus", "Priority Level:",
                  choices = list(
                    "Red Only" = "RED",
                    "Red + Yellow" = "RED,YELLOW",
                    "All Priorities" = "all"
                  ),
                  selected = "RED"
                )
              )
            ),
            fluidRow(
              column(3,
                radioButtons("lookback_period", "Rainfall Lookback Period:",
                  choices = list(
                    "Last 7 days" = 7,
                    "Last 14 days" = 14,
                    "Last 30 days" = 30
                  ),
                  selected = 7,
                  inline = TRUE
                )
              ),
              column(3,
                selectInput("facility_filter", "Facility:",
                  choices = c("All Facilities" = "all"),
                  selected = "all"
                )
              ),
              column(3,
                radioButtons("prehatch_filter", "Prehatch Sites:",
                  choices = list(
                    "Include All" = "include",
                    "Exclude Prehatch" = "exclude", 
                    "Prehatch Only" = "only"
                  ),
                  selected = "include",
                  inline = TRUE
                )
              ),
              column(3,
                numericInput("dip_threshold", "Treatment Threshold (dip count):",
                  value = 2.0,
                  min = 0.1,
                  max = 50,
                  step = 0.1
                )
              )
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("air_sites_with_rain", width = 3),
          valueBoxOutput("sites_needing_inspection", width = 3),
          valueBoxOutput("sites_inspected", width = 3),
          valueBoxOutput("sites_needing_treatment", width = 3)
        ),
        
        fluidRow(
          box(title = "Air Sites Pipeline Map", status = "primary", solidHeader = TRUE, width = 8,
            leafletOutput("pipeline_map", height = "500px")
          ),
          box(title = "Pipeline Summary", status = "info", solidHeader = TRUE, width = 4,
            div(style = "height: 500px; overflow-y: auto;",
              h4("Step 1: Rainfall → Air Sites"),
              DT::dataTableOutput("rainfall_summary_table"),
              h4("Step 2: Inspections Needed"),
              DT::dataTableOutput("inspection_summary_table"),
              h4("Step 3: Treatment Queue"),
              DT::dataTableOutput("treatment_queue_summary")
            )
          )
        ),
        
        fluidRow(
          box(title = "Air Sites Needing Attention", status = "warning", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("pipeline_table")
          )
        )
      ),
      
      # Inspection Status Tab
      tabItem(tabName = "inspection",
        fluidRow(
          box(title = "Inspection Status Controls", status = "success", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4,
                h4("Sites requiring inspection after rainfall events")
              ),
              column(4,
                checkboxInput("show_overdue_only", "Show Overdue Inspections Only", value = FALSE)
              ),
              column(4,
                numericInput("inspection_days_threshold", "Days Since Rainfall (Overdue):",
                  value = 3,
                  min = 1,
                  max = 14,
                  step = 1
                )
              )
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("total_inspections_needed", width = 3),
          valueBoxOutput("inspections_completed", width = 3),
          valueBoxOutput("inspections_overdue", width = 3),
          valueBoxOutput("inspection_completion_rate", width = 3)
        ),
        
        fluidRow(
          box(title = "Inspection Status by Facility", status = "success", solidHeader = TRUE, width = 8,
            plotlyOutput("inspection_status_plot", height = "400px")
          ),
          box(title = "Inspection Timeline", status = "info", solidHeader = TRUE, width = 4,
            plotOutput("inspection_timeline", height = "400px")
          )
        ),
        
        fluidRow(
          box(title = "Sites Needing Inspection", status = "warning", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("inspection_status_table")
          )
        )
      ),
      
      # Treatment Queue Tab  
      tabItem(tabName = "treatment_queue",
        fluidRow(
          box(title = "Treatment Queue Controls", status = "danger", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4,
                h4("Sites requiring treatment based on inspection results")
              ),
              column(4,
                checkboxInput("show_untreated_only", "Show Untreated Sites Only", value = TRUE)
              ),
              column(4,
                selectInput("treatment_type_filter", "Treatment Type:",
                  choices = list(
                    "All Treatments" = "all",
                    "Air Treatment (A)" = "A",
                    "Ground Treatments (1,3,D)" = "ground"
                  ),
                  selected = "all"
                )
              )
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("sites_requiring_treatment", width = 3),
          valueBoxOutput("air_treatments_completed", width = 3),
          valueBoxOutput("ground_treatments_completed", width = 3),
          valueBoxOutput("treatment_backlog", width = 3)
        ),
        
        fluidRow(
          box(title = "Treatment Queue Map", status = "danger", solidHeader = TRUE, width = 8,
            leafletOutput("treatment_queue_map", height = "500px")
          ),
          box(title = "Treatment Priority", status = "warning", solidHeader = TRUE, width = 4,
            plotOutput("treatment_priority_plot", height = "500px")
          )
        ),
        
        fluidRow(
          box(title = "Sites in Treatment Queue", status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("treatment_queue_table")
          )
        )
      ),

      # Performance Metrics Tab (renamed from map)
      tabItem(tabName = "metrics",
        fluidRow(
          box(title = "Controls", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(3,
                dateRangeInput("date_range", "Date Range:",
                  start = Sys.Date() - 365,
                  end = Sys.Date(),
                  max = Sys.Date()
                )
              ),
              column(3,
                selectInput("facility_filter", "Facility:",
                  choices = c("Select Facility" = "none"),
                  selected = "none"
                )
              ),
              column(3,
                selectInput("wetland_type_filter", "Wetland Type:",
                  choices = c("All Types" = "all"),
                  selected = "all",
                  multiple = TRUE
                )
              ),
              column(3,
                selectInput("priority_filter", "Priority:",
                  choices = c("All Priorities" = "all"),
                  selected = "all",
                  multiple = TRUE
                )
              )
            ),
            fluidRow(
              column(6,
                sliderInput("min_rainfall", "Minimum Rainfall (inches):",
                  min = 0, max = 5, value = 0, step = 0.1
                )
              ),
              column(6,
                radioButtons("rainfall_period", "Rainfall Period:",
                  choices = list(
                    "Last 7 days" = "week",
                    "Last 30 days" = "month", 
                    "Selected date range" = "custom"
                  ),
                  selected = "month",
                  inline = TRUE
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(title = "Breeding Sites and Rainfall", status = "info", solidHeader = TRUE, width = 12, height = "600px",
            leafletOutput("rainfall_map", height = "550px")
          )
        ),
        
        fluidRow(
          column(6,
            valueBoxOutput("total_sites", width = 12)
          ),
          column(6,
            valueBoxOutput("avg_rainfall", width = 12)
          )
        )
      ),
      
      # Treatment Analysis Tab
      tabItem(tabName = "treatment",
        fluidRow(
          box(title = "Treatment Response Analysis", status = "success", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4,
                numericInput("treatment_threshold", "Min Rainfall for Treatment (inches):",
                  value = 1.0,
                  min = 0.1,
                  max = 10,
                  step = 0.1
                )
              ),
              column(4,
                numericInput("max_response_days", "Max Response Days:",
                  value = 7,
                  min = 1,
                  max = 30,
                  step = 1
                )
              ),
              column(4,
                selectInput("treatment_facility", "Facility:",
                  choices = c("All Facilities" = "all"),
                  selected = "all"
                )
              )
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("total_rainfall_events", width = 3),
          valueBoxOutput("sites_treated_after_rain", width = 3),
          valueBoxOutput("avg_treatment_response", width = 3),
          valueBoxOutput("treatment_success_rate", width = 3)
        ),
        
        fluidRow(
          box(title = "Rainfall to Treatment Timeline", status = "info", solidHeader = TRUE, width = 8,
            plotlyOutput("treatment_timeline_plot", height = "400px")
          ),
          box(title = "Response Time Distribution", status = "warning", solidHeader = TRUE, width = 4,
            plotOutput("response_time_dist", height = "400px")
          )
        ),
        
        fluidRow(
          box(title = "Treatment Response Details", status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("treatment_response_table")
          )
        )
      ),
      
      # Wetland Metrics Tab
      tabItem(tabName = "metrics",
        fluidRow(
          box(title = "Wetland Metrics Controls", status = "info", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4,
                numericInput("metrics_rainfall_threshold", "Rainfall Threshold (inches):",
                  value = 1.0,
                  min = 0.1,
                  max = 10,
                  step = 0.1
                )
              ),
              column(4,
                selectInput("metrics_facility", "Facility:",
                  choices = c("All Facilities" = "all"),
                  selected = "all"
                )
              ),
              column(4,
                radioButtons("metrics_period", "Analysis Period:",
                  choices = list(
                    "Last 30 days" = 30,
                    "Last 60 days" = 60,
                    "Last 90 days" = 90
                  ),
                  selected = 30,
                  inline = TRUE
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(title = "Precipitation Threshold Analysis", status = "primary", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("threshold_analysis_table")
          ),
          box(title = "Wetland Status by Acreage", status = "info", solidHeader = TRUE, width = 6,
            plotOutput("acreage_status_plot", height = "300px")
          )
        ),
        
        fluidRow(
          box(title = "Risk Designation Matrix", status = "warning", solidHeader = TRUE, width = 8,
            DT::dataTableOutput("risk_matrix_table")
          ),
          box(title = "Risk Distribution", status = "success", solidHeader = TRUE, width = 4,
            plotOutput("risk_distribution_plot", height = "300px")
          )
        )
      ),
      
      # Details Tab
      tabItem(tabName = "details",
        fluidRow(
          box(title = "Site Rainfall Details", status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("site_rainfall_table")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load filter choices
  observe({
    con <- get_db_connection()
    if (!is.null(con)) {
      tryCatch({
        # Get facilities
        facilities <- dbGetQuery(con, "SELECT DISTINCT facility FROM loc_breeding_sites WHERE facility IS NOT NULL ORDER BY facility")
        fac_choices <- setNames(facilities$facility, facilities$facility)
        fac_choices_all <- c("All Facilities" = "all", fac_choices)
        fac_choices_select <- c("Select Facility" = "none", fac_choices)
        
        # Get wetland types
        types <- dbGetQuery(con, "SELECT DISTINCT type FROM loc_breeding_sites WHERE type IS NOT NULL ORDER BY type")
        type_choices <- setNames(types$type, types$type)
        type_choices <- c("All Types" = "all", type_choices)
        
        # Get priorities
        priorities <- dbGetQuery(con, "SELECT DISTINCT priority FROM loc_breeding_sites WHERE priority IS NOT NULL ORDER BY priority")
        priority_choices <- setNames(priorities$priority, priorities$priority)
        priority_choices <- c("All Priorities" = "all", priority_choices)
        
        # Get location filters (counties and cities from sitecodes with lookup names)
        sitecodes <- dbGetQuery(con, "SELECT DISTINCT sitecode FROM loc_breeding_sites WHERE sitecode IS NOT NULL")
        if (nrow(sitecodes) > 0) {
          counties <- unique(substr(sitecodes$sitecode, 1, 2))
          counties <- counties[!is.na(counties) & nchar(counties) == 2]
          
          # Get county names from lookup table
          county_lookup <- dbGetQuery(con, "SELECT id, name FROM public.lookup_county ORDER BY id")
          county_names <- setNames(county_lookup$name, sprintf("%02d", county_lookup$id))
          
          # Create choices with names but values as codes
          county_choices <- c("All Counties" = "all")
          for (code in counties) {
            name <- county_names[code]
            if (!is.na(name)) {
              county_choices[paste0(name, " (", code, ")")] <- code
            } else {
              county_choices[code] <- code
            }
          }
          updateSelectInput(session, "county_filter", choices = county_choices)
        }
        
        # Update all facility dropdowns
        updateSelectInput(session, "facility_filter", choices = fac_choices_all)
        
        updateSelectInput(session, "wetland_type_filter", choices = type_choices)
        updateSelectInput(session, "priority_filter", choices = priority_choices)
        
        dbDisconnect(con)
      }, error = function(e) {
        showNotification(paste("Error loading filters:", e$message), type = "error")
        if (!is.null(con)) dbDisconnect(con)
      })
    }
  })
  
  # Helper function to get treatment data
  get_treatment_data <- function(start_date, end_date, data_source = "current") {
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    
    tryCatch({
      # Choose table based on data source
      table_name <- if (data_source == "archive") "public.dblarv_insptrt_archive" else "public.dblarv_insptrt_current"
      
      query <- sprintf("
        SELECT 
          t.inspdate,
          t.facility,
          t.sitecode,
          t.action,
          t.acres,
          t.matcode,
          ST_X(ST_Centroid(ST_Transform(s.geom, 4326))) as longitude,
          ST_Y(ST_Centroid(ST_Transform(s.geom, 4326))) as latitude
        FROM %s t
        LEFT JOIN loc_breeding_sites s ON t.sitecode = s.sitecode
        WHERE t.inspdate >= '%s' 
          AND t.inspdate <= '%s'
          AND t.action IN ('4', '2', '1')
          AND s.geom IS NOT NULL
        ORDER BY t.inspdate, t.sitecode
      ", table_name, start_date, end_date)
      
      result <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      if (nrow(result) > 0) {
        result$inspdate <- as.Date(result$inspdate)
        return(result)
      } else {
        return(data.frame())
      }
      
    }, error = function(e) {
      showNotification(paste("Error loading treatment data:", e$message), type = "error")
      if (!is.null(con)) dbDisconnect(con)
      return(data.frame())
    })
  }
  
  # Air site pipeline data - Step 1: Air sites with qualifying rainfall
  air_sites_with_rain <- reactive({
    req(input$pretend_today, input$rainfall_threshold, input$lookback_period)
    
    # Calculate lookback period
    end_date <- input$pretend_today
    start_date <- end_date - as.numeric(input$lookback_period)
    
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    
      tryCatch({
        # Build priority filter
        priority_condition <- if (input$priority_focus == "all") {
          ""
        } else {
          priorities <- unlist(strsplit(input$priority_focus, ","))
          priority_list <- paste(sprintf("'%s'", priorities), collapse = ",")
          sprintf("AND s.priority IN (%s)", priority_list)
        }      # Build facility filter
      facility_condition <- if (input$facility_filter == "all") {
        ""
      } else {
        sprintf("AND s.facility = '%s'", input$facility_filter)
      }
      
      # Build prehatch filter - TODO: Find correct column name for prehatch entities
      prehatch_condition <- ""
      if (input$prehatch_filter == "exclude") {
        prehatch_condition <- "AND s.prehatch NOT IN ('BRIQUETS', 'BTI', 'PELLET', 'REHATCH')"
      } else if (input$prehatch_filter == "only") {
        prehatch_condition <- "AND s.prehatch IN ('BRIQUETS', 'BTI', 'PELLET', 'REHATCH')"
      }
      
      # Get AIR sites with rainfall above threshold
      query <- sprintf("
        SELECT 
          s.sitecode,
          s.acres,
          s.type,
          s.priority,
          s.facility,
          s.air_gnd,
          s.prehatch,
          ST_X(ST_Centroid(ST_Transform(s.geom, 4326))) as longitude,
          ST_Y(ST_Centroid(ST_Transform(s.geom, 4326))) as latitude,
          COALESCE(SUM(r.rain_inches), 0) as total_rainfall,
          COUNT(r.date) as rain_days,
          MAX(r.date) as last_rain_date
        FROM loc_breeding_sites s
        LEFT JOIN breeding_site_hrap h ON s.sitecode = h.sitecode
        LEFT JOIN nws_precip_site_history r ON h.sitecode = r.sitecode 
          AND r.date >= '%s' 
          AND r.date <= '%s'
        WHERE s.geom IS NOT NULL
          AND s.startdate IS NOT NULL
          AND (s.enddate IS NULL OR s.enddate > '%s')
          AND s.air_gnd = 'A'
          %s
          %s
          %s
        GROUP BY s.sitecode, s.acres, s.type, s.priority, s.facility, s.air_gnd, s.prehatch,
                 ST_X(ST_Centroid(ST_Transform(s.geom, 4326))), ST_Y(ST_Centroid(ST_Transform(s.geom, 4326)))
        HAVING COALESCE(SUM(r.rain_inches), 0) >= %f
        ORDER BY s.priority, total_rainfall DESC
      ", start_date, end_date, end_date, priority_condition, facility_condition, prehatch_condition, input$rainfall_threshold)
      
      result <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      # Add days since rain calculation
      if (nrow(result) > 0) {
        result$last_rain_date <- as.Date(result$last_rain_date)
        result$days_since_rain <- as.numeric(input$pretend_today - result$last_rain_date)
      }
      
      return(result)
      
    }, error = function(e) {
      showNotification(paste("Error loading air sites with rain:", e$message), type = "error")
      if (!is.null(con)) dbDisconnect(con)
      return(data.frame())
    })
  })
  
  # Step 2: Get inspection data for air sites
  inspection_data <- reactive({
    air_sites <- air_sites_with_rain()
    if (is.null(air_sites) || nrow(air_sites) == 0) return(data.frame())
    
    req(input$pretend_today)
    end_date <- input$pretend_today
    start_date <- end_date - as.numeric(input$lookback_period)
    
    con <- get_db_connection()
    if (is.null(con)) return(data.frame())
    
    tryCatch({
      # Choose table based on data source
      table_name <- if (input$data_source == "archive") "public.dblarv_insptrt_archive" else "public.dblarv_insptrt_current"
      
      # Get sites that need inspection (from air sites with rain)
      site_list <- paste(sprintf("'%s'", air_sites$sitecode), collapse = ",")
      
      # Get inspections (actions 1, 4, 3) after rainfall events
      query <- sprintf("
        SELECT 
          t.sitecode,
          t.inspdate,
          t.facility,
          t.action,
          t.numdip,
          t.wet
        FROM %s t
        WHERE t.sitecode IN (%s)
          AND t.inspdate >= '%s' 
          AND t.inspdate <= '%s'
          AND t.action IN ('1', '4', '3')
        ORDER BY t.sitecode, t.inspdate DESC
      ", table_name, site_list, start_date, end_date)
      
      inspections <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      if (nrow(inspections) > 0) {
        inspections$inspdate <- as.Date(inspections$inspdate)
        
        # For each air site, determine inspection status
        for (i in 1:nrow(air_sites)) {
          site_inspections <- inspections[inspections$sitecode == air_sites$sitecode[i] & 
                                        inspections$inspdate >= air_sites$last_rain_date[i], ]
          
          if (nrow(site_inspections) > 0) {
            # Get most recent inspection after rainfall
            latest_inspection <- site_inspections[which.max(site_inspections$inspdate), ]
            air_sites$inspected[i] <- TRUE
            air_sites$inspection_date[i] <- latest_inspection$inspdate
            air_sites$days_to_inspection[i] <- as.numeric(latest_inspection$inspdate - air_sites$last_rain_date[i])
            air_sites$numdip[i] <- latest_inspection$numdip
            air_sites$wet_status[i] <- latest_inspection$wet
            air_sites$needs_treatment[i] <- !is.na(latest_inspection$numdip) && latest_inspection$numdip >= input$dip_threshold
          } else {
            air_sites$inspected[i] <- FALSE
            air_sites$inspection_date[i] <- as.Date(NA)
            air_sites$days_to_inspection[i] <- NA
            air_sites$numdip[i] <- NA
            air_sites$wet_status[i] <- NA
            air_sites$needs_treatment[i] <- FALSE
          }
        }
      } else {
        # No inspections found
        air_sites$inspected <- FALSE
        air_sites$inspection_date <- as.Date(NA)
        air_sites$days_to_inspection <- NA
        air_sites$numdip <- NA
        air_sites$wet_status <- NA
        air_sites$needs_treatment <- FALSE
      }
      
      return(air_sites)
      
    }, error = function(e) {
      if (!is.null(con)) dbDisconnect(con)
      showNotification(paste("Error loading inspection data:", e$message), type = "error")
      return(air_sites)
    })
  })
  
  # Step 3: Get treatment data for sites needing treatment
  treatment_queue_data <- reactive({
    inspection_data <- inspection_data()
    if (is.null(inspection_data) || nrow(inspection_data) == 0) return(data.frame())
    
    # Filter to sites that need treatment
    sites_needing_treatment <- inspection_data[inspection_data$needs_treatment & !is.na(inspection_data$needs_treatment), ]
    if (nrow(sites_needing_treatment) == 0) return(data.frame())
    
    req(input$pretend_today)
    end_date <- input$pretend_today
    start_date <- end_date - as.numeric(input$lookback_period)
    
    con <- get_db_connection()
    if (is.null(con)) return(sites_needing_treatment)
    
    tryCatch({
      # Choose table based on data source
      table_name <- if (input$data_source == "archive") "public.dblarv_insptrt_archive" else "public.dblarv_insptrt_current"
      
      site_list <- paste(sprintf("'%s'", sites_needing_treatment$sitecode), collapse = ",")
      
      # Get treatments (actions A, 1, 3, D) after inspection
      query <- sprintf("
        SELECT 
          t.sitecode,
          t.inspdate as treatment_date,
          t.facility,
          t.action as treatment_action,
          t.matcode,
          t.acres as treated_acres
        FROM %s t
        WHERE t.sitecode IN (%s)
          AND t.inspdate >= '%s' 
          AND t.inspdate <= '%s'
          AND t.action IN ('A', '1', '3', 'D')
        ORDER BY t.sitecode, t.inspdate DESC
      ", table_name, site_list, start_date, end_date)
      
      treatments <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      if (nrow(treatments) > 0) {
        treatments$treatment_date <- as.Date(treatments$treatment_date)
        
        # For each site needing treatment, check if it was treated after inspection
        for (i in 1:nrow(sites_needing_treatment)) {
          if (!is.na(sites_needing_treatment$inspection_date[i])) {
            site_treatments <- treatments[treatments$sitecode == sites_needing_treatment$sitecode[i] & 
                                        treatments$treatment_date >= sites_needing_treatment$inspection_date[i], ]
            
            if (nrow(site_treatments) > 0) {
              # Get most recent treatment after inspection
              latest_treatment <- site_treatments[which.max(site_treatments$treatment_date), ]
              sites_needing_treatment$treated[i] <- TRUE
              sites_needing_treatment$treatment_date[i] <- latest_treatment$treatment_date
              sites_needing_treatment$treatment_action[i] <- latest_treatment$treatment_action
              sites_needing_treatment$days_to_treatment[i] <- as.numeric(latest_treatment$treatment_date - sites_needing_treatment$inspection_date[i])
            } else {
              sites_needing_treatment$treated[i] <- FALSE
              sites_needing_treatment$treatment_date[i] <- as.Date(NA)
              sites_needing_treatment$treatment_action[i] <- NA
              sites_needing_treatment$days_to_treatment[i] <- NA
            }
          } else {
            sites_needing_treatment$treated[i] <- FALSE
            sites_needing_treatment$treatment_date[i] <- as.Date(NA)
            sites_needing_treatment$treatment_action[i] <- NA
            sites_needing_treatment$days_to_treatment[i] <- NA
          }
        }
      } else {
        # No treatments found
        sites_needing_treatment$treated <- FALSE
        sites_needing_treatment$treatment_date <- as.Date(NA)
        sites_needing_treatment$treatment_action <- NA
        sites_needing_treatment$days_to_treatment <- NA
      }
      
      return(sites_needing_treatment)
      
    }, error = function(e) {
      if (!is.null(con)) dbDisconnect(con)
      showNotification(paste("Error loading treatment queue data:", e$message), type = "error")
      return(sites_needing_treatment)
    })
  })
  
  # Update city filter based on county selection
  observe({
    req(input$county_filter)
    if (input$county_filter != "all") {
      con <- get_db_connection()
      if (!is.null(con)) {
        tryCatch({
          # Get cities for selected county
          sitecodes <- dbGetQuery(con, sprintf("
            SELECT DISTINCT sitecode 
            FROM loc_breeding_sites 
            WHERE sitecode LIKE '%s%%'
          ", input$county_filter))
          
          if (nrow(sitecodes) > 0) {
            cities <- unique(substr(sitecodes$sitecode, 3, 4))
            cities <- cities[!is.na(cities) & nchar(cities) == 2]
            
            # Get city names from lookup table
            city_lookup <- dbGetQuery(con, "SELECT towncode, city FROM public.lookup_towncode_name")
            city_names <- setNames(city_lookup$city, city_lookup$towncode)
            
            # Create choices with names but values as codes
            city_choices <- c("All Cities" = "all")
            for (code in cities) {
              towncode <- paste0(input$county_filter, code)
              name <- city_names[towncode]
              if (!is.na(name)) {
                city_choices[paste0(name, " (", code, ")")] <- code
              } else {
                city_choices[code] <- code
              }
            }
            updateSelectInput(session, "city_filter", choices = city_choices)
          }
          
          dbDisconnect(con)
        }, error = function(e) {
          if (!is.null(con)) dbDisconnect(con)
        })
      }
    }
  })
  
  # Reactive data loading
  site_rainfall_data <- reactive({
    req(input$date_range)
    if (is.null(input$facility_filter) || input$facility_filter == "none") {
      return(data.frame())
    }
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    tryCatch({
      # ...existing code...
      # Determine date range based on selection
      if (input$rainfall_period == "week") {
        start_date <- Sys.Date() - 7
        end_date <- Sys.Date()
      } else if (input$rainfall_period == "month") {
        start_date <- Sys.Date() - 30
        end_date <- Sys.Date()
      } else {
        start_date <- input$date_range[1]
        end_date <- input$date_range[2]
      }
      # Build WHERE conditions
      where_conditions <- c()
      if (input$facility_filter != "none") {
        where_conditions <- c(where_conditions, sprintf("s.facility = '%s'", input$facility_filter))
      }
      # Wetland type multi-select
      wet_types <- input$wetland_type_filter
      if (!is.null(wet_types) && !("all" %in% wet_types)) {
        wet_types_sql <- paste(sprintf("'%s'", wet_types), collapse = ",")
        where_conditions <- c(where_conditions, sprintf("s.type IN (%s)", wet_types_sql))
      }
      # Priority multi-select
      priorities <- input$priority_filter
      if (!is.null(priorities) && !("all" %in% priorities)) {
        priorities_sql <- paste(sprintf("'%s'", priorities), collapse = ",")
        where_conditions <- c(where_conditions, sprintf("s.priority IN (%s)", priorities_sql))
      }
      where_clause <- if (length(where_conditions) > 0) {
        paste("AND", paste(where_conditions, collapse = " AND "))
      } else {
        ""
      }
      # ...existing code...
      # Query to get site data with rainfall
      query <- sprintf("
        SELECT 
          s.sitecode,
          s.acres,
          s.type,
          s.priority,
          s.facility,
          s.startdate,
          h.hrap_x,
          h.hrap_y,
          ST_X(ST_Centroid(ST_Transform(s.geom, 4326))) as longitude,
          ST_Y(ST_Centroid(ST_Transform(s.geom, 4326))) as latitude,
          COALESCE(AVG(r.rain_inches), 0) as avg_rainfall,
          COALESCE(SUM(r.rain_inches), 0) as total_rainfall,
          COUNT(r.date) as rain_days
        FROM loc_breeding_sites s
        LEFT JOIN breeding_site_hrap h ON s.sitecode = h.sitecode
        LEFT JOIN nws_precip_site_history r ON h.sitecode = r.sitecode 
          AND r.date >= '%s' 
          AND r.date <= '%s'
        WHERE s.geom IS NOT NULL
          AND s.startdate IS NOT NULL
          AND (s.enddate IS NULL OR s.enddate > CURRENT_DATE)
          %s
        GROUP BY s.sitecode, s.acres, s.type, s.priority, s.facility, s.startdate, h.hrap_x, h.hrap_y, 
                 ST_X(ST_Centroid(ST_Transform(s.geom, 4326))), ST_Y(ST_Centroid(ST_Transform(s.geom, 4326)))
        HAVING COALESCE(SUM(r.rain_inches), 0) >= %f
        ORDER BY total_rainfall DESC
      ", start_date, end_date, where_clause, input$min_rainfall)
      result <- dbGetQuery(con, query)
      dbDisconnect(con)
      if (nrow(result) > 0) {
        # ...existing code...
        # Add color coding based on rainfall
        result$color <- cut(result$total_rainfall,
          breaks = c(0, 0.25, 0.5, 1.0, 2.0, 3.5, 5.0, 7.0, Inf),
          labels = c("Very Low", "Low", "Moderate-Low", "Moderate", "Moderate-High", "High", "Very High", "Extreme"),
          include.lowest = TRUE
        )
        result$color_hex <- case_when(
          result$color == "Very Low" ~ "#006837",         # dark green
          result$color == "Low" ~ "#2ecc40",              # green
          result$color == "Moderate-Low" ~ "#b2df8a",     # yellow-green
          result$color == "Moderate" ~ "#ffff99",         # yellow
          result$color == "Moderate-High" ~ "#ffd700",    # gold
          result$color == "High" ~ "#ff9800",             # orange
          result$color == "Very High" ~ "#e41a1c",        # red
          result$color == "Extreme" ~ "#800026",          # dark red
          TRUE ~ "#cccccc"
        )
        return(result)
      } else {
        return(data.frame())
      }
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
      if (!is.null(con)) dbDisconnect(con)
      return(data.frame())
    })
  })
  
  # Rainfall map
  output$rainfall_map <- renderLeaflet({
    data <- site_rainfall_data()
    
    if (is.null(data) || nrow(data) == 0) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -93.2, lat = 44.9, zoom = 10) %>%
        addPopups(lng = -93.2, lat = 44.9, popup = "No data available for selected criteria")
    } else {
      
      # Create popup content
      popup_content <- sprintf(
        "<strong>Site:</strong> %s<br/>
         <strong>Type:</strong> %s<br/>
         <strong>Priority:</strong> %s<br/>
         <strong>Facility:</strong> %s<br/>
         <strong>Acres:</strong> %.2f<br/>
         <strong>Total Rainfall:</strong> %.2f inches<br/>
         <strong>Avg Rainfall:</strong> %.3f inches<br/>
         <strong>Rain Days:</strong> %.0f",
        data$sitecode, data$type, data$priority, data$facility,
        data$acres, data$total_rainfall, data$avg_rainfall, data$rain_days
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = ~total_rainfall * 2 + 4, 
          color = "white",
          fillColor = ~color_hex,
          fillOpacity = 0.8,
          weight = 1,
          popup = popup_content
        ) %>%
        addLegend(
          position = "bottomright",
          colors = c("#006837", "#2ecc40", "#b2df8a", "#ffff99", "#ffd700", "#ff9800", "#e41a1c", "#800026"),
          labels = c(
            "Very Low (0-0.25\")",
            "Low (0.25-0.5\")",
            "Moderate-Low (0.5-1\")",
            "Moderate (1-2\")",
            "Moderate-High (2-3.5\")",
            "High (3.5-5\")",
            "Very High (5-7\")",
            "Extreme (7\"+)"
          ),
          title = "Total Rainfall",
          opacity = 0.8
        )
    }
  })
  
  # Pipeline tab value boxes
  output$air_sites_with_rain <- renderValueBox({
    data <- air_sites_with_rain()
    value <- ifelse(is.null(data), 0, nrow(data))
    
    valueBox(
      value = value,
      subtitle = "Air Sites with Rain",
      icon = icon("plane"),
      color = "blue"
    )
  })
  
  output$sites_needing_inspection <- renderValueBox({
    air_data <- air_sites_with_rain()
    inspection_data <- inspection_data()
    
    if (is.null(inspection_data) || nrow(inspection_data) == 0) {
      value <- ifelse(is.null(air_data), 0, nrow(air_data))
    } else if ("inspected" %in% names(inspection_data)) {
      # Count sites that haven't been inspected
      value <- sum(!inspection_data$inspected, na.rm = TRUE)
    } else {
      # If inspected column doesn't exist, assume all need inspection
      value <- nrow(inspection_data)
    }
    
    valueBox(
      value = value,
      subtitle = "Need Inspection",
      icon = icon("search"),
      color = "orange"
    )
  })
  
  output$sites_inspected <- renderValueBox({
    data <- inspection_data()
    value <- ifelse(is.null(data), 0, sum(data$inspected, na.rm = TRUE))
    
    valueBox(
      value = value,
      subtitle = "Inspected",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$sites_needing_treatment <- renderValueBox({
    data <- inspection_data()
    value <- ifelse(is.null(data), 0, sum(data$needs_treatment, na.rm = TRUE))
    
    valueBox(
      value = value,
      subtitle = "Need Treatment",
      icon = icon("syringe"),
      color = "red"
    )
  })
  
  # Priority map
  output$priority_map <- renderLeaflet({
    data <- priority_data()
    
    if (is.null(data) || nrow(data) == 0) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -93.2, lat = 44.9, zoom = 10) %>%
        addPopups(lng = -93.2, lat = 44.9, popup = "No high priority sites found")
    } else {
      # Color by treatment status
      data$color <- case_when(
        !data$treated ~ "#e74c3c",  # Red for untreated
        data$treatment_response == "Timely" ~ "#27ae60",  # Green for timely
        data$treatment_response == "Delayed" ~ "#f39c12",  # Orange for delayed
        TRUE ~ "#95a5a6"  # Gray for other
      )
      
      popup_content <- sprintf(
        "<strong>Site:</strong> %s<br/>
         <strong>Priority:</strong> %s<br/>
         <strong>Facility:</strong> %s<br/>
         <strong>Acres:</strong> %.2f<br/>
         <strong>Total Rainfall:</strong> %.2f inches<br/>
         <strong>Treatment Status:</strong> %s<br/>
         %s",
        data$sitecode, data$priority, data$facility, data$acres, 
        data$total_rainfall, data$treatment_response,
        ifelse(data$treated, 
               sprintf("<strong>Days to Treatment:</strong> %d", data$days_to_treatment),
               "<strong>Action Required</strong>")
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = ~pmax(5, total_rainfall * 2),
          color = "white",
          fillColor = ~color,
          fillOpacity = 0.8,
          weight = 2,
          popup = popup_content
        ) %>%
        addLegend(
          position = "bottomright",
          colors = c("#e74c3c", "#f39c12", "#27ae60"),
          labels = c("Untreated", "Delayed Treatment", "Timely Treatment"),
          title = "Treatment Status",
          opacity = 0.8
        )
    }
  })
  
  # Priority summary table
  output$priority_summary_table <- DT::renderDataTable({
    data <- priority_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No data available"))
    }
    
    summary_stats <- data.frame(
      Metric = c("Total Sites", "Untreated", "Treated Timely", "Treated Late", "Avg Response (days)"),
      Value = c(
        nrow(data),
        sum(!data$treated),
        sum(data$treatment_response == "Timely", na.rm = TRUE),
        sum(data$treatment_response == "Delayed", na.rm = TRUE),
        round(mean(data$days_to_treatment, na.rm = TRUE), 1)
      )
    )
    
    datatable(summary_stats,
      options = list(
        pageLength = 5,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })
  
  # Priority table
  output$priority_table <- DT::renderDataTable({
    data <- priority_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No high priority sites found"))
    }
    
    display_data <- data %>%
      select(
        `Site Code` = sitecode,
        Priority = priority,
        Facility = facility,
        `Type` = type,
        `Acres` = acres,
        `Rainfall (inches)` = total_rainfall,
        `Rain Days` = rain_days,
        `Last Rain` = last_rain_date,
        `Treated` = treated,
        `Treatment Date` = treatment_date,
        `Days to Treatment` = days_to_treatment,
        `Response` = treatment_response
      ) %>%
      mutate(
        Acres = round(Acres, 2),
        `Rainfall (inches)` = round(`Rainfall (inches)`, 2)
      )
    
    datatable(display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(5, 'desc'))
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Priority",
        backgroundColor = styleEqual(
          c("RED", "YELLOW", "BLUE", "GREEN"),
          c("#ffebee", "#fff8e1", "#e3f2fd", "#e8f5e8")
        )
      ) %>%
      formatStyle("Response",
        backgroundColor = styleEqual(
          c("Not Treated", "Delayed", "Timely"),
          c("#ffcdd2", "#ffecb3", "#c8e6c9")
        )
      )
  })

  # Value boxes for original tabs
  output$total_sites <- renderValueBox({
    data <- site_rainfall_data()
    value <- ifelse(is.null(data), 0, nrow(data))
    
    valueBox(
      value = value,
      subtitle = "Breeding Sites",
      icon = icon("map-marker"),
      color = "blue"
    )
  })
  
  output$avg_rainfall <- renderValueBox({
    data <- site_rainfall_data()
    value <- ifelse(is.null(data) || nrow(data) == 0, 0, round(mean(data$total_rainfall, na.rm = TRUE), 2))
    
    valueBox(
      value = paste(value, "inches"),
      subtitle = "Average Rainfall",
      icon = icon("cloud-rain"),
      color = "aqua"
    )
  })
  
  # Rainfall trend plot
  output$rainfall_trend_plot <- renderPlot({
    data <- site_rainfall_data()
    
    if (is.null(data) || nrow(data) == 0) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No data available"), size = 6) +
        theme_void()
    } else {
      # Get daily rainfall data for trend
      con <- get_db_connection()
      if (!is.null(con)) {
        tryCatch({
          # Determine date range
          if (input$rainfall_period == "week") {
            start_date <- Sys.Date() - 7
            end_date <- Sys.Date()
          } else if (input$rainfall_period == "month") {
            start_date <- Sys.Date() - 30
            end_date <- Sys.Date()
          } else {
            start_date <- input$date_range[1]
            end_date <- input$date_range[2]
          }
          
          daily_rain <- dbGetQuery(con, sprintf("
            SELECT 
              date,
              AVG(rain_inches) as avg_rainfall,
              COUNT(DISTINCT sitecode) as sites_with_data
            FROM nws_precip_site_history 
            WHERE date >= '%s' AND date <= '%s'
            GROUP BY date 
            ORDER BY date
          ", start_date, end_date))
          
          dbDisconnect(con)
          
          if (nrow(daily_rain) > 0) {
            daily_rain$date <- as.Date(daily_rain$date)
            
            ggplot(daily_rain, aes(x = date, y = avg_rainfall)) +
              geom_line(color = "steelblue", size = 1) +
              geom_point(color = "steelblue", size = 2) +
              labs(
                title = "Average Daily Rainfall Across All Sites",
                x = "Date",
                y = "Average Rainfall (inches)"
              ) +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          } else {
            ggplot() + 
              geom_text(aes(x = 0, y = 0, label = "No rainfall data available"), size = 6) +
              theme_void()
          }
        }, error = function(e) {
          if (!is.null(con)) dbDisconnect(con)
          ggplot() + 
            geom_text(aes(x = 0, y = 0, label = "Error loading trend data"), size = 6) +
            theme_void()
        })
      } else {
        ggplot() + 
          geom_text(aes(x = 0, y = 0, label = "Database connection failed"), size = 6) +
          theme_void()
      }
    }
  })
  
  # Rainfall by location
  output$rainfall_by_location <- renderPlot({
    data <- site_rainfall_data()
    
    if (is.null(data) || nrow(data) == 0) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No data available"), size = 5) +
        theme_void()
    } else {
      # Extract location components from sitecode
      data$county <- substr(data$sitecode, 1, 2)
      data$city <- substr(data$sitecode, 3, 4)
      data$section <- substr(data$sitecode, 5, 6)
      
      # Filter and group based on selection
      if (input$location_level == "county") {
        location_data <- data %>%
          group_by(county) %>%
          summarise(
            avg_rainfall = mean(total_rainfall, na.rm = TRUE),
            site_count = n(),
            .groups = "drop"
          ) %>%
          arrange(desc(avg_rainfall)) %>%
          head(15)  # Limit to top 15 for readability
        
        plot_title <- "Average Rainfall by County"
        x_label <- "County"
        
      } else if (input$location_level == "city") {
        filtered_data <- data
        if (input$county_filter != "all") {
          filtered_data <- data %>% filter(county == input$county_filter)
        }
        
        location_data <- filtered_data %>%
          mutate(city_label = paste0(county, city)) %>%
          group_by(city_label) %>%
          summarise(
            avg_rainfall = mean(total_rainfall, na.rm = TRUE),
            site_count = n(),
            .groups = "drop"
          ) %>%
          arrange(desc(avg_rainfall)) %>%
          head(15)
        
        plot_title <- "Average Rainfall by City"
        x_label <- "City Code"
        
      } else {  # section level
        filtered_data <- data
        if (input$county_filter != "all") {
          filtered_data <- data %>% filter(county == input$county_filter)
        }
        if (input$city_filter != "all") {
          filtered_data <- filtered_data %>% filter(city == input$city_filter)
        }
        
        location_data <- filtered_data %>%
          mutate(section_label = paste0(county, city, section)) %>%
          group_by(section_label) %>%
          summarise(
            avg_rainfall = mean(total_rainfall, na.rm = TRUE),
            site_count = n(),
            .groups = "drop"
          ) %>%
          arrange(desc(avg_rainfall)) %>%
          head(15)
        
        plot_title <- "Average Rainfall by Section"
        x_label <- "Section Code"
      }
      
      if (nrow(location_data) > 0) {
        x_var <- names(location_data)[1]  # First column (county, city_label, or section_label)
        
        ggplot(location_data, aes_string(x = paste0("reorder(", x_var, ", avg_rainfall)"), y = "avg_rainfall")) +
          geom_col(fill = "lightblue", color = "steelblue") +
          coord_flip() +
          labs(
            title = plot_title,
            x = x_label,
            y = "Average Rainfall (inches)"
          ) +
          theme_minimal() +
          theme(axis.text.y = element_text(size = 8))
      } else {
        ggplot() + 
          geom_text(aes(x = 0, y = 0, label = "No data for selected filters"), size = 5) +
          theme_void()
      }
    }
  })
  
  # Rainfall by priority
  output$rainfall_by_priority <- renderPlot({
    data <- site_rainfall_data()
    
    if (is.null(data) || nrow(data) == 0) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No data available"), size = 5) +
        theme_void()
    } else {
      priority_summary <- data %>%
        group_by(priority) %>%
        summarise(
          avg_rainfall = mean(total_rainfall, na.rm = TRUE),
          site_count = n(),
          .groups = "drop"
        )
      
      # Define priority colors
      priority_colors <- c("RED" = "#e74c3c", "YELLOW" = "#f39c12", "BLUE" = "#3498db", "GREEN" = "#27ae60")
      
      ggplot(priority_summary, aes(x = priority, y = avg_rainfall, fill = priority)) +
        geom_col() +
        scale_fill_manual(values = priority_colors, name = "Priority") +
        labs(
          title = "Average Rainfall by Priority Level",
          x = "Priority",
          y = "Average Rainfall (inches)"
        ) +
        theme_minimal()
    }
  })
  
  # Treatment Analysis Tab Outputs
  # Treatment analysis data
  treatment_analysis_data <- reactive({
    req(input$pretend_today, input$treatment_threshold, input$max_response_days)
    
    end_date <- input$pretend_today
    start_date <- end_date - 90  # Look back 90 days for comprehensive analysis
    
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    
    tryCatch({
      # Get rainfall events above threshold
      rainfall_events <- dbGetQuery(con, sprintf("
        SELECT 
          r.sitecode,
          r.date as rain_date,
          r.rain_inches,
          s.facility,
          s.priority,
          s.acres
        FROM nws_precip_site_history r
        JOIN breeding_site_hrap h ON r.sitecode = h.sitecode
        JOIN loc_breeding_sites s ON h.sitecode = s.sitecode
        WHERE r.date >= '%s' 
          AND r.date <= '%s'
          AND r.rain_inches >= %f
          AND s.startdate IS NOT NULL
          AND (s.enddate IS NULL OR s.enddate > '%s')
        ORDER BY r.date, r.sitecode
      ", start_date, end_date, input$treatment_threshold, end_date))
      
      if (nrow(rainfall_events) == 0) {
        dbDisconnect(con)
        return(data.frame())
      }
      
      rainfall_events$rain_date <- as.Date(rainfall_events$rain_date)
      
      # Get treatments for analysis
      treatments <- get_treatment_data(start_date, end_date + 30, input$data_source)  # Extended range for treatments
      
      if (nrow(treatments) == 0) {
        rainfall_events$treated <- FALSE
        rainfall_events$treatment_date <- as.Date(NA)
        rainfall_events$response_days <- NA
        dbDisconnect(con)
        return(rainfall_events)
      }
      
      # Match treatments to rainfall events
      for (i in 1:nrow(rainfall_events)) {
        site_treatments <- treatments[
          treatments$sitecode == rainfall_events$sitecode[i] &
          treatments$inspdate > rainfall_events$rain_date[i] &
          treatments$inspdate <= (rainfall_events$rain_date[i] + input$max_response_days), 
        ]
        
        if (nrow(site_treatments) > 0) {
          earliest_treatment <- site_treatments[which.min(site_treatments$inspdate), ]
          rainfall_events$treated[i] <- TRUE
          rainfall_events$treatment_date[i] <- earliest_treatment$inspdate
          rainfall_events$response_days[i] <- as.numeric(earliest_treatment$inspdate - rainfall_events$rain_date[i])
        } else {
          rainfall_events$treated[i] <- FALSE
          rainfall_events$treatment_date[i] <- as.Date(NA)
          rainfall_events$response_days[i] <- NA
        }
      }
      
      # Filter by facility if specified
      if (input$treatment_facility != "all") {
        rainfall_events <- rainfall_events[rainfall_events$facility == input$treatment_facility, ]
      }
      
      dbDisconnect(con)
      return(rainfall_events)
      
    }, error = function(e) {
      if (!is.null(con)) dbDisconnect(con)
      showNotification(paste("Error in treatment analysis:", e$message), type = "error")
      return(data.frame())
    })
  })
  
  # Treatment value boxes
  output$total_rainfall_events <- renderValueBox({
    data <- treatment_analysis_data()
    value <- ifelse(is.null(data), 0, nrow(data))
    
    valueBox(
      value = value,
      subtitle = "Rainfall Events",
      icon = icon("cloud-rain"),
      color = "blue"
    )
  })
  
  output$sites_treated_after_rain <- renderValueBox({
    data <- treatment_analysis_data()
    value <- ifelse(is.null(data), 0, sum(data$treated, na.rm = TRUE))
    
    valueBox(
      value = value,
      subtitle = "Sites Treated",
      icon = icon("syringe"),
      color = "green"
    )
  })
  
  output$avg_treatment_response <- renderValueBox({
    data <- treatment_analysis_data()
    treated_data <- data[data$treated & !is.na(data$response_days), ]
    value <- ifelse(nrow(treated_data) == 0, "N/A",
                   paste(round(mean(treated_data$response_days), 1), "days"))
    
    valueBox(
      value = value,
      subtitle = "Avg Response Time",
      icon = icon("clock"),
      color = "orange"
    )
  })
  
  output$treatment_success_rate <- renderValueBox({
    data <- treatment_analysis_data()
    rate <- ifelse(nrow(data) == 0, 0, round(sum(data$treated, na.rm = TRUE) / nrow(data) * 100, 1))
    
    valueBox(
      value = paste0(rate, "%"),
      subtitle = "Treatment Rate",
      icon = icon("percentage"),
      color = "purple"
    )
  })
  
  # Treatment timeline plot
  output$treatment_timeline_plot <- renderPlotly({
    data <- treatment_analysis_data()
    
    if (is.null(data) || nrow(data) == 0) {
      p <- ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No data available"), size = 6) +
        theme_void()
      return(ggplotly(p))
    }
    
    # Create timeline data
    timeline_data <- data %>%
      filter(treated) %>%
      select(sitecode, rain_date, treatment_date, response_days, facility, rain_inches) %>%
      pivot_longer(cols = c(rain_date, treatment_date), names_to = "event_type", values_to = "date") %>%
      mutate(
        event_type = case_when(
          event_type == "rain_date" ~ "Rainfall",
          event_type == "treatment_date" ~ "Treatment"
        ),
        event_color = case_when(
          event_type == "Rainfall" ~ "blue",
          event_type == "Treatment" ~ "red"
        )
      )
    
    p <- ggplot(timeline_data, aes(x = date, y = sitecode)) +
      geom_line(aes(group = sitecode), color = "gray", alpha = 0.5) +
      geom_point(aes(color = event_type, size = rain_inches), alpha = 0.7) +
      scale_color_manual(values = c("Rainfall" = "blue", "Treatment" = "red")) +
      labs(
        title = "Rainfall to Treatment Timeline",
        x = "Date",
        y = "Site Code",
        color = "Event Type",
        size = "Rainfall (inches)"
      ) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 6))
    
    ggplotly(p, tooltip = c("x", "y", "colour", "size"))
  })
  
  # Response time distribution
  output$response_time_dist <- renderPlot({
    data <- treatment_analysis_data()
    treated_data <- data[data$treated & !is.na(data$response_days), ]
    
    if (nrow(treated_data) == 0) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No treatment data"), size = 5) +
        theme_void()
    } else {
      ggplot(treated_data, aes(x = response_days)) +
        geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.7) +
        geom_vline(xintercept = 7, color = "red", linetype = "dashed", size = 1) +
        labs(
          title = "Response Time Distribution",
          x = "Days to Treatment",
          y = "Count"
        ) +
        theme_minimal() +
        annotate("text", x = 7.5, y = Inf, label = "7-day target", 
                color = "red", vjust = 2, hjust = 0)
    }
  })
  
  # Treatment response table
  output$treatment_response_table <- DT::renderDataTable({
    data <- treatment_analysis_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No rainfall events found"))
    }
    
    display_data <- data %>%
      select(
        `Site Code` = sitecode,
        Facility = facility,
        Priority = priority,
        `Rain Date` = rain_date,
        `Rainfall (inches)` = rain_inches,
        `Treated` = treated,
        `Treatment Date` = treatment_date,
        `Response Days` = response_days
      ) %>%
      mutate(
        `Rainfall (inches)` = round(`Rainfall (inches)`, 2)
      )
    
    datatable(display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(3, 'desc'))
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Treated",
        backgroundColor = styleEqual(c(TRUE, FALSE), c("#c8e6c9", "#ffcdd2"))
      ) %>%
      formatStyle("Response Days",
        backgroundColor = styleInterval(c(3, 7), c("#c8e6c9", "#fff3e0", "#ffcdd2"))
      )
  })
  
  # Wetland Metrics Tab Outputs
  wetland_metrics_data <- reactive({
    req(input$metrics_rainfall_threshold, input$metrics_period)
    
    end_date <- input$pretend_today
    start_date <- end_date - as.numeric(input$metrics_period)
    
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    
    tryCatch({
      facility_condition <- if (input$metrics_facility == "all") {
        ""
      } else {
        sprintf("AND s.facility = '%s'", input$metrics_facility)
      }
      
      query <- sprintf("
        SELECT 
          s.sitecode,
          s.acres,
          s.type,
          s.priority,
          s.facility,
          COALESCE(SUM(r.rain_inches), 0) as total_rainfall,
          COUNT(CASE WHEN r.rain_inches > 0 THEN 1 END) as rain_days,
          CASE 
            WHEN COALESCE(SUM(r.rain_inches), 0) = 0 THEN 'Dry'
            WHEN COALESCE(SUM(r.rain_inches), 0) < %f THEN 'Under Threshold'
            ELSE 'Over Threshold'
          END as rainfall_status,
          CASE 
            WHEN COALESCE(SUM(r.rain_inches), 0) >= 3.0 THEN 'High Risk'
            WHEN COALESCE(SUM(r.rain_inches), 0) >= %f THEN 'Medium Risk'
            WHEN COALESCE(SUM(r.rain_inches), 0) > 0 THEN 'Low Risk'
            ELSE 'No Risk'
          END as risk_designation
        FROM loc_breeding_sites s
        LEFT JOIN breeding_site_hrap h ON s.sitecode = h.sitecode
        LEFT JOIN nws_precip_site_history r ON h.sitecode = r.sitecode 
          AND r.date >= '%s' 
          AND r.date <= '%s'
        WHERE s.geom IS NOT NULL
          AND s.startdate IS NOT NULL
          AND (s.enddate IS NULL OR s.enddate > '%s')
          %s
        GROUP BY s.sitecode, s.acres, s.type, s.priority, s.facility
        ORDER BY s.facility, s.priority, total_rainfall DESC
      ", input$metrics_rainfall_threshold, input$metrics_rainfall_threshold, 
         start_date, end_date, end_date, facility_condition)
      
      result <- dbGetQuery(con, query)
      dbDisconnect(con)
      return(result)
      
    }, error = function(e) {
      if (!is.null(con)) dbDisconnect(con)
      showNotification(paste("Error in metrics analysis:", e$message), type = "error")
      return(data.frame())
    })
  })
  
  # Threshold analysis table
  output$threshold_analysis_table <- DT::renderDataTable({
    data <- wetland_metrics_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No data available"))
    }
    
    summary_data <- data %>%
      group_by(rainfall_status) %>%
      summarise(
        Count = n(),
        `Total Acres` = round(sum(acres, na.rm = TRUE), 1),
        `Avg Acres` = round(mean(acres, na.rm = TRUE), 2),
        `Avg Rainfall` = round(mean(total_rainfall, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      arrange(desc(Count))
    
    datatable(summary_data,
      options = list(
        pageLength = 10,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE
    ) %>%
      formatStyle("rainfall_status",
        backgroundColor = styleEqual(
          c("Dry", "Under Threshold", "Over Threshold"),
          c("#f8f9fa", "#fff3cd", "#d1ecf1")
        )
      )
  })
  
  # Acreage status plot
  output$acreage_status_plot <- renderPlot({
    data <- wetland_metrics_data()
    
    if (is.null(data) || nrow(data) == 0) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No data available"), size = 5) +
        theme_void()
    } else {
      acreage_summary <- data %>%
        group_by(rainfall_status) %>%
        summarise(total_acres = sum(acres, na.rm = TRUE), .groups = "drop")
      
      ggplot(acreage_summary, aes(x = rainfall_status, y = total_acres, fill = rainfall_status)) +
        geom_col() +
        scale_fill_manual(
          values = c("Dry" = "#6c757d", "Under Threshold" = "#ffc107", "Over Threshold" = "#17a2b8")
        ) +
        labs(
          title = "Total Acreage by Rainfall Status",
          x = "Rainfall Status",
          y = "Total Acres"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    }
  })
  
  # Risk matrix table
  output$risk_matrix_table <- DT::renderDataTable({
    data <- wetland_metrics_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No data available"))
    }
    
    risk_summary <- data %>%
      group_by(priority, risk_designation) %>%
      summarise(
        Count = n(),
        `Total Acres` = round(sum(acres, na.rm = TRUE), 1),
        `Avg Rainfall` = round(mean(total_rainfall, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = risk_designation,
        values_from = c(Count, `Total Acres`, `Avg Rainfall`),
        values_fill = 0
      )
    
    datatable(risk_summary,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Risk distribution plot
  output$risk_distribution_plot <- renderPlot({
    data <- wetland_metrics_data()
    
    if (is.null(data) || nrow(data) == 0) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No data available"), size = 5) +
        theme_void()
    } else {
      risk_counts <- data %>%
        count(risk_designation) %>%
        mutate(percentage = n / sum(n) * 100)
      
      ggplot(risk_counts, aes(x = "", y = percentage, fill = risk_designation)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        scale_fill_manual(
          values = c("No Risk" = "#28a745", "Low Risk" = "#ffc107", 
                    "Medium Risk" = "#fd7e14", "High Risk" = "#dc3545")
        ) +
        labs(
          title = "Risk Distribution",
          fill = "Risk Level"
        ) +
        theme_void() +
        theme(legend.position = "bottom")
    }
  })

  # Site details table
  output$site_rainfall_table <- DT::renderDataTable({
    data <- site_rainfall_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No data available for selected criteria"))
    }
    
    display_data <- data %>%
      select(
        `Site Code` = sitecode,
        Facility = facility,
        `Wetland Type` = type,
        Priority = priority,
        `Acres` = acres,
        `Total Rainfall (inches)` = total_rainfall,
        `Average Rainfall (inches)` = avg_rainfall,
        `Rain Days` = rain_days
      ) %>%
      mutate(
        Acres = round(Acres, 2),
        `Total Rainfall (inches)` = round(`Total Rainfall (inches)`, 3),
        `Average Rainfall (inches)` = round(`Average Rainfall (inches)`, 3)
      )
    
    datatable(display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(5, 'desc'))  # Sort by total rainfall descending
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Priority",
        backgroundColor = styleEqual(
          c("RED", "YELLOW", "BLUE", "GREEN"),
          c("#ffebee", "#fff8e1", "#e3f2fd", "#e8f5e8")
        )
      ) %>%
      formatStyle("`Total Rainfall (inches)`",
        backgroundColor = styleInterval(
          c(0.5, 1.0, 2.0, 5.0),
          c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")
        ),
        color = styleInterval(c(2.0), c("black", "white"))
      )
  })
  
  # Air Site Pipeline Tab Outputs
  output$pipeline_map <- renderLeaflet({
    data <- inspection_data()
    if (is.null(data) || nrow(data) == 0) {
      return(leaflet() %>% addTiles())
    }
    
    # Create color palette based on inspection status
    pal <- colorFactor(
      palette = c("red", "green", "orange"),
      domain = c("Not Inspected", "Inspected", "Needs Treatment")
    )
    
    # Determine status for each site
    data$status <- ifelse(is.na(data$inspected) | !data$inspected, "Not Inspected",
                         ifelse(data$needs_treatment, "Needs Treatment", "Inspected"))
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        color = ~pal(status),
        popup = ~paste("Site:", sitecode, "<br>",
                      "Status:", status, "<br>",
                      "Last Rain:", last_rain_date, "<br>",
                      if (!is.na(inspection_date)) paste("Inspected:", inspection_date) else "Not Inspected"),
        radius = 8,
        fillOpacity = 0.8
      ) %>%
      addLegend("bottomright", pal = pal, values = ~status, title = "Site Status")
  })
  
  output$rainfall_summary_table <- DT::renderDataTable({
    data <- air_sites_with_rain()
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Message = "No air sites with recent rainfall")))
    }
    
    summary_data <- data %>%
      select(sitecode, facility, last_rain_date, total_rainfall) %>%
      arrange(desc(last_rain_date))
    
    datatable(summary_data, 
              options = list(pageLength = 5, scrollX = TRUE),
              caption = "Air Sites with Recent Rainfall")
  }, server = FALSE)
  
  output$inspection_summary_table <- DT::renderDataTable({
    data <- inspection_data()
    if (is.null(data) || nrow(data) == 0 || !"inspected" %in% names(data)) {
      return(datatable(data.frame(Message = "No inspection data available")))
    }
    
    summary_data <- data %>%
      filter(!inspected) %>%
      select(sitecode, facility, last_rain_date, days_since_rain = days_since_rain) %>%
      arrange(desc(days_since_rain))
    
    datatable(summary_data,
              options = list(pageLength = 5, scrollX = TRUE),
              caption = "Sites Needing Inspection")
  }, server = FALSE)
  
  output$treatment_queue_summary <- DT::renderDataTable({
    data <- treatment_queue_data()
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Message = "No sites need treatment")))
    }
    
    summary_data <- data %>%
      select(sitecode, facility, inspection_date, numdip) %>%
      arrange(desc(numdip))
    
    datatable(summary_data,
              options = list(pageLength = 5, scrollX = TRUE),
              caption = "Sites Needing Treatment")
  }, server = FALSE)
  
  output$pipeline_table <- DT::renderDataTable({
    data <- inspection_data()
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Message = "No data available")))
    }
    
    # Show all sites with their status
    display_data <- data %>%
      mutate(
        Status = case_when(
          is.na(inspected) | !inspected ~ "Needs Inspection",
          needs_treatment ~ "Needs Treatment", 
          TRUE ~ "Inspected"
        ),
        `Days Since Rain` = days_since_rain,
        `Inspection Date` = as.character(inspection_date),
        `Dip Count` = numdip
      ) %>%
      select(
        `Site Code` = sitecode,
        Facility = facility,
        Status,
        `Last Rain` = last_rain_date,
        `Days Since Rain`,
        `Inspection Date`,
        `Dip Count`
      ) %>%
      arrange(desc(`Days Since Rain`))
    
    datatable(display_data,
              options = list(pageLength = 10, scrollX = TRUE),
              caption = "All Air Sites Pipeline Status") %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(
                    c("Needs Inspection", "Needs Treatment", "Inspected"),
                    c("#ffcccc", "#fff2cc", "#ccffcc")
                  ))
  }, server = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)