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
      menuItem("Treatment Queue", tabName = "treatment_queue", icon = icon("list-alt"))
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
                    "Last 24 hours" = 1,
                    "Last 48 hours" = 2,
                    "Last 72 hours" = 3,
                    "Last 4 days" = 4,
                    "Last 5 days" = 5,
                    "Last 7 days" = 7
                  ),
                  selected = 3,
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
                selectInput("status_filter", "Site Status:",
                  choices = c("All Statuses" = "all",
                             "Unknown" = "U",
                             "Needs Inspection" = "Needs Inspection",
                             "Isp (under threshold)" = "Isp (under threshold)",
                             "Needs Treatment" = "Needs Treatment",
                             "Active Treatment" = "Active Treatment"),
                  selected = "all"
                )
              ),
              column(2,
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
          valueBoxOutput("sites_active_treatment", width = 3),
          valueBoxOutput("sites_unknown_status", width = 3),
          valueBoxOutput("treatment_effectiveness", width = 3),
          valueBoxOutput("pipeline_completion", width = 3)
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
  
  # Air site pipeline data - Step 1: ALL air sites with rainfall data
  all_air_sites <- reactive({
    req(input$pretend_today, input$rain_threshold, input$lookback_period)
    
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
      
      # Get ALL AIR sites with their rainfall data (not filtered by threshold)
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
        ORDER BY s.priority, total_rainfall DESC
      ", start_date, end_date, end_date, priority_condition, facility_condition, prehatch_condition)
      
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
  
  # Step 2: Get ALL air sites and calculate their status
  inspection_data <- reactive({
    # Get ALL air sites (not just those with rain)
    air_sites <- all_air_sites()
    
    req(input$pretend_today)
    end_date <- input$pretend_today
    
    if (is.null(air_sites) || nrow(air_sites) == 0) return(data.frame())
    
    con <- get_db_connection()
    if (is.null(con)) return(data.frame())
    
    tryCatch({
      # Get ALL air sites first (not just those with rain)
      facility_condition <- if (input$facility_filter == "all") {
        ""
      } else {
        sprintf("AND s.facility = '%s'", input$facility_filter)
      }
      
      # Query to get all air sites with their basic info and rainfall data
      all_air_sites_query <- sprintf("
        WITH rain_data AS (
          SELECT 
            s.sitecode,
            s.facility,
            s.acres,
            s.type,
            s.priority,
            s.air_gnd,
            s.prehatch,
            ST_X(ST_Centroid(ST_Transform(s.geom, 4326))) as longitude,
            ST_Y(ST_Centroid(ST_Transform(s.geom, 4326))) as latitude,
            COALESCE(SUM(r.rain_inches), 0) as total_rainfall,
            COUNT(CASE WHEN r.rain_inches >= %f THEN 1 END) as rain_days,
            MAX(r.date) as last_rain_date,
            (%s - MAX(r.date)) as days_since_rain
          FROM loc_breeding_sites s
          LEFT JOIN nws_precip_site_history r ON s.rain_site = r.station
            AND r.date >= '%s' - INTERVAL '%d days'
            AND r.date <= '%s'
          WHERE s.air_gnd = 'A'
            %s
          GROUP BY s.sitecode, s.facility, s.acres, s.type, s.priority, s.air_gnd, s.prehatch, s.geom
        )
        SELECT * FROM rain_data
        ORDER BY facility, sitecode",
        input$rain_threshold,
        end_date,
        end_date,
        as.numeric(input$lookback_period),
        end_date,
        facility_condition
      )
      
      air_sites <- dbGetQuery(con, all_air_sites_query)
    
      if (nrow(air_sites) == 0) {
        dbDisconnect(con)
        return(data.frame())
      }
      
      # Get material effectiveness data  
      mattype_query <- "SELECT mattype, effect_days FROM mattype_list WHERE effect_days IS NOT NULL"
      mattype_data <- dbGetQuery(con, mattype_query)
      # Choose table based on data source
      table_name <- if (input$data_source == "archive") "public.dblarv_insptrt_archive" else "public.dblarv_insptrt_current"
      
      # Get sites that need inspection (from air sites with rain)
      site_list <- paste(sprintf("'%s'", air_sites$sitecode), collapse = ",")
      
      # Get both inspections and treatments from last 30 days
      activity_start_date <- end_date - 30  # Look back 30 days for all activities
      query <- sprintf("
        SELECT 
          t.sitecode,
          t.inspdate,
          t.facility,
          t.action,
          t.numdip,
          t.wet,
          t.matcode,
          CASE 
            WHEN t.action IN ('2', '4') THEN 'inspection'
            WHEN t.action = '1' AND (t.matcode IS NULL OR t.matcode = '') THEN 'inspection'
            WHEN t.action = 'A' THEN 'treatment'
            WHEN t.action IN ('3', '1') AND t.matcode IS NOT NULL AND t.matcode != '' THEN 'treatment'
            ELSE 'other'
          END as activity_type
        FROM %s t
        WHERE t.sitecode IN (%s)
          AND t.inspdate >= '%s' 
          AND t.inspdate <= '%s'
          AND t.action IN ('1', '2', '4', 'A', '3')
        ORDER BY t.sitecode, t.inspdate DESC
      ", table_name, site_list, activity_start_date, end_date)
      
      activities <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      if (nrow(activities) > 0) {
        activities$inspdate <- as.Date(activities$inspdate)
        
        # Proper status lifecycle management
        # Status flow: U -> Needs Inspection -> Isp(under)/Needs Treatment -> Active Treatment -> U(expired)
        
        # Get material effectiveness data for treatment duration
        mat_query <- "SELECT matcode, mattype, effect_days FROM public.mattype_list WHERE effect_days IS NOT NULL"
        mat_data <- tryCatch({
          con_mat <- get_db_connection()
          if (!is.null(con_mat)) {
            result <- dbGetQuery(con_mat, mat_query)
            dbDisconnect(con_mat)
            result
          } else {
            data.frame(matcode = character(0), mattype = character(0), effect_days = numeric(0))
          }
        }, error = function(e) {
          data.frame(matcode = character(0), mattype = character(0), effect_days = numeric(0))
        })
        
        for (i in 1:nrow(air_sites)) {
          site_activities <- activities[activities$sitecode == air_sites$sitecode[i], ]
          current_date <- input$pretend_today
          rain_date <- air_sites$last_rain_date[i]
          
          # Initialize site data
          air_sites$inspected[i] <- FALSE
          air_sites$inspection_date[i] <- as.Date(NA)
          air_sites$days_to_inspection[i] <- NA
          air_sites$numdip[i] <- NA
          air_sites$wet_status[i] <- NA
          air_sites$needs_treatment[i] <- FALSE
          air_sites$site_status[i] <- "U" # Default to Unknown
          air_sites$treatment_status[i] <- "None"
          air_sites$days_since_treatment[i] <- NA
          
          if (nrow(site_activities) > 0) {
            # Get most recent inspection and treatment
            inspections <- site_activities[site_activities$activity_type == 'inspection', ]
            treatments <- site_activities[site_activities$activity_type == 'treatment', ]
            
            latest_inspection <- NULL
            latest_treatment <- NULL
            
            if (nrow(inspections) > 0) {
              latest_inspection <- inspections[which.max(inspections$inspdate), ]
            }
            if (nrow(treatments) > 0) {
              latest_treatment <- treatments[which.max(treatments$inspdate), ]
            }
            
            # Determine site status based on timeline and activities
            
            # Step 1: Check if site has active treatment (within effectiveness period)
            if (!is.null(latest_treatment)) {
              treatment_date <- latest_treatment$inspdate
              matcode <- latest_treatment$matcode
              
              # Get effectiveness period for this material
              effect_days <- 30 # Default
              if (nrow(mat_data) > 0 && matcode %in% mat_data$matcode) {
                effect_days <- mat_data$effect_days[mat_data$matcode == matcode][1]
                if (is.na(effect_days)) effect_days <- 30
              }
              
              days_since_treatment <- as.numeric(current_date - treatment_date)
              air_sites$days_since_treatment[i] <- days_since_treatment
              
              # Check if treatment is still active
              if (days_since_treatment <= effect_days) {
                # Check if there's been significant rainfall since treatment
                if (!is.na(rain_date) && rain_date > treatment_date) {
                  # Rain after treatment - reset to needs inspection
                  air_sites$site_status[i] <- "Needs Inspection"
                  air_sites$treatment_status[i] <- "Expired (rainfall)"
                } else {
                  # Treatment still active
                  air_sites$site_status[i] <- "Active Treatment"
                  air_sites$treatment_status[i] <- paste("Active", effect_days - days_since_treatment, "days left")
                }
              } else {
                # Treatment expired
                if (!is.na(rain_date) && rain_date > treatment_date) {
                  # Rain after expired treatment
                  air_sites$site_status[i] <- "Needs Inspection"
                  air_sites$treatment_status[i] <- "Expired"
                } else {
                  # No rain after expired treatment
                  air_sites$site_status[i] <- "U"
                  air_sites$treatment_status[i] <- "Expired"
                }
              }
            }
            
            # Step 2: If not in active treatment, check inspection status
            if (air_sites$site_status[i] %in% c("U", "Needs Inspection", "Isp (under threshold)")) {
              if (!is.null(latest_inspection)) {
                inspection_date <- latest_inspection$inspdate
                air_sites$inspection_date[i] <- inspection_date
                air_sites$numdip[i] <- latest_inspection$numdip
                air_sites$wet_status[i] <- latest_inspection$wet
                
                # Special case: If site was "Isp (under threshold)" but there's new qualifying rainfall after the last inspection, 
                # it should change to "Needs Inspection"
                if (air_sites$site_status[i] == "Isp (under threshold)" && 
                    !is.na(rain_date) && inspection_date < rain_date) {
                  air_sites$site_status[i] <- "Needs Inspection"
                }
                
                # Check if inspection is after most recent qualifying rainfall
                if (!is.na(rain_date) && inspection_date >= rain_date) {
                  # Site was inspected after rainfall
                  air_sites$inspected[i] <- TRUE
                  air_sites$days_to_inspection[i] <- as.numeric(inspection_date - rain_date)
                  
                  # Determine status based on dip count
                  if (!is.na(latest_inspection$numdip)) {
                    if (latest_inspection$numdip >= input$dip_threshold) {
                      # Above threshold - check if treated since inspection
                      treated_since_inspection <- FALSE
                      if (!is.null(latest_treatment) && latest_treatment$inspdate > inspection_date) {
                        treated_since_inspection <- TRUE
                      }
                      
                      if (!treated_since_inspection) {
                        air_sites$site_status[i] <- "Needs Treatment"
                        air_sites$needs_treatment[i] <- TRUE
                      }
                    } else {
                      # Below threshold
                      air_sites$site_status[i] <- "Isp (under threshold)"
                    }
                  }
                } else {
                  # Inspection before rainfall - needs new inspection
                  air_sites$site_status[i] <- "Needs Inspection"
                }
              } else {
                # No inspection found
                if (!is.na(rain_date)) {
                  air_sites$site_status[i] <- "Needs Inspection"
                } else {
                  air_sites$site_status[i] <- "U"
                }
              }
            }
          } else {
            # No activities found
            if (!is.na(rain_date)) {
              air_sites$site_status[i] <- "Needs Inspection"
            } else {
              air_sites$site_status[i] <- "U"
            }
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
          AND t.action IN ('A', '3', '1')
          AND t.matcode IS NOT NULL 
          AND t.matcode != ''
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
    data <- all_air_sites()
    value <- ifelse(is.null(data), 0, nrow(data))
    
    valueBox(
      value = value,
      subtitle = "Air Sites with Rain",
      icon = icon("plane"),
      color = "blue"
    )
  })
  
  output$sites_needing_inspection <- renderValueBox({
    inspection_data <- inspection_data()
    
    if (is.null(inspection_data) || nrow(inspection_data) == 0) {
      value <- 0
    } else if ("site_status" %in% names(inspection_data)) {
      # Count sites with "Needs Inspection" status
      value <- sum(inspection_data$site_status == "Needs Inspection", na.rm = TRUE)
    } else if ("inspected" %in% names(inspection_data)) {
      # Fallback to old logic
      value <- sum(!inspection_data$inspected, na.rm = TRUE)
    } else {
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
    if (is.null(data) || nrow(data) == 0) {
      value <- 0
    } else if ("site_status" %in% names(data)) {
      # Count sites with "Isp (under threshold)" status
      value <- sum(data$site_status == "Isp (under threshold)", na.rm = TRUE)
    } else {
      # Fallback to old logic
      value <- sum(data$inspected, na.rm = TRUE)
    }
    
    valueBox(
      value = value,
      subtitle = "Isp (under threshold)",
      icon = icon("check-circle"),
      color = "red"
    )
  })
  
  output$sites_needing_treatment <- renderValueBox({
    data <- inspection_data()
    if (is.null(data) || nrow(data) == 0) {
      value <- 0
    } else if ("site_status" %in% names(data)) {
      # Count sites with "Needs Treatment" status
      value <- sum(data$site_status == "Needs Treatment", na.rm = TRUE)
    } else {
      # Fallback to old logic
      value <- sum(data$needs_treatment, na.rm = TRUE)
    }
    
    valueBox(
      value = value,
      subtitle = "Need Treatment",
      icon = icon("syringe"),
      color = "green"
    )
  })
  
  output$sites_active_treatment <- renderValueBox({
    data <- inspection_data()
    if (is.null(data) || nrow(data) == 0) {
      value <- 0
    } else if ("site_status" %in% names(data)) {
      value <- sum(data$site_status == "Active Treatment", na.rm = TRUE)
    } else {
      value <- 0
    }
    
    valueBox(
      value = value,
      subtitle = "Active Treatment",
      icon = icon("shield-alt"),
      color = "green"
    )
  })
  
  output$sites_unknown_status <- renderValueBox({
    data <- inspection_data()
    if (is.null(data) || nrow(data) == 0) {
      value <- 0
    } else if ("site_status" %in% names(data)) {
      value <- sum(data$site_status == "U", na.rm = TRUE)
    } else {
      value <- 0
    }
    
    valueBox(
      value = value,
      subtitle = "Unknown Status",
      icon = icon("question"),
      color = "black"
    )
  })
  
  output$treatment_effectiveness <- renderValueBox({
    data <- inspection_data()
    if (is.null(data) || nrow(data) == 0) {
      value <- "N/A"
    } else if ("site_status" %in% names(data)) {
      treated <- sum(data$site_status == "Active Treatment", na.rm = TRUE)
      needed <- sum(data$site_status == "Needs Treatment", na.rm = TRUE)
      total_needing_treatment <- treated + needed
      if (total_needing_treatment > 0) {
        rate <- round(treated / total_needing_treatment * 100, 1)
        value <- paste0(rate, "%")
      } else {
        value <- "100%"
      }
    } else {
      value <- "N/A"
    }
    
    valueBox(
      value = value,
      subtitle = "Treatment Rate",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  output$pipeline_completion <- renderValueBox({
    data <- inspection_data()
    if (is.null(data) || nrow(data) == 0) {
      value <- "N/A"
    } else if ("site_status" %in% names(data)) {
      completed <- sum(data$site_status %in% c("Isp (under threshold)", "Active Treatment"), na.rm = TRUE)
      total <- nrow(data)
      if (total > 0) {
        rate <- round(completed / total * 100, 1)
        value <- paste0(rate, "%")
      } else {
        value <- "0%"
      }
    } else {
      value <- "N/A"
    }
    
    valueBox(
      value = value,
      subtitle = "Pipeline Complete",
      icon = icon("check-double"),
      color = "blue"
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
    
    # Apply status filter
    if (!is.null(input$status_filter) && input$status_filter != "all") {
      if ("site_status" %in% names(data)) {
        data <- data[data$site_status == input$status_filter, ]
      }
    }
    
    # Check if required columns exist
    required_cols <- c("longitude", "latitude", "sitecode")
    if (!all(required_cols %in% names(data))) {
      showNotification("Map data missing required coordinates", type = "warning")
      return(leaflet() %>% addTiles())
    }
    
    # Filter out rows with missing coordinates
    data <- data[!is.na(data$longitude) & !is.na(data$latitude), ]
    if (nrow(data) == 0) {
      return(leaflet() %>% addTiles())
    }
    
    # Use the new site_status column or fall back to old logic
    if ("site_status" %in% names(data)) {
      data$status <- data$site_status
    } else {
      # Fallback to old logic if site_status not available
      data$status <- "Not Inspected"
      if ("inspected" %in% names(data)) {
        inspected_status <- !is.na(data$inspected) & data$inspected
        data$status[inspected_status] <- "Isp (under threshold)"
        
        if ("needs_treatment" %in% names(data)) {
          treatment_needed <- !is.na(data$needs_treatment) & data$needs_treatment
          data$status[inspected_status & treatment_needed] <- "Needs Treatment"
        }
      }
    }
    
    # Create color palette for all possible statuses
    all_statuses <- c("U", "Needs Inspection", "Isp (under threshold)", "Needs Treatment", "Active Treatment")
    pal <- colorFactor(
      palette = c("gray", "red", "yellow", "orange", "green"),
      domain = all_statuses
    )
    
    # Create popup text safely
    popup_text <- paste(
      "Site:", data$sitecode, "<br>",
      "Status:", data$status, "<br>",
      "Last Rain:", ifelse(!is.na(data$last_rain_date), as.character(data$last_rain_date), "Unknown"), "<br>",
      "Rainfall Amount:", ifelse("total_rainfall" %in% names(data) & !is.na(data$total_rainfall), 
                                paste(data$total_rainfall, "inches"), "Unknown"), "<br>",
      ifelse("inspection_date" %in% names(data) & !is.na(data$inspection_date), 
             paste("Inspected:", as.character(data$inspection_date)), 
             "Not Inspected")
    )
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        color = ~pal(status),
        popup = popup_text,
        radius = 8,
        fillOpacity = 0.8
      ) %>%
      addLegend("bottomright", pal = pal, values = ~status, title = "Site Status")
  })
  
  output$rainfall_summary_table <- DT::renderDataTable({
    data <- all_air_sites()
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
        Status = site_status,
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
                    c("Needs Inspection", "Needs Treatment", "Isp (under threshold)", "Active Treatment", "U"),
                    c("#ffcccc", "#ccffcc", "#ccffcc", "#ccffcc", "#f0f0f0")
                  ))
  }, server = FALSE)
  
  # Inspection Status Tab Outputs
  output$total_inspections_needed <- renderValueBox({
    data <- inspection_data()
    if (is.null(data)) {
      value <- 0
    } else {
      # Count sites that need inspection (not yet inspected or need re-inspection)
      value <- sum(!data$inspected, na.rm = TRUE)
    }
    
    valueBox(
      value = value,
      subtitle = "Total Inspections Needed",
      icon = icon("list-check"),
      color = "blue"
    )
  })
  
  output$inspections_completed <- renderValueBox({
    data <- inspection_data()
    if (is.null(data)) {
      value <- 0
    } else {
      value <- sum(data$inspected, na.rm = TRUE)
    }
    
    valueBox(
      value = value,
      subtitle = "Inspections Completed",
      icon = icon("check"),
      color = "green"
    )
  })
  
  output$inspections_overdue <- renderValueBox({
    data <- inspection_data()
    if (is.null(data) || !"days_since_rain" %in% names(data)) {
      value <- 0
    } else {
      overdue_threshold <- input$inspection_days_threshold
      value <- sum(!data$inspected & data$days_since_rain > overdue_threshold, na.rm = TRUE)
    }
    
    valueBox(
      value = value,
      subtitle = "Overdue Inspections",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$inspection_completion_rate <- renderValueBox({
    data <- inspection_data()
    if (is.null(data) || nrow(data) == 0) {
      value <- "0%"
    } else {
      completed <- sum(data$inspected, na.rm = TRUE)
      total <- nrow(data)
      rate <- ifelse(total > 0, round(completed / total * 100, 1), 0)
      value <- paste0(rate, "%")
    }
    
    valueBox(
      value = value,
      subtitle = "Completion Rate",
      icon = icon("percent"),
      color = "purple"
    )
  })
  
  output$inspection_status_plot <- renderPlotly({
    data <- inspection_data()
    if (is.null(data) || nrow(data) == 0) {
      p <- ggplot() + 
        geom_text(aes(x = 1, y = 1, label = "No data available"), size = 5) +
        theme_void()
      return(ggplotly(p))
    }
    
    # Summarize by facility
    summary_data <- data %>%
      group_by(facility) %>%
      summarise(
        total_sites = n(),
        inspected = sum(inspected, na.rm = TRUE),
        needs_inspection = sum(!inspected, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = c(inspected, needs_inspection),
                  names_to = "status",
                  values_to = "count") %>%
      mutate(status = case_when(
        status == "inspected" ~ "Inspected",
        status == "needs_inspection" ~ "Needs Inspection"
      ))
    
    p <- ggplot(summary_data, aes(x = facility, y = count, fill = status)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Inspected" = "#28a745", "Needs Inspection" = "#dc3545")) +
      labs(title = "Inspection Status by Facility",
           x = "Facility",
           y = "Number of Sites",
           fill = "Status") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$inspection_timeline <- renderPlot({
    data <- inspection_data()
    if (is.null(data) || nrow(data) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No data available", cex = 1.5)
      return()
    }
    
    # Create timeline data
    if ("days_since_rain" %in% names(data)) {
      timeline_data <- data %>%
        filter(!is.na(days_since_rain)) %>%
        mutate(
          days_category = case_when(
            days_since_rain <= 1 ~ "0-1 days",
            days_since_rain <= 3 ~ "2-3 days", 
            days_since_rain <= 7 ~ "4-7 days",
            TRUE ~ ">7 days"
          )
        ) %>%
        count(days_category, inspected) %>%
        mutate(status = ifelse(inspected, "Inspected", "Not Inspected"))
      
      ggplot(timeline_data, aes(x = days_category, y = n, fill = status)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c("Inspected" = "#28a745", "Not Inspected" = "#dc3545")) +
        labs(title = "Days Since Rainfall",
             x = "Days Since Rain",
             y = "Number of Sites",
             fill = "Status") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$inspection_status_table <- DT::renderDataTable({
    data <- inspection_data()
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Message = "No inspection data available")))
    }
    
    # Filter based on checkbox
    display_data <- data
    if (input$show_overdue_only && "days_since_rain" %in% names(data)) {
      overdue_threshold <- input$inspection_days_threshold
      display_data <- data[!data$inspected & data$days_since_rain > overdue_threshold, ]
    }
    
    # Format for display
    if (nrow(display_data) > 0) {
      display_data <- display_data %>%
        mutate(
          Status = ifelse(inspected, "Inspected", "Needs Inspection"),
          `Days Since Rain` = ifelse(is.na(days_since_rain), "", as.character(days_since_rain)),
          `Inspection Date` = ifelse(is.na(inspection_date), "", as.character(inspection_date)),
          `Dip Count` = ifelse(is.na(numdip), "", as.character(numdip))
        ) %>%
        select(
          `Site Code` = sitecode,
          Facility = facility,
          Status,
          `Days Since Rain`,
          `Inspection Date`,
          `Dip Count`,
          Priority = priority
        ) %>%
        arrange(desc(`Days Since Rain`))
      
      datatable(display_data,
                options = list(pageLength = 15, scrollX = TRUE),
                caption = ifelse(input$show_overdue_only, 
                                "Overdue Inspections", 
                                "All Sites Needing Inspection")) %>%
        formatStyle("Status",
                    backgroundColor = styleEqual(
                      c("Needs Inspection", "Inspected"),
                      c("#ffcccc", "#ccffcc")
                    ))
    } else {
      datatable(data.frame(Message = "No sites match the current filters"))
    }
  }, server = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)