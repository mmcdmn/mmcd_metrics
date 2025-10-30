# Air Work Pipeline Application
# Based on requirements: Track all air sites with proper status lifecycle

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(leaflet)
  library(DT)
  library(plotly)
  library(tidyr)
})

# Source the shared database helper functions
suppressWarnings({
  source("../../shared/db_helpers.R")
})

# Database connection is now handled by db_helpers.R
# No need for manual database configuration - using shared get_db_connection()

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Air Work Pipeline"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Air Site Status", tabName = "status", icon = icon("helicopter")),
      menuItem("Flow Testing", tabName = "testing", icon = icon("flask"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "status",
        fluidRow(
          box(title = "Controls", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(3,
                dateInput("analysis_date", "Analysis Date:",
                  value = Sys.Date(),
                  max = Sys.Date()
                )
              ),
              column(3,
                selectInput("lookback_period", "Rainfall Lookback Period:",
                  choices = list(
                    "24 hours" = 1,
                    "48 hours" = 2, 
                    "72 hours" = 3,
                    "4 days" = 4
                  ),
                  selected = 3
                )
              ),
              column(3,
                numericInput("rain_threshold", "Rain Threshold (inches):",
                  value = 1.0,
                  min = 0.1,
                  max = 5.0,
                  step = 0.1
                )
              ),
              column(3,
                selectInput("facility_filter", "Facility:",
                  choices = c("All Facilities" = "all"),
                  selected = "all"
                )
              )
            ),
            fluidRow(
              column(3,
                numericInput("treatment_threshold", "Treatment Threshold (larvae count):",
                  value = 1,
                  min = 0,
                  max = 100,
                  step = 1
                )
              ),
              column(9, "")
            ),
            fluidRow(
              column(4,
                selectInput("status_filter", "Status Filter:",
                  choices = c("All Statuses" = "all",
                             "Unknown" = "Unknown",
                             "Needs Inspection" = "Needs Inspection", 
                             "Under Threshold" = "Under Threshold",
                             "Needs Treatment" = "Needs Treatment"),
                  selected = "all"
                )
              ),
              column(4,
                selectInput("priority_filter", "Priority:",
                  choices = c("All Priorities" = "all", "RED" = "RED"),
                  selected = "RED"
                )
              ),
              column(4,
                checkboxGroupInput("zone_filter", "Filter by Zone:",
                  choices = c("P1" = "1", "P2" = "2"),
                  selected = c("1", "2"))
              )
            ),
            fluidRow(
              column(4,
                actionButton("refresh_data", "Refresh Data", class = "btn-primary")
              ),
              column(8, "")
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("total_air_sites", width = 3),
          valueBoxOutput("sites_needs_inspection", width = 3),
          valueBoxOutput("sites_under_threshold", width = 3),
          valueBoxOutput("sites_needs_treatment", width = 3)
        ),
        
        fluidRow(
          box(title = "Air Site Status Map", status = "primary", solidHeader = TRUE, width = 8,
            leafletOutput("status_map", height = "500px")
          ),
          box(title = "Status Summary", status = "info", solidHeader = TRUE, width = 4,
            plotlyOutput("status_chart", height = "500px")
          )
        ),
        
        fluidRow(
          box(title = "Site Details", status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("site_details_table")
          )
        )
      ),
      
      tabItem(tabName = "testing",
        fluidRow(
          box(title = "Pipeline Flow Testing", status = "primary", solidHeader = TRUE, width = 12,
            h4("Test the Rainfall → Inspection → Treatment Pipeline"),
            p("This tool helps verify the flow logic by showing status transitions over time."),
            
            fluidRow(
              column(3,
                dateInput("test_start_date", "Test Start Date:",
                  value = Sys.Date() - 7,
                  max = Sys.Date()
                )
              ),
              column(3,
                dateInput("test_end_date", "Test End Date:",
                  value = Sys.Date(),
                  max = Sys.Date()
                )
              ),
              column(3,
                selectInput("test_data_type", "Data Type:",
                  choices = list(
                    "Real Database Data" = "real",
                    "Synthetic Test Data" = "synthetic"
                  ),
                  selected = "real"
                )
              ),
              column(3,
                actionButton("run_flow_test", "Run Flow Test", class = "btn-success")
              )
            ),
            
            conditionalPanel(
              condition = "input.test_data_type == 'synthetic'",
              fluidRow(
                column(12,
                  h5("Synthetic Data Parameters:"),
                  fluidRow(
                    column(3,
                      numericInput("synth_total_sites", "Total Sites:", value = 100, min = 10, max = 1000)
                    ),
                    column(3,
                      numericInput("synth_rain_pct", "% Sites with Rain:", value = 60, min = 0, max = 100)
                    ),
                    column(3,
                      numericInput("synth_inspect_pct", "% Rain Sites Inspected:", value = 80, min = 0, max = 100)
                    ),
                    column(3,
                      numericInput("synth_above_thresh_pct", "% Above Threshold:", value = 30, min = 0, max = 100)
                    )
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(title = "Flow Test Results", status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("flow_test_results")
          )
        ),
        
        fluidRow(
          box(title = "Daily Status Counts", status = "success", solidHeader = TRUE, width = 6,
            plotlyOutput("daily_counts_chart")
          ),
          box(title = "Status Summary", status = "warning", solidHeader = TRUE, width = 6,
            verbatimTextOutput("flow_summary")
          )
        ),
        
        fluidRow(
          box(title = "Data Validation", status = "danger", solidHeader = TRUE, width = 12,
            verbatimTextOutput("validation_summary")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
   source_colors <- get_status_colors()
  shiny_colors <- get_shiny_colors()
  
  # Load facility choices
  observe({
    tryCatch({
      # Use the shared function to get facility choices
      fac_choices <- get_facility_choices(include_all = TRUE)
      updateSelectInput(session, "facility_filter", choices = fac_choices)
    }, error = function(e) {
      showNotification(paste("Error loading facilities:", e$message), type = "error")
    })
  })
  
  # Main data reactive - gets all air sites with calculated status
  air_sites_data <- reactive({
    input$refresh_data  # Trigger on refresh button
    req(input$analysis_date, input$lookback_period, input$rain_threshold, input$treatment_threshold)
    
    con <- get_db_connection()
    if (is.null(con)) return(data.frame())
    
    tryCatch({
      analysis_date <- input$analysis_date
      lookback_days <- as.numeric(input$lookback_period)
      rain_threshold <- as.numeric(input$rain_threshold)
      treatment_threshold <- as.numeric(input$treatment_threshold)
      
      # Build facility filter
      facility_condition <- if (input$facility_filter == "all") {
        ""
      } else {
        sprintf("AND b.facility = '%s'", input$facility_filter)
      }
      
      # Build priority filter  
      priority_condition <- if (input$priority_filter == "all") {
        ""
      } else {
        sprintf("AND b.priority = '%s'", input$priority_filter)
      }
      
      # Build zone filter
      zone_condition <- if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
        zone_list <- paste0("'", input$zone_filter, "'", collapse = ",")
        sprintf("AND g.zone IN (%s)", zone_list)
      } else {
        ""
      }
      
      # Get all active air sites with rainfall data and status calculation
      query <- sprintf("
        WITH ActiveAirSites AS (
          SELECT 
            b.facility,
            b.sitecode,
            b.acres,
            b.priority,
            b.prehatch,
            g.zone,
            ST_X(ST_Centroid(ST_Transform(b.geom, 4326))) as longitude,
            ST_Y(ST_Centroid(ST_Transform(b.geom, 4326))) as latitude
          FROM loc_breeding_sites b
          LEFT JOIN public.gis_sectcode g ON LEFT(b.sitecode, 6) || '-' = g.sectcode
            OR LEFT(b.sitecode, 6) || 'N' = g.sectcode
            OR LEFT(b.sitecode, 6) || 'S' = g.sectcode
            OR LEFT(b.sitecode, 6) || 'E' = g.sectcode
            OR LEFT(b.sitecode, 6) || 'W' = g.sectcode
          WHERE (b.enddate IS NULL OR b.enddate > '%s')
            AND b.air_gnd = 'A'
            AND b.geom IS NOT NULL
            %s
            %s
            %s
        ),
        
        RainfallData AS (
          SELECT 
            a.sitecode,
            COALESCE(SUM(r.rain_inches), 0) as total_rainfall,
            MAX(r.date) as last_rain_date
          FROM ActiveAirSites a
          LEFT JOIN nws_precip_site_history r ON a.sitecode = r.sitecode
            AND r.date >= '%s'::date - INTERVAL '%d days'
            AND r.date <= '%s'::date
          GROUP BY a.sitecode
        ),
        
        RecentInspections AS (
          SELECT DISTINCT ON (sitecode)
            sitecode,
            inspdate as last_inspection_date,
            numdip,
            action
          FROM (
            SELECT sitecode, inspdate, numdip, action
            FROM dblarv_insptrt_current
            WHERE action IN ('1', '2', '4')
              AND inspdate >= '%s'::date - INTERVAL '90 days'
              AND inspdate <= '%s'::date
          ) combined_inspections
          ORDER BY sitecode, inspdate DESC
        ),
        
        RecentTreatments AS (
          SELECT DISTINCT ON (t.sitecode)
            t.sitecode,
            t.inspdate as last_treatment_date,
            t.mattype,
            m.effect_days,
            ('%s'::date - t.inspdate::date) as days_since_treatment
          FROM (
            SELECT sitecode, inspdate, mattype, action
            FROM dblarv_insptrt_current
            WHERE action IN ('A', '3', 'D')
              AND inspdate >= '%s'::date - INTERVAL '90 days'
              AND inspdate <= '%s'::date
          ) t
          LEFT JOIN mattype_list m ON t.mattype = m.mattype
          ORDER BY t.sitecode, t.inspdate DESC
        )
        
        SELECT 
          a.*,
          r.total_rainfall,
          r.last_rain_date,
          ('%s' - r.last_rain_date) as days_since_rain,
          TO_CHAR(i.last_inspection_date, 'YYYY-MM-DD') as last_inspection_date,
          i.numdip,
          ('%s' - i.last_inspection_date) as days_since_inspection,
          TO_CHAR(t.last_treatment_date, 'YYYY-MM-DD') as last_treatment_date,
          t.days_since_treatment,
          t.effect_days,
          t.mattype,
          
          -- Status calculation logic
          CASE
            -- Active treatment (within effectiveness period)
            WHEN t.days_since_treatment IS NOT NULL 
                 AND t.effect_days IS NOT NULL 
                 AND t.days_since_treatment <= t.effect_days THEN 'Active Treatment'
            
            -- Sites that need treatment (inspected with larvae above threshold, treatment expired or no treatment)
            WHEN i.numdip >= %d 
                 AND (t.days_since_treatment IS NULL 
                      OR t.effect_days IS NULL 
                      OR t.days_since_treatment > t.effect_days)
                 AND ('%s' - i.last_inspection_date) <= 3 THEN 'Needs Treatment'
            
            -- Sites under threshold (inspected with larvae below treatment threshold)  
            WHEN i.numdip IS NOT NULL AND i.numdip < %d THEN 'Under Threshold'
            
            -- Sites that need inspection (qualifying rainfall, no recent inspection or inspection expired)
            WHEN r.total_rainfall >= %f 
                 AND (i.last_inspection_date IS NULL 
                      OR r.last_rain_date > i.last_inspection_date) THEN 'Needs Inspection'
            
            -- Sites that were treated but treatment expired > 3 days ago, reset to Unknown
            WHEN t.days_since_treatment IS NOT NULL 
                 AND t.days_since_treatment > COALESCE(t.effect_days, 30) + 3 THEN 'Unknown'
            
            -- Default to Unknown
            ELSE 'Unknown'
          END as site_status
          
        FROM ActiveAirSites a
        LEFT JOIN RainfallData r ON a.sitecode = r.sitecode  
        LEFT JOIN RecentInspections i ON a.sitecode = i.sitecode
        LEFT JOIN RecentTreatments t ON a.sitecode = t.sitecode
        ORDER BY a.facility, a.sitecode",
        
        # Parameters for query
        analysis_date,     # Active sites filter
        facility_condition,
        priority_condition, # Priority filter
        zone_condition,    # Zone filter
        analysis_date,     # Rainfall start date
        lookback_days,     # Rainfall lookback period
        analysis_date,     # Rainfall end date
        analysis_date,     # Recent inspections start
        analysis_date,     # Recent inspections end (no future dates)
        analysis_date,     # Days since treatment calculation
        analysis_date,     # Recent treatments start
        analysis_date,     # Recent treatments end (no future dates)
        analysis_date,     # Days since rain calculation  
        analysis_date,     # Days since inspection calculation
        treatment_threshold, # Treatment threshold for needs treatment
        analysis_date,     # Status calculation reference date
        treatment_threshold, # Treatment threshold for under threshold
        rain_threshold     # Rain threshold for needs inspection
      )
      
      result <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      # Apply status filter if specified
      if (input$status_filter != "all") {
        result <- result[result$site_status == input$status_filter, ]
      }
      
      return(result)
      
    }, error = function(e) {
      showNotification(paste("Error loading site data:", e$message), type = "error")
      if (!is.null(con)) dbDisconnect(con)
      return(data.frame())
    })
  })
  # Value boxes
  output$total_air_sites <- renderValueBox({
    data <- air_sites_data()
    value <- if (nrow(data) > 0) nrow(data) else 0
    
    valueBox(
      value = value,
      subtitle = "Total Air Sites",
      icon = icon("helicopter"),
      color = shiny_colors["completed"]
    )
  })
  
  output$sites_needs_inspection <- renderValueBox({
    data <- air_sites_data()
    value <- if (nrow(data) > 0) sum(data$site_status == "Needs Inspection", na.rm = TRUE) else 0
    
    valueBox(
      value = value,
      subtitle = "Needs Inspection", 
      icon = icon("search"),
      color = shiny_colors["needs_action"]
    )
  })
  
  output$sites_under_threshold <- renderValueBox({
    data <- air_sites_data()
    value <- if (nrow(data) > 0) sum(data$site_status == "Under Threshold", na.rm = TRUE) else 0
    
    valueBox(
      value = value,
      subtitle = "Under Treatment Threshold",
      icon = icon("check-circle"),
      color = shiny_colors["completed"]
    )
  })
  
  output$sites_needs_treatment <- renderValueBox({
    data <- air_sites_data()
    value <- if (nrow(data) > 0) sum(data$site_status == "Needs Treatment", na.rm = TRUE) else 0
    
    valueBox(
      value = value,
      subtitle = "Needs Treatment",
      icon = icon("syringe"), 
      color = shiny_colors["needs_treatment"]
    )
  })
  
  # Status map
  output$status_map <- renderLeaflet({
    data <- air_sites_data()
    
    if (nrow(data) == 0) {
      return(leaflet() %>% addTiles())
    }
    
    # Filter out missing coordinates
    data <- data[!is.na(data$longitude) & !is.na(data$latitude), ]
    
    if (nrow(data) == 0) {
      return(leaflet() %>% addTiles())
    }
    
    # Color mapping for status - sourced from db_helpers
    color_map <- get_status_color_map()
    
    # Map colors to data
    data$color <- sapply(data$site_status, function(status) {
      if (status %in% names(color_map)) color_map[[status]] else color_map[["Unknown"]]
    })
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        color = ~color,
        fillColor = ~color,
        radius = 5,
        stroke = TRUE,
        fillOpacity = 0.8,
        popup = ~paste0(
          "Site: ", sitecode, "<br/>",
          "Status: ", site_status, "<br/>",
          "Facility: ", facility, "<br/>",
          "Zone: P", ifelse(is.na(zone), "?", zone), "<br/>",
          "Total Rainfall: ", round(total_rainfall, 2), " inches<br/>",
          "Days Since Rain: ", days_since_rain,
          ifelse(site_status %in% c("Under Threshold", "Needs Treatment"),
                 paste0("<br/>Last Inspection: ", 
                       ifelse(is.na(last_inspection_date) | last_inspection_date == "", "None", 
                              tryCatch(format(as.Date(last_inspection_date), "%m/%d/%y"), 
                                     error = function(e) last_inspection_date)),
                       "<br/>Larvae Count: ", 
                       ifelse(is.na(numdip), "N/A", numdip)), 
                 "")
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors = unlist(color_map),
        labels = names(color_map),
        title = "Site Status"
      )
  })
  
  # Status chart
  output$status_chart <- renderPlotly({
    data <- air_sites_data()
    
    if (nrow(data) == 0) {
      return(plot_ly() %>% add_text(text = "No data available", x = 0.5, y = 0.5))
    }
    
    status_counts <- data %>%
      count(site_status) %>%
      arrange(desc(n))
    
    # Map statuses to Shiny named colors using sapply
    colors_for_chart <- sapply(status_counts$site_status, function(status) {
      switch(status,
        "Unknown" = shiny_colors[["unknown"]],
        "Needs Inspection" = shiny_colors[["needs_action"]],
        "Under Threshold" = shiny_colors[["completed"]],
        "Needs Treatment" = shiny_colors[["needs_treatment"]],
        "Active Treatment" = shiny_colors[["active"]],
        "gray"  # default
      )
    })
    
    plot_ly(status_counts, x = ~reorder(site_status, -n), y = ~n, type = "bar",
            marker = list(color = colors_for_chart)) %>%
      layout(
        title = "Sites by Status",
        xaxis = list(title = "Status"),
        yaxis = list(title = "Count")
      )
  })
  
  # Site details table
  output$site_details_table <- DT::renderDataTable({
    data <- air_sites_data()
    
    if (nrow(data) == 0) {
      return(datatable(data.frame(Message = "No data available")))
    }
    
    # Select and rename columns for display
    display_data <- data %>%
      mutate(
        formatted_zone = ifelse(is.na(zone), "Unknown", paste0("P", zone)),
        formatted_inspection_date = ifelse(is.na(last_inspection_date) | last_inspection_date == "", 
                                         "None", 
                                         tryCatch(format(as.Date(last_inspection_date), "%m/%d/%y"), 
                                                error = function(e) last_inspection_date)),
        formatted_treatment_date = ifelse(is.na(last_treatment_date) | last_treatment_date == "", 
                                        "None", 
                                        tryCatch(format(as.Date(last_treatment_date), "%m/%d/%y"), 
                                               error = function(e) last_treatment_date)),
        material_used = ifelse(site_status == "Active Treatment" & !is.na(mattype) & mattype != "", 
                              mattype, 
                              "N/A")
      ) %>%
      select(
        `Site Code` = sitecode,
        Facility = facility,
        Zone = formatted_zone,
        Status = site_status,
        Priority = priority,
        `Total Rainfall` = total_rainfall,
        `Days Since Rain` = days_since_rain,
        `Last Inspection` = formatted_inspection_date,
        `Days Since Inspection` = days_since_inspection,
        `Larvae Count` = numdip,
        `Last Treatment` = formatted_treatment_date,
        `Days Since Treatment` = days_since_treatment,
        `Material Used` = material_used
      )
    
    # Get centralized status colors from db_helpers
    color_map <- get_status_color_map()
    status_names <- names(color_map)
    status_colors <- unlist(color_map)
    
    datatable(display_data,
              options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(status_names, status_colors))
  })
  
  # Flow Testing Logic
  flow_test_data <- eventReactive(input$run_flow_test, {
    req(input$test_start_date, input$test_end_date, input$test_data_type)
    
    if (input$test_data_type == "synthetic") {
      return(generate_synthetic_test_data())
    }
    
    con <- get_db_connection()
    if (is.null(con)) return(data.frame())
    
    tryCatch({
      start_date <- input$test_start_date
      end_date <- input$test_end_date
      
      # Create a sequence of dates to test
      date_sequence <- seq(from = start_date, to = end_date, by = "day")
      
      results <- data.frame()
      
      for (test_date in date_sequence) {
        current_date <- as.Date(test_date, origin = "1970-01-01")
        
        # Get status counts for this specific date
        query <- sprintf("
          WITH ActiveAirSites AS (
            SELECT 
              b.sitecode,
              b.facility,
              b.priority
            FROM loc_breeding_sites b
            WHERE (b.enddate IS NULL OR b.enddate > '%s')
              AND b.air_gnd = 'A'
              AND b.geom IS NOT NULL
              AND b.priority = 'RED'
          ),
          
          RainfallData AS (
            SELECT 
              a.sitecode,
              COALESCE(SUM(r.rain_inches), 0) as total_rainfall,
              MAX(r.date) as last_rain_date
            FROM ActiveAirSites a
            LEFT JOIN nws_precip_site_history r ON a.sitecode = r.sitecode
              AND r.date >= '%s'::date - INTERVAL '3 days'
              AND r.date <= '%s'::date
            GROUP BY a.sitecode
          ),
          
          RecentInspections AS (
            SELECT DISTINCT ON (sitecode)
              sitecode,
              inspdate as last_inspection_date,
              numdip
            FROM dblarv_insptrt_current
            WHERE action IN ('1', '2', '4')
              AND inspdate >= '%s'::date - INTERVAL '90 days'
              AND inspdate <= '%s'::date
            ORDER BY sitecode, inspdate DESC
          ),
          
          RecentTreatments AS (
            SELECT DISTINCT ON (t.sitecode)
              t.sitecode,
              t.inspdate as last_treatment_date,
              m.effect_days,
              ('%s'::date - t.inspdate::date) as days_since_treatment
            FROM dblarv_insptrt_current t
            LEFT JOIN mattype_list m ON t.mattype = m.mattype
            WHERE t.action IN ('A', '3', 'D')
              AND t.inspdate >= '%s'::date - INTERVAL '90 days'
              AND t.inspdate <= '%s'::date
            ORDER BY t.sitecode, t.inspdate DESC
          )
          
          SELECT 
            COUNT(*) as total_sites,
            SUM(CASE WHEN r.total_rainfall >= 1.0 THEN 1 ELSE 0 END) as sites_with_rain,
            -- Count sites that have been inspected (have inspection records)
            SUM(CASE WHEN i.sitecode IS NOT NULL THEN 1 ELSE 0 END) as sites_inspected,
            -- Status-based counts that match main query logic
            SUM(CASE WHEN i.numdip IS NOT NULL AND i.numdip < 1 THEN 1 ELSE 0 END) as under_threshold,
            SUM(CASE WHEN i.numdip >= 1 
                      AND (t.days_since_treatment IS NULL 
                           OR t.effect_days IS NULL 
                           OR t.days_since_treatment > t.effect_days)
                      AND ('%s' - i.last_inspection_date) <= 3 THEN 1 ELSE 0 END) as needs_treatment,
            SUM(CASE WHEN t.days_since_treatment IS NOT NULL 
                      AND t.effect_days IS NOT NULL 
                      AND t.days_since_treatment <= t.effect_days THEN 1 ELSE 0 END) as active_treatment,
            -- Sites needing inspection: have qualifying rain but no recent inspection or inspection is older than rain
            SUM(CASE WHEN r.total_rainfall >= 1.0 
                      AND (i.last_inspection_date IS NULL 
                           OR r.last_rain_date > i.last_inspection_date) THEN 1 ELSE 0 END) as needs_inspection,
            -- Add unknown status count for better validation
            SUM(CASE WHEN (t.days_since_treatment IS NOT NULL 
                           AND t.days_since_treatment > COALESCE(t.effect_days, 30) + 3)
                      OR (r.total_rainfall < 1.0 AND i.sitecode IS NULL)
                      OR (i.sitecode IS NULL AND r.total_rainfall < 1.0) THEN 1 ELSE 0 END) as unknown_status
          FROM ActiveAirSites a
          LEFT JOIN RainfallData r ON a.sitecode = r.sitecode  
          LEFT JOIN RecentInspections i ON a.sitecode = i.sitecode
          LEFT JOIN RecentTreatments t ON a.sitecode = t.sitecode",
          
          current_date,  # Active sites
          current_date,  # Rainfall start
          current_date,  # Rainfall end
          current_date,  # Inspections start
          current_date,  # Inspections end
          current_date,  # Treatment calculation
          current_date,  # Treatments start
          current_date,  # Treatments end
          current_date   # Status calculation
        )
        
        day_result <- dbGetQuery(con, query)
        day_result$test_date <- current_date
        results <- rbind(results, day_result)
      }
      
      dbDisconnect(con)
      return(results)
      
    }, error = function(e) {
      showNotification(paste("Error running flow test:", e$message), type = "error")
      if (!is.null(con)) dbDisconnect(con)
      return(data.frame())
    })
  })
  
  # Synthetic Test Data Generation
  generate_synthetic_test_data <- reactive({
    req(input$synth_total_sites, input$synth_rain_pct, input$synth_inspect_pct, input$synth_above_thresh_pct)
    
    start_date <- input$test_start_date
    end_date <- input$test_end_date
    date_sequence <- seq(from = start_date, to = end_date, by = "day")
    
    results <- data.frame()
    
    # Simulate the flow over time
    total_sites <- input$synth_total_sites
    sites_with_rain <- round(total_sites * input$synth_rain_pct / 100)
    sites_inspected <- round(sites_with_rain * input$synth_inspect_pct / 100)
    sites_above_thresh <- round(sites_inspected * input$synth_above_thresh_pct / 100)
    
    # Initialize state variables
    active_treatment <- 0
    needs_treatment <- 0
    under_threshold <- 0
    
    for (i in 1:length(date_sequence)) {
      current_date <- date_sequence[i]
      
      # Day 1: Sites get rain and are inspected
      if (i == 1) {
        needs_treatment <- sites_above_thresh
        under_threshold <- sites_inspected - sites_above_thresh  # This should equal sites_under_thresh
        active_treatment <- 0
      }
      # Day 2-3: Some sites get treated (simulate 50% treatment rate per day)
      else if (i <= 3 && needs_treatment > 0) {
        new_treatments <- round(needs_treatment * 0.5)
        active_treatment <- active_treatment + new_treatments
        needs_treatment <- needs_treatment - new_treatments
      }
      # After day 5: Treatments start expiring (assuming 3-day treatment effect)
      else if (i > 5) {
        # Some active treatments expire and go to Unknown (not counted in inspected categories)
        expiring <- round(active_treatment * 0.3)
        active_treatment <- active_treatment - expiring
        # Note: Expired treatments don't go back to under_threshold - they become Unknown status
        # and are no longer counted as "inspected sites"
        sites_inspected <- sites_inspected - expiring
      }
      
      day_result <- data.frame(
        test_date = current_date,
        total_sites = total_sites,
        sites_with_rain = ifelse(i == 1, sites_with_rain, round(sites_with_rain * 0.1)), # Less rain after day 1
        sites_inspected = sites_inspected,
        under_threshold = under_threshold,
        needs_treatment = needs_treatment,
        active_treatment = active_treatment,
        needs_inspection = total_sites - sites_inspected
      )
      
      results <- rbind(results, day_result)
    }
    
    return(results)
  })
  
  output$flow_test_results <- DT::renderDataTable({
    data <- flow_test_data()
    
    if (nrow(data) == 0) {
      return(datatable(data.frame(Message = "No test data available")))
    }
    
    datatable(data, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatStyle(c("needs_treatment", "active_treatment"), 
                  backgroundColor = "lightyellow")
  })
  
  output$daily_counts_chart <- renderPlotly({
    data <- flow_test_data()
    
    if (nrow(data) == 0) {
      return(plot_ly() %>% add_text(text = "No data available", x = 0.5, y = 0.5))
    }
    
    # Reshape data for plotting
    plot_data <- data %>%
      select(test_date, needs_inspection, under_threshold, needs_treatment, active_treatment) %>%
      tidyr::gather(key = "status", value = "count", -test_date)
    
    plot_ly(plot_data, x = ~test_date, y = ~count, color = ~status, type = "scatter", mode = "lines+markers") %>%
      layout(
        title = "Status Counts Over Time",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Count")
      )
  })
  
  output$flow_summary <- renderText({
    data <- flow_test_data()
    
    if (nrow(data) == 0) {
      return("No test data available")
    }
    
    latest <- data[nrow(data), ]
    
    paste(
      "FLOW TEST SUMMARY",
      "=================",
      paste("Total Air Sites:", latest$total_sites),
      paste("Sites with Qualifying Rain:", latest$sites_with_rain),
      paste("Sites Inspected:", latest$sites_inspected),
      paste("Under Threshold:", latest$under_threshold),
      paste("Needs Treatment:", latest$needs_treatment), 
      paste("Active Treatment:", latest$active_treatment),
      paste("Needs Inspection:", latest$needs_inspection),
      "",
      "EXPECTED FLOW:",
      "Rain → Inspection → (Under Threshold OR Needs Treatment) → Active Treatment → Unknown",
      sep = "\n"
    )
  })
  
  output$validation_summary <- renderText({
    data <- flow_test_data()
    
    if (nrow(data) == 0) {
      return("No data to validate")
    }
    
    latest <- data[nrow(data), ]
    
    # Validation checks
    inspected_accounted <- latest$under_threshold + latest$needs_treatment + latest$active_treatment
    inspected_missing <- latest$sites_inspected - inspected_accounted
    
    # Total accounted sites (including unknown)
    total_accounted <- latest$under_threshold + latest$needs_treatment + latest$active_treatment + 
                      latest$needs_inspection + ifelse("unknown_status" %in% names(latest), latest$unknown_status, 0)
    total_missing <- latest$total_sites - total_accounted
    
    validation_issues <- c()
    
    # Check data types and values
    if (any(sapply(latest, function(x) !is.numeric(x) && !is.integer(x)))) {
      non_numeric <- names(latest)[!sapply(latest, function(x) is.numeric(x) || is.integer(x))]
      validation_issues <- c(validation_issues, 
        sprintf("⚠️  DATA TYPE ISSUE: Non-numeric columns: %s", paste(non_numeric, collapse = ", ")))
    }
    
    if (inspected_missing > 0) {
      validation_issues <- c(validation_issues, 
        sprintf("❌ MISSING: %d inspected sites are unaccounted for in status categories", inspected_missing))
    }
    
    if (total_missing > 0) {
      validation_issues <- c(validation_issues,
        sprintf("❌ TOTAL MISSING: %d sites unaccounted for across all categories", total_missing))
    }
    
    # More flexible rain logic check
    rain_logic_diff <- latest$sites_with_rain - (latest$sites_inspected + latest$needs_inspection)
    if (abs(rain_logic_diff) > 0) {
      validation_issues <- c(validation_issues,
        sprintf("⚠️  RAIN LOGIC: Sites with rain (%d) vs Inspected + Needs Inspection (%d). Diff: %d", 
                latest$sites_with_rain, latest$sites_inspected + latest$needs_inspection, rain_logic_diff))
    }
    
    if (length(validation_issues) == 0) {
      validation_issues <- c("✅ All data validation checks passed!")
    }
    
    paste(
      "DATA VALIDATION REPORT",
      "======================",
      sprintf("Total Sites: %d", latest$total_sites),
      sprintf("Sites with Rain: %d", latest$sites_with_rain),
      sprintf("Sites Inspected: %d", latest$sites_inspected),
      sprintf("Needs Inspection: %d", latest$needs_inspection),
      "",
      "STATUS BREAKDOWN:",
      sprintf("  Under Threshold: %d", latest$under_threshold),
      sprintf("  Needs Treatment: %d", latest$needs_treatment), 
      sprintf("  Active Treatment: %d", latest$active_treatment),
      if("unknown_status" %in% names(latest)) sprintf("  Unknown Status: %d", latest$unknown_status) else "  Unknown Status: Not calculated",
      "",
      sprintf("Inspected Accounted For: %d/%d (%.1f%%)", 
              inspected_accounted, latest$sites_inspected, 
              ifelse(latest$sites_inspected > 0, (inspected_accounted/latest$sites_inspected)*100, 0)),
      sprintf("Total Accounted For: %d/%d (%.1f%%)", 
              total_accounted, latest$total_sites,
              ifelse(latest$total_sites > 0, (total_accounted/latest$total_sites)*100, 0)),
      "",
      "VALIDATION RESULTS:",
      paste(validation_issues, collapse = "\n"),
      if(length(validation_issues) == 0) "✅ All basic validation checks passed!" else "",
      "",
      "DATA TYPES:",
      paste(sprintf("  %s: %s", names(latest), sapply(latest, class)), collapse = "\n"),
      sep = "\n"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)