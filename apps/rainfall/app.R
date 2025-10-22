# Air Site Status Tracking Application
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
})

# Load environment variables
env_paths <- c(
  "../../.env",
  "../../../.env", 
  "/srv/shiny-server/.env"
)

env_loaded <- FALSE
for (path in env_paths) {
  if (file.exists(path)) {
    readRenviron(path)
    env_loaded <- TRUE
    break
  }
}

# Database configuration
db_host <- Sys.getenv("DB_HOST")
db_port <- Sys.getenv("DB_PORT")
db_user <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")
db_name <- Sys.getenv("DB_NAME")

# Database connection helper
get_db_connection <- function() {
  tryCatch({
    dbConnect(RPostgres::Postgres(),
             host = db_host,
             port = db_port,
             dbname = db_name,
             user = db_user,
             password = db_password)
  }, error = function(e) {
    showNotification(paste("Database connection error:", e$message), type = "error")
    return(NULL)
  })
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Air Site Status Tracking"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Air Site Status", tabName = "status", icon = icon("plane"))
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
                             "Unknown" = "U",
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
                actionButton("refresh_data", "Refresh Data", class = "btn-primary")
              )
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
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Load facility choices
  observe({
    con <- get_db_connection()
    if (!is.null(con)) {
      tryCatch({
        facilities <- dbGetQuery(con, 
          "SELECT DISTINCT facility FROM loc_breeding_sites 
           WHERE air_gnd = 'A' AND facility IS NOT NULL 
           ORDER BY facility")
        
        fac_choices <- c("All Facilities" = "all", setNames(facilities$facility, facilities$facility))
        updateSelectInput(session, "facility_filter", choices = fac_choices)
        
        dbDisconnect(con)
      }, error = function(e) {
        showNotification(paste("Error loading facilities:", e$message), type = "error")
        if (!is.null(con)) dbDisconnect(con)
      })
    }
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
      
      # Get all active air sites with rainfall data and status calculation
      query <- sprintf("
        WITH ActiveAirSites AS (
          SELECT 
            b.facility,
            b.sitecode,
            b.acres,
            b.priority,
            b.prehatch,
            ST_X(ST_Centroid(ST_Transform(b.geom, 4326))) as longitude,
            ST_Y(ST_Centroid(ST_Transform(b.geom, 4326))) as latitude
          FROM loc_breeding_sites b
          WHERE (b.enddate IS NULL OR b.enddate > '%s')
            AND b.air_gnd = 'A'
            AND b.geom IS NOT NULL
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
            WHERE action = 'A'
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
                 AND t.days_since_treatment > COALESCE(t.effect_days, 30) + 3 THEN 'U'
            
            -- Default to Unknown
            ELSE 'U'
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
      icon = icon("plane"),
      color = "blue"
    )
  })
  
  output$sites_needs_inspection <- renderValueBox({
    data <- air_sites_data()
    value <- if (nrow(data) > 0) sum(data$site_status == "Needs Inspection", na.rm = TRUE) else 0
    
    valueBox(
      value = value,
      subtitle = "Needs Inspection", 
      icon = icon("search"),
      color = "yellow"
    )
  })
  
  output$sites_under_threshold <- renderValueBox({
    data <- air_sites_data()
    value <- if (nrow(data) > 0) sum(data$site_status == "Under Threshold", na.rm = TRUE) else 0
    
    valueBox(
      value = value,
      subtitle = "Under Treatment Threshold",
      icon = icon("check-circle"),
      color = "blue"
    )
  })
  
  output$sites_needs_treatment <- renderValueBox({
    data <- air_sites_data()
    value <- if (nrow(data) > 0) sum(data$site_status == "Needs Treatment", na.rm = TRUE) else 0
    
    valueBox(
      value = value,
      subtitle = "Needs Treatment",
      icon = icon("syringe"), 
      color = "red"
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
    
    # Color mapping for status
    colors <- c(
      "U" = "gray",
      "Needs Inspection" = "yellow", 
      "Under Threshold" = "blue",
      "Needs Treatment" = "red",
      "Active Treatment" = "green"
    )
    
    data$color <- colors[data$site_status]
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        color = ~color,
        radius = 5,
        stroke = TRUE,
        fillOpacity = 0.8,
        popup = ~paste0(
          "Site: ", sitecode, "<br/>",
          "Status: ", site_status, "<br/>",
          "Facility: ", facility, "<br/>",
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
        colors = colors,
        labels = names(colors),
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
    
    # Consistent color mapping
    status_colors <- c(
      "U" = "gray",
      "Needs Inspection" = "yellow", 
      "Under Threshold" = "blue",
      "Needs Treatment" = "red",
      "Active Treatment" = "green"
    )
    
    # Map colors to the actual statuses in the data
    colors_for_chart <- status_colors[status_counts$site_status]
    
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
        formatted_inspection_date = ifelse(is.na(last_inspection_date) | last_inspection_date == "", 
                                         "None", 
                                         tryCatch(format(as.Date(last_inspection_date), "%m/%d/%y"), 
                                                error = function(e) last_inspection_date)),
        formatted_treatment_date = ifelse(is.na(last_treatment_date) | last_treatment_date == "", 
                                        "None", 
                                        tryCatch(format(as.Date(last_treatment_date), "%m/%d/%y"), 
                                               error = function(e) last_treatment_date))
      ) %>%
      select(
        `Site Code` = sitecode,
        Facility = facility,
        Status = site_status,
        Priority = priority,
        `Total Rainfall` = total_rainfall,
        `Days Since Rain` = days_since_rain,
        `Last Inspection` = formatted_inspection_date,
        `Days Since Inspection` = days_since_inspection,
        `Larvae Count` = numdip,
        `Last Treatment` = formatted_treatment_date,
        `Days Since Treatment` = days_since_treatment
      )
    
    datatable(display_data,
              options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(
                    c("U", "Needs Inspection", "Under Threshold", "Needs Treatment", "Active Treatment"),
                    c("#f0f0f0", "#ffff99", "#cce7ff", "#f8d7da", "#d4edda")
                  ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)