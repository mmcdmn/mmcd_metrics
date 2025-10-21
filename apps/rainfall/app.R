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
  dashboardHeader(title = "Rainfall and Breeding Sites Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Rainfall Map", tabName = "map", icon = icon("map")),
      menuItem("Rainfall Trends", tabName = "trends", icon = icon("chart-line")),
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
      # Map Tab
      tabItem(tabName = "map",
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
      
      # Trends Tab
      tabItem(tabName = "trends",
        fluidRow(
          box(title = "Rainfall Trends Over Time", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("rainfall_trend_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(title = "Rainfall by Location", status = "info", solidHeader = TRUE, width = 6,
            fluidRow(
              column(6,
                selectInput("location_level", "Location Level:",
                  choices = list(
                    "County" = "county",
                    "City" = "city",
                    "Section" = "section"
                  ),
                  selected = "city"
                )
              ),
              column(6,
                conditionalPanel(
                  condition = "input.location_level == 'city' || input.location_level == 'section'",
                  selectInput("county_filter", "Filter by County:",
                    choices = c("All Counties" = "all"),
                    selected = "all"
                  )
                )
              )
            ),
            fluidRow(
              column(12,
                conditionalPanel(
                  condition = "input.location_level == 'section'",
                  selectInput("city_filter", "Filter by City:",
                    choices = c("All Cities" = "all"),
                    selected = "all"
                  )
                )
              )
            ),
            plotOutput("rainfall_by_location", height = "300px")
          ),
          box(title = "Rainfall by Priority", status = "warning", solidHeader = TRUE, width = 6,
            plotOutput("rainfall_by_priority", height = "350px")
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
        fac_choices <- c("Select Facility" = "none", fac_choices)
        
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
        
        updateSelectInput(session, "facility_filter", choices = fac_choices)
        updateSelectInput(session, "wetland_type_filter", choices = type_choices)
        updateSelectInput(session, "priority_filter", choices = priority_choices)
        
        dbDisconnect(con)
      }, error = function(e) {
        showNotification(paste("Error loading filters:", e$message), type = "error")
        if (!is.null(con)) dbDisconnect(con)
      })
    }
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
  
  # Value boxes
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
}

# Run the application
shinyApp(ui = ui, server = server)