# Suppress R CMD check notes for dplyr/ggplot2 NSE variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "facility", "inspdate", "date_diff", "round_start", "round_id", "sitecode", "acres", "start_date", "checkback_count", "last_treatment_date", "next_treatment_date", "checkback", "last_treatment", "daily_summary", "sites_treated", "timing_df", "days_to_checkback"
  ))
}

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DBI)
library(RPostgreSQL)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DBI)
library(RPostgreSQL)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)

# Database connection function
get_db_connection <- function() {
  tryCatch({
    dbConnect(PostgreSQL(),
              host = "rds-readonly.mmcd.org",
              port = 5432,
              dbname = "mmcd_data",
              user = "mmcd_read",
              password = "mmcd2012")
  }, error = function(e) {
    showNotification(paste("Database connection failed:", e$message), type = "error", duration = 10)
    NULL
  })
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Control Efficacy - Air Treatment Checkbacks"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Treatment Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Checkback Status", tabName = "status", icon = icon("tasks")),
      menuItem("Site Details", tabName = "details", icon = icon("map-marker")),
      menuItem("Multiple Checkbacks", tabName = "multiple", icon = icon("search-plus"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .info-box {
          border-radius: 8px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3c8dbc;
        }
      "))
    ),
    
    tabItems(
      # Analysis Tab
      tabItem(tabName = "analysis",
        fluidRow(
          # Controls
          box(title = "Analysis Controls", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(3,
                dateRangeInput("date_range", "Select Date Range:",
                  start = Sys.Date() - 90,
                  end = Sys.Date(),
                  max = Sys.Date()
                )
              ),
              column(2,
                selectInput("facility_filter", "Facility:",
                  choices = c("All Facilities" = "all"),
                  selected = "all"
                )
              ),
              column(2,
                selectInput("matcode_filter", "Material Code:",
                  choices = c("All Materials" = "all"),
                  selected = "all"
                )
              ),
              column(3,
                radioButtons("checkback_type", "Checkback Target:",
                  choices = list("Percentage" = "percent", "Fixed Number" = "number"),
                  selected = "percent",
                  inline = TRUE
                )
              ),
              column(3,
                conditionalPanel(
                  condition = "input.checkback_type == 'percent'",
                  numericInput("checkback_percent", "Required Checkback %:",
                    value = 10,
                    min = 0,
                    max = 100,
                    step = 5
                  )
                ),
                conditionalPanel(
                  condition = "input.checkback_type == 'number'",
                  numericInput("checkback_number", "Number of Checkbacks Required:",
                    value = 10,
                    min = 1,
                    step = 1
                  )
                )
              )
            ),
            # Removed duplicate numericInput for checkback_number
          )
        ),
        
        fluidRow(
          # Summary Statistics
          valueBoxOutput("total_treatments", width = 3),
          valueBoxOutput("sites_treated", width = 3),
          valueBoxOutput("checkbacks_needed", width = 3),
          valueBoxOutput("checkbacks_completed", width = 3)
        ),
        
        fluidRow(
          # Treatment Timeline
          box(title = "Air Treatment Events", status = "info", solidHeader = TRUE, width = 12,
            plotOutput("treatment_timeline", height = "400px")
          )
        ),
        
        fluidRow(
          # Checkback Status by Facility
          box(title = "Checkback Status by Facility", status = "success", solidHeader = TRUE, width = 8,
            DT::dataTableOutput("facility_status")
          ),
          
          # Days to Checkback Distribution  
          box(title = "Time to Checkback", status = "warning", solidHeader = TRUE, width = 4,
            plotOutput("checkback_timing", height = "300px")
          )
        )
      ),
      
      # Status Tab
      tabItem(tabName = "status",
        fluidRow(
          box(title = "Air Treatment Round Details", status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("treatment_rounds")
          )
        ),
        
        
        fluidRow(
            box(title = "All Sites with Checkbacks", status = "info", solidHeader = TRUE, width = 8,
                DT::dataTableOutput("all_checkbacks_table")
            ),
            box(title = "Dip Count Changes (Pre/Post Treatment)", status = "warning", solidHeader = TRUE, width = 4,
                plotOutput("dip_changes_plot", height = "400px")
            )
        )
      ),
      
      # Details Tab  
      tabItem(tabName = "details",
        fluidRow(
          box(title = "Site-Level Treatment and Checkback Details", status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("site_details")
          )
        ),
        
        fluidRow(
          box(title = "Efficacy Analysis", status = "success", solidHeader = TRUE, width = 12,
            plotOutput("efficacy_plot", height = "400px")
          )
        )
      ),
      
      # Multiple Checkbacks Tab
      tabItem(tabName = "multiple",
        fluidRow(
          box(title = "Sites with Multiple Checkbacks", status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("multiple_checkbacks_table")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    treatments = NULL,
    checkbacks = NULL,
    facilities = NULL
  )
  
  # Load data
  observe({
    con <- get_db_connection()
    if (!is.null(con)) {
      tryCatch({
        # Get facilities for filter (from air treatments - action = 'A')
        facilities <- dbGetQuery(con, "SELECT DISTINCT facility FROM dblarv_insptrt_current WHERE action = 'A' ORDER BY facility")
        fac_choices <- setNames(facilities$facility, facilities$facility)
        fac_choices <- c("All Facilities" = "all", fac_choices)
        
        # Get material codes for filter (from air treatments only)
        matcodes <- dbGetQuery(con, "SELECT DISTINCT matcode FROM dblarv_insptrt_current WHERE action = 'A' AND matcode IS NOT NULL AND matcode != '' ORDER BY matcode")
        if (nrow(matcodes) == 0) {
          # Ensure dropdown always has at least the All option
          mat_choices <- c("All Materials" = "all")
        } else {
          mat_choices <- setNames(matcodes$matcode, matcodes$matcode)
          mat_choices <- c("All Materials" = "all", mat_choices)
        }
        
        updateSelectInput(session, "facility_filter", choices = fac_choices)
        updateSelectInput(session, "matcode_filter", choices = mat_choices)
        values$facilities <- facilities
        
        dbDisconnect(con)
      }, error = function(e) {
        showNotification(paste("Error loading filters:", e$message), type = "error")
        dbDisconnect(con)
      })
    }
  })
  
  # Reactive data loading
  treatment_data <- reactive({
    req(input$date_range)
    
    # Validate date range inputs
    if (is.null(input$date_range) || length(input$date_range) != 2) {
      return(NULL)
    }
    
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    
    tryCatch({
      # Query for air treatments (action = 'A')
      query <- "
        SELECT 
          inspdate,
          facility,
          foreman,
          sitecode,
          action,
          numdip,
          diphabitat,
          acres,
          matcode,
          pkey_pg,
          insptime
        FROM dblarv_insptrt_current 
        WHERE action = 'A' 
          AND inspdate >= $1 
          AND inspdate <= $2
        ORDER BY inspdate, facility, sitecode
      "
      
      params <- list(
        as.character(input$date_range[1]),
        as.character(input$date_range[2])
      )
      
      treatments <- dbGetQuery(con, query, params = params)
      dbDisconnect(con)
      
      if (!is.null(treatments) && nrow(treatments) > 0) {
        treatments$inspdate <- as.Date(treatments$inspdate)
        # Filter by facility if selected
        if (!is.null(input$facility_filter) && input$facility_filter != "all") {
          treatments <- treatments[treatments$facility == input$facility_filter, ]
        }
        # Filter by matcode if selected
        if (!is.null(input$matcode_filter) && input$matcode_filter != "all") {
          treatments <- treatments[treatments$matcode == input$matcode_filter, ]
        }
        return(treatments)
      } else {
        return(NULL)
      }
      
    }, error = function(e) {
      showNotification(paste("Error loading treatment data:", e$message), type = "error")
      if (exists("con") && !is.null(con)) {
        try(dbDisconnect(con), silent = TRUE)
      }
      return(NULL)
    })
  })
  
  # Get checkback data
  checkback_data <- reactive({
    req(treatment_data())
    
    # Validate treatment data exists
    treatments <- treatment_data()
    if (is.null(treatments) || nrow(treatments) == 0) {
      return(NULL)
    }
    
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    
    tryCatch({
      # Get checkback inspections for treated sites
      treated_sites <- unique(treatments$sitecode)
      
      if (length(treated_sites) == 0) return(NULL)
      
      # Create placeholder string for SQL IN clause - use $3, $4, etc. for consistency
      site_placeholders <- paste0("$", 3:(2 + length(treated_sites)), collapse = ",")
      
      query <- paste("
        SELECT 
          inspdate,
          facility,
          foreman,
          sitecode,
          action,
          numdip,
          diphabitat,
          pkey_pg,
          insptime,
          posttrt_p
        FROM dblarv_insptrt_current 
        WHERE sitecode IN (", site_placeholders, ")
          AND inspdate >= $1 
          AND inspdate <= $2
          AND action = '4'
          AND posttrt_p IS NOT NULL
        ORDER BY inspdate, sitecode
      ")
      
      params <- c(list(
        as.character(input$date_range[1]),
        as.character(input$date_range[2])
      ), as.list(treated_sites))
      
      checkbacks <- dbGetQuery(con, query, params = params)
      
      dbDisconnect(con)
      
      if (nrow(checkbacks) > 0) {
        checkbacks$inspdate <- as.Date(checkbacks$inspdate)
        return(checkbacks)
      } else {
        return(NULL)
      }
      
    }, error = function(e) {
      showNotification(paste("Error loading checkback data:", e$message), type = "error")
      if (exists("con") && !is.null(con)) {
        try(dbDisconnect(con), silent = TRUE)
      }
      return(NULL)
    })
  })
  
  # Calculate treatment rounds (multi-day treatments grouped)
  treatment_rounds <- reactive({
    req(treatment_data())
    
    treatments <- treatment_data()
    if (is.null(treatments) || nrow(treatments) == 0) {
      return(NULL)
    }
    
    # Group consecutive treatment days by facility
    rounds <- treatments %>%
      arrange(facility, inspdate) %>%
      group_by(facility) %>%
      mutate(
        date_diff = as.numeric(inspdate - lag(inspdate, default = inspdate[1] - 2)),
        round_start = ifelse(date_diff > 1, TRUE, FALSE),
        round_id = cumsum(round_start)
      ) %>%
      group_by(facility, round_id) %>%
      summarise(
        start_date = min(inspdate),
        end_date = max(inspdate),
        days_duration = as.numeric(max(inspdate) - min(inspdate)) + 1,
        sites_treated = n_distinct(sitecode),
        total_sites = n(),
        total_acres = sum(acres, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        round_name = paste(facility, format(start_date, "%m/%d"), sep = "-"),
        checkbacks_needed = case_when(
          input$checkback_type == "percent" ~ ceiling(sites_treated * input$checkback_percent / 100),
          TRUE ~ pmin(input$checkback_number, sites_treated)
        )
      )
    
    return(rounds)
  })
  
  # Calculate checkback status
  checkback_status <- reactive({
    req(treatment_rounds())
    
    rounds <- treatment_rounds()
    checkbacks <- checkback_data()
    treatments <- treatment_data()
    
    # Validate required data exists
    if (is.null(rounds) || nrow(rounds) == 0 || is.null(treatments) || nrow(treatments) == 0) {
      return(NULL)
    }
    
    # Allow function to work even if no checkbacks exist yet
    if (is.null(checkbacks)) {
      checkbacks <- data.frame()
    }
    
    # For each round, find checkbacks within reasonable timeframe
    results <- list()
    
    for (i in seq_len(nrow(rounds))) {
      round <- rounds[i, ]
      
      # Get sites treated in this round
      round_treatments <- treatments %>%
        filter(
          facility == round$facility,
          inspdate >= round$start_date,
          inspdate <= round$end_date
        )
      
      # For each site in this round, only count checkbacks that haven't been invalidated by newer treatments
      valid_checkbacks <- data.frame()
      
      if (nrow(checkbacks) > 0) {
        for (site in unique(round_treatments$sitecode)) {
          # Get the last treatment date for this site in this round
          last_treatment_date <- max(round_treatments$inspdate[round_treatments$sitecode == site])
          
          # Get all future treatments for this site (to invalidate checkbacks)
          future_treatments <- treatments %>%
            filter(sitecode == site, inspdate > last_treatment_date)
          
          # Get checkbacks for this site after the round
          site_checkbacks <- checkbacks %>%
            filter(
              sitecode == site,
              inspdate > last_treatment_date
            )
          
          if (nrow(site_checkbacks) > 0) {
            # If there are future treatments, only count checkbacks before the next treatment
            if (nrow(future_treatments) > 0) {
              next_treatment_date <- min(future_treatments$inspdate)
              site_checkbacks <- site_checkbacks %>%
                filter(inspdate < next_treatment_date)
            }
            
            # Take only the first valid checkback
            if (nrow(site_checkbacks) > 0) {
              first_checkback <- site_checkbacks %>%
                arrange(inspdate) %>%
                slice(1) %>%
                mutate(
                  days_to_checkback = as.numeric(inspdate - last_treatment_date),
                  round_end = round$end_date
                )
              
              valid_checkbacks <- rbind(valid_checkbacks, first_checkback)
            }
          }
        }
      }
      
      results[[i]] <- data.frame(
        facility = round$facility,
        round_name = round$round_name,
        start_date = round$start_date,
        end_date = round$end_date,
        sites_treated = round$sites_treated,
        checkbacks_needed = round$checkbacks_needed,
        checkbacks_completed = nrow(valid_checkbacks),
        completion_rate = round(nrow(valid_checkbacks) / round$checkbacks_needed * 100, 1),
        avg_days_to_checkback = ifelse(nrow(valid_checkbacks) > 0, 
                                     round(mean(valid_checkbacks$days_to_checkback, na.rm = TRUE), 1), 
                                     NA)
      )
    }
    
    if (length(results) > 0) {
      return(do.call(rbind, results))
    } else {
      return(NULL)
    }
  })
  
  # Multiple checkbacks analysis
  multiple_checkbacks_data <- reactive({
    checkbacks <- checkback_data()
    treatments <- treatment_data()
    
    if (is.null(checkbacks) || nrow(checkbacks) == 0 || 
        is.null(treatments) || nrow(treatments) == 0) {
      return(NULL)
    }
    
    # Find sites with multiple checkbacks
    sites_with_multiple <- checkbacks %>%
      group_by(sitecode) %>%
      summarise(checkback_count = n(), .groups = "drop") %>%
      filter(checkback_count > 1) %>%
      pull(sitecode)
    
    if (length(sites_with_multiple) == 0) {
      return(NULL)
    }
    
    # Get detailed checkback sequence for sites with multiple checkbacks
    multiple_details <- list()
    
    for (site in sites_with_multiple) {
      site_treatments <- treatments %>%
        filter(sitecode == site) %>%
        arrange(inspdate)
      
      site_checkbacks <- checkbacks %>%
        filter(sitecode == site) %>%
        arrange(inspdate)
      
      # Create sequence showing treatment -> checkback patterns
      for (i in seq_len(nrow(site_checkbacks))) {
        checkback <- site_checkbacks[i, ]
        
        # Find the most recent treatment before this checkback
        recent_treatment <- site_treatments %>%
          filter(inspdate <= checkback$inspdate) %>%
          arrange(desc(inspdate)) %>%
          slice(1)
        
        if (nrow(recent_treatment) > 0) {
          # Calculate change from previous checkback
          prev_dip <- if (i > 1) site_checkbacks$numdip[i-1] else recent_treatment$numdip
          dip_change <- checkback$numdip - prev_dip
          
          multiple_details[[length(multiple_details) + 1]] <- data.frame(
            sitecode = site,
            facility = checkback$facility,
            treatment_date = recent_treatment$inspdate,
            checkback_date = checkback$inspdate,
            checkback_sequence = i,
            days_since_treatment = as.numeric(checkback$inspdate - recent_treatment$inspdate),
            treatment_dip = recent_treatment$numdip,
            checkback_dip = checkback$numdip,
            previous_dip = prev_dip,
            dip_change_from_previous = dip_change,
            total_change_from_treatment = checkback$numdip - recent_treatment$numdip
          )
        }
      }
    }
    
    if (length(multiple_details) > 0) {
      return(do.call(rbind, multiple_details))
    } else {
      return(NULL)
    }
  })
  
  # All checkbacks summary
  all_checkbacks_summary <- reactive({
    checkbacks <- checkback_data()
    treatments <- treatment_data()
    
    if (is.null(checkbacks) || nrow(checkbacks) == 0 || 
        is.null(treatments) || nrow(treatments) == 0) {
      return(NULL)
    }
    
    # Get all sites with at least one checkback
    checkback_summary <- list()
    
    for (site in unique(checkbacks$sitecode)) {
      site_treatments <- treatments %>%
        filter(sitecode == site) %>%
        arrange(inspdate)
      
      site_checkbacks <- checkbacks %>%
        filter(sitecode == site) %>%
        arrange(inspdate)
      
      # Get the most recent treatment and first checkback after it
      last_treatment <- site_treatments %>%
        slice(nrow(site_treatments))
      
      first_checkback <- site_checkbacks %>%
        filter(inspdate > last_treatment$inspdate) %>%
        arrange(inspdate) %>%
        slice(1)
      
      if (nrow(first_checkback) > 0) {
        checkback_summary[[length(checkback_summary) + 1]] <- data.frame(
          sitecode = site,
          facility = first_checkback$facility,
          last_treatment_date = last_treatment$inspdate,
          first_checkback_date = first_checkback$inspdate,
          total_checkbacks = nrow(site_checkbacks),
          days_to_first_checkback = as.numeric(first_checkback$inspdate - last_treatment$inspdate),
          treatment_dip = last_treatment$numdip,
          first_checkback_dip = first_checkback$numdip,
          initial_reduction = last_treatment$numdip - first_checkback$numdip,
          percent_reduction = ifelse(last_treatment$numdip > 0, 
                                   round((last_treatment$numdip - first_checkback$numdip) / last_treatment$numdip * 100, 1), 
                                   0)
        )
      }
    }
    
    if (length(checkback_summary) > 0) {
      return(do.call(rbind, checkback_summary))
    } else {
      return(NULL)
    }
  })
  
  # Value boxes
  output$total_treatments <- renderValueBox({
    treatments <- treatment_rounds()
    value <- ifelse(is.null(treatments), 0, nrow(treatments))
    
    valueBox(
      value = value,
      subtitle = "Treatment Rounds",
      icon = icon("plane"),
      color = "blue"
    )
  })
  
  output$sites_treated <- renderValueBox({
    treatments <- treatment_rounds()
    value <- ifelse(is.null(treatments), 0, sum(treatments$sites_treated, na.rm = TRUE))
    
    valueBox(
      value = value,
      subtitle = "Sites Treated",
      icon = icon("map-marker"),
      color = "green"
    )
  })
  
  output$checkbacks_needed <- renderValueBox({
    treatments <- treatment_rounds()
    value <- ifelse(is.null(treatments), 0, sum(treatments$checkbacks_needed, na.rm = TRUE))
    
    valueBox(
      value = value,
      subtitle = "Checkbacks Needed",
      icon = icon("tasks"),
      color = "yellow"
    )
  })
  
  output$checkbacks_completed <- renderValueBox({
    status <- checkback_status()
    value <- ifelse(is.null(status), 0, sum(status$checkbacks_completed, na.rm = TRUE))
    
    valueBox(
      value = value,
      subtitle = "Checkbacks Completed",
      icon = icon("check"),
      color = "purple"
    )
  })
  
  # Treatment timeline plot
  output$treatment_timeline <- renderPlot({
    treatments <- treatment_data()
    
    if (is.null(treatments)) {
      p <- ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No treatment data available"), size = 6) +
        theme_void()
      return(p)
    }
    
    # Daily site counts by facility
    daily_summary <- treatments %>%
      group_by(inspdate, facility) %>%
      summarise(sites_treated = n(), .groups = "drop")
    
    p <- ggplot(daily_summary, aes(x = inspdate, y = sites_treated, fill = facility)) +
      geom_col(position = "dodge") +
      labs(
        title = "Air Treatment Activity Over Time",
        x = "Date",
        y = "Sites Treated",
        fill = "Facility"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
    
    return(p)
  })
  
  # Facility status table
  output$facility_status <- DT::renderDataTable({
    status <- checkback_status()
    
    if (is.null(status)) {
      return(data.frame(Message = "No data available"))
    }
    
    datatable(status,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = c("completion_rate"), render = JS(
            "function(data, type, row, meta) {",
            "if(type === 'display' && data != null) {",
            "var color = data >= 80 ? 'green' : data >= 50 ? 'orange' : 'red';",
            "return '<span style=\"color: ' + color + '; font-weight: bold;\">' + data + '%</span>';",
            "} else { return data; }",
            "}"
          ))
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle("completion_rate",
        backgroundColor = styleInterval(c(50, 80), c("#ffebee", "#fff3e0", "#e8f5e8"))
      )
  })
  
  # Checkback timing plot
  output$checkback_timing <- renderPlot({
    treatments <- treatment_data()
    checkbacks <- checkback_data()
    
    if (is.null(treatments) || is.null(checkbacks)) {
      p <- ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No data"), size = 4) +
        theme_void()
      return(p)
    }
    
    # Calculate days between treatment and checkback
    timing_data <- list()
    
    for (site in unique(treatments$sitecode)) {
      site_treatments <- treatments[treatments$sitecode == site, ]
      site_checkbacks <- checkbacks[checkbacks$sitecode == site, ]
      
      if (nrow(site_checkbacks) > 0) {
        for (i in 1:nrow(site_treatments)) {
          treatment_date <- site_treatments$inspdate[i]
          future_checkbacks <- site_checkbacks[site_checkbacks$inspdate > treatment_date, ]
          
          if (nrow(future_checkbacks) > 0) {
            first_checkback <- future_checkbacks[which.min(future_checkbacks$inspdate), ]
            days_diff <- as.numeric(first_checkback$inspdate - treatment_date)
            
            timing_data[[length(timing_data) + 1]] <- data.frame(
              sitecode = site,
              facility = site_treatments$facility[i],
              days_to_checkback = days_diff
            )
          }
        }
      }
    }
    
    if (length(timing_data) > 0) {
      timing_df <- do.call(rbind, timing_data)
      
      p <- ggplot(timing_df, aes(x = days_to_checkback)) +
        geom_histogram(binwidth = 2, fill = "steelblue", alpha = 0.7) +
        labs(
          title = "Days to Checkback",
          x = "Days After Treatment",
          y = "Count"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
      
      return(p)
    } else {
      p <- ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No checkbacks found"), size = 4) +
        theme_void()
      return(p)
    }
  })
  
  # Treatment rounds table
  output$treatment_rounds <- DT::renderDataTable({
    rounds <- treatment_rounds()
    
    if (is.null(rounds)) {
      return(data.frame(Message = "No treatment rounds found"))
    }
    
    display_data <- rounds %>%
      select(
        Round = round_name,
        Facility = facility,
        `Start Date` = start_date,
        `End Date` = end_date,
        `Duration (Days)` = days_duration,
        `Sites Treated` = sites_treated,
        `Total Visits` = total_sites,
        `Acres Treated` = total_acres,
        `Checkbacks Needed` = checkbacks_needed
      ) %>%
      mutate(
        `Acres Treated` = round(`Acres Treated`, 1)
      )
    
    datatable(display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Checkback progress table
  output$checkback_progress <- DT::renderDataTable({
    status <- checkback_status()
    
    if (is.null(status)) {
      return(data.frame(Message = "No checkback data available"))
    }
    
    display_data <- status %>%
      select(
        Round = round_name,
        Facility = facility,
        `Treatment Period` = paste(start_date, "to", end_date),
        `Sites Treated` = sites_treated,
        `Checkbacks Needed` = checkbacks_needed,
        `Checkbacks Done` = checkbacks_completed,
        `Completion %` = completion_rate,
        `Avg Days to Checkback` = avg_days_to_checkback
      )
    
    datatable(display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Completion %",
        backgroundColor = styleInterval(c(50, 80), c("#ffcdd2", "#ffecb3", "#c8e6c9")),
        fontWeight = "bold"
      )
  })
  
  # Site details table
  output$site_details <- DT::renderDataTable({
    treatments <- treatment_data()
    checkbacks <- checkback_data()
    
    if (is.null(treatments)) {
      return(data.frame(Message = "No treatment data available"))
    }
    
    # Combine treatment and checkback information
    site_details <- list()
    
    for (site in unique(treatments$sitecode)) {
      site_treatments <- treatments[treatments$sitecode == site, ]
      site_checkbacks <- if (!is.null(checkbacks)) {
        checkbacks[checkbacks$sitecode == site, ]
      } else {
        data.frame()
      }
      
      for (i in 1:nrow(site_treatments)) {
        treatment <- site_treatments[i, ]
        
        # Find checkbacks after this treatment
        if (nrow(site_checkbacks) > 0) {
          future_checkbacks <- site_checkbacks[site_checkbacks$inspdate > treatment$inspdate, ]
          
          if (nrow(future_checkbacks) > 0) {
            first_checkback <- future_checkbacks[which.min(future_checkbacks$inspdate), ]
            days_diff <- as.numeric(first_checkback$inspdate - treatment$inspdate)
            checkback_status <- "Completed"
            checkback_date <- first_checkback$inspdate
            checkback_dip <- first_checkback$numdip
          } else {
            days_diff <- NA
            checkback_status <- "Pending"
            checkback_date <- NA
            checkback_dip <- NA
          }
        } else {
          days_diff <- NA
          checkback_status <- "Pending"
          checkback_date <- NA
          checkback_dip <- NA
        }
        
        site_details[[length(site_details) + 1]] <- data.frame(
          sitecode = treatment$sitecode,
          facility = treatment$facility,
          matcode = treatment$matcode,
          treatment_date = treatment$inspdate,
          treatment_dip = treatment$numdip,
          checkback_status = checkback_status,
          checkback_date = checkback_date,
          checkback_dip = checkback_dip,
          days_to_checkback = days_diff,
          stringsAsFactors = FALSE
        )
      }
    }
    
    if (length(site_details) > 0) {
      details_df <- do.call(rbind, site_details)
      
      display_data <- details_df %>%
        arrange(facility, treatment_date, sitecode) %>%
        select(
          `Site Code` = sitecode,
          Facility = facility,
          `Material Code` = matcode,
          `Treatment Date` = treatment_date,
          `Treatment Dip Count` = treatment_dip,
          `Checkback Status` = checkback_status,
          `Checkback Date` = checkback_date,
          `Checkback Dip Count` = checkback_dip,
          `Days to Checkback` = days_to_checkback
        )
      
      datatable(display_data,
        options = list(
          pageLength = 20,
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        formatStyle("Checkback Status",
          backgroundColor = styleEqual(c("Completed", "Pending"), c("#c8e6c9", "#ffecb3"))
        )
    } else {
      return(data.frame(Message = "No detailed site data available"))
    }
  })
  
  # Efficacy plot
  output$efficacy_plot <- renderPlot({
    treatments <- treatment_data()
    checkbacks <- checkback_data()
    
    if (is.null(treatments) || is.null(checkbacks)) {
      p <- ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No data for efficacy analysis"), size = 6) +
        theme_void()
      return(p)
    }
    
    # Calculate dip count changes
    efficacy_data <- list()
    
    for (site in unique(treatments$sitecode)) {
      site_treatments <- treatments[treatments$sitecode == site, ]
      site_checkbacks <- checkbacks[checkbacks$sitecode == site, ]
      
      for (i in 1:nrow(site_treatments)) {
        treatment <- site_treatments[i, ]
        
        # Find checkbacks after this treatment
        future_checkbacks <- site_checkbacks[site_checkbacks$inspdate > treatment$inspdate, ]
        
        if (nrow(future_checkbacks) > 0) {
          checkback <- future_checkbacks[which.min(future_checkbacks$inspdate), ]
          
          efficacy_data[[length(efficacy_data) + 1]] <- data.frame(
            sitecode = treatment$sitecode,
            facility = treatment$facility,
            matcode = treatment$matcode,
            treatment_date = treatment$inspdate,
            pre_treatment_dip = treatment$numdip,
            post_treatment_dip = checkback$numdip,
            days_between = as.numeric(checkback$inspdate - treatment$inspdate),
            reduction = treatment$numdip - checkback$numdip,
            percent_reduction = ifelse(treatment$numdip > 0, 
                                     (treatment$numdip - checkback$numdip) / treatment$numdip * 100, 
                                     0)
          )
        }
      }
    }
    
    if (length(efficacy_data) > 0) {
      efficacy_df <- do.call(rbind, efficacy_data)
      
      p <- ggplot(efficacy_df, aes(x = pre_treatment_dip, y = post_treatment_dip, 
                                   color = facility, size = days_between)) +
        geom_point(alpha = 0.7) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray", size = 1) +
        labs(
          title = "Treatment Efficacy: Pre vs Post Treatment Dip Counts",
          x = "Pre-Treatment Dip Count",
          y = "Post-Treatment Dip Count",
          color = "Facility",
          size = "Days to Checkback"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "right"
        ) +
        scale_size_continuous(range = c(2, 6))
      
      return(p)
    } else {
      p <- ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No efficacy data available"), size = 6) +
        theme_void()
      return(p)
    }
  })
  
  # Multiple checkbacks table
  output$multiple_checkbacks_table <- DT::renderDataTable({
    multiple_data <- multiple_checkbacks_data()
    
    if (is.null(multiple_data)) {
      return(data.frame(Message = "No sites with multiple checkbacks found"))
    }
    
    display_data <- multiple_data %>%
      arrange(sitecode, checkback_sequence) %>%
      select(
        `Site Code` = sitecode,
        Facility = facility,
        `Treatment Date` = treatment_date,
        `Checkback Date` = checkback_date,
        `Checkback #` = checkback_sequence,
        `Days Since Treatment` = days_since_treatment,
        `Treatment Dip` = treatment_dip,
        `Checkback Dip` = checkback_dip,
        `Previous Dip` = previous_dip,
        `Change from Previous` = dip_change_from_previous,
        `Total Change` = total_change_from_treatment
      )
    
    datatable(display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Change from Previous",
        backgroundColor = styleInterval(c(-0.1, 0.1), c("#ffcdd2", "#fff9c4", "#c8e6c9")),
        fontWeight = "bold"
      ) %>%
      formatStyle("Total Change",
        backgroundColor = styleInterval(c(-0.1, 0.1), c("#ffcdd2", "#fff9c4", "#c8e6c9")),
        fontWeight = "bold"
      )
  })
  
  # All checkbacks table
  output$all_checkbacks_table <- DT::renderDataTable({
    all_data <- all_checkbacks_summary()
    
    if (is.null(all_data)) {
      return(data.frame(Message = "No sites with checkbacks found"))
    }
    
    display_data <- all_data %>%
      arrange(facility, sitecode) %>%
      select(
        `Site Code` = sitecode,
        Facility = facility,
        `Last Treatment` = last_treatment_date,
        `First Checkback` = first_checkback_date,
        `Total Checkbacks` = total_checkbacks,
        `Days to Checkback` = days_to_first_checkback,
        `Treatment Dip` = treatment_dip,
        `First Checkback Dip` = first_checkback_dip,
        `Initial Reduction` = initial_reduction,
        `% Reduction` = percent_reduction
      )
    
    datatable(display_data,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle("% Reduction",
        backgroundColor = styleInterval(c(0, 50, 80), c("#ffcdd2", "#fff9c4", "#c8e6c9", "#a5d6a7")),
        fontWeight = "bold"
      )
  })
  
  # Dip changes plot (pre-treatment vs first checkback for all sites)
  output$dip_changes_plot <- renderPlot({
    all_data <- all_checkbacks_summary()
    if (is.null(all_data) || nrow(all_data) == 0) {
      p <- ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No checkback data"), size = 5) +
        theme_void()
      return(p)
    }
    # Prepare paired data: one row per site, pre-treatment and first checkback
    plot_df <- all_data %>%
      select(sitecode, treatment_dip, first_checkback_dip, days_to_first_checkback) %>%
      tidyr::pivot_longer(cols = c(treatment_dip, first_checkback_dip),
                           names_to = "type", values_to = "dip")
    plot_df$type <- factor(plot_df$type, levels = c("treatment_dip", "first_checkback_dip"),
                           labels = c("Pre-Treatment", "First Checkback"))
    # Dumbbell plot: paired points with lines, plus boxplot overlay
    dodge_amt <- 0.2
    p <- ggplot(plot_df, aes(x = type, y = dip, group = sitecode, color = days_to_first_checkback)) +
      geom_boxplot(aes(group = type), color = "gray40", fill = NA, width = 0.3, position = position_nudge(x = 0.15), outlier.shape = NA) +
      geom_line(aes(group = sitecode), color = "gray70", alpha = 0.5, size = 1, position = position_dodge(width = dodge_amt)) +
      geom_point(size = 3, alpha = 0.8, position = position_dodge(width = dodge_amt)) +
      labs(
        title = "Dip Count Change: Pre-Treatment vs First Checkback",
        x = "",
        y = "Dip Count",
        color = "Days Between"
      ) +
      scale_color_gradientn(colors = c("red", "orange", "green", "blue", "purple")) +
      coord_cartesian(ylim = c(-2, 25), expand = FALSE) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 12),
        legend.position = "right"
      )
    return(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)