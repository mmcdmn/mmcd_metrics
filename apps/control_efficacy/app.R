library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DBI)
library(RPostgreSQL)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)

# Load environment variables
if (file.exists(".env")) {
  readRenviron(".env")
} else if (file.exists("../../.env")) {
  readRenviron("../../.env")
} else if (file.exists("/home/alexpdyak32/Documents/mmcd/mmcd_metrics/.env")) {
  readRenviron("/home/alexpdyak32/Documents/mmcd/mmcd_metrics/.env")
}

# Database connection function
get_db_connection <- function() {
  tryCatch({
    dbConnect(PostgreSQL(),
              host = Sys.getenv("DB_HOST"),
              port = as.integer(Sys.getenv("DB_PORT", "5432")),
              dbname = Sys.getenv("DB_NAME"),
              user = Sys.getenv("DB_USER"),
              password = Sys.getenv("DB_PASSWORD"))
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
      menuItem("Site Details", tabName = "details", icon = icon("map-marker"))
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
                    value = 20,
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
          box(title = "Treatment Campaign Details", status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("treatment_campaigns")
          )
        ),
        
        fluidRow(
          box(title = "Checkback Progress Tracking", status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("checkback_progress")
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
          createdate,
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
      # Get checkback inspections for treated sites (posttrt_p = 'P')
      treated_sites <- unique(treatments$sitecode)
      
      if (length(treated_sites) == 0) return(NULL)
      
      # Create placeholder string for SQL IN clause
      placeholders <- paste(rep("?", length(treated_sites)), collapse = ",")
      
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
          createdate,
          insptime,
          posttrt_p
        FROM dblarv_insptrt_current 
        WHERE sitecode IN (", placeholders, ")
          AND inspdate >= $1 
          AND inspdate <= $2
          AND action = '4'
          AND posttrt_p = 'P'
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
  
  # Calculate treatment campaigns (multi-day treatments grouped)
  treatment_campaigns <- reactive({
    req(treatment_data())
    
    treatments <- treatment_data()
    if (is.null(treatments) || nrow(treatments) == 0) {
      return(NULL)
    }
    
    # Group consecutive treatment days by facility
    campaigns <- treatments %>%
      arrange(facility, inspdate) %>%
      group_by(facility) %>%
      mutate(
        date_diff = as.numeric(inspdate - lag(inspdate, default = inspdate[1] - 2)),
        campaign_start = ifelse(date_diff > 1, TRUE, FALSE),
        campaign_id = cumsum(campaign_start)
      ) %>%
      group_by(facility, campaign_id) %>%
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
        campaign_name = paste(facility, format(start_date, "%m/%d"), sep = "-"),
        checkbacks_needed = case_when(
          input$checkback_type == "percent" ~ ceiling(sites_treated * input$checkback_percent / 100),
          TRUE ~ pmin(input$checkback_number, sites_treated)
        )
      )
    
    return(campaigns)
  })
  
  # Calculate checkback status
  checkback_status <- reactive({
    req(treatment_campaigns(), checkback_data())
    
    campaigns <- treatment_campaigns()
    checkbacks <- checkback_data()
    treatments <- treatment_data()
    
    # Validate all required data exists
    if (is.null(campaigns) || nrow(campaigns) == 0 || 
        is.null(checkbacks) || nrow(checkbacks) == 0 || 
        is.null(treatments) || nrow(treatments) == 0) {
      return(NULL)
    }
    
    # For each campaign, find checkbacks within reasonable timeframe (e.g., 30 days)
    results <- list()
    
  for (i in seq_len(nrow(campaigns))) {
      campaign <- campaigns[i, ]
      
      # Get sites treated in this campaign
      campaign_treatments <- treatments %>%
        filter(
          facility == campaign$facility,
          inspdate >= campaign$start_date,
          inspdate <= campaign$end_date
        )
      
        # Get checkbacks for these sites after treatment
        campaign_checkbacks <- checkbacks %>%
          filter(
            facility == campaign$facility,
            sitecode %in% campaign_treatments$sitecode,
            inspdate > campaign$end_date
          ) %>%
          group_by(sitecode) %>%
          arrange(inspdate) %>%
          slice(1) %>%  # First checkback only
          ungroup() %>%
          mutate(
            days_to_checkback = as.numeric(inspdate - campaign$end_date)
          )
        results[[i]] <- data.frame(
        facility = campaign$facility,
        campaign_name = campaign$campaign_name,
        start_date = campaign$start_date,
        end_date = campaign$end_date,
        sites_treated = campaign$sites_treated,
        checkbacks_needed = campaign$checkbacks_needed,
        checkbacks_completed = nrow(campaign_checkbacks),
        completion_rate = round(nrow(campaign_checkbacks) / campaign$checkbacks_needed * 100, 1),
        avg_days_to_checkback = ifelse(nrow(campaign_checkbacks) > 0, 
                                     round(mean(campaign_checkbacks$days_to_checkback, na.rm = TRUE), 1), 
                                     NA)
      )
    }
    
    if (length(results) > 0) {
      return(do.call(rbind, results))
    } else {
      return(NULL)
    }
  })
  
  # Value boxes
  output$total_treatments <- renderValueBox({
    treatments <- treatment_campaigns()
    value <- ifelse(is.null(treatments), 0, nrow(treatments))
    
    valueBox(
      value = value,
      subtitle = "Treatment Campaigns",
      icon = icon("plane"),
      color = "blue"
    )
  })
  
  output$sites_treated <- renderValueBox({
    treatments <- treatment_campaigns()
    value <- ifelse(is.null(treatments), 0, sum(treatments$sites_treated, na.rm = TRUE))
    
    valueBox(
      value = value,
      subtitle = "Sites Treated",
      icon = icon("map-marker"),
      color = "green"
    )
  })
  
  output$checkbacks_needed <- renderValueBox({
    treatments <- treatment_campaigns()
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
  
  # Treatment campaigns table
  output$treatment_campaigns <- DT::renderDataTable({
    campaigns <- treatment_campaigns()
    
    if (is.null(campaigns)) {
      return(data.frame(Message = "No treatment campaigns found"))
    }
    
    display_data <- campaigns %>%
      select(
        Campaign = campaign_name,
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
        Campaign = campaign_name,
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
}

# Run the application
shinyApp(ui = ui, server = server)