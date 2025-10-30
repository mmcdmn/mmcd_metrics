# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(DT)
  library(plotly)
  library(tidyr)
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "Ground Prehatch Progress",
    # Add filters to the header using tags$li - moved expiry controls to the right
    tags$li(
      style = "padding: 5px 8px; margin: 2px;",
      selectizeInput("facility_filter", "Facility:",
                    choices = c("All" = "all"),
                    selected = "all", 
                    multiple = TRUE,
                    options = list(placeholder = "Select facilities...")),
      class = "dropdown"
    ),
    tags$li(
      style = "padding: 5px 8px; margin: 2px;",
      selectizeInput("foreman_filter", "FOS:",
                    choices = c("All" = "all"),
                    selected = "all",
                    multiple = TRUE,
                    options = list(placeholder = "Select FOS...")),
      class = "dropdown"
    ),
    tags$li(
      style = "padding: 5px 8px; margin: 2px;",
      checkboxGroupInput("zone_filter", "Filter by Zone:",
                        choices = c("P1" = "1", "P2" = "2"),
                        selected = c("1", "2"),
                        inline = TRUE),
      class = "dropdown"
    ),
    tags$li(
      style = "padding: 5px 8px; margin: 2px;",
      radioButtons("group_by", "Group by:",
                  choices = c("All MMCD" = "mmcd_all",
                             "Facility" = "facility", 
                             "FOS" = "foreman",
                             "Section" = "sectcode"),
                  selected = "facility",
                  inline = TRUE),
      class = "dropdown"
    ),
    tags$li(
      style = "padding: 5px 8px; margin: 2px;",
      dateInput("custom_today", "Pretend Today is:",
               value = Sys.Date(), 
               format = "yyyy-mm-dd"),
      class = "dropdown"
    ),
    tags$li(
      style = "padding: 5px 8px; margin: 2px; float: right;",
      checkboxInput("show_expiring_only", "Expiring Only", value = FALSE),
      class = "dropdown"
    ),
    tags$li(
      style = "padding: 5px 8px; margin: 2px; float: right;",
      sliderInput("expiring_days", "Days Until Expiring:",
                 min = 1, max = 60, value = 14, step = 1),
      class = "dropdown"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Progress Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Detailed View", tabName = "details", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    # Add minimal CSS to prevent header overlap with sidebar tabs
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          margin-top: 130px !important;
        }
        .main-header {
          height: 120px !important;
          background-color: #3c8dbc !important;
        }
        .main-header .navbar {
          height: 120px !important;
          background-color: #3c8dbc !important;
        }
        .main-sidebar {
          margin-top: 120px !important;
        }
        .skin-blue .main-header .navbar {
          background-color: #3c8dbc !important;
        }
        .skin-blue .main-header .logo {
          background-color: #367fa9 !important;
        }
      "))
    ),
    tabItems(
            # Overview tab
      tabItem(tabName = "overview",
        fluidRow(
          box(
            title = "Summary Statistics",
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              valueBoxOutput("total_sites", width = 2),
              valueBoxOutput("prehatch_sites", width = 2),
              valueBoxOutput("treated_sites", width = 2),
              valueBoxOutput("needs_treatment", width = 2),
              valueBoxOutput("treated_pct", width = 2),
              valueBoxOutput("expiring_pct", width = 2)
            ),
            conditionalPanel(
              condition = "input.group_by == 'sectcode'",
              div(
                style = "background-color: #f8f9fa; padding: 8px; border-radius: 4px; margin-top: 10px;",
                HTML("<i class='fa fa-info-circle' style='color: #17a2b8;'></i> 
                     <strong>Note:</strong> Section filtering is only available when a specific facility is selected (not 'All').")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Ground Prehatch Treatment Progress",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("progress_chart")
          )
        )
      ),
      
      # Details tab  
      tabItem(tabName = "details",
        fluidRow(
          box(
            title = "Details-Specific Filters",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            selectInput("details_status_filter", "Treatment Status:",
                       choices = c("All" = "all",
                                 "Needs Treatment" = "no_treatment",
                                 "Currently Treated" = "treated", 
                                 "Expiring Soon" = "expiring",
                                 "Expired Treatment" = "expired"),
                       selected = "all")
          )
        ),
        fluidRow(
          box(
            title = "Ground Prehatch Progress Details",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput("details_status_filter", "Treatment Status Filter:",
                       choices = c("All" = "all",
                                 "Needs Treatment" = "no_treatment",
                                 "Currently Treated" = "treated", 
                                 "Expiring Soon" = "expiring",
                                 "Expired Treatment" = "expired"),
                       selected = "all"),
            div(style = "margin-bottom: 10px;",
                downloadButton("download_details_data", "Download Table Data (CSV)", 
                             class = "btn-success btn-sm")
            ),
            DTOutput("details_table")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Initialize facility choices from db_helpers
  observe({
    facility_choices <- get_facility_choices(include_all = TRUE)
    # Add "None" option at the beginning
    facility_choices <- c("None (No Data)" = "none", facility_choices)
    updateSelectizeInput(session, "facility_filter", choices = facility_choices, selected = "all")
  })
  
  # Initialize foreman choices from db_helpers  
  observe({
    foremen_lookup <- get_foremen_lookup()
    foremen_choices <- c("All" = "all")
    foremen_choices <- c(
      foremen_choices,
      setNames(foremen_lookup$emp_num, foremen_lookup$shortname)
    )
    updateSelectizeInput(session, "foreman_filter", choices = foremen_choices, selected = "all")
  })
  
  # Note: Section grouping is always allowed - user guidance provided in UI conditional panel

  # Note: Section grouping is now always available regardless of facility selection
  # The conditional panel in the UI provides user guidance about section filtering
  
  # Fetch ground prehatch data
  ground_data <- reactive({
    req(input$zone_filter, input$group_by)  # Ensure required inputs are available
    
    con <- get_db_connection()
    if (is.null(con)) return(data.frame())
    
    tryCatch({
      # Use the provided SQL query with slight modifications for current year
      current_year <- format(Sys.Date(), "%Y")
      
      # Use custom date if provided, otherwise use current date
      simulation_date <- if (!is.null(input$custom_today)) input$custom_today else Sys.Date()
      
      query <- sprintf("
WITH ActiveSites_g AS (
  SELECT sc.facility, sc.zone, sc.fosarea, left(b.sitecode,7) AS sectcode,
         b.sitecode, acres, air_gnd, priority, prehatch, drone, remarks,
         sc.fosarea as foreman
  FROM loc_breeding_sites b
  LEFT JOIN gis_sectcode sc ON left(b.sitecode,7)=sc.sectcode
  WHERE (b.enddate IS NULL OR b.enddate>'%s-05-01')
    AND b.air_gnd='G'
  ORDER BY sc.facility, sc.sectcode, b.sitecode, b.prehatch
)
SELECT sitecnts.facility, sitecnts.zone, sitecnts.fosarea, sitecnts.sectcode, sitecnts.foreman,
       tot_ground, not_prehatch_sites, prehatch_sites_cnt, drone_sites_cnt,
       COALESCE(ph_treated_cnt, 0) AS ph_treated_cnt,
       COALESCE(ph_expiring_cnt, 0) AS ph_expiring_cnt,
       COALESCE(ph_expired_cnt, 0) AS ph_expired_cnt,
       prehatch_sites_cnt-(COALESCE(ph_treated_cnt,0)+COALESCE(ph_expiring_cnt, 0)) AS ph_notactivetrt_cnt
FROM
(SELECT facility, zone, fosarea, sectcode, foreman,
        COUNT(CASE WHEN (air_gnd='G') THEN 1 END) AS tot_ground,
        COUNT(CASE WHEN (air_gnd='G' AND prehatch IS NULL) THEN 1 END) AS not_prehatch_sites,
        COUNT(CASE WHEN (air_gnd='G' AND prehatch IN ('PREHATCH','BRIQUET')) THEN 1 END) AS prehatch_sites_cnt,
        COUNT(CASE WHEN (air_gnd='G' AND drone IN ('Y','M','C')) THEN 1 END) AS drone_sites_cnt
 FROM ActiveSites_g a
 GROUP BY facility, zone, fosarea, sectcode, foreman
 ORDER BY facility, zone, fosarea, sectcode, foreman) sitecnts
LEFT JOIN(
SELECT facility, zone, fosarea, sectcode, foreman,
       COUNT(CASE WHEN (prehatch_status='treated') THEN 1 END) AS ph_treated_cnt,
       COUNT(CASE WHEN (prehatch_status='expiring') THEN 1 END) AS ph_expiring_cnt,
       COUNT(CASE WHEN (prehatch_status='expired') THEN 1 END) AS ph_expired_cnt
FROM (  
  SELECT facility, zone, fosarea, sectcode, foreman, sitecode,
         CASE
           WHEN age > COALESCE(effect_days::integer, 150)::double precision THEN 'expired'::text
           WHEN days_retrt_early IS NOT NULL AND age > days_retrt_early::double precision THEN 'expiring'::text
           WHEN age<= effect_days::integer::double precision THEN 'treated'::text
           ELSE 'unknown'::text
         END AS prehatch_status,
         inspdate, matcode, age, effect_days, days_retrt_early
  FROM (  
    SELECT DISTINCT ON (a.sitecode)
           a.sitecode, a.sectcode, a.facility, a.fosarea, a.zone, a.foreman,
           c.pkey_pg AS insptrt_id,
           date_part('days'::text, '%s'::timestamp - c.inspdate::timestamp with time zone) AS age,
           c.matcode, c.inspdate, p.effect_days, p.days_retrt_early
    FROM (SELECT * FROM dblarv_insptrt_current WHERE inspdate>'%s-01-01' AND inspdate <= '%s') c
    JOIN activesites_g a ON c.sitecode = a.sitecode
    JOIN (SELECT * FROM mattype_list_targetdose WHERE prehatch IS TRUE) p USING (matcode)
    ORDER BY a.sitecode, c.inspdate DESC, c.insptime DESC
  ) s_grd
  ORDER BY sitecode
) list
GROUP BY facility, zone, fosarea, sectcode, foreman
ORDER BY facility, zone, fosarea, sectcode, foreman
) trtcnts ON trtcnts.sectcode = sitecnts.sectcode AND COALESCE(trtcnts.foreman, '') = COALESCE(sitecnts.foreman, '')
ORDER BY sectcode", current_year, simulation_date, current_year, simulation_date)
      
      result <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      # Map facility names for display
      result <- map_facility_names(result)
      
      return(result)
      
    }, error = function(e) {
      warning(paste("Error loading ground prehatch data:", e$message))
      if (!is.null(con)) dbDisconnect(con)
      return(data.frame())
    })
  })
  
  # Fetch individual site details for expiring sites table
  site_details <- reactive({
    con <- get_db_connection()
    if (is.null(con)) return(data.frame())
    
    tryCatch({
      current_year <- format(Sys.Date(), "%Y")
      
      # Use the expiring_days input for calculations
      expiring_days <- input$expiring_days
      if (is.null(expiring_days)) expiring_days <- 14
      
      # Use custom date if provided, otherwise use current date
      simulation_date <- if (!is.null(input$custom_today)) input$custom_today else Sys.Date()
      
      query <- sprintf("
WITH ActiveSites_g AS (
  SELECT sc.facility, sc.zone, sc.fosarea, left(sitecode,7) AS sectcode,
         sitecode, acres, air_gnd, priority, prehatch, drone, remarks
  FROM loc_breeding_sites b
  LEFT JOIN gis_sectcode sc ON left(b.sitecode,7)=sc.sectcode
  WHERE (enddate IS NULL OR enddate>'%s-05-01')
    AND air_gnd='G'
  ORDER BY facility, sectcode, sitecode, prehatch
)
SELECT a.facility, a.zone, a.fosarea, a.sectcode, a.sitecode,
       a.acres, a.priority, a.prehatch, a.drone,
       CASE
         WHEN age > COALESCE(effect_days::integer, 150)::double precision THEN 'expired'::text
         WHEN days_retrt_early IS NOT NULL AND age > (days_retrt_early::double precision - %d) THEN 'expiring'::text
         WHEN age <= effect_days::integer::double precision THEN 'treated'::text
         ELSE 'no_treatment'::text
       END AS prehatch_status,
       s_grd.inspdate, s_grd.matcode, s_grd.age, s_grd.effect_days, s_grd.days_retrt_early
FROM ActiveSites_g a
LEFT JOIN (  
  SELECT DISTINCT ON (a.sitecode)
         a.sitecode, a.sectcode, a.facility, a.fosarea, a.zone,
         c.pkey_pg AS insptrt_id,
         date_part('days'::text, '%s'::timestamp - c.inspdate::timestamp with time zone) AS age,
         c.matcode, c.inspdate, p.effect_days, p.days_retrt_early
  FROM (SELECT * FROM dblarv_insptrt_current WHERE inspdate>'%s-01-01' AND inspdate <= '%s') c
  JOIN activesites_g a ON c.sitecode = a.sitecode
  JOIN (SELECT * FROM mattype_list_targetdose WHERE prehatch IS TRUE) p USING (matcode)
  ORDER BY a.sitecode, c.inspdate DESC, c.insptime DESC
) s_grd ON s_grd.sitecode = a.sitecode
WHERE a.prehatch IN ('PREHATCH','BRIQUET')
ORDER BY a.facility, a.sectcode, a.sitecode", current_year, expiring_days, simulation_date, current_year, simulation_date)
      
      result <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      # Map facility names for display
      result <- map_facility_names(result)
      
      return(result)
      
    }, error = function(e) {
      warning(paste("Error loading site details:", e$message))
      if (!is.null(con)) dbDisconnect(con)
      return(data.frame())
    })
  })
  
  # Update foreman filter based on facility selection
  observe({
    data <- ground_data()
    
    # Filter by facility if not 'all'
    if (!is.null(input$facility_filter) && !("all" %in% input$facility_filter)) {
      data <- data %>% filter(facility %in% input$facility_filter)
    }
    
    # Get foremen lookup to map empnum to names
    foremen_lookup <- get_foremen_lookup()
    
    # Get unique fosarea values and create foreman choices with names
    foreman_choices <- c("None (No Data)" = "none", "All" = "all")
    if (nrow(data) > 0 && "fosarea" %in% names(data) && nrow(foremen_lookup) > 0) {
      unique_foremen <- sort(unique(na.omit(data$fosarea)))
      
      # Map empnum to shortname for display
      for (empnum in unique_foremen) {
        foreman_row <- foremen_lookup[foremen_lookup$emp_num == empnum, ]
        if (nrow(foreman_row) > 0) {
          display_name <- foreman_row$shortname[1]
          foreman_choices <- c(foreman_choices, setNames(empnum, display_name))
        } else {
          # Fallback to empnum if not found in lookup
          foreman_choices <- c(foreman_choices, setNames(empnum, empnum))
        }
      }
    }
    
    updateSelectizeInput(session, "foreman_filter", choices = foreman_choices, selected = "all")
  })
  
  # Filter data based on user selections
  filtered_data <- reactive({
    req(input$zone_filter, input$group_by, input$facility_filter)
    # Don't require foreman_filter since it starts as NULL
    
    data <- ground_data()
    
    if (nrow(data) == 0) return(data)
    
    # Return empty data if "none" is selected for facility or foreman
    if (!is.null(input$facility_filter) && ("none" %in% input$facility_filter) && length(input$facility_filter) == 1) {
      return(data.frame())
    }
    if (!is.null(input$foreman_filter) && ("none" %in% input$foreman_filter) && length(input$foreman_filter) == 1) {
      return(data.frame())
    }
    
    # Filter by zone if selected
    if (!is.null(input$zone_filter) && length(input$zone_filter) > 0) {
      data <- data %>% filter(zone %in% input$zone_filter)
    }
    
    # Filter by facility
    if (!is.null(input$facility_filter) && !("all" %in% input$facility_filter)) {
      data <- data %>% filter(facility %in% input$facility_filter)
    }
    
    # Filter by foreman
    if (!is.null(input$foreman_filter) && !("all" %in% input$foreman_filter)) {
      data <- data %>% filter(foreman %in% input$foreman_filter)
    }
    
    return(data)
  })
  
  # Aggregate data based on grouping level  
  aggregated_data <- reactive({
    data <- filtered_data()
    
    if (nrow(data) == 0) return(data)

    # Map facility names for display
    data <- map_facility_names(data)

    # If "Show Only Expiring Sites" is checked, filter to only expiring sites before aggregation
    if (input$show_expiring_only) {
      # Get site details to filter to expiring sites only
      site_data <- site_details()
      expiring_sites <- site_data %>% filter(prehatch_status == "expiring")
      
      if (nrow(expiring_sites) == 0) {
        return(data.frame())
      }
      
      # Filter based on grouping level
      if (input$group_by == "facility") {
        data <- data %>% filter(facility %in% expiring_sites$facility)
      } else if (input$group_by == "foreman") {
        data <- data %>% filter(foreman %in% expiring_sites$foreman)
      } else if (input$group_by == "sectcode") {
        data <- data %>% filter(sectcode %in% expiring_sites$sectcode)
      }
      
      if (nrow(data) == 0) {
        return(data.frame())
      }
    }

    # Check if we should show zones as separate entities
    show_zones_separately <- length(input$zone_filter) > 1

    # Define grouping and display based on group_by selection
    if (input$group_by == "mmcd_all") {
      if (show_zones_separately) {
        # Group by zone when both P1 and P2 are selected
        result <- data %>%
          group_by(zone) %>%
          summarise(
            tot_ground = sum(tot_ground, na.rm = TRUE),
            not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
            prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
            drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
            ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
            ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
            ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
            ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            display_name = paste0("MMCD (All) - P", zone),
            group_name = "mmcd_all",
            combined_group = paste0("MMCD (All) (P", zone, ")")
          )
      } else {
        # Aggregate everything together for MMCD (All)
        result <- data %>%
          summarise(
            tot_ground = sum(tot_ground, na.rm = TRUE),
            not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
            prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
            drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
            ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
            ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
            ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
            ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            display_name = "MMCD (All)",
            group_name = "mmcd_all"
          )
      }
    } else if (input$group_by == "facility") {
      if (show_zones_separately) {
        # Group by facility and zone
        result <- data %>%
          group_by(facility, facility_display, zone) %>%
          summarise(
            tot_ground = sum(tot_ground, na.rm = TRUE),
            not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
            prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
            drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
            ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
            ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
            ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
            ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            display_name = paste0(facility_display, " (P", zone, ")"),
            group_name = facility,
            combined_group = paste0(facility, " (P", zone, ")")
          )
      } else {
        # Group by facility only
        result <- data %>%
          group_by(facility, facility_display) %>%
          summarise(
            tot_ground = sum(tot_ground, na.rm = TRUE),
            not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
            prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
            drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
            ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
            ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
            ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
            ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            display_name = facility_display,
            group_name = facility
          )
      }
    } else if (input$group_by == "foreman") {
      # Get foreman lookup for display names
      foremen_lookup <- get_foremen_lookup()
      
      if (show_zones_separately) {
        # Group by foreman and zone
        result <- data %>%
          group_by(foreman, zone) %>%
          summarise(
            tot_ground = sum(tot_ground, na.rm = TRUE),
            not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
            prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
            drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
            ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
            ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
            ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
            ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            foreman_name = sapply(foreman, function(f) {
              matches <- which(trimws(as.character(foremen_lookup$emp_num)) == trimws(as.character(f)))
              if(length(matches) > 0) foremen_lookup$shortname[matches[1]] else paste0("FOS #", f)
            }),
            display_name = paste0(foreman_name, " (P", zone, ")"),
            group_name = foreman,
            combined_group = paste0(foreman, " (P", zone, ")")
          )
      } else {
        # Group by foreman only
        result <- data %>%
          group_by(foreman) %>%
          summarise(
            tot_ground = sum(tot_ground, na.rm = TRUE),
            not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
            prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
            drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
            ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
            ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
            ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
            ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            foreman_name = sapply(foreman, function(f) {
              matches <- which(trimws(as.character(foremen_lookup$emp_num)) == trimws(as.character(f)))
              if(length(matches) > 0) foremen_lookup$shortname[matches[1]] else paste0("FOS #", f)
            }),
            display_name = foreman_name,
            group_name = foreman
          )
      }
    } else if (input$group_by == "sectcode") {
      # Section grouping - always show individual sections, but filter by zone
      result <- data %>%
        group_by(facility, facility_display, zone, sectcode) %>%
        summarise(
          tot_ground = sum(tot_ground, na.rm = TRUE),
          not_prehatch_sites = sum(not_prehatch_sites, na.rm = TRUE),
          prehatch_sites_cnt = sum(prehatch_sites_cnt, na.rm = TRUE),
          drone_sites_cnt = sum(drone_sites_cnt, na.rm = TRUE),
          ph_treated_cnt = sum(ph_treated_cnt, na.rm = TRUE),
          ph_expiring_cnt = sum(ph_expiring_cnt, na.rm = TRUE),
          ph_expired_cnt = sum(ph_expired_cnt, na.rm = TRUE),
          ph_notactivetrt_cnt = sum(ph_notactivetrt_cnt, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          display_name = paste0(facility_display, " - ", sectcode),
          group_name = sectcode
        )
    }
    
    return(result)
  })
  
  # Value boxes
  output$total_sites <- renderValueBox({
    data <- filtered_data()
    total <- if(nrow(data) > 0) sum(data$tot_ground, na.rm = TRUE) else 0
    
    valueBox(
      value = total,
      subtitle = "Total Ground Sites",
      icon = icon("map-marker"),
      color = "blue"
    )
  })
  
  output$prehatch_sites <- renderValueBox({
    data <- filtered_data()
    total <- if(nrow(data) > 0) sum(data$prehatch_sites_cnt, na.rm = TRUE) else 0
    
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = total,
      subtitle = "Prehatch Sites",
      icon = icon("egg"),
      color = shiny_colors["planned"]
    )
  })
  
  output$treated_sites <- renderValueBox({
    data <- filtered_data()
    total <- if(nrow(data) > 0) sum(data$ph_treated_cnt, na.rm = TRUE) else 0
    
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = total,
      subtitle = "Treated Sites",
      icon = icon("check-circle"),
      color = shiny_colors["active"]
    )
  })
  
  output$needs_treatment <- renderValueBox({
    data <- filtered_data()
    total <- if(nrow(data) > 0) sum(data$ph_notactivetrt_cnt, na.rm = TRUE) else 0
    
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = total,
      subtitle = "Needs Treatment",
      icon = icon("exclamation-triangle"),
      color = shiny_colors["needs_treatment"]
    )
  })
  
  output$treated_pct <- renderValueBox({
    data <- filtered_data()
    if (nrow(data) > 0) {
      total_treated <- sum(data$ph_treated_cnt, na.rm = TRUE)
      total_prehatch <- sum(data$prehatch_sites_cnt, na.rm = TRUE)
      pct <- if(total_prehatch > 0) round((total_treated / total_prehatch) * 100, 1) else 0
      value_text <- paste0(pct, "%")
    } else {
      value_text <- "0%"
    }
    
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = value_text,
      subtitle = "% Treated",
      icon = icon("check-circle"),
      color = shiny_colors["active"]
    )
  })
  
  output$expiring_pct <- renderValueBox({
    data <- filtered_data()
    if (nrow(data) > 0) {
      total_expiring <- sum(data$ph_expiring_cnt, na.rm = TRUE)
      total_prehatch <- sum(data$prehatch_sites_cnt, na.rm = TRUE)
      pct <- if(total_prehatch > 0) round((total_expiring / total_prehatch) * 100, 1) else 0
      value_text <- paste0(pct, "%")
    } else {
      value_text <- "0%"
    }
    
    shiny_colors <- get_shiny_colors()
    valueBox(
      value = value_text,
      subtitle = "% Expiring",
      icon = icon("clock"),
      color = shiny_colors["planned"]
    )
  })
  
  # Progress chart
  output$progress_chart <- renderPlotly({
    data <- aggregated_data()
    
    if (nrow(data) == 0) {
      # Create an empty plot with a helpful message
      empty_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                label = if(input$show_expiring_only) {
                  paste("No expiring sites found within", input$expiring_days, "days\nwith the selected filters")
                } else {
                  "No data available with selected filters"
                }, 
                size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        xlim(0, 1) + 
        ylim(0, 1)
      
      return(ggplotly(empty_plot) %>% layout(height = 400))
    }
    
    # Calculate dynamic height based on number of rows
    # Minimum 500px, add 70px per row, maximum 25500px for better spacing
    dynamic_height <- max(500, min(25500, nrow(data) * 70 + 200))
    
    # Get colors from centralized db_helpers
    status_colors <- get_status_colors()
    
    # Prepare data for stacked bar chart
    chart_data <- data %>%
      select(display_name, ph_treated_cnt, ph_expiring_cnt, ph_expired_cnt, ph_notactivetrt_cnt) %>%
      tidyr::pivot_longer(cols = -display_name, names_to = "status", values_to = "count") %>%
      mutate(
        status_label = case_when(
          status == "ph_treated_cnt" ~ "Treated",
          status == "ph_expiring_cnt" ~ "Expiring",
          status == "ph_expired_cnt" ~ "Expired",
          status == "ph_notactivetrt_cnt" ~ "Needs Treatment"
        ),
        color = case_when(
          status == "ph_treated_cnt" ~ unname(status_colors["active"]),
          status == "ph_expiring_cnt" ~ unname(status_colors["planned"]),
          status == "ph_expired_cnt" ~ unname(status_colors["unknown"]),
          status == "ph_notactivetrt_cnt" ~ unname(status_colors["needs_treatment"])
        )
      )

    # If showing expiring only, filter chart data to only show expiring status
    if (input$show_expiring_only) {
      chart_data <- chart_data %>% filter(status == "ph_expiring_cnt")
    }

    # Create title based on filters
    chart_title <- if (input$show_expiring_only) {
      paste("Ground Prehatch Expiring Sites (Within", input$expiring_days, "Days)")
    } else {
      "Ground Prehatch Treatment Progress"
    }
    
    p <- ggplot(chart_data, aes(x = reorder(display_name, count), y = count, fill = status_label)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = setNames(chart_data$color, chart_data$status_label)) +
      coord_flip() +
      labs(
        title = chart_title,
        x = case_when(
          input$group_by == "mmcd_all" ~ "MMCD",
          input$group_by == "facility" ~ "Facility",
          input$group_by == "foreman" ~ "FOS",
          input$group_by == "sectcode" ~ "Section"
        ),
        y = "Number of Sites",
        fill = "Treatment Status"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 13),
        axis.text.y = element_text(size = 12, margin = margin(r = 8)),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.y = element_line(color = "grey90", size = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey95", size = 0.3),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(
        height = dynamic_height,
        margin = list(l = 150, r = 50, t = 80, b = 80),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.1,
          xanchor = "left",
          yanchor = "top"
        )
      )
  })
  
  # Separate reactive for details data with independent filters
  details_data <- reactive({
    # Get expiring days parameter
    current_expiring_days <- input$expiring_days
    if (is.null(current_expiring_days)) current_expiring_days <- 14
    
    # Use custom date if provided, otherwise use current date
    simulation_date <- if (!is.null(input$custom_today)) input$custom_today else Sys.Date()
    
    con <- get_db_connection()
    if (is.null(con)) return(data.frame())
    
    tryCatch({
      current_year <- format(Sys.Date(), "%Y")
      
      query <- sprintf("
WITH ActiveSites_g AS (
  SELECT sc.facility, sc.zone, sc.fosarea, left(sitecode,7) AS sectcode,
         sitecode, acres, air_gnd, priority, prehatch, drone, remarks
  FROM loc_breeding_sites b
  LEFT JOIN gis_sectcode sc ON left(b.sitecode,7)=sc.sectcode
  WHERE (enddate IS NULL OR enddate>'%s-05-01')
    AND air_gnd='G'
  ORDER BY facility, sectcode, sitecode, prehatch
)
SELECT a.facility, a.zone, a.fosarea, a.sectcode, a.sitecode,
       a.acres, a.priority, a.prehatch, a.drone,
       CASE
         WHEN age > COALESCE(effect_days::integer, 150)::double precision THEN 'expired'::text
         WHEN days_retrt_early IS NOT NULL AND age > (days_retrt_early::double precision - %d) THEN 'expiring'::text
         WHEN age <= effect_days::integer::double precision THEN 'treated'::text
         ELSE 'no_treatment'::text
       END AS prehatch_status,
       s_grd.inspdate, s_grd.matcode, s_grd.age, s_grd.effect_days, s_grd.days_retrt_early
FROM ActiveSites_g a
LEFT JOIN (  
  SELECT DISTINCT ON (a.sitecode)
         a.sitecode, a.sectcode, a.facility, a.fosarea, a.zone,
         c.pkey_pg AS insptrt_id,
         date_part('days'::text, '%s'::timestamp - c.inspdate::timestamp with time zone) AS age,
         c.matcode, c.inspdate, p.effect_days, p.days_retrt_early
  FROM (SELECT * FROM dblarv_insptrt_current WHERE inspdate>'%s-01-01' AND inspdate <= '%s') c
  JOIN activesites_g a ON c.sitecode = a.sitecode
  JOIN (SELECT * FROM mattype_list_targetdose WHERE prehatch IS TRUE) p USING (matcode)
  ORDER BY a.sitecode, c.inspdate DESC, c.insptime DESC
) s_grd ON s_grd.sitecode = a.sitecode
WHERE a.prehatch IN ('PREHATCH','BRIQUET')
ORDER BY a.facility, a.sectcode, a.sitecode", current_year, current_expiring_days, simulation_date, current_year, simulation_date)
      
      result <- dbGetQuery(con, query)
      dbDisconnect(con)
      
      # Map facility names for display
      result <- map_facility_names(result)
      
      # Return empty data if "none" is selected for facility or foreman
      if (!is.null(input$facility_filter) && ("none" %in% input$facility_filter) && length(input$facility_filter) == 1) {
        return(data.frame())
      }
      if (!is.null(input$foreman_filter) && ("none" %in% input$foreman_filter) && length(input$foreman_filter) == 1) {
        return(data.frame())
      }
      
      # Apply shared filters (using main filters, not separate details filters)
      if (!is.null(input$facility_filter) && !("all" %in% input$facility_filter)) {
        result <- result %>% filter(facility %in% input$facility_filter)
      }
      
      # Filter by foreman (fosarea)
      if (!is.null(input$foreman_filter) && !("all" %in% input$foreman_filter)) {
        result <- result %>% filter(fosarea %in% input$foreman_filter)
      }
      
      # Filter by status (this is the only separate filter for details)
      if (!is.null(input$details_status_filter) && input$details_status_filter != "all") {
        result <- result %>% filter(prehatch_status == input$details_status_filter)
      }
      
      return(result)
      
    }, error = function(e) {
      warning(paste("Error loading details data:", e$message))
      if (!is.null(con)) dbDisconnect(con)
      return(data.frame())
    })
  })
  
  # Details table
  output$details_table <- renderDT({
    data <- details_data()
    
    if (nrow(data) == 0) {
      return(datatable(data.frame(Message = "No data available with selected filters"), options = list(pageLength = 10)))
    }
    # Get foremen lookup for display names
    foremen_lookup <- get_foremen_lookup()
    
    # Map fosarea (empnum) to foreman names
    data$foreman_name <- NA
    for (i in 1:nrow(data)) {
      if (!is.na(data$fosarea[i])) {
        foreman_row <- foremen_lookup[foremen_lookup$emp_num == data$fosarea[i], ]
        if (nrow(foreman_row) > 0) {
          data$foreman_name[i] <- foreman_row$shortname[1]
        } else {
          data$foreman_name[i] <- data$fosarea[i]  # Fallback to empnum
        }
      }
    }
    
    # Format data for display
    display_data <- data %>%
      select(
        Facility = facility_display,
        `Priority Zone` = zone,
        Section = sectcode,
        Sitecode = sitecode,
        FOS = foreman_name,
        Acres = acres,
        Priority = priority,
        `Treatment Type` = prehatch,
        Status = prehatch_status,
        `Last Treatment` = inspdate,
        `Days Since Last Treatment` = age,
        Material = matcode,
        `Effect Days` = effect_days
      ) %>%
      arrange(Facility, Section, Sitecode)
    
    # Round numeric columns
    if ("Days Since Last Treatment" %in% names(display_data)) {
      display_data$`Days Since Last Treatment` <- round(as.numeric(display_data$`Days Since Last Treatment`), 1)
    }
    if ("Acres" %in% names(display_data)) {
      display_data$Acres <- round(as.numeric(display_data$Acres), 2)
    }
    
    datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 5:12)
        )
      ),
      rownames = FALSE
    )
  })
  
  # Download handler for details data
  output$download_details_data <- downloadHandler(
    filename = function() {
      paste("ground_prehatch_details_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- details_data()
      if (nrow(data) > 0) {
        # Get foremen lookup for display names
        foremen_lookup <- get_foremen_lookup()
        
        # Map fosarea (empnum) to foreman names
        data$foreman_name <- NA
        for (i in 1:nrow(data)) {
          if (!is.na(data$fosarea[i])) {
            foreman_row <- foremen_lookup[foremen_lookup$emp_num == data$fosarea[i], ]
            if (nrow(foreman_row) > 0) {
              data$foreman_name[i] <- foreman_row$shortname[1]
            } else {
              data$foreman_name[i] <- data$fosarea[i]  # Fallback to empnum
            }
          }
        }
        
        # Format for download (same as displayed data)
        download_data <- data %>%
          select(
            Facility = facility_display,
            `Priority Zone` = zone,
            Section = sectcode,
            Sitecode = sitecode,
            FOS = foreman_name,
            Acres = acres,
            Priority = priority,
            `Treatment Type` = prehatch,
            Status = prehatch_status,
            `Last Treatment` = inspdate,
            `Days Since Last Treatment` = age,
            Material = matcode,
            `Effect Days` = effect_days
          ) %>%
          arrange(Facility, Section, Sitecode)
        
        # Round numeric columns
        if ("Days Since Last Treatment" %in% names(download_data)) {
          download_data$`Days Since Last Treatment` <- round(as.numeric(download_data$`Days Since Last Treatment`), 1)
        }
        if ("Acres" %in% names(download_data)) {
          download_data$Acres <- round(as.numeric(download_data$Acres), 2)
        }
      } else {
        download_data <- data.frame(Message = "No data available with current filters")
      }
      write.csv(download_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)