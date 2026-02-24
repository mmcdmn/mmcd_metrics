# Control Efficacy App
# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(shinyjs)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(DT)
  library(plotly)
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")
source("../../shared/server_utilities.R")
source("../../shared/stat_box_helpers.R")

# Source external function files
source("ui_helper.R")
source("data_functions.R")
source("efficacy_data_functions.R")
source("checkback_functions.R")
source("display_functions.R")

# Set application name for AWS RDS monitoring
set_app_name("control_efficacy")

# =============================================================================
# STARTUP OPTIMIZATION: Preload lookup tables into cache
# =============================================================================
message("[control_efficacy] Preloading lookup tables...")
tryCatch({
  get_facility_lookup()
  get_foremen_lookup()
  message("[control_efficacy] Lookup tables preloaded")
}, error = function(e) message("[control_efficacy] Preload warning: ", e$message))

# Suppress R CMD check notes for dplyr/ggplot2 NSE variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "facility", "inspdate", "date_diff", "round_start", "round_id", "sitecode",
    "acres", "start_date", "checkback_count", "last_treatment_date",
    "next_treatment_date", "checkback", "last_treatment", "daily_summary",
    "sites_treated", "timing_df", "days_to_checkback", "facility_display",
    "genus", "season", "trt_type", "trt_action", "pre_genus_pct", "post_genus_pct",
    "pre_genus_dips", "post_genus_dips", "pct_reduction", "pre_numdip", "post_numdip",
    "sampnum_yr", "per", "total_pct", "category"
  ))
}

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- control_efficacy_ui()

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {

  # ===========================================================================
  # HELP MODAL
  # ===========================================================================
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Control Efficacy App - Help",
      size = "l",
      easyClose = TRUE,

      h3("Key Definitions"),
      tags$dl(
        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Brood (Treatment Round)"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;",
          "A group of consecutive treatment days at the same facility.",
          "Consecutive means a maximum of 1 day gap between treatments.",
          "Each brood is identified by facility and start date (e.g., 'BBF-06/14')."
        ),

        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Treatment Types"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;",
          tags$ul(
            tags$li(tags$b("Air (A):"), " Aerial application"),
            tags$li(tags$b("Drone (D):"), " Drone application"),
            tags$li(tags$b("Ground (3):"), " Ground-based application")
          )
        ),

        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Checkback Inspection"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;",
          "An inspection conducted after a treatment to assess effectiveness.",
          "Must have a post-treatment dip count recorded (posttrt_p IS NOT NULL)."
        ),

        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Genus-Level Dip Count"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;",
          "For each sample, species data records what percentage belongs to each genus.",
          "The genus dip count is:",
          tags$br(),
          tags$code("genus_dips = numdip \u00D7 (sum of species % for that genus) / 100"),
          tags$br(), tags$br(),
          "Example: 50 dips, Aedes species = 80% \u2192 Aedes dips = 50 \u00D7 80/100 = 40"
        ),

        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Bti Filter"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;",
          "Filters treatments to only those using Bti-based larvicide materials.",
          "Uses the mattype_list table: active_ingredient LIKE '%Bti%' AND physinv_list = '(1) Larvicide'."
        ),

        tags$dt(style = "font-size: 16px; margin-top: 15px; color: #856404;", "Control Checkbacks"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;",
          "A ", tags$b("control checkback"), " occurs when ", tags$b("no treatments happened between"),
          " the pre-inspection and the post (checkback) inspection. The pre and post",
          " measurements are a natural population change baseline.",
          tags$br(), tags$br(),
          tags$b("How they affect results:"),
          tags$ul(
            tags$li("Control checkbacks are ", tags$b("excluded"), " from valid checkback counts and the efficacy display."),
            tags$li("They appear in a separate Control Checkback Details table in the Control Efficacy tab."),
            tags$li("When Mulla's formula is enabled, all control observations within a season and genus",
                     " are ", tags$b("averaged"), " to produce a single C1/C2 correction ratio.",
                     " This averaged ratio is then applied to every treated checkback in that season.",
                     " Individual controls are NOT matched 1-to-1 with treated checkbacks."),
            tags$li("If natural populations declined between control observations (C1/C2 > 1),",
                     " standard reductions were overstated — Mulla's will lower them.",
                     " If populations increased (C1/C2 < 1), Mulla's will raise them.")
          )
        )
      ),

      h3(style = "margin-top: 25px;", "Calculations"),
      tags$dl(
        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Standard % Reduction"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;",
          tags$code("% Reduction = (pre_genus_dips - post_genus_dips) / pre_genus_dips \u00D7 100"),
          tags$br(),
          "Example: Pre=40, Post=8 \u2192 (40-8)/40 = 80% reduction"
        ),

        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Mulla's Formula"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;",
          "When control data is available (checkbacks where treatment happened before",
          " the pre-inspection), the full correction is applied:",
          tags$br(),
          tags$code("% Reduction = 100 - (T2/T1) × (C1/C2) × 100"),
          tags$br(),
          "Where T = treated site, C = control site, 1 = pre count, 2 = post count.",
          tags$br(),
          "C1/C2 corrects for natural population change between observations.",
          tags$br(),
          "Control data is matched by season and genus. Without control data,",
          " the simplified version is used (mathematically equivalent to standard)."
        ),

        tags$dt(style = "font-size: 16px; margin-top: 15px; color: #d63031;", "Graph Display"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;",
          tags$b("Important:"), " Negative % reduction values are displayed as 0% in the box plot graph only.",
          " This affects the visual representation, median lines, box quartiles, and summary statistics",
          " (median reduction, mean reduction values shown above the graph).",
          tags$br(), tags$br(),
          tags$b("Data tables"), " continue to show the original negative values unchanged.",
          " This ensures you can still access the raw data while having a cleaner graph visualization."
        ),

        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Season Classification"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;",
          "Spring vs Summer is determined by the annual spring threshold date from",
          " lookup_threshold_larv (ACT4-P1). Treatment dates before the threshold = Spring;",
          " on or after = Summer."
        ),

        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Checkback Progress"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;",
          tags$b("Percentage Mode:"), " ceiling(sites_treated \u00D7 percentage / 100)",
          tags$br(),
          tags$b("Fixed Number Mode:"), " min(fixed_number, sites_treated)",
          tags$br(),
          tags$b("Completion Rate:"), " (completed / needed) \u00D7 100"
        )
      ),

      h3(style = "margin-top: 25px;", "Data Sources"),
      tags$dl(
        tags$dt(style = "font-size: 16px; margin-top: 15px;", "Database Tables"),
        tags$dd(style = "font-size: 14px; margin-left: 20px;",
          tags$ul(
            tags$li(tags$b("dblarv_insptrt_current/archive:"), " Inspections and treatments"),
            tags$li(tags$b("dblarv_species_current/archive:"), " Species composition per sample"),
            tags$li(tags$b("lookup_specieslist:"), " Species \u2192 genus mapping"),
            tags$li(tags$b("mattype_list + mattype_list_targetdose:"), " Material codes and properties"),
            tags$li(tags$b("gis_sectcode:"), " Facility and zone information"),
            tags$li(tags$b("lookup_threshold_larv:"), " Spring date thresholds")
          )
        )
      ),

      footer = modalButton("Close")
    ))
  })

  # ===========================================================================
  # TOGGLE FILTER SECTION
  # ===========================================================================
  
  # Track filter visibility state
  filters_visible <- reactiveVal(TRUE)
  
  # Toggle filter visibility (main button)
  observeEvent(input$toggle_filters, {
    current_state <- filters_visible()
    new_state <- !current_state
    filters_visible(new_state)
    
    # Update button text and icon
    if (new_state) {
      updateActionButton(session, "toggle_filters", 
        label = "Hide Filters", 
        icon = icon("eye-slash"))
      # Show sidebar column and restore normal layout
      shinyjs::removeClass("main_layout", "sidebar-hidden")
    } else {
      updateActionButton(session, "toggle_filters", 
        label = "Show Filters", 
        icon = icon("eye"))
      # Hide sidebar column and expand main content
      shinyjs::addClass("main_layout", "sidebar-hidden")
    }
  })
  
  # Toggle filter visibility (floating button)
  observeEvent(input$toggle_filters_floating, {
    filters_visible(TRUE)
    updateActionButton(session, "toggle_filters", 
      label = "Hide Filters", 
      icon = icon("eye-slash"))
    # Show sidebar column and restore normal layout
    shinyjs::removeClass("main_layout", "sidebar-hidden")
  })

  # ===========================================================================
  # SHARED: Helper to convert year range to date range
  # ===========================================================================
  date_range <- reactive({
    req(input$year_range)
    list(
      start = paste0(input$year_range[1], "-01-01"),
      end   = paste0(input$year_range[2], "-12-31")
    )
  })

  # ===========================================================================
  # SHARED: Initialize filter choices on app load
  # ===========================================================================
  observe({
    facility_choices <- get_facility_choices()
    updateSelectInput(session, "facility_filter", choices = facility_choices, selected = "all")
  })

  observe({
    matcodes <- get_treatment_material_choices()
    updateSelectInput(session, "matcode_filter_progress", choices = matcodes, selected = "all")
  })

  # Update dosage filter choices when material type changes
  observeEvent(input$material_type_filter, {
    ingredient <- input$material_type_filter
    con <- get_db_connection()
    if (!is.null(con)) {
      dosages <- load_dosage_options(con, if (ingredient == "all") NULL else ingredient)
      safe_disconnect(con)
      if (!is.null(dosages) && nrow(dosages) > 0) {
        labels <- unique(dosages$dosage_label)
        choices <- c("All Dosages" = "all", setNames(labels, labels))
      } else {
        choices <- c("All Dosages" = "all")
      }
    } else {
      choices <- c("All Dosages" = "all")
    }
    updateSelectInput(session, "dosage_filter", choices = choices, selected = "all")
  })

  # ===========================================================================
  # PROGRESS TAB REACTIVES
  # ===========================================================================
  treatment_data_progress <- eventReactive(input$refresh_data_progress, {
    req(date_range(), input$facility_filter, input$matcode_filter_progress)
    dr <- date_range()

    withProgress(message = "Loading treatment data...", value = 0.5, {
      load_treatment_data(
        start_date = dr$start,
        end_date = dr$end,
        facility_filter = input$facility_filter,
        matcode_filter = input$matcode_filter_progress
      )
    })
  })

  checkback_data_progress <- eventReactive(input$refresh_data_progress, {
    req(treatment_data_progress())
    dr <- date_range()

    withProgress(message = "Loading checkback data...", value = 0.5, {
      treatments <- treatment_data_progress()
      if (is.null(treatments) || nrow(treatments) == 0) return(NULL)

      treated_sites <- unique(treatments$sitecode)
      load_checkback_data(
        treated_sites = treated_sites,
        start_date = dr$start,
        end_date = dr$end
      )
    })
  })

  # Load ALL inspections for control checkback detection
  site_inspections_data <- eventReactive(input$refresh_data_progress, {
    req(treatment_data_progress())
    dr <- date_range()

    withProgress(message = "Loading inspection data for control detection...", value = 0.5, {
      treatments <- treatment_data_progress()
      if (is.null(treatments) || nrow(treatments) == 0) return(NULL)

      treated_sites <- unique(treatments$sitecode)
      load_site_inspections(
        treated_sites = treated_sites,
        start_date = dr$start,
        end_date = dr$end
      )
    })
  })

  treatment_rounds_progress <- eventReactive(input$refresh_data_progress, {
    req(treatment_data_progress())

    withProgress(message = "Calculating treatment rounds...", value = 0.5, {
      calculate_treatment_rounds(
        treatments = treatment_data_progress(),
        checkback_type = input$checkback_type_progress,
        checkback_percent = input$checkback_percent_progress,
        checkback_number = input$checkback_number_progress
      )
    })
  })

  checkback_status_progress <- eventReactive(input$refresh_data_progress, {
    req(treatment_rounds_progress())

    withProgress(message = "Calculating checkback status...", value = 0.5, {
      calculate_checkback_status(
        rounds = treatment_rounds_progress(),
        checkbacks = checkback_data_progress(),
        treatments = treatment_data_progress(),
        all_inspections = site_inspections_data()
      )
    })
  })

  # ===========================================================================
  # PROGRESS TAB OUTPUTS
  # ===========================================================================
  output$checkback_progress_chart <- renderPlotly({
    input$color_theme
    status <- checkback_status_progress()

    result <- create_checkback_progress_chart(
      checkback_status = status,
      theme = input$color_theme
    )

    if (is.list(result)) result$plot else result
  })

  output$checkback_progress_chart_ui <- renderUI({
    status <- checkback_status_progress()
    result <- create_checkback_progress_chart(
      checkback_status = status,
      theme = input$color_theme
    )
    height <- if (is.list(result)) result$height else 400
    plotlyOutput("checkback_progress_chart", height = paste0(height, "px"))
  })

  output$total_checkbacks_needed <- renderUI({
    status <- checkback_status_progress()
    total <- if (!is.null(status)) sum(status$checkbacks_needed, na.rm = TRUE) else 0
    status_colors <- get_status_colors(theme = input$color_theme)
    create_stat_box(value = total, title = "Total Checkbacks Needed",
                    bg_color = status_colors["unknown"], icon = icon("clipboard-list"))
  })

  output$total_checkbacks_completed <- renderUI({
    status <- checkback_status_progress()
    completed <- if (!is.null(status)) sum(status$checkbacks_completed, na.rm = TRUE) else 0
    status_colors <- get_status_colors(theme = input$color_theme)
    create_stat_box(value = completed, title = "Total Checkbacks Completed",
                    bg_color = status_colors["active"], icon = icon("check-circle"))
  })

  output$overall_completion_rate <- renderUI({
    status <- checkback_status_progress()
    status_colors <- get_status_colors(theme = input$color_theme)

    if (!is.null(status)) {
      needed <- sum(status$checkbacks_needed, na.rm = TRUE)
      completed <- sum(status$checkbacks_completed, na.rm = TRUE)
      rate <- if (needed > 0) round(completed / needed * 100, 1) else 0
      color <- if (rate >= 80) {
        status_colors["active"]
      } else if (rate >= 50) {
        status_colors["needs_action"]
      } else {
        status_colors["needs_treatment"]
      }
    } else {
      rate <- 0
      color <- status_colors["needs_treatment"]
    }

    create_stat_box(value = paste0(rate, "%"), title = "Overall Completion Rate",
                    bg_color = color, icon = icon("percentage"))
  })

  output$avg_days_to_checkback <- renderUI({
    status <- checkback_status_progress()
    avg <- if (!is.null(status)) round(mean(status$avg_days_to_checkback, na.rm = TRUE), 1) else 0
    status_colors <- get_status_colors(theme = input$color_theme)
    create_stat_box(value = avg, title = "Avg Days to Checkback",
                    bg_color = status_colors["planned"], icon = icon("calendar-day"))
  })

  # ===========================================================================
  # STATUS TABLES TAB: Shared data + outputs
  # ===========================================================================

  # Load species data for site details (status tables tab)
  species_data_for_checkbacks <- eventReactive(input$refresh_data_progress, {
    req(checkback_data_progress(), treatment_data_progress())
    dr <- date_range()

    withProgress(message = "Loading species data...", value = 0, {
      sample_ids <- c(
        checkback_data_progress()$sampnum_yr,
        treatment_data_progress()$presamp_yr,
        treatment_data_progress()$sampnum_yr
      )
      load_species_data_for_samples(
        sample_ids = sample_ids,
        start_date = dr$start,
        end_date = dr$end
      )
    })
  })

  site_details <- eventReactive(input$refresh_data_progress, {
    req(treatment_data_progress())
    create_site_details(
      treatments = treatment_data_progress(),
      checkbacks = checkback_data_progress(),
      species_data = species_data_for_checkbacks(),
      all_inspections = site_inspections_data()
    )
  })

  # Brood Status Table
  output$checkback_status_table <- DT::renderDataTable({
    status <- checkback_status_progress()

    if (is.null(status)) {
      return(data.frame(Message = "No data available"))
    }

    display_data <- status %>%
      select(
        Brood = round_name, Facility = facility,
        `Start Date` = start_date, `End Date` = end_date,
        `Sites Treated` = sites_treated,
        `Checkbacks Needed` = checkbacks_needed,
        `Checkbacks Completed` = checkbacks_completed,
        `Completion Rate (%)` = completion_rate,
        `Avg Days to Checkback` = avg_days_to_checkback
      )

    DT::datatable(display_data,
      options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = '_all'))),
      rownames = FALSE
    ) %>%
      formatStyle("Completion Rate (%)",
        backgroundColor = styleInterval(
          c(10, 20, 30, 40, 50, 60, 70, 80, 90),
          c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf",
            "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850", "#006837")))
  }, server = FALSE)

  # Control Checkback Details Table (same format as efficacy table)
  output$control_details <- DT::renderDataTable({
    data <- efficacy_data_raw()
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No control checkbacks found"))
    }

    control_data <- data[data$is_control == TRUE, ]
    if (is.null(control_data) || nrow(control_data) == 0) {
      return(data.frame(Message = "No control checkbacks found - all checkbacks had treatments between pre and post inspections"))
    }

    # Filter to rows with valid data, deduplicate to one genus per display
    display_data <- control_data %>%
      filter(!is.na(pct_reduction)) %>%
      mutate(
        `% Reduction` = round(pct_reduction, 1),
        `Pre Genus Dips` = round(pre_genus_dips, 2),
        `Post Genus Dips` = round(post_genus_dips, 2),
        `Pre Genus %` = round(pre_genus_pct, 0),
        `Post Genus %` = round(post_genus_pct, 0),
        Acres = round(acres, 1)
      ) %>%
      select(
        Site = sitecode, Facility = facility,
        Year = year, Season = season, Genus = genus,
        `Trt Type` = trt_type, `Mat Code` = trt_matcode,
        `Trt Date` = trt_date, `Pre Date` = pre_date, `Post Date` = post_date,
        `Days from Trt` = days_from_trt,
        `Pre Dips` = pre_numdip, `Post Dips` = post_numdip,
        `Pre Genus %`, `Post Genus %`,
        `Pre Genus Dips`, `Post Genus Dips`,
        `% Reduction`, Acres
      ) %>%
      arrange(desc(`Trt Date`), Site, Genus)

    display_data$Site <- make_sitecode_link(display_data$Site)

    datatable(display_data,
      escape = FALSE,
      options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = '_all'))),
      rownames = FALSE, filter = 'top'
    ) %>%
      formatStyle(columns = names(display_data),
                  backgroundColor = "#fff3cd")
  }, server = FALSE)

  # Invalid Checkback Details Table (from site_details / progress tab)
  output$invalid_checkback_details_progress <- DT::renderDataTable({
    data <- site_details()
    if (is.null(data) || nrow(data) == 0 || !"is_invalid" %in% names(data)) {
      return(data.frame(Message = "No invalid checkbacks found"))
    }

    invalid_data <- data[data$is_invalid == TRUE, ]
    if (nrow(invalid_data) == 0) {
      return(data.frame(Message = "No invalid checkbacks found - all pre-inspections occurred outside active treatment windows"))
    }

    display_data <- invalid_data %>%
      mutate(
        Acres = round(acres, 1)
      ) %>%
      select(
        Site = sitecode, Facility = facility,
        `Matched Material` = mattype,
        `Matched Trt Date` = treatment_date,
        `Trt Timestamp` = trt_timestamp,
        `Pre Date` = pre_inspection_date,
        `Pre Timestamp` = pre_timestamp,
        `Post Date` = checkback_date,
        `Post Timestamp` = post_timestamp,
        `Pre Dips` = pre_treatment_dips, `Post Dips` = post_treatment_dips,
        Acres, Reason = invalid_reason
      ) %>%
      arrange(desc(`Matched Trt Date`), Site)

    display_data$Site <- make_sitecode_link(display_data$Site)

    datatable(display_data,
      escape = FALSE,
      options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = '_all'))),
      rownames = FALSE, filter = 'top'
    ) %>%
      formatStyle(columns = names(display_data),
                  backgroundColor = "#f8d7da")
  }, server = FALSE)

  # ===========================================================================
  # CONTROL EFFICACY TAB
  # ===========================================================================

  # Load efficacy data on refresh — always load all materials for filtering flexibility
  efficacy_data_raw <- eventReactive(input$refresh_data_progress, {
    req(input$year_range)

    withProgress(message = "Loading efficacy data (SQL + species)...", value = 0.3, {
      load_efficacy_data(
        start_year = input$year_range[1],
        end_year   = input$year_range[2],
        bti_only   = FALSE,  # Always load all materials for immediate filtering
        use_mullas = isTRUE(input$use_mullas)
      )
    })
  })

  # Filtered efficacy data based on sidebar controls
  efficacy_data_filtered <- reactive({
    data <- efficacy_data_raw()
    if (is.null(data) || nrow(data) == 0) return(NULL)

    filtered <- data

    # Exclude control and invalid checkbacks from the display
    # (controls are used internally for Mulla's correction;
    #  invalids have pre-inspection during active treatment)
    if ("is_control" %in% names(filtered)) {
      filtered <- filtered[filtered$is_control == FALSE, ]
    }
    if ("is_invalid" %in% names(filtered)) {
      filtered <- filtered[filtered$is_invalid == FALSE, ]
    }

    # Season filter
    if (!is.null(input$season_filter) && length(input$season_filter) > 0) {
      filtered <- filtered %>% filter(season %in% input$season_filter)
    } else {
      return(NULL)
    }

    # Genus filter (selectInput: "Both", "Aedes", or "Culex")
    if (!is.null(input$genus_filter) && input$genus_filter != "Both") {
      filtered <- filtered %>% filter(genus == input$genus_filter)
    }

    # Treatment type filter
    if (!is.null(input$trt_type_filter) && length(input$trt_type_filter) > 0) {
      filtered <- filtered %>% filter(trt_type %in% input$trt_type_filter)
    } else {
      return(NULL)
    }

    # Material type filter
    mat_filter <- input$material_type_filter
    if (!is.null(mat_filter) && mat_filter != "all") {
      con <- get_db_connection()
      if (!is.null(con)) {
        mat_codes <- get_matcodes_by_ingredient(con, mat_filter)
        safe_disconnect(con)
        if (length(mat_codes) > 0) {
          filtered <- filtered %>% filter(trt_matcode %in% mat_codes)
        }
      }
    }

    # Facility filter (shared)
    if (!is.null(input$facility_filter) && input$facility_filter != "all") {
      filtered <- filtered %>% filter(facility == input$facility_filter)
    }

    # Dosage filter
    if (!is.null(input$dosage_filter) && input$dosage_filter != "all" && "dosage_label" %in% names(filtered)) {
      filtered <- filtered %>% filter(dosage_label == input$dosage_filter)
    }

    if (nrow(filtered) == 0) return(NULL)
    return(filtered)
  })

  # Box plot output
  output$reduction_boxplot <- renderPlotly({
    withProgress(message = "Rendering box plots...", value = 0.5, {
      comp_mode <- if (!is.null(input$comparison_mode)) input$comparison_mode else "genus"
      create_reduction_boxplot(
        efficacy_data = efficacy_data_filtered(),
        theme = input$color_theme,
        comparison_mode = comp_mode
      )
    })
  })

  # Stat boxes
  output$efficacy_valid_count <- renderUI({
    data <- efficacy_data_filtered()
    valid <- if (!is.null(data)) sum(!is.na(data$pct_reduction)) else 0
    status_colors <- get_status_colors(theme = input$color_theme)
    create_stat_box(value = valid, title = "Valid Observations",
                    bg_color = status_colors["unknown"], icon = icon("chart-bar"))
  })

  output$efficacy_median_reduction <- renderUI({
    data <- efficacy_data_filtered()
    med <- 0
    if (!is.null(data) && any(!is.na(data$pct_reduction))) {
      plot_values <- data$pct_reduction[!is.na(data$pct_reduction)]
      plot_values <- pmax(plot_values, 0)
      med <- round(median(plot_values, na.rm = TRUE), 1)
    }
    status_colors <- get_status_colors(theme = input$color_theme)
    color <- if (med >= 80) status_colors["active"] else if (med >= 50) status_colors["needs_action"] else status_colors["needs_treatment"]
    create_stat_box(value = paste0(med, "%"), title = "Median % Reduction",
                    bg_color = color, icon = icon("chart-line"))
  })

  output$efficacy_mean_reduction <- renderUI({
    data <- efficacy_data_filtered()
    avg <- 0
    if (!is.null(data) && any(!is.na(data$pct_reduction))) {
      plot_values <- data$pct_reduction[!is.na(data$pct_reduction)]
      plot_values <- pmax(plot_values, 0)
      avg <- round(mean(plot_values, na.rm = TRUE), 1)
    }
    status_colors <- get_status_colors(theme = input$color_theme)
    color <- if (avg >= 80) status_colors["active"] else if (avg >= 50) status_colors["needs_action"] else status_colors["needs_treatment"]
    create_stat_box(value = paste0(avg, "%"), title = "Mean % Reduction",
                    bg_color = color, icon = icon("calculator"))
  })

  output$efficacy_pct_above_80 <- renderUI({
    data <- efficacy_data_filtered()
    if (!is.null(data) && any(!is.na(data$pct_reduction))) {
      plot_values <- data$pct_reduction[!is.na(data$pct_reduction)]
      plot_values <- pmax(plot_values, 0)
      pct_above <- round(sum(plot_values >= 80) / length(plot_values) * 100, 1)
    } else {
      pct_above <- 0
    }
    status_colors <- get_status_colors(theme = input$color_theme)
    color <- if (pct_above >= 70) status_colors["active"] else if (pct_above >= 40) status_colors["needs_action"] else status_colors["needs_treatment"]
    create_stat_box(value = paste0(pct_above, "%"), title = "% Above 80% Reduction",
                    bg_color = color, icon = icon("check-double"))
  })

  # Efficacy data table
  output$efficacy_table <- DT::renderDataTable({
    data <- efficacy_data_filtered()

    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No efficacy data available. Click Refresh Data."))
    }

    display_data <- data %>%
      filter(!is.na(pct_reduction)) %>%
      mutate(
        `% Reduction` = round(pct_reduction, 1),
        `Pre Genus Dips` = round(pre_genus_dips, 2),
        `Post Genus Dips` = round(post_genus_dips, 2),
        `Pre Genus %` = round(pre_genus_pct, 0),
        `Post Genus %` = round(post_genus_pct, 0),
        Acres = round(acres, 1)
      ) %>%
      select(
        Site = sitecode, Facility = facility,
        Year = year, Season = season, Genus = genus,
        `Trt Type` = trt_type, `Mat Code` = trt_matcode,
        `Trt Date` = trt_date, `Pre Date` = pre_date, `Post Date` = post_date,
        `Days from Trt` = days_from_trt,
        `Pre Dips` = pre_numdip, `Post Dips` = post_numdip,
        `Pre Genus %`, `Post Genus %`,
        `Pre Genus Dips`, `Post Genus Dips`,
        `% Reduction`, Acres
      ) %>%
      arrange(desc(`Trt Date`), Site, Genus)

    # Link sitecodes to data.mmcd.org map
    display_data$Site <- make_sitecode_link(display_data$Site)

    datatable(display_data,
      escape = FALSE,
      options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = '_all'))),
      rownames = FALSE, filter = 'top'
    ) %>%
      formatStyle("% Reduction",
        backgroundColor = styleInterval(
          c(-50, 0, 25, 50, 75, 90, 95),
          c("#67001f", "#d73027", "#f46d43", "#fee08b", "#d9ef8b",
            "#a6d96a", "#66bd63", "#1a9850")))
  }, server = FALSE)

  # Invalid Checkback Details Table (from efficacy data - pre during active treatment)
  output$invalid_checkback_details <- DT::renderDataTable({
    data <- efficacy_data_raw()
    if (is.null(data) || nrow(data) == 0 || !"is_invalid" %in% names(data)) {
      return(data.frame(Message = "No invalid checkbacks found"))
    }

    invalid_data <- data[data$is_invalid == TRUE, ]
    if (is.null(invalid_data) || nrow(invalid_data) == 0) {
      return(data.frame(Message = "No invalid checkbacks found - all pre-inspections occurred outside active treatment windows"))
    }

    # Deduplicate across genera — show one row per checkback
    display_data <- invalid_data %>%
      filter(!duplicated(paste(sitecode, post_date, trt_date))) %>%
      mutate(
        Acres = round(acres, 1)
      ) %>%
      select(
        Site = sitecode, Facility = facility,
        Year = year, Season = season,
        `Trt Type` = trt_type, Material = trt_mattype,
        `Trt Date` = trt_date, `Pre Date` = pre_date, `Post Date` = post_date,
        `Prior Trt Date` = prior_trt_date, `Prior Material` = prior_trt_material,
        `Effect Days` = prior_trt_effect_days, `Trt Expiry` = prior_trt_expiry,
        `Days Remaining` = prior_trt_days_remaining,
        `Pre Dips` = pre_numdip, `Post Dips` = post_numdip,
        Acres, Reason = invalid_reason
      ) %>%
      arrange(desc(`Trt Date`), Site)

    display_data$Site <- make_sitecode_link(display_data$Site)

    datatable(display_data,
      escape = FALSE,
      options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = '_all'))),
      rownames = FALSE, filter = 'top'
    ) %>%
      formatStyle(columns = names(display_data),
                  backgroundColor = "#f8d7da")
  }, server = FALSE)
}

# Run the app
shinyApp(ui = ui, server = server)
