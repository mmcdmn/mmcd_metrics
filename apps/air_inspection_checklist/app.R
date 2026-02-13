# Air Inspection Checklist App
# Checklist view of RED air sites showing inspection status by FOS area and section

# Load shared libraries and utilities
source("../../shared/app_libraries.R")
source("../../shared/server_utilities.R")
source("../../shared/db_helpers.R")
source("../../shared/stat_box_helpers.R")

# Source app-specific function files
source("ui_helper.R")
source("data_functions.R")
source("display_functions.R")

# Set application name for AWS RDS monitoring
set_app_name("air_inspection_checklist")

# Load environment variables
load_env_vars()

# =============================================================================
# STARTUP OPTIMIZATION: Preload lookup tables into cache
# =============================================================================
message("[air_inspection_checklist] Preloading lookup tables...")
tryCatch({
  get_facility_lookup()
  get_foremen_lookup()
  message("[air_inspection_checklist] Lookup tables preloaded")
}, error = function(e) message("[air_inspection_checklist] Preload warning: ", e$message))

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- air_inspection_checklist_ui()

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {

  # ===========================================================================
  # URL PARAMETER PARSING
  # ===========================================================================
  url_params <- reactiveValues(
    facility = NULL,
    fos = NULL,
    lookback = NULL
  )

  observe({
    query <- parseQueryString(session$clientData$url_search)

    if (!is.null(query$facility) && query$facility != "" && query$facility != "all") {
      url_params$facility <- query$facility
    }
    if (!is.null(query$fos) && query$fos != "") {
      url_params$fos <- query$fos
    }
    if (!is.null(query$lookback) && query$lookback != "") {
      lb <- as.integer(query$lookback)
      if (!is.na(lb) && lb >= 1 && lb <= 7) {
        url_params$lookback <- lb
      }
    }
  })

  # ===========================================================================
  # INITIALIZE FILTER CHOICES
  # ===========================================================================
  observe({
    # Facility choices
    facility_choices <- get_facility_choices()
    selected_facility <- if (!is.null(url_params$facility)) url_params$facility else "all"
    updateSelectInput(session, "facility_filter",
                      choices = facility_choices, selected = selected_facility)

    # FOS choices
    foreman_choices <- get_foreman_choices()
    selected_fos <- if (!is.null(url_params$fos)) url_params$fos else "all"
    updateSelectInput(session, "foreman_filter",
                      choices = foreman_choices, selected = selected_fos)

    # Lookback days from URL
    if (!is.null(url_params$lookback)) {
      updateSliderInput(session, "lookback_days", value = url_params$lookback)
    }
  })

  # ===========================================================================
  # UPDATE FOS CHOICES WHEN FACILITY CHANGES
  # ===========================================================================
  observeEvent(input$facility_filter, {
    foremen_lookup <- get_foremen_lookup()

    if (input$facility_filter == "all") {
      foreman_choices <- get_foreman_choices()
    } else {
      filtered_foremen <- foremen_lookup[foremen_lookup$facility == input$facility_filter, ]
      foreman_choices <- c("All FOS" = "all")
      if (nrow(filtered_foremen) > 0) {
        display_names <- paste0(filtered_foremen$shortname, " (", filtered_foremen$facility, ")")
        foreman_choices <- c(
          foreman_choices,
          setNames(filtered_foremen$shortname, display_names)
        )
      }
    }

    updateSelectInput(session, "foreman_filter",
                      choices = foreman_choices, selected = "all")
  })

  # ===========================================================================
  # REFRESH BUTTON - Capture inputs and load data
  # ===========================================================================
  refresh_inputs <- eventReactive(input$refresh, {
    # Convert facility filter
    fac <- if (is.null(input$facility_filter) || input$facility_filter == "all") {
      NULL
    } else {
      input$facility_filter
    }

    # Convert FOS filter: map shortname to emp_num for SQL
    fos <- NULL
    if (!is.null(input$foreman_filter) && input$foreman_filter != "all") {
      foremen_lookup <- get_foremen_lookup()
      matched <- foremen_lookup[foremen_lookup$shortname == input$foreman_filter, ]
      if (nrow(matched) > 0) {
        fos <- trimws(as.character(matched$emp_num))
      }
    }

    # Parse zone filter
    zone_vals <- if (!is.null(input$zone_filter)) {
      strsplit(input$zone_filter, ",")[[1]]
    } else {
      "1"
    }

    list(
      facility = fac,
      foreman = fos,
      lookback_days = input$lookback_days,
      analysis_date = input$analysis_date,
      show_unfinished = input$show_unfinished_only,
      zone = zone_vals,
      show_active_treatment = isTRUE(input$show_active_treatment)
    )
  }, ignoreNULL = FALSE)

  # ===========================================================================
  # REACTIVE DATA
  # ===========================================================================
  checklist_data <- reactive({
    params <- refresh_inputs()
    if (is.null(params)) return(data.frame())

    withProgress(message = "Loading checklist data...", value = 0.3, {
      data <- get_checklist_data(
        facility_filter = params$facility,
        foreman_filter = params$foreman,
        lookback_days = params$lookback_days,
        analysis_date = params$analysis_date,
        zone_filter = params$zone,
        include_active_treatment = params$show_active_treatment
      )
      incProgress(0.7)
      data
    })
  })

  # ===========================================================================
  # CHECKLIST DISPLAY
  # ===========================================================================
  output$checklist_display <- renderUI({
    data <- checklist_data()
    show_unfinished <- if (!is.null(input$show_unfinished_only)) {
      input$show_unfinished_only
    } else {
      FALSE
    }
    build_checklist_html(data, show_unfinished_only = show_unfinished)
  })
}

# =============================================================================
# RUN APPLICATION
# =============================================================================
shinyApp(ui = ui, server = server)
