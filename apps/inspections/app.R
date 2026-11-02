# Inspections App - Prehatch Coverage (the 51% goal)
# Shows which ground PREHATCH sites count toward the goal (red bug in 5 years OR
# sampled this year) and, for the ones that don't, why. See NOTES.md.

library(shiny)
library(DT)
library(dplyr)
library(lubridate)
library(plotly)

# Source shared database helpers and local functions
source("../../shared/db_helpers.R")
source("../../shared/accessibility_helpers.R")
source("../../shared/server_utilities.R")
source("../../shared/stat_box_helpers.R")
source("../../shared/url_state_helpers.R")  # deep-linking + auto-refresh engine
source("data_functions.R")
source("display_functions.R")
source("ui_helper.R")

# Set application name for AWS RDS monitoring
set_app_name("inspections")

# Preload lookups so the first page render doesn't wait on them
invisible(get_facility_lookup())
invisible(get_foremen_lookup())

# Old links used ?tabs=gaps|analytics|larvae|red_bug_gaps (and, briefly, scorecard);
# send them somewhere sensible
legacy_tab_to_new <- function(value) {
  switch(as.character(value)[1],
    progress = , sites = , rules = as.character(value)[1],
    red_bug_gaps = "sites",
    "progress")
}

# ?fos= accepts a foreman shortname or the 4-digit area code (emp_num)
fos_to_shortname <- function(value) {
  codes <- trimws(unlist(strsplit(as.character(value), ",")))
  lookup <- get_foremen_lookup()
  vapply(codes, function(code) {
    if (grepl("^[0-9]+$", code) && nrow(lookup) > 0) {
      hit <- lookup$shortname[sprintf("%04d", as.numeric(lookup$emp_num)) == sprintf("%04d", as.numeric(code))]
      if (length(hit) > 0) return(hit[1])
    }
    code
  }, character(1), USE.NAMES = FALSE)
}

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE)

# Define UI
ui <- tagList(url_state_js(), create_main_ui())

# Define server logic
server <- function(input, output, session) {

  # Deep-linking + auto-refresh (shared/url_state_helpers.R). Filters apply
  # live, so there is no Refresh button to click after the inputs are set.
  wire_deep_links(input, output, session,
    spec = list(
      tabs         = list(input = "tabs",         type = "tab", transform = legacy_tab_to_new),
      facility     = list(input = "facility",     type = "select"),
      zone         = list(input = "zone",         type = "radio"),
      fos          = list(input = "fosarea",      type = "selectize", transform = fos_to_shortname),
      priority     = list(input = "priority",     type = "selectize"),
      drone_filter = list(input = "drone_filter", type = "radio"),
      status       = list(input = "site_status",  type = "select"),
      reason       = list(input = "site_reason",  type = "select"),
      as_of        = list(input = "as_of",        type = "date"),
      color_theme  = list(input = "color_theme",  type = "select")
    ),
    refresh_for = function(state) character(0))

  # Reactive theme handling
  current_theme <- reactive({
    input$color_theme %||% "MMCD"
  })

  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })

  # Update FOS choices when facility changes
  observeEvent(input$facility, {
    foremen_lookup <- get_foremen_lookup()

    if (input$facility == "all") {
      fosarea_choices <- get_foreman_choices(include_all = TRUE)
    } else {
      filtered_foremen <- foremen_lookup[foremen_lookup$facility == input$facility, ]
      fosarea_choices <- c("All FOS" = "all")
      if (nrow(filtered_foremen) > 0) {
        display_names <- paste0(filtered_foremen$shortname, " (", filtered_foremen$facility, ")")
        fosarea_choices <- c(fosarea_choices, setNames(filtered_foremen$shortname, display_names))
      }
    }

    updateSelectizeInput(session, "fosarea", choices = fosarea_choices, selected = "all")
  })

  # ============= DATA =============
  # One per-site table (all facilities/zones) drives every view; filters below
  # are applied in R. Same function the Overview tile uses.
  as_of_date <- reactive({
    if (is.null(input$as_of) || is.na(input$as_of)) Sys.Date() else as.Date(input$as_of)
  })

  site_data <- eventReactive(list(input$refresh, input$as_of), {
    as_of <- as_of_date()
    withProgress(message = "Loading prehatch coverage...", value = 0.5, {
      raw <- cached_call("inspections_site_coverage", load_prehatch_site_coverage, as_of = as_of)
      sites <- classify_site_coverage(raw, as_of)
      sites <- add_fos_names(sites)
      if (nrow(sites) > 0) sites <- map_facility_names(sites)
      sites
    })
  }, ignoreNULL = FALSE)

  filtered_sites <- reactive({
    df <- site_data()
    if (nrow(df) == 0) return(df)

    if (length(input$facility) > 0 && !"all" %in% input$facility) {
      df <- df[df$facility %in% input$facility, , drop = FALSE]
    }
    if (length(input$fosarea) > 0 && !"all" %in% input$fosarea) {
      df <- df[df$fos_name %in% input$fosarea, , drop = FALSE]
    }
    zones <- strsplit(input$zone %||% "1,2", ",")[[1]]
    df <- df[df$zone %in% zones, , drop = FALSE]
    if (length(input$priority) > 0 && !"all" %in% input$priority) {
      df <- df[df$priority %in% input$priority, , drop = FALSE]
    }
    drone_value <- input$drone_filter %||% "include_drone"
    if (drone_value == "drone_only") {
      df <- df[df$drone %in% "Y", , drop = FALSE]
    } else if (drone_value == "no_drone") {
      df <- df[!(df$drone %in% "Y"), , drop = FALSE]
    }
    df
  })

  overall <- reactive({
    summarise_coverage(filtered_sites())
  })

  # ============= PROGRESS =============
  # Wording for the two sides of the goal lives in coverage_functions.R
  # (COVERAGE_LABEL_COUNTING = "Sampled", COVERAGE_LABEL_NOT_COUNTING = "Missing")
  output$hero <- renderUI({
    s <- overall()
    if (s$total == 0) {
      return(div(class = "cov-note", "No prehatch sites match these filters."))
    }

    goal <- coverage_goal_pct()
    level <- if (s$pct >= goal) "good" else if (s$pct >= coverage_warning_pct()) "warning" else "alert"
    bg <- resolve_box_color(level, theme = current_theme())
    fg <- if (exists("contrast_text_color", mode = "function", inherits = TRUE)) contrast_text_color(bg) else "#ffffff"

    need_text <- if (s$goal_met) {
      sprintf("Goal met - %s sites to spare. That many could drop to %s before the total falls below %s%%.",
              fmt_n(s$cushion), COVERAGE_LABEL_NOT_COUNTING, goal)
    } else {
      sprintf("%s more sites need to be %s to reach %s%%.",
              fmt_n(s$needed), COVERAGE_LABEL_COUNTING, goal)
    }

    div(class = "cov-hero", style = sprintf("background: %s; color: %s;", bg, fg),
      div(class = "cov-pct", sprintf("%.1f%%", coverage_pct_label(s$pct))),
      div(class = "cov-sub",
          sprintf("%s of %s prehatch sites are %s (as of %s)",
                  fmt_n(s$counting), fmt_n(s$total), COVERAGE_LABEL_COUNTING,
                  format(as_of_date(), "%Y-%m-%d"))),
      div(class = "cov-bar", role = "progressbar",
          `aria-valuenow` = round(s$pct, 1), `aria-valuemin` = 0, `aria-valuemax` = 100,
          `aria-label` = paste("Percent of prehatch sites that are", COVERAGE_LABEL_COUNTING),
          div(class = "cov-fill", style = sprintf("width: %.1f%%; background: %s;", min(100, s$pct), fg)),
          div(class = "cov-goal", style = sprintf("left: %s%%;", goal))),
      div(class = "cov-goal-label", span(style = sprintf("left: %s%%;", goal), sprintf("Goal %s%%", goal))),
      div(class = "cov-need", need_text)
    )
  })

  stat_box <- function(value, title, status_key, icon_name) {
    colors <- get_status_colors(theme = current_theme())
    create_stat_box(
      value = fmt_n(value), title = title,
      bg_color = unname(colors[status_key]), icon = icon(icon_name)
    )
  }

  # The three boxes split every site once: ID pending (a sample taken this year
  # with no result entered yet), Sampled with nothing pending, and Missing.
  # Sampled + ID pending is the hero count: pending sites count toward the goal.
  output$box_sampled <- renderUI({
    df <- filtered_sites()
    stat_box(sum(df$counts & !df$id_pending), paste0("Sites ",COVERAGE_LABEL_COUNTING, ", ID entered"),
             "active", "check-circle")
  })
  # in_lab is the theme's "lab / needs ID" color (also used for Needs ID in air_sites_simple)
  output$box_id_pending <- renderUI({
    stat_box(sum(filtered_sites()$id_pending), COVERAGE_LABEL_ID_PENDING, "in_lab", "microscope")
  })
  output$box_not_counting <- renderUI({
    stat_box(overall()$not_counting, COVERAGE_LABEL_NOT_COUNTING, "needs_treatment", "times-circle")
  })

  output$reason_chart <- renderPlotly({
    p <- create_reason_chart(filtered_sites(), current_theme())
    validate(need(!is.null(p), "Every site in this selection counts toward the goal."))
    p
  })

  # Clicking a reason bar jumps to those sites
  observeEvent(event_data("plotly_click", source = "reason_chart"), {
    clicked <- event_data("plotly_click", source = "reason_chart")
    if (!is.null(clicked$key)) {
      updateSelectInput(session, "site_status", selected = "not_counting")
      updateSelectInput(session, "site_reason", selected = as.character(clicked$key[[1]]))
      updateTabsetPanel(session, "tabs", selected = "sites")
    }
  })

  # Progress by facility / FOS. The output box is sized from the number of bars
  # (a fixed height clipped the chart once every FOS was listed).
  group_summary <- reactive({
    df <- filtered_sites()
    if (nrow(df) == 0) return(NULL)
    summarise_coverage(df, by = input$group_by %||% "facility_display")
  })

  output$group_chart_ui <- renderUI({
    s <- group_summary()
    plotlyOutput("group_chart", height = paste0(group_chart_height(if (is.null(s)) 0L else nrow(s)), "px"))
  })

  output$group_chart <- renderPlotly({
    s <- group_summary()
    validate(need(!is.null(s), "No prehatch sites match these filters."))
    create_group_chart(s, label_col = input$group_by %||% "facility_display",
                       goal = coverage_goal_pct(), theme = current_theme())
  })

  output$aging_chart <- renderPlotly({
    p <- create_aging_chart(aging_out_by_month(filtered_sites(), as_of_date(), 12), current_theme())
    validate(need(!is.null(p), sprintf("No %s sites are due to drop to %s in the next 12 months.",
                                       COVERAGE_LABEL_COUNTING, COVERAGE_LABEL_NOT_COUNTING)))
    p
  })

  # ============= SITE LIST =============
  site_view <- reactive({
    df <- filtered_sites()
    if (nrow(df) == 0) return(df)

    view <- input$site_status %||% "not_counting"
    df <- switch(view,
      not_counting = df[!df$counts, , drop = FALSE],
      counting     = df[df$counts, , drop = FALSE],
      id_pending   = df[df$id_pending, , drop = FALSE],
      df)

    reason <- input$site_reason %||% "all"
    if (reason != "all") df <- df[as.character(df$status) == reason, , drop = FALSE]

    sort_sites_for_work(df, view)
  })

  output$site_count_text <- renderText({
    paste0(fmt_n(nrow(site_view())), " sites")
  })

  output$sites_table <- DT::renderDataTable({
    render_coverage_site_table(site_view(), current_theme())
  })

  output$download_sites <- downloadHandler(
    filename = function() {
      paste0("prehatch_coverage_sites_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- format_coverage_sites(site_view(), link = FALSE)
      if (nrow(data) > 0) {
        export_csv_safe(data, file)
      } else {
        export_csv_safe(data.frame("No sites match these filters" = character(0)), file)
      }
    }
  )
}

# Create Shiny app
shinyApp(ui = ui, server = server)
