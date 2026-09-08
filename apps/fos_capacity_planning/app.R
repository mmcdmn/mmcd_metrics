# FOS Capacity & Border-to-Border Planning
# =============================================================================
# Strategic offseason planning tool: per-FOS-area inventory/capacity numbers,
# staffing ratios, facility->section driving distance, and a what-if planner
# that promotes P2 sections into P1 and estimates added staff.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(tidyr)
  library(DT)
  library(leaflet)
  library(sf)
  library(htmlwidgets)
})

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

source("../../shared/db_helpers.R")
source("data_functions.R")
source("display_functions.R")

set_app_name("fos_capacity_planning")

message("[fos_capacity] Preloading lookup tables...")
tryCatch({
  get_facility_lookup(); get_foremen_lookup()
}, error = function(e) message("[fos_capacity] Preload warning: ", e$message))

METRIC_CHOICES <- c(
  "Ground sites"   = "ground_sites",
  "Air sites"      = "air_sites",
  "Prehatch sites" = "prehatch_sites",
  "Prehatch acres" = "prehatch_acres",
  "Sections"       = "sections",
  "Wet catch basins" = "wet_cb"
)

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', Arial, sans-serif; }
    .well { background:#f5f7fa; }
    h2 { color:#2c5aa0; }
    .metric-note { color:#555; font-size:13px; }
  "))),
  titlePanel("FOS Capacity & Border-to-Border Planning"),
  tags$p(class = "metric-note",
    "Per-FOS-area inventory for 5-year planning. Zones: P1 and P2. ",
    "All counts are the active universe (breeding sites, wet catch basins, ",
    "sections, and assigned Inspectors)."),

  tabsetPanel(
    id = "tabs",

    # ---- Tab 1: Capacity by FOS ------------------------------------------
    tabPanel(
      "Capacity by FOS",
      br(),
      wellPanel(
        fluidRow(
          column(3, selectInput("cap_facility", "Facility:",
                                 choices = c("All facilities" = "all"))),
          column(3, checkboxInput("cap_priority",
                                  "Break down by priority", FALSE)),
          column(3, checkboxInput("cap_drivetime",
                                  "Show est. drive time", FALSE)),
          column(3, downloadButton("cap_download", "Download CSV"))
        ),
        conditionalPanel(
          "input.cap_drivetime",
          fluidRow(
            column(3, numericInput("avg_mph", "Avg mph", 45, 5, 80, 1)),
            column(3, numericInput("circuity", "Road circuity factor",
                                   1.3, 1, 2, 0.05))
          )
        )
      ),
      h4("Capacity per FOS area (P1 / P2)"),
      div(style = "overflow-x:auto;", DT::DTOutput("cap_table")),
      br(),
      h4("Averages per FOS area"),
      tags$p(class = "metric-note",
             "Mean load per FOS area, by facility and district-wide."),
      tabsetPanel(
        tabPanel("P1", br(), DT::DTOutput("avg_p1")),
        tabPanel("P2", br(), DT::DTOutput("avg_p2"))
      )
    ),

    # ---- Tab 2: Staffing --------------------------------------------------
    tabPanel(
      "Staffing",
      br(),
      h4("FOS per facility & techs per FOS"),
      tags$p(class = "metric-note",
             "Inspectors reporting to each FieldSuper (FOS), and FOS per facility. ",
             "Load-per-tech uses P1 load."),
      div(style = "overflow-x:auto;", DT::DTOutput("staff_table"))
    ),

    # ---- Tab 3: What-If Planner ------------------------------------------
    tabPanel(
      "What-If Planner",
      br(),
      wellPanel(
        fluidRow(
          column(4, selectInput("wi_facility", "Facility:", choices = NULL)),
          column(4, selectInput("wi_metric", "Staffing driver (Estimate A):",
                                 choices = METRIC_CHOICES,
                                 selected = "ground_sites")),
          column(4, numericInput("wi_per_tech",
                                  "Load per tech (blank = district avg)",
                                  value = NA, min = 1))
        ),
        # Equations — how each estimate is computed (shown up front)
        tags$div(style = paste0("background:#eef3fb;border:1px solid #d0daec;",
                                "border-radius:6px;padding:10px 14px;margin-top:6px;"),
          tags$div(style = "font-weight:600;color:#2c5aa0;margin-bottom:4px;",
                   "How the estimates are computed"),
          tags$p(style = "margin:4px 0;font-family:Consolas,monospace;font-size:13px;",
            HTML("<b>A. Added techs</b> = &Sigma;<sub>FOS</sub> ( &lceil;new P1 load &divide; load-per-tech&rceil; &minus; &lceil;current P1 load &divide; load-per-tech&rceil; )")),
          tags$p(style = "margin:4px 0;font-family:Consolas,monospace;font-size:13px;",
            HTML("<b>B. Crews</b> = &Sigma;<sub>job type</sub> ( sites &times; jobs-per-site &times; hours-per-job ) &divide; hours-per-crew-season")),
          tags$p(class = "metric-note", style = "margin:8px 0 0;",
            "Click a section on the map to add/remove it; hold ",
            tags$b("Shift"), " and drag a box to add many. Every number in the ",
            "equations is shown in the tables below and updates live.")
        )
      ),
      fluidRow(
        column(7,
          div(style = "display:flex;align-items:center;gap:12px;margin-bottom:6px;flex-wrap:wrap;",
            h4(style = "margin:0;", "Select P2 sections to promote"),
            actionButton("wi_selectall", "Select all shown", class = "btn-default btn-sm"),
            actionButton("wi_clear", "Clear", class = "btn-default btn-sm"),
            uiOutput("wi_selcount", inline = TRUE)
          ),
          leaflet::leafletOutput("wi_map", height = 440),
          br(),
          tags$details(
            tags$summary(style = "cursor:pointer;color:#2c5aa0;",
                         "Show section list / details"),
            div(style = "overflow-x:auto;margin-top:8px;", DT::DTOutput("wi_sections"))
          )
        ),
        column(5,
          h4("Estimate A — load per tech"),
          uiOutput("wi_summary"),
          br(),
          div(style = "overflow-x:auto;", DT::DTOutput("wi_by_fos"))
        )
      ),
      hr(),
      h4("Estimate B — crews needed (2001 Workload Study method)"),
      tags$p(class = "metric-note",
        "site counts × jobs-per-site (live, from actual treatments) × ",
        "hours-per-job (editable, 2001 study defaults) ÷ hours-per-crew-season ",
        "= crews required. A crew = 1 FOS + its inspectors."),
      wellPanel(
        fluidRow(
          column(2, selectInput("wi_rate_year", "Jobs/site season:", choices = NULL)),
          column(3, radioButtons("wi_rate_scope", "Jobs/site rates:",
                                 c("District" = "DISTRICT", "This facility" = "FAC"),
                                 selected = "DISTRICT", inline = TRUE)),
          column(2, numericInput("wi_crew_hours", "Hrs / crew-season", 2184, 100, step = 1))
        ),
        fluidRow(
          column(2, numericInput("wi_h_air_trt", "Hrs air trt", 1.35, 0, step = 0.05)),
          column(2, numericInput("wi_h_air_insp", "Hrs air insp", 0.66, 0, step = 0.05)),
          column(2, numericInput("wi_h_gnd_insp_trt", "Hrs gnd insp&trt", 1.08, 0, step = 0.05)),
          column(2, numericInput("wi_h_gnd_trt", "Hrs gnd trt", 0.55, 0, step = 0.05)),
          column(2, numericInput("wi_h_gnd_insp", "Hrs gnd insp", 0.17, 0, step = 0.05))
        )
      ),
      fluidRow(
        column(7,
          uiOutput("wi_crew_summary"),
          div(style = "overflow-x:auto;", DT::DTOutput("wi_crew_by_job"))
        ),
        column(5,
          h5("Live jobs-per-site rates used (P1)"),
          div(style = "overflow-x:auto;", DT::DTOutput("wi_rates"))
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # ---- Load + aggregate once per session --------------------------------
  base <- reactiveVal(NULL)
  observe({
    if (!is.null(base())) return()
    sections <- load_capacity_sections()
    staffing <- load_capacity_staffing()
    cap      <- aggregate_capacity_by_fos(sections, staffing)
    priority <- load_capacity_by_priority()
    # FOS name onto sections for the what-if picker
    foremen <- tryCatch(get_foremen_lookup(), error = function(e) data.frame())
    fos_name <- if (nrow(foremen) > 0) {
      setNames(foremen$shortname, as.character(foremen$emp_num))
    } else character(0)
    sections$fos_name <- ifelse(as.character(sections$fosarea) %in% names(fos_name),
                                fos_name[as.character(sections$fosarea)],
                                paste0("FOS ", sections$fosarea))
    base(list(sections = sections, staffing = staffing, cap = cap,
              priority = priority))
    # Populate facility choices
    facs <- sort(unique(cap$facility))
    fac_lkp <- tryCatch(get_facility_lookup(), error = function(e) data.frame())
    fac_name <- if (nrow(fac_lkp) > 0) setNames(fac_lkp$short_name, fac_lkp$full_name) else NULL
    fac_choices <- setNames(facs, facs)
    if (!is.null(fac_name)) {
      # label with full name where available
      labels <- sapply(facs, function(a) {
        fn <- names(fac_name)[fac_name == a]
        if (length(fn) > 0) paste0(fn[1], " (", a, ")") else a
      })
      fac_choices <- setNames(facs, labels)
    }
    updateSelectInput(session, "cap_facility",
                      choices = c("All facilities" = "all", fac_choices))
    updateSelectInput(session, "wi_facility", choices = fac_choices,
                      selected = facs[1])
    # Jobs/site season selector: last complete year default, choices last 6 yrs
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    yrs <- as.character(seq(cur_yr, cur_yr - 5))
    updateSelectInput(session, "wi_rate_year", choices = yrs,
                      selected = as.character(cur_yr - 1))
  })

  cap_filtered <- reactive({
    b <- base(); req(b)
    cap <- b$cap
    if (!is.null(input$cap_facility) && input$cap_facility != "all") {
      cap <- cap[cap$facility == input$cap_facility, ]
    }
    cap
  })

  # ---- Capacity table ---------------------------------------------------
  output$cap_table <- DT::renderDT({
    cap <- cap_filtered(); req(nrow(cap) > 0)

    if (isTRUE(input$cap_priority)) {
      b <- base()
      pri <- b$priority
      if (input$cap_facility != "all") pri <- pri[pri$facility == input$cap_facility, ]
      foremen <- tryCatch(get_foremen_lookup(), error = function(e) data.frame())
      nm <- if (nrow(foremen) > 0) setNames(foremen$shortname, as.character(foremen$emp_num)) else character(0)
      pri$FOS <- ifelse(as.character(pri$fosarea) %in% names(nm),
                        nm[as.character(pri$fosarea)], paste0("FOS ", pri$fosarea))
      pri$Zone <- paste0("P", pri$zone)
      disp <- pri[, c("facility", "FOS", "Zone", "priority",
                      "ground_sites", "air_sites", "prehatch_sites", "prehatch_acres")]
      names(disp) <- c("Facility", "FOS", "Zone", "Priority",
                       "Ground", "Air", "PH sites", "PH acres")
      return(DT::datatable(disp, rownames = FALSE, filter = "top",
                           options = list(pageLength = 25, scrollX = TRUE)))
    }

    w <- capacity_wide(cap)
    if (isTRUE(input$cap_drivetime)) {
      w$drive_min_P1 <- est_drive_minutes(w$avg_miles_P1, input$avg_mph, input$circuity)
      w$drive_min_P2 <- est_drive_minutes(w$avg_miles_P2, input$avg_mph, input$circuity)
    }
    disp <- w
    disp$facility <- NULL; disp$fosarea <- NULL
    # nicer names
    names(disp) <- gsub("_P1", " P1", names(disp))
    names(disp) <- gsub("_P2", " P2", names(disp))
    names(disp) <- gsub("facility_name", "Facility", names(disp))
    names(disp) <- gsub("fos_name", "FOS", names(disp))
    names(disp) <- gsub("n_techs", "Techs", names(disp))
    names(disp) <- gsub("sections", "Sect", names(disp))
    names(disp) <- gsub("ground_sites", "Ground", names(disp))
    names(disp) <- gsub("air_sites", "Air", names(disp))
    names(disp) <- gsub("prehatch_sites", "PHsite", names(disp))
    names(disp) <- gsub("prehatch_acres", "PHac", names(disp))
    names(disp) <- gsub("wet_cb", "WetCB", names(disp))
    names(disp) <- gsub("avg_miles", "AvgMi", names(disp))
    names(disp) <- gsub("drive_min", "DriveMin", names(disp))
    DT::datatable(disp, rownames = FALSE, filter = "top",
                  options = list(pageLength = 40, scrollX = TRUE))
  })

  output$cap_download <- downloadHandler(
    filename = function() paste0("fos_capacity_", Sys.Date(), ".csv"),
    content = function(file) {
      cap <- cap_filtered()
      out <- if (isTRUE(input$cap_priority)) base()$priority else capacity_wide(cap)
      utils::write.csv(out, file, row.names = FALSE)
    }
  )

  output$avg_p1 <- DT::renderDT({
    cap <- cap_filtered(); req(nrow(cap) > 0)
    DT::datatable(capacity_averages(cap, "1"), rownames = FALSE,
                  options = list(pageLength = 15, scrollX = TRUE))
  })
  output$avg_p2 <- DT::renderDT({
    cap <- cap_filtered(); req(nrow(cap) > 0)
    DT::datatable(capacity_averages(cap, "2"), rownames = FALSE,
                  options = list(pageLength = 15, scrollX = TRUE))
  })

  # ---- Staffing table ---------------------------------------------------
  output$staff_table <- DT::renderDT({
    b <- base(); req(b)
    cap <- b$cap
    p1 <- cap[cap$zone == "1", ]
    p1$ground_per_tech <- ifelse(p1$n_techs > 0, round(p1$ground_sites / p1$n_techs, 1), NA)
    p1$sections_per_tech <- ifelse(p1$n_techs > 0, round(p1$sections / p1$n_techs, 1), NA)
    p1$wetcb_per_tech <- ifelse(p1$n_techs > 0, round(p1$wet_cb / p1$n_techs, 1), NA)
    fpf <- b$staffing$fos_per_fac
    fpf_map <- if (nrow(fpf) > 0) setNames(fpf$n_fos, fpf$facility) else integer(0)
    p1$fos_in_facility <- ifelse(p1$facility %in% names(fpf_map),
                                 as.integer(fpf_map[p1$facility]), NA)
    disp <- p1[, c("facility_name", "fos_name", "n_techs", "fos_in_facility",
                   "ground_sites", "ground_per_tech", "sections", "sections_per_tech",
                   "wet_cb", "wetcb_per_tech")]
    names(disp) <- c("Facility", "FOS", "Techs", "FOS in facility",
                     "P1 Ground", "Ground/tech", "P1 Sect", "Sect/tech",
                     "P1 WetCB", "WetCB/tech")
    DT::datatable(disp, rownames = FALSE, filter = "top",
                  options = list(pageLength = 40, scrollX = TRUE))
  })

  # ---- What-If: shared selection state (sectcodes to promote) -----------
  sel <- reactiveVal(character(0))
  observeEvent(input$wi_facility, sel(character(0)))       # reset on facility change
  observeEvent(input$wi_clear, sel(character(0)))
  observeEvent(input$wi_selectall, {
    g <- wi_geo(); if (!is.null(g) && nrow(g) > 0) sel(unique(as.character(g$sectcode)))
  })

  wi_p2 <- reactive({
    b <- base(); req(b, input$wi_facility)
    s <- b$sections
    s <- s[s$facility == input$wi_facility & s$zone == "2", ]
    s[order(s$fos_name, -s$miles), ]
  })

  # P2 section polygons for the facility, with load attributes merged on
  wi_geo <- reactive({
    b <- base(); req(b, input$wi_facility)
    geo <- load_section_geo(input$wi_facility)
    geo <- geo[geo$zone == "2" & !is.na(geo$wkt), , drop = FALSE]
    if (nrow(geo) == 0) return(NULL)
    g <- tryCatch(sf::st_as_sf(geo, wkt = "wkt", crs = 4326),
                  error = function(e) NULL)
    if (is.null(g)) return(NULL)
    attrs <- b$sections[, c("sectcode", "fos_name", "ground_sites",
                            "air_sites", "wet_cb", "miles")]
    merge(g, attrs, by = "sectcode", all.x = TRUE)
  })

  output$wi_selcount <- renderUI({
    tags$span(style = "color:#2c5aa0;font-weight:600;",
              sprintf("Selected: %d section(s)", length(sel())))
  })

  # ---- Map (click to toggle; Shift+drag box to add) ---------------------
  .box_js <- "
  function(el, x) {
    var map = this;
    if (map.boxZoom) { map.boxZoom.disable(); }
    var start = null, rect = null, shift = false;
    document.addEventListener('keydown', function(e){ if(e.key==='Shift'){shift=true;} });
    document.addEventListener('keyup', function(e){ if(e.key==='Shift'){shift=false;} });
    map.on('mousedown', function(e){
      if(!shift){return;} map.dragging.disable(); start = e.latlng;
      if(rect){ map.removeLayer(rect); rect=null; }
    });
    map.on('mousemove', function(e){
      if(!shift || !start){return;}
      var b = L.latLngBounds(start, e.latlng);
      if(rect){ rect.setBounds(b); }
      else { rect = L.rectangle(b, {color:'#2c5aa0', weight:1, dashArray:'4', fillOpacity:0.1}).addTo(map); }
    });
    map.on('mouseup', function(e){
      if(!start){return;}
      var b = L.latLngBounds(start, e.latlng);
      map.dragging.enable();
      if(rect){ map.removeLayer(rect); rect=null; }
      var sw=b.getSouthWest(), ne=b.getNorthEast();
      Shiny.setInputValue('wi_box', {s:sw.lat, w:sw.lng, n:ne.lat, e:ne.lng, nonce:Math.random()}, {priority:'event'});
      start = null;
    });
  }"

  output$wi_map <- leaflet::renderLeaflet({
    g <- wi_geo()
    if (is.null(g) || nrow(g) == 0) {
      return(leaflet::leaflet() %>% leaflet::addTiles())
    }
    lbl <- sprintf(
      "<b>%s</b><br/>FOS: %s<br/>Ground %d &middot; Air %d &middot; WetCB %d<br/>%.1f mi from facility",
      g$sectcode, g$fos_name, g$ground_sites, g$air_sites, g$wet_cb, g$miles)
    bb <- as.numeric(sf::st_bbox(g))
    leaflet::leaflet(g) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addPolygons(
        layerId = ~sectcode, group = "base",
        weight = 1, color = "#7c8a97", fillColor = "#c9d3df", fillOpacity = 0.5,
        label = lapply(lbl, htmltools::HTML),
        highlightOptions = leaflet::highlightOptions(weight = 2, fillOpacity = 0.75)) %>%
      leaflet::fitBounds(bb[1], bb[2], bb[3], bb[4]) %>%
      htmlwidgets::onRender(.box_js)
  })

  # Highlight selected sections (redraw a "sel" overlay group on change)
  observe({
    g <- wi_geo()
    proxy <- leaflet::leafletProxy("wi_map")
    proxy %>% leaflet::clearGroup("sel")
    s <- sel()
    if (!is.null(g) && length(s) > 0) {
      gg <- g[as.character(g$sectcode) %in% s, ]
      if (nrow(gg) > 0) {
        proxy %>% leaflet::addPolygons(
          data = gg, layerId = ~paste0("sel_", sectcode), group = "sel",
          weight = 2, color = "#1e3d6f", fillColor = "#2c5aa0", fillOpacity = 0.6)
      }
    }
  })

  observeEvent(input$wi_map_shape_click, {
    id <- input$wi_map_shape_click$id
    if (is.null(id)) return()
    id <- sub("^sel_", "", id)                 # overlay click deselects
    cur <- sel()
    if (id %in% cur) sel(setdiff(cur, id)) else sel(c(cur, id))
  })

  observeEvent(input$wi_box, {
    bx <- input$wi_box; g <- wi_geo(); req(!is.null(g), nrow(g) > 0)
    inb <- as.character(g$sectcode[
      g$lat >= bx$s & g$lat <= bx$n & g$lon >= bx$w & g$lon <= bx$e])
    inb <- inb[!is.na(inb)]
    if (length(inb) > 0) sel(union(sel(), inb))
  })

  # Section list (informational; reflects current selection)
  output$wi_sections <- DT::renderDT({
    s <- wi_p2(); req(nrow(s) > 0)
    s$Sel <- ifelse(as.character(s$sectcode) %in% sel(), "✓", "")
    disp <- s[, c("Sel", "sectcode", "fos_name", "ground_sites", "air_sites",
                  "prehatch_sites", "wet_cb", "miles")]
    disp$drive_min <- est_drive_minutes(disp$miles,
                                        input$avg_mph %||% 45, input$circuity %||% 1.3)
    names(disp) <- c("Sel", "Section", "FOS", "Ground", "Air", "PH sites",
                     "WetCB", "Miles", "Drive min")
    DT::datatable(disp, rownames = FALSE, filter = "top", selection = "none",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  # ---- Estimate A: load per tech (live) ---------------------------------
  wi_result <- reactive({
    b <- base(); req(b)
    per_tech <- if (is.null(input$wi_per_tech) || is.na(input$wi_per_tech)) NULL else input$wi_per_tech
    whatif_compute(b$sections, sel(), b$cap,
                   metric = input$wi_metric, per_tech = per_tech)
  })

  output$wi_summary <- renderUI({
    r <- wi_result(); t <- r$totals
    metric_label <- names(METRIC_CHOICES)[METRIC_CHOICES == t$metric]
    cur_tot <- sum(r$by_fos$cur_p1, na.rm = TRUE)
    new_tot <- sum(r$by_fos$new_p1, na.rm = TRUE)
    tags$div(
      tags$p(sprintf("Promoting %d P2 section(s) into P1.", t$promoted_n)),
      tags$p(HTML(sprintf(
        "Driver: <b>%s</b>. Baseline <b>%.1f</b> per tech. P1 %s: <b>%s</b> &rarr; <b>%s</b> (+%s).",
        metric_label, t$per_tech, metric_label,
        format(cur_tot, big.mark = ","), format(new_tot, big.mark = ","),
        format(t$added_load, big.mark = ",")))),
      tags$h3(style = "color:#2c5aa0;",
              sprintf("Additional techs needed: %d", t$added_techs))
    )
  })

  output$wi_by_fos <- DT::renderDT({
    r <- wi_result(); by <- r$by_fos
    by <- by[by$added_load != 0 | (!is.na(by$added_techs) & by$added_techs != 0), ]
    if (nrow(by) == 0) return(DT::datatable(
      data.frame(Note = "Select sections on the map to see per-FOS impact"),
      rownames = FALSE, options = list(dom = "t")))
    names(by) <- c("Facility", "FOS", "emp", "Techs", "Cur P1", "New P1",
                   "Added", "Need (new)", "Add techs", "Gap now")
    by$emp <- NULL
    DT::datatable(by, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  # ---- Estimate B: crews (2001 study method, live) ----------------------
  jps_all <- reactive({
    yr <- suppressWarnings(as.integer(input$wi_rate_year)); req(!is.na(yr))
    load_jobs_per_site(yr)
  })

  wi_scope <- reactive({
    if (isTRUE(input$wi_rate_scope == "FAC")) input$wi_facility else "DISTRICT"
  })

  wi_rates_df <- reactive({
    jps <- jps_all(); req(nrow(jps) > 0)
    r <- jps[jps$facility == wi_scope() & jps$zone == "1", ]
    if (nrow(r) == 0) r <- jps[jps$facility == "DISTRICT" & jps$zone == "1", ]
    r
  })

  wi_crew_result <- reactive({
    b <- base(); req(b)
    promo <- b$sections[as.character(b$sections$sectcode) %in% sel(), ]
    rd <- wi_rates_df()
    rates <- setNames(rd$rate, rd$job_type)
    hpj <- c(air_trt = input$wi_h_air_trt, air_insp = input$wi_h_air_insp,
             gnd_insp_trt = input$wi_h_gnd_insp_trt, gnd_trt = input$wi_h_gnd_trt,
             gnd_insp = input$wi_h_gnd_insp)
    estimate_crews(promo, rates, hpj,
                   input$wi_crew_hours %||% STUDY_CREW_HOURS_SEASON)
  })

  output$wi_crew_summary <- renderUI({
    r <- wi_crew_result(); t <- r$totals
    tags$div(
      tags$p(sprintf(
        "Promoted area: %s ground + %s air sites → %s seasonal jobs → %s staff-hours.",
        format(t$ground_sites, big.mark = ","), format(t$air_sites, big.mark = ","),
        format(t$jobs, big.mark = ","), format(t$hours, big.mark = ","))),
      tags$p(HTML(sprintf(
        "<span style='font-family:Consolas,monospace;'>Crews = %s hrs &divide; %s hrs/crew = <b>%.2f</b></span>",
        format(t$hours, big.mark = ","), format(t$hrs_per_crew, big.mark = ","),
        t$crews))),
      tags$h3(style = "color:#2c5aa0;", sprintf("Crews required: %.2f", t$crews))
    )
  })

  output$wi_crew_by_job <- DT::renderDT({
    r <- wi_crew_result(); bj <- r$by_job
    total_row <- data.frame(
      Job = "TOTAL", Sites = NA, `Jobs/site` = NA,
      Jobs = sum(bj$Jobs, na.rm = TRUE), `Hrs/job` = NA,
      Hours = round(sum(bj$Hours, na.rm = TRUE), 1),
      check.names = FALSE)
    DT::datatable(rbind(bj, total_row), rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE))
  })

  output$wi_rates <- DT::renderDT({
    r <- wi_rates_df(); req(nrow(r) > 0)
    r$Job <- JOB_TYPE_LABELS[r$job_type]
    disp <- r[, c("Job", "jobs", "distinct_sites", "rate")]
    names(disp) <- c("Job", "Jobs (season)", "Distinct sites", "Jobs/site")
    DT::datatable(disp, rownames = FALSE,
      caption = sprintf("%s P1, season %s", wi_scope(), unique(r$year)),
      options = list(dom = "t", scrollX = TRUE))
  })
}

shinyApp(ui, server)
