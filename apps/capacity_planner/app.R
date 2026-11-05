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
source("../../shared/accessibility_helpers.R")
source("../../shared/url_state_helpers.R")  # deep-linking engine
source("data_functions.R")
source("display_functions.R")

set_app_name("capacity_planner")

message("[fos_capacity] Preloading lookup tables...")
tryCatch({
  get_facility_lookup(); get_foremen_lookup()
}, error = function(e) message("[fos_capacity] Preload warning: ", e$message))

# Every site count is paired with its acres (sections / catch basins are count-only)
METRIC_CHOICES <- c(
  "Ground sites"   = "ground_sites",
  "Ground acres"   = "ground_acres",
  "Air sites"      = "air_sites",
  "Air acres"      = "air_acres",
  "Prehatch sites" = "prehatch_sites",
  "Prehatch acres" = "prehatch_acres",
  "Sections"       = "sections",
  "Wet catch basins" = "wet_cb"
)
BOX_METRIC_CHOICES <- c(METRIC_CHOICES, "Avg miles from facility" = "avg_miles")

# Small "Download CSV" button used under the result tables
dl_btn <- function(id, label = "Download CSV") {
  downloadButton(id, label, class = "btn-default btn-sm")
}

# =============================================================================
# UI
# =============================================================================
ui <- accessible_page(
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', Arial, sans-serif; }
    .well { background:#f5f7fa; }
    h2 { color:#2c5aa0; }
    .metric-note { color:#555; font-size:13px; }
    caption { color:#555; }
    .shift-hint { display:inline; }
    .touch-hint { display:none; }
    @media (hover: none) {
      .shift-hint { display:none; }
      .touch-hint { display:inline; }
    }
    /* Phone layout */
    @media (max-width: 767px) {
      .container-fluid { padding-left:8px; padding-right:8px; }
      .row { margin-left:-8px; margin-right:-8px; }
      [class*='col-'] { padding-left:8px; padding-right:8px; }
      .well { padding:10px; }
      .form-control, .selectize-input { font-size:16px; }
      .btn, .shiny-download-link { width:100%; min-height:40px; margin-bottom:6px; }
      .nav-tabs { display:flex; flex-wrap:nowrap; overflow-x:auto; white-space:nowrap; }
      .nav-tabs > li { float:none; }
      .metric-note, .eq-box p { overflow-wrap:anywhere; }
      .eq-box p { font-size:11px !important; }
      table.dataTable { font-size:12px; }
      .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_info { float:none; text-align:left; }
      .dataTables_wrapper .dataTables_paginate { float:none; text-align:center; }
      #wi_map { height:320px !important; }
    }
    /* DataTables' default selected row is white text on a mid blue (4.2:1, fails WCAG AA); a clicked
       facility row (the FOS drill-down) uses a light highlight with dark text instead. html:root
       outranks the library's own :root whatever order the stylesheets load in. */
    html:root { --dt-row-selected: 205, 222, 250; --dt-row-selected-text: 31, 41, 51;
                --dt-row-selected-link: 28, 70, 140; }
    /* What-If results update in place: no fading while they recalculate */
    #wi_workload.recalculating, #wi_workload_fos.recalculating, #wi_pertech.recalculating,
    #wi_a_table.recalculating, #wi_a_fos.recalculating, #wi_crew_by_job.recalculating,
    #wi_crew_fac.recalculating, #wi_selcount.recalculating, #wi_a_summary.recalculating,
    #wi_crew_summary.recalculating { opacity:1 !important; }
  "))),
  url_state_js(),  # deep-linking client glue (shared)
  tags$script(HTML("
    window.wiSel = [];
    window.wiApplySel = function() {
      var s = window.wiSel || [];
      document.querySelectorAll('input.wi-sel').forEach(function(c) {
        c.checked = s.indexOf(c.getAttribute('data-sect')) !== -1;
      });
    };
    // DataTables a11y: the scrolling body must be keyboard reachable, and the hidden sizing
    // copy of the header that scrollX adds must not be read as empty header cells.
    function a11yDT() {
      $('.dataTables_wrapper').each(function() {
        var body = $(this).find('.dataTables_scrollBody');
        if (body.length) {
          var cap = $.trim($(this).find('caption').first().text());
          var id = ($(this).parent().attr('id') || '').replace(/_/g, ' ');
          body.attr({tabindex: 0, role: 'region',
                     'aria-label': (cap || id || 'Data') + ' table, scrollable, use arrow keys to scroll'});
          body.find('thead').attr('aria-hidden', 'true');
          body.find('thead th').each(function() {
            var tx = $.trim($(this).text());
            if (tx) { $(this).attr('aria-label', tx); }
          });
        }
      });
    }
    $(document).on('init.dt draw.dt column-sizing.dt', function() { a11yDT(); setTimeout(a11yDT, 400); });
    document.addEventListener('toggle', function(e) {
      if (e.target && e.target.open) {
        setTimeout(function() {
          if ($.fn.dataTable) { $.fn.dataTable.tables({visible: true, api: true}).columns.adjust(); }
          window.dispatchEvent(new Event('resize'));
        }, 50);
      }
    }, true);
    // The three staffing boxes are shared by whichever facility is selected. The server fills them
    // with `wi_staff_set` (which says which facility the values belong to and does not count as an
    // edit); an edit is sent, tagged with that facility, only while the facility selector and the
    // boxes agree. A value typed or applied in the instant between the selection changing and the
    // new facility's values arriving is dropped instead of being written to the wrong facility.
    window.wiStaffFac = null; window.wiStaffPushing = false;
    $(document).on('change', '#wi_techs_per_crew, #wi_drone_offload, #wi_drone_air_offload', function() {
      if (window.wiStaffPushing || !window.Shiny || !Shiny.setInputValue) { return; }
      var fac = $('#wi_facility').val();
      if (!fac || fac !== window.wiStaffFac) { return; }
      Shiny.setInputValue('wi_staff_edit', {id: this.id, value: parseFloat(this.value),
        fac: fac, nonce: Math.random()}, {priority: 'event'});
    });
    // Restyle only the changed sections on the What-If map (no redraw of every polygon)
    function wiRestyle(m, tries) {
      var w = window.HTMLWidgets && HTMLWidgets.find('#wi_map');
      var map = w && w.getMap && w.getMap();
      var ids = [].concat(m.id || []), col = [].concat(m.col || []),
          op = [].concat(m.op || []), lbl = [].concat(m.lbl || []);
      var missing = {id: [], col: [], op: [], lbl: []};
      ids.forEach(function(id, i) {
        var l = map && map.layerManager && map.layerManager.getLayer('shape', id);
        if (!l) { missing.id.push(id); missing.col.push(col[i]); missing.op.push(op[i]);
                  missing.lbl.push(lbl[i]); return; }
        l.setStyle({color: col[i], fillColor: col[i], fillOpacity: op[i]});
        if (l.setTooltipContent) { l.setTooltipContent(lbl[i]); }
      });
      if (missing.id.length && tries < 20) {          // polygons still being drawn: try again
        setTimeout(function() { wiRestyle(missing, tries + 1); }, 150);
      }
    }
    $(document).on('shiny:connected', function() {
      Shiny.addCustomMessageHandler('wi_sel_sync', function(m) {
        window.wiSel = [].concat(m || []); window.wiApplySel();
      });
      Shiny.addCustomMessageHandler('wi_restyle', function(m) { wiRestyle(m, 0); });
      Shiny.addCustomMessageHandler('wi_staff_set', function(m) {
        window.wiStaffFac = m.fac; window.wiStaffPushing = true;
        try {
          [['wi_techs_per_crew', m.techs], ['wi_drone_offload', m.off], ['wi_drone_air_offload', m.air]].forEach(function(p) {
            var el = document.getElementById(p[0]);
            if (el && p[1] !== null && p[1] !== undefined) { el.value = p[1]; $(el).trigger('change'); }
          });
        } finally { window.wiStaffPushing = false; }
      });
      // Build the link from the page's own address (the server sits behind a proxy), show it
      // and copy it; the box is the fallback when the browser blocks clipboard access.
      Shiny.addCustomMessageHandler('wi_share_link', function(m) {
        var url = window.location.origin + window.location.pathname + '?tabs=whatif&s=' + m.token;
        var box = document.getElementById('wi_share_url'),
            wrap = document.getElementById('wi_share_box'),
            st = document.getElementById('wi_share_status');
        if (!box || !wrap || !st) { return; }
        wrap.style.display = 'block'; box.value = url;
        var msg = ' (' + url.length + ' characters).';
        var warn = url.length > 2000 ? ' Some chat and email apps cut off long links; download the changed sections CSV to share instead.' : '';
        function manual() { box.focus(); box.select(); st.textContent = 'Select the link above and copy it' + msg + warn; }
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(url).then(function() {
            st.textContent = 'Link copied to the clipboard' + msg + warn;
          }, manual);
        } else { manual(); }
      });
    });
  ")),
  titlePanel("FOS Capacity & Border-to-Border Planning"),
  uiOutput("load_banner"),
  tags$p(class = "metric-note",
    "Per-FOS-area inventory for 5-year planning. Zones: P1 and P2. ",
    "All counts are the active universe (breeding sites, wet catch basins, ",
    "sections). Staffing comes from the confirmed district crew counts ",
    "(editable on the Staffing and What-If tabs), not the database active flag."),

  tabsetPanel(
    id = "tabs",

    # ---- Tab 1: Capacity by FOS ------------------------------------------
    tabPanel(
      "Capacity by FOS", value = "capacity",
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
        fluidRow(
          column(12, radioButtons("cap_measure", "Show:",
                                  c("Site counts and acres" = "both",
                                    "Site counts only" = "sites",
                                    "Acres only" = "acres"),
                                  selected = "both", inline = TRUE))
        ),
        conditionalPanel(
          "input.cap_drivetime",
          fluidRow(
            column(6, radioButtons("cap_drive_mode", "Drive time from:",
                                   c("Routed (OpenStreetMap roads)" = "routed",
                                     "Straight-line estimate" = "estimate"),
                                   selected = "routed", inline = TRUE))
          ),
          conditionalPanel(
            "input.cap_drive_mode == 'routed'",
            fluidRow(column(3, numericInput("cap_speed_pct", "Field speed vs. map speed (%)",
                                            100, 20, 200, 5)))
          ),
          conditionalPanel(
            "input.cap_drive_mode == 'estimate'",
            fluidRow(
              column(3, numericInput("avg_mph", "Avg mph", 45, 5, 80, 1)),
              column(3, numericInput("circuity", "Road circuity factor",
                                     1.3, 1, 2, 0.05))
            )
          ),
          tags$p(class = "metric-note",
            tags$b("Routed"), " times are real driving routes from the facility to each ",
            "section's centroid on OpenStreetMap roads (free-flow speed limits, no live ",
            "traffic; a centroid in a wetland is routed to the nearest road). Set the field ",
            "speed below 100% if crews drive slower than the map assumes. ",
            tags$b("Circuity"), " = road distance / straight-line distance. The straight-line ",
            "estimate is straight-line miles x circuity / average mph. The table compares ",
            "the two for each facility."),
          uiOutput("drive_note"),
          div(style = "overflow-x:auto;max-width:900px;", DT::DTOutput("drive_summary")),
          div(style = "margin:6px 0;", dl_btn("dl_drive", "Download drive-time summary (CSV)"))
        )
      ),
      h4("Capacity per FOS area (P1 / P2)"),
      tags$p(class = "metric-note",
             "Columns are labelled as site counts or acres (ground, air and prehatch ",
             "each have both). Sections and wet catch basins are counts only. ",
             "Use the toggle above to show sites, acres, or both."),
      div(style = "overflow-x:auto;", DT::DTOutput("cap_table")),
      br(),
      h4("Averages per FOS area"),
      tags$p(class = "metric-note",
             "Mean and median load per FOS area, by facility and district-wide (all facilities, ",
             "regardless of the facility filter above). In the ",
             "box plot each point is one FOS area; the box is the middle 50% with the ",
             "median line, the dashed line is the mean, and whiskers reach the ",
             "lowest/highest values within 1.5 x the box height."),
      div(style = "margin-bottom:8px;", dl_btn("dl_avg", "Download averages (CSV)")),
      selectInput("avg_metric", "Box plot metric:", choices = BOX_METRIC_CHOICES,
                  selected = "ground_sites", width = "320px"),
      tabsetPanel(
        tabPanel("P1", br(), plotly::plotlyOutput("box_p1", height = 380),
                 uiOutput("box_note_p1"), br(), DT::DTOutput("avg_p1")),
        tabPanel("P2", br(), plotly::plotlyOutput("box_p2", height = 380),
                 uiOutput("box_note_p2"), br(), DT::DTOutput("avg_p2"))
      )
    ),

    # ---- Tab 2: Staffing --------------------------------------------------
    tabPanel(
      "Staffing", value = "staffing",
      br(),
      h4("Staffing assumptions (editable)"),
      tags$p(class = "metric-note",
             "Confirmed district staffing. One crew = 1 FOS area + its ground techs. ",
             "Double-click a Crews, Techs/crew, Drone techs or offload cell to ",
             "change it; all estimates update. Drone techs are kept out of the ground ",
             "tech count. Ground offload % = share of a facility's ground-treated load ",
             "(ground sites, prehatch sites/acres) that drones take over. Air offload % = ",
             "share of air-site inspections drones take over. All ground techs inspect air ",
             "sites too; drones only help. Offloads lower the load the techs are staffed for."),
      div(style = "overflow-x:auto;", DT::DTOutput("staff_edit")),
      div(style = "margin-top:6px;",
          actionButton("staff_reset", "Reset to defaults", class = "btn-default btn-sm"),
          dl_btn("dl_staff", "Download staffing (CSV)")),
      uiOutput("staff_check"),
      br(),
      h4("Baseline: what each tech and crew carries (P1)"),
      tags$p(class = "metric-note",
             "P1 ground, air and prehatch sites and acres divided by ground techs and ",
             "crews. This is the average load a tech / crew carries, and the baseline ",
             "for the What-If planner. All ground techs inspect air sites."),
      div(style = "overflow-x:auto;", DT::DTOutput("staff_baseline")),
      div(style = "margin:6px 0;", dl_btn("dl_baseline", "Download baseline (CSV)")),
      br(),
      h4("Techs per FOS area"),
      tags$p(class = "metric-note",
             "Each FOS area is one crew, so it carries its facility's techs per crew. ",
             "Load-per-tech uses P1 load."),
      div(style = "overflow-x:auto;", DT::DTOutput("staff_table")),
      div(style = "margin:6px 0;", dl_btn("dl_techs", "Download techs per FOS area (CSV)"))
    ),

    # ---- Tab 3: What-If Planner ------------------------------------------
    tabPanel(
      "What-If Planner", value = "whatif",
      br(),
      uiOutput("wi_shared_note"),
      h4("1. Build a scenario"),
      tags$p(class = "metric-note",
        "Click a P2 section on the map, or drag a box, to promote it to P1 right away. Pick a ",
        "destination first to also move it to another facility; it then takes that facility's ",
        "color. Select it again to put it back."),
      wellPanel(
        fluidRow(
          column(4, selectInput("wi_move_to", "Move sections to:",
                                choices = c("Keep facility" = "keep")))
        ),
        div(style = "display:flex;align-items:center;gap:12px;flex-wrap:wrap;",
          actionButton("wi_undo", "Reset scenario", class = "btn-default btn-sm"),
          dl_btn("dl_wi_sections", "Download changed sections (CSV)"),
          actionButton("wi_share", "Copy link to this scenario", icon = icon("link"),
                       class = "btn-default btn-sm"),
          uiOutput("wi_selcount", inline = TRUE)),
        tags$div(id = "wi_share_box", style = "display:none;margin-top:8px;",
          tags$label(`for` = "wi_share_url", "Link to this scenario"),
          tags$input(id = "wi_share_url", type = "text", readonly = NA, class = "form-control",
                     onfocus = "this.select()"),
          tags$p(id = "wi_share_status", class = "metric-note", role = "status",
                 style = "margin:4px 0 0;"))
      ),
      a11y_figure(
        leaflet::leafletOutput("wi_map", height = 440),
        label = paste("Map of all priority 1 and priority 2 sections, colored by facility.",
                      "Click or tap a section to promote it to P1 and again to undo it, or tick",
                      "sections in the section list below.")),
      tags$p(class = "metric-note", style = "margin:6px 0;",
        tags$span(class = "touch-hint", "Tap a section to promote it (tap again to undo). To do many, turn on ",
          tags$b("Box select"), " on the map and drag one box (it turns off after each box). "),
        tags$span(class = "shift-hint", "Click a section to promote it (click again to undo); hold ",
          tags$b("Shift"), " and drag a box to do many. "),
        "Color is the facility each section belongs to; solid is P1 and light is P2. ",
        "Promoted sections turn solid, and moved sections take the new facility's color."),
      tags$details(
        tags$summary(style = "cursor:pointer;color:#2c5aa0;",
                     "Show section list (tick a box to promote or undo a section; works with the keyboard)"),
        div(style = "overflow-x:auto;margin-top:8px;", DT::DTOutput("wi_sections"))
      ),
      hr(),
      h4("2. What changed"),
      tags$p(class = "metric-note",
        "Added to P1, by facility, with the median one-way drive minutes to the current P1 ",
        "sections and to the added ones, and their ratio (above 1 = the added sections are ",
        "farther away, so more driving). A move shows as + at the facility that receives the sections and - at ",
        "the one that gives them up. Click a facility row, or pick it below, to see its FOS areas."),
      fluidRow(
        column(3, checkboxInput("wi_travel", "Include travel time", TRUE)),
        column(5, conditionalPanel(
          "input.wi_travel",
          radioButtons("wi_drive_mode", "One-way drive time from:",
                       c("Routed (OpenStreetMap roads)" = "routed",
                         "Straight-line estimate" = "estimate"),
                       selected = "routed", inline = TRUE))),
        column(3, conditionalPanel(
          "input.wi_travel",
          numericInput("wi_travel_share", "Travel share of workday (%)", 20, 0, 90, 5)))
      ),
      radioButtons("wi_measure", "Show:",
                   c("Site counts and acres" = "both", "Site counts only" = "sites",
                     "Acres only" = "acres"), selected = "both", inline = TRUE),
      div(style = "overflow-x:auto;", DT::DTOutput("wi_workload")),
      div(style = "margin:6px 0;",
          dl_btn("dl_wi_changes", "Download by facility (CSV)"),
          dl_btn("dl_wi_fos", "Download by FOS area (CSV)")),
      selectInput("wi_drill", "Break down by FOS area:", choices = c("Pick a facility" = ""),
                  width = "320px"),
      uiOutput("wi_drill_title"),
      div(style = "overflow-x:auto;", DT::DTOutput("wi_workload_fos")),
      hr(),
      h4("3. Load per tech and per crew (P1), now to after"),
      tags$p(class = "metric-note",
        "Same staffing assumptions as the Staffing tab."),
      div(style = "overflow-x:auto;", DT::DTOutput("wi_pertech")),
      div(style = "margin:6px 0;", dl_btn("dl_wi_pertech", "Download load per tech and crew (CSV)")),
      hr(),
      h4("4. Staffing estimates"),
      fluidRow(
        column(3, selectInput("wi_metric", "Estimate A driver:", choices = METRIC_CHOICES,
                              selected = "ground_sites")),
        column(3, numericInput("wi_per_tech", "Load per tech (blank = district avg)",
                               value = NA, min = 1))
      ),
      tags$div(class = "eq-box", style = paste0("background:#eef3fb;border:1px solid #d0daec;",
                              "border-radius:6px;padding:10px 14px;margin:6px 0;"),
        tags$div(style = "font-weight:600;color:#2c5aa0;margin-bottom:4px;",
                 "How the estimates are computed"),
        tags$p(style = "margin:4px 0;font-family:Consolas,monospace;font-size:13px;",
          HTML("<b>A. Techs change</b> (per FOS area) = &lceil;(P1 &minus; removed + added &divide; capacity factor) &divide; load-per-tech&rceil; &minus; &lceil;P1 &divide; load-per-tech&rceil; &nbsp; (negative = techs freed; sections moved in from another facility form an unassigned row with no current P1)")),
        tags$p(style = "margin:0 0 4px 18px;font-family:Consolas,monospace;font-size:12px;color:#555;",
          HTML("capacity factor = 1 &divide; [ (1 &minus; <i>s</i>) + <i>s</i> &times; travel ratio ] &nbsp;|&nbsp; <i>s</i> = travel share of workday")),
        tags$p(style = "margin:0 0 4px 18px;font-family:Consolas,monospace;font-size:12px;color:#555;",
          HTML("travel ratio = median <i>t</i>(added sections, timed from the facility) &divide; median <i>t</i>(P1 sections now)")),
        tags$p(style = "margin:4px 0;font-family:Consolas,monospace;font-size:13px;",
          HTML("<b>B. Net crews</b> = [ &Sigma;<sub>job type</sub> ( (added &minus; removed sites) &times; jobs-per-site &times; hours-per-job ) + travel ] &divide; hours-per-crew-season")),
        tags$p(style = "margin:0 0 4px 18px;font-family:Consolas,monospace;font-size:12px;color:#555;",
          HTML("travel = &Sigma;<sub>section</sub> ( round trips &times; 2 &times; <i>t</i> ), added minus removed")),
        tags$p(style = "margin:4px 0 4px 18px;font-family:Consolas,monospace;font-size:12px;color:#555;",
          HTML("<i>t</i> = one-way drive time = routed minutes &divide; field speed % &nbsp;|&nbsp; miles &times; circuity &divide; mph &times; 60 (straight-line)")),
        tags$p(style = "margin:0 0 4px 18px;font-family:Consolas,monospace;font-size:12px;color:#555;",
          HTML("travel time off: capacity factor = 1, travel = 0. travel ratio above 1 = more driving than the current P1, below 1 = less"))
      ),
      h5("Estimate A: techs from load per tech, per FOS area"),
      uiOutput("wi_a_summary"),
      tags$p(class = "metric-note",
        "By facility (the sum of its FOS areas). Click a facility, or use the picker above, ",
        "to see its FOS areas."),
      div(style = "overflow-x:auto;", DT::DTOutput("wi_a_table")),
      uiOutput("wi_a_drill_title"),
      div(style = "overflow-x:auto;", DT::DTOutput("wi_a_fos")),
      div(style = "margin:6px 0;", dl_btn("dl_wi_a", "Download Estimate A, per FOS area (CSV)")),
      hr(),
      h5("Estimate B: net crews (2001 Workload Study method)"),
      uiOutput("wi_crew_summary"),
      fluidRow(
        column(7,
          div(style = "overflow-x:auto;", DT::DTOutput("wi_crew_by_job")),
          div(style = "margin:6px 0;", dl_btn("dl_wi_b_job", "Download by job type (CSV)")),
          h5("By facility"),
          div(style = "overflow-x:auto;", DT::DTOutput("wi_crew_fac")),
          div(style = "margin:6px 0;", dl_btn("dl_wi_b_fac", "Download by facility (CSV)"))
        ),
        column(5,
          h5("Jobs per site used in Estimate B"),
          tags$p(class = "metric-note",
            "How many times each P1 site was actually serviced per season (jobs ÷ distinct ",
            "sites, from real inspection/treatment records for the season chosen in the ",
            "settings below). Estimate B multiplies the sites entering P1 by these rates. ",
            "Drone treatments are not counted as air work."),
          div(style = "overflow-x:auto;", DT::DTOutput("wi_rates"))
        )
      ),
      tags$details(
        tags$summary(style = "cursor:pointer;color:#2c5aa0;font-weight:600;",
                     "Estimate settings: staffing for one facility, jobs per site, hours per job, trips"),
        wellPanel(
          fluidRow(
            column(4, selectInput("wi_facility", "Staffing facility (edits apply to it):", choices = NULL)),
            column(4, numericInput("wi_techs_per_crew",
                                   "Techs per crew (this facility)", value = NA, min = 0, step = 0.01)),
            column(4, numericInput("wi_drone_offload",
                                   "Drone offload % of ground load (this facility)",
                                   value = NA, min = 0, max = 100, step = 5))
          ),
          fluidRow(
            column(4, numericInput("wi_drone_air_offload",
                                   "Drone offload % of air inspections (this facility)",
                                   value = NA, min = 0, max = 100, step = 5)),
            column(8, uiOutput("wi_baseline"))
          ),
          tags$p(class = "metric-note",
                 "Staffing edits here are shared with the Staffing tab. Ground offload ",
                 "applies to ground sites and prehatch sites/acres; air offload applies to ",
                 "air sites (all techs inspect air sites, drones help). Sections and catch ",
                 "basins are not offloaded."),
          fluidRow(
            column(2, selectInput("wi_rate_year", "Jobs/site season:", choices = NULL)),
            column(3, radioButtons("wi_rate_scope", "Jobs/site rates:",
                                   c("District" = "DISTRICT", "Each facility's own" = "FAC"),
                                   selected = "DISTRICT", inline = TRUE)),
            column(2, numericInput("wi_crew_hours", "Hrs / crew-season", 2184, 100, step = 1))
          ),
          fluidRow(
            column(2, numericInput("wi_h_air_trt", "Hrs air trt", 1.35, 0, step = 0.05)),
            column(2, numericInput("wi_h_air_insp", "Hrs air insp", 0.66, 0, step = 0.05)),
            column(2, numericInput("wi_h_gnd_insp_trt", "Hrs gnd insp&trt", 1.08, 0, step = 0.05)),
            column(2, numericInput("wi_h_gnd_trt", "Hrs gnd trt", 0.55, 0, step = 0.05)),
            column(2, numericInput("wi_h_gnd_insp", "Hrs gnd insp", 0.17, 0, step = 0.05))
          ),
          tags$p(class = "metric-note", style = "margin:8px 0 2px;",
                 tags$b("Drive time"), " (added to Estimate B's hours): each section is driven ",
                 "this many round trips per season. Set trips to 0 to exclude travel (e.g. if ",
                 "the hours-per-job above already include drive time)."),
          fluidRow(
            column(3, numericInput("wi_crew_trips", "Round trips/section (season)",
                                   DEFAULT_TRIPS_PER_SECTION, 0, step = 1)),
            column(3, conditionalPanel(
              "input.wi_travel && input.wi_drive_mode == 'routed'",
              numericInput("wi_speed_pct", "Field speed vs. map speed (%)", 100, 20, 200, 5))),
            column(2, conditionalPanel(
              "input.wi_travel && input.wi_drive_mode == 'estimate'",
              numericInput("wi_avg_mph", "Avg mph", 45, 5, 80, 1))),
            column(3, conditionalPanel(
              "input.wi_travel && input.wi_drive_mode == 'estimate'",
              numericInput("wi_circuity", "Road circuity factor", 1.3, 1, 2, 0.05)))
          )
        )
      )
    )
  )
)

# =============================================================================
# DEEP LINKS / SHARED SCENARIOS
# =============================================================================
# Every What-If input is a deep-link parameter (hand-editable), and the same list is what a
# shared-scenario link saves and restores (share_state_settings() only accepts these ids).
DEEPLINK_SPEC <- list(
  tabs           = list(input = "tabs",         type = "tab"),
  cap_facility   = list(input = "cap_facility", type = "select"),
  cap_priority   = list(input = "cap_priority", type = "checkbox"),
  cap_measure    = list(input = "cap_measure",  type = "radio"),
  avg_metric     = list(input = "avg_metric",   type = "select"),
  cap_drivetime  = list(input = "cap_drivetime", type = "checkbox"),
  avg_mph        = list(input = "avg_mph",      type = "numeric"),
  circuity       = list(input = "circuity",     type = "numeric"),
  cap_drive_mode = list(input = "cap_drive_mode", type = "radio"),
  cap_speed_pct  = list(input = "cap_speed_pct",  type = "numeric"),
  wi_travel      = list(input = "wi_travel",      type = "checkbox"),
  wi_drive_mode  = list(input = "wi_drive_mode",  type = "radio"),
  wi_speed_pct   = list(input = "wi_speed_pct",   type = "numeric"),
  wi_travel_share = list(input = "wi_travel_share", type = "numeric"),
  wi_facility    = list(input = "wi_facility",  type = "select"),
  wi_metric      = list(input = "wi_metric",    type = "select"),
  wi_per_tech    = list(input = "wi_per_tech",  type = "numeric"),
  wi_techs_per_crew = list(input = "wi_techs_per_crew", type = "numeric"),
  wi_drone_offload  = list(input = "wi_drone_offload",  type = "numeric"),
  wi_drone_air_offload = list(input = "wi_drone_air_offload", type = "numeric"),
  wi_rate_year   = list(input = "wi_rate_year", type = "select"),
  wi_rate_scope  = list(input = "wi_rate_scope", type = "radio"),
  wi_crew_hours  = list(input = "wi_crew_hours", type = "numeric"),
  wi_crew_trips  = list(input = "wi_crew_trips", type = "numeric"),
  wi_avg_mph     = list(input = "wi_avg_mph",   type = "numeric"),
  wi_circuity    = list(input = "wi_circuity",  type = "numeric"),
  wi_measure     = list(input = "wi_measure",   type = "radio"),
  wi_drill       = list(input = "wi_drill",     type = "select"),
  wi_move_to     = list(input = "wi_move_to",   type = "select"),
  wi_h_air_trt      = list(input = "wi_h_air_trt",      type = "numeric"),
  wi_h_air_insp     = list(input = "wi_h_air_insp",     type = "numeric"),
  wi_h_gnd_insp_trt = list(input = "wi_h_gnd_insp_trt", type = "numeric"),
  wi_h_gnd_trt      = list(input = "wi_h_gnd_trt",      type = "numeric"),
  wi_h_gnd_insp     = list(input = "wi_h_gnd_insp",     type = "numeric")
)
# The three staffing boxes just show the selected facility's row of the staffing table, which a
# link saves whole, so they are not saved separately.
SHARE_SETTING_IDS <- setdiff(grep("^wi_", names(DEEPLINK_SPEC), value = TRUE),
                             c("wi_techs_per_crew", "wi_drone_offload", "wi_drone_air_offload"))

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # Deep-linking (shared/url_state_helpers.R). This app is LIVE (estimates
  # recompute reactively, no Refresh button), so refresh_for returns
  # character(0): the engine applies inputs (with its two-pass settle so the
  # facility->section-selection reset doesn't clobber deep-linked values) and
  # clicks nothing.
  wire_deep_links(input, output, session, spec = DEEPLINK_SPEC,
                  refresh_for = function(state) character(0),
                  expand_state = function(state) expand_share(state))

  # ---- Staffing state (defaults from config; user-editable) -------------
  # Single source for the Staffing and What-If tabs.
  staff <- reactiveVal(default_staffing())

  # ---- Load once per session --------------------------------------------
  # The loaders swallow database errors and return empty tables. Saving an empty
  # result for the whole session made the What-If sections show Shiny's generic
  # "check your logs" error until the page was reloaded, so a failed load is
  # reported (banner + log line) and retried via the button instead.
  base <- reactiveVal(NULL)
  load_failed <- reactiveVal(FALSE)
  load_nonce  <- reactiveVal(0)
  routed_available <- reactiveVal(FALSE)
  observeEvent(input$data_retry, load_nonce(load_nonce() + 1))
  observe({
    load_nonce()
    if (!is.null(isolate(base()))) return()
    sections <- tryCatch(load_capacity_sections(), error = function(e) {
      message("[capacity_planner] load_capacity_sections failed: ", conditionMessage(e))
      data.frame()
    })
    if (is.null(sections) || nrow(sections) == 0) {
      message("[capacity_planner] no section data returned (database unreachable or query failed)")
      load_failed(TRUE)
      return()
    }
    load_failed(FALSE)
    routed <- load_routed_drivetimes()          # every facility x section pair
    sections <- attach_routed_drivetimes(sections, routed)
    has_routed <- any(!is.na(sections$drive_min_routed))
    routed_available(has_routed)
    for (id in c("cap_drive_mode", "wi_drive_mode")) {
      updateRadioButtons(session, id, selected = if (has_routed) "routed" else "estimate")
    }
    cap      <- aggregate_capacity_by_fos(sections, staff())
    priority <- load_capacity_by_priority()
    # FOS name onto sections for the what-if picker
    foremen <- tryCatch(get_foremen_lookup(), error = function(e) data.frame())
    fos_name <- if (nrow(foremen) > 0) {
      setNames(foremen$shortname, as.character(foremen$emp_num))
    } else character(0)
    sections$fos_name <- ifelse(as.character(sections$fosarea) %in% names(fos_name),
                                fos_name[as.character(sections$fosarea)],
                                paste0("FOS ", sections$fosarea))
    base(list(sections = sections, priority = priority, times = routed))
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
    updateSelectInput(session, "wi_move_to", choices = c("Keep facility" = "keep", fac_choices))
    updateSelectInput(session, "wi_drill", choices = c("Pick a facility" = "", fac_choices))
    # Jobs/site season selector: last complete year default, choices last 6 yrs
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    yrs <- as.character(seq(cur_yr, cur_yr - 5))
    updateSelectInput(session, "wi_rate_year", choices = yrs,
                      selected = as.character(cur_yr - 1))
  })

  output$load_banner <- renderUI({
    if (!isTRUE(load_failed())) return(NULL)
    div(class = "alert alert-warning", role = "alert",
        tags$b("Capacity data could not be loaded from the database. "),
        "Nothing is wrong with your selections; this is usually a short connection problem. ",
        actionButton("data_retry", "Try again", class = "btn-sm btn-default"))
  })

  # Capacity table re-aggregated whenever the staffing assumptions change
  cap_live <- reactive({
    b <- base(); req(b)
    aggregate_capacity_by_fos(b$sections, staff())
  })

  cap_filtered <- reactive({
    cap <- cap_live()
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
      pcols <- c("ground_sites", "ground_acres", "air_sites", "air_acres",
                 "prehatch_sites", "prehatch_acres")
      pcols <- pcols[pcols %in% capacity_visible_metrics(input$cap_measure %||% "both")]
      disp <- pri[, c("facility", "FOS", "Zone", "priority", pcols)]
      names(disp) <- c("Facility", "FOS", "Zone", "Priority",
                       unname(CAPACITY_METRIC_LABELS[pcols]))
      return(capacity_dt(disp, left = 2, rownames = FALSE, filter = "top",
                         options = list(pageLength = 25)))
    }

    w <- capacity_wide(cap)
    if (isTRUE(input$cap_drivetime)) {
      secs <- base()$sections
      secs$dm <- section_drive_minutes(secs, input$cap_drive_mode %||% "estimate",
                                       input$avg_mph %||% 45, input$circuity %||% 1.3,
                                       input$cap_speed_pct %||% 100)
      m <- stats::aggregate(dm ~ fosarea + zone, data = secs,
                            FUN = function(x) round(mean(x, na.rm = TRUE), 1))
      for (z in c("1", "2")) {
        w[[paste0("drive_min_P", z)]] <- m$dm[match(paste(w$fosarea, z), paste(m$fosarea, m$zone))]
      }
    }
    disp <- select_measure_cols(w, input$cap_measure %||% "both")
    disp$facility <- NULL; disp$fosarea <- NULL
    if ("n_techs" %in% names(disp)) disp$n_techs <- round(disp$n_techs, 1)  # 40 techs / 6 crews
    # explicit labels: "Ground sites P1", "Ground acres P1", ...
    names(disp) <- vapply(names(disp), capacity_col_label, character(1))
    capacity_dt(disp, left = 2, rownames = FALSE, filter = "top",
                options = list(pageLength = 40))
  })

  output$drive_note <- renderUI({
    if (isTRUE(routed_available())) return(NULL)
    tags$p(class = "metric-note", style = "color:#a15c00;",
           "Routed drive times are not available for this deployment; the straight-line ",
           "estimate is being used.")
  })

  drive_summary_df <- reactive({
    b <- base(); req(b)
    s <- routed_summary(b$sections, input$avg_mph %||% 45, input$circuity %||% 1.3)
    req(nrow(s) > 0)
    s
  })
  output$drive_summary <- DT::renderDT({
    capacity_dt(drive_summary_df(), rownames = FALSE, selection = "none",
                caption = "Median per section (sections more than 1 mile from the facility)",
                options = list(dom = "t", ordering = FALSE))
  })

  output$cap_download <- downloadHandler(
    filename = function() paste0("fos_capacity_", Sys.Date(), ".csv"),
    content = function(file) {
      cap <- cap_filtered()
      out <- if (isTRUE(input$cap_priority)) base()$priority else capacity_wide(cap)
      out <- select_measure_cols(out, input$cap_measure %||% "both")
      utils::write.csv(out, file, row.names = FALSE)
    }
  )

  # The averages section always covers every facility (not the facility filter), so
  # the DISTRICT row / box is the true district, not just the filtered facility.
  avg_data <- function(zone_val) {
    cap <- cap_live(); req(nrow(cap) > 0)
    measure <- input$cap_measure %||% "both"
    a <- capacity_averages(cap, zone_val, capacity_visible_metrics(measure))
    names(a) <- vapply(names(a), capacity_col_label, character(1))
    a
  }
  avg_table <- function(zone_val) {
    capacity_dt(avg_data(zone_val), rownames = FALSE, options = list(pageLength = 15))
  }
  output$avg_p1 <- DT::renderDT(avg_table("1"))
  output$avg_p2 <- DT::renderDT(avg_table("2"))

  # Box plot of one metric across FOS areas: DISTRICT box + one box per facility
  box_plot <- function(zone_val) {
    cap <- cap_live(); req(nrow(cap) > 0)
    m <- input$avg_metric %||% "ground_sites"
    d <- capacity_boxplot_data(cap, zone_val, m); req(nrow(d) > 0)
    plotly::plot_ly(
      d, x = ~group, y = ~value, type = "box", boxpoints = "all", jitter = 0.4,
      pointpos = 0, boxmean = TRUE, text = ~fos_name, hoverinfo = "y+text",
      line = list(color = "#2c5aa0", width = 1.5),
      fillcolor = "rgba(44,90,160,0.18)",
      marker = list(color = "#2c5aa0", size = 5, opacity = 0.7)) %>%
      plotly::layout(
        xaxis = list(title = ""),
        yaxis = list(title = sprintf("%s per FOS area (P%s)",
                                     capacity_col_label(m), zone_val)),
        showlegend = FALSE, margin = list(l = 50, r = 10, t = 10, b = 60)) %>%
      plotly::config(displayModeBar = FALSE)
  }
  # Text summary of the same numbers (the plot itself is not screen-reader friendly)
  box_note <- function(zone_val) {
    cap <- cap_live(); req(nrow(cap) > 0)
    m <- input$avg_metric %||% "ground_sites"
    d <- capacity_boxplot_data(cap, zone_val, m)
    v <- d$value[d$group == "DISTRICT"]; req(length(v) > 0)
    tags$p(class = "metric-note", sprintf(
      "DISTRICT %s, P%s, across %d FOS areas: min %s, median %s, mean %s, max %s.",
      tolower(capacity_col_label(m)), zone_val, length(v),
      format(round(min(v), 1), big.mark = ","), format(round(stats::median(v), 1), big.mark = ","),
      format(round(mean(v), 1), big.mark = ","), format(round(max(v), 1), big.mark = ",")))
  }
  output$box_p1 <- plotly::renderPlotly(box_plot("1"))
  output$box_p2 <- plotly::renderPlotly(box_plot("2"))
  output$box_note_p1 <- renderUI(box_note("1"))
  output$box_note_p2 <- renderUI(box_note("2"))

  # ---- Staffing table ---------------------------------------------------
  # Editable assumptions table. Column indexes are 0-based (rownames = FALSE).
  STAFF_EDIT_COLS <- c("1" = "crews", "2" = "techs_per_crew",
                       "4" = "drone_techs", "5" = "drone_offload_pct",
                       "6" = "drone_air_offload_pct")

  staff_edit_df <- reactive({
    b <- base(); req(b)
    s <- staff()
    db <- db_fos_area_counts(b$sections)
    d <- data.frame(
      Facility = s$facility, Crews = s$crews, `Techs/crew` = s$techs_per_crew,
      `Ground techs` = s$crews * s$techs_per_crew, `Drone techs` = s$drone_techs,
      `Ground offload %` = s$drone_offload_pct,
      `Air offload %` = s$drone_air_offload_pct,
      `DB FOS areas` = db$db_fos_areas[match(s$facility, db$facility)],
      check.names = FALSE)
    tot <- data.frame(Facility = "DISTRICT", Crews = sum(d$Crews), `Techs/crew` = NA,
                      `Ground techs` = sum(d$`Ground techs`),
                      `Drone techs` = sum(d$`Drone techs`),
                      `Ground offload %` = NA, `Air offload %` = NA,
                      `DB FOS areas` = sum(d$`DB FOS areas`, na.rm = TRUE),
                      check.names = FALSE)
    rbind(d, tot)
  })

  output$staff_edit <- DT::renderDT({
    DT::datatable(staff_edit_df(), rownames = FALSE, selection = "none",
                  editable = list(target = "cell", disable = list(columns = c(0, 3, 7))),
                  options = list(dom = "t", pageLength = 20, ordering = FALSE)) %>%
      DT::formatRound("Techs/crew", 2)
  })

  observeEvent(input$staff_edit_cell_edit, {
    info <- input$staff_edit_cell_edit
    col <- STAFF_EDIT_COLS[as.character(info$col)]
    s <- staff()
    if (is.na(col) || info$row > nrow(s)) return()   # DISTRICT row / locked columns
    v <- suppressWarnings(as.numeric(info$value))
    if (is.na(v) || v < 0 || (grepl("offload_pct$", col) && v > 100)) {
      showNotification("Enter a non-negative number (offload is 0-100).", type = "warning")
      staff(s)   # re-render to discard the bad edit
      return()
    }
    s[[col]][info$row] <- v
    staff(s)
  })

  observeEvent(input$staff_reset, staff(default_staffing()))

  output$staff_check <- renderUI({
    b <- base(); req(b)
    s <- staff(); db <- db_fos_area_counts(b$sections)
    n <- db$db_fos_areas[match(s$facility, db$facility)]
    bad <- !is.na(n) & n != s$crews
    if (!any(bad)) return(NULL)
    tags$p(class = "metric-note", style = "color:#a15c00;margin-top:6px;",
      "Check: ", paste(sprintf("%s has %d crews but %d FOS areas in the database",
                               s$facility[bad], as.integer(s$crews[bad]), as.integer(n[bad])),
                       collapse = "; "),
      ". Per-FOS numbers assume one crew per FOS area; district totals use the crews entered above.")
  })

  staff_baseline_df <- reactive({
    bl <- staffing_baseline(cap_live(), staff()); req(nrow(bl) > 0)
    cols <- c(facility = "Facility", crews = "Crews", ground_techs = "Ground techs",
              drone_techs = "Drone techs",
              p1_ground_sites = "P1 Ground sites", ground_sites_per_tech = "Ground sites/tech",
              p1_ground_acres = "P1 Ground acres", ground_acres_per_tech = "Ground acres/tech",
              ground_acres_per_crew = "Ground acres/crew",
              p1_air_sites = "P1 Air sites", air_sites_per_tech = "Air sites/tech",
              p1_air_acres = "P1 Air acres", air_acres_per_tech = "Air acres/tech",
              air_acres_per_crew = "Air acres/crew",
              p1_ph_sites = "P1 Prehatch sites", ph_sites_per_tech = "Prehatch sites/tech",
              p1_ph_acres = "P1 Prehatch acres", ph_acres_per_tech = "Prehatch acres/tech",
              ph_acres_per_crew = "Prehatch acres/crew")
    disp <- bl[, names(cols)]
    names(disp) <- unname(cols)
    disp
  })
  output$staff_baseline <- DT::renderDT({
    capacity_dt(staff_baseline_df(), rownames = FALSE, selection = "none",
                options = list(dom = "t", pageLength = 20, ordering = FALSE))
  })

  staff_table_df <- reactive({
    b <- base(); req(b)
    cap <- cap_live()
    p1 <- cap[cap$zone == "1", ]
    pt <- function(x) ifelse(p1$n_techs > 0, round(x / p1$n_techs, 1), NA)
    p1$ground_per_tech <- pt(p1$ground_sites)
    p1$ground_acres_per_tech <- pt(p1$ground_acres)
    p1$air_per_tech <- pt(p1$air_sites)
    p1$air_acres_per_tech <- pt(p1$air_acres)
    p1$sections_per_tech <- pt(p1$sections)
    p1$wetcb_per_tech <- pt(p1$wet_cb)
    s <- staff()
    p1$fos_in_facility <- as.integer(s$crews[match(p1$facility, s$facility)])
    p1$n_techs <- round(p1$n_techs, 1)
    disp <- p1[, c("facility_name", "fos_name", "n_techs", "fos_in_facility",
                   "ground_sites", "ground_per_tech", "ground_acres", "ground_acres_per_tech",
                   "air_sites", "air_per_tech", "air_acres", "air_acres_per_tech",
                   "sections", "sections_per_tech", "wet_cb", "wetcb_per_tech")]
    names(disp) <- c("Facility", "FOS", "Techs", "FOS in facility",
                     "P1 Ground sites", "Ground sites/tech", "P1 Ground acres", "Ground acres/tech",
                     "P1 Air sites", "Air sites/tech", "P1 Air acres", "Air acres/tech",
                     "P1 Sections", "Sections/tech", "P1 Wet CB", "Wet CB/tech")
    disp
  })
  output$staff_table <- DT::renderDT({
    capacity_dt(staff_table_df(), left = 2, rownames = FALSE, filter = "top",
                options = list(pageLength = 40))
  })

  # ---- What-If: scenario state -------------------------------------------
  # changes = the scenario. Every section the user clicks is promoted to P1 right away,
  # and also moved to the "Move sections to" facility when one is picked. Every section is
  # always shown on the map.
  no_changes <- data.frame(sectcode = character(), to_facility = character(),
                           to_zone = character(), stringsAsFactors = FALSE)
  changes <- reactiveVal(no_changes)
  wi_msg <- reactiveVal("")

  # ---- Shared scenarios (a link carries the whole scenario in its `s` parameter) ----
  # expand_share() runs once when the page opens from a link (shared/url_state_helpers.R):
  # it restores the scenario, the staffing table and the map view, and hands the saved settings
  # back to the engine, which applies them like ordinary deep-link params (explicit URL params
  # win). A link that cannot be restored leaves the app at its starting point and says why.
  shared_info <- reactiveVal(NULL)
  restore_view <- reactiveVal(NULL)

  expand_share <- function(state) {
    tok <- state[["s"]]
    if (is.null(tok) || length(tok) == 0 || !nzchar(as.character(tok)[1])) return(state)
    if (is.null(state[["tabs"]])) state[["tabs"]] <- "whatif"
    x <- share_token_decode(as.character(tok)[1])
    if (is.null(x)) { shared_info(list(status = "invalid")); return(state) }
    date <- if (is.character(x$t) && length(x$t) == 1 && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x$t)) x$t else ""
    b <- isolate(base())
    if (is.null(b)) { shared_info(list(status = "nodata", date = date)); return(state) }
    un <- scenario_unpack(x, b$sections$sectcode)
    if (!isTRUE(un$ok)) { shared_info(list(status = un$reason, date = date)); return(state) }
    changes(un$changes)
    staff(share_state_staff(x, isolate(staff())))
    restore_view(share_state_view(x))
    a <- scenario_apply(b$sections, un$changes, b$times)
    zone <- as.character(b$sections$zone)
    num <- function(v) if (length(v) == 1 && is.numeric(v)) v else NA_real_
    then_changed <- !is.na(num(x$d$p1)) && (num(x$d$p1) != sum(zone == "1") || num(x$d$p2) != sum(zone == "2"))
    shared_info(list(status = "ok", date = date, promoted = sum(a$promoted), moved = sum(a$moved),
                     data_changed = isTRUE(then_changed), p1_then = num(x$d$p1), p1_now = sum(zone == "1")))
    utils::modifyList(share_state_settings(x, SHARE_SETTING_IDS), state)
  }

  output$wi_shared_note <- renderUI({
    si <- shared_info(); if (is.null(si)) return(NULL)
    when <- if (nzchar(si$date %||% "")) format(as.Date(si$date), " made %b %d, %Y") else ""
    body <- switch(si$status,
      ok = tagList(
        tags$b(sprintf("Opened from a shared scenario%s: ", when)),
        sprintf(paste0("%d section(s) promoted to P1, %d moved to another facility. Change ",
                       "anything you like; your changes are not saved to the link. Use Copy ",
                       "link to this scenario to share your own."), si$promoted, si$moved),
        if (isTRUE(si$data_changed)) tags$p(style = "margin:6px 0 0;", sprintf(
          paste0("The section data has changed since this link was made (%s P1 sections then, ",
                 "%s now), so the numbers can differ from what the sender saw."),
          format(si$p1_then, big.mark = ","), format(si$p1_now, big.mark = ",")))),
      sections_changed = "This link was made before the section list changed, so the scenario can't be restored. The planner is showing the starting point.",
      nodata = "This link could not be restored because the section data did not load. Reload the page to try again.",
      "This link is damaged or incomplete, so the scenario can't be restored. The planner is showing the starting point.")
    tags$div(class = "alert alert-info", role = "status", body)
  })

  # Copy link: pack the scenario, every What-If setting, the staffing table and the map view
  observeEvent(input$wi_share, {
    b <- base(); req(b)
    settings <- lapply(setNames(SHARE_SETTING_IDS, SHARE_SETTING_IDS), function(id) input[[id]])
    ctr <- input$wi_map_center; zm <- input$wi_map_zoom
    view <- if (!is.null(ctr) && !is.null(zm)) list(lat = ctr$lat, lng = ctr$lng, z = zm) else NULL
    st <- scenario_share_state(changes(), b$sections, settings, staff(), view)
    session$sendCustomMessage("wi_share_link", list(token = share_token_encode(st)))
  })

  # Section table with the scenario applied (facility / zone hold the NEW values,
  # facility_from / zone_from the original ones; row order matches base()$sections)
  wi_after <- reactive({
    b <- base(); req(b)
    scenario_apply(b$sections, changes(), b$times)
  })

  # One-way drive minutes for each section at its ORIGINAL and its NEW facility
  wi_dm <- reactive({
    b <- base(); a <- wi_after()
    f <- function(s) section_drive_minutes(s, input$wi_drive_mode %||% "estimate",
                                           input$wi_avg_mph %||% 45, input$wi_circuity %||% 1.3,
                                           input$wi_speed_pct %||% 100)
    list(before = f(b$sections), after = f(a))
  })

  # Section polygons for every facility: loaded once, when the tab is first opened
  geo_raw <- reactiveVal(NULL)
  observe({
    req(base(), identical(input$tabs, "whatif"))
    if (!is.null(isolate(geo_raw()))) return()
    geo <- load_section_geo()
    geo <- geo[!is.na(geo$wkt), , drop = FALSE]
    if (nrow(geo) == 0) return()
    geo_raw(tryCatch(sf::st_as_sf(geo[, c("sectcode", "lat", "lon", "wkt")],
                                  wkt = "wkt", crs = 4326),
                     error = function(e) NULL))
  })

  # Select / deselect sections (the rules live in scenario_toggle(), which is unit tested):
  # selecting promotes a P2 section to P1 and moves it when a destination is picked; selecting
  # it again puts it back; a box undoes sections that are all already in the scenario.
  update_changes <- function(codes, mode = "toggle") {
    b <- base(); req(b)
    r <- scenario_toggle(b$sections, changes(), codes, input$wi_move_to %||% "keep", mode)
    before <- changes()
    changes(r$changes)
    wi_msg(if (r$skipped > 0 && nrow(r$changes) == nrow(before)) sprintf(
      "%d P1 section(s) unchanged: pick another facility under Move sections to move them.",
      r$skipped) else "")
    session$sendCustomMessage("wi_sel_sync", as.list(changes()$sectcode))
  }

  observeEvent(input$wi_map_shape_click, {
    id <- input$wi_map_shape_click$id
    if (!is.null(id)) update_changes(id, "toggle")
  })
  observeEvent(input$wi_toggle, {                       # section-list checkbox (keyboard route)
    x <- input$wi_toggle
    update_changes(x$code, if (isTRUE(x$on)) "set" else "off")
  })
  observeEvent(input$wi_undo, { changes(no_changes); wi_msg("") })
  observe(session$sendCustomMessage("wi_sel_sync", as.list(changes()$sectcode)))

  output$wi_selcount <- renderUI({
    a <- wi_after()
    warn <- if (any(a$time_missing)) sprintf(
      " No drive time for %d moved section(s); they use no travel.", sum(a$time_missing)) else ""
    tags$span(style = "color:#2c5aa0;font-weight:600;", role = "status",
              sprintf("Scenario: %d section(s) promoted to P1, %d moved to another facility. %s%s",
                      sum(a$promoted), sum(a$moved), wi_msg(), warn))
  })

  # ---- Map (click to promote / undo; Shift+drag or the Box select toggle to do many) --
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

    // Touch / pen / mouse: a Box select toggle on the map. While on, one-finger drag
    // draws the rectangle (map panning is off); a tap with no drag still toggles a section.
    var boxMode = false, pStart = null, pRect = null, pId = null, moved = false, boxBtn = null;
    var cont = map.getContainer();
    function setBox(on) {
      boxMode = on;
      if (boxBtn) {
        boxBtn.setAttribute('aria-pressed', on ? 'true' : 'false');
        boxBtn.innerHTML = 'Box select: ' + (on ? 'ON' : 'off');
        boxBtn.style.background = on ? '#2c5aa0' : '#fff';
        boxBtn.style.color = on ? '#fff' : '#1f2933';
      }
      if (on) { map.dragging.disable(); } else { map.dragging.enable(); }
      cont.style.touchAction = on ? 'none' : '';
    }
    var Ctl = L.Control.extend({
      options: {position: 'topright'},
      onAdd: function() {
        var d = L.DomUtil.create('div', 'leaflet-bar');
        var a = L.DomUtil.create('button', '', d);
        a.type = 'button'; a.title = 'Drag a box on the map to select many sections';
        a.setAttribute('aria-pressed', 'false');
        a.style.cssText = 'min-width:44px;min-height:40px;padding:0 10px;border:0;cursor:pointer;' +
          'background:#fff;color:#1f2933;font:600 13px sans-serif;';
        a.innerHTML = 'Box select: off';
        L.DomEvent.disableClickPropagation(d);
        boxBtn = a;
        L.DomEvent.on(a, 'click', function() { setBox(!boxMode); });
        return d;
      }
    });
    map.addControl(new Ctl());
    function ll(e) {
      var r = cont.getBoundingClientRect();
      return map.containerPointToLatLng(L.point(e.clientX - r.left, e.clientY - r.top));
    }
    cont.addEventListener('pointerdown', function(e) {
      if (!boxMode || !e.isPrimary || e.target.closest('.leaflet-control')) { return; }
      pId = e.pointerId; pStart = ll(e); moved = false;
      cont._x0 = e.clientX; cont._y0 = e.clientY;
      try { cont.setPointerCapture(pId); } catch (err) {}
    });
    cont.addEventListener('pointermove', function(e) {
      if (pStart === null || e.pointerId !== pId) { return; }
      if (Math.abs(e.clientX - cont._x0) + Math.abs(e.clientY - cont._y0) > 10) { moved = true; }
      if (!moved) { return; }
      var b = L.latLngBounds(pStart, ll(e));
      if (pRect) { pRect.setBounds(b); }
      else { pRect = L.rectangle(b, {color:'#2c5aa0', weight:1, dashArray:'4', fillOpacity:0.1}).addTo(map); }
    });
    function endBox(e) {
      if (pStart === null || e.pointerId !== pId) { return; }
      var b = L.latLngBounds(pStart, ll(e));
      if (pRect) { map.removeLayer(pRect); pRect = null; }
      pStart = null; pId = null;
      if (!moved) { return; }
      var sw = b.getSouthWest(), ne = b.getNorthEast();
      Shiny.setInputValue('wi_box', {s:sw.lat, w:sw.lng, n:ne.lat, e:ne.lng, nonce:Math.random()}, {priority:'event'});
      setBox(false);   // one box per tap, so page scrolling returns right away
    }
    cont.addEventListener('pointerup', endBox);
    cont.addEventListener('pointercancel', function() {
      if (pRect) { map.removeLayer(pRect); pRect = null; }
      pStart = null; pId = null;
    });
  }"

  # Facility colors (qualitative); the same facility always gets the same color
  fac_colors <- function(facs) {
    pal <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666")
    setNames(pal[(seq_along(facs) - 1) %% length(pal) + 1], facs)
  }
  map_facs <- function() sort(unique(isolate(base())$sections$facility))
  sec_color <- function(facility) {
    f <- map_facs(); unname(fac_colors(f)[match(facility, f)])
  }
  sec_opacity <- function(zone) ifelse(as.character(zone) == "1", 0.75, 0.15)
  # Hover label of a section (rows of scenario_apply() output)
  sec_label <- function(g) {
    changed <- g$moved | g$promoted
    where <- ifelse(changed,
                    sprintf("%s P%s &rarr; %s P%s", g$facility_from, g$zone_from, g$facility, g$zone),
                    sprintf("%s P%s", g$facility, g$zone))
    sprintf(
      paste0("<b>%s</b> (FOS %s)<br/>%s<br/>Ground %d sites / %.1f ac &middot; ",
             "Air %d sites / %.1f ac &middot; WetCB %d%s"),
      g$sectcode, g$fos_name, where, g$ground_sites, g$ground_acres, g$air_sites, g$air_acres,
      g$wet_cb,
      ifelse(is.na(g$drive_min_routed), "",
             sprintf("<br/>~%.0f min drive from %s (OSM)", g$drive_min_routed, g$facility)))
  }

  output$wi_map <- leaflet::renderLeaflet({
    g <- geo_raw()
    m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
      leaflet::addTiles()
    if (is.null(g)) return(m)
    facs <- map_facs()
    bb <- as.numeric(sf::st_bbox(g))
    m %>%
      leaflet::fitBounds(bb[1], bb[2], bb[3], bb[4]) %>%
      leaflet::addLegend("bottomright", colors = unname(fac_colors(facs)), labels = facs,
                         title = "Facility", opacity = 0.9) %>%
      htmlwidgets::onRender(.box_js)
  })

  # leafletProxy calls sent before the browser has built the map are dropped, so the
  # layers wait for the map's first bounds report (leaflet sends it once it exists).
  map_ready <- reactiveVal(FALSE)
  observeEvent(input$wi_map_bounds, map_ready(TRUE), once = TRUE)

  # Every section is drawn ONCE, in its original facility color (solid = P1, light = P2).
  # A scenario change then restyles only the sections that changed (wi_restyle, in the page
  # script) instead of redrawing all of them, so selecting is instant and does not flash.
  # A moved section takes its new facility's color; a promoted one turns solid.
  styled <- new.env(parent = emptyenv())
  styled$key <- character(0)           # sectcode -> "facility|zone" for sections now restyled

  restyle_sync <- function() {
    b <- isolate(base()); if (is.null(b)) return(invisible())
    ch <- isolate(changes())
    codes <- unique(c(names(styled$key), ch$sectcode))
    if (length(codes) == 0) return(invisible())
    sec <- b$sections[as.character(b$sections$sectcode) %in% codes, , drop = FALSE]
    a <- scenario_apply(sec, ch[ch$sectcode %in% codes, , drop = FALSE], b$times,
                        facilities = unique(b$sections$facility))
    code <- as.character(a$sectcode)
    cur <- paste(a$facility, a$zone, sep = "|")
    orig <- paste(a$facility_from, a$zone_from, sep = "|")
    prev <- ifelse(code %in% names(styled$key), styled$key[code], orig)
    send <- which(cur != prev)
    styled$key <- setNames(cur[cur != orig], code[cur != orig])
    if (length(send) > 0) {
      session$sendCustomMessage("wi_restyle", list(
        id = as.list(code[send]), col = as.list(sec_color(a$facility[send])),
        op = as.list(sec_opacity(a$zone[send])), lbl = as.list(sec_label(a[send, , drop = FALSE]))))
    }
    invisible()
  }

  observe({
    req(map_ready(), !is.null(geo_raw()))
    g <- geo_raw(); b <- base(); req(b)
    a <- scenario_apply(b$sections, NULL, b$times)
    d <- merge(g[, "sectcode"],
               a[, c("sectcode", "facility", "zone", "facility_from", "zone_from", "moved",
                     "promoted", "fos_name", "ground_sites", "ground_acres", "air_sites",
                     "air_acres", "wet_cb", "drive_min_routed")],
               by = "sectcode")
    cols <- sec_color(d$facility)
    leaflet::leafletProxy("wi_map") %>%
      leaflet::clearGroup("base") %>%
      leaflet::addPolygons(
        data = d, layerId = d$sectcode, group = "base",
        weight = 1, color = cols, opacity = 0.9,
        fillColor = cols, fillOpacity = sec_opacity(d$zone),
        label = lapply(sec_label(d), htmltools::HTML),
        highlightOptions = leaflet::highlightOptions(weight = 3))
    styled$key <- character(0)
    # proxy calls go out after this flush; send the scenario's styles right behind them
    session$onFlushed(restyle_sync, once = TRUE)
    rv <- isolate(restore_view())               # a shared link's map view replaces the fit-to-data view
    if (!is.null(rv)) {
      leaflet::leafletProxy("wi_map") %>% leaflet::setView(rv$lng, rv$lat, rv$z)
      restore_view(NULL)
    }
  })

  # Highest priority, so the map reacts before the tables below are recomputed
  observeEvent(changes(), restyle_sync(), ignoreInit = TRUE, priority = 1000)

  observeEvent(input$wi_box, {
    bx <- input$wi_box; g <- geo_raw(); req(!is.null(g))
    inb <- as.character(g$sectcode[
      g$lat >= bx$s & g$lat <= bx$n & g$lon >= bx$w & g$lon <= bx$e])
    update_changes(inb[!is.na(inb)], "toggle")
  })

  # Section list (keyboard route): a ticked box = the section is in the scenario. It lists the
  # sections as they are today and is not rebuilt when the scenario changes (the ticks follow it).
  output$wi_sections <- DT::renderDT({
    b <- base(); req(b)
    s <- b$sections; req(nrow(s) > 0)
    s <- s[order(s$facility, s$zone, s$sectcode), ]
    s$Sel <- sprintf(
      '<input type="checkbox" class="wi-sel" data-sect="%s" aria-label="Promote section %s">',
      as.character(s$sectcode), as.character(s$sectcode))
    s$Priority <- paste0("P", s$zone)
    s$drive_min <- section_drive_minutes(s, input$wi_drive_mode %||% "estimate",
                                         input$wi_avg_mph %||% 45, input$wi_circuity %||% 1.3,
                                         input$wi_speed_pct %||% 100)
    disp <- s[, c("Sel", "sectcode", "facility", "Priority", "fos_name", "ground_sites",
                  "ground_acres", "air_sites", "air_acres", "prehatch_sites", "prehatch_acres",
                  "wet_cb", "drive_min")]
    for (col in c("ground_acres", "air_acres", "prehatch_acres")) disp[[col]] <- round(disp[[col]], 1)
    names(disp) <- c("Promote", "Section", "Facility", "Priority", "FOS", "Ground sites",
                     "Ground acres", "Air sites", "Air acres", "PH sites", "PH acres",
                     "WetCB", "Drive min")
    capacity_dt(disp, left = 2, rownames = FALSE, filter = "top", selection = "none",
                escape = -1,
                callback = DT::JS("table.on('change', 'input.wi-sel', function() {",
                  "Shiny.setInputValue('wi_toggle', {code: this.getAttribute('data-sect'),",
                  "on: this.checked, nonce: Math.random()}, {priority: 'event'}); });"),
                options = list(pageLength = 10,
                               drawCallback = DT::JS("function() { if (window.wiApplySel) window.wiApplySel(); }")))
  })

  # ---- What changed: the numbers the scenario adds to P1 ------------------
  fmt_n <- function(x) vapply(x, function(z) format(round(z, 1), big.mark = ",", trim = TRUE), "")
  fmt_chg <- function(chg) ifelse(is.na(chg), "n/a",
                                  ifelse(chg > 0, paste0("+", fmt_n(chg)),
                                         ifelse(chg < 0, paste0("-", fmt_n(abs(chg))), "0")))
  fmt_min <- function(x) ifelse(is.na(x), "-", fmt_n(x))
  # Drive times are MEDIAN one-way minutes: to the group's current P1 sections, and to the
  # sections the scenario adds to its P1. Travel ratio = added / current (above 1 = the added
  # sections are farther away, so more driving), the number Estimate A turns into a capacity factor.
  chg_table <- function(cmp, first) {
    mets <- intersect(SCENARIO_METRICS, capacity_visible_metrics(input$wi_measure %||% "both"))
    out <- data.frame(cmp$label, stringsAsFactors = FALSE)
    names(out) <- first
    for (k in mets) out[[paste("+", capacity_col_label(k))]] <- fmt_chg(cmp[[paste0(k, "_chg")]])
    if ("added_min" %in% names(cmp)) {
      out[["Median drive min to current P1"]] <- fmt_min(cmp$p1_min_before)
      out[["Median drive min to added sections"]] <- fmt_min(cmp$added_min)
      if (isTRUE(input$wi_travel %||% TRUE)) {
        ratio <- cmp$added_min / cmp$p1_min_before
        out[["Travel ratio (added / current)"]] <- ifelse(
          is.na(ratio) | !is.finite(ratio), "-", sprintf("%.2f", ratio))
      }
    }
    out
  }
  # Clicking a facility row (here or in Estimate A) picks it for the FOS breakdown
  pick_facility <- function(keys, rows) {
    rows <- rows[!is.na(rows) & rows >= 1 & rows <= length(keys)]
    if (length(rows) == 1 && !identical(keys[rows], "DISTRICT")) {
      updateSelectInput(session, "wi_drill", selected = keys[rows])
    }
  }
  sel_opts <- function(keys) {
    i <- match(input$wi_drill %||% "", keys, nomatch = 0L)[1]
    list(mode = "single", target = "row", selected = if (i > 0) i else NULL)
  }

  # A table that updates IN PLACE when only its numbers change (no re-initialisation, so no
  # flash). It is rebuilt only when its columns change (the Show: choice, travel on / off, or
  # the empty "Note" placeholder). `select` (optional) returns the row to highlight.
  live_dt <- function(id, data_fun, build, select = NULL) {
    cols <- reactiveVal(NULL)
    built <- new.env(parent = emptyenv()); built$cols <- NULL
    observe({
      # nothing to show (e.g. no facility picked yet) -> columns NULL -> the table is cleared
      nm <- tryCatch(names(data_fun()), error = function(e) {
        if (!inherits(e, "shiny.silent.error")) message("[capacity_planner] ", id, ": ", conditionMessage(e))
        NULL
      })
      if (!identical(nm, isolate(cols()))) cols(nm)
    }, priority = 10)
    output[[id]] <- DT::renderDT({
      req(cols())
      d <- isolate(data_fun())
      built$cols <- names(d)
      build(d)
    })
    proxy <- DT::dataTableProxy(id)
    observeEvent(data_fun(), {
      d <- data_fun()
      if (identical(names(d), built$cols)) {
        DT::replaceData(proxy, d, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
        if (!is.null(select)) DT::selectRows(proxy, select(d))
      }
    }, ignoreInit = TRUE)
  }

  wi_cmp <- reactive({
    a <- wi_after(); dm <- wi_dm()
    scenario_compare(a, "1", SCENARIO_METRICS, dm_before = dm$before, dm_after = dm$after)
  })

  wi_workload_df <- reactive({
    cmp <- wi_cmp(); req(nrow(cmp) > 0)
    chg_table(cmp, "Facility")
  })
  live_dt("wi_workload", wi_workload_df,
          build = function(d) capacity_dt(d, left = 1, rownames = FALSE,
                                          selection = isolate(sel_opts(wi_cmp()$key)),
                                          options = list(dom = "t", ordering = FALSE, pageLength = 20)),
          select = function(d) { i <- match(input$wi_drill %||% "", isolate(wi_cmp()$key), nomatch = 0L)[1]
                                 if (i > 0) i else NULL })
  observeEvent(input$wi_workload_rows_selected, {
    pick_facility(wi_cmp()$key, input$wi_workload_rows_selected)
  })

  output$wi_drill_title <- renderUI({
    f <- input$wi_drill %||% ""
    if (!nzchar(f)) return(tags$p(class = "metric-note", "Pick a facility to see it by FOS area."))
    tags$h5(sprintf("%s by FOS area (sections moved in from another facility are unassigned)", f))
  })

  wi_workload_fos_df <- reactive({
    f <- input$wi_drill %||% ""; req(nzchar(f))
    a <- wi_after(); dm <- wi_dm()
    cmp <- scenario_compare(a, "1", SCENARIO_METRICS, facility = f,
                            dm_before = dm$before, dm_after = dm$after)
    req(nrow(cmp) > 0)
    chg_table(cmp, "FOS area")
  })
  live_dt("wi_workload_fos", wi_workload_fos_df,
          build = function(d) capacity_dt(d, left = 1, rownames = FALSE, selection = "none",
                                          options = list(dom = "t", ordering = FALSE, pageLength = 20)))

  # Load per tech and per crew (P1), now -> after
  wi_pertech_base <- reactive({
    a <- wi_after(); st <- staff()
    now <- a; now$facility <- a$facility_from; now$zone <- a$zone_from
    list(b0 = staffing_baseline(scenario_cap(now), st),
         b1 = staffing_baseline(scenario_cap(a), st))
  })
  wi_pertech_df <- reactive({
    pb <- wi_pertech_base(); b0 <- pb$b0; b1 <- pb$b1
    req(nrow(b1) > 0)
    arrow <- function(x0, x1) ifelse(is.na(x0) & is.na(x1), "n/a",
                                     sprintf("%s → %s", ifelse(is.na(x0), "n/a", fmt_n(x0)),
                                             ifelse(is.na(x1), "n/a", fmt_n(x1))))
    i0 <- match(b1$facility, b0$facility)
    data.frame(
      Facility = b1$facility,
      `Ground techs` = fmt_n(b1$ground_techs),
      `Ground sites/tech` = arrow(b0$ground_sites_per_tech[i0], b1$ground_sites_per_tech),
      `Ground acres/tech` = arrow(b0$ground_acres_per_tech[i0], b1$ground_acres_per_tech),
      `Air sites/tech` = arrow(b0$air_sites_per_tech[i0], b1$air_sites_per_tech),
      `Air acres/tech` = arrow(b0$air_acres_per_tech[i0], b1$air_acres_per_tech),
      `Prehatch acres/tech` = arrow(b0$ph_acres_per_tech[i0], b1$ph_acres_per_tech),
      `Ground acres/crew` = arrow(b0$ground_acres_per_crew[i0], b1$ground_acres_per_crew),
      `Air acres/crew` = arrow(b0$air_acres_per_crew[i0], b1$air_acres_per_crew),
      `Prehatch acres/crew` = arrow(b0$ph_acres_per_crew[i0], b1$ph_acres_per_crew),
      check.names = FALSE, stringsAsFactors = FALSE)
  })
  live_dt("wi_pertech", wi_pertech_df,
          build = function(d) capacity_dt(d, left = 1, rownames = FALSE, selection = "none",
                                          options = list(dom = "t", ordering = FALSE, pageLength = 20)))

  # ---- What-If staffing controls (shared with the Staffing tab) ----------
  # Facility switch or a Staffing-tab edit -> refresh the inputs; an input edit
  # -> write back to staff(). Only writes when the value differs, so no loop.
  observe({
    s <- staff(); fac <- input$wi_facility
    req(fac, fac %in% s$facility)
    row <- s[s$facility == fac, ]
    session$sendCustomMessage("wi_staff_set", list(
      fac = fac, techs = round(row$techs_per_crew, 2), off = row$drone_offload_pct,
      air = row$drone_air_offload_pct))
  })

  # The three inputs are shared by whichever facility is selected, so a value pushed for one
  # facility can arrive back after the selection moved to another; reading input$wi_techs_per_crew
  # here wrote that stale value into the new facility and the two values then bounced back and
  # forth forever (seen when a link restored a non-first facility). Values are now pushed with
  # `wi_staff_set` (tagged with their facility, never echoed as edits) and the page sends
  # `wi_staff_edit` only for a genuine edit made while the boxes belong to the selected facility;
  # staff_apply_edit() writes to THAT facility only.
  WI_STAFF_EDIT <- list(
    wi_techs_per_crew    = list(col = "techs_per_crew",        lo = 0, hi = Inf, tol = 0.005),
    wi_drone_offload     = list(col = "drone_offload_pct",     lo = 0, hi = 100, tol = 0),
    wi_drone_air_offload = list(col = "drone_air_offload_pct", lo = 0, hi = 100, tol = 0))
  observeEvent(input$wi_staff_edit, {
    x <- input$wi_staff_edit
    sp <- WI_STAFF_EDIT[[as.character(x$id)[1]]]
    if (is.null(sp)) return()
    s <- isolate(staff())
    new <- staff_apply_edit(s, as.character(x$fac)[1], sp$col,
                            suppressWarnings(as.numeric(x$value)), sp$lo, sp$hi, sp$tol)
    if (!identical(new, s)) staff(new)
  })

  output$wi_baseline <- renderUI({
    bl <- staffing_baseline(cap_live(), staff()); req(nrow(bl) > 0, input$wi_facility)
    r <- bl[bl$facility == input$wi_facility, ]; req(nrow(r) == 1)
    f <- function(x) if (is.na(x)) "n/a" else format(x, big.mark = ",")
    tags$div(class = "metric-note", style = "margin-top:25px;",
      HTML(sprintf(
        paste0("<b>%s baseline (P1):</b> %s crews &middot; %s ground techs &middot; %s drone techs",
               "<br/>Ground: %s sites/tech &middot; %s acres/tech &middot; %s acres/crew",
               "<br/>Air: %s sites/tech &middot; %s acres/tech &middot; %s acres/crew",
               "<br/>Prehatch: %s acres/tech &middot; %s acres/crew"),
        r$facility, f(r$crews), f(r$ground_techs), f(r$drone_techs),
        f(r$ground_sites_per_tech), f(r$ground_acres_per_tech), f(r$ground_acres_per_crew),
        f(r$air_sites_per_tech), f(r$air_acres_per_tech), f(r$air_acres_per_crew),
        f(r$ph_acres_per_tech), f(r$ph_acres_per_crew))))
  })

  # ---- Estimate A: techs from load per tech (live) -----------------------
  wi_staff_result <- reactive({
    a <- wi_after(); dm <- wi_dm()
    per_tech <- if (is.null(input$wi_per_tech) || is.na(input$wi_per_tech)) NULL else input$wi_per_tech
    travel_on <- isTRUE(input$wi_travel %||% TRUE)
    scenario_staffing(a, staff(), metric = input$wi_metric %||% "ground_sites", per_tech = per_tech,
                      dm_before = if (travel_on) dm$before else NULL,
                      dm_after = if (travel_on) dm$after else NULL,
                      travel_adjust = travel_on, travel_share = input$wi_travel_share %||% 20)
  })

  output$wi_a_summary <- renderUI({
    r <- wi_staff_result(); t <- r$totals
    req(!is.null(t$metric))
    metric_label <- names(METRIC_CHOICES)[METRIC_CHOICES == t$metric]
    tags$div(
      tags$p(HTML(sprintf("Driver: <b>%s</b> &middot; baseline <b>%.1f</b> per tech &middot; travel time %s",
                          metric_label, t$per_tech, if (isTRUE(t$travel_adjusted)) "on" else "off"))),
      tags$h3(style = "color:#2c5aa0;", sprintf("Net techs change: %+d", as.integer(t$techs_change)))
    )
  })

  # Estimate A columns: the load added / removed, drive times and capacity factor are in
  # What changed, so only the techs are shown here
  a_cols <- function(by, first) {
    by$n_techs <- round(by$n_techs, 1)
    out <- data.frame(by$.label, by$n_techs, by$needed_now, by$needed_new,
                      fmt_chg(by$techs_change), by$gap_now, stringsAsFactors = FALSE)
    names(out) <- c(first, "Techs", "Need now", "Need after", "Techs change", "Gap now")
    out
  }

  # By facility: the sum of its FOS areas (one FOS area = one crew)
  wi_a_df <- reactive({
    r <- wi_staff_result(); by <- r$by_facility
    req(nrow(by) > 0)
    by <- by[by$added_load != 0 | by$removed_load != 0, , drop = FALSE]   # may be 0 rows
    by$.label <- by$facility
    a_cols(by, "Facility")
  })
  live_dt("wi_a_table", wi_a_df,
          build = function(d) {
            capacity_dt(d, left = 1, rownames = FALSE, selection = isolate(sel_opts(d$Facility)),
                        options = list(dom = "t", ordering = FALSE, pageLength = 20,
                                       language = list(emptyTable =
                                         "Click sections on the map to see the staffing impact")))
          },
          select = function(d) { i <- match(input$wi_drill %||% "", d$Facility, nomatch = 0L)[1]
                                 if (i > 0) i else NULL })
  observeEvent(input$wi_a_table_rows_selected, {
    by <- wi_staff_result()$by_facility
    by <- by[by$added_load != 0 | by$removed_load != 0, , drop = FALSE]
    pick_facility(by$facility, input$wi_a_table_rows_selected)
  })

  output$wi_a_drill_title <- renderUI({
    f <- input$wi_drill %||% ""
    if (!nzchar(f)) return(tags$p(class = "metric-note", "Pick a facility to see its FOS areas."))
    tags$h5(sprintf("%s by FOS area (sections moved in from another facility are unassigned)", f))
  })

  wi_a_fos_df <- reactive({
    f <- input$wi_drill %||% ""; req(nzchar(f))
    r <- wi_staff_result(); by <- r$by_fos
    by <- by[by$facility == f, , drop = FALSE]
    req(nrow(by) > 0)
    by$.label <- by$fos_name
    a_cols(by, "FOS area")
  })
  live_dt("wi_a_fos", wi_a_fos_df,
          build = function(d) capacity_dt(d, left = 1, rownames = FALSE, selection = "none",
                                          options = list(dom = "t", ordering = FALSE, pageLength = 20)))

  # ---- Estimate B: crews (2001 study method, live) ----------------------
  jps_all <- reactive({
    yr <- suppressWarnings(as.integer(input$wi_rate_year)); req(!is.na(yr))
    load_jobs_per_site(yr)
  })

  # The facility shown in the rates table (the staffing facility, or the district)
  wi_scope <- reactive({
    if (isTRUE(input$wi_rate_scope == "FAC")) input$wi_facility else "DISTRICT"
  })

  wi_rates_df <- reactive({
    jps <- jps_all(); req(nrow(jps) > 0)
    r <- jps[jps$facility == wi_scope() & jps$zone == "1", ]
    if (nrow(r) == 0) r <- jps[jps$facility == "DISTRICT" & jps$zone == "1", ]
    r
  })

  # Job rates used by the estimate: district-wide, or each facility's own
  wi_rates_list <- reactive({
    jps <- jps_all(); req(nrow(jps) > 0)
    p1 <- jps[jps$zone == "1", ]
    pick <- function(f) { r <- p1[p1$facility == f, ]; setNames(r$rate, r$job_type) }
    out <- list(DISTRICT = pick("DISTRICT"))
    if (isTRUE(input$wi_rate_scope == "FAC")) {
      for (f in setdiff(unique(p1$facility), "DISTRICT")) out[[f]] <- pick(f)
    }
    out
  })

  wi_crew_result <- reactive({
    a <- wi_after(); dm <- wi_dm()
    hpj <- c(air_trt = input$wi_h_air_trt, air_insp = input$wi_h_air_insp,
             gnd_insp_trt = input$wi_h_gnd_insp_trt, gnd_trt = input$wi_h_gnd_trt,
             gnd_insp = input$wi_h_gnd_insp)
    travel_on <- isTRUE(input$wi_travel %||% TRUE)
    scenario_crews(a, wi_rates_list(), hpj,
                   input$wi_crew_hours %||% STUDY_CREW_HOURS_SEASON,
                   trips_per_section = if (travel_on) input$wi_crew_trips %||% DEFAULT_TRIPS_PER_SECTION else 0,
                   avg_mph = input$wi_avg_mph %||% 45, circuity = input$wi_circuity %||% 1.3,
                   dm_before = if (travel_on) dm$before else NULL,
                   dm_after = if (travel_on) dm$after else NULL)
  })

  output$wi_crew_summary <- renderUI({
    r <- wi_crew_result(); t <- r$totals
    tags$div(
      tags$p(sprintf(
        "%s section(s) enter P1 and %s leave it: net %s ground + %s air sites, %s seasonal jobs.",
        format(t$n_added, big.mark = ","), format(t$n_removed, big.mark = ","),
        format(t$ground_sites, big.mark = ","), format(t$air_sites, big.mark = ","),
        format(t$jobs, big.mark = ","))),
      tags$p(HTML(sprintf(
        "<span style='font-family:Consolas,monospace;'>%s on-site hrs + %s travel hrs = %s net hrs &divide; %s hrs/crew = <b>%.2f</b></span>",
        format(t$onsite_hours, big.mark = ","), format(t$travel_hours, big.mark = ","),
        format(t$hours, big.mark = ","), format(t$hrs_per_crew, big.mark = ","),
        t$crews))),
      tags$h3(style = "color:#2c5aa0;", sprintf("Net crews change: %+.2f", t$crews)),
      tags$p(class = "metric-note",
             "Crews to service the sections entering P1 at P1 intensity, minus the crews the ",
             "sections leaving P1 free up (a crew = 1 FOS + its inspectors).")
    )
  })

  wi_crew_job_df <- reactive({
    r <- wi_crew_result(); bj <- r$by_job; t <- r$totals
    req(nrow(bj) > 0)
    travel_row <- data.frame(
      Job = "Travel (drive time)", Sites = NA, `Jobs/site` = NA,
      Jobs = NA, `Hrs/job` = NA, Hours = t$travel_hours,
      check.names = FALSE)
    total_row <- data.frame(
      Job = "TOTAL (incl. travel)", Sites = NA, `Jobs/site` = NA,
      Jobs = sum(bj$Jobs, na.rm = TRUE), `Hrs/job` = NA,
      Hours = t$hours,
      check.names = FALSE)
    rbind(bj, travel_row, total_row)
  })
  live_dt("wi_crew_by_job", wi_crew_job_df,
          build = function(d) capacity_dt(d, rownames = FALSE, options = list(dom = "t")))

  wi_crew_fac_df <- reactive({
    r <- wi_crew_result(); f <- r$by_facility
    labs <- c("Facility", "Sections in", "Sections out", "Hours added", "Hours removed",
              "Net hours", "Net crews")
    if (nrow(f) == 0) {                                   # same columns, no rows
      f <- data.frame(character(0), integer(0), integer(0), numeric(0), numeric(0), numeric(0),
                      numeric(0), stringsAsFactors = FALSE)
    }
    names(f) <- labs
    f
  })
  live_dt("wi_crew_fac", wi_crew_fac_df,
          build = function(d) {
            capacity_dt(d, left = 1, rownames = FALSE, selection = "none",
                        options = list(dom = "t", ordering = FALSE,
                                       language = list(emptyTable =
                                         "Build a scenario above to see crews by facility")))
          })

  output$wi_rates <- DT::renderDT({
    r <- wi_rates_df(); req(nrow(r) > 0)
    r$Job <- JOB_TYPE_LABELS[r$job_type]
    disp <- r[, c("Job", "jobs", "distinct_sites", "rate")]
    names(disp) <- c("Job", "Jobs (season)", "Distinct sites", "Jobs/site")
    capacity_dt(disp, rownames = FALSE,
      caption = sprintf("%s P1 sites, season %s", wi_scope(), unique(r$year)),
      options = list(dom = "t"))
  })

  # ---- CSV downloads ------------------------------------------------------
  # The Staffing and Capacity downloads are the tables as shown; the What-If ones are plain
  # numbers (see the scenario_export_* functions), not the formatted text on the page.
  csv_out <- function(id, prefix, data_fun) {
    output[[id]] <- downloadHandler(
      filename = function() paste0("capacity_", prefix, "_", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(data_fun(), file, row.names = FALSE, na = "")
    )
  }
  csv_out("dl_avg", "averages", function() {
    rbind(cbind(priority = "P1", avg_data("1")), cbind(priority = "P2", avg_data("2")))
  })
  csv_out("dl_drive", "drive_time_summary", drive_summary_df)
  csv_out("dl_staff", "staffing_assumptions", staff_edit_df)
  csv_out("dl_baseline", "baseline_per_tech", staff_baseline_df)
  csv_out("dl_techs", "techs_per_fos_area", staff_table_df)
  csv_out("dl_wi_sections", "whatif_changed_sections", function() {
    dm <- wi_dm(); scenario_export_changes(wi_after(), dm$before, dm$after)
  })
  csv_out("dl_wi_changes", "whatif_by_facility", function() {
    scenario_export_compare(wi_cmp(), "facility")
  })
  csv_out("dl_wi_fos", "whatif_by_fos_area", function() {
    dm <- wi_dm(); scenario_export_fos(wi_after(), dm$before, dm$after)
  })
  csv_out("dl_wi_pertech", "whatif_load_per_tech_and_crew", function() {
    pb <- wi_pertech_base(); scenario_export_load(pb$b0, pb$b1)
  })
  csv_out("dl_wi_a", "whatif_estimate_a_techs", function() scenario_export_staffing(wi_staff_result()))
  csv_out("dl_wi_b_job", "whatif_estimate_b_by_job_type", function() {
    d <- wi_crew_job_df()
    names(d) <- c("job", "sites", "jobs_per_site", "jobs", "hours_per_job", "hours")
    d
  })
  csv_out("dl_wi_b_fac", "whatif_estimate_b_by_facility", function() {
    d <- wi_crew_fac_df()
    names(d) <- c("facility", "sections_in", "sections_out", "hours_added", "hours_removed",
                  "net_hours", "net_crews")
    d
  })
}

shinyApp(ui, server)
