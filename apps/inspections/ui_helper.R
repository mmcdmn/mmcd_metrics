# Inspections App - UI Helper Functions
# =============================================================================
# Layout for the prehatch coverage (51% goal) app. Only defines functions, so it
# is safe to source on its own (tests do).
#
# The words "Sampled" / "Missing" come from COVERAGE_LABEL_COUNTING /
# COVERAGE_LABEL_NOT_COUNTING (coverage_functions.R), so they can be renamed in
# one place.

library(shiny)
library(DT)
library(plotly)

# Styles for the progress hero + progress bar
coverage_css <- function() {
  tags$head(tags$style(HTML("
    .cov-hero { border-radius: 8px; padding: 18px 24px; margin-bottom: 16px; }
    .cov-hero .cov-pct { font-size: 56px; font-weight: 700; line-height: 1.05; }
    .cov-hero .cov-sub { font-size: 20px; margin-top: 4px; }
    .cov-hero .cov-need { font-size: 22px; font-weight: 600; margin-top: 10px; }
    .cov-bar { position: relative; height: 22px; border-radius: 11px; margin: 16px 0 6px 0;
               background: rgba(0,0,0,0.25); }
    .cov-bar .cov-fill { height: 100%; border-radius: 11px; }
    .cov-bar .cov-goal { position: absolute; top: -7px; bottom: -7px; width: 4px;
                         background: #111; border-radius: 2px; }
    .cov-goal-label { position: relative; height: 22px; font-size: 15px; }
    .cov-goal-label span { position: absolute; transform: translateX(-50%); white-space: nowrap; }
    .cov-note { color: #555; font-size: 15px; margin: 4px 0 14px 0; }
    .cov-legend td, .cov-legend th { padding: 6px 12px; vertical-align: top; }
    .cov-swatch { display: inline-block; width: 14px; height: 14px; border-radius: 3px;
                  margin-right: 6px; vertical-align: -2px; }
  ")))
}

# Plain-language explanation tab
create_rules_panel <- function() {
  sampled <- COVERAGE_LABEL_COUNTING
  missing <- COVERAGE_LABEL_NOT_COUNTING
  id_pending <- COVERAGE_LABEL_ID_PENDING
  goal <- coverage_goal_pct()
  status_label <- function(key) COVERAGE_STATUS_LABELS[[key]]

  tagList(
    h3("The goal"),
    p("At least ", strong(paste0(goal, "% of ground PREHATCH sites")), " should be ", strong(sampled),
      ". A site is ", strong(sampled), " if either of these is true:"),
    tags$ol(
      tags$li(strong("Red bug in the last 5 years: "),
              "a sample taken at the site came back red (redblue = R) on an inspection in the last 5 years."),
      tags$li(strong("Sampled this year: "),
              "any inspection this calendar year produced a sample number, whatever the result ",
              "(red, blue, no bugs, or a result not yet entered).")
    ),
    p("Every other site is ", strong(missing), ". The Progress tab shows the percentage that are ", sampled,
      " and how many more sites it takes to reach the goal."),
    h3("Why the number doesn't always move when you inspect"),
    tags$ul(
      tags$li(strong("Inspecting is not the same as sampling. "),
              "An inspection that finds the site dry, or finds no larvae to dip, produces no sample. ",
              "Those sites show as “", status_label("inspected_no_sample"), "” and stay ", missing, "."),
      tags$li(strong("Red bugs age out. "),
              "A red bug stops counting 5 years after the inspection that found it. ",
              "The Progress tab shows how many sites drop to ", missing, " each month."),
      tags$li(strong("A this-year sample is temporary. "),
              "A site that is ", sampled, " only because it was sampled this year (not red) drops to ",
              missing, " on Jan 1."),
      tags$li(strong(id_pending, ". "),
              "A sample was taken this year and its result is not entered yet. The site counts as ", sampled,
              " in the meantime. Once the result is entered it is simply red or blue and is no longer ",
              id_pending, ": red keeps the site ", sampled, " for 5 years; blue (or no bugs) keeps it ", sampled,
              " until Jan 1, then it is ", missing, " unless another sample is taken."),
      tags$li(strong("Pupae bioassays count only if they found something. "),
              "A pupae bioassay (action P) counts as a sample only when it has a sample number ",
              "AND a number per dip greater than 0. A bioassay with no number per dip is ignored. ",
              "Like any other sample, it shows as ", id_pending, " until its result is entered.")
    ),
    h3("What each status means"),
    tags$table(class = "cov-legend",
      tags$thead(tags$tr(tags$th("Status"), tags$th("Group"), tags$th("What to do"))),
      tags$tbody(
        tags$tr(tags$td(status_label("red_5yr")), tags$td(sampled),
                tags$td(paste0("Nothing needed until the “", sampled, " through” date."))),
        tags$tr(tags$td(status_label("sampled_this_year")), tags$td(paste0(sampled, ", until Dec 31")),
                tags$td(paste0("Needs a red bug (or a fresh sample next year) to stay ", sampled,
                               "."))),
        tags$tr(tags$td(status_label("red_expired")), tags$td(missing),
                tags$td("Sample it when wet; a red bug brings it back for another 5 years.")),
        tags$tr(tags$td(status_label("no_red_sampled")), tags$td(missing),
                tags$td("Sampled in the last 5 yrs but never red, and not sampled this year. Sample again when wet.")),
        tags$tr(tags$td(status_label("inspected_no_sample")), tags$td(missing),
                tags$td("Visited but nothing to sample. Check 'Likely to yield' (how often it is wet).")),
        tags$tr(tags$td(status_label("not_inspected")), tags$td(missing),
                tags$td("No visit on record in the last 5 years."))
      )
    ),
    h3("Which sites are included"),
    p("Ground sites (not air) that are still active on the 'as of' date and have a ",
      "prehatch type of PREHATCH, PELLET, BRIQUET or PRE1ONLY. BTI-only sites are not prehatch sites. ",
      "Drone sites are included. The same numbers drive the Prehatch Red Bugs tile on the Overview.")
  )
}

# Create the main UI
create_main_ui <- function() {
  sampled <- COVERAGE_LABEL_COUNTING
  missing <- COVERAGE_LABEL_NOT_COUNTING
  id_pending <- COVERAGE_LABEL_ID_PENDING

  accessible_page(
    get_universal_text_css(),
    coverage_css(),

    titlePanel("Prehatch Coverage"),

    sidebarLayout(
      sidebarPanel(
        actionButton("refresh", "Refresh data", icon = icon("refresh"),
          class = "btn-success", style = "width: 100%;"),
        selectInput("facility", "Facility:",
          choices = get_facility_choices(), selected = "all"),

        selectizeInput("fosarea", "FOS Area:",
          choices = get_foreman_choices(include_all = TRUE),
          selected = "all", multiple = TRUE),

        radioButtons("zone", "Zone:",
          choices = c("P1 Only" = "1", "P2 Only" = "2", "P1 and P2" = "1,2"),
          selected = "1,2"),

        selectizeInput("priority", "Priority:",
          choices = get_priority_choices(include_all = TRUE),
          selected = "all", multiple = TRUE),

        radioButtons("drone_filter", "Drone Sites:",
          choices = c("Include Drone Sites" = "include_drone",
                      "Drone Sites Only" = "drone_only",
                      "Non-Drone Sites Only" = "no_drone"),
          selected = "include_drone"),

        dateInput("as_of", "as of:"),

        hr(),
        selectInput("color_theme", "Color Theme:",
          choices = get_theme_choices(), selected = "MMCD")
      ),

      accessible_main_panel(
        tabsetPanel(
          id = "tabs",

          tabPanel("Progress", value = "progress",
            br(),
            uiOutput("hero"),
            fluidRow(
              column(4, uiOutput("box_sampled")),
              column(4, uiOutput("box_id_pending")),
              column(4, uiOutput("box_not_counting"))
            ),
            p(class = "cov-note",
              id_pending, " is a sample taken this year whose result is not entered yet. Those sites are ",
              sampled, " for now, so ", sampled, " + ", id_pending, " is the total above."),
            h3("Prehatch Sites that haven't had a Red Bug in the Last 5 Years (aka missing)"),
            p(class = "cov-note", "Click a bar to list those sites."),
            plotlyOutput("reason_chart", height = "320px"),
            h3("Progress toward the goal"),
            radioButtons("group_by", "Show by:",
              choices = c("Facility" = "facility_display", "FOS" = "fos_name"),
              selected = "facility_display", inline = TRUE),
            # Height depends on how many bars there are (see group_chart_height)
            uiOutput("group_chart_ui"),
            h3(paste0("When ", sampled, " sites drop to ", missing)),
            p(class = "cov-note",
              "Red bugs leave the 5-year window as the sample ages; sites that are ", sampled,
              " only because of this year's sample reset on Jan 1. "),
            plotlyOutput("aging_chart", height = "360px")
          ),

          tabPanel("Site List", value = "sites",
            br(),
            fluidRow(
              column(4,
                selectInput("site_status", "Show:",
                  choices = c(setNames("not_counting", paste(missing, "sites")),
                              setNames("counting", paste(sampled, "sites")),
                              setNames("id_pending", paste(id_pending, "sites")),
                              "All sites" = "all"),
                  selected = "not_counting")
              ),
              column(5,
                selectInput("site_reason", "Reason:",
                  choices = c("Any reason" = "all",
                              setNames(COVERAGE_STATUS_LEVELS,
                                       unname(COVERAGE_STATUS_LABELS[COVERAGE_STATUS_LEVELS]))),
                  selected = "all")
              ),
              column(3,
                br(),
                downloadButton("download_sites", "Download CSV", class = "btn-success btn-sm")
              )
            ),
            textOutput("site_count_text"),
            p(class = "cov-note",
              missing, " sites are sorted with the best chance of a sample first ",
              "(sites that are wet most often); ", id_pending, " sites by oldest sample first. ",
              "Click a site code to open it in Webster."),
            DT::dataTableOutput("sites_table")
          ),

          tabPanel("How It's Counted", value = "rules",
            br(),
            create_rules_panel()
          )
        )
      )
    )
  )
}
