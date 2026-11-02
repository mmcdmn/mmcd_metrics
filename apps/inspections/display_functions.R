# Inspections App - Display Functions
# =============================================================================
# Charts and tables for the prehatch coverage (51% goal) views. Everything takes
# the classified site data from coverage_functions.R. Colors come from the
# selected theme (get_status_colors) so colorblind themes keep working.

library(DT)
library(plotly)
library(dplyr)

CHART_FONT <- list(size = 15, family = "Arial, sans-serif", color = "#333")

#' Theme color for each coverage status. Greens/blues count toward the goal,
#' warm colors and gray do not.
coverage_status_colors <- function(theme = "MMCD") {
  sc <- get_status_colors(theme = theme)
  c(
    red_5yr             = unname(sc["active"]),
    sampled_this_year   = unname(sc["completed"]),
    red_expired         = unname(sc["needs_treatment"]),
    no_red_sampled      = unname(sc["needs_action"]),
    inspected_no_sample = unname(sc["planned"]),
    not_inspected       = unname(sc["unknown"])
  )
}

# Light tint of a color for table cell backgrounds (keeps dark text readable)
tint_color <- function(col, alpha = 0.3) {
  grDevices::adjustcolor(col, alpha.f = alpha)
}

# =============================================================================
# PROGRESS CHARTS
# =============================================================================

#' Horizontal bar: how many Missing sites fall in each reason.
#' Bars carry key = status so a click can jump to that reason in the site list.
create_reason_chart <- function(sites, theme = "MMCD") {
  not_counting <- sites[!sites$counts, , drop = FALSE]
  if (nrow(not_counting) == 0) return(NULL)

  tab <- not_counting %>%
    count(status, status_label, name = "n") %>%
    arrange(n)
  cols <- coverage_status_colors(theme)

  plot_ly(
    tab, x = ~n, y = ~as.character(status_label), type = "bar", orientation = "h",
    key = ~as.character(status), source = "reason_chart",
    marker = list(color = unname(cols[as.character(tab$status)])),
    text = ~format(n, big.mark = ","), textposition = "outside", cliponaxis = FALSE,
    hovertemplate = "%{y}: %{x:,} sites<br>Click to list these sites<extra></extra>"
  ) %>%
    layout(
      xaxis = list(title = list(text = paste(COVERAGE_LABEL_NOT_COUNTING, "sites"), font = CHART_FONT),
                   tickfont = CHART_FONT, rangemode = "tozero"),
      yaxis = list(title = "", tickfont = CHART_FONT,
                   categoryorder = "array", categoryarray = as.character(tab$status_label)),
      margin = list(l = 230, r = 60, t = 10, b = 50),
      font = CHART_FONT
    ) %>%
    event_register("plotly_click")
}

# Layout of the progress chart: one row per facility / FOS plus the margins
GROUP_ROW_PX <- 28
GROUP_MARGIN_TOP_PX <- 40
GROUP_MARGIN_BOTTOM_PX <- 120

#' Pixel height of the progress chart. It grows with the number of bars: a fixed
#' output height clips the chart once there are more than ~10 rows (every FOS),
#' so the UI sizes plotlyOutput() with this.
group_chart_height <- function(n_groups) {
  max(340, GROUP_ROW_PX * n_groups + GROUP_MARGIN_TOP_PX + GROUP_MARGIN_BOTTOM_PX)
}

#' Horizontal stacked bars (percent of sites) per facility / FOS with the goal
#' line and the number of sites still needed to reach it. Most complete group on
#' top, least complete at the bottom.
#'
#' The chart fills its output box; size that box with group_chart_height().
#'
#' @param summary_df summarise_coverage() output with a label column
#' @param label_col column holding the bar labels
create_group_chart <- function(summary_df, label_col, goal = 51, theme = "MMCD") {
  if (nrow(summary_df) == 0) return(NULL)
  cols <- coverage_status_colors(theme)
  sampled <- COVERAGE_LABEL_COUNTING
  missing <- COVERAGE_LABEL_NOT_COUNTING

  d <- summary_df
  d$label <- as.character(d[[label_col]])
  d$via_red_pct <- 100 * d$via_red / d$total
  d$via_sample_pct <- 100 * d$via_sample_only / d$total
  d$not_pct <- 100 * d$not_counting / d$total
  d$note <- ifelse(d$goal_met,
                   paste0("+", format(d$cushion, big.mark = ","), " spare"),
                   paste0("needs ", format(d$needed, big.mark = ",")))
  d <- d[order(-d$pct, d$needed, d$label), ]
  d$label <- factor(d$label, levels = d$label)

  # Plotly draws the first category at the bottom; reverse the axis so the
  # first (most complete) row is on top.
  plot_area_px <- GROUP_ROW_PX * nrow(d)

  plot_ly(d, y = ~label) %>%
    add_trace(x = ~via_red_pct, type = "bar", orientation = "h",
              name = paste0(sampled, ": red bug in last 5 yrs"),
              marker = list(color = cols[["red_5yr"]]),
              hovertemplate = paste0("%{y}<br>", sampled, " (red bug): %{x:.1f}% of sites<extra></extra>")) %>%
    add_trace(x = ~via_sample_pct, type = "bar", orientation = "h",
              name = paste0(sampled, ": this year's sample only"),
              marker = list(color = cols[["sampled_this_year"]]),
              hovertemplate = paste0("%{y}<br>", sampled, " (this year only): %{x:.1f}%<extra></extra>")) %>%
    add_trace(x = ~not_pct, type = "bar", orientation = "h", name = missing,
              marker = list(color = cols[["red_expired"]]),
              hovertemplate = paste0("%{y}<br>", missing, ": %{x:.1f}%<extra></extra>")) %>%
    add_trace(x = rep(103, nrow(d)), y = ~label, type = "scatter", mode = "text",
              text = ~note, textposition = "middle right", showlegend = FALSE, hoverinfo = "skip",
              textfont = list(size = 13, color = "#333")) %>%
    layout(
      barmode = "stack",
      xaxis = list(title = list(text = "% of prehatch sites", font = CHART_FONT),
                   range = c(0, 125), tickvals = c(0, 25, 50, 75, 100), tickfont = CHART_FONT),
      yaxis = list(title = "", tickfont = list(size = 14, family = "Arial, sans-serif"),
                   categoryorder = "array", categoryarray = levels(d$label),
                   autorange = "reversed", automargin = TRUE),
      shapes = list(list(type = "line", x0 = goal, x1 = goal, y0 = 0, y1 = 1, yref = "paper",
                         line = list(color = "#111", width = 3, dash = "dash"))),
      annotations = list(list(x = goal, y = 1.02, yref = "paper", text = paste0("Goal ", goal, "%"),
                              showarrow = FALSE, font = list(size = 14, color = "#111"))),
      # paper units are the plot area, so convert ~55px under the axis to a fraction
      legend = list(orientation = "h", x = 0.5, xanchor = "center",
                    y = -55 / plot_area_px, yanchor = "top", font = CHART_FONT),
      margin = list(l = 130, r = 20, t = GROUP_MARGIN_TOP_PX, b = GROUP_MARGIN_BOTTOM_PX),
      font = CHART_FONT
    )
}

#' Sites that drop to Missing, by month: red bugs ageing out of the 5-year window
#' versus this year's samples resetting on Jan 1.
create_aging_chart <- function(aging_df, theme = "MMCD") {
  if (nrow(aging_df) == 0 || sum(aging_df$n) == 0) return(NULL)
  cols <- coverage_status_colors(theme)
  aging_df$month_label <- format(aging_df$month, "%b %Y")
  levels_m <- unique(aging_df$month_label[order(aging_df$month)])
  aging_df$month_label <- factor(aging_df$month_label, levels = levels_m)

  series <- c("Red bug ages out of 5-yr window" = cols[["red_5yr"]],
              "This year's sample (resets Jan 1)" = cols[["sampled_this_year"]])

  p <- plot_ly()
  for (nm in names(series)) {
    d <- aging_df[aging_df$drops_because == nm, ]
    p <- add_trace(p, data = d, x = ~month_label, y = ~n, type = "bar", name = nm,
                   marker = list(color = unname(series[[nm]])),
                   hovertemplate = paste0("%{x}<br>", nm, ": %{y:,} sites<extra></extra>"))
  }
  p %>% layout(
    barmode = "stack",
    xaxis = list(title = "", tickfont = CHART_FONT, tickangle = -45),
    yaxis = list(title = list(text = paste("Sites that drop to", COVERAGE_LABEL_NOT_COUNTING), font = CHART_FONT),
                 tickfont = CHART_FONT),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3, font = CHART_FONT),
    margin = list(l = 70, r = 20, t = 10, b = 110),
    font = CHART_FONT
  )
}

# =============================================================================
# SITE LIST TABLE
# =============================================================================

#' Order sites for the work list: best chance of getting a sample first
sort_sites_for_work <- function(sites, view = "not_counting") {
  if (nrow(sites) == 0) return(sites)
  if (view == "not_counting" || view == "all") {
    rank <- match(sites$likely_to_yield, c("High", "Medium", "Unknown", "Low"))
    sites[order(sites$counts, rank, -ifelse(is.na(sites$wet_pct), -1, sites$wet_pct), sites$sitecode), ]
  } else if (view == "id_pending") {
    # waiting on the lab: the sample that has waited longest first
    sites[order(sites$last_sample_date, sites$sitecode), ]
  } else {
    # counting: the ones that stop counting soonest first
    sites[order(ifelse(is.na(sites$days_left), Inf, sites$days_left), sites$sitecode), ]
  }
}

#' Friendly column set for the site table / CSV
format_coverage_sites <- function(sites, link = TRUE) {
  if (nrow(sites) == 0) return(data.frame())

  facility <- if ("facility_display" %in% names(sites)) sites$facility_display else sites$facility
  fos <- if ("fos_name" %in% names(sites)) sites$fos_name else sites$fosarea
  result <- ifelse(is.na(sites$last_sample_date), NA_character_,
                   ifelse(sites$last_sample_result %in% "R", "Red",
                          ifelse(sites$last_sample_result %in% "B", "Blue", "Not entered yet")))

  out <- data.frame(
    `Site` = if (link) make_sitecode_link(sites$sitecode) else sites$sitecode,
    `Status` = as.character(sites$status_label),
    `Why` = sites$reason,
    `Facility` = facility,
    `FOS` = fos,
    `Zone` = sites$zone,
    `Priority` = sites$priority,
    `Visits (5 yr)` = sites$insp_5y,
    `Visits (this yr)` = sites$insp_this_year,
    `Wet %` = sites$wet_pct,
    `Wet visits` = ifelse(sites$wet_n > 0, paste0(sites$wet_count, " of ", sites$wet_n), NA_character_),
    `Likely to yield` = sites$likely_to_yield,
    `Last inspected` = sites$last_insp_date,
    `Last wet` = sites$last_wet_date,
    `Samples (5 yr)` = sites$samples_5y,
    `Samples (this yr)` = sites$samples_this_year,
    `Samples ID pending` = sites$pending_this_year,
    `Last sample` = sites$last_sample_date,
    `Last sample result` = result,
    `Last red bug` = sites$last_red_date,
    `Counts through` = sites$counts_through,
    `Days left` = sites$days_left,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  # "Sampled through <date>": the last day the site stays in the Sampled group
  names(out)[names(out) == "Counts through"] <- paste(COVERAGE_LABEL_COUNTING, "through")
  names(out)[names(out) == "Samples ID pending"] <- paste("Samples", COVERAGE_LABEL_ID_PENDING)
  out
}

render_coverage_site_table <- function(sites, theme = "MMCD") {
  formatted <- format_coverage_sites(sites)

  if (nrow(formatted) == 0) {
    return(DT::datatable(data.frame(Message = "No sites match these filters"),
                         rownames = FALSE, options = list(dom = "t")))
  }

  cols <- coverage_status_colors(theme)
  labels <- unname(COVERAGE_STATUS_LABELS[COVERAGE_STATUS_LEVELS])

  DT::datatable(
    formatted,
    escape = FALSE,
    rownames = FALSE,
    filter = "top",
    class = "compact stripe hover",
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      autoWidth = FALSE,
      order = list(),   # rows arrive pre-sorted (sort_sites_for_work)
      columnDefs = list(list(width = "420px", targets = 2)),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().container()).find('table').css('font-size', '14px');",
        "$(this.api().table().container()).find('thead').css('font-weight', 'bold');",
        "}"
      )
    )
  ) %>%
    DT::formatStyle(
      "Status",
      backgroundColor = DT::styleEqual(labels, tint_color(unname(cols[COVERAGE_STATUS_LEVELS]), 0.35))
    ) %>%
    DT::formatStyle(
      "Likely to yield",
      backgroundColor = DT::styleEqual(
        c("High", "Medium", "Low", "Unknown"),
        c(tint_color("#16a34a", 0.3), tint_color("#eab308", 0.3), tint_color("#9ca3af", 0.3), "transparent")
      )
    ) %>%
    DT::formatStyle(
      "Days left",
      backgroundColor = DT::styleInterval(c(30, 90), c(tint_color("#dc2626", 0.3), tint_color("#eab308", 0.3), "transparent"))
    )
}
