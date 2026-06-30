# FOS Detail Dashboard — UI Renderer
# =============================================================================
# render_fos_detail_dashboard() replaces the generic per-metric boxes when a
# specific FOS is selected (?view=fos&facility=Sr&fos=Alex+D).
# All data is loaded fresh here — does NOT rely on pre-filtered data[[]].
# =============================================================================

PREHATCH_GREEN_THRESH  <- 85
PREHATCH_YELLOW_THRESH <- 60

# Local copy — shared/server_utilities.R cannot be sourced by the overview app
# (regression test guards against basename "server_utilities.R"). Keep in sync.
if (!exists("make_sitecode_link", mode = "function")) {
  make_sitecode_link <- function(sitecode) {
    ifelse(
      is.na(sitecode) | nchar(trimws(as.character(sitecode))) == 0,
      as.character(sitecode),
      paste0(
        '<a href="https://webster.mmcd.org/map?search=', sitecode,
        '" target="_blank" style="color:#60a5fa;text-decoration:none;font-weight:500;">',
        sitecode, '</a>'
      )
    )
  }
}

.prehatch_color <- function(pct) {
  if (is.na(pct) || pct >= PREHATCH_GREEN_THRESH)  return("#22c55e")
  if (pct >= PREHATCH_YELLOW_THRESH)                return("#eab308")
  "#ef4444"
}

.status_dot <- function(pct) {
  tags$span(
    style = sprintf(
      "display:inline-block;width:10px;height:10px;border-radius:50%%;background:%s;margin-right:5px;vertical-align:middle;",
      .prehatch_color(pct)
    )
  )
}

.section <- function(title, icon_name, ..., open = TRUE) {
  tags$details(
    if (open) list(open = NA) else NULL,
    style = "margin-bottom:14px;",
    tags$summary(
      style = paste0(
        "cursor:pointer;padding:10px 14px;background:#1e293b;",
        "border-radius:6px;font-weight:600;font-size:1.05em;color:#f1f5f9;",
        "list-style:none;display:flex;align-items:center;gap:8px;"
      ),
      icon(icon_name),
      title
    ),
    div(style = "padding:10px 2px;", ...)
  )
}

.frac_tile <- function(label, numer, denom, tile_color = "#3b82f6",
                       sub_text = NULL) {
  div(
    style = paste0(
      "display:inline-block;text-align:center;min-width:170px;",
      "background:#1e293b;border:1px solid #334155;",
      "border-radius:8px;padding:14px 18px;margin:4px;"
    ),
    div(
      style = sprintf("font-size:1.5em;font-weight:700;color:%s;", tile_color),
      sprintf("%s / %s ac", numer, denom)
    ),
    div(style = "font-size:0.85em;color:#cbd5e1;margin-top:4px;", label),
    if (!is.null(sub_text)) {
      div(style = "font-size:0.85em;color:#e2e8f0;margin-top:2px;font-weight:600;",
          sub_text)
    }
  )
}

.pct_tile <- function(label, numer, denom, tile_color = "#3b82f6") {
  pct <- if (denom > 0) round(100 * numer / denom, 1) else 0
  div(
    style = paste0(
      "display:inline-block;text-align:center;min-width:160px;",
      "background:#1e293b;border:1px solid #334155;",
      "border-radius:8px;padding:14px 18px;margin:4px;"
    ),
    div(
      style = sprintf("font-size:2em;font-weight:700;color:%s;", tile_color),
      sprintf("%.1f%%", pct)
    ),
    div(style = "font-size:0.85em;color:#cbd5e1;margin-top:4px;", label),
    div(style = "font-size:0.8em;color:#e2e8f0;margin-top:2px;",
        sprintf("%d / %d", numer, denom))
  )
}

.dark_table <- function(...) {
  tags$table(
    class = "table table-sm table-dark",
    style = "color:#e2e8f0;",
    ...
  )
}

# Main renderer ---------------------------------------------------------------

render_fos_detail_dashboard <- function(fos_emp_num, fos_display_name, facility,
                                         analysis_date, zone_filter = NULL) {
  week_num <- as.integer(lubridate::week(analysis_date))

  # --- Load all data ---------------------------------------------------------
  prehatch_data <- tryCatch(
    load_fos_prehatch_township(fos_emp_num, analysis_date, zone_filter),
    error = function(e) {
      warning(paste("[FOS UI] prehatch:", e$message))
      list(summary = data.frame(), sites = data.frame())
    }
  )

  suco_data <- tryCatch(
    load_fos_suco(facility, analysis_date, zone_filter),
    error = function(e) {
      warning(paste("[FOS UI] suco:", e$message))
      data.frame()
    }
  )

  cb_data <- tryCatch(
    load_fos_catch_basin(fos_emp_num, facility, analysis_date, zone_filter),
    error = function(e) {
      warning(paste("[FOS UI] catch_basin:", e$message))
      data.frame()
    }
  )

  air <- tryCatch(
    load_fos_air_work(fos_emp_num, analysis_date),
    error = function(e) {
      warning(paste("[FOS UI] air_work:", e$message))
      list(summary = list(total_checked_ac = 0, red_ac = 0, blue_ac = 0,
                          red_total_ac = 0, red_treated_ac = 0,
                          pct_red_done = 0, is_complete = FALSE),
           sites = data.frame())
    }
  )

  bioassays <- tryCatch(
    load_fos_bioassays(facility, analysis_date),
    error = function(e) {
      warning(paste("[FOS UI] bioassays:", e$message))
      data.frame()
    }
  )

  # --- Header ----------------------------------------------------------------
  back_url <- sprintf("?view=fos&facility=%s&zone=1", facility)
  header <- div(
    style = "display:flex;align-items:center;gap:16px;margin-bottom:16px;",
    tags$a(
      href = back_url,
      style = "color:#60a5fa;text-decoration:none;font-size:0.9em;",
      icon("arrow-left"),
      sprintf(" Back to %s FOS Overview", facility)
    ),
    div(
      style = "font-size:1.1em;font-weight:600;color:#e2e8f0;",
      sprintf("%s • %s • Week %d", fos_display_name, facility, week_num)
    )
  )

  # --- Section 1: Ground Prehatch by Township --------------------------------
  prehatch_sum   <- prehatch_data$summary
  prehatch_sites <- prehatch_data$sites

  prehatch_section <- if (is.null(prehatch_sum) || nrow(prehatch_sum) == 0) {
    .section("Ground Prehatch", "seedling",
      div(style = "color:#e2e8f0;", "No prehatch sites found for this FOS area.")
    )
  } else {
    overall_pct <- round(100 * sum(prehatch_sum$treated) /
                           max(sum(prehatch_sum$total), 1), 1)

    town_rows <- lapply(seq_len(nrow(prehatch_sum)), function(i) {
      r <- prehatch_sum[i, ]

      town_sites <- if (!is.null(prehatch_sites) && nrow(prehatch_sites) > 0) {
        prehatch_sites[prehatch_sites$towncode == r$towncode, ]
      } else data.frame()

      site_detail <- if (nrow(town_sites) > 0) {
        town_sites <- town_sites[order(town_sites$sitecode), ]
        tags$details(
          tags$summary(
            style = "cursor:pointer;color:#60a5fa;font-size:0.85em;padding:4px 0;",
            sprintf("Show %d sites", nrow(town_sites))
          ),
          div(
            style = "margin-top:6px;max-height:250px;overflow-y:auto;",
            .dark_table(
              tags$thead(tags$tr(
                tags$th(style = "color:#cbd5e1;", "Site"),
                tags$th(style = "color:#cbd5e1;", "Treated?"),
                tags$th(style = "color:#cbd5e1;", "Expiring?")
              )),
              tags$tbody(lapply(seq_len(nrow(town_sites)), function(j) {
                s <- town_sites[j, ]
                treated_txt <- if (isTRUE(s$is_active)) {
                  tags$span(style = "color:#22c55e;", icon("check"), " Yes")
                } else {
                  tags$span(style = "color:#ef4444;", icon("times"), " No")
                }
                exp_txt <- if (isTRUE(s$is_expiring)) {
                  exp_label <- if (!is.null(s$expiry_date) && !is.na(s$expiry_date)) {
                    format(as.Date(s$expiry_date), "%b %d")
                  } else "Soon"
                  tags$span(style = "color:#eab308;", icon("clock"), " ", exp_label)
                } else ""
                tags$tr(
                  tags$td(HTML(make_sitecode_link(s$sitecode))),
                  tags$td(treated_txt),
                  tags$td(exp_txt)
                )
              }))
            )
          )
        )
      } else NULL

      tags$tr(
        tags$td(
          div(style = "font-weight:500;color:#e2e8f0;", r$city),
          site_detail
        ),
        tags$td(style = "text-align:right;color:#e2e8f0;vertical-align:top;",
                r$total),
        tags$td(style = "text-align:right;color:#e2e8f0;vertical-align:top;",
                r$treated),
        tags$td(style = "text-align:right;vertical-align:top;white-space:nowrap;",
          .status_dot(r$pct),
          tags$span(style = sprintf("color:%s;font-weight:600;", .prehatch_color(r$pct)),
                    sprintf("%.1f%%", r$pct))
        )
      )
    })

    .section(
      sprintf("Ground Prehatch (FOS) — overall %.1f%% treated", overall_pct),
      "seedling",
      .dark_table(
        style = "max-width:500px;",
        tags$thead(tags$tr(
          tags$th(style = "color:#cbd5e1;", "Township"),
          tags$th(style = "text-align:right;color:#cbd5e1;", "Sites"),
          tags$th(style = "text-align:right;color:#cbd5e1;", "Treated"),
          tags$th(style = "text-align:right;color:#cbd5e1;", "% Treated")
        )),
        tags$tbody(town_rows)
      )
    )
  }

  # --- Section 2: SUCO Goal --------------------------------------------------
  # get_config_threshold("goal","suco") returns the whole sub-list
  # (goal_per_facility, num_facilities, ...) — pull the single scalar we need.
  suco_goal <- tryCatch({
    cfg <- get_config_threshold("goal", "suco")
    g <- if (is.list(cfg)) cfg$goal_per_facility else cfg
    as.integer(g)[1]
  }, error = function(e) 12L)
  if (length(suco_goal) == 0 || is.na(suco_goal)) suco_goal <- 12L
  fac_total <- as.integer(if (!is.null(suco_data) && nrow(suco_data) > 0) {
    sum(suco_data$active, na.rm = TRUE)
  } else 0L)
  suco_pct  <- as.integer(min(100, round(100 * fac_total / max(suco_goal, 1))))
  bar_color <- if (suco_pct >= 100) "#22c55e" else if (suco_pct >= 50) "#eab308" else "#3b82f6"

  all_fos <- tryCatch({
    lkp <- get_foremen_lookup()
    lkp[lkp$facility == facility, c("emp_num", "shortname")]
  }, error = function(e) data.frame(emp_num = character(), shortname = character()))

  if (nrow(all_fos) > 0) {
    if (!is.null(suco_data) && nrow(suco_data) > 0) {
      suco_merged <- merge(all_fos, suco_data[, c("fos", "active")],
                           by.x = "emp_num", by.y = "fos", all.x = TRUE)
    } else {
      suco_merged <- all_fos
      suco_merged$active <- 0L
    }
    suco_merged$active[is.na(suco_merged$active)] <- 0L
    suco_merged <- suco_merged[order(-suco_merged$active, suco_merged$shortname), ]
  } else {
    suco_merged <- data.frame(emp_num = character(), shortname = character(),
                               active = integer())
  }

  suco_rows <- if (nrow(suco_merged) > 0) {
    lapply(seq_len(nrow(suco_merged)), function(i) {
      r <- suco_merged[i, ]
      is_me <- as.character(r$emp_num) == fos_emp_num
      row_style <- if (isTRUE(is_me)) "font-weight:700;color:#60a5fa;" else "color:#e2e8f0;"
      tags$tr(
        style = row_style,
        tags$td(r$shortname),
        tags$td(style = "text-align:right;", r$active)
      )
    })
  } else list()

  suco_section <- .section(
    sprintf("SUCO Goal — %s: %d / %d", facility, fac_total, suco_goal),
    "bullseye",
    div(
      style = "max-width:380px;margin-bottom:12px;",
      div(
        style = "background:#334155;border-radius:4px;height:20px;overflow:hidden;",
        div(style = sprintf(
          "background:%s;width:%d%%;height:100%%;border-radius:4px;",
          bar_color, suco_pct
        ))
      ),
      div(style = "font-size:0.85em;color:#cbd5e1;margin-top:4px;",
          sprintf("%d%% of weekly goal", suco_pct))
    ),
    if (length(suco_rows) > 0) {
      .dark_table(
        style = "max-width:280px;",
        tags$thead(tags$tr(
          tags$th(style = "color:#cbd5e1;", "FOS"),
          tags$th(style = "text-align:right;color:#cbd5e1;", "SUCOs")
        )),
        tags$tbody(suco_rows)
      )
    }
  )

  # --- Section 3: Catch Basin ------------------------------------------------
  cb_total    <- 0L
  cb_active   <- 0L
  cb_expiring <- 0L
  if (!is.null(cb_data) && nrow(cb_data) > 0) {
    cb_total    <- as.integer(sum(cb_data$total,    na.rm = TRUE))
    cb_active   <- as.integer(sum(cb_data$active,   na.rm = TRUE))
    cb_expiring <- as.integer(sum(cb_data$expiring, na.rm = TRUE))
  }
  cb_pct_color <- .prehatch_color(if (cb_total > 0) 100 * cb_active / cb_total else 0)

  catch_section <- .section(
    sprintf("Catch Basin (FOS) — %d total sites", cb_total),
    "water",
    div(
      style = "display:flex;gap:10px;flex-wrap:wrap;",
      .pct_tile("Treated", cb_active, cb_total, cb_pct_color),
      .pct_tile("Expiring", cb_expiring, cb_total,
                if (cb_expiring > 0) "#ef4444" else "#64748b")
    )
  )

  # --- Section 4: Air Work Acres ---------------------------------------------
  s <- air$summary
  brood_badge <- if (isTRUE(s$is_complete)) {
    tags$span(
      style = paste0(
        "background:#22c55e;color:#fff;padding:2px 8px;",
        "border-radius:4px;font-size:0.85em;margin-left:8px;"
      ),
      icon("check"), " Brood complete"
    )
  } else NULL

  air_section <- .section(
    tags$span(
      tags$span(style = "color:#f1f5f9;", "Air Work Acres"),
      brood_badge
    ),
    "plane",
    div(
      style = "display:flex;gap:8px;flex-wrap:wrap;margin-bottom:8px;",
      .frac_tile("Red / total checked",
                 s$red_ac, s$total_checked_ac, "#ef4444"),
      .frac_tile("Blue / total checked",
                 s$blue_ac, s$total_checked_ac, "#3b82f6"),
      .frac_tile("Red treated / total red",
                 s$red_treated_ac, s$red_total_ac, "#22c55e",
                 sprintf("%.1f%%", s$pct_red_done))
    ),
    if (!is.null(air$sites) && nrow(air$sites) > 0) {
      tags$details(
        tags$summary(
          style = "cursor:pointer;color:#60a5fa;font-size:0.9em;",
          "Show site detail"
        ),
        div(
          style = "margin-top:8px;max-height:300px;overflow-y:auto;",
          .dark_table(
            tags$thead(tags$tr(
              tags$th(style = "color:#cbd5e1;", "Site"),
              tags$th(style = "color:#cbd5e1;", "Acres"),
              tags$th(style = "color:#cbd5e1;", "R/B"),
              tags$th(style = "color:#cbd5e1;", "Dips"),
              tags$th(style = "color:#cbd5e1;", "Last Insp."),
              tags$th(style = "color:#cbd5e1;", "Treated?")
            )),
            tags$tbody(lapply(seq_len(nrow(air$sites)), function(i) {
              r <- air$sites[i, ]
              rb_color <- if (!is.na(r$redblue) && r$redblue == "R") "#ef4444" else
                          if (!is.na(r$redblue) && r$redblue == "B") "#3b82f6" else
                          "#94a3b8"
              tags$tr(
                tags$td(HTML(make_sitecode_link(r$sitecode))),
                tags$td(style = "color:#e2e8f0;",
                        sprintf("%.1f", as.numeric(r$acres))),
                tags$td(style = sprintf("color:%s;font-weight:600;", rb_color),
                        if (is.na(r$redblue)) "?" else r$redblue),
                tags$td(style = "color:#e2e8f0;",
                        if (is.na(r$numdip)) "" else sprintf("%.1f", r$numdip)),
                tags$td(style = "color:#e2e8f0;",
                        as.character(r$last_insp_date)),
                tags$td(
                  if (isTRUE(r$is_treated))
                    tags$span(style = "color:#22c55e;", icon("check"))
                  else ""
                )
              )
            }))
          )
        )
      )
    }
  )

  # --- Section 5: Bioassays --------------------------------------------------
  bio_total      <- as.integer(sum(bioassays$n,            na.rm = TRUE))
  bio_with_pupae <- as.integer(sum(bioassays$n_with_pupae, na.rm = TRUE))

  has_pupae_col <- !is.null(bioassays$n_with_pupae)

  bio_rows <- if (!is.null(bioassays) && nrow(bioassays) > 0) {
    lapply(seq_len(nrow(bioassays)), function(i) {
      r <- bioassays[i, ]
      is_me <- as.character(r$fosarea) == fos_emp_num
      row_style <- if (isTRUE(is_me)) "font-weight:700;color:#60a5fa;" else "color:#e2e8f0;"
      pupae_cell <- if (has_pupae_col) {
        tags$td(style = "text-align:right;",
                if (is.na(r$n_with_pupae)) "0" else as.character(r$n_with_pupae))
      } else NULL
      tags$tr(
        style = row_style,
        tags$td(if (is.na(r$shortname)) r$fosarea else r$shortname),
        tags$td(style = "text-align:right;", r$n),
        pupae_cell
      )
    })
  } else {
    list(tags$tr(
      tags$td(colspan = if (has_pupae_col) "3" else "2",
              style = "color:#e2e8f0;", "No bioassays this week")
    ))
  }

  pupae_header <- if (has_pupae_col) {
    tags$th(style = "text-align:right;color:#cbd5e1;", ">0 Pupae")
  } else NULL

  bio_section <- .section(
    sprintf("Bioassays (facility, this week) — %d total (%d with pupae)",
            bio_total, bio_with_pupae),
    "flask",
    .dark_table(
      style = "max-width:320px;",
      tags$thead(tags$tr(
        tags$th(style = "color:#cbd5e1;", "FOS"),
        tags$th(style = "text-align:right;color:#cbd5e1;", "Count"),
        pupae_header
      )),
      tags$tbody(bio_rows)
    )
  )

  # --- Assemble --------------------------------------------------------------
  # The whole panel is dark-themed: every text color in this file is light
  # (#e2e8f0 / #cbd5e1) or a semantic accent, so the container MUST have a dark
  # background or the text is invisible. We also force table cells dark here
  # because Bootstrap's `table-dark` class does not take effect in this app.
  dashboard_css <- tags$style(HTML(paste0(
    ".fos-detail-dashboard{background-color:#0f172a;}",
    ".fos-detail-dashboard table{background-color:#1e293b !important;}",
    ".fos-detail-dashboard th,.fos-detail-dashboard td{",
    "background-color:#1e293b !important;border-color:#334155 !important;}"
  )))

  div(
    class = "fos-detail-dashboard",
    style = paste0("max-width:900px;background-color:#0f172a;color:#e2e8f0;",
                   "padding:16px;border-radius:8px;"),
    dashboard_css,
    header,
    prehatch_section,
    suco_section,
    catch_section,
    air_section,
    bio_section
  )
}
