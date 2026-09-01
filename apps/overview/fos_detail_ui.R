# FOS Detail Dashboard — UI Renderer
# =============================================================================
# render_fos_detail_dashboard() replaces the generic per-metric boxes when a
# specific FOS is selected (?view=fos&facility=Sr&fos=Alex+D).
# All data is loaded fresh here — does NOT rely on pre-filtered data[[]].
# =============================================================================

# Thresholds come from config/app_config.yaml (thresholds.fixed_pct.air_sites)
# so this panel cannot drift from the overview value boxes that use the same
# rule. Literals are the fallback if the config is unavailable.
.fos_thresh <- function() {
  cfg <- tryCatch(get_config_threshold("fixed_pct", "air_sites"), error = function(e) NULL)
  list(green  = if (!is.null(cfg$good))    cfg$good    else 85,
       yellow = if (!is.null(cfg$warning)) cfg$warning else 60)
}

# Local copy — shared/server_utilities.R cannot be sourced by the overview app
# (regression test guards against basename "server_utilities.R"). Keep in sync.
if (!exists("make_sitecode_link", mode = "function")) {
  make_sitecode_link <- function(sitecode) {
    ifelse(
      is.na(sitecode) | nchar(trimws(as.character(sitecode))) == 0,
      as.character(sitecode),
      paste0(
        '<a href="https://webster.mmcd.org/map?search=', sitecode,
        '" target="_blank" style="color:var(--fos-link);text-decoration:none;font-weight:500;">',
        sitecode, '</a>'
      )
    )
  }
}

#' Build the CSS custom properties this panel is styled with
#'
#' Every color in this file is written as var(--fos-*); this is the single
#' place those variables get values. Swapping theme swaps the whole panel.
#' @param theme Color theme name
#' @return CSS declaration string for the wrapper element
.fos_css_vars <- function(theme = getOption("mmcd.color.theme", "MMCD")) {
  pal <- tryCatch(get_theme_palette(theme), error = function(e) NULL)
  sf  <- pal$surface
  ind <- tryCatch(get_indicator_colors(theme = theme), error = function(e) NULL)

  g <- function(v, fallback) {
    if (is.null(v) || length(v) != 1 || is.na(v) || !nzchar(v)) fallback else unname(v)
  }
  vars <- c(
    "--fos-bg"          = g(sf["bg"],          "#0f172a"),
    "--fos-panel"       = g(sf["panel"],       "#1e293b"),
    "--fos-border"      = g(sf["border"],      "#334155"),
    "--fos-text"        = g(sf["text"],        "#e2e8f0"),
    "--fos-text-muted"  = g(sf["text_muted"],  "#cbd5e1"),
    "--fos-text-strong" = g(sf["text_strong"], "#f1f5f9"),
    "--fos-link"        = g(sf["link"],        "#60a5fa"),
    "--fos-neutral"     = g(sf["neutral"],     "#64748b"),
    "--fos-faint"       = g(sf["faint"],       "#94a3b8"),
    "--fos-good"        = g(ind["good"],       "#22c55e"),
    "--fos-warn"        = g(ind["warning"],    "#eab308"),
    "--fos-bad"         = g(ind["alert"],      "#ef4444"),
    "--fos-info"        = g(pal$primary[1],    "#3b82f6")
  )
  # Red/Blue air site classification is domain data, not status: a "Blue" site
  # must stay blue under every theme, so these two are deliberately fixed.
  vars["--fos-cat-red"]  <- "#ef4444"
  vars["--fos-cat-blue"] <- "#3b82f6"

  # The accent button and badges sit on --fos-good; pick readable text for them.
  vars["--fos-accent"]    <- vars[["--fos-good"]]
  vars["--fos-on-accent"] <- if (exists("contrast_text_color", mode = "function", inherits = TRUE)) {
    contrast_text_color(vars[["--fos-good"]])
  } else "#ffffff"

  paste0(paste0(names(vars), ":", unname(vars), ";", collapse = ""))
}

.prehatch_color <- function(pct) {
  th <- .fos_thresh()
  if (is.na(pct) || pct >= th$green)  return("var(--fos-good)")
  if (pct >= th$yellow)               return("var(--fos-warn)")
  "var(--fos-bad)"
}

.status_dot <- function(pct) {
  tags$span(
    style = sprintf(
      "display:inline-block;width:10px;height:10px;border-radius:50%%;background:%s;margin-right:5px;vertical-align:middle;",
      .prehatch_color(pct)
    )
  )
}

.section <- function(title, section_icon, ..., open = TRUE) {
  # section_icon may be a FontAwesome name (character) or a prebuilt tag such as
  # an <img> (see .img_icon) — matching how index.html uses image-based icons.
  icon_tag <- if (is.character(section_icon)) icon(section_icon) else section_icon
  tags$details(
    if (open) list(open = NA) else NULL,
    style = "margin-bottom:14px;",
    tags$summary(
      style = paste0(
        "cursor:pointer;padding:10px 14px;background:var(--fos-panel);",
        "border-radius:6px;font-weight:600;font-size:1.05em;color:var(--fos-text-strong);",
        "list-style:none;display:flex;align-items:center;gap:8px;"
      ),
      icon_tag,
      title
    ),
    div(style = "padding:10px 2px;", ...)
  )
}

#' Image-based section icon, served from the overview app's www/assets folder
#' (same mechanism as the metric icons in dynamic_ui.R and index.html).
#' @param src Path under www, e.g. "assets/drone.jpg"
.img_icon <- function(src, alt = "") {
  tags$img(
    src = src, alt = alt,
    style = "width:1.15em;height:1.15em;object-fit:contain;vertical-align:middle;"
  )
}

.frac_tile <- function(label, numer, denom, tile_color = "var(--fos-info)",
                       sub_text = NULL) {
  div(
    style = paste0(
      "display:inline-block;text-align:center;min-width:170px;",
      "background:var(--fos-panel);border:1px solid var(--fos-border);",
      "border-radius:8px;padding:14px 18px;margin:4px;"
    ),
    div(
      style = sprintf("font-size:1.5em;font-weight:700;color:%s;", tile_color),
      sprintf("%s / %s ac", numer, denom)
    ),
    div(style = "font-size:0.85em;color:var(--fos-text-muted);margin-top:4px;", label),
    if (!is.null(sub_text)) {
      div(style = "font-size:0.85em;color:var(--fos-text);margin-top:2px;font-weight:600;",
          sub_text)
    }
  )
}

.pct_tile <- function(label, numer, denom, tile_color = "var(--fos-info)") {
  pct <- if (denom > 0) round(100 * numer / denom, 1) else 0
  div(
    style = paste0(
      "display:inline-block;text-align:center;min-width:160px;",
      "background:var(--fos-panel);border:1px solid var(--fos-border);",
      "border-radius:8px;padding:14px 18px;margin:4px;"
    ),
    div(
      style = sprintf("font-size:2em;font-weight:700;color:%s;", tile_color),
      sprintf("%.1f%%", pct)
    ),
    div(style = "font-size:0.85em;color:var(--fos-text-muted);margin-top:4px;", label),
    div(style = "font-size:0.8em;color:var(--fos-text);margin-top:2px;",
        sprintf("%d / %d", numer, denom))
  )
}

.dark_table <- function(...) {
  tags$table(
    class = "table table-sm table-dark",
    style = "color:var(--fos-text);",
    ...
  )
}

#' Build a per-township treatment section (used for both ground prehatch and
#' drone — identical layout, different data + heading).
#'
#' @param township_data  List(summary, sites) from load_fos_prehatch_township()
#' @param title_prefix   Heading text before the "— overall X% treated" suffix
#' @param icon_name      Font Awesome icon name for the section header
#' @param empty_msg      Message shown when there are no sites
.township_section <- function(township_data, title_prefix, icon_name, empty_msg) {
  summary_df <- township_data$summary
  sites_df   <- township_data$sites

  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return(.section(title_prefix, icon_name,
      div(style = "color:var(--fos-text);", empty_msg)))
  }

  overall_pct <- round(100 * sum(summary_df$treated) /
                         max(sum(summary_df$total), 1), 1)

  town_rows <- lapply(seq_len(nrow(summary_df)), function(i) {
    r <- summary_df[i, ]

    town_sites <- if (!is.null(sites_df) && nrow(sites_df) > 0) {
      sites_df[sites_df$towncode == r$towncode, ]
    } else data.frame()

    site_detail <- if (nrow(town_sites) > 0) {
      town_sites <- town_sites[order(town_sites$sitecode), ]
      tags$details(
        tags$summary(
          style = "cursor:pointer;color:var(--fos-link);font-size:0.85em;padding:4px 0;",
          sprintf("Show %d sites", nrow(town_sites))
        ),
        div(
          style = "margin-top:6px;max-height:250px;overflow-y:auto;",
          .dark_table(
            tags$thead(tags$tr(
              tags$th(style = "color:var(--fos-text-muted);", "Site"),
              tags$th(style = "color:var(--fos-text-muted);", "Treated?"),
              tags$th(style = "color:var(--fos-text-muted);", "Expiring?")
            )),
            tags$tbody(lapply(seq_len(nrow(town_sites)), function(j) {
              s <- town_sites[j, ]
              treated_txt <- if (isTRUE(s$is_active)) {
                tags$span(style = "color:var(--fos-good);", icon("check"), " Yes")
              } else {
                tags$span(style = "color:var(--fos-bad);", icon("times"), " No")
              }
              exp_txt <- if (isTRUE(s$is_expiring)) {
                exp_label <- if (!is.null(s$expiry_date) && !is.na(s$expiry_date)) {
                  format(as.Date(s$expiry_date), "%b %d")
                } else "Soon"
                tags$span(style = "color:var(--fos-warn);", icon("clock"), " ", exp_label)
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
        div(style = "font-weight:500;color:var(--fos-text);", r$city),
        site_detail
      ),
      tags$td(style = "text-align:right;color:var(--fos-text);vertical-align:top;",
              r$total),
      tags$td(style = "text-align:right;color:var(--fos-text);vertical-align:top;",
              r$treated),
      tags$td(style = "text-align:right;vertical-align:top;white-space:nowrap;",
        .status_dot(r$pct),
        tags$span(style = sprintf("color:%s;font-weight:600;", .prehatch_color(r$pct)),
                  sprintf("%.1f%%", r$pct))
      )
    )
  })

  .section(
    sprintf("%s — overall %.1f%% treated", title_prefix, overall_pct),
    icon_name,
    .dark_table(
      style = "max-width:500px;",
      tags$thead(tags$tr(
        tags$th(style = "color:var(--fos-text-muted);", "Township"),
        tags$th(style = "text-align:right;color:var(--fos-text-muted);", "Sites"),
        tags$th(style = "text-align:right;color:var(--fos-text-muted);", "Treated"),
        tags$th(style = "text-align:right;color:var(--fos-text-muted);", "% Treated")
      )),
      tags$tbody(town_rows)
    )
  )
}

# Main renderer ---------------------------------------------------------------

render_fos_detail_dashboard <- function(fos_emp_num, fos_display_name, facility,
                                         analysis_date, zone_filter = NULL,
                                         theme = getOption("mmcd.color.theme", "MMCD")) {
  week_num <- as.integer(lubridate::week(analysis_date))

  # --- Load all data ---------------------------------------------------------
  # Ground prehatch EXCLUDES drone sites; drone sites get their own section,
  # sourced from the drone app so ALL drone sites are included (every priority,
  # regardless of prehatch status).
  prehatch_data <- tryCatch(
    load_fos_prehatch_township(fos_emp_num, analysis_date, zone_filter),
    error = function(e) {
      warning(paste("[FOS UI] prehatch:", e$message))
      list(summary = data.frame(), sites = data.frame())
    }
  )

  drone_data <- tryCatch(
    load_fos_drone_township(fos_emp_num, analysis_date, zone_filter),
    error = function(e) {
      warning(paste("[FOS UI] drone:", e$message))
      list(summary = data.frame(), sites = data.frame())
    }
  )

  structures_data <- tryCatch(
    load_fos_structures_township(fos_emp_num, analysis_date, zone_filter),
    error = function(e) {
      warning(paste("[FOS UI] structures:", e$message))
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
    style = "display:flex;align-items:center;gap:16px;margin-bottom:16px;flex-wrap:wrap;",
    tags$a(
      href = back_url,
      style = "color:var(--fos-link);text-decoration:none;font-size:0.9em;",
      icon("arrow-left"),
      sprintf(" Back to %s FOS Overview", facility)
    ),
    div(
      style = "font-size:1.1em;font-weight:600;color:var(--fos-text);",
      sprintf("%s • %s • Week %d", fos_display_name, facility, week_num)
    ),
    # Opens the crew-note builder (wired in dynamic_server.R). Only present in
    # this FOS-detail view, so it never shows on the generic overviews.
    actionButton(
      "open_crew_note",
      label = tagList(icon("clipboard-list"), " Make Crew Instructions"),
      style = paste0(
        "margin-left:auto;background:var(--fos-accent);color:var(--fos-on-accent);border:none;",
        "border-radius:6px;padding:8px 16px;font-weight:600;font-size:0.9em;"
      )
    )
  )

  # --- Section 1: Ground Prehatch by Township (drone sites excluded) ----------
  prehatch_section <- .township_section(
    prehatch_data,
    "Ground Prehatch (FOS)",
    .img_icon("assets/ground.png", "Ground prehatch"),
    "No ground prehatch sites found for this FOS area."
  )

  # --- Section 1b: Drone by Township (drone sites only) ----------------------
  drone_section <- .township_section(
    drone_data,
    "Drone (FOS)",
    .img_icon("assets/drone.jpg", "Drone"),
    "No drone sites found for this FOS area."
  )

  # --- Section 1c: Structures by Township ------------------------------------
  structures_section <- .township_section(
    structures_data,
    "Structures (FOS)",
    .img_icon("assets/catchbasin.png", "Structures"),
    "No structures found for this FOS area."
  )

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
  bar_color <- if (suco_pct >= 100) "var(--fos-good)" else if (suco_pct >= 50) "var(--fos-warn)" else "var(--fos-info)"

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
      row_style <- if (isTRUE(is_me)) "font-weight:700;color:var(--fos-link);" else "color:var(--fos-text);"
      tags$tr(
        style = row_style,
        tags$td(r$shortname),
        tags$td(style = "text-align:right;", r$active)
      )
    })
  } else list()

  suco_section <- .section(
    sprintf("SUCO Goal — %s: %d / %d", facility, fac_total, suco_goal),
    .img_icon("assets/tree-solid-full.svg", "SUCO"),
    div(
      style = "max-width:380px;margin-bottom:12px;",
      div(
        style = "background:var(--fos-border);border-radius:4px;height:20px;overflow:hidden;",
        div(style = sprintf(
          "background:%s;width:%d%%;height:100%%;border-radius:4px;",
          bar_color, suco_pct
        ))
      ),
      div(style = "font-size:0.85em;color:var(--fos-text-muted);margin-top:4px;",
          sprintf("%d%% of weekly goal", suco_pct))
    ),
    if (length(suco_rows) > 0) {
      .dark_table(
        style = "max-width:280px;",
        tags$thead(tags$tr(
          tags$th(style = "color:var(--fos-text-muted);", "FOS"),
          tags$th(style = "text-align:right;color:var(--fos-text-muted);", "SUCOs")
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
    .img_icon("assets/catchbasin.png", "Catch basin"),
    div(
      style = "display:flex;gap:10px;flex-wrap:wrap;",
      .pct_tile("Treated", cb_active, cb_total, cb_pct_color),
      .pct_tile("Expiring", cb_expiring, cb_total,
                if (cb_expiring > 0) "var(--fos-bad)" else "var(--fos-neutral)")
    )
  )

  # --- Section 4: Air Work Acres ---------------------------------------------
  s <- air$summary
  brood_badge <- if (isTRUE(s$is_complete)) {
    tags$span(
      style = paste0(
        "background:var(--fos-good);color:var(--fos-on-accent);padding:2px 8px;",
        "border-radius:4px;font-size:0.85em;margin-left:8px;"
      ),
      icon("check"), " Brood complete"
    )
  } else NULL

  air_section <- .section(
    tags$span(
      tags$span(style = "color:var(--fos-text-strong);", "Air Work Acres"),
      brood_badge
    ),
    .img_icon("assets/helicopter-solid-full.svg", "Air work"),
    div(
      style = "display:flex;gap:8px;flex-wrap:wrap;margin-bottom:8px;",
      .frac_tile("Red / total checked",
                 s$red_ac, s$total_checked_ac, "var(--fos-cat-red)"),
      .frac_tile("Blue / total checked",
                 s$blue_ac, s$total_checked_ac, "var(--fos-cat-blue)"),
      .frac_tile("Red treated / total red",
                 s$red_treated_ac, s$red_total_ac, "var(--fos-good)",
                 sprintf("%.1f%%", s$pct_red_done))
    ),
    if (!is.null(air$sites) && nrow(air$sites) > 0) {
      tags$details(
        tags$summary(
          style = "cursor:pointer;color:var(--fos-link);font-size:0.9em;",
          "Show site detail"
        ),
        div(
          style = "margin-top:8px;max-height:300px;overflow-y:auto;",
          .dark_table(
            tags$thead(tags$tr(
              tags$th(style = "color:var(--fos-text-muted);", "Site"),
              tags$th(style = "color:var(--fos-text-muted);", "Acres"),
              tags$th(style = "color:var(--fos-text-muted);", "R/B"),
              tags$th(style = "color:var(--fos-text-muted);", "Dips"),
              tags$th(style = "color:var(--fos-text-muted);", "Last Insp."),
              tags$th(style = "color:var(--fos-text-muted);", "Treated?")
            )),
            tags$tbody(lapply(seq_len(nrow(air$sites)), function(i) {
              r <- air$sites[i, ]
              # R/B is the site's own red/blue classification — a domain
              # category, not a status, so it does not follow the theme.
              rb_color <- if (!is.na(r$redblue) && r$redblue == "R") "var(--fos-cat-red)" else
                          if (!is.na(r$redblue) && r$redblue == "B") "var(--fos-cat-blue)" else
                          "var(--fos-faint)"
              tags$tr(
                tags$td(HTML(make_sitecode_link(r$sitecode))),
                tags$td(style = "color:var(--fos-text);",
                        sprintf("%.1f", as.numeric(r$acres))),
                tags$td(style = sprintf("color:%s;font-weight:600;", rb_color),
                        if (is.na(r$redblue)) "?" else r$redblue),
                tags$td(style = "color:var(--fos-text);",
                        if (is.na(r$numdip)) "" else sprintf("%.1f", r$numdip)),
                tags$td(style = "color:var(--fos-text);",
                        as.character(r$last_insp_date)),
                tags$td(
                  if (isTRUE(r$is_treated))
                    tags$span(style = "color:var(--fos-good);", icon("check"))
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
      row_style <- if (isTRUE(is_me)) "font-weight:700;color:var(--fos-link);" else "color:var(--fos-text);"
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
              style = "color:var(--fos-text);", "No bioassays this week")
    ))
  }

  pupae_header <- if (has_pupae_col) {
    tags$th(style = "text-align:right;color:var(--fos-text-muted);", ">0 Pupae")
  } else NULL

  bio_section <- .section(
    sprintf("Bioassays (facility, this week) — %d total (%d with pupae)",
            bio_total, bio_with_pupae),
    .img_icon("assets/adult.png", "Bioassays"),
    .dark_table(
      style = "max-width:320px;",
      tags$thead(tags$tr(
        tags$th(style = "color:var(--fos-text-muted);", "FOS"),
        tags$th(style = "text-align:right;color:var(--fos-text-muted);", "Count"),
        pupae_header
      )),
      tags$tbody(bio_rows)
    )
  )

  # --- Assemble --------------------------------------------------------------
  # Every color in this panel is a var(--fos-*) reference; .fos_css_vars()
  # resolves them from the active theme's `surface` + `indicators` blocks. The
  # container must define them (and carry the background) or the light text
  # would be invisible. Table cells are forced here because Bootstrap's
  # `table-dark` class does not take effect in this app.
  css_vars <- .fos_css_vars(theme)
  dashboard_css <- tags$style(HTML(paste0(
    ".fos-detail-dashboard{background-color:var(--fos-bg);}",
    ".fos-detail-dashboard table{background-color:var(--fos-panel) !important;}",
    ".fos-detail-dashboard th,.fos-detail-dashboard td{",
    "background-color:var(--fos-panel) !important;border-color:var(--fos-border) !important;}"
  )))

  div(
    class = "fos-detail-dashboard",
    style = paste0(css_vars,
                   "max-width:900px;background-color:var(--fos-bg);color:var(--fos-text);",
                   "padding:16px;border-radius:8px;"),
    dashboard_css,
    header,
    prehatch_section,
    drone_section,
    structures_section,
    suco_section,
    catch_section,
    air_section,
    bio_section
  )
}
