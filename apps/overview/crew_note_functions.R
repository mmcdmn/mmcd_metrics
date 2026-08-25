# Crew Instructions Report — HTML Generator
# =============================================================================
# build_crew_note_html() turns the FOS crew-note builder form (per-employee,
# per-day tasks + crew-wide notes) plus the FOS's live status data into a single
# self-contained, printable HTML document.
#
# The look is adapted from the hand-authored crew-note.html: a light "pine
# green" field report with a print stylesheet. Everything is inlined so the
# downloaded .html opens and prints standalone (no Shiny, no external assets).
#
# This file is intentionally independent of fos_detail_ui.R — that renderer is
# dark-themed for the on-screen dashboard; the printed report is light-themed.
# The status-data shapes are the SAME ones the dashboard loaders return:
#   prehatch/structures : list(summary[towncode,city,total,treated,pct], sites)
#   catch_basin         : data.frame(fos,total,active,expiring)  (one row)
#   suco                : data.frame(fos,display_name,active)
# =============================================================================

# --- small HTML-escaping + formatting helpers --------------------------------

#' Escape user-entered text for safe insertion into HTML.
.cn_escape <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub('"',  "&quot;", x, fixed = TRUE)
  x
}

#' Escape text then turn newlines into <br> so multi-line task/notes render.
.cn_multiline <- function(x) {
  gsub("\n", "<br>", .cn_escape(x), fixed = TRUE)
}

#' Status pill class for a treated-percentage (matches crew-note semantics).
.cn_pct_pill <- function(pct) {
  if (is.na(pct))      return("warn")
  if (pct >= 100)      return("done")
  if (pct >= 85)       return("done")
  if (pct >= 60)       return("warn")
  "crit"
}

# --- section builders --------------------------------------------------------

#' One township status block (light-theme analogue of the dashboard's
#' .township_section): a per-town table plus, for towns with untreated sites,
#' the untreated site codes as monospace chips grouped by section prefix.
#'
#' @param township_data list(summary, sites) from a load_fos_*_township loader
#' @param title         Section heading (e.g. "Ground Prehatch")
#' @param assignee      Optional small right-aligned label (crew member / focus)
#' @return HTML string, or "" if there is nothing to show
.cn_township_section <- function(township_data, title, assignee = NULL) {
  summary_df <- township_data$summary
  sites_df   <- township_data$sites
  if (is.null(summary_df) || nrow(summary_df) == 0) return("")

  overall_pct <- round(100 * sum(summary_df$treated) /
                         max(sum(summary_df$total), 1), 1)
  overall_pill <- .cn_pct_pill(overall_pct)

  # Per-town rows
  summary_df <- summary_df[order(summary_df$city), ]
  rows <- vapply(seq_len(nrow(summary_df)), function(i) {
    r <- summary_df[i, ]
    left <- max(0L, as.integer(r$total) - as.integer(r$treated))

    # Untreated site chips for this town, grouped by 6-char section prefix.
    chips_html <- ""
    if (left > 0 && !is.null(sites_df) && nrow(sites_df) > 0) {
      untreated <- sites_df[sites_df$towncode == r$towncode &
                              !(sites_df$is_active %in% TRUE), ]
      untreated <- untreated[!is.na(untreated$sitecode), ]
      if (nrow(untreated) > 0) {
        untreated <- untreated[order(untreated$sitecode), ]
        codes <- as.character(untreated$sitecode)
        section <- substr(codes, 1, 6)
        tail_code <- substr(codes, 8, nchar(codes))  # after the "-"
        tail_code[tail_code == ""] <- codes[tail_code == ""]
        parts <- lapply(unique(section), function(sec) {
          these <- tail_code[section == sec]
          chip_spans <- paste0('<span class="chip">', .cn_escape(these),
                               "</span>", collapse = "")
          paste0('<div class="sect"><div class="pre">', .cn_escape(sec),
                 '</div><div class="chips">', chip_spans, "</div></div>")
        })
        chips_html <- paste0(
          '<details><summary>', left, ' untreated</summary>',
          '<div class="chipwrap">', paste0(parts, collapse = ""), "</div></details>"
        )
      }
    }

    sprintf(
      paste0('<tr><td class="town-name">%s%s</td>',
             '<td class="num">%d</td><td class="num">%d</td>',
             '<td class="num"><span class="dot %s"></span>%.1f%%</td></tr>'),
      .cn_escape(r$city), chips_html,
      as.integer(r$total), as.integer(r$treated),
      .cn_pct_pill(r$pct), r$pct
    )
  }, character(1))

  assignee_html <- if (!is.null(assignee) && nzchar(assignee)) {
    sprintf('<span class="assignee">%s</span>', .cn_escape(assignee))
  } else ""

  paste0(
    '<div class="status-card">',
    '<div class="card-head"><span class="card-title">', .cn_escape(title),
    '</span>', assignee_html, '</div>',
    '<div class="card-meta"><span class="pill ', overall_pill, '">',
    sprintf("%.1f%% treated", overall_pct), '</span></div>',
    '<div class="table-wrap"><table class="status-table">',
    '<thead><tr><th>Township</th><th class="num">Sites</th>',
    '<th class="num">Treated</th><th class="num">% Treated</th></tr></thead>',
    '<tbody>', paste0(rows, collapse = ""), '</tbody></table></div>',
    '</div>'
  )
}

#' Catch basin Treated / Expiring tiles (from load_fos_catch_basin one-row df).
.cn_catch_basin_section <- function(cb_data) {
  if (is.null(cb_data) || nrow(cb_data) == 0) return("")
  total    <- as.integer(sum(cb_data$total,    na.rm = TRUE))
  active   <- as.integer(sum(cb_data$active,   na.rm = TRUE))
  expiring <- as.integer(sum(cb_data$expiring, na.rm = TRUE))
  if (total == 0) return("")
  treated_pct <- round(100 * active / total, 1)

  paste0(
    '<div class="status-card">',
    '<div class="card-head"><span class="card-title">Catch Basins</span>',
    sprintf('<span class="assignee">%d total sites</span>', total), '</div>',
    '<div class="tiles">',
    sprintf(paste0('<div class="tile"><div class="tile-num %s">%.1f%%</div>',
                   '<div class="tile-lbl">Treated</div>',
                   '<div class="tile-sub">%d / %d</div></div>'),
            .cn_pct_pill(treated_pct), treated_pct, active, total),
    sprintf(paste0('<div class="tile"><div class="tile-num %s">%d</div>',
                   '<div class="tile-lbl">Expiring</div>',
                   '<div class="tile-sub">of %d</div></div>'),
            if (expiring > 0) "crit" else "muted", expiring, total),
    '</div></div>'
  )
}

#' SUCO facility progress + per-FOS table (from load_fos_suco df).
.cn_suco_section <- function(suco_data, facility, suco_goal = 12L,
                             highlight_fos = NULL) {
  if (is.null(suco_data) || nrow(suco_data) == 0) return("")
  fac_total <- as.integer(sum(suco_data$active, na.rm = TRUE))
  pct <- as.integer(min(100, round(100 * fac_total / max(suco_goal, 1))))
  bar_class <- if (pct >= 100) "done" else if (pct >= 50) "warn" else "info"

  suco_data <- suco_data[order(-suco_data$active), ]
  rows <- vapply(seq_len(nrow(suco_data)), function(i) {
    r <- suco_data[i, ]
    nm <- if (!is.null(r$display_name) && !is.na(r$display_name) &&
              nzchar(as.character(r$display_name))) r$display_name else r$fos
    is_me <- !is.null(highlight_fos) && as.character(r$fos) == highlight_fos
    sprintf('<tr%s><td>%s</td><td class="num">%d</td></tr>',
            if (isTRUE(is_me)) ' class="me"' else "",
            .cn_escape(nm), as.integer(r$active))
  }, character(1))

  paste0(
    '<div class="status-card">',
    '<div class="card-head"><span class="card-title">SUCO Goal</span>',
    sprintf('<span class="assignee">%s: %d / %d</span>', .cn_escape(facility),
            fac_total, suco_goal), '</div>',
    '<div class="bar"><span class="', bar_class, '" style="width:', pct,
    '%;"></span></div>',
    sprintf('<div class="bar-lbl">%d%% of weekly goal</div>', pct),
    '<div class="table-wrap"><table class="status-table"><thead><tr>',
    '<th>FOS</th><th class="num">SUCOs</th></tr></thead><tbody>',
    paste0(rows, collapse = ""), '</tbody></table></div></div>'
  )
}

# --- main document builder ---------------------------------------------------

#' Build the full standalone crew-note HTML document.
#'
#' @param fos_display    Supervisor display name (e.g. "Alex D")
#' @param facility       Facility code (e.g. "Sr")
#' @param days           Character vector of day labels, in order (e.g.
#'                       c("Thursday Aug 21","Friday Aug 22"))
#' @param crew           Data frame: emp_num, shortname (the roster)
#' @param tasks          Named list keyed "<emp_num>_<dayIdx>" -> task text;
#'                       plus "<emp_num>_off" -> logical (day off / vacation)
#' @param crew_notes     Free-text crew-wide notes (may contain newlines)
#' @param priority_ladder Character vector of priority lines (top = highest)
#' @param status         Named list with elements: prehatch, structures,
#'                       catch_basin, suco (any may be NULL / empty)
#' @param week_label     Header date-range label (e.g. "Aug 21 – 22, 2026")
#' @param suco_goal      Weekly SUCO goal per facility (default 12)
#' @return A single HTML string (complete <!DOCTYPE html> document)
build_crew_note_html <- function(fos_display, facility, days, crew, tasks,
                                  crew_notes, priority_ladder, status,
                                  week_label = "", suco_goal = 12L) {

  if (is.null(days) || length(days) == 0) days <- character(0)
  if (is.null(crew)) crew <- data.frame(emp_num = character(),
                                        shortname = character())
  tasks <- if (is.null(tasks)) list() else tasks

  # --- Priority ladder (optional — priorities are never fixed) ---
  pl <- if (is.null(priority_ladder)) character(0) else priority_ladder
  pl <- pl[!is.na(pl) & nzchar(trimws(pl))]
  priority_block <- ""
  footer_priority <- ""
  if (length(pl) > 0) {
    ladder_rungs <- paste0(vapply(seq_along(pl), function(i) {
      sprintf(paste0('<div class="rung"><div class="n">%d</div>',
                     '<div class="t">%s</div></div>'),
              i, .cn_escape(pl[i]))
    }, character(1)), collapse = "")
    priority_block <- paste0(
      '<div class="priority">',
      '<h3>Priority Order — All Crew</h3>',
      '<div class="ladder">', ladder_rungs, '</div>',
      '<p class="heat-note">Work the <b>top priorities first thing in the ',
      'morning</b> while it\'s cool; shift down the list as heat and ',
      'conditions require.</p>',
      '</div>')
    footer_priority <- paste0("Priority: ",
                              paste0(.cn_escape(pl), collapse = " &rarr; "))
  }

  # --- Assignments table: one column per day, one row per crew member ---
  day_headers <- paste0(vapply(days, function(d) {
    sprintf('<th>%s</th>', .cn_escape(d))
  }, character(1)), collapse = "")

  crew_rows <- if (nrow(crew) > 0) {
    paste0(vapply(seq_len(nrow(crew)), function(i) {
      emp <- as.character(crew$emp_num[i])
      nm  <- crew$shortname[i]
      is_off <- isTRUE(tasks[[paste0(emp, "_off")]])
      day_cells <- paste0(vapply(seq_along(days), function(d) {
        key <- paste0(emp, "_", d)
        val <- tasks[[key]]
        sprintf('<td class="task">%s</td>', .cn_multiline(val))
      }, character(1)), collapse = "")
      sprintf('<tr%s><td class="who">%s</td>%s</tr>',
              if (is_off) ' class="off"' else "",
              .cn_escape(nm), day_cells)
    }, character(1)), collapse = "")
  } else {
    sprintf('<tr><td class="who">—</td><td class="task" colspan="%d">%s</td></tr>',
            max(length(days), 1),
            "No crew found for this supervisor. Add notes below.")
  }

  assignments_html <- paste0(
    '<h2 class="section">Assignments</h2>',
    '<div class="table-wrap"><table class="assign"><thead><tr><th>Crew</th>',
    day_headers, '</tr></thead><tbody>', crew_rows, '</tbody></table></div>'
  )

  # --- Status sections (full) ---
  status <- if (is.null(status)) list() else status
  status_blocks <- paste0(
    .cn_township_section(status$prehatch %||% list(summary = data.frame()),
                         "Ground Prehatch"),
    .cn_township_section(status$structures %||% list(summary = data.frame()),
                         "Structures"),
    .cn_catch_basin_section(status$catch_basin),
    .cn_suco_section(status$suco, facility, suco_goal,
                     highlight_fos = status$fos_emp_num)
  )
  status_html <- if (nzchar(status_blocks)) {
    paste0('<h2 class="section">Status — What\'s Left</h2>',
           '<div class="status-grid">', status_blocks, '</div>')
  } else ""

  # --- Crew notes ---
  notes_html <- if (!is.null(crew_notes) && nzchar(trimws(crew_notes))) {
    paste0('<h2 class="section">Crew Notes</h2>',
           '<div class="notes">', .cn_multiline(crew_notes), '</div>')
  } else ""

  header_dates <- if (nzchar(week_label)) {
    sprintf('<div class="dates">%s</div>', .cn_escape(week_label))
  } else ""

  # --- Assemble document ---
  paste0(
'<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Crew Instructions — ', .cn_escape(fos_display), '</title>
<style>', .cn_stylesheet(), '</style>
</head>
<body>
<div class="toolbar"><button class="print-btn" onclick="window.print()">Print / Save PDF</button></div>
<div class="sheet">
  <header class="masthead">
    <div>
      <p class="eyebrow">Field Crew Instructions</p>
      <h1>', .cn_escape(fos_display), ' &middot; ', .cn_escape(facility), '</h1>
    </div>', header_dates, '
  </header>

  ', priority_block, '

  ', assignments_html, '
  ', status_html, '
  ', notes_html, '

  <footer class="foot">
    <span>MMCD &middot; ', .cn_escape(facility), ' &middot; ', .cn_escape(fos_display), '</span>
    <span>', footer_priority, '</span>
  </footer>
</div>
</body>
</html>'
  )
}

# NULL-coalescing helper (base R has none).
`%||%` <- function(a, b) if (is.null(a)) b else a

# --- the inlined print stylesheet (pine-green field report) ------------------

.cn_stylesheet <- function() {
'
  :root{
    --paper:#fff;--panel:#f6f7f4;--ink:#1b2320;--muted:#5c6a63;--faint:#8a978f;
    --rule:#d9ded8;--rule-strong:#b9c2ba;--pine:#1f5a49;--pine-soft:#e7efe9;
    --done:#2e7d5b;--done-bg:#e6f2ea;--warn:#a9691a;--warn-bg:#fbeed8;
    --crit:#a63a2c;--crit-bg:#f7e2dd;--info:#3b6ea5;--off:#6b7480;
  }
  *{box-sizing:border-box;}
  body{margin:0;background:var(--panel);color:var(--ink);line-height:1.5;
    font-family:"Segoe UI",-apple-system,BlinkMacSystemFont,"Helvetica Neue",Arial,sans-serif;
    -webkit-font-smoothing:antialiased;}
  .sheet{max-width:940px;margin:0 auto;background:var(--paper);padding:40px 48px 56px;}
  .toolbar{max-width:940px;margin:20px auto 0;padding:0 48px;display:flex;justify-content:flex-end;}
  .print-btn{font:inherit;font-size:13px;font-weight:600;letter-spacing:.02em;color:#fff;
    background:var(--pine);border:none;border-radius:6px;padding:9px 18px;cursor:pointer;}
  .print-btn:hover{background:#17493b;}
  .masthead{display:flex;justify-content:space-between;align-items:flex-end;gap:24px;
    border-bottom:3px solid var(--pine);padding-bottom:16px;}
  .masthead .eyebrow{font-size:11px;font-weight:700;letter-spacing:.18em;text-transform:uppercase;
    color:var(--pine);margin:0 0 6px;}
  .masthead h1{font-size:28px;line-height:1.1;margin:0;letter-spacing:-.01em;}
  .masthead .dates{text-align:right;font-size:13px;color:var(--muted);white-space:nowrap;}
  h2.section{font-size:12px;font-weight:700;letter-spacing:.16em;text-transform:uppercase;
    color:var(--pine);margin:36px 0 14px;padding-bottom:6px;border-bottom:1px solid var(--rule);}
  .priority{margin-top:24px;background:var(--pine-soft);border:1px solid #cfe0d6;
    border-radius:10px;padding:16px 20px;}
  .priority h3{margin:0 0 12px;font-size:12px;letter-spacing:.14em;text-transform:uppercase;color:var(--pine);}
  .ladder{display:flex;flex-wrap:wrap;gap:10px;}
  .rung{flex:1 1 180px;display:flex;gap:12px;align-items:center;background:#fff;
    border:1px solid #cfe0d6;border-radius:8px;padding:10px 14px;}
  .rung .n{flex:none;width:24px;height:24px;border-radius:50%;background:var(--pine);color:#fff;
    font-size:13px;font-weight:700;display:grid;place-items:center;}
  .rung .t{font-weight:700;font-size:14px;}
  .heat-note{margin:12px 0 0;font-size:13px;}
  .heat-note b{color:var(--pine);}
  .table-wrap{overflow-x:auto;}
  table.assign{width:100%;border-collapse:collapse;font-size:13.5px;}
  table.assign th{text-align:left;font-size:10.5px;letter-spacing:.08em;text-transform:uppercase;
    color:var(--faint);border-bottom:1px solid var(--rule-strong);padding:0 10px 6px 0;vertical-align:bottom;}
  table.assign td{vertical-align:top;padding:10px 10px 10px 0;border-bottom:1px solid var(--rule);}
  table.assign td.who{font-weight:700;white-space:nowrap;width:90px;}
  table.assign td.task{color:var(--ink);min-width:150px;}
  .assign tr.off td.who{color:var(--off);}
  .assign tr.off td.task{color:var(--muted);font-style:italic;}
  .status-grid{display:grid;grid-template-columns:repeat(2,1fr);gap:16px;}
  .status-card{border:1px solid var(--rule);border-radius:10px;padding:14px 16px 16px;background:#fff;
    box-shadow:inset 4px 0 0 var(--pine);break-inside:avoid;}
  .card-head{display:flex;justify-content:space-between;align-items:baseline;gap:10px;margin-bottom:6px;}
  .card-title{font-size:16px;font-weight:700;}
  .assignee{font-size:11px;color:var(--muted);font-weight:600;}
  .card-meta{margin-bottom:10px;}
  .pill{font-size:11px;font-weight:700;padding:2px 9px;border-radius:999px;}
  .pill.done{background:var(--done-bg);color:var(--done);}
  .pill.warn{background:var(--warn-bg);color:var(--warn);}
  .pill.crit{background:var(--crit-bg);color:var(--crit);}
  table.status-table{width:100%;border-collapse:collapse;font-size:12.5px;font-variant-numeric:tabular-nums;}
  table.status-table th{text-align:left;font-size:10px;letter-spacing:.06em;text-transform:uppercase;
    color:var(--faint);border-bottom:1px solid var(--rule-strong);padding:0 6px 5px 0;}
  table.status-table td{padding:5px 6px 5px 0;border-bottom:1px solid var(--rule);vertical-align:top;}
  table.status-table td.num,table.status-table th.num{text-align:right;white-space:nowrap;}
  td.town-name{font-weight:500;}
  .dot{display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:5px;vertical-align:middle;}
  .dot.done{background:var(--done);}.dot.warn{background:var(--warn);}.dot.crit{background:var(--crit);}
  details{margin-top:3px;}
  summary{cursor:pointer;color:var(--pine);font-size:11px;font-weight:600;}
  .chipwrap{margin-top:6px;}
  .sect{margin-bottom:7px;}
  .sect .pre{font-size:10.5px;font-weight:700;color:var(--muted);margin-bottom:3px;}
  .chips{display:flex;flex-wrap:wrap;gap:4px;}
  .chip{font-family:"Consolas",ui-monospace,monospace;font-size:11px;font-variant-numeric:tabular-nums;
    background:var(--crit-bg);color:var(--crit);border:1px solid #e6c3bb;border-radius:5px;padding:1px 6px;white-space:nowrap;}
  .tiles{display:flex;gap:10px;flex-wrap:wrap;}
  .tile{flex:1 1 120px;text-align:center;background:var(--panel);border:1px solid var(--rule);border-radius:8px;padding:12px;}
  .tile-num{font-size:1.7em;font-weight:800;font-variant-numeric:tabular-nums;}
  .tile-num.done{color:var(--done);}.tile-num.warn{color:var(--warn);}.tile-num.crit{color:var(--crit);}.tile-num.muted{color:var(--faint);}
  .tile-lbl{font-size:12px;color:var(--muted);margin-top:4px;}
  .tile-sub{font-size:11px;color:var(--faint);margin-top:2px;}
  .bar{height:14px;background:var(--rule);border-radius:7px;overflow:hidden;max-width:360px;}
  .bar>span{display:block;height:100%;border-radius:7px;}
  .bar>span.done{background:var(--done);}.bar>span.warn{background:var(--warn);}.bar>span.info{background:var(--info);}
  .bar-lbl{font-size:11px;color:var(--muted);margin:4px 0 10px;}
  table.status-table tr.me{font-weight:700;color:var(--pine);}
  .notes{background:var(--panel);border:1px solid var(--rule);border-left:5px solid var(--pine);
    border-radius:8px;padding:14px 18px;font-size:13.5px;line-height:1.6;white-space:normal;}
  footer.foot{margin-top:34px;padding-top:12px;border-top:1px solid var(--rule);font-size:11.5px;
    color:var(--faint);display:flex;justify-content:space-between;flex-wrap:wrap;gap:8px;}
  @media (max-width:720px){
    .sheet{padding:28px 22px 40px;}
    .status-grid{grid-template-columns:1fr;}
    .masthead{flex-direction:column;align-items:flex-start;gap:10px;}
    .masthead .dates{text-align:left;}
  }
  @page{size:letter portrait;margin:0.5in;}
  @media print{
    body{background:#fff;}
    .toolbar{display:none;}
    .sheet{max-width:none;margin:0;padding:0;}
    details{display:none;}
    .status-grid{break-inside:auto;}
    .status-card{break-inside:avoid;}
    *{-webkit-print-color-adjust:exact;print-color-adjust:exact;}
  }
'
}
