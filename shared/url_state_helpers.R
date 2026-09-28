# =============================================================================
# MMCD METRICS - SHARED URL STATE HELPERS
# =============================================================================
# Deep-linking + auto-refresh engine shared by every app.
#
# Goal: a link like
#   /ground_prehatch_progress/?tabs=historical&facility=Sr&hist_display_metric=acres&autorefresh=true
# lands directly on that view with filters pre-applied and (optionally) keeps
# itself current -- with NO manual filter interaction and NO manual Refresh click.
#
# Why this is not just "read the query string and set inputs":
#   1. Most apps gate their charts behind a manual Refresh button
#      (eventReactive(input$refresh, ...) + req(input$refresh)). Setting inputs
#      from the URL renders nothing until that button is (really) clicked -- so
#      after applying inputs we must also programmatically click Refresh.
#   2. Some inputs (e.g. facility_filter) get their CHOICES populated async by a
#      separate observe() on load, and some inputs reset each other
#      (e.g. hist_time_period resets hist_display_metric). Applying too early
#      loses the race.
# Both are solved by driving the apply from the FIRST `shiny:idle` (client-side),
# i.e. after the initial flush -- choices are populated, resets have fired -- then
# firing Refresh on the NEXT idle, once our input updates have round-tripped.
#
# This file has no DB/network dependencies and is safe to source into any app.
# It intentionally does NOT define `%||%` (apps define their own); it uses
# explicit NULL checks so it can't clash.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. READING URL STATE
# -----------------------------------------------------------------------------

#' Parse the current session's query string into a named list.
#' @param session Shiny session
#' @return named list of character values ("" params omitted), empty list if none
#' @export
read_url_state <- function(session) {
  qs <- tryCatch(session$clientData$url_search, error = function(e) "")
  if (is.null(qs) || !nzchar(qs)) return(list())
  as.list(shiny::parseQueryString(qs))
}

#' Parse a raw query string (e.g. the value sent by url_state_js()).
#' @param raw Character query string, with or without leading "?"
#' @return named list of character values
#' @export
parse_url_state <- function(raw) {
  if (is.null(raw) || !nzchar(raw)) return(list())
  as.list(shiny::parseQueryString(raw))
}

#' Is auto-refresh requested? TRUE only for explicit truthy values.
#' @param state named list from read_url_state()/parse_url_state()
#' @export
autorefresh_enabled <- function(state) {
  v <- state[["autorefresh"]]
  if (is.null(v) || length(v) == 0) return(FALSE)
  tolower(as.character(v)[1]) %in% c("true", "1", "yes", "on")
}

# -----------------------------------------------------------------------------
# 2. APPLYING URL STATE TO INPUTS
# -----------------------------------------------------------------------------

.url_as_logical <- function(v) {
  tolower(as.character(v)[1]) %in% c("true", "1", "yes", "on")
}

#' Apply one input update by declared type (handles selected-vs-value differences).
.apply_one_input <- function(session, id, type, value) {
  switch(
    type,
    select       = shiny::updateSelectInput(session, id, selected = value),
    # selectize may be multiple=TRUE (e.g. suco_history "fos"): comma-split so
    # ?fos=1904,1905 selects both. A single value (no comma) is a length-1 vector.
    selectize    = shiny::updateSelectizeInput(
      session, id,
      selected = trimws(strsplit(as.character(value), ",")[[1]])
    ),
    radio        = shiny::updateRadioButtons(session, id, selected = value),
    tab          = shiny::updateTabsetPanel(session, id, selected = value),
    checkbox     = shiny::updateCheckboxInput(session, id, value = .url_as_logical(value)),
    numeric      = shiny::updateNumericInput(session, id, value = suppressWarnings(as.numeric(value))),
    slider       = shiny::updateSliderInput(session, id, value = suppressWarnings(as.numeric(value))),
    slider_range = shiny::updateSliderInput(
      session, id,
      value = suppressWarnings(as.numeric(trimws(strsplit(as.character(value), ",")[[1]])))
    ),
    date         = shiny::updateDateInput(session, id, value = as.Date(value)),
    daterange    = {
      parts <- trimws(strsplit(as.character(value), ",")[[1]])
      shiny::updateDateRangeInput(session, id,
        start = as.Date(parts[1]),
        end   = if (length(parts) > 1) as.Date(parts[2]) else NULL)
    },
    checkgroup   = shiny::updateCheckboxGroupInput(
      session, id,
      selected = trimws(strsplit(as.character(value), ",")[[1]])
    ),
    text         = shiny::updateTextInput(session, id, value = value),
    # default: treat as a select
    shiny::updateSelectInput(session, id, selected = value)
  )
}

#' Apply a parsed URL state to Shiny inputs using a declarative spec.
#'
#' @param session Shiny session
#' @param state   named list from parse_url_state()/read_url_state()
#' @param spec    named list keyed by URL param name. Each entry is a list:
#'   - input:     input id to update (default = the param name)
#'   - type:      one of "select","selectize","radio","tab","checkbox",
#'                "numeric","slider","slider_range","date","text"
#'                (default "select")
#'   - transform: optional function(rawValue) -> value applied before updating
#' @return character vector of param keys actually applied (invisibly)
#' @export
apply_url_state <- function(session, state, spec) {
  if (length(state) == 0 || length(spec) == 0) return(invisible(character(0)))
  applied <- character(0)
  for (key in names(spec)) {
    raw <- state[[key]]
    if (is.null(raw) || length(raw) == 0 || !nzchar(as.character(raw)[1])) next
    entry <- spec[[key]]
    input_id <- if (!is.null(entry$input)) entry$input else key
    type <- if (!is.null(entry$type)) entry$type else "select"
    value <- if (!is.null(entry$transform)) entry$transform(raw) else raw
    ok <- tryCatch({
      .apply_one_input(session, input_id, type, value)
      TRUE
    }, error = function(e) {
      warning(sprintf("[url_state] could not apply '%s' -> #%s: %s",
                      key, input_id, conditionMessage(e)))
      FALSE
    })
    if (isTRUE(ok)) applied <- c(applied, key)
  }
  invisible(applied)
}

# -----------------------------------------------------------------------------
# 3. TRIGGERING REFRESH / AUTO-REFRESH
# -----------------------------------------------------------------------------

#' Ask the client to click one or more Refresh actionButtons once inputs have
#' settled. Pass a vector to click a SEQUENCE (each on the next idle) -- needed by
#' two-stage apps like `inspections` (load_data -> analyze_X).
#' Pairs with the `mmcd_deeplink_refresh` handler in url_state_js().
#' @export
trigger_deeplink_refresh <- function(session, input_id) {
  session$sendCustomMessage("mmcd_deeplink_refresh",
                            list(input_ids = as.list(as.character(input_id))))
}

#' Keep a view current by re-clicking its Refresh button on an interval, unless
#' paused. Skips the immediate first tick (the initial load is handled by the
#' deep-link refresh). Honors a checkbox input `pause_input_id`.
#'
#' @param interval_ms refresh cadence (default 5 min). 0/NA disables.
#' @export
observe_autorefresh <- function(input, session, refresh_input_id,
                                enabled = FALSE, interval_ms = 300000,
                                pause_input_id = "mmcd_autorefresh_pause") {
  if (!isTRUE(enabled)) return(invisible(NULL))
  if (is.null(interval_ms) || is.na(interval_ms) || interval_ms <= 0) return(invisible(NULL))
  first <- TRUE
  shiny::observe({
    if (isTRUE(input[[pause_input_id]])) return()
    shiny::invalidateLater(interval_ms, session)
    if (first) { first <<- FALSE; return() }  # don't double-fire the initial load
    session$sendCustomMessage("mmcd_autorefresh_click", list(input_id = refresh_input_id))
  })
  invisible(NULL)
}

#' A small "Auto-refreshing every N min · Pause" bar (WCAG 2.2.2 pause control).
#' Render this via uiOutput only when auto-refresh is enabled.
#' @export
autorefresh_indicator_ui <- function(interval_ms = 300000,
                                     pause_input_id = "mmcd_autorefresh_pause") {
  mins <- round(interval_ms / 60000, 1)
  shiny::div(
    class = "mmcd-autorefresh-bar",
    style = paste0(
      "display:flex;align-items:center;gap:10px;margin-bottom:10px;padding:6px 12px;",
      "background:#eef3fb;border:1px solid #d0daec;border-radius:6px;",
      "color:#2c5aa0;font-weight:600;"),
    shiny::icon("sync"),
    shiny::span(sprintf("Auto-refreshing every %s min", mins)),
    shiny::div(
      style = "margin-left:auto;",
      shiny::checkboxInput(pause_input_id, "Pause", value = FALSE)
    )
  )
}

# -----------------------------------------------------------------------------
# 4. BUILDING SHAREABLE LINKS
# -----------------------------------------------------------------------------

#' Build a deep-link URL from a base path + params (skips empty/NULL params).
#' Multi-value params are joined with commas. Keys and values are URL-encoded.
#' @export
build_deep_link_url <- function(base_path, params) {
  if (length(params) == 0) return(base_path)
  keep <- vapply(params, function(v) {
    !(is.null(v) || length(v) == 0 || !nzchar(paste(as.character(v), collapse = "")))
  }, logical(1))
  params <- params[keep]
  if (length(params) == 0) return(base_path)
  pieces <- vapply(names(params), function(k) {
    v <- paste(as.character(params[[k]]), collapse = ",")
    paste0(utils::URLencode(k, reserved = TRUE), "=", utils::URLencode(v, reserved = TRUE))
  }, character(1))
  paste0(base_path, "?", paste(pieces, collapse = "&"))
}

#' Copy a URL to the viewer's clipboard (pairs with url_state_js()).
#' @export
send_copy_link <- function(session, url) {
  session$sendCustomMessage("mmcd_copy_link", list(url = url))
}

#' One-call deep-link + auto-refresh wiring for an app's server().
#'
#' Reads ?params once (after the client settles), applies them to inputs via
#' `spec`, clicks the right Refresh button, and -- when ?autorefresh=true --
#' injects the pause bar and starts the interval. Keeps each app's diff to a
#' single server call plus one `url_state_js()` in its UI.
#'
#' @param spec        see apply_url_state()
#' @param refresh_for function(state) -> the actionButton id to click for this
#'   view (default always "refresh"); use it to pick e.g. "hist_refresh" when
#'   ?tabs=historical.
#' @param autorefresh_interval_ms cadence for ?autorefresh=true (default 5 min)
#' @export
wire_deep_links <- function(input, output, session, spec,
                            refresh_for = function(state) "refresh",
                            autorefresh_interval_ms = 300000) {
  applied_flag <- shiny::reactiveVal(FALSE)
  reapplied_flag <- shiny::reactiveVal(FALSE)
  saved_state <- shiny::reactiveVal(NULL)

  # PASS 1: apply inputs, then ask the client to tell us once the cascade has
  # settled. We do NOT click Refresh yet -- setting a parent input (e.g.
  # facility_filter) can trigger the app's own observers to RESET a dependent
  # input (e.g. foreman_filter/"fos" back to "all") after our value round-trips,
  # which would clobber the deep-linked value and be captured by Refresh.
  shiny::observeEvent(input$mmcd_url_state_raw, {
    if (isTRUE(applied_flag())) return()
    applied_flag(TRUE)
    state <- parse_url_state(input$mmcd_url_state_raw)
    saved_state(state)
    apply_url_state(session, state, spec)
    session$sendCustomMessage("mmcd_deeplink_reapply", list())
  })

  # PASS 2: after the client reports idle (cross-input resets have fired),
  # re-apply the SAME state so dependent inputs win, THEN click Refresh so it
  # captures the deep-linked values -- not the reset defaults. Re-applying is
  # idempotent for apps without cross-resets (a parent input unchanged since
  # pass 1 fires no new cascade), so this is safe for every wired app.
  finalize <- function() {
    if (isTRUE(reapplied_flag())) return()
    reapplied_flag(TRUE)
    state <- saved_state()
    if (is.null(state)) state <- list()
    apply_url_state(session, state, spec)
    refresh_id <- tryCatch(refresh_for(state), error = function(e) "refresh")
    # Drop empties: refresh_for may deliberately return character(0) for an
    # auto-updating app (apply inputs, no button to click).
    refresh_id <- refresh_id[!is.na(refresh_id) & nzchar(refresh_id)]
    if (length(refresh_id) > 0) trigger_deeplink_refresh(session, refresh_id)
    if (autorefresh_enabled(state) && length(refresh_id) > 0) {
      shiny::insertUI("body", where = "afterBegin",
                      ui = autorefresh_indicator_ui(autorefresh_interval_ms),
                      immediate = TRUE)
      # For a multi-button sequence, the last button is the one to re-fire on
      # the interval (earlier stages, e.g. load_data, only need to run once).
      observe_autorefresh(input, session, refresh_id[length(refresh_id)],
                          enabled = TRUE, interval_ms = autorefresh_interval_ms)
    }
  }
  shiny::observeEvent(input$mmcd_url_state_reapply, { finalize() })
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# 5. CLIENT-SIDE GLUE (include once per app UI)
# -----------------------------------------------------------------------------

#' JS that: (a) on the first `shiny:idle` sends the raw query string to the server
#' as input `mmcd_url_state_raw` (only if there are params), (b) clicks a Refresh
#' button once inputs have settled (`mmcd_deeplink_refresh`), (c) clicks a button
#' immediately for auto-refresh ticks (`mmcd_autorefresh_click`), and (d) copies a
#' URL to the clipboard (`mmcd_copy_link`).
#' @export
url_state_js <- function() {
  shiny::tags$script(shiny::HTML("
    (function() {
      // (a) Send URL params to the server on the first idle after connect, so
      // the initial flush (choice population, cross-input resets) has completed.
      var sentUrlState = false;
      $(document).on('shiny:idle', function() {
        if (sentUrlState) return;
        sentUrlState = true;
        var qs = window.location.search;
        if (qs && qs.length > 1) {
          Shiny.setInputValue('mmcd_url_state_raw', qs, {priority: 'event'});
        }
      });

      // (b) Click one or more Refresh buttons after the input updates we applied
      // have round-tripped. A sequence (e.g. load_data then analyze_X) is clicked
      // one button per idle, so each stage finishes before the next fires.
      Shiny.addCustomMessageHandler('mmcd_deeplink_refresh', function(msg) {
        var ids = msg.input_ids || (msg.input_id ? [msg.input_id] : []);
        if (!ids.length) return;
        var i = 0, started = false;
        // A button can be a uiOutput that only renders once its tab is shown
        // (suspendWhenHidden), so retry a few times until it exists.
        function clickWithRetry(id, tries) {
          var el = document.getElementById(id);
          if (el) { el.click(); return; }
          if (tries > 0) setTimeout(function(){ clickWithRetry(id, tries - 1); }, 250);
        }
        function next() {
          if (i >= ids.length) return;
          var id = ids[i]; i++;
          clickWithRetry(id, 8);   // up to ~2s for a late-rendering button
          if (i < ids.length) {
            $(document).one('shiny:idle', function() { setTimeout(next, 120); });
          }
        }
        function start() { if (started) return; started = true; next(); }
        $(document).one('shiny:idle', function() { setTimeout(start, 80); });
        setTimeout(start, 1800); // fallback if idle already passed
      });

      // (b2) After pass-1 apply, wait for the client to go idle (so any
      // cross-input reset the app fires -- e.g. changing facility resets the
      // FOS/foreman filter to 'all' -- has completed), then tell the server to
      // re-apply the URL state and click Refresh. This makes the deep-linked
      // value win over the reset default.
      Shiny.addCustomMessageHandler('mmcd_deeplink_reapply', function(msg) {
        var done = false;
        function fire() {
          if (done) return; done = true;
          Shiny.setInputValue('mmcd_url_state_reapply', Date.now(), {priority: 'event'});
        }
        $(document).one('shiny:idle', function() { setTimeout(fire, 120); });
        setTimeout(fire, 1500); // fallback if idle already passed
      });

      // (c) Auto-refresh tick: click the Refresh button now.
      Shiny.addCustomMessageHandler('mmcd_autorefresh_click', function(msg) {
        var el = document.getElementById(msg.input_id);
        if (el) el.click();
      });

      // (d) Copy a shareable link to the clipboard.
      Shiny.addCustomMessageHandler('mmcd_copy_link', function(msg) {
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(msg.url);
        }
      });
    })();
  "))
}
