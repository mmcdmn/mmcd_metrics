# =============================================================================
# TEST: Accessibility (ADA / WCAG 2.1 AA) conformance across ALL apps
# =============================================================================
# Every user-facing app must use the shared accessibility layer:
#   shared/accessibility_helpers.R
#   shared/assets/accessibility.css
#   shared/assets/accessibility.js
#
# Apps are discovered automatically from apps/*/app.R, so a NEW app is checked
# the moment its folder exists - nobody has to remember to add it here.
#
# If a test fails, THE APP MUST BE CHANGED. See shared/SHARED_RESOURCES.md,
# "Quick Start - Accessibility", for the one-line fix for each rule.
#
# Static rules read the code through R's parser (getParseData), not regexes,
# so strings and comments ("#55606B", "# fluidPage(") never trigger them.
# =============================================================================

suppressPackageStartupMessages(library(shiny))  # stat_box_helpers.R uses tags/div unqualified

get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

root <- get_project_root()

# Folders under apps/ that are not interactive Shiny pages.
A11Y_EXCLUDED_APPS <- c(
  "refresh_views",  # headless cron utility; no person ever uses its UI
  "about"           # static HTML page; checked in its own test below
)

#' Every Shiny app folder, with the folder whose app.R actually runs.
#' overview's runnable app lives one level down, in overview/unified/.
discover_apps <- function(root) {
  dirs <- setdiff(list.dirs(file.path(root, "apps"), recursive = FALSE, full.names = FALSE),
                  A11Y_EXCLUDED_APPS)
  runnable <- vapply(dirs, function(d) {
    if (file.exists(file.path(root, "apps", d, "app.R"))) return(file.path(root, "apps", d))
    if (file.exists(file.path(root, "apps", d, "unified", "app.R"))) {
      return(file.path(root, "apps", d, "unified"))
    }
    NA_character_
  }, character(1))
  runnable[!is.na(runnable)]
}

APPS <- discover_apps(root)

#' All R source files belonging to an app (its whole folder tree).
app_r_files <- function(app) {
  top <- file.path(root, "apps", app)
  list.files(top, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
}

#' Parse data for one file; NULL if the file does not parse (other tests
#' cover syntax, and this suite should report accessibility problems only).
parse_data <- function(path) {
  tryCatch(getParseData(parse(path, keep.source = TRUE)), error = function(e) NULL)
}

#' File text with HTML comments removed, so a comment that *explains* a rule
#' ("no <meta http-equiv=refresh> here because ...") cannot trip it.
read_code <- function(path) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  gsub("(?s)<!--.*?-->", "", text, perl = TRUE)
}

#' Function calls named `fns` in a file, as "file:line call()" strings.
find_calls <- function(path, fns) {
  pd <- parse_data(path)
  if (is.null(pd)) return(character(0))
  hits <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% fns, ]
  if (nrow(hits) == 0) return(character(0))
  paste0(basename(path), ":", hits$line1, " ", hits$text, "()")
}


# =============================================================================
# DISCOVERY
# =============================================================================

test_that("app discovery finds the apps", {
  # Guards against the suite silently checking nothing.
  expect_gt(length(APPS), 10)
  expect_true("overview" %in% names(APPS), info = "overview/unified must be discovered")
})


# =============================================================================
# PAGE SHELL - skip link, landmarks, lang, contrast, focus, shared JS
# =============================================================================

test_that("every app sources the shared accessibility layer", {
  for (app in names(APPS)) {
    app_r <- paste(readLines(file.path(APPS[[app]], "app.R"), warn = FALSE), collapse = "\n")
    expect_true(grepl("accessibility_helpers.R", app_r, fixed = TRUE),
                info = paste0(app, ": app.R must source(\"../../shared/accessibility_helpers.R\")"))
  }
})

test_that("every app builds its page with accessible_page() / accessible_dashboard_page()", {
  bare <- c("fluidPage", "fixedPage", "fillPage", "bootstrapPage", "navbarPage", "dashboardPage")
  for (app in names(APPS)) {
    files <- app_r_files(app)
    wrapped <- unlist(lapply(files, find_calls,
                             fns = c("accessible_page", "accessible_dashboard_page")))
    expect_true(length(wrapped) > 0,
                info = paste0(app, ": no accessible_page()/accessible_dashboard_page() call found"))
    raw <- unlist(lapply(files, find_calls, fns = bare))
    expect_equal(length(raw), 0,
                 info = paste0(app, ": replace with accessible_page()/accessible_dashboard_page(): ",
                               paste(raw, collapse = ", ")))
  }
})

test_that("no app gives the page two main landmarks", {
  # accessible_main_panel() sets id = "main-content"; a second one duplicates it.
  for (app in names(APPS)) {
    uses <- unlist(lapply(app_r_files(app), find_calls, fns = "accessible_main_panel"))
    expect_true(length(uses) <= 1,
                info = paste0(app, ": use accessible_main_panel() once; keep other panels as ",
                              "mainPanel(): ", paste(uses, collapse = ", ")))
  }
})


# =============================================================================
# CONTENT RULES
# =============================================================================

#' Shiny input functions whose second argument is the label.
LABELLED_INPUTS <- c(
  "textInput", "textAreaInput", "numericInput", "passwordInput", "fileInput",
  "selectInput", "selectizeInput", "varSelectInput", "checkboxInput",
  "checkboxGroupInput", "radioButtons", "dateInput", "dateRangeInput", "sliderInput"
)

#' Positional arguments of a call, in order; NA for the named ones.
#' Used to catch checkboxInput("id", NULL) - a label that is missing without
#' ever writing `label =`.
positional_args <- function(pd, call_id) {
  kids <- pd[pd$parent == call_id, ]
  if (nrow(kids) < 2) return(character(0))
  kids <- kids[order(kids$line1, kids$col1), ][-1, ]        # drop the function name
  kids <- kids[!(kids$token %in% c("'('", "')'")), ]
  args <- character(0); current <- character(0); named <- FALSE
  flush <- function() if (named) NA_character_ else paste(current, collapse = "")
  for (i in seq_len(nrow(kids))) {
    token <- kids[i, ]
    if (token$token == "','") {
      args <- c(args, flush()); current <- character(0); named <- FALSE; next
    }
    if (token$token == "SYMBOL_SUB") named <- TRUE
    current <- c(current, getParseText(pd, token$id))
  }
  c(args, flush())
}

test_that("no input is created with a positionally empty label", {
  for (app in names(APPS)) {
    for (path in app_r_files(app)) {
      pd <- parse_data(path)
      if (is.null(pd)) next
      calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% LABELLED_INPUTS, ]
      for (k in seq_len(nrow(calls))) {
        call_id <- pd$parent[pd$id == calls$parent[k]]
        args <- positional_args(pd, call_id)
        if (length(args) < 2 || is.na(args[2])) next
        expect_false(trimws(args[2]) %in% c("NULL", "\"\"", "''"),
                     info = paste0(app, ": ", basename(path), ":", calls$line1[k], " ",
                                   calls$text[k], "() has no label - pass ",
                                   "tags$span(class = \"sr-only\", \"...\") to hide it visually"))
      }
    }
  }
})

test_that("every input has a label (WCAG 1.3.1 / 4.1.2)", {
  # label = NULL / "" leaves the control with no accessible name. To hide a
  # label visually, keep it: label = tags$span(class = "sr-only", "...").
  for (app in names(APPS)) {
    for (path in app_r_files(app)) {
      pd <- parse_data(path)
      if (is.null(pd)) next
      terms <- pd[pd$terminal, ]
      terms <- terms[order(terms$line1, terms$col1), ]
      at <- which(terms$token == "SYMBOL_SUB" & terms$text == "label")
      for (i in at) {
        value <- terms[i + 2, ]  # label, =, value
        empty <- value$token == "NULL_CONST" ||
                 (value$token == "STR_CONST" && value$text %in% c("\"\"", "''"))
        expect_false(empty, info = paste0(app, ": ", basename(path), ":", terms$line1[i],
                                          " has label = ", value$text))
      }
    }
  }
})

test_that("every image has alt text (WCAG 1.1.1)", {
  # Use alt = "" when the image is decorative (e.g. an icon beside visible text).
  for (app in names(APPS)) {
    for (path in app_r_files(app)) {
      pd <- parse_data(path)
      if (is.null(pd)) next
      calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "img", ]
      for (k in seq_len(nrow(calls))) {
        call_id <- pd$parent[pd$id == calls$parent[k]]
        call_text <- getParseText(pd, call_id)
        expect_true(grepl("\\balt\\s*=", call_text),
                    info = paste0(app, ": ", basename(path), ":", calls$line1[k], " img() has no alt"))
      }
    }
  }
})

test_that("no page refreshes itself (WCAG 2.2.1)", {
  # Auto-refresh must update in place (invalidateLater) with a pause control,
  # never reload the page - see "Display mode" in SHARED_RESOURCES.md.
  scan <- c(list.files(file.path(root, "apps"), pattern = "\\.(R|js|html)$",
                       recursive = TRUE, full.names = TRUE),
            list.files(file.path(root, "shared"), pattern = "\\.(R|js|html)$",
                       recursive = TRUE, full.names = TRUE),
            file.path(root, "index.html"))
  for (path in scan) {
    text <- read_code(path)
    expect_false(grepl("http-equiv\\s*=\\s*[\"']refresh", text, ignore.case = TRUE),
                 info = paste0(path, ": <meta http-equiv=\"refresh\"> is not allowed"))
    if (grepl("/apps/", path, fixed = TRUE)) {
      expect_false(grepl("location.reload(", text, fixed = TRUE),
                   info = paste0(path, ": location.reload() is not allowed in apps"))
    }
  }
})

test_that("no link is used as a button", {
  # <a href="#" onclick=...> announces as a link and reports no state; use
  # a11y_disclosure() for show/hide, or actionButton() for actions.
  for (app in names(APPS)) {
    for (path in app_r_files(app)) {
      text <- paste(readLines(path, warn = FALSE), collapse = "\n")
      expect_false(grepl("href\\s*=\\s*\"#\"\\s*,\\s*onclick", text),
                   info = paste0(app, ": ", basename(path), " uses a link as a button"))
    }
  }
})


# =============================================================================
# SHARED LAYER - the primitives every app relies on
# =============================================================================

source(file.path(root, "shared", "accessibility_helpers.R"))
source(file.path(root, "shared", "stat_box_helpers.R"))

render_body <- function(ui) paste(htmltools::renderTags(ui)$html, collapse = "\n")
count_of <- function(text, pattern) lengths(regmatches(text, gregexpr(pattern, text, fixed = TRUE)))

test_that("the shared CSS and JS are found from every app folder depth", {
  expect_true(file.exists(file.path(root, "shared", "assets", "accessibility.css")))
  expect_true(file.exists(file.path(root, "shared", "assets", "accessibility.js")))
  # Resolve exactly as a running app does: from apps/<app>/ and apps/overview/unified/.
  # on.exit guarantees the working directory is restored even if a lookup
  # errors - leaving it moved would break every test file that runs after.
  start_dir <- getwd()
  on.exit(setwd(start_dir), add = TRUE)
  for (dir in c(file.path(root, "apps", "drone"), file.path(root, "apps", "overview", "unified"))) {
    setwd(dir)
    found <- c(.a11y_find_asset("accessibility.css"), .a11y_find_asset("accessibility.js"))
    setwd(start_dir)
    expect_equal(length(found), 2, info = paste("assets not found from", dir))
  }
})

test_that("accessible_page(): skip link first, one main landmark, title in a banner", {
  ui <- accessible_page(
    shiny::titlePanel("Test App"),
    shiny::sidebarLayout(shiny::sidebarPanel(shiny::selectInput("x", "Facility:", c("a", "b"))),
                         accessible_main_panel(shiny::plotOutput("p")))
  )
  body <- render_body(ui)
  expect_equal(count_of(body, 'role="main"'), 1)   # annotated, not nested
  expect_equal(count_of(body, "<main"), 0)
  expect_true(grepl('id="main-content"', body, fixed = TRUE))
  expect_true(regexpr("a11y-skip-link", body, fixed = TRUE) < regexpr("<header", body, fixed = TRUE))
  expect_true(grepl('<header class="a11y-banner">', body, fixed = TRUE))
  expect_true(grepl('<h1 class="h2">Test App</h1>', body, fixed = TRUE))
  expect_true(grepl('for="x"', body, fixed = TRUE))
})

test_that("accessible_dashboard_page(): skip link precedes the dashboard header", {
  skip_if_not_installed("shinydashboard")
  ui <- accessible_dashboard_page(
    shinydashboard::dashboardHeader(title = "T"),
    shinydashboard::dashboardSidebar(),
    shinydashboard::dashboardBody(shiny::h2("body"))
  )
  body <- render_body(ui)
  expect_true(regexpr("a11y-skip-link", body, fixed = TRUE) < regexpr("main-header", body, fixed = TRUE))
})

test_that("a11y_disclosure(): real button whose aria-controls resolves", {
  html <- as.character(a11y_disclosure("help", "Show/Hide Help", shiny::p("text")))
  expect_true(grepl('<button type="button" id="help"', html, fixed = TRUE))
  expect_true(grepl('aria-expanded="false"', html, fixed = TRUE))
  expect_true(grepl('aria-controls="help-region"', html, fixed = TRUE))
  expect_true(grepl('<div id="help-region" hidden>', html, fixed = TRUE))
})

test_that("a11y_figure(): authored accessible name", {
  html <- as.character(a11y_figure(shiny::plotOutput("c"), "Drone acres by week"))
  expect_true(grepl('data-a11y-label="Drone acres by week"', html, fixed = TRUE))
  expect_true(grepl('aria-label="Drone acres by week"', html, fixed = TRUE))
})

test_that("stat boxes: decorative icon, named info button, no nested controls", {
  img_box <- as.character(create_stat_box(value = "5", title = "X", bg_color = "#2c5aa0",
                                          icon = "a.png", icon_type = "image"))
  expect_true(grepl('alt=""', img_box, fixed = TRUE))

  clickable <- as.character(create_stat_box(value = "1", title = "Drone", bg_color = "#2c5aa0",
                                            content_attrs = stat_box_toggle_attrs()))
  expect_true(grepl('role="button" tabindex="0" data-a11y-toggle=".stat-box-clickable"',
                    clickable, fixed = TRUE))
})


# =============================================================================
# STATIC PAGES - served straight from disk, outside Shiny
# =============================================================================

test_that("static pages link the shared assets and have the page landmarks", {
  pages <- c(file.path(root, "index.html"), file.path(root, "apps", "about", "index.html"))
  for (page in pages) {
    html <- paste(readLines(page, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_true(grepl("shared/assets/accessibility.css", html, fixed = TRUE), info = page)
    expect_true(grepl("shared/assets/accessibility.js", html, fixed = TRUE), info = page)
    expect_true(grepl('<html lang="en"', html, fixed = TRUE), info = page)
    expect_equal(count_of(html, "<main"), 1, info = page)
    expect_true(grepl('class="a11y-skip-link"', html, fixed = TRUE), info = page)
    expect_true(grepl("<footer", html, fixed = TRUE), info = page)
  }
})

test_that("the 503 page is self-contained and never force-reloads", {
  html <- read_code(file.path(root, "shared", "assets", "503.html"))
  expect_true(grepl('<html lang="en"', html, fixed = TRUE))
  expect_false(grepl("http-equiv", html, fixed = TRUE))
  expect_true(grepl("Pause automatic retry", html, fixed = TRUE))
  # nginx serves it while Shiny Server (and shared/assets) may be down.
  expect_false(grepl("shared/assets/accessibility", html, fixed = TRUE))
})
