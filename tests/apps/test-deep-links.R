# =============================================================================
# TESTS: every tab of every deployed app can be reached by a URL deep link
# =============================================================================
# A tab container (tabsetPanel / tabBox / sidebarMenu / navlistPanel / navbarPage)
# only becomes linkable when it has an `id` AND that id is a type = "tab" entry in
# the app's wire_deep_links() spec. This reads every deployed app (shiny-server.conf)
# and fails when a container is missing either, so a new tab cannot ship unlinkable.
# =============================================================================

library(testthat)

.root_file <- function(rel_path) {
  for (p in c(file.path("../..", rel_path), rel_path)) if (file.exists(p)) return(p)
  stop("Cannot find ", rel_path)
}

CONTAINERS <- c("tabsetPanel", "tabBox", "sidebarMenu", "navlistPanel", "navbarPage")

# Deployed Shiny app folders (relative to apps/), from the app_dir lines.
deployed_app_dirs <- function() {
  conf <- readLines(.root_file("shiny-server.conf"), warn = FALSE)
  hit <- regmatches(conf, regexpr("app_dir\\s+/srv/shiny-server/apps/[^;[:space:]]+", conf))
  unique(sub("^app_dir\\s+/srv/shiny-server/apps/", "", hit))
}

# Tab containers in one R file: function name and id ("" when it has none).
containers_in <- function(file) {
  pd <- utils::getParseData(parse(file, keep.source = TRUE), includeText = TRUE)
  if (is.null(pd)) return(list())
  calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% CONTAINERS, , drop = FALSE]
  lapply(seq_len(nrow(calls)), function(i) {
    fn_expr <- pd$parent[pd$id == calls$id[i]]
    call_id <- pd$parent[pd$id == fn_expr]
    kids <- pd[pd$parent == call_id, , drop = FALSE]
    kids <- kids[order(kids$line1, kids$col1), , drop = FALSE]
    id_at <- which(kids$token == "SYMBOL_SUB" & kids$text == "id")
    id <- ""
    if (length(id_at) == 1 && id_at + 2 <= nrow(kids)) {
      id <- gsub("^[\"']|[\"']$", "", kids$text[id_at + 2])
    }
    list(fn = calls$text[i], id = id, line = calls$line1[i])
  })
}

# Is `id` a type = "tab" entry in the app's deep-link spec?
id_is_wired <- function(app_text, id) {
  esc <- gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", id)
  by_input <- sprintf("input\\s*=\\s*\"%s\"[^\n]*type\\s*=\\s*\"tab\"", esc)
  by_key   <- sprintf("(^|[^A-Za-z0-9_])%s\\s*=\\s*list\\(\\s*type\\s*=\\s*\"tab\"", esc)
  grepl(by_input, app_text, perl = TRUE) || grepl(by_key, app_text, perl = TRUE)
}

test_that("every deployed app is found", {
  apps <- deployed_app_dirs()
  expect_gt(length(apps), 10)
  expect_true("suco_history" %in% apps)
  expect_true("trap_surveillance" %in% apps)
})

test_that("every tab container in a deployed app has an id and is wired into its deep links", {
  problems <- character()
  for (app in deployed_app_dirs()) {
    dir <- .root_file(file.path("apps", app))
    r_files <- list.files(dir, pattern = "\\.R$", full.names = TRUE)
    app_file <- file.path(dir, "app.R")
    app_text <- if (file.exists(app_file)) paste(readLines(app_file, warn = FALSE), collapse = "\n") else ""
    for (f in r_files) {
      for (cn in containers_in(f)) {
        where <- sprintf("%s/%s:%d %s()", app, basename(f), cn$line, cn$fn)
        if (!nzchar(cn$id)) {
          problems <- c(problems, paste(where, "has no id, so its tabs cannot be linked"))
        } else if (!id_is_wired(app_text, cn$id)) {
          problems <- c(problems, sprintf("%s id \"%s\" is not a type = \"tab\" entry in %s/app.R's deep-link spec",
                                          where, cn$id, app))
        }
      }
    }
  }
  expect_equal(problems, character(0), info = paste(problems, collapse = "\n"))
})

test_that("apps with tab containers call wire_deep_links (or a documented equivalent)", {
  # ground_prehatch_progress wires the same spec through its own single-pass observer.
  own_engine <- c("ground_prehatch_progress")
  missing <- character()
  for (app in setdiff(deployed_app_dirs(), own_engine)) {
    dir <- .root_file(file.path("apps", app))
    n <- sum(vapply(list.files(dir, pattern = "\\.R$", full.names = TRUE),
                    function(f) length(containers_in(f)), numeric(1)))
    app_file <- file.path(dir, "app.R")
    if (n > 0 && file.exists(app_file) &&
        !any(grepl("wire_deep_links", readLines(app_file, warn = FALSE), fixed = TRUE))) {
      missing <- c(missing, app)
    }
  }
  expect_equal(missing, character(0))
})

# -----------------------------------------------------------------------------
# trap_surveillance: short tab names and the button each page needs
# -----------------------------------------------------------------------------
trap_env <- new.env(parent = globalenv())
for (e in parse(.root_file("apps/trap_surveillance/app.R"))) {
  if (is.call(e) && identical(as.character(e[[1]]), "<-") &&
      as.character(e[[2]]) %in% c("TRAP_TAB_ALIASES", "trap_tab_value", "trap_refresh_for")) {
    eval(e, trap_env)
  }
}

test_that("trap_surveillance accepts short names and the menu tab names", {
  v <- trap_env$trap_tab_value
  expect_equal(v("methods"), "methods_tab")
  expect_equal(v("methods_tab"), "methods_tab")
  expect_equal(v(" METHODS "), "methods_tab")
  expect_equal(v("map"), "map_tab")
  expect_equal(v("trend"), "trend_tab")
  expect_equal(v("table"), "table_tab")
  expect_equal(v("analysis"), "trap_analysis_tab")
  expect_equal(v("causal"), "trap_analysis_tab")
  expect_equal(v("trap_analysis_tab"), "trap_analysis_tab")
})

test_that("every trap_surveillance menu page can be named in a link", {
  ui <- readLines(.root_file("apps/trap_surveillance/ui_helper.R"), warn = FALSE)
  menu <- regmatches(ui, regexpr("menuItem\\([^)]*tabName = \"[a-z_]+\"", ui))
  tabs <- sub(".*tabName = \"([a-z_]+)\"", "\\1", menu)
  expect_setequal(tabs, c("map_tab", "trend_tab", "table_tab", "trap_analysis_tab", "methods_tab"))
  expect_true(all(tabs %in% trap_env$TRAP_TAB_ALIASES))
})

test_that("trap_surveillance links click only the button their page needs", {
  r <- trap_env$trap_refresh_for
  expect_equal(r(list()), "refresh")                       # no ?tabs= -> map, as before
  expect_equal(r(list(tabs = "map")), "refresh")
  expect_equal(r(list(tabs = "table")), "refresh")
  expect_equal(r(list(tabs = "analysis")), "analysis_refresh")
  expect_equal(r(list(tabs = "trap_analysis_tab")), "analysis_refresh")
  expect_length(r(list(tabs = "methods")), 0)              # static text: no data load
  expect_length(r(list(tabs = "methods_tab")), 0)
  expect_length(r(list(tabs = "trend")), 0)
  expect_equal(r(list(tabs = "")), "refresh")
  expect_equal(r(list(tabs = "nonsense")), "refresh")
})
