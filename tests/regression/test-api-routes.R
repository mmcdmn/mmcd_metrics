# =============================================================================
# REGRESSION TEST: API routes vs the app functions they reuse
# =============================================================================
# Routes call app data functions by name, and a tryCatch turns a bad call into a
# 400 on every request, so nothing fails until a client hits it. These tests need
# no database: the routes are evaluated with their loaders stubbed.
# =============================================================================

library(testthat)
library(dplyr)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  if (file.exists("../apps")) return("..")
  stop("Cannot find project root")
}

route_path <- function(file) file.path(get_project_root(), "api/routes", file)

# Evaluate one top-level helper from a route file.
load_helper <- function(file, name, env) {
  is_helper <- function(e) length(e) > 1 && identical(e[[2]], as.name(name))
  eval(Filter(is_helper, as.list(parse(route_path(file))))[[1]], env)
}

# Evaluate the handler that follows a route's @get line.
load_route <- function(file, path, env) {
  exprs <- parse(route_path(file), keep.source = TRUE)
  first_lines <- vapply(attr(exprs, "srcref"), function(s) as.integer(s[1]), integer(1))
  get_line <- grep(paste0("@get ", path, "\\s*$"), readLines(route_path(file)))
  eval(exprs[[which(first_lines > get_line)[1]]], env)
}

route_env <- function() {
  env <- new.env(parent = globalenv())
  env$`%||%` <- `%||%`
  env$api_error <- function(res, status, msg) stop(msg)
  env$validate_zone <- function(zone) c("1", "2")
  env$validate_facility <- function(v) v
  env$validate_foreman <- function(v) v
  env$validate_priority <- function(v) v
  env$validate_group_by <- function(v, allowed, default) v
  env$clean_text <- function(v, n) v
  env$apply_row_limit <- function(df, limit = NULL, default_limit = 500L) {
    n <- min(nrow(df), as.integer(limit %||% default_limit))
    list(count = nrow(df), data = df[seq_len(n), , drop = FALSE])
  }
  env
}

test_that("every route calls app functions with arguments they accept", {
  root <- get_project_root()
  walk <- function(x, f) {
    if (is.call(x)) f(x)
    if (is.call(x) || is.pairlist(x)) {
      items <- as.list(x)
      for (i in seq_along(items)) {
        if (identical(items[[i]], quote(expr = ))) next
        el <- items[[i]]
        if (is.call(el) || (is.pairlist(el) && !is.null(el))) walk(el, f)
      }
    }
  }
  # Functions of an app, including files its data_functions.R sources itself.
  app_functions <- function(app_dir) {
    fns <- list()
    for (f in list.files(app_dir, pattern = "[.]R$", full.names = TRUE)) {
      if (basename(f) %in% c("app.R", "ui.R", "server.R")) next
      for (e in parse(f, keep.source = FALSE)) {
        if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
            is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) {
          fns[[as.character(e[[2]])]] <- names(e[[3]][[2]]) %||% character(0)
        }
      }
    }
    fns
  }

  checked <- 0L
  for (route_file in list.files(file.path(root, "api/routes"), pattern = "[.]R$", full.names = TRUE)) {
    exprs <- parse(route_file, keep.source = FALSE)
    envs <- list()
    for (e in exprs) {
      if (is.call(e) && identical(e[[1]], as.name("source")) && !is.null(e$local)) {
        app <- sub("^/srv/shiny-server/", "", as.character(e[[2]]))
        envs[[as.character(e$local)]] <- file.path(root, dirname(app))
      }
    }
    for (e in exprs) walk(e, function(cl) {
      h <- cl[[1]]
      if (!(is.call(h) && identical(h[[1]], as.name("$")) && is.name(h[[2]]))) return(invisible())
      dir <- envs[[as.character(h[[2]])]]
      if (is.null(dir)) return(invisible())
      fn <- as.character(h[[3]])
      formals_of <- app_functions(dir)[[fn]]
      where <- paste0(basename(route_file), ": ", as.character(h[[2]]), "$", fn)
      expect_false(is.null(formals_of), info = paste(where, "does not exist"))
      if ("..." %in% formals_of) return(invisible())
      supplied <- names(as.list(cl)[-1])
      supplied <- supplied[nzchar(supplied %||% character(0))]
      expect_true(all(supplied %in% formals_of),
                  info = paste(where, "got", paste(setdiff(supplied, formals_of), collapse = ",")))
      checked <<- checked + 1L
    })
  }
  expect_gt(checked, 40L)
})

test_that("apply_row_limit returns all rows unless a limit is given", {
  exprs <- as.list(parse(file.path(get_project_root(), "api/api_helpers.R")))
  is_helper <- function(e) length(e) > 1 && identical(e[[2]], as.name("apply_row_limit"))
  apply_row_limit <- eval(Filter(is_helper, exprs)[[1]])
  df <- data.frame(id = seq_len(6000))

  expect_equal(nrow(apply_row_limit(df)$data), 6000L)
  expect_equal(nrow(apply_row_limit(df, "")$data), 6000L)
  expect_equal(nrow(apply_row_limit(df, "abc")$data), 6000L)

  limited <- apply_row_limit(df, "10")
  expect_equal(c(limited$count, limited$returned, nrow(limited$data)), c(6000L, 10L, 10L))
  expect_true(limited$truncated)
  expect_equal(nrow(apply_row_limit(df, "9999")$data), 5000L)
  expect_equal(nrow(apply_row_limit(df, default_limit = 25L)$data), 25L)
})

test_that("material-codes passes load_dosage_options a connection", {
  env <- route_env()
  closed <- FALSE
  env$get_db_connection <- function() "con"
  env$safe_disconnect <- function(con) closed <<- TRUE
  env$efficacy_env <- list(load_dosage_options = function(con, ingredient = NULL) {
    expect_equal(con, "con")
    data.frame(matcode = "A1")
  })
  route <- load_route("treatments.R", "/material-codes", env)

  expect_equal(route(list(), list())$matcode, "A1")
  expect_true(closed)
})

test_that("larval returns the data frame load_raw_data gives it", {
  env <- route_env()
  load_helper("inspections.R", ".insp_drone_filter", env)
  env$insp_env <- list(load_raw_data = function(drone_filter, ...) {
    expect_equal(drone_filter, "drone_only")
    data.frame(sitecode = c("190401", "190402"))
  })
  route <- load_route("inspections.R", "/larval", env)

  result <- route(list(), list(), drone_filter = "drone_only", limit = "1")
  expect_equal(result$count, 2L)
  expect_equal(nrow(result$data), 1L)
  expect_equal(nrow(route(list(), list(), drone_filter = "drone_only")$data), 2L)
  expect_error(route(list(), list(), drone_filter = "Y"), "drone_filter must be")
})

test_that("SUCO by species uses the species branch and species-level data", {
  env <- route_env()
  load_helper("surveillance.R", ".load_suco", env)
  load_helper("surveillance.R", ".suco_date_range", env)
  load_helper("surveillance.R", ".suco_rows", env)
  env$suco_env <- list(
    get_suco_data = function(return_species_details, ...) {
      expect_true(return_species_details)
      data.frame(species_name = "Culex", cnt = 2)
    },
    filter_suco_data = function(data, ...) data,
    create_summary_stats = function(data, group_by, data_source) {
      expect_equal(group_by, "species")
      data.frame(Species = "Culex", Total_Species_Count = 2)
    }
  )
  route <- load_route("surveillance.R", "/suco/summary-by-group", env)

  result <- route(list(), list(), group_by = "species_name")
  expect_equal(result$groups[[1]]$Species, "Culex")

  env$suco_env$filter_suco_data <- function(data, ...) data.frame(species_name = NA_character_)
  expect_length(route(list(), list(), group_by = "species_name")$groups, 0)
})

test_that("top locations are ranked per location before the limit", {
  env <- new.env(parent = globalenv())
  load_helper("surveillance.R", ".suco_rank_locations", env)
  samples <- data.frame(
    location = c("A", "B", "B", "B", "C", "C"),
    visits = 1,
    inspdate = as.Date("2026-06-01") + 0:5
  )

  ranked <- env$.suco_rank_locations(samples, "visits")
  expect_equal(ranked$location, c("B", "C", "A"))
  expect_equal(ranked$visits, c(3, 2, 1))
  expect_equal(ranked$first_suco[1], as.Date("2026-06-02"))
  expect_equal(ranked$last_suco[1], as.Date("2026-06-04"))

  samples$species_count <- c(9, 1, 1, 1, 2, 2)
  expect_equal(env$.suco_rank_locations(samples, "species")$location, c("A", "C", "B"))
})

test_that("control-efficacy material filter matches the active ingredient", {
  env <- route_env()
  env$EFFICACY_TRT_ACTIONS <- list(air = "A")
  eff <- data.frame(
    facility = "E", genus = "Aedes", season = "Spring", trt_action = "A",
    active_ingredient = c("Spinosad", "(S)-Methoprene"),
    is_control = FALSE, is_invalid = FALSE, pct_reduction = c(90, 50)
  )
  env$efficacy_env <- list(load_efficacy_data = function(...) eff)
  route <- load_route("treatments.R", "/control-efficacy/summary", env)

  expect_equal(route(list(), list(), material_type = "spinosad")$median_pct_reduction, 90)
  expect_equal(route(list(), list(), material_type = "methoprene")$median_pct_reduction, 50)
  expect_equal(route(list(), list(), material_type = "all")$valid_count, 2L)
})
