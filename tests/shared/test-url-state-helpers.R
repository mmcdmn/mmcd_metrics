# =============================================================================
# Tests for the shared deep-link engine hooks (shared/url_state_helpers.R)
# =============================================================================
library(testthat)

resolve_shared_file <- function(rel_path) {
  for (candidate in c(rel_path, file.path("..", rel_path), file.path("..", "..", rel_path))) {
    if (file.exists(candidate)) return(candidate)
  }
  NA_character_
}

env <- new.env()
src <- resolve_shared_file("shared/url_state_helpers.R")
if (!is.na(src)) source(src, local = env)

test_that("expand_url_state leaves the state alone without a hook", {
  skip_if(is.na(src), "url_state_helpers.R not found")
  st <- list(tabs = "whatif", s = "abc")
  expect_identical(env$expand_url_state(st), st)
  expect_identical(env$expand_url_state(st, NULL), st)
})

test_that("expand_url_state returns what the hook returns", {
  skip_if(is.na(src), "url_state_helpers.R not found")
  hook <- function(state) { state$extra <- "1"; state$tabs <- NULL; state }
  out <- env$expand_url_state(list(tabs = "capacity", s = "abc"), hook)
  expect_equal(out$extra, "1")
  expect_null(out$tabs)
  expect_equal(out$s, "abc")
})

test_that("a hook that errors or returns a non-list leaves the state unchanged", {
  skip_if(is.na(src), "url_state_helpers.R not found")
  st <- list(tabs = "whatif")
  expect_warning(res <- env$expand_url_state(st, function(state) stop("boom")), "expand_state failed")
  expect_identical(res, st)
  expect_identical(env$expand_url_state(st, function(state) "not a list"), st)
  expect_identical(env$expand_url_state(st, function(state) NULL), st)
})

test_that("wire_deep_links accepts expand_state and stays backward compatible", {
  skip_if(is.na(src), "url_state_helpers.R not found")
  args <- names(formals(env$wire_deep_links))
  expect_true("expand_state" %in% args)
  expect_null(formals(env$wire_deep_links)$expand_state)      # default: no hook
  body_txt <- paste(deparse(body(env$wire_deep_links)), collapse = " ")
  expect_true(grepl("expand_url_state", body_txt, fixed = TRUE))
})
