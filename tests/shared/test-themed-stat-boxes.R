# =============================================================================
# Tests for theme-aware stat boxes (stat_box_helpers.R)
# =============================================================================
# create_stat_box() previously took only a raw hex and hardcoded white text.
# It now accepts status keywords, resolves them through the active theme, and
# picks a readable foreground automatically.
# =============================================================================

library(testthat)
library(shiny)

context("Themed Stat Boxes")

render_html <- function(tag) htmltools::doRenderTags(tag)

test_that("resolve_box_color passes literal hex through untouched", {
  expect_equal(resolve_box_color("#123456"), "#123456")
  expect_equal(resolve_box_color("#123456", theme = "Wong"), "#123456")
})

test_that("resolve_box_color resolves indicator keywords per theme", {
  for (theme in get_available_themes()) {
    ind <- get_indicator_colors(theme)
    expect_equal(resolve_box_color("good", theme), unname(ind[["good"]]))
    expect_equal(resolve_box_color("warning", theme), unname(ind[["warning"]]))
    expect_equal(resolve_box_color("alert", theme), unname(ind[["alert"]]))
  }
})

test_that("resolve_box_color resolves theme status names", {
  for (theme in get_available_themes()) {
    colors <- get_status_colors(theme)
    for (status in c("active", "completed", "planned", "unknown")) {
      expect_equal(resolve_box_color(status, theme), unname(colors[[status]]))
    }
  }
})

test_that("resolve_box_color never emits NA into a style attribute", {
  # A typo in a lookup (the historical status_colors[unknonwn] bug) used to
  # produce NA and render background-color: NA.
  for (bad in list(NA_character_, NULL, "", "not_a_status")) {
    out <- resolve_box_color(bad)
    expect_true(is.character(out) && length(out) == 1 && !is.na(out))
    expect_match(out, "^#[0-9A-Fa-f]{6}$")
  }
})

test_that("create_stat_box accepts a status keyword and applies theme color", {
  for (theme in c("MMCD", "Wong", "Viridis")) {
    html <- render_html(create_stat_box("42", "Test", "good", theme = theme))
    expect_true(grepl(unname(get_indicator_colors(theme)[["good"]]), html, fixed = TRUE))
  }
})

test_that("create_stat_box auto-picks readable text color", {
  # Viridis warning is near-yellow: must get dark text, not white.
  html <- render_html(create_stat_box("1", "Warn", "warning", theme = "Viridis"))
  expect_true(grepl("#1a1a1a", html, fixed = TRUE))
  # MMCD good is dark: white text.
  html2 <- render_html(create_stat_box("1", "Good", "good", theme = "MMCD"))
  expect_true(grepl("#ffffff", html2, fixed = TRUE))
})

test_that("create_stat_box stays backwards compatible", {
  # Existing callers pass a raw hex and sometimes an explicit text color.
  html <- render_html(create_stat_box("7", "Legacy", "#667eea", text_color = "#ffffff"))
  expect_true(grepl("#667eea", html, fixed = TRUE))
  expect_true(grepl("#ffffff", html, fixed = TRUE))
})

test_that("create_stat_box never renders a literal NA background", {
  html <- render_html(create_stat_box("3", "T", NA_character_))
  expect_false(grepl("background-color: NA", html, fixed = TRUE))
})

test_that("create_status_stat_box emits no warning on its default theme", {
  # The old default was theme = "default", not a valid theme name, so every
  # call warned "Theme default not found" and ignored the configured theme.
  warnings_seen <- character(0)
  withCallingHandlers(
    invisible(create_status_stat_box("5", "Title", "active")),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings_seen, 0)
})

test_that("create_status_stat_box honors the configured theme", {
  old <- getOption("mmcd.color.theme")
  on.exit(options(mmcd.color.theme = old), add = TRUE)
  options(mmcd.color.theme = "Wong")
  html <- render_html(create_status_stat_box("9", "T", "active"))
  expect_true(grepl(unname(get_status_colors("Wong")[["active"]]), html, fixed = TRUE))
})

test_that("create_status_stat_box supports indicator keywords", {
  html <- render_html(create_status_stat_box("1", "T", "alert", theme = "MMCD"))
  expect_true(grepl("#dc2626", html, fixed = TRUE))
})
