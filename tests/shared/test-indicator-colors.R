# =============================================================================
# Tests for theme indicator colors, surface tokens, and the config override
# =============================================================================
# Covers the status-color system used by value boxes: good/warning/alert now
# come from the active theme (color_themes.R indicators block) rather than a
# single global set, with config/app_config.yaml thresholds.colors retained as
# an opt-in override.
# =============================================================================

library(testthat)

context("Indicator Colors and Theme Surface Tokens")

HEX_RE <- "^#[0-9A-Fa-f]{6}$"

test_that("every theme defines indicators with good/warning/alert", {
  for (theme in get_available_themes()) {
    ind <- get_theme_palette(theme)$indicators
    expect_false(is.null(ind), info = paste("Theme missing indicators:", theme))
    for (key in c("good", "warning", "alert")) {
      expect_true(key %in% names(ind),
                  info = paste("Theme", theme, "missing indicator:", key))
      expect_match(unname(ind[[key]]), HEX_RE,
                   info = paste("Invalid hex for", theme, key))
    }
  }
})

test_that("every theme defines surface tokens", {
  required <- c("bg", "panel", "border", "text", "text_muted",
                "link", "neutral", "faint", "text_strong")
  for (theme in get_available_themes()) {
    sf <- get_theme_palette(theme)$surface
    expect_false(is.null(sf), info = paste("Theme missing surface:", theme))
    for (key in required) {
      expect_true(key %in% names(sf),
                  info = paste("Theme", theme, "missing surface token:", key))
      expect_match(unname(sf[[key]]), HEX_RE,
                   info = paste("Invalid hex for", theme, "surface", key))
    }
  }
})

test_that("every theme defines sequential_heat", {
  # Previously MMCD-only, which silently broke any non-MMCD theme for
  # consumers reading the sequential_heat ramp.
  for (theme in get_available_themes()) {
    heat <- get_theme_palette(theme)$sequential_heat
    expect_false(is.null(heat), info = paste("Theme missing sequential_heat:", theme))
    expect_gte(length(heat), 2)
    for (color in heat) {
      expect_match(color, HEX_RE, info = paste("Invalid heat hex in", theme))
    }
  }
})

test_that("indicator colors are drawn from the theme own palette", {
  # Keeps each theme internally coherent - no foreign colors introduced.
  # MMCD is exempt on purpose: it keeps the three historical status colors so
  # the default dashboard looks exactly as it did before themes drove these.
  for (theme in setdiff(get_available_themes(), "MMCD")) {
    p <- get_theme_palette(theme)
    own <- toupper(c(p$primary, unname(p$status)))
    for (key in c("good", "warning", "alert")) {
      expect_true(toupper(unname(p$indicators[[key]])) %in% own,
                  info = paste(theme, key, "is not in that theme palette"))
    }
  }
})

test_that("get_indicator_colors returns all three keys as valid hex", {
  for (theme in get_available_themes()) {
    ind <- get_indicator_colors(theme)
    expect_setequal(names(ind), c("good", "warning", "alert"))
    for (key in names(ind)) {
      expect_match(unname(ind[[key]]), HEX_RE)
    }
  }
})

test_that("the theme actually drives indicator colors", {
  # Regression guard for the override trap: if thresholds.colors is repopulated
  # in app_config.yaml (or re-added as a hardcoded default in config.R), every
  # theme collapses to the same three colors and the theme-aware status system
  # silently stops working.
  mmcd <- get_indicator_colors("MMCD")
  for (theme in setdiff(get_available_themes(), "MMCD")) {
    expect_false(identical(mmcd, get_indicator_colors(theme)),
                 info = paste("Theme", theme, "resolves to the same indicator",
                              "colors as MMCD - the thresholds.colors override",
                              "is probably active"))
  }
})

test_that("MMCD indicator colors are unchanged from the historical values", {
  ind <- get_indicator_colors("MMCD")
  expect_equal(unname(ind[["good"]]), "#16a34a")
  expect_equal(unname(ind[["warning"]]), "#eab308")
  expect_equal(unname(ind[["alert"]]), "#dc2626")
})

# Approximate deuteranopia (most common red-green deficiency) so we can test
# what actually matters: whether the three indicator colors stay distinguishable
# for a colorblind viewer. A naive "is it green / is it red" hue check is the
# wrong test - Wong pairs bluish-green with vermillion precisely because that
# pair survives this transform, even though it reads as green-vs-red.
simulate_deuteranopia <- function(hex) {
  v <- col2rgb(hex)[, 1]
  c(
    0.625 * v[1] + 0.375 * v[2],
    0.700 * v[1] + 0.300 * v[2],
    0.300 * v[2] + 0.700 * v[3]
  )
}

test_that("indicator colors stay distinguishable under deuteranopia", {
  for (theme in get_available_themes()) {
    ind <- get_indicator_colors(theme)
    sims <- lapply(c("good", "warning", "alert"),
                   function(k) simulate_deuteranopia(unname(ind[[k]])))
    pairs <- list(c(1, 2), c(1, 3), c(2, 3))
    labels <- c("good/warning", "good/alert", "warning/alert")
    for (i in seq_along(pairs)) {
      d <- sqrt(sum((sims[[pairs[[i]][1]]] - sims[[pairs[[i]][2]]])^2))
      expect_gt(d, 40)
    }
  }
})

test_that("get_indicator_colors falls back cleanly for an unknown theme", {
  suppressWarnings({
    ind <- get_indicator_colors("NotATheme")
  })
  expect_setequal(names(ind), c("good", "warning", "alert"))
  for (key in names(ind)) expect_match(unname(ind[[key]]), HEX_RE)
})

test_that("contrast_text_color picks readable text for light and dark", {
  expect_equal(contrast_text_color("#000000"), "#ffffff")
  expect_equal(contrast_text_color("#ffffff"), "#1a1a1a")
  # Viridis warning is near-yellow; white text on it is unreadable
  expect_equal(contrast_text_color("#FDE724"), "#1a1a1a")
  expect_equal(contrast_text_color("#16a34a"), "#ffffff")
})

test_that("contrast_text_color handles NULL/NA/empty without erroring", {
  expect_equal(contrast_text_color(NULL), "#ffffff")
  expect_equal(contrast_text_color(NA_character_), "#ffffff")
  expect_equal(contrast_text_color(""), "#ffffff")
})

test_that("every theme indicator color gets readable text", {
  for (theme in get_available_themes()) {
    ind <- get_indicator_colors(theme)
    for (key in names(ind)) {
      fg <- contrast_text_color(unname(ind[[key]]))
      expect_true(fg %in% c("#ffffff", "#1a1a1a"), info = paste(theme, key))
    }
  }
})

test_that("hex_to_rgba produces a valid rgba string", {
  expect_equal(hex_to_rgba("#dc2626", 0.3), "rgba(220,38,38,0.3)")
  expect_equal(hex_to_rgba("#000000", 1), "rgba(0,0,0,1)")
})

test_that("hex_to_rgba degrades gracefully on bad input", {
  expect_true(grepl("^rgba", hex_to_rgba(NA_character_, 0.3)))
  expect_true(grepl("^rgba", hex_to_rgba(NULL, 0.3)))
})

test_that("get_theme_choices covers every available theme", {
  choices <- get_theme_choices()
  expect_setequal(unname(choices), get_available_themes())
  expect_true(all(nzchar(names(choices))))
})

test_that("get_theme_choices includes ColorBrewer", {
  # ColorBrewer was missing from the overview picker and its URL whitelist
  # while every other app offered it.
  expect_true("ColorBrewer" %in% unname(get_theme_choices()))
})
