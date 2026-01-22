# =============================================================================
# Tests for Color Theme Functions (color_themes.R, db_helpers.R)
# =============================================================================

library(testthat)

context("Color Theme Functions")

# =============================================================================
# get_theme_palette() tests
# =============================================================================

test_that("get_theme_palette returns valid structure for MMCD theme", {
  palette <- get_theme_palette("MMCD")
  
  expect_type(palette, "list")
  expect_true("primary" %in% names(palette))
  expect_true("facilities" %in% names(palette))
  expect_true("status" %in% names(palette))
})

test_that("get_theme_palette returns valid structure for all supported themes", {
  themes <- c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer")
  
  for (theme in themes) {
    palette <- get_theme_palette(theme)
    expect_type(palette, "list")
    expect_true("facilities" %in% names(palette), 
                info = paste("Theme missing facilities:", theme))
    expect_true("status" %in% names(palette), 
                info = paste("Theme missing status:", theme))
  }
})

test_that("get_theme_palette facility colors include all facilities", {
  required_facilities <- c("E", "MO", "N", "Sj", "Sr", "Wm", "Wp")
  palette <- get_theme_palette("MMCD")
  
  for (facility in required_facilities) {
    expect_true(facility %in% names(palette$facilities), 
                info = paste("Missing facility:", facility))
  }
})

test_that("get_theme_palette status colors include required statuses", {
  required_statuses <- c("active", "completed", "planned", "needs_treatment", "unknown")
  palette <- get_theme_palette("MMCD")
  
  for (status in required_statuses) {
    expect_true(status %in% names(palette$status),
                info = paste("Missing status:", status))
  }
})

# =============================================================================
# get_status_colors() tests
# =============================================================================

test_that("get_status_colors returns named vector", {
  colors <- get_status_colors()
  
  expect_type(colors, "character")
  expect_true(length(names(colors)) > 0)
})

test_that("get_status_colors includes required status types", {
  colors <- get_status_colors()
  
  required <- c("active", "completed", "unknown")
  for (status in required) {
    expect_true(status %in% names(colors), 
                info = paste("Missing status color:", status))
  }
})

test_that("get_status_colors returns valid hex colors", {
  colors <- get_status_colors()
  
  # Check that all colors are valid hex format
  for (color in colors) {
    expect_match(color, "^#[0-9A-Fa-f]{6}$", 
                 info = paste("Invalid hex color:", color))
  }
})

test_that("get_status_colors respects theme parameter", {
  mmcd_colors <- get_status_colors(theme = "MMCD")
  ibm_colors <- get_status_colors(theme = "IBM")
  
  # Colors should be different between themes
  expect_false(identical(mmcd_colors, ibm_colors))
})

# =============================================================================
# get_facility_base_colors() tests
# =============================================================================

test_that("get_facility_base_colors returns named vector", {
  colors <- get_facility_base_colors()
  
  expect_type(colors, "character")
  expect_true(length(colors) >= 7)  # At least 7 facilities
})

test_that("get_facility_base_colors includes all facilities", {
  colors <- get_facility_base_colors()
  required <- c("E", "MO", "N", "Sj", "Sr", "Wm", "Wp")
  
  for (facility in required) {
    expect_true(facility %in% names(colors),
                info = paste("Missing facility:", facility))
  }
})

test_that("get_facility_base_colors returns valid hex colors", {
  colors <- get_facility_base_colors()
  
  for (color in colors) {
    expect_match(color, "^#[0-9A-Fa-f]{6}$",
                 info = paste("Invalid hex color:", color))
  }
})

test_that("get_facility_base_colors respects theme parameter", {
  mmcd_colors <- get_facility_base_colors(theme = "MMCD")
  wong_colors <- get_facility_base_colors(theme = "Wong")
  
  expect_false(identical(mmcd_colors, wong_colors))
})

# =============================================================================
# get_themed_foreman_colors() tests
# =============================================================================

test_that("get_themed_foreman_colors returns named vector", {
  colors <- get_themed_foreman_colors()
  
  expect_type(colors, "character")
  expect_true(length(colors) > 0)
})

test_that("get_themed_foreman_colors returns valid hex colors", {
  colors <- get_themed_foreman_colors()
  
  for (color in colors) {
    expect_match(color, "^#[0-9A-Fa-f]{6}$",
                 info = paste("Invalid hex color:", color))
  }
})

# =============================================================================
# generate_distinct_colors() tests (if available)
# =============================================================================

test_that("generate_distinct_colors returns requested number of colors", {
  skip_if_not(exists("generate_distinct_colors", mode = "function"))
  
  colors <- generate_distinct_colors(5)
  expect_length(colors, 5)
  
  colors <- generate_distinct_colors(10)
  expect_length(colors, 10)
})

test_that("generate_distinct_colors returns empty for n=0", {
  skip_if_not(exists("generate_distinct_colors", mode = "function"))
  
  colors <- generate_distinct_colors(0)
  expect_length(colors, 0)
})
