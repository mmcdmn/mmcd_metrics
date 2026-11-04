# =============================================================================
# TEST: section-cards app
# =============================================================================
# Tests for the printed card content: remarks space, highlight badges, watermark.
# Runs in isolated mode - generate_card_html needs no database, and the two lookups
# generate_section_cards_html makes are stubbed below.
# =============================================================================

get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

root <- get_project_root()
cards_env <- new.env()
cards_env$get_facility_lookup <- function() data.frame(short_name = "E", full_name = "East")
cards_env$get_foremen_lookup <- function() data.frame(shortname = "Smith", emp_num = "8208")
cards_env$humanize_column_name <- function(x) x
source(file.path(root, "apps/section-cards/display_functions.R"), local = cards_env)

card_row <- function(...) {
  defaults <- list(sitecode = "620205-001", priority = "YELLOW", acres = 0.5, prehatch = NA_character_,
                   drone = NA_character_, ra = NA_character_, perturbans = NA_character_, remarks = "")
  as.data.frame(modifyList(defaults, list(...)), stringsAsFactors = FALSE)
}

render_card <- function(row, title_fields = c("sitecode", "remarks"), watermark_fields = NULL, ...) {
  cards_env$generate_card_html(row, title_fields, "date", 1, list(remarks = "Remarks", drone = "Drone"),
                               character(0), character(0), watermark_fields = watermark_fields, ...)
}

test_that("blank remarks still get their space", {
  expect_true(grepl("info-item remarks", render_card(card_row())))
  html <- cards_env$generate_section_cards_html(card_row(), c("sitecode", "remarks"), "date")
  expect_true(grepl("info-item remarks", html))
  expect_true(grepl("--sc-remarks-box:", html, fixed = TRUE))
})

test_that("existing remarks are shown", {
  expect_true(grepl("New 2024", render_card(card_row(remarks = "New 2024"))))
})

test_that("no remarks space when the field is not selected", {
  expect_false(grepl("info-item remarks", render_card(card_row(), title_fields = "sitecode")))
})

test_that("every layout stops short of the paper's bottom edge", {
  for (cards in c(4, 6, 8, 10, 12)) {
    html <- cards_env$generate_section_cards_html(card_row(), "sitecode", "date", cards_per_page = cards)
    expect_true(grepl("--sc-page-padding: 0.2in 0.25in 0.05in;", html, fixed = TRUE), info = cards)
    expect_true(grepl("--sc-usable-height: 10.5in;", html, fixed = TRUE), info = cards)
  }
})

test_that("table columns get the short and longer widths", {
  col_widths <- function(fields) {
    html <- cards_env$build_table_header(fields, list())
    matches <- regmatches(html, gregexpr("width: [0-9.]+%", html))[[1]]
    setNames(as.numeric(gsub("[^0-9.]", "", matches)), fields)
  }
  all_cols <- c("date", "wet_pct", "emp_num", "num_dip", "sample_num", "amt", "mat")
  widths <- col_widths(all_cols)
  expect_equal(sum(widths), 100, tolerance = 0.01)
  expect_gt(min(widths[c("date", "emp_num", "sample_num")]), max(widths[c("wet_pct", "num_dip", "amt", "mat")]))

  # Fills the card with any selection, and a [DB] column sits between the two groups
  widths <- col_widths(c("date", "amt", "airmap_num"))
  expect_equal(sum(widths), 100, tolerance = 0.01)
  expect_gt(widths[["date"]], widths[["airmap_num"]])
  expect_gt(widths[["airmap_num"]], widths[["amt"]])
})

test_that("every column label gets a font limit that fits its column", {
  html <- cards_env$build_table_header(c("sample_num", "amt"), list(sample_num = "Sample #", amt = "Amt"))
  expect_equal(lengths(regmatches(html, gregexpr("--sc-th-fit: calc(", html, fixed = TRUE))), 2)
})

test_that("only Drone:Y gets the drone highlight", {
  drone_y <- render_card(card_row(drone = "y"), title_fields = "drone")
  expect_true(grepl('class="drone-field">Drone: Y<', drone_y, fixed = TRUE))

  drone_m <- render_card(card_row(drone = "M"), title_fields = "drone")
  expect_false(grepl("drone-field", drone_m))
  expect_true(grepl("Drone:</span> M", drone_m, fixed = TRUE))

  expect_false(grepl("Drone", render_card(card_row(), title_fields = "drone")))
})

test_that("prehatch materials get their own highlight", {
  badge <- function(value) render_card(card_row(prehatch = value), title_fields = "prehatch")
  expect_true(grepl('class="prehatch-field">PREHATCH<', badge("PREHATCH"), fixed = TRUE))
  expect_true(grepl('class="prehatch-field">PELLET<', badge("PELLET"), fixed = TRUE))
  expect_true(grepl('class="prehatch-field prehatch-briquet">BRIQUET<', badge("BRIQUET"), fixed = TRUE))
  expect_true(grepl('class="prehatch-field prehatch-bti">BTI<', badge("bti"), fixed = TRUE))
  expect_false(grepl("prehatch-field", badge(NA_character_)))
})

test_that("the perturbans watermark reads Cattail", {
  html <- render_card(card_row(perturbans = "Y"), watermark_fields = "perturbans")
  expect_true(grepl(">Cattail</span>", html, fixed = TRUE))
  expect_false(grepl("Perturbans", html, fixed = TRUE))
})

test_that("highlight colors match the mapping standards", {
  css <- paste(readLines(file.path(root, "apps/section-cards/www/section-cards-print.css"), warn = FALSE),
               collapse = "\n")
  expect_colors <- c(.priority_yellow = "#FFFF00", .ra_field = "#FF69B4",
                     .prehatch_field = "#32CD32", .prehatch_briquet = "#008000",
                     .prehatch_bti = "#D00000", .drone_field = "#B388FF")
  for (selector in names(expect_colors)) {
    rule <- paste0(gsub("_", "-", selector, fixed = TRUE), "\\s*\\{ background-color: ", expect_colors[[selector]])
    expect_true(grepl(rule, css), info = selector)
  }
})
