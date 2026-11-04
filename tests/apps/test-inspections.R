# =============================================================================
# TEST: inspections app (prehatch coverage / the 51% goal)
# =============================================================================
# Tests the REAL functions in apps/inspections/coverage_functions.R. They are
# pure (no database): classification works on a data frame, and the one function
# that normally queries the DB (load_prehatch_site_coverage) is stubbed.
# =============================================================================

skip_if_not_installed("dplyr")
skip_if_not_installed("lubridate")

# Helper to get project root (tests run from tests/apps or project root)
get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

# Load the real coverage functions into their own environment (as the Overview does)
coverage_env <- new.env(parent = globalenv())
local({
  suppressPackageStartupMessages({ library(dplyr); library(lubridate) })
}, envir = globalenv())
source(file.path(get_project_root(), "apps/inspections/coverage_functions.R"),
       local = coverage_env, chdir = TRUE)

AS_OF <- as.Date("2026-10-27")
D <- as.Date

# One synthetic row in the shape load_prehatch_site_coverage() returns
make_site <- function(sitecode = "020207-001", ...) {
  defaults <- list(
    sitecode = sitecode, facility = "N", fosarea = "0204", zone = "1", priority = "RED",
    drone = NA_character_, acres = 1, prehatch = "PREHATCH",
    insp_5y = 0L, insp_this_year = 0L, wet_n = 0L, wet_count = 0L,
    last_insp_date = D(NA), last_wet_date = D(NA),
    samples_5y = 0L, samples_this_year = 0L, pending_this_year = 0L,
    last_sample_date = D(NA), last_sample_result = NA_character_,
    last_red_date_5y = D(NA), last_red_older = D(NA)
  )
  as.data.frame(modifyList(defaults, list(...)), stringsAsFactors = FALSE)
}

classify <- function(...) coverage_env$classify_site_coverage(make_site(...), AS_OF)

# -----------------------------------------------------------------------------
# The rule
# -----------------------------------------------------------------------------

test_that("a red bug inside the 5-year window counts and says when it stops", {
  r <- classify(last_red_date_5y = D("2024-06-15"), samples_5y = 1L, last_sample_result = "R")
  expect_equal(as.character(r$status), "red_5yr")
  expect_true(r$counts)
  expect_equal(r$counts_through, D("2029-06-15"))
  expect_equal(r$drops_because, "Red bug ages out of 5-yr window")
})

test_that("the 5-year window is inclusive of its first day", {
  on_edge <- classify(last_red_date_5y = D("2021-10-27"))
  expect_equal(as.character(on_edge$status), "red_5yr")

  day_before <- classify(last_red_older = D("2021-10-26"))
  expect_equal(as.character(day_before$status), "red_expired")
  expect_false(day_before$counts)
})

test_that("any sample this year counts, whatever the result", {
  for (result in c("R", "B", NA_character_)) {
    r <- classify(samples_5y = 1L, samples_this_year = 1L, last_sample_date = D("2026-07-01"),
                  last_sample_result = result)
    expect_true(r$counts, info = paste("result:", result))
  }
  blue <- classify(samples_5y = 1L, samples_this_year = 1L, last_sample_date = D("2026-07-01"),
                   last_sample_result = "B")
  expect_equal(as.character(blue$status), "sampled_this_year")
  expect_equal(blue$counts_through, D("2026-12-31"))
  expect_equal(blue$drops_because, "This year's sample (resets Jan 1)")
})

test_that("an inspection with no sample does not count", {
  r <- classify(insp_5y = 6L, insp_this_year = 2L, wet_n = 6L, wet_count = 1L,
                last_insp_date = D("2026-08-01"))
  expect_equal(as.character(r$status), "inspected_no_sample")
  expect_false(r$counts)
})

test_that("a pupae bioassay can carry a sample only when numdip > 0", {
  # the SQL helper needs build_sql_in_list (shared/db_helpers.R, which opens a pool) - stub it
  coverage_env$build_sql_in_list <- function(x) paste0("'", x, "'", collapse = ", ")

  bare <- coverage_env$.sample_action_sql()
  expect_match(bare, "action IN ('1', '2', '4')", fixed = TRUE)
  expect_match(bare, "action = 'P' AND numdip > 0", fixed = TRUE)

  aliased <- coverage_env$.sample_action_sql("i.")
  expect_match(aliased, "i.action = 'P' AND i.numdip > 0", fixed = TRUE)
  expect_false(grepl("[^.]numdip", aliased))   # every column carries the alias
})

test_that("ID pending = a sample taken this year whose result is not entered yet, and nothing else", {
  base <- list(samples_5y = 1L, samples_this_year = 1L, last_sample_date = D("2026-07-01"))

  waiting <- do.call(classify, c(base, list(pending_this_year = 1L)))
  expect_equal(as.character(waiting$status), "sampled_this_year")
  expect_true(waiting$id_pending)

  # the ID is already back (blue / no bugs): still Sampled until Jan 1, but not waiting on anything
  answered <- do.call(classify, c(base, list(pending_this_year = 0L, last_sample_result = "B")))
  expect_equal(as.character(answered$status), "sampled_this_year")
  expect_false(answered$id_pending)

  # a site that already has a red bug can also have a sample waiting for its ID: that
  # sample IS pending, even though the site's status (red_5yr) does not depend on it
  red_waiting <- classify(last_red_date_5y = D("2025-06-01"), samples_5y = 1L, samples_this_year = 1L,
                          pending_this_year = 2L, last_sample_date = D("2026-07-01"))
  expect_equal(as.character(red_waiting$status), "red_5yr")
  expect_true(red_waiting$counts)
  expect_true(red_waiting$id_pending)
  expect_match(red_waiting$reason, "ID pending for 2 sample(s) taken this year", fixed = TRUE)

  # ...and once that ID is back there is nothing left to wait for
  red_answered <- classify(last_red_date_5y = D("2025-06-01"), samples_5y = 1L, samples_this_year = 1L,
                           pending_this_year = 0L, last_sample_result = "R", last_sample_date = D("2026-07-01"))
  expect_false(red_answered$id_pending)
  expect_false(grepl("ID pending", red_answered$reason, fixed = TRUE))

  # a red bug site with no sample this year has nothing pending
  expect_false(classify(last_red_date_5y = D("2025-06-01"), samples_5y = 1L)$id_pending)

  # sites that are Missing are never ID pending
  expect_false(classify(insp_5y = 3L)$id_pending)
  expect_false(classify()$id_pending)
})

test_that("Sampled (nothing pending), ID pending and Missing split every site exactly once", {
  sites <- coverage_env$classify_site_coverage(do.call(rbind, list(
    make_site("red_only",  last_red_date_5y = D("2025-06-01"), samples_5y = 1L),
    make_site("red_wait",  last_red_date_5y = D("2025-06-01"), samples_5y = 2L, samples_this_year = 1L,
              pending_this_year = 1L, last_sample_date = D("2026-07-01")),
    make_site("blue_done", samples_5y = 1L, samples_this_year = 1L, pending_this_year = 0L,
              last_sample_date = D("2026-07-01"), last_sample_result = "B"),
    make_site("new_wait",  samples_5y = 1L, samples_this_year = 1L, pending_this_year = 1L,
              last_sample_date = D("2026-07-01")),
    make_site("dry",       insp_5y = 3L),
    make_site("never")
  )), AS_OF)

  sampled_done <- sites$counts & !sites$id_pending
  waiting      <- sites$id_pending
  missing      <- !sites$counts
  expect_equal(sites$sitecode[sampled_done], c("red_only", "blue_done"))
  expect_equal(sites$sitecode[waiting], c("red_wait", "new_wait"))
  expect_equal(sites$sitecode[missing], c("dry", "never"))
  expect_equal(sampled_done + waiting + missing, rep(1L, nrow(sites)))
  # pending sites count toward the goal, so Sampled + ID pending is the headline count
  expect_equal(sum(sampled_done) + sum(waiting), coverage_env$summarise_coverage(sites)$counting)
})

test_that("a pupae-bioassay sample with no result yet reads like any other pending sample", {
  # A bioassay has no larval ID row, so its result stays blank until entered: it must say
  # "ID pending", and in the table "Not entered yet" - not a separate bioassay label.
  r <- classify(samples_5y = 1L, samples_this_year = 1L, pending_this_year = 1L,
                last_sample_date = D("2026-07-01"))
  expect_true(r$id_pending)
  expect_match(r$reason, coverage_env$COVERAGE_LABEL_ID_PENDING, fixed = TRUE)
  expect_false(grepl("bioassay", r$reason, ignore.case = TRUE))
})

test_that("a this-year sample whose result is in reads as the result, not as ID pending", {
  r <- classify(samples_5y = 1L, samples_this_year = 1L, pending_this_year = 0L,
                last_sample_date = D("2026-07-01"), last_sample_result = "B")
  expect_match(r$reason, "(blue)", fixed = TRUE)
  expect_false(grepl(coverage_env$COVERAGE_LABEL_ID_PENDING, r$reason, fixed = TRUE))
})

test_that("changing the label constants renames the wording everywhere it is built", {
  old <- c(coverage_env$COVERAGE_LABEL_COUNTING, coverage_env$COVERAGE_LABEL_NOT_COUNTING,
           coverage_env$COVERAGE_LABEL_ID_PENDING)
  on.exit({
    assign("COVERAGE_LABEL_COUNTING", old[1], envir = coverage_env)
    assign("COVERAGE_LABEL_NOT_COUNTING", old[2], envir = coverage_env)
    assign("COVERAGE_LABEL_ID_PENDING", old[3], envir = coverage_env)
  }, add = TRUE)
  assign("COVERAGE_LABEL_COUNTING", "Done", envir = coverage_env)
  assign("COVERAGE_LABEL_NOT_COUNTING", "Todo", envir = coverage_env)
  assign("COVERAGE_LABEL_ID_PENDING", "Waiting", envir = coverage_env)

  red <- classify(last_red_date_5y = D("2024-06-15"))
  expect_match(red$reason, "stays Done through", fixed = TRUE)
  sampled <- classify(samples_5y = 1L, samples_this_year = 1L, last_sample_date = D("2026-07-01"),
                      last_sample_result = "B")
  expect_match(sampled$reason, "drops to Todo on Jan 1", fixed = TRUE)
  waiting <- classify(samples_5y = 1L, samples_this_year = 1L, pending_this_year = 1L,
                      last_sample_date = D("2026-07-01"))
  expect_match(waiting$reason, "Waiting for 1 of 1 sample(s) this year", fixed = TRUE)
})

test_that("red bug + this-year sample: the later of the two end dates wins", {
  # red ages out well after Dec 31 -> red is the reason it keeps counting
  long_red <- classify(last_red_date_5y = D("2026-05-01"), samples_this_year = 1L, samples_5y = 1L)
  expect_equal(as.character(long_red$status), "red_5yr")
  expect_equal(long_red$drops_because, "Red bug ages out of 5-yr window")

  # red ages out Nov 15 but a sample this year keeps it counting until Dec 31
  short_red <- classify(last_red_date_5y = D("2021-11-15"), samples_this_year = 1L, samples_5y = 1L)
  expect_equal(as.character(short_red$status), "red_5yr")
  expect_equal(short_red$counts_through, D("2026-12-31"))
  expect_equal(short_red$drops_because, "This year's sample (resets Jan 1)")
})

test_that("every non-counting status is reachable, with precedence", {
  expect_equal(as.character(classify()$status), "not_inspected")
  expect_equal(as.character(classify(insp_5y = 3L)$status), "inspected_no_sample")
  expect_equal(as.character(classify(insp_5y = 3L, samples_5y = 2L, last_sample_result = "B",
                                     last_sample_date = D("2025-07-01"))$status), "no_red_sampled")
  expect_equal(as.character(classify(last_red_older = D("2019-07-01"))$status), "red_expired")

  # an older red bug beats "sampled, no red" - it is the more informative reason
  both <- classify(insp_5y = 3L, samples_5y = 2L, last_sample_result = "B",
                   last_sample_date = D("2025-07-01"), last_red_older = D("2019-07-01"))
  expect_equal(as.character(both$status), "red_expired")

  # this-year sample beats an expired red bug
  revived <- classify(last_red_older = D("2019-07-01"), samples_this_year = 1L, samples_5y = 1L)
  expect_equal(as.character(revived$status), "sampled_this_year")
})

test_that("only red_5yr and sampled_this_year count toward the goal", {
  sites <- do.call(rbind, list(
    make_site("a", last_red_date_5y = D("2025-06-01")),
    make_site("b", samples_this_year = 1L, samples_5y = 1L),
    make_site("c", last_red_older = D("2018-06-01")),
    make_site("d", samples_5y = 1L, last_sample_result = "B"),
    make_site("e", insp_5y = 4L),
    make_site("f")
  ))
  r <- coverage_env$classify_site_coverage(sites, AS_OF)
  expect_equal(r$counts, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(as.character(r$status),
               c("red_5yr", "sampled_this_year", "red_expired", "no_red_sampled",
                 "inspected_no_sample", "not_inspected"))
  expect_true(all(nzchar(r$reason)))
})

test_that("window start survives Feb 29 (no NA) and matches Postgres", {
  expect_equal(coverage_env$coverage_window_start(D("2024-02-29")), D("2019-02-28"))
  expect_equal(coverage_env$coverage_window_start(D("2026-10-27")), D("2021-10-27"))
})

test_that("likely_to_yield comes from how often the site is wet", {
  expect_equal(classify(wet_n = 1L, wet_count = 1L)$likely_to_yield, "Unknown")
  expect_equal(classify(wet_n = 10L, wet_count = 5L)$likely_to_yield, "High")
  expect_equal(classify(wet_n = 10L, wet_count = 2L)$likely_to_yield, "Medium")
  expect_equal(classify(wet_n = 10L, wet_count = 1L)$likely_to_yield, "Low")
  expect_true(is.na(classify()$wet_pct))
})

test_that("classify handles zero rows", {
  r <- coverage_env$classify_site_coverage(coverage_env$empty_site_coverage(), AS_OF)
  expect_equal(nrow(r), 0)
  expect_true(all(c("status", "counts", "reason", "counts_through") %in% names(r)))
})

# -----------------------------------------------------------------------------
# Summaries
# -----------------------------------------------------------------------------

counting_sites <- function(counting, total) {
  data.frame(
    facility = "N", counts = c(rep(TRUE, counting), rep(FALSE, total - counting)),
    status = factor(c(rep("red_5yr", counting), rep("not_inspected", total - counting)),
                    levels = coverage_env$COVERAGE_STATUS_LEVELS),
    stringsAsFactors = FALSE
  )
}

test_that("sites_needed is exact at the goal boundary", {
  s50 <- coverage_env$summarise_coverage(counting_sites(50, 100), goal = 51)
  expect_equal(c(s50$needed, s50$goal_met), c(1, 0))

  s51 <- coverage_env$summarise_coverage(counting_sites(51, 100), goal = 51)
  expect_equal(s51$needed, 0)
  expect_true(s51$goal_met)
  expect_equal(s51$cushion, 0)          # 0.51 * 100 must not round up to 52

  s60 <- coverage_env$summarise_coverage(counting_sites(60, 100), goal = 51)
  expect_equal(s60$cushion, 9)
})

test_that("summarise_coverage groups and totals consistently", {
  sites <- rbind(
    transform(counting_sites(3, 10), facility = "N"),
    transform(counting_sites(8, 10), facility = "Sj")
  )
  by_fac <- coverage_env$summarise_coverage(sites, by = "facility", goal = 51)
  expect_equal(by_fac$total, c(10, 10))
  expect_equal(by_fac$counting[by_fac$facility == "Sj"], 8)
  expect_equal(sum(by_fac$not_counting), 9)
})

test_that("percent labels truncate rather than round up to the goal", {
  expect_equal(coverage_env$coverage_pct_label(50.96), 50.9)
  expect_equal(coverage_env$coverage_pct_label(51), 51)
  expect_equal(coverage_env$coverage_pct_label(100 * 51 / 100), 51)
  expect_equal(coverage_env$coverage_pct_label(56.68), 56.6)
})

test_that("aging_out_by_month zero-fills and buckets the day AFTER counts_through", {
  sites <- coverage_env$classify_site_coverage(do.call(rbind, list(
    make_site("a", last_red_date_5y = D("2021-11-30")),                       # counts through 2026-11-30
    make_site("b", last_red_date_5y = D("2022-04-15")),                       # counts through 2027-04-15
    make_site("c", samples_this_year = 1L, samples_5y = 1L)                   # resets Jan 1
  )), AS_OF)
  # site a also has no sample this year, so it drops after Nov 30 -> December bucket
  aging <- coverage_env$aging_out_by_month(sites, AS_OF, months = 12)
  expect_equal(nrow(aging), 24)  # 12 months x 2 reasons
  red <- aging[aging$drops_because == "Red bug ages out of 5-yr window", ]
  expect_equal(red$n[red$month == D("2026-12-01")], 1)
  expect_equal(red$n[red$month == D("2027-04-01")], 1)
  reset <- aging[aging$drops_because == "This year's sample (resets Jan 1)", ]
  expect_equal(reset$n[reset$month == D("2027-01-01")], 1)
  expect_equal(sum(aging$n), 3)
})

# -----------------------------------------------------------------------------
# Overview registry contract (apps/overview calls load_raw_data(status_types = character(0)))
# -----------------------------------------------------------------------------

test_that("overview counts keep the shape the Overview expects", {
  real_loader <- coverage_env$load_prehatch_site_coverage
  on.exit(assign("load_prehatch_site_coverage", real_loader, envir = coverage_env), add = TRUE)
  assign("load_prehatch_site_coverage", function(as_of = Sys.Date(), ...) {
    do.call(rbind, list(
      make_site("a", fosarea = "0204", zone = "1", last_red_date_5y = D("2025-06-01")),
      make_site("b", fosarea = "0204", zone = "1"),
      make_site("c", facility = "Sj", fosarea = "7002", zone = "2", samples_this_year = 1L, samples_5y = 1L)
    ))
  }, envir = coverage_env)

  out <- coverage_env$get_coverage_overview_counts(AS_OF)
  expect_named(out, c("sites", "treatments", "pre_aggregated"))
  expect_true(out$pre_aggregated)
  expect_named(out$sites, c("facility", "fosarea", "zone", "total_count", "active_count", "expiring_count"))
  expect_type(out$sites$facility, "character")
  expect_type(out$sites$fosarea, "character")
  expect_type(out$sites$zone, "character")
  expect_type(out$sites$total_count, "integer")
  expect_equal(out$sites$active_count + out$sites$expiring_count, out$sites$total_count)
  expect_equal(sum(out$sites$total_count), 3)
  expect_equal(sum(out$sites$active_count), 2)
})

test_that("overview counts return an empty, correctly-shaped result when there is no data", {
  real_loader <- coverage_env$load_prehatch_site_coverage
  on.exit(assign("load_prehatch_site_coverage", real_loader, envir = coverage_env), add = TRUE)
  assign("load_prehatch_site_coverage", function(...) coverage_env$empty_site_coverage(), envir = coverage_env)

  out <- coverage_env$get_coverage_overview_counts(AS_OF)
  expect_equal(nrow(out$sites), 0)
  expect_named(out$sites, c("facility", "fosarea", "zone", "total_count", "active_count", "expiring_count"))
  expect_true(out$pre_aggregated)
})

# -----------------------------------------------------------------------------
# Progress chart + site table (display_functions.R)
# -----------------------------------------------------------------------------

# Load the display functions beside the coverage ones (constants resolve through the
# parent). get_status_colors lives in shared/db_helpers.R, which opens a pool - stub it.
display_env <- new.env(parent = coverage_env)
if (requireNamespace("plotly", quietly = TRUE) && requireNamespace("DT", quietly = TRUE)) {
  display_env$get_status_colors <- function(theme = "MMCD") {
    c(active = "#16a34a", completed = "#4169E1", planned = "#fdb73e",
      needs_action = "#FF4500", needs_treatment = "#FF0000", unknown = "#9ca3af")
  }
  suppressPackageStartupMessages(
    source(file.path(get_project_root(), "apps/inspections/display_functions.R"),
           local = display_env, chdir = TRUE)
  )
}

group_summary <- function(counting, total = 10) {
  do.call(rbind, lapply(seq_along(counting), function(i) {
    coverage_env$summarise_coverage(
      transform(counting_sites(counting[i], total), facility = paste0("F", i)),
      by = "facility", goal = 51)
  }))
}

test_that("the progress chart lists the most complete group first", {
  skip_if_not(exists("create_group_chart", envir = display_env), "plotly/DT not installed")
  s <- group_summary(c(3, 9, 6))   # F1 30%, F2 90%, F3 60%
  built <- plotly::plotly_build(display_env$create_group_chart(s, "facility", goal = 51))
  y_axis <- built$x$layout$yaxis

  expect_equal(y_axis$categoryarray, c("F2", "F3", "F1"))
  expect_equal(y_axis$autorange, "reversed")   # plotly draws the first category at the bottom otherwise
  expect_null(built$x$layout$height)           # sized by the output box, not baked into the plot
})

test_that("ties in the progress chart are ordered by sites still needed, then name", {
  skip_if_not(exists("create_group_chart", envir = display_env), "plotly/DT not installed")
  s <- group_summary(c(5, 5, 5))
  built <- plotly::plotly_build(display_env$create_group_chart(s, "facility", goal = 51))
  expect_equal(built$x$layout$yaxis$categoryarray, c("F1", "F2", "F3"))
})

test_that("the progress chart is tall enough for every bar (the FOS view was clipped)", {
  skip_if_not(exists("group_chart_height", envir = display_env), "plotly/DT not installed")
  h <- display_env$group_chart_height
  expect_equal(h(0), h(3))                                   # never collapses
  expect_gt(h(40), h(10))                                    # grows with the bars
  expect_gte(h(40), 40 * display_env$GROUP_ROW_PX)           # a row of space per bar
  expect_gt(h(40), 400)                                      # the old fixed plotlyOutput height
})

test_that("site table headers use the Sampled wording and an outstanding result reads 'Not entered yet'", {
  skip_if_not(exists("format_coverage_sites", envir = display_env), "plotly/DT not installed")
  sites <- coverage_env$classify_site_coverage(do.call(rbind, list(
    make_site("a", samples_5y = 1L, samples_this_year = 1L, pending_this_year = 1L,
              last_sample_date = D("2026-07-01")),
    make_site("b", samples_5y = 1L, samples_this_year = 1L, last_sample_date = D("2026-07-01"),
              last_sample_result = "R"),
    make_site("c")
  )), AS_OF)
  tbl <- display_env$format_coverage_sites(sites, link = FALSE)

  expect_true(paste(coverage_env$COVERAGE_LABEL_COUNTING, "through") %in% names(tbl))
  expect_false("Counts through" %in% names(tbl))
  expect_equal(tbl[["Last sample result"]], c("Not entered yet", "Red", NA))
  # how many of this year's samples are still waiting (the latest one may already have its result)
  expect_equal(tbl[[paste("Samples", coverage_env$COVERAGE_LABEL_ID_PENDING)]], c(1L, 0L, 0L))
})

test_that("ID pending sites are listed oldest sample first", {
  skip_if_not(exists("sort_sites_for_work", envir = display_env), "plotly/DT not installed")
  sites <- coverage_env$classify_site_coverage(do.call(rbind, list(
    make_site("new", samples_5y = 1L, samples_this_year = 1L, pending_this_year = 1L,
              last_sample_date = D("2026-10-01")),
    make_site("old", samples_5y = 1L, samples_this_year = 1L, pending_this_year = 1L,
              last_sample_date = D("2026-03-15")),
    make_site("mid", samples_5y = 1L, samples_this_year = 1L, pending_this_year = 1L,
              last_sample_date = D("2026-07-01"))
  )), AS_OF)
  expect_equal(display_env$sort_sites_for_work(sites, "id_pending")$sitecode, c("old", "mid", "new"))
})

# -----------------------------------------------------------------------------
# UI / display files load
# -----------------------------------------------------------------------------

test_that("display_functions can be sourced without errors", {
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/inspections/display_functions.R"), local = TRUE)
  })
})

test_that("ui_helper functions can be sourced without errors", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/inspections/ui_helper.R"), local = TRUE)
  })
})
