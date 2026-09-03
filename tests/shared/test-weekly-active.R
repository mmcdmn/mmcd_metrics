# =============================================================================
# Tests for the weekly active-treatment expansion (shared/historical_helpers.R)
# =============================================================================
# These replace a per-week filter loop that lived in four apps. The tests pin
# the semantics that loop had, so a future change cannot silently drift:
#   - a row is active on a Friday when start <= friday <= end (both inclusive)
#   - output is week-major, original row order preserved within each week
#   - rows overlapping no Friday, or with a missing bound, are dropped
# =============================================================================

library(testthat)
library(dplyr)

context("Weekly active expansion")

# Reference implementation: the loop this helper replaced.
loop_reference <- function(data, start_date, end_date) {
  out <- data.frame()
  for (ws in seq.Date(as.Date(start_date), as.Date(end_date), by = "week")) {
    wf <- as.Date(ws, origin = "1970-01-01") + 4
    wl <- paste0(lubridate::year(wf), "-W",
                 sprintf("%02d", lubridate::week(wf)))
    active <- data[!is.na(data$inspdate) & !is.na(data$treatment_end) &
                     data$inspdate <= wf & data$treatment_end >= wf, , drop = FALSE]
    if (nrow(active) > 0) {
      active$time_period <- wl
      out <- rbind(out, active)
    }
  }
  rownames(out) <- NULL
  out
}

mk <- function(inspdate, effect_days, ...) {
  d <- data.frame(inspdate = as.Date(inspdate), stringsAsFactors = FALSE, ...)
  d$treatment_end <- d$inspdate + effect_days
  d
}

# ---------------------------------------------------------------------------
# weekly_fridays()
# ---------------------------------------------------------------------------

test_that("weekly_fridays returns one Friday per week with YYYY-Www labels", {
  f <- weekly_fridays("2024-01-01", "2024-01-31")
  expect_true(all(c("friday", "time_period") %in% names(f)))
  # Every returned date must actually be a Friday
  expect_true(all(weekdays(f$friday) == weekdays(as.Date("2024-01-05"))))
  expect_true(all(grepl("^[0-9]{4}-W[0-9]{2}$", f$time_period)))
  expect_equal(nrow(f), length(seq.Date(as.Date("2024-01-01"),
                                        as.Date("2024-01-31"), by = "week")))
})

test_that("weekly_fridays handles an empty range", {
  f <- weekly_fridays("2024-01-10", "2024-01-01")
  expect_equal(nrow(f), 0)
  expect_true(all(c("friday", "time_period") %in% names(f)))
})

# ---------------------------------------------------------------------------
# expand_active_by_week() - core semantics
# ---------------------------------------------------------------------------

test_that("expansion matches the per-week loop it replaced", {
  set.seed(7)
  n <- 400
  d <- mk(as.Date("2023-01-01") + sample(0:700, n, TRUE),
          sample(c(7, 14, 28), n, TRUE),
          site = sprintf("S%03d", sample(1:80, n, TRUE)))
  fridays <- weekly_fridays("2023-01-01", "2024-12-31")

  new <- expand_active_by_week(d, fridays)
  old <- loop_reference(d, "2023-01-01", "2024-12-31")

  expect_equal(nrow(new), nrow(old))
  expect_equal(new$time_period, old$time_period)
  expect_equal(new$site, old$site)
  expect_equal(new$inspdate, old$inspdate)
})

test_that("interval bounds are inclusive on both ends", {
  fridays <- weekly_fridays("2024-01-01", "2024-01-31")
  fri <- fridays$friday[2]
  # Treatment starting exactly on a Friday and ending exactly on it
  d <- mk(fri, 0, site = "A")
  res <- expand_active_by_week(d, fridays)
  expect_equal(nrow(res), 1)
  expect_equal(res$time_period, fridays$time_period[2])
})

test_that("rows overlapping no Friday are dropped", {
  fridays <- weekly_fridays("2024-01-01", "2024-01-31")
  # A one-day treatment on the Saturday right after a Friday
  d <- mk(fridays$friday[1] + 1, 0, site = "A")
  expect_equal(nrow(expand_active_by_week(d, fridays)), 0)
})

test_that("a treatment spanning several weeks emits one row per week", {
  fridays <- weekly_fridays("2024-01-01", "2024-03-31")
  d <- mk(fridays$friday[1], 21, site = "A")   # covers 4 Fridays
  res <- expand_active_by_week(d, fridays)
  expect_equal(nrow(res), 4)
  expect_equal(res$time_period, fridays$time_period[1:4])
})

test_that("rows with a missing start or end date are dropped", {
  fridays <- weekly_fridays("2024-01-01", "2024-01-31")
  d <- data.frame(inspdate = as.Date(c("2024-01-01", NA, "2024-01-01")),
                  treatment_end = as.Date(c("2024-01-31", "2024-01-31", NA)),
                  site = c("A", "B", "C"), stringsAsFactors = FALSE)
  res <- expand_active_by_week(d, fridays)
  expect_true(all(res$site == "A"))
})

test_that("output is week-major with original row order inside each week", {
  fridays <- weekly_fridays("2024-01-01", "2024-01-31")
  d <- mk(rep(as.Date("2024-01-01"), 3), 60, site = c("A", "B", "C"))
  res <- expand_active_by_week(d, fridays)
  # Each week must list A, B, C in that order
  by_week <- split(res$site, res$time_period)
  expect_true(all(vapply(by_week, function(x) identical(x, c("A", "B", "C")),
                         logical(1))))
})

# ---------------------------------------------------------------------------
# Edge cases and contract
# ---------------------------------------------------------------------------

test_that("empty inputs return an empty frame with the label column", {
  fridays <- weekly_fridays("2024-01-01", "2024-01-31")
  d <- mk(as.Date(character(0)), numeric(0), site = character(0))
  res <- expand_active_by_week(d, fridays)
  expect_equal(nrow(res), 0)
  expect_true("time_period" %in% names(res))

  d2 <- mk(as.Date("2024-01-01"), 30, site = "A")
  expect_equal(nrow(expand_active_by_week(d2, fridays[0, ])), 0)
})

test_that("an unsorted Friday grid still expands correctly", {
  fridays <- weekly_fridays("2024-01-01", "2024-03-31")
  d <- mk(fridays$friday[1], 21, site = "A")
  shuffled <- fridays[sample(nrow(fridays)), ]
  expect_equal(
    sort(expand_active_by_week(d, shuffled)$time_period),
    sort(expand_active_by_week(d, fridays)$time_period)
  )
})

test_that("custom column names are honoured", {
  fridays <- weekly_fridays("2024-01-01", "2024-01-31", label_col = "wk")
  d <- data.frame(from = as.Date("2024-01-01"), to = as.Date("2024-01-31"),
                  site = "A", stringsAsFactors = FALSE)
  res <- expand_active_by_week(d, fridays, start_col = "from",
                               end_col = "to", label_col = "wk")
  expect_true("wk" %in% names(res))
  # Only the Fridays that fall inside [from, to] - the last grid Friday is
  # 2024-02-02, past the interval end, so 4 of the 5 match.
  expect_equal(nrow(res), sum(fridays$friday >= d$from & fridays$friday <= d$to))
})

test_that("a missing interval column is an error, not a silent empty result", {
  fridays <- weekly_fridays("2024-01-01", "2024-01-31")
  d <- data.frame(inspdate = as.Date("2024-01-01"), site = "A")
  expect_error(expand_active_by_week(d, fridays), "treatment_end")
})

# ---------------------------------------------------------------------------
# The ground-prehatch PRE1ONLY rule, expressed as an interval
# ---------------------------------------------------------------------------

test_that("PRE1ONLY as a year-end interval matches the old year-equality test", {
  set.seed(11)
  n <- 200
  d <- mk(as.Date("2022-01-01") + sample(0:1000, n, TRUE),
          sample(c(7, 14), n, TRUE),
          site = sprintf("S%03d", sample(1:40, n, TRUE)))
  pre1 <- sample(unique(d$site), 15)
  d$is_pre1 <- d$site %in% pre1

  start_date <- as.Date("2022-01-01")
  end_date <- as.Date("2024-12-31")

  # Old behaviour: PRE1ONLY active for the rest of its calendar year
  old <- data.frame()
  for (ws in seq.Date(start_date, end_date, by = "week")) {
    wf <- as.Date(ws, origin = "1970-01-01") + 4
    wl <- paste0(lubridate::year(wf), "-W", sprintf("%02d", lubridate::week(wf)))
    act <- d[d$inspdate <= wf &
               ((d$is_pre1 & lubridate::year(d$inspdate) == lubridate::year(wf)) |
                (!d$is_pre1 & d$treatment_end >= wf)), , drop = FALSE]
    if (nrow(act) > 0) {
      act$time_period <- wl
      old <- rbind(old, act)
    }
  }
  rownames(old) <- NULL

  # New behaviour: fold the rule into treatment_end
  d2 <- d
  d2$treatment_end <- as.Date(ifelse(
    d2$is_pre1,
    as.Date(paste0(lubridate::year(d2$inspdate), "-12-31")),
    d2$treatment_end), origin = "1970-01-01")
  new <- expand_active_by_week(d2, weekly_fridays(start_date, end_date))

  expect_equal(nrow(new), nrow(old))
  expect_equal(new$time_period, old$time_period)
  expect_equal(new$site, old$site)
})
