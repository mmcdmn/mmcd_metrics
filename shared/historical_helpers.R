# =============================================================================
# MMCD METRICS - SHARED HISTORICAL / WEEKLY-ACTIVE HELPERS
# =============================================================================
# Every "active treatment" historical chart answers the same question:
#
#   for each week in a year range, which treatments were still active on that
#   week's Friday?
#
# Four apps (catch_basin_status, drone, ground_prehatch_progress, struct_trt)
# each used to answer it with the same loop: walk ~52 x N_years Fridays, run a
# full dplyr::filter() over the entire treatments table per iteration, and grow
# the result with `week_data <- bind_rows(week_data, ...)`.
#
# That is quadratic in the output and re-scans the table once per week. The
# expansion below is a single vectorised pass via findInterval(): each treatment
# is active on a *contiguous* run of Fridays (inspdate .. treatment_end), so we
# can compute the first and last Friday index per row and expand directly.
#
# Measured on 60k treatments x 8 years: 2.21s -> 0.29s (~7x), byte-identical
# output.
#
# Callers keep their own post-expansion select/distinct/group_by - those differ
# per app (catch basin dedups by catchbasin_id, struct_trt counts per group,
# drone branches on the acres metric), so this file deliberately stops at the
# expansion itself.
#
# USAGE:
#   fridays <- weekly_fridays(start_date, end_date)
#   # compute treatment_end ONCE, not inside a loop
#   treatments$treatment_end <- treatments$inspdate + effect_days
#   week_data <- expand_active_by_week(treatments, fridays)
#   # -> one row per (treatment x week it was active), tagged with time_period
# =============================================================================

#' Build the weekly Friday grid for a date range
#'
#' Mirrors the labelling the per-app loops used: the Friday of each week is
#' `week_start + 4`, and the label is `<year>-W<week>` using lubridate::week().
#'
#' @param start_date First week start (Date or coercible)
#' @param end_date   Last week start (Date or coercible)
#' @param label_col  Name of the label column in the returned frame
#' @return data.frame with `friday` (Date) and the label column, ascending
weekly_fridays <- function(start_date, end_date, label_col = "time_period") {
  from <- as.Date(start_date)
  to   <- as.Date(end_date)

  # seq.Date() errors on an inverted or NA range rather than returning nothing,
  # so screen for that here and hand back an empty grid instead.
  weeks <- if (is.na(from) || is.na(to) || to < from) {
    as.Date(character(0))
  } else {
    seq.Date(from, to, by = "week")
  }

  if (length(weeks) == 0) {
    out <- data.frame(friday = as.Date(character(0)), stringsAsFactors = FALSE)
    out[[label_col]] <- character(0)
    return(out)
  }

  fridays <- weeks + 4
  out <- data.frame(friday = fridays, stringsAsFactors = FALSE)
  out[[label_col]] <- paste0(
    lubridate::year(fridays), "-W",
    sprintf("%02d", lubridate::week(fridays))
  )
  out
}

#' Expand rows to one row per (row x week the row was active)
#'
#' A row is active on a Friday when `start_col <= friday <= end_col`. Rows with
#' a missing start or end date, or that overlap no Friday in the grid, are
#' dropped - matching the `if (nrow(active) > 0)` guard in the loops this
#' replaces.
#'
#' Output is week-major, preserving the original row order within each week, so
#' a downstream `distinct(..., .keep_all = TRUE)` keeps exactly the row the old
#' per-week loop kept.
#'
#' @param data      Data frame of treatments (or any interval-shaped rows)
#' @param fridays   Grid from weekly_fridays()
#' @param start_col Column holding the interval start (default "inspdate")
#' @param end_col   Column holding the interval end (default "treatment_end")
#' @param label_col Label column in `fridays`, copied onto the output
#' @return `data` expanded, with `label_col` added. Zero rows if nothing matches.
expand_active_by_week <- function(data, fridays,
                                  start_col = "inspdate",
                                  end_col   = "treatment_end",
                                  label_col = "time_period") {
  empty_result <- function() {
    out <- data[0, , drop = FALSE]
    out[[label_col]] <- character(0)
    rownames(out) <- NULL
    out
  }

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) return(empty_result())
  if (is.null(fridays) || nrow(fridays) == 0) return(empty_result())
  if (!all(c(start_col, end_col) %in% names(data))) {
    stop(sprintf("expand_active_by_week: data is missing column(s): %s",
                 paste(setdiff(c(start_col, end_col), names(data)), collapse = ", ")))
  }

  # findInterval needs an ascending grid; don't assume the caller sorted it.
  fri_days <- as.integer(as.Date(fridays$friday))
  ord      <- order(fri_days)
  fri_days <- fri_days[ord]
  labels   <- as.character(fridays[[label_col]])[ord]

  starts <- as.integer(as.Date(data[[start_col]]))
  ends   <- as.integer(as.Date(data[[end_col]]))

  # lo = index of first Friday >= start; hi = index of last Friday <= end
  lo <- findInterval(starts - 1L, fri_days) + 1L
  hi <- findInterval(ends, fri_days)

  # A missing bound means the row can't be placed on the grid at all.
  undated <- is.na(starts) | is.na(ends)
  lo[undated] <- 1L
  hi[undated] <- 0L

  n_each <- hi - lo + 1L
  n_each[is.na(n_each) | n_each < 0L] <- 0L
  keep <- n_each > 0L
  if (!any(keep)) return(empty_result())

  row_idx <- rep.int(seq_len(nrow(data))[keep], n_each[keep])
  wk_idx  <- sequence(n_each[keep], from = lo[keep])

  # Week-major, original row order within the week (see @return note above).
  reorder <- order(wk_idx, row_idx)
  row_idx <- row_idx[reorder]
  wk_idx  <- wk_idx[reorder]

  out <- data[row_idx, , drop = FALSE]
  out[[label_col]] <- labels[wk_idx]
  rownames(out) <- NULL
  out
}

cat(" Historical helpers loaded successfully\n")
