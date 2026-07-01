# =============================================================================
# API Routes — Catch Basins
# =============================================================================
# Catch basins (loc_catchbasin) are storm-drain sumps — a SPECIFIC entity,
# distinct from structure treatments (loc_cxstruct: window wells, dry wells,
# tarps, tires, bird baths, etc.). They live in their own app
# (apps/catch_basin_status) and get their own route file + URL namespace so the
# two are never conflated.
#
# Sources the catch basin app's data_functions.R — NO new SQL.
# All endpoints are mounted under /v1/public/data/catch-basins/...
# =============================================================================

source("/srv/shiny-server/shared/db_helpers.R")
source("/srv/shiny-server/shared/app_libraries.R")
source("/srv/api/api_helpers.R")

cb_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/catch_basin_status/data_functions.R", local = cb_env, chdir = TRUE)

# ── Catch Basin Status ──

#* Get catch basin treatment status.
#* Returns aggregated treatment counts per section/facility with active/expiring/expired.
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname (e.g. "Alex D"). Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /
#* @serializer json
function(req, res,
         facility = NULL,
         foreman = NULL,
         zone = "1,2",
         analysis_date = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn    <- validate_zone(zone)
    adate <- validate_date(analysis_date)

    data <- cb_env$load_raw_data(
      analysis_date   = adate,
      facility_filter = fac,
      foreman_filter  = fman,
      zone_filter     = zn
    )

    sites <- data$sites
    if (is.null(sites) || nrow(sites) == 0) {
      return(list(count = 0L, total = data$total_count %||% 0L, data = list()))
    }

    list(
      count = nrow(sites),
      total = data$total_count %||% nrow(sites),
      data  = sites
    )
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Catch Basin Summary (value-box stats) ──

#* Get catch basin summary — total wet CBs, active(treated), expiring, expired, percent treated.
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname (e.g. "Alex D"). Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /summary
#* @serializer json
function(req, res,
         facility = NULL,
         foreman = NULL,
         zone = "1,2",
         analysis_date = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn    <- validate_zone(zone)
    adate <- validate_date(analysis_date)

    data <- cb_env$load_raw_data(
      analysis_date   = adate,
      facility_filter = fac,
      foreman_filter  = fman,
      zone_filter     = zn
    )

    sites <- data$sites
    if (is.null(sites) || nrow(sites) == 0) {
      return(list(
        analysis_date = as.character(adate),
        total_wet = 0L, total_treated = 0L, total_expiring = 0L,
        total_expired = 0L, percent_treated = 0
      ))
    }

    total_wet      <- sum(sites$total_count, na.rm = TRUE)
    total_treated  <- sum(sites$active_count, na.rm = TRUE)
    total_expiring <- sum(sites$expiring_count, na.rm = TRUE)
    total_expired  <- sum(sites$expired_count, na.rm = TRUE)
    pct            <- if (total_wet > 0) round(100 * total_treated / total_wet, 1) else 0

    list(
      analysis_date   = as.character(adate),
      filters         = list(facility = fac, foreman = foreman, zone = zn),
      total_wet       = total_wet,
      total_treated   = total_treated,
      total_expiring  = total_expiring,
      total_expired   = total_expired,
      percent_treated = pct
    )
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Catch Basin Summary BY FACILITY ──

#* Get catch basin summary broken down by facility — one row per facility with totals.
#* Use for facility comparisons, charts, and LLM multi-facility queries.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /summary-by-facility
#* @serializer json
function(req, res,
         zone = "1,2",
         analysis_date = NULL) {
  tryCatch({
    zn    <- validate_zone(zone)
    adate <- validate_date(analysis_date)

    data <- cb_env$load_raw_data(
      analysis_date   = adate,
      facility_filter = "all",
      foreman_filter  = "all",
      zone_filter     = zn
    )

    grouped <- cb_env$process_catch_basin_data(
      data,
      group_by     = "facility",
      combine_zones = TRUE
    )

    if (is.null(grouped) || nrow(grouped) == 0) {
      return(list(analysis_date = as.character(adate), facility_summaries = list()))
    }

    rows <- lapply(seq_len(nrow(grouped)), function(i) {
      r <- grouped[i, ]
      list(
        facility        = r$display_name,
        total_count     = as.integer(r$total_count),
        active_count    = as.integer(r$active_count),
        expiring_count  = as.integer(r$expiring_count %||% 0),
        expired_count   = as.integer(r$expired_count %||% 0),
        pct_treated     = round(as.numeric(r$pct_treated %||% 0), 1)
      )
    })

    list(
      analysis_date      = as.character(adate),
      facility_summaries = rows
    )
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Catch Basin Expiration Schedule ──

#* Get WHEN wet catch basin treatments expire, as day-window buckets.
#* Reuses the catch basin app's load_raw_data() — calling it at several
#* `expiring_days` thresholds and differencing the expiring counts builds a
#* timeline (next 14 days, 15-30, 31-60, 61-90, beyond 90, already expired)
#* WITHOUT any new SQL. Answers "when do the catch basins expire" /
#* "what date do most expire" / "is it a long way off".
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname (e.g. "Alex D"). Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /expiration-schedule
#* @serializer json
function(req, res,
         facility = NULL,
         foreman = NULL,
         zone = "1,2",
         analysis_date = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn    <- validate_zone(zone)
    adate <- validate_date(analysis_date)

    # load_raw_data() flags a basin as "expiring" when it lapses within
    # `expiring_days`. Cumulative counts at increasing thresholds let us bucket
    # the upcoming expirations. total_active / total_expired are independent of
    # the threshold, so we read them from any populated call.
    thresholds <- c(14L, 30L, 60L, 90L)
    cumulative <- setNames(integer(length(thresholds)), as.character(thresholds))
    total_active  <- 0L
    total_expired <- 0L
    have_totals   <- FALSE

    for (n in thresholds) {
      d <- cb_env$load_raw_data(
        analysis_date   = adate,
        facility_filter = fac,
        foreman_filter  = fman,
        zone_filter     = zn,
        expiring_days   = n
      )
      s <- d$sites
      if (is.null(s) || nrow(s) == 0) next
      cumulative[as.character(n)] <- sum(s$expiring_count, na.rm = TRUE)
      if (!have_totals) {
        total_active  <- sum(s$active_count, na.rm = TRUE)
        total_expired <- sum(s$expired_count, na.rm = TRUE)
        have_totals   <- TRUE
      }
    }

    nz <- function(x) max(0L, as.integer(x))
    c14 <- cumulative["14"]; c30 <- cumulative["30"]
    c60 <- cumulative["60"]; c90 <- cumulative["90"]

    buckets <- list(
      list(label = "next 14 days",  max_days = 14L, count = nz(c14)),
      list(label = "15-30 days",    max_days = 30L, count = nz(c30 - c14)),
      list(label = "31-60 days",    max_days = 60L, count = nz(c60 - c30)),
      list(label = "61-90 days",    max_days = 90L, count = nz(c90 - c60)),
      list(label = "beyond 90 days", max_days = NA, count = nz(total_active - c90))
    )

    # Soonest = first non-empty bucket; peak = bucket with the most basins
    nonzero <- Filter(function(b) b$count > 0, buckets)
    soonest <- if (length(nonzero) > 0) nonzero[[1]] else NULL
    peak <- NULL
    for (b in buckets) {
      if (is.null(peak) || b$count > peak$count) peak <- b
    }
    if (!is.null(peak) && peak$count == 0) peak <- NULL

    list(
      analysis_date = as.character(adate),
      filters       = list(facility = fac, foreman = foreman %||% "all", zone = zn),
      total_active  = as.integer(total_active),
      total_expired = as.integer(total_expired),
      soonest       = soonest,
      peak          = peak,
      buckets       = buckets
    )
  }, error = function(e) api_error(res, 400, e$message))
}
