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

CB_GROUP_BYS <- c("mmcd_all", "facility", "foreman", "sectcode", "township")
# Default expiring window — from the shared config (config/app_config.yaml
# metric_defaults.catch_basin.expiring_days), same source the overview reads. Fallback 7.
CB_EXPIRING_DEFAULT <- as.integer(get_metric_default("catch_basin", "expiring_days", 7L))
CB_EXPIRING_FILTERS <- c("all", "expiring", "expiring_expired")

# Filter the pre-aggregated section rows by expiring status (matches the app's
# "Site Filter" control): "expiring" keeps sections with any expiring treatments,
# "expiring_expired" keeps sections with expiring OR already-expired treatments.
.cb_expiring_filter <- function(sites, expiring_filter) {
  if (is.null(sites) || nrow(sites) == 0) return(sites)
  ef <- expiring_filter %||% "all"
  if (ef == "all") return(sites)
  ec <- if ("expiring_count" %in% names(sites)) sites$expiring_count else rep(0L, nrow(sites))
  xc <- if ("expired_count"  %in% names(sites)) sites$expired_count  else rep(0L, nrow(sites))
  keep <- if (ef == "expiring_expired") (ec > 0) | (xc > 0) else (ec > 0)
  sites[which(keep), , drop = FALSE]
}

# Roll the pre-aggregated catch-basin section rows up by a chosen dimension.
# load_raw_data() returns one row per (facility, zone, fosarea, sectcode) with
# total/active/expiring/expired counts plus facility_full/foreman_name display names.
.cb_group_rows <- function(sites, group_by) {
  if (is.null(sites) || nrow(sites) == 0) return(list())
  town_lkp <- if (group_by == "township") tryCatch(get_town_lookup(), error = function(e) NULL) else NULL
  sites$.k <- switch(group_by,
    mmcd_all = rep("All MMCD", nrow(sites)),
    facility = as.character(sites$facility),
    foreman  = as.character(sites$fosarea),
    sectcode = as.character(sites$sectcode),
    township = substr(as.character(sites$sectcode), 1, 4),
    as.character(sites$facility)
  )
  lapply(sort(unique(sites$.k)), function(k) {
    sub    <- sites[sites$.k == k, ]
    total  <- sum(sub$total_count,  na.rm = TRUE)
    active <- sum(sub$active_count, na.rm = TRUE)
    disp <- switch(group_by,
      facility = as.character(sub$facility_full[1]),
      foreman  = as.character(sub$foreman_name[1]),
      township = { m <- if (!is.null(town_lkp)) town_lkp$city[match(k, town_lkp$towncode4)] else NA
                   if (length(m) && !is.na(m)) m else k },
      k
    )
    list(
      group          = k,
      display_name   = disp,
      total_count    = as.integer(total),
      active_count   = as.integer(active),
      expiring_count = as.integer(sum(sub$expiring_count, na.rm = TRUE)),
      expired_count  = as.integer(sum(sub$expired_count,  na.rm = TRUE)),
      pct_treated    = if (total > 0) round(100 * active / total, 1) else 0
    )
  })
}

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
#* @param town Township/city name (e.g. Eagan) or 4-digit town code. Omit for all towns.
#* @param expiring_days Days-ahead window that counts as "expiring" (1-60). Default from registry (7).
#* @param expiring_filter Site filter: all, expiring, or expiring_expired. Default all.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /summary
#* @serializer json
function(req, res,
         facility = NULL,
         foreman = NULL,
         zone = "1,2",
         town = NULL,
         expiring_days = NULL,
         expiring_filter = "all",
         analysis_date = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn    <- validate_zone(zone)
    tc    <- validate_town(town)
    ef    <- validate_group_by(expiring_filter, CB_EXPIRING_FILTERS, "all")
    exdays <- suppressWarnings(as.integer(expiring_days %||% CB_EXPIRING_DEFAULT))
    if (is.na(exdays) || exdays < 1L || exdays > 60L) stop("expiring_days must be between 1 and 60")
    adate <- validate_date(analysis_date)

    data <- cb_env$load_raw_data(
      analysis_date   = adate,
      facility_filter = fac,
      foreman_filter  = fman,
      zone_filter     = zn,
      expiring_days   = exdays
    )

    sites <- .cb_expiring_filter(filter_sites_by_town(data$sites, tc), ef)
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
      expiring_days   = exdays,
      filters         = list(facility = fac, foreman = foreman, zone = zn,
                            town = tc %||% "all", expiring_filter = ef),
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

# ── Catch Basin Summary BY GROUP (mmcd_all / facility / foreman / sectcode / township) ──

#* Get catch basin summary rolled up by a chosen dimension.
#* @param group_by One of: mmcd_all, facility, foreman, sectcode, township. Default facility.
#* @param facility Facility code to narrow to. Omit for all.
#* @param foreman FOS shortname to narrow to. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param town Township/city name or 4-digit town code to narrow to. Omit for all.
#* @param expiring_days Days-ahead window that counts as "expiring" (1-60). Default from registry (7).
#* @param expiring_filter Site filter: all, expiring, or expiring_expired. Default all.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /summary-by-group
#* @serializer json
function(req, res,
         group_by = "facility", facility = NULL, foreman = NULL,
         zone = "1,2", town = NULL, expiring_days = NULL,
         expiring_filter = "all", analysis_date = NULL) {
  tryCatch({
    grp   <- validate_group_by(group_by, CB_GROUP_BYS, "facility")
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn    <- validate_zone(zone)
    tc    <- validate_town(town)
    ef    <- validate_group_by(expiring_filter, CB_EXPIRING_FILTERS, "all")
    exdays <- suppressWarnings(as.integer(expiring_days %||% CB_EXPIRING_DEFAULT))
    if (is.na(exdays) || exdays < 1L || exdays > 60L) stop("expiring_days must be between 1 and 60")
    adate <- validate_date(analysis_date)

    data  <- cb_env$load_raw_data(
      analysis_date   = adate,
      facility_filter = fac,
      foreman_filter  = fman,
      zone_filter     = zn,
      expiring_days   = exdays
    )
    sites <- .cb_expiring_filter(filter_sites_by_town(data$sites, tc), ef)
    list(
      analysis_date   = as.character(adate),
      group_by        = grp,
      expiring_days   = exdays,
      expiring_filter = ef,
      groups          = .cb_group_rows(sites, grp)
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
#* @param town Township/city name (e.g. Eagan) or 4-digit town code. Omit for all towns.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /expiration-schedule
#* @serializer json
function(req, res,
         facility = NULL,
         foreman = NULL,
         zone = "1,2",
         town = NULL,
         analysis_date = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn    <- validate_zone(zone)
    tc    <- validate_town(town)
    adate <- validate_date(analysis_date)

    # load_raw_data() flags a basin as "expiring" when it lapses within `expiring_days`.
    # Calling it at increasing thresholds and differencing the cumulative expiring
    # counts builds the day-window timeline (shared build_expiration_schedule helper).
    sched <- build_expiration_schedule(function(n) {
      d <- cb_env$load_raw_data(
        analysis_date   = adate,
        facility_filter = fac,
        foreman_filter  = fman,
        zone_filter     = zn,
        expiring_days   = n
      )
      s <- filter_sites_by_town(d$sites, tc)
      if (is.null(s) || nrow(s) == 0) return(NULL)
      list(
        expiring = sum(s$expiring_count, na.rm = TRUE),
        active   = sum(s$active_count,   na.rm = TRUE),
        expired  = sum(s$expired_count,  na.rm = TRUE)
      )
    })

    c(list(
      analysis_date = as.character(adate),
      filters       = list(facility = fac, foreman = foreman %||% "all",
                          zone = zn, town = tc %||% "all")
    ), sched)
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Catch Basin Treatments By Week (time series) ──

#* Get catch basin treatment counts bucketed by week, over a date range.
#* Unlike the snapshot-style endpoints above (one point-in-time analysis_date),
#* this counts individual treatment EVENTS (inspdate) within the requested
#* range -- answers "how many treatments happened each week / this year",
#* a real trend, not "what does the status look like as of one date".
#* Reuses the app's load_historical_treatments() (individual treatment rows
#* with inspdate, via load_raw_data(include_archive=TRUE, ...)) -- NO new SQL.
#* Returns a bare array (one point per week), matching the surveillance
#* mle-trend endpoint's convention: [{"yrwk": 202619, "treated_count": 12}, ...].
#* @param facility Facility code. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param start_date Start of the range (YYYY-MM-DD). Default January 1 of the current year.
#* @param end_date End of the range (YYYY-MM-DD). Default today.
#* @get /treatments-by-week
#* @serializer json
function(req, res,
         facility = NULL,
         zone = "1,2",
         start_date = NULL,
         end_date = NULL) {
  tryCatch({
    fac <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    zn  <- validate_zone(zone)

    today <- Sys.Date()
    sd <- if (!is.null(start_date) && nzchar(start_date)) {
      d <- tryCatch(as.Date(start_date, "%Y-%m-%d"), error = function(e) as.Date(NA))
      if (is.na(d)) stop("start_date must be YYYY-MM-DD")
      d
    } else {
      as.Date(sprintf("%d-01-01", as.integer(format(today, "%Y"))))
    }
    ed <- if (!is.null(end_date) && nzchar(end_date)) {
      d <- tryCatch(as.Date(end_date, "%Y-%m-%d"), error = function(e) as.Date(NA))
      if (is.na(d)) stop("end_date must be YYYY-MM-DD")
      d
    } else {
      today
    }
    if (ed > today + 1L) stop("end_date cannot be in the future")
    if (sd > ed) stop("start_date must be before end_date")
    if (as.numeric(ed - sd) > 5 * 365) stop("date range cannot exceed 5 years")

    start_year <- as.integer(format(sd, "%Y"))
    end_year   <- as.integer(format(ed, "%Y"))

    data <- cb_env$load_raw_data(
      include_archive = TRUE,
      start_year      = start_year,
      end_year        = end_year,
      zone_filter     = zn
    )
    treatments <- data$treatments
    if (!is.null(treatments) && nrow(treatments) > 0) {
      treatments <- treatments[treatments$inspdate >= sd & treatments$inspdate <= ed, , drop = FALSE]
      if (!identical(fac, "all")) {
        treatments <- treatments[treatments$facility == fac, , drop = FALSE]
      }
    }

    if (is.null(treatments) || nrow(treatments) == 0) {
      return(list())
    }

    weekly <- treatments %>%
      mutate(yrwk = year(inspdate) * 100L + week(inspdate)) %>%
      group_by(yrwk) %>%
      summarize(treated_count = n(), .groups = "drop") %>%
      arrange(yrwk)

    lapply(seq_len(nrow(weekly)), function(i) {
      list(yrwk = as.integer(weekly$yrwk[i]), treated_count = as.integer(weekly$treated_count[i]))
    })
  }, error = function(e) api_error(res, 400, e$message))
}
