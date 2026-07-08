# =============================================================================
# API Routes — Drone Treatment Status
# =============================================================================
# Current drone-treatment status for drone breeding sites (loc_breeding_sites with
# drone IN ('Y','M','C') OR air_gnd='D'). Mirrors the drone dashboard app's own
# filter surface — sourced from apps/drone/data_functions.R (NO new SQL).
#
# This is DISTINCT from the private drone CHECKLIST (drone.R at /v1/private/drone),
# which returns per-round records for the Sheets filler. Mounted under
# /v1/public/data/drone/...
#
# Filters (all from the drone app, NOT the overview): drone_types (Y/M/C),
# prehatch_only, facility, foreman, zone, town. Group-by: facility/foreman/mmcd_all.
# Status logic (from the app): is_active = treatment_end_date >= today;
# is_expiring = active AND end_date <= today+7 (expiring is a subset of active).
# =============================================================================

source("/srv/shiny-server/shared/db_helpers.R")
source("/srv/shiny-server/shared/app_libraries.R")
source("/srv/api/api_helpers.R")

drone_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/drone/data_functions.R", local = drone_env, chdir = TRUE)

DRONE_GROUP_BYS <- c("mmcd_all", "facility", "foreman")

# drone_types: subset of Y (autonomous), M (manual), C. Default all three.
.validate_drone_types <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return(c("Y", "M", "C"))
  parts <- trimws(toupper(unlist(strsplit(as.character(v), ",", fixed = TRUE))))
  bad <- parts[!parts %in% c("Y", "M", "C")]
  if (length(bad) > 0) stop("drone_types must be Y, M, or C (comma-separated)")
  parts
}

# Load drone status with the full filter surface applied, reusing app functions only.
# apply_data_filters converts foreman SHORTNAMES -> emp_num, so pass the shortname.
.load_drone <- function(adate, dtypes, fac, fman, zn, tc, prehatch_only) {
  data <- drone_env$load_raw_data(drone_types = dtypes, analysis_date = adate)
  data <- drone_env$apply_data_filters(
    data, facility_filter = fac, foreman_filter = fman,
    zone_filter = zn, prehatch_only = prehatch_only
  )
  data$sites      <- filter_sites_by_town(data$sites, tc)
  data$treatments <- filter_sites_by_town(data$treatments, tc)
  data
}

# Roll drone sites up by facility / foreman / mmcd_all -> clean rows list.
.drone_group_rows <- function(sites, group_by) {
  if (is.null(sites) || nrow(sites) == 0) return(list())
  sites$.k <- switch(group_by,
    mmcd_all = rep("All MMCD", nrow(sites)),
    facility = as.character(sites$facility),
    foreman  = as.character(sites$foreman),
    as.character(sites$facility)
  )
  fac_lkp <- tryCatch(get_facility_lookup(), error = function(e) NULL)
  fos_lkp <- tryCatch(get_foremen_lookup(),  error = function(e) NULL)
  lapply(sort(unique(as.character(sites$.k))), function(k) {
    sub    <- sites[sites$.k == k, ]
    total  <- nrow(sub)
    active <- sum(sub$is_active == TRUE, na.rm = TRUE)
    disp <- switch(group_by,
      facility = { m <- if (!is.null(fac_lkp)) fac_lkp$full_name[match(k, fac_lkp$short_name)] else NA
                   if (length(m) && !is.na(m)) m else k },
      foreman  = { m <- if (!is.null(fos_lkp)) fos_lkp$shortname[match(k, as.character(fos_lkp$emp_num))] else NA
                   if (length(m) && !is.na(m)) m else paste("FOS", k) },
      k
    )
    list(
      group          = k,
      display_name   = disp,
      total_count    = total,
      active_count   = as.integer(active),
      expiring_count = as.integer(sum(sub$is_expiring == TRUE, na.rm = TRUE)),
      expired_count  = as.integer(total - active),
      total_acres    = round(sum(sub$acres, na.rm = TRUE), 2),
      active_acres   = round(sum(ifelse(sub$is_active == TRUE, sub$acres, 0), na.rm = TRUE), 2),
      pct_active     = if (total > 0) round(100 * active / total, 1) else 0
    )
  })
}

# ── Drone Status Summary (value-box stats) ──

#* Get drone treatment summary — total sites/acres, active, expiring, expired, % active.
#* @param drone_types Drone designation: Y, M, C (comma-separated). Default Y,M,C.
#* @param prehatch_only If true, only prehatch drone sites. Default false.
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname (e.g. "Alex D"). Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param town Township/city name (e.g. Eagan) or 4-digit town code. Omit for all towns.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /summary
#* @serializer json
function(req, res,
         drone_types = NULL, prehatch_only = "false", facility = NULL,
         foreman = NULL, zone = "1,2", town = NULL, analysis_date = NULL) {
  tryCatch({
    dtypes <- .validate_drone_types(drone_types)
    ph     <- isTRUE(as.logical(prehatch_only))
    fac    <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman   <- if (!is.null(foreman) && nzchar(foreman)) { validate_foreman(foreman); clean_text(foreman) } else "all"
    zn     <- validate_zone(zone)
    tc     <- validate_town(town)
    adate  <- validate_date(analysis_date)

    data  <- .load_drone(adate, dtypes, fac, fman, zn, tc, ph)
    sites <- data$sites
    if (is.null(sites) || nrow(sites) == 0) {
      return(list(analysis_date = as.character(adate),
                  total_sites = 0L, active_count = 0L, expiring_count = 0L,
                  expired_count = 0L, active_pct = 0, total_acres = 0, active_acres = 0))
    }
    total  <- nrow(sites)
    active <- sum(sites$is_active == TRUE, na.rm = TRUE)
    list(
      analysis_date  = as.character(adate),
      filters        = list(drone_types = dtypes, prehatch_only = ph, facility = fac,
                            foreman = foreman %||% "all", zone = zn, town = tc %||% "all"),
      total_sites    = total,
      active_count   = as.integer(active),
      expiring_count = as.integer(sum(sites$is_expiring == TRUE, na.rm = TRUE)),
      expired_count  = as.integer(total - active),
      active_pct     = if (total > 0) round(100 * active / total, 1) else 0,
      total_acres    = round(sum(sites$acres, na.rm = TRUE), 2),
      active_acres   = round(sum(ifelse(sites$is_active == TRUE, sites$acres, 0), na.rm = TRUE), 2)
    )
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Drone Status Summary BY GROUP (facility / foreman / mmcd_all) ──

#* Get drone treatment summary rolled up by a chosen dimension.
#* @param group_by One of: facility, foreman, mmcd_all. Default facility.
#* @param drone_types Drone designation: Y, M, C (comma-separated). Default Y,M,C.
#* @param prehatch_only If true, only prehatch drone sites. Default false.
#* @param facility Facility code to narrow to. Omit for all.
#* @param foreman FOS shortname to narrow to. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param town Township/city name or 4-digit code to narrow to. Omit for all.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /summary-by-group
#* @serializer json
function(req, res,
         group_by = "facility", drone_types = NULL, prehatch_only = "false",
         facility = NULL, foreman = NULL, zone = "1,2", town = NULL, analysis_date = NULL) {
  tryCatch({
    grp    <- validate_group_by(group_by, DRONE_GROUP_BYS, "facility")
    dtypes <- .validate_drone_types(drone_types)
    ph     <- isTRUE(as.logical(prehatch_only))
    fac    <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman   <- if (!is.null(foreman) && nzchar(foreman)) { validate_foreman(foreman); clean_text(foreman) } else "all"
    zn     <- validate_zone(zone)
    tc     <- validate_town(town)
    adate  <- validate_date(analysis_date)

    data <- .load_drone(adate, dtypes, fac, fman, zn, tc, ph)
    list(
      analysis_date = as.character(adate),
      group_by      = grp,
      groups        = .drone_group_rows(data$sites, grp)
    )
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Drone Expiration Schedule ──

#* Get WHEN drone treatments expire, as day-window buckets (next 14 / 15-30 / 31-60 /
#* 61-90 / beyond 90) plus soonest and peak windows. Computed from each active site's
#* latest treatment_end_date (inspdate + effect_days) — NO new SQL.
#* @param drone_types Drone designation: Y, M, C (comma-separated). Default Y,M,C.
#* @param prehatch_only If true, only prehatch drone sites. Default false.
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param town Township/city name or 4-digit code. Omit for all.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /expiration-schedule
#* @serializer json
function(req, res,
         drone_types = NULL, prehatch_only = "false", facility = NULL,
         foreman = NULL, zone = "1,2", town = NULL, analysis_date = NULL) {
  tryCatch({
    dtypes <- .validate_drone_types(drone_types)
    ph     <- isTRUE(as.logical(prehatch_only))
    fac    <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman   <- if (!is.null(foreman) && nzchar(foreman)) { validate_foreman(foreman); clean_text(foreman) } else "all"
    zn     <- validate_zone(zone)
    tc     <- validate_town(town)
    adate  <- validate_date(analysis_date)

    data  <- .load_drone(adate, dtypes, fac, fman, zn, tc, ph)
    sites <- data$sites
    trt   <- data$treatments
    total_active  <- 0L
    total_expired <- if (!is.null(sites) && nrow(sites) > 0) sum(sites$is_active == FALSE, na.rm = TRUE) else 0L
    days_until <- numeric(0)
    if (!is.null(trt) && nrow(trt) > 0 && "treatment_end_date" %in% names(trt)) {
      latest <- trt %>%
        dplyr::group_by(sitecode) %>%
        dplyr::arrange(dplyr::desc(inspdate)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::filter(is_active == TRUE)
      days_until <- as.numeric(as.Date(latest$treatment_end_date) - as.Date(adate))
      days_until <- days_until[!is.na(days_until) & days_until >= 0]
      total_active <- length(days_until)
    }

    sched <- build_expiration_schedule(function(n) {
      list(expiring = sum(days_until <= n), active = total_active, expired = total_expired)
    })

    c(list(
      analysis_date = as.character(adate),
      filters       = list(drone_types = dtypes, prehatch_only = ph, facility = fac,
                          foreman = foreman %||% "all", zone = zn, town = tc %||% "all")
    ), sched)
  }, error = function(e) api_error(res, 400, e$message))
}
