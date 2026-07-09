# =============================================================================
# API Routes — Breeding Sites
# =============================================================================
# Endpoints for air sites, ground prehatch sites, and section card data.
# Sources existing data_functions.R from each app — NO new SQL.
#
# All endpoints are mounted under /v1/public/data/breeding/...
# =============================================================================

# Shared helpers (validation, DB connection, error formatting)
source("/srv/shiny-server/shared/db_helpers.R")
source("/srv/shiny-server/shared/app_libraries.R")
source("/srv/api/api_helpers.R")

# Source app data functions (paths are container-absolute)
source("/srv/shiny-server/apps/air_sites_simple/data_functions.R")

# Ground prehatch has its own load_raw_data — load into a namespace to avoid collision
ground_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/ground_prehatch_progress/data_functions.R", local = ground_env, chdir = TRUE)

# Section cards
cards_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/section-cards/data_functions.R", local = cards_env, chdir = TRUE)

# ── Ground prehatch shared helpers ──
# Ground prehatch default expiring window: registry-first (7, matching the app's UI slider
# default), NOT the loader's raw default of 14. Group-by dimensions come straight from the
# site-level details (facility/foreman/sectcode, plus township = first 4 of sectcode, plus mmcd_all).
GROUND_EXPIRING_DEFAULT <- as.integer(registry_default("ground_prehatch", "expiring_days", 7L))
GROUND_GROUP_BYS <- c("mmcd_all", "township", "sectcode", "facility", "foreman")

.validate_expiring_filter <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return("all")
  s <- tolower(trimws(as.character(v)))
  if (!s %in% c("all", "expiring", "expiring_expired")) {
    stop("expiring_filter must be all, expiring, or expiring_expired")
  }
  s
}

# Load ground prehatch site-level details with the FULL app filter surface applied
# (matches ground_prehatch app.R: zone, facility, foreman, include_drone, expiring_filter).
# Reuses the app's get_site_details_data + filter_ground_data (no new SQL); town is a
# sectcode-prefix filter. expiring_filter narrows to expiring / expiring+expired sites.
.load_ground <- function(adate, exdays, fac, fman, zn, tc,
                         include_drone = FALSE, expiring_filter = "all") {
  details <- ground_env$get_site_details_data(expiring_days = exdays, analysis_date = adate)
  if (is.null(details) || nrow(details) == 0) return(details)
  details <- ground_env$filter_ground_data(
    details, zone_filter = zn, facility_filter = fac,
    foreman_filter = fman, include_drone = include_drone
  )
  details <- filter_sites_by_town(details, tc)
  if (!is.null(details) && nrow(details) > 0) {
    if (identical(expiring_filter, "expiring")) {
      details <- details[details$prehatch_status == "expiring", , drop = FALSE]
    } else if (identical(expiring_filter, "expiring_expired")) {
      details <- details[details$prehatch_status %in% c("expiring", "expired"), , drop = FALSE]
    }
  }
  details
}

# Roll ground prehatch details up by a chosen dimension -> clean rows list.
.ground_group_rows <- function(details, group_by) {
  if (is.null(details) || nrow(details) == 0) return(list())
  details$.grp <- switch(group_by,
    mmcd_all = rep("All MMCD", nrow(details)),
    township = substr(as.character(details$sectcode), 1, 4),
    sectcode = as.character(details$sectcode),
    facility = as.character(details$facility),
    foreman  = as.character(details$foreman),
    as.character(details$facility)
  )
  fac_lkp  <- tryCatch(get_facility_lookup(), error = function(e) NULL)
  fos_lkp  <- tryCatch(get_foremen_lookup(),  error = function(e) NULL)
  town_lkp <- if (group_by == "township") tryCatch(get_town_lookup(), error = function(e) NULL) else NULL
  disp <- function(k) {
    if (group_by == "facility" && !is.null(fac_lkp)) {
      m <- fac_lkp$full_name[match(k, fac_lkp$short_name)]; if (length(m) && !is.na(m)) return(m)
    } else if (group_by == "foreman" && !is.null(fos_lkp)) {
      m <- fos_lkp$shortname[match(k, as.character(fos_lkp$emp_num))]; if (length(m) && !is.na(m)) return(m)
    } else if (group_by == "township" && !is.null(town_lkp)) {
      m <- town_lkp$city[match(k, town_lkp$towncode4)]; if (length(m) && !is.na(m)) return(m)
    }
    k
  }
  lapply(sort(unique(details$.grp)), function(k) {
    sub      <- details[details$.grp == k, ]
    treated  <- sum(sub$prehatch_status == "treated",  na.rm = TRUE)
    expiring <- sum(sub$prehatch_status == "expiring", na.rm = TRUE)
    expired  <- sum(sub$prehatch_status == "expired",  na.rm = TRUE)
    active   <- treated + expiring
    list(
      group          = k,
      display_name   = disp(k),
      total_count    = nrow(sub),
      active_count   = active,
      treated_count  = treated,
      expiring_count = expiring,
      expired_count  = expired,
      total_acres    = round(sum(sub$acres, na.rm = TRUE), 2),
      active_acres   = round(sum(ifelse(sub$prehatch_status %in% c("treated", "expiring"), sub$acres, 0), na.rm = TRUE), 2),
      pct_treated    = if ((active + expired) > 0) round(100 * active / (active + expired), 1) else 0
    )
  })
}

# ── Air Sites ──

# ── Red air shared helpers ──
# Red air statuses (the dashboard "Status Filter" values).
AIR_STATUSES <- c("Active Treatment", "Inspected", "Needs ID", "Needs Treatment", "Unknown")

.air_zone <- function(zn) {
  if (length(zn) >= 2) "P1 + P2 Combined" else if ("1" %in% zn) "P1" else if ("2" %in% zn) "P2" else NULL
}
.validate_air_status <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return("all")
  s <- trimws(as.character(v))
  if (tolower(s) == "all") return("all")
  if (!s %in% AIR_STATUSES) stop(paste0("status must be one of: all, ", paste(AIR_STATUSES, collapse = ", ")))
  s
}
.validate_air_material <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return("all")
  parts <- trimws(unlist(strsplit(as.character(v), ",", fixed = TRUE)))
  bad <- parts[!grepl("^[A-Za-z0-9 _+./-]+$", parts)]
  if (length(bad) > 0) stop("invalid material filter")
  parts
}
.validate_larvae <- function(v) {
  n <- suppressWarnings(as.integer(v %||% 2L))
  if (is.na(n) || n < 0L || n > 100L) stop("larvae_threshold must be 0-100")
  n
}
.validate_bti <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return(NULL)
  n <- suppressWarnings(as.integer(v))
  if (is.na(n) || n < 1L || n > 60L) stop("bti_effect_days must be 1-60")
  n
}

# Load red-air sites with the full filter surface. Uses get_air_sites_data (which applies
# site-status logic + larvae_threshold + bti override internally); foreman/status/material/
# town are post-load filters. Returns a data.frame with site_status, acres, sitecode, etc.
.load_air <- function(adate, fac, fman, zn, pri, larvae, bti, status_filter, material, tc) {
  air_fac <- if (!is.null(fac) && length(fac) > 0 && !all(fac %in% c("all", ""))) fac else NULL
  data <- get_air_sites_data(
    analysis_date            = adate,
    facility_filter          = air_fac,
    priority_filter          = pri,
    zone_filter              = .air_zone(zn),
    larvae_threshold         = larvae,
    bti_effect_days_override = bti
  )
  if (is.null(data) || nrow(data) == 0) return(data.frame())
  data$acres <- ifelse(is.na(data$acres), 0, as.numeric(data$acres))
  if (!is.null(fman) && !identical(fman, "all") && "foreman" %in% names(data)) {
    data <- data[as.character(data$foreman) == as.character(fman), , drop = FALSE]
  }
  if (!identical(status_filter, "all") && "site_status" %in% names(data)) {
    data <- data[data$site_status == status_filter, , drop = FALSE]
  }
  if (!identical(material, "all") && "last_treatment_material" %in% names(data)) {
    pat <- paste(material, collapse = "|")
    keep <- grepl(pat, data$last_treatment_material, ignore.case = TRUE) & !is.na(data$last_treatment_material)
    data <- data[which(keep), , drop = FALSE]
  }
  filter_sites_by_town(data, tc)
}

#* Get air breeding site status (current treatment/inspection state).
#* Returns sites with active treatment status, acres, priority, facility, zone, material.
#* @param facility Facility code (E, MO, N, Sj, Sr, Wm, Wp). Omit for all.
#* @param foreman FOS shortname to filter by (e.g. "Alex D"). Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param priority Priority filter: RED, YELLOW, BLUE, GREEN, PURPLE (comma-separated). Default RED.
#* @param status Status filter: Active Treatment, Inspected, Needs ID, Needs Treatment, Unknown, or all. Default all.
#* @param larvae_threshold Min dip count that triggers lab sampling. Default 2.
#* @param material Treatment material name(s), comma-separated substring match. Omit for all.
#* @param bti_effect_days Override BTI effect days (1-60). Omit for DB default.
#* @param town Township/city name or 4-digit town code. Omit for all towns.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /air-sites
#* @serializer json
function(req, res,
         facility = NULL, foreman = NULL, zone = "1,2", priority = "RED",
         status = "all", larvae_threshold = NULL, material = NULL,
         bti_effect_days = NULL, town = NULL, analysis_date = NULL, limit = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else NULL
    zn    <- validate_zone(zone)
    pri   <- validate_priority(priority)
    st    <- .validate_air_status(status)
    lar   <- .validate_larvae(larvae_threshold)
    mat   <- .validate_air_material(material)
    bti   <- .validate_bti(bti_effect_days)
    tc    <- validate_town(town)
    adate <- validate_date(analysis_date)

    sites <- .load_air(adate, fac, fman, zn, pri, lar, bti, st, mat, tc)
    if (is.null(sites) || nrow(sites) == 0) {
      return(list(count = 0L, data = list()))
    }
    apply_row_limit(sites, limit)
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Air Sites Summary (value-box stats) ──

#* Get air site summary counts by status — same numbers the dashboard value boxes show.
#* Returns total sites, acres, and count/acres broken down by status
#* (Active Treatment, Inspected, Needs ID, Needs Treatment, Unknown).
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname (e.g. "Alex D"). Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param priority Priority: RED, YELLOW, BLUE, GREEN, PURPLE (comma-separated). Default RED.
#* @param status Status filter: Active Treatment, Inspected, Needs ID, Needs Treatment, Unknown, or all. Default all.
#* @param larvae_threshold Min dip count that triggers lab sampling. Default 2.
#* @param material Treatment material name(s), comma-separated substring match. Omit for all.
#* @param bti_effect_days Override BTI effect days (1-60). Omit for DB default.
#* @param town Township/city name (e.g. Eagan) or 4-digit town code. Omit for all towns.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /air-sites/summary
#* @serializer json
function(req, res,
         facility = NULL, foreman = NULL, zone = "1,2", priority = "RED",
         status = "all", larvae_threshold = NULL, material = NULL,
         bti_effect_days = NULL, town = NULL, analysis_date = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else NULL
    zn    <- validate_zone(zone)
    pri   <- validate_priority(priority)
    st    <- .validate_air_status(status)
    lar   <- .validate_larvae(larvae_threshold)
    mat   <- .validate_air_material(material)
    bti   <- .validate_bti(bti_effect_days)
    tc    <- validate_town(town)
    adate <- validate_date(analysis_date)

    sites <- .load_air(adate, fac, fman, zn, pri, lar, bti, st, mat, tc)
    if (is.null(sites) || nrow(sites) == 0) {
      return(list(
        analysis_date = as.character(adate),
        total_sites = 0L, total_acres = 0,
        by_status = list()
      ))
    }

    # Value-box stats by site_status: both sites (count) and acres are returned.
    by_status <- lapply(AIR_STATUSES, function(s) {
      subset <- sites[sites$site_status == s, ]
      list(status = s, count = nrow(subset), acres = round(sum(subset$acres, na.rm = TRUE), 2))
    })
    names(by_status) <- AIR_STATUSES

    list(
      analysis_date = as.character(adate),
      filters = list(facility = fac, foreman = foreman %||% "all", zone = zn, priority = pri,
                     status = st, larvae_threshold = lar, town = tc %||% "all"),
      total_sites = nrow(sites),
      total_acres = round(sum(sites$acres, na.rm = TRUE), 2),
      by_status = by_status
    )
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Air Sites Summary BY FACILITY ──

#* Get air site summary broken down by each facility — sites and acres by status per facility.
#* Use for facility comparisons, charts, and LLM multi-facility queries.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param priority Priority: RED, YELLOW, BLUE, GREEN, PURPLE (comma-separated). Default RED.
#* @param status Status filter: Active Treatment, Inspected, Needs ID, Needs Treatment, Unknown, or all. Default all.
#* @param larvae_threshold Min dip count that triggers lab sampling. Default 2.
#* @param material Treatment material name(s), comma-separated substring match. Omit for all.
#* @param town Township/city name (e.g. Eagan) or 4-digit town code. Omit for all towns.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /air-sites/summary-by-facility
#* @serializer json
function(req, res,
         zone = "1,2", priority = "RED", status = "all",
         larvae_threshold = NULL, material = NULL, town = NULL, analysis_date = NULL) {
  tryCatch({
    zn    <- validate_zone(zone)
    pri   <- validate_priority(priority)
    st    <- .validate_air_status(status)
    lar   <- .validate_larvae(larvae_threshold)
    mat   <- .validate_air_material(material)
    tc    <- validate_town(town)
    adate <- validate_date(analysis_date)

    sites <- .load_air(adate, NULL, "all", zn, pri, lar, NULL, st, mat, tc)
    if (is.null(sites) || nrow(sites) == 0) {
      return(list(analysis_date = as.character(adate), facility_summaries = list()))
    }

    facs <- unique(sites$facility)
    facs <- facs[!is.na(facs) & nzchar(facs)]

    rows <- lapply(sort(facs), function(f) {
      subset <- sites[sites$facility == f, ]
      by_st <- lapply(AIR_STATUSES, function(s) {
        ss <- subset[subset$site_status == s, ]
        list(status = s, count = nrow(ss), acres = round(sum(ss$acres, na.rm = TRUE), 2))
      })
      names(by_st) <- AIR_STATUSES
      list(
        facility    = f,
        total_sites = nrow(subset),
        total_acres = round(sum(subset$acres, na.rm = TRUE), 2),
        by_status   = by_st
      )
    })

    list(analysis_date = as.character(adate), facility_summaries = rows)
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Ground Prehatch Sites ──

#* Get ground prehatch breeding sites with treatment status.
#* @param facility Facility code. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /ground-prehatch
#* @serializer json
function(req, res,
         facility = NULL,
         zone = "1,2",
         analysis_date = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    zn    <- validate_zone(zone)
    adate <- validate_date(analysis_date)

    data <- ground_env$load_raw_data(
      analysis_date   = adate,
      include_archive = FALSE
    )

    # Apply filters
    data <- ground_env$apply_data_filters(
      data,
      facility_filter = fac,
      zone_filter     = zn
    )

    sites <- data$sites
    if (is.null(sites) || nrow(sites) == 0) {
      return(list(count = 0L, data = list()))
    }

    list(
      count = nrow(sites),
      data  = sites
    )
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Ground Prehatch Summary (value-box stats) ──

#* Get ground prehatch summary counts — total sites, treated, expiring, expired, skipped, percent treated.
#* Same numbers the dashboard value boxes show.
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname (e.g. "Alex D"). Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param town Township/city name (e.g. Eagan) or 4-digit town code. Omit for all towns.
#* @param include_drone If true, include drone-applied prehatch sites. Default false.
#* @param expiring_filter Narrow to sites: all, expiring, or expiring_expired. Default all.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @param expiring_days Days until expiration threshold (1-60). Default 14.
#* @get /ground-prehatch/summary
#* @serializer json
function(req, res,
         facility = NULL,
         foreman = NULL,
         zone = "1,2",
         town = NULL,
         include_drone = "false",
         expiring_filter = "all",
         analysis_date = NULL,
         expiring_days = 14) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else NULL
    zn    <- validate_zone(zone)
    tc    <- validate_town(town)
    incl  <- isTRUE(as.logical(include_drone))
    ef    <- .validate_expiring_filter(expiring_filter)
    adate <- validate_date(analysis_date)
    exdays <- suppressWarnings(as.integer(expiring_days %||% GROUND_EXPIRING_DEFAULT))
    if (is.na(exdays) || exdays < 1L || exdays > 60L) {
      stop("expiring_days must be between 1 and 60")
    }

    details <- .load_ground(adate, exdays, fac, fman, zn, tc, include_drone = incl, expiring_filter = ef)

    if (is.null(details) || nrow(details) == 0) {
      return(list(
        analysis_date = as.character(adate),
        filters = list(facility = fac, foreman = foreman, zone = zn, town = tc %||% "all", expiring_days = exdays),
        total_prehatch = 0L, total_treated = 0L, total_active = 0L,
        total_expiring = 0L, total_expired = 0L, total_skipped = 0L,
        treated_pct = 0,
        total_acres = 0,
        treated_acres = 0,
        active_acres = 0,
        expiring_acres = 0,
        expired_acres = 0,
        skipped_acres = 0
      ))
    }

    total_prehatch <- nrow(details)
    total_treated  <- sum(details$prehatch_status == "treated", na.rm = TRUE)
    total_expiring <- sum(details$prehatch_status == "expiring", na.rm = TRUE)
    total_active   <- total_treated + total_expiring
    total_expired  <- sum(details$prehatch_status == "expired", na.rm = TRUE)
    total_skipped  <- sum(details$prehatch_status == "skipped", na.rm = TRUE)

    treated_acres  <- round(sum(ifelse(details$prehatch_status == "treated", details$acres, 0), na.rm = TRUE), 2)
    expiring_acres <- round(sum(ifelse(details$prehatch_status == "expiring", details$acres, 0), na.rm = TRUE), 2)
    active_acres   <- round(treated_acres + expiring_acres, 2)
    expired_acres  <- round(sum(ifelse(details$prehatch_status == "expired", details$acres, 0), na.rm = TRUE), 2)
    skipped_acres  <- round(sum(ifelse(details$prehatch_status == "skipped", details$acres, 0), na.rm = TRUE), 2)

    treated_pct    <- if ((total_active + total_expired) > 0)
      round(100 * total_active / (total_active + total_expired), 1) else 0

    list(
      analysis_date  = as.character(adate),
      filters        = list(facility = fac, foreman = foreman, zone = zn, town = tc %||% "all", expiring_days = exdays),
      total_prehatch = total_prehatch,
      total_treated  = total_treated,
      total_active   = total_active,
      total_expiring = total_expiring,
      total_expired  = total_expired,
      total_skipped  = total_skipped,
      treated_pct    = treated_pct,
      total_acres    = round(sum(details$acres, na.rm = TRUE), 2),
      treated_acres  = treated_acres,
      active_acres   = active_acres,
      expiring_acres = expiring_acres,
      expired_acres  = expired_acres,
      skipped_acres  = skipped_acres
    )
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Ground Prehatch Summary BY GROUP (mmcd_all / township / sectcode / facility / foreman) ──

#* Get ground prehatch summary rolled up by a chosen dimension.
#* @param group_by One of: mmcd_all, township, sectcode, facility, foreman. Default facility.
#* @param facility Facility code to narrow to. Omit for all.
#* @param foreman FOS shortname to narrow to. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param town Township/city name or 4-digit code to narrow to. Omit for all.
#* @param include_drone If true, include drone-applied prehatch sites. Default false.
#* @param expiring_filter Narrow to sites: all, expiring, or expiring_expired. Default all.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @param expiring_days Expiring window (1-60). Default 14.
#* @get /ground-prehatch/summary-by-group
#* @serializer json
function(req, res,
         group_by = "facility", facility = NULL, foreman = NULL, zone = "1,2",
         town = NULL, include_drone = "false", expiring_filter = "all",
         analysis_date = NULL, expiring_days = 14) {
  tryCatch({
    grp   <- validate_group_by(group_by, GROUND_GROUP_BYS, "facility")
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else NULL
    zn    <- validate_zone(zone)
    tc    <- validate_town(town)
    incl  <- isTRUE(as.logical(include_drone))
    ef    <- .validate_expiring_filter(expiring_filter)
    adate <- validate_date(analysis_date)
    exdays <- suppressWarnings(as.integer(expiring_days %||% GROUND_EXPIRING_DEFAULT))
    if (is.na(exdays) || exdays < 1L || exdays > 60L) stop("expiring_days must be between 1 and 60")

    details <- .load_ground(adate, exdays, fac, fman, zn, tc, include_drone = incl, expiring_filter = ef)
    list(
      analysis_date = as.character(adate),
      group_by      = grp,
      expiring_days = exdays,
      groups        = .ground_group_rows(details, grp)
    )
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Ground Prehatch Expiration Schedule ──

#* Get WHEN ground prehatch treatments expire, as day-window buckets (next 14 / 15-30 /
#* 31-60 / 61-90 / beyond 90) plus soonest and peak windows. Reuses get_site_details_data
#* at several expiring_days thresholds — NO new SQL.
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param town Township/city name or 4-digit code. Omit for all.
#* @param include_drone If true, include drone-applied prehatch sites. Default false.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /ground-prehatch/expiration-schedule
#* @serializer json
function(req, res,
         facility = NULL, foreman = NULL, zone = "1,2", town = NULL,
         include_drone = "false", analysis_date = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else NULL
    zn    <- validate_zone(zone)
    tc    <- validate_town(town)
    incl  <- isTRUE(as.logical(include_drone))
    adate <- validate_date(analysis_date)

    sched <- build_expiration_schedule(function(n) {
      d <- .load_ground(adate, n, fac, fman, zn, tc, include_drone = incl)
      if (is.null(d) || nrow(d) == 0) return(NULL)
      list(
        expiring = sum(d$prehatch_status == "expiring", na.rm = TRUE),
        active   = sum(d$prehatch_status %in% c("treated", "expiring"), na.rm = TRUE),
        expired  = sum(d$prehatch_status == "expired", na.rm = TRUE)
      )
    })

    c(list(
      analysis_date = as.character(adate),
      filters       = list(facility = fac %||% "all", foreman = foreman %||% "all",
                          zone = zn, town = tc %||% "all", include_drone = incl)
    ), sched)
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Section Cards (Breeding Site Cards) ──

#* Get breeding site card data from the loc_breeding_site_cards table.
#* Returns sitecode, priority, acres, type, air/ground, species flags,
#* facility, zone, foreman, and any dynamic columns.
#* @param facility Facility code. Omit for all.
#* @param foreman Foreman/FOS area filter. Omit for all.
#* @get /section-cards
#* @serializer json
function(req, res,
         facility = NULL,
         foreman = NULL) {
  tryCatch({
    data <- cards_env$get_breeding_sites_with_sections()

    # Apply filters in R (the function loads all data)
    if (!is.null(facility) && nzchar(facility)) {
      fac <- validate_facility(facility)
      data <- data[toupper(data$facility) == toupper(fac), ]
    }
    if (!is.null(foreman) && nzchar(foreman)) {
      data <- data[toupper(data$foreman) == toupper(foreman) |
                   toupper(data$fosarea) == toupper(foreman), ]
    }

    if (nrow(data) == 0) {
      return(list(count = 0L, data = list()))
    }

    list(
      count = nrow(data),
      data  = data
    )
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get filter options for section cards (facilities, sections, FOS areas).
#* @param facility Facility code to narrow section/fosarea options. Omit for all.
#* @get /section-cards/filters
#* @serializer json
function(req, res, facility = NULL) {
  tryCatch({
    fac <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    cards_env$get_filter_options(facility_filter = fac)
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get Webster-table breeding sites (original loc_breeding_sites + gis_sectcode).
#* @param facility Facility code. Omit for all.
#* @get /webster-sites
#* @serializer json
function(req, res, facility = NULL) {
  tryCatch({
    data <- cards_env$get_webster_breeding_sites()

    if (!is.null(facility) && nzchar(facility)) {
      fac <- validate_facility(facility)
      data <- data[toupper(data$facility) == toupper(fac), ]
    }

    list(count = nrow(data), data = data)
  }, error = function(e) api_error(res, 400, e$message))
}
