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
# Ground prehatch default expiring window = 14 — matches the ground_prehatch app's own
# get_site_details_data() default (expiring_days = 14). Source of truth is the app, NOT
# the overview. Group-by dimensions come straight from the site-level details
# (facility/foreman/sectcode, plus township = first 4 of sectcode, plus mmcd_all).
GROUND_EXPIRING_DEFAULT <- 14L
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

#* Get air breeding site status (current treatment/inspection state).
#* Returns sites with active treatment status, acres, priority, facility, zone.
#* @param facility Facility code (E, MO, N, Sj, Sr, Wm, Wp). Omit for all.
#* @param foreman FOS shortname to filter by (e.g. "Alex D"). Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param priority Priority filter: RED, YELLOW, BLUE, GREEN, PURPLE (comma-separated). Default RED.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /air-sites
#* @serializer json
function(req, res,
         facility = NULL,
         foreman = NULL,
         zone = "1,2",
         priority = "RED",
         analysis_date = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    fman  <- if (!is.null(foreman) && nzchar(foreman)) { validate_foreman(foreman); clean_text(foreman) } else NULL
    zn    <- validate_zone(zone)
    pri   <- validate_priority(priority)
    adate <- validate_date(analysis_date)

    data <- load_raw_data(
      analysis_date    = adate,
      facility_filter  = fac,
      zone_filter      = zn,
      priority_filter  = pri
    )

    sites <- data$sites
    # Apply foreman filter by matching fosarea shortname
    if (!is.null(fman) && !is.null(sites) && nrow(sites) > 0 && "foreman" %in% names(sites)) {
      sites <- sites[tolower(sites$foreman) == tolower(fman), ]
    }

    if (is.null(sites) || nrow(sites) == 0) {
      return(list(count = 0L, data = list()))
    }

    list(
      count = nrow(sites),
      data  = sites
    )
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
#* @param town Township/city name (e.g. Eagan) or 4-digit town code. Omit for all towns.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /air-sites/summary
#* @serializer json
function(req, res,
         facility = NULL,
         foreman = NULL,
         zone = "1,2",
         priority = "RED",
         town = NULL,
         analysis_date = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    fman  <- if (!is.null(foreman) && nzchar(foreman)) { validate_foreman(foreman); clean_text(foreman) } else NULL
    zn    <- validate_zone(zone)
    pri   <- validate_priority(priority)
    tc    <- validate_town(town)
    adate <- validate_date(analysis_date)

    data <- load_raw_data(
      analysis_date    = adate,
      facility_filter  = fac,
      zone_filter      = zn,
      priority_filter  = pri
    )

    sites <- filter_sites_by_town(data$sites, tc)
    # Apply foreman filter by matching fosarea shortname
    if (!is.null(fman) && !is.null(sites) && nrow(sites) > 0 && "foreman" %in% names(sites)) {
      sites <- sites[tolower(sites$foreman) == tolower(fman), ]
    }

    if (is.null(sites) || nrow(sites) == 0) {
      return(list(
        analysis_date = as.character(adate),
        total_sites = 0L, total_acres = 0,
        by_status = list()
      ))
    }

    # Compute value-box stats grouped by site_status
    statuses <- c("Active Treatment", "Inspected", "Needs ID", "Needs Treatment", "Unknown")
    by_status <- lapply(statuses, function(st) {
      subset <- sites[sites$site_status == st, ]
      list(
        status = st,
        count  = nrow(subset),
        acres  = round(sum(subset$acres, na.rm = TRUE), 2)
      )
    })
    names(by_status) <- statuses

    list(
      analysis_date = as.character(adate),
      filters = list(facility = fac, foreman = foreman, zone = zn, priority = pri, town = tc %||% "all"),
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
#* @param town Township/city name (e.g. Eagan) or 4-digit town code. Omit for all towns.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /air-sites/summary-by-facility
#* @serializer json
function(req, res,
         zone = "1,2",
         priority = "RED",
         town = NULL,
         analysis_date = NULL) {
  tryCatch({
    zn    <- validate_zone(zone)
    pri   <- validate_priority(priority)
    tc    <- validate_town(town)
    adate <- validate_date(analysis_date)

    data <- load_raw_data(
      analysis_date    = adate,
      facility_filter  = NULL,
      zone_filter      = zn,
      priority_filter  = pri
    )

    sites <- filter_sites_by_town(data$sites, tc)
    if (is.null(sites) || nrow(sites) == 0) {
      return(list(analysis_date = as.character(adate), facility_summaries = list()))
    }

    facs <- unique(sites$facility)
    facs <- facs[!is.na(facs) & nzchar(facs)]
    statuses <- c("Active Treatment", "Inspected", "Needs ID", "Needs Treatment", "Unknown")

    rows <- lapply(sort(facs), function(f) {
      subset <- sites[sites$facility == f, ]
      by_st <- lapply(statuses, function(st) {
        s <- subset[subset$site_status == st, ]
        list(status = st, count = nrow(s), acres = round(sum(s$acres, na.rm = TRUE), 2))
      })
      names(by_st) <- statuses
      list(
        facility    = f,
        total_sites = nrow(subset),
        total_acres = round(sum(subset$acres, na.rm = TRUE), 2),
        by_status   = by_st
      )
    })

    list(
      analysis_date      = as.character(adate),
      facility_summaries = rows
    )
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
