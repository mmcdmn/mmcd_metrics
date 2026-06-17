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
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /air-sites/summary
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
      filters = list(facility = fac, foreman = foreman, zone = zn, priority = pri),
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
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /air-sites/summary-by-facility
#* @serializer json
function(req, res,
         zone = "1,2",
         priority = "RED",
         analysis_date = NULL) {
  tryCatch({
    zn    <- validate_zone(zone)
    pri   <- validate_priority(priority)
    adate <- validate_date(analysis_date)

    data <- load_raw_data(
      analysis_date    = adate,
      facility_filter  = NULL,
      zone_filter      = zn,
      priority_filter  = pri
    )

    sites <- data$sites
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
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @param expiring_days Days until expiration threshold (1-60). Default 14.
#* @get /ground-prehatch/summary
#* @serializer json
function(req, res,
         facility = NULL,
         foreman = NULL,
         zone = "1,2",
         analysis_date = NULL,
         expiring_days = 14) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else NULL
    zn    <- validate_zone(zone)
    adate <- validate_date(analysis_date)
    exdays <- suppressWarnings(as.integer(expiring_days %||% 14L))
    if (is.na(exdays) || exdays < 1L || exdays > 60L) {
      stop("expiring_days must be between 1 and 60")
    }

    details <- ground_env$get_site_details_data(
      expiring_days = exdays,
      analysis_date = adate
    )

    details <- ground_env$filter_ground_data(
      details,
      zone_filter = zn,
      facility_filter = fac,
      foreman_filter = fman,
      include_drone = FALSE
    )

    if (is.null(details) || nrow(details) == 0) {
      return(list(
        analysis_date = as.character(adate),
        filters = list(facility = fac, foreman = foreman, zone = zn, expiring_days = exdays),
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
      filters        = list(facility = fac, foreman = foreman, zone = zn, expiring_days = exdays),
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
