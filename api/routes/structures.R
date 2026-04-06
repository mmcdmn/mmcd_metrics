# =============================================================================
# API Routes — Structures & Catch Basins
# =============================================================================
# Endpoints for structure treatments and catch basin data.
# Sources existing data_functions.R from each app — NO new SQL.
#
# All endpoints are mounted under /v1/public/data/structures/...
# =============================================================================

source("/srv/shiny-server/shared/db_helpers.R")
source("/srv/shiny-server/shared/app_libraries.R")
source("/srv/api/api_helpers.R")

struct_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/struct_trt/data_functions.R", local = struct_env, chdir = TRUE)

cb_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/catch_basin_status/data_functions.R", local = cb_env, chdir = TRUE)

# ── Structure Treatments ──

#* Get structure treatment sites with current status.
#* Returns sitecode, structure type, status, chambers, priority, facility, zone.
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname (e.g. "Alex D"). Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param structure_type Structure type filter: D, W, U (comma-separated). Omit for all.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /struct-treatments
#* @serializer json
function(req, res,
         facility = NULL,
         foreman = NULL,
         zone = "1,2",
         structure_type = NULL,
         analysis_date = NULL) {
  tryCatch({
    fac    <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman   <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn     <- validate_zone(zone)
    stype  <- if (!is.null(structure_type) && nzchar(structure_type)) {
      trimws(unlist(strsplit(toupper(structure_type), ",", fixed = TRUE)))
    } else "all"
    adate  <- validate_date(analysis_date)

    data <- struct_env$load_raw_data(
      analysis_date         = adate,
      facility_filter       = fac,
      foreman_filter        = fman,
      zone_filter           = zn,
      structure_type_filter = stype
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

# ── Structure Treatment Summary (value-box stats) ──

#* Get structure treatment summary — total structures, active, expiring, percent treated.
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname (e.g. "Alex D"). Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param structure_type Type: D, W, U (comma-separated). Omit for all.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /struct-treatments/summary
#* @serializer json
function(req, res,
         facility = NULL,
         foreman = NULL,
         zone = "1,2",
         structure_type = NULL,
         analysis_date = NULL) {
  tryCatch({
    fac    <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman   <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn     <- validate_zone(zone)
    stype  <- if (!is.null(structure_type) && nzchar(structure_type)) {
      trimws(unlist(strsplit(toupper(structure_type), ",", fixed = TRUE)))
    } else "all"
    adate  <- validate_date(analysis_date)

    data <- struct_env$load_raw_data(
      analysis_date         = adate,
      facility_filter       = fac,
      foreman_filter        = fman,
      zone_filter           = zn,
      structure_type_filter = stype
    )

    sites <- data$sites
    if (is.null(sites) || nrow(sites) == 0) {
      return(list(
        analysis_date = as.character(adate),
        total_count = 0L, active_count = 0L, expiring_count = 0L, active_pct = 0
      ))
    }

    total    <- nrow(sites)
    active   <- sum(sites$is_active == TRUE, na.rm = TRUE)
    expiring <- sum(sites$is_expiring == TRUE, na.rm = TRUE)
    active_pct <- if (total > 0) round(100 * active / total, 1) else 0

    list(
      analysis_date  = as.character(adate),
      filters        = list(facility = fac, foreman = foreman, zone = zn, structure_type = stype),
      total_count    = total,
      active_count   = active,
      expiring_count = expiring,
      active_pct     = active_pct
    )
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get structure filter options (facilities, sections, FOS areas, structure types).
#* @param facility Facility code to narrow options. Omit for all.
#* @get /struct-treatments/filters
#* @serializer json
function(req, res, facility = NULL) {
  tryCatch({
    fac <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL

    # section-cards has get_structure_filter_options
    cards_env <- new.env(parent = globalenv())
    source("/srv/shiny-server/apps/section-cards/data_functions.R", local = cards_env, chdir = TRUE)
    cards_env$get_structure_filter_options(facility_filter = fac)
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Catch Basins ──

#* Get catch basin treatment status.
#* Returns aggregated treatment counts per section/facility with active/expiring/expired.
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname (e.g. "Alex D"). Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /catch-basins
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
#* @get /catch-basins/summary
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
