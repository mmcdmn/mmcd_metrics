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
source("/srv/shiny-server/apps/ground_prehatch_progress/data_functions.R", local = ground_env)

# Section cards
cards_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/section-cards/data_functions.R", local = cards_env)

# ── Air Sites ──

#* Get air breeding site status (current treatment/inspection state).
#* Returns sites with active treatment status, acres, priority, facility, zone.
#* @param facility Facility code (E, MO, N, Sj, Sr, Wm, Wp). Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param priority Priority filter: RED, YELLOW, BLUE, GREEN, PURPLE (comma-separated). Default RED.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /air-sites
#* @serializer json
function(req, res,
         facility = NULL,
         zone = "1,2",
         priority = "RED",
         analysis_date = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
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
    if (is.null(sites) || nrow(sites) == 0) {
      return(list(count = 0L, data = list()))
    }

    list(
      count = nrow(sites),
      data  = sites
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
