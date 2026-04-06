# =============================================================================
# API Routes — Inspections & Drone
# =============================================================================
# Endpoints for general inspection data, drone site data, and air checklist.
# Sources existing data_functions.R from each app — NO new SQL.
#
# All endpoints are mounted under /v1/public/data/inspections/...
# =============================================================================

source("/srv/shiny-server/shared/db_helpers.R")
source("/srv/shiny-server/shared/app_libraries.R")
source("/srv/api/api_helpers.R")

insp_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/inspections/data_functions.R", local = insp_env)

drone_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/drone/data_functions.R", local = drone_env)

checklist_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/air_inspection_checklist/data_functions.R", local = checklist_env)

# ── General Inspections ──

#* Get larval inspection data (inspections across all site types).
#* @param facility Facility code. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param priority Priority filter: RED, YELLOW, BLUE, GREEN, PURPLE (comma-separated). Omit for all.
#* @param drone_filter Drone filter: Y, M, C, all. Default all.
#* @param start_year Start year for historical. Omit for current only.
#* @param end_year End year for historical. Omit for current only.
#* @get /larval
#* @serializer json
function(req, res,
         facility = NULL,
         zone = "1,2",
         priority = NULL,
         drone_filter = "all",
         start_year = NULL,
         end_year = NULL) {
  tryCatch({
    fac  <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    zn   <- validate_zone(zone)
    pri  <- if (!is.null(priority) && nzchar(priority)) validate_priority(priority) else NULL
    df   <- if (!is.null(drone_filter) && drone_filter %in% c("Y", "M", "C", "all")) drone_filter else "all"
    sy   <- if (!is.null(start_year) && nzchar(start_year)) as.integer(start_year) else NULL
    ey   <- if (!is.null(end_year) && nzchar(end_year)) as.integer(end_year) else NULL

    data <- insp_env$load_raw_data(
      facility_filter = fac,
      zone_filter     = zn,
      priority_filter = pri,
      drone_filter    = df,
      start_year      = sy,
      end_year        = ey
    )

    sites <- data$sites
    if (is.null(sites) || nrow(sites) == 0) {
      return(list(count = 0L, data = list()))
    }

    list(count = nrow(sites), data = sites)
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Drone Sites ──

#* Get drone-designated breeding site data.
#* @param facility Facility code. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /drone
#* @serializer json
function(req, res,
         facility = NULL,
         zone = "1,2",
         analysis_date = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    zn    <- validate_zone(zone)
    adate <- validate_date(analysis_date)

    data <- drone_env$load_raw_data(analysis_date = adate)

    # Apply filters
    data <- drone_env$apply_data_filters(
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
      total = data$total_count %||% nrow(sites),
      data  = sites
    )
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Air Inspection Checklist (full operational checklist) ──

#* Get the full air inspection checklist with bug lab, claims, and treatment data.
#* This is the comprehensive operational checklist used for daily field planning.
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1.
#* @param lookback_days Days back to check for inspections (1-14). Default 2.
#* @param priority Priority filter: RED, YELLOW, BLUE, etc. Default RED.
#* @get /checklist
#* @serializer json
function(req, res,
         facility = NULL,
         foreman = NULL,
         zone = "1",
         lookback_days = 2,
         priority = "RED") {
  tryCatch({
    fac <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    fm  <- if (!is.null(foreman) && nzchar(foreman)) clean_text(foreman, 32L) else NULL
    zn  <- validate_zone(zone)
    lb  <- validate_lookback(lookback_days)
    pri <- validate_priority(priority)

    data <- checklist_env$get_checklist_data(
      facility_filter = fac,
      foreman_filter  = fm,
      zone_filter     = if (length(zn) == 1) zn else zn[1],
      lookback_days   = lb,
      priority_filter = if (length(pri) == 1) pri else pri[1]
    )

    if (is.null(data) || nrow(data) == 0) {
      return(list(count = 0L, summary = list(), data = list()))
    }

    summary <- checklist_env$summarize_checklist(data)

    list(
      count   = nrow(data),
      summary = summary,
      data    = data
    )
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get field employees list (inspectors, foremen, supervisors).
#* @get /employees
#* @serializer json
function(req, res) {
  tryCatch({
    checklist_env$get_field_employees()
  }, error = function(e) api_error(res, 400, e$message))
}
