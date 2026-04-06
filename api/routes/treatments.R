# =============================================================================
# API Routes — Treatments
# =============================================================================
# Endpoints for cattail inspections, cattail treatments, and control efficacy.
# Sources existing data_functions.R from each app — NO new SQL.
#
# All endpoints are mounted under /v1/public/data/treatments/...
# =============================================================================

source("/srv/shiny-server/shared/db_helpers.R")
source("/srv/shiny-server/shared/app_libraries.R")
source("/srv/api/api_helpers.R")

cattail_insp_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/cattail_inspections/data_functions.R", local = cattail_insp_env)

cattail_trt_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/cattail_treatments/data_functions.R", local = cattail_trt_env)

efficacy_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/control_efficacy/data_functions.R", local = efficacy_env)

# ── Cattail Inspections ──

#* Get cattail inspection data with progress toward goals.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /cattail-inspections
#* @serializer json
function(req, res,
         zone = "1,2",
         analysis_date = NULL) {
  tryCatch({
    zn    <- validate_zone(zone)
    adate <- validate_date(analysis_date)

    data <- cattail_insp_env$load_raw_data(
      analysis_date = adate,
      zone_filter   = zn
    )

    sites <- data$sites
    list(
      count      = nrow(sites),
      total      = data$total_count %||% nrow(sites),
      goal_count = data$goal_count %||% 0L,
      data       = sites
    )
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get cattail inspection goals by facility.
#* @get /cattail-goals
#* @serializer json
function(req, res) {
  tryCatch({
    cattail_insp_env$get_cattail_goals()
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Cattail Treatments ──

#* Get cattail treatment records for the current or specified year.
#* Returns sitecode, treatment date, action, material, acres, facility, zone.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /cattail-treatments
#* @serializer json
function(req, res, analysis_date = NULL) {
  tryCatch({
    adate <- validate_date(analysis_date)
    year  <- as.integer(format(adate, "%Y"))

    data <- cattail_trt_env$load_cattail_treatments(
      analysis_date = adate,
      current_year  = year
    )

    if (is.null(data) || nrow(data) == 0) {
      return(list(count = 0L, data = list()))
    }

    list(count = nrow(data), data = data)
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Control Efficacy ──

#* Get treatment records with checkback data for evaluating efficacy.
#* Returns treatment and post-treatment inspection pairs.
#* @param start_date Start date (YYYY-MM-DD). Default 30 days ago.
#* @param end_date End date (YYYY-MM-DD). Default today.
#* @param facility Facility code. Omit for all.
#* @param matcode Material code filter. Omit for all.
#* @get /control-efficacy
#* @serializer json
function(req, res,
         start_date = NULL,
         end_date = NULL,
         facility = NULL,
         matcode = NULL) {
  tryCatch({
    ed <- validate_date(end_date)
    sd <- if (!is.null(start_date) && nzchar(start_date)) {
      validate_date(start_date)
    } else {
      ed - 30L
    }
    fac <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    mc  <- if (!is.null(matcode) && nzchar(matcode)) clean_text(matcode, 16L) else "all"

    data <- efficacy_env$load_treatment_data(
      start_date      = sd,
      end_date        = ed,
      facility_filter = fac,
      matcode_filter  = mc
    )

    if (is.null(data) || nrow(data) == 0) {
      return(list(count = 0L, data = list()))
    }

    # Also load checkback data for treated sites
    treated_sites <- unique(data$sitecode)
    checkbacks <- tryCatch(
      efficacy_env$load_checkback_data(treated_sites, sd, ed),
      error = function(e) data.frame()
    )

    list(
      treatment_count  = nrow(data),
      treatments       = data,
      checkback_count  = nrow(checkbacks),
      checkbacks       = checkbacks
    )
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get available material codes with dosage info.
#* @get /material-codes
#* @serializer json
function(req, res) {
  tryCatch({
    efficacy_env$load_dosage_options(matcode = NULL)
  }, error = function(e) api_error(res, 400, e$message))
}
