# =============================================================================
# API Routes — Surveillance
# =============================================================================
# Endpoints for trap surveillance, mosquito monitoring, and SUCO history.
# Sources existing data_functions.R from each app — NO new SQL.
#
# All endpoints are mounted under /v1/public/data/surveillance/...
# =============================================================================

source("/srv/shiny-server/shared/db_helpers.R")
source("/srv/shiny-server/shared/app_libraries.R")
source("/srv/api/api_helpers.R")

trap_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/trap_surveillance/data_functions.R", local = trap_env)

monitoring_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/mosquito-monitoring/data_functions.R", local = monitoring_env)

suco_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/suco_history/data_functions.R", local = suco_env)

# ── Trap Surveillance (MLE, MIR, Abundance) ──

#* Get mosquito abundance data for a given year-week.
#* Returns trap counts per area with species breakdown.
#* @param year Year (YYYY). Default current year.
#* @param yrwk Year-week code (YYYYWW). Omit for latest available.
#* @param species Species name. Default Total_Cx_vectors.
#* @get /abundance
#* @serializer json
function(req, res,
         year = NULL,
         yrwk = NULL,
         species = "Total_Cx_vectors") {
  tryCatch({
    yr <- if (!is.null(year) && nzchar(year)) {
      as.integer(year)
    } else {
      as.integer(format(Sys.Date(), "%Y"))
    }
    wk <- if (!is.null(yrwk) && nzchar(yrwk)) clean_text(yrwk, 8L) else NULL
    spp <- clean_text(species, 64L) %||% "Total_Cx_vectors"

    data <- trap_env$fetch_abundance_data(year = yr, yrwk = wk, spp_name = spp)

    if (is.null(data) || nrow(data) == 0) {
      return(list(count = 0L, data = list()))
    }

    list(count = nrow(data), data = data)
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get abundance summarized by VI area for a given year-week.
#* @param yrwk Year-week code (YYYYWW). Required.
#* @param species Species name. Default Total_Cx_vectors.
#* @get /abundance/by-area
#* @serializer json
function(req, res, yrwk, species = "Total_Cx_vectors") {
  tryCatch({
    wk  <- clean_text(yrwk, 8L)
    if (is.null(wk)) stop("yrwk parameter is required")
    spp <- clean_text(species, 64L) %||% "Total_Cx_vectors"

    trap_env$fetch_abundance_by_area(yrwk = wk, spp_name = spp)
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get MLE (Maximum Likelihood Estimate) infection rate by area.
#* @param yrwk Year-week code (YYYYWW). Required.
#* @get /mle/by-area
#* @serializer json
function(req, res, yrwk) {
  tryCatch({
    wk <- clean_text(yrwk, 8L)
    if (is.null(wk)) stop("yrwk parameter is required")
    trap_env$fetch_mle_by_area(yrwk = wk)
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get MLE trend data for the year.
#* @param year Year (YYYY). Default current year.
#* @get /mle/trend
#* @serializer json
function(req, res, year = NULL) {
  tryCatch({
    yr <- if (!is.null(year) && nzchar(year)) as.integer(year) else NULL
    trap_env$fetch_mle_trend(year = yr)
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get multi-year average MLE by epiweek for comparison.
#* @param n_years Number of years to average. Default 5.
#* @get /mle/average
#* @serializer json
function(req, res, n_years = 5) {
  tryCatch({
    n  <- suppressWarnings(as.integer(n_years))
    if (is.na(n) || n < 1 || n > 20) n <- 5L
    yr <- as.integer(format(Sys.Date(), "%Y"))
    trap_env$fetch_mle_avg_by_epiweek(current_year = yr, n_years = n)
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get MIR (Minimum Infection Rate) by area.
#* @param yrwk Year-week code (YYYYWW). Required.
#* @get /mir/by-area
#* @serializer json
function(req, res, yrwk) {
  tryCatch({
    wk <- clean_text(yrwk, 8L)
    if (is.null(wk)) stop("yrwk parameter is required")
    trap_env$fetch_mir_by_area(yrwk = wk)
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Mosquito Monitoring (NightTrap CO2) ──

#* Get mosquito monitoring trap data (CO2 trap counts by species).
#* @param species Species filter. Default Total_Ae_+_Cq.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param analysis_date Date for analysis (YYYY-MM-DD). Default today.
#* @get /monitoring
#* @serializer json
function(req, res,
         species = "Total_Ae_+_Cq",
         zone = "1,2",
         analysis_date = NULL) {
  tryCatch({
    spp   <- clean_text(species, 64L) %||% "Total_Ae_+_Cq"
    zn    <- validate_zone(zone)
    adate <- validate_date(analysis_date)

    data <- monitoring_env$load_raw_data(
      analysis_date  = adate,
      zone_filter    = zn,
      species_filter = spp
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

# ── SUCO History ──

#* Get SUCO (service/utility/complaint operations) inspection data.
#* Returns harborage inspections with species, location, and geometry.
#* @param data_source Data source: all, current, archive. Default all.
#* @param start_date Start date (YYYY-MM-DD). Omit for full range.
#* @param end_date End date (YYYY-MM-DD). Omit for full range.
#* @get /suco
#* @serializer json
function(req, res,
         data_source = "all",
         start_date = NULL,
         end_date = NULL) {
  tryCatch({
    ds <- if (!is.null(data_source) && data_source %in% c("all", "current", "archive")) {
      data_source
    } else "all"

    dr <- NULL
    if (!is.null(start_date) && nzchar(start_date) &&
        !is.null(end_date) && nzchar(end_date)) {
      dr <- c(validate_date(start_date), validate_date(end_date))
    }

    data <- suco_env$get_suco_data(
      data_source = ds,
      date_range  = dr
    )

    if (is.null(data) || nrow(data) == 0) {
      return(list(count = 0L, data = list()))
    }

    list(count = nrow(data), data = data)
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get available SUCO species list.
#* @get /suco/species
#* @serializer json
function(req, res) {
  tryCatch({
    suco_env$get_available_species()
  }, error = function(e) api_error(res, 400, e$message))
}
