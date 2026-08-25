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
source("/srv/shiny-server/apps/trap_surveillance/data_functions.R", local = trap_env, chdir = TRUE)

monitoring_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/mosquito-monitoring/data_functions.R", local = monitoring_env, chdir = TRUE)

suco_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/suco_history/data_functions.R", local = suco_env, chdir = TRUE)

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

# ── SUCO shared helpers ──
SUCO_GROUP_BYS <- c("mmcd_all", "facility", "foreman", "species_name")

# Load + filter SUCO data once (reuses get_suco_data + filter_suco_data; no new SQL).
.load_suco <- function(ds, dr, fac, fman, zn, spp) {
  data <- suco_env$get_suco_data(data_source = ds, date_range = dr)
  if (is.null(data) || nrow(data) == 0) return(data)
  suco_env$filter_suco_data(
    data, facility_filter = fac, foreman_filter = fman,
    zone_filter = zn, date_range = dr, species_filter = spp
  )
}
.suco_date_range <- function(start_date, end_date) {
  if (!is.null(start_date) && nzchar(start_date) && !is.null(end_date) && nzchar(end_date)) {
    c(validate_date(start_date), validate_date(end_date))
  } else NULL
}
.suco_rows <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

# ── SUCO Summary (district totals + weekly goal) ──

#* Get SUCO SUMMARY — total SUCOs, distinct locations, species count, and the weekly
#* goal (12/facility, 72 district). Reuses create_summary_stats. Answers "are we hitting
#* the SUCO goal", "how many SUCOs this week".
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param species Species name. Omit for all species.
#* @param start_date Start date YYYY-MM-DD. Omit for full range.
#* @param end_date End date YYYY-MM-DD. Omit for full range.
#* @param data_source all, current, or archive. Default all.
#* @get /suco/summary
#* @serializer json
function(req, res, facility = NULL, foreman = NULL, zone = "1,2", species = NULL,
         start_date = NULL, end_date = NULL, data_source = "all") {
  tryCatch({
    ds  <- if (!is.null(data_source) && data_source %in% c("all", "current", "archive")) data_source else "all"
    fac <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn  <- validate_zone(zone)
    spp <- if (!is.null(species) && nzchar(species)) clean_text(species, 64L) else "All"
    dr  <- .suco_date_range(start_date, end_date)

    filt <- .load_suco(ds, dr, fac, fman, zn, spp)
    goal <- tryCatch(get_config_threshold("goal", "suco"), error = function(e) NULL)
    base <- list(
      filters = list(facility = fac, foreman = foreman %||% "all", zone = zn,
                     species = spp, data_source = ds),
      goal_per_facility = goal$goal_per_facility %||% 12,
      district_goal     = goal$district_goal %||% 72
    )
    if (is.null(filt) || nrow(filt) == 0) {
      return(c(base, list(total_sucos = 0L, total_locations = 0L, total_species_count = 0L)))
    }
    st <- suco_env$create_summary_stats(filt, group_by = "mmcd_all", data_source = ds)
    c(base, list(
      total_sucos         = as.integer(st$Total_SUCOs[1] %||% 0),
      total_locations     = as.integer(st$Total_Locations[1] %||% 0),
      total_species_count = as.integer(st$Total_Species_Count[1] %||% 0),
      first_suco          = as.character(st$First_SUCO[1] %||% NA),
      last_suco           = as.character(st$Last_SUCO[1] %||% NA)
    ))
  }, error = function(e) api_error(res, 400, e$message))
}

# ── SUCO Summary BY GROUP ──

#* Get SUCO summary rolled up by mmcd_all / facility / foreman / species_name.
#* @param group_by One of: mmcd_all, facility, foreman, species_name. Default facility.
#* @param facility Facility code to narrow to. Omit for all.
#* @param foreman FOS shortname to narrow to. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param species Species name. Omit for all.
#* @param start_date Start date YYYY-MM-DD. Omit for full range.
#* @param end_date End date YYYY-MM-DD. Omit for full range.
#* @param data_source all, current, or archive. Default all.
#* @get /suco/summary-by-group
#* @serializer json
function(req, res, group_by = "facility", facility = NULL, foreman = NULL, zone = "1,2",
         species = NULL, start_date = NULL, end_date = NULL, data_source = "all") {
  tryCatch({
    grp <- validate_group_by(group_by, SUCO_GROUP_BYS, "facility")
    ds  <- if (!is.null(data_source) && data_source %in% c("all", "current", "archive")) data_source else "all"
    fac <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn  <- validate_zone(zone)
    spp <- if (!is.null(species) && nzchar(species)) clean_text(species, 64L) else "All"
    dr  <- .suco_date_range(start_date, end_date)

    filt <- .load_suco(ds, dr, fac, fman, zn, spp)
    if (is.null(filt) || nrow(filt) == 0) {
      return(list(group_by = grp, groups = list()))
    }
    st <- suco_env$create_summary_stats(filt, group_by = grp, data_source = ds)
    list(group_by = grp, groups = .suco_rows(st))
  }, error = function(e) api_error(res, 400, e$message))
}

# ── SUCO Top Locations (ranked) ──

#* Get the TOP SUCO locations, ranked by visits or species count. Reuses get_top_locations.
#* Answers "top SUCO locations", "most-visited harborages", "first X locations".
#* @param mode visits (most-visited) or species (most species). Default visits.
#* @param species Species name (for species mode / filtering). Omit for all.
#* @param facility Facility code. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param start_date Start date YYYY-MM-DD. Omit for full range.
#* @param end_date End date YYYY-MM-DD. Omit for full range.
#* @param data_source all, current, or archive. Default all.
#* @param limit Max locations to return. Default 25.
#* @get /suco/top-locations
#* @serializer json
function(req, res, mode = "visits", species = NULL, facility = NULL, zone = "1,2",
         start_date = NULL, end_date = NULL, data_source = "all", limit = NULL) {
  tryCatch({
    md  <- tolower(trimws(mode %||% "visits"))
    if (!md %in% c("visits", "species")) stop("mode must be visits or species")
    ds  <- if (!is.null(data_source) && data_source %in% c("all", "current", "archive")) data_source else "all"
    fac <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    zn  <- validate_zone(zone)
    spp <- if (!is.null(species) && nzchar(species)) clean_text(species, 64L) else "All"
    dr  <- .suco_date_range(start_date, end_date)

    filt <- .load_suco(ds, dr, fac, "all", zn, spp)
    if (is.null(filt) || nrow(filt) == 0) return(list(mode = md, count = 0L, data = list()))
    top <- suco_env$get_top_locations(filt, mode = md, species_filter = spp)
    if (is.null(top) || nrow(top) == 0) return(list(mode = md, count = 0L, data = list()))
    out <- apply_row_limit(top, limit, default_limit = 25L)
    c(list(mode = md, filters = list(facility = fac, zone = zn, species = spp)), out)
  }, error = function(e) api_error(res, 400, e$message))
}
