# =============================================================================
# API Routes — Structure Treatments
# =============================================================================
# Structure treatments (loc_cxstruct): window wells, dry wells, underground
# chambers, and other man-made harborages. This is DISTINCT from catch basins
# (loc_catchbasin) — those are storm-drain sumps and live in catch_basins.R
# under /v1/public/data/catch-basins/...
#
# Sources existing data_functions.R from the struct_trt app — NO new SQL.
# All endpoints are mounted under /v1/public/data/structures/...
#
# This is the TEMPLATE route: it exposes the app's FULL filter + group-by
# surface (facility, foreman, zone, structure_type, priority, status) and a
# generalized expiration-schedule, using only existing app loaders. Defaults for
# omitted values come from the overview metric_registry (registry_default()).
#
# NOTE on the two letter-code filters (they are DIFFERENT columns):
#   • structure_type  -> loc.s_type      (e.g. CV, PR, CV/PR — physical type)
#   • status          -> loc.status_udw  (D=dry, W=wet, U=unknown)
# =============================================================================

source("/srv/shiny-server/shared/db_helpers.R")
source("/srv/shiny-server/shared/app_libraries.R")
source("/srv/api/api_helpers.R")

struct_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/struct_trt/data_functions.R", local = struct_env, chdir = TRUE)

# Structure treatments default expiring window (registry is the source of defaults).
STRUCT_EXPIRING_DEFAULT <- as.integer(registry_default("structure", "expiring_days", 7L))
STRUCT_GROUP_BYS <- c("facility", "foreman", "mmcd_all")

# ── Local filter validators (loc_cxstruct-specific codes) ──
# structure_type is a free-form s_type code (CV, PR, CV/PR). Single code or "all".
.validate_structure_type <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return("all")
  s <- toupper(trimws(as.character(v)))
  if (nchar(s) > 12L || !grepl("^[A-Z/]+$", s)) stop("invalid structure_type code")
  s
}
# priority for structures is loc.priority (not the air color enum). Alphanumeric codes.
.validate_struct_priority <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return("all")
  parts <- trimws(toupper(unlist(strsplit(as.character(v), ",", fixed = TRUE))))
  bad <- parts[!grepl("^[A-Z0-9]+$", parts)]
  if (length(bad) > 0) stop("invalid priority code")
  parts
}

# ── Shared loader: applies the full filter surface, reusing app functions only ──
# SQL-level filters (facility, structure_type, priority, status, zone) go to
# load_raw_data; foreman is applied post-load via the app's apply_data_filters
# (load_raw_data's universe query doesn't filter on fosarea).
.load_structs <- function(adate, fac, fman, zn, stype, prio, status, expiring_days) {
  data <- struct_env$load_raw_data(
    analysis_date         = adate,
    facility_filter       = fac,
    structure_type_filter = stype,
    priority_filter       = prio,
    status_types          = status,
    zone_filter           = zn,
    expiring_days         = expiring_days
  )
  if (!is.null(fman) && !identical(fman, "all")) {
    data <- struct_env$apply_data_filters(
      data, foreman_filter = fman, zone_filter = zn
    )
  }
  data
}

# ── Turn an aggregate_structure_data() frame into a clean rows list ──
.fmt_struct_groups <- function(grouped) {
  if (is.null(grouped) || nrow(grouped) == 0) return(list())
  lapply(seq_len(nrow(grouped)), function(i) {
    r <- grouped[i, ]
    total  <- as.integer(r$total_count %||% 0)
    active <- as.integer(r$active_count %||% 0)
    list(
      group          = as.character(r$group_name %||% r$display_name %||% ""),
      display_name   = as.character(r$display_name %||% r$group_name %||% ""),
      total_count    = total,
      active_count   = active,
      expiring_count = as.integer(r$expiring_count %||% 0),
      pct_treated    = if (total > 0) round(100 * active / total, 1) else 0
    )
  })
}

# ── Structure Treatments (site-level list) ──

#* Get structure treatment sites with current status.
#* Returns sitecode, structure type, status, priority, facility, zone, foreman, is_active, is_expiring.
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname (e.g. "Alex D"). Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param structure_type s_type code (e.g. CV, PR, CV/PR). Omit for all.
#* @param priority loc.priority code(s), comma-separated. Omit for all.
#* @param status status_udw: D,W,U (comma-separated). Default D,W,U (full universe).
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @param limit Max rows to return. Default 500.
#* @get /struct-treatments
#* @serializer json
function(req, res,
         facility = NULL, foreman = NULL, zone = "1,2",
         structure_type = NULL, priority = NULL, status = NULL,
         analysis_date = NULL, limit = NULL) {
  tryCatch({
    fac    <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman   <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn     <- validate_zone(zone)
    stype  <- .validate_structure_type(structure_type)
    prio   <- .validate_struct_priority(priority)
    stat   <- validate_type_codes(status, c("D", "W", "U"), "status")
    if (identical(stat, "all")) stat <- c("D", "W", "U")
    adate  <- validate_date(analysis_date)

    data  <- .load_structs(adate, fac, fman, zn, stype, prio, stat, STRUCT_EXPIRING_DEFAULT)
    sites <- data$sites
    if (is.null(sites) || nrow(sites) == 0) {
      return(list(count = 0L, total = data$total_count %||% 0L, data = list()))
    }
    out <- apply_row_limit(sites, limit)
    c(list(total = data$total_count %||% nrow(sites)), out)
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Structure Treatment Summary (value-box stats) ──

#* Get structure treatment summary — total structures, treated (active), expiring, % treated.
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param structure_type s_type code (CV, PR, ...). Omit for all.
#* @param priority loc.priority code(s). Omit for all.
#* @param status status_udw D,W,U. Default D,W,U.
#* @param expiring_days Days-ahead window that counts as "expiring". Default from registry.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /struct-treatments/summary
#* @serializer json
function(req, res,
         facility = NULL, foreman = NULL, zone = "1,2",
         structure_type = NULL, priority = NULL, status = NULL,
         expiring_days = NULL, analysis_date = NULL) {
  tryCatch({
    fac    <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman   <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn     <- validate_zone(zone)
    stype  <- .validate_structure_type(structure_type)
    prio   <- .validate_struct_priority(priority)
    stat   <- validate_type_codes(status, c("D", "W", "U"), "status")
    if (identical(stat, "all")) stat <- c("D", "W", "U")
    adate  <- validate_date(analysis_date)
    exp_n  <- as.integer(expiring_days %||% STRUCT_EXPIRING_DEFAULT)

    data  <- .load_structs(adate, fac, fman, zn, stype, prio, stat, exp_n)
    sites <- data$sites
    if (is.null(sites) || nrow(sites) == 0) {
      return(list(analysis_date = as.character(adate),
                  total_count = 0L, active_count = 0L, expiring_count = 0L, active_pct = 0))
    }
    total    <- nrow(sites)
    active   <- sum(sites$is_active == TRUE, na.rm = TRUE)
    expiring <- sum(sites$is_expiring == TRUE, na.rm = TRUE)

    list(
      analysis_date  = as.character(adate),
      expiring_days  = exp_n,
      filters        = list(facility = fac, foreman = foreman %||% "all", zone = zn,
                            structure_type = stype, priority = prio, status = stat),
      total_count    = total,
      active_count   = as.integer(active),
      expiring_count = as.integer(expiring),
      active_pct     = if (total > 0) round(100 * active / total, 1) else 0
    )
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Structure Treatment Summary BY GROUP (facility / foreman / mmcd_all) ──

#* Get structure treatment summary rolled up by a chosen dimension.
#* Reuses the app's aggregate_structure_data(). Use for comparisons and charts.
#* @param group_by One of: facility, foreman, mmcd_all. Default facility.
#* @param facility Facility code to narrow to. Omit for all.
#* @param foreman FOS shortname to narrow to. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param structure_type s_type code. Omit for all.
#* @param priority loc.priority code(s). Omit for all.
#* @param status status_udw D,W,U. Default D,W,U.
#* @param separate_zones If true, split each group into P1/P2 rows. Default false.
#* @param expiring_days Expiring window. Default from registry.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /struct-treatments/summary-by-group
#* @serializer json
function(req, res,
         group_by = "facility", facility = NULL, foreman = NULL, zone = "1,2",
         structure_type = NULL, priority = NULL, status = NULL,
         separate_zones = "false", expiring_days = NULL, analysis_date = NULL) {
  tryCatch({
    grp    <- validate_group_by(group_by, STRUCT_GROUP_BYS, "facility")
    fac    <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman   <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn     <- validate_zone(zone)
    stype  <- .validate_structure_type(structure_type)
    prio   <- .validate_struct_priority(priority)
    stat   <- validate_type_codes(status, c("D", "W", "U"), "status")
    if (identical(stat, "all")) stat <- c("D", "W", "U")
    adate  <- validate_date(analysis_date)
    exp_n  <- as.integer(expiring_days %||% STRUCT_EXPIRING_DEFAULT)
    combine <- !isTRUE(as.logical(separate_zones))

    data <- .load_structs(adate, fac, fman, zn, stype, prio, stat, exp_n)
    if (is.null(data$sites) || nrow(data$sites) == 0) {
      return(list(analysis_date = as.character(adate), group_by = grp, groups = list()))
    }
    grouped <- struct_env$aggregate_structure_data(
      data$sites, data$treatments,
      group_by = grp, zone_filter = zn, combine_zones = combine
    )
    list(
      analysis_date = as.character(adate),
      group_by      = grp,
      expiring_days = exp_n,
      groups        = .fmt_struct_groups(grouped)
    )
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get structure treatment summary broken down by facility (alias of summary-by-group?group_by=facility).
#* Kept for backward compatibility.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param structure_type s_type code. Omit for all.
#* @param priority loc.priority code(s). Omit for all.
#* @param status status_udw D,W,U. Default D,W,U.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /struct-treatments/summary-by-facility
#* @serializer json
function(req, res, zone = "1,2",
         structure_type = NULL, priority = NULL, status = NULL, analysis_date = NULL) {
  tryCatch({
    zn     <- validate_zone(zone)
    stype  <- .validate_structure_type(structure_type)
    prio   <- .validate_struct_priority(priority)
    stat   <- validate_type_codes(status, c("D", "W", "U"), "status")
    if (identical(stat, "all")) stat <- c("D", "W", "U")
    adate  <- validate_date(analysis_date)

    data <- .load_structs(adate, "all", "all", zn, stype, prio, stat, STRUCT_EXPIRING_DEFAULT)
    if (is.null(data$sites) || nrow(data$sites) == 0) {
      return(list(analysis_date = as.character(adate), facility_summaries = list()))
    }
    grouped <- struct_env$aggregate_structure_data(
      data$sites, data$treatments,
      group_by = "facility", zone_filter = zn, combine_zones = TRUE
    )
    rows <- .fmt_struct_groups(grouped)
    # keep legacy field name `facility`
    rows <- lapply(rows, function(r) { r$facility <- r$group; r })
    list(analysis_date = as.character(adate), facility_summaries = rows)
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Structure Expiration Schedule (WHEN treatments lapse) ──

#* Get WHEN structure treatments expire, as day-window buckets.
#* Reuses load_raw_data() at several expiring_days thresholds and differences the
#* expiring counts to build a timeline (next 14 days, 15-30, 31-60, 61-90, beyond 90)
#* plus the soonest window, the peak window, and total active — NO new SQL.
#* Answers "when do the structures expire" / "what needs re-treatment soon".
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param structure_type s_type code. Omit for all.
#* @param priority loc.priority code(s). Omit for all.
#* @param status status_udw D,W,U. Default D,W,U.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /struct-treatments/expiration-schedule
#* @serializer json
function(req, res,
         facility = NULL, foreman = NULL, zone = "1,2",
         structure_type = NULL, priority = NULL, status = NULL, analysis_date = NULL) {
  tryCatch({
    fac    <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman   <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn     <- validate_zone(zone)
    stype  <- .validate_structure_type(structure_type)
    prio   <- .validate_struct_priority(priority)
    stat   <- validate_type_codes(status, c("D", "W", "U"), "status")
    if (identical(stat, "all")) stat <- c("D", "W", "U")
    adate  <- validate_date(analysis_date)

    sched <- build_expiration_schedule(function(n) {
      d <- .load_structs(adate, fac, fman, zn, stype, prio, stat, n)
      s <- d$sites
      if (is.null(s) || nrow(s) == 0) return(NULL)
      list(
        expiring = sum(s$is_expiring == TRUE, na.rm = TRUE),
        active   = sum(s$is_active   == TRUE, na.rm = TRUE),
        expired  = 0L   # structures track active treatments only, no distinct "lapsed" universe
      )
    })

    c(list(
      analysis_date = as.character(adate),
      filters       = list(facility = fac, foreman = foreman %||% "all", zone = zn,
                          structure_type = stype, priority = prio, status = stat)
    ), sched)
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Filter options ──

#* Get structure filter options (facilities, sections, FOS areas, structure types).
#* @param facility Facility code to narrow options. Omit for all.
#* @get /struct-treatments/filters
#* @serializer json
function(req, res, facility = NULL) {
  tryCatch({
    fac <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    cards_env <- new.env(parent = globalenv())
    source("/srv/shiny-server/apps/section-cards/data_functions.R", local = cards_env, chdir = TRUE)
    cards_env$get_structure_filter_options(facility_filter = fac)
  }, error = function(e) api_error(res, 400, e$message))
}
