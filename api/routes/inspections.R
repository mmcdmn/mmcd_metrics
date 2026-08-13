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
source("/srv/shiny-server/apps/inspections/data_functions.R", local = insp_env, chdir = TRUE)

drone_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/drone/data_functions.R", local = drone_env, chdir = TRUE)

checklist_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/air_inspection_checklist/data_functions.R", local = checklist_env, chdir = TRUE)

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

# ── Inspections shared filter validators (reused by the red-bug endpoints) ──
.insp_air_gnd <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return("both")
  s <- tolower(trimws(as.character(v)))
  s <- switch(s, a = "A", air = "A", g = "G", ground = "G", both = "both", all = "both", s)
  if (!s %in% c("A", "G", "both")) stop("air_gnd must be A, G, or both")
  s
}
.insp_drone_filter <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return("all")
  s <- tolower(trimws(as.character(v)))
  if (s %in% c("include_drone", "all", "include")) return("all")
  if (!s %in% c("drone_only", "no_drone")) {
    stop("drone_filter must be drone_only, no_drone, or include_drone")
  }
  s
}
# fosarea filter = FOS shortname(s); get_red_bug_* convert shortnames -> emp_num internally.
.insp_fosarea <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return(NULL)
  parts <- trimws(unlist(strsplit(as.character(v), ",", fixed = TRUE)))
  bad <- parts[!grepl("^[A-Za-z0-9 ._-]+$", parts)]
  if (length(bad) > 0) stop("invalid fosarea")
  parts
}

# ── Red Bug Gaps (sites without a red-bug find in N years) ──

#* Get sites WITHOUT a recent red-bug find (a prehatch coverage gap), ranked oldest-first.
#* Reuses get_red_bug_gaps() — self-contained, fast. Supports sort_by (oldest|newest) + limit
#* so callers can ask for the "first X" longest-standing gaps.
#* @param years_gap Years since last red bug to count as a gap (1-20). Default 5.
#* @param facility Facility code. Omit for all.
#* @param fosarea FOS shortname(s), comma-separated. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param priority Priority: RED, YELLOW, BLUE, GREEN, PURPLE (comma-separated). Omit for all.
#* @param air_gnd Site type: A, G, or both. Default both.
#* @param drone_filter drone_only, no_drone, or include_drone. Default include_drone.
#* @param prehatch_only If true, prehatch sites only. Default false.
#* @param sort_by oldest (longest-standing gap first) or newest. Default oldest.
#* @param limit Max rows to return. Default 500.
#* @get /red-bug-gaps
#* @serializer json
function(req, res, years_gap = 5, facility = NULL, fosarea = NULL, zone = "1,2",
         priority = NULL, air_gnd = "both", drone_filter = "include_drone",
         prehatch_only = "false", sort_by = "oldest", limit = NULL) {
  tryCatch({
    yg  <- suppressWarnings(as.integer(years_gap %||% 5L))
    if (is.na(yg) || yg < 1L || yg > 20L) stop("years_gap must be 1-20")
    fac <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    fos <- .insp_fosarea(fosarea)
    zn  <- validate_zone(zone)
    pri <- if (!is.null(priority) && nzchar(priority)) validate_priority(priority) else NULL
    ag  <- .insp_air_gnd(air_gnd)
    dfl <- .insp_drone_filter(drone_filter)
    ph  <- isTRUE(as.logical(prehatch_only))

    gaps <- insp_env$get_red_bug_gaps(
      years_gap = yg, facility_filter = fac, fosarea_filter = fos, zone_filter = zn,
      priority_filter = pri, air_gnd_filter = ag, drone_filter = dfl, prehatch_only = ph
    )
    if (is.null(gaps) || nrow(gaps) == 0) {
      return(list(years_gap = yg, count = 0L, data = list()))
    }
    # get_red_bug_gaps returns oldest-gap-first; reverse for sort_by=newest.
    if (identical(tolower(sort_by %||% "oldest"), "newest")) {
      gaps <- gaps[rev(seq_len(nrow(gaps))), , drop = FALSE]
    }
    out <- apply_row_limit(gaps, limit)
    c(list(years_gap = yg,
           filters = list(facility = fac %||% "all", zone = zn, air_gnd = ag,
                          drone_filter = dfl, prehatch_only = ph),
           sort_by = tolower(sort_by %||% "oldest")), out)
  }, error = function(e) api_error(res, 400, e$message))
}

#* Get red-bug gap coverage rolled up by facility or FOS — total sites, gap sites, gap %.
#* Reuses get_red_bug_gaps + get_red_bug_all_sites + the facility/FOS analysis.
#* @param group_by facility or fos. Default facility.
#* @param years_gap Years since last red bug (1-20). Default 5.
#* @param facility Facility code. Omit for all.
#* @param fosarea FOS shortname(s), comma-separated. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param priority Priority (comma-separated). Omit for all.
#* @param air_gnd Site type: A, G, or both. Default both.
#* @param drone_filter drone_only, no_drone, or include_drone. Default include_drone.
#* @param prehatch_only If true, prehatch sites only. Default false.
#* @get /red-bug-gaps/by-group
#* @serializer json
function(req, res, group_by = "facility", years_gap = 5, facility = NULL, fosarea = NULL,
         zone = "1,2", priority = NULL, air_gnd = "both",
         drone_filter = "include_drone", prehatch_only = "false") {
  tryCatch({
    grp <- validate_group_by(group_by, c("facility", "fos"), "facility")
    yg  <- suppressWarnings(as.integer(years_gap %||% 5L))
    if (is.na(yg) || yg < 1L || yg > 20L) stop("years_gap must be 1-20")
    fac <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    fos <- .insp_fosarea(fosarea)
    zn  <- validate_zone(zone)
    pri <- if (!is.null(priority) && nzchar(priority)) validate_priority(priority) else NULL
    ag  <- .insp_air_gnd(air_gnd)
    dfl <- .insp_drone_filter(drone_filter)
    ph  <- isTRUE(as.logical(prehatch_only))

    common <- list(facility_filter = fac, fosarea_filter = fos, zone_filter = zn,
                   priority_filter = pri, air_gnd_filter = ag, drone_filter = dfl,
                   prehatch_only = ph)
    gaps <- do.call(insp_env$get_red_bug_gaps, c(list(years_gap = yg), common))
    alls <- do.call(insp_env$get_red_bug_all_sites, common)
    df <- if (grp == "fos") insp_env$get_red_bug_fos_analysis(gaps, alls)
          else insp_env$get_red_bug_facility_analysis(gaps, alls)
    rows <- if (is.null(df) || nrow(df) == 0) list() else
      lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
    list(group_by = grp, years_gap = yg, groups = rows)
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
