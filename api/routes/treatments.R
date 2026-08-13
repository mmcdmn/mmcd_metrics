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
source("/srv/shiny-server/apps/cattail_inspections/data_functions.R", local = cattail_insp_env, chdir = TRUE)

cattail_trt_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/cattail_treatments/data_functions.R", local = cattail_trt_env, chdir = TRUE)

efficacy_env <- new.env(parent = globalenv())
source("/srv/shiny-server/apps/control_efficacy/data_functions.R", local = efficacy_env, chdir = TRUE)
# load_efficacy_data (the % reduction computation) lives in a separate file — source it too.
source("/srv/shiny-server/apps/control_efficacy/efficacy_data_functions.R", local = efficacy_env, chdir = TRUE)

# Map UI treatment-type labels to insptrt action codes.
EFFICACY_TRT_ACTIONS <- list(air = "A", drone = "D", ground = "3")

# ── Cattail treatments shared helpers ──
CATTAIL_GROUP_BYS <- c("facility", "foreman", "zone", "mmcd_all")

# Serialize a summary data.frame (facility/zone/fos summary) into a clean rows list.
.cattail_rows <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

# Load + filter cattail treatment data once, then aggregate (reuses app functions; no new SQL).
.load_cattail <- function(adate, fac, fman, zn) {
  data <- cattail_trt_env$load_raw_data(
    analysis_date = adate, facility_filter = fac, zone_filter = zn
  )
  if (!is.null(fman) && !identical(fman, "all")) {
    data <- cattail_trt_env$apply_data_filters(data, foreman_filter = fman, zone_filter = zn)
  }
  cattail_trt_env$aggregate_cattail_data(data, analysis_date = adate)
}

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

#* Get cattail inspection PROGRESS vs goal — per facility + district total.
#* Reuses get_progress_summary() (goal vs actual per facility/zone). Answers
#* "how far along are cattail inspections" / "which facility is behind goal".
#* @param year Year to compute progress for. Default current year.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /cattail-inspections/progress
#* @serializer json
function(req, res, year = NULL, zone = "1,2", analysis_date = NULL) {
  tryCatch({
    zn    <- validate_zone(zone)
    adate <- validate_date(analysis_date)
    yr    <- suppressWarnings(as.integer(year %||% format(adate, "%Y")))
    if (is.na(yr) || yr < 2000L || yr > as.integer(format(Sys.Date(), "%Y")) + 1L) {
      stop("invalid year")
    }

    summ <- cattail_insp_env$get_progress_summary(
      year = yr, analysis_date = adate, zone_filter = zn
    )
    if (is.null(summ) || nrow(summ) == 0) {
      return(list(year = yr, analysis_date = as.character(adate),
                  district = list(goal = 0L, actual = 0L, pct = 0),
                  facilities = list()))
    }
    rows <- lapply(seq_len(nrow(summ)), function(i) as.list(summ[i, , drop = FALSE]))
    dgoal   <- sum(summ$total_goal, na.rm = TRUE)
    dactual <- sum(summ$total_actual, na.rm = TRUE)
    list(
      year          = yr,
      analysis_date = as.character(adate),
      district      = list(goal = as.integer(dgoal), actual = as.integer(dactual),
                           pct = if (dgoal > 0) round(100 * dactual / dgoal, 1) else 0),
      facilities    = rows
    )
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

# ── Cattail Treatment Summary (value-box stats) ──

#* Get cattail treatment summary — inspected/treated/need-treatment sites + acres,
#* inspection coverage %, treatment completion %. Reuses aggregate_cattail_data.
#* @param facility Facility code. Omit for all.
#* @param foreman FOS shortname. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /cattail-treatments/summary
#* @serializer json
function(req, res, facility = NULL, foreman = NULL, zone = "1,2", analysis_date = NULL) {
  tryCatch({
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn    <- validate_zone(zone)
    adate <- validate_date(analysis_date)

    agg <- .load_cattail(adate, fac, fman, zn)
    ts  <- agg$total_summary
    if (is.null(ts) || nrow(ts) == 0) {
      return(list(analysis_date = as.character(adate), total_count = 0L))
    }
    c(list(
      analysis_date = as.character(adate),
      filters = list(facility = fac, foreman = foreman %||% "all", zone = zn)
    ), as.list(ts[1, , drop = FALSE]))
  }, error = function(e) api_error(res, 400, e$message))
}

# ── Cattail Treatment Summary BY GROUP ──

#* Get cattail treatment summary rolled up by facility / foreman / zone / mmcd_all.
#* @param group_by One of: facility, foreman, zone, mmcd_all. Default facility.
#* @param facility Facility code to narrow to. Omit for all.
#* @param foreman FOS shortname to narrow to. Omit for all.
#* @param zone Zone filter: 1, 2, or 1,2. Default 1,2.
#* @param analysis_date Date YYYY-MM-DD. Default today.
#* @get /cattail-treatments/summary-by-group
#* @serializer json
function(req, res, group_by = "facility", facility = NULL, foreman = NULL,
         zone = "1,2", analysis_date = NULL) {
  tryCatch({
    grp   <- validate_group_by(group_by, CATTAIL_GROUP_BYS, "facility")
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else "all"
    fman  <- if (!is.null(foreman) && nzchar(foreman)) validate_foreman(foreman) else "all"
    zn    <- validate_zone(zone)
    adate <- validate_date(analysis_date)

    agg <- .load_cattail(adate, fac, fman, zn)
    df  <- switch(grp,
      facility = agg$facility_summary,
      foreman  = agg$fos_summary,
      zone     = agg$zone_summary,
      mmcd_all = agg$total_summary,
      agg$facility_summary
    )
    list(
      analysis_date = as.character(adate),
      group_by      = grp,
      groups        = .cattail_rows(df)
    )
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

# ── Control Efficacy Summary (% reduction stats) ──

#* Get control-efficacy SUMMARY — valid/invalid/control checkback counts, median %
#* reduction, and % of checkbacks with >=80% reduction, plus a per-genus breakdown.
#* Reuses load_efficacy_data() (one row per genus per checkback). Answers "how well
#* did treatments work", "what's the median reduction", "efficacy by genus/season".
#* @param start_year First year of the window. Default current year.
#* @param end_year Last year of the window. Default current year.
#* @param facility Facility code. Omit for all.
#* @param genus Both, Aedes, or Culex. Default Both.
#* @param season Spring and/or Summer (comma-separated). Omit for all.
#* @param trt_type Treatment type(s): Air, Ground, Drone (comma-separated). Omit for all.
#* @param material_type all, bti, methoprene, or spinosad. Default all.
#* @param use_mullas Apply Mulla's control correction. Default false.
#* @get /control-efficacy/summary
#* @serializer json
function(req, res, start_year = NULL, end_year = NULL, facility = NULL,
         genus = "Both", season = NULL, trt_type = NULL,
         material_type = "all", use_mullas = "false") {
  tryCatch({
    cur   <- as.integer(format(Sys.Date(), "%Y"))
    sy    <- suppressWarnings(as.integer(start_year %||% cur))
    ey    <- suppressWarnings(as.integer(end_year %||% cur))
    if (is.na(sy) || is.na(ey) || sy < 2010L || ey > cur + 1L || sy > ey) stop("invalid year range")
    fac   <- if (!is.null(facility) && nzchar(facility)) validate_facility(facility) else NULL
    gen   <- { g <- tolower(trimws(genus %||% "both"))
               if (!g %in% c("both", "aedes", "culex")) stop("genus must be Both, Aedes, or Culex")
               g }
    seas  <- if (!is.null(season) && nzchar(season)) {
      s <- tools::toTitleCase(tolower(trimws(unlist(strsplit(season, ",", fixed = TRUE)))))
      bad <- s[!s %in% c("Spring", "Summer")]; if (length(bad) > 0) stop("season must be Spring/Summer")
      s
    } else NULL
    ttypes <- if (!is.null(trt_type) && nzchar(trt_type)) {
      t <- tolower(trimws(unlist(strsplit(trt_type, ",", fixed = TRUE))))
      bad <- t[!t %in% names(EFFICACY_TRT_ACTIONS)]; if (length(bad) > 0) stop("trt_type must be Air/Ground/Drone")
      unlist(EFFICACY_TRT_ACTIONS[t], use.names = FALSE)
    } else NULL
    mtype <- tolower(trimws(material_type %||% "all"))
    if (!mtype %in% c("all", "bti", "methoprene", "spinosad")) stop("invalid material_type")
    mulla <- isTRUE(as.logical(use_mullas))

    eff <- efficacy_env$load_efficacy_data(
      start_year = sy, end_year = ey, bti_only = (mtype == "bti"), use_mullas = mulla
    )
    empty <- list(start_year = sy, end_year = ey,
                  valid_count = 0L, invalid_count = 0L, control_count = 0L,
                  median_pct_reduction = NA, pct_above_80 = 0, by_genus = list())
    if (is.null(eff) || nrow(eff) == 0) return(empty)

    # Post-filters (all in R on the one loaded frame — no new query).
    if (!is.null(fac)) eff <- eff[!is.na(eff$facility) & eff$facility %in% fac, , drop = FALSE]
    if (gen != "both") eff <- eff[tolower(eff$genus) == gen, , drop = FALSE]
    if (!is.null(seas) && "season" %in% names(eff)) eff <- eff[eff$season %in% seas, , drop = FALSE]
    if (!is.null(ttypes) && "trt_action" %in% names(eff)) eff <- eff[eff$trt_action %in% ttypes, , drop = FALSE]
    if (mtype %in% c("methoprene", "spinosad") && "mattype" %in% names(eff)) {
      eff <- eff[grepl(mtype, tolower(eff$mattype %||% ""), fixed = TRUE), , drop = FALSE]
    }
    if (nrow(eff) == 0) return(empty)

    valid <- eff[!(eff$is_control %in% TRUE) & !(eff$is_invalid %in% TRUE) & !is.na(eff$pct_reduction), , drop = FALSE]
    n_valid   <- nrow(valid)
    n_invalid <- sum(eff$is_invalid == TRUE, na.rm = TRUE)
    n_control <- sum(eff$is_control == TRUE, na.rm = TRUE)

    by_genus <- lapply(split(valid, valid$genus), function(g) {
      list(genus = as.character(g$genus[1]), n = nrow(g),
           median_reduction = round(median(g$pct_reduction, na.rm = TRUE), 1),
           pct_above_80 = round(100 * mean(g$pct_reduction >= 80, na.rm = TRUE), 1))
    })

    list(
      start_year = sy, end_year = ey,
      filters = list(facility = fac %||% "all", genus = genus, season = seas %||% "all",
                     trt_type = trt_type %||% "all", material_type = mtype, use_mullas = mulla),
      valid_count = n_valid, invalid_count = as.integer(n_invalid), control_count = as.integer(n_control),
      median_pct_reduction = if (n_valid > 0) round(median(valid$pct_reduction, na.rm = TRUE), 1) else NA,
      pct_above_80 = if (n_valid > 0) round(100 * mean(valid$pct_reduction >= 80, na.rm = TRUE), 1) else 0,
      by_genus = unname(by_genus)
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
