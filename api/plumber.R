# =============================================================================
# MMCD Metrics — Plumber REST API
# =============================================================================
# This is the ONLY file that defines the live API.
# api/filters.R is a deprecated legacy file and is NOT run in the container.
#
# STARTED BY: startup.sh  (Rscript /srv/api/run_plumber.R)
# INTERNAL PORT: 9000 (never exposed externally)
# EXTERNAL URL:  https://metrics.mmcd.org/v1/...
#
# ─── CHANGING THE API KEY ────────────────────────────────────────────────────
# The key is set via the API_KEYS environment variable.
# Default placeholder is written in startup.sh — search that file for:
#   API_KEYS=
# Change it there, then rebuild the container.
# For production you should pass it as a Docker/AWS env var instead of
# hardcoding it in startup.sh.
#
# ─── ROUTES ──────────────────────────────────────────────────────────────────
# Public  (no key):  GET /v1/public/health
#                    GET /v1/public/facilities
#                    GET /v1/public/foremen
#                    GET /v1/public/threshold
#
# Private (key req): GET /v1/private/sectcodes
#                    GET /v1/private/air-checklist
#
# =============================================================================

library(plumber)
library(jsonlite)
library(dplyr)
library(DBI)

# ── Shared helpers: DB connection, lookup tables, table strategy, SQL helpers
source("/srv/shiny-server/shared/db_helpers.R")
source("/srv/shiny-server/shared/app_libraries.R")

load_env_vars()

# Pre-warm lookup caches at startup so first requests are fast
tryCatch({
  get_facility_lookup()
  get_foremen_lookup()
  message("[api] Lookup tables pre-warmed")
}, error = function(e) {
  message("[api] Lookup pre-warm failed (non-fatal): ", e$message)
})


# =============================================================================
# ── AUTH HELPERS
# =============================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

get_api_keys <- function() {
  raw <- Sys.getenv("API_KEYS", unset = "")
  if (identical(raw, "")) return(character(0))
  keys <- trimws(unlist(strsplit(raw, ",", fixed = TRUE)))
  keys[nzchar(keys)]
}

extract_token <- function(req) {
  key_hdr  <- req$HTTP_X_API_KEY    %||% ""
  auth_hdr <- req$HTTP_AUTHORIZATION %||% ""
  if (nzchar(key_hdr)) return(trimws(key_hdr))
  if (grepl("^Bearer\\s+", auth_hdr, ignore.case = TRUE))
    return(sub("^Bearer\\s+", "", auth_hdr, ignore.case = TRUE))
  ""
}

is_authorized <- function(req) {
  keys <- get_api_keys()
  if (length(keys) == 0) return(FALSE)
  tok <- extract_token(req)
  if (!nzchar(tok)) return(FALSE)
  # Use identical() to avoid timing side-channels leaking key length
  any(vapply(keys, identical, logical(1), tok))
}


# =============================================================================
# ── INPUT VALIDATION
# Each function either returns a safe value or stop()s with a clear message.
# =============================================================================

# Generic text: max length + allow-list of characters
clean_text <- function(value, max_chars = 32L) {
  if (is.null(value) || !nzchar(trimws(value %||% ""))) return(NULL)
  s <- trimws(as.character(value))
  if (nchar(s) > max_chars) stop(paste0("value too long (max ", max_chars, " chars)"))
  if (!grepl("^[A-Za-z0-9 _-]+$", s)) stop("value contains invalid characters")
  s
}

# Integer with hard bounds — never allows NA or out-of-range values through
clean_limit <- function(v, default = 500L, max_val = 5000L) {
  n <- suppressWarnings(as.integer(v %||% default))
  if (is.na(n) || n < 1L) n <- default
  if (n > max_val) n <- max_val
  n
}

# Facility: format check THEN allow-list against live DB lookup
validate_facility <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return(NULL)
  s <- trimws(as.character(v))
  if (nchar(s) > 8L || !grepl("^[A-Za-z0-9]+$", s)) stop("invalid facility code")
  lkp <- tryCatch(get_facility_lookup(), error = function(e) NULL)
  if (!is.null(lkp) && nrow(lkp) > 0) {
    # Case-insensitive match, but return the DB-cased value
    match_row <- lkp[tolower(lkp$short_name) == tolower(s), ]
    if (nrow(match_row) == 0) stop(paste0("unknown facility: ", s))
    return(match_row$short_name[1])
  }
  s
}

# Foreman shortname → emp_num (allow-list; prevents guessing column values)
validate_foreman <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return(NULL)
  s <- trimws(as.character(v))
  if (nchar(s) > 32L || !grepl("^[A-Za-z0-9 _-]+$", s)) stop("invalid foreman format")
  lkp <- tryCatch(get_foremen_lookup(), error = function(e) NULL)
  if (is.null(lkp) || nrow(lkp) == 0) return(NULL)
  row <- lkp[tolower(lkp$shortname) == tolower(s), ]
  if (nrow(row) == 0) stop(paste0("unknown foreman shortname: ", s))
  as.character(row$emp_num[1])
}

# Zone: only "1", "2", or combination
validate_zone <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return(c("1", "2"))
  parts <- trimws(unlist(strsplit(as.character(v), ",", fixed = TRUE)))
  bad <- parts[!parts %in% c("1", "2")]
  if (length(bad) > 0) stop(paste0("zone must be 1 or 2, got: ", paste(bad, collapse = ",")))
  parts
}

# Date: ISO-8601 only, max 2 years in the past, not in the future
validate_date <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return(Sys.Date())
  d <- tryCatch(as.Date(trimws(v), "%Y-%m-%d"), error = function(e) as.Date(NA))
  if (is.na(d)) stop("date must be YYYY-MM-DD")
  if (d > Sys.Date() + 1L || d < Sys.Date() - 730L) stop("date out of allowed range")
  d
}

# Lookback: strict 1–14 integer
validate_lookback <- function(v) {
  n <- suppressWarnings(as.integer(v %||% 2L))
  if (is.na(n) || n < 1L || n > 14L) stop("lookback_days must be 1-14")
  n
}

# Priority: optional comma-separated list (RED, YELLOW, BLUE, GREEN, PURPLE)
validate_priority <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return(NULL)
  valid <- c("RED", "YELLOW", "BLUE", "GREEN", "PURPLE")
  parts <- trimws(toupper(unlist(strsplit(as.character(v), ",", fixed = TRUE))))
  bad <- parts[!parts %in% valid]
  if (length(bad) > 0) stop(paste0("invalid priority: ", paste(bad, collapse = ",")))
  parts
}

# Build a SQL IN clause from already-validated values.
# Uses dbQuoteString as a second safety layer — even if validation is bypassed
# somehow, the driver will quote values correctly.
safe_in <- function(con, col, vals, prefix = "AND ") {
  if (is.null(vals) || length(vals) == 0) return("")
  quoted <- vapply(vals, function(v) as.character(DBI::dbQuoteString(con, v)), character(1))
  paste0(prefix, col, " IN (", paste(quoted, collapse = ", "), ")")
}

# Standard error response
api_error <- function(res, status, msg) {
  res$status <- as.integer(status)
  list(error = msg)
}

# Convert a named choices vector to the label/code list format
to_choice_list <- function(choices, all_label) {
  out <- lapply(names(choices), function(n) list(label = n, code = unname(choices[[n]])))
  c(list(list(label = all_label, code = "all")), out)
}


# =============================================================================
# ── PLUMBER FILTERS
# =============================================================================

#* CORS headers — required for browser and Google Sheets access
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Headers", "Authorization, X-API-Key, Content-Type")
  res$setHeader("Access-Control-Allow-Methods", "GET, OPTIONS")
  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$status <- 204
    return(list())
  }
  plumber::forward()
}

#* Auth gate — blocks /v1/private/* without a valid API key
#* @filter auth_gate
function(req, res) {
  if (startsWith(req$PATH_INFO %||% "", "/v1/private")) {
    if (!is_authorized(req))
      return(api_error(res, 401, "unauthorized: valid API key required"))
  }
  plumber::forward()
}


# =============================================================================
# ── PUBLIC ENDPOINTS  (no API key required)
# =============================================================================

#* @get /v1/public/health
#* @json
function() {
  list(status = "ok", timestamp = as.character(Sys.time()))
}

#* @get /v1/public/facilities
#* @json
function() {
  tryCatch(
    to_choice_list(get_facility_choices(include_all = FALSE), "All Facilities"),
    error = function(e) list(error = e$message)
  )
}

#* @get /v1/public/foremen
#* @json
function() {
  tryCatch(
    to_choice_list(get_foreman_choices(include_all = FALSE), "All FOS Areas"),
    error = function(e) list(error = e$message)
  )
}

#* Numdip threshold based on the spring ACT4-P1 date.
#* Returns 1 before the threshold date, 2 on/after. Resets to 1 on Jan 1.
#* @get /v1/public/threshold
#* @json
function() {
  tryCatch({
    today <- Sys.Date()
    current_year <- as.integer(format(today, "%Y"))

    con <- get_db_connection()
    on.exit(safe_disconnect(con), add = TRUE)

    qry <- DBI::sqlInterpolate(con,
      "SELECT date_start
       FROM   public.lookup_threshold_larv
       WHERE  description = 'ACT4-P1'
         AND  EXTRACT(year FROM date_start) = ?yr
         AND  NOT (EXTRACT(month FROM date_start) = 1
                   AND EXTRACT(day FROM date_start) = 1)
       ORDER  BY date_start
       LIMIT  1",
      yr = current_year
    )
    result <- DBI::dbGetQuery(con, qry)

    if (nrow(result) > 0) {
      threshold_date <- as.Date(result$date_start[1])
      thresh <- if (today >= threshold_date) 2L else 1L
    } else {
      threshold_date <- NULL
      thresh <- 1L
    }

    list(
      threshold        = thresh,
      threshold_date   = if (!is.null(threshold_date)) as.character(threshold_date) else NA,
      as_of            = as.character(today),
      year             = current_year
    )
  }, error = function(e) {
    list(threshold = 2L, threshold_date = NA,
         as_of = as.character(Sys.Date()), error = e$message)
  })
}


# =============================================================================
# ── PRIVATE ENDPOINTS  (API key required)
# =============================================================================

#* Section-code reference data.
#* @param facility Optional facility code (MO, E, W, N, Sr, Sj …)
#* @param limit    Row cap 1–5000, default 500
#* @get /v1/private/sectcodes
#* @json
function(facility = NULL, limit = 500, res) {
  tryCatch({
    fac_v <- validate_facility(facility)
    lim_v <- clean_limit(limit)

    con <- get_db_connection()
    on.exit(safe_disconnect(con), add = TRUE)

    # sqlInterpolate handles value binding — no string concatenation
    sql <- DBI::sqlInterpolate(
      con,
      "SELECT sectcode, facility, fosarea, zone
       FROM   public.gis_sectcode
       WHERE  (?facility IS NULL OR facility = ?facility)
       ORDER  BY sectcode
       LIMIT  ?limit",
      facility = fac_v,
      limit    = lim_v
    )

    rows <- DBI::dbGetQuery(con, sql)
    list(count = nrow(rows), data = rows, timestamp = as.character(Sys.time()))
  }, error = function(e) api_error(res, 400, e$message))
}


#* Air site checklist — inspection, active treatment, and lab bug status.
#* Mirrors the Shiny Air Inspection Checklist app.
#* Use this endpoint to drive a live Google Sheet that stays in sync with the DB.
#*
#* @param facility     Optional facility code (MO, E, W, N, Sr, Sj …)
#* @param foreman      Optional FOS shortname (e.g. Smith)
#* @param zone         Zones to include: "1", "2", or "1,2"  (default "1,2")
#* @param lookback_days How many days back to look for inspections (1–14, default 2)
#* @param as_of        Analysis date YYYY-MM-DD (default today)
#* @param priority     Optional priority filter (comma-sep): RED, YELLOW, BLUE, GREEN, PURPLE
#* @get /v1/private/air-checklist
#* @json
function(facility = NULL, foreman = NULL, zone = "1,2",
         lookback_days = 2, as_of = NULL, priority = NULL, res) {
  tryCatch({

    # ── 1. Validate every parameter (no raw user string touches SQL)
    fac_v     <- validate_facility(facility)
    fos_emp_v <- validate_foreman(foreman)
    zone_v    <- validate_zone(zone)
    lb_v      <- validate_lookback(lookback_days)
    pri_v     <- validate_priority(priority)
    date_v    <- validate_date(as_of)
    start_v   <- date_v - lb_v

    # ── 2. Choose current vs archive table based on actual DB year ranges
    tbl     <- get_table_strategy(date_v)
    ins_tbl <- if (tbl$query_archive && !tbl$query_current)
                 "dblarv_insptrt_archive"
               else if (!tbl$query_archive)
                 "dblarv_insptrt_current"
               else
                 "(SELECT * FROM dblarv_insptrt_current
                   UNION ALL SELECT * FROM dblarv_insptrt_archive)"
    smp_tbl <- if (tbl$query_archive && !tbl$query_current)
                 "dblarv_sample_archive"
               else if (!tbl$query_archive)
                 "dblarv_sample_current"
               else
                 "(SELECT * FROM dblarv_sample_current
                   UNION ALL SELECT * FROM dblarv_sample_archive)"

    # ── 3. Open connection; all quoting done with dbQuoteString
    con <- get_db_connection()
    on.exit(safe_disconnect(con), add = TRUE)

    # WHERE clauses: validated values → dbQuoteString → IN clause
    fac_clause <- safe_in(con, "sc.facility", fac_v)
    fos_clause <- safe_in(con, "sc.fosarea",  fos_emp_v)
    zon_clause <- safe_in(con, "sc.zone",     zone_v)
    pri_clause <- safe_in(con, "b.priority",  pri_v)

    # Dates: already validated as R Date objects; only converted to string here
    d_sql  <- as.character(date_v)
    lb_sql <- as.character(start_v)

    query <- paste0("
      WITH RedAirSites AS (
        SELECT DISTINCT ON (b.sitecode)
          b.sitecode, b.acres, b.priority,
          sc.facility, sc.zone, sc.fosarea, sc.sectcode
        FROM loc_breeding_sites b
        LEFT JOIN gis_sectcode sc ON LEFT(b.sitecode, 7) = sc.sectcode
        WHERE (b.enddate IS NULL OR b.enddate > '", d_sql, "')
          AND b.air_gnd = 'A'
          ", pri_clause, "
          ", fac_clause, "
          ", fos_clause, "
          ", zon_clause, "
        ORDER BY b.sitecode, b.enddate NULLS LAST
      ),

      RecentInspections AS (
        SELECT
          i.sitecode,
          i.inspdate,
          i.numdip,
          i.wet,
          i.emp1,
          i.sampnum_yr,
          ROW_NUMBER() OVER (PARTITION BY i.sitecode ORDER BY i.inspdate DESC) AS rn
        FROM ", ins_tbl, " i
        WHERE i.inspdate BETWEEN '", lb_sql, "'::date AND '", d_sql, "'::date
          AND i.action IN ('2','4')
          AND i.sitecode IN (SELECT sitecode FROM RedAirSites)
      ),

      RecentTreatments AS (
        SELECT
          t.sitecode,
          t.inspdate                                                      AS last_trt_date,
          t.mattype,
          t.inspdate + INTERVAL '1 day' * COALESCE(mt.effect_days, 14)   AS trt_expiry,
          COALESCE(mt.prehatch, FALSE)                                    AS is_prehatch,
          ROW_NUMBER() OVER (PARTITION BY t.sitecode ORDER BY t.inspdate DESC) AS rn
        FROM ", ins_tbl, " t
        LEFT JOIN mattype_list_targetdose mt ON t.matcode = mt.matcode
        WHERE t.inspdate <= '", d_sql, "'::date
          AND t.action IN ('3','A','D')
          AND t.matcode IS NOT NULL AND t.matcode != ''
          AND t.sitecode IN (SELECT sitecode FROM RedAirSites)
      ),

      ActiveTreatments AS (
        SELECT sitecode, mattype AS active_material,
               last_trt_date, trt_expiry, is_prehatch
        FROM RecentTreatments
        WHERE rn = 1 AND trt_expiry > '", d_sql, "'::date
      ),

      LabResults AS (
        SELECT ls.sampnum_yr, ls.redblue, ls.missing
        FROM ", smp_tbl, " ls
        WHERE ls.sampnum_yr IS NOT NULL
      ),

      Employees AS (
        SELECT DISTINCT ON (emp_num) emp_num, shortname
        FROM employee_list
        WHERE active = true
        ORDER BY emp_num, pkey DESC
      ),

      FosNames AS (
        SELECT DISTINCT ON (emp_num) emp_num, shortname AS fos_name
        FROM employee_list
        WHERE active = true AND emp_type = 'FieldSuper'
        ORDER BY emp_num, pkey DESC
      ),

      TownLookup AS (
        SELECT DISTINCT
          LPAD(CAST(towncode AS text), 4, '0') AS towncode4,
          city
        FROM lookup_towncode_name
      )

      SELECT
        s.sitecode,
        ROUND(s.acres::numeric, 2)          AS acres,
        s.priority,
        s.facility,
        s.zone,
        fl.fos_name,
        s.fosarea,
        s.sectcode,
        COALESCE(town.city, LEFT(s.sitecode, 4)) AS township_name,
        cards.airmap_num,
        (i.sitecode IS NOT NULL)            AS was_inspected,
        i.inspdate                          AS last_insp_date,
        i.numdip                            AS dip_count,
        i.wet                               AS pct_wet,
        emp.shortname                       AS inspector_name,
        i.sampnum_yr,
        COALESCE(cards.remarks, '')                          AS remarks,
        (COALESCE(cards.ra, '') != '')                       AS restricted_area,
        (COALESCE(cards.drone, '') IN ('Y','M'))             AS drone,
        (COALESCE(cards.sample, '') != '')                   AS needs_sample,
        CASE
          WHEN i.sitecode IS NULL
            OR i.sampnum_yr IS NULL
            OR i.sampnum_yr = ''           THEN 'No Sample'
          WHEN lr.missing IS TRUE          THEN 'Pending Lab'
          WHEN lr.redblue = 'R'            THEN 'Red Bugs'
          WHEN lr.redblue = 'B'            THEN 'Blue Bugs'
          ELSE                                  'No Bugs'
        END                                 AS bug_status,
        (at.sitecode IS NOT NULL)           AS has_active_treatment,
        at.active_material,
        at.last_trt_date                    AS active_trt_date,
        at.trt_expiry                       AS active_trt_expiry,
        COALESCE(at.is_prehatch, FALSE)     AS is_prehatch
      FROM RedAirSites s
      LEFT JOIN RecentInspections i     ON s.sitecode = i.sitecode AND i.rn = 1
      LEFT JOIN LabResults lr           ON i.sampnum_yr = lr.sampnum_yr
      LEFT JOIN ActiveTreatments at     ON s.sitecode = at.sitecode
      LEFT JOIN loc_breeding_site_cards_sjsreast2 cards
                                        ON s.sitecode = cards.sitecode
      LEFT JOIN TownLookup town        ON LEFT(s.sitecode, 4) = town.towncode4
      LEFT JOIN Employees emp           ON i.emp1 = emp.emp_num::text
      LEFT JOIN FosNames fl             ON s.fosarea = fl.emp_num::text
      ORDER BY s.fosarea, town.city NULLS LAST, cards.airmap_num NULLS LAST, s.sectcode, s.sitecode
    ")

    rows <- DBI::dbGetQuery(con, query)

    # Ensure logical columns come back as TRUE/FALSE (some drivers return 0/1)
    for (col in c("was_inspected", "has_active_treatment", "is_prehatch")) {
      if (col %in% names(rows)) rows[[col]] <- as.logical(rows[[col]])
    }

    list(
      count           = nrow(rows),
      as_of           = d_sql,
      lookback_days   = lb_v,
      facility_filter = facility %||% "all",
      foreman_filter  = foreman  %||% "all",
      priority_filter = if (!is.null(pri_v)) paste(pri_v, collapse = ",") else "all",
      data            = rows,
      refreshed_at    = as.character(Sys.time())
    )
  }, error = function(e) api_error(res, 400, e$message))
}
