# =============================================================================
# API Routes — Drone Treatment Checklist
# =============================================================================
# Returns drone treatment data organized by round (sequential treatment per site
# per year). Used by the Google Sheets drone_filler script.
#
# Mounted under /v1/private/drone/...
# Auth is enforced by the parent plumber's auth_gate filter (blocks all
# /v1/private/* requests without a valid API key).
# =============================================================================

source("/srv/shiny-server/shared/db_helpers.R")
source("/srv/shiny-server/shared/app_libraries.R")
source("/srv/api/api_helpers.R")


# =============================================================================
# ── DRONE TREATMENT CHECKLIST
# =============================================================================

#* Get drone treatment rounds for a given year.
#* Returns one row per sitecode per round, ordered by date.
#* Round 1 = first drone treatment of the year, Round 2 = second, etc.
#*
#* @param year       Year to query (default current year)
#* @param sitecodes  Optional comma-separated sitecodes to filter
#* @param facility   Optional facility code filter
#* @get /checklist
#* @serializer json
function(req, res, year = NULL, sitecodes = NULL, facility = NULL) {
  tryCatch({
    # ── Validate parameters ──
    yr <- if (is.null(year) || !nzchar(trimws(year %||% ""))) {
      as.integer(format(Sys.Date(), "%Y"))
    } else {
      n <- suppressWarnings(as.integer(year))
      if (is.na(n) || n < 2000 || n > 2100) stop("invalid year")
      n
    }

    # Parse sitecodes if provided
    site_filter <- NULL
    if (!is.null(sitecodes) && nzchar(trimws(sitecodes %||% ""))) {
      sites_raw <- trimws(unlist(strsplit(as.character(sitecodes), ",", fixed = TRUE)))
      # Validate each sitecode format
      bad <- sites_raw[!grepl("^[A-Za-z0-9 _-]+$", sites_raw) | nchar(sites_raw) > 20]
      if (length(bad) > 0) stop("invalid sitecode format")
      site_filter <- sites_raw[nzchar(sites_raw)]
    }

    # Validate facility
    fac_filter <- NULL
    if (!is.null(facility) && nzchar(trimws(facility %||% ""))) {
      fac_filter <- validate_facility(facility)
    }

    # ── Query database ──
    con <- get_db_connection()
    if (is.null(con)) stop("database connection failed")
    on.exit(safe_disconnect(con), add = TRUE)

    # Build site filter clause
    site_clause <- ""
    if (!is.null(site_filter) && length(site_filter) > 0) {
      quoted <- paste(DBI::dbQuoteString(con, site_filter), collapse = ",")
      site_clause <- paste0("AND t.sitecode IN (", quoted, ")")
    }

    # Build facility clause
    fac_clause <- ""
    if (!is.null(fac_filter)) {
      fac_clause <- paste0("AND sc.fac_for_air = ", DBI::dbQuoteString(con, fac_filter))
    }

    query <- paste0("
      WITH drone_treatments AS (
        SELECT
          t.sitecode,
          t.inspdate,
          t.matcode,
          t.amts,
          t.acres,
          t.emp1,
          t.pkey
        FROM dblarv_insptrt_current t
        LEFT JOIN gis_sectcode sc ON LEFT(t.sitecode, 7) = sc.sectcode
        WHERE t.action = 'D'
          AND EXTRACT(YEAR FROM t.inspdate) = ", yr, "
          AND t.matcode IS NOT NULL
          ", site_clause, "
          ", fac_clause, "

        UNION ALL

        SELECT
          t.sitecode,
          t.inspdate,
          t.matcode,
          t.amts,
          t.acres,
          t.emp1,
          t.pkey
        FROM dblarv_insptrt_archive t
        LEFT JOIN gis_sectcode sc ON LEFT(t.sitecode, 7) = sc.sectcode
        WHERE t.action = 'D'
          AND EXTRACT(YEAR FROM t.inspdate) = ", yr, "
          AND t.matcode IS NOT NULL
          ", site_clause, "
          ", fac_clause, "
      ),
      ranked AS (
        SELECT
          sitecode,
          inspdate,
          matcode,
          amts,
          acres,
          emp1,
          ROW_NUMBER() OVER (PARTITION BY sitecode ORDER BY inspdate, pkey) AS round_num
        FROM drone_treatments
      )
      SELECT
        sitecode,
        round_num,
        inspdate AS treatment_date,
        amts AS amount,
        acres,
        emp1 AS emp_num,
        matcode AS material
      FROM ranked
      ORDER BY sitecode, round_num
    ")

    results <- DBI::dbGetQuery(con, query)

    if (is.null(results) || nrow(results) == 0) {
      return(list(
        year  = yr,
        count = 0L,
        data  = list()
      ))
    }

    # Format dates as M/D for display
    results$treatment_date <- format(as.Date(results$treatment_date), "%m/%d/%Y")

    list(
      year  = yr,
      count = nrow(results),
      data  = results
    )

  }, error = function(e) {
    api_error(res, 400, e$message)
  })
}
