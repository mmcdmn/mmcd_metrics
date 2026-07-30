# =============================================================================
# API Routes — Drone Treatment Checklist
# =============================================================================
# Returns drone treatment data organized by round (sequential treatment per site
# per year). Used by the Google Sheets drone_filler script.
#
# Treatment inclusion:
#   action='D'      — drone treatment, any matcode
#   action='1','3'  — ground treatment, but ONLY if:
#                      (a) mattype_list_targetdose.prehatch = true  AND
#                      (b) site is a drone-designated site in loc_breeding_sites
#                          (drone IN Y/M/C or air_gnd='D', not ended)
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
#* @param year             Year to query (default current year)
#* @param sitecodes        Optional comma-separated sitecodes to filter
#* @param facility         Optional facility code filter
#* @param ground_materials Comma-separated matcodes to count for ground actions
#*                         (1 & 3) on drone-designated sites. If omitted, falls
#*                         back to mattype_list_targetdose.prehatch = true.
#*                         Pass empty string to disable ground treatment counting.
#* @get /checklist
#* @serializer json
function(req, res, year = NULL, sitecodes = NULL, facility = NULL,
         ground_materials = NULL) {
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

    # Parse ground_materials: NULL → prehatch fallback; "" → disabled; "T7,N1,…" → list
    gm_provided  <- !is.null(ground_materials)
    gm_disabled  <- gm_provided && !nzchar(trimws(ground_materials))
    gm_list      <- NULL
    if (gm_provided && !gm_disabled) {
      gm_raw  <- trimws(unlist(strsplit(as.character(ground_materials), ",", fixed = TRUE)))
      bad_mat <- gm_raw[!grepl("^[A-Za-z0-9]+$", gm_raw) | nchar(gm_raw) > 10]
      if (length(bad_mat) > 0) stop("invalid matcode in ground_materials")
      gm_list <- gm_raw[nzchar(gm_raw)]
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

    # Build ground-action filter clause:
    #   gm_disabled → no ground actions counted (action D only)
    #   gm_list     → explicit matcode list from caller
    #   fallback    → prehatch = true from mattype_list_targetdose
    ground_clause <- if (gm_disabled) {
      "t.action = 'D'"
    } else if (!is.null(gm_list)) {
      quoted_mats <- paste(DBI::dbQuoteString(con, gm_list), collapse = ",")
      paste0(
        "t.action = 'D'\n",
        "          OR (t.action IN ('1','3') AND t.matcode IN (", quoted_mats, ")",
        " AND EXISTS (\n",
        "            SELECT 1 FROM public.loc_breeding_sites bs\n",
        "            WHERE bs.sitecode = t.sitecode\n",
        "              AND (bs.drone IN ('Y','M','C') OR bs.air_gnd = 'D')\n",
        "              AND (bs.enddate IS NULL OR bs.enddate > CURRENT_DATE)\n",
        "          ))"
      )
    } else {
      paste0(
        "t.action = 'D'\n",
        "          OR (t.action IN ('1','3') AND m.prehatch = true AND EXISTS (\n",
        "            SELECT 1 FROM public.loc_breeding_sites bs\n",
        "            WHERE bs.sitecode = t.sitecode\n",
        "              AND (bs.drone IN ('Y','M','C') OR bs.air_gnd = 'D')\n",
        "              AND (bs.enddate IS NULL OR bs.enddate > CURRENT_DATE)\n",
        "          ))"
      )
    }

    # Only need the mattype join for the prehatch fallback path
    mat_join <- if (!gm_disabled && is.null(gm_list)) {
      "LEFT JOIN mattype_list_targetdose m ON t.matcode = m.matcode"
    } else {
      ""
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
          t.pkey_pg
        FROM dblarv_insptrt_current t
        LEFT JOIN gis_sectcode sc ON LEFT(t.sitecode, 7) = sc.sectcode
        ", mat_join, "
        WHERE (
          ", ground_clause, "
        )
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
          t.pkey_pg
        FROM dblarv_insptrt_archive t
        LEFT JOIN gis_sectcode sc ON LEFT(t.sitecode, 7) = sc.sectcode
        ", mat_join, "
        WHERE (
          ", ground_clause, "
        )
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
          ROW_NUMBER() OVER (PARTITION BY sitecode ORDER BY inspdate, pkey_pg) AS round_num
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
