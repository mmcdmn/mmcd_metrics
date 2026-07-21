# =============================================================================
# LIVE ABUNDANCE FALLBACK — Trap Surveillance
# =============================================================================
# Computes mosquito abundance ON READ, straight from the live source view
# dbadult_insp_w_id_ff3s_allyears, for weeks that are STALE / missing in the
# materialized view dbadult_mon_nt_co2_forvectorabundance.
#
# The materialized view is Jordan's abundance definition, but it is a SNAPSHOT
# and is not auto-refreshed. When lab counts land after the last refresh, the
# most recent weeks show rows with mosqcount = NULL ("empty"), even though the
# underlying live view already has the counts. This module recovers those weeks.
#
# Never writes to the database. Results cached in Redis (short TTL — this data
# is current-season and changes as lab IDs come in). If Redis is unavailable it
# simply recomputes (the query is a cheap aggregation, no external package).
#
# Methodology mirrors the materialized-view SQL EXACTLY (verified: reproduces
# the matview for non-stale weeks, recovers real counts for stale weeks):
#   base:  dbadult_insp_w_id_ff3s_allyears, network_type='mnt', survtype='6',
#          missing IS NULL; week via calc_week_num(inspdate).
#   viarea: loc_vectorindexareas_sections_a on LEFT(sitecode,7);
#           left(loc_code,1)='X' OR sitecode IS NULL -> 'Out'.
#   count:  per species column, with the zero-count rule:
#           CASE WHEN <col> IS NULL AND zero_count='t' THEN 0 ELSE <col> END
#
# Return-column shapes MATCH the fetch_abundance_* functions in data_functions.R
# so these are drop-in fallbacks.
# =============================================================================

.LIVE_ABUNDANCE_TTL <- 1800L  # 30 min — cheap query, current-season data

.cache_live_abundance <- function(key, load_func) {
  if (exists("get_app_cached_redis", mode = "function")) {
    return(get_app_cached_redis(key, load_func, ttl = .LIVE_ABUNDANCE_TTL))
  }
  load_func()
}

# Map the abundance-view spp_name to its source count column in
# dbadult_insp_w_id_ff3s_allyears. (Whitelist -> no user string reaches SQL.)
.ABUNDANCE_SPP_COL <- c(
  "Total_Cx_vectors"        = "cxvectotal",
  "Cx_pipiens_33"           = "cx_pip_cnt",
  "Cx_restuans_34"          = "cx_res_cnt",
  "Cx_tarsalis_36"          = "cx_tar_cnt",
  "Cx_restuans/pipiens_372" = "cxrp372cnt"
)

# Base FROM/WHERE for the live source (without the week filter, added by caller).
.LIVE_ABUNDANCE_BASE <- "
  FROM dbadult_insp_w_id_ff3s_allyears s
  LEFT JOIN loc_vectorindexareas_sections_a v ON v.sectcode = LEFT(s.sitecode, 7)
  WHERE s.network_type = 'mnt' AND s.survtype = '6' AND s.missing IS NULL"

# =============================================================================
# ABUNDANCE BY AREA — matches fetch_abundance_by_area()
# columns: viarea, total_count, num_traps, avg_per_trap
# =============================================================================
compute_abundance_by_area_live <- function(yrwk, spp_name = "Total_Cx_vectors") {
  cnt_col <- .ABUNDANCE_SPP_COL[[spp_name]]
  if (is.null(cnt_col)) {
    warning(sprintf("[live] abundance: unmapped spp_name '%s'", spp_name))
    return(NULL)
  }
  key <- paste0("live_abund_area:", yrwk, ":", spp_name)
  .cache_live_abundance(key, function() {
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    on.exit(safe_disconnect(con))

    # mosqcount expression with the zero-count rule (same as the matview)
    mc <- sprintf("CASE WHEN s.%s IS NULL AND s.zero_count = 't' THEN 0 ELSE s.%s END",
                  cnt_col, cnt_col)

    q <- sprintf(
      "WITH sampleset AS (
         SELECT s.loc_code, s.zero_count,
                %s AS mosqcount,
                CASE WHEN left(s.loc_code,1) = 'X' OR s.sitecode IS NULL THEN 'Out'
                     ELSE v.viareaa END AS viarea
         %s
           AND calc_week_num(s.inspdate) = %d
       )
       SELECT viarea,
              SUM(mosqcount)            AS total_count,
              COUNT(DISTINCT loc_code)  AS num_traps,
              CASE WHEN COUNT(DISTINCT loc_code) > 0
                   THEN ROUND(SUM(mosqcount)::numeric / COUNT(DISTINCT loc_code), 2)
                   ELSE 0 END           AS avg_per_trap
       FROM sampleset
       GROUP BY viarea
       ORDER BY viarea",
      mc, .LIVE_ABUNDANCE_BASE, as.integer(yrwk)
    )

    tryCatch({
      d <- dbGetQuery(con, q)
      if (is.null(d) || nrow(d) == 0 || all(is.na(d$total_count))) return(NULL)
      message(sprintf("[live] Abundance by area: %d areas for yrwk %s, %s (computed from source view)",
                      nrow(d), yrwk, spp_name))
      d
    }, error = function(e) {
      warning(paste("[live] abundance by area failed:", e$message)); NULL
    })
  })
}

# =============================================================================
# ABUNDANCE PER-TRAP ROWS — matches fetch_abundance_data()
# columns: viarea, loc_code, inspdate, year, yrwk, epiweek, spp_name, mosqcount
# `weeks` = integer vector of yrwk values to compute (used to fill stale weeks).
# =============================================================================
compute_abundance_data_live <- function(weeks, spp_name = "Total_Cx_vectors") {
  cnt_col <- .ABUNDANCE_SPP_COL[[spp_name]]
  if (is.null(cnt_col)) {
    warning(sprintf("[live] abundance data: unmapped spp_name '%s'", spp_name))
    return(NULL)
  }
  weeks <- unique(as.integer(weeks))
  weeks <- weeks[!is.na(weeks)]
  if (length(weeks) == 0) return(NULL)

  key <- paste0("live_abund_data:", paste(sort(weeks), collapse = ","), ":", spp_name)
  .cache_live_abundance(key, function() {
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    on.exit(safe_disconnect(con))

    mc <- sprintf("CASE WHEN s.%s IS NULL AND s.zero_count = 't' THEN 0 ELSE s.%s END",
                  cnt_col, cnt_col)
    week_list <- paste(weeks, collapse = ", ")

    q <- sprintf(
      "SELECT CASE WHEN left(s.loc_code,1) = 'X' OR s.sitecode IS NULL THEN 'Out'
                   ELSE v.viareaa END                       AS viarea,
              s.loc_code,
              s.inspdate,
              EXTRACT(year FROM s.inspdate)::numeric         AS year,
              calc_week_num(s.inspdate)::numeric             AS yrwk,
              SUBSTR(calc_week_num(s.inspdate)::text, 5, 2)  AS epiweek,
              '%s'::text                                     AS spp_name,
              (%s)::numeric                                  AS mosqcount
       %s
         AND calc_week_num(s.inspdate) IN (%s)
       ORDER BY yrwk, viarea, loc_code",
      spp_name, mc, .LIVE_ABUNDANCE_BASE, week_list
    )

    tryCatch({
      d <- dbGetQuery(con, q)
      if (is.null(d) || nrow(d) == 0) return(NULL)
      message(sprintf("[live] Abundance data: %d rows for weeks %s, %s (computed from source view)",
                      nrow(d), week_list, spp_name))
      d
    }, error = function(e) {
      warning(paste("[live] abundance data failed:", e$message)); NULL
    })
  })
}

# =============================================================================
# WEEKS WITH REAL ABUNDANCE (live source) — drives current-season trend rebuilds
# =============================================================================
# Distinct yrwk values for `year` that have ACTUAL abundance counts in the live
# source view (cxvectotal set, or a confirmed zero-count sample), optionally
# on/before up_to_date. Used so trend rebuilds iterate only weeks that have data
# — not the genuinely-empty future weeks the materialized view also carries.
weeks_with_abundance_live <- function(year, up_to_date = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(integer(0))
  on.exit(safe_disconnect(con))

  date_clause <- if (!is.null(up_to_date)) {
    sprintf("AND s.inspdate <= '%s'::date", as.character(up_to_date))
  } else ""

  q <- sprintf(
    "SELECT DISTINCT calc_week_num(s.inspdate) AS yrwk
     FROM dbadult_insp_w_id_ff3s_allyears s
     WHERE s.network_type = 'mnt' AND s.survtype = '6' AND s.missing IS NULL
       AND (s.cxvectotal IS NOT NULL OR s.zero_count = 't')
       AND EXTRACT(year FROM s.inspdate) = %d %s
     ORDER BY yrwk",
    as.integer(year), date_clause
  )

  tryCatch({
    d <- dbGetQuery(con, q)
    if (is.null(d) || nrow(d) == 0) integer(0) else as.integer(d$yrwk)
  }, error = function(e) {
    warning(paste("[live] weeks_with_abundance failed:", e$message)); integer(0)
  })
}
