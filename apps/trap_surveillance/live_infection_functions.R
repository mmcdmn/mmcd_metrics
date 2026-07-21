# =============================================================================
# LIVE INFECTION-RATE FALLBACK — Trap Surveillance
# =============================================================================
# Computes MLE / MIR infection rates ON READ, straight from the raw virus-pool
# tables, for weeks that are MISSING from the pre-calculated static tables
# (dbvirus_mle_yrwk_area, dbvirus_mir_yrwk_area, ...). Those tables were a
# one-time Feb-2026 upload and are not refreshed, so the current season has no
# rows and Vector Index (N x P) collapses to 0.
#
# This module NEVER writes to the database. Results are cached in Redis
# (on-read) so the expensive MLE computation runs at most once per day per
# (week x species). If Redis is unavailable it simply recomputes.
#
# Methodology is byte-for-byte reproducible against the static tables (verified):
#   - Pool set: dbadult_insp_current UNION archive -> dbvirus_pool ->
#     dbvirus_pool_test (target='WNV'); survtype IN (4,5,6); network 'mnt'/NULL;
#     missing IS NULL; week via calc_week_num(inspdate); viarea via
#     loc_vectorindexareas_sections_a on LEFT(sitecode,7), NULL -> 'Out'.
#   - MLE: PooledInfRate::pooledBin(x, m, pt.method='firth', scale=1)  (0-1 scale)
#   - MIR: positive / mosquitoes * 1000
#
# Return-column shapes intentionally MATCH the fetch_* functions in
# data_functions.R so these are drop-in fallbacks.
# =============================================================================

# --- Redis on-read cache wrapper (graceful if Redis / helpers absent) ---------
.LIVE_INFECTION_TTL <- 86400L  # 24h — current-season pools change at most daily

.cache_live_infection <- function(key, load_func) {
  if (exists("get_app_cached_redis", mode = "function")) {
    return(get_app_cached_redis(key, load_func, ttl = .LIVE_INFECTION_TTL))
  }
  load_func()
}

# --- Shared pool-set filters (identical to the static-table methodology) ------
# Returns a WHERE fragment (without the week filter, which callers add).
.live_pool_filters <- function(spp_code = NULL) {
  spp_clause <- if (!is.null(spp_code) && nzchar(as.character(spp_code))) {
    sprintf("AND p.spp_code = '%s'", as.character(spp_code))
  } else ""
  paste0(
    "t.target = 'WNV'
       AND i.survtype IN ('4','5','6')
       AND (i.network_type = 'mnt' OR i.network_type IS NULL)
       AND i.missing IS NULL
       ", spp_clause
  )
}

# UNION of current + archive adult inspections (pool data spans both).
.LIVE_ALL_INSP <- "
  (SELECT sampnum_yr, sitecode, network_type, survtype, missing, inspdate
     FROM dbadult_insp_current
   UNION ALL
   SELECT sampnum_yr, sitecode, network_type, survtype, missing, inspdate
     FROM dbadult_insp_archive) i"

# =============================================================================
# MIR — pure SQL aggregation (no external package)
# =============================================================================

# MIR by area for one week. spp_code = NULL -> all Culex (matches
# fetch_mir_by_area); a code -> species-specific (matches fetch_mir_by_area_spp).
compute_mir_by_area_live <- function(yrwk, spp_code = NULL) {
  key <- paste0("live_mir_area:", yrwk, ":", spp_code %||% "all")
  .cache_live_infection(key, function() {
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    on.exit(safe_disconnect(con))

    q <- sprintf(
      "SELECT COALESCE(v.viareaa, 'Out') AS viarea,
              SUM(CASE WHEN t.result = 'Pos' THEN 1 ELSE 0 END)::integer AS positive,
              COUNT(*)::integer        AS total_pools,
              SUM(p.count)::integer    AS total_mosquitoes
       FROM %s
       JOIN dbvirus_pool p       ON i.sampnum_yr = p.sampnum_yr
       JOIN dbvirus_pool_test t  ON t.poolnum   = p.poolnum
       LEFT JOIN loc_vectorindexareas_sections_a v ON v.sectcode = LEFT(i.sitecode, 7)
       WHERE %s
         AND calc_week_num(i.inspdate) = %d
       GROUP BY COALESCE(v.viareaa, 'Out')
       ORDER BY viarea",
      .LIVE_ALL_INSP, .live_pool_filters(spp_code), as.integer(yrwk)
    )

    tryCatch({
      d <- dbGetQuery(con, q)
      if (is.null(d) || nrow(d) == 0) return(NULL)
      d$mir  <- ifelse(d$total_mosquitoes > 0,
                       d$positive / d$total_mosquitoes * 1000, 0)
      d$year <- substr(as.character(yrwk), 1, 4)
      d$yrwk <- as.character(yrwk)
      d$mir_id <- paste0("live-", yrwk, "-", d$viarea)
      if (!is.null(spp_code)) d$spp_code <- as.character(spp_code)
      message(sprintf("[live] MIR by area: %d areas for yrwk %s%s (computed)",
                      nrow(d), yrwk,
                      if (!is.null(spp_code)) paste0(", spp ", spp_code) else ""))
      d
    }, error = function(e) {
      warning(paste("[live] MIR by area failed:", e$message)); NULL
    })
  })
}

# =============================================================================
# MLE — pooledBin per area (requires PooledInfRate)
# =============================================================================

# Pull pool-level rows (viarea, pool_size, x=Pos flag) for one week, then run
# the Firth-corrected pooled MLE per area. Matches fetch_mle_by_area /
# fetch_mle_by_area_spp column shapes.
compute_mle_by_area_live <- function(yrwk, spp_code = NULL) {
  key <- paste0("live_mle_area:", yrwk, ":", spp_code %||% "all")
  .cache_live_infection(key, function() {
    if (!requireNamespace("PooledInfRate", quietly = TRUE)) {
      warning("[live] PooledInfRate not installed - cannot compute MLE fallback")
      return(NULL)
    }
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    on.exit(safe_disconnect(con))

    q <- sprintf(
      "SELECT COALESCE(v.viareaa, 'Out') AS viarea,
              p.count AS pool_size,
              CASE WHEN t.result = 'Pos' THEN 1 ELSE 0 END AS x
       FROM %s
       JOIN dbvirus_pool p       ON i.sampnum_yr = p.sampnum_yr
       JOIN dbvirus_pool_test t  ON t.poolnum   = p.poolnum
       LEFT JOIN loc_vectorindexareas_sections_a v ON v.sectcode = LEFT(i.sitecode, 7)
       WHERE %s
         AND calc_week_num(i.inspdate) = %d",
      .LIVE_ALL_INSP, .live_pool_filters(spp_code), as.integer(yrwk)
    )

    tryCatch({
      pools <- dbGetQuery(con, q)
      if (is.null(pools) || nrow(pools) == 0) return(NULL)

      d <- .pooledbin_by_group(pools, "viarea")
      if (is.null(d) || nrow(d) == 0) return(NULL)

      d$yrwk   <- as.character(yrwk)
      d$mle_id <- paste0("live-", yrwk, "-", d$viarea)
      if (!is.null(spp_code)) d$spp_code <- as.character(spp_code)
      # Column order to mirror the static-table SELECT
      cols <- c("mle_id", "yrwk", "viarea",
                if (!is.null(spp_code)) "spp_code",
                "mle", "mle_lower", "mle_upper")
      d <- d[, cols, drop = FALSE]
      message(sprintf("[live] MLE by area: %d areas for yrwk %s%s (computed)",
                      nrow(d), yrwk,
                      if (!is.null(spp_code)) paste0(", spp ", spp_code) else ""))
      d
    }, error = function(e) {
      warning(paste("[live] MLE by area failed:", e$message)); NULL
    })
  })
}

# Run pooledBin(x, m) per group. `pools` needs columns: <group_col>, pool_size, x.
# Returns data.frame(<group_col>, mle, mle_lower, mle_upper).
.pooledbin_by_group <- function(pools, group_col) {
  groups <- unique(pools[[group_col]])
  rows <- lapply(groups, function(g) {
    d <- pools[pools[[group_col]] == g, ]
    valid <- !is.na(d$pool_size) & d$pool_size > 0 & !is.na(d$x)
    if (sum(valid) == 0) return(NULL)
    r <- tryCatch(
      PooledInfRate::pooledBin(x = d$x[valid], m = d$pool_size[valid],
                               pt.method = "firth", scale = 1),
      error = function(e) NULL
    )
    if (is.null(r)) return(NULL)
    out <- data.frame(
      grp       = g,
      mle       = as.numeric(r$P),
      mle_lower = as.numeric(r$Lower),
      mle_upper = as.numeric(r$Upper),
      stringsAsFactors = FALSE
    )
    names(out)[1] <- group_col
    out
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

# =============================================================================
# DISTRICT-WIDE TRENDS — one row per yrwk for the whole year
# =============================================================================

# District-wide MLE per week (matches fetch_mle_trend: mle_id, yrwk, mle,
# mle_lower, mle_upper). One DB pull for the year, pooledBin per yrwk.
compute_mle_trend_live <- function(year) {
  key <- paste0("live_mle_trend:", year)
  .cache_live_infection(key, function() {
    if (!requireNamespace("PooledInfRate", quietly = TRUE)) {
      warning("[live] PooledInfRate not installed - cannot compute MLE trend fallback")
      return(NULL)
    }
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    on.exit(safe_disconnect(con))

    q <- sprintf(
      "SELECT calc_week_num(i.inspdate) AS yrwk,
              p.count AS pool_size,
              CASE WHEN t.result = 'Pos' THEN 1 ELSE 0 END AS x
       FROM %s
       JOIN dbvirus_pool p       ON i.sampnum_yr = p.sampnum_yr
       JOIN dbvirus_pool_test t  ON t.poolnum   = p.poolnum
       WHERE %s
         AND calc_week_num(i.inspdate) >= %d
         AND calc_week_num(i.inspdate) <  %d",
      .LIVE_ALL_INSP, .live_pool_filters(NULL),
      as.integer(year) * 100L, (as.integer(year) + 1L) * 100L
    )

    tryCatch({
      pools <- dbGetQuery(con, q)
      if (is.null(pools) || nrow(pools) == 0) return(NULL)
      pools$yrwk <- as.integer(pools$yrwk)

      d <- .pooledbin_by_group(pools, "yrwk")
      if (is.null(d) || nrow(d) == 0) return(NULL)
      d <- d[order(d$yrwk), ]
      d$mle_id <- paste0("live-", d$yrwk)
      d <- d[, c("mle_id", "yrwk", "mle", "mle_lower", "mle_upper")]
      message(sprintf("[live] MLE trend: %d weeks for %s (computed)", nrow(d), year))
      d
    }, error = function(e) {
      warning(paste("[live] MLE trend failed:", e$message)); NULL
    })
  })
}

# District-wide MIR per week (matches fetch_mir_trend: mir_id, yrwk, positive,
# total_pools, total_mosquitoes, mir, mir_se).
compute_mir_trend_live <- function(year) {
  key <- paste0("live_mir_trend:", year)
  .cache_live_infection(key, function() {
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    on.exit(safe_disconnect(con))

    q <- sprintf(
      "SELECT calc_week_num(i.inspdate) AS yrwk,
              SUM(CASE WHEN t.result = 'Pos' THEN 1 ELSE 0 END)::integer AS positive,
              COUNT(*)::integer     AS total_pools,
              SUM(p.count)::integer AS total_mosquitoes
       FROM %s
       JOIN dbvirus_pool p       ON i.sampnum_yr = p.sampnum_yr
       JOIN dbvirus_pool_test t  ON t.poolnum   = p.poolnum
       WHERE %s
         AND calc_week_num(i.inspdate) >= %d
         AND calc_week_num(i.inspdate) <  %d
       GROUP BY calc_week_num(i.inspdate)
       ORDER BY yrwk",
      .LIVE_ALL_INSP, .live_pool_filters(NULL),
      as.integer(year) * 100L, (as.integer(year) + 1L) * 100L
    )

    tryCatch({
      d <- dbGetQuery(con, q)
      if (is.null(d) || nrow(d) == 0) return(NULL)
      d$yrwk <- as.integer(d$yrwk)
      d$mir  <- ifelse(d$total_mosquitoes > 0,
                       d$positive / d$total_mosquitoes * 1000, 0)
      # Binomial SE (same formula fetch_mir_trend computes in R)
      p_hat  <- ifelse(d$total_mosquitoes > 0, d$positive / d$total_mosquitoes, 0)
      d$mir_se <- ifelse(d$total_mosquitoes > 0,
                         sqrt(p_hat * (1 - p_hat) / d$total_mosquitoes) * 1000, 0)
      d$mir_id <- paste0("live-", d$yrwk)
      d <- d[, c("mir_id", "yrwk", "positive", "total_pools",
                 "total_mosquitoes", "mir", "mir_se")]
      message(sprintf("[live] MIR trend: %d weeks for %s (computed)", nrow(d), year))
      d
    }, error = function(e) {
      warning(paste("[live] MIR trend failed:", e$message)); NULL
    })
  })
}
