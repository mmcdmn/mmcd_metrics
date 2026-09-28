# =============================================================================
# MMCD METRICS - SHARED EMBED HELPERS
# =============================================================================
# Builds small, stable, embed-ready payloads for the public drop-in endpoints
# (/v1/public/embed/*) and the static widget page (apps/embed/).
#
# HARD RULE: these helpers are CACHE-ONLY. They read the already-warm
# `historical_averages` Redis hash (kept fresh for every historical_enabled
# metric by regenerate_cache(), 14-day TTL) via get_cached_average_redis(), and
# they NEVER trigger a live DB load. A public embed must never make a viewer --
# or the database -- wait. On a cache miss the payload comes back status
# "unavailable" so the widget can show a gentle "refreshing" message instead.
#
# Depends on (sourced by the caller, e.g. api/plumber.R):
#   - shared/redis_cache.R   (get_cached_average_redis, redis_get, HISTORICAL_META_KEY)
#   - apps/overview/metric_registry.R (get_metric_config, get_historical_metrics)
# =============================================================================

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Candidate column names, in preference order, for the x (time) axis and the
# y (value) axis of a cached averages data.frame. We introspect rather than
# assume, so a schema tweak upstream doesn't silently break embeds.
.EMBED_X_CANDIDATES <- c("week", "analysis_week", "period", "year", "date",
                         "inspection_year", "label", "x")
.EMBED_Y_CANDIDATES <- c("value", "avg_value", "average", "count", "acres", "y")

#' Which metrics can be embedded (those with a warm historical series).
#' @return character vector of metric ids
#' @export
embeddable_metrics <- function() {
  if (exists("get_historical_metrics", mode = "function")) {
    return(get_historical_metrics())
  }
  character(0)
}

.embed_pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) > 0) return(hit[1])
  NULL
}

.embed_updated_at <- function() {
  if (!exists("HISTORICAL_META_KEY")) return(NA_character_)
  meta <- tryCatch(redis_get(HISTORICAL_META_KEY), error = function(e) NULL)
  if (is.null(meta) || is.null(meta$generated_date)) return(NA_character_)
  as.character(meta$generated_date)
}

#' Normalize a cached averages data.frame into series of {x, y} points.
#' Splits into one series per `zone` when that column is present.
.embed_series_from_df <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(list())
  ycol <- .embed_pick_col(df, .EMBED_Y_CANDIDATES)
  if (is.null(ycol)) {
    # fall back to the first numeric column
    num <- names(df)[vapply(df, is.numeric, logical(1))]
    if (length(num) == 0) return(list())
    ycol <- num[1]
  }
  xcol <- .embed_pick_col(df, .EMBED_X_CANDIDATES)
  xvals <- if (is.null(xcol)) seq_len(nrow(df)) else df[[xcol]]

  make_points <- function(sub_x, sub_y) {
    lapply(seq_along(sub_y), function(i) {
      list(x = jsonlite::unbox(as.character(sub_x[i])),
           y = jsonlite::unbox(if (is.na(sub_y[i])) NA else as.numeric(sub_y[i])))
    })
  }

  if ("zone" %in% names(df)) {
    zones <- unique(df$zone)
    lapply(zones, function(z) {
      sel <- df$zone == z
      list(name = jsonlite::unbox(paste0("Zone ", z)),
           points = make_points(xvals[sel], df[[ycol]][sel]))
    })
  } else {
    list(list(name = jsonlite::unbox(ycol),
              points = make_points(xvals, df[[ycol]])))
  }
}

#' Build the embed CHART payload for a metric (cache-only).
#'
#' @param metric  registry metric id (e.g. "ground_prehatch")
#' @param avg_type "5yr","10yr","yearly_district","yearly_facilities"
#' @return list ready to be JSON-serialized (use auto_unbox=TRUE serializer)
#' @export
get_embed_chart_payload <- function(metric, avg_type = "10yr") {
  config <- if (exists("get_metric_config", mode = "function")) get_metric_config(metric) else NULL
  if (is.null(config)) {
    return(list(status = jsonlite::unbox("error"),
                error = jsonlite::unbox(sprintf("unknown metric '%s'", metric))))
  }
  df <- tryCatch(get_cached_average_redis(metric, avg_type), error = function(e) NULL)
  base <- list(
    metric       = jsonlite::unbox(metric),
    display_name = jsonlite::unbox(trimws(config$display_name %||% metric)),
    y_label      = jsonlite::unbox(config$y_label %||% "Value"),
    avg_type     = jsonlite::unbox(avg_type),
    updated_at   = jsonlite::unbox(.embed_updated_at()),
    source       = jsonlite::unbox("historical_averages_cache")
  )
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(c(base, list(status = jsonlite::unbox("unavailable"), series = list())))
  }
  c(base, list(status = jsonlite::unbox("ok"), series = .embed_series_from_df(df)))
}

#' Build the embed STATBOX payload for a metric (cache-only, infra scaffold).
#' Returns the metric's most recent cached value as a single tile.
#' @export
get_embed_statbox_payload <- function(metric, avg_type = "10yr") {
  chart <- get_embed_chart_payload(metric, avg_type)
  if (!identical(as.character(chart$status), "ok") || length(chart$series) == 0) {
    return(list(status = chart$status,
                metric = chart$metric,
                display_name = chart$display_name %||% jsonlite::unbox(metric)))
  }
  first_series <- chart$series[[1]]
  pts <- first_series$points
  last_val <- if (length(pts) > 0) pts[[length(pts)]]$y else jsonlite::unbox(NA)
  list(
    status       = jsonlite::unbox("ok"),
    metric       = chart$metric,
    display_name = chart$display_name,
    label        = chart$y_label,
    value        = last_val,
    updated_at   = chart$updated_at,
    source       = jsonlite::unbox("historical_averages_cache")
  )
}

#' List embeddable metrics with their display metadata (for consumers/widget).
#' @export
get_embed_metric_list <- function() {
  ids <- embeddable_metrics()
  lapply(ids, function(id) {
    cfg <- get_metric_config(id)
    list(
      metric       = jsonlite::unbox(id),
      display_name = jsonlite::unbox(trimws(cfg$display_name %||% id)),
      y_label      = jsonlite::unbox(cfg$y_label %||% "Value"),
      category     = jsonlite::unbox(cfg$category %||% "")
    )
  })
}
