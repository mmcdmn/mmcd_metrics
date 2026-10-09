# =============================================================================
# MMCD METRICS - SHARED EMBED HELPERS
# =============================================================================
# Builds small, stable, embed-ready payloads for the public drop-in endpoints
# (/v1/public/embed/*) and the static widget page (apps/embed/).
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

# =============================================================================
# EMBED FRAMEWORK -- full filter/group_by/graph parity with the URL handler.
# -----------------------------------------------------------------------------
# =============================================================================

CACHE_PREFIX_EMBED <- "embed"   # embed  per-filter results (short TTL)

#' Coerce/validate one raw query value by declared type.
#' Types: enum, int, num, logical, date, daterange, csv, string.
.embed_coerce <- function(raw, type, allowed = NULL, default = NULL) {
  if (is.null(raw) || length(raw) == 0 || !nzchar(as.character(raw)[1])) return(default)
  v <- as.character(raw)[1]
  out <- switch(
    type,
    int     = suppressWarnings(as.integer(v)),
    num     = suppressWarnings(as.numeric(v)),
    logical = tolower(v) %in% c("true", "1", "yes", "on"),
    date    = suppressWarnings(as.Date(v)),
    daterange = {
      parts <- trimws(strsplit(v, ",")[[1]])
      d <- suppressWarnings(as.Date(parts))
      if (all(is.na(d))) NULL else d
    },
    csv     = trimws(strsplit(v, ",")[[1]]),
    # enum + string: keep as-is
    v
  )
  if (length(out) == 1 && is.na(out)) return(default)
  # enum validation (single or csv): drop values not in `allowed`
  if (!is.null(allowed) && type %in% c("enum", "csv", "string")) {
    keep <- out[out %in% allowed]
    if (length(keep) == 0) return(default)
    out <- keep
  }
  out
}

#' Parse + validate an app's embed params from a raw query list.
#' Reads EMBED_PARAM_SPEC[[app]] (from shared/embed_producers.R).
#' @return named list of normalized params, always including `$view`.
#' @export
parse_embed_params <- function(app, query) {
  spec <- .embed_spec_for(app)
  if (is.null(spec)) return(NULL)
  # Resolve the view (tab) first.
  view <- .embed_coerce(query[["view"]], "enum",
                        allowed = spec$views, default = spec$default_view)
  p <- list(view = view)
  for (nm in names(spec$params)) {
    ps <- spec$params[[nm]]
    p[[nm]] <- .embed_coerce(query[[nm]], ps$type %||% "string",
                             allowed = ps$allowed, default = ps$default)
  }
  p
}

#' Look up an app's param spec (defined in shared/embed_producers.R).
.embed_spec_for <- function(app) {
  if (exists("EMBED_PARAM_SPEC", mode = "list")) {
    sp <- get("EMBED_PARAM_SPEC")
    if (!is.null(sp[[app]])) return(sp[[app]])
  }
  NULL
}

#' Look up an app's producer for a view (defined in shared/embed_producers.R).
.embed_producer_for <- function(app, view) {
  if (!exists("EMBED_PRODUCERS", mode = "list")) return(NULL)
  reg <- get("EMBED_PRODUCERS")
  app_reg <- reg[[app]]
  if (is.null(app_reg)) return(NULL)
  fn <- app_reg[[view]]
  if (is.null(fn)) fn <- app_reg[["default"]]
  fn
}

#' Deterministic cache key from app + view + sorted params.
#' @export
embed_cache_key <- function(app, view, params) {
  # sort params by name so query-string order never changes the key
  p <- params[order(names(params))]
  build_cache_key(CACHE_PREFIX_EMBED, app, view, p)
}

.embed_format_x <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  as.character(x)
}

#' Turn a producer descriptor into JSON-ready grouped series.
#' descriptor: list(df, x_col, y_col, group_col=NULL, x_label, y_label, resolved_options)
#' One series per unique value of group_col (fallback: zone, then a single series).
#' @export
normalize_series <- function(descriptor) {
  df <- descriptor$df
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(list())
  xcol <- descriptor$x_col; ycol <- descriptor$y_col
  if (is.null(xcol) || !(xcol %in% names(df))) xcol <- .embed_pick_col(df, .EMBED_X_CANDIDATES)
  if (is.null(ycol) || !(ycol %in% names(df))) ycol <- .embed_pick_col(df, .EMBED_Y_CANDIDATES)
  if (is.null(xcol) || is.null(ycol)) return(list())
  gcol <- descriptor$group_col
  if (!is.null(gcol) && !(gcol %in% names(df))) gcol <- NULL
  if (is.null(gcol) && "zone" %in% names(df)) gcol <- "zone"

  make_points <- function(sx, sy) lapply(seq_along(sy), function(i) {
    list(x = jsonlite::unbox(.embed_format_x(sx[i])),
         y = jsonlite::unbox(if (is.na(sy[i])) NA else as.numeric(sy[i])))
  })

  if (is.null(gcol)) {
    return(list(list(name = jsonlite::unbox(descriptor$y_label %||% ycol),
                     points = make_points(df[[xcol]], df[[ycol]]))))
  }
  groups <- unique(df[[gcol]])
  lapply(groups, function(g) {
    sel <- df[[gcol]] == g
    list(name = jsonlite::unbox(as.character(g)),
         points = make_points(df[[xcol]][sel], df[[ycol]][sel]))
  })
}

#' Wrap resolved params as an unboxed JSON object (scalars stay scalars).
.embed_unbox_options <- function(opts) {
  if (is.null(opts) || length(opts) == 0) return(list())
  lapply(opts, function(v) {
    if (length(v) == 1) jsonlite::unbox(as.character(v)) else as.character(v)
  })
}

# -----------------------------------------------------------------------------
# Non-series payload kinds (chosen: extend the payload per type).
# A producer sets descriptor$kind to "map" | "boxplot" | "table" (default
# "series"); get_embed_result dispatches to the matching normalizer below.
# -----------------------------------------------------------------------------

EMBED_MAX_POINTS <- 5000L   # cap map points per payload
EMBED_MAX_ROWS   <- 1000L   # cap table rows per payload

#' Map payload: [{lat, lon, value?, label?}] from lat/lon (+ optional value/label) cols.
#' @export
normalize_map <- function(descriptor) {
  df <- descriptor$df
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(list())
  latc <- descriptor$lat_col; lonc <- descriptor$lon_col
  if (is.null(latc) || is.null(lonc) || !all(c(latc, lonc) %in% names(df))) return(list())
  lat <- suppressWarnings(as.numeric(df[[latc]]))
  lon <- suppressWarnings(as.numeric(df[[lonc]]))
  ok <- !is.na(lat) & !is.na(lon)
  df <- df[ok, , drop = FALSE]; lat <- lat[ok]; lon <- lon[ok]
  n <- min(nrow(df), EMBED_MAX_POINTS)
  vc <- descriptor$value_col; lc <- descriptor$label_col
  sc <- descriptor$size_col; cc <- descriptor$category_col
  pc <- descriptor$popup_col
  lapply(seq_len(n), function(i) {
    pt <- list(lat = jsonlite::unbox(lat[i]), lon = jsonlite::unbox(lon[i]))
    if (!is.null(vc) && vc %in% names(df)) {
      v <- df[[vc]][i]
      pt$value <- jsonlite::unbox(if (is.na(v)) NA else if (is.numeric(v)) as.numeric(v) else as.character(v))
    }
    if (!is.null(sc) && sc %in% names(df)) {
      s <- suppressWarnings(as.numeric(df[[sc]][i]))
      pt$size <- jsonlite::unbox(if (is.na(s)) NA else s)
    }
    if (!is.null(cc) && cc %in% names(df)) pt$category <- jsonlite::unbox(as.character(df[[cc]][i]))
    if (!is.null(lc) && lc %in% names(df)) pt$label <- jsonlite::unbox(as.character(df[[lc]][i]))
    if (!is.null(pc) && pc %in% names(df)) pt$popup <- jsonlite::unbox(as.character(df[[pc]][i]))
    pt
  })
}

#' Boxplot payload: [{name, min, q1, median, q3, max, n}] per group.
#' @export
normalize_boxplot <- function(descriptor) {
  df <- descriptor$df
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(list())
  vcol <- descriptor$value_col
  if (is.null(vcol) || !(vcol %in% names(df))) return(list())
  gcol <- descriptor$group_col
  if (!is.null(gcol) && !(gcol %in% names(df))) gcol <- NULL
  groups <- if (is.null(gcol)) list(list(name = descriptor$y_label %||% vcol, sel = rep(TRUE, nrow(df))))
            else lapply(unique(df[[gcol]]), function(g) list(name = as.character(g), sel = df[[gcol]] == g))
  out <- lapply(groups, function(grp) {
    v <- suppressWarnings(as.numeric(df[[vcol]][grp$sel]))
    v <- v[!is.na(v)]
    if (length(v) == 0) return(NULL)
    q <- as.numeric(stats::quantile(v, c(0, .25, .5, .75, 1), names = FALSE, type = 7))
    list(name = jsonlite::unbox(as.character(grp$name)),
         min = jsonlite::unbox(q[1]), q1 = jsonlite::unbox(q[2]),
         median = jsonlite::unbox(q[3]), q3 = jsonlite::unbox(q[4]),
         max = jsonlite::unbox(q[5]), n = jsonlite::unbox(length(v)))
  })
  out[!vapply(out, is.null, logical(1))]
}

#' Table payload: list(columns=[...], rows=[{col: val, ...}]).
#' @export
normalize_table <- function(descriptor) {
  df <- descriptor$df
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(list(columns = list(), rows = list()))
  cols <- descriptor$columns
  if (is.null(cols)) cols <- names(df)
  cols <- cols[cols %in% names(df)]
  cap <- descriptor$max_rows %||% EMBED_MAX_ROWS
  n <- min(nrow(df), cap)
  rows <- lapply(seq_len(n), function(i) {
    r <- lapply(cols, function(cn) {
      v <- df[[cn]][i]
      jsonlite::unbox(if (length(v) == 0 || is.na(v)) NA
                      else if (is.numeric(v)) as.numeric(v) else as.character(v))
    })
    names(r) <- cols
    r
  })
  list(columns = as.list(cols), rows = rows)
}

#' Choropleth payload helper: parallel per-area [{id, value, label}]. The
#' GeoJSON FeatureCollection and the binned color scale are attached separately
#' in .embed_build_payload (geojson passes through as-is; scale is in options).
#' @export
normalize_choropleth <- function(descriptor) {
  areas <- descriptor$areas
  if (is.null(areas) || length(areas) == 0) return(list())
  lapply(areas, function(a) {
    out <- list(id = jsonlite::unbox(as.character(a$id)))
    v <- a$value
    out$value <- jsonlite::unbox(if (is.null(v) || (length(v) == 1 && is.na(v))) NA
                                 else if (is.numeric(v)) as.numeric(v) else as.character(v))
    if (!is.null(a$label)) out$label <- jsonlite::unbox(as.character(a$label))
    out
  })
}

#' Binned color scale for a choropleth: breaks/colors stay arrays, na_color and
#' legend_max unbox to scalars. Returns NULL if no scale was supplied.
.embed_choropleth_scale <- function(cs) {
  if (is.null(cs)) return(NULL)
  list(
    breaks     = as.numeric(cs$breaks),
    colors     = as.character(cs$colors),
    na_color   = jsonlite::unbox(as.character(cs$na_color %||% "#C0C0C0")),
    legend_max = jsonlite::unbox(as.numeric(cs$legend_max %||% max(cs$breaks)))
  )
}

#' Build the JSON payload for a descriptor, dispatching on descriptor$kind.
#' Returns the payload WITHOUT the per-response `source` tag.
.embed_build_payload <- function(app, view, desc, err = NULL) {
  kind <- desc$kind %||% "series"
  base <- list(
    app          = jsonlite::unbox(app),
    view         = jsonlite::unbox(view %||% ""),
    payload_type = jsonlite::unbox(kind),
    resolved_options = .embed_unbox_options(desc$resolved_options),
    x_label      = jsonlite::unbox(desc$x_label %||% ""),
    y_label      = jsonlite::unbox(desc$y_label %||% "Value"),
    updated_at   = jsonlite::unbox(format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  )
  empty <- !is.null(err) || is.null(desc$df) || !is.data.frame(desc$df) || nrow(desc$df) == 0
  if (empty) {
    payload <- c(base, list(status = jsonlite::unbox("unavailable")),
                 switch(kind,
                        map        = list(points = list()),
                        boxplot    = list(boxes = list()),
                        table      = list(columns = list(), rows = list()),
                        choropleth = list(geojson = NULL, areas = list(),
                                          feature_id_key = jsonlite::unbox(desc$feature_id_key %||% "properties.id"),
                                          color_scale = .embed_choropleth_scale(desc$color_scale)),
                        list(series = list())))
    if (!is.null(err)) payload$note <- jsonlite::unbox(err)
    return(payload)
  }
  data_parts <- switch(kind,
    map        = list(points = normalize_map(desc)),
    boxplot    = list(boxes = normalize_boxplot(desc)),
    table      = normalize_table(desc),
    choropleth = list(geojson = desc$geojson, areas = normalize_choropleth(desc),
                      feature_id_key = jsonlite::unbox(desc$feature_id_key %||% "properties.id"),
                      color_scale = .embed_choropleth_scale(desc$color_scale)),
    list(series = normalize_series(desc)))
  c(base, list(status = jsonlite::unbox("ok")), data_parts)
}

#' MAIN v2 entry: peek cache -> compute via producer -> cache -> return payload.
#' @param app   app id (e.g. "suco_history")
#' @param query raw named list from the request query string
#' @return JSON-ready list (auto_unbox serializer); status ok|unavailable|error
#' @export
get_embed_result <- function(app, query) {
  p <- parse_embed_params(app, query)
  if (is.null(p)) {
    return(list(status = jsonlite::unbox("error"),
                error = jsonlite::unbox(sprintf("unknown or unsupported app '%s'", app))))
  }
  view <- p$view
  key  <- tryCatch(embed_cache_key(app, view, p), error = function(e) NULL)

  # 1. cache peek
  if (!is.null(key)) {
    cached <- tryCatch(redis_get(key), error = function(e) NULL)
    if (!is.null(cached) && is.list(cached)) {
      cached$source <- jsonlite::unbox("cache")
      return(cached)
    }
  }

  # 2. compute live via the app's producer
  producer <- .embed_producer_for(app, view)
  if (is.null(producer)) {
    return(list(status = jsonlite::unbox("error"), app = jsonlite::unbox(app),
                view = jsonlite::unbox(view %||% ""),
                error = jsonlite::unbox(sprintf("no embed view '%s' for app '%s'", view, app))))
  }
  desc <- tryCatch(producer(p), error = function(e) {
    structure(list(), embed_error = conditionMessage(e))
  })
  if (is.null(desc)) desc <- structure(list(), embed_error = "producer returned NULL")
  err <- attr(desc, "embed_error")
  payload <- .embed_build_payload(app, view, desc, err)

  # 3. cache only successful payloads (without the per-response `source` tag)
  if (identical(as.character(payload$status), "ok") && !is.null(key)) {
    tryCatch(redis_set(key, payload, ttl = if (exists("TTL_5_MIN")) TTL_5_MIN else 300L),
             error = function(e) NULL)
  }
  c(payload, list(source = jsonlite::unbox("live")))
}
