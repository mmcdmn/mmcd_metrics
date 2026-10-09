# =============================================================================
# MMCD METRICS - EMBED PRODUCERS (v2 full-parity drop-in embeds)
# =============================================================================
# One "producer" per app + view. Each producer REPRODUCES that app's own chart
# data by calling the app's own data-layer functions in the same order the app's
# server does, then returns a small descriptor that shared/embed_helpers.R
# normalizes into JSON series. Running the app's own code is what guarantees the
# embed matches the deep-link URL exactly (same filters, group_by, graph type).
#
# Two lookup tables drive the framework in embed_helpers.R:
#   EMBED_PARAM_SPEC[[app]]  -- which URL params an app accepts (mirrors the
#                               deep-link spec + documents/deep_linking_and_embeds.md)
#   EMBED_PRODUCERS[[app]][[view]] -- function(params) -> descriptor | NULL
#
# A descriptor is list(df, x_col, y_col, group_col=NULL, x_label, y_label,
# resolved_options=list(...)). Return an empty/NULL df for "no data".
#
# Depends (sourced by caller AFTER redis_cache.R + db_helpers.R + app_libraries.R):
#   - shared/db_helpers.R  (get_db_connection, is_valid_filter, lookups) -- these
#     are visible to each app env because we source with parent = globalenv().
# =============================================================================

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -----------------------------------------------------------------------------
# App environment loader (reuses the proven pattern from
# apps/overview/historical_functions.R:240-250). Sources an app's data files
# into an isolated env whose parent is globalenv(), so the shared helpers the app
# relies on (get_db_connection, dplyr, ...) resolve. Cached per app for the life
# of the process -- sourcing only DEFINES functions, it opens no connections.
# -----------------------------------------------------------------------------
.embed_app_envs <- new.env(parent = emptyenv())

.embed_apps_base <- function() {
  # mirror get_apps_base_path() candidates; return the first that exists
  cands <- c(
    "/srv/shiny-server/apps",
    "apps",
    "../apps",
    "../../apps",
    ".."            # when cwd is already an app folder
  )
  for (base in cands) {
    if (dir.exists(file.path(base, "suco_history")) ||
        dir.exists(file.path(base, "drone"))) return(base)
  }
  if (exists("get_apps_base_path", mode = "function")) {
    return(tryCatch(get_apps_base_path(), error = function(e) "apps"))
  }
  "apps"
}

#' Absolute path to an app's folder under apps/ (NULL if it can't be resolved).
.embed_app_dir <- function(app_folder) {
  base <- .embed_apps_base()
  d <- file.path(base, app_folder)
  if (dir.exists(d)) normalizePath(d, winslash = "/", mustWork = FALSE) else NULL
}

#' Run `expr` with the working directory set to the app's folder, restoring it
#' afterward. Needed because some app functions (e.g. drone's
#' create_historical_data) `source('data_functions.R')` by RELATIVE path at call
#' time -- fine in-app (cwd = app dir) but "cannot open the connection" in the
#' plumber process whose cwd is elsewhere.
.embed_in_app_wd <- function(app_folder, expr) {
  d <- .embed_app_dir(app_folder)
  if (is.null(d)) return(force(expr))
  old <- getwd(); on.exit(setwd(old), add = TRUE)
  setwd(d)
  force(expr)
}

#' Load (and cache) an app's data functions into an isolated environment.
#' @param app_folder folder under apps/ (e.g. "suco_history")
#' @param files      R files in that folder to source (default data_functions.R)
#' @return environment with the app's functions, or NULL on failure
load_app_env <- function(app_folder, files = c("data_functions.R")) {
  cache_id <- paste0(app_folder, "::", paste(files, collapse = ","))
  if (!is.null(.embed_app_envs[[cache_id]])) return(.embed_app_envs[[cache_id]])
  base <- .embed_apps_base()
  env <- new.env(parent = globalenv())
  ok <- FALSE
  for (f in files) {
    path <- file.path(base, app_folder, f)
    if (file.exists(path)) {
      tryCatch({ source(path, local = env, chdir = TRUE); ok <- TRUE },
               error = function(e) cat("[embed] source error", path, ":", conditionMessage(e), "\n"))
    }
  }
  if (!ok || length(ls(env)) == 0) return(NULL)
  .embed_app_envs[[cache_id]] <- env
  env
}

#' Descriptor constructors (what a producer returns). Each sets `kind`, which
#' get_embed_result dispatches on. Default kind is "series".
.embed_desc <- function(df, x_col, y_col, group_col = NULL,
                        x_label = "", y_label = "Value", resolved_options = list()) {
  list(kind = "series", df = df, x_col = x_col, y_col = y_col, group_col = group_col,
       x_label = x_label, y_label = y_label, resolved_options = resolved_options)
}

#' Map descriptor: points from lat/lon (+ optional value/size/category/label).
#' `size_col` carries a per-point marker radius (the app's staged size scale);
#' `category_col` carries a per-point category (e.g. treatment status) that the
#' widget colors by, with a legend. `popup_col` carries a pre-built HTML string
#' (same content as the app's own Leaflet popup) shown when a point is clicked.
.embed_desc_map <- function(df, lat_col, lon_col, value_col = NULL, size_col = NULL,
                            category_col = NULL, label_col = NULL, popup_col = NULL,
                            x_label = "", y_label = "", resolved_options = list()) {
  list(kind = "map", df = df, lat_col = lat_col, lon_col = lon_col,
       value_col = value_col, size_col = size_col, category_col = category_col,
       label_col = label_col, popup_col = popup_col, x_label = x_label,
       y_label = y_label, resolved_options = resolved_options)
}

#' Boxplot descriptor: distribution of value_col split by group_col.
.embed_desc_boxplot <- function(df, value_col, group_col = NULL,
                                x_label = "", y_label = "Value", resolved_options = list()) {
  list(kind = "boxplot", df = df, value_col = value_col, group_col = group_col,
       x_label = x_label, y_label = y_label, resolved_options = resolved_options)
}

#' Table descriptor: rows of the given columns (defaults to all df columns).
.embed_desc_table <- function(df, columns = NULL, max_rows = NULL,
                              x_label = "", y_label = "", resolved_options = list()) {
  list(kind = "table", df = df, columns = columns, max_rows = max_rows,
       x_label = x_label, y_label = y_label, resolved_options = resolved_options)
}

#' Choropleth descriptor: filled polygons shaded by a binned value (e.g. the
#' trap VI-area map). `geojson` is a parsed GeoJSON FeatureCollection (one
#' feature per area, `id` = the join key); `areas` is the parallel per-area
#' {id,value,label}. The binned color scale (breaks + colors + na_color) rides
#' in resolved_options so the widget shades exactly as the app's Leaflet does.
#' `df` is the feature attribute table (drives the empty-data check only).
.embed_desc_choropleth <- function(df, geojson, areas, color_scale = NULL,
                                   feature_id_key = "properties.viarea",
                                   x_label = "", y_label = "", resolved_options = list()) {
  list(kind = "choropleth", df = df, geojson = geojson, areas = areas,
       color_scale = color_scale, feature_id_key = feature_id_key,
       x_label = x_label, y_label = y_label, resolved_options = resolved_options)
}

#' Convert an sf object to a parsed GeoJSON FeatureCollection (list), keeping
#' only `keep_cols` as feature properties. Uses GDAL via sf (no extra package):
#' writes a temp .geojson and reads it back parsed so plumber's JSON serializer
#' re-emits it verbatim. Returns NULL on any failure.
.embed_sf_to_geojson <- function(sf_obj, keep_cols = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) return(NULL)
  tryCatch({
    g <- sf_obj
    if (!is.null(keep_cols)) {
      keep <- intersect(keep_cols, names(g))
      g <- g[, keep]  # sf keeps the geometry column automatically
    }
    tf <- tempfile(fileext = ".geojson")
    on.exit(unlink(tf), add = TRUE)
    sf::st_write(g, tf, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
    jsonlite::fromJSON(readLines(tf, warn = FALSE), simplifyVector = FALSE)
  }, error = function(e) NULL)
}

# =============================================================================
# PARAM SPECS -- mirror the deep-link params (documents/deep_linking_and_embeds.md).
# type: enum|int|num|logical|date|daterange|csv|string ; allowed: for enum/csv.
# =============================================================================
EMBED_PARAM_SPEC <- list()
EMBED_PRODUCERS  <- list()

#' Advertise the v2 embed surface (apps -> views + accepted params) for consumers.
#' @export
get_embed_app_catalog <- function() {
  if (!exists("EMBED_PARAM_SPEC", mode = "list")) return(list())
  lapply(names(EMBED_PARAM_SPEC), function(app) {
    sp <- EMBED_PARAM_SPEC[[app]]
    list(
      app          = jsonlite::unbox(app),
      views        = sp$views,
      default_view = jsonlite::unbox(sp$default_view %||% (sp$views[1] %||% "")),
      params = lapply(names(sp$params), function(nm) {
        ps <- sp$params[[nm]]
        entry <- list(name = jsonlite::unbox(nm),
                      type = jsonlite::unbox(ps$type %||% "string"))
        if (!is.null(ps$allowed)) entry$allowed <- ps$allowed
        if (!is.null(ps$default)) entry$default <- jsonlite::unbox(as.character(ps$default)[1])
        entry
      })
    )
  })
}

# ---------------------------------------------------------------------------
# suco_history  (richest bespoke filters; worked example)
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["suco_history"]] <- list(
  views = c("graph", "top_locations", "map", "detailed"),
  default_view = "graph",
  params = list(
    facility           = list(type = "string",  default = "all"),
    zone               = list(type = "enum",    allowed = c("all","1","2"), default = "all"),
    fos                = list(type = "csv",      default = NULL),
    group_by           = list(type = "enum",    allowed = c("mmcd_all","facility","foreman","species_name"), default = "mmcd_all"),
    species            = list(type = "string",  default = "All"),
    graph_type         = list(type = "enum",    allowed = c("stacked_bar","bar","line","point","area"), default = "stacked_bar"),
    top_locations_mode = list(type = "enum",    allowed = c("visits","species"), default = "visits"),
    basemap            = list(type = "enum",    allowed = c("osm","carto","satellite"), default = "osm"),
    color_theme        = list(type = "string",  default = "MMCD"),
    date_range         = list(type = "daterange", default = NULL)
  )
)

.suco_graph_producer <- function(p) {
  env <- load_app_env("suco_history")
  if (is.null(env)) return(NULL)
  gb  <- p$group_by %||% "mmcd_all"
  rsd <- identical(gb, "species_name")
  d   <- env$get_suco_data("all", p$date_range, rsd)
  d   <- env$filter_suco_data(d, p$facility %||% "all", p$fos,
                              p$zone %||% "all", p$date_range, p$species %||% "All")
  df  <- env$aggregate_suco_data(d, gb, p$zone %||% "all")
  # server's effective_graph_type: species_name is always drawn as bars
  gtype <- if (identical(gb, "species_name")) "bar" else (p$graph_type %||% "stacked_bar")
  .embed_desc(df, x_col = "time_group", y_col = "count", group_col = gb,
              x_label = "Week", y_label = "SUCO count",
              resolved_options = list(view = "graph", group_by = gb, graph_type = gtype))
}

.suco_top_locations_producer <- function(p) {
  env <- load_app_env("suco_history")
  if (is.null(env)) return(NULL)
  mode <- p$top_locations_mode %||% "visits"
  d <- env$get_suco_data("all", p$date_range, TRUE)  # needs species detail cols
  d <- env$filter_suco_data(d, p$facility %||% "all", p$fos,
                            p$zone %||% "all", p$date_range, p$species %||% "All")
  tl <- env$get_top_locations(d, mode, p$species %||% "All")
  ycol <- if (identical(mode, "species")) "species_count" else "visits"
  df <- data.frame()
  if (is.data.frame(tl) && nrow(tl) > 0 && all(c("location", ycol) %in% names(tl))) {
    agg <- aggregate(list(count = tl[[ycol]]),
                     by = list(location = tl$location),
                     FUN = function(z) sum(z, na.rm = TRUE))
    df <- agg[order(-agg$count), , drop = FALSE]
  }
  .embed_desc(df, x_col = "location", y_col = "count", group_col = NULL,
              x_label = "Location",
              y_label = if (identical(mode, "species")) "Species count" else "Visits",
              resolved_options = list(view = "top_locations",
                                      top_locations_mode = mode, graph_type = "bar"))
}

# Staged marker radius by specimen count -- EXACT copy of the app's scale in
# create_spatial_data() (data_functions.R) so the embed map matches the app.
.suco_marker_size <- function(cnt) {
  cnt <- suppressWarnings(as.numeric(cnt)); cnt[is.na(cnt)] <- 0
  dplyr::case_when(
    cnt == 0   ~ 4,  cnt == 1   ~ 6,  cnt <= 5   ~ 8,  cnt <= 10  ~ 10,
    cnt <= 20  ~ 12, cnt <= 30  ~ 14, cnt <= 50  ~ 16, cnt <= 75  ~ 18,
    cnt <= 100 ~ 20, TRUE       ~ 22)
}

# Extract a single species' count from the app's species_summary ("<br>"-joined
# "Name: N" lines) -- mirrors create_spatial_data()'s per-species sapply.
.suco_species_count <- function(summary, species_filter) {
  vapply(summary, function(s) {
    if (is.na(s) || s %in% c("No species data available", "No species identified")) return(0)
    lines <- unlist(strsplit(s, "<br>"))
    line <- lines[grepl(species_filter, lines, fixed = TRUE)]
    if (length(line) > 0) {
      m <- regexpr(": ([0-9]+)", line[1])
      if (m > 0) return(as.numeric(gsub(": ", "", regmatches(line[1], m))))
    }
    0
  }, numeric(1), USE.NAMES = FALSE)
}

.suco_map_producer <- function(p) {
  env <- load_app_env("suco_history")
  if (is.null(env)) return(NULL)
  gb   <- p$group_by %||% "mmcd_all"
  spec <- p$species %||% "All"
  # Always request species detail -- marker size is driven by specimen count.
  d <- env$get_suco_data("all", p$date_range, TRUE)
  d <- env$filter_suco_data(d, p$facility %||% "all", p$fos,
                            p$zone %||% "all", p$date_range, spec)
  if (!is.data.frame(d) || nrow(d) == 0 || !all(c("x", "y") %in% names(d))) {
    return(.embed_desc_map(data.frame(), "y", "x"))
  }
  # display_species_count: the whole-sample total, or the one filtered species.
  if (!identical(spec, "All") && "species_summary" %in% names(d)) {
    cnt <- .suco_species_count(d$species_summary, spec)
  } else if ("total_species_count" %in% names(d)) {
    cnt <- suppressWarnings(as.numeric(d$total_species_count))
  } else {
    cnt <- rep(0, nrow(d))
  }
  cnt[is.na(cnt)] <- 0
  d$.count <- cnt
  d$.size  <- .suco_marker_size(cnt)
  # Human label (park and/or sitecode).
  lab <- if ("park_name" %in% names(d)) as.character(d$park_name) else rep("", nrow(d))
  lab[is.na(lab)] <- ""
  if ("sitecode" %in% names(d)) {
    sc <- as.character(d$sitecode)
    lab <- ifelse(nzchar(lab), paste0(lab, " (", sc, ")"), sc)
  }
  d$.label <- lab
  # Popup content -- same fields/format as the app's own Leaflet popup
  # (display_functions.R's popup_text): Date, Facility, FOS, Location,
  # Species Count, Species Found.
  facility_name <- get_facility_display_names(d$facility)
  foreman_name  <- get_foreman_display_names(d$foreman)
  location      <- if ("location" %in% names(d)) as.character(d$location) else lab
  species_found <- if ("species_summary" %in% names(d)) as.character(d$species_summary) else NA_character_
  species_found[is.na(species_found) | !nzchar(species_found)] <- "No species identified"
  insp_date <- if (inherits(d$inspdate, "Date")) format(d$inspdate, "%Y-%m-%d") else as.character(d$inspdate)
  d$.popup <- paste0(
    "<b>Date:</b> ", insp_date, "<br>",
    "<b>Facility:</b> ", facility_name, "<br>",
    "<b>FOS:</b> ", foreman_name, "<br>",
    "<b>Location:</b> ", location, "<br>",
    "<b>Species Count:</b> ", cnt, "<br>",
    "<b>Species Found:</b><br>", species_found
  )
  .embed_desc_map(d, lat_col = "y", lon_col = "x",
                  value_col = ".count", size_col = ".size", label_col = ".label",
                  popup_col = ".popup",
                  y_label = "Specimen count",
                  resolved_options = list(view = "map", group_by = gb,
                                          species = spec, basemap = p$basemap %||% "osm"))
}

.suco_detailed_producer <- function(p) {
  env <- load_app_env("suco_history")
  if (is.null(env)) return(NULL)
  # create_detailed_samples_table() calls make_sitecode_link() (defined in
  # shared/server_utilities.R, not sourced in the plumber process). We drop the
  # Sitecode column anyway, so give the env an identity stub.
  if (is.null(env$make_sitecode_link))
    env$make_sitecode_link <- function(x) as.character(x)
  d <- env$get_suco_data("all", p$date_range, TRUE)  # species detail cols needed
  d <- env$filter_suco_data(d, p$facility %||% "all", p$fos,
                            p$zone %||% "all", p$date_range, p$species %||% "All")
  df <- env$create_detailed_samples_table(d, p$species %||% "All")
  # Drop the HTML-linked Sitecode column so the table renders as plain text.
  cols <- c("Date", "Facility", "FOS", "Zone", "Location", "Species_Count", "Species_Found")
  .embed_desc_table(df, columns = cols,
                    resolved_options = list(view = "detailed"))
}

EMBED_PRODUCERS[["suco_history"]] <- list(
  graph         = .suco_graph_producer,
  top_locations = .suco_top_locations_producer,
  map           = .suco_map_producer,
  detailed      = .suco_detailed_producer,
  default       = .suco_graph_producer
)

# ---------------------------------------------------------------------------
# Shared helpers for the registry "historical" trend charts
# ---------------------------------------------------------------------------

#' Resolve a year range from a "from,to" param (default: last 6 seasons).
.embed_resolve_years <- function(p) {
  cur <- as.integer(format(Sys.Date(), "%Y"))
  yr <- p$year_range
  if (!is.null(yr) && length(yr) >= 1) {
    parts <- suppressWarnings(as.integer(trimws(strsplit(as.character(yr)[1], ",")[[1]])))
    parts <- parts[!is.na(parts)]
    if (length(parts) >= 2) return(list(start = min(parts), end = max(parts)))
    if (length(parts) == 1) return(list(start = parts[1], end = cur))
  }
  list(start = cur - 5, end = cur)
}

#' Normalize any app's zone param into (zone_filter vector, hist_zone_display).
#' Accepts 1 / 2 / "1,2" / combined / separate and drone's p1_only/p1_p2_* forms.
.embed_zone_to_filter <- function(zone) {
  z <- tolower(as.character(zone %||% "combined"))
  if (z %in% c("1", "p1", "p1_only"))        return(list(filter = c("1"), display = "combined"))
  if (z %in% c("2", "p2", "p2_only"))        return(list(filter = c("2"), display = "combined"))
  if (z %in% c("1,2", "separate", "p1_p2_separate", "show-both"))
    return(list(filter = c("1", "2"), display = "show-both"))
  # combined / p1_p2_combined / all / anything else
  list(filter = c("1", "2"), display = "combined")
}

.embed_metric_label <- function(m) {
  m <- tolower(as.character(m %||% ""))
  if (grepl("acre", m)) return("Acres")
  if (grepl("site", m)) return("Sites")
  if (grepl("treat", m)) return("Treatments")
  if (grepl("count", m)) return("Count")
  "Value"
}

#' Factory: a "historical" producer for apps whose create_historical_data returns
#' a group_label / time_period / value frame (ground_prehatch, drone, ...).
#' @param files    app R files to source (data_functions.R + historical_functions.R)
#' @param extra_fn function(p) -> named list of app-specific extra args
#'                 (e.g. include_drone / prehatch_only)
#' @param defaults per-app default display_metric + group_by
make_historical_producer <- function(app_folder,
                                      fn_name = "create_historical_data",
                                      files = c("data_functions.R", "historical_functions.R"),
                                      extra_fn = function(p) list(),
                                      default_metric = "treatment_acres",
                                      default_group = "mmcd_all",
                                      out_x = "time_period",
                                      out_y = "value",
                                      out_group = "group_label") {
  force(app_folder); force(fn_name); force(files); force(extra_fn)
  function(p) {
    env <- load_app_env(app_folder, files)
    if (is.null(env) || is.null(env[[fn_name]])) return(NULL)
    yrs <- .embed_resolve_years(p)
    zf  <- .embed_zone_to_filter(p$zone)
    tp  <- p$time_period %||% "yearly"
    dm  <- p$display_metric %||% default_metric
    gb  <- p$group_by %||% default_group
    args <- c(list(
      start_year          = yrs$start,
      end_year            = yrs$end,
      hist_time_period    = tp,
      hist_display_metric = dm,
      hist_group_by       = gb,
      hist_zone_display   = zf$display,
      facility_filter     = p$facility %||% "all",
      zone_filter         = zf$filter,
      foreman_filter      = p$fos %||% "all"
    ), extra_fn(p))
    # Some apps' historical fns re-source files by relative path at call time,
    # so run with cwd = the app folder (restored afterward).
    df <- .embed_in_app_wd(app_folder, do.call(env[[fn_name]], args))
    .embed_desc(df, x_col = out_x, y_col = out_y, group_col = out_group,
                x_label = if (identical(tp, "weekly")) "Week" else "Year",
                y_label = .embed_metric_label(dm),
                resolved_options = list(view = "historical", group_by = gb,
                                        time_period = tp, display_metric = dm,
                                        graph_type = p$chart_type %||% "stacked_bar"))
  }
}

# ---------------------------------------------------------------------------
# ground_prehatch_progress (historical trend)
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["ground_prehatch_progress"]] <- list(
  views = c("historical", "details", "overview"),
  default_view = "historical",
  params = list(
    facility        = list(type = "string",  default = "all"),
    zone            = list(type = "enum", allowed = c("1","2","1,2","combined"), default = "combined"),
    fos             = list(type = "csv",     default = NULL),
    group_by        = list(type = "enum", allowed = c("mmcd_all","facility","foreman","sectcode"), default = "mmcd_all"),
    time_period     = list(type = "enum", allowed = c("yearly","weekly"), default = "yearly"),
    display_metric  = list(type = "enum", allowed = c("sites","acres","treatment_acres"), default = "treatment_acres"),
    chart_type      = list(type = "enum", allowed = c("stacked_bar","grouped_bar","line","area"), default = "stacked_bar"),
    year_range      = list(type = "string", default = NULL),
    include_drone   = list(type = "logical", default = TRUE),
    expiring_filter = list(type = "enum", allowed = c("all","expiring","expiring_expired"), default = "all"),
    expiring_days   = list(type = "int",     default = 14L)
  )
)
# Detailed View table -- reproduces create_details_table()'s column mapping as a
# plain frame (it returns a DT widget, unusable as a payload).
.gp_details_producer <- function(p) {
  env <- load_app_env("ground_prehatch_progress", c("data_functions.R"))
  if (is.null(env) || is.null(env$get_site_details_data) ||
      is.null(env$filter_ground_data) || is.null(env$load_raw_data)) return(NULL)
  adate <- Sys.Date()
  raw <- tryCatch(env$load_raw_data(analysis_date = adate, include_archive = FALSE),
                  error = function(e) NULL)
  sd <- env$get_site_details_data(p$expiring_days %||% 14L, adate, raw_data = raw)
  if (!is.data.frame(sd) || nrow(sd) == 0) return(.embed_desc_table(data.frame()))
  zf <- .embed_zone_to_filter(p$zone)
  inc_drone <- if (is.null(p$include_drone)) TRUE else isTRUE(p$include_drone)
  fd <- env$filter_ground_data(sd, zf$filter, p$facility %||% "all",
                               p$fos %||% "all", inc_drone)
  ef <- p$expiring_filter %||% "all"
  if (identical(ef, "expiring"))
    fd <- fd[which(fd$prehatch_status == "expiring"), , drop = FALSE]
  else if (identical(ef, "expiring_expired"))
    fd <- fd[which(fd$prehatch_status %in% c("expiring", "expired")), , drop = FALSE]
  if (!is.data.frame(fd) || nrow(fd) == 0) return(.embed_desc_table(data.frame()))
  # FOS emp_num -> shortname (same match() the app's create_details_table uses)
  fl <- tryCatch(get_foremen_lookup(), error = function(e) NULL)
  if (!is.null(fl) && "fosarea" %in% names(fd)) {
    m <- match(fd$fosarea, fl$emp_num)
    fd$foreman_name <- ifelse(is.na(fd$fosarea), NA_character_,
                              ifelse(is.na(m), as.character(fd$fosarea),
                                     as.character(fl$shortname)[m]))
  } else {
    fd$foreman_name <- if ("fosarea" %in% names(fd)) as.character(fd$fosarea) else NA_character_
  }
  if (!("facility_display" %in% names(fd)) && "facility" %in% names(fd))
    fd$facility_display <- fd$facility
  ren <- c(Facility = "facility_display", `Priority Zone` = "zone", Section = "sectcode",
           Sitecode = "sitecode", FOS = "foreman_name", Acres = "acres", Priority = "priority",
           `Treatment Type` = "prehatch", Status = "prehatch_status",
           `Last Treatment` = "inspdate", `Days Since Last Treatment` = "age",
           Material = "matcode", `Effect Days` = "effect_days")
  ren <- ren[ren %in% names(fd)]
  out <- fd[, unname(ren), drop = FALSE]
  names(out) <- names(ren)
  .embed_desc_table(out, columns = names(ren),
                    resolved_options = list(view = "details"))
}

# Progress Overview: per-group status breakdown (Treated/Expiring/Expired/
# Skipped) as a stacked bar -- the same components the app's overlay chart shows.
.gp_overview_producer <- function(p) {
  env <- load_app_env("ground_prehatch_progress", c("data_functions.R"))
  if (is.null(env) || is.null(env$get_ground_prehatch_data) ||
      is.null(env$aggregate_data_by_group) || is.null(env$filter_ground_data) ||
      is.null(env$get_site_details_data) || is.null(env$load_raw_data)) return(NULL)
  adate <- Sys.Date()
  raw <- tryCatch(env$load_raw_data(analysis_date = adate, include_archive = FALSE),
                  error = function(e) NULL)
  zf <- .embed_zone_to_filter(p$zone)
  gpd <- env$get_ground_prehatch_data(zf$filter, adate, p$expiring_days %||% 14L, raw_data = raw)
  if (!is.data.frame(gpd) || nrow(gpd) == 0) return(.embed_desc(data.frame(), "x", "y"))
  inc_drone <- if (is.null(p$include_drone)) TRUE else isTRUE(p$include_drone)
  filt <- env$filter_ground_data(gpd, zf$filter, p$facility %||% "all", p$fos %||% "all", inc_drone)
  sd <- env$get_site_details_data(p$expiring_days %||% 14L, adate, raw_data = raw)
  agg <- env$aggregate_data_by_group(filt, p$group_by %||% "mmcd_all", zf$filter,
                                     p$expiring_filter %||% "all", sd,
                                     identical(zf$display, "combined"))
  if (!is.data.frame(agg) || nrow(agg) == 0) return(.embed_desc(data.frame(), "x", "y"))
  metric <- p$display_metric %||% "sites"
  cols <- if (identical(metric, "acres"))
    c(Treated = "ph_treated_acres", Expiring = "ph_expiring_acres",
      Expired = "ph_expired_acres", Skipped = "ph_skipped_acres")
  else
    c(Treated = "ph_treated_cnt", Expiring = "ph_expiring_cnt",
      Expired = "ph_expired_cnt", Skipped = "ph_skipped_cnt")
  cols <- cols[cols %in% names(agg)]
  xcol <- if ("display_name" %in% names(agg)) "display_name"
          else if ((p$group_by %||% "mmcd_all") %in% names(agg)) p$group_by else NULL
  if (length(cols) == 0 || is.null(xcol)) return(.embed_desc(data.frame(), "x", "y"))
  xg <- as.character(agg[[xcol]])
  long <- do.call(rbind, lapply(names(cols), function(st) data.frame(
    .x = xg, type = st,
    value = suppressWarnings(as.numeric(agg[[cols[[st]]]])), stringsAsFactors = FALSE)))
  .embed_desc(long, x_col = ".x", y_col = "value", group_col = "type",
              x_label = "Group", y_label = if (identical(metric, "acres")) "Acres" else "Sites",
              resolved_options = list(view = "overview", group_by = p$group_by %||% "mmcd_all",
                                      display_metric = metric, graph_type = "stacked_bar"))
}

EMBED_PRODUCERS[["ground_prehatch_progress"]] <- local({
  hist <- make_historical_producer(
    "ground_prehatch_progress",
    extra_fn = function(p) list(include_drone = if (is.null(p$include_drone)) TRUE else isTRUE(p$include_drone)),
    default_metric = "treatment_acres")
  list(historical = hist, details = .gp_details_producer,
       overview = .gp_overview_producer, default = hist)
})

# ---------------------------------------------------------------------------
# drone (historical trend)
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["drone"]] <- list(
  views = c("historical", "site_stats", "map", "current"),
  default_view = "historical",
  params = list(
    facility       = list(type = "string",  default = "all"),
    zone           = list(type = "enum", allowed = c("p1_only","p2_only","p1_p2_separate","p1_p2_combined","1","2","1,2","combined"), default = "p1_p2_combined"),
    fos            = list(type = "csv",     default = NULL),
    group_by       = list(type = "enum", allowed = c("mmcd_all","facility","foreman","sectcode"), default = "mmcd_all"),
    time_period    = list(type = "enum", allowed = c("yearly","weekly"), default = "yearly"),
    display_metric = list(type = "enum", allowed = c("sites","site_acres","treatment_acres"), default = "treatment_acres"),
    chart_type     = list(type = "enum", allowed = c("stacked_bar","grouped_bar","line","area","step"), default = "stacked_bar"),
    year_range     = list(type = "string", default = NULL),
    prehatch_only  = list(type = "logical", default = FALSE),
    expiring_days  = list(type = "int",     default = 14L),
    map_basemap    = list(type = "enum", allowed = c("osm","carto","satellite"), default = "osm"),
    current_metric = list(type = "enum", allowed = c("sites","treated_acres"), default = "sites")
  )
)
.drone_site_stats_producer <- function(p) {
  env <- load_app_env("drone", c("data_functions.R"))
  if (is.null(env) || is.null(env$get_site_stats_data) || is.null(env$get_sitecode_data)) return(NULL)
  zf  <- .embed_zone_to_filter(p$zone)
  yrs <- .embed_resolve_years(p)
  gb  <- p$group_by %||% "mmcd_all"
  raw <- env$get_sitecode_data(yrs$start, yrs$end, zf$filter,
                               p$facility %||% "all", p$fos %||% "all",
                               isTRUE(p$prehatch_only))
  if (!is.data.frame(raw) || nrow(raw) == 0) return(.embed_desc_table(data.frame()))
  df <- env$get_site_stats_data(raw, zf$filter,
                                combine_zones = identical(zf$display, "combined"),
                                group_by = gb)
  .embed_desc_table(df, resolved_options = list(view = "site_stats", group_by = gb))
}

# Map of drone sites, colored by treatment status (load_spatial_data returns an
# sf object -> extract coords with sf, drop geometry).
.drone_map_producer <- function(p) {
  env <- load_app_env("drone", c("data_functions.R"))
  if (is.null(env) || is.null(env$load_spatial_data)) return(NULL)
  if (!requireNamespace("sf", quietly = TRUE)) return(.embed_desc_map(data.frame(), "lat", "lon"))
  zf <- .embed_zone_to_filter(p$zone)
  sfobj <- tryCatch(env$load_spatial_data(
    analysis_date = Sys.Date(), zone_filter = zf$filter,
    facility_filter = p$facility %||% "all", foreman_filter = p$fos %||% "all",
    prehatch_only = isTRUE(p$prehatch_only), expiring_days = p$expiring_days %||% 14L),
    error = function(e) NULL)
  if (is.null(sfobj) || !inherits(sfobj, "sf") || nrow(sfobj) == 0)
    return(.embed_desc_map(data.frame(), "lat", "lon"))
  coords <- sf::st_coordinates(sfobj)
  df <- sf::st_drop_geometry(sfobj)
  df$.lon <- coords[, 1]; df$.lat <- coords[, 2]
  df$.label <- if ("sitecode" %in% names(df)) as.character(df$sitecode) else ""
  ccol <- if ("treatment_status" %in% names(df)) "treatment_status" else NULL
  vcol <- if ("treated_acres" %in% names(df)) "treated_acres" else NULL
  .embed_desc_map(df, lat_col = ".lat", lon_col = ".lon", value_col = vcol,
                  category_col = ccol, label_col = ".label", y_label = "Treated acres",
                  resolved_options = list(view = "map", basemap = p$map_basemap %||% "osm"))
}

# Current Progress: per-group Active / Expiring / Expired drone sites (or acres).
.drone_current_producer <- function(p) {
  env <- load_app_env("drone", c("data_functions.R", "display_functions.R"))
  if (is.null(env) || is.null(env$load_raw_data) || is.null(env$apply_data_filters) ||
      is.null(env$process_current_data)) return(NULL)
  zf <- .embed_zone_to_filter(p$zone)
  data <- tryCatch(env$load_raw_data(drone_types = c("Y", "M", "C"), analysis_date = Sys.Date()),
                   error = function(e) NULL)
  if (is.null(data)) return(.embed_desc(data.frame(), "x", "y"))
  filtered <- env$apply_data_filters(data = data, facility_filter = p$facility %||% "all",
                                     foreman_filter = p$fos %||% "all",
                                     prehatch_only = isTRUE(p$prehatch_only))
  processed <- tryCatch(env$process_current_data(
    drone_sites = filtered$sites, drone_treatments = filtered$treatments,
    zone_filter = zf$filter, combine_zones = identical(zf$display, "combined"),
    expiring_days = p$expiring_days %||% 14L, group_by = p$group_by %||% "mmcd_all",
    analysis_date = Sys.Date()), error = function(e) NULL)
  df <- if (is.list(processed) && "data" %in% names(processed)) processed$data else processed
  if (!is.data.frame(df) || nrow(df) == 0) return(.embed_desc(data.frame(), "x", "y"))
  metric <- p$current_metric %||% "sites"
  if (identical(metric, "treated_acres")) {
    tot <- "total_acres"; act <- "active_acres"; exp <- "expiring_acres"; ylab <- "Treated acres"
  } else {
    tot <- "total_count"; act <- "active_count"; exp <- "expiring_count"; ylab <- "Sites"
  }
  if (!all(c(tot, act) %in% names(df)) || !("display_name" %in% names(df)))
    return(.embed_desc(data.frame(), "x", "y"))
  xg  <- as.character(df$display_name)
  tv  <- suppressWarnings(as.numeric(df[[tot]]))
  av  <- suppressWarnings(as.numeric(df[[act]]))
  ev  <- if (exp %in% names(df)) suppressWarnings(as.numeric(df[[exp]])) else rep(0, nrow(df))
  long <- rbind(
    data.frame(.x = xg, type = "Active",  value = av - ev, stringsAsFactors = FALSE),
    data.frame(.x = xg, type = "Expiring", value = ev,     stringsAsFactors = FALSE),
    data.frame(.x = xg, type = "Expired",  value = tv - av, stringsAsFactors = FALSE))
  .embed_desc(long, x_col = ".x", y_col = "value", group_col = "type",
              x_label = "Group", y_label = ylab,
              resolved_options = list(view = "current", group_by = p$group_by %||% "mmcd_all",
                                      current_metric = metric, graph_type = "stacked_bar"))
}
EMBED_PRODUCERS[["drone"]] <- local({
  hist <- make_historical_producer(
    "drone",
    extra_fn = function(p) list(prehatch_only = isTRUE(p$prehatch_only)),
    default_metric = "treatment_acres")
  list(historical = hist, site_stats = .drone_site_stats_producer,
       map = .drone_map_producer, current = .drone_current_producer, default = hist)
})

# ---------------------------------------------------------------------------
# catch_basin_status (historical trend; fn create_historical_cb_data ->
# time_period / count / group_name)
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["catch_basin_status"]] <- list(
  views = c("historical", "details", "overview"),
  default_view = "historical",
  params = list(
    facility        = list(type = "string",  default = "all"),
    zone            = list(type = "enum", allowed = c("1","2","1,2","combined"), default = "combined"),
    fos             = list(type = "csv",     default = NULL),
    group_by        = list(type = "enum", allowed = c("facility","foreman","sectcode","mmcd_all"), default = "mmcd_all"),
    time_period     = list(type = "enum", allowed = c("yearly","weekly"), default = "yearly"),
    display_metric  = list(type = "enum", allowed = c("treatments","sites","total_count","weekly_active_treatments"), default = "treatments"),
    chart_type      = list(type = "enum", allowed = c("stacked_bar","grouped_bar","line","area"), default = "stacked_bar"),
    year_range      = list(type = "string", default = NULL),
    expiring_filter = list(type = "enum", allowed = c("all","expiring","expiring_expired"), default = "all"),
    expiring_days   = list(type = "int",     default = 14L)
  )
)
.cb_details_producer <- function(p) {
  env <- load_app_env("catch_basin_status", c("data_functions.R", "display_functions.R"))
  if (is.null(env) || is.null(env$load_raw_data) ||
      is.null(env$process_catch_basin_data) || is.null(env$format_details_table)) return(NULL)
  zf <- .embed_zone_to_filter(p$zone)
  data <- env$load_raw_data(analysis_date = Sys.Date(), include_archive = FALSE,
                            facility_filter = p$facility %||% "all",
                            foreman_filter = p$fos %||% "all",
                            zone_filter = zf$filter,
                            expiring_days = p$expiring_days %||% 14L)
  df <- if (is.list(data) && "sites" %in% names(data)) data$sites else data
  if (!is.data.frame(df) || nrow(df) == 0) return(.embed_desc_table(data.frame()))
  processed <- env$process_catch_basin_data(df, group_by = p$group_by %||% "facility",
                                            combine_zones = identical(zf$display, "combined"),
                                            expiring_filter = p$expiring_filter %||% "all")
  tbl <- env$format_details_table(processed)
  if (!is.data.frame(tbl) || nrow(tbl) == 0) return(.embed_desc_table(data.frame()))
  .embed_desc_table(tbl, resolved_options = list(view = "details"))
}
.cb_overview_producer <- function(p) {
  env <- load_app_env("catch_basin_status", c("data_functions.R", "display_functions.R"))
  if (is.null(env) || is.null(env$load_raw_data) || is.null(env$process_catch_basin_data)) return(NULL)
  zf <- .embed_zone_to_filter(p$zone)
  data <- env$load_raw_data(analysis_date = Sys.Date(), include_archive = FALSE,
                            facility_filter = p$facility %||% "all",
                            foreman_filter = p$fos %||% "all",
                            zone_filter = zf$filter, expiring_days = p$expiring_days %||% 14L)
  df <- if (is.list(data) && "sites" %in% names(data)) data$sites else data
  if (!is.data.frame(df) || nrow(df) == 0) return(.embed_desc(data.frame(), "x", "y"))
  pr <- env$process_catch_basin_data(df, group_by = p$group_by %||% "facility",
                                     combine_zones = identical(zf$display, "combined"),
                                     expiring_filter = p$expiring_filter %||% "all")
  if (!is.data.frame(pr) || nrow(pr) == 0) return(.embed_desc(data.frame(), "x", "y"))
  cols <- c(Treated = "active_count", Expiring = "expiring_count",
            Expired = "expired_count", `Never Treated` = "untreated_count")
  cols <- cols[cols %in% names(pr)]
  xcol <- if ("display_name" %in% names(pr)) "display_name"
          else if ((p$group_by %||% "facility") %in% names(pr)) p$group_by else NULL
  if (length(cols) == 0 || is.null(xcol)) return(.embed_desc(data.frame(), "x", "y"))
  xg <- as.character(pr[[xcol]])
  long <- do.call(rbind, lapply(names(cols), function(st) data.frame(
    .x = xg, type = st,
    value = suppressWarnings(as.numeric(pr[[cols[[st]]]])), stringsAsFactors = FALSE)))
  .embed_desc(long, x_col = ".x", y_col = "value", group_col = "type",
              x_label = "Group", y_label = "Wet catch basins",
              resolved_options = list(view = "overview", group_by = p$group_by %||% "facility",
                                      graph_type = "stacked_bar"))
}
EMBED_PRODUCERS[["catch_basin_status"]] <- local({
  hist <- make_historical_producer(
    "catch_basin_status", fn_name = "create_historical_cb_data",
    default_metric = "treatments", out_y = "count", out_group = "group_name")
  list(historical = hist, details = .cb_details_producer,
       overview = .cb_overview_producer, default = hist)
})

# ---------------------------------------------------------------------------
# struct_trt (historical trend; fn create_historical_struct_data ->
# time_period / count / group_name; extra structure_type + status_types)
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["struct_trt"]] <- list(
  views = c("historical", "current"),
  default_view = "historical",
  params = list(
    facility       = list(type = "string",  default = "all"),
    zone           = list(type = "enum", allowed = c("1","2","1,2","combined"), default = "combined"),
    fos            = list(type = "csv",     default = NULL),
    group_by       = list(type = "enum", allowed = c("facility","foreman","township","mmcd_all"), default = "mmcd_all"),
    time_period    = list(type = "enum", allowed = c("yearly","weekly"), default = "yearly"),
    display_metric = list(type = "enum", allowed = c("treatments","structures_count","proportion","weekly_active_treatments"), default = "treatments"),
    chart_type     = list(type = "enum", allowed = c("stacked_bar","grouped_bar","line","area","pie"), default = "stacked_bar"),
    structure_type = list(type = "csv",     default = NULL),
    status_types   = list(type = "csv",     allowed = c("D","W","U"), default = c("D","W","U")),
    expiring_days  = list(type = "int",     default = 14L),
    year_range     = list(type = "string", default = NULL)
  )
)
# Current Progress: per-group Active / Expiring / Untreated structures.
.struct_current_producer <- function(p) {
  env <- load_app_env("struct_trt", c("data_functions.R"))
  if (is.null(env) || is.null(env$get_all_structures) ||
      is.null(env$aggregate_structure_data) || is.null(env$load_raw_data)) return(NULL)
  zf <- .embed_zone_to_filter(p$zone)
  stt <- if (is.null(p$status_types)) c("D", "W", "U") else p$status_types
  structures <- env$get_all_structures(p$facility %||% "all", p$fos %||% "all",
                                       p$structure_type %||% "all", "all", stt, zf$filter)
  cd <- tryCatch(env$load_raw_data(analysis_date = Sys.Date(), expiring_days = p$expiring_days %||% 14L,
                 facility_filter = p$facility %||% "all", foreman_filter = p$fos %||% "all",
                 structure_type_filter = p$structure_type %||% "all", priority_filter = "all",
                 status_types = stt, zone_filter = zf$filter), error = function(e) NULL)
  treatments <- if (is.list(cd) && "treatments" %in% names(cd)) cd$treatments else cd
  agg <- env$aggregate_structure_data(structures, treatments, p$group_by %||% "facility",
                                      zf$filter, identical(zf$display, "combined"))
  if (!is.data.frame(agg) || nrow(agg) == 0 || !all(c("total_count", "active_count") %in% names(agg)))
    return(.embed_desc(data.frame(), "x", "y"))
  xcol <- if ("display_name" %in% names(agg)) "display_name"
          else if ((p$group_by %||% "facility") %in% names(agg)) p$group_by else NULL
  if (is.null(xcol)) return(.embed_desc(data.frame(), "x", "y"))
  xg  <- as.character(agg[[xcol]])
  tot <- suppressWarnings(as.numeric(agg$total_count))
  act <- suppressWarnings(as.numeric(agg$active_count))
  exp <- if ("expiring_count" %in% names(agg)) suppressWarnings(as.numeric(agg$expiring_count)) else rep(0, nrow(agg))
  long <- rbind(
    data.frame(.x = xg, type = "Active",    value = act - exp, stringsAsFactors = FALSE),
    data.frame(.x = xg, type = "Expiring",  value = exp,       stringsAsFactors = FALSE),
    data.frame(.x = xg, type = "Untreated", value = tot - act, stringsAsFactors = FALSE))
  .embed_desc(long, x_col = ".x", y_col = "value", group_col = "type",
              x_label = "Group", y_label = "Structures",
              resolved_options = list(view = "current", group_by = p$group_by %||% "facility",
                                      graph_type = "stacked_bar"))
}
EMBED_PRODUCERS[["struct_trt"]] <- local({
  hist <- make_historical_producer(
    "struct_trt", fn_name = "create_historical_struct_data",
    default_metric = "treatments", out_y = "count", out_group = "group_name",
    extra_fn = function(p) list(
      structure_type_filter = p$structure_type,
      status_types = if (is.null(p$status_types)) c("D","W","U") else p$status_types))
  list(historical = hist, current = .struct_current_producer, default = hist)
})

# ---------------------------------------------------------------------------
# cattail_inspections (Historical Comparison: per-facility baseline vs current)
# get_historical_progress_data -> facility_display / type / site_count / acre_count
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["cattail_inspections"]] <- list(
  views = c("historical", "progress", "treatment_planning"),
  default_view = "historical",
  params = list(
    hist_zone     = list(type = "enum", allowed = c("p1","p2","combined","separate"), default = "combined"),
    hist_years    = list(type = "int",    default = 3L),
    hist_facility = list(type = "csv",     default = "all"),
    hist_metric   = list(type = "enum", allowed = c("sites","acres"), default = "sites"),
    goal_year     = list(type = "string", default = NULL),
    goal_column   = list(type = "enum", allowed = c("total","p1","p2","separate"), default = "total"),
    custom_today  = list(type = "date",   default = NULL),
    facility      = list(type = "string", default = "all"),
    view_type     = list(type = "enum", allowed = c("acres","sites"), default = "acres"),
    plan_types    = list(type = "csv",    default = NULL)
  )
)
.cattail_insp_historical_producer <- function(p) {
  env <- load_app_env("cattail_inspections", c("data_functions.R", "historical_functions.R"))
  if (is.null(env) || is.null(env$get_historical_progress_data)) return(NULL)
  hy <- p$hist_years %||% 3L
  df <- env$get_historical_progress_data(hy, p$hist_zone %||% "combined",
                                         p$hist_facility %||% "all")
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(.embed_desc(data.frame(), x_col = "x", y_col = "y"))
  }
  metric <- p$hist_metric %||% "sites"
  df$.y <- if (identical(metric, "acres")) df$acre_count else df$site_count
  fac_col <- if ("facility_display" %in% names(df)) "facility_display" else "facility"
  df$.x <- if ("zone" %in% names(df)) paste0(df[[fac_col]], " P", df$zone) else as.character(df[[fac_col]])
  gcol <- if ("type" %in% names(df)) "type" else NULL
  .embed_desc(df, x_col = ".x", y_col = ".y", group_col = gcol,
              x_label = "Facility",
              y_label = if (identical(metric, "acres")) "Unique Acres Inspected" else "Unique Sites Inspected",
              resolved_options = list(view = "historical", hist_metric = metric,
                                      hist_zone = p$hist_zone %||% "combined",
                                      graph_type = "grouped_bar"))
}
# Progress vs Goal: grouped bars of Goal vs Actual inspections per facility.
.cattail_insp_progress_producer <- function(p) {
  env <- load_app_env("cattail_inspections", c("data_functions.R", "progress_functions.R"))
  if (is.null(env) || is.null(env$get_progress_data)) return(NULL)
  zo <- p$goal_column %||% "total"
  yr <- p$goal_year %||% format(Sys.Date(), "%Y")
  ct <- if (!is.null(p$custom_today)) p$custom_today else Sys.Date()
  df <- tryCatch(env$get_progress_data(yr, zo, ct), error = function(e) NULL)
  if (!is.data.frame(df) || nrow(df) == 0) return(.embed_desc(data.frame(), "x", "y"))
  if (all(c("goal", "actual") %in% names(df)) && !("count" %in% names(df))) {
    # "separate" wide form -> long (Goal / Actual Inspections)
    has_zone <- "zone" %in% names(df)
    xg <- if (has_zone) paste0(df$facility, " ", df$zone) else as.character(df$facility)
    df <- data.frame(
      .x    = c(xg, xg),
      type  = c(rep("Goal", nrow(df)), rep("Actual Inspections", nrow(df))),
      count = c(suppressWarnings(as.numeric(df$goal)), suppressWarnings(as.numeric(df$actual))),
      stringsAsFactors = FALSE)
    xcol <- ".x"
  } else {
    xcol <- "facility"
  }
  .embed_desc(df, x_col = xcol, y_col = "count", group_col = "type",
              x_label = "Facility", y_label = "Inspections",
              resolved_options = list(view = "progress", goal_column = zo,
                                      goal_year = yr, graph_type = "grouped_bar"))
}

# Treatment Planning: one bar per plan type (Air/Drone/Ground/None/Unknown),
# summed across facilities (or filtered to one) -- EXACTLY as the app's
# create_treatment_plan_plot_with_data does (x = plan_type, y = acres or site
# count per view_type). get_treatment_plan_data returns per-facility x plan_type
# rows with total_acres; "sites" view counts rows (n()) like the app's "all" path.
.cattail_insp_treatment_producer <- function(p) {
  env <- load_app_env("cattail_inspections", c("planned_treatment_functions.R"))
  if (is.null(env) || is.null(env$get_treatment_plan_data)) return(NULL)
  df <- tryCatch(env$get_treatment_plan_data(), error = function(e) NULL)
  if (!is.data.frame(df) || nrow(df) == 0) return(.embed_desc(data.frame(), "x", "y"))
  fac <- p$facility %||% "all"
  if (!identical(fac, "all") && "facility" %in% names(df)) df <- df[df$facility == fac, , drop = FALSE]
  pt <- p$plan_types
  if (!is.null(pt) && !(length(pt) == 1 && identical(pt, "all")) && "airgrnd_plan" %in% names(df))
    df <- df[df$airgrnd_plan %in% pt, , drop = FALSE]
  if (nrow(df) == 0) return(.embed_desc(data.frame(), "x", "y"))
  df$plan_type <- as.character(df$plan_type)
  view_type <- p$view_type %||% "acres"
  acres <- tapply(suppressWarnings(as.numeric(df$total_acres)), df$plan_type,
                  function(z) sum(z, na.rm = TRUE))
  sites <- tapply(rep(1L, nrow(df)), df$plan_type, sum)
  pts <- names(acres)
  out <- data.frame(
    plan_type = pts,
    value = if (identical(view_type, "sites")) as.numeric(sites[pts]) else as.numeric(acres[pts]),
    stringsAsFactors = FALSE)
  .embed_desc(out, x_col = "plan_type", y_col = "value", group_col = NULL,
              x_label = "Plan Type",
              y_label = if (identical(view_type, "sites")) "Number of Sites" else "Total Acres",
              resolved_options = list(view = "treatment_planning", view_type = view_type,
                                      graph_type = "bar"))
}

EMBED_PRODUCERS[["cattail_inspections"]] <- list(
  historical         = .cattail_insp_historical_producer,
  progress           = .cattail_insp_progress_producer,
  treatment_planning = .cattail_insp_treatment_producer,
  default            = .cattail_insp_historical_producer
)

# ---------------------------------------------------------------------------
# inspections (gap / wet / larvae analysis tables). Two-stage in the app
# (Load Data -> Analyze); here we just load with the same filters then call the
# analysis function, mirroring each tab's server reactive.
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["inspections"]] <- list(
  views = c("gaps", "analytics", "larvae", "red_bug_gaps"),
  default_view = "gaps",
  params = list(
    facility          = list(type = "string", default = "all"),
    zone              = list(type = "enum", allowed = c("1","2","1,2","all"), default = "all"),
    fos               = list(type = "csv",    default = NULL),
    priority          = list(type = "csv",    default = NULL),
    air_gnd           = list(type = "enum", allowed = c("A","G","both"), default = "both"),
    drone_filter      = list(type = "enum", allowed = c("drone_only","no_drone","include_drone"), default = "include_drone"),
    years_gap         = list(type = "int",    default = 3L),
    years_red_bug_gap = list(type = "int",    default = 5L),
    red_bug_group_by  = list(type = "enum", allowed = c("facility","fos"), default = "facility"),
    years_back        = list(type = "int",    default = 5L),
    larvae_threshold  = list(type = "int",    default = 2L),
    min_inspections   = list(type = "int",    default = 5L),
    spring_only       = list(type = "logical", default = FALSE),
    prehatch_only     = list(type = "logical", default = FALSE)
  )
)
# Shared: load comprehensive_data with the app's filters (Load Data stage).
.inspections_load <- function(env, p) {
  fac <- p$facility; if (is.null(fac) || identical(fac, "all")) fac <- NULL
  fos <- p$fos; if (is.null(fos) || (length(fos) == 1 && identical(fos, "all"))) fos <- NULL
  zone <- p$zone %||% "all"
  zf <- if (identical(zone, "all")) NULL else if (identical(zone, "1,2")) c("1", "2") else zone
  pri <- p$priority; if (is.null(pri) || (length(pri) == 1 && identical(pri, "all"))) pri <- NULL
  env$load_raw_data(facility_filter = fac, fosarea_filter = fos, zone_filter = zf,
                    priority_filter = pri, drone_filter = p$drone_filter %||% "include_drone",
                    spring_only = isTRUE(p$spring_only), prehatch_only = isTRUE(p$prehatch_only))
}
.inspections_gaps_producer <- function(p) {
  env <- load_app_env("inspections", c("data_functions.R"))
  if (is.null(env) || is.null(env$get_inspection_gaps_from_data)) return(NULL)
  comp <- .inspections_load(env, p)
  if (!is.data.frame(comp) || nrow(comp) == 0) return(.embed_desc_table(data.frame()))
  ag <- p$air_gnd %||% "both"
  if (!identical(ag, "both") && "air_gnd" %in% names(comp)) comp <- comp[comp$air_gnd == ag, , drop = FALSE]
  df <- env$get_inspection_gaps_from_data(comp, p$years_gap %||% 3L, Sys.Date())
  .embed_desc_table(df, resolved_options = list(view = "gaps", years_gap = p$years_gap %||% 3L))
}
.inspections_wet_producer <- function(p) {
  env <- load_app_env("inspections", c("data_functions.R"))
  if (is.null(env) || is.null(env$get_wet_frequency_from_data)) return(NULL)
  comp <- .inspections_load(env, p)
  if (!is.data.frame(comp) || nrow(comp) == 0) return(.embed_desc_table(data.frame()))
  df <- env$get_wet_frequency_from_data(comp, p$air_gnd %||% "both",
                                        p$min_inspections %||% 5L, p$years_back %||% 5L)
  .embed_desc_table(df, resolved_options = list(view = "analytics"))
}
.inspections_larvae_producer <- function(p) {
  env <- load_app_env("inspections", c("data_functions.R"))
  if (is.null(env) || is.null(env$get_high_larvae_sites_from_data)) return(NULL)
  comp <- .inspections_load(env, p)
  if (!is.data.frame(comp) || nrow(comp) == 0) return(.embed_desc_table(data.frame()))
  df <- env$get_high_larvae_sites_from_data(comp, p$larvae_threshold %||% 2L,
                                            p$years_back %||% 5L, p$air_gnd %||% "both")
  .embed_desc_table(df, resolved_options = list(view = "larvae",
                                                threshold = p$larvae_threshold %||% 2L))
}
# Red Bug Gaps: gap vs recently-found sites per facility/FOS (the tab's bar chart).
.inspections_red_bug_producer <- function(p) {
  env <- load_app_env("inspections", c("data_functions.R"))
  if (is.null(env) || is.null(env$get_red_bug_gaps) || is.null(env$get_red_bug_all_sites)) return(NULL)
  fac <- p$facility; if (is.null(fac) || identical(fac, "all")) fac <- NULL
  fos <- p$fos; if (is.null(fos) || (length(fos) == 1 && identical(fos, "all"))) fos <- NULL
  zone <- p$zone %||% "all"
  zf <- if (identical(zone, "all")) NULL else if (identical(zone, "1,2")) c("1", "2") else zone
  pri <- p$priority; if (is.null(pri) || (length(pri) == 1 && identical(pri, "all"))) pri <- NULL
  args <- list(facility_filter = fac, fosarea_filter = fos, zone_filter = zf,
               priority_filter = pri, air_gnd_filter = p$air_gnd %||% "both",
               drone_filter = p$drone_filter %||% "include_drone",
               prehatch_only = isTRUE(p$prehatch_only), ref_date = Sys.Date())
  gap  <- do.call(env$get_red_bug_gaps, c(list(years_gap = p$years_red_bug_gap %||% 5L), args))
  alls <- do.call(env$get_red_bug_all_sites, args)
  gb <- p$red_bug_group_by %||% "facility"
  if (identical(gb, "fos") && !is.null(env$get_red_bug_fos_analysis)) {
    an <- env$get_red_bug_fos_analysis(gap, alls); xcol <- "fos_name"
  } else {
    an <- env$get_red_bug_facility_analysis(gap, alls); xcol <- "facility"
  }
  if (!is.data.frame(an) || nrow(an) == 0 || !(xcol %in% names(an))) return(.embed_desc(data.frame(), "x", "y"))
  xg <- as.character(an[[xcol]])
  long <- data.frame(
    .x    = c(xg, xg),
    type  = c(rep("Red Bug Gap", nrow(an)), rep("Recently Found", nrow(an))),
    count = c(suppressWarnings(as.numeric(an$gap_sites)),
              suppressWarnings(as.numeric(an$recently_found_sites))),
    stringsAsFactors = FALSE)
  .embed_desc(long, x_col = ".x", y_col = "count", group_col = "type",
              x_label = if (identical(gb, "fos")) "FOS" else "Facility", y_label = "Sites",
              resolved_options = list(view = "red_bug_gaps", red_bug_group_by = gb,
                                      graph_type = "stacked_bar"))
}
EMBED_PRODUCERS[["inspections"]] <- list(
  gaps         = .inspections_gaps_producer,
  analytics    = .inspections_wet_producer,
  larvae       = .inspections_larvae_producer,
  red_bug_gaps = .inspections_red_bug_producer,
  default      = .inspections_gaps_producer
)

# ---------------------------------------------------------------------------
# trap_surveillance (abundance / infection / vector_index trend charts). Each
# calls the app's own fetch_* trend loader; week is derived from yrwk as the app
# does. (The map view is sf-based -> deferred to the careful pass.)
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["trap_surveillance"]] <- list(
  views = c("abundance", "infection", "vector_index", "map"),
  default_view = "abundance",
  params = list(
    year             = list(type = "int",    default = NULL),
    yrwk             = list(type = "int",    default = NULL),
    species          = list(type = "string", default = "Total_Cx_vectors"),
    infection_metric = list(type = "enum", allowed = c("mle", "mir"), default = "mle"),
    metric_type      = list(type = "enum", allowed = c("abundance", "infection", "vector_index"), default = "abundance"),
    color_theme      = list(type = "string", default = "MMCD")
  )
)
.trap_year <- function(p) {
  y <- suppressWarnings(as.integer(p$year))
  if (length(y) != 1 || is.na(y)) as.integer(format(Sys.Date(), "%Y")) else y
}
.trap_week_col <- function(df) {
  if (!("week" %in% names(df)) && "yrwk" %in% names(df))
    df$week <- as.numeric(substr(as.character(df$yrwk), 5, 6))
  df
}
.trap_abundance_producer <- function(p) {
  env <- load_app_env("trap_surveillance", c("data_functions.R"))
  if (is.null(env) || is.null(env$fetch_abundance_data)) return(NULL)
  ad <- env$fetch_abundance_data(year = .trap_year(p), spp_name = p$species %||% "Total_Cx_vectors")
  if (!is.data.frame(ad) || nrow(ad) == 0 || !all(c("yrwk", "viarea", "mosqcount", "loc_code") %in% names(ad)))
    return(.embed_desc(data.frame(), "week", "avg_per_trap"))
  # Same per-area aggregation the app's abundance chart uses.
  g <- dplyr::summarise(dplyr::group_by(ad, yrwk, viarea),
                        total_count = sum(mosqcount, na.rm = TRUE),
                        num_traps = dplyr::n_distinct(loc_code), .groups = "drop")
  g <- as.data.frame(g)
  g$avg_per_trap <- g$total_count / pmax(g$num_traps, 1)
  g$week <- as.numeric(substr(as.character(g$yrwk), 5, 6))
  .embed_desc(g, x_col = "week", y_col = "avg_per_trap", group_col = "viarea",
              x_label = "Epiweek", y_label = "Avg per trap",
              resolved_options = list(view = "abundance",
                                      species = p$species %||% "Total_Cx_vectors",
                                      graph_type = "line"))
}
.trap_infection_producer <- function(p) {
  env <- load_app_env("trap_surveillance", c("data_functions.R"))
  met <- p$infection_metric %||% "mle"
  fn <- if (identical(met, "mir")) env$fetch_mir_trend else env$fetch_mle_trend
  if (is.null(fn)) return(NULL)
  td <- fn(.trap_year(p))
  if (!is.data.frame(td) || nrow(td) == 0) return(.embed_desc(data.frame(), "week", met))
  td <- .trap_week_col(td)
  ycol <- if (met %in% names(td)) met else .embed_pick_col(td, c("mle", "mir", "value"))
  .embed_desc(td, x_col = "week", y_col = ycol, group_col = NULL,
              x_label = "Epiweek", y_label = toupper(met),
              resolved_options = list(view = "infection", infection_metric = met,
                                      graph_type = "line"))
}
.trap_vi_producer <- function(p) {
  env <- load_app_env("trap_surveillance", c("data_functions.R"))
  if (is.null(env) || is.null(env$fetch_vi_district_trend)) return(NULL)
  met <- p$infection_metric %||% "mle"
  td <- env$fetch_vi_district_trend(year = .trap_year(p),
                                    spp_name = p$species %||% "Total_Cx_vectors",
                                    infection_metric = met)
  if (!is.data.frame(td) || nrow(td) == 0) return(.embed_desc(data.frame(), "week", "vector_index"))
  td <- .trap_week_col(td)
  ycol <- if ("vector_index" %in% names(td)) "vector_index" else .embed_pick_col(td, c("vi", "value"))
  .embed_desc(td, x_col = "week", y_col = ycol, group_col = NULL,
              x_label = "Epiweek", y_label = "Vector Index (N x P)",
              resolved_options = list(view = "vector_index", infection_metric = met,
                                      species = p$species %||% "Total_Cx_vectors",
                                      graph_type = "line"))
}
# VI-area choropleth: shaded polygons, exactly as render_surveillance_map draws
# them. Reproduces its metric selection + non-linear colorBin scale, joins the
# per-area values onto the VI-area polygons, and emits a GeoJSON FeatureCollection
# + the binned color scale so the widget shades identically (choropleth kind).
.trap_map_producer <- function(p) {
  env <- load_app_env("trap_surveillance", c("data_functions.R"))
  if (is.null(env) || is.null(env$fetch_combined_area_data) ||
      is.null(env$load_vi_area_geometries)) return(NULL)
  if (!requireNamespace("sf", quietly = TRUE)) return(NULL)
  spp <- p$species %||% "Total_Cx_vectors"
  inf_met <- p$infection_metric %||% "mle"
  metric_type <- p$metric_type %||% "abundance"

  # Resolve the week: explicit yrwk, else the latest available week for the year.
  # (NULL yrwk -> as.integer() is integer(0); guard length so is.na() doesn't
  # choke on a zero-length value -- "argument is of length zero".)
  yrwk <- suppressWarnings(as.integer(p$yrwk))
  if (length(yrwk) != 1 || is.na(yrwk)) {
    yrwk <- NA_integer_
    yr <- .trap_year(p)
    wk <- tryCatch(env$fetch_available_weeks(yr), error = function(e) NULL)
    if (is.data.frame(wk) && nrow(wk) > 0 && "yrwk" %in% names(wk)) {
      cand <- suppressWarnings(as.integer(wk$yrwk))
      cand <- cand[!is.na(cand)]
      if (length(cand) > 0) yrwk <- max(cand)
    }
  }
  if (length(yrwk) != 1 || is.na(yrwk)) return(.embed_desc_choropleth(data.frame(), NULL, list()))

  areas_sf <- tryCatch(env$load_vi_area_geometries(), error = function(e) NULL)
  if (is.null(areas_sf) || !inherits(areas_sf, "sf") || nrow(areas_sf) == 0)
    return(.embed_desc_choropleth(data.frame(), NULL, list()))
  combined <- tryCatch(env$fetch_combined_area_data(yrwk = yrwk, spp_name = spp,
                       infection_metric = inf_met), error = function(e) NULL)

  # Metric column + non-linear breaks (verbatim from render_surveillance_map).
  if (metric_type == "vector_index") {
    metric_col <- "vector_index"; metric_label <- "Vector Index (N x P)"; fmt <- "%.4f"
    fixed_breaks <- c(0, 0.02, 0.08, 0.2, 0.5, 1.0, 2.0)
  } else if (metric_type == "infection") {
    if (identical(inf_met, "mle")) {
      metric_col <- "infection_rate"; metric_label <- "MLE (Infection Rate)"; fmt <- "%.6f"
      fixed_breaks <- c(0, 0.001, 0.005, 0.01, 0.02, 0.04, 0.06)
    } else {
      metric_col <- "mir_raw"; metric_label <- "MIR (per 1000)"; fmt <- "%.6f"
      fixed_breaks <- c(0, 2, 5, 15, 30, 60, 100)
    }
  } else {
    metric_col <- "avg_per_trap"; metric_label <- "Avg Mosquitoes/Trap"; fmt <- "%.1f"
    fixed_breaks <- c(0, 1, 3, 7, 12, 20, 30)
  }

  # Join per-area values onto the polygons (same left_join as the app).
  areas_sf$viarea <- as.character(areas_sf$viarea)
  val <- rep(NA_real_, nrow(areas_sf))
  if (is.data.frame(combined) && nrow(combined) > 0 && metric_col %in% names(combined)) {
    combined$viarea <- as.character(combined$viarea)
    m <- match(areas_sf$viarea, combined$viarea)
    val <- suppressWarnings(as.numeric(combined[[metric_col]][m]))
  }
  map_sf <- areas_sf[, "viarea"]
  map_sf$value <- val

  # Heat ramp: theme ramp if present, else the app's literal fallback.
  n_bins <- length(fixed_breaks) - 1
  theme_heat <- tryCatch(get_theme_palette(p$color_theme %||% "MMCD")$sequential_heat,
                         error = function(e) NULL)
  if (is.null(theme_heat) || length(theme_heat) < 2)
    theme_heat <- c("#ffffcc", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#800026")
  heat_colors <- grDevices::colorRampPalette(theme_heat)(n_bins)

  geojson <- .embed_sf_to_geojson(map_sf, keep_cols = c("viarea", "value"))
  if (is.null(geojson)) return(.embed_desc_choropleth(data.frame(), NULL, list()))
  areas <- lapply(seq_len(nrow(map_sf)), function(i)
    list(id = map_sf$viarea[i], value = map_sf$value[i], label = map_sf$viarea[i]))

  .embed_desc_choropleth(
    df = data.frame(viarea = map_sf$viarea, value = map_sf$value, stringsAsFactors = FALSE),
    geojson = geojson, areas = areas,
    feature_id_key = "properties.viarea",
    color_scale = list(breaks = fixed_breaks, colors = heat_colors,
                       na_color = "#C0C0C0", legend_max = max(fixed_breaks)),
    x_label = "", y_label = metric_label,
    resolved_options = list(
      view = "map", metric_type = metric_type, infection_metric = inf_met,
      species = spp, yrwk = yrwk, metric_label = metric_label, value_format = fmt))
}
EMBED_PRODUCERS[["trap_surveillance"]] <- list(
  abundance    = .trap_abundance_producer,
  infection    = .trap_infection_producer,
  vector_index = .trap_vi_producer,
  map          = .trap_map_producer,
  default      = .trap_abundance_producer
)

# ---------------------------------------------------------------------------
# air_sites_simple (Historical Analysis: treatment vs inspection volume over
# time). get_comprehensive_historical_data -> weekly/yearly summary -> 2 series.
# (status map + pipeline funnel -> deferred to the careful pass.)
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["air_sites_simple"]] <- list(
  views = c("historical", "status", "pipeline"),
  default_view = "historical",
  params = list(
    facility           = list(type = "string", default = "all"),
    zone               = list(type = "string", default = "all"),
    priority           = list(type = "csv",    default = NULL),
    status             = list(type = "enum", allowed = c("all","Unknown","Inspected","Needs ID","Needs Treatment","Active Treatment"), default = "all"),
    metric_type        = list(type = "enum", allowed = c("sites","acres"), default = "sites"),
    volume_time_period = list(type = "enum", allowed = c("weekly","yearly"), default = "weekly"),
    hist_chart_type    = list(type = "string", default = "line"),
    hist_start_date    = list(type = "date",   default = NULL),
    hist_end_date      = list(type = "date",   default = NULL),
    analysis_date      = list(type = "date",   default = NULL),
    larvae_threshold   = list(type = "int",    default = 2L)
  )
)

.air_analysis_date <- function(p) {
  d <- p$analysis_date
  if (is.null(d) || length(d) != 1 || is.na(d)) return(Sys.Date())
  dd <- suppressWarnings(as.Date(d)); if (is.na(dd)) Sys.Date() else dd
}
.air_historical_producer <- function(p) {
  env <- load_app_env("air_sites_simple", c("data_functions.R", "historical_functions.R"))
  if (is.null(env) || is.null(env$get_comprehensive_historical_data)) return(NULL)
  fac <- p$facility; if (is.null(fac) || identical(fac, "all")) fac <- NULL
  pri <- p$priority; if (is.null(pri) || (length(pri) == 1 && identical(pri, "all"))) pri <- NULL
  zone <- p$zone %||% "all"; zf <- if (zone %in% c("all", "All")) NULL else zone
  cd <- env$get_comprehensive_historical_data(
    start_date = p$hist_start_date, end_date = p$hist_end_date,
    facility_filter = fac, priority_filter = pri, zone_filter = zf,
    larvae_threshold = p$larvae_threshold %||% 2L)
  vol <- if (is.list(cd)) cd$treatment_volumes else NULL
  tp <- p$volume_time_period %||% "weekly"
  summ <- if (identical(tp, "weekly")) env$create_weekly_treatment_summary(vol)
          else env$create_yearly_treatment_summary(vol)
  if (!is.data.frame(summ) || nrow(summ) == 0) return(.embed_desc(data.frame(), "x", "y"))
  metric <- p$metric_type %||% "sites"
  tcol <- if (identical(metric, "acres")) "treatment_acres" else "treatment_sites"
  icol <- if (identical(metric, "acres")) "inspection_acres" else "inspection_sites"
  xcol <- if (identical(tp, "weekly")) "week_start_date" else "year"
  if (!all(c(xcol, tcol, icol) %in% names(summ))) return(.embed_desc(data.frame(), "x", "y"))
  xg <- as.character(summ[[xcol]])
  long <- data.frame(
    .x    = c(xg, xg),
    type  = c(rep("Treatment", nrow(summ)), rep("Inspection", nrow(summ))),
    value = c(suppressWarnings(as.numeric(summ[[tcol]])), suppressWarnings(as.numeric(summ[[icol]]))),
    stringsAsFactors = FALSE)
  .embed_desc(long, x_col = ".x", y_col = "value", group_col = "type",
              x_label = if (identical(tp, "weekly")) "Week" else "Year",
              y_label = if (identical(metric, "acres")) "Acres" else "Sites",
              resolved_options = list(view = "historical", volume_time_period = tp,
                                      metric_type = metric,
                                      graph_type = p$hist_chart_type %||% "line"))
}
# Air Site Status map: sites colored by site_status (plain longitude/latitude).
.air_status_map_producer <- function(p) {
  env <- load_app_env("air_sites_simple", c("data_functions.R"))
  if (is.null(env) || is.null(env$get_air_sites_data)) return(NULL)
  fac <- p$facility; if (is.null(fac) || identical(fac, "all")) fac <- NULL
  pri <- p$priority; if (is.null(pri) || (length(pri) == 1 && identical(pri, "all"))) pri <- NULL
  zone <- p$zone %||% "all"; zf <- if (zone %in% c("all", "All")) NULL else zone
  data <- tryCatch(env$get_air_sites_data(analysis_date = .air_analysis_date(p),
    facility_filter = fac, priority_filter = pri, zone_filter = zf,
    larvae_threshold = p$larvae_threshold %||% 2L), error = function(e) NULL)
  if (!is.data.frame(data) || nrow(data) == 0) return(.embed_desc_map(data.frame(), "latitude", "longitude"))
  st <- p$status %||% "all"
  if (!identical(st, "all") && "site_status" %in% names(data))
    data <- data[which(data$site_status == st), , drop = FALSE]
  if (!all(c("longitude", "latitude") %in% names(data))) {
    if (all(c("x", "y") %in% names(data))) { data$longitude <- data$x; data$latitude <- data$y }
    else return(.embed_desc_map(data.frame(), "latitude", "longitude"))
  }
  if (nrow(data) == 0) return(.embed_desc_map(data.frame(), "latitude", "longitude"))
  .embed_desc_map(data, lat_col = "latitude", lon_col = "longitude",
                  category_col = if ("site_status" %in% names(data)) "site_status" else NULL,
                  value_col = if ("acres" %in% names(data)) "acres" else NULL,
                  label_col = if ("sitecode" %in% names(data)) "sitecode" else NULL,
                  y_label = "Acres",
                  resolved_options = list(view = "status"))
}
# Pipeline Snapshot: the per-facility treatment-process summary table (same
# loader as the status map, then the app's create_treatment_process_summary).
.air_pipeline_producer <- function(p) {
  env <- load_app_env("air_sites_simple", c("data_functions.R", "display_functions.R"))
  if (is.null(env) || is.null(env$get_air_sites_data) ||
      is.null(env$create_treatment_process_summary))
    return(NULL)
  fac <- p$facility; if (is.null(fac) || identical(fac, "all")) fac <- NULL
  pri <- p$priority; if (is.null(pri) || (length(pri) == 1 && identical(pri, "all"))) pri <- NULL
  zone <- p$zone %||% "all"; zf <- if (zone %in% c("all", "All")) NULL else zone
  data <- tryCatch(env$get_air_sites_data(analysis_date = .air_analysis_date(p),
    facility_filter = fac, priority_filter = pri, zone_filter = zf,
    larvae_threshold = p$larvae_threshold %||% 2L), error = function(e) NULL)
  metric <- p$metric_type %||% "sites"
  if (!is.data.frame(data)) data <- data.frame()
  summ <- tryCatch(env$create_treatment_process_summary(data, metric_type = metric),
                   error = function(e) NULL)
  if (!is.data.frame(summ)) return(.embed_desc_table(data.frame()))
  .embed_desc_table(summ, x_label = "", y_label = "",
                    resolved_options = list(view = "pipeline", metric_type = metric))
}
EMBED_PRODUCERS[["air_sites_simple"]] <- list(
  historical = .air_historical_producer,
  status     = .air_status_map_producer,
  pipeline   = .air_pipeline_producer,
  default    = .air_historical_producer
)

# ---------------------------------------------------------------------------
# mosquito_surveillance_map (survey points sized/banded by mosquito count).
# The app loads `dbadult_mapdata_forr_calclat` at module scope and filters/
# aggregates inline; there is no reusable function, so we run the same query +
# per-loc_code aggregation here (keeping the raw long/lat columns -> no sf).
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["mosquito_surveillance_map"]] <- list(
  views = c("map"),
  default_view = "map",
  params = list(
    species    = list(type = "string", default = "Total_Ae_+_Cq"),
    survtype   = list(type = "enum", allowed = c("Sweep","CO2(reg)","Gravid","CO2(elev)","All"), default = "All"),
    date_range = list(type = "daterange", default = NULL),
    agg        = list(type = "enum", allowed = c("sum","avg"), default = "sum")
  )
)
.surveillance_map_producer <- function(p) {
  con <- tryCatch(get_db_connection(), error = function(e) NULL)
  if (is.null(con)) return(.embed_desc_map(data.frame(), "lat", "long"))
  df <- tryCatch(DBI::dbGetQuery(con, paste(
    "SELECT long, lat, mosqcount, spp_name, survtypename, inspdate, loc_code",
    "FROM dbadult_mapdata_forr_calclat")), error = function(e) NULL)
  tryCatch(safe_disconnect(con), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(.embed_desc_map(data.frame(), "lat", "long"))
  df$mosqcount <- suppressWarnings(as.numeric(df$mosqcount))
  sv <- p$survtype %||% "All"
  if (!identical(sv, "All")) df <- df[which(df$survtypename == sv), , drop = FALSE]
  spp <- p$species %||% "Total_Ae_+_Cq"
  df <- df[which(df$spp_name == spp), , drop = FALSE]
  dr <- p$date_range
  if (!is.null(dr) && length(dr) >= 2) {
    df$inspdate <- as.Date(df$inspdate)
    df <- df[which(!is.na(df$inspdate) & df$inspdate >= dr[1] & df$inspdate <= dr[2]), , drop = FALSE]
  }
  if (nrow(df) == 0) return(.embed_desc_map(data.frame(), "lat", "long"))
  agg <- dplyr::summarise(dplyr::group_by(df, loc_code),
           Sum = sum(mosqcount, na.rm = TRUE), Avg = round(mean(mosqcount, na.rm = TRUE), 2),
           long = dplyr::first(long), lat = dplyr::first(lat), .groups = "drop")
  agg <- as.data.frame(agg)
  mode <- p$agg %||% "sum"
  agg$.val <- if (identical(mode, "avg")) agg$Avg else agg$Sum
  agg <- agg[which(agg$.val > 0), , drop = FALSE]
  if (nrow(agg) == 0) return(.embed_desc_map(data.frame(), "lat", "long"))
  bands <- c("1-2", "3-10", "11-25", "26-50", "51-100", "100+")
  agg$.band <- as.character(cut(agg$.val, breaks = c(-Inf, 2, 10, 25, 50, 100, Inf), labels = bands))
  agg$.size <- 4 + match(agg$.band, bands) * 2
  .embed_desc_map(agg, lat_col = "lat", lon_col = "long", value_col = ".val",
                  size_col = ".size", category_col = ".band", label_col = "loc_code",
                  y_label = if (identical(mode, "avg")) "Avg per trap" else "Total",
                  resolved_options = list(view = "map", species = spp, survtype = sv, agg = mode))
}
EMBED_PRODUCERS[["mosquito_surveillance_map"]] <- list(
  map = .surveillance_map_producer, default = .surveillance_map_producer
)

# ---------------------------------------------------------------------------
# control_efficacy (% reduction boxplot by genus / material / dosage). Mirrors
# the app's load_efficacy_data + sidebar-filter pipeline, then boxplots
# pct_reduction grouped by the comparison dimension.
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["control_efficacy"]] <- list(
  views = c("boxplot"),
  default_view = "boxplot",
  params = list(
    comparison_mode = list(type = "enum", allowed = c("genus","material","dosage"), default = "genus"),
    genus           = list(type = "enum", allowed = c("Both","Aedes","Culex"), default = "Both"),
    material_type   = list(type = "string", default = "all"),
    dosage          = list(type = "string", default = "all"),
    season          = list(type = "csv", allowed = c("Spring","Summer"), default = c("Spring","Summer")),
    trt_type        = list(type = "csv", allowed = c("Air","Ground","Drone"), default = c("Air","Ground","Drone")),
    facility        = list(type = "string", default = "all"),
    year_range      = list(type = "string", default = NULL),
    use_mullas      = list(type = "logical", default = FALSE)
  )
)
.control_efficacy_boxplot_producer <- function(p) {
  env <- load_app_env("control_efficacy", c("data_functions.R"))
  if (is.null(env) || is.null(env$load_efficacy_data)) return(NULL)
  yrs <- .embed_resolve_years(p)
  d <- tryCatch(env$load_efficacy_data(start_year = yrs$start, end_year = yrs$end,
                bti_only = FALSE, use_mullas = isTRUE(p$use_mullas)), error = function(e) NULL)
  if (!is.data.frame(d) || nrow(d) == 0) return(.embed_desc_boxplot(data.frame(), "pct_reduction"))
  if ("is_control" %in% names(d)) d <- d[which(d$is_control == FALSE), , drop = FALSE]
  if ("is_invalid" %in% names(d)) d <- d[which(d$is_invalid == FALSE), , drop = FALSE]
  season <- p$season %||% c("Spring", "Summer")
  if ("season" %in% names(d)) d <- d[which(d$season %in% season), , drop = FALSE]
  genus <- p$genus %||% "Both"
  if (!identical(genus, "Both") && "genus" %in% names(d)) d <- d[which(d$genus == genus), , drop = FALSE]
  tt <- p$trt_type %||% c("Air", "Ground", "Drone")
  if ("trt_type" %in% names(d)) d <- d[which(d$trt_type %in% tt), , drop = FALSE]
  mat <- p$material_type %||% "all"
  if (!identical(mat, "all") && !is.null(env$get_matcodes_by_ingredient) && "trt_matcode" %in% names(d)) {
    con <- tryCatch(get_db_connection(), error = function(e) NULL)
    if (!is.null(con)) {
      codes <- tryCatch(env$get_matcodes_by_ingredient(con, mat), error = function(e) character(0))
      tryCatch(safe_disconnect(con), error = function(e) NULL)
      if (length(codes) > 0) d <- d[which(d$trt_matcode %in% codes), , drop = FALSE]
    }
  }
  fac <- p$facility %||% "all"
  if (!identical(fac, "all") && "facility" %in% names(d)) d <- d[which(d$facility == fac), , drop = FALSE]
  dos <- p$dosage %||% "all"
  if (!identical(dos, "all") && "dosage_label" %in% names(d)) d <- d[which(d$dosage_label == dos), , drop = FALSE]
  if (!("pct_reduction" %in% names(d)) || nrow(d) == 0) return(.embed_desc_boxplot(data.frame(), "pct_reduction"))
  d <- d[which(!is.na(d$pct_reduction)), , drop = FALSE]
  if (nrow(d) == 0) return(.embed_desc_boxplot(data.frame(), "pct_reduction"))
  d$pct_reduction <- pmax(suppressWarnings(as.numeric(d$pct_reduction)), 0)
  cm <- p$comparison_mode %||% "genus"
  gcol <- if (identical(cm, "material") && "active_ingredient" %in% names(d)) "active_ingredient"
          else if (identical(cm, "dosage") && "dosage_label" %in% names(d)) "dosage_label"
          else "genus"
  .embed_desc_boxplot(d, value_col = "pct_reduction", group_col = gcol, y_label = "% Reduction",
                      resolved_options = list(view = "boxplot", comparison_mode = cm))
}
EMBED_PRODUCERS[["control_efficacy"]] <- list(
  boxplot = .control_efficacy_boxplot_producer,
  default = .control_efficacy_boxplot_producer
)

# ---------------------------------------------------------------------------
# cattail_treatments -- Progress view: per-group status breakdown
# (Under Threshold / Need Treatment / Treated). Reproduces the group-label +
# status summarise that create_current_progress_chart does internally.
# (historical = buried aggregation, map = sf -> deferred.)
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["cattail_treatments"]] <- list(
  views = c("progress", "map", "historical"),
  default_view = "progress",
  params = list(
    facility            = list(type = "string", default = "all"),
    foreman             = list(type = "string", default = "all"),
    zone_display        = list(type = "enum", allowed = c("p1","p2","separate","combined"), default = "combined"),
    group_by            = list(type = "enum", allowed = c("mmcd_all","foreman","facility","zone"), default = "facility"),
    display_metric_type = list(type = "enum", allowed = c("sites","acres"), default = "sites"),
    hist_status_metric  = list(type = "enum", allowed = c("need_treatment","treated","pct_treated"), default = "need_treatment"),
    hist_chart_type     = list(type = "enum", allowed = c("line","area","stacked_bar","grouped_bar"), default = "line"),
    year_range          = list(type = "csv", default = NULL),
    basemap             = list(type = "enum", allowed = c("carto","satellite","osm"), default = "carto")
  )
)
.cattail_trt_progress_producer <- function(p) {
  env <- load_app_env("cattail_treatments", c("data_functions.R"))
  if (is.null(env) || is.null(env$load_raw_data) || is.null(env$apply_data_filters) ||
      is.null(env$aggregate_cattail_data)) return(NULL)
  zd <- p$zone_display %||% "combined"
  zone_filter <- switch(zd, "p1" = "1", "p2" = "2", "separate" = c("1", "2"), c("1", "2"))
  combine <- zd %in% c("combined", "p1", "p2")
  cur <- as.integer(format(Sys.Date(), "%Y"))
  raw <- tryCatch(env$load_raw_data(analysis_date = Sys.Date(), include_archive = TRUE,
                  start_year = cur - 2, end_year = cur), error = function(e) NULL)
  if (is.null(raw)) return(.embed_desc(data.frame(), "x", "y"))
  filt <- env$apply_data_filters(data = raw, zone_filter = zone_filter,
                                 facility_filter = p$facility %||% "all")
  agg <- env$aggregate_cattail_data(filt, Sys.Date())
  sd <- if (is.list(agg) && "sites_data" %in% names(agg)) agg$sites_data else NULL
  if (!is.data.frame(sd) || nrow(sd) == 0 || !("final_status" %in% names(sd)))
    return(.embed_desc(data.frame(), "x", "y"))
  gb <- p$group_by %||% "facility"
  grp <- if (gb == "mmcd_all") rep("All MMCD", nrow(sd))
    else if (gb == "foreman" && !combine) paste("FOS", sd$fosarea, "- Zone", sd$zone)
    else if (gb == "foreman") paste("FOS", sd$fosarea)
    else if (gb == "sectcode") paste("Section", sd$sectcode)
    else if (!combine && "zone" %in% names(sd)) paste(sd$facility, "- Zone", sd$zone)
    else as.character(sd$facility)
  sd$.grp <- grp
  metric <- p$display_metric_type %||% "sites"
  tally <- function(status) {
    v <- if (identical(metric, "acres")) ifelse(sd$final_status == status, suppressWarnings(as.numeric(sd$acres)), 0)
         else as.numeric(sd$final_status == status)
    tapply(v, sd$.grp, function(z) sum(z, na.rm = TRUE))
  }
  ut <- tally("under_threshold"); nt <- tally("need_treatment"); tr <- tally("treated")
  groups <- names(ut)
  long <- rbind(
    data.frame(.x = groups, type = "Under Threshold", value = as.numeric(ut), stringsAsFactors = FALSE),
    data.frame(.x = groups, type = "Need Treatment",  value = as.numeric(nt), stringsAsFactors = FALSE),
    data.frame(.x = groups, type = "Treated",         value = as.numeric(tr), stringsAsFactors = FALSE))
  .embed_desc(long, x_col = ".x", y_col = "value", group_col = "type",
              x_label = "Group", y_label = if (identical(metric, "acres")) "Acres" else "Sites",
              resolved_options = list(view = "progress", group_by = gb,
                                      display_metric_type = metric, graph_type = "stacked_bar"))
}
# Treatment map: cattail sites colored by status (filtered$sites is sf).
.cattail_trt_map_producer <- function(p) {
  env <- load_app_env("cattail_treatments", c("data_functions.R"))
  if (is.null(env) || is.null(env$load_raw_data) || is.null(env$apply_data_filters))
    return(NULL)
  zd <- p$zone_display %||% "combined"
  zone_filter <- switch(zd, "p1" = "1", "p2" = "2", "separate" = c("1", "2"), c("1", "2"))
  cur <- as.integer(format(Sys.Date(), "%Y"))
  raw <- tryCatch(env$load_raw_data(analysis_date = Sys.Date(), include_archive = TRUE,
                  start_year = cur - 2, end_year = cur), error = function(e) NULL)
  if (is.null(raw)) return(.embed_desc_map(data.frame(), "lat", "lon"))
  filt <- env$apply_data_filters(data = raw, zone_filter = zone_filter,
                                 facility_filter = p$facility %||% "all")
  sites <- filt$sites
  if (is.null(sites) || nrow(sites) == 0) return(.embed_desc_map(data.frame(), "lat", "lon"))
  if (inherits(sites, "sf")) {
    if (!requireNamespace("sf", quietly = TRUE)) return(.embed_desc_map(data.frame(), "lat", "lon"))
    coords <- sf::st_coordinates(sites)
    df <- sf::st_drop_geometry(sites)
    df$.lon <- coords[, 1]; df$.lat <- coords[, 2]
  } else if (all(c("x", "y") %in% names(sites))) {
    df <- sites; df$.lon <- df$x; df$.lat <- df$y
  } else if (all(c("longitude", "latitude") %in% names(sites))) {
    df <- sites; df$.lon <- df$longitude; df$.lat <- df$latitude
  } else {
    return(.embed_desc_map(data.frame(), "lat", "lon"))
  }
  ccol <- if ("final_status" %in% names(df)) "final_status"
          else if ("state" %in% names(df)) "state" else NULL
  .embed_desc_map(df, lat_col = ".lat", lon_col = ".lon", category_col = ccol,
                  label_col = if ("sitecode" %in% names(df)) "sitecode" else NULL,
                  resolved_options = list(view = "map", basemap = p$basemap %||% "carto"))
}
# Historical Analysis: inspection-year series by group_label. Reproduces the
# plot_data pipeline inside create_historical_analysis_chart (group_label build +
# the per-display_metric aggregation) from get_historical_cattail_data's
# $inspections/$treatments frames -> one series per group_label.
.cattail_trt_historical_producer <- function(p) {
  env <- load_app_env("cattail_treatments", c("data_functions.R", "historical_functions.R"))
  if (is.null(env) || is.null(env$get_historical_cattail_data)) return(NULL)
  cur <- as.integer(format(Sys.Date(), "%Y"))
  yr <- suppressWarnings(as.integer(p$year_range))
  yr <- yr[!is.na(yr)]
  if (length(yr) >= 2) { sy <- min(yr); ey <- max(yr) } else { sy <- cur - 4; ey <- cur }
  start_date <- as.Date(paste0(sy, "-09-01"))
  end_date   <- as.Date(paste0(ey + 1, "-08-01"))
  display_metric <- p$hist_status_metric %||% "need_treatment"
  metric_type    <- p$display_metric_type %||% "sites"
  group_by       <- p$group_by %||% "facility"
  zd <- p$zone_display %||% "combined"
  combine_zones <- !identical(zd, "separate")
  fac_f <- p$facility %||% "all"
  fos_f <- p$foreman %||% "all"

  hist <- tryCatch(env$get_historical_cattail_data(
    time_period = "yearly", display_metric = display_metric,
    start_date = start_date, end_date = end_date), error = function(e) NULL)
  if (is.null(hist)) return(.embed_desc(data.frame(), "x", "y"))
  insp <- hist$inspections; trt <- hist$treatments
  if (is.null(insp)) insp <- data.frame(); if (is.null(trt)) trt <- data.frame()

  fac_lk <- tryCatch(get_facility_lookup(), error = function(e) NULL)
  fos_lk <- tryCatch(get_foremen_lookup(), error = function(e) NULL)
  add_labels <- function(d) {
    if (!is.data.frame(d) || nrow(d) == 0) return(d)
    d$facility_short <- d$facility
    if (!is.null(fac_lk) && all(c("full_name", "short_name") %in% names(fac_lk))) {
      m <- fac_lk$short_name[match(d$facility, fac_lk$full_name)]
      d$facility_short <- ifelse(is.na(m), d$facility, m)
    }
    d$foreman_name <- paste("FOS", d$fosarea)
    if (!is.null(fos_lk) && all(c("emp_num", "shortname") %in% names(fos_lk))) {
      m <- fos_lk$shortname[match(d$fosarea, fos_lk$emp_num)]
      d$foreman_name <- ifelse(is.na(m), paste("FOS", d$fosarea), m)
    }
    d$group_label <- with(d,
      ifelse(group_by == "facility" & !combine_zones, paste(facility_short, "- Zone", zone),
      ifelse(group_by == "facility" &  combine_zones, facility_short,
      ifelse(group_by == "foreman"  & !combine_zones, paste(foreman_name, "- Zone", zone),
      ifelse(group_by == "foreman"  &  combine_zones, foreman_name,
      ifelse(group_by == "zone", paste("Zone", zone), "All"))))))
    d
  }
  insp <- add_labels(insp); trt <- add_labels(trt)
  valid <- function(f) !is.null(f) && !(length(f) == 1 && f %in% c("all", "All", ""))
  if (valid(fac_f)) {
    if (nrow(insp) > 0) insp <- insp[insp$facility %in% fac_f, , drop = FALSE]
    if (nrow(trt)  > 0) trt  <- trt[trt$facility %in% fac_f, , drop = FALSE]
  }
  if (valid(fos_f)) {
    if (nrow(insp) > 0) insp <- insp[insp$foreman_name %in% fos_f, , drop = FALSE]
    if (nrow(trt)  > 0) trt  <- trt[trt$foreman_name %in% fos_f, , drop = FALSE]
  }

  `%>%` <- magrittr::`%>%`
  plot_data <- NULL; y_label <- "Sites Need Treatment"
  if (display_metric == "treated") {
    y_label <- if (metric_type == "acres") "Acres Treated" else "Sites Treated"
    if (nrow(trt) > 0) {
      if (metric_type == "acres") {
        plot_data <- trt %>%
          dplyr::group_by(sitecode, inspection_year, group_label) %>%
          dplyr::arrange(dplyr::desc(trtdate)) %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
          dplyr::group_by(inspection_year, group_label) %>%
          dplyr::summarise(value = sum(dplyr::coalesce(treated_acres, 0), na.rm = TRUE), .groups = "drop")
      } else {
        plot_data <- trt %>%
          dplyr::group_by(inspection_year, group_label) %>%
          dplyr::summarise(value = dplyr::n_distinct(sitecode), .groups = "drop")
      }
    }
  } else if (display_metric == "pct_treated") {
    y_label <- "% Sites Treated (of Need Treatment)"
    need_c <- if (nrow(insp) > 0) insp %>% dplyr::filter(need_treatment == TRUE) %>%
        dplyr::group_by(sitecode, inspection_year, group_label) %>%
        dplyr::arrange(dplyr::desc(inspdate)) %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
        dplyr::group_by(inspection_year, group_label) %>%
        dplyr::summarise(sites_need = dplyr::n_distinct(sitecode), .groups = "drop") else NULL
    trt_c <- if (nrow(trt) > 0) trt %>%
        dplyr::group_by(inspection_year, group_label) %>%
        dplyr::summarise(sites_treated = dplyr::n_distinct(sitecode), .groups = "drop") else NULL
    if (!is.null(need_c) && nrow(need_c) > 0) {
      plot_data <- need_c
      if (!is.null(trt_c)) plot_data <- dplyr::left_join(plot_data, trt_c, by = c("inspection_year", "group_label"))
      if (!("sites_treated" %in% names(plot_data))) plot_data$sites_treated <- 0
      plot_data <- plot_data %>% dplyr::mutate(
        sites_treated = ifelse(is.na(sites_treated), 0, sites_treated),
        value = ifelse(sites_need > 0, (sites_treated / sites_need) * 100, 0))
    }
  } else {
    y_label <- if (metric_type == "acres") "Acres Need Treatment" else "Sites Need Treatment"
    if (nrow(insp) > 0) {
      base <- insp %>% dplyr::filter(need_treatment == TRUE) %>%
        dplyr::group_by(sitecode, inspection_year, group_label) %>%
        dplyr::arrange(dplyr::desc(inspdate)) %>% dplyr::slice(1) %>% dplyr::ungroup()
      plot_data <- if (metric_type == "acres")
        base %>% dplyr::group_by(inspection_year, group_label) %>%
          dplyr::summarise(value = sum(acres, na.rm = TRUE), .groups = "drop")
      else
        base %>% dplyr::group_by(inspection_year, group_label) %>%
          dplyr::summarise(value = dplyr::n_distinct(sitecode), .groups = "drop")
    }
  }
  if (is.null(plot_data) || nrow(plot_data) == 0) return(.embed_desc(data.frame(), "x", "y"))
  plot_data <- as.data.frame(plot_data)
  plot_data$.x <- as.character(plot_data$inspection_year)
  ct <- p$hist_chart_type %||% "line"
  gt <- switch(ct, "grouped_bar" = "bar", "stacked_bar" = "stacked_bar", ct)
  .embed_desc(plot_data, x_col = ".x", y_col = "value", group_col = "group_label",
              x_label = "Inspection Year", y_label = y_label,
              resolved_options = list(view = "historical", group_by = group_by,
                                      display_metric = display_metric,
                                      display_metric_type = metric_type, graph_type = gt))
}
EMBED_PRODUCERS[["cattail_treatments"]] <- list(
  progress   = .cattail_trt_progress_producer,
  map        = .cattail_trt_map_producer,
  historical = .cattail_trt_historical_producer,
  default    = .cattail_trt_progress_producer
)

# ---------------------------------------------------------------------------
# mosquito-monitoring (avg mosquitoes/trap over time by species). The app loads
# at module scope and aggregates inline; we reproduce load_raw_data() + the
# per-inspdate/species mean. "All" view = zone 1 (the app's main chart).
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["mosquito-monitoring"]] <- list(
  views = c("All", "Compare"),
  default_view = "All",
  params = list(
    facility    = list(type = "string", default = "All"),
    species     = list(type = "csv",    default = "Total_Ae_+_Cq"),
    years       = list(type = "string", default = NULL),
    facilityONE = list(type = "string", default = "All"),
    speciesONE  = list(type = "csv",    default = "Total_Ae_+_Cq"),
    zoneONE     = list(type = "enum", allowed = c("1", "2+X", "All"), default = "All"),
    yearsONE    = list(type = "string", default = NULL)
  )
)
.mm_series <- function(m0, facility, species, years_param, zones) {
  if (!is.data.frame(m0) || nrow(m0) == 0) return(data.frame())
  if (!identical(facility, "All") && "facility" %in% names(m0))
    m0 <- m0[which(m0$facility %in% facility), , drop = FALSE]
  yr <- .embed_resolve_years(list(year_range = years_param))
  keep <- rep(TRUE, nrow(m0))
  if ("Year" %in% names(m0)) keep <- keep & m0$Year >= yr$start & m0$Year <= yr$end
  if ("spp_name" %in% names(m0)) keep <- keep & m0$spp_name %in% species
  if (!is.null(zones) && "zone" %in% names(m0)) keep <- keep & m0$zone %in% zones
  m0 <- m0[which(keep), , drop = FALSE]
  if (nrow(m0) == 0 || !all(c("inspdate", "spp_name", "mosqcount") %in% names(m0))) return(data.frame())
  g <- dplyr::summarise(dplyr::group_by(m0, inspdate, spp_name),
                        avg = round(mean(mosqcount, na.rm = TRUE), 1), .groups = "drop")
  as.data.frame(g)
}
.mosquito_monitoring_all_producer <- function(p) {
  env <- load_app_env("mosquito-monitoring", c("data_functions.R"))
  if (is.null(env) || is.null(env$load_raw_data)) return(NULL)
  m0 <- tryCatch(env$load_raw_data(), error = function(e) NULL)
  g <- .mm_series(m0, p$facility %||% "All", p$species %||% "Total_Ae_+_Cq", p$years, c(1))
  if (nrow(g) == 0) return(.embed_desc(data.frame(), "inspdate", "avg"))
  .embed_desc(g, x_col = "inspdate", y_col = "avg", group_col = "spp_name",
              x_label = "Date", y_label = "Avg per trap (Zone 1)",
              resolved_options = list(view = "All", graph_type = "line"))
}
.mosquito_monitoring_compare_producer <- function(p) {
  env <- load_app_env("mosquito-monitoring", c("data_functions.R"))
  if (is.null(env) || is.null(env$load_raw_data)) return(NULL)
  m0 <- tryCatch(env$load_raw_data(), error = function(e) NULL)
  z <- switch(p$zoneONE %||% "All", "1" = c(1), "2+X" = c(2, "X"), NULL)
  g <- .mm_series(m0, p$facilityONE %||% "All", p$speciesONE %||% "Total_Ae_+_Cq", p$yearsONE, z)
  if (nrow(g) == 0) return(.embed_desc(data.frame(), "inspdate", "avg"))
  .embed_desc(g, x_col = "inspdate", y_col = "avg", group_col = "spp_name",
              x_label = "Date", y_label = "Avg per trap",
              resolved_options = list(view = "Compare", zone = p$zoneONE %||% "All",
                                      graph_type = "line"))
}
EMBED_PRODUCERS[["mosquito-monitoring"]] <- list(
  All     = .mosquito_monitoring_all_producer,
  Compare = .mosquito_monitoring_compare_producer,
  default = .mosquito_monitoring_all_producer
)
