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

#' Map descriptor: points from lat/lon (+ optional value/label) columns.
.embed_desc_map <- function(df, lat_col, lon_col, value_col = NULL, label_col = NULL,
                            x_label = "", y_label = "", resolved_options = list()) {
  list(kind = "map", df = df, lat_col = lat_col, lon_col = lon_col,
       value_col = value_col, label_col = label_col,
       x_label = x_label, y_label = y_label, resolved_options = resolved_options)
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
  views = c("graph", "top_locations"),
  default_view = "graph",
  params = list(
    facility           = list(type = "string",  default = "all"),
    zone               = list(type = "enum",    allowed = c("all","1","2"), default = "all"),
    fos                = list(type = "csv",      default = NULL),
    group_by           = list(type = "enum",    allowed = c("mmcd_all","facility","foreman","species_name"), default = "mmcd_all"),
    species            = list(type = "string",  default = "All"),
    graph_type         = list(type = "enum",    allowed = c("stacked_bar","bar","line","point","area"), default = "stacked_bar"),
    top_locations_mode = list(type = "enum",    allowed = c("visits","species"), default = "visits"),
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

EMBED_PRODUCERS[["suco_history"]] <- list(
  graph         = .suco_graph_producer,
  top_locations = .suco_top_locations_producer,
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
    df <- do.call(env[[fn_name]], args)
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
  views = c("historical"),
  default_view = "historical",
  params = list(
    facility       = list(type = "string",  default = "all"),
    zone           = list(type = "enum", allowed = c("1","2","1,2","combined"), default = "combined"),
    fos            = list(type = "csv",     default = NULL),
    group_by       = list(type = "enum", allowed = c("mmcd_all","facility","foreman","sectcode"), default = "mmcd_all"),
    time_period    = list(type = "enum", allowed = c("yearly","weekly"), default = "yearly"),
    display_metric = list(type = "enum", allowed = c("sites","acres","treatment_acres"), default = "treatment_acres"),
    chart_type     = list(type = "enum", allowed = c("stacked_bar","grouped_bar","line","area"), default = "stacked_bar"),
    year_range     = list(type = "string", default = NULL),
    include_drone  = list(type = "logical", default = TRUE)
  )
)
EMBED_PRODUCERS[["ground_prehatch_progress"]] <- local({
  prod <- make_historical_producer(
    "ground_prehatch_progress",
    extra_fn = function(p) list(include_drone = if (is.null(p$include_drone)) TRUE else isTRUE(p$include_drone)),
    default_metric = "treatment_acres")
  list(historical = prod, default = prod)
})

# ---------------------------------------------------------------------------
# drone (historical trend)
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["drone"]] <- list(
  views = c("historical"),
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
    prehatch_only  = list(type = "logical", default = FALSE)
  )
)
EMBED_PRODUCERS[["drone"]] <- local({
  prod <- make_historical_producer(
    "drone",
    extra_fn = function(p) list(prehatch_only = isTRUE(p$prehatch_only)),
    default_metric = "treatment_acres")
  list(historical = prod, default = prod)
})

# ---------------------------------------------------------------------------
# catch_basin_status (historical trend; fn create_historical_cb_data ->
# time_period / count / group_name)
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["catch_basin_status"]] <- list(
  views = c("historical"),
  default_view = "historical",
  params = list(
    facility       = list(type = "string",  default = "all"),
    zone           = list(type = "enum", allowed = c("1","2","1,2","combined"), default = "combined"),
    fos            = list(type = "csv",     default = NULL),
    group_by       = list(type = "enum", allowed = c("facility","foreman","sectcode","mmcd_all"), default = "mmcd_all"),
    time_period    = list(type = "enum", allowed = c("yearly","weekly"), default = "yearly"),
    display_metric = list(type = "enum", allowed = c("treatments","sites","total_count","weekly_active_treatments"), default = "treatments"),
    chart_type     = list(type = "enum", allowed = c("stacked_bar","grouped_bar","line","area"), default = "stacked_bar"),
    year_range     = list(type = "string", default = NULL)
  )
)
EMBED_PRODUCERS[["catch_basin_status"]] <- local({
  prod <- make_historical_producer(
    "catch_basin_status", fn_name = "create_historical_cb_data",
    default_metric = "treatments", out_y = "count", out_group = "group_name")
  list(historical = prod, default = prod)
})

# ---------------------------------------------------------------------------
# struct_trt (historical trend; fn create_historical_struct_data ->
# time_period / count / group_name; extra structure_type + status_types)
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["struct_trt"]] <- list(
  views = c("historical"),
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
    year_range     = list(type = "string", default = NULL)
  )
)
EMBED_PRODUCERS[["struct_trt"]] <- local({
  prod <- make_historical_producer(
    "struct_trt", fn_name = "create_historical_struct_data",
    default_metric = "treatments", out_y = "count", out_group = "group_name",
    extra_fn = function(p) list(
      structure_type_filter = p$structure_type,
      status_types = if (is.null(p$status_types)) c("D","W","U") else p$status_types))
  list(historical = prod, default = prod)
})

# ---------------------------------------------------------------------------
# cattail_inspections (Historical Comparison: per-facility baseline vs current)
# get_historical_progress_data -> facility_display / type / site_count / acre_count
# ---------------------------------------------------------------------------
EMBED_PARAM_SPEC[["cattail_inspections"]] <- list(
  views = c("historical"),
  default_view = "historical",
  params = list(
    hist_zone     = list(type = "enum", allowed = c("p1","p2","combined","separate"), default = "combined"),
    hist_years    = list(type = "int",    default = 3L),
    hist_facility = list(type = "csv",     default = "all"),
    hist_metric   = list(type = "enum", allowed = c("sites","acres"), default = "sites")
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
EMBED_PRODUCERS[["cattail_inspections"]] <- list(
  historical = .cattail_insp_historical_producer,
  default    = .cattail_insp_historical_producer
)
