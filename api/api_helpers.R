# =============================================================================
# API Shared Helpers — Validation, Auth, Error Formatting
# =============================================================================
# Sourced by both the main plumber.R and all route sub-routers.
# Contains input validation, auth, and error helpers.
# =============================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ── Input Validation ──

clean_text <- function(value, max_chars = 32L) {
  if (is.null(value) || !nzchar(trimws(value %||% ""))) return(NULL)
  s <- trimws(as.character(value))
  if (nchar(s) > max_chars) stop(paste0("value too long (max ", max_chars, " chars)"))
  if (!grepl("^[A-Za-z0-9 _+.-]+$", s)) stop("value contains invalid characters")
  s
}

clean_limit <- function(v, default = 500L, max_val = 5000L) {
  n <- suppressWarnings(as.integer(v %||% default))
  if (is.na(n) || n < 1L) n <- default
  if (n > max_val) n <- max_val
  n
}

validate_facility <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return(NULL)
  s <- trimws(as.character(v))
  if (nchar(s) > 8L || !grepl("^[A-Za-z0-9]+$", s)) stop("invalid facility code")
  lkp <- tryCatch(get_facility_lookup(), error = function(e) NULL)
  if (!is.null(lkp) && nrow(lkp) > 0) {
    match_row <- lkp[tolower(lkp$short_name) == tolower(s), ]
    if (nrow(match_row) == 0) stop(paste0("unknown facility: ", s))
    return(match_row$short_name[1])
  }
  s
}

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

validate_zone <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return(c("1", "2"))
  parts <- trimws(unlist(strsplit(as.character(v), ",", fixed = TRUE)))
  bad <- parts[!parts %in% c("1", "2")]
  if (length(bad) > 0) stop(paste0("zone must be 1 or 2, got: ", paste(bad, collapse = ",")))
  parts
}

validate_date <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return(Sys.Date())
  d <- tryCatch(as.Date(trimws(v), "%Y-%m-%d"), error = function(e) as.Date(NA))
  if (is.na(d)) stop("date must be YYYY-MM-DD")
  if (d > Sys.Date() + 1L || d < Sys.Date() - 730L) stop("date out of allowed range")
  d
}

validate_lookback <- function(v) {
  n <- suppressWarnings(as.integer(v %||% 2L))
  if (is.na(n) || n < 1L || n > 14L) stop("lookback_days must be 1-14")
  n
}

validate_priority <- function(v) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return(NULL)
  valid <- c("RED", "YELLOW", "BLUE", "GREEN", "PURPLE")
  parts <- trimws(toupper(unlist(strsplit(as.character(v), ",", fixed = TRUE))))
  bad <- parts[!parts %in% valid]
  if (length(bad) > 0) stop(paste0("invalid priority: ", paste(bad, collapse = ",")))
  parts
}

# Validate group_by against an app's allowed dimensions. Empty -> default (first allowed).
validate_group_by <- function(v, allowed, default = allowed[1]) {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return(default)
  s <- tolower(trimws(as.character(v)))
  if (!s %in% allowed) {
    stop(paste0("group_by must be one of: ", paste(allowed, collapse = ", ")))
  }
  s
}

# Validate a comma-separated set of single-letter codes (e.g. structure_type/status D,W,U).
# Returns "all" when omitted (matching the apps' own default), else an uppercase vector.
validate_type_codes <- function(v, valid, label = "type") {
  if (is.null(v) || !nzchar(trimws(v %||% ""))) return("all")
  parts <- trimws(toupper(unlist(strsplit(as.character(v), ",", fixed = TRUE))))
  bad <- parts[!parts %in% valid]
  if (length(bad) > 0) stop(paste0("invalid ", label, ": ", paste(bad, collapse = ",")))
  parts
}

safe_in <- function(con, col, vals, prefix = "AND ") {
  if (is.null(vals) || length(vals) == 0) return("")
  quoted <- vapply(vals, function(v) as.character(DBI::dbQuoteString(con, v)), character(1))
  paste0(prefix, col, " IN (", paste(quoted, collapse = ", "), ")")
}

api_error <- function(res, status, msg) {
  res$status <- as.integer(status)
  list(error = msg)
}

# Server-side row limit for API responses.
# LLM callers pass limit=200 (default); dashboards can request more.
apply_row_limit <- function(df, limit = NULL, default_limit = 500L, max_limit = 5000L) {
  n <- suppressWarnings(as.integer(limit %||% default_limit))
  if (is.na(n) || n < 1L) n <- default_limit
  if (n > max_limit) n <- max_limit
  total <- nrow(df)
  if (total > n) {
    list(
      count     = total,
      returned  = n,
      truncated = TRUE,
      data      = df[seq_len(n), , drop = FALSE]
    )
  } else {
    list(count = total, data = df)
  }
}

to_choice_list <- function(choices, all_label) {
  out <- lapply(names(choices), function(n) list(label = n, code = unname(choices[[n]])))
  c(list(list(label = all_label, code = "all")), out)
}

# ── Registry defaults ──
# The overview app's metric_registry.R is the SINGLE SOURCE OF DEFAULTS. When an API
# caller omits a value (e.g. expiring_days, priority, species, goal), fall back to that
# metric's registry `load_params`. Route files supply their own filters; the registry
# only fills the blanks. Sourced + cached once per router.
# .registry_env: NULL = not tried yet, FALSE = tried and failed, environment = loaded.
.registry_env <- NULL
registry_default <- function(metric, param, fallback = NULL) {
  if (is.null(.registry_env)) {
    e <- new.env(parent = globalenv())
    ok <- tryCatch({
      source("/srv/shiny-server/apps/overview/metric_registry.R", local = e, chdir = TRUE)
      TRUE
    }, error = function(err) FALSE)
    .registry_env <<- if (ok) e else FALSE
  }
  if (isFALSE(.registry_env)) return(fallback)
  cfg <- tryCatch(.registry_env$get_metric_config(metric), error = function(e) NULL)
  val <- tryCatch(cfg$load_params[[param]], error = function(e) NULL)
  val %||% fallback
}

# ── Generic expiration schedule ──
# Generalizes the catch-basin bucketed-timeline pattern to ANY app whose loader flags
# more sites as "expiring" as the expiring_days window grows (structures, ground
# prehatch, drone, red air, cattail...). `count_fn(n)` is called for each threshold and
# must return list(expiring = <cumulative count expiring within n days>,
#                  active = <total active>, expired = <total already lapsed>) or NULL.
# active/expired are read from the first populated call (threshold-independent).
build_expiration_schedule <- function(count_fn, thresholds = c(14L, 30L, 60L, 90L)) {
  nz <- function(x) max(0L, as.integer(x %||% 0L))
  cumulative   <- setNames(integer(length(thresholds)), as.character(thresholds))
  total_active <- 0L; total_expired <- 0L; have_totals <- FALSE
  for (n in thresholds) {
    r <- count_fn(n)
    if (is.null(r)) next
    cumulative[as.character(n)] <- nz(r$expiring)
    if (!have_totals && !is.null(r$active)) {
      total_active  <- nz(r$active)
      total_expired <- nz(r$expired)
      have_totals   <- TRUE
    }
  }
  c14 <- cumulative["14"]; c30 <- cumulative["30"]
  c60 <- cumulative["60"]; c90 <- cumulative["90"]
  buckets <- list(
    list(label = "next 14 days",   max_days = 14L, count = nz(c14)),
    list(label = "15-30 days",     max_days = 30L, count = nz(c30 - c14)),
    list(label = "31-60 days",     max_days = 60L, count = nz(c60 - c30)),
    list(label = "61-90 days",     max_days = 90L, count = nz(c90 - c60)),
    list(label = "beyond 90 days", max_days = NA,  count = nz(total_active - c90))
  )
  nonzero <- Filter(function(b) b$count > 0, buckets)
  soonest <- if (length(nonzero) > 0) nonzero[[1]] else NULL
  peak <- NULL
  for (b in buckets) if (is.null(peak) || b$count > peak$count) peak <- b
  if (!is.null(peak) && peak$count == 0) peak <- NULL
  list(
    total_active  = as.integer(total_active),
    total_expired = as.integer(total_expired),
    soonest       = soonest,
    peak          = peak,
    buckets       = buckets
  )
}
