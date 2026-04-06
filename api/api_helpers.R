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

safe_in <- function(con, col, vals, prefix = "AND ") {
  if (is.null(vals) || length(vals) == 0) return("")
  quoted <- vapply(vals, function(v) as.character(DBI::dbQuoteString(con, v)), character(1))
  paste0(prefix, col, " IN (", paste(quoted, collapse = ", "), ")")
}

api_error <- function(res, status, msg) {
  res$status <- as.integer(status)
  list(error = msg)
}

to_choice_list <- function(choices, all_label) {
  out <- lapply(names(choices), function(n) list(label = n, code = unname(choices[[n]])))
  c(list(list(label = all_label, code = "all")), out)
}
