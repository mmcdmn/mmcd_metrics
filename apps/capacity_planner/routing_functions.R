# Road routing for the capacity planner (OSRM, OpenStreetMap roads)
# =============================================================================

OSRM_CHUNK_SIZE <- 400          # destinations per /table request (the server's max-table-size is 2000)
OSRM_SNAP_FLAG_M <- 1000        # a centroid snapped further than this to a road is flagged

# Columns of a routed lookup (the same ones load_routed_drivetimes() returns)
ROUTED_TIME_COLUMNS <- c("facility", "sectcode", "drive_min_routed", "drive_miles_routed",
                         "snap_m", "snap_flag", "straight_miles")

#' Straight-line (great-circle) miles between points; vectorised.
route_haversine_miles <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 3958.7613 * asin(pmin(1, sqrt(a)))
}

#' The router's address: OSRM_URL (environment) wins over capacity_planner.osrm_url (config).
#' @return the URL without a trailing slash, or "" when none is configured
capacity_osrm_url <- function() {
  u <- Sys.getenv("OSRM_URL", "")
  if (!nzchar(u)) {
    cfg <- .capacity_config()$capacity_planner$osrm_url
    u <- if (is.character(cfg) && length(cfg) == 1 && !is.na(cfg)) cfg else ""
  }
  sub("/+$", "", trimws(u))
}

# config/app_config.yaml as a list (shared/config.R is loaded on demand); an empty list when unavailable
.capacity_config <- function() {
  if (!exists("get_app_config", mode = "function") && file.exists("../../shared/config.R")) {
    source("../../shared/config.R")
  }
  cfg <- tryCatch(get_app_config(), error = function(e) NULL)
  if (is.list(cfg)) cfg else list()
}

# HTTP GET behind a small interface so tests can stand in for the router.

.osrm_http_get <- function(url, timeout_s = 120) {
  r <- httr::GET(url, httr::timeout(timeout_s))
  list(status = httr::status_code(r), text = httr::content(r, "text", encoding = "UTF-8"))
}

# One /table request: raw seconds / meters from `origin` (lon, lat) to the rows of `dests`
.osrm_table_chunk <- function(origin, dests, osrm_url, http_get, timeout_s) {
  coords <- c(sprintf("%.6f,%.6f", origin$lon, origin$lat),
              sprintf("%.6f,%.6f", dests$lon, dests$lat))
  url <- sprintf("%s/table/v1/driving/%s?sources=0&destinations=%s&annotations=duration,distance",
                 osrm_url, paste(coords, collapse = ";"),
                 paste(seq_len(nrow(dests)), collapse = ";"))
  r <- http_get(url, timeout_s)
  if (!identical(as.integer(r$status), 200L)) {
    stop("OSRM table request failed: HTTP ", r$status, " ", substr(r$text, 1, 200))
  }
  j <- jsonlite::fromJSON(r$text)
  if (!identical(j$code, "Ok")) stop("OSRM error: ", j$code)
  data.frame(
    # one source row: a matrix for several destinations, a bare vector for one
    duration_s = as.numeric(unlist(j$durations)),
    distance_m = as.numeric(unlist(j$distances)),
    snap_m     = as.numeric(j$destinations$distance)
  )
}

# lon / lat of a facility location, or an error
.osrm_check_origin <- function(origin) {
  lon <- as.numeric(origin$lon); lat <- as.numeric(origin$lat)
  if (length(lon) != 1 || length(lat) != 1 || !is.finite(lon) || !is.finite(lat) ||
      lon < -180 || lon > 180 || lat < -90 || lat > 90) stop("Invalid facility location")
  list(lon = lon, lat = lat)
}

#' The routed rows of one facility location from the router's raw answer.
#'
#' @param origin list or one-row data.frame with facility, lon, lat
#' @param dests  data.frame(sectcode, lon, lat), the section centroids, in the order of the answer
#' @param duration_s,distance_m,snap_m one value per destination: seconds and meters by road, and the meters the
#'   centroid was moved to reach a road. A missing value is NA or negative (the page sends -1).
#' @return data.frame(ROUTED_TIME_COLUMNS), one row per destination; a destination the router cannot reach has
#'   NA minutes / miles
osrm_routes_from_raw <- function(origin, dests, duration_s, distance_m, snap_m) {
  o <- .osrm_check_origin(origin)
  if (is.null(dests) || nrow(dests) == 0) stop("No sections to route to")
  if (!all(lengths(list(duration_s, distance_m, snap_m)) == nrow(dests))) {
    stop("The router's answer does not match the sections")
  }
  clean <- function(x) { x <- suppressWarnings(as.numeric(x)); x[!is.finite(x) | x < 0] <- NA_real_; x }
  snap <- round(clean(snap_m), 0)
  fac <- if (is.null(origin$facility)) NA_character_ else as.character(origin$facility)[1]
  data.frame(
    facility = fac, sectcode = as.character(dests$sectcode),
    drive_min_routed   = round(clean(duration_s) / 60, 1),
    drive_miles_routed = round(clean(distance_m) / 1609.344, 2),
    snap_m = snap,
    snap_flag = !is.na(snap) & snap > OSRM_SNAP_FLAG_M,
    straight_miles = round(route_haversine_miles(o$lon, o$lat, dests$lon, dests$lat), 2),
    stringsAsFactors = FALSE, row.names = NULL
  )[, ROUTED_TIME_COLUMNS]
}

#' Route one facility location to every section centroid from R (used by scripts/build_drivetimes.R).
#'
#' @param origin list or one-row data.frame with facility, lon, lat
#' @param dests  data.frame(sectcode, lon, lat), the section centroids
#' @param osrm_url router address
#' @param chunk_size destinations per request
#' @param http_get function(url, timeout_s) -> list(status, text); httr by default
#' @return data.frame(ROUTED_TIME_COLUMNS), see osrm_routes_from_raw()
osrm_route_facility <- function(origin, dests, osrm_url, chunk_size = OSRM_CHUNK_SIZE,
                                http_get = .osrm_http_get, timeout_s = 30) {
  if (!nzchar(osrm_url)) stop("No routing service is configured")
  o <- .osrm_check_origin(origin)
  if (is.null(dests) || nrow(dests) == 0) stop("No sections to route to")
  idx <- split(seq_len(nrow(dests)), ceiling(seq_len(nrow(dests)) / chunk_size))
  parts <- lapply(idx, function(i) .osrm_table_chunk(o, dests[i, , drop = FALSE], osrm_url, http_get, timeout_s))
  r <- do.call(rbind, parts)
  osrm_routes_from_raw(origin, dests, r$duration_s, r$distance_m, r$snap_m)
}
