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
    if (!exists("get_app_config", mode = "function") && file.exists("../../shared/config.R")) {
      source("../../shared/config.R")
    }
    cfg <- tryCatch(get_app_config()$capacity_planner$osrm_url, error = function(e) NULL)
    u <- if (is.character(cfg) && length(cfg) == 1 && !is.na(cfg)) cfg else ""
  }
  sub("/+$", "", trimws(u))
}

# Where the router is looked for when nothing is configured: from inside the dashboard container the
# machine's own OSRM (Docker Desktop's host.docker.internal), a router run directly on this machine, and
# a compose service called "osrm".
OSRM_DEFAULT_URLS <- c("http://host.docker.internal:5000", "http://localhost:5000", "http://osrm:5000")

#' The addresses to try: the configured one only when there is one (it wins and is not second-guessed),
#' otherwise the defaults.
capacity_osrm_candidates <- function(configured = capacity_osrm_url()) {
  if (nzchar(configured)) configured else OSRM_DEFAULT_URLS
}

OSRM_DEFAULT_API_KEY <- "mmcd-sheets-abc123xyz"

osrm_api_key <- function() {
  keys <- trimws(unlist(strsplit(Sys.getenv("API_KEYS", ""), ",", fixed = TRUE)))
  keys <- keys[nzchar(keys)]
  if (length(keys)) keys[[1]] else OSRM_DEFAULT_API_KEY
}

#' Headers to send to the router at `url`: the key for https, nothing for a local http router.
osrm_request_headers <- function(url) {
  if (grepl("^https://", url, ignore.case = TRUE)) c("X-API-Key" = osrm_api_key()) else character(0)
}

# HTTP GET behind a small interface so tests can stand in for the router
.osrm_http_get <- function(url, timeout_s = 120) {
  h <- osrm_request_headers(url)
  r <- if (length(h)) httr::GET(url, httr::timeout(timeout_s), httr::add_headers(.headers = h))
       else httr::GET(url, httr::timeout(timeout_s))
  list(status = httr::status_code(r), text = httr::content(r, "text", encoding = "UTF-8"))
}

#' Is the router answering? (a tiny route request)
osrm_reachable <- function(osrm_url, http_get = .osrm_http_get, timeout_s = 5) {
  if (!nzchar(osrm_url)) return(FALSE)
  r <- tryCatch(http_get(paste0(osrm_url, "/route/v1/driving/-93.2650,44.9778;-93.0900,44.9537?overview=false"),
                         timeout_s), error = function(e) NULL)
  !is.null(r) && identical(as.integer(r$status), 200L)
}

#' The first address that answers, or "" when none does.
find_osrm_url <- function(candidates = capacity_osrm_candidates(), http_get = .osrm_http_get, timeout_s = 3) {
  for (u in candidates) if (osrm_reachable(u, http_get, timeout_s)) return(u)
  ""
}

# One /table request: `origin` (lon, lat) to the rows of `dests`
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
    drive_min_routed   = round(as.numeric(unlist(j$durations)) / 60, 1),
    drive_miles_routed = round(as.numeric(unlist(j$distances)) / 1609.344, 2),
    snap_m             = round(as.numeric(j$destinations$distance), 0)
  )
}

#' Route one facility location to every section centroid.
#'
#' @param origin list or one-row data.frame with facility, lon, lat
#' @param dests  data.frame(sectcode, lon, lat), the section centroids
#' @param osrm_url router address
#' @param chunk_size destinations per request
#' @param http_get function(url, timeout_s) -> list(status, text); httr by default
#' @return data.frame(ROUTED_TIME_COLUMNS): one row per destination; a destination the router
#'   cannot reach has NA minutes / miles
osrm_route_facility <- function(origin, dests, osrm_url, chunk_size = OSRM_CHUNK_SIZE,
                                http_get = .osrm_http_get, timeout_s = 120) {
  if (!nzchar(osrm_url)) stop("No routing service is configured")
  lon <- as.numeric(origin$lon); lat <- as.numeric(origin$lat)
  if (length(lon) != 1 || length(lat) != 1 || !is.finite(lon) || !is.finite(lat) ||
      lon < -180 || lon > 180 || lat < -90 || lat > 90) stop("Invalid facility location")
  if (is.null(dests) || nrow(dests) == 0) stop("No sections to route to")
  fac <- if (is.null(origin$facility)) NA_character_ else as.character(origin$facility)[1]
  origin <- list(lon = lon, lat = lat)
  idx <- split(seq_len(nrow(dests)), ceiling(seq_len(nrow(dests)) / chunk_size))
  parts <- lapply(idx, function(i) .osrm_table_chunk(origin, dests[i, , drop = FALSE], osrm_url,
                                                      http_get, timeout_s))
  r <- do.call(rbind, parts)
  data.frame(
    facility = fac, sectcode = as.character(dests$sectcode),
    r,
    snap_flag = !is.na(r$snap_m) & r$snap_m > OSRM_SNAP_FLAG_M,
    straight_miles = round(route_haversine_miles(lon, lat, dests$lon, dests$lat), 2),
    stringsAsFactors = FALSE, row.names = NULL
  )[, ROUTED_TIME_COLUMNS]
}
