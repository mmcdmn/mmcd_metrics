# Build the routed facility -> section drive-time lookup (run by hand, not by the app)
# =============================================================================
# For every facility and every section it serves, ask an OSRM router (OpenStreetMap
# roads) for the driving time and road distance from the facility point to the
# section centroid, and save the result to
#   apps/capacity_planner/data/facility_section_drivetime.csv
# The app (load_routed_drivetimes) uses this file instead of the straight-line
# estimate. The app's database connection is read-only, so the lookup is a tracked
# CSV; re-run this script when sections or facilities change.
#
# One-time router setup (Docker, Minnesota OpenStreetMap extract):
#   curl -L -o minnesota-latest.osm.pbf \
#     https://download.geofabrik.de/north-america/us/minnesota-latest.osm.pbf
#   docker run --rm -v "$PWD:/data" osrm/osrm-backend osrm-extract -p /opt/car.lua /data/minnesota-latest.osm.pbf
#   docker run --rm -v "$PWD:/data" osrm/osrm-backend osrm-partition /data/minnesota-latest.osrm
#   docker run --rm -v "$PWD:/data" osrm/osrm-backend osrm-customize /data/minnesota-latest.osrm
#   docker run -d --name osrm-mn -p 5000:5000 -v "$PWD:/data" osrm/osrm-backend \
#     osrm-routed --algorithm mld --max-table-size 2000 /data/minnesota-latest.osrm
#
# Run from the project root:
#   Rscript apps/capacity_planner/scripts/build_drivetimes.R [osrm_url] [osm_extract_label]
#
# Notes: OSRM times are free-flow (speed-limit) times with no live traffic.
# Section centroids can fall in wetlands/lakes; OSRM snaps them to the nearest road
# and snap_m records how far that snap was (flagged when > 1 km).
# =============================================================================

suppressPackageStartupMessages({
  library(DBI); library(httr); library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
osrm_url   <- if (length(args) >= 1) args[1] else "http://localhost:5000"
osm_label  <- if (length(args) >= 2) args[2] else "geofabrik minnesota-latest"
out_path   <- "apps/capacity_planner/data/facility_section_drivetime.csv"
chunk_size <- 400          # destinations per OSRM table request (server max-table-size is 2000)
snap_flag_m <- 1000

if (!file.exists("shared/db_helpers.R")) stop("Run from the project root (mmcd_metrics/).")
source("shared/db_helpers.R")

# ---- Inputs from the database ----------------------------------------------
con <- get_db_connection()
on.exit(safe_disconnect(con), add = TRUE)

fac <- dbGetQuery(con, "
  SELECT abbrv AS facility,
         ST_X(ST_Transform(ST_Centroid(the_geom), 4326)) AS lon,
         ST_Y(ST_Transform(ST_Centroid(the_geom), 4326)) AS lat
  FROM gis_facility WHERE the_geom IS NOT NULL")

# Same section universe as load_capacity_sections()
sec <- dbGetQuery(con, "
  SELECT sc.sectcode, sc.facility,
         ST_X(ST_Transform(sc.latloncentroid, 4326)) AS lon,
         ST_Y(ST_Transform(sc.latloncentroid, 4326)) AS lat
  FROM gis_sectcode sc
  WHERE sc.fosarea IS NOT NULL AND sc.fosarea <> '' AND sc.zone IN ('1', '2')
    AND sc.latloncentroid IS NOT NULL")

sec <- sec[sec$facility %in% fac$facility, ]
cat(sprintf("%d sections across %d facilities\n", nrow(sec), length(unique(sec$facility))))

# ---- Router check ------------------------------------------------------------
chk <- tryCatch(GET(paste0(osrm_url, "/route/v1/driving/-93.2650,44.9778;-93.0900,44.9537?overview=false"), timeout(10)),
                error = function(e) NULL)
if (is.null(chk) || status_code(chk) != 200) stop("OSRM router not reachable at ", osrm_url)

# ---- Route facility -> each section centroid -----------------------------
route_chunk <- function(origin, dests) {
  coords <- c(sprintf("%.6f,%.6f", origin$lon, origin$lat),
              sprintf("%.6f,%.6f", dests$lon, dests$lat))
  url <- sprintf("%s/table/v1/driving/%s?sources=0&destinations=%s&annotations=duration,distance",
                 osrm_url, paste(coords, collapse = ";"),
                 paste(seq_len(nrow(dests)), collapse = ";"))
  r <- GET(url, timeout(120))
  if (status_code(r) != 200) stop("OSRM table request failed: HTTP ", status_code(r), " ", substr(content(r, "text"), 1, 200))
  j <- fromJSON(content(r, "text", encoding = "UTF-8"))
  if (!identical(j$code, "Ok")) stop("OSRM error: ", j$code)
  data.frame(
    drive_min   = round(as.numeric(j$durations[1, ]) / 60, 1),
    drive_miles = round(as.numeric(j$distances[1, ]) / 1609.344, 2),
    snap_m      = round(as.numeric(j$destinations$distance), 0)
  )
}

res <- list()
for (f in fac$facility) {
  s <- sec[sec$facility == f, ]
  if (nrow(s) == 0) next
  origin <- fac[fac$facility == f, ]
  idx <- split(seq_len(nrow(s)), ceiling(seq_len(nrow(s)) / chunk_size))
  parts <- lapply(idx, function(i) route_chunk(origin, s[i, ]))
  r <- cbind(facility = f, sectcode = s$sectcode, do.call(rbind, parts))
  res[[f]] <- r
  cat(sprintf("  %-3s %4d sections routed\n", f, nrow(r)))
}
out <- do.call(rbind, res)
out$snap_flag <- !is.na(out$snap_m) & out$snap_m > snap_flag_m
out$built     <- as.character(Sys.Date())
out$osm_extract <- osm_label
out <- out[order(out$facility, out$sectcode), ]

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write.csv(out, out_path, row.names = FALSE)

# ---- Summary ---------------------------------------------------------------
cat(sprintf("\nWrote %s (%d rows)\n", out_path, nrow(out)))
cat(sprintf("Unreachable (NA): %d\n", sum(is.na(out$drive_min))))
cat(sprintf("Snapped > %d m:   %d\n", snap_flag_m, sum(out$snap_flag)))
print(do.call(rbind, lapply(split(out, out$facility), function(d) data.frame(
  facility = d$facility[1], n = nrow(d), median_min = median(d$drive_min, na.rm = TRUE),
  p90_min = round(quantile(d$drive_min, 0.9, na.rm = TRUE), 1),
  median_road_mi = median(d$drive_miles, na.rm = TRUE), snap_over_1km = sum(d$snap_flag)))),
  row.names = FALSE)
