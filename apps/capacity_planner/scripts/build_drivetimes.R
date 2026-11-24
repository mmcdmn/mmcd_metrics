# Build the routed facility -> section drive-time lookup (run by hand, not by the app)
# =============================================================================
# For EVERY facility and EVERY section (not just the section's own facility, so the
# What-If planner can move a section to another facility), ask an OSRM router
# (OpenStreetMap roads) for the driving time and road distance from each facility office
# to the section centroid, and save the result to
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
locations_path <- "apps/capacity_planner/data/facility_locations.csv"
snap_flag_m <- 1000        # (the routing code flags a centroid snapped further than this)

if (!file.exists("shared/db_helpers.R")) stop("Run from the project root (mmcd_metrics/).")
source("shared/db_helpers.R")
# The routing request itself lives in routing_functions.R: the What-If planner uses the same code to
# recalculate drive times when a facility dot is moved or Facility X is pinned.
source("apps/capacity_planner/routing_functions.R")

# ---- Inputs from the database ----------------------------------------------
con <- get_db_connection()
on.exit(safe_disconnect(con), add = TRUE)

if (!file.exists(locations_path)) stop("Missing facility locations file: ", locations_path)
fac <- read.csv(locations_path, stringsAsFactors = FALSE)
required_locations <- c("facility", "lon", "lat")
if (!all(required_locations %in% names(fac)) ||
    anyDuplicated(fac$facility) ||
    any(!is.finite(fac$lon)) || any(!is.finite(fac$lat)) ||
    any(fac$lon < -180 | fac$lon > 180 | fac$lat < -90 | fac$lat > 90)) {
  stop("Invalid facility locations in ", locations_path)
}

# Same section universe as load_capacity_sections()
sec <- dbGetQuery(con, "
  SELECT sc.sectcode, sc.facility,
         ST_X(ST_Transform(sc.latloncentroid, 4326)) AS lon,
         ST_Y(ST_Transform(sc.latloncentroid, 4326)) AS lat
  FROM gis_sectcode sc
  WHERE sc.fosarea IS NOT NULL AND sc.fosarea <> '' AND sc.zone IN ('1', '2')
    AND sc.latloncentroid IS NOT NULL")

missing_fac <- setdiff(unique(sec$facility), fac$facility)
if (length(missing_fac)) {
  stop("No office coordinates in ", locations_path, " for: ",
       paste(missing_fac, collapse = ", "))
}
fac <- fac[fac$facility %in% unique(sec$facility), ]
sec <- sec[sec$facility %in% fac$facility, ]
cat(sprintf("%d sections, %d facilities -> %d facility x section pairs\n",
            nrow(sec), nrow(fac), nrow(sec) * nrow(fac)))

# ---- Router check ------------------------------------------------------------
chk <- tryCatch(GET(paste0(osrm_url, "/route/v1/driving/-93.2650,44.9778;-93.0900,44.9537?overview=false"), timeout(10)),
                error = function(e) NULL)
if (is.null(chk) || status_code(chk) != 200) stop("OSRM router not reachable at ", osrm_url)

# ---- Route each facility office -> each section centroid -----------------
res <- list()
for (f in fac$facility) {
  # every section, from every facility (osrm_route_facility: one /table request per 400 sections)
  r <- osrm_route_facility(fac[fac$facility == f, ], sec, osrm_url)
  names(r)[names(r) == "drive_min_routed"]   <- "drive_min"       # the committed CSV's column names
  names(r)[names(r) == "drive_miles_routed"] <- "drive_miles"
  res[[f]] <- r
  cat(sprintf("  %-3s %4d sections routed\n", f, nrow(r)))
}
out <- do.call(rbind, res)
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
