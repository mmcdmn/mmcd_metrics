# Build data/ra_excluded_sites.csv: active breeding sites inside Restricted Area (RA)
# polygons with monitor_ok = FALSE. No work happens there, so the capacity planner drops
# them from site counts and acres. Offline because the app's DB connection is read-only
# and ra_view.gpkg is a QGIS export.
#
# The RA layer is used ONLY for this spatial test. Its e_restrctspec column holds the
# employee/FOS number of whoever set the restriction (e.g. 1904); it is ignored. Sites stay
# with the FOS of their section (gis_sectcode.fosarea).
#
# Run from the project root:  Rscript apps/capacity_planner/scripts/build_ra_exclusions.R

suppressPackageStartupMessages({ library(DBI); library(sf) })
if (!file.exists("shared/db_helpers.R")) stop("Run from the project root (mmcd_metrics/).")
source("shared/db_helpers.R")

GPKG <- "extra/qgis/ra_view.gpkg"
OUT  <- "apps/capacity_planner/data/ra_excluded_sites.csv"

ra <- st_read(GPKG, layer = "ra_view", quiet = TRUE)
ra <- ra[!is.na(ra$monitor_ok) & ra$monitor_ok == 0 & !st_is_empty(ra), ]
ra <- st_make_valid(ra)
cat(sprintf("RA polygons with monitor_ok = FALSE: %d\n", nrow(ra)))

con <- get_db_connection()
on.exit(safe_disconnect(con), add = TRUE)
sites <- dbGetQuery(con, "
  SELECT b.sitecode, b.air_gnd, b.prehatch, b.acres,
         ST_AsText(ST_Centroid(ST_Transform(b.geom, 26915))) AS wkt
  FROM loc_breeding_sites b
  WHERE b.enddate IS NULL AND b.geom IS NOT NULL")
pts <- st_as_sf(sites, wkt = "wkt", crs = 26915)

hit <- lengths(st_intersects(pts, st_union(ra))) > 0
out <- sites[hit, c("sitecode", "air_gnd", "prehatch", "acres")]
out <- out[order(out$sitecode), ]
out$built <- as.character(Sys.Date())
write.csv(out, OUT, row.names = FALSE)
cat(sprintf("Excluded %d of %d active sites (%.1f acres) -> %s\n",
            nrow(out), nrow(sites), sum(out$acres, na.rm = TRUE), OUT))
