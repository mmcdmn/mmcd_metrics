# Minimal script to create just the section boundaries needed for SF mapping
# This is the essential file for the trap surveillance vector index visualization

library(sf)
library(DBI)
library(readr)

# Source database helpers
source("../../shared/db_helpers.R")

# Create data directory
if (!dir.exists("data")) dir.create("data")

cat("Creating section boundaries for SF mapping...\n")
cat(paste(rep("=", 40), collapse = ""), "\n")

con <- get_db_connection()
if (is.null(con)) {
  stop("Could not connect to database")
}

tryCatch({
  # Get section boundaries with all necessary attributes
  cat("Querying gis_sectcode table...\n")
  
  query <- "
  SELECT 
    gid,
    sectcode,
    zone,
    facility,
    fosarea,
    ST_Transform(the_geom, 4326) as geometry
  FROM public.gis_sectcode
  WHERE the_geom IS NOT NULL
  ORDER BY sectcode
  "
  
  # Read directly as spatial data
  sections_sf <- st_read(con, query = query)
  
  cat("Retrieved", nrow(sections_sf), "section polygons\n")
  
  # Save as GeoPackage (more modern than shapefile)
  gpkg_path <- file.path("data", "sections_boundaries.gpkg")
  st_write(sections_sf, gpkg_path, delete_dsn = TRUE)
  cat("Saved as GeoPackage:", gpkg_path, "\n")
  
  # Also save as shapefile for compatibility
  shp_path <- file.path("data", "sections_boundaries.shp")
  st_write(sections_sf, shp_path, delete_dsn = TRUE)
  cat("Saved as Shapefile:", shp_path, "\n")
  
  # Export centroids to CSV for inspection
  centroids <- st_centroid(sections_sf)
  centroids_df <- centroids %>%
    mutate(
      centroid_lon = st_coordinates(.)[,1],
      centroid_lat = st_coordinates(.)[,2]
    ) %>%
    st_drop_geometry()
  
  write_csv(centroids_df, file.path("data", "section_centroids.csv"))
  cat("Section centroids saved to: data/section_centroids.csv\n")
  
  # Print summary
  cat("\nSUMMARY:\n")
  cat("- Sections:", nrow(sections_sf), "\n")
  cat("- Zones:", length(unique(sections_sf$zone)), "(", paste(sort(unique(sections_sf$zone)), collapse = ", "), ")\n")
  cat("- Facilities:", length(unique(sections_sf$facility)), "(", paste(sort(unique(sections_sf$facility)), collapse = ", "), ")\n")
  
  bbox <- st_bbox(sections_sf)
  cat("- Extent: Lon", round(bbox[1], 3), "to", round(bbox[3], 3), 
      ", Lat", round(bbox[2], 3), "to", round(bbox[4], 3), "\n")
  
  cat("\n✓ Ready to use with trap surveillance SF mapping!\n")
  
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
}, finally = {
  dbDisconnect(con)
})