# Alternative approach: Export to CSV first, then create shapefiles
# This gives you more control and the ability to inspect/modify the data

library(sf)
library(DBI)
library(dplyr)
library(readr)

# Source the database helpers
source("../../shared/db_helpers.R")

# Create data directory
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

# Function to export spatial data to CSV with WKT geometry
export_to_csv <- function(query, filename, description = "") {
  cat("Exporting", description, "to", filename, "\n")
  
  con <- get_db_connection()
  if (is.null(con)) {
    cat("ERROR: Could not connect to database\n")
    return(NULL)
  }
  
  tryCatch({
    # Get the data with geometry as WKT (Well-Known Text)
    df <- dbGetQuery(con, query)
    
    if (nrow(df) == 0) {
      cat("WARNING: No data found\n")
      dbDisconnect(con)
      return(NULL)
    }
    
    # Save to CSV
    csv_path <- file.path(data_dir, filename)
    write_csv(df, csv_path)
    
    cat("SUCCESS: Exported", nrow(df), "rows to", csv_path, "\n")
    cat("Columns:", paste(names(df), collapse = ", "), "\n\n")
    
    dbDisconnect(con)
    return(df)
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    dbDisconnect(con)
    return(NULL)
  })
}

# Function to convert CSV with WKT geometry to shapefile
csv_to_shapefile <- function(csv_file, shp_file, geom_column = "geometry_wkt", crs = 4326) {
  cat("Converting", csv_file, "to", shp_file, "\n")
  
  tryCatch({
    # Read CSV
    df <- read_csv(file.path(data_dir, csv_file), show_col_types = FALSE)
    
    if (nrow(df) == 0) {
      cat("ERROR: CSV file is empty\n")
      return(FALSE)
    }
    
    # Convert WKT to sf object
    sf_data <- st_as_sf(df, wkt = geom_column, crs = crs)
    
    # Save as shapefile
    shp_path <- file.path(data_dir, shp_file)
    st_write(sf_data, shp_path, delete_dsn = TRUE, quiet = FALSE)
    
    cat("SUCCESS: Created shapefile with", nrow(sf_data), "features\n\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(FALSE)
  })
}

cat("STEP 1: Exporting spatial data to CSV files\n")
cat(rep("=", 50), "\n")

# 1. Export section boundaries to CSV
section_csv_query <- "
SELECT 
  gid,
  sectcode,
  zone,
  facility,
  fosarea,
  ST_X(ST_Transform(ST_Centroid(the_geom), 4326)) as centroid_lon,
  ST_Y(ST_Transform(ST_Centroid(the_geom), 4326)) as centroid_lat,
  ST_AsText(ST_Transform(the_geom, 4326)) as geometry_wkt
FROM public.gis_sectcode
ORDER BY sectcode
"

sections_df <- export_to_csv(
  query = section_csv_query,
  filename = "sections_data.csv",
  description = "Section Boundaries with Centroids"
)

# 2. Export facility boundaries to CSV
facility_csv_query <- "
SELECT 
  facility,
  COUNT(*) as section_count,
  ST_X(ST_Transform(ST_Centroid(ST_Union(the_geom)), 4326)) as centroid_lon,
  ST_Y(ST_Transform(ST_Centroid(ST_Union(the_geom)), 4326)) as centroid_lat,
  ST_AsText(ST_Transform(ST_Union(the_geom), 4326)) as geometry_wkt
FROM public.gis_sectcode
GROUP BY facility
ORDER BY facility
"

facility_df <- export_to_csv(
  query = facility_csv_query,
  filename = "facility_data.csv", 
  description = "Facility Boundaries"
)

# 3. Export zone boundaries to CSV
zone_csv_query <- "
SELECT 
  zone,
  CASE 
    WHEN zone = '1' THEN 'P1'
    WHEN zone = '2' THEN 'P2'
    WHEN zone = '3' THEN 'P3' 
    WHEN zone = '4' THEN 'P4'
    ELSE 'Unknown'
  END as zone_name,
  COUNT(*) as section_count,
  ST_X(ST_Transform(ST_Centroid(ST_Union(the_geom)), 4326)) as centroid_lon,
  ST_Y(ST_Transform(ST_Centroid(ST_Union(the_geom)), 4326)) as centroid_lat,
  ST_AsText(ST_Transform(ST_Union(the_geom), 4326)) as geometry_wkt
FROM public.gis_sectcode
WHERE zone IS NOT NULL
GROUP BY zone
ORDER BY zone
"

zone_df <- export_to_csv(
  query = zone_csv_query,
  filename = "zone_data.csv",
  description = "Zone Boundaries"
)

# 4. Export recent trap locations to CSV
trap_csv_query <- "
SELECT 
  t.ainspecnum,
  t.facility,
  t.survtype,
  CASE 
    WHEN t.survtype = '4' THEN 'Elevated CO2'
    WHEN t.survtype = '5' THEN 'Gravid Trap'
    WHEN t.survtype = '6' THEN 'CO2 Overnight'
    ELSE 'Other'
  END as trap_type_name,
  t.inspdate::date as inspdate,
  t.x as longitude,
  t.y as latitude,
  ST_AsText(ST_Transform(ST_SetSRID(ST_MakePoint(t.x, t.y), 4326), 4326)) as geometry_wkt
FROM public.dbadult_insp_current t
WHERE t.inspdate >= CURRENT_DATE - INTERVAL '90 days'
  AND t.x IS NOT NULL 
  AND t.y IS NOT NULL
  AND t.survtype IN ('4', '5', '6')
ORDER BY t.inspdate DESC, t.ainspecnum
LIMIT 1000
"

trap_df <- export_to_csv(
  query = trap_csv_query,
  filename = "trap_locations.csv",
  description = "Recent Trap Locations"
)

cat("\nSTEP 2: Converting CSV files to shapefiles\n")
cat(rep("=", 50), "\n")

# Convert CSV files to shapefiles
if (!is.null(sections_df)) {
  csv_to_shapefile("sections_data.csv", "sections_boundaries.shp")
}

if (!is.null(facility_df)) {
  csv_to_shapefile("facility_data.csv", "facility_boundaries.shp")
}

if (!is.null(zone_df)) {
  csv_to_shapefile("zone_data.csv", "zone_boundaries.shp")
}

if (!is.null(trap_df)) {
  csv_to_shapefile("trap_locations.csv", "recent_trap_locations.shp")
}

# Summary
cat("PROCESS COMPLETE!\n")
cat(rep("=", 50), "\n")
cat("Files created in:", file.path(getwd(), data_dir), "\n")

if (dir.exists(data_dir)) {
  csv_files <- list.files(data_dir, pattern = "\\.csv$")
  shp_files <- list.files(data_dir, pattern = "\\.shp$")
  
  cat("\nCSV files:", length(csv_files), "\n")
  if (length(csv_files) > 0) cat("  ", paste(csv_files, collapse = ", "), "\n")
  
  cat("\nShapefiles:", length(shp_files), "\n") 
  if (length(shp_files) > 0) cat("  ", paste(shp_files, collapse = ", "), "\n")
}

cat("\nYou can now:\n")
cat("1. Inspect the CSV files to verify data\n")
cat("2. Open the shapefiles in QGIS if needed\n") 
cat("3. Run the trap surveillance app - it will automatically use these files\n")