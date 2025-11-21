# Extract geometry data from PostgreSQL and create shapefiles directly in R
# This script downloads the necessary geometry data and creates .shp files

library(sf)
library(DBI)
library(dplyr)

# Source the database helpers
source("../../shared/db_helpers.R")

# Create data directory if it doesn't exist
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created data directory:", data_dir, "\n")
}

# Function to extract geometries from database and save as shapefile
extract_and_save_geometry <- function(table_name, query, filename, description = "") {
  cat("\n", rep("=", 50), "\n")
  cat("Extracting", description, "from", table_name, "\n")
  cat(rep("=", 50), "\n")
  
  con <- get_db_connection()
  if (is.null(con)) {
    cat("ERROR: Could not connect to database\n")
    return(FALSE)
  }
  
  tryCatch({
    # Read spatial data directly from PostGIS
    sf_data <- st_read(con, query = query, quiet = FALSE)
    
    if (nrow(sf_data) == 0) {
      cat("WARNING: No data found for query\n")
      dbDisconnect(con)
      return(FALSE)
    }
    
    # Ensure we're in WGS84 (EPSG:4326) for web compatibility
    if (st_crs(sf_data)$epsg != 4326) {
      cat("Transforming to WGS84 (EPSG:4326)...\n")
      sf_data <- st_transform(sf_data, 4326)
    }
    
    # Save as shapefile
    output_path <- file.path(data_dir, filename)
    st_write(sf_data, output_path, delete_dsn = TRUE, quiet = FALSE)
    
    cat("SUCCESS: Saved", nrow(sf_data), "features to", output_path, "\n")
    cat("Columns:", paste(names(sf_data), collapse = ", "), "\n")
    
    # Print spatial extent
    bbox <- st_bbox(sf_data)
    cat("Spatial extent:\n")
    cat("  X range:", round(bbox["xmin"], 4), "to", round(bbox["xmax"], 4), "\n")
    cat("  Y range:", round(bbox["ymin"], 4), "to", round(bbox["ymax"], 4), "\n")
    
    dbDisconnect(con)
    return(TRUE)
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    dbDisconnect(con)
    return(FALSE)
  })
}

# 1. Extract section boundaries (main geometry needed for vector index)
cat("Starting geometry extraction...\n")

section_query <- "
SELECT 
  gid,
  sectcode,
  zone,
  facility,
  fosarea,
  ST_Transform(the_geom, 4326) as geometry
FROM public.gis_sectcode
ORDER BY sectcode
"

success1 <- extract_and_save_geometry(
  table_name = "gis_sectcode",
  query = section_query,
  filename = "sections_boundaries.shp",
  description = "Section Boundaries"
)

# 2. Extract facility boundaries (if available in your database)
facility_query <- "
SELECT 
  facility,
  ST_Transform(ST_Union(the_geom), 4326) as geometry
FROM public.gis_sectcode
GROUP BY facility
ORDER BY facility
"

success2 <- extract_and_save_geometry(
  table_name = "gis_sectcode (grouped by facility)",
  query = facility_query, 
  filename = "facility_boundaries.shp",
  description = "Facility Boundaries"
)

# 3. Extract zone boundaries
zone_query <- "
SELECT 
  zone,
  CASE 
    WHEN zone = '1' THEN 'P1'
    WHEN zone = '2' THEN 'P2' 
    WHEN zone = '3' THEN 'P3'
    WHEN zone = '4' THEN 'P4'
    ELSE 'Unknown'
  END as zone_name,
  ST_Transform(ST_Union(the_geom), 4326) as geometry
FROM public.gis_sectcode
WHERE zone IS NOT NULL
GROUP BY zone
ORDER BY zone
"

success3 <- extract_and_save_geometry(
  table_name = "gis_sectcode (grouped by zone)",
  query = zone_query,
  filename = "zone_boundaries.shp", 
  description = "Zone Boundaries"
)

# 4. Create a sample of trap locations (recent traps for context)
trap_query <- "
SELECT 
  t.ainspecnum,
  t.facility,
  t.survtype,
  t.inspdate::date as inspdate,
  ST_Transform(ST_SetSRID(ST_MakePoint(t.x, t.y), 4326), 4326) as geometry
FROM public.dbadult_insp_current t
WHERE t.inspdate >= CURRENT_DATE - INTERVAL '90 days'
  AND t.x IS NOT NULL 
  AND t.y IS NOT NULL
  AND t.survtype IN ('4', '5', '6')
ORDER BY t.inspdate DESC, t.ainspecnum
LIMIT 1000
"

success4 <- extract_and_save_geometry(
  table_name = "dbadult_insp_current", 
  query = trap_query,
  filename = "recent_trap_locations.shp",
  description = "Recent Trap Locations"
)

# Summary
cat("\n", rep("=", 60), "\n")
cat("EXTRACTION SUMMARY\n")
cat(rep("=", 60), "\n")
cat("Section boundaries:", if(success1) "SUCCESS" else "FAILED", "\n")
cat("Facility boundaries:", if(success2) "SUCCESS" else "FAILED", "\n") 
cat("Zone boundaries:", if(success3) "SUCCESS" else "FAILED", "\n")
cat("Trap locations:", if(success4) "SUCCESS" else "FAILED", "\n")

if (success1) {
  cat("\nREADY TO USE!\n")
  cat("Your section boundaries are now available at:\n")
  cat("  ", file.path(getwd(), data_dir, "sections_boundaries.shp"), "\n")
  cat("\nThe trap surveillance app will automatically load these files.\n")
} else {
  cat("\nERROR: Section boundaries extraction failed.\n")
  cat("Check your database connection and table permissions.\n")
}

cat("\nFiles created in directory:", file.path(getwd(), data_dir), "\n")
if (dir.exists(data_dir)) {
  files_created <- list.files(data_dir, pattern = "\\.(shp|dbf|shx|prj)$")
  if (length(files_created) > 0) {
    cat("Files:", paste(files_created, collapse = ", "), "\n")
  }
}