# PostgreSQL to R Geospatial Data Export Scripts

This directory contains utilities for extracting geospatial data from PostgreSQL/PostGIS databases and converting it to R-compatible formats (shapefiles, GeoPackage, CSV) without requiring QGIS.

## Purpose

These scripts replace the manual QGIS workflow for creating spatial data files by:
- Connecting directly to PostgreSQL/PostGIS databases
- Extracting geometries and converting coordinate systems
- Creating shapefiles and other geospatial formats for use in R/Shiny apps
- Providing CSV exports for data inspection and validation

## Scripts Overview

### 1. `extract_geometries_from_db.R` ⭐ **RECOMMENDED**
**Purpose**: Direct database-to-shapefile conversion with full automation

**What it does**:
- Extracts section boundaries from `gis_sectcode` table
- Creates facility boundaries by grouping sections  
- Generates zone boundaries (P1, P2)
- Exports recent trap locations from `dbadult_insp_current`
- Transforms all geometries to WGS84 (EPSG:4326)

**Output Files**:
- `sections_boundaries.shp` - Individual section polygons
- `facility_boundaries.shp` - Facility-level polygon boundaries
- `zone_boundaries.shp` - Zone-level polygon boundaries  
- `recent_trap_locations.shp` - Point locations of recent traps

### 2. `extract_to_csv_then_shp.R`
**Purpose**: Two-stage process for data inspection and validation

**What it does**:
- First exports spatial data to CSV with WKT (Well-Known Text) geometry
- Allows manual inspection/editing of CSV files
- Converts CSV files to shapefiles using WKT geometry column
- Includes centroid coordinates for reference

**Output Files**:
- `sections_data.csv` - Section data with geometry as WKT text
- `facility_data.csv` - Facility data with geometry as WKT text
- `zone_data.csv` - Zone data with geometry as WKT text
- `trap_locations.csv` - Trap point data with geometry as WKT text
- Plus corresponding `.shp` files converted from CSV

### 3. `create_section_boundaries.R`
**Purpose**: Minimal extraction for essential section boundaries only

**What it does**:
- Extracts only section boundary polygons
- Creates both modern GeoPackage (.gpkg) and legacy Shapefile (.shp) formats
- Generates section centroid CSV for reference
- Provides summary statistics

**Output Files**:
- `sections_boundaries.gpkg` - Section polygons (GeoPackage format)
- `sections_boundaries.shp` - Section polygons (Shapefile format)
- `section_centroids.csv` - Section centroid coordinates

## How to Run

### Windows Command Line (PowerShell)

Navigate to the Q_to_R directory first:
```powershell
cd "c:\Users\datatech\Documents\mmcd_metrics\shared\Q_to_R"
```

Then run your chosen script:

```powershell
# Option 1: Full extraction (recommended)
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" extract_geometries_from_db.R

# Option 2: CSV-first approach for data inspection
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" extract_to_csv_then_shp.R

# Option 3: Minimal sections only
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" create_section_boundaries.R
```

### From R/RStudio

```r
# Set working directory
setwd("c:/Users/datatech/Documents/mmcd_metrics/shared/Q_to_R")

# Run chosen script
source("extract_geometries_from_db.R")        # Full extraction
# OR
source("extract_to_csv_then_shp.R")          # CSV first approach  
# OR
source("create_section_boundaries.R")        # Minimal extraction
```

### From Windows Explorer
1. Open PowerShell in the Q_to_R folder (Shift + Right-click → "Open PowerShell window here")
2. Run the PowerShell command for your chosen script

## Prerequisites

### R Packages Required
```r
install.packages(c("sf", "DBI", "RPostgres", "dplyr", "readr"))
```

### Database Connection
- Scripts use `get_db_connection()` from `shared/db_helpers.R`
- Ensure database credentials are properly configured
- PostgreSQL database must have PostGIS extension installed
- User must have read access to target tables

### Required Database Tables
- `public.gis_sectcode` - Section boundaries with geometry
- `public.dbadult_insp_current` - Trap inspection data (for trap locations)

## Output Directory Structure

After running any script:
```
Q_to_R/
├── extract_geometries_from_db.R
├── extract_to_csv_then_shp.R  
├── create_section_boundaries.R
├── README.md
└── data/                          # Created by scripts
    ├── sections_boundaries.shp    # Section polygons
    ├── sections_boundaries.gpkg   # Section polygons (GeoPackage)
    ├── facility_boundaries.shp    # Facility polygons
    ├── zone_boundaries.shp        # Zone polygons
    ├── recent_trap_locations.shp  # Trap point locations
    ├── sections_data.csv          # Section data (CSV approach)
    ├── facility_data.csv          # Facility data (CSV approach)
    ├── zone_data.csv             # Zone data (CSV approach)
    ├── trap_locations.csv        # Trap data (CSV approach)
    └── section_centroids.csv     # Centroid coordinates
```

## ✅ Next Steps - You Have Shapefiles!

**Congratulations!** Your shapefiles are now created in the `data/` folder. Here's what to do next:

### Option 1: Use in Trap Surveillance App (Recommended)

Copy the shapefiles to the trap surveillance app:
```powershell
# Copy shapefiles to trap surveillance app
Copy-Item "data\sections_boundaries.*" -Destination "..\..\apps\trap_survillance_test\data\" -Force

# Or copy entire data folder
Copy-Item "data" -Destination "..\..\apps\trap_survillance_test\" -Recurse -Force
```

Then run the trap surveillance app to see your SF-based mapping!

### Option 2: Use in Any Other Shiny App

```powershell
# Copy to specific app (replace APP_NAME)
Copy-Item "data" -Destination "..\..\apps\APP_NAME\" -Recurse -Force
```

### Option 3: Open in QGIS for Visualization

1. Open QGIS
2. Go to **Layer** → **Add Layer** → **Add Vector Layer**
3. Browse to your `Q_to_R\data\` folder  
4. Select any `.shp` file (e.g., `sections_boundaries.shp`)
5. Click **Add** to visualize your data

### Option 4: Use in R Scripts

```r
library(sf)
library(ggplot2)

# Load and plot section boundaries
sections <- st_read("data/sections_boundaries.shp")
ggplot(sections) + geom_sf(aes(fill = zone)) + theme_void()

# Load trap locations
traps <- st_read("data/recent_trap_locations.shp")
ggplot(traps) + geom_sf(aes(color = survtype)) + theme_void()
```

### Verify Your Files

Check what you have:
```powershell
# List all shapefiles created
Get-ChildItem "data\*.shp" | Format-Table Name, Length, LastWriteTime
```

### Quick Test in R

```r
# Quick verification
library(sf)
sections <- st_read("data/sections_boundaries.shp")
cat("Sections loaded:", nrow(sections), "polygons\n")
cat("Zones found:", paste(unique(sections$zone), collapse = ", "), "\n")
plot(sections["zone"], main = "MMCD Sections by Zone")
```

## Using Exported Files

### In Shiny Apps
Copy the generated `data/` folder to your app directory:
```powershell
# Example: Copy to trap surveillance app
Copy-Item "data" -Destination "..\..\apps\trap_survillance_test\" -Recurse -Force
```

### In QGIS
- Load any `.shp` or `.gpkg` file directly in QGIS
- Files are in WGS84 projection (EPSG:4326) for web compatibility
- CSV files can be loaded and joined with geometry if needed

### In R Scripts
```r
library(sf)

# Load section boundaries
sections <- st_read("data/sections_boundaries.shp")
# OR
sections <- st_read("data/sections_boundaries.gpkg")

# Load facility boundaries  
facilities <- st_read("data/facility_boundaries.shp")

# Inspect data
plot(sections["zone"])  # Color by zone
summary(sections)       # Data summary
```

## Troubleshooting

### "Could not connect to database"
1. Check database credentials in `shared/db_helpers.R`
2. Verify PostgreSQL server is running
3. Confirm network connectivity to database server
4. Check user permissions for target tables

### "No data found for query"
1. Verify table names exist: `public.gis_sectcode`, `public.dbadult_insp_current`
2. Check if tables contain data: `SELECT COUNT(*) FROM public.gis_sectcode`
3. Verify geometry columns exist: `the_geom` in `gis_sectcode`

### "PostGIS extension not found" 
1. Database must have PostGIS installed
2. Check with: `SELECT PostGIS_Version();`
3. Install PostGIS if missing (requires admin privileges)

### "Permission denied" errors
1. Database user needs SELECT permissions on target tables
2. Check with database administrator
3. Test permissions: `SELECT * FROM public.gis_sectcode LIMIT 1`

### Files not created in data/ folder
1. Check R console output for specific error messages
2. Verify write permissions in current directory
3. Check disk space availability
4. Run script from correct directory (Q_to_R folder)

## Advanced Usage

### Modifying Queries
Edit the SQL queries in each script to:
- Filter specific facilities or zones
- Add additional attributes from related tables
- Change date ranges for trap data
- Modify coordinate transformations

### Custom Output Formats
Scripts can be modified to export to:
- GeoJSON format: `st_write(data, "file.geojson")`
- KML format: `st_write(data, "file.kml")`
- Different projections: `st_transform(data, target_crs)`

### Batch Processing
Create a master script to run all extractions:
```r
source("extract_geometries_from_db.R")
source("extract_to_csv_then_shp.R") 
source("create_section_boundaries.R")
```