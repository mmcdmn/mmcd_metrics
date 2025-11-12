# QGIS Demo - Trap Surveillance Heat Map

## Overview
This app creates a professional heat map visualization of mosquito trap surveillance data using QGIS Server. It uses the **exact same SQL queries and vector index calculation** as `trap_survillance_test`, but renders the results through QGIS Server for enhanced cartographic presentation.

## Key Features

### Same Data Logic as trap_survillance_test
- Uses identical SQL from `trap_survillance_test` to fetch trap inspection data
- Implements the same k-NN inverse-distance weighted vector index calculation
- Supports the same filters: species, trap types, facility, k-neighbors, analysis date

### QGIS Server Rendering
- Professional heat map visualization with graduated colors (yellow → orange → red)
- Graduated symbol sizes based on vector index values
- Server-side rendering for better performance with large datasets
- WMS tile service for standard web map integration

### GPKG Reference Layers
The app now includes reference layers from GeoPackage files:
- **MMCD_Sections_2025.gpkg** - Current section boundaries (outline only)
- **7counties.gpkg** - Seven-county metro area (boundary outline)
- **MMCD_FacilityArea.gpkg** - MMCD facility boundaries

These layers are rendered by QGIS Server and provide spatial context for the heat map.

## Architecture

### Workflow
1. **Data Query & Processing (R)**
   - Query PostgreSQL for trap inspections using SQL from `trap_survillance_test`
   - Calculate vector index using k-NN algorithm (same as `trap_survillance_test`)
   - Results stored in temporary GPKG file

2. **QGIS Project Generation (R)**
   - Dynamically create QGIS project (.qgs) file
   - Configure heat map symbology with graduated colors
   - Include GPKG reference layers
   - Save project to `/qgis/projects/`

3. **Map Rendering (QGIS Server)**
   - QGIS Server reads project file
   - Renders styled layers as WMS tiles
   - Serves tiles via Apache proxy

4. **Display (Leaflet)**
   - Leaflet map displays WMS layer
   - Interactive markers for sections and traps
   - Layer controls for toggling visibility

### File Structure
```
qgis_demo/
├── app.R                    # Main Shiny application
├── data_functions.R         # SQL queries & vector index calculation (from trap_survillance_test)
├── qgis_helpers.R          # QGIS project generation & WMS URL builder
└── NOTES.md                # This file
```

## Data Functions (from trap_survillance_test)

### `compute_section_vector_index()`
Copied directly from `trap_survillance_test/data_functions.R`. This function:
1. Queries most recent trap inspection per location
2. Joins with species counts
3. Filters by species, trap types, facility, and date
4. Calculates k-NN inverse-distance weighted vector index for each section

**SQL Query Structure:**
```sql
WITH ranked_traps AS (
  -- Rank inspections by date to get most recent per location
  SELECT t.ainspecnum, t.facility, t.x, t.y, t.survtype, t.inspdate,
         ROW_NUMBER() OVER (PARTITION BY t.x, t.y, t.facility ORDER BY t.inspdate DESC) as rn
  FROM public.dbadult_insp_current t
  WHERE t.inspdate::date <= [analysis_date]
    AND t.x IS NOT NULL AND t.y IS NOT NULL
    AND t.survtype IN ([trap_types])
),
latest_traps AS (
  -- Get only most recent inspection per location
  SELECT * FROM ranked_traps WHERE rn = 1
)
-- Join with species counts
SELECT lt.*, COALESCE(SUM(s.cnt), 0) as species_count
FROM latest_traps lt
LEFT JOIN public.dbadult_species_current s ON lt.ainspecnum = s.ainspecnum
GROUP BY lt.ainspecnum, lt.facility, lt.x, lt.y, lt.survtype, lt.inspdate
```

**Vector Index Algorithm:**
For each section:
1. Find k nearest traps
2. Calculate weights: `weight = 1 / distance`
3. Compute: `vector_index = Σ(weight × count) / Σ(weight)`

### `get_species_choices()` & `get_facility_choices()`
Helper functions for UI selectors (same as trap_survillance_test)

## QGIS Helper Functions

### `generate_qgis_heatmap_project()`
Creates a QGIS project file with:
- **Sections heat map layer** - Points with graduated color/size symbology
- **MMCD Sections layer** - Polygon boundaries (transparent fill, outline only)
- **7 Counties layer** - County boundaries
- **Facility Areas layer** - MMCD facility boundaries

The function:
1. Converts section data to sf points
2. Writes to temporary GPKG file
3. Generates QGIS XML with styled layers
4. References shared GPKG files for context layers

### `generate_wms_url()`
Builds WMS GetMap URL for QGIS Server with parameters:
- `MAP`: Path to .qgs project file
- `LAYERS`: Layer names to display
- `BBOX`: Bounding box from data extent
- `CRS`: EPSG:4326 (WGS84)
- `FORMAT`: image/png with transparency

## Docker Configuration

### Dockerfile Changes
Added symlink so QGIS Server can access GPKG files:
```dockerfile
RUN ln -s /srv/shiny-server/shared /qgis/shared && \
    chmod -R 755 /srv/shiny-server/shared/gpkg
```

This allows QGIS Server to read:
- `/srv/shiny-server/shared/gpkg/*.gpkg` files
- Dynamically generated data GPKGs in `/qgis/projects/`

### QGIS Server Setup
Already configured in dockerfile:
- Apache with FastCGI for QGIS Server
- `/qgis/` endpoint for WMS requests
- `/qgis/projects/` directory for project files

## GPKG Layers Included

### MMCD_Sections_2025.gpkg
- Latest section boundaries (2025)
- Rendered as transparent polygons with outline only
- Provides spatial reference for heat map

### 7counties.gpkg
- Seven-county metro area boundaries
- Dark gray outline, no fill
- Shows regional context

### MMCD_FacilityArea.gpkg
- MMCD facility operational areas
- Blue outline, no fill
- Shows facility boundaries

### 2020CensusBlockAnalysis.gpkg
- Census block data (available but not currently used)
- Could be added for demographic context in future

## Usage

### Filters (Same as trap_survillance_test)
- **Analysis Date**: Only traps inspected on/before this date
- **Species Filter**: Select specific species or "All Species"
- **Trap Types**: Multi-select (Elevated CO2, Gravid, CO2 Overnight)
- **k-Neighbors**: Number of nearest traps (1-10, default 4)
- **Facility Filter**: Filter by facility or "All"

### Workflow
1. Select filters in sidebar
2. Click "Refresh Heat Map" button
3. R computes vector index
4. QGIS project generated with styling
5. Map displays with:
   - QGIS-rendered heat map layer (WMS)
   - Interactive section markers (Leaflet)
   - Interactive trap markers (Leaflet)
   - Reference layers (counties, facilities)

### Map Layers
- **Heat Map** - QGIS WMS layer with professional rendering
- **Sections** - Interactive circles colored by vector index
- **Traps** - Blue markers showing trap locations and counts
- **Base Maps** - OSM or CartoDB Positron

## Advantages Over trap_survillance_test

### Professional Cartography
- Graduated symbology with color ramps
- Consistent styling defined in QGIS
- Server-side rendering for performance

### Reference Layers
- County and facility boundaries for context
- Section polygons for spatial reference
- All rendered by QGIS with professional styling

### Scalability
- Server-side rendering handles large datasets
- WMS standard for interoperability
- Can cache tiles for performance

### Visual Distinction
- Different rendering approach than trap_survillance_test
- Heat map symbology vs. simple colored circles
- Additional context layers

## Technical Notes

### Coordinate Systems
- All data in EPSG:4326 (WGS84) for Leaflet compatibility
- QGIS handles CRS transformations
- Distance calculations in EPSG:3857 (Web Mercator)

### File Paths in Docker
- App files: `/srv/shiny-server/apps/qgis_demo/`
- Shared files: `/srv/shiny-server/shared/`
- GPKG files: `/srv/shiny-server/shared/gpkg/`
- QGIS projects: `/qgis/projects/`
- Symlink: `/qgis/shared` → `/srv/shiny-server/shared`

### Performance
- SQL query optimized with window functions
- Vector index calculated in R (vectorized)
- QGIS renders once, serves tiles multiple times
- Temporary GPKGs cleaned up periodically

## Future Enhancements

### Interpolation
- Use QGIS heatmap interpolation (IDW, kernel density)
- Generate continuous surface from points
- More sophisticated than graduated symbols

### Animation
- Time-series heat maps
- Animate vector index over time
- Show trends and patterns

### Additional Layers
- Add trap density layer
- Include treatment areas
- Overlay with census data

### Export
- Export styled maps as PNG/PDF
- Generate reports with maps
- Save project files for QGIS Desktop

## Comparison with trap_survillance_test

| Feature | trap_survillance_test | qgis_demo |
|---------|----------------------|-----------|
| SQL Query | ✓ Same | ✓ Same |
| Vector Index Algorithm | ✓ k-NN IDW | ✓ k-NN IDW |
| Filters | ✓ Species, date, traps, k, facility | ✓ Species, date, traps, k, facility |
| Rendering | Leaflet circles | QGIS WMS + Leaflet |
| Styling | JavaScript color scales | QGIS symbology |
| Reference Layers | None | Counties, facilities, sections |
| Performance | Client-side | Server-side |
| Interoperability | Leaflet-specific | WMS standard |

Both apps provide the same analytical results, but with different visualization approaches.
