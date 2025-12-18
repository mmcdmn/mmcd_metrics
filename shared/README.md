# Shared Resources

This folder contains shared utilities and resources used across all apps in the mmcd_metrics project.

## Files

### Database Helpers
- **`db_helpers.R`** - Common database connection and utility functions
- **`db_pool.R`** - Database connection pooling with pool package (v1.0.4)
- **`color_themes.R`** - Centralized color theme definitions for consistent UI styling

### Static Assets
- **`assets/`** - Images, icons, and static resources for apps and landing pages
  - `adult.png` - Favicon and mosquito app icon
  - `catchbasin.png` - Catch basin status icon
  - `cattail_background.png` - Cattail app icon
  - `drone.jpg` - Drone treatment icon
  - `favicon.ico` - Site favicon
  - `helicopter-solid-full.svg` - Air treatment icon
  - `jedi-order-brands-solid-full.svg` - About section icon
  - `larvae.png` - Inspection coverage and checkback icon
  - `tree-solid-full.svg` - SUCO history icon

### Geospatial Data Export (Q_to_R)
- **`Q_to_R/`** - PostgreSQL to R geospatial data extraction scripts
  - `extract_geometries_from_db.R` - Direct database to shapefile conversion
  - `extract_to_csv_then_shp.R` - Two-stage CSV then shapefile export
  - `create_section_boundaries.R` - Minimal section boundaries extraction
  - `data/` - Extracted shapefiles including VectorIndexAreasA2025.shp (12 grouped areas from 3207 sections)
  - See `Q_to_R/README.md` for detailed usage instructions

### Documentation Sync System
- **`sync_all_docs.R`** - Universal documentation sync script (main logic)
- **`sync_all_docs.ps1`** - PowerShell wrapper with enhanced UI
- **`sync_all_docs.bat`** - Windows batch file for double-click execution  
- **`sync_all_docs.sh`** - Git Bash/Linux wrapper with R auto-detection
- **`README_UNIVERSAL_SYNC.md`** - Detailed documentation for the sync system

## Quick Start - Documentation Sync

The universal sync system automatically converts all `NOTES.md` files to `NOTES.html` across all apps.

### Windows (Recommended)
```powershell
# From shared/ folder
.\sync_all_docs.ps1              # Sync changed files
.\sync_all_docs.ps1 -Status      # Check what needs updating
.\sync_all_docs.ps1 -Force       # Rebuild all HTML files
```

### Alternative Methods
```bash
# Double-click sync_all_docs.bat in Windows Explorer

# Or use Git Bash
./sync_all_docs.sh --status

# Or run R directly
Rscript sync_all_docs.R

# Run on windows
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" sync_all_docs.R --status
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" sync_all_docs.R --Force

# out with the old, in with the new
cd "c:\Users\datatech\Documents\mmcd_metrics\shared"; Remove-Item "c:\Users\datatech\Documents\mmcd_metrics\apps\cattail\NOTES.html" -Force; & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" sync_all_docs.R --force
```

### Geospatial Data Extraction (Q_to_R)
```powershell
# Navigate to Q_to_R directory
cd "c:\Users\datatech\Documents\mmcd_metrics\shared\Q_to_R"

# Run geospatial data extraction (choose one)
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" extract_geometries_from_db.R     # Full extraction
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" extract_to_csv_then_shp.R       # CSV first approach
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" create_section_boundaries.R     # Minimal extraction
```

### Current Apps with Documentation
- `air_sites_simple/`
- `catch_basin_status/`
- `cattail_inspections/`
- `cattail_treatments/`
- `control_efficacy/`
- `drone/`
- `ground_prehatch_progress/`
- `inspections/`
- `section-cards/`
- `struct_trt/`
- `suco_history/`
- `trap_survillance_test/`

The system automatically discovers new apps when you add `NOTES.md` files - no configuration needed!

### Current Q_to_R Scripts
- `extract_geometries_from_db.R` - PostgreSQL to shapefile conversion
- `extract_to_csv_then_shp.R` - CSV export with shapefile conversion
- `create_section_boundaries.R` - Essential section boundaries only

See `Q_to_R/README.md` for complete geospatial data extraction documentation.

## Requirements

- **R**: The sync system requires R to be installed
- **Windows**: PowerShell execution may need to be enabled for `.ps1` files
- **Location**: Scripts must be run from the `shared/` folder

## See Also

- `README_UNIVERSAL_SYNC.md` - Complete documentation sync system guide
- Individual app folders for app-specific documentation