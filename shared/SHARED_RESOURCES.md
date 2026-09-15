# Shared Resources

This folder contains shared utilities and resources used across all apps in the mmcd_metrics project.

## Files

### Database & Data Helpers
- **`db_helpers.R`** - Common database connection and utility functions
- **`db_pool.R`** - Database connection pooling with pool package (v1.0.4)

### Cache System
- **`cache_utilities.R`** - Dynamic cache management for historical averages (registry-driven)
- **`cache/`** - Cache data files
  - `historical_averages_cache.rds` - Pre-calculated 5yr/10yr historical averages

### Themes & Colors
- **`color_themes.R`** - Centralized color theme definitions for consistent UI styling

### Accessibility (ADA / WCAG 2.1 AA)
- **`accessibility_helpers.R`** - Shared accessibility layer for every app. See
  "Quick Start - Accessibility" below. **New apps must use this** - it is what keeps
  the platform ADA compliant.

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
  - See `Q_to_R/GEOSPATIAL_EXTRACTION.md` for detailed usage instructions

### Documentation Sync System
- **`sync_all_docs.R`** - Universal documentation sync script (main logic)
- **`sync_all_docs.ps1`** - PowerShell wrapper with enhanced UI
- **`sync_all_docs.bat`** - Windows batch file for double-click execution  
- **`sync_all_docs.sh`** - Git Bash/Linux wrapper with R auto-detection
- **`README_UNIVERSAL_SYNC.md`** - Detailed documentation for the sync system

## Quick Start - Accessibility

`accessibility_helpers.R` is the single place the platform's ADA/WCAG compliance
lives. Adopting it in an app is **one rename** at the outermost page constructor.

### Standard app (`fluidPage`)

```r
source("../../shared/accessibility_helpers.R")   # next to the other shared sources

ui <- accessible_page(                 # was: fluidPage(
  get_universal_text_css(),
  titlePanel("My App"),
  sidebarLayout(
    sidebarPanel(...),
    accessible_main_panel(...)         # was: mainPanel(  -- optional, see below
  )
)
```

### Dashboard app (`shinydashboard`)

```r
source("../../shared/accessibility_helpers.R")

ui <- accessible_dashboard_page(       # was: dashboardPage(
  dashboardHeader(...),
  dashboardSidebar(...),
  dashboardBody(...)                   # unchanged - do NOT restructure it
)
```

### What the rename gives you

| WCAG criterion | Handled by |
|---|---|
| 2.4.1 Bypass Blocks | "Skip to main content" link, rendered first in the body |
| 1.4.11 Non-text Contrast | `#55606B` borders on inputs/neutral buttons (6.4:1 on white) |
| 2.4.7 Focus Visible | 3px focus ring on every interactive element |
| 3.1.1 Language of Page | `lang="en"` on `<html>` |
| 1.3.1 Info and Relationships | `<main>` landmark (explicit, or auto-assigned at runtime) |

### `accessible_main_panel()` is optional

The skip link locates the main region **at runtime** via a fallback chain
(`#main-content` -> `<main>` -> `[role=main]` -> `.content-wrapper` -> `.tab-content`
-> mainPanel column -> first heading), skipping hidden tabs. So the bare
`accessible_page()` rename already works. Use `accessible_main_panel()` when an app
has exactly one `mainPanel` and you want an exact, explicit target.

**Do not** use it where it would create a duplicate `id="main-content"` (an app with
several `mainPanel`s across tabs) or where `mainPanel` carries its own `class=`
(the class belongs on the panel, not the landmark). Those apps rely on the runtime
fallback - `cattail_inspections` and `section-cards` are the existing examples.

### Other components

- `skip_link(target_id, label)` - the link itself, if you build a page shell by hand
- `main_landmark(..., id)` - wrap any region in `<main>`
- `accessibility_head()` - the CSS + JS bundle for a hand-built shell
- `get_accessibility_css(border_color, focus_color)` - override the design tokens

### Writing accessible UI in a new app

- Always pass a real `label` to `selectInput`/`textInput`/etc. Shiny turns it into
  `<label for="...">` automatically. If a label must not be seen, hide it rather than
  dropping it: `label = tags$span(class = "sr-only", "Task for Jane on Monday")`.
  (`.sr-only` is defined here, so it works on Bootstrap 3 and 5 alike.)
- Give every `img()`/`tags$img()` a concise `alt`, or `alt = ""` if purely decorative.
  Never use a filename or a bare number as alt text.
- Filled buttons (`.btn-primary` etc.) are deliberately left alone - their background
  already delineates the hit area. A neutral/unfilled button gets the shared border.

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

See `Q_to_R/GEOSPATIAL_EXTRACTION.md` for complete geospatial data extraction documentation.

## Requirements

- **R**: The sync system requires R to be installed
- **Windows**: PowerShell execution may need to be enabled for `.ps1` files
- **Location**: Scripts must be run from the `shared/` folder

## See Also

- `README_UNIVERSAL_SYNC.md` - Complete documentation sync system guide
- Individual app folders for app-specific documentation