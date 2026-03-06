# Project Architecture

This platform hosts multiple R Shiny applications in an organized, scalable structure with a **modular architecture** and centralized helper module.

## Directory Structure

```
mmcd_metrics/
├── shared/
│   ├── db_helpers.R              # Centralized hub for all apps
│   ├── db_pool.R                 # Database connection pooling for performance
│   ├── color_themes.R            # Centralized color definitions
│   ├── stat_box_helpers.R        # Reusable stat box UI components
│   ├── server_utilities.R        # Shared server-side utilities
│   ├── geometry_helpers.R        # Geospatial/shapefile loading functions
│   ├── cache_utilities.R         # Dynamic cache management
│   ├── config.R                  # App configuration loader
│   ├── app_libraries.R           # Shared library loading
│   ├── app_cache.R               # Application-level caching
│   ├── redis_cache.R             # Redis cache integration
│   ├── sync_all_docs.R           # Documentation sync (NOTES.md → NOTES.html)
│   ├── sync_all_docs.sh          # Bash wrapper for sync
│   ├── SHARED_RESOURCES.md       # Shared module documentation
│   ├── assets/                   # Shared icons and images
│   │   ├── adult.png             # Favicon and mosquito app icon
│   │   ├── catchbasin.png        # Catch basin status icon
│   │   ├── cattail_background.png # Cattail app icon
│   │   ├── drone.jpg             # Drone treatment icon
│   │   ├── favicon.ico           # Site favicon
│   │   ├── helicopter-solid-full.svg # Air treatment icon
│   │   ├── jedi-order-brands-solid-full.svg # About section icon
│   │   ├── larvae.png            # Inspection coverage and checkback icon
│   │   └── tree-solid-full.svg   # SUCO history icon
│   ├── cache/                    # Cache data files
│   │   └── historical_averages_cache.rds
│   └── Q_to_R/                   # Geospatial data extraction scripts
│       ├── extract_geometries_from_db.R   # Full DB-to-shapefile extraction
│       ├── extract_to_csv_then_shp.R      # Two-stage CSV then shapefile
│       ├── create_section_boundaries.R    # Minimal section boundaries
│       ├── GEOSPATIAL_EXTRACTION.md       # Extraction documentation
│       └── data/                          # Extracted shapefiles
│           ├── facility_boundaries.shp
│           ├── zone_boundaries.shp
│           ├── sections_boundaries.shp
│           ├── recent_trap_locations.shp
│           ├── VectorIndexAreasA2025.shp
│           ├── harborages/{facility}/{emp}_harborages.shp
│           └── air_sites/{facility}/{emp}_airsites.shp
├── apps/
│   ├── about/                    # About page (index.html)
│   ├── air_inspection_checklist/ # Air inspection checklist
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── ui_helper.R
│   │   └── NOTES.md
│   ├── air_sites_simple/         # Air site status tracking
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── historical_functions.R
│   │   ├── ui_helper.R
│   │   └── NOTES.md
│   ├── catch_basin_status/       # Catch basin treatment tracking
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── historical_functions.R
│   │   ├── ui_helper.R
│   │   └── NOTES.md
│   ├── cattail_inspections/      # Cattail inspection progress
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── historical_functions.R
│   │   ├── planned_treatment_functions.R
│   │   ├── progress_functions.R
│   │   └── NOTES.md
│   ├── cattail_treatments/       # Cattail treatment tracking
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── historical_functions.R
│   │   ├── ui_helper.R
│   │   └── NOTES.md
│   ├── control_efficacy/         # Air treatment checkback analysis
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── checkback_functions.R
│   │   ├── efficacy_data_functions.R
│   │   ├── ui_helper.R
│   │   └── NOTES.md
│   ├── drone/                    # Drone treatment tracking
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── historical_functions.R
│   │   ├── ui_helper.R
│   │   └── NOTES.md
│   ├── ground_prehatch_progress/ # Ground prehatch treatment
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── historical_functions.R
│   │   ├── ui_helpers.R
│   │   └── NOTES.md
│   ├── inspections/              # Inspection coverage analysis
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── ui_helper.R
│   │   └── NOTES.md
│   ├── mosquito-monitoring/      # CO2 trap surveillance
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   └── display_functions.R
│   ├── mosquito_surveillance_map/ # Mosquito surveillance mapping
│   │   ├── app.R
│   │   └── shp/
│   ├── overview/                 # Unified overview dashboard
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── dynamic_server.R
│   │   ├── dynamic_ui.R
│   │   ├── generate_cache.R
│   │   ├── historical_cache.R
│   │   ├── historical_functions.R
│   │   ├── metric_registry.R
│   │   ├── url_router.R
│   │   ├── OVERVIEW_FRAMEWORK.md
│   │   └── unified/
│   ├── red_air_legacy/           # Legacy air treatment pipeline
│   │   ├── app.R
│   │   ├── air_status_functions.R
│   │   ├── legacy_air_status_functions.R
│   │   ├── optimized_air_status_functions.R
│   │   └── flow_testing_functions.R
│   ├── section-cards/            # Printable section field cards
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── ui_helper.R
│   │   └── NOTES.md
│   ├── struct_trt/               # Structure treatment tracking
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── historical_functions.R
│   │   ├── ui_helper.R
│   │   └── NOTES.md
│   ├── suco_history/             # SUCO surveillance analysis
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── ui_helpers.R
│   │   └── NOTES.md
│   ├── trap_surveillance/        # Trap surveillance with Vector Index
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── mle_trap_based.R
│   │   ├── ui_helper.R
│   │   └── NOTES.md
│   ├── trap_survillance_test/    # Trap surveillance prototype
│   │   ├── app.R
│   │   ├── data_functions.R
│   │   ├── display_functions.R
│   │   ├── mle_trap_based.R
│   │   └── ui_helper.R
│   └── test-app/                 # Admin/testing utilities
│       └── app.R
├── api/
│   └── filters.R                 # API filter endpoints
├── config/
│   └── app_config.yaml           # Application configuration
├── documents/                    # Technical documentation
│   ├── ARCHITECTURE.md           # This file
│   ├── applications.md           # App descriptions and features
│   ├── setup.md                  # Installation and deployment
│   ├── updating-config.md        # Config update guide
│   ├── DATABASE_SCHEMA_REFERENCE.md
│   ├── CONNECTION_POOLING_PERFORMANCE_REPORT.md
│   ├── OPTIMIZATION_ANALYSIS.md
│   ├── PERFORMANCE_OPTIMIZATION_GUIDE.md
│   ├── LOAD_BALANCER_ARCHITECTURE.md
│   ├── COLOR_THEMES_README.md
│   ├── THEME_CONFIGURATION.md
│   ├── GEOSPATIAL_DATA.md        # Q_to_R shapefile reference
│   └── USER_FEEDBACK_BACKLOG.md
├── lua/                          # OpenResty/Lua routing layer
│   ├── dynamic_route.lua
│   ├── init_route.lua
│   ├── log_route.lua
│   └── proxy_handler.lua
├── tests/                        # Test suite
│   ├── testthat.R                # Main test runner
│   ├── test_stubs.R              # Mock functions
│   ├── TEST_SUITE.md             # Test documentation
│   ├── shared/                   # Shared module tests
│   ├── apps/                     # App-specific tests
│   ├── integration/              # Integration tests
│   ├── regression/               # Regression tests
│   └── manual/                   # Manual test procedures
├── dockerfile                    # Docker container configuration
├── nginx.conf                    # Nginx configuration
├── shiny-server.conf             # Shiny Server configuration
├── startup.sh                    # Container startup script
├── index.html                    # Main landing page
└── README.md                     # Project README (you are here)
```

## Centralized Shared Resources (`shared/`)

All apps source `shared/db_helpers.R` which provides centralized utilities and automatically loads all other shared modules.

**Core Files:**
- **`db_helpers.R`** - Central hub: database connections, facility/FOS lookups, date thresholds, priority choices
- **`color_themes.R`** - **ALL colors in ALL apps come from ONE location!** Change colors here to update everywhere. Supports 6 themes: MMCD (default), IBM, Wong, Tol, Viridis, ColorBrewer
- **`db_pool.R`** - Connection pooling for 22% fewer errors and 81% faster queries
- **`stat_box_helpers.R`** - Reusable UI components for consistent value boxes
- **`geometry_helpers.R`** - Shapefile loading, CRS transforms, map layer helpers
- **`server_utilities.R`** - Shared server-side utilities (zone filtering, facility filtering)
- **`cache_utilities.R`** - Dynamic cache management for historical averages

**Usage in apps:**
```r
source("../../shared/db_helpers.R")  # Loads all shared modules
conn <- get_pool()                    # Get connection pool
palette <- get_theme_palette()        # Get current theme colors
```
