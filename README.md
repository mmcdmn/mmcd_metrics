# MMCD Metrics Dashboard

![MMCD Analytics](https://img.shields.io/badge/MMCD-Analytics-blue)
![R Shiny](https://img.shields.io/badge/R-Shiny-276DC3)
![Status](https://img.shields.io/badge/Status-Active-green)

A comprehensive analytics platform for the Metropolitan Mosquito Control District, providing interactive dashboards for mosquito surveillance, treatment analysis, and operational metrics.




## Table of Contents

- [Documentation Resources](#documentation-resources)
- [Common Filter & Grouping Options](#common-filter--grouping-options)
- [Architecture](#architecture)
- [Centralized Shared Resources](#centralized-shared-resources-shared)
- [Applications](#applications)
  - [Inspections Coverage](#inspections-coverage) - [Notes](apps/inspections/NOTES.md)
  - [Ground Prehatch Progress](#ground-prehatch-progress) - [Notes](apps/ground_prehatch_progress/NOTES.md)
  - [Air Sites Simple](#air-sites-simple) - [Notes](apps/air_sites_simple/NOTES.md)
  - [Drone Treatment](#drone-treatment) - [Notes](apps/drone/NOTES.md)
  - [Air Treatment Checkbacks](#air-treatment-checkbacks) - [Notes](apps/control_efficacy/NOTES.md)
  - [Catch Basin Status](#catch-basin-status) - [Notes](apps/catch_basin_status/NOTES.md)
  - [Structural Treatment](#structural-treatment) - [Notes](apps/struct_trt/NOTES.md)
  - [SUCO History](#suco-history) - [Notes](apps/suco_history/NOTES.md)
  - [Mosquito Surveillance Map](#mosquito-surveillance-map)
  - [Mosquito Monitoring](#mosquito-monitoring)
  - [Cattail Inspections](#cattail-inspections) - [Notes](apps/cattail_inspections/NOTES.md)
  - [Cattail Treatments](#cattail-treatments) - [Notes](apps/cattail_treatments/NOTES.md)
  - [Trap Surveillance Test](#trap-surveillance-test) - [Notes](apps/trap_survillance_test/NOTES.md)
  - [Section Cards](#section-cards) - [Notes](apps/section-cards/NOTES.md)
  - [Test Application](#test-application)
- [Installation & Deployment](#installation--deployment)
  - [Prerequisites - System Dependencies](#prerequisites---system-dependencies)
  - [R Package Installation](#r-package-installation)
  - [Shiny Server Setup](#shiny-server-setup)
  - [Running Apps Individually with R (Without Docker)](#running-apps-individually-with-r-without-docker)
  - [Production Deployment](#production-deployment)
- [Git Workflow for Contributors](#git-workflow-for-contributors)
  - [Getting Started - Cloning the Repository](#getting-started---cloning-the-repository)
  - [Branch Strategy](#branch-strategy)
  - [Basic Git Commands](#basic-git-commands)
  - [Typical Workflow for Making Changes](#typical-workflow-for-making-changes)
  - [Common Scenarios](#common-scenarios)
  - [Best Practices](#best-practices)
  - [Handling Merge Conflicts](#handling-merge-conflicts)
- [Important Bug Fixes](#important-bug-fixes)

## Documentation Resources

Comprehensive technical documentation and performance reports:

### Database & Schema
- **[Database Schema Reference](documents/DATABASE_SCHEMA_REFERENCE.md)** - Comprehensive guide to all database tables, columns, relationships, and common join patterns used across applications

### Performance & Optimization
- **[Connection Pooling Performance Report](documents/CONNECTION_POOLING_PERFORMANCE_REPORT.md)** - Before/after analysis of database connection pooling implementation showing 22% reduction in errors and improved reliability
- **[Optimization Analysis](documents/OPTIMIZATION_ANALYSIS.md)** - Comprehensive performance optimization opportunities and implementation strategies
- **[Before and After Data Pooling Tests](documents/before%20and%20after%20data%20pooling%20tests.pdf)** - Visual comparison of stress test results

### UI Customization
- **[Color Themes Configuration](documents/COLOR_THEMES_README.md)** - Centralized color theme system where one change in `shared/db_helpers.R` automatically updates colors across all apps
- **[Theme Configuration Guide](documents/THEME_CONFIGURATION.md)** - Detailed theme customization instructions

### Development Resources
- **[Shared Resources README](shared/README.md)** - Documentation for shared utilities, database helpers, geospatial data extraction (Q_to_R), and documentation sync system
- **[User Feedback Backlog](documents/USER_FEEDBACK_BACKLOG.md)** - Feature requests and improvements from user feedback

## Common Filter & Grouping Options

Apps share consistent filter and grouping controls for unified user experience:

**Standard Filters (used when appropriate for each app):**
- **Facility** - Filter by facility location (Sr, Sj, N, E, MO, etc)
- **FOS Area** - Filter by Field Operations Supervisor (foreman) assignment
- **Zone** - P1 only, P2 only, P1+P2 separate, or Combined P1+P2
- **Priority** - Filter by site priority level (RED, YELLOW, BLUE, GREEN, PURPLE)
- **Treatment Type** - Air, Ground, or Drone (note: drone can be subcategorized as ground or air depending on application)
- **Pretend Today Is** - Simulate a different analysis date to see what status/progress would have been on that date (useful for testing, historical analysis, or planning)
- **Days Until Expiring** - For treatment progress apps, set threshold for sites approaching treatment expiration
- **Color Theme** - Choose from 6 themes (MMCD default, IBM, Wong, Tol, Viridis, ColorBrewer) - changes all charts and maps

**Display Metric Options:**
- **Sites vs Acres** - Toggle between count of sites or total acres for all visualizations and summaries

**Group By Options:**
- **All MMCD** - Show aggregate data across entire district
- **Facility** - Group results by facility
- **FOS** - Group results by Field Operations Supervisor
- **Section** - Group results by geographic section (available in some apps)

**How "Pretend Today Is" Works:** This date control allows you to run the analysis as if today were a different date. For example, setting it to a past date shows what the status would have looked like then, while setting it to a future date can help with planning. All status calculations, expiration logic, and "days until expiring" filters use this date instead of the actual current date.
NOTE: "Pretend Today Is" will only work if the query includes the same join with the archive as it does with the current

## Architecture

This platform hosts multiple R Shiny applications in an organized, scalable structure with a **modular architecture** and centralized helper module:

```
mmcd_metrics/
├── shared/
│   ├── db_helpers.R              # Centralized hub for all apps
│   ├── db_pool.R                 # Database connection pooling for performance
│   ├── color_themes.R            # Centralized color definitions
│   ├── stat_box_helpers.R        # Reusable stat box UI components
│   ├── assets/                   # Shared icons and images
│   └── Q_to_R/                   # Geospatial data extraction scripts
│       └── data/                 # Extracted shapefiles and boundaries
├── apps/
│   ├── index.html                # Main landing page
│   ├── about/                    # About page
│   │   └── index.html
│   ├── inspections/              # Inspection coverage analysis (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing and database queries
│   │   ├── display_functions.R   # Visualization and chart generation
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── catch_basin_status/       # Catch basin treatment tracking (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── historical_functions.R # Historical analysis functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── mosquito-monitoring/      # CO2 trap surveillance data
│   │   └── app.R
│   ├── suco_history/             # SUCO surveillance analysis (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing and database queries
│   │   ├── display_functions.R   # Visualization and chart generation
│   │   ├── ui_helpers.R          # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── drone/                    # Comprehensive drone treatment (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── historical_functions.R # Historical data analysis functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── struct_trt/               # Structure treatment (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── historical_functions.R # Historical analysis functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── ground_prehatch_progress/ # Ground prehatch treatment (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── historical_functions.R # Historical analysis functions
│   │   ├── ui_helpers.R          # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── cattail_inspections/      # Cattail inspection tracking (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing and database queries
│   │   ├── display_functions.R   # Visualization and chart generation
│   │   ├── historical_functions.R # Historical analysis functions
│   │   ├── planned_treatment_functions.R # Treatment planning functions
│   │   ├── progress_functions.R  # Progress tracking functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── cattail_treatments/       # Cattail treatment tracking (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing and database queries
│   │   ├── display_functions.R   # Visualization and chart generation
│   │   ├── historical_functions.R # Historical analysis with DOY-based inspection years
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── red_air_legacy/           # Legacy air treatment pipeline
│   │   ├── app.R                 # Main application logic
│   │   ├── air_status_functions.R # Air site status processing
│   │   ├── legacy_air_status_functions.R # Legacy status functions
│   │   ├── optimized_air_status_functions.R # Optimized functions
│   │   ├── flow_testing_functions.R # Flow testing utilities
│   │   └── test_14_day_persistence.R # Testing functions
│   ├── air_sites_simple/         # Air sites simple (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── historical_functions.R # Historical analysis functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── mosquito_surveillance_map/ # Mosquito surveillance mapping
│   │   ├── app.R                 # Main application logic
│   │   └── shp/                  # Shapefile data (geographic boundaries)
│   ├── control_efficacy/         # Air treatment checkback efficacy (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── checkback_functions.R # Checkback analysis functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── trap_survillance_test/    # Trap surveillance test (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── mle_trap_based.R      # MLE calculation functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── section-cards/            # Section Cards (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   └── test-app/                 # Test application 
│       └── app.R                 # Main application logic
├── documents/                    # Technical documentation and reports
│   ├── CONNECTION_POOLING_PERFORMANCE_REPORT.md
│   ├── OPTIMIZATION_ANALYSIS.md
│   ├── COLOR_THEMES_README.md
│   ├── THEME_CONFIGURATION.md
│   ├── USER_FEEDBACK_BACKLOG.md
│   └── before and after data pooling tests.pdf
├── dockerfile                    # Docker container configuration
├── index.html                    # Main landing page
├── README.md                     # This file
└── shiny-server.conf            # Shiny Server configuration
```

## Centralized Shared Resources (`shared/`)

All apps source `shared/db_helpers.R` which provides centralized utilities and automatically loads all other shared modules.

**Core Files:**
- **`db_helpers.R`** - Central hub: database connections, facility/FOS lookups, date thresholds, priority choices
- **`color_themes.R`** - **ALL colors in ALL apps come from ONE location!** Change colors here to update everywhere. Supports 6 themes: MMCD (default), IBM, Wong, Tol, Viridis, ColorBrewer. See [Color Themes Configuration](documents/COLOR_THEMES_README.md)
- **`db_pool.R`** - Connection pooling for 22% fewer errors and 81% faster queries. See [Performance Report](documents/CONNECTION_POOLING_PERFORMANCE_REPORT.md)
- **`stat_box_helpers.R`** - Reusable UI components for consistent value boxes

**Usage in apps:**
```r
source("../../shared/db_helpers.R")  # Loads all shared modules
conn <- get_pool()                    # Get connection pool
palette <- get_theme_palette()        # Get current theme colors
```

**Additional Resources:**
- **`assets/`** - Shared icons and images for landing pages
- **`Q_to_R/`** - Geospatial data extraction scripts (PostgreSQL to shapefiles)

See [Shared Resources README](shared/README.md) for complete documentation.


## Applications

### Inspections Coverage
- **Path**: `/inspections/`
- **Purpose**: Analyze inspection coverage for all sites across facilities
- **Documentation**: [Technical Notes](apps/inspections/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with tabbed interface
  - **`data_functions.R`**: Database queries and coverage calculations
  - **`display_functions.R`**: Visualization and chart generation
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Site Analytics**: Total sites, wet frequency analysis, and summary statistics
  - **Larvae Threshold Analysis**: Sites exceeding larvae counts with frequency tracking
  - **Coverage Gaps**: Identify sites with inspection gaps or never inspected
  - Uses both current and archive records for complete history

### Ground Prehatch Progress
- **Path**: `/ground_prehatch_progress/`
- **Purpose**: Track and analyze ground prehatch treatment progress and performance
- **Documentation**: [Technical Notes](apps/ground_prehatch_progress/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with clean tabbed interface
  - **`data_functions.R`**: Database queries, progress calculations, and performance metrics
  - **`display_functions.R`**: Chart generation, progress visualizations, and dashboard displays
  - **`historical_functions.R`**: Historical trend analysis functions
  - **`ui_helpers.R`**: UI component functions and reusable interface elements
- **Features**:
  - **Progress Overview Tab**: Sites with active/expiring treatments aggregated by facility, FOS, or section
  - **Detailed View Tab**: Individual site details with treatment status and download
  - **Map Tab**: Interactive map with color-coded treatment status markers
  - **Historical Analysis Tab**: Multi-year trends (sites, treatments, or acres by year/week)
  - Prehatch-specific filtering with dry site detection

### Air Sites Simple
- **Path**: `/air_sites_simple/`
- **Purpose**: Monitor air site status and treatment pipeline (no rainfall tracking)
- **Documentation**: [Technical Notes](apps/air_sites_simple/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with tabbed interface
  - **`data_functions.R`**: Database queries, progress calculations, and performance metrics
  - **`display_functions.R`**: Chart generation, progress visualizations, and dashboard displays
  - **`historical_functions.R`**: Historical trend analysis functions
  - **`ui_helper.R`**: UI component functions and reusable interface elements
- **Features**:
  - Interactive map with color-coded site status dots
  - Treatment lifecycle: Needs Inspection → Under Threshold → Needs Treatment → Active Treatment
  - BTI material effect days tracking
  - Larvae threshold monitoring
  - Red bug detection tracking

### Drone Treatment
- **Path**: `/drone/`
- **Documentation**: [Technical Notes](apps/drone/NOTES.md)
- **Purpose**: Comprehensive drone treatment tracking with real-time progress and historical analysis
- **Modular Structure**:
  - **`app.R`**: Main application logic with multi-tab interface
  - **`data_functions.R`**: Database queries and treatment calculations
  - **`historical_functions.R`**: Historical trend analysis and data processing functions
  - **`display_functions.R`**: Visualization and chart generation
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Current Progress Tab**: Active treatments and expiring treatments summary
  - **Map Tab**: Leaflet map with color-coded markers and popup details
  - **Historical Trends Tab**: Multi-year trends analyzing sites, treatments, or acres treated
  - **Site Statistics Tab**: Average site size, largest sites, smallest sites analysis
  - Prehatch toggle filter for seasonal analysis

### Air Treatment Checkbacks
- **Path**: `/control_efficacy/`
- **Purpose**: Air treatment checkback analysis and efficacy monitoring
- **Documentation**: [Technical Notes](apps/control_efficacy/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with tabbed interface
  - **`data_functions.R`**: Database queries and treatment data processing
  - **`display_functions.R`**: Visualization and chart generation
  - **`checkback_functions.R`**: Checkback analysis and efficacy calculations
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Checkback Progress Tab**: Brood completion progress and outstanding checkbacks tracking
  - **Status Tables Tab**: Brood status overview and detailed checkback results with larvae counts
  - **Control Efficacy Tab**: Pre-treatment vs post-treatment dip count efficacy analysis
  - Species composition in checkback samples
  - Treatment-to-checkback timing validation

### Catch Basin Status
- **Path**: `/catch_basin_status/`
- **Purpose**: Monitor catch basin treatment status across facilities
- **Documentation**: [Technical Notes](apps/catch_basin_status/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with tabbed interface
  - **`data_functions.R`**: Database queries and status calculations
  - **`display_functions.R`**: Chart generation and progress visualizations
  - **`historical_functions.R`**: Historical trend analysis functions
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Status Overview Tab**: High-level summary metrics and treatment status charts
  - **Detailed View Tab**: Granular site-by-site breakdown with last treatment dates
  - **Historical Analysis Tab**: Multi-year treatment trends and patterns
  - Focus on wet catch basins only (status_udw = 'W')
  - Treatment expiration tracking with days until expiring filter

### Structural Treatment
- **Path**: `/struct_trt/`
- **Purpose**: Comprehensive structure treatment tracking with current progress and historical analysis
- **Documentation**: [Technical Notes](apps/struct_trt/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with dual-tab interface
  - **`data_functions.R`**: Database queries, treatment calculations, and data processing
  - **`display_functions.R`**: Chart generation, progress visualizations, and historical analysis displays
  - **`historical_functions.R`**: Historical trend analysis functions
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Progress Tab**: Real-time active structure treatment monitoring
  - **History Tab**: Multi-year historical trends and patterns
  - Customizable treatment effect_days parameter for expiration calculations
  - Structure type categorization and filtering
  - Proportion of structures under active treatment tracking

### SUCO History
- **Path**: `/suco_history/`
- **Purpose**: Surveillance Count (SUCO) historical analysis dashboard
- **Documentation**: [Technical Notes](apps/suco_history/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main UI and server logic with tabbed interface
  - **`data_functions.R`**: Database queries, species mapping, spatial data processing, and top locations analysis
  - **`display_functions.R`**: Interactive maps with leaflet, trend charts, and plotly visualizations
  - **`ui_helpers.R`**: UI component functions and interface helpers
- **Features**:
  - **Graph Tab**: Time series visualization of SUCO trends over time
  - **Map Tab**: Spatial distribution with dynamic marker sizing based on counts
  - **Summary Table Tab**: Aggregated statistics grouped by facility or FOS
  - **Detailed Samples Tab**: Raw SUCO inspection records with full details
  - **Top Locations Tab**: Rankings by visit frequency or species 

### Mosquito Surveillance Map
- **Path**: `/mosquito_surveillance_map/`
- **Purpose**: Interactive geographic mapping of mosquito surveillance data
- **Features**:
  - Species filtering and surveillance type selection
  - Temporal analysis with date range controls
  - Visualize mosquito counts across monitoring locations
  - Note: Application takes time to load due to data volume

### Mosquito Monitoring
- **Path**: `/mosquito-monitoring/`
- **Purpose**: CO2 trap mosquito surveillance analysis
- **Features**: 
  - Species-specific analysis with 50+ mosquito species
  - Facility and zone comparisons
  - Interactive time series with hover tooltips

### Cattail Inspections
- **Path**: `/cattail_inspections/`
- **Purpose**: Track cattail inspection progress and site assessments
- **Documentation**: [Technical Notes](apps/cattail_inspections/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with multi-tab interface
  - **`data_functions.R`**: Database queries, site calculations, and inspection data processing
  - **`display_functions.R`**: Chart generation, progress visualizations, and map displays
  - **`historical_functions.R`**: Historical trend analysis functions
  - **`planned_treatment_functions.R`**: Treatment planning functions
  - **`progress_functions.R`**: Progress tracking functions
  - **`ui_helper.R`**: UI component functions and interface helpers
- **Features**:
  - Progress tracking against goals with historical comparison
  - Single inspection per site counting (most recent only)
  - Site count goals by facility (p1_totsitecount, p2_totsitecount)
  - Wet/dry site status and larvae dip count tracking
  - Action code '9' filtering for cattail inspections only

### Cattail Treatments
- **Path**: `/cattail_treatments/`
- **Purpose**: Track cattail treatment applications and effectiveness with DOY-based inspection year logic
- **Documentation**: [Technical Notes](apps/cattail_treatments/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with multi-tab interface (Progress, Historical, Map)
  - **`data_functions.R`**: Database queries, treatment calculations, and data processing
  - **`display_functions.R`**: Chart generation with multiple chart types (line/bar/stacked), progress visualizations
  - **`historical_functions.R`**: Historical analysis with DOY-based inspection year calculation (Fall-Summer seasonal cycles)
  - **`ui_helper.R`**: UI component functions and reusable interface elements
- **Features**:
  - **Progress Tab**: Current treatment status with 3 chart types
  - **Historical Tab**: Multi-year trends using DOY-based inspection years
  - **Inspection Year Logic**: Fall (DOY 244-365) + Summer (DOY 135-213) seasonal cycles
  - Multiple display metrics: Sites Treated, % Treated, Sites Need Treatment
  - Chart type options: Line, grouped bar, stacked bar
  - Action '9' inspections + Action '3'/'A' treatments
  - Cattail material code filtering

### Trap Surveillance Test
- **Path**: `/trap_survillance_test/`
- **Purpose**: Prototype app to compute section vector index using k-nearest traps
- **Documentation**: [Technical Notes](apps/trap_survillance_test/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic
  - **`data_functions.R`**: Data processing functions
  - **`display_functions.R`**: Visualization functions
  - **`mle_trap_based.R`**: MLE calculation functions using PooledInfRate package
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Trap-based MLE**: Maximum Likelihood Estimation for virus infection risk
  - **k-NN Distance-Weighted Averaging**: Spatial interpolation across sections
  - Population Index, MLE, and Vector Index calculations using PooledInfRate package
  - Interactive leaflet map with section polygons, trap markers, and Vector Index Areas
  - Unified SQL query with CTEs for optimal performance
  - Species filtering for targeted analysis

### Section Cards
- **Path**: `/section-cards/`
- **Purpose**: Demo application for section cards functionality
- **Documentation**: [Technical Notes](apps/section-cards/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic
  - **`data_functions.R`**: Data processing functions
  - **`display_functions.R`**: Card generation and layout functions
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Printable Field Cards**: 6 cards per page optimized for standard printing
  - **Configurable Content**: Select which site fields to display in header vs table
  - **Section Grouping**: Option to group cards by section (prevents mixing on pages)
  - **Download as HTML**: Export standalone file for printing
  - Active sites only (enddate IS NULL)

### Test Application
- **Path**: `/test-app/`
- **Purpose**: Testing and validation application
- **Features**:
  - Useful for verifying application links on the dashboard
  - Testing environment for new features
  - Uses built-in R datasets without database dependencies

## Installation & Deployment

### System Requirements
- **Operating System**: Ubuntu 18.04+ or similar Debian-based Linux distribution
- **R Version**: R 4.0+ (tested with R 4.5.2)
- **Memory**: Minimum 4GB RAM (8GB recommended for production)
- **Storage**: At least 2GB free space for dependencies and applications
- **Network**: Internet access for package installation and database connectivity

### Distributed Cache (Redis)
The apps use Redis for shared caching across containers. This repo currently hardcodes Redis to the local Docker host name `mmcd-redis` (port 6379, db 0, prefix `mmcd:`, no password). Ensure all app containers run on the same Docker network as the `mmcd-redis` container so they can reach it by name.

### Prerequisites - System Dependencies
The MMCD applications require extensive geospatial and database libraries. Install all system dependencies first:

```bash
# Update package list
sudo apt update

# Install R and core development tools
sudo apt install -y r-base r-base-dev gdebi-core nginx

# Install database connectivity libraries
sudo apt install -y libcurl4-openssl-dev libssl-dev libxml2-dev libpq-dev

# Install geospatial libraries (required for mapping applications)
sudo apt install -y libgdal-dev libudunits2-dev libproj-dev

# Install font and text processing libraries
sudo apt install -y libfontconfig1-dev libfreetype-dev libpng-dev \
    libharfbuzz-dev libfribidi-dev

# Install compilation tools (required for R package compilation)
sudo apt install -y gfortran cmake

# Install additional graphics and computation libraries
sudo apt install -y libgl1-mesa-dev libglu1-mesa libx11-dev libxt-dev libxft-dev \
    libtiff-dev libjpeg-dev libgeos-dev libgmp-dev libgsl-dev \
    libv8-dev libpoppler-cpp-dev libmagick++-dev
```

#### R Package Installation
Install R packages in the correct order to handle dependencies. For Shiny Server deployment, packages must be installed system-wide to be accessible to the shiny user:

```bash
# Install core packages (system-wide for Shiny Server)
sudo R -e "install.packages(c( \
  'shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'RPostgres', \
  'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', \
  'plotrix', 'dtplyr', 'vroom', 'tidyverse', 'tidyr', \
  'classInt', 's2', \
  'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster', \
  'plotly', 'purrr', 'tibble', 'pool' \
), lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"

# Install additional visualization packages for SF mapping
sudo R -e "install.packages(c('viridis', 'gridExtra', 'ggspatial', 'rosm', 'prettymapr'), \
  lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"

# Install PooledInfRate for MLE calculations (trap_survillance_test app)
sudo R -e "install.packages('remotes', lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/'); \
  remotes::install_github('CDCgov/PooledInfRate')"
```

### Shiny Server Setup

Download and install Shiny Server for production deployment:

```bash
# Download Shiny Server
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb

# Install Shiny Server
sudo gdebi -n shiny-server-1.5.22.1017-amd64.deb

# Copy applications and shared resources to Shiny Server directory
sudo cp -r apps/* /srv/shiny-server/
sudo cp -r shared /srv/shiny-server/

# Set proper ownership
sudo chown -R shiny:shiny /srv/shiny-server

# Start and enable Shiny Server
sudo systemctl start shiny-server
sudo systemctl enable shiny-server
```


Access the applications:
- Main Dashboard: `http://localhost:3838/`
- Individual Applications: `http://localhost:3838/app-name/`

### Running Apps Individually with R (Without Docker)

You can run any Shiny app directly using R without Docker or Shiny Server. This is ideal for local development and testing.

#### Windows Setup

#**NOTE:** Has not actualy worked yet but theoretically it can
**Step 1: Install R**

Download and install R from [CRAN](https://cran.r-project.org/bin/windows/base/):
```powershell
# Or use Windows Package Manager
winget install RProject.R

# Or use Chocolatey
choco install r.project
```

**Step 2: Install Required R Packages**

Open PowerShell and run:
```powershell
# Install ALL required packages to user library
& "C:\Program Files\R\R-4.5.2\bin\R.exe" -e "dir.create(Sys.getenv('R_LIBS_USER'), recursive=TRUE, showWarnings=FALSE); install.packages(c('shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'RPostgres', 'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', 'plotrix', 'dtplyr', 'vroom', 'tidyverse', 'classInt', 's2', 'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster', 'plotly', 'purrr', 'tibble'), repos='https://cran.rstudio.com/', lib=Sys.getenv('R_LIBS_USER'), dependencies=TRUE)"
```

> **Note**: Adjust the R path (`C:\Program Files\R\R-4.5.2\bin\R.exe`) to match your installed version.

**Verify Installation (Optional)**

Run the verification script to confirm all packages are installed:
```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" verify_packages.R
```

**Step 3: Configure Environment Variables**

Create a `.env` file in the project root with your database credentials:
```bash
# Copy the example file
cp .env.example .env

# Edit .env with your database credentials:
DB_HOST=your-database-host
DB_PORT=5432
DB_USER=your-username
DB_PASSWORD=your-password
DB_NAME=your-database-name
```

**Step 4: Run an App**

Navigate to any app directory and run it:
```powershell
# Navigate to the app directory
cd c:\Users\yourusername\Documents\mmcd_metrics\apps\test-app

# Run the app
& "C:\Program Files\R\R-4.5.2\bin\R.exe" -e "shiny::runApp(port=3838, host='127.0.0.1', launch.browser=FALSE)"
```

The app will be available at: **http://127.0.0.1:3838**

#### Linux/Mac Setup

**Step 1: Install R**
```bash
# Ubuntu/Debian
sudo apt install r-base r-base-dev

# macOS (using Homebrew)
brew install r
```

**Step 2: Install Required R Packages**
```bash
# Install ALL required packages
R -e "install.packages(c( \
  'shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'RPostgres', \
  'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', \
  'plotrix', 'dtplyr', 'vroom', 'tidyverse', \
  'classInt', 's2', \
  'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster', \
  'plotly', 'purrr', 'tibble' \
), repos='https://cran.rstudio.com/', dependencies=TRUE)"
```

**Step 3: Configure Environment Variables**
```bash
# Copy and edit the .env file
cp .env.example .env
nano .env  # or use your preferred editor
```

**Step 4: Run an App**
```bash
# Navigate to any app directory
cd apps/test-app

# Run the app
R -e "shiny::runApp(port=3838, host='127.0.0.1', launch.browser=FALSE)"
```

#### Running Different Apps

You can run any app in the `apps/` directory:

```powershell
# Windows examples:

# Test app (minimal database requirements)
cd apps\test-app
& "C:\Program Files\R\R-4.5.2\bin\R.exe" -e "shiny::runApp(port=3838, host='127.0.0.1')"

# Simple test app (no database required)
cd apps\simple-test
& "C:\Program Files\R\R-4.5.2\bin\R.exe" -e "shiny::runApp(port=3838, host='127.0.0.1')"

# Mosquito monitoring app
cd apps\mosquito-monitoring
& "C:\Program Files\R\R-4.5.2\bin\R.exe" -e "shiny::runApp(port=3838, host='127.0.0.1')"

# SUCO history app
cd apps\suco_history
& "C:\Program Files\R\R-4.5.2\bin\R.exe" -e "shiny::runApp(port=3838, host='127.0.0.1')"
```

```bash
# Linux/Mac examples:
cd apps/test-app && R -e "shiny::runApp(port=3838)"
cd apps/mosquito-monitoring && R -e "shiny::runApp(port=3838)"
cd apps/suco_history && R -e "shiny::runApp(port=3838)"
```

#### Using RStudio (Optional)

If you have RStudio installed:

1. Open RStudio
2. Open the `app.R` file from any app directory (e.g., `apps/test-app/app.R`)
3. Click the **"Run App"** button in the top-right of the editor
4. The app will launch in a browser or RStudio viewer

#### Stopping the App

To stop a running Shiny app:
- Press `Ctrl+C` in the terminal/PowerShell window
- Or close the terminal window

### Production Deployment

For a complete production setup on a fresh Ubuntu machine:

```bash
# Step 1: Update system and install dependencies
sudo apt update && sudo apt upgrade -y

# Step 2: Install all system dependencies (see Prerequisites section above)
sudo apt install -y r-base r-base-dev gdebi-core \
    libcurl4-openssl-dev libssl-dev libxml2-dev libpq-dev \
    libgdal-dev libudunits2-dev libproj-dev \
    libfontconfig1-dev libfreetype-dev libpng-dev \
    libharfbuzz-dev libfribidi-dev gfortran cmake libabsl-dev

# Step 3: Install R packages (single command with all required packages)
R -e "install.packages(c( \
  'shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'RPostgres', \
  'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', \
  'plotrix', 'dtplyr', 'vroom', 'tidyverse', \
  'classInt', 's2', \
  'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster', \
  'plotly', 'purrr', 'tibble' \
), repos='https://cran.rstudio.com/')"

# Step 4: Clone repository
git clone https://github.com/ablepacifist/mmcd_metrics_1.git mmcd_metrics
cd mmcd_metrics

# Step 5: Install Shiny Server
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb
sudo gdebi -n shiny-server-1.5.22.1017-amd64.deb

# Step 6: Deploy applications
sudo cp -r apps/* /srv/shiny-server/
sudo cp -r shared /srv/shiny-server/
sudo chown -R shiny:shiny /srv/shiny-server

# Step 7: Start services
sudo systemctl start shiny-server
sudo systemctl enable shiny-server
```


### Environment Variables Setup

#### Local Development
1. Copy the example environment file:
   ```bash
   cp .env.example .env
   ```
2. Edit `.env` with your actual database credentials:
   ```bash
   DB_HOST=your-database-host
   DB_PORT=5432
   DB_USER=your-username
   DB_PASSWORD=your-password
   DB_NAME=your-database-name
   ```

#### Production Deployment
In production (AWS App Runner, Docker, etc.), set these environment variables:
- `DB_HOST`
- `DB_PORT` 
- `DB_USER`
- `DB_PASSWORD`
- `DB_NAME`

The application will automatically use environment variables if no .env file is found.

### Docker Deployment

To build and run the dashboard using Docker:

```bash
# Build the Docker image
docker build -t mmcd-dashboard .

# For local development (with .env file)
docker run -p 3838:3838 --env-file .env mmcd-dashboard

# For production (with environment variables)
docker run -p 3838:3838 \
  -e DB_HOST=your-db-host \
  -e DB_NAME=your-db-name \
  -e DB_USER=your-db-user \
  -e DB_PASSWORD=your-db-password \
  mmcd-dashboard
```

Access the dashboard at: `http://localhost:3838`


#### AWS Deployment with Secure Environment Variables

For AWS deployment, **DO NOT** copy .env files to the container. Instead, use AWS Secrets Manager or environment variables:

**Option 1: AWS Secrets Manager (Recommended)**
```bash
# Store secrets in AWS Secrets Manager
aws secretsmanager create-secret \
  --name "mmcd-dashboard-db" \
  --description "Database credentials for MMCD Dashboard" \
  --secret-string '{"DB_HOST":"your-host","DB_NAME":"your-db","DB_USER":"your-user","DB_PASSWORD":"your-password"}'

# Use in ECS task definition or EC2 user data
```

**Option 2: Environment Variables in Docker**
```bash
# Run with environment variables (for AWS ECS/EC2)
docker run -p 3838:3838 \
  -e DB_HOST=$DB_HOST \
  -e DB_NAME=$DB_NAME \
  -e DB_USER=$DB_USER \
  -e DB_PASSWORD=$DB_PASSWORD \
  mmcd-dashboard
```

**Option 3: Local Development Only**
```bash
# Only for local development - create .env file manually
cp .env.example .env
# Edit .env with your local database credentials
docker run -p 3838:3838 --env-file .env mmcd-dashboard
```


```

## Git Workflow for Contributors

**Repository**: [https://github.com/mmcdmn/mmcd_metrics](https://github.com/mmcdmn/mmcd_metrics)

> **Access Required**: You must have repository access rights to push changes. Contact the administrator if you need access.

### Getting Started - Cloning the Repository

If you're starting fresh, clone the repository and checkout the dev branch:

```bash
# Clone the repository
git clone https://github.com/mmcdmn/mmcd_metrics.git

# Navigate into the project
cd mmcd_metrics

# Switch to dev branch (where you'll do your work)
git checkout dev

# Verify you're on dev branch
git branch
```

### Branch Strategy

This repository uses a two-branch workflow:

- **`dev`** - Development branch for testing and review
  - All contributors should push their changes here first
  - Used for testing new features and bug fixes
  - Safe environment for experimentation

- **`main`** - Production branch deployed to AWS
  - Protected branch
  - Automatically deployed to AWS App Runner for production use
  - Only stable, tested code should be merged here
  - **Merging to main is done via GitHub Pull Request, not locally**

### Basic Git Commands

#### Check Current Status
See what files you've changed and current branch:
```bash
git status
```

#### Switch Between Branches
```bash
# Switch to dev branch (where you should work)
git checkout dev

# Switch to main branch (to review production code)
git checkout main

# Create and switch to a new feature branch
git checkout -b feature-name
```

#### Stage Your Changes
Add files you want to commit:
```bash
# Add specific file
git add path/to/file.R

# Add all changed files in a directory
git add apps/my-app/

# Add all changed files (use carefully!)
git add .
```

#### Commit Your Changes
Save your changes with a descriptive message:
```bash
git commit -m "Brief description of what you changed"

# Example commit messages:
git commit -m "Fix zone assignment bug in drone app"
git commit -m "Add archive data support to trap surveillance"
git commit -m "Update README with Git workflow instructions"
```

#### Push to Remote Repository
```bash
# Push your changes to dev branch
git push origin dev

# Push changes to a feature branch
git push origin feature-name
```

### Typical Workflow for Making Changes

1. **Start on dev branch**
   ```bash
   git checkout dev
   git pull origin dev  # Get latest changes
   ```

2. **Make your changes**
   - Edit files in your preferred editor
   - Test your changes locally

3. **Stage and commit**
   ```bash
   git status                    # See what changed
   git add apps/my-app/app.R     # Stage your files
   git commit -m "Description of changes"
   ```

4. **Push to dev**
   ```bash
   git push origin dev
   ```

5. **Test on dev environment**
   - Verify your changes work as expected
   - Have others review if needed

6. **Merge to main (when ready for production)**
   
   **Merging is done on GitHub, not locally:**
   
   a. Go to the repository on GitHub: [https://github.com/mmcdmn/mmcd_metrics](https://github.com/mmcdmn/mmcd_metrics)
   
   b. Click "Pull requests" tab
   
   c. Click "New pull request"
   
   d. Set base branch to `main` and compare branch to `dev`
   
   e. Review the changes, add a description
   
   f. Click "Create pull request"
   
   g. Wait for review/approval (if required)
   
   h. Click "Merge pull request" to deploy to production
   
   i. Once merged, the changes automatically deploy to AWS App Runner

### Common Scenarios

#### Pulling Latest Changes
Before starting work, always pull the latest code:
```bash
git fetch
git checkout dev
git pull origin dev
```

#### Viewing Your Changes
See what you've modified:
```bash
git status                    # Files changed
git diff                      # See line-by-line changes
git diff apps/my-app/app.R   # Changes in specific file
```

#### Undoing Changes
If you made a mistake:
```bash
# Discard changes to a specific file
git checkout -- path/to/file.R

# Discard all uncommitted changes (careful!)
git reset --hard
```

#### Viewing Commit History
```bash
git log                       # Full history
git log --oneline             # Compact view
git log --graph --oneline     # Visual branch history
```

### Best Practices

1. **Always work on `dev` first** - Never push directly to `main`
2. **Pull before you push** - Get latest changes to avoid conflicts
3. **Commit often** - Small, focused commits are easier to review
4. **Write clear commit messages** - Future you will thank present you
5. **Test before merging to main** - Production should always be stable
6. **Communicate with team** - Let others know about major changes

### Handling Merge Conflicts

If Git can't automatically merge your changes:

1. **Identify conflicted files**
   ```bash
   git status
   ```

2. **Open conflicted files** - Look for conflict markers:
   ```
   <<<<<<< HEAD
   Your changes
   =======
   Someone else's changes
   >>>>>>> branch-name
   ```

3. **Resolve conflicts** - Edit files to keep the correct code

4. **Mark as resolved and commit**
   ```bash
   git add conflicted-file.R
   git commit -m "Resolve merge conflicts"
   ```

### Getting Help

```bash
git help                     # General help
git help commit             # Help for specific command
git status                  # Shows current state and helpful hints
```

For more information, see the [Git documentation](https://git-scm.com/doc) or contact a team member with Git experience.

---

## Important Bug Fixes

### **Zone Assignment Fix (November 2025)**
**CRITICAL FIX**: Corrected ambiguous JOIN logic in `apps/drone/data_functions.R` that was causing incorrect zone assignments.

**Problem**: Sites were showing wrong zones due to broad pattern matching in SQL JOINs. For example:
- Site `191819-045` was incorrectly showing zone 2 instead of zone 1
- Root cause: JOIN matched both `191819-` (zone 1) and `191819E` (zone 2) sectcodes

**Solution**: Changed from broad OR-based pattern matching to precise sectcode matching:
```sql
-- OLD (incorrect - would match multiple sectcodes)
LEFT JOIN public.gis_sectcode g ON LEFT(sitecode, 6) || '-' = g.sectcode
  OR LEFT(sitecode, 6) || 'N' = g.sectcode
  OR LEFT(sitecode, 6) || 'E' = g.sectcode
  OR LEFT(sitecode, 6) || 'W' = g.sectcode

-- NEW (correct - exact match only)  
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(sitecode,7)
```
