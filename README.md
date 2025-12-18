# MMCD Metrics Dashboard

![MMCD Analytics](https://img.shields.io/badge/MMCD-Analytics-blue)
![R Shiny](https://img.shields.io/badge/R-Shiny-276DC3)
![Status](https://img.shields.io/badge/Status-Active-green)

A comprehensive analytics platform for the Metropolitan Mosquito Control District, providing interactive dashboards for mosquito surveillance, treatment analysis, and operational metrics.




## Table of Contents

- [Documentation Resources](#documentation-resources)
- [Architecture](#architecture)
- [Centralized Helpers Module](#centralized-helpers-module-shareddb_helpersr)
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
  - [Section Cards DEMO](#section-cards-demo) - [Notes](apps/section-cards/NOTES.md)
  - [Test Application](#test-application)
- [Installation & Deployment](#installation--deployment)
  - [Prerequisites - System Dependencies](#prerequisites---system-dependencies)
  - [R Package Installation](#r-package-installation)
  - [Shiny Server Setup](#shiny-server-setup)
  - [Running Apps Individually with R (Without Docker)](#running-apps-individually-with-r-without-docker)
  - [Production Deployment](#production-deployment)

## Documentation Resources

Comprehensive technical documentation and performance reports:

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
│   ├── section-cards/            # Section cards DEMO (modular structure)
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
  - Filter by air/ground, FOS area, facility, and zone
  - Identify sites with no inspection in the last X years
  - Uses both current and archive records
  - Coverage metrics and site status tracking

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
  - **Clean persistent filter panel**: All controls stay visible when switching between tabs
  - **Responsive design**: Modern layout with gradient headers and improved spacing
  - **Progress Overview Tab**: Real-time progress tracking with summary value boxes
  - **Detailed View Tab**: Comprehensive site details table with download functionality
  - **Modular UI components**: Reusable UI functions in ui_helpers.R for maintainable code
  - Performance metrics and completion rate analysis (powered by `data_functions.R`)
  - Interactive visualizations and progress charts (powered by `display_functions.R`)
  - Facility-level performance comparisons with P1/P2 zone support
  - Treatment timeline analysis and goal tracking with date simulation
  - Consistent color schemes from centralized `db_helpers.R`

### Air Sites Simple
- **Path**: `/air_sites_simple/`
- **Purpose**: Monitor air site status, rainfall impact, and treatment pipeline
- **Documentation**: [Technical Notes](apps/air_sites_simple/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with tabbed interface
  - **`data_functions.R`**: Database queries, progress calculations, and performance metrics
  - **`display_functions.R`**: Chart generation, progress visualizations, and dashboard displays
  - **`historical_functions.R`**: Historical trend analysis functions
  - **`ui_helper.R`**: UI component functions and reusable interface elements
- **Features**:
  - Interactive map showing site status with color-coded dots (powered by `air_status_functions.R`)
  - Rainfall tracking and analysis
  - Treatment lifecycle management: Needs Inspection → Under Threshold → Needs Treatment → Active Treatment
  - Real-time status summary chart
  - Detailed site information table
  - **Uses centralized colors from `db_helpers.R`**: Changing colors in db_helpers automatically updates the map, chart, and table

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
  - **Progress Tab**: Real-time tracking of drone-based treatment operations
  - **Historical Tab**: Historical trend analysis with percentage and count views (powered by `historical_functions.R`)
  - Active treatment area monitoring and coverage analysis
  - Facility, FOS, zone, and section grouping options
  - Interactive date range selection and customizable time periods
  - Modular external function files for maintainable code

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
  - Air treatment campaign identification (multi-day treatments grouped)
  - Configurable checkback requirements (percentage or fixed number)
  - Checkback completion rate tracking by facility
  - Treatment-to-checkback timing analysis
  - Dip count change visualization (pre vs post treatment)
  - Site-level treatment and checkback details

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
  - Track active treatments on wet catch basins
  - Treatment coverage percentages by facility
  - Summary statistics and detailed breakdowns
  - Facility and foreman filtering
  - Historical trend analysis

### Structural Treatment
- **Path**: `/struct_trt/`
- **Purpose**: Comprehensive structural treatment tracking with current progress and historical analysis
- **Documentation**: [Technical Notes](apps/struct_trt/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with dual-tab interface
  - **`data_functions.R`**: Database queries, treatment calculations, and data processing
  - **`display_functions.R`**: Chart generation, progress visualizations, and historical analysis displays
  - **`historical_functions.R`**: Historical trend analysis functions
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Progress Tab**: Monitor current structural treatment activities (powered by `data_functions.R`)
  - **History Tab**: Historical analysis of structure treatment activities (powered by `display_functions.R`)
  - Proportion of structures under treatment tracking
  - Customizable treatment duration settings and real-time calculations
  - Historical time series and breakdowns by facility, type, and priority
  - Date simulation ("pretend today is") functionality
  - Snapshot and priority breakdowns for comprehensive analysis

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
  - Interactive maps with leaflet and custom marker sizing
  - Facility and foreman filtering with consistent colors from `db_helpers.R`
  - Temporal trend analysis (weekly/monthly with epidemiological weeks)
  - Top locations identification with species-based analysis
  - Spatial data visualization with P1/P2 zone support
  - Species filtering and detailed popup information

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
  - Real-time inspection progress tracking
  - Site assessment and status monitoring
  - Facility and zone filtering
  - Historical inspection trend analysis
  - Interactive maps showing inspection status
  - Progress metrics and completion rates

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
  - **Progress Tab**: Current treatment status with 3 chart types and zone separation
  - **Historical Tab**: Multi-year trends using DOY-based inspection years (Fall Year N + Summer Year N+1)
  - **Inspection Year Logic**: DOY 244-365 (Fall) and DOY 135-213 (Summer) seasonal definitions
  - Multiple display metrics: Sites Treated (as of Aug 1), % Treated, Sites Need Treatment
  - Chart type options: Line charts, grouped bar charts, stacked bar charts
  - Zone filtering: P1/P2 separate or combined display
  - Facility and foreman grouping options
  - Treatment effectiveness tracking
  - Consistent color schemes from centralized `db_helpers.R`

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
  - k-nearest neighbor spatial averaging for section-level metrics
  - Population index, MLE, and Vector Index calculations
  - Interactive leaflet map with section polygons and trap markers
  - Species filtering and trap type selection
  - Distance-weighted averaging for spatial analysis

### Section Cards DEMO
- **Path**: `/section-cards/`
- **Purpose**: Demo application for section cards functionality
- **Documentation**: [Technical Notes](apps/section-cards/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic
  - **`data_functions.R`**: Data processing functions
  - **`display_functions.R`**: Card generation and layout functions
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - Printable site cards for field work
  - Customizable card layouts and field selection
  - Filtering by facility, zone, FOS, and section
  - Print-optimized formatting

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



# One line remove all used ports and run new
```bash
docker stop mmcd-dashboard && docker rm mmcd-dashboard && cd /home/alex/Documents/mmcd/mmcd_metrics && docker build -t mmcd-dashboard . 2>&1 | tail -2 && docker run -d --name mmcd-dashboard -p 3838:3838 -e DB_HOST=rds-readonly.mmcd.org -e DB_PORT=5432 -e DB_USER=mmcd_read -e DB_PASSWORD=mmcd2012 -e DB_NAME=mmcd_data mmcd-dashboard
```

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
