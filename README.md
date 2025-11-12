# MMCD Metrics Dashboard

![MMCD Analytics](https://img.shields.io/badge/MMCD-Analytics-blue)
![R Shiny](https://img.shields.io/badge/R-Shiny-276DC3)
![Status](https://img.shields.io/badge/Status-Active-green)

A comprehensive analytics platform for the Metropolitan Mosquito Control District, providing interactive dashboards for mosquito surveillance, treatment analysis, and operational metrics.


## Table of Contents

- [Architecture](#architecture)
- [Centralized Helpers Module](#centralized-helpers-module-shareddb_helpersr)
- [Applications](#applications)
  - [Mosquito Monitoring](#mosquito-monitoring)
  - [SUCO History](#suco-history) - [Notes](apps/suco_history/NOTES.md)
  - [Drone Treatment](#drone-treatment) -[Notes](apps/drone/NOTES.md)
  - [Structural Treatment](#structural-treatment) - [Notes](apps/struct_trt/NOTES.md)
  - [Cattail Management](#cattail-management) - [Notes](apps/cattail/NOTES.md)
  - [Ground Prehatch Progress](#ground-prehatch-progress) - [Notes](apps/ground_prehatch_progress/NOTES.md)
  - [Red Air Pipeline](#red-air-pipeline)
  - [Mosquito Surveillance Map](#mosquito-surveillance-map)
  - [Test Application](#test-application)
  - [Control Efficacy](#control-efficacy)
  - [Treatment Analysis](#treatment-analysis)
  - [Trap Surveillance Test](#trap-surveillance-test)
  - [Air Sites Simple](#air-sites-simple)
  - [Red Air Legacy](#red-air-legacy)
- [Installation & Deployment](#installation--deployment)
  - [Prerequisites - System Dependencies](#prerequisites---system-dependencies)
  - [R Package Installation](#r-package-installation)
  - [Shiny Server Setup](#shiny-server-setup)
  - [Quick Local Testing](#quick-local-testing)
  - [Running Apps Individually with R (Without Docker)](#running-apps-individually-with-r-without-docker)
  - [Production Deployment](#production-deployment)
- [Application URLs](#application-urls)
- [Adding New Applications](#adding-new-applications)
- [Troubleshooting](#troubleshooting)
- [Development Workflow](#development-workflow)
- [Contributing](#contributing)

## Architecture

This platform hosts multiple R Shiny applications in an organized, scalable structure with a **modular architecture** and centralized helper module:

```
mmcd_metrics/
├── shared/
│   └── db_helpers.R              # Centralized hub for all apps
│       ├── Database connectivity functions
│       ├── Shared facility and FOS data lookups
│       ├── Centralized color definitions (update here changes colors everywhere)
│       └── Common utility functions used across all applications
├── apps/
│   ├── index.html                # Main landing page
│   ├── about/                    # About page
│   │   └── index.html
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
│   │   ├── historical_functions.R  # Historical data analysis functions
│   │   ├── display_functions.R   # Visualization functions
│   │   └── ui_helper.R           # UI component functions
│   ├── struct_trt/               # Structure treatment (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── ground_prehatch_progress/ # Ground prehatch treatment (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── ui_helpers.R          # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── cattail/                  # Comprehensive cattail management (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── planned_treatment_functions.R # Treatment planning functions
│   │   ├── progress_functions.R  # Progress tracking functions
│   │   ├── historical_functions.R # Historical data functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── red_air_legacy/           # Legacy air treatment pipeline
│   │   ├── app.R                 # Main application logic
│   │   ├── air_status_functions.R # Air site status processing
│   │   ├── legacy_air_status_functions.R # Legacy status functions
│   │   ├── optimized_air_status_functions.R # Optimized functions
│   │   └── test_14_day_persistence.R # Testing functions
│   ├── air_sites_simple/         # Air sites simple (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── air_status_functions_enhanced.R # Enhanced status functions
│   │   ├── air_status_functions.R # Air site status processing
│   │   └── ui_helper.R           # UI component functions
│   ├── mosquito_surveillance_map/ # Mosquito surveillance mapping
│   │   ├── app.R                 # Main application logic
│   │   └── shp/                  # Shapefile data (geographic boundaries)
│   ├── control_efficacy/         # Air treatment checkback efficacy
│   │   └── app.R                 # Main application logic
│   ├── treatment-analysis/       # Treatment analysis dashboard
│   │   └── app.R                 # Main application logic
│   ├── trap_survillance_test/    # Trap surveillance test (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── ui_helper.R           # UI component functions
│   │   └── test-sql.R            # SQL testing utilities
│   └── test-app/                 # Test application 
│       └── app.R                 # Main application logic
```




## Applications

### Mosquito Monitoring
- **Path**: `/mosquito-monitoring/`
- **Purpose**: CO2 trap mosquito surveillance analysis
- **Features**: 
  - Species-specific analysis with 50+ mosquito species
  - Facility and zone comparisons
  - Interactive time series with hover tooltips
  - Standard error visualization
  - Logarithmic scale options

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
  - Comprehensive tooltip customization and stacked bar charts

### Drone Treatment
- **Path**: `/drone/`
- **Documentation**: [Technical Notes](apps/drone/NOTES.md)
- **Purpose**: Comprehensive drone treatment tracking with real-time progress and historical analysis
- **Modular Structure**:
  - **`app.R`**: Main application logic with multi-tab interface
  - **`historical_functions.R`**: Historical trend analysis and data processing functions
  - **`site_average_functions.R`**: Site-level average calculations and statistical analysis
- **Features**:
  - **Progress Tab**: Real-time tracking of drone-based treatment operations
  - **Historical Tab**: Historical trend analysis with percentage and count views (powered by `historical_functions.R`)
  - **Site Average Tab**: Site-level average calculations and trend analysis (powered by `site_average_functions.R`)
  - Active treatment area monitoring and coverage analysis
  - P1/P2 zone filtering with alpha transparency
  - Multiple display metrics (sites, treatments, acres)
  - Facility, FOS, and section grouping options
  - Interactive date range selection and customizable time periods
  - Modular external function files for maintainable code


### Structural Treatment
- **Path**: `/struct_trt/`
- **Purpose**: Comprehensive structural treatment tracking with current progress and historical analysis
- **Documentation**: [Technical Notes](apps/struct_trt/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with dual-tab interface
  - **`data_functions.R`**: Database queries, treatment calculations, and data processing
  - **`display_functions.R`**: Chart generation, progress visualizations, and historical analysis displays
- **Features**:
  - **Progress Tab**: Monitor current structural treatment activities (powered by `data_functions.R`)
  - **History Tab**: Historical analysis of structure treatment activities (powered by `display_functions.R`)
  - Proportion of structures under treatment tracking
  - Customizable treatment duration settings and real-time calculations
  - Historical time series and breakdowns by facility, type, and priority
  - Date simulation ("pretend today is") functionality
  - Snapshot and priority breakdowns for comprehensive analysis

### Cattail 
- **Path**: `/cattail/`
- **Purpose**: Comprehensive cattail  dashboard with inspection tracking and treatment planning
- **Documentation**: [Technical Notes](apps/cattail/NOTES.md)
- **Features**:
  - **Inspection Progress Tab**: Track completed cattail inspections versus annual goals
  - **Treatment Planning Tab**: Planning and tracking dashboard for cattail treatments
  - Real-time inspection status monitoring and progress metrics
  - Facility-level inspection performance analysis
  - Planned treatment area visualization and resource allocation
  - Schedule and resource allocation tracking for targeted cattail habitat 
  - Centralized color system for consistent status visualization
  - Modular external function files for treatment planning functionality

### Ground Prehatch Progress
- **Path**: `/ground_prehatch_progress/`
- **Purpose**: Track and analyze ground prehatch treatment progress and performance
- **Documentation**: [Technical Notes](apps/ground_prehatch_progress/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with clean tabbed interface
  - **`data_functions.R`**: Database queries, progress calculations, and performance metrics
  - **`display_functions.R`**: Chart generation, progress visualizations, and dashboard displays
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

### Red Air Pipeline
- **Path**: `/red_air/`
- **Purpose**: Monitor air site status, rainfall impact, and treatment pipeline
- **Modular Structure**:
  - **`app.R`**: Main application logic with interactive dashboard
  - **`air_status_functions.R`**: Air site status processing and lifecycle management
  - **`flow_testing_functions.R`**: Flow testing, validation tools, and pipeline analysis
- **Features**:
  - Interactive map showing site status with color-coded dots (powered by `air_status_functions.R`)
  - Rainfall tracking and analysis
  - Treatment lifecycle management: Needs Inspection → Under Threshold → Needs Treatment → Active Treatment
  - Real-time status summary chart
  - Detailed site information table
  - Flow testing and validation tools (powered by `flow_testing_functions.R`)
  - **Uses centralized colors from `db_helpers.R`**: Changing colors in db_helpers automatically updates the map, chart, and table

### Test Application
- **Path**: `/test-app/`
- **Purpose**: Testing and validation application
- **Features**:
  - Useful for verifying application links on the dashboard
  - Testing environment for new features

### Control Efficacy
- **Path**: `/control_efficacy/`
- **Purpose**: Air treatment checkback analysis and efficacy monitoring
- **Features**:
  - Air treatment campaign identification (multi-day treatments grouped)
  - Configurable checkback requirements (percentage or fixed number)
  - Checkback completion rate tracking by facility
  - Treatment-to-checkback timing analysis
  - Dip count change visualization (pre vs post treatment)
  - Site-level treatment and checkback details
  - Multi-day treatment campaign support

## Installation & Deployment

### Workspace Setup
Before starting, set up your workspace path. Throughout this guide, replace `/path/to/your/workspace` with your actual workspace directory:

```bash
# Example workspace paths:
# /home/username/Documents/mmcd
# /opt/mmcd
# /var/www/mmcd
export MMCD_WORKSPACE="/path/to/your/workspace/mmcd"
```

For the remainder of this guide, we'll use `$MMCD_WORKSPACE` to refer to your workspace directory.

### System Requirements
- **Operating System**: Ubuntu 18.04+ or similar Debian-based Linux distribution
- **R Version**: R 4.0+ (tested with R 4.5.1)
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
sudo apt install -y gfortran cmake libabsl-dev
```

#### R Package Installation
Install R packages in the correct order to handle dependencies. For Shiny Server deployment, packages must be installed system-wide to be accessible to the shiny user:

```bash
# Install ALL required packages in one command (user-level)
R -e "install.packages(c( \
  'shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'RPostgres', \
  'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', \
  'plotrix', 'dtplyr', 'vroom', 'tidyverse', \
  'classInt', 's2', \
  'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster', \
  'plotly', 'purrr', 'tibble' \
), repos='https://cran.rstudio.com/')"

# IMPORTANT: For Shiny Server deployment, also install packages system-wide
# This ensures the shiny user can access all required packages
sudo R -e "install.packages(c( \
  'shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'RPostgres', \
  'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', \
  'plotrix', 'dtplyr', 'vroom', 'tidyverse', \
  'classInt', 's2', \
  'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster', \
  'plotly', 'purrr', 'tibble' \
), lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"
```


### Centralized Helpers Module (`shared/db_helpers.R`)

The `shared/db_helpers.R` file serves as the central hub for shared functionality across all applications:

**Key Features:**
- **Centralized Color Definitions**: All color assignments (hex colors for maps/tables, Shiny named colors for dashboards) are defined once here. Changing a color updates it everywhere in the program.
- **Facility Data**: Shared lookups for facility names and properties
- **FOS (Field Operations Supervisor) Data**: Centralized FOS name and assignment management
- **Database Connectivity**: Shared database connection utilities
- **Common Utility Functions**: Shared formatting, date handling, and data transformation functions

**Color System:**
- `get_status_colors()`: Returns hex color codes for all status types
- `get_shiny_colors()`: Returns Shiny dashboard named colors (for valueBox, charts)
- `get_status_color_map()`: Maps status display names to hex colors for visualizations (maps, tables)

**How to Use in Your App:**
```r
# Source the helpers at the top of your app.R
source("../../shared/db_helpers.R")

# In your server function
source_colors <- get_status_colors()
shiny_colors <- get_shiny_colors()
color_map <- get_status_color_map()

# Access facility or FOS data
facilities <- get_facility_choices(include_all = TRUE)
```

## Application URLs

### Local Development
- **Main Dashboard**: `http://localhost:3838/`
- **Mosquito Monitoring**: `http://localhost:3838/mosquito-monitoring/`
- **SUCO History**: `http://localhost:3838/suco_history/`
- **Drone Treatment**: `http://localhost:3838/drone/`
- **Structural Treatment**: `http://localhost:3838/struct_trt/`
- **Cattail Management**: `http://localhost:3838/cattail/`
- **Control Efficacy**: `http://localhost:3838/control_efficacy/`
- **Ground Prehatch Progress**: `http://localhost:3838/ground_prehatch_progress/`
- **Red Air Pipeline**: `http://localhost:3838/red_air/`

### Production Server
Replace `your-server.com` with your actual server address:
- **Main Dashboard**: `http://your-server.com:3838/`
- **Applications**: `http://your-server.com:3838/app-name/`

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

#### Docker Troubleshooting

If you encounter "port already in use" errors:

1. **Check what's using the port**:
   ```bash
   sudo lsof -i :3838
   ```

2. **Kill processes using the port**:
   ```bash
   sudo kill -9 <PID>
   ```

3. **Use a different port**:
   ```bash
   docker run -p 4000:3838 mmcd-dashboard
   ```

4. **Clean up Docker resources**:
   ```bash
   docker system prune
   ```

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