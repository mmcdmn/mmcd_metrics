# MMCD Metrics Dashboard

![MMCD Analytics](https://img.shields.io/badge/MMCD-Analytics-blue)
![R Shiny](https://img.shields.io/badge/R-Shiny-276DC3)
![Docker](https://img.shields.io/badge/Docker-Enabled-2496ED)
![QGIS Server](https://img.shields.io/badge/QGIS-Server-589632)
![Status](https://img.shields.io/badge/Status-Active-green)

A comprehensive analytics platform for the Metropolitan Mosquito Control District (MMCD), providing interactive dashboards for mosquito surveillance, treatment analysis, and operational metrics. Built with R Shiny and enhanced with QGIS Server for professional cartographic rendering.

## Quick Start

```bash
# Clone and navigate to project
cd mmcd_metrics

# Development setup with Docker
cp .env.example .env  # Configure database credentials
docker build -t mmcd-dashboard .
docker run -d --name mmcd_metrics -p 3838:3838 -p 8080:80 --env-file .env mmcd-dashboard

# Access applications
# Main Dashboard: http://localhost:3838
# QGIS Server: http://localhost:8080/qgis/
```

## Table of Contents

- [Architecture](#architecture)
  - [System Overview](#system-overview)
  - [Directory Structure](#directory-structure)
  - [Centralized Helpers Module](#centralized-helpers-module-shareddb_helpersr)
  - [QGIS Server Architecture](#qgis-server-architecture)
- [Applications](#applications)
  - [Surveillance & Monitoring](#surveillance--monitoring)
    - [Mosquito Monitoring](#mosquito-monitoring)
    - [SUCO History](#suco-history)
    - [Mosquito Surveillance Map](#mosquito-surveillance-map)
  - [Treatment Management](#treatment-management)
    - [Drone Treatment](#drone-treatment)
    - [Structural Treatment](#structural-treatment)
    - [Ground Prehatch Progress](#ground-prehatch-progress)
    - [Cattail Management](#cattail-management)
    - [Red Air Pipeline](#red-air-pipeline)
    - [Control Efficacy](#control-efficacy)
  - [Development & Testing](#development--testing)
    - [QGIS Server Demo](#qgis-server-demo)
    - [Test Application](#test-application)
- [Installation & Deployment](#installation--deployment)
  - [System Requirements](#system-requirements)
  - [Docker Deployment](#docker-deployment)
    - [Development Setup](#development-setup)
    - [Production Deployment](#production-deployment)
  - [Manual Installation](#manual-installation)
  - [QGIS Server Configuration](#qgis-server-configuration)
- [Application URLs](#application-urls)
- [Development Guide](#development-guide)
  - [Adding New Applications](#adding-new-applications)
  - [Using Shared Helpers](#using-shared-helpers)
  - [Docker Development Workflow](#docker-development-workflow)
- [Troubleshooting](#troubleshooting)
- [Contributing](#contributing)

## Architecture

### System Overview

The MMCD Metrics Dashboard is a multi-application platform built on:
- **R Shiny**: Interactive web applications
- **QGIS Server**: Professional map rendering (WMS/WFS protocols)
- **PostgreSQL + PostGIS**: Spatial database
- **Docker**: Containerized deployment with Apache, Shiny Server, and QGIS Server
- **Modular Design**: Shared utilities and consistent styling across all apps

### Directory Structure

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
│   └── qgis_demo/                # QGIS Server integration demo
│       ├── app.R                 # Main application with WMS integration
│       └── README.md             # Detailed technical documentation
├── dockerfile                    # Docker container configuration
├── shiny-server.conf             # Shiny Server routing configuration
└── startup.sh                    # Container initialization script
```




## Applications

### Surveillance & Monitoring

#### Mosquito Monitoring
- **Path**: `/mosquito-monitoring/`
- **Purpose**: CO2 trap mosquito surveillance analysis
- **Key Features**: 
  - Species-specific analysis with 50+ mosquito species
  - Facility and zone comparisons
  - Interactive time series with hover tooltips and standard error visualization
  - Logarithmic scale options for data visualization

#### SUCO History
- **Path**: `/suco_history/` | [📄 Technical Notes](apps/suco_history/NOTES.md)
- **Purpose**: Surveillance Count (SUCO) historical analysis dashboard
- **Modular Structure**: `app.R` (main logic) | `data_functions.R` (queries) | `display_functions.R` (visualizations) | `ui_helpers.R` (UI components)
- **Key Features**:
  - Interactive leaflet maps with custom marker sizing and spatial data visualization
  - Facility and foreman filtering with consistent colors from `db_helpers.R`
  - Temporal trend analysis (weekly/monthly with epidemiological weeks)
  - Top locations identification with species-based analysis and P1/P2 zone support
  - Comprehensive tooltip customization and stacked bar charts

#### Mosquito Surveillance Map
- **Path**: `/mosquito_surveillance_map/`
- **Purpose**: Geographic visualization of mosquito surveillance data
- **Key Features**:
  - Shapefile-based boundary visualization
  - Interactive mapping with surveillance data overlay

### Treatment Management

#### Drone Treatment
- **Path**: `/drone/` | [📄 Technical Notes](apps/drone/NOTES.md)
- **Purpose**: Comprehensive drone treatment tracking with real-time progress and historical analysis
- **Modular Structure**: `app.R` | `historical_functions.R` | `site_average_functions.R`
- **Key Features**:
  - **Progress Tab**: Real-time tracking of drone-based treatment operations
  - **Historical Tab**: Trend analysis with percentage and count views
  - **Site Average Tab**: Site-level calculations and statistical analysis
  - Active treatment area monitoring, P1/P2 zone filtering with alpha transparency
  - Multiple display metrics (sites, treatments, acres) with facility/FOS/section grouping

#### Structural Treatment
- **Path**: `/struct_trt/` | [📄 Technical Notes](apps/struct_trt/NOTES.md)
- **Purpose**: Comprehensive structural treatment tracking with current progress and historical analysis
- **Modular Structure**: `app.R` | `data_functions.R` | `display_functions.R`
- **Key Features**:
  - **Progress Tab**: Monitor current structural treatment activities
  - **History Tab**: Historical analysis with time series and breakdowns by facility/type/priority
  - Proportion of structures under treatment tracking with customizable duration settings
  - Date simulation ("pretend today is") functionality for scenario testing
  - Snapshot and priority breakdowns for comprehensive analysis

#### Ground Prehatch Progress
- **Path**: `/ground_prehatch_progress/` | [📄 Technical Notes](apps/ground_prehatch_progress/NOTES.md)
- **Purpose**: Track and analyze ground prehatch treatment progress and performance
- **Modular Structure**: `app.R` | `data_functions.R` | `display_functions.R` | `ui_helpers.R`
- **Key Features**:
  - **Clean persistent filter panel** that stays visible when switching between tabs
  - **Responsive design** with gradient headers and improved spacing
  - **Progress Overview Tab**: Real-time tracking with summary value boxes and performance metrics
  - **Detailed View Tab**: Comprehensive site details table with download functionality
  - Facility-level performance comparisons with P1/P2 zone support and goal tracking

#### Cattail Management
- **Path**: `/cattail/` | [📄 Technical Notes](apps/cattail/NOTES.md)
- **Purpose**: Comprehensive cattail management dashboard with inspection tracking and treatment planning
- **Modular Structure**: `app.R` | `planned_treatment_functions.R` | `progress_functions.R` | `historical_functions.R`
- **Key Features**:
  - **Inspection Progress Tab**: Track completed cattail inspections versus annual goals
  - **Treatment Planning Tab**: Planning and tracking dashboard for cattail treatments
  - Real-time inspection status monitoring and facility-level performance analysis
  - Planned treatment area visualization, resource allocation tracking
  - Centralized color system from `db_helpers.R` for consistent status visualization

#### Red Air Pipeline
- **Path**: `/red_air/`
- **Purpose**: Monitor air site status, rainfall impact, and treatment pipeline
- **Modular Structure**: `app.R` | `air_status_functions.R` | `flow_testing_functions.R`
- **Key Features**:
  - Interactive map showing site status with color-coded dots
  - Treatment lifecycle management: Needs Inspection → Under Threshold → Needs Treatment → Active Treatment
  - Rainfall tracking and analysis with real-time status summary chart
  - Flow testing and validation tools with detailed site information table
  - **Uses centralized colors from `db_helpers.R`** for consistent visualization

#### Control Efficacy
- **Path**: `/control_efficacy/`
- **Purpose**: Air treatment checkback analysis and efficacy monitoring
- **Key Features**:
  - Air treatment campaign identification (multi-day treatments grouped)
  - Configurable checkback requirements (percentage or fixed number)
  - Checkback completion rate tracking by facility
  - Treatment-to-checkback timing analysis with dip count change visualization
  - Site-level treatment and checkback details

### Development & Testing

#### QGIS Server Demo
- **Path**: `/qgis_demo/` | [📄 Technical Notes](apps/qgis_demo/README.md)
- **Purpose**: Demonstration of QGIS Server integration with R Shiny for professional cartographic rendering
- **Technical Stack**: QGIS Server 3.28 | Apache2 | PostGIS | Leaflet | WMS 1.3.0
- **Key Features**:
  - Dynamic QGIS project generation based on user-selected filters
  - WMS (Web Map Service) layer rendering from QGIS Server
  - Interactive facility filtering with SQL-based data queries
  - Professional map rendering with QGIS styling capabilities
  - Real-time map refresh with filtered PostGIS data
  - Leaflet map viewer with QGIS WMS overlay and facility location markers
  - Coordinate transformation from UTM to WGS84 (EPSG:4326)
  - Data table view with facility details
- **Architecture Flow**:
  - User selects filters → R Shiny generates `.qgs` project file → QGIS Server renders filtered PostGIS data as WMS tiles → Displayed in Leaflet map
  - Dynamic SQL filtering applied at database layer for optimal performance
- **Use Cases**:
  - Testing QGIS Server integration patterns for production apps
  - Prototyping professional map rendering workflows
  - Demonstrating server-side cartographic capabilities
  - Building foundation for styled operational maps

#### Test Application
- **Path**: `/test-app/`
- **Purpose**: Testing and validation environment
- **Features**: Useful for verifying application links on dashboard and testing new features

## Installation & Deployment

### System Requirements

| Component | Requirement |
|-----------|-------------|
| **Operating System** | Ubuntu 18.04+ or Debian-based Linux |
| **R Version** | R 4.0+ (tested with R 4.5.1) |
| **Memory** | Minimum 4GB RAM (8GB recommended for production) |
| **Storage** | At least 2GB free space for dependencies |
| **Network** | Internet access for package installation and database connectivity |
| **Docker** | Docker Engine 20.10+ (for containerized deployment) |

### Docker Deployment

**Recommended Method**: Docker provides the easiest deployment with all dependencies pre-configured.

#### Development Setup
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

### QGIS Server Demo
- **Path**: `/qgis_demo/`
- **Purpose**: Demonstration of QGIS Server integration with R Shiny for professional cartographic rendering
- **Features**:
  - Dynamic QGIS project generation based on user filters
  - WMS (Web Map Service) layer rendering from QGIS Server
  - Interactive facility filtering with SQL-based data queries
  - Professional map rendering with QGIS styling capabilities
  - PostGIS database integration for spatial data
  - Real-time map refresh with filtered data
  - Leaflet map viewer with QGIS WMS overlay
  - Red marker indicators for facility locations
  - Data table view with facility details
- **Technical Stack**:
  - QGIS Server 3.28 (map rendering engine)
  - Apache2 (web server proxy for QGIS Server)
  - PostGIS (spatial database)
  - Leaflet (client-side map display)
  - WMS 1.3.0 protocol
- **Architecture**:
  - User selects filters → R Shiny generates .qgs project file → QGIS Server renders filtered PostGIS data as WMS tiles → Displayed in Leaflet map
  - Coordinate transformation from UTM to WGS84 (EPSG:4326) for proper display
  - Dynamic SQL filtering applied at database layer
- **Use Cases**:
  - Testing QGIS Server integration patterns
  - Prototyping professional map rendering for production apps
  - Demonstrating server-side cartographic capabilities
  - Building foundation for styled operational maps

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
- **QGIS Server Demo**: `http://localhost:3838/qgis_demo/`

### Production Server
Replace `your-server.com` with your actual server address:
- **Main Dashboard**: `http://your-server.com:3838/`
- **Applications**: `http://your-server.com:3838/app-name/`

### Docker Deployment

#### Development Setup

**For local development with live code reloading:**

```bash
# Step 1: Create .env file with your database credentials
cp .env.example .env
# Edit .env with your local database settings (DB_HOST, DB_NAME, DB_USER, DB_PASSWORD)

# Step 2: Build the Docker image
docker build -t mmcd-dashboard .

# Step 3: Run container (stop and remove old container first if exists)
docker stop mmcd_metrics 2>/dev/null || true
docker rm mmcd_metrics 2>/dev/null || true
docker run -d --name mmcd_metrics -p 3838:3838 -p 8080:80 --env-file .env mmcd-dashboard

# Step 4: Verify startup
docker logs -f mmcd_metrics
# Wait for: "Shiny Server started" and "Apache httpd web server running"
```

**Development Access Points:**
- 🌐 **Shiny Dashboard**: `http://localhost:3838`
- 🗺️ **QGIS Server**: `http://localhost:8080/qgis/`
- 📍 **QGIS Demo App**: `http://localhost:3838/qgis_demo/`

**Quick Development Commands:**

```bash
# View live logs
docker logs -f mmcd_metrics

# Check running containers
docker ps

# Stop/remove container
docker stop mmcd_metrics && docker rm mmcd_metrics

# Full restart cycle after code changes (one command)
docker build -t mmcd-dashboard . && \
docker stop mmcd_metrics && docker rm mmcd_metrics && \
docker run -d --name mmcd_metrics -p 3838:3838 -p 8080:80 --env-file .env mmcd-dashboard

# Access container shell for debugging
docker exec -it mmcd_metrics /bin/bash
```

#### Production Deployment

**For production environments:**

```bash
# Build the Docker image
docker build -t mmcd-dashboard .

# Run with environment variables (AWS ECS/EC2, no .env file)
docker run -d --name mmcd_metrics \
  -p 3838:3838 -p 8080:80 \
  -e DB_HOST=your-db-host \
  -e DB_NAME=your-db-name \
  -e DB_USER=your-db-user \
  -e DB_PASSWORD=your-db-password \
  mmcd-dashboard

# Or with .env file (if available)
docker run -d --name mmcd_metrics \
  -p 3838:3838 -p 8080:80 \
  --env-file .env \
  mmcd-dashboard
```

**Container Services:**
- **Shiny Server** (port 3838): Hosts all R Shiny applications
- **Apache2** (port 80 → host port 8080): Web server proxy for QGIS Server
- **QGIS Server** (via Apache): Renders maps using WMS protocol

**Production Best Practices:**
- Use AWS Secrets Manager or environment variables (not .env files)
- Set up reverse proxy (nginx) for SSL/TLS termination
- Configure log rotation for Docker logs
- Implement health checks and auto-restart policies
- Use Docker volumes for persistent data if needed

### Manual Installation

**For non-Docker deployments**, install system dependencies and R packages:

The QGIS Server integration enables professional cartographic rendering with dynamic filtering. Here's how it works:

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          User Browser                                    │
│  ┌────────────────────────────────────────────────────────────────────┐ │
│  │  Leaflet Map Viewer                                                 │ │
│  │  - Base map tiles                                                   │ │
│  │  - WMS overlay from QGIS Server                                     │ │
│  │  - Interactive markers                                              │ │
│  └────────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────┘
                                    ▲
                                    │ HTTP Requests (WMS GetMap)
                                    │
┌─────────────────────────────────────────────────────────────────────────┐
│                    Docker Container (mmcd_metrics)                       │
│                                                                          │
│  ┌─────────────────────────────────────────────────────────────┐       │
│  │  R Shiny Server (Port 3838)                                  │       │
│  │  ┌─────────────────────────────────────────────────────┐    │       │
│  │  │  QGIS Demo App (/qgis_demo/)                        │    │       │
│  │  │                                                      │    │       │
│  │  │  1. User selects filters (facility, date range)     │    │       │
│  │  │  2. Click "Refresh Map"                             │    │       │
│  │  │  3. App generates SQL filter (e.g., abbrv='E')      │    │       │
│  │  │  4. Creates .qgs project file with filter           │    │       │
│  │  │  5. Saves to /qgis/projects/facilities_E_*.qgs      │    │       │
│  │  │  6. Renders Leaflet map with WMS URL                │    │       │
│  │  └─────────────────────────────────────────────────────┘    │       │
│  └─────────────────────────────────────────────────────────────┘       │
│                                                                          │
│  ┌─────────────────────────────────────────────────────────────┐       │
│  │  Apache + QGIS Server (Port 80 → Host 8080)             │       │
│  │                                                           │       │
│  │  1. Receives WMS GetMap request                          │       │
│  │  2. Reads .qgs project file                              │       │
│  │  3. Connects to PostGIS with SQL filter                  │       │
│  │  4. Queries: SELECT * FROM gis_facility WHERE abbrv='E'  │       │
│  │  5. Renders geometries with QGIS styling                 │       │
│  │  6. Returns PNG tiles to browser                         │       │
│  └─────────────────────────────────────────────────────────────┘       │
│                                                                          │
│  ┌─────────────────────────────────────────────────────────────┐       │
│  │  Shared Directory: /qgis/projects/                          │       │
│  │  - facilities_all_20251112_153045.qgs                       │       │
│  │  - facilities_E_20251112_153128.qgs                         │       │
│  │  - facilities_Wp_20251112_153256.qgs                        │       │
│  └─────────────────────────────────────────────────────────────┘       │
└─────────────────────────────────────────────────────────────────────────┘
                                    ▲
                                    │ SQL Queries
                                    │
┌─────────────────────────────────────────────────────────────────────────┐
│                    PostgreSQL + PostGIS Database                         │
│                                                                          │
│  ┌─────────────────────────────────────────────────────────────┐       │
│  │  gis_facility table                                          │       │
│  │  ┌───────┬──────────┬─────────────────────────────┐         │       │
│  │  │ gid   │ abbrv    │ the_geom (MultiPolygon)     │         │       │
│  │  ├───────┼──────────┼─────────────────────────────┤         │       │
│  │  │ 1     │ Sj       │ MULTIPOLYGON(((...)))       │         │       │
│  │  │ 2     │ Sr       │ MULTIPOLYGON(((...)))       │         │       │
│  │  │ 3     │ Wp       │ MULTIPOLYGON(((...)))       │         │       │
│  │  │ 4     │ E        │ MULTIPOLYGON(((...)))       │         │       │
│  │  └───────┴──────────┴─────────────────────────────┘         │       │
│  │                                                               │       │
│  │  Coordinate System: UTM (transformed to EPSG:4326)           │       │
│  └─────────────────────────────────────────────────────────────┘       │
└─────────────────────────────────────────────────────────────────────────┘
```

**Data Flow:**
1. **User Interaction**: User selects filters in Shiny UI
2. **Project Generation**: R Shiny creates `.qgs` XML file with PostGIS connection and SQL filter
3. **WMS Request**: Leaflet map requests tiles from QGIS Server with project path
4. **Database Query**: QGIS Server connects to PostGIS and applies SQL filter
5. **Rendering**: QGIS renders geometries with professional styling
6. **Display**: Browser receives PNG tiles and overlays on Leaflet map

**Key Components:**
- **R Shiny**: UI, filter logic, project file generation
- **QGIS Server**: Professional map rendering engine (WMS protocol)
- **Apache**: Web server proxy for QGIS Server (FastCGI)
- **PostGIS**: Spatial database with geometry data
- **Leaflet**: Client-side map display library

**Benefits:**
- Server-side rendering reduces client load
- Professional cartographic quality
- Dynamic filtering without page reload
- Standard WMS protocol (interoperable)
- Complex styling capabilities from QGIS

### QGIS Server Configuration

The Docker container includes a fully configured QGIS Server setup:

| Configuration | Value |
|---------------|-------|
| **QGIS Version** | 3.28.0 |
| **Apache Module** | FastCGI enabled |
| **Project Storage** | `/qgis/projects/` (writable by shiny user) |
| **WMS Endpoint** | `http://localhost:8080/qgis/` |
| **Protocols** | WMS 1.3.0, WFS |

**Testing QGIS Server:**
```bash
# Check if QGIS Server is responding
curl "http://localhost:8080/qgis/?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetCapabilities"

# List QGIS project files in container
docker exec mmcd_metrics ls -la /qgis/projects/

# View Apache/QGIS Server logs
docker logs mmcd_metrics | grep -i qgis

# Check Apache status
docker exec mmcd_metrics service apache2 status
```

### Production Server
Replace `your-server.com` with your actual server address:
- **Main Dashboard**: `http://your-server.com:3838/`
- **Applications**: `http://your-server.com:3838/app-name/`
- **QGIS Server**: `http://your-server.com:8080/qgis/`

## Development Guide

### Docker Development Workflow

**Typical development cycle:**

1. **Make code changes** in `apps/` directory
2. **Rebuild container**:
   ```bash
   docker build -t mmcd-dashboard .
   ```
3. **Restart container**:
   ```bash
   docker stop mmcd_metrics && docker rm mmcd_metrics
   docker run -d --name mmcd_metrics -p 3838:3838 -p 8080:80 --env-file .env mmcd-dashboard
   ```
4. **Test changes** at `http://localhost:3838/app-name/`
5. **View logs** for debugging:
   ```bash
   docker logs -f mmcd_metrics
   ```

**One-liner for quick iterations:**
```bash
docker build -t mmcd-dashboard . && \
docker stop mmcd_metrics && docker rm mmcd_metrics && \
docker run -d --name mmcd_metrics -p 3838:3838 -p 8080:80 --env-file .env mmcd-dashboard
```

### Adding New Applications

**Step 1: Create app directory**
```bash
mkdir -p apps/new_app
```

**Step 2: Create app.R**
```r
# apps/new_app/app.R
library(shiny)
source("../../shared/db_helpers.R")  # Always source helpers

ui <- fluidPage(
  titlePanel("New App"),
  # Your UI here
)

server <- function(input, output, session) {
  # Your server logic here
}

shinyApp(ui = ui, server = server)
```

**Step 3: Update shiny-server.conf**
```bash
# Add location block in shiny-server.conf
location /new_app {
  app_dir /srv/shiny-server/apps/new_app;
  log_dir /var/log/shiny-server;
}
```

**Step 4: Update index.html**
Add link to main landing page at `apps/index.html`

**Step 5: Rebuild and test**
```bash
docker build -t mmcd-dashboard . && \
docker stop mmcd_metrics && docker rm mmcd_metrics && \
docker run -d --name mmcd_metrics -p 3838:3838 -p 8080:80 --env-file .env mmcd-dashboard
```

### Using Shared Helpers

The `shared/db_helpers.R` module provides centralized functionality:

```r
# At the top of your app.R
source("../../shared/db_helpers.R")

# Database connection
con <- get_db_connection()
data <- dbGetQuery(con, "SELECT * FROM table")
dbDisconnect(con)

# Facility lookups
facilities <- get_facility_choices(include_all = TRUE)

# FOS lookups
fos_list <- get_fos_choices()

# Centralized colors (updates everywhere when changed in db_helpers.R)
status_colors <- get_status_colors()
shiny_colors <- get_shiny_colors()
color_map <- get_status_color_map()

# Use in visualizations
leaflet() %>%
  addCircleMarkers(color = color_map["Active"])
```
