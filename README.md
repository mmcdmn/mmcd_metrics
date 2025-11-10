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

### Shiny Server Setup

Download and install Shiny Server for production deployment:

```bash
# Download Shiny Server
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb

# Install Shiny Server
sudo gdebi -n shiny-server-1.5.22.1017-amd64.deb

# Copy applications to Shiny Server directory
sudo cp -r $MMCD_WORKSPACE/apps/* /srv/shiny-server/

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

#### Troubleshooting

**"Package not found" errors:**
```powershell
# Install the missing package
& "C:\Program Files\R\R-4.5.2\bin\R.exe" -e "install.packages('package-name', repos='https://cran.rstudio.com/', lib=Sys.getenv('R_LIBS_USER'))"
```

**"Cannot connect to database" errors:**
- Verify your `.env` file exists and contains correct credentials
- Ensure you have network access to the database server
- Check that the database server allows connections from your IP

**Port already in use:**
```powershell
# Use a different port
& "C:\Program Files\R\R-4.5.2\bin\R.exe" -e "shiny::runApp(port=4000, host='127.0.0.1')"
```

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
git clone https://github.com/ablepacifist/mmcd_metrics_1.git
cd mmcd_metrics_1

# Step 5: Install Shiny Server
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb
sudo gdebi -n shiny-server-1.5.22.1017-amd64.deb

# Step 6: Deploy applications
sudo cp -r $MMCD_WORKSPACE/apps/* /srv/shiny-server/
sudo chown -R shiny:shiny /srv/shiny-server

# Step 7: Start services
sudo systemctl start shiny-server
sudo systemctl enable shiny-server
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

## Adding New Applications

### Step 1: Create Application Directory
```bash
mkdir -p $MMCD_WORKSPACE/apps/your-new-app
```

### Step 2: Create Modular App Structure


```bash
# Create the application directory structure
mkdir -p $MMCD_WORKSPACE/apps/your-new-app
cd $MMCD_WORKSPACE/apps/your-new-app
```

#### Create `app.R` (Main Application File)
```r
# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DBI)
  library(dplyr)
  # ... other libraries as needed
})

# Source the shared database helper functions
source("../../shared/db_helpers.R")


server <- function(input, output, session) {

  # Get centralized colors
  source_colors <- get_status_colors()
  shiny_colors <- get_shiny_colors()
  color_map <- get_status_color_map()
  
  # Get facility choices
  facilities <- get_facility_choices(include_all = TRUE)
  

```

### Step 3: Update Landing Page
Add your new application to `$MMCD_WORKSPACE/apps/index.html`:

```html
<div class="app-card">
    <h3>Your New App</h3>
    <p>Description of what your new application does.</p>
    <a href="/your-new-app/" class="app-link" target="_blank">Open Application</a>
</div>
```


### Step 4: Deploy to Shiny Server
```bash
# Copy the entire application directory to Shiny Server
sudo cp -r $MMCD_WORKSPACE/apps/your-new-app /srv/shiny-server/
sudo chown -R shiny:shiny /srv/shiny-server/your-new-app

# IMPORTANT: Always copy updated index.html after making changes
sudo cp $MMCD_WORKSPACE/apps/index.html /srv/shiny-server/

# Restart Shiny Server to ensure new applications are loaded
sudo systemctl restart shiny-server

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

### **CRITICAL DEPLOYMENT NOTE**
**Always copy `index.html` to Shiny Server after making changes!**

Shiny Server serves files from `/srv/shiny-server/`, not from your development directory. Any changes to `apps/index.html` in your workspace will NOT be visible until you copy them:

```bash
# This command is REQUIRED after ANY change to index.html
sudo cp $MMCD_WORKSPACE/apps/index.html /srv/shiny-server/
```

**When to run this command:**
- After adding new applications to the dashboard
- After updating application descriptions or links  
- After changing any HTML/CSS in the landing page
- After modifying button links or layout

**Why this is necessary:**
- The development files are in `$MMCD_WORKSPACE/apps/`
- Shiny Server serves files from `/srv/shiny-server/`
- These are separate directories - changes in one don't automatically appear in the other

## Troubleshooting

### Common Issues and Solutions

#### Application Not Loading
```bash
# Check if Shiny Server is running
sudo systemctl status shiny-server

# Check application logs
sudo tail -f /var/log/shiny-server/*.log

# Verify file permissions
ls -la /srv/shiny-server/
```

#### R Package Installation Errors
```bash
# For sf package compilation issues
sudo apt install -y libgdal-dev libudunits2-dev libproj-dev

# For database connectivity issues
sudo apt install -y libpq-dev

# For text rendering issues
sudo apt install -y libfontconfig1-dev libfreetype-dev
```

#### Database Connection Issues
```bash
# Test database connectivity (using environment variables)
R -e "
if (file.exists('.env')) readRenviron('.env');
library(DBI); library(RPostgreSQL);
con <- dbConnect(PostgreSQL(), 
  host=Sys.getenv('DB_HOST'), 
  dbname=Sys.getenv('DB_NAME'), 
  user=Sys.getenv('DB_USER'), 
  password=Sys.getenv('DB_PASSWORD')); 
dbListTables(con); dbDisconnect(con)"

# If database connection fails, check network connectivity
ping your-database-host.com

# For development/testing without database access, you can modify app.R files to use sample data
# instead of live database connections
```

#### Button Click Issues on Dashboard
If application buttons on the main dashboard don't respond when clicked:

1. **Check if index.html changes were deployed**: 
   ```bash
   # ALWAYS run this after changing index.html
   sudo cp $MMCD_WORKSPACE/apps/index.html /srv/shiny-server/
   ```

2. **Browser pop-up blocking**: If links have `target="_blank"`, browsers may block pop-ups
   - Remove `target="_blank"` from links in index.html
   - Or allow pop-ups for localhost in browser settings

3. **Check browser console for JavaScript errors**: Press F12 and look for errors in Console tab

4. **Verify application availability**: Test individual app URLs directly (e.g., `http://localhost:3838/test-app/`)

5. **Check Shiny Server logs**: `sudo tail -f /var/log/shiny-server.log`

6. **Common causes**:
   - Database connectivity issues (applications timeout during initialization)
   - Missing R packages for the shiny user
   - Network firewall blocking database connections
   - Applications taking too long to load due to large datasets
   - Outdated index.html in Shiny Server directory

7. **Test with the Test Application**: The test app should always work since it doesn't require database connectivity

#### Network Connectivity for Database Access
The MMCD applications require access to your database server on port 5432. If you're experiencing connection timeouts:

```bash
# Test if the database server is reachable (replace with your actual host)
ping your-database-host.com
telnet your-database-host.com 5432

# Check firewall rules
sudo ufw status

# For testing purposes, you can modify applications to use sample data instead of live connections
```
