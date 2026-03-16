# Installation & Deployment Guide

## System Requirements
- **Operating System**: Ubuntu 18.04+ or similar Debian-based Linux distribution
- **R Version**: R 4.0+ (tested with R 4.5.2)
- **Memory**: Minimum 4GB RAM (8GB recommended for production)
- **Storage**: At least 2GB free space for dependencies and applications
- **Network**: Internet access for package installation and database connectivity

## Distributed Cache (Redis)
The apps use Redis for shared caching across containers. This repo currently hardcodes Redis to the local Docker host name `mmcd-redis` (port 6379, db 0, prefix `mmcd:`, no password). Ensure all app containers run on the same Docker network as the `mmcd-redis` container so they can reach it by name.

## Prerequisites - System Dependencies
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

## R Package Installation
Install R packages in the correct order to handle dependencies. For Shiny Server deployment, packages must be installed system-wide to be accessible to the shiny user:

```bash
# Install core packages (system-wide for Shiny Server)
sudo R -e "install.packages(c( \
  'shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'RPostgres', \
  'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', \
  'plotrix', 'dtplyr', 'vroom', 'tidyverse', 'tidyr', \
  'classInt', 's2', \
  'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster', \
  'plotly', 'purrr', 'tibble', 'pool', 'yaml' \
), lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"

# Install additional visualization packages for SF mapping
sudo R -e "install.packages(c('viridis', 'gridExtra', 'ggspatial', 'rosm', 'prettymapr'), \
  lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"

# Install PooledInfRate for MLE calculations (trap_survillance_test app)
sudo R -e "install.packages('remotes', lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/'); \
  remotes::install_github('CDCgov/PooledInfRate')"
```

## Shiny Server Setup

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

## Running Apps Individually with R (Without Docker)

You can run any Shiny app directly using R without Docker or Shiny Server. This is ideal for local development and testing.

### Windows Setup

**NOTE:** Has not actually worked yet but theoretically it can

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
& "C:\Program Files\R\R-4.5.2\bin\R.exe" -e "dir.create(Sys.getenv('R_LIBS_USER'), recursive=TRUE, showWarnings=FALSE); install.packages(c('shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'RPostgres', 'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', 'plotrix', 'dtplyr', 'vroom', 'tidyverse', 'classInt', 's2', 'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster', 'plotly', 'purrr', 'tibble', 'yaml'), repos='https://cran.rstudio.com/', lib=Sys.getenv('R_LIBS_USER'), dependencies=TRUE)"
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

### Linux/Mac Setup

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
  'plotly', 'purrr', 'tibble', 'yaml' \
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

### Running Different Apps

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

### Using RStudio (Optional)

If you have RStudio installed:

1. Open RStudio
2. Open the `app.R` file from any app directory (e.g., `apps/test-app/app.R`)
3. Click the **"Run App"** button in the top-right of the editor
4. The app will launch in a browser or RStudio viewer

### Stopping the App

To stop a running Shiny app:
- Press `Ctrl+C` in the terminal/PowerShell window
- Or close the terminal window

## Production Deployment

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
  'plotly', 'purrr', 'tibble', 'yaml' \
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

## Environment Variables Setup

### Local Development
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

### Production Deployment
In production (AWS App Runner, Docker, etc.), set these environment variables:
- `DB_HOST`
- `DB_PORT` 
- `DB_USER`
- `DB_PASSWORD`
- `DB_NAME`

The application will automatically use environment variables if no .env file is found.

## Docker Deployment

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

### Windows (Docker Desktop + WSL2) Quick Start

Use this section when working on Windows and testing the full local stack (OpenResty/nginx + multiple Shiny workers).

```powershell
# 1) Install Docker Desktop (one time)
winget install --id Docker.DockerDesktop -e

# 2) Verify WSL2 backend is available
wsl --status
wsl -l -v

# 3) Verify Docker engine is ready
docker version
```

If Docker is installed but `docker` is not recognized in your current PowerShell session, open a new terminal window and run `docker version` again.

#### Build and Run Using Existing `.env`

From the repository root:

```powershell
cd C:\Users\yourusername\Documents\mmcd_metrics

# Build image (repo uses lowercase "dockerfile")
docker build -f dockerfile -t mmcd-dashboard:local .

# Run container with your existing .env values
docker run -d --name mmcd-local -p 3838:3838 --env-file .env mmcd-dashboard:local

# Confirm startup and mode
docker ps --filter "name=mmcd-local"
docker logs --tail 200 mmcd-local
```

Open: `http://localhost:3838`

#### Rebuild Loop After Code Changes (Windows)

Most changes in this repo are copied into the image at build time. To see updates, rebuild and restart:

```powershell
cd C:\Users\yourusername\Documents\mmcd_metrics

# Stop/remove old container
docker rm -f mmcd-local

# Rebuild image from latest source
docker build -f dockerfile -t mmcd-dashboard:local .

# Start fresh container with same env file
docker run -d --name mmcd-local -p 3838:3838 --env-file .env mmcd-dashboard:local

# Check logs
docker logs --tail 120 mmcd-local
```

Tip: Use a hard browser refresh (Ctrl+F5) after restart to avoid cached JS/CSS.

#### Common Windows Docker Troubleshooting

- Error: `exec /startup.sh: no such file or directory`
  - Cause: line endings in `startup.sh` are CRLF.
  - Fix: keep the Dockerfile step that normalizes line endings (`sed -i 's/\r$//' /startup.sh`) and rebuild.

- Error: Docker daemon not running
  - Start Docker Desktop, wait until status is "Engine running", then retry.

- Port already in use on 3838
  - Stop the previous container: `docker rm -f mmcd-local`
  - Or map a different host port: `-p 3839:3838`

### AWS Deployment with Secure Environment Variables

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
