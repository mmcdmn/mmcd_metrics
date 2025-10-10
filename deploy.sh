#!/bin/bash
# MMCD Shiny Applications Deployment Script
# Updated for the complete MMCD analytics platform

set -e  # Exit on any error

echo "Starting MMCD Shiny Applications Deployment..."

# Check if running as root
if [[ $EUID -eq 0 ]]; then
   echo "ERROR: This script should not be run as root for security reasons"
   echo "   Run as a regular user with sudo privileges"
   exit 1
fi

# Set workspace path - user should modify this
MMCD_WORKSPACE="${MMCD_WORKSPACE:-$(pwd)}"
echo "Using workspace: $MMCD_WORKSPACE"

# Validate workspace
if [[ ! -d "$MMCD_WORKSPACE/apps" ]]; then
    echo "ERROR: apps directory not found in $MMCD_WORKSPACE"
    echo "   Please set MMCD_WORKSPACE environment variable or run from the project root"
    exit 1
fi

# Update system packages
echo "Updating system packages..."
sudo apt update

# Install R and required system dependencies
echo "Installing R and comprehensive system dependencies..."
sudo apt install -y \
    r-base r-base-dev gdebi-core \
    libcurl4-openssl-dev libssl-dev libxml2-dev libpq-dev \
    libgdal-dev libudunits2-dev libproj-dev \
    libfontconfig1-dev libfreetype-dev libpng-dev \
    libharfbuzz-dev libfribidi-dev \
    gfortran cmake libabsl-dev

# Install Shiny Server
echo "Installing Shiny Server..."
if ! command -v shiny-server &> /dev/null; then
    echo "   Downloading Shiny Server..."
    wget -q https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb
    echo "   Installing Shiny Server..."
    sudo gdebi -n shiny-server-1.5.22.1017-amd64.deb
    rm shiny-server-1.5.22.1017-amd64.deb
else
    echo "   Shiny Server already installed"
fi

# Install required R packages (both user and system-wide)
echo "Installing R packages..."
echo "   Installing packages for current user..."
R -e "
packages <- c('shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 
              'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', 
              'plotrix', 'dtplyr', 'vroom', 'tidyverse', 'classInt', 's2', 
              'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster')
install.packages(packages, repos='https://cran.rstudio.com/')
"

echo "   Installing packages system-wide for Shiny Server..."
sudo R -e "
packages <- c('shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 
              'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', 
              'plotrix', 'dtplyr', 'vroom', 'tidyverse', 'classInt', 's2', 
              'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster')
install.packages(packages, lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')
"

# Create necessary directories
echo "Creating directories..."
sudo mkdir -p /var/log/shiny-server
sudo mkdir -p /srv/shiny-server
sudo mkdir -p /etc/shiny-server

# Copy applications to Shiny Server directory
echo "Copying applications..."
sudo cp -r "$MMCD_WORKSPACE/apps"/* /srv/shiny-server/
sudo chown -R shiny:shiny /srv/shiny-server

# Create default Shiny Server configuration
echo "Setting up Shiny Server configuration..."
sudo tee /etc/shiny-server/shiny-server.conf > /dev/null <<EOF
# Define the user to run applications as
run_as shiny;

# Define a server that listens on port 3838
server {
  listen 3838;

  # Define a location at the base URL
  location / {

    # Host the directory of Shiny Apps stored in this directory
    site_dir /srv/shiny-server;

    # Log all Shiny output to files in this directory
    log_dir /var/log/shiny-server;

    # When a user visits the base URL rather than a particular application,
    # an index of the applications available in this directory will be shown.
    directory_index on;
  }
}
EOF

# Start and enable Shiny Server
echo "Starting Shiny Server..."
sudo systemctl restart shiny-server
sudo systemctl enable shiny-server

# Check if Shiny Server is running
if sudo systemctl is-active --quiet shiny-server; then
    echo "SUCCESS: Shiny Server is running"
else
    echo "ERROR: Shiny Server failed to start"
    echo "   Check logs: sudo journalctl -u shiny-server -f"
    exit 1
fi

# Test applications
echo "Testing deployment..."
sleep 5  # Give Shiny Server time to start

# Test main dashboard
if curl -s -f http://127.0.0.1:3838/ > /dev/null; then
    echo "SUCCESS: Main dashboard is accessible"
else
    echo "WARNING: Main dashboard test failed"
fi

# Test the test application
if curl -s -f http://127.0.0.1:3838/test-app/ > /dev/null; then
    echo "SUCCESS: Test application is accessible"
else
    echo "WARNING: Test application not accessible (may need database connectivity)"
fi

echo ""
echo "DEPLOYMENT COMPLETE!"
echo ""
echo "Your applications are now available at:"
echo "   Main Dashboard: http://$(hostname -I | awk '{print $1}'):3838/"
echo "   Test Application: http://$(hostname -I | awk '{print $1}'):3838/test-app/"
echo "   Mosquito Monitoring: http://$(hostname -I | awk '{print $1}'):3838/mosquito-monitoring/"
echo "   SUCO History: http://$(hostname -I | awk '{print $1}'):3838/suco_history/"
echo "   Drone Treatment Progress: http://$(hostname -I | awk '{print $1}'):3838/drone_trt_progress/"
echo "   Structural Treatment Progress: http://$(hostname -I | awk '{print $1}'):3838/struct_trt_progress/"
echo "   Cattail Planned Treatments: http://$(hostname -I | awk '{print $1}'):3838/cattail_planned_trt/"
echo "   Drone Treatment History: http://$(hostname -I | awk '{print $1}'):3838/drone_trt_history/"
echo ""
echo "Next steps:"
echo "   1. Test all applications in your browser"
echo "   2. Check Shiny Server logs if any applications fail: sudo tail -f /var/log/shiny-server.log"
echo "   3. For production, consider setting up Nginx reverse proxy and SSL"
echo "   4. Ensure database connectivity for the MMCD applications"
echo ""
echo "Troubleshooting:"
echo "   - If buttons don't work: Check browser console (F12)"
echo "   - If apps timeout: Check database connectivity to data.mmcd.org"
echo "   - View logs: sudo journalctl -u shiny-server -f"
echo "   - Restart: sudo systemctl restart shiny-server"
