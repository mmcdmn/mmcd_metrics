#!/bin/bash

# MMCD Dashboard Deployment Package Creator
# This script creates a deployment package for the hostdash team using existing Docker setup

echo "Creating MMCD Dashboard deployment package..."

# Create deployment directory
DEPLOY_DIR="mmcd-dashboard-deploy"
rm -rf $DEPLOY_DIR
mkdir -p $DEPLOY_DIR

# Copy all necessary files
echo "Copying application files..."
cp -r apps/ $DEPLOY_DIR/
cp index.html $DEPLOY_DIR/
cp dockerfile $DEPLOY_DIR/Dockerfile 
cp shiny-server.conf $DEPLOY_DIR/

# Create an improved Dockerfile that works better cross-platform
cat > $DEPLOY_DIR/Dockerfile << 'EOF'
FROM rocker/shiny:latest

# Install system dependencies for geospatial and database packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev libpq-dev \
    libgdal-dev libudunits2-dev libproj-dev \
    libfontconfig1-dev libfreetype-dev libpng-dev \
    libharfbuzz-dev libfribidi-dev gfortran cmake gdebi-core \
    libgl1-mesa-dev libglu1-mesa libx11-dev libxt-dev libxft-dev \
    libtiff-dev libjpeg-dev libgeos-dev libgmp-dev libgsl-dev \
    libv8-dev libpoppler-cpp-dev libmagick++-dev && \
    rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c( \
  'shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'RPostgres', \
  'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', \
  'plotrix', 'dtplyr', 'vroom', 'tidyverse', \
  'classInt', 's2', \
  'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster', \
  'plotly', 'purrr', 'tibble' \
), lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"

# Remove existing shiny-server content
RUN rm -rf /srv/shiny-server/*

# Copy shiny-server configuration
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Copy index.html to root
COPY index.html /srv/shiny-server/index.html

# Copy apps directory
COPY apps/ /srv/shiny-server/apps/

# Set ownership and permissions
RUN chown -R shiny:shiny /srv/shiny-server && \
    chmod -R 755 /srv/shiny-server

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
EOF

# Create flexible docker-compose configurations
echo "Creating docker-compose configurations..."

# Default docker-compose.yml
cat > $DEPLOY_DIR/docker-compose.yml << 'EOF'
version: '3.8'
services:
  mmcd-dashboard:
    build: .
    ports:
      - "${HOST_PORT:-3838}:3838"  # Use HOST_PORT env var or default to 3838
    restart: unless-stopped
    environment:
      - SHINY_LOG_STDERR=1
    volumes:
      # Optional: mount logs for debugging
      - ./logs:/var/log/shiny-server/
    user: "${USER_ID:-1000}:${GROUP_ID:-1000}"  # Run as specified user/group
EOF

# Alternative: Non-root user docker-compose
cat > $DEPLOY_DIR/docker-compose-nonroot.yml << 'EOF'
version: '3.8'
services:
  mmcd-dashboard:
    build: 
      context: .
      dockerfile: Dockerfile-nonroot
    ports:
      - "${HOST_PORT:-8080}:3838"  # Default to 8080 for non-root
    restart: unless-stopped
    environment:
      - SHINY_LOG_STDERR=1
    volumes:
      - ./logs:/home/shiny/logs
    user: "1000:1000"  # Run as non-root user
EOF

# Create non-root Dockerfile
cat > $DEPLOY_DIR/Dockerfile-nonroot << 'EOF'
FROM rocker/shiny:latest

# Create a non-root user
RUN groupadd -g 1000 shiny && \
    useradd -u 1000 -g shiny -s /bin/bash -m shiny

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev libpq-dev \
    libgdal-dev libudunits2-dev libproj-dev \
    libfontconfig1-dev libfreetype-dev libpng-dev \
    libharfbuzz-dev libfribidi-dev gfortran cmake gdebi-core \
    libgl1-mesa-dev libglu1-mesa libx11-dev libxt-dev libxft-dev \
    libtiff-dev libjpeg-dev libgeos-dev libgmp-dev libgsl-dev \
    libv8-dev libpoppler-cpp-dev libmagick++-dev && \
    rm -rf /var/lib/apt/lists/*

# Install R packages as root, then switch to non-root
RUN R -e "install.packages(c( \
  'shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'RPostgres', \
  'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', \
  'plotrix', 'dtplyr', 'vroom', 'tidyverse', \
  'classInt', 's2', \
  'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster', \
  'plotly', 'purrr', 'tibble' \
), lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"

# Create app directory and set permissions
RUN mkdir -p /home/shiny/app /home/shiny/logs && \
    chown -R shiny:shiny /home/shiny

# Copy shiny-server config
COPY --chown=shiny:shiny shiny-server-nonroot.conf /home/shiny/shiny-server.conf

# Copy index.html
COPY --chown=shiny:shiny index.html /home/shiny/app/index.html

# Copy apps directory
COPY --chown=shiny:shiny apps/ /home/shiny/app/apps/

# Switch to non-root user
USER shiny
WORKDIR /home/shiny/app

EXPOSE 3838
CMD ["/usr/bin/shiny-server", "/home/shiny/shiny-server.conf"]
EOF

# Create non-root shiny-server config
cat > $DEPLOY_DIR/shiny-server-nonroot.conf << 'EOF'
# Non-root Shiny Server Configuration
run_as shiny;

server {
  listen 3838;

  # Serve static dashboard at root
  location / {
    site_dir /home/shiny/app;
    directory_index on;
    log_dir /home/shiny/logs;
  }

  # Serve each Shiny app from its subdirectory
  location /test-app {
    app_dir /home/shiny/app/apps/test-app;
    log_dir /home/shiny/logs;
  }

  location /mosquito-monitoring {
    app_dir /home/shiny/app/apps/mosquito-monitoring;
    log_dir /home/shiny/logs;
  }

  location /suco_history {
    app_dir /home/shiny/app/apps/suco_history;
    log_dir /home/shiny/logs;
  }

  location /drone_trt_progress {
    app_dir /home/shiny/app/apps/drone_trt_progress;
    log_dir /home/shiny/logs;
  }

  location /drone_trt_history {
    app_dir /home/shiny/app/apps/drone_trt_history;
    log_dir /home/shiny/logs;
  }

  location /struct_trt_progress {
    app_dir /home/shiny/app/apps/struct_trt_progress;
    log_dir /home/shiny/logs;
  }

  location /cattail_planned_trt {
    app_dir /home/shiny/app/apps/cattail_planned_trt;
    log_dir /home/shiny/logs;
  }

  location /cattail_inspection_progress {
    app_dir /home/shiny/app/apps/cattail_inspection_progress;
    log_dir /home/shiny/logs;
  }

  location /treatment-analysis {
    app_dir /home/shiny/app/apps/treatment-analysis;
    log_dir /home/shiny/logs;
  }
}
EOF

# Create environment configuration file
cat > $DEPLOY_DIR/.env.example << 'EOF'
# Configuration for MMCD Dashboard deployment
# Copy this to .env and modify as needed

# Port configuration
HOST_PORT=3838

# User/Group IDs for non-root deployment
USER_ID=1000
GROUP_ID=1000

# For custom port deployment, change HOST_PORT:
# HOST_PORT=8080
# HOST_PORT=9999
EOF

# Create logs directory for Docker volume
mkdir -p $DEPLOY_DIR/logs

# Create .dockerignore
cat > $DEPLOY_DIR/.dockerignore << 'EOF'
logs/
*.log
.git/
*.md
create-deployment-package.sh
mmcd-dashboard-deploy/
*.tar.gz
EOF

# Create deployment instructions
cat > $DEPLOY_DIR/DEPLOYMENT.md << 'EOF'
# MMCD Dashboard Deployment Instructions

## Deployment Options

### Option 1: Standard Docker Deployment (Default)
```bash
# Extract package
tar -xzf mmcd-dashboard-deploy.tar.gz
cd mmcd-dashboard-deploy

# Deploy with default settings (port 3838)
docker-compose up -d
```

### Option 2: Custom Port Deployment
```bash
# Set custom port using environment variable
echo "HOST_PORT=8080" > .env
docker-compose up -d

# Or modify docker-compose.yml directly and change:
# - "${HOST_PORT:-3838}:3838" to - "YOUR_PORT:3838"
```

### Option 3: Non-Root User Deployment (Recommended for Shared Systems)
```bash
# Use the non-root configuration
docker-compose -f docker-compose-nonroot.yml up -d

# Or with custom port:
echo "HOST_PORT=8080" > .env
docker-compose -f docker-compose-nonroot.yml up -d
```

### Option 4: Manual Installation (No Docker/Root Required)

If Docker is not available or not preferred:

#### Prerequisites
- R 4.0+ installed
- User has permission to install R packages in their home directory

#### Installation Steps
```bash
# 1. Extract package
tar -xzf mmcd-dashboard-deploy.tar.gz
cd mmcd-dashboard-deploy

# 2. Install R packages (user-level, no sudo required)
R -e "install.packages(c('shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgres', 'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', 'plotrix', 'dtplyr', 'vroom', 'tidyverse', 'classInt', 's2', 'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster', 'plotly', 'purrr', 'tibble'), repos='https://cran.rstudio.com/')"

# 3. Run on custom port (e.g., 8080)
R -e "shiny::runApp('.', host='0.0.0.0', port=8080)"

# 4. Or run individual apps:
cd apps/mosquito-monitoring
R -e "shiny::runApp('.', host='0.0.0.0', port=8080)"
```

## Port Configuration Examples

### Common Port Options
- **3838**: Standard Shiny Server port
- **8080**: Common alternative web port
- **9999**: High-numbered port (often unrestricted)
- **8888**: Jupyter-style port
- **3000**: Node.js-style port

### Environment Variable Configuration
Create a `.env` file in the deployment directory:
```bash
# Custom port
HOST_PORT=8080

# Custom user/group (for non-root deployment)
USER_ID=1000
GROUP_ID=1000
```

## Reverse Proxy Setup

### Apache (any port)
```apache
# For port 8080 deployment
ProxyPass /mmcd-dashboard/ http://localhost:8080/
ProxyPassReverse /mmcd-dashboard/ http://localhost:8080/
ProxyPreserveHost On
```

### Nginx (any port)
```nginx
# For port 8080 deployment
location /mmcd-dashboard/ {
    proxy_pass http://localhost:8080/;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;
}
```

### Subdomain Setup
```nginx
# For subdomain setup (mmcd.your-domain.com)
server {
    listen 80;
    server_name mmcd.your-domain.com;
    
    location / {
        proxy_pass http://localhost:8080/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
    }
}
```

## Security Considerations

### Non-Root Deployment Benefits
- Container runs as user ID 1000 (not root)
- Limited file system access
- Reduced attack surface
- Suitable for shared hosting environments

### Network Security
- Dashboard only needs outbound access to rds-readonly.mmcd.org:5432
- No inbound database connections required
- Read-only database credentials
- No file upload capabilities

### Firewall Configuration
```bash
# Only allow outbound database connections
sudo ufw allow out 5432/tcp

# Allow inbound on your chosen port
sudo ufw allow 8080/tcp  # or your chosen port
```

## Database Configuration

The applications connect to:
- **Host**: rds-readonly.mmcd.org
- **Database**: mmcd_data
- **User**: mmcd_read
- **Password**: mmcd2012
- **Port**: 5432

**Network Requirements**: The host system must be able to reach `rds-readonly.mmcd.org:5432`

## Troubleshooting

### Docker Issues
```bash
# Check logs
docker-compose logs mmcd-dashboard

# Restart
docker-compose restart

# Rebuild (if needed)
docker-compose build --no-cache

# Check which ports are in use
docker-compose ps
```

### Port Conflicts
```bash
# Check if port is already in use
netstat -tlnp | grep :8080
# or
ss -tlnp | grep :8080

# Find available ports
for port in {8080..8090}; do
    nc -z localhost $port || echo "Port $port is available"
done
```

### Database Connectivity
```bash
# Test database connection
telnet rds-readonly.mmcd.org 5432

# Check if blocked by firewall
curl -v telnet://rds-readonly.mmcd.org:5432
```

### Manual Installation Issues
```bash
# Check R package installation
R -e "library(shiny); library(DBI); library(RPostgres)"

# Run with verbose output
R -e "options(shiny.trace=TRUE); shiny::runApp('.', port=8080)"
```

## Advanced Configuration

### Running Multiple Instances
```bash
# Run on different ports for different environments
# Development
echo "HOST_PORT=8080" > .env.dev
docker-compose --env-file .env.dev up -d

# Staging  
echo "HOST_PORT=8081" > .env.staging
docker-compose --env-file .env.staging -p mmcd-staging up -d
```

### Custom Resource Limits
Add to docker-compose.yml:
```yaml
services:
  mmcd-dashboard:
    # ... existing config ...
    deploy:
      resources:
        limits:
          memory: 2G
          cpus: '1.0'
```

## Support
- **Technical Issues**: Contact MMCD IT team
- **Database Access**: Contact Alex directly  
- **Application Bugs**: Submit GitHub issue

## Quick Reference Commands

```bash
# Standard deployment (port 3838)
docker-compose up -d

# Custom port deployment (port 8080)
echo "HOST_PORT=8080" > .env && docker-compose up -d

# Non-root deployment (port 8080, safer for shared systems)
echo "HOST_PORT=8080" > .env && docker-compose -f docker-compose-nonroot.yml up -d

# Manual R deployment (no Docker)
R -e "shiny::runApp('.', host='0.0.0.0', port=8080)"

# Check status
docker-compose ps

# View logs
docker-compose logs -f

# Stop
docker-compose down
```
EOF

# Create archive
echo "Creating deployment archive..."
tar -czf mmcd-dashboard-deploy.tar.gz $DEPLOY_DIR/

echo ""
echo "Deployment package created: mmcd-dashboard-deploy.tar.gz"
echo ""
echo " Package contents:"
echo "   - All Shiny applications (9 apps)"
echo "   - Main dashboard (index.html)"
echo "   - Standard Docker configuration"
echo "   - Non-root Docker configuration (safer for shared systems)"
echo "   - Manual installation option (no Docker required)"
echo "   - Flexible port configuration"
echo "   - Complete deployment instructions"
echo ""
echo " Deployment options:"
echo "   OPTION 1 - Standard Docker (any port):"
echo "     tar -xzf mmcd-dashboard-deploy.tar.gz"
echo "     cd mmcd-dashboard-deploy"
echo "     echo 'HOST_PORT=8080' > .env  # or any port they prefer"
echo "     docker-compose up -d"
echo ""
echo "   OPTION 2 - Non-root Docker (recommended for shared systems):"
echo "     tar -xzf mmcd-dashboard-deploy.tar.gz"
echo "     cd mmcd-dashboard-deploy"
echo "     echo 'HOST_PORT=8080' > .env"
echo "     docker-compose -f docker-compose-nonroot.yml up -d"
echo ""
echo "   OPTION 3 - Manual installation (no Docker/root required):"
echo "     tar -xzf mmcd-dashboard-deploy.tar.gz"
echo "     cd mmcd-dashboard-deploy"
echo "     R -e \"install.packages(c('shiny', 'DBI', 'RPostgres', ...))\""
echo "     R -e \"shiny::runApp('.', host='0.0.0.0', port=8080)\""
echo ""
echo " They can use ANY port they want (3838, 8080, 9999, etc.)"
echo " Non-root option runs without root privileges"
echo " All options include reverse proxy configuration examples"
echo ""
echo "Applications included:"
ls -1 apps/ | sed 's/^/   - /'

# Cleanup
rm -rf $DEPLOY_DIR

echo ""
echo "✨ Ready to send to hostdash team!"