# MMCD Metrics Dashboard

![MMCD Analytics](https://img.shields.io/badge/MMCD-Analytics-blue)
![R Shiny](https://img.shields.io/badge/R-Shiny-276DC3)
![Status](https://img.shields.io/badge/Status-Active-green)

A comprehensive analytics platform for the Metropolitan Mosquito Control District, providing interactive dashboards for mosquito surveillance, treatment analysis, and operational metrics.

## Table of Contents

- [Architecture](#architecture)
- [Applications](#applications)
  - [Mosquito Monitoring](#mosquito-monitoring)
  - [SUCO History](#suco-history)
  - [Drone Treatment Progress](#drone-treatment-progress)
  - [Structural Treatment Progress](#structural-treatment-progress)
  - [Cattail Planned Treatments](#cattail-planned-treatments)
  - [Drone Treatment History](#drone-treatment-history)
- [Installation & Deployment](#installation--deployment)
  - [Prerequisites - System Dependencies](#prerequisites---system-dependencies)
  - [R Package Installation](#r-package-installation)
  - [Shiny Server Setup](#shiny-server-setup)
  - [Quick Local Testing](#quick-local-testing)
  - [Production Deployment](#production-deployment)
- [Application URLs](#application-urls)
- [Adding New Applications](#adding-new-applications)
- [Troubleshooting](#troubleshooting)
- [Development Workflow](#development-workflow)
- [Contributing](#contributing)

## Architecture

This platform hosts multiple R Shiny applications in an organized, scalable structure:

```
apps/
├── index.html                    # Main landing page
├── mosquito-monitoring/          # CO2 trap surveillance data
│   └── app.R
├── suco_history/                 # SUCO surveillance analysis  
│   └── app.R
├── drone_trt_progress/           # Drone treatment progress tracking
│   └── app.R
├── drone_trt_history/            # Drone treatment historical analysis
│   └── app.R
├── struct_trt_progress/          # Structural treatment progress
│   └── app.R
└── cattail_planned_trt/          # Cattail treatment planning
    └── app.R
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
- **Features**:
  - Interactive maps with leaflet
  - Facility and foreman filtering
  - Temporal trend analysis (weekly/monthly)
  - Top locations identification
  - Spatial data visualization

### Drone Treatment Progress
- **Path**: `/drone_trt_progress/`
- **Purpose**: Real-time tracking of drone-based treatment operations
- **Features**:
  - Active treatment area monitoring
  - Progress metrics and coverage analysis
  - Aerial mosquito control operations tracking
  - Interactive date range selection

### Structural Treatment Progress
- **Path**: `/struct_trt_progress/`
- **Purpose**: Monitor structural treatment activities
- **Features**:
  - Proportion of structures under treatment tracking
  - Customizable treatment duration settings
  - Comprehensive trend visualization
  - Real-time calculations

### Cattail Planned Treatments
- **Path**: `/cattail_planned_trt/`
- **Purpose**: Planning and tracking dashboard for cattail marsh treatments
- **Features**:
  - Planned treatment area visualization
  - Schedule and resource allocation tracking
  - Targeted cattail habitat management

### Drone Treatment History
- **Path**: `/drone_trt_history/`
- **Purpose**: Historical analysis of drone treatment operations
- **Features**:
  - Trend visualization and effectiveness metrics
  - Comprehensive records of past treatments
  - Historical treatment pattern analysis

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
# Install core Shiny and data manipulation packages (user-level)
R -e "install.packages(c('shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', 'plotrix', 'dtplyr', 'vroom', 'tidyverse'), repos='https://cran.rstudio.com/')"

# Install spatial analysis dependencies (user-level)
R -e "install.packages(c('classInt', 's2'), repos='https://cran.rstudio.com/')"

# Install geospatial and mapping packages (user-level)
R -e "install.packages(c('sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster'), repos='https://cran.rstudio.com/')"

# IMPORTANT: For Shiny Server deployment, also install packages system-wide
# This ensures the shiny user can access all required packages
sudo R -e "install.packages(c('shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', 'plotrix', 'dtplyr', 'vroom', 'tidyverse', 'classInt', 's2', 'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster'), lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"
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

### Quick Local Testing
```bash
# Clone the repository
git clone https://github.com/ablepacifist/mmcd_metrics_1.git
cd mmcd_metrics_1

# Set workspace variable
export MMCD_WORKSPACE=$(pwd)

# Start the main dashboard (in one terminal)
cd apps
python3 -m http.server 8080

# Test individual applications (in another terminal)
cd $MMCD_WORKSPACE/apps/mosquito-monitoring
R -e "shiny::runApp(host='127.0.0.1', port=3838)"
```

Access the applications:
- Main Dashboard: `http://localhost:3838/`
- Individual Applications: `http://localhost:3838/app-name/`

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

# Step 3: Install R packages (run each command and wait for completion)
R -e "install.packages(c('shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', 'plotrix', 'dtplyr', 'vroom', 'tidyverse'), repos='https://cran.rstudio.com/')"
R -e "install.packages(c('classInt', 's2'), repos='https://cran.rstudio.com/')"
R -e "install.packages(c('sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster'), repos='https://cran.rstudio.com/')"

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

## Application URLs

### Local Development
- **Main Dashboard**: `http://localhost:3838/`
- **Mosquito Monitoring**: `http://localhost:3838/mosquito-monitoring/`
- **SUCO History**: `http://localhost:3838/suco_history/`
- **Drone Treatment Progress**: `http://localhost:3838/drone_trt_progress/`
- **Structural Treatment Progress**: `http://localhost:3838/struct_trt_progress/`
- **Cattail Planned Treatments**: `http://localhost:3838/cattail_planned_trt/`
- **Drone Treatment History**: `http://localhost:3838/drone_trt_history/`

### Production Server
Replace `your-server.com` with your actual server address:
- **Main Dashboard**: `http://your-server.com:3838/`
- **Applications**: `http://your-server.com:3838/app-name/`

## Adding New Applications

### Step 1: Create Application Directory
```bash
mkdir -p $MMCD_WORKSPACE/apps/your-new-app
```

### Step 2: Create app.R File
Create `$MMCD_WORKSPACE/apps/your-new-app/app.R` with this template:

```r
# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  # Add other libraries as needed
})

# Define UI
ui <- fluidPage(
  titlePanel("Your Application Title"),
  
  sidebarLayout(
    sidebarPanel(
      # Add your input controls here
      dateRangeInput("date_range", "Select Date Range:",
                     start = Sys.Date() - 365,
                     end = Sys.Date()),
      
      selectInput("facility", "Select Facility:",
                  choices = c("All", "E", "MO", "N", "Sj", "Sr", "W2", "Wm", "Wp"),
                  selected = "All"),
      
      actionButton("refresh", "Refresh Data", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("main_plot")),
        tabPanel("Data", dataTableOutput("data_table"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Database connection function
  get_data <- reactive({
    input$refresh  # Dependency on refresh button
    
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = "mmcd_data",
      host = "rds-readonly.mmcd.org",
      port = 5432,
      user = "mmcd_read",
      password = "mmcd2012"
    )
    
    # Your SQL query here
    query <- "SELECT * FROM your_table WHERE date_column BETWEEN ? AND ?"
    data <- dbGetQuery(con, query)
    
    dbDisconnect(con)
    return(data)
  })
  
  # Generate plots
  output$main_plot <- renderPlot({
    data <- get_data()
    
    # Your ggplot code here
    ggplot(data, aes(x = date, y = value)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Your Chart Title")
  })
  
  # Generate data table
  output$data_table <- renderDataTable({
    get_data()
  }, options = list(pageLength = 15))
}

# Run the application
shinyApp(ui = ui, server = server)
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
# Copy to Shiny Server
sudo cp -r $MMCD_WORKSPACE/apps/your-new-app /srv/shiny-server/
sudo chown -R shiny:shiny /srv/shiny-server/your-new-app

# IMPORTANT: Always copy updated index.html after making changes
sudo cp $MMCD_WORKSPACE/apps/index.html /srv/shiny-server/

# Restart Shiny Server
sudo systemctl restart shiny-server
```

```bash
#if docker is in place
docker build -t mmcd-dashboard .
docker run -p 3838:3838 mmcd-dashboard
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
# Test database connectivity
R -e "library(DBI); library(RPostgreSQL); con <- dbConnect(PostgreSQL(), host='data.mmcd.org', dbname='mmcd_data', user='mmcd_read', password='mmcd2012'); dbListTables(con); dbDisconnect(con)"

# If database connection fails, check network connectivity
ping data.mmcd.org

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
The MMCD applications require access to `data.mmcd.org` on port 5432. If you're experiencing connection timeouts:

```bash
# Test if the database server is reachable
ping data.mmcd.org
telnet data.mmcd.org 5432

# Check firewall rules
sudo ufw status

# For testing purposes, you can modify applications to use sample data instead of live connections
```

## Development Workflow

### Local Development Process
1. **Clone repository**: `git clone https://github.com/ablepacifist/mmcd_metrics_1.git`
2. **Create new app locally**: `mkdir -p $MMCD_WORKSPACE/apps/your-new-app`
3. **Develop application**: Create `app.R` file with Shiny application code
4. **Test locally**: `R -e "shiny::runApp('$MMCD_WORKSPACE/apps/your-new-app', port=3838)"`
5. **Add to dashboard**: Update `$MMCD_WORKSPACE/apps/index.html` with new application link
6. **Test integration**: Verify application works through dashboard
7. **Commit changes**: `git add . && git commit -m "Add new application"`
8. **Deploy to server**: Copy to `/srv/shiny-server/` and restart Shiny Server


## Contributing

1. Clone the repository
2. Create a new branch for your application
3. Follow the application structure guidelines
4. Test thoroughly
5. Update documentation
6. Submit a pull request

## License

This project is maintained by the Metropolitan Mosquito Control District.

## Support

For technical support or questions about adding new applications, contact the MMCD IT team or Alex directly.

---
*Last updated: October 2025*
*Dependencies verified and tested on Ubuntu 24.04 LTS with R 4.5.1*
