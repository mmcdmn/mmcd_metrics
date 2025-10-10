# MMCD Metrics Dashboard

![MMCD Analytics](https://img.shields.io/badge/MMCD-Analytics-blue)
![R Shiny](https://img.shields.io/badge/R-Shiny-276DC3)
![Status](https://img.shields.io/badge/Status-Active-green)

A comprehensive analytics platform for the Metropolitan Mosquito Control District, providing interactive dashboards for mosquito surveillance, treatment analysis, and operational metrics.

## Architecture

This platform hosts multiple R Shiny applications in an organized, scalable structure:

```
apps/
├── index.html                    # Main landing page
├── mosquito-monitoring/          # CO2 trap surveillance data
│   └── app.R
├── suco-analysis/               # SUCO surveillance analysis  
│   └── app.R
└── treatment-analysis/          # Drone Treatment tracking & analysis
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

### SUCO Analysis  
- **Path**: `/suco-analysis/`
- **Purpose**: Surveillance Count (SUCO) analysis dashboard
- **Features**:
  - Interactive maps with leaflet
  - Facility and foreman filtering
  - Temporal trend analysis (weekly/monthly)
  - Top locations identification
  - Spatial data visualization

### Treatment Analysis
- **Path**: `/treatment-analysis/`
- **Purpose**: Active treatment proportion tracking
- **Features**:
  - Customizable treatment duration
  - Proportion of structures under treatment
  - Interactive date range selection
  - Comprehensive data tables
  - Real-time calculations

## Installation & Deployment

### Prerequisites
```bash
# System dependencies
sudo apt update
sudo apt install -y r-base r-base-dev libcurl4-openssl-dev libssl-dev \
    libxml2-dev libpq-dev libgdal-dev libudunits2-dev libproj-dev \
    gdebi-core nginx
```

### Quick Deployment
```bash
# Clone the repository
git clone https://github.com/ablepacifist/mmcd_metrics_1.git
cd mmcd_metrics_1

# Run automated deployment
chmod +x deploy.sh
./deploy.sh
```

### Manual Setup
```bash
# Install R packages
R -e "install.packages(c('shiny', 'DBI', 'RPostgres', 'dplyr', 'ggplot2', 
                         'lubridate', 'scales', 'leaflet', 'sf', 'stringr', 
                         'DT'), repos='https://cran.rstudio.com/')"

# Install Shiny Server
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb
sudo gdebi -n shiny-server-1.5.22.1017-amd64.deb

# Copy applications
sudo cp -r apps/* /srv/shiny-server/
sudo chown -R shiny:shiny /srv/shiny-server
```

## Adding New Applications

### Step 1: Create Application Directory
```bash
mkdir -p apps/your-new-app
```

### Step 2: Create app.R File
Create `apps/your-new-app/app.R` with this template:

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
Add your new application to `apps/index.html`:

```html
<div class="app-card">
    <h3>Your New App</h3>
    <p>Description of what your new application does.</p>
    <a href="/your-new-app/" class="app-link">Open Application</a>
</div>
```

### Step 4: Update Nginx Configuration
Add to `shiny-server.conf`:

```
location /your-new-app {
  app_dir /home/alexpdyak32/Documents/mmcd/apps/your-new-app;
  log_dir /var/log/shiny-server;
}
```

### Step 5: Deploy
```bash
# Copy to Shiny Server
sudo cp -r apps/your-new-app /srv/shiny-server/
sudo chown -R shiny:shiny /srv/shiny-server

# Restart services
sudo systemctl restart shiny-server
sudo systemctl restart nginx
```

## Database Schema

### Available Tables
- `dbadult_insp_current` - Current adult inspection data
- `dbadult_insp_archive` - Archived adult inspection data  
- `dblarv_insptrt_current` - Current larvicide treatment data
- `loc_cxstruct` - Structure location data
- `dbadult_mon_nt_co2_tall2_forr` - CO2 trap monitoring data

### Common Query Patterns

#### Date-based filtering:
```sql
SELECT * FROM table_name 
WHERE inspdate BETWEEN 'YYYY-MM-DD' AND 'YYYY-MM-DD'
```

#### Facility filtering:
```sql
SELECT * FROM table_name 
WHERE facility IN ('E', 'MO', 'N')
```

#### SUCO data (survtype = 7):
```sql
SELECT * FROM dbadult_insp_current 
WHERE survtype = '7'
```

## Common R Patterns

### Database Connection
```r
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "mmcd_data",
  host = "rds-readonly.mmcd.org",
  port = 5432,
  user = "mmcd_read",
  password = "mmcd2012"
)
```

### Date Processing
```r
data %>%
  mutate(
    inspdate = as.Date(inspdate),
    year = year(inspdate),
    month = month(inspdate),
    week_start = floor_date(inspdate, "week")
  )
```

### Interactive Plots
```r
output$plot <- renderPlot({
  ggplot(data, aes(x = date, y = value)) +
    geom_line(color = "steelblue") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
})
```

## URLs After Deployment

- **Main Dashboard**: `http://your-server/`
- **Applications**: `http://your-server/app-name/`
- **Direct Shiny Access**: `http://your-server:3838/app-name/`

## Troubleshooting

### Check Shiny Server Status
```bash
sudo systemctl status shiny-server
sudo journalctl -u shiny-server -f
```

### Check Application Logs
```bash
sudo tail -f /var/log/shiny-server/*.log
```

### Restart Services
```bash
sudo systemctl restart shiny-server
sudo systemctl restart nginx
```

### Test R Packages
```bash
R -e "library(shiny); library(DBI); library(RPostgres)"
```

## Development Workflow

1. **Create new app locally**
2. **Test with**: `R -e "shiny::runApp('apps/your-app')"`
3. **Add to index.html**
4. **Update configuration files**
5. **Commit to Git**
6. **Deploy to server**

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

For technical support or questions about adding new applications, contact the MMCD IT team.

---
*Last updated: October 2025*
