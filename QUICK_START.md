# Quick Start: Adding New Applications

## 🚀 5-Minute Setup for New Apps

### 1. Create Directory
```bash
mkdir -p apps/your-app-name
```

### 2. Copy Template
```bash
cat > apps/your-app-name/app.R << 'EOF'
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
})

ui <- fluidPage(
  titlePanel("Your App Title"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dates", "Date Range:", 
                     start = Sys.Date() - 365, end = Sys.Date()),
      actionButton("refresh", "Refresh")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    input$refresh
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "mmcd_data",
                     host = "rds-readonly.mmcd.org",
                     port = 5432,
                     user = "mmcd_read", 
                     password = "mmcd2012")
    
    # Your query here
    result <- dbGetQuery(con, "SELECT * FROM your_table LIMIT 100")
    dbDisconnect(con)
    result
  })
  
  output$plot <- renderPlot({
    ggplot(data(), aes(x = date, y = value)) + 
      geom_line() + 
      theme_minimal()
  })
}

shinyApp(ui, server)
EOF
```

### 3. Test Locally
```bash
R -e "shiny::runApp('apps/your-app-name')"
```

### 4. Add to Landing Page
Edit `apps/index.html` and add:
```html
<div class="app-card">
    <h3>📊 Your App</h3>
    <p>App description here.</p>
    <a href="/your-app-name/" class="app-link">Open Application</a>
</div>
```

### 5. Deploy
```bash
sudo cp -r apps/your-app-name /srv/shiny-server/
sudo systemctl restart shiny-server
```

## 📊 Common Database Queries

### Mosquito Data
```sql
SELECT * FROM dbadult_mon_nt_co2_tall2_forr 
WHERE inspdate >= '2025-01-01'
```

### SUCO Data  
```sql
SELECT * FROM dbadult_insp_current 
WHERE survtype = '7' AND inspdate BETWEEN '2025-01-01' AND '2025-12-31'
```

### Treatment Data
```sql
SELECT * FROM dblarv_insptrt_current
WHERE inspdate >= '2025-01-01'
```

## 🎨 UI Components

### Date Inputs
```r
dateRangeInput("dates", "Select Dates:", start = Sys.Date() - 30, end = Sys.Date())
```

### Dropdowns
```r
selectInput("facility", "Facility:", choices = c("All", "E", "MO", "N"))
```

### Tabs
```r
tabsetPanel(
  tabPanel("Plot", plotOutput("plot")),
  tabPanel("Data", dataTableOutput("table"))
)
```

## 🔧 Quick Fixes

### App Won't Load
```bash
sudo tail -f /var/log/shiny-server/*.log
```

### Database Connection Issues
```r
# Test connection
con <- dbConnect(RPostgres::Postgres(), 
                 dbname = "mmcd_data",
                 host = "rds-readonly.mmcd.org", 
                 port = 5432,
                 user = "mmcd_read", 
                 password = "mmcd2012")
dbListTables(con)
```

### Missing Packages
```r
install.packages(c("shiny", "DBI", "RPostgres", "dplyr", "ggplot2"))
```
