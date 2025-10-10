# Load required libraries
suppressPackageStartupMessages({
library(shiny)
library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(leaflet) 
library(sf) 
library(stringr) 
})

# Define UI for the application
ui <- fluidPage(
# Application title
titlePanel("SUCO (Surveillance Count) Analysis Dashboard"),

# Sidebar with controls
sidebarLayout(
sidebarPanel(
# Time period selection
radioButtons("time_interval", "Time Interval:",
choices = c("Weekly" = "week",
"Monthly" = "month"),
selected = "week"),

# Group by selection
radioButtons("group_by", "Group By:",
choices = c("Facility" = "facility",
"Foreman" = "foreman"),
selected = "facility"),

# Date range selection - default to current year
dateRangeInput("date_range", "Select Date Range:",
start = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
end = Sys.Date(),
format = "yyyy-mm-dd"),

# Filter by facility
selectInput("facility_filter", "Filter by Facility:",
choices = c("All", "E", "MO", "N", "Sj", "Sr", "W2", "Wm", "Wp"),
selected = "All"),

# Filter by foreman (populated dynamically)
selectInput("foreman_filter", "Filter by Foreman:",
choices = c("All"),
selected = "All"),

# Map Controls
conditionalPanel(
condition = "input.tabset == 'Map'",
hr(),
h4("Map Options"),

# Base map selection
selectInput("basemap", "Base Map:",
choices = c("Carto Light" = "carto",
"OpenStreetMap" = "osm",
"Stamen Terrain" = "terrain",
"Esri Satellite" = "satellite"),
selected = "carto"),

# Simple toggle for marker size
radioButtons("marker_size", "Marker Size:",
choices = c("Large" = "large",
"Extra Large" = "xlarge"),
selected = "xlarge", inline = TRUE)
)
),

# Main panel for displaying the graphs
mainPanel(
tabsetPanel(id = "tabset",
tabPanel("Graph", value = "Graph", plotOutput("trend_plot", height = "500px")),
tabPanel("Map", value = "Map", leafletOutput("map", height = "600px")),
tabPanel("Summary Table", value = "Table", dataTableOutput("summary_table")),
tabPanel("Top Locations", value = "TopLoc", plotOutput("location_plot", height = "500px"))
)
)
)
)

# Define server logic
server <- function(input, output, session) {

# Function to convert well-known binary (WKB) to sf object
wkb_to_sf <- function(wkb) {
if (is.na(wkb) || is.null(wkb) || wkb == "") {
return(NULL)
}

tryCatch({
# First, try to parse as hex WKB
point <- sf::st_as_sfc(wkb, hex = TRUE, crs = 4326)
return(point)
}, error = function(e) {
tryCatch({
# If that fails, try parsing as WKT
if (substr(wkb, 1, 5) == "POINT" ||
substr(wkb, 1, 10) == "MULTIPOINT" ||
substr(wkb, 1, 10) == "LINESTRING" ||
substr(wkb, 1, 7) == "POLYGON") {
point <- sf::st_as_sfc(wkb, crs = 4326)
return(point)
}
return(NULL)
}, error = function(e) {
return(NULL)
})
})
}

# Fetch SUCO data from both current and archive tables
suco_data <- reactive({
con <- dbConnect(
RPostgres::Postgres(),
dbname = "mmcd_data",
host = "rds-readonly.mmcd.org",
port = 5432,
user = "mmcd_read",
password = "mmcd2012"
)

# Date range for query
start_date <- format(input$date_range[1], "%Y-%m-%d")
end_date <- format(input$date_range[2], "%Y-%m-%d")

# Query current data for SUCOs (survtype = 7) with geometry
current_query <- sprintf("
SELECT
id, ainspecnum, facility, foreman, inspdate, sitecode,
address1, park_name, survtype, fieldcount, comments,
x, y, ST_AsText(geometry) as geometry_text
FROM public.dbadult_insp_current
WHERE survtype = '7'
AND inspdate BETWEEN '%s' AND '%s'
", start_date, end_date)

# Query archive data for SUCOs
archive_query <- sprintf("
SELECT
id, ainspecnum, facility, foreman, inspdate, sitecode,
address1, park_name, survtype, fieldcount, comments,
x, y, ST_AsText(geometry) as geometry_text
FROM public.dbadult_insp_archive
WHERE survtype = '7'
AND inspdate BETWEEN '%s' AND '%s'
", start_date, end_date)

# Execute queries
current_data <- dbGetQuery(con, current_query)
archive_data <- dbGetQuery(con, archive_query)

# Close connection
dbDisconnect(con)

# Combine data and process dates
all_data <- bind_rows(
mutate(current_data, source = "Current"),
mutate(archive_data, source = "Archive")
) %>%
mutate(
inspdate = as.Date(inspdate),
year = year(inspdate),
month = month(inspdate),
week_start = floor_date(inspdate, "week", week_start = 1), # Week starting on Monday
month_label = format(inspdate, "%b %Y"),
location = ifelse(!is.na(park_name) & park_name != "", park_name,
ifelse(!is.na(address1) & address1 != "", address1, sitecode))
)

return(all_data)
})

# Process spatial data for mapping
spatial_data <- reactive({
data <- filtered_data()

# Create sf object for mapping
sf_data <- data %>%
# Use x and y coordinates if available, otherwise try to parse geometry
mutate(
longitude = as.numeric(x),
latitude = as.numeric(y),
has_coords = !is.na(longitude) & !is.na(latitude) &
longitude > -180 & longitude < 180 &
latitude > -90 & latitude < 90
) %>%
filter(has_coords) %>%
# Create sf object
st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

return(sf_data)
})

# Update foreman filter choices based on available data
observe({
data <- suco_data()

# Filter by facility if selected
if (input$facility_filter != "All") {
data <- data %>% filter(facility == input$facility_filter)
}

# Get unique foremen
foremen <- sort(unique(data$foreman))
foremen_choices <- c("All", foremen)

# Update select input
updateSelectInput(session, "foreman_filter", choices = foremen_choices)
})

# Filter data based on user selections
filtered_data <- reactive({
data <- suco_data()

# Filter by facility if selected
if (input$facility_filter != "All") {
data <- data %>% filter(facility == input$facility_filter)
}

# Filter by foreman if selected
if (input$foreman_filter != "All") {
data <- data %>% filter(foreman == input$foreman_filter)
}

return(data)
})

# Aggregate data by selected time interval and grouping
aggregated_data <- reactive({
data <- filtered_data()

# Define the time interval column based on user selection
if (input$time_interval == "week") {
# For weekly, use the Monday start date of each week
data <- data %>%
mutate(time_group = week_start)
} else {
# For monthly, use the month and year
data <- data %>%
mutate(time_group = floor_date(inspdate, "month"))
}

# Define the grouping column
group_col <- input$group_by

# Group and summarize data
result <- data %>%
group_by(time_group, !!sym(group_col)) %>%
summarize(
count = n(),
total_fieldcount = sum(fieldcount, na.rm = TRUE),
.groups = "drop"
) %>%
arrange(time_group)

return(result)
})

# Generate trend plot
output$trend_plot <- renderPlot({
data <- aggregated_data()

# Handle case when no data is available
if (nrow(data) == 0) {
return(
ggplot() +
annotate("text", x = 0.5, y = 0.5,
label = "No SUCO data available with the selected filters", size = 6) +
theme_void()
)
}

# Format time_group based on selected interval
if (input$time_interval == "week") {
# Format as MM/DD for weekly
data$time_label <- format(data$time_group, "%m/%d")
} else {
# Format as Month Year for monthly
data$time_label <- format(data$time_group, "%b %Y")
}

# Determine group column for coloring
group_col <- input$group_by

# Create title based on selections
title_interval <- ifelse(input$time_interval == "week", "Weekly", "Monthly")
title_group <- ifelse(input$group_by == "facility", "Facility", "Foreman")
facility_text <- ifelse(input$facility_filter == "All", "All Facilities", paste("Facility:", input$facility_filter))
foreman_text <- ifelse(input$foreman_filter == "All", "All Foremen", paste("Foreman:", input$foreman_filter))

# Create plot
p <- ggplot(data, aes(x = time_group, y = count, fill = !!sym(group_col))) +
geom_bar(stat = "identity", position = "dodge") +
labs(
title = paste(title_interval, "SUCO Counts by", title_group),
subtitle = paste(facility_text, "-", foreman_text),
x = ifelse(input$time_interval == "week", "Week Starting", "Month"),
y = "Number of SUCOs",
fill = title_group
) +
theme_minimal() +
theme(
plot.title = element_text(face = "bold", size = 16),
axis.title = element_text(face = "bold"),
axis.text.x = element_text(angle = 45, hjust = 1),
legend.position = "bottom",
legend.title = element_text(face = "bold")
)

# Set x-axis date format based on time interval
if (input$time_interval == "week") {
# For weekly data, format as MM/DD
p <- p + scale_x_date(
date_breaks = "2 weeks",
date_labels = "%m/%d",
limits = c(min(data$time_group), max(data$time_group))
)
} else {
# For monthly data, format as Mon YYYY
p <- p + scale_x_date(
date_breaks = "1 month",
date_labels = "%b %Y",
limits = c(min(data$time_group), max(data$time_group))
)
}

return(p)
})

# Generate map
output$map <- renderLeaflet({
# Get spatial data
data <- spatial_data()

# Determine marker size based on selection (now much larger by default)
marker_size <- if(input$marker_size == "large") 12 else 18 # Base size

# Set up basemap provider
basemap <- switch(input$basemap,
"osm" = providers$OpenStreetMap,
"carto" = providers$CartoDB.Positron,
"terrain" = providers$Stamen.Terrain,
"satellite" = providers$Esri.WorldImagery,
providers$CartoDB.Positron)

# Handle case when no data is available
if (nrow(data) == 0) {
# Return empty map with message
return(
leaflet() %>%
addProviderTiles(basemap) %>%
setView(lng = -93.2, lat = 45.0, zoom = 9) %>%
addControl(html = "<div style='background-color: white; padding: 10px;'><h4>No SUCO locations available with the selected filters</h4></div>",
position = "topleft")
)
}

# Create color palette based on field count or facility
if (input$group_by == "facility") {
# Create a color palette for facilities
facilities <- unique(data$facility)
pal <- colorFactor(palette = "Set1", domain = facilities)

# Create map with facility coloring
leaflet(data) %>%
# Add base map
addProviderTiles(basemap) %>%
# Fit map to data bounds
fitBounds(
lng1 = min(st_coordinates(data)[,1]),
lat1 = min(st_coordinates(data)[,2]),
lng2 = max(st_coordinates(data)[,1]),
lat2 = max(st_coordinates(data)[,2])
) %>%
# Add points with MUCH larger size and bold black border
addCircleMarkers(
radius = marker_size, # Fixed large size
color = "black", # Black border
weight = 2.0, # Thicker border
fillColor = ~pal(facility),
fillOpacity = 0.9,
popup = ~paste0("<b>Date:</b> ", inspdate, "<br>",
"<b>Facility:</b> ", facility, "<br>",
"<b>Foreman:</b> ", foreman, "<br>",
"<b>Location:</b> ", location, "<br>",
"<b>Field Count:</b> ", fieldcount)
) %>%
# Add legend
addLegend(
position = "bottomright",
title = "Facility",
pal = pal,
values = ~facility,
opacity = 0.9
)
} else {
# Create a color palette for foremen
foremen <- unique(data$foreman)
pal <- colorFactor(palette = "Set1", domain = foremen)

# Create map with foreman coloring
leaflet(data) %>%
# Add base map
addProviderTiles(basemap) %>%
# Fit map to data bounds
fitBounds(
lng1 = min(st_coordinates(data)[,1]),
lat1 = min(st_coordinates(data)[,2]),
lng2 = max(st_coordinates(data)[,1]),
lat2 = max(st_coordinates(data)[,2])
) %>%
# Add points with MUCH larger size and bold black border
addCircleMarkers(
radius = marker_size, # Fixed large size
color = "black", # Black border
weight = 2.0, # Thicker border
fillColor = ~pal(foreman),
fillOpacity = 0.9,
popup = ~paste0("<b>Date:</b> ", inspdate, "<br>",
"<b>Facility:</b> ", facility, "<br>",
"<b>Foreman:</b> ", foreman, "<br>",
"<b>Location:</b> ", location, "<br>",
"<b>Field Count:</b> ", fieldcount)
) %>%
# Add legend
addLegend(
position = "bottomright",
title = "Foreman",
pal = pal,
values = ~foreman,
opacity = 0.9
)
}
})

# Generate location plot (top locations with most SUCOs)
output$location_plot <- renderPlot({
data <- filtered_data()

# Handle case when no data is available
if (nrow(data) == 0) {
return(
ggplot() +
annotate("text", x = 0.5, y = 0.5,
label = "No SUCO data available with the selected filters", size = 6) +
theme_void()
)
}

# Identify top locations
top_locations <- data %>%
group_by(location) %>%
summarize(
count = n(),
avg_fieldcount = mean(fieldcount, na.rm = TRUE),
.groups = "drop"
) %>%
arrange(desc(count)) %>%
head(15) # Top 15 locations

# Create title based on selections
facility_text <- ifelse(input$facility_filter == "All", "All Facilities", paste("Facility:", input$facility_filter))
foreman_text <- ifelse(input$foreman_filter == "All", "All Foremen", paste("Foreman:", input$foreman_filter))

# Create plot
ggplot(top_locations, aes(x = reorder(location, count), y = count)) +
geom_bar(stat = "identity", fill = "steelblue") +
geom_text(aes(label = count), hjust = -0.2) +
coord_flip() + # Horizontal bars for better label readability
labs(
title = "Top SUCO Locations",
subtitle = paste(facility_text, "-", foreman_text),
x = "Location",
y = "Number of SUCOs"
) +
theme_minimal() +
theme(
plot.title = element_text(face = "bold", size = 16),
axis.title = element_text(face = "bold")
)
})

# Generate summary table
output$summary_table <- renderDataTable({
# Get filtered data
data <- filtered_data()

# Group by selected factor and calculate summary stats
group_col <- input$group_by

summary_data <- data %>%
group_by(across(all_of(group_col))) %>%
summarize(
Total_SUCOs = n(),
Total_Locations = n_distinct(sitecode),
Avg_Field_Count = round(mean(fieldcount, na.rm = TRUE), 1),
First_SUCO = min(inspdate),
Last_SUCO = max(inspdate)
) %>%
arrange(desc(Total_SUCOs))

# Rename column for display
if (group_col == "facility") {
colnames(summary_data)[1] <- "Facility"
} else {
colnames(summary_data)[1] <- "Foreman"
}

return(summary_data)
}, options = list(pageLength = 15, searching = TRUE))
}

# Run the application
shinyApp(ui = ui, server = server)
