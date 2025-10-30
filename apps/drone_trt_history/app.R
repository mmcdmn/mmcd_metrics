#-----------------------------------------
#Alex's note:
# There seems to be some discrepencies between dblarv_insptrt_current and loc_breeding_sites in what is designated as a drone treatment.
# In loc_breeding_sites, site are given a designation of drone with Y,N,M,C where as in dblarv_insptrt_current, sites are treated using action code 'D'. 
#-----------------------------------------
# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
})

# Source shared database helpers
source("../../shared/db_helpers.R")

# Load environment variables from .env file (for local development)
# or from Docker environment variables (for production)
env_paths <- c(
  "../../.env",           # For local development
  "../../../.env",        # Alternative local path
  "/srv/shiny-server/.env" # Docker path
)

# Try to load from .env file first
env_loaded <- FALSE
for (path in env_paths) {
  if (file.exists(path)) {
    readRenviron(path)
    env_loaded <- TRUE
    break
  }
}

# Database connection is now handled by db_helpers.R

# Define UI for the application
ui <- fluidPage(
  # Application title
  titlePanel("Drone Treatments by Year and Facility"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      # Toggle between treatments count and unique sites
      radioButtons("count_type", "Display Metric:",
                   choices = c("Number of Treatments" = "treatments",
                               "Number of Unique Sites" = "sites"),
                   selected = "treatments"),
      
      # Filter by facility
      selectInput("facility_filter", "Select Facility:",
                  choices = c("All", "E", "MO", "N", "Sj", "Sr", "W2", "Wm", "Wp"),
                  selected = "All"),
      
      # Years to include
      sliderInput("year_range", "Select Year Range:",
                  min = 2018, max = 2025, value = c(2018, 2025), step = 1),
      
      # Add radio button for site size metric
      radioButtons("site_size_metric", "Site Size Metric:",
                   choices = c("Average" = "avg", "Smallest" = "min", "Largest" = "max"),
                   selected = "avg"),
      
      helpText(
        "This graph shows drone treatments by facility over time.",
        tags$br(),
        "Note: If a site was partially treated, this program counts the full acres for that site.",
        tags$br(),
        "Data sources:",
        tags$ul(
          tags$li("Archive data: treatments with action = 'D'"),
          tags$li("Current data: treatments with airgrnd_plan = 'D' or action = 'D'")
        ))
    ),
    
    # Main panel for displaying the graph
    mainPanel(
      plotOutput("droneGraph", height = "600px"),
      plotOutput("siteAvgSizeGraph", height = "400px"),
      dataTableOutput("summaryTable"),
      h4("5 Smallest and 5 Largest Drone Sites per Year"),
      dataTableOutput("siteExtremesTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Fetch all raw data
  raw_data <- reactive({
    con <- get_db_connection()
    if (is.null(con)) {
      return(NULL)
    }
    # Get archive data
    archive_query <- sprintf("
SELECT
facility,
sitecode,
inspdate,
action
FROM public.dblarv_insptrt_archive
WHERE action = 'D'
AND EXTRACT(YEAR FROM inspdate) BETWEEN %d AND %d
", input$year_range[1], input$year_range[2])
    archive_data <- dbGetQuery(con, archive_query)
    # Get current data
    current_query <- sprintf("
SELECT
facility,
sitecode,
inspdate,
action,
airgrnd_plan
FROM public.dblarv_insptrt_current
WHERE (airgrnd_plan = 'D' OR action = 'D')
AND EXTRACT(YEAR FROM inspdate) BETWEEN %d AND %d
", input$year_range[1], input$year_range[2])
    current_data <- dbGetQuery(con, current_query)
    # Get site size info from loc_breeding_sites
    sitecodes <- unique(c(archive_data$sitecode, current_data$sitecode))
    if (length(sitecodes) > 0) {
      sitecodes_str <- paste(sprintf("'%s'", sitecodes), collapse = ",")
      lbs_query <- sprintf("
SELECT sitecode, acres, facility FROM public.loc_breeding_sites WHERE sitecode IN (%s)", sitecodes_str)
      lbs_data <- dbGetQuery(con, lbs_query)
    } else {
      lbs_data <- data.frame(sitecode=character(), acres=numeric(), facility=character())
    }
    dbDisconnect(con)
    list(
      archive = archive_data,
      current = current_data,
      lbs = lbs_data
    )
  })
  
  # Process the data based on user selections
  processed_data <- reactive({
    # Get raw data
    data_list <- raw_data()
    
    # Process archive data
    archive_data <- data_list$archive %>%
      mutate(
        source = "Archive",
        year = year(inspdate)
      )
    
    # Process current data
    current_data <- data_list$current %>%
      mutate(
        source = "Current",
        year = year(inspdate)
      )
    
    # Combine the data
    all_data <- bind_rows(archive_data, current_data)
    
    # Filter by facility if specified
    if (input$facility_filter != "All") {
      all_data <- all_data %>% filter(facility == input$facility_filter)
    }
    
    # Process based on count type
    if (input$count_type == "treatments") {
      # Count all treatments
      results <- all_data %>%
        group_by(facility, year) %>%
        summarize(count = n(), .groups = "drop")
    } else {
      # Count unique sites
      results <- all_data %>%
        group_by(facility, year) %>%
        summarize(count = n_distinct(sitecode), .groups = "drop")
    }
    
    # Make sure we have entries for all years in the range
    all_years <- seq(from = input$year_range[1], to = input$year_range[2])
    all_facilities <- unique(all_data$facility)
    
    # Create complete grid
    if (length(all_facilities) > 0) {
      expanded_grid <- expand.grid(
        facility = all_facilities,
        year = all_years
      )
      
      # Join with actual data
      results <- expanded_grid %>%
        left_join(results, by = c("facility", "year")) %>%
        mutate(count = ifelse(is.na(count), 0, count))
    } else {
      # Handle case with no data
      results <- data.frame(facility = character(), year = integer(), count = integer())
    }
    
    return(results)
  })
  
  # Generate the plot
  output$droneGraph <- renderPlot({
    # Get processed data
    data <- processed_data()
    
    # Handle case when no data is available
    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No drone treatment data available with the selected filters", size = 6) +
          theme_void()
      )
    }
    
    # Set y-axis label based on selected count type
    y_label <- ifelse(input$count_type == "treatments",
                      "Number of Treatments",
                      "Number of Unique Sites")
    
    # Set up the plot
    if (input$facility_filter == "All") {
      # Grouped bar plot for all facilities
      p <- ggplot(data, aes(x = year, y = count, fill = facility)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
          title = paste("Drone", ifelse(input$count_type == "treatments", "Treatments", "Sites"), "by Year and Facility"),
          x = "Year",
          y = y_label,
          fill = "Facility"
        ) +
        scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2], 1)) +
        # Ensure y-axis uses integer breaks
        scale_y_continuous(breaks = function(x) seq(0, max(x), by = max(1, round(max(x)/10)))) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          axis.title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.title = element_text(face = "bold")
        )
    } else {
      # Line plot for a single facility
      p <- ggplot(data, aes(x = year, y = count)) +
        geom_line(linewidth = 1.5, color = "steelblue") +
        geom_point(size = 3, color = "steelblue") +
        geom_text(aes(label = count), vjust = -1) +
        labs(
          title = paste("Drone", ifelse(input$count_type == "treatments", "Treatments", "Sites"),
                        "by Year -", input$facility_filter, "Facility"),
          x = "Year",
          y = y_label
        ) +
        scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2], 1)) +
        scale_y_continuous(breaks = function(x) seq(0, max(x), by = max(1, round(max(x)/10)))) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          axis.title = element_text(face = "bold")
        )
    }
    
    return(p)
  })
  
  # Generate the site size graph (avg, smallest, largest)
  output$siteAvgSizeGraph <- renderPlot({
    data_list <- raw_data()
    archive_data <- data_list$archive
    current_data <- data_list$current
    lbs_data <- data_list$lbs
    archive_data <- left_join(archive_data, lbs_data, by = c("sitecode", "facility"))
    current_data <- left_join(current_data, lbs_data, by = c("sitecode", "facility"))
    all_data <- bind_rows(archive_data, current_data)
    if (input$facility_filter != "All") {
      all_data <- all_data %>% filter(facility == input$facility_filter)
    }
    all_data <- all_data %>% filter(!is.na(acres) & acres > 0)
    all_data <- all_data %>% mutate(year = year(inspdate))
    metric <- input$site_size_metric
    if (input$facility_filter == "All") {
      summary <- all_data %>% group_by(year, facility)
      if (metric == "avg") {
        summary <- summary %>% summarize(val = mean(acres, na.rm = TRUE), .groups = "drop")
        ylab <- "Average Site Acres"
        title <- "Average Drone Site Size (Acres) by Year and Facility"
      } else if (metric == "min") {
        summary <- summary %>% summarize(val = min(acres, na.rm = TRUE), .groups = "drop")
        ylab <- "Smallest Site Acres"
        title <- "Smallest Drone Site (Acres) by Year and Facility"
      } else {
        summary <- summary %>% summarize(val = max(acres, na.rm = TRUE), .groups = "drop")
        ylab <- "Largest Site Acres"
        title <- "Largest Drone Site (Acres) by Year and Facility"
      }
      p <- ggplot(summary, aes(x = year, y = val, color = facility)) +
        geom_line(linewidth = 1.5) +
        geom_point(size = 3) +
        geom_text(aes(label = round(val, 2)), vjust = -1) +
        labs(
          title = title,
          x = "Year",
          y = ylab,
          color = "Facility"
        ) +
        scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2], 1)) +
        theme_minimal()
    } else {
      summary <- all_data %>% group_by(year)
      if (metric == "avg") {
        summary <- summary %>% summarize(val = mean(acres, na.rm = TRUE), .groups = "drop")
        ylab <- "Average Site Acres"
        title <- paste("Average Drone Site Size (Acres) by Year -", input$facility_filter, "Facility")
      } else if (metric == "min") {
        summary <- summary %>% summarize(val = min(acres, na.rm = TRUE), .groups = "drop")
        ylab <- "Smallest Site Acres"
        title <- paste("Smallest Drone Site (Acres) by Year -", input$facility_filter, "Facility")
      } else {
        summary <- summary %>% summarize(val = max(acres, na.rm = TRUE), .groups = "drop")
        ylab <- "Largest Site Acres"
        title <- paste("Largest Drone Site (Acres) by Year -", input$facility_filter, "Facility")
      }
      p <- ggplot(summary, aes(x = year, y = val)) +
        geom_line(linewidth = 1.5, color = "darkgreen") +
        geom_point(size = 3, color = "darkgreen") +
        geom_text(aes(label = round(val, 2)), vjust = -1) +
        labs(
          title = title,
          x = "Year",
          y = ylab
        ) +
        scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2], 1)) +
        theme_minimal()
    }
    return(p)
  })

  # Redesigned extremes table: split by facility, 5 smallest and 5 largest columns
  output$siteExtremesTable <- renderDataTable({
    data_list <- raw_data()
    archive_data <- data_list$archive
    current_data <- data_list$current
    lbs_data <- data_list$lbs
    archive_data <- left_join(archive_data, lbs_data, by = c("sitecode", "facility"))
    current_data <- left_join(current_data, lbs_data, by = c("sitecode", "facility"))
    all_data <- bind_rows(archive_data, current_data)
    if (input$facility_filter != "All") {
      all_data <- all_data %>% filter(facility == input$facility_filter)
    }
    all_data <- all_data %>% filter(!is.na(acres) & acres > 0)
    all_data <- all_data %>% mutate(year = year(inspdate))
    # For each year/facility, get 5 smallest and 5 largest
    # For each year/facility, get min/max acres per sitecode, then top 5 distinct sites
    site_extremes <- all_data %>%
      group_by(year, facility, sitecode) %>%
      summarize(
        min_acres = min(acres, na.rm = TRUE),
        max_acres = max(acres, na.rm = TRUE),
        .groups = "drop"
      )
    # 5 smallest distinct sites per year/facility
    smallest <- site_extremes %>%
      group_by(year, facility) %>%
      arrange(min_acres) %>%
      slice_head(n = 5) %>%
      ungroup()
    # 5 largest distinct sites per year/facility
    largest <- site_extremes %>%
      group_by(year, facility) %>%
      arrange(desc(max_acres)) %>%
      slice_head(n = 5) %>%
      ungroup()
    # Combine into wide format
    make_col <- function(df, label, acres_col) {
      df %>% group_by(year, facility) %>%
        summarize(
          !!paste0(label, "_sitecodes") := paste(sitecode, collapse=", "),
          !!paste0(label, "_acres") := paste(round(.data[[acres_col]],2), collapse=", "),
          .groups = "drop"
        )
    }
    smallest_wide <- make_col(smallest, "Smallest", "min_acres")
    largest_wide  <- make_col(largest, "Largest", "max_acres")
    extremes_wide <- full_join(smallest_wide, largest_wide, by = c("year", "facility"))
    extremes_wide <- extremes_wide %>% arrange(year, facility)
    colnames(extremes_wide) <- c("Year", "Facility", "5 Smallest Sitecodes", "5 Smallest Acres", "5 Largest Sitecodes", "5 Largest Acres")
    return(extremes_wide)
    # Combine into wide format
    make_col <- function(df, label) {
      df %>% group_by(year, facility) %>%
        summarize(
          !!paste0(label, "_sitecodes") := paste(sitecode, collapse=", "),
          !!paste0(label, "_acres") := paste(round(acres,2), collapse=", "),
          .groups = "drop"
        )
    }
    smallest_wide <- make_col(smallest, "Smallest")
    largest_wide  <- make_col(largest, "Largest")
    extremes_wide <- full_join(smallest_wide, largest_wide, by = c("year", "facility"))
    extremes_wide <- extremes_wide %>% arrange(year, facility)
    colnames(extremes_wide) <- c("Year", "Facility", "5 Smallest Sitecodes", "5 Smallest Acres", "5 Largest Sitecodes", "5 Largest Acres")
    return(extremes_wide)
  }, options = list(pageLength = 20, searching = TRUE))
}

# Run the application
shinyApp(ui = ui, server = server)