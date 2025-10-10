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
      
      helpText("This graph shows drone treatments by facility over time.",
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
      dataTableOutput("summaryTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Fetch all raw data
  raw_data <- reactive({
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = "mmcd_data",
      host = "rds-readonly.mmcd.org",
      port = 5432,
      user = "mmcd_read",
      password = "mmcd2012"
    )
    
    # Get archive data with complete record details
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
    
    # Get current data with complete record details
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
    
    dbDisconnect(con)
    
    # Combine the datasets
    list(
      archive = archive_data,
      current = current_data
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
  
  # Generate the summary table
  output$summaryTable <- renderDataTable({
    # Get the filtered data
    data <- processed_data()
    
    # Create a summary table
    summary_table <- data %>%
      group_by(facility) %>%
      summarize(
        Total_Count = sum(count),
        Years_with_Data = sum(count > 0)
      ) %>%
      arrange(desc(Total_Count))
    
    # Set column names based on count type
    if (input$count_type == "treatments") {
      colnames(summary_table) <- c("Facility", "Total Treatments", "Years with Data")
    } else {
      colnames(summary_table) <- c("Facility", "Total Unique Sites", "Years with Data")
    }
    
    return(summary_table)
  }, options = list(pageLength = 10, searching = FALSE, dom = 't'))
}

# Run the application
shinyApp(ui = ui, server = server)