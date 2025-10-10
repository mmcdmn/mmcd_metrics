# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(lubridate)
})

# Define UI for the application
ui <- fluidPage(
  # Application title
  titlePanel("Treatment Analysis Dashboard"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      # Date range selection
      dateRangeInput("date_range", "Select Date Range:",
                     start = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                     end = Sys.Date(),
                     format = "yyyy-mm-dd"),
      
      # Treatment duration selection
      numericInput("treatment_duration", "Treatment Duration (days):",
                   value = 30, min = 1, max = 365, step = 1),
      
      # Refresh button
      actionButton("refresh", "Refresh Data", class = "btn-primary"),
      
      hr(),
      
      # Summary info
      h4("Summary"),
      verbatimTextOutput("summary_text")
    ),
    
    # Main panel for displaying the plot
    mainPanel(
      tabsetPanel(
        tabPanel("Trend Plot", 
                 plotOutput("trend_plot", height = "500px")),
        tabPanel("Data Table", 
                 dataTableOutput("data_table"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive function to fetch and process data
  treatment_data <- reactive({
    input$refresh  # Dependency on refresh button
    
    # Connect to the PostgreSQL database
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = "mmcd_data",
      host = "rds-readonly.mmcd.org",  # Updated to match your other apps
      port = 5432,
      user = "mmcd_read",
      password = "mmcd2012"
    )
    
    # Fetch treatment data
    start_date_str <- format(input$date_range[1], "%Y-%m-%d")
    end_date_str <- format(input$date_range[2], "%Y-%m-%d")
    
    query <- sprintf("
    SELECT
      sitecode,
      inspdate
    FROM dblarv_insptrt_current
    WHERE inspdate BETWEEN '%s' AND '%s'
    ", start_date_str, end_date_str)
    
    treatment_data <- dbGetQuery(con, query)
    
    # Fetch total structures
    query_total_structures <- "
    SELECT COUNT(DISTINCT sitecode) AS total_structures
    FROM loc_cxstruct
    WHERE enddate IS NULL AND status_UDW = 'W'
    "
    total_structures <- dbGetQuery(con, query_total_structures)$total_structures
    
    # Close the database connection
    dbDisconnect(con)
    
    # Process the data if we have any
    if (nrow(treatment_data) > 0) {
      # Generate the full date range
      date_range <- seq.Date(input$date_range[1], input$date_range[2], by = "day")
      
      # Process treatment data
      treatment_data <- treatment_data %>%
        mutate(
          inspdate = as.Date(inspdate),
          enddate = inspdate + input$treatment_duration
        )
      
      # Create a dataframe of daily changes
      daily_changes <- data.frame(
        date = c(treatment_data$inspdate, treatment_data$enddate),
        change = c(rep(1, nrow(treatment_data)), rep(-1, nrow(treatment_data)))
      )
      
      # Aggregate daily changes by date
      daily_changes <- daily_changes %>%
        group_by(date) %>%
        summarize(change = sum(change), .groups = "drop") %>%
        arrange(date)
      
      # Calculate cumulative active treatments
      all_dates <- data.frame(date = date_range)
      treatment_trends <- all_dates %>%
        left_join(daily_changes, by = "date") %>%
        mutate(change = ifelse(is.na(change), 0, change)) %>%
        arrange(date) %>%
        mutate(
          active_treatments = cumsum(change),
          proportion_active_treatment = active_treatments / total_structures
        )
      
      return(list(
        trends = treatment_trends,
        total_structures = total_structures,
        total_treatments = nrow(treatment_data)
      ))
    } else {
      return(list(
        trends = data.frame(),
        total_structures = 0,
        total_treatments = 0
      ))
    }
  })
  
  # Generate trend plot
  output$trend_plot <- renderPlot({
    data <- treatment_data()
    
    if (nrow(data$trends) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No treatment data available for the selected date range", size = 6) +
          theme_void()
      )
    }
    
    ggplot(data$trends, aes(x = date, y = proportion_active_treatment)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 1) +
      labs(
        title = "Proportion of Structures with Active Treatment Over Time",
        subtitle = paste("Treatment Duration:", input$treatment_duration, "days"),
        x = "Date",
        y = "Proportion of Active Treatment"
      ) +
      scale_y_continuous(labels = percent_format()) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Generate summary text
  output$summary_text <- renderText({
    data <- treatment_data()
    
    paste(
      "Total Structures:", format(data$total_structures, big.mark = ","), "\n",
      "Total Treatments:", format(data$total_treatments, big.mark = ","), "\n",
      "Date Range:", format(input$date_range[1], "%Y-%m-%d"), "to", format(input$date_range[2], "%Y-%m-%d")
    )
  })
  
  # Generate data table
  output$data_table <- renderDataTable({
    data <- treatment_data()
    
    if (nrow(data$trends) > 0) {
      data$trends %>%
        select(date, active_treatments, proportion_active_treatment) %>%
        mutate(
          proportion_active_treatment = percent(proportion_active_treatment, accuracy = 0.1)
        ) %>%
        rename(
          Date = date,
          "Active Treatments" = active_treatments,
          "Proportion Active" = proportion_active_treatment
        )
    } else {
      data.frame()
    }
  }, options = list(pageLength = 15, searching = TRUE))
}

# Run the application
shinyApp(ui = ui, server = server)
