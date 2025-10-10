library(shiny)
library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Proportion of Structures with Active Treatment"),
  
  # Sidebar layout for user inputs
  sidebarLayout(
    sidebarPanel(
      # Dropdown for start year
      selectInput(
        "start_year",
        "Start Year:",
        choices = seq(2010, 2025),
        selected = 2018
      ),
      
      # Dropdown for end year
      selectInput(
        "end_year",
        "End Year:",
        choices = seq(2010, 2025),
        selected = 2025
      ),
      
      # Dropdown for facility filter
      selectInput(
        "facility_filter",
        "Facility:",
        choices = c("all", "E", "MO", "N", "Sj", "Sr", "W2", "Wm", "Wp"),
        selected = "all"
      )
    ),
    
    mainPanel(
      # Output plot
      plotOutput("treatment_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Function to construct the facility filter condition for SQL
  get_facility_condition <- function(facility) {
    if (facility == "all") {
      return("") # No filtering
    } else {
      return(sprintf("AND trt.facility = '%s'", facility)) # Filter by the specified facility
    }
  }
  
  ##!!!!!!!!!!!!-----------
  # NOTE: this may only get us the total structure count no matter what was put
  ##-----------------------
  get_facility_condition_total <- function(facility) {
    if (facility == "all") {
      return("") # No filtering
    } else {
      return(sprintf("AND facility = '%s'", facility)) # Reference the correct column directly
    }
  }
  
  # Reactive function to fetch and process data
  treatment_data <- reactive({
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = "mmcd_data",
      host = "rds-readonly.mmcd.org",
      port = 5432,
      user = "mmcd_read",
      password = "mmcd2012"
    )
    
    # Fetch archive data
    query_archive <- sprintf(
      "
SELECT
trt.sitecode,
trt.inspdate,
COALESCE(mat.effect_days, 0) AS effect_days
FROM public.dblarv_insptrt_archive trt
LEFT JOIN public.mattype_list_targetdose mat
ON trt.matcode = mat.matcode
WHERE trt.inspdate >= date '%d-01-01'
AND trt.inspdate < date '%d-01-01'
AND trt.list_type = 'STR'
%s
",
      as.numeric(input$start_year),
      as.numeric(input$end_year) + 1,
      get_facility_condition(input$facility_filter)
    )
    archive_data <- dbGetQuery(con, query_archive)
    
    # Fetch current data
    query_current <- sprintf(
      "
SELECT
trt.sitecode,
trt.inspdate,
COALESCE(mat.effect_days, 0) AS effect_days
FROM public.dblarv_insptrt_current trt
LEFT JOIN public.mattype_list_targetdose mat
ON trt.matcode = mat.matcode
WHERE trt.inspdate >= date '%d-01-01'
AND trt.inspdate < date '%d-01-01'
AND trt.list_type = 'STR'
%s
",
      as.numeric(input$start_year),
      as.numeric(input$end_year) + 1,
      get_facility_condition(input$facility_filter)
    )
    current_data <- dbGetQuery(con, query_current)
    
    #Fetch Total Structures
    query_total_structures <- sprintf(
      "
SELECT COUNT(DISTINCT sitecode) AS total_structures
FROM loc_cxstruct
WHERE 1=1
AND enddate IS NULL
%s
",
      get_facility_condition_total(input$facility_filter)
    )
    
    total_structures <- dbGetQuery(con, query_total_structures)$total_structures
    dbDisconnect(con)
    
    # Combine and process data
    bind_rows(archive_data, current_data) %>%
      mutate(inspdate = as.Date(inspdate),
             enddate = inspdate + effect_days)
  })
  
  # Generate the plot
  output$treatment_plot <- renderPlot({
    data <- treatment_data()
    
    # Handle cases where no data is available
    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "No data available for the selected range.",
            size = 6
          ) +
          theme_void()
      )
    }
    
    # Generate the full date range
    start_date <- as.Date(sprintf("%d-03-01", as.numeric(input$start_year)))
    end_date <- as.Date(sprintf("%d-12-31", as.numeric(input$end_year)))
    date_range <- seq.Date(start_date, end_date, by = "day")
    
    # Create a dataframe of daily changes
    daily_changes <- data.frame(
      date = c(data$inspdate, data$enddate),
      change = c(rep(1, nrow(data)), rep(-1, nrow(data))) # +1 for start, -1 for end
    ) %>%
      group_by(date) %>%
      summarize(change = sum(change)) %>%
      arrange(date)
    
    # Calculate cumulative active treatments
    all_dates <- data.frame(date = date_range)
    treatment_trends <- all_dates %>%
      left_join(daily_changes, by = "date") %>%
      mutate(change = ifelse(is.na(change), 0, change)) %>%
      arrange(date) %>%
      mutate(active_treatments = cumsum(change))
    
    # Normalize to proportion of total structures
    treatment_trends <- treatment_trends %>%
      mutate(proportion_active_treatment = active_treatments / total_structures)
    
    # Plot the data
    ggplot(treatment_trends,
           aes(x = date, y = proportion_active_treatment)) +
      geom_line(color = "blue") +
      labs(
        title = sprintf(
          "Proportion of Structures with Active Treatment (%d to %d)",
          as.numeric(input$start_year),
          as.numeric(input$end_year)
        ),
        x = "Date",
        y = "Proportion of Active Treatment"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
