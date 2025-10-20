library(shiny)
library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)

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

# If no .env file found, environment variables should already be set by Docker

# Database configuration using environment variables
db_host <- Sys.getenv("DB_HOST")
db_port <- Sys.getenv("DB_PORT")
db_user <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")
db_name <- Sys.getenv("DB_NAME")

# Define UI for the app
ui <- fluidPage(
  titlePanel("Proportion of Structures with Active Treatment current and historical"),
  
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
      ),
      
      # Dropdown for structure type filter
      selectInput(
        "structure_type_filter",
        "Structure Type:",
        choices = c("all", "AP", "CG", "CV", "PC", "PR", "RG", "RR", "SP", "SS", "US", "WO", "XX"),
        selected = "all"
      ),
      
      # Dropdown for priority filter
      selectInput(
        "priority_filter",
        "Priority:",
        choices = c("all", "BLUE", "GREEN", "RED", "YELLOW"),
        selected = "all"
      ),
      
      # Checkbox for priority breakdown view
      checkboxInput(
        "show_priority_breakdown",
        "Show Priority Breakdown",
        value = FALSE
      ),
      
      # Multi-select for status_udw values to include
      checkboxGroupInput(
        "include_status_values",
        "Include Status Values:",
        choices = c("D (Dry)" = "D", "W (Wet)" = "W", "U (Unknown)" = "U"),
        selected = c("D", "W", "U")
      ),
      
      # Date input for priority breakdown (conditional)
      conditionalPanel(
        condition = "input.show_priority_breakdown == true",
        dateInput(
          "snapshot_date",
          "Snapshot Date:",
          value = Sys.Date(),
          format = "yyyy-mm-dd"
        )
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
  
  # Function to construct structure type filter condition for SQL
  get_structure_type_condition <- function(structure_type) {
    if (structure_type == "all") {
      return("")
    } else {
      # Handle compound types (e.g., CV/PR matches both CV and PR)
      # Use case-insensitive matching with UPPER()
      return(sprintf("AND (UPPER(loc.s_type) = '%s' OR UPPER(loc.s_type) LIKE '%%/%s' OR UPPER(loc.s_type) LIKE '%s/%%' OR UPPER(loc.s_type) LIKE '%%/%s/%%')", 
                     toupper(structure_type), toupper(structure_type), toupper(structure_type), toupper(structure_type)))
    }
  }
  
  # Function to construct priority filter condition for SQL
  get_priority_condition <- function(priority) {
    if (priority == "all") {
      return("")
    } else {
      return(sprintf("AND loc.priority = '%s'", priority))
    }
  }
  
  # Function to construct status_udw filter condition for SQL
  get_status_condition <- function(include_status_values) {
    if (is.null(include_status_values) || length(include_status_values) == 0) {
      # If no status values selected, exclude all structures with status_udw
      return("AND loc.status_udw IS NULL")
    } else if (length(include_status_values) == 3) {
      # If all status values selected, include all structures
      return("")
    } else {
      # Include only selected status values (case-insensitive)
      status_list <- paste0("'", include_status_values, "'", collapse = ", ")
      return(sprintf("AND (loc.status_udw IS NULL OR UPPER(loc.status_udw) IN (%s))", status_list))
    }
  }
  
  get_facility_condition_total <- function(facility, structure_type, priority, include_status_values) {
    conditions <- c()
    if (facility != "all") {
      conditions <- c(conditions, sprintf("facility = '%s'", facility))
    }
    if (structure_type != "all") {
      # Handle compound types and case-insensitivity for total structure count
      conditions <- c(conditions, sprintf("(UPPER(s_type) = '%s' OR UPPER(s_type) LIKE '%%/%s' OR UPPER(s_type) LIKE '%s/%%' OR UPPER(s_type) LIKE '%%/%s/%%')", 
                                        toupper(structure_type), toupper(structure_type), toupper(structure_type), toupper(structure_type)))
    }
    if (priority != "all") {
      conditions <- c(conditions, sprintf("priority = '%s'", priority))
    }
    
    # Handle status filter
    if (is.null(include_status_values) || length(include_status_values) == 0) {
      conditions <- c(conditions, "status_udw IS NULL")
    } else if (length(include_status_values) < 3) {
      # If not all status values selected, filter accordingly
      status_list <- paste0("'", include_status_values, "'", collapse = ", ")
      conditions <- c(conditions, sprintf("(status_udw IS NULL OR UPPER(status_udw) IN (%s))", status_list))
    }
    # If all 3 status values selected, no additional condition needed
    
    if (length(conditions) > 0) {
      return(paste("AND", paste(conditions, collapse = " AND ")))
    } else {
      return("")
    }
  }
  
  # Reactive function to fetch and process data
  treatment_data <- reactive({
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = db_name,
      host = db_host,
      port = as.numeric(db_port),
      user = db_user,
      password = db_password
    )
    
    # Fetch archive data with structure info
    query_archive <- sprintf(
      "
SELECT
trt.sitecode,
trt.inspdate,
COALESCE(mat.effect_days, 0) AS effect_days,
loc.s_type,
loc.priority,
loc.facility
FROM public.dblarv_insptrt_archive trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
WHERE trt.inspdate >= date '%d-01-01'
AND trt.inspdate < date '%d-01-01'
AND trt.list_type = 'STR'
%s
%s
%s
%s
",
      as.numeric(input$start_year),
      as.numeric(input$end_year) + 1,
      get_facility_condition(input$facility_filter),
      get_structure_type_condition(input$structure_type_filter),
      get_priority_condition(input$priority_filter),
      get_status_condition(input$include_status_values)
    )
    archive_data <- dbGetQuery(con, query_archive)
    
    # Fetch current data with structure info
    query_current <- sprintf(
      "
SELECT
trt.sitecode,
trt.inspdate,
COALESCE(mat.effect_days, 0) AS effect_days,
loc.s_type,
loc.priority,
loc.facility
FROM public.dblarv_insptrt_current trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
WHERE trt.inspdate >= date '%d-01-01'
AND trt.inspdate < date '%d-01-01'
AND trt.list_type = 'STR'
%s
%s
%s
%s
",
      as.numeric(input$start_year),
      as.numeric(input$end_year) + 1,
      get_facility_condition(input$facility_filter),
      get_structure_type_condition(input$structure_type_filter),
      get_priority_condition(input$priority_filter),
      get_status_condition(input$include_status_values)
    )
    current_data <- dbGetQuery(con, query_current)
    
    #Fetch Total Structures (active structures only)
    query_total_structures <- sprintf(
      "
SELECT COUNT(DISTINCT sitecode) AS total_structures
FROM loc_cxstruct
WHERE 1=1
AND (enddate IS NULL OR enddate > CURRENT_DATE)
%s
",
      get_facility_condition_total(input$facility_filter, input$structure_type_filter, input$priority_filter, input$include_status_values)
    )
    
    total_structures <- dbGetQuery(con, query_total_structures)$total_structures
    dbDisconnect(con)
    
    # Combine and process data
    combined_data <- bind_rows(archive_data, current_data) %>%
      mutate(inspdate = as.Date(inspdate),
             enddate = inspdate + effect_days)
    
    list(data = combined_data, total_structures = total_structures)
  })
  
  # Generate the plot
  output$treatment_plot <- renderPlot({
    td <- treatment_data()
    data <- td$data
    total_structures <- td$total_structures
    
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
    
    # Handle cases where total_structures is missing or zero
    if (is.null(total_structures) || is.na(total_structures) || total_structures == 0) {
      return(
        ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "No structure data available or total structures is zero.",
            size = 6
          ) +
          theme_void()
      )
    }
    
    # Generate priority breakdown plot if selected
    if (input$show_priority_breakdown) {
      # Calculate active treatments by priority for the snapshot date
      snapshot_date <- as.Date(input$snapshot_date)
      if (is.null(snapshot_date)) {
        snapshot_date <- Sys.Date()
      }
      
      # Get priority totals for active structures on snapshot date
      con <- dbConnect(
        RPostgres::Postgres(),
        dbname = db_name,
        host = db_host,
        port = as.numeric(db_port),
        user = db_user,
        password = db_password
      )
      
      query_priority_totals <- sprintf(
        "
SELECT priority, COUNT(DISTINCT sitecode) AS total_structures
FROM loc_cxstruct
WHERE 1=1
AND (enddate IS NULL OR enddate > '%s')
%s
GROUP BY priority
",
        as.character(snapshot_date),
        get_facility_condition_total(input$facility_filter, input$structure_type_filter, "all", input$include_status_values)
      )
      priority_totals <- dbGetQuery(con, query_priority_totals)
      dbDisconnect(con)
      
      # Filter data for treatments active on snapshot date (count distinct sites only)
      active_treatments_by_priority <- data %>%
        filter(inspdate <= snapshot_date & (inspdate + effect_days) >= snapshot_date) %>%
        # Count distinct sitecodes only, regardless of overlapping treatments
        distinct(sitecode, priority) %>%
        group_by(priority) %>%
        summarize(active_count = n(), .groups = "drop")
      
      # Merge with totals
      priority_summary <- priority_totals %>%
        left_join(active_treatments_by_priority, by = "priority") %>%
        mutate(
          active_count = ifelse(is.na(active_count), 0, active_count),
          proportion = active_count / total_structures,
          priority = ifelse(is.na(priority), "Unknown", priority)
        )
      
      # Create bar chart
      return(
        ggplot(priority_summary, aes(x = priority, y = proportion, fill = priority)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = c(
            "BLUE" = "#2E86C1", 
            "GREEN" = "#27AE60", 
            "RED" = "#E74C3C", 
            "YELLOW" = "#F39C12",
            "Unknown" = "#7F8C8D"
          )) +
          labs(
            title = sprintf("Active Treatment Proportion by Priority (%s)", format(snapshot_date, "%Y-%m-%d")),
            x = "Priority Level",
            y = "Proportion with Active Treatment",
            fill = "Priority"
          ) +
          scale_y_continuous(labels = scales::percent_format()) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    }
    
    # Generate the full date range for time series
    start_date <- as.Date(sprintf("%d-03-01", as.numeric(input$start_year)))
    end_date <- as.Date(sprintf("%d-12-31", as.numeric(input$end_year)))
    date_range <- seq.Date(start_date, end_date, by = "day")
    
    # Get structure start/end dates to calculate dynamic totals
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = db_name,
      host = db_host,
      port = as.numeric(db_port),
      user = db_user,
      password = db_password
    )
    
    query_structure_dates <- sprintf(
      "
SELECT sitecode, startdate, enddate
FROM loc_cxstruct
WHERE 1=1
%s
",
      get_facility_condition_total(input$facility_filter, input$structure_type_filter, input$priority_filter, input$include_status_values)
    )
    structure_dates <- dbGetQuery(con, query_structure_dates)
    dbDisconnect(con)
    
    # Create a dataframe of daily changes for treatments (by sitecode to avoid double-counting)
    # First, get unique treatment periods by sitecode
    treatment_periods <- data %>%
      select(sitecode, inspdate, effect_days) %>%
      mutate(enddate = inspdate + effect_days) %>%
      arrange(sitecode, inspdate)
    
    # For each sitecode, merge overlapping treatment periods
    merged_treatments <- treatment_periods %>%
      group_by(sitecode) %>%
      arrange(inspdate) %>%
      mutate(
        # Create groups for overlapping treatments
        lag_enddate = lag(inspdate + effect_days, default = as.Date("1900-01-01")),
        new_period = inspdate > lag_enddate,
        period_id = cumsum(new_period)
      ) %>%
      group_by(sitecode, period_id) %>%
      summarize(
        start_date = min(inspdate),
        end_date = max(inspdate + effect_days),
        .groups = "drop"
      )
    
    daily_treatment_changes <- data.frame(
      date = c(merged_treatments$start_date, merged_treatments$end_date),
      change = c(rep(1, nrow(merged_treatments)), rep(-1, nrow(merged_treatments))) # +1 for start, -1 for end
    ) %>%
      group_by(date) %>%
      summarize(treatment_change = sum(change)) %>%
      arrange(date)
    
    # Create a dataframe of daily changes for structure counts
    structure_changes <- data.frame()
    if (nrow(structure_dates) > 0) {
      start_changes <- structure_dates %>%
        filter(!is.na(startdate)) %>%
        mutate(date = as.Date(startdate), structure_change = 1) %>%
        select(date, structure_change)
      
      end_changes <- structure_dates %>%
        filter(!is.na(enddate)) %>%
        mutate(date = as.Date(enddate), structure_change = -1) %>%
        select(date, structure_change)
      
      structure_changes <- bind_rows(start_changes, end_changes) %>%
        group_by(date) %>%
        summarize(structure_change = sum(structure_change)) %>%
        arrange(date)
    }
    
    # Calculate cumulative active treatments and total structures over time
    all_dates <- data.frame(date = date_range)
    treatment_trends <- all_dates %>%
      left_join(daily_treatment_changes, by = "date") %>%
      left_join(structure_changes, by = "date") %>%
      mutate(
        treatment_change = ifelse(is.na(treatment_change), 0, treatment_change),
        structure_change = ifelse(is.na(structure_change), 0, structure_change)
      ) %>%
      arrange(date) %>%
      mutate(
        active_treatments = cumsum(treatment_change),
        total_structures_dynamic = total_structures + cumsum(structure_change)
      ) %>%
      # Avoid division by zero
      mutate(
        proportion_active_treatment = ifelse(
          total_structures_dynamic > 0, 
          active_treatments / total_structures_dynamic, 
          0
        )
      )
    
    # Create filter description for title
    filters <- c()
    if (input$facility_filter != "all") filters <- c(filters, paste("Facility:", input$facility_filter))
    if (input$structure_type_filter != "all") filters <- c(filters, paste("Type:", input$structure_type_filter))
    if (input$priority_filter != "all") filters <- c(filters, paste("Priority:", input$priority_filter))
    
    filter_text <- if (length(filters) > 0) paste(" -", paste(filters, collapse = ", ")) else ""
    
    # Plot the data
    ggplot(treatment_trends,
           aes(x = date, y = proportion_active_treatment)) +
      geom_line(color = "blue") +
      labs(
        title = sprintf(
          "Proportion of Structures with Active Treatment (%d to %d)%s",
          as.numeric(input$start_year),
          as.numeric(input$end_year),
          filter_text
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
