# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
})

# Source the shared database helper functions
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

# If no .env file found, environment variables should already be set by Docker

# Database configuration using environment variables
db_host <- Sys.getenv("DB_HOST")
db_port <- Sys.getenv("DB_PORT")
db_user <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")
db_name <- Sys.getenv("DB_NAME")

# Define UI for the application
ui <- fluidPage(
  # Application title
  titlePanel("Structures with Active and Expiring Treatments"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      # Slider to control the expiration window
      sliderInput("expiring_days", "Days Until Expiration:",
                  min = 1, max = 30, value = 7, step = 1),
      
      # Date input to simulate "today"
      dateInput("custom_today", "Pretend Today is:",
                value = Sys.Date(), 
                format = "yyyy-mm-dd"),
      
      # Toggle for structure status types
      checkboxGroupInput("status_types", "Include Structure Status:",
                         choices = c("Dry (D)" = "D",
                                     "Wet (W)" = "W", 
                                     "Unknown (U)" = "U"),
                         selected = c("D", "W", "U")),
      
      # Dropdown for facility filter
      selectInput("facility_filter", "Facility:",
                  choices = c("All" = "all", "E", "MO", "N", "Sj", "Sr", "W2", "Wm", "Wp"),
                  selected = "all"),
      
      # Dropdown for structure type filter  
      selectInput("structure_type_filter", "Structure Type:",
                  choices = c("All" = "all", "AP", "CB", "CG", "cv", "CV", "CV/PR", "CV/RR", "DR", "PC", "Pool", "PR", "RG", "RR", "SP", "SS", "US", "W", "wo", "WO", "XX"),
                  selected = "all"),
      
      # Dropdown for priority filter
      selectInput("priority_filter", "Priority:",
                  choices = c("All" = "all", "BLUE", "GREEN", "RED", "YELLOW"),
                  selected = "all"),
      
      helpText("This visualization shows structures by facility with three categories:",
               tags$br(),
               tags$ul(
                 tags$li(tags$span(style = "color:gray", "Gray: Total structures")),
                 tags$li(tags$span(style = paste0("color:", get_status_colors()["active"]), "Green: Structures with active treatments")),
                 tags$li(tags$span(style = paste0("color:", get_status_colors()["planned"]), "Orange: Structures with treatments expiring within the selected days"))
               )),
      
      helpText(tags$b("Date Simulation:"),
               tags$br(),
               "Use 'Pretend Today is' to see what treatments would be active/expiring on any specific date. Only treatments that would still be active on that date are considered."),
      
      helpText(tags$b("Structure Status:"),
               tags$br(),
               tags$ul(
                 tags$li(tags$b("D:"), "Dry - Structure is dry"),
                 tags$li(tags$b("W:"), "Wet - Structure has water"),
                 tags$li(tags$b("U:"), "Unknown - Status not determined")
               ))
    ),
    
    # Main panel for displaying the graph
    mainPanel(
      plotOutput("structureGraph", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Fetch data from database
  raw_data <- reactive({
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = db_name,
      host = db_host,
      port = as.numeric(db_port),
      user = db_user,
      password = db_password
    )
    
    # Build the status filter based on user selection
    status_types <- paste0("'", paste(input$status_types, collapse = "','"), "'")
    
    # Build facility filter condition
    facility_condition <- if (input$facility_filter == "all") "" else sprintf("AND facility = '%s'", input$facility_filter)
    
    # Build structure type filter condition  
    struct_type_condition <- if (input$structure_type_filter == "all") "" else sprintf("AND s_type = '%s'", input$structure_type_filter)
    
    # Build priority filter condition
    priority_condition <- if (input$priority_filter == "all") "" else sprintf("AND priority = '%s'", input$priority_filter)
    
    # Query to get structures from loc_cxstruct
    structures_query <- sprintf("
SELECT sitecode, facility, status_udw, s_type, priority
FROM public.loc_cxstruct
WHERE (status_udw IN (%s) OR status_udw IS NULL)
AND (enddate IS NULL OR enddate > CURRENT_DATE)
%s
%s
%s
", status_types, facility_condition, struct_type_condition, priority_condition)
    
    structures <- dbGetQuery(con, structures_query)
    
    # Query to get treatment information from current treatments
    treatments_query <- "
SELECT t.sitecode, t.facility, t.inspdate, t.matcode, m.effect_days
FROM public.dblarv_insptrt_current t
LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
WHERE t.list_type = 'STR'
"
    treatments <- dbGetQuery(con, treatments_query)
    
    dbDisconnect(con)
    
    # Process the data
    # Identify structures with treatments
    structure_treatments <- treatments %>%
      inner_join(structures, by = c("sitecode", "facility"))
    
    # Calculate treatment status (active) - will be recalculated in processed_data with custom date
    structure_treatments <- structure_treatments %>%
      mutate(
        inspdate = as.Date(inspdate),
        effect_days = ifelse(is.na(effect_days), 0, effect_days),
        treatment_end_date = inspdate + effect_days
      )
    
    # Return all the data needed for filtering later
    list(
      structures = structures,
      structure_treatments = structure_treatments
    )
  })
  
  # Process data based on user inputs
  processed_data <- reactive({
    # Get raw data
    data <- raw_data()
    structures <- data$structures
    structure_treatments <- data$structure_treatments
    
    # Use custom date from input
    current_date <- as.Date(input$custom_today)
    
    # Calculate expiring window
    expiring_start_date <- current_date
    expiring_end_date <- current_date + input$expiring_days
    
    # Update treatment status based on custom date
    structure_treatments <- structure_treatments %>%
      mutate(
        is_active = treatment_end_date >= current_date,
        is_expiring = treatment_end_date >= expiring_start_date &
          treatment_end_date <= expiring_end_date &
          treatment_end_date >= current_date
      )
    
    # Count total structures by facility
    total_structures <- structures %>%
      group_by(facility) %>%
      summarize(
        total_structures = n()
      )
    
    # Count active structures by facility
    # Need to handle overlapping treatments by taking distinct sitecodes only
    active_structures <- structure_treatments %>%
      filter(is_active == TRUE) %>%
      distinct(sitecode, facility) %>%
      group_by(facility) %>%
      summarize(
        active_structures = n()
      )
    
    # Count expiring structures by facility
    expiring_structures <- structure_treatments %>%
      filter(is_expiring == TRUE) %>%
      distinct(sitecode, facility) %>%
      group_by(facility) %>%
      summarize(
        expiring_structures = n()
      )
    
    # Combine all the counts
    result <- total_structures %>%
      left_join(active_structures, by = "facility") %>%
      left_join(expiring_structures, by = "facility") %>%
      mutate(
        active_structures = ifelse(is.na(active_structures), 0, active_structures),
        expiring_structures = ifelse(is.na(expiring_structures), 0, expiring_structures)
      )
    
    return(result)
  })
  
  # Generate the plot
  output$structureGraph <- renderPlot({
    # Get the processed data
    data <- processed_data()
    
    # Handle case when no data is available (e.g., no structures with selected filters)
    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No data available with the selected filters", size = 6) +
          theme_void()
      )
    }
    
    # Set display values (only structures, no acres)
    data$y_total <- data$total_structures
    data$y_active <- data$active_structures
    data$y_expiring <- data$expiring_structures
    y_label <- "Number of Structures"
    title_metric <- "Number of Structures"
    
    # Create a new column to determine which labels to show (avoiding overplot)
    data$show_active_label <- data$y_active != data$y_expiring
    
    # Calculate y-axis maximum for proper positioning
    y_max <- max(data$y_total) * 1.1
    
    # Set up the title with appropriate filters
    status_types_text <- paste(input$status_types, collapse = ", ")
    facility_text <- ifelse(input$facility_filter == "all", "All Facilities", paste("Facility:", input$facility_filter))
    
    # Create the plot
    p <- ggplot(data, aes(x = facility)) +
      # Get status colors from db_helpers
      status_colors <- get_status_colors()
      
      # First draw total bars
      geom_bar(aes(y = y_total), stat = "identity", fill = "gray80", alpha = 0.7) +
      # Then overlay active bars
      geom_bar(aes(y = y_active), stat = "identity", fill = status_colors["active"]) +
      # Finally overlay expiring bars
      geom_bar(aes(y = y_expiring), stat = "identity", fill = status_colors["planned"]) +
      
      # Add labels on top of each bar
      geom_text(aes(y = y_total, label = y_total), vjust = -0.5, color = "black") +
      # Add expiring labels
      geom_text(aes(y = y_expiring, label = y_expiring), vjust = 1.5, color = "black", fontface = "bold") +
      
      # Add labels and title
      labs(
        title = paste("Structures by Facility -", title_metric),
        subtitle = paste("Status types:", status_types_text, "-", facility_text,
                         "- Expiring within", input$expiring_days, "days"),
        x = "Facility",
        y = y_label
      ) +
      # Customize appearance
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        legend.position = "none"
      )
    
    # Add active labels only where they differ from expiring (safer approach)
    if (any(data$show_active_label)) {
      p <- p + geom_text(data = data[data$show_active_label,],
                         aes(y = y_active, label = y_active),
                         vjust = -0.5, color = status_colors["active"], fontface = "bold")
    }
    
    # Add legend manually
    p + annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.9, ymax = y_max * 0.95,
                 fill = "gray80", alpha = 0.7) +
      annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.8, ymax = y_max * 0.85,
               fill = status_colors["active"]) +
      annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.7, ymax = y_max * 0.75,
               fill = status_colors["planned"]) +
      annotate("text", x = 0.1, y = y_max * 0.925,
               label = "Total", hjust = 0) +
      annotate("text", x = 0.1, y = y_max * 0.825,
               label = "Active", hjust = 0) +
      annotate("text", x = 0.1, y = y_max * 0.725,
               label = "Expiring", hjust = 0)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
