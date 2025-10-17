# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
})

# Database configuration (HARDCODED TEMPORARILY)

# Define UI for the application
ui <- fluidPage(
  # Application title
  titlePanel("Drone Sites with Active and Expiring Treatments"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      # Toggle button to switch between sites and acres
      radioButtons("display_metric", "Display Metric:",
                   choices = c("Number of Sites" = "sites",
                               "Total Acres" = "acres"),
                   selected = "sites"),
      
      # Slider to control the expiration window
      sliderInput("expiring_days", "Days Until Expiration:",
                  min = 1, max = 30, value = 7, step = 1),
      
      # Toggle for prehatch filter
      checkboxInput("prehatch_only", "Show Only Prehatch Sites", value = FALSE),
      
      # Toggle for drone designation types
      checkboxGroupInput("drone_types", "Include Drone Designations:",
                         choices = c("Yes (Y)" = "Y",
                                     "Maybe (M)" = "M",
                                     "Considering (C)" = "C"),
                         selected = "Y"),
      
      helpText("This visualization shows drone sites by facility with three categories:",
               tags$br(),
               tags$ul(
                 tags$li(tags$span(style = "color:gray", "Gray: Total sites/acres")),
                 tags$li(tags$span(style = "color:steelblue", "Blue: Sites/acres with active treatments")),
                 tags$li(tags$span(style = "color:orange", "Orange: Sites/acres with treatments expiring within the selected days"))
               )),
      
      helpText(tags$b("Drone Designations:"),
               tags$br(),
               tags$ul(
                 tags$li(tags$b("Y:"), "Yes - Confirmed drone site"),
                 tags$li(tags$b("M:"), "Maybe - Potential drone site"),
                 tags$li(tags$b("C:"), "Considering (?) - Under evaluation"),
                 tags$li(tags$b("N:"), "No - Not suitable for drone (excluded)")
               ))
    ),
    
    # Main panel for displaying the graph
    mainPanel(
      plotOutput("droneGraph", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Current date
  current_date <- Sys.Date() 
  
  # Fetch data from database
  raw_data <- reactive({
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = "mmcd_data",
      host = "rds-readonly.mmcd.org",
      port = 5432,
      user = "mmcd_read",
      password = "mmcd2012"
    )
    
    # Build the drone designation filter based on user selection
    drone_types <- paste0("'", paste(input$drone_types, collapse = "','"), "'")
    
    # Query to get drone sites from loc_breeding_sites including acres and prehatch status
    # Include both drone='Y' and air_gnd='D' as indicators
    drone_sites_query <- sprintf("
SELECT sitecode, facility, acres, prehatch, drone
FROM public.loc_breeding_sites
WHERE (drone IN (%s) OR air_gnd = 'D')
AND enddate IS NULL
", drone_types)
    
    drone_sites <- dbGetQuery(con, drone_sites_query)
    
    # Query to get treatment information and material types
    treatments_query <- "
SELECT t.sitecode, t.facility, t.inspdate, t.matcode, t.acres, m.effect_days
FROM public.dblarv_insptrt_current t
LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
"
    treatments <- dbGetQuery(con, treatments_query)
    
    dbDisconnect(con)
    
    # Process the data
    # Identify drone sites with treatments
    drone_treatments <- treatments %>%
      inner_join(drone_sites, by = c("sitecode", "facility")) %>%
      # Use treatment acres if available, otherwise use site acres
      mutate(acres = ifelse(is.na(acres.x), acres.y, acres.x))
    
    #Calculate treatment status (active)
    drone_treatments <- drone_treatments %>%
      mutate(
        inspdate = as.Date(inspdate),
        effect_days = ifelse(is.na(effect_days), 0, effect_days),
        treatment_end_date = inspdate + effect_days,
        is_active = treatment_end_date >= current_date
      )
    
    # Return all the data needed for filtering later
    list(
      drone_sites = drone_sites,
      drone_treatments = drone_treatments
    )
  })
  
  # Process data based on user inputs
  processed_data <- reactive({
    # Get raw data
    data <- raw_data()
    drone_sites <- data$drone_sites
    drone_treatments <- data$drone_treatments
    
    # Apply prehatch filter if selected
    if (input$prehatch_only) {
      # Filter to only prehatch sites
      drone_sites <- drone_sites %>%
        filter(prehatch == 'PREHATCH')
      
      # Update treatments to only include prehatch sites
      drone_treatments <- drone_treatments %>%
        filter(prehatch == 'PREHATCH')
    }
    
    # Calculate expiring window
    expiring_start_date <- current_date
    expiring_end_date <- current_date + input$expiring_days
    
    # Update treatment status to include expiring
    drone_treatments <- drone_treatments %>%
      mutate(
        is_expiring = is_active & treatment_end_date >= expiring_start_date &
          treatment_end_date <= expiring_end_date
      )
    
    # Count total drone sites and total acres by facility
    total_drone_sites <- drone_sites %>%
      group_by(facility) %>%
      summarize(
        total_sites = n(),
        total_acres = sum(acres, na.rm = TRUE)
      )
    
    # Count active drone sites and active acres by facility
    active_drone_sites <- drone_treatments %>%
      filter(is_active == TRUE) %>%
      distinct(sitecode, facility, acres) %>%
      group_by(facility) %>%
      summarize(
        active_sites = n(),
        active_acres = sum(acres, na.rm = TRUE)
      )
    
    # Count expiring drone sites and expiring acres by facility
    expiring_drone_sites <- drone_treatments %>%
      filter(is_expiring == TRUE) %>%
      distinct(sitecode, facility, acres) %>%
      group_by(facility) %>%
      summarize(
        expiring_sites = n(),
        expiring_acres = sum(acres, na.rm = TRUE)
      )
    
    # Combine all the counts and acres
    result <- total_drone_sites %>%
      left_join(active_drone_sites, by = "facility") %>%
      left_join(expiring_drone_sites, by = "facility") %>%
      mutate(
        active_sites = ifelse(is.na(active_sites), 0, active_sites),
        active_acres = ifelse(is.na(active_acres), 0, active_acres),
        expiring_sites = ifelse(is.na(expiring_sites), 0, expiring_sites),
        expiring_acres = ifelse(is.na(expiring_acres), 0, expiring_acres),
        # Round acres to whole numbers
        total_acres = round(total_acres),
        active_acres = round(active_acres),
        expiring_acres = round(expiring_acres)
      )
    
    return(result)
  })
  
  # Generate the plot
  output$droneGraph <- renderPlot({
    # Get the processed data
    data <- processed_data()
    
    # Handle case when no data is available (e.g., no prehatch sites)
    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No data available with the selected filters", size = 6) +
          theme_void()
      )
    }
    
    # Choose which metric to display based on user input
    if (input$display_metric == "sites") {
      # Display number of sites
      data$y_total <- data$total_sites
      data$y_active <- data$active_sites
      data$y_expiring <- data$expiring_sites
      y_label <- "Number of Sites"
      title_metric <- "Number of Sites"
    } else {
      # Display acres
      data$y_total <- data$total_acres
      data$y_active <- data$active_acres
      data$y_expiring <- data$expiring_acres
      y_label <- "Total Acres"
      title_metric <- "Acres"
    }
    
    # Create a new column to determine which labels to show (avoiding overplot)
    data$show_active_label <- data$y_active != data$y_expiring
    
    # Calculate y-axis maximum for proper positioning
    y_max <- max(data$y_total) * 1.1
    
    # Set up the title with appropriate filters
    drone_types_text <- paste(input$drone_types, collapse = ", ")
    prehatch_filter_text <- ifelse(input$prehatch_only, "Prehatch Sites Only", "All Sites")
    
    # Create the plot
    p <- ggplot(data, aes(x = facility)) +
      # First draw total bars
      geom_bar(aes(y = y_total), stat = "identity", fill = "gray80", alpha = 0.7) +
      # Then overlay active bars
      geom_bar(aes(y = y_active), stat = "identity", fill = "steelblue") +
      # Finally overlay expiring bars
      geom_bar(aes(y = y_expiring), stat = "identity", fill = "orange") +
      
      # Add labels on top of each bar
      geom_text(aes(y = y_total, label = y_total), vjust = -0.5, color = "black") +
      # Add expiring labels
      geom_text(aes(y = y_expiring, label = y_expiring), vjust = 1.5, color = "black", fontface = "bold") +
      
      # Add labels and title
      labs(
        title = paste("Drone Sites by Facility -", title_metric),
        subtitle = paste("Drone types:", drone_types_text, "-", prehatch_filter_text,
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
                         vjust = -0.5, color = "steelblue", fontface = "bold")
    }
    
    # Add legend manually
    p + annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.9, ymax = y_max * 0.95,
                 fill = "gray80", alpha = 0.7) +
      annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.8, ymax = y_max * 0.85,
               fill = "steelblue") +
      annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.7, ymax = y_max * 0.75,
               fill = "orange") +
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

