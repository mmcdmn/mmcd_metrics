# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
})

# Source shared helper functions
source("../../shared/db_helpers.R")

# Load environment variables
load_env_vars()

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
                 tags$li(tags$span(style = paste0("color:", get_status_colors()["unknown"]), "Gray: Total sites/acres")),
                 tags$li(tags$span(style = paste0("color:", get_status_colors()["active"]), "Green: Sites/acres with active treatments")),
                 tags$li(tags$span(style = paste0("color:", get_status_colors()["planned"]), "Orange: Sites/acres with treatments expiring within the selected days"))
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
    
    # Main panel for displaying the graphs with tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Current Progress", 
                 plotOutput("droneGraph", height = "600px")
        ),
        tabPanel("Historical Trends",
                 # Historical controls row
                 fluidRow(
                   column(3,
                          radioButtons("hist_count_type", "Display Metric:",
                                       choices = c("Number of Treatments" = "treatments",
                                                   "Number of Unique Sites" = "sites"),
                                       selected = "treatments")
                   ),
                   column(3,
                          selectInput("hist_start_year", "Start Year:",
                                      choices = seq(2010, 2025),
                                      selected = 2018)
                   ),
                   column(3,
                          selectInput("hist_end_year", "End Year:",
                                      choices = seq(2010, 2025),
                                      selected = 2025)
                   ),
                   column(3,
                          checkboxInput("hist_show_percentages", "Show Percentages", value = FALSE)
                   )
                 ),
                 plotOutput("historicalGraph", height = "600px"),
                 plotOutput("siteAvgSizeGraph", height = "400px"),
                 tableOutput("summaryTable"),
                 h4("5 Smallest and 5 Largest Drone Sites per Year"),
                 tableOutput("siteExtremesTable")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Current date
  current_date <- Sys.Date() 
  
  # Fetch data from database
  raw_data <- reactive({
    con <- get_db_connection()
    if (is.null(con)) return(data.frame())
    
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
      summarise(
        total_sites = n(),
        total_acres = sum(acres, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      map_facility_names()
    
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
    
    # Get colors from shared helper functions
    status_colors <- get_status_colors()
    
    # Map colors for this application's specific needs
    total_color <- status_colors["unknown"]        # Gray for total sites
    active_color <- status_colors["active"]        # Green for active treatments
    expiring_color <- status_colors["planned"]     # Orange for expiring treatments
    
    # Create the plot
    p <- ggplot(data, aes(x = facility_display)) +
      # First draw total bars
      geom_bar(aes(y = y_total), stat = "identity", fill = total_color, alpha = 0.7) +
      # Then overlay active bars
      geom_bar(aes(y = y_active), stat = "identity", fill = active_color) +
      # Finally overlay expiring bars
      geom_bar(aes(y = y_expiring), stat = "identity", fill = expiring_color) +
      
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
        axis.text.x = element_text(face = "bold", size = 16, color = "black", angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    # Add active labels only where they differ from expiring (safer approach)
    if (any(data$show_active_label)) {
      p <- p + geom_text(data = data[data$show_active_label,],
                         aes(y = y_active, label = y_active),
                         vjust = -0.5, color = active_color, fontface = "bold")
    }
    
    # Add legend manually using mapped colors
    p + annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.9, ymax = y_max * 0.95,
                 fill = total_color, alpha = 0.7) +
      annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.8, ymax = y_max * 0.85,
               fill = active_color) +
      annotate("rect", xmin = -0.5, xmax = 0, ymin = y_max * 0.7, ymax = y_max * 0.75,
               fill = expiring_color) +
      annotate("text", x = 0.1, y = y_max * 0.925,
               label = "Total", hjust = 0) +
      annotate("text", x = 0.1, y = y_max * 0.825,
               label = "Active", hjust = 0) +
      annotate("text", x = 0.1, y = y_max * 0.725,
               label = "Expiring", hjust = 0)
  })
  
  # Historical data reactive
  historical_raw_data <- reactive({
    con <- get_db_connection()
    if (is.null(con)) {
      return(NULL)
    }
    
    # Get archive data
    archive_query <- sprintf("
      SELECT facility, sitecode, inspdate, action
      FROM public.dblarv_insptrt_archive
      WHERE action = 'D'
      AND EXTRACT(YEAR FROM inspdate) BETWEEN %d AND %d
    ", input$hist_start_year, input$hist_end_year)
    
    archive_data <- dbGetQuery(con, archive_query)
    
    # Get current data  
    current_query <- sprintf("
      SELECT facility, sitecode, inspdate, action, airgrnd_plan
      FROM public.dblarv_insptrt_current
      WHERE (airgrnd_plan = 'D' OR action = 'D')
      AND EXTRACT(YEAR FROM inspdate) BETWEEN %d AND %d
    ", input$hist_start_year, input$hist_end_year)
    
    current_data <- dbGetQuery(con, current_query)
    
    # Get site size info from loc_breeding_sites
    sitecodes <- unique(c(archive_data$sitecode, current_data$sitecode))
    if (length(sitecodes) > 0) {
      sitecodes_str <- paste(sprintf("'%s'", sitecodes), collapse = ",")
      lbs_query <- sprintf("
        SELECT sitecode, acres, facility 
        FROM public.loc_breeding_sites 
        WHERE sitecode IN (%s)", sitecodes_str)
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
  
  # Process historical data based on user selections
  historical_processed_data <- reactive({
    # Get raw data
    data_list <- historical_raw_data()
    if (is.null(data_list)) return(data.frame())
    
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
    
    # Process based on count type
    if (input$hist_count_type == "treatments") {
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
    all_years <- seq(from = input$hist_start_year, to = input$hist_end_year)
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
  
  # Historical plot output
  output$historicalGraph <- renderPlot({
    data <- historical_processed_data()
    if (nrow(data) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No data available for the selected criteria", cex = 1.5)
      return()
    }
    
    if (input$hist_show_percentages) {
      # Show as percentages
      data <- data %>%
        group_by(year) %>%
        mutate(percentage = round(count / sum(count) * 100, 1)) %>%
        ungroup()
      
      p <- ggplot(data, aes(x = year, y = percentage, fill = facility)) +
        geom_col(position = "stack") +
        labs(
          title = "Drone Treatments by Facility (Percentage)",
          x = "Year",
          y = "Percentage (%)",
          fill = "Facility"
        ) +
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
        scale_x_continuous(breaks = seq(min(data$year), max(data$year), 1)) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    } else {
      # Show raw counts
      p <- ggplot(data, aes(x = year, y = count, fill = facility)) +
        geom_col(position = "stack") +
        labs(
          title = paste("Drone", ifelse(input$hist_count_type == "treatments", "Treatments", "Sites Treated"), "by Facility"),
          x = "Year", 
          y = ifelse(input$hist_count_type == "treatments", "Number of Treatments", "Number of Sites"),
          fill = "Facility"
        ) +
        scale_x_continuous(breaks = seq(min(data$year), max(data$year), 1)) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }
    
    print(p)
  })
  
  # Site average size graph output
  output$siteAvgSizeGraph <- renderPlot({
    data_list <- historical_raw_data()
    if (is.null(data_list)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No data available", cex = 1.5)
      return()
    }
    
    # Combine archive and current data
    archive_data <- data_list$archive %>% mutate(source = "Archive")
    current_data <- data_list$current %>% mutate(source = "Current")
    all_data <- bind_rows(archive_data, current_data)
    
    if (nrow(all_data) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No treatment data available", cex = 1.5)
      return()
    }
    
    # Join with site size data
    site_data <- all_data %>%
      inner_join(data_list$lbs, by = "sitecode") %>%
      mutate(year = year(inspdate)) %>%
      group_by(facility.x, year) %>%
      summarize(avg_acres = mean(acres, na.rm = TRUE), .groups = "drop") %>%
      rename(facility = facility.x)
    
    if (nrow(site_data) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No site size data available", cex = 1.5)
      return()
    }
    
    p <- ggplot(site_data, aes(x = year, y = avg_acres, color = facility)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(
        title = "Average Site Size Treated by Drone (Acres)",
        x = "Year",
        y = "Average Acres",
        color = "Facility"
      ) +
      scale_x_continuous(breaks = seq(min(site_data$year), max(site_data$year), 1)) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    print(p)
  })
  
  # Summary table output
  output$summaryTable <- renderTable({
    data <- historical_processed_data()
    if (nrow(data) == 0) {
      return(data.frame(Message = "No data available for the selected criteria"))
    }
    
    # Create summary by facility and overall
    summary_by_facility <- data %>%
      group_by(facility) %>%
      summarize(
        Total = sum(count),
        Average = round(mean(count), 1),
        Min = min(count),
        Max = max(count),
        .groups = "drop"
      )
    
    # Add overall totals
    overall <- data %>%
      group_by(year) %>%
      summarize(total_year = sum(count), .groups = "drop") %>%
      summarize(
        facility = "OVERALL",
        Total = sum(total_year),
        Average = round(mean(total_year), 1),
        Min = min(total_year),
        Max = max(total_year)
      )
    
    # Combine
    result <- bind_rows(summary_by_facility, overall)
    
    # Rename for display
    result <- result %>%
      rename(
        Facility = facility,
        `Total Count` = Total,
        `Avg per Year` = Average,
        `Min Year` = Min,
        `Max Year` = Max
      )
    
    return(result)
  }, striped = TRUE, hover = TRUE, spacing = "m")
  
  # Site extremes table output  
  output$siteExtremesTable <- renderTable({
    data_list <- historical_raw_data()
    if (is.null(data_list)) {
      return(data.frame(Message = "No data available"))
    }
    
    # Combine archive and current data
    archive_data <- data_list$archive %>% mutate(source = "Archive")
    current_data <- data_list$current %>% mutate(source = "Current")
    all_data <- bind_rows(archive_data, current_data)
    
    if (nrow(all_data) == 0) {
      return(data.frame(Message = "No treatment data available"))
    }
    
    # Join with site size data
    site_data <- all_data %>%
      inner_join(data_list$lbs, by = "sitecode") %>%
      select(sitecode, acres, facility.x) %>%
      distinct() %>%
      rename(facility = facility.x)
    
    if (nrow(site_data) == 0) {
      return(data.frame(Message = "No site size data available"))
    }
    
    # Find extremes
    largest_sites <- site_data %>%
      arrange(desc(acres)) %>%
      head(5) %>%
      mutate(Category = "Largest Sites") %>%
      select(Category, sitecode, acres, facility)
    
    smallest_sites <- site_data %>%
      arrange(acres) %>%
      head(5) %>%
      mutate(Category = "Smallest Sites") %>%
      select(Category, sitecode, acres, facility)
    
    # Combine
    result <- bind_rows(largest_sites, smallest_sites) %>%
      rename(
        Type = Category,
        `Site Code` = sitecode,
        `Acres` = acres,
        `Facility` = facility
      )
    
    return(result)
  }, striped = TRUE, hover = TRUE, spacing = "m")
}

# Run the application
shinyApp(ui = ui, server = server)

