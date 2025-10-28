# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(scales)
  library(leaflet) # For interactive maps
  library(sf) # For handling spatial data
  library(stringr) # For string manipulation
  library(plotly)
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

# Define UI for the application
ui <- fluidPage(
  # Application title
  titlePanel("SUCO Analysis Dashboard"),

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
                               "Foreman" = "foreman",
                               "MMCD (All)" = "mmcd_all"),
                   selected = "facility"),
      
      # Date range selection - default to current year
      dateRangeInput("date_range", "Select Date Range:",
                     start = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                     end = Sys.Date(),
                     format = "yyyy-mm-dd"),
      
      # Filter by facility (multi-select)
      selectizeInput("facility_filter", "Filter by Facility:",
                  choices = c("All"),  # Will be populated from get_facility_lookup()
                  selected = "All", multiple = TRUE),
      
      # Filter by foreman (multi-select, populated dynamically)
      selectizeInput("foreman_filter", "Filter by FOS:",
                  choices = c("All"),
                  selected = "All", multiple = TRUE),
      
      # Add species filter to sidebarPanel
      selectInput("species_filter", "Filter by Species:", choices = c("All"), selected = "All"),
      # Graph type selector
      selectInput("graph_type", "Graph Type:",
                  choices = c("Bar" = "bar", "Line" = "line", "Point" = "point", "Area" = "area"),
                  selected = "bar"),
      # Map Controls
      conditionalPanel(
        condition = "input.tabset == 'Map'",
        hr(),
        h4("Map Options"),
        
        # Base map selection
        selectInput("basemap", "Base Map:",
                    choices = c("OpenStreetMap" = "osm",
                                "Carto Light" = "carto",
                                "Esri Satellite" = "satellite"),
                    selected = "carto"),
        
        # Marker size adjustment
        sliderInput("marker_size", "Marker Size Multiplier:",
                    min = 1, max = 5, value = 3, step = 0.5)
      )
    ),
    
    # Main panel for displaying the graphs
    mainPanel(
      tabsetPanel(id = "tabset",
                  tabPanel("Graph", value = "Graph", plotOutput("trend_plot", height = "500px")),
                  tabPanel("Map", value = "Map", leafletOutput("map", height = "600px")),
                  tabPanel("Summary Table", value = "Table", dataTableOutput("summary_table")),
                  tabPanel("Top Locations", value = "TopLoc", plotlyOutput("location_plotly", height = "500px"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize facility choices from db_helpers
  observe({
    facilities <- get_facility_lookup()
    # Create named vector with full names as labels and short names as values
    facility_choices <- c("All" = "All")
    facility_choices <- c(
      facility_choices,
      setNames(facilities$short_name, facilities$full_name)
    )
    updateSelectizeInput(session, "facility_filter", choices = facility_choices)
  })
  
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
  
  # Add a mapping from spp code to readable species name
  species_code_map <- c(
    '26' = 'Cx. tarsalis',
    '32' = 'Cx. erraticus',
    '33' = 'Cx. pipiens',
    '34' = 'Cx. restuans',
    '35' = 'Cx. salinarius',
    '36' = 'Cx. tarsalis',
    '37' = 'Cx. territans',
    '43' = 'Or. signifera',
    '44' = 'Ps. ciliata',
    '45' = 'Ps. columbiae',
    '46' = 'Ps. ferox',
    '47' = 'Ps. horrida',
    '48' = 'Ur. sapphirina',
    '49' = 'sp49_smith',
    '50' = 'sp50_hende'
  )
  
  # Fetch and join SUCO and species data
  suco_data <- reactive({
    con <- get_db_connection()
    if (is.null(con)) {
      return(data.frame())  # Return empty data frame if connection fails
    }
    
    # Date range for query
    start_date <- format(input$date_range[1], "%Y-%m-%d")
    end_date <- format(input$date_range[2], "%Y-%m-%d")
    
    # Query current data for SUCOs (survtype = 7) with harborage lookup for current foreman assignments
    current_query <- sprintf("
SELECT
s.id, s.ainspecnum, s.facility, 
CASE 
  WHEN h.foreman IS NOT NULL AND h.foreman != '' THEN h.foreman 
  WHEN s.foreman IS NOT NULL AND s.foreman != '' THEN s.foreman
  ELSE NULL
END as foreman,
s.inspdate, s.sitecode,
s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
s.x, s.y, ST_AsText(s.geometry) as geometry_text,
COALESCE(h.facility, '') as harborage_facility
FROM public.dbadult_insp_current s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND s.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR s.inspdate <= h.enddate)
WHERE s.survtype = '7'
AND s.inspdate BETWEEN '%s' AND '%s'
", start_date, end_date)
    
    # Query archive data for SUCOs with harborage lookup for current foreman assignments
    archive_query <- sprintf("
SELECT
s.id, s.ainspecnum, s.facility,
CASE 
  WHEN h.foreman IS NOT NULL AND h.foreman != '' THEN h.foreman 
  WHEN s.foreman IS NOT NULL AND s.foreman != '' THEN s.foreman
  ELSE NULL
END as foreman,
s.inspdate, s.sitecode,
s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
s.x, s.y, ST_AsText(s.geometry) as geometry_text,
COALESCE(h.facility, '') as harborage_facility
FROM public.dbadult_insp_archive s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND s.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR s.inspdate <= h.enddate)
WHERE s.survtype = '7'
AND s.inspdate BETWEEN '%s' AND '%s'
", start_date, end_date)
    
    # Execute queries
    current_data <- dbGetQuery(con, current_query)
    archive_data <- dbGetQuery(con, archive_query)
    
    # Query species tables (current and archive)
    species_current_query <- sprintf("
SELECT ainspecnum, spp, cnt
FROM public.dbadult_species_current
WHERE ainspecnum IS NOT NULL
")
    species_archive_query <- sprintf("
SELECT ainspecnum, spp, cnt
FROM public.dbadult_species_archive
WHERE ainspecnum IS NOT NULL
")
    species_current <- dbGetQuery(con, species_current_query)
    species_archive <- dbGetQuery(con, species_archive_query)

    # Query species lookup table using centralized function
    species_lookup <- get_species_lookup()

    # Close connection
    dbDisconnect(con)

    # Combine current and archive data
    all_data <- bind_rows(
      mutate(current_data, source = "Current"),
      mutate(archive_data, source = "Archive")
    ) %>%
      mutate(
        inspdate = as.Date(inspdate),
        year = year(inspdate),
        month = month(inspdate),
        week_start = floor_date(inspdate, "week", week_start = 1),
        month_label = format(inspdate, "%b %Y"),
        location = ifelse(!is.na(park_name) & park_name != "", park_name,
                          ifelse(!is.na(address1) & address1 != "", address1, sitecode)),
        # Use harborage facility when harborage foreman is used
        # This ensures foreman colors match the correct facility
        facility = ifelse(!is.na(harborage_facility) & harborage_facility != "", 
                         harborage_facility, facility)
      )
    
    # Debug: Check foreman data after SQL queries
    # Combine species data
    all_species <- bind_rows(species_current, species_archive)

    # Join SUCO data with species data and lookup for names
    joined_data <- all_data %>%
      left_join(all_species, by = "ainspecnum") %>%
      left_join(species_lookup, by = c("spp" = "sppcode")) %>%
      mutate(
        species_name = dplyr::case_when(
          !is.na(genus) & !is.na(species) ~ paste(genus, species),
          !is.na(spp) ~ as.character(spp),
          TRUE ~ NA_character_
        ),
        # Ensure foreman values are consistent strings
        foreman = trimws(as.character(foreman))
      )

    # DEBUG: Check final data before returning
    
    return(joined_data)
  })
  
  # Update species filter choices based on available data (use species_name)
  observe({
    data <- suco_data()
    
    # Get unique species
    spp_choices <- sort(unique(na.omit(data$species_name)))
    spp_choices <- c("All", spp_choices)
    
    # Update select input
    updateSelectInput(session, "species_filter", choices = spp_choices)
  })
  
  # Update foreman filter choices based on available data (multi-select aware)
  observe({
    data <- suco_data()
    foremen_lookup <- get_foremen_lookup()
    
    # Filter by facility if not 'All' and not empty
    if (!is.null(input$facility_filter) && !("All" %in% input$facility_filter)) {
      data <- data %>% filter(facility %in% input$facility_filter)
    }
    
    # Get unique foremen numbers from data
    foremen_nums <- sort(unique(na.omit(data$foreman)))
    
    # Create mapping from emp_num to shortname for display with robust matching
    foremen_names <- sapply(foremen_nums, function(num) {
      # Ensure both sides are strings and trimmed
      num_str <- trimws(as.character(num))
      matches <- which(trimws(as.character(foremen_lookup$emp_num)) == num_str)
      if(length(matches) > 0) {
        foremen_lookup$shortname[matches[1]]
      } else {
        paste0("FOS #", num_str)  # fallback to formatted number
      }
    })
    
    # Create choices with names as labels and numbers as values
    foremen_choices <- setNames(
      c("All", foremen_nums),
      c("All", foremen_names)
    )
    
    updateSelectInput(session, "foreman_filter", choices = foremen_choices)
  })

  # Filter data based on user selections, now including multi-select for facility/foreman
  filtered_data <- reactive({
    data <- suco_data()
    # Filter by facility if not 'All' and not empty
    if (!is.null(input$facility_filter) && !("All" %in% input$facility_filter)) {
      data <- data %>% filter(facility %in% input$facility_filter)
    }
    # Filter by foreman if not 'All' and not empty
    if (!is.null(input$foreman_filter) && !("All" %in% input$foreman_filter)) {
      data <- data %>% filter(foreman %in% input$foreman_filter)
    }
    # Filter by species if selected
    if (input$species_filter != "All") {
      data <- data %>% filter(species_name == input$species_filter)
    }
    return(data)
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
    
    if (group_col == "mmcd_all") {
      # Aggregate for the whole MMCD (no grouping by facility or foreman)
      result <- data %>%
        group_by(time_group) %>%
        summarize(
          count = n(),
          total_fieldcount = sum(fieldcount, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(time_group)
      # Add a dummy column for plotting
      result$mmcd_all <- "MMCD (All)"
    } else {
      # Group and summarize data
      result <- data %>%
        group_by(time_group, !!sym(group_col)) %>%
        summarize(
          count = n(),
          total_fieldcount = sum(fieldcount, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(time_group)
    }
    
    return(result)
  })
  
  # Generate trend plot
  output$trend_plot <- renderPlot({
    data <- aggregated_data()
    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No SUCO data available with the selected filters", size = 6) +
          theme_void()
      )
    }
    if (input$time_interval == "week") {
      data$time_label <- format(data$time_group, "%m/%d")
    } else {
      data$time_label <- format(data$time_group, "%b %Y")
    }
    group_col <- input$group_by
    if (group_col == "mmcd_all") {
      group_col <- "mmcd_all"
    }
    
    # Get facility and foreman lookups for labels
    facilities <- get_facility_lookup()
    foremen_lookup <- get_foremen_lookup()
    
    # Create facility name mapping
    facility_names <- setNames(facilities$full_name, facilities$short_name)
    
    # Create foreman name mapping
    foreman_names <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
    
    title_interval <- ifelse(input$time_interval == "week", "Weekly", "Monthly")
    title_group <- ifelse(input$group_by == "facility", "Facility", "Foreman")
    
    # Get facility and foreman lookups for display names in subtitle
    facility_names <- setNames(facilities$full_name, facilities$short_name)
    foreman_names <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
    
    # Use full names in filter text
    facility_text <- if ("All" %in% input$facility_filter) {
      "All Facilities"
    } else {
      display_names <- sapply(input$facility_filter, function(f) facility_names[f] %||% f)
      paste("Facility:", paste(display_names, collapse=", "))
    }
    
    foreman_text <- if ("All" %in% input$foreman_filter) {
      "All Foremen"
    } else {
      display_names <- sapply(input$foreman_filter, function(f) foreman_names[f] %||% f)
      paste("Foreman:", paste(display_names, collapse=", "))
    }
    
    # Get color scales from db_helpers based on grouping
    custom_colors <- if(group_col == "facility") {
      get_facility_base_colors()
    } else if(group_col == "foreman") {
      # Get the foreman colors from db_helpers
      foreman_colors <- get_foreman_colors()
      foremen_lookup <- get_foremen_lookup()
      
      # Debug info
      
      # Create mapping from foreman NUMBER to facility-based colors
      # Step 1: Get the shortname for each foreman number in the data
      # Step 2: Get the facility-based color for that shortname
      
      foremen_in_data <- unique(na.omit(data[[group_col]]))
      emp_colors <- character(0)
      
      for (foreman_num in foremen_in_data) {
        foreman_num_str <- trimws(as.character(foreman_num))
        
        # Find the shortname for this foreman number
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
        
        if(length(matches) > 0) {
          shortname <- foremen_lookup$shortname[matches[1]]
          facility <- foremen_lookup$facility[matches[1]]
          
          # Get the facility-based color for this shortname
          if(shortname %in% names(foreman_colors)) {
            emp_colors[foreman_num_str] <- foreman_colors[shortname]
          }
        }
      }
      
      # Remove any NA colors
      emp_colors <- emp_colors[!is.na(emp_colors)]
      
      # Return the mapped colors
      emp_colors
    } else {
      NULL
    }
    
    # Create the plot using the original group_col values for color mapping
    p <- ggplot(data, aes(x = time_group, y = count, 
                         color = !!sym(group_col), 
                         fill = !!sym(group_col), 
                         group = !!sym(group_col)))
    
    
    # Add color scales based on grouping
    if(!is.null(custom_colors)) {
      # Add color scales with labels for the legend
      if(group_col == "facility") {
        # For facilities, map short names to full names in legend
        p <- p + scale_color_manual(
          values = custom_colors,
          labels = function(x) sapply(x, function(v) facility_names[v] %||% v),
          drop = FALSE
        ) + scale_fill_manual(
          values = custom_colors,
          labels = function(x) sapply(x, function(v) facility_names[v] %||% v),
          drop = FALSE
        )
      } else {
        # For foremen, map employee numbers to names in legend
        p <- p + scale_color_manual(
          values = custom_colors,
          labels = function(x) sapply(x, function(v) foreman_names[v] %||% v),
          drop = FALSE
        ) + scale_fill_manual(
          values = custom_colors,
          labels = function(x) sapply(x, function(v) foreman_names[v] %||% v),
          drop = FALSE
        )
      }
    } else {
      p <- p + scale_color_discrete() + scale_fill_discrete()
    }
    if (input$graph_type == "bar") {
      p <- p + geom_bar(stat = "identity", position = "dodge")
    } else if (input$graph_type == "line") {
      p <- p + geom_line(size = 1.2)
    } else if (input$graph_type == "point") {
      p <- p + geom_point(size = 3)
    } else if (input$graph_type == "area") {
      p <- p + geom_area(position = "stack", alpha = 0.6)
    }
    p <- p + labs(
      title = paste(title_interval, "SUCO Counts by", ifelse(input$group_by == "mmcd_all", "MMCD (All)", title_group)),
      subtitle = paste(facility_text, "-", foreman_text),
      x = ifelse(input$time_interval == "week", "Week Starting", "Month"),
      y = "Number of SUCOs",
      fill = ifelse(input$group_by == "mmcd_all", "MMCD (All)", title_group),
      color = ifelse(input$group_by == "mmcd_all", "MMCD (All)", title_group)
    ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")
      )
    if (input$time_interval == "week") {
      p <- p + scale_x_date(
        date_breaks = "2 weeks",
        date_labels = "%m/%d",
        limits = c(min(data$time_group), max(data$time_group))
      )
    } else {
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
    
    # Get marker size multiplier
    size_multiplier <- input$marker_size
    
    # Get facility and foremen lookups for display names
    facilities <- get_facility_lookup()
    
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
      # Get facility colors and lookup from db_helpers
      facility_colors <- get_facility_base_colors()
      facilities <- get_facility_lookup()
      foremen_lookup <- get_foremen_lookup()  # Add foremen lookup for popups
      foremen_lookup$emp_num <- as.character(foremen_lookup$emp_num)  # Ensure string format
      
      # Create popup text beforehand to avoid scope issues
      data <- data %>%
        mutate(
          popup_text = {
            # Get proper foreman names with robust matching
            foreman_names <- sapply(foreman, function(f) {
              if (!is.na(f) && f != "" && !is.null(f)) {
                # Ensure both foreman and emp_num are strings for comparison
                foreman_str <- trimws(as.character(f))
                
                # Find matching foreman in lookup table
                matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_str)
                
                if(length(matches) > 0) {
                  foremen_lookup$shortname[matches[1]]
                } else {
                  # Fallback: show the raw foreman ID if no match found
                  paste0("FOS #", foreman_str)
                }
              } else {
                "No FOS assigned"
              }
            })
            
            facility_names_vec <- sapply(facility, function(f) {
              fname <- facility_names[f]
              if(length(fname) > 0) fname[1] else f
            })
            
            paste0("<b>Date:</b> ", inspdate, "<br>",
                   "<b>Facility:</b> ", facility_names_vec, "<br>",
                   "<b>FOS:</b> ", foreman_names, "<br>",
                   "<b>Location:</b> ", location, "<br>",
                   "<b>Field Count:</b> ", fieldcount)
          }
        )
      facility_names <- setNames(facilities$full_name, facilities$short_name)
      
      # Create color palette function
      pal <- colorFactor(
        palette = facility_colors,
        domain = names(facility_colors))
      
      # Create a named vector for legend labels
      legend_labels <- sapply(names(facility_colors), function(code) facility_names[code] %||% code)
      
      # Create map with facility coloring
      leaflet(data) %>%
        addProviderTiles(basemap) %>%
        fitBounds(
          lng1 = min(st_coordinates(data)[,1]),
          lat1 = min(st_coordinates(data)[,2]),
          lng2 = max(st_coordinates(data)[,1]),
          lat2 = max(st_coordinates(data)[,2])
        ) %>%
        addCircleMarkers(
          radius = ~pmin(15, (3 * size_multiplier)),
          color = "black",
          weight = 1.5,
          fillColor = ~pal(facility),
          fillOpacity = 0.8,
          popup = ~popup_text
        ) %>%
        addLegend(
          position = "bottomright",
          title = "Facility",
          colors = facility_colors,
          labels = legend_labels,
          opacity = 0.8
        )
    } else if (input$group_by == "foreman") {
      # Filter out records with NA foreman before processing
      data <- data %>% filter(!is.na(foreman) & foreman != "")
      
      # Check if we have any data left after filtering
      if (nrow(data) == 0) {
        return(
          leaflet() %>%
            addProviderTiles(basemap) %>%
            setView(lng = -93.2, lat = 45.0, zoom = 9) %>%
            addControl(html = "<div style='background-color: white; padding: 10px;'><h4>No SUCO locations with valid FOS data available</h4></div>",
                       position = "topleft")
        )
      }
      
      # Get both colors and lookup exactly as documented
      foreman_colors <- get_foreman_colors()
      foremen_lookup <- get_foremen_lookup()
      
      # Ensure employee numbers are consistently formatted
      # foremen_lookup$emp_num is already properly formatted
      
      # Create mapping from foreman NUMBER to facility-based colors (same logic as plot)
      foremen_in_data <- unique(na.omit(data$foreman))
      emp_colors <- character(0)
      
      for (foreman_num in foremen_in_data) {
        foreman_num_str <- trimws(as.character(foreman_num))
        
        # Find the shortname for this foreman number
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_num_str)
        
        if(length(matches) > 0) {
          shortname <- foremen_lookup$shortname[matches[1]]
          facility <- foremen_lookup$facility[matches[1]]
          
          
          # Get the facility-based color for this shortname
          if(shortname %in% names(foreman_colors)) {
            emp_colors[foreman_num_str] <- foreman_colors[shortname]
          }
        }
      }
      
      # Remove any NA colors
      emp_colors <- emp_colors[!is.na(emp_colors)]
      
      # Create ORDERED colors to ensure legend and map match exactly
      # Order by facility, then by foreman within facility (same as legend)
      ordered_foremen <- foremen_lookup[order(foremen_lookup$facility, foremen_lookup$shortname), ]
      ordered_emp_colors <- character(0)
      ordered_emp_numbers <- character(0)
      
      for (i in 1:nrow(ordered_foremen)) {
        emp_num <- trimws(as.character(ordered_foremen$emp_num[i]))
        if (emp_num %in% names(emp_colors)) {
          ordered_emp_colors <- c(ordered_emp_colors, emp_colors[emp_num])
          ordered_emp_numbers <- c(ordered_emp_numbers, emp_num)
        }
      }
      names(ordered_emp_colors) <- ordered_emp_numbers
      
      # Create color palette function for leaflet using ORDERED colors
      pal <- colorFactor(
        palette = ordered_emp_colors,
        domain = names(ordered_emp_colors),
        ordered = TRUE  # Maintain order
      )
      
      # Create popup text beforehand for foreman map too
      data <- data %>%
        mutate(
          popup_text_foreman = {
            # Get proper foreman names with robust matching
            foreman_names <- sapply(foreman, function(f) {
              if (!is.na(f) && f != "" && !is.null(f)) {
                # Ensure both foreman and emp_num are strings for comparison
                foreman_str <- trimws(as.character(f))
                
                # Find matching foreman in lookup table
                matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_str)
                
                if(length(matches) > 0) {
                  foremen_lookup$shortname[matches[1]]
                } else {
                  # Fallback: show the raw foreman ID if no match found
                  paste0("FOS #", foreman_str)
                }
              } else {
                "No FOS assigned"
              }
            })
            
            facility_names_vec <- sapply(facility, function(f) {
              fname <- facilities$full_name[facilities$short_name == f]
              if(length(fname) > 0) fname[1] else f
            })
            
            paste0("<b>Date:</b> ", inspdate, "<br>",
                   "<b>Facility:</b> ", facility_names_vec, "<br>",
                   "<b>FOS:</b> ", foreman_names, "<br>",
                   "<b>Location:</b> ", location, "<br>",
                   "<b>Field Count:</b> ", fieldcount)
          }
        )
      
      # Create map with foreman coloring
      leaflet(data) %>%
        addProviderTiles(basemap) %>%
        fitBounds(
          lng1 = min(st_coordinates(data)[,1]),
          lat1 = min(st_coordinates(data)[,2]),
          lng2 = max(st_coordinates(data)[,1]),
          lat2 = max(st_coordinates(data)[,2])
        ) %>%
        addCircleMarkers(
          radius = ~pmin(15, (3 * size_multiplier)),
          color = "black",
          weight = 1.5,
          fillColor = ~pal(foreman),
          fillOpacity = 0.8,
          popup = ~popup_text_foreman
        ) %>%
        addLegend(
          position = "bottomright",
          title = "FOS",
          colors = ordered_emp_colors,  # Use same ordered colors as map palette
          labels = {
            # Use same ordering as the map palette
            ordered_labels <- character(0)
            for (i in 1:length(ordered_emp_numbers)) {
              emp_num <- ordered_emp_numbers[i]
              foreman_info <- foremen_lookup[trimws(as.character(foremen_lookup$emp_num)) == emp_num, ]
              if(nrow(foreman_info) > 0) {
                ordered_labels <- c(ordered_labels, foreman_info$shortname[1])
              }
            }
            ordered_labels
          },
          opacity = 0.8
        )
    } else {
      # For MMCD (All) case, use a single color
      leaflet(data) %>%
        addProviderTiles(basemap) %>%
        fitBounds(
          lng1 = min(st_coordinates(data)[,1]),
          lat1 = min(st_coordinates(data)[,2]),
          lng2 = max(st_coordinates(data)[,1]),
          lat2 = max(st_coordinates(data)[,2])
        ) %>%
        addCircleMarkers(
          radius = ~pmin(15, (3 * size_multiplier)),
          color = "black",
          weight = 1.5,
          fillColor = "#1f77b4", # Standard blue color
          fillOpacity = 0.8,
          popup = ~paste0("<b>Date:</b> ", inspdate, "<br>",
                          "<b>Facility:</b> ", facility, "<br>",
                          "<b>Foreman:</b> ", foreman, "<br>",
                          "<b>Location:</b> ", location, "<br>",
                          "<b>Field Count:</b> ", fieldcount)
        ) %>%
        addLegend(
          position = "bottomright",
          title = "MMCD",
          colors = "#1f77b4",
          labels = "All",
          opacity = 0.8
        )
    }
  })
  
  # Generate summary table
  output$summary_table <- renderDataTable({
    data <- filtered_data()
    group_col <- input$group_by
    
    if (group_col == "mmcd_all") {
      # Summarize for the whole MMCD (no grouping)
      summary_data <- data %>%
        summarize(
          Total_SUCOs = n(),
          Total_Locations = n_distinct(sitecode),
          Total_Species_Count = sum(cnt, na.rm = TRUE),
          First_SUCO = min(inspdate),
          Last_SUCO = max(inspdate)
        )
      rownames(summary_data) <- "MMCD (All)"
    } else if (group_col == "facility") {
      # Get all facilities from lookup
      facilities <- get_facility_lookup()
      
      # Create base data frame with all facilities
      all_facilities <- data.frame(
        facility = facilities$short_name,
        full_name = facilities$full_name
      )
      
      # Summarize actual data
      summary_stats <- data %>%
        group_by(facility) %>%
        summarize(
          Total_SUCOs = n(),
          Total_Locations = n_distinct(sitecode),
          Total_Species_Count = sum(cnt, na.rm = TRUE),
          First_SUCO = min(inspdate),
          Last_SUCO = max(inspdate)
        )
      
      # Join with all facilities to include zeros
      summary_data <- all_facilities %>%
        left_join(summary_stats, by = "facility") %>%
        mutate(
          Total_SUCOs = replace_na(Total_SUCOs, 0),
          Total_Locations = replace_na(Total_Locations, 0),
          Total_Species_Count = replace_na(Total_Species_Count, 0),
          First_SUCO = as.Date(First_SUCO),
          Last_SUCO = as.Date(Last_SUCO)
        ) %>%
        arrange(desc(Total_SUCOs), facility)
      
      # Rename columns
      colnames(summary_data)[1:2] <- c("Facility", "Facility_Name")
      
    } else {  # foreman grouping
      # Get foreman lookup
      foremen_lookup <- get_foremen_lookup()
      
      # Note: both foremen_lookup$emp_num and data$foreman are already properly formatted
      
      # Create base data frame with all foremen
      all_foremen <- data.frame(
        foreman = foremen_lookup$emp_num,
        shortname = foremen_lookup$shortname,
        facility = foremen_lookup$facility
      )
      
      # Summarize actual data
      summary_stats <- data %>%
        group_by(foreman) %>%
        summarize(
          Total_SUCOs = n(),
          Total_Locations = n_distinct(sitecode),
          Total_Species_Count = sum(cnt, na.rm = TRUE),
          First_SUCO = min(inspdate),
          Last_SUCO = max(inspdate)
        )
      
      # Join with all foremen to include zeros
      summary_data <- all_foremen %>%
        left_join(summary_stats, by = c("foreman")) %>%
        mutate(
          Total_SUCOs = replace_na(Total_SUCOs, 0),
          Total_Locations = replace_na(Total_Locations, 0),
          Total_Species_Count = replace_na(Total_Species_Count, 0),
          First_SUCO = as.Date(First_SUCO),
          Last_SUCO = as.Date(Last_SUCO)
        ) %>%
        arrange(desc(Total_SUCOs), facility, shortname)
      
      # Rename columns
      colnames(summary_data)[1:3] <- c("Foreman", "Name", "Facility")
    }
    return(summary_data)
  }, options = list(pageLength = 15, 
                   searching = TRUE,
                   columnDefs = list(list(
                     targets = c("First_SUCO", "Last_SUCO"),
                     render = JS("function(data, type, row) {
                       return data ? data : 'N/A';
                     }")
                   ))))
  
  # Generate location plot (top locations with most SUCOs)
  output$location_plotly <- plotly::renderPlotly({
    data <- filtered_data()
    # Always group by location for top locations
    top_locations <- data %>%
      group_by(location) %>%
      summarize(visits = n(), .groups = "drop") %>%
      filter(!is.na(location)) %>%  # Remove NA locations
      arrange(desc(visits)) %>%
      head(15)
    if (nrow(top_locations) == 0) {
      return(plotly::ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No SUCO data available with the selected filters", size = 6) + theme_void()))
    }
    p <- ggplot(top_locations, aes(x = reorder(location, visits), y = visits, text = location)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = visits), hjust = 1.3, color = "black") +  # Move number further right
      coord_flip() +
      labs(title = "Top SUCO Locations", x = "Location", y = "Number of Visits") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16), axis.title = element_text(face = "bold"))
    p <- plotly::ggplotly(p, tooltip = c("x", "y", "text"), source = "location_plotly")
    # Register the click event
    plotly::event_register(p, 'plotly_click')
    p
  })
  
  # React to plotly click and update map and tab
  observe({
    click <- plotly::event_data("plotly_click", source = "location_plotly")
    if (!is.null(click)) {
      idx <- click$pointNumber + 1  # R is 1-based
      data <- filtered_data()
      top_locations <- data %>%
        group_by(location) %>%
        summarize(visits = n(), .groups = "drop") %>%
        arrange(desc(visits)) %>%
        head(15)
      if (idx > 0 && idx <= nrow(top_locations)) {
        loc <- top_locations$location[idx]
        # Use spatial_data() for accurate coordinates (same as map)
        spatial <- spatial_data()
        # Find the first point in spatial_data with this location
        if (nrow(spatial) > 0 && loc %in% spatial$location) {
          point <- spatial[spatial$location == loc, ][1, ]
          coords <- sf::st_coordinates(point)
          lng <- coords[1]
          lat <- coords[2]
          if (!is.na(lng) && !is.na(lat)) {
            updateTabsetPanel(session, "tabset", selected = "Map")
            leafletProxy("map") %>%
              setView(lng = lng, lat = lat, zoom = 15) %>%
              addCircleMarkers(lng = lng, lat = lat, radius = 15, color = "red", fill = TRUE, fillOpacity = 0.7, layerId = "highlighted_location")
          }
        }
      }
    } else {
      leafletProxy("map") %>% clearGroup("highlighted_location")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)