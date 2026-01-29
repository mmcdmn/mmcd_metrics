# SUCO History Analysis App
# Simplified version - merged tabs with date range filter

# Load shared libraries and utilities
source("../../shared/app_libraries.R")
source("../../shared/server_utilities.R")
source("../../shared/db_helpers.R")

# Source external function files
source("data_functions.R")
source("display_functions.R")
source("ui_helpers.R")

# Set application name for AWS RDS monitoring
set_app_name("suco_history")

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- fluidPage(
  # Use universal CSS from db_helpers for consistent text sizing
  get_universal_text_css(base_increase = 8),
  
  titlePanel("SUCO Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      # Refresh button at top
      actionButton("refresh", "Refresh Data", 
                   icon = icon("refresh"),
                   class = "btn-primary btn-lg",
                   style = "width: 100%;"),
      p(style = "text-align: center; margin-top: 10px; font-size: 0.9em; color: #666;",
        "Click refresh to get data for selected filters"),
      
      hr(),
      
      # Date range selection
      h4("Date Selection"),
      dateRangeInput("date_range", "Date Range:",
                     start = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                     end = Sys.Date(),
                     format = "yyyy-mm-dd"),
      hr(),
      
      # Group by selection
      h4("Analysis Options"),
      radioButtons("group_by", "Group By:",
                   choices = c("MMCD (All)" = "mmcd_all",
                               "Facility" = "facility", 
                               "FOS" = "foreman",
                               "Species" = "species_name"),
                   selected = "mmcd_all"),
      
      hr(),
      
      # Filters section
      h4("Filters"),
      
      selectInput("zone_filter", "Zone:",
                  choices = c("P1 + P2" = "all", "P1 Only" = "1", "P2 Only" = "2"),
                  selected = "all"),
      
      selectInput("facility_filter", "Facility:",
                  choices = NULL),
      
      selectizeInput("foreman_filter", "FOS area:",
                     choices = NULL, multiple = TRUE),
      
      selectInput("species_filter", "Species:", 
                  choices = c("All"), selected = "All"),
      
      hr(),
      
      # Display Options
      h4("Display Options"),
      
      selectInput("color_theme", "Color Theme:",
                  choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
                  selected = "MMCD"),
      
      selectInput("graph_type", "Graph Type:",
                  choices = c("Stacked Bar" = "stacked_bar",
                              "Bar" = "bar", 
                              "Line" = "line", 
                              "Point" = "point", 
                              "Area" = "area"),
                  selected = "stacked_bar"),
      
      # Map controls (conditional)
      conditionalPanel(
        condition = "input.tabs == 'map'",
        selectInput("basemap", "Base Map:",
                    choices = c("OpenStreetMap" = "osm",
                                "Carto Light" = "carto",
                                "Esri Satellite" = "satellite"),
                    selected = "carto")
      ),
      
      # Top locations mode (conditional)
      conditionalPanel(
        condition = "input.tabs == 'top_locations'",
        selectInput("top_locations_mode", "Top Locations Mode:",
                    choices = c("Most Visited" = "visits", 
                                "Most Species" = "species"),
                    selected = "visits")
      )
    ),
    
    # Main panel with single tabset (merged Current/Archive)
    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("Graph", value = "graph",
                 br(),
                 plotOutput("trend_plot", height = "600px")
        ),
        tabPanel("Map", value = "map",
                 br(),
                 leafletOutput("suco_map", height = "600px")
        ),
        tabPanel("Detailed Samples", value = "detailed",
                 br(),
                 fluidRow(
                   column(10, h4("Detailed Samples")),
                   column(2, downloadButton("download_detailed", "Download CSV", 
                                            class = "btn-success btn-sm", 
                                            style = "float: right;"))
                 ),
                 dataTableOutput("detailed_table")
        ),
        tabPanel("Top Locations", value = "top_locations",
                 br(),
                 plotlyOutput("location_plotly", height = "700px")
        )
      )
    )
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # Theme handling
  current_theme <- reactive({
    input$color_theme
  })
  
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
  
  # ===========================================================================
  # INITIALIZE FILTERS
  # ===========================================================================
  
  # Initialize facility choices
  observe({
    facility_choices <- get_facility_choices()
    updateSelectInput(session, "facility_filter", choices = facility_choices, selected = "all")
  })
  
  # Initialize foreman choices
  observe({
    foremen_lookup <- get_foremen_lookup()
    foremen_choices <- c("All" = "all")
    foremen_choices <- c(
      foremen_choices,
      setNames(foremen_lookup$emp_num, foremen_lookup$shortname)
    )
    updateSelectizeInput(session, "foreman_filter", choices = foremen_choices, selected = "all")
  })
  
  # Update FOS choices when facility changes
  observeEvent(input$facility_filter, {
    foremen_lookup <- get_foremen_lookup()
    
    if (input$facility_filter == "all") {
      foremen_choices <- c("All" = "all")
      foremen_choices <- c(
        foremen_choices,
        setNames(foremen_lookup$emp_num, foremen_lookup$shortname)
      )
    } else {
      filtered_foremen <- foremen_lookup[foremen_lookup$facility == input$facility_filter, ]
      foremen_choices <- c("All" = "all")
      if (nrow(filtered_foremen) > 0) {
        foremen_choices <- c(
          foremen_choices,
          setNames(filtered_foremen$emp_num, filtered_foremen$shortname)
        )
      }
    }
    
    updateSelectizeInput(session, "foreman_filter", choices = foremen_choices, selected = "all")
  })
  
  # Initialize species choices - uses 5 hardcoded species relevant to SUCOs
  observe({
    species_names <- get_available_species()
    if (length(species_names) > 0) {
      species_choices <- c("All", species_names)
      updateSelectInput(session, "species_filter", choices = species_choices, selected = "All")
    }
  })
  
  # ===========================================================================
  # REFRESH BUTTON PATTERN
  # ===========================================================================
  
  refresh_inputs <- eventReactive(input$refresh, {
    foreman_val <- isolate(input$foreman_filter)
    if (is.null(foreman_val) || length(foreman_val) == 0) {
      foreman_val <- "all"
    }
    
    list(
      group_by = isolate(input$group_by),
      date_range = isolate(input$date_range),
      zone_filter = isolate(input$zone_filter),
      facility_filter = isolate(input$facility_filter),
      foreman_filter = foreman_val,
      species_filter = isolate(input$species_filter),
      graph_type = isolate(input$graph_type),
      top_locations_mode = isolate(input$top_locations_mode),
      basemap = isolate(input$basemap)
    )
  })
  
  # ===========================================================================
  # DATA LOADING - Single source (queries both current + archive by date)
  # ===========================================================================
  
  suco_data <- eventReactive(input$refresh, {
    inputs <- refresh_inputs()
    withProgress(message = "Loading SUCO data...", value = 0.5, {
      # Request species details if grouping by species
      return_species_details <- inputs$group_by == "species_name"
      get_suco_data("all", inputs$date_range, return_species_details)
    })
  })
  
  filtered_data <- reactive({
    data <- suco_data()
    inputs <- refresh_inputs()
    filter_suco_data(data, inputs$facility_filter, inputs$foreman_filter, 
                     inputs$zone_filter, inputs$date_range, inputs$species_filter)
  })
  
  spatial_data <- reactive({
    data <- filtered_data()
    inputs <- refresh_inputs()
    # Pass grouping information for species-aware spatial data
    if (inputs$group_by == "species_name") {
      # For species grouping, we need the detailed species data
      create_spatial_data(data, inputs$species_filter, group_by = "species_name")
    } else {
      create_spatial_data(data, "All", group_by = inputs$group_by)  
    }
  })
  
  aggregated_data <- reactive({
    req(input$refresh)
    inputs <- refresh_inputs()
    data <- filtered_data()
    aggregate_suco_data(data, inputs$group_by, inputs$zone_filter)
  })
  
  # ===========================================================================
  # OUTPUT: TREND PLOT (with 72 SUCO/week target line)
  # ===========================================================================
  
  output$trend_plot <- renderPlot({
    req(input$refresh)
    inputs <- refresh_inputs()
    data <- aggregated_data()
    
    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No SUCO data available with the selected filters", size = 6) +
          theme_void()
      )
    }
    
    # Weekly labels
    data$time_label <- format(data$time_group, "%m/%d")
    group_col <- inputs$group_by
    
    # Get lookups
    facilities <- get_facility_lookup()
    foremen_lookup <- get_foremen_lookup()
    facility_names <- setNames(facilities$full_name, facilities$short_name)
    foreman_names <- setNames(foremen_lookup$shortname, foremen_lookup$emp_num)
    
    # Get colors
    custom_colors <- if(group_col == "facility") {
      get_facility_base_colors(theme = current_theme())
    } else if(group_col == "foreman") {
      map_foreman_emp_to_colors(theme = current_theme())
    } else if(group_col == "species_name") {
      # Ensure we're using species colors from shared db_helpers
      get_species_display_colors()
    } else {
      NULL
    }
    
    # Build plot
    p <- ggplot(data, aes(x = time_group, y = count, 
                          color = !!sym(group_col), 
                          fill = !!sym(group_col), 
                          group = !!sym(group_col)))
    
    # Add color scales
    if(!is.null(custom_colors)) {
      if (group_col == "facility") {
        labels_mapping <- function(x) sapply(x, function(code) facility_names[code] %||% code)
      } else if (group_col == "foreman") {
        labels_mapping <- function(x) sapply(x, function(num) foreman_names[as.character(num)] %||% paste0("FOS #", num))
      } else if (group_col == "species_name") {
        # For species, use identity mapping but ensure all species in data are covered
        species_in_data <- unique(data[[group_col]])
        species_in_data <- species_in_data[!is.na(species_in_data)]
        # Filter custom_colors to only include species actually in the data
        used_colors <- custom_colors[names(custom_colors) %in% species_in_data]
        labels_mapping <- function(x) x  # Use species names as-is
      } else {
        labels_mapping <- NULL
      }
      
      if (group_col == "species_name") {
        p <- p + scale_color_manual(values = used_colors, labels = labels_mapping, drop = FALSE, na.value = "gray50") + 
                 scale_fill_manual(values = used_colors, labels = labels_mapping, drop = FALSE, na.value = "gray50")
      } else {
        p <- p + scale_color_manual(values = custom_colors, labels = labels_mapping, drop = FALSE, na.value = "gray50") + 
                 scale_fill_manual(values = custom_colors, labels = labels_mapping, drop = FALSE, na.value = "gray50")
      }
    }
    
    # Add geometry based on graph type (auto-switch to bar for species)
    effective_graph_type <- if(group_col == "species_name") "bar" else inputs$graph_type
    
    if (effective_graph_type == "bar") {
      p <- p + geom_bar(stat = "identity", position = "dodge")
    } else if (effective_graph_type == "stacked_bar") {
      p <- p + geom_bar(stat = "identity", position = "stack")
    } else if (effective_graph_type == "line") {
      p <- p + geom_line(linewidth = 1.2)
    } else if (effective_graph_type == "point") {
      p <- p + geom_point(size = 3)
    } else if (effective_graph_type == "area") {
      p <- p + geom_area(position = "stack", alpha = 0.6)
    }
    
    # Add TARGET LINE at 72 SUCOs per week
    p <- p + geom_hline(yintercept = 72, color = "red", linetype = "dashed", linewidth = 1.2) +
      annotate("text", x = min(data$time_group), y = 72, 
               label = "Target: 72/week", hjust = 0, vjust = -0.5, 
               color = "red", fontface = "bold", size = 4)
    
    # Calculate and show average line
    weekly_totals <- data %>%
      group_by(time_group) %>%
      summarize(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      filter(total_count > 0)
    
    if (nrow(weekly_totals) > 0) {
      avg_sucos_per_week <- mean(weekly_totals$total_count, na.rm = TRUE)
      
      p <- p + geom_hline(yintercept = avg_sucos_per_week, color = "blue", 
                          linetype = "dotted", linewidth = 1) +
        annotate("text", x = min(data$time_group), y = avg_sucos_per_week,
                 label = sprintf("Avg: %.1f/week", avg_sucos_per_week), 
                 hjust = 0, vjust = 1.5, color = "blue", fontface = "italic", size = 3.5)
    }
    
    # Titles and theme
    title_group <- case_when(
      inputs$group_by == "facility" ~ "by Facility",
      inputs$group_by == "foreman" ~ "by FOS", 
      inputs$group_by == "species_name" ~ "by Species",
      inputs$group_by == "mmcd_all" ~ "(All MMCD)",
      TRUE ~ ""
    )
    
    p <- p +
      labs(
        title = paste("Weekly SUCO Inspections", title_group),
        subtitle = paste("Date range:", format(inputs$date_range[1], "%m/%d/%Y"), 
                        "to", format(inputs$date_range[2], "%m/%d/%Y")),
        x = "Week",
        y = "Number of SUCOs"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(size = 16, color = "gray40"),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        legend.position = if(group_col == "mmcd_all") "none" else "right",
        legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(size = 16)
      )
    
    return(p)
  })
  
  # ===========================================================================
  # OUTPUT: MAP (uses create_suco_map from display_functions.R)
  # ===========================================================================
  
  output$suco_map <- renderLeaflet({
    req(input$refresh)
    inputs <- refresh_inputs()
    data <- spatial_data()
    # Pass group_by information to the map function
    create_suco_map(data, inputs, data_source = "all", theme = current_theme(), group_by = inputs$group_by)
  })
  
  # ===========================================================================
  # OUTPUT: DETAILED TABLE
  # ===========================================================================
  
  output$detailed_table <- renderDataTable({
    req(input$refresh)
    inputs <- refresh_inputs()
    data <- filtered_data()
    create_detailed_samples_table(data, inputs$species_filter)
  }, options = list(pageLength = 25, 
                    searching = TRUE,
                    scrollX = TRUE,
                    columnDefs = list(list(
                      targets = c("Species_Found"),
                      render = JS("function(data, type, row) {
                        return data ? data.substring(0, 100) + (data.length > 100 ? '...' : '') : 'N/A';
                      }")
                    ))))
  
  # ===========================================================================
  # OUTPUT: TOP LOCATIONS
  # ===========================================================================
  
  output$location_plotly <- plotly::renderPlotly({
    req(input$refresh)
    inputs <- refresh_inputs()
    data <- filtered_data()
    top_locations <- get_top_locations(data, inputs$top_locations_mode, inputs$species_filter)
    create_location_plotly(top_locations, "all", inputs$top_locations_mode, theme = current_theme())
  })
  
  # Handle plotly click to update map
  observe({
    click <- plotly::event_data("plotly_click", source = "location_plotly")
    if (!is.null(click)) {
      idx <- click$pointNumber + 1
      inputs <- refresh_inputs()
      data <- filtered_data()
      top_locations <- get_top_locations(data, inputs$top_locations_mode, inputs$species_filter)
      if (idx > 0 && idx <= nrow(top_locations)) {
        loc <- top_locations$location[idx]
        spatial <- spatial_data()
        if (nrow(spatial) > 0 && loc %in% spatial$location) {
          point <- spatial[spatial$location == loc, ][1, ]
          coords <- sf::st_coordinates(point)
          lng <- coords[1]
          lat <- coords[2]
          if (!is.na(lng) && !is.na(lat)) {
            updateTabsetPanel(session, "tabs", selected = "map")
            leafletProxy("suco_map") %>%
              setView(lng = lng, lat = lat, zoom = 15) %>%
              addCircleMarkers(lng = lng, lat = lat, radius = 15, color = "red", 
                               fill = TRUE, fillOpacity = 0.7, layerId = "highlighted_location")
          }
        }
      }
    } else {
      leafletProxy("suco_map") %>% clearGroup("highlighted_location")
    }
  })
  
  # ===========================================================================
  # DOWNLOAD HANDLERS
  # ===========================================================================
  
  output$download_detailed <- downloadHandler(
    filename = function() {
      paste0("suco_detailed_", Sys.Date(), ".csv")
    },
    content = function(file) {
      inputs <- refresh_inputs()
      data <- filtered_data()
      detailed_data <- create_detailed_samples_table(data, inputs$species_filter)
      export_csv_safe(detailed_data, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)