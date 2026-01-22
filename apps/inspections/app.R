# Inspections App - Site Inspection Coverage Gaps and Analytics

library(shiny)
library(DT)
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)

# Source shared database helpers and local functions
source("../../shared/db_helpers.R")
source("../../shared/server_utilities.R")
source("data_functions.R")
source("display_functions.R")
source("ui_helper.R")

# Set application name for AWS RDS monitoring
set_app_name("inspections")

# Define UI
ui <- create_main_ui()

# Define server logic
server <- function(input, output, session) {
  
  # Reactive theme handling
  current_theme <- reactive({
    input$color_theme
  })
  
  # Set global theme option when changed
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
  
  # Helper for null coalescing
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # Update FOS choices when facility changes
  observeEvent(input$facility, {
    foremen_lookup <- get_foremen_lookup()
    
    if (input$facility == "all") {
      # Show all FOS when "All Facilities" is selected
      fosarea_choices <- get_fosarea_display_choices()
    } else {
      # Filter FOS by selected facility
      filtered_foremen <- foremen_lookup[foremen_lookup$facility == input$facility, ]
      fosarea_choices <- c("All FOS" = "all")
      if (nrow(filtered_foremen) > 0) {
        display_names <- paste0(filtered_foremen$shortname, " (", filtered_foremen$facility, ")")
        fosarea_choices <- c(
          fosarea_choices,
          setNames(filtered_foremen$shortname, display_names)
        )
      }
    }
    
    updateSelectizeInput(session, "fosarea", choices = fosarea_choices, selected = "all")
  })
  
  # ============= SINGLE UNIFIED DATA RETRIEVAL =============
  # Single reactive data source that loads ALL inspection data with shared filters
  comprehensive_data <- eventReactive(input$load_data, {
    withProgress(message = "Loading inspection data...", value = 0.5, {
      # Apply ALL shared filters consistently
      facility_filter <- if (length(input$facility) == 0 || "all" %in% input$facility) NULL else input$facility
      fosarea_filter <- if (length(input$fosarea) == 0 || "all" %in% input$fosarea) NULL else input$fosarea
      
      # Parse zone filter - handle radio button
      zone_value <- input$zone %||% "all"
      zone_filter <- if (zone_value == "all") {
      NULL
    } else if (zone_value == "1,2") {
      c("1", "2")
    } else {
      zone_value
    }
    
    priority_filter <- if (length(input$priority) == 0 || "all" %in% input$priority) NULL else input$priority
    drone_filter <- input$drone_filter %||% "include_drone"
    spring_only <- input$spring_only %||% FALSE
    prehatch_only <- input$prehatch_only %||% FALSE
    
    # ONE SINGLE QUERY GETS ALL DATA
    
    get_all_inspection_data(
      facility_filter = facility_filter,
      fosarea_filter = fosarea_filter, 
      zone_filter = zone_filter,
      priority_filter = priority_filter,
      drone_filter = drone_filter,
      spring_only = spring_only,
      prehatch_only = prehatch_only
    )
    })
  }, ignoreNULL = FALSE)
  
  # Data loading summary
  output$data_summary <- renderText({
    if (input$load_data == 0) {
      "Click 'Load Data' to begin analysis"
    } else {
      comp_data <- comprehensive_data()
      if (nrow(comp_data) == 0) {
        "No data found with current filters"
      } else {
        total_sites <- get_total_sites_count_from_data(comp_data, input$air_gnd %||% "both")
        total_records <- format(nrow(comp_data), big.mark = ",")
        paste0(format(total_sites, big.mark = ","), " sites | ", total_records, " records loaded")
      }
    }
  })
  
  # ============= DYNAMIC BUTTON RENDERING =============
  # Gaps button with count
  output$gaps_button_ui <- renderUI({
    if (input$load_data == 0) {
      actionButton("analyze_gaps", 
        " Analyze Inspection Gaps", 
        class = "btn-refresh", 
        style = "width: 100%; padding: 12px;")
    } else {
      comp_data <- comprehensive_data()
      filtered_data <- comp_data
      if (!is.null(input$air_gnd) && input$air_gnd != "both") {
        filtered_data <- filtered_data %>% filter(air_gnd == input$air_gnd)
      }
      gap_result <- get_inspection_gaps_from_data(filtered_data, input$years_gap %||% 3, Sys.Date())
      count <- format(nrow(gap_result), big.mark = ",")
      years <- input$years_gap %||% 3
      actionButton("analyze_gaps", 
        paste0(count, " sites with gaps ≥ ", years, " years"), 
        class = "btn-refresh", 
        style = "width: 100%; padding: 12px;")
    }
  })
  
  # Wet frequency button with count
  output$wet_button_ui <- renderUI({
    if (input$load_data == 0) {
      actionButton("analyze_wet", 
        " Analyze Wet Frequency", 
        class = "btn-refresh", 
        style = "width: 100%; padding: 12px;")
    } else {
      comp_data <- comprehensive_data()
      filtered_data <- comp_data
      if (!is.null(input$air_gnd) && input$air_gnd != "both") {
        filtered_data <- filtered_data %>% filter(air_gnd == input$air_gnd)
      }
      wet_result <- get_wet_frequency_from_data(filtered_data, input$air_gnd %||% "both", 
                                                 input$min_inspections %||% 5, 5)
      count <- format(nrow(wet_result), big.mark = ",")
      min_insp <- input$min_inspections %||% 5
      actionButton("analyze_wet", 
        paste0(count, " sites with ≥ ", min_insp, " inspections"), 
        class = "btn-refresh", 
        style = "width: 100%; padding: 12px;")
    }
  })
  
  # Larvae button with count
  output$larvae_button_ui <- renderUI({
    if (input$load_data == 0) {
      actionButton("analyze_larvae", 
        " Find High Dip Count Sites", 
        class = "btn-refresh", 
        style = "width: 100%; padding: 12px;")
    } else {
      comp_data <- comprehensive_data()
      filtered_data <- comp_data
      if (!is.null(input$air_gnd) && input$air_gnd != "both") {
        filtered_data <- filtered_data %>% filter(air_gnd == input$air_gnd)
      }
      larvae_result <- get_high_larvae_sites_from_data(filtered_data, input$larvae_threshold %||% 2, 5, input$air_gnd %||% "both")
      count <- format(nrow(larvae_result), big.mark = ",")
      threshold <- input$larvae_threshold %||% 2
      actionButton("analyze_larvae", 
        paste0(count, " sites with larvae ≥ ", threshold, " in the last 5 years"), 
        class = "btn-refresh", 
        style = "width: 100%; padding: 12px;")
    }
  })
  
  # ============= INSPECTION GAPS TAB =============
  gap_data <- eventReactive(input$analyze_gaps, {
    if (input$load_data == 0) {
      showNotification("Please load data first using the 'Load Data' button above", type = "warning")
      return(data.frame())
    }
    
    comp_data <- comprehensive_data()
    
    # Apply air/ground filter for this specific analysis
    filtered_data <- comp_data
    if (!is.null(input$air_gnd) && input$air_gnd != "both") {
      filtered_data <- filtered_data %>% 
        filter(air_gnd == input$air_gnd)
    }
    
    get_inspection_gaps_from_data(
      comprehensive_data = filtered_data,
      years_gap = input$years_gap %||% 3,
      ref_date = Sys.Date()
    )
  })
  
  output$gaps_summary <- renderText({
    if (input$analyze_gaps == 0) {
      "Configure settings and click 'Analyze'"
    } else {
      data <- gap_data()
      if (nrow(data) == 0) {
        "No sites found with inspection gaps"
      } else {
        never_inspected <- sum(data$inspection_status == "Never Inspected", na.rm = TRUE)
        gap_sites <- sum(data$inspection_status == "Inspection Gap", na.rm = TRUE)
        paste0(format(nrow(data), big.mark = ","), " gap sites: ", 
               format(never_inspected, big.mark = ","), " never inspected, ",
               format(gap_sites, big.mark = ","), " with ", input$years_gap, "+ year gaps")
      }
    }
  })

  output$gaps_table <- DT::renderDataTable({
    if (input$analyze_gaps == 0 || input$load_data == 0) {
      DT::datatable(
        data.frame(Message = "Load data and click 'Analyze Inspection Gaps' to view results"),
        rownames = FALSE,
        options = list(dom = 't', ordering = FALSE)
      )
    } else {
      render_gap_table(gap_data(), theme = current_theme())
    }
  })
  
  # ============= SITE ANALYTICS TAB =============
  # ============= WET ANALYSIS TAB (USES MAIN DATA) =============
  wet_analysis_data <- eventReactive(input$analyze_wet, {
    if (input$load_data == 0) {
      showNotification("Please load data first using the 'Load Data' button above", type = "warning")
      return(list())
    }
    
    # Use the SAME comprehensive data - no separate query needed!
    comp_data <- comprehensive_data()
    air_gnd_filter <- input$air_gnd %||% "both"
    
    list(
      total_sites = get_total_sites_count_from_data(comp_data, air_gnd_filter),
      wet_frequency = get_wet_frequency_from_data(comp_data, air_gnd_filter, input$min_inspections %||% 5, input$years_back %||% 5),
      summary_stats = get_summary_stats_from_data(comp_data, air_gnd_filter, input$years_back %||% 5)
    )
  })
  
  # Auto-calculate basic stats when data is loaded
  observe({
    if (input$load_data > 0) {
      comp_data <- comprehensive_data()
      air_gnd_filter <- input$air_gnd %||% "both"
      
      output$total_sites_count <- renderText({
        total_sites <- get_total_sites_count_from_data(comp_data, air_gnd_filter)
        format(total_sites, big.mark = ",")
      })
      
      stats <- get_summary_stats_from_data(comp_data, air_gnd_filter, input$years_back %||% 5)
      
      # Total active sites stat box - use wet analysis data
      output$total_active_sites_box <- renderUI({
        if (input$analyze_wet == 0) {
          total_sites <- 0
        } else {
          wet_data_result <- wet_analysis_data()
          wet_comp_data <- if (length(wet_data_result) > 0 && !is.null(wet_data_result$wet_frequency)) {
            # Get unique sites from wet frequency data
            wet_data_result$wet_frequency %>% distinct(sitecode) %>% nrow()
          } else {
            0
          }
          total_sites <- wet_comp_data
        }
        
        # Get theme-based colors
        status_colors <- get_status_colors(theme = current_theme())
        
        create_stat_box(
          value = format(total_sites, big.mark = ","),
          title = "Sites Analyzed",
          bg_color = unname(status_colors["active"]),
          text_color = "#ffffff",
          icon = icon("map-marker")
        )
      })
      
      # Overall wet percentage stat box
      output$overall_wet_percentage_box <- renderUI({
        if (input$analyze_wet == 0) {
          wet_pct <- 0
        } else {
          wet_data_result <- wet_analysis_data()
          wet_pct <- if (length(wet_data_result) > 0 && !is.null(wet_data_result$wet_frequency) && nrow(wet_data_result$wet_frequency) > 0) {
            # Calculate average wet percentage across all sites
            round(mean(wet_data_result$wet_frequency$wet_percentage, na.rm = TRUE), 1)
          } else {
            0
          }
        }
        
        # Get theme-based colors
        status_colors <- get_status_colors(theme = current_theme())
        
        create_stat_box(
          value = paste0(wet_pct, "%"),
          title = "Average Wet Frequency",
          bg_color = unname(status_colors["completed"]),
          text_color = "#ffffff",
          icon = icon("percentage")
        )
      })
    } else {
      output$total_sites_count <- renderText("-")
      
      # Default stat boxes when no data
      output$total_active_sites_box <- renderUI({
        status_colors <- get_status_colors(theme = current_theme())
        
        create_stat_box(
          value = "-",
          title = "Sites Analyzed",
          bg_color = unname(status_colors["unknown"]),
          text_color = "#ffffff",
          icon = icon("map-marker")
        )
      })
      
      output$overall_wet_percentage_box <- renderUI({
        status_colors <- get_status_colors(theme = current_theme())
        
        create_stat_box(
          value = "-",
          title = "Average Wet Frequency",
          bg_color = unname(status_colors["unknown"]),
          text_color = "#ffffff",
          icon = icon("percentage")
        )
      })
    }
  })
  
  output$wet_frequency_table <- DT::renderDataTable({
    if (input$analyze_wet == 0 || input$load_data == 0) {
      DT::datatable(
        data.frame(Message = "Load data and click 'Analyze Wet Frequency' to view detailed results"),
        rownames = FALSE,
        options = list(dom = 't', ordering = FALSE)
      )
    } else {
      data <- wet_analysis_data()
      render_wet_frequency_table(data$wet_frequency, theme = current_theme())
    }
  })
  
  # ============= LARVAE THRESHOLD TAB =============
  larvae_data <- eventReactive(input$analyze_larvae, {
    if (input$load_data == 0) {
      showNotification("Please load data first using the 'Load Data' button above", type = "warning")
      return(data.frame())
    }
    
    comp_data <- comprehensive_data()
    air_gnd_filter <- input$air_gnd %||% "both"
    
    get_high_larvae_sites_from_data(
      comprehensive_data = comp_data,
      threshold = input$larvae_threshold %||% 2,
      years_back = input$years_back %||% 5,
      air_gnd_filter = air_gnd_filter
    )
  })
  
  output$larvae_summary <- renderText({
    if (input$analyze_larvae == 0) {
      "Configure settings and click 'Find High Larvae Sites'"
    } else {
      data <- larvae_data()
      threshold <- input$larvae_threshold %||% 2
      years_back <- input$years_back %||% 5
      
      if (nrow(data) == 0) {
        paste0("No sites found with larvae ≥ ", threshold, 
               " in the last ", years_back, " years")
      } else {
        paste0(format(nrow(data), big.mark = ","), " sites with larvae ≥ ", 
               threshold, " in the last ", years_back, " years")
      }
    }
  })
  
  output$larvae_table <- DT::renderDataTable({
    if (input$analyze_larvae == 0 || input$load_data == 0) {
      DT::datatable(
        data.frame(Message = "Load data and click 'Find High Larvae Sites' to view results"),
        rownames = FALSE,
        options = list(dom = 't', ordering = FALSE)
      )
    } else {
      render_high_larvae_table(larvae_data(), theme = current_theme())
    }
  })
  
  # ============= CHART OUTPUTS =============
  
  # Wet frequency chart
  output$wet_frequency_chart <- renderPlotly({
    if (input$analyze_wet == 0 || input$load_data == 0) return(NULL)
    wet_result <- wet_analysis_data()
    if (length(wet_result) == 0 || is.null(wet_result$wet_frequency) || nrow(wet_result$wet_frequency) == 0) return(NULL)
    create_wet_frequency_chart(wet_result$wet_frequency, theme = current_theme())
  })
  
  # Priority distribution chart  
  output$priority_chart <- renderPlotly({
    if (input$load_data == 0) return(NULL)
    comp_data <- comprehensive_data()
    if (nrow(comp_data) == 0) return(NULL)
    
    # Get unique sites with priority info
    site_data <- comp_data %>%
      distinct(sitecode, priority, .keep_all = TRUE)
    create_priority_chart(site_data, theme = current_theme())
  })
  
  # Exceedance frequency chart
  output$exceedance_frequency_chart <- renderPlotly({
    if (input$analyze_larvae == 0 || input$load_data == 0) return(NULL)
    larvae_data_result <- larvae_data()
    create_exceedance_frequency_chart(larvae_data_result, theme = current_theme())
  })
  
  # Larvae distribution chart
  output$larvae_distribution_chart <- renderPlotly({
    if (input$analyze_larvae == 0 || input$load_data == 0) return(NULL)
    larvae_data_result <- larvae_data()
    create_larvae_distribution_chart(larvae_data_result, theme = current_theme())
  })
  
  # Facility gap chart
  output$facility_gap_chart <- renderPlotly({
    if (input$analyze_gaps == 0 || input$load_data == 0) return(NULL)
    
    # Get both comprehensive data and gap data for analysis
    comp_data <- comprehensive_data()
    gap_data_result <- gap_data()
    
    # Create facility analysis
    facility_analysis <- get_facility_gap_analysis(comp_data, gap_data_result)
    
    # Create the chart
    create_facility_gap_chart(facility_analysis, theme = current_theme())
  })
  
  # Download handlers for CSV exports
  output$download_gaps_data <- downloadHandler(
    filename = function() {
      paste0("inspection_gaps_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (input$analyze_gaps == 0 || input$load_data == 0) {
        export_csv_safe(data.frame("No data available" = "Load data and analyze gaps first"), file)
      } else {
        gap_data_result <- gap_data()
        if (!is.null(gap_data_result) && nrow(gap_data_result) > 0) {
          export_csv_safe(gap_data_result, file)
        } else {
          export_csv_safe(data.frame("No gaps found" = character(0)), file)
        }
      }
    }
  )
  
  output$download_wet_frequency_data <- downloadHandler(
    filename = function() {
      paste0("wet_frequency_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (input$analyze_wet == 0 || input$load_data == 0) {
        export_csv_safe(data.frame("No data available" = "Load data and analyze wet frequency first"), file)
      } else {
        wet_data <- wet_analysis_data()
        if (!is.null(wet_data$wet_frequency) && nrow(wet_data$wet_frequency) > 0) {
          export_csv_safe(wet_data$wet_frequency, file)
        } else {
          export_csv_safe(data.frame("No wet frequency data" = character(0)), file)
        }
      }
    }
  )
  
  output$download_larvae_data <- downloadHandler(
    filename = function() {
      paste0("larvae_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (input$analyze_larvae == 0 || input$load_data == 0) {
        export_csv_safe(data.frame("No data available" = "Load data and analyze larvae first"), file)
      } else {
        larvae_data_result <- larvae_data()
        if (!is.null(larvae_data_result) && nrow(larvae_data_result) > 0) {
          export_csv_safe(larvae_data_result, file)
        } else {
          export_csv_safe(data.frame("No high larvae sites found" = character(0)), file)
        }
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)