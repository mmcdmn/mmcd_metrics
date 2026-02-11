# =============================================================================
# Trap Surveillance Dashboard
# =============================================================================
# Displays mosquito vector abundance, MLE/MIR infection rates, and Vector Index
# by Vector Index Area using pre-calculated database views.
#
# NO in-app KNN or MLE calculations — all data comes from materialized views.
# =============================================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(rlang)  # for %||% operator

source("../../shared/db_helpers.R")
source("ui_helper.R")
source("data_functions.R")
source("display_functions.R")

ui <- trap_surveillance_ui()

server <- function(input, output, session) {
  
  # =========================================================================
  # INITIALIZATION - Load years and species on startup
  # =========================================================================
  
  # Species choices (static)
  species_choices <- get_surveillance_species_choices()
  updateSelectInput(session, "species", choices = species_choices, selected = "Total_Cx_vectors")
  
  # Load available years
  observe({
    years <- fetch_available_years()
    if (!is.null(years)) {
      updateSelectInput(session, "year", choices = years, selected = years[1])
    }
  })
  
  # Update weeks when year changes
  observeEvent(input$year, {
    req(input$year)
    weeks_data <- fetch_available_weeks(as.integer(input$year))
    if (!is.null(weeks_data) && nrow(weeks_data) > 0) {
      # Build nice labels with actual dates when available
      week_choices <- setNames(
        as.character(weeks_data$yrwk),
        ifelse(!is.na(weeks_data$week_days) & weeks_data$week_days != "",
               sprintf(" %s (Wk %s)", weeks_data$week_days, weeks_data$epiweek),
               sprintf(" (Wk %s)",  weeks_data$epiweek))
      )
      updateSelectInput(session, "yrwk", choices = week_choices, selected = week_choices[1])
    }
  })
  
  # =========================================================================
  # REACTIVE DATA - Fetch combined data on refresh
  # =========================================================================
  
  area_data <- eventReactive(input$refresh, {
    req(input$yrwk)
    req(input$species)
    
    withProgress(message = "Fetching surveillance data...", value = 0.3, {
      result <- fetch_combined_area_data(
        yrwk = input$yrwk,
        spp_name = input$species,
        infection_metric = input$infection_metric %||% "mle"
      )
      setProgress(1, detail = "Complete!")
      result
    })
  })
  
  # Load area geometries (cached)
  areas_sf <- reactive({
    load_vi_area_geometries()
  })
  
  # =========================================================================
  # MAP OUTPUT
  # =========================================================================
  
  output$map <- leaflet::renderLeaflet({
    data <- area_data()
    geom <- areas_sf()
    
    spp_label <- SPECIES_MAP[[input$species]]$label %||% input$species
    yrwk_label <- input$yrwk %||% ""
    
    render_surveillance_map(
      combined_data = data,
      areas_sf = geom,
      metric_type = input$metric_type %||% "abundance",
      infection_metric = input$infection_metric %||% "mle",
      spp_label = spp_label,
      yrwk_label = yrwk_label,
      color_theme = input$color_theme %||% "MMCD"
    )
  })
  
  # =========================================================================
  # TREND PLOTS
  # =========================================================================
  
  output$mle_trend_plot <- plotly::renderPlotly({
    req(input$year)
    
    trend_data <- fetch_mle_trend(input$year)
    if (is.null(trend_data) || nrow(trend_data) == 0) {
      return(plotly::plot_ly() %>% 
               plotly::layout(title = "No MLE trend data available"))
    }
    
    # Extract week number for x-axis
    trend_data$week <- as.numeric(substr(as.character(trend_data$yrwk), 5, 6))
    
    p <- ggplot(trend_data, aes(x = week, y = mle)) +
      geom_ribbon(aes(ymin = mle_lower, ymax = mle_upper), alpha = 0.2, fill = "#2c5aa0") +
      geom_line(color = "#2c5aa0", linewidth = 1) +
      geom_point(color = "#2c5aa0", size = 2) +
      labs(
        title = sprintf("District-Wide MLE — %s", input$year),
        x = "Epiweek",
        y = "MLE (Infection Rate)"
      ) +
      theme_minimal() +
      theme(text = element_text(size = 13))
    
    plotly::ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$abundance_trend_plot <- plotly::renderPlotly({
    req(input$year, input$species)
    
    withProgress(message = "Loading abundance trend...", value = 0.3, {
      abundance_data <- fetch_abundance_data(year = as.integer(input$year), spp_name = input$species)
      setProgress(1)
    })
    
    if (is.null(abundance_data) || nrow(abundance_data) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "No abundance data available"))
    }
    
    # Aggregate by yrwk and viarea
    trend_agg <- abundance_data %>%
      dplyr::group_by(yrwk, viarea) %>%
      dplyr::summarise(
        total_count = sum(mosqcount, na.rm = TRUE),
        num_traps = dplyr::n_distinct(loc_code),
        avg_per_trap = total_count / max(num_traps, 1),
        .groups = "drop"
      ) %>%
      dplyr::mutate(week = as.numeric(substr(as.character(yrwk), 5, 6)))
    
    p <- ggplot(trend_agg, aes(x = week, y = avg_per_trap, color = viarea)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.5) +
      labs(
        title = sprintf("Abundance by Area — %s — %s", input$year, 
                        SPECIES_MAP[[input$species]]$label %||% input$species),
        x = "Epiweek",
        y = "Avg Mosquitoes/Trap",
        color = "VI Area"
      ) +
      theme_minimal() +
      theme(text = element_text(size = 12),
            legend.position = "right")
    
    plotly::ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # =========================================================================
  # DATA TABLE
  # =========================================================================
  
  output$data_table <- DT::renderDT({
    data <- area_data()
    if (is.null(data) || nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "Click 'Refresh Data' to load"), 
                           options = list(pageLength = 15)))
    }
    
    # Clean up for display
    display_data <- data %>%
      dplyr::select(
        `VI Area` = viarea,
        `Total Count` = total_count,
        `Traps` = num_traps,
        `Avg/Trap` = avg_per_trap,
        dplyr::everything()
      )
    
    # Remove internal columns
    display_data <- display_data %>%
      dplyr::select(-dplyr::any_of(c("rate_lower", "rate_upper")))
    
    DT::datatable(display_data, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  caption = sprintf("Week %s — %s", 
                                    input$yrwk %||% "?",
                                    SPECIES_MAP[[input$species]]$label %||% "?"))
  })
  
  # =========================================================================
  # DOWNLOAD
  # =========================================================================
  
  output$download_data <- downloadHandler(
    filename = function() {
      sprintf("trap_surveillance_%s_%s_%s.csv", input$yrwk, input$species, Sys.Date())
    },
    content = function(file) {
      tryCatch({
        data <- area_data()
        if (!is.null(data) && nrow(data) > 0) {
          result <- export_csv_safe(data, file, clean_data = TRUE)
          if (!result$success) {
            write.csv(data, file, row.names = FALSE, na = "")
          }
        } else {
          write.csv(data.frame(Message = "No data available"), file, row.names = FALSE)
        }
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )
}

shinyApp(ui = ui, server = server)
