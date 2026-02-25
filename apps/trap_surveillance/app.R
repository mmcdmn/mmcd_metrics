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
  
  # Load all trap locations from shapefile (cached — fast, no DB query)
  all_traps <- reactive({
    load_trap_locations()
  })
  
  # Populate comparison week dropdown when year changes
  observeEvent(input$year, {
    weeks_data <- fetch_available_weeks(as.integer(input$year))
    if (!is.null(weeks_data) && nrow(weeks_data) > 0) {
      week_choices <- setNames(
        as.character(weeks_data$yrwk),
        ifelse(!is.na(weeks_data$week_days) & weeks_data$week_days != "",
               sprintf(" %s (Wk %s)", weeks_data$week_days, weeks_data$epiweek),
               sprintf(" (Wk %s)", weeks_data$epiweek))
      )
      # Default compare-to: second week if available
      sel <- if (length(week_choices) >= 2) week_choices[2] else week_choices[1]
      updateSelectInput(session, "yrwk_b", choices = week_choices, selected = sel)
    }
  })
  
  # Comparison data — fetched for the second week on refresh
  compare_data <- eventReactive(input$refresh, {
    if (!isTRUE(input$compare_mode)) return(NULL)
    req(input$yrwk_b)
    req(input$species)
    fetch_combined_area_data(
      yrwk = input$yrwk_b,
      spp_name = input$species,
      infection_metric = input$infection_metric %||% "mle"
    )
  })
  
  # =========================================================================
  # MAP OUTPUT - normal or comparison mode
  # =========================================================================
  
  output$map <- leaflet::renderLeaflet({
    data <- area_data()
    geom <- areas_sf()
    
    spp_label <- SPECIES_MAP[[input$species]]$label %||% input$species
    yrwk_label <- input$yrwk %||% ""
    metric_type <- input$metric_type %||% "abundance"
    trap_locations <- all_traps()
    
    # Comparison mode: show delta map
    if (isTRUE(input$compare_mode)) {
      data_b <- compare_data()
      return(render_comparison_map(
        data_a = data,
        data_b = data_b,
        areas_sf = geom,
        metric_type = metric_type,
        infection_metric = input$infection_metric %||% "mle",
        spp_label = spp_label,
        yrwk_a = input$yrwk %||% "",
        yrwk_b = input$yrwk_b %||% "",
        all_traps = trap_locations
      ))
    }
    
    render_surveillance_map(
      combined_data = data,
      areas_sf = geom,
      metric_type = metric_type,
      infection_metric = input$infection_metric %||% "mle",
      spp_label = spp_label,
      yrwk_label = yrwk_label,
      color_theme = input$color_theme %||% "MMCD",
      all_traps = trap_locations
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
    
    # Fetch 5-year and 10-year averages
    avg_5yr  <- fetch_mle_avg_by_epiweek(input$year, 5)
    avg_10yr <- fetch_mle_avg_by_epiweek(input$year, 10)
    
    p <- ggplot(trend_data, aes(x = week, y = mle)) +
      geom_ribbon(aes(ymin = mle_lower, ymax = mle_upper), alpha = 0.2, fill = "#2c5aa0")
    
    # Add 10-year avg line (behind 5-year)
    if (!is.null(avg_10yr) && nrow(avg_10yr) > 0) {
      p <- p + geom_line(data = avg_10yr, aes(x = week, y = avg_mle, linetype = "10-yr Avg"),
                         color = "#e67e22", linewidth = 0.9, alpha = 0.7)
    }
    # Add 5-year avg line
    if (!is.null(avg_5yr) && nrow(avg_5yr) > 0) {
      p <- p + geom_line(data = avg_5yr, aes(x = week, y = avg_mle, linetype = "5-yr Avg"),
                         color = "#27ae60", linewidth = 0.9, alpha = 0.7)
    }
    
    p <- p +
      geom_line(aes(linetype = paste0(input$year)), color = "#2c5aa0", linewidth = 1) +
      geom_point(color = "#2c5aa0", size = 2) +
      scale_linetype_manual(
        name = "",
        values = c(setNames("solid", input$year), "5-yr Avg" = "dashed", "10-yr Avg" = "dotted"),
        guide = guide_legend(override.aes = list(
          color = c("#2c5aa0", "#27ae60", "#e67e22")
        ))
      ) +
      labs(
        title = sprintf("District-Wide MLE — %s", input$year),
        x = "Epiweek",
        y = "MLE (Infection Rate)"
      ) +
      theme_minimal() +
      theme(text = element_text(size = 13),
            legend.position = "bottom",
            legend.margin = margin(t = 5, b = 0),
            plot.margin = margin(b = 40))
    
    plotly::ggplotly(p, tooltip = c("x", "y")) %>%
      plotly::layout(
        legend = list(orientation = "h", y = -0.25, x = 0.5, xanchor = "center"),
        margin = list(b = 80)
      )
  })
  
  # -------------------------------------------------------------------------
  # MIR TREND PLOT
  # -------------------------------------------------------------------------
  output$mir_trend_plot <- plotly::renderPlotly({
    req(input$year)
    
    trend_data <- fetch_mir_trend(input$year)
    if (is.null(trend_data) || nrow(trend_data) == 0) {
      return(plotly::plot_ly() %>% 
               plotly::layout(title = "No MIR trend data available"))
    }
    
    # Extract week number for x-axis
    trend_data$week <- as.numeric(substr(as.character(trend_data$yrwk), 5, 6))
    
    # Fetch 5-year and 10-year averages
    avg_5yr  <- fetch_mir_avg_by_epiweek(input$year, 5)
    avg_10yr <- fetch_mir_avg_by_epiweek(input$year, 10)
    
    p <- ggplot(trend_data, aes(x = week, y = mir)) +
      geom_ribbon(aes(ymin = pmax(mir - mir_se, 0), ymax = mir + mir_se), 
                  alpha = 0.2, fill = "#8e44ad")
    
    # Add 10-year avg line (behind 5-year)
    if (!is.null(avg_10yr) && nrow(avg_10yr) > 0) {
      p <- p + geom_line(data = avg_10yr, aes(x = week, y = avg_mir, linetype = "10-yr Avg"),
                         color = "#e67e22", linewidth = 0.9, alpha = 0.7)
    }
    # Add 5-year avg line
    if (!is.null(avg_5yr) && nrow(avg_5yr) > 0) {
      p <- p + geom_line(data = avg_5yr, aes(x = week, y = avg_mir, linetype = "5-yr Avg"),
                         color = "#27ae60", linewidth = 0.9, alpha = 0.7)
    }
    
    p <- p +
      geom_line(aes(linetype = paste0(input$year)), color = "#8e44ad", linewidth = 1) +
      geom_point(color = "#8e44ad", size = 2) +
      scale_linetype_manual(
        name = "",
        values = c(setNames("solid", input$year), "5-yr Avg" = "dashed", "10-yr Avg" = "dotted"),
        guide = guide_legend(override.aes = list(
          color = c("#8e44ad", "#27ae60", "#e67e22")
        ))
      ) +
      labs(
        title = sprintf("District-Wide MIR — %s", input$year),
        x = "Epiweek",
        y = "MIR (per 1000 mosquitoes)"
      ) +
      theme_minimal() +
      theme(text = element_text(size = 13),
            legend.position = "bottom",
            legend.margin = margin(t = 5, b = 0),
            plot.margin = margin(b = 40))
    
    plotly::ggplotly(p, tooltip = c("x", "y")) %>%
      plotly::layout(
        legend = list(orientation = "h", y = -0.25, x = 0.5, xanchor = "center"),
        margin = list(b = 80)
      )
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
    
    # Aggregate by yrwk (district-wide for cleaner chart with avg lines)
    trend_district <- abundance_data %>%
      dplyr::group_by(yrwk) %>%
      dplyr::summarise(
        total_count = sum(mosqcount, na.rm = TRUE),
        num_traps = dplyr::n_distinct(loc_code),
        avg_per_trap = total_count / max(num_traps, 1),
        .groups = "drop"
      ) %>%
      dplyr::mutate(week = as.numeric(substr(as.character(yrwk), 5, 6)))
    
    # Also aggregate by area for the colored lines
    trend_agg <- abundance_data %>%
      dplyr::group_by(yrwk, viarea) %>%
      dplyr::summarise(
        total_count = sum(mosqcount, na.rm = TRUE),
        num_traps = dplyr::n_distinct(loc_code),
        avg_per_trap = total_count / max(num_traps, 1),
        .groups = "drop"
      ) %>%
      dplyr::mutate(week = as.numeric(substr(as.character(yrwk), 5, 6)))
    
    # Fetch 5-year and 10-year averages
    avg_5yr  <- fetch_abundance_avg_by_epiweek(input$year, 5, input$species)
    avg_10yr <- fetch_abundance_avg_by_epiweek(input$year, 10, input$species)
    
    p <- ggplot(trend_agg, aes(x = week, y = avg_per_trap, color = viarea)) +
      geom_line(linewidth = 0.8, alpha = 0.6) +
      geom_point(size = 1.5, alpha = 0.6)
    
    # Add 10-year avg line
    if (!is.null(avg_10yr) && nrow(avg_10yr) > 0) {
      p <- p + geom_line(data = avg_10yr, aes(x = week, y = avg_per_trap, color = NULL),
                         color = "#e67e22", linewidth = 1.2, linetype = "dotted",
                         inherit.aes = FALSE)
    }
    # Add 5-year avg line
    if (!is.null(avg_5yr) && nrow(avg_5yr) > 0) {
      p <- p + geom_line(data = avg_5yr, aes(x = week, y = avg_per_trap, color = NULL),
                         color = "#27ae60", linewidth = 1.2, linetype = "dashed",
                         inherit.aes = FALSE)
    }
    # Add district-wide average as a bold black line
    p <- p + geom_line(data = trend_district, aes(x = week, y = avg_per_trap, color = NULL),
                       color = "#2c3e50", linewidth = 1.3, linetype = "solid",
                       inherit.aes = FALSE)
    
    p <- p +
      labs(
        title = sprintf("Abundance by Area — %s — %s", input$year, 
                        SPECIES_MAP[[input$species]]$label %||% input$species),
        subtitle = "Solid black = district avg | Dashed green = 5-yr avg | Dotted orange = 10-yr avg",
        x = "Epiweek",
        y = "Avg Mosquitoes/Trap",
        color = "VI Area"
      ) +
      theme_minimal() +
      theme(text = element_text(size = 12),
            legend.position = "right",
            plot.subtitle = element_text(size = 10, color = "#666"))
    
    plotly::ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      plotly::layout(
        annotations = list(
          list(text = "Dashed green = 5-yr avg | Dotted orange = 10-yr avg | Bold black = district avg",
               x = 0.5, y = 1.05, xref = "paper", yref = "paper",
               showarrow = FALSE, font = list(size = 11, color = "#666"))
        )
      )
  })
  
  # -------------------------------------------------------------------------
  # VECTOR INDEX TREND PLOT (per area, like abundance)
  # -------------------------------------------------------------------------
  output$vi_trend_plot <- plotly::renderPlotly({
    req(input$year, input$species)
    
    withProgress(message = "Loading vector index trend...", value = 0.3, {
      vi_data <- fetch_vi_area_trend(
        year = as.integer(input$year), 
        spp_name = input$species,
        infection_metric = input$infection_metric %||% "mle"
      )
      setProgress(1)
    })
    
    if (is.null(vi_data) || nrow(vi_data) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "No vector index data available"))
    }
    
    # District-wide average per week (for bold black line)
    vi_district <- vi_data %>%
      dplyr::group_by(yrwk, week) %>%
      dplyr::summarise(
        vector_index = mean(vector_index, na.rm = TRUE),
        .groups = "drop"
      )
    
    infection_label <- if ((input$infection_metric %||% "mle") == "mle") "MLE" else "MIR"
    
    p <- ggplot(vi_data, aes(x = week, y = vector_index, color = viarea)) +
      geom_line(linewidth = 0.8, alpha = 0.6) +
      geom_point(size = 1.5, alpha = 0.6)
    
    # District-wide average
    p <- p + geom_line(data = vi_district, aes(x = week, y = vector_index, color = NULL),
                       color = "#2c3e50", linewidth = 1.3, linetype = "solid",
                       inherit.aes = FALSE)
    
    p <- p +
      labs(
        title = sprintf("Vector Index by Area — %s — %s (%s)", input$year,
                        SPECIES_MAP[[input$species]]$label %||% input$species, infection_label),
        subtitle = "VI = Avg/Trap × Infection Rate | Bold black = district avg",
        x = "Epiweek",
        y = "Vector Index (N × P)",
        color = "VI Area"
      ) +
      theme_minimal() +
      theme(text = element_text(size = 12),
            legend.position = "right",
            plot.subtitle = element_text(size = 10, color = "#666"))
    
    plotly::ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      plotly::layout(
        annotations = list(
          list(text = "Bold black = district avg",
               x = 0.5, y = 1.05, xref = "paper", yref = "paper",
               showarrow = FALSE, font = list(size = 11, color = "#666"))
        )
      )
  })
  
  # =========================================================================
  # DATA TABLE — with CI and SE columns
  # =========================================================================
  
  output$data_table <- DT::renderDT({
    data <- area_data()
    if (is.null(data) || nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "Click 'Refresh Data' to load"), 
                           options = list(pageLength = 15)))
    }
    
    infection_met <- input$infection_metric %||% "mle"
    
    # ==== COMPARISON MODE TABLE ====
    if (isTRUE(input$compare_mode)) {
      data_b <- compare_data()
      if (is.null(data_b) || nrow(data_b) == 0) {
        return(DT::datatable(data.frame(Message = "No data for comparison week"), 
                             options = list(pageLength = 15)))
      }
      
      # Pick metric columns
      if (infection_met == "mle") {
        cols <- c("viarea", "avg_per_trap", "infection_rate", "vector_index")
        col_labels <- c("VI Area", "Avg/Trap", "Infection Rate", "Vector Index")
      } else {
        cols <- c("viarea", "avg_per_trap", "vector_index")
        if ("mir_raw" %in% names(data)) {
          cols <- c("viarea", "avg_per_trap", "mir_raw", "vector_index")
          col_labels <- c("VI Area", "Avg/Trap", "MIR", "Vector Index")
        } else {
          col_labels <- c("VI Area", "Avg/Trap", "Vector Index")
        }
      }
      
      # Subset to available columns
      cols_a <- intersect(cols, names(data))
      cols_b <- intersect(cols, names(data_b))
      
      a <- data[, cols_a, drop = FALSE]
      b <- data_b[, cols_b, drop = FALSE]
      
      cmp <- merge(a, b, by = "viarea", suffixes = c("_wkA", "_wkB"), all = TRUE)
      
      # Compute deltas for numeric columns (skip viarea)
      num_cols <- setdiff(cols, "viarea")
      for (nc in num_cols) {
        ca <- paste0(nc, "_wkA")
        cb <- paste0(nc, "_wkB")
        if (ca %in% names(cmp) && cb %in% names(cmp)) {
          cmp[[paste0(nc, "_delta")]] <- round(cmp[[cb]] - cmp[[ca]], 4)
        }
      }
      
      # Round numeric
      cmp <- cmp %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 4)))
      
      return(DT::datatable(cmp, 
                      options = list(pageLength = 15, scrollX = TRUE),
                      caption = sprintf("Comparison: Wk %s vs Wk %s — %s",
                                        input$yrwk %||% "?", input$yrwk_b %||% "?",
                                        SPECIES_MAP[[input$species]]$label %||% "?")))
    }
    
    # ==== NORMAL MODE TABLE ====
    display_data <- data
    
    infection_met <- input$infection_metric %||% "mle"
    
    if (infection_met == "mle") {
      # SE from 95% CI: SE = (upper - lower) / (2 * 1.96)
      # CI formatted as "(lower - upper)"
      if (all(c("rate_lower", "rate_upper") %in% names(display_data))) {
        display_data <- display_data %>%
          dplyr::mutate(
            infection_se = ifelse(!is.na(rate_lower) & !is.na(rate_upper),
                                  (rate_upper - rate_lower) / (2 * 1.96), NA_real_),
            ci_95 = ifelse(!is.na(rate_lower) & !is.na(rate_upper),
                           sprintf("(%s - %s)", 
                                   formatC(round(rate_lower, 4), format = "f", digits = 4),
                                   formatC(round(rate_upper, 4), format = "f", digits = 4)),
                           NA_character_)
          )
      }
    } else {
      # MIR SE: binomial SE = sqrt(p * (1-p) / n) * 1000
      # CI: MIR ± 1.96 × SE
      if (all(c("positive", "total_mosquitoes") %in% names(display_data))) {
        display_data <- display_data %>%
          dplyr::mutate(
            p_hat = ifelse(total_mosquitoes > 0, positive / total_mosquitoes, 0),
            infection_se = ifelse(total_mosquitoes > 0,
                                  sqrt(p_hat * (1 - p_hat) / total_mosquitoes) * 1000, NA_real_),
            ci_lower = ifelse(!is.na(infection_se), pmax(mir_raw - 1.96 * infection_se, 0), NA_real_),
            ci_upper = ifelse(!is.na(infection_se), mir_raw + 1.96 * infection_se, NA_real_),
            ci_95 = ifelse(!is.na(ci_lower),
                           sprintf("(%s - %s)", 
                                   formatC(round(ci_lower, 4), format = "f", digits = 4),
                                   formatC(round(ci_upper, 4), format = "f", digits = 4)),
                           NA_character_)
          ) %>%
          dplyr::select(-p_hat, -ci_lower, -ci_upper)
      }
    }
    
    # Build clean display columns
    if (infection_met == "mle") {
      display_data <- display_data %>%
        dplyr::select(
          `VI Area` = viarea,
          `Total Count` = total_count,
          `Traps` = num_traps,
          `Avg/Trap` = avg_per_trap,
          `Infection Rate` = infection_rate,
          `SE` = dplyr::any_of("infection_se"),
          `95% CI` = dplyr::any_of("ci_95"),
          `Vector Index` = vector_index,
          dplyr::any_of(c("rate_lower", "rate_upper"))
        ) %>%
        dplyr::select(-dplyr::any_of(c("rate_lower", "rate_upper")))
    } else {
      display_data <- display_data %>%
        dplyr::select(
          `VI Area` = viarea,
          `Total Count` = total_count,
          `Traps` = num_traps,
          `Avg/Trap` = avg_per_trap,
          `MIR (per 1000)` = dplyr::any_of("mir_raw"),
          `SE` = dplyr::any_of("infection_se"),
          `95% CI` = dplyr::any_of("ci_95"),
          `Positive` = dplyr::any_of("positive"),
          `Pools` = dplyr::any_of("total_pools"),
          `Mosquitoes` = dplyr::any_of("total_mosquitoes"),
          `Vector Index` = vector_index
        )
    }
    
    # Round all numeric columns to 4 decimal places (CI is already formatted as string)
    display_data <- display_data %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 4)))
    
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
