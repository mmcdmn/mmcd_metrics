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
  # Used as fallback when no week is selected
  all_traps <- reactive({
    load_trap_locations()
  })
  
  # Load traps for selected week with pool details (replaces shapefile when week is selected)
  week_traps <- eventReactive(input$refresh, {
    req(input$yrwk)
    virus_target <- if (!is.null(input$infection_metric) && input$infection_metric == "mir") "WNV" else "WNV"
    fetch_traps_for_week(yrwk = input$yrwk, virus_target = virus_target)
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
    week_trap_data <- week_traps()
    
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
        all_traps = trap_locations,
        week_traps = week_trap_data
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
      all_traps = trap_locations,
      week_traps = week_trap_data
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
    
    infection_met <- input$infection_metric %||% "mle"
    
    withProgress(message = "Loading vector index trend...", value = 0.3, {
      trend_data <- fetch_vi_district_trend(
        year = as.integer(input$year),
        spp_name = input$species,
        infection_metric = infection_met
      )
      setProgress(1)
    })
    
    if (is.null(trend_data) || nrow(trend_data) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "No vector index data available"))
    }
    
    infection_label <- if (infection_met == "mle") "MLE" else "MIR"
    
    # Fetch 5-year and 10-year VI averages
    avg_5yr  <- fetch_vi_avg_by_epiweek(input$year, 5, input$species, infection_met)
    avg_10yr <- fetch_vi_avg_by_epiweek(input$year, 10, input$species, infection_met)
    
    # Build linetype scale dynamically based on available data
    lt_values <- c(setNames("solid", as.character(input$year)))
    lt_colors <- c("#c0392b")
    has_5yr  <- !is.null(avg_5yr) && nrow(avg_5yr) > 0
    has_10yr <- !is.null(avg_10yr) && nrow(avg_10yr) > 0
    
    p <- ggplot(trend_data, aes(x = week, y = vector_index)) +
      geom_ribbon(aes(ymin = vi_lower, ymax = vi_upper), alpha = 0.2, fill = "#c0392b")
    
    # Add 10-year avg line (behind 5-year)
    if (has_10yr) {
      p <- p + geom_line(data = avg_10yr, aes(x = week, y = avg_vi, linetype = "10-yr Avg"),
                         color = "#e67e22", linewidth = 0.9, alpha = 0.7)
      lt_values <- c(lt_values, "10-yr Avg" = "dotted")
      lt_colors <- c(lt_colors, "#e67e22")
    }
    # Add 5-year avg line
    if (has_5yr) {
      p <- p + geom_line(data = avg_5yr, aes(x = week, y = avg_vi, linetype = "5-yr Avg"),
                         color = "#27ae60", linewidth = 0.9, alpha = 0.7)
      lt_values <- c(lt_values, "5-yr Avg" = "dashed")
      lt_colors <- c(lt_colors, "#27ae60")
    }
    
    p <- p +
      geom_line(aes(linetype = as.character(input$year)), color = "#c0392b", linewidth = 1) +
      geom_point(color = "#c0392b", size = 2) +
      scale_linetype_manual(
        name = "",
        values = lt_values,
        guide = guide_legend(override.aes = list(color = lt_colors))
      ) +
      labs(
        title = sprintf("District-Wide Vector Index — %s — %s (%s)", input$year,
                        SPECIES_MAP[[input$species]]$label %||% input$species, infection_label),
        x = "Epiweek",
        y = "Vector Index (N × P)"
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
  # VECTOR INDEX BY AREA TREND PLOT (per area colored lines + district avg)
  # -------------------------------------------------------------------------
  output$vi_area_trend_plot <- plotly::renderPlotly({
    req(input$year, input$species)
    
    infection_met <- input$infection_metric %||% "mle"
    
    withProgress(message = "Loading VI by area...", value = 0.3, {
      vi_data <- fetch_vi_area_trend(
        year = as.integer(input$year),
        spp_name = input$species,
        infection_metric = infection_met
      )
      setProgress(1)
    })
    
    if (is.null(vi_data) || nrow(vi_data) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "No vector index data available"))
    }
    
    # District-wide average per week
    vi_district <- vi_data %>%
      dplyr::group_by(yrwk, week) %>%
      dplyr::summarise(
        vector_index = mean(vector_index, na.rm = TRUE),
        .groups = "drop"
      )
    
    infection_label <- if (infection_met == "mle") "MLE" else "MIR"
    
    p <- ggplot(vi_data, aes(x = week, y = vector_index, color = viarea)) +
      geom_line(linewidth = 0.8, alpha = 0.6) +
      geom_point(size = 1.5, alpha = 0.6) +
      geom_line(data = vi_district, aes(x = week, y = vector_index),
               color = "#2c3e50", linewidth = 1.3, linetype = "solid",
               inherit.aes = FALSE) +
      labs(
        title = sprintf("Vector Index by Area \u2014 %s \u2014 %s (%s)", input$year,
                        SPECIES_MAP[[input$species]]$label %||% input$species, infection_label),
        subtitle = "Bold black = district avg",
        x = "Epiweek",
        y = "Vector Index (N \u00d7 P)",
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
  # CAUSAL ANALYSIS TAB
  # =========================================================================
  
  # Populate year choices for analysis tab
  observe({
    years <- fetch_available_years()
    if (!is.null(years)) {
      analysis_choices <- c("All Years" = "all", setNames(as.character(years), years))
      updateSelectInput(session, "analysis_year", choices = analysis_choices, selected = "all")
    }
  })
  
  # Master reactive: runs all phases in sequence
  analysis_data <- eventReactive(input$analysis_refresh, {
    year_val <- if (!is.null(input$analysis_year) && input$analysis_year != "all") {
      as.integer(input$analysis_year)
    } else NULL
    
    bw <- input$analysis_bandwidth %||% 3
    radius <- input$analysis_radius %||% 10
    
    withProgress(message = "Running causal analysis...", value = 0.05, {
      # Phase 2: Fetch & score traps
      setProgress(0.15, detail = "Scoring traps (Phase 2)...")
      perf <- fetch_trap_performance(year = year_val)
      if (is.null(perf) || nrow(perf) == 0) return(NULL)
      
      # Phase 1: Spatial risk kernel
      setProgress(0.35, detail = "Computing spatial risk (Phase 1)...")
      risk <- compute_spatial_risk(perf, bandwidth_km = bw, max_radius_km = radius)
      
      # Phase 3: Causal factors
      setProgress(0.55, detail = "Analyzing causal factors (Phase 3)...")
      causal <- compute_causal_factors(risk)
      
      # Area coverage
      setProgress(0.70, detail = "Computing area coverage...")
      coverage <- compute_area_coverage(risk)
      
      # Risk surface grid
      setProgress(0.85, detail = "Generating risk surface...")
      surface <- tryCatch(
        generate_risk_surface(risk, grid_res = 0.005, bandwidth_km = bw),
        error = function(e) { message("Risk surface error: ", e$message); NULL }
      )
      
      setProgress(1, detail = "Complete!")
      list(
        risk_data = risk,
        causal = causal,
        coverage = coverage,
        surface = surface
      )
    })
  })
  
  # --- Combined Map ---
  output$analysis_map <- leaflet::renderLeaflet({
    result <- analysis_data()
    if (is.null(result)) {
      return(leaflet::leaflet() %>% leaflet::addTiles() %>%
               leaflet::setView(lng = -93.3, lat = 44.95, zoom = 9) %>%
               leaflet::addControl(
                 html = "<div style='background:white;padding:10px;'>Click 'Run Analysis' to begin</div>",
                 position = "topright"))
    }
    geom <- areas_sf()
    render_trap_analysis_map(
      risk_data = result$risk_data,
      risk_surface = result$surface,
      areas_sf = geom,
      area_coverage = result$coverage
    )
  })
  
  # --- Area Coverage Table ---
  output$coverage_table <- DT::renderDT({
    result <- analysis_data()
    if (is.null(result) || is.null(result$coverage) || nrow(result$coverage) == 0) {
      return(DT::datatable(data.frame(Message = "Click 'Run Analysis' to begin")))
    }
    
    display <- result$coverage %>%
      dplyr::select(
        `VI Area` = viarea,
        `Traps` = n_traps,
        `Avg Score` = avg_score,
        `Avg Risk` = avg_risk,
        `Pools` = total_pools,
        `Positives` = total_positive,
        `Pos %` = positivity_pct,
        `Low %` = pct_low,
        `Grade` = coverage_grade
      ) %>%
      dplyr::arrange(match(Grade, c("Gap", "Thin", "Adequate", "Good")))
    
    DT::datatable(display,
                  options = list(pageLength = 20, scrollX = TRUE, dom = "t"),
                  rownames = FALSE,
                  caption = "Coverage by VI Area") %>%
      DT::formatStyle("Grade",
                       backgroundColor = DT::styleEqual(
                         c("Good", "Adequate", "Thin", "Gap"),
                         c("#d5f5e3", "#fef9e7", "#fdebd0", "#fadbd8")),
                       fontWeight = "bold")
  })
  
  # --- Factor Importance Plot ---
  output$importance_plot <- plotly::renderPlotly({
    result <- analysis_data()
    if (is.null(result) || is.null(result$causal)) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "Click 'Run Analysis' to begin"))
    }
    
    imp <- result$causal$importance
    if (nrow(imp) == 0) {
      return(plotly::plot_ly() %>% plotly::layout(title = "No factor data"))
    }
    
    p <- ggplot(imp, aes(x = reorder(factor, abs(correlation)), y = correlation, fill = correlation)) +
      geom_col(alpha = 0.85) +
      geom_text(aes(label = sprintf("r=%.2f %s", correlation, significance)),
                hjust = ifelse(imp$correlation > 0, -0.1, 1.1), size = 3) +
      scale_fill_gradient2(low = "#e74c3c", mid = "#f1c40f", high = "#27ae60", midpoint = 0) +
      coord_flip() +
      labs(x = NULL, y = "Spearman r", fill = "r") +
      theme_minimal() +
      theme(text = element_text(size = 11), legend.position = "none")
    
    plotly::ggplotly(p, tooltip = c("y"))
  })
  
  # --- Score Distribution Histogram ---
  output$score_histogram <- plotly::renderPlotly({
    result <- analysis_data()
    if (is.null(result)) {
      return(plotly::plot_ly() %>% plotly::layout(title = "No data"))
    }
    
    data <- result$risk_data
    p <- ggplot(data, aes(x = composite_score, fill = performance_tier)) +
      geom_histogram(binwidth = 0.05, boundary = 0, color = "white", alpha = 0.85) +
      scale_fill_manual(values = c("High" = "#27ae60", "Medium" = "#f39c12", "Low" = "#e74c3c")) +
      labs(x = "Composite Score", y = "Count", fill = "Tier") +
      theme_minimal() +
      theme(text = element_text(size = 11), legend.position = "bottom")
    
    plotly::ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # --- Summary Boxes ---
  output$analysis_summary_boxes <- renderUI({
    result <- analysis_data()
    if (is.null(result)) {
      return(div(style = "padding:20px; text-align:center; color:#888;",
                 h4("Click 'Run Analysis'")))
    }
    
    data <- result$risk_data
    cov <- result$coverage
    n_traps <- nrow(data)
    n_high <- sum(data$performance_tier == "High")
    n_low  <- sum(data$performance_tier == "Low")
    avg_score <- round(mean(data$composite_score, na.rm = TRUE), 3)
    total_pools <- sum(as.numeric(data$total_pools), na.rm = TRUE)
    total_positive <- sum(as.numeric(data$total_positive), na.rm = TRUE)
    
    n_gap_areas <- if (!is.null(cov)) sum(cov$coverage_grade %in% c("Gap", "Thin")) else 0
    n_good_areas <- if (!is.null(cov)) sum(cov$coverage_grade == "Good") else 0
    
    year_label <- if (!is.null(input$analysis_year) && input$analysis_year != "all") {
      input$analysis_year
    } else "All Years"
    
    surface_gaps <- if (!is.null(result$surface)) {
      sum(result$surface$is_gap)
    } else 0
    surface_total <- if (!is.null(result$surface)) nrow(result$surface) else 1
    gap_pct <- round(100 * surface_gaps / max(surface_total, 1), 0)
    
    div(
      div(style = "text-align:center; margin-bottom:6px;",
          h5(strong(year_label), style = "color:#666; margin:0;")),
      div(style = "display:grid; grid-template-columns:1fr 1fr; gap:6px;",
        div(style = "background:#ecf0f1; padding:10px; border-radius:5px; text-align:center;",
            h4(n_traps, style = "margin:0; color:#2c3e50;"),
            p("Traps", style = "margin:0; font-size:10px; color:#888;")),
        div(style = "background:#d5f5e3; padding:10px; border-radius:5px; text-align:center;",
            h4(n_high, style = "margin:0; color:#27ae60;"),
            p("High", style = "margin:0; font-size:10px; color:#888;")),
        div(style = "background:#fadbd8; padding:10px; border-radius:5px; text-align:center;",
            h4(n_low, style = "margin:0; color:#e74c3c;"),
            p("Low", style = "margin:0; font-size:10px; color:#888;")),
        div(style = paste0("background:", ifelse(n_gap_areas > 0, "#fadbd8", "#d5f5e3"),
                           "; padding:10px; border-radius:5px; text-align:center;"),
            h4(n_gap_areas, style = paste0("margin:0; color:", ifelse(n_gap_areas > 0, "#e74c3c", "#27ae60"), ";")),
            p("Gap Areas", style = "margin:0; font-size:10px; color:#888;"))
      ),
      hr(style = "margin:6px 0;"),
      div(style = "font-size:12px;",
        p(strong("Avg Score: "), sprintf("%.3f", avg_score), style = "margin:2px 0;"),
        p(strong("Pools: "), format(total_pools, big.mark = ","), style = "margin:2px 0;"),
        p(strong("Positive: "), format(total_positive, big.mark = ","),
          sprintf(" (%.1f%%)", ifelse(total_pools > 0, total_positive / total_pools * 100, 0)),
          style = "margin:2px 0;"),
        p(strong("Coverage Gaps: "), sprintf("%d%% of grid", gap_pct), style = "margin:2px 0;"),
        p(strong("Good Areas: "), n_good_areas, style = "margin:2px 0;")
      )
    )
  })
  
  # --- Dose-Response Helper ---
  make_dr_plot <- function(result, factor_name, x_label) {
    if (is.null(result) || is.null(result$causal) || is.null(result$causal$dose_response[[factor_name]])) {
      return(plotly::plot_ly() %>% plotly::layout(title = "No data"))
    }
    
    dr <- result$causal$dose_response[[factor_name]]
    raw <- result$risk_data
    
    factor_col <- switch(factor_name,
      "Pools Collected"  = as.numeric(raw$total_pools),
      "Positive Pools"   = as.numeric(raw$total_positive),
      "Avg Yield/Week"   = as.numeric(raw$avg_per_week),
      "Weeks Active"     = as.numeric(raw$weeks_active),
      "Total Mosquitoes" = as.numeric(raw$total_mosq)
    )
    
    p <- ggplot() +
      geom_point(data = data.frame(x = factor_col, y = raw$composite_score),
                 aes(x = x, y = y), alpha = 0.3, color = "#7f8c8d", size = 1.5) +
      geom_point(data = dr, aes(x = bin_midpoint, y = avg_score),
                 color = "#2c5aa0", size = 3) +
      geom_line(data = dr, aes(x = bin_midpoint, y = avg_score),
                color = "#2c5aa0", linewidth = 1) +
      geom_ribbon(data = dr, aes(x = bin_midpoint,
                                  ymin = pmax(avg_score - 1.96 * se_score, 0),
                                  ymax = pmin(avg_score + 1.96 * se_score, 1)),
                  fill = "#2c5aa0", alpha = 0.15) +
      labs(x = x_label, y = "Avg Score") +
      theme_minimal() +
      theme(text = element_text(size = 11))
    
    plotly::ggplotly(p, tooltip = c("x", "y"))
  }
  
  output$dr_pools_plot <- plotly::renderPlotly({
    make_dr_plot(analysis_data(), "Pools Collected", "Total Pools Collected")
  })
  
  output$dr_positives_plot <- plotly::renderPlotly({
    make_dr_plot(analysis_data(), "Positive Pools", "Total Positive Pools")
  })
  
  # --- Scorecard + Recommendations Table ---
  output$analysis_table <- DT::renderDT({
    result <- analysis_data()
    if (is.null(result)) {
      return(DT::datatable(data.frame(Message = "Click 'Run Analysis' to begin")))
    }
    
    data <- result$risk_data
    recs <- if (!is.null(result$causal$recommendations)) {
      result$causal$recommendations %>% dplyr::select(loc_code, recommendation)
    } else {
      data.frame(loc_code = character(0), recommendation = character(0))
    }
    
    display <- data %>%
      dplyr::left_join(recs, by = "loc_code") %>%
      dplyr::select(
        `Trap` = loc_code,
        `VI Area` = viarea,
        `Weeks` = weeks_active,
        `Avg/Wk` = avg_per_week,
        `Pools` = total_pools,
        `Pos.` = total_positive,
        `Yield` = yield_score,
        `Testing` = testing_score,
        `Detection` = detection_score,
        `Consist.` = consistency_score,
        `Score` = composite_score,
        `Risk` = risk_index,
        `Tier` = performance_tier,
        `Recommendation` = recommendation
      ) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3))) %>%
      dplyr::arrange(desc(Score))
    
    DT::datatable(display,
                  options = list(pageLength = 25, scrollX = TRUE,
                                 order = list(list(10, "desc"))),
                  caption = sprintf("Trap Scorecard — %s",
                                    if (!is.null(input$analysis_year) && input$analysis_year != "all")
                                      input$analysis_year else "All Years")) %>%
      DT::formatStyle("Tier",
                       backgroundColor = DT::styleEqual(
                         c("High", "Medium", "Low"),
                         c("#d5f5e3", "#fdebd0", "#fadbd8"))) %>%
      DT::formatStyle("Score",
                       background = DT::styleColorBar(range(display$Score, na.rm = TRUE), "#3498db"),
                       backgroundSize = "98% 88%",
                       backgroundRepeat = "no-repeat",
                       backgroundPosition = "center") %>%
      DT::formatStyle("Recommendation",
                       fontStyle = "italic", fontSize = "11px")
  })
  
  # Download analysis data
  output$download_analysis_data <- downloadHandler(
    filename = function() {
      sprintf("trap_analysis_%s_%s.csv",
              input$analysis_year %||% "all", Sys.Date())
    },
    content = function(file) {
      tryCatch({
        result <- analysis_data()
        if (!is.null(result) && !is.null(result$risk_data)) {
          export_data <- result$risk_data %>%
            dplyr::select(loc_code, viarea, years_active, weeks_active,
                          total_mosq, avg_per_week, total_pools, total_positive,
                          total_tested, positivity_rate_pct,
                          yield_score, testing_score, detection_score,
                          consistency_score, composite_score, performance_tier,
                          spatial_risk, risk_index, risk_tier,
                          lon, lat)
          write.csv(export_data, file, row.names = FALSE, na = "")
        } else {
          write.csv(data.frame(Message = "No data"), file, row.names = FALSE)
        }
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Download failed:", e$message)), file, row.names = FALSE)
      })
    }
  )
  
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
