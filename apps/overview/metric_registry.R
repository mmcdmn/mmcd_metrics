# Overview Metric Registry
# =============================================================================
# SINGLE SOURCE OF TRUTH for all overview metrics.
# 
# To add a new metric:
#   1. Add a new entry to get_metric_registry() below
#   2. Create corresponding load_raw_data() function in the app's data_functions.R
#   3. That's it! The UI and server will automatically iterate through this list.
#
# Each metric defines:
#   - display_name: Title shown in UI
#   - icon: FontAwesome icon name  
#   - y_label: Label for chart Y-axis
#   - bg_color: Color for stat box
#   - app_folder: Folder containing data_functions.R
#   - has_acres: Whether this metric uses acres (for historical)
#   - historical_enabled: Whether to show historical charts
# =============================================================================

# Ensure shiny is available for HTML()
if (!requireNamespace("shiny", quietly = TRUE)) {
  HTML <- function(x) x
} else {
  HTML <- shiny::HTML
}

# =============================================================================
# CONFIGURATION HELPERS
# =============================================================================

#' Get the year range for historical data
#' @param n_years Number of years to include (default 5, includes current year)
#' @return List with start_year and end_year
#' @export
get_historical_year_range <- function(n_years = 5) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  list(
    start_year = current_year - n_years + 1,
    end_year = current_year
  )
}

#' Get the path to the apps folder
#' Works from: apps/overview/district, apps/overview/facilities, apps/overview, apps/district_overview
#' @return Character string path to apps folder
#' @export
get_apps_base_path <- function() {
  # Check relative paths - use any known app folder (drone is simple name)
  # When running from apps/overview/district or apps/overview/facilities
  if (file.exists("../../drone/data_functions.R")) return("../..")
  # When running from apps/overview or apps/district_overview
  if (file.exists("../drone/data_functions.R")) return("..")
  # When running on server
  if (file.exists("/srv/shiny-server/apps/drone/data_functions.R")) return("/srv/shiny-server/apps")
  # Fallback
  return("../..")
}

# =============================================================================
# METRIC DEFINITIONS - EDIT THIS LIST TO ADD/REMOVE METRICS
# =============================================================================

#' Get the registry of all available metrics
#' THIS IS THE ONLY PLACE TO ADD/REMOVE METRICS
#' 
#' @return List of metric configurations
#' @export
get_metric_registry <- function() {
  list(
    catch_basin = list(
      id = "catch_basin",
      display_name = "Catch Basins",
      short_name = "CB",
      icon = "tint",
      image_path = "assets/catchbasin.png",
      y_label = "Active Catch Basins",
      bg_color = "#667eea",
      app_folder = "catch_basin_status",
      has_acres = FALSE,
      historical_enabled = TRUE,
      use_active_calculation = TRUE,  # Calculate ACTIVE treatments per week (using effect_days)
      display_metric = "treatments",  # count of treatments
      filter_info = HTML("<b>Filters Applied:</b><br>
                         • Wet catch basins only (status_udw = 'W')<br>
                         • All facilities<br>
                         • Zone filter from dropdown"),
      load_params = list(expiring_days = 7)
    ),
    
    drone = list(
      id = "drone",
      display_name = "Drone Sites",
      short_name = "Drone",
      icon = "helicopter",
      image_path = "assets/drone.jpg",
      y_label = "Active Drone Acres",
      bg_color = "#f5576c",
      app_folder = "drone",
      has_acres = TRUE,
      historical_enabled = TRUE,
      use_active_calculation = TRUE,  # Calculate ACTIVE treatments per week
      display_metric = "treatment_acres",  # use acres for historical
      filter_info = HTML("<b>Filters Applied:</b><br>
                         • Drone types: Y, M, C<br>
                         • All facilities, all foremen<br>
                         • Prehatch only: YES<br>
                         • Zone filter from dropdown"),
      load_params = list(expiring_days = 7)
    ),
    
    ground_prehatch = list(
      id = "ground_prehatch",
      display_name = "Ground Prehatch",
      short_name = "Ground",
      icon = "seedling",
      image_path = "assets/ground.png",
      y_label = "Active Prehatch Acres",
      bg_color = "#4facfe",
      app_folder = "ground_prehatch_progress",
      has_acres = TRUE,
      historical_enabled = TRUE,
      use_active_calculation = TRUE,  # Calculate ACTIVE treatments per week
      display_metric = "treatment_acres",  # use acres for historical
      filter_info = HTML("<b>Filters Applied:</b><br>
                         • Prehatch sites only<br>
                         • All facilities<br>
                         • Zone filter from dropdown"),
      load_params = list(expiring_days = 7)
    ),
    
    structure = list(
      id = "structure",
      display_name = "Structures",
      short_name = "Struct",
      icon = "building",
      image_path = "assets/layer-group.jpg",
      y_label = "Active Structures",
      bg_color = "#43e97b",
      app_folder = "struct_trt",
      has_acres = FALSE,
      historical_enabled = TRUE,
      use_active_calculation = TRUE,  # Calculate ACTIVE treatments per week
      display_metric = "treatments",  # count of treatments
      filter_info = HTML("<b>Filters Applied:</b><br>
                         • Structure types: All<br>
                         • Status: W, U (Wet, Unknown)<br>
                         • Priority: All<br>
                         • Zone filter from dropdown"),
      load_params = list(expiring_days = 7)
    ),
    
    cattail_treatments = list(
      id = "cattail_treatments",
      display_name = "Cattail Treatments", 
      short_name = "Cattail",
      icon = "spa",
      y_label = "Cattail Acres",
      bg_color = "#ff9500",
      app_folder = "cattail_treatments",
      has_acres = TRUE,
      historical_enabled = TRUE,
      historical_type = "yearly_grouped",  # Yearly grouped bars by facility/MMCD
      historical_group_by = "facility",    # Group by facility for facilities overview, MMCD for district
      historical_year_column = "inspection_year",  # Use this column for year if present (seasonal logic)
      use_active_calculation = TRUE,       # Sites needing treatment or treated
      display_metric = "sites",            # count of active sites
      filter_info = HTML("<b>Filters Applied:</b><br>
                         • Inspected cattail sites only<br>
                         • Treated: Sites that have been treated<br>
                         • Needs Treatment: Sites requiring treatment<br>
                         • All facilities<br>
                         • Zone filter from dropdown"),
      load_params = list(expiring_days = 30)  # Cattail treatments are seasonal
    )
  )
}

#' Get the list of active metrics to display
#' Returns the metric IDs in display order
#' 
#' @return Character vector of metric IDs
#' @export
get_active_metrics <- function() {
  # Returns IDs of metrics to display - order matters!
  names(get_metric_registry())
}

#' Get metrics that have historical enabled
#' @return Character vector of metric IDs with historical charts
#' @export
get_historical_metrics <- function() {
  registry <- get_metric_registry()
  names(registry)[sapply(registry, function(m) isTRUE(m$historical_enabled))]
}

#' Get a single metric config by ID
#' 
#' @param metric_id The metric ID
#' @return Metric configuration list or NULL
#' @export
get_metric_config <- function(metric_id) {
  registry <- get_metric_registry()
  registry[[metric_id]]
}

#' Add a new metric to the registry
#' Call this from your app to register custom metrics
#' 
#' @param id Unique metric identifier
#' @param display_name Display name for UI
#' @param icon FontAwesome icon name
#' @param y_label Y-axis label for charts
#' @param bg_color Background color for stat box
#' @param filter_info HTML description of filters
#' @param load_params List of parameters for data loading
#' @return The metric configuration
#' @export
create_metric_config <- function(id, display_name, icon, y_label, bg_color, 
                                  filter_info = "", load_params = list()) {
  list(
    id = id,
    display_name = display_name,
    short_name = display_name,
    icon = icon,
    y_label = y_label,
    bg_color = bg_color,
    filter_info = if (is.character(filter_info)) HTML(filter_info) else filter_info,
    load_params = load_params
  )
}

# =============================================================================
# OVERVIEW TYPES
# =============================================================================

#' Get configuration for an overview type
#' 
#' @param type One of: "district", "facilities", "fos"
#' @return Configuration list for the overview type
#' @export
get_overview_config <- function(type) {
  configs <- list(
    district = list(
      title = "District Overview Dashboard",
      subtitle = "All MMCD treatment progress by zone - Click a bar to drill down to Facilities",
      group_by = "zone",
      load_function = "load_data_by_zone",
      chart_function = "create_zone_chart",
      enable_drill_down = TRUE,
      drill_down_target = "facilities_overview",
      historical_type = "weekly",     # weekly line charts
      historical_group_by = "mmcd_all" # single line, all MMCD combined
    ),
    
    facilities = list(
      title = "Facilities Overview Dashboard",
      subtitle = "Compare treatment progress across all facilities",
      group_by = "facility",
      load_function = "load_data_by_facility",
      chart_function = "create_overview_chart",
      enable_drill_down = FALSE,
      drill_down_target = NULL,
      historical_type = "yearly",     # yearly stacked bars
      historical_group_by = "facility" # bars grouped by facility
    ),
    
    fos = list(
      title = "FOS Overview Dashboard",
      subtitle = "Field Operations Supervisor treatment progress",
      group_by = "fos",
      load_function = "load_data_by_fos",
      chart_function = "create_overview_chart",
      enable_drill_down = FALSE,
      drill_down_target = NULL,
      historical_type = "yearly",  # yearly stacked bars
      historical_group_by = "fos"
    )
  )
  
  configs[[type]]
}

# =============================================================================
# DYNAMIC UI GENERATORS - Iterate through registry automatically
# =============================================================================

#' Generate metric chart UI elements dynamically from registry
#' @param metrics Vector of metric IDs to include (defaults to all active)
#' @param chart_height Height of each chart
#' @return List of UI elements (fluidRow with chart panels)
#' @export
generate_metric_charts_ui <- function(metrics = get_active_metrics(), chart_height = "350px") {
  registry <- get_metric_registry()
  
  # Create a chart panel for each metric
  chart_panels <- lapply(metrics, function(metric_id) {
    config <- registry[[metric_id]]
    if (is.null(config)) return(NULL)
    
    column(6,
      div(class = "chart-panel",
        style = "background: white; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
        h4(config$display_name, style = "margin-top: 0;"),
        plotlyOutput(paste0(metric_id, "_chart"), height = chart_height)
      )
    )
  })
  
  # Arrange in rows of 2
  chart_rows <- split(chart_panels, ceiling(seq_along(chart_panels) / 2))
  lapply(chart_rows, function(row_panels) {
    do.call(fluidRow, row_panels)
  })
}

#' Generate historical chart UI elements dynamically from registry
#' @param overview_type One of: "district", "facilities", "fos"
#' @param chart_height Height of each chart
#' @return List of UI elements
#' @export
generate_historical_charts_ui <- function(overview_type = "district", chart_height = "300px") {
  metrics <- get_historical_metrics()
  registry <- get_metric_registry()
  
  # Create a chart panel for each metric with historical enabled
  chart_panels <- lapply(metrics, function(metric_id) {
    config <- registry[[metric_id]]
    if (is.null(config)) return(NULL)
    
    column(6,
      div(class = "chart-panel historical-chart",
        style = "background: white; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
        h5(paste(config$display_name, "- Historical"), style = "margin-top: 0; color: #666;"),
        plotlyOutput(paste0(metric_id, "_historical_chart"), height = chart_height)
      )
    )
  })
  
  # Arrange in rows of 2
  chart_rows <- split(chart_panels, ceiling(seq_along(chart_panels) / 2))
  
  # Add section header
  c(
    list(
      fluidRow(
        column(12,
          h3("Historical Trends (Last 5 Years)", 
             style = "margin-top: 30px; margin-bottom: 20px; border-bottom: 2px solid #eee; padding-bottom: 10px;")
        )
      )
    ),
    lapply(chart_rows, function(row_panels) {
      do.call(fluidRow, row_panels)
    })
  )
}

#' Generate summary stat boxes dynamically from registry
#' @param data Named list of data frames keyed by metric_id
#' @return UI element with stat boxes
#' @export
generate_summary_stats_ui <- function(data) {
  metrics <- get_active_metrics()
  registry <- get_metric_registry()
  
  stat_boxes <- lapply(metrics, function(metric_id) {
    config <- registry[[metric_id]]
    metric_data <- data[[metric_id]]
    
    # Calculate stats
    if (!is.null(metric_data) && nrow(metric_data) > 0) {
      total <- sum(metric_data$total, na.rm = TRUE)
      active <- sum(metric_data$active, na.rm = TRUE)
      pct <- ceiling(100 * active / max(1, total))
    } else {
      total <- 0
      active <- 0
      pct <- 0
    }
    
    column(12 / length(metrics),
      create_stat_box(
        value = paste0(pct, "%"),
        title = paste0(config$display_name, ": ", format(active, big.mark = ","),
                      " / ", format(total, big.mark = ","), " treated"),
        bg_color = config$bg_color,
        icon = if (!is.null(config$image_path)) config$image_path else config$icon,
        icon_type = if (!is.null(config$image_path)) "image" else "fontawesome"
      )
    )
  })
  
  do.call(fluidRow, stat_boxes)
}

# =============================================================================
# DYNAMIC SERVER HELPERS - Iterate through registry automatically
# =============================================================================

#' Load all metric data dynamically from registry
#' @param inputs List of input values (zone_filter, custom_today, expiring_days, separate_zones)
#' @param load_function Function to use for loading (load_data_by_zone, load_data_by_facility, etc.)
#' @return Named list of data frames keyed by metric_id
#' @export
load_all_metrics_dynamic <- function(inputs, load_function) {
  metrics <- get_active_metrics()
  registry <- get_metric_registry()
  
  results <- setNames(
    lapply(metrics, function(metric_id) {
      tryCatch({
        load_function(
          metric = metric_id,
          analysis_date = inputs$custom_today,
          expiring_days = inputs$expiring_days,
          zone_filter = inputs$zone_filter,
          separate_zones = inputs$separate_zones
        )
      }, error = function(e) {
        cat("ERROR loading", metric_id, ":", e$message, "\n")
        data.frame()
      })
    }),
    metrics
  )
  
  results
}

#' Load all historical data dynamically from registry
#' @param overview_type One of: "district", "facilities", "fos"
#' @param zone_filter Vector of zones
#' @return Named list of data frames keyed by metric_id
#' @export
load_all_historical_dynamic <- function(overview_type, zone_filter = c("1", "2")) {
  metrics <- get_historical_metrics()
  registry <- get_metric_registry()
  overview_config <- get_overview_config(overview_type)
  
  # Determine time_period and group_by from overview config
  time_period <- if (!is.null(overview_config$historical_type)) overview_config$historical_type else "weekly"
  group_by <- if (!is.null(overview_config$historical_group_by)) overview_config$historical_group_by else "mmcd_all"
  
  years <- get_historical_year_range(5)
  
  results <- setNames(
    lapply(metrics, function(metric_id) {
      config <- registry[[metric_id]]
      
      tryCatch({
        load_historical_metric_data(
          metric = metric_id,
          start_year = years$start_year,
          end_year = years$end_year,
          group_by = group_by,
          time_period = time_period,
          display_metric = config$display_metric,
          zone_filter = zone_filter
        )
      }, error = function(e) {
        cat("ERROR loading historical", metric_id, ":", e$message, "\n")
        data.frame()
      })
    }),
    metrics
  )
  
  results
}

#' Create render outputs for all metric charts dynamically
#' @param output Shiny output object
#' @param data_reactive Reactive that returns named list of data
#' @param theme_reactive Reactive that returns current theme
#' @param chart_function Function to create charts
#' @export
setup_metric_chart_outputs <- function(output, data_reactive, theme_reactive, chart_function) {
  metrics <- get_active_metrics()
  registry <- get_metric_registry()
  
  lapply(metrics, function(metric_id) {
    config <- registry[[metric_id]]
    output_id <- paste0(metric_id, "_chart")
    
    output[[output_id]] <- renderPlotly({
      req(data_reactive())
      data <- data_reactive()[[metric_id]]
      
      if (is.null(data) || nrow(data) == 0) {
        return(create_empty_chart(config$display_name, "No data available"))
      }
      
      chart_function(
        data = data,
        title = config$display_name,
        y_label = config$y_label,
        theme = theme_reactive(),
        metric_type = metric_id
      )
    })
  })
}

#' Create render outputs for all historical charts dynamically
#' @param output Shiny output object
#' @param data_reactive Reactive that returns named list of historical data
#' @param overview_type One of: "district", "facilities", "fos"
#' @export
setup_historical_chart_outputs <- function(output, data_reactive, overview_type) {
  metrics <- get_historical_metrics()
  registry <- get_metric_registry()
  overview_config <- get_overview_config(overview_type)
  
  chart_type <- if (!is.null(overview_config$historical_type) && overview_config$historical_type == "yearly") {
    "bar"
  } else {
    "line"
  }
  
  lapply(metrics, function(metric_id) {
    config <- registry[[metric_id]]
    output_id <- paste0(metric_id, "_historical_chart")
    
    output[[output_id]] <- renderPlotly({
      req(data_reactive())
      data <- data_reactive()[[metric_id]]
      
      if (is.null(data) || nrow(data) == 0) {
        return(create_empty_chart(paste(config$display_name, "Historical"), "No historical data"))
      }
      
      y_label <- if (config$has_acres) paste(config$short_name, "Acres") else config$y_label
      
      if (chart_type == "bar") {
        create_historical_bar_chart(data, config$display_name, y_label, config$bg_color)
      } else {
        create_historical_line_chart(data, config$display_name, y_label, config$bg_color)
      }
    })
  })
}
