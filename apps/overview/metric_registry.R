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
  # When running from workspace root
  if (file.exists("apps/drone/data_functions.R")) return("apps")
  # Docker container path
  if (file.exists("/srv/shiny-server/drone/data_functions.R")) return("/srv/shiny-server")
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
    # =========================================================================
    # VECTOR CATEGORY - Catch basins, structures, SUCO
    # =========================================================================
    catch_basin = list(
      id = "catch_basin",
      display_name = "Catch Basins",
      short_name = "CB",
      icon = "tint",
      image_path = "assets/catchbasin.png",
      category = "Vector",
      y_label = "Active Catch Basins",
      bg_color = "#667eea",
      app_folder = "catch_basin_status",
      has_acres = FALSE,
      historical_enabled = TRUE,
      use_active_calculation = TRUE,  # Calculate ACTIVE treatments per week (using effect_days)
      display_metric = "treatments",  # count of treatments
      chart_types = c("bar", "pie"),  # Support both bar and pie charts
      default_chart_type = "bar",     # Default to bar chart
      # Detail boxes shown when drilling down to facility level
      # Uses columns from load_data_by_facility: total, active, expiring
      detail_boxes = list(
        list(id = "total", title = "Wet CBs", column = "total", icon = "tint", status = "completed"),
        list(id = "active", title = "Treated", column = "active", icon = "check-circle", status = "active"),
        list(id = "expiring", title = "Expiring", column = "expiring", icon = "exclamation-triangle", status = "planned")
      ),
      filter_info = HTML("<b>Filters Applied:</b><br>
                         • Wet catch basins only (status_udw = 'W')<br>
                         • All facilities<br>
                         • Zone filter from dropdown"),
      load_params = list(expiring_days = 7)
    ),
    
    # =========================================================================
    # FLOODWATER CATEGORY - Drone, Ground Prehatch
    # =========================================================================
    drone = list(
      id = "drone",
      display_name = "Drone Acres",
      short_name = "Drone",
      icon = "helicopter",
      image_path = "assets/drone.jpg",
      category = "Floodwater",
      y_label = "Active Drone Acres",
      bg_color = "#f5576c",
      app_folder = "drone",
      has_acres = TRUE,
      historical_enabled = TRUE,
      use_active_calculation = TRUE,  # Calculate ACTIVE treatments per week
      display_metric = "treatment_acres",  # use acres for historical
      chart_types = c("bar", "pie"),  # Support both bar and pie charts
      default_chart_type = "bar",     # Default to bar chart
      # Detail boxes shown when drilling down to facility level
      # Uses columns from load_data_by_facility: total, active, expiring (as acres for this metric)
      detail_boxes = list(
        list(id = "total", title = "Total Acres", column = "total", icon = "expand", status = "completed"),
        list(id = "active", title = "Active Acres", column = "active", icon = "check-circle", status = "active"),
        list(id = "expiring", title = "Expiring Acres", column = "expiring", icon = "exclamation-triangle", status = "planned")
      ),
      filter_info = HTML("<b>Filters Applied:</b><br>
                         • Drone types: Y, M, C<br>
                         • All facilities, all foremen<br>
                         • Prehatch only: YES<br>
                         • Zone filter from dropdown"),
      load_params = list(expiring_days = 7)
    ),
    
    ground_prehatch = list(
      id = "ground_prehatch",
      display_name = "Ground Prehatch Acres",
      short_name = "Ground",
      icon = "seedling",
      image_path = "assets/ground.png",
      category = "Floodwater",
      y_label = "Active Prehatch Acres",
      bg_color = "#207010",
      app_folder = "ground_prehatch_progress",
      has_acres = TRUE,
      historical_enabled = TRUE,
      use_active_calculation = TRUE,  # Calculate ACTIVE treatments per week
      display_metric = "treatment_acres",  # use acres for historical
      chart_types = c("bar", "pie"),  # Support both bar and pie charts
      default_chart_type = "bar",     # Default to bar chart
      # Detail boxes shown when drilling down to facility level
      # Uses columns from load_data_by_facility: total, active, expiring
      detail_boxes = list(
        list(id = "total", title = "Total Acres", column = "total", icon = "egg", status = "completed"),
        list(id = "active", title = "Active Acres", column = "active", icon = "check-circle", status = "active"),
        list(id = "expiring", title = "Expiring Acres", column = "expiring", icon = "exclamation-triangle", status = "planned")
      ),
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
      category = "Vector",
      y_label = "Active Structures",
      bg_color = "#3851db",
      app_folder = "struct_trt",
      has_acres = FALSE,
      historical_enabled = TRUE,
      use_active_calculation = TRUE,  # Calculate ACTIVE treatments per week
      display_metric = "treatments",  # count of treatments
      chart_types = c("bar", "pie"),  # Support both bar and pie charts
      default_chart_type = "bar",     # Default to bar chart
      # Detail boxes shown when drilling down to facility level
      detail_boxes = list(
        list(id = "total", title = "Total Sites", column = "total", icon = "map-marker-alt", status = "completed"),
        list(id = "active", title = "Treated", column = "active", icon = "check-circle", status = "active"),
        list(id = "expiring", title = "Expiring", column = "expiring", icon = "exclamation-triangle", status = "planned")
      ),
      filter_info = HTML("<b>Filters Applied:</b><br>
                         • Structure types: All<br>
                         • Status: W, U (Wet, Unknown)<br>
                         • Priority: All<br>
                         • Zone filter from dropdown"),
      load_params = list(expiring_days = 7)
    ),
    
    # =========================================================================
    # CATTAIL CATEGORY - Treatments and Inspections
    # =========================================================================
    cattail_treatments = list(
      id = "cattail_treatments",
      display_name = "Cattail Treatments",
      category = "Cattail", 
      short_name = "Cattail",
      icon = "spa",
      image_path = "assets/cattail_background.png",
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
      # Detail boxes shown when drilling down to facility level
      detail_boxes = list(
        list(id = "treated", title = "Acres Treated", column = "treated", icon = "check-circle", status = "active"),
        list(id = "expiring", title = "Acres Need Treatment", column = "expiring", icon = "exclamation-triangle", status = "needs_treatment")
      ),
      filter_info = HTML("<b>Filters Applied:</b><br>
                         • Inspected cattail sites only<br>
                         • Treated: Sites that have been treated<br>
                         • Needs Treatment: Sites requiring treatment<br>
                         • All facilities<br>
                         • Zone filter from dropdown"),
      load_params = list(expiring_days = 30)  # Cattail treatments are seasonal
    ),

    # =========================================================================
    # ADULT SAMPLES CATEGORY - Mosquito surveillance
    # =========================================================================
    mosquito_monitoring = list(
      id = "mosquito_monitoring",
      display_name = "avg mosquitoes per trap",
      short_name = "Mosquito",
      icon = "bug",
      image_path = "assets/adult.png",
      category = "Adult Samples",
      y_label = "Avg Mosquitoes per Trap",
      bg_color = "#10b981",
      app_folder = "mosquito-monitoring",
      has_acres = FALSE,
      historical_enabled = TRUE,  # Enable historical trending
      # Display configuration - makes behavior generic (no special case checks needed)
      display_as_average = TRUE,  # Show avg values instead of percentages
      aggregate_as_average = TRUE,  # Use mean instead of sum for weekly aggregation
      # Detail boxes shown when drilling down to facility level
      detail_boxes = list(
        list(id = "historical", title = "10-Year Average", column = "total", icon = "history", status = "completed"),
        list(id = "current", title = "Current Avg/Trap", column = "active", icon = "bug", status = "active")
      ),
      chart_labels = list(
        total = "10-Year Average",
        active = "avg per trap",
        expiring = "Above Average"
      ),
      filter_info = HTML("<b>Filters Applied:</b><br>
                         • CO2 trap counts (dbadult_mon_nt_co2_tall2_forr)<br>
                         • Species: Total Ae + Cq (combined Aedes)<br>
                         • Current week vs 10-year average for same week<br>
                         • All facilities<br>
                         • Zone filter from dropdown"),
      load_params = list(expiring_days = 30, species_filter = "Total_Ae_+_Cq")
    ),
    
    suco = list(
      id = "suco",
      display_name = "SUCO Capacity",
      short_name = "SUCO",
      icon = "search",
      image_path = "assets/bucket.png",  # Use adult mosquito icon
      category = "Adult Samples",
      y_label = "SUCOs Completed",
      bg_color = "#6366f1",  # Indigo color
      app_folder = "suco_history",
      has_acres = FALSE,
      historical_enabled = FALSE,  # Can enable later with weekly historical data
      use_active_calculation = FALSE,  # SUCOs use count-based progress
      display_metric = "inspections",  # count of SUCO inspections
      display_as_average = TRUE,  # Show capacity-style display (not percentage)
      chart_types = c("bar"),
      default_chart_type = "bar",
      # Detail boxes shown when drilling down to facility level
      detail_boxes = list(
        list(id = "capacity", title = "Weekly Capacity", column = "total", icon = "chart-line", status = "completed"),
        list(id = "completed", title = "Completed", column = "active", icon = "check-circle", status = "active")
      ),
      chart_labels = list(
        total = "Weekly Capacity",
        active = "Completed",
        expiring = "Above Capacity"
      ),
      filter_info = HTML("<b>Filters Applied:</b><br>
                         • SUCO inspections only (survtype = 7)<br>
                         • Current week (Monday through today)<br>
                         • All facilities<br>
                         • Zone filter from dropdown"),
      load_params = list(
        capacity_total = 72,  # District-wide capacity: 72 SUCOs per week
        capacity_per_facility = 72 / 7,  # Per-facility capacity
        time_period = "current_week"  # Use current week for progress
      )
    ),
    
    cattail_inspections = list(
      id = "cattail_inspections",
      display_name = "Cattail Inspections Progress",
      short_name = "Cat Insp",
      icon = "tasks",
      category = "Cattail",
      y_label = "Sites Inspected vs Goal",
      bg_color = "#8b5cf6",  # Purple color
      app_folder = "cattail_inspections",
      has_acres = FALSE,
      historical_enabled = FALSE,  # Progress vs goal is yearly, not weekly trending
      use_active_calculation = FALSE,  # Uses goal-based progress
      display_metric = "progress",  # progress toward goal
      chart_types = c("bar"),
      default_chart_type = "bar",
      # Detail boxes shown when drilling down to facility level
      detail_boxes = list(
        list(id = "goal", title = "Goal", column = "total", icon = "bullseye", status = "completed"),
        list(id = "inspected", title = "Inspected", column = "active", icon = "check-circle", status = "active"),
        list(id = "remaining", title = "Remaining", column = "expiring", icon = "clock", status = "planned")
      ),
      chart_labels = list(
        total = "Goal",
        active = "Inspected",
        expiring = "Remaining"
      ),
      filter_info = HTML("<b>Filters Applied:</b><br>
                         • Cattail inspections (action = '9')<br>
                         • Unique sites only<br>
                         • Season: Aug-Dec<br>
                         • Zone filter (P1, P2, Total)<br>
                         • Year from dropdown"),
      load_params = list(
        zone_filter = c("1", "2"),  # Default to both zones (total)
        time_period = "yearly"  # Yearly progress tracking
      )
    )
  )
}

#' Get the ordered list of metric categories
#' Categories are displayed in this order in the UI
#' @return Character vector of category names
#' @export
get_metric_categories <- function() {
  c("Floodwater", "Vector", "Adult Samples", "Cattail")
}

#' Get metrics by category
#' @param category The category name (e.g., "Floodwater", "Vector")
#' @return Character vector of metric IDs in that category
#' @export
get_metrics_by_category <- function(category) {
  registry <- get_metric_registry()
  names(registry)[sapply(registry, function(m) {
    isTRUE(m$category == category)
  })]
}

#' Get all metrics grouped by category
#' @return Named list with categories as keys and metric ID vectors as values
#' @export
get_metrics_grouped_by_category <- function() {
  categories <- get_metric_categories()
  setNames(
    lapply(categories, get_metrics_by_category),
    categories
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

#' Get detail boxes configuration for a metric
#' Returns the list of detail box definitions for drill-down views
#' 
#' @param metric_id The metric ID
#' @return List of detail box configurations or NULL if not defined
#' @export
get_metric_detail_boxes <- function(metric_id) {
  config <- get_metric_config(metric_id)
  if (!is.null(config) && !is.null(config$detail_boxes)) {
    return(config$detail_boxes)
  }
  return(NULL)
}

#' Check if a metric has detail boxes available
#' @param metric_id The metric ID
#' @return Boolean
#' @export
has_detail_boxes <- function(metric_id) {
  !is.null(get_metric_detail_boxes(metric_id))
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
