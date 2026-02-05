# Functions for Cattail Inspection Progress vs Goal
# Now uses data_functions.R for data loading

# Source data_functions.R if not already loaded
if (!exists("load_raw_data", mode = "function")) {
  # Try to find data_functions.R relative to this file
  script_dir <- dirname(sys.frame(1)$ofile %||% ".")
  data_functions_path <- file.path(script_dir, "data_functions.R")
  
  # Fallback paths for different execution contexts
  if (!file.exists(data_functions_path)) {
    data_functions_path <- "data_functions.R"  # Current directory
  }
  if (!file.exists(data_functions_path)) {
    data_functions_path <- "apps/cattail_inspections/data_functions.R"  # From project root
  }
  
  if (file.exists(data_functions_path)) {
    source(data_functions_path)
  } else {
    # Create stub function for testing environments
    load_raw_data <- function(...) {
      data.frame(
        facility = character(0),
        zone = character(0),
        total_count = numeric(0),
        active_count = numeric(0),
        expiring_count = numeric(0)
      )
    }
    message("Using stub load_raw_data function - data_functions.R not available")
  }
}

#' Get inspection progress data (actuals vs goals)
#' @param year The year to query
#' @param zone_option One of: "total", "p1", "p2", "separate"
#' @param custom_today Analysis date
#' @return Data frame for plotting
get_progress_data <- function(year, zone_option, custom_today) {
  # Map zone_option to zone_filter for load_raw_data
  zone_filter <- switch(zone_option,
    "total" = c("1", "2"),
    "p1" = c("1"),
    "p2" = c("2"),
    "separate" = c("1", "2"),
    c("1", "2")  # default
  )
  
  # Use the new data_functions.R
  data <- load_raw_data(
    analysis_date = custom_today,
    zone_filter = zone_filter,
    end_year = as.numeric(year)
  )
  
  if (is.null(data$sites) || nrow(data$sites) == 0) {
    return(data.frame())
  }
  
  sites <- data$sites
  
  if (zone_option == "separate") {
    # Faceted view: P1 and P2 for each facility
    p1_data <- sites %>%
      select(facility, p1_goal, p1_actual) %>%
      mutate(zone = "P1", goal = p1_goal, actual = p1_actual) %>%
      select(facility, zone, goal, actual)
    
    p2_data <- sites %>%
      select(facility, p2_goal, p2_actual) %>%
      mutate(zone = "P2", goal = p2_goal, actual = p2_actual) %>%
      select(facility, zone, goal, actual)
    
    plot_data <- bind_rows(p1_data, p2_data) %>%
      map_facility_names()
    
  } else if (zone_option == "p1") {
    # P1 only
    plot_data <- sites %>%
      select(facility, goal = p1_goal, actual = p1_actual) %>%
      tidyr::pivot_longer(cols = c(goal, actual), names_to = "type", values_to = "count") %>%
      mutate(type = ifelse(type == "goal", "Goal", "Actual Inspections")) %>%
      map_facility_names()
      
  } else if (zone_option == "p2") {
    # P2 only
    plot_data <- sites %>%
      select(facility, goal = p2_goal, actual = p2_actual) %>%
      tidyr::pivot_longer(cols = c(goal, actual), names_to = "type", values_to = "count") %>%
      mutate(type = ifelse(type == "goal", "Goal", "Actual Inspections")) %>%
      map_facility_names()
      
  } else {
    # Total (P1+P2 combined) - need to sum the separate P1/P2 rows
    plot_data <- sites %>%
      group_by(facility) %>%
      summarise(
        goal = sum(total_count, na.rm = TRUE),
        actual = sum(active_count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(cols = c(goal, actual), names_to = "type", values_to = "count") %>%
      mutate(type = ifelse(type == "goal", "Goal", "Actual Inspections")) %>%
      map_facility_names()
  }
  
  return(plot_data)
}

# Get progress data for BOTH P1 and P2 side by side - kept for backward compatibility
get_progress_data_both_zones <- function(year, custom_today) {
  get_progress_data(year, "separate", custom_today)
}

#' Create progress plot - handles all zone options
#' @param data Data from get_progress_data()
#' @param zone_option One of: "total", "p1", "p2", "separate"
#' @param theme Color theme
create_progress_plot <- function(data, zone_option = "total", theme = "MMCD") {
  if (nrow(data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 0.5, y = 0.5, label = "No data available"), size = 6) +
           theme_void())
  }
  
  # Get colors from centralized db_helpers with theme support
  status_colors <- get_status_colors(theme = theme)
  
  if (zone_option == "separate") {
    # Faceted plot for P1 and P2 separate
    plot_data <- data %>%
      tidyr::pivot_longer(cols = c(goal, actual), names_to = "type", values_to = "count") %>%
      mutate(type = ifelse(type == "goal", "Goal", "Actual"))
    
    p <- ggplot(plot_data, aes(x = facility_display, y = count, fill = type,
                                text = paste0(zone, " - ", type, ": ", format(count, big.mark = ",")))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
      facet_wrap(~zone, ncol = 2) +
      scale_fill_manual(values = c("Actual" = unname(status_colors["active"]), 
                                    "Goal" = unname(status_colors["planned"]))) +
      labs(
        title = "Cattail Inspections: P1 vs P2 Progress",
        x = "Facility",
        y = "Number of Sites",
        fill = "Legend"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(face = "bold", size = 11, color = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 11, color = "black"),
        axis.title.y = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 11),
        legend.title = element_text(face = "bold", size = 11),
        strip.text = element_text(face = "bold", size = 12)
      )
  } else {
    # Single bar chart for total, p1, or p2
    zone_label <- switch(zone_option, "p1" = " (P1)", "p2" = " (P2)", "")
    
    p <- ggplot(data, aes(x = facility_display, y = count, fill = type, 
                          text = paste0(type, ": ", format(count, big.mark = ","), " sites"))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.7) +
      scale_fill_manual(values = c("Actual Inspections" = unname(status_colors["active"]), 
                                     "Goal" = unname(status_colors["planned"]))) +
      labs(
        title = paste0("Cattail Inspections vs. Goal", zone_label),
        x = "Facility",
        y = "Number of Sites",
        fill = "Legend"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(face = "bold", size = 14, color = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 14, color = "black"),
        axis.title.y = element_text(face = "bold", size = 14),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size = 12)
      )
  }
  
  ggplotly(p, tooltip = "text")
}

# Backward compatibility
create_progress_plot_both_zones <- function(data, theme = "MMCD") {
  create_progress_plot(data, "separate", theme)
}

#' Get detailed site list for progress data
#' @param year Year to query
#' @param zone_option One of: "total", "p1", "p2", "separate"
#' @param custom_today Analysis date
get_progress_sites_detail <- function(year, zone_option, custom_today) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  # Map zone_option to SQL filter
  zone_filter <- switch(zone_option,
    "total" = "ai.zone IN ('1', '2')",
    "p1" = "ai.zone = '1'",
    "p2" = "ai.zone = '2'",
    "separate" = "ai.zone IN ('1', '2')",
    "ai.zone IN ('1', '2')"  # default
  )
  
  # Get detailed site information with most recent inspection
  query_detail <- sprintf("
    WITH valid_sites AS (
      SELECT sitecode 
      FROM public.loc_breeding_sites 
      WHERE (enddate IS NULL OR enddate > '%s')
    ),
    all_inspections AS (
      SELECT a.facility, a.sitecode, a.inspdate, a.wet, a.numdip,
             g.zone,
             ROW_NUMBER() OVER (PARTITION BY a.sitecode ORDER BY a.inspdate DESC) as rn
      FROM (
        SELECT facility, sitecode, inspdate, wet, numdip FROM public.dblarv_insptrt_archive
        WHERE action = '9'
          AND EXTRACT(YEAR FROM inspdate) = %d
          AND inspdate <= '%s'
        UNION ALL
        SELECT facility, sitecode, inspdate, wet, numdip FROM public.dblarv_insptrt_current
        WHERE action = '9'
          AND EXTRACT(YEAR FROM inspdate) = %d
          AND inspdate <= '%s'
      ) a
      LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 7) = g.sectcode
      WHERE a.sitecode IN (SELECT sitecode FROM valid_sites)
    )
    SELECT 
      ai.facility,
      ai.sitecode,
      ai.inspdate,
      ai.wet,
      ai.numdip,
      ai.zone,
      COALESCE(b.acres, 0) as acres
    FROM all_inspections ai
    LEFT JOIN public.loc_breeding_sites b ON ai.sitecode = b.sitecode
    WHERE ai.rn = 1
      AND %s
    ORDER BY ai.facility, ai.zone, ai.inspdate DESC
  ", custom_today, as.numeric(year), custom_today, as.numeric(year), custom_today, zone_filter)
  
  result <- dbGetQuery(con, query_detail)
  safe_disconnect(con)
  
  if (nrow(result) > 0) {
    result <- result %>%
      mutate(
        facility = trimws(facility),
        inspdate = as.Date(inspdate),
        zone_label = ifelse(zone == "1", "P1", ifelse(zone == "2", "P2", zone))
      )
  }
  
  return(result)
}

message("✓ cattail_inspections/progress_functions.R loaded")