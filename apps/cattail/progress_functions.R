# Functions for Cattail Inspection Progress vs Goal

# Get inspection progress data (actuals vs goals)
get_progress_data <- function(year, goal_column, custom_today) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  # Get actual inspections from archive (filter by year, date, reinspect, and join for zone)
  # Note: archive table doesn't have enddate column - it's historical closed data
  query_archive <- sprintf(
    paste(
      "SELECT a.facility, g.zone, COUNT(*) AS inspections",
      "FROM public.dblarv_insptrt_archive a",
      "LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 6) || '-' = g.sectcode",
      "  OR LEFT(a.sitecode, 6) || 'N' = g.sectcode",
      "  OR LEFT(a.sitecode, 6) || 'S' = g.sectcode",
      "  OR LEFT(a.sitecode, 6) || 'E' = g.sectcode",
      "  OR LEFT(a.sitecode, 6) || 'W' = g.sectcode",
      "WHERE a.action = '9'",
      "  AND EXTRACT(YEAR FROM a.inspdate) = %d",
      "  AND a.inspdate <= '%s'",
      "  AND (a.reinspect IS NULL OR a.reinspect = 'f')",
      "GROUP BY a.facility, g.zone",
      sep = "\n"
    ),
    as.numeric(year),
    as.character(custom_today)
  )
  archive <- dbGetQuery(con, query_archive)
  
  # Get actual inspections from current (filter by year, date, reinspect, and join for zone)
  query_current <- sprintf(
    paste(
      "SELECT a.facility, g.zone, COUNT(*) AS inspections",
      "FROM public.dblarv_insptrt_current a",
      "LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 6) || '-' = g.sectcode",
      "  OR LEFT(a.sitecode, 6) || 'N' = g.sectcode",
      "  OR LEFT(a.sitecode, 6) || 'S' = g.sectcode",
      "  OR LEFT(a.sitecode, 6) || 'E' = g.sectcode",
      "  OR LEFT(a.sitecode, 6) || 'W' = g.sectcode",
      "WHERE a.action = '9'",
      "  AND EXTRACT(YEAR FROM a.inspdate) = %d",
      "  AND a.inspdate <= '%s'",
      "  AND (a.reinspect IS NULL OR a.reinspect = 'f')",
      "GROUP BY a.facility, g.zone",
      sep = "\n"
    ),
    as.numeric(year),
    as.character(custom_today)
  )
  current <- dbGetQuery(con, query_current)
  
  # Combine actuals
  actuals <- bind_rows(archive, current) %>%
    mutate(facility = trimws(facility), zone = as.character(zone)) %>%
    group_by(facility, zone) %>%
    summarize(inspections = sum(as.numeric(inspections), na.rm = TRUE), .groups = "drop")
  actuals$inspections <- as.numeric(actuals$inspections)
  
  # Get goals
  goals <- dbGetQuery(con, "SELECT facility, p1_totsitecount, p2_totsitecount FROM public.cattail_pctcomplete_base") %>%
    mutate(facility = trimws(facility))
  
  dbDisconnect(con)
  
  # Determine which zone to use based on goal_column
  selected_zone <- ifelse(goal_column == "p1_totsitecount", "1", "2")
  
  # Filter actuals for the selected zone
  actuals_zone <- actuals %>% filter(zone == selected_zone)
  
  # Build a complete facility list for this zone
  all_facilities <- union(actuals_zone$facility, goals$facility)
  
  # Build actuals and goals for all facilities in this zone
  actuals_long <- tibble::tibble(
    facility = all_facilities,
    count = purrr::map_dbl(all_facilities, function(f) {
      val <- actuals_zone$inspections[actuals_zone$facility == f]
      if (length(val) == 0) 0 else val
    }),
    type = "Actual Inspections"
  )
  
  goals_long <- tibble::tibble(
    facility = all_facilities,
    count = purrr::map_dbl(all_facilities, function(f) {
      val <- goals[[goal_column]][goals$facility == f]
      if (length(val) == 0) 0 else val
    }),
    type = "Goal"
  )
  
  plot_data <- dplyr::bind_rows(actuals_long, goals_long) %>%
    map_facility_names()
  
  return(plot_data)
}

# Create progress plot
create_progress_plot <- function(data) {
  if (nrow(data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 0.5, y = 0.5, label = "No data available"), size = 6) +
           theme_void())
  }
  
  # Get colors from centralized db_helpers
  status_colors <- get_status_colors()
  
  ggplot(data, aes(x = facility_display, y = count, fill = type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.7) +
    scale_fill_manual(values = c("Actual Inspections" = unname(status_colors["active"]), 
                                   "Goal" = unname(status_colors["planned"]))) +
    labs(
      title = "Cattail Inspections vs. Goal by Facility",
      x = "Facility",
      y = "Number of Sites",
      fill = "Legend"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", size = 16, color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(face = "bold", size = 16, color = "black"),
      axis.title.y = element_text(face = "bold", size = 16),
      legend.text = element_text(face = "bold", size = 14),
      legend.title = element_text(face = "bold", size = 14)
    )
}
