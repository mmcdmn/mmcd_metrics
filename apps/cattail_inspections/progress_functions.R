# Functions for Cattail Inspection Progress vs Goal

# Get inspection progress data (actuals vs goals)
get_progress_data <- function(year, goal_column, custom_today) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  # Get distinct sites with most recent inspection from archive and current tables
  query_combined <- sprintf(
    paste(
      "WITH all_inspections AS (",
      "  SELECT a.facility, a.sitecode, a.inspdate,",
      "         g.zone,",
      "         ROW_NUMBER() OVER (PARTITION BY a.sitecode ORDER BY a.inspdate DESC) as rn",
      "  FROM (",
      "    SELECT facility, sitecode, inspdate FROM public.dblarv_insptrt_archive",
      "    WHERE action = '9'",
      "      AND EXTRACT(YEAR FROM inspdate) = %d",
      "      AND inspdate <= '%s'",
      "    UNION ALL",
      "    SELECT facility, sitecode, inspdate FROM public.dblarv_insptrt_current",
      "    WHERE action = '9'",
      "      AND EXTRACT(YEAR FROM inspdate) = %d",
      "      AND inspdate <= '%s'",
      "  ) a",
      "  LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 7) = g.sectcode",
      "  LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode",
      "  WHERE (b.enddate IS NULL OR b.enddate > CURRENT_DATE)",
      ")",
      "SELECT facility, zone, COUNT(DISTINCT sitecode) AS inspections",
      "FROM all_inspections",
      "WHERE rn = 1",
      "GROUP BY facility, zone",
      sep = "\n"
    ),
    as.numeric(year),
    as.character(custom_today),
    as.numeric(year),
    as.character(custom_today)
  )
  
  # Get the combined result
  combined_result <- dbGetQuery(con, query_combined)
  
  # Combine actuals
  actuals <- combined_result %>%
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
  
  p <- ggplot(data, aes(x = facility_display, y = count, fill = type, 
                        text = paste0(type, ": ", count, " sites"))) +
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
  
  # Convert to plotly for interactive tooltips
  ggplotly(p, tooltip = "text")
}

# Get detailed site list for progress data
get_progress_sites_detail <- function(year, goal_column, custom_today) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  # Determine which zone to use based on goal_column
  selected_zone <- ifelse(goal_column == "p1_totsitecount", "1", "2")
  
  # Get detailed site information with most recent inspection
  query_detail <- sprintf(
    paste(
      "WITH all_inspections AS (",
      "  SELECT a.facility, a.sitecode, a.inspdate, a.wet, a.numdip,",
      "         g.zone,",
      "         ROW_NUMBER() OVER (PARTITION BY a.sitecode ORDER BY a.inspdate DESC) as rn",
      "  FROM (",
      "    SELECT facility, sitecode, inspdate, wet, numdip FROM public.dblarv_insptrt_archive",
      "    WHERE action = '9'",
      "      AND EXTRACT(YEAR FROM inspdate) = %d",
      "      AND inspdate <= '%s'",
      "    UNION ALL",
      "    SELECT facility, sitecode, inspdate, wet, numdip FROM public.dblarv_insptrt_current",
      "    WHERE action = '9'",
      "      AND EXTRACT(YEAR FROM inspdate) = %d",
      "      AND inspdate <= '%s'",
      "  ) a",
      "  LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 7) = g.sectcode",
      "  LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode",
      "  WHERE (b.enddate IS NULL OR b.enddate > CURRENT_DATE)",
      ")",
      "SELECT ",
      "  ai.facility,",
      "  ai.sitecode,",
      "  ai.inspdate,",
      "  ai.wet,",
      "  ai.numdip,",
      "  COALESCE(b.acres, 0) as acres",
      "FROM all_inspections ai",
      "LEFT JOIN public.loc_breeding_sites b ON ai.sitecode = b.sitecode",
      "WHERE ai.rn = 1",
      "  AND ai.zone = '%s'",
      "  AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)",
      "ORDER BY ai.facility, ai.inspdate DESC",
      sep = "\n"
    ),
    as.numeric(year),
    as.character(custom_today),
    as.numeric(year),
    as.character(custom_today),
    selected_zone
  )
  
  result <- dbGetQuery(con, query_detail)
  dbDisconnect(con)
  
  if (nrow(result) > 0) {
    result <- result %>%
      mutate(
        facility = trimws(facility),
        inspdate = as.Date(inspdate)
      )
  }
  
  return(result)
}
