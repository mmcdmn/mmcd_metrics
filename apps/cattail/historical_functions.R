# Historical Comparison Functions for Cattail Inspection Progress App
# Functions to handle the historical comparison tab functionality

# Get historical progress data (current year vs historical baseline)
get_historical_progress_data <- function(hist_years, hist_zone, hist_facility_filter) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  start_year <- current_year - hist_years
  end_year <- current_year - 1  # Historical years only (exclude current)
  
  # Use zone directly (no goal table involved)
  selected_zone <- hist_zone
  
  # Build facility filter
  facility_filter <- ""
  if (!is.null(hist_facility_filter) && length(hist_facility_filter) > 0 && 
      !("all" %in% hist_facility_filter)) {
    facility_list <- paste0("'", hist_facility_filter, "'", collapse = ", ")
    facility_filter <- sprintf("AND a.facility IN (%s)", facility_list)
  }
  
  # Query for HISTORICAL inspections (last X years, excluding current year)
  query_historical <- sprintf("
  WITH all_inspections AS (
    SELECT 
      a.facility,
      a.sitecode,
      g.zone
    FROM public.dblarv_insptrt_archive a
    LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 6) || '-' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'N' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'S' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'E' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'W' = g.sectcode
    LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
    WHERE a.action = '9'
      AND EXTRACT(YEAR FROM a.inspdate) >= %d
      AND EXTRACT(YEAR FROM a.inspdate) <= %d
      AND (a.reinspect IS NULL OR a.reinspect = 'f')
      AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
      AND g.zone = '%s'
      %s
    
    UNION ALL
    
    SELECT 
      a.facility,
      a.sitecode,
      g.zone
    FROM public.dblarv_insptrt_current a
    LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 6) || '-' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'N' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'S' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'E' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'W' = g.sectcode
    LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
    WHERE a.action = '9'
      AND EXTRACT(YEAR FROM a.inspdate) >= %d
      AND EXTRACT(YEAR FROM a.inspdate) <= %d
      AND (a.reinspect IS NULL OR a.reinspect = 'f')
      AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
      AND g.zone = '%s'
      %s
  )
  SELECT 
    facility,
    COUNT(DISTINCT sitecode)::integer as count
  FROM all_inspections
  GROUP BY facility
  ORDER BY facility
  ", start_year, end_year, selected_zone, facility_filter,
     start_year, end_year, selected_zone, facility_filter)
  
  historical_result <- dbGetQuery(con, query_historical)
  
  # Query for CURRENT YEAR inspections
  query_current <- sprintf("
  WITH all_inspections AS (
    SELECT 
      a.facility,
      a.sitecode,
      g.zone
    FROM public.dblarv_insptrt_archive a
    LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 6) || '-' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'N' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'S' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'E' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'W' = g.sectcode
    LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
    WHERE a.action = '9'
      AND EXTRACT(YEAR FROM a.inspdate) = %d
      AND (a.reinspect IS NULL OR a.reinspect = 'f')
      AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
      AND g.zone = '%s'
      %s
    
    UNION ALL
    
    SELECT 
      a.facility,
      a.sitecode,
      g.zone
    FROM public.dblarv_insptrt_current a
    LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 6) || '-' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'N' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'S' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'E' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'W' = g.sectcode
    LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
    WHERE a.action = '9'
      AND EXTRACT(YEAR FROM a.inspdate) = %d
      AND (a.reinspect IS NULL OR a.reinspect = 'f')
      AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
      AND g.zone = '%s'
      %s
  )
  SELECT 
    facility,
    COUNT(DISTINCT sitecode)::integer as count
  FROM all_inspections
  GROUP BY facility
  ORDER BY facility
  ", current_year, selected_zone, facility_filter,
     current_year, selected_zone, facility_filter)
  
  current_result <- dbGetQuery(con, query_current)
  
  dbDisconnect(con)
  
  # Convert to numeric to avoid integer64 issues
  if (nrow(historical_result) > 0) {
    historical_result$count <- as.numeric(historical_result$count)
    historical_result$type <- "Historical Baseline"
  }
  
  if (nrow(current_result) > 0) {
    current_result$count <- as.numeric(current_result$count)
    current_result$type <- "Current Year"
  }
  
  # Combine both datasets
  combined <- bind_rows(historical_result, current_result)
  
  if (nrow(combined) == 0) {
    return(data.frame())
  }
  
  # Add facility display names
  combined <- combined %>%
    map_facility_names(facility_col = "facility")
  
  return(combined)
}

# Create historical progress plot
create_historical_progress_plot <- function(data, hist_years) {
  if (nrow(data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 0.5, y = 0.5, label = "No data available"), size = 6) +
           theme_void())
  }
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Get status colors for the comparison types
  status_colors <- get_status_colors()
  
  # Create side-by-side bars for better comparison visibility
  ggplot(data, aes(x = facility_display, y = count, fill = type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(
      values = c(
        "Historical Baseline" = unname(status_colors["planned"]),
        "Current Year" = unname(status_colors["active"])
      ),
      name = "Period"
    ) +
    labs(
      title = sprintf("Cattail Inspections: %d vs Previous %d Years (Combined Total)", 
                     current_year, hist_years),
      x = "Facility",
      y = "Number of Unique Sites Inspected"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", size = 14, angle = 45, hjust = 1),
      axis.text.y = element_text(face = "bold", size = 14),
      axis.title.x = element_text(face = "bold", size = 16),
      axis.title.y = element_text(face = "bold", size = 16),
      plot.title = element_text(face = "bold", size = 16),
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(face = "bold", size = 12),
      legend.position = "top"
    )
}

# Get sites table data
get_sites_table_data <- function(hist_years, hist_zone, hist_facility_filter, sites_view_type) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  start_year <- current_year - hist_years
  end_year <- current_year - 1  # EXCLUDE current year for historical range
  
  # Use zone directly (no goal table involved)
  selected_zone <- hist_zone
  
  # Build facility filter
  facility_filter <- ""
  if (!is.null(hist_facility_filter) && length(hist_facility_filter) > 0 && 
      !("all" %in% hist_facility_filter)) {
    facility_list <- paste0("'", hist_facility_filter, "'", collapse = ", ")
    facility_filter <- sprintf("AND a.facility IN (%s)", facility_list)
  }
  
  if (sites_view_type == "unchecked") {
    # Show sites from last X years that have NOT been checked this current year
    query <- sprintf("
    WITH historical_sites AS (
      SELECT DISTINCT
        a.sitecode,
        a.facility
      FROM public.dblarv_insptrt_current a
      LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 6) || '-' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'N' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'S' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'E' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'W' = g.sectcode
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) >= %d
        AND EXTRACT(YEAR FROM a.inspdate) <= %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
        AND g.zone = '%s'
        %s
      
      UNION
      
      SELECT DISTINCT
        a.sitecode,
        a.facility
      FROM public.dblarv_insptrt_archive a
      LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 6) || '-' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'N' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'S' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'E' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'W' = g.sectcode
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) >= %d
        AND EXTRACT(YEAR FROM a.inspdate) <= %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
        AND g.zone = '%s'
        %s
    ),
    current_year_sites AS (
      SELECT DISTINCT
        a.sitecode,
        a.facility
      FROM public.dblarv_insptrt_current a
      LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 6) || '-' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'N' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'S' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'E' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'W' = g.sectcode
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) = %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
        AND g.zone = '%s'
        %s
      
      UNION
      
      SELECT DISTINCT
        a.sitecode,
        a.facility
      FROM public.dblarv_insptrt_archive a
      LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 6) || '-' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'N' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'S' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'E' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'W' = g.sectcode
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) = %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
        AND g.zone = '%s'
        %s
    ),
    unchecked_sites AS (
      SELECT hs.sitecode, hs.facility
      FROM historical_sites hs
      LEFT JOIN current_year_sites cys ON hs.sitecode = cys.sitecode AND hs.facility = cys.facility
      WHERE cys.sitecode IS NULL
    ),
    ranked_inspections AS (
      SELECT 
        a.sitecode,
        a.facility,
        a.inspdate,
        a.wet,
        a.acres,
        a.acres_plan,
        ROW_NUMBER() OVER (PARTITION BY a.sitecode, a.facility ORDER BY a.inspdate DESC) as rn
      FROM public.dblarv_insptrt_current a
      INNER JOIN unchecked_sites us ON a.sitecode = us.sitecode AND a.facility = us.facility
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) >= %d
        AND EXTRACT(YEAR FROM a.inspdate) <= %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
      
      UNION ALL
      
      SELECT 
        a.sitecode,
        a.facility,
        a.inspdate,
        a.wet,
        a.acres,
        a.acres_plan,
        ROW_NUMBER() OVER (PARTITION BY a.sitecode, a.facility ORDER BY a.inspdate DESC) as rn
      FROM public.dblarv_insptrt_archive a
      INNER JOIN unchecked_sites us ON a.sitecode = us.sitecode AND a.facility = us.facility
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) >= %d
        AND EXTRACT(YEAR FROM a.inspdate) <= %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
    )
    SELECT 
      sitecode,
      facility,
      inspdate as last_inspection,
      wet,
      acres,
      acres_plan
    FROM ranked_inspections
    WHERE rn = 1
    ORDER BY facility, sitecode
    ", start_year, end_year, selected_zone, facility_filter,
       start_year, end_year, selected_zone, facility_filter,
       current_year, selected_zone, facility_filter,
       current_year, selected_zone, facility_filter,
       start_year, end_year,
       start_year, end_year)
    
  } else {
    # Show all sites from last X years (EXCLUDING current year)
    query <- sprintf("
    WITH ranked_inspections AS (
      SELECT 
        a.sitecode,
        a.facility,
        a.inspdate,
        a.wet,
        a.acres,
        a.acres_plan,
        g.zone,
        ROW_NUMBER() OVER (PARTITION BY a.sitecode, a.facility ORDER BY a.inspdate DESC) as rn
      FROM public.dblarv_insptrt_current a
      LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 6) || '-' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'N' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'S' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'E' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'W' = g.sectcode
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) >= %d
        AND EXTRACT(YEAR FROM a.inspdate) <= %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
        AND g.zone = '%s'
        %s
      
      UNION ALL
      
      SELECT 
        a.sitecode,
        a.facility,
        a.inspdate,
        a.wet,
        a.acres,
        a.acres_plan,
        g.zone,
        ROW_NUMBER() OVER (PARTITION BY a.sitecode, a.facility ORDER BY a.inspdate DESC) as rn
      FROM public.dblarv_insptrt_archive a
      LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 6) || '-' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'N' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'S' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'E' = g.sectcode
        OR LEFT(a.sitecode, 6) || 'W' = g.sectcode
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) >= %d
        AND EXTRACT(YEAR FROM a.inspdate) <= %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
        AND g.zone = '%s'
        %s
    )
    SELECT 
      sitecode,
      facility,
      inspdate as last_inspection,
      wet,
      acres,
      acres_plan
    FROM ranked_inspections
    WHERE rn = 1
    ORDER BY facility, sitecode
    ", start_year, end_year, selected_zone, facility_filter,
       start_year, end_year, selected_zone, facility_filter)
  }
  
  result <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  if (nrow(result) == 0) {
    return(data.frame())
  }
  
  # Format date and convert numeric types
  result$last_inspection <- as.Date(result$last_inspection)
  result$acres <- as.numeric(result$acres)
  result$acres_plan <- as.numeric(result$acres_plan)
  
  return(result)
}
