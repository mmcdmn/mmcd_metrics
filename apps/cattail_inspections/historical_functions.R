# Historical Comparison Functions for Cattail Inspection Progress App
# Functions to handle the historical comparison tab functionality

# Get historical progress data (current year vs historical baseline)
get_historical_progress_data <- function(hist_years, hist_zone, hist_facility_filter) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  start_year <- current_year - hist_years
  end_year <- current_year - 1  # Historical years only (exclude current)
  
  # Convert zone display option to SQL filter
  zone_condition <- ""
  group_by_zone <- FALSE
  if (hist_zone == "p1") {
    zone_condition <- "AND g.zone = '1'"
  } else if (hist_zone == "p2") {
    zone_condition <- "AND g.zone = '2'"
  } else if (hist_zone == "combined") {
    zone_condition <- "AND g.zone IN ('1', '2')"
  } else if (hist_zone == "separate") {
    zone_condition <- "AND g.zone IN ('1', '2')"
    group_by_zone <- TRUE
  }
  
  # Build facility filter
  facility_filter <- ""
  if (!is.null(hist_facility_filter) && length(hist_facility_filter) > 0 && 
      !("all" %in% hist_facility_filter)) {
    facility_list <- paste0("'", hist_facility_filter, "'", collapse = ", ")
    facility_filter <- sprintf("AND a.facility IN (%s)", facility_list)
  }
  
  # Build GROUP BY and SELECT based on zone option
  if (group_by_zone) {
    group_select <- "facility, zone,"
    group_by_clause <- "GROUP BY facility, zone"
  } else {
    group_select <- "facility,"
    group_by_clause <- "GROUP BY facility"
  }
  
  # Query for HISTORICAL inspections (last X years, excluding current year)
  query_historical <- sprintf("
  WITH all_inspections AS (
    SELECT 
      a.facility,
      a.sitecode,
      g.zone
    FROM public.dblarv_insptrt_archive a
    LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
    LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
    WHERE a.action = '9'
      AND EXTRACT(YEAR FROM a.inspdate) >= %d
      AND EXTRACT(YEAR FROM a.inspdate) <= %d
      AND (a.reinspect IS NULL OR a.reinspect = 'f')
      AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
      %s
      %s
    
    UNION ALL
    
    SELECT 
      a.facility,
      a.sitecode,
      g.zone
    FROM public.dblarv_insptrt_current a
    LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
    LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
    WHERE a.action = '9'
      AND EXTRACT(YEAR FROM a.inspdate) >= %d
      AND EXTRACT(YEAR FROM a.inspdate) <= %d
      AND (a.reinspect IS NULL OR a.reinspect = 'f')
      AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
      %s
      %s
  )
  SELECT 
    %s
    COUNT(DISTINCT sitecode)::integer as site_count,
    SUM(DISTINCT_ACRES.acres)::numeric as acre_count
  FROM (
    SELECT DISTINCT
      facility,
      sitecode,
      %s
      FIRST_VALUE(COALESCE(acres_plan, acres)) OVER (
        PARTITION BY facility, sitecode 
        ORDER BY inspdate DESC
      ) as acres
    FROM (
      SELECT 
        ai.facility,
        ai.sitecode,
        %s
        a.inspdate,
        a.acres_plan,
        b.acres
      FROM all_inspections ai
      LEFT JOIN public.dblarv_insptrt_archive a ON ai.sitecode = a.sitecode AND ai.facility = a.facility
      LEFT JOIN public.loc_breeding_sites b ON ai.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) >= %d
        AND EXTRACT(YEAR FROM a.inspdate) <= %d
      
      UNION ALL
      
      SELECT 
        ai.facility,
        ai.sitecode,
        %s
        a.inspdate,
        a.acres_plan,
        b.acres
      FROM all_inspections ai
      LEFT JOIN public.dblarv_insptrt_current a ON ai.sitecode = a.sitecode AND ai.facility = a.facility
      LEFT JOIN public.loc_breeding_sites b ON ai.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) >= %d
        AND EXTRACT(YEAR FROM a.inspdate) <= %d
    ) site_acres
  ) DISTINCT_ACRES
  %s
  ORDER BY facility%s
  ", start_year, end_year, zone_condition, facility_filter,
     start_year, end_year, zone_condition, facility_filter,
     group_select,
     if (group_by_zone) "zone," else "",
     if (group_by_zone) "ai.zone," else "",
     start_year, end_year,
     if (group_by_zone) "ai.zone," else "",
     start_year, end_year,
     group_by_clause,
     if (group_by_zone) ", zone" else "")
  
  historical_result <- dbGetQuery(con, query_historical)
  
  # Query for CURRENT YEAR inspections
  query_current <- sprintf("
  WITH all_inspections AS (
    SELECT 
      a.facility,
      a.sitecode,
      g.zone
    FROM public.dblarv_insptrt_archive a
    LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
    LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
    WHERE a.action = '9'
      AND EXTRACT(YEAR FROM a.inspdate) = %d
      AND (a.reinspect IS NULL OR a.reinspect = 'f')
      AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
      %s
      %s
    
    UNION ALL
    
    SELECT 
      a.facility,
      a.sitecode,
      g.zone
    FROM public.dblarv_insptrt_current a
    LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
    LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
    WHERE a.action = '9'
      AND EXTRACT(YEAR FROM a.inspdate) = %d
      AND (a.reinspect IS NULL OR a.reinspect = 'f')
      AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
      %s
      %s
  )
  SELECT 
    %s
    COUNT(DISTINCT sitecode)::integer as site_count,
    SUM(DISTINCT_ACRES.acres)::numeric as acre_count
  FROM (
    SELECT DISTINCT
      facility,
      sitecode,
      %s
      FIRST_VALUE(COALESCE(acres_plan, acres)) OVER (
        PARTITION BY facility, sitecode 
        ORDER BY inspdate DESC
      ) as acres
    FROM (
      SELECT 
        ai.facility,
        ai.sitecode,
        %s
        a.inspdate,
        a.acres_plan,
        b.acres
      FROM all_inspections ai
      LEFT JOIN public.dblarv_insptrt_archive a ON ai.sitecode = a.sitecode AND ai.facility = a.facility
      LEFT JOIN public.loc_breeding_sites b ON ai.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) = %d
      
      UNION ALL
      
      SELECT 
        ai.facility,
        ai.sitecode,
        %s
        a.inspdate,
        a.acres_plan,
        b.acres
      FROM all_inspections ai
      LEFT JOIN public.dblarv_insptrt_current a ON ai.sitecode = a.sitecode AND ai.facility = a.facility
      LEFT JOIN public.loc_breeding_sites b ON ai.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) = %d
    ) site_acres
  ) DISTINCT_ACRES
  %s
  ORDER BY facility%s
  ", current_year, zone_condition, facility_filter,
     current_year, zone_condition, facility_filter,
     group_select,
     if (group_by_zone) "zone," else "",
     if (group_by_zone) "ai.zone," else "",
     current_year,
     if (group_by_zone) "ai.zone," else "",
     current_year,
     group_by_clause,
     if (group_by_zone) ", zone" else "")
  
  current_result <- dbGetQuery(con, query_current)
  
  dbDisconnect(con)
  
  # Convert to numeric to avoid integer64 issues
  if (nrow(historical_result) > 0) {
    historical_result$site_count <- as.numeric(historical_result$site_count)
    historical_result$acre_count <- as.numeric(historical_result$acre_count)
    historical_result$type <- "Historical Baseline"
  }
  
  if (nrow(current_result) > 0) {
    current_result$site_count <- as.numeric(current_result$site_count)
    current_result$acre_count <- as.numeric(current_result$acre_count)
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
create_historical_progress_plot <- function(data, hist_years, metric = "sites", theme = "MMCD") {
  if (nrow(data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 0.5, y = 0.5, label = "No data available"), size = 6) +
           theme_void())
  }
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Select the appropriate metric column and labels
  if (metric == "acres") {
    data$value <- data$acre_count
    y_label <- "Unique Acres Inspected"
    metric_label <- "Acres"
  } else {
    data$value <- data$site_count
    y_label <- "Number of Unique Sites Inspected"
    metric_label <- "Sites"
  }
  
  # Check if zone column exists (for "P1 and P2 Separate" option)
  has_zone <- "zone" %in% names(data)
  
  # Create combined display label for x-axis
  if (has_zone) {
    data$x_label <- paste0(data$facility_display, " - P", data$zone)
    title_suffix <- "(P1 and P2 Separate)"
  } else {
    data$x_label <- data$facility_display
    title_suffix <- ""
  }
  
  # Get status colors for the comparison types
  status_colors <- get_status_colors(theme = theme)
  
  # Create side-by-side bars for better comparison visibility
  ggplot(data, aes(x = x_label, y = value, fill = type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(
      values = c(
        "Historical Baseline" = unname(status_colors["planned"]),
        "Current Year" = unname(status_colors["active"])
      ),
      name = "Period"
    ) +
    labs(
      title = sprintf("Cattail Inspections (%s): %d vs Previous %d Years %s", 
                     metric_label, current_year, hist_years, title_suffix),
      x = "Facility",
      y = y_label
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
  
  # Convert zone display option to SQL filter
  zone_condition <- ""
  if (hist_zone == "p1") {
    zone_condition <- "AND g.zone = '1'"
  } else if (hist_zone == "p2") {
    zone_condition <- "AND g.zone = '2'"
  } else if (hist_zone %in% c("combined", "separate")) {
    zone_condition <- "AND g.zone IN ('1', '2')"
  }
  
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
      LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) >= %d
        AND EXTRACT(YEAR FROM a.inspdate) <= %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
        %s
        %s
      
      UNION
      
      SELECT DISTINCT
        a.sitecode,
        a.facility
      FROM public.dblarv_insptrt_archive a
      LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) >= %d
        AND EXTRACT(YEAR FROM a.inspdate) <= %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
        %s
        %s
    ),
    current_year_sites AS (
      SELECT DISTINCT
        a.sitecode,
        a.facility
      FROM public.dblarv_insptrt_current a
      LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) = %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
        %s
        %s
      
      UNION
      
      SELECT DISTINCT
        a.sitecode,
        a.facility
      FROM public.dblarv_insptrt_archive a
      LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) = %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
        %s
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
        a.numdip,
        COALESCE(a.acres_plan, b.acres) as acres,
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
        a.numdip,
        COALESCE(a.acres_plan, b.acres) as acres,
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
      numdip,
      acres
    FROM ranked_inspections
    WHERE rn = 1
    ORDER BY facility, sitecode
    ", start_year, end_year, zone_condition, facility_filter,
       start_year, end_year, zone_condition, facility_filter,
       current_year, zone_condition, facility_filter,
       current_year, zone_condition, facility_filter,
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
        a.numdip,
        COALESCE(a.acres_plan, b.acres) as acres,
        g.zone,
        ROW_NUMBER() OVER (PARTITION BY a.sitecode, a.facility ORDER BY a.inspdate DESC) as rn
      FROM public.dblarv_insptrt_current a
      LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) >= %d
        AND EXTRACT(YEAR FROM a.inspdate) <= %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
        %s
        %s
      
      UNION ALL
      
      SELECT 
        a.sitecode,
        a.facility,
        a.inspdate,
        a.wet,
        a.numdip,
        COALESCE(a.acres_plan, b.acres) as acres,
        g.zone,
        ROW_NUMBER() OVER (PARTITION BY a.sitecode, a.facility ORDER BY a.inspdate DESC) as rn
      FROM public.dblarv_insptrt_archive a
      LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      WHERE a.action = '9'
        AND EXTRACT(YEAR FROM a.inspdate) >= %d
        AND EXTRACT(YEAR FROM a.inspdate) <= %d
        AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
        %s
        %s
    )
    SELECT 
      sitecode,
      facility,
      inspdate as last_inspection,
      wet,
      numdip,
      acres
    FROM ranked_inspections
    WHERE rn = 1
    ORDER BY facility, sitecode
    ", start_year, end_year, zone_condition, facility_filter,
       start_year, end_year, zone_condition, facility_filter)
  }
  
  result <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  if (nrow(result) == 0) {
    return(data.frame())
  }
  
  # Format date and convert numeric types
  result$last_inspection <- as.Date(result$last_inspection)
  result$acres <- as.numeric(result$acres)
  
  return(result)
}
