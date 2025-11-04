# Data functions for SUCO History app
# These functions handle both current-only and current+archive data

source("../../shared/db_helpers.R")

# Get SUCO data with proper current vs all distinction
get_suco_data <- function(data_source = "all", date_range = NULL) {
  con <- get_db_connection()
  if (is.null(con)) {
    return(data.frame())  # Return empty data frame if connection fails
  }
  
  # Use provided date range or default
  if (is.null(date_range) || length(date_range) != 2) {
    # Default to current year
    start_date <- paste0(format(Sys.Date(), "%Y"), "-01-01")
    end_date <- format(Sys.Date(), "%Y-%m-%d")
  } else {
    start_date <- format(date_range[1], "%Y-%m-%d")
    end_date <- format(date_range[2], "%Y-%m-%d")
  }
  
  if (data_source == "current") {
    # Current data only - from dbadult_insp_current table
    current_query <- sprintf("
SELECT
s.id, s.ainspecnum, s.facility, 
CASE 
  WHEN h.foreman IS NOT NULL AND h.foreman != '' THEN h.foreman 
  WHEN s.foreman IS NOT NULL AND s.foreman != '' THEN s.foreman
  ELSE NULL
END as foreman,
s.inspdate, s.sitecode,
s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
s.x, s.y,
g.zone
FROM public.dbadult_insp_current s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND s.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR s.inspdate <= h.enddate)
LEFT JOIN public.gis_sectcode g ON LEFT(s.sitecode, 6) || '-' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'N' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'S' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'E' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'W' = g.sectcode
WHERE s.survtype = '7'
AND s.inspdate BETWEEN '%s' AND '%s'
", start_date, end_date)
    
    current_data <- dbGetQuery(con, current_query)
    
    # Get species data
    species_current_query <- "
SELECT ainspecnum, spp, cnt
FROM public.dbadult_species_current
WHERE ainspecnum IS NOT NULL
"
    species_current <- dbGetQuery(con, species_current_query)
    
    # Get species lookup
    species_lookup <- get_species_lookup()
    
    dbDisconnect(con)
    
    if (nrow(current_data) == 0) {
      return(data.frame())
    }
    
    # Process data with all required fields
    processed_data <- current_data %>%
      mutate(
        inspdate = as.Date(inspdate),
        year = year(inspdate),
        month = month(inspdate),
        week_start = floor_date(inspdate, "week", week_start = 1),
        month_label = format(inspdate, "%b %Y"),
        location = ifelse(!is.na(park_name) & park_name != "", park_name,
                          ifelse(!is.na(address1) & address1 != "", address1, sitecode)),
        foreman = trimws(as.character(foreman)),
        zone = as.character(zone)
      ) %>%
      left_join(species_current, by = "ainspecnum", relationship = "many-to-many") %>%
      left_join(species_lookup, by = c("spp" = "sppcode")) %>%
      mutate(
        species_name = case_when(
          !is.na(genus) & !is.na(species) ~ paste(genus, species),
          !is.na(spp) ~ as.character(spp),
          TRUE ~ NA_character_
        )
      )
    
    return(processed_data)
    
  } else {
    # All data - both current and archive
    # Query current data for SUCOs (survtype = 7) with harborage lookup for current foreman assignments
    current_query <- sprintf("
SELECT
s.id, s.ainspecnum, s.facility, 
CASE 
  WHEN h.foreman IS NOT NULL AND h.foreman != '' THEN h.foreman 
  WHEN s.foreman IS NOT NULL AND s.foreman != '' THEN s.foreman
  ELSE NULL
END as foreman,
s.inspdate, s.sitecode,
s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
s.x, s.y, ST_AsText(s.geometry) as geometry_text,
COALESCE(h.facility, '') as harborage_facility,
g.zone
FROM public.dbadult_insp_current s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND s.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR s.inspdate <= h.enddate)
LEFT JOIN public.gis_sectcode g ON LEFT(s.sitecode, 6) || '-' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'N' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'S' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'E' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'W' = g.sectcode
WHERE s.survtype = '7'
AND s.inspdate BETWEEN '%s' AND '%s'
", start_date, end_date)
    
    # Query archive data for SUCOs with harborage lookup for current foreman assignments
    archive_query <- sprintf("
SELECT
s.id, s.ainspecnum, s.facility,
CASE 
  WHEN h.foreman IS NOT NULL AND h.foreman != '' THEN h.foreman 
  WHEN s.foreman IS NOT NULL AND s.foreman != '' THEN s.foreman
  ELSE NULL
END as foreman,
s.inspdate, s.sitecode,
s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
s.x, s.y, ST_AsText(s.geometry) as geometry_text,
COALESCE(h.facility, '') as harborage_facility,
g.zone
FROM public.dbadult_insp_archive s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND s.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR s.inspdate <= h.enddate)
LEFT JOIN public.gis_sectcode g ON LEFT(s.sitecode, 6) || '-' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'N' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'S' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'E' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'W' = g.sectcode
WHERE s.survtype = '7'
AND s.inspdate BETWEEN '%s' AND '%s'
", start_date, end_date)
    
    # Execute queries
    current_data <- dbGetQuery(con, current_query)
    archive_data <- dbGetQuery(con, archive_query)
    
    # Query species tables (current and archive)
    species_current_query <- "
SELECT ainspecnum, spp, cnt
FROM public.dbadult_species_current
WHERE ainspecnum IS NOT NULL
"
    species_archive_query <- "
SELECT ainspecnum, spp, cnt
FROM public.dbadult_species_archive
WHERE ainspecnum IS NOT NULL
"
    species_current <- dbGetQuery(con, species_current_query)
    species_archive <- dbGetQuery(con, species_archive_query)

    # Query species lookup table using centralized function
    species_lookup <- get_species_lookup()

    # Close connection
    dbDisconnect(con)

    # Combine current and archive data
    all_data <- bind_rows(
      mutate(current_data, source = "Current"),
      mutate(archive_data, source = "Archive")
    ) %>%
      mutate(
        inspdate = as.Date(inspdate),
        year = year(inspdate),
        month = month(inspdate),
        week_start = floor_date(inspdate, "week", week_start = 1),
        month_label = format(inspdate, "%b %Y"),
        location = ifelse(!is.na(park_name) & park_name != "", park_name,
                          ifelse(!is.na(address1) & address1 != "", address1, sitecode)),
        # Use harborage facility when harborage foreman is used
        # This ensures foreman colors match the correct facility
        facility = ifelse(!is.na(harborage_facility) & harborage_facility != "", 
                         harborage_facility, facility)
      )
    
    # Combine species data
    all_species <- bind_rows(species_current, species_archive)

    # Join SUCO data with species data and lookup for names
    joined_data <- all_data %>%
      left_join(all_species, by = "ainspecnum", relationship = "many-to-many") %>%
      left_join(species_lookup, by = c("spp" = "sppcode")) %>%
      mutate(
        species_name = dplyr::case_when(
          !is.na(genus) & !is.na(species) ~ paste(genus, species),
          !is.na(spp) ~ as.character(spp),
          TRUE ~ NA_character_
        ),
        # Ensure foreman values are consistent strings
        foreman = trimws(as.character(foreman))
      )
    
    return(joined_data)
  }
}

# Filter SUCO data based on UI inputs
filter_suco_data <- function(data, facility_filter, foreman_filter, zone_filter, date_range, species_filter = "All") {
  filtered_data <- data
  
  # Apply zone filter
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    filtered_data <- filtered_data %>%
      filter(zone %in% zone_filter)
  }
  
  # Apply facility filter
  if (!is.null(facility_filter) && !("All" %in% facility_filter)) {
    filtered_data <- filtered_data %>%
      filter(facility %in% facility_filter)
  }
  
  # Apply foreman filter  
  if (!is.null(foreman_filter) && !("All" %in% foreman_filter)) {
    filtered_data <- filtered_data %>%
      filter(foreman %in% foreman_filter)
  }
  
  # Apply species filter
  if (!is.null(species_filter) && species_filter != "All") {
    filtered_data <- filtered_data %>%
      filter(species_name == species_filter)
  }
  
  # Date range filter is already applied in get_suco_data function
  
  return(filtered_data)
}

# Create spatial data for mapping
create_spatial_data <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Create sf object for mapping
  sf_data <- data %>%
    # Use x and y coordinates if available, otherwise try to parse geometry
    mutate(
      longitude = as.numeric(x),
      latitude = as.numeric(y),
      has_coords = !is.na(longitude) & !is.na(latitude) &
        longitude > -180 & longitude < 180 &
        latitude > -90 & latitude < 90
    ) %>%
    filter(has_coords) %>%
    # Create sf object
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  return(sf_data)
}

# Aggregate SUCO data by time interval and grouping
aggregate_suco_data <- function(data, group_by, zone_filter) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Use weekly intervals (Monday start date of each week)
  data <- data %>%
    mutate(time_group = week_start)
  
  # Define the grouping column
  group_col <- group_by
  
  # We should NOT show zones separately when user selects "P1 + P2" 
  # Zones should only be shown separately when user wants to compare zones
  # For now, we'll only show zones separately if explicitly requested (never in this version)
  # This means "P1 + P2" will always combine the zones into single bars/lines
  show_zones_separately <- FALSE
  
  if (group_col == "mmcd_all") {
    if (show_zones_separately) {
      # Group by zone when both P1 and P2 are selected
      result <- data %>%
        group_by(time_group, zone) %>%
        summarize(
          count = n(),
          total_fieldcount = sum(fieldcount, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(time_group) %>%
        mutate(zone_label = paste0("P", zone))
    } else {
      # Aggregate for the whole MMCD (no grouping by facility or foreman)
      result <- data %>%
        group_by(time_group) %>%
        summarize(
          count = n(),
          total_fieldcount = sum(fieldcount, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(time_group)
      # Add a dummy column for plotting
      result$mmcd_all <- "MMCD (All)"
    }
  } else {
    if (show_zones_separately) {
      # Group by both the selected grouping column AND zone
      result <- data %>%
        group_by(time_group, !!sym(group_col), zone) %>%
        summarize(
          count = n(),
          total_fieldcount = sum(fieldcount, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(time_group) %>%
        mutate(
          combined_group = paste0(!!sym(group_col), " (P", zone, ")"),
          zone_label = paste0("P", zone)
        )
    } else {
      # Group and summarize data by the selected column only
      result <- data %>%
        group_by(time_group, !!sym(group_col)) %>%
        summarize(
          count = n(),
          total_fieldcount = sum(fieldcount, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(time_group)
    }
  }
  
  return(result)
}

# Create summary statistics for data table
create_summary_stats <- function(data, group_by, data_source = "all") {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  group_col <- group_by
  
  if (group_col == "mmcd_all") {
    # Summarize for the whole MMCD (no grouping)
    summary_data <- data %>%
      summarize(
        Total_SUCOs = n(),
        Total_Locations = n_distinct(sitecode),
        Total_Species_Count = sum(cnt, na.rm = TRUE),
        First_SUCO = min(inspdate),
        Last_SUCO = max(inspdate)
      )
    
    # Add appropriate row name based on data source
    row_label <- if (data_source == "current") "MMCD (All) - Current Data" else "MMCD (All)"
    rownames(summary_data) <- row_label
    
  } else if (group_col == "facility") {
    # Get all facilities from lookup
    facilities <- get_facility_lookup()
    
    # Create base data frame with all facilities
    all_facilities <- data.frame(
      facility = facilities$short_name,
      full_name = facilities$full_name
    )
    
    # Summarize actual data
    summary_stats <- data %>%
      group_by(facility) %>%
      summarize(
        Total_SUCOs = n(),
        Total_Locations = n_distinct(sitecode),
        Total_Species_Count = sum(cnt, na.rm = TRUE),
        First_SUCO = min(inspdate),
        Last_SUCO = max(inspdate)
      )
    
    # Join with all facilities to include zeros
    summary_data <- all_facilities %>%
      left_join(summary_stats, by = "facility") %>%
      mutate(
        Total_SUCOs = replace_na(Total_SUCOs, 0),
        Total_Locations = replace_na(Total_Locations, 0),
        Total_Species_Count = replace_na(Total_Species_Count, 0),
        First_SUCO = as.Date(First_SUCO),
        Last_SUCO = as.Date(Last_SUCO)
      ) %>%
      arrange(desc(Total_SUCOs), facility)
    
    # Rename columns
    colnames(summary_data)[1:2] <- c("Facility", "Facility_Name")
    
  } else {  # foreman grouping
    # Get foreman lookup
    foremen_lookup <- get_foremen_lookup()
    
    # Create base data frame with all foremen
    all_foremen <- data.frame(
      foreman = foremen_lookup$emp_num,
      shortname = foremen_lookup$shortname,
      facility = foremen_lookup$facility
    )
    
    # Summarize actual data
    summary_stats <- data %>%
      group_by(foreman) %>%
      summarize(
        Total_SUCOs = n(),
        Total_Locations = n_distinct(sitecode),
        Total_Species_Count = sum(cnt, na.rm = TRUE),
        First_SUCO = min(inspdate),
        Last_SUCO = max(inspdate)
      )
    
    # Join with all foremen to include zeros
    summary_data <- all_foremen %>%
      left_join(summary_stats, by = c("foreman")) %>%
      mutate(
        Total_SUCOs = replace_na(Total_SUCOs, 0),
        Total_Locations = replace_na(Total_Locations, 0),
        Total_Species_Count = replace_na(Total_Species_Count, 0),
        First_SUCO = as.Date(First_SUCO),
        Last_SUCO = as.Date(Last_SUCO)
      ) %>%
      arrange(desc(Total_SUCOs), facility, shortname)
    
    # Rename columns
    colnames(summary_data)[1:3] <- c("FOS", "Name", "Facility")
  }
  
  return(summary_data)
}

# Get top locations for plotly chart
get_top_locations <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Always group by location for top locations
  top_locations <- data %>%
    group_by(location) %>%
    summarize(visits = n(), .groups = "drop") %>%
    filter(!is.na(location)) %>%  # Remove NA locations
    arrange(desc(visits)) %>%
    head(15)
  
  return(top_locations)
}

# Function to convert well-known binary (WKB) to sf object
wkb_to_sf <- function(wkb) {
  if (is.na(wkb) || is.null(wkb) || wkb == "") {
    return(NULL)
  }
  
  tryCatch({
    # First, try to parse as hex WKB
    point <- sf::st_as_sfc(wkb, hex = TRUE, crs = 4326)
    return(point)
  }, error = function(e) {
    tryCatch({
      # If that fails, try parsing as WKT
      if (substr(wkb, 1, 5) == "POINT" ||
          substr(wkb, 1, 10) == "MULTIPOINT" ||
          substr(wkb, 1, 10) == "LINESTRING" ||
          substr(wkb, 1, 7) == "POLYGON") {
        point <- sf::st_as_sfc(wkb, crs = 4326)
        return(point)
      }
      return(NULL)
    }, error = function(e) {
      return(NULL)
    })
  })
}