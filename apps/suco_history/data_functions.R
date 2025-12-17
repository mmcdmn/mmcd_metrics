# Data functions for SUCO History app
# These functions handle both current-only and current+archive data

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(sf)
})

source("../../shared/db_helpers.R")

# Get available species list for filter dropdown
get_available_species <- function(date_range = NULL) {
  con <- get_db_connection()
  if (is.null(con)) {
    return(character(0))
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
  
  tryCatch({
    # Get SUCOs from the date range
    sucos_query <- sprintf("
    SELECT DISTINCT s.ainspecnum
    FROM public.dbadult_insp_current s
    WHERE s.survtype = '7' AND s.inspdate BETWEEN '%s' AND '%s'
    UNION
    SELECT DISTINCT s.ainspecnum
    FROM public.dbadult_insp_archive s
    WHERE s.survtype = '7' AND s.inspdate BETWEEN '%s' AND '%s'
    ", start_date, end_date, start_date, end_date)
    
    sucos <- dbGetQuery(con, sucos_query)
    
    if (nrow(sucos) == 0) {
      safe_disconnect(con)
      return(character(0))
    }
    
    # Get species for these SUCOs
    ainspecnums <- paste0("'", sucos$ainspecnum, "'", collapse = ", ")
    species_query <- sprintf("
    SELECT DISTINCT sp.spp, l.genus, l.species
    FROM (
      SELECT spp FROM public.dbadult_species_current WHERE ainspecnum IN (%s)
      UNION 
      SELECT spp FROM public.dbadult_species_archive WHERE ainspecnum IN (%s)
    ) sp
    LEFT JOIN public.lookup_specieslist l ON sp.spp = l.sppcode
    WHERE sp.spp IS NOT NULL
    ORDER BY l.genus, l.species
    ", ainspecnums, ainspecnums)
    
    species_data <- dbGetQuery(con, species_query)
    safe_disconnect(con)
    
    # Use enhanced species mapping from db_helpers
    species_map <- get_enhanced_species_mapping(format_style = "display", include_code = FALSE)
    
    # Create species names using the mapping
    species_names <- species_data %>%
      mutate(
        species_name = case_when(
          !is.na(spp) & as.character(spp) %in% names(species_map) ~ species_map[as.character(spp)],
          !is.na(genus) & !is.na(species) ~ paste(genus, species),
          !is.na(spp) ~ as.character(spp),
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(species_name)) %>%
      pull(species_name) %>%
      unique() %>%
      sort()
    
    return(species_names)
    
  }, error = function(e) {
    safe_disconnect(con)
    return(character(0))
  })
}

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
    # Use loc_harborage as authoritative source for foreman and facility
    # Fallback to gis_sectcode facility if harborage doesn't have data
    # Additional fallback to original inspection data for foreman if harborage is NULL
    current_query <- sprintf("
SELECT
s.id, s.ainspecnum, 
COALESCE(h.facility, g.facility, s.facility) as facility,
COALESCE(h.foreman, s.foreman) as foreman,
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
    
    safe_disconnect(con)
    
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
        epi_week = epiweek(inspdate),
        epi_year = epiyear(inspdate),
        epi_week_label = paste0(" ", epi_week),
        month_label = format(inspdate, "%b %Y"),
        location = ifelse(!is.na(park_name) & park_name != "", park_name,
                          ifelse(!is.na(address1) & address1 != "", address1, sitecode)),
        foreman = trimws(as.character(foreman)),
        zone = as.character(zone)
      ) %>%
      left_join(species_current, by = "ainspecnum", relationship = "many-to-many") %>%
      left_join(species_lookup, by = c("spp" = "sppcode")) %>%
      mutate(
        species_name = {
          # Use enhanced species mapping from db_helpers
          species_map <- get_enhanced_species_mapping(format_style = "display", include_code = FALSE)
          case_when(
            !is.na(spp) & as.character(spp) %in% names(species_map) ~ species_map[as.character(spp)],
            !is.na(genus) & !is.na(species) ~ paste(genus, species),
            !is.na(spp) ~ as.character(spp),
            TRUE ~ NA_character_
          )
        }
      )
    
    # Create species summary for each unique SUCO inspection
    species_summaries <- processed_data %>%
      filter(!is.na(species_name) & !is.na(cnt)) %>%
      group_by(ainspecnum) %>%
      summarize(
        species_summary = {
          species_data <- pick(everything()) %>%
            group_by(species_name) %>%
            summarize(total_count = sum(cnt, na.rm = TRUE), .groups = "drop") %>%
            arrange(desc(total_count))
          
          if (nrow(species_data) == 0) {
            "No species identified"
          } else {
            species_list <- species_data %>%
              mutate(species_text = paste0(species_name, ": ", total_count)) %>%
              pull(species_text)
            
            # Limit to top 5 species to keep popup reasonable
            if (length(species_list) > 5) {
              total_others <- sum(species_data$total_count[6:nrow(species_data)])
              species_list <- c(species_list[1:5], paste0("Others: ", total_others))
            }
            
            paste(species_list, collapse = "<br>")
          }
        },
        total_species_count = sum(cnt, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Create final dataset with one row per SUCO inspection
    final_data <- processed_data %>%
      group_by(ainspecnum, id, inspdate, sitecode, address1, park_name, 
               survtype, fieldcount, comments, x, y, 
               facility, foreman, zone, year, month, week_start, 
               epi_week, epi_year, epi_week_label, month_label, location) %>%
      summarize(.groups = "drop") %>%
      left_join(species_summaries, by = "ainspecnum") %>%
      mutate(
        species_summary = ifelse(is.na(species_summary), "No species data available", species_summary),
        total_species_count = ifelse(is.na(total_species_count), 0, total_species_count)
      )
    
    return(final_data)
    
  } else {
    # All data - both current and archive
    # Query current data for SUCOs (survtype = 7) with harborage lookup for current foreman assignments
    # Use loc_harborage as authoritative source for foreman and facility
    # Fallback to gis_sectcode facility if harborage doesn't have data
    # Additional fallback to original inspection data for foreman if harborage is NULL
    current_query <- sprintf("
SELECT
s.id, s.ainspecnum, 
COALESCE(h.facility, g.facility, s.facility) as facility,
COALESCE(h.foreman, s.foreman) as foreman,
s.inspdate, s.sitecode,
s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
s.x, s.y, ST_AsText(s.geometry) as geometry_text,
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
    # Use loc_harborage as authoritative source for foreman and facility
    # Fallback to gis_sectcode facility if harborage doesn't have data
    # Additional fallback to original inspection data for foreman if harborage is NULL
    archive_query <- sprintf("
SELECT
s.id, s.ainspecnum, 
COALESCE(h.facility, g.facility, s.facility) as facility,
COALESCE(h.foreman, s.foreman) as foreman,
s.inspdate, s.sitecode,
s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
s.x, s.y, ST_AsText(s.geometry) as geometry_text,
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
    safe_disconnect(con)

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
        epi_week = epiweek(inspdate),
        epi_year = epiyear(inspdate),
        epi_week_label = paste0("EW ", epi_week),
        month_label = format(inspdate, "%b %Y"),
        location = ifelse(!is.na(park_name) & park_name != "", park_name,
                          ifelse(!is.na(address1) & address1 != "", address1, sitecode))
      )
    # Combine species data
    all_species <- bind_rows(species_current, species_archive)

    # Join SUCO data with species data and lookup for names
    joined_data <- all_data %>%
      left_join(all_species, by = "ainspecnum", relationship = "many-to-many") %>%
      left_join(species_lookup, by = c("spp" = "sppcode")) %>%
      mutate(
        species_name = {
          # Use enhanced species mapping from db_helpers
          species_map <- get_enhanced_species_mapping(format_style = "display", include_code = FALSE)
          dplyr::case_when(
            !is.na(spp) & as.character(spp) %in% names(species_map) ~ species_map[as.character(spp)],
            !is.na(genus) & !is.na(species) ~ paste(genus, species),
            !is.na(spp) ~ as.character(spp),
            TRUE ~ NA_character_
          )
        },
        # Ensure foreman values are consistent strings
        foreman = trimws(as.character(foreman))
      )
    
    # Create species summary for each unique SUCO inspection
    # This aggregates species data per SUCO for mapping purposes
    species_summaries <- joined_data %>%
      filter(!is.na(species_name) & !is.na(cnt)) %>%
      group_by(ainspecnum) %>%
      summarize(
        species_summary = {
          species_data <- pick(everything()) %>%
            group_by(species_name) %>%
            summarize(total_count = sum(cnt, na.rm = TRUE), .groups = "drop") %>%
            arrange(desc(total_count))
          
          if (nrow(species_data) == 0) {
            "No species identified"
          } else {
            species_list <- species_data %>%
              mutate(species_text = paste0(species_name, ": ", total_count)) %>%
              pull(species_text)
            
            # Limit to top 5 species to keep popup reasonable
            if (length(species_list) > 5) {
              total_others <- sum(species_data$total_count[6:nrow(species_data)])
              species_list <- c(species_list[1:5], paste0("Others: ", total_others))
            }
            
            paste(species_list, collapse = "<br>")
          }
        },
        total_species_count = sum(cnt, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Add species summaries back to the main data and create final dataset
    # For mapping and display, we want one row per SUCO inspection
    final_data <- joined_data %>%
      group_by(ainspecnum, id, inspdate, sitecode, address1, park_name, 
               survtype, fieldcount, comments, x, y, geometry_text, 
               facility, foreman, zone, source, year, month, week_start, 
               epi_week, epi_year, epi_week_label, month_label, location) %>%
      summarize(.groups = "drop") %>%
      left_join(species_summaries, by = "ainspecnum") %>%
      mutate(
        species_summary = ifelse(is.na(species_summary), "No species data available", species_summary),
        total_species_count = ifelse(is.na(total_species_count), 0, total_species_count)
      )
    
    return(final_data)
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
  if (!is.null(foreman_filter) && !("all" %in% tolower(foreman_filter))) {
    # Convert shortnames to emp_nums for filtering
    foremen_lookup <- get_foremen_lookup()
    
    # Get emp_nums that match the selected shortnames
    selected_emp_nums <- foremen_lookup %>%
      filter(shortname %in% foreman_filter) %>%
      pull(emp_num) %>%
      as.character()
    
    # Filter by the emp_nums
    if (length(selected_emp_nums) > 0) {
      filtered_data <- filtered_data %>%
        filter(as.character(foreman) %in% selected_emp_nums)
    }
  }
  
  # Apply species filter - check if species appears in species_summary
  if (!is.null(species_filter) && species_filter != "All") {
    filtered_data <- filtered_data %>%
      filter(grepl(species_filter, species_summary, fixed = TRUE))
  }
  
  # Date range filter is already applied in get_suco_data function
  
  return(filtered_data)
}

# Create spatial data for mapping with species-based sizing
create_spatial_data <- function(data, species_filter = "All") {
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
    # Calculate display species count based on filter
    mutate(
      display_species_count = if (species_filter != "All") {
        # Extract count for specific species from species_summary
        sapply(species_summary, function(summary) {
          if (is.na(summary) || summary == "No species data available" || summary == "No species identified") {
            return(0)
          }
          # Look for the specific species in the summary
          lines <- unlist(strsplit(summary, "<br>"))
          species_line <- lines[grepl(species_filter, lines, fixed = TRUE)]
          if (length(species_line) > 0) {
            # Extract number after the colon
            count_match <- regexpr(": ([0-9]+)", species_line[1])
            if (count_match > 0) {
              count_str <- regmatches(species_line[1], count_match)
              return(as.numeric(gsub(": ", "", count_str)))
            }
          }
          return(0)
        })
      } else {
        # Use total species count when no filter applied
        total_species_count
      }
    )
  
  # Handle multiple SUCOs at same location by adding small random offset
  # Group by exact coordinates and add small jitter for overlapping points
  sf_data <- sf_data %>%
    group_by(longitude, latitude) %>%
    mutate(
      # Add small random offset (max 0.0001 degrees ≈ 11 meters) for overlapping points
      longitude_adj = longitude + runif(n(), -0.0001, 0.0001),
      latitude_adj = latitude + runif(n(), -0.0001, 0.0001),
      # Calculate size based on species count using staged/exponential approach
      marker_size = case_when(
        display_species_count == 0 ~ 4,      # Stage 1: No species
        display_species_count == 1 ~ 6,      # Stage 2: 1 species
        display_species_count <= 5 ~ 8,      # Stage 3: 2-5 species
        display_species_count <= 10 ~ 10,    # Stage 4: 6-10 species
        display_species_count <= 20 ~ 12,    # Stage 5: 11-20 species
        display_species_count <= 30 ~ 14,    # Stage 6: 21-30 species
        display_species_count <= 50 ~ 16,    # Stage 7: 31-50 species
        display_species_count <= 75 ~ 18,    # Stage 8: 51-75 species
        display_species_count <= 100 ~ 20,   # Stage 9: 76-100 species
        TRUE ~ 22                             # Stage 10: 100+ species
      )
    ) %>%
    ungroup() %>%
    # Create sf object with adjusted coordinates
    st_as_sf(coords = c("longitude_adj", "latitude_adj"), crs = 4326)
  
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
          epi_week_label = first(epi_week_label),
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
          epi_week_label = first(epi_week_label),
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
          epi_week_label = first(epi_week_label),
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
          epi_week_label = first(epi_week_label),
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
        Total_Species_Count = sum(total_species_count, na.rm = TRUE),
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
        Total_Species_Count = sum(total_species_count, na.rm = TRUE),
        First_SUCO = min(inspdate),
        Last_SUCO = max(inspdate)
      )
    
    # Join with all facilities to include zeros
    summary_data <- all_facilities %>%
      left_join(summary_stats, by = "facility") %>%
      mutate(
        Total_SUCOs = ifelse(is.na(Total_SUCOs), 0, Total_SUCOs),
        Total_Locations = ifelse(is.na(Total_Locations), 0, Total_Locations),
        Total_Species_Count = ifelse(is.na(Total_Species_Count), 0, Total_Species_Count),
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
        Total_Species_Count = sum(total_species_count, na.rm = TRUE),
        First_SUCO = min(inspdate),
        Last_SUCO = max(inspdate)
      )
    
    # Join with all foremen to include zeros
    summary_data <- all_foremen %>%
      left_join(summary_stats, by = c("foreman")) %>%
      mutate(
        Total_SUCOs = ifelse(is.na(Total_SUCOs), 0, Total_SUCOs),
        Total_Locations = ifelse(is.na(Total_Locations), 0, Total_Locations),
        Total_Species_Count = ifelse(is.na(Total_Species_Count), 0, Total_Species_Count),
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
get_top_locations <- function(data, mode = "visits", species_filter = "All") {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  if (mode == "species") {
    # Mode: Top locations by species count
    if (species_filter != "All") {
      # Filter for specific species and sum their counts
      sample_data <- data %>%
        mutate(
          # Extract count for specific species from species_summary
          species_count = sapply(species_summary, function(summary) {
            if (is.na(summary) || summary == "No species data available" || summary == "No species identified") {
              return(0)
            }
            lines <- unlist(strsplit(summary, "<br>"))
            species_line <- lines[grepl(species_filter, lines, fixed = TRUE)]
            if (length(species_line) > 0) {
              count_match <- regexpr(": ([0-9]+)", species_line[1])
              if (count_match > 0) {
                count_str <- regmatches(species_line[1], count_match)
                return(as.numeric(gsub(": ", "", count_str)))
              }
            }
            return(0)
          })
        ) %>%
        filter(species_count > 0) %>%
        # Add sample identifiers for stacking
        mutate(
          sample_id = paste0("Sample_", row_number()),
          date_label = format(inspdate, "%m/%d"),
          date_numeric = as.numeric(inspdate)  # For continuous color scale
        )
      
      # Get top locations by total species count
      top_location_names <- sample_data %>%
        group_by(location) %>%
        summarize(total_species = sum(species_count, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total_species)) %>%
        head(25) %>%
        pull(location)
      
      # Return sample-level data for top locations
      top_locations <- sample_data %>%
        filter(location %in% top_location_names) %>%
        select(location, species_count, sample_id, date_label, date_numeric, inspdate, species_summary) %>%
        arrange(desc(species_count))
        
    } else {
      # All species - use total species count
      sample_data <- data %>%
        mutate(
          sample_id = paste0("Sample_", row_number()),
          date_label = format(inspdate, "%m/%d"),
          date_numeric = as.numeric(inspdate)  # For continuous color scale
        )
      
      # Get top locations by total species count
      top_location_names <- sample_data %>%
        group_by(location) %>%
        summarize(total_species = sum(total_species_count, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total_species)) %>%
        head(25) %>%
        pull(location)
      
      # Return sample-level data for top locations
      top_locations <- sample_data %>%
        filter(location %in% top_location_names) %>%
        select(location, species_count = total_species_count, sample_id, date_label, date_numeric, inspdate, species_summary) %>%
        arrange(desc(species_count))
    }
  } else {
    # Mode: Top locations by visits (individual samples as stacks)
    sample_data <- data %>%
      mutate(
        sample_id = paste0("Sample_", row_number()),
        date_label = format(inspdate, "%m/%d"),
        date_numeric = as.numeric(inspdate),  # For continuous color scale
        visit_count = 1  # Each sample represents one visit
      )
    
    # Get top locations by total visit count
    top_location_names <- sample_data %>%
      group_by(location) %>%
      summarize(total_visits = n(), .groups = "drop") %>%
      arrange(desc(total_visits)) %>%
      head(25) %>%
      pull(location)
    
    # Return sample-level data for top locations
    top_locations <- sample_data %>%
      filter(location %in% top_location_names) %>%
      select(location, visits = visit_count, sample_id, date_label, date_numeric, inspdate, species_summary) %>%
      arrange(location, inspdate)
  }
  
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

# Function to create species summary text for popups
create_species_summary <- function(data_subset) {
  # data_subset should be all rows for a single SUCO (same ainspecnum)
  if (nrow(data_subset) == 0) {
    return("No species data available")
  }
  
  # Filter out NA species and aggregate by species
  species_data <- data_subset %>%
    filter(!is.na(species_name) & !is.na(cnt)) %>%
    group_by(species_name) %>%
    summarize(total_count = sum(cnt, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_count))
  
  if (nrow(species_data) == 0) {
    return("No species identified")
  }
  
  # Create formatted species list
  species_list <- species_data %>%
    mutate(species_text = paste0(species_name, ": ", total_count)) %>%
    pull(species_text)
  
  # Limit to top 5 species to keep popup reasonable
  if (length(species_list) > 5) {
    total_others <- sum(species_data$total_count[6:nrow(species_data)])
    species_list <- c(species_list[1:5], paste0("Others: ", total_others))
  }
  
  return(paste(species_list, collapse = "<br>"))
}

# Create detailed samples table with harborage and species information
create_detailed_samples_table <- function(data, species_filter = "All") {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Get facility and foreman lookups for proper names
  facilities <- get_facility_lookup()
  foremen_lookup <- get_foremen_lookup()
  
  # Create facility name mapping
  facility_names <- setNames(facilities$full_name, facilities$short_name)
  
  # Create the detailed table
  detailed_table <- data %>%
    mutate(
      # Get facility full name
      facility_name = sapply(facility, function(f) {
        fname <- facility_names[f]
        if(length(fname) > 0 && !is.na(fname)) fname else f
      }),
      
      # Get foreman name
      foreman_name = sapply(foreman, function(f) {
        if (!is.na(f) && f != "" && !is.null(f)) {
          foreman_str <- trimws(as.character(f))
          matches <- which(trimws(as.character(foremen_lookup$emp_num)) == foreman_str)
          if(length(matches) > 0) {
            foremen_lookup$shortname[matches[1]]
          } else {
            paste0("FOS #", foreman_str)
          }
        } else {
          "No FOS assigned"
        }
      }),
      
      # Format species count based on filter
      filtered_species_count = if (species_filter != "All") {
        sapply(species_summary, function(summary) {
          if (is.na(summary) || summary == "No species data available" || summary == "No species identified") {
            return(0)
          }
          lines <- unlist(strsplit(summary, "<br>"))
          species_line <- lines[grepl(species_filter, lines, fixed = TRUE)]
          if (length(species_line) > 0) {
            count_match <- regexpr(": ([0-9]+)", species_line[1])
            if (count_match > 0) {
              count_str <- regmatches(species_line[1], count_match)
              return(as.numeric(gsub(": ", "", count_str)))
            }
          }
          return(0)
        })
      } else {
        total_species_count
      },
      
      # Clean up species summary for table display
      species_display = gsub("<br>", "; ", species_summary)
    ) %>%
    # Filter to only show SUCOs with the selected species (if filtered)
    {if (species_filter != "All") filter(., filtered_species_count > 0) else .} %>%
    # Select and rename columns for the table
    select(
      Date = inspdate,
      Facility = facility_name,
      FOS = foreman_name,
      Zone = zone,
      Sitecode = sitecode,
      Location = location,
      Species_Count = filtered_species_count,
      Species_Found = species_display
    ) %>%
    arrange(desc(Species_Count), Date)
  
  return(detailed_table)
}