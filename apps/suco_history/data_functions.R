# Data functions for SUCO History app
# These functions handle both current-only and current+archive data

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(sf)
})

source("../../shared/db_helpers.R")

# Get available species list for SUCO filter dropdown
# Based on analysis of 2025 data, only 5 species are relevant to SUCOs
get_available_species <- function(date_range = NULL) {
  # Return the 5 species actually found in SUCO data (from 2025 analysis)
  # Ordered by frequency of occurrence
  species_names <- c(
    "Aedes triseriatus",   # Most common (401 occurrences)
    "Aedes japonicus",     # Second (266 occurrences) 
    "Culex tarsalis",      # Third (124 occurrences)
    "Culiseta melanura",   # Fourth (5 occurrences)
    "Aedes albopictus"     # Fifth (2 occurrences)
  )
  
  return(species_names)
}

# Determine which tables to query based on date range
# Uses get_historical_year_ranges from shared/db_helpers.R
get_optimal_table_strategy <- function(date_range) {
  if (is.null(date_range) || length(date_range) != 2) {
    return(list(query_current = TRUE, query_archive = TRUE))
  }
  
  con <- get_db_connection()
  if (is.null(con)) {
    return(list(query_current = TRUE, query_archive = TRUE))
  }
  
  tryCatch({
    # Get year ranges for inspection tables
    year_ranges <- get_historical_year_ranges(con, 
                                               "public.dbadult_insp_current",
                                               "public.dbadult_insp_archive", 
                                               "inspdate")
    
    safe_disconnect(con)
    
    # Extract years from date range
    start_year <- as.numeric(format(date_range[1], "%Y"))
    end_year <- as.numeric(format(date_range[2], "%Y"))
    requested_years <- seq(start_year, end_year, 1)
    
    # Determine overlap
    current_overlap <- length(intersect(requested_years, year_ranges$current_years)) > 0
    archive_overlap <- length(intersect(requested_years, year_ranges$archive_years)) > 0
    
    return(list(
      query_current = current_overlap,
      query_archive = archive_overlap,
      requested_years = requested_years,
      current_years = year_ranges$current_years,
      archive_years = year_ranges$archive_years
    ))
    
  }, error = function(e) {
    safe_disconnect(con)
    # Fallback to querying both tables
    return(list(query_current = TRUE, query_archive = TRUE))
  })
}

# Get SUCO data with optimized table querying
get_suco_data <- function(data_source = "all", date_range = NULL, return_species_details = FALSE) {
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
  
  # Determine optimal querying strategy
  table_strategy <- get_optimal_table_strategy(date_range)
  
  cat("Table strategy: Current =", table_strategy$query_current, ", Archive =", table_strategy$query_archive, "\n")
  
  # Build queries based on table strategy
  queries <- list()
  
  if (table_strategy$query_current) {
    queries$current <- sprintf("
SELECT
s.id, s.ainspecnum, 
COALESCE(h.facility, g.facility, s.facility) as facility,
COALESCE(h.foreman, s.foreman) as foreman,
s.inspdate, s.sitecode,
s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
s.x, s.y, ST_AsText(s.geometry) as geometry_text,
g.zone,
'current' as source_table
FROM public.dbadult_insp_current s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND s.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR s.inspdate <= h.enddate)
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(s.sitecode, 7)
WHERE s.survtype = '7'
AND s.inspdate BETWEEN '%s' AND '%s'
", start_date, end_date)
  }
  
  if (table_strategy$query_archive) {
    queries$archive <- sprintf("
SELECT
s.id, s.ainspecnum, 
COALESCE(h.facility, g.facility, s.facility) as facility,
COALESCE(h.foreman, s.foreman) as foreman,
s.inspdate, s.sitecode,
s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
s.x, s.y, ST_AsText(s.geometry) as geometry_text,
g.zone,
'archive' as source_table
FROM public.dbadult_insp_archive s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND s.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR s.inspdate <= h.enddate)
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(s.sitecode, 7)
WHERE s.survtype = '7'
AND s.inspdate BETWEEN '%s' AND '%s'
", start_date, end_date)
  }
  
  # Execute queries and combine results
  all_data_parts <- list()
  for (query_name in names(queries)) {
    tryCatch({
      result <- dbGetQuery(con, queries[[query_name]])
      if (nrow(result) > 0) {
        all_data_parts[[query_name]] <- result
        cat("Retrieved", nrow(result), "SUCOs from", query_name, "table\n")
      }
    }, error = function(e) {
      warning(paste("Error querying", query_name, "table:", e$message))
    })
  }
  
  if (length(all_data_parts) == 0) {
    safe_disconnect(con)
    return(data.frame())
  }
  
  # Combine all data
  all_data <- bind_rows(all_data_parts)
  
  # Get species data from both tables
  species_queries <- list()
  if (table_strategy$query_current) {
    species_queries$current <- "SELECT ainspecnum, spp, cnt FROM public.dbadult_species_current WHERE ainspecnum IS NOT NULL"
  }
  if (table_strategy$query_archive) {
    species_queries$archive <- "SELECT ainspecnum, spp, cnt FROM public.dbadult_species_archive WHERE ainspecnum IS NOT NULL"
  }
  
  all_species_parts <- list()
  for (query_name in names(species_queries)) {
    tryCatch({
      result <- dbGetQuery(con, species_queries[[query_name]])
      if (nrow(result) > 0) {
        all_species_parts[[query_name]] <- result
      }
    }, error = function(e) {
      warning(paste("Error querying species", query_name, "table:", e$message))
    })
  }
  
  all_species <- if (length(all_species_parts) > 0) bind_rows(all_species_parts) else data.frame()
  
  # Get species lookup
  species_lookup <- get_species_lookup()
  safe_disconnect(con)
  
  if (nrow(all_data) == 0) {
    return(data.frame())
  }
  
  # Process data
  processed_data <- all_data %>%
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
                        ifelse(!is.na(address1) & address1 != "", address1, sitecode)),
      foreman = trimws(as.character(foreman)),
      zone = as.character(zone)
    ) %>%
    left_join(all_species, by = "ainspecnum", relationship = "many-to-many") %>%
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
  
  # Return appropriate format based on requirement
  if (return_species_details) {
    # Return detailed species data for grouping - one row per species per inspection
    # IMPORTANT: Include zero-species inspections by keeping NA species and converting them
    species_details <- processed_data %>%
      # Keep all rows but convert NA species to "No species" with cnt = 0
      mutate(
        species_name = ifelse(is.na(species_name), NA_character_, species_name),
        cnt = ifelse(is.na(cnt), 0, cnt)
      ) %>%
      # Add summarized fields back to each species record
      group_by(ainspecnum) %>%
      mutate(
        species_summary = {
          species_data <- pick(everything()) %>%
            filter(!is.na(species_name)) %>%
            group_by(species_name) %>%
            summarize(total_count = sum(cnt, na.rm = TRUE), .groups = "drop") %>%
            arrange(desc(total_count))
          
          if (nrow(species_data) == 0) {
            "No species identified"
          } else {
            species_list <- species_data %>%
              mutate(species_text = paste0(species_name, ": ", total_count)) %>%
              pull(species_text)
            
            if (length(species_list) > 5) {
              total_others <- sum(species_data$total_count[6:nrow(species_data)])
              species_list <- c(species_list[1:5], paste0("Others: ", total_others))
            }
            paste(species_list, collapse = "<br>")
          }
        },
        total_species_count = sum(cnt, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      # Now deduplicate: for each inspection, keep species rows OR one "No species" row if none
      group_by(ainspecnum) %>%
      mutate(
        has_species = any(!is.na(species_name) & species_name != ""),
        row_keep = !is.na(species_name) | (!has_species & row_number() == 1)
      ) %>%
      filter(row_keep) %>%
      select(-has_species, -row_keep) %>%
      ungroup()
    
    return(species_details)
  } else {
    # Create final dataset with one row per SUCO inspection (existing behavior)
    final_data <- processed_data %>%
      group_by(ainspecnum, id, inspdate, sitecode, address1, park_name, 
               survtype, fieldcount, comments, x, y, geometry_text,
               facility, foreman, zone, year, month, week_start, 
               epi_week, epi_year, epi_week_label, month_label, location, source_table) %>%
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
  # Return empty data frame if input is empty or NULL
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  filtered_data <- data
  
  # Apply zone filter
  if (!is.null(zone_filter) && length(zone_filter) > 0 && zone_filter != "all" && "zone" %in% names(filtered_data)) {
    filtered_data <- filtered_data %>%
      filter(zone %in% zone_filter)
  }
  
  # Apply facility filter (single-select)
  if (!is.null(facility_filter) && facility_filter != "all" && "facility" %in% names(filtered_data)) {
    filtered_data <- filtered_data %>%
      filter(facility == facility_filter)
  }
  
  # Apply foreman filter using shared helper
  if (is_valid_filter(foreman_filter) && "foreman" %in% names(filtered_data)) {
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
  if (!is.null(species_filter) && species_filter != "All" && "species_summary" %in% names(filtered_data)) {
    filtered_data <- filtered_data %>%
      filter(grepl(species_filter, species_summary, fixed = TRUE))
  }
  
  # Date range filter is already applied in get_suco_data function
  
  return(filtered_data)
}

# Create spatial data for mapping with species-based sizing
create_spatial_data <- function(data, species_filter = "All", group_by = "mmcd_all") {
  if (nrow(data) == 0) {
    return(data.frame())
  }

  # For species grouping, we need to handle the data differently
  if (group_by == "species_name" && "species_name" %in% colnames(data)) {
    # Species grouping: create one point per species per location with species info
    sf_data <- data %>%
      # Include zero species locations by replacing NA with "No species"
      mutate(species_name = ifelse(is.na(species_name), "No species", species_name)) %>%
      mutate(
        longitude = as.numeric(x),
        latitude = as.numeric(y),
        has_coords = !is.na(longitude) & !is.na(latitude) &
          longitude > -180 & longitude < 180 &
          latitude > -90 & latitude < 90
      ) %>%
      filter(has_coords) %>%
      # Keep species information for coloring
      mutate(
        display_species_count = cnt,
        marker_size = case_when(
          cnt == 0 ~ 4,
          cnt == 1 ~ 6,
          cnt <= 5 ~ 8,
          cnt <= 10 ~ 10,
          cnt <= 20 ~ 12,
          TRUE ~ 14
        )
      ) %>%
      # Create sf object
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    return(sf_data)
  }
  
  # Regular grouping (existing logic)
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
  
  # Map species group_by to correct column name
  if (group_col == "species_name") {
    species_grouping <- TRUE
    # group_col stays as "species_name" since that's the actual column name
  } else {
    species_grouping <- FALSE
  }
  
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
  } else if (species_grouping) {
    # Check if we have detailed species data - if not, throw error
    if (!("species_name" %in% colnames(data)) || sum(!is.na(data$species_name)) == 0) {
      stop("Species grouping requires detailed species data. No species_name column found or all values are NA.")
    }
    
    # Use detailed species data directly
    result <- data %>%
      filter(!is.na(species_name)) %>%
      group_by(time_group, species_name) %>%
      summarize(
        count = n_distinct(ainspecnum),  # Number of distinct SUCOs with this species
        total_count = sum(cnt, na.rm = TRUE),  # Total specimens of this species
        epi_week_label = first(epi_week_label),
        .groups = "drop"
      ) %>%
      arrange(time_group, species_name)
    
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
    
  } else if (group_col == "species") {
    # Species grouping requires detailed species data
    if (!("species_name" %in% colnames(data)) || sum(!is.na(data$species_name)) == 0) {
      stop("Species summary requires detailed species data. No species_name column found or all values are NA.")
    }
    
    # Summarize by species using detailed data
    summary_data <- data %>%
      filter(!is.na(species_name)) %>%
      group_by(species_name) %>%
      summarize(
        Total_SUCOs = n_distinct(ainspecnum), # Distinct SUCOs with this species
        Total_Species_Count = sum(cnt, na.rm = TRUE), # Total count of this species
        First_SUCO = min(inspdate),
        Last_SUCO = max(inspdate),
        .groups = "drop"
      ) %>%
      arrange(desc(Total_Species_Count), species_name)
    
    # Rename first column
    colnames(summary_data)[1] <- "Species"
    
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
          date_label = paste0("EW ", epiweek(inspdate)),
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
          date_label = paste0("EW ", epiweek(inspdate)),
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
        date_label = paste0("EW ", epiweek(inspdate)),
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