# Structure Treatment - Data Functions
# Functions for fetching and processing structure treatment data

# Helper function to construct facility filter condition for SQL
get_facility_condition <- function(facility_filter) {
  if (is.null(facility_filter) || ("all" %in% facility_filter)) {
    return("") # No filtering
  } else {
    facility_list <- paste0("'", paste(facility_filter, collapse = "','"), "'")
    return(sprintf("AND gis.facility IN (%s)", facility_list))
  }
}

# Function to construct structure type filter condition for SQL
get_structure_type_condition <- function(structure_type) {
  if (structure_type == "all") {
    return("")
  } else {
    # Handle case-insensitive matching and combinations like CV/PR
    if (toupper(structure_type) == "CV") {
      # Match CV, cv, and CV/PR, cv/pr combinations (case-insensitive)
      return("AND (UPPER(loc.s_type) = 'CV' OR UPPER(loc.s_type) LIKE 'CV/%' OR UPPER(loc.s_type) LIKE '%/CV')")
    } else if (toupper(structure_type) == "PR") {
      # Match PR and PR combinations like CV/PR (case-insensitive)
      return("AND (UPPER(loc.s_type) = 'PR' OR UPPER(loc.s_type) LIKE 'PR/%' OR UPPER(loc.s_type) LIKE '%/PR')")
    } else {
      # For other types, use case-insensitive exact match
      return(sprintf("AND UPPER(loc.s_type) = UPPER('%s')", structure_type))
    }
  }
}

# Function to construct priority filter condition for SQL
get_priority_condition <- function(priority) {
  if (priority == "all") {
    return("")
  } else {
    return(sprintf("AND loc.priority = '%s'", priority))
  }
}

# Function to construct status condition for SQL
get_status_condition <- function(status_types) {
  if (is.null(status_types) || length(status_types) == 0) {
    return("AND FALSE") # No statuses selected, return no results
  } else {
    status_list <- paste0("'", paste(status_types, collapse = "','"), "'")
    return(sprintf("AND loc.status_udw IN (%s)", status_list))
  }
}

# Function to construct foreman (FOS) filter condition for SQL
get_foreman_condition <- function(foreman_filter) {
  if (is.null(foreman_filter) || ("all" %in% foreman_filter) || length(foreman_filter) == 0) {
    return("") # No filtering
  } else {
    # Map shortnames to emp_num using employee_list
    # foreman_filter contains shortnames, but gis.fosarea contains emp_num
    # We need to query to get emp_num for the selected shortnames
    con <- get_db_connection()
    if (is.null(con)) return("")
    
    tryCatch({
      shortname_list <- paste0("'", paste(foreman_filter, collapse = "','"), "'")
      emp_nums <- dbGetQuery(con, sprintf("
        SELECT emp_num 
        FROM employee_list 
        WHERE shortname IN (%s)
      ", shortname_list))
      safe_disconnect(con)
      
      if (nrow(emp_nums) > 0) {
        emp_num_list <- paste0("'", paste(emp_nums$emp_num, collapse = "','"), "'")
        return(sprintf("AND gis.fosarea IN (%s)", emp_num_list))
      } else {
        return("")
      }
    }, error = function(e) {
      if (!is.null(con)) safe_disconnect(con)
      return("")
    })
  }
}

# Helper function for total structures query conditions
get_facility_condition_total <- function(facility_filter, structure_type_filter, priority_filter, status_types) {
  conditions <- character(0)
  
  # Facility condition using shared helper
  if (is_valid_filter(facility_filter)) {
    facility_list <- build_sql_in_list(facility_filter)
    conditions <- c(conditions, sprintf("AND gis.facility IN (%s)", facility_list))
  }
  
  # Structure type condition
  if (structure_type_filter != "all") {
    # Handle case-insensitive matching and combinations like CV/PR
    if (toupper(structure_type_filter) == "CV") {
      # Match CV, cv, and CV/PR, cv/pr combinations (case-insensitive)
      conditions <- c(conditions, "AND (UPPER(loc.s_type) = 'CV' OR UPPER(loc.s_type) LIKE 'CV/%' OR UPPER(loc.s_type) LIKE '%/CV')")
    } else if (toupper(structure_type_filter) == "PR") {
      # Match PR and PR combinations like CV/PR (case-insensitive)
      conditions <- c(conditions, "AND (UPPER(loc.s_type) = 'PR' OR UPPER(loc.s_type) LIKE 'PR/%' OR UPPER(loc.s_type) LIKE '%/PR')")
    } else {
      # For other types, use case-insensitive exact match
      conditions <- c(conditions, sprintf("AND UPPER(loc.s_type) = UPPER('%s')", structure_type_filter))
    }
  }
  
  # Priority condition
  if (priority_filter != "all") {
    conditions <- c(conditions, sprintf("AND loc.priority = '%s'", priority_filter))
  }
  
  # Status condition
  if (!is.null(status_types) && length(status_types) > 0) {
    status_list <- paste0("'", paste(status_types, collapse = "','"), "'")
    conditions <- c(conditions, sprintf("AND loc.status_udw IN (%s)", status_list))
  }
  
  return(paste(conditions, collapse = " "))
}

# Function to get current structure treatment data (STANDARD FUNCTION - renamed from get_current_structure_data)
load_raw_data <- function(analysis_date = Sys.Date(), include_archive = FALSE,
                          start_year = NULL, end_year = NULL, include_geometry = FALSE,
                          expiring_days = 7, facility_filter = "all", foreman_filter = "all", 
                          structure_type_filter = "all", priority_filter = "all", 
                          status_types = c("D", "W", "U"), zone_filter = c("1", "2")) {
  con <- get_db_connection()
  if (is.null(con)) return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
  
  tryCatch({
    # Query for current structure treatments using gis_sectcode for zone, fosarea, and facility
    query <- sprintf(
      "
SELECT DISTINCT ON (trt.sitecode)
  trt.sitecode,
  trt.inspdate,
  gis.facility,
  gis.fosarea as foreman,
  COALESCE(mat.effect_days, 30) AS effect_days,
  loc.s_type,
  loc.priority,
  loc.status_udw as status,
  gis.zone
FROM public.dblarv_insptrt_current trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode  
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE trt.list_type = 'STR'
  AND (loc.enddate IS NULL OR loc.enddate > CURRENT_DATE)
  AND trt.inspdate <= '%s'::date
  AND loc.startdate <= '%s'::date
%s
%s
%s
%s
%s
ORDER BY trt.sitecode, trt.inspdate DESC
",
      analysis_date,
      analysis_date,
      get_facility_condition(facility_filter),
      get_foreman_condition(foreman_filter),
      get_structure_type_condition(structure_type_filter),
      get_priority_condition(priority_filter),
      get_status_condition(status_types)
    )
    
    current_data <- dbGetQuery(con, query)
    
    # Get total structures count
    query_total <- sprintf(
      "
SELECT COUNT(DISTINCT loc.sitecode)::bigint AS total_count
FROM loc_cxstruct loc
LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE 1=1
AND (loc.enddate IS NULL OR loc.enddate > CURRENT_DATE)
%s
",
      get_facility_condition_total(facility_filter, structure_type_filter, priority_filter, status_types)
    )
    
    total_count <- as.integer(dbGetQuery(con, query_total)$total_count)
    safe_disconnect(con)
    
    # Process the data
    if (nrow(current_data) > 0) {
      current_data <- current_data %>%
        mutate(
          inspdate = as.Date(inspdate),
          enddate = inspdate + effect_days,
          days_since_treatment = as.numeric(analysis_date - inspdate),
          is_active = days_since_treatment <= effect_days,
          is_expiring = days_since_treatment > (effect_days - expiring_days) & days_since_treatment <= effect_days
        ) %>%
        # Filter by zone
        filter(zone %in% zone_filter) %>%
        # Map facility names
        {map_facility_names(.)}
    }
    
    # Return STANDARDIZED format - same as all other apps
    # For struct_trt, each treatment row IS a unique site (due to DISTINCT ON sitecode)
    # So sites = treatments (they're the same data here)
    return(list(
      sites = current_data,
      treatments = current_data,
      total_count = total_count
    ))
    
  }, error = function(e) {
    warning(paste("Error loading current structure data:", e$message))
    if (!is.null(con)) safe_disconnect(con)
    return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
  })
}

#' Apply filters to structure data - STANDARDIZED FORMAT
#' Standard function to filter structure data
#' @param data The result from load_raw_data (list with sites, treatments, total_count)
#' @param facility_filter Vector of selected facilities
#' @param foreman_filter Vector of selected foremen  
#' @param zone_filter Vector of selected zones
#' @return Filtered data list with standardized keys
apply_data_filters <- function(data, facility_filter = NULL,
                              foreman_filter = NULL, zone_filter = NULL) {
  # Use standardized keys
  sites <- data$sites
  treatments <- data$treatments
  
  if (is.null(sites) || nrow(sites) == 0) {
    return(list(sites = data.frame(), treatments = data.frame(), total_count = 0))
  }
  
  # Apply facility filter
  if (!is.null(facility_filter) && !("all" %in% facility_filter) && length(facility_filter) > 0) {
    sites <- sites %>% filter(facility %in% facility_filter)
    treatments <- treatments %>% filter(facility %in% facility_filter)
  }
  
  # Apply foreman filter
  if (!is.null(foreman_filter) && !("all" %in% foreman_filter) && length(foreman_filter) > 0) {
    sites <- sites %>% filter(foreman %in% foreman_filter)
    treatments <- treatments %>% filter(foreman %in% foreman_filter)
  }
  
  # Apply zone filter  
  if (!is.null(zone_filter) && length(zone_filter) > 0) {
    sites <- sites %>% filter(zone %in% zone_filter)
    treatments <- treatments %>% filter(zone %in% zone_filter)
  }
  
  # Return STANDARDIZED format
  return(list(
    sites = sites,
    treatments = treatments,
    total_count = data$total_count  # Keep original total_count from universe
  ))
}

# Function to get historical structure treatment data
get_historical_structure_data <- function(start_year = 2023, end_year = 2025, facility_filter = "all", structure_type_filter = "all", priority_filter = "all", status_types = c("D", "W", "U"), zone_filter = c("1", "2")) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Construct zone filter condition using gis_sectcode
    zone_condition <- ""
    if (!is.null(zone_filter) && length(zone_filter) > 0) {
      zone_list <- paste0("'", paste(zone_filter, collapse = "','"), "'")
      zone_condition <- sprintf("AND gis.zone IN (%s)", zone_list)
    }
    
    # Fetch archive data using gis_sectcode for zone, fosarea, and facility
    query_archive <- sprintf(
      "
SELECT DISTINCT ON (trt.sitecode)
trt.sitecode,
trt.inspdate,
COALESCE(mat.effect_days, 30) AS effect_days,
loc.s_type,
loc.priority,
gis.facility,
gis.fosarea as foreman,
gis.zone
FROM public.dblarv_insptrt_archive trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE trt.inspdate >= date '%d-01-01'
AND trt.inspdate < date '%d-01-01'
AND trt.list_type = 'STR'
AND (loc.enddate IS NULL OR loc.enddate > trt.inspdate)
%s
%s
%s
%s
%s
ORDER BY trt.sitecode, trt.inspdate DESC
",
      as.numeric(start_year),
      as.numeric(end_year) + 1,
      zone_condition,
      get_facility_condition(facility_filter),
      get_structure_type_condition(structure_type_filter),
      get_priority_condition(priority_filter),
      get_status_condition(status_types)
    )
    
    archive_data <- dbGetQuery(con, query_archive)
    
    # Fetch current data with structure info using gis_sectcode
    query_current <- sprintf(
      "
SELECT DISTINCT ON (trt.sitecode)
trt.sitecode,
trt.inspdate,
COALESCE(mat.effect_days, 30) AS effect_days,
loc.s_type,
loc.priority,
gis.facility,
gis.fosarea as foreman,
gis.zone
FROM public.dblarv_insptrt_current trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE trt.inspdate >= date '%d-01-01'
AND trt.inspdate < date '%d-01-01'
AND trt.list_type = 'STR'
AND (loc.enddate IS NULL OR loc.enddate > trt.inspdate)
%s
%s
%s
%s
%s
ORDER BY trt.sitecode, trt.inspdate DESC
",
      as.numeric(start_year),
      as.numeric(end_year) + 1,
      zone_condition,
      get_facility_condition(facility_filter),
      get_structure_type_condition(structure_type_filter),
      get_priority_condition(priority_filter),
      get_status_condition(status_types)
    )
    
    current_data <- dbGetQuery(con, query_current)
    
    # Get total structures (active structures only)
    query_total_count <- sprintf(
      "
SELECT COUNT(DISTINCT loc.sitecode) AS total_count
FROM loc_cxstruct loc
LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE 1=1
AND (loc.enddate IS NULL OR loc.enddate > CURRENT_DATE)
%s
",
      get_facility_condition_total(facility_filter, structure_type_filter, priority_filter, status_types)
    )
    
    total_count <- as.numeric(dbGetQuery(con, query_total_count)$total_count)
    safe_disconnect(con)
    
    # Combine and process data
    combined_data <- bind_rows(archive_data, current_data) %>%
      mutate(inspdate = as.Date(inspdate),
             enddate = inspdate + effect_days)
    
    return(list(
      treatments = combined_data,
      total_count = total_count
    ))
    
  }, error = function(e) {
    warning(paste("Error loading historical structure data:", e$message))
    if (!is.null(con)) safe_disconnect(con)
    return(list(treatments = data.frame(), total_count = 0))
  })
}

# Function to get all structures for total counts
get_all_structures <- function(facility_filter = "all", foreman_filter = "all", structure_type_filter = "all", priority_filter = "all", status_types = c("D", "W", "U"), zone_filter = c("1", "2")) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Get all active structures with zone, fosarea, and facility from gis_sectcode
    query <- sprintf(
      "
SELECT 
  loc.sitecode,
  gis.facility,
  loc.s_type,
  loc.priority,
  loc.status_udw as status,
  gis.zone,
  gis.fosarea as foreman
FROM public.loc_cxstruct loc
LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE (loc.enddate IS NULL OR loc.enddate > CURRENT_DATE)
%s
%s
%s
%s
%s
",
      get_facility_condition(facility_filter),
      get_foreman_condition(foreman_filter),
      get_structure_type_condition(structure_type_filter),
      get_priority_condition(priority_filter),
      get_status_condition(status_types)
    )
    
    structures <- dbGetQuery(con, query)
    safe_disconnect(con)
    
    # Filter by zone and map facility names
    if (nrow(structures) > 0) {
      structures <- structures %>%
        filter(zone %in% zone_filter) %>%
        {map_facility_names(.)}
    }
    
    return(structures)
    
  }, error = function(e) {
    warning(paste("Error loading structures:", e$message))
    if (!is.null(con)) safe_disconnect(con)
    return(data.frame())
  })
}

# Function to process and aggregate structure data by group
aggregate_structure_data <- function(structures, treatments, group_by = "facility", zone_filter = c("1", "2"), combine_zones = FALSE) {
  if (nrow(structures) == 0) {
    return(data.frame())
  }
  
  # Determine grouping column
  group_col <- case_when(
    group_by == "facility" ~ "facility",
    group_by == "foreman" ~ "foreman", 
    group_by == "mmcd_all" ~ "mmcd_all",
    TRUE ~ "facility"
  )
  
  # Add group column for mmcd_all case
  if (group_col == "mmcd_all") {
    structures$mmcd_all <- "All MMCD"
    if (nrow(treatments) > 0) {
      treatments$mmcd_all <- "All MMCD"
    }
  }
  
  # Create combined group for zone display
  if (length(zone_filter) > 1 && !combine_zones) {
    # Show zones separately using standardized format: "Name P1"
    structures$combined_group <- paste0(structures[[group_col]], " P", structures$zone)
    if (nrow(treatments) > 0) {
      treatments$combined_group <- paste0(treatments[[group_col]], " P", treatments$zone)
    }
  }
  
  # Count total structures by group
  if (length(zone_filter) > 1 && !combine_zones) {
    # Separate zones - group by combined_group
    total_count <- structures %>%
      group_by(combined_group) %>%
      summarize(total_count = n(), .groups = 'drop')
  } else {
    # For single zone OR combined zones, group by main column
    if (group_col == "facility") {
      total_count <- structures %>%
        group_by(!!sym(group_col)) %>%
        summarize(
          total_count = n(),
          facility_display = first(facility_display),
          .groups = 'drop'
        )
    } else if (group_col == "foreman") {
      total_count <- structures %>%
        group_by(!!sym(group_col)) %>%
        summarize(
          total_count = n(),
          .groups = 'drop'
        )
    } else {
      total_count <- structures %>%
        group_by(!!sym(group_col)) %>%
        summarize(total_count = n(), .groups = 'drop')
    }
  }
  
  # Count active structures by group
  if (nrow(treatments) > 0) {
    if (length(zone_filter) > 1 && !combine_zones) {
      # Separate zones
      active_count <- treatments %>%
        filter(is_active == TRUE) %>%
        distinct(sitecode, combined_group) %>%
        group_by(combined_group) %>%
        summarize(active_count = n(), .groups = 'drop')
      
      expiring_count <- treatments %>%
        filter(is_expiring == TRUE) %>%
        distinct(sitecode, combined_group) %>%
        group_by(combined_group) %>%
        summarize(expiring_count = n(), .groups = 'drop')
    } else {
      # Single zone OR combined zones
      active_count <- treatments %>%
        filter(is_active == TRUE) %>%
        distinct(sitecode, !!sym(group_col)) %>%
        group_by(!!sym(group_col)) %>%
        summarize(active_count = n(), .groups = 'drop')
      
      expiring_count <- treatments %>%
        filter(is_expiring == TRUE) %>%
        distinct(sitecode, !!sym(group_col)) %>%
        group_by(!!sym(group_col)) %>%
        summarize(expiring_count = n(), .groups = 'drop')
    }
  } else {
    active_count <- data.frame()
    expiring_count <- data.frame()
  }
  
  # Combine all counts
  if (length(zone_filter) > 1 && !combine_zones) {
    # Separate zones - use combined_group
    combined_data <- total_count %>%
      left_join(active_count, by = "combined_group") %>%
      left_join(expiring_count, by = "combined_group") %>%
      mutate(
        active_count = ifelse(is.na(active_count), 0, active_count),
        expiring_count = ifelse(is.na(expiring_count), 0, expiring_count),
        group_name = sapply(strsplit(combined_group, " \\(P"), function(x) x[1])
      )
    
    # Create proper display names based on group type
    if (group_col == "foreman") {
      # For foreman, convert employee numbers to shortnames with zones
      foremen_lookup <- get_foremen_lookup()
      combined_data$display_name <- sapply(combined_data$combined_group, function(cg) {
        # Extract emp_num and zone from combined_group like "0203 P1"
        base_name <- sub(" P[12]$", "", cg)
        zone_match <- regmatches(cg, regexpr(" P[12]$", cg))
        zone_part <- if(length(zone_match) > 0) zone_match else ""
        
        # Look up shortname
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == trimws(as.character(base_name)))
        if(length(matches) > 0) {
          shortname <- foremen_lookup$shortname[matches[1]]
          return(paste0(shortname, zone_part))
        } else {
          return(paste0("FOS #", base_name, zone_part))
        }
      })
    } else if (group_col == "facility") {
      # For facility, map short names to full names with zones
      facilities <- get_facility_lookup()
      facility_map <- setNames(facilities$full_name, facilities$short_name)
      
      combined_data$display_name <- sapply(combined_data$combined_group, function(cg) {
        # Extract facility and zone from combined_group like "Sr P2"
        base_name <- sub(" P[12]$", "", cg)
        zone_match <- regmatches(cg, regexpr(" P[12]$", cg))
        zone_part <- if(length(zone_match) > 0) zone_match else ""
        
        # Map facility name
        if (base_name %in% names(facility_map)) {
          facility_long <- facility_map[base_name]
          return(paste0(facility_long, zone_part))
        } else {
          return(cg)  # fallback to original if no mapping found
        }
      })
    } else {
      combined_data$display_name <- combined_data$combined_group
    }
  } else {
    join_col <- group_col
    combined_data <- total_count %>%
      left_join(active_count, by = join_col) %>%
      left_join(expiring_count, by = join_col) %>%
      mutate(
        active_count = ifelse(is.na(active_count), 0, active_count),
        expiring_count = ifelse(is.na(expiring_count), 0, expiring_count)
      )
    
    # Add display names
    if (group_col == "facility") {
      combined_data$display_name <- combined_data$facility_display
    } else if (group_col == "foreman") {
      foremen_lookup <- get_foremen_lookup()
      combined_data$display_name <- sapply(combined_data$foreman, function(f) {
        matches <- which(trimws(as.character(foremen_lookup$emp_num)) == trimws(as.character(f)))
        if(length(matches) > 0) foremen_lookup$shortname[matches[1]] else paste0("FOS #", f)
      })
    } else {
      combined_data$display_name <- combined_data[[group_col]]
    }
    
    combined_data$group_name <- combined_data[[group_col]]
  }
  
  return(combined_data)
}
