# Structure Treatment - Data Functions
# Functions for fetching and processing structure treatment data

# Helper function to construct facility filter condition for SQL
get_facility_condition <- function(facility_filter) {
  if (is.null(facility_filter) || ("all" %in% facility_filter)) {
    return("") # No filtering
  } else {
    facility_list <- paste0("'", paste(facility_filter, collapse = "','"), "'")
    return(sprintf("AND trt.facility IN (%s)", facility_list))
  }
}

# Function to construct structure type filter condition for SQL
get_structure_type_condition <- function(structure_type) {
  if (structure_type == "all") {
    return("")
  } else {
    return(sprintf("AND loc.s_type = '%s'", structure_type))
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
    return(sprintf("AND loc.status IN (%s)", status_list))
  }
}

# Helper function for total structures query conditions
get_facility_condition_total <- function(facility_filter, structure_type_filter, priority_filter, status_types) {
  conditions <- character(0)
  
  # Facility condition
  if (!is.null(facility_filter) && !("all" %in% facility_filter)) {
    facility_list <- paste0("'", paste(facility_filter, collapse = "','"), "'")
    conditions <- c(conditions, sprintf("AND facility IN (%s)", facility_list))
  }
  
  # Structure type condition
  if (structure_type_filter != "all") {
    conditions <- c(conditions, sprintf("AND s_type = '%s'", structure_type_filter))
  }
  
  # Priority condition
  if (priority_filter != "all") {
    conditions <- c(conditions, sprintf("AND priority = '%s'", priority_filter))
  }
  
  # Status condition
  if (!is.null(status_types) && length(status_types) > 0) {
    status_list <- paste0("'", paste(status_types, collapse = "','"), "'")
    conditions <- c(conditions, sprintf("AND status IN (%s)", status_list))
  }
  
  return(paste(conditions, collapse = " "))
}

# Function to get current structure treatment data
get_current_structure_data <- function(custom_today, expiring_days, facility_filter, structure_type_filter, priority_filter, status_types, zone_filter) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Query for current structure treatments
    query <- sprintf(
      "
SELECT 
  trt.sitecode,
  trt.inspdate,
  trt.facility,
  trt.fosarea as foreman,
  COALESCE(mat.effect_days, 0) AS effect_days,
  loc.s_type,
  loc.priority,
  loc.status,
  sc.zone
FROM public.dblarv_insptrt_current trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode  
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
LEFT JOIN public.gis_sectcode sc ON LEFT(trt.sitecode, 7) = sc.sectcode
WHERE trt.list_type = 'STR'
%s
%s
%s
%s
",
      get_facility_condition(facility_filter),
      get_structure_type_condition(structure_type_filter),
      get_priority_condition(priority_filter),
      get_status_condition(status_types)
    )
    
    current_data <- dbGetQuery(con, query)
    
    # Get total structures count
    query_total <- sprintf(
      "
SELECT COUNT(DISTINCT sitecode) AS total_structures
FROM loc_cxstruct
WHERE 1=1
AND (enddate IS NULL OR enddate > CURRENT_DATE)
%s
",
      get_facility_condition_total(facility_filter, structure_type_filter, priority_filter, status_types)
    )
    
    total_structures <- dbGetQuery(con, query_total)$total_structures
    dbDisconnect(con)
    
    # Process the data
    if (nrow(current_data) > 0) {
      current_data <- current_data %>%
        mutate(
          inspdate = as.Date(inspdate),
          enddate = inspdate + effect_days,
          days_since_treatment = as.numeric(custom_today - inspdate),
          is_active = days_since_treatment <= effect_days,
          is_expiring = days_since_treatment > (effect_days - expiring_days) & days_since_treatment <= effect_days
        ) %>%
        # Filter by zone
        filter(zone %in% zone_filter) %>%
        # Map facility names
        {map_facility_names(.)}
    }
    
    return(list(
      treatments = current_data,
      total_structures = total_structures
    ))
    
  }, error = function(e) {
    warning(paste("Error loading current structure data:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(list(treatments = data.frame(), total_structures = 0))
  })
}

# Function to get historical structure treatment data
get_historical_structure_data <- function(start_year, end_year, facility_filter, structure_type_filter, priority_filter, status_types) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Fetch archive data
    query_archive <- sprintf(
      "
SELECT
trt.sitecode,
trt.inspdate,
COALESCE(mat.effect_days, 0) AS effect_days,
loc.s_type,
loc.priority,
loc.facility
FROM public.dblarv_insptrt_archive trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
WHERE trt.inspdate >= date '%d-01-01'
AND trt.inspdate < date '%d-01-01'
AND trt.list_type = 'STR'
%s
%s
%s
%s
",
      as.numeric(start_year),
      as.numeric(end_year) + 1,
      get_facility_condition(facility_filter),
      get_structure_type_condition(structure_type_filter),
      get_priority_condition(priority_filter),
      get_status_condition(status_types)
    )
    
    archive_data <- dbGetQuery(con, query_archive)
    
    # Fetch current data with structure info
    query_current <- sprintf(
      "
SELECT
trt.sitecode,
trt.inspdate,
COALESCE(mat.effect_days, 0) AS effect_days,
loc.s_type,
loc.priority,
loc.facility
FROM public.dblarv_insptrt_current trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
WHERE trt.inspdate >= date '%d-01-01'
AND trt.inspdate < date '%d-01-01'
AND trt.list_type = 'STR'
%s
%s
%s
%s
",
      as.numeric(start_year),
      as.numeric(end_year) + 1,
      get_facility_condition(facility_filter),
      get_structure_type_condition(structure_type_filter),
      get_priority_condition(priority_filter),
      get_status_condition(status_types)
    )
    
    current_data <- dbGetQuery(con, query_current)
    
    # Get total structures (active structures only)
    query_total_structures <- sprintf(
      "
SELECT COUNT(DISTINCT sitecode) AS total_structures
FROM loc_cxstruct
WHERE 1=1
AND (enddate IS NULL OR enddate > CURRENT_DATE)
%s
",
      get_facility_condition_total(facility_filter, structure_type_filter, priority_filter, status_types)
    )
    
    total_structures <- dbGetQuery(con, query_total_structures)$total_structures
    dbDisconnect(con)
    
    # Combine and process data
    combined_data <- bind_rows(archive_data, current_data) %>%
      mutate(inspdate = as.Date(inspdate),
             enddate = inspdate + effect_days)
    
    return(list(
      treatments = combined_data,
      total_structures = total_structures
    ))
    
  }, error = function(e) {
    warning(paste("Error loading historical structure data:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(list(treatments = data.frame(), total_structures = 0))
  })
}

# Function to get all structures for total counts
get_all_structures <- function(facility_filter, structure_type_filter, priority_filter, status_types, zone_filter) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Get all active structures with zone info
    query <- sprintf(
      "
SELECT 
  loc.sitecode,
  loc.facility,
  loc.s_type,
  loc.priority,
  loc.status,
  sc.zone,
  sc.fosarea as foreman
FROM public.loc_cxstruct loc
LEFT JOIN public.gis_sectcode sc ON LEFT(loc.sitecode, 7) = sc.sectcode
WHERE (loc.enddate IS NULL OR loc.enddate > CURRENT_DATE)
%s
",
      get_facility_condition_total(facility_filter, structure_type_filter, priority_filter, status_types)
    )
    
    structures <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    # Filter by zone and map facility names
    if (nrow(structures) > 0) {
      structures <- structures %>%
        filter(zone %in% zone_filter) %>%
        {map_facility_names(.)}
    }
    
    return(structures)
    
  }, error = function(e) {
    warning(paste("Error loading structures:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}

# Function to process and aggregate structure data by group
aggregate_structure_data <- function(structures, treatments, group_by, zone_filter) {
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
  if (length(zone_filter) > 1) {
    structures$combined_group <- paste0(structures[[group_col]], " (P", structures$zone, ")")
    if (nrow(treatments) > 0) {
      treatments$combined_group <- paste0(treatments[[group_col]], " (P", treatments$zone, ")")
    }
  }
  
  # Count total structures by group
  if (length(zone_filter) > 1) {
    total_structures <- structures %>%
      group_by(combined_group) %>%
      summarize(total_structures = n(), .groups = 'drop')
  } else {
    total_structures <- structures %>%
      group_by(!!sym(group_col)) %>%
      summarize(total_structures = n(), .groups = 'drop')
  }
  
  # Count active structures by group
  if (nrow(treatments) > 0) {
    if (length(zone_filter) > 1) {
      active_structures <- treatments %>%
        filter(is_active == TRUE) %>%
        distinct(sitecode, combined_group) %>%
        group_by(combined_group) %>%
        summarize(active_structures = n(), .groups = 'drop')
      
      expiring_structures <- treatments %>%
        filter(is_expiring == TRUE) %>%
        distinct(sitecode, combined_group) %>%
        group_by(combined_group) %>%
        summarize(expiring_structures = n(), .groups = 'drop')
    } else {
      active_structures <- treatments %>%
        filter(is_active == TRUE) %>%
        distinct(sitecode, !!sym(group_col)) %>%
        group_by(!!sym(group_col)) %>%
        summarize(active_structures = n(), .groups = 'drop')
      
      expiring_structures <- treatments %>%
        filter(is_expiring == TRUE) %>%
        distinct(sitecode, !!sym(group_col)) %>%
        group_by(!!sym(group_col)) %>%
        summarize(expiring_structures = n(), .groups = 'drop')
    }
  } else {
    active_structures <- data.frame()
    expiring_structures <- data.frame()
  }
  
  # Combine all counts
  if (length(zone_filter) > 1) {
    combined_data <- total_structures %>%
      left_join(active_structures, by = "combined_group") %>%
      left_join(expiring_structures, by = "combined_group") %>%
      mutate(
        active_structures = ifelse(is.na(active_structures), 0, active_structures),
        expiring_structures = ifelse(is.na(expiring_structures), 0, expiring_structures),
        display_name = combined_group,
        group_name = sapply(strsplit(combined_group, " \\(P"), function(x) x[1])
      )
  } else {
    join_col <- group_col
    combined_data <- total_structures %>%
      left_join(active_structures, by = join_col) %>%
      left_join(expiring_structures, by = join_col) %>%
      mutate(
        active_structures = ifelse(is.na(active_structures), 0, active_structures),
        expiring_structures = ifelse(is.na(expiring_structures), 0, expiring_structures)
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