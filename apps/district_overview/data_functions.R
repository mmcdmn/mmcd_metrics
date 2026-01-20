# District Overview - Data Functions
# REUSES existing functions from other apps - no duplicate SQL!
# 
# SOLUTION FOR FUNCTION NAME COLLISIONS:
# Instead of sourcing all files at once (which causes collisions),
# we use local() environments to isolate each app's functions.

# =============================================================================
# FACILITY ORDERING FUNCTIONS - Use database lookup for consistent ordering
# =============================================================================

#' Get consistent facility ordering from database
get_facility_order <- function() {
  facilities <- get_facility_lookup()
  if (nrow(facilities) == 0) {
    # Fallback order if database query fails
    return(c("East", "North", "South Jordan", "South Reserve", "West Metro"))
  }
  return(facilities$full_name)
}

#' Order facilities consistently across all charts
#' @param data Data frame with display_name column
#' @param separate_zones Whether zones are separated (affects factor levels)
order_facilities <- function(data, separate_zones = FALSE) {
  if (!"display_name" %in% names(data)) {
    return(data)
  }
  
  facility_order <- get_facility_order()
  
  if (separate_zones) {
    # For separate zones, create levels like "East (P1)", "East (P2)", etc.
    zone_levels <- unlist(lapply(facility_order, function(f) {
      c(paste0(f, " (P1)"), paste0(f, " (P2)"))
    }))
    data <- data %>%
      mutate(display_name = factor(display_name, levels = zone_levels))
  } else {
    # For combined facilities, use facility order
    data <- data %>%
      mutate(display_name = factor(display_name, levels = facility_order))
  }
  
  # Sort by the factor levels
  data <- data %>% arrange(display_name)
  
  return(data)
}

# =============================================================================
# ISOLATED ENVIRONMENT LOADING
# =============================================================================

# Load catch basin functions in isolated environment
catch_basin_env <- local({
  source("../catch_basin_status/data_functions.R", local = TRUE)
  environment()
})

# Load drone functions in isolated environment  
drone_env <- local({
  source("../drone/data_functions.R", local = TRUE)
  source("../drone/display_functions.R", local = TRUE)
  environment()
})

# Load ground prehatch functions in isolated environment
ground_prehatch_env <- local({
  source("../ground_prehatch_progress/data_functions.R", local = TRUE)
  environment()
})

# Load structure functions in isolated environment
struct_env <- local({
  source("../struct_trt/data_functions.R", local = TRUE)
  environment()
})

# =============================================================================
# WRAPPER FUNCTIONS - Call existing app functions and summarize by facility
# =============================================================================

#' Load catch basin overview using existing app function
#' @param zone_filter Zone filter ("1", "2", or c("1", "2"))
#' @param separate_zones If TRUE, show P1 and P2 as separate bars
load_catch_basin_overview <- function(zone_filter = c("1", "2"), 
                                       custom_today = Sys.Date(), 
                                       expiring_days = 14,
                                       separate_zones = FALSE) {
  cat("CB Overview: date =", as.character(custom_today), "zones =", paste(zone_filter, collapse=","), 
      "separate =", separate_zones, "\n")
  
  # Call function from catch basin environment
  data <- tryCatch({
    catch_basin_env$load_catch_basin_data(
      facility_filter = "all",
      foreman_filter = "all",
      zone_filter = zone_filter,
      custom_today = custom_today,
      expiring_days = expiring_days
    )
  }, error = function(e) {
    cat("CB: load_catch_basin_data error:", e$message, "\n")
    return(data.frame())
  })
  
  if (is.null(data) || nrow(data) == 0) {
    cat("CB: No data returned\n")
    return(data.frame())
  }
  
  cat("CB: Got", nrow(data), "rows, aggregating by facility\n")
  
  # Aggregate by facility (and zone if separate)
  if (separate_zones && length(zone_filter) == 2) {
    result <- data %>%
      group_by(facility, zone) %>%
      summarize(
        total = sum(wet_cb_count, na.rm = TRUE),
        active = sum(count_wet_activetrt, na.rm = TRUE),
        expiring = sum(count_wet_expiring, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(display_name = facility) %>%
      map_facility_names(facility_col = "display_name") %>%
      mutate(display_name = paste0(display_name_display, " (P", zone, ")")) %>%
      select(-display_name_display)
  } else {
    result <- data %>%
      group_by(facility) %>%
      summarize(
        total = sum(wet_cb_count, na.rm = TRUE),
        active = sum(count_wet_activetrt, na.rm = TRUE),
        expiring = sum(count_wet_expiring, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(display_name = facility) %>%
      map_facility_names(facility_col = "display_name") %>%
      mutate(display_name = display_name_display) %>%
      select(-display_name_display)
  }
  
  # Apply consistent ordering
  result <- order_facilities(result, separate_zones)
  cat("CB: Aggregated to", nrow(result), "rows\n")
  return(result)
}


#' Load drone overview using isolated drone environment
#' @param separate_zones If TRUE, show P1 and P2 as separate bars
load_drone_overview <- function(zone_filter = c("1", "2"),
                                 analysis_date = Sys.Date(),
                                 expiring_days = 7,
                                 separate_zones = FALSE) {
  cat("Drone Overview: date =", as.character(analysis_date), "zones =", paste(zone_filter, collapse=","),
      "separate =", separate_zones, "\n")
  
  # Load raw data using drone environment
  raw <- tryCatch({
    drone_env$load_raw_data(
      drone_types = c("Y", "M", "C"),
      analysis_date = analysis_date
    )
  }, error = function(e) {
    cat("Drone: load_raw_data error:", e$message, "\n")
    return(list(drone_sites = data.frame(), drone_treatments = data.frame()))
  })
  
  if (is.null(raw$drone_sites) || nrow(raw$drone_sites) == 0) {
    cat("Drone: No raw sites data\n")
    return(data.frame())
  }
  
  cat("Drone: Got", nrow(raw$drone_sites), "sites,", nrow(raw$drone_treatments), "treatments\n")
  
  # Apply filters using drone environment
  filtered <- tryCatch({
    drone_env$apply_data_filters(
      data = raw,
      facility_filter = "all",
      foreman_filter = "all",
      prehatch_only = FALSE
    )
  }, error = function(e) {
    cat("Drone: apply_data_filters error:", e$message, "\n")
    return(list(drone_sites = data.frame(), drone_treatments = data.frame()))
  })
  
  if (is.null(filtered$drone_sites) || nrow(filtered$drone_sites) == 0) {
    cat("Drone: No filtered sites data\n")
    return(data.frame())
  }
  
  cat("Drone: Filtered to", nrow(filtered$drone_sites), "sites\n")
  
  # For separate zones, we need to call process_current_data with combine_zones = FALSE
  combine_zones_param <- !separate_zones
  
  # Process using drone environment's display function
  processed <- tryCatch({
    drone_env$process_current_data(
      drone_sites = filtered$drone_sites,
      drone_treatments = filtered$drone_treatments,
      zone_filter = zone_filter,
      combine_zones = combine_zones_param,
      expiring_days = expiring_days,
      group_by = "facility",
      analysis_date = analysis_date
    )
  }, error = function(e) {
    cat("Drone: process_current_data error:", e$message, "\n")
    return(list(data = data.frame()))
  })
  
  if (is.null(processed$data) || nrow(processed$data) == 0) {
    cat("Drone: No processed data\n")
    return(data.frame())
  }
  
  cat("Drone: Processed", nrow(processed$data), "rows\n")
  
  # Return in standard format - drone already has proper display_name from process_current_data
  result <- processed$data %>%
    transmute(
      facility = facility,
      zone = if ("zone" %in% names(processed$data)) zone else NA,
      display_name = display_name,
      total = total_sites,
      active = active_sites,
      expiring = expiring_sites
    )
  
  # Apply consistent ordering
  result <- order_facilities(result, separate_zones)
  return(result)
}


#' Load ground prehatch overview using isolated environment
#' @param separate_zones If TRUE, show P1 and P2 as separate bars
load_ground_prehatch_overview <- function(zone_filter = c("1", "2"),
                                           simulation_date = Sys.Date(),
                                           expiring_days = 14,
                                           separate_zones = FALSE) {
  cat("Ground Prehatch Overview: date =", as.character(simulation_date), "zones =", paste(zone_filter, collapse=","),
      "separate =", separate_zones, "\n")
  
  # Call function from ground prehatch environment
  data <- tryCatch({
    ground_prehatch_env$get_ground_prehatch_data(
      zone_filter = zone_filter,
      simulation_date = simulation_date,
      expiring_days = expiring_days
    )
  }, error = function(e) {
    cat("Ground: get_ground_prehatch_data error:", e$message, "\n")
    return(data.frame())
  })
  
  if (is.null(data) || nrow(data) == 0) {
    cat("Ground: No data returned\n")
    return(data.frame())
  }
  
  cat("Ground: Got", nrow(data), "rows, aggregating by facility\n")
  
  # Aggregate by facility (and zone if separate)
  if (separate_zones && length(zone_filter) == 2 && "zone" %in% names(data)) {
    result <- data %>%
      group_by(facility, zone) %>%
      summarize(
        total = sum(prehatch_sites_cnt, na.rm = TRUE),
        active = sum(ph_treated_cnt, na.rm = TRUE) + sum(ph_expiring_cnt, na.rm = TRUE),
        expiring = sum(ph_expiring_cnt, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(display_name = facility) %>%
      map_facility_names(facility_col = "display_name") %>%
      mutate(display_name = paste0(display_name_display, " (P", zone, ")")) %>%
      select(-display_name_display)
  } else {
    result <- data %>%
      group_by(facility) %>%
      summarize(
        total = sum(prehatch_sites_cnt, na.rm = TRUE),
        active = sum(ph_treated_cnt, na.rm = TRUE) + sum(ph_expiring_cnt, na.rm = TRUE),
        expiring = sum(ph_expiring_cnt, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(display_name = facility) %>%
      map_facility_names(facility_col = "display_name") %>%
      mutate(display_name = display_name_display) %>%
      select(-display_name_display)
  }
  
  # Apply consistent ordering
  result <- order_facilities(result, separate_zones)
  cat("Ground: Aggregated to", nrow(result), "rows\n")
  return(result)
}


#' Load structure overview using isolated environment
#' MATCHES struct_trt app logic exactly:
#' - Total = ALL structures from get_all_structures() (loc_cxstruct)
#' - Active/Expiring = from treatments (dblarv_insptrt_current with is_active flag)
#' 
#' The struct_trt app calls:
#' 1. all_structures() -> get_all_structures() -> queries loc_cxstruct (ALL structures)
#' 2. current_data() -> get_current_structure_data() -> queries dblarv_insptrt_current (treatments only)
#' 3. aggregate_structure_data(structures, treatments) -> combines them
#' 
#' @param separate_zones If TRUE, show P1 and P2 as separate bars
load_structure_overview <- function(zone_filter = c("1", "2"),
                                     custom_today = Sys.Date(),
                                     expiring_days = 7,
                                     separate_zones = FALSE) {
  cat("Structure Overview: date =", as.character(custom_today), "zones =", paste(zone_filter, collapse=","),
      "separate =", separate_zones, "\n")
  
  # =========================================================================
  # STEP 1: Get ALL structures (for total count) - matches struct_trt's all_structures()
  # =========================================================================
  all_structures <- tryCatch({
    struct_env$get_all_structures(
      facility_filter = "all",
      foreman_filter = "all",
      structure_type_filter = "all",
      priority_filter = "all",
      status_types = c("D", "W", "U"),
      zone_filter = zone_filter
    )
  }, error = function(e) {
    cat("Structure: get_all_structures error:", e$message, "\n")
    return(data.frame())
  })
  
  if (is.null(all_structures) || nrow(all_structures) == 0) {
    cat("Structure: No structures data\n")
    return(data.frame())
  }
  
  cat("Structure: Got", nrow(all_structures), "total structures\n")
  
  # =========================================================================
  # STEP 2: Get treatments (for active/expiring counts) - matches struct_trt's current_data()
  # =========================================================================
  treatment_result <- tryCatch({
    struct_env$get_current_structure_data(
      custom_today = custom_today,
      expiring_days = expiring_days,
      facility_filter = "all",
      foreman_filter = "all",
      structure_type_filter = "all",
      priority_filter = "all",
      status_types = c("D", "W", "U"),
      zone_filter = zone_filter
    )
  }, error = function(e) {
    cat("Structure: get_current_structure_data error:", e$message, "\n")
    return(list(treatments = data.frame()))
  })
  
  treatments <- treatment_result$treatments
  if (is.null(treatments)) treatments <- data.frame()
  
  cat("Structure: Got", nrow(treatments), "treatment rows\n")
  
  # =========================================================================
  # STEP 3: Aggregate - TOTAL from all_structures, ACTIVE/EXPIRING from treatments
  # This matches struct_trt's aggregate_structure_data() logic
  # =========================================================================
  
  if (separate_zones && length(zone_filter) == 2 && "zone" %in% names(all_structures)) {
    # Count TOTAL structures by facility + zone (from all_structures)
    total_counts <- all_structures %>%
      group_by(facility, zone) %>%
      summarize(total = n_distinct(sitecode), .groups = "drop")
    
    # Count active structures (DISTINCT sitecodes from treatments)
    if (nrow(treatments) > 0 && "is_active" %in% names(treatments)) {
      active_counts <- treatments %>%
        filter(is_active == TRUE) %>%
        distinct(sitecode, facility, zone) %>%
        group_by(facility, zone) %>%
        summarize(active = n(), .groups = "drop")
      
      expiring_counts <- treatments %>%
        filter(is_expiring == TRUE) %>%
        distinct(sitecode, facility, zone) %>%
        group_by(facility, zone) %>%
        summarize(expiring = n(), .groups = "drop")
    } else {
      active_counts <- data.frame(facility = character(), zone = character(), active = integer())
      expiring_counts <- data.frame(facility = character(), zone = character(), expiring = integer())
    }
    
    # Combine counts
    agg_result <- total_counts %>%
      left_join(active_counts, by = c("facility", "zone")) %>%
      left_join(expiring_counts, by = c("facility", "zone")) %>%
      mutate(
        active = ifelse(is.na(active), 0, active),
        expiring = ifelse(is.na(expiring), 0, expiring),
        display_name = facility
      ) %>%
      map_facility_names(facility_col = "display_name") %>%
      mutate(display_name = paste0(display_name_display, " (P", zone, ")")) %>%
      select(-display_name_display)
  } else {
    # Count TOTAL structures by facility (from all_structures)
    total_counts <- all_structures %>%
      group_by(facility) %>%
      summarize(total = n_distinct(sitecode), .groups = "drop")
    
    # Count active structures (DISTINCT sitecodes from treatments)
    if (nrow(treatments) > 0 && "is_active" %in% names(treatments)) {
      active_counts <- treatments %>%
        filter(is_active == TRUE) %>%
        distinct(sitecode, facility) %>%
        group_by(facility) %>%
        summarize(active = n(), .groups = "drop")
      
      expiring_counts <- treatments %>%
        filter(is_expiring == TRUE) %>%
        distinct(sitecode, facility) %>%
        group_by(facility) %>%
        summarize(expiring = n(), .groups = "drop")
    } else {
      active_counts <- data.frame(facility = character(), active = integer())
      expiring_counts <- data.frame(facility = character(), expiring = integer())
    }
    
    # Combine counts
    agg_result <- total_counts %>%
      left_join(active_counts, by = "facility") %>%
      left_join(expiring_counts, by = "facility") %>%
      mutate(
        active = ifelse(is.na(active), 0, active),
        expiring = ifelse(is.na(expiring), 0, expiring),
        display_name = facility
      ) %>%
      map_facility_names(facility_col = "display_name") %>%
      mutate(display_name = display_name_display) %>%
      select(-display_name_display)
  }
  
  # Apply consistent ordering
  agg_result <- order_facilities(agg_result, separate_zones)
  cat("Structure: Aggregated to", nrow(agg_result), "rows\n")
  
  # Debug output
  cat("Structure totals:", paste(agg_result$display_name, "total=", agg_result$total, "active=", agg_result$active, collapse = " | "), "\n")
  check_pct <- agg_result %>%
    mutate(pct = round(100 * active / pmax(1, total), 1))
  cat("Structure percentages:", paste(agg_result$display_name, check_pct$pct, "%", collapse = ", "), "\n")
  
  return(agg_result)
}
