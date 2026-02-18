# checkback_functions.R
# Brood calculation and checkback status tracking for control efficacy app
#
# Functions:
# - calculate_treatment_rounds() - Group consecutive treatment days into broods
# - calculate_checkback_status() - Determine checkback completion for each brood
# - find_multiple_checkbacks() - Identify sites with multiple checkbacks

library(dplyr)

#' Calculate treatment rounds (broods) from treatment data
#' 
#' Groups consecutive treatment days by facility into "rounds" or "broods"
#' Consecutive days = max 1 day gap
#'
#' @param treatments Data frame from load_treatment_data()
#' @param checkback_type Either "percent" or "number"
#' @param checkback_percent Percentage of sites requiring checkback (if type="percent")
#' @param checkback_number Fixed number of checkbacks required (if type="number")
#'
#' @return Data frame with columns: facility, round_id, round_name, start_date, 
#'         end_date, days_duration, sites_treated, total_sites, total_acres, 
#'         checkbacks_needed
calculate_treatment_rounds <- function(treatments, checkback_type = "percent", 
                                      checkback_percent = 10, checkback_number = 10) {
  
  if (is.null(treatments) || nrow(treatments) == 0) {
    return(NULL)
  }
  
  # Group consecutive treatment days by facility
  rounds <- treatments %>%
    arrange(facility, inspdate) %>%
    group_by(facility) %>%
    mutate(
      date_diff = as.numeric(inspdate - lag(inspdate, default = inspdate[1] - 2)),
      round_start = ifelse(date_diff > 1, TRUE, FALSE),
      round_id = cumsum(round_start)
    ) %>%
    group_by(facility, round_id) %>%
    summarise(
      start_date = min(inspdate),
      end_date = max(inspdate),
      days_duration = as.numeric(max(inspdate) - min(inspdate)) + 1,
      sites_treated = n_distinct(sitecode),
      total_sites = n(),
      total_acres = sum(acres, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      round_name = paste(facility, format(start_date, "%m/%d"), sep = "-"),
      checkbacks_needed = case_when(
        checkback_type == "percent" ~ ceiling(sites_treated * checkback_percent / 100),
        TRUE ~ pmin(checkback_number, sites_treated)
      )
    )
  
  return(rounds)
}

#' Calculate checkback status for each treatment round
#'
#' Determines how many checkbacks have been completed for each brood.
#' Only counts valid checkbacks (those not invalidated by subsequent treatments).
#'
#' @param rounds Data frame from calculate_treatment_rounds()
#' @param checkbacks Data frame from load_checkback_data()
#' @param treatments Original treatment data frame
#'
#' @return Data frame with columns: facility, round_name, start_date, end_date,
#'         sites_treated, checkbacks_needed, checkbacks_completed, 
#'         completion_rate, avg_days_to_checkback
calculate_checkback_status <- function(rounds, checkbacks, treatments, all_inspections = NULL) {
  
  # Validate required data exists
  if (is.null(rounds) || nrow(rounds) == 0 || is.null(treatments) || nrow(treatments) == 0) {
    return(NULL)
  }
  
  # Allow function to work even if no checkbacks exist yet
  if (is.null(checkbacks)) {
    checkbacks <- data.frame()
  }
  
  # For each round, find checkbacks within reasonable timeframe
  results <- list()
  
  for (i in seq_len(nrow(rounds))) {
    round <- rounds[i, ]
    
    # Get sites treated in this round
    round_treatments <- treatments %>%
      filter(
        facility == round$facility,
        inspdate >= round$start_date,
        inspdate <= round$end_date
      )
    
    # For each site in this round, only count checkbacks that haven't been invalidated by newer treatments
    valid_checkbacks <- data.frame()
    
    if (nrow(checkbacks) > 0) {
      for (site in unique(round_treatments$sitecode)) {
        # Get the last treatment date for this site in this round
        last_treatment_date <- max(round_treatments$inspdate[round_treatments$sitecode == site])
        
        # Get all future treatments for this site (to invalidate checkbacks)
        future_treatments <- treatments %>%
          filter(sitecode == site, inspdate > last_treatment_date)
        
        # Get checkbacks for this site after the round
        site_checkbacks <- checkbacks %>%
          filter(
            sitecode == site,
            inspdate > last_treatment_date
          )
        
        if (nrow(site_checkbacks) > 0) {
          # If there are future treatments, only count checkbacks before the next treatment
          if (nrow(future_treatments) > 0) {
            next_treatment_date <- min(future_treatments$inspdate)
            site_checkbacks <- site_checkbacks %>%
              filter(inspdate < next_treatment_date)
          }
          
          # Exclude control checkbacks: no treatments occurred between
          # the pre-inspection and the post (checkback) inspection.
          # Use timestamps for same-day accuracy.
          if (!is.null(all_inspections) && nrow(all_inspections) > 0 && nrow(site_checkbacks) > 0) {
            site_insps <- all_inspections[all_inspections$sitecode == site & is.na(all_inspections$posttrt_p), ]

            # Build combined datetime for treatments at this site
            site_trt <- treatments[treatments$sitecode == site, ]
            site_trt_dt <- paste(site_trt$inspdate, ifelse(is.na(site_trt$insptime) | site_trt$insptime == "", "00:00:00", site_trt$insptime))

            keep_mask <- vapply(seq_len(nrow(site_checkbacks)), function(j) {
              cb_date <- site_checkbacks$inspdate[j]
              cb_time <- if ("insptime" %in% names(site_checkbacks)) site_checkbacks$insptime[j] else "23:59:59"
              if (is.na(cb_time) || cb_time == "") cb_time <- "23:59:59"
              cb_dt <- paste(cb_date, cb_time)

              # Most recent non-posttrt inspection before this checkback (by datetime)
              pre_times <- ifelse(is.na(site_insps$insptime) | site_insps$insptime == "", "00:00:00", site_insps$insptime)
              pre_dts <- paste(site_insps$inspdate, pre_times)
              valid_pre <- pre_dts[pre_dts < cb_dt]
              if (length(valid_pre) == 0) return(TRUE)
              pre_dt <- max(valid_pre)

              # Any treatments between pre and post (by datetime)?
              any(site_trt_dt > pre_dt & site_trt_dt < cb_dt)
            }, logical(1))
            site_checkbacks <- site_checkbacks[keep_mask, , drop = FALSE]
          }
          
          # Take only the first valid checkback
          if (nrow(site_checkbacks) > 0) {
            first_checkback <- site_checkbacks %>%
              arrange(inspdate) %>%
              slice(1) %>%
              mutate(
                days_to_checkback = as.numeric(inspdate - last_treatment_date),
                round_end = round$end_date
              )
            
            valid_checkbacks <- rbind(valid_checkbacks, first_checkback)
          }
        }
      }
    }
    
    results[[i]] <- data.frame(
      facility = round$facility,
      round_name = round$round_name,
      start_date = round$start_date,
      end_date = round$end_date,
      sites_treated = round$sites_treated,
      checkbacks_needed = round$checkbacks_needed,
      checkbacks_completed = nrow(valid_checkbacks),
      completion_rate = round(nrow(valid_checkbacks) / round$checkbacks_needed * 100, 1),
      avg_days_to_checkback = ifelse(nrow(valid_checkbacks) > 0, 
                                   round(mean(valid_checkbacks$days_to_checkback, na.rm = TRUE), 1), 
                                   NA)
    )
  }
  
  if (length(results) > 0) {
    return(do.call(rbind, results))
  } else {
    return(NULL)
  }
}

#' Find sites with multiple checkbacks
#'
#' Identifies sites that have received more than one checkback inspection,
#' which may indicate persistent mosquito issues.
#'
#' @param checkbacks Data frame from load_checkback_data()
#' @param treatments Original treatment data frame
#'
#' @return Data frame with multiple checkback details per site
find_multiple_checkbacks <- function(checkbacks, treatments) {
  
  if (is.null(checkbacks) || nrow(checkbacks) == 0 || 
      is.null(treatments) || nrow(treatments) == 0) {
    return(NULL)
  }
  
  # Find sites with multiple checkbacks
  sites_with_multiple <- checkbacks %>%
    group_by(sitecode) %>%
    summarise(
      checkback_count = n(),
      .groups = "drop"
    ) %>%
    filter(checkback_count > 1)
  
  if (nrow(sites_with_multiple) == 0) {
    return(NULL)
  }
  
  # Get details for these sites
  details <- checkbacks %>%
    filter(sitecode %in% sites_with_multiple$sitecode) %>%
    arrange(sitecode, inspdate) %>%
    group_by(sitecode) %>%
    mutate(
      checkback_number = row_number(),
      days_since_previous = as.numeric(inspdate - lag(inspdate, default = inspdate[1]))
    ) %>%
    ungroup()
  
  return(details)
}

#' Create site-level treatment and checkback details
#'
#' Comprehensive view of each site's treatment history and checkback status.
#'
#' @param treatments Treatment data frame
#' @param checkbacks Checkback data frame
#'
#' @return Data frame with site-level details including treatment dates,
#'         checkback dates, and dip counts
create_site_details <- function(treatments, checkbacks, species_data = NULL, all_inspections = NULL) {
  
  if (is.null(checkbacks) || nrow(checkbacks) == 0) {
    return(NULL)
  }
  
  if (is.null(treatments) || nrow(treatments) == 0) {
    return(NULL)
  }
  
  # Create one row per checkback
  checkback_list <- list()
  
  get_species_info <- function(sample_id, species_data) {
    if (is.null(species_data) || is.null(sample_id) || is.na(sample_id) || sample_id == "") {
      return(list(comp = NA_character_, redblue = NA, missing = NA))
    }
    species_match <- species_data %>% filter(sampnum_yr == !!sample_id)
    if (nrow(species_match) > 0) {
      return(list(
        comp = species_match$species_composition[1],
        redblue = species_match$redblue[1],
        missing = species_match$missing[1]
      ))
    }
    return(list(comp = NA_character_, redblue = NA, missing = NA))
  }
  
  for (i in 1:nrow(checkbacks)) {
    checkback <- checkbacks[i, ]
    site <- checkback$sitecode
    checkback_date <- checkback$inspdate
    sampnum_yr <- checkback$sampnum_yr
    
    # Get species composition for this checkback (post-treatment) if available
    post_info <- get_species_info(sampnum_yr, species_data)
    post_species_comp <- post_info$comp
    redblue_val <- post_info$redblue
    missing_val <- post_info$missing
    
    # Find all treatments at this site BEFORE this checkback
    site_treatments <- treatments %>%
      filter(sitecode == site, inspdate < checkback_date) %>%
      arrange(inspdate)
    
    if (nrow(site_treatments) > 0) {
      # Get the most recent treatment before this checkback
      last_treatment_idx <- nrow(site_treatments)
      treatment_date <- site_treatments$inspdate[last_treatment_idx]
      pre_treatment_dips <- site_treatments$numdip[last_treatment_idx]
      treatment_acres <- site_treatments$acres[last_treatment_idx]
      matcode <- site_treatments$matcode[last_treatment_idx]
      mattype <- site_treatments$mattype[last_treatment_idx]
      effect_days <- site_treatments$effect_days[last_treatment_idx]
      pre_sampnum_yr <- site_treatments$presamp_yr[last_treatment_idx]
      if (is.null(pre_sampnum_yr) || is.na(pre_sampnum_yr) || pre_sampnum_yr == "") {
        pre_sampnum_yr <- site_treatments$sampnum_yr[last_treatment_idx]
      }
      pre_info <- get_species_info(pre_sampnum_yr, species_data)
      pre_species_comp <- pre_info$comp
      
      # Find the inspection date (could be same as treatment or earlier)
      inspection_date <- site_treatments$inspdate[last_treatment_idx]
      
      days_diff <- as.numeric(checkback_date - treatment_date)
      
      # Detect control checkbacks: no treatments occurred between the
      # pre-inspection and the post (checkback) inspection.
      # Use timestamps for same-day accuracy.
      is_control <- FALSE
      pre_inspection_date <- NA
      if (!is.null(all_inspections) && nrow(all_inspections) > 0) {
        site_insps <- all_inspections[all_inspections$sitecode == site & is.na(all_inspections$posttrt_p), ]

        # Build datetime for checkback
        cb_time <- if ("insptime" %in% names(checkback)) checkback$insptime else "23:59:59"
        if (is.na(cb_time) || cb_time == "") cb_time <- "23:59:59"
        cb_dt <- paste(checkback_date, cb_time)

        # Build datetimes for pre-inspections
        pre_times <- ifelse(is.na(site_insps$insptime) | site_insps$insptime == "", "00:00:00", site_insps$insptime)
        pre_dts <- paste(site_insps$inspdate, pre_times)
        valid_pre <- pre_dts[pre_dts < cb_dt]

        if (length(valid_pre) > 0) {
          pre_dt <- max(valid_pre)
          pre_inspection_date <- as.Date(substr(pre_dt, 1, 10))

          # Build datetimes for treatments at this site
          site_trts <- treatments[treatments$sitecode == site, ]
          trt_times <- ifelse(is.na(site_trts$insptime) | site_trts$insptime == "", "00:00:00", site_trts$insptime)
          trt_dts <- paste(site_trts$inspdate, trt_times)

          # Is there any treatment between pre and post (by datetime)?
          is_control <- !any(trt_dts > pre_dt & trt_dts < cb_dt)
        }
      }
      
      checkback_list[[i]] <- data.frame(
        sitecode = site,
        facility = checkback$facility,
        inspection_date = inspection_date,
        treatment_date = treatment_date,
        checkback_date = checkback_date,
        pre_treatment_dips = pre_treatment_dips,
        post_treatment_dips = checkback$numdip,
        acres = treatment_acres,
        matcode = matcode,
        mattype = mattype,
        effect_days = effect_days,
        days_to_checkback = days_diff,
        pre_species_composition = pre_species_comp,
        post_species_composition = post_species_comp,
        species_composition = post_species_comp,
        redblue = redblue_val,
        missing = missing_val,
        is_control = is_control,
        pre_inspection_date = pre_inspection_date,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(checkback_list) > 0) {
    result <- do.call(rbind, checkback_list)
    return(result)
  } else {
    return(NULL)
  }
}
