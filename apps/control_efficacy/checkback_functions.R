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
calculate_checkback_status <- function(rounds, checkbacks, treatments) {
  
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
create_site_details <- function(treatments, checkbacks, species_data = NULL) {
  
  if (is.null(checkbacks) || nrow(checkbacks) == 0) {
    return(NULL)
  }
  
  if (is.null(treatments) || nrow(treatments) == 0) {
    return(NULL)
  }
  
  # Create one row per checkback
  checkback_list <- list()
  
  for (i in 1:nrow(checkbacks)) {
    checkback <- checkbacks[i, ]
    site <- checkback$sitecode
    checkback_date <- checkback$inspdate
    sampnum_yr <- checkback$sampnum_yr
    
    # Get species composition for this checkback if available
    species_comp <- ""
    redblue_val <- NA
    missing_val <- NA
    if (!is.null(species_data) && !is.null(sampnum_yr) && !is.na(sampnum_yr) && sampnum_yr != "") {
      species_match <- species_data %>% filter(sampnum_yr == !!sampnum_yr)
      if (nrow(species_match) > 0) {
        species_comp <- species_match$species_composition[1]
        redblue_val <- species_match$redblue[1]
        missing_val <- species_match$missing[1]
      }
    }
    
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
      
      # Find the inspection date (could be same as treatment or earlier)
      inspection_date <- site_treatments$inspdate[last_treatment_idx]
      
      days_diff <- as.numeric(checkback_date - treatment_date)
      
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
        species_composition = species_comp,
        redblue = redblue_val,
        missing = missing_val,
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
