# data_functions.R
# Database query functions for control efficacy app
#
# Functions:
# - load_treatment_data() - Get air treatment records (action='A')
# - load_checkback_data() - Get checkback inspections for treated sites
# - load_species_data_for_samples() - Get species composition for sample IDs

library(DBI)
library(RPostgres)

#' Load air treatment data within date range
#'
#' @param start_date Start date for query (character YYYY-MM-DD)
#' @param end_date End date for query (character YYYY-MM-DD)
#' @param facility_filter Filter by facility code ("all" for no filter)
#' @param matcode_filter Filter by material code ("all" for no filter)
#'
#' @return Data frame with columns: inspdate, facility, foreman, sitecode, 
#'         action, numdip, diphabitat, acres, matcode, pkey_pg, insptime
load_treatment_data <- function(start_date, end_date, facility_filter = "all", matcode_filter = "all") {
  
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  
  tryCatch({
    # Query for air treatments (action = 'A')
    # Use gis_sectcode for facility and zone (100% coverage vs ~78% from loc table)
    # CRITICAL: dblarv_insptrt_current = current year ONLY, dblarv_insptrt_archive = past years
    
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    start_year <- as.integer(format(as.Date(start_date), "%Y"))
    end_year <- as.integer(format(as.Date(end_date), "%Y"))
    
    need_archive <- start_year < current_year
    need_current <- end_year >= current_year
    
    # Build single query with UNION ALL when needed
    base_select <- "
      SELECT 
        insp.inspdate,
        gis.facility,
        gis.zone,
        insp.sitecode,
        insp.action,
        insp.numdip,
        insp.diphabitat,
        insp.acres,
        insp.matcode,
        insp.sampnum_yr,
        insp.presamp_yr,
        mat.mattype,
        mat.effect_days,
        insp.pkey_pg,
        insp.insptime
      FROM %s insp
      LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(insp.sitecode, 7)
      LEFT JOIN mattype_list_targetdose mat ON insp.matcode = mat.matcode
      WHERE insp.action = 'A' 
        AND insp.inspdate >= '%s'
        AND insp.inspdate <= '%s'
    "
    
    queries <- c()
    if (need_current) {
      queries <- c(queries, sprintf(base_select, "dblarv_insptrt_current", start_date, end_date))
    }
    if (need_archive) {
      queries <- c(queries, sprintf(base_select, "dblarv_insptrt_archive", start_date, end_date))
    }
    
    query <- paste(queries, collapse = " UNION ALL ")
    treatments <- dbGetQuery(con, query)
    safe_disconnect(con)
    
    if (!is.null(treatments) && nrow(treatments) > 0) {
      treatments$inspdate <- as.Date(treatments$inspdate)
      
      # Filter by facility if selected
      if (facility_filter != "all") {
        treatments <- treatments[treatments$facility == facility_filter, ]
      }
      
      # Filter by matcode if selected
      if (matcode_filter != "all") {
        treatments <- treatments[treatments$matcode == matcode_filter, ]
      }
      
      return(treatments)
    } else {
      return(NULL)
    }
    
  }, error = function(e) {
    cat("Error loading treatment data: ", e$message, "\n", file = stderr())
    message("Error loading treatment data: ", e$message)
    if (exists("con") && !is.null(con)) {
      try(safe_disconnect(con), silent = TRUE)
    }
    return(NULL)
  })
}

#' Load checkback inspection data for treated sites
#'
#' @param treated_sites Vector of sitecodes that were treated
#' @param start_date Start date for treatments (character YYYY-MM-DD)
#' @param end_date End date for checkback search (character YYYY-MM-DD)
#'
#' @return Data frame with columns: inspdate, facility, foreman, sitecode,
#'         action, numdip, diphabitat, pkey_pg
load_checkback_data <- function(treated_sites, start_date, end_date) {
  
  if (is.null(treated_sites) || length(treated_sites) == 0) {
    return(NULL)
  }
  
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  
  tryCatch({
    # Query for checkback inspections at treated sites
    # Action code: '4' = checkback inspection (post-treatment)
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    start_year <- as.integer(format(as.Date(start_date), "%Y"))
    end_year <- as.integer(format(as.Date(end_date), "%Y"))
    
    need_archive <- start_year < current_year
    need_current <- end_year >= current_year
    
    # Build site list for IN clause
    site_list <- paste0("'", paste(treated_sites, collapse = "','"), "'")
    
    # Build single query with UNION ALL when needed
    base_select <- "
      SELECT 
        insp.inspdate,
        gis.facility,
        gis.zone,
        insp.sitecode,
        insp.action,
        insp.numdip,
        insp.diphabitat,
        insp.posttrt_p,
        insp.sampnum_yr,
        insp.pkey_pg
      FROM %s insp
      LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(insp.sitecode, 7)
      WHERE insp.sitecode IN (%s)
        AND insp.action = '4'
        AND insp.posttrt_p IS NOT NULL
        AND insp.inspdate >= '%s'
        AND insp.inspdate <= '%s'
    "
    
    queries <- c()
    if (need_current) {
      queries <- c(queries, sprintf(base_select, "dblarv_insptrt_current", site_list, start_date, end_date))
    }
    if (need_archive) {
      queries <- c(queries, sprintf(base_select, "dblarv_insptrt_archive", site_list, start_date, end_date))
    }
    
    query <- paste(queries, collapse = " UNION ALL ")
    checkbacks <- dbGetQuery(con, query)
    safe_disconnect(con)
    
    if (nrow(checkbacks) > 0) {
      checkbacks$inspdate <- as.Date(checkbacks$inspdate)
      return(checkbacks)
    } else {
      return(NULL)
    }
    
  }, error = function(e) {
    message("Error loading checkback data: ", e$message)
    if (exists("con") && !is.null(con)) {
      try(safe_disconnect(con), silent = TRUE)
    }
    return(NULL)
  })
}

#' Load species data for a list of sample IDs
#'
#' @param sample_ids Character vector of sampnum_yr values
#' @param start_date Start date for query (character YYYY-MM-DD)
#' @param end_date End date for query (character YYYY-MM-DD)
#'
#' @return Data frame with sampnum_yr and species composition string
load_species_data_for_samples <- function(sample_ids, start_date, end_date) {
  
  if (is.null(sample_ids) || length(sample_ids) == 0) {
    return(NULL)
  }
  
  # Filter to valid sample IDs
  sample_ids <- sample_ids[!is.na(sample_ids) & sample_ids != ""]
  if (length(sample_ids) == 0) {
    return(NULL)
  }
  
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  
  tryCatch({
    # Determine which tables to query based on date range
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    start_year <- as.integer(format(as.Date(start_date), "%Y"))
    end_year <- as.integer(format(as.Date(end_date), "%Y"))
    
    need_archive <- start_year < current_year
    need_current <- end_year >= current_year
    
    # Build sample list for IN clause
    sample_list <- paste0("'", paste(unique(sample_ids), collapse = "','"), "'")
    
    # Build query with UNION ALL when needed
    base_select <- "
      SELECT 
        spp.sampnum_yr,
        spp.spp,
        spp.per as count,
        spec.genus,
        spec.species,
        samp.redblue,
        samp.missing
      FROM %s spp
      LEFT JOIN lookup_specieslist spec ON spp.spp = CAST(spec.sppcode AS VARCHAR)
      LEFT JOIN %s samp ON spp.sampnum_yr = samp.sampnum_yr
      WHERE spp.sampnum_yr IN (%s)
    "
    
    queries <- c()
    if (need_current) {
      queries <- c(queries, sprintf(base_select, "dblarv_species_current", "dblarv_sample_current", sample_list))
    }
    if (need_archive) {
      queries <- c(queries, sprintf(base_select, "dblarv_species_archive", "dblarv_sample_archive", sample_list))
    }
    
    query <- paste(queries, collapse = " UNION ALL ")
    species_data <- dbGetQuery(con, query)
    safe_disconnect(con)
    
    if (!is.null(species_data) && nrow(species_data) > 0) {
      # Aggregate species for each sample
      # Create formatted species strings like "Ae. vexans (90%), Cu. pipiens (10%)"
      species_summary <- species_data %>%
        group_by(sampnum_yr) %>%
        summarise(
          # Get redblue and missing from first row (should be same for all rows of same sample)
          redblue = first(redblue),
          missing = first(missing),
          # Create species composition string
          species_list = list({
            if (all(is.na(spp))) {
              NA
            } else {
              df <- data.frame(
                spp = spp[!is.na(spp)],
                count = count[!is.na(spp)],
                genus = genus[!is.na(spp)],
                species = species[!is.na(spp)],
                stringsAsFactors = FALSE
              ) %>%
                mutate(
                  species_name = ifelse(
                    !is.na(genus) & !is.na(species) & genus != "" & species != "",
                    paste0(substr(genus, 1, 2), ". ", species),
                    paste0("Spp", spp)
                  )
                ) %>%
                arrange(desc(count))
              
              paste(
                paste0(df$species_name, " (", df$count, "%)"),
                collapse = ", "
              )
            }
          }),
          .groups = "drop"
        ) %>%
        mutate(
          species_composition = ifelse(
            missing == TRUE | is.na(missing),
            "[Sample Missing]",
            ifelse(
              is.na(species_list) | lengths(species_list) == 0,
              "[No Species Data]",
              as.character(species_list)
            )
          )
        ) %>%
        select(sampnum_yr, species_composition, redblue, missing)
      
      return(species_summary)
    } else {
      return(NULL)
    }
    
  }, error = function(e) {
    message("Error loading species data: ", e$message)
    if (exists("con") && !is.null(con)) {
      try(safe_disconnect(con), silent = TRUE)
    }
    return(NULL)
  })
}

