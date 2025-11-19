# Inspections App - Data Functions

library(dplyr)
library(DBI)
library(RPostgres)
library(lubridate)

# Get site choices from gis_sectcode
get_site_choices <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(list(facility = NULL, fosarea = NULL, zone = NULL))
  df <- dbGetQuery(con, "SELECT DISTINCT facility, fosarea, zone FROM gis_sectcode ORDER BY facility, fosarea, zone")
  dbDisconnect(con)
  list(
    facility = sort(unique(df$facility)),
    fosarea = sort(unique(df$fosarea)),
    zone = sort(unique(df$zone))
  )
}

# Get inspection records 
get_inspection_gaps <- function(air_gnd_filter, facility_filter, fosarea_filter, zone_filter, priority_filter, drone_filter, years_gap, ref_date = Sys.Date()) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Build filter conditions
    site_filters <- c()
    
    if (!is.null(facility_filter) && length(facility_filter) > 0 && !"all" %in% facility_filter) {
      facilities_str <- paste0("'", facility_filter, "'", collapse = ",")
      site_filters <- c(site_filters, paste0("sc.facility IN (", facilities_str, ")"))
    }
    
    if (!is.null(fosarea_filter) && length(fosarea_filter) > 0 && !"all" %in% fosarea_filter) {
      # Map foreman shortnames to their emp_num (which corresponds to fosarea)
      foreman_lookup <- get_foremen_lookup()
      if (nrow(foreman_lookup) > 0) {
        # Convert shortnames to emp_num values
        fosarea_codes <- character(0)
        for (fosarea in fosarea_filter) {
          matching_foreman <- foreman_lookup[foreman_lookup$shortname == fosarea, ]
          if (nrow(matching_foreman) > 0) {
            fosarea_codes <- c(fosarea_codes, matching_foreman$emp_num)
          }
        }
        if (length(fosarea_codes) > 0) {
          fosareas_str <- paste0("'", fosarea_codes, "'", collapse = ",")
          site_filters <- c(site_filters, paste0("sc.fosarea IN (", fosareas_str, ")"))
        }
      }
    }

    if (!is.null(zone_filter) && length(zone_filter) > 0) {
      zones_str <- paste0("'", zone_filter, "'", collapse = ",")
      site_filters <- c(site_filters, paste0("sc.zone IN (", zones_str, ")"))
    }
    
    if (!is.null(priority_filter) && length(priority_filter) > 0 && !"all" %in% priority_filter) {
      priorities_str <- paste0("'", priority_filter, "'", collapse = ",")
      site_filters <- c(site_filters, paste0("b.priority IN (", priorities_str, ")"))
    }
    
    # Add drone filtering
    if (!is.null(drone_filter) && drone_filter != "all") {
      if (drone_filter == "drone_only") {
        site_filters <- c(site_filters, "b.drone = 'Y'")
      } else if (drone_filter == "no_drone") {
        site_filters <- c(site_filters, "(b.drone IS NULL OR b.drone != 'Y')")
      } else if (drone_filter == "include_drone") {
        # Include all sites but prioritize/highlight drone sites
        # No additional filter needed - all sites included
      }
    }
    
    # Combine filters
    where_clause <- if (length(site_filters) > 0) {
      paste0(" AND ", paste(site_filters, collapse = " AND "))
    } else {
      ""
    }
    
    gap_cutoff <- format(as.Date(ref_date) - years(years_gap), "%Y-%m-%d")
    
    qry <- sprintf("
    -- Step 1: Get filtered active sites
    WITH filtered_sites AS (
      SELECT 
        b.sitecode,
        sc.facility,
        sc.fosarea,
        sc.zone,
        b.air_gnd,
        b.priority,
        b.drone
      FROM loc_breeding_sites b
      INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
      WHERE b.enddate IS NULL
      AND b.air_gnd = '%s'
      %s
    ),
    -- Step 2: Get most recent inspection per site
    site_inspections AS (
      SELECT 
        fs.sitecode,
        fs.facility,
        fs.fosarea,
        fs.zone,
        fs.air_gnd,
        fs.priority,
        fs.drone,
        i.inspdate,
        i.action,
        i.numdip,
        ROW_NUMBER() OVER (PARTITION BY fs.sitecode ORDER BY i.inspdate DESC NULLS LAST) as rn
      FROM filtered_sites fs
      LEFT JOIN (
        SELECT sitecode, inspdate, action, numdip
        FROM dblarv_insptrt_current
        WHERE action IN ('1','2','4')
        AND (action != '1' OR numdip IS NOT NULL)
        UNION ALL
        SELECT sitecode, inspdate, action, numdip
        FROM dblarv_insptrt_archive
        WHERE action IN ('1','2','4')
        AND (action != '1' OR numdip IS NOT NULL)
      ) i ON fs.sitecode = i.sitecode
    )
    -- Step 3: Filter to gap sites only
    SELECT 
      sitecode,
      facility,
      fosarea,
      zone,
      air_gnd,
      priority,
      drone,
      COALESCE(inspdate, '1900-01-01'::date) as last_inspection_date,
      numdip as last_numdip,
      CASE 
        WHEN inspdate IS NULL THEN 999999
        ELSE (CURRENT_DATE - inspdate::date)
      END as days_since_inspection,
      CASE 
        WHEN inspdate IS NULL THEN 'Never Inspected'
        WHEN inspdate < '%s'::date THEN 'Inspection Gap'
        ELSE 'Recently Inspected'
      END as inspection_status
    FROM site_inspections
    WHERE rn = 1
    AND (inspdate IS NULL OR inspdate < '%s'::date)
    ORDER BY COALESCE(inspdate, '1900-01-01'::date) ASC, sitecode
    ", air_gnd_filter, where_clause, gap_cutoff, gap_cutoff)
    
    result <- dbGetQuery(con, qry)
    dbDisconnect(con)
    
    return(result)
    
  }, error = function(e) {
    warning(paste("Error in get_inspection_gaps:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}