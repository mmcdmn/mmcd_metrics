# Compute section-level MLE using per-trap MLE calculation with k-NN distance-weighted averaging
# This replaces the old pool-level k-NN approach

compute_section_mle_trap_based <- function(species_codes, analysis_date = Sys.Date(), k = 4, 
                                            trap_types = c("4", "5", "6"),
                                            virus_target = "WNV", pt_method = "firth", scale = 1000,
                                            group_by = "location", progress = NULL, 
                                            prefetched_data = NULL) {  
  
  # If data already fetched, use it instead of querying database
  if (!is.null(prefetched_data)) {
    message("Using pre-fetched surveillance data (skipping database query)")
    pool_data <- prefetched_data$pools
    
    if (is.null(pool_data) || nrow(pool_data) == 0) {
      message("No pool data in pre-fetched data")
      return(list(sections = data.frame(), trap_mles = data.frame(), sections_sf = NULL))
    }
    
    message("DEBUG MLE: Pre-fetched pool data has ", nrow(pool_data), " pools")
    message("DEBUG MLE: Positive pools in pre-fetched data: ", sum(pool_data$result == "Pos", na.rm = TRUE))
    
    # Transform unified format to trap-aggregated format
    # CRITICAL: Match the PostgreSQL array format that the database query returns
    trap_pools <- pool_data %>%
      group_by(sampnum_yr) %>%  # Group by sampnum_yr (trap inspection ID)
      summarise(
        sampnum_yr = first(sampnum_yr),
        facility = first(facility),
        lon = first(lon),
        lat = first(lat),
        inspdate = first(date),
        # FIX: Convert to PostgreSQL array format "{val1,val2,val3}" to match database query
        results = paste0("{", paste(result, collapse = ","), "}"),
        pool_sizes = paste0("{", paste(value, collapse = ","), "}"),
        poolnums = paste0("{", paste(poolnum, collapse = ","), "}"),
        num_pools = n(),
        last_test_date = max(date),
        .groups = "drop"
      )
    
    message("Transformed ", nrow(pool_data), " pool records into ", nrow(trap_pools), " trap records")
    
    # DEBUG: Check how many have positive results
    traps_with_pos <- sum(grepl("Pos", trap_pools$results))
    message("DEBUG MLE: Trap records with positive pools: ", traps_with_pos)
    
  } else {
    # Original database query logic
    con <- get_db_connection()
    if (is.null(con)) return(data.frame())
    on.exit(safe_disconnect(con))

    # Handle species filtering - ensure all codes are character strings
    species_sql <- ""
    if (!("all" %in% tolower(species_codes))) {
      # Force conversion to character and wrap in quotes
      species_codes_char <- as.character(species_codes)
      species_filter <- paste(sprintf("'%s'", species_codes_char), collapse = ",")
      species_sql <- sprintf("AND p.spp_code IN (%s)", species_filter)
    }
    
    message("Species SQL filter: ", species_sql)
    
    trap_types_sql <- paste(sprintf("'%s'", trap_types), collapse = ",")
    
    # Query to get virus test data grouped by trap (sampnum_yr)
    # Filter by trap types (survtype) to match surveillance parameters
    virus_q <- sprintf(
      "SELECT 
        pl.sampnum_yr,
        g.facility,
        ST_X(ST_Transform(g.geometry, 4326)) as lon, 
        ST_Y(ST_Transform(g.geometry, 4326)) as lat,
        g.inspdate,
        ARRAY_AGG(pl.result ORDER BY pl.poolnum) as results,
        ARRAY_AGG(pl.count ORDER BY pl.poolnum) as pool_sizes,
        ARRAY_AGG(pl.poolnum ORDER BY pl.poolnum) as poolnums,
        ARRAY_AGG(pl.spp_code ORDER BY pl.poolnum) as species,
        COUNT(*) as num_pools,
        MAX(pl.testdate) as last_test_date
       FROM 
       (SELECT t.id AS test_id, t.poolnum, result, t.date AS testdate, status, method, target, 
               p.sampnum_yr, p.spp_code, p.count
        FROM dbvirus_pool_test t
        LEFT JOIN dbvirus_pool p ON p.poolnum = t.poolnum
        WHERE t.date >= '%s'::date - INTERVAL '90 days'
          AND t.date <= '%s'::date
          AND t.target = '%s'
          %s
       ) pl
       LEFT JOIN (
         SELECT ainspecnum, sitecode, address1, sampnum_yr, c.loc_code, network_type, 
                c.facility, survtype, inspdate, 
                CASE WHEN c.network_type IS NOT NULL THEN l.geom
                     ELSE c.geometry END AS geometry
         FROM dbadult_insp_current c
         LEFT JOIN 
         (SELECT a.loc_code, n.geom 
          FROM loc_mondaynight_active a 
          LEFT JOIN loc_mondaynight n ON n.loc_code = a.loc_code
          WHERE a.enddate IS NULL ORDER BY a.loc_code) l ON l.loc_code = c.loc_code
         WHERE c.survtype IN (%s)
       ) g ON g.sampnum_yr = pl.sampnum_yr
       WHERE g.geometry IS NOT NULL
       GROUP BY pl.sampnum_yr, g.facility, g.geometry, g.inspdate
       HAVING COUNT(*) > 0",
      as.character(analysis_date),
      as.character(analysis_date),
      virus_target,
      species_sql,
      trap_types_sql
    )
    
    trap_pools <- dbGetQuery(con, virus_q)
    
    message("Trap-level pool query returned ", nrow(trap_pools), " traps with pools")
  }
  
  if (nrow(trap_pools) == 0) {
    message("No trap-level pool data found")
    return(list(sections = data.frame(), trap_mles = data.frame(), sections_sf = NULL))
  }
  
  # Calculate MLE per trap
  message("Stage 1: Calculating MLE for ", nrow(trap_pools), " traps...")
  total_traps <- nrow(trap_pools)
  
  # Capture progress object for use in lapply
  prog <- progress
  
  trap_mle_results <- lapply(seq_len(nrow(trap_pools)), function(i) {
    if (i %% 50 == 0 || i == total_traps) {
      cat(sprintf("\r  Processing trap %d of %d (%.1f%%)    ", i, total_traps, (i/total_traps)*100))
      flush.console()
    }
    trap_row <- trap_pools[i, ]
    
    # Parse PostgreSQL arrays
    results <- strsplit(gsub("[{}]", "", trap_row$results), ",")[[1]]
    pool_sizes <- as.numeric(strsplit(gsub("[{}]", "", trap_row$pool_sizes), ",")[[1]])
    
    # Convert results to binary (1=Pos, 0=Neg)
    x_binary <- ifelse(results == "Pos", 1, 0)
    
    # DEBUG: Check first positive trap
    if (i == 1 && any(grepl("Pos", trap_row$results))) {
      message("\nDEBUG TRAP 1 (should have positive):")
      message("  Raw results: '", trap_row$results, "'")
      message("  Parsed results: ", paste(results, collapse = ", "))
      message("  x_binary: ", paste(x_binary, collapse = ", "))
      message("  Sum positive: ", sum(x_binary))
    }
    
    # Remove invalid entries
    valid_idx <- !is.na(pool_sizes) & pool_sizes > 0 & !is.na(x_binary)
    if (sum(valid_idx) == 0) return(NULL)
    
    x_valid <- x_binary[valid_idx]
    m_valid <- pool_sizes[valid_idx]
    
    # Calculate MLE for this trap
    tryCatch({
      # Use PooledInfRate package directly
      if (!requireNamespace("PooledInfRate", quietly = TRUE)) {
        stop("PooledInfRate package is required. Install from: https://github.com/CDCgov/PooledInfRate")
      }
      
      raw_result <- PooledInfRate::pooledBin(
        x = x_valid,
        m = m_valid,
        pt.method = pt_method,
        scale = scale
      )
      
      # Normalize output format
      mle_value <- as.numeric(raw_result$P)
      mle_lower <- as.numeric(raw_result$Lower)
      mle_upper <- as.numeric(raw_result$Upper)
      
      if (is.na(mle_value) || length(mle_value) == 0) {
        return(NULL)
      }
      
      data.frame(
        sampnum_yr = trap_row$sampnum_yr,
        facility = trap_row$facility,
        lon = trap_row$lon,
        lat = trap_row$lat,
        inspdate = trap_row$inspdate,
        mle = mle_value,
        mle_lower = mle_lower,
        mle_upper = mle_upper,
        num_pools = as.integer(trap_row$num_pools),
        num_positive = as.integer(sum(x_valid)),
        total_mosquitoes = sum(m_valid),
        last_test_date = trap_row$last_test_date,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      # Log ALL errors with trap ID
      cat(sprintf("\n✗ ERROR for trap %s: %s\n", trap_row$sampnum_yr, e$message))
      return(NULL)
    })
  })
  
  cat("\n")  # Newline after progress bar
  
  # Combine results
  trap_mle_df <- do.call(rbind, trap_mle_results[!sapply(trap_mle_results, is.null)])
  
  if (is.null(trap_mle_df) || nrow(trap_mle_df) == 0) {
    message("No valid trap-level MLEs calculated")
    return(list(sections = data.frame(), trap_mles = data.frame(), sections_sf = NULL))
  }
  
  message("Calculated MLE for ", nrow(trap_mle_df), " traps")
  message("DEBUG MLE: Traps with MLE > 0: ", sum(trap_mle_df$mle > 0, na.rm = TRUE))
  message("DEBUG MLE: Traps with positive pools: ", sum(trap_mle_df$num_positive > 0, na.rm = TRUE))
  
  # Load section geometries
  sections_sf <- load_section_geometries()
  if (is.null(sections_sf) || nrow(sections_sf) == 0) {
    message("No section geometries found")
    return(list(sections = data.frame(), trap_mles = trap_mle_df, sections_sf = NULL))
  }
  
  message("Section geometries loaded: ", nrow(sections_sf), " rows")
  
  # Create centroids for k-NN calculation
  sections_centroids <- suppressWarnings(st_centroid(sections_sf))
  sects <- sections_centroids %>%
    mutate(
      lon = st_coordinates(.)[,1],
      lat = st_coordinates(.)[,2]
    ) %>%
    st_drop_geometry() %>%
    select(gid, sectcode, zone, facility, lon, lat)
  
  # Convert to spatial objects
  traps_sf <- st_as_sf(trap_mle_df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  sects_sf <- st_as_sf(sects, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  # Store original lon/lat
  sects_sf$orig_lon <- sects_sf$lon
  sects_sf$orig_lat <- sects_sf$lat
  
  # Transform to meters for distance calculation
  traps_m <- st_transform(traps_sf, 3857)
  sects_m <- st_transform(sects_sf, 3857)
  
  message("Stage 2: k-NN distance-weighted averaging for ", nrow(sects_m), " sections with ", nrow(traps_m), " trap MLEs")
  total_sections <- nrow(sects_m)
  
  if (group_by == "location") {
    # GROUP BY LOCATION
    # Multiple traps can have same coordinates (different species tested at same location)
    trap_locations <- trap_mle_df %>%
      group_by(lon, lat) %>%
      summarise(
        location_id = first(sampnum_yr),
        num_traps_here = n(),
        .groups = "drop"
      )
    
    message("  Grouping by location: ", nrow(trap_locations), " unique trap locations (from ", nrow(trap_mle_df), " trap inspections)")
    
    # Create spatial object for unique locations
    trap_locs_sf <- st_as_sf(trap_locations, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
    trap_locs_m <- st_transform(trap_locs_sf, 3857)
  } else {
    message("  Grouping by individual trap: ", nrow(trap_mle_df), " trap inspections")
  }
  
  # Capture progress object for use in lapply
  prog <- progress
  
  # For each section, use k-NN IDW to calculate weighted average of trap MLEs
  results <- lapply(seq_len(total_sections), function(i) {
    # Update progress bar for Shiny
    if (!is.null(prog) && i %% 100 == 0) {
      prog$set(value = 0.2 + (i / total_sections) * 0.7, 
                   detail = sprintf("Section %d of %d (%.0f%%)", i, total_sections, (i/total_sections)*100))
    }
    
    # Console progress (keep for command line usage)
    if (i %% 100 == 0 || i == total_sections) {
      pct <- (i / total_sections) * 100
      cat(sprintf("\r  Processing section %d of %d (%.1f%%)    ", i, total_sections, pct))
      flush.console()
    }
    
    sect_row <- sects_m[i, ]
    
    if (group_by == "location") {
      # Find k nearest UNIQUE LOCATIONS
      dists_to_locations <- as.numeric(st_distance(sect_row, trap_locs_m))
      
      if (all(is.na(dists_to_locations)) || length(dists_to_locations) == 0) return(NULL)
      
      k_actual <- min(k, length(dists_to_locations))
      k_loc_idx <- order(dists_to_locations)[1:k_actual]
      k_locations <- trap_locations[k_loc_idx, ]
      
      # Get ALL traps at these k unique locations
      nearby_traps <- trap_mle_df %>%
        filter(lon %in% k_locations$lon & lat %in% k_locations$lat)
      
      # Calculate distance to each trap's location (traps at same location get same distance)
      trap_dists <- sapply(seq_len(nrow(nearby_traps)), function(j) {
        trap_lon <- nearby_traps$lon[j]
        trap_lat <- nearby_traps$lat[j]
        # Find which k location this trap belongs to
        loc_idx <- which(k_locations$lon == trap_lon & k_locations$lat == trap_lat)[1]
        dists_to_locations[k_loc_idx[loc_idx]]
      })
      
      num_locations_used <- k_actual
    } else {
      # Find k nearest INDIVIDUAL TRAPS (old behavior - may have duplicate distances)
      dists_to_traps <- as.numeric(st_distance(sect_row, traps_m))
      
      if (all(is.na(dists_to_traps)) || length(dists_to_traps) == 0) return(NULL)
      
      k_actual <- min(k, length(dists_to_traps))
      k_trap_idx <- order(dists_to_traps)[1:k_actual]
      nearby_traps <- trap_mle_df[k_trap_idx, ]
      trap_dists <- dists_to_traps[k_trap_idx]
      
      num_locations_used <- length(unique(paste(nearby_traps$lon, nearby_traps$lat)))
    }
    
    trap_dists[trap_dists == 0] <- 1e-6  # Avoid division by zero
    
    trap_dists[trap_dists == 0] <- 1e-6  # Avoid division by zero
    
    # Calculate inverse distance weights
    weights <- 1 / trap_dists
    weight_sum <- sum(weights)
    
    # Distance-weighted average of MLE values
    mle_weighted <- sum(weights * nearby_traps$mle) / weight_sum
    mle_lower_weighted <- sum(weights * nearby_traps$mle_lower) / weight_sum
    mle_upper_weighted <- sum(weights * nearby_traps$mle_upper) / weight_sum
    
    # Debug: Print first few sections to verify distance weighting is working
    if (i <= 3 && any(nearby_traps$mle > 0)) {
      cat(sprintf("\n  DEBUG Section %s: MLE=%.2f from %d traps at %d locations (dist: %.0f-%.0fm, trap MLEs: %s)\n", 
                  sect_row$sectcode, mle_weighted, nrow(nearby_traps), num_locations_used,
                  min(trap_dists), max(trap_dists),
                  paste(sprintf("%.2f", nearby_traps$mle), collapse=", ")))
    }
    
    # Extract section info
    sect_attrs <- st_drop_geometry(sect_row)
    
    # Get the most recent pool date from nearby traps
    pool_dates <- nearby_traps$inspdate[!is.na(nearby_traps$inspdate)]
    nearest_pool_date <- if(length(pool_dates) > 0) as.character(max(pool_dates)) else NA_character_
    
    data.frame(
      gid = as.integer(sect_attrs$gid[1]),
      sectcode = as.character(sect_attrs$sectcode[1]),
      zone = as.character(sect_attrs$zone[1]),
      facility = as.character(sect_attrs$facility[1]),
      lon = as.numeric(sect_attrs$orig_lon[1]),
      lat = as.numeric(sect_attrs$orig_lat[1]),
      mle = mle_weighted,
      mle_lower = mle_lower_weighted,
      mle_upper = mle_upper_weighted,
      num_traps = nrow(nearby_traps),
      num_locations = num_locations_used,
      nearest_dist = min(trap_dists),
      farthest_dist = max(trap_dists),
      num_pools = as.integer(sum(nearby_traps$num_pools)),
      num_positive = as.integer(sum(nearby_traps$num_positive)),
      total_tested = sum(nearby_traps$total_mosquitoes),
      nearest_pool_date = nearest_pool_date,
      stringsAsFactors = FALSE
    )
  })
  
  cat("\n")  # Newline after progress
  
  # Combine section results
  section_results <- do.call(rbind, results[!sapply(results, is.null)])
  
  if (is.null(section_results) || nrow(section_results) == 0) {
    message("No section results calculated")
    return(list(sections = data.frame(), trap_mles = trap_mle_df, sections_sf = NULL))
  }
  
  message("✓ Completed: ", nrow(section_results), " sections with trap-based k-NN distance-weighted MLE")
  
  # Check for duplicate sectcodes before join
  if (any(duplicated(section_results$sectcode))) {
    warning("Duplicate sectcodes found in section_results: ", 
            paste(section_results$sectcode[duplicated(section_results$sectcode)], collapse=", "))
  }
  if (any(duplicated(sections_sf$sectcode))) {
    warning("Duplicate sectcodes found in sections_sf: ",
            paste(sections_sf$sectcode[duplicated(sections_sf$sectcode)], collapse=", "))
  }
  
  # Join with section geometries
  sections_with_mle <- sections_sf %>%
    left_join(section_results, by = "sectcode") %>%
    filter(!is.na(mle))
  
  message("After join: ", nrow(sections_with_mle), " sections with MLE data")
  
  # Return sections data, trap MLE data, and section geometries
  return(list(
    sections = section_results,
    trap_mles = trap_mle_df,
    sections_sf = sections_with_mle
  ))
}
