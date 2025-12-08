# OPTIMIZED DATA FUNCTIONS
# Single-query approach to fetch all data at once
library(DBI)
library(dplyr)
library(sf)

# =============================================================================
# UNIFIED DATA FETCH - Get all trap and pool data in ONE query
# =============================================================================
fetch_unified_surveillance_data <- function(analysis_date = Sys.Date(), 
                                           species_codes = "all",
                                           facility_filter = NULL,
                                           trap_types = c("4", "5", "6"),
                                           virus_target = "WNV",
                                           lookback_days = 90) {
  
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(dbDisconnect(con))
  
  # Build filters
  trap_type_filter <- paste(sprintf("'%s'", trap_types), collapse = ",")
  
  species_filter <- ""
  if (length(species_codes) > 0 && !("all" %in% tolower(species_codes))) {
    species_filter <- sprintf("AND s.spp IN (%s)", paste(species_codes, collapse = ","))
  }
  
  facility_filter_sql <- ""
  if (!is.null(facility_filter) && length(facility_filter) > 0 && !("all" %in% tolower(facility_filter))) {
    facility_filter_sql <- sprintf("AND t.facility IN (%s)", paste(sprintf("'%s'", facility_filter), collapse = ","))
  }
  
  # UNIFIED QUERY - Get traps, pools, and sections in one go
  unified_q <- sprintf(
    "WITH 
    -- Latest trap inspections per location
    latest_traps AS (
      SELECT DISTINCT ON (t.x, t.y, t.facility)
        t.ainspecnum, t.facility, t.x, t.y, t.survtype, t.inspdate,
        COALESCE(SUM(s.cnt), 0) as species_count
      FROM public.dbadult_insp_current t
      LEFT JOIN public.dbadult_species_current s ON t.ainspecnum = s.ainspecnum %s
      WHERE t.inspdate::date <= '%s'::date 
        AND t.x IS NOT NULL AND t.y IS NOT NULL
        AND t.survtype IN (%s)
        %s
      GROUP BY t.ainspecnum, t.facility, t.x, t.y, t.survtype, t.inspdate
      ORDER BY t.x, t.y, t.facility, t.inspdate DESC
    ),
    
    -- Virus pool tests with geometries
    virus_pools AS (
      SELECT 
        t.id AS test_id, 
        t.poolnum, 
        t.result, 
        t.date AS testdate, 
        t.target,
        p.spp_code, 
        p.count,
        ST_X(ST_Transform(
          CASE WHEN c.network_type IS NOT NULL THEN l.geom ELSE c.geometry END, 
          4326)) as lon,
        ST_Y(ST_Transform(
          CASE WHEN c.network_type IS NOT NULL THEN l.geom ELSE c.geometry END, 
          4326)) as lat,
        c.facility
      FROM dbvirus_pool_test t
      LEFT JOIN dbvirus_pool p ON p.poolnum = t.poolnum
      LEFT JOIN dbadult_insp_current c ON c.sampnum_yr = p.sampnum_yr
      LEFT JOIN (
        SELECT a.loc_code, n.geom 
        FROM loc_mondaynight_active a 
        LEFT JOIN loc_mondaynight n ON n.loc_code = a.loc_code
        WHERE a.enddate IS NULL
      ) l ON l.loc_code = c.loc_code
      WHERE t.date >= '%s'::date - INTERVAL '%d days'
        AND t.date <= '%s'::date
        AND t.target = '%s'
        AND c.survtype IN (%s)
        %s
        %s
    )
    
    -- Return combined results
    SELECT 'trap' as data_type, ainspecnum as id, facility, x as lon, y as lat, 
           survtype as type, inspdate as date, species_count as value, 
           NULL as result, NULL as poolnum
    FROM latest_traps
    
    UNION ALL
    
    SELECT 'pool' as data_type, test_id::text as id, facility, lon, lat,
           target as type, testdate as date, count as value, result, poolnum
    FROM virus_pools
    WHERE lon IS NOT NULL AND lat IS NOT NULL",
    species_filter,
    as.character(analysis_date),
    trap_type_filter,
    facility_filter_sql,
    as.character(analysis_date),
    lookback_days,
    as.character(analysis_date),
    virus_target,
    trap_type_filter,
    facility_filter_sql,
    species_filter
  )
  
  # Execute unified query
  all_data <- dbGetQuery(con, unified_q)
  
  message(sprintf("Unified query fetched %d records (%d traps, %d pools)",
                  nrow(all_data),
                  sum(all_data$data_type == "trap"),
                  sum(all_data$data_type == "pool")))
  
  # Split into trap and pool data
  trap_data <- all_data %>% filter(data_type == "trap")
  pool_data <- all_data %>% filter(data_type == "pool")
  
  return(list(
    traps = trap_data,
    pools = pool_data,
    all_data = all_data
  ))
}

# =============================================================================
# OPTIMIZED COMPUTATION - Use pre-fetched data
# =============================================================================
compute_all_metrics_optimized <- function(surveillance_data, sections_sf, k = 4,
                                         pt_method = "firth", scale = 1000) {
  
  if (is.null(surveillance_data)) {
    return(NULL)
  }
  
  trap_data <- surveillance_data$traps
  pool_data <- surveillance_data$pools
  
  # Check data availability
  if (nrow(trap_data) == 0) {
    message("No trap data available")
    return(NULL)
  }
  
  # Convert to spatial objects ONCE
  traps_sf <- st_as_sf(trap_data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  traps_m <- st_transform(traps_sf, 3857)
  
  # Section centroids
  sections_centroids <- suppressWarnings(st_centroid(sections_sf))
  sects_m <- st_transform(sections_centroids, 3857)
  
  # Prepare pool data if available
  has_pools <- nrow(pool_data) > 0
  if (has_pools) {
    pools_sf <- st_as_sf(pool_data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
    pools_m <- st_transform(pools_sf, 3857)
  }
  
  message(sprintf("Computing metrics for %d sections (k=%d)", nrow(sects_m), k))
  
  # VECTORIZED COMPUTATION - Calculate all metrics in one pass
  results <- lapply(seq_len(nrow(sects_m)), function(i) {
    sect_row <- sects_m[i, ]
    sect_attrs <- st_drop_geometry(sect_row)
    
    # --- POPULATION INDEX (N) ---
    trap_dists <- as.numeric(st_distance(sect_row, traps_m))
    
    if (all(is.na(trap_dists)) || length(trap_dists) == 0) {
      return(NULL)
    }
    
    k_trap <- min(k, length(trap_dists))
    k_trap_idx <- order(trap_dists)[1:k_trap]
    k_trap_dists <- trap_dists[k_trap_idx]
    k_trap_dists[k_trap_dists == 0] <- 1e-6
    
    trap_weights <- 1 / k_trap_dists
    trap_counts <- as.numeric(traps_m$value[k_trap_idx])
    N <- sum(trap_weights * trap_counts) / sum(trap_weights)
    
    nearest_trap_count <- sum(trap_counts)
    last_inspection <- max(traps_m$date[k_trap_idx])
    
    # --- MLE & VECTOR INDEX (if pool data available) ---
    mle_value <- NA_real_
    mle_lower <- NA_real_
    mle_upper <- NA_real_
    num_pools <- 0L
    num_positive <- 0L
    total_tested <- 0L
    nearest_pool_date <- NA_character_
    P <- 0
    vector_index_metric <- 0
    
    if (has_pools) {
      pool_dists <- as.numeric(st_distance(sect_row, pools_m))
      
      if (!all(is.na(pool_dists)) && length(pool_dists) > 0) {
        k_pool <- min(k, length(pool_dists))
        k_pool_idx <- order(pool_dists)[1:k_pool]
        
        nearby_pools <- pool_data[k_pool_idx, ]
        
        # Validate pool data
        valid_idx <- !is.na(nearby_pools$value) & 
                     nearby_pools$value > 0 & 
                     !is.na(nearby_pools$result)
        
        if (sum(valid_idx) > 0) {
          m_sizes_valid <- nearby_pools$value[valid_idx]
          x_binary <- ifelse(nearby_pools$result[valid_idx] == "Pos", 1, 0)
          
          # Use PooledInfRate package directly
          if (!requireNamespace("PooledInfRate", quietly = TRUE)) {
            stop("PooledInfRate package is required. Install from: https://github.com/CDCgov/PooledInfRate")
          }
          
          mle_result <- tryCatch({
            raw_result <- PooledInfRate::pooledBin(
              x = x_binary, 
              m = m_sizes_valid, 
              pt.method = pt_method, 
              scale = scale
            )
            # Normalize output format
            list(
              estimates = data.frame(
                point = as.numeric(raw_result$P),
                lower = as.numeric(raw_result$Lower),
                upper = as.numeric(raw_result$Upper)
              )
            )
          }, error = function(e) NULL)
          
          if (!is.null(mle_result) && !is.null(mle_result$estimates)) {
            mle_value <- as.numeric(mle_result$estimates$point[1])
            mle_lower <- as.numeric(mle_result$estimates$lower[1])
            mle_upper <- as.numeric(mle_result$estimates$upper[1])
            
            num_pools <- length(m_sizes_valid)
            num_positive <- sum(x_binary)
            total_tested <- sum(m_sizes_valid)
            nearest_pool_date <- as.character(max(nearby_pools$date[valid_idx], na.rm = TRUE))
            
            # Calculate Vector Index
            P <- mle_value / scale
            vector_index_metric <- N * P
          }
        }
      }
    }
    
    # Return all metrics at once
    data.frame(
      gid = as.integer(sect_attrs$gid[1]),
      sectcode = as.character(sect_attrs$sectcode[1]),
      zone = as.character(sect_attrs$zone[1]),
      facility = as.character(sect_attrs$facility[1]),
      # Population Index metrics
      vector_index = N,
      nearest_trap_count = nearest_trap_count,
      last_inspection = as.character(last_inspection),
      # MLE metrics
      mle = mle_value,
      mle_lower = mle_lower,
      mle_upper = mle_upper,
      num_pools = num_pools,
      num_positive = num_positive,
      total_tested = total_tested,
      nearest_pool_date = nearest_pool_date,
      # Vector Index metrics
      N = N,
      P = P,
      vector_index_metric = vector_index_metric,
      stringsAsFactors = FALSE
    )
  })
  
  results <- results[!sapply(results, is.null)]
  
  if (length(results) == 0) {
    return(NULL)
  }
  
  section_results <- do.call(rbind, results)
  
  message(sprintf("Computed metrics for %d sections", nrow(section_results)))
  
  return(section_results)
}

# =============================================================================
# WRAPPER FUNCTIONS - Maintain compatibility with existing code
# =============================================================================

# Main computation function - replaces all three separate functions
compute_all_surveillance_metrics <- function(species_codes = "all", 
                                            analysis_date = Sys.Date(), 
                                            k = 4, 
                                            facility_filter = NULL, 
                                            trap_types = c("4", "5", "6"),
                                            virus_target = "WNV", 
                                            pt_method = "firth", 
                                            scale = 1000,
                                            metric_type = "all") {
  
  # Step 1: Fetch ALL data in one query
  surveillance_data <- fetch_unified_surveillance_data(
    analysis_date = analysis_date,
    species_codes = species_codes,
    facility_filter = facility_filter,
    trap_types = trap_types,
    virus_target = virus_target
  )
  
  if (is.null(surveillance_data)) {
    return(list(sections = data.frame(), sections_sf = NULL, traps = data.frame(), pools = data.frame()))
  }
  
  # Step 2: Load geometries (cached if already loaded)
  sections_sf <- load_section_geometries()
  if (is.null(sections_sf)) {
    return(list(sections = data.frame(), sections_sf = NULL, traps = data.frame(), pools = data.frame()))
  }
  
  # Step 3: Compute all metrics in one pass
  results <- compute_all_metrics_optimized(
    surveillance_data = surveillance_data,
    sections_sf = sections_sf,
    k = k,
    pt_method = pt_method,
    scale = scale
  )
  
  if (is.null(results)) {
    return(list(sections = data.frame(), sections_sf = NULL, traps = data.frame(), pools = data.frame()))
  }
  
  # Step 4: Merge with geometries
  sections_with_metrics <- sections_sf %>%
    inner_join(results, by = "sectcode")
  
  # Filter based on metric type to only return sections with valid data for that metric
  if (metric_type == "popindex") {
    sections_with_metrics <- sections_with_metrics %>% filter(!is.na(vector_index))
  } else if (metric_type == "mle") {
    sections_with_metrics <- sections_with_metrics %>% filter(!is.na(mle))
  } else if (metric_type == "vector_index") {
    sections_with_metrics <- sections_with_metrics %>% filter(!is.na(vector_index_metric))
  }
  
  return(list(
    sections = results,
    sections_sf = sections_with_metrics,
    traps = surveillance_data$traps,
    pools = surveillance_data$pools
  ))
}

# Wrapper for Vector Index - uses trap-based MLE with k-NN spatial averaging
compute_section_vector_index_metric <- function(species_codes = "all",
                                                analysis_date = Sys.Date(),
                                                k = 4,
                                                facility_filter = NULL,
                                                trap_types = c("4", "5", "6"),
                                                virus_target = "WNV",
                                                pt_method = "firth",
                                                scale = 1000,
                                                group_by = "location",
                                                progress = NULL) {
  # Vector Index = N × P where:
  #   N = Population Index (k-NN IDW of trap counts)
  #   P = MLE / 1000 (trap-based MLE with k-NN DWA)
  
  if (!is.null(progress)) {
    progress$set(0.05, detail = "Stage 1a: Calculating trap-based MLEs...")
  }
  
  # STEP 1: Get trap-based MLE results (same as MLE metric)
  # Create sub-progress updater for MLE (0.05 to 0.45 = 40% of total)
  mle_progress <- if (!is.null(progress)) {
    list(set = function(value, detail) {
      # Scale MLE progress from 0-1 to 0.05-0.45
      scaled_value <- 0.05 + (value * 0.40)
      progress$set(scaled_value, detail = paste("Stage 1:", detail))
    })
  } else NULL
  
  mle_results <- compute_section_mle_trap_based(
    species_codes = species_codes,
    analysis_date = analysis_date,
    k = k,
    trap_types = trap_types,
    virus_target = virus_target,
    pt_method = pt_method,
    scale = scale,
    group_by = group_by,
    progress = mle_progress
  )
  
  if (is.null(mle_results) || is.null(mle_results$sections_sf)) {
    return(list(sections = data.frame(), sections_sf = NULL, traps = data.frame(), pools = data.frame()))
  }
  
  if (!is.null(progress)) {
    progress$set(0.50, detail = "50%: Fetching trap count data...")
  }
  
  # STEP 2: Get population index results
  popindex_results <- compute_section_vector_index(
    species_codes = species_codes,
    analysis_date = analysis_date,
    k = k,
    facility_filter = facility_filter,
    trap_types = trap_types
  )
  
  if (!is.null(progress)) {
    progress$set(0.65, detail = "65%: Computing k-NN weighted averages...")
  }
  
  if (is.null(popindex_results) || is.null(popindex_results$sections_sf)) {
    return(list(sections = data.frame(), sections_sf = NULL, traps = data.frame(), pools = data.frame()))
  }
  
  if (!is.null(progress)) {
    progress$set(0.70, detail = "70%: Population index complete")
  }
  
  if (!is.null(progress)) {
    progress$set(0.75, detail = "75%: Merging population and infection data...")
  }
  
  # STEP 3: Merge MLE and Population Index data
  # Get ONLY the MLE-specific columns from mle_results (includes pool statistics)
  mle_data <- st_drop_geometry(mle_results$sections_sf) %>%
    select(sectcode, mle, mle_lower, mle_upper, 
           num_pools, num_positive, total_tested, 
           nearest_pool_date)
  
  # Get ONLY the population-specific columns from popindex_results
  # NOTE: popindex also has num_pools/num_positive/total_tested columns but they are 
  # garbage values (NAs or wrong data) since popindex doesn't use pool data!
  # We ONLY want the N (population index) and trap-related columns from popindex.
  popindex_data <- st_drop_geometry(popindex_results$sections_sf) %>%
    select(sectcode, N = vector_index, nearest_trap_count, last_inspection)
  
  # Combine the two datasets - popindex has N, mle_data has pool stats
  # Use inner_join since we only want sections that have BOTH valid N and valid MLE
  combined_data <- popindex_data %>%
    inner_join(mle_data, by = "sectcode")
  
  # STEP 4: Calculate Vector Index = N × P
  if (!is.null(progress)) {
    progress$set(0.80, detail = "80%: Calculating Vector Index (N × P)...")
  }
  
  combined_data <- combined_data %>%
    mutate(
      P = ifelse(!is.na(mle), mle / scale, 0),
      vector_index_metric = N * P,
      last_pool_date = nearest_pool_date
    )
  
  # Filter to only sections with valid Vector Index (both N and P available)
  valid_sections <- combined_data %>%
    filter(!is.na(vector_index_metric) & !is.na(mle))
  
  if (!is.null(progress)) {
    progress$set(0.90, detail = "Stage 3c: Merging with geometries...")
  }
  
  # STEP 5: Merge back with geometries
  sections_sf <- load_section_geometries()
  sections_with_vi <- sections_sf %>%
    inner_join(valid_sections, by = "sectcode")
  
  if (!is.null(progress)) {
    progress$set(0.98, detail = "Stage 3d: Finalizing results...")
  }
  
  if (!is.null(progress)) {
    progress$set(1.0, detail = "Complete!")
  }
  
  return(list(
    sections = valid_sections,
    sections_sf = sections_with_vi,
    traps = mle_results$traps,
    pools = mle_results$pools,
    trap_mles = mle_results$trap_mles  # Include trap-level MLE data for display
  ))
}

# Wrapper for Population Index - calls compute_all_surveillance_metrics
compute_section_vector_index <- function(species_codes = "all",
                                         analysis_date = Sys.Date(),
                                         k = 4,
                                         facility_filter = NULL,
                                         trap_types = c("4", "5", "6")) {
  # Population Index only needs trap count data (N)
  result <- compute_all_surveillance_metrics(
    species_codes = species_codes,
    analysis_date = analysis_date,
    k = k,
    facility_filter = facility_filter,
    trap_types = trap_types,
    virus_target = "WNV",  # Not used for popindex but required parameter
    pt_method = "firth",   # Not used for popindex but required parameter
    scale = 1000,          # Not used for popindex but required parameter
    metric_type = "popindex"
  )
  
  return(result)
}

# =============================================================================
# ADDITIONAL OPTIMIZATIONS
# =============================================================================

# Cache section geometries to avoid repeated loading
.section_geometries_cache <- NULL

load_section_geometries <- function(force_reload = FALSE) {
  if (!force_reload && !is.null(.section_geometries_cache)) {
    return(.section_geometries_cache)
  }
  
  # [... existing load_section_geometries code ...]
  # Then cache it:
  q_to_r_data_path <- file.path("..", "..", "shared", "Q_to_R", "data")
  shp_path <- file.path(q_to_r_data_path, "sections_boundaries.shp")
  
  if (file.exists(shp_path)) {
    sections_sf <- st_read(shp_path, quiet = TRUE)
    if (!st_is_longlat(sections_sf)) {
      sections_sf <- st_transform(sections_sf, 4326)
    }
    sections_sf <- st_make_valid(sections_sf)
    .section_geometries_cache <<- sections_sf
    return(sections_sf)
  }
  
  return(NULL)
}
