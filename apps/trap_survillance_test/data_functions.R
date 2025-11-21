library(DBI)
library(dplyr)
library(sf)

# Load section boundary geometries from shared Q_to_R location
load_section_geometries <- function() {
  # Path to shared Q_to_R data directory
  q_to_r_data_path <- file.path("..", "..", "shared", "Q_to_R", "data")
  
  gpkg_path <- file.path(q_to_r_data_path, "sections_boundaries.gpkg") 
  shp_path <- file.path(q_to_r_data_path, "sections_boundaries.shp")
  
  # Try GeoPackage first
  if (file.exists(gpkg_path)) {
    message("Loading section geometries from shared Q_to_R: ", gpkg_path)
    sections_sf <- st_read(gpkg_path, quiet = TRUE)
    
    # Ensure CRS is WGS84
    if (!st_is_longlat(sections_sf)) {
      sections_sf <- st_transform(sections_sf, 4326)
    }
    
    # Clean up any invalid geometries
    sections_sf <- st_make_valid(sections_sf)
    
    return(sections_sf)
  } 
  # Try shapefile next
  else if (file.exists(shp_path)) {
    message("Loading section geometries from shared Q_to_R: ", shp_path)
    sections_sf <- st_read(shp_path, quiet = TRUE)
    
    # Ensure CRS is WGS84
    if (!st_is_longlat(sections_sf)) {
      sections_sf <- st_transform(sections_sf, 4326)
    }
    
    # Clean up any invalid geometries
    sections_sf <- st_make_valid(sections_sf)
    
    return(sections_sf)
  } 
  # Fallback to database
  else {
    message("No geometry files found in shared Q_to_R, loading from database...")
    con <- get_db_connection()
    if (is.null(con)) return(NULL)
    on.exit(dbDisconnect(con))
    
    # Get full geometries from database
    sect_q <- "SELECT gid, sectcode, zone, facility, fosarea,
                      ST_Transform(the_geom, 4326) as geometry,
                      ST_X(ST_Transform(ST_Centroid(the_geom), 4326)) as lon, 
                      ST_Y(ST_Transform(ST_Centroid(the_geom), 4326)) as lat 
               FROM public.gis_sectcode
               WHERE the_geom IS NOT NULL"
    
    sections_sf <- st_read(con, query = sect_q, quiet = TRUE)
    return(sections_sf)
  }
}

# Return named vector of trap type choices
get_trap_type_choices <- function() {
  # Hardcoded survtype values
  choices <- c(
    "Elevated CO2" = "4",
    "Gravid Trap" = "5",
    "CO2 Overnight" = "6"
  )
  return(choices)
}

# Return named vector of species choices (numeric sppcode as value, "Genus species" as label)
get_species_choices <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(character(0))
  on.exit(dbDisconnect(con))

  q <- "SELECT sppcode, genus, species FROM public.lookup_specieslist ORDER BY genus, species"
  df <- dbGetQuery(con, q)
  if (nrow(df) == 0) return(character(0))
  
  # Create labels: "Genus species (sppcode)"
  labels <- paste0(df$genus, " ", df$species, " (", df$sppcode, ")")
  
  # Create named vector where VALUES are numeric sppcode and NAMES are the labels
  choices <- setNames(as.character(df$sppcode), labels)
  
  # Add "All" option at the beginning
  choices <- c("All Species" = "all", choices)
  return(choices)
}

# Compute section vector index using k-NN inverse-distance weighting
compute_section_vector_index <- function(species_codes, analysis_date = Sys.Date(), k = 4, facility_filter = NULL, trap_types = c("4", "5", "6")) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  on.exit(dbDisconnect(con))

  # Build trap type filter - survtype is varchar, so quote the values
  trap_type_filter <- paste(sprintf("'%s'", trap_types), collapse = ",")
  
  # Optimized query: get MOST RECENT trap inspection per location with species counts
  trap_q <- sprintf(
    "WITH ranked_traps AS (
       SELECT t.ainspecnum, t.facility, t.x, t.y, t.survtype, t.inspdate,
              ROW_NUMBER() OVER (PARTITION BY t.x, t.y, t.facility ORDER BY t.inspdate DESC) as rn
       FROM public.dbadult_insp_current t
       WHERE t.inspdate::date <= '%s'::date 
         AND t.x IS NOT NULL AND t.y IS NOT NULL
         AND t.survtype IN (%s)
         %s
     ),
     latest_traps AS (
       SELECT ainspecnum, facility, x, y, survtype, inspdate
       FROM ranked_traps
       WHERE rn = 1
     )
     SELECT lt.ainspecnum, lt.facility, lt.x as lon, lt.y as lat, lt.survtype, lt.inspdate::date as inspdate,
            COALESCE(SUM(s.cnt), 0) as species_count
     FROM latest_traps lt
     LEFT JOIN public.dbadult_species_current s ON lt.ainspecnum = s.ainspecnum %s
     GROUP BY lt.ainspecnum, lt.facility, lt.x, lt.y, lt.survtype, lt.inspdate",
    as.character(analysis_date),
    trap_type_filter,
    if (!is.null(facility_filter) && length(facility_filter) > 0 && !("all" %in% tolower(facility_filter))) {
      sprintf("AND t.facility IN (%s)", paste(sprintf("'%s'", facility_filter), collapse = ","))
    } else "",
    if (length(species_codes) > 0 && !("all" %in% tolower(species_codes))) {
      sprintf("AND s.spp IN (%s)", paste(species_codes, collapse = ","))
    } else ""
  )
  
  traps <- dbGetQuery(con, trap_q)
  
  # Debug: print query and row count
  message("Trap query returned ", nrow(traps), " rows")
  if (nrow(traps) == 0) {
    message("No traps found with query: ", trap_q)
    return(list(sections = data.frame(), traps = data.frame(), sections_sf = NULL))
  }

  # Load section geometries (with full polygons)
  sections_sf <- load_section_geometries()
  if (is.null(sections_sf) || nrow(sections_sf) == 0) {
    message("No section geometries found")
    return(list(sections = data.frame(), traps = data.frame(), sections_sf = NULL))
  }
  
  message("Section geometries loaded: ", nrow(sections_sf), " rows")

  # Create centroids for KNN calculation
  sections_centroids <- suppressWarnings(st_centroid(sections_sf))
  sects <- sections_centroids %>%
    mutate(
      lon = st_coordinates(.)[,1],
      lat = st_coordinates(.)[,2]
    ) %>%
    st_drop_geometry() %>%
    select(gid, sectcode, zone, facility, lon, lat)

  # Now do all spatial operations in R (faster than multiple DB calls)
  traps_sf <- st_as_sf(traps, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  sects_sf <- st_as_sf(sects, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  # Store original lon/lat before transformation
  sects_sf$orig_lon <- sects_sf$lon
  sects_sf$orig_lat <- sects_sf$lat
  
  traps_m <- st_transform(traps_sf, 3857)
  sects_m <- st_transform(sects_sf, 3857)

  # Vectorized k-NN computation
  results <- lapply(seq_len(nrow(sects_m)), function(i) {
    sect_row <- sects_m[i, ]
    dists <- as.numeric(st_distance(sect_row, traps_m))
    
    if (all(is.na(dists)) || length(dists) == 0) return(NULL)
    
    # Find k nearest
    k_actual <- min(k, length(dists))
    k_idx <- order(dists)[1:k_actual]
    k_dists <- dists[k_idx]
    k_dists[k_dists == 0] <- 1e-6
    
    # Inverse distance weighting
    weights <- 1 / k_dists
    counts <- as.numeric(traps_m$species_count[k_idx])  # Convert to numeric to avoid integer64 issues
    weighted_sum <- sum(weights * counts)
    weight_sum <- sum(weights)
    vector_index <- weighted_sum / weight_sum
    
    # Get inspection dates of k nearest traps
    nearest_dates <- traps_m$inspdate[k_idx]
    most_recent_date <- max(nearest_dates)
    
    # Debug for first few sections
    if (i <= 3) {
      message(sprintf("Section %d: k=%d, counts=%s (class=%s), dists=%s", 
                     i, k_actual, 
                     paste(counts, collapse=","),
                     class(counts),
                     paste(round(k_dists, 0), collapse=",")))
      message(sprintf("  weights=%s, weighted_sum=%.6f, weight_sum=%.6f, vector_index=%.2f", 
                     paste(sprintf("%.6f", weights), collapse=","),
                     weighted_sum, weight_sum, vector_index))
    }
    
    data.frame(
      gid = as.integer(sect_row$gid),
      sectcode = as.character(sect_row$sectcode),
      zone = as.character(sect_row$zone),
      facility = as.character(sect_row$facility),
      vector_index = vector_index,
      nearest_trap_count = sum(counts),
      last_inspection = as.character(most_recent_date),
      lon = sect_row$orig_lon,
      lat = sect_row$orig_lat,
      stringsAsFactors = FALSE
    )
  })
  
  results <- results[!sapply(results, is.null)]
  message("Computed vector index for ", length(results), " sections")
  if (length(results) == 0) {
    return(list(sections = data.frame(), traps = data.frame(), sections_sf = sections_sf))
  }
  
  section_results <- do.call(rbind, results)
  
  # Merge vector index results back with geometries
  sections_with_index <- sections_sf %>%
    left_join(section_results, by = "sectcode") %>%
    filter(!is.na(vector_index))
  
  # Return sections data, traps data, and section geometries
  return(list(
    sections = section_results,
    traps = traps %>% select(ainspecnum, facility, lon, lat, survtype, inspdate, species_count),
    sections_sf = sections_with_index
  ))
}
