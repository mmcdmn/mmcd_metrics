# =============================================================================
# DATA FUNCTIONS - Trap Surveillance (Pre-calculated Views)
# =============================================================================
# Uses pre-calculated materialized views for vector abundance and MLE/MIR data.
# NO in-app KNN or MLE calculations - everything comes from the database.
#
# Key tables:
#   dbadult_mon_nt_co2_forvectorabundance  - mosquito counts by trap/night/species/viarea
#   dbvirus_mle_yrwk_area                  - pre-calculated MLE by yrwk × viarea
#   dbvirus_mle_yrwk_area_spp             - pre-calculated MLE by yrwk × viarea × species
#   dbvirus_mir_yrwk_area                  - pre-calculated MIR by yrwk × viarea
#   dbvirus_mir_yrwk_area_spp             - pre-calculated MIR by yrwk × viarea × species
#   loc_vectorindexareas_sections_a        - section polygons grouped by viarea
# =============================================================================

library(DBI)
library(dplyr)
library(sf)

# Species mapping: spp_code (MLE/MIR tables) <-> spp_name (abundance table)
SPECIES_MAP <- list(
  "Total_Cx_vectors"        = list(code = NULL,  label = "Total Culex Vectors"),
  "Cx_pipiens_33"           = list(code = "33",  label = "Culex pipiens"),
  "Cx_restuans_34"          = list(code = "34",  label = "Culex restuans"),
  "Cx_tarsalis_36"          = list(code = "36",  label = "Culex tarsalis"),
  "Cx_restuans/pipiens_372" = list(code = "372", label = "Culex restuans/pipiens")
)

# Species choices for selectInput  (display label -> spp_name key)
get_surveillance_species_choices <- function() {
  choices <- sapply(SPECIES_MAP, function(x) x$label)
  names_vec <- names(SPECIES_MAP)
  names(names_vec) <- choices
  # Flip so display label is name, spp_name is value
  setNames(names(SPECIES_MAP), sapply(SPECIES_MAP, function(x) x$label))
}

# Convert spp_name to spp_code for MLE/MIR queries
spp_name_to_code <- function(spp_name) {
  if (is.null(spp_name) || spp_name == "Total_Cx_vectors") return(NULL)
  info <- SPECIES_MAP[[spp_name]]
  if (!is.null(info)) info$code else NULL
}

# =============================================================================
# ABUNDANCE DATA - Fetch vector abundance by area and week
# =============================================================================
fetch_abundance_data <- function(year = NULL, yrwk = NULL, spp_name = "Total_Cx_vectors") {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_disconnect(con))
  
  # Build filters
  where_clauses <- c("1=1")
  
  if (!is.null(year)) {
    where_clauses <- c(where_clauses, sprintf("year = %s", as.integer(year)))
  }
  
  if (!is.null(yrwk)) {
    where_clauses <- c(where_clauses, sprintf("yrwk = %s", as.numeric(yrwk)))
  }
  
  if (!is.null(spp_name) && spp_name != "all") {
    where_clauses <- c(where_clauses, sprintf("spp_name = '%s'", spp_name))
  }
  
  q <- sprintf(
    "SELECT viarea, loc_code, inspdate, year, yrwk, epiweek, spp_name, mosqcount
     FROM dbadult_mon_nt_co2_forvectorabundance
     WHERE %s
     ORDER BY yrwk DESC, viarea, loc_code",
    paste(where_clauses, collapse = " AND ")
  )
  
  tryCatch({
    data <- dbGetQuery(con, q)
    message(sprintf("Abundance query returned %d rows", nrow(data)))
    data
  }, error = function(e) {
    warning(paste("Abundance query failed:", e$message))
    NULL
  })
}

# Aggregate abundance by area for a given yrwk
fetch_abundance_by_area <- function(yrwk, spp_name = "Total_Cx_vectors") {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_disconnect(con))
  
  q <- sprintf(
    "SELECT viarea, 
            SUM(mosqcount) as total_count,
            COUNT(DISTINCT loc_code) as num_traps,
            CASE WHEN COUNT(DISTINCT loc_code) > 0 
                 THEN ROUND(SUM(mosqcount)::numeric / COUNT(DISTINCT loc_code), 2)
                 ELSE 0 END as avg_per_trap
     FROM dbadult_mon_nt_co2_forvectorabundance
     WHERE yrwk = %s AND spp_name = '%s'
     GROUP BY viarea
     ORDER BY viarea",
    as.numeric(yrwk), spp_name
  )
  
  tryCatch({
    data <- dbGetQuery(con, q)
    message(sprintf("Abundance by area: %d areas for yrwk %s", nrow(data), yrwk))
    data
  }, error = function(e) {
    warning(paste("Abundance by area query failed:", e$message))
    NULL
  })
}

# =============================================================================
# MLE DATA - Fetch pre-calculated MLE values
# =============================================================================

# MLE by yrwk × area (Total Cx vectors — all species combined)
fetch_mle_by_area <- function(yrwk) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_disconnect(con))
  
  q <- sprintf(
    "SELECT mle_id, yrwk, viarea, 
            p::numeric as mle, lower::numeric as mle_lower, upper::numeric as mle_upper
     FROM dbvirus_mle_yrwk_area
     WHERE yrwk = '%s'
     ORDER BY viarea",
    as.character(yrwk)
  )
  
  tryCatch({
    data <- dbGetQuery(con, q)
    message(sprintf("MLE by area: %d rows for yrwk %s", nrow(data), yrwk))
    data
  }, error = function(e) {
    warning(paste("MLE by area query failed:", e$message))
    NULL
  })
}

# MLE by yrwk × area × species (specific species)
fetch_mle_by_area_spp <- function(yrwk, spp_code) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_disconnect(con))
  
  q <- sprintf(
    "SELECT mle_id, yrwk, viarea, spp_code,
            p::numeric as mle, lower::numeric as mle_lower, upper::numeric as mle_upper
     FROM dbvirus_mle_yrwk_area_spp
     WHERE yrwk = '%s' AND spp_code = '%s'
     ORDER BY viarea",
    as.character(yrwk), as.character(spp_code)
  )
  
  tryCatch({
    data <- dbGetQuery(con, q)
    message(sprintf("MLE by area+spp: %d rows for yrwk %s, spp %s", nrow(data), yrwk, spp_code))
    data
  }, error = function(e) {
    warning(paste("MLE by area+spp query failed:", e$message))
    NULL
  })
}

# MLE district-wide by yrwk (for trend chart)
fetch_mle_trend <- function(year = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_disconnect(con))
  
  where_clause <- if (!is.null(year)) {
    sprintf("WHERE yrwk::text LIKE '%s%%'", as.character(year))
  } else ""
  
  q <- sprintf(
    "SELECT mle_id, yrwk, p::numeric as mle, lower::numeric as mle_lower, upper::numeric as mle_upper
     FROM dbvirus_mle_yrwk
     %s
     ORDER BY yrwk",
    where_clause
  )
  
  tryCatch({
    dbGetQuery(con, q)
  }, error = function(e) {
    warning(paste("MLE trend query failed:", e$message))
    NULL
  })
}

# MLE multi-year average by epiweek (for 5yr/10yr avg lines on trend chart)
# Returns avg MLE per epiweek across the given year range
fetch_mle_avg_by_epiweek <- function(current_year, n_years) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_disconnect(con))
  
  start_year <- as.integer(current_year) - n_years
  end_year   <- as.integer(current_year) - 1  # exclude current year
  
  # Extract epiweek from yrwk (last 2 digits) and average across years
  q <- sprintf(
    "SELECT (yrwk::integer %% 100) AS week,
            AVG(p::numeric) AS avg_mle
     FROM dbvirus_mle_yrwk
     WHERE (yrwk::integer / 100) BETWEEN %d AND %d
     GROUP BY (yrwk::integer %% 100)
     ORDER BY week",
    start_year, end_year
  )
  
  tryCatch({
    data <- dbGetQuery(con, q)
    data$week <- as.numeric(data$week)  # Ensure numeric for ggplot scale
    message(sprintf("MLE %d-yr avg: %d weeks from %d-%d", n_years, nrow(data), start_year, end_year))
    data
  }, error = function(e) {
    warning(paste("MLE avg query failed:", e$message))
    NULL
  })
}

# Abundance multi-year average by epiweek for a species (for 5yr/10yr avg lines)
# Returns avg abundance per trap per epiweek across the given year range
fetch_abundance_avg_by_epiweek <- function(current_year, n_years, spp_name = "Total_Cx_vectors") {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_disconnect(con))
  
  start_year <- as.integer(current_year) - n_years
  end_year   <- as.integer(current_year) - 1
  
  q <- sprintf(
    "SELECT epiweek AS week,
            SUM(mosqcount)::numeric / GREATEST(COUNT(DISTINCT loc_code), 1) AS avg_per_trap
     FROM dbadult_mon_nt_co2_forvectorabundance
     WHERE year BETWEEN %d AND %d
       AND spp_name = '%s'
     GROUP BY epiweek
     ORDER BY epiweek",
    start_year, end_year, spp_name
  )
  
  tryCatch({
    data <- dbGetQuery(con, q)
    data$week <- as.numeric(data$week)  # Ensure numeric for ggplot scale
    message(sprintf("Abundance %d-yr avg: %d weeks from %d-%d", n_years, nrow(data), start_year, end_year))
    data
  }, error = function(e) {
    warning(paste("Abundance avg query failed:", e$message))
    NULL
  })
}

# =============================================================================
# MIR DATA - Fetch pre-calculated MIR values
# =============================================================================

# MIR by yrwk × area (Total Cx vectors)
fetch_mir_by_area <- function(yrwk) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_disconnect(con))
  
  q <- sprintf(
    "SELECT mir_id, year, yrwk, viarea,
            positive::integer as positive, total::integer as total_pools, 
            mosquitoes::integer as total_mosquitoes, mir::numeric as mir
     FROM dbvirus_mir_yrwk_area
     WHERE yrwk = '%s'
     ORDER BY viarea",
    as.character(yrwk)
  )
  
  tryCatch({
    data <- dbGetQuery(con, q)
    message(sprintf("MIR by area: %d rows for yrwk %s", nrow(data), yrwk))
    data
  }, error = function(e) {
    warning(paste("MIR by area query failed:", e$message))
    NULL
  })
}

# MIR by yrwk × area × species
fetch_mir_by_area_spp <- function(yrwk, spp_code) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_disconnect(con))
  
  q <- sprintf(
    "SELECT mir_id, year, yrwk, viarea, spp_code,
            positive::integer as positive, total::integer as total_pools, 
            mosquitoes::integer as total_mosquitoes, mir::numeric as mir
     FROM dbvirus_mir_yrwk_area_spp
     WHERE yrwk = '%s' AND spp_code = '%s'
     ORDER BY viarea",
    as.character(yrwk), as.character(spp_code)
  )
  
  tryCatch({
    data <- dbGetQuery(con, q)
    message(sprintf("MIR by area+spp: %d rows for yrwk %s, spp %s", nrow(data), yrwk, spp_code))
    data
  }, error = function(e) {
    warning(paste("MIR by area+spp query failed:", e$message))
    NULL
  })
}

# =============================================================================
# GEOMETRY - Load vector index area polygons from database
# =============================================================================

# Cache for area geometries
.vi_area_geom_cache <- NULL

load_vi_area_geometries <- function(force_reload = FALSE) {
  if (!force_reload && !is.null(.vi_area_geom_cache)) {
    return(.vi_area_geom_cache)
  }
  
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_disconnect(con))
  
  # Load section polygons grouped by viarea, dissolve into area polygons
  q <- "SELECT viareaa as viarea, 
               ST_Transform(ST_Union(geom), 4326) as geom,
               COUNT(*) as num_sections,
               SUM(area) as total_area
        FROM loc_vectorindexareas_sections_a
        GROUP BY viareaa
        ORDER BY viareaa"
  
  tryCatch({
    areas_sf <- st_read(con, query = q)
    areas_sf <- st_make_valid(areas_sf)
    message(sprintf("Loaded %d vector index area polygons from database", nrow(areas_sf)))
    .vi_area_geom_cache <<- areas_sf
    areas_sf
  }, error = function(e) {
    warning(paste("Failed to load VI area geometries:", e$message))
    # Fallback: try shapefile
    load_vi_area_geometries_from_shapefile()
  })
}

# Fallback: load from shapefile if DB geometry load fails
load_vi_area_geometries_from_shapefile <- function() {
  q_to_r_path <- file.path("..", "..", "shared", "Q_to_R", "data")
  shp_path <- file.path(q_to_r_path, "VectorIndexAreasA2025.shp")
  
  if (!file.exists(shp_path)) {
    warning("VectorIndexAreasA2025.shp not found")
    return(NULL)
  }
  
  tryCatch({
    areas_sf <- st_read(shp_path, quiet = TRUE)
    if (st_crs(areas_sf)$epsg != 4326) {
      areas_sf <- st_transform(areas_sf, 4326)
    }
    areas_sf <- st_make_valid(areas_sf)
    
    # Dissolve by VIareaA
    if ("VIareaA" %in% names(areas_sf)) {
      old_s2 <- sf_use_s2()
      sf_use_s2(FALSE)
      areas_sf <- areas_sf %>%
        group_by(viarea = VIareaA) %>%
        summarize(
          num_sections = n(),
          total_area = sum(AREA, na.rm = TRUE),
          .groups = "drop"
        )
      sf_use_s2(old_s2)
    }
    
    message(sprintf("Loaded %d VI area polygons from shapefile", nrow(areas_sf)))
    .vi_area_geom_cache <<- areas_sf
    areas_sf
  }, error = function(e) {
    warning(paste("Failed to load shapefile:", e$message))
    NULL
  })
}

# Load background layers (facility boundaries, counties) for map display
load_background_layers <- function() {
  q_to_r_path <- file.path("..", "..", "shared", "Q_to_R", "data")
  mosquito_map_path <- file.path("..", "mosquito_surveillance_map", "shp")
  
  layers <- list()
  
  # Facility boundaries
  facilities_path <- file.path(q_to_r_path, "facility_boundaries.shp")
  if (file.exists(facilities_path)) {
    layers$facilities <- st_read(facilities_path, quiet = TRUE)
  }
  
  # Zone boundaries
  zones_path <- file.path(q_to_r_path, "zone_boundaries.shp")
  if (file.exists(zones_path)) {
    layers$zones <- st_read(zones_path, quiet = TRUE)
  }
  
  # Counties
  counties_path <- file.path(mosquito_map_path, "Counties_4326.shp")
  if (file.exists(counties_path)) {
    layers$counties <- st_read(counties_path, quiet = TRUE)
  }
  
  return(layers)
}

# =============================================================================
# AVAILABLE WEEKS - Get list of yrwk values for dropdown
# =============================================================================
fetch_available_weeks <- function(year = NULL) {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_disconnect(con))
  
  where_clause <- if (!is.null(year)) {
    sprintf("WHERE a.year = %s", as.integer(year))
  } else ""
  
  q <- sprintf(
    "SELECT DISTINCT a.yrwk, a.epiweek, a.year, l.week_days
     FROM dbadult_mon_nt_co2_forvectorabundance a
     LEFT JOIN lookup_weeknum l ON a.yrwk::text = l.wknumyr::text
     %s
     ORDER BY a.yrwk DESC",
    where_clause
  )
  
  tryCatch({
    data <- dbGetQuery(con, q)
    data
  }, error = function(e) {
    warning(paste("Available weeks query failed:", e$message))
    # Fallback without date ranges if lookup table doesn't exist
    q_fallback <- sprintf(
      "SELECT DISTINCT yrwk, epiweek, year, NULL as week_days
       FROM dbadult_mon_nt_co2_forvectorabundance
       %s
       ORDER BY yrwk DESC",
      where_clause
    )
    tryCatch({
      dbGetQuery(con, q_fallback)
    }, error = function(e2) {
      warning(paste("Fallback weeks query also failed:", e2$message))
      NULL
    })
  })
}

# Get available years
fetch_available_years <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  on.exit(safe_disconnect(con))
  
  q <- "SELECT DISTINCT year FROM dbadult_mon_nt_co2_forvectorabundance ORDER BY year DESC"
  
  tryCatch({
    data <- dbGetQuery(con, q)
    data$year
  }, error = function(e) {
    warning(paste("Available years query failed:", e$message))
    NULL
  })
}

# =============================================================================
# COMBINED DATA - Merge abundance + MLE/MIR for map display
# =============================================================================
fetch_combined_area_data <- function(yrwk, spp_name = "Total_Cx_vectors", 
                                     infection_metric = "mle") {
  
  # 1) Get abundance data for this week/species
  abundance <- fetch_abundance_by_area(yrwk, spp_name)
  
  # 2) Get infection rate data
  spp_code <- spp_name_to_code(spp_name)
  
  if (infection_metric == "mle") {
    if (is.null(spp_code)) {
      # Total Cx vectors -> use area-level (all species combined)
      infection <- fetch_mle_by_area(yrwk)
    } else {
      infection <- fetch_mle_by_area_spp(yrwk, spp_code)
    }
    # Standardize column names
    if (!is.null(infection) && nrow(infection) > 0) {
      infection <- infection %>%
        select(viarea, infection_rate = mle, rate_lower = mle_lower, rate_upper = mle_upper)
    }
  } else {
    # MIR
    if (is.null(spp_code)) {
      infection <- fetch_mir_by_area(yrwk)
    } else {
      infection <- fetch_mir_by_area_spp(yrwk, spp_code)
    }
    if (!is.null(infection) && nrow(infection) > 0) {
      infection <- infection %>%
        mutate(infection_rate = mir / 1000) %>%  # Scale MIR to per-mosquito for VI calc
        select(viarea, infection_rate, positive, total_pools, total_mosquitoes,
               mir_raw = mir)
    }
  }
  
  # 3) Merge abundance + infection
  if (is.null(abundance) || nrow(abundance) == 0) {
    return(NULL)
  }
  
  combined <- abundance
  if (!is.null(infection) && nrow(infection) > 0) {
    combined <- combined %>%
      left_join(infection, by = "viarea")
  } else {
    # No infection data at all for this week/species - set all to 0
    combined$infection_rate <- 0
  }
  
  # For areas with abundance but no infection data, treat as 0 infection rate (not NA!)
  # 0 is a real value meaning "no infection detected" — NOT "no data"
  combined <- combined %>%
    mutate(
      infection_rate = ifelse(is.na(infection_rate) & !is.na(total_count), 0, infection_rate)
    )
  
  # Ensure mir_raw is 0 (not NA) for areas with abundance but no MIR match
  if ("mir_raw" %in% names(combined)) {
    combined <- combined %>%
      mutate(mir_raw = ifelse(is.na(mir_raw) & !is.na(total_count), 0, mir_raw))
  }
  # Ensure MLE columns are 0 (not NA) for areas with abundance but no MLE match
  if ("rate_lower" %in% names(combined)) {
    combined <- combined %>%
      mutate(
        rate_lower = ifelse(is.na(rate_lower) & !is.na(total_count), 0, rate_lower),
        rate_upper = ifelse(is.na(rate_upper) & !is.na(total_count), 0, rate_upper)
      )
  }
  
  # 4) Calculate Vector Index = avg_per_trap × infection_rate
  combined <- combined %>%
    mutate(
      # Now calculate vector index - use 0 infection rate when available
      vector_index = ifelse(!is.na(avg_per_trap) & !is.na(infection_rate),
                            avg_per_trap * infection_rate, NA_real_)
    )
  
  message(sprintf("Combined data: %d areas, %d with infection data, %d with zero infection rate, %d with vector index",
                  nrow(combined),
                  sum(!is.na(combined$infection_rate) & combined$infection_rate > 0),
                  sum(!is.na(combined$infection_rate) & combined$infection_rate == 0),
                  sum(!is.na(combined$vector_index))))
  
  combined
}
