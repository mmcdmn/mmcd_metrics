# =============================================================================
# STUB DATA: SUCO History App
# =============================================================================
# Provides realistic test data for suco_history unit tests
# Based on actual dbadult_insp_current and dbadult_species_current schema
# =============================================================================

#' Get stub SUCO inspection data
#' 
#' @param n Number of records to generate (default: 10)
#' @return Data frame matching get_suco_data output
#' @export
get_stub_suco_inspections <- function(n = 10) {
  set.seed(42)  # Reproducible
  
  # Sample data matching real schema
  facilities <- c("E", "N", "W", "Wm", "Sj")
  foremen <- c("8201", "8202", "8203", "8207", "0204", "7002")
  zones <- c("1", "2")
  
  # Generate sample sitecodes (format: XXXXXX-XXX)
  sitecodes <- paste0(
    sprintf("%06d", sample(100000:999999, n, replace = TRUE)),
    "-",
    sprintf("%03d", sample(100:999, n, replace = TRUE))
  )
  
  # Generate dates within 2025
  start_date <- as.Date("2025-01-01")
  end_date <- as.Date("2025-12-31")
  dates <- sample(seq(start_date, end_date, by = "day"), n, replace = TRUE)
  
  # Sample locations
  park_names <- c(
    "Central Park", "Lake View Park", "Forest Reserve", 
    "Riverside Trail", "Oak Grove", "Maple Heights",
    NA, NA  # Some will have no park name
  )
  
  addresses <- c(
    "123 Main St", "456 Oak Ave", "789 Lake Rd",
    "321 River Blvd", "555 Forest Ln",
    NA, NA  # Some will have no address
  )
  
  # Generate coordinates (Twin Cities area: ~44.9-45.1 lat, ~93.0-93.4 lng)
  lngs <- runif(n, -93.4, -93.0)
  lats <- runif(n, 44.9, 45.1)
  
  data.frame(
    id = 1:n,
    ainspecnum = paste0("SUCO", sprintf("%06d", 1:n)),
    facility = sample(facilities, n, replace = TRUE),
    foreman = sample(foremen, n, replace = TRUE),
    inspdate = dates,
    sitecode = sitecodes,
    address1 = sample(addresses, n, replace = TRUE),
    park_name = sample(park_names, n, replace = TRUE),
    survtype = "7",  # SUCO survey type
    fieldcount = sample(0:20, n, replace = TRUE),
    comments = NA_character_,
    x = lngs,
    y = lats,
    geometry_text = paste0("POINT(", lngs, " ", lats, ")"),
    zone = sample(zones, n, replace = TRUE),
    source_table = "current",
    year = as.integer(format(dates, "%Y")),
    month = as.integer(format(dates, "%m")),
    week_start = lubridate::floor_date(dates, "week", week_start = 1),
    epi_week = lubridate::epiweek(dates),
    epi_year = lubridate::epiyear(dates),
    epi_week_label = paste0("EW ", lubridate::epiweek(dates)),
    month_label = format(dates, "%b %Y"),
    location = ifelse(
      !is.na(park_names[sample(1:length(park_names), n, replace = TRUE)]),
      park_names[sample(1:length(park_names), n, replace = TRUE)],
      addresses[sample(1:length(addresses), n, replace = TRUE)]
    ),
    species_summary = c(
      "Aedes triseriatus: 5<br>Aedes japonicus: 3",
      "Culex tarsalis: 2",
      "No species identified",
      "Aedes triseriatus: 10",
      "Multiple species: 15",
      if (n > 5) rep("Aedes japonicus: 1<br>Culex tarsalis: 1", n - 5) else NULL
    )[1:n],
    total_species_count = c(8, 2, 0, 10, 15, if (n > 5) rep(2, n - 5) else NULL)[1:n],
    stringsAsFactors = FALSE
  )
}

#' Get stub species data
#' 
#' @param inspection_data Data frame from get_stub_suco_inspections
#' @return Data frame matching species table structure
#' @export
get_stub_suco_species <- function(inspection_data = NULL) {
  if (is.null(inspection_data)) {
    inspection_data <- get_stub_suco_inspections()
  }
  
  # Species codes and names
  species <- data.frame(
    sppcode = c(1, 2, 3, 4, 5),
    genus = c("Aedes", "Aedes", "Culex", "Culiseta", "Aedes"),
    species = c("triseriatus", "japonicus", "tarsalis", "melanura", "albopictus"),
    stringsAsFactors = FALSE
  )
  
  # Create species records for each inspection
  species_records <- list()
  
  for (i in seq_len(nrow(inspection_data))) {
    insp <- inspection_data[i, ]
    
    # Random number of species (0-3) per inspection
    n_species <- sample(0:3, 1)
    
    if (n_species > 0) {
      selected_species <- sample(1:5, n_species, replace = FALSE)
      
      for (spp in selected_species) {
        species_records[[length(species_records) + 1]] <- data.frame(
          ainspecnum = insp$ainspecnum,
          spp = spp,
          cnt = sample(1:10, 1),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(species_records) > 0) {
    do.call(rbind, species_records)
  } else {
    data.frame(ainspecnum = character(), spp = integer(), cnt = integer())
  }
}

#' Get stub filtered SUCO data (after filter_suco_data would be applied)
#' 
#' @param facility_filter Optional facility filter
#' @param zone_filter Optional zone filter
#' @param foreman_filter Optional foreman filter
#' @return Filtered data frame
#' @export
get_stub_filtered_suco_data <- function(facility_filter = NULL, 
                                         zone_filter = NULL, 
                                         foreman_filter = NULL) {
  data <- get_stub_suco_inspections(20)
  
  if (!is.null(facility_filter) && facility_filter != "all") {
    data <- data[data$facility == facility_filter, ]
  }
  
  if (!is.null(zone_filter) && zone_filter != "all") {
    data <- data[data$zone == zone_filter, ]
  }
  
  if (!is.null(foreman_filter) && !("all" %in% foreman_filter)) {
    data <- data[data$foreman %in% foreman_filter, ]
  }
  
  data
}

#' Get stub spatial SUCO data (sf object)
#' 
#' @param n Number of records
#' @return sf object matching create_spatial_data output
#' @export
get_stub_spatial_suco_data <- function(n = 10) {
  data <- get_stub_suco_inspections(n)
  
  # Add spatial columns
  data$longitude <- data$x
  data$latitude <- data$y
  data$display_species_count <- data$total_species_count
  data$marker_size <- ifelse(data$total_species_count == 0, 4,
                              ifelse(data$total_species_count <= 5, 8, 12))
  
  # Convert to sf
  sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
}

message(" stub_suco_data.R loaded - SUCO test data available")
