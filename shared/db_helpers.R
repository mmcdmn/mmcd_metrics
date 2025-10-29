# =============================================================================
# MMCD METRICS - DATABASE HELPER FUNCTIONS
# =============================================================================
# This file contains shared database connections, lookup functions, and 
# utility functions used across multiple MMCD dashboard applications.
# 
# All apps should source this file: source("../../shared/db_helpers.R")
# =============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
})

# Load environment variables function
load_env_vars <- function() {
  # Load environment variables from .env file (for local development)
  # or from Docker environment variables (for production)
  env_paths <- c(
    "../../.env",           # For local development from apps/*/
    "../../../.env",        # Alternative local path
    "/srv/shiny-server/.env", # Docker path
    ".env"                  # Root directory (when called from shared/)
  )
  
  # Try to load from .env file first
  env_loaded <- FALSE
  for (path in env_paths) {
    if (file.exists(path)) {
      readRenviron(path)
      env_loaded <- TRUE
      break
    }
  }
  
  # Environment variables might already be set (Docker)
  required_vars <- c("POSTGRES_HOST", "POSTGRES_PORT", "POSTGRES_DB", 
                     "POSTGRES_USER", "POSTGRES_PASSWORD")
  
  if (!env_loaded && !all(sapply(required_vars, function(var) Sys.getenv(var) != ""))) {
    warning("Could not load environment variables from .env file and required variables are not set")
    return(FALSE)
  }
  
  return(TRUE)
}

# Centralized database connection function
get_db_connection <- function() {
  # Load environment variables
  if (!load_env_vars()) {
    warning("Failed to load environment variables")
    return(NULL)
  }
  
  tryCatch({
    con <- dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("POSTGRES_HOST"),
      port = as.numeric(Sys.getenv("POSTGRES_PORT")),
      dbname = Sys.getenv("POSTGRES_DB"),
      user = Sys.getenv("POSTGRES_USER"),
      password = Sys.getenv("POSTGRES_PASSWORD")
    )
    return(con)
  }, error = function(e) {
    warning(paste("Database connection failed:", e$message))
    return(NULL)
  })
}

# Database connection function
get_db_connection <- function() {
  tryCatch({
    # Load environment variables
    load_env_vars()
    
    # Database configuration using environment variables
    db_host <- Sys.getenv("DB_HOST")
    db_port <- Sys.getenv("DB_PORT")
    db_user <- Sys.getenv("DB_USER")
    db_password <- Sys.getenv("DB_PASSWORD")
    db_name <- Sys.getenv("DB_NAME")
    
    # Check if all required environment variables are set
    if (any(c(db_host, db_port, db_user, db_password, db_name) == "")) {
      warning("Missing required database environment variables")
      return(NULL)
    }
    
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = db_name,
      host = db_host,
      port = as.numeric(db_port),
      user = db_user,
      password = db_password
    )
    
    return(con)
  }, error = function(e) {
    warning(paste("Database connection failed:", e$message))
    return(NULL)
  })
}

# Facility lookup functions
get_facility_lookup <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Get facility lookup from gis_facility table, excluding special facilities
    facilities <- dbGetQuery(con, "
      SELECT DISTINCT 
        abbrv as short_name,
        city as full_name
      FROM public.gis_facility
      WHERE abbrv NOT IN ('OT', 'MF', 'AW', 'RW')
      ORDER BY abbrv
    ")
    
    dbDisconnect(con)
    return(facilities)
    
  }, error = function(e) {
    warning(paste("Error loading facility lookup:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}

# Get facility choices for selectInput widgets
get_facility_choices <- function(include_all = TRUE) {
  facilities <- get_facility_lookup()
  
  if (nrow(facilities) == 0) {
    return(c("All Facilities" = "all"))
  }
  
  choices <- setNames(facilities$short_name, facilities$full_name)
  
  if (include_all) {
    choices <- c("All Facilities" = "all", choices)
  }
  
  return(choices)
}

# Map facility short codes to full names for display
map_facility_names <- function(data, facility_col = "facility") {
  facilities <- get_facility_lookup()
  
  if (nrow(facilities) == 0 || !facility_col %in% names(data)) {
    return(data)
  }
  
  # Create a named vector for mapping
  facility_map <- setNames(facilities$full_name, facilities$short_name)
  
  # Map the facility names, keeping original if no mapping found
  data[[paste0(facility_col, "_display")]] <- ifelse(
    data[[facility_col]] %in% names(facility_map),
    facility_map[data[[facility_col]]],
    data[[facility_col]]
  )
  
  return(data)
}

# Priority lookup
get_priority_choices <- function(include_all = TRUE) {
  choices <- c("HIGH" = "RED", "MEDIUM" = "YELLOW", "LOW" = "BLUE","GREEN" = "PREHATCH")
  
  if (include_all) {
    choices <- c("All Priorities" = "all", choices)
  }
  
  return(choices)
}

# Material type lookup
get_material_types <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    materials <- dbGetQuery(con, "
      SELECT 
        list_type,
        matcode,
        display_text,
        order_num,
        heading,
        startdate,
        enddate,
        tdose,
        unit,
        area,
        tdosedisplay,
        mattype
      FROM lookup_matcode_entrylist 
      ORDER BY matcode
    ")
    
    dbDisconnect(con)
    return(materials)
    
  }, error = function(e) {
    warning(paste("Error loading material types:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}

# Get material choices for selectInput widgets
get_material_choices <- function(include_all = TRUE) {
  materials <- get_material_types()
  
  if (nrow(materials) == 0) {
    return(c("All Materials" = "all"))
  }
  
  # Use display_text if available, otherwise fall back to matcode
  labels <- ifelse(!is.na(materials$display_text) & materials$display_text != "", 
                   materials$display_text, 
                   materials$matcode)
  
  choices <- setNames(materials$matcode, labels)
  
  if (include_all) {
    choices <- c("All Materials" = "all", choices)
  }
  
  return(choices)
}

# Get foremen (field supervisors) lookup table
get_foremen_lookup <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
        # Get active field supervisors basic info
    foremen <- dbGetQuery(con, "
      SELECT 
        emp_num,
        shortname,
        facility
      FROM employee_list 
      WHERE emp_type = 'FieldSuper'
        AND active = true 
        AND facility IS NOT NULL
      ORDER BY facility, shortname
    ")
    
    dbDisconnect(con)
    return(foremen)
    
  }, error = function(e) {
    warning(paste("Error loading foremen lookup:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}

# Get species lookup table
get_species_lookup <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    # Get species lookup from lookup_specieslist table
    species_lookup <- dbGetQuery(con, "
      SELECT 
        sppcode,
        genus,
        species
      FROM lookup_specieslist 
      ORDER BY genus, species
    ")
    
    dbDisconnect(con)
    return(species_lookup)
    
  }, error = function(e) {
    warning(paste("Error loading species lookup:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}

# Get species code to name mapping
get_species_code_map <- function() {
  species_lookup <- get_species_lookup()
  
  if (nrow(species_lookup) == 0) {
    return(character(0))
  }
  
  # Create mapping from sppcode to formatted species name
  species_map <- character(0)
  
  for (i in 1:nrow(species_lookup)) {
    code <- as.character(species_lookup$sppcode[i])
    genus <- species_lookup$genus[i]
    species <- species_lookup$species[i]
    
    # Create formatted name
    if (!is.na(genus) && !is.na(species) && genus != "" && species != "") {
      # Use abbreviated genus (first 2 letters) + full species name
      formatted_name <- paste0(substr(genus, 1, 2), ". ", species)
    } else if (!is.na(genus) && genus != "") {
      formatted_name <- genus
    } else if (!is.na(species) && species != "") {
      formatted_name <- species
    } else {
      formatted_name <- paste0("Species ", code)
    }
    
    species_map[code] <- formatted_name
  }
  
  return(species_map)
}



# Internal helper function to generate visually distinct colors
# This function creates a set of unique colors that are visually distinct from each other
# Parameters:
#   n: Number of colors to generate
# Returns: Vector of hex color codes
generate_distinct_colors <- function(n) {
  if (n <= 0) return(character(0))
  
  # Use HSV color space for even distribution
  hues <- seq(0, 1, length.out = n + 1)[1:n]  # Spread hues evenly
  
  # Create colors with constant saturation and value for consistency
  colors <- sapply(hues, function(h) {
    hsv(h = h, s = 0.8, v = 0.9)
  })
  
  return(colors)
}

#' Get Consistent Facility Colors
#' 
#' This function generates and returns a consistent color mapping for facilities.
#' Each facility gets assigned a unique color that remains consistent across all visualizations.
#' 
#' @param alpha_zones Optional. Vector of zone identifiers (e.g., c("1", "2")) to apply 
#'   alpha transparency for zone differentiation. P1 zones get full opacity (1.0), 
#'   P2 zones get reduced opacity (0.6). If NULL, returns standard colors.
#' @param combined_groups Optional. Vector of combined group names (e.g., "AP (P1)", "NM (P2)")
#'   to extract base facility names from for color mapping. Used with alpha_zones.
#' 
#' Usage:
#' ```r
#' # Standard facility colors:
#' facility_colors <- get_facility_base_colors()
#' ggplot(data, aes(x = x, y = y, color = facility)) +
#'   scale_color_manual(values = facility_colors)
#'
#' # Zone-aware colors with alpha:
#' zone_colors <- get_facility_base_colors(
#'   alpha_zones = c("1", "2"), 
#'   combined_groups = unique(data$combined_group)
#' )
#' ggplot(data, aes(x = x, y = y, color = combined_group, alpha = zone_factor)) +
#'   scale_color_manual(values = zone_colors$colors) +
#'   scale_alpha_manual(values = zone_colors$alpha_values)
#' ```
#' 
#' Returns:
#'   If alpha_zones is NULL: Named vector where names are facility short names and values are hex colors.
#'   If alpha_zones provided: List with $colors (named vector) and $alpha_values (named vector for zones).
get_facility_base_colors <- function(alpha_zones = NULL, combined_groups = NULL) {
  facilities <- get_facility_lookup()
  if (nrow(facilities) == 0) return(c())
  
  # Generate one color per facility
  colors <- generate_distinct_colors(nrow(facilities))
  
  # Map colors to facility short names
  result <- setNames(colors, facilities$short_name)
  
  # Handle zone differentiation if requested
  if (!is.null(alpha_zones) && length(alpha_zones) > 1 && !is.null(combined_groups)) {
    # Extract base names from combined groups and map to colors
    combined_colors <- character(0)
    for (combined_name in combined_groups) {
      base_name <- gsub("\\s*\\([^)]+\\)$", "", combined_name)
      base_name <- trimws(base_name)
      if (base_name %in% names(result)) {
        combined_colors[combined_name] <- result[base_name]
      }
    }
    
    # Return zone-aware result
    return(list(
      colors = combined_colors,
      alpha_values = c("1" = 1.0, "2" = 0.6)
    ))
  }
  
  return(result)
}

#' Get Consistent Foreman Colors Based on Facility
#' 
#' This function generates and returns a color mapping for foremen where each foreman's
#' color is a variation of their facility's base color. This ensures that foremen from
#' the same facility have similar but distinguishable colors.
#' 
#' @param alpha_zones Optional. Vector of zone identifiers (e.g., c("1", "2")) to apply 
#'   alpha transparency for zone differentiation. P1 zones get full opacity (1.0), 
#'   P2 zones get reduced opacity (0.6). If NULL, returns standard colors.
#' @param combined_groups Optional. Vector of combined group names (e.g., "John S. (P1)", "Jane D. (P2)")
#'   to extract base foreman names from for color mapping. Used with alpha_zones.
#' 
#' Important Notes:
#' 1. The foreman colors are based on employee numbers (e.g., "7002", "8203")
#' 2. You must use get_foremen_lookup() to map between employee numbers and names
#' 
#' Usage:
#' ```r
#' # Standard foreman colors:
#' foreman_colors <- get_foreman_colors()
#' foremen_lookup <- get_foremen_lookup()
#' emp_colors <- setNames(foreman_colors[foremen_lookup$shortname], foremen_lookup$emp_num)
#' ggplot(data, aes(x = x, y = y, color = foreman)) +
#'   scale_color_manual(values = emp_colors)
#' 
#' # Zone-aware colors:
#' zone_colors <- get_foreman_colors(
#'   alpha_zones = c("1", "2"), 
#'   combined_groups = unique(data$combined_group)
#' )
#' ggplot(data, aes(x = x, y = y, color = combined_group, alpha = zone_factor)) +
#'   scale_color_manual(values = zone_colors$colors) +
#'   scale_alpha_manual(values = zone_colors$alpha_values)
#' ```
#' 
#' Returns:
#'   If alpha_zones is NULL: Named vector where names are foreman shortnames and values are hex colors.
#'   If alpha_zones provided: List with $colors (named vector) and $alpha_values (named vector for zones).
get_foreman_colors <- function(alpha_zones = NULL, combined_groups = NULL) {
  foremen <- get_foremen_lookup()
  if (nrow(foremen) == 0) return(c())
  
  facility_colors <- get_facility_base_colors()
  foreman_colors <- character(nrow(foremen))
  
  # For each facility
  for (facility in unique(foremen$facility)) {
    # Get foremen for this facility
    facility_foremen <- foremen[foremen$facility == facility, ]
    n_foremen <- nrow(facility_foremen)
    
    # Get base color for facility
    base_color <- facility_colors[facility]
    if (is.na(base_color)) next
    
    # Convert base color to HSV for manipulation
    rgb_base <- col2rgb(base_color) / 255
    hsv_base <- rgb2hsv(rgb_base[1], rgb_base[2], rgb_base[3])
    
    # Generate variations of the base color
    if (n_foremen == 1) {
      # Single foreman gets the facility base color
      foreman_colors[foremen$shortname == facility_foremen$shortname] <- base_color
    } else {
      # Sort foremen by shortname for consistent ordering
      facility_foremen <- facility_foremen[order(facility_foremen$shortname), ]
      base_hue <- hsv_base[1]
      
      # Use a smaller hue range for better facility color grouping
      hue_range <- 0.08  # ±8% variation around facility color (was 15%)
      
      # Create distinct but related colors for foremen in this facility
      for (i in 1:n_foremen) {
        foreman_name <- facility_foremen$shortname[i]
        
        # Calculate hue offset based on position
        if (n_foremen == 2) {
          # For 2 foremen: one slightly lighter, one slightly darker
          hue_offset <- ifelse(i == 1, -hue_range/2, hue_range/2)
        } else {
          # For 3+ foremen: spread across the hue range
          hue_offset <- -hue_range + (2 * hue_range * (i - 1) / (n_foremen - 1))
        }
        
        # Calculate foreman hue (keep within facility color family)
        foreman_hue <- (base_hue + hue_offset) %% 1
        
        # Vary saturation and brightness more distinctly
        if (n_foremen <= 3) {
          # For small groups, use more pronounced saturation/value differences
          saturation <- 0.6 + (0.35 * (i - 1) / max(1, n_foremen - 1))  # 0.6 to 0.95
          value <- 0.65 + (0.3 * (i - 1) / max(1, n_foremen - 1))       # 0.65 to 0.95
        } else {
          # For larger groups, use smaller but still distinct differences
          saturation <- 0.65 + (0.3 * (i - 1) / max(1, n_foremen - 1))  # 0.65 to 0.95
          value <- 0.7 + (0.25 * (i - 1) / max(1, n_foremen - 1))       # 0.7 to 0.95
        }
        
        # Assign the color for this foreman
        foreman_idx <- which(foremen$shortname == foreman_name)
        foreman_colors[foreman_idx] <- hsv(h = foreman_hue, s = saturation, v = value)
      }
    }
  }
  
  # Create named vector
  names(foreman_colors) <- foremen$shortname
  
  # Handle zone differentiation if requested
  if (!is.null(alpha_zones) && length(alpha_zones) > 1 && !is.null(combined_groups)) {
    # Extract base names from combined groups and map to colors
    combined_colors <- character(0)
    for (combined_name in combined_groups) {
      base_name <- gsub("\\s*\\([^)]+\\)$", "", combined_name)
      base_name <- trimws(base_name)
      if (base_name %in% names(foreman_colors)) {
        combined_colors[combined_name] <- foreman_colors[base_name]
      }
    }
    
    # Return zone-aware result
    return(list(
      colors = combined_colors,
      alpha_values = c("1" = 1.0, "2" = 0.6)
    ))
  }
  
  return(foreman_colors)
}

# Common date range options
get_date_range_choices <- function() {
  return(list(
    "Last 7 days" = 7,
    "Last 14 days" = 14,
    "Last 30 days" = 30,
    "Last 60 days" = 60,
    "Last 90 days" = 90,
    "Current year" = as.numeric(Sys.Date() - as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")))
  ))
}

# Format date for display
format_display_date <- function(date_col) {
  ifelse(is.na(date_col) | date_col == "", 
         "None", 
         tryCatch(format(as.Date(date_col), "%m/%d/%y"), 
                error = function(e) as.character(date_col)))
}

# Core status colors - single source of truth for all status indicators
get_status_colors <- function() {
  return(c(
    # Core status colors - no duplicates or aliases
    "active" = "#187018",      # forest green for active/in-progress/treatment
    "completed" = "#4169E1",   # Royal blue for completed
    "planned" = "#FFA500",     # Orange for planned/pending
    "needs_action" = "#FF4500", # Red-orange for needs inspection
    "needs_treatment" = "#FF0000", # Pure red for needs treatment
    "unknown" = "#A9A9A9"      # Dark gray for unknown status
  ))
}

# Map hex colors to Shiny named colors for valueBox and dashboard elements
# This converts db_helpers hex colors to Shiny's accepted named colors
get_shiny_colors <- function() {
  return(c(
    "active" = "olive",          # #187018 → olive (closest to forest green)
    "completed" = "blue",      # #4169E1 → blue
    "planned" = "orange",      # #FFA500 → orange
    "needs_action" = "yellow", # #FF4500 → yellow (closest to red-orange)
    "needs_treatment" = "red", # #FF0000 → red
    "unknown" = "gray"         # #A9A9A9 → gray
  ))
}

# Map status names to hex colors for visualizations (maps, charts, tables)
get_status_color_map <- function() {
  status_colors <- get_status_colors()
  return(list(
    "Unknown" = status_colors["unknown"],
    "Needs Inspection" = status_colors["planned"],      # Orange/yellow for needs inspection
    "Under Threshold" = status_colors["completed"],
    "Needs Treatment" = status_colors["needs_treatment"],
    "Active Treatment" = status_colors["active"]
  ))
}

# Get color descriptions for different status types
get_status_descriptions <- function() {
  return(c(
    # General status descriptions
    "U" = "Unknown status",
    "Unknown" = "Unknown status",
    "Needs Inspection" = "Requires inspection", 
    "Under Threshold" = "Below treatment threshold",
    "Needs Treatment" = "Requires treatment",
    "Active Treatment" = "Currently being treated",
    "Completed" = "Treatment completed",
    "Pending" = "Treatment pending",
    "In Progress" = "Work in progress",
    
    # Treatment progress descriptions
    "total" = "Total sites/acres available",
    
    # Priority descriptions
    "RED" = "High priority",
    "YELLOW" = "Medium priority", 
    "GREEN" = "PREHATCH",
    "HIGH" = "High priority",
    "MEDIUM" = "Medium priority",
    "LOW" = "Low priority",
    
    # Special status descriptions
    "PREHATCH" = "Prehatch site status"
  ))
}

# Mosquito Species Visualization Functions
# Centralized color and shape mappings for mosquito species in surveillance data

# Get color mappings for mosquito species
get_mosquito_species_colors <- function() {
  return(list(
    "Total_Ae_+_Cq" = "#000000", Total_Ae_springs = "#008000", Total_Ae_summers = "#ffa500",
    Cq_perturbans_42 = "#800080", Total_Cx_vectors = "#FF0000", Cx_erraticus_32 = "#000000",
    Cx_pipiens_33 = "#0000FF", Cx_restuans_34 = "#008000", Cx_salinarius_35 = "#87cefa",
    Cx_tarsalis_36 = "#a52a2a", Cx_territans_37 = "#00ff7f", "Cx_restuans/pipiens_372" = "#40e0d0",
    Cx_unknown_371 = "#ffa500", An_barberi_27 = "#FFFF00", An_earlei_28 = "#ffc0cb",
    An_punctipennis_29 = "#0000FF", An_quadrimaculatus_30 = "#FF0000", An_walkeri_31 = "#ffa500",
    sp311an_un = "#800080", Total_Anopheles = "#87cefa", sp01_abser = "#FF0000", sp03_aurif = "#FFFF00",
    sp04_euedes = "#f08080", sp05_campest = "#adff2f", sp08_commun = "#483d8b", sp09_diant = "#00FFFF",
    sp118abpun = "#800080", sp11_excru = "#ffa500", sp12_fitch = "#a52a2a", sp13_flave = "#800000",
    sp14_imple = "#7fff00", sp15_intrud = "#ffd700", sp17_pioni = "#FF00FF", sp18_punct = "#0000FF",
    sp19_ripar = "#008000", sp20_spenc = "#ff1493", sp22_stimu = "#708090", sp23_provo = "#ff6347",
    Ae_cinereus_7 = "#006400", Ae_triseriatus_24 = "#0000FF", Ae_vexans_26 = "#FF0000",
    sp02_atrop = "#ff1493", Ae_canadensis_6 = "#000000", Ae_dorsalis_10 = "#808080", sp16_nigro = "#ffd700",
    sp21_stict = "#FF00FF", sp25_trivi = "#800080", sp261ae_unid = "#000000", sp262spr_unid = "#008000",
    sp264summ_unid = "#ffa500", sp50_hende = "#7fff00", Ae_albopictus_51 = "#FF0000",
    Ae_japonicus_52 = "#008000", Ps_ciliata_44 = "#a52a2a", Ps_columbiae_45 = "#008000",
    Ps_ferox_46 = "#000000", sp471ps_un = "#808080", Ps_horrida_47 = "#FF0000", sp38_inorn = "#0000FF",
    Total_Psorophora = "#00FFFF", Culiseta_melanura = "#FF0000", sp40_minne = "#ffa500", sp41_morsi = "#a52a2a",
    sp411cs_un = "#808080", Or_signifera_43 = "#87cefa", Ur_sapphirina_48 = "#00008b", sp49_smith = "#0000FF"
  ))
}

# Get shape mappings for mosquito species (ggplot shape numbers)
get_mosquito_species_shapes <- function() {
  return(list(
    "Total_Ae_+_Cq" = 1, Total_Ae_springs = 1, Total_Ae_summers = 1, Cq_perturbans_42 = 1,
    Total_Cx_vectors = 1, Cx_erraticus_32 = 15, Cx_pipiens_33 = 15, Cx_restuans_34 = 15,
    Cx_salinarius_35 = 15, Cx_tarsalis_36 = 15, Cx_territans_37 = 15, "Cx_restuans/pipiens_372" = 15,
    Cx_unknown_371 = 15, An_barberi_27 = 4, An_earlei_28 = 4, An_punctipennis_29 = 4,
    An_quadrimaculatus_30 = 4, An_walkeri_31 = 4, sp311an_un = 4, Total_Anopheles = 4,
    sp01_abser = 19, sp03_aurif = 19, sp04_euedes = 19, sp05_campest = 19, sp08_commun = 19,
    sp09_diant = 19, sp118abpun = 19, sp11_excru = 19, sp12_fitch = 19, sp13_flave = 19,
    sp14_imple = 19, sp15_intrud = 19, sp17_pioni = 19, sp18_punct = 19, sp19_ripar = 19,
    sp20_spenc = 19, sp22_stimu = 19, sp23_provo = 19, Ae_cinereus_7 = 19, Ae_triseriatus_24 = 19,
    Ae_vexans_26 = 19, sp02_atrop = 19, Ae_canadensis_6 = 19, Ae_dorsalis_10 = 19, sp16_nigro = 19,
    sp21_stict = 19, sp25_trivi = 19, sp261ae_unid = 19, sp262spr_unid = 19, sp264summ_unid = 19,
    sp50_hende = 19, Ae_albopictus_51 = 19, Ae_japonicus_52 = 19, Ps_ciliata_44 = 3,
    Ps_columbiae_45 = 3, Ps_ferox_46 = 3, sp471ps_un = 3, Ps_horrida_47 = 3, sp38_inorn = 3,
    Total_Psorophora = 3, Culiseta_melanura = 18, sp40_minne = 18, sp41_morsi = 18, sp411cs_un = 18,
    Or_signifera_43 = 18, Ur_sapphirina_48 = 18, sp49_smith = 18
  ))
}

# Treatment Plan Type Colors
# Dynamic function to get treatment plan types and assign consistent colors

#' Get Treatment Plan Type Lookup
#' 
#' This function dynamically fetches the available treatment plan types from the database
#' and returns them with their full names for display purposes.
#' 
#' Returns:
#'   Data frame with columns: plan_code, plan_name, description
get_treatment_plan_types <- function() {
  con <- get_db_connection()
  if (is.null(con)) {
    # Return default mapping if database is unavailable
    return(data.frame(
      plan_code = c("A", "D", "G", "N", "U"),
      plan_name = c("Air", "Drone", "Ground", "None", "Unknown"),
      description = c("Air treatment", "Drone treatment", "Ground treatment", "No treatment planned", "Unknown treatment type"),
      stringsAsFactors = FALSE
    ))
  }
  
  tryCatch({
    # Get distinct treatment plan types from the current treatments table
    plan_types <- dbGetQuery(con, "
      SELECT DISTINCT 
        airgrnd_plan as plan_code,
        CASE 
          WHEN airgrnd_plan = 'A' THEN 'Air'
          WHEN airgrnd_plan = 'D' THEN 'Drone' 
          WHEN airgrnd_plan = 'G' THEN 'Ground'
          WHEN airgrnd_plan = 'N' THEN 'None'
          WHEN airgrnd_plan = 'U' THEN 'Unknown'
          ELSE airgrnd_plan
        END as plan_name,
        CASE 
          WHEN airgrnd_plan = 'A' THEN 'Air treatment'
          WHEN airgrnd_plan = 'D' THEN 'Drone treatment' 
          WHEN airgrnd_plan = 'G' THEN 'Ground treatment'
          WHEN airgrnd_plan = 'N' THEN 'No treatment planned'
          WHEN airgrnd_plan = 'U' THEN 'Unknown treatment type'
          ELSE 'Other treatment type'
        END as description
      FROM public.dblarv_insptrt_current 
      WHERE airgrnd_plan IS NOT NULL
      ORDER BY 
        CASE airgrnd_plan 
          WHEN 'A' THEN 1
          WHEN 'D' THEN 2
          WHEN 'G' THEN 3
          WHEN 'N' THEN 4
          WHEN 'U' THEN 5
          ELSE 6
        END
    ")
    
    dbDisconnect(con)
    return(plan_types)
    
  }, error = function(e) {
    warning(paste("Error loading treatment plan types:", e$message))
    if (!is.null(con)) dbDisconnect(con)
    
    # Return default mapping if query fails
    return(data.frame(
      plan_code = c("A", "D", "G", "N", "U"),
      plan_name = c("Air", "Drone", "Ground", "None", "Unknown"),
      description = c("Air treatment", "Drone treatment", "Ground treatment", "No treatment planned", "Unknown treatment type"),
      stringsAsFactors = FALSE
    ))
  })
}

#' Get Consistent Colors for Treatment Plan Types
#' 
#' This function generates and returns a consistent color mapping for treatment plan types.
#' Each plan type gets assigned a unique, visually distinct color that remains consistent 
#' across all visualizations.
#' 
#' Usage:
#' ```r
#' # Get colors for treatment plan types
#' plan_colors <- get_treatment_plan_colors()
#' 
#' # In ggplot2 using plan codes (A, D, G, N, U):
#' ggplot(data, aes(x = plan_type, y = acres, fill = airgrnd_plan)) +
#'   scale_fill_manual(values = plan_colors)
#' 
#' # In ggplot2 using plan names (Air, Drone, Ground, None, Unknown):
#' plan_name_colors <- get_treatment_plan_colors(use_names = TRUE)
#' ggplot(data, aes(x = plan_name, y = acres, fill = plan_name)) +
#'   scale_fill_manual(values = plan_name_colors)
#' ```
#' 
#' Parameters:
#'   use_names: If TRUE, returns colors mapped to plan names (Air, Drone, etc.)
#'              If FALSE (default), returns colors mapped to plan codes (A, D, etc.)
#' 
#' Returns:
#'   Named vector where names are either plan codes or plan names, and values are hex colors
get_treatment_plan_colors <- function(use_names = FALSE) {
  plan_types <- get_treatment_plan_types()
  if (nrow(plan_types) == 0) return(c())
  
  # Define specific colors for common treatment plan types for consistency
  predefined_colors <- c(
    "A" = "#E41A1C",    # Red for Air
    "D" = "#377EB8",    # Blue for Drone  
    "G" = "#4DAF4A",    # Green for Ground
    "N" = "#984EA3",    # Purple for None
    "U" = "#FF7F00"     # Orange for Unknown
  )
  
  # Start with predefined colors
  colors <- character(nrow(plan_types))
  names(colors) <- plan_types$plan_code
  
  # Assign predefined colors where available
  for (i in seq_len(nrow(plan_types))) {
    code <- plan_types$plan_code[i]
    if (code %in% names(predefined_colors)) {
      colors[code] <- predefined_colors[code]
    }
  }
  
  # For any codes not in predefined list, generate distinct colors
  missing_codes <- plan_types$plan_code[!plan_types$plan_code %in% names(predefined_colors)]
  if (length(missing_codes) > 0) {
    additional_colors <- generate_distinct_colors(length(missing_codes))
    names(additional_colors) <- missing_codes
    colors[missing_codes] <- additional_colors
  }
  
  # If use_names is TRUE, convert keys from codes to names
  if (use_names) {
    name_map <- setNames(plan_types$plan_name, plan_types$plan_code)
    names(colors) <- name_map[names(colors)]
  }
  
  return(colors)
}

#' Get Treatment Plan Choices for Select Inputs
#' 
#' Returns properly formatted choices for selectInput widgets with full names as labels
#' and plan codes as values for database queries.
#' 
#' Usage:
#' ```r
#' # In UI:
#' checkboxGroupInput(
#'   "plan_types",
#'   "Select Treatment Plan Types:",
#'   choices = get_treatment_plan_choices(),
#'   selected = c("A", "D", "G")
#' )
#' ```
#' 
#' Parameters:
#'   include_all: If TRUE, includes "All Types" option
#' 
#' Returns:
#'   Named vector suitable for selectInput choices
get_treatment_plan_choices <- function(include_all = FALSE) {
  plan_types <- get_treatment_plan_types()
  
  if (nrow(plan_types) == 0) {
    return(c("Air (A)" = "A", "Drone (D)" = "D", "Ground (G)" = "G", "None (N)" = "N", "Unknown (U)" = "U"))
  }
  
  # Create choices with format "Name (Code)" = "Code"
  labels <- paste0(plan_types$plan_name, " (", plan_types$plan_code, ")")
  choices <- setNames(plan_types$plan_code, labels)
  
  if (include_all) {
    choices <- c("All Types" = "all", choices)
  }
  
  return(choices)
}

# =============================================================================
# COLOR HELPER FUNCTIONS - ONE STOP SHOP
# =============================================================================
# Centralized color management with optional zone differentiation support.
# All color functions support optional alpha_zones parameter for P1/P2 zones.

#' Apply Zone Differentiation to ggplot
#' 
#' Helper function to add zone alpha scaling to plots when using zone-aware colors.
#' Use this with the zone-aware results from get_facility_base_colors() or get_foreman_colors().
#' 
#' @param plot ggplot object
#' @param alpha_values Named vector of alpha values (from color function result$alpha_values)
#' @param representative_color Optional hex color for legend display (if NULL, uses gray)
#' @return Modified ggplot object with zone alpha scale and legend
#' @export
add_zone_alpha_to_plot <- function(plot, alpha_values, representative_color = NULL) {
  if (is.null(alpha_values)) {
    return(plot)
  }
  
  # Choose representative fill for legend
  rep_fill <- ifelse(is.null(representative_color), "#808080", representative_color)
  
  plot <- plot +
    ggplot2::scale_alpha_manual(
      name = "Zone",
      values = alpha_values,
      labels = c("1" = "P1 (Solid)", "2" = "P2 (Faded)"),
      drop = FALSE
    ) +
    ggplot2::guides(alpha = ggplot2::guide_legend(
      override.aes = list(
        fill = rep(rep_fill, length.out = length(alpha_values)),
        alpha = unname(alpha_values)
      )
    ))
  
  return(plot)
}
