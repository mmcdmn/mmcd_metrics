# =============================================================================
# QGIS Server Helper Functions for Heat Map Generation
# =============================================================================

library(sf)

#' Generate WMS URL for QGIS Server
#' 
#' Creates a WMS GetMap URL for embedding QGIS maps in the Shiny app
#' 
#' @param project_name Name of the QGIS project file (without .qgs extension)
#' @param layers Comma-separated list of layer names to display
#' @param bbox Bounding box as c(xmin, ymin, xmax, ymax)
#' @param width Image width in pixels
#' @param height Image height in pixels
#' @param crs Coordinate reference system (default: EPSG:4326)
#' @return Complete WMS GetMap URL
generate_wms_url <- function(project_name, layers, bbox = NULL, 
                            width = 800, height = 600, 
                            crs = "EPSG:4326") {
    # Base QGIS Server URL (accessible from within container)
  base_url <- "http://127.0.0.1/qgis/"
  
  # Default bbox for Minnesota if not specified
  if (is.null(bbox)) {
    bbox <- c(-97.5, 43.0, -89.5, 49.5)
  }
  
  bbox_str <- paste(bbox, collapse = ",")
  
  # Build WMS GetMap request
  params <- list(
    SERVICE = "WMS",
    VERSION = "1.3.0",
    REQUEST = "GetMap",
    MAP = paste0("/qgis/projects/", project_name, ".qgs"),
    LAYERS = layers,
    STYLES = "",
    FORMAT = "image/png",
    TRANSPARENT = "TRUE",
    CRS = crs,
    BBOX = bbox_str,
    WIDTH = as.character(width),
    HEIGHT = as.character(height)
  )
  
  # Build query string - ensure all params are character
  query_string <- paste(
    names(params), 
    sapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE)), 
    sep = "=", 
    collapse = "&"
  )
  
  return(paste0(base_url, "?", query_string))
}

#' Generate QGIS Project for Heat Map Visualization
#' 
#' Creates a QGIS project file (.qgs) with heat map styling for trap surveillance data
#' The project includes both GPKG reference layers and dynamically generated data
#' 
#' @param sections_data Data frame with section vector index results
#' @param traps_data Data frame with trap location and count data
#' @param analysis_date Date string for the analysis
#' @param species_label Label describing which species are included
#' @return Project name (without .qgs extension) or NULL if failed
generate_qgis_heatmap_project <- function(sections_data, traps_data, 
                                          analysis_date = as.character(Sys.Date()),
                                          species_label = "All Species") {
  
  tryCatch({
    # Generate unique project name
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    project_name <- paste0("trap_heatmap_", timestamp)
    project_path <- file.path("/qgis/projects", paste0(project_name, ".qgs"))
    
    # Create temporary GPKG file for the sections heat map data
    gpkg_path <- file.path("/qgis/projects", paste0(project_name, "_data.gpkg"))
    
    # Convert sections data to sf object with points
    sections_sf <- st_as_sf(sections_data, 
                           coords = c("lon", "lat"), 
                           crs = 4326, 
                           remove = FALSE)
    
    # Write to GPKG
    st_write(sections_sf, gpkg_path, layer = "sections_heatmap", 
             driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    
    message("Created GPKG at: ", gpkg_path)
    
  # Read the template file
  template_path <- file.path("apps", "qgis_demo", "minimal_template.xml")
  
  if (!file.exists(template_path)) {
    stop("Template file not found: ", template_path)
  }
  
  qgs_xml <- readLines(template_path, warn = FALSE)
    qgs_xml <- paste(qgs_xml, collapse = "\n")
    
    # Replace template placeholders
    qgs_xml <- gsub("{{ANALYSIS_DATE}}", analysis_date, qgs_xml, fixed = TRUE)
    qgs_xml <- gsub("{{SPECIES_LABEL}}", species_label, qgs_xml, fixed = TRUE)
    qgs_xml <- gsub("{{GPKG_PATH}}", gpkg_path, qgs_xml, fixed = TRUE)
    
    # Write QGS file
    writeLines(qgs_xml, project_path)
    message("Created QGIS project at: ", project_path)
    
    return(project_name)
    
  }, error = function(e) {
    message("Error creating QGIS project: ", e$message)
    return(NULL)
  })
}

#' Generate Advanced QGIS Project with Professional Cartography
#' 
#' Creates a QGIS project with true heat map interpolation, advanced styling,
#' and cartographic features that Leaflet cannot replicate
#' 
#' @param sections_data Data frame with section vector index results  
#' @param traps_data Data frame with trap location and count data
#' @param analysis_date Date string for the analysis
#' @param species_label Label describing which species are included
#' @return Project name (without .qgs extension) or NULL if failed
generate_advanced_qgis_project <- function(sections_data, traps_data, 
                                          analysis_date = as.character(Sys.Date()),
                                          species_label = "All Species") {
  
  tryCatch({
    # Generate unique project name
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    project_name <- paste0("advanced_heatmap_", timestamp)
    project_path <- file.path("/qgis/projects", paste0(project_name, ".qgs"))
    
    # Create GPKG with enhanced data
    gpkg_path <- file.path("/qgis/projects", paste0(project_name, "_data.gpkg"))
    
    # Debug and convert to sf with error handling
    cat("Converting sections to sf. Class:", class(sections_data), "\n")
    cat("Columns:", paste(colnames(sections_data), collapse=", "), "\n")
    cat("Rows:", nrow(sections_data), "\n")
    
    # Ensure sections_data is a data.frame and has required columns
    if (!is.data.frame(sections_data)) {
      stop("sections_data must be a data.frame, got: ", class(sections_data))
    }
    
    if (!all(c("lon", "lat", "vector_index") %in% colnames(sections_data))) {
      stop("sections_data missing required columns: ", 
           paste(setdiff(c("lon", "lat", "vector_index"), colnames(sections_data)), collapse=", "))
    }
    
    # Remove any rows with missing coordinates
    sections_clean <- sections_data[!is.na(sections_data$lon) & !is.na(sections_data$lat), ]
    cat("Clean sections rows:", nrow(sections_clean), "\n")
    
    # Convert to sf with proper error handling
    sections_sf <- st_as_sf(sections_clean, 
                           coords = c("lon", "lat"), 
                           crs = 4326, 
                           remove = FALSE)
    
    # Add risk classification for advanced symbology
    sections_sf$risk_category <- cut(
      sections_sf$vector_index,
      breaks = c(0, 10, 25, 50, Inf),
      labels = c("Low", "Medium", "High", "Critical"),
      include.lowest = TRUE
    )
    
    # Write enhanced data
    st_write(sections_sf, gpkg_path, layer = "sections_heatmap", 
             driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    
    # Read working template that actually functions
    template_path <- file.path("/srv/shiny-server/apps/qgis_demo", "simple_template.xml")
    if (!file.exists(template_path)) {
      message("Simple template not found, using fallback")
      return(generate_qgis_heatmap_project(sections_data, traps_data, analysis_date, species_label))
    }
    
    qgs_xml <- readLines(template_path, warn = FALSE)
    qgs_xml <- paste(qgs_xml, collapse = "\n")
    
    # Replace placeholders
    qgs_xml <- gsub("{{ANALYSIS_DATE}}", analysis_date, qgs_xml, fixed = TRUE)
    qgs_xml <- gsub("{{SPECIES_LABEL}}", species_label, qgs_xml, fixed = TRUE)
    qgs_xml <- gsub("{{GPKG_PATH}}", gpkg_path, qgs_xml, fixed = TRUE)
    
    # Write project file
    writeLines(qgs_xml, project_path)
    message("Created advanced QGIS project at: ", project_path)
    
    return(project_name)
    
  }, error = function(e) {
    message("Error creating advanced QGIS project: ", e$message)
    return(NULL)
  })
}