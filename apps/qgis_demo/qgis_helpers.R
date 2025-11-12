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
  # Base QGIS Server URL (accessible from Apache proxy)
  base_url <- "http://localhost:8080/qgis/"
  
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
    WIDTH = width,
    HEIGHT = height
  )
  
  # Build query string
  query_string <- paste(
    names(params), 
    sapply(params, utils::URLencode, reserved = TRUE), 
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
    
    # Get database connection parameters for PostGIS layers
    load_env_vars()
    db_host <- Sys.getenv("DB_HOST")
    db_port <- Sys.getenv("DB_PORT", "5432")
    db_user <- Sys.getenv("DB_USER")
    db_password <- Sys.getenv("DB_PASSWORD")
    db_name <- Sys.getenv("DB_NAME")
    
    # Create QGIS project XML
    qgs_xml <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<qgis projectname="Trap Surveillance Heat Map" version="3.28.0-Firenze">
  <homePath path=""/>
  <title>Trap Surveillance Heat Map - %s</title>
  <properties>
    <WMSServiceTitle type="QString">Trap Surveillance Heat Map</WMSServiceTitle>
    <WMSServiceAbstract type="QString">Vector index heat map for mosquito trap surveillance - %s</WMSServiceAbstract>
    <WMSExtent type="QgsRectangle">
      <xmin>-97.5</xmin>
      <ymin>43.0</ymin>
      <xmax>-89.5</xmax>
      <ymax>49.5</ymax>
    </WMSExtent>
  </properties>
  
  <projectlayers>
    <!-- Sections Heat Map Layer (from GPKG) -->
    <maplayer>
      <id>sections_heatmap</id>
      <datasource>%s|layername=sections_heatmap</datasource>
      <keywordList>
        <value>heat map</value>
        <value>vector index</value>
      </keywordList>
      <layername>sections_heatmap</layername>
      <srs>
        <spatialrefsys>
          <proj4>+proj=longlat +datum=WGS84 +no_defs</proj4>
          <srsid>3452</srsid>
          <srid>4326</srid>
          <authid>EPSG:4326</authid>
          <description>WGS 84</description>
        </spatialrefsys>
      </srs>
      <provider encoding="UTF-8">ogr</provider>
      <renderer-v2 type="graduatedSymbol" graduatedMethod="GraduatedColor" attr="vector_index" symbollevels="0">
        <ranges>
          <range symbol="0" lower="0.0" upper="10.0" label="0 - 10" render="true"/>
          <range symbol="1" lower="10.0" upper="25.0" label="10 - 25" render="true"/>
          <range symbol="2" lower="25.0" upper="50.0" label="25 - 50" render="true"/>
          <range symbol="3" lower="50.0" upper="100.0" label="50 - 100" render="true"/>
          <range symbol="4" lower="100.0" upper="999999.0" label="> 100" render="true"/>
        </ranges>
        <symbols>
          <symbol alpha="0.7" type="marker" name="0" clip_to_extent="1">
            <layer class="SimpleMarker">
              <prop k="color" v="255,255,0,180"/>
              <prop k="size" v="3"/>
              <prop k="outline_color" v="0,0,0,255"/>
              <prop k="outline_width" v="0.4"/>
            </layer>
          </symbol>
          <symbol alpha="0.7" type="marker" name="1" clip_to_extent="1">
            <layer class="SimpleMarker">
              <prop k="color" v="255,200,0,180"/>
              <prop k="size" v="5"/>
              <prop k="outline_color" v="0,0,0,255"/>
              <prop k="outline_width" v="0.4"/>
            </layer>
          </symbol>
          <symbol alpha="0.7" type="marker" name="2" clip_to_extent="1">
            <layer class="SimpleMarker">
              <prop k="color" v="255,150,0,180"/>
              <prop k="size" v="7"/>
              <prop k="outline_color" v="0,0,0,255"/>
              <prop k="outline_width" v="0.4"/>
            </layer>
          </symbol>
          <symbol alpha="0.7" type="marker" name="3" clip_to_extent="1">
            <layer class="SimpleMarker">
              <prop k="color" v="255,69,0,180"/>
              <prop k="size" v="9"/>
              <prop k="outline_color" v="0,0,0,255"/>
              <prop k="outline_width" v="0.4"/>
            </layer>
          </symbol>
          <symbol alpha="0.7" type="marker" name="4" clip_to_extent="1">
            <layer class="SimpleMarker">
              <prop k="color" v="255,0,0,180"/>
              <prop k="size" v="12"/>
              <prop k="outline_color" v="0,0,0,255"/>
              <prop k="outline_width" v="0.4"/>
            </layer>
          </symbol>
        </symbols>
      </renderer-v2>
    </maplayer>
    
    <!-- MMCD Sections from GPKG (optional reference layer) -->
    <maplayer>
      <id>mmcd_sections</id>
      <datasource>/srv/shiny-server/shared/gpkg/MMCD_Sections_2025.gpkg</datasource>
      <layername>MMCD Sections 2025</layername>
      <srs>
        <spatialrefsys>
          <proj4>+proj=longlat +datum=WGS84 +no_defs</proj4>
          <srsid>3452</srsid>
          <srid>4326</srid>
          <authid>EPSG:4326</authid>
          <description>WGS 84</description>
        </spatialrefsys>
      </srs>
      <provider encoding="UTF-8">ogr</provider>
      <renderer-v2 type="singleSymbol" symbollevels="0">
        <symbols>
          <symbol alpha="0.3" type="fill" name="0" clip_to_extent="1">
            <layer class="SimpleFill">
              <prop k="color" v="200,200,200,80"/>
              <prop k="outline_color" v="100,100,100,255"/>
              <prop k="outline_width" v="0.26"/>
              <prop k="style" v="no"/>
            </layer>
          </symbol>
        </symbols>
      </renderer-v2>
    </maplayer>
    
    <!-- 7 Counties Reference Layer -->
    <maplayer>
      <id>seven_counties</id>
      <datasource>/srv/shiny-server/shared/gpkg/7counties.gpkg</datasource>
      <layername>7 Counties</layername>
      <srs>
        <spatialrefsys>
          <proj4>+proj=longlat +datum=WGS84 +no_defs</proj4>
          <srsid>3452</srsid>
          <srid>4326</srid>
          <authid>EPSG:4326</authid>
          <description>WGS 84</description>
        </spatialrefsys>
      </srs>
      <provider encoding="UTF-8">ogr</provider>
      <renderer-v2 type="singleSymbol" symbollevels="0">
        <symbols>
          <symbol alpha="0.0" type="fill" name="0" clip_to_extent="1">
            <layer class="SimpleFill">
              <prop k="color" v="0,0,0,0"/>
              <prop k="outline_color" v="80,80,80,255"/>
              <prop k="outline_width" v="0.8"/>
              <prop k="style" v="no"/>
            </layer>
          </symbol>
        </symbols>
      </renderer-v2>
    </maplayer>
    
    <!-- MMCD Facility Areas -->
    <maplayer>
      <id>facility_areas</id>
      <datasource>/srv/shiny-server/shared/gpkg/MMCD_FacilityArea.gpkg</datasource>
      <layername>MMCD Facility Areas</layername>
      <srs>
        <spatialrefsys>
          <proj4>+proj=longlat +datum=WGS84 +no_defs</proj4>
          <srsid>3452</srsid>
          <srid>4326</srid>
          <authid>EPSG:4326</authid>
          <description>WGS 84</description>
        </spatialrefsys>
      </srs>
      <provider encoding="UTF-8">ogr</provider>
      <renderer-v2 type="singleSymbol" symbollevels="0">
        <symbols>
          <symbol alpha="0.0" type="fill" name="0" clip_to_extent="1">
            <layer class="SimpleFill">
              <prop k="color" v="0,0,0,0"/>
              <prop k="outline_color" v="60,60,200,255"/>
              <prop k="outline_width" v="0.6"/>
              <prop k="style" v="no"/>
            </layer>
          </symbol>
        </symbols>
      </renderer-v2>
    </maplayer>
  </projectlayers>
  
  <layerorder>
    <layer id="sections_heatmap"/>
    <layer id="mmcd_sections"/>
    <layer id="facility_areas"/>
    <layer id="seven_counties"/>
  </layerorder>
  
  <legend updateDrawingOrder="true">
    <legendlayer drawingOrder="-1" open="true" checked="Qt::Checked" name="sections_heatmap" showFeatureCount="0">
      <filegroup open="true" hidden="false">
        <legendlayerfile isInOverview="0" layerid="sections_heatmap" visible="1"/>
      </filegroup>
    </legendlayer>
    <legendlayer drawingOrder="-1" open="false" checked="Qt::Unchecked" name="mmcd_sections" showFeatureCount="0">
      <filegroup open="false" hidden="false">
        <legendlayerfile isInOverview="0" layerid="mmcd_sections" visible="0"/>
      </filegroup>
    </legendlayer>
    <legendlayer drawingOrder="-1" open="false" checked="Qt::Checked" name="facility_areas" showFeatureCount="0">
      <filegroup open="false" hidden="false">
        <legendlayerfile isInOverview="0" layerid="facility_areas" visible="1"/>
      </filegroup>
    </legendlayer>
    <legendlayer drawingOrder="-1" open="false" checked="Qt::Checked" name="seven_counties" showFeatureCount="0">
      <filegroup open="false" hidden="false">
        <legendlayerfile isInOverview="0" layerid="seven_counties" visible="1"/>
      </filegroup>
    </legendlayer>
  </legend>
  
  <mapcanvas annotationsVisible="1" name="theMapCanvas">
    <units>degrees</units>
    <extent>
      <xmin>-97.5</xmin>
      <ymin>43.0</ymin>
      <xmax>-89.5</xmax>
      <ymax>49.5</ymax>
    </extent>
    <rotation>0</rotation>
    <destinationsrs>
      <spatialrefsys>
        <proj4>+proj=longlat +datum=WGS84 +no_defs</proj4>
        <srsid>3452</srsid>
        <srid>4326</srid>
        <authid>EPSG:4326</authid>
        <description>WGS 84</description>
      </spatialrefsys>
    </destinationsrs>
  </mapcanvas>
</qgis>', analysis_date, species_label, gpkg_path)
    
    # Write QGS file
    writeLines(qgs_xml, project_path)
    message("Created QGIS project at: ", project_path)
    
    return(project_name)
    
  }, error = function(e) {
    message("Error creating QGIS project: ", e$message)
    return(NULL)
  })
}
