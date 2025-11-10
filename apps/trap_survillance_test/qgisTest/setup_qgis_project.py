#!/usr/bin/env python3
"""
Setup script to create a QGIS project for trap surveillance data
This should be run inside the Docker container after it starts
"""

import os
from qgis.core import (
    QgsApplication,
    QgsProject,
    QgsDataSourceUri,
    QgsVectorLayer,
    QgsCoordinateReferenceSystem,
    QgsSimpleFillSymbolLayer,
    QgsFillSymbol,
    QgsRendererCategory,
    QgsCategorizedSymbolRenderer
)
from qgis.PyQt.QtGui import QColor

# Initialize QGIS Application
QgsApplication.setPrefixPath("/usr", True)
qgs = QgsApplication([], False)
qgs.initQgis()

try:
    # Create new project
    project = QgsProject.instance()
    project.clear()
    
    # Set project CRS to WGS84
    crs = QgsCoordinateReferenceSystem("EPSG:4326")
    project.setCrs(crs)
    
    # Get database connection parameters from environment variables
    db_host = os.environ.get('DB_HOST', 'localhost')
    db_port = os.environ.get('DB_PORT', '5432')
    db_name = os.environ.get('DB_NAME', 'mosquito_data')
    db_user = os.environ.get('DB_USER', 'postgres')
    db_password = os.environ.get('DB_PASSWORD', '')
    
    # Create connection URI for trap sites using real schema from trap_surveillance_test
    # Query gets the most recent trap inspections with species counts
    uri = QgsDataSourceUri()
    uri.setConnection(db_host, db_port, db_name, db_user, db_password)
    
    # Use a SQL query to get trap data with geometry
    # This matches the real query structure from trap_surveillance_test
    sql_query = """
        WITH ranked_traps AS (
          SELECT t.ainspecnum, t.facility, t.x as lon, t.y as lat, t.survtype, t.inspdate,
                 ROW_NUMBER() OVER (PARTITION BY t.x, t.y, t.facility ORDER BY t.inspdate DESC) as rn
          FROM public.dbadult_insp_current t
          WHERE t.inspdate::date >= (CURRENT_DATE - INTERVAL '90 days')
            AND t.x IS NOT NULL AND t.y IS NOT NULL
            AND t.survtype IN ('4', '5', '6')
        ),
        latest_traps AS (
          SELECT ainspecnum, facility, lon, lat, survtype, inspdate
          FROM ranked_traps
          WHERE rn = 1
        )
        SELECT 
          lt.ainspecnum as id,
          lt.facility,
          lt.survtype as trap_type,
          lt.inspdate::date as inspection_date,
          COALESCE(SUM(s.cnt), 0) as species_count,
          ST_SetSRID(ST_MakePoint(lt.lon, lt.lat), 4326) as geom
        FROM latest_traps lt
        LEFT JOIN public.dbadult_species_current s ON lt.ainspecnum = s.ainspecnum
        GROUP BY lt.ainspecnum, lt.facility, lt.lon, lt.lat, lt.survtype, lt.inspdate
    """
    
    uri.setDataSource("", f"({sql_query})", "geom", "", "id")
    
    # Create vector layer
    trap_layer = QgsVectorLayer(uri.uri(), "trap_sites", "postgres")
    
    if trap_layer.isValid():
        # Create categorized renderer based on trap_type
        categories = []
        
        # Define colors for trap types (matching trap_surveillance_test)
        trap_type_colors = {
            '4': QColor('#E41A1C'),  # Elevated CO2 - Red
            '5': QColor('#377EB8'),  # Gravid Trap - Blue
            '6': QColor('#4DAF4A')   # CO2 Overnight - Green
        }
        
        for trap_type, color in trap_type_colors.items():
            symbol = QgsFillSymbol.createSimple({'color': color.name(), 'outline_color': 'black'})
            category = QgsRendererCategory(trap_type, symbol, f"Trap Type {trap_type}")
            categories.append(category)
        
        renderer = QgsCategorizedSymbolRenderer('trap_type', categories)
        trap_layer.setRenderer(renderer)
        
        # Add layer to project
        project.addMapLayer(trap_layer)
        print(f"✓ Successfully added trap_sites layer with {trap_layer.featureCount()} features")
    else:
        print(f"✗ Failed to create trap_sites layer")
        print(f"Error: {trap_layer.error().message()}")
    
    # Save project
    project_path = "/qgis/projects/trap_surveillance.qgs"
    os.makedirs(os.path.dirname(project_path), exist_ok=True)
    
    if project.write(project_path):
        print(f"✓ Project saved to {project_path}")
    else:
        print(f"✗ Failed to save project to {project_path}")
    
except Exception as e:
    print(f"✗ Error creating QGIS project: {e}")

finally:
    # Clean up
    qgs.exitQgis()
