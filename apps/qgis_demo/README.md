# QGIS Server Integration Demo

This Shiny application demonstrates how to integrate QGIS Server with R Shiny for dynamic, filterable map rendering.

## Overview

This demo shows how to:
- Create dynamic QGIS project files (.qgs) based on user filter selections
- Generate WMS (Web Map Service) URLs for QGIS Server
- Display QGIS-rendered maps in a Shiny application using Leaflet
- Filter PostGIS data through QGIS Server in real-time

## Architecture

```
┌─────────────────┐
│  Shiny UI       │ ← User selects filters (facility, date range)
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Server Logic   │ ← Generates .qgs file with SQL filters
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  QGIS Server    │ ← Renders PostGIS data as WMS tiles
│  (Apache:80)    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Leaflet Map    │ ← Displays WMS layer in browser
│  (Client Side)  │
└─────────────────┘
```

## How It Works

1. **User Interface**: Users select filters in the Shiny sidebar (facility, date range, etc.)

2. **Refresh Map**: When the user clicks "Refresh Map", the server:
   - Builds an SQL WHERE clause based on selected filters
   - Generates a unique QGIS project name
   - Creates a .qgs XML file with PostGIS connection and filters
   - Saves it to `/qgis/projects/`

3. **QGIS Server Rendering**: QGIS Server:
   - Reads the .qgs project file
   - Connects to PostGIS database
   - Applies SQL filters
   - Renders the filtered data as WMS tiles

4. **Map Display**: The Leaflet map:
   - Requests WMS tiles from QGIS Server
   - Displays them as an overlay on the base map
   - Shows interactive markers for filtered locations

## Key Functions

### In `app.R`:

- `generate_wms_url()`: Creates WMS GetMap URLs for QGIS Server
- `create_qgis_project()`: Generates .qgs project XML files
- `filtered_data()`: Queries PostGIS database based on filters

### In `shared/db_helpers.R`:

- `generate_wms_capabilities_url()`: Creates WMS GetCapabilities URLs
- `generate_wms_getmap_url()`: Creates WMS GetMap URLs
- `build_postgis_uri()`: Builds PostGIS connection strings for QGIS
- `create_qgis_project_file()`: Creates .qgs project files
- `get_minnesota_extent()`: Returns Minnesota bounding box
- `validate_qgis_project()`: Checks if project file exists

## Testing the Demo

### First Test: Basic Map Display

1. Navigate to `/qgis_demo` in your browser
2. Click "Refresh Map" without changing any filters
3. You should see:
   - A Leaflet map centered on Minnesota
   - QGIS-rendered facility polygons (if available)
   - Markers for facility locations
   - Value boxes showing record counts

### Second Test: Filter Application

1. Select a specific facility from the dropdown
2. Click "Refresh Map"
3. The map should update to show only the selected facility
4. The data table should show filtered results

### Third Test: Multiple Filters

1. Select a facility
2. Adjust the date range
3. Click "Refresh Map"
4. Verify both filters are applied

## Troubleshooting

### QGIS Server not responding

**Check if QGIS Server is running:**
```bash
docker exec <container_name> ps aux | grep qgis
```

**Check Apache logs:**
```bash
docker exec <container_name> tail -f /var/log/apache2/error.log
```

### WMS tiles not loading

**Test WMS GetCapabilities:**
```bash
curl "http://localhost/qgis/?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetCapabilities&MAP=/qgis/projects/default.qgs"
```

**Check project file exists:**
```bash
docker exec <container_name> ls -la /qgis/projects/
```

### Database connection issues

**Test database connection from R:**
```r
source("../../shared/db_helpers.R")
con <- get_db_connection()
DBI::dbGetQuery(con, "SELECT 1")
```

**Check environment variables:**
```bash
docker exec <container_name> env | grep DB_
```

### Empty maps

**Verify data exists:**
```sql
SELECT COUNT(*) FROM public.gis_facility;
SELECT ST_AsText(geom) FROM public.gis_facility LIMIT 1;
```

**Check geometry column:**
- Ensure the geometry column name matches what's in `create_qgis_project()`
- Default is `geom`

## Production Considerations

### Performance

1. **Caching**: Implement project file caching for common filter combinations
2. **Tile Caching**: Enable QGIS Server tile caching for better performance
3. **Indexes**: Ensure PostGIS tables have spatial indexes
4. **Connection Pooling**: Use database connection pooling

### Security

1. **Credentials**: Never expose database passwords in .qgs files shared publicly
2. **Access Control**: Implement proper authentication for sensitive data
3. **SQL Injection**: Validate and sanitize all user inputs used in SQL queries

### Scalability

1. **Project Management**: Clean up old .qgs files periodically
2. **Load Balancing**: Use multiple QGIS Server instances for high traffic
3. **CDN**: Consider CDN for serving map tiles
4. **Async Rendering**: Implement async map rendering for large datasets

## Next Steps

To extend this demo:

1. **Styling**: Create properly styled QGIS projects in QGIS Desktop, save as .qgs templates
2. **Multiple Layers**: Add multiple PostGIS layers to a single project
3. **Legend**: Add QGIS Server legend images to the UI
4. **Print**: Implement QGIS Server print composer integration
5. **WFS**: Add WFS (Web Feature Service) for editable features
6. **Raster Data**: Include raster layers (aerial imagery, DEMs)

## Resources

- [QGIS Server Documentation](https://docs.qgis.org/latest/en/docs/server_manual/)
- [OGC WMS Specification](https://www.ogc.org/standards/wms)
- [Leaflet WMS Documentation](https://leafletjs.com/reference.html#tilelayer-wms)
- [PostGIS Documentation](https://postgis.net/documentation/)

## Example Use Cases

### Mosquito Surveillance

Filter trap locations by:
- Date range (show only recent traps)
- Species detected
- Trap type
- Facility/zone

### Treatment Progress

Show treatment sites with:
- Treatment status (planned, in-progress, completed)
- Treatment type (air, ground, drone)
- Priority level
- Date filters

### Breeding Site Monitoring

Display breeding sites with:
- Current status
- Priority classification
- Last inspection date
- Treatment history
