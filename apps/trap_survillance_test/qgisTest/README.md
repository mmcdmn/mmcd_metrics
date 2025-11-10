# QGIS Integration Test App

This test app verifies QGIS Server integration with R Shiny.

## Features

1. **QGIS Server Connection Test**: Verifies that QGIS Server is running and accessible
2. **Database Connection Test**: Verifies PostgreSQL connection for trap data
3. **WMS Layer Loading**: Tests loading QGIS-rendered layers as WMS in Leaflet
4. **Interactive Map**: Displays trap surveillance data via QGIS Server

## Usage

### Inside Docker Container

1. Build and run the Docker container:
   ```bash
   docker build -t mmcd-metrics .
   docker run -p 3838:3838 -p 80:80 \
     -e DB_HOST=your_db_host \
     -e DB_NAME=your_db_name \
     -e DB_USER=your_db_user \
     -e DB_PASSWORD=your_db_password \
     mmcd-metrics
   ```

2. Set up the QGIS project (run once after container starts):
   ```bash
   docker exec -it <container_id> python3 /srv/shiny-server/apps/trap_survillance_test/qgisTest/setup_qgis_project.py
   ```

3. Access the test app:
   - Via Apache proxy: `http://localhost/shiny/apps/trap_survillance_test/qgisTest/`
   - Direct to Shiny: `http://localhost:3838/trap_survillance_test/qgisTest/`

### Test Steps

1. Click **"Test QGIS Server"** to verify QGIS Server is responding
2. Click **"Test Database"** to verify database connectivity
3. Click **"Load WMS Layer"** to render trap sites from QGIS Server
4. View the database results table at the bottom

## Architecture

- **R Shiny**: Frontend UI and application logic
- **QGIS Server**: Renders geospatial data as WMS layers
- **Apache**: Reverse proxy for both QGIS Server and Shiny
- **PostgreSQL**: Database storing trap surveillance data
- **Leaflet**: Interactive map displaying WMS layers

## URLs

- QGIS Server capabilities: `http://localhost/qgis/?SERVICE=WMS&REQUEST=GetCapabilities`
- QGIS WMS endpoint: `http://localhost/qgis/?map=/qgis/projects/trap_surveillance.qgs`
- Shiny app (direct): `http://localhost:3838/trap_survillance_test/qgisTest/`
- Shiny app (proxied): `http://localhost/shiny/apps/trap_survillance_test/qgisTest/`

## Troubleshooting

### QGIS Server not responding
- Check Apache status: `service apache2 status`
- Check QGIS Server logs: `journalctl -u apache2`
- Verify QGIS installation: `qgis_mapserv.fcgi -v`

### WMS Layer not loading
- Verify QGIS project exists: `ls -la /qgis/projects/`
- Check project validity: Run setup script again
- Verify database connection from QGIS Server

### Database connection failed
- Check environment variables are set correctly
- Verify database host is accessible from container
- Test connection: `psql -h $DB_HOST -U $DB_USER -d $DB_NAME`
