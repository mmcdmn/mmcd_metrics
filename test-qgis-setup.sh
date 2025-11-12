#!/bin/bash
# =============================================================================
# QGIS Server Test Script
# =============================================================================
# This script tests whether QGIS Server is properly configured and accessible
# Run this after starting your Docker container to verify the setup.
# =============================================================================

echo "========================================"
echo "QGIS Server Configuration Test"
echo "========================================"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test 1: Check if QGIS Server binary exists
echo "Test 1: Checking QGIS Server binary..."
if docker exec mmcd_metrics test -f /usr/lib/cgi-bin/qgis_mapserv.fcgi; then
    echo -e "${GREEN}✓ QGIS Server binary found${NC}"
else
    echo -e "${RED}✗ QGIS Server binary not found${NC}"
    echo "  Expected location: /usr/lib/cgi-bin/qgis_mapserv.fcgi"
fi
echo ""

# Test 2: Check if Apache is running
echo "Test 2: Checking Apache service..."
if docker exec mmcd_metrics service apache2 status > /dev/null 2>&1; then
    echo -e "${GREEN}✓ Apache is running${NC}"
else
    echo -e "${RED}✗ Apache is not running${NC}"
    echo "  Try: docker exec mmcd_metrics service apache2 start"
fi
echo ""

# Test 3: Check if QGIS projects directory exists
echo "Test 3: Checking QGIS projects directory..."
if docker exec mmcd_metrics test -d /qgis/projects; then
    echo -e "${GREEN}✓ QGIS projects directory exists${NC}"
    PROJECT_COUNT=$(docker exec mmcd_metrics ls -1 /qgis/projects/*.qgs 2>/dev/null | wc -l)
    echo "  Found $PROJECT_COUNT .qgs project file(s)"
else
    echo -e "${RED}✗ QGIS projects directory not found${NC}"
    echo "  Creating directory..."
    docker exec mmcd_metrics mkdir -p /qgis/projects
fi
echo ""

# Test 4: Test WMS GetCapabilities (requires a project file)
echo "Test 4: Testing WMS GetCapabilities..."
# First create a test project if none exists
docker exec mmcd_metrics bash -c 'if [ ! -f /qgis/projects/test.qgs ]; then cat > /qgis/projects/test.qgs << "EOF"
<?xml version="1.0" encoding="UTF-8"?>
<qgis projectname="test" version="3.28.0">
  <properties>
    <WMSServiceTitle type="QString">Test Project</WMSServiceTitle>
  </properties>
</qgis>
EOF
fi'

# Now test the WMS request
RESPONSE=$(docker exec mmcd_metrics curl -s -o /dev/null -w "%{http_code}" "http://localhost/qgis/?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetCapabilities&MAP=/qgis/projects/test.qgs")

if [ "$RESPONSE" == "200" ]; then
    echo -e "${GREEN}✓ WMS GetCapabilities responded with HTTP 200${NC}"
else
    echo -e "${RED}✗ WMS GetCapabilities failed (HTTP $RESPONSE)${NC}"
    echo "  Try checking Apache error logs:"
    echo "  docker exec mmcd_metrics tail -f /var/log/apache2/error.log"
fi
echo ""

# Test 5: Check database connection from container
echo "Test 5: Testing database connection..."
DB_TEST=$(docker exec mmcd_metrics bash -c 'source /srv/shiny-server/.env 2>/dev/null && if [ -n "$DB_HOST" ]; then echo "ok"; fi')
if [ "$DB_TEST" == "ok" ]; then
    echo -e "${GREEN}✓ Database environment variables are set${NC}"
    docker exec mmcd_metrics bash -c 'source /srv/shiny-server/.env && echo "  DB_HOST: $DB_HOST" && echo "  DB_PORT: $DB_PORT" && echo "  DB_NAME: $DB_NAME"'
else
    echo -e "${YELLOW}⚠ Database environment variables not found in .env${NC}"
    echo "  This is OK if you're using AWS environment variables"
fi
echo ""

# Test 6: Check Shiny Server
echo "Test 6: Checking Shiny Server..."
if docker exec mmcd_metrics pgrep -x shiny-server > /dev/null; then
    echo -e "${GREEN}✓ Shiny Server is running${NC}"
else
    echo -e "${RED}✗ Shiny Server is not running${NC}"
fi
echo ""

# Test 7: Check if QGIS demo app exists
echo "Test 7: Checking QGIS demo app..."
if docker exec mmcd_metrics test -f /srv/shiny-server/apps/qgis_demo/app.R; then
    echo -e "${GREEN}✓ QGIS demo app found${NC}"
    echo "  Access at: http://localhost:3838/qgis_demo/"
else
    echo -e "${RED}✗ QGIS demo app not found${NC}"
fi
echo ""

echo "========================================"
echo "Summary"
echo "========================================"
echo ""
echo "If all tests passed, you can access:"
echo "  - Shiny apps: http://localhost:3838/"
echo "  - QGIS demo: http://localhost:3838/qgis_demo/"
echo "  - QGIS WMS: http://localhost/qgis/"
echo ""
echo "Useful commands:"
echo "  - View Apache logs: docker exec mmcd_metrics tail -f /var/log/apache2/error.log"
echo "  - View Shiny logs: docker exec mmcd_metrics tail -f /var/log/shiny-server/*.log"
echo "  - List QGIS projects: docker exec mmcd_metrics ls -la /qgis/projects/"
echo "  - Restart Apache: docker exec mmcd_metrics service apache2 restart"
echo ""
