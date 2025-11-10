#!/bin/bash
# Quick build and test script for QGIS integration

echo "Building Docker image with QGIS Server..."
docker build -t mmcd-metrics:qgis-test .

if [ $? -eq 0 ]; then
    echo "✓ Build successful!"
    echo ""
    echo "To run the container:"
    echo "docker run -d -p 3838:3838 -p 80:80 \\"
    echo "  -e DB_HOST=your_db_host \\"
    echo "  -e DB_PORT=5432 \\"
    echo "  -e DB_NAME=your_db_name \\"
    echo "  -e DB_USER=your_db_user \\"
    echo "  -e DB_PASSWORD=your_db_password \\"
    echo "  --name mmcd-qgis-test \\"
    echo "  mmcd-metrics:qgis-test"
    echo ""
    echo "After starting, set up QGIS project:"
    echo "docker exec mmcd-qgis-test python3 /srv/shiny-server/apps/trap_survillance_test/qgisTest/setup_qgis_project.py"
    echo ""
    echo "Access test app at:"
    echo "  http://localhost:3838/trap_survillance_test/qgisTest/"
    echo "  http://localhost/shiny/apps/trap_survillance_test/qgisTest/"
else
    echo "✗ Build failed!"
    exit 1
fi
