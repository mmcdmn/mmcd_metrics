#!/bin/bash

# Create .env file from environment variables at container startup
echo "# Environment variables for MMCD Dashboard" > /srv/shiny-server/.env
echo "DB_HOST=${DB_HOST}" >> /srv/shiny-server/.env
echo "DB_PORT=${DB_PORT}" >> /srv/shiny-server/.env
echo "DB_USER=${DB_USER}" >> /srv/shiny-server/.env
echo "DB_PASSWORD=${DB_PASSWORD}" >> /srv/shiny-server/.env
echo "DB_NAME=${DB_NAME}" >> /srv/shiny-server/.env

# Set ownership
chown shiny:shiny /srv/shiny-server/.env

echo "Created .env file with environment variables"
cat /srv/shiny-server/.env

# Start Shiny Server
exec /usr/bin/shiny-server