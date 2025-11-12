#!/bin/bash

# Check if .env file was copied into container (from build context)
if [ -f /srv/shiny-server/.env ]; then
    echo "Using .env file from build context"
    # Source it and convert DB_* to POSTGRES_* format
    source /srv/shiny-server/.env
    
    # Create new .env with POSTGRES_* variables (what db_helpers.R expects)
    echo "# Environment variables for MMCD Dashboard" > /srv/shiny-server/.env
    echo "POSTGRES_HOST=${DB_HOST:-${POSTGRES_HOST}}" >> /srv/shiny-server/.env
    echo "POSTGRES_PORT=${DB_PORT:-${POSTGRES_PORT:-5432}}" >> /srv/shiny-server/.env
    echo "POSTGRES_USER=${DB_USER:-${POSTGRES_USER}}" >> /srv/shiny-server/.env
    echo "POSTGRES_PASSWORD=${DB_PASSWORD:-${POSTGRES_PASSWORD}}" >> /srv/shiny-server/.env
    echo "POSTGRES_DB=${DB_NAME:-${POSTGRES_DB}}" >> /srv/shiny-server/.env
else
    # Create .env from environment variables (for manual docker run with -e flags)
    echo "# Environment variables for MMCD Dashboard" > /srv/shiny-server/.env
    echo "POSTGRES_HOST=${POSTGRES_HOST:-${DB_HOST}}" >> /srv/shiny-server/.env
    echo "POSTGRES_PORT=${POSTGRES_PORT:-${DB_PORT:-5432}}" >> /srv/shiny-server/.env
    echo "POSTGRES_USER=${POSTGRES_USER:-${DB_USER}}" >> /srv/shiny-server/.env
    echo "POSTGRES_PASSWORD=${POSTGRES_PASSWORD:-${DB_PASSWORD}}" >> /srv/shiny-server/.env
    echo "POSTGRES_DB=${POSTGRES_DB:-${DB_NAME}}" >> /srv/shiny-server/.env
fi

# Set ownership
chown shiny:shiny /srv/shiny-server/.env

echo "Created .env file with environment variables"
cat /srv/shiny-server/.env

# Start Shiny Server
exec /usr/bin/shiny-server