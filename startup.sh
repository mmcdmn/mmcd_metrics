#!/bin/bash
# =============================================================================
# MMCD Dashboard - Smart Startup Script
# =============================================================================
# Two modes controlled by the ENABLE_NGINX environment variable:
#
#   ENABLE_NGINX=true  (local development)
#     → Runs multiple Shiny Server instances behind nginx for true
#       concurrency from a single machine.
#
#   ENABLE_NGINX=false or unset  (production / AWS App Runner)
#     → Runs a single Shiny Server directly on port 3838.
#       App Runner handles concurrency by spinning up multiple
#       container instances, so nginx is unnecessary and actually
#       breaks WebSocket connections (double-proxy issue).
#
# Additional env vars:
#   SHINY_WORKERS  – number of Shiny Server instances (default: 3)
#   PORT           – override the listen port (App Runner sets this)
# =============================================================================

set -e

# --------------- Create .env file from Docker env vars ---------------
echo "# Environment variables for MMCD Dashboard" > /srv/shiny-server/.env
echo "DB_HOST=${DB_HOST}" >> /srv/shiny-server/.env
echo "DB_PORT=${DB_PORT}" >> /srv/shiny-server/.env
echo "DB_USER=${DB_USER}" >> /srv/shiny-server/.env
echo "DB_PASSWORD=${DB_PASSWORD}" >> /srv/shiny-server/.env
echo "DB_NAME=${DB_NAME}" >> /srv/shiny-server/.env

chown shiny:shiny /srv/shiny-server/.env
echo "Created .env file with environment variables"

# --------------- Signal handling for clean shutdown ---------------
PIDS=()
cleanup() {
    echo "Shutting down all processes..."
    for pid in "${PIDS[@]}"; do
        kill "$pid" 2>/dev/null || true
    done
    wait
    exit 0
}
trap cleanup SIGTERM SIGINT

# =============================================================================
# MODE: Production (multiple Shiny instances behind nginx)
# =============================================================================
if [ "${ENABLE_NGINX:-true}" != "false" ]; then
    NUM_WORKERS=${SHINY_WORKERS:-3}
    BASE_PORT=3839
    LISTEN_PORT=${PORT:-3838}

    echo "Starting $NUM_WORKERS Shiny Server worker instances..."

    # Build the nginx upstream block dynamically
    {
        echo "    upstream shiny_workers {"
        echo "        least_conn;"
        echo "        keepalive 32;"  # Connection pooling
        for i in $(seq 1 "$NUM_WORKERS"); do
            echo "        server 127.0.0.1:$((BASE_PORT + i - 1)) max_fails=3 fail_timeout=30s;"
        done
        echo "    }"
    } > /etc/nginx/upstream.conf

    # Update nginx to listen on the correct port (App Runner sets PORT env var)
    sed -i "s/listen 3838/listen ${LISTEN_PORT}/" /etc/nginx/nginx.conf
    
    # Patch the main nginx config to include the generated upstream
    sed -i '/upstream shiny_workers/,/}/c\    include /etc/nginx/upstream.conf;' /etc/nginx/nginx.conf

    for i in $(seq 1 "$NUM_WORKERS"); do
        PORT=$((BASE_PORT + i - 1))
        LOG_DIR="/var/log/shiny-server/instance-${i}"
        CONF="/etc/shiny-server/shiny-server-${i}.conf"

        mkdir -p "$LOG_DIR"
        chown -R shiny:shiny "$LOG_DIR"

        sed "s/listen 3838/listen ${PORT}/" /etc/shiny-server/shiny-server.conf \
            | sed "s|log_dir /var/log/shiny-server;|log_dir ${LOG_DIR};|g" \
            > "$CONF"

        /usr/bin/shiny-server "$CONF" &
        PIDS+=($!)
        echo "  ✓ Instance $i  →  port $PORT  (PID ${PIDS[-1]})"
    done

    # Start nginx (load balancer)
    nginx -g 'daemon off;' &
    PIDS+=($!)
    echo "  ✓ nginx load balancer on port $LISTEN_PORT (PID ${PIDS[-1]})"

    echo ""
    echo "============================================================"
    echo "  MMCD Dashboard ready — $NUM_WORKERS concurrent workers"
    echo "  Each user gets their own R process for true parallelism"
    echo "============================================================"
    echo ""

    # Wait for any process to exit, then shut everything down
    wait -n 2>/dev/null || wait
    echo "A child process exited unexpectedly — shutting down."
    cleanup
    exit 0
fi

# =============================================================================
# MODE: Direct (single Shiny Server, for debugging only)  
# =============================================================================
echo "WARNING: Running in single-threaded mode!"
echo "Users will queue behind each other. Use ENABLE_NGINX=true for concurrency."
    LISTEN_PORT=${PORT:-3838}

    echo ""
    echo "============================================================"
    echo "  MMCD Dashboard — Single-threaded mode (debugging only)"
    echo "  Listening on port $LISTEN_PORT"  
    echo "  WARNING: Users will queue behind each other!"
    echo "============================================================"
    echo ""

    # If App Runner provides a PORT env var, patch the config
    if [ "$LISTEN_PORT" != "3838" ]; then
        sed -i "s/listen 3838/listen ${LISTEN_PORT}/" /etc/shiny-server/shiny-server.conf
    fi

    exec /usr/bin/shiny-server
fi

# =============================================================================
# MODE: Local development (multiple instances behind nginx)
# =============================================================================
NUM_WORKERS=${SHINY_WORKERS:-3}
BASE_PORT=3839

echo "Starting $NUM_WORKERS Shiny Server worker instances..."

# Build the nginx upstream block dynamically
{
    echo "    upstream shiny_workers {"
    echo "        least_conn;"
    for i in $(seq 1 "$NUM_WORKERS"); do
        echo "        server 127.0.0.1:$((BASE_PORT + i - 1));"
    done
    echo "    }"
} > /etc/nginx/upstream.conf

# Patch the main nginx config to include the generated upstream
sed -i '/upstream shiny_workers/,/}/c\    include /etc/nginx/upstream.conf;' /etc/nginx/nginx.conf

for i in $(seq 1 "$NUM_WORKERS"); do
    PORT=$((BASE_PORT + i - 1))
    LOG_DIR="/var/log/shiny-server/instance-${i}"
    CONF="/etc/shiny-server/shiny-server-${i}.conf"

    mkdir -p "$LOG_DIR"
    chown -R shiny:shiny "$LOG_DIR"

    sed "s/listen 3838/listen ${PORT}/" /etc/shiny-server/shiny-server.conf \
        | sed "s|log_dir /var/log/shiny-server;|log_dir ${LOG_DIR};|g" \
        > "$CONF"

    /usr/bin/shiny-server "$CONF" &
    PIDS+=($!)
    echo "  ✓ Instance $i  →  port $PORT  (PID ${PIDS[-1]})"
done

# Start nginx (load balancer on :3838)
nginx -g 'daemon off;' &
PIDS+=($!)
echo "  ✓ nginx load balancer on port 3838 (PID ${PIDS[-1]})"

echo ""
echo "============================================================"
echo "  MMCD Dashboard ready — $NUM_WORKERS concurrent workers"
echo "  Users are load-balanced for true parallel execution"
echo "============================================================"
echo ""

# Wait for any process to exit, then shut everything down
wait -n 2>/dev/null || wait
echo "A child process exited unexpectedly — shutting down."
cleanup