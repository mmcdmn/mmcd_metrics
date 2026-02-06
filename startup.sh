#!/bin/bash
# =============================================================================
# MMCD Dashboard - Multi-Instance Startup Script
# =============================================================================
# Shiny Server Open Source runs ONE R process per app (single-threaded).
# To achieve true concurrency we spin up multiple Shiny Server instances
# on different internal ports and let nginx load-balance across them.
#
# Set SHINY_WORKERS env var to control the number of instances (default: 3).
# =============================================================================

set -e

# --------------- Configuration ---------------
NUM_WORKERS=${SHINY_WORKERS:-3}
BASE_PORT=3839
PIDS=()

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
cleanup() {
    echo "Shutting down all processes..."
    for pid in "${PIDS[@]}"; do
        kill "$pid" 2>/dev/null || true
    done
    wait
    exit 0
}
trap cleanup SIGTERM SIGINT

# --------------- Generate per-instance configs & start workers ---------------
echo "Starting $NUM_WORKERS Shiny Server worker instances..."

# Build the nginx upstream block dynamically based on NUM_WORKERS
# Write a small include file with the server list
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

    # Generate config: swap the listen port and give each instance its own log dir
    sed "s/listen 3838/listen ${PORT}/" /etc/shiny-server/shiny-server.conf \
        | sed "s|log_dir /var/log/shiny-server;|log_dir ${LOG_DIR};|g" \
        > "$CONF"

    /usr/bin/shiny-server "$CONF" &
    PIDS+=($!)
    echo "  ✓ Instance $i  →  port $PORT  (PID ${PIDS[-1]})"
done

# --------------- Start nginx (load balancer on :3838) ---------------
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