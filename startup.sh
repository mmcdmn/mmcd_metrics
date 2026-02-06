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
    # Detect App Runner - optimize nginx for proxy chain instead of disabling
    if [ -n "${PORT}" ] || [ -n "${AWS_REGION}" ] || [ -n "${_HANDLER}" ]; then
        echo "🔧 App Runner detected - configuring nginx for proxy chain compatibility"
        echo "Optimizing WebSocket and timeout settings..."
        APP_RUNNER_MODE=true
    else
        APP_RUNNER_MODE=false
    fi
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
    
    # App Runner specific optimizations
    if [ "$APP_RUNNER_MODE" = "true" ]; then
        echo "Applying App Runner nginx optimizations..."
        
        # Add real IP detection for App Runner's load balancer
        sed -i '/server_name _;/a\        # App Runner real IP detection\
        real_ip_header X-Forwarded-For;\
        set_real_ip_from 10.0.0.0/8;\
        set_real_ip_from 172.16.0.0/12;\
        set_real_ip_from 192.168.0.0/16;\
        real_ip_recursive on;' /etc/nginx/nginx.conf
        
        # Increase timeouts for proxy chain
        sed -i 's/proxy_read_timeout 86400s/proxy_read_timeout 7200s/' /etc/nginx/nginx.conf
        sed -i 's/proxy_connect_timeout 60s/proxy_connect_timeout 300s/' /etc/nginx/nginx.conf
        sed -i 's/client_body_timeout 60s/client_body_timeout 300s/' /etc/nginx/nginx.conf
        sed -i 's/client_header_timeout 60s/client_header_timeout 300s/' /etc/nginx/nginx.conf
        
        # Add additional WebSocket headers for proxy chain
        sed -i '/proxy_set_header X-Forwarded-Proto/a\            proxy_set_header X-Forwarded-Host $host;\
            proxy_set_header X-Forwarded-Port $server_port;\
            proxy_ignore_client_abort on;' /etc/nginx/nginx.conf
        
        # Force HTTP/1.1 and disable proxy caching completely
        sed -i '/proxy_buffering off;/a\            proxy_cache off;\
            proxy_store off;' /etc/nginx/nginx.conf
        
        # Disable proxy buffering completely for streaming
        sed -i 's/proxy_buffering off;/proxy_buffering off;\
            proxy_buffer_size 4k;\
            proxy_buffers 8 4k;\
            proxy_busy_buffers_size 8k;/' /etc/nginx/nginx.conf
    fi
    
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
    if [ "$APP_RUNNER_MODE" = "true" ]; then
        echo "  MMCD Dashboard ready — $NUM_WORKERS concurrent workers (App Runner optimized)"
        echo "  WebSocket proxy chain configured for App Runner compatibility"
    else
        echo "  MMCD Dashboard ready — $NUM_WORKERS concurrent workers"
        echo "  Each user gets their own R process for true parallelism"
    fi
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