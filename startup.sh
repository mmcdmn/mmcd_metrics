#!/bin/bash
# =============================================================================
# MMCD Dashboard - Startup Script
# =============================================================================
# Two modes:
#   DEFAULT (App Runner): Single Shiny Server worker on port 3838
#   ENABLE_NGINX=true (Fargate): Multiple workers behind nginx + embedded Redis
#
# Environment variables:
#   ENABLE_NGINX        – "true" to enable multi-worker nginx mode; unset/false = single worker
#   SHINY_WORKERS       – number of workers when ENABLE_NGINX=true (default: 3)
# =============================================================================

set -e

# --------------- Create .env file from Docker env vars ---------------
echo "# Environment variables for MMCD Dashboard" > /srv/shiny-server/.env
echo "DB_HOST=${DB_HOST}" >> /srv/shiny-server/.env
echo "DB_PORT=${DB_PORT}" >> /srv/shiny-server/.env
echo "DB_USER=${DB_USER}" >> /srv/shiny-server/.env
echo "DB_PASSWORD=${DB_PASSWORD}" >> /srv/shiny-server/.env
echo "DB_NAME=${DB_NAME}" >> /srv/shiny-server/.env

# Write runtime mode so R apps know the actual deployment state
echo "ENABLE_NGINX=${ENABLE_NGINX:-false}" >> /srv/shiny-server/.env
echo "SHINY_WORKERS=${SHINY_WORKERS:-3}" >> /srv/shiny-server/.env

# Detect platform and write to .env
if [ -n "${ECS_TASK_ARN}" ] || [ -n "${ECS_CONTAINER_METADATA_URI}" ] || [ -n "${ECS_CONTAINER_METADATA_URI_V4}" ]; then
    echo "MMCD_PLATFORM=ECS/Fargate" >> /srv/shiny-server/.env
elif [ -n "${AWS_EXECUTION_ENV}" ] && echo "${AWS_EXECUTION_ENV}" | grep -q 'AWS_ECS'; then
    echo "MMCD_PLATFORM=ECS/Fargate" >> /srv/shiny-server/.env
elif echo "$(hostname -f 2>/dev/null)" | grep -q 'apprunner\|awsapprunner'; then
    echo "MMCD_PLATFORM=App Runner" >> /srv/shiny-server/.env
elif echo "$(hostname -f 2>/dev/null)" | grep -q 'compute.internal'; then
    echo "MMCD_PLATFORM=AWS (unknown service)" >> /srv/shiny-server/.env
else
    echo "MMCD_PLATFORM=Local/Docker" >> /srv/shiny-server/.env
fi

chown shiny:shiny /srv/shiny-server/.env
echo "Created .env file with environment variables"

# --------------- Start embedded Redis (shared cache for all workers) ------
echo "Starting embedded Redis server..."
redis-server --daemonize yes \
  --bind 127.0.0.1 \
  --port 6379 \
  --maxmemory 128mb \
  --maxmemory-policy allkeys-lru \
  --loglevel warning \
  --save "" \
  --appendonly no
if redis-cli -h 127.0.0.1 ping > /dev/null 2>&1; then
    echo "  Redis running on 127.0.0.1:6379 (shared by all Shiny workers)"
else
    echo "  WARNING: Redis failed to start — apps will fall back to file cache"
fi

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
# MODE: Multi-worker with nginx (only if ENABLE_NGINX=true)
# =============================================================================
if [ "${ENABLE_NGINX}" = "true" ]; then
    NUM_WORKERS=${SHINY_WORKERS:-3}
    BASE_PORT=3839

    echo "Starting $NUM_WORKERS Shiny Server worker instances..."

    # Generate workers.list for Lua routing init
    > /etc/nginx/workers.list
    for i in $(seq 1 "$NUM_WORKERS"); do
        echo "127.0.0.1:$((BASE_PORT + i - 1))" >> /etc/nginx/workers.list
    done
    echo "  Workers list: /etc/nginx/workers.list"

    for i in $(seq 1 "$NUM_WORKERS"); do
        PORT=$((BASE_PORT + i - 1))
        LOG_DIR="/var/log/shiny-server/instance-${i}"
        CONF="/etc/shiny-server/shiny-server-${i}.conf"

        mkdir -p "$LOG_DIR"
        chown -R shiny:shiny "$LOG_DIR"

        sed "s/listen 3838/listen ${PORT}/" /etc/shiny-server/shiny-server.conf \
            | sed "s|log_dir /var/log/shiny-server;|log_dir ${LOG_DIR};|g" \
            > "$CONF"

        SHINY_INSTANCE_ID="$i" SHINY_INSTANCE_PORT="$PORT" /usr/bin/shiny-server "$CONF" &
        PIDS+=($!)
        echo "  ✓ Instance $i  →  port $PORT  (PID ${PIDS[-1]})"
    done

    # Write worker PID→instance map so R apps can identify their worker
    > /srv/shiny-server/.worker_map
    for i in $(seq 1 "$NUM_WORKERS"); do
        PORT=$((BASE_PORT + i - 1))
        echo "${PIDS[$((i-1))]} $i $PORT" >> /srv/shiny-server/.worker_map
    done
    chown shiny:shiny /srv/shiny-server/.worker_map
    echo "  Worker map: /srv/shiny-server/.worker_map"

    # Wait for all workers to be listening before starting nginx
    echo "Waiting for Shiny workers to be ready..."
    for i in $(seq 1 "$NUM_WORKERS"); do
        PORT=$((BASE_PORT + i - 1))
        RETRIES=0
        while ! bash -c "echo > /dev/tcp/127.0.0.1/${PORT}" 2>/dev/null; do
            RETRIES=$((RETRIES + 1))
            if [ $RETRIES -ge 30 ]; then
                echo "   Worker on port $PORT not ready after 30s — starting nginx anyway"
                break
            fi
            sleep 1
        done
        if [ $RETRIES -lt 30 ]; then
            echo "   Worker on port $PORT is ready"
        fi
    done

    # Pre-create nginx logs readable by shiny user (for R diagnostic panels)
    touch /var/log/nginx/access.log /var/log/nginx/error.log
    chmod 644 /var/log/nginx/access.log /var/log/nginx/error.log

    # Register workers in Redis for dynamic Lua routing
    echo "Registering workers in Redis for dynamic routing..."
    for i in $(seq 1 "$NUM_WORKERS"); do
        WORKER_ADDR="127.0.0.1:$((BASE_PORT + i - 1))"
        redis-cli -h 127.0.0.1 SADD "mmcd:workers" "$WORKER_ADDR" > /dev/null 2>&1
        redis-cli -h 127.0.0.1 SET  "mmcd:load:${WORKER_ADDR}" "0" > /dev/null 2>&1
    done
    # Clear any stale route mappings from previous runs
    for key in $(redis-cli -h 127.0.0.1 KEYS "mmcd:route:*" 2>/dev/null); do
        redis-cli -h 127.0.0.1 DEL "$key" > /dev/null 2>&1
    done
    redis-cli -h 127.0.0.1 DEL "mmcd:route_log" > /dev/null 2>&1
    echo "  Workers registered, load counters initialized"

    # Start OpenResty (load balancer on :3838)
    openresty -c /etc/nginx/nginx.conf -g 'daemon off;' &
    PIDS+=($!)
    echo "   OpenResty load balancer on port 3838 (PID ${PIDS[-1]})"

    # Pre-populate the shared cache in the background
    echo "Starting background cache warm-up..."
        Rscript -e '
      tryCatch({
        source("/srv/shiny-server/shared/cache_utilities.R")
        source("/srv/shiny-server/shared/db_helpers.R")
        regenerate_cache()
        cat("[startup] Cache warm-up complete\n")
      }, error = function(e) {
        cat("[startup] Cache warm-up failed:", e$message, "\n")
      })
        ' > /var/log/cache-warmup.log 2>&1 &
        disown  # Detach so wait -n doesn't track the Rscript process
        echo "   Cache warm-up running in background (check /var/log/cache-warmup.log)"
        echo "   Cache warm-up running in background (check /var/log/cache-warmup.log)"

    echo ""
    echo "============================================================"
    echo "  MMCD Dashboard ready — $NUM_WORKERS concurrent workers"
    echo "  Load balancer: OpenResty + Lua (dynamic routing)"
    echo "  Shared cache: Redis on 127.0.0.1:6379"
    echo "  Route TTL: ${ROUTE_TTL_SECONDS:-600}s"
    echo "============================================================"
    echo ""

    # Wait for any process to exit, then shut everything down
    wait -n 2>/dev/null || wait
    echo "A child process exited unexpectedly — shutting down."
    cleanup
fi

# =============================================================================
# MODE: Single worker (default for App Runner)
# =============================================================================
LISTEN_PORT=${PORT:-3838}

echo ""
echo "============================================================"
echo "  MMCD Dashboard — Single worker mode (App Runner compatible)"
echo "  Listening on port $LISTEN_PORT"
echo "============================================================"
echo ""

# If a PORT env var is provided, patch the config
if [ "$LISTEN_PORT" != "3838" ]; then
    sed -i "s/listen 3838/listen ${LISTEN_PORT}/" /etc/shiny-server/shiny-server.conf
fi

SHINY_INSTANCE_ID="1" SHINY_INSTANCE_PORT="$LISTEN_PORT" exec /usr/bin/shiny-server
cleanup