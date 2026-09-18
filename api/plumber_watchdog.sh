#!/bin/bash
# Keeps the Plumber API running: restarts it whenever the process exits, and
# kills it (so it gets restarted) when it is alive but stops answering HTTP.
set +e

PORT="${PLUMBER_PORT:-9000}"
LOG=/var/log/plumber-api.log
PID_FILE=/var/run/plumber-main.pid
HEALTH_URL="http://127.0.0.1:${PORT}/v1/public/health"
STARTUP_GRACE=120   # seconds a fresh process gets to load before health checks count
HEALTH_EVERY=20     # seconds between health checks
MAX_FAILS=3         # consecutive failed checks (15s timeout each) before killing

log() { echo "[watchdog $(date '+%F %T')] $*" >> "$LOG"; }

while true; do
  Rscript -e '
    pr <- source("/srv/api/run_plumber.R")$value
    pr$run(host="127.0.0.1", port='"${PORT}"', swagger=FALSE)
  ' >> "$LOG" 2>&1 &
  PID=$!
  echo "$PID" > "$PID_FILE"
  log "Plumber started (PID $PID)"

  started=$(date +%s)
  last_check=$started
  fails=0
  while kill -0 "$PID" 2>/dev/null; do
    sleep 2
    now=$(date +%s)
    [ $((now - started)) -lt "$STARTUP_GRACE" ] && continue
    [ $((now - last_check)) -lt "$HEALTH_EVERY" ] && continue
    last_check=$now
    if curl -s -o /dev/null -m 15 "$HEALTH_URL"; then
      fails=0
    else
      fails=$((fails + 1))
      log "health check failed ($fails/$MAX_FAILS)"
      if [ "$fails" -ge "$MAX_FAILS" ]; then
        log "Plumber (PID $PID) not responding — killing it"
        kill -9 "$PID" 2>/dev/null
        break
      fi
    fi
  done

  wait "$PID"
  status=$?
  log "Plumber (PID $PID) exited with status $status — restarting in 2s"
  sleep 2
done
