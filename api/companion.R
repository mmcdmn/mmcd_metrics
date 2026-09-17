# Companion restart listener — runs on port 9001, separate from the main
# Plumber API (port 9000). Stays alive independently so the web UI can
# trigger a restart even when the main process is completely dead.
# reset api
# nginx routes: POST /v1/private/restart-companion  →  POST /restart here
# startup.sh writes the main Plumber PID to /var/run/plumber-main.pid
# after each (re)start; this endpoint reads that file and sends SIGTERM.
# The watchdog loop in startup.sh detects the exit and restarts within ~2s.

#* @post /restart
#* @json
function(req, res) {
  # No auth — this endpoint is loopback-only (127.0.0.1:9001, never exposed
  # through nginx to the public internet), so network isolation is the guard.
  pid_file <- "/var/run/plumber-main.pid"
  signaled <- FALSE

  if (file.exists(pid_file)) {
    pid <- suppressWarnings(as.integer(readLines(pid_file, n = 1L)))
    if (!is.na(pid) && pid > 0L) {
      # SIGKILL (not SIGTERM) — httpuv catches SIGTERM and may not exit;
      # SIGKILL cannot be caught or ignored by any process.
      system2("kill", c("-9", as.character(pid)))
      message("[companion] Sent SIGKILL to Plumber PID ", pid)
      signaled <- TRUE
    }
  }

  if (!signaled) {
    # PID file missing or stale — find and kill any Rscript running run_plumber.R
    result <- system("pkill -9 -f 'run_plumber.R'", intern = TRUE)
    message("[companion] pkill -9 run_plumber.R — watchdog will restart")
    signaled <- TRUE
  }

  list(
    status   = "restarting",
    signaled = signaled,
    message  = "Plumber process killed. Watchdog will restart within ~3 seconds."
  )
}
