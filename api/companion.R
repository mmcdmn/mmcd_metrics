# Companion restart listener — runs on port 9001, separate from the main
# Plumber API (port 9000). Stays alive independently so the web UI can
# trigger a restart even when the main process is completely dead.
#
# nginx routes: POST /v1/private/restart-companion  →  POST /restart here
# startup.sh writes the main Plumber PID to /var/run/plumber-main.pid
# after each (re)start; this endpoint reads that file and sends SIGTERM.
# The watchdog loop in startup.sh detects the exit and restarts within ~2s.

#* @post /restart
#* @json
function(req, res) {
  auth     <- if (!is.null(req$HTTP_AUTHORIZATION)) req$HTTP_AUTHORIZATION else ""
  api_keys <- Sys.getenv("API_KEYS")
  valid    <- nzchar(api_keys) &&
              any(trimws(strsplit(api_keys, ",")[[1]]) == sub("^Bearer\\s+", "", auth))

  if (!nzchar(auth) || !valid) {
    res$status <- 401L
    return(list(error = "unauthorized"))
  }

  pid_file <- "/var/run/plumber-main.pid"
  signaled <- FALSE
  if (file.exists(pid_file)) {
    pid <- suppressWarnings(as.integer(readLines(pid_file, n = 1L)))
    if (!is.na(pid) && pid > 0L) {
      system2("kill", c("-TERM", as.character(pid)))
      message("[companion] Sent SIGTERM to Plumber PID ", pid)
      signaled <- TRUE
    }
  }

  list(
    status   = "restarting",
    signaled = signaled,
    message  = if (signaled)
      "Plumber process signaled. Watchdog will restart within ~3 seconds."
    else
      "PID file not found — watchdog will restart Plumber if it is not already running."
  )
}
