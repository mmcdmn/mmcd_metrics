# Companion restart listener — runs on port 9001, separate from the main
# Plumber API (port 9000)
#
# POST /restart kills every running Plumber process; api/plumber_watchdog.sh
# starts a fresh one. If the watchdog itself is not running, this starts it.
# returns logs. no auth

find_pids <- function(pattern) {
  pids <- suppressWarnings(as.integer(list.files("/proc")))
  pids <- pids[!is.na(pids)]
  hit <- vapply(pids, function(p) {
    cl <- tryCatch(readBin(sprintf("/proc/%d/cmdline", p), "raw", 65536L),
                   error = function(e) raw(0))
    cl[cl == as.raw(0)] <- as.raw(32)
    grepl(pattern, rawToChar(cl), fixed = TRUE)
  }, logical(1))
  pids[hit]
}

api_responding <- function() {
  system("curl -s -o /dev/null -m 3 http://127.0.0.1:9000/v1/public/health",
         ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
}

#* @post /restart
#* @json
function(req, res) {
  t0 <- Sys.time()

  killed <- find_pids("run_plumber.R")
  for (p in killed) tools::pskill(p, tools::SIGKILL)

  watchdog_running <- length(find_pids("plumber_watchdog.sh")) > 0
  if (!watchdog_running) {
    system("setsid bash /srv/api/plumber_watchdog.sh </dev/null >/dev/null 2>&1 &")
  }
# poll the api response
  api_up <- FALSE
  while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < 90) {
    Sys.sleep(3)
    if (api_responding()) { api_up <- TRUE; break }
  }

  log_file <- "/var/log/plumber-api.log"
  list(
    api_up           = api_up,
    seconds          = round(as.numeric(difftime(Sys.time(), t0, units = "secs"))),
    killed_pids      = killed,
    watchdog_running = watchdog_running,
    log_tail         = if (file.exists(log_file)) tail(readLines(log_file, warn = FALSE), 40) else character(0)
  )
}
