# =============================================================================
# Plumber API Boot Script
# =============================================================================
# Loads the main plumber.R (existing endpoints) and mounts the data API
# sub-routers from api/routes/*.R under /v1/public/data/...
#
# Called by startup.sh instead of directly plumbing plumber.R.
# =============================================================================

library(plumber)

# Plumb the main API (existing endpoints: health, facilities, foremen, etc.)
pr <- plumber::plumb("/srv/api/plumber.R")

# Mount data API sub-routers
# Each route file is a standalone Plumber router sourcing app data_functions.R
route_dir <- "/srv/api/routes"

mount_route <- function(pr, prefix, file) {
  path <- file.path(route_dir, file)
  if (file.exists(path)) {
    tryCatch({
      sub <- plumber::plumb(path)
      pr$mount(prefix, sub)
      message(sprintf("[api] Mounted %s -> %s", file, prefix))
    }, error = function(e) {
      message(sprintf("[api] SKIP %s (load failed: %s)", file, e$message))
    })
  } else {
    message(sprintf("[api] SKIP %s (not found)", path))
  }
  pr
}

pr <- mount_route(pr, "/v1/public/data/breeding",     "breeding_sites.R")
pr <- mount_route(pr, "/v1/public/data/structures",    "structures.R")
pr <- mount_route(pr, "/v1/public/data/treatments",    "treatments.R")
pr <- mount_route(pr, "/v1/public/data/surveillance",  "surveillance.R")
pr <- mount_route(pr, "/v1/public/data/inspections",   "inspections.R")

message("[api] All data routes mounted")

pr
