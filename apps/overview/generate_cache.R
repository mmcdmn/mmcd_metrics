# Script to generate historical averages cache
# Run this script to pre-calculate and cache historical averages
# This speeds up dashboard loading by avoiding slow historical queries
#
# Usage: 
#   source("generate_cache.R")
#   # or run from R console:
#   # setwd("c:/Users/datatech/Documents/mmcd_metrics/apps/overview")
#   # source("generate_cache.R")

cat("=== Historical Averages Cache Generator ===\n\n")

# Set working directory to overview app
script_dir <- tryCatch({
  dirname(sys.frame(1)$ofile)
}, error = function(e) {
  getwd()  # Use current working directory if sys.frame fails
})

if (!is.null(script_dir) && nzchar(script_dir)) {
  setwd(script_dir)
}

cat("Working directory:", getwd(), "\n\n")

# Source required files
source("../../shared/app_libraries.R")
source("../../shared/db_pool.R")
source("metric_registry.R")

# Now source the cache system (but disable it during generation)
USE_CACHED_AVERAGES <- FALSE  # Disable caching during generation
source("historical_cache.R")

# Source historical functions AFTER disabling cache
source("historical_functions.R")

cat("\n--- Starting cache generation ---\n\n")

# Generate the cache
regenerate_historical_cache(zone_filter = c("1", "2"))

cat("\n--- Cache generation complete ---\n")
cat("\nTo enable caching, set USE_CACHED_AVERAGES <- TRUE in historical_cache.R\n")
cat("To view cache status, run: view_cache_status()\n")
