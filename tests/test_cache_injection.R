#!/usr/bin/env Rscript

# Test script to inject fake stub data into cache and verify apps use it
setwd("/srv/shiny-server/shared")

cat("=== CACHE INJECTION TEST ===\n")
cat("This test injects FAKE data into the lookup cache to prove caching works.\n\n")

# Load the db_helpers
source("db_helpers.R")

# Step 1: Show current real data
cat("1. Current REAL facility data from database:\n")
clear_lookup_cache()  # Clear to force fresh fetch
real_facilities <- get_facility_lookup()
print(real_facilities)

cat("\n2. Current REAL foremen data (first 5):\n")
real_foremen <- get_foremen_lookup()
print(head(real_foremen, 5))

# Step 2: Inject FAKE data into the cache
cat("\n3. Injecting FAKE data into lookup cache...\n")

fake_facilities <- data.frame(
  short_name = c("FAKE1", "FAKE2", "TEST"),
  full_name = c("Fake Facility One", "Fake Facility Two", "Test Facility"),
  stringsAsFactors = FALSE
)

fake_foremen <- data.frame(
  emp_num = c("9999", "8888", "7777"),
  shortname = c("FAKEBOSS", "TESTLEAD", "STUBFOS"),
  facility = c("FAKE1", "FAKE2", "TEST"),
  stringsAsFactors = FALSE
)

# Write directly to cache file
set_cached_lookup("facilities", fake_facilities)
set_cached_lookup("foremen", fake_foremen)

cat("   Fake facilities injected:", nrow(fake_facilities), "rows\n")
cat("   Fake foremen injected:", nrow(fake_foremen), "rows\n")

# Step 3: Verify cache now returns FAKE data
cat("\n4. Verifying cache returns FAKE data:\n")
cached_facilities <- get_facility_lookup()
print(cached_facilities)

cached_foremen <- get_foremen_lookup()
print(cached_foremen)

if (all(cached_facilities$short_name == fake_facilities$short_name)) {
  cat("\n✓ SUCCESS: Cache is returning FAKE facility data!\n")
} else {
  cat("\n✗ FAILURE: Cache is NOT using the injected data\n")
}

if (all(cached_foremen$shortname == fake_foremen$shortname)) {
  cat("✓ SUCCESS: Cache is returning FAKE foremen data!\n")
} else {
  cat("✗ FAILURE: Cache is NOT using the injected foremen data\n")
}

# Step 4: Show cache file info
cat("\n5. Cache file info:\n")
cache_file <- get_lookup_cache_file()
cat("   Path:", cache_file, "\n")
cat("   Size:", file.info(cache_file)$size, "bytes\n")
cat("   Modified:", as.character(file.info(cache_file)$mtime), "\n")

cat("\n=== FAKE DATA IS NOW IN CACHE ===\n")
cat("Go to catch_basin_status app and check if the dropdowns show:\n")
cat("  - FAKE1, FAKE2, TEST (facilities)\n")
cat("  - FAKEBOSS, TESTLEAD, STUBFOS (foremen)\n")
cat("\nTo restore real data, run: docker exec mmcd-test Rscript -e \"source('/srv/shiny-server/shared/db_helpers.R'); clear_lookup_cache()\"\n")