# MMCD Metrics - Performance Optimization Guide
**Date:** January 2026

## Overview

This document describes the performance optimizations implemented to help the MMCD Metrics dashboard handle higher concurrent user loads. The optimizations focus on reducing server-side computation and database load through intelligent caching.

---

## Key Optimization Strategies

### 1. Multi-Layer Caching Architecture

We've implemented a three-tier caching system:

```
┌─────────────────────────────────────────────────────────┐
│                    Request Flow                         │
├─────────────────────────────────────────────────────────┤
│                                                         │
│   User Request                                          │
│        │                                                │
│        ▼                                                │
│   ┌─────────────────────┐                              │
│   │  In-Memory Cache    │  ◄── FASTEST (< 1ms)         │
│   │  (per R process)    │      TTL: 5 minutes          │
│   └─────────┬───────────┘                              │
│             │ miss                                      │
│             ▼                                          │
│   ┌─────────────────────┐                              │
│   │  File-Based Cache   │  ◄── FAST (< 10ms)           │
│   │  (shared/cache/)    │      Persists across restart │
│   └─────────┬───────────┘                              │
│             │ miss                                      │
│             ▼                                          │
│   ┌─────────────────────┐                              │
│   │  Database Pool      │  ◄── Connection reuse        │
│   │  (1-15 connections) │      No connect overhead     │
│   └─────────────────────┘                              │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

### 2. Connection Pooling (Already Implemented)

The `shared/db_pool.R` module maintains persistent database connections:

- **Pool size:** 1-15 connections per app
- **Idle timeout:** 1 hour
- **Auto-reconnect:** Yes
- **Result:** ~80% reduction in connection overhead

### 3. In-Memory Lookup Cache (NEW)

Located in `shared/db_helpers.R`:

```r
# Lookup tables are cached in memory for 5 minutes
# Multiple sessions in the same R process share the cache
get_facility_lookup()  # First call: hits DB
get_facility_lookup()  # Second call: returns cached (< 1ms)
```

**Cached Lookups:**
- `get_facility_lookup()` - Facility names and codes
- `get_foremen_lookup()` - Field supervisors
- `get_species_lookup()` - Mosquito species
- `get_material_choices()` - Treatment materials

### 4. App-Level Caching Module (NEW)

Located in `shared/app_cache.R`:

```r
# In your app.R:
source("../../shared/app_cache.R")

# Create a cached data loader
cached_data <- create_cached_data_loader(
  load_func = function() load_raw_data(Sys.Date()),
  cache_key = "drone_data_today",
  ttl_minutes = 5
)

# Use in server:
server <- function(input, output, session) {
  data <- cached_data()  # Returns cached data if available
}
```

---

## How to Apply Optimizations to an App

### Step 1: Add Cache Module

Add to your `app.R` after the existing sources:

```r
source("../../shared/app_libraries.R")
source("../../shared/db_helpers.R")
source("../../shared/app_cache.R")  # ADD THIS
```

### Step 2: Use Cached Data Loaders

Replace expensive reactive data loading:

**Before (every user hits database):**
```r
server <- function(input, output, session) {
  raw_data <- eventReactive(input$refresh, {
    load_raw_data(input$analysis_date)
  })
}
```

**After (shared cache between sessions):**
```r
server <- function(input, output, session) {
  raw_data <- eventReactive(input$refresh, {
    cache_key <- make_cache_key("drone", input$analysis_date)
    get_cached(cache_key, 
               function() load_raw_data(input$analysis_date),
               ttl_minutes = 5)
  })
}
```

### Step 3: Preload Lookups on Startup

Add to the TOP of your `app.R` (outside server function):

```r
# Preload lookup tables into cache on app startup
# This ensures the first user doesn't wait for DB queries
tryCatch({
  get_facility_lookup()
  get_foremen_lookup()
}, error = function(e) message("Preload failed: ", e$message))
```

---

## Stress Testing

### Running the Enhanced Stress Test

```r
source("stress_test_v2.R")

# Quick test with 10 users for 30 seconds
quick_test(10, 30)

# Full progressive test (5 → 50 users)
results <- run_stress_test()

# Save as baseline for future comparison
results <- run_stress_test(save_baseline = TRUE)

# Test a specific slow endpoint
test_endpoint("SUCO History", num_requests = 50)
```

### Interpreting Results

| Metric | Good | Warning | Critical |
|--------|------|---------|----------|
| Success Rate | > 98% | 95-98% | < 95% |
| Avg Response | < 1000ms | 1000-2000ms | > 2000ms |
| P95 Response | < 3000ms | 3000-5000ms | > 5000ms |
| Overall Score | > 80 | 60-80 | < 60 |

### Baseline Comparison

After making optimizations, run:

```r
# This will automatically compare to the saved baseline
run_stress_test()
```

Results show improvement/regression per endpoint.

---

## Performance Targets

### Current State (Jan 2026 baseline)

| Metric | Value |
|--------|-------|
| Max concurrent users | ~50 |
| Avg response time | 750ms |
| P95 response time | 1900ms |
| Success rate | 98.5% |

### Target State

| Metric | Target |
|--------|--------|
| Max concurrent users | 100+ |
| Avg response time | < 500ms |
| P95 response time | < 1500ms |
| Success rate | > 99% |

---

## Best Practices for High Load

### 1. Minimize Database Queries Per Session

**Bad:**
```r
observeEvent(input$facility, {
  data <- dbGetQuery(conn, query)  # Hits DB on every change
})
```

**Good:**
```r
observeEvent(input$refresh, {  # Only query on explicit refresh
  data <- get_cached("key", function() dbGetQuery(conn, query))
})
```

### 2. Use Reactive Values for Filter State

**Bad:**
```r
# Calling get_foremen_lookup() multiple times
foremen <- get_foremen_lookup()  # In observe
foremen <- get_foremen_lookup()  # In renderUI
foremen <- get_foremen_lookup()  # In another observe
```

**Good:**
```r
# Load once, store in reactive
foremen_lookup <- reactive({
  get_foremen_lookup()  # In-memory cache makes this fast
}) %>% bindCache("foremen_lookup")  # Optional: Shiny cache
```

### 3. Batch Database Operations

**Bad:**
```r
# N+1 query pattern
for (site in sites) {
  data <- dbGetQuery(conn, paste0("SELECT * FROM sites WHERE id = ", site))
}
```

**Good:**
```r
# Single batch query
site_ids <- paste0("'", sites, "'", collapse = ", ")
data <- dbGetQuery(conn, paste0("SELECT * FROM sites WHERE id IN (", site_ids, ")"))
```

### 4. Use Progress Indicators

For long operations, show users progress:

```r
withProgress(message = "Loading data...", {
  setProgress(0.3, detail = "Fetching from database...")
  data <- load_raw_data()
  
  setProgress(0.7, detail = "Processing results...")
  processed <- process_data(data)
})
```

---

## Cache Management

### Clearing Caches

```r
# Clear in-memory lookup cache (all apps)
source("shared/db_helpers.R")
clear_memory_cache()

# Clear file-based lookup cache
clear_lookup_cache()

# Clear app-level cache
source("shared/app_cache.R")
clear_app_cache()

# View cache stats
print_cache_info()
```

### Cache Debugging

Enable debug mode to see cache hits/misses:

```r
# In app_cache.R
CACHE_CONFIG$debug <- TRUE
```

Output:
```
📦 Cache HIT: drone_data_today (accessed 5 times)
🔄 Cache MISS: struct_data (loading...)
💾 Cache SET: struct_data (TTL: 5 min)
```

---

## Monitoring in Production

### Shiny Server Logs

```bash
# View real-time logs
docker logs mmcd-dashboard --follow

# Filter for performance issues
docker logs mmcd-dashboard 2>&1 | grep -E "(ERROR|WARN|timeout)"
```

### Database Connection Monitoring

```sql
-- See active connections from MMCD apps
SELECT application_name, count(*), state
FROM pg_stat_activity 
WHERE application_name LIKE 'mmcd_%'
GROUP BY application_name, state;
```

---

## Troubleshooting

### High Memory Usage

**Symptoms:** Container restarts, slow responses, OOM errors

**Solutions:**
1. Reduce `CACHE_CONFIG$max_entries` in app_cache.R
2. Lower `maxSize` in db_pool.R connection pool
3. Add `app_idle_timeout` to shiny-server.conf

### Slow First Load

**Symptoms:** First user experiences 5-10 second load times

**Solutions:**
1. Add lookup preloading to app.R
2. Implement cache warming via scheduled task
3. Increase historical cache TTL

### Database Connection Exhaustion

**Symptoms:** "Too many clients" errors, timeouts

**Solutions:**
1. Check pool usage: `pool::dbGetInfo(get_pool())`
2. Ensure apps use `get_pool()` not `get_db_connection()`
3. Reduce `maxSize` if hitting AWS RDS limits

---

## Files Reference

| File | Purpose |
|------|---------|
| `shared/db_pool.R` | Database connection pooling |
| `shared/db_helpers.R` | Lookup caching, SQL helpers |
| `shared/app_cache.R` | App-level data caching |
| `shared/cache_utilities.R` | Historical data caching |
| `stress_test_v2.R` | Performance testing |
| `shiny-server.conf` | Server scaling config |

---

## Changelog

### January 2026
- Added in-memory lookup cache (5-min TTL)
- Created app_cache.R module
- Enhanced stress_test_v2.R with baseline comparison
- Updated shiny-server.conf for scaling
- Documentation update
