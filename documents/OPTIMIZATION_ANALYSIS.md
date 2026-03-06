# MMCD Metrics — Performance Optimization Plan
## Updated: March 4, 2026

## Executive Summary

The overview dashboards (FOS, facility, district) have identifiable performance
bottlenecks in three areas: **chart rendering** (ggplotly conversion overhead),
**historical computation** (O(years × 52 × N) nested loops), and **payload
size** (~50KB inline CSS/JS shipped uncompressed on every page load). The FOS
view amplifies the historical loop problem by calling
`get_historical_week_avg_by_entity()` up to 120 times (20 FOS × 6 metrics),
each re-running the year/week loop even though the raw 10-year data is already
cached in memory.

R is single-threaded, so metric loading cannot be parallelized within a Shiny
session. Instead, this plan focuses on making each operation faster, eliminating
redundant work, and reducing what the browser must download.

---

## Architecture Context

| Component | Detail |
|-----------|--------|
| **Workers** | 3 Shiny processes on ports 3839-3841 behind OpenResty |
| **Routing** | Lua-based load-aware sticky routing (Redis + shared dict, 10-min TTL) |
| **Caching** | Redis (shared, 128MB) + in-memory R environments (per-worker) |
| **Charts** | Pre-rendered with `suspendWhenHidden = FALSE` for instant display on click |
| **Docker** | Single container: Redis + 3 Shiny workers + OpenResty |

All optimizations must be compatible with this multi-worker, sticky-session
architecture. Changes to Redis are visible to all workers; in-memory changes
are per-worker only.

---

## Priority 0 — Quick Wins

### 0.1 Add gzip Compression to nginx 

**Problem:** The initial HTML response includes ~1,078 lines of inline CSS/JS
(~50KB uncompressed). JSON responses from Shiny and Plotly payloads are also
sent uncompressed. No `gzip` directive exists in `nginx.conf`.

**Fix:** Add gzip configuration in the `http {}` block of `nginx.conf`:

```nginx
gzip on;
gzip_vary on;
gzip_proxied any;
gzip_comp_level 4;
gzip_min_length 1024;
gzip_types text/plain text/css application/javascript application/json
           text/xml application/xml application/xml+rss text/javascript;
```

**Impact:** ~70% reduction in transfer size for HTML, CSS, JS, and JSON
responses. Directly reduces page load time, especially on slower connections.
No interaction with load balancing — gzip is applied at the nginx layer before
the response reaches the client, regardless of which worker served it.

**Effort:** 5 minutes. **Risk:** None.

### 0.2 Migrate `create_overview_chart()` from ggplotly to native plot_ly 

**Problem:** `ggplotly()` converts a ggplot2 object to Plotly by serializing
the entire ggplot structure, then re-parsing it — this costs 100-500ms per
chart. With `suspendWhenHidden = FALSE` pre-rendering all 11 metric charts,
this adds 1-5 seconds to every page load.

**Current code** (`display_functions.R` L80-196): Uses `ggplot()` +
`geom_bar(stat="identity")` + `coord_flip()` + `theme_minimal()` to build
horizontal bar charts, then `do.call(ggplotly, ...)` to convert. The features
used are simple: layered horizontal bars with custom fill colors, tooltip text
via `aes(text=...)`, click events via `aes(key=...)`, and basic `theme()`
adjustments. No facets, no coord_polar, no custom geoms.

**Fix:** Replace with native `plot_ly(type = "bar", orientation = "h")`.
The same file already has 4 working native `plot_ly()` functions to follow as
templates: `create_overview_pie_chart()` (L279), `create_comparison_chart()`
(L430), `create_yearly_grouped_chart()` (L616). Also convert
`create_empty_chart()` (L363-375) — trivial replacement.

**What breaks:** Nothing functionally. The layered bar approach translates
directly to multiple `add_trace()` calls. Tooltips use `hovertext` instead of
`aes(text=...)`. Click events use `event_register("plotly_click")` + `key`
parameter — identical pattern. Visual styling may need minor tweaking to match
the ggplot theme.

**Impact:** 100-500ms savings per chart × 11 metrics = **1-5s faster page load**.

**Effort:** 2-3 hours. **Risk:** Low — visual regression in bar chart styling
(verify side-by-side).

---

## Priority 1 — High Impact

### 1.1 Vectorize Active-on-Friday Nested Loops 

**Problem:** Three functions share the same O(years × 52 × N) nested loop
pattern that iterates through every week of every year, calling
`dplyr::filter()` on the full treatments table each iteration:

1. `calculate_historical_weekly()` — `historical_functions.R` L634-700 — full
   nested `for yr` × `for week_start` (10 years × 52 weeks = 520 iterations)
2. `.load_current_year_for_cache_uncached()` — `historical_functions.R` L149-170
   — single-year version (~52 iterations)
3. `get_historical_week_avg_by_entity()` — `dynamic_server.R` L495-527 —
   per-entity version, skips non-matching weeks but still loops through all
   520 to find the target week

**The active-on-Friday logic:** For each week, find all treatments where
`inspdate <= friday AND treatment_end >= friday`, dedup by `treatment_id`
(keep longest `treatment_end`), sum values. This is a classic interval overlap
problem.

**Fix (vectorized approach using dplyr — no new dependencies):**

```r
# Pre-compute all Fridays for the date range
fridays <- data.frame(
  friday = seq.Date(as.Date(paste0(start_year, "-01-01")),
                    as.Date(paste0(end_year, "-12-31")), by = "week") + 4
) %>%
  mutate(year = year(friday), week_num = week(friday)) %>%
  filter(year(friday) == year)  # remove year-boundary spills

# Cross-join: every treatment × every Friday where it's active
active <- inner_join(
  treatments %>% select(treatment_id, inspdate, treatment_end, value),
  fridays,
  by = character(),  # cross join
  relationship = "many-to-many"
) %>%
  filter(inspdate <= friday, treatment_end >= friday) %>%
  group_by(treatment_id, year, week_num) %>%
  slice_max(treatment_end, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(year, week_num) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
```

This replaces 520 sequential `dplyr::filter()` calls with a single vectorized
join + group_by.

**Impact:** Significant on cache miss — especially for FOS view where
`get_historical_week_avg_by_entity()` is called 120+ times.

**Effort:** 3-4 hours. **Risk:** Medium — must preserve edge cases.

### 1.2 Move Inline CSS/JS to External Browser-Cacheable Files 

**Problem:** `get_overview_css()` (~565 lines, `dynamic_ui.R` L104-669) and
`get_overview_js()` (~513 lines, L674-1187) embed all styling and behavior as
inline `tags$style(HTML(...))` and `tags$script(HTML(...))`. This ~50KB payload
is sent in the HTML on **every page load** and cannot be browser-cached.

**Fix:** Extract to `apps/overview/www/overview.css` and
`apps/overview/www/overview.js`. Update the R functions to return external
references:

```r
get_overview_css <- function() {
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "overview.css"))
}

get_overview_js <- function() {
  tags$script(src = "overview.js")
}
```

Shiny automatically serves files from each app's `www/` subdirectory — this
already works for the 7 image assets in `apps/overview/www/assets/`.

**Load balancing compatibility:** Works perfectly because:
- Sticky routing ensures all requests from a session go to the same worker
- Every worker has identical file copies (baked into the Docker image)
- Even without sticky sessions, files are identical across workers
- Browser caches these files after first load → 0 bytes on subsequent loads

**Impact:** After first visit, saves ~50KB (or ~15KB with gzip) on every
subsequent page load.

**Effort:** 1-2 hours. **Risk:** Low.

---

## Priority 2 — Medium Impact

### 2.1 Batch FOS/Facility Historical Averages in a Single Pass

**Problem:** The FOS view calls `get_dynamic_value_box_info()` inside nested
loops: `lapply(all_fos, ...)` × `for (metric_id in metrics)` = ~120 calls
(`dynamic_server.R` L1161-1256). Each call chains through
`get_historical_week_avg_by_fos()` → `get_historical_week_avg_by_entity()`
(L441-560), which filters the raw 10-year data to one entity and runs the
year/week loop. The facility view has a similar pattern with ~42 calls.

**Fix:** Add a batch function `get_all_entity_week_averages()` that:
1. Loads the raw data once via `get_cached_raw_historical()`
2. For the target `week_num`, computes active-on-Friday for ALL entities in
   a single vectorized pass (group_by entity + year, summarise)
3. Stores the full entity→avg map in Redis with key
   `hist_batch:{metric_id}:w{week_num}` and 7-day TTL
4. Returns the full named list

**Impact:** Reduces ~120 separate loop computations to 1 vectorized computation.

**Effort:** 3-4 hours. **Risk:** Low — adds alongside existing per-entity path.

### 2.2 Async Cache Regeneration with Scheduled Refresh

**Problem:** When the historical average cache is >14 days stale,
`.trigger_background_repopulate()` runs `regenerate_historical_cache()`
**synchronously** despite its name, blocking the first user request
(`historical_cache.R` L124-138).

**Fix (two parts):**

**Part A — Async regeneration:** Add `callr` as a dependency. Replace the
synchronous call with `callr::r_bg()` to run in a separate R process.
Return stale data immediately while the background process refreshes Redis.

**Part B — Scheduled refresh:** Add a cron-like loop in `startup.sh` that
runs regeneration every 12 hours:
```bash
(while true; do sleep 43200; Rscript -e '
  source("historical_cache.R"); regenerate_historical_cache()
' >> /var/log/cache-warmup.log 2>&1; done) &
disown
```
Add a Redis lock key (`mmcd:regen_lock` with 30-min expiry) so only one
process regenerates at a time.

**Impact:** Eliminates the "first request after cache expiry" freeze.

**Effort:** 2-3 hours. **Risk:** Low.

---

## Priority 3 — Nice to Have

### 3.1 Limit Plotly Resize to Visible Charts

**Problem:** `dynamic_ui.R` ~L865 calls Plotly resize on ALL charts including
hidden pre-rendered ones.

**Fix:** Change `$('.plotly').each(...)` to `$('.plotly:visible').each(...)`.

**Effort:** 1 minute. **Risk:** None.

### 3.2 Hoist Lookup Calls Above Metric Loop

**Problem:** `get_foremen_lookup()` and `get_facility_lookup()` called inside
the per-metric uncached path in `data_functions.R`.

**Fix:** Call once in parent function, pass as parameters.

**Effort:** 30 minutes. **Risk:** None.

---

## Deferred / Not Implementing

| Item | Reason |
|------|--------|
| **Parallelize metric loading** | R is single-threaded; `future` adds complexity for marginal gain since workers already provide process-level parallelism |
| **Remove `suspendWhenHidden = FALSE`** | Pre-rendering is intentional — charts must be ready when user clicks value boxes |
| **Replace Redis `KEYS` with `SCAN`** | `KEYS` is only used in admin/diagnostic paths, not hot paths; current usage is acceptable |
| **Change TTL values** | Current TTLs are appropriate for the data refresh patterns |
| **Reduce Lua retry count** | Current 10-retry with 1s delay works well for startup scenarios |

---

## Implementation Order

Given a time constraint, implement in this order for maximum impact:

1. **gzip** (5 min) — immediate transfer size reduction
2. **ggplotly → plot_ly migration** (2-3 hrs) — 1-5s page load improvement
3. **Externalize CSS/JS** (1-2 hrs) — browser caching for repeat visits
4. **Vectorize loops** (3-4 hrs) — faster historical calculations
5. **Batch entity averages** (3-4 hrs) — FOS view N+1 elimination
6. **Async cache regen** (2-3 hrs) — eliminates first-request freeze

Total estimated effort: ~13-18 hours across all priorities.

---

## Verification Plan

| Change | Verification |
|--------|-------------|
| gzip | `curl -H "Accept-Encoding: gzip" -sI http://localhost:3838/ \| grep Content-Encoding` → `gzip` |
| plot_ly migration | Visual comparison of bar charts; click-to-drill-down works; tooltips match |
| External CSS/JS | Browser DevTools → Network tab shows `overview.css`/`overview.js` as separate cached requests (304 on reload) |
| Vectorized loops | `Rscript tests/testthat.R` — existing historical tests pass |
| Batch averages | FOS view load time before/after with browser DevTools |
| Async regen | First request after cache expiry returns immediately with stale data |

---

## Previous Optimizations (Completed)

These were implemented in prior work and are documented here for reference:

- **Connection pooling** (`shared/db_pool.R`) — reduced per-query connection overhead
- **Redis tiered caching** (`shared/redis_cache.R`) — shared cache across workers with TTL tiers (2min–7day)
- **Load-aware sticky routing** (`lua/proxy_handler.lua`) — prevents hotspot workers
- **WebSocket detection** (`nginx.conf`) — separate handler for Shiny WebSocket connections
- **Historical cache warm-up** (`startup.sh`) — pre-populates cache on container start
- **Current-year cache helper** (`get_cached_curyr_data()`) — 3-min TTL for current year data
- **Back-navigation date persistence** (`dynamic_ui.R`) — saves/restores date on browser back button
