# Cattail Inspections Progress Dashboard - Technical Documentation

## Overview
Dashboard to track cattail inspection progress against goals with historical comparison analysis.

---

## Data Sources

### Action Codes
- **Action '9'**: Cattail inspections (primary focus)
- All other action codes are filtered out

### Key Database Tables
- **`dblarv_insptrt_archive`**: Historical inspection records
- **`dblarv_insptrt_current`**: Current year inspection records
- **`loc_breeding_sites`**: Site master data with acres and enddate
- **`gis_sectcode`**: Zone and facility assignments (P1/P2 zones)
- **`cattail_pctcomplete_base`**: Site count goals by facility (p1_totsitecount, p2_totsitecount)

### Critical Data Fields
- **`sitecode`**: Unique site identifier
- **`facility`**: Facility code from gis_sectcode (NOT from inspection records)
- **`zone`**: P1 (zone='1') or P2 (zone='2') from gis_sectcode
- **`inspdate`**: Date of inspection
- **`wet`**: Whether site was wet during inspection
- **`numdip`**: Number of larvae found per dip
- **`acres_plan`**: Planned acres for treatment (from inspection record)
- **`acres`**: Actual site acres (from loc_breeding_sites)
- **`enddate`**: Site end date (NULL = active site)

---

## Inspection Counting Logic

### Single Inspection Per Site Rule
**CRITICAL**: Each site should only be counted ONCE per query period, regardless of how many times it was inspected.

**Method**: Use `ROW_NUMBER()` window function partitioned by sitecode, ordered by most recent inspection:
```sql
ROW_NUMBER() OVER (PARTITION BY a.sitecode ORDER BY a.inspdate DESC) as rn
WHERE rn = 1  -- Only the most recent inspection per site
```

### Active Sites Filter
**CRITICAL**: Always filter by enddate to exclude closed/inactive sites:
```sql
LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
WHERE (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
```

### Reinspect Field (DEPRECATED)
The `reinspect` field is **NOT RELIABLE** for counting logic:
- `reinspect = 't'` means "should be re-inspected" but doesn't guarantee it happened
- `reinspect = 'f'` or `NULL` is the first inspection
- **Solution**: Ignore the `reinspect` field entirely and use `ROW_NUMBER()` to ensure one count per site

---

## Progress vs Goal Tab

### Query 1: Get Inspection Counts by Facility and Zone

**Purpose**: Count unique sites inspected per facility/zone for comparison against goals.

```sql
WITH all_inspections AS (
  SELECT 
    a.facility,
    a.sitecode,
    a.inspdate,
    g.zone,
    ROW_NUMBER() OVER (PARTITION BY a.sitecode ORDER BY a.inspdate DESC) as rn
  FROM (
    -- Archive records
    SELECT facility, sitecode, inspdate 
    FROM public.dblarv_insptrt_archive
    WHERE action = '9'
      AND EXTRACT(YEAR FROM inspdate) = 2025  -- Selected year
      AND inspdate <= '2025-12-04'            -- Custom "today" date
    
    UNION ALL
    
    -- Current records
    SELECT facility, sitecode, inspdate 
    FROM public.dblarv_insptrt_current
    WHERE action = '9'
      AND EXTRACT(YEAR FROM inspdate) = 2025  -- Selected year
      AND inspdate <= '2025-12-04'            -- Custom "today" date
  ) a
  LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
  LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
  WHERE (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
)
SELECT 
  facility,
  zone,
  COUNT(DISTINCT sitecode) AS inspections
FROM all_inspections
WHERE rn = 1  -- Only most recent inspection per site
GROUP BY facility, zone
```

**Key Points**:
- ✅ Uses `LEFT(a.sitecode, 7)` for exact sectcode matching
- ✅ Filters active sites with enddate check
- ✅ Uses ROW_NUMBER() to count each site only once
- ✅ UNION ALL combines archive and current tables
- ✅ Groups by facility and zone for P1/P2 separation

### Query 2: Get Site Details for Table Display

**Purpose**: Retrieve detailed information for each site contributing to the progress chart.

```sql
WITH all_inspections AS (
  SELECT 
    a.facility,
    a.sitecode,
    a.inspdate,
    a.wet,
    a.numdip,
    g.zone,
    ROW_NUMBER() OVER (PARTITION BY a.sitecode ORDER BY a.inspdate DESC) as rn
  FROM (
    -- Archive records
    SELECT facility, sitecode, inspdate, wet, numdip 
    FROM public.dblarv_insptrt_archive
    WHERE action = '9'
      AND EXTRACT(YEAR FROM inspdate) = 2025
      AND inspdate <= '2025-12-04'
    
    UNION ALL
    
    -- Current records
    SELECT facility, sitecode, inspdate, wet, numdip 
    FROM public.dblarv_insptrt_current
    WHERE action = '9'
      AND EXTRACT(YEAR FROM inspdate) = 2025
      AND inspdate <= '2025-12-04'
  ) a
  LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
)
SELECT 
  ai.facility,
  ai.sitecode,
  ai.inspdate,
  ai.wet,
  ai.numdip,
  COALESCE(b.acres, 0) as acres
FROM all_inspections ai
LEFT JOIN public.loc_breeding_sites b ON ai.sitecode = b.sitecode
WHERE ai.rn = 1                    -- Most recent inspection only
  AND ai.zone = '1'                -- Selected zone (P1 or P2)
  AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
ORDER BY ai.facility, ai.inspdate DESC
```

**Key Points**:
- ✅ Returns individual site records (not aggregated)
- ✅ Includes wet, numdip, and acres for detailed analysis
- ✅ Filters by selected zone (P1 or P2 based on goal selection)
- ✅ One row per site (most recent inspection)

---

## Historical Comparison Tab

### Query 3: Historical Baseline (Multi-Year Average)

**Purpose**: Calculate average inspection counts over previous N years (excluding current year).

```sql
WITH all_inspections AS (
  SELECT 
    a.facility,
    a.sitecode,
    g.zone
  FROM public.dblarv_insptrt_archive a
  LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
  LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
  WHERE a.action = '9'
    AND EXTRACT(YEAR FROM a.inspdate) >= 2022  -- Start year (current - hist_years)
    AND EXTRACT(YEAR FROM a.inspdate) <= 2024  -- End year (current - 1)
    AND g.zone = '1'                            -- Zone filter (P1, P2, combined, or separate)
    AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
  
  UNION ALL
  
  SELECT 
    a.facility,
    a.sitecode,
    g.zone
  FROM public.dblarv_insptrt_current a
  LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
  LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
  WHERE a.action = '9'
    AND EXTRACT(YEAR FROM a.inspdate) >= 2022
    AND EXTRACT(YEAR FROM a.inspdate) <= 2024
    AND g.zone = '1'
    AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
)
SELECT 
  facility,
  zone,
  COUNT(DISTINCT sitecode)::integer as site_count,
  SUM(DISTINCT_ACRES.acres)::numeric as acre_count
FROM (
  SELECT DISTINCT
    facility,
    sitecode,
    zone,
    FIRST_VALUE(COALESCE(acres_plan, acres)) OVER (
      PARTITION BY facility, sitecode 
      ORDER BY inspdate DESC
    ) as acres
  FROM (
    -- Get acres_plan from inspection records
    SELECT 
      ai.facility,
      ai.sitecode,
      ai.zone,
      a.inspdate,
      a.acres_plan,
      b.acres
    FROM all_inspections ai
    LEFT JOIN public.dblarv_insptrt_archive a 
      ON ai.sitecode = a.sitecode AND ai.facility = a.facility
    LEFT JOIN public.loc_breeding_sites b ON ai.sitecode = b.sitecode
    WHERE a.action = '9'
      AND EXTRACT(YEAR FROM a.inspdate) >= 2022
      AND EXTRACT(YEAR FROM a.inspdate) <= 2024
    
    UNION ALL
    
    SELECT 
      ai.facility,
      ai.sitecode,
      ai.zone,
      a.inspdate,
      a.acres_plan,
      b.acres
    FROM all_inspections ai
    LEFT JOIN public.dblarv_insptrt_current a 
      ON ai.sitecode = a.sitecode AND ai.facility = a.facility
    LEFT JOIN public.loc_breeding_sites b ON ai.sitecode = b.sitecode
    WHERE a.action = '9'
      AND EXTRACT(YEAR FROM a.inspdate) >= 2022
      AND EXTRACT(YEAR FROM a.inspdate) <= 2024
  ) site_acres
) DISTINCT_ACRES
GROUP BY facility, zone
ORDER BY facility, zone
```

**Key Points**:
- ✅ CTE filters to historical years only (excludes current year)
- ✅ Nested query calculates acres using FIRST_VALUE window function
- ✅ Prefers `acres_plan` from inspection, falls back to `acres` from loc_breeding_sites
- ✅ COUNT DISTINCT ensures one count per site across all historical years
- ✅ Returns both site_count and acre_count metrics

### Query 4: Current Year Progress

**Purpose**: Count inspections completed in the current year for comparison against historical baseline.

```sql
WITH all_inspections AS (
  SELECT 
    a.facility,
    a.sitecode,
    g.zone
  FROM public.dblarv_insptrt_archive a
  LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
  LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
  WHERE a.action = '9'
    AND EXTRACT(YEAR FROM a.inspdate) = 2025  -- Current year only
    AND g.zone = '1'                          -- Zone filter
    AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
  
  UNION ALL
  
  SELECT 
    a.facility,
    a.sitecode,
    g.zone
  FROM public.dblarv_insptrt_current a
  LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
  LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
  WHERE a.action = '9'
    AND EXTRACT(YEAR FROM a.inspdate) = 2025
    AND g.zone = '1'
    AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
)
SELECT 
  facility,
  zone,
  COUNT(DISTINCT sitecode)::integer as site_count,
  SUM(DISTINCT_ACRES.acres)::numeric as acre_count
FROM (
  SELECT DISTINCT
    facility,
    sitecode,
    zone,
    FIRST_VALUE(COALESCE(acres_plan, acres)) OVER (
      PARTITION BY facility, sitecode 
      ORDER BY inspdate DESC
    ) as acres
  FROM (
    SELECT 
      ai.facility,
      ai.sitecode,
      ai.zone,
      a.inspdate,
      a.acres_plan,
      b.acres
    FROM all_inspections ai
    LEFT JOIN public.dblarv_insptrt_archive a 
      ON ai.sitecode = a.sitecode AND ai.facility = a.facility
    LEFT JOIN public.loc_breeding_sites b ON ai.sitecode = b.sitecode
    WHERE a.action = '9'
      AND EXTRACT(YEAR FROM a.inspdate) = 2025
    
    UNION ALL
    
    SELECT 
      ai.facility,
      ai.sitecode,
      ai.zone,
      a.inspdate,
      a.acres_plan,
      b.acres
    FROM all_inspections ai
    LEFT JOIN public.dblarv_insptrt_current a 
      ON ai.sitecode = a.sitecode AND ai.facility = a.facility
    LEFT JOIN public.loc_breeding_sites b ON ai.sitecode = b.sitecode
    WHERE a.action = '9'
      AND EXTRACT(YEAR FROM a.inspdate) = 2025
  ) site_acres
) DISTINCT_ACRES
GROUP BY facility, zone
ORDER BY facility, zone
```

**Key Points**:
- ✅ Identical structure to historical query but filtered to current year only
- ✅ Enables side-by-side comparison: Current Year vs Historical Baseline
- ✅ Both queries share same zone filtering logic for consistency

---

## Zone Filtering Options

### User Interface Options:
1. **P1 Only** (`hist_zone = "p1"`)
   - SQL: `AND g.zone = '1'`
   - Groups by facility only
   
2. **P2 Only** (`hist_zone = "p2"`)
   - SQL: `AND g.zone = '2'`
   - Groups by facility only
   
3. **P1 and P2 Combined** (`hist_zone = "combined"`)
   - SQL: `AND g.zone IN ('1', '2')`
   - Groups by facility only (aggregates both zones)
   
4. **P1 and P2 Separate** (`hist_zone = "separate"`)
   - SQL: `AND g.zone IN ('1', '2')`
   - Groups by facility AND zone (shows separate bars)

### Implementation Pattern:
```r
# Dynamic SQL generation based on zone selection
if (hist_zone == "p1") {
  zone_condition <- "AND g.zone = '1'"
  group_by_clause <- "GROUP BY facility"
} else if (hist_zone == "p2") {
  zone_condition <- "AND g.zone = '2'"
  group_by_clause <- "GROUP BY facility"
} else if (hist_zone == "combined") {
  zone_condition <- "AND g.zone IN ('1', '2')"
  group_by_clause <- "GROUP BY facility"
} else if (hist_zone == "separate") {
  zone_condition <- "AND g.zone IN ('1', '2')"
  group_by_clause <- "GROUP BY facility, zone"
}
```

---

## Metrics: Sites vs Acres

### Site-Based Metric (Primary)
- **Source**: COUNT(DISTINCT sitecode)
- **Goal Source**: `cattail_pctcomplete_base` (p1_totsitecount, p2_totsitecount)
- **Use Case**: Measures number of unique locations inspected
- **Progress Calculation**: `(sites_inspected / goal_sites) * 100`

### Acre-Based Metric (Secondary)
- **Source**: SUM of acres per unique site
- **Acre Priority**: `COALESCE(acres_plan, acres)` - Prefers planned acres, falls back to site master acres
- **No Goals**: Acre goals NOT stored in database
- **Use Case**: Measures total area coverage
- **Note**: Source table in image uses acres for % calculation (different methodology)

### Key Difference:
- **App uses SITES** for % complete (matches database goals)
- **Source table uses ACRES** for % complete (no goals available in database)
- Both are valid but measure different aspects of progress

---

## Value Box Color Coding

Progress indicators use color thresholds:
```r
if (pct_complete >= 100) {
  color <- "green"    # Goal met or exceeded
} else if (pct_complete >= 75) {
  color <- "yellow"   # 75-99% complete
} else if (pct_complete >= 50) {
  color <- "orange"   # 50-74% complete
} else {
  color <- "red"      # Below 50% complete
}
```

---

## Common Pitfalls & Solutions

### ❌ WRONG: Using reinspect field for counting
```sql
WHERE (a.reinspect IS NULL OR a.reinspect = 'f')  -- Unreliable!
```

### ✅ CORRECT: Use ROW_NUMBER() for single count per site
```sql
ROW_NUMBER() OVER (PARTITION BY a.sitecode ORDER BY a.inspdate DESC) as rn
WHERE rn = 1
```

### ❌ WRONG: Ambiguous zone assignment with pattern matching
```sql
LEFT JOIN gis_sectcode g ON LEFT(sitecode, 6) || '-' = g.sectcode
  OR LEFT(sitecode, 6) || 'N' = g.sectcode  -- Matches multiple sectcodes!
```

### ✅ CORRECT: Exact sectcode matching
```sql
LEFT JOIN gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
```

### ❌ WRONG: Forgetting to filter inactive sites
```sql
-- Missing enddate filter allows closed sites!
```

### ✅ CORRECT: Always filter by enddate
```sql
LEFT JOIN loc_breeding_sites b ON a.sitecode = b.sitecode
WHERE (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
```

### ❌ WRONG: Using facility from inspection records
```sql
SELECT a.facility  -- May be outdated or incorrect
FROM dblarv_insptrt_archive a
```

### ✅ CORRECT: Using facility from gis_sectcode
```sql
SELECT g.facility  -- Always current, zone-aware
FROM dblarv_insptrt_archive a
LEFT JOIN gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
```

---

## Database Schema References

### gis_sectcode (Zone Master Table)
```
sectcode (varchar) - First 7 characters of sitecode
zone (varchar)     - '1' (P1) or '2' (P2)
facility (varchar) - Facility code
```

### loc_breeding_sites (Site Master Table)
```
sitecode (varchar) - Unique site identifier
acres (numeric)    - Site acreage
enddate (date)     - Site closure date (NULL = active)
```

### cattail_pctcomplete_base (Goal Table)
```
facility (varchar)        - Facility code
p1_totsitecount (integer) - P1 site goal
p2_totsitecount (integer) - P2 site goal
comments (text)           - Goal notes/updates
```

### dblarv_insptrt_archive / current (Inspection Tables)
```
sitecode (varchar)    - Site identifier
facility (varchar)    - Facility (DO NOT USE - use gis_sectcode.facility)
action (varchar)      - '9' for inspections
inspdate (date)       - Inspection date
wet (boolean)         - Site wet during inspection
numdip (integer)      - Larvae count per dip
acres_plan (numeric)  - Planned treatment acres
reinspect (boolean)   - Re-inspection flag (UNRELIABLE - DO NOT USE)
```

---

## File Structure

```
apps/cattail_inspections/
├── app.R                     # Main UI and server logic
├── progress_functions.R      # Progress vs Goal queries and charts
├── historical_functions.R    # Historical comparison queries and charts
├── ui_helper.R              # UI component functions
├── data_functions.R         # Shared data processing utilities
├── display_functions.R      # Shared visualization utilities
└── NOTES.md                 # This documentation file
```

---

## Version History

- **December 2025**: Complete query rewrite with corrected zone assignment logic
  - Fixed ambiguous sectcode JOIN pattern
  - Removed unreliable reinspect field dependency
  - Added mandatory loc_breeding_sites enddate filter
  - Documented all SQL queries with exact syntax
  - Clarified single-count-per-site methodology

- **November 2025**: Added site details table with downloadable CSV
- **October 2025**: Added historical comparison tab with zone filtering
