# Cattail Inspection Progress - Technical Notes

## Overview
This Shiny app tracks cattail larval mosquito inspection progress across three perspectives:
1. **Progress vs Goal** - Current year actual inspections compared to facility-specific goals
2. **Historical Comparison** - Current year performance vs previous years' baseline
3. **Treatment Planning** - Planned treatments by facility and plan type

## Data Sources

### Tables Used

#### Primary Inspection Tables
- **`dblarv_insptrt_current`** - Active larval inspection records
  - Contains ongoing inspections for current operations
  - Fields: `sitecode`, `facility`, `action`, `inspdate`, `reinspect`, `wet`, `numdip`, `acres`, `acres_plan`
  - **NOTE**: Does NOT have `enddate` column
  
- **`dblarv_insptrt_archive`** - Historical larval inspection records
  - Contains closed/archived inspections from previous years
  - Same schema as current table
  - Fields: `sitecode`, `facility`, `action`, `inspdate`, `reinspect`, `wet`, `numdip`, `acres`, `acres_plan`
  - **NOTE**: Does NOT have `enddate` column

#### Supporting Tables
- **`loc_breeding_sites`** - Site lifecycle tracking
  - **CRITICAL**: This is the ONLY table with `enddate` column
  - Tracks when sites are opened and closed
  - **CAN HAVE MULTIPLE ROWS PER SITECODE** with different enddates
  - Used to filter out sites that no longer exist
  
- **`gis_sectcode`** - Section/zone information
  - Links site codes to geographic zones (P1 = zone '1', P2 = zone '2')
  - Join pattern: 
    ```sql
    LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
    ```
  - **CRITICAL**: Uses exact 7-character match to prevent ambiguous zone assignments
  - **Fixed November 2025**: Changed from broad pattern matching to precise sectcode matching (see README.md Bug Fixes)
  
- **`cattail_pctcomplete_base`** - Goal/target data
  - Contains facility-specific goals for P1 and P2 zones
  - Fields: `facility`, `p1_totsitecount`, `p2_totsitecount`
  - **ONLY USED FOR PROGRESS VS GOAL TAB** - not for historical comparison

### Action Codes
- **Action '9'** - Cattail inspections (this is the focus of the app)
- Other actions are filtered out

### Data Fields for Action '9' Records
- **`sitecode`** - Unique site identifier
- **`facility`** - Facility responsible for the inspection
- **`inspdate`** - Date of inspection 
- **`wet`** - Whether the site was wet during inspection
- **`numdip`** - Number of dips taken during inspection (MUST be non-zero for action='9')
- **`acres`** - Site acres treated
- **`acres_plan`** - Planned acres for treatment
- **`reinspect`** - Flag indicating if this is a re-inspection

### Reinspect Flag
- **reinspect IS NULL OR reinspect = 'f'** - First inspection (counted)
- **reinspect = 't'** - Re-inspection (excluded to avoid double-counting)

## Critical Data Integrity Issue: Site Enddate Filtering

### The Problem
**Inspection tables do NOT have enddate columns.** Sites that have been closed (enddate in past) can still have records in inspection tables, which would incorrectly inflate counts.

### The Solution
**ALWAYS join with `loc_breeding_sites` and filter by enddate:**

```sql
LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
WHERE (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
```

### Why This Matters
- Site `190208-003` had 11 rows in `loc_breeding_sites`, ALL with past enddates
- Without the join, inspections from 2019-2023 appeared in results
- This caused "ghost sites" to appear in historical data
- **EVERY query that counts sites MUST include this filter**

### Which Queries Need This Filter?
 **Historical Comparison queries** - Archive AND Current tables (both historical and current year queries)
 **Sites table queries** - Any query showing site details

 **Progress vs Goal queries** - These are year-specific and don't need active site filtering (the goal is based on that year's operation)


### SQL Query Pattern for Site Details
```sql
-- Example: Get site details including required numdip
SELECT 
  a.sitecode,
  a.facility, 
  a.inspdate,
  a.wet,
  a.numdip,    --  for action='9'
  a.acres,
  a.acres_plan
FROM public.dblarv_insptrt_current a
LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode  
WHERE a.action = '9'
  AND (a.reinspect IS NULL OR a.reinspect = 'f')
  AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
  AND a.numdip > 0  -- Ensure valid dip count
```

## Tab 1: Progress vs Goal

### Purpose
Compare actual unique site inspections to facility-specific goals for the selected year and zone.

### Key Features
- **Year selector** - Choose year to analyze (defaults to current year)
- **Goal Type selector** - P1 (Primary Goal) or P2 (Secondary Goal)
- **Custom Date slider** - Analyze progress "as of" a specific date in the year

### Data Flow
1. Query `dblarv_insptrt_archive` and `dblarv_insptrt_current` for:
   - Action = '9' (cattail)
   - Selected year
   - Inspections on or before custom date
   - Exclude reinspects
   
2. Join with `gis_sectcode` to get zone information

3. Group by facility and zone, COUNT total inspections

4. Query `cattail_pctcomplete_base` for goals (p1_totsitecount or p2_totsitecount)

5. Filter to selected zone only (zone '1' for P1, zone '2' for P2)

6. Create side-by-side bar chart:
   - Green bars: Actual inspections
   - Gray bars: Goal

### SQL Query Pattern
```sql
-- Get actuals from archive
SELECT a.facility, g.zone, COUNT(*) AS inspections
FROM public.dblarv_insptrt_archive a
LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, ...) = LEFT(g.sectcode, ...)
WHERE a.action = '9'
  AND EXTRACT(YEAR FROM a.inspdate) = [year]
  AND a.inspdate <= '[custom_date]'
  AND (a.reinspect IS NULL OR a.reinspect = 'f')
GROUP BY a.facility, g.zone

-- Combine with current
UNION with same query on dblarv_insptrt_current

-- Get goals
SELECT facility, p1_totsitecount, p2_totsitecount 
FROM public.cattail_pctcomplete_base
```

**NOTE**: No `loc_breeding_sites` join needed here because we're measuring year-specific operational progress, not active site status.

## Tab 2: Historical Comparison

### Purpose
Compare current year's unique site inspections to a historical baseline (average of previous X years) by zone.

### Key Features
- **Zone selector** - P1 (Zone 1) or P2 (Zone 2)
  - **IMPORTANT**: This is NOT "goal type" - it's just zone filtering
  - The goal table is NOT used in this tab at all
- **Previous X years slider** - Number of historical years to compare (1-10, default 3)
- **Facility filter** - Multi-select to focus on specific facilities or all
- **Sites table view toggle**:
  - "All sites in last X years" - Show all sites inspected historically
  - "Sites NOT checked this year" - Show sites needing attention

### Site Inspection Details Table
The sites table shows detailed information for each site including:
- **Site Code** - Unique identifier
- **Facility** - Responsible facility
- **Last Inspection** - Date of most recent inspection
- **Wet** - Whether site was wet during last inspection  
- **Num Dip** - Number of dips taken (must be >0 for valid action='9' records)
- **Acres** - Site acres
- **Acres Plan** - Planned treatment acres

**Key SQL Elements:**
- Uses `ROW_NUMBER() OVER (PARTITION BY sitecode, facility ORDER BY inspdate DESC)` to get most recent inspection per site
- Includes `numdip` field to show inspection completeness
- Filters for valid inspections: `action='9'`, non-reinspects, active sites only

### Historical Baseline Calculation
**Historical Baseline** = ALL unique sites inspected in years [current_year - X] through [current_year - 1]

Example (current year = 2025, X = 3):
- Historical range: 2022, 2023, 2024 (excludes 2025)
- Query finds ALL unique sites inspected across those 3 years COMBINED
- This is a **TOTAL COMBINED COUNT**, not an average
- If a site was inspected in 2022, 2023, and 2024, it's counted once
- If a site was only inspected in 2023, it's counted once

**Current Year** = Unique sites inspected in 2025

**Important**: This shows TOTAL unique sites over X years, NOT the average per year.

### Data Flow

#### Graph Query
1. Query for **Historical Baseline**:
   ```sql
   WHERE EXTRACT(YEAR FROM a.inspdate) >= [current_year - X]
     AND EXTRACT(YEAR FROM a.inspdate) <= [current_year - 1]
     AND g.zone = '[selected_zone]'
     AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)  -- Critical!
   ```
   - Combines archive + current tables with UNION ALL
   - Joins with `gis_sectcode` for zone filtering
   - **Joins with `loc_breeding_sites` to exclude ended sites**
   - Groups by facility
   - Uses `COUNT(DISTINCT sitecode)` to avoid duplicates

2. Query for **Current Year**:
   ```sql
   WHERE EXTRACT(YEAR FROM a.inspdate) = [current_year]
     AND g.zone = '[selected_zone]'
     AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)  -- Critical!
   ```
   - Same structure as historical query
   - Only difference: single year filter

3. Combine results with type labels:
   - Historical results get `type = "Historical Baseline"`
   - Current results get `type = "Current Year"`

4. Plot as side-by-side bars:
   - Gray bars: Historical Baseline
   - Green bars: Current Year
   - Legend at top shows which is which

#### Sites Table Query

**Two modes based on `sites_view_type`:**

##### Mode 1: "All sites in last X years"
Shows every unique site inspected in the historical range, with details about when it was last inspected.

```sql
WITH ranked_inspections AS (
  SELECT 
    a.sitecode,
    a.facility,
    a.inspdate,
    a.wet,
    a.numdip,     -- Number of dips (must be >0 for action='9')
    a.acres,
    a.acres_plan,
    g.zone,
    ROW_NUMBER() OVER (PARTITION BY a.sitecode, a.facility ORDER BY a.inspdate DESC) as rn
  FROM public.dblarv_insptrt_current a
  LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
  LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
  WHERE a.action = '9'
    AND EXTRACT(YEAR FROM a.inspdate) >= [start_year]
    AND EXTRACT(YEAR FROM a.inspdate) <= [end_year]
    AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
    AND g.zone = '[selected_zone]'
    [facility_filter]
  
  UNION ALL
  
  [Same query on dblarv_insptrt_archive]
)
SELECT 
  sitecode,
  facility,
  inspdate as last_inspection,
  wet,
  numdip,
  acres,
  acres_plan
FROM ranked_inspections
WHERE rn = 1
ORDER BY facility, sitecode
```

##### Mode 2: "Sites NOT checked this year"
Shows only sites that were inspected historically but have NOT been inspected in the current year.

Uses the same query structure but adds a filter to exclude sites that appear in current year data.

**Key Insight**: Mode 2 is a filtered subset of Mode 1, showing only sites that need attention.

### Why COUNT(DISTINCT sitecode)?
A site can be inspected multiple times in a year (e.g., checkbacks). Using `COUNT(DISTINCT sitecode)` ensures:
- Site with 1 inspection = counted once
- Site with 5 inspections = counted once
- This gives unique site count, not total inspection count

Example for Sr facility in 2025:
- Total inspections: 1,822
- Unique sites: 1,662
- Difference: 160 sites had multiple inspections

### Treatment Planning

### Purpose
Show planned treatments (not actual inspections) grouped by facility and plan type.

### Key Features
- **View by**: Acres or Number of Sites
- **Facility filter**: Select specific facility or all
- **Plan type checkboxes**: A, D, G, N, U (different treatment plan categories)

### Site Details Table
Shows site-level treatment planning details including:
- **Site Code** - Unique identifier
- **Facility** - Responsible facility  
- **Plan Type** - Treatment plan category (Air, Drone, Ground, None, Unknown)
- **Inspection Date** - Date of inspection
- **Wet** - Site wetness status
- **Num Dip** - Number of dips taken (must be >0 for action='9')
- **Acres** - Site acres
- **Acres Plan** - Planned treatment acres

### SQL Query Example
```sql
SELECT 
  a.sitecode,
  a.facility,
  a.airgrnd_plan,
  a.inspdate,
  a.wet,
  a.numdip,      -- Dip count validation
  a.acres,
  a.acres_plan
FROM public.dblarv_insptrt_current a
LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
WHERE a.action = '9'
  AND a.acres_plan IS NOT NULL
  AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
  [facility_filter]
  [plan_types_filter]
ORDER BY a.facility, a.airgrnd_plan, a.sitecode
```

**NOTE**: This tab's queries and logic are in `planned_treatment_functions.R`

## Comprehensive SQL Query Examples

### Progress vs Goal Queries

```sql
WITH all_inspections AS (
  -- Get all cattail inspections for the selected year up to custom date
  SELECT 
    a.facility,
    a.sitecode,
    a.inspdate,
    g.zone,
    ROW_NUMBER() OVER (PARTITION BY a.sitecode ORDER BY a.inspdate DESC) as rn
  FROM (
    -- Archive table
    SELECT facility, sitecode, inspdate 
    FROM public.dblarv_insptrt_archive
    WHERE action = '9'
      AND EXTRACT(YEAR FROM inspdate) = [year]
      AND inspdate <= '[custom_date]'
    
    UNION ALL
    
    -- Current table
    SELECT facility, sitecode, inspdate 
    FROM public.dblarv_insptrt_current
    WHERE action = '9'
      AND EXTRACT(YEAR FROM inspdate) = [year]
      AND inspdate <= '[custom_date]'
  ) a
  LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 7) = g.sectcode
)
SELECT 
  facility,
  zone,
  COUNT(DISTINCT sitecode) AS inspections
FROM all_inspections
WHERE rn = 1
GROUP BY facility, zone
```

#### Get Goals from Base Table
```sql
SELECT facility, p1_totsitecount, p2_totsitecount 
FROM public.cattail_pctcomplete_base
```

### Historical Comparison Queries

#### Historical Baseline (Combined Years)
```sql
-- Get historical baseline: all unique sites from last 3 years (2022-2024)
WITH all_inspections AS (
  SELECT 
    a.facility,
    a.sitecode,
    g.zone
  FROM public.dblarv_insptrt_archive a
  LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
  LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
  WHERE a.action = '9'
    AND EXTRACT(YEAR FROM a.inspdate) >= 2022
    AND EXTRACT(YEAR FROM a.inspdate) <= 2024
    AND (a.reinspect IS NULL OR a.reinspect = 'f')
    AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
    AND g.zone = '1'
  
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
    AND (a.reinspect IS NULL OR a.reinspect = 'f')
    AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
    AND g.zone = '1'
)
SELECT 
  facility,
  COUNT(DISTINCT sitecode)::integer as count
FROM all_inspections
GROUP BY facility
ORDER BY facility
```

#### Current Year Comparison
```sql
-- Get current year (2025) unique sites
WITH all_inspections AS (
  SELECT 
    a.facility,
    a.sitecode,
    g.zone
  FROM public.dblarv_insptrt_archive a
  LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(a.sitecode, 7)
  LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
  WHERE a.action = '9'
    AND EXTRACT(YEAR FROM a.inspdate) = 2025
    AND (a.reinspect IS NULL OR a.reinspect = 'f')
    AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
    AND g.zone = '1'
  
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
    AND (a.reinspect IS NULL OR a.reinspect = 'f')
    AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
    AND g.zone = '1'
)
SELECT 
  facility,
  COUNT(DISTINCT sitecode)::integer as count
FROM all_inspections
GROUP BY facility
ORDER BY facility
```

### Treatment Planning Queries

#### Get Treatment Plan Summary
```sql
SELECT a.facility, a.airgrnd_plan, SUM(a.acres_plan) as total_acres
FROM public.dblarv_insptrt_current a
LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
WHERE a.action = '9'
AND a.acres_plan IS NOT NULL
AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
GROUP BY a.airgrnd_plan, a.facility
ORDER BY a.airgrnd_plan, a.facility
```

#### Get Site Details for Treatment Planning
```sql
SELECT 
  a.sitecode,
  a.facility,
  a.airgrnd_plan,
  a.inspdate,
  a.wet,
  a.numdip,
  a.acres,
  a.acres_plan
FROM public.dblarv_insptrt_current a
LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
WHERE a.action = '9'
  AND a.acres_plan IS NOT NULL
  AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
  AND a.facility = 'Sr'
  AND a.airgrnd_plan IN ('A', 'D', 'G')
ORDER BY a.facility, a.airgrnd_plan, a.sitecode
```

## Code Organization

### File Structure
```
cattail/
├── app.R                          # Main UI and server logic (NO SQL!)
├── progress_functions.R           # Progress vs Goal tab functions
├── historical_functions.R         # Historical Comparison tab functions
├── planned_treatment_functions.R  # Treatment Planning tab functions
├── test_sr_counts.R              # Test script for Sr facility counts
├── test_sr_zone_comparison.R     # Test script for zone-specific counts
└── NOTES.md                       # This file
```

### Design Principles
1. **Separation of Concerns**: UI/server in app.R, ALL SQL in function files
2. **No SQL in app.R**: Every query is wrapped in a function
3. **Consistent Patterns**: All tabs follow same structure:
   - eventReactive for data loading on refresh button
   - Function calls with input parameters
   - Render functions for plots and tables

### Function Files

#### `progress_functions.R`
- `get_progress_data(year, goal_column, custom_today)` - Query actuals and goals
- `create_progress_plot(data)` - Render progress bar chart

#### `historical_functions.R`
- `get_historical_progress_data(hist_years, hist_zone, hist_facility_filter)` - Query historical + current year data
- `create_historical_progress_plot(data, hist_years)` - Render side-by-side comparison chart
- `get_sites_table_data(hist_years, hist_zone, hist_facility_filter, sites_view_type)` - Query site details

#### `planned_treatment_functions.R`
- Functions for treatment planning tab (not detailed here)

### Shared Utilities (`shared/db_helpers.R`)
- `get_db_connection()` - Database connection helper
- `get_facility_choices()` - Facility dropdown options
- `get_facility_base_colors()` - Color mapping for facilities
- `get_status_colors()` - Color mapping for status types (active, planned, etc.)
- `map_facility_names()` - Convert facility codes to display names


### Date Handling
- Dates stored as PostgreSQL `date` or `timestamp`
- Converted to R `Date` objects
- UI date inputs use `dateInput()` widget
- SQL date filtering: `EXTRACT(YEAR FROM inspdate) = 2025`

## Performance Considerations

### Query Optimization
1. **Filter early**: Apply action, year, reinspect filters in WHERE clause before joins
2. **UNION ALL vs UNION**: Use UNION ALL (keeps duplicates) then COUNT DISTINCT instead of UNION (removes duplicates during merge)
3. **Index-friendly**: Year extraction and zone filtering are indexed in database

### R-Side Processing
1. **Numeric conversion**: Convert integer64 to numeric immediately after query
2. **Group operations**: Use dplyr for efficient grouping and summarization
3. **Reactive data**: Use `eventReactive` to load data only on button click, preventing accidental heavy queries
