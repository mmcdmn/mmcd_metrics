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
  - Fields: `sitecode`, `facility`, `action`, `inspdate`, `reinspect`
  - **NOTE**: Does NOT have `enddate` column
  
- **`dblarv_insptrt_archive`** - Historical larval inspection records
  - Contains closed/archived inspections from previous years
  - Same schema as current table
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
    LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 6) || '-' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'N' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'S' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'E' = g.sectcode
      OR LEFT(a.sitecode, 6) || 'W' = g.sectcode
    ```
  - Note: Some sitecodes use directional suffixes (N, S, E, W) instead of '-'
  
- **`cattail_pctcomplete_base`** - Goal/target data
  - Contains facility-specific goals for P1 and P2 zones
  - Fields: `facility`, `p1_totsitecount`, `p2_totsitecount`
  - **ONLY USED FOR PROGRESS VS GOAL TAB** - not for historical comparison

### Action Codes
- **Action '9'** - Cattail inspections (this is the focus of the app)
- Other actions are filtered out

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
✅ **Historical Comparison queries** - Archive AND Current tables (both historical and current year queries)
✅ **Sites table queries** - Any query showing site details

❌ **Progress vs Goal queries** - These are year-specific and don't need active site filtering (the goal is based on that year's operation)

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

### Historical Baseline Calculation
**Historical Baseline** = Unique sites inspected in years [current_year - X] through [current_year - 1]

Example (current year = 2025, X = 3):
- Historical range: 2022, 2023, 2024 (excludes 2025)
- Query finds ALL unique sites inspected across those 3 years
- This is a **COMBINED count**, not an average

**Current Year** = Unique sites inspected in 2025

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
WITH historical_sites AS (
  -- Get all sites from historical range (archive + current)
  SELECT DISTINCT sitecode, facility
  WHERE inspdate between [start_year] and [end_year]
    AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
),
current_year_sites AS (
  -- Get sites inspected this year
  SELECT DISTINCT sitecode
  WHERE EXTRACT(YEAR FROM inspdate) = [current_year]
)
SELECT h.sitecode, h.facility,
       MAX(a.inspdate) as last_inspdate,
       CASE WHEN c.sitecode IS NOT NULL THEN 'Yes' ELSE 'No' END as checked_this_year
FROM historical_sites h
LEFT JOIN inspections a ON h.sitecode = a.sitecode
LEFT JOIN current_year_sites c ON h.sitecode = c.sitecode
GROUP BY h.sitecode, h.facility, c.sitecode
```

##### Mode 2: "Sites NOT checked this year"
Shows only sites that were inspected historically but have NOT been inspected in the current year.

```sql
WITH historical_sites AS (
  -- Sites from historical range only
),
current_year_sites AS (
  -- Sites inspected this year
)
SELECT h.sitecode, h.facility, MAX(a.inspdate) as last_inspdate
FROM historical_sites h
LEFT JOIN current_year_sites c ON h.sitecode = c.sitecode
WHERE c.sitecode IS NULL  -- Not in current year
```

**Key Insight**: Mode 2 is a filtered subset of Mode 1, showing only sites that need attention.

### Why COUNT(DISTINCT sitecode)?
A site can be inspected multiple times in a year (e.g., monthly checks). Using `COUNT(DISTINCT sitecode)` ensures:
- Site with 1 inspection = counted once
- Site with 5 inspections = counted once
- This gives unique site count, not total inspection count

Example for Sr facility in 2025:
- Total inspections: 1,822
- Unique sites: 1,662
- Difference: 160 sites had multiple inspections

### Zone vs Goal Confusion
**IMPORTANT**: The Historical Comparison tab selector was incorrectly labeled "Goal Type" initially, but it should be "Zone":
- **Progress vs Goal tab**: Uses goal table, compares to targets
- **Historical Comparison tab**: Does NOT use goal table, just filters by zone for geographic grouping

Zones are simply geographic areas:
- **Zone 1 (P1)** = Primary area operations
- **Zone 2 (P2)** = Secondary area operations

## Tab 3: Treatment Planning

### Purpose
Show planned treatments (not actual inspections) grouped by facility and plan type.

### Key Features
- **View by**: Acres or Number of Sites
- **Facility filter**: Select specific facility or all
- **Plan type checkboxes**: A, D, G, N, U (different treatment plan categories)

### Data Flow
Queries planned treatment data and displays:
- Bar chart showing planned acres/sites by facility
- Table showing site-level details with plan types

**NOTE**: This tab's queries and logic are in `planned_treatment_functions.R`

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

## Common Data Patterns

### Facility Codes
- **E** - Eden Prairie
- **N** - North Metro
- **Sj** - St. Paul  
- **Sr** - South Metro
- **Wm** - Maple Grove
- **Wp** - Plymouth

Display names are mapped via `map_facility_names()` from db_helpers.

### Zone Mapping
Zones are extracted from site codes via `gis_sectcode` join:
```sql
LEFT JOIN public.gis_sectcode g 
  ON LEFT(a.sitecode, 6) || '-' = g.sectcode
  OR LEFT(a.sitecode, 6) || 'N' = g.sectcode
  OR LEFT(a.sitecode, 6) || 'S' = g.sectcode
  OR LEFT(a.sitecode, 6) || 'E' = g.sectcode
  OR LEFT(a.sitecode, 6) || 'W' = g.sectcode
```

This matches the first 6 characters of the sitecode and tries multiple suffix patterns ('-', 'N', 'S', 'E', 'W') to find the corresponding sectcode and get the zone.

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

## Testing & Validation

### Test Scripts

#### `test_sr_counts.R`
Tests Sr facility inspection counts for 2024 vs 2025:
- Verifies COUNT(DISTINCT sitecode) logic
- Checks for duplicate inspections
- Shows zone distribution
- Confirms enddate filtering works

#### `test_sr_zone_comparison.R`
Tests zone-specific counts to match app output:
- Runs same query as app with zone filtering
- Compares P1 vs P2 results
- Validates that test matches actual app behavior

### Known Test Results (as of Nov 2025)
**Sr Facility - Zone 1 (P1):**
- 2024: 858 unique sites
- 2025: 1,207 unique sites (+40% increase)

**Sr Facility - Zone 2 (P2):**
- 2024: 122 unique sites
- 2025: 170 unique sites (+39% increase)

### Validation Checklist
✅ No duplicate site codes in results (use COUNT DISTINCT)
✅ Sites with past enddates are excluded (loc_breeding_sites join)
✅ Reinspects are excluded (reinspect = 'f' or NULL)
✅ Only action '9' included
✅ Zone filtering matches user selection
✅ Historical range excludes current year

## Common Issues & Solutions

### Issue 1: Sites with Past Enddates Appearing
**Symptom**: Sites that no longer exist show up in results
**Cause**: Missing `loc_breeding_sites` join with enddate filter
**Solution**: Always join and filter:
```sql
LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
WHERE (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
```

### Issue 2: Duplicate Site Counts
**Symptom**: Count is higher than expected
**Cause**: Using `COUNT(*)` instead of `COUNT(DISTINCT sitecode)`
**Solution**: Always use `COUNT(DISTINCT sitecode)` when counting unique sites

### Issue 3: Integer64 Precision Loss
**Symptom**: Calculations produce unexpected results
**Cause**: PostgreSQL returns integer64 for COUNT/SUM
**Solution**: Convert to numeric immediately:
```r
result$count <- as.numeric(result$count)
```

### Issue 4: Wrong Tab Using Goal Table
**Symptom**: Historical comparison showing goal-related data
**Cause**: Confusion between "goal type" and "zone"
**Solution**: 
- Progress vs Goal tab: Uses `cattail_pctcomplete_base` table
- Historical Comparison tab: Does NOT use goal table, only zone filtering

### Issue 5: Overlapping Bars in Chart
**Symptom**: Can't see both historical and current bars when current is higher
**Cause**: Overlaid bar design covers smaller bar
**Solution**: Use side-by-side (position_dodge) bars instead of overlaid

## Future Enhancements

Potential improvements:
- Add year-over-year trend analysis
- Export tables to Excel
- Show percentage progress toward goal
- Add time-series animation
- Include weather/climate data correlation
- Highlight facilities behind schedule
- Add email alerts for low progress
- Show site-level inspection history timeline

## Database Connection

### Connection Details
- Host: rds-readonly.mmcd.org
- Port: 5432
- Database: mmcd_data
- User: readonly
- Connection managed via `get_db_connection()` from shared/db_helpers.R

### Best Practices
- Always disconnect after query: `dbDisconnect(con)`
- Use parameterized queries with sprintf for safe SQL generation
- Return empty data.frame() if connection fails
- Check for NULL connection before querying

## Key Takeaways

1. **Site enddate filtering is CRITICAL** - Must join `loc_breeding_sites` on historical queries
2. **COUNT DISTINCT for unique sites** - Sites can have multiple inspections
3. **Historical baseline is COMBINED count** - Not an average, all sites across X years
4. **Zone ≠ Goal** - Historical tab uses zones for geographic filtering, not goal targets
5. **Separate SQL from UI** - All queries in function files, never in app.R
6. **Test your queries** - Use test scripts to validate counts match expected results
