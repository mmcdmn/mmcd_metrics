# Drone Sites Treatment Tracking - Technical Notes

## Overview
This Shiny app tracks drone-treated mosquito breeding sites across three perspectives:
1. **Current Progress** - Sites with active/expiring treatments, aggregated by facility, FOS, or section
2. **Historical Trends** - Multi-year trends in drone treatments (sites, treatments, or acres)
3. **Site Statistics** - Site-level statistics showing average, largest, and smallest treated sites

## Data Sources

### Tables Used

#### Primary Treatment Tables
- **`dblarv_insptrt_current`** - Active larval treatment records
  - Contains ongoing treatments for current operations
  - Fields: `sitecode`, `facility`, `inspdate`, `matcode`, `acres`, `foreman`, `action`, `airgrnd_plan`
  - Filter: `airgrnd_plan = 'D'` OR `action = 'D'` (drone treatments)
  - **NOTE**: Does NOT have `zone` or `enddate` columns
  
- **`dblarv_insptrt_archive`** - Historical larval treatment records
  - Contains closed/archived treatments from previous years
  - Same schema as current table
  - Filter: `action = 'D'` (drone treatments)
  - **NOTE**: Does NOT have `zone` or `enddate` columns

#### Supporting Tables
- **`loc_breeding_sites`** - Site information and lifecycle tracking
  - **CRITICAL**: This is the ONLY table with `enddate` column
  - Tracks site characteristics: `acres`, `prehatch`, `drone` designation
  - **CAN HAVE MULTIPLE ROWS PER SITECODE** with different enddates
  - Used to filter out sites that no longer exist
  - Join pattern: **MUST filter by `enddate IS NULL`** to get active sites only
  
- **`gis_sectcode`** - Section/zone information
  - Links site codes to geographic zones (P1 = zone '1', P2 = zone '2')
  - Provides FOS (Field Operations Supervisor) area assignments
  - Join pattern handles two sitecode formats:
    ```sql
    LEFT JOIN public.gis_sectcode sc ON left(sitecode,7) = sc.sectcode
    ```
  - Note: Some sitecodes use '-' separator (190208-003), others use directional suffixes (191102W010)
  
- **`mattype_list_targetdose`** - Material effectiveness duration
  - Contains `effect_days` - how long a treatment remains effective
  - Joined with treatment records to calculate treatment end dates
  - Default: 14 days if `effect_days` is NULL

- **`employee_list`** - FOS (Field Operations Supervisor) information
  - Filter: `emp_type = 'FieldSuper'` AND `active = true`
  - Used to map FOS areas from gis_sectcode to actual employee numbers

### Drone Designation Filters
- **Drone field values**: 'Y', 'M', 'C' (different drone types)
- **air_gnd field**: 'D' (drone designation)
- Sites are included if: `drone IN ('Y', 'M', 'C')` OR `air_gnd = 'D'`

### Treatment Status Logic
- **Active Treatment**: `treatment_end_date >= current_date`
  - `treatment_end_date = inspdate + effect_days`
  - Materials have varying effectiveness periods (from mattype_list_targetdose)
  
- **Expiring Treatment**: Active AND ending within specified days window
  - User can adjust "Days Until Expiration" slider (default: 7 days)
  - Shows treatments requiring attention soon

### Foreman (FOS) Assignment
- **Site foreman** (jurisdictional): From `gis_sectcode` → FOS area
- **Treatment foreman** (who applied): From treatment record
- **App uses SITE foreman** for filtering and grouping (jurisdictional assignment)

## Critical Data Integrity Issue: Site Enddate Filtering

### The Problem
**Treatment tables do NOT have enddate columns.** Sites that have been closed can still have treatment records in the inspection tables.

### The Solution
**ALWAYS join with `loc_breeding_sites` and filter by enddate:**

```sql
LEFT JOIN public.loc_breeding_sites b ON t.sitecode = b.sitecode AND t.facility = b.facility
WHERE (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
```

### Why This Matters
- Without this filter, treatments on closed sites appear in results
- This causes "ghost sites" to inflate counts
- **Current Progress tab filters during load_raw_data()**
- **Historical and Site Statistics tabs must handle this in their queries**

## Tab 1: Current Progress

### Purpose
Show drone sites grouped by facility/FOS/section with treatment status breakdown (total sites, active treatments, expiring treatments).

### Key Features
- **Display Metric selector** - Number of Sites or Total Acres
- **Expiring Days slider** - Define the expiration warning window (1-30 days)
- **Zone filter** - P1, P2, or both (separate or combined)
- **Facility filter** - Multi-select specific facilities
- **FOS filter** - Multi-select specific Field Operations Supervisors
- **Prehatch filter** - Show only prehatch sites
- **Group By selector** - Facility, FOS, or Section (current tab only)

### Data Flow
1. Load raw data via `load_raw_data()`:
   - Query `loc_breeding_sites` for active drone sites (enddate IS NULL)
   - Query `dblarv_insptrt_current` for drone treatments
   - Join treatments with sites to get zone, foreman, prehatch info
   - Calculate treatment status (active/expired)

2. Apply filters via `apply_data_filters()`:
   - Facility filter (if not "all")
   - FOS filter (convert shortnames to employee numbers)
   - Prehatch filter (if checked)

3. Process for display via `process_current_data()`:
   - Apply zone filter
   - Calculate expiring window based on slider
   - Group by selected dimension (facility/FOS/section)
   - Handle zone separation (if both zones selected, show separately)
   - Aggregate counts: total sites, active sites, expiring sites

### Visualization
- **Layered bar chart** with three bars per group:
  - Gray faded bar: Total sites (background)
  - Solid color bar: Sites with active treatments
  - Orange bar: Sites with expiring treatments (overlay)
- Colors mapped by group type:
  - **Facility**: Distinct facility colors from db_helpers
  - **FOS**: Facility-based colors mapped through foreman lookup
  - **Section**: Inherits facility colors based on section location

### Zone Handling
- **Both zones selected**: Shows groups separately (e.g., "South Rosemount (P1)", "South Rosemount (P2)")
- **Single zone**: Shows groups without zone suffix
- **Zone differentiation**: Uses alpha transparency (P1 solid, P2 faded) when both zones shown

## Tab 2: Historical Trends

### Purpose
Analyze multi-year trends in drone treatment activity across facilities, FOS, or combined.

### Key Features
- **Display Metric selector** - Number of Sites, Number of Treatments, or Number of Acres
- **Year Range selectors** - Start Year and End Year (2010-2025)
- **Show Percentages checkbox** - Toggle between raw counts and percentage distribution
- **Zone filter** - P1, P2, or both (shared with Current Progress)
- **Facility/FOS filters** - Shared with Current Progress
- **Group By selector** - Facility or FOS (section not available for historical)

### Data Flow
1. Query historical data via `get_historical_raw_data()`:
   - Query `dblarv_insptrt_archive` for historical drone treatments
   - Query `dblarv_insptrt_current` for recent drone treatments
   - Join with `loc_breeding_sites` for site info (acres, prehatch, zone)
   - Filter by year range

2. Process via `get_historical_processed_data()`:
   - Apply facility, FOS, and prehatch filters
   - Group by selected dimension and year
   - Calculate metric:
     - **Sites**: `COUNT(DISTINCT sitecode)` per group per year
     - **Treatments**: `COUNT(*)` all treatments per group per year
     - **Acres**: `SUM(acres)` total acres treated per group per year

3. Fill in missing years:
   - Ensure all groups have entries for all years in range
   - Fill missing values with 0

### Visualization
- **Stacked bar chart** showing distribution across groups over time
- **X-axis**: Years (one bar per year)
- **Y-axis**: Count or percentage
- **Colors**: Same color mapping as Current Progress
- **Zone handling**: When both zones selected, shows zone-specific breakdowns with alpha transparency

### Percentage Mode
- Calculates each group's contribution to the year total
- Formula: `(group_count / year_total) * 100`
- Y-axis: 0-100%
- Useful for seeing relative distribution shifts over time

## Tab 3: Site Statistics

### Purpose
Analyze site-level treatment statistics and identify largest/smallest drone sites.

### Key Features
- **Show selector** - Average, Largest, or Smallest site sizes
- **Year Range selectors** - Start Year and End Year (2010-2025)
- **Site Rankings tables** - Top 10 largest and smallest individual treatments
- **Zone filter** - Shared with other tabs
- **Facility/FOS filters** - Shared with other tabs
- **Group By selector** - Facility or FOS

### Data Flow
1. Query individual treatment records via `get_sitecode_data()`:
   - Query both archive and current tables for drone treatments
   - Join with `loc_breeding_sites` for zone, foreman, prehatch info
   - Each row = one treatment instance with recorded acres
   - Filter by year range and user selections

2. Aggregate via `get_site_stats_data()`:
   - Group by facility or FOS
   - Calculate statistics:
     - `avg_site_acres`: Mean of all treatment acres in group
     - `min_site_acres`: Smallest single treatment in group
     - `max_site_acres`: Largest single treatment in group
     - `n_treatments`: Total treatment count

### Visualization
- **Horizontal bar chart** (coord_flip)
- **X-axis**: Facility or FOS name
- **Y-axis**: Acres (average, min, or max based on selection)
- Bars sorted by value (largest at top)
- Colors match Current Progress tab

### Site Rankings Tables
- **Largest Sites Table**: Top 10 treatments by acres
- **Smallest Sites Table**: Top 10 smallest treatments (filters out 0 acres)
- Columns: Sitecode, Treated Acres, Facility, Material, Year
- Shows individual treatment instances, not aggregated

## Code Organization

### File Structure
```
drone/
├── app.R                           # Main app - UI orchestration and server logic
├── ui_helper.R                     # UI definition (drone_ui function)
├── data_functions.R                # All database queries and data processing
├── display_functions.R             # Visualization helpers and color mapping
├── historical_functions.R          # Historical trends data and plotting
└── NOTES.md                        # This file
```

### Design Principles
1. **Separation of Concerns**: UI, data, and display logic are separate
2. **Refresh Button Pattern**: No data loads until user clicks "Refresh Data"
3. **Input Isolation**: All inputs captured via `refresh_inputs()` when button clicked
4. **Reusable Functions**: Color mapping, zone grouping, and filtering are modular
5. **Consistent Patterns**: All tabs follow same filter/process/display flow

### Key Functions by File

#### app.R
- `refresh_inputs()` - Captures all UI inputs when refresh clicked (prevents reactive reruns)
- `raw_data()` - Loads and processes data ONLY when refresh button clicked
- `processed_data()` - Accesses current progress processed data
- `sitecode_data()` - Loads site statistics data when refresh clicked
- Output renderers for all three tabs

#### data_functions.R
- `load_raw_data()` - Queries drone sites and treatments from database
- `apply_data_filters()` - Applies facility, FOS, and prehatch filters
- `get_sitecode_data()` - Individual treatment records for site statistics
- `get_site_stats_data()` - Aggregates treatment data by group

#### display_functions.R
- `create_zone_groups()` - Creates combined group labels for zone separation
- `get_visualization_colors()` - Returns appropriate colors for grouping type
- `process_current_data()` - Processes filtered data for current progress display

#### historical_functions.R
- `get_historical_raw_data()` - Queries historical treatment data
- `get_historical_processed_data()` - Processes and aggregates historical data
- `create_historical_plot()` - Generates historical trends visualization

## Common Data Patterns


### Zone Mapping
- **P1** = Zone '1' (Primary zone)
- **P2** = Zone '2' (Secondary zone)

### FOS (Field Operations Supervisor) Assignment
- Mapped through `gis_sectcode.fosarea` → `employee_list.emp_num`
- Short names (e.g., "JOHN") mapped to employee numbers in db_helpers
- Only active field supervisors included

### Sitecode Patterns
- **Standard format**: `190208-003` (facility + '-' + number)
- **Directional format**: `191102W010` (facility + direction + number)
- Join pattern must handle both cases

### Material Codes
- **Bti** products: Standard larvicide
- **Monomolecular films**: Surface treatment
- Treatment duration varies by material (from mattype_list_targetdose)

## Refresh Button Pattern

### Why It's Critical
Without the refresh button pattern, changing ANY filter (prehatch, zone, facility) would immediately trigger database queries, causing:
- Slow UI performance
- Accidental heavy queries
- Multiple rapid-fire database hits

### How It Works
1. **On app load**: Only UI options (facility names, FOS names) are loaded from db_helpers
2. **User adjusts filters**: No database queries run - just UI state changes
3. **User clicks "Refresh Data"**: 
   - `refresh_inputs()` captures ALL current input values using `isolate()`
   - `raw_data()` eventReactive executes, using captured inputs
   - `sitecode_data()` eventReactive executes, using captured inputs
   - All data processing happens with frozen input values
4. **User changes filters again**: Charts stay the same (using frozen data)
5. **User clicks "Refresh Data" again**: New query with new input values

### Key Implementation Details
```r
# Capture inputs when refresh clicked
refresh_inputs <- eventReactive(input$refresh, {
  list(
    zone_filter = isolate(input$zone_filter),
    facility_filter = isolate(input$facility_filter),
    # ... all other inputs
  )
})

# Load data ONLY when refresh clicked, using captured inputs
raw_data <- eventReactive(input$refresh, {
  inputs <- refresh_inputs()
  # Use inputs$zone_filter, NOT input$zone_filter
  load_and_process_data(zone_filter = inputs$zone_filter, ...)
})

# Output renderers use req() AND captured inputs
output$plot <- renderPlot({
  req(input$refresh)  # Don't run until refresh clicked
  inputs <- refresh_inputs()  # Get frozen input values
  # Use inputs$*, NOT input$*
})
```

## Key Takeaways

1. **Refresh Button Pattern**: Essential for performance - NO data queries until refresh clicked
2. **Zone and Foreman Must Be Joined**: Treatment tables don't have these columns, must join from loc_breeding_sites and gis_sectcode
3. **Site Enddate Filtering**: Always filter `enddate IS NULL` to exclude closed sites
4. **FOS = Jurisdictional Assignment**: Use site foreman (from gis_sectcode), not treatment foreman
5. **Sitecode Formats Vary**: Join logic must handle both '-' and directional suffixes (N, S, E, W)
6. **Treatment Status**: Calculate from inspdate + effect_days, compare to current_date
7. **Zone Separation**: When both P1 and P2 selected, show separately with alpha transparency
8. **COUNT DISTINCT**: Always use `COUNT(DISTINCT sitecode)` for unique site counts
9. **Input Isolation**: Use `refresh_inputs()` captured values, NOT live `input$*` values
10. **Color Consistency**: Use db_helpers for facility and FOS color mapping across all visualizations

---

## SQL Queries Reference

This section documents all SQL queries used in the drone app for reference and troubleshooting.

### 1. Current Drone Sites Query
**Function**: `load_raw_data()` in `data_functions.R`  
**Purpose**: Load active drone sites with zone and FOS information

```sql
SELECT b.sitecode, b.facility, b.acres, b.prehatch, b.drone, 
       CASE 
         WHEN e.emp_num IS NOT NULL AND e.active = true THEN sc.fosarea
         ELSE NULL
       END as foreman, 
       sc.zone,
       left(b.sitecode,7) as sectcode
FROM public.loc_breeding_sites b
LEFT JOIN public.gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
  AND e.emp_type = 'FieldSuper' 
  AND e.active = true
WHERE (b.drone IN ('Y','M','C') OR b.air_gnd = 'D')
AND b.enddate IS NULL
```

**Key Points**:
- Filters for active drone sites only (`enddate IS NULL`)
- Joins with gis_sectcode to get zone and FOS area
- Joins with employee_list to get active field supervisors
- Handles multiple sitecode formats with `left(sitecode,7)`

### 2. Current Treatments Query
**Function**: `load_raw_data()` in `data_functions.R`  
**Purpose**: Load current drone treatments with effectiveness duration

```sql
SELECT t.sitecode, t.facility, t.inspdate, t.matcode, t.acres as treated_acres, t.foreman, m.effect_days
FROM public.dblarv_insptrt_current t
LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
WHERE (t.airgrnd_plan = 'D' OR t.action = 'D')
```

**Key Points**:
- Filters drone treatments: `airgrnd_plan = 'D'` OR `action = 'D'`
- Joins with mattype_list_targetdose for treatment duration
- Uses actual recorded treated acres from treatment records

### 3. Individual Treatment Records Query (Site Statistics)
**Function**: `get_sitecode_data()` in `data_functions.R`  
**Purpose**: Get individual treatment records for site statistics analysis

```sql
WITH treatment_data AS (
    SELECT 
        t.facility, 
        t.sitecode, 
        t.inspdate, 
        t.matcode, 
        t.amts as amount_used,
        t.acres as recorded_acres,
        t.foreman as treatment_foreman,
        EXTRACT(YEAR FROM t.inspdate) as year
    FROM public.dblarv_insptrt_current t
    WHERE (t.airgrnd_plan = 'D' OR t.action = 'D')
        AND EXTRACT(YEAR FROM t.inspdate) BETWEEN [start_year] AND [end_year]
        AND t.matcode IS NOT NULL
        AND t.acres IS NOT NULL
        AND t.acres > 0
    
    UNION ALL
    
    SELECT 
        t.facility, 
        t.sitecode, 
        t.inspdate, 
        t.matcode, 
        t.amts as amount_used,
        t.acres as recorded_acres,
        t.foreman as treatment_foreman,
        EXTRACT(YEAR FROM t.inspdate) as year
    FROM public.dblarv_insptrt_archive t
    WHERE t.action = 'D'
        AND EXTRACT(YEAR FROM t.inspdate) BETWEEN [start_year] AND [end_year]
        AND t.matcode IS NOT NULL
        AND t.acres IS NOT NULL
        AND t.acres > 0
),
-- Get deduplicated location data for prehatch info and zone
location_data AS (
    SELECT DISTINCT ON (b.sitecode, b.facility)
        b.sitecode, 
        b.facility, 
        b.prehatch,
        CASE 
          WHEN e.emp_num IS NOT NULL AND e.active = true THEN sc.fosarea
          ELSE NULL
        END as foreman,
        sc.zone
    FROM public.loc_breeding_sites b
    LEFT JOIN public.gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
    LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
      AND e.emp_type = 'FieldSuper' 
      AND e.active = true
    ORDER BY b.sitecode, b.facility
)
-- Return individual treatment records, not aggregated
SELECT DISTINCT
    t.sitecode,
    t.facility,
    t.recorded_acres as acres,
    t.inspdate,
    t.year,
    t.matcode,
    l.zone,
    l.foreman,
    l.prehatch,
    -- Add a unique treatment ID for identification
    ROW_NUMBER() OVER (ORDER BY t.sitecode, t.inspdate) as treatment_id
FROM treatment_data t
LEFT JOIN location_data l ON t.sitecode = l.sitecode AND t.facility = l.facility
ORDER BY t.sitecode, t.inspdate DESC
```

**Key Points**:
- Combines current and archive tables with UNION ALL
- Uses CTEs for better organization and performance
- Filters out zero-acre treatments
- Joins with location data for zone and FOS information
- Returns individual treatment records, not aggregated

### 4. Historical Archive Data Query
**Function**: `get_historical_raw_data()` in `historical_functions.R`  
**Purpose**: Load historical treatment data from archive

```sql
SELECT facility, sitecode, inspdate, action, matcode, amts as amount, acres as treated_acres
FROM public.dblarv_insptrt_archive
WHERE action = 'D'
AND EXTRACT(YEAR FROM inspdate) BETWEEN [start_year] AND [end_year]
```

### 5. Historical Current Data Query
**Function**: `get_historical_raw_data()` in `historical_functions.R`  
**Purpose**: Load recent treatment data from current table

```sql
SELECT facility, sitecode, inspdate, action, airgrnd_plan, matcode, amts as amount, acres as treated_acres
FROM public.dblarv_insptrt_current
WHERE (airgrnd_plan = 'D' OR action = 'D')
AND EXTRACT(YEAR FROM inspdate) BETWEEN [start_year] AND [end_year]
```

### 6. Site Information for Historical Data
**Function**: `get_historical_raw_data()` in `historical_functions.R`  
**Purpose**: Get site characteristics for historical treatments

```sql
SELECT b.sitecode, b.acres, b.facility, b.prehatch, b.drone, b.air_gnd,
       CASE 
         WHEN e.emp_num IS NOT NULL AND e.active = true THEN sc.fosarea
         ELSE NULL
       END as foreman, 
       sc.zone,
       left(b.sitecode,7) as sectcode
FROM public.loc_breeding_sites b
LEFT JOIN public.gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
  AND e.emp_type = 'FieldSuper' 
  AND e.active = true
WHERE b.sitecode IN ([sitecode_list])
AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
```

**Key Points**:
- Filters for active sites (`enddate IS NULL` or future enddate)
- Same join pattern as current sites query
- Uses IN clause with dynamic sitecode list

### 7. Facility Lookup Query
**Function**: `get_facility_lookup()` in `shared/db_helpers.R`  
**Purpose**: Map facility abbreviations to full names

```sql
SELECT DISTINCT 
  abbrv as short_name,
  city as full_name
FROM public.gis_facility
WHERE abbrv NOT IN ('OT', 'MF', 'AW', 'RW')
ORDER BY abbrv
```

**Key Points**:
- Excludes special facility codes
- Maps abbreviations like "PNA" to full city names
- Used for display name mapping throughout the app

### 8. Simple Site Existence Check
**Function**: `get_historical_raw_data()` in `historical_functions.R`  
**Purpose**: Verify loc_breeding_sites table connectivity

```sql
SELECT b.sitecode, b.facility 
FROM public.loc_breeding_sites b
WHERE b.sitecode IN ([sitecode_list])
LIMIT 10
```

## SQL Query Patterns

### Common Join Pattern
Most queries use this pattern to connect treatment records with site information:

```sql
FROM treatment_table t
LEFT JOIN public.loc_breeding_sites b ON t.sitecode = b.sitecode AND t.facility = b.facility
LEFT JOIN public.gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
  AND e.emp_type = 'FieldSuper' 
  AND e.active = true
```

### Drone Treatment Filters
Consistent pattern across all queries:
- **Current table**: `(airgrnd_plan = 'D' OR action = 'D')`
- **Archive table**: `action = 'D'`
- **Site designation**: `(drone IN ('Y','M','C') OR air_gnd = 'D')`

### Site Status Filtering
Always filter active sites: `enddate IS NULL` or `enddate > CURRENT_DATE`

### Year Filtering
Standard pattern: `EXTRACT(YEAR FROM inspdate) BETWEEN [start] AND [end]`

---