# Drone Sites Treatment Tracking - Technical Notes

## Overview
This Shiny app tracks drone-treated mosquito breeding sites across three perspectives:
1. **Current Progress** - Sites with active/expiring treatments, aggregated by facility, FOS, or section
2. **Historical Trends** - Multi-year trends in drone treatments (sites, treatments, or acres)
3. **Site Statistics** - Site-level statistics showing average, largest, and smallest treated sites

## Data Sources

### Core Database Tables and Columns

#### Primary Treatment Tables
- **`public.dblarv_insptrt_current`** - Active larval treatment records
  - **Key Columns**: `sitecode`, `facility`, `inspdate`, `matcode`, `foreman`, `action`, `airgrnd_plan`, `amts`
  - **Drone Filter**: `airgrnd_plan = 'D'` OR `action = 'D'`
  - **Missing Columns**: Does NOT contain `zone`, `enddate`, `prehatch` - must join for these
  - **Data Quality**: Contains ongoing treatments, some may be planned vs executed
  
- **`public.dblarv_insptrt_archive`** - Historical larval treatment records  
  - **Key Columns**: `sitecode`, `facility`, `inspdate`, `matcode`,  `foreman`, `action`, `amts`
  - **Drone Filter**: `action = 'D'` (archive table only uses action column)
  - **Missing Columns**: Does NOT contain `zone`, `enddate`, `prehatch`, `airgrnd_plan` - must join for these
  - **Data Quality**: Historical closed treatments, generally more reliable than current

#### Essential Supporting Tables
- **`public.loc_breeding_sites`** - Site master data
  - **Key Columns**: 
    - `sitecode`, `facility` (composite key)
    - `acres` (site capacity, not treated acres)
    - `enddate` (**CRITICAL**: NULL = active site, NOT NULL = closed site)
    - `prehatch` (boolean: site treats pre-hatch mosquitoes)
    - `drone` (values: 'Y', 'M', 'C' for drone-capable sites)
    - `air_gnd` (alternate drone designation, value: 'D')
  - **Critical Join Logic**: **MUST filter `enddate IS NULL`** or risk including closed sites
  - **Data Quality**: Multiple rows per sitecode possible with different enddates
  - **Usage**: Source of truth for site characteristics and active status
  
- **`public.gis_sectcode`** - Geographic section mapping and zone assignments
  - **Key Columns**:
    - `sectcode` (7-character section identifier, e.g. '191031N')
    - `zone` (values: '1' = P1, '2' = P2)
    - `fosarea` (FOS employee number assignment)
  - **Join Pattern**: `LEFT JOIN public.gis_sectcode sc ON left(sitecode,7) = sc.sectcode`
  - **Sitecode Formats Handled**:
    - Standard: `190208-003` → sectcode `1902080`
    - Directional: `191102W010` → sectcode `1911020` 
  - **Data Quality**: Authoritative source for zone and FOS assignments
  
- **`public.mattype_list_targetdose`** - Material effectiveness and dosage data
  - **Key Columns**:
    - `matcode` (material identifier, joins to treatment tables)
    - `effect_days` (treatment effectiveness duration in days)
  - **Default Logic**: When `effect_days IS NULL`, assume 14 days
  - **Usage**: Calculate treatment end dates: `inspdate + effect_days`
  
### Data Collection Strategy

#### Key Join Patterns Used Throughout Application

**Standard Treatment-to-Site Join Pattern:**
```sql
-- Connect treatment records with site information
FROM public.dblarv_insptrt_current t
LEFT JOIN public.loc_breeding_sites b ON t.sitecode = b.sitecode AND t.facility = b.facility
LEFT JOIN public.gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
  AND e.emp_type = 'FieldSuper' AND e.active = true
LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
WHERE (b.enddate IS NULL OR b.enddate > CURRENT_DATE)  -- Active sites only
  AND (t.airgrnd_plan = 'D' OR t.action = 'D')         -- Drone treatments only
```

**Note**: See "SQL Queries Reference" section below for complete, up-to-date query implementations.

#### Critical Data Integration Notes

**Sitecode Format Handling:**
- **Standard Format**: `190208-003` (7-digit section + dash + site num)
- **Directional Format**: `191102W010` (6-digit + direction + site num)
- **Join Logic**: `left(sitecode,7)` extracts section code for both formats
- **Example**: Both `190208-003` and `191102W010` → sectcodes `1902080` and `1911020`

**Treatment vs Site Acres:**
- **Site Acres**: `loc_breeding_sites.acres` = site capacity/potential treatment area
- **Treated Acres**: `dblarv_insptrt_*.acres` = actual area treated in specific application
- **Usage**: Site acres for capacity analysis, treated acres for actual treatment metrics

**Foreman Assignment Logic:**
- **Site Foreman**: `gis_sectcode.fosarea` → jurisdictional responsibility
- **Treatment Foreman**: `dblarv_insptrt_*.foreman` → who performed the treatment  
- **App Standard**: Uses site foreman (jurisdictional) for filtering and grouping

**Zone Assignment:**
- **P1 Sites**: `gis_sectcode.zone = '1'` (Primary zone)
- **P2 Sites**: `gis_sectcode.zone = '2'` (Secondary zone)
- **Missing Zones**: Some sites may not have zone assignments in gis_sectcode

**Treatment Effectiveness Calculation:**
- **Formula**: `treatment_end_date = inspdate + COALESCE(effect_days, 14)`
- **Active Status**: `treatment_end_date >= CURRENT_DATE` (or analysis_date)
- **Material Lookup**: Join `mattype_list_targetdose` for specific `effect_days`

### Chart Type Options

**Stacked Bar Chart (Default)**:
- **Best for**: Showing total volume and individual contributions
- **Usage**: Cumulative view of all groups combined
- **Visual**: Traditional stacked bars, groups stack vertically

**Grouped Bar Chart**:
- **Best for**: Direct comparison between groups
- **Usage**: Side-by-side comparison of facilities/FOS performance
- **Visual**: Bars grouped side-by-side for each time period

**Line Chart**:
- **Best for**: Trend analysis and pattern identification
- **Usage**: Tracking changes over time for each group
- **Visual**: Connected lines with data points for each group

**Area Chart**:
- **Best for**: Visual emphasis of volume and trends
- **Usage**: Highlighting magnitude of treatment activity
- **Visual**: Filled areas under trend lines

**Step Chart**:
- **Best for**: Discrete changes and threshold analysis
- **Usage**: Showing step-wise changes in treatment levels
- **Visual**: Step-wise lines connecting data points

### Chart Type Behavior

**With Percentages Enabled**:
- **Stacked Bar & Area**: Shows percentage distribution (0-100%)
- **Line Chart**: Shows percentage trends over time
- **Grouped Bar**: Percentages not applicable (disabled)

**With Zone Separation**:
- All chart types support P1/P2 zone differentiation
- Alpha transparency applied: P1 (solid), P2 (faded)
- Colors remain consistent across chart types

**Time Period Compatibility**:
- All chart types work with both yearly and weekly periods
- Weekly charts automatically adjust text sizing for readability
- Line charts particularly effective for weekly trend analysis

### Time Period Options
**Yearly View (Default)**:
- Shows one bar per year in the selected range
- Useful for long-term trend analysis
- Better for multi-year comparisons
- Less cluttered display

**Weekly View**:
- Shows one bar per week in the selected range
- Provides detailed seasonal and short-term patterns
- Useful for identifying weekly treatment patterns
- More detailed but potentially dense for large date ranges
- Week format: "YYYY-W##" (e.g., "2024-W15" for week 15 of 2024)



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
### Key Functions by File

#### app.R
- `refresh_inputs()` - Captures all UI inputs when refresh clicked (prevents reactive reruns)
- `raw_data()` - Loads and processes data ONLY when refresh button clicked
- `processed_data()` - Accesses current progress processed data from raw_data result
- `sitecode_data()` - Loads site statistics data when refresh clicked
- `server()` - Main server function with output renderers for all three tabs

#### data_functions.R
- `load_raw_data()` - Queries drone sites and treatments from database with analysis date support
- `apply_data_filters()` - Applies facility, FOS, and prehatch filters to processed data
- `get_sitecode_data()` - Individual treatment records for site statistics with zone/FOS data
- `get_site_stats_data()` - Aggregates treatment data by group for site statistics

#### display_functions.R
- `create_zone_groups()` - Creates combined group labels for zone separation
- `get_visualization_colors()` - Returns appropriate colors for grouping type with zone support
- `process_current_data()` - Processes filtered data for current progress display with expiring logic
- `create_historical_plot()` - Generates historical trends visualization with multiple chart types

#### historical_functions.R
- `create_historical_data()` - Creates historical dataset with time period and zone separation logic
- `get_historical_processed_data()` - Processes and aggregates historical data with combined_group handling
- `get_shared_historical_data()` - Queries shared historical treatment data

#### ui_helper.R
- `drone_ui()` - Main UI definition function with tabPanel layout and all input controls

---

## Data Collection and Aggregation Logic

### Overview of Data Processing Pipeline
The drone app follows a consistent pattern for data collection and aggregation across all three tabs. Data processing is done **outside of SQL** - SQL queries return raw records which are then aggregated using R/dplyr operations.

### Core Aggregation Methods

#### Total Sites vs Total Acres Calculation
**IMPORTANT**: Both total sites and total acres calculations are performed **outside of SQL** using R aggregation functions.

**Total Sites Calculation:**
```r
# Count UNIQUE sites (not treatments)
total_sites <- drone_sites %>%
  group_by(!!sym(group_col)) %>%
  summarize(total_sites = n(), .groups = "drop")  # n() counts rows = sites

# For zone separation
total_sites <- drone_sites %>%
  group_by(combined_group, zone) %>%
  summarize(total_sites = n(), .groups = "drop")
```

**Total Acres Calculation:**
```r
# Sum acres from SITE records (loc_breeding_sites.acres)
total_acres <- drone_sites %>%
  group_by(!!sym(group_col)) %>%
  summarize(total_acres = sum(acres, na.rm = TRUE), .groups = "drop")
```

**Treatment Sites vs Treatment Acres:**
```r
# Count unique SITES with active treatments (not treatment instances)
active_sites <- drone_treatments %>%
  filter(is_active) %>%
  group_by(!!sym(group_col)) %>%
  summarize(active_sites = n_distinct(sitecode), .groups = "drop")

# Sum TREATED acres from treatment records (treated_acres)
active_acres <- drone_treatments %>%
  filter(is_active) %>%
  group_by(!!sym(group_col)) %>%
  summarize(active_acres = sum(acres, na.rm = TRUE), .groups = "drop")
```

#### Historical Metrics Aggregation
**All historical aggregations are done in R after SQL returns raw records:**

**Time Period Creation**
```r
# Weekly aggregation
if (hist_time_period == "weekly") {
  all_data <- all_data %>%
    mutate(
      year = year(inspdate),
      week = week(inspdate),
      time_period = paste0(year, "-W", sprintf("%02d", week)),
      time_sort = year * 100 + week
    )
} else {
  # Yearly aggregation (default)
  all_data <- all_data %>%
    mutate(
      time_period = as.character(year),
      time_sort = year
    )
}
```

**Sites Metric:**
```r
# Count unique sites treated per group per time period
if (hist_display_metric == "sites") {
  results <- all_data %>%
    group_by(!!group_var, !!time_var, time_sort) %>%
    summarize(count = n_distinct(sitecode), .groups = "drop")
}
```

**Treatments Metric:**
```r
# Count total treatment instances per group per time period
if (hist_display_metric == "treatments") {
  results <- all_data %>%
    group_by(!!group_var, !!time_var, time_sort) %>%
    summarize(count = n(), .groups = "drop")  # n() counts all treatment records
}
```

**Acres Metric:**
```r
# Sum treated acres from treatment records per group per time period
if (hist_display_metric == "acres") {
  results <- all_data %>%
    group_by(!!group_var, !!time_var, time_sort) %>%
    summarize(count = sum(treated_acres, na.rm = TRUE), .groups = "drop")
}
```

#### Site Statistics Aggregation
**Site statistics are calculated from individual treatment records:**

```r
# Calculate statistics from individual treatment records
site_stats <- sitecode_data_raw %>%
  group_by(!!sym(group_col)) %>%
  summarize(
    avg_site_acres = mean(acres, na.rm = TRUE),      # Average of all treatment acres
    min_site_acres = min(acres, na.rm = TRUE),       # Smallest single treatment
    max_site_acres = max(acres, na.rm = TRUE),       # Largest single treatment
    n = n_distinct(sitecode),                        # Count unique sites
    n_treatments = n(),                              # Count total treatments
    .groups = "drop"
  )
```

### Key Aggregation Principles

1. **DISTINCT Counting**: Always use `n_distinct(sitecode)` when counting sites to avoid double-counting
2. **Treatment vs Site Acres**: 
   - **Site acres**: From `loc_breeding_sites.acres` (site capacity)
   - **Treatment acres**: From `dblarv_insptrt_*.acres` (actual treated area)
3. **R-based Aggregation**: All calculations use R/dplyr `group_by()` and `summarize()`
4. **Zone Separation**: When zones are separated, aggregation includes both `group` and `zone` columns
5. **Missing Values**: Use `na.rm = TRUE` in all sum/mean calculations

### Data Sources vs Aggregation Separation

**SQL Queries**: Return raw, individual records
- `load_raw_data()`: Returns individual site records and treatment records
- `get_sitecode_data()`: Returns individual treatment instances
- `get_shared_historical_data()`: Returns individual historical treatments

**R Aggregation**: Processes raw records into summaries
- `process_current_data()`: Aggregates sites and treatments by group
- `get_site_stats_data()`: Calculates statistics from treatment records
- Historical functions: Aggregate treatments by group and year

### Treatment Status Logic (R-based)
**Active Treatment Calculation:**
```r
drone_treatments <- drone_treatments %>%
  mutate(
    inspdate = as.Date(inspdate),
    effect_days = ifelse(is.na(effect_days), 14, effect_days),  # Default 14 days
    treatment_end_date = inspdate + effect_days,
    is_active = treatment_end_date >= current_date
  )
```

**Expiring Treatment Calculation:**
```r
drone_treatments <- drone_treatments %>%
  mutate(
    is_expiring = is_active & 
                  treatment_end_date >= expiring_start_date & 
                  treatment_end_date <= expiring_end_date
  )
```

## SQL Queries Reference

This section documents all SQL queries used in the drone app for reference and troubleshooting.

### 1. Current Drone Sites Query
**Function**: `load_raw_data()` in `data_functions.R`  
**Purpose**: Load active drone sites with zone and FOS information from authoritative gis_sectcode source

```sql
SELECT 
    sc.sectcode || '-' || '001' as sitecode,  -- Generate sitecode from sectcode
    sc.facility,
    sc.acres,
    sc.prehatch,
    sc.drone_type,
    CASE 
        WHEN e.emp_num IS NOT NULL AND e.active = true THEN sc.fosarea
        ELSE NULL
    END as foreman,
    sc.zone,
    sc.sectcode
FROM public.gis_sectcode sc
LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
    AND e.emp_type = 'FieldSuper' 
    AND e.active = true
WHERE sc.drone_type IN ('Y', 'M', 'C')
    AND sc.active = true
    AND (sc.enddate IS NULL OR sc.enddate > CURRENT_DATE)
```

**Key Points**:
- Uses `gis_sectcode` as the authoritative source for site data
- Filters for active drone sites (`active = true`, `enddate IS NULL`)
- Gets zone, FOS area, prehatch, and acres directly from gis_sectcode
- Joins with employee_list to validate active field supervisors
- Generates sitecode from sectcode for compatibility

### 2. Current Treatments Query
**Function**: `load_raw_data()` in `data_functions.R`  
**Purpose**: Load current drone treatments with zone/FOS data and effectiveness duration

```sql
SELECT 
    t.sitecode, 
    t.facility, 
    t.inspdate, 
    t.matcode, 
    t.acres as treated_acres, 
    t.foreman as treatment_foreman,
    m.effect_days,
    sc.zone,
    CASE 
        WHEN e.emp_num IS NOT NULL AND e.active = true THEN sc.fosarea
        ELSE NULL
    END as site_foreman,
    sc.prehatch,
    sc.acres as site_acres
FROM public.dblarv_insptrt_current t
LEFT JOIN public.gis_sectcode sc ON left(t.sitecode,7) = sc.sectcode
LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
    AND e.emp_type = 'FieldSuper' 
    AND e.active = true
LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
WHERE (t.airgrnd_plan = 'D' OR t.action = 'D')
    AND sc.drone_type IN ('Y', 'M', 'C')
    AND (sc.enddate IS NULL OR sc.enddate > CURRENT_DATE)
```

**Key Points**:
- Filters drone treatments: `airgrnd_plan = 'D'` OR `action = 'D'`
- Joins with gis_sectcode to get zone, FOS, prehatch data (authoritative source)
- Joins with employee_list to validate active field supervisors
- Joins with mattype_list_targetdose for treatment duration
- Filters for active drone sites using gis_sectcode.enddate
- Distinguishes between treatment_foreman (who applied) and site_foreman (jurisdictional)

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
        AND t.inspdate <= '[analysis_date]'
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
        AND t.inspdate <= '[analysis_date]'
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
- **Analysis date filter**: Excludes treatments recorded after the specified analysis date
- Joins with location data for zone and FOS information
- Returns individual treatment records, not aggregated

### 4. Historical Treatment Data Query (Combined Archive + Current)
**Function**: `get_shared_historical_data()` in `historical_functions.R`  
**Purpose**: Load historical treatment data with zone/FOS information from both archive and current

```sql
-- Historical treatments from archive
SELECT 
    t.facility, 
    t.sitecode, 
    t.inspdate, 
    t.action, 
    t.matcode, 
    t.amts as amount, 
    t.acres as treated_acres,
    sc.zone,
    CASE 
        WHEN e.emp_num IS NOT NULL AND e.active = true THEN sc.fosarea
        ELSE NULL
    END as foreman,
    sc.prehatch,
    sc.acres as site_acres
FROM public.dblarv_insptrt_archive t
LEFT JOIN public.gis_sectcode sc ON left(t.sitecode,7) = sc.sectcode
LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
    AND e.emp_type = 'FieldSuper' 
    AND e.active = true
WHERE t.action = 'D'
    AND EXTRACT(YEAR FROM t.inspdate) BETWEEN ? AND ?
    AND t.inspdate <= ?
    AND sc.drone_type IN ('Y', 'M', 'C')
    AND (sc.enddate IS NULL OR sc.enddate > ?)

UNION ALL

-- Recent treatments from current
SELECT 
    t.facility, 
    t.sitecode, 
    t.inspdate, 
    t.action, 
    t.matcode, 
    t.amts as amount, 
    t.acres as treated_acres,
    sc.zone,
    CASE 
        WHEN e.emp_num IS NOT NULL AND e.active = true THEN sc.fosarea
        ELSE NULL
    END as foreman,
    sc.prehatch,
    sc.acres as site_acres
FROM public.dblarv_insptrt_current t
LEFT JOIN public.gis_sectcode sc ON left(t.sitecode,7) = sc.sectcode
LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
    AND e.emp_type = 'FieldSuper' 
    AND e.active = true
WHERE (t.airgrnd_plan = 'D' OR t.action = 'D')
    AND EXTRACT(YEAR FROM t.inspdate) BETWEEN ? AND ?
    AND t.inspdate <= ?
    AND sc.drone_type IN ('Y', 'M', 'C')
    AND (sc.enddate IS NULL OR sc.enddate > ?)
```

**Key Points**:
- Combines archive and current treatment tables with UNION ALL
- Uses gis_sectcode as authoritative source for zone, FOS, and site data
- Filters drone treatments by action/airgrnd_plan and drone_type
- Includes year range and analysis date filtering
- Joins with employee_list for active field supervisor validation
- Filters for active sites using gis_sectcode.enddate
- Returns both treatment data and site characteristics in single query

### 5. Facility Lookup Query
**Function**: `get_facility_lookup()` in `shared/db_helpers.R`  
**Purpose**: Map facility abbreviations to full names for display

```sql
SELECT DISTINCT 
    abbrv as short_name,
    city as full_name
FROM public.gis_facility
WHERE abbrv NOT IN ('OT', 'MF', 'AW', 'RW')
ORDER BY abbrv
```

**Key Points**:
- Maps facility codes (e.g., 'E', 'N', 'Sj') to full names (e.g., 'Eastern Hennepin')
- Excludes special facility codes from UI dropdowns
- Used throughout app for display name mapping
- Provides consistent facility naming across all tabs

### 6. FOS Employee Lookup Query