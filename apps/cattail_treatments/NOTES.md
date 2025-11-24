# Cattail Treatments - Technical Notes

## Overview
This Shiny app tracks cattail treatment progress at mosquito breeding sites across three perspectives:
1. **Progress Overview** - Sites with inspection status and treatment progress, aggregated by facility, FOS, or section
2. **Historical Analysis** - Multi-year trends in cattail inspections and treatments 
3. **Future Tabs** - Treatment planning and management (to be developed)

## Data Sources

### Core Database Tables and Columns

#### Primary Inspection Tables
- **`public.dblarv_insptrt_current`** - Active larval inspection records
  - **Key Columns**: `sitecode`, `inspdate`, `action`, `numdip`, `matcode`, `mattype`, `amts`, `acres`
  - **Cattail Filter**: `action = '9'` (inspection) with `numdip` determining treatment need
  - **Missing Columns**: Does NOT contain `zone`, `enddate`, `facility` - must join for these
  - **Data Quality**: Contains ongoing inspection records with cattail dip counts
  
- **`public.dblarv_insptrt_archive`** - Historical larval inspection/treatment records  
  - **Key Columns**: `sitecode`, `inspdate`, `action`, `numdip`, `matcode`, `mattype`, `amts`, `acres`
  - **Cattail Filter**: `action = '9'` (inspection) OR `action IN ('3', 'A')` (treatments)
  - **Missing Columns**: Does NOT contain `zone`, `enddate`, `facility` - must join for these
  - **Data Quality**: Historical closed inspection/treatment records, generally more reliable

#### Primary Treatment Tables  
- **`public.dblarv_insptrt_current`** - Active larval treatment records
  - **Key Columns**: `sitecode`, `inspdate`, `action`, `matcode`, `mattype`, `amts`, `acres`
  - **Treatment Filter**: `action IN ('3', 'A')` with cattail material codes
  - **Treatment Actions**: 
    - `action = '3'` - Standard treatment application
    - `action = 'A'` - Alternative treatment application
  - **Data Quality**: Contains ongoing treatments, some may be planned vs executed
  
- **`public.dblarv_insptrt_archive`** - Historical larval treatment records  
  - **Same structure as current table**
  - **Data Quality**: Historical closed treatments, generally more reliable than current

#### Essential Supporting Tables
- **`public.loc_breeding_sites`** - Site master data
  - **Key Columns**: 
    - `sitecode` (unique site identifier)
    - `acres` (site capacity, not treated acres)
    - `enddate` (**CRITICAL**: NULL = active site, NOT NULL = closed site)
    - `air_gnd` (treatment method designation) 
    - `drone` (values: 'Y', 'M', 'C' for drone-capable sites)
    - `geom` (PostGIS geometry data for mapping, UTM projection)
  - **Critical Join Logic**: **MUST filter `enddate IS NULL`** or risk including closed sites
  - **Data Quality**: Single row per sitecode for active sites
  - **Usage**: Source of truth for site characteristics and active status
  
- **`public.gis_sectcode`** - Geographic section mapping and zone assignments
  - **Key Columns**:
    - `sectcode` (7-character section identifier, e.g. '191031N')
    - `facility` (facility short code)
    - `zone` (values: '1' = Zone 1, '2' = Zone 2)
    - `fosarea` (FOS employee number assignment)
  - **Join Pattern**: `LEFT JOIN public.gis_sectcode sc ON LEFT(sitecode,7) = sc.sectcode`
  - **Sitecode Formats Handled**:
    - Standard: `190208-003` → sectcode `1902080`
    - Directional: `191102W010` → sectcode `1911020` 
  - **Data Quality**: Authoritative source for facility, zone and FOS assignments

- **`public.mattype_list_targetdose`** - Material codes and target programs
  - **Key Columns**:
    - `matcode` (material identifier, joins to inspection/treatment tables)
    - `prgassign_default` (target program assignment)
    - **Cattail Filter**: `prgassign_default = 'Cat'` identifies cattail materials
  - **Usage**: Validates that treatments use cattail-appropriate materials
  - **Treatment Logic**: Used to identify valid cattail treatment materials

### Data Collection Strategy

#### Key Join Patterns Used Throughout Application

**Standard Inspection-to-Site Join Pattern:**
```sql
-- Connect inspection records with site information
FROM public.dblarv_insptrt_current i
LEFT JOIN public.loc_breeding_sites b ON i.sitecode = b.sitecode
LEFT JOIN public.gis_sectcode sc ON LEFT(i.sitecode,7) = sc.sectcode
WHERE i.action = '9'  -- Action 9 = inspected
  AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)  -- Active sites only
```

**Standard Treatment-to-Site Join Pattern:**
```sql
-- Connect treatment records with cattail material validation
FROM public.dblarv_insptrt_current t
LEFT JOIN public.gis_sectcode sc ON LEFT(t.sitecode,7) = sc.sectcode
WHERE t.action IN ('3', 'A')  -- Actions 3/A = treatments
  AND t.matcode IN (
    SELECT matcode 
    FROM public.mattype_list_targetdose
    WHERE prgassign_default = 'Cat'
  )
```

**Note**: See "SQL Queries Reference" section below for complete, up-to-date query implementations.

#### Critical Data Integration Notes

**Sitecode Format Handling:**
- **Standard Format**: `190208-003` (7-digit section + dash + site num)
- **Directional Format**: `191102W010` (6-digit + direction + site num)
- **Join Logic**: `LEFT(sitecode,7)` extracts section code for both formats
- **Example**: Both `190208-003` and `191102W010` → sectcodes `1902080` and `1911020`

**Inspection vs Treatment Acres:**
- **Site Acres**: `loc_breeding_sites.acres` = site capacity/potential treatment area
- **Treated Acres**: `dblarv_insptrt_*.acres` = actual area treated in specific application
- **Usage**: Site acres for capacity analysis, treated acres for actual treatment metrics

**Cattail Treatment Determination:**
- **Inspection Logic**: `numdip > 0` indicates site needs cattail treatment
- **3-State System**: 
  - `under_threshold` (grey): `numdip = 0` - no treatment needed
  - `need_treatment` (red): `numdip > 0` - treatment required
  - `treated` (green): site had treatment applied after inspection showing need

**Treatment Timing Logic:**
- **Inspection Season**: Typically fall season of year N
- **Treatment Season**: Fall/winter of year N OR spring/summer of year N+1
- **Treatment Windows**:
  - Fall/Winter: September-December of inspection year
  - Spring/Summer: May-July of year after inspection
- **Year Mapping**: Spring/Summer treatments in year N+1 apply to year N inspections

**Zone Assignment:**
- **Zone 1**: `gis_sectcode.zone = '1'` 
- **Zone 2**: `gis_sectcode.zone = '2'`
- **Missing Zones**: Some sites may not have zone assignments in gis_sectcode

**Material Code Validation:**
- **Cattail Materials**: JOIN with `mattype_list_targetdose WHERE prgassign_default = 'Cat'`
- **Treatment Validation**: Only treatments using cattail materials are counted
- **Action Types**: Both action '3' and action 'A' represent valid treatments

## Tab 1: Progress Overview

### Purpose
Display current status of cattail sites with 3-state progress tracking (under threshold, need treatment, treated).

### Key Features
- **Value boxes** showing summary statistics (total sites inspected, sites needing treatment, sites treated, treatment completion percentage)
- **Stacked progress chart** showing 3-state breakdown by facility
- **Zone filters** - Zone 1, Zone 2, or both zones
- **Facility filters** - Individual facilities or all facilities combined
- **Date range controls** - Analysis date and year selection
- **Refresh button** - Manual data loading (no auto-refresh)

### Data Flow
1. Query cattail inspection sites via `load_cattail_data()`:
   - Filter for sites with cattail inspections (`action = '9'`)
   - Join with site characteristics and zone assignments
   - Get most recent inspection per site (DISTINCT ON sitecode)
   - Calculate treatment need based on `numdip > 0`

2. Query cattail treatments via `load_cattail_treatments()`:
   - Get treatments using cattail materials (`prgassign_default = 'Cat'`)
   - Include both action types ('3' and 'A') for treatments
   - Cover both treatment seasons (fall same year + spring next year)
   - Map treatments to inspection year using season logic

3. Aggregate via `aggregate_cattail_data()`:
   - Match treatments to sites that need treatment
   - Calculate 3-state metrics:
     - `under_threshold_sites`: Sites with `numdip = 0`
     - `need_treatment_sites`: Sites with `numdip > 0` and no treatment
     - `treated_sites`: Sites with `numdip > 0` and treatment applied
   - Calculate completion percentage

### Visualization
- **3-state value boxes**:
  - Grey box: Sites under threshold (no treatment needed)
  - Red box: Sites needing treatment 
  - Green box: Sites treated
  - Blue box: Treatment completion percentage
- **Stacked progress chart**:
  - Grey bars: Total sites under threshold
  - Red bars: Sites needing treatment (remaining)
  - Green bars: Sites treated
  - Colors consistent with 3-state system

### 3-State Logic Implementation
```r
# Sites categorized into 3 states based on inspection + treatment status
sites_data <- sites_data %>%
  mutate(
    final_status = case_when(
      sitecode %in% treated_sites ~ "treated",           # Green - has treatment
      treatment_status == "need_treatment" ~ "need_treatment",  # Red - needs treatment  
      treatment_status == "under_threshold" ~ "under_threshold", # Grey - no treatment needed
      TRUE ~ "under_threshold"
    ),
    state = final_status  # UI compatibility
  )
```

## Tab 2: Historical Analysis

### Purpose
Analyze multi-year trends in cattail inspections and treatment progress across facilities and zones.

### Key Features
- **Display Metric selector** - Sites Inspected, Sites Need Treatment, Number of Inspections
- **Time Period selector** - Monthly or Yearly aggregation
- **Chart Type selector** - Line Chart or Bar Chart
- **Date Range controls** - Start and end dates (defaults to 2022-01-01 to current)
- **Zone/Facility filters** - Same as Progress Overview tab
- **Group By selector** - Facility, Foreman (FOS), or Zone

### Data Flow
1. Query historical data via `get_historical_cattail_data()`:
   - Combine current and archive inspection tables
   - Filter for cattail inspections (`action = '9'`)
   - Join with site and zone information
   - Filter by selected date range

2. Process time periods:
   - **Monthly**: Use `floor_date(inspdate, "month")` for monthly grouping
   - **Yearly**: Use `floor_date(inspdate, "year")` for yearly grouping
   - Sort by time period for proper chart ordering

3. Aggregate via chart creation:
   - Group by selected dimension (facility, foreman, zone) and time period
   - Calculate metrics:
     - **Sites Inspected**: `n_distinct(sitecode)` - Count unique sites inspected
     - **Sites Need Treatment**: Filter `numdip > 0` then count distinct sites
     - **Number of Inspections**: `n()` - Count total inspection instances

### Visualization
- **Line Charts**: Show trends over time with distinct colors per group
- **Bar Charts**: Show volumes with grouped or stacked bars
- **Interactive plotly**: Hover tooltips with detailed information
- **Legend Management**: Proper legend titles based on chart type (color vs fill)
- **Date Formatting**: Appropriate axis labels based on time period selection

## Code Organization

### File Structure
```
cattail_treatments/
├── app.R                     # Main app - UI orchestration and server logic
├── data_functions.R          # All database queries and data processing
├── display_functions.R       # Visualization helpers (placeholder for future)
├── historical_functions.R    # Historical trends data and plotting
└── NOTES.md                  # This file
```

### Key Functions by File

#### app.R
- `cattail_values()` - Reactive value box calculations with 3-state metrics
- Server output renderers:
  - `output$progress_chart` - 3-state progress chart
  - `output$historical_chart` - Historical trend analysis
  - Value box outputs for all 3 states plus completion rate

#### data_functions.R
- `load_cattail_data()` - Main data loading with inspection sites and treatments
- `load_cattail_treatments()` - Treatment data loading with seasonal logic
- `aggregate_cattail_data()` - 3-state aggregation with treatment matching
- `filter_cattail_data()` - Apply UI filters (zone, facility, foreman, date range)

#### historical_functions.R
- `get_historical_cattail_data()` - Queries combined historical inspection data
- `create_historical_analysis_chart()` - Generates historical charts with multiple options
- UI helper functions for time period, chart type, and date range selectors

## Data Collection and Aggregation Logic

### Overview of Data Processing Pipeline
The cattail treatments app processes inspection and treatment data separately, then matches them in R to determine the final 3-state status.

### Core Aggregation Methods

#### 3-State Sites Calculation
**All calculations performed in R using dplyr after SQL returns raw records:**

**Inspection Sites Classification:**
```r
# Classify sites based on cattail dip count (numdip)
cattail_sites <- cattail_sites %>%
  mutate(
    treatment_status = case_when(
      numdip > 0 ~ "need_treatment",    # Red - cattail found
      numdip == 0 ~ "under_threshold", # Grey - no cattail
      TRUE ~ "under_threshold"         # Default to safe state
    )
  )
```

**Treatment Matching Logic:**
```r
# Match treatments to sites for current inspection year
treated_sites <- treatments %>%
  filter(inspection_year == current_year) %>%  # Match to inspection year
  pull(sitecode) %>%
  unique()

# Update final status based on treatment application
sites_data <- sites_data %>%
  mutate(
    final_status = case_when(
      sitecode %in% treated_sites ~ "treated",              # Green - treated
      treatment_status == "need_treatment" ~ "need_treatment", # Red - still needs treatment
      treatment_status == "under_threshold" ~ "under_threshold", # Grey - no treatment needed
      TRUE ~ "under_threshold"
    ),
    state = final_status  # For UI compatibility
  )
```

**Progress Metrics:**
```r
# Calculate 3-state summary metrics
total_summary <- data.frame(
  under_threshold_sites = sum(sites_data$final_status == 'under_threshold', na.rm = TRUE),
  need_treatment_sites = sum(sites_data$final_status == 'need_treatment', na.rm = TRUE),
  treated_sites = sum(sites_data$final_status == 'treated', na.rm = TRUE),
  
  # Treatment completion rate
  treatment_completion_rate = ifelse(
    sum(sites_data$final_status %in% c('need_treatment', 'treated'), na.rm = TRUE) > 0,
    round(100 * sum(sites_data$final_status == 'treated', na.rm = TRUE) / 
          sum(sites_data$final_status %in% c('need_treatment', 'treated'), na.rm = TRUE), 1),
    0
  )
)
```

#### Historical Metrics Aggregation
**All historical aggregations done in R after SQL returns raw inspection records:**

**Sites Inspected Metric:**
```r
# Count unique sites inspected per group per time period
if (display_metric == "sites") {
  plot_data <- historical_data %>%
    group_by(time_group, group_label) %>%
    summarise(value = n_distinct(sitecode), .groups = "drop")
}
```

**Sites Need Treatment Metric:**
```r
# Count unique sites needing treatment per group per time period  
if (display_metric == "need_treatment") {
  plot_data <- historical_data %>%
    filter(state == "need_treatment") %>%  # Only sites with numdip > 0
    group_by(time_group, group_label) %>%
    summarise(value = n_distinct(sitecode), .groups = "drop")
}
```

**Number of Inspections Metric:**
```r
# Count total inspection instances per group per time period
if (display_metric == "inspections") {
  plot_data <- historical_data %>%
    group_by(time_group, group_label) %>%
    summarise(value = n(), .groups = "drop")  # n() counts all inspection records
}
```

### Treatment Seasonal Logic (R-based)
**Treatment Season Classification:**
```r
treatments <- treatments %>%
  mutate(
    treatment_season = case_when(
      trt_month %in% c(9, 10, 11, 12) ~ "Fall/Winter",      # Sept-Dec
      trt_month %in% c(5, 6, 7, 8) ~ "Spring/Summer",       # May-Aug  
      TRUE ~ "Other"
    ),
    # Map treatment year to inspection year
    inspection_year = case_when(
      treatment_season == "Fall/Winter" ~ trt_year,          # Same year
      treatment_season == "Spring/Summer" ~ trt_year - 1,    # Previous year
      TRUE ~ trt_year
    )
  )
```

### Data Sources vs Aggregation Separation

**SQL Queries**: Return raw, individual records
- `load_cattail_data()`: Returns individual inspection site records with most recent inspection per site
- `load_cattail_treatments()`: Returns individual treatment records with seasonal classification
- `get_historical_cattail_data()`: Returns individual historical inspections

**R Aggregation**: Processes raw records into summaries
- `aggregate_cattail_data()`: Matches treatments to sites and calculates 3-state metrics
- Historical chart functions: Aggregate by time period and grouping dimension
- Value box calculations: Summarize overall 3-state statistics

## SQL Queries Reference

This section documents all SQL queries used in the cattail treatments app for reference and troubleshooting.

### 1. Cattail Inspection Sites Query  
**Function**: `load_cattail_data()` in `data_functions.R`  
**Purpose**: Load cattail inspection sites with zone and treatment need determination

```sql
WITH breeding_sites AS (
  SELECT 
    sc.facility, 
    sc.zone, 
    sc.fosarea, 
    LEFT(b.sitecode,7) AS sectcode,
    b.sitecode,
    b.acres,
    b.air_gnd,
    CASE WHEN b.drone IS NOT NULL THEN 'D' ELSE NULL END as drone,
    sc.fosarea as foreman
  FROM public.loc_breeding_sites b
  LEFT JOIN public.gis_sectcode sc ON LEFT(b.sitecode,7) = sc.sectcode
  WHERE (b.enddate IS NULL OR b.enddate > $1)
),
inspection_data AS (
  -- Get the most recent inspection per site (distinct sites only)
  WITH all_inspections AS (
    -- Current year inspections
    SELECT 
      i.sitecode,
      i.inspdate,
      i.action,
      i.numdip,
      i.acres_plan,
      EXTRACT(year FROM i.inspdate) as year
    FROM public.dblarv_insptrt_current i
    WHERE i.inspdate BETWEEN $2 AND $1
      AND i.action = '9'  -- Action 9 = inspected
    
    UNION ALL
    
    -- Archive inspections (if include_archive is true)
    SELECT 
      a.sitecode,
      a.inspdate,
      a.action,
      a.numdip,
      a.acres_plan,
      EXTRACT(year FROM a.inspdate) as year
    FROM public.dblarv_insptrt_archive a
    WHERE $3 = TRUE
      AND a.inspdate BETWEEN $2 AND $1
      AND a.action = '9'  -- Action 9 = inspected
  )
  SELECT DISTINCT ON (sitecode)
    sitecode,
    inspdate,
    action,
    numdip,
    acres_plan,
    year
  FROM all_inspections
  ORDER BY sitecode, inspdate DESC  -- Most recent inspection per site
)
SELECT 
  b.sitecode,
  b.sectcode,
  b.acres,
  b.air_gnd,
  b.drone,
  b.facility,
  b.zone,
  b.fosarea,
  b.foreman,
  -- Add display and calculated fields
  b.facility as facility_display,  -- We'll map this later
  0 as treated_acres,  -- Placeholder for display functions
  -- Determine treatment state - all sites are inspected since we use INNER JOIN
  CASE 
    WHEN i.sitecode IS NOT NULL THEN 'inspected'
    ELSE 'inspected'  -- This shouldn't happen with INNER JOIN
  END as inspection_status,
  CASE 
    WHEN i.numdip > 0 THEN 'need_treatment'
    WHEN i.numdip = 0 THEN 'under_threshold' 
    ELSE 'under_threshold'  -- Default to under_threshold for safety
  END as treatment_status,
  i.inspdate,
  i.numdip,
  i.acres_plan,
  i.year as inspection_year
FROM breeding_sites b
INNER JOIN inspection_data i ON b.sitecode = i.sitecode  -- Only inspected sites
ORDER BY b.sitecode
```

**Key Points**:
- **CTE Structure**: Uses CTEs for clear separation of site and inspection logic
- **DISTINCT ON**: Ensures one record per site with most recent inspection
- **Action Filter**: `action = '9'` identifies inspection records (vs treatment records)
- **Treatment Need Logic**: `numdip > 0` indicates cattail found and treatment needed
- **Active Sites**: Filters for sites with `enddate IS NULL` or active during analysis period
- **Archive Support**: Optionally includes archived inspections based on parameter

### 2. Cattail Treatments Query
**Function**: `load_cattail_treatments()` in `data_functions.R`  
**Purpose**: Load cattail treatments with seasonal timing logic

```sql
SELECT 
  i.sitecode, 
  i.inspdate AS trtdate, 
  i.action, 
  i.mattype, 
  i.matcode, 
  i.amts, 
  i.acres, 
  sc.facility,
  sc.zone,
  sc.fosarea,
  EXTRACT(year FROM i.inspdate) as trt_year,
  EXTRACT(month FROM i.inspdate) as trt_month
FROM public.dblarv_insptrt_current i
LEFT JOIN public.gis_sectcode sc ON LEFT(i.sitecode,7) = sc.sectcode
WHERE i.action IN ('3', 'A')  -- Action 3 = treatment, Action A = treatment
  AND i.matcode IN (
    SELECT matcode 
    FROM public.mattype_list_targetdose
    WHERE prgassign_default = 'Cat'
  )
  AND (
    -- Fall/winter treatments (same year as inspection)
    (i.inspdate BETWEEN $1 AND $2)
    OR
    -- Spring/summer treatments (year after inspection) 
    (i.inspdate BETWEEN $3 AND $4)
  )

UNION ALL

SELECT 
  a.sitecode, 
  a.inspdate AS trtdate, 
  a.action, 
  a.mattype, 
  a.matcode, 
  a.amts, 
  a.acres, 
  sc.facility,
  sc.zone,
  sc.fosarea,
  EXTRACT(year FROM a.inspdate) as trt_year,
  EXTRACT(month FROM a.inspdate) as trt_month
FROM public.dblarv_insptrt_archive a
LEFT JOIN public.gis_sectcode sc ON LEFT(a.sitecode,7) = sc.sectcode
WHERE a.action IN ('3', 'A')  -- Action 3 = treatment, Action A = treatment
  AND a.matcode IN (
    SELECT matcode 
    FROM public.mattype_list_targetdose
    WHERE prgassign_default = 'Cat'
  )
  AND (
    -- Fall/winter treatments (same year as inspection)
    (a.inspdate BETWEEN $1 AND $2)
    OR
    -- Spring/summer treatments (year after inspection) 
    (a.inspdate BETWEEN $3 AND $4)
  )

ORDER BY trtdate
```

**Date Parameters**:
```r
# For current_year = 2024 (inspection year)
fall_start <- as.Date("2024-09-01")    # $1 - Fall treatments start
fall_end <- as.Date("2024-12-31")      # $2 - Fall treatments end
spring_start <- as.Date("2025-05-01")  # $3 - Spring treatments start  
spring_end <- as.Date("2025-07-31")    # $4 - Spring treatments end
```

**Key Points**:
- **Treatment Actions**: Both `action = '3'` and `action = 'A'` represent treatments
- **Material Validation**: JOIN with `mattype_list_targetdose WHERE prgassign_default = 'Cat'`
- **Seasonal Windows**: Covers both fall same year and spring following year
- **UNION ALL**: Combines current and archive treatment records
- **Geographic Info**: Joins with `gis_sectcode` for facility/zone assignment

### 3. Historical Cattail Inspections Query
**Function**: `get_historical_cattail_data()` in `historical_functions.R`  
**Purpose**: Load historical cattail inspections for trend analysis

```sql
SELECT 
  i.sitecode,
  i.inspdate,
  i.action,
  i.numdip,
  i.acres_plan,
  EXTRACT(year FROM i.inspdate) as year,
  EXTRACT(month FROM i.inspdate) as month,
  EXTRACT(week FROM i.inspdate) as week,
  sc.facility,
  sc.zone,
  sc.fosarea,
  b.acres
FROM public.dblarv_insptrt_current i
LEFT JOIN public.loc_breeding_sites b ON i.sitecode = b.sitecode
LEFT JOIN public.gis_sectcode sc ON LEFT(i.sitecode,7) = sc.sectcode
WHERE i.inspdate BETWEEN $1 AND $2
  AND i.action = '9'  -- Action 9 = inspected
  AND (b.enddate IS NULL OR b.enddate > $2)

UNION ALL

SELECT 
  a.sitecode,
  a.inspdate,
  a.action,
  a.numdip,
  a.acres_plan,
  EXTRACT(year FROM a.inspdate) as year,
  EXTRACT(month FROM a.inspdate) as month,
  EXTRACT(week FROM a.inspdate) as week,
  sc.facility,
  sc.zone,
  sc.fosarea,
  b.acres
FROM public.dblarv_insptrt_archive a
LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
LEFT JOIN public.gis_sectcode sc ON LEFT(a.sitecode,7) = sc.sectcode
WHERE a.inspdate BETWEEN $1 AND $2
  AND a.action = '9'  -- Action 9 = inspected
  AND (b.enddate IS NULL OR b.enddate > $2)

ORDER BY inspdate
```

**Key Points**:
- **Historical Range**: Uses start_date and end_date parameters for flexible date ranges
- **Time Extractions**: Extracts year, month, week for different aggregation periods
- **All Inspections**: Does NOT use DISTINCT ON - includes all historical inspections
- **Active Sites Filter**: Ensures sites were active during the analysis period
- **Archive Integration**: Combines current and archive tables for complete history

### 4. Treatment Season Classification (R Logic)
**Function**: R processing in `load_cattail_treatments()`  
**Purpose**: Classify treatments by season and map to inspection years

```r
# Add treatment season classification
treatments <- treatments %>%
  mutate(
    trtdate = as.Date(trtdate),
    treatment_season = case_when(
      trt_month %in% c(9, 10, 11, 12) ~ "Fall/Winter",
      trt_month %in% c(5, 6, 7, 8) ~ "Spring/Summer", 
      TRUE ~ "Other"
    ),
    inspection_year = case_when(
      treatment_season == "Fall/Winter" ~ trt_year,     # Same year as inspection
      treatment_season == "Spring/Summer" ~ trt_year - 1, # Previous year inspection
      TRUE ~ trt_year
    ),
    action_desc = case_when(
      action == '3' ~ "Treatment",
      action == 'A' ~ "Treatment", 
      TRUE ~ paste("Action", action)
    )
  )
```

**Key Points**:
- **Season Windows**: Fall/Winter (Sept-Dec), Spring/Summer (May-Aug)
- **Year Mapping**: Spring treatments in year N+1 map to year N inspections
- **Treatment Types**: Both action '3' and 'A' classified as "Treatment"
- **Inspection Matching**: `inspection_year` used to match treatments to inspection cycles

### 5. 3-State Aggregation Logic (R Logic)
**Function**: R processing in `aggregate_cattail_data()`  
**Purpose**: Calculate final treatment status by matching inspections with treatments

```r
# Match treatments to sites for current year
if (!is.null(treatments) && nrow(treatments) > 0) {
  # Get sites that were treated in the current year cycle
  treated_sites <- treatments %>%
    filter(inspection_year == current_year) %>%
    pull(sitecode) %>%
    unique()
  
  # Update treatment status to include 'treated' state
  sites_data <- sites_data %>%
    mutate(
      final_status = case_when(
        sitecode %in% treated_sites ~ "treated",
        treatment_status == "need_treatment" ~ "need_treatment", 
        treatment_status == "under_threshold" ~ "under_threshold",
        TRUE ~ "under_threshold"
      ),
      # Update state field to match final_status for UI compatibility
      state = final_status
    )
} else {
  # No treatments data, use original status
  sites_data$final_status <- sites_data$treatment_status
  sites_data$state <- sites_data$treatment_status
}
```

**Key Points**:
- **Treatment Matching**: Uses `inspection_year` to match treatments to inspection cycles
- **3-State Logic**: under_threshold → need_treatment → treated progression
- **Site-Based**: Tracks unique sites, not treatment instances
- **Fallback Logic**: Handles case when no treatments are available
- **UI Compatibility**: Maintains `state` field for existing UI components