# Ground Prehatch Treatment Progress - Technical Notes

## Overview
This Shiny app tracks ground prehatch mosquito breeding sites across four perspectives:
1. **Progress Overview** - Sites with active/expiring treatments, aggregated by facility, FOS, or section
2. **Detailed View** - Individual site details with treatment status and download capabilities
3. **Map** - Interactive map showing prehatch sites with color-coded treatment status markers
4. **Historical Analysis** - Multi-year trends in prehatch treatments (sites, treatments, or acres)

## Data Sources

### Core Database Tables and Columns

#### Primary Treatment Tables
- **`public.dblarv_insptrt_current`** - Active larval treatment records
  - **Key Columns**: `sitecode`, `facility`, `inspdate`, `matcode`, `foreman`, `action`, `airgrnd_plan`, `amts`
  - **Prehatch Filter**: JOIN with `loc_breeding_sites` WHERE `prehatch = true`
  - **Missing Columns**: Does NOT contain `zone`, `enddate`, `prehatch` - must join for these
  - **Data Quality**: Contains ongoing treatments, some may be planned vs executed
  
- **`public.dblarv_insptrt_archive`** - Historical larval treatment records  
  - **Key Columns**: `sitecode`, `facility`, `inspdate`, `matcode`, `foreman`, `action`, `amts`
  - **Prehatch Filter**: JOIN with `loc_breeding_sites` WHERE `prehatch = true`
  - **Missing Columns**: Does NOT contain `zone`, `enddate`, `prehatch`, `airgrnd_plan` - must join for these
  - **Data Quality**: Historical closed treatments, generally more reliable than current

#### Essential Supporting Tables
- **`public.loc_breeding_sites`** - Site master data
  - **Key Columns**: 
    - `sitecode`, `facility` (composite key)
    - `acres` (site capacity, not treated acres)
    - `enddate` (**CRITICAL**: NULL = active site, NOT NULL = closed site)
    - `prehatch` (boolean: **TRUE = prehatch site**, FALSE = standard site)
    - `drone` (values: 'Y', 'M', 'C' for drone-capable sites)
    - `air_gnd` (alternate treatment designation)
    - `geom` (PostGIS geometry data for mapping, UTM projection)
  - **Critical Join Logic**: **MUST filter `enddate IS NULL`** or risk including closed sites
  - **Prehatch Logic**: **MUST filter `prehatch = true`** for prehatch-only analysis
  - **Data Quality**: Multiple rows per sitecode possible with different enddates
  - **Usage**: Source of truth for site characteristics, active status, and spatial coordinates
  - **Spatial Data**: Geometry stored in UTM projection, requires `ST_Transform(geom, 4326)` for web mapping
  
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

**Standard Treatment-to-Prehatch-Site Join Pattern:**
```sql
-- Connect treatment records with prehatch site information
FROM public.dblarv_insptrt_current t
LEFT JOIN public.loc_breeding_sites b ON t.sitecode = b.sitecode AND t.facility = b.facility
LEFT JOIN public.gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
LEFT JOIN public.employee_list e ON sc.fosarea = e.emp_num 
  AND e.emp_type = 'FieldSuper' AND e.active = true
LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
WHERE (b.enddate IS NULL OR b.enddate > CURRENT_DATE)  -- Active sites only
  AND b.prehatch = true                                  -- Prehatch sites only
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
- **Active Status**: `treatment_end_date >= analysis_date`
- **Expiring Status**: `treatment_end_date BETWEEN analysis_date AND (analysis_date + expiring_days)`
- **Expired Status**: `treatment_end_date < analysis_date`
- **Material Lookup**: Join `mattype_list_targetdose` for specific `effect_days`

**Prehatch Site Identification:**
- **Primary Filter**: `loc_breeding_sites.prehatch = true`
- **Definition**: Sites designated for pre-hatch larval mosquito treatment
- **Treatment Timing**: Typically treated earlier in season before mosquito emergence
- **Operational Context**: May expire intentionally due to seasonal factors (drying, weather)

## Tab 1: Progress Overview

### Purpose
Display current status of prehatch sites with active, expiring, and expired treatment categories.

### Key Features
- **Value boxes** showing summary statistics (total sites, prehatch sites, treated sites, expired sites, percentages)
- **Layered bar chart** showing treatment progress by group (facility, FOS, or section)
- **Site Filter** - All Sites, Expiring Only, or Expiring + Expired
- **Expiring Days slider** - Configurable threshold for expiring treatments (1-60 days)
- **Date simulation** - "Pretend Today is" date picker for status analysis
- **Zone filters** - P1 Only, P2 Only, P1 and P2 Separate, Combined P1+P2
- **Group By selector** - All MMCD, Facility, FOS, or Section

### Data Flow
1. Query prehatch sites via `get_ground_prehatch_data()`:
   - Filter for active prehatch sites (`prehatch = true`, `enddate IS NULL`)
   - Join with zone and FOS assignments
   - Return one record per site with characteristics

2. Query current treatments via `get_ground_prehatch_data()`:
   - Get latest treatment per prehatch site
   - Calculate treatment status based on end date vs analysis date
   - Join with effectiveness data for duration calculation

3. Aggregate via `aggregate_ground_prehatch_data()`:
   - Group by selected dimension (facility, FOS, section, or all MMCD)
   - Calculate counts:
     - `tot_ground`: Total ground sites (context)
     - `prehatch_sites_cnt`: Total prehatch sites in group
     - `ph_treated_cnt`: Sites with active treatments
     - `ph_expiring_cnt`: Sites with expiring treatments (within X days)
     - `ph_expired_cnt`: Sites with expired treatments

### Visualization
- **Layered bar chart** with three layers per group (same as drone app):
  - Gray background bar: Total prehatch sites (background)
  - Group color bar: Sites with active treatments (facility/foreman colors)
  - Orange overlay bar: Sites with expiring treatments (status orange)
- Colors mapped by group type:
  - **Facility**: Distinct facility colors from db_helpers
  - **FOS**: Facility-based colors mapped through foreman lookup
  - **Section**: Inherits facility colors based on section location
  - **MMCD All**: Uses status colors (green for active, orange for expiring)

### Zone Handling
- **Both zones selected**: Shows groups separately (e.g., "Anoka (P1)", "Anoka (P2)")
- **Single zone**: Shows groups without zone suffix
- **Zone differentiation**: Uses alpha transparency (P1 solid, P2 faded) when both zones shown

## Tab 2: Detailed View

### Purpose
Provide detailed site-by-site view of prehatch treatment status with filtering and download capabilities.

### Key Features
- **Detailed data table** showing individual site information:
  - Facility, Priority Zone, Section, Sitecode
  - FOS assignment, Acres, Priority
  - Treatment Type, Status, Last Treatment Date
  - Days Since Last Treatment, Material, Effect Days
- **Download CSV** functionality for filtered data
- **Same filters** as Progress Overview tab
- **Sortable columns** and searchable interface
- **Pagination** for large datasets

### Data Flow
1. Use same data source as Progress Overview (`get_site_details_data()`)
2. Return individual site records (not aggregated)
3. Map foreman employee numbers to names via `get_foremen_lookup()`
4. Format dates and numeric values for display
5. Apply same filtering logic as overview tab

### Data Table Features
- **Responsive design** with horizontal scrolling for small screens
- **Center-aligned** numeric and status columns
- **Date formatting** (MM/DD/YY format)
- **Decimal precision** (2 places for acres, 1 place for age)
- **Foreman mapping** from employee numbers to readable names

## Tab 3: Map

### Purpose
Provide interactive map visualization of prehatch sites with color-coded treatment status markers.

### Key Features
- **Interactive Leaflet Map** with multiple basemap options:
  - Streets (CartoDB Positron) - Default
  - Satellite (Esri World Imagery)
  - Terrain (Esri World Topo)
  - OpenStreetMap
- **Color-coded markers** by treatment status:
  - **Green**: Active treatments (#187018)
  - **Orange**: Expiring treatments (#FF4500)
  - **Red**: Expired treatments (#FF0000)
  - **Gray**: No treatment recorded (#A9A9A9)
- **Site popups** with detailed information:
  - Sitecode, facility, zone, site acres
  - Treatment status and last treatment details
  - Last material used and treated acres
- **Map legend** showing treatment status color scheme
- **Data table** below map with site details and filtering
- **Shared filters** with other tabs (zone, facility, FOS, expiring days)

### Data Flow
1. Load spatial data via `load_spatial_data()`:
   - Uses `load_raw_data()` with `include_geometry = TRUE`
   - Extracts coordinates using `ST_Transform(ST_Centroid(geom), 4326)`
   - Applies same filters as Progress Overview tab
   - Calculates treatment status for each site

2. Process spatial data:
   - Join prehatch sites with latest treatment information
   - Calculate treatment status based on end date vs current date
   - Filter sites to only those with valid coordinates
   - Convert to sf object for leaflet mapping

### Visualization
- **Base map**: Multiple provider tiles via leaflet providers
- **Markers**: CircleMarkers with 6px radius, black borders
- **Colors**: Based on treatment status using status color scheme
- **Popups**: HTML-formatted with site and treatment details
- **Legend**: Bottom-right position showing status colors
- **Bounds**: Automatic map fitting to data extent

### Coordinate Transformation
- **Source**: PostGIS geometry in UTM projection (EPSG:26915)
- **Target**: WGS84 decimal degrees (EPSG:4326) for web mapping
- **SQL Transform**: `ST_X(ST_Transform(ST_Centroid(geom), 4326))` for longitude
- **Coordinate Range**: Minnesota extent (~-93.8 to -92.8 lng, 44.6 to 45.4 lat)

## Tab 4: Historical Analysis

### Purpose
Analyze multi-year trends in prehatch treatment activity across facilities, FOS, or combined.

### Key Features
- **Display Metric selector** - Number of Sites, Number of Treatments, or Number of Acres
- **Time Period selector** - Weekly or Yearly aggregation
- **Year Range controls** - Start Year and End Year (configurable 20-year range)
- **Chart Type selector** - Stacked Bar, Grouped Bar, Line Chart, Area Chart, Step Chart
- **Zone filter** - P1, P2, or both (shared with other tabs)
- **Facility/FOS filters** - Shared with other tabs
- **Group By selector** - All MMCD, Facility, FOS, or Section

### Data Flow
1. Query historical data via `get_historical_prehatch_data()`:
   - Combine archive and current treatment tables
   - Filter for prehatch sites via JOIN with `loc_breeding_sites`
   - Join with zone and FOS assignments
   - Filter by year range and analysis date

2. Process time periods:
   - **Weekly**: Create week numbers (`YYYY-W##` format)
   - **Yearly**: Use year values directly
   - Sort by time period for proper chart ordering

3. Aggregate via `aggregate_historical_data()`:
   - Group by selected dimension and time period
   - Calculate metrics:
     - **Sites**: `n_distinct(sitecode)` - Count unique sites treated
     - **Treatments**: `n()` - Count total treatment instances
     - **Acres**: `sum(treated_acres)` - Sum actual treated acres

### Visualization
- **Multiple chart types** with consistent color schemes:
  - **Stacked Bar**: Shows total volume with group contributions
  - **Grouped Bar**: Side-by-side comparison between groups
  - **Line Chart**: Trend analysis over time
  - **Area Chart**: Filled area showing volume trends
  - **Step Chart**: Step-wise changes in treatment levels
- **Colors**: Same facility/foreman color mapping as Progress Overview
- **Zone Support**: Alpha transparency for P1/P2 differentiation when both selected

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

### Time Period Options
**Yearly View (Default)**:
- Shows one data point per year in the selected range
- Useful for long-term trend analysis
- Better for multi-year comparisons
- Less cluttered display

**Weekly View**:
- Shows one data point per week in the selected range
- Provides detailed seasonal and short-term patterns
- Useful for identifying weekly treatment patterns
- More detailed but potentially dense for large date ranges
- Week format: "YYYY-W##" (e.g., "2024-W15" for week 15 of 2024)

## Code Organization

### File Structure
```
ground_prehatch_progress/
├── app.R                           # Main app - UI orchestration and server logic
├── ui_helpers.R                    # UI component functions
├── data_functions.R                # All database queries and data processing
├── display_functions.R             # Visualization helpers and color mapping
├── historical_functions_simple.R   # Historical trends data and plotting
└── NOTES.md                        # This file
```

### Key Functions by File

#### app.R
- `refresh_inputs()` - Captures all UI inputs when refresh clicked (prevents reactive reruns)
- `ground_data()` - Loads prehatch sites when refresh button clicked
- `site_details()` - Loads detailed site data when refresh button clicked
- `aggregated_data()` - Aggregates data by group for progress charts
- `map_spatial_data()` - Loads spatial data with geometry for map visualization
- `server()` - Main server function with output renderers for all four tabs

#### data_functions.R
- `get_ground_prehatch_data()` - Queries prehatch sites with zone/FOS data and analysis date support
- `load_raw_data()` - Unified data loading with geometry support for mapping
- `load_spatial_data()` - Loads spatial data with coordinates and treatment status
- `aggregate_ground_prehatch_data()` - Aggregates prehatch data by group with zone handling

#### display_functions.R
- `create_progress_chart()` - Generates layered bar chart matching drone app style
- `create_historical_chart()` - Generates historical trends with multiple chart types
- `create_ground_map()` - Creates leaflet map with color-coded treatment status markers
- `create_details_table()` - Formats detailed site data for display
- `create_value_boxes()` - Calculates summary statistics for value boxes

#### historical_functions_simple.R
- `get_historical_prehatch_data()` - Queries combined archive and current historical data
- `aggregate_historical_data()` - Processes and aggregates historical data by group and time
- `create_historical_details_table()` - Formats historical data for download/display

#### ui_helpers.R
- `create_filter_panel()` - Main filter controls with conditional logic
- `create_progress_chart_box()` - Progress overview chart container with important note
- `create_details_table_box()` - Detailed view table container with download
- `create_map_box()` - Map container with basemap controls and important note
- `create_historical_chart_box()` - Historical analysis chart container with important note
- `create_important_note()` - Universal important note about expired prehatch sites

## Data Collection and Aggregation Logic

### Overview of Data Processing Pipeline
The ground prehatch progress app follows the same pattern as the drone app for data collection and aggregation. Data processing is done **outside of SQL** - SQL queries return raw records which are then aggregated using R/dplyr operations.

### Core Aggregation Methods

#### Total Sites vs Total Acres Calculation
**IMPORTANT**: Both total sites and total acres calculations are performed **outside of SQL** using R aggregation functions.

**Prehatch Sites Calculation:**
```r
# Count UNIQUE prehatch sites (not treatments)
prehatch_sites <- ground_sites %>%
  filter(prehatch == TRUE) %>%
  group_by(!!sym(group_col)) %>%
  summarize(prehatch_sites_cnt = n(), .groups = "drop")  # n() counts rows = sites

# For zone separation
prehatch_sites <- ground_sites %>%
  filter(prehatch == TRUE) %>%
  group_by(combined_group, zone) %>%
  summarize(prehatch_sites_cnt = n(), .groups = "drop")
```

**Site Acres Calculation:**
```r
# Sum acres from SITE records (loc_breeding_sites.acres)
prehatch_acres <- ground_sites %>%
  filter(prehatch == TRUE) %>%
  group_by(!!sym(group_col)) %>%
  summarize(total_acres = sum(acres, na.rm = TRUE), .groups = "drop")
```

**Treated Sites vs Treated Acres:**
```r
# Count unique SITES with active treatments (not treatment instances)
treated_sites <- prehatch_treatments %>%
  filter(is_active) %>%
  group_by(!!sym(group_col)) %>%
  summarize(ph_treated_cnt = n_distinct(sitecode), .groups = "drop")

# Sum TREATED acres from treatment records (treated_acres)
treated_acres <- prehatch_treatments %>%
  filter(is_active) %>%
  group_by(!!sym(group_col)) %>%
  summarize(treated_acres = sum(acres, na.rm = TRUE), .groups = "drop")
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
# Count unique prehatch sites treated per group per time period
if (hist_display_metric == "sites") {
  results <- all_data %>%
    group_by(!!group_var, !!time_var, time_sort) %>%
    summarize(count = n_distinct(sitecode), .groups = "drop")
}
```

**Treatments Metric:**
```r
# Count total prehatch treatment instances per group per time period
if (hist_display_metric == "treatments") {
  results <- all_data %>%
    group_by(!!group_var, !!time_var, time_sort) %>%
    summarize(count = n(), .groups = "drop")  # n() counts all treatment records
}
```

**Acres Metric:**
```r
# Sum treated acres from prehatch treatment records per group per time period
if (hist_display_metric == "acres") {
  results <- all_data %>%
    group_by(!!group_var, !!time_var, time_sort) %>%
    summarize(count = sum(treated_acres, na.rm = TRUE), .groups = "drop")
}
```
### Data Sources vs Aggregation Separation

**SQL Queries**: Return raw, individual records
- `get_ground_prehatch_data()`: Returns individual prehatch site records and treatment records
- `get_historical_prehatch_data()`: Returns individual historical treatments for prehatch sites
- `load_spatial_data()`: Returns prehatch sites with coordinates

**R Aggregation**: Processes raw records into summaries
- `aggregate_ground_prehatch_data()`: Aggregates sites and treatments by group
- `aggregate_historical_data()`: Aggregates historical treatments by group and time period
- Value box calculations: Summarize overall statistics

### Treatment Status Logic (R-based)
**Active Treatment Calculation:**
```r
prehatch_treatments <- prehatch_treatments %>%
  mutate(
    inspdate = as.Date(inspdate),
    effect_days = ifelse(is.na(effect_days), 14, effect_days),  # Default 14 days
    treatment_end_date = inspdate + effect_days,
    is_active = treatment_end_date >= analysis_date
  )
```

**Expiring Treatment Calculation:**
```r
prehatch_treatments <- prehatch_treatments %>%
  mutate(
    expiring_start_date = analysis_date,
    expiring_end_date = analysis_date + expiring_days,
    is_expiring = is_active & 
                  treatment_end_date >= expiring_start_date & 
                  treatment_end_date <= expiring_end_date
  )
```

**Expired Treatment Calculation:**
```r
prehatch_treatments <- prehatch_treatments %>%
  mutate(
    is_expired = treatment_end_date < analysis_date
  )
```

## SQL Queries Reference

This section documents all SQL queries used in the ground prehatch progress app for reference and troubleshooting.

### 1. Current Prehatch Sites Query
**Function**: `get_ground_prehatch_data()` in `data_functions.R`  
**Purpose**: Load active prehatch sites with zone and FOS information

```sql
SELECT b.sitecode, g.facility, b.acres, b.prehatch, 
       CASE 
         WHEN e.emp_num IS NOT NULL AND e.active = true THEN g.fosarea
         ELSE NULL
       END as foreman, 
       g.zone,
       left(b.sitecode,7) as sectcode
FROM public.loc_breeding_sites b
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode,7)
LEFT JOIN public.employee_list e ON g.fosarea = e.emp_num 
  AND e.emp_type = 'FieldSuper' 
  AND e.active = true
WHERE b.prehatch = true
  AND b.enddate IS NULL
```

**Key Points**:
- **Prehatch Filter**: `b.prehatch = true` ensures only prehatch sites
- Uses `loc_breeding_sites` as primary source, joined with `gis_sectcode` for zone/FOS data
- **PRECISE SECTCODE MATCHING**: `g.sectcode = left(b.sitecode,7)` ensures exact match
- Filters for active sites (`enddate IS NULL`)
- Gets zone, FOS area from gis_sectcode via exact sectcode matching
- Joins with employee_list to validate active field supervisors

### 2. Current Prehatch Treatments Query
**Function**: `get_ground_prehatch_data()` in `data_functions.R`  
**Purpose**: Load current treatments for prehatch sites with zone/FOS data and effectiveness duration

```sql
SELECT t.sitecode, t.inspdate, t.matcode, t.acres as treated_acres, 
       t.foreman, m.effect_days, g.facility, g.zone, g.fosarea,
       b.prehatch
FROM public.dblarv_insptrt_current t
LEFT JOIN public.loc_breeding_sites b ON t.sitecode = b.sitecode AND t.facility = b.facility
LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(t.sitecode,7)
WHERE b.prehatch = true
  AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
```

**Key Points**:
- **Prehatch Sites Only**: Joins with `loc_breeding_sites` and filters `b.prehatch = true`
- **PRECISE SECTCODE MATCHING**: `g.sectcode = left(t.sitecode,7)` ensures exact match
- Joins with mattype_list_targetdose for treatment duration
- Gets facility, zone, and fosarea from gis_sectcode via exact sectcode matching
- Filters for active sites to avoid treatments on closed prehatch sites

### 3. Historical Prehatch Treatment Data Query
**Function**: `get_historical_prehatch_data()` in `historical_functions_simple.R`  
**Purpose**: Load historical treatment data for prehatch sites from both archive and current

```sql
-- Historical prehatch treatments from archive
SELECT 
    t.facility, 
    t.sitecode, 
    t.inspdate, 
    t.action, 
    t.matcode, 
    t.amts as amount, 
    t.acres as treated_acres,
    g.zone,
    g.fosarea as foreman
FROM public.dblarv_insptrt_archive t
LEFT JOIN public.loc_breeding_sites b ON t.sitecode = b.sitecode AND t.facility = b.facility
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(t.sitecode,7)
WHERE b.prehatch = true
  AND EXTRACT(YEAR FROM t.inspdate) BETWEEN ? AND ?
  AND t.inspdate <= ?

UNION ALL

-- Recent prehatch treatments from current
SELECT 
    t.facility, 
    t.sitecode, 
    t.inspdate, 
    t.action, 
    t.matcode, 
    t.amts as amount, 
    t.acres as treated_acres,
    g.zone,
    g.fosarea as foreman
FROM public.dblarv_insptrt_current t
LEFT JOIN public.loc_breeding_sites b ON t.sitecode = b.sitecode AND t.facility = b.facility
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(t.sitecode,7)
WHERE b.prehatch = true
  AND EXTRACT(YEAR FROM t.inspdate) BETWEEN ? AND ?
  AND t.inspdate <= ?
```

**Key Points**:
- **Prehatch Sites Only**: Both queries join with `loc_breeding_sites` and filter `b.prehatch = true`
- Combines archive and current treatment tables with UNION ALL
- **PRECISE SECTCODE MATCHING**: Uses exact sectcode matching in both queries
- Includes year range and analysis date filtering
- Returns treatment data with accurate zone/FOS information for prehatch sites only

### 4. Spatial Prehatch Data Query (Map Functionality)
**Function**: `load_spatial_data()` in `data_functions.R`  
**Purpose**: Load prehatch sites with spatial coordinates for interactive mapping

```sql
SELECT 
    b.sitecode, 
    g.facility, 
    b.acres, 
    b.prehatch, 
    CASE 
        WHEN e.emp_num IS NOT NULL AND e.active = true THEN g.fosarea
        ELSE NULL
    END as foreman, 
    g.zone,
    left(b.sitecode,7) as sectcode,
    ST_X(ST_Transform(ST_Centroid(b.geom), 4326)) as lng, 
    ST_Y(ST_Transform(ST_Centroid(b.geom), 4326)) as lat
FROM public.loc_breeding_sites b
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode,7)
LEFT JOIN public.employee_list e ON g.fosarea = e.emp_num 
    AND e.emp_type = 'FieldSuper' 
    AND e.active = true
WHERE b.prehatch = true
    AND b.enddate IS NULL
    AND b.geom IS NOT NULL
```

**Key Points**:
- **Prehatch Sites Only**: `b.prehatch = true` filters for prehatch sites
- **PRECISE SECTCODE MATCHING**: `g.sectcode = left(b.sitecode,7)` ensures exact match
- **Geometry Transform**: `ST_Transform(ST_Centroid(b.geom), 4326)` converts UTM to WGS84 decimal degrees
- **Coordinate Extraction**: `ST_X()` and `ST_Y()` extract longitude and latitude values
- **Geometry Filter**: `b.geom IS NOT NULL` ensures only sites with valid coordinates
- **Web Mapping Ready**: Coordinates returned in standard web mercator projection (EPSG:4326)

### 5. Site Details Query
**Function**: `get_site_details_data()` in `data_functions.R`  
**Purpose**: Load detailed information for individual prehatch sites with latest treatment data

```sql
WITH latest_treatments AS (
    SELECT DISTINCT ON (sitecode, facility)
        sitecode, 
        facility, 
        inspdate, 
        matcode,
        foreman as treatment_foreman,
        acres as treated_acres,
        effect_days
    FROM (
        SELECT t.sitecode, t.facility, t.inspdate, t.matcode, 
               t.foreman, t.acres, m.effect_days
        FROM public.dblarv_insptrt_current t
        LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
        
        UNION ALL
        
        SELECT t.sitecode, t.facility, t.inspdate, t.matcode, 
               t.foreman, t.acres, m.effect_days
        FROM public.dblarv_insptrt_archive t
        LEFT JOIN public.mattype_list_targetdose m ON t.matcode = m.matcode
    ) all_treatments
    ORDER BY sitecode, facility, inspdate DESC
),
prehatch_sites AS (
    SELECT 
        b.sitecode, 
        b.facility, 
        b.acres, 
        b.prehatch,
        b.priority,
        g.zone,
        g.fosarea as site_foreman,
        left(b.sitecode,7) as sectcode
    FROM public.loc_breeding_sites b
    LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode,7)
    WHERE b.prehatch = true
      AND b.enddate IS NULL
)
SELECT 
    s.sitecode,
    s.facility,
    s.acres,
    s.priority,
    s.zone,
    s.sectcode,
    s.site_foreman as fosarea,
    COALESCE(t.inspdate, '1900-01-01'::date) as inspdate,
    COALESCE(t.matcode, 'None') as matcode,
    COALESCE(t.effect_days, 14) as effect_days,
    CASE 
        WHEN t.inspdate IS NOT NULL THEN 
            EXTRACT(DAYS FROM AGE(CURRENT_DATE, t.inspdate))
        ELSE NULL
    END as age
FROM prehatch_sites s
LEFT JOIN latest_treatments t ON s.sitecode = t.sitecode AND s.facility = t.facility
ORDER BY s.facility, s.sectcode, s.sitecode
```

**Key Points**:
- **CTE for Latest Treatments**: Uses window function to get most recent treatment per site
- **Prehatch Sites Only**: Filters `b.prehatch = true` in the prehatch_sites CTE
- **Combined Data Sources**: UNION ALL of current and archive treatment tables
- **Age Calculation**: Computes days since last treatment using PostgreSQL AGE function
- **Default Values**: Uses COALESCE for missing treatment data
- **Ordered Results**: Sorts by facility, section, and sitecode for consistent display
