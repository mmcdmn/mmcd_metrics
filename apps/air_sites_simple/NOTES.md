# Air Sites Analysis - Technical Notes

## Overview
This Shiny app tracks air-treated mosquito breeding sites across three perspectives:
1. **Current Status** - Sites with active/expiring treatments, red bug detections, and priority status
2. **Map** - Interactive map showing air sites with color-coded status markers based on recent inspections and treatments
3. **Historical Analysis** - Multi-year inspection summary with red bug detection ratios and treatment metrics

## Data Sources

### Core Database Tables and Columns

#### Primary Inspection Tables
- **`public.dblarv_insptrt_current`** - Active larval inspection and treatment records
  - **Key Columns**: `sitecode`, `facility`, `inspdate`, `matcode`, `foreman`, `action`, `numdip`, `sampnum_yr`
  - **Air Inspection Filter**: `action IN ('2', '4')` for actual inspections
  - **Air Treatment Filter**: `action IN ('3', 'A', 'D')` for treatments with air materials
  - **Missing Columns**: Does NOT contain `zone`, `enddate`, `priority` - must join for these
  - **Data Quality**: Contains ongoing inspections/treatments, some may be planned vs executed
  
- **`public.dblarv_insptrt_archive`** - Historical larval inspection and treatment records  
  - **Key Columns**: `sitecode`, `facility`, `inspdate`, `matcode`, `foreman`, `action`, `numdip`, `sampnum_yr`
  - **Air Inspection Filter**: `action IN ('2', '4')` for actual inspections
  - **Air Treatment Filter**: `action IN ('3', 'A', 'D')` for treatments
  - **Missing Columns**: Does NOT contain `zone`, `enddate`, `priority` - must join for these
  - **Data Quality**: Historical closed inspections/treatments, generally more reliable than current

#### Essential Supporting Tables
- **`public.loc_breeding_sites`** - Site master data
  - **Key Columns**: 
    - `sitecode`, `facility` (composite key)
    - `acres` (site capacity, not treated acres)
    - `enddate` (**CRITICAL**: NULL = active site, NOT NULL = closed site)
    - `air_gnd` (values: 'A' = air sites, 'G' = ground sites)
    - `priority` (site priority: RED, BLUE, YELLOW)
    - `geom` (PostGIS geometry data for mapping, UTM projection)
  - **Critical Join Logic**: **MUST filter `enddate IS NULL` OR handle temporal matching** for active periods
  - **Air Site Filter**: **MUST filter `air_gnd = 'A'`** for air sites only
  - **Data Quality**: Multiple rows per sitecode possible with different enddates representing active periods
  - **Usage**: Source of truth for site characteristics, active status, and spatial coordinates
  - **Spatial Data**: Geometry stored in UTM projection, requires `ST_Transform(geom, 4326)` for web mapping
  
- **`public.gis_sectcode`** - Geographic section mapping and zone assignments
  - **Key Columns**:
    - `sectcode` (7-character section identifier, e.g. '191031N')
    - `facility` (**PREFERRED**: Use this over loc_breeding_sites.facility for consistency)
    - `zone` (values: '1' = P1, '2' = P2, '3' = P3, '4' = P4)
    - `fosarea` (FOS employee number assignment)
  - **Join Pattern**: `LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(sitecode, 7)`
  - **Sitecode Formats Handled**:
    - Standard: `190208-003` → sectcode `1902080`
    - Directional: `191102W010` → sectcode `1911020` 
  - **Data Quality**: Authoritative source for zone, facility, and FOS assignments
  
- **`public.mattype_list_targetdose`** - Material effectiveness and dosage data
  - **Key Columns**:
    - `matcode` (material identifier, joins to treatment tables)
    - `effect_days` (treatment effectiveness duration in days)
  - **Default Logic**: When `effect_days IS NULL`, assume 14 days for air treatments
  - **Usage**: Calculate treatment end dates: `inspdate + effect_days`

#### Lab Sample Tables
- **`public.dblarv_sample_current`** - Current lab sample analysis results
  - **Key Columns**:
    - `sampnum_yr` (sample identifier, links to inspection records)
    - `redblue` (values: 'R' = red bugs detected, 'B' = blue bugs detected)
    - `form_type` (values: 'AIR' = air samples, 'LOW' = ground samples)
    - `missing` (boolean: FALSE = valid sample, TRUE = missing/invalid)
  - **Air Sample Filter**: **MUST filter `form_type = 'AIR'`** for air site samples
  - **Valid Sample Filter**: **MUST filter `missing = FALSE OR missing IS NULL`** for valid samples
  
- **`public.dblarv_sample_archive`** - Historical lab sample analysis results
  - **Key Columns**: Same as current table
  - **Usage**: Combined with current table for complete historical red bug analysis
  - **Data Quality**: Historical lab results, generally complete and reliable

### Data Collection Strategy

#### Key Join Patterns Used Throughout Application

**Standard Air Site Analysis Pattern:**
```sql
-- Connect inspections with air site information and lab results
FROM public.dblarv_insptrt_current i
LEFT JOIN public.loc_breeding_sites b ON i.sitecode = b.sitecode
LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(i.sitecode, 7)
LEFT JOIN public.dblarv_sample_current s ON i.sampnum_yr = s.sampnum_yr 
  AND s.form_type = 'AIR' 
  AND (s.missing = FALSE OR s.missing IS NULL)
LEFT JOIN public.mattype_list_targetdose m ON i.matcode = m.matcode
WHERE (b.enddate IS NULL OR b.enddate >= i.inspdate)  -- Active sites during inspection
  AND b.air_gnd = 'A'                                 -- Air sites only
  AND b.geom IS NOT NULL                             -- Sites with valid coordinates
  AND i.action IN ('2', '4')                         -- Inspection actions only
```

**Note**: See "SQL Queries Reference" section below for complete, up-to-date query implementations.

#### Critical Data Integration Notes

**Sitecode Format Handling:**
- **Standard Format**: `190208-003` (7-digit section + dash + site num)
- **Directional Format**: `191102W010` (6-digit + direction + site num)
- **Join Logic**: `LEFT(sitecode, 7)` extracts section code for both formats
- **Example**: Both `190208-003` and `191102W010` → sectcodes `1902080` and `1911020`

**Site Record Temporal Matching:**
- **Challenge**: Sites have multiple records with different `enddate` values representing active periods
- **Solution**: Match inspection date with site active period: `(b.enddate IS NULL OR b.enddate >= i.inspdate)`
- **Deduplication**: Use `ROW_NUMBER() OVER (PARTITION BY i.sitecode, i.inspdate ORDER BY...)` to select best match

**Facility Data Source Priority:**
- **Preferred**: `COALESCE(g.facility, b.facility)` - Use gis_sectcode.facility first
- **Fallback**: loc_breeding_sites.facility when gis_sectcode data unavailable
- **Consistency**: Ensures facility data matches other MMCD applications

**Lab Sample Matching:**
- **Join Key**: `i.sampnum_yr = s.sampnum_yr` links inspections to lab samples
- **Air Samples Only**: Filter `s.form_type = 'AIR'` for air site samples
- **Valid Samples**: Filter `(s.missing = FALSE OR s.missing IS NULL)` includes both explicit FALSE and NULL values

**Treatment vs Site Acres:**
- **Site Acres**: `loc_breeding_sites.acres` = site capacity/potential treatment area
- **Treated Acres**: `dblarv_insptrt_*.acres` = actual area treated in specific application
- **Usage**: Site acres for capacity analysis, treated acres for actual treatment metrics

**Zone Assignment:**
- **P1 Sites**: `gis_sectcode.zone = '1'` (Primary zone)
- **P2 Sites**: `gis_sectcode.zone = '2'` (Secondary zone)
- **P3 Sites**: `gis_sectcode.zone = '3'` (Tertiary zone)
- **P4 Sites**: `gis_sectcode.zone = '4'` (Quaternary zone)
- **Missing Zones**: Some sites may not have zone assignments in gis_sectcode

**Red Bug Detection Logic:**
- **Detection**: `s.redblue = 'R'` indicates red bugs found in sample
- **Blue Bugs**: `s.redblue = 'B'` indicates blue bugs found (not red)
- **No Detection**: `s.redblue IS NULL` or no matching sample record

## Tab 1: Current Status

### Purpose
Display current status of air sites with inspection, treatment, and lab analysis data including red bug detection rates.

### Key Features
- **Value boxes** showing summary statistics:
  - Total air sites, sites with recent inspections
  - Sites needing treatment, sites with active treatments
  - Red bug detection rate, sites above larval threshold
- **Status Summary Chart** showing site distribution by status categories
- **Detailed Status Table** with individual site information and download capability
- **Configurable filters** - facility, priority, zone, larval threshold, analysis date
- **BTI override** - Configurable BTI granule effectiveness days

### Site Status Definitions
Air sites are classified into mutually exclusive status categories based on recent inspection and treatment data:

#### **Active Treatment**
- **Definition**: Site has treatment that is still effective as of analysis date
- **Logic**: `treatment_end_date >= analysis_date`
- **Calculation**: `treatment_end_date = last_treatment_date + effect_days`
- **Color**: Green (#2E8B57)
- **Priority**: Highest - Site is adequately treated

#### **Needs Treatment**
- **Definition**: Site has recent inspection showing larvae above threshold, no active treatment
- **Logic**: `last_inspection_numdip >= larvae_threshold AND (no treatment OR treatment expired)`
- **Threshold**: Configurable larvae threshold (default: 2 dips)
- **Color**: Red (#DC143C)
- **Priority**: High - Site requires immediate treatment attention

#### **Recently Inspected**
- **Definition**: Site has recent inspection below threshold, no active treatment needed
- **Logic**: `last_inspection_numdip < larvae_threshold AND no active treatment`
- **Threshold**: Below configurable larvae threshold
- **Color**: Blue (#4682B4)
- **Priority**: Medium - Site is monitored but doesn't need treatment

#### **In Lab**
- **Definition**: Site has recent inspection with sample sent to lab, awaiting results
- **Logic**: `has_recent_inspection AND has_sampnum_yr AND no lab results yet`
- **Sample Status**: Sample sent but `redblue` result not available
- **Color**: Orange (#FF8C00)
- **Priority**: Medium - Awaiting lab analysis for treatment decision

#### **No Recent Data**
- **Definition**: Site has no recent inspection or treatment data
- **Logic**: `no recent inspection AND no active treatment`
- **Timeframe**: Beyond analysis date window for recent activity
- **Color**: Gray (#808080)
- **Priority**: Low - Site status unknown, may need inspection

### Red Bug Metrics
Air site analysis includes specific metrics for red bug (Chironomus) detection:

#### **Red Bug Detection Rate**
- **Definition**: Percentage of lab samples showing red bugs
- **Calculation**: `(sites_with_red_bugs / sites_with_lab_results) * 100`
- **Lab Sample Required**: Only counts sites with valid air lab samples
- **Detection Logic**: `redblue = 'R'` in sample results

#### **Red Bug Sites Count**
- **Definition**: Number of sites with confirmed red bug detections
- **Logic**: Sites with `redblue = 'R'` in most recent lab sample
- **Timeframe**: Based on analysis date and recent activity window

### Data Flow
1. **Load Air Sites**: Query active air sites with zone/facility information
2. **Get Recent Inspections**: Latest inspection per site within analysis window
3. **Get Active Treatments**: Current treatments with effectiveness calculations
4. **Get Lab Results**: Link inspections to lab samples for red bug analysis
5. **Calculate Status**: Apply status logic rules to categorize each site
6. **Aggregate Metrics**: Summarize counts and percentages for value boxes and charts

## Tab 2: Map

### Purpose
Provide interactive map visualization of air sites with color-coded status markers and detailed popup information.

### Key Features
- **Interactive Leaflet Map** with multiple basemap options:
  - Streets (CartoDB Positron) - Default
  - Satellite (Esri World Imagery)  
  - Terrain (Esri World Topo)
  - OpenStreetMap
- **Color-coded markers** by site status:
  - **Green**: Active Treatment (#2E8B57)
  - **Red**: Needs Treatment (#DC143C)
  - **Blue**: Recently Inspected (#4682B4)
  - **Orange**: In Lab (#FF8C00)
  - **Gray**: No Recent Data (#808080)
- **Site popups** with detailed information:
  - Sitecode, facility, zone, priority, site acres
  - Treatment status and last inspection/treatment details
  - Lab results including red bug detection
  - Last material used and effectiveness days
- **Map legend** showing status color scheme
- **Shared filters** with Current Status tab (zone, facility, priority, larval threshold)

### Data Flow
1. **Load Spatial Data**: Query air sites with coordinates via `ST_Transform(ST_Centroid(geom), 4326)`
2. **Calculate Status**: Apply same status logic as Current Status tab
3. **Filter Sites**: Remove sites without valid coordinates (`geom IS NOT NULL`)
4. **Create Markers**: Generate color-coded circle markers for each site
5. **Generate Popups**: Format HTML popups with site and status details

### Coordinate Transformation
- **Source**: PostGIS geometry in UTM projection (EPSG:26915)
- **Target**: WGS84 decimal degrees (EPSG:4326) for web mapping
- **SQL Transform**: `ST_X(ST_Transform(ST_Centroid(geom), 4326))` for longitude
- **Coordinate Range**: Minnesota extent (~-93.8 to -92.8 lng, 44.6 to 45.4 lat)

## Tab 3: Historical Analysis

### Purpose
Analyze multi-year trends in air site inspection activity, red bug detection rates, and treatment effectiveness.

### Key Features
- **Historical Inspection Summary Table** showing:
  - Site identification (sitecode, facility, priority, zone, acres)
  - Inspection metrics (total inspections, inspections above threshold)
  - Red bug metrics (red bug inspections, red bug detection ratio)
  - Data source breakdown (archive vs current inspections)
  - Active years (years with inspection activity)
- **Configurable year range** - Start Year and End Year selectors
- **Same filters** as other tabs (facility, priority, zone, larval threshold)
- **Download capability** for complete historical dataset
- **Data source transparency** - Shows archive vs current data breakdown

### Historical Metrics Definitions

#### **Total Inspections**
- **Definition**: Count of ALL inspection visits to site (actions '2' and '4')
- **Includes**: Inspections with `numdip = 0` (no larvae found)
- **Includes**: Inspections without lab samples
- **Calculation**: `COUNT(*)` from combined inspection tables
- **Purpose**: Denominator for all percentage calculations

#### **Red Bug Inspections**  
- **Definition**: Count of inspections where lab analysis detected red bugs
- **Logic**: Inspections with `sampnum_yr` linking to samples with `redblue = 'R'`
- **Requires**: Valid lab sample with air form type
- **Calculation**: `COUNT(CASE WHEN redblue = 'R' THEN 1 END)`
- **Purpose**: Numerator for red bug detection rate

#### **Red Bug Ratio**
- **Definition**: Percentage of inspections resulting in red bug detection
- **Formula**: `(red_bug_inspections / total_inspections) * 100`
- **Interpretation**: Higher ratios indicate consistent red bug presence
- **Range**: 0% (no detections) to 100% (all inspections found red bugs)
- **Usage**: Compare sites and facilities for red bug prevalence

#### **Inspections Above Threshold**
- **Definition**: Count of inspections with larvae count >= threshold
- **Logic**: `numdip >= larvae_threshold` (default threshold: 2)
- **Purpose**: Indicates treatment need frequency
- **Calculation**: `COUNT(CASE WHEN numdip >= threshold THEN 1 END)`

#### **Archive vs Current Breakdown**
- **Archive Inspections**: Count from `dblarv_insptrt_archive` table
- **Current Inspections**: Count from `dblarv_insptrt_current` table  
- **Purpose**: Data source transparency and completeness verification
- **Expected Pattern**: Archive data for historical years, current data for recent dates

### Data Flow
1. **Combine Data Sources**: UNION ALL of archive and current inspection tables
2. **Filter Air Sites**: Join with `loc_breeding_sites` for `air_gnd = 'A'`
3. **Temporal Matching**: Match inspection dates with site active periods
4. **Lab Sample Linking**: Join with combined sample tables via `sampnum_yr`
5. **Facility Integration**: Use `gis_sectcode.facility` for consistency
6. **Aggregate by Site**: Group by sitecode and calculate metrics
7. **Format Results**: Convert arrays and format percentages for display

## Code Organization

### File Structure
```
air_sites_simple/
├── app.R                    # Main app - UI orchestration and server logic  
├── ui_helper.R              # UI component functions and input controls
├── data_functions.R         # Current data queries and processing
├── historical_functions.R   # Historical analysis queries and processing
└── NOTES.md                # This file
```

### Key Functions by File

#### app.R
- `server()` - Main server function with reactive data loading and output rendering
- **Current Status Tab**: Value boxes, status chart, detailed table with download
- **Map Tab**: Interactive leaflet map with color-coded status markers  
- **Historical Tab**: Historical inspection summary table with download

#### data_functions.R
- `get_air_sites_data()` - Loads current air site data with status calculations
- `get_db_connection()` - Database connection management (from shared/db_helpers.R)
- **Status Calculations**: Determines site categories based on inspection/treatment data
- **Lab Integration**: Links inspections to lab samples for red bug analysis
- **Spatial Support**: Coordinate extraction for mapping functionality

#### historical_functions.R  
- `get_historical_inspection_summary()` - Loads combined historical inspection data
- **Multi-table Integration**: UNION ALL of archive and current tables
- **Red Bug Analysis**: Calculates detection ratios across multiple years
- **Temporal Filtering**: Year range and date filtering for historical periods
- **Data Source Tracking**: Tracks archive vs current data proportions

#### ui_helper.R
- `create_*_ui()` functions for major UI components
- **Filter Controls**: Facility, priority, zone, threshold, date selectors
- **Layout Helpers**: Consistent box styling and responsive design
- **Input Validation**: Ensures valid ranges and selections

## Data Collection and Processing Logic

### Current Status Analysis Pipeline

#### Air Site Identification
```sql
-- Base query for active air sites
SELECT b.sitecode, b.facility, b.priority, b.acres, b.air_gnd,
       g.zone, g.facility as gis_facility, g.fosarea
FROM loc_breeding_sites b
LEFT JOIN gis_sectcode g ON g.sectcode = LEFT(b.sitecode, 7)
WHERE b.air_gnd = 'A'                    -- Air sites only
  AND b.geom IS NOT NULL                 -- Sites with coordinates
  AND (b.enddate IS NULL OR b.enddate > analysis_date)  -- Active sites
```

#### Recent Inspection Data
```sql
-- Latest inspection per air site
WITH recent_inspections AS (
  SELECT i.sitecode, i.inspdate, i.numdip, i.sampnum_yr,
         ROW_NUMBER() OVER (PARTITION BY i.sitecode 
                           ORDER BY i.inspdate DESC) as rn
  FROM dblarv_insptrt_current i
  WHERE i.action IN ('2', '4')           -- Inspection actions
    AND i.inspdate <= analysis_date
)
SELECT * FROM recent_inspections WHERE rn = 1
```

#### Red Bug Lab Analysis
```sql
-- Link inspections to lab results
SELECT ls.sampnum_yr, ls.redblue, ls.form_type, ls.missing
FROM dblarv_sample_current ls
WHERE ls.form_type = 'AIR'               -- Air samples only
  AND (ls.missing = FALSE OR ls.missing IS NULL)  -- Valid samples
```

#### Treatment Effectiveness
```sql
-- Active treatments with end dates
SELECT t.sitecode, t.inspdate, t.matcode,
       t.inspdate + COALESCE(m.effect_days, 14) as treatment_end_date
FROM dblarv_insptrt_current t
LEFT JOIN mattype_list_targetdose m ON t.matcode = m.matcode
WHERE t.action IN ('3', 'A', 'D')       -- Treatment actions
  AND t.inspdate + COALESCE(m.effect_days, 14) >= analysis_date
```

### Historical Analysis Pipeline

#### Combined Data Sources
The historical analysis combines archive and current tables to provide complete multi-year perspective:

```sql
-- Inspection data from both sources
WITH combined_inspections AS (
  -- Archive inspection data
  SELECT sitecode, inspdate, sampnum_yr, numdip, action, 'archive' as source_table
  FROM dblarv_insptrt_archive
  WHERE action IN ('2', '4')
    AND EXTRACT(YEAR FROM inspdate) BETWEEN start_year AND end_year
  
  UNION ALL
  
  -- Current inspection data  
  SELECT sitecode, inspdate, sampnum_yr, numdip, action, 'current' as source_table
  FROM dblarv_insptrt_current
  WHERE action IN ('2', '4')
    AND EXTRACT(YEAR FROM inspdate) BETWEEN start_year AND end_year
)
```

#### Sample Data Integration
```sql
-- Combined sample results
WITH combined_samples AS (
  -- Archive sample data
  SELECT sampnum_yr, redblue, missing, form_type, 'archive' as source_table
  FROM dblarv_sample_archive
  WHERE form_type = 'AIR'
    AND (missing = FALSE OR missing IS NULL)
  
  UNION ALL
  
  -- Current sample data
  SELECT sampnum_yr, redblue, missing, form_type, 'current' as source_table  
  FROM dblarv_sample_current
  WHERE form_type = 'AIR'
    AND (missing = FALSE OR missing IS NULL)
)
```

#### Metric Calculations
All historical metrics are calculated **after** SQL returns raw records using R aggregation:

**Total Inspections per Site:**
```r
result$total_inspections <- combined_data %>%
  group_by(sitecode) %>%
  summarise(count = n(), .groups = 'drop')
```

**Red Bug Inspections per Site:**
```r
result$red_bug_inspections <- combined_data %>%
  filter(redblue == 'R') %>%
  group_by(sitecode) %>%
  summarise(count = n(), .groups = 'drop')
```

**Red Bug Detection Rate:**
```r
result$red_bug_ratio <- ifelse(result$total_inspections > 0,
  round((result$red_bug_inspections / result$total_inspections) * 100, 1),
  0)
```

## SQL Queries Reference

This section documents all SQL queries used in the air sites analysis app for reference and troubleshooting.

### 1. Current Air Sites Query
**Function**: `get_air_sites_data()` in `data_functions.R`  
**Purpose**: Load active air sites with comprehensive status analysis

```sql
WITH ActiveAirSites AS (
  SELECT 
    b.sitecode,
    COALESCE(g.facility, b.facility) as facility,
    b.acres,
    b.priority,
    g.zone,
    ST_X(ST_Centroid(ST_Transform(b.geom, 4326))) as longitude,
    ST_Y(ST_Centroid(ST_Transform(b.geom, 4326))) as latitude
  FROM loc_breeding_sites b
  LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(b.sitecode, 7)
  WHERE (b.enddate IS NULL OR b.enddate > analysis_date)
    AND b.air_gnd = 'A'
    AND b.geom IS NOT NULL
),

-- Get most recent inspections (actions 2, 4)
RecentInspections AS (
  SELECT 
    i.sitecode,
    i.inspdate as last_inspection_date,
    i.numdip,
    i.sampnum_yr,
    ROW_NUMBER() OVER (PARTITION BY i.sitecode ORDER BY i.inspdate DESC) as rn
  FROM dblarv_insptrt_current i
  WHERE i.inspdate <= analysis_date
    AND i.action IN ('2', '4')
    AND i.sitecode IN (SELECT sitecode FROM ActiveAirSites)
),

-- Get most recent treatments (actions 3, A, D)
RecentTreatments AS (
  SELECT 
    t.sitecode,
    t.inspdate as last_treatment_date,
    t.matcode,
    t.mattype,
    ROW_NUMBER() OVER (PARTITION BY t.sitecode ORDER BY t.inspdate DESC) as rn
  FROM dblarv_insptrt_current t
  WHERE t.inspdate <= analysis_date
    AND t.action IN ('3', 'A', 'D')
    AND t.matcode IS NOT NULL
    AND t.matcode != ''
    AND t.sitecode IN (SELECT sitecode FROM ActiveAirSites)
),

-- Get lab sample data for red/blue bug determination (AIR samples only)
LabSampleData AS (
  SELECT 
    ls.sampnum_yr,
    ls.redblue,
    ls.missing,
    CASE 
      WHEN ls.redblue = 'R' THEN 1
      ELSE 0
    END as has_red_bugs
  FROM dblarv_sample_current ls
  WHERE ls.sampnum_yr IS NOT NULL
    AND (ls.missing = FALSE OR ls.missing IS NULL)
    AND ls.form_type = 'AIR'
)

SELECT 
  a.sitecode,
  a.facility,
  a.priority,
  a.zone,
  a.acres,
  a.longitude,
  a.latitude,
  ri.last_inspection_date,
  ri.numdip,
  ri.sampnum_yr,
  rt.last_treatment_date,
  rt.matcode,
  rt.mattype as last_treatment_material,
  CASE WHEN rt.matcode = 'Bti_gran' THEN bti_effect_days 
       WHEN m.effect_days IS NOT NULL THEN m.effect_days 
       ELSE 14 END as effect_days,
  lsd.redblue,
  lsd.has_red_bugs,
  CASE WHEN ri.sampnum_yr IS NOT NULL THEN 1 ELSE 0 END as has_sample,
  CASE WHEN lsd.sampnum_yr IS NOT NULL THEN 1 ELSE 0 END as has_lab_result
FROM ActiveAirSites a
LEFT JOIN RecentInspections ri ON a.sitecode = ri.sitecode AND ri.rn = 1
LEFT JOIN RecentTreatments rt ON a.sitecode = rt.sitecode AND rt.rn = 1
LEFT JOIN LabSampleData lsd ON ri.sampnum_yr = lsd.sampnum_yr
LEFT JOIN mattype_list_targetdose m ON rt.matcode = m.matcode
ORDER BY a.facility, a.sitecode
```

**Key Points**:
- **Air Sites Only**: `b.air_gnd = 'A'` ensures only air treatment sites
- **Active Sites Only**: `(b.enddate IS NULL OR b.enddate > analysis_date)` excludes closed sites
- **Facility Priority**: `COALESCE(g.facility, b.facility)` uses gis_sectcode facility first
- **Coordinate Transform**: `ST_Transform(ST_Centroid(b.geom), 4326)` for web mapping
- **Latest Data Only**: `ROW_NUMBER()` window functions get most recent inspections/treatments
- **Lab Integration**: Links inspections to samples via `sampnum_yr` for red bug analysis
- **BTI Override**: Supports configurable BTI granule effectiveness days

### 2. Historical Air Sites Inspection Summary Query
**Function**: `get_historical_inspection_summary()` in `historical_functions.R`  
**Purpose**: Load comprehensive multi-year inspection analysis with red bug metrics

```sql
WITH combined_inspections AS (
  -- Archive inspection data
  SELECT 
    sitecode, inspdate, sampnum_yr, numdip, action, 'archive' as source_table
  FROM dblarv_insptrt_archive
  WHERE action IN ('2', '4')
    AND EXTRACT(YEAR FROM inspdate) BETWEEN start_year AND end_year
  
  UNION ALL
  
  -- Current inspection data
  SELECT 
    sitecode, inspdate, sampnum_yr, numdip, action, 'current' as source_table
  FROM dblarv_insptrt_current
  WHERE action IN ('2', '4')
    AND EXTRACT(YEAR FROM inspdate) BETWEEN start_year AND end_year
),

all_inspections AS (
  SELECT 
    b.sitecode,
    COALESCE(g.facility, b.facility) as facility,
    b.priority,
    g.zone,
    b.acres,
    i.inspdate,
    i.sampnum_yr,
    i.numdip,
    i.source_table,
    ROW_NUMBER() OVER (PARTITION BY i.sitecode, i.inspdate ORDER BY 
      CASE WHEN b.enddate IS NULL THEN 1 ELSE 0 END DESC,
      b.enddate DESC
    ) as rn
  FROM loc_breeding_sites b
  LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(b.sitecode, 7)
  INNER JOIN combined_inspections i ON b.sitecode = i.sitecode
  WHERE (b.enddate IS NULL OR b.enddate >= i.inspdate)
    AND b.air_gnd = 'A'
    AND b.geom IS NOT NULL
),

filtered_inspections AS (
  SELECT * FROM all_inspections WHERE rn = 1
),

combined_samples AS (
  -- Archive sample data
  SELECT 
    sampnum_yr, redblue, missing, form_type, 'archive' as source_table
  FROM dblarv_sample_archive
  WHERE form_type = 'AIR'
    AND (missing = FALSE OR missing IS NULL)
  
  UNION ALL
  
  -- Current sample data  
  SELECT 
    sampnum_yr, redblue, missing, form_type, 'current' as source_table
  FROM dblarv_sample_current
  WHERE form_type = 'AIR'
    AND (missing = FALSE OR missing IS NULL)
),

red_bug_samples AS (
  SELECT 
    fi.sitecode,
    COUNT(CASE WHEN cs.redblue = 'R' THEN 1 END) as red_bug_count,
    COUNT(CASE WHEN cs.redblue IS NOT NULL THEN 1 END) as total_samples
  FROM filtered_inspections fi
  LEFT JOIN combined_samples cs ON fi.sampnum_yr = cs.sampnum_yr
  GROUP BY fi.sitecode
)

SELECT 
  fi.sitecode,
  fi.facility,
  fi.priority,
  fi.zone,
  fi.acres,
  COUNT(*)::INTEGER as total_inspections,
  SUM(CASE WHEN fi.numdip >= larvae_threshold THEN 1 ELSE 0 END)::INTEGER as inspections_above_threshold,
  COALESCE(rbs.red_bug_count, 0)::INTEGER as red_bug_inspections,
  COALESCE(rbs.total_samples, 0)::INTEGER as samples_with_results,
  COUNT(CASE WHEN fi.source_table = 'archive' THEN 1 END)::INTEGER as archive_inspections,
  COUNT(CASE WHEN fi.source_table = 'current' THEN 1 END)::INTEGER as current_inspections,
  ARRAY_AGG(DISTINCT EXTRACT(YEAR FROM fi.inspdate) 
           ORDER BY EXTRACT(YEAR FROM fi.inspdate)) as years_with_inspections
FROM filtered_inspections fi
LEFT JOIN red_bug_samples rbs ON fi.sitecode = rbs.sitecode
GROUP BY fi.sitecode, fi.facility, fi.priority, fi.zone, fi.acres, 
         rbs.red_bug_count, rbs.total_samples
ORDER BY fi.sitecode
```

**Key Points**:
- **Combined Data Sources**: UNION ALL of archive and current tables for complete history
- **Air Sites Only**: Filters `b.air_gnd = 'A'` for air treatment sites
- **Temporal Matching**: `(b.enddate IS NULL OR b.enddate >= i.inspdate)` matches site active periods
- **Facility Consistency**: Uses `gis_sectcode.facility` for consistent facility data
- **Sample Integration**: Combines archive and current sample tables via UNION ALL
- **Red Bug Analysis**: Counts red bug detections via `redblue = 'R'` filtering
- **Data Source Tracking**: Tracks archive vs current inspection counts for transparency
- **Metric Calculation**: All key metrics calculated in single query for efficiency

### 3. Air Site Spatial Data Query (Map Functionality)
**Function**: Used within `get_air_sites_data()` with `include_geometry = TRUE`  
**Purpose**: Load air sites with coordinates for interactive mapping

```sql
SELECT 
  b.sitecode,
  COALESCE(g.facility, b.facility) as facility,
  b.priority,
  b.acres,
  g.zone,
  ST_X(ST_Transform(ST_Centroid(b.geom), 4326)) as lng,
  ST_Y(ST_Transform(ST_Centroid(b.geom), 4326)) as lat,
  -- Include inspection and treatment data for status calculation
  ri.last_inspection_date,
  ri.numdip,
  ri.sampnum_yr,
  rt.last_treatment_date,
  rt.matcode,
  lsd.redblue,
  lsd.has_red_bugs
FROM loc_breeding_sites b
LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(b.sitecode, 7)
-- ... same joins as Current Air Sites Query for status data
WHERE b.air_gnd = 'A'
  AND (b.enddate IS NULL OR b.enddate > analysis_date)
  AND b.geom IS NOT NULL
ORDER BY b.facility, b.sitecode
```

**Key Points**:
- **Air Sites Only**: `b.air_gnd = 'A'` filter for air treatment sites
- **Valid Coordinates**: `b.geom IS NOT NULL` ensures mappable sites
- **Coordinate Transform**: `ST_Transform(ST_Centroid(b.geom), 4326)` for web mapping standards
- **Status Data**: Includes same inspection/treatment joins for status calculation
- **Web Mapping Ready**: Returns longitude/latitude in WGS84 decimal degrees (EPSG:4326)

**Coordinate Transformation Details**:
- **Source Projection**: UTM Zone 15N (EPSG:26915) - Minnesota standard
- **Target Projection**: WGS84 (EPSG:4326) - Web mapping standard
- **Centroid Calculation**: `ST_Centroid()` finds geometric center of site polygon
- **Expected Range**: Minnesota bounds (~-94.6 to -89.5 lng, 43.5 to 49.4 lat)