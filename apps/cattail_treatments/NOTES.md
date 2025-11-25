# Cattail Treatments - Data Documentation

## Purpose
Documents the data sources, SQL queries, and aggregation logic for the cattail treatments dashboard. This file focuses exclusively on data definitions and processing.

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
    - `action = 'A'` - AIR treatment application
  - **Data Quality**: Contains ongoing treatments, some may be planned vs executed
  
- **`public.dblarv_insptrt_archive`** - Historical larval treatment records  
  - **Same structure as current table**
  - **Data tine**: Historical treatments from pervious years

#### Essential Supporting Tables
- **`public.loc_breeding_sites`** - Site master data
  - **Key Columns**: 
    - `sitecode` (unique site identifier)
    - `acres` (site size, not treated acres)
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

## Inspection Year Definition

### DOY-Based Seasonal Logic
Cattail inspections and treatments follow seasonal cycles that don't align with calendar years. The **inspection year** is defined as:

**Inspection Year N = Fall of Year N (Sept-Dec) + Summer of Year N+1 (May-Aug)**

Example: **Inspection Year 2024** includes:
- Fall 2024: September 1, 2024 - December 31, 2024 (DOY 244-365)
- Summer 2025: May 1, 2025 - August 1, 2025 (DOY 135-213)

### SQL Implementation
The inspection year is calculated in SQL using Day of Year (DOY):

```sql
-- Calculate inspection_year based on DOY
CASE
  WHEN EXTRACT(DOY FROM inspdate) BETWEEN 244 AND 365 THEN EXTRACT(YEAR FROM inspdate)
  WHEN EXTRACT(DOY FROM inspdate) BETWEEN 135 AND 213 THEN EXTRACT(YEAR FROM inspdate) - 1
  ELSE NULL
END as inspection_year
```

**DOY Ranges:**
- **Fall/Winter Season**: DOY 244-365 (Sept 1 - Dec 31) → Same calendar year
- **Spring/Summer Season**: DOY 135-213 (May 15 - Aug 1) → Previous calendar year

**Examples:**
- Inspection on September 15, 2024 (DOY 259) → Inspection Year 2024
- Inspection on July 1, 2025 (DOY 182) → Inspection Year 2024
- Treatment on November 10, 2024 → Applies to Inspection Year 2024
- Treatment on June 15, 2025 → Applies to Inspection Year 2024

### Metric Labels
Metrics that are measured "as of Aug 1" are labeled accordingly:
- "Sites Treated (as of Aug 1)"
- "% Treated of Need Treatment (as of Aug 1)"

This clarifies that treatment counts represent the snapshot at the end of the inspection year cycle.

## SQL Queries

### 1. Historical Cattail Inspection Data
**Function**: `get_historical_cattail_data()` in `historical_functions.R`  
**Purpose**: Load inspection records with DOY-based inspection year calculation

```sql
-- Current inspections
SELECT 
  i.sitecode,
  i.inspdate,
  i.numdip,
  sc.facility,
  sc.zone,
  sc.fosarea,
  EXTRACT(DOY FROM i.inspdate) as doy,
  CASE
    WHEN EXTRACT(DOY FROM i.inspdate) BETWEEN 244 AND 365 THEN EXTRACT(YEAR FROM i.inspdate)
    WHEN EXTRACT(DOY FROM i.inspdate) BETWEEN 135 AND 213 THEN EXTRACT(YEAR FROM i.inspdate) - 1
    ELSE NULL
  END as inspection_year
FROM public.dblarv_insptrt_current i
LEFT JOIN public.loc_breeding_sites b ON i.sitecode = b.sitecode
LEFT JOIN public.gis_sectcode sc ON LEFT(i.sitecode, 7) = sc.sectcode
WHERE i.action = '9'  -- Inspection action
  AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
  AND EXTRACT(DOY FROM i.inspdate) BETWEEN 135 AND 365  -- Only seasonal dates

UNION ALL

-- Archive inspections
SELECT 
  a.sitecode,
  a.inspdate,
  a.numdip,
  sc.facility,
  sc.zone,
  sc.fosarea,
  EXTRACT(DOY FROM a.inspdate) as doy,
  CASE
    WHEN EXTRACT(DOY FROM a.inspdate) BETWEEN 244 AND 365 THEN EXTRACT(YEAR FROM a.inspdate)
    WHEN EXTRACT(DOY FROM a.inspdate) BETWEEN 135 AND 213 THEN EXTRACT(YEAR FROM a.inspdate) - 1
    ELSE NULL
  END as inspection_year
FROM public.dblarv_insptrt_archive a
LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
LEFT JOIN public.gis_sectcode sc ON LEFT(a.sitecode, 7) = sc.sectcode
WHERE a.action = '9'
  AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
  AND EXTRACT(DOY FROM a.inspdate) BETWEEN 135 AND 365
```

**Key Points:**
- **DOY Filtering**: `BETWEEN 135 AND 365` excludes winter/early spring
- **Inspection Year**: Calculated in SQL using DOY CASE logic
- **Active Sites**: `enddate IS NULL` or active during analysis
- **Union**: Combines current + archive for complete history

### 2. Historical Cattail Treatment Data
**Function**: `get_historical_cattail_data()` in `historical_functions.R`  
**Purpose**: Load treatment records with DOY-based inspection year calculation

```sql
-- Current treatments
SELECT 
  t.sitecode,
  t.inspdate as trtdate,
  t.action,
  sc.facility,
  sc.zone,
  sc.fosarea,
  EXTRACT(DOY FROM t.inspdate) as doy,
  CASE
    WHEN EXTRACT(DOY FROM t.inspdate) BETWEEN 244 AND 365 THEN EXTRACT(YEAR FROM t.inspdate)
    WHEN EXTRACT(DOY FROM t.inspdate) BETWEEN 135 AND 213 THEN EXTRACT(YEAR FROM t.inspdate) - 1
    ELSE NULL
  END as inspection_year
FROM public.dblarv_insptrt_current t
LEFT JOIN public.gis_sectcode sc ON LEFT(t.sitecode, 7) = sc.sectcode
WHERE t.action IN ('3', 'A')  -- Treatment actions
  AND t.matcode IN (
    SELECT matcode 
    FROM public.mattype_list_targetdose
    WHERE prgassign_default = 'Cat'
  )
  AND EXTRACT(DOY FROM t.inspdate) BETWEEN 135 AND 365

UNION ALL

-- Archive treatments
SELECT 
  t.sitecode,
  t.inspdate as trtdate,
  t.action,
  sc.facility,
  sc.zone,
  sc.fosarea,
  EXTRACT(DOY FROM t.inspdate) as doy,
  CASE
    WHEN EXTRACT(DOY FROM t.inspdate) BETWEEN 244 AND 365 THEN EXTRACT(YEAR FROM t.inspdate)
    WHEN EXTRACT(DOY FROM t.inspdate) BETWEEN 135 AND 213 THEN EXTRACT(YEAR FROM t.inspdate) - 1
    ELSE NULL
  END as inspection_year
FROM public.dblarv_insptrt_archive t
LEFT JOIN public.gis_sectcode sc ON LEFT(t.sitecode, 7) = sc.sectcode
WHERE t.action IN ('3', 'A')
  AND t.matcode IN (
    SELECT matcode 
    FROM public.mattype_list_targetdose
    WHERE prgassign_default = 'Cat'
  )
  AND EXTRACT(DOY FROM t.inspdate) BETWEEN 135 AND 365
```

**Key Points:**
- **Material Validation**: Subquery ensures only cattail materials
- **Treatment Actions**: Both '3' and 'A' are valid treatment actions
- **Same DOY Logic**: Matches inspection query for consistency
- **Inspection Year**: Treatments mapped to inspection year using DOY

## Data Aggregation Logic

### Group Label Creation (R Processing)
**Function**: `create_historical_analysis_chart()` in `historical_functions.R`

Inspection and treatment data are grouped with optional zone separation:

```r
# For inspections
inspection_data <- inspection_data %>%
  mutate(
    group_label = case_when(
      group_by == "facility" & !combine_zones ~ paste(facility, "- Zone", zone),
      group_by == "facility" & combine_zones ~ facility,
      group_by == "foreman" & !combine_zones ~ paste("FOS", fosarea, "- Zone", zone),
      group_by == "foreman" & combine_zones ~ paste("FOS", fosarea),
      group_by == "zone" ~ paste("Zone", zone),
      TRUE ~ "All"
    )
  )

# Same logic for treatments
treatment_data <- treatment_data %>%
  mutate(
    group_label = case_when(
      # ... identical to inspection logic
    )
  )
```

**Key Points:**
- **Zone Separation**: `combine_zones = FALSE` adds "- Zone X" suffix
- **Consistency**: Both inspections and treatments use identical grouping
- **Foreman Format**: Uses "FOS 123" format for foreman display
- **Grouping Options**: facility, foreman, or zone-only grouping

### Metric Calculation (R Processing)
**Function**: `create_historical_analysis_chart()` in `historical_functions.R`
- **to be made**