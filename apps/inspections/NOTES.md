# Site Inspection Coverage Gaps - Technical Notes

## Overview
This Shiny app identifies mosquito breeding sites with inspection coverage gaps across MMCD operations. It analyzes sites that haven't been inspected within a specified time period to help prioritize field work and ensure comprehensive site coverage.

**Key Purpose**: Find sites that are "falling through the cracks" - either never inspected or not inspected recently enough to maintain proper surveillance coverage.

## Data Sources

### Tables Used

#### Primary Site Table
- **`loc_breeding_sites`** - Master site registry
  - Contains all mosquito breeding sites in the system
  - Fields: `sitecode`, `facility`, `air_gnd`, `priority`, `enddate`, `drone`
  - **CRITICAL FILTER**: `enddate IS NULL` for active sites only
  - **NOTE**: This is the authoritative source for active/inactive sites
  - **Drone Field**: `drone = 'Y'` identifies drone-designated sites

#### Inspection Tables (Current & Archive)
- **`dblarv_insptrt_current`** - Active larval inspection/treatment records
  - Contains ongoing and recent inspection records
  - Fields: `sitecode`, `inspdate`, `action`, `numdip`
  - Filter: `action IN ('1','2','4')` for actual inspections
  - Additional filter: `(action != '1' OR numdip IS NOT NULL)` to exclude incomplete dip counts

- **`dblarv_insptrt_archive`** - Historical larval inspection/treatment records
  - Contains closed/archived inspection records from previous years
  - Same schema as current table
  - Same filtering logic as current table
  - **UNION ALL** with current table for complete inspection history

#### Supporting Tables
- **`gis_sectcode`** - Section/zone geographic mapping
  - Links site codes to geographic zones and FOS areas
  - Zone mapping: P1 = zone '1', P2 = zone '2' (character fields)
  - FOS area assignment through `fosarea` field (maps to employee numbers)
  - Join pattern: `LEFT JOIN gis_sectcode sc ON left(sitecode,7) = sc.sectcode`

- **`employee_list`** - Field Operations Supervisor (FOS) information
  - Filter: `emp_type = 'FieldSuper'` AND `active = true`
  - Maps FOS areas (fosarea = emp_num) to foreman names and facilities
  - **CRITICAL MAPPING**: UI shows foreman names, but database fosarea contains emp_num

### Site Type Filtering
- **Air Sites**: `air_gnd = 'A'` - Aerial treatment sites
- **Ground Sites**: `air_gnd = 'G'` - Ground-accessible treatment sites

### Inspection Action Codes
- **Action '1'**: Dip count inspection (must have numdip value)
- **Action '2'**: Treatment applied
- **Action '4'**: Site inspection/assessment
- **Filter Logic**: `action IN ('1','2','4') AND (action != '1' OR numdip IS NOT NULL)`

### Priority System
- **RED**: Highest priority sites requiring frequent inspection
- **YELLOW**: Medium priority sites  
- **GREEN**: Lower priority sites
- **BLUE**: Routine monitoring sites
- **NULL**: Sites without assigned priority

### Drone Site Classification
- **Drone Sites**: Identified by `drone = 'Y'` in `loc_breeding_sites` table
- **Non-Drone Sites**: Sites where `drone IS NULL OR drone != 'Y'`
- **Filter Options**:
  - 🏠 **All Sites**: No drone filtering (default)
  - 🚁 **Drone Sites Only**: Show only `drone = 'Y'` sites
  - 🚶 **Non-Drone Sites Only**: Show only non-drone sites
  - 🔧 **Include Drone Sites**: Show all sites with drone highlighting

## Gap Detection Logic

### Core Algorithm
The app identifies sites with inspection gaps using a multi-step process:

1. **Active Site Identification**: Query `loc_breeding_sites` for active sites only (`enddate IS NULL`)
2. **Inspection History**: Query both current and archive inspection tables for all inspection records
3. **Most Recent Inspection**: Use `ROW_NUMBER() OVER (PARTITION BY sitecode ORDER BY inspdate DESC NULLS LAST)` to find latest inspection per site
4. **Gap Classification**: Compare most recent inspection date to gap threshold

### Gap Categories
- **Never Inspected**: Sites with no inspection records (`inspdate IS NULL`)
- **Inspection Gap**: Sites with most recent inspection older than threshold (`inspdate < gap_cutoff_date`)
- **Recently Inspected**: Sites inspected within the gap threshold (filtered out from results)

### Gap Threshold Calculation
- **User Input**: "Years Since Last Inspection" (default: 3 years)
- **Cutoff Date**: `gap_cutoff = current_date - (years_gap * 365 days)`
- **Query Filter**: `WHERE (inspdate IS NULL OR inspdate < gap_cutoff_date)`

## App Architecture

### UI Framework
- **shinydashboard**: Modern dashboard layout with collapsible boxes
- **Custom CSS**: Gradient styling, hover effects, professional appearance
- **Filter Panel**: Organized with logical grouping and clear labels
- **Refresh Button**: Prominent call-to-action with loading state

### Filter Controls

#### Site Type Selector
- **Air Sites (🚁)**: `air_gnd = 'A'`
- **Ground Sites (🚶)**: `air_gnd = 'G'`
- Single selection dropdown

#### Facility Filter
- **Multi-select**: Choose one or more facilities
- **Data Source**: `get_facility_choices()` from db_helpers
- **Display**: Full facility names (e.g., "South Rosemount")
- **Values**: Short codes (e.g., "Sr") for SQL filtering

#### FOS Area Filter  
- **Multi-select**: Choose one or more Field Operations Supervisors
- **Data Source**: `get_foreman_choices()` from db_helpers
- **Display**: "Foreman Name (Facility)" format (e.g., "Andrew C (Sr)")
- **Values**: Foreman shortnames (e.g., "Andrew C")
- **Database Mapping**: Shortnames mapped to emp_num for fosarea filtering

#### Zone Filter
- **Multi-select**: Zone 1, Zone 2, or both
- **Hardcoded Values**: Character '1' and '2' (zone is always these values)
- **Display**: "Zone 1", "Zone 2", "All Zones"

#### Priority Filter
- **Multi-select**: RED, YELLOW, GREEN, BLUE priorities
- **Display**: Color-coded priority levels
- **Database**: Maps directly to priority field values

#### Drone Site Filter
- **Radio Buttons**: Single selection for drone site filtering
- **Options**:
  -  "Drone Sites Only" - Filter to `drone = 'Y'` sites only
  -  "Non-Drone Sites Only" - Filter to exclude drone sites
  -  "Include Drone Sites" - Show all sites with drone highlighting
- **Database Filter**: Applied to `b.drone` field in main query

#### Gap Threshold
- **Numeric Input**: Years since last inspection (1-10 years)
- **Default**: 3 years
- **Purpose**: Define what constitutes an "inspection gap"

### Refresh Button Pattern
The app uses an **eventReactive** pattern to prevent automatic data loading:

## Data Processing Pipeline

### Step 1: Site Filtering (`filtered_sites` CTE)
```sql
WITH filtered_sites AS (
  SELECT 
    b.sitecode,
    sc.facility,
    sc.fosarea,
    sc.zone,
    b.air_gnd,
    b.priority
  FROM loc_breeding_sites b
  INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
  WHERE b.enddate IS NULL
  AND b.air_gnd = '[air_gnd_filter]'
  [additional_filters]
)
```

### Step 2: Inspection History (`site_inspections` CTE)
```sql
site_inspections AS (
  SELECT 
    fs.sitecode,
    fs.facility,
    fs.fosarea,
    fs.zone,
    fs.air_gnd,
    fs.priority,
    i.inspdate,
    i.action,
    i.numdip,
    ROW_NUMBER() OVER (PARTITION BY fs.sitecode ORDER BY i.inspdate DESC NULLS LAST) as rn
  FROM filtered_sites fs
  LEFT JOIN (
    SELECT sitecode, inspdate, action, numdip
    FROM dblarv_insptrt_current
    WHERE action IN ('1','2','4')
    AND (action != '1' OR numdip IS NOT NULL)
    UNION ALL
    SELECT sitecode, inspdate, action, numdip
    FROM dblarv_insptrt_archive
    WHERE action IN ('1','2','4')
    AND (action != '1' OR numdip IS NOT NULL)
  ) i ON fs.sitecode = i.sitecode
)
```

### Step 3: Gap Classification and Final Results
```sql
SELECT 
  sitecode,
  facility,
  fosarea,
  zone,
  air_gnd,
  priority,
  COALESCE(inspdate, '1900-01-01'::date) as last_inspection_date,
  numdip as last_numdip,
  CASE 
    WHEN inspdate IS NULL THEN 999999
    ELSE (CURRENT_DATE - inspdate::date)
  END as days_since_inspection,
  CASE 
    WHEN inspdate IS NULL THEN 'Never Inspected'
    WHEN inspdate < '[gap_cutoff_date]'::date THEN 'Inspection Gap'
    ELSE 'Recently Inspected'
  END as inspection_status
FROM site_inspections
WHERE rn = 1
AND (inspdate IS NULL OR inspdate < '[gap_cutoff_date]'::date)
ORDER BY COALESCE(inspdate, '1900-01-01'::date) ASC, sitecode
```

## Performance Optimization

### Query Optimization Techniques
1. **CTE Structure**: Breaks complex query into logical steps for better execution planning
2. **Early Filtering**: Applies site filters before joining inspection data
3. **UNION ALL**: Combines current and archive inspection tables efficiently  
4. **Indexed Joins**: Uses sitecode for joins (likely indexed)
5. **Window Functions**: `ROW_NUMBER()` efficiently finds most recent inspection per site

## Results Display

### Summary Statistics
The app displays a summary of findings in a gradient-styled box:
- **Total sites found** with inspection gaps
- **Breakdown**: Never inspected vs. sites with gaps
- **Time context**: Gap threshold in user-friendly format

Example: "Found 2,316 sites with gaps: 718 never inspected, 1,598 with 3+ year gaps"

### Data Table Features
- **Interactive filtering**: Search and sort capabilities
- **Responsive design**: Adapts to screen size  
- **Pagination**: Handles large result sets efficiently
- **Export options**: CSV download for further analysis

### Column Definitions
- **Site Code**: Unique identifier for the breeding site
- **Facility**: MMCD facility managing the site (full name display)
- **FOS Area**: Field Operations Supervisor responsible (numeric code)
- **Zone**: P1 or P2 geographic zone designation
- **Priority**: Site priority level (RED/YELLOW/GREEN/BLUE)
- **Last Inspection**: Date of most recent inspection (1900-01-01 if never inspected)
- **Last Numdip**: Mosquito dip count from most recent inspection
- **Days Since**: Number of days since last inspection (999999 if never inspected)
- **Status**: "Never Inspected" or "Inspection Gap"

## Code Organization

### File Structure
```
inspections/
├── app.R                     # Main app - UI orchestration and server logic
├── ui_helper.R              # UI definition and styling
├── data_functions.R         # Database queries and data processing
├── display_functions.R      # Table formatting and display helpers
└── NOTES.md                 # This file
```

### Key Functions by File

#### app.R
- **UI Definition**: Calls `create_main_ui()` for dashboard layout
- **Server Logic**: Manages reactive data and filter interactions  
- **Event Handling**: Refresh button triggers data loading
- **Output Rendering**: Summary statistics and data table display

#### ui_helper.R
- `create_main_ui()` - Creates shinydashboard layout with custom CSS
- `get_facility_display_choices()` - Facility filter options with full names
- `get_fosarea_display_choices()` - FOS area filter options using foreman data
- Custom CSS for modern styling and responsive design

#### data_functions.R
- `get_inspection_gaps()` - Main query function with optimized CTEs
- `get_site_choices()` - Helper for site characteristic options
- Filter mapping logic (especially critical FOS area mapping)
- Error handling for database connection issues

#### display_functions.R
- `render_gap_table()` - Formats inspection gaps data for DT display
- Column formatting and styling
- Export configuration
- Table interaction setup

## SQL Queries Reference

### 1. Main Inspection Gaps Query
**Function**: `get_inspection_gaps()` in `data_functions.R`  
**Purpose**: Find sites with inspection coverage gaps

```sql
-- Step 1: Get filtered active sites
WITH filtered_sites AS (
  SELECT 
    b.sitecode,
    sc.facility,
    sc.fosarea,
    sc.zone,
    b.air_gnd,
    b.priority
  FROM loc_breeding_sites b
  INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
  WHERE (b.enddate IS NULL OR b.enddate > CURRENT_DATE - INTERVAL '2 years')
  AND b.air_gnd = '[air_gnd_filter]'
  [additional_site_filters]
),
-- Step 2: Get most recent inspection per site
site_inspections AS (
  SELECT 
    fs.sitecode,
    fs.facility,
    fs.fosarea,
    fs.zone,
    fs.air_gnd,
    fs.priority,
    i.inspdate,
    i.action,
    i.numdip,
    ROW_NUMBER() OVER (PARTITION BY fs.sitecode ORDER BY i.inspdate DESC NULLS LAST) as rn
  FROM filtered_sites fs
  LEFT JOIN (
    SELECT sitecode, inspdate, action, numdip
    FROM dblarv_insptrt_current
    WHERE action IN ('1','2','4')
    AND (action != '1' OR numdip IS NOT NULL)
    UNION ALL
    SELECT sitecode, inspdate, action, numdip
    FROM dblarv_insptrt_archive
    WHERE action IN ('1','2','4')
    AND (action != '1' OR numdip IS NOT NULL)
  ) i ON fs.sitecode = i.sitecode
)
-- Step 3: Filter to gap sites only
SELECT 
  sitecode,
  facility,
  fosarea,
  zone,
  air_gnd,
  priority,
  COALESCE(inspdate, '1900-01-01'::date) as last_inspection_date,
  numdip as last_numdip,
  CASE 
    WHEN inspdate IS NULL THEN 999999
    ELSE (CURRENT_DATE - inspdate::date)
  END as days_since_inspection,
  CASE 
    WHEN inspdate IS NULL THEN 'Never Inspected'
    WHEN inspdate < '[gap_cutoff_date]'::date THEN 'Inspection Gap'
    ELSE 'Recently Inspected'
  END as inspection_status
FROM site_inspections
WHERE rn = 1
AND (inspdate IS NULL OR inspdate < '[gap_cutoff_date]'::date)
ORDER BY COALESCE(inspdate, '1900-01-01'::date) ASC, sitecode
```

**Key Features**:
- **Three-step CTE structure** for optimal performance
- **UNION ALL** combines current and archive inspection tables
- **Window function** finds most recent inspection per site
- **Gap classification** separates never-inspected from gap sites
- **Filter flexibility** supports all UI filter combinations

### 2. Site Choices Query
**Function**: `get_site_choices()` in `data_functions.R`  
**Purpose**: Get available filter options from site data

```sql
SELECT DISTINCT facility, fosarea, zone 
FROM gis_sectcode 
ORDER BY facility, fosarea, zone
```

**Key Points**:
- Provides available options for dynamic filter population
- Currently unused in favor of db_helpers lookup functions
- Could be used for dynamic zone choices if needed

### 3. Foreman Lookup Query
**Function**: `get_foremen_lookup()` in `shared/db_helpers.R`  
**Purpose**: Map foreman shortnames to employee numbers for FOS area filtering

```sql
SELECT 
  emp_num,
  shortname,
  facility
FROM employee_list 
WHERE emp_type = 'FieldSuper'
  AND active = true 
  AND facility IS NOT NULL
ORDER BY facility, shortname
```

### 4. Basic Site Count Query (Debugging)
**Purpose**: Verify data availability and filter effectiveness

```sql
SELECT COUNT(*) as total_sites
FROM loc_breeding_sites 
WHERE enddate IS NULL
```

**Usage**: Troubleshooting when no results found

### 5. Air/Ground Distribution Query (Debugging)
**Purpose**: Check site type distribution

```sql
SELECT air_gnd, COUNT(*) as count
FROM loc_breeding_sites 
WHERE enddate IS NULL
GROUP BY air_gnd
ORDER BY air_gnd
```

**Expected Results**:
- A: ~28K air sites
- G: ~227K ground sites  
- NULL: Small number of unclassified sites

### 6. Zone Distribution Query (Debugging)
**Purpose**: Verify zone mapping and counts

```sql
SELECT sc.zone, COUNT(*) as count
FROM loc_breeding_sites b
INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
WHERE b.enddate IS NULL
AND b.air_gnd = 'A'
GROUP BY sc.zone
ORDER BY sc.zone
```

**Key Points**:
- Confirms zone values are character '1' and '2'
- Tests sitecode join pattern
- Verifies geographic distribution

### 7. Inspection Statistics Query (Debugging)
**Purpose**: Verify inspection data availability

```sql
SELECT 
  COUNT(*) as total_inspections,
  COUNT(DISTINCT sitecode) as unique_sites
FROM (
  SELECT sitecode, inspdate, action, numdip
  FROM dblarv_insptrt_current
  WHERE action IN ('1','2','4')
  AND (action != '1' OR numdip IS NOT NULL)
  UNION ALL
  SELECT sitecode, inspdate, action, numdip
  FROM dblarv_insptrt_archive
  WHERE action IN ('1','2','4')
  AND (action != '1' OR numdip IS NOT NULL)
) inspections
```

**Expected Results**:
- ~1.7M total inspection records
- ~128K unique sites with inspection history
- Confirms substantial inspection data exists

---

## Database Schema Notes

### Table Relationships
- `loc_breeding_sites` ← (1:1) → `gis_sectcode` (via sitecode prefix)
- `gis_sectcode` ← (1:1) → `employee_list` (via fosarea = emp_num)  
- `loc_breeding_sites` ← (1:many) → `dblarv_insptrt_current` (via sitecode)
- `loc_breeding_sites` ← (1:many) → `dblarv_insptrt_archive` (via sitecode)

### Critical Field Mappings
- **sitecode**: 7-character prefix matches gis_sectcode.sectcode
- **fosarea** (in gis_sectcode): Maps to emp_num in employee_list  
- **zone** (in gis_sectcode): Character values '1' (P1) or '2' (P2)
- **air_gnd** (in loc_breeding_sites): 'A' for air, 'G' for ground sites
- **action** (in inspection tables): '1','2','4' for valid inspections

### Data Quality Considerations
- **Active Sites Only**: CRITICAL - Must filter by `enddate IS NULL` for current operations
- **Drone Site Identification**: Use `drone = 'Y'` exclusively - no other values indicate drone sites
- **Valid Inspections**: Action codes must be validated and numdip required for dip counts
- **Complete History**: Archive and current tables must both be queried for full inspection history
- **Sitecode Consistency**: Join pattern handles multiple sitecode formats consistently

---

## Key Implementation Points

### Critical Requirements
1. **Active Sites Only**: Must filter `enddate IS NULL` 
2. **FOS Area Mapping**: Always map foreman shortnames to emp_num for database filtering 
3. **Drone Site Logic**: Use `drone = 'Y'` exclusively - no other values indicate drone sites
4. **Refresh Button Pattern**: Essential for performance - NO data queries until refresh clicked
5. **Zone Fields Are Character**: Zone '1' and '2' are stored as character, not numeric

### Performance Considerations
- **CTE Query Structure**: Three-step CTE approach provides optimal performance (~2.7 seconds)
- **Early Filtering**: Apply site filters before joining inspection data
- **UNION ALL Required**: Must combine current and archive inspection tables for complete history
- **Window Functions**: `ROW_NUMBER()` efficiently finds most recent inspection per site