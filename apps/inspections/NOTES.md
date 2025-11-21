# Site Inspection Analytics - Technical Notes

## Overview
This Shiny app provides comprehensive analytics for mosquito breeding site inspections across MMCD operations. It includes site coverage gap analysis, wet site frequency tracking, and larvae threshold monitoring to optimize field operations and surveillance effectiveness.

**Key Features**:
- **Site Analytics**: Total sites, wet frequency analysis, and summary statistics
- **Larvae Threshold Analysis**: Sites exceeding larvae counts with frequency tracking
- **Coverage Gaps**: Sites with inspection gaps or never inspected
- **Unified Filtering**: Consistent filter system across all analytics tabs

## Data Sources

### Tables Used in Queries

#### 1. `loc_breeding_sites` - Site Registry
**Entities Retrieved:**
- `sitecode` - Unique site identifier  
- `air_gnd` - Site type ('A'=Air, 'G'=Ground)
- `priority` - Site priority (RED, YELLOW, GREEN, BLUE)
- `enddate` - Site termination date (NULL for active sites)
- `drone` - Drone site designation ('Y' for drone sites)

**Usage:** Primary site information, filtered by `enddate IS NULL` for active sites only

#### 2. `gis_sectcode` - Geographic/Administrative Data (AUTHORITATIVE SOURCE)
**Entities Retrieved:**
- `sectcode` - 7-character section code (joins to left 7 chars of sitecode)
- `facility` - Facility name (Sr, Nr, Er, Wr, etc.)
- `fosarea` - FOS area code (4-digit formatted emp_num like "0203", "1904")  
- `zone` - Zone designation ('1' for P1, '2' for P2)

**Usage:** Authoritative source for all facility/fosarea/zone data used in filtering

#### 3. `dblarv_insptrt_current` - Current Inspection Records
**Entities Retrieved:**
- `sitecode` - Site identifier (joins to loc_breeding_sites)
- `inspdate` - Inspection date
- `action` - Action code ('1','2','3','4', etc.)
- `numdip` - Larvae dip count (numeric)
- `wet` - Wet condition ('0'=dry, '1'-'9'=wet levels, 'A'=flooded)

**Usage:** Recent inspection data with action filtering: `(action IN ('1','2','4') OR (action = '3' AND wet = '0'))`

#### 4. `dblarv_insptrt_archive` - Historical Inspection Records  
**Entities Retrieved:**
- Same fields as `dblarv_insptrt_current`
- Historical inspection records from previous years

**Usage:** Combined with current table via UNION ALL for complete inspection history

#### 5. `employee_list` - FOS Personnel Data
**Entities Retrieved:**
- `emp_num` - Employee number (maps to gis_sectcode.fosarea when formatted as 4-digit)
- `shortname` - Foreman short name (e.g., "Andrew M.", "Sarah K.")
- `facility` - Facility assignment
- `emp_type` - Employee type (filtered to 'FieldSuper')
- `active` - Active status (filtered to true)

**Usage:** Maps UI foreman choices to fosarea codes for filtering: `sprintf("%04d", emp_num)`


### Site Type Filtering
- **Air Sites**: `air_gnd = 'A'` - Aerial treatment sites
- **Ground Sites**: `air_gnd = 'G'` - Ground-accessible treatment sites
- **Both**: No air_gnd filtering applied

### Inspection Action Codes (BASED ON ACTUAL FILTERING LOGIC)
**Note:** Specific action descriptions not documented - filter logic based on inspection requirements:
- **Action '1'**: Always included as valid inspection
- **Action '2'**: Always included as valid inspection  
- **Action '3'**: Only included when `wet = '0'` (dry site inspection)
- **Action '4'**: Always included as valid inspection
- **Filter Logic:** `(action IN ('1','2','4') OR (action = '3' AND wet = '0'))`

### Wet Field Logic 
The `wet` field uses numeric values, not Y/N:
- **'0'**: Dry site (no standing water)
- **'1'-'9'**: Various levels of wet conditions (1=minimal, 9=very wet)
- **'A'**: Flooded site (equivalent to >100% wet, maximum wet condition)
- **Analysis**: Any wet value > 0 or = 'A' counts as "wet" for frequency calculations

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
  -  **All Sites**: No drone filtering (default)
  -  **Drone Sites Only**: Show only `drone = 'Y'` sites
  -  **Non-Drone Sites Only**: Show only non-drone sites

## App Architecture 

### Multi-Tab Dashboard
The app now features three main analytics tabs:

#### 1. Site Analytics Tab
- **Total Sites Count**: Active sites matching filters
- **Wet Frequency Analysis**: How often sites are found wet (wet > 0 or wet = 'A')
- **Summary Statistics**: Comprehensive site metrics
- **Shared Filtering**: Uses unified years_back filter

#### 2. Larvae Threshold Tab  
- **High Larvae Sites**: Sites exceeding larvae threshold with frequency analysis
- **Frequency Metrics**: How often sites exceed thresholds (not just maximum counts)
- **Threshold Configuration**: Adjustable larvae count threshold
- **Exceedance Tracking**: Total inspections, exceedances, and frequency percentages

#### 3. Coverage Gaps Tab
- **Inspection Gaps**: Sites with inspection coverage gaps
- **Never Inspected**: Sites without any inspection records
- **Gap Threshold**: Configurable years since last inspection

## Data Processing Pipeline

### Comprehensive Data Function
**Function**: `get_comprehensive_inspection_data()` in `data_functions.R`
**Purpose**: Single source for all inspection data with proper filtering

```sql
WITH filtered_sites AS (
  SELECT 
    b.sitecode,
    sc.facility,
    sc.fosarea, 
    sc.zone,
    b.air_gnd,
    b.priority,
    b.drone
  FROM loc_breeding_sites b
  INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
  WHERE (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
  [additional_site_filters]
),
all_inspections AS (
  SELECT 
    fs.*,
    i.inspdate,
    i.action,
    i.numdip,
    i.wet
  FROM filtered_sites fs
  LEFT JOIN (
    SELECT sitecode, inspdate, action, numdip, wet
    FROM dblarv_insptrt_current
    WHERE (action IN ('1','2','4') OR (action = '3' AND wet = '0'))
    UNION ALL
    SELECT sitecode, inspdate, action, numdip, wet
    FROM dblarv_insptrt_archive
    WHERE (action IN ('1','2','4') OR (action = '3' AND wet = '0'))
  ) i ON fs.sitecode = i.sitecode
)
SELECT * FROM all_inspections
ORDER BY sitecode, inspdate DESC
```

### Analytics Functions 

#### Wet Frequency Analysis
**Function**: `get_wet_frequency_from_data()`
**Logic**: 
- Filters data by years_back parameter
- Counts total inspections per site
- Counts "wet" inspections (wet > 0 or wet = 'A')
- Calculates wet frequency percentage
- Applies minimum inspection threshold

#### High Larvae Analysis  
**Function**: `get_high_larvae_sites_from_data()`
**Logic**:
- Filters to sites with numdip data within years_back timeframe
- Groups by site characteristics
- Calculates frequency metrics:
  - `total_inspections`: Count of all inspections
  - `threshold_exceedances`: Count of inspections >= threshold
  - `exceedance_frequency`: Percentage of inspections exceeding threshold
  - `max_numdip`, `avg_numdip`: Larvae count statistics
  - `first_high_date`, `last_high_date`: Date tracking

#### Summary Statistics
**Function**: `get_summary_stats_from_data()`
**Metrics**:
- Total active sites
- Sites with inspections vs. never inspected
- Inspection frequency distributions
- Wet site statistics
- Priority breakdown

---

## SQL Queries Reference 

### 1. Comprehensive Inspection Data Query 
**Function**: `get_comprehensive_inspection_data()` in `data_functions.R`  
**Purpose**: Single source for all inspection analytics

[SQL structure shown above in Data Processing Pipeline]

### 2. High Larvae Sites Analysis 
**Function**: `get_high_larvae_sites_from_data()` 
**Purpose**: Sites exceeding larvae thresholds with frequency analysis

```sql
filtered_data %>%
  filter(!is.na(numdip), 
         !is.na(inspdate),
         inspdate >= cutoff_date) %>%
  group_by(sitecode, facility, fosarea, zone, air_gnd, priority) %>%
  summarise(
    total_inspections = n(),
    threshold_exceedances = sum(numdip >= threshold, na.rm = TRUE),
    max_numdip = max(numdip, na.rm = TRUE),
    avg_numdip = round(mean(numdip, na.rm = TRUE), 1),
    last_high_date = max(inspdate[numdip >= threshold], na.rm = TRUE),
    first_high_date = min(inspdate[numdip >= threshold], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(threshold_exceedances > 0) %>%
  mutate(
    exceedance_frequency = round(100.0 * threshold_exceedances / total_inspections, 1)
  )
```

### 3. Wet Frequency Analysis 
**Function**: `get_wet_frequency_from_data()`
**Purpose**: Track how often sites have standing water

```sql
# R/dplyr equivalent of wet frequency calculation
filtered_data %>%
  filter(!is.na(inspdate), inspdate >= cutoff_date) %>%
  mutate(
    is_wet = case_when(
      wet == "A" ~ TRUE,           # A = flooded (>100%)
      wet == "0" ~ FALSE,          # 0 = dry
      as.numeric(wet) > 0 ~ TRUE,  # 1-9 = various wet levels
      TRUE ~ FALSE
    )
  ) %>%
  group_by(sitecode, facility, fosarea, zone, air_gnd, priority) %>%
  summarise(
    total_inspections = n(),
    wet_inspections = sum(is_wet, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(total_inspections >= min_inspections) %>%
  mutate(
    wet_frequency = round(100.0 * wet_inspections / total_inspections, 1)
  )
```
---

