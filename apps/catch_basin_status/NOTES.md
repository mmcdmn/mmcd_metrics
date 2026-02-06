# Catch Basin Status - Technical Notes

## Overview
This Shiny app tracks catch basin treatment status across MMCD facilities, providing comprehensive monitoring of wet catch basins and their treatment coverage. The app focuses on structures that hold water (wet status) and tracks their treatment history to identify coverage gaps and expiring treatments.

**Key Features**:
- **Status Overview**: Summary statistics and visual charts of catch basin treatment status
- **Detailed View**: Site-by-site breakdown with treatment details and download capabilities
- **Historical Analysis**: Multi-year trends in catch basin treatments
- **Unified Filtering**: Consistent filter system across all tabs

## Data Sources

### Core Database Tables and Columns

#### 1. `loc_catchbasin` - Catch Basin Registry
**Key Columns**:
- `gid` - Unique catch basin identifier (primary key)
- `sitecode` - Site identifier
- `facility` - Facility name
- `status_udw` - Wet/dry status:
  - **'W'** - Wet (catch basin contains water) - PRIMARY FOCUS
  - **'D'** - Dry (catch basin has no water)
  - **'U'** - Unknown (status not determined)
- `lettergrp` - Letter group classification
  - **'Z'** - Excluded from analysis (special designation)
- `enddate` - Termination date (NULL = active catch basin, NOT NULL = closed)

**Critical Filters**:
- **Active Sites**: `enddate IS NULL` - Only analyze catch basins that are still active
- **Wet Sites**: `status_udw = 'W'` - Focus on catch basins with standing water
- **Valid Sites**: `lettergrp <> 'Z'` - Exclude special Z-designated sites

**Usage**: Primary catch basin inventory and wet/dry status tracking

#### 2. `gis_sectcode` - Geographic Section Mapping (AUTHORITATIVE SOURCE)
**Key Columns**:
- `sectcode` - 7-character section code (joins to `LEFT(sitecode, 7)`)
- `facility` - Facility name (Sr, E, Sj, Wp, etc.)
- `fosarea` - FOS area code (4-digit formatted emp_num like "0203", "1904")
- `zone` - Zone designation:
  - **'1'** - P1 (Primary zone)
  - **'2'** - P2 (Secondary zone)

**Join Pattern**: `LEFT JOIN gis_sectcode sc ON LEFT(loc_catchbasin.sitecode, 7) = sc.sectcode`

**Usage**: Authoritative source for facility, fosarea, and zone assignments

#### 3. `dblarv_insptrt_current` - Current Treatment Records
**Key Columns**:
- `pkey_pg` - Primary key (links to treatment detail tables)
- `sitecode` - Site identifier
- `inspdate` - Inspection/treatment date
- `insptime` - Inspection/treatment time (for ordering when dates match)
- `matcode` - Material code (joins to mattype_list_targetdose)

**Usage**: Current year catch basin treatment records

#### 4. `dblarv_insptrt_archive` - Historical Treatment Records
**Key Columns**:
- Same structure as `dblarv_insptrt_current`
- Contains historical treatment data from previous years

**Usage**: Historical catch basin treatment data for multi-year analysis

#### 5. `dblarv_treatment_catchbasin` - Treatment Detail (Current)
**Key Columns**:
- `treatment_id` - Links to `dblarv_insptrt_current.pkey_pg`
- `catchbasin_id` - Links to `loc_catchbasin.gid`
- `status` - Treatment status:
  - **'T'** - Treated (treatmented)
  - **'S'** - Skipped (treatment planned but skipped)


**Join Pattern**: `JOIN dblarv_treatment_catchbasin ON dblarv_insptrt_current.pkey_pg = dblarv_treatment_catchbasin.treatment_id`

**Usage**: Links treatments to specific catch basins and provides treatment status flags

#### 6. `dblarv_treatment_cb_archive` - Treatment Detail (Archive)
**Key Columns**:
- Same structure as `dblarv_treatment_catchbasin`
- Historical treatment detail records

**Usage**: Historical treatment-to-catchbasin linkage

#### 7. `mattype_list_targetdose` - Material Effectiveness Data
**Key Columns**:
- `matcode` - Material identifier
- `effect_days` - Treatment effectiveness duration in days
- `days_retrt_early` - Days before expiration to flag as "expiring"

**Default Values**:
- **effect_days**: Defaults to 150 days if NULL
- **expiring threshold**: User-configurable (default 14 days)

**Usage**: Calculate treatment end dates and expiring status

#### 8. `employee_list` - FOS Personnel Data
**Key Columns**:
- `emp_num` - Employee number (maps to gis_sectcode.fosarea)
- `shortname` - Foreman short name
- `facility` - Facility assignment
- `emp_type` - Employee type (filtered to 'FieldSuper')
- `active` - Active status (filtered to true)

**FOS Code Format**: `sprintf("%04d", emp_num)` - Convert to 4-digit string for matching

**Usage**: Maps fosarea codes to readable foreman names for filtering

### Data Collection Strategy

#### Treatment Status Logic

The app calculates catch basin treatment status using age-based logic:

**Status Definitions**:
1. **Treated**: Treatment age ≤ effect_days AND status NOT 'S' or 'P'
2. **Expiring**: Treatment age > (effect_days - expiring_days) AND age ≤ effect_days
3. **Expired**: Treatment age > effect_days
4. **Skipped**: Treatment status = 'S' (planned but skipped)
5. **?**: Treatment status = 'P' ()
6. **Skipped-Expiring**: Skipped treatment nearing end of theoretical effectiveness

**Age Calculation**:
```sql
date_part('days', analysis_date::timestamp - inspdate::timestamp) AS age
```

**Treatment Status SQL Logic**:
```sql
CASE
  WHEN age > COALESCE(effect_days::integer, 150)::double precision 
    THEN 'expired'
  WHEN age > (COALESCE(effect_days::integer, 150) - expiring_days)::double precision 
    THEN CASE
      WHEN status = 'S' THEN 'skipped-expiring'
      ELSE 'expiring'
    END
  WHEN status = 'S' THEN 'skipped'
  WHEN status = 'P' THEN 'planned'
  ELSE 'treated'
END AS active_status
```

#### Key Join Patterns

**Standard Treatment Query**:
```sql
-- Get latest treatment per catch basin with status calculation
SELECT DISTINCT ON (loc_catchbasin.gid)
  loc_catchbasin.gid,
  loc_catchbasin.facility,
  LEFT(loc_catchbasin.sitecode, 7) AS sectcode,
  loc_catchbasin.status_udw,
  dblarv_insptrt_current.pkey_pg AS insptrt_id,
  date_part('days', analysis_date::timestamp - 
    dblarv_insptrt_current.inspdate::timestamp) AS age,
  dblarv_treatment_catchbasin.status,
  mattype_list_targetdose.effect_days
FROM dblarv_insptrt_current
JOIN dblarv_treatment_catchbasin 
  ON dblarv_insptrt_current.pkey_pg = dblarv_treatment_catchbasin.treatment_id
JOIN loc_catchbasin 
  ON dblarv_treatment_catchbasin.catchbasin_id = loc_catchbasin.gid
JOIN mattype_list_targetdose USING (matcode)
LEFT JOIN gis_sectcode sc ON LEFT(loc_catchbasin.sitecode, 7) = sc.sectcode
WHERE dblarv_insptrt_current.inspdate <= analysis_date
  AND loc_catchbasin.status_udw = 'W'
  AND loc_catchbasin.lettergrp <> 'Z'
ORDER BY loc_catchbasin.gid, 
         dblarv_insptrt_current.inspdate DESC, 
         dblarv_insptrt_current.insptime DESC
```

**Note**: `DISTINCT ON (loc_catchbasin.gid)` ensures only the most recent treatment per catch basin is returned.

## Tab 1: Status Overview

### Purpose
Display current status of wet catch basins with treatment coverage statistics and visual charts.

### Key Features
- **Value boxes** showing summary statistics:
  - Total wet catch basins
  - Wet catch basins with active treatment
  - Treatment coverage percentage
  - Expiring treatments count
  - Expired treatments count
- **Layered bar chart** showing treatment status by group (MMCD, facility, FOS, or section)
- **Site Filter** - All Sites, Expiring Only, or Expiring + Expired
- **Expiring Days slider** - Configurable threshold for expiring treatments (1-60 days, default 14)
- **Date simulation** - "Pretend Today is" date picker for status analysis
- **Zone filters** - P1 Only, P2 Only, P1 and P2 Separate, Combined P1+P2
- **Group By selector** - All MMCD, Facility, FOS, or Section

### Metrics Explained

**Total Wet Catch Basins**:
- Count of all catch basins with `status_udw = 'W'`
- Filtered by: `enddate IS NULL` (active) AND `lettergrp <> 'Z'` (valid sites)
- This is the denominator for coverage calculations

**Wet CB with Active Treatment**:
- Count of wet catch basins with active_status IN ('treated', 'expiring')
- Includes both fully active and expiring treatments
- This is the numerator for coverage percentage

**Treatment Coverage %**:
- Formula: `(Active Treatment Count / Total Wet CB Count) × 100`
- Indicates percentage of wet catch basins with current protection
- Color-coded: Green (≥75%), Orange (50-74%), Red (<50%)

**Expiring**:
- Count of catch basins with active_status = 'expiring'
- Treatments nearing expiration within user-defined threshold (default 14 days)
- Alerts operators to catch basins needing retreatment soon

**Expired**:
- Count of catch basins with active_status = 'expired'
- Treatments that have exceeded their effective duration
- Indicates catch basins needing immediate attention

### Visualization

**Layered Bar Chart**:
- **Gray background bar**: Total wet catch basins (shows total inventory)
- **Green bar**: Catch basins with active treatments (treated + expiring)
- **Orange overlay**: Expiring treatments (subset of green bar)

**Color Mapping**:
- **Facility grouping**: Uses facility-specific colors from db_helpers
- **FOS grouping**: Inherits facility colors based on FOS assignment
- **Section grouping**: Inherits facility colors based on section location
- **MMCD All**: Uses status colors (green for active, orange for expiring)

**Zone Handling**:
- **Combined zones**: Aggregates P1 and P2 into single bars per group
- **Separate zones**: Shows P1 and P2 as distinct bars (e.g., "Anoka P1", "Anoka P2")
- **Zone differentiation**: P1 = solid colors, P2 = faded (alpha transparency)

## Tab 2: Detailed View

### Purpose
Provide detailed site-by-site view of catch basin treatment status with filtering and download capabilities.

### Key Features
- **Detailed data table** showing:
  - Facility, Zone, FOS area, Section code
  - Total wet catch basins
  - Active treatments count
  - Expiring treatments count
  - Expired treatments count
  - Treatment coverage percentage
  - Untreated count
- **Download CSV** functionality for filtered data
- **Same filters** as Status Overview tab
- **Sortable columns** and searchable interface
- **Pagination** for large datasets

### Data Table Features
- **Responsive design** with horizontal scrolling
- **Center-aligned** numeric columns
- **Percentage formatting** (1 decimal place)
- **Row highlighting** on hover
- **Conditional formatting** for coverage percentages

## Tab 3: Historical Analysis

### Purpose
Analyze multi-year trends in catch basin treatment activity across time periods.

### Key Features
- **Display Metric selector**:
  - **Yearly - Total Treatments**: Count of all treatment instances (catch basins treated multiple times contribute multiple counts)
  - **Yearly - Unique Wet CB Treated**: Count of unique catch basins treated (each counted once regardless of retreatment frequency)
  - **Weekly - Active Treatments**: Count of treatments active on Friday of each week (shows treatment coverage over time)
- **Time Period selector**: Weekly or Yearly aggregation
- **Year Range controls**: Start Year and End Year (configurable range)
- **Chart Type selector**: Line, Bar, Area, or Stacked charts
- **Zone filter**: P1, P2, or both (shared with other tabs)
- **Facility/FOS filters**: Shared with other tabs
- **Group By selector**: All MMCD, Facility, or FOS

### Data Flow

1. **Query historical data** via `get_catch_basin_historical_data()`:
   - Combines current and archive treatment tables
   - Filters for wet catch basins (`status_udw = 'W'`)
   - Excludes Z-designated sites (`lettergrp <> 'Z'`)
   - Applies year range filter

2. **Process time periods**:
   - **Weekly**: Create week numbers (YYYY-W## format) based on Friday of each week
   - **Yearly**: Use year values directly

3. **Aggregate by metric**:
   - **Total Treatments**: Count all treatment records `n()`
   - **Unique Wet CB**: Count distinct catch basins `n_distinct(catchbasin_id)`
   - **Active Treatments (weekly)**: For each Friday, count treatments where:
     - `inspdate + effect_days >= friday_date` (treatment still active on Friday)


## Refresh Pattern

### Current Tab Refresh
- Triggered by **Refresh** button
- Captures all filter inputs when clicked:
  - Zone filter (P1, P2, combined, or separate)
  - Facility filter
  - Foreman filter
  - Group by selection
  - Custom analysis date
  - Expiring days threshold
  - Expiring filter (all, expiring only, expiring + expired)

### Historical Tab Refresh
- Triggered by **Historical Refresh** button
- Captures historical-specific inputs:
  - Display metric selection
  - Time period (weekly/yearly)
  - Year range (start/end)
  - Chart type
  - All shared filters (zone, facility, foreman)

**Rationale**: Separate refresh buttons allow users to configure complex filter combinations and query expensive data only when ready, improving performance and user control.

## Function Reference

### Data Functions (data_functions.R)

**`load_catch_basin_data(facility_filter, foreman_filter, zone_filter, custom_today, expiring_days)`**
- Loads catch basin status data with treatment calculations
- Applies facility, foreman, and zone filters
- Calculates treatment age and status for custom analysis date
- Returns aggregated data by facility, zone, fosarea, and sectcode

**`process_catch_basin_data(data, group_by, combine_zones, expiring_filter)`**
- Processes raw data into grouped summaries
- Applies expiring filter (all sites, expiring only, or expiring + expired)
- Groups by mmcd_all, facility, foreman, or sectcode
- Calculates percentages and untreated counts
- Returns processed data with display_name and group_name fields

### Historical Functions (historical_functions.R)

**`get_catch_basin_historical_data(start_year, end_year, facility_filter, zone_filter, foreman_filter)`**
- Queries historical treatment data from current and archive tables
- Combines data using UNION ALL
- Filters by year range, facility, zone, and foreman
- Returns treatment records with sectcode, facility, zone, fosarea, dates, and effect_days

**`process_historical_data(data, time_period, display_metric, group_by, combine_zones)`**
- Processes historical data into time-series format
- Creates weekly or yearly aggregations
- Calculates selected metric (total treatments, unique catch basins, or active treatments)
- Groups by selected dimension (all MMCD, facility, or FOS)
- Returns time-series data ready for charting

### Display Functions (display_functions.R)

**`create_status_chart(data, group_by, expiring_filter, theme)`**
- Creates layered bar chart with status visualization
- Uses theme-aware colors
- Implements three-layer design (total wet, active, expiring)
- Returns ggplot object

**`create_details_table(data)`**
- Creates interactive DataTable with catch basin details
- Formats percentages and numeric columns
- Implements sorting and searching
- Returns DT::datatable object

### UI Helper Functions (ui_helper.R)

**`catch_basin_ui()`**
- Creates complete UI structure
- Implements sidebar filter panel
- Creates tabbed main panel (Overview, Detailed View, Historical Analysis)
- Returns fluidPage UI object

**`create_sidebar_filters()`**
- Creates comprehensive filter panel
- Includes all filter inputs (zone, facility, foreman, date, expiring days)
- Implements refresh buttons for current and historical tabs
- Returns tagList of filter inputs

**`create_overview_value_boxes()`**
- Creates value box container for overview metrics
- Returns fluidRow with 5 value boxes

## File Structure

```
apps/catch_basin_status/
├── app.R                       # Main application UI and server logic
├── data_functions.R            # Data loading and processing
├── historical_functions.R      # Historical data queries and processing
├── display_functions.R         # Chart and table creation
├── ui_helper.R                 # UI component functions
└── NOTES.md                    # This file - technical documentation
```

## Notes and Considerations

### Catch Basin vs Site Distinction
- **Catch Basins**: Individual storm drain structures (gid in loc_catchbasin)
- **Sites**: Can contain multiple catch basins (sitecode grouping)
- **This app**: Focuses on individual catch basin tracking, not site-level aggregation

### Treatment Status Priority
1. **Expired**: Highest priority - needs immediate treatment
2. **Expiring**: Second priority - needs treatment soon
3. **Treated**: Actively protected
4. **Skipped/Planned**: Special statuses for operational tracking

### Performance Considerations
- Historical queries can be expensive (multi-year data)
- Separate refresh buttons allow users to control when expensive queries execute
- DISTINCT ON clause ensures efficient retrieval of latest treatment per catch basin
- Zone filtering should be applied early in query for best performance

### Data Quality Notes
- **lettergrp = 'Z'**: Special designation excluded from all analyses
- **status_udw**: Maintained through field inspections, may have lag time
- **enddate**: Some catch basins may have enddate set in future for planned closures
- **Treatment dates**: Archive table contains complete historical record; current table updated in real-time

### Future Enhancements
- Add spatial mapping tab showing catch basin locations
- Implement treatment frequency analysis (retreatment intervals)
- Add material effectiveness comparison
- Include weather correlation analysis for wet/dry status changes
