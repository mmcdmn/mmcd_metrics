# Structure Treatment Progress - Technical Notes

## Overview
This Shiny app tracks mosquito control structure treatments across two perspectives:
1. **Current Progress** - Active and expiring treatments grouped by facility or FOS
2. **Historical Trends** - Year-over-year treatment activity comparison

## Data Sources

### Tables Used

#### Primary Treatment Tables
- **`dblarv_insptrt_current`** - Active structure treatment records
  - Contains ongoing treatments for current operations
  - Fields: `sitecode`, `facility`, `inspdate`, `matcode`, `list_type`
  - **list_type = 'STR'** identifies structure treatments
  - **NOTE**: Does NOT have `enddate` column
  
- **`dblarv_insptrt_archive`** - Historical structure treatment records
  - Contains closed/archived treatments from previous years
  - Same schema as current table
  - **NOTE**: Does NOT have `enddate` column

- **`mattype_list_targetdose`** - Treatment material effectiveness
  - Links material codes to effect duration
  - Fields: `matcode`, `effect_days`
  - **Effect Days**: Number of days the treatment remains effective
  - **Default**: 30 days if material code not found

#### Supporting Tables
- **`loc_cxstruct`** - Structure location and characteristics
  - **CRITICAL**: This is the ONLY table with `enddate` column
  - Contains all structure definitions
  - Fields: `sitecode`, `facility`, `foreman`, `s_type`, `priority`, `status_udw`, `zone`, `startdate`, `enddate`
  - Tracks when structures are opened and closed
  - Used to filter out structures that no longer exist
  - **Status Codes**:
    - **'D'** - Dry (structure has no water)
    - **'W'** - Wet (structure contains water)
    - **'U'** - Unknown (status not determined)

### Zone Filtering
- **P1 Only** (`zone = '1'`) - Primary zone structures only
- **P2 Only** (`zone = '2'`) - Secondary zone structures only
- **P1 and P2 Separate** (`zone = '1,2'`) - Both zones shown separately in chart
- **Combined P1+P2** (`zone = 'combined'`) - Both zones aggregated together

### Query Pattern for Proper Enddate Filtering

**CRITICAL**: Use `gis_sectcode` table for zone, foreman (fosarea), AND facility data to ensure 100% data coverage and consistency.

```sql
-- OPTIMAL PATTERN: Join with gis_sectcode for zone, fosarea, and facility
SELECT 
  trt.sitecode,
  trt.inspdate,
  gis.facility,
  gis.fosarea as foreman,
  COALESCE(mat.effect_days, 30) AS effect_days,
  loc.s_type,
  loc.priority,
  loc.status_udw as status,
  gis.zone
FROM public.dblarv_insptrt_current trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode  
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE trt.list_type = 'STR'
  AND (loc.enddate IS NULL OR loc.enddate > CURRENT_DATE)
  [... additional filters ...]

-- WRONG: Using loc.zone, loc.foreman, or trt.facility causes data inconsistencies
SELECT trt.sitecode, loc.zone, loc.foreman, trt.facility, ...
FROM public.dblarv_insptrt_current trt
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
-- This query has poor zone/foreman coverage AND facility mismatches!
```

**Why gis_sectcode for ALL three fields?**
- **Zone Coverage**: `gis.zone` provides 100% coverage (vs ~78% from `loc.zone`)
- **Foreman Field**: `gis.fosarea` is the proper foreman identifier (more reliable than `loc.foreman`)
- **Facility Consistency**: `gis.facility` matches with fosarea, preventing "strange people" showing up for a facility
  - Using `trt.facility` or `loc.facility` can result in mismatched facility/foreman combinations
  - All three fields (zone, fosarea, facility) come from the same authoritative source
- **Sectcode Join**: `loc.sectcode` in `loc_cxstruct` matches `gis.sectcode` exactly
- **Tested Results**: 3,395 treatments in last 90 days all have zone data when using `gis_sectcode`

## Tab 1: Current Progress

### Purpose
Monitor structures with active treatments and those expiring soon, grouped by facility or Field Operations Supervisor (FOS).

### Key Features
- **Days Until Expiration slider** (1-30 days) - Define "expiring soon" threshold
- **Pretend Today Is date picker** - Analyze data as of a specific date (useful for historical analysis)
- **Structure Status checkboxes** - Filter by Dry/Wet/Unknown status
- **Facility filter** - Multi-select facilities (defaults to "all")
- **Group By selector** - View by Facility, FOS, or All MMCD combined
- **Zone Display** - P1 only, P2 only, separate, or combined

### Treatment Categories
1. **Active** - Treatments still effective (expiration date > custom_today)
2. **Expiring** - Treatments expiring within X days (custom_today < expiration ≤ custom_today + X)
3. **Overdue** - Structures without active treatments (need treatment)

### Calculation Logic

#### Step 1: Get All Qualifying Structures with Treatments
Query structures that have treatments, using gis_sectcode for zone, foreman, and facility:
```sql
SELECT 
  trt.sitecode,
  trt.inspdate,
  gis.facility,
  gis.fosarea as foreman,
  COALESCE(mat.effect_days, 30) AS effect_days,
  loc.s_type,
  loc.priority,
  loc.status_udw as status,
  gis.zone
FROM public.dblarv_insptrt_current trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode  
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE trt.list_type = 'STR'
  AND (loc.enddate IS NULL OR loc.enddate > CURRENT_DATE)
  [... additional filters ...]
```

#### Step 2: Calculate Treatment Status in R
After loading data, determine if each treatment is active or expiring:
```r
current_data <- current_data %>%
  mutate(
    inspdate = as.Date(inspdate),
    enddate = inspdate + effect_days,  # Treatment expiration date
    days_since_treatment = as.numeric(custom_today - inspdate),
    is_active = days_since_treatment <= effect_days,
    is_expiring = days_since_treatment > (effect_days - expiring_days) & 
                   days_since_treatment <= effect_days
  )
```

**Status Logic**:
- **Active**: `days_since_treatment <= effect_days`
- **Expiring**: `days_since_treatment` between `(effect_days - expiring_days)` and `effect_days`
- **Overdue**: All structures without active treatments (calculated by set difference)

#### Step 3: Get Total Active Structures
Separate query to count all active structures (baseline for overdue calculation):
```sql
SELECT COUNT(DISTINCT sitecode)::bigint AS total_structures
FROM public.loc_cxstruct
WHERE (enddate IS NULL OR enddate > DATE '[custom_today]')
  AND zone IN ([zone_filter])
  [... additional filters ...]
```

#### Step 4: Aggregate by Group
Combine structures and treatments, group by facility/FOS/MMCD:
- **total_structures**: Count all active structures
- **active_structures**: Count DISTINCT sitecodes with `is_active = TRUE`
- **expiring_structures**: Count DISTINCT sitecodes with `is_expiring = TRUE`
- **overdue**: `total_structures - (active + expiring)`


## Tab 2: Historical Trends

### Purpose
Compare treatment activity across multiple years to identify trends and patterns.

### Key Features
- **Start Year / End Year selectors** - Define date range for comparison
- **Year-over-year line chart** - Track treatment patterns by year
- **Same filters as Current Progress** - Facility, structure type, zone, etc.

### Data Query Pattern
Query both archive and current tables, ensuring structures were active when treated:
```sql
-- Archive data
SELECT DISTINCT
  trt.sitecode,
  trt.inspdate,
  COALESCE(mat.effect_days, 30) AS effect_days,
  loc.s_type,
  gis.facility,
  gis.fosarea as foreman,
  gis.zone,
  EXTRACT(YEAR FROM trt.inspdate) as treatment_year
FROM public.dblarv_insptrt_archive trt
LEFT JOIN public.mattype_list_targetdose mat 
  ON trt.matcode = mat.matcode
LEFT JOIN public.loc_cxstruct loc
  ON trt.sitecode = loc.sitecode
LEFT JOIN public.gis_sectcode gis
  ON loc.sectcode = gis.sectcode
WHERE trt.list_type = 'STR'
  AND trt.inspdate >= DATE '[start_year]-01-01'
  AND trt.inspdate < DATE '[end_year+1]-01-01'
  AND (loc.enddate IS NULL OR loc.enddate > trt.inspdate)
  [... additional filters ...]

-- Current data (same pattern)
-- Combined in R with bind_rows()
```

**Key Points**:
- Use `gis_sectcode` for facility, foreman (fosarea), and zone to ensure data consistency
- Filter: `enddate > inspdate` ensures structure existed when treated
- `DISTINCT` prevents duplicate records from gis_sectcode join
- Queries both archive and current tables, combines results
- Extract `treatment_year` for yearly aggregation


### Removed priority filtering
The app originally included a priority filter in the UI:
```r
# REMOVED CODE:
selectInput("priority_filter", "Priority:",
            choices = get_priority_choices(include_all = TRUE),
            selected = "all")
```

### Why It Was Removed
**Data Incomplete**: Not all facilities have priority values assigned to structures. Filtering by priority would exclude many structures with NULL priority, giving incomplete results.

### Current Implementation
- Priority filter **removed from UI**
- Priority parameter **hardcoded to "all"** in `refresh_inputs()`
- Data functions still accept priority parameter for future use
- All structures included regardless of priority value

### Code Comment
```r
# we removed priority filter for now because data is incomplete
# not all facilities have priorities for the structures
```

### If Re-enabling Priority Filter
1. Verify data completeness across all facilities
2. Add back UI control in sidebar
3. Change `priority_filter = "all"` to `priority_filter = isolate(input$priority_filter)` in `refresh_inputs()`
4. Test with facilities that have NULL priorities

### Enddate Filtering

**Added Enddate Filtering - Eliminates 211,232 ghost structures (87% of database)**

To ensure accurate data representation, enddate filtering has been implemented in all queries. This eliminates ghost structures—structures that no longer exist but were previously included in results.

#### Current Queries
```sql
(loc.enddate IS NULL OR loc.enddate > CURRENT_DATE)
```
- Filters out structures with an enddate in the past, ensuring only active structures are included.

#### Historical Queries
```sql
(loc.enddate IS NULL OR loc.enddate > trt.inspdate)
```
- Ensures structures were active at the time of treatment, preventing inclusion of structures that were closed before treatment occurred.

## File Structure

### app.R (Main Application)
- UI definition with sidebar filters and tabbed display
- Server function with refresh pattern implementation
- Input capture and data loading logic
- Output rendering for both charts

### data_functions.R (Database Queries)
**Key functions:**
- `get_current_structure_data()` - Active/expiring/overdue treatments
- `get_all_structures()` - Complete structure list with filters
- `get_historical_structure_data()` - Year-over-year treatment counts
- `aggregate_structure_data()` - Group and summarize for charts

**Helper functions for SQL conditions:**
- `get_facility_condition()` - Facility filter SQL (uses `trt.facility`)
- `get_structure_type_condition()` - Type filter with CV/PR handling (uses `loc.s_type`)
- `get_priority_condition()` - Priority filter SQL (uses `loc.priority`)
- `get_status_condition()` - Status filter SQL (uses `loc.status_udw`)
- `get_facility_condition_total()` - Combined filters for total counts (no table prefix)

### display_functions.R (Visualization)
- `create_current_progress_chart()` - Horizontal stacked bar chart
  - Blue/Orange/Red color scheme
  - Large readable fonts
  - Legend with status categories
- `create_historical_trends_chart()` - Line chart with trends
  - Year-over-year comparison
  - Smoothed trend line
  - Large axis labels and titles
