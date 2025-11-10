# Ground Prehatch Treatment Progress - Technical Notes

## Overview

This Shiny dashboard tracks ground prehatch treatment progress across two main views:

1. **Progress Overview** - Summary statistics and progress charts showing treatment status by facility, FOS, or section
2. **Detailed View** - Site-level details table with treatment history and status

The app focuses on prehatch sites (sites requiring pre-hatch mosquito control treatments) and tracks their treatment status over time.

## Data Sources

### Tables Used

#### Primary Tables

- **`loc_breeding_sites`** - Site information and lifecycle tracking
  - Contains site characteristics: `acres`, `prehatch`, `drone`, `air_gnd`, `priority`
  - Tracks site lifecycle: `enddate` (when site was closed/inactivated)
  - **CRITICAL**: Sites with `enddate` before current season are filtered out
  - Filter: `air_gnd = 'G'` (ground sites only)
  - Join pattern: `LEFT JOIN gis_sectcode ON left(sitecode,7) = sectcode`

- **`dblarv_insptrt_current`** - Current larval treatment records
  - Contains treatment records: `inspdate`, `matcode`, `insptime`
  - Used to calculate treatment age and status
  - Filter: `inspdate > CURRENT_YEAR-01-01 AND inspdate <= simulation_date`
  - Join pattern: Uses sitecode to match with loc_breeding_sites

- **`gis_sectcode`** - Section/zone information
  - Links site codes to geographic zones (P1 = zone '1', P2 = zone '2')
  - Provides FOS (Field Operations Supervisor) area assignments (`fosarea`)
  - Join pattern: `left(sitecode,7) = sectcode`

- **`mattype_list_targetdose`** - Material effectiveness information
  - Contains `effect_days` - how long a treatment remains effective
  - Contains `days_retrt_early` - early retreatment threshold for expiring status
  - Filter: `prehatch = TRUE` (only prehatch materials)
  - Used to calculate treatment status (treated, expiring, expired)

- **`employee_list`** - FOS information
  - Used to get active Field Operations Supervisors
  - Filter: `emp_type = 'FieldSuper'` AND `active = true`

### Treatment Status Logic

Treatment status is calculated based on the age of the most recent treatment:

- **Treated**: `age <= effect_days` - Treatment is still effective
- **Expiring**: `age > days_retrt_early` - Treatment approaching expiration (early retreatment threshold)
- **Expired**: `age > effect_days` - Treatment has expired, site needs retreatment
- **No Treatment**: Site has no treatment records or doesn't match criteria

Age calculation: `CURRENT_DATE (or simulation_date) - inspdate`

Default effectiveness: 30 days if `effect_days` is NULL

## Critical Data Integrity Issue: Site Enddate Filtering

### The Problem

The `enddate` column in `loc_breeding_sites` tracks when a site is closed/inactivated. Sites that have been closed can still have historical treatment records.

### The Solution

**ALWAYS filter by enddate in the SQL query:**

```sql
WHERE (b.enddate IS NULL OR b.enddate > 'CURRENT_YEAR-05-01')
```

This ensures only active sites are included in the analysis. Using May 1st as the cutoff allows for sites that closed during the season to still show historical data.

### Why This Matters

- Without this filter, closed sites appear in results
- This causes "ghost sites" to inflate counts
- Treatment statistics become inaccurate
- Both `get_ground_prehatch_data()` and `get_site_details_data()` apply this filter

## Tab 1: Progress Overview

### Purpose

Show aggregated treatment progress statistics with visual charts and summary metrics.

### Key Features

- **6 Value Boxes** showing key metrics:
  - Total Ground Sites
  - Prehatch Sites
  - Treated Sites
  - Needs Treatment
  - Treated % (percentage of prehatch sites with active treatments)
  - Expiring % (percentage of prehatch sites with expiring treatments)

- **Stacked Bar Chart** showing treatment status breakdown by selected grouping:
  - Treated (Green) - Sites with active treatments
  - Expiring (Orange) - Sites with treatments about to expire
  - Expired (Gray) - Sites with expired treatments
  - Needs Treatment (Red) - Sites requiring treatment

- **Grouping Options**:
  - All MMCD - District-wide view
  - Facility - Group by facility
  - FOS - Group by Field Operations Supervisor
  - Section - Group by section code

- **Zone Options**:
  - P1 Only - Show only Priority 1 zone
  - P2 Only - Show only Priority 2 zone
  - P1 and P2 Separate - Show both zones with separate bars
  - Combined P1+P2 - Show both zones combined into single bars

### Data Flow

1. **Data Loading**: `get_ground_prehatch_data()` fetches aggregated section-level data
2. **Filtering**: `filter_ground_data()` applies facility, FOS, and zone filters
3. **Aggregation**: `aggregate_data_by_group()` rolls up data by selected grouping level
4. **Visualization**: `create_progress_chart()` generates stacked bar chart
5. **Value Boxes**: `create_value_boxes()` calculates summary statistics

### Visualization Details

The chart uses color-coded stacking:
- **Active** (Green): Sites with current effective treatments
- **Planned/Expiring** (Orange): Sites with treatments expiring soon
- **Unknown/Expired** (Gray): Sites with expired treatments
- **Needs Treatment** (Red): Sites requiring immediate attention

Chart is interactive (Plotly) with hover tooltips showing exact counts.

## Tab 2: Detailed View

### Purpose

Provide site-level detail table with treatment history and downloadable CSV export.

### Key Features

- **Detailed Table** showing individual site information:
  - Facility, Priority Zone, Section, Sitecode
  - FOS assignment
  - Acres, Priority level
  - Treatment Type (PREHATCH, BRIQUET)
  - Status (treated, expiring, expired, or blank)
  - Last Treatment date
  - Days Since Last Treatment
  - Material used
  - Effect Days (treatment effectiveness duration)

- **CSV Download** button for exporting filtered data

- **Pagination & Search** via DataTables interface

### Data Flow

1. **Data Loading**: `get_site_details_data()` fetches site-level treatment records
2. **Filtering**: `filter_ground_data()` applies facility, FOS, and zone filters
3. **Display**: `create_details_table()` formats data for DataTables
4. **Download**: `prepare_download_data()` prepares CSV export

## Code Organization

### File Structure

```
ground_prehatch_progress/
├── app.R                           # Main app - UI orchestration and server logic
├── ui_helpers.R                    # UI components (filter panel, value boxes, etc.)
├── data_functions.R                # Database queries and data processing
├── display_functions.R             # Charts, tables, and visualization functions
├── NOTES.md                        # Documentation (this file)
└── NOTES.html                      # Documentation (HTML version)
```

### Design Principles

1. **Separation of Concerns**: UI, data, and display logic are separate
2. **Refresh Button Pattern**: No data loads until user clicks "Refresh Data"
3. **Input Isolation**: All inputs captured via `refresh_inputs()` when button clicked
4. **Reusable Functions**: UI components, data processing, and visualization are modular
5. **Consistent Patterns**: All outputs follow same filter/process/display flow

## Refresh Button Pattern - Critical for Performance

### Why It's Critical

Without this pattern, changing ANY filter would immediately trigger database queries, which:
- Slows down the app when adjusting multiple filters
- Creates unnecessary database load
- Provides poor user experience

### How It Works

1. **On app load**: Only UI options (facility names, FOS names) are loaded
2. **User adjusts filters**: No database queries run - just UI state changes
3. **User clicks "Refresh Data"**: 
   - `refresh_inputs()` captures ALL current input values using `isolate()`
   - `ground_data()` eventReactive executes, using captured inputs
   - `site_details()` eventReactive executes, using captured inputs
   - All data processing happens with frozen input values
4. **User changes filters again**: Charts stay the same (using frozen data)
5. **User clicks "Refresh Data" again**: New query with new input values

### Key Implementation

```r
# Capture inputs when refresh clicked
refresh_inputs <- eventReactive(input$refresh, {
  zone_value <- isolate(input$zone_filter)
  
  # Parse zone filter
  parsed_zones <- if (zone_value == "combined") {
    c("1", "2")  # Include both zones but will be combined
  } else if (zone_value == "1,2") {
    c("1", "2")  # Include both zones separately
  } else {
    zone_value  # Single zone
  }
  
  list(
    zone_filter_raw = zone_value,
    zone_filter = parsed_zones,
    combine_zones = (zone_value == "combined"),
    facility_filter = isolate(input$facility_filter),
    foreman_filter = isolate(input$foreman_filter),
    # ... all other inputs
  )
})

# Load data ONLY when refresh clicked
ground_data <- eventReactive(input$refresh, {
  inputs <- refresh_inputs()
  # Use inputs$zone_filter, NOT input$zone_filter
  get_ground_prehatch_data(inputs$zone_filter, simulation_date)
})
```

**CRITICAL**: Zone parsing happens inside `refresh_inputs()` to avoid circular dependencies. The parsed zone values and combine_zones flag are stored directly in the inputs list.

## Common Data Patterns


### Zone Mapping

- **P1** = Zone '1' (Primary zone)
- **P2** = Zone '2' (Secondary zone)

### FOS Assignment Pattern

FOS (Field Operations Supervisor) assignment comes from:
1. `gis_sectcode.fosarea` - Links section to FOS area
2. `employee_list` - Maps FOS area to actual employee
3. Display uses `shortname` from employee_list

### Sitecode Format

Standard format: First 7 characters = section code
- Example: `190208-003` → section `190208-`
- Used for joining with `gis_sectcode`

### Treatment Status Hierarchy

1. **Most Recent Treatment** is selected using `DISTINCT ON (sitecode)` with `ORDER BY inspdate DESC, insptime DESC`
2. **Age Calculation** uses date_part to get days since treatment
3. **Status Classification** uses CASE statement with effect_days and days_retrt_early thresholds

## Special Features

### "Pretend Today Is" Date Simulation

Allows users to simulate what the treatment status would look like on any given date:
- Changes the reference date for age calculations
- Useful for historical analysis or planning
- Default: Current system date

### "Days Until Expiring" Slider

Controls the threshold for "expiring" status (default: 14 days):
- Adjusts when treatments are flagged as approaching expiration
- Range: 1-60 days
- Useful for adjusting planning windows

### "Show Expiring Only" Filter

When checked:
- Filters to only show sites/sections with expiring treatments
- Zeros out all other treatment status counts
- Useful for focusing on sites requiring immediate attention

### Zone Display Options

Four different ways to view zone data:
1. **P1 Only**: Filter to Priority 1 zone only
2. **P2 Only**: Filter to Priority 2 zone only
3. **P1 and P2 Separate**: Show both zones with separate bars/rows
4. **Combined P1+P2**: Show both zones aggregated together

## Testing & Validation

### Test Scenarios

1. **Refresh Button Test**:
   - Change filters without clicking refresh → No data should load
   - Click refresh → Data loads with selected filters
   - Change filters again → Data stays the same until refresh

2. **Zone Filtering Test**:
   - P1 Only: Should show only zone '1' data
   - P2 Only: Should show only zone '2' data
   - Separate: Should show separate entries for P1 and P2
   - Combined: Should show single entries with both zones aggregated

3. **Grouping Test**:
   - All MMCD: Single bar/entry for entire district
   - Facility: One bar per facility
   - FOS: One bar per Field Operations Supervisor
   - Section: One bar per section code

4. **Treatment Status Test**:
   - Recent treatments should show as "Treated"
   - Old treatments should show as "Expired"
   - Treatments approaching expiration should show as "Expiring"
   - Sites with no treatments should show as "Needs Treatment"

5. **Date Simulation Test**:
   - Set "Pretend Today Is" to past date
   - Verify treatment statuses change accordingly
   - Sites treated after simulation date should not appear

6. **CSV Download Test**:
   - Apply filters and refresh
   - Click download button
   - Verify CSV contains correct filtered data


## Key Takeaways

1. **Refresh Button Pattern**: Essential for performance - NO data queries until refresh clicked
2. **Zone Parsing in refresh_inputs()**: Prevents circular dependencies by parsing zone values during input capture
3. **Integer64 Conversion**: Always convert COUNT() results to numeric to avoid overflow warnings
4. **Site Enddate Filtering**: Always filter `enddate IS NULL OR enddate > season_start` to exclude closed sites
5. **FOS = Jurisdictional Assignment**: Use fosarea from gis_sectcode, mapped through employee_list
6. **Treatment Status Hierarchy**: Most recent treatment wins, status based on age vs effect_days
7. **Date Simulation**: Allows historical analysis by changing reference date for age calculations
8. **Zone Display Options**: Four modes (P1, P2, Separate, Combined) controlled by combine_zones flag
9. **SQL Query Structure**: Two main queries - aggregated section data and detailed site data
10. **Value Box Calculations**: Aggregate at top level for district-wide statistics

## Last Updated

November 10, 2025

## App Version

Refactored with modular file structure and refresh button pattern
