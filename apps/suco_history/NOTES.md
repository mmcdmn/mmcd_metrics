# SUCO Analysis Dashboard - Technical Notes

## Overview
This Shiny app provides comprehensive analysis of SUCO inspections across multiple perspectives:
1. **Current Data** - Analysis of current year SUCO inspections
2. **All Data (Current + Archive)** - Historical analysis combining current and archived data

Each perspective includes:
- **Graph** - Time series visualization of SUCO counts
- **Map** - Spatial distribution of SUCO locations
- **Summary Table** - Aggregated statistics by facility/FOS
- **Detailed Samples** - Individual SUCO inspection records
- **Top Locations** - Most visited locations or locations with most species

## Data Sources

### Tables Used

#### Primary Inspection Tables
- **`dbadult_insp_current`** - Active adult surveillance inspection records
  - Contains ongoing inspections for current operations
  - Fields: `id`, `ainspecnum`, `facility`, `foreman`, `inspdate`, `sitecode`, `address1`, `park_name`, `survtype`, `fieldcount`, `comments`, `x`, `y`, `geometry`
  - **survtype = '7'** identifies SUCO inspections
  - **NOTE**: Does NOT have `enddate` column
  
- **`dbadult_insp_archive`** - Historical adult surveillance inspection records
  - Contains closed/archived inspections from previous years
  - Same schema as current table
  - **NOTE**: Does NOT have `enddate` column

#### Species Tables
- **`dbadult_species_current`** - Species counts for current inspections
  - Links to inspections via `ainspecnum`
  - Fields: `ainspecnum`, `spp` (species code), `cnt` (count)
  
- **`dbadult_species_archive`** - Species counts for archived inspections
  - Same schema as current species table

#### Supporting Tables
- **`loc_harborage`** - Harborage site lifecycle and assignment tracking
  - **AUTHORITATIVE SOURCE** for foreman and facility assignments
  - Fields: `sitecode`, `facility`, `foreman`, `startdate`, `enddate`
  - Used to determine which FOS/facility was responsible at time of inspection
  - **Join logic**: `s.inspdate >= h.startdate AND (h.enddate IS NULL OR s.inspdate <= h.enddate)`
  
- **`gis_sectcode`** - Section/zone information
  - Links site codes to geographic zones (P1 = zone '1', P2 = zone '2')
  - Fields: `sectcode`, `facility`, `fosarea`, `zone`
  - **Fallback** for facility if harborage doesn't have data
  - Join pattern:
    ```sql
    LEFT JOIN public.gis_sectcode g ON LEFT(s.sitecode, 6) || '-' = g.sectcode
      OR LEFT(s.sitecode, 6) || 'N' = g.sectcode
      OR LEFT(s.sitecode, 6) || 'S' = g.sectcode
      OR LEFT(s.sitecode, 6) || 'E' = g.sectcode
      OR LEFT(s.sitecode, 6) || 'W' = g.sectcode
    ```
  
- **`lookup_specieslist`** - Species name lookup
  - Maps species codes to genus and species names
  - Fields: `sppcode`, `genus`, `species`

### Survtype Codes
- **survtype '7'** - SUCO inspections (this is the focus of the app)
- Other survtypes are filtered out

### Data Hierarchy and Fallback Logic

The app uses a cascading fallback approach for facility and foreman assignment:

```sql
-- Facility assignment (in order of preference):
COALESCE(h.facility, g.facility, s.facility) as facility

-- Foreman assignment (in order of preference):
COALESCE(h.foreman, s.foreman) as foreman
```

**Why this hierarchy?**
1. **`loc_harborage`** is the most accurate source for practical assignments
2. **`gis_sectcode`** provides geographic-based facility assignment
3. **Original inspection data** (`s.facility`, `s.foreman`) is the final fallback

## Query Patterns

### Current Data Query Pattern
```sql
SELECT
  s.id, s.ainspecnum, 
  COALESCE(h.facility, g.facility, s.facility) as facility,
  COALESCE(h.foreman, s.foreman) as foreman,
  s.inspdate, s.sitecode,
  s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
  s.x, s.y,
  g.zone
FROM public.dbadult_insp_current s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND s.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR s.inspdate <= h.enddate)
LEFT JOIN public.gis_sectcode g ON LEFT(s.sitecode, 6) || '-' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'N' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'S' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'E' = g.sectcode
  OR LEFT(s.sitecode, 6) || 'W' = g.sectcode
WHERE s.survtype = '7'
  AND s.inspdate BETWEEN [start_date] AND [end_date]
```

### All Data Query Pattern
Same as current data query, but queries both `dbadult_insp_current` and `dbadult_insp_archive`, then combines with `bind_rows()`.

### Species Data Integration
After retrieving inspection data:
1. Query species tables for all `ainspecnum` values
2. Join with `lookup_specieslist` for species names
3. Use enhanced species mapping from `db_helpers.R`
4. Create species summary for each SUCO inspection

## Key Features

### Date Controls

#### Date Shortcut Buttons
- **This Year** - Sets date range to Jan 1 of current year through today
- **This Month** - Sets date range to first day of current month through today
- **This Week** - Sets date range to Monday of current week through today

#### Year Range Slider (All Data Tab Only)
- Allows multi-year selection for historical analysis
- Min: 2015, Max: Current year
- Automatically syncs with custom date range

#### Custom Date Range
- Allows precise date selection
- Behavior depends on active tab:
  - **Current Data Tab**: Restricted to current year only
  - **All Data Tab**: Full date range available

### Grouping Options

#### MMCD (All)
- Aggregates all SUCOs across entire organization
- No facility or FOS breakdown

#### Facility
- Groups by facility assignment
- Uses facility colors from `db_helpers.R`
- Shows facility full names in displays

#### FOS (Field Operations Supervisor)
- Groups by foreman assignment
- Uses foreman colors from `db_helpers.R` (facility-based)
- Shows foreman short names in displays

### Filter Options

#### Zone Filter
- **P1 + P2** - Both zones combined
- **P1** - Primary zone only (zone = '1')
- **P2** - Secondary zone only (zone = '2')

#### Facility Filter
- Multi-select dropdown
- Populated from `get_facility_lookup()`
- "All" option includes all facilities

#### FOS Filter
- Multi-select dropdown
- Dynamically populated based on selected facility
- Shows foreman short names
- "All" option includes all foremen

#### Species Filter
- Single-select dropdown
- Dynamically populated from available species in date range
- Uses enhanced species mapping for display names
- Filters SUCOs to only those containing the selected species

### Graph Types

The app supports multiple visualization types:
- **Bar** - Side-by-side bars
- **Stacked Bar** - Stacked bars (default)
- **Line** - Line chart
- **Point** - Scatter plot
- **Area** - Filled area chart

### Top Locations Display Modes

#### Most Visited
- Shows locations with most SUCO samples
- Each segment represents one SUCO sample
- Colors indicate sample date (gradient from old to new)

#### Most Species
- Shows locations with highest species counts
- Each segment represents one SUCO sample
- Colors indicate sample date (gradient from old to new)
- Can be filtered to specific species

## Tab 1: Graph

### Purpose
Visualize SUCO inspection trends over time, grouped by facility, FOS, or all MMCD.

### Key Features
- **Time Interval**: Weekly (Monday start date)
- **Average Line** (All Data tab only): Shows average SUCOs per week across entire date range
- **Year Boundaries**: Vertical lines separate different years
- **Epi Week Labels**: X-axis uses epidemiological week numbers

### Calculation Logic

#### Step 1: Aggregate by Time Group
```r
data %>%
  mutate(time_group = week_start) %>%  # Monday of each week
  group_by(time_group, [group_by_column]) %>%
  summarize(
    count = n(),
    total_fieldcount = sum(fieldcount, na.rm = TRUE),
    epi_week_label = first(epi_week_label),
    .groups = "drop"
  )
```

#### Step 2: Create Plot with Color Mapping
- **Facility grouping**: Uses `get_facility_base_colors()`
- **FOS grouping**: Maps foreman numbers to facility-based colors via `get_foreman_colors()`
- **MMCD grouping**: Single blue color

#### Step 3: Add Average Line (All Data Only)
```r
# Calculate total SUCOs per week (across all groups)
weekly_totals <- data %>%
  group_by(time_group) %>%
  summarize(total_count = sum(count, na.rm = TRUE))

avg_sucos_per_week <- mean(weekly_totals$total_count, na.rm = TRUE)

# Add horizontal line at average
geom_hline(yintercept = avg_sucos_per_week, color = "red", linetype = "dashed")
```

## Tab 2: Map

### Purpose
Display spatial distribution of SUCO inspections with interactive markers.

### Marker Sizing Logic
Markers are sized based on species count using a staged approach:

```r
marker_size = case_when(
  display_species_count == 0 ~ 4,      # No species
  display_species_count == 1 ~ 6,      # 1 species
  display_species_count <= 5 ~ 8,      # 2-5 species
  display_species_count <= 10 ~ 10,    # 6-10 species
  display_species_count <= 20 ~ 12,    # 11-20 species
  display_species_count <= 30 ~ 14,    # 21-30 species
  display_species_count <= 50 ~ 16,    # 31-50 species
  display_species_count <= 75 ~ 18,    # 51-75 species
  display_species_count <= 100 ~ 20,   # 76-100 species
  TRUE ~ 22                             # 100+ species
)
```

### Overlapping Point Handling
When multiple SUCOs occur at the same location:
```r
# Add small random offset (max 0.0001 degrees ≈ 11 meters)
longitude_adj = longitude + runif(n(), -0.0001, 0.0001)
latitude_adj = latitude + runif(n(), -0.0001, 0.0001)
```

### Color Schemes

#### Facility Map
- Uses `get_facility_base_colors()` from `db_helpers.R`
- Each facility has a distinct color
- Legend shows facility full names

#### FOS Map
- Uses `get_foreman_colors()` from `db_helpers.R`
- Colors are facility-based (same facility = similar color shades)
- Legend ordered by facility, then foreman
- Shows foreman short names

#### MMCD Map
- Single blue color (`#1f77b4`)
- No facility/FOS differentiation

### Popup Content
```
Date: [inspdate]
Facility: [facility full name]
FOS: [foreman short name]
Location: [location]
Species Count: [display_species_count]
Species Found:
  [species_name]: [count]
  [species_name]: [count]
  ...
```

### Basemap Options
- **OpenStreetMap** - Standard OSM tiles
- **Carto Light** - Clean, minimal basemap (default)
- **Terrain** - Topographic map
- **Esri Satellite** - Satellite imagery

## Tab 3: Summary Table

### Purpose
Provide aggregated statistics grouped by facility or FOS.

### Columns

#### MMCD (All) Grouping
| Column | Description |
|--------|-------------|
| Total_SUCOs | Total number of SUCO inspections |
| Total_Locations | Count of unique sitecodes |
| Total_Species_Count | Sum of all species counts |
| First_SUCO | Earliest inspection date |
| Last_SUCO | Most recent inspection date |

#### Facility Grouping
| Column | Description |
|--------|-------------|
| Facility | Facility short code |
| Facility_Name | Facility full name |
| Total_SUCOs | Total SUCO inspections |
| Total_Locations | Unique locations |
| Total_Species_Count | Sum of species counts |
| First_SUCO | Earliest inspection |
| Last_SUCO | Most recent inspection |

#### FOS Grouping
| Column | Description |
|--------|-------------|
| FOS | Foreman employee number |
| Name | Foreman short name |
| Facility | Assigned facility |
| Total_SUCOs | Total SUCO inspections |
| Total_Locations | Unique locations |
| Total_Species_Count | Sum of species counts |
| First_SUCO | Earliest inspection |
| Last_SUCO | Most recent inspection |

### Key Features
- **Includes zeros**: Shows all facilities/foremen even with 0 SUCOs in selected date range
- **Sorted by activity**: Ordered by Total_SUCOs descending
- **Searchable**: Built-in search functionality
- **Paginated**: 15 rows per page

## Tab 4: Detailed Samples

### Purpose
Show individual SUCO inspection records with full details.

### Columns
- **Date** - Inspection date
- **Facility** - Facility full name
- **FOS** - Foreman short name
- **Zone** - P1 or P2
- **Sitecode** - Site identifier
- **Location** - Park name or address
- **Species_Count** - Total count (or filtered species count)
- **Species_Found** - Semicolon-separated species list with counts

### Filtering Behavior
When a species filter is applied:
- Only shows SUCOs containing that species
- Species_Count shows count of filtered species only
- Sorted by Species_Count descending

### Display Features
- **Scrollable**: Horizontal scroll for wide content
- **Searchable**: Can search across all columns
- **Paginated**: 25 rows per page
- **Truncated species list**: Long species lists are abbreviated with "..."

## Tab 5: Top Locations

### Purpose
Identify and visualize locations with most SUCO activity or highest species diversity.

### Display Modes

#### Most Visited
- Shows top 25 locations by number of SUCO samples
- Each horizontal segment = one SUCO sample
- Color gradient = sample date (older → darker, newer → lighter)
- Y-axis label: "Individual Samples"

#### Most Species
- Shows top 25 locations by total species count
- Each horizontal segment = one SUCO sample with species count
- Color gradient = sample date
- Y-axis label: "Species Count per Sample"
- When species filter applied: shows count of that specific species

### Interactive Features
- **Click to map**: Clicking a bar switches to Map tab and zooms to that location
- **Hover details**: Shows date, location, and species found
- **Stacked bars**: Multiple samples at same location stack vertically

### Sample Coloring
Uses viridis color scale for date gradient:
```r
fill = date_numeric  # Continuous numeric date value
scale_fill_viridis_c(option = "viridis", name = "Date")
```

## Code Organization

### File Structure
```
suco_history/
├── app.R                    # Main UI and server logic
├── data_functions.R         # Database queries and data processing
├── display_functions.R      # Visualization functions
└── ui_helpers.R             # UI interaction handlers
```

### Design Principles
1. **Separation of Concerns**: UI/server in `app.R`, all SQL in `data_functions.R`
2. **No SQL in app.R**: Every query wrapped in a function
3. **Reusable Components**: Same map/plot/table functions for both current and all data
4. **Consistent Patterns**: Parallel structure for current and all data tabs

### Function Files

#### data_functions.R
Core data retrieval and processing:
- `get_available_species(date_range)` - Populate species filter dropdown
- `get_suco_data(data_source, date_range)` - Main data query function
- `filter_suco_data(data, ...)` - Apply user filters
- `create_spatial_data(data, species_filter)` - Prepare data for mapping
- `aggregate_suco_data(data, group_by, zone_filter)` - Time series aggregation
- `create_summary_stats(data, group_by, data_source)` - Summary table data
- `get_top_locations(data, mode, species_filter)` - Top locations data
- `create_detailed_samples_table(data, species_filter)` - Detailed table data

#### display_functions.R
Visualization rendering:
- `create_suco_map(data, input, data_source)` - Leaflet map creation
- `create_location_plotly(data, data_source, mode)` - Top locations chart
- `create_trend_plot(aggregated_data, ...)` - Time series plot
- `convert_zone_selection(zone_input)` - Helper for zone filtering

#### ui_helpers.R
UI interaction handlers:
- `handle_date_shortcut(shortcut_type, ...)` - Date button handlers
- `handle_year_range_change(...)` - Year slider updates
- `handle_date_range_change(...)` - Date picker updates
- `set_updating_flags(...)` - Prevent infinite update loops

### Shared Utilities (shared/db_helpers.R)
- `get_db_connection()` - Database connection helper
- `get_facility_lookup()` - Facility codes and full names
- `get_foremen_lookup()` - Foreman numbers, names, and facilities
- `get_facility_base_colors()` - Color mapping for facilities
- `get_foreman_colors()` - Color mapping for foremen (facility-based)
- `get_species_lookup()` - Species code to name mapping
- `get_enhanced_species_mapping()` - Enhanced species display names

## Common Data Patterns

### Species Summary Creation
For each SUCO inspection (`ainspecnum`):
1. Join inspection with species tables
2. Join species codes with lookup table
3. Apply enhanced species mapping
4. Aggregate species by name and count
5. Format as HTML for popups:
   ```
   Species Name 1: 50
   Species Name 2: 32
   Species Name 3: 15
   Others: 25
   ```
6. Limit to top 5 species to keep popups manageable

---

*Last updated: November 2025*
