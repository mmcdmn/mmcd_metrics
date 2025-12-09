# Control Efficacy - Air Treatment Checkbacks - Technical Notes

## Overview
This Shiny app tracks control efficacy of air treatments by analyzing checkback inspection data across three perspectives:
1. **Checkback Progress** - Tracks treatment rounds (broods) and checkback completion status by facility
2. **Status Tables** - Shows brood status and detailed checkback results with species composition
3. **Control Efficacy** - Visualizes pre/post treatment dip counts to assess treatment effectiveness

## Data Sources

### Core Database Tables and Columns

#### Primary Treatment and Checkback Tables
- **`public.dblarv_insptrt_current`** - Active larval inspection/treatment records
  - **Key Columns**: `sitecode`, `facility`, `inspdate`, `action`, `numdip`, `diphabitat`, `acres`, `matcode`, `sampnum_yr`, `posttrt_p`
  - **Action Codes**: 
    - `'A'` = Air treatment (primary treatments tracked in this app)
    - `'4'` = Checkback inspection (post-treatment follow-up)
  - **Checkback Link**: `posttrt_p` field indicates if inspection is post-treatment
  - **Sample Link**: `sampnum_yr` connects to species and sample metadata tables
  - **Missing Columns**: Does NOT contain `zone`, `mattype`, `effect_days` - must join for these
  - **Data Quality**: Contains ongoing treatments and checkbacks
  
- **`public.dblarv_insptrt_archive`** - Historical larval inspection/treatment records  
  - **Key Columns**: `sitecode`, `facility`, `inspdate`, `action`, `numdip`, `diphabitat`, `matcode`, `sampnum_yr`, `posttrt_p`
  - **Action Codes**: Same as current table ('A' for treatment, '4' for checkback)
  - **Checkback Link**: `posttrt_p` field indicates post-treatment status
  - **Sample Link**: `sampnum_yr` connects to species and sample metadata
  - **Missing Columns**: Does NOT contain `zone`, `mattype`, `effect_days`, `acres` - must join for these
  - **Data Quality**: Historical closed records, generally reliable

#### Species and Sample Data Tables
- **`public.dblarv_species_current`** - Active species composition records
  - **Key Columns**:
    - `sampnum_yr` (joins to dblarv_insptrt via sampnum_yr)
    - `spp` (species code as VARCHAR, joins to lookup_specieslist.sppcode)
    - `per` (percentage of sample, INTEGER 0-100)
  - **Join Pattern**: `ON spp.spp = CAST(spec.sppcode AS VARCHAR)` - type conversion required!
  - **Aggregation**: Multiple rows per sampnum_yr (one per species found)
  - **Data Quality**: Contains species composition for each sample taken

- **`public.dblarv_species_archive`** - Historical species composition records
  - **Key Columns**: Same as current (`sampnum_yr`, `spp`, `per`)
  - **Join Pattern**: Same type conversion required for species lookup
  - **Aggregation**: Multiple rows per sampnum_yr
  - **Data Quality**: Historical species data for archived samples

- **`public.dblarv_sample_current`** - Active sample metadata
  - **Key Columns**:
    - `sampnum_yr` (joins to species and inspection tables)
    - `redblue` (sample designation: 'R' = Red, 'B' = Blue)
    - `missing` (BOOLEAN: TRUE = sample was missing/not collected)
  - **Usage**: Provides sample quality flags and classifications
  - **Data Quality**: Essential for filtering out missing or problematic samples

- **`public.dblarv_sample_archive`** - Historical sample metadata
  - **Key Columns**: Same as current (`sampnum_yr`, `redblue`, `missing`)
  - **Usage**: Same metadata flags for archived samples
  - **Data Quality**: Historical sample quality information

#### Essential Supporting Tables
- **`public.gis_sectcode`** - Geographic section mapping and zone assignments
  - **Key Columns**:
    - `sectcode` (7-character section identifier, e.g. '191031N')
    - `zone` (values: '1' = P1, '2' = P2)
    - `facility` (facility designation)
  - **Join Pattern**: `LEFT JOIN public.gis_sectcode sc ON left(sitecode,7) = sc.sectcode`
  - **Sitecode Formats Handled**:
    - Standard: `190208-003` → sectcode `1902080`
    - Directional: `191102W010` → sectcode `1911020` 
  - **Data Quality**: Authoritative source for zone and facility assignments
  
- **`public.mattype_list_targetdose`** - Material effectiveness and dosage data
  - **Key Columns**:
    - `matcode` (material identifier, joins to treatment tables)
    - `mattype` (material type description, e.g., 'Bs_Vlexgran')
    - `effect_days` (treatment effectiveness duration in days, e.g., 30)
  - **Usage**: Provides material names and effectiveness windows
  - **Join**: `LEFT JOIN mattype_list_targetdose mat ON insp.matcode = mat.matcode`

- **`public.lookup_specieslist`** - Mosquito species reference data
  - **Key Columns**:
    - `sppcode` (INTEGER species code)
    - `genus` (genus name, e.g., 'Aedes', 'Culex')
    - `species` (species name, e.g., 'vexans', 'pipiens')
  - **Critical Join**: `ON spp.spp = CAST(spec.sppcode AS VARCHAR)` - CAST required due to type mismatch
  - **Usage**: Translates numeric species codes to readable names
  - **Output Format**: "Ae. vexans", "Cu. pipiens" (abbreviated genus + species)

### Data Collection Strategy

#### Key Join Patterns Used Throughout Application

**Standard Treatment Query with Material Data:**
```sql
-- Get air treatments with material type information
SELECT 
  insp.inspdate,
  gis.facility,
  gis.zone,
  insp.sitecode,
  insp.action,
  insp.numdip,
  insp.diphabitat,
  insp.acres,
  insp.matcode,
  mat.mattype,
  mat.effect_days,
  insp.pkey_pg,
  insp.insptime
FROM public.dblarv_insptrt_current insp
LEFT JOIN public.gis_sectcode gis ON left(insp.sitecode,7) = gis.sectcode
LEFT JOIN public.mattype_list_targetdose mat ON insp.matcode = mat.matcode
WHERE insp.action = 'A'  -- Air treatments only
  AND insp.inspdate >= ?
  AND insp.inspdate <= ?
ORDER BY insp.inspdate
```

**Checkback Query with Sample Numbers:**
```sql
-- Get checkback inspections with sample data linkage
SELECT 
  insp.inspdate,
  gis.facility,
  gis.zone,
  insp.sitecode,
  insp.action,
  insp.numdip,
  insp.diphabitat,
  insp.posttrt_p,
  insp.sampnum_yr,  -- Critical for species join
  insp.pkey_pg
FROM public.dblarv_insptrt_current insp
LEFT JOIN public.gis_sectcode gis ON left(insp.sitecode,7) = gis.sectcode
WHERE insp.sitecode IN (treated_sites)
  AND insp.action = '4'  -- Checkback inspections only
  AND insp.posttrt_p IS NOT NULL
  AND insp.inspdate >= ?
  AND insp.inspdate <= ?
```

**Species Composition Query with Type Conversion:**
```sql
-- Get species composition with readable names
-- CRITICAL: Type conversion required (spp is VARCHAR, sppcode is INTEGER)
SELECT 
  spp.sampnum_yr,
  spp.spp,
  spp.per,
  spec.genus,
  spec.species,
  samp.redblue,
  samp.missing
FROM public.dblarv_species_current spp
LEFT JOIN public.lookup_specieslist spec 
  ON spp.spp = CAST(spec.sppcode AS VARCHAR)  -- Type conversion essential!
LEFT JOIN public.dblarv_sample_current samp 
  ON spp.sampnum_yr = samp.sampnum_yr
WHERE spp.sampnum_yr IN (checkback_samples)
ORDER BY spp.sampnum_yr, spp.per DESC
```

#### Critical Data Integration Notes

**Sitecode Format Handling:**
- **Standard Format**: `190208-003` (7-digit section + dash + site num)
- **Directional Format**: `191102W010` (6-digit + direction + site num)
- **Join Logic**: `left(sitecode,7)` extracts section code for both formats
- **Example**: Both `190208-003` and `191102W010` → sectcodes `1902080` and `1911020`

**Treatment Rounds (Broods):**
- **Definition**: Groups of treatments on consecutive days at same facility
- **Gap Tolerance**: Maximum 1 day between treatments to remain in same round
- **Round ID**: Facility code + start date (e.g., "BBF-06/14")
- **Purpose**: Track treatment campaigns as operational units

**Checkback Timing:**
- **Post-Treatment**: Checkbacks occur after air treatments
- **Timing Window**: Tracked via days_to_checkback (checkback_date - treatment_date)
- **Material Effect Window**: Compare checkback timing to material effect_days
- **Optimal Range**: Typically checkbacks within effect window show best efficacy

**Species Composition Formatting:**
- **Format**: "Ae. vexans (90%), Cu. pipiens (10%)"
- **Abbreviation**: Genus first letter + period + species name
- **Percentage**: From dblarv_species.per field
- **Special Cases**:
  - `"[Sample Missing]"` when dblarv_sample.missing = TRUE
  - `"[No Species Data]"` when no species records found for sample
- **Aggregation**: Concatenate all species per sampnum_yr, ordered by percentage DESC

**Dip Count Changes:**
- **Pre-Treatment**: `numdip` from treatment inspection (action = 'A')
- **Post-Treatment**: `numdip` from checkback inspection (action = '4')
- **Effectiveness Metric**: Compare pre vs post dip counts
- **Expected Pattern**: Lower post-treatment dips indicate effective control
- **Habitat Context**: `diphabitat` indicates dips per specific habitat count

## Tab 1: Checkback Progress

### Purpose
Track progress of checkback inspections for air treatment rounds (broods) by facility.

### Key Features
- **Value boxes** showing summary statistics:
  - Total Sites Treated (across all rounds)
  - Total Checkbacks Completed
  - Average Days to First Checkback
  - Checkback Completion Rate (percentage)
- **Interactive bar chart** showing checkback completion by facility
- **Treatment round grouping** with configurable gap tolerance
- **Checkback target selector**:
  - **Percent** - Target percentage of sites needing checkbacks
  - **Number** - Fixed number of checkback sites per round
- **Date range selector** - Filter treatments by inspection date
- **Facility filter** - View specific facilities or all combined

### Data Flow
1. Load treatments via `load_treatment_data()`:
   - Query air treatments (action = 'A') in date range
   - Include material type and effect days
   - Join with gis_sectcode for facility/zone

2. Calculate treatment rounds via `calculate_treatment_rounds()`:
   - Group consecutive treatment days by facility
   - Use 1-day gap tolerance (configurable)
   - Assign round IDs (facility-date format)
   - Track sites treated per round

3. Load checkbacks via `load_checkback_data()`:
   - Query checkback inspections (action = '4') for treated sites
   - Include sampnum_yr for species linkage
   - Filter for post-treatment checkbacks only

4. Calculate checkback status via `calculate_checkback_status()`:
   - Match checkbacks to treatment rounds
   - Count sites checked vs sites treated per round
   - Calculate completion percentages
   - Track timing (days to first checkback)

### Visualization
- **Stacked bar chart** per facility showing:
  - Total sites treated (background bar)
  - Sites with checkbacks completed (colored bar)
  - Completion percentage labels
- **Color scheme**: Facility-specific colors from get_facility_colors()
- **Tooltip**: Detailed breakdown on hover

## Tab 2: Status Tables

### Purpose
Provide detailed tabular views of treatment rounds (brood status) and individual checkback results.

### Key Features
Two distinct tables in single tab:

**Brood Status Table:**
- Shows treatment round summaries
- Columns: Facility, Round ID, Start/End Date, Days Duration, Sites Treated, Checkbacks Count, Completion %
- Sortable and filterable

**Site Details Table:**
- Shows individual checkback records (one row per checkback)
- Columns: 
  - Site Info: Sitecode, Facility, Inspection Date
  - Treatment Info: Treatment Date, Material Code, Material Type, Effect Days, Acres, Pre-Treatment Dips
  - Checkback Info: Checkback Date, Days to Checkback, Post-Treatment Dips
  - Species Info: Species Composition, Red/Blue Designation, Missing Flag
- **Per-Checkback Model**: Same site can appear multiple times if multiple checkbacks conducted
- Filterable by date range, facility, material, species

### Data Flow
1. Load all data sources:
   - Treatments (with material metadata)
   - Checkbacks (with sample numbers)
   - Species data (aggregated compositions)
   - Sample metadata (redblue, missing flags)

2. Create site details via `create_site_details()`:
   - **Critical**: Iterate through each checkback (not sites)
   - For each checkback, find last treatment BEFORE checkback date
   - Calculate days_to_checkback
   - Join species composition from species_data
   - Join redblue and missing flags
   - Return one row per checkback with full context

3. Apply filters:
   - Date range on treatment dates
   - Facility filter
   - Material code filter
   - **Species filter** - Filter by species presence in composition string

### Species Filter Implementation
- **Dynamic population**: Extract unique species from all species_composition strings
- **Options include**:
  - "All Species" (default - no filter)
  - Individual species (e.g., "Ae. vexans", "Cu. pipiens")
  - "[Sample Missing]" - Show only checkbacks with missing samples
  - "[No Species Data]" - Show checkbacks without species data
- **Filter logic**: `grepl(selected_species, species_composition, fixed=TRUE)`

### Data Model - Per-Checkback vs Per-Site
**IMPORTANT**: Data model changed from per-site aggregation to per-checkback records.

**Old Model (Incorrect)**:
- One row per site
- Aggregated checkback data (first checkback, last checkback, etc.)
- Problem: Lost detail when multiple checkbacks per site

**New Model (Current)**:
- One row per checkback
- Same site can appear multiple times
- Each row shows: inspection → treatment → checkback chain
- Benefits: Full detail, proper species tracking, accurate timing

**Column Name Changes**:
```
OLD NAME                  → NEW NAME
first_treatment          → treatment_date
first_checkback          → checkback_date
days_to_first_checkback  → days_to_checkback
last_dip_count           → post_treatment_dips
total_acres              → acres
has_checkback            → (removed - all rows have checkbacks)
```

## Tab 3: Control Efficacy

### Purpose
Visualize treatment effectiveness by comparing pre-treatment vs post-treatment dip counts.

### Key Features
- **Dumbbell chart** showing dip count changes:
  - Each line connects pre-treatment (left) to post-treatment (right) dip count
  - Color gradient by days to checkback (red = soon, purple = late)
  - Interactive tooltips with site details
- **"Show Most Recent Brood Only"** button - Filter to most recent treatment date
- **"Show All Broods"** button - Display all checkbacks in date range
- **All checkbacks table** - Detailed table of all sites with checkbacks
- **Shared filters** - Date range, facility, material, species

### Data Flow
1. Use same data sources as Status Tables tab
2. Filter via `filtered_site_details_for_dip()`:
   - Apply all standard filters
   - Additional mode: "recent" vs "all"
   - Recent mode: Filter to max(treatment_date) only

3. Create visualization via `create_dip_changes_chart()`:
   - Reshape data: pre_treatment_dips and post_treatment_dips as separate rows
   - Create dumbbell plot with geom_line and geom_point
   - Color by days_to_checkback gradient
   - Add boxplots for distribution context

### Dumbbell Chart Details
- **X-axis**: Treatment stage ("Pre-Treatment" vs "Post-Treatment")
- **Y-axis**: Dip count (log scale for wide range)
- **Lines**: Connect same site's pre/post values
- **Color gradient**: 
  - Red: Early checkbacks (0 days)
  - Orange → Yellow → Green: Mid-range
  - Blue → Purple: Late checkbacks (30+ days)
- **Boxplots**: Show distribution at each stage (faded, behind data)
- **Tooltips**: Sitecode, facility, acres, material, days to checkback

### Recent Brood Filter Logic
**Fixed Implementation** (no longer depends on treatment_rounds):
```r
if (dip_filter_mode() == "recent") {
  most_recent_date <- max(details$treatment_date, na.rm = TRUE)
  details <- details %>% filter(treatment_date >= most_recent_date)
}
```
- Always shows checkbacks from the most recent treatment date in filtered data
- No dependency on Progress tab's treatment_rounds calculation
- Works immediately without needing to click refresh

## Code Organization

### File Structure
```
control_efficacy/
├── app.R                      # Main app - UI and server logic
├── data_functions.R           # Database queries (treatments, checkbacks, species)
├── checkback_functions.R      # Treatment round and checkback processing
├── display_functions.R        # Chart and table visualization functions
├── test_all_functions.R       # Comprehensive function testing script
└── NOTES.md                   # This file
```

### Key Functions by File

#### app.R
- `treatment_data_progress()` - Loads treatments when refresh clicked
- `checkback_data_progress()` - Loads checkbacks when refresh clicked
- `species_data_for_checkbacks()` - Loads species composition data
- `treatment_rounds_progress()` - Calculates treatment rounds for Progress tab
- `checkback_status_progress()` - Calculates checkback completion status
- `site_details()` - Creates detailed per-checkback records with species
- `filtered_site_details()` - Applies all filters to site details
- `filtered_site_details_for_dip()` - Additional filter for dip chart (recent mode)

#### data_functions.R
- `load_treatment_data(start_date, end_date)` - Queries air treatments with material info
  - Combines current and archive tables
  - Joins with gis_sectcode for facility/zone
  - Joins with mattype_list_targetdose for material metadata
  - Returns: inspdate, facility, zone, sitecode, action, numdip, diphabitat, acres, matcode, mattype, effect_days

- `load_checkback_data(treated_sites, start_date, end_date)` - Queries checkback inspections
  - Filters to action = '4' (checkbacks only)
  - Requires posttrt_p IS NOT NULL
  - Includes sampnum_yr for species linkage
  - Returns: inspdate, facility, zone, sitecode, action, numdip, diphabitat, posttrt_p, sampnum_yr

- `load_species_data_for_checkbacks(checkbacks, start_date, end_date)` - Queries species composition
  - Joins dblarv_species with lookup_specieslist (with type conversion)
  - Joins dblarv_sample for redblue and missing flags
  - Aggregates species per sampnum_yr into formatted strings
  - Returns: sampnum_yr, species_composition, redblue, missing

#### checkback_functions.R
- `calculate_treatment_rounds(treatments, checkback_type, checkback_percent, checkback_number)` - Groups consecutive treatments into rounds
  - Groups by facility
  - Identifies gaps > 1 day to split rounds
  - Assigns round IDs
  - Calculates round statistics
  - Returns: facility, round_id, start_date, end_date, days_duration, sites_treated, total_sites, total_acres

- `calculate_checkback_status(rounds, checkbacks, treatments)` - Matches checkbacks to rounds
  - Links checkbacks to treatment rounds by facility and date
  - Counts checkbacks per round
  - Calculates completion percentages
  - Tracks timing metrics
  - Returns: round data + checkback_count, first_checkback_date, days_to_first_checkback, pct_checked

- `create_site_details(treatments, checkbacks, species_data)` - Creates per-checkback detail records
  - **CRITICAL**: Iterates through checkbacks (not sites)
  - For each checkback: finds last treatment BEFORE that checkback
  - Calculates days_to_checkback = checkback_date - treatment_date
  - Joins species_composition from species_data by sampnum_yr
  - Joins redblue and missing from species_data
  - Returns: sitecode, facility, inspection_date, treatment_date, checkback_date, pre_treatment_dips, post_treatment_dips, acres, matcode, mattype, effect_days, days_to_checkback, species_composition, redblue, missing

#### display_functions.R
- `create_checkback_progress_chart(data)` - Stacked bar chart for Progress tab
  - Shows sites treated vs sites checked per facility
  - Facility color scheme
  - Completion percentage labels

- `create_dip_changes_chart(site_details, theme)` - Dumbbell plot for Control Efficacy tab
  - Reshapes data for pre/post comparison
  - Creates connected points with lines
  - Color gradient by days_to_checkback
  - Adds boxplot context
  - Interactive plotly output

- `create_efficacy_scatter(site_details, selected_sites)` - Scatter plot alternative
  - Shows post_treatment_dips vs days_to_checkback
  - Highlights selected sites
  - Shows trend lines

## Data Quality and Edge Cases

### Missing Data Handling

**Missing Checkbacks:**
- Sites without checkbacks are excluded from site_details table
- Progress tab tracks completion rate (shows 0% if no checkbacks)
- Not an error - some treatments may not require checkbacks yet

**Missing Species Data:**
- Species composition shows "[No Species Data]" when no species records
- Species composition shows "[Sample Missing]" when missing = TRUE in dblarv_sample
- Both cases are valid filter options in species filter

**Missing Material Metadata:**
- effect_days defaults to NULL if not in mattype_list_targetdose
- mattype shows matcode if no match found
- Does not break calculations - just limits metadata display

**Missing Facility/Zone:**
- Some sites may not have gis_sectcode matches
- facility and zone will be NULL
- Filters handle NULL appropriately (excluded from specific facility filters)

### Type Conversion Gotcha
**CRITICAL**: Species join requires type conversion:
```sql
-- WRONG (will fail):
LEFT JOIN lookup_specieslist spec ON spp.spp = spec.sppcode

-- CORRECT:
LEFT JOIN lookup_specieslist spec ON spp.spp = CAST(spec.sppcode AS VARCHAR)
```
- `dblarv_species.spp` is VARCHAR
- `lookup_specieslist.sppcode` is INTEGER
- Without CAST, join returns zero matches

### Treatment-Checkback Matching Logic
**Per-Checkback Model** requires finding correct treatment:
```r
# For each checkback, find last treatment BEFORE checkback date
treatment_before <- treatments %>%
  filter(sitecode == checkback$sitecode,
         inspdate <= checkback$inspdate) %>%
  arrange(desc(inspdate)) %>%
  slice(1)
```
- Prevents negative days_to_checkback
- Handles multiple treatments at same site
- Ensures proper treatment-checkback pairing

### Species Composition Aggregation
**Multi-Species Samples** require string aggregation:
```r
# Group by sampnum_yr and concatenate species
species_by_sample <- species_raw %>%
  group_by(sampnum_yr) %>%
  summarize(
    species_composition = paste(
      paste0(genus_abbrev, ". ", species, " (", per, "%)"),
      collapse = ", "
    )
  )
```
- Orders by percentage DESC before concatenation
- Handles 1-N species per sample
- Creates human-readable format

## Testing

### Test Script: test_all_functions.R
Comprehensive testing framework that validates:
1. Database connection
2. Treatment data loading (with material metadata)
3. Checkback data loading (with sample numbers)
4. Species data loading (with type conversion and aggregation)
5. Site details creation (per-checkback model)
6. Display function rendering (chart generation)

### Running Tests
```r
# From control_efficacy directory
Rscript test_all_functions.R

# Or from R console
source("test_all_functions.R")
```

### Test Output
- ✓ = Success
- ✗ = Error
- ⚠ = Warning or skipped

### Common Test Issues
- **No treatments in date range**: Expand start_date range (default 90 days)
- **Checkback query hangs**: Large site lists may be slow, test limits to 10 sites
- **Species data missing**: Expected for samples without species collection

## Known Issues and Warnings

### Harmless Warnings
**"Ignoring unknown aesthetics: text"**
- Appears when creating plotly charts
- Reason: ggplot doesn't recognize plotly's text aesthetic
- Impact: None - plotly uses text for tooltips correctly
- Status: Cosmetic warning, can be ignored

### Performance Considerations
**Large Date Ranges:**
- Queries spanning years may be slow
- Checkback query with 100+ sites can take 10-30 seconds
- Recommendation: Use focused date ranges when possible

**Species Data Loading:**
- Aggregation of species strings can be slow with 1000+ samples
- Progress indicator shows loading status
- Recommendation: Filter by facility to reduce data volume

## Future Enhancements

### Potential Features
- **Species color coding** in display tables
- **Redblue filter** in shared controls
- **Species summary statistics** in value boxes
- **Species trend analysis** over time
- **Material effectiveness comparison** charts
- **Checkback timing optimization** analysis

### Data Model Improvements
- **Add GPS coordinates** to site details for mapping
- **Track multiple checkbacks per site** with sequence numbers
- **Add weather data** for treatment effectiveness context
- **Link to air site data** for site characteristics

## SQL Queries Reference

### Complete Treatment Query
```sql
-- Union current and archive tables for date ranges spanning years
SELECT 
  insp.inspdate,
  gis.facility,
  gis.zone,
  insp.sitecode,
  insp.action,
  insp.numdip,
  insp.diphabitat,
  insp.acres,
  insp.matcode,
  mat.mattype,
  mat.effect_days,
  insp.pkey_pg,
  insp.insptime
FROM public.dblarv_insptrt_current insp
LEFT JOIN public.gis_sectcode gis ON left(insp.sitecode,7) = gis.sectcode
LEFT JOIN public.mattype_list_targetdose mat ON insp.matcode = mat.matcode
WHERE insp.action = 'A'
  AND insp.inspdate >= ?
  AND insp.inspdate <= ?

UNION ALL

SELECT 
  insp.inspdate,
  gis.facility,
  gis.zone,
  insp.sitecode,
  insp.action,
  insp.numdip,
  insp.diphabitat,
  NULL as acres,  -- Not in archive
  insp.matcode,
  mat.mattype,
  mat.effect_days,
  insp.pkey_pg,
  NULL as insptime  -- Not in archive
FROM public.dblarv_insptrt_archive insp
LEFT JOIN public.gis_sectcode gis ON left(insp.sitecode,7) = gis.sectcode
LEFT JOIN public.mattype_list_targetdose mat ON insp.matcode = mat.matcode
WHERE insp.action = 'A'
  AND insp.inspdate >= ?
  AND insp.inspdate <= ?
ORDER BY inspdate
```

### Complete Checkback Query
```sql
-- Query checkbacks for treated sites
SELECT 
  insp.inspdate,
  gis.facility,
  gis.zone,
  insp.sitecode,
  insp.action,
  insp.numdip,
  insp.diphabitat,
  insp.posttrt_p,
  insp.sampnum_yr,
  insp.pkey_pg
FROM public.dblarv_insptrt_current insp
LEFT JOIN public.gis_sectcode gis ON left(insp.sitecode,7) = gis.sectcode
WHERE insp.sitecode IN ('site1', 'site2', ...)
  AND insp.action = '4'
  AND insp.posttrt_p IS NOT NULL
  AND insp.inspdate >= ?
  AND insp.inspdate <= ?

UNION ALL

SELECT 
  insp.inspdate,
  gis.facility,
  gis.zone,
  insp.sitecode,
  insp.action,
  insp.numdip,
  insp.diphabitat,
  insp.posttrt_p,
  insp.sampnum_yr,
  insp.pkey_pg
FROM public.dblarv_insptrt_archive insp
LEFT JOIN public.gis_sectcode gis ON left(insp.sitecode,7) = gis.sectcode
WHERE insp.sitecode IN ('site1', 'site2', ...)
  AND insp.action = '4'
  AND insp.posttrt_p IS NOT NULL
  AND insp.inspdate >= ?
  AND insp.inspdate <= ?
ORDER BY inspdate
```

### Complete Species Query with Type Conversion
```sql
-- Query species composition with required type conversion
SELECT 
  spp.sampnum_yr,
  spp.spp,
  spp.per,
  spec.genus,
  spec.species,
  samp.redblue,
  samp.missing
FROM public.dblarv_species_current spp
LEFT JOIN public.lookup_specieslist spec 
  ON spp.spp = CAST(spec.sppcode AS VARCHAR)  -- Type conversion essential!
LEFT JOIN public.dblarv_sample_current samp 
  ON spp.sampnum_yr = samp.sampnum_yr
WHERE spp.sampnum_yr IN ('2024-12345', '2024-12346', ...)

UNION ALL

SELECT 
  spp.sampnum_yr,
  spp.spp,
  spp.per,
  spec.genus,
  spec.species,
  samp.redblue,
  samp.missing
FROM public.dblarv_species_archive spp
LEFT JOIN public.lookup_specieslist spec 
  ON spp.spp = CAST(spec.sppcode AS VARCHAR)
LEFT JOIN public.dblarv_sample_archive samp 
  ON spp.sampnum_yr = samp.sampnum_yr
WHERE spp.sampnum_yr IN ('2024-12345', '2024-12346', ...)
ORDER BY sampnum_yr, per DESC
```

## Changelog

### 2024-12-05 - Per-Checkback Model Implementation
- **BREAKING CHANGE**: Restructured data model from per-site to per-checkback
- Changed `create_site_details()` to iterate through checkbacks instead of sites
- Added species composition tracking from dblarv_species tables
- Added sample metadata (redblue, missing flags) from dblarv_sample tables
- Implemented species filter with dynamic population
- Added material type and effect days from mattype_list_targetdose
- Updated all column names to reflect new model:
  - treatment_date, checkback_date, days_to_checkback, post_treatment_dips, acres
- Fixed "Recent Brood Only" button to work without dependency on treatment_rounds
- Removed obsolete has_checkback filters (all rows have checkbacks now)
- Created comprehensive test suite (test_all_functions.R)
- Updated all display functions to use new column names
