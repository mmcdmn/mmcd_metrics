# Inspections App – Technical Notes

## Database Tables & Entities

| Table | Key Columns Used |
|---|---|
| `loc_breeding_sites` | `sitecode`, `enddate`, `air_gnd`, `priority`, `drone`, `acres`, `prehatch` |
| `gis_sectcode` | `sectcode`, `facility`, `fosarea`, `zone` |
| `dblarv_insptrt_current` | `sitecode`, `inspdate`, `action`, `numdip`, `wet`, `sampnum_yr` |
| `dblarv_insptrt_archive` | Same columns as current |
| `dblarv_sample_current` | `sampnum_yr`, `redblue` |
| `dblarv_sample_archive` | `sampnum_yr`, `redblue` |

---

## SQL Queries

### 1. Overview Registry Fast Path — Prehatch Ground Sites

```sql
SELECT b.sitecode, sc.facility, sc.fosarea, sc.zone
FROM loc_breeding_sites b
INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
WHERE (b.enddate IS NULL OR b.enddate > '%s'::date)
  AND b.air_gnd = 'G'
  AND b.prehatch IS NOT NULL
```

### 2. Overview Registry Fast Path — Latest Red Bug Sample (current)

```sql
SELECT i.sitecode, MAX(i.inspdate) AS last_insp
FROM dblarv_insptrt_current i
INNER JOIN dblarv_sample_current s ON i.sampnum_yr = s.sampnum_yr
WHERE i.inspdate IS NOT NULL
  AND i.sampnum_yr IS NOT NULL
  AND s.redblue = 'R'
GROUP BY i.sitecode
```

### 3. Overview Registry Fast Path — Latest Red Bug Sample (archive, chunked)

```sql
SELECT i.sitecode, MAX(i.inspdate) AS archive_insp
FROM dblarv_insptrt_archive i
INNER JOIN dblarv_sample_archive s ON i.sampnum_yr = s.sampnum_yr
WHERE i.sitecode IN (%s)
  AND i.inspdate IS NOT NULL
  AND i.sampnum_yr IS NOT NULL
  AND i.inspdate >= '%s'::date - interval '5 years'
  AND s.redblue = 'R'
GROUP BY i.sitecode
```

> Archive queries are chunked in batches of 5000 sitecodes via IN-clause.

### 4. Main Comprehensive Query (normal app mode)

```sql
WITH filtered_sites AS (
  SELECT
    b.sitecode, sc.facility, sc.fosarea, sc.zone,
    b.air_gnd, b.priority, b.drone, b.acres, b.prehatch
  FROM loc_breeding_sites b
  INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
  WHERE (b.enddate IS NULL OR b.enddate > '%s'::date)
    %s  -- dynamic filter clause
),
site_years AS (
  SELECT
    sitecode,
    array_to_string(
      array_agg(DISTINCT EXTRACT(year FROM inspdate)::text
                ORDER BY EXTRACT(year FROM inspdate)::text), ', '
    ) as years_with_data
  FROM (
    SELECT sitecode, inspdate FROM dblarv_insptrt_current WHERE inspdate IS NOT NULL
    UNION ALL
    SELECT sitecode, inspdate FROM dblarv_insptrt_archive WHERE inspdate IS NOT NULL
  ) all_inspections
  GROUP BY sitecode
)
SELECT
  fs.sitecode, fs.facility, fs.fosarea, fs.zone,
  fs.air_gnd, fs.priority, fs.drone, fs.acres, fs.prehatch,
  COALESCE(sy.years_with_data, 'No data') as years_with_data,
  i.inspdate, i.action, i.numdip, i.wet
FROM filtered_sites fs
LEFT JOIN site_years sy ON fs.sitecode = sy.sitecode
LEFT JOIN (
  SELECT sitecode, inspdate, action, numdip, wet FROM dblarv_insptrt_current
  UNION ALL
  SELECT sitecode, inspdate, action, numdip, wet FROM dblarv_insptrt_archive
) i ON fs.sitecode = i.sitecode
ORDER BY fs.sitecode, i.inspdate DESC
```

### 5. Site Choices Helper

```sql
SELECT DISTINCT facility, fosarea, zone
FROM gis_sectcode
ORDER BY facility, fosarea, zone
```

---

## R-Side Logic

### Two Execution Paths in `load_raw_data()`

1. **Overview registry fast path** (`status_types = character(0)`): Three separate queries for prehatch ground sites + red bug detection from `dblarv_sample_current/archive` (`redblue = 'R'`). Archive chunked in 5000-sitecode batches. Returns `list(sites, treatments, pre_aggregated = TRUE)`.

2. **Normal app path**: Single CTE query joining sites → inspections (current UNION ALL archive) with a `site_years` CTE aggregating distinct inspection years per site.

### Filter Building

- Uses `build_sql_in_list()` for facility, zone, priority
- FOS area filter maps `shortname` → `emp_num` via `get_foremen_lookup()`, zero-pads to 4 digits
- Drone filter: `"drone_only"` → `b.drone = 'Y'`; `"no_drone"` → `(b.drone IS NULL OR b.drone != 'Y')`

### Spring Filtering (R-side, post-query)

Uses `get_spring_date_thresholds()` for per-year cutoff dates. `is_spring_inspection()` checks `inspdate >= Jan 1` AND `inspdate < spring_cutoff`. Recalculates `years_with_data` for spring-only records. Preserves all sites even without spring inspections.

### Prehatch Filtering (R-side)

`filter(!is.na(prehatch))` — applied post-query.

### Action Filter Rule

Used in inspection gaps, larvae threshold, and summary stats:
```r
action %in% c('1','2','4') | (action == '3' & wet == '0')
```
Includes inspections with actions 1/2/4 unconditionally, and action 3 only when `wet == '0'` (dry).

### Inspection Gaps Analysis (`get_inspection_gaps_from_data()`)

- Applies action filter, groups by site, takes most recent `inspdate`
- No matching record → "Never Inspected" (date = `1900-01-01`, days = 999999)
- Last inspection before `ref_date - years_gap` → "Inspection Gap"

### Wet Frequency Analysis (`get_wet_frequency_from_data()`)

- **No action filtering** — uses all records with `!is.na(wet)` and within cutoff date
- Wet = any of `'1'-'9'` or `'A'`; `'A'` = flooded
- Calculates `wet_percentage` and `flooded_percentage` per site
- Filters to `total_inspections >= min_inspections`

### High Larvae Analysis (`get_high_larvae_sites_from_data()`)

- Applies action filter, filters to `inspdate >= cutoff_date` and `!is.na(numdip)`
- Per-site: `total_inspections`, `threshold_exceedances` (count of `numdip >= threshold`), `max_dip_count`, `avg_dip_count`, `exceedance_frequency` (percentage)
- Sorted by exceedance frequency descending

### Facility Gap Analysis (`get_facility_gap_analysis()`)

Joins total sites (distinct sitecodes by facility + fosarea) with gap sites, calculates gap vs. recently-inspected percentages.

### Display: Conditional DT Styling

- **Inspection Gaps table**: `Days Since` colored by interval (green < 365d < yellow < 730d < orange < 1095d < red). Never Inspected rows → pink background.
- **Wet Frequency table**: `wet_percentage` → 7-tier blue gradient (10/20/40/60/80/95%). `flooded_percentage` → 7-tier green gradient (5/10/20/40/60/80%).
- **High Larvae table**: `exceedance_frequency` → 7-tier purple-to-red gradient (5/10/25/50/75/90%). `avg_dip_count` → 6-tier blue-to-orange gradient (1/3/6/12/25).

---

## Shapefiles

**None.** No `.shp`, `.gpkg`, or PostGIS geometry used in this app.

---

## Shared Functions Used

| Function | Source File |
|---|---|
| `get_db_connection()` | `shared/db_helpers.R` |
| `safe_disconnect()` | `shared/db_helpers.R` |
| `is_valid_filter()` | `shared/db_helpers.R` |
| `build_sql_in_list()` | `shared/db_helpers.R` |
| `get_foremen_lookup()` | `shared/db_helpers.R` |
| `get_facility_lookup()` | `shared/db_helpers.R` |
| `get_foreman_choices()` | `shared/db_helpers.R` |
| `get_priority_choices()` | `shared/db_helpers.R` |
| `get_spring_date_thresholds()` | `shared/db_helpers.R` |
| `get_universal_text_css()` | `shared/db_helpers.R` |
| `make_sitecode_link()` | `shared/server_utilities.R` |
| `set_app_name()` | `shared/server_utilities.R` |
| `export_csv_safe()` | `shared/server_utilities.R` |
| `map_facility_names()` | `shared/server_utilities.R` |
| `create_distribution_chart()` | `shared/server_utilities.R` |
| `cached_load_raw_data()` | `shared/server_utilities.R` |
| `create_stat_box()` | `shared/stat_box_helpers.R` |
| `get_status_colors()` | `shared/color_themes.R` |
