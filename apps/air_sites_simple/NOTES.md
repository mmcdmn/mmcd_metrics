# Air Sites Simple — Developer Notes

## Database Tables & Entities

| Table | Purpose |
|-------|---------|
| `loc_breeding_sites` | Master site list. Filtered by `air_gnd='A'`, `geom IS NOT NULL`. Columns: `sitecode`, `acres`, `enddate`, `startdate`, `air_gnd`, `priority`, `geom` (PostGIS, UTM). |
| `gis_sectcode` | Section → facility / zone / fosarea mapping. Joined via `LEFT(sitecode, 7) = sectcode`. |
| `dblarv_insptrt_current` | Current-year inspections (actions `'2','4'`) and treatments (actions `'3','A','D'`). |
| `dblarv_insptrt_archive` | Archive inspections/treatments. Used when `get_table_strategy()` indicates archive or for historical analysis. |
| `dblarv_sample_current` | Lab sample results. Filtered by `form_type='AIR'` and `missing=FALSE`. Joined on `sampnum_yr`. Provides `redblue` (R/B). |
| `dblarv_sample_archive` | Archive lab samples; same schema, used for historical queries. |
| `mattype_list_targetdose` | Material metadata: `effect_days`, `prehatch`, `tdose`, `unit`, `area`. Joined on `matcode`. Default `effect_days` = 14 when NULL. |
| `employee_list` | Employee lookup (used indirectly via shared helpers). |

## SQL Queries

### Main point-in-time query (`get_air_sites_data()`)

```sql
WITH ActiveAirSites AS (
  SELECT b.facility, b.sitecode, b.acres, b.priority, g.zone,
         ST_X(ST_Centroid(ST_Transform(b.geom, 4326))) AS longitude,
         ST_Y(ST_Centroid(ST_Transform(b.geom, 4326))) AS latitude
  FROM loc_breeding_sites b
  LEFT JOIN gis_sectcode g ON g.sectcode = LEFT(b.sitecode, 7)
  WHERE (b.enddate IS NULL OR b.enddate > :analysis_date)
    AND b.air_gnd = 'A' AND b.geom IS NOT NULL
    -- optional: AND b.facility IN (:facility_filter)
    -- optional: AND b.priority IN (:priority_filter)
    -- optional: AND g.zone = :zone_filter
),

RecentInspections AS (
  SELECT i.sitecode, i.inspdate AS last_inspection_date, i.numdip, i.sampnum_yr,
         ROW_NUMBER() OVER (PARTITION BY i.sitecode ORDER BY i.inspdate DESC) AS rn
  FROM <inspection_table> i
  WHERE i.inspdate <= :analysis_date AND i.action IN ('2','4')
    AND i.sitecode IN (SELECT sitecode FROM ActiveAirSites)
),

RecentTreatments AS (
  SELECT t.sitecode, t.inspdate AS last_treatment_date, t.matcode, t.mattype,
         ROW_NUMBER() OVER (PARTITION BY t.sitecode ORDER BY t.inspdate DESC) AS rn
  FROM <inspection_table> t
  WHERE t.inspdate <= :analysis_date AND t.action IN ('3','A','D')
    AND t.matcode IS NOT NULL AND t.matcode != ''
    AND t.sitecode IN (SELECT sitecode FROM ActiveAirSites)
),

LabSampleData AS (
  SELECT ls.sampnum_yr, ls.redblue, ls.missing,
         CASE WHEN ls.redblue = 'R' THEN 1 ELSE 0 END AS has_red_bugs
  FROM <sample_table> ls
  WHERE ls.sampnum_yr IS NOT NULL AND ls.missing = FALSE AND ls.form_type = 'AIR'
),

TreatmentInfo AS (
  SELECT rt.sitecode, rt.last_treatment_date, rt.matcode, rt.mattype,
         CONCAT(rt.mattype, ' ', mt.tdose, ' ', mt.unit, ' per ', mt.area) AS last_treatment_material,
         rt.last_treatment_date + INTERVAL '1 day' * COALESCE(mt.effect_days, 14) AS treatment_expiry
  FROM RecentTreatments rt
  LEFT JOIN mattype_list_targetdose mt ON rt.matcode = mt.matcode
  WHERE rt.rn = 1
),

InspectionInfo AS (
  SELECT ri.sitecode, ri.last_inspection_date, ri.numdip, ri.sampnum_yr,
         ls.redblue, ls.has_red_bugs, ls.missing
  FROM RecentInspections ri
  LEFT JOIN LabSampleData ls ON ri.sampnum_yr = ls.sampnum_yr
  WHERE ri.rn = 1
)

SELECT a.*, t.last_treatment_date, t.matcode, t.last_treatment_material, t.treatment_expiry,
       i.last_inspection_date, i.numdip AS last_larvae_count, i.sampnum_yr,
       i.redblue, i.has_red_bugs, i.missing
FROM ActiveAirSites a
LEFT JOIN TreatmentInfo t ON a.sitecode = t.sitecode
LEFT JOIN InspectionInfo i ON a.sitecode = i.sitecode
ORDER BY a.sitecode
```

A configurable `bti_effect_days_override` can replace the `COALESCE(mt.effect_days, 14)` for Bti_gran materials.

### Historical air treatments query (`load_air_treatments()`)

```sql
SELECT t.inspdate, b.facility, g.zone, g.fosarea, b.sitecode, b.acres,
       COALESCE(mt.effect_days, 14) AS effect_days
FROM <inspection_table> t
JOIN loc_breeding_sites b ON t.sitecode = b.sitecode
LEFT JOIN mattype_list_targetdose mt ON t.matcode = mt.matcode
LEFT JOIN gis_sectcode g ON LEFT(b.sitecode, 7) = g.sectcode
WHERE t.action IN ('3','A','D')
  AND t.matcode IS NOT NULL AND t.matcode != ''
  AND b.air_gnd = 'A' AND b.geom IS NOT NULL
  -- optional: AND b.priority IN (:priority_filter)
  -- optional: AND g.zone IN (:zone_filter)
  AND EXTRACT(YEAR FROM t.inspdate) BETWEEN :start_year AND :end_year
```

Runs against both `_current` and `_archive` tables; results are `bind_rows()`'d.

### Comprehensive historical query (`get_comprehensive_historical_data()`)

A large `UNION ALL` query that combines archive + current inspections and treatments into `combined_operations`, joins to `combined_samples` (archive + current lab data), then produces two result sets in one trip:

- **`site_inspection_summary`** — per-site aggregation: total inspections, red-bug inspections, samples with results, years active.
- **`treatment_volume_summary`** — per-date/facility aggregation: operation counts, total acres, week/year breakdowns.

Returned as a single query with a `dataset_type` discriminator column; split in R.

## R-Side Logic

- **Site status classification** (`apply_site_status_logic()`): Each site is assigned one of five statuses in priority order:
  1. **Active Treatment** — `treatment_expiry > analysis_date`
  2. **Needs Treatment** — inspected within 7 days, `larvae_count >= threshold`, red bugs confirmed or no sample taken
  3. **Needs ID** — inspected within 7 days, sample sent but `missing = TRUE` (lab pending)
  4. **Inspected** — inspected within 7 days, under threshold or blue bugs only
  5. **Unknown** — no recent data, or inspection expired beyond 7 days, or treatment after inspection has expired
- **Post-treatment inspection invalidation**: If a treatment occurred after the last inspection and the treatment has now expired, the site reverts to Unknown (the stale inspection is not used).
- **Lab processing metrics** (`analyze_lab_processing_metrics()`): Computes red-bug detection rate, samples above/below threshold, pending-lab counts from the loaded data.
- **Treatment efficiency metrics** (`create_treatment_efficiency_metrics()`): Calculates coverage and treatment rates in both site-count and acres modes.
- **Facility name mapping**: `map_facility_names()` converts short codes to display names throughout charts and tables.

## Shapefiles

Loaded via `shared/geometry_helpers.R`:

| Layer | Source | Contents |
|-------|--------|----------|
| Facility boundaries | `shared/Q_to_R/data/facility_boundaries.shp` | Facility boundary polygons for map overlay |
| Zone boundaries | `shared/Q_to_R/data/zone_boundaries.shp` | P1/P2 zone boundary polygons |
| Air site polygons | `shared/Q_to_R/data/air_sites/<facility>/<emp_num>_airsites.shp` | Per-FOS air site polygons. Loaded on demand when "Load Air Site Polygons" toggle is enabled; hidden below zoom level 13. |

## Shared Functions Used

| Function | Source |
|----------|--------|
| `get_db_connection()` / `safe_disconnect()` | `shared/db_helpers.R` |
| `is_valid_filter()` / `build_sql_in_list()` | `shared/db_helpers.R` |
| `get_table_strategy()` | `shared/db_helpers.R` |
| `get_historical_year_ranges()` | `shared/db_helpers.R` |
| `get_facility_lookup()` / `get_foremen_lookup()` | `shared/db_helpers.R` |
| `get_facility_choices()` / `get_priority_choices()` | `shared/db_helpers.R` |
| `get_available_zones()` / `get_material_choices()` | `shared/db_helpers.R` |
| `map_facility_names()` | `shared/db_helpers.R` |
| `make_sitecode_link()` | `shared/db_helpers.R` |
| `apply_standard_data_filters()` | `shared/db_helpers.R` |
| `get_status_colors()` / `get_status_color_map()` | `shared/db_helpers.R` |
| `create_stat_box()` / `create_status_stat_box()` | `shared/stat_box_helpers.R` |
| `load_background_layers()` / `add_background_layers()` | `shared/geometry_helpers.R` |
| `load_airsite_layers()` / `add_airsite_layer()` | `shared/geometry_helpers.R` |
| `set_app_name()` / `load_env_vars()` | `shared/server_utilities.R` |
| `get_universal_text_css()` | `shared/server_utilities.R` |
