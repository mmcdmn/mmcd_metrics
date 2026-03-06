# Ground Prehatch Progress App – Technical Notes

## Database Tables & Entities

| Table | Key Columns Used |
|---|---|
| `loc_breeding_sites` | `sitecode`, `acres`, `air_gnd`, `priority`, `prehatch`, `drone`, `remarks`, `enddate`, `geom` |
| `gis_sectcode` | `sectcode`, `facility`, `zone`, `fosarea` |
| `dblarv_insptrt_current` | `sitecode`, `inspdate`, `matcode`, `insptime`, `acres` (as `treated_acres`) |
| `dblarv_insptrt_archive` | Same columns as current |
| `mattype_list_targetdose` | `matcode`, `prehatch` (boolean), `effect_days` |

---

## SQL Queries

### 1. Prehatch Sites

```sql
SELECT sc.facility, sc.zone, sc.fosarea, left(b.sitecode,7) AS sectcode,
       b.sitecode, acres, air_gnd, priority, prehatch, drone, remarks,
       sc.fosarea as foreman
       [, ST_AsText(ST_Transform(b.geom, 4326)) as geometry]  -- conditional
FROM loc_breeding_sites b
LEFT JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
WHERE (b.enddate IS NULL OR b.enddate > '<analysis_date>'::date)
  AND b.air_gnd = 'G'
  AND b.prehatch IN ('PREHATCH','BRIQUET','PELLET')
ORDER BY sc.facility, sc.sectcode, b.sitecode, b.prehatch
```

> Geometry column is only included when `include_geometry = TRUE`. Converts PostGIS geom to WGS84 WKT.

### 2. Current Treatments

```sql
SELECT c.sitecode, c.inspdate, c.matcode, c.insptime,
       c.acres as treated_acres, sc.facility, sc.zone, sc.fosarea,
       p.effect_days, 'current' as data_source
FROM dblarv_insptrt_current c
JOIN gis_sectcode sc ON left(c.sitecode, 7) = sc.sectcode
JOIN mattype_list_targetdose p ON c.matcode = p.matcode
WHERE p.prehatch IS TRUE
  AND c.inspdate <= '<analysis_date>'::date
  AND EXISTS (SELECT 1 FROM loc_breeding_sites b
              WHERE b.sitecode = c.sitecode AND b.prehatch IS NOT NULL)
```

> Historical mode adds: `AND EXTRACT(YEAR FROM c.inspdate) BETWEEN <start_year - 1> AND <end_year>`. The year buffer (`start_year - 1`) captures long-lasting briquette treatments applied late in the prior year.

### 3. Archive Treatments

```sql
SELECT c.sitecode, c.inspdate, c.matcode, c.insptime,
       c.acres as treated_acres, sc.facility, sc.zone, sc.fosarea,
       p.effect_days, 'archive' as data_source
FROM dblarv_insptrt_archive c
JOIN gis_sectcode sc ON left(c.sitecode, 7) = sc.sectcode
JOIN mattype_list_targetdose p ON c.matcode = p.matcode
WHERE p.prehatch IS TRUE
  AND c.inspdate <= '<analysis_date>'::date
  AND EXISTS (SELECT 1 FROM loc_breeding_sites b
              WHERE b.sitecode = c.sitecode AND b.prehatch IS NOT NULL)
```

> Same year-buffer logic as current query when in historical mode.

---

## R-Side Logic

### Status Classification (data_functions.R)

Most recent treatment per site is found via `desc(inspdate), desc(insptime)`, then:

```r
age = as.numeric(analysis_date - inspdate)
prehatch_status = case_when(
  age > effect_days                        ~ "expired",
  age > (effect_days - expiring_days)      ~ "expiring",
  age <= effect_days                       ~ "treated",
  TRUE                                     ~ "unknown"
)
```

- `effect_days` comes from `mattype_list_targetdose` per material
- `expiring_days` is user-configurable (slider, default 14)
- Sites with **no treatment records** are classified as `"expired"`

### Active Treatment Stamps (raw data level)

```r
treatment_end = inspdate + effect_days
is_active = treatment_end >= current_date
is_expiring = is_active & treatment_end <= (current_date + 7)  # hardcoded 7-day window
```

### Aggregation (`get_ground_prehatch_data()`)

Groups by `facility`, `zone`, `fosarea`, `sectcode` and produces:
- `prehatch_sites_cnt` / `prehatch_acres` — total prehatch universe
- `ph_treated_cnt/acres`, `ph_expiring_cnt/acres`, `ph_expired_cnt/acres`, `ph_skipped_cnt/acres`
- Standardized: `total_count`, `active_count`, `expiring_count`, `expired_count`, `total_acres`, `active_acres`, `expiring_acres`

### Group-By Aggregation (`aggregate_data_by_group()`)

Supports 4 levels: `mmcd_all`, `facility`, `foreman`, `sectcode`. When `zone_filter = "1,2"` (not combined), adds zone dimension (e.g., "Sj P1", "Sj P2"). Computes `treated_pct = 100 * ph_treated_cnt / prehatch_sites_cnt`.

### Expiring Filter Logic

- `"expiring"` — zeros out treated/expired counts, shows only expiring sites
- `"expiring_expired"` — zeros out treated counts, shows expiring + expired combined

### Historical Weekly Active Logic (historical_functions.R)

Iterates every Friday in the date range:
```r
treatment_end = inspdate + ifelse(is.na(effect_days), 14, effect_days)
filter(inspdate <= week_friday, treatment_end >= week_friday)
```
Counts unique sites (or sums unique site acres) with active treatment on each Friday.

### Historical Acre Deduplication

For `acres`, `site_acres`, `active_acres` metrics:
```r
value = sum(acres[!duplicated(sitecode)], na.rm = TRUE)
```
Each site's acres counted only once per time period, regardless of repeat treatments.


---

## Shapefiles

**None.** All geometry comes from PostGIS (`loc_breeding_sites.geom`), transformed to WGS84 via `ST_AsText(ST_Transform(b.geom, 4326))` in SQL, then converted to `sf` in R.

---

## Shared Functions Used

| Function | Source File |
|---|---|
| `get_db_connection()` | `shared/db_helpers.R` |
| `safe_disconnect()` | `shared/db_helpers.R` |
| `get_facility_lookup()` | `shared/db_helpers.R` |
| `get_foremen_lookup()` | `shared/db_helpers.R` |
| `get_facility_choices()` | `shared/db_helpers.R` |
| `map_facility_names()` | `shared/db_helpers.R` |
| `is_valid_filter()` | `shared/db_helpers.R` |
| `get_table_strategy()` | `shared/db_helpers.R` |
| `get_universal_text_css()` | `shared/server_utilities.R` |
| `make_sitecode_link()` | `shared/server_utilities.R` |
| `export_csv_safe()` | `shared/server_utilities.R` |
| `set_app_name()` | `shared/server_utilities.R` |
| `apply_standard_data_filters()` | `shared/server_utilities.R` |
| `apply_historical_group_labels()` | `shared/server_utilities.R` |
| `summarize_historical_data()` | `shared/server_utilities.R` |
| `create_trend_chart()` | `shared/server_utilities.R` |
| `map_facility_display_names_to_colors()` | `shared/server_utilities.R` |
| `map_foreman_display_names_to_colors()` | `shared/server_utilities.R` |
| `create_stat_box()` | `shared/stat_box_helpers.R` |
| `get_status_colors()` | `shared/color_themes.R` |
| `get_facility_base_colors()` | `shared/color_themes.R` |
| `get_foreman_colors()` | `shared/color_themes.R` |
