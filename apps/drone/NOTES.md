# Drone App – Technical Notes

## Database Tables & Entities

| Table | Key Columns Used |
|---|---|
| `loc_breeding_sites` | `sitecode`, `facility`, `acres`, `prehatch`, `drone`, `air_gnd`, `enddate`, `startdate`, `geom` |
| `gis_sectcode` | `sectcode`, `facility`, `zone`, `fosarea` |
| `employee_list` | `emp_num`, `emp_type`, `active` |
| `dblarv_insptrt_current` | `sitecode`, `facility`, `inspdate`, `matcode`, `acres`, `foreman`, `action`, `airgrnd_plan`, `amts` |
| `dblarv_insptrt_archive` | Same as current minus `airgrnd_plan` |
| `mattype_list_targetdose` | `matcode`, `effect_days` |

---

## SQL Queries

### 1. Drone Sites (loc_breeding_sites)

```sql
SELECT b.sitecode, b.facility, b.acres, b.prehatch, b.drone, b.air_gnd,
       g.zone, g.fosarea AS fos,
       ST_X(ST_Transform(ST_Centroid(b.geom), 4326)) AS lng,
       ST_Y(ST_Transform(ST_Centroid(b.geom), 4326)) AS lat
FROM loc_breeding_sites b
LEFT JOIN gis_sectcode g ON b.sectcode = g.sectcode
WHERE (b.drone IN ('Y', 'M', 'C') OR b.air_gnd = 'D')
  AND (b.enddate IS NULL OR b.enddate >= CURRENT_DATE)
  AND (b.startdate IS NULL OR b.startdate <= CURRENT_DATE)
```

> Geometry columns are conditionally included only when `include_geometry = TRUE`. The `ST_Transform(ST_Centroid(...), 4326)` converts UTM centroids to WGS84 lon/lat.

### 2. Current Treatments (dblarv_insptrt_current)

```sql
SELECT t.sitecode, t.facility, t.inspdate, t.matcode, t.acres,
       t.foreman, t.action, t.airgrnd_plan, t.amts,
       g.zone, g.fosarea AS fos,
       m.effect_days
FROM dblarv_insptrt_current t
LEFT JOIN gis_sectcode g ON t.sectcode = g.sectcode
LEFT JOIN mattype_list_targetdose m ON t.matcode = m.matcode
WHERE (t.airgrnd_plan = 'D' OR t.action = 'D')
  AND t.inspdate >= '{start_date}'
  AND t.inspdate <= '{end_date}'
```

### 3. Archive Treatments (dblarv_insptrt_archive)

```sql
SELECT t.sitecode, t.facility, t.inspdate, t.matcode, t.acres,
       t.foreman, t.action, t.amts,
       g.zone, g.fosarea AS fos,
       m.effect_days
FROM dblarv_insptrt_archive t
LEFT JOIN gis_sectcode g ON t.sectcode = g.sectcode
LEFT JOIN mattype_list_targetdose m ON t.matcode = m.matcode
WHERE t.action = 'D'
  AND t.inspdate >= '{start_date}'
  AND t.inspdate <= '{end_date}'
```

> Archive table has no `airgrnd_plan` column — only `action = 'D'` is used.

### 4. Site Statistics CTE

```sql
WITH treatment_data AS (
  -- current + archive UNION ALL, filtered by date range and drone action
),
location_data AS (
  SELECT sitecode, facility, acres
  FROM loc_breeding_sites
  WHERE (drone IN ('Y','M','C') OR air_gnd = 'D')
    AND (enddate IS NULL OR enddate >= CURRENT_DATE)
    AND (startdate IS NULL OR startdate <= CURRENT_DATE)
)
SELECT l.sitecode, l.facility, l.acres,
       COUNT(t.inspdate) AS n_treatments,
       MAX(t.inspdate) AS last_treatment
FROM location_data l
LEFT JOIN treatment_data t ON l.sitecode = t.sitecode
GROUP BY l.sitecode, l.facility, l.acres
```

---

## R-Side Logic

### Treatment Status Classification (data_functions.R)

- `treatment_end_date = inspdate + effect_days` (defaults to 0 if `effect_days` is NA)
- `is_active = treatment_end_date >= current_date`
- `is_expiring = is_active & treatment_end_date <= (current_date + 7)`
- `expiring` is a **subset** of `active`, not a separate category
- Expired count = total − active


### Drone Type Filtering

- Sites: `drone IN ('Y', 'M', 'C') OR air_gnd = 'D'`
- Current treatments: `airgrnd_plan = 'D' OR action = 'D'`
- Archive treatments: `action = 'D'` only


---

## Shapefiles

**None.** All geometry comes from PostGIS (`loc_breeding_sites.geom`), transformed to WGS84 via `ST_Transform(ST_Centroid(geom), 4326)` in SQL, then converted to `sf` in R.

---

## Shared Functions Used

| Function | Source File |
|---|---|
| `get_db_connection()` | `shared/db_helpers.R` |
| `safe_disconnect()` | `shared/db_helpers.R` |
| `get_facility_lookup()` | `shared/db_helpers.R` |
| `get_foremen_lookup()` | `shared/db_helpers.R` |
| `get_facility_choices()` | `shared/db_helpers.R` |
| `get_foreman_choices()` | `shared/db_helpers.R` |
| `get_table_strategy()` | `shared/db_helpers.R` |
| `apply_standard_data_filters()` | `shared/server_utilities.R` |
| `is_valid_filter()` | `shared/server_utilities.R` |
| `cached_load_raw_data()` | `shared/server_utilities.R` |
| `get_universal_text_css()` | `shared/server_utilities.R` |
| `make_sitecode_link()` | `shared/server_utilities.R` |
| `export_csv_safe()` | `shared/server_utilities.R` |
| `set_app_name()` | `shared/server_utilities.R` |
| `load_env_vars()` | `shared/server_utilities.R` |
| `map_facility_names()` | `shared/server_utilities.R` |
| `apply_historical_group_labels()` | `shared/server_utilities.R` |
| `summarize_historical_data()` | `shared/server_utilities.R` |
| `create_trend_chart()` | `shared/server_utilities.R` |
| `map_facility_display_names_to_colors()` | `shared/server_utilities.R` |
| `map_foreman_display_names_to_colors()` | `shared/server_utilities.R` |
| `create_stat_box()` | `shared/stat_box_helpers.R` |
| `get_status_colors()` | `shared/color_themes.R` |
| `get_facility_base_colors()` | `shared/color_themes.R` |
| `get_foreman_colors()` | `shared/color_themes.R` |
| `get_themed_foreman_colors()` | `shared/color_themes.R` |
