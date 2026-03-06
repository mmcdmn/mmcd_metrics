# Structure Treatment App – Technical Notes

## Database Tables & Entities

| Table | Key Columns Used |
|---|---|
| `loc_cxstruct` | `sitecode`, `sectcode`, `s_type`, `priority`, `status_udw`, `enddate`, `startdate` |
| `gis_sectcode` | `sectcode`, `facility`, `zone`, `fosarea` |
| `dblarv_insptrt_current` | `sitecode`, `inspdate`, `matcode`, `list_type` |
| `dblarv_insptrt_archive` | Same columns as current |
| `mattype_list_targetdose` | `matcode`, `effect_days` |
| `employee_list` | `emp_num`, `shortname` |

---

## SQL Queries

### 1. All Structures

```sql
SELECT
  loc.sitecode, gis.facility, loc.s_type, loc.priority,
  loc.status_udw as status, gis.zone, gis.fosarea as foreman
FROM public.loc_cxstruct loc
LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE (loc.enddate IS NULL OR loc.enddate > '%s'::date)
  %s  -- zone/facility/structure_type/priority/status conditions
```

### 2. Current Treatments

```sql
SELECT
  trt.sitecode, trt.inspdate, gis.facility,
  gis.fosarea as foreman,
  COALESCE(mat.effect_days, 30) AS effect_days,
  loc.s_type, loc.priority, loc.status_udw as status, gis.zone
FROM public.dblarv_insptrt_current trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE trt.list_type = 'STR'
  AND (loc.enddate IS NULL OR loc.enddate > '%s'::date)
  AND trt.inspdate <= '%s'::date
  AND loc.startdate <= '%s'::date
  %s  -- zone/facility/structure_type/priority/status conditions
```

> Historical mode appends: `AND EXTRACT(YEAR FROM trt.inspdate) BETWEEN %d AND %d`

### 3. Archive Treatments

Identical structure to current treatments query, from `public.dblarv_insptrt_archive`.

### 4. Historical — Most Recent Treatment Per Site (archive)

```sql
SELECT DISTINCT ON (trt.sitecode)
  trt.sitecode, trt.inspdate,
  COALESCE(mat.effect_days, 30) AS effect_days,
  loc.s_type, loc.priority, gis.facility,
  gis.fosarea as foreman, gis.zone
FROM public.dblarv_insptrt_archive trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE trt.inspdate >= date '%d-01-01'
  AND trt.inspdate < date '%d-01-01'
  AND trt.list_type = 'STR'
  AND (loc.enddate IS NULL OR loc.enddate > trt.inspdate)
  %s  -- filter conditions
ORDER BY trt.sitecode, trt.inspdate DESC
```

> Same query used against current table.

### 5. Historical Total Count

```sql
SELECT COUNT(DISTINCT loc.sitecode) AS total_count
FROM loc_cxstruct loc
LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE 1=1
  AND (loc.enddate IS NULL OR loc.enddate > CURRENT_DATE)
  %s  -- facility/structure_type/priority/status conditions
```

### 6. Historical Proportion Total Counts (grouped variants)

```sql
SELECT
  gis.facility, [gis.zone,] [gis.fosarea,]
  COUNT(DISTINCT loc.sitecode)::INTEGER as total_count
FROM loc_cxstruct loc
INNER JOIN gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE 1=1
  %s  -- filter conditions
GROUP BY gis.facility [, gis.zone] [, gis.fosarea]
```

### 7. Employee Number Lookup

```sql
SELECT emp_num
FROM employee_list
WHERE shortname IN (%s)
```

### 8. Historical Load (for trend charts)

```sql
SELECT DISTINCT
  trt.sitecode, trt.inspdate,
  COALESCE(mat.effect_days, 30) AS effect_days,
  loc.s_type, loc.priority, gis.facility, gis.fosarea,
  gis.zone, EXTRACT(YEAR FROM trt.inspdate) as treatment_year
FROM public.dblarv_insptrt_current trt
LEFT JOIN public.mattype_list_targetdose mat ON trt.matcode = mat.matcode
LEFT JOIN public.loc_cxstruct loc ON trt.sitecode = loc.sitecode
LEFT JOIN public.gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE EXTRACT(YEAR FROM trt.inspdate) IN (%s)
  AND trt.list_type = 'STR'
  AND (loc.enddate IS NULL OR loc.enddate > trt.inspdate)
  %s  -- filter conditions
```

> Same structure for archive table with `BETWEEN %d AND %d` for year range.

---

## R-Side Logic

### Treatment Status Classification (data_functions.R)

```r
days_since_treatment = as.numeric(analysis_date - inspdate)
is_active = days_since_treatment <= effect_days
is_expiring = days_since_treatment > (effect_days - expiring_days) &
              days_since_treatment <= effect_days
```

- `effect_days` defaults to 30 via `COALESCE(mat.effect_days, 30)` in SQL
- `is_expiring` is a subset of `is_active`

### Structure Type Filtering

- `CV` matches: `'CV'`, `'CV/%'`, `'%/CV'` (case-insensitive)
- `PR` matches: `'PR'`, `'PR/%'`, `'%/PR'` (case-insensitive)
- Other types: exact case-insensitive `UPPER()` match

### Foreman Filter Resolution

UI `shortname` values → `emp_num` via cached `get_foremen_lookup()`, then builds SQL `IN` clause on `gis.fosarea`.

### Aggregation (`aggregate_structure_data()`)

Groups by `facility`, `foreman`, or `mmcd_all`:
- When showing zones separately: `combined_group = "{name} P{zone}"`
- `total_count` = all structures, `active_count` = distinct sitecodes with `is_active = TRUE`, `expiring_count` = distinct sitecodes with `is_expiring = TRUE`
- Maps `emp_num` → `shortname` and facility codes → full names for display

### Display Table Status

```r
treatment_status = case_when(
  is_active & is_expiring ~ "Expiring",
  is_active               ~ "Treated",
  TRUE                    ~ "Expired"
)
```

Structures without any treatment record get "No Treatment".

### Historical Weekly Analysis (historical_functions.R)

Iterates each Friday in date range, determines active treatments on each Friday:
```r
filter(as.Date(inspdate) <= week_friday, treatment_end >= week_friday)
```

### Historical Proportion Calculation

- Counts treated structures per group per time period
- Re-queries DB for total structure counts per group with identical filters
- `proportion = (treated_structures / total_count) * 100`

---

## Shapefiles

**None.** No `.shp`, `.gpkg`, or PostGIS geometry used. The `include_geometry` parameter exists in `load_raw_data()` signature but is never used in query logic.

---

## Shared Functions Used

| Function | Source File |
|---|---|
| `get_db_connection()` | `shared/db_helpers.R` |
| `safe_disconnect()` | `shared/db_helpers.R` |
| `build_sql_in_clause()` | `shared/db_helpers.R` |
| `build_sql_equals_clause()` | `shared/db_helpers.R` |
| `build_sql_in_list()` | `shared/db_helpers.R` |
| `is_valid_filter()` | `shared/db_helpers.R` |
| `get_facility_lookup()` | `shared/db_helpers.R` |
| `get_foremen_lookup()` | `shared/db_helpers.R` |
| `get_facility_choices()` | `shared/db_helpers.R` |
| `get_foreman_choices()` | `shared/db_helpers.R` |
| `get_structure_type_choices()` | `shared/db_helpers.R` |
| `map_facility_names()` | `shared/db_helpers.R` |
| `get_table_strategy()` | `shared/db_helpers.R` |
| `get_historical_year_ranges()` | `shared/db_helpers.R` |
| `get_universal_text_css()` | `shared/server_utilities.R` |
| `make_sitecode_link()` | `shared/server_utilities.R` |
| `export_csv_safe()` | `shared/server_utilities.R` |
| `set_app_name()` | `shared/server_utilities.R` |
| `cached_load_raw_data()` | `shared/server_utilities.R` |
| `apply_standard_data_filters()` | `shared/server_utilities.R` |
| `create_stat_box()` | `shared/stat_box_helpers.R` |
| `get_status_colors()` | `shared/color_themes.R` |
| `map_facility_display_names_to_colors()` | `shared/color_themes.R` |
| `map_foreman_display_names_to_colors()` | `shared/color_themes.R` |
| `get_facility_base_colors()` | `shared/color_themes.R` |
| `create_trend_chart()` | `shared/server_utilities.R` |
