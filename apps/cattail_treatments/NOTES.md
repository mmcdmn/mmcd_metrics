# Cattail Treatments – Developer Notes

## Database Tables & Entities

| Table | Purpose |
|-------|---------|
| `dblarv_insptrt_current` | Current inspection/treatment records. Inspections: `action = '9'`. Treatments: `action IN ('3', 'A', 'D')`. |
| `dblarv_insptrt_archive` | Historical inspection/treatment records (same schema as current). |
| `loc_breeding_sites` | Site master — `sitecode`, `acres`, `enddate`, `air_gnd`, `drone` ('Y'/'M'/'C'), `geom` (PostGIS, used for map). |
| `gis_sectcode` | Zone/facility/FOS lookup — join `LEFT(sitecode, 7) = sectcode`. Provides `facility`, `zone` ('1'/'2'), `fosarea`. |
| `mattype_list_targetdose` | Material code lookup — `prgassign_default = 'Cat'` or `prg_alt1 = 'Cat'` identifies cattail materials. |

---

## SQL Queries

### 1. Main Cattail Inspections (`data_functions.R → load_raw_data`)

Parameterized query (`$1` = analysis_date, `$2` = start_date, `$3` = include_archive boolean):

```sql
WITH breeding_sites AS (
  SELECT sc.facility, sc.zone, sc.fosarea, LEFT(b.sitecode,7) AS sectcode,
         b.sitecode, b.acres, b.air_gnd,
         CASE WHEN b.drone IS NOT NULL THEN 'D' ELSE NULL END as drone,
         sc.fosarea as foreman
  FROM public.loc_breeding_sites b
  LEFT JOIN public.gis_sectcode sc ON LEFT(b.sitecode,7) = sc.sectcode
  WHERE (b.enddate IS NULL OR b.enddate > $1)
),
inspection_data AS (
  WITH all_inspections AS (
    SELECT i.sitecode, i.inspdate, i.action, i.numdip,
           COALESCE(i.acres_plan, b.acres) as acres_plan,
           EXTRACT(year FROM i.inspdate) as year
    FROM public.dblarv_insptrt_current i
    LEFT JOIN public.loc_breeding_sites b ON i.sitecode = b.sitecode
    WHERE i.inspdate BETWEEN $2 AND $1 AND i.action = '9'
    UNION ALL
    SELECT a.sitecode, a.inspdate, a.action, a.numdip,
           COALESCE(a.acres_plan, b.acres) as acres_plan,
           EXTRACT(year FROM a.inspdate) as year
    FROM public.dblarv_insptrt_archive a
    LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
    WHERE $3 = TRUE AND a.inspdate BETWEEN $2 AND $1 AND a.action = '9'
  )
  SELECT DISTINCT ON (sitecode) sitecode, inspdate, action, numdip, acres_plan, year
  FROM all_inspections
  ORDER BY sitecode, inspdate DESC
)
SELECT b.*, i.inspdate, i.numdip, i.acres_plan, i.year as inspection_year,
       CASE WHEN i.numdip > 0 THEN 'need_treatment' ELSE 'under_threshold' END as treatment_status
FROM breeding_sites b
INNER JOIN inspection_data i ON b.sitecode = i.sitecode
ORDER BY b.sitecode
```

### 2. Cattail Treatments (`data_functions.R → load_cattail_treatments`)

Parameterized (`$1` = analysis_date). Filters by cattail material codes and seasonal DOY windows:

```sql
SELECT i.sitecode, LEFT(i.sitecode,7) AS sectcode, i.inspdate AS trtdate,
       i.action, i.mattype, i.matcode, i.amts, i.acres,
       sc.facility, sc.zone, sc.fosarea,
       EXTRACT(year FROM i.inspdate) as trt_year,
       EXTRACT(month FROM i.inspdate) as trt_month
FROM public.dblarv_insptrt_current i
LEFT JOIN public.gis_sectcode sc ON LEFT(i.sitecode,7) = sc.sectcode
WHERE i.action IN ('3', 'A', 'D')
  AND i.inspdate <= $1
  AND i.matcode IN (
    SELECT matcode FROM public.mattype_list_targetdose
    WHERE prgassign_default = 'Cat' OR prg_alt1 = 'Cat'
  )
  AND (
    EXTRACT(DOY FROM i.inspdate) BETWEEN 244 AND 365   -- Fall/Winter: Sept 1 – Dec 31
    OR EXTRACT(DOY FROM i.inspdate) BETWEEN 135 AND 213  -- Spring/Summer: May 15 – Aug 1
  )

UNION ALL
-- (identical block for dblarv_insptrt_archive)
ORDER BY trtdate
```

### 3. Historical Inspections (`historical_functions.R → get_historical_cattail_data`)

Same season-DOY filters as above. Adds computed `inspection_year`:

```sql
CASE
  WHEN EXTRACT(DOY FROM inspdate) BETWEEN 244 AND 365
       THEN EXTRACT(year FROM inspdate)          -- fall belongs to that calendar year
  WHEN EXTRACT(DOY FROM inspdate) BETWEEN 135 AND 213
       THEN EXTRACT(year FROM inspdate) - 1      -- spring belongs to PREVIOUS year
  ELSE EXTRACT(year FROM inspdate)
END as inspection_year
```

Both archive and current tables are UNION ALL'd, filtered by date range `$1..$2`, `action = '9'`, and enddate check.

### 4. Historical Treatments (`historical_functions.R`)

Same structure as #3 but filters `action IN ('3', 'A', 'D')` with the cattail material subquery. Same DOY/season `inspection_year` logic.

---

## R-Side Logic

- **3-state treatment workflow**: Every site that had a cattail inspection (`action = '9'`) gets classified as `under_threshold` (numdip = 0), `need_treatment` (numdip > 0), or `treated` (matched to a treatment record by sitecode + inspection_year). Sites without a prior inspection are excluded—treatment-only sites are intentionally not shown.
- **Inspection year logic**: `inspection_year` is derived from the treatment/inspection season. Fall/winter (DOY 244-365, Sept 1 – Dec 31) belongs to that calendar year. Spring/summer (DOY 135-213, May 15 – Aug 1) belongs to the **previous** calendar year. Analysis date month determines which inspection year to use: months 1-7 → previous year, months 8-12 → current year.
- **Treatment matching**: `load_cattail_treatments()` returns all cattail treatments; `aggregate_cattail_data()` marks sites as "treated" if their sitecode appears in treatments for the relevant `inspection_year`.
- **Aggregation**: `aggregate_cattail_data()` produces `total_summary`, `facility_summary`, `zone_summary`, and `fos_summary` data frames with counts, acres, and rates for each 3-state category.
- **Filtering pipeline**: `apply_data_filters()` delegates to shared `apply_standard_data_filters()` with `foreman_col = "fosarea"`.
- **Overview registry compatible**: `load_raw_data()` returns `list(sites, treatments, total_count)` with `is_active` / `is_expiring` columns.
- **Historical chart**: `create_historical_analysis_chart()` supports three display metrics (`need_treatment`, `treated`, `pct_treated`), toggleable sites/acres, group-by facility/foreman/zone, with chart types (line, grouped bar, stacked bar, area). Maps facility short codes to full names for display. Uses `create_trend_chart()`.
- **Leaflet map**: `create_cattail_map()` reads PostGIS `geom` from `loc_breeding_sites`, transforms to WGS84, colors markers by treatment status. Validates coordinates and filters invalid rows.
- **Cached data loading**: `app.R` calls `cached_load_raw_data("cattail_treatments", ...)` for caching support.
- **Material code caveat**: A UI warning notes that only treatments with cattail-specific matcodes (e.g., G3) appear; general larvicide matcodes (e.g., G2/matcode 16) used for cattail work will not show.

---

## Shapefiles

None loaded externally. Map uses PostGIS geometry (`loc_breeding_sites.geom`) queried from the database directly, rendered via `sf` + `leaflet`.

---

## Shared Functions Used

| Function | Source |
|----------|--------|
| `get_db_connection()` / `safe_disconnect()` | `shared/db_helpers.R` |
| `get_facility_lookup()` / `get_foremen_lookup()` | `shared/db_helpers.R` |
| `get_facility_choices()` | `shared/db_helpers.R` |
| `map_facility_names()` | `shared/db_helpers.R` |
| `is_valid_filter()` / `build_sql_in_list()` | `shared/db_helpers.R` |
| `make_sitecode_link()` | `shared/db_helpers.R` |
| `apply_standard_data_filters()` | `shared/db_helpers.R` |
| `cached_load_raw_data()` | `shared/db_helpers.R` |
| `get_status_colors()` / `get_facility_base_colors()` / `get_themed_foreman_colors()` | `shared/db_helpers.R` / `shared/color_themes.R` |
| `create_trend_chart()` | `shared/db_helpers.R` |
| `create_stat_box()` | `shared/stat_box_helpers.R` |
| `get_universal_text_css()` | `shared/server_utilities.R` |
| `set_app_name()` | `shared/server_utilities.R` |
