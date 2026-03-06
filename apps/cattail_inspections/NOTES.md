# Cattail Inspections – Developer Notes

## Database Tables & Entities

| Table | Purpose |
|-------|---------|
| `dblarv_insptrt_current` | Current-year inspection/treatment records |
| `dblarv_insptrt_archive` | Historical inspection/treatment records |
| `loc_breeding_sites` | Site master — `sitecode`, `acres`, `enddate` (NULL = active) |
| `gis_sectcode` | Zone/facility lookup — join on `LEFT(sitecode, 7) = sectcode`; provides `zone` ('1'=P1, '2'=P2) and `facility` |
| `cattail_pctcomplete_base` | Inspection goals per facility — `p1_totsitecount`, `p2_totsitecount` |

All queries filter `action = '9'` (cattail inspection action code).

---

## SQL Queries

### 1. Progress Counts by Facility/Zone (`data_functions.R → load_raw_data`)

```sql
WITH valid_sites AS (
  SELECT sitecode 
  FROM public.loc_breeding_sites 
  WHERE (enddate IS NULL OR enddate > :analysis_date)
),
all_inspections AS (
  SELECT a.facility, a.sitecode, a.inspdate, g.zone
  FROM (
    SELECT facility, sitecode, inspdate FROM public.dblarv_insptrt_archive
    WHERE action = '9'
      AND EXTRACT(YEAR FROM inspdate) = :year
      AND inspdate <= :analysis_date
    UNION ALL
    SELECT facility, sitecode, inspdate FROM public.dblarv_insptrt_current
    WHERE action = '9'
      AND EXTRACT(YEAR FROM inspdate) = :year
      AND inspdate <= :analysis_date
  ) a
  LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 7) = g.sectcode
  WHERE a.sitecode IN (SELECT sitecode FROM valid_sites)
)
SELECT facility, zone, COUNT(DISTINCT sitecode) AS inspections
FROM all_inspections
GROUP BY facility, zone
ORDER BY facility, zone
```

### 2. Goals Lookup (`data_functions.R → load_raw_data`)

```sql
SELECT facility, p1_totsitecount, p2_totsitecount
FROM public.cattail_pctcomplete_base
```

### 3. Historical Treatments (daily counts) (`data_functions.R → load_historical_treatments`)

Same CTE structure as #1 but uses `BETWEEN :start_year AND :end_year` on year and groups by `facility, zone, inspdate`.

### 4. Progress Site Detail (`progress_functions.R → get_progress_sites_detail`)

```sql
WITH valid_sites AS (
  SELECT sitecode FROM public.loc_breeding_sites
  WHERE (enddate IS NULL OR enddate > :custom_today)
),
all_inspections AS (
  SELECT a.facility, a.sitecode, a.inspdate, a.wet, a.numdip, g.zone,
         ROW_NUMBER() OVER (PARTITION BY a.sitecode ORDER BY a.inspdate DESC) as rn
  FROM (
    SELECT facility, sitecode, inspdate, wet, numdip FROM public.dblarv_insptrt_archive
    WHERE action = '9' AND EXTRACT(YEAR FROM inspdate) = :year AND inspdate <= :custom_today
    UNION ALL
    SELECT facility, sitecode, inspdate, wet, numdip FROM public.dblarv_insptrt_current
    WHERE action = '9' AND EXTRACT(YEAR FROM inspdate) = :year AND inspdate <= :custom_today
  ) a
  LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, 7) = g.sectcode
  WHERE a.sitecode IN (SELECT sitecode FROM valid_sites)
)
SELECT ai.facility, ai.sitecode, ai.inspdate, ai.wet, ai.numdip, ai.zone,
       COALESCE(b.acres, 0) as acres
FROM all_inspections ai
LEFT JOIN public.loc_breeding_sites b ON ai.sitecode = b.sitecode
WHERE ai.rn = 1 AND :zone_filter
ORDER BY ai.facility, ai.zone, ai.inspdate DESC
```

### 5. Historical Comparison – Baseline & Current Year (`historical_functions.R → get_historical_progress_data`)

Two parallel queries with identical structure: one for `EXTRACT(YEAR) >= start_year AND <= end_year` (baseline) and one for `EXTRACT(YEAR) = current_year`. Both use:

- CTE `all_inspections` filtered by action='9', zone condition, facility filter, enddate check, and `(reinspect IS NULL OR reinspect = 'f')`
- Nested subquery with `FIRST_VALUE(COALESCE(acres_plan, acres)) OVER (PARTITION BY facility, sitecode ORDER BY inspdate DESC)` to pick best acres value per site
- Final aggregation: `COUNT(DISTINCT sitecode)::integer as site_count`, `SUM(acres)::numeric as acre_count`
- Dynamic GROUP BY: by `facility` only, or by `facility, zone` when zone option = "separate"

### 6. Sites Table – All or Unchecked (`historical_functions.R → get_sites_table_data`)

**"All" mode**: `ROW_NUMBER() OVER (PARTITION BY sitecode, facility ORDER BY inspdate DESC)` with `rn = 1` across historical year range. Returns most recent inspection per site.

**"Unchecked" mode**: Four CTEs:
1. `historical_sites` — DISTINCT sitecodes from archive+current in historical year range
2. `current_year_sites` — DISTINCT sitecodes inspected in current year
3. `unchecked_sites` — LEFT JOIN anti-pattern to find sites in #1 but not in #2
4. `ranked_inspections` — most recent inspection for unchecked sites only

### 7. Treatment Planning (COMMENTED OUT in app.R) (`planned_treatment_functions.R`)

Queries `dblarv_insptrt_current` for `action='9'` with non-null `acres_plan`, grouped by `airgrnd_plan` (A/D/G/N/U) and `facility`. This entire tab is disabled — code exists but is not wired into the UI.

---

## R-Side Logic

- **Overview registry compatible**: `load_raw_data()` returns `list(sites, treatments, total_count, goal_count, pre_aggregated = TRUE)` so the overview app can call it directly.
- **Zone option mapping**: UI selections ("total", "p1", "p2", "separate") are mapped to `zone_filter` vectors and SQL `WHERE` clauses dynamically.
- **Facility display names**: `map_facility_names()` converts short codes to full names for chart labels.
- **Progress value boxes**: Color-coded by completion percentage — green (≥100%), yellow (≥75%), orange (≥50%), red (<50%). In "separate" mode, each box shows `P1: x% | P2: y%`.
- **Sitecode links**: `make_sitecode_link()` creates clickable links to `data.mmcd.org` map in DT tables.
- **Historical comparison metric**: User toggles between "sites" (`COUNT DISTINCT sitecode`) and "acres" (`SUM` of `COALESCE(acres_plan, acres)`) — both use same queries, just different output columns.
- **Chart rendering**: Progress tab uses `ggplotly()` directly; historical tab delegates to shared `create_trend_chart()` with `chart_type = "grouped_bar"`.
- **Lookup preloading**: `get_facility_lookup()` and `get_foremen_lookup()` are called at startup outside the server function to cache lookups.

---

## Shapefiles

None. This app does not load or display any spatial data.

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
| `export_csv_safe()` | `shared/db_helpers.R` |
| `get_status_colors()` / `get_treatment_plan_colors()` | `shared/db_helpers.R` / `shared/color_themes.R` |
| `create_trend_chart()` | `shared/db_helpers.R` |
| `get_universal_text_css()` | `shared/server_utilities.R` |
| `set_app_name()` | `shared/server_utilities.R` |
