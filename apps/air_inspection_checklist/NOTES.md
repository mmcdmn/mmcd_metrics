# Air Inspection Checklist — Developer Notes

## Database Tables & Entities

| Table | Purpose |
|-------|---------|
| `loc_breeding_sites` | Source of RED air sites (`air_gnd='A'`, `priority='RED'`). Columns: `sitecode`, `acres`, `enddate`, `air_gnd`, `priority`. |
| `gis_sectcode` | Maps sitecodes to facility, zone, fosarea, sectcode via `LEFT(sitecode, 7) = sectcode`. |
| `dblarv_insptrt_current` | Current-year inspections & treatments. Used for inspection lookback (actions `'2','4'`) and recent treatments (actions `'3','A','D'`). |
| `dblarv_insptrt_archive` | Same schema as above; used when `analysis_date` falls in a prior year (via `get_table_strategy()`). |
| `dblarv_sample_current` | Lab sample results. Joined on `sampnum_yr` to get `redblue` (R/B bug type) and `missing` flag. |
| `dblarv_sample_archive` | Archive version of sample table; used when querying prior years. |
| `mattype_list_targetdose` | Material metadata. Joined on `matcode` to get `effect_days` and `prehatch` flag for active-treatment logic. |
| `loc_breeding_site_cards_sjsreast2` | Card/AirMap lookup. Joined on `sitecode` to get `airmap_num` for sub-grouping. |
| `employee_list` | Employee names. Joined on `emp_num` to resolve inspector shortname from `emp1`. |

## SQL Query

The main query is a single CTE-based statement built in `get_checklist_data()`:

```sql
WITH RedAirSites AS (
  SELECT DISTINCT ON (b.sitecode)
    b.sitecode, b.acres,
    sc.facility, sc.zone, sc.fosarea, sc.sectcode
  FROM loc_breeding_sites b
  LEFT JOIN gis_sectcode sc ON LEFT(b.sitecode, 7) = sc.sectcode
  WHERE (b.enddate IS NULL OR b.enddate > :analysis_date)
    AND b.air_gnd = 'A'
    AND b.priority = 'RED'
    AND sc.facility IN (:facility_filter)   -- optional
    AND sc.fosarea  IN (:foreman_filter)    -- optional
    AND sc.zone     IN (:zone_filter)       -- optional
  ORDER BY b.sitecode, b.enddate NULLS LAST
),

RecentInspections AS (
  SELECT i.sitecode, i.inspdate, i.numdip, i.emp1, i.sampnum_yr,
         ROW_NUMBER() OVER (PARTITION BY i.sitecode ORDER BY i.inspdate DESC) AS rn
  FROM <inspection_table> i
  WHERE i.inspdate BETWEEN :lookback_start AND :analysis_date
    AND i.action IN ('2','4')
    AND i.sitecode IN (SELECT sitecode FROM RedAirSites)
),

RecentTreatments AS (
  SELECT t.sitecode, t.inspdate AS last_treatment_date, t.matcode, t.mattype,
         COALESCE(mt.effect_days, 14) AS effect_days,
         t.inspdate + INTERVAL '1 day' * COALESCE(mt.effect_days, 14) AS treatment_expiry,
         COALESCE(mt.prehatch, FALSE) AS is_prehatch,
         ROW_NUMBER() OVER (PARTITION BY t.sitecode ORDER BY t.inspdate DESC) AS rn
  FROM <inspection_table> t
  LEFT JOIN mattype_list_targetdose mt ON t.matcode = mt.matcode
  WHERE t.inspdate <= :analysis_date
    AND t.action IN ('3','A','D')
    AND t.matcode IS NOT NULL AND t.matcode != ''
    AND t.sitecode IN (SELECT sitecode FROM RedAirSites)
),

ActiveTreatmentSites AS (
  SELECT sitecode, mattype AS active_material,
         last_treatment_date AS active_trt_date,
         treatment_expiry AS active_trt_expiry, is_prehatch
  FROM RecentTreatments
  WHERE rn = 1 AND treatment_expiry > :analysis_date
),

LabResults AS (
  SELECT ls.sampnum_yr, ls.redblue, ls.missing
  FROM <sample_table> ls
  WHERE ls.sampnum_yr IS NOT NULL
)

SELECT
  s.*, cards.airmap_num,
  i.inspdate AS last_insp_date, i.numdip AS dip_count,
  i.emp1 AS inspector_emp, emp.shortname AS inspector_name,
  i.sampnum_yr, lr.redblue, lr.missing AS lab_missing,
  CASE WHEN i.sitecode IS NOT NULL THEN TRUE ELSE FALSE END AS was_inspected,
  CASE WHEN ats.sitecode IS NOT NULL THEN TRUE ELSE FALSE END AS has_active_treatment,
  ats.active_material, ats.active_trt_date, ats.active_trt_expiry,
  COALESCE(ats.is_prehatch, FALSE) AS is_prehatch
FROM RedAirSites s
LEFT JOIN RecentInspections i      ON s.sitecode = i.sitecode AND i.rn = 1
LEFT JOIN LabResults lr             ON i.sampnum_yr = lr.sampnum_yr
LEFT JOIN ActiveTreatmentSites ats  ON s.sitecode = ats.sitecode
LEFT JOIN loc_breeding_site_cards_sjsreast2 cards ON s.sitecode = cards.sitecode
LEFT JOIN employee_list emp         ON i.emp1 = emp.emp_num::text
ORDER BY s.fosarea, cards.airmap_num NULLS LAST, s.sectcode, s.sitecode
```

`<inspection_table>` is `dblarv_insptrt_current` or `dblarv_insptrt_archive` depending on `get_table_strategy(analysis_date)`. Same pattern for `<sample_table>`.

## R-Side Logic

- **Table strategy**: `get_table_strategy(analysis_date)` decides whether to query `_current` or `_archive` tables based on the analysis date year.
- **Prehatch filtering**: After the query, sites with an active prehatch treatment are removed unless the user enables the "Show Prehatch Treatment Sites" toggle (`include_active_treatment`).
- **FOS display names**: `fosarea` (emp_num) is mapped to human-readable names via `get_foreman_display_names()`.
- **AirMap grouping**: Sites are grouped first by FOS, then by `airmap_num` (from the cards table), then by `sectcode`.
- **Bug status derivation**: Each site is classified as `Red Bugs`, `Blue Bugs`, `Pending Lab`, `No Bugs`, or `No Sample` based on `redblue` and `missing` columns from the lab results.
- **Inspector display**: Falls back from `employee_list.shortname` → `emp1` number → blank.
- **JSON generation at startup**: Writes `www/facilities.json` and `www/foremen.json` for use by the root `index.html` landing page filters.

## Shapefiles

None.

## Shared Functions Used

| Function | Source |
|----------|--------|
| `get_db_connection()` | `shared/db_helpers.R` |
| `safe_disconnect()` | `shared/db_helpers.R` |
| `is_valid_filter()` | `shared/db_helpers.R` |
| `build_sql_in_list()` | `shared/db_helpers.R` |
| `get_table_strategy()` | `shared/db_helpers.R` |
| `get_facility_lookup()` | `shared/db_helpers.R` |
| `get_foremen_lookup()` | `shared/db_helpers.R` |
| `get_facility_choices()` | `shared/db_helpers.R` |
| `get_foreman_choices()` | `shared/db_helpers.R` |
| `get_foreman_display_names()` | `shared/db_helpers.R` |
| `make_sitecode_link()` | `shared/db_helpers.R` |
| `set_app_name()` | `shared/server_utilities.R` |
| `load_env_vars()` | `shared/server_utilities.R` |
| `get_universal_text_css()` | `shared/server_utilities.R` |
