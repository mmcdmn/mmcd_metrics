# Catch Basin Status — Developer Notes

## Database Tables & Entities

| Table | Purpose |
|-------|---------|
| `loc_catchbasin` | Catch basin registry. Key columns: `gid` (PK), `sitecode`, `facility`, `status_udw` (`'W'`=wet, `'D'`=dry, `'U'`=unknown), `lettergrp` (`'Z'`=excluded), `enddate` (NULL=active). |
| `gis_sectcode` | Section → facility / zone / fosarea mapping. Joined via `LEFT(sitecode, 7) = sectcode`. |
| `gis_facility` | Facility list with `abbrv` short codes. Used in CROSS JOIN to build the outer scaffold of all facility/section combos. |
| `dblarv_insptrt_current` | Current-year inspection/treatment records. Joined to catch basins via `dblarv_treatment_catchbasin`. Key columns: `pkey_pg`, `sitecode`, `inspdate`, `insptime`, `matcode`. |
| `dblarv_insptrt_archive` | Archive inspection/treatment records. Same schema as current. |
| `dblarv_treatment_catchbasin` | Links treatments to individual catch basins (current year). Columns: `treatment_id` → `pkey_pg`, `catchbasin_id` → `loc_catchbasin.gid`, `status` (`'T'`=treated, `'S'`=skipped, `'P'`=planned). |
| `dblarv_treatment_cb_archive` | Archive version of the catch-basin treatment linkage table. |
| `mattype_list_targetdose` | Material metadata. `effect_days` (default 150 for CB), `days_retrt_early`. Joined via `matcode`. |
| `employee_list` | Employee lookup for FOS name mapping. |

## SQL Queries

### Main status query (`load_raw_data()`)

Returns pre-aggregated counts per section. The outer scaffold ensures every facility/section combination appears even with zero counts.

```sql
SELECT o.facility, o.zone, o.fosarea, o.sectcode,
       COALESCE(wet.total_count, 0)::integer  AS total_count,
       COALESCE(t.active_count, 0)::integer   AS active_count,
       COALESCE(t.expiring_count, 0)::integer AS expiring_count,
       COALESCE(t.expired_count, 0)::integer  AS expired_count

FROM (
  -- Scaffold: every facility × section combination
  SELECT DISTINCT g.abbrv AS facility, sc.zone, sc.fosarea, sc.sectcode
  FROM gis_facility g
  CROSS JOIN gis_sectcode sc
  WHERE g.abbrv IN (:facility_list)
    -- optional: AND sc.fosarea IN (:foreman_filter)
    -- optional: AND sc.zone IN (:zone_filter)
) o

LEFT JOIN (
  -- Wet catch basin counts per section
  SELECT facility, LEFT(sitecode, 7) AS sectcode, COUNT(*) AS total_count
  FROM loc_catchbasin
  LEFT JOIN gis_sectcode sc ON LEFT(sitecode, 7) = sc.sectcode
  WHERE (enddate IS NULL OR enddate > :analysis_date)
    AND lettergrp <> 'Z' AND status_udw = 'W'
  GROUP BY facility, LEFT(sitecode, 7)
) wet ON wet.facility = o.facility AND wet.sectcode = o.sectcode

LEFT JOIN (
  -- Treatment status counts per section (inner DISTINCT ON gets latest treatment per basin)
  SELECT cbstat.facility, cbstat.sectcode,
         COUNT(*) FILTER (WHERE active_status IN ('treated','expiring')) AS active_count,
         COUNT(*) FILTER (WHERE active_status = 'expiring')             AS expiring_count,
         COUNT(*) FILTER (WHERE active_status = 'expired')              AS expired_count
  FROM (
    SELECT s_tcb.gid, s_tcb.facility, s_tcb.sectcode, s_tcb.status_udw,
      CASE
        WHEN age > COALESCE(effect_days, 150) THEN 'expired'
        WHEN age > (COALESCE(effect_days, 150) - :expiring_days) THEN
          CASE WHEN status = 'S' THEN 'skipped-expiring' ELSE 'expiring' END
        WHEN status = 'S' THEN 'skipped'
        WHEN status = 'P' THEN 'planned'
        ELSE 'treated'
      END AS active_status
    FROM (
      SELECT DISTINCT ON (loc_catchbasin.gid)
        loc_catchbasin.gid, loc_catchbasin.facility,
        LEFT(loc_catchbasin.sitecode, 7) AS sectcode,
        loc_catchbasin.status_udw, loc_catchbasin.lettergrp, loc_catchbasin.enddate,
        insptrt.pkey_pg AS insptrt_id,
        DATE_PART('days', :analysis_date::timestamp - insptrt.inspdate::timestamp) AS age,
        dblarv_treatment_catchbasin.status,
        mattype_list_targetdose.effect_days
      FROM <insptrt_table> insptrt
      JOIN dblarv_treatment_catchbasin ON insptrt.pkey_pg = dblarv_treatment_catchbasin.treatment_id
      JOIN loc_catchbasin ON dblarv_treatment_catchbasin.catchbasin_id = loc_catchbasin.gid
      JOIN mattype_list_targetdose USING (matcode)
      LEFT JOIN gis_sectcode sc ON LEFT(loc_catchbasin.sitecode, 7) = sc.sectcode
      WHERE insptrt.inspdate <= :analysis_date
      ORDER BY loc_catchbasin.gid, insptrt.inspdate DESC, insptrt.insptime DESC
    ) s_tcb
  ) cbstat
  WHERE cbstat.status_udw = 'W'
  GROUP BY cbstat.facility, cbstat.sectcode
) t ON t.facility = o.facility AND t.sectcode = o.sectcode

ORDER BY o.facility, o.zone, o.fosarea, o.sectcode
```

`<insptrt_table>` is `dblarv_insptrt_current` or chosen by `get_table_strategy(analysis_date)`.

### Historical yearly query (`load_historical_cb_data()`)

```sql
SELECT loc_catchbasin.facility, sc.zone, sc.fosarea,
       LEFT(loc_catchbasin.sitecode, 7) AS sectcode,
       EXTRACT(YEAR FROM insptrt.inspdate) AS treatment_year,
       COUNT(*) AS total_count,
       COUNT(*) FILTER (WHERE tcb.status IN ('A','W')) AS active_count,
       ...
FROM <insptrt_table> insptrt
JOIN <treatment_cb_table> tcb ON insptrt.pkey_pg = tcb.treatment_id
JOIN loc_catchbasin ON tcb.catchbasin_id = loc_catchbasin.gid
JOIN mattype_list_targetdose USING (matcode)
LEFT JOIN gis_sectcode sc ON LEFT(loc_catchbasin.sitecode, 7) = sc.sectcode
WHERE EXTRACT(YEAR FROM insptrt.inspdate) BETWEEN :start_year AND :end_year
  AND loc_catchbasin.status_udw = 'W' AND loc_catchbasin.lettergrp <> 'Z'
GROUP BY facility, zone, fosarea, sectcode, treatment_year
```

Runs against both `_current` and `_archive` tables; results are `bind_rows()`'d.

### Historical raw treatments (`load_historical_treatments()`)

Returns individual treatment rows for weekly active-treatment analysis:

```sql
SELECT trt.inspdate, loc.facility, sc.zone, sc.fosarea,
       loc.gid AS catchbasin_id, COALESCE(mat.effect_days, 28) AS effect_days
FROM <insptrt_table> trt
JOIN <treatment_cb_table> tcb ON trt.pkey_pg = tcb.treatment_id
JOIN loc_catchbasin loc ON tcb.catchbasin_id = loc.gid
JOIN mattype_list_targetdose mat USING (matcode)
LEFT JOIN gis_sectcode sc ON LEFT(loc.sitecode, 7) = sc.sectcode
WHERE EXTRACT(YEAR FROM trt.inspdate) BETWEEN :start AND :end
  AND loc.status_udw = 'W' AND loc.lettergrp <> 'Z'
```

Archive query also filters `trt.action = '6'`.

## R-Side Logic

- **Pre-aggregated output**: The main query returns counts per section (not per-basin rows) because there are 50K+ catch basins. A `pre_aggregated = TRUE` flag tells downstream code to sum counts rather than count rows.
- **Treatment status classification**: Age-based status in SQL: `treated` → `expiring` → `expired`, with `skipped`/`planned` overrides based on treatment `status` column.
- **Effect days default**: 150 days for catch basin materials (vs 14 for air sites).
- **Weekly active treatment chart**: For each Friday in the range, R filters to treatments where `inspdate <= friday AND inspdate + effect_days >= friday`, then counts distinct `catchbasin_id` values.
- **Grouping**: Data can be grouped by MMCD-wide, facility, FOS, or section, with optional zone separation (P1/P2 side-by-side or combined).
- **Coverage %**: `(active_count / total_count) * 100` — color-coded green ≥75%, orange 50-74%, red <50%.

## Shapefiles

None.

## Shared Functions Used

| Function | Source |
|----------|--------|
| `get_db_connection()` / `safe_disconnect()` | `shared/db_helpers.R` |
| `is_valid_filter()` / `build_sql_in_list()` | `shared/db_helpers.R` |
| `build_sql_in_clause()` / `build_sql_equals_clause()` | `shared/db_helpers.R` |
| `get_table_strategy()` | `shared/db_helpers.R` |
| `get_historical_year_ranges()` | `shared/db_helpers.R` |
| `get_facility_lookup()` / `get_foremen_lookup()` | `shared/db_helpers.R` |
| `get_facility_choices()` / `get_foreman_choices()` | `shared/db_helpers.R` |
| `apply_standard_data_filters()` | `shared/db_helpers.R` |
| `create_stat_box()` / `create_status_stat_box()` | `shared/stat_box_helpers.R` |
| `set_app_name()` / `load_env_vars()` | `shared/server_utilities.R` |
| `get_universal_text_css()` | `shared/server_utilities.R` |
