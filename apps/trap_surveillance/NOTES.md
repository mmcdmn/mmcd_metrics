# Trap Surveillance App – Technical Notes

## Database Tables & Entities

| Table | Key Columns Used |
|---|---|
| `dbadult_mon_nt_co2_forvectorabundance` | `viarea`, `loc_code`, `inspdate`, `year`, `yrwk`, `epiweek`, `spp_name`, `mosqcount` |
| `dbvirus_mle_yrwk_area` | `mle_id`, `yrwk`, `viarea`, `p` (→ `mle`), `lower` (→ `mle_lower`), `upper` (→ `mle_upper`) |
| `dbvirus_mle_yrwk_area_spp` | `mle_id`, `yrwk`, `viarea`, `spp_code`, `p` (→ `mle`), `lower`, `upper` |
| `dbvirus_mle_yrwk` | `mle_id`, `yrwk`, `p` (→ `mle`), `lower`, `upper` |
| `dbvirus_mir_yrwk_area` | `mir_id`, `year`, `yrwk`, `viarea`, `positive`, `total` (→ `total_pools`), `mosquitoes` (→ `total_mosquitoes`), `mir` |
| `dbvirus_mir_yrwk_area_spp` | `mir_id`, `year`, `yrwk`, `viarea`, `spp_code`, `positive`, `total`, `mosquitoes`, `mir` |
| `dbvirus_mir_yrwk` | `mir_id`, `yrwk`, `positive`, `total`, `mosquitoes`, `mir` |
| `loc_vectorindexareas_sections_a` | `viareaa` (→ `viarea`), `geom`, `area` |
| `loc_mondaynight` | `loc_code`, `loc_facility`, `zone`, `virus_test`, `geom` |
| `dbadult_insp_current` | `ainspecnum`, `sampnum_yr`, `loc_code`, `inspdate`, `facility`, `survtype`, `network_type`, `missing` |
| `dbadult_insp_archive` | Same columns as current |
| `dbvirus_pool` | `poolnum`, `sampnum_yr`, `spp_code`, `count` (→ `pool_size`) |
| `dbvirus_pool_test` | `poolnum`, `result`, `target`, `date` (→ `test_date`) |
| `lookup_specieslist` | `sppcode`, `genus`, `species` |
| `lookup_weeknum` | `wknumyr`, `week_days` |

---

## SQL Queries

### 1. Abundance by Year/Week/Species

```sql
SELECT viarea, loc_code, inspdate, year, yrwk, epiweek, spp_name, mosqcount
FROM dbadult_mon_nt_co2_forvectorabundance
WHERE {dynamic clauses: year =, yrwk =, spp_name =}
ORDER BY yrwk DESC, viarea, loc_code
```

### 2. Abundance Aggregated by Area

```sql
SELECT viarea,
       SUM(mosqcount) as total_count,
       COUNT(DISTINCT loc_code) as num_traps,
       CASE WHEN COUNT(DISTINCT loc_code) > 0
            THEN ROUND(SUM(mosqcount)::numeric / COUNT(DISTINCT loc_code), 2)
            ELSE 0 END as avg_per_trap
FROM dbadult_mon_nt_co2_forvectorabundance
WHERE yrwk = {yrwk} AND spp_name = '{spp_name}'
GROUP BY viarea
ORDER BY viarea
```

### 3. MLE by Area (total Cx)

```sql
SELECT mle_id, yrwk, viarea,
       p::numeric as mle, lower::numeric as mle_lower, upper::numeric as mle_upper
FROM dbvirus_mle_yrwk_area
WHERE yrwk = '{yrwk}'
ORDER BY viarea
```

### 4. MLE by Area × Species

```sql
SELECT mle_id, yrwk, viarea, spp_code,
       p::numeric as mle, lower::numeric as mle_lower, upper::numeric as mle_upper
FROM dbvirus_mle_yrwk_area_spp
WHERE yrwk = '{yrwk}' AND spp_code = '{spp_code}'
ORDER BY viarea
```

### 5. MLE District-Wide Trend

```sql
SELECT mle_id, yrwk, p::numeric as mle,
       lower::numeric as mle_lower, upper::numeric as mle_upper
FROM dbvirus_mle_yrwk
WHERE yrwk::text LIKE '{year}%'
ORDER BY yrwk
```

### 6. MLE Multi-Year Average by Epiweek

```sql
SELECT (yrwk::integer % 100) AS week,
       AVG(p::numeric) AS avg_mle
FROM dbvirus_mle_yrwk
WHERE (yrwk::integer / 100) BETWEEN {start_year} AND {end_year}
GROUP BY (yrwk::integer % 100)
ORDER BY week
```

### 7. Abundance Multi-Year Average by Epiweek

```sql
SELECT epiweek AS week,
       SUM(mosqcount)::numeric / GREATEST(COUNT(DISTINCT loc_code), 1) AS avg_per_trap
FROM dbadult_mon_nt_co2_forvectorabundance
WHERE year BETWEEN {start_year} AND {end_year}
  AND spp_name = '{spp_name}'
GROUP BY epiweek
ORDER BY epiweek
```

### 8. MIR by Area (total Cx)

```sql
SELECT mir_id, year, yrwk, viarea,
       positive::integer as positive, total::integer as total_pools,
       mosquitoes::integer as total_mosquitoes, mir::numeric as mir
FROM dbvirus_mir_yrwk_area
WHERE yrwk = '{yrwk}'
ORDER BY viarea
```

### 9. MIR by Area × Species

```sql
SELECT mir_id, year, yrwk, viarea, spp_code,
       positive::integer as positive, total::integer as total_pools,
       mosquitoes::integer as total_mosquitoes, mir::numeric as mir
FROM dbvirus_mir_yrwk_area_spp
WHERE yrwk = '{yrwk}' AND spp_code = '{spp_code}'
ORDER BY viarea
```

### 10. MIR District-Wide Trend

```sql
SELECT mir_id, yrwk,
       positive::integer as positive, total::integer as total_pools,
       mosquitoes::integer as total_mosquitoes, mir::numeric as mir
FROM dbvirus_mir_yrwk
WHERE yrwk::text LIKE '{year}%'
ORDER BY yrwk
```

### 11. MIR Multi-Year Average by Epiweek

```sql
SELECT (yrwk::integer % 100) AS week,
       AVG(mir::numeric) AS avg_mir
FROM dbvirus_mir_yrwk
WHERE (yrwk::integer / 100) BETWEEN {start_year} AND {end_year}
GROUP BY (yrwk::integer % 100)
ORDER BY week
```

### 12. Traps for Week (large CTE)

```sql
WITH week_abundance AS (
  SELECT loc_code, inspdate, mosqcount
  FROM dbadult_mon_nt_co2_forvectorabundance
  WHERE yrwk = {yrwk_int} AND spp_name = 'Total_Cx_vectors'
),
week_inspections AS (
  SELECT DISTINCT i.ainspecnum, i.sampnum_yr, i.loc_code, i.inspdate,
         i.facility, i.survtype
  FROM dbadult_insp_current i
  WHERE i.network_type = 'mnt' AND i.survtype = '6' AND i.missing IS NULL
    AND calc_week_num(i.inspdate) = {yrwk_int}
  UNION ALL
  SELECT DISTINCT i.ainspecnum, i.sampnum_yr, i.loc_code, i.inspdate,
         i.facility, i.survtype
  FROM dbadult_insp_archive i
  WHERE i.network_type = 'mnt' AND i.survtype = '6' AND i.missing IS NULL
    AND calc_week_num(i.inspdate) = {yrwk_int}
),
trap_pools AS (
  SELECT wi.loc_code, wi.sampnum_yr, p.poolnum, p.spp_code,
         p.count as pool_size, t.result, t.target, t.date as test_date,
         ls.genus, ls.species
  FROM week_inspections wi
  JOIN dbvirus_pool p ON wi.sampnum_yr = p.sampnum_yr
  LEFT JOIN dbvirus_pool_test t ON p.poolnum = t.poolnum
  LEFT JOIN lookup_specieslist ls ON p.spp_code = CAST(ls.sppcode AS VARCHAR)
  WHERE t.target = '{virus_target}' OR t.target IS NULL
)
SELECT wa.loc_code, wi.facility, wa.inspdate,
       wa.mosqcount as cx_vector_count,
       mn.loc_facility, mn.zone, mn.virus_test,
       ST_X(ST_Transform(mn.geom, 4326)) as lon,
       ST_Y(ST_Transform(mn.geom, 4326)) as lat,
       tp.poolnum, tp.spp_code, tp.pool_size, tp.result,
       tp.target, tp.test_date, tp.genus, tp.species
FROM week_abundance wa
JOIN week_inspections wi ON wi.loc_code = wa.loc_code AND wi.inspdate = wa.inspdate
LEFT JOIN loc_mondaynight mn ON mn.loc_code = wa.loc_code
LEFT JOIN trap_pools tp ON tp.loc_code = wa.loc_code
WHERE mn.geom IS NOT NULL
ORDER BY wa.loc_code
```

> Uses server-side DB function `calc_week_num(inspdate)` to match inspections to yrwk.

### 13. VI Area Geometries (dissolved)

```sql
SELECT viareaa as viarea,
       ST_Transform(ST_Union(geom), 4326) as geom,
       COUNT(*) as num_sections,
       SUM(area) as total_area
FROM loc_vectorindexareas_sections_a
GROUP BY viareaa
ORDER BY viareaa
```

### 14. Vector Index Area Trend (CTE)

```sql
WITH abundance AS (
  SELECT viarea, yrwk,
         SUM(mosqcount) as total_count,
         COUNT(DISTINCT loc_code) as num_traps,
         CASE WHEN COUNT(DISTINCT loc_code) > 0
              THEN SUM(mosqcount)::numeric / COUNT(DISTINCT loc_code) ELSE 0 END as avg_per_trap
  FROM dbadult_mon_nt_co2_forvectorabundance
  WHERE year = {year} AND spp_name = '{spp_name}'
  GROUP BY viarea, yrwk
),
infection AS (
  SELECT viarea, yrwk,
         {infection_col} as infection_rate
  FROM {infection_table} inf
  WHERE yrwk::text LIKE '{year}%' {spp_filter}
)
SELECT a.viarea, a.yrwk, a.avg_per_trap,
       COALESCE(i.infection_rate, 0) as infection_rate,
       a.avg_per_trap * COALESCE(i.infection_rate, 0) as vector_index
FROM abundance a
LEFT JOIN infection i ON a.viarea = i.viarea AND a.yrwk::text = i.yrwk::text
ORDER BY a.viarea, a.yrwk
```

> `infection_table` is `dbvirus_mle_yrwk_area[_spp]` for MLE or `dbvirus_mir_yrwk_area[_spp]` for MIR.

### 15. Available Weeks

```sql
SELECT DISTINCT a.yrwk, a.epiweek, a.year, l.week_days
FROM dbadult_mon_nt_co2_forvectorabundance a
LEFT JOIN lookup_weeknum l ON a.yrwk::text = l.wknumyr::text
WHERE a.year = {year}
ORDER BY a.yrwk DESC
```

### 16. Available Years

```sql
SELECT DISTINCT year
FROM dbadult_mon_nt_co2_forvectorabundance
ORDER BY year DESC
```

### 17. Overview — Latest Week + VI Computation

```sql
-- Find latest yrwk
SELECT MAX(yrwk) as yrwk
FROM dbadult_mon_nt_co2_forvectorabundance
WHERE year = {query_year} AND inspdate <= '{analysis_date}'::date

-- Compute VI per area for that week
WITH abundance AS (
  SELECT viarea, SUM(mosqcount) AS total_count,
         COUNT(DISTINCT loc_code) AS num_traps,
         CASE WHEN COUNT(DISTINCT loc_code) > 0
              THEN SUM(mosqcount)::numeric / COUNT(DISTINCT loc_code) ELSE 0 END AS avg_per_trap
  FROM dbadult_mon_nt_co2_forvectorabundance
  WHERE yrwk = {target_yrwk} AND spp_name = '{spp_name}'
  GROUP BY viarea
),
infection AS (
  SELECT viarea,
         CASE WHEN p IS NOT NULL AND p::text <> 'NULL' AND p::text <> ''
              THEN p::numeric ELSE 0 END AS mle
  FROM dbvirus_mle_yrwk_area
  WHERE yrwk = '{target_yrwk}'
)
SELECT a.viarea, a.avg_per_trap, COALESCE(i.mle, 0) AS mle,
       a.avg_per_trap * COALESCE(i.mle, 0) AS vector_index
FROM abundance a LEFT JOIN infection i ON a.viarea = i.viarea
ORDER BY vector_index DESC NULLS LAST
```

---

## R-Side Logic

### Species Mapping (data_functions.R)

`SPECIES_MAP` maps 5 identifiers to `(code, label)` pairs:
- `"Total_Cx_vectors"` → code `NULL` (uses area-level tables, not species-specific)
- `"Cx_pipiens_33"` → `"33"`, `"Cx_restuans_34"` → `"34"`, `"Cx_tarsalis_36"` → `"36"`, `"Cx_restuans/pipiens_372"` → `"372"`

When code is `NULL`, queries hit `dbvirus_mle_yrwk_area`; otherwise `dbvirus_mle_yrwk_area_spp`.

### MIR SE Computation (R-side)

```r
p_hat = positive / total_mosquitoes
mir_se = sqrt(p_hat * (1 - p_hat) / total_mosquitoes) * 1000
```

MLE SE derived from CI: `SE = (upper - lower) / (2 × 1.96)`.

### Trap Aggregation (data_functions.R)

Raw join returns multiple rows per trap (one per pool). Aggregated via `group_by(loc_code)`:
- `num_pools = sum(!is.na(poolnum))`
- `num_positive = sum(result == "Pos")`
- `pool_details_html` = HTML table with pool#, genus+species, size, result (color-coded red/green)
- Converted to `sf` via `st_as_sf(coords = c("lon", "lat"), crs = 4326)`

### Map Choropleth Breaks (display_functions.R)

Non-linear fixed breaks by metric:
- **Abundance**: `[0, 1, 3, 7, 12, 20, 30]`
- **MLE infection**: `[0, 0.001, 0.005, 0.01, 0.02, 0.04, 0.06]`
- **MIR infection**: `[0, 2, 5, 15, 30, 60, 100]`
- **Vector Index**: `[0, 0.02, 0.08, 0.2, 0.5, 1.0, 2.0]`

Palette: 7-step yellow-to-red (`colorBin`). Values beyond max break are clamped.

---

## Shapefiles

| File | Location | Purpose |
|---|---|---|
| `recent_trap_locations.shp` | `shared/Q_to_R/data/` | All trap locations as fallback when no week selected. Fields: `survtype`, `facility`, `inspdate`. |
| `VectorIndexAreasA2025.shp` | `shared/Q_to_R/data/` | Fallback if DB geometry load fails. Dissolved by `VIareaA` column. |
| `facility_boundaries.shp` | `shared/Q_to_R/data/` | Background facility boundary layer |
| `zone_boundaries.shp` | `shared/Q_to_R/data/` | Background zone boundary layer |
| `Counties_4326.shp` | `apps/mosquito_surveillance_map/shp/` | Background county boundaries |

PostGIS geometry is also used:
- `loc_vectorindexareas_sections_a.geom` — dissolved via `ST_Union` → VI area polygons
- `loc_mondaynight.geom` — trap point locations extracted as lon/lat via `ST_Transform`
- `calc_week_num()` — server-side DB function matching inspections to yrwk

---

## Shared Functions Used

| Function | Source File |
|---|---|
| `get_db_connection()` | `shared/db_helpers.R` |
| `safe_disconnect()` | `shared/db_helpers.R` |
| `get_theme_palette()` | `shared/color_themes.R` |
| `get_universal_text_css()` | `shared/server_utilities.R` |
| `export_csv_safe()` | `shared/server_utilities.R` |
