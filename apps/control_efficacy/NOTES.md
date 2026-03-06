# Control Efficacy – Developer Notes

## Database Tables & Entities

| Table | Purpose |
|-------|---------|
| `dblarv_insptrt_current` | Current-year inspection/treatment records. Air treatments: `action = 'A'`. Checkbacks: `action = '4'` with `posttrt_p IS NOT NULL`. Other inspections: `action IN ('1', '2', '4')`. |
| `dblarv_insptrt_archive` | Historical inspection/treatment records (same schema). |
| `gis_sectcode` | Zone/facility lookup — join `LEFT(sitecode, 7) = sectcode`. Provides `facility`, `zone`. |
| `mattype_list_targetdose` | Material metadata — `matcode`, `mattype`, `effect_days`. Join on `matcode`. |
| `mattype_list` | Material master — `active_ingredient`, `physinv_list`. Join on `mattype`. Used for Bti matcode filtering and dosage lookups. |
| `dblarv_species_current` | Species composition per sample — `sampnum_yr`, `spp` (VARCHAR), `per` (percentage). |
| `dblarv_species_archive` | Historical species composition (same schema). |
| `dblarv_sample_current` | Sample metadata — `sampnum_yr`, `redblue` ('R'/'B'), `missing` (boolean). |
| `dblarv_sample_archive` | Historical sample metadata. |
| `lookup_specieslist` | Species reference — `sppcode` (INTEGER), `genus`, `species`. **Critical**: join requires `spp = CAST(sppcode AS VARCHAR)` type conversion. |

---

## SQL Queries

### 1. Air Treatments (`data_functions.R → load_treatment_data`)

Dynamically builds UNION ALL of current/archive based on date range. Only queries tables whose year range overlaps:

```sql
SELECT insp.inspdate, gis.facility, gis.zone, insp.sitecode, insp.action,
       insp.numdip, insp.diphabitat, insp.acres, insp.matcode,
       insp.sampnum_yr, insp.presamp_yr,
       mat.mattype, mat.effect_days, insp.pkey_pg, insp.insptime
FROM dblarv_insptrt_current insp
LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(insp.sitecode, 7)
LEFT JOIN mattype_list_targetdose mat ON insp.matcode = mat.matcode
WHERE insp.action = 'A'
  AND insp.inspdate >= :start_date AND insp.inspdate <= :end_date
-- UNION ALL with archive when start_year < current_year
```

### 2. Checkback Inspections (`data_functions.R → load_checkback_data`)

Queries treated sitecodes for `action = '4'` with `posttrt_p IS NOT NULL`:

```sql
SELECT insp.inspdate, insp.insptime, gis.facility, gis.zone, insp.sitecode,
       insp.action, insp.numdip, insp.diphabitat, insp.posttrt_p,
       insp.sampnum_yr, insp.pkey_pg
FROM dblarv_insptrt_current insp
LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(insp.sitecode, 7)
WHERE insp.sitecode IN (:treated_sites)
  AND insp.action = '4'
  AND insp.posttrt_p IS NOT NULL
  AND insp.inspdate >= :start_date AND insp.inspdate <= :end_date
-- UNION ALL with archive when needed
```

### 3. Species Composition (`data_functions.R → load_species_data_for_samples`)

```sql
SELECT spp.sampnum_yr, spp.spp, spp.per as count,
       spec.genus, spec.species,
       samp.redblue, samp.missing
FROM dblarv_species_current spp
LEFT JOIN lookup_specieslist spec ON spp.spp = CAST(spec.sppcode AS VARCHAR)
LEFT JOIN dblarv_sample_current samp ON spp.sampnum_yr = samp.sampnum_yr
WHERE spp.sampnum_yr IN (:sample_ids)
-- UNION ALL with archive tables
```

### 4. Site Inspections (`data_functions.R → load_site_inspections`)

Gets actions `'4', '2', '1'` (checkbacks + regular inspections) at treated sites for control-checkback detection:

```sql
SELECT insp.inspdate, insp.insptime,
       COALESCE(gis.facility, insp.facility) AS facility,
       insp.sitecode, insp.action, insp.numdip,
       insp.sampnum_yr, insp.posttrt_p, insp.pkey_pg
FROM dblarv_insptrt_current insp
LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(insp.sitecode, 7)
WHERE insp.sitecode IN (:treated_sites)
  AND insp.action IN ('4', '2', '1')
  AND insp.inspdate >= :start_date AND insp.inspdate <= :end_date
```

### 5. Efficacy Post-Treatment Checks (`efficacy_data_functions.R → load_efficacy_data`)

**Step 1a** — bulk-fetch all post-treatment checks:

```sql
SELECT pkey_pg, inspdate AS post_date, insptime AS post_time,
       sitecode, numdip AS post_numdip, sampnum_yr AS post_sampnum_yr,
       action AS post_action,
       COALESCE(gis.facility, insp.facility) AS facility, insp.acres
FROM dblarv_insptrt_current insp
LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(insp.sitecode, 7)
WHERE insp.posttrt_p IS NOT NULL
  AND insp.inspdate >= :start_date AND insp.inspdate <= :end_date
-- UNION ALL with archive
```

**Step 1b** — get ALL records for those sites (wider window, 90 days earlier):

```sql
SELECT insp.pkey_pg, insp.inspdate, insp.insptime, insp.sitecode,
       insp.numdip, insp.sampnum_yr, insp.presamp_yr, insp.action,
       insp.matcode, insp.mattype, insp.posttrt_p,
       COALESCE(gis.facility, insp.facility) AS facility, insp.acres,
       COALESCE(mt.effect_days, 14) AS effect_days
FROM dblarv_insptrt_current insp
LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(insp.sitecode, 7)
LEFT JOIN mattype_list_targetdose mt ON insp.matcode = mt.matcode
WHERE insp.sitecode IN (:sites) AND insp.inspdate >= :wider_start AND insp.inspdate <= :end_date
-- UNION ALL with archive
```

**Step 3** — genus-level species percentages:

```sql
SELECT sampnum_yr, genus, SUM(per) AS total_pct
FROM (
  SELECT spp.sampnum_yr, spec.genus, spp.per
  FROM dblarv_species_current spp
  JOIN lookup_specieslist spec ON spp.spp = CAST(spec.sppcode AS VARCHAR)
  WHERE spp.sampnum_yr IN (:samples)
    AND spec.genus IN ('Aedes', 'Culex') AND spp.per IS NOT NULL
  UNION ALL
  -- archive
) sub
GROUP BY sampnum_yr, genus
```

### 6. Bti Matcode Lookup (`data_functions.R → get_matcodes_by_ingredient`)

```sql
SELECT DISTINCT t.matcode
FROM mattype_list_targetdose t
JOIN mattype_list m ON t.mattype = m.mattype
WHERE m.active_ingredient ILIKE '%Bti%'
  AND (m.physinv_list = '(1) Larvicide' OR t.mattype = 'Bti_VbacGS')
```

### 7. Dosage Options (`data_functions.R → load_dosage_options`)

```sql
SELECT DISTINCT t.matcode, t.tdose, t.unit, t.area, m.active_ingredient
FROM mattype_list_targetdose t
JOIN mattype_list m ON t.mattype = m.mattype
WHERE m.physinv_list = '(1) Larvicide'
  AND m.active_ingredient ILIKE '%:ingredient%'
ORDER BY m.active_ingredient, t.tdose
```

---

## R-Side Logic

- **Brood (treatment round) calculation** (`checkback_functions.R → calculate_treatment_rounds`): Groups consecutive treatment days (max 1-day gap) per facility into "rounds", each named `facility-MM/DD`. Calculates `checkbacks_needed` as either a percentage of sites or a fixed number.
- **Checkback status** (`checkback_functions.R → calculate_checkback_status`): For each brood, finds valid checkbacks per treated site. A checkback is invalidated if a subsequent treatment occurred before it. Control checkbacks (no treatment between pre-inspection and post-check) are excluded. Only the first valid checkback per site counts.
- **Efficacy pipeline** (`efficacy_data_functions.R → load_efficacy_data`): 5-step process:
  1. Fetch all post-treatment checks  
  2. Fetch all records at those sites in wider window  
  3. **Vectorized matching**: For each post-check, finds the most recent treatment and most recent pre-inspection at the same site using timestamp ordering. Split by sitecode for fast lookup.  
  4. **Bti filter applied AFTER matching** (not before) to avoid silent drops when most recent treatment isn't Bti.  
  5. Compute genus-level dip counts and % reduction for Aedes/Culex separately.
- **% Reduction formulas**: Standard = `(pre - post) / pre * 100`. Optional Mulla's formula: `100 - (T2/T1) × (C1/C2) × 100` where C1/C2 is the control correction factor (natural population change), averaged per season.
- **Control vs Invalid classification**: Control = treatment datetime ≤ pre-inspection datetime (no treatment between pre and post). Invalid = 2+ treatments between pre and post inspections (can't attribute result to single treatment). Air treatments are exempt from invalid rule. Invalid takes priority over control.
- **Season assignment**: Uses `get_spring_date_thresholds()` to classify treatments as Spring or Summer based on year-specific date thresholds.
- **Species formatting**: `load_species_data_for_samples()` aggregates species per sample into strings like `"Ae. vexans (90%), Cu. pipiens (10%)"`. Missing samples flagged as `[Sample Missing]`.
- **Material detail lookup**: After matching, fetches `active_ingredient`, `tdose`, dosage label for matched matcodes.

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
| `make_sitecode_link()` | `shared/db_helpers.R` |
| `get_status_colors()` / `get_facility_base_colors()` | `shared/db_helpers.R` / `shared/color_themes.R` |
| `get_spring_date_thresholds()` | `shared/db_helpers.R` |
| `create_stat_box()` | `shared/stat_box_helpers.R` |
| `get_universal_text_css()` | `shared/server_utilities.R` |
| `set_app_name()` | `shared/server_utilities.R` |
