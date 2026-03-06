# SUCO History App – Technical Notes

## Database Tables & Entities

| Table | Key Columns Used |
|---|---|
| `dbadult_insp_current` | `id`, `ainspecnum`, `facility`, `foreman`, `inspdate`, `sitecode`, `address1`, `park_name`, `survtype`, `fieldcount`, `comments`, `x`, `y`, `geometry` |
| `dbadult_insp_archive` | Same columns as current |
| `loc_harborage` | `sitecode`, `facility`, `foreman`, `startdate`, `enddate` |
| `gis_sectcode` | `sectcode`, `facility`, `fosarea`, `zone` |
| `dbadult_species_current` | `ainspecnum`, `spp`, `cnt` |
| `dbadult_species_archive` | `ainspecnum`, `spp`, `cnt` |

Additional lookup tables accessed via shared helpers:
- Species lookup via `get_species_lookup()` — `sppcode`, `genus`, `species`
- Facility lookup via `get_facility_lookup()` — `short_name`, `full_name`
- Foremen lookup via `get_foremen_lookup()` — `emp_num`, `shortname`, `facility`

---

## SQL Queries

### 1. Main SUCO Inspections — Current Table

```sql
SELECT DISTINCT ON (s.id)
  s.id, s.ainspecnum,
  COALESCE(g.facility, h.facility, s.facility) as facility,
  COALESCE(g.fosarea, h.foreman, s.foreman) as foreman,
  s.inspdate, s.sitecode,
  s.address1, s.park_name, s.survtype, s.fieldcount, s.comments,
  COALESCE(s.x, ST_X(ST_Transform(s.geometry, 4326))) as x,
  COALESCE(s.y, ST_Y(ST_Transform(s.geometry, 4326))) as y,
  ST_AsText(s.geometry) as geometry_text,
  COALESCE(g.zone,
    CASE WHEN h.sitecode LIKE '%%P1%%' THEN '1'
         WHEN h.sitecode LIKE '%%P2%%' THEN '2'
         ELSE NULL END) as zone,
  'current' as source_table
FROM public.dbadult_insp_current s
LEFT JOIN public.loc_harborage h ON s.sitecode = h.sitecode
  AND s.inspdate >= h.startdate
  AND (h.enddate IS NULL OR s.inspdate <= h.enddate)
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(s.sitecode, 7)
WHERE s.survtype = '7'
  AND s.inspdate BETWEEN '%s' AND '%s'
ORDER BY s.id, h.startdate DESC NULLS LAST
```

> `survtype = '7'` identifies SUCO inspections. Facility/foreman/zone are resolved via COALESCE chain: `gis_sectcode` → `loc_harborage` → inspection record itself.

### 2. Main SUCO Inspections — Archive Table

Identical structure to current query, from `public.dbadult_insp_archive`, with `'archive'` as `source_table`.

### 3. Species Data

```sql
SELECT ainspecnum, spp, cnt FROM public.dbadult_species_current WHERE ainspecnum IS NOT NULL
```

```sql
SELECT ainspecnum, spp, cnt FROM public.dbadult_species_archive WHERE ainspecnum IS NOT NULL
```

### 4. Table Strategy

Uses `get_historical_year_ranges(con, "public.dbadult_insp_current", "public.dbadult_insp_archive", "inspdate")` to determine which tables have data for the requested date range.

---

## R-Side Logic

### Species Processing (data_functions.R)

- Left-joins inspection data to species tables on `ainspecnum`
- `get_enhanced_species_mapping()` maps numeric species codes to display names
- Per-inspection: top 5 species with counts, remainder grouped as "Others"
- Two return modes: one-row-per-SUCO (concatenated summary) or one-row-per-species (for species charts)

### Hardcoded SUCO-Relevant Species List

Five species (from 2025 analysis, ordered by frequency):
1. *Aedes triseriatus* (401), 2. *Aedes japonicus* (266), 3. *Culex tarsalis* (124), 4. *Culiseta melanura* (5), 5. *Aedes albopictus* (2)

### Date/Time Calculations

- `week_start = floor_date(inspdate, "week", week_start = 1)` — Monday-anchored weeks
- `epi_week = epiweek(inspdate)`, `epi_year = epiyear(inspdate)` — epidemiological week/year
- `location = COALESCE(park_name, address1, sitecode)` — display name priority

### Spatial Data Creation (data_functions.R)

- Converts `x`/`y` to `sf` objects with CRS 4326
- Overlapping points: random jitter ±0.0001° (~11m) per coordinate group
- 10-stage marker sizing based on species count (0 → size 4, 1 → 6, 2-5 → 8, ... 100+ → 22)
- Species grouping mode: one point per species per location with count-based sizing
- Marker opacity: 0.35 for zero-species locations, 0.8 for others; border weight 4 for zero-species, 1.5 for positive

### Target Line Logic (app.R)

- 72 SUCOs/week target — shown only if `max(count) >= 30`
- Average line — `mean()` of non-zero weekly totals

### Map Rendering (display_functions.R)

- Four map modes: MMCD All (single blue), Facility (color palette), Foreman (FOS palette), Species (species-aware with "Multiple species" support)
- Species map: circular offset pattern (0.0003° ≈ 33m) for overlapping per-species points
- Harborage polygons conditionally loaded, hidden by default, shown at zoom ≥ 13
- Background layers: facility boundaries + zone boundaries as overlay groups

---

## Shapefiles

Geometry is sourced from:

1. **PostGIS** — `dbadult_insp_current/archive.geometry` column, extracted via `ST_AsText(s.geometry)` and `ST_X/ST_Y(ST_Transform(s.geometry, 4326))`
2. **Facility/zone shapefiles** (via `load_background_layers()` in `shared/geometry_helpers.R`):
  - `shared/Q_to_R/data/facility_boundaries.shp`
  - `shared/Q_to_R/data/zone_boundaries.shp`
3. **Harborage shapefiles** (via `load_harborage_layers()` in `shared/geometry_helpers.R`, conditional when enabled):
  - Directory scanned: `shared/Q_to_R/data/harborages/`
  - Folder pattern: one subfolder per facility (e.g., `shared/Q_to_R/data/harborages/SJ/`)
  - File pattern inside each facility folder: `*_harborages.shp` (example: `8202_harborages.shp`)
  - Foreman filtering is applied by parsing emp_num from filename prefix (e.g., `8202`)

---

## Shared Functions Used

| Function | Source File |
|---|---|
| `get_db_connection()` | `shared/db_helpers.R` |
| `safe_disconnect()` | `shared/db_helpers.R` |
| `get_historical_year_ranges()` | `shared/db_helpers.R` |
| `get_species_lookup()` | `shared/db_helpers.R` |
| `get_enhanced_species_mapping()` | `shared/db_helpers.R` |
| `get_facility_lookup()` | `shared/db_helpers.R` |
| `get_foremen_lookup()` | `shared/db_helpers.R` |
| `get_facility_choices()` | `shared/db_helpers.R` |
| `is_valid_filter()` | `shared/db_helpers.R` |
| `make_sitecode_link()` | `shared/db_helpers.R` |
| `get_universal_text_css()` | `shared/db_helpers.R` |
| `get_facility_base_colors()` | `shared/color_themes.R` |
| `map_foreman_emp_to_colors()` | `shared/color_themes.R` |
| `get_species_display_colors()` | `shared/color_themes.R` |
| `get_foreman_display_names()` | `shared/db_helpers.R` |
| `get_facility_display_names()` | `shared/db_helpers.R` |
| `set_app_name()` | `shared/server_utilities.R` |
| `export_csv_safe()` | `shared/server_utilities.R` |
| `load_background_layers()` | `shared/geometry_helpers.R` |
| `add_background_layers()` | `shared/geometry_helpers.R` |
| `load_harborage_layers()` | `shared/geometry_helpers.R` |
| `add_harborage_layer()` | `shared/geometry_helpers.R` |
| `calculate_map_bounds()` | `shared/geometry_helpers.R` |
| `apply_map_bounds()` | `shared/geometry_helpers.R` |
