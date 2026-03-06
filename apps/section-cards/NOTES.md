# Section Cards App – Technical Notes

## Database Tables & Entities

| Table | Key Columns Used |
|---|---|
| `loc_breeding_site_cards_sjsreast2` | `sitecode`, `priority`, `acres`, `type`, `air_gnd`, `culex`, `spr_aedes`, `coq_pert` (→ `perturbans`), `prehatch`, `remarks`, `drone`, `sample`, `facility`, `zone`, `foreman` (→ `fosarea`), `ra`, plus all dynamic columns discovered at runtime |
| `loc_breeding_sites` | `sitecode`, `priority`, `acres`, `type`, `air_gnd`, `culex`, `spr_aedes`, `coq_pert` (→ `perturbans`), `prehatch`, `remarks`, `drone`, `sample`, `enddate` |
| `loc_cxstruct` | `sitecode`, `s_type`, `status_udw`, `sqft`, `priority`, `culex`, `chambers`, `comments` (→ `remarks`), `enddate` |
| `gis_sectcode` | `sectcode`, `facility`, `zone`, `fosarea` |
| `information_schema.columns` | `column_name`, `table_schema`, `table_name`, `ordinal_position` |

---

## SQL Queries

### 1. Dynamic Column Discovery

```sql
SELECT column_name
FROM information_schema.columns
WHERE table_schema = 'public'
  AND table_name = 'loc_breeding_site_cards_sjsreast2'
ORDER BY ordinal_position
# Section Cards App – Technical Notes

## Database Tables & Entities

| Table | Key Columns Used |
|---|---|
| `loc_breeding_site_cards_sjsreast2` | `sitecode`, `priority`, `acres`, `type`, `air_gnd`, `culex`, `spr_aedes`, `coq_pert` (→ `perturbans`), `prehatch`, `remarks`, `drone`, `sample`, `facility`, `zone`, `foreman` (→ `fosarea`), `ra`, plus all dynamic columns discovered at runtime |
| `loc_breeding_sites` | `sitecode`, `priority`, `acres`, `type`, `air_gnd`, `culex`, `spr_aedes`, `coq_pert` (→ `perturbans`), `prehatch`, `remarks`, `drone`, `sample`, `enddate` |
| `loc_cxstruct` | `sitecode`, `s_type`, `status_udw`, `sqft`, `priority`, `culex`, `chambers`, `comments` (→ `remarks`), `enddate` |
| `gis_sectcode` | `sectcode`, `facility`, `zone`, `fosarea` |
| `information_schema.columns` | `column_name`, `table_schema`, `table_name`, `ordinal_position` |

---

## SQL Queries

### 1. Dynamic Column Discovery

```sql
SELECT column_name
FROM information_schema.columns
WHERE table_schema = 'public'
    AND table_name = 'loc_breeding_site_cards_sjsreast2'
ORDER BY ordinal_position
```

### 2. Dynamic Columns With Data Check

```sql
SELECT CASE WHEN COUNT(NULLIF({col}::text, '')) > 0 THEN 1 ELSE 0 END AS {col}, ...
FROM public."loc_breeding_site_cards_sjsreast2"
[WHERE facility = '{facility_filter}' AND foreman = '{fosarea_filter}']
```

### 3. Custom Table Filter Options

```sql
SELECT DISTINCT
    facility,
    left(sitecode, 7) as section,
    foreman as fosarea
FROM public."loc_breeding_site_cards_sjsreast2"
WHERE facility IS NOT NULL AND sitecode IS NOT NULL AND foreman IS NOT NULL
    [AND facility = '{facility_filter}']
    [AND foreman = '{fosarea_filter}']
ORDER BY facility, left(sitecode, 7), foreman
```

### 4. Custom Town Codes

```sql
SELECT DISTINCT left(sitecode, 4) as towncode
FROM public."loc_breeding_site_cards_sjsreast2"
WHERE sitecode IS NOT NULL AND foreman IS NOT NULL
    [AND facility = '{facility_filter}']
    [AND foreman = '{fosarea_filter}']
ORDER BY left(sitecode, 4)
```

### 5. Custom Sections by Town Code

```sql
SELECT DISTINCT left(sitecode, 7) as sectcode
FROM public."loc_breeding_site_cards_sjsreast2"
WHERE sitecode IS NOT NULL AND foreman IS NOT NULL
    [AND left(sitecode, 4) = '{towncode_filter}']
    [AND facility = '{facility_filter}']
    [AND foreman = '{fosarea_filter}']
ORDER BY left(sitecode, 7)
```

### 6. Custom Breeding Sites

```sql
SELECT
    sitecode, priority, acres, type, air_gnd,
    culex, spr_aedes, coq_pert as perturbans,
    prehatch, remarks, drone, sample,
    facility, zone, foreman, foreman as fosarea, ra,
    left(sitecode, 7) as section
    [, {dynamic_col1}, {dynamic_col2}, ...]
FROM public."loc_breeding_site_cards_sjsreast2"
ORDER BY sitecode
```

### 7. Webster Breeding Sites

```sql
SELECT
    b.sitecode, b.priority, b.acres, b.type, b.air_gnd,
    b.culex, b.spr_aedes, b.coq_pert as perturbans,
    b.prehatch, b.remarks, b.drone, b.sample,
    g.facility as facility, g.zone, g.fosarea,
    g.sectcode as section
FROM public.loc_breeding_sites b
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode, 7)
WHERE b.enddate IS NULL
ORDER BY b.sitecode
```

### 8. Webster Filter Options

```sql
SELECT DISTINCT
    g.facility, g.sectcode as section, g.fosarea
FROM public.loc_breeding_sites b
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode, 7)
WHERE b.enddate IS NULL AND g.facility IS NOT NULL AND b.sitecode IS NOT NULL
    [AND g.facility = '{facility_filter}']
    [AND g.fosarea = '{fosarea_filter}']
ORDER BY g.facility, g.sectcode, g.fosarea
```

### 9. Webster Town Codes

```sql
SELECT DISTINCT left(b.sitecode, 4) as towncode
FROM public.loc_breeding_sites b
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode, 7)
WHERE b.enddate IS NULL AND b.sitecode IS NOT NULL AND g.facility IS NOT NULL
    [AND g.facility = '{facility_filter}']
    [AND g.fosarea = '{fosarea_filter}']
ORDER BY left(b.sitecode, 4)
```

### 10. Webster Sections by Town Code

```sql
SELECT DISTINCT g.sectcode
FROM public.loc_breeding_sites b
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode, 7)
WHERE b.enddate IS NULL AND b.sitecode IS NOT NULL AND g.facility IS NOT NULL
    [AND left(b.sitecode, 4) = '{towncode_filter}']
    [AND g.facility = '{facility_filter}']
    [AND g.fosarea = '{fosarea_filter}']
    AND g.sectcode IS NOT NULL
ORDER BY g.sectcode
```

### 11. Structures

```sql
SELECT
    loc.sitecode, loc.s_type, loc.status_udw, loc.sqft,
    loc.priority, loc.culex, loc.chambers,
    loc.comments as remarks,
    g.sectcode as section, g.zone, g.facility, g.fosarea
FROM public.loc_cxstruct loc
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(loc.sitecode, 7)
WHERE loc.enddate IS NULL
ORDER BY loc.sitecode
```

### 12. Structure Town Codes

```sql
SELECT DISTINCT left(g.sectcode, 4) as towncode
FROM public.loc_cxstruct loc
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(loc.sitecode, 7)
WHERE loc.enddate IS NULL AND g.sectcode IS NOT NULL
    [AND g.facility = '{facility_filter}']
    [AND g.fosarea = '{fosarea_filter}']
ORDER BY left(g.sectcode, 4)
```

### 13. Structure Sections by Town Code

```sql
SELECT DISTINCT g.sectcode
FROM public.loc_cxstruct loc
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(loc.sitecode, 7)
WHERE loc.enddate IS NULL AND g.sectcode IS NOT NULL
    [AND left(g.sectcode, 4) = '{towncode_filter}']
    [AND g.facility = '{facility_filter}']
    [AND g.fosarea = '{fosarea_filter}']
ORDER BY g.sectcode
```

### 14. Structure Filter Options

```sql
SELECT DISTINCT
    g.facility, g.sectcode as section, g.fosarea, loc.s_type
FROM public.loc_cxstruct loc
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(loc.sitecode, 7)
WHERE loc.enddate IS NULL AND g.facility IS NOT NULL
    [AND g.facility = '{facility_filter}']
    [AND g.fosarea = '{fosarea_filter}']
ORDER BY g.facility, g.sectcode, g.fosarea
```

---

## R-Side Logic

### Three Data Source Paths

1. **Custom table** (`breeding` + `use_webster = FALSE`): Queries `loc_breeding_site_cards_sjsreast2` directly; facility/zone/foreman are columns in the table; supports dynamic column discovery via `information_schema`.
2. **Webster table** (`breeding` + `use_webster = TRUE`): Queries `loc_breeding_sites` joined to `gis_sectcode`; no dynamic columns; filters on `enddate IS NULL`.
3. **Structures** (`structures`): Queries `loc_cxstruct` joined to `gis_sectcode`; filters on `enddate IS NULL`.

### Dynamic Column Discovery

- `get_dynamic_columns()` queries `information_schema.columns` for `loc_breeding_site_cards_sjsreast2`, subtracts `CORE_BREEDING_COLS` + `EXCLUDED_INTERNAL_COLS` (including `geom`), returns the remainder as dynamic columns (e.g., `airmap_num`, `partialtrt`, `perimacres`).
- `get_dynamic_cols_with_data()` checks which dynamic columns have any non-null/non-empty values for the current facility/foreman filter using `COUNT(NULLIF(col::text, ''))`.
- Dynamic columns appear in the UI title-field checkboxes (prefixed `[DB]`).

### Data Sanitization (`sanitize_breeding_data()`)

Normalizes differences between custom and Webster table formats:
- Trims whitespace on all character columns
- Converts empty strings → `NA` for flag fields (`culex`, `spr_aedes`, `drone`, `sample`, `perturbans`, `prehatch`)
- Uppercases flag fields, `priority`, `air_gnd`
- Truncates `air_gnd` to 1 character
- Coerces `acres` and `type` to numeric
- Sets `NA` remarks to empty string

### Cascading Filter Logic

Filters cascade: **Facility → FOS Area → Town Code → Section**. Each `observeEvent` re-queries the appropriate function (custom, Webster, or Structure variant) to update downstream choices. Zone is independent.

### Prehatch Calculation

When `prehatch_calc` is selected as a watermark or title field:
```r
calculation = max(round(as.numeric(acres) * 2.5, 2), 0.05)
```
Displays as a red overlay with "2.5/ac" label and the computed value.

### Card Generation & Pagination

- `generate_section_cards_html()` creates print-optimized HTML with configurable cards per page (6/8/10/12)
- Cards grouped/split by any combination of: section, priority, structure type
- Double-sided mode pads odd-page groups with a blank page
- Pre-builds table header and empty rows once, reuses for all cards
- Progress callback reports every 50 cards

### Priority Color Coding

`GREEN` → `#90EE90`, `RED` → `#FFB6C1`, `YELLOW` → `#FFFFE0`, `BLUE` → `#ADD8E6`, `ORANGE` → `#FFD580`, `PURPLE` → `#E6D5FF`.

### Special Field Display

- **Culex / Spring Aedes / Perturbans / Sample / RA**: Only shown if value is `"Y"` — displayed as colored badges
- **Chambers**: Only shown for PR-type structures with value > 0
- **Status (D/W/U)**: Mapped to Dry/Wet/Unknown with color coding
- **Facility / Foreman**: Codes resolved to human names via lookup maps at display time

---

## Shapefiles

**None.** No `.shp`, `.gpkg`, or PostGIS geometry used. The `geom` column in `loc_breeding_site_cards_sjsreast2` is explicitly listed in `EXCLUDED_INTERNAL_COLS` and excluded from queries.

---

## Shared Functions Used

| Function | Source File |
|---|---|
| `get_db_connection()` | `shared/db_helpers.R` |
| `safe_disconnect()` | `shared/db_helpers.R` |
| `set_app_name()` | `shared/db_helpers.R` |
| `load_env_vars()` | `shared/server_utilities.R` |
| `get_facility_lookup()` | `shared/db_helpers.R` |
| `get_foremen_lookup()` | `shared/db_helpers.R` |
| `get_structure_type_choices()` | `shared/db_helpers.R` |

