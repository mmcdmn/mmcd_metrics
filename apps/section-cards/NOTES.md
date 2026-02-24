# Section Cards - Technical Notes

## Overview
This app generates printable field cards for:
1. **Air/Ground Sites** (breeding sites)
2. **Structures**

Air/Ground mode supports two data sources:
- **QGIS method (default):** `public.loc_breeding_site_cards_sjsreast2`
- **Webster method:** `public.loc_breeding_sites` + `public.gis_sectcode`

UI toggle (Air/Ground only): **Use Webster data (no custom columns)**

## Data Sources

### Core Database Tables
- **`public.loc_breeding_site_cards_sjsreast2`**
  - Includes app-facing values for `facility`, `zone`, `foreman`
  - Supports dynamic extra columns (`[DB] ...` options)

- **`public.loc_breeding_sites`**
  - Webster Air/Ground source when toggle is ON
  - Core breeding-site fields only
  - Requires section join to resolve geography/assignment fields

- **`public.gis_sectcode`**
  - Provides `sectcode`, `facility`, `zone`, `fosarea`
  - Used by Webster Air/Ground mode and Structures mode

- **`public.loc_cxstruct`**
  - Structures source

## Table Comparison (QGIS vs Webster)

### Where Facility and Foreman/FOS filters come from

#### QGIS method (`loc_breeding_site_cards_sjsreast2`)
- **Facility filter source:** `loc_breeding_site_cards_sjsreast2.facility`
- **Foreman/FOS filter source:** `loc_breeding_site_cards_sjsreast2.foreman`
- App-level mapping: `fosarea = foreman`

#### Webster method (`loc_breeding_sites` + `gis_sectcode`)
- **Facility filter source:** `gis_sectcode.facility`
- **Foreman/FOS filter source:** `gis_sectcode.fosarea`
- **Not sourced from:** `loc_breeding_sites.foreman`

### Other differences
- **Section**
  - QGIS: `left(sitecode, 7)`
  - Webster: `gis_sectcode.sectcode`
- **Zone**
  - QGIS: `loc_breeding_site_cards_sjsreast2.zone`
  - Webster: `gis_sectcode.zone`


## Data Collection Strategy

### QGIS method pattern
Single-table read from `public.loc_breeding_site_cards_sjsreast2`.

### Webster method pattern
Join-based read:
```sql
FROM public.loc_breeding_sites b
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode, 7)
WHERE b.enddate IS NULL
```

## App Flow

### Filter cascade (Air/Ground)
1. Facility
2. FOS Area
3. Town Code
4. Section



## Key Files
- `app.R` - mode switching, filter cascade, card generation/download
- `data_functions.R` - query helpers for current/Webster/structures
- `display_functions.R` - card HTML/CSS rendering
- `ui_helper.R` - filter and configuration controls

## SQL Reference
used in `get_webster_breeding_sites()`:

```sql
SELECT
  b.sitecode,
  b.priority,
  b.acres,
  b.type,
  b.air_gnd,
  b.culex,
  b.spr_aedes,
  b.coq_pert as perturbans,
  b.prehatch,
  b.remarks,
  b.drone,
  b.sample,
  g.facility as facility,
  g.zone,
  g.fosarea,
  g.sectcode as section
FROM public.loc_breeding_sites b
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode, 7)
WHERE b.enddate IS NULL
ORDER BY b.sitecode
```
