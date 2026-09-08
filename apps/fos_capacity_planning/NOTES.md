# FOS Capacity & Border-to-Border Planning – Technical Notes

Strategic offseason tool giving per-FOS-area capacity numbers so ROMs/facilities can
build 5-year border-to-border plans and estimate staffing to move P2 areas into P1.
**Static inventory** (current universe), not treatment progress.

## Database Tables & Entities

| Table | Key Columns Used |
|---|---|
| `loc_breeding_sites` | `sitecode`, `air_gnd` (G/A), `priority`, `prehatch`, `acres`, `enddate` |
| `gis_sectcode` | `sectcode`, `facility`, `zone` (1/2), `fosarea`, `latloncentroid` |
| `loc_catchbasin` | `sitecode`, `status_udw`, `lettergrp`, `enddate` |
| `gis_facility` | `abbrv`, `the_geom` (facility point) |
| `employee_list` | `emp_type`, `active`, `facility`, `fieldsuper` |

## R-Side Logic (`data_functions.R`)

- **`load_capacity_sections()`** — section grain (one row per `gis_sectcode`), with
  ground/air/prehatch site counts + prehatch acres (from `loc_breeding_sites`, joined
  `LEFT(sitecode,7)=sectcode`, `enddate IS NULL`), wet catch basins
  (`status_udw='W'`, `lettergrp<>'Z'`), and straight-line **miles** facility→section
  (`ST_Distance` on `gis_facility.the_geom` centroid vs `gis_sectcode.latloncentroid`,
  4326 geography, ÷1609.34). This is the base grain for the capacity table AND the
  what-if section picker. **All `COUNT(*)` cast `::int`** (RPostgres returns
  `integer64`/bit64 which mis-reads as a garbage double otherwise).
- **`load_capacity_by_priority()`** — site grain grouped by facility/fosarea/zone/priority
  (priority is a site attribute, so it can't roll up from section grain).
- **`load_capacity_staffing()`** — techs per FOS (`emp_type='Inspector'` grouped by
  `fieldsuper` = the FOS emp_num they report to, which equals `gis_sectcode.fosarea`);
  FOS per facility (`emp_type='FieldSuper'` by facility). `active=true`.
- **`aggregate_capacity_by_fos()`** — rolls sections up to facility×fosarea×zone; adds FOS
  shortname (`get_foremen_lookup`), facility full name (`get_facility_lookup`), techs.
- **`capacity_averages(cap, zone)`** — mean of each metric across FOS areas, per facility
  and district-wide.

## Display / What-If (`display_functions.R`)

- **`capacity_wide(cap)`** — pivots zone-specific metrics to P1/P2 columns (one row per FOS).
- **`est_drive_minutes(miles, avg_mph, circuity)`** = `miles × circuity ÷ avg_mph × 60`
  (avg_mph default 45, circuity 1.3; both editable in the UI). Straight-line estimate — no
  routing dependency. *Future:* an optional `facility_section_drivetime` lookup (routed
  minutes from an offline batch) can override, falling back to this estimate.
- **`whatif_compute(sections, promote_sectcodes, cap, metric, per_tech)`** — moves promoted
  P2 sections into P1, recomputes each FOS's P1 load of `metric`, and estimates
  **added techs = ceil(new_P1/per_tech) − ceil(cur_P1/per_tech)** (the increase caused by
  the promotion; unaffected FOS = 0). `per_tech` defaults to the district average P1
  load/tech and is editable in the UI. Also reports `gap_now` (current over/under vs baseline).

## Workload-study crew model (2001 Ch. 11) — What-If "Estimate B"

Second staffing estimate alongside the load-per-tech one, using MMCD's established
expansion-proposal method: **site counts × jobs-per-site × hours-per-job ÷
hours-per-crew-season = crews required** (crew = 1 FOS + its inspectors).

- **`load_jobs_per_site(year)`** — live jobs-per-distinct-site rates from
  `dblarv_insptrt_current`+`archive` for the season, by facility × zone × job_type
  (+ DISTRICT roll-up via `GROUPING SETS`). Action→job map: `A`/`D`→air_trt, `4`→air_insp,
  `1`→gnd_insp_trt, `3`→gnd_trt, `2`→gnd_insp. `rate = jobs ÷ distinct sites serviced`
  (Table 11.4). Expansion uses the **P1** rates (P1 sites are serviced more often).
- **Hours-per-job** is NOT in any DB (no timesheet — only GPS `track_point_realtime` +
  an FF10 hand-entry table), so it's editable UI input, defaulting to the study's district
  means (Table 11.3): air trt 1.35, air insp 0.66, gnd insp&trt 1.08, gnd trt 0.55,
  gnd insp 0.17. Hours-per-crew-season default 2,184 (Table 11.6). Constants:
  `STUDY_HOURS_PER_JOB`, `STUDY_CREW_HOURS_SEASON`, `JOB_TYPE_LABELS`, `JOB_TYPE_SITEKIND`.
- **`estimate_crews(promoted_sections, rates, hours_per_job, hrs_per_crew)`** — jobs =
  site_count(kind) × rate; hours = jobs × hrs/job; crews = Σhours ÷ hrs/crew. Verified:
  all 339 Sr P2 sections (1309 gnd + 298 air) → 9,601 jobs → 6,621 hrs → 3.03 crews.

## Interactive section map (What-If)

`load_section_geo(facility)` returns each section's WGS84 polygon (`the_geom`) as WKT +
centroid lat/lon; the app builds an `sf` (P2 only) and renders a Leaflet map. Selection
of sections to promote is driven by the map into a shared `sel` reactiveVal:
- **Click** a polygon → toggle (add/remove); `layerId = sectcode`, selected sections get a
  highlighted overlay in group `"sel"` (redrawn via `leafletProxy`). Overlay `layerId`s are
  prefixed `sel_` and stripped on click so a highlighted section deselects.
- **Shift + drag a box** → add all sections whose centroid falls in the box. Implemented
  with a dependency-free `htmlwidgets::onRender` handler (`leaflet.extras` is NOT installed)
  that disables box-zoom, draws a rectangle, and sends bounds to `Shiny.setInputValue('wi_box')`.
- Buttons: **Select all shown**, **Clear**; a live "Selected: N" readout; the section list
  DT mirrors the selection (read-only `Sel` column).

Both estimates recompute **live** from `sel()` (no run button). The two governing
equations are shown up front, and every input number is exposed:
- **A. Added techs** = Σ_FOS ( ⌈new P1 ÷ load-per-tech⌉ − ⌈current P1 ÷ load-per-tech⌉ )
- **B. Crews** = Σ_jobtype ( sites × jobs-per-site × hours-per-job ) ÷ hours-per-crew-season
  (the by-job table shows sites, jobs/site, jobs, hrs/job, hours, and a TOTAL row).

## App (`app.R`) — three tabs

1. **Capacity by FOS** — wide table (P1/P2 per metric), facility filter, priority-breakdown
   toggle, optional est. drive-time columns, averages (P1/P2), CSV download.
2. **Staffing** — techs per FOS, FOS per facility, P1 load-per-tech ratios.
3. **What-If Planner** — pick facility → select P2 sections → Estimate → added-techs summary
   + per-FOS impact table.

## Cross-checks (validated against live DB)

- Sr FOS 1905 (Alex D) P1: 102 sections, 1881 ground, 190 air, 561 prehatch sites,
  ~242 prehatch acres, 6124 wet catch basins, avg 6.8 mi, 4 techs.
- Facility geom present for all field facilities (E, N, Sj, Sr, Wm, Wp); all 3,207
  sections have `latloncentroid`.

## Shapefiles

**None.** Geometry read live from PostGIS (`gis_facility.the_geom`,
`gis_sectcode.latloncentroid`).

## Shared Functions Used

| Function | Source |
|---|---|
| `get_db_connection()` / `safe_disconnect()` | `shared/db_helpers.R` |
| `get_foremen_lookup()` / `get_facility_lookup()` | `shared/db_helpers.R` |
| `set_app_name()` | `shared/db_pool.R` |
