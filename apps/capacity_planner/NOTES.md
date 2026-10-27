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
| `employee_list` | *(no longer used for tech counts: the `active` flag was unreliable)* |

## R-Side Logic (`data_functions.R`)

- **`load_capacity_sections()`** — section grain (one row per `gis_sectcode`), with
  ground/air/prehatch site counts **and acres** (`ground_acres`, `air_acres`,
  `prehatch_acres`; every site count has an acres partner; from `loc_breeding_sites`, joined
  `LEFT(sitecode,7)=sectcode`, `enddate IS NULL`), wet catch basins
  (`status_udw='W'`, `lettergrp<>'Z'`), and straight-line **miles** facility→section
  (`ST_Distance` on `gis_facility.the_geom` centroid vs `gis_sectcode.latloncentroid`,
  4326 geography, ÷1609.34). This is the base grain for the capacity table AND the
  what-if section picker. **All `COUNT(*)` cast `::int`** (RPostgres returns
  `integer64`/bit64 which mis-reads as a garbage double otherwise).
- **`load_capacity_by_priority()`** — site grain grouped by facility/fosarea/zone/priority
  (priority is a site attribute, so it can't roll up from section grain).
- **`default_staffing()`** — ROM-confirmed district staffing per facility (crews, techs per
  crew, drone techs, ground offload %, air offload %) from `config/app_config.yaml` →
  `capacity_planner.staffing` (fallback: `CAPACITY_STAFFING_FALLBACK`). **Replaces the
  `employee_list` `active` count**, which was inconsistent. 1 crew = 1 FOS area + its ground
  techs (techs exclude the FOS); this matches the DB's FOS-area count per facility
  (E7 N6 Sj4 Sr4 Wm5 Wp5). Drone techs are facility-level and are NOT in the ground tech
  count. Users edit these in the app (session-only); config is the default.
- **`db_fos_area_counts(sections)`** — FOS areas per facility in the DB, shown as a
  cross-check against configured crews.
- **`aggregate_capacity_by_fos()`** — rolls sections up to facility×fosarea×zone; adds FOS
  shortname (`get_foremen_lookup`), facility full name (`get_facility_lookup`), and from
  the staffing table `n_techs` (= facility techs/crew), `drone_techs`, `drone_offload_pct`.
- **`capacity_averages(cap, zone, metrics)`** — **mean and median** (`<metric>_mean`,
  `<metric>_median`) of each metric across FOS areas, per facility and DISTRICT. The
  Averages section always covers all facilities (ignores the facility filter) so DISTRICT is
  the true district.
- **`capacity_boxplot_data(cap, zone, metric)`** — one row per FOS area with a DISTRICT group
  + one group per facility; drives the plotly box plots (median line, dashed mean, FOS-area
  points) in the Averages section. A text summary under each plot repeats the DISTRICT
  min/median/mean/max for screen readers.
- **Sites / acres toggle** — `capacity_visible_metrics(measure)`, `select_measure_cols()` and
  `capacity_col_label()` (display_functions.R) drive the "Show" radio (`cap_measure`: both /
  sites / acres) on the capacity table, priority view, averages and CSV. Columns are labelled
  explicitly ("Ground sites P1", "Ground acres P1"). Sections and wet catch basins are
  count-only (no acres).

## Display / What-If (`display_functions.R`)

- **`capacity_wide(cap)`** — pivots zone-specific metrics to P1/P2 columns (one row per FOS).
- **`est_drive_minutes(miles, avg_mph, circuity)`** = `miles × circuity ÷ avg_mph × 60`
  (avg_mph default 45, circuity 1.3; both editable in the UI). Straight-line estimate —
  no routing dependency; used when routed times are unavailable or the user picks it.
- **Routed drive times (default)** — `data/facility_section_drivetime.csv` (facility, sectcode,
  drive_min, drive_miles, snap_m, snap_flag, built, osm_extract): one-way free-flow driving
  time and road distance from the facility point (`gis_facility` centroid) to each section
  centroid (`gis_sectcode.latloncentroid`), routed on OpenStreetMap roads by a self-hosted
  **OSRM** (Docker, Minnesota Geofabrik extract). Built **offline** by
  `scripts/build_drivetimes.R` (setup commands are in its header) and committed, because the
  app's database connection is read-only; re-run it when sections or facilities change.
  `load_routed_drivetimes()` / `attach_routed_drivetimes()` add `drive_min_routed`,
  `drive_miles_routed`, `snap_m`, `snap_flag` to the section table;
  `section_drive_minutes(sections, mode, avg_mph, circuity, speed_pct)` returns one-way
  minutes (routed, falling back per section to the estimate; `speed_pct` scales routed
  minutes, e.g. 80 = crews travel at 80% of map speed). `routed_summary()` compares routed
  vs. straight-line per facility (circuity = road mi ÷ straight-line mi, implied mph).
  Limits: no live traffic; times are facility → section (crews really chain sections in a
  day); centroids inside wetlands/lakes snap to the nearest road (`snap_flag` = > 1 km).
  First build (2026-10-26, 3,207 sections, none unreachable, 20 snapped > 1 km): median
  circuity 1.43 (1.35–1.62 by facility; Sr highest because of river crossings) and implied
  speed ~34 mph, so the old 1.3 / 45 mph estimate understated drive time by ~1.4× (Sr ~1.8×).
- **`staffing_baseline(cap, staff)`** — per facility + DISTRICT: P1 ground, air and prehatch
  sites and acres divided by ground techs (crews × techs/crew) and crews → sites/tech,
  acres/tech, acres/crew for each of ground, air and prehatch. **ALL ground techs inspect air sites**, so
  air load is per ground tech, not per drone tech. This is the "what a tech /
  crew can cover" baseline.
- **Drone offload** — drones help ground techs in two ways, each its own per-facility %:
  `drone_offload_pct` scales `DRONE_OFFSET_METRICS` (ground sites/acres, prehatch sites/acres) and
  `drone_air_offload_pct` scales `DRONE_AIR_OFFSET_METRICS` (air sites = air *inspections*) by
  `(1 − pct/100)` before Estimate A, so ground techs are staffed only for what drones don't
  take. Drone treatments (action `D`) are on GROUND sites. Default 0% (not yet calibrated).
  Sections / wet CB are never offloaded.
- **`whatif_compute(sections, promote_sectcodes, cap, metric, per_tech)`** — moves promoted
  P2 sections into P1, recomputes each FOS's P1 load of `metric`, and estimates
  **added techs = ceil(new_P1/per_tech) − ceil(cur_P1/per_tech)** (the increase caused by
  the promotion; unaffected FOS = 0). `per_tech` defaults to the district average P1
  load/tech and is editable in the UI. Also reports `gap_now` (current over/under vs baseline).
  **Travel time (`drive_minutes`, `travel_adjust`):** per FOS, `travel_ratio` = (avg one-way
  drive minutes to its current P1 sections) ÷ (avg to its promoted sections). The promoted
  load is worked at `per_tech × travel_ratio` (farther than usual → ratio < 1 → each tech
  carries less): `needed_new = ceil(cur_P1/per_tech + added_load/(per_tech × ratio))`; the
  existing P1 load keeps the baseline capacity, and drone offload is applied first. Ratio 1
  (no promoted sections, missing times, or travel off) reproduces the unadjusted formula
  exactly. By-FOS table shows Avg P1 min / Avg promoted min / Travel ratio. The ratio is
  relative, so the field-speed % does not change Estimate A (it does change B's travel hours);
  switching routed ↔ straight-line source does.
- **Shared travel control (What-If, top well):** one "Include travel time" checkbox
  (`wi_travel`, default on) + source (routed / straight-line) + field speed % / avg mph /
  circuity feed BOTH estimates. Off → A uses ratio 1 and B's travel hours are 0.

## Workload-study crew model (2001 Ch. 11) — What-If "Estimate B"

Second staffing estimate alongside the load-per-tech one, using MMCD's established
expansion-proposal method, now with **drive time** folded in:
**( site counts × jobs-per-site × hours-per-job + travel ) ÷ hours-per-crew-season
= crews required** (crew = 1 FOS + its inspectors). Travel (drive-time) hours are
added on top of the study's on-site hours — see the `estimate_crews` bullet.

- **`load_jobs_per_site(year)`** — live jobs-per-distinct-site rates from
  `dblarv_insptrt_current`+`archive` for the season, by facility × zone × job_type
  (+ DISTRICT roll-up via `GROUPING SETS`). Action→job map: `A`→air_trt (**not** `D`: drone treatments are on GROUND sites and are excluded), `4`→air_insp,
  `1`→gnd_insp_trt, `3`→gnd_trt, `2`→gnd_insp. `rate = jobs ÷ distinct sites serviced`
  (Table 11.4). Expansion uses the **P1** rates (P1 sites are serviced more often).
- **Hours-per-job** is NOT in any DB (no timesheet — only GPS `track_point_realtime` +
  an FF10 hand-entry table), so it's editable UI input, defaulting to the study's district
  means (Table 11.3): air trt 1.35, air insp 0.66, gnd insp&trt 1.08, gnd trt 0.55,
  gnd insp 0.17. Hours-per-crew-season default 2,184 (Table 11.6). Constants:
  `STUDY_HOURS_PER_JOB`, `STUDY_CREW_HOURS_SEASON`, `JOB_TYPE_LABELS`, `JOB_TYPE_SITEKIND`.
- **`estimate_crews(promoted_sections, rates, hours_per_job, hrs_per_crew,
  trips_per_section, avg_mph, circuity)`** — *on-site:* jobs = site_count(kind) × rate,
  hours = jobs × hrs/job. *Travel (added):* each promoted section is driven
  `trips_per_section` round trips/season, so travel hrs = Σ_section trips × 2 ×
  (one-way drive hours from `section_drive_minutes()`: routed by default, else
  `est_drive_minutes(miles, avg_mph, circuity)`). **crews =
  (on-site + travel) ÷ hrs/crew.** Returns `totals$onsite_hours`, `travel_hours`, and
  `hours` (= the full on-site + travel pool). Verified on-site (no travel): all 339 Sr
  P2 sections (1309 gnd + 298 air) → 9,601 jobs → 6,621 hrs → 3.03 crews. `avg_mph`/
  `circuity`/trips are editable on the What-If tab (`wi_avg_mph`, `wi_circuity`,
  `wi_crew_trips`; `DEFAULT_TRIPS_PER_SECTION = 8`, a planning assumption not a study
  value). **Caveat:** if the study hours-per-job already include travel, set trips = 0
  to avoid double-counting.

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
- **A. Added techs** = Σ_FOS ( ⌈current P1 ÷ load-per-tech + added load ÷ (load-per-tech × travel ratio)⌉ − ⌈current P1 ÷ load-per-tech⌉ )
- **B. Additional crews** = [ Σ_jobtype ( sites × jobs-per-site × hours-per-job ) + travel ] ÷ hours-per-crew-season,
  where **travel = Σ_section ( round trips × 2 × one-way drive time )**. The by-job table
  shows sites, jobs/site, jobs, hrs/job, hours, plus a **Travel (drive time)** row and a
  **TOTAL (incl. travel)** row; the summary breaks out on-site + travel = total hrs.
  Computed from the **promoted sections only**, so it is the *additional* crews to service
  the selected area at P1 intensity (matches the study's expansion figure, Table 11.6) —
  **not** a facility total, and not net of current P2 effort.

## App (`app.R`) — three tabs

1. **Capacity by FOS** — wide table (P1/P2 per metric), facility filter, sites/acres/both
   toggle, priority-breakdown toggle, optional est. drive-time columns, averages with
   mean + median and box plots (P1/P2, selectable metric), CSV download.
2. **Staffing** — editable staffing table (crews, techs/crew, drone techs, drone offload %,
   Reset to defaults, DB FOS-area cross-check), the P1 baseline table (acres/tech,
   acres/crew, air sites/tech), and techs per FOS with P1 load-per-tech ratios.
   Edits drive every estimate (`staff` reactiveVal → `cap_live()`).
3. **What-If Planner** — pick facility → (optionally edit that facility's techs/crew and
   drone offload %, shared with the Staffing tab) → select P2 sections → Estimate →
   added-techs summary + per-FOS impact table.

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
