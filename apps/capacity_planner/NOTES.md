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
- **RA exclusion** — active sites inside Restricted Area polygons with `monitor_ok = FALSE`
  (no work happens there) are dropped from every site count and acre figure, in both
  loaders, via `ra_exclusion_sql()`. The list (`data/ra_excluded_sites.csv`) is built offline
  by `scripts/build_ra_exclusions.R` (spatial test of `loc_breeding_sites.geom` centroids
  against `extra/qgis/ra_view.gpkg`) and committed; re-run when RA data or sites change.
  The gpkg's `e_restrctspec` values (employee/FOS numbers like 1904) are ignored; sites stay
  with the FOS of their section (`gis_sectcode.fosarea`). First build: 5,860 of 86,525 sites
  (40,690 acres) excluded.
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
  drive_min, drive_miles, snap_m, straight_miles, snap_flag, built, osm_extract): one-way
  free-flow driving time and road distance from each facility point (`gis_facility` centroid)
  to **every** section centroid (`gis_sectcode.latloncentroid`), so a section moved to another
  facility in the What-If planner has a real time from its new facility (6 x 3,207 = 19,242
  pairs; `straight_miles` is the haversine distance for the straight-line fallback), routed on OpenStreetMap roads by a self-hosted
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
- **Scenarios (What-If)** — a scenario is a set of per-section changes `(sectcode, to_facility,
  to_zone)`. Everything the user clicks is promoted to P1 and, when a destination facility is
  picked, moved there (P1 sections can only change by being moved). Pure functions:
  - `scenario_apply(sections, changes, times)` — returns the section table with the NEW
    `facility`/`zone` plus `facility_from`, `zone_from`, `fos_new` (NA once moved to another
    facility: it belongs to none of that facility's FOS areas), `moved`, `promoted`,
    `time_missing`. Rules: promote is P2 -> P1 only (no demotion); a moved section keeps its
    priority unless it is also promoted; unknown facilities / no-ops are ignored; the last
    change for a section wins. A moved row's `miles` and routed drive columns are re-keyed to
    its new facility from `times` (the full drive-time table).
  - `scenario_compare(after, zone = "1", metrics, facility = NULL, dm_before, dm_after)` — the
    P1 change per facility + DISTRICT (`facility = NULL`) or per FOS area of one facility
    (`facility = "Sj"`, with an "unassigned" row for sections moved in), each metric as
    `_before`, `_after`, `_chg`, plus `p1_min_before` / `added_min` (median drive minutes to the
    group's P1 sections now / to the sections newly in its P1). The What changed tables show the
    `_chg` values. `scenario_cap()` + `staffing_baseline()` give load per tech/crew before and
    after; `scenario_drive_summary()` the median P1 drive minutes.
  - `scenario_staffing(after, staff, metric, per_tech, ...)` (Estimate A) — **per FOS area**
    (one FOS area = one crew). **added** = sections newly in the FOS area's P1 (promoted in
    place), **removed** = its P1 sections moved away. Sections moved INTO a facility land in
    that facility's **unassigned** row (no current P1 or techs, so all the techs it needs are new):
    `techs change = ceil((P1 - removed + added / capacity factor) / per_tech) - ceil(P1 / per_tech)`;
    negative = techs freed. `by_facility` sums the FOS rows; the UI shows it by facility and
    drills into the FOS areas of one facility on click. `per_tech` defaults to the district
    average P1 load per tech and is editable. Drone offload is applied first. **Travel:**
    `travel_ratio` = median one-way minutes to the FOS area's added sections / median to its
    current P1 sections (timed from the facility; the unassigned row uses the facility's median
    P1 time): above 1 = the added sections are farther (more driving), below 1 = closer. Only the
    travel share `s` of a tech's day (editable, default 20%) scales with it:
    `capacity_factor = 1 / ((1 - s) + s * ratio)`, bounded above by 1/(1 - s); only the added
    load is scaled, removed load frees techs at the baseline. Factor 1 (nothing added, missing
    times, travel off, or s = 0) gives the plain formula. The field-speed %, avg mph and
    circuity cancel out of the ratio (they live in the Estimate settings).
  - `scenario_crews(after, rates, ...)` (Estimate B) — `estimate_crews()` per facility on the
    added sections minus the removed ones (each timed from its own facility), with district rates
    or each facility's own; net crews + per-facility hours. Not per FOS area.
- **Shared travel control (What-If, Staffing impact):** one "Include travel time" checkbox
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

## Interactive scenario builder (What-If)

Workflow: build a scenario, see what it adds, load per tech / crew, then the staffing estimates
(all visible, no Apply button). Page order: 1 Build a scenario, 2 What changed, 3 Load per tech
and per crew, 4 Staffing estimates. Each number appears once: What changed holds the added load,
drive times and travel ratio; Estimate A shows only techs.
- **Map** — `load_section_geo()` (no argument = all facilities, simplified polygons) is loaded
  once when the tab is first opened and drawn on a canvas Leaflet map. **Every section is always
  shown** (no facility / priority filters). Color = the facility the section belongs to now (a
  moved section takes its new facility's color), solid = P1, light = P2 (a promoted section turns
  solid). The polygons are drawn ONCE with `leafletProxy`, only after the map reports its bounds
  (`map_ready`, proxy messages sent before the browser builds the map are dropped). A change then
  restyles only the sections that changed: `restyle_sync()` diffs against the last styled state and
  sends a `wi_restyle` custom message (`setStyle` + `setTooltipContent` in the page script), from
  an observer with priority 1000 so the map moves before the tables are recomputed (about 50 ms
  click to recolor). It must not redraw: clearing and re-adding 3,207 polygons is what made it
  flash. The hover highlight sets only `weight` (leaflet restores the creation-time values on
  mouseout, so a highlighted fillOpacity would undo the restyle). `scenario_apply()` takes
  `facilities =` because restyle applies a subset of rows and would otherwise reject a move to a
  facility that is not in the subset.
- **Change sections** — they apply immediately through `scenario_toggle()` (pure, unit tested):
  selecting a P2 section promotes it to P1 and, with a destination under **Move sections to**,
  moves it there; **selecting it again undoes that** (a promoted section goes back to P2). A
  P1 section can only change by being moved. With a different destination picked, selecting a
  moved section re-moves it instead of undoing it. A **box** (Shift + drag on desktop, the
  **Box select** button on touch, one box per tap) undoes everything in it when every section it
  can change is already in the scenario, otherwise it adds the missing ones. The section-list
  checkboxes (keyboard route) always apply / undo. **Reset scenario** clears everything. The
  scenario is interactive and not deep-linked.
- **What changed** — what the scenario ADDS to P1, by facility: sections, ground/air/prehatch
  sites and acres, wet catch basins, then the drive times. Both are **medians** of one-way
  minutes: **to current P1** (the group's P1 sections today) and **to added sections** (the
  sections the scenario brings into its P1, timed from that facility). **Travel ratio** = added
  / current (above 1 = the added sections are farther away, so more driving), computed from the
  two numbers in the row, so it can be checked by hand; hidden when travel is off. Estimate A turns the ratio into
  its capacity factor (see the equations); the factor itself is not shown. A move shows as + at
  the receiving facility and - at the one giving up P1 sections. Click a facility row (or use the
  picker) for the same table by FOS area; an "unassigned" row (sections moved in) has no P1 of
  its own, so it is compared with the facility's median P1 time. The travel controls (include
  travel, drive source, travel share) sit at the top of this section.
- **Load per tech and per crew** — P1 load per tech (sites and acres) and per crew, now -> after.
- **Staffing estimates** — Estimate A driver / load per tech, the equations, Estimate A, Estimate
  B (net crews, by facility). Estimate A is only techs: techs, need now, need after, **techs
  change**, gap now (by facility, drill into FOS areas); the load added / removed, drive times and
  travel ratio are in What changed. The staffing edits for one facility, jobs per site, hours per job
  and trips sit in a collapsed "Estimate settings" section.
  - **A. Techs change** (per FOS area) = ceil((P1 - removed + added / capacity factor) / load-per-tech) - ceil(P1 / load-per-tech)
  - **B. Net crews** = [ Sum_jobtype ( (added - removed sites) x jobs-per-site x hours-per-job ) + travel ] / hours-per-crew-season,
    where **travel = Sum_section ( round trips x 2 x one-way drive time )**, added minus removed.
- **Share a scenario** — **Copy link to this scenario** (section 1) makes a link whose `s=`
  parameter carries everything: the changed sections (one bitmap per destination group over the
  sorted section list, plus a fingerprint of that list), all `wi_*` inputs except the three
  per-facility staffing boxes, the WHOLE staffing table (17-digit text, because the image's
  jsonlite keeps only 15 digits and 6.666666666666667 techs per crew must round-trip exactly), the
  map centre / zoom, the date and a data stamp (P1 / P2 counts). Nothing is stored server-side
  (Redis is memory-only, the DB connection is read-only), so links never expire. Pure functions
  `scenario_pack` / `scenario_unpack` / `scenario_share_state` / `share_token_encode` /
  `share_token_decode` / `share_state_*` in `display_functions.R` (unit tested, incl. tampered
  and oversized tokens: JSON only, 12,000-char cap, allow-listed ids, validated values).
  Opening a link runs `expand_share()` through the shared engine's `expand_state` hook: it
  restores the scenario, staffing table and map view, then the engine applies the saved settings
  (two passes). The recipient gets an editable copy plus a banner; a link made against a different
  section list says it can't be restored (never guessed), and a changed P1 / P2 count adds a
  data-changed note. The link is built in the page from `location.origin + pathname` (the app
  sits behind OpenResty) and shown in a read-only box, the fallback when the clipboard is blocked.
- **Staffing boxes (`wi_techs_per_crew`, `wi_drone_offload`, `wi_drone_air_offload`)** show the
  selected facility's row. They used to be written back from `input$wi_*`, which looped forever
  when a stale value pushed for one facility arrived after the selection moved to another (seen
  with any link that selects a non-first facility). Now the server pushes values with a
  `wi_staff_set` custom message that names the facility and is never an edit; the page sends
  `wi_staff_edit` (tagged with the facility) only for a genuine edit while the boxes belong to the
  selected facility; `staff_apply_edit()` writes to that facility only and ignores display
  rounding (5.71 for 5.714...), which also stops East's techs drifting to 5.71.
- **Selected table rows** use a light highlight with dark text (`--dt-row-selected*` in the head
  CSS); DataTables' default white-on-blue (4.2:1) failed WCAG AA.
- **CSV downloads** — every results table has a "Download CSV" button (`csv_out()` at the end
  of the server): Capacity (the existing one, plus averages and the drive-time summary),
  Staffing (assumptions, baseline, techs per FOS area), and What-If (changed sections, by
  facility, by FOS area for all facilities, load per tech and crew, Estimate A per FOS area,
  Estimate B by job type and by facility). The Capacity and Staffing files are the tables as
  shown; the What-If files hold plain numbers with snake_case headers (`scenario_export_*()`,
  unit tested), not the formatted text on the page (so "+4,443" is 4443 and "319.3 -> 320.9" is
  a `_now` and an `_after` column). Files are named `capacity_<table>_<date>.csv`.
- **Tables update in place** — `live_dt()` renders a table once and then pushes new numbers with
  `DT::replaceData`, so a click does not re-initialise (flash) the results tables. It rebuilds a
  table only when its columns change (Show: sites/acres, travel on/off, or the empty "Note"
  placeholder). The section list is built from today's sections and not rebuilt per change (its
  ticks follow the scenario through `wi_sel_sync`). The CSS also stops Shiny's fade on the
  What-If outputs.

## App (`app.R`) — three tabs

1. **Capacity by FOS** — wide table (P1/P2 per metric), facility filter, sites/acres/both
   toggle, priority-breakdown toggle, optional est. drive-time columns, averages with
   mean + median and box plots (P1/P2, selectable metric), CSV download.
2. **Staffing** — editable staffing table (crews, techs/crew, drone techs, drone offload %,
   Reset to defaults, DB FOS-area cross-check), the P1 baseline table (acres/tech,
   acres/crew, air sites/tech), and techs per FOS with P1 load-per-tech ratios.
   Edits drive every estimate (`staff` reactiveVal → `cap_live()`).
3. **What-If Planner** — click sections on the map to promote them to P1 (optionally moving
   them to another facility) and see what that adds (sections, sites, acres, catch basins,
   drive time) by facility and by FOS area, then the techs (per FOS area) and crews estimates.

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

## Phone layout

- One `@media (max-width: 767px)` block in the `app.R` head: tighter gutters/wells, full-width
  buttons, 16 px inputs (no iOS zoom), tabs swipe in one row, equations wrap, smaller DT font,
  What-If map 320 px tall. Desktop is unchanged.
- Every table goes through `capacity_dt()` (`display_functions.R`): `scrollX` plus DT's
  FixedColumns, so the facility/FOS label columns stay pinned while the rest swipe. Pass `left`
  for how many leading columns to pin (default 1; the editable Staffing table is not pinned).
- Map selection: tap toggles a section. To select many on touch, the map has a **Box select**
  toggle (top right): while ON, one-finger drag draws a rectangle (map panning is off) and adds
  (or undoes, if all are already in) every section whose centroid is inside, same as desktop Shift+drag (pointer events, so it also
  works with mouse/pen). A tap without dragging still toggles one section. The hint text switches
  on `@media (hover: none)`. Selecting a section again undoes it, on both.
- Box plots hide the Plotly mode bar and use compact margins.
