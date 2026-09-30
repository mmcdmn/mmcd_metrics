# Deep Linking, Auto-Refresh & Drop-in Embeds

Status + usage reference for the URL deep-linking / auto-refresh feature and the
drop-in embed widgets.

- **Deep link** = a URL whose query params pre-apply an app's filters and then
  auto-click its Refresh (or Generate) button — or nothing, for apps that update
  live — so the view loads with no manual interaction.
- **`autorefresh=true`** = after the initial load, the view re-refreshes on an
  interval (default 5 min) with a visible **Pause** control. Default is off.
- **Embed** = a filter-free, cache-only chart or stat tile for dropping into other
  sites (the JS "webster" app, the public website) via `<iframe>` or a JSON fetch.

Engine: `shared/url_state_helpers.R` (deep link + auto-refresh),
`shared/embed_helpers.R` + `api/plumber.R` + `apps/embed/` (embeds).

> **Values are exact.** Put the **value**, not the label, in the URL. Example:
> the SUCO graph type shows "Bar" but the value is `bar` → `?graph_type=bar`.
> Values with spaces must be URL-encoded (`Needs ID` → `Needs%20ID`).
> `(from DB)` = the choices are loaded at runtime (facilities, FOS, species,
> years); use the app's own dropdown to see the current list.

---

## 1. Progress

### Completed — deep-linkable + auto-refresh (16 apps)

| App | Route | Notes |
|---|---|---|
| Ground Prehatch Progress | `/ground_prehatch_progress/` | full historical yearly/weekly params |
| Structure Treatments | `/struct_trt/` | split yearly/weekly metric inputs |
| SUCO History | `/suco_history/` | no historical tab |
| Cattail Treatments | `/cattail_treatments/` | 2 refresh buttons (progress vs historical) |
| Catch Basin Status | `/catch_basin_status/` | 2 refresh buttons, split yearly/weekly metric |
| Drone | `/drone/` | 4 tabs |
| Red Air Sites | `/air_sites_simple/` | one refresh button; tab values are titles |
| Inspections | `/inspections/` | **two-stage**: auto-clicks Load Data → the tab's Analyze |
| Cattail Inspections | `/cattail_inspections/` | 2 refresh buttons (per tab) |
| Trap Surveillance | `/trap_surveillance/` | targets the main map view |
| Control Efficacy | `/control_efficacy/` | single refresh, 3 tabs |
| Mosquito Monitoring | `/mosquito-monitoring/` | Update button per tab (All / Compare) |
| Mosquito Surveillance Map | `/mosquito_surveillance_map/` | auto-updating map (no refresh click) |
| Section Cards | `/section-cards/` | filters + Generate Cards |
| Air Inspection Checklist | `/air_inspection_checklist/` | bespoke parser (claims/emp feature); auto-loads on open, now covers all filters + autorefresh |
| FOS Capacity Planning | `/fos_capacity_planning/` | live estimates (no refresh click); deep-links tab + facility + what-if inputs |

### Already self-sufficient (its own URL params, not this engine)

| App | Route |
|---|---|
| Overview (district/facility/FOS) | `/overview/` |

### Embed infrastructure — built (v2: full filter parity)

- Public API (no key, CORS `*`): `/v1/public/embed/chart`,
  `/v1/public/embed/statbox`, `/v1/public/embed/metrics`; static widget `/embed/`.
- The embed API now accepts the **same params** as the deep-link URLs (app, view,
  facility, zone, fos, group_by, graph_type, time_period, …) and reproduces each
  app's own chart data — **peek Redis → compute-live-on-miss → cache (short TTL)**.
  Returns data-series JSON + resolved options; the widget draws by `graph_type`.
- v1 `?metric=<id>&type=<avg>` (cache-only historical_averages) still works.
- Per-app rollout is in progress — see §4 "App coverage".

### Not yet done

| App | Why |
|---|---|
| red_air_legacy | legacy, superseded by `air_sites_simple`; **not deployed** (no `shiny-server.conf` route) |
| about, embed, refresh_views, test-app, trap_survillance_test | static / headless / test pages |

### Verification status

All 16 apps parse-clean; 10 construct-verified locally, the rest pull
container-only packages so they are parse-verified (they construct in the
container). Accessibility + per-app suites pass. Deep links + embeds confirmed
working in the browser by the user.

**Filter-coverage audit (every deployed app):** each app's discrete inputs were
extracted from source (via `getParseData`) and cross-checked against its
deep-link spec — **no data filter is missing**. Notes from the audit:
- Ground Prehatch's Historical tab reads the **main** sidebar
  facility/zone/fos/group_by (already in the spec); its `hist_*` sidebar copies
  are vestigial (nothing reads them), so they are intentionally not mapped.
- Engine fix (this round): a two-pass apply now re-applies URL state after the
  initial cross-input cascade settles, so a value like `fos=1904` is no longer
  clobbered when setting `facility=` resets the FOS dropdown. Applies to every
  app with a parent→child filter reset.

---

## 2. How to use the URLs (localhost)

Full local stack (Docker, OpenResty on 3838): `http://localhost:3838/<app>/?param=value&param2=value2`

Single app run (`shiny::runApp(port=3838)`): no `/<app>/` prefix, and `/v1/...` +
`/embed/` are unavailable (they need the full stack).

- Combine as many params as you want at once.
- Add `&autorefresh=true` to auto-refresh with a Pause control.
- Use the **value** (right of the `=`), URL-encode spaces (`%20`).

**Shared value sets** (used by the per-app tables below):
- **facility** — `all | E | N | Sj | Sr | Wm | Wp` (Sr = South Rosemount)
- **fos** — the FOS **number** (the FieldSuper `emp_num`, which equals the `fosarea` code), or `all`. These are per-facility, e.g. **South Rosemount (Sr) = `1903 | 1904 | 1905 | 1906`**; other facilities use their own number ranges `(from DB)`. Multiple allowed, comma-separated: `fos=1904,1905`. (Pair with the matching `facility` so the FOS belongs to it, e.g. `facility=Sr&fos=1904`.)
- **color_theme** — `MMCD` `(other theme names from DB)`
- **autorefresh** — `true | false`
- date value = `YYYY-MM-DD`; range value = `YYYY-MM-DD,YYYY-MM-DD`; slider range = `from,to` (e.g. `2018,2025`); number = a plain number.

---

## 3. Per-app URL parameters (Param → Options)

### Ground Prehatch Progress — `/ground_prehatch_progress/`

| Param | Options |
|---|---|
| `tabs` | `overview` \| `details` \| `historical` |
| `facility` | facility |
| `zone` | `1` \| `2` \| `1,2` \| `combined` |
| `fos` | fos |
| `group_by` | `facility` \| `foreman` \| `township` \| `sectcode` \| `mmcd_all` |
| `display_metric` | `sites` \| `acres` |
| `expiring_filter` | `all` \| `expiring` \| `expiring_expired` |
| `expiring_days` | number |
| `hist_time_period` | `yearly` \| `weekly` |
| `hist_display_metric` | `sites` \| `acres` \| `treatment_acres` \| `weekly_active_count` \| `weekly_active_acres` |
| `hist_chart_type` | `stacked_bar` \| `grouped_bar` \| `line` \| `area` |
| `hist_year_range` | from,to |
| `include_drone` | `true` \| `false` |
| `custom_today` | date |
| `color_theme` | color_theme |

Example: `.../ground_prehatch_progress/?tabs=historical&facility=Sr&zone=1&hist_time_period=yearly&hist_display_metric=acres&hist_year_range=2018,2025`

### Structure Treatments — `/struct_trt/`

| Param | Options |
|---|---|
| `tabs` | `current` \| `historical` |
| `facility` | facility |
| `zone` | `1` \| `2` \| `1,2` \| `combined` |
| `fos` | fos |
| `group_by` | `facility` \| `foreman` \| `township` \| `mmcd_all` |
| `structure_type` | `(from DB)` |
| `time_period` | `yearly` \| `weekly` |
| `metric_yearly` | `treatments` \| `structures_count` \| `proportion` |
| `metric_weekly` | `weekly_active_treatments` |
| `chart_type_regular` | `stacked_bar` \| `grouped_bar` \| `line` \| `area` |
| `chart_type_prop` | `grouped_bar` \| `line` \| `pie` |
| `year_range` | from,to |
| `expiring_days` | number |
| `custom_today` | date |
| `status_types` | `D` \| `W` \| `U` (comma-separated) |
| `color_theme` | color_theme |

Example: `.../struct_trt/?tabs=historical&facility=Sr&time_period=yearly&metric_yearly=treatments&group_by=facility`

### SUCO History — `/suco_history/`

| Param | Options |
|---|---|
| `tabs` | `detailed` \| `graph` \| `map` \| `top_locations` |
| `facility` | facility |
| `zone` | `all` \| `1` \| `2` |
| `fos` | fos |
| `group_by` | `mmcd_all` \| `facility` \| `foreman` \| `species_name` |
| `species` | `(from DB)` |
| `graph_type` | `stacked_bar` \| `bar` \| `line` \| `point` \| `area` |
| `top_locations_mode` | `visits` \| `species` |
| `basemap` | `osm` \| `carto` \| `satellite` |
| `date_range` | range |
| `load_harborages` | `true` \| `false` |
| `color_theme` | color_theme |

Example: `.../suco_history/?tabs=graph&group_by=facility&graph_type=bar`

### Cattail Treatments — `/cattail_treatments/`

| Param | Options |
|---|---|
| `tabs` | `progress` \| `map` \| `historical` |
| `facility` | facility |
| `zone` | `p1` \| `p2` \| `separate` \| `combined` |
| `fos` | fos |
| `group_by` | `mmcd_all` \| `foreman` \| `facility` |
| `display_metric_type` | `sites` \| `acres` |
| `hist_status_metric` | `need_treatment` \| `treated` \| `pct_treated` |
| `progress_chart_type` | `line` \| `bar` \| `stacked` |
| `hist_chart_type` | `stacked_bar` \| `grouped_bar` \| `line` \| `area` |
| `year_range` | from,to |
| `analysis_date` | date |
| `basemap` | `carto` \| `satellite` \| `osm` |
| `color_theme` | color_theme |

Example: `.../cattail_treatments/?tabs=historical&facility=Sr&zone=p1&hist_status_metric=treated`

### Catch Basin Status — `/catch_basin_status/`

| Param | Options |
|---|---|
| `tabs` | `overview` \| `details` \| `historical` |
| `facility` | facility |
| `zone` | `1` \| `2` \| `1,2` \| `combined` |
| `fos` | fos |
| `group_by` | `facility` \| `foreman` \| `sectcode` \| `mmcd_all` |
| `time_period` | `yearly` \| `weekly` |
| `metric` | `treatments` \| `sites` |
| `metric_yearly` | `treatments` \| `total_count` |
| `metric_weekly` | `weekly_active_treatments` |
| `chart_type` | `stacked_bar` \| `grouped_bar` \| `line` \| `area` |
| `expiring_filter` | `all` \| `expiring` \| `expiring_expired` |
| `expiring_days` | number |
| `year_range` | from,to |
| `custom_today` | date |
| `color_theme` | color_theme |

Example: `.../catch_basin_status/?tabs=historical&facility=Sr&time_period=yearly&metric_yearly=treatments`

### Drone — `/drone/`

| Param | Options |
|---|---|
| `tabs` | `current` \| `historical` \| `map` \| `site_stats` |
| `facility` | facility |
| `zone` | `p1_only` \| `p2_only` \| `p1_p2_separate` \| `p1_p2_combined` |
| `fos` | fos |
| `group_by` | `facility` \| `foreman` \| `sectcode` \| `mmcd_all` |
| `display_metric` | `sites` \| `treated_acres` |
| `time_period` | `yearly` \| `weekly` |
| `hist_display_metric` | `sites` \| `site_acres` \| `treatment_acres` |
| `hist_chart_type` | `stacked_bar` \| `grouped_bar` \| `line` \| `area` \| `step` |
| `site_stat_type` | `average` \| `largest` \| `smallest` |
| `prehatch_only` | `true` \| `false` |
| `year_range` | from,to |
| `site_year_range` | from,to |
| `expiring_days` | number |
| `analysis_date` | date |
| `map_basemap` | `carto` \| `satellite` \| `osm` |
| `color_theme` | color_theme |

Example: `.../drone/?tabs=historical&facility=Sr&zone=p1_only&time_period=weekly&hist_display_metric=treatment_acres`

### Red Air Sites — `/air_sites_simple/`

| Param | Options |
|---|---|
| `main_tabset` | `Air Site Status` \| `Pipeline Snapshot` \| `Historical Analysis` (URL-encode spaces) |
| `facility` | facility |
| `zone` | `(from DB)` |
| `metric_type` | `sites` \| `acres` |
| `status` | `all` \| `Unknown` \| `Inspected` \| `Needs ID` \| `Needs Treatment` \| `Active Treatment` |
| `priority` | `(from DB)` |
| `material` | `(from DB)` |
| `volume_time_period` | `weekly` \| `yearly` |
| `process_status` | `Unknown` \| `Inspected` \| `Needs ID` \| `Needs Treatment` \| `Active Treatment` (comma-separated) |
| `hist_chart_type` | `(from DB)` |
| `hist_start_date` | date |
| `hist_end_date` | date |
| `analysis_date` | date |
| `larvae_threshold` | number |
| `bti_effect_days` | number |
| `show_polygons` | `true` \| `false` |
| `color_theme` | color_theme |

Example: `.../air_sites_simple/?main_tabset=Historical%20Analysis&facility=Sr&metric_type=acres`

### Inspections — `/inspections/` (two-stage: Load Data → Analyze)

| Param | Options |
|---|---|
| `tabs` | `gaps` \| `larvae` \| `red_bug_gaps` \| `analytics` |
| `facility` | facility |
| `zone` | `1` \| `2` \| `1,2` |
| `fos` | fos |
| `priority` | `(from DB)` |
| `air_gnd` | `A` \| `G` \| `both` |
| `drone_filter` | `drone_only` \| `no_drone` \| `include_drone` |
| `red_bug_group_by` | `facility` \| `fos` |
| `prehatch_only` | `true` \| `false` |
| `spring_only` | `true` \| `false` |
| `years_gap` | number |
| `years_red_bug_gap` | number |
| `years_back` | number |
| `larvae_threshold` | number |
| `min_inspections` | number |
| `color_theme` | color_theme |

Example: `.../inspections/?tabs=red_bug_gaps&facility=Sr&zone=1&years_red_bug_gap=5`

### Cattail Inspections — `/cattail_inspections/`

| Param | Options |
|---|---|
| `tabs` | `Progress vs Goal` \| `Historical Comparison` (URL-encode spaces) |
| `goal_year` | `(from DB)` |
| `goal_column` | `total` \| `p1` \| `p2` \| `separate` |
| `hist_zone` | `p1` \| `p2` \| `combined` \| `separate` |
| `hist_years` | number |
| `hist_facility` | facility |
| `hist_metric` | `sites` \| `acres` |
| `sites_view_type` | `all` \| `unchecked` |
| `custom_today` | date |
| `theme_historical` | color_theme (Historical tab) |
| `theme_progress` | color_theme (Progress tab) |

Example: `.../cattail_inspections/?tabs=Historical%20Comparison&hist_metric=acres&hist_years=5`

### Trap Surveillance — `/trap_surveillance/` (main map view)

| Param | Options |
|---|---|
| `metric_type` | `abundance` \| `infection` \| `vector_index` |
| `species` | `(from DB)` |
| `year` | `(from DB)` |
| `yrwk` | `(from DB)` |
| `infection_metric` | `mle` \| `mir` |
| `compare_mode` | `true` \| `false` |
| `yrwk_b` | `(from DB)` |
| `analysis_year` | `(from DB)` |
| `analysis_bandwidth` | number |
| `analysis_radius` | number |
| `color_theme` | color_theme |

Example: `.../trap_surveillance/?metric_type=vector_index&infection_metric=mle`

### Control Efficacy — `/control_efficacy/`

| Param | Options |
|---|---|
| `tabs` | `efficacy` \| `progress` \| `status_tables` |
| `facility` | facility |
| `comparison_mode` | `genus` \| `material` \| `dosage` |
| `genus` | `Both` \| `Aedes` \| `Culex` |
| `material_type` | `all` \| `bti` \| `methoprene` \| `spinosad` |
| `dosage` | `all` `(more from DB)` |
| `matcode` | `(from DB)` |
| `season` | `Spring` \| `Summer` (comma-separated) |
| `trt_type` | `Air` \| `Ground` \| `Drone` (comma-separated) |
| `use_mullas` | `true` \| `false` |
| `checkback_type` | `(from DB)` |
| `checkback_number` | number |
| `checkback_percent` | number |
| `year_range` | from,to |
| `color_theme` | color_theme |

Example: `.../control_efficacy/?tabs=progress&facility=Sr&year_range=2020,2025`

### Mosquito Monitoring — `/mosquito-monitoring/`

| Param | Options |
|---|---|
| `mm_tabs` | `All` \| `Compare` |
| `facility` | `All` `(or facility from DB)` |
| `species` | `(from DB)` |
| `years` | from,to |
| `facilityONE` | `All` `(or facility from DB)` |
| `speciesONE` | `(from DB)` |
| `zoneONE` | `2+X` \| `All` |
| `yearsONE` | from,to |

Example: `.../mosquito-monitoring/?mm_tabs=All&facility=Sr&years=2015,2025`

### Mosquito Surveillance Map — `/mosquito_surveillance_map/` (auto-updates)

| Param | Options |
|---|---|
| `species` | `(from DB)` |
| `survtype` | `Sweep` \| `CO2(reg)` \| `Gravid` \| `CO2(elev)` \| `All` |
| `date_range` | range |
| `labels` | `true` \| `false` |
| `color_theme` | color_theme |

Example: `.../mosquito_surveillance_map/?survtype=All&date_range=2025-05-01,2025-09-01`

### Section Cards — `/section-cards/` (filters + Generate Cards)

| Param | Options |
|---|---|
| `facility` | `all` `(or facility from DB)` |
| `zone` | `all` \| `1` \| `2` |
| `priority` | `all` \| `RED` \| `YELLOW` \| `GREEN` \| `BLUE` \| `ORANGE` \| `PURPLE` |
| `fosarea` | `all` `(or FOS from DB)` |
| `section` | `all` `(or section from DB)` |
| `towncode` | `all` `(or towncode from DB)` |
| `air_gnd` | `all` \| `A` \| `G` |
| `drone` | `all` \| `include` \| `exclude` \| `only` |
| `status_udw` | `all` \| `D` \| `W` \| `U` |
| `structure_type` | `(from DB)` |
| `site_type` | `breeding` \| `structures` |
| `cards_per_page` | `4` \| `6` \| `8` \| `10` \| `12` |
| `num_rows` | number |
| `double_sided` | `true` \| `false` |
| `split_by_section` | `true` \| `false` |
| `split_by_priority` | `true` \| `false` |
| `split_by_type` | `true` \| `false` |
| `use_webster` | `true` \| `false` |
| `autofill_history` | `true` \| `false` |
| `title_fields` | `(from DB)` (comma-separated) |
| `watermark_fields` | `(from DB)` (comma-separated) |

Example: `.../section-cards/?facility=Sr&zone=1&priority=RED&cards_per_page=6&site_type=breeding`

### Air Inspection Checklist — `/air_inspection_checklist/` (auto-loads on open)

Bespoke parser (drives the claims/employee feature); the view loads immediately
with no Refresh click needed.

| Param | Options |
|---|---|
| `facility` | `all` `(or facility from DB)` |
| `fos` | a FOS **shortname** `(from DB)` — note: this app keys the FOS filter by shortname, not the number |
| `priority` | `RED` (default) \| `YELLOW` \| `GREEN` \| `BLUE` \| `ORANGE` \| `PURPLE` `(from DB)`; comma-separated for multiple |
| `zone` | `1` (P1 Only) \| `2` (P2 Only) \| `1,2` (P1 and P2 Combined) |
| `lookback` | number `1`–`7` (days) |
| `emp` | an employee number `(from DB)` — pre-selects the claiming employee |
| `analysis_date` | date |
| `show_unfinished` | `true` \| `false` |
| `show_active_treatment` | `true` \| `false` |
| `theme` | color_theme |
| `autorefresh` | `true` \| `false` |

Example: `.../air_inspection_checklist/?facility=Sr&zone=1&priority=RED&lookback=3`

### FOS Capacity Planning — `/fos_capacity_planning/` (live, no Refresh click)

Estimates recompute reactively, so params apply and the view updates with no
button click. (Section promotion in the What-If planner is interactive and not
deep-linked.)

| Param | Options |
|---|---|
| `tabs` | `capacity` \| `staffing` \| `whatif` |
| `cap_facility` | `all` `(or facility from DB)` — Capacity tab facility |
| `cap_priority` | `true` \| `false` — break down by priority |
| `cap_drivetime` | `true` \| `false` — show est. drive time |
| `avg_mph` | number (Capacity tab drive-time speed) |
| `circuity` | number (road circuity factor) |
| `wi_facility` | a facility `(from DB)` — What-If planner facility |
| `wi_metric` | staffing driver id `(from DB)` — Estimate A |
| `wi_per_tech` | number (capacity per tech) |
| `wi_rate_year` | a year `(from DB)` — jobs/site season |
| `wi_rate_scope` | `DISTRICT` \| `FAC` |
| `wi_crew_hours` | number (hrs / crew-season) |
| `wi_crew_trips` | number (round trips/section/season) |
| `wi_avg_mph` | number (What-If drive-time speed) |
| `wi_circuity` | number (What-If circuity factor) |

Example: `.../fos_capacity_planning/?tabs=whatif&wi_facility=Sr&wi_rate_scope=FAC`

### Overview (its own params) — `/overview/`

| Param | Options |
|---|---|
| `view` | `district` \| `facility` \| `fos` |
| `metric` | a metric id (see `/v1/public/embed/metrics`) |
| `zone` | `1` \| `2` |
| `date` | date |

Example: `.../overview/?view=district`

---

## 4. Embed widgets & API

The embed API now accepts the **same params** as the deep-link URLs above, per
app. An embed URL mirrors a deep link. Simply, swap the app path for
`?app=<app>` on `/embed/` or `/v1/public/embed/chart`.

### Widget page (iframe-able) — `/embed/`

| Param | Options |
|---|---|
| `app` | an app id (see coverage table below) — **required** for v2 |
| `view` | the app's tab/view (e.g. `graph`, `historical`, `top_locations`) |
| `kind` | `chart` \| `statbox` (default `chart`) |
| *...filters* | any of that app's params from §3 (facility, zone, fos, group_by, graph_type, time_period, display_metric, …) — forwarded verbatim |

Examples:
- `http://localhost:3838/embed/?app=suco_history&view=graph&group_by=facility&graph_type=bar`
- `http://localhost:3838/embed/?app=ground_prehatch_progress&view=historical&facility=Sr&display_metric=acres`
- `<iframe src="http://localhost:3838/embed/?app=drone&view=historical" width="600" height="400" style="border:0"></iframe>`

### How it works (freshness)

- **Peek → compute → cache.** A request first checks Redis. Warm combo → instant
  (`source:"cache"`). Cold/rare combo → the API runs that app's own data code once,
  returns it (`source:"live"`), and caches it for a short TTL (~5 min) so repeats
  are instant. A public viewer never gets a 500 — failures/empties come back
  `status:"unavailable"` (HTTP 200) and the widget shows a soft message.
- Each chart returns **data series + resolved options** (JSON); the widget draws it
  with the resolved `graph_type` client-side.

### JSON API (no key, CORS `*`)

- `GET /v1/public/embed/metrics` — lists v1 metrics **and** the v2 `apps` catalog
  (each app's `views` + accepted `params`).
- `GET /v1/public/embed/chart?app=<app>&view=<view>&<filters…>`
- `GET /v1/public/embed/statbox?app=<app>&view=<view>&<filters…>` — the latest
  point of the first series as a tile.

Example: `http://localhost:3838/v1/public/embed/chart?app=suco_history&view=graph&group_by=facility&graph_type=bar`

**v1 back-compat:** the old `?metric=<id>&type=<avg>` form still works on both
`/embed/` and the JSON endpoints (serves the cache-only historical_averages series).

### App coverage (embed v2 producers)

| App | Views wired | Status |
|---|---|---|
| suco_history | `graph`, `top_locations` | ✅ done |
| ground_prehatch_progress | `historical` | ✅ done |
| drone | `historical` | ✅ done |
| catch_basin_status | `historical` | ✅ done |
| struct_trt | `historical` | ✅ done |
| cattail_inspections | `historical` | ✅ done |
| trap_surveillance, mosquito-monitoring | trend charts | ⏳ producer pending (series) |
| cattail_treatments | `historical` | ⏳ producer pending (aggregation inside its chart fn) |
| mosquito_surveillance_map, air_sites_simple | map (points) | ⏳ producer pending (`map` payload ready) |
| control_efficacy | boxplot | ⏳ producer pending (`boxplot` payload ready) |
| inspections | gap tables | ⏳ producer pending (`table` payload ready) |

**Payload kinds** (chosen: extend per type) — all four are built in the framework
and rendered by the widget: `series` (x/y, drawn by `graph_type`), `map`
(points `[{lat,lon,value,label}]` → OpenStreetMap), `boxplot` (per-group
min/q1/median/q3/max), `table` (columns + rows). Each response carries
`payload_type`. Remaining work is per-app **producers** that feed these shapes.

**Out of embed scope** (no graph to embed): section-cards, fos_capacity_planning,
air_inspection_checklist, and all legacy apps.

---

## 5. Known limitations

- Embed v2 is being rolled out per app (see coverage table); apps marked *in
  progress* still only answer the v1 metric-based embed.
- First view of a **cold** filter combo runs one live query (then cached) — not
  instant; warm combos are instant.
- Map / boxplot / gap-table apps don't fit the `{x,y}` series JSON yet — a payload
  shape for those is an open decision.
- `(from DB)` params (facility, FOS, species, years, some chart/priority/material
  filters) load their choices at runtime — the value list is whatever the app's
  dropdown currently shows; facility short codes are `all | E | N | Sj | Sr | Wm | Wp`.

