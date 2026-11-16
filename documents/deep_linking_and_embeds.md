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
| Inspections (Prehatch Coverage) | `/inspections/` | filters apply live, nothing to click |
| Cattail Inspections | `/cattail_inspections/` | 2 refresh buttons (per tab) |
| Trap Surveillance | `/trap_surveillance/` | all five pages (Map, Trend, Table, Causal Analysis, Methods) via `?tabs=`; map filters + Refresh Data |
| Checkback Efficacy | `/control_efficacy/` | single refresh, 4 tabs (incl. Bioassay Efficacy); route keeps the `control_efficacy` id |
| Mosquito Monitoring | `/mosquito-monitoring/` | Update button per tab (All / Compare) |
| Mosquito Surveillance Map | `/mosquito_surveillance_map/` | auto-updating map (no refresh click) |
| Section Cards | `/section-cards/` | filters + Generate Cards |
| Air Inspection Checklist | `/air_inspection_checklist/` | bespoke parser (claims/emp feature); auto-loads on open, now covers all filters + autorefresh |
| Capacity Planner | `/capacity_planner/` | live estimates (no refresh click); deep-links tab + facility + what-if inputs |


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

### Inspections (Prehatch Coverage, the 51% goal) — `/inspections/`

Filters apply live, so there is no refresh button to click.

| Param | Options |
|---|---|
| `tabs` | `progress` \| `sites` \| `rules` |
| `facility` | facility |
| `zone` | `1` \| `2` \| `1,2` |
| `fos` | foreman shortname, or the 4-digit area code |
| `priority` | `(from DB)` |
| `drone_filter` | `include_drone` \| `drone_only` \| `no_drone` |
| `status` | `not_counting` \| `counting` \| `id_pending` \| `all` (Site List) |
| `reason` | `all` \| `red_5yr` \| `sampled_this_year` \| `red_expired` \| `no_red_sampled` \| `inspected_no_sample` \| `not_inspected` (Site List) |
| `as_of` | date (`YYYY-MM-DD`), default today |
| `color_theme` | color_theme |

Old links keep working: `tabs=red_bug_gaps` opens the Site List (the Missing sites); `tabs=scorecard`, `gaps`, `analytics` and `larvae` open the Progress tab. (`status=not_counting` means the Missing sites, `status=counting` the Sampled ones and `status=id_pending` the sites waiting on a result; the first two URL values did not change when the labels did. `status=ending_soon` no longer exists.) The old `air_gnd`, `prehatch_only`, `spring_only`, `years_*`, `larvae_threshold` and `min_inspections` params are ignored (the app is now ground prehatch sites only).

Example: `.../inspections/?tabs=sites&status=not_counting&reason=inspected_no_sample&facility=Sr&zone=1`

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

### Trap Surveillance — `/trap_surveillance/` (every page)

| Param | Options |
|---|---|
| `tabs` | `map` \| `trend` \| `table` \| `analysis` \| `methods` (the menu names `map_tab`, `trend_tab`, `table_tab`, `trap_analysis_tab`, `methods_tab` also work) |
| `metric_type` | `abundance` \| `infection` \| `vector_index` |
| `species` | `(from DB)` |
| `year` | `(from DB)` |
| `compare_year` | `(from DB)` — overlays a second year on the Trend tab's vector index graph (omit for none) |
| `yrwk` | `(from DB)` |
| `infection_metric` | `mle` \| `mir` |
| `compare_mode` | `true` \| `false` |
| `yrwk_b` | `(from DB)` |
| `analysis_year` | `(from DB)` |
| `analysis_bandwidth` | number |
| `analysis_radius` | number |
| `color_theme` | color_theme |

Example: `.../trap_surveillance/?metric_type=vector_index&infection_metric=mle`

Link to a page: `.../trap_surveillance/?tabs=methods` (Methods), `.../trap_surveillance/?tabs=trend&year=2025&species=Total_Cx_vectors` (Trend); compare two years of vector index: `.../trap_surveillance/?tabs=trend&year=2026&compare_year=2025&species=Total_Cx_vectors&infection_metric=mle`. `map` and `table` click Refresh Data; `analysis` clicks Run Analysis (use with `analysis_year`, `analysis_bandwidth`, `analysis_radius`); `trend` and `methods` load nothing extra.

### Checkback Efficacy — `/control_efficacy/`

The app is labelled **Checkback Efficacy**; the route, the embed app id and all
param names keep the original `control_efficacy` spelling, so existing links and
embeds are unaffected.

Four tabs. The `efficacy` / `progress` / `status_tables` tabs measure **checkback**
efficacy (pre- vs post-treatment dip counts, `action='4'`). The `bioassay` tab
measures **bioassay** efficacy (pupal emergence tests, `action='P'`) and has its
own `bio_*` filters.

| Param | Options |
|---|---|
| `tabs` | `efficacy` \| `bioassay` \| `progress` \| `status_tables` |
| `facility` | facility (shared by all tabs) |
| `year_range` | from,to (shared by all tabs) |
| `color_theme` | color_theme |
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

Bioassay tab (`tabs=bioassay`) — every sidebar filter is a param:

| Param | Options |
|---|---|
| `bio_comparison` | `window` \| `material` \| `dosage` \| `genus` |
| `bio_genus` | `Both` \| `Aedes` \| `Culex` \| `Unknown` |
| `bio_season` | `Spring` \| `Summer` (comma-separated) |
| `bio_trt_type` | `Air` \| `Ground` \| `Drone` \| `Catch Basin` \| `None` (comma-separated, URL-encode spaces) |
| `bio_window` | `Within window` \| `Past expiry` \| `Untreated` (comma-separated, **URL-encode spaces**) |
| `bio_material_type` | `all` \| `bti` \| `methoprene` \| `spinosad` |
| `bio_dosage` | `all` `(more from DB)` |
| `bio_pending` | `true` \| `false` — include bioassays still awaiting an emergence count in the table |

Examples:
- `.../control_efficacy/?tabs=progress&facility=Sr&year_range=2020,2025`
- `.../control_efficacy/?tabs=bioassay&bio_comparison=material&bio_window=Within%20window&facility=Sr`
- `.../control_efficacy/?tabs=bioassay&bio_trt_type=Ground,Catch%20Basin&bio_pending=true`

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

### Capacity Planner — `/capacity_planner/` (live, no Refresh click)

Estimates recompute reactively, so params apply and the view updates with no
button click. The What-If scenario itself (the sections you select, move and promote) is too
big for individual params; it travels in one packed `s` parameter, made by the **Copy link to
this scenario** button (see "Sharing a What-If scenario" below).

| Param | Options |
|---|---|
| `tabs` | `capacity` \| `staffing` \| `whatif` |
| `avg_zone` | `p1` \| `p2` — P1/P2 box plot tab inside the Capacity tab |
| `cap_facility` | `all` `(or facility from DB)` — Capacity tab facility |
| `cap_priority` | `true` \| `false` — break down by priority |
| `cap_measure` | `both` \| `sites` \| `acres` — show site counts, acres, or both |
| `avg_metric` | a metric id, e.g. `ground_sites`, `ground_acres`, `air_sites`, `air_acres`, `prehatch_sites`, `prehatch_acres`, `sections`, `wet_cb`, `avg_miles` — box-plot metric in the Averages section |
| `cap_drivetime` | `true` \| `false` — show est. drive time |
| `cap_drive_mode` | `routed` \| `estimate` — Capacity tab drive-time source (routed OpenStreetMap roads vs straight-line estimate) |
| `cap_speed_pct` | number (field speed as % of map speed, routed mode) |
| `avg_mph` | number (Capacity tab drive-time speed, estimate mode) |
| `circuity` | number (road circuity factor) |
| `wi_facility` | a facility `(from DB)` — the facility whose staffing (techs per crew, drone offload) the What-If "Estimate settings" section edits |
| `wi_metric` | staffing driver id `(from DB)` — Estimate A |
| `wi_per_tech` | number (capacity per tech) |
| `wi_techs_per_crew` | number (techs per crew for the What-If facility) |
| `wi_drone_offload` | number 0-100 (% of the What-If facility's ground load drones take over) |
| `wi_drone_air_offload` | number 0-100 (% of the What-If facility's air-site inspections drones take over) |
| `wi_rate_year` | a year `(from DB)` — jobs/site season |
| `wi_rate_scope` | `DISTRICT` \| `FAC` — Estimate B job rates: district-wide, or each facility's own |
| `wi_crew_hours` | number (hrs / crew-season) |
| `wi_crew_trips` | number (round trips/section/season) |
| `wi_travel` | `true` \| `false` — include travel time in both estimates (default true) |
| `wi_drive_mode` | `routed` \| `estimate` — drive-time source for Estimate A and B |
| `wi_speed_pct` | number (field speed as % of map speed, routed mode) |
| `wi_travel_share` | number (percent of a tech's workday spent driving, default 20; Estimate A capacity factor) |
| `wi_avg_mph` | number (What-If drive-time speed, estimate mode) |
| `wi_circuity` | number (What-If circuity factor) |
| `wi_measure` | `both` \| `sites` \| `acres` — What changed: site counts, acres, or both |
| `wi_shade` | `facility` \| `density` \| `residents` — What-If map shading: facility color (default), 2020 census residents per sq mi, or residents per section. Saved in shared links |
| `wi_drill` | a facility `(from DB)` — facility broken down by FOS area in What changed / Estimate A |
| `wi_move_to` | `keep` \| a facility `(from DB)` — the "Move sections to" destination |
| `wi_h_air_trt`, `wi_h_air_insp`, `wi_h_gnd_insp_trt`, `wi_h_gnd_trt`, `wi_h_gnd_insp` | numbers (Estimate B hours per job) |
| `s` | packed scenario from Copy link (see below); not meant to be typed |

Example: `.../capacity_planner/?tabs=whatif&wi_facility=Sr&wi_rate_scope=FAC`

#### Sharing a What-If scenario

Build a scenario (promote and move sections, change settings, pan the map), press **Copy link
to this scenario**, and send the link. Whoever opens it lands on the What-If tab with the same
sections promoted or moved, the same settings and staffing table, and the same map view. They
get an **editable copy**: their changes are never saved to your link, and they can press Copy
link to make their own.

- **Nothing is stored on the server.** The link carries the scenario itself in `s`
  (base64url of gzipped JSON), so it never expires and survives rebuilds. A bitmap per
  destination over the sorted section list keeps it short: about 800 characters for a 286-section
  move with settings; 300 scattered sections about 500.
- **What it saves:** the promoted / moved sections and where they go, every What-If input in
  the table above (except the three per-facility staffing boxes, which come from the staffing
  table), the whole staffing table (crews, techs per crew, drone techs, offload %), the map
  centre and zoom, the date, and a data stamp (number of P1 / P2 sections).
- **Numbers are recalculated from the live database when the link is opened**, not frozen. If the
  P1 / P2 section counts differ from the stamp, a banner says the data changed since the link was
  made.
- **If the section list itself changed** (sections added or removed), the link says it can't be
  restored and the planner opens at its starting point, instead of putting changes on the wrong
  sections.
- Explicit URL params next to `s` win over what the link saved (e.g. `...&s=...&wi_metric=air_sites`).
- A damaged, truncated or tampered `s` shows "damaged or incomplete" and is otherwise ignored.
  It is only ever parsed as size-limited JSON (never unserialized), and every value is validated.
- Links over about 2,000 characters can be cut off by chat and email apps; the page warns, and the
  **Download changed sections (CSV)** button is the alternative.
- Plumbing: `shared/url_state_helpers.R` `wire_deep_links(expand_state = fn)` lets an app turn the
  parsed URL into a fuller state once before it is applied; capacity_planner uses it to restore
  the scenario (`expand_share()` in `app.R`; pack / token functions in `display_functions.R`).

### Admin Tools — `/test-app/` (nothing auto-loads)

| Param | Options |
|---|---|
| `tabs` | `overview` \| `cache` \| `runtime` \| `config` \| `registry` \| `claims` \| `colors` |
| `cache_tabs` | `historical` \| `lookup` \| `tiered` — inner tab of Cache Manager |
| `runtime_tabs` | `environment` \| `load_balancing` \| `access_log` — inner tab of Runtime & Routing |
| `colors_tabs` | `facilities` \| `status_colors` \| `treatment_plans` \| `species` \| `theme_preview` — inner tab of Color Reference |

Example: `.../test-app/?tabs=cache&cache_tabs=tiered`

### Trap Surveillance (test) — `/trap_survillance_test/` (nothing auto-loads)

| Param | Options |
|---|---|
| `tabs` | `map_sf` \| `table` |

Example: `.../trap_survillance_test/?tabs=table`

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
- `http://localhost:3838/embed/?app=trap_surveillance&view=map&metric_type=vector_index&infection_metric=mle` (VI-area choropleth)
- `http://localhost:3838/embed/?app=trap_surveillance&view=vector_index&year=2026&compare_year=2025&species=Total_Cx_vectors&infection_metric=mle` (district vector index, 2026 vs 2025 by epiweek; `compare_year` applies to the `vector_index` view only)
- `http://localhost:3838/embed/?app=air_sites_simple&view=pipeline&metric_type=acres` (process-summary table)
- `http://localhost:3838/embed/?app=cattail_treatments&view=historical&hist_status_metric=treated&group_by=foreman`
- `http://localhost:3838/embed/?app=control_efficacy&view=bioassay&bio_comparison=window` (bioassay % inhibition)


---
