# Inspections App – Technical Notes (Prehatch Coverage, the 51% goal)

## What this app answers

The district goal: **at least 51% of ground PREHATCH sites should be Sampled**, where a site is Sampled if it has
a **red-bug sample in the last 5 years OR was sampled at all in the current year**. Every other site is **Missing**.
The app shows the current percentage, how many more sites are needed, and – for every Missing site – why
(how often it was inspected, how often it is wet, when the last sample and last red bug were).

The Overview tile `prehatch_coverage` is computed by the same code, so the two always agree.

### The words: "Sampled", "Missing" and "ID pending"

These are provisional and may be renamed. They are three constants in `coverage_functions.R`
(`COVERAGE_LABEL_COUNTING` = "Sampled", `COVERAGE_LABEL_NOT_COUNTING` = "Missing",
`COVERAGE_LABEL_ID_PENDING` = "ID pending"); every label, chart legend,
table header and sentence the app builds uses them, so **one edit renames them everywhere in the app**.
Not covered: the Overview tile's strings are plain text in `apps/overview/metric_registry.R`
(`detail_boxes`, `chart_labels`, `filter_info`), and `documents/deep_linking_and_embeds.md` / this file. Internal
names (`counts`, `not_counting`, the `status=` URL values) are deliberately NOT renamed, so links keep working.

## The rule (as of date D)

| Status (label shown) | Group | Meaning |
|---|---|---|
| `red_5yr` (Red bug in last 5 yrs) | Sampled | Sample with `redblue='R'` on an inspection in `[D − 5y, D]` (window start inclusive) |
| `sampled_this_year` (Sampled this year (temporary)) | Sampled, until Dec 31 | Any inspection in D's calendar year produced a sample number, any result. Drops to Missing Jan 1 |
| `red_expired` (Last red bug over 5 yrs ago) | Missing | Has a red bug, but older than 5 years |
| `no_red_sampled` (Sampled before this year, never red) | Missing | Sampled in the last 5 years (not this year), never red |
| `inspected_no_sample` (Inspected, no sample taken) | Missing | Inspected in the last 5 years, no sample ever taken |
| `not_inspected` (Not inspected in 5 yrs) | Missing | No inspection in the last 5 years |

**ID pending** (`id_pending`, a flag on top of the status, not a status of its own): the site has a sample taken this
year with no `redblue` result entered yet (`pending_this_year > 0`). That is all it means – whatever else the site has,
including a red bug from an earlier year. Once the result is entered the sample is just red or blue and the site is no
longer ID pending: red keeps it Sampled for 5 years, blue / no bugs keeps it Sampled until Dec 31 and then Missing.
Pending sites count toward the goal, so they are Sampled *and* ID pending. Today (2026-10-27): 1,019 sites are ID
pending – 388 of the 530 `sampled_this_year` sites plus 631 sites whose status is `red_5yr` (a red bug from an earlier
year, with a new sample still waiting). The other 142 `sampled_this_year` sites already have a blue / no-bugs result and
are not ID pending. (An earlier version limited ID pending to the 388 and called the 631 "never ID pending"; that hid
samples that really are waiting, so it was dropped.) This is *not* "drops within 90 days": until Dec 31 every
`sampled_this_year` site is within 90 days, including the 142 whose result is back. For 49 of the 1,019 the *latest*
sample already has a result and an older one is the one waiting, so the table has a "Samples ID pending" column.

Precedence: `red_5yr` > `sampled_this_year` > `red_expired` > `no_red_sampled` > `inspected_no_sample` > `not_inspected`.
`counts_through` is the last day a site keeps counting: the later of (red bug date + 5 years) and (Dec 31 if
sampled this year). `drops_because` says which of the two ends it.

**Denominator:** ground sites (`air_gnd='G'`) still active on D (`enddate IS NULL OR enddate > D`) with
`prehatch IN ('PREHATCH','PELLET','BRIQUET','PRE1ONLY')`. BTI-only sites are not prehatch sites. Drone sites are
included. (Verified 2026-10-27: 16,187 sites – 15,717 PREHATCH, 357 PRE1ONLY, 113 BRIQUET; no PELLET or BTI on
active ground sites, so this equals the old `prehatch IS NOT NULL` count today.)

**Window arithmetic** uses `lubridate::%m-%` in R and the resulting date is sent to SQL, so Feb 29 rolls to
Feb 28 (no NA) and R and Postgres can't disagree.

## Database Tables & Entities

| Table | Key Columns Used |
|---|---|
| `loc_breeding_sites` | `sitecode`, `enddate`, `air_gnd`, `priority`, `drone`, `acres`, `prehatch` |
| `gis_sectcode` | `sectcode`, `facility`, `fosarea`, `zone` |
| `dblarv_insptrt_current` | `sitecode`, `inspdate`, `action`, `wet`, `sampnum_yr` |
| `dblarv_insptrt_archive` | Same columns as current |
| `dblarv_sample_current` | `sampnum_yr`, `redblue` |
| `dblarv_sample_archive` | `sampnum_yr`, `redblue` |

`current` and `archive` do not overlap (checked by `pkey_pg`), so they are combined with `UNION ALL`.
Each inspection is joined to the sample table of the same age (current↔current, archive↔archive).

### Action codes used

| Use | Rule |
|---|---|
| Counts as an inspection (`insp_5y`, `last_insp_date`) | `action IN ('1','2','4') OR (action='3' AND wet='0')` (the pre-rewrite rule) |
| Wet frequency (`wet_n`, `wet_count`, `last_wet_date`) | `action IN ('1','2','4')` and `wet` in `0-9`,`A`. Dry = `0`, wet = `1-9`/`A`. `S` and NULL are ignored. Treatments (action 3) are excluded so treated wet sites don't inflate the % |
| Can carry a sample (red bug / sampled this year) | `action IN ('1','2','4')`, **or** `action = 'P' AND numdip > 0` (`.sample_action_sql()`); in every case `sampnum_yr IS NOT NULL` |

**Action `P` (pupae bioassay) counts as a sample only when it has a sample number AND `numdip` (larvae per dip) > 0.**
A bioassay that found nothing says nothing about the site. A NULL `numdip` is not > 0, so it is ignored. A `P` row does
not count as an *inspection* (visits / wet % / last inspected are unchanged) – only as a sample. Its result comes from
`dblarv_sample_*.redblue` when a lab row exists; most bioassays have none (only 16 of 479 scored 2026 bioassays had a
sample row when last measured). That is treated like any other sample whose result has not been entered yet – ID
pending, "Not entered yet" in the table – because the result is expected to be entered eventually. (A separate
"pupae bioassay" label was tried and removed.)
Effect measured 2026-10-27: 3 sites moved from Missing to Sampled (9,311 → 9,314 of 16,187; 57.5% either way);
0 sites changed red-bug status. Action `B` has samples but only on non-prehatch sites.

## SQL (`load_prehatch_site_coverage` in `coverage_functions.R`)

One query, one row per denominator site, all facilities/zones (filters are applied in R so one cached copy serves
everyone and the Overview):

```sql
WITH den AS (            -- denominator
  SELECT b.sitecode, sc.facility, sc.fosarea, sc.zone, b.priority, b.drone, b.acres, b.prehatch
  FROM loc_breeding_sites b
  INNER JOIN gis_sectcode sc ON left(b.sitecode,7) = sc.sectcode
  WHERE (b.enddate IS NULL OR b.enddate > :D) AND b.air_gnd = 'G' AND b.prehatch IN (...)
),
raw_visits AS (         -- 5-year window, current UNION ALL archive, each LEFT JOIN its sample table
  SELECT i.sitecode, i.inspdate, i.action, i.wet, i.numdip, i.sampnum_yr, s.redblue
  FROM dblarv_insptrt_current i LEFT JOIN dblarv_sample_current s USING (sampnum_yr)
  WHERE i.inspdate BETWEEN :window_start AND :D AND i.sitecode IN (SELECT sitecode FROM den)
  UNION ALL ... archive ...
),
flagged AS (...),       -- is_insp / is_visit / is_wet / is_sample / this_year flags
agg AS (                -- per-site COUNT(*) FILTER / MAX(...) FILTER, last_sample_result via array_agg
  ...
),
older_red AS (          -- most recent red bug BEFORE the window, so expired sites show when they last had one
  SELECT i.sitecode, MAX(i.inspdate) ... FROM dblarv_insptrt_archive i JOIN dblarv_sample_archive s ...
  WHERE s.redblue='R' AND i.action IN ('1','2','4') AND i.inspdate < :window_start
)
SELECT den.*, agg.*, older_red.last_red_older FROM den LEFT JOIN agg ... LEFT JOIN older_red ...
```

Takes ~4-5 s (the pre-rewrite Overview fast path took ~2 s but could not answer "why").

## R-Side Logic

| File | Contents |
|---|---|
| `coverage_functions.R` | The rule. `load_prehatch_site_coverage()` (DB), `classify_site_coverage()` (**pure**, unit-tested), `summarise_coverage()`, `aging_out_by_month()`, `get_coverage_overview_counts()`, `add_fos_names()`, goal/threshold helpers |
| `data_functions.R` | Entry point other code sources. Defines the `load_raw_data()` dispatcher |
| `legacy_functions.R` | Pre-rewrite analyses, moved unchanged, still called by embeds / API / LLM (see below) |
| `display_functions.R`, `ui_helper.R`, `app.R` | The web app |

Goal and warning percentages come from `config/app_config.yaml` (`thresholds.fixed_pct.prehatch_coverage`:
51 / 41) via `get_config_threshold()`; fallback 51 / 41.

`summarise_coverage()` returns exact `pct`, `needed` (= `ceil(goal·total/100) − counting`, with an epsilon so
0.51 × 100 isn't 52) and `cushion`. Percent is *truncated* to 1 decimal for display (50.96 → 50.9) so a miss is never
shown as 51.

`likely_to_yield`: High ≥ 50% wet, Medium ≥ 20%, Low < 20%, Unknown with fewer than 2 wet readings.

## Who else uses this folder (keep these contracts)

| Caller | Uses | Contract |
|---|---|---|
| Overview (`apps/overview/data_functions.R`) | `load_raw_data(..., status_types = character(0))` | `list(sites = df(facility, fosarea, zone, total_count, active_count, expiring_count), treatments = df(), pre_aggregated = TRUE)`. Must keep `...` and the `facility_filter` formal (Overview checks `formals()`). `active_count` = counting, `expiring_count` = not counting |
| Embed producers (`shared/embed_producers.R`) | `load_raw_data()` (plain data frame), `get_inspection_gaps_from_data`, `get_wet_frequency_from_data`, `get_high_larvae_sites_from_data`, `get_red_bug_gaps`, `get_red_bug_all_sites`, `get_red_bug_facility_analysis`, `get_red_bug_fos_analysis` | Names, args and output columns unchanged |
| API (`api/routes/inspections.R`) | `get_red_bug_gaps`, `get_red_bug_all_sites`, the two `*_analysis` functions, `load_raw_data` | Same. `/larval` is already broken (expects `$sites`; gets a data frame) – not changed here |
| LLM (`llm/tool_registry.py`) | `get_red_bug_gaps`, `get_red_bug_gaps_by_group` via the API | Same |

All callers `source()` `data_functions.R` with `chdir = TRUE` into a private environment, which is why it sources its
siblings with `local = TRUE`.

**The legacy red-bug functions use a different definition** – "no red bug in N years", `prehatch IS NOT NULL`, no
this-year clause – so their numbers will not match the 51% figure. A coverage API endpoint / LLM tool built on
`load_prehatch_site_coverage()` is the natural follow-up.

## Display

- **Progress** (tab value `progress`; was "Scorecard" – `?tabs=scorecard` still opens it): exact %, "N more sites need to
  be Sampled" (or spare), three boxes that split every site exactly once (Sampled with nothing pending / ID pending /
  Missing; on 2026-10-27 8,295 + 1,019 + 6,873 = 16,187, and Sampled + ID pending = the 9,314 in the headline), reason
  bar chart (click a bar → Site List), progress by facility or FOS with the goal line, and "when Sampled sites drop to
  Missing" by month (red bugs ageing out vs this-year samples resetting Jan 1). The red-bug vs this-year-only split is
  in the facility/FOS chart, not the boxes. The ID pending box uses the theme's `in_lab` ("lab / needs ID")
  status color and a microscope icon, as Needs ID does in `air_sites_simple`; it is deliberately not the amber
  `planned` color, which "Inspected, no sample taken" uses. The Site List "ID pending sites" filter uses the same flag,
  so box count = list rows, sorted oldest sample first. (This box was "Sampled, but drops to Missing within 90 days"
  until 2026-10-28; that definition was dropped because it was wrong for sites whose result is already in.)
- **Progress by facility / FOS:** listed most complete first (highest % at the top; ties by sites still needed, then
  name). The plot area is sized from the number of bars (`group_chart_height()`, via `uiOutput("group_chart_ui")`) –
  a fixed 400px `plotlyOutput` clipped the FOS view (~40 bars need ~1,300px).
- **Site List:** filter by status and reason; DT sorted so sites most likely to yield a sample come first; CSV download.
  The date column is headed "Sampled through" (last day the site stays Sampled).
- **How It's Counted:** plain-language rule, built from the same label constants.
- Status colors come from the selected theme via `get_status_colors()`.

## Shapefiles

**None.** No `.shp`, `.gpkg`, or PostGIS geometry used in this app.

## Shared Functions Used

| Function | Source File |
|---|---|
| `get_db_connection()`, `safe_disconnect()` | `shared/db_helpers.R` |
| `build_sql_in_list()` | `shared/db_helpers.R` |
| `get_foremen_lookup()`, `get_foreman_choices()` | `shared/db_helpers.R` |
| `get_facility_lookup()`, `get_facility_choices()`, `map_facility_names()` | `shared/db_helpers.R` |
| `get_priority_choices()`, `get_universal_text_css()`, `get_status_colors()`, `export_csv_safe()` | `shared/db_helpers.R` |
| `set_app_name()` | `shared/db_pool.R` |
| `get_theme_choices()` | `shared/color_themes.R` |
| `get_config_threshold()` | `shared/config.R` |
| `cached_call()`, `make_sitecode_link()` | `shared/server_utilities.R` |
| `create_stat_box()`, `resolve_box_color()` | `shared/stat_box_helpers.R` |
| `accessible_page()`, `accessible_main_panel()` | `shared/accessibility_helpers.R` |
| `wire_deep_links()`, `url_state_js()` | `shared/url_state_helpers.R` |

## Tests

`tests/apps/test-inspections.R` – DB-free tests of the real `coverage_functions.R` and `display_functions.R` (every
status, the inclusive window edge, Feb 29, precedence, goal arithmetic, month bucketing, the bioassay SQL condition,
the ID pending flag and its wording, renaming via the label constants, chart ordering and height, table headers and
sort order, the Overview contract with a stubbed loader). The live app session itself is not covered by an automated
test.
