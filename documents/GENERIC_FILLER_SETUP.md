# Sync Google Sheets with Webster (`generic_filler.gs`)

> Auto-fills any Google Sheet from the MMCD Metrics API, highlights rows by
> rule, and syncs employee claims across every sheet using it — all from one
> CONFIG block. No code to write, no matter the sheet's layout.

This replaces the old per-script setup docs (`inspection_filler.gs` /
`INSPECTION_FILLER_SETUP.md`). `generic_filler.gs` can do everything that
script did and more — one script, one doc, works for any sheet.

---

## Basic CSV Import Workflow

1. Export a CSV from a QGIS layer (or wherever your sitecode list comes from).
2. Open Google Sheets and create a blank spreadsheet.
3. **File → Import → Upload**, select the CSV.
4. Add a header row if you don't have one — row 1 is always skipped by the
   script (`DATA_START: 2`).
5. Edit headers, tab layout, and formatting as needed.

## Script Setup

1. Open the spreadsheet → **Extensions → Apps Script**.
2. Delete anything already in `Code.gs`.
3. Paste in the entire script from
   [`apps_script/generic_filler.gs`](https://github.com/mmcdmn/mmcd_metrics/blob/main/apps_script/generic_filler.gs).
4. Save (Ctrl+S).
5. **Project Settings (⚙ gear icon) → Script Properties** → add:

   | Property | Value |
   |---|---|
   | `API_BASE` | `https://metrics.mmcd.org/v1` |
   | `API_KEY` | `mmcd-sheets-abc123xyz` |

   This is the shared internal key for all MMCD sheets — not secret, use this
   exact value.

## First Run

1. Edit the `CONFIG` block at the top of the script to match your sheet (see
   reference tables below).
2. Select **`refreshData`** from the function dropdown and click **▶ Run**.
3. First time only: Google asks for permissions → Review permissions → pick
   your account → Advanced → Go to (project name) → Allow.
4. Check the **Execution log** for per-tab results:
   ```
   Tab "Sheet1": 42 sites. Claims: +2 -0 ↓1
   refreshData complete: 2:41:03 PM
   ```

## Auto-Refresh (Optional)

- Run **`setupAutoRefresh`** once to refresh automatically, every
  `REFRESH_MINUTES` minutes.
- Run **`removeAutoRefresh`** to stop it.

## Clear All Claims (Reset)

Run **`clearAllClaimsFromSheets`** to wipe every open (uninspected) claim
across all tabs and remove them from Redis. Inspected rows are never touched.
Run `refreshData` once afterward to confirm everything's back in sync.

---

## What the Script Does

On every `refreshData()` run, per tab (skipping anything in `SKIP_TABS`):

1. Reads every sitecode starting at `DATA_START`.
2. Forces the sitecode column — and every column it writes to — to **plain
   text format**, so Sheets can never silently reinterpret a value as a
   number, date, or scientific notation (see [Known Gotchas](#known-gotchas)).
3. Calls `API_ENDPOINT` with `FACILITY`, `ACTIONS`, and `LOOKBACK_DAYS`.
4. Writes every `mode: 'value'` column from the API response.
5. Colors every `mode: 'highlight'` column where its rule matches, clears it
   where it doesn't.
6. Syncs `CLAIM_COL` two-way with Redis, so a claim typed on one sheet shows
   up on every other sheet pointed at the same site. Once a site is filled,
   the real inspector's number replaces the claim automatically.
7. Clears every value/highlight column for a site once it ages out of
   `LOOKBACK_DAYS` — except columns listed in `PRESERVE_COLS`.
8. Turns every real sitecode into a clickable link to Webster.

---

## CONFIG Reference

| Key | Meaning | Default |
|---|---|---|
| `API_ENDPOINT` | Which API endpoint to call. See [Endpoints](#endpoints-this-script-can-use) below — not every endpoint works here. | `/private/site-inspections` |
| `FACILITY` | Limit results to one facility: `E`, `N`, `Sj`, `Sr`, `Wm`, `Wp`. **Always set this.** Blank pulls all ~86,000 active sites district-wide. | `''` |
| `ACTIONS` | Action codes to keep, e.g. `[9]` or `[1,2,3]`. Required for `/private/site-inspections`. | `[]` |
| `LOOKBACK_DAYS` | How many days back the API searches. A site with no record in this window is treated as open and its columns clear (except `PRESERVE_COLS`). | `14` |
| `REFRESH_MINUTES` | Interval used by `setupAutoRefresh`. Does nothing until that's run. | `1` |
| `FILLED_FIELD` | API field that marks a site "done." `was_completed` for `/private/site-inspections`, `was_inspected` for `/private/air-checklist`. | `was_completed` |
| `DATA_START` | First row that may hold a sitecode. Row 1 is always a header and is always skipped. | `2` |
| `SITECODE_COL` | Column letter holding sitecodes. | `'A'` |
| `SKIP_TABS` | Tab names to ignore entirely. `Summary` is always skipped in addition to this list. | `['Summary','Config','Template','Instructions']` |
| `SKIP_ROW_PATTERN` | Regex for divider/label rows (town names, "Totals", etc.) that aren't real sitecodes. | see script |
| `SITECODE_PATTERN` | What a real sitecode looks like. A row matching neither this nor a known API sitecode is skipped. | `/^\d{6}[A-Z]?-?\d+$/i` |
| `ENABLE_CLAIMS` | Turn the claim workflow on/off. | `true` |
| `CLAIM_COL` | Column where an emp# is typed to claim an open site. | `'D'` |
| `CLAIM_COLOR` | Background color while claimed but not yet inspected. | `'#FFF2CC'` |
| `PRESERVE_COLS` | Columns that are **never** cleared, no matter how old a site gets. For manually-typed notes. | *(set per sheet)* |
| `COLUMNS` | The list that actually fills/colors cells — see below. | `[]` |
| `TAB_OVERRIDES` | Per-tab overrides of any key above. | `{}` |

---

## `COLUMNS` Reference

Every entry is one of two modes.

### `mode: 'value'`

```js
{ mode: 'value', source: 'inspdate', col: 'C', format: 'date', always: false }
```

- **`source`** — an API field name (list below).
- **`col`** — the column letter to write into.
- **`format: 'date'`** — optional, writes a clean date string.
- **`always: true`** — keep the value even on an open site with no record yet
  in the lookback window. Use this for **SITE fields** (below); leave it off
  for inspection results so they correctly clear when a site ages out.

**SITE fields** — on `/private/site-inspections`, always present, even for a
site with no record at all. Pair with `always: true`:

| Field | Meaning |
|---|---|
| `site_acres` | Acres from the site record. Use this, not `acres` below — `acres` is the inspection record's own acres field and is populated on 0% of recent ground visits. |
| `air_gnd` | The site's standing air/ground designation (`A` or `G`). |
| `priority` | `RED` / `ORANGE` / `YELLOW` / `BLUE` / `GREEN` / `PURPLE`. |
| `site_type` | Site type code. |
| `site_remarks` | The site's standing notes — access, owner, permissions. **This is what you want for "remarks."** `rems1`/`rems2` below are per-visit notes, populated on under 1% of ground visits. |

**INSPECTION fields** — blank until the site has a record in the lookback
window:

| Field | Meaning |
|---|---|
| `inspdate` | Date of the inspection or treatment. |
| `numdip` | Larvae per dip (can be fractional, e.g. `0.7`). |
| `wet` | Percent-wet code (`0`–`9`, `A`, `S`). |
| `emp1` / `emp2` | Primary / secondary employee number. |
| `matcode` | Material/product code (treatments). |
| `amts` | Amount applied (treatments). |
| `acres` | Inspection record's own acres — almost always empty, use `site_acres`. |
| `acres_plan` | Planned acres (action `9` only). |
| `airgrnd_plan` | Air/ground plan code for that visit. |
| `sampnum_yr` | Sample number/year. |
| `posttrt_p` | Post-treatment flag (checkbacks). |
| `reinspect` | Reinspect flag. |
| `rems1` / `rems2` | Per-visit remarks — almost always empty, use `site_remarks`. |
| `comments` | Free-text comments field. |
| `action` | The action code itself (`'1'`, `'9'`, `'D'`, etc). |
| `activeTrt` | True if the material is still within its effect window (uses that material's own `effect_days` — if you need a fixed window instead, e.g. exactly 30 days regardless of material, use `LOOKBACK_DAYS` + a highlight rule on `action` instead; see example below). |

### `mode: 'highlight'`

```js
{ mode: 'highlight', color: '#C6EFCE', cols: null,
  rule: { field: 'numdip', op: '>=', value: 2 } }
```

- **`color`** — hex color applied when the rule matches.
- **`cols`** — array of column letters to color, or `null` for every `value`
  column (the "whole row").
- **`rule`** — `{ field, op, value }`. `op` is `>`, `<`, `=`, `>=`, `<=`, or
  `in`. `in` matches when the field equals **any** entry of `value` (an
  array) — e.g. `{ field: 'action', op: 'in', value: ['1','3'] }` for
  "action is 1 or 3." Omit `rule` entirely to highlight whenever the site has
  a record at all.

**Multiple highlight entries can target the same cell.** For each cell, the
**last** entry whose rule matches wins; the cell only clears if **no** entry
matched it. This is what lets you do "whole row green, except the dip column
turns red" — give the green rule `cols: null` and the red rule
`cols: ['D']`; D ends up red when both match, every other value column stays
green.

---

## Claiming Behavior

- Typing an emp# into `CLAIM_COL` on an open site pushes a claim to Redis —
  every other sheet pointed at the same site sees it immediately.
- Only the `CLAIM_COL` cell gets `CLAIM_COLOR`, and only while the site is
  claimed but not yet inspected.
- **To show who actually did the work once a site is inspected:** add a
  `value` entry whose `col` matches `CLAIM_COL`, with `source: 'emp1'`:
  ```js
  CLAIM_COL: 'B',
  COLUMNS: [
    { mode: 'value', source: 'emp1', col: 'B' },   // same column as CLAIM_COL
    ...
  ```
  The script automatically treats this column as claim-owned: your typed
  claim is never blanked by the value-fill step, and once the site is filled,
  the API's real `emp1` replaces it.

---

## `TAB_OVERRIDES`

Any top-level `CONFIG` key can be overridden for one tab, by its exact name
(case-sensitive):

```js
TAB_OVERRIDES: {
  'Sheet2': {
    COLUMNS: [
      { mode: 'value', source: 'inspdate', col: 'F', format: 'date' },
      { mode: 'value', source: 'numdip',   col: 'D' },
    ],
  },
},
```

**A `COLUMNS` override replaces the *entire* array for that tab.** You can't
override just one entry — list everything that tab still needs, with only
the part you're changing actually different. Everything else (`CLAIM_COL`,
`FACILITY`, `ACTIONS`, …) still falls back to the top-level `CONFIG` unless
you also name it in the override object.

---

## Example Configurations

### Simple: inspections + treatments for one facility

```js
API_ENDPOINT:  '/private/site-inspections',
FACILITY:      'Sr',
ACTIONS:       [9],
LOOKBACK_DAYS: 14,
FILLED_FIELD:  'was_completed',
ENABLE_CLAIMS: true,
CLAIM_COL:    'B',
PRESERVE_COLS: [],
COLUMNS: [
  { mode: 'value', source: 'emp1',         col: 'B' },
  { mode: 'value', source: 'site_acres',   col: 'C', always: true },
  { mode: 'value', source: 'priority',     col: 'D', always: true },
  { mode: 'value', source: 'inspdate',     col: 'E', format: 'date' },
  { mode: 'value', source: 'site_remarks', col: 'F', always: true },
],
```

### Ground inspections + treatments, forced 30-day window, two-tier highlight

Row turns green when the last visit was a treatment; the dip-count cell
turns red on top of that if dip count is 2 or higher.

```js
API_ENDPOINT:  '/private/site-inspections',
FACILITY:      'Sr',
ACTIONS:       [1, 2, 3],     // 1 = ground insp & treat, 2 = ground inspection, 3 = ground treatment
LOOKBACK_DAYS: 30,             // forces exactly 30 days, regardless of each material's own effect_days
FILLED_FIELD:  'was_completed',
ENABLE_CLAIMS: true,
CLAIM_COL:    'B',
PRESERVE_COLS: [],
COLUMNS: [
  { mode: 'value', source: 'emp1',       col: 'B' },
  { mode: 'value', source: 'site_acres', col: 'C', always: true },
  { mode: 'value', source: 'numdip',     col: 'D' },
  { mode: 'value', source: 'matcode',    col: 'E' },
  { mode: 'value', source: 'amts',       col: 'F' },
  { mode: 'value', source: 'site_remarks', col: 'G', always: true },

  { mode: 'highlight', color: '#C6EFCE', cols: null,
    rule: { field: 'action', op: 'in', value: ['1', '3'] } },

  { mode: 'highlight', color: '#FFC7CE', cols: ['D'],
    rule: { field: 'numdip', op: '>=', value: 2 } },
],
```

---

## Known Gotchas

- **Row 1 is always skipped.** `DATA_START: 2` means a sitecode in row 1
  never gets read, with no error — it just silently never fills in.
- **Leave `FACILITY` blank and every run pulls ~86,000 rows.** Always set it
  unless you specifically need the whole district.
- **A `COLUMNS` override in `TAB_OVERRIDES` replaces the whole array**, not
  just the entry you changed.
- **`acres` and `rems1`/`rems2` are almost always empty.** Use `site_acres`
  and `site_remarks` instead — see the field tables above.
- **The API returns only the single most recent matching record per site.**
  If a site has two separate events in the window (say, an inspection on one
  day and a treatment on another), you only see the later one — the earlier
  record's fields are gone from that row entirely. Action `1` ("ground insp &
  treat") covers the common single-visit case where this doesn't matter.
- **Stacking two highlight rules isn't automatically "OR."** A cell only
  clears if *no* rule matched it that row — use `op: 'in'` for a true OR
  across multiple values on one field.
- **`CLAIM_COL`'s background always resets once a site is filled**, even if
  a highlight rule with `cols: null` also targets that column. This is
  expected — the claim-color logic runs after the general highlight logic
  and intentionally owns that one cell.
- **`/private/cattail-checklist` does not work with this script.** It
  returns `{inspections, reinspects, sites}`, not one row per site — every
  column would stay blank. That endpoint is for `apps_script/cattail_filler.gs`
  only.
- **`/private/air-checklist` uses entirely different field names** than
  `/private/site-inspections` (`dip_count` not `numdip`, `pct_wet` not `wet`,
  `inspector_name` not `emp1`, `last_insp_date` not `inspdate`, and
  `FILLED_FIELD: 'was_inspected'` not `'was_completed'`). Don't mix the two
  field-name sets.

---

## Troubleshooting

| Problem | Fix |
|---|---|
| "Missing Script Property: API_BASE" / "API_KEY" | Project Settings → Script Properties → add both (see Script Setup above). |
| No data appearing at all | Check the Execution log for "API error ___". Confirm `API_ENDPOINT`, `ACTIONS`, and `FACILITY` are right for what you're trying to track. |
| A column just never fills | Check the `source` name against the field table for your endpoint — the two endpoints use different field names. |
| A date or number looks wrong (e.g. a 1899 date in a number column) | Old formatting on that cell. Re-paste the current script — `writeCol_` now forces plain text before every write — then re-run `refreshData`. |
| Sitecode turned into a huge number and its link broke | Same cause as above — re-paste the current script. The corrupted cell will need to be manually re-typed once; the original text is gone. |
| Claims not syncing | Confirm `ENABLE_CLAIMS: true`, `CLAIM_COL` is set, and the API is reachable. |
| Auto-refresh not running | Re-run `setupAutoRefresh()`. Check the clock icon (Triggers) in the Apps Script sidebar. |
| Values clearing sooner than expected | Increase `LOOKBACK_DAYS`. |
| Highlight color won't show / wrong rule wins | Check `cols` on each highlight entry — `null` means every value column, so two rules with `cols: null` will always have the later one win outright, not blend. |

---

## Endpoints This Script Can Use

| Endpoint | Use for | `FILLED_FIELD` | Notes |
|---|---|---|---|
| `/private/site-inspections` | Any action code(s) — this is the one to default to | `was_completed` | Requires `ACTIONS`. Returns the SITE fields and INSPECTION fields documented above. |
| `/private/air-checklist` | The legacy aerial inspection workflow (actions 2 & 4) | `was_inspected` | Different field names — see Known Gotchas. |

`/private/cattail-checklist` is **not** usable here — use
`apps_script/cattail_filler.gs` for cattail sheets instead.

**Script Properties:**

| Property | Value |
|---|---|
| `API_BASE` | `https://metrics.mmcd.org/v1` |
| `API_KEY` | `mmcd-sheets-abc123xyz` |

---

## Other Sheet Scripts

A few sheets still run their own dedicated script instead of
`generic_filler.gs`, because they predate it or need something it doesn't
do generically yet:

- `apps_script/cattail_filler.gs` — cattail inspections, builds its own
  Summary and Reinspects tabs.
- `apps_script/drone_filler.gs`, `apps_script/checkback_filler.gs` —
  drone treatment tracking, post-treatment checkback tracking.
- `apps_script/inspection_filler.gs` — the predecessor to this script.

For any **new** sheet, use `generic_filler.gs`.
