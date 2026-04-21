# Inspection Data Filler — Setup Guide

> Auto-fills inspection data into your Google Sheet from the MMCD Metrics API.  
> Works with **any column layout** — just configure the column letters once.

---

## What the Script Does

When you run refreshInspectionData, the script does this:

1. Reads all tabs (except tabs listed in SKIP_TABS)
2. Reads all sitecodes in each tab (starting at DATA_START or that tab's override)
3. Calls Webster/MMCD Metrics API for recent inspection data
4. Checks whether each site has inspection data in the last LOOKBACK_DAYS
5. Auto-fills the columns you configured (Emp, Date, Wet, Dip, Sample)
6. Clears stale inspection values when a site is outside LOOKBACK_DAYS
7. Syncs claims (if ENABLE_CLAIMS is true)

Employee number behavior:
- If someone types an emp number for an uninspected site (for example, 1905), the script/API resolves it and displays the employee short name when available.
- For inspected sites, Emp is filled from inspection data directly (inspector name).

---

## 1. Open Apps Script

1. Open your Google Sheet
2. Click **Extensions → Apps Script**
3. If there's existing code in `Code.gs`, select all and delete it

## 2. Paste the Script

1. Open `inspection_filler.gs` (ask your administrator or find it in the repo under `apps_script/`)
2. Copy the **entire** script
3. Paste it into the Apps Script editor (replacing everything)
4. Press **Ctrl+S** to save

## 3. Add Script Properties (API credentials)

This is required — the script won't run without these.

1. In the Apps Script editor, click the **⚙ gear icon** (Project Settings) in the left sidebar
2. Scroll down to **Script Properties**
3. Click **Add script property** and add these two properties:

| Property | Value |
|----------|-------|
| `API_BASE` | `https://metrics.mmcd.org/v1` |
| `API_KEY` | `mmcd-sheets-abc123xyz` |

4. Click **Save script properties**

> The API key above (`mmcd-sheets-abc123xyz`) is the shared internal key for all MMCD sheets.  
> It is not secret — use this exact value.

---

## 4. Toggle Features

See the config settings section below for details on how to configure the script for your sheet.


## 5. Run It

1. In the Apps Script editor, select **`refreshInspectionData`** from the function dropdown (top toolbar)
2. Click the **▶ Run** button
3. **First time only**: Google will ask for permissions
   - Click "Review permissions"
   - Choose your Google account
   - Click "Advanced" → "Go to (project name)"
   - Click "Allow"
4. Check the **Execution log** at the bottom for results

You should see output like:
```
Tab "Edina": 12 inspected, 45 sitecodes. Claims: +0 -0 ↓3
Tab "Minnetonka": 8 inspected, 29 sitecodes. Claims: +2 -0 ↓1
All tabs done: 20 inspected (150 from API). Claims: 2 pushed, 4 pulled, 0 removed.
```

---

## 6. Set Up Auto-Refresh (Optional)

To refresh data automatically every minute:

1. Select **`setupAutoRefresh`** from the function dropdown
2. Click **▶ Run**

To stop auto-refresh:

1. Select **`removeAutoRefresh`** from the function dropdown
2. Click **▶ Run**

You can change the interval in CONFIG:
```javascript
REFRESH_MINUTES: 1,    // Options: 1, 5, 10, 15, or 30
```

You can also control stale-data retention with these:
```javascript
LOOKBACK_DAYS: 2,            // Keep all inspections for this many days
LOOKBACK_ABOVE_THRESH: 7,    // Keep only at or above threshold inspections this long
```

How these two variables differ:
- LOOKBACK_DAYS is the normal retention window for **all** inspections.
- LOOKBACK_ABOVE_THRESH is an extended retention window only for sites at or above the threshold dip value.

Example use case:
- You want to recheck all sites that were under threshold, but keep sites you already know are breeding above threshold.
- Set LOOKBACK_DAYS to 2 and LOOKBACK_ABOVE_THRESH to 7.
- Outcome: under-threshold rows clear after 2 days, while threshold-or-higher rows can stay for up to 7 days.

If entries disappeared that you still want included, increase LOOKBACK_DAYS and/or LOOKBACK_ABOVE_THRESH, then run refreshInspectionData again.

---
## Config Settings


### Configure Your Column Layout

Back in the code editor, find the **CONFIG** section near the top (around line 30). Update the column letters to match YOUR sheet:

```javascript
const CONFIG = {

  // ── Row Layout ─────────────────────────────────────────
  DATA_START: 3,         // First row with sitecodes (not headers)

  // ── Column Mapping ─────────────────────────────────────
  SITECODE_COL: 'A',     // Column with sitecodes (REQUIRED)
  ACRES_COL:    'C',     // Column with acreage
  EMP_COL:      'D',     // Column for Emp# / inspector name
  DATE_COL:     'E',     // Column for inspection date
  WET_COL:      'F',     // Column for % Wet
  DIP_COL:      'G',     // Column for #/Dip
  SAMPLE_COL:   'H',     // Column for Sample#
  ...
};
```

### How to determine your layout

Look at your sheet's header row. Find which column letter each field is in:

| What to look for | CONFIG setting | Example |
|-----------------|----------------|---------|
| Sitecode (6-digit codes like `270101`) | `SITECODE_COL` | `'A'` |
| Acres | `ACRES_COL` | `'C'` |
| Emp# or Inspector | `EMP_COL` | `'D'` |
| Date | `DATE_COL` | `'E'` |
| % Wet | `WET_COL` | `'F'` |
| #/Dip or Dip count | `DIP_COL` | `'G'` |
| Sample# | `SAMPLE_COL` | `'H'` |

**If your sheet doesn't have a column** (e.g., no Sample# column), set it to empty string:
```javascript
SAMPLE_COL: '',    // Skip — my sheet doesn't have this
```

### DATA_START — where does your data begin?

Count down from the top of the sheet:

| Your sheet layout | DATA_START |
|------------------|------------|
| Row 1 = Stats, Row 2 = Headers, Row 3 = Data | `3` |
| Row 1 = Headers, Row 2 = Data | `2` |
| Row 1 = Title, Row 2 = Stats, Row 3 = Headers, Row 4 = Data | `4` |
| Row 1 = Section header, Row 2 = Data | `2` |

> `DATA_START` is the first row that might contain a sitecode **or** a section divider.  
> Section dividers (like "Book Edina" or "273908-") are auto-detected and skipped.

### Per-Tab Overrides (different tabs, different layouts)

Most spreadsheets have multiple tabs, and they don't always start on the same row.
Use `TAB_OVERRIDES` to set tab-specific settings. Tabs not listed here use the global defaults.

```javascript
TAB_OVERRIDES: {
  'Edina':       { DATA_START: 4 },                   // data starts row 4
  'Plymouth':    { DATA_START: 2, SAMPLE_COL: '' },    // row 2, no Sample column
  'Minnetonka':  { DATA_START: 3, WET_COL: 'G' },     // row 3, wet in col G
},
```

The tab name must be **exact** (case-sensitive — match what's on the sheet tab).

You can override any of these per tab:
- `DATA_START`, `STATS_ROW`, `SHOW_STATS_ROW`
- `SITECODE_COL`, `ACRES_COL`, `EMP_COL`, `DATE_COL`, `WET_COL`, `DIP_COL`, `SAMPLE_COL`

Anything you **don't** list in a tab's override keeps the global default.

---
These are on/off switches in the CONFIG section:

### % Wet Dropdown Labels
```javascript
USE_WET_LABELS: true,   // true → writes "3 = 30-39%"
                         // false → writes just "3"
```
Set to `true` if your % Wet column has **data validation dropdowns** with labels like "3 = 30-39%".  
Set to `false` if it's just plain numbers.

### Stats Row (Remaining / Acres / Threshold)
```javascript
SHOW_STATS_ROW: true,   // true → updates row 1 with remaining count
STATS_ROW: 1,           // which row to write stats to
```
Set to `false` if you don't want the script writing "Remaining / Acres / Threshold" in row 1.

### Claims Sync (Redis)
```javascript
ENABLE_CLAIMS: true,    // true → syncs emp# entries with Redis
                         // false → only fills inspection data
```
When enabled, emp# values typed in uninspected rows get shared across all sheets via Redis. Other users will see your claims, and you'll see theirs. Emp numbers are also resolved to employee names when the lookup is available. Set to `false` if you just want inspection data without the claim coordination.

### Row Skip Pattern
```javascript
SKIP_ROW_PATTERN: /^book\s|^\d{6,7}-?\s*$/i,
```
This regex identifies rows that look like section dividers (not sitecodes). The default handles:
- `Book Edina`, `Book Plymouth` (rows starting with "Book")
- `273908-`, `2730021` (6-7 digit section codes)

If your sheet has different divider formats, adjust the regex or ask Alex.

### Tabs to Skip
```javascript
SKIP_TABS: ['Summary', 'Config', 'Template', 'Instructions'],
```
Add any tab names that the script should ignore entirely.

---

## CONFIG Reference (What Each Variable Means)

Use this as a checklist when filling out the CONFIG block.

| Variable | What it means | Typical value |
|---------|----------------|---------------|
| `LOOKBACK_DAYS` | How far back to look for inspections. If set to `2`, only inspections from the last 2 days are treated as current; older inspection values are cleared on refresh. | `2` |
| `LOOKBACK_ABOVE_THRESH` | Extended retention for high-risk rows. Inspections older than LOOKBACK_DAYS are still kept up to this value only if dip is at or above threshold. | `7` |
| `REFRESH_MINUTES` | Auto-refresh trigger interval used by `setupAutoRefresh()`. Does nothing by itself until you run setupAutoRefresh. | `1` |
| `DATA_START` | First row that may contain sitecodes or section divider rows. Rows above are ignored. | `2`, `3`, or `4` |
| `SITECODE_COL` | Column letter containing sitecodes (required). | `'A'` |
| `ACRES_COL` | Column letter for acres (used for Remaining Acres stat). Set to `''` if unused. | `'C'` |
| `EMP_COL` | Column letter for Emp/Inspector/Claim. For uninspected rows, emp numbers can auto-resolve to names. | `'D'` |
| `DATE_COL` | Column letter for last inspection date. | `'E'` |
| `WET_COL` | Column letter for percent wet values. | `'F'` |
| `DIP_COL` | Column letter for dips per site. | `'G'` |
| `SAMPLE_COL` | Column letter for sample number. Set to `''` if your tab does not have this column. | `'H'` |
| `USE_WET_LABELS` | Write dropdown labels (like `3 = 30-39%`) instead of raw code (`3`). | `true` |
| `SHOW_STATS_ROW` | Whether to write Remaining/Acres/Threshold summary row. | `true` |
| `STATS_ROW` | Which row to write the stats summary into. | `1` |
| `ENABLE_CLAIMS` | Enables Redis claim sync and employee number/name coordination. | `true` |
| `SKIP_ROW_PATTERN` | Regex for non-sitecode rows (like Book rows or divider rows). | `/^book\s|^\d{6,7}-?\s*$/i` |
| `SKIP_TABS` | Exact tab names to ignore completely. | `['Summary', ...]` |
| `TAB_OVERRIDES` | Per-tab overrides for row starts and columns when tabs differ. | `{ 'Edina': { DATA_START: 4 } }` |

---

## Example Configurations

### All tabs same layout (simple)
Every tab has Row 1 = Stats, Row 2 = Headers, Row 3+ = Data

```javascript
DATA_START: 3,
SITECODE_COL: 'A', ACRES_COL: 'C', EMP_COL: 'D', DATE_COL: 'E',
WET_COL: 'F', DIP_COL: 'G', SAMPLE_COL: 'H',
USE_WET_LABELS: true, SHOW_STATS_ROW: true,
TAB_OVERRIDES: {},   // nothing to override — all tabs are the same
```

### Tabs with different starting rows
Edina starts on row 4, Minnetonka on row 3, Plymouth on row 2 (no stats row):

```javascript
DATA_START: 3,       // default for most tabs
SITECODE_COL: 'A', ACRES_COL: 'C', EMP_COL: 'D', DATE_COL: 'E',
WET_COL: 'F', DIP_COL: 'G', SAMPLE_COL: 'H',
USE_WET_LABELS: true, SHOW_STATS_ROW: true,
TAB_OVERRIDES: {
  'Edina':      { DATA_START: 4 },
  'Plymouth':   { DATA_START: 2, SHOW_STATS_ROW: false },
},
```

### Tabs with different column layouts
One tab uses column G for wet, another uses column I for sample:

```javascript
DATA_START: 3,
SITECODE_COL: 'A', ACRES_COL: 'C', EMP_COL: 'D', DATE_COL: 'E',
WET_COL: 'F', DIP_COL: 'G', SAMPLE_COL: 'H',
TAB_OVERRIDES: {
  'Special Tab': { DATA_START: 2, WET_COL: 'G', SAMPLE_COL: 'I' },
  'No Sample':   { SAMPLE_COL: '' },
},
```

---

## Troubleshooting

| Problem | Fix |
|---------|-----|
| "Missing Script Property: API_BASE" | You forgot Step 3. Go to ⚙ Project Settings → Script Properties and add `API_BASE` and `API_KEY`. |
| "Missing Script Property: API_KEY" | Same as above — add the `API_KEY` property with value `mmcd-sheets-abc123xyz`. |
| No data appearing | Check the Execution log for "API error" messages. Verify your API_BASE URL is correct. |
| Wrong columns being filled | Double-check your column letter settings in CONFIG match your sheet headers. |
| "Book" rows getting filled | Adjust `SKIP_ROW_PATTERN` to match your section divider format. |
| Dropdown validation errors on % Wet | Set `USE_WET_LABELS: true` if your cells have dropdown validation. |
| Auto-refresh not working | Run `setupAutoRefresh()` again. Check Triggers page (clock icon in left sidebar). |
| Script is slow | Reduce `LOOKBACK_DAYS` to 1. Add unused tabs to `SKIP_TABS`. |
| Entries disappeared but you want them back | Increase `LOOKBACK_DAYS` and/or `LOOKBACK_ABOVE_THRESH`, then run `refreshInspectionData()` again. |
| Claims not syncing | Ensure `ENABLE_CLAIMS: true` and the API is reachable. |

---

## Reference

**API Endpoints Used:**
- `GET /v1/private/air-checklist` — inspection data for all air sites
- `GET /v1/private/claims` — current Redis claims
- `POST /v1/private/claims` — push claims to Redis
- `POST /v1/private/claims/remove` — remove claims from Redis
- `GET /v1/private/employees` — employee number → name lookup
- `GET /v1/public/threshold` — current dip threshold value

**Script Properties:**
- `API_BASE`: `https://metrics.mmcd.org/v1`
- `API_KEY`: `mmcd-sheets-abc123xyz`
