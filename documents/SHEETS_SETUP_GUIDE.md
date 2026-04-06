# Air Inspection Workload Manager — Google Sheets Setup Guide

This document covers both Apps Scripts that power the Air Inspection Google Sheets:

1. **`sheet_builder.gs`** — The main workload manager (per-FOS tabs, color coding, claims, Summary)
2. **`raw_data_export.gs`** — A simpler raw data dump for ad-hoc analysis

---

## Table of Contents

- [Quick Start — Workload Manager](#quick-start--workload-manager)
- [Quick Start — Raw Data Export](#quick-start--raw-data-export)
- [Script Variables Reference](#script-variables-reference)
- [How Auto-Refresh Works](#how-auto-refresh-works)
- [Sheet Layout and Workflow](#sheet-layout-and-workflow)
- [Color Coding Reference](#color-coding-reference)
- [Township Tables (Summary Tab)](#township-tables-summary-tab)
- [Troubleshooting](#troubleshooting)

---

## Quick Start — Workload Manager

### Step 1: Create a new Google Sheet

Open Google Sheets and create a blank spreadsheet. Name it something like "Air Inspection Workload – Sr".

### Step 2: Open the Apps Script editor

Go to **Extensions → Apps Script**. This opens the script editor in a new tab.

### Step 3: Paste the script

Delete any existing code in the editor. Copy the entire contents of `sheet_builder.gs` and paste it in.

### Step 4: Set Script Properties

In the Apps Script editor, go to **Project Settings** (gear icon on the left sidebar). Scroll down to **Script Properties** and add these two properties:

| Property | Value |
|----------|-------|
| `API_BASE` | `https://metrics.mmcd.org/v1` |
| `API_KEY` | Your API key (get this from Alex) |

### Step 5: Configure the script

At the top of the script, edit the `CONFIG` object to match your facility:

```javascript
const CONFIG = {
  FACILITY:       'Sr',             // Your facility code
  ZONE:           '1,2',            // Zones to include
  PRIORITIES:     'YELLOW,RED',     // Priority levels to show
  LOOKBACK_DAYS:  2,                // Days back to check for inspections (1–14)
  REFRESH_MINUTES: 1,               // Recommended: 1 minute
  MAP_LINK:       '',               // Optional: link to air site map
  CONTACTS_LINK:  '',               // Optional: link to contacts sheet
};
```

### Step 6: Run the first refresh

In the Apps Script editor, select `refreshChecklist` from the function dropdown at the top, then click **Run**. The first run will ask you to authorize the script — click through the prompts and allow access.

The script will create one tab per FOS and a Summary tab.

### Step 7: Set up auto-refresh

Select `setupAutoRefresh` from the function dropdown and click **Run**. This creates a time-based trigger that automatically calls `refreshChecklist` every 1 minute (or whatever `CONFIG.REFRESH_MINUTES` is set to).

> **Timing note:** The refresh itself takes roughly 1 minute to complete (API call + sheet update). With a 1-minute trigger interval, data could be up to ~2 minutes old in the worst case. If the data looks stale, just wait — the next refresh cycle will update it automatically. In the worst case you may need to wait up to 4 minutes depending on timing.

### Step 8: Share the sheet

Share the Google Sheet with your team. Everyone sees the same live data. Everyone can type their Employee ID into the "Claim Emp ID" column to claim sites.

---

## Quick Start — Raw Data Export

The raw data export script is simpler — it dumps all API columns into a single "Raw Data" tab.

1. Create a **new** Google Sheet (or a new Apps Script project in an existing sheet)
2. **Extensions → Apps Script** → paste the contents of `raw_data_export.gs`
3. Go to **Project Settings → Script Properties** and add `API_BASE` and `API_KEY` (same as above)
4. Edit the `FILTERS` object at the top of the script to match your needs
5. Select `exportRawData` from the dropdown and click **Run**
6. (Optional) Run `setupAutoRefresh` to auto-refresh every minute

---

## Script Variables Reference

### `sheet_builder.gs` — CONFIG

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `FACILITY` | string | `'Sr'` | Facility code. Valid values: `Sr`, `MO`, `E`, `Wp`, `Wm`, `N`, `Sj`. Case-insensitive — the API normalizes it. |
| `ZONE` | string | `'1,2'` | Which zones to include. `'1'` for zone 1 only, `'2'` for zone 2 only, `'1,2'` for both. |
| `PRIORITIES` | string | `'YELLOW,RED'` | Comma-separated priority levels to include. Options: `RED`, `YELLOW`, `BLUE`, `GREEN`, `PURPLE`. Set to `''` (empty string) to include all priorities. |
| `LOOKBACK_DAYS` | number | `2` | How many days back to check for inspections (1–14). This controls the `lookback_days` parameter sent to the API. |
| `REFRESH_MINUTES` | number | `1` | How often auto-refresh runs, in minutes. Must be 1, 5, 10, 15, or 30 (Google Apps Script limitation). **Recommended: 1**. |
| `MAP_LINK` | string | `''` | URL to an air site map. If set, shown in the Summary tab. |
| `CONTACTS_LINK` | string | `''` | URL to a contacts sheet. If set, shown in the Summary tab. |

### `raw_data_export.gs` — FILTERS

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `FACILITY` | string | `'SR'` | Same as above. |
| `ZONE` | string | `'1,2'` | Same as above. |
| `PRIORITY` | string | `'YELLOW,RED'` | Same as `PRIORITIES` above. |
| `LOOKBACK_DAYS` | number | `2` | Days back for inspections (1–14). The workload manager gets this automatically from the threshold endpoint. |
| `REFRESH_MINUTES` | number | `1` | Same as above. |

### Changing variables

To change any variable:

1. Open the Google Sheet → **Extensions → Apps Script**
2. Edit the value in the `CONFIG` (or `FILTERS`) object at the top of the script
3. **Save** the script (Ctrl+S)
4. The next auto-refresh cycle will use the new values

If you changed `REFRESH_MINUTES`, run `setupAutoRefresh` again to recreate the trigger with the new interval.

---

## How Auto-Refresh Works

- **`setupAutoRefresh()`** — Creates a time-triggered function that runs `refreshChecklist` (or `exportRawData`) every N minutes. Run this **once**. It automatically removes any old trigger before creating a new one.
- **`removeAutoRefresh()`** — Removes the auto-refresh trigger. The sheet will stop updating until you run `setupAutoRefresh()` again or manually run the refresh function.

The trigger runs in the background even when the sheet is closed. To see active triggers: Apps Script editor → **Triggers** (clock icon in left sidebar).

> **Important:** Google Apps Script has daily execution time quotas. At 1-minute intervals, the script runs ~1,440 times per day. Each run takes roughly 1 minute. A consumer Google account has a quota of 90 minutes/day of execution time, which means 1-minute intervals may hit the quota. If you are using a **Google Workspace** (organizational) account, the daily quota is 6 hours — plenty of room.

---

## Sheet Layout and Workflow

### FOS Tabs

Each FOS (Field Operations Supervisor) gets their own tab. The tab is named after the FOS (e.g. "Eric S.", "Monica W.").

**Row layout:**

| Row | Content |
|-----|---------|
| Row 1 | **Stats banner** — Shows remaining/inspected count, percentage, red bugs count, and numdip threshold |
| Row 2 | **Info bar** — FOS name, facility, instructions, last refresh timestamp |
| Row 3 | **Column headers** |
| Row 4+ | **Data rows** — One row per air site |

**Column layout:**

| Column | Source | Description |
|--------|--------|-------------|
| Sitecode | API | Unique breeding site ID |
| Claim Emp ID | **Synced** | FOS staff types their Employee ID here to claim a site. **Synced with Redis** — claims made in the Shiny app appear here, and claims typed here appear in the Shiny app. |
| Acres | API | Site acreage |
| RA | API | Restricted area flag (Y if restricted) |
| AirMap | API | Air map reference number |
| Status | API | Y if inspected within lookback window |
| Priority | API | Priority level (RED, YELLOW, etc.) |
| #/Dip | API | Number of dips taken |
| % Wet | API | Percentage of site that is wet |
| Sample # | API | Lab sample number |
| Bug Status | API | Lab result: Red Bugs, Blue Bugs, Pending Lab, No Bugs, No Sample |
| Remarks | API | Site card remarks |
| Treatment | API | Active treatment material (if unexpired) |
| New Notes | **Manual** | Free-text field for FOS staff to add notes |
| Drone | **Manual** | Mark if drone is needed |
| Needs Sample | **Manual** | Mark if sampling is needed |

**Manual columns** (Claim Emp ID, New Notes, Drone, Needs Sample) are highlighted with a blue header. Their values are **preserved across refreshes** — the script snapshots them before the refresh and restores them after. Manual data is keyed by sitecode, so it persists even if row order changes.

### Data sorting

Rows are sorted by:
1. Township name (alphabetical)
2. Section code (alphabetical)
3. Uninspected sites first (Status blank before Status Y)
4. Sitecode (alphabetical)

**Section borders:** A thick black line separates different section codes, so you can visually group sites by section.

### Hiding rows

You can right-click rows and **Hide rows** to collapse inspected sites or sites you don't need to see. Hidden rows are **preserved across refreshes** — the script snapshots which sitecodes are hidden and re-hides them after the refresh.

### Claiming workflow

Claims are **synced between Google Sheets and the Shiny Air Inspection Checklist app** via the Redis cache. Both systems share the same claim data through the API.

**How it works:**

1. Staff opens the sheet and finds their tab
2. They scan for unclaimed, uninspected sites (no green background, no Claim Emp ID)
3. They type their Employee ID in the **Claim Emp ID** column for the sites they plan to inspect
4. Claimed rows turn light blue so others can see what's taken
5. After inspection, the next refresh will show Status = Y and a green background

**Claim sync (every refresh cycle):**

1. The script fetches all active claims from the Redis cache (via `GET /v1/private/claims`)
2. It snapshots all Claim Emp ID values currently in the sheet
3. It merges them: **the most recent action wins**
   - If a claim exists in Redis but the sheet cell is empty → Redis claim fills the cell (someone claimed it in the Shiny app)
   - If the sheet cell has a value but Redis doesn't → the sheet claim is pushed to Redis (someone claimed it in the sheet)
   - If both have a value → the sheet value wins (typing in the sheet is the most recent human action)
   - If both are empty → nothing happens
4. After writing all tabs, any new sheet-originated claims are POSTed to Redis so the Shiny app sees them

**Important timing note:** Since the sheet refreshes every ~1 minute, there is a short window where a claim made in Sheets won't be visible in the Shiny app (and vice versa). In the worst case, it may take up to ~2 minutes for claims to sync between systems. This is expected.

### Summary Tab

The Summary tab is always the first tab and shows:

- **Stats table** — Total sites, inspected, remaining, at threshold, % done, treatment acres, % breeding at threshold — broken out by FOS and totals
- **Links** — Air site map and contacts (if configured)
- **Township tables** — Two tables listing all townships:
  - "All Townships in Township Order" — sorted alphabetically by township name
  - "All Townships Separated By FOS" — sorted by FOS, then township

Each township row shows: Township name, 4-digit township code, FOS name, and Done? (Y if all sites in that township are inspected).

---

## Color Coding Reference

### Row backgrounds

| Color | Meaning |
|-------|---------|
| Light green (`#e8f5e9`) | Site has been inspected (Status = Y) |
| Light blue (`#e3f2fd`) | Site has been claimed (Claim Emp ID is filled in) |
| White/no color | Unclaimed, uninspected site |

### Priority column

| Color | Priority |
|-------|----------|
| Light red (`#f8d7da`) | RED |
| Light yellow (`#fff3cd`) | YELLOW |
| Light blue (`#d1ecf1`) | BLUE |
| Light green (`#d4edda`) | GREEN |
| Light purple (`#e2d6f3`) | PURPLE |

### Bug Status column

| Color | Status |
|-------|--------|
| Red (`#ffcdd2`) | Red Bugs |
| Yellow (`#fff9c4`) | Pending Lab |
| Blue (`#bbdefb`) | Blue Bugs |

### Other

| Color | Meaning |
|-------|---------|
| Purple (`#f3e5f5`) | RA column — site is a restricted area |
| Stats banner red (`#c62828`) | Sites remaining |
| Stats banner green (`#2e7d32`) | All sites inspected |

---

## Township Tables (Summary Tab)

The township tables show one row per unique (township, FOS) combination. The **Code** column shows the 4-digit township code (the first 4 digits of the sitecode). A township is marked **Done? = Y** only when every site in that township assigned to that FOS has been inspected.

---

## Troubleshooting

### "Missing Script Property: API_BASE" or "Missing Script Property: API_KEY"

You need to add Script Properties. Go to Apps Script editor → **Project Settings** (gear icon) → **Script Properties** → **Add script property**.

### Sheet goes blank briefly during refresh

The workload manager uses in-place updates — it overwrites cell values without clearing the sheet. You should always see data during a refresh. If a tab is completely blank, it's likely the very first creation of that tab.

### Manual data (claims, notes) disappeared

Manual data is preserved by sitecode. If a site was removed from the API results (e.g. it was reclassified to a different priority), the manual data for that site will be lost. Manual data is also lost if you re-create the Apps Script project from scratch.

### Auto-refresh stopped working

Check the Triggers screen in Apps Script (clock icon in left sidebar). If there are error notifications, click them to see what went wrong. Common causes:
- API is down
- Script Properties were deleted
- Google daily execution quota was exceeded (consumer accounts: 90 min/day)

Run `removeAutoRefresh()` then `setupAutoRefresh()` to reset the trigger.

### "Edit anyway?" warning on protected cells

This should not happen. The script removes all protections before writing data. If you see this, run `refreshChecklist` manually once.

### Township shows wrong FOS

The township tables group by the 4-digit township code and FOS name from the API data. If a township shows the wrong FOS, the underlying site data in the database has that FOS assignment. Contact the data team to correct it.

### How do I change which priorities are shown?

Edit `CONFIG.PRIORITIES` at the top of the script. Examples:
- `'RED'` — only RED priority sites
- `'YELLOW,RED'` — YELLOW and RED
- `'RED,YELLOW,BLUE,GREEN,PURPLE'` — all priorities
- `''` — also all priorities (no filter)

Save the script and the next auto-refresh will use the new filter.

### How do I set this up for a different facility?

Change `CONFIG.FACILITY` to the new facility code (e.g. `'MO'`, `'E'`, `'N'`). Change `CONFIG.ZONE` if needed. Save and refresh.
