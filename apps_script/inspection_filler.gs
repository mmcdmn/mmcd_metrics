// ============================================================================
// INSPECTION DATA FILLER — Google Apps Script  (v2 — Universal / Configurable)
// ============================================================================
// Reads sitecodes from your sheet, fetches inspection data from the MMCD
// Metrics API, and fills in Emp#, Date, % Wet, #/Dip, and Sample columns.
// Syncs claims with Redis so all sheets stay coordinated.
//
// ★ CUSTOMIZE the CONFIG section below to match YOUR sheet's column layout.
//
// SETUP (one-time):
//   1. Open your Google Sheet → Extensions → Apps Script
//   2. Delete anything in Code.gs → paste this entire script
//   3. Go to Project Settings (⚙) → Script Properties. Add two properties:
//       Property:  API_BASE   Value:  https://metrics.mmcd.org/v1
//       Property:  API_KEY    Value:  mmcd-sheets-abc123xyz
//   4. Come back to the editor → Run ▶ refreshInspectionData()
//      (Google will ask for permissions — click Allow)
//   5. Optional: Run ▶ setupAutoRefresh() to auto-refresh every minute
//
// See the full setup guide: INSPECTION_FILLER_SETUP.md
// ============================================================================


// ╔══════════════════════════════════════════════════════════════════════════╗
// ║  ★★★  CONFIGURATION — EDIT THIS SECTION TO MATCH YOUR SHEET  ★★★      ║
// ╚══════════════════════════════════════════════════════════════════════════╝

const CONFIG = {

  // ── API Settings ──────────────────────────────────────────────────────
  LOOKBACK_DAYS:   2,    // Days back to check for inspections (1–14)
  LOOKBACK_ABOVE_THRESH: 7, // Days to keep hot-site data (dip ≥ threshold); 0 = same as LOOKBACK_DAYS
  REFRESH_MINUTES: 1,    // Auto-refresh interval: 1, 5, 10, 15, or 30

  // ── Row Layout ────────────────────────────────────────────────────────
  // DATA_START: the first row that might contain sitecodes or section dividers.
  // Everything above this row is ignored (headers, stats, titles, etc.)
  DATA_START: 3,         // First data row (1-based). Examples: 3, 4, 2

  // ── Column Mapping ────────────────────────────────────────────────────
  // Tell the script which column letter holds each piece of data.
  // Use the column LETTER as shown in Google Sheets (A, B, C, ... Z, AA, AB).
  // Set any column to '' (empty string) to SKIP that field entirely.
  //
  // Example layouts:
  //   Edina sheet:       SITECODE='A', ACRES='C', EMP='D', DATE='E', WET='F', DIP='G', SAMPLE='H'
  //   Minnetonka sheet:  SITECODE='A', ACRES='C', EMP='D', DATE='E', WET='F', DIP='G', SAMPLE='H'
  //   Plymouth sheet:    SITECODE='A', ACRES='C', EMP='D', DATE='E', WET='F', DIP='G', SAMPLE='H'
  //
  SITECODE_COL: 'A',     // Column with sitecodes (REQUIRED)
  ACRES_COL:    'C',     // Column with acreage (for Remaining Acres count)
  EMP_COL:      'D',     // Column for Emp# / inspector name / claim ID
  DATE_COL:     'E',     // Column for inspection date
  WET_COL:      'F',     // Column for % Wet
  DIP_COL:      'G',     // Column for #/Dip
  SAMPLE_COL:   'H',     // Column for Sample#

  // ── % Wet Dropdown Labels ─────────────────────────────────────────────
  // If your sheet has DATA VALIDATION dropdowns on the % Wet column,
  // set this to true so the script writes labels like "3 = 30-39%"
  // instead of just "3".
  // If your sheet uses plain numbers (0, 1, 2, ... 9, A, S), set to false.
  USE_WET_LABELS: true,

  // ── Row 1 Stats (Remaining / Acres / Threshold) ───────────────────────
  // Set to true to write a stats summary in row 1 of each tab.
  // Set to false if you don't want the script to touch row 1.
  SHOW_STATS_ROW: true,
  STATS_ROW: 1,          // Which row to write stats to (usually 1)

  // ── Claims Sync ───────────────────────────────────────────────────────
  // Set to true to enable two-way claim syncing via the Redis/API.
  // When enabled, if a user types their emp# in the EMP_COL of an
  // uninspected site, it gets pushed to Redis so other sheets see it.
  // Set to false to only fill in inspection data (no claim sync).
  ENABLE_CLAIMS: true,

  // ── Row Skip Pattern ──────────────────────────────────────────────────
  // Regex pattern to identify rows that are NOT sitecodes (section headers,
  // dividers, etc.) These rows are skipped during processing.
  // Default skips rows starting with "Book " or section codes like "273908-"
  // (6-7 digit string ending with a dash and no trailing characters).
  // Blank/empty rows are always skipped automatically.
  SKIP_ROW_PATTERN: /^book\s|^\d{6,7}-?\s*$/i,

  // ── Tabs to Skip ─────────────────────────────────────────────────────
  // Names of tabs that should NOT be processed (case-sensitive).
  // Add any tab names that don't contain inspection data.
  SKIP_TABS: ['Summary', 'Config', 'Template', 'Instructions'],

  // ── Per-Tab Overrides ─────────────────────────────────────────────────
  // Different tabs can have different starting rows and column layouts.
  // List each tab by EXACT name → override whichever settings differ.
  // Any setting NOT listed here falls back to the defaults above.
  //
  // Supported overrides:
  //   DATA_START, STATS_ROW, SHOW_STATS_ROW,
  //   SITECODE_COL, ACRES_COL, EMP_COL, DATE_COL, WET_COL, DIP_COL, SAMPLE_COL
  //
  // Example:
  //   'Edina':       { DATA_START: 4 },                  // data starts on row 4
  //   'Plymouth':    { DATA_START: 2, SAMPLE_COL: '' },   // no Sample col, data on row 2
  //   'Minnetonka':  { DATA_START: 3, WET_COL: 'G' },     // wet is col G instead of F
  //
  TAB_OVERRIDES: {
    // 'Edina':       { DATA_START: 4 },
    // 'Minnetonka':  { DATA_START: 3 },
    // 'Plymouth':    { DATA_START: 2 },
  },
};


// ── % Wet label lookup (only used when USE_WET_LABELS is true) ──────────
const WET_LABELS = {
  '0': '0 = DRY',     '1': '1 = 1-19%',   '2': '2 = 20-29%',
  '3': '3 = 30-39%',  '4': '4 = 40-49%',  '5': '5 = 50-59%',
  '6': '6 = 60-69%',  '7': '7 = 70-79%',  '8': '8 = 80-89%',
  '9': '9 = 90-99%',  'A': 'A = >100%',   'S': 'S = Snow/Unk',
};


// ╔══════════════════════════════════════════════════════════════════════════╗
// ║  END OF CONFIGURATION — Do not edit below unless you know what you're  ║
// ║  doing. The rest of the script uses the CONFIG values above.           ║
// ╚══════════════════════════════════════════════════════════════════════════╝


/** Convert column letter(s) to 1-based number: 'A'→1, 'B'→2, 'AA'→27 */
function colNum_(letter) {
  if (!letter) return 0;
  let n = 0;
  for (let i = 0; i < letter.length; i++) {
    n = n * 26 + (letter.toUpperCase().charCodeAt(i) - 64);
  }
  return n;
}

/**
 * Build resolved config for a specific tab.
 * Merges TAB_OVERRIDES[tabName] on top of global CONFIG defaults.
 * Returns { dataStart, statsRow, showStats, col: { SITECODE, ACRES, ... } }
 */
function getTabConfig_(tabName) {
  const ov = (CONFIG.TAB_OVERRIDES || {})[tabName] || {};
  const ds  = ov.DATA_START      != null ? ov.DATA_START      : CONFIG.DATA_START;
  const sr  = ov.STATS_ROW       != null ? ov.STATS_ROW       : CONFIG.STATS_ROW;
  const ss  = ov.SHOW_STATS_ROW  != null ? ov.SHOW_STATS_ROW  : CONFIG.SHOW_STATS_ROW;
  return {
    dataStart: ds,
    statsRow:  sr,
    showStats: ss,
    col: {
      SITECODE: colNum_(ov.SITECODE_COL != null ? ov.SITECODE_COL : CONFIG.SITECODE_COL),
      ACRES:    colNum_(ov.ACRES_COL    != null ? ov.ACRES_COL    : CONFIG.ACRES_COL),
      EMP_NUM:  colNum_(ov.EMP_COL      != null ? ov.EMP_COL      : CONFIG.EMP_COL),
      DATE:     colNum_(ov.DATE_COL     != null ? ov.DATE_COL     : CONFIG.DATE_COL),
      PCT_WET:  colNum_(ov.WET_COL      != null ? ov.WET_COL      : CONFIG.WET_COL),
      NUM_DIP:  colNum_(ov.DIP_COL      != null ? ov.DIP_COL      : CONFIG.DIP_COL),
      SAMPLE:   colNum_(ov.SAMPLE_COL   != null ? ov.SAMPLE_COL   : CONFIG.SAMPLE_COL),
    },
  };
}

function getProp_(key) {
  const val = PropertiesService.getScriptProperties().getProperty(key);
  if (!val) throw new Error('Missing Script Property: ' + key + '. Go to Project Settings → Script Properties and add it.');
  return val;
}

function readCol_(sheet, dataStart, col, numRows) {
  if (!col) return null;
  return sheet.getRange(dataStart, col, numRows, 1).getValues();
}

function writeCol_(sheet, dataStart, col, numRows, data) {
  if (!col || !data) return;
  sheet.getRange(dataStart, col, numRows, 1).setValues(data);
}

function isSkipRow_(val) {
  if (!val) return true;
  return CONFIG.SKIP_ROW_PATTERN.test(val);
}


// ════════════════════════════════════════════════════════════════════════════
// MAIN
// ════════════════════════════════════════════════════════════════════════════

function refreshInspectionData() {
  const lock = LockService.getScriptLock();
  if (!lock.tryLock(10000)) {
    Logger.log('Skipping refresh — another execution is still running.');
    return;
  }

  const ss = SpreadsheetApp.getActiveSpreadsheet();
  const apiData = fetchChecklist_();
  if (!apiData || apiData.length === 0) {
    Logger.log('No data returned from API.');
    return;
  }

  const lookup = {};
  for (const row of apiData) {
    if (row.sitecode) lookup[row.sitecode] = row;
  }

  const sheets = ss.getSheets();
  const skipSet = new Set(CONFIG.SKIP_TABS);
  skipSet.add('Summary');  // Always skip the Summary tab during data processing
  let totalUpdated = 0, totalClaims = { pushed: 0, pulled: 0, removed: 0 };
  const tabStats = [];

  // Fetch threshold once (used to decide which extended-lookback inspections to keep)
  let threshold = 2;
  try {
    const base = getProp_('API_BASE');
    const r = UrlFetchApp.fetch(base + '/public/threshold', { muteHttpExceptions: true });
    if (r.getResponseCode() === 200) { threshold = JSON.parse(r.getContentText()).threshold || 2; }
  } catch (e) { /* keep default */ }

  // Cutoff date for normal lookback — older inspections kept only if dip ≥ threshold
  const normalCutoff = new Date();
  normalCutoff.setHours(0, 0, 0, 0);
  normalCutoff.setDate(normalCutoff.getDate() - CONFIG.LOOKBACK_DAYS);

  for (const sheet of sheets) {
    const name = sheet.getName();
    if (skipSet.has(name)) continue;

    // ── Per-tab config (DATA_START, columns, stats) ──
    const tc = getTabConfig_(name);
    const ds = tc.dataStart;
    const col = tc.col;

    const lastRow = sheet.getLastRow();
    if (lastRow < ds) continue;

    const numRows = lastRow - ds + 1;
    const scValues = sheet.getRange(ds, col.SITECODE, numRows, 1).getValues();
    const siteRows = {};
    for (let i = 0; i < numRows; i++) {
      const sc = String(scValues[i][0]).trim();
      if (!isSkipRow_(sc)) siteRows[sc] = i;
    }
    if (Object.keys(siteRows).length === 0) continue;

    const empCol = readCol_(sheet, ds, col.EMP_NUM, numRows);
    const datCol = readCol_(sheet, ds, col.DATE,    numRows);
    const wetCol = readCol_(sheet, ds, col.PCT_WET, numRows);
    const dipCol = readCol_(sheet, ds, col.NUM_DIP, numRows);
    const smpCol = readCol_(sheet, ds, col.SAMPLE,  numRows);

    let updated = 0;
    for (const [sc, idx] of Object.entries(siteRows)) {
      const info = lookup[sc];
      let keep = false;
      if (info && info.was_inspected) {
        // Within normal lookback → always keep.
        // Outside normal but within extended → keep only if dip ≥ threshold.
        const inspDate = new Date(info.last_insp_date);
        const withinNormal = inspDate >= normalCutoff;
        const dipNum = parseFloat(info.dip_count);
        const aboveThresh = !isNaN(dipNum) && dipNum >= threshold;
        keep = withinNormal || aboveThresh;
      }

      if (keep) {
        if (empCol) empCol[idx][0] = info.inspector_name || '';
        if (datCol) datCol[idx][0] = info.last_insp_date || '';
        if (wetCol) {
          const wetCode = info.pct_wet != null ? String(info.pct_wet).trim() : '';
          wetCol[idx][0] = CONFIG.USE_WET_LABELS ? (WET_LABELS[wetCode] || wetCode) : wetCode;
        }
        if (dipCol) dipCol[idx][0] = info.dip_count  != null ? info.dip_count : '';
        if (smpCol) smpCol[idx][0] = info.sampnum_yr != null ? String(info.sampnum_yr) : '';
        updated++;
      } else {
        // Not kept — clear stale inspection data.
        // Emp column is left alone here; claims sync handles it for uninspected sites.
        const hadData = (datCol && datCol[idx][0]) || (wetCol && wetCol[idx][0]) || (dipCol && dipCol[idx][0]) || (smpCol && smpCol[idx][0]);
        if (datCol) datCol[idx][0] = '';
        if (wetCol) wetCol[idx][0] = '';
        if (dipCol) dipCol[idx][0] = '';
        if (smpCol) smpCol[idx][0] = '';
        if (hadData) Logger.log('  Cleared stale data for ' + sc + ' (outside lookback / below threshold)');
      }
    }

    writeCol_(sheet, ds, col.DATE,    numRows, datCol);
    writeCol_(sheet, ds, col.PCT_WET, numRows, wetCol);
    writeCol_(sheet, ds, col.NUM_DIP, numRows, dipCol);
    writeCol_(sheet, ds, col.SAMPLE,  numRows, smpCol);

    let claimResult = { pushed: 0, pulled: 0, removed: 0 };
    if (CONFIG.ENABLE_CLAIMS && empCol) {
      claimResult = syncClaims_(siteRows, lookup, empCol);
    }
    writeCol_(sheet, ds, col.EMP_NUM, numRows, empCol);

    // Compute per-tab stats (also writes stats row if showStats is true)
    const stats = updateRemainingCount(sheet, tc, siteRows, dipCol);
    tabStats.push({ name, remaining: stats.remaining, checked: stats.checked,
                    atThreshold: stats.atThreshold, hotAcres: stats.hotAcres,
                    threshold: stats.threshold });

    // Add hyperlinks to sitecode cells
    setSitecodeLinks_(sheet, ds, col, siteRows);

    totalUpdated += updated;
    totalClaims.pushed  += claimResult.pushed;
    totalClaims.pulled  += claimResult.pulled;
    totalClaims.removed += claimResult.removed;

    Logger.log('Tab "' + name + '": ' + updated + ' inspected, '
      + Object.keys(siteRows).length + ' sitecodes. '
      + 'Claims: +' + claimResult.pushed + ' -' + claimResult.removed
      + ' ↓' + claimResult.pulled);
  }

  Logger.log('All tabs done: ' + totalUpdated + ' inspected (' + apiData.length + ' from API). '
    + 'Claims: ' + totalClaims.pushed + ' pushed, '
    + totalClaims.pulled + ' pulled, '
    + totalClaims.removed + ' removed. '
    + new Date().toLocaleTimeString());

  // Write the Summary tab
  updateSummaryTab_(ss, tabStats);
}


// ════════════════════════════════════════════════════════════════════════════
// API
// ════════════════════════════════════════════════════════════════════════════

function fetchChecklist_() {
  const base = getProp_('API_BASE');
  const key  = getProp_('API_KEY');
  const lookback = Math.max(CONFIG.LOOKBACK_DAYS, CONFIG.LOOKBACK_ABOVE_THRESH || 0);
  const url = base + '/private/air-checklist?lookback_days=' + lookback;
  const r = UrlFetchApp.fetch(url, {
    method: 'get',
    headers: { 'Authorization': 'Bearer ' + key },
    muteHttpExceptions: true,
  });
  if (r.getResponseCode() !== 200) {
    Logger.log('API error ' + r.getResponseCode() + ': ' + r.getContentText());
    return [];
  }
  const payload = JSON.parse(r.getContentText());
  return Array.isArray(payload) ? payload
       : Array.isArray(payload.data) ? payload.data
       : [];
}

function fetchClaims_() {
  try {
    const base = getProp_('API_BASE');
    const key  = getProp_('API_KEY');
    const r = UrlFetchApp.fetch(
      base + '/private/claims?lookback_days=' + CONFIG.LOOKBACK_DAYS,
      { headers: { 'Authorization': 'Bearer ' + key }, muteHttpExceptions: true }
    );
    if (r.getResponseCode() === 200) {
      const j = JSON.parse(r.getContentText());
      const list = Array.isArray(j) ? j : (j.data || []);
      const map = {};
      for (const c of list) {
        if (c.sitecode && c.emp_num) {
          map[c.sitecode] = { emp_num: String(c.emp_num), emp_name: c.emp_name || '', time: c.time || '' };
        }
      }
      return map;
    }
  } catch (e) { Logger.log('fetchClaims_: ' + e.message); }
  return {};
}

function pushClaimsToRedis_(claims) {
  if (!claims || claims.length === 0) return;
  try {
    const base = getProp_('API_BASE');
    const key  = getProp_('API_KEY');
    UrlFetchApp.fetch(base + '/private/claims', {
      method: 'post',
      contentType: 'application/json',
      headers: { 'Authorization': 'Bearer ' + key },
      payload: JSON.stringify({
        claims: claims.map(c => ({ sitecode: c.sitecode, emp_num: c.emp_num, emp_name: c.emp_num }))
      }),
      muteHttpExceptions: true,
    });
    Logger.log('Pushed ' + claims.length + ' claim(s) to Redis.');
  } catch (e) { Logger.log('pushClaimsToRedis_: ' + e.message); }
}

function removeClaimsFromRedis_(sitecodes) {
  if (!sitecodes || sitecodes.length === 0) return;
  try {
    const base = getProp_('API_BASE');
    const key  = getProp_('API_KEY');
    UrlFetchApp.fetch(base + '/private/claims/remove', {
      method: 'post',
      contentType: 'application/json',
      headers: { 'Authorization': 'Bearer ' + key },
      payload: JSON.stringify({ sitecodes: sitecodes }),
      muteHttpExceptions: true,
    });
    Logger.log('Removed ' + sitecodes.length + ' claim(s) from Redis.');
  } catch (e) { Logger.log('removeClaimsFromRedis_: ' + e.message); }
}

function fetchEmployeeLookup_() {
  try {
    const base = getProp_('API_BASE');
    const key  = getProp_('API_KEY');
    const r = UrlFetchApp.fetch(
      base + '/private/employees',
      { headers: { 'Authorization': 'Bearer ' + key }, muteHttpExceptions: true }
    );
    if (r.getResponseCode() === 200) {
      const j = JSON.parse(r.getContentText());
      const list = Array.isArray(j) ? j : (j.data || []);
      const map = {};
      for (const e of list) {
        if (e.emp_num && e.shortname) map[String(e.emp_num)] = String(e.shortname);
      }
      return map;
    }
  } catch (e) { Logger.log('fetchEmployeeLookup_: ' + e.message); }
  return {};
}


// ════════════════════════════════════════════════════════════════════════════
// CLAIMS SYNC
// ════════════════════════════════════════════════════════════════════════════

function syncClaims_(siteRows, lookup, empCol) {
  const claimCol   = empCol;
  const REMOVED    = '__REMOVED__';
  const redisClaims = fetchClaims_();
  const claimState  = loadClaimState_();
  const empLookup   = fetchEmployeeLookup_();
  const newState    = {};
  const toAdd       = [];
  const toRemove    = [];
  let pushed = 0, pulled = 0, removed = 0;

  function resolveName(val) {
    if (!val) return val;
    const s = String(val).trim();
    return empLookup[s] || s;
  }

  for (const [sc, idx] of Object.entries(siteRows)) {
    if (lookup[sc] && lookup[sc].was_inspected) continue;
    const sheetVal   = String(claimCol[idx][0] || '').trim();
    const stateEntry = claimState[sc];
    const isPending  = stateEntry === REMOVED;
    const prevVal    = isPending ? '' : (stateEntry || '');
    const isNew      = stateEntry === undefined;
    const redisVal   = redisClaims[sc] ? String(redisClaims[sc].emp_num || '').trim()  : '';
    const redisName  = redisClaims[sc] ? String(redisClaims[sc].emp_name || '').trim() : '';
    const redisDisplay = resolveName(redisName || redisVal);

    if (isPending) {
      if (sheetVal) {
        claimCol[idx][0] = resolveName(sheetVal); newState[sc] = claimCol[idx][0];
        if (sheetVal !== redisVal) { toAdd.push({ sitecode: sc, emp_num: sheetVal }); pushed++; }
      } else if (redisVal) { toRemove.push(sc); newState[sc] = REMOVED; removed++; }
    } else if (isNew) {
      if (sheetVal) {
        if (sheetVal !== redisVal) { toAdd.push({ sitecode: sc, emp_num: sheetVal }); pushed++; }
        claimCol[idx][0] = (redisDisplay && sheetVal === redisVal) ? redisDisplay : resolveName(sheetVal);
        newState[sc] = claimCol[idx][0];
      } else if (redisDisplay) { claimCol[idx][0] = redisDisplay; newState[sc] = redisDisplay; pulled++; }
    } else if (sheetVal !== prevVal) {
      if (sheetVal) {
        if (sheetVal !== redisVal) { toAdd.push({ sitecode: sc, emp_num: sheetVal }); pushed++; }
        claimCol[idx][0] = resolveName(sheetVal); newState[sc] = claimCol[idx][0];
      } else { if (redisVal) { toRemove.push(sc); removed++; } newState[sc] = REMOVED; }
    } else {
      if (redisDisplay) {
        claimCol[idx][0] = redisDisplay; newState[sc] = redisDisplay;
        if (redisDisplay !== prevVal) pulled++;
      } else if (prevVal) { claimCol[idx][0] = resolveName(prevVal); newState[sc] = claimCol[idx][0]; }
    }
  }

  if (toAdd.length > 0)    pushClaimsToRedis_(toAdd);
  if (toRemove.length > 0) removeClaimsFromRedis_(toRemove);

  // Merge this tab's state into the full state (don't wipe other tabs' entries)
  const mergedState = Object.assign({}, claimState, newState);
  saveClaimState_(mergedState);
  return { pushed, pulled, removed };
}

function saveClaimState_(state) {
  try {
    PropertiesService.getDocumentProperties().setProperty('CLAIM_STATE', JSON.stringify(state || {}));
  } catch (e) { Logger.log('saveClaimState_ ERROR: ' + e.message); }
}

function loadClaimState_() {
  try {
    const raw = PropertiesService.getDocumentProperties().getProperty('CLAIM_STATE');
    return raw ? JSON.parse(raw) : {};
  } catch (e) { return {}; }
}


// ════════════════════════════════════════════════════════════════════════════
// REMAINING COUNT  (Stats Row)
// ════════════════════════════════════════════════════════════════════════════

function updateRemainingCount(sheet, tabCfg, siteRows, dipCol) {
  if (!sheet) sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  if (!tabCfg) tabCfg = getTabConfig_(sheet.getName());
  const ds = tabCfg.dataStart;
  const col = tabCfg.col;
  const lastRow = sheet.getLastRow();
  if (lastRow < ds) return { remaining: 0, checked: 0, atThreshold: 0, hotAcres: 0, threshold: 2 };
  const numRows = lastRow - ds + 1;

  if (!siteRows) {
    siteRows = {};
    const scValues = sheet.getRange(ds, col.SITECODE, numRows, 1).getValues();
    for (let i = 0; i < numRows; i++) {
      const sc = String(scValues[i][0]).trim();
      if (!isSkipRow_(sc)) siteRows[sc] = i;
    }
  }
  if (!dipCol && col.NUM_DIP) {
    dipCol = sheet.getRange(ds, col.NUM_DIP, numRows, 1).getValues();
  }

  let threshold = 2;
  try {
    const base = getProp_('API_BASE');
    const r = UrlFetchApp.fetch(base + '/public/threshold', { muteHttpExceptions: true });
    if (r.getResponseCode() === 200) { threshold = JSON.parse(r.getContentText()).threshold || 2; }
  } catch (e) { /* keep default */ }

  let remaining = 0, checked = 0, atThreshold = 0, hotAcres = 0;
  const acresCol = col.ACRES ? sheet.getRange(ds, col.ACRES, numRows, 1).getValues() : null;
  for (const [, idx] of Object.entries(siteRows)) {
    const dip = dipCol ? dipCol[idx][0] : '';
    if (dip === '' || dip === null || dip === undefined) {
      remaining++;
    } else {
      checked++;
      const dipNum = parseFloat(dip);
      if (!isNaN(dipNum) && dipNum >= threshold) {
        atThreshold++;
        if (acresCol) {
          const a = parseFloat(acresCol[idx][0]);
          if (!isNaN(a)) hotAcres += a;
        }
      }
    }
  }
  hotAcres = Math.round(hotAcres * 100) / 100;

  if (tabCfg.showStats) {
    const sr = tabCfg.statsRow;
    sheet.getRange(sr, 1).setValue('Remaining');
    sheet.getRange(sr, 2).setValue(remaining);
    sheet.getRange(sr, 3).setValue('Hot Acres');
    sheet.getRange(sr, 4).setValue(hotAcres);
    sheet.getRange(sr, 5).setValue('Threshold =');
    sheet.getRange(sr, 6).setValue(threshold);
  }

  return { remaining, checked, atThreshold, hotAcres, threshold };
}


// ════════════════════════════════════════════════════════════════════════════
// SUMMARY TAB
// ════════════════════════════════════════════════════════════════════════════

function updateSummaryTab_(ss, tabStats) {
  if (!tabStats || tabStats.length === 0) return;

  let summary = ss.getSheetByName('Summary');
  if (!summary) {
    summary = ss.insertSheet('Summary');
    // Move to first position
    ss.setActiveSheet(summary);
    ss.moveActiveSheet(1);
  }

  // Clear previous content
  summary.clear();

  // Header row
  const headers = ['Tab', 'Remaining', 'Checked', '# At Threshold', 'Hot Acres'];
  summary.getRange(1, 1, 1, headers.length).setValues([headers]);
  summary.getRange(1, 1, 1, headers.length).setFontWeight('bold');

  // Per-tab rows
  const rows = tabStats.map(t => [t.name, t.remaining, t.checked, t.atThreshold, t.hotAcres]);
  if (rows.length > 0) {
    summary.getRange(2, 1, rows.length, headers.length).setValues(rows);
  }

  // Totals row
  const totalsRow = rows.length + 2;
  const totals = tabStats.reduce((acc, t) => {
    acc.remaining    += t.remaining;
    acc.checked      += t.checked;
    acc.atThreshold  += t.atThreshold;
    acc.hotAcres     += t.hotAcres;
    return acc;
  }, { remaining: 0, checked: 0, atThreshold: 0, hotAcres: 0 });
  totals.hotAcres = Math.round(totals.hotAcres * 100) / 100;

  summary.getRange(totalsRow, 1, 1, headers.length).setValues([
    ['TOTAL', totals.remaining, totals.checked, totals.atThreshold, totals.hotAcres]
  ]);
  summary.getRange(totalsRow, 1, 1, headers.length).setFontWeight('bold');

  // Threshold & timestamp
  const threshold = tabStats.length > 0 ? tabStats[0].threshold : 2;
  summary.getRange(totalsRow + 2, 1).setValue('Threshold');
  summary.getRange(totalsRow + 2, 2).setValue(threshold);
  summary.getRange(totalsRow + 3, 1).setValue('Last Updated');
  summary.getRange(totalsRow + 3, 2).setValue(new Date().toLocaleString());

  // Auto-fit columns
  for (let c = 1; c <= headers.length; c++) {
    summary.autoResizeColumn(c);
  }

  Logger.log('Summary tab updated: ' + totals.remaining + ' remaining, '
    + totals.checked + ' checked, ' + totals.atThreshold + ' at threshold, '
    + totals.hotAcres + ' hot acres.');
}


// ════════════════════════════════════════════════════════════════════════════
// AUTO-REFRESH
// ════════════════════════════════════════════════════════════════════════════

// ════════════════════════════════════════════════════════════════════════════
// SITECODE HYPERLINKS
// ════════════════════════════════════════════════════════════════════════════

const SITECODE_URL_BASE = 'https://data.mmcd.org/m1/map?search=';

/**
 * Set hyperlinks on sitecode cells so they open the MMCD map.
 * Only sets a link if the cell doesn't already have one.
 */
function setSitecodeLinks_(sheet, ds, col, siteRows) {
  if (!col.SITECODE) return;
  for (const [sc, idx] of Object.entries(siteRows)) {
    const cell = sheet.getRange(ds + idx, col.SITECODE);
    const existing = cell.getRichTextValue();
    // Skip if already linked
    if (existing && existing.getLinkUrl()) continue;
    const richText = SpreadsheetApp.newRichTextValue()
      .setText(sc)
      .setLinkUrl(SITECODE_URL_BASE + encodeURIComponent(sc))
      .build();
    cell.setRichTextValue(richText);
  }
}


function setupAutoRefresh() {
  ScriptApp.getProjectTriggers().forEach(t => {
    if (t.getHandlerFunction() === 'refreshInspectionData') ScriptApp.deleteTrigger(t);
  });
  ScriptApp.newTrigger('refreshInspectionData')
    .timeBased()
    .everyMinutes(CONFIG.REFRESH_MINUTES)
    .create();
  Logger.log('Auto-refresh set: every ' + CONFIG.REFRESH_MINUTES + ' minute(s).');
}

function removeAutoRefresh() {
  ScriptApp.getProjectTriggers().forEach(t => {
    if (t.getHandlerFunction() === 'refreshInspectionData') ScriptApp.deleteTrigger(t);
  });
  Logger.log('Auto-refresh removed.');
}
