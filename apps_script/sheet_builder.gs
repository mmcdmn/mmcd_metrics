// ============================================================================
// AIR INSPECTION WORKLOAD MANAGER — Google Apps Script
// ============================================================================
// Fetches air checklist data from the MMCD Metrics API.
// Creates per-FOS tabs with color coding, claim tracking, and progress stats.
//
// SETUP:
//   1. Open your Google Sheet → Extensions → Apps Script
//   2. Paste this entire script
//   3. Go to Project Settings → Script Properties. Add:
//       API_BASE:  https://metrics.mmcd.org/v1
//       API_KEY:   <your key>
//   4. Run refreshChecklist() or attach it to a button / time trigger
// ============================================================================

// ── CHANGE THESE TO CUSTOMIZE ────────────────────────────────────────────────
const CONFIG = {
  FACILITY:       'Sr',             // Facility code: Sr, MO, E, Wp, Wm, N, Sj
  ZONE:           '1,2',            // Zones: '1', '2', or '1,2'
  PRIORITIES:     'YELLOW,RED',     // Priority filter: RED, YELLOW, BLUE, GREEN, PURPLE (comma-sep, '' for all)
  LOOKBACK_DAYS:  2,                // Days back to check for inspections (1–14)
  REFRESH_MINUTES: 1,               // Auto-refresh interval in minutes (1, 5, 10, 15, 30)
  MAP_LINK:       '',               // Air Site Map link (shown in Summary)
  CONTACTS_LINK:  '',               // Contacts sheet link (shown in Summary)
};

function getProp_(key) {
  const val = PropertiesService.getScriptProperties().getProperty(key);
  if (!val) throw new Error('Missing Script Property: ' + key);
  return val;
}

// ── Column layout ────────────────────────────────────────────────────────────
const ALL_COLUMNS = [
  'Sitecode', 'Claim Emp ID', 'Acres', 'RA', 'AirMap', 'Status',
  'Priority', '#/Dip', '% Wet', 'Sample #', 'Bug Status',
  'Remarks', 'Treatment', 'New Notes', 'Drone', 'Needs Sample',
];
const MANUAL_COL_NAMES = ['Claim Emp ID', 'New Notes', 'Drone', 'Needs Sample'];
const MANUAL_INDICES   = MANUAL_COL_NAMES.map(n => ALL_COLUMNS.indexOf(n));

const STATS_ROW  = 1;
const FILTER_ROW = 2;
const HEADER_ROW = 3;
const DATA_START = 4;

const C = {
  HEADER_BG:       '#1a237e',
  MANUAL_HEADER_BG:'#0b5ed7',
  HEADER_FG:       '#ffffff',
  DONE_BG:         '#e8f5e9',
  CLAIMED_BG:      '#e3f2fd',
  PRIORITY_RED:    '#f8d7da',
  PRIORITY_YELLOW: '#fff3cd',
  PRIORITY_BLUE:   '#d1ecf1',
  PRIORITY_GREEN:  '#d4edda',
  PRIORITY_PURPLE: '#e2d6f3',
  RED_BUGS:        '#ffcdd2',
  PENDING_LAB:     '#fff9c4',
  BLUE_BUGS:       '#bbdefb',
  RA_BG:           '#f3e5f5',
  STATS_BG:        '#fafafa',
  REMAIN_RED:      '#c62828',
  REMAIN_GREEN:    '#2e7d32',
  PCT_GREEN:       '#e8f5e9',
  PCT_YELLOW:      '#fff9c4',
  PCT_RED:         '#ffcdd2',
};


// ════════════════════════════════════════════════════════════════════════════
// ENTRY POINTS
// ════════════════════════════════════════════════════════════════════════════

function refreshChecklist() {
  const ss = SpreadsheetApp.getActiveSpreadsheet();
  const thresholdNum = getThreshold_();
  const rows = fetchChecklistData_();
  if (!rows || rows.length === 0) {
    Logger.log('No air sites returned for ' + CONFIG.PRIORITIES + ' priorities.');
    return;
  }

  const manualSnap = snapshotManualData_(ss);
  const hiddenSnap = snapshotHiddenRows_(ss);
  const redisClaims = fetchClaims_();   // { sitecode: { emp_num, emp_name, time } }
  const previousClaims = loadPreviousClaims_();

  const byFos = {};
  for (const r of rows) {
    const fos = r.fos_name || 'Unknown FOS';
    (byFos[fos] = byFos[fos] || []).push(r);
  }

  // Merge claims: compare sheet vs Redis vs previous state
  const { merged: mergedClaims, toRemove, toAdd } =
    mergeSheetAndRedisClaims_(manualSnap, redisClaims, previousClaims);

  for (const fos of Object.keys(byFos).sort()) {
    writeFosTab_(ss, safeName_(fos), byFos[fos], manualSnap, mergedClaims, thresholdNum);
  }

  // Re-hide rows that were hidden before refresh
  restoreHiddenRows_(ss, hiddenSnap);

  // Push new/changed claims to Redis
  if (toAdd.length > 0) pushClaimsToRedis_(toAdd);

  // Remove explicitly deleted claims from Redis
  if (toRemove.length > 0) removeClaimsFromRedis_(toRemove);

  // Save the final claim state for next-refresh comparison
  savePreviousClaims_(mergedClaims);

  writeSummary_(ss, byFos, thresholdNum);
  SpreadsheetApp.flush();
}

function refreshAirChecklist() { refreshChecklist(); }

/**
 * Run this ONCE to set up automatic refresh.
 * Uses CONFIG.REFRESH_MINUTES (default 1 min).
 * Go to Apps Script → Run → setupAutoRefresh
 */
function setupAutoRefresh() {
  ScriptApp.getProjectTriggers().forEach(t => {
    if (t.getHandlerFunction() === 'refreshChecklist') ScriptApp.deleteTrigger(t);
  });
  ScriptApp.newTrigger('refreshChecklist')
    .timeBased()
    .everyMinutes(CONFIG.REFRESH_MINUTES)
    .create();
  Logger.log('Auto-refresh set: every ' + CONFIG.REFRESH_MINUTES + ' minute(s).');
}

/** Remove the automatic refresh trigger */
function removeAutoRefresh() {
  ScriptApp.getProjectTriggers().forEach(t => {
    if (t.getHandlerFunction() === 'refreshChecklist') ScriptApp.deleteTrigger(t);
  });
  Logger.log('Auto-refresh removed.');
}


// ════════════════════════════════════════════════════════════════════════════
// API CALLS
// ════════════════════════════════════════════════════════════════════════════

function getThreshold_() {
  try {
    const base = getProp_('API_BASE');
    const r = UrlFetchApp.fetch(base + '/public/threshold',
                                { muteHttpExceptions: true });
    if (r.getResponseCode() === 200) {
      const j = JSON.parse(r.getContentText());
      return j.threshold || 2;
    }
  } catch (e) { Logger.log('threshold: ' + e.message); }
  return 2;
}

function fetchChecklistData_() {
  const base = getProp_('API_BASE');
  const key  = getProp_('API_KEY');

  let url = base + '/private/air-checklist'
    + '?facility=' + CONFIG.FACILITY
    + '&lookback_days=' + CONFIG.LOOKBACK_DAYS
    + '&zone=' + encodeURIComponent(CONFIG.ZONE);
  if (CONFIG.PRIORITIES) url += '&priority=' + encodeURIComponent(CONFIG.PRIORITIES);

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

/** Fetch active claims from Redis via the API */
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

/**
 * Merge sheet claims and Redis claims using previous-refresh state.
 *
 * Logic per sitecode on the sheet:
 *   sheet changed (sheet != prev) ──► sheet wins (add / remove)
 *   sheet unchanged (sheet == prev) ──► Redis is authoritative
 *
 * This handles:
 *   • User types new claim     → pushed to Redis
 *   • User clears a claim      → removed from Redis
 *   • Shiny app adds claim     → appears on sheet next refresh
 *   • Shiny app removes claim  → disappears from sheet next refresh
 *
 * Returns { merged, toRemove, toAdd }
 */
function mergeSheetAndRedisClaims_(manualSnap, redisClaims, previousClaims) {
  const merged   = {};
  const toRemove = [];   // sitecodes to delete from Redis
  const toAdd    = [];   // { sitecode, emp_num } to push to Redis

  const claimColIdx = MANUAL_COL_NAMES.indexOf('Claim Emp ID');  // 0

  // Build current sheet claim state from snapshot
  const sheetClaims = {};  // sitecode → emp string (or '')
  for (const snapKey of Object.keys(manualSnap)) {
    const sitecode = snapKey.split('::')[1];
    if (!sitecode) continue;
    sheetClaims[sitecode] = String(manualSnap[snapKey][claimColIdx] || '').trim();
  }

  // Resolve every sitecode that appears on a sheet tab
  for (const sc of Object.keys(sheetClaims)) {
    const sheetVal = sheetClaims[sc];
    const prevVal  = previousClaims[sc] || '';
    const redisVal = redisClaims[sc] ? String(redisClaims[sc].emp_num || '').trim() : '';

    if (sheetVal && sheetVal !== prevVal) {
      // User typed or changed a claim → sheet wins
      merged[sc] = sheetVal;
      if (sheetVal !== redisVal) toAdd.push({ sitecode: sc, emp_num: sheetVal });
    } else if (!sheetVal && prevVal) {
      // User cleared a claim that was shown last refresh → explicit removal
      if (redisVal) toRemove.push(sc);
      // merged stays empty for this sc
    } else if (redisVal) {
      // No user change — Redis is authoritative
      merged[sc] = redisVal;
    }
    // else: no claim anywhere
  }

  // Redis claims for sitecodes NOT on any sheet tab → carry forward
  for (const sc of Object.keys(redisClaims)) {
    if (!(sc in sheetClaims)) {
      merged[sc] = String(redisClaims[sc].emp_num || '').trim();
    }
  }

  return { merged, toRemove, toAdd };
}

/** Push claim additions to Redis via the API */
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

/** Remove claims from Redis via the API */
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

/** Save the final merged-claims map for next-refresh comparison */
function savePreviousClaims_(claims) {
  try {
    PropertiesService.getDocumentProperties()
      .setProperty('PREV_CLAIMS', JSON.stringify(claims));
  } catch (e) { Logger.log('savePreviousClaims_: ' + e.message); }
}

/** Load the claims map that was saved at the end of the previous refresh */
function loadPreviousClaims_() {
  try {
    const raw = PropertiesService.getDocumentProperties().getProperty('PREV_CLAIMS');
    return raw ? JSON.parse(raw) : {};
  } catch (e) { return {}; }
}


// ════════════════════════════════════════════════════════════════════════════
// MANUAL DATA PRESERVATION
// ════════════════════════════════════════════════════════════════════════════

function snapshotManualData_(ss) {
  const snap = {};
  for (const sh of ss.getSheets()) {
    const name = sh.getName();
    if (name === 'Summary' || name === 'Config') continue;
    const last = sh.getLastRow();
    if (last < DATA_START) continue;

    const numRows = last - DATA_START + 1;
    const allData = sh.getRange(DATA_START, 1, numRows, ALL_COLUMNS.length).getValues();

    for (let i = 0; i < numRows; i++) {
      const sc = String(allData[i][0]).trim();   // col 1 = Sitecode
      if (!sc) continue;
      const manualVals = MANUAL_INDICES.map(idx => allData[i][idx]);
      snap[name + '::' + sc] = manualVals;
    }
  }
  return snap;
}

/** Snapshot which sitecodes have hidden rows, keyed by tab name */
function snapshotHiddenRows_(ss) {
  const snap = {};
  for (const sh of ss.getSheets()) {
    const name = sh.getName();
    if (name === 'Summary' || name === 'Config') continue;
    const last = sh.getLastRow();
    if (last < DATA_START) continue;

    const numRows = last - DATA_START + 1;
    const codes = sh.getRange(DATA_START, 1, numRows, 1).getValues();
    const hidden = new Set();
    for (let i = 0; i < numRows; i++) {
      if (sh.isRowHiddenByUser(DATA_START + i)) {
        const sc = String(codes[i][0]).trim();
        if (sc) hidden.add(sc);
      }
    }
    if (hidden.size > 0) snap[name] = hidden;
  }
  return snap;
}

/** After data is rewritten, re-hide rows matching the snapshot */
function restoreHiddenRows_(ss, hiddenSnap) {
  for (const [tabName, hiddenSet] of Object.entries(hiddenSnap)) {
    const sh = ss.getSheetByName(tabName);
    if (!sh) continue;
    const last = sh.getLastRow();
    if (last < DATA_START) continue;

    const numRows = last - DATA_START + 1;
    const codes = sh.getRange(DATA_START, 1, numRows, 1).getValues();
    for (let i = 0; i < numRows; i++) {
      const sc = String(codes[i][0]).trim();
      if (sc && hiddenSet.has(sc)) {
        sh.hideRows(DATA_START + i);
      }
    }
  }
}


// ════════════════════════════════════════════════════════════════════════════
// FOS TAB WRITER
// ════════════════════════════════════════════════════════════════════════════

function writeFosTab_(ss, tabName, rows, manualSnap, mergedClaims, thresholdNum) {
  let sh = ss.getSheetByName(tabName);
  const isNew = !sh;
  if (isNew) sh = ss.insertSheet(tabName);

  // Remove protections from prior runs (prevents "Edit anyway?" warnings)
  if (!isNew) {
    const prots = sh.getProtections(SpreadsheetApp.ProtectionType.RANGE);
    if (prots.length) prots.forEach(p => p.remove());
    sh.showRows(1, sh.getMaxRows());
  }

  // Sort by township, section, uninspected first, then sitecode
  rows.sort((a, b) => {
    return (a.township_name || '').localeCompare(b.township_name || '')
        || (a.sectcode || '').localeCompare(b.sectcode || '')
        || ((a.was_inspected === b.was_inspected) ? 0 : (a.was_inspected ? 1 : -1))
        || (a.sitecode || '').localeCompare(b.sitecode || '');
  });

  const total     = rows.length;
  const inspected = rows.filter(r => r.was_inspected).length;
  const remaining = total - inspected;
  const pct       = total > 0 ? Math.round(100 * inspected / total) : 0;
  const redBugs   = rows.filter(r => r.bug_status === 'Red Bugs').length;

  const statsText = remaining === 0
    ? `All ${total} sites inspected | Threshold: ${thresholdNum}`
    : `Remaining: ${remaining} | Inspected: ${inspected} / ${total} (${pct}%) | Red Bugs: ${redBugs} | Threshold: ${thresholdNum}`;
  const infoText = `${tabName} | ${CONFIG.FACILITY} Facility | Claim sites by typing your Emp ID in "Claim Emp ID" | Last Refresh: ${new Date().toLocaleString()}`;

  if (isNew) {
    // First run: merge, format, headers, column widths
    sh.getRange(STATS_ROW, 1, 1, ALL_COLUMNS.length).merge()
      .setValue(statsText).setFontSize(13).setFontWeight('bold')
      .setFontColor(remaining === 0 ? C.REMAIN_GREEN : C.REMAIN_RED)
      .setBackground(C.STATS_BG).setVerticalAlignment('middle');
    sh.setRowHeight(STATS_ROW, 36);
    sh.getRange(FILTER_ROW, 1, 1, ALL_COLUMNS.length).merge()
      .setValue(infoText).setFontSize(10).setFontColor('#666666').setBackground(C.STATS_BG);
    sh.getRange(HEADER_ROW, 1, 1, ALL_COLUMNS.length)
      .setValues([ALL_COLUMNS]).setBackground(C.HEADER_BG).setFontColor(C.HEADER_FG)
      .setFontWeight('bold').setHorizontalAlignment('center');
    for (const colName of MANUAL_COL_NAMES) {
      sh.getRange(HEADER_ROW, ALL_COLUMNS.indexOf(colName) + 1).setBackground(C.MANUAL_HEADER_BG);
    }
  } else {
    // Update: just overwrite the two text values (merge/formatting persists)
    sh.getRange(STATS_ROW, 1).setValue(statsText)
      .setFontColor(remaining === 0 ? C.REMAIN_GREEN : C.REMAIN_RED);
    sh.getRange(FILTER_ROW, 1).setValue(infoText);
  }

  if (rows.length === 0) { if (isNew) finalizeTab_(sh); return; }

  const grid = [];
  for (const row of rows) {
    const done = row.was_inspected;
    const vals = [
      row.sitecode   || '',
      '',
      row.acres      != null ? row.acres : '',
      row.restricted_area ? 'Y' : '',
      row.airmap_num || '',
      done ? 'Y' : '',
      row.priority   || '',
      row.dip_count  != null ? row.dip_count : '',
      row.pct_wet    != null ? row.pct_wet : '',
      row.sampnum_yr || '',
      row.bug_status || '',
      row.remarks    || '',
      row.has_active_treatment ? (row.active_material || 'Y') : '',
      '', '', '',
    ];
    const key = tabName + '::' + (row.sitecode || '');
    if (manualSnap[key]) {
      for (let j = 0; j < MANUAL_COL_NAMES.length; j++) {
        vals[MANUAL_INDICES[j]] = manualSnap[key][j] || '';
      }
    }
    // Apply merged claim (Redis or sheet, whichever is most recent)
    const sc = row.sitecode || '';
    if (mergedClaims[sc]) {
      vals[MANUAL_INDICES[0]] = mergedClaims[sc];  // Claim Emp ID
    }
    grid.push(vals);
  }

  const numCols = ALL_COLUMNS.length;

  // Capture old extent BEFORE writing new data
  const oldLastRow = sh.getLastRow();

  // Overwrite data in place — sheet is NEVER blank
  sh.getRange(DATA_START, 1, grid.length, numCols).setValues(grid);

  // Clear leftover rows from previous refresh (if old data was longer)
  const newLastRow = DATA_START + grid.length - 1;
  if (oldLastRow > newLastRow) {
    sh.getRange(newLastRow + 1, 1, oldLastRow - newLastRow, numCols)
      .clearContent().clearFormat();
  }

  // ── Batch styling ──────────────────────────────────────────────────────
  const statusIdx = ALL_COLUMNS.indexOf('Status');
  const priIdx    = ALL_COLUMNS.indexOf('Priority');
  const bugIdx    = ALL_COLUMNS.indexOf('Bug Status');
  const raIdx     = ALL_COLUMNS.indexOf('RA');
  const claimIdx  = ALL_COLUMNS.indexOf('Claim Emp ID');
  const priColorMap = { RED: C.PRIORITY_RED, YELLOW: C.PRIORITY_YELLOW, BLUE: C.PRIORITY_BLUE, GREEN: C.PRIORITY_GREEN, PURPLE: C.PRIORITY_PURPLE };

  const bgColors = [], fontColors = [], fontWeights = [], hAligns = [];
  for (let i = 0; i < rows.length; i++) {
    const row = rows[i];
    const claimedBy = String(grid[i][claimIdx] || '').trim();
    const rowBg = row.was_inspected ? C.DONE_BG : claimedBy ? C.CLAIMED_BG : null;
    const bg = new Array(numCols).fill(rowBg);
    const fc = new Array(numCols).fill(null);
    const fw = new Array(numCols).fill(null);
    const ha = new Array(numCols).fill(null);

    if (row.was_inspected) {
      fc[statusIdx] = C.REMAIN_GREEN; fw[statusIdx] = 'bold'; ha[statusIdx] = 'center';
    }
    const pri = String(row.priority || '').toUpperCase();
    if (priColorMap[pri]) { bg[priIdx] = priColorMap[pri]; fw[priIdx] = 'bold'; }
    if (row.bug_status === 'Red Bugs')       { bg[bugIdx] = C.RED_BUGS; fw[bugIdx] = 'bold'; }
    else if (row.bug_status === 'Pending Lab') { bg[bugIdx] = C.PENDING_LAB; }
    else if (row.bug_status === 'Blue Bugs')   { bg[bugIdx] = C.BLUE_BUGS; }
    if (row.restricted_area) { bg[raIdx] = C.RA_BG; fw[raIdx] = 'bold'; }

    bgColors.push(bg); fontColors.push(fc); fontWeights.push(fw); hAligns.push(ha);
  }

  const dataRange = sh.getRange(DATA_START, 1, rows.length, numCols);
  dataRange.setBackgrounds(bgColors);
  dataRange.setFontColors(fontColors);
  dataRange.setFontWeights(fontWeights);
  dataRange.setHorizontalAlignments(hAligns);

  // Clear all old borders then re-apply section breaks
  dataRange.setBorder(false, false, false, false, false, false);
  for (let i = 0; i < rows.length - 1; i++) {
    if ((rows[i].sectcode || '') !== (rows[i + 1].sectcode || '')) {
      sh.getRange(DATA_START + i, 1, 1, numCols)
        .setBorder(null, null, true, null, null, null, '#000000', SpreadsheetApp.BorderStyle.SOLID_THICK);
    }
  }

  if (isNew) finalizeTab_(sh);
}


// ════════════════════════════════════════════════════════════════════════════
// SUMMARY TAB
// ════════════════════════════════════════════════════════════════════════════

function writeSummary_(ss, byFos, thresholdNum) {
  let sh = ss.getSheetByName('Summary');
  if (!sh) sh = ss.insertSheet('Summary', 0);
  sh.clear();

  const fosNames = Object.keys(byFos).sort();
  const numCols  = 2 + fosNames.length;   // "Air Stats" + "Totals" + each FOS

  // Compute per-FOS stats
  let totalAll = 0, checkedAll = 0, threshAll = 0, remAll = 0, trtAcresAll = 0;
  const stats = {};
  for (const fos of fosNames) {
    const sites   = byFos[fos];
    const total   = sites.length;
    const checked = sites.filter(r => r.was_inspected).length;
    const atThr   = sites.filter(r => !r.was_inspected && r.has_active_treatment).length;
    const rem     = total - checked - atThr;
    const pctDone = total > 0 ? (100 * checked / total) : 0;
    const trtAcres = sites
      .filter(r => r.has_active_treatment)
      .reduce((s, r) => s + (parseFloat(r.acres) || 0), 0);
    const pctTHR  = total > 0 ? (100 * atThr / total) : 0;
    stats[fos] = { total, checked, atThr, rem, pctDone, trtAcres, pctTHR };
    totalAll += total; checkedAll += checked; threshAll += atThr; trtAcresAll += trtAcres;
  }
  remAll = totalAll - checkedAll - threshAll;
  const pctDoneAll = totalAll > 0 ? (100 * checkedAll / totalAll) : 0;
  const pctTHRAll  = totalAll > 0 ? (100 * threshAll / totalAll) : 0;

  // Row 1: Header
  sh.getRange(1, 1, 1, numCols)
    .setValues([['Air Stats', 'Totals', ...fosNames]])
    .setFontWeight('bold').setBackground(C.HEADER_BG).setFontColor(C.HEADER_FG);

  // Rows 2-8: Stats
  const metricsData = [
    ['Total Air Sites',    totalAll,                  ...fosNames.map(f => stats[f].total)],
    ['# of sites checked', checkedAll,                ...fosNames.map(f => stats[f].checked)],
    ['# of sites rem.',    remAll,                    ...fosNames.map(f => stats[f].rem)],
    ['# at Threshold',     threshAll,                 ...fosNames.map(f => stats[f].atThr)],
    ['% Done',             pctDoneAll.toFixed(2),     ...fosNames.map(f => stats[f].pctDone.toFixed(2))],
    ['Treatment Acres',    trtAcresAll.toFixed(2),    ...fosNames.map(f => stats[f].trtAcres.toFixed(2))],
    ['% Breeding THR',    pctTHRAll.toFixed(2),       ...fosNames.map(f => stats[f].pctTHR.toFixed(2))],
  ];
  sh.getRange(2, 1, metricsData.length, numCols).setValues(metricsData);
  sh.getRange(2, 1, metricsData.length, 1).setFontWeight('bold');

  // Color-code % Done row
  const pctRow = 6;
  for (let c = 2; c <= numCols; c++) {
    const v = parseFloat(sh.getRange(pctRow, c).getValue());
    sh.getRange(pctRow, c).setBackground(v >= 90 ? C.PCT_GREEN : v >= 60 ? C.PCT_YELLOW : C.PCT_RED);
  }

  let r = 2 + metricsData.length + 1;   // blank row after stats

  // Links
  if (CONFIG.MAP_LINK) {
    sh.getRange(r, 1).setValue('Air Site Map w/ FOS areas').setFontWeight('bold');
    sh.getRange(r, 2).setValue(CONFIG.MAP_LINK);
    r++;
  }
  if (CONFIG.CONTACTS_LINK) {
    sh.getRange(r, 1).setValue('Contacts').setFontWeight('bold');
    sh.getRange(r, 2).setValue(CONFIG.CONTACTS_LINK);
    r++;
  }
  r++;

  // Township tables
  const allRows = [];
  for (const fos of fosNames) allRows.push(...byFos[fos]);
  const townships = getTownshipRows_(allRows);

  // Table 1: All Townships in Township Order
  sh.getRange(r, 1).setValue('All Townships in Township Order')
    .setFontWeight('bold').setFontSize(11);
  r++;
  sh.getRange(r, 1, 1, 4).setValues([['Townships', 'Code', 'FOS', 'Done?']])
    .setFontWeight('bold').setBackground('#e0e0e0');
  r++;
  const byTownOrder = [...townships].sort((a, b) => a.town.localeCompare(b.town) || a.code.localeCompare(b.code));
  const townData1 = byTownOrder.map(t => [t.town, t.code, t.fos, t.done ? 'Y' : 'FALSE']);
  if (townData1.length) {
    sh.getRange(r, 1, townData1.length, 4).setValues(townData1);
    r += townData1.length;
  }
  r++;

  // Table 2: All Townships Separated By FOS
  sh.getRange(r, 1).setValue('All Townships Separated By FOS')
    .setFontWeight('bold').setFontSize(11);
  r++;
  sh.getRange(r, 1, 1, 4).setValues([['Townships', 'Code', 'FOS', 'Done?']])
    .setFontWeight('bold').setBackground('#e0e0e0');
  r++;
  const byFosOrder = [...townships].sort((a, b) => a.fos.localeCompare(b.fos) || a.town.localeCompare(b.town));
  const townData2 = byFosOrder.map(t => [t.town, t.code, t.fos, t.done ? 'Y' : 'FALSE']);
  if (townData2.length) {
    sh.getRange(r, 1, townData2.length, 4).setValues(townData2);
    r += townData2.length;
  }

  sh.setFrozenRows(1);
  for (let c = 1; c <= Math.max(numCols, 4); c++) sh.autoResizeColumn(c);
}


// ════════════════════════════════════════════════════════════════════════════
// HELPERS
// ════════════════════════════════════════════════════════════════════════════

function getTownshipRows_(rows) {
  // Township code = first 4 digits of the sitecode. One row per (code, FOS).
  const groups = {};
  for (const r of rows) {
    const town  = r.township_name || 'Unknown';
    const fos   = r.fos_name || 'Unknown';
    const code4 = r.sectcode ? r.sectcode.substring(0, 4) : '';
    const key   = code4 + '|||' + fos;
    if (!groups[key]) groups[key] = { town, code: code4, fos, allDone: true };
    if (!r.was_inspected) groups[key].allDone = false;
  }
  return Object.values(groups).map(g => ({
    town: g.town, code: g.code, fos: g.fos, done: g.allDone,
  }));
}

function finalizeTab_(sh) {
  sh.setFrozenRows(HEADER_ROW);
  sh.setFrozenColumns(0);
  const widthMap = {
    'Sitecode': 120,
    'Claim Emp ID': 110,
    'Acres': 70,
    'RA': 45,
    'AirMap': 70,
    'Status': 55,
    'Priority': 85,
    '#/Dip': 60,
    '% Wet': 65,
    'Sample #': 100,
    'Bug Status': 110,
    'Remarks': 320,
    'Treatment': 120,
    'New Notes': 260,
    'Drone': 80,
    'Needs Sample': 110,
  };
  for (let c = 1; c <= ALL_COLUMNS.length; c++) {
    sh.setColumnWidth(c, widthMap[ALL_COLUMNS[c - 1]] || 100);
  }
}

function safeName_(name) {
  if (!name) return 'Unknown';
  return String(name).replace(/[\/\\?*\[\]:]/g, '-').substring(0, 100);
}
