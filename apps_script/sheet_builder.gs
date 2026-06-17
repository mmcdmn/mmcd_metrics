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
  // Prevent overlapping executions (trigger could fire while previous run is still going)
  const lock = LockService.getScriptLock();
  if (!lock.tryLock(10000)) {
    Logger.log('Skipping refresh — another execution is still running.');
    return;
  }

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
  Logger.log('fetchClaims_: ' + Object.keys(redisClaims).length + ' claims from Redis: '
    + Object.keys(redisClaims).join(', '));
  const claimState = loadClaimState_();

  const byFos = {};
  for (const r of rows) {
    const fos = r.fos_name || 'Unknown FOS';
    (byFos[fos] = byFos[fos] || []).push(r);
  }

  // Merge claims: compare sheet vs Redis vs saved state
  const { merged: mergedClaims, toRemove, toAdd, newState } =
    mergeSheetAndRedisClaims_(manualSnap, redisClaims, claimState);

  const allLateClaimEdits = {};
  for (const fos of Object.keys(byFos).sort()) {
    const edits = writeFosTab_(ss, safeName_(fos), byFos[fos], manualSnap, mergedClaims, thresholdNum);
    if (edits) Object.assign(allLateClaimEdits, edits);
  }

  // Reconcile late claim edits: user edited claim cells during the API window
  // The merge already ran on stale snapshot, so we fix toRemove/toAdd/newState here.
  for (const [sc, freshClaim] of Object.entries(allLateClaimEdits)) {
    const redisVal = redisClaims[sc] ? String(redisClaims[sc].emp_num || '').trim() : '';
    if (freshClaim) {
      // User added or changed a claim during the API window
      if (freshClaim !== (mergedClaims[sc] || '')) {
        toAdd.push({ sitecode: sc, emp_num: freshClaim });
      }
      newState[sc] = freshClaim;
      Logger.log('LATE-EDIT CLAIM ADD ' + sc + ': "' + freshClaim + '"');
    } else {
      // User cleared a claim during the API window
      if (redisVal) toRemove.push(sc);
      newState[sc] = '__REMOVED__';
      delete mergedClaims[sc];
      Logger.log('LATE-EDIT CLAIM REMOVE ' + sc + ' (redisVal=' + redisVal + ')');
    }
  }

  // Re-hide rows that were hidden before refresh
  restoreHiddenRows_(ss, hiddenSnap);

  // Push new/changed claims to Redis
  if (toAdd.length > 0) pushClaimsToRedis_(toAdd);

  // Remove explicitly deleted claims from Redis
  if (toRemove.length > 0) removeClaimsFromRedis_(toRemove);

  // Save the claim state for next-refresh comparison (single atomic save)
  saveClaimState_(newState);

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
  const rows = Array.isArray(payload) ? payload
             : Array.isArray(payload.data) ? payload.data
             : [];

  // Diagnostic: log first inspected row to verify API fields
  const sample = rows.find(r => r.was_inspected);
  if (sample) {
    Logger.log('API SAMPLE (inspected): ' + sample.sitecode
      + ' | dip_count=' + sample.dip_count
      + ' | pct_wet=' + sample.pct_wet
      + ' | sampnum_yr=' + sample.sampnum_yr
      + ' | keys=' + Object.keys(sample).join(','));
  } else {
    Logger.log('API SAMPLE: no inspected sites in response (' + rows.length + ' total)');
  }
  return rows;
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
 * Merge sheet claims and Redis claims using saved claim state.
 *
 * claimState stores THREE kinds of values per sitecode:
 *   'empId'      — an active claim was displayed
 *   ''           — no claim was displayed (Redis may populate it next time)
 *   '__REMOVED__' — user explicitly removed a claim; block Redis until confirmed gone
 *
 * Sitecodes NOT in claimState are new to the sheet → Redis is authoritative.
 *
 * Returns { merged, toRemove, toAdd, newState }
 */
function mergeSheetAndRedisClaims_(manualSnap, redisClaims, claimState) {
  const REMOVED = '__REMOVED__';
  const merged   = {};
  const toRemove = [];   // sitecodes to delete from Redis
  const toAdd    = [];   // { sitecode, emp_num } to push to Redis
  const newState = {};   // what to save for next cycle

  const claimColIdx = MANUAL_COL_NAMES.indexOf('Claim Emp ID');  // 0

  // Build current sheet claim state from snapshot
  const sheetClaims = {};  // sitecode → emp string (or '')
  for (const sc of Object.keys(manualSnap)) {
    sheetClaims[sc] = String(manualSnap[sc][claimColIdx] || '').trim();
  }

  // Resolve every sitecode that appears on a sheet tab
  for (const sc of Object.keys(sheetClaims)) {
    const sheetVal = sheetClaims[sc];
    const stateEntry = claimState[sc];               // undefined | '' | empId | '__REMOVED__'
    const isPending  = stateEntry === REMOVED;
    const prevVal    = isPending ? '' : (stateEntry || '');
    const isNew      = stateEntry === undefined;     // first time on the sheet
    const redisVal   = redisClaims[sc] ? String(redisClaims[sc].emp_num || '').trim() : '';

    // Log any sitecode where something interesting is happening
    if (sheetVal || prevVal || redisVal || isPending) {
      Logger.log('MERGE ' + sc + ': sheet="' + sheetVal + '" state="' + String(stateEntry)
        + '" prev="' + prevVal + '" redis="' + redisVal + '" isNew=' + isNew + ' isPending=' + isPending);
    }

    if (isPending) {
      // ── Pending removal from a previous cycle ──
      if (sheetVal) {
        // User typed a new claim → cancel the removal
        merged[sc] = sheetVal;
        newState[sc] = sheetVal;
        if (sheetVal !== redisVal) toAdd.push({ sitecode: sc, emp_num: sheetVal });
        Logger.log('  → CANCEL PENDING, use sheet: ' + sheetVal);
      } else if (redisVal) {
        // Redis still has it → keep blocking, re-try removal
        toRemove.push(sc);
        newState[sc] = REMOVED;
        Logger.log('  → RE-TRY REMOVE (Redis still has it)');
      } else {
        Logger.log('  → REMOVAL COMPLETE (Redis confirmed gone)');
      }

    } else if (isNew) {
      // ── First time this sitecode appears on the sheet ──
      if (sheetVal) {
        merged[sc] = sheetVal;
        newState[sc] = sheetVal;
        if (sheetVal !== redisVal) toAdd.push({ sitecode: sc, emp_num: sheetVal });
        Logger.log('  → NEW: use sheet claim: ' + sheetVal);
      } else if (redisVal) {
        merged[sc] = redisVal;
        newState[sc] = redisVal;
        Logger.log('  → NEW: use Redis claim: ' + redisVal);
      }

    } else if (sheetVal !== prevVal) {
      // ── User changed the claim cell ──
      if (sheetVal) {
        // Added or changed claim
        merged[sc] = sheetVal;
        newState[sc] = sheetVal;
        if (sheetVal !== redisVal) toAdd.push({ sitecode: sc, emp_num: sheetVal });
        Logger.log('  → CHANGED: push to Redis: ' + sheetVal);
      } else {
        // Cleared claim → explicit removal
        if (redisVal) toRemove.push(sc);
        newState[sc] = REMOVED;
        Logger.log('  → CLEARED: mark removed (redisVal=' + redisVal + ')');
      }

    } else {
      // ── No user change → Redis is authoritative ──
      if (redisVal) {
        merged[sc] = redisVal;
        newState[sc] = redisVal;
        Logger.log('  → UNCHANGED: use Redis: ' + redisVal);
      } else if (prevVal) {
        // Had a claim last time, Redis lost it → keep sheet value
        merged[sc] = prevVal;
        newState[sc] = prevVal;
        Logger.log('  → UNCHANGED: keep prev: ' + prevVal);
      }
    }
  }

  // Redis claims for sitecodes NOT on any sheet tab → carry forward
  for (const sc of Object.keys(redisClaims)) {
    if (!(sc in sheetClaims)) {
      const stateEntry = claimState[sc];
      if (stateEntry === REMOVED) {
        toRemove.push(sc);
        newState[sc] = REMOVED;
      } else {
        merged[sc] = String(redisClaims[sc].emp_num || '').trim();
        newState[sc] = merged[sc];
      }
    }
  }

  const pendingCount = Object.values(newState).filter(v => v === REMOVED).length;
  Logger.log('mergeSheetAndRedisClaims_: toRemove=' + toRemove.length
    + ' toAdd=' + toAdd.length + ' pending=' + pendingCount);
  return { merged, toRemove, toAdd, newState };
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
    const r = UrlFetchApp.fetch(base + '/private/claims/remove', {
      method: 'post',
      contentType: 'application/json',
      headers: { 'Authorization': 'Bearer ' + key },
      payload: JSON.stringify({ sitecodes: sitecodes }),
      muteHttpExceptions: true,
    });
    const code = r.getResponseCode();
    Logger.log('removeClaimsFromRedis_: HTTP ' + code
      + ' | sites: ' + sitecodes.join(',') + ' | body: ' + r.getContentText());
  } catch (e) { Logger.log('removeClaimsFromRedis_ ERROR: ' + e.message); }
}

/**
 * Save claim state — single atomic write.
 * Values: empId string for active claims, '__REMOVED__' for pending removals.
 */
function saveClaimState_(state) {
  try {
    const json = JSON.stringify(state || {});
    PropertiesService.getDocumentProperties().setProperty('CLAIM_STATE', json);
    Logger.log('saveClaimState_: ' + Object.keys(state || {}).length + ' entries, '
      + json.length + ' bytes');
  } catch (e) { Logger.log('saveClaimState_ ERROR: ' + e.message); }
}

/** Load claim state from previous refresh */
function loadClaimState_() {
  try {
    // Try new CLAIM_STATE key first, fall back to legacy PREV_CLAIMS
    const raw = PropertiesService.getDocumentProperties().getProperty('CLAIM_STATE')
             || PropertiesService.getDocumentProperties().getProperty('PREV_CLAIMS');
    const state = raw ? JSON.parse(raw) : {};
    const entries = Object.keys(state).length;
    const removedCount = Object.values(state).filter(v => v === '__REMOVED__').length;
    Logger.log('loadClaimState_: ' + entries + ' entries, ' + removedCount + ' pending removals'
      + (entries <= 30 ? ' | data=' + raw : ''));
    return state;
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
      // Key by sitecode only (globally unique) — immune to tab-name drift
      snap[sc] = manualVals;
    }
  }
  Logger.log('snapshotManualData_: captured ' + Object.keys(snap).length + ' sitecodes');
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

  if (rows.length === 0) { if (isNew) finalizeTab_(sh); return {}; }

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
    const key = row.sitecode || '';
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
  const lateClaimEdits = {};  // sitecode → new claim value (from late edits)

  // ── Late-edit guard: re-read manual columns to catch edits made during API calls ──
  // The initial snapshot was taken ~60-80s ago. Any user edits since then would be
  // overwritten. Re-read the sheet NOW and merge any changes into the grid.
  if (!isNew) {
    const currentLast = sh.getLastRow();
    if (currentLast >= DATA_START) {
      const nFresh = currentLast - DATA_START + 1;
      const freshData = sh.getRange(DATA_START, 1, nFresh, numCols).getValues();
      const freshMap = {};  // sitecode → full row array
      for (let f = 0; f < nFresh; f++) {
        const sc = String(freshData[f][0]).trim();
        if (sc) freshMap[sc] = freshData[f];
      }
      let lateEdits = 0;
      for (let i = 0; i < grid.length; i++) {
        const sc = String(grid[i][0]).trim();
        if (!sc || !freshMap[sc]) continue;
        for (let j = 0; j < MANUAL_INDICES.length; j++) {
          const idx = MANUAL_INDICES[j];
          const freshVal = String(freshMap[sc][idx] || '').trim();
          const snapVal  = String((manualSnap[sc] || [])[j] || '').trim();
          if (freshVal !== snapVal) {
            // User edited this cell during the execution window → use fresh value
            grid[i][idx] = freshMap[sc][idx];
            lateEdits++;
            // Track late claim edits so refreshChecklist can update Redis/state
            if (idx === MANUAL_INDICES[0]) {  // Claim Emp ID column
              lateClaimEdits[sc] = freshVal;
            }
          }
        }
      }
      if (lateEdits > 0) Logger.log('writeFosTab_ ' + tabName + ': captured ' + lateEdits + ' late edit(s)');
    }
  }

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
  return lateClaimEdits;
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
