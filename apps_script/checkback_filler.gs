// ============================================================================
// CHECKBACK TRACKER — Google Apps Script
// ============================================================================
// Fetches sites that need post-treatment checkbacks from the MMCD Metrics API,
// builds/updates tabs per brood, and syncs claims with Redis so all sheets
// stay coordinated.
//
// SETUP (one-time):
//   1. Open your Google Sheet → Extensions → Apps Script
//   2. Delete anything in Code.gs → paste this entire script
//   3. Go to Project Settings (⚙) → Script Properties. Add two properties:
//       Property:  API_BASE   Value:  https://metrics.mmcd.org/v1
//       Property:  API_KEY    Value:  mmcd-sheets-abc123xyz
//   4. Come back to the editor → Run ▶ refreshCheckbackData()
//      (Google will ask for permissions — click Allow)
//   5. Optional: Run ▶ setupAutoRefresh() to auto-refresh every minute
// ============================================================================


// ╔══════════════════════════════════════════════════════════════════════════╗
// ║  ★★★  CONFIGURATION — EDIT THIS SECTION TO MATCH YOUR NEEDS  ★★★      ║
// ╚══════════════════════════════════════════════════════════════════════════╝

const CONFIG = {

  // ── Checkback Parameters ──────────────────────────────────────────────
  FACILITY:       '',         // Facility filter (e.g. 'MO', 'E', 'W', 'N'). '' = all
  LOOKBACK_DAYS:  14,         // Days back to look for treatments (1–60)
  CHECKBACK_TYPE: 'percent',  // 'percent' or 'number'
  CHECKBACK_TARGET: 10,       // If percent: 1–100. If number: fixed count per brood
  MATCODE:        '',         // Material code filter. '' = all

  // ── Auto-Refresh ─────────────────────────────────────────────────────
  REFRESH_MINUTES: 1,         // Auto-refresh interval: 1, 5, 10, 15, or 30

  // ── Sheet Layout ──────────────────────────────────────────────────────
  // The script creates one tab per brood with the following columns:
  HEADER_ROW: 1,              // Row for column headers
  DATA_START: 2,              // First data row (below header)

  // Column assignments (letters):
  SITECODE_COL:     'A',      // Sitecode
  ACRES_COL:        'B',      // Acres
  TREATMENT_DATE_COL: 'C',   // Last treatment date in brood
  MATTYPE_COL:      'D',      // Material type (readable name)
  EFFECT_DAYS_COL:  'E',     // Material effective days
  EMP_COL:          'F',      // Claim / Emp# for checkback assignment
  CHECKBACK_DATE_COL: 'G',   // (filled when checkback is completed — from API)
  POST_DIP_COL:     'H',     // Post-treatment #/dip (from completed checkbacks)

  // ── Claims Sync ───────────────────────────────────────────────────────
  // Uses the same Redis claims system as inspection_filler.
  // "claim in one should be the same as the other"
  ENABLE_CLAIMS: true,

  // ── Tabs ──────────────────────────────────────────────────────────────
  SKIP_TABS: ['Summary', 'Config', 'Template', 'Instructions'],
  SUMMARY_TAB: 'Summary',
};


// ╔══════════════════════════════════════════════════════════════════════════╗
// ║  END OF CONFIGURATION                                                  ║
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

const COL = {
  get SITECODE()       { return colNum_(CONFIG.SITECODE_COL); },
  get ACRES()          { return colNum_(CONFIG.ACRES_COL); },
  get TREATMENT_DATE() { return colNum_(CONFIG.TREATMENT_DATE_COL); },
  get MATTYPE()        { return colNum_(CONFIG.MATTYPE_COL); },
  get EFFECT_DAYS()    { return colNum_(CONFIG.EFFECT_DAYS_COL); },
  get EMP()            { return colNum_(CONFIG.EMP_COL); },
  get CHECKBACK_DATE() { return colNum_(CONFIG.CHECKBACK_DATE_COL); },
  get POST_DIP()       { return colNum_(CONFIG.POST_DIP_COL); },
};

const SITECODE_URL_BASE = 'https://webster.mmcd.org/map?search=';

function getProp_(key) {
  const val = PropertiesService.getScriptProperties().getProperty(key);
  if (!val) throw new Error('Missing Script Property: ' + key + '. Go to Project Settings → Script Properties and add it.');
  return val;
}


// ════════════════════════════════════════════════════════════════════════════
// MAIN
// ════════════════════════════════════════════════════════════════════════════

function refreshCheckbackData() {
  const lock = LockService.getScriptLock();
  if (!lock.tryLock(10000)) {
    Logger.log('Skipping refresh — another execution is still running.');
    return;
  }

  const ss = SpreadsheetApp.getActiveSpreadsheet();
  const apiResult = fetchCheckbackChecklist_();
  if (!apiResult) {
    Logger.log('No data returned from checkback-checklist API.');
    return;
  }

  const broods = apiResult.broods || [];
  const sites  = apiResult.data   || [];

  if (broods.length === 0) {
    Logger.log('No broods found for the configured parameters.');
    return;
  }

  // Group sites by brood name (sanitized for sheet tab names)
  const sitesByBrood = {};
  for (const site of sites) {
    let brood = String(site.brood || 'Unknown');
    brood = brood.replace(/[\/\\?*\[\]]/g, '-');
    if (!sitesByBrood[brood]) sitesByBrood[brood] = [];
    sitesByBrood[brood].push(site);
  }

  const broodStats = [];
  const skipSet = new Set(CONFIG.SKIP_TABS);
  let totalClaims = { pushed: 0, pulled: 0, removed: 0 };

  for (const brood of broods) {
    // Sanitize brood name for use as sheet tab name
    let broodName = String(brood.brood || 'Unknown').substring(0, 100);
    broodName = broodName.replace(/[\/\\?*\[\]]/g, '-');  // Remove illegal tab chars
    if (!broodName || broodName === 'Unknown') continue;
    const broodSites = sitesByBrood[broodName] || [];

    // Create or get the tab for this brood
    let sheet = ss.getSheetByName(broodName);
    if (!sheet) {
      sheet = ss.insertSheet(broodName);
      writeHeaders_(sheet);
    }

    // Write site data and sync claims
    const claimResult = fillBroodTab_(sheet, broodSites);
    totalClaims.pushed  += claimResult.pushed;
    totalClaims.pulled  += claimResult.pulled;
    totalClaims.removed += claimResult.removed;

    broodStats.push({
      brood: broodName,
      facility: brood.facility,
      start_date: brood.start_date,
      end_date: brood.end_date,
      sites_treated: brood.sites_treated,
      checkbacks_needed: brood.checkbacks_needed,
      checkbacks_completed: brood.checkbacks_completed,
      remaining: brood.remaining,
    });

    Logger.log('Brood "' + broodName + '": ' + broodSites.length + ' sites needing checkback. '
      + 'Claims: +' + claimResult.pushed + ' ↓' + claimResult.pulled + ' -' + claimResult.removed);
  }

  // Remove tabs for broods that no longer exist (unless in SKIP_TABS)
  const activeBroods = new Set(broods.map(b => b.brood));
  activeBroods.add(CONFIG.SUMMARY_TAB);
  for (const tab of CONFIG.SKIP_TABS) activeBroods.add(tab);

  // Don't delete tabs — just log if there are orphans
  for (const sheet of ss.getSheets()) {
    const name = sheet.getName();
    if (!activeBroods.has(name) && !skipSet.has(name)) {
      Logger.log('Tab "' + name + '" has no matching brood — consider removing it.');
    }
  }

  // Update summary tab
  updateSummaryTab_(ss, broodStats, apiResult);

  Logger.log('Refresh complete. ' + sites.length + ' sites across ' + broods.length + ' broods. '
    + 'Claims: ' + totalClaims.pushed + ' pushed, '
    + totalClaims.pulled + ' pulled, '
    + totalClaims.removed + ' removed.');
}


// ════════════════════════════════════════════════════════════════════════════
// TAB MANAGEMENT
// ════════════════════════════════════════════════════════════════════════════

function writeHeaders_(sheet) {
  const headers = ['Sitecode', 'Acres', 'Treatment Date', 'Material',
                   'Effect Days', 'Claimed By', 'Checkback Date', '#/Dip'];
  sheet.getRange(CONFIG.HEADER_ROW, 1, 1, headers.length).setValues([headers]);
  sheet.getRange(CONFIG.HEADER_ROW, 1, 1, headers.length).setFontWeight('bold');
  sheet.setFrozenRows(CONFIG.HEADER_ROW);
}

/**
 * Fill a brood tab with site data and sync claims.
 * Preserves existing claim values for sites still in the list.
 */
function fillBroodTab_(sheet, broodSites) {
  const ds = CONFIG.DATA_START;

  // Read existing sitecodes + claims to preserve claims during refresh
  const lastRow = sheet.getLastRow();
  const existingClaims = {};
  if (lastRow >= ds) {
    const numRows = lastRow - ds + 1;
    const scVals = sheet.getRange(ds, COL.SITECODE, numRows, 1).getValues();
    const empVals = COL.EMP ? sheet.getRange(ds, COL.EMP, numRows, 1).getValues() : null;
    for (let i = 0; i < numRows; i++) {
      const sc = String(scVals[i][0]).trim();
      if (sc && empVals) {
        existingClaims[sc] = String(empVals[i][0] || '').trim();
      }
    }
  }

  // Sort sites by sitecode for consistent ordering
  broodSites.sort((a, b) => String(a.sitecode || '').localeCompare(String(b.sitecode || '')));

  // Build output arrays
  const numSites = broodSites.length;
  if (numSites === 0) {
    // Clear data area if no sites remain
    if (lastRow >= ds) {
      sheet.getRange(ds, 1, lastRow - ds + 1, 8).clearContent();
    }
    return { pushed: 0, pulled: 0, removed: 0 };
  }

  const scOut     = [];
  const acresOut  = [];
  const trtOut    = [];
  const matOut    = [];
  const effOut    = [];
  const empOut    = [];
  const cbDateOut = [];
  const dipOut    = [];
  const siteRows  = {};  // sitecode → row index for claims sync

  for (let i = 0; i < numSites; i++) {
    const site = broodSites[i];
    scOut.push([site.sitecode || '']);
    acresOut.push([site.acres != null ? site.acres : '']);
    trtOut.push([site.treatment_date || '']);
    matOut.push([site.mattype || site.matcode || '']);
    effOut.push([site.effect_days != null ? site.effect_days : '']);
    // Preserve existing claim if site was already in the sheet
    empOut.push([existingClaims[site.sitecode] || '']);
    cbDateOut.push([site.checkback_date || '']);
    dipOut.push([site.post_dip != null ? site.post_dip : '']);
    if (site.needs_checkback !== false) siteRows[site.sitecode] = i;  // Only sync claims for remaining sites
  }

  // Clear any extra rows from previous refresh
  if (lastRow >= ds + numSites) {
    sheet.getRange(ds + numSites, 1, lastRow - ds - numSites + 1, 8).clearContent();
  }

  // Write columns
  sheet.getRange(ds, COL.SITECODE, numSites, 1).setValues(scOut);
  sheet.getRange(ds, COL.ACRES, numSites, 1).setValues(acresOut);
  sheet.getRange(ds, COL.TREATMENT_DATE, numSites, 1).setValues(trtOut);
  sheet.getRange(ds, COL.MATTYPE, numSites, 1).setValues(matOut);
  sheet.getRange(ds, COL.EFFECT_DAYS, numSites, 1).setValues(effOut);
  sheet.getRange(ds, COL.CHECKBACK_DATE, numSites, 1).setValues(cbDateOut);
  sheet.getRange(ds, COL.POST_DIP, numSites, 1).setValues(dipOut);

  // Claims sync — only for sites still needing a checkback (siteRows excludes completed ones)
  let claimResult = { pushed: 0, pulled: 0, removed: 0 };
  if (CONFIG.ENABLE_CLAIMS && Object.keys(siteRows).length > 0) {
    claimResult = syncClaims_(siteRows, empOut);
  }
  sheet.getRange(ds, COL.EMP, numSites, 1).setValues(empOut);

  // Add sitecode hyperlinks
  setSitecodeLinks_(sheet, ds, COL.SITECODE, numSites, scOut);

  return claimResult;
}


// ════════════════════════════════════════════════════════════════════════════
// API
// ════════════════════════════════════════════════════════════════════════════

function fetchCheckbackChecklist_() {
  const base = getProp_('API_BASE');
  const key  = getProp_('API_KEY');

  let url = base + '/private/checkback-checklist?lookback_days=' + CONFIG.LOOKBACK_DAYS
    + '&checkback_type=' + encodeURIComponent(CONFIG.CHECKBACK_TYPE)
    + '&checkback_target=' + CONFIG.CHECKBACK_TARGET;

  if (CONFIG.FACILITY) {
    url += '&facility=' + encodeURIComponent(CONFIG.FACILITY);
  }
  if (CONFIG.MATCODE) {
    url += '&matcode=' + encodeURIComponent(CONFIG.MATCODE);
  }

  Logger.log('Fetching: ' + url);

  const options = {
    method: 'get',
    headers: { 'Authorization': 'Bearer ' + key },
    muteHttpExceptions: true
  };

  const r = UrlFetchApp.fetch(url, options);

  if (r.getResponseCode() !== 200) {
    Logger.log('API error ' + r.getResponseCode() + ': ' + r.getContentText());
    return null;
  }

  const payload = JSON.parse(r.getContentText());
  Logger.log('API returned: ' + (payload.count || 0) + ' sites, ' + (payload.broods ? payload.broods.length : 0) + ' broods');
  return payload;
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

/**
 * Sync claims for a single brood tab.
 * siteRows: { sitecode → index }
 * empCol: [[val], [val], ...] array (mutated in place)
 */
function syncClaims_(siteRows, empCol) {
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
    const sheetVal   = String(empCol[idx][0] || '').trim();
    const stateEntry = claimState[sc];
    const isPending  = stateEntry === REMOVED;
    const prevVal    = isPending ? '' : (stateEntry || '');
    const isNew      = stateEntry === undefined;
    const redisVal   = redisClaims[sc] ? String(redisClaims[sc].emp_num || '').trim()  : '';
    const redisName  = redisClaims[sc] ? String(redisClaims[sc].emp_name || '').trim() : '';
    const redisDisplay = resolveName(redisName || redisVal);

    if (isPending) {
      if (sheetVal) {
        empCol[idx][0] = resolveName(sheetVal); newState[sc] = empCol[idx][0];
        if (sheetVal !== redisVal) { toAdd.push({ sitecode: sc, emp_num: sheetVal }); pushed++; }
      } else if (redisVal) { toRemove.push(sc); newState[sc] = REMOVED; removed++; }
    } else if (isNew) {
      if (sheetVal) {
        if (sheetVal !== redisVal) { toAdd.push({ sitecode: sc, emp_num: sheetVal }); pushed++; }
        empCol[idx][0] = (redisDisplay && sheetVal === redisVal) ? redisDisplay : resolveName(sheetVal);
        newState[sc] = empCol[idx][0];
      } else if (redisDisplay) { empCol[idx][0] = redisDisplay; newState[sc] = redisDisplay; pulled++; }
    } else if (sheetVal !== prevVal) {
      if (sheetVal) {
        if (sheetVal !== redisVal) { toAdd.push({ sitecode: sc, emp_num: sheetVal }); pushed++; }
        empCol[idx][0] = resolveName(sheetVal); newState[sc] = empCol[idx][0];
      } else { if (redisVal) { toRemove.push(sc); removed++; } newState[sc] = REMOVED; }
    } else {
      if (redisDisplay) {
        empCol[idx][0] = redisDisplay; newState[sc] = redisDisplay;
        if (redisDisplay !== prevVal) pulled++;
      } else if (prevVal) { empCol[idx][0] = resolveName(prevVal); newState[sc] = empCol[idx][0]; }
    }
  }

  if (toAdd.length > 0)    pushClaimsToRedis_(toAdd);
  if (toRemove.length > 0) removeClaimsFromRedis_(toRemove);

  // Merge this tab's state into the full state
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
// SITECODE HYPERLINKS
// ════════════════════════════════════════════════════════════════════════════

function setSitecodeLinks_(sheet, ds, scCol, numRows, scOut) {
  if (!scCol) return;
  for (let i = 0; i < numRows; i++) {
    const sc = String(scOut[i][0]).trim();
    if (!sc) continue;
    const cell = sheet.getRange(ds + i, scCol);
    const targetUrl = SITECODE_URL_BASE + encodeURIComponent(sc);
    const existing = cell.getRichTextValue();
    if (existing && existing.getLinkUrl() === targetUrl) continue;
    const richText = SpreadsheetApp.newRichTextValue()
      .setText(sc)
      .setLinkUrl(targetUrl)
      .build();
    cell.setRichTextValue(richText);
  }
}


// ════════════════════════════════════════════════════════════════════════════
// SUMMARY TAB
// ════════════════════════════════════════════════════════════════════════════

function updateSummaryTab_(ss, broodStats, apiResult) {
  let summary = ss.getSheetByName(CONFIG.SUMMARY_TAB);
  if (!summary) {
    summary = ss.insertSheet(CONFIG.SUMMARY_TAB);
    ss.setActiveSheet(summary);
    ss.moveActiveSheet(1);
  }

  summary.clear();

  // ── Config info ──
  summary.getRange(1, 1).setValue('Checkback Tracker').setFontWeight('bold').setFontSize(12);
  summary.getRange(2, 1).setValue('Facility:');
  summary.getRange(2, 2).setValue(CONFIG.FACILITY || 'All');
  summary.getRange(3, 1).setValue('Lookback Days:');
  summary.getRange(3, 2).setValue(CONFIG.LOOKBACK_DAYS);
  summary.getRange(4, 1).setValue('Target:');
  summary.getRange(4, 2).setValue(CONFIG.CHECKBACK_TARGET + (CONFIG.CHECKBACK_TYPE === 'percent' ? '%' : ' sites'));
  summary.getRange(5, 1).setValue('Material:');
  summary.getRange(5, 2).setValue(CONFIG.MATCODE || 'All');

  // ── Brood progress table ──
  const tableStart = 7;
  const headers = ['Brood', 'Facility', 'Start', 'End', 'Sites Treated',
                   'Needed', 'Completed', 'Remaining', 'Progress'];
  summary.getRange(tableStart, 1, 1, headers.length).setValues([headers]);
  summary.getRange(tableStart, 1, 1, headers.length).setFontWeight('bold');

  const rows = [];
  let totalTreated = 0, totalNeeded = 0, totalCompleted = 0, totalRemaining = 0;

  for (const b of broodStats) {
    const progress = b.checkbacks_needed > 0
      ? Math.round(b.checkbacks_completed / b.checkbacks_needed * 100) + '%'
      : '—';
    rows.push([
      b.brood, b.facility, b.start_date, b.end_date,
      b.sites_treated, b.checkbacks_needed, b.checkbacks_completed,
      b.remaining, progress
    ]);
    totalTreated   += b.sites_treated;
    totalNeeded    += b.checkbacks_needed;
    totalCompleted += b.checkbacks_completed;
    totalRemaining += b.remaining;
  }

  if (rows.length > 0) {
    summary.getRange(tableStart + 1, 1, rows.length, headers.length).setValues(rows);
  }

  // Totals row
  const totalsRow = tableStart + rows.length + 1;
  const totalProgress = totalNeeded > 0 ? Math.round(totalCompleted / totalNeeded * 100) + '%' : '—';
  summary.getRange(totalsRow, 1, 1, headers.length).setValues([
    ['TOTAL', '', '', '', totalTreated, totalNeeded, totalCompleted, totalRemaining, totalProgress]
  ]);
  summary.getRange(totalsRow, 1, 1, headers.length).setFontWeight('bold');

  // Timestamp
  const metaRow = totalsRow + 2;
  summary.getRange(metaRow, 1).setValue('Last Updated');
  summary.getRange(metaRow, 2).setValue(new Date().toLocaleString());

  // Auto-resize columns
  for (let c = 1; c <= headers.length; c++) {
    summary.autoResizeColumn(c);
  }

  Logger.log('Summary tab updated: ' + totalNeeded + ' needed, '
    + totalCompleted + ' completed, ' + totalRemaining + ' remaining.');
}


// ════════════════════════════════════════════════════════════════════════════
// AUTO-REFRESH
// ════════════════════════════════════════════════════════════════════════════

function setupAutoRefresh() {
  ScriptApp.getProjectTriggers().forEach(t => {
    if (t.getHandlerFunction() === 'refreshCheckbackData') ScriptApp.deleteTrigger(t);
  });
  ScriptApp.newTrigger('refreshCheckbackData')
    .timeBased()
    .everyMinutes(CONFIG.REFRESH_MINUTES)
    .create();
  Logger.log('Auto-refresh set: every ' + CONFIG.REFRESH_MINUTES + ' minute(s).');
}

function removeAutoRefresh() {
  ScriptApp.getProjectTriggers().forEach(t => {
    if (t.getHandlerFunction() === 'refreshCheckbackData') ScriptApp.deleteTrigger(t);
  });
  Logger.log('Auto-refresh removed.');
}
