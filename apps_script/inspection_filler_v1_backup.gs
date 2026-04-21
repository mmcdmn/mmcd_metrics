// ============================================================================
// INSPECTION DATA FILLER — Google Apps Script
// ============================================================================
// Reads sitecodes from column A of the active sheet, fetches inspection data
// from the MMCD Metrics API, and fills in Emp#, Date, % Wet, #/Dip, and
// Sample columns. D (Emp#) also acts as the Claim Emp ID — users type their
// emp# to claim uninspected sites; Redis keeps them in sync across sheets.
//
// SETUP:
//   1. Open your Google Sheet → Extensions → Apps Script
//   2. Paste this entire script
//   3. Go to Project Settings → Script Properties. Add:
//       API_BASE:  https://metrics.mmcd.org/v1
//       API_KEY:   <your key>
//   4. Run refreshInspectionData() manually, or run setupAutoRefresh() once
// ============================================================================

const CONFIG = {
  LOOKBACK_DAYS:   2,   // Days back to check for inspections (1–14)
  LOOKBACK_ABOVE_THRESH: 7, // Days to keep hot-site data (dip ≥ threshold); 0 = same as LOOKBACK_DAYS
  REFRESH_MINUTES: 1,   // Auto-refresh interval in minutes (1, 5, 10, 15, 30)
};

// ── Column positions (1-based) matching your sheet layout ────────────────────
// A=Sitecode  B=Priority  C=Acres  D=Emp#  E=Date  F=%Wet  G=#/Dip  H=Sample
// D (Emp#) doubles as the Claim Emp ID — user types their emp# to claim a site;
// filled automatically by the API once the site has been inspected.
const COL = {
  SITECODE:  1,   // A
  EMP_NUM:   4,   // D  — inspector / claim emp ID
  DATE:      5,   // E
  PCT_WET:   6,   // F
  NUM_DIP:   7,   // G
  SAMPLE:    8,   // H
};

const HEADER_ROW = 2;   // Row with column headers
const DATA_START = 3;   // First row that may contain data or "Book" dividers

// ── Map raw wet code → dropdown validation label ─────────────────────────────
const WET_LABELS = {
  '0': '0 = DRY',
  '1': '1 = 1-19%',
  '2': '2 = 20-29%',
  '3': '3 = 30-39%',
  '4': '4 = 40-49%',
  '5': '5 = 50-59%',
  '6': '6 = 60-69%',
  '7': '7 = 70-79%',
  '8': '8 = 80-89%',
  '9': '9 = 90-99%',
  'A': 'A = >100%',
  'S': 'S = Snow/Unk',
};

function getProp_(key) {
  const val = PropertiesService.getScriptProperties().getProperty(key);
  if (!val) throw new Error('Missing Script Property: ' + key);
  return val;
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

  // 1. Fetch data once (shared across all tabs)
  const apiData = fetchChecklist_();
  if (!apiData || apiData.length === 0) {
    Logger.log('No data returned from API.');
    return;
  }

  const lookup = {};
  for (const row of apiData) {
    if (row.sitecode) lookup[row.sitecode] = row;
  }

  // 2. Process every tab that has data rows
  const sheets = ss.getSheets();
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
    if (name === 'Summary') continue;  // Skip the Summary tab
    const lastRow = sheet.getLastRow();
    if (lastRow < DATA_START) continue;

    const numRows = lastRow - DATA_START + 1;

    // Read sitecodes from column A
    const scValues = sheet.getRange(DATA_START, COL.SITECODE, numRows, 1).getValues();
    const siteRows = {};
    for (let i = 0; i < numRows; i++) {
      const sc = String(scValues[i][0]).trim();
      if (sc && !/^book\s/i.test(sc)) siteRows[sc] = i;
    }
    if (Object.keys(siteRows).length === 0) continue;

    // Read current values (batch read)
    const empCol = sheet.getRange(DATA_START, COL.EMP_NUM, numRows, 1).getValues();
    const datCol = sheet.getRange(DATA_START, COL.DATE,    numRows, 1).getValues();
    const wetCol = sheet.getRange(DATA_START, COL.PCT_WET, numRows, 1).getValues();
    const dipCol = sheet.getRange(DATA_START, COL.NUM_DIP, numRows, 1).getValues();
    const smpCol = sheet.getRange(DATA_START, COL.SAMPLE,  numRows, 1).getValues();

    // Fill in inspected data — clear stale data for sites outside lookback
    let updated = 0;
    for (const [sc, idx] of Object.entries(siteRows)) {
      const info = lookup[sc];
      let keep = false;
      if (info && info.was_inspected) {
        const inspDate = new Date(info.last_insp_date);
        const withinNormal = inspDate >= normalCutoff;
        const dipNum = parseFloat(info.dip_count);
        const aboveThresh = !isNaN(dipNum) && dipNum >= threshold;
        keep = withinNormal || aboveThresh;
      }

      if (keep) {
        empCol[idx][0] = info.inspector_name || '';
        datCol[idx][0] = info.last_insp_date || '';
        const wetCode = info.pct_wet != null ? String(info.pct_wet).trim() : '';
        wetCol[idx][0] = WET_LABELS[wetCode] || wetCode;
        dipCol[idx][0] = info.dip_count  != null ? info.dip_count : '';
        smpCol[idx][0] = info.sampnum_yr != null ? String(info.sampnum_yr) : '';
        updated++;
      } else {
        // Not kept — clear stale data.
        // Emp column left alone; claims sync handles it.
        const hadData = datCol[idx][0] || wetCol[idx][0] || dipCol[idx][0] || smpCol[idx][0];
        datCol[idx][0] = '';
        wetCol[idx][0] = '';
        dipCol[idx][0] = '';
        smpCol[idx][0] = '';
        if (hadData) Logger.log('  Cleared stale data for ' + sc + ' (outside lookback / below threshold)');
      }
    }

    // Write inspection columns (emp written after claims sync)
    sheet.getRange(DATA_START, COL.DATE,    numRows, 1).setValues(datCol);
    sheet.getRange(DATA_START, COL.PCT_WET, numRows, 1).setValues(wetCol);
    sheet.getRange(DATA_START, COL.NUM_DIP, numRows, 1).setValues(dipCol);
    sheet.getRange(DATA_START, COL.SAMPLE,  numRows, 1).setValues(smpCol);

    // Sync claims for uninspected sites
    const claimResult = syncClaims_(siteRows, lookup, empCol);

    // Write emp column (inspector names + resolved claims)
    sheet.getRange(DATA_START, COL.EMP_NUM, numRows, 1).setValues(empCol);

    // Update row 1 stats for THIS tab
    const stats = updateRemainingCount(sheet, siteRows, dipCol);
    tabStats.push({ name, remaining: stats.remaining, checked: stats.checked,
                    atThreshold: stats.atThreshold, hotAcres: stats.hotAcres,
                    threshold: stats.threshold });

    // Add hyperlinks to sitecode cells
    setSitecodeLinks_(sheet, siteRows);

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

  // No facility or priority filter — returns all air sites so we can match
  // any sitecode regardless of facility/zone.
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

/**
 * Fetch emp_num → shortname lookup from the API.
 * Returns an object like { '1905': 'Alex D', '2010': 'Smith' }.
 */
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
      Logger.log('fetchEmployeeLookup_: ' + Object.keys(map).length + ' employees.');
      return map;
    }
  } catch (e) { Logger.log('fetchEmployeeLookup_: ' + e.message); }
  return {};
}


// ════════════════════════════════════════════════════════════════════════════
// CLAIMS SYNC
// ════════════════════════════════════════════════════════════════════════════

/**
 * Sync D (Emp#) column with Redis claims for UNINSPECTED sites.
 * Inspected sites already have their inspector name filled by the API.
 *
 * Employee numbers typed by users (e.g. "1905") are pushed to Redis,
 * which resolves them to names (e.g. "Smith"). After pushing, we re-fetch
 * so the sheet shows the name immediately instead of the number.
 *
 * Same merge strategy as sheet_builder.gs:
 *   - If user added/changed a claim → push to Redis
 *   - If user cleared a claim → remove from Redis
 *   - If Redis has a claim the user hasn't touched → pull into sheet
 *
 * @param {Object} siteRows  sitecode → row index map
 * @param {Object} lookup    sitecode → API row (to check was_inspected)
 * @param {Array}  empCol    2D array for col D (mutated in place)
 * @returns {{ pushed: number, pulled: number, removed: number }}
 */
function syncClaims_(siteRows, lookup, empCol) {
  const claimCol = empCol;
  const REMOVED    = '__REMOVED__';
  const redisClaims = fetchClaims_();
  const claimState  = loadClaimState_();
  const empLookup   = fetchEmployeeLookup_();  // emp_num → shortname
  const newState    = {};
  const toAdd       = [];
  const toRemove    = [];
  let pushed = 0, pulled = 0, removed = 0;

  /** Resolve a value: if it looks like an emp number, convert to name */
  function resolveName(val) {
    if (!val) return val;
    const s = String(val).trim();
    if (empLookup[s]) return empLookup[s];
    return s;
  }

  for (const [sc, idx] of Object.entries(siteRows)) {
    // Skip inspected sites — their emp# is already set from the API
    if (lookup[sc] && lookup[sc].was_inspected) continue;

    const rawSheetVal = String(claimCol[idx][0] || '').trim();
    const sheetVal    = rawSheetVal;  // raw value for comparison
    const stateEntry  = claimState[sc];
    const isPending   = stateEntry === REMOVED;
    const prevVal     = isPending ? '' : (stateEntry || '');
    const isNew       = stateEntry === undefined;
    const redisVal    = redisClaims[sc] ? String(redisClaims[sc].emp_num || '').trim() : '';
    // Prefer resolved name from Redis; fall back to local lookup; then emp_num
    const redisName   = redisClaims[sc] ? String(redisClaims[sc].emp_name || '').trim() : '';
    const redisDisplay = resolveName(redisName || redisVal);

    if (isPending) {
      if (sheetVal) {
        claimCol[idx][0] = resolveName(sheetVal);
        newState[sc] = claimCol[idx][0];
        if (sheetVal !== redisVal) {
          toAdd.push({ sitecode: sc, emp_num: sheetVal });
          pushed++;
        }
      } else if (redisVal) {
        toRemove.push(sc);
        newState[sc] = REMOVED;
        removed++;
      }

    } else if (isNew) {
      if (sheetVal) {
        if (sheetVal !== redisVal) {
          toAdd.push({ sitecode: sc, emp_num: sheetVal });
          pushed++;
        }
        // Show resolved name immediately
        claimCol[idx][0] = (redisDisplay && sheetVal === redisVal) ? redisDisplay : resolveName(sheetVal);
        newState[sc] = claimCol[idx][0];
      } else if (redisDisplay) {
        claimCol[idx][0] = redisDisplay;
        newState[sc] = redisDisplay;
        pulled++;
      }

    } else if (sheetVal !== prevVal) {
      if (sheetVal) {
        if (sheetVal !== redisVal) {
          toAdd.push({ sitecode: sc, emp_num: sheetVal });
          pushed++;
        }
        claimCol[idx][0] = resolveName(sheetVal);
        newState[sc] = claimCol[idx][0];
      } else {
        if (redisVal) { toRemove.push(sc); removed++; }
        newState[sc] = REMOVED;
      }

    } else {
      // No user change → Redis is authoritative
      if (redisDisplay) {
        claimCol[idx][0] = redisDisplay;
        newState[sc] = redisDisplay;
        if (redisDisplay !== prevVal) pulled++;
      } else if (prevVal) {
        claimCol[idx][0] = resolveName(prevVal);
        newState[sc] = claimCol[idx][0];
      }
    }
  }

  // Push / remove
  if (toAdd.length > 0)    pushClaimsToRedis_(toAdd);
  if (toRemove.length > 0) removeClaimsFromRedis_(toRemove);

  // Merge this tab's state into the full state (don't wipe other tabs' entries)
  const mergedState = Object.assign({}, claimState, newState);
  saveClaimState_(mergedState);
  return { pushed, pulled, removed };
}

/** Save claim state to Document Properties for next-refresh comparison */
function saveClaimState_(state) {
  try {
    PropertiesService.getDocumentProperties().setProperty('CLAIM_STATE', JSON.stringify(state || {}));
  } catch (e) { Logger.log('saveClaimState_ ERROR: ' + e.message); }
}

/** Load claim state from previous refresh */
function loadClaimState_() {
  try {
    const raw = PropertiesService.getDocumentProperties().getProperty('CLAIM_STATE');
    return raw ? JSON.parse(raw) : {};
  } catch (e) { return {}; }
}


// ════════════════════════════════════════════════════════════════════════════
// REMAINING COUNT  (Row 1 stats)
// ════════════════════════════════════════════════════════════════════════════

/**
 * Count uninspected sites (no #/Dip value) and update the stats in row 1.
 * Row 1 layout:  A="Remaining"  B=<count>  C="Acres"  D=<acres>  E="Threshold ="  F=<threshold>
 *
 * Can be called standalone (from a button / menu) or is called automatically
 * at the end of refreshInspectionData().
 *
 * @param {Sheet}  [sheet]    Optional — defaults to active sheet.
 * @param {Object} [siteRows] Optional — pre-built sitecode→idx map (skips re-read).
 * @param {Array}  [dipCol]   Optional — pre-read #/Dip column (skips re-read).
 */
function updateRemainingCount(sheet, siteRows, dipCol) {
  if (!sheet) sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  const lastRow = sheet.getLastRow();
  if (lastRow < DATA_START) return { remaining: 0, checked: 0, atThreshold: 0, hotAcres: 0, threshold: 2 };

  const numRows = lastRow - DATA_START + 1;

  // Build siteRows if not provided
  if (!siteRows) {
    siteRows = {};
    const scValues = sheet.getRange(DATA_START, COL.SITECODE, numRows, 1).getValues();
    for (let i = 0; i < numRows; i++) {
      const sc = String(scValues[i][0]).trim();
      if (sc && !/^book\s/i.test(sc)) siteRows[sc] = i;
    }
  }

  // Read #/Dip column if not provided
  if (!dipCol) {
    dipCol = sheet.getRange(DATA_START, COL.NUM_DIP, numRows, 1).getValues();
  }

  // Read Acres column
  const acresCol = sheet.getRange(DATA_START, 3, numRows, 1).getValues(); // col C

  // Fetch threshold from API
  let threshold = 2;
  try {
    const base = getProp_('API_BASE');
    const r = UrlFetchApp.fetch(base + '/public/threshold', { muteHttpExceptions: true });
    if (r.getResponseCode() === 200) {
      const j = JSON.parse(r.getContentText());
      threshold = j.threshold || 2;
    }
  } catch (e) { /* keep default */ }

  let remaining = 0, checked = 0, atThreshold = 0, hotAcres = 0;
  for (const [, idx] of Object.entries(siteRows)) {
    const dip = dipCol[idx] ? dipCol[idx][0] : '';
    if (dip === '' || dip === null || dip === undefined) {
      remaining++;
    } else {
      checked++;
      const dipNum = parseFloat(dip);
      if (!isNaN(dipNum) && dipNum >= threshold) {
        atThreshold++;
        const a = parseFloat(acresCol[idx][0]);
        if (!isNaN(a)) hotAcres += a;
      }
    }
  }

  hotAcres = Math.round(hotAcres * 100) / 100;

  // Write row 1 stats
  sheet.getRange(1, 1).setValue('Remaining');
  sheet.getRange(1, 2).setValue(remaining);
  sheet.getRange(1, 3).setValue('Hot Acres');
  sheet.getRange(1, 4).setValue(hotAcres);
  sheet.getRange(1, 5).setValue('Threshold =');
  sheet.getRange(1, 6).setValue(threshold);

  Logger.log('Stats row: ' + remaining + ' remaining, '
    + checked + ' checked, ' + atThreshold + ' at threshold, '
    + hotAcres + ' hot acres >= ' + threshold + '/dip.');

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
    ss.setActiveSheet(summary);
    ss.moveActiveSheet(1);
  }

  summary.clear();

  const headers = ['Tab', 'Remaining', 'Checked', '# At Threshold', 'Hot Acres'];
  summary.getRange(1, 1, 1, headers.length).setValues([headers]);
  summary.getRange(1, 1, 1, headers.length).setFontWeight('bold');

  const rows = tabStats.map(t => [t.name, t.remaining, t.checked, t.atThreshold, t.hotAcres]);
  if (rows.length > 0) {
    summary.getRange(2, 1, rows.length, headers.length).setValues(rows);
  }

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

  const threshold = tabStats.length > 0 ? tabStats[0].threshold : 2;
  summary.getRange(totalsRow + 2, 1).setValue('Threshold');
  summary.getRange(totalsRow + 2, 2).setValue(threshold);
  summary.getRange(totalsRow + 3, 1).setValue('Last Updated');
  summary.getRange(totalsRow + 3, 2).setValue(new Date().toLocaleString());

  for (let c = 1; c <= headers.length; c++) {
    summary.autoResizeColumn(c);
  }

  Logger.log('Summary tab updated: ' + totals.remaining + ' remaining, '
    + totals.checked + ' checked, ' + totals.atThreshold + ' at threshold, '
    + totals.hotAcres + ' hot acres.');
}


// ════════════════════════════════════════════════════════════════════════════
// SITECODE HYPERLINKS
// ════════════════════════════════════════════════════════════════════════════

const SITECODE_URL_BASE = 'https://data.mmcd.org/m1/map?search=';

/**
 * Set hyperlinks on sitecode cells so they open the MMCD map.
 * Only sets a link if the cell doesn't already have one.
 */
function setSitecodeLinks_(sheet, siteRows) {
  for (const [sc, idx] of Object.entries(siteRows)) {
    const cell = sheet.getRange(DATA_START + idx, COL.SITECODE);
    const existing = cell.getRichTextValue();
    if (existing && existing.getLinkUrl()) continue;
    const richText = SpreadsheetApp.newRichTextValue()
      .setText(sc)
      .setLinkUrl(SITECODE_URL_BASE + encodeURIComponent(sc))
      .build();
    cell.setRichTextValue(richText);
  }
}


// ════════════════════════════════════════════════════════════════════════════
// AUTO-REFRESH
// ════════════════════════════════════════════════════════════════════════════

/** Run once to set up the 1-minute auto-refresh trigger */
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

/** Remove the auto-refresh trigger */
function removeAutoRefresh() {
  ScriptApp.getProjectTriggers().forEach(t => {
    if (t.getHandlerFunction() === 'refreshInspectionData') ScriptApp.deleteTrigger(t);
  });
  Logger.log('Auto-refresh removed.');
}
