// ============================================================================
// CATTAIL INSPECTIONS 2026 — Google Apps Script
// ============================================================================
// Reads sitecodes from the "Sites" tab and fills inspection data from the API.
// Generates a "Summary" tab (inspected acres, plan breakdown) and a
// "Reinspects" tab (sites with reinspect='t' records this year).
//
// SHEET SETUP (do ONCE before running):
//   1. Create a Google Sheet with three tabs: Sites, Summary, Reinspects
//   2. In "Sites" row 1 add headers:
//        A=Sitecode  B=Acres  C=Emp#  D=Last Insp Date  E=Wet  F=Dip  G=Plan  H=Reinspect
//   3. Paste sitecodes into col A and acres into col B from
//        extra/cattail_sites_2026-07-27.csv  (skip the header row — data in row 2+)
//
// SCRIPT SETUP:
//   1. Open the sheet → Extensions → Apps Script → paste this script
//   2. Project Settings (⚙) → Script Properties → add:
//        API_BASE  =  https://metrics.mmcd.org/v1
//        API_KEY   =  <your bearer token>
//   3. Run refreshData() once manually to authorize, then run setupTrigger()
//      for hourly auto-refresh.
//
// CLAIMING:
//   Type your emp# in col C (Emp#) for any unclaimed site to claim it.
//   Claims are synced to Redis so other sheets/users see them immediately.
//   Once the site is inspected the API emp1 takes over automatically.
// ============================================================================

const CONFIG = {
  YEAR:          new Date().getFullYear(),
  SITES_TAB:     'Sites',
  SUMMARY_TAB:   'Summary',
  REINSPECT_TAB: 'Reinspects',
  DATA_START:    2,      // row 1 = header
  COL: {
    SITECODE:  1,        // A — pre-populated from CSV
    ACRES:     2,        // B — pre-populated from CSV
    EMP:       3,        // C — inspector emp# / claim  (API + Redis)
    LAST_INSP: 4,        // D — last inspection date    (API)
    WET:       5,        // E — wetness code            (API)
    DIP:       6,        // F — numdip                  (API)
    PLAN:      7,        // G — Air/Drone/Ground/None/Unknown (API)
    REINSPECT: 8,        // H — "Y" if reinspect record exists (API)
  }
};

const PLAN_NAMES        = { A: 'Air', D: 'Drone', G: 'Ground', N: 'None', U: 'Unknown' };
const SITECODE_URL_BASE = 'https://webster.mmcd.org/map?search=';

// ============================================================================
// MAIN ENTRY POINT
// ============================================================================

function refreshData() {
  const lock = LockService.getScriptLock();
  if (!lock.tryLock(30000)) {
    Logger.log('Skipping refresh — another run is still in progress.');
    return;
  }
  try {
    Logger.log('refreshData start ' + new Date().toLocaleTimeString());

    const apiData = fetchCattailChecklist_();
    if (!apiData) return;

    const inspMap   = {};
    const reinspSet = {};
    (apiData.inspections || []).forEach(row => {
      inspMap[String(row.sitecode).trim()] = row;
    });
    (apiData.reinspects || []).forEach(row => {
      reinspSet[String(row.sitecode).trim()] = row;
    });

    const ss = SpreadsheetApp.getActiveSpreadsheet();

    // Build the set of sitecodes this sheet actually tracks (all data tabs).
    // Reinspects are then filtered to only those sites, so we don't show
    // reinspect flags from other facilities or areas.
    const sheetSitecodes = new Set();
    ss.getSheets().forEach(sheet => {
      const name = sheet.getName();
      if (name === CONFIG.SUMMARY_TAB || name === CONFIG.REINSPECT_TAB) return;
      const lastRow = sheet.getLastRow();
      if (lastRow < CONFIG.DATA_START) return;
      sheet.getRange(CONFIG.DATA_START, CONFIG.COL.SITECODE,
                     lastRow - CONFIG.DATA_START + 1, 1)
           .getValues()
           .forEach(([sc]) => { if (sc) sheetSitecodes.add(String(sc).trim()); });
    });
    const filteredReinspects = (apiData.reinspects || [])
      .filter(r => sheetSitecodes.has(String(r.sitecode).trim()));
    Logger.log('Reinspects: ' + (apiData.reinspects || []).length + ' from API → '
               + filteredReinspects.length + ' in this sheet');

    updateSitesTab_(ss, inspMap, reinspSet);
    updateSummaryTab_(ss);
    updateReinspectsTab_(ss, filteredReinspects);

    Logger.log('refreshData done ' + new Date().toLocaleTimeString());
  } catch (e) {
    Logger.log('refreshData crashed: ' + e.message + '\n' + e.stack);
  } finally {
    lock.releaseLock();
  }
}

// ============================================================================
// API FETCH
// ============================================================================

function getProp_(key) {
  return PropertiesService.getScriptProperties().getProperty(key);
}

function fetchCattailChecklist_() {
  const base = getProp_('API_BASE');
  const key  = getProp_('API_KEY');
  if (!base || !key) {
    Logger.log('Missing Script Properties: API_BASE and/or API_KEY');
    return null;
  }

  const url = base + '/private/cattail-checklist?year=' + CONFIG.YEAR;
  let resp;
  try {
    resp = UrlFetchApp.fetch(url, {
      headers: { 'Authorization': 'Bearer ' + key },
      muteHttpExceptions: true,
    });
  } catch (e) {
    Logger.log('Network error: ' + e.message);
    return null;
  }

  if (resp.getResponseCode() !== 200) {
    Logger.log('API error ' + resp.getResponseCode() + ': ' + resp.getContentText().substring(0, 300));
    return null;
  }

  const data = JSON.parse(resp.getContentText());
  Logger.log('API: ' + (data.insp_count || 0) + ' inspections, ' + (data.reinspect_count || 0) + ' reinspects');
  return data;
}

// ============================================================================
// CLAIMS — Redis-backed site claiming (same pattern as inspection_filler.gs)
// ============================================================================

function fetchClaims_() {
  try {
    const base = getProp_('API_BASE');
    const key  = getProp_('API_KEY');
    const r = UrlFetchApp.fetch(
      base + '/private/claims',
      { headers: { 'Authorization': 'Bearer ' + key }, muteHttpExceptions: true }
    );
    if (r.getResponseCode() === 200) {
      const j    = JSON.parse(r.getContentText());
      const list = Array.isArray(j) ? j : (j.data || []);
      const map  = {};
      for (const c of list) {
        if (c.sitecode && c.emp_num) {
          map[String(c.sitecode).trim()] = String(c.emp_num).trim();
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
    Logger.log('Pushed ' + claims.length + ' claim(s) to Redis');
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
    Logger.log('Removed ' + sitecodes.length + ' claim(s) from Redis');
  } catch (e) { Logger.log('removeClaimsFromRedis_: ' + e.message); }
}

// ============================================================================
// SITES TAB — fill cols C–H from API data + Redis claims
// ============================================================================

function updateSitesTab_(ss, inspMap, reinspSet) {
  const sheet = ss.getSheetByName(CONFIG.SITES_TAB);
  if (!sheet) { Logger.log('No "Sites" tab'); return; }

  const lastRow = sheet.getLastRow();
  if (lastRow < CONFIG.DATA_START) return;
  const dataRows = lastRow - CONFIG.DATA_START + 1;

  const sitecodes = sheet.getRange(CONFIG.DATA_START, CONFIG.COL.SITECODE, dataRows, 1).getValues();

  // Read current EMP col to detect user-typed claims before overwriting
  const empVals = sheet.getRange(CONFIG.DATA_START, CONFIG.COL.EMP, dataRows, 1).getValues();

  // Fetch active claims from Redis
  const claimMap = fetchClaims_();

  const newClaims = [];
  const toRemove  = [];

  // Build 6 cols: EMP, LAST_INSP, WET, DIP, PLAN, REINSPECT
  const writeData = sitecodes.map((r, i) => {
    const sc    = String(r[0]).trim();
    const insp  = sc ? inspMap[sc] : null;
    const typed = String(empVals[i][0]).trim();

    if (insp) {
      // Site is inspected — use API emp1; clear any pending claim
      const apiEmp = String(insp.emp1 || '').trim();
      if (typed && typed !== apiEmp) toRemove.push(sc);
      const dip  = (insp.numdip !== null && insp.numdip !== undefined && insp.numdip !== '')
                   ? Number(insp.numdip) : '';
      const plan = PLAN_NAMES[insp.airgrnd_plan] || (insp.airgrnd_plan || '');
      return [apiEmp, insp.last_insp_date || '', insp.wet || '', dip, plan, reinspSet[sc] ? 'Y' : ''];
    }

    // Not yet inspected — handle claiming
    const redisClaim = claimMap[sc] || '';
    let empToShow = redisClaim;

    if (typed && typed !== redisClaim) {
      // User typed a new emp# — push as claim
      newClaims.push({ sitecode: sc, emp_num: typed });
      empToShow = typed;
    }

    return [empToShow, '', '', '', '', reinspSet[sc] ? 'Y' : ''];
  });

  if (newClaims.length > 0) pushClaimsToRedis_(newClaims);
  if (toRemove.length  > 0) removeClaimsFromRedis_(toRemove);

  // Write cols C–H (6 cols starting at EMP=3)
  sheet.getRange(CONFIG.DATA_START, CONFIG.COL.EMP, dataRows, 6).setValues(writeData);
  setSitecodeLinks_(sheet, CONFIG.DATA_START, dataRows);
  Logger.log('Sites tab: ' + dataRows + ' rows written, ' + newClaims.length + ' new claim(s)');
}

// ============================================================================
// SUMMARY TAB — computed totals from the Sites tab
// ============================================================================

function updateSummaryTab_(ss) {
  const sitesSheet   = ss.getSheetByName(CONFIG.SITES_TAB);
  const summarySheet = ss.getSheetByName(CONFIG.SUMMARY_TAB);
  if (!sitesSheet || !summarySheet) return;

  const lastRow = sitesSheet.getLastRow();
  if (lastRow < CONFIG.DATA_START) return;
  const dataRows = lastRow - CONFIG.DATA_START + 1;

  // Read cols A–H (sitecode, acres, emp, last_insp, wet, dip, plan, reinspect)
  const data = sitesSheet.getRange(CONFIG.DATA_START, 1, dataRows, 8).getValues();

  let totalSites = 0, totalAcres = 0, inspSites = 0, inspAcres = 0;
  const planCounts = { Air: 0, Drone: 0, Ground: 0, None: 0, Unknown: 0 };
  const planAcres  = { Air: 0, Drone: 0, Ground: 0, None: 0, Unknown: 0 };

  data.forEach(row => {
    const sc       = String(row[0]).trim();
    if (!sc) return;
    const acres    = parseFloat(row[1]) || 0;
    const lastInsp = String(row[3]).trim();   // col D (index 3)
    const plan     = String(row[6]).trim();   // col G (index 6)

    totalSites++;
    totalAcres += acres;

    if (lastInsp) {
      inspSites++;
      inspAcres += acres;
      if (plan && planCounts[plan] !== undefined) {
        planCounts[plan]++;
        planAcres[plan] += acres;
      }
    }
  });

  const pct = totalSites > 0 ? (100 * inspSites / totalSites).toFixed(1) + '%' : '—';
  const now = new Date().toLocaleString();

  summarySheet.clearContents();
  summarySheet.getRange('A1').setValue('Cattail Inspections ' + CONFIG.YEAR + ' — Summary');
  summarySheet.getRange('A2').setValue('Last updated: ' + now);

  summarySheet.getRange('A4:B4').setValues([['Metric', 'Value']]);
  summarySheet.getRange('A5:B10').setValues([
    ['Total Sites',        totalSites],
    ['Total Acres',        totalAcres.toFixed(2)],
    ['Inspected Sites',    inspSites],
    ['Inspected Acres',    inspAcres.toFixed(2)],
    ['Not Yet Inspected',  totalSites - inspSites],
    ['% Sites Complete',   pct],
  ]);

  summarySheet.getRange('A12:C12').setValues([['Plan Type', 'Sites', 'Acres']]);
  let r = 13;
  ['Air', 'Drone', 'Ground', 'None', 'Unknown'].forEach(plan => {
    summarySheet.getRange(r, 1, 1, 3).setValues([
      [plan, planCounts[plan], planAcres[plan].toFixed(2)]
    ]);
    r++;
  });

  Logger.log('Summary tab updated');
}

// ============================================================================
// REINSPECTS TAB
// Sites where reinspect='t' was flagged during an inspection this year.
//   orig_insp_date = date the reinspect flag was set ("come back to this site")
//   reinspect_date = date of any subsequent inspection after that flag (blank if pending)
// ============================================================================

function updateReinspectsTab_(ss, reinspects) {
  const sheet = ss.getSheetByName(CONFIG.REINSPECT_TAB);
  if (!sheet) { Logger.log('No "Reinspects" tab'); return; }

  // Build acres map from Sites tab
  const acresMap = {};
  const sitesSheet = ss.getSheetByName(CONFIG.SITES_TAB);
  if (sitesSheet && sitesSheet.getLastRow() >= CONFIG.DATA_START) {
    const n = sitesSheet.getLastRow() - CONFIG.DATA_START + 1;
    sitesSheet.getRange(CONFIG.DATA_START, 1, n, 2).getValues()
      .forEach(r => { if (r[0]) acresMap[String(r[0]).trim()] = r[1]; });
  }

  sheet.clearContents();
  sheet.getRange(1, 1, 1, 5).setValues([[
    'Sitecode', 'Acres', 'Orig Insp Date', 'Reinsp Date', 'Status'
  ]]);

  if (!reinspects.length) {
    Logger.log('No reinspect records this year');
    return;
  }

  const dataRows = reinspects.map(r => {
    const sc   = String(r.sitecode).trim();
    const done = r.reinspect_done === true || r.reinspect_done === 'true';
    return [
      sc,
      acresMap[sc] !== undefined ? acresMap[sc] : '',
      r.orig_insp_date || '',            // when reinspect flag was set
      r.reinspect_date || '',            // blank until they go back
      done ? 'Reinspected' : 'Pending Reinspect',
    ];
  });

  sheet.getRange(2, 1, dataRows.length, 5).setValues(dataRows);

  // Add hyperlinks to sitecode col (col A, starting row 2)
  setSitecodeLinks_(sheet, 2, dataRows.length);
  Logger.log('Reinspects tab: ' + dataRows.length + ' sites');
}

// ============================================================================
// SITECODE HYPERLINKS
// ============================================================================

/**
 * Batch-set hyperlinks on sitecode cells (col A) so they open the MMCD map.
 * Reads all RichText values at once to avoid per-cell API overhead.
 */
function setSitecodeLinks_(sheet, startRow, numRows) {
  if (!numRows) return;
  const col   = CONFIG.COL.SITECODE;
  const range = sheet.getRange(startRow, col, numRows, 1);
  const values    = range.getValues();
  const richTexts = range.getRichTextValues();

  let changed = false;
  const updated = richTexts.map((row, i) => {
    const sc = String(values[i][0]).trim();
    if (!sc || !/^\d{4}/.test(sc)) return row;
    const url      = SITECODE_URL_BASE + encodeURIComponent(sc);
    const existing = row[0];
    if (existing && existing.getLinkUrl() === url) return row;
    changed = true;
    return [SpreadsheetApp.newRichTextValue().setText(sc).setLinkUrl(url).build()];
  });

  if (changed) range.setRichTextValues(updated);
}

// ============================================================================
// TRIGGER SETUP
// ============================================================================

function setupTrigger() {
  ScriptApp.getProjectTriggers().forEach(t => ScriptApp.deleteTrigger(t));
  ScriptApp.newTrigger('refreshData').timeBased().everyHours(1).create();
  Logger.log('Hourly trigger set for refreshData');
}
