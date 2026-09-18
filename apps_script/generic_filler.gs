// ============================================================================
// GENERIC DATA FILLER — Google Apps Script
// ============================================================================
// Reads sitecodes from your sheet, fetches data from any MMCD Metrics API
// endpoint, and fills columns or highlights rows based entirely on CONFIG.
// Syncs employee claims with Redis so all sheets stay coordinated.
//
// TWO OUTPUT MODES per column:
//   'value'     → writes an API field's value into a sheet column
//   'highlight' → colors cells when a rule is satisfied (clears when not)
//
// SETUP (one-time):
//   1. Open your Google Sheet → Extensions → Apps Script
//   2. Delete anything in Code.gs → paste this entire script
//   3. Go to Project Settings (⚙) → Script Properties. Add two properties:
//       Property:  API_BASE   Value:  https://metrics.mmcd.org/v1
//       Property:  API_KEY    Value:  mmcd-sheets-abc123xyz
//   4. Come back to the editor → Run ▶ refreshData()
//      (Google will ask for permissions — click Allow)
//   5. Optional: Run ▶ setupAutoRefresh() to auto-refresh every minute
// ============================================================================


// ╔══════════════════════════════════════════════════════════════════════════╗
// ║  ★★★  CONFIGURATION — EDIT THIS SECTION TO MATCH YOUR SHEET  ★★★      ║
// ╚══════════════════════════════════════════════════════════════════════════╝

const CONFIG = {

  // ── API ─────────────────────────────────────────────────────────────────
  // API_ENDPOINT: path appended to API_BASE Script Property.
  //   '/private/air-checklist'    — aerial inspection workflow (actions 2 & 4 only)
  //   '/private/site-inspections' — generic; use with ACTIONS to pick any action code(s)
  //   '/private/cattail-checklist'— cattail-specific (action 9 + site metadata)
  //
  // ACTIONS: action codes to keep from the response.
  //   Required when using /private/site-inspections (e.g. [9] or [1,3]).
  //   Ignored by endpoints that don't return an 'action' field.
  API_ENDPOINT:  '/private/site-inspections',
  ACTIONS:       [],          // e.g. [9] for acres-plan treatments, [1,3] for ground inspections
  LOOKBACK_DAYS: 14,
  REFRESH_MINUTES: 1,

  // FILLED_FIELD: which boolean field in the API response marks a site as done.
  // Sites where this is false are treated as open/unclaimed.
  //   'was_completed'  — use this with /private/site-inspections (the generic endpoint)
  //   'was_inspected'  — use this with /private/air-checklist only
  FILLED_FIELD:  'was_completed',

  // ── Sheet layout ─────────────────────────────────────────────────────────
  DATA_START:    2,           // First data row (1-based)
  SITECODE_COL: 'A',          // Column that holds sitecodes (REQUIRED)
  SKIP_TABS:    ['Summary', 'Config', 'Template', 'Instructions'],
  SKIP_ROW_PATTERN: /^book\s|^(sites?|total|totals|done)\b|^\d{1,4}$/i,

  // ── Claiming ──────────────────────────────────────────────────────────────
  // When an employee types their emp# in CLAIM_COL on an open (unfilled) site,
  // the claim is pushed to Redis so all sheets see it.
  // ONLY the CLAIM_COL cell on that row gets the CLAIM_COLOR highlight.
  ENABLE_CLAIMS: true,
  CLAIM_COL:    'D',          // Column where emp# is typed
  CLAIM_COLOR:  '#FFF2CC',    // Background color for the active claim cell

  // ── Preserve from reset ───────────────────────────────────────────────────
  // These columns are NEVER cleared when a site ages out of the lookback window.
  // Typically Remarks (notes).
  PRESERVE_COLS: ['C', 'I'],

  // ── Output columns ────────────────────────────────────────────────────────
  //
  // Valid source field names from /private/site-inspections:
  //   inspdate     — date of the inspection or treatment (use format: 'date')
  //   numdip       — larvae per dip count
  //   wet          — percent wet code (0–9, A, S)
  //   emp1         — primary employee number
  //   emp2         — secondary employee number (often null)
  //   matcode      — material/product code (treatments)
  //   amts         — amount applied (treatments)
  //   acres        — acres of the site
  //   acres_plan   — planned acres (action 9 only)
  //   airgrnd_plan — air/ground plan code
  //   sampnum_yr   — sample number/year
  //   posttrt_p    — post-treatment (checkbacks)
  //   reinspect    — reinspect flag
  //   rems1        — remarks line 1
  //   rems2        — remarks line 2
  //   comments     — free-text comments field
  //   action       — the action code itself (e.g. '9', 'D', '1')
  //   activeTrt    — true if material is still within its effect window (uses effect_days)
  //
  //  mode: 'value'
  //    source: one of the field names above
  //    col:    column letter to write into
  //    format: 'date' to format dates nicely; omit for raw value
  //
  //  mode: 'highlight'
  //    color: CSS hex color to apply when rule matches
  //    cols:  array of column letters, e.g. ['C','D']
  //           OR null to color ALL value-mode columns on that row
  //    rule:  { field, op, value }  — op is '>' '<' '=' '>=' '<='
  //           Omit rule entirely to always highlight when site has data.
  //
  COLUMNS: [
    { mode: 'value', source: 'inspdate',   col: 'C', format: 'date' },
    { mode: 'value', source: 'numdip',     col: 'D' },
    { mode: 'value', source: 'emp1',       col: 'E' },
    { mode: 'value', source: 'acres',      col: 'F' },
    { mode: 'value', source: 'acres_plan', col: 'G' },

    // ── Highlight examples (uncomment and change) ──
    // Green on cols D–G when numdip > 0:
    // { mode: 'highlight', color: '#C6EFCE', cols: ['D','E','F','G'],
    //   rule: { field: 'numdip', op: '>', value: 0 } },
    //
    // Red on whole row when wet = 0 (dry):
    // { mode: 'highlight', color: '#FFCCCC', cols: null,
    //   rule: { field: 'wet', op: '=', value: 0 } },
  ],

  // ── Per-tab overrides ─────────────────────────────────────────────────────
  // Any top-level CONFIG key can be overridden for a specific tab (by exact name).
  // COLUMNS overrides replace the entire array for that tab.
  TAB_OVERRIDES: {
    // 'Sheet2': { LOOKBACK_DAYS: 30, CLAIM_COLOR: '#FCE4EC' },
  },
};


// ╔══════════════════════════════════════════════════════════════════════════╗
// ║  END OF CONFIGURATION                                                    ║
// ╚══════════════════════════════════════════════════════════════════════════╝


// ════════════════════════════════════════════════════════════════════════════
// UTILITIES
// ════════════════════════════════════════════════════════════════════════════

/** Convert column letter(s) to 1-based number: 'A'→1, 'B'→2, 'AA'→27 */
function colNum_(letter) {
  if (!letter) return 0;
  let n = 0;
  for (let i = 0; i < letter.length; i++) {
    n = n * 26 + (letter.toUpperCase().charCodeAt(i) - 64);
  }
  return n;
}

/** Normalize truthy/falsey config values. */
function toBool_(value, defaultValue) {
  if (value === undefined || value === null) return defaultValue;
  if (typeof value === 'boolean') return value;
  const s = String(value).trim().toLowerCase();
  if (s === 'true' || s === '1' || s === 'yes' || s === 'on') return true;
  if (s === 'false' || s === '0' || s === 'no' || s === 'off' || s === '') return false;
  return defaultValue;
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
  try {
    sheet.getRange(dataStart, col, numRows, 1).setValues(data);
    SpreadsheetApp.flush();
  } catch (e) {
    try {
      const range = sheet.getRange(dataStart, col, numRows, 1);
      range.clearDataValidations();
      range.setValues(data);
      SpreadsheetApp.flush();
      Logger.log('writeCol_ retry OK after clearing validation on tab "' + sheet.getName() + '" col ' + col);
    } catch (e2) {
      Logger.log('writeCol_ FAILED on tab "' + sheet.getName() + '" col ' + col + ': ' + e2.message);
    }
  }
}

function isSkipRow_(val, pattern) {
  if (!val) return true;
  return (pattern || CONFIG.SKIP_ROW_PATTERN).test(val);
}

/**
 * Build resolved config for a specific tab, merging TAB_OVERRIDES on top of
 * global CONFIG. Returns an enriched object with pre-computed column numbers.
 */
function getTabConfig_(tabName) {
  const ov = (CONFIG.TAB_OVERRIDES || {})[tabName] || {};

  function pick(key) { return ov[key] !== undefined ? ov[key] : CONFIG[key]; }

  const columns      = pick('COLUMNS') || [];
  const preserveCols = pick('PRESERVE_COLS') || [];

  const valueCols = columns
    .filter(c => c.mode === 'value' && c.col)
    .map(c => ({ source: c.source, col: colNum_(c.col), colLetter: c.col, format: c.format || '' }));

  const hlCols = columns
    .filter(c => c.mode === 'highlight')
    .map(c => ({
      color: c.color || '#FFFF00',
      rule:  c.rule || null,
      // resolve cols list; null means all value columns (computed at call time)
      cols: c.cols ? c.cols.map(colNum_) : null,
    }));

  const allValueColNums = valueCols.map(c => c.col);

  return {
    dataStart:    pick('DATA_START')    || 2,
    scCol:        colNum_(pick('SITECODE_COL') || 'A'),
    filledField:  pick('FILLED_FIELD')  || 'was_inspected',
    lookback:     pick('LOOKBACK_DAYS') || 14,
    endpoint:     pick('API_ENDPOINT')  || '/private/air-checklist',
    actions:      pick('ACTIONS')       || [],
    enableClaims: toBool_(pick('ENABLE_CLAIMS'), true),
    claimCol:     colNum_(pick('CLAIM_COL') || 'D'),
    claimColor:   pick('CLAIM_COLOR')   || '#FFF2CC',
    preserveSet:  new Set(preserveCols.map(colNum_)),
    skipPattern:  pick('SKIP_ROW_PATTERN') || CONFIG.SKIP_ROW_PATTERN,
    skipTabs:     new Set(pick('SKIP_TABS') || []),
    valueCols,
    hlCols,
    allValueColNums,
  };
}


// ════════════════════════════════════════════════════════════════════════════
// API FETCH
// ════════════════════════════════════════════════════════════════════════════

/**
 * Fetch data from the configured endpoint.
 * Sends actions as a query param if configured; also filters client-side.
 * Returns sitecode → row object map.
 */
function fetchData_(tabCfg) {
  const base    = getProp_('API_BASE');
  const key     = getProp_('API_KEY');
  const actions = tabCfg.actions || [];
  let url = base + tabCfg.endpoint + '?lookback_days=' + tabCfg.lookback;
  if (actions.length > 0) url += '&actions=' + actions.join(',');

  const r = UrlFetchApp.fetch(url, {
    method: 'get',
    headers: { 'Authorization': 'Bearer ' + key },
    muteHttpExceptions: true,
  });
  if (r.getResponseCode() !== 200) {
    Logger.log('API error ' + r.getResponseCode() + ': ' + r.getContentText());
    return {};
  }

  const payload = JSON.parse(r.getContentText());
  const rows = Array.isArray(payload) ? payload
             : Array.isArray(payload.data) ? payload.data
             : [];

  // Client-side action filter — only active when the API returns an 'action'
  // field AND ACTIONS is non-empty. Skips rows where row.action is undefined
  // (older endpoints that don't return the field pass through unfiltered).
  const actionSet = actions.length > 0 ? new Set(actions.map(String)) : null;
  const map = {};
  for (const row of rows) {
    if (!row.sitecode) continue;
    if (actionSet && row.action !== undefined && row.action !== null &&
        !actionSet.has(String(row.action))) continue;
    map[row.sitecode] = row;
  }
  return map;
}


// ════════════════════════════════════════════════════════════════════════════
// RULE EVALUATION
// ════════════════════════════════════════════════════════════════════════════

/**
 * Evaluate a highlight rule against an API row.
 * rule = { field, op, value }  — op is '>', '<', '=', '>=', '<='
 * Returns true to apply the highlight color.
 */
function evalRule_(apiRow, rule) {
  if (!rule) return true;  // no rule = always highlight
  const raw = apiRow[rule.field];
  if (raw === undefined || raw === null || raw === '') return false;
  const v = Number(raw);
  const t = Number(rule.value);
  if (rule.op === '>')  return v >  t;
  if (rule.op === '<')  return v <  t;
  if (rule.op === '=')  return String(raw) == String(rule.value); // loose eq for string/number
  if (rule.op === '>=') return v >= t;
  if (rule.op === '<=') return v <= t;
  return false;
}


// ════════════════════════════════════════════════════════════════════════════
// WRITE VALUES
// ════════════════════════════════════════════════════════════════════════════

/**
 * Write value-mode columns. For filled sites: pull values from API.
 * For unfilled / expired sites: clear (unless column is in preserveSet).
 * Returns { colNum: [values array] } map for batch writing.
 */
function buildValueArrays_(siteRows, dataRows, lookup, tabCfg) {
  const out = {};
  for (const vc of tabCfg.valueCols) {
    out[vc.col] = Array.from({ length: dataRows }, () => ['']);
  }

  for (const [sc, idx] of Object.entries(siteRows)) {
    const info = lookup[sc];
    const filled = info && info[tabCfg.filledField];

    for (const vc of tabCfg.valueCols) {
      if (tabCfg.preserveSet.has(vc.col)) continue;  // never touch preserved cols
      if (filled && info[vc.source] !== undefined && info[vc.source] !== null) {
        let val = info[vc.source];
        if (vc.format === 'date' && val) val = String(val);
        out[vc.col][idx][0] = val;
      }
      // else: already '' (reset)
    }
  }
  return out;
}


// ════════════════════════════════════════════════════════════════════════════
// APPLY HIGHLIGHTS
// ════════════════════════════════════════════════════════════════════════════

/**
 * Apply / clear highlight-mode COLUMNS for each site row.
 * Multiple highlight entries can overlap; later entries in COLUMNS win
 * if they target the same cell.
 */
function applyHighlights_(sheet, ds, siteRows, lookup, tabCfg) {
  if (tabCfg.hlCols.length === 0) return;

  for (const [sc, idx] of Object.entries(siteRows)) {
    const info   = lookup[sc];
    const filled = info && info[tabCfg.filledField];
    const row    = ds + idx;

    for (const hl of tabCfg.hlCols) {
      // Resolve which columns to color
      const targetCols = hl.cols !== null ? hl.cols : tabCfg.allValueColNums;
      if (!targetCols || targetCols.length === 0) continue;

      const applies = filled ? evalRule_(info, hl.rule) : false;
      const color   = applies ? hl.color : null;

      // Apply to each target column individually to avoid large contiguous-range assumptions
      for (const cn of targetCols) {
        if (tabCfg.preserveSet.has(cn)) continue;
        sheet.getRange(row, cn).setBackground(color);
      }
    }
  }
}


// ════════════════════════════════════════════════════════════════════════════
// CLAIM HIGHLIGHTING
// ════════════════════════════════════════════════════════════════════════════

/**
 * Apply claim color to CLAIM_COL cell when the site is claimed but unfilled.
 * Clears claim color on filled sites or sites with no active claim.
 */
function applyClaimHighlights_(sheet, ds, siteRows, lookup, claimColData, tabCfg) {
  if (!tabCfg.enableClaims || !tabCfg.claimCol) return;
  for (const [sc, idx] of Object.entries(siteRows)) {
    const info   = lookup[sc];
    const filled = info && info[tabCfg.filledField];
    const empVal = String(claimColData[idx][0] || '').trim();
    const claimed = !filled && empVal;
    sheet.getRange(ds + idx, tabCfg.claimCol).setBackground(claimed ? tabCfg.claimColor : null);
  }
}


// ════════════════════════════════════════════════════════════════════════════
// SITECODE HYPERLINKS
// ════════════════════════════════════════════════════════════════════════════

const SITECODE_URL_BASE = 'https://webster.mmcd.org/map?search=';

function setSitecodeLinks_(sheet, ds, scCol, siteRows) {
  if (!scCol) return;
  for (const [sc, idx] of Object.entries(siteRows)) {
    const cell      = sheet.getRange(ds + idx, scCol);
    const targetUrl = SITECODE_URL_BASE + encodeURIComponent(sc);
    const existing  = cell.getRichTextValue();
    if (existing && existing.getLinkUrl() === targetUrl) continue;
    cell.setRichTextValue(
      SpreadsheetApp.newRichTextValue().setText(sc).setLinkUrl(targetUrl).build()
    );
  }
}


// ════════════════════════════════════════════════════════════════════════════
// CLAIMS SYNC  (identical state machine to inspection_filler.gs)
// ════════════════════════════════════════════════════════════════════════════

function fetchClaims_() {
  try {
    const base = getProp_('API_BASE');
    const key  = getProp_('API_KEY');
    const r = UrlFetchApp.fetch(
      base + '/private/claims?lookback_days=' + CONFIG.LOOKBACK_DAYS,
      { headers: { 'Authorization': 'Bearer ' + key }, muteHttpExceptions: true }
    );
    if (r.getResponseCode() === 200) {
      const j    = JSON.parse(r.getContentText());
      const list = Array.isArray(j) ? j : (j.data || []);
      const map  = {};
      for (const c of list) {
        if (c.sitecode && c.emp_num)
          map[c.sitecode] = { emp_num: String(c.emp_num), emp_name: c.emp_name || '', time: c.time || '' };
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
      payload: JSON.stringify({ sitecodes }),
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
      const j    = JSON.parse(r.getContentText());
      const list = Array.isArray(j) ? j : (j.data || []);
      const map  = {};
      for (const e of list) {
        if (e.emp_num && e.shortname) map[String(e.emp_num)] = String(e.shortname);
      }
      return map;
    }
  } catch (e) { Logger.log('fetchEmployeeLookup_: ' + e.message); }
  return {};
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

/**
 * Two-way claim sync between sheet EMP column and Redis.
 * filledField: the API response key that marks a site as done.
 * Returns { pushed, pulled, removed } counts.
 */
function syncClaims_(siteRows, lookup, empCol, filledField) {
  const REMOVED     = '__REMOVED__';
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
    // Skip sites that are already filled — claims only apply to open sites
    if (lookup[sc] && lookup[sc][filledField]) continue;

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

  saveClaimState_(Object.assign({}, claimState, newState));
  return { pushed, pulled, removed };
}


// ════════════════════════════════════════════════════════════════════════════
// MAIN
// ════════════════════════════════════════════════════════════════════════════

function refreshData() {
  const lock = LockService.getScriptLock();
  if (!lock.tryLock(10000)) {
    Logger.log('Skipping refresh — another execution is still running.');
    return;
  }

  try {
    const ss      = SpreadsheetApp.getActiveSpreadsheet();
    const sheets  = ss.getSheets();
    const globalSkip = new Set(CONFIG.SKIP_TABS || []);
    globalSkip.add('Summary');

    // Use global config for the initial fetch (tabs that need different endpoints
    // will re-fetch inside the loop via their own tabCfg — see below).
    const globalCfg  = getTabConfig_('__global__');
    const globalData = fetchData_(globalCfg);

    for (const sheet of sheets) {
      const name = sheet.getName();
      if (globalSkip.has(name)) continue;

      const tc      = getTabConfig_(name);
      const ds      = tc.dataStart;
      const lastRow = sheet.getLastRow();
      if (lastRow < ds) continue;
      const numRows = lastRow - ds + 1;

      // Decide which lookup to use: re-fetch only if this tab's endpoint / actions differ
      const needsOwnFetch = tc.endpoint !== globalCfg.endpoint ||
                            JSON.stringify(tc.actions) !== JSON.stringify(globalCfg.actions) ||
                            tc.lookback !== globalCfg.lookback;
      const lookup = needsOwnFetch ? fetchData_(tc) : globalData;

      // Build siteRows map: sitecode → 0-based row index within data range
      const scValues = sheet.getRange(ds, tc.scCol, numRows, 1).getValues();
      const siteRows = {};
      let lastDataIdx = -1;
      for (let i = 0; i < numRows; i++) {
        const sc = String(scValues[i][0]).trim();
        if (!sc || isSkipRow_(sc, tc.skipPattern)) continue;
        if (!lookup[sc]) continue;
        siteRows[sc] = i;
        if (i > lastDataIdx) lastDataIdx = i;
      }
      if (Object.keys(siteRows).length === 0) continue;
      const dataRows = lastDataIdx + 1;

      // ── Write value columns ──
      const valueArrays = buildValueArrays_(siteRows, dataRows, lookup, tc);
      for (const [colNum, data] of Object.entries(valueArrays)) {
        writeCol_(sheet, ds, Number(colNum), dataRows, data);
      }

      // ── Apply highlight-mode columns ──
      applyHighlights_(sheet, ds, siteRows, lookup, tc);

      // ── Claims sync ──
      let claimResult = { pushed: 0, pulled: 0, removed: 0 };
      let empCol = null;
      if (tc.enableClaims && tc.claimCol) {
        empCol = readCol_(sheet, ds, tc.claimCol, dataRows);
        claimResult = syncClaims_(siteRows, lookup, empCol, tc.filledField);
        writeCol_(sheet, ds, tc.claimCol, dataRows, empCol);
        applyClaimHighlights_(sheet, ds, siteRows, lookup, empCol, tc);
      }

      // ── Sitecode hyperlinks ──
      setSitecodeLinks_(sheet, ds, tc.scCol, siteRows);

      Logger.log('Tab "' + name + '": ' + Object.keys(siteRows).length + ' sites. '
        + 'Claims: +' + claimResult.pushed + ' -' + claimResult.removed
        + ' ↓' + claimResult.pulled);
    }

    Logger.log('refreshData complete: ' + new Date().toLocaleTimeString());
  } catch (e) {
    Logger.log('refreshData CRASHED: ' + e.message + '\n' + e.stack);
  } finally {
    lock.releaseLock();
  }
}


// ════════════════════════════════════════════════════════════════════════════
// CLEAR ALL CLAIMS
// ════════════════════════════════════════════════════════════════════════════

/**
 * Clear all uninspected claim values from every sheet tab.
 * Also removes those claims from Redis so they don't reappear on next refresh.
 */
function clearAllClaimsFromSheets() {
  const lock = LockService.getScriptLock();
  if (!lock.tryLock(10000)) {
    Logger.log('Skipping clearAllClaimsFromSheets — another execution is still running.');
    return;
  }

  const ss       = SpreadsheetApp.getActiveSpreadsheet();
  const sheets   = ss.getSheets();
  const skipSet  = new Set(CONFIG.SKIP_TABS || []);
  skipSet.add('Summary');

  const globalCfg  = getTabConfig_('__global__');
  const globalData = fetchData_(globalCfg);

  const toRemoveMap = {};
  let totalCleared  = 0;

  for (const sheet of sheets) {
    const name = sheet.getName();
    if (skipSet.has(name)) continue;

    const tc      = getTabConfig_(name);
    if (!tc.enableClaims || !tc.claimCol) continue;

    const ds      = tc.dataStart;
    const lastRow = sheet.getLastRow();
    if (lastRow < ds) continue;
    const numRows = lastRow - ds + 1;

    const needsOwnFetch = tc.endpoint !== globalCfg.endpoint ||
                          JSON.stringify(tc.actions) !== JSON.stringify(globalCfg.actions);
    const lookup = needsOwnFetch ? fetchData_(tc) : globalData;

    const scValues = sheet.getRange(ds, tc.scCol, numRows, 1).getValues();
    let lastDataIdx = -1;
    for (let i = 0; i < numRows; i++) {
      const sc = String(scValues[i][0] || '').trim();
      if (sc && !isSkipRow_(sc, tc.skipPattern) && lookup[sc]) lastDataIdx = i;
    }
    if (lastDataIdx < 0) continue;
    const dataRows = lastDataIdx + 1;

    const empCol = readCol_(sheet, ds, tc.claimCol, dataRows);
    if (!empCol) continue;

    let clearedThisTab = 0;
    for (let i = 0; i < dataRows; i++) {
      const sc = String(scValues[i][0] || '').trim();
      if (!sc || isSkipRow_(sc, tc.skipPattern) || !lookup[sc]) continue;
      const info = lookup[sc];
      if (info && info[tc.filledField]) continue;
      if (!String(empCol[i][0] || '').trim()) continue;
      empCol[i][0] = '';
      toRemoveMap[sc] = true;
      // Clear claim highlight
      sheet.getRange(ds + i, tc.claimCol).setBackground(null);
      clearedThisTab++;
    }

    if (clearedThisTab > 0) {
      writeCol_(sheet, ds, tc.claimCol, dataRows, empCol);
      totalCleared += clearedThisTab;
      Logger.log('Tab "' + name + '": cleared ' + clearedThisTab + ' claim(s).');
    }
  }

  const toRemove = Object.keys(toRemoveMap);
  if (toRemove.length > 0) removeClaimsFromRedis_(toRemove);
  saveClaimState_({});

  lock.releaseLock();
  Logger.log('clearAllClaimsFromSheets done: ' + totalCleared + ' cleared, '
    + toRemove.length + ' removed from Redis.');
}


// ════════════════════════════════════════════════════════════════════════════
// AUTO-REFRESH
// ════════════════════════════════════════════════════════════════════════════

function setupAutoRefresh() {
  ScriptApp.getProjectTriggers().forEach(t => {
    if (t.getHandlerFunction() === 'refreshData') ScriptApp.deleteTrigger(t);
  });
  ScriptApp.newTrigger('refreshData')
    .timeBased()
    .everyMinutes(CONFIG.REFRESH_MINUTES)
    .create();
  Logger.log('Auto-refresh set: every ' + CONFIG.REFRESH_MINUTES + ' minute(s).');
}

function removeAutoRefresh() {
  ScriptApp.getProjectTriggers().forEach(t => {
    if (t.getHandlerFunction() === 'refreshData') ScriptApp.deleteTrigger(t);
  });
  Logger.log('Auto-refresh removed.');
}
