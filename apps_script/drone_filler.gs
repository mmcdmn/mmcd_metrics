// ============================================================================
// DRONE TREATMENT FILLER — Google Apps Script
// ============================================================================
// Reads sitecodes from round tabs, fetches drone treatment data from the MMCD
// Metrics API, and fills in Amount, Date, Emp#, and Material columns.
//
// Round logic: The first drone treatment for a site in a given year is Round 1,
// the second is Round 2, etc. Each round tab only gets data for its round.
//
// ★ CUSTOMIZE the CONFIG section below to match YOUR sheet's column layout.
//
// SETUP (one-time):
//   1. Open your drone Google Sheet → Extensions → Apps Script
//   2. Delete anything in Code.gs → paste this entire script
//   3. Go to Project Settings (⚙) → Script Properties. Add two properties:
//       Property:  API_BASE   Value:  https://metrics.mmcd.org/v1
//       Property:  API_KEY    Value:  mmcd-sheets-abc123xyz
//   4. Come back to the editor → Run ▶ refreshDroneData()
//      (Google will ask for permissions — click Allow)
//   5. Optional: Run ▶ setupAutoRefresh() to auto-refresh every 5 minutes
// ============================================================================


// ╔══════════════════════════════════════════════════════════════════════════╗
// ║  ★★★  CONFIGURATION — EDIT THIS SECTION TO MATCH YOUR SHEET  ★★★      ║
// ╚══════════════════════════════════════════════════════════════════════════╝

const CONFIG = {

  // ── API Settings ──────────────────────────────────────────────────────
  YEAR: null,             // Year to query. null = current year.
  REFRESH_MINUTES: 5,    // Auto-refresh interval: 1, 5, 10, 15, or 30

  // ── Row Layout ────────────────────────────────────────────────────────
  DATA_START: 2,         // First data row (1-based). Row 1 = header usually.

  // ── Column Mapping ────────────────────────────────────────────────────
  // Column letters as shown in Google Sheets. Set to '' to SKIP that field.
  //
  // From the Rosemount sheet layout:
  //   A = Sitecode, B = Acres, C = Amount, D = Date, E = Emp#,
  //   F = Material, G = Flight#, H = Supervisor, I = Coordinates, J = Remarks
  //
  SITECODE_COL: 'A',     // Column with sitecodes (REQUIRED)
  AMOUNT_COL:   'C',     // Column for Amount (lbs applied) — from DB
  DATE_COL:     'D',     // Column for treatment date — from DB
  EMP_COL:      'E',     // Column for Emp# — from DB
  MATERIAL_COL: 'F',     // Column for Material code — from DB
  FLIGHT_COL:   '',      // Column for Flight# — NOT from DB (manual entry, leave empty)

  // ── Tab Handling ────────────────────────────────────────────────────────
  // ALL tabs are processed as round tabs EXCEPT those matching the skip list/pattern.
  // Round number is determined by tab order: first non-skip tab = Round 1, second = Round 2, etc.
  SKIP_TABS: ['Summary', 'Config', 'Template', 'Instructions'],
  SKIP_TAB_PATTERN: /master/i,

  // ── Sitecode Pattern ────────────────────────────────────────────────────
  // Regex that matches valid sitecodes. Only rows matching this get processed.
  // Everything else (section headers, totals, blanks, etc.) is automatically skipped.
  // Format: 6-7 digits, dash, 2-3 digits (e.g., "191105-004", "0214150-08")
  SITECODE_PATTERN: /^\d{6,7}-\d{2,3}$/,

  // ── Per-Tab Overrides ─────────────────────────────────────────────────
  // Override column mapping or DATA_START for specific tabs.
  TAB_OVERRIDES: {
    // 'Round 1': { DATA_START: 3 },
  },
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

/** Get script property or throw helpful error */
function getProp_(key) {
  const val = PropertiesService.getScriptProperties().getProperty(key);
  if (!val) throw new Error('Missing Script Property: ' + key + '. Go to Project Settings → Script Properties and add it.');
  return val;
}

/**
 * Build resolved config for a specific tab.
 * Merges TAB_OVERRIDES on top of global CONFIG defaults.
 */
function getTabConfig_(tabName) {
  const ov = (CONFIG.TAB_OVERRIDES || {})[tabName] || {};
  const ds = ov.DATA_START != null ? ov.DATA_START : CONFIG.DATA_START;
  return {
    dataStart: ds,
    col: {
      SITECODE: colNum_(ov.SITECODE_COL != null ? ov.SITECODE_COL : CONFIG.SITECODE_COL),
      AMOUNT:   colNum_(ov.AMOUNT_COL   != null ? ov.AMOUNT_COL   : CONFIG.AMOUNT_COL),
      DATE:     colNum_(ov.DATE_COL     != null ? ov.DATE_COL     : CONFIG.DATE_COL),
      EMP_NUM:  colNum_(ov.EMP_COL      != null ? ov.EMP_COL      : CONFIG.EMP_COL),
      MATERIAL: colNum_(ov.MATERIAL_COL != null ? ov.MATERIAL_COL : CONFIG.MATERIAL_COL),
      FLIGHT:   colNum_(ov.FLIGHT_COL   != null ? ov.FLIGHT_COL   : CONFIG.FLIGHT_COL),
    },
  };
}

/** Read a single column as 2D array */
function readCol_(sheet, dataStart, col, numRows) {
  if (!col || numRows <= 0) return null;
  return sheet.getRange(dataStart, col, numRows, 1).getValues();
}

/** Write a single column from 2D array */
function writeCol_(sheet, dataStart, col, numRows, data) {
  if (!col || !data || numRows <= 0) return;
  try {
    sheet.getRange(dataStart, col, numRows, 1).setValues(data);
    SpreadsheetApp.flush();
  } catch (e) {
    try {
      const range = sheet.getRange(dataStart, col, numRows, 1);
      range.clearDataValidations();
      range.setValues(data);
      SpreadsheetApp.flush();
      Logger.log('writeCol_ retry OK after clearing validation on "' + sheet.getName() + '" col ' + col);
    } catch (e2) {
      Logger.log('writeCol_ FAILED on "' + sheet.getName() + '" col ' + col + ': ' + e2.message);
    }
  }
}

/** Check if a row value is a valid sitecode (only process these, skip everything else) */
function isSitecode_(val) {
  if (!val) return false;
  const s = String(val).trim();
  if (!s) return false;
  return CONFIG.SITECODE_PATTERN.test(s);
}


// ════════════════════════════════════════════════════════════════════════════
// MAIN
// ════════════════════════════════════════════════════════════════════════════

/**
 * Main entry point — call this from the menu or trigger.
 * Fetches all drone treatment data for the year, then fills each round tab.
 */
function refreshDroneData() {
  const lock = LockService.getScriptLock();
  if (!lock.tryLock(10000)) {
    Logger.log('Skipping refresh — another execution is still running.');
    return;
  }

  try {
    const ss = SpreadsheetApp.getActiveSpreadsheet();
    const apiData = fetchDroneChecklist_();
    if (!apiData || apiData.length === 0) {
      Logger.log('No drone treatment data returned from API.');
      return;
    }

    // Build lookup: { round_num: { sitecode: { amount, treatment_date, emp_num, material } } }
    const roundLookup = {};
    for (const row of apiData) {
      const rn = row.round_num;
      if (!roundLookup[rn]) roundLookup[rn] = {};
      roundLookup[rn][row.sitecode] = {
        amount:    row.amount,
        date:      row.treatment_date,
        emp_num:   row.emp_num,
        material:  row.material,
      };
    }

    const sheets = ss.getSheets();
    const skipSet = new Set(CONFIG.SKIP_TABS);
    let totalUpdated = 0;

    // Debug: log available tabs and rounds found
    const tabNames = sheets.map(s => s.getName());
    Logger.log('Sheet tabs: ' + JSON.stringify(tabNames));
    Logger.log('Round lookup keys: ' + JSON.stringify(Object.keys(roundLookup)));
    Logger.log('API returned ' + apiData.length + ' treatment records');

    let roundNum = 0;
    for (const sheet of sheets) {
      const tabName = sheet.getName();

      // Skip tabs in skip list or matching skip pattern
      if (skipSet.has(tabName)) continue;
      if (CONFIG.SKIP_TAB_PATTERN && CONFIG.SKIP_TAB_PATTERN.test(tabName)) continue;

      // Each non-skip tab is the next round
      roundNum++;

      // Get data for this round
      const roundData = roundLookup[roundNum];
      if (!roundData || Object.keys(roundData).length === 0) {
        Logger.log('Tab "' + tabName + '" (Round ' + roundNum + '): no treatment data yet.');
        continue;
      }

      const tabUpdated = fillRoundTab_(sheet, tabName, roundData);
      totalUpdated += tabUpdated;
      Logger.log('Tab "' + tabName + '" (Round ' + roundNum + '): ' + tabUpdated + ' sites filled.');
    }

    Logger.log('Done. Total sites updated across all rounds: ' + totalUpdated);

  } finally {
    lock.releaseLock();
  }
}


/**
 * Fill a single round tab with treatment data.
 * @param {Sheet} sheet - The sheet object
 * @param {string} tabName - Tab name for config lookup
 * @param {Object} roundData - { sitecode: { amount, date, emp_num, material } }
 * @returns {number} count of sites filled
 */
function fillRoundTab_(sheet, tabName, roundData) {
  const tc = getTabConfig_(tabName);
  const lastRow = sheet.getLastRow();
  if (lastRow < tc.dataStart) return 0;

  const numRows = lastRow - tc.dataStart + 1;

  // Read sitecode column
  const sitecodes = readCol_(sheet, tc.dataStart, tc.col.SITECODE, numRows);
  if (!sitecodes) return 0;

  // Debug: log first few sitecodes and what we're looking for
  const sampleSites = sitecodes.slice(0, 10).map(r => String(r[0]).trim()).filter(s => isSitecode_(s));
  const roundKeys = Object.keys(roundData).slice(0, 5);
  Logger.log('Tab "' + tabName + '": first sitecodes = ' + JSON.stringify(sampleSites) + ', looking for = ' + JSON.stringify(roundKeys));

  // Read existing data columns (preserve manual entries where API has no data)
  const existingAmount   = readCol_(sheet, tc.dataStart, tc.col.AMOUNT,   numRows);
  const existingDate     = readCol_(sheet, tc.dataStart, tc.col.DATE,     numRows);
  const existingEmp      = readCol_(sheet, tc.dataStart, tc.col.EMP_NUM,  numRows);
  const existingMaterial = readCol_(sheet, tc.dataStart, tc.col.MATERIAL, numRows);

  // Prepare output arrays (start with existing values)
  const outAmount   = existingAmount   ? existingAmount.map(r => [r[0]])   : Array.from({length: numRows}, () => ['']);
  const outDate     = existingDate     ? existingDate.map(r => [r[0]])     : Array.from({length: numRows}, () => ['']);
  const outEmp      = existingEmp      ? existingEmp.map(r => [r[0]])      : Array.from({length: numRows}, () => ['']);
  const outMaterial = existingMaterial  ? existingMaterial.map(r => [r[0]]) : Array.from({length: numRows}, () => ['']);

  let filled = 0;

  for (let i = 0; i < numRows; i++) {
    const raw = String(sitecodes[i][0]).trim();
    if (!isSitecode_(raw)) continue;

    const site = raw;
    const match = roundData[site];
    if (!match) continue;

    // Fill columns from API data (overwrite existing)
    if (tc.col.AMOUNT && match.amount != null) {
      outAmount[i] = [match.amount];
    }
    if (tc.col.DATE && match.date) {
      // Format as short date (remove leading zeros): "04/30/2025" → "4/30"
      const formatted = formatShortDate_(match.date);
      outDate[i] = [formatted];
    }
    if (tc.col.EMP_NUM && match.emp_num) {
      outEmp[i] = [match.emp_num];
    }
    if (tc.col.MATERIAL && match.material) {
      outMaterial[i] = [match.material];
    }

    filled++;
  }

  // Write back columns
  writeCol_(sheet, tc.dataStart, tc.col.AMOUNT,   numRows, outAmount);
  writeCol_(sheet, tc.dataStart, tc.col.DATE,     numRows, outDate);
  writeCol_(sheet, tc.dataStart, tc.col.EMP_NUM,  numRows, outEmp);
  writeCol_(sheet, tc.dataStart, tc.col.MATERIAL, numRows, outMaterial);

  return filled;
}


/**
 * Format date from API ("MM/DD/YYYY") to short form ("M/D" or "M/D/YYYY" if not current year).
 */
function formatShortDate_(dateStr) {
  if (!dateStr) return '';
  // API returns "MM/DD/YYYY"
  const parts = dateStr.split('/');
  if (parts.length < 3) return dateStr;
  const month = parseInt(parts[0], 10);
  const day   = parseInt(parts[1], 10);
  const year  = parseInt(parts[2], 10);
  const currentYear = new Date().getFullYear();
  if (year === currentYear) {
    return month + '/' + day;
  }
  return month + '/' + day + '/' + year;
}


// ════════════════════════════════════════════════════════════════════════════
// API FETCH
// ════════════════════════════════════════════════════════════════════════════

/**
 * Fetch all drone treatment data for the configured year from the API.
 * Returns array of { sitecode, round_num, treatment_date, amount, emp_num, material }
 */
function fetchDroneChecklist_() {
  const base = getProp_('API_BASE');
  const key  = getProp_('API_KEY');
  const year = CONFIG.YEAR || new Date().getFullYear();

  const url = base + '/private/drone/checklist?year=' + year;
  const response = UrlFetchApp.fetch(url, {
    method: 'get',
    headers: { 'Authorization': 'Bearer ' + key },
    muteHttpExceptions: true,
  });

  if (response.getResponseCode() !== 200) {
    Logger.log('API error ' + response.getResponseCode() + ': ' + response.getContentText());
    return [];
  }

  const payload = JSON.parse(response.getContentText());
  return Array.isArray(payload) ? payload
       : Array.isArray(payload.data) ? payload.data
       : [];
}


// ════════════════════════════════════════════════════════════════════════════
// AUTO-REFRESH & MENU
// ════════════════════════════════════════════════════════════════════════════

/** Set up a time-based trigger to auto-refresh. */
function setupAutoRefresh() {
  // Remove any existing triggers for this function
  const triggers = ScriptApp.getProjectTriggers();
  for (const t of triggers) {
    if (t.getHandlerFunction() === 'refreshDroneData') {
      ScriptApp.deleteTrigger(t);
    }
  }
  ScriptApp.newTrigger('refreshDroneData')
    .timeBased()
    .everyMinutes(CONFIG.REFRESH_MINUTES)
    .create();
  Logger.log('Auto-refresh set: every ' + CONFIG.REFRESH_MINUTES + ' minute(s).');
}

/** Remove the auto-refresh trigger. */
function stopAutoRefresh() {
  const triggers = ScriptApp.getProjectTriggers();
  let removed = 0;
  for (const t of triggers) {
    if (t.getHandlerFunction() === 'refreshDroneData') {
      ScriptApp.deleteTrigger(t);
      removed++;
    }
  }
  Logger.log('Removed ' + removed + ' trigger(s).');
}

/** Add a custom menu to the sheet for easy access. */
function onOpen() {
  SpreadsheetApp.getUi().createMenu('Drone Treatments')
    .addItem('Refresh Now', 'refreshDroneData')
    .addItem('Start Auto-Refresh', 'setupAutoRefresh')
    .addItem('Stop Auto-Refresh', 'stopAutoRefresh')
    .addToUi();
}
