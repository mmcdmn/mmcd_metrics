// ============================================================================
// AIR INSPECTION WORKLOAD MANAGER — Google Apps Script
// ============================================================================
// Fetches RED air checklist data for [Sr] facility from the MMCD Metrics API.
// Creates per-FOS tabs with color coding, claim tracking, and progress stats.
//
// SETUP:
//   1. Open your Google Sheet → Extensions → Apps Script
//   2. Paste this entire script
//   3. Go to Project Settings → Script Properties. Add these two values:
//       API_BASE:  https://metrics.mmcd.org/v1
//       API_KEY:   <your key>
//   4. Run refreshChecklist() or attach it to a button / time trigger
// ============================================================================

const CONFIG = {
  FACILITY:  'SR',
  ZONE:      '1,2',
};

// Read API_BASE and API_KEY from Script Properties (set in Project Settings)
function getProp_(key) {
  const val = PropertiesService.getScriptProperties().getProperty(key);
  if (!val) throw new Error('Missing Script Property: ' + key + '. Go to Project Settings → Script Properties and add it.');
  return val;
}

// ── Column definitions ──────────────────────────────────────────────────────
// DB-sourced columns (auto-filled, protected from accidental edits)
const DB_COLUMNS = [
  'Township', 'Section', 'Sitecode', 'Priority', 'Acres', 'AirMap', 'Status',
  'Inspector', '#/Dip', '% Wet', 'Sample #', 'Bug Status',
  'RA', 'Remarks', 'Treatment',
];
// Manual columns (editable by employees — preserved across refreshes)
const MANUAL_COLUMNS = ['Claim Emp ID', 'New Notes', 'Drone', 'Needs Sample'];
const ALL_COLUMNS    = [...DB_COLUMNS, ...MANUAL_COLUMNS];

// Header row positions
const STATS_ROW  = 1;   // Overview bar
const FILTER_ROW = 2;   // Filter / info bar
const HEADER_ROW = 3;   // Column headers
const DATA_START = 4;   // First data row

// Colors
const C = {
  HEADER_BG:      '#1a237e',   // dark indigo — table header background
  MANUAL_HEADER_BG:'#0b5ed7',  // blue for editable columns
  HEADER_FG:      '#ffffff',   // white — table header text color
  SECTION_BAR_BG: '#000000',   // black section separator
  TOWN_BAR_BG:    '#263238',   // township separator
  TOWN_BAR_FG:    '#ffffff',
  DONE_BG:        '#e8f5e9',   // light green — inspected
  CLAIMED_BG:     '#e3f2fd',   // light blue — claimed but not inspected
  TODO_BG:        '#ffffff',   // white — unclaimed, not inspected
  PRIORITY_RED:   '#f8d7da',
  PRIORITY_YELLOW:'#fff3cd',
  PRIORITY_BLUE:  '#d1ecf1',
  PRIORITY_GREEN: '#d4edda',
  PRIORITY_PURPLE:'#e2d6f3',
  RED_BUGS:       '#ffcdd2',   // light red — red bug indicator
  PENDING_LAB:    '#fff9c4',   // light yellow — pending lab results
  BLUE_BUGS:      '#bbdefb',   // light blue — blue bug indicator
  RA_BG:          '#f3e5f5',   // light purple — restricted area
  STATS_BG:       '#fafafa',   // near-white grey — stats box background
  REMAIN_RED:     '#c62828',   // dark red — remaining count (high/bad)
  REMAIN_GREEN:   '#2e7d32',   // dark green — remaining count (low/good)
  PCT_GREEN:      '#e8f5e9',   // light green — percentage threshold good
  PCT_YELLOW:     '#fff9c4',   // light yellow — percentage threshold warning
  PCT_RED:        '#ffcdd2',   // light red — percentage threshold bad
};


// ════════════════════════════════════════════════════════════════════════════
// ENTRY POINTS
// ════════════════════════════════════════════════════════════════════════════

/** Main refresh — call from button or trigger */
function refreshChecklist() {
  const ss = SpreadsheetApp.getActiveSpreadsheet();

  // 1. Get threshold-based lookback
  const thresholdDays = getThresholdLookback_();

  // 2. Fetch Sr facility data
  const rows = fetchChecklistData_(thresholdDays);
  if (!rows || rows.length === 0) {
    SpreadsheetApp.getUi().alert('No Sr RED air sites returned from API.');
    return;
  }

  // 3. Save existing manual edits before overwriting
  const manualSnap = snapshotManualData_(ss);

  // 4. Group by FOS
  const byFos = {};
  for (const r of rows) {
    const fos = r.fos_name || 'Unknown FOS';
    (byFos[fos] = byFos[fos] || []).push(r);
  }

  // 5. Write each FOS tab
  for (const fos of Object.keys(byFos).sort()) {
    writeFosTab_(ss, safeName_(fos), byFos[fos], manualSnap, thresholdDays);
  }

  // 6. Write summary tab
  writeSummary_(ss, byFos, thresholdDays);

  SpreadsheetApp.flush();
}

/** Alias so triggers / buttons bound to either name work */
function refreshAirChecklist() { refreshChecklist(); }


// ════════════════════════════════════════════════════════════════════════════
// API CALLS
// ════════════════════════════════════════════════════════════════════════════

function getThresholdLookback_() {
  try {
    const base = getProp_('API_BASE');
    const r = UrlFetchApp.fetch(base + '/public/threshold',
                                { muteHttpExceptions: true });
    if (r.getResponseCode() === 200) {
      const j = JSON.parse(r.getContentText());
      return j.lookback_days || 2;
    }
  } catch (e) { Logger.log('threshold: ' + e.message); }
  return 2;
}

function fetchChecklistData_(lookback) {
  const base = getProp_('API_BASE');
  const key  = getProp_('API_KEY');

  const url = base + '/private/air-checklist'
    + '?facility=' + CONFIG.FACILITY
    + '&lookback_days=' + lookback
    + '&zone=' + encodeURIComponent(CONFIG.ZONE);

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


// ════════════════════════════════════════════════════════════════════════════
// MANUAL DATA PRESERVATION
// ════════════════════════════════════════════════════════════════════════════

/**
 * Reads every existing FOS tab and builds a map:
 *   { "TabName::sitecode" → [ClaimedBy, NewNotes, Drone, NeedsSample] }
 */
function snapshotManualData_(ss) {
  const snap = {};
  const mStart = DB_COLUMNS.length + 1;          // first manual column
  const mCount = MANUAL_COLUMNS.length;

  for (const sh of ss.getSheets()) {
    const name = sh.getName();
    if (name === 'Summary' || name === 'Config') continue;
    const last = sh.getLastRow();
    if (last < DATA_START) continue;

    const numRows = last - DATA_START + 1;
    const codes  = sh.getRange(DATA_START, 1, numRows, 1).getValues();     // col A
    const manual = sh.getRange(DATA_START, mStart, numRows, mCount).getValues();

    for (let i = 0; i < numRows; i++) {
      const sc = String(codes[i][0]).trim();
      if (!sc) continue;
      const hasAny = manual[i].some(v => v !== '' && v != null);
      if (hasAny) snap[name + '::' + sc] = manual[i];
    }
  }
  return snap;
}


// ════════════════════════════════════════════════════════════════════════════
// FOS TAB WRITER
// ════════════════════════════════════════════════════════════════════════════

function writeFosTab_(ss, tabName, rows, manualSnap, thresholdDays) {
  let sh = ss.getSheetByName(tabName);
  if (!sh) sh = ss.insertSheet(tabName);
  sh.clear();
  sh.clearConditionalFormatRules();

  // Sort by township, section, not-inspected first, then sitecode
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

  // ── Row 1: Stats banner ────────────────────────────────────────────────
  const statsText = remaining === 0
    ? `All ${total} sites inspected | Threshold: ${thresholdDays}`
    : `Remaining: ${remaining} | Inspected: ${inspected} / ${total} (${pct}%) | Red Bugs: ${redBugs} | Threshold: ${thresholdDays}`;

  sh.getRange(STATS_ROW, 1, 1, ALL_COLUMNS.length).merge()
    .setValue(statsText)
    .setFontSize(13).setFontWeight('bold')
    .setFontColor(remaining === 0 ? C.REMAIN_GREEN : C.REMAIN_RED)
    .setBackground(C.STATS_BG)
    .setVerticalAlignment('middle');
  sh.setRowHeight(STATS_ROW, 36);

  // ── Row 2: Info bar ────────────────────────────────────────────────────
  sh.getRange(FILTER_ROW, 1, 1, ALL_COLUMNS.length).merge()
    .setValue(`${tabName} | Sr Facility | Claim sites by typing your Emp ID in "Claim Emp ID" | Last Refresh: ${new Date().toLocaleString()}`)
    .setFontSize(10).setFontColor('#666666')
    .setBackground(C.STATS_BG);

  // ── Row 3: Column headers ──────────────────────────────────────────────
  const hdr = sh.getRange(HEADER_ROW, 1, 1, ALL_COLUMNS.length);
  hdr.setValues([ALL_COLUMNS])
     .setBackground(C.HEADER_BG).setFontColor(C.HEADER_FG)
     .setFontWeight('bold').setHorizontalAlignment('center');
  const manualStart = DB_COLUMNS.length + 1;
  sh.getRange(HEADER_ROW, manualStart, 1, MANUAL_COLUMNS.length)
    .setBackground(C.MANUAL_HEADER_BG);

  // ── Row 4+: Data rows ─────────────────────────────────────────────────
  if (rows.length === 0) { finalizeTab_(sh); return; }

  const townGroups = {};
  for (const row of rows) {
    const townFallback = (row.sitecode || '').substring(0, 4);
    const town = (row.township_name || '').trim() || townFallback || 'Unknown Township';
    const section = row.sectcode || 'No Section';
    if (!townGroups[town]) townGroups[town] = {};
    if (!townGroups[town][section]) townGroups[town][section] = [];
    townGroups[town][section].push(row);
  }

  const output = [];
  const townNames = Object.keys(townGroups).sort();
  for (const town of townNames) {
    output.push({ type: 'town', label: `TOWNSHIP: ${town}` });
    const sections = Object.keys(townGroups[town]).sort();
    for (const section of sections) {
      output.push({ type: 'section', label: `Section ${section}` });
      const secRows = townGroups[town][section].sort((a, b) => {
        return ((a.was_inspected === b.was_inspected) ? 0 : (a.was_inspected ? 1 : -1))
          || (a.sitecode || '').localeCompare(b.sitecode || '');
      });
      for (const row of secRows) {
        const done = row.was_inspected;
        const dbVals = [
          town,
          row.sectcode   || '',
          row.sitecode   || '',
          row.priority   || '',
          row.acres      != null ? row.acres : '',
          row.airmap_num || '',
          done ? 'Y' : '',
          done ? (row.inspector_name || '') : '',
          row.dip_count  != null ? row.dip_count : '',
          row.pct_wet    != null ? row.pct_wet : '',
          row.sampnum_yr || '',
          row.bug_status || '',
          row.restricted_area ? 'Y' : '',
          row.remarks    || '',
          row.has_active_treatment ? (row.active_material || 'Y') : '',
        ];
        const key = tabName + '::' + (row.sitecode || '');
        const manual = manualSnap[key] || new Array(MANUAL_COLUMNS.length).fill('');
        output.push({ type: 'data', row, values: [...dbVals, ...manual] });
      }
    }
  }

  const grid = output.map(item => {
    if (item.type === 'data') return item.values;
    return [item.label, ...new Array(ALL_COLUMNS.length - 1).fill('')];
  });

  sh.getRange(DATA_START, 1, grid.length, ALL_COLUMNS.length).setValues(grid);

  // ── Styling and color coding (row by row) ─────────────────────────────
  for (let i = 0; i < output.length; i++) {
    const r   = DATA_START + i;
    const entry = output[i];

    if (entry.type === 'town') {
      const rng = sh.getRange(r, 1, 1, ALL_COLUMNS.length);
      rng.merge().setBackground(C.TOWN_BAR_BG).setFontColor(C.TOWN_BAR_FG)
        .setFontWeight('bold').setFontSize(12).setHorizontalAlignment('left');
      sh.setRowHeight(r, 28);
      continue;
    }

    if (entry.type === 'section') {
      const rng = sh.getRange(r, 1, 1, ALL_COLUMNS.length);
      rng.merge().setBackground(C.SECTION_BAR_BG).setFontColor('#ffffff')
        .setFontWeight('bold').setHorizontalAlignment('left');
      sh.setRowHeight(r, 24);
      continue;
    }

    const row = entry.row;
    const rng = sh.getRange(r, 1, 1, ALL_COLUMNS.length);
    const claimedBy = String(entry.values[DB_COLUMNS.length] || '').trim();

    // Row background
    if (row.was_inspected) {
      rng.setBackground(C.DONE_BG);
    } else if (claimedBy) {
      rng.setBackground(C.CLAIMED_BG);
    }

    // Status column — green bold checkmark
    const statusCol = DB_COLUMNS.indexOf('Status') + 1;
    if (row.was_inspected) {
      sh.getRange(r, statusCol)
        .setFontColor(C.REMAIN_GREEN).setFontWeight('bold')
        .setHorizontalAlignment('center');
    }

    // Priority cell
    const priCol = DB_COLUMNS.indexOf('Priority') + 1;
    const pri = String(row.priority || '').toUpperCase();
    if (pri === 'RED') sh.getRange(r, priCol).setBackground(C.PRIORITY_RED).setFontWeight('bold');
    if (pri === 'YELLOW') sh.getRange(r, priCol).setBackground(C.PRIORITY_YELLOW).setFontWeight('bold');
    if (pri === 'BLUE') sh.getRange(r, priCol).setBackground(C.PRIORITY_BLUE).setFontWeight('bold');
    if (pri === 'GREEN') sh.getRange(r, priCol).setBackground(C.PRIORITY_GREEN).setFontWeight('bold');
    if (pri === 'PURPLE') sh.getRange(r, priCol).setBackground(C.PRIORITY_PURPLE).setFontWeight('bold');

    // Bug Status cell
    const bugCol = DB_COLUMNS.indexOf('Bug Status') + 1;
    if (row.bug_status === 'Red Bugs') {
      sh.getRange(r, bugCol).setBackground(C.RED_BUGS).setFontWeight('bold');
    } else if (row.bug_status === 'Pending Lab') {
      sh.getRange(r, bugCol).setBackground(C.PENDING_LAB);
    } else if (row.bug_status === 'Blue Bugs') {
      sh.getRange(r, bugCol).setBackground(C.BLUE_BUGS);
    }

    // RA cell
    const raCol = DB_COLUMNS.indexOf('RA') + 1;
    if (row.restricted_area) {
      sh.getRange(r, raCol).setBackground(C.RA_BG).setFontWeight('bold');
    }
  }

  finalizeTab_(sh);

  // Protect DB columns (warning only — doesn't lock)
  const prot = sh.getRange(DATA_START, 1, grid.length, DB_COLUMNS.length).protect();
  prot.setDescription('Auto-filled from database — edit the blue columns on the right');
  prot.setWarningOnly(true);
}


// ════════════════════════════════════════════════════════════════════════════
// SUMMARY TAB
// ════════════════════════════════════════════════════════════════════════════

function writeSummary_(ss, byFos, thresholdDays) {
  let sh = ss.getSheetByName('Summary');
  if (!sh) sh = ss.insertSheet('Summary', 0);
  sh.clear();

  const sumHeaders = ['FOS', 'Total', 'Inspected', 'Remaining', '% Done', 'Red Bugs', 'Claimed'];

  // Aggregate totals
  let totalAll = 0, doneAll = 0, redAll = 0, claimedAll = 0;
  const fosStats = [];
  for (const fos of Object.keys(byFos).sort()) {
    const sites = byFos[fos];
    const t = sites.length;
    const d = sites.filter(r => r.was_inspected).length;
    const rem = t - d;
    const pct = t > 0 ? Math.round(100 * d / t) : 0;
    const rb  = sites.filter(r => r.bug_status === 'Red Bugs').length;

    // Count claims from existing sheet
    const tab = ss.getSheetByName(safeName_(fos));
    let claimed = 0;
    if (tab && tab.getLastRow() >= DATA_START) {
      const claimCol = DB_COLUMNS.length + 1;
      const vals = tab.getRange(DATA_START, claimCol,
                                tab.getLastRow() - DATA_START + 1, 1).getValues();
      claimed = vals.filter(v => String(v[0]).trim()).length;
    }

    fosStats.push([fos, t, d, rem, pct + '%', rb, claimed]);
    totalAll += t; doneAll += d; redAll += rb; claimedAll += claimed;
  }

  const remAll = totalAll - doneAll;
  const pctAll = totalAll > 0 ? Math.round(100 * doneAll / totalAll) : 0;

  // Title
  sh.getRange(1, 1, 1, sumHeaders.length).merge()
    .setValue('Sr Facility Inspection Summary')
    .setFontSize(16).setFontWeight('bold').setBackground('#f5f5f5');

  // Global stats
  const globalText = remAll === 0
    ? `All ${totalAll} sites inspected | Threshold: ${thresholdDays}`
    : `Remaining: ${remAll} | ${doneAll}/${totalAll} (${pctAll}%) | Red Bugs: ${redAll} | Claimed: ${claimedAll} | Threshold: ${thresholdDays} | ${new Date().toLocaleString()}`;

  sh.getRange(2, 1, 1, sumHeaders.length).merge()
    .setValue(globalText)
    .setFontSize(12).setFontWeight('bold')
    .setFontColor(remAll === 0 ? C.REMAIN_GREEN : C.REMAIN_RED)
    .setBackground(C.STATS_BG);

  // Spacer
  sh.getRange(3, 1).setValue('');

  // Table header
  sh.getRange(4, 1, 1, sumHeaders.length).setValues([sumHeaders])
    .setBackground(C.HEADER_BG).setFontColor(C.HEADER_FG)
    .setFontWeight('bold').setHorizontalAlignment('center');

  // FOS rows
  for (let i = 0; i < fosStats.length; i++) {
    const r = 5 + i;
    sh.getRange(r, 1, 1, sumHeaders.length).setValues([fosStats[i]]);

    const pctNum = parseInt(fosStats[i][4]);
    const pctBg = pctNum >= 90 ? C.PCT_GREEN : pctNum >= 60 ? C.PCT_YELLOW : C.PCT_RED;
    sh.getRange(r, 5).setBackground(pctBg).setHorizontalAlignment('center');

    const rem = fosStats[i][3];
    sh.getRange(r, 4).setFontColor(rem > 0 ? C.REMAIN_RED : C.REMAIN_GREEN)
                      .setFontWeight('bold');

    if (fosStats[i][5] > 0) {
      sh.getRange(r, 6).setBackground(C.RED_BUGS).setFontWeight('bold');
    }
  }

  // Totals row
  const totRow = 5 + fosStats.length;
  sh.getRange(totRow, 1, 1, sumHeaders.length)
    .setValues([['TOTAL', totalAll, doneAll, remAll, pctAll + '%', redAll, claimedAll]])
    .setFontWeight('bold')
    .setBackground('#e0e0e0');
  const tPctBg = pctAll >= 90 ? C.PCT_GREEN : pctAll >= 60 ? C.PCT_YELLOW : C.PCT_RED;
  sh.getRange(totRow, 5).setBackground(tPctBg);

  sh.setFrozenRows(4);
  for (let c = 1; c <= sumHeaders.length; c++) sh.autoResizeColumn(c);
}


// ════════════════════════════════════════════════════════════════════════════
// HELPERS
// ════════════════════════════════════════════════════════════════════════════

function finalizeTab_(sh) {
  sh.setFrozenRows(HEADER_ROW);
  // Row 1 and Row 2 are merged across many columns. Freezing a single column
  // across a merged range causes a Google Sheets exception.
  sh.setFrozenColumns(0);
  const widthMap = {
    'Township': 190,
    'Section': 120,
    'Sitecode': 115,
    'Priority': 85,
    'Acres': 70,
    'AirMap': 70,
    'Status': 55,
    'Inspector': 130,
    '#/Dip': 60,
    '% Wet': 65,
    'Sample #': 100,
    'Bug Status': 110,
    'RA': 45,
    'Remarks': 320,
    'Treatment': 120,
    'Claim Emp ID': 110,
    'New Notes': 260,
    'Drone': 80,
    'Needs Sample': 110,
  };
  for (let c = 1; c <= ALL_COLUMNS.length; c++) {
    const colName = ALL_COLUMNS[c - 1];
    const width = widthMap[colName] || 100;
    sh.setColumnWidth(c, width);
  }
}

function safeName_(name) {
  if (!name) return 'Unknown';
  return String(name).replace(/[\/\\?*\[\]:]/g, '-').substring(0, 100);
}
