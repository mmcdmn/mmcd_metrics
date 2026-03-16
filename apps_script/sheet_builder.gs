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
  FACILITY:       'SR',             // Facility code: SR, MO, E, Wp, Wm, N, Sj
  ZONE:           '1,2',            // Zones: '1', '2', or '1,2'
  PRIORITIES:     'YELLOW,RED',     // Priority filter: RED, YELLOW, BLUE, GREEN, PURPLE (comma-sep, '' for all)
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
  const thresholdDays = getThresholdLookback_();
  const rows = fetchChecklistData_(thresholdDays);
  if (!rows || rows.length === 0) {
    SpreadsheetApp.getUi().alert('No air sites returned for ' + CONFIG.PRIORITIES + ' priorities.');
    return;
  }

  const manualSnap = snapshotManualData_(ss);

  const byFos = {};
  for (const r of rows) {
    const fos = r.fos_name || 'Unknown FOS';
    (byFos[fos] = byFos[fos] || []).push(r);
  }

  for (const fos of Object.keys(byFos).sort()) {
    writeFosTab_(ss, safeName_(fos), byFos[fos], manualSnap, thresholdDays);
  }

  writeSummary_(ss, byFos, thresholdDays);
  SpreadsheetApp.flush();
}

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

  let url = base + '/private/air-checklist'
    + '?facility=' + CONFIG.FACILITY
    + '&lookback_days=' + lookback
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
      const hasAny = manualVals.some(v => v !== '' && v != null);
      if (hasAny) snap[name + '::' + sc] = manualVals;
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
    .setValue(`${tabName} | ${CONFIG.FACILITY} Facility | Claim sites by typing your Emp ID in "Claim Emp ID" | Last Refresh: ${new Date().toLocaleString()}`)
    .setFontSize(10).setFontColor('#666666')
    .setBackground(C.STATS_BG);

  // ── Row 3: Column headers ──────────────────────────────────────────────
  const hdr = sh.getRange(HEADER_ROW, 1, 1, ALL_COLUMNS.length);
  hdr.setValues([ALL_COLUMNS])
     .setBackground(C.HEADER_BG).setFontColor(C.HEADER_FG)
     .setFontWeight('bold').setHorizontalAlignment('center');
  for (const colName of MANUAL_COL_NAMES) {
    sh.getRange(HEADER_ROW, ALL_COLUMNS.indexOf(colName) + 1)
      .setBackground(C.MANUAL_HEADER_BG);
  }

  // ── Row 4+: Data rows (no separator rows — borders between sections) ──
  if (rows.length === 0) { finalizeTab_(sh); return; }

  const grid = [];
  for (const row of rows) {
    const done = row.was_inspected;
    const vals = [
      row.sitecode   || '',
      '',                                                       // Claim Emp ID (from snapshot)
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
      '',                                                       // New Notes
      '',                                                       // Drone
      '',                                                       // Needs Sample
    ];
    // Overlay manual snapshot
    const key = tabName + '::' + (row.sitecode || '');
    if (manualSnap[key]) {
      for (let j = 0; j < MANUAL_COL_NAMES.length; j++) {
        vals[MANUAL_INDICES[j]] = manualSnap[key][j] || '';
      }
    }
    grid.push(vals);
  }

  sh.getRange(DATA_START, 1, grid.length, ALL_COLUMNS.length).setValues(grid);

  // ── Styling: row colors, priority, bug status, RA ──────────────────────
  const statusCol = ALL_COLUMNS.indexOf('Status') + 1;
  const priCol    = ALL_COLUMNS.indexOf('Priority') + 1;
  const bugCol    = ALL_COLUMNS.indexOf('Bug Status') + 1;
  const raCol     = ALL_COLUMNS.indexOf('RA') + 1;
  const claimCol  = ALL_COLUMNS.indexOf('Claim Emp ID') + 1;

  for (let i = 0; i < rows.length; i++) {
    const r   = DATA_START + i;
    const row = rows[i];
    const rng = sh.getRange(r, 1, 1, ALL_COLUMNS.length);
    const claimedBy = String(grid[i][claimCol - 1] || '').trim();

    if (row.was_inspected) {
      rng.setBackground(C.DONE_BG);
      sh.getRange(r, statusCol)
        .setFontColor(C.REMAIN_GREEN).setFontWeight('bold')
        .setHorizontalAlignment('center');
    } else if (claimedBy) {
      rng.setBackground(C.CLAIMED_BG);
    }

    // Priority cell
    const pri = String(row.priority || '').toUpperCase();
    const priColors = { RED: C.PRIORITY_RED, YELLOW: C.PRIORITY_YELLOW, BLUE: C.PRIORITY_BLUE, GREEN: C.PRIORITY_GREEN, PURPLE: C.PRIORITY_PURPLE };
    if (priColors[pri]) sh.getRange(r, priCol).setBackground(priColors[pri]).setFontWeight('bold');

    // Bug Status cell
    if (row.bug_status === 'Red Bugs')    sh.getRange(r, bugCol).setBackground(C.RED_BUGS).setFontWeight('bold');
    else if (row.bug_status === 'Pending Lab') sh.getRange(r, bugCol).setBackground(C.PENDING_LAB);
    else if (row.bug_status === 'Blue Bugs')   sh.getRange(r, bugCol).setBackground(C.BLUE_BUGS);

    // RA cell
    if (row.restricted_area) sh.getRange(r, raCol).setBackground(C.RA_BG).setFontWeight('bold');
  }

  // ── Section borders: thick black bottom border at section/township breaks ─
  for (let i = 0; i < rows.length - 1; i++) {
    const curSect = rows[i].sectcode || '';
    const nxtSect = rows[i + 1].sectcode || '';
    if (curSect !== nxtSect) {
      sh.getRange(DATA_START + i, 1, 1, ALL_COLUMNS.length)
        .setBorder(null, null, true, null, null, null, '#000000', SpreadsheetApp.BorderStyle.SOLID_THICK);
    }
  }

  finalizeTab_(sh);
}


// ════════════════════════════════════════════════════════════════════════════
// SUMMARY TAB
// ════════════════════════════════════════════════════════════════════════════

function writeSummary_(ss, byFos, thresholdDays) {
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
  for (const t of byTownOrder) {
    sh.getRange(r, 1, 1, 4).setValues([[t.town, t.code, t.fos, t.done ? 'Y' : 'FALSE']]);
    r++;
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
  for (const t of byFosOrder) {
    sh.getRange(r, 1, 1, 4).setValues([[t.town, t.code, t.fos, t.done ? 'Y' : 'FALSE']]);
    r++;
  }

  sh.setFrozenRows(1);
  for (let c = 1; c <= Math.max(numCols, 4); c++) sh.autoResizeColumn(c);
}


// ════════════════════════════════════════════════════════════════════════════
// HELPERS
// ════════════════════════════════════════════════════════════════════════════

function getTownshipRows_(rows) {
  const groups = {};
  for (const r of rows) {
    const town = r.township_name || 'Unknown';
    const fos  = r.fos_name || 'Unknown';
    const key  = town + '|||' + fos;
    if (!groups[key]) groups[key] = { town, fos, sects: new Set(), allDone: true };
    if (r.sectcode) groups[key].sects.add(r.sectcode);
    if (!r.was_inspected) groups[key].allDone = false;
  }

  const result = [];
  for (const g of Object.values(groups)) {
    const sects = [...g.sects].sort();
    if (sects.length === 0) {
      result.push({ town: g.town, code: '', fos: g.fos, done: g.allDone });
      continue;
    }
    const town4 = sects[0].substring(0, 4);
    const secNums = [...new Set(sects.map(s => parseInt(s.substring(4, 6), 10)))].sort((a, b) => a - b);
    if (secNums.length === 0) {
      result.push({ town: g.town, code: town4 + '-', fos: g.fos, done: g.allDone });
      continue;
    }
    // Find contiguous ranges
    const ranges = [];
    let start = secNums[0], end = secNums[0];
    for (let i = 1; i < secNums.length; i++) {
      if (secNums[i] <= end + 1) { end = secNums[i]; }
      else { ranges.push([start, end]); start = secNums[i]; end = secNums[i]; }
    }
    ranges.push([start, end]);

    for (const [s, e] of ranges) {
      const code = s === e
        ? town4 + String(s).padStart(2, '0') + '-'
        : town4 + String(s).padStart(2, '0') + '-' + String(e).padStart(2, '0');
      result.push({ town: g.town, code, fos: g.fos, done: g.allDone });
    }
  }
  return result;
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
