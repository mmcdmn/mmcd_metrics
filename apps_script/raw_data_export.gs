// ============================================================================
// RAW AIR INSPECTION DATA EXPORT
// ============================================================================
// Pulls raw air inspection data from the MMCD Metrics API into a Google Sheet.
// Change the FILTERS below, then run exportRawData().
//
// SETUP:
//   1. Extensions → Apps Script → paste this script
//   2. Project Settings → Script Properties:
//       API_BASE:  https://metrics.mmcd.org/v1
//       API_KEY:   <your key>
//   3. Run exportRawData()
//   4. (Optional) Run setupAutoRefresh() once to auto-refresh
// ============================================================================

// ── FILTERS — change these to control what data is pulled ────────────────────
const FILTERS = {
  FACILITY:        'SR',           // Facility: SR, MO, E, W, N, Sj  ('' = all)
  ZONE:            '1,2',          // Zone: '1', '2', '1,2'
  PRIORITY:        'YELLOW,RED',   // Priority: RED, YELLOW, BLUE, GREEN, PURPLE  ('' = all)
  LOOKBACK_DAYS:   2,              // Days back for inspections: 1–14
  REFRESH_MINUTES: 1,              // Auto-refresh interval in minutes (1, 5, 10, 15, 30)
  // FOREMAN:      '',             // FOS shortname filter (optional)
};

function getProp_(key) {
  const val = PropertiesService.getScriptProperties().getProperty(key);
  if (!val) throw new Error('Missing Script Property: ' + key);
  return val;
}

function exportRawData() {
  const base = getProp_('API_BASE');
  const key  = getProp_('API_KEY');

  let url = base + '/private/air-checklist?lookback_days=' + FILTERS.LOOKBACK_DAYS;
  if (FILTERS.FACILITY) url += '&facility=' + encodeURIComponent(FILTERS.FACILITY);
  if (FILTERS.ZONE)     url += '&zone='     + encodeURIComponent(FILTERS.ZONE);
  if (FILTERS.PRIORITY) url += '&priority='  + encodeURIComponent(FILTERS.PRIORITY);
  if (FILTERS.FOREMAN)  url += '&foreman='   + encodeURIComponent(FILTERS.FOREMAN);

  const resp = UrlFetchApp.fetch(url, {
    headers: { 'Authorization': 'Bearer ' + key },
    muteHttpExceptions: true,
  });

  if (resp.getResponseCode() !== 200) {
    Logger.log('API error ' + resp.getResponseCode() + ': ' + resp.getContentText());
    return;
  }

  const payload = JSON.parse(resp.getContentText());
  const rows = Array.isArray(payload) ? payload : payload.data || [];
  if (!rows.length) {
    Logger.log('No data returned for current filters.');
    return;
  }

  const ss = SpreadsheetApp.getActiveSpreadsheet();
  let sh = ss.getSheetByName('Raw Data');
  if (!sh) sh = ss.insertSheet('Raw Data');
  sh.clear();

  const headers = Object.keys(rows[0]);
  sh.getRange(1, 1, 1, headers.length).setValues([headers])
    .setFontWeight('bold').setBackground('#1a237e').setFontColor('#ffffff');

  const data = rows.map(r => headers.map(h => r[h] != null ? r[h] : ''));
  sh.getRange(2, 1, data.length, headers.length).setValues(data);

  sh.setFrozenRows(1);
  headers.forEach((_, i) => sh.autoResizeColumn(i + 1));

  Logger.log(data.length + ' rows exported to Raw Data tab.');
}

/** Run ONCE to set up auto-refresh. Uses FILTERS.REFRESH_MINUTES. */
function setupAutoRefresh() {
  ScriptApp.getProjectTriggers().forEach(t => {
    if (t.getHandlerFunction() === 'exportRawData') ScriptApp.deleteTrigger(t);
  });
  ScriptApp.newTrigger('exportRawData')
    .timeBased()
    .everyMinutes(FILTERS.REFRESH_MINUTES)
    .create();
  Logger.log('Auto-refresh set: every ' + FILTERS.REFRESH_MINUTES + ' minute(s).');
}

/** Remove the auto-refresh trigger */
function removeAutoRefresh() {
  ScriptApp.getProjectTriggers().forEach(t => {
    if (t.getHandlerFunction() === 'exportRawData') ScriptApp.deleteTrigger(t);
  });
  Logger.log('Auto-refresh removed.');
}
