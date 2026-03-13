// Air Inspection Checklist - JavaScript
// Externalized for caching & reduced payload
// Claims are stored in Redis (server-side) and synced via Shiny custom messages.

// =========================================================================
// AirMap collapsible toggle
// =========================================================================
$(document).on('click', '.airmap-toggle', function() {
  var target = $(this).data('target');
  var panel = $('#' + target);
  $(this).toggleClass('open');
  panel.slideToggle(200);
});

// =========================================================================
// Claim feature — Redis-backed via Shiny messages
// =========================================================================
var currentEmployee = null; // {emp_num, emp_name}
var currentClaims = {};     // sitecode -> {emp_num, emp_name, time, claim_date}

// Receive employee info from server
Shiny.addCustomMessageHandler('set_employee', function(data) {
  currentEmployee = data;
  renderEmployeeBar();
  // Request current claims from server
  Shiny.setInputValue('request_claims', Math.random());
});

// Receive claims state from server (Redis)
Shiny.addCustomMessageHandler('claims_update', function(claims) {
  currentClaims = claims || {};
  applyClaimsToUI();
});

function toggleClaim(sitecode) {
  if (!currentEmployee) {
    alert('No employee selected.\n\nGo back to the dashboard and select your name from the Employee dropdown before opening this app.');
    return;
  }

  var existing = currentClaims[sitecode];

  if (existing && existing.emp_num === currentEmployee.emp_num) {
    // Unclaim own claim
    Shiny.setInputValue('unclaim_site', {
      sitecode: sitecode
    }, {priority: 'event'});
    return;
  }

  if (existing && existing.emp_num !== currentEmployee.emp_num) {
    // Someone else already claimed — ask for confirmation
    var ok = confirm(
      'Site ' + sitecode + ' is already claimed by ' + existing.emp_name + '.\n\n' +
      'Do you want to replace their claim with yours?'
    );
    if (!ok) return;
  }

  // Set claim via server -> Redis
  Shiny.setInputValue('claim_site', {
    sitecode: sitecode,
    emp_num: currentEmployee.emp_num,
    emp_name: currentEmployee.emp_name
  }, {priority: 'event'});
}

function applyClaimsToUI() {
  // Clear all existing claim badges first
  $('.claim-badge, .claim-warning-detail').remove();
  $('.checklist-item').removeClass('item-claimed item-claim-warning');

  // Apply each claim
  Object.keys(currentClaims).forEach(function(sitecode) {
    applyClaimToRow(sitecode, currentClaims[sitecode]);
  });
}

function getClaimWarning(row, claimData) {
  var inspectorEmp = row.attr('data-inspector-emp') || '';
  var wasInspected = row.attr('data-inspected') === 'true';

  if (!wasInspected || !claimData) return null;

  // Case 1: Inspector is someone other than the claimer
  if (inspectorEmp && inspectorEmp !== claimData.emp_num) {
    return {
      type: 'inspector-mismatch',
      message: 'Claimed by ' + claimData.emp_name +
               ' but inspected by a different employee (Emp #' + inspectorEmp + ').'
    };
  }

  // Case 2: Site was inspected before it was claimed
  var inspDate = row.attr('data-insp-date') || '';
  if (inspDate && claimData.time) {
    var claimDate = claimData.time.slice(0, 10); // YYYY-MM-DD
    if (inspDate <= claimDate) {
      return {
        type: 'inspected-before-claim',
        message: 'This site was inspected on ' + inspDate +
                 ' before it was claimed by ' + claimData.emp_name +
                 '. The inspection is still valid.'
      };
    }
  }

  return null;
}

function applyClaimToRow(sitecode, claimData) {
  var row = $('[data-sitecode="' + sitecode + '"]');
  if (row.length === 0) return;

  // Remove existing claim badges & warning detail
  row.find('.claim-badge, .claim-warning-detail').remove();
  row.removeClass('item-claimed item-claim-warning');

  if (!claimData) return;

  row.addClass('item-claimed');

  var warning = getClaimWarning(row, claimData);
  var badge = '';

  if (warning) {
    row.addClass('item-claim-warning');
    badge = '<span class="claim-badge claim-mismatch" title="' +
      escapeHtml(warning.message) + '">' +
      '\u26a0 Claimed: ' + escapeHtml(claimData.emp_name) + '</span>' +
      '<span class="claim-warning-detail">' + escapeHtml(warning.message) + '</span>';
  } else {
    badge = '<span class="claim-badge">Claimed: ' +
      escapeHtml(claimData.emp_name) + '</span>';
  }

  row.find('.item-details').append(badge);
}

function escapeHtml(str) {
  var div = document.createElement('div');
  div.appendChild(document.createTextNode(str));
  return div.innerHTML;
}

function renderEmployeeBar() {
  // Remove old bar if present
  $('#employee-bar-container').remove();

  var tapOrClick = ('ontouchstart' in window) ? 'Double-tap' : 'Double-click';

  if (currentEmployee) {
    var html = '<div id="employee-bar-container" class="employee-bar">' +
      '<span>\ud83d\udc64 Logged in as: <span class="emp-name">' +
      escapeHtml(currentEmployee.emp_name) +
      '</span> (#' + escapeHtml(currentEmployee.emp_num) + ')</span>' +
      '<span class="emp-hint">' + tapOrClick + ' a site to claim it</span></div>';
    $('.checklist-value-boxes').before(html);
  } else {
    var html = '<div id="employee-bar-container" class="no-employee-bar">' +
      '\u26a0 No employee selected. Go back to the dashboard and pick your name to enable claiming sites.</div>';
    if ($('.checklist-value-boxes').length) {
      $('.checklist-value-boxes').before(html);
    }
  }
}

// =========================================================================
// Click / tap handlers for claiming
// =========================================================================

// Double-click (desktop)
$(document).on('dblclick', '.checklist-item[data-sitecode]', function(e) {
  e.preventDefault();
  var sitecode = $(this).attr('data-sitecode');
  toggleClaim(sitecode);
});

// Double-tap (mobile) — custom detector since dblclick is unreliable on touch
var lastTapTarget = null;
var lastTapTime = 0;
$(document).on('touchend', '.checklist-item[data-sitecode]', function(e) {
  var now = Date.now();
  var sitecode = $(this).attr('data-sitecode');
  if (lastTapTarget === sitecode && (now - lastTapTime) < 350) {
    e.preventDefault();
    toggleClaim(sitecode);
    lastTapTarget = null;
  } else {
    lastTapTarget = sitecode;
    lastTapTime = now;
  }
});

// =========================================================================
// Re-apply claims on Shiny re-render
// =========================================================================
$(document).on('shiny:value', function(event) {
  if (event.name === 'checklist_display') {
    setTimeout(function() {
      applyClaimsToUI();
      renderEmployeeBar();
    }, 100);
  }
});

// =========================================================================
// Mobile sidebar toggle
// =========================================================================
$(document).on('click', '#sidebar-toggle', function() {
  // Shiny wraps sidebarPanel in <div class="col-sm-3"><form class="well">
  // We need to toggle on the outer col-sm wrapper, not the inner form
  var sidebar = $('.well').closest('.col-sm-3, .col-sm-4');
  sidebar.toggleClass('sidebar-open');
  $('body').toggleClass('sidebar-overlay-active');
});
$(document).on('click', '.sidebar-overlay', function() {
  var sidebar = $('.well').closest('.col-sm-3, .col-sm-4');
  sidebar.removeClass('sidebar-open');
  $('body').removeClass('sidebar-overlay-active');
});
