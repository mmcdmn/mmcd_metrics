// Overview Dashboard JavaScript - Externalized for caching & reduced payload

// --- Performance timing ---
window._mmcdPerf = { pageStart: performance.now() };
// Unified info button handler — works for BOTH stat boxes and chart titles
$(document).on('click', '.stat-box-info-btn, .chart-info-btn', function(e) {
  e.stopPropagation();
  
  // Remove any existing popover
  $('.info-popover').remove();
  
  var btn = $(this);
  var description = btn.data('description') || '';
  var wikiLink = btn.data('wiki-link') || '';
  
  if (!description && !wikiLink) return;
  
  // Build popover content
  var html = '<div class="info-popover">';
  html += '<button class="info-close">&times;</button>';
  if (description) {
    html += '<div class="info-description">' + description + '</div>';
  }
  if (wikiLink) {
    html += '<a class="info-wiki-link" href="' + wikiLink + '" target="_blank">';
    html += '<i class="fa fa-external-link-alt"></i> View full documentation</a>';
  }
  html += '</div>';
  
  // Append to body with fixed positioning
  var $popover = $(html).appendTo('body');
  
  // Position relative to button using viewport coordinates
  var rect = btn[0].getBoundingClientRect();
  var popW = $popover.outerWidth();
  var winW = $(window).width();
  
  // Vertical: below the button
  var topPos = rect.bottom + 6;
  
  // Horizontal: try right-aligned with button, flip if would overflow
  var rightAligned = rect.right - popW;
  var leftAligned = rect.left;
  var arrowRight;
  
  if (rightAligned >= 8) {
    // Right-align: popover right edge near button right edge
    $popover.css('left', rightAligned + 'px');
    arrowRight = (popW - (rect.width / 2) - 4);
    $popover.find('.info-popover').addBack().each(function() {
      // no-op; arrow positioned via pseudo-element
    });
    $popover.attr('data-arrow-pos', 'right');
    $popover.css('--arrow-left', 'auto');
    $popover.css('--arrow-right', Math.max(8, (rect.width / 2)) + 'px');
  } else {
    // Left-align: popover left edge near button left edge
    $popover.css('left', Math.max(8, leftAligned) + 'px');
    $popover.attr('data-arrow-pos', 'left');
    $popover.css('--arrow-left', Math.max(8, (rect.width / 2)) + 'px');
    $popover.css('--arrow-right', 'auto');
  }
  
  $popover.css('top', topPos + 'px');
  
  // Set arrow position via inline style on ::before (use a style element)
  var arrowDir = $popover.attr('data-arrow-pos');
  var arrowStyle;
  if (arrowDir === 'right') {
    arrowStyle = 'right: ' + Math.max(8, Math.round(rect.width / 2)) + 'px; left: auto;';
  } else {
    arrowStyle = 'left: ' + Math.max(8, Math.round(rect.width / 2)) + 'px; right: auto;';
  }
  // Apply arrow positioning via a unique style tag
  $('#info-popover-arrow-style').remove();
  $('head').append('<style id="info-popover-arrow-style">.info-popover::before { ' + arrowStyle + ' }</style>');
});

// Close popover
$(document).on('click', '.info-popover .info-close', function(e) {
  e.stopPropagation();
  $(this).closest('.info-popover').remove();
  $('#info-popover-arrow-style').remove();
});
$(document).on('click', function(e) {
  if (!$(e.target).closest('.info-popover, .stat-box-info-btn, .chart-info-btn').length) {
    $('.info-popover').remove();
    $('#info-popover-arrow-style').remove();
  }
});
Shiny.addCustomMessageHandler('navigate', function(url) {
  // Before navigating away, cache the current rendered content
  // so the back button can restore it INSTANTLY without re-fetching data
  var statsWrapper = document.getElementById('summary_stats_wrapper');
  if (statsWrapper && statsWrapper.style.display !== 'none') {
    var cacheKey = 'mmcd_page_cache_' + window.location.href;
    try {
      sessionStorage.setItem(cacheKey, statsWrapper.innerHTML);
    } catch(e) { /* storage full - ignore */ }
  }
  // Save the analysis date so back-nav restores it (not today's date)
  // NOTE: #custom_today is a div wrapper; the actual value is in the inner <input>
  var dateVal = $('#custom_today input').val();
  if (dateVal) {
    sessionStorage.setItem('mmcd_date_cache_' + window.location.href, dateVal);
  }
  // Save zone filter too
  var zoneVal = $('#zone_filter').val();
  if (zoneVal) {
    sessionStorage.setItem('mmcd_zone_cache_' + window.location.href, zoneVal);
  }
  // Mark that we're navigating forward (drill-down) so target page auto-loads
  sessionStorage.setItem('mmcd_auto_refresh', 'true');
  window.location.href = url;
});

// On page load: restore cached content for back-nav, or auto-refresh for drill-down
$(document).on('shiny:connected', function() {
  // Check if this is a back/forward navigation
  var navEntries = performance.getEntriesByType('navigation');
  var isBackForward = navEntries.length > 0 && navEntries[0].type === 'back_forward';
  
  if (isBackForward) {
    // BACK/FORWARD: restore cached HTML instantly (no database hit!)
    var cacheKey = 'mmcd_page_cache_' + window.location.href;
    var cachedHtml = sessionStorage.getItem(cacheKey);
    if (cachedHtml) {
      var statsWrapper = document.getElementById('summary_stats_wrapper');
      var initialPrompt = document.getElementById('initial_prompt_static');
      var loadingSkeleton = document.getElementById('loading_skeleton_static');
      if (statsWrapper) {
        statsWrapper.innerHTML = cachedHtml;
        statsWrapper.style.display = '';
      }
      if (initialPrompt) initialPrompt.style.display = 'none';
      if (loadingSkeleton) loadingSkeleton.style.display = 'none';
      
      // Restore analysis date so re-drill uses the same date, not today
      var savedDate = sessionStorage.getItem('mmcd_date_cache_' + window.location.href);
      if (savedDate && savedDate !== 'undefined') {
        // Use Shiny's own input binding to set the date properly
        // This is exactly what updateDateInput() does server-side
        var dateEl = document.getElementById('custom_today');
        if (dateEl) {
          var binding = $(dateEl).data('shiny-input-binding');
          if (binding) {
            binding.receiveMessage(dateEl, {value: savedDate});
          } else {
            // Fallback: target the inner <input> with datepicker API
            var $inp = $(dateEl).find('input');
            if ($inp.length) {
              if (typeof $inp.bsDatepicker === 'function') {
                $inp.bsDatepicker('update', savedDate);
              } else {
                $inp.datepicker('update', savedDate);
              }
              $inp.trigger('change');
            }
          }
        }
      }
      // Restore zone filter
      var savedZone = sessionStorage.getItem('mmcd_zone_cache_' + window.location.href);
      if (savedZone && savedZone !== 'undefined') {
        $('#zone_filter').val(savedZone).trigger('change');
      }
      // Done — user sees previous content instantly, no refresh needed
      return;
    }
    // No cache found — fall through to show normal prompt
  }
  
  // Check if this is a forward drill-down navigation
  if (sessionStorage.getItem('mmcd_auto_refresh') === 'true') {
    sessionStorage.removeItem('mmcd_auto_refresh');
    setTimeout(function() { $('#refresh').click(); }, 300);
  }
});

// Also cache content when user clicks the Back button (before navigation)
window.addEventListener('pagehide', function() {
  var statsWrapper = document.getElementById('summary_stats_wrapper');
  if (statsWrapper && statsWrapper.style.display !== 'none') {
    var cacheKey = 'mmcd_page_cache_' + window.location.href;
    try {
      sessionStorage.setItem(cacheKey, statsWrapper.innerHTML);
    } catch(e) { /* storage full - ignore */ }
    // Also save the date and zone for this page (inner <input> has the actual value)
    var dateVal = $('#custom_today input').val();
    if (dateVal) {
      try { sessionStorage.setItem('mmcd_date_cache_' + window.location.href, dateVal); } catch(e) {}
    }
    var zoneVal = $('#zone_filter').val();
    if (zoneVal) {
      try { sessionStorage.setItem('mmcd_zone_cache_' + window.location.href, zoneVal); } catch(e) {}
    }
  }
});

// Clear banners and reset active states when refresh is clicked
$(document).on('click', '#refresh', function() {
  window._mmcdPerf.refreshStart = performance.now();
  // Remove all comparison banners (they will be stale after refresh)
  $('.comparison-banner').remove();
  // Close all open charts and reset active states
  $('.chart-panel-wrapper.visible').removeClass('visible');
  $('.stat-box-clickable.active').removeClass('active');
  // Hide initial prompt (first load) and stats, show skeleton
  $('#initial_prompt_static').hide();
  $('#summary_stats_wrapper').hide();
  $('#loading_skeleton_static').fadeIn(150);
});

// Handler to swap skeleton for real content when data is ready
Shiny.addCustomMessageHandler('hideLoadingSkeleton', function(msg) {
  var renderMs = Math.round(performance.now() - (window._mmcdPerf.refreshStart || window._mmcdPerf.pageStart));
  console.log('[PERF] Server render → content ready: ' + renderMs + 'ms');
  $('#initial_prompt_static').hide();
  $('#loading_skeleton_static').fadeOut(200, function() {
    $('#summary_stats_wrapper').fadeIn(300);
    
    // Force all pre-rendered Plotly charts to resize after layout is applied
    setTimeout(function() {
      $('.category-chart .plotly:visible').each(function() {
        if (window.Plotly && this.layout) {
          window.Plotly.Plots.resize(this);
          window.Plotly.redraw(this);
        }
      });
    }, 250);
  });
});

// Value box click to toggle chart visibility (for district view - metric boxes)
$(document).on('click', '.stat-box-clickable[data-metric-id]', function() {
  var metricId = $(this).data('metric-id');
  
  // Check if this metric has a redirect URL (e.g., vector_index -> trap surveillance map)
  var redirectUrl = $(this).data('redirect');
  if (redirectUrl && redirectUrl !== '') {
    window.location.href = redirectUrl;
    return;
  }
  
  var chartWrapper = $('#chart_wrapper_' + metricId);
  var statBox = $(this);
  
  // Get comparison data from data attributes
  var currentWeek = statBox.data('current-week');
  var historicalAvg = statBox.data('historical-avg');
  var pctDiff = statBox.data('pct-diff');
  var weekNum = statBox.data('week-num');
  
  // Toggle visibility
  chartWrapper.toggleClass('visible');
  statBox.toggleClass('active');
  
  // Update comparison banner in chart wrapper
  var comparisonBanner = chartWrapper.find('.comparison-banner');
  if (chartWrapper.hasClass('visible') && historicalAvg && currentWeek !== undefined && currentWeek !== '') {
    var diffClass = pctDiff >= 0 ? 'positive' : 'negative';
    var diffSign = pctDiff >= 0 ? '+' : '';
    var bannerHtml = '<div class="comparison-banner ' + diffClass + '">' +
      '<span class="current">Current: ' + Math.round(currentWeek).toLocaleString() + '</span>' +
      '<span class="separator">|</span>' +
      '<span class="historical">10yr Week Avg: ' + Math.round(historicalAvg).toLocaleString() + '</span>' +
      '<span class="separator">|</span>' +
      '<span class="diff ' + diffClass + '">' + diffSign + pctDiff + '%</span>' +
      '</div>';
    if (comparisonBanner.length) {
      comparisonBanner.replaceWith(bannerHtml);
    } else {
      chartWrapper.prepend(bannerHtml);
    }
  } else {
    comparisonBanner.remove();
  }
  
  // Scroll to chart if now visible and resize Plotly
  if (chartWrapper.hasClass('visible')) {
    setTimeout(function() {
      // Force the chart to fully show first
      chartWrapper.show();
      
      // Find and resize plotly charts in this specific wrapper
      chartWrapper.find('.plotly').each(function() {
        var plotElement = this;
        
        if (window.Plotly) {
          // Force plotly to detect the new dimensions
          if (plotElement.layout) {
            window.Plotly.Plots.resize(plotElement);
          }
          
          // If chart still not visible, try to redraw
          setTimeout(function() {
            if (window.Plotly && plotElement.layout) {
              window.Plotly.redraw(plotElement);
            }
          }, 200);
        }
      });
      
      // Chart revealed in-place, no auto-scroll
    }, 100);
  } else {
    // Also resize remaining charts when one is hidden
    setTimeout(function() {
      $('.chart-panel-wrapper.visible .plotly').each(function() {
        if (window.Plotly && this.layout) {
          window.Plotly.Plots.resize(this);
        }
      });
    }, 350);
  }
  
  // Send visibility state to server
  Shiny.setInputValue(metricId + '_chart_visible', chartWrapper.hasClass('visible'), {priority: 'event'});
});

// Facility box click to show detail boxes (for facilities view)
$(document).on('click', '.stat-box-clickable[data-facility]', function() {
  var facility = $(this).data('facility');
  var statBox = $(this);
  var detailContainer = $('#facility_detail_container');
  
  // Toggle active state on facility boxes
  $('.stat-box-clickable[data-facility]').removeClass('active');
  
  // Remove any existing comparison banner in detail container
  detailContainer.find('.comparison-banner').remove();
  
  // Check if this facility is already selected
  var currentFacility = detailContainer.data('current-facility');
  if (currentFacility === facility) {
    // Clicking same facility - toggle off
    detailContainer.data('current-facility', null);
    detailContainer.removeClass('visible');
    Shiny.setInputValue('selected_facility', null, {priority: 'event'});
  } else {
    // Clicking new facility - show details / drill down
    statBox.addClass('active');
    detailContainer.data('current-facility', facility);
    detailContainer.addClass('visible');
    Shiny.setInputValue('selected_facility', facility, {priority: 'event'});
    
    // Add comparison banner from stat box data attributes
    var currentWeek = statBox.data('current-week');
    var historicalAvg = statBox.data('historical-avg');
    var pctDiff = statBox.data('pct-diff');
    var weekNum = statBox.data('week-num');
    
    if (historicalAvg && currentWeek !== undefined && currentWeek !== '') {
      var diffClass = pctDiff >= 0 ? 'positive' : 'negative';
      var diffSign = pctDiff >= 0 ? '+' : '';
      var bannerHtml = '<div class="comparison-banner ' + diffClass + '">' +
        '<span class="current">Current: ' + Math.round(currentWeek).toLocaleString() + '</span>' +
        '<span class="separator">|</span>' +
        '<span class="historical">10yr Week ' + weekNum + ' Avg: ' + Math.round(historicalAvg).toLocaleString() + '</span>' +
        '<span class="separator">|</span>' +
        '<span class="diff ' + diffClass + '">' + diffSign + pctDiff + '%</span>' +
        '</div>';
      // Prepend banner into detail container (appears above detail boxes)
      detailContainer.prepend(bannerHtml);
    }
    
    // Scroll to detail container
    setTimeout(function() {
      if (detailContainer.length && detailContainer.hasClass('visible')) {
        $('html, body').animate({
          scrollTop: detailContainer.offset().top - 100
        }, 300);
      }
    }, 100);
  }
});

// FOS box click handler (for fos view) - toggle detail boxes
$(document).on('click', '.stat-box-clickable[data-fos]', function() {
  var fos = $(this).data('fos');
  var statBox = $(this);
  var detailContainer = $('#facility_detail_container');
  
  // Toggle active state on FOS boxes
  $('.stat-box-clickable[data-fos]').removeClass('active');
  
  // Check if this FOS is already selected
  var currentFos = detailContainer.data('current-fos');
  if (currentFos === fos) {
    // Clicking same FOS - toggle off
    detailContainer.data('current-fos', null);
    detailContainer.removeClass('visible');
    Shiny.setInputValue('selected_fos', '', {priority: 'event'});
  } else {
    // Clicking new FOS - show details
    statBox.addClass('active');
    detailContainer.data('current-fos', fos);
    detailContainer.addClass('visible');
    Shiny.setInputValue('selected_fos', fos, {priority: 'event'});
    
    // Scroll to detail container
    setTimeout(function() {
      if (detailContainer.length && detailContainer.hasClass('visible')) {
        $('html, body').animate({
          scrollTop: detailContainer.offset().top - 100
        }, 300);
      }
    }, 100);
  }
});

// Drill-down button click (appears when chart is revealed via value box click)
$(document).on('click', '.drill-down-btn', function(e) {
  e.stopPropagation();
  var metricId = $(this).data('metric-id');
  var facility = $(this).data('facility');
  if (facility) {
    // Facility drill-down to FOS view
    Shiny.setInputValue('drill_down_facility_btn', facility, {priority: 'event'});
  } else if (metricId) {
    // District drill-down to facilities view
    Shiny.setInputValue('drill_down_btn', metricId, {priority: 'event'});
  }
});

// Legacy: Chart type toggle functionality (kept for backwards compatibility)
$(document).on('click', '.chart-toggle-btn', function() {
  var btn = $(this);
  var metric = btn.attr('id').replace('_toggle_bar', '').replace('_toggle_pie', '');
  var chartType = btn.attr('id').includes('_toggle_pie') ? 'pie' : 'bar';
  
  // Update button states
  $('#' + metric + '_toggle_bar, #' + metric + '_toggle_pie').removeClass('active');
  btn.addClass('active');
  
  // Send chart type change to server
  Shiny.setInputValue(metric + '_chart_type', chartType, {priority: 'event'});
});
