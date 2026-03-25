/* ============================================================
   section-cards-print.js
   Handles double-sided blank-page insertion for Section Cards.

   The R code emits content pages as:
     <div class="card-page page-break" data-group="GROUPNAME"> ... </div>

   When double-sided printing is enabled the R code adds
   data-double-sided="true" on the .cards-container wrapper.

   This script:
     1. Listens for the 'section-cards-rendered' Shiny custom event
        (fired after cards are injected into the DOM).
     2. Removes any previously inserted blank separators.
     3. Walks the card-page elements, groups them by data-group,
        and inserts a blank separator page after every group that
        uses an odd number of pages — so the next group always
        starts on the FRONT of a new sheet.
   ============================================================ */

(function () {
  'use strict';

  /**
   * Remove all previously inserted blank separators.
   */
  function clearBlanks(container) {
    var blanks = container.querySelectorAll('.blank-separator');
    for (var i = 0; i < blanks.length; i++) {
      blanks[i].parentNode.removeChild(blanks[i]);
    }
  }

  /**
   * Walk the card-page elements inside `container`, count pages
   * per data-group, and insert a blank separator after any group
   * that ends on an odd page count (so the next group starts on
   * a fresh front sheet).
   */
  function insertBlankPages(container) {
    clearBlanks(container);

    var pages = container.querySelectorAll('.card-page');
    if (pages.length === 0) return;

    // Collect runs of consecutive pages with the same data-group
    var groups = [];       // [{group: "name", startIdx: N, count: N, lastEl: el}]
    var currentGroup = null;
    var currentCount = 0;

    for (var i = 0; i < pages.length; i++) {
      var g = pages[i].getAttribute('data-group') || '__none__';
      if (g !== currentGroup) {
        if (currentGroup !== null) {
          groups.push({
            group: currentGroup,
            count: currentCount,
            lastEl: pages[i - 1]
          });
        }
        currentGroup = g;
        currentCount = 1;
      } else {
        currentCount++;
      }
    }
    // Push the last group
    if (currentGroup !== null) {
      groups.push({
        group: currentGroup,
        count: currentCount,
        lastEl: pages[pages.length - 1]
      });
    }

    // For every group except the last: if it used an odd number of
    // pages, insert a blank separator page right after its last page.
    for (var j = 0; j < groups.length - 1; j++) {
      if (groups[j].count % 2 === 1) {
        var blank = document.createElement('div');
        blank.className = 'blank-separator page-break';
        blank.setAttribute('aria-hidden', 'true');
        // Screen preview label (hidden in print via .no-print)
        blank.innerHTML = '<span class="no-print">(blank back page)</span>';

        // Insert after the last page of this group
        var ref = groups[j].lastEl;
        ref.parentNode.insertBefore(blank, ref.nextSibling);
      }
    }

    // The very last element in the container should NOT have
    // page-break-after (so we don't get a trailing blank).
    var allPages = container.querySelectorAll('.card-page, .blank-separator');
    for (var k = 0; k < allPages.length; k++) {
      allPages[k].classList.add('page-break');
    }
    // Remove page-break from the very last one
    if (allPages.length > 0) {
      allPages[allPages.length - 1].classList.remove('page-break');
    }
  }

  // ---- Shiny integration ----
  // R fires: session$sendCustomMessage('section-cards-rendered', TRUE)
  // after the cards HTML is set via renderUI.

  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('section-cards-rendered', function (_msg) {
      // Short delay so Shiny's DOM update is flushed
      setTimeout(function () {
        var container = document.querySelector('.cards-container');
        if (!container) return;

        var doubleSided = container.getAttribute('data-double-sided') === 'true';
        if (doubleSided) {
          insertBlankPages(container);
        } else {
          clearBlanks(container);
          // Ensure all pages except last have page-break
          var allP = container.querySelectorAll('.card-page');
          for (var i = 0; i < allP.length; i++) {
            if (i < allP.length - 1) {
              allP[i].classList.add('page-break');
            } else {
              allP[i].classList.remove('page-break');
            }
          }
        }
      }, 100);
    });
  }
})();
