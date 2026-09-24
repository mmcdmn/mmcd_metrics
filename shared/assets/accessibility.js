/* =============================================================================
   MMCD SHARED ACCESSIBILITY BEHAVIORS (WCAG 2.1 AA)
   =============================================================================
   The single source of accessibility behavior for the whole platform.

     * Shiny apps    - inlined by shared/accessibility_helpers.R
     * Static pages  - <script src="shared/assets/accessibility.js" defer>

   Everything here is generic: it recognizes standard Shiny / Bootstrap /
   htmlwidgets markup and adds the semantics those libraries leave out, so no
   app has to repeat the work. Every enhancer is idempotent and safe to re-run.

   Enhancers
     skip link + main landmark  WCAG 2.4.1, 1.3.1
     landmark labels            1.3.1  (sidebar "Filters", dashboard menu nav)
     tabs                       4.1.2, 2.1.1  (roles, states, arrow keys)
     sliders                    4.1.2  (role/name/value on the focusable track)
     disclosures                4.1.2  ([data-a11y-disclosure] buttons)
     role="button"/"link" divs  2.1.1  (keyboard activation)
     tables                     1.3.1  (empty headers, DT column filters)
     charts and maps            1.1.1  (accessible names)
     icons                      1.1.1  (decorative icon() output hidden)
     heading levels             1.3.1  (no skipped levels in the outline)
     scrollable regions         2.1.1  (reachable with the keyboard)
     iframes                    4.1.2  (title)
     link hints                 3.2.5  (new tab, email, PDF announced)
     A11y.announce()            4.1.3  (polite status messages)
   ============================================================================= */
(function (window, document) {
  'use strict';

  if (window.A11y && window.A11y.__loaded) { return; }

  var $ = window.jQuery || null;
  var idCounter = 0;


  /* ---------------------------------------------------------------------------
     Utilities
     ------------------------------------------------------------------------ */

  function qsa(root, selector) {
    var list = [];
    if (!root) { return list; }
    if (root.nodeType === 1 && root.matches && root.matches(selector)) { list.push(root); }
    if (root.querySelectorAll) {
      var found = root.querySelectorAll(selector);
      for (var i = 0; i < found.length; i++) { list.push(found[i]); }
    }
    return list;
  }

  function closest(el, selector) {
    return el && el.closest ? el.closest(selector) : null;
  }

  function isVisible(el) {
    return !!(el && (el.offsetWidth || el.offsetHeight || el.getClientRects().length));
  }

  function textOf(el) {
    return el ? (el.textContent || '').replace(/\s+/g, ' ').trim() : '';
  }

  function ensureId(el, prefix) {
    if (!el.id) {
      idCounter += 1;
      el.id = 'a11y-' + prefix + '-' + idCounter;
    }
    return el.id;
  }

  function srOnly(str) {
    var span = document.createElement('span');
    span.className = 'sr-only';
    span.textContent = str;
    return span;
  }

  var raf = window.requestAnimationFrame
    ? window.requestAnimationFrame.bind(window)
    : function (fn) { return window.setTimeout(fn, 16); };


  /* ---------------------------------------------------------------------------
     A11y.announce() - WCAG 4.1.3 Status Messages
     One shared polite live region. Clearing and re-setting the text makes a
     repeated message ("Updated 10:40") announce again.
     ------------------------------------------------------------------------ */

  var liveRegion = null;

  function announce(message, politeness) {
    if (!document.body) { return; }
    if (!liveRegion) {
      liveRegion = document.createElement('div');
      liveRegion.id = 'a11y-live-region';
      liveRegion.className = 'sr-only';
      liveRegion.setAttribute('aria-atomic', 'true');
      document.body.appendChild(liveRegion);
    }
    liveRegion.setAttribute('aria-live', politeness === 'assertive' ? 'assertive' : 'polite');
    liveRegion.textContent = '';
    window.setTimeout(function () { liveRegion.textContent = String(message); }, 100);
  }


  /* ---------------------------------------------------------------------------
     Skip link + main landmark - WCAG 2.4.1, 1.3.1
     The page must expose exactly one main landmark. Shiny's mainPanel()
     already renders role="main"; shinydashboard and bare tabset layouts render
     none; an app with a sidebarLayout per tab renders several. Normalize all
     three cases here instead of in every app.
     ------------------------------------------------------------------------ */

  var MAIN_FALLBACKS = [
    '.content-wrapper',                               /* shinydashboard body */
    '.tab-content',                                   /* tabsetPanel body    */
    '.row > div[class*="col-sm-"]:not(:first-child)'  /* sidebarLayout output */
  ];

  function topLevelMains() {
    return qsa(document, 'main, [role="main"]').filter(function (el) {
      var p = el.parentElement;
      while (p) {
        if (p.matches('main, [role="main"]')) { return false; }
        p = p.parentElement;
      }
      return true;
    });
  }

  function commonAncestor(nodes) {
    var anc = nodes[0].parentElement;
    while (anc) {
      var holdsAll = true;
      for (var i = 1; i < nodes.length; i++) {
        if (!anc.contains(nodes[i])) { holdsAll = false; break; }
      }
      if (holdsAll) { return anc; }
      anc = anc.parentElement;
    }
    return document.body;
  }

  function markMain(el) {
    if (el.tagName !== 'MAIN') { el.setAttribute('role', 'main'); }
    if (!el.id) {
      el.id = document.getElementById('main-content') ? ensureId(el, 'main') : 'main-content';
    }
    if (!el.hasAttribute('tabindex')) { el.setAttribute('tabindex', '-1'); }
    if (!el.hasAttribute('aria-label') && !el.hasAttribute('aria-labelledby')) {
      el.setAttribute('aria-label', 'Main content');
    }
    el.classList.add('a11y-main-landmark');
    return el;
  }

  function normalizeMains() {
    var mains = topLevelMains();

    if (mains.length === 1) { return markMain(mains[0]); }

    if (mains.length > 1) {
      /* A <main> element cannot give up its role, so only role="main" divs
         (Shiny mainPanels) can be merged onto their common ancestor. */
      var hasElement = mains.some(function (m) { return m.tagName === 'MAIN'; });
      if (hasElement) { return mains[0]; }
      mains.forEach(function (m) {
        m.removeAttribute('role');
        m.classList.remove('a11y-main-landmark');
        if (m.id === 'main-content') { m.removeAttribute('id'); }
      });
      return markMain(commonAncestor(mains));
    }

    for (var i = 0; i < MAIN_FALLBACKS.length; i++) {
      var candidate = document.querySelector(MAIN_FALLBACKS[i]);
      if (candidate) { return markMain(candidate); }
    }
    return null;
  }

  function focusMain(evt) {
    var link = evt && evt.currentTarget;
    var wanted = link ? (link.getAttribute('href') || '').replace(/^#/, '') : '';
    var target = (wanted && document.getElementById(wanted)) || normalizeMains();
    if (!target) { return; }
    if (evt) { evt.preventDefault(); }
    if (!target.hasAttribute('tabindex')) { target.setAttribute('tabindex', '-1'); }
    target.focus({ preventScroll: false });
    if (target.scrollIntoView) { target.scrollIntoView({ block: 'start', behavior: 'smooth' }); }
  }

  function wireSkipLinks(root) {
    qsa(root, '.a11y-skip-link').forEach(function (link) {
      if (link.getAttribute('data-a11y-wired')) { return; }
      link.setAttribute('data-a11y-wired', '1');
      link.addEventListener('click', focusMain);
    });
  }


  /* ---------------------------------------------------------------------------
     Landmark labels - WCAG 1.3.1
     Several complementary regions are only distinguishable by name.
     ------------------------------------------------------------------------ */

  function labelLandmarks(root) {
    /* Shiny's sidebarPanel puts role="complementary" on a <form>, where ARIA in
       HTML does not allow it. Move the landmark to the wrapping column. */
    qsa(root, 'form[role="complementary"]').forEach(function (form) {
      var host = form.parentElement;
      if (!host || host.hasAttribute('role')) { return; }
      host.setAttribute('role', 'complementary');
      if (form.hasAttribute('aria-label')) {
        host.setAttribute('aria-label', form.getAttribute('aria-label'));
        form.removeAttribute('aria-label');
      }
      form.removeAttribute('role');
    });

    /* Landmarks of the same role must be told apart by name, so an app with a
       sidebar per tab cannot have two regions both called "Filters". Computed
       over the document so the names stay stable as content re-renders. */
    var sidebars = qsa(document, '[role="complementary"], aside').filter(function (el) {
      return !el.hasAttribute('aria-label') && !el.hasAttribute('aria-labelledby');
    });
    sidebars.forEach(function (el, i) {
      var base = el.classList.contains('main-sidebar') ? 'Sidebar' : 'Filters';
      if (sidebars.length > 1) {
        var nearby = precedingHeading(el);
        base = nearby ? base + ': ' + nearby : base + ' ' + (i + 1);
      }
      el.setAttribute('aria-label', base);
    });
    /* shinydashboard's menu is navigation, not just a list of links. */
    qsa(root, '.main-sidebar .sidebar').forEach(function (el) {
      if (el.hasAttribute('role') || !el.querySelector('.sidebar-menu')) { return; }
      el.setAttribute('role', 'navigation');
      el.setAttribute('aria-label', 'Dashboard menu');
    });

    /* shinydashboard puts aria-selected / aria-expanded on its menu links.
       Neither is allowed on a link, so screen readers get invalid markup and
       no usable state. Say "current page" instead, which is what these are. */
    qsa(root, '.sidebar-menu a[data-toggle="tab"]').forEach(function (link) {
      var active = link.parentElement && link.parentElement.classList.contains('active');
      link.removeAttribute('aria-selected');
      link.removeAttribute('aria-expanded');
      if (active) { link.setAttribute('aria-current', 'page'); }
      else { link.removeAttribute('aria-current'); }
    });
  }


  /* ---------------------------------------------------------------------------
     Scrollable regions - WCAG 2.1.1
     A region that scrolls but holds nothing focusable (a DT scroll body, a
     <pre> of output) cannot be reached by keyboard at all. tabindex="0" lets
     someone scroll it with the arrow keys.
     ------------------------------------------------------------------------ */

  var FOCUSABLE = 'a[href], button, input, select, textarea, [tabindex]:not([tabindex="-1"])';

  function enhanceScrollRegions(root) {
    qsa(root, '.dataTables_scrollBody, pre, .a11y-scroll, [style*="overflow"]').forEach(function (el) {
      if (el.hasAttribute('tabindex') || el.getAttribute('data-a11y-scroll')) { return; }
      var style = window.getComputedStyle(el);
      var scrolls = /(auto|scroll)/.test(style.overflowY) && el.scrollHeight > el.clientHeight + 2;
      if (!scrolls || el.querySelector(FOCUSABLE)) { return; }
      /* tabindex alone makes it reachable. Deliberately no role="region":
         several scroll areas on one page would become landmarks sharing the
         same name, which is itself a violation. */
      el.setAttribute('data-a11y-scroll', '1');
      el.setAttribute('tabindex', '0');
    });
  }


  /* ---------------------------------------------------------------------------
     Tabs - WCAG 4.1.2, 2.1.1 (WAI-ARIA tabs pattern, manual activation)
     Safe for Shiny: the tab input binding reads only the .active class and
     data-value, and listens for shown.bs.tab (shiny.js tabinput binding), so
     roles, states and tabindex can be layered on without touching input.tabs.
     Manual activation (arrows move focus, Enter/Space selects) because a
     hidden Shiny output recomputes when its tab is shown.
     ------------------------------------------------------------------------ */

  var TAB_SELECTOR = [
    'a[data-toggle="tab"]', 'a[data-bs-toggle="tab"]',
    'a[data-toggle="pill"]', 'a[data-bs-toggle="pill"]',
    'button[data-bs-toggle="tab"]', 'button[data-bs-toggle="pill"]'
  ].join(', ');

  function tabTargetId(tab) {
    var ref = tab.getAttribute('data-bs-target') || tab.getAttribute('data-target') ||
              tab.getAttribute('href') || '';
    return ref.charAt(0) === '#' ? ref.slice(1) : '';
  }

  function isTabSelected(tab) {
    if (tab.classList.contains('active')) { return true; }                /* BS5 */
    var li = tab.parentElement;
    return !!(li && li.tagName === 'LI' && li.classList.contains('active')); /* BS3 */
  }

  function tabsOf(list) {
    return qsa(list, TAB_SELECTOR).filter(function (tab) {
      return !closest(tab, '.dropdown-menu');
    });
  }

  function syncTablist(list) {
    var tabs = tabsOf(list);
    var anySelected = false;
    tabs.forEach(function (tab) {
      var selected = isTabSelected(tab);
      tab.setAttribute('aria-selected', selected ? 'true' : 'false');
      tab.setAttribute('tabindex', selected ? '0' : '-1');
      if (selected) { anySelected = true; }
    });
    if (!anySelected && tabs.length) { tabs[0].setAttribute('tabindex', '0'); }
  }

  function activateTab(tab) {
    if ($ && $.fn && $.fn.tab) { $(tab).tab('show'); } else { tab.click(); }
  }

  function onTabKeydown(e) {
    var tab = closest(e.target, '[role="tab"]');
    if (!tab) { return; }
    var list = closest(tab, '[role="tablist"]');
    var tabs = tabsOf(list).filter(isVisible);
    var i = tabs.indexOf(tab);
    var vertical = list.classList.contains('nav-stacked') ||
                   list.getAttribute('aria-orientation') === 'vertical';
    var next = null;

    switch (e.key) {
      case 'ArrowRight': if (!vertical) { next = tabs[(i + 1) % tabs.length]; } break;
      case 'ArrowLeft':  if (!vertical) { next = tabs[(i - 1 + tabs.length) % tabs.length]; } break;
      case 'ArrowDown':  if (vertical)  { next = tabs[(i + 1) % tabs.length]; } break;
      case 'ArrowUp':    if (vertical)  { next = tabs[(i - 1 + tabs.length) % tabs.length]; } break;
      case 'Home': next = tabs[0]; break;
      case 'End':  next = tabs[tabs.length - 1]; break;
      case 'Enter':
      case ' ':
      case 'Spacebar':
        e.preventDefault();
        activateTab(tab);
        return;
      default:
        return;
    }

    if (next) {
      e.preventDefault();
      tabs.forEach(function (t) { t.setAttribute('tabindex', t === next ? '0' : '-1'); });
      next.focus();
    }
  }

  function enhanceTabs(root) {
    qsa(root, 'ul.nav, div.nav, nav.nav').forEach(function (list) {
      var tabs = tabsOf(list);
      if (!tabs.length) { return; }

      list.setAttribute('role', 'tablist');
      tabs.forEach(function (tab) {
        var li = tab.parentElement;
        if (li && li.tagName === 'LI') { li.setAttribute('role', 'presentation'); }
        tab.setAttribute('role', 'tab');
        ensureId(tab, 'tab');
        var paneId = tabTargetId(tab);
        var pane = paneId ? document.getElementById(paneId) : null;
        if (pane) {
          tab.setAttribute('aria-controls', paneId);
          pane.setAttribute('role', 'tabpanel');
          pane.setAttribute('aria-labelledby', tab.id);
        }
      });
      syncTablist(list);

      if (!list.getAttribute('data-a11y-tabs')) {
        list.setAttribute('data-a11y-tabs', '1');
        list.addEventListener('keydown', onTabKeydown);
        /* Leaving the tablist hands the tab stop back to the selected tab. */
        list.addEventListener('focusout', function (e) {
          if (!list.contains(e.relatedTarget)) { syncTablist(list); }
        });
      }
    });
  }

  function onTabShown(e) {
    var tab = e.target;
    var list = closest(tab, '[role="tablist"], ul.nav');
    if (list) { syncTablist(list); }
    var paneId = tab && tab.getAttribute ? tabTargetId(tab) : '';
    var pane = paneId ? document.getElementById(paneId) : null;
    schedule(pane || document, 60);
  }


  /* ---------------------------------------------------------------------------
     Sliders - WCAG 4.1.2
     ion.rangeSlider (Shiny's sliderInput) makes <span class="irs-line"
     tabindex="0"> the focusable element and already moves it with the arrow
     keys, but gives it no role, name or value, while the labelled <input> is
     taken out of the tab order. Put role/name/value on the track.
     aria-valuetext reuses the visible value label, which is right for date
     sliders (their raw value is a timestamp).
     ------------------------------------------------------------------------ */

  function syncSlider(input) {
    if (!$) { return; }
    var inst = $(input).data('ionRangeSlider');
    var group = closest(input, '.form-group, .shiny-input-container') || input.parentElement;
    var line = group ? group.querySelector('.irs-line') : null;
    if (!inst || !line) { return; }

    var r = inst.result || {};
    line.setAttribute('role', 'slider');

    var labelId = input.id ? input.id + '-label' : '';
    if (labelId && document.getElementById(labelId)) {
      line.setAttribute('aria-labelledby', labelId);
    } else if (!line.hasAttribute('aria-label')) {
      line.setAttribute('aria-label', input.getAttribute('data-a11y-label') || 'Slider');
    }

    if (r.min !== undefined) { line.setAttribute('aria-valuemin', r.min); }
    if (r.max !== undefined) { line.setAttribute('aria-valuemax', r.max); }
    if (r.from !== undefined) { line.setAttribute('aria-valuenow', r.from); }

    var isRange = inst.options && inst.options.type === 'double';
    var valueText = isRange
      ? [textOf(group.querySelector('.irs-from')), textOf(group.querySelector('.irs-to'))]
          .filter(Boolean).join(' to ')
      : textOf(group.querySelector('.irs-single'));
    if (!valueText && r.from !== undefined) {
      valueText = isRange ? r.from + ' to ' + r.to : String(r.from);
    }
    if (valueText) { line.setAttribute('aria-valuetext', valueText); }
  }

  function enhanceSliders(root) {
    qsa(root, 'input.js-range-slider').forEach(function (input) {
      syncSlider(input);
      if ($ && !input.getAttribute('data-a11y-slider')) {
        input.setAttribute('data-a11y-slider', '1');
        /* ion.rangeSlider writes the value and fires change after every move. */
        $(input).on('change.a11y', function () { syncSlider(input); });
      }
    });
  }


  /* ---------------------------------------------------------------------------
     Disclosures - WCAG 4.1.2
     <button data-a11y-disclosure aria-expanded aria-controls="region">.
     Regions with class a11y-collapse animate (see accessibility.css); others
     use the hidden attribute. Opt-in via the data attribute so third-party
     widgets that manage their own aria-expanded are never touched.
     ------------------------------------------------------------------------ */

  function setDisclosure(btn, expanded) {
    var regionId = btn.getAttribute('aria-controls');
    var region = regionId ? document.getElementById(regionId) : null;
    btn.setAttribute('aria-expanded', expanded ? 'true' : 'false');
    if (region) {
      if (region.classList.contains('a11y-collapse')) {
        if (expanded) { region.classList.add('is-open'); } else { region.classList.remove('is-open'); }
      } else {
        region.hidden = !expanded;
      }
    }
    var evt;
    try {
      evt = new CustomEvent('a11y:toggle', { bubbles: true, detail: { expanded: expanded } });
    } catch (err) {
      evt = document.createEvent('CustomEvent');
      evt.initCustomEvent('a11y:toggle', true, false, { expanded: expanded });
    }
    btn.dispatchEvent(evt);
  }

  document.addEventListener('click', function (e) {
    var btn = closest(e.target, '[data-a11y-disclosure][aria-controls]');
    if (!btn) { return; }
    e.preventDefault();
    setDisclosure(btn, btn.getAttribute('aria-expanded') !== 'true');
  });


  /* ---------------------------------------------------------------------------
     role="button" / role="link" on non-native elements - WCAG 2.1.1
     Any element marked role="button" (e.g. the overview value boxes) becomes
     keyboard-operable: Enter/Space fire the same click handler a mouse does.
     role="link" elements (boxes that navigate) activate on Enter only.
     [data-a11y-toggle] elements also report their state: aria-expanded mirrors
     the .active class the app's own script toggles.
     ------------------------------------------------------------------------ */

  document.addEventListener('keydown', function (e) {
    var isEnter = e.key === 'Enter';
    var isSpace = e.key === ' ' || e.key === 'Spacebar';
    if (!isEnter && !isSpace) { return; }
    var el = e.target;
    var role = el && el.getAttribute ? el.getAttribute('role') : null;
    if (role !== 'button' && role !== 'link') { return; }
    if (role === 'link' && !isEnter) { return; }  /* links activate on Enter only */
    var tag = el.tagName;
    if (tag === 'BUTTON' || tag === 'INPUT' || tag === 'SUMMARY' || tag === 'SELECT' || tag === 'TEXTAREA') { return; }
    if (tag === 'A' && el.hasAttribute('href') && isEnter) { return; } /* native links already do */
    e.preventDefault();
    el.click();
  });

  /* data-a11y-toggle="" means the element's own .active class is the state;
     a selector value names the ancestor the app's click handler toggles
     (stat_box_toggle_attrs() uses ".stat-box-clickable"). */
  function enhanceToggles(root) {
    qsa(root, '[data-a11y-toggle]').forEach(function (el) {
      if (!el.hasAttribute('role')) { el.setAttribute('role', 'button'); }
      if (!el.hasAttribute('tabindex')) { el.setAttribute('tabindex', '0'); }
      var selector = el.getAttribute('data-a11y-toggle');
      var stateEl = (selector && closest(el, selector)) || el;
      var sync = function () {
        el.setAttribute('aria-expanded', stateEl.classList.contains('active') ? 'true' : 'false');
      };
      sync();
      if (!el.getAttribute('data-a11y-toggle-wired') && window.MutationObserver) {
        el.setAttribute('data-a11y-toggle-wired', '1');
        new MutationObserver(sync).observe(stateEl, { attributes: true, attributeFilter: ['class'] });
      }
    });
  }


  /* ---------------------------------------------------------------------------
     Tables - WCAG 1.3.1
     DT's default rownames = TRUE renders an empty header cell; DT column
     filters (filter = "top") render inputs with only a placeholder.
     ------------------------------------------------------------------------ */

  function enhanceTables(root) {
    /* Only the leading empty header is DT's row-number column. Later empty
       headers mean the app passed no column name, which it must fix - naming
       them all "Row" would hide that. */
    qsa(root, 'tr').forEach(function (row) {
      var first = row.cells && row.cells.length ? row.cells[0] : null;
      if (!first || first.tagName !== 'TH') { return; }
      if (first.getAttribute('data-a11y-th') || textOf(first) !== '') { return; }
      first.setAttribute('data-a11y-th', '1');
      first.appendChild(srOnly('Row'));
    });

    qsa(root, '.dataTables_wrapper table').forEach(function (table) {
      var head = table.tHead;
      if (!head || head.rows.length < 2) { return; }
      var titles = head.rows[0].cells;
      var filterRow = head.rows[head.rows.length - 1];
      for (var c = 0; c < filterRow.cells.length; c++) {
        var fields = filterRow.cells[c].querySelectorAll('input, select');
        for (var k = 0; k < fields.length; k++) {
          var field = fields[k];
          if (field.hasAttribute('aria-label') || field.hasAttribute('aria-labelledby')) { continue; }
          var column = titles[c] ? textOf(titles[c]) : '';
          field.setAttribute('aria-label', 'Filter ' + (column || 'column ' + (c + 1)));
        }
      }
    });
  }


  /* ---------------------------------------------------------------------------
     Charts and maps - WCAG 1.1.1
     Name chain: authored (a11y_figure / data-a11y-label) > plotly layout
     title > nearest preceding heading > the tab the chart sits in > generic.
     Plotly gets role="figure", not "img": img would hide the modebar buttons
     inside it. Leaflet maps stay focusable for keyboard panning, so they get
     a named region. renderPlot images get a real alt instead of Shiny's
     default "Plot object".
     ------------------------------------------------------------------------ */

  var HEADING_SELECTOR = 'h1, h2, h3, h4, h5, h6, .box-title, .panel-title, .chart-title, legend';
  var CHART_CONTAINERS = '.tab-pane, .box, .panel, .well, [role="tabpanel"], .modal-content';

  function precedingHeading(el) {
    var anc = el.parentElement;
    for (var depth = 0; anc && depth < 6; depth++, anc = anc.parentElement) {
      var headings = anc.querySelectorAll(HEADING_SELECTOR);
      for (var i = headings.length - 1; i >= 0; i--) {
        var h = headings[i];
        if (h.contains(el)) { continue; }
        if (!(h.compareDocumentPosition(el) & Node.DOCUMENT_POSITION_FOLLOWING)) { continue; }
        var t = textOf(h);
        if (t) { return t; }
      }
      if (anc.matches(CHART_CONTAINERS)) { break; }
    }
    return '';
  }

  function tabLabelFor(el) {
    var pane = closest(el, '.tab-pane');
    if (!pane) { return ''; }
    var tabId = pane.getAttribute('aria-labelledby');
    var tab = tabId ? document.getElementById(tabId) : null;
    if (!tab && pane.id) {
      tab = document.querySelector('[href="#' + pane.id + '"], [data-bs-target="#' + pane.id + '"]');
    }
    return textOf(tab);
  }

  function plotlyTitle(el) {
    var layout = el.layout || el._fullLayout;
    if (!layout || !layout.title) { return ''; }
    var t = typeof layout.title === 'string' ? layout.title : (layout.title.text || '');
    return String(t).replace(/<[^>]*>/g, ' ').replace(/\s+/g, ' ').trim();
  }

  function authoredLabel(el) {
    if (el.getAttribute('data-a11y-label')) { return el.getAttribute('data-a11y-label'); }
    var fig = closest(el, '.a11y-figure[data-a11y-label]');
    return fig ? fig.getAttribute('data-a11y-label') : '';
  }

  function chartName(el, kind) {
    var authored = authoredLabel(el);
    if (authored) { return authored; }
    var derived = (kind === 'chart' ? plotlyTitle(el) : '') || precedingHeading(el) || tabLabelFor(el);
    var noun = kind === 'map' ? 'Map' : 'Chart';
    return derived ? noun + ': ' + derived : noun;
  }

  function enhanceCharts(root) {
    qsa(root, '.html-widget.plotly').forEach(function (el) {
      /* Inside a11y_figure the <figure> already carries the name. */
      if (closest(el, '.a11y-figure[data-a11y-label]')) { return; }
      el.setAttribute('role', 'figure');
      el.setAttribute('aria-label', chartName(el, 'chart'));
    });

    qsa(root, '.html-widget.leaflet').forEach(function (el) {
      /* Always named: the map container itself takes keyboard focus. */
      el.setAttribute('role', 'region');
      el.setAttribute('aria-label', chartName(el, 'map'));
    });

    qsa(root, '.shiny-plot-output img').forEach(function (img) {
      var alt = img.getAttribute('alt');
      if (alt && alt !== 'Plot object') { return; }
      img.setAttribute('alt', chartName(closest(img, '.shiny-plot-output') || img, 'chart'));
    });
  }


  function enhanceHeadings(root) {
    /* Always computed over the whole document: the outline is a page-level
       property, and a re-render must not renumber one fragment in isolation. */
    var headings = qsa(document, 'h1, h2, h3, h4, h5, h6');
    var stack = [];
    headings.forEach(function (h) {
      var native = parseInt(h.tagName.slice(1), 10);
      while (stack.length && stack[stack.length - 1].native >= native) { stack.pop(); }
      var assigned = stack.length ? stack[stack.length - 1].assigned + 1 : 1;
      stack.push({ native: native, assigned: assigned });
      if (assigned === native) {
        if (h.getAttribute('data-a11y-level')) {   /* previously adjusted, now correct */
          h.removeAttribute('aria-level');
          h.removeAttribute('role');
          h.removeAttribute('data-a11y-level');
        }
        return;
      }
      h.setAttribute('role', 'heading');
      h.setAttribute('aria-level', String(assigned));
      h.setAttribute('data-a11y-level', String(assigned));
    });
  }


  /* ---------------------------------------------------------------------------
     Icons - WCAG 1.1.1, 2.5.3
     Shiny's icon() renders <i role="presentation" aria-label="refresh icon">.
     A global aria-label overrides role="presentation", so screen readers say
     "refresh icon" before every labelled button, value box and tab. An icon
     next to real text is decorative: hide it. An icon that is the only content
     of a control is its name source, so it keeps the label.
     ------------------------------------------------------------------------ */

  var CONTROL_SELECTOR = 'button, a, label, summary, [role="button"], [role="tab"], .btn';

  function enhanceIcons(root) {
    qsa(root, 'i[aria-label], svg[aria-label]').forEach(function (icon) {
      if (!/ icon$/.test(icon.getAttribute('aria-label') || '')) { return; }
      var control = closest(icon, CONTROL_SELECTOR);
      var controlNamed = control && (textOf(control) ||
        control.hasAttribute('aria-label') || control.hasAttribute('aria-labelledby'));
      if (control && !controlNamed) { return; }  /* icon-only, unnamed: icon is the name */
      icon.removeAttribute('aria-label');
      icon.removeAttribute('role');
      icon.setAttribute('aria-hidden', 'true');
    });
  }


  /* ---------------------------------------------------------------------------
     Iframes and link hints
     ------------------------------------------------------------------------ */

  function enhanceIframes(root) {
    qsa(root, 'iframe').forEach(function (frame) {
      if (frame.getAttribute('title')) { return; }
      frame.setAttribute('title', frame.getAttribute('data-a11y-title') || 'Embedded content');
    });
  }

  /* Warn before a link changes context unexpectedly: a new tab (WCAG 3.2.5),
     the mail app, or a PDF reader. Appended as hidden text, or to an existing
     aria-label, so the visible text stays the start of the name. */
  function addLinkHint(a, hint, flag, alreadySaysIt) {
    if (a.getAttribute(flag)) { return; }
    a.setAttribute(flag, '1');
    var existing = textOf(a) + ' ' + (a.getAttribute('aria-label') || '');
    if (alreadySaysIt.test(existing)) { return; }
    if (a.hasAttribute('aria-label')) {
      a.setAttribute('aria-label', a.getAttribute('aria-label') + ' ' + hint);
    } else {
      a.appendChild(srOnly(' ' + hint));
    }
  }

  function enhanceLinkHints(root) {
    qsa(root, 'a[target="_blank"]').forEach(function (a) {
      addLinkHint(a, '(opens in new tab)', 'data-a11y-newtab', /new (tab|window)/i);
    });
    qsa(root, 'a[href^="mailto:"]').forEach(function (a) {
      addLinkHint(a, '(opens email)', 'data-a11y-mailto', /\bemail\b|e-mail/i);
    });
    qsa(root, 'a[href$=".pdf"], a[href*=".pdf?"], a[href*=".pdf#"]').forEach(function (a) {
      addLinkHint(a, '(PDF)', 'data-a11y-pdf', /\bpdf\b/i);
    });
  }


  /* ---------------------------------------------------------------------------
     Scheduling
     Full pass at load; afterwards only the subtree Shiny just rendered, batched
     per frame. No document-wide MutationObserver: leaflet and plotly mutate
     the DOM constantly and would make every page pay for it.
     ------------------------------------------------------------------------ */

  var ENHANCERS = [
    wireSkipLinks, labelLandmarks, enhanceIcons, enhanceTabs, enhanceSliders,
    enhanceToggles, enhanceTables, enhanceCharts, enhanceIframes, enhanceLinkHints,
    enhanceHeadings, enhanceScrollRegions
  ];

  function enhance(root) {
    root = root || document;
    for (var i = 0; i < ENHANCERS.length; i++) {
      try {
        ENHANCERS[i](root);
      } catch (err) {
        if (window.console && window.console.warn) { window.console.warn('[a11y]', err); }
      }
    }
  }

  var pending = [];
  var flushQueued = false;

  function flush() {
    flushQueued = false;
    var roots = pending;
    pending = [];
    if (roots.indexOf(document) !== -1) { roots = [document]; }
    roots.forEach(function (r) {
      if (r === document || document.contains(r)) { enhance(r); }
    });
    normalizeMains();
  }

  function schedule(root, delay) {
    window.setTimeout(function () {
      var target = root || document;
      if (pending.indexOf(target) === -1) { pending.push(target); }
      if (!flushQueued) { flushQueued = true; raf(flush); }
    }, delay || 0);
  }

  /* WCAG 3.1.1 - last resort for a page shell that declares no language
     (the R helpers set it server-side). */
  function ensurePageLanguage() {
    if (!document.documentElement.getAttribute('lang')) {
      document.documentElement.setAttribute('lang', 'en');
    }
  }

  /* WCAG 2.4.2 - an app that builds its own header instead of using
     titlePanel() renders no <title>, leaving the browser tab unnamed. Fall
     back to the page's first heading. */
  function ensurePageTitle() {
    if (document.title && document.title.trim()) { return; }
    var heading = document.querySelector('h1, [role="heading"][aria-level="1"]');
    var text = textOf(heading);
    if (text) { document.title = text; }
  }

  function init() {
    ensurePageLanguage();
    ensurePageTitle();
    enhance(document);
    normalizeMains();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

  /* Shiny fires its events through jQuery.trigger(), which native
     addEventListener never receives - these must be jQuery handlers. */
  if ($) {
    $(document).on('shiny:connected', function () { schedule(document, 0); });
    $(document).on('shiny:value', function (e) {
      schedule(e.target, 50);   /* after the value is rendered */
      schedule(e.target, 800);  /* htmlwidgets and DT finish asynchronously */
    });
    $(document).on('shiny:bound', function (e) {
      var el = e.target;
      if (el && el.classList && el.classList.contains('js-range-slider')) { schedule(el.parentElement, 0); }
    });
    $(document).on('shiny:updateinput', function (e) {
      var el = e.target;
      /* updateSliderInput rebuilds the slider DOM after this event fires. */
      if (el && el.classList && el.classList.contains('js-range-slider')) {
        window.setTimeout(function () { syncSlider(el); }, 0);
      }
    });
    $(document).on('shown.bs.tab', onTabShown);
    $(document).on('shown.bs.modal', function (e) { schedule(e.target, 50); });
  } else {
    document.addEventListener('shown.bs.tab', onTabShown);
  }


  /* ---------------------------------------------------------------------------
     Public API
     ------------------------------------------------------------------------ */

  window.A11y = {
    __loaded: true,
    announce: announce,
    enhance: function (root) { enhance(root || document); normalizeMains(); },
    setDisclosure: setDisclosure,
    focusMain: focusMain
  };
})(window, document);
