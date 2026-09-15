# =============================================================================
# ACCESSIBILITY HELPERS (ADA / WCAG 2.1 AA)
# =============================================================================
#
# Shared accessibility layer for every MMCD Shiny app. Built to be adopted with
# a single rename at an app's outermost page constructor:
#
#     fluidPage(...)      ->  accessible_page(...)
#     dashboardPage(...)  ->  accessible_dashboard_page(...)
#
# That one change gives the app:
#   * a "Skip to main content" link (WCAG 2.4.1 Bypass Blocks)
#   * >= 3:1 contrast borders on buttons / form fields (WCAG 1.4.11 Non-text Contrast)
#   * a visible keyboard focus indicator (WCAG 2.4.7 Focus Visible)
#   * a declared page language (WCAG 3.1.1 Language of Page)
#
# The skip link finds the main content region at runtime (see the fallback chain
# in get_accessibility_js) so it works without any further per-app markup. Apps
# that want an exact target can mark the region explicitly with
# accessible_main_panel() or main_landmark().
#
# This file is standalone on purpose - it does not require any other shared/
# file to be sourced first, and all Shiny calls are namespaced so it works
# whether or not the app has attached shiny/shinydashboard.
#
# Usage in a new app:
#     source("../../shared/accessibility_helpers.R")
#     ui <- accessible_page(
#       get_universal_text_css(),
#       titlePanel("My App"),
#       sidebarLayout(
#         sidebarPanel(...),
#         accessible_main_panel(...)   # optional but preferred
#       )
#     )
# =============================================================================


# -----------------------------------------------------------------------------
# Design tokens
# -----------------------------------------------------------------------------

#' Default border color for interactive control boundaries.
#'
#' #55606B measures ~6.4:1 against white and ~5.9:1 against Bootstrap's #f5f5f5
#' well/sidebar background - comfortably past the 3:1 required by WCAG 1.4.11.
#'
#' This is deliberately a fixed neutral rather than a value pulled from
#' shared/color_themes.R: those palettes define `surface` colors for dark
#' data-viz cards (bg #0f172a etc.), while every app actually renders its
#' controls on a light page background. Deriving from them would produce
#' near-invisible borders.
A11Y_BORDER_COLOR <- "#55606B"

#' Focus ring color. ~8:1 against white; stands clear of the border color so
#' the focused control is unambiguous.
A11Y_FOCUS_COLOR <- "#1d4ed8"


# -----------------------------------------------------------------------------
# CSS
# -----------------------------------------------------------------------------

#' Get Accessibility CSS
#'
#' Returns HTML tags with the shared accessibility CSS. Injected automatically
#' by accessible_page() / accessible_dashboard_page(); call it directly only if
#' an app builds its page shell by hand.
#'
#' Covers control-boundary contrast, focus visibility, skip-link presentation,
#' and a Bootstrap-version-independent .sr-only.
#'
#' @param border_color Border color for control boundaries (default A11Y_BORDER_COLOR)
#' @param focus_color Focus ring color (default A11Y_FOCUS_COLOR)
#' @return HTML tags containing CSS styles
#' @export
get_accessibility_css <- function(border_color = A11Y_BORDER_COLOR,
                                  focus_color  = A11Y_FOCUS_COLOR) {

  shiny::tags$style(shiny::HTML(paste0("
      /* ===================================================================
         MMCD Shared Accessibility CSS (WCAG 2.1 AA)
         Source: shared/accessibility_helpers.R
         =================================================================== */

      /* --- WCAG 1.4.11 Non-text Contrast -------------------------------
         Unfilled controls (text inputs, selects, textareas, default/neutral
         buttons) rely on their border to show the hit area. Bootstrap and
         AdminLTE default to #ccc / #ced4da, which is ~1.5:1 on white and
         fails. Filled buttons (.btn-primary etc.) are intentionally excluded:
         their background color already delineates the hit area, and forcing a
         gray border on them would fight the app's theme.                    */
      .form-control,
      .selectize-input,
      .selectize-control.single .selectize-input,
      .selectize-control.multi .selectize-input,
      textarea,
      select,
      input[type='text'],
      input[type='number'],
      input[type='date'],
      input[type='search'],
      input[type='email'],
      input[type='password'],
      .btn-default,
      .btn:not(.btn-primary):not(.btn-success):not(.btn-info):not(.btn-warning):not(.btn-danger):not(.btn-link) {
        border: 1px solid ", border_color, " !important;
      }

      /* Shiny renders date ranges and numeric steppers as grouped inputs;
         keep the group's outer boundary visible too. */
      .input-group .form-control,
      .input-group-addon,
      .input-daterange .input-group-addon {
        border: 1px solid ", border_color, " !important;
      }

      /* Disabled controls keep a perceivable boundary but read as inactive.
         WCAG 1.4.11 exempts disabled controls, so a lighter tone is fine. */
      .form-control[disabled],
      .form-control[readonly],
      fieldset[disabled] .form-control,
      .btn[disabled],
      .btn.disabled {
        border-color: #8a929c !important;
      }

      /* --- WCAG 2.4.7 Focus Visible ------------------------------------- */
      a:focus-visible,
      button:focus-visible,
      input:focus-visible,
      select:focus-visible,
      textarea:focus-visible,
      [tabindex]:focus-visible,
      .btn:focus-visible,
      .selectize-input.focus {
        outline: 3px solid ", focus_color, " !important;
        outline-offset: 2px !important;
      }

      /* Older engines without :focus-visible still get a ring. */
      .btn:focus,
      .form-control:focus {
        outline: 3px solid ", focus_color, " !important;
        outline-offset: 2px !important;
      }

      /* The main landmark is focused programmatically by the skip link.
         It is not an interactive control, so suppress its ring - the page
         scroll position is the feedback the user needs. */
      .a11y-main-landmark:focus,
      .a11y-main-landmark:focus-visible {
        outline: none !important;
      }

      /* --- WCAG 2.4.1 Bypass Blocks: skip link --------------------------
         Off-screen until focused, then pinned to the top-left so the first
         Tab on page load reveals it.                                        */
      .a11y-skip-link {
        position: absolute !important;
        left: -9999px !important;
        top: auto !important;
        width: 1px !important;
        height: 1px !important;
        overflow: hidden !important;
        z-index: 100000 !important;
      }

      .a11y-skip-link:focus,
      .a11y-skip-link:focus-visible {
        position: fixed !important;
        left: 8px !important;
        top: 8px !important;
        width: auto !important;
        height: auto !important;
        overflow: visible !important;
        padding: 10px 18px !important;
        background: #ffffff !important;
        color: #10253f !important;
        font-weight: 700 !important;
        text-decoration: underline !important;
        border: 2px solid ", border_color, " !important;
        border-radius: 4px !important;
        box-shadow: 0 2px 10px rgba(0,0,0,0.3) !important;
        outline: 3px solid ", focus_color, " !important;
        outline-offset: 2px !important;
      }

      /* --- Visually-hidden utility --------------------------------------
         Bootstrap 3 ships .sr-only and Bootstrap 5 renamed it
         .visually-hidden. Define both here so visually-hidden labels behave
         the same in the bslib apps as in the Bootstrap 3 apps.              */
      .sr-only,
      .visually-hidden {
        position: absolute !important;
        width: 1px !important;
        height: 1px !important;
        padding: 0 !important;
        margin: -1px !important;
        overflow: hidden !important;
        clip: rect(0, 0, 0, 0) !important;
        white-space: nowrap !important;
        border: 0 !important;
      }
    ")))
}


# -----------------------------------------------------------------------------
# JS - runtime discovery of the main content region
# -----------------------------------------------------------------------------

#' Get Accessibility JS
#'
#' Returns the script that powers the skip link. Rather than requiring every
#' app to hand-mark a target, the handler walks a fallback chain and focuses
#' the first match, so a bare accessible_page() rename is enough:
#'
#'   1. #main-content              - explicit target (main_landmark)
#'   2. <main>                     - any semantic main element
#'   3. [role='main']              - ARIA-marked region
#'   4. .content-wrapper           - AdminLTE / shinydashboard body
#'   5. .tab-content               - tabsetPanel body
#'   6. Shiny's mainPanel column   - sidebarLayout output side
#'   7. first h1/h2                - last-resort content anchor
#'
#' @return HTML tags containing the script
#' @export
get_accessibility_js <- function() {

  shiny::tags$script(shiny::HTML("
      (function () {
        var SELECTORS = [
          '#main-content',
          'main',
          '[role=\"main\"]',
          '.content-wrapper',
          '.tab-content',
          '.row > div[class*=\"col-sm-\"]:not(:first-child)',
          'h1, h2'
        ];

        function findMain() {
          for (var i = 0; i < SELECTORS.length; i++) {
            var el = document.querySelector(SELECTORS[i]);
            if (el && el.offsetParent !== null) { return el; }
          }
          return null;
        }

        /* WCAG 1.3.1 / 2.4.1 - make sure the page exposes exactly one main
           landmark. Apps that call accessible_main_panel() already have a
           real <main>; the rest (shinydashboard's .content-wrapper, plain
           tabsetPanel layouts) get promoted here so screen-reader landmark
           navigation works without any per-app markup. */
        function ensureLandmark() {
          if (document.querySelector('main, [role=\"main\"]')) { return; }
          var el = findMain();
          if (!el) { return; }
          el.setAttribute('role', 'main');
          /* Name the landmark so it announces as 'Main content, region' and
             reads clearly in the screen-reader landmark list, matching what
             main_landmark() sets server-side. */
          if (!el.hasAttribute('aria-label')) {
            el.setAttribute('aria-label', 'Main content');
          }
          if (!el.id) { el.id = 'main-content'; }
          if (!el.hasAttribute('tabindex')) { el.setAttribute('tabindex', '-1'); }
          el.classList.add('a11y-main-landmark');
        }

        function focusMain(evt) {
          var target = findMain();
          if (!target) { return; }
          if (evt) { evt.preventDefault(); }

          /* A non-interactive container needs tabindex to accept focus.
             -1 keeps it out of the normal tab sequence.                */
          if (!target.hasAttribute('tabindex')) {
            target.setAttribute('tabindex', '-1');
          }
          target.classList.add('a11y-main-landmark');
          target.focus({ preventScroll: false });
          target.scrollIntoView({ block: 'start', behavior: 'smooth' });

          /* Reflect the jump in the URL without triggering a reload. */
          if (target.id && window.history && window.history.replaceState) {
            window.history.replaceState(null, '', '#' + target.id);
          }
        }

        function wire() {
          ensureLandmark();
          var links = document.querySelectorAll('.a11y-skip-link');
          for (var i = 0; i < links.length; i++) {
            if (links[i].dataset.a11yWired) { continue; }
            links[i].dataset.a11yWired = '1';
            links[i].addEventListener('click', focusMain);
            links[i].addEventListener('keydown', function (e) {
              if (e.key === 'Enter' || e.key === ' ' || e.key === 'Spacebar') {
                focusMain(e);
              }
            });
          }
        }

        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', wire);
        } else {
          wire();
        }

        /* Shiny can render the skip link after initial load (renderUI,
           router-driven views), so re-wire once Shiny reports ready. */
        document.addEventListener('shiny:connected', wire);
      })();
    "))
}


#' Bundle the accessibility CSS + JS for the document head.
#'
#' @param ... Passed through to get_accessibility_css()
#' @return A tags$head() containing the shared accessibility assets
#' @export
accessibility_head <- function(...) {
  shiny::tags$head(
    get_accessibility_css(...),
    get_accessibility_js()
  )
}


# -----------------------------------------------------------------------------
# Components
# -----------------------------------------------------------------------------

#' Skip link (WCAG 2.4.1 Bypass Blocks)
#'
#' Render this as the first element on the page so it is the first thing the
#' keyboard reaches. Hidden until focused.
#'
#' @param target_id Fragment id to jump to. The JS fallback chain runs if no
#'   element with this id exists, so the link still works when unset.
#' @param label Visible text once focused
#' @return A Shiny tag
#' @export
skip_link <- function(target_id = "main-content", label = "Skip to main content") {
  shiny::tags$a(
    href  = paste0("#", target_id),
    class = "a11y-skip-link",
    label
  )
}


#' Wrap content in a <main> landmark
#'
#' Gives the skip link an exact destination and adds a navigable landmark
#' @param ... Content to wrap
#' @param id Element id (must match the skip link target)
#' @param label Accessible name for the landmark
#' @return A Shiny tag
#' @export
main_landmark <- function(..., id = "main-content", label = "Main content") {
  shiny::tags$main(
    id           = id,
    class        = "a11y-main-landmark",
    tabindex     = "-1",
    role         = "main",
    `aria-label` = label,
    ...
  )
}


#' Accessible drop-in for shiny::mainPanel()
#'
#' Identical to mainPanel() but emits the content inside a <main> landmark, so
#' the skip link lands past the title banner and the sidebar controls.
#'
#' @param ... Content (as for mainPanel)
#' @param width Bootstrap column width (as for mainPanel)
#' @param id Landmark id
#' @param label Accessible name for the landmark
#' @return A Shiny tag
#' @export
accessible_main_panel <- function(..., width = 8, id = "main-content",
                                  label = "Main content") {
  shiny::mainPanel(
    main_landmark(..., id = id, label = label),
    width = width
  )
}


# -----------------------------------------------------------------------------
# Page shells
# -----------------------------------------------------------------------------

#' Accessible drop-in for shiny::fluidPage()
#'
#' Same argument contract as fluidPage() - pass `theme` through for bslib apps.
#' Injects the shared CSS/JS and puts the skip link ahead of all page content.
#'
#' @param ... UI elements (as for fluidPage)
#' @param title Browser title (as for fluidPage)
#' @param theme Bootstrap theme (as for fluidPage; supports bslib objects)
#' @param lang Page language for WCAG 3.1.1 (default "en")
#' @param skip_target Fragment id the skip link aims at
#' @return A Shiny UI definition
#' @export
accessible_page <- function(..., title = NULL, theme = NULL, lang = "en",
                            skip_target = "main-content") {
  shiny::fluidPage(
    title = title,
    theme = theme,
    lang  = lang,
    accessibility_head(),
    skip_link(skip_target),
    ...
  )
}


#' Accessible drop-in for shinydashboard::dashboardPage()
#'
#' A true drop-in: the argument contract is identical to dashboardPage(), so
#' adoption is a single rename and the app's dashboardBody() stays untouched.
#'
#'     dashboardPage(header, sidebar, dashboardBody(...))
#'  -> accessible_dashboard_page(header, sidebar, dashboardBody(...))
#'
#' Two deliberate choices here:
#'
#' 1. The skip link is a sibling rendered *before* dashboardPage() rather than
#'    inside the body. dashboardHeader and dashboardSidebar precede
#'    dashboardBody in the DOM, so a link placed in the body would sit after
#'    the very chrome it is meant to bypass.
#'
#' 2. The body is NOT wrapped in a <main> element. AdminLTE styles its
#'    .content-wrapper and that element's children, and injecting a wrapper
#'    between them risks breaking the dashboard layout. Instead the shared JS
#'    promotes .content-wrapper to the main landmark at runtime (see
#'    get_accessibility_js), which gives the same screen-reader semantics with
#'    no structural change.
#'
#' @param ... Arguments passed straight through to dashboardPage()
#' @param skip_target Fragment id the skip link aims at
#' @return A Shiny UI definition
#' @export
accessible_dashboard_page <- function(..., skip_target = "main-content") {
  if (!requireNamespace("shinydashboard", quietly = TRUE)) {
    stop("accessible_dashboard_page() requires the shinydashboard package")
  }

  shiny::tagList(
    accessibility_head(),
    skip_link(skip_target),
    shinydashboard::dashboardPage(...)
  )
}
