# Air Inspection Checklist - UI Helper
# Builds the Shiny UI for the air inspection checklist app

#' Create the main UI for the air inspection checklist app
#' @return Shiny UI object
air_inspection_checklist_ui <- function() {
  fluidPage(
    get_universal_text_css(),
    tags$head(
      tags$style(HTML("
        .checklist-section {
          margin-bottom: 20px;
        }
        .checklist-header {
          background-color: #2c5aa0;
          color: white;
          padding: 8px 12px;
          border-radius: 4px 4px 0 0;
          font-size: 16px;
          font-weight: bold;
        }
        .section-subheader {
          background-color: #e8eef5;
          padding: 6px 12px;
          font-weight: bold;
          font-size: 14px;
          border-bottom: 1px solid #ccc;
        }
        .section-inner-subheader {
          background-color: #f0f4f8;
          padding: 4px 12px 4px 24px;
          font-weight: 600;
          font-size: 13px;
          color: #495057;
          border-bottom: 1px solid #dee2e6;
          border-top: 1px solid #dee2e6;
        }
        .checklist-item {
          padding: 6px 12px;
          border-bottom: 1px solid #eee;
          display: flex;
          align-items: center;
          font-size: 13px;
        }
        .checklist-item:hover {
          background-color: #f8f9fa;
        }
        .item-done {
          color: #28a745;
        }
        .item-not-done {
          color: #dc3545;
        }
        .item-sitecode {
          width: 140px;
          font-weight: bold;
        }
        .item-status-icon {
          width: 30px;
          text-align: center;
          font-size: 16px;
        }
        .item-details {
          flex: 1;
          color: #666;
          font-size: 12px;
        }
        .item-bug-badge {
          display: inline-block;
          padding: 2px 8px;
          border-radius: 10px;
          font-size: 11px;
          font-weight: bold;
          margin-left: 8px;
        }
        .bug-red {
          background-color: #dc3545;
          color: white;
        }
        .bug-blue {
          background-color: #007bff;
          color: white;
        }
        .bug-pending {
          background-color: #ffc107;
          color: #333;
        }
        .bug-none {
          background-color: #6c757d;
          color: white;
        }
        .badge-treatment {
          padding: 2px 8px;
          border-radius: 10px;
          font-size: 11px;
          font-weight: bold;
          margin-left: 8px;
          background-color: #6f42c1;
          color: white;
        }
        .item-active-trt {
          background-color: #f3eefb;
        }
        .summary-box {
          text-align: center;
          padding: 15px;
          border-radius: 8px;
          margin-bottom: 10px;
        }
        .summary-number {
          font-size: 28px;
          font-weight: bold;
        }
        .summary-label {
          font-size: 12px;
          color: #666;
        }
        /* AirMap collapsible sub-headers */
        .airmap-toggle {
          display: flex;
          align-items: center;
          gap: 6px;
          transition: background-color 0.15s;
        }
        .airmap-toggle:hover {
          background-color: #dde4ef;
        }
        .airmap-chevron {
          transition: transform 0.2s;
          font-size: 12px;
          width: 16px;
        }
        .airmap-toggle.open .airmap-chevron {
          transform: rotate(90deg);
        }
        /* Claimed row styling */
        .checklist-item.item-claimed {
          background-color: #fff3cd;
          border-left: 4px solid #ffc107;
        }
        .checklist-item.item-claimed:hover {
          background-color: #ffecb5;
        }
        .claim-badge {
          display: inline-block;
          padding: 2px 8px;
          border-radius: 10px;
          font-size: 11px;
          font-weight: bold;
          margin-left: 8px;
          background-color: #ffc107;
          color: #333;
        }
        .claim-badge.claim-mismatch {
          background-color: #fd7e14;
          color: white;
        }
        .claim-mismatch-icon {
          cursor: help;
        }
        /* Employee identity bar */
        .employee-bar {
          background: linear-gradient(135deg, #2c5aa0, #1e3d6f);
          color: white;
          padding: 8px 16px;
          border-radius: 6px;
          margin-bottom: 12px;
          display: flex;
          align-items: center;
          justify-content: space-between;
          font-size: 14px;
        }
        .employee-bar .emp-name {
          font-weight: bold;
        }
        .employee-bar .emp-hint {
          font-size: 12px;
          opacity: 0.8;
        }
        .no-employee-bar {
          background-color: #f8d7da;
          color: #721c24;
          padding: 8px 16px;
          border-radius: 6px;
          margin-bottom: 12px;
          font-size: 13px;
        }
      ")),
      # JavaScript for collapsible AirMap sections and claim feature
      tags$script(HTML("
        // =====================================================================
        // AirMap collapsible toggle
        // =====================================================================
        $(document).on('click', '.airmap-toggle', function() {
          var target = $(this).data('target');
          var panel = $('#' + target);
          $(this).toggleClass('open');
          panel.slideToggle(200);
        });

        // =====================================================================
        // Claim feature using localStorage
        // =====================================================================
        var currentEmployee = null; // {emp_num, emp_name}

        // Receive employee info from server
        Shiny.addCustomMessageHandler('set_employee', function(data) {
          currentEmployee = data;
          renderEmployeeBar();
          applyClaimsToUI();
        });

        function getClaimsKey() {
          // Key scoped to date so claims reset daily
          var today = new Date().toISOString().slice(0, 10);
          return 'air_checklist_claims_' + today;
        }

        function getClaims() {
          try {
            var raw = localStorage.getItem(getClaimsKey());
            return raw ? JSON.parse(raw) : {};
          } catch(e) { return {}; }
        }

        function saveClaims(claims) {
          localStorage.setItem(getClaimsKey(), JSON.stringify(claims));
        }

        function toggleClaim(sitecode) {
          if (!currentEmployee) {
            alert('No employee selected.\\n\\nGo back to the dashboard and select your name from the Employee dropdown before opening this app.');
            return;
          }
          var claims = getClaims();
          if (claims[sitecode] && claims[sitecode].emp_num === currentEmployee.emp_num) {
            // Unclaim
            delete claims[sitecode];
          } else {
            // Claim (overwrite anyone else's claim)
            claims[sitecode] = {
              emp_num: currentEmployee.emp_num,
              emp_name: currentEmployee.emp_name,
              time: new Date().toISOString()
            };
          }
          saveClaims(claims);
          applyClaimToRow(sitecode, claims[sitecode]);
        }

        function applyClaimsToUI() {
          var claims = getClaims();
          Object.keys(claims).forEach(function(sitecode) {
            applyClaimToRow(sitecode, claims[sitecode]);
          });
        }

        function applyClaimToRow(sitecode, claimData) {
          var row = $('[data-sitecode=\"' + sitecode + '\"]');
          if (row.length === 0) return;

          // Remove existing claim badges
          row.find('.claim-badge').remove();
          row.removeClass('item-claimed');

          if (claimData) {
            row.addClass('item-claimed');

            // Check if site was inspected by someone else
            var inspectorEmp = row.attr('data-inspector-emp') || '';
            var wasInspected = row.attr('data-inspected') === 'true';
            var badge = '';

            if (wasInspected && inspectorEmp && inspectorEmp !== claimData.emp_num) {
              badge = '<span class=\"claim-badge claim-mismatch\" title=\"Claimed by ' +
                claimData.emp_name + ' but inspected by a different employee\">' +
                '\\u26a0 Claimed: ' + claimData.emp_name + '</span>';
            } else {
              badge = '<span class=\"claim-badge\">Claimed: ' + claimData.emp_name + '</span>';
            }
            row.find('.item-details').append(badge);
          }
        }

        function renderEmployeeBar() {
          // Remove old bar if present
          $('#employee-bar-container').remove();

          if (currentEmployee) {
            var html = '<div id=\"employee-bar-container\" class=\"employee-bar\">' +
              '<span>\\ud83d\\udc64 Logged in as: <span class=\"emp-name\">' +
              currentEmployee.emp_name + '</span> (#' + currentEmployee.emp_num + ')</span>' +
              '<span class=\"emp-hint\">Double-click a site to claim it</span></div>';
            $('.checklist-value-boxes').before(html);
          } else {
            var html = '<div id=\"employee-bar-container\" class=\"no-employee-bar\">' +
              '\\u26a0 No employee selected. Go back to the dashboard and pick your name to enable claiming sites.</div>';
            // Insert before value boxes if they exist, else at top
            if ($('.checklist-value-boxes').length) {
              $('.checklist-value-boxes').before(html);
            }
          }
        }

        // Double-click/double-tap handler for claiming
        $(document).on('dblclick', '.checklist-item[data-sitecode]', function(e) {
          e.preventDefault();
          var sitecode = $(this).attr('data-sitecode');
          toggleClaim(sitecode);
        });

        // Double-tap support for mobile
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

        // Re-apply claims whenever Shiny re-renders the checklist
        $(document).on('shiny:value', function(event) {
          if (event.name === 'checklist_display') {
            setTimeout(function() {
              applyClaimsToUI();
              renderEmployeeBar();
            }, 100);
          }
        });
      "))
    ),

    titlePanel("Air Inspection Checklist"),

    sidebarLayout(
      sidebarPanel(
        width = 3,
        actionButton("refresh", "Refresh Data",
                     icon = icon("refresh"),
                     class = "btn-success",
                     style = "width: 100%;"),
        hr(),

        selectInput("facility_filter", "Facility:", choices = NULL),

        selectInput("foreman_filter", "FOS:",
                    choices = NULL),

        selectInput("zone_filter", "Zone Display:",
                    choices = c("P1 Only" = "1",
                                "P2 Only" = "2",
                                "P1 and P2 Combined" = "1,2"),
                    selected = "1"),

        sliderInput("lookback_days", "Lookback Days:",
                    min = 1, max = 7, value = 2, step = 1),

        checkboxInput("show_unfinished_only", "Show Only Unfinished",
                      value = FALSE),

        checkboxInput("show_active_treatment", "Show Prehatch Treatment Sites",
                      value = FALSE),

        hr(),
        dateInput("analysis_date", "Analysis Date:",
                  value = Sys.Date(),
                  max = Sys.Date()),
        
        hr(),
        div(style = "font-size: 12px; color: #888; padding: 5px;",
            p(icon("info-circle"), " RED air sites checked for inspections in the last N days."),
            p(icon("check-circle", class = "item-done"), " = Inspected (with employee # and dip count)"),
            p(icon("times-circle", class = "item-not-done"), " = Not inspected")
        )
      ),

      mainPanel(
        width = 9,
        uiOutput("checklist_display")
      )
    )
  )
}
