# display_functions.R
# Display and visualization functions for Section Cards

#' Generate HTML for section cards with printable layout
#' 
#' Creates a printable layout of section cards (6 per page)
#' Each card has a title section and a data table
#' 
#' @param data Data frame with breeding site information
#' @param title_fields Vector of field names to display in title section
#' @param table_fields Vector of field names to display in table
#' @param num_rows Number of empty rows to include in each card table
#' @param split_by_section Logical, if TRUE split cards by section (no mixing on pages)
#' @param split_by_priority Logical, if TRUE split cards by priority (no mixing on pages)
#' @return HTML string with printable section cards
generate_section_cards_html <- function(data, title_fields, table_fields, num_rows = 5, split_by_section = FALSE, split_by_priority = FALSE, split_by_type = FALSE, progress_fn = NULL, double_sided = FALSE, watermark_fields = NULL, cards_per_page = 6) {
  
  # Map facility codes to full names using db_helpers
  facility_lookup <- get_facility_lookup()
  facility_map <- setNames(facility_lookup$full_name, facility_lookup$short_name)
  
  # Map FOS codes to names using db_helpers
  fos_lookup <- get_foremen_lookup()
  fos_map <- setNames(fos_lookup$shortname, sprintf("%04d", as.numeric(fos_lookup$emp_num)))
  
  # Define field labels for display
  field_labels <- list(
    # Common fields
    sitecode = "Site Code",
    priority = "Priority",
    culex = "Culex",
    remarks = "Remarks",
    section = "Section",
    zone = "Zone",
    facility = "Facility",
    fosarea = "FOS Area",
    foreman = "Foreman",
    
    # Breeding site specific fields
    acres = "Acres",
    type = "Type",
    air_gnd = "Air/Gnd",
    spr_aedes = "Spring Aedes",
    perturbans = "Perturbans",
    prehatch = "Prehatch",
    prehatch_calc = "Prehatch Calculation",
    sample = "Sample Site",
    drone = "Drone",
    
    # Dynamic columns from JK table (common extras)
    ra = "Restricted",
    airmap_num = "Airmap #",
    partialtrt = "Partial Trt",
    perimacres = "Perim Acres",
    elevthresh = "Elev Thresh",
    mapped = "Mapped",
    catinsp = "Cat Insp",
    percent = "Percent",
    dip = "Dip",
    
    # Structure specific fields
    s_type = "Type",
    status_udw = "Status",
    sqft = "Sq Ft",
    chambers = "Chambers",
    
    # Table fields (these will be empty columns for manual entry)
    date = "Date",
    wet_pct = "Wet %",
    emp_num = "Emp #",
    num_dip = "#/Dip",
    sample_num = "Sample #",
    amt = "Amt",
    mat = "Mat"
  )
  
  # Start HTML with print-friendly CSS
  grid_rows <- ceiling(cards_per_page / 2)
  
  # ---- Density scaling ----
  # Scale factor: 6 cards = 1.0 (baseline), more cards = smaller, fewer = capped at 1.0
  card_scale <- min(1.0, 6 / cards_per_page)  # 4->1.0, 6->1.0, 8->0.75, 10->0.6, 12->0.5
  # Page margins shrink to reclaim space at higher density
  page_margin <- switch(as.character(cards_per_page),
    "4"  = "0.5in",
    "6"  = "0.5in",
    "8"  = "0.4in",
    "10" = "0.3in",
    "12" = "0.25in",
    "0.5in"
  )
  margin_num <- as.numeric(gsub("in", "", page_margin))
  # Usable print area on letter paper (8.5 x 11).
  # This is set as the card-page height. Since card-page has padding = page_margin
  # and box-sizing: border-box, the CONTENT area is (11 - 2*margin) - 2*margin.
  # But we set usable_height = 11 (the full page) because @page margin is 0.
  usable_height <- 11
  # Gap between cards scales down
  grid_gap <- max(2, round(15 * card_scale))
  
  # ---- Dynamic font / row sizing ----
  # Content area inside card-page after padding is subtracted (border-box)
  content_height_in <- 11 - 2 * margin_num
  # Available height per card in px (96 dpi: 1in = 96px)
  card_height_px <- (content_height_in * 96 / grid_rows) - grid_gap
  # Account for card border (2px top + 2px bottom)
  card_height_px <- card_height_px - 4

  # Baseline VISIBLE sizes (what the user sees after scale transform)
  base_title_font   <- 14
  base_remarks_font <- 11
  base_info_font    <- 9
  base_header_pad   <- 6
  base_header_gap   <- 3
  base_th_font      <- 9
  base_td_height    <- 25
  base_td_pad       <- 4
  base_td_font      <- 10

  # ---- Squeeze: computed against VISIBLE card height ----
  # card_height_px is the grid cell = the visible size on paper.
  # Base sizes are also visible sizes. Squeeze is purely: does it fit?
  hdr1 <- base_title_font * 1.15 + base_remarks_font * 1.15 * 3 + base_header_gap * 3 + 2 * base_header_pad + 4
  th1  <- base_th_font * 1.15 + 2 * base_td_pad + 2
  rows_space1 <- card_height_px - hdr1 - th1
  td1 <- rows_space1 / num_rows
  squeeze1 <- min(1.0, td1 / base_td_height)

  # Pass 2: re-estimate header with squeezed fonts, reclaim space
  s1_title   <- max(7, floor(base_title_font   * squeeze1))
  s1_remarks <- max(6, floor(base_remarks_font * squeeze1))
  s1_hpad    <- max(1, floor(base_header_pad   * squeeze1))
  s1_hgap    <- max(1, floor(base_header_gap   * squeeze1))
  s1_th      <- max(5, floor(base_th_font      * squeeze1))
  s1_tdpad   <- max(1, floor(base_td_pad       * squeeze1))
  hdr2 <- s1_title * 1.15 + s1_remarks * 1.15 * 3 + s1_hgap * 3 + 2 * s1_hpad + 4
  th2  <- s1_th * 1.15 + 2 * s1_tdpad + 2
  rows_space2 <- card_height_px - hdr2 - th2
  td2 <- rows_space2 / num_rows
  squeeze <- min(1.0, td2 / base_td_height)

  # Compute VISIBLE sizes (what the user sees on paper)
  vis_title_font   <- max(7,  floor(base_title_font   * squeeze))
  vis_remarks_font <- max(6,  floor(base_remarks_font * squeeze))
  vis_info_font    <- max(6,  floor(base_info_font    * squeeze))
  vis_header_pad   <- max(1,  floor(base_header_pad   * squeeze))
  vis_header_gap   <- max(1,  floor(base_header_gap   * squeeze))
  vis_th_font      <- max(5,  floor(base_th_font      * squeeze))
  vis_td_pad       <- max(1,  floor(base_td_pad       * squeeze))
  vis_td_font      <- max(6,  floor(base_td_font      * squeeze))

  # CSS variables must be set in INTERNAL px (before scale transform).
  # The card is rendered at 1/card_scale size then scaled down by card_scale,
  # so we divide visible sizes by card_scale to get internal values.
  sc_title_font   <- round(vis_title_font   / card_scale)
  sc_remarks_font <- round(vis_remarks_font / card_scale)
  sc_info_font    <- round(vis_info_font    / card_scale)
  sc_header_pad   <- round(vis_header_pad   / card_scale)
  sc_header_gap   <- round(vis_header_gap   / card_scale)
  sc_th_font      <- round(vis_th_font      / card_scale)
  sc_td_pad       <- round(vis_td_pad       / card_scale)
  sc_td_font      <- round(vis_td_font      / card_scale)
  # Badge padding
  sc_badge_vpad <- max(1, round(2 * squeeze / card_scale))
  sc_badge_hpad <- max(2, round(4 * squeeze / card_scale))
  
  html <- paste0('
  <style>
    /* @page margin set to 0 so Chrome does not render its default
       headers (date/time) and footers (URL/page numbers).
       Content spacing is handled via padding on .card-page. */
    @page {
      size: letter;
      margin: 0;
    }
  </style>
  
  <div class="cards-container" style="
      --sc-page-margin: ', page_margin, ';
      --sc-usable-height: ', usable_height, 'in;
      --sc-grid-rows: ', grid_rows, ';
      --sc-grid-gap: ', grid_gap, 'px;
      --sc-card-scale: ', card_scale, ';
      --sc-title-font: ', sc_title_font, 'px;
      --sc-remarks-font: ', sc_remarks_font, 'px;
      --sc-info-font: ', sc_info_font, 'px;
      --sc-header-pad: ', sc_header_pad, 'px;
      --sc-header-gap: ', sc_header_gap, 'px;
      --sc-th-font: ', sc_th_font, 'px;
      --sc-td-pad: ', sc_td_pad, 'px;
      --sc-td-font: ', sc_td_font, 'px;
      --sc-badge-pad: ', sc_badge_vpad, 'px ', sc_badge_hpad, 'px;
    " data-double-sided="', tolower(as.character(double_sided)), '">
  ')
  
  # =========================================================================
  # OPTIMIZATION: Pre-build table header + empty rows (identical for all cards)
  # =========================================================================
  table_header_parts <- character(length(table_fields))
  for (i in seq_along(table_fields)) {
    field <- table_fields[i]
    label <- if (field %in% names(field_labels)) field_labels[[field]] else humanize_column_name(field)
    col_class <- if (i %% 2 == 1) "col-odd" else "col-even"
    table_header_parts[i] <- paste0('        <th class="', col_class, '">', label, '</th>\n')
  }
  prebuilt_table_header <- paste0(
    '    <table class="card-table">\n      <thead><tr>\n',
    paste0(table_header_parts, collapse = ""),
    '      </tr></thead>\n      <tbody>\n'
  )
  
  # Pre-build empty rows
  empty_row_cells <- character(length(table_fields))
  for (i in seq_along(table_fields)) {
    col_class <- if (i %% 2 == 1) "col-odd" else "col-even"
    empty_row_cells[i] <- paste0('          <td class="', col_class, '"></td>\n')
  }
  single_empty_row <- paste0('        <tr>\n', paste0(empty_row_cells, collapse = ""), '        </tr>\n')
  prebuilt_empty_rows <- paste0(rep(single_empty_row, num_rows), collapse = "")
  prebuilt_table_footer <- paste0(prebuilt_empty_rows, '      </tbody>\n    </table>\n')
  
  # Collect all HTML parts into a vector (much faster than paste0 accumulation)
  html_parts <- list(html)
  part_idx <- 2
  
  # Helper: report progress periodically
  total_rows <- nrow(data)
  cards_done <- 0
  report_progress <- function(cards_done) {
    if (!is.null(progress_fn) && (cards_done %% 50 == 0 || cards_done == total_rows)) {
      pct <- cards_done / total_rows
      progress_fn(pct, sprintf("Card %d / %d", cards_done, total_rows))
    }
  }
  
  # Helper: generate one card using pre-built table template
  build_card <- function(row) {
    generate_card_html(row, title_fields, table_fields, num_rows,
                       field_labels, facility_map, fos_map,
                       prebuilt_table_header, prebuilt_table_footer,
                       watermark_fields)
  }
  
  # Handle splitting by section, priority, and/or type
  if (split_by_section || split_by_priority || split_by_type) {
    # Determine grouping columns
    grouping_cols <- c()
    if (split_by_section && "section" %in% names(data)) {
      grouping_cols <- c(grouping_cols, "section")
    }
    if (split_by_priority && "priority" %in% names(data)) {
      grouping_cols <- c(grouping_cols, "priority")
    }
    if (split_by_type && "s_type" %in% names(data)) {
      grouping_cols <- c(grouping_cols, "s_type")
    }
    
    # Sort by grouping columns (NAs go last)
    if (length(grouping_cols) > 0) {
      data <- data[do.call(order, data[grouping_cols]), ]
      
      # ---- Drop rows with NA in ANY grouping column ----
      # These come from LEFT JOIN misses and can't be meaningfully grouped.
      na_mask <- rep(FALSE, nrow(data))
      for (gc in grouping_cols) {
        na_mask <- na_mask | is.na(data[[gc]])
      }
      if (any(na_mask)) {
        message(sprintf("[section_cards] Dropping %d rows with NA grouping columns", sum(na_mask)))
        data <- data[!na_mask, ]
      }
      
      # Create grouping key (single or compound)
      if (length(grouping_cols) == 1) {
        data$._grp_key <- as.character(data[[grouping_cols[1]]])
      } else {
        data$._grp_key <- do.call(paste, c(data[grouping_cols], sep = "_"))
      }
      
      groups <- unique(data$._grp_key)
      group_data_list <- lapply(groups, function(g) {
        data[data$._grp_key == g, ]
      })
      group_names <- groups
      
    } else {
      data$._grp_key <- "all"
      group_data_list <- list(data)
      group_names <- "all"
    }
    
    # ---- Emit content pages with data-group (no page-break, no blanks) ----
    # JS will add page-break classes and blank separators after render.
    for (grp_idx in seq_along(group_data_list)) {
      grp_data <- group_data_list[[grp_idx]]
      grp_label <- htmltools::htmlEscape(group_names[grp_idx])
      
      num_pages <- ceiling(nrow(grp_data) / cards_per_page)
      
      for (page in seq_len(num_pages)) {
        start_idx <- (page - 1) * cards_per_page + 1
        end_idx <- min(page * cards_per_page, nrow(grp_data))
        
        html_parts[[part_idx]] <- paste0(
          '<div class="card-page" data-group="', grp_label, '">\n')
        part_idx <- part_idx + 1
        
        for (i in seq(start_idx, end_idx)) {
          html_parts[[part_idx]] <- build_card(grp_data[i, ])
          part_idx <- part_idx + 1
          cards_done <- cards_done + 1
          report_progress(cards_done)
        }
        
        html_parts[[part_idx]] <- '</div>\n'
        part_idx <- part_idx + 1
      }
    }
    
  } else {
    # Normal mode: group cards into pages without section splits
    num_pages <- ceiling(nrow(data) / cards_per_page)
    
    for (page in seq_len(num_pages)) {
      start_idx <- (page - 1) * cards_per_page + 1
      end_idx <- min(page * cards_per_page, nrow(data))
      
      html_parts[[part_idx]] <- paste0('<div class="card-page" data-group="all">\n')
      part_idx <- part_idx + 1
      
      for (i in seq(start_idx, end_idx)) {
        html_parts[[part_idx]] <- build_card(data[i, ])
        part_idx <- part_idx + 1
        cards_done <- cards_done + 1
        report_progress(cards_done)
      }
      
      html_parts[[part_idx]] <- '</div>\n'
      part_idx <- part_idx + 1
    }
  }
  
  html_parts[[part_idx]] <- '</div>'
  
  # Single collapse at the end (much faster than incremental paste0)
  return(paste0(html_parts, collapse = ""))
}

#' Generate HTML for a single card
#' 
#' Helper function to generate the HTML for one section card
#' 
#' @param row Single row data frame with site information
#' @param title_fields Vector of field names for title section
#' @param table_fields Vector of field names for data table
#' @param num_rows Number of empty rows in table
#' @param field_labels Named list of field display labels
#' @param facility_map Named vector mapping facility codes to names
#' @param fos_map Named vector mapping FOS codes to names
#' @return HTML string for one card
generate_card_html <- function(row, title_fields, table_fields, num_rows, 
                               field_labels, facility_map, fos_map,
                               prebuilt_table_header = NULL, prebuilt_table_footer = NULL,
                               watermark_fields = NULL) {
  html <- ''
  
  # Track if we need prehatch overlay (computed at end)
  prehatch_overlay_value <- NULL
  
  # Card header with title and selected info fields
  html <- paste0(html, '  <div class="section-card">\n')
  html <- paste0(html, '    <div class="card-header">\n')
  html <- paste0(html, '      <div class="card-title">', row$sitecode, '</div>\n')
  html <- paste0(html, '      <div class="card-info">\n')
  
  for (field in title_fields) {
    if (field != "sitecode") {
      # Special handling for computed fields that don't exist in the row data
      if (field == "prehatch_calc") {
        # Prehatch calculation - calculate value for overlay display
        prehatch_val <- row[["prehatch"]]
        acres_val <- row[["acres"]]
        if (!is.na(prehatch_val) && prehatch_val != "" && 
            !is.na(acres_val) && !is.na(as.numeric(acres_val))) {
          calculation <- round(as.numeric(acres_val) * 2.5, 2)
          # Ensure minimum of 0.05
          calculation <- max(calculation, 0.05)
          prehatch_overlay_value <- sprintf("%.2f", calculation)
        }
        # Don't add to header - will be displayed as overlay at bottom
      } else if (field %in% names(row)) {
        # Handle regular database fields
        value <- if(is.na(row[[field]])) "" else as.character(row[[field]])
      
      # Map facility code to full name
      if (field == "facility" && value %in% names(facility_map)) {
        value <- facility_map[value]
      }
      
      # Map FOS code to name
      if (field == "fosarea" && value %in% names(fos_map)) {
        value <- fos_map[value]
      }
      
      # Map foreman code to name
      if (field == "foreman" && value %in% names(fos_map)) {
        value <- fos_map[value]
      }
      
      # Get label with fallback for dynamic columns
      label <- if (field %in% names(field_labels)) field_labels[[field]] else humanize_column_name(field)
      
      # Special handling for culex and spr_aedes - only show if Y or not null/empty
      if (field == "culex") {
        if (!is.na(row[[field]]) && row[[field]] != "" && toupper(row[[field]]) == "Y") {
          # Show culex label with green background
          html <- paste0(html, '        <div class="info-item"><span class="culex-field">',
                        label, '</span></div>\n')
        }
      } else if (field == "spr_aedes") {
        if (!is.na(row[[field]]) && row[[field]] != "" && toupper(row[[field]]) == "Y") {
          # Show spring aedes label with orange background
          html <- paste0(html, '        <div class="info-item"><span class="spring-aedes-field">',
                        label, '</span></div>\n')
        }
        # If not Y, don't show anything
      } else if (field == "perturbans") {
        if (!is.na(row[[field]]) && row[[field]] != "" && toupper(row[[field]]) == "Y") {
          # Show perturbans label with light blue background
          html <- paste0(html, '        <div class="info-item"><span class="perturbans-field">',
                        label, '</span></div>\n')
        }
        # If not Y, don't show anything
      } else if (field == "sample") {
        # Sample site - only show if Y with pink background
        if (!is.na(row[[field]]) && row[[field]] != "" && toupper(row[[field]]) == "Y") {
          html <- paste0(html, '        <div class="info-item"><span class="sample-field">',
                        label, '</span></div>\n')
        }
        # If not Y, don't show anything
      } else if (field == "ra") {
        # Restricted Area - only show if Y with pink background
        if (!is.na(row[[field]]) && row[[field]] != "" && toupper(row[[field]]) == "Y") {
          html <- paste0(html, '        <div class="info-item"><span class="ra-field">',
                        'Restricted</span></div>\n')
        }
        # If not Y, don't show anything
      } else if (field == "prehatch") {
        # Prehatch with blue background
        if (!is.na(value) && value != "") {
          html <- paste0(html, '        <div class="info-item"><span class="prehatch-field">',
                        toupper(value), '</span></div>\n')
        }
      } else if (field == "status_udw") {
        # Structure status (D/W/U) with color coding
        if (!is.na(value) && value != "") {
          status_label <- switch(toupper(value),
            "D" = "Dry",
            "W" = "Wet", 
            "U" = "Unknown",
            value
          )
          status_class <- switch(toupper(value),
            "D" = "status-dry",
            "W" = "status-wet",
            "U" = "status-unknown",
            ""
          )
          html <- paste0(html, '        <div class="info-item"><span class="info-label">',
                        label, ':</span> <span class="', status_class, '">', status_label, '</span></div>\n')
        }
      } else if (field == "s_type") {
        # Structure type - display as uppercase
        if (!is.na(value) && value != "") {
          html <- paste0(html, '        <div class="info-item"><span class="info-label">',
                        label, ':</span> <span class="structure-type">', toupper(value), '</span></div>\n')
        }
      } else if (field == "sqft") {
        # Square feet
        if (!is.na(value) && value != "" && as.numeric(value) > 0) {
          html <- paste0(html, '        <div class="info-item"><span class="info-label">',
                        label, ':</span> ', value, '</div>\n')
        }
      } else if (field == "chambers") {
        # Chambers - only show for PR type structures and if value > 0
        s_type_val <- row[["s_type"]]
        if (!is.na(s_type_val) && grepl("PR", toupper(s_type_val)) && 
            !is.na(value) && value != "" && as.numeric(value) > 0) {
          html <- paste0(html, '        <div class="info-item"><span class="info-label">',
                        label, ':</span> ', value, '</div>\n')
        }
      } else if (field == "priority") {
        # Priority with color coding based on value
        if (!is.na(value) && value != "") {
          priority_class <- paste0("priority-", tolower(value))
          html <- paste0(html, '        <div class="info-item"><span class="', priority_class, '">', toupper(value), '</span></div>\n')
        }
      } else if (field == "remarks") {
        # Remarks get special styling - larger and full width (skip if empty)
        if (!is.na(value) && value != "") {
          html <- paste0(html, '        <div class="info-item remarks"><span class="info-label">',
                        label, ':</span> ', value, '</div>\n')
        }
      } else {
        # Normal display for other fields (skip if empty/NA)
        if (!is.na(value) && value != "") {
          html <- paste0(html, '        <div class="info-item"><span class="info-label">',
                        label, ':</span> ', value, '</div>\n')
        }
      }
      } # Close the else if (field %in% names(row)) block
    }
  }
  
  html <- paste0(html, '      </div>\n')
  html <- paste0(html, '    </div>\n')
  
  # Data table: use pre-built parts if available (optimization)
  if (!is.null(prebuilt_table_header) && !is.null(prebuilt_table_footer)) {
    html <- paste0(html, prebuilt_table_header, prebuilt_table_footer)
  } else {
    # Fallback: build table dynamically
    html <- paste0(html, '    <table class="card-table">\n')
    html <- paste0(html, '      <thead><tr>\n')
    
    for (i in seq_along(table_fields)) {
      field <- table_fields[i]
      label <- if (field %in% names(field_labels)) field_labels[[field]] else humanize_column_name(field)
      col_class <- if (i %% 2 == 1) "col-odd" else "col-even"
      html <- paste0(html, '        <th class="', col_class, '">', label, '</th>\n')
    }
    
    html <- paste0(html, '      </tr></thead>\n')
    html <- paste0(html, '      <tbody>\n')
    
    for (j in 1:num_rows) {
      html <- paste0(html, '        <tr>\n')
      for (i in seq_along(table_fields)) {
        col_class <- if (i %% 2 == 1) "col-odd" else "col-even"
        html <- paste0(html, '          <td class="', col_class, '"></td>\n')
      }
      html <- paste0(html, '        </tr>\n')
    }
    
    html <- paste0(html, '      </tbody>\n')
    html <- paste0(html, '    </table>\n')
  }
  
  # Add watermark overlays at bottom of card (on top of table columns, semi-transparent)
  if (!is.null(watermark_fields) && length(watermark_fields) > 0) {
    wm_items <- character(0)
    
    for (wm_field in watermark_fields) {
      if (wm_field == "culex" && "culex" %in% names(row) &&
          !is.na(row[["culex"]]) && toupper(row[["culex"]]) == "Y") {
        wm_items <- c(wm_items, '<span class="watermark-item watermark-culex">Culex</span>')
      } else if (wm_field == "spr_aedes" && "spr_aedes" %in% names(row) &&
                 !is.na(row[["spr_aedes"]]) && toupper(row[["spr_aedes"]]) == "Y") {
        wm_items <- c(wm_items, '<span class="watermark-item watermark-spring-aedes">Spring Aedes</span>')
      } else if (wm_field == "perturbans" && "perturbans" %in% names(row) &&
                 !is.na(row[["perturbans"]]) && toupper(row[["perturbans"]]) == "Y") {
        wm_items <- c(wm_items, '<span class="watermark-item watermark-perturbans">Perturbans</span>')
      } else if (wm_field == "prehatch_calc") {
        prehatch_val <- row[["prehatch"]]
        acres_val <- row[["acres"]]
        if (!is.na(prehatch_val) && prehatch_val != "" &&
            !is.na(acres_val) && !is.na(as.numeric(acres_val))) {
          calc_val <- max(round(as.numeric(acres_val) * 2.5, 2), 0.05)
          wm_items <- c(wm_items, paste0('<span class="watermark-item watermark-prehatch-calc">2.5/ac<br>',
                                          sprintf("%.2f", calc_val), '</span>'))
        }
      } else if (wm_field == "sample" && "sample" %in% names(row) &&
                 !is.na(row[["sample"]]) && toupper(row[["sample"]]) == "Y") {
        wm_items <- c(wm_items, '<span class="watermark-item watermark-sample">Sample Site</span>')
      }
    }
    
    if (length(wm_items) > 0) {
      html <- paste0(html, '    <div class="watermark-container">\n      ',
                     paste0(wm_items, collapse = '\n      '), '\n    </div>\n')
    }
  }
  
  # Add prehatch overlay if calculated (legacy - used when prehatch_calc is in title_fields)
  if (!is.null(prehatch_overlay_value)) {
    html <- paste0(html, '    <div class="prehatch-overlay">\n')
    html <- paste0(html, '      <div class="prehatch-rate">2.5/ac</div>\n')
    html <- paste0(html, '      <div class="prehatch-value">', prehatch_overlay_value, '</div>\n')
    html <- paste0(html, '    </div>\n')
  }
  
  html <- paste0(html, '  </div>\n')
  
  return(html)
}
