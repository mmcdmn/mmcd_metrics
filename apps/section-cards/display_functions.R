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
generate_section_cards_html <- function(data, title_fields, table_fields, num_rows = 5, split_by_section = FALSE, split_by_priority = FALSE, split_by_type = FALSE, progress_fn = NULL) {
  
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
    ra = "RA",
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
  html <- '
  <style>
    @media print {
      @page {
        size: letter;
        margin: 0.5in;
      }
      .page-break {
        page-break-after: always;
      }
      .no-print {
        display: none !important;
      }
      /* Force colors to print */
      * {
        -webkit-print-color-adjust: exact !important;
        print-color-adjust: exact !important;
        color-adjust: exact !important;
      }
    }
    
    .cards-container {
      width: 100%;
    }
    
    .card-page {
      display: grid;
      grid-template-columns: 1fr 1fr;
      grid-template-rows: repeat(3, auto);
      gap: 15px;
      margin-bottom: 20px;
    }
    
    .section-card {
      border: 2px solid #333;
      page-break-inside: avoid;
      background: white;
      font-family: Arial, sans-serif;
      font-size: 11px;
      position: relative;
    }
    
    .prehatch-overlay {
      position: absolute;
      bottom: 10px;
      right: 10px;
      text-align: right;
      color: red !important;
      font-weight: bold;
      z-index: 10;
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .prehatch-rate {
      color: red !important;
      font-size: 12px;
    }
    .prehatch-value {
      color: red !important;
      font-size: 16px;
      font-weight: bold;
    }

    .card-header {
      background: #f0f0f0;
      border-bottom: 2px solid #333;
      padding: 8px;
    }
    
    .card-title {
      font-size: 16px;
      font-weight: bold;
      margin: 0 0 5px 0;
      color: #000;
    }
    
    .card-info {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(100px, 1fr));
      gap: 5px;
      margin-top: 5px;
    }
    
    .info-item {
      font-size: 10px;
    }
    
    .info-item.remarks {
      font-size: 12px;
      grid-column: 1 / -1;
      margin-top: 3px;
    }
    
    .info-item.special-label {
      font-size: 9px;
      padding: 2px 4px;
      background: #d0d0d0;
      border-radius: 3px;
      display: inline-block;
    }
    
    .priority-green { 
      background-color: #90EE90 !important; 
      background: #90EE90 !important; 
      padding: 2px 6px; 
      border-radius: 3px; 
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .priority-red { 
      background-color: #FFB6C1 !important; 
      background: #FFB6C1 !important; 
      padding: 2px 6px; 
      border-radius: 3px; 
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .priority-yellow { 
      background-color: #FFFFE0 !important; 
      background: #FFFFE0 !important; 
      padding: 2px 6px; 
      border-radius: 3px; 
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .priority-blue { 
      background-color: #ADD8E6 !important; 
      background: #ADD8E6 !important; 
      padding: 2px 6px; 
      border-radius: 3px; 
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .priority-orange { 
      background-color: #FFD580 !important; 
      background: #FFD580 !important; 
      padding: 2px 6px; 
      border-radius: 3px; 
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .priority-purple { 
      background-color: #E6D5FF !important; 
      background: #E6D5FF !important; 
      padding: 2px 6px; 
      border-radius: 3px; 
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    
    .culex-field { 
      background-color: #32CD32 !important; /* Changed to a different green */
      background: #32CD32 !important; 
      padding: 2px 6px; 
      border-radius: 3px; 
      font-weight: bold; 
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .sample-field { 
      background-color: #FFB6C1 !important; 
      background: #FFB6C1 !important; 
      padding: 2px 6px; 
      border-radius: 3px; 
      font-weight: bold; 
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .spring-aedes-field { 
      background-color: #FFD580 !important; 
      background: #FFD580 !important; 
      padding: 2px 6px; 
      border-radius: 3px; 
      font-weight: bold; 
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .perturbans-field { 
      background-color: #4169E1 !important; 
      background: #4169E1 !important; 
      padding: 2px 6px; 
      border-radius: 3px; 
      font-weight: bold; 
      color: white !important;
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .prehatch-field { 
      background-color: #DDA0DD !important; 
      background: #DDA0DD !important; 
      padding: 2px 6px; 
      border-radius: 3px; 
      font-weight: bold; 
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .prehatch-overlay {
      position: absolute;
      bottom: 10px;
      right: 10px;
      text-align: right;
      color: red !important;
      font-weight: bold;
      font-size: 14px;
      z-index: 10;
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .prehatch-rate {
      color: red !important;
      font-size: 12px;
    }
    .prehatch-value {
      color: red !important;
      font-size: 16px;
      font-weight: bold;
    }
    
    /* Structure status colors */
    .status-dry {
      background-color: #F4A460 !important;
      background: #F4A460 !important;
      padding: 2px 6px;
      border-radius: 3px;
      font-weight: bold;
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .status-wet {
      background-color: #4682B4 !important;
      background: #4682B4 !important;
      color: white !important;
      padding: 2px 6px;
      border-radius: 3px;
      font-weight: bold;
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .status-unknown {
      background-color: #D3D3D3 !important;
      background: #D3D3D3 !important;
      padding: 2px 6px;
      border-radius: 3px;
      font-weight: bold;
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    .structure-type {
      background-color: #9370DB !important;
      background: #9370DB !important;
      color: white !important;
      padding: 2px 6px;
      border-radius: 3px;
      font-weight: bold;
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
      color-adjust: exact !important;
    }
    
    .info-label {
      font-weight: bold;
      color: #555;
    }
    
    .card-table {
      width: 100%;
      border-collapse: collapse;
    }
    
    .card-table th {
      background: #e0e0e0;
      border: 1px solid #333;
      padding: 4px;
      font-size: 9px;
      font-weight: bold;
      text-align: center;
    }
    
    .card-table th.col-odd {
      background: #f0f0f0;
    }
    
    .card-table th.col-even {
      background: #e8e8e8;
    }
    
    .card-table td {
      border: 1px solid #333;
      padding: 4px;
      height: 25px;
      font-size: 10px;
    }
    
    .card-table td.col-odd {
      background: #fafafa;
    }
    
    .card-table td.col-even {
      background: #f0f0f0;
    }
    
    .page-footer {
      position: fixed;
      bottom: 10px;
      right: 20px;
      font-size: 8px;
      color: #999;
      font-style: italic;
      z-index: 1000;
    }
    
    @media print {
      .page-footer {
        position: fixed;
        bottom: 0.5in;
        right: 0.5in;
        font-size: 8pt;
        color: #999 !important;
        -webkit-print-color-adjust: exact;
        print-color-adjust: exact;
      }
    }
    
    @media screen {
      .cards-container {
        padding: 20px;
        background: #f5f5f5;
      }
      .card-page {
        background: white;
        padding: 20px;
        margin-bottom: 40px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }
    }
  </style>
  
  <div class="cards-container">
  '
  
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
                       prebuilt_table_header, prebuilt_table_footer)
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
    
    # Sort by grouping columns
    if (length(grouping_cols) > 0) {
      data <- data[do.call(order, data[grouping_cols]), ]
      
      # Create grouping combinations
      if (length(grouping_cols) == 1) {
        groups <- unique(data[[grouping_cols[1]]])
        group_data_list <- lapply(groups, function(g) {
          data[data[[grouping_cols[1]]] == g, ]
        })
        group_names <- groups
      } else {
        # Multiple grouping columns - create combination groups
        data$temp_group <- do.call(paste, c(data[grouping_cols], sep = "_"))
        groups <- unique(data$temp_group)
        group_data_list <- lapply(groups, function(g) {
          subset_data <- data[data$temp_group == g, ]
          subset_data$temp_group <- NULL
          subset_data
        })
        group_names <- groups
      }
    } else {
      group_data_list <- list(data)
      group_names <- "all"
    }
    
    # Process each group separately
    for (grp_idx in seq_along(group_data_list)) {
      grp_data <- group_data_list[[grp_idx]]
      
      cards_per_page <- 6
      num_pages <- ceiling(nrow(grp_data) / cards_per_page)
      
      for (page in 1:num_pages) {
        start_idx <- (page - 1) * cards_per_page + 1
        end_idx <- min(page * cards_per_page, nrow(grp_data))
        
        is_last_group <- (grp_idx == length(group_data_list))
        is_last_page <- (page == num_pages)
        add_page_break <- !(is_last_group && is_last_page)
        
        html_parts[[part_idx]] <- paste0('<div class="card-page', 
                       ifelse(add_page_break, ' page-break', ''), '">\n')
        part_idx <- part_idx + 1
        
        for (i in start_idx:end_idx) {
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
    # Normal mode: group cards into pages of 6 without section splits
    cards_per_page <- 6
    num_pages <- ceiling(nrow(data) / cards_per_page)
    
    for (page in 1:num_pages) {
      start_idx <- (page - 1) * cards_per_page + 1
      end_idx <- min(page * cards_per_page, nrow(data))
      
      html_parts[[part_idx]] <- paste0('<div class="card-page', 
                     ifelse(page < num_pages, ' page-break', ''), '">\n')
      part_idx <- part_idx + 1
      
      for (i in start_idx:end_idx) {
        html_parts[[part_idx]] <- build_card(data[i, ])
        part_idx <- part_idx + 1
        cards_done <- cards_done + 1
        report_progress(cards_done)
      }
      
      html_parts[[part_idx]] <- '</div>\n'
      part_idx <- part_idx + 1
    }
  }
  
  html_parts[[part_idx]] <- '</div><div class="page-footer">Section Cards by Alex Dyakin</div>'
  
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
                               prebuilt_table_header = NULL, prebuilt_table_footer = NULL) {
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
      } else if (field == "prehatch") {
        # Prehatch with blue background
        if (!is.na(value) && value != "") {
          html <- paste0(html, '        <div class="info-item"><span class="info-label">',
                        label, ':</span> <span class="prehatch-field">', value, '</span></div>\n')
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
          html <- paste0(html, '        <div class="info-item"><span class="info-label">',
                        label, ':</span> <span class="', priority_class, '">', value, '</span></div>\n')
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
  
  # Add prehatch overlay if calculated
  if (!is.null(prehatch_overlay_value)) {
    html <- paste0(html, '    <div class="prehatch-overlay">\n')
    html <- paste0(html, '      <div class="prehatch-rate">2.5/ac</div>\n')
    html <- paste0(html, '      <div class="prehatch-value">', prehatch_overlay_value, '</div>\n')
    html <- paste0(html, '    </div>\n')
  }
  
  html <- paste0(html, '  </div>\n')
  
  return(html)
}
