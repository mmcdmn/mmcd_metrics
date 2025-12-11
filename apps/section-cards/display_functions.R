# display_functions.R
# Display and visualization functions for sections-cards DEMO

#' Generate HTML for section cards with printable layout
#' 
#' Creates a printable layout of section cards (6 per page)
#' Each card has a title section and a data table
#' 
#' @param data Data frame with breeding site information
#' @param title_fields Vector of field names to display in title section
#' @param table_fields Vector of field names to display in table
#' @param num_rows Number of empty rows to include in each card table
#' @return HTML string with printable section cards
generate_section_cards_html <- function(data, title_fields, table_fields, num_rows = 5) {
  
  # Define field labels for display
  field_labels <- list(
    sitecode = "Site Code",
    priority = "Priority",
    acres = "Acres",
    type = "Type",
    air_gnd = "Air/Gnd",
    culex = "Culex",
    spr_aedes = "Spring Aedes",
    prehatch = "Prehatch",
    remarks = "Remarks",
    drone = "Drone",
    section = "Section",
    zone = "Zone",
    facility = "Facility",
    fosarea = "FOS Area",
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
    
    .card-table td {
      border: 1px solid #333;
      padding: 4px;
      height: 25px;
      font-size: 10px;
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
  
  # Group cards into pages of 6
  cards_per_page <- 6
  num_pages <- ceiling(nrow(data) / cards_per_page)
  
  for (page in 1:num_pages) {
    start_idx <- (page - 1) * cards_per_page + 1
    end_idx <- min(page * cards_per_page, nrow(data))
    
    html <- paste0(html, '<div class="card-page', 
                   ifelse(page < num_pages, ' page-break', ''), '">\n')
    
    for (i in start_idx:end_idx) {
      row <- data[i, ]
      
      # Card header with title and selected info fields
      html <- paste0(html, '  <div class="section-card">\n')
      html <- paste0(html, '    <div class="card-header">\n')
      html <- paste0(html, '      <div class="card-title">', row$sitecode, '</div>\n')
      html <- paste0(html, '      <div class="card-info">\n')
      
      for (field in title_fields) {
        if (field != "sitecode" && field %in% names(row)) {
          value <- if(is.na(row[[field]])) "" else as.character(row[[field]])
          label <- field_labels[[field]]
          html <- paste0(html, '        <div class="info-item"><span class="info-label">',
                        label, ':</span> ', value, '</div>\n')
        }
      }
      
      html <- paste0(html, '      </div>\n')
      html <- paste0(html, '    </div>\n')
      
      # Data table with empty rows for manual entry
      html <- paste0(html, '    <table class="card-table">\n')
      html <- paste0(html, '      <thead><tr>\n')
      
      for (field in table_fields) {
        label <- field_labels[[field]]
        html <- paste0(html, '        <th>', label, '</th>\n')
      }
      
      html <- paste0(html, '      </tr></thead>\n')
      html <- paste0(html, '      <tbody>\n')
      
      # Add empty rows for data entry
      for (j in 1:num_rows) {
        html <- paste0(html, '        <tr>\n')
        for (field in table_fields) {
          html <- paste0(html, '          <td></td>\n')
        }
        html <- paste0(html, '        </tr>\n')
      }
      
      html <- paste0(html, '      </tbody>\n')
      html <- paste0(html, '    </table>\n')
      html <- paste0(html, '  </div>\n')
    }
    
    html <- paste0(html, '</div>\n')
  }
  
  html <- paste0(html, '</div>')
  
  return(html)
}
