# Inspections App - Display Functions

library(DT)
library(dplyr)

# Format inspection gaps data for display
format_inspection_gaps <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  # Format the data for display
  data %>%
    mutate(
      `Site Code` = sitecode,
      `Facility` = facility,
      `FOS Area` = fosarea,
      `Zone` = zone,
      `Air/Ground` = air_gnd,
      `Priority` = priority,
      `Last Inspection` = case_when(
        last_inspection_date == as.Date('1900-01-01') ~ "Never",
        is.na(last_inspection_date) ~ "Never",
        TRUE ~ format(last_inspection_date, "%Y-%m-%d")
      ),
      `Num Dips` = case_when(
        is.na(last_numdip) ~ "-",
        TRUE ~ as.character(last_numdip)
      ),
      `Days Since` = case_when(
        days_since_inspection == 999999 ~ "Never",
        TRUE ~ paste(format(days_since_inspection, big.mark=","), "days")
      ),
      `Status` = inspection_status
    ) %>%
    select(`Site Code`, `Facility`, `FOS Area`, `Zone`, `Air/Ground`, `Priority`, 
           `Last Inspection`, `Num Dips`, `Days Since`, `Status`)
}

# Render the inspection gap table
render_gap_table <- function(data) {
  formatted_data <- format_inspection_gaps(data)
  
  if (nrow(formatted_data) == 0) {
    return(DT::datatable(data.frame(Message = "No sites found with inspection gaps"), 
                        rownames = FALSE, options = list(dom = 't')))
  }
  
  DT::datatable(
    formatted_data,
    rownames = FALSE,
    options = list(
      pageLength = 25, 
      autoWidth = TRUE, 
      scrollX = TRUE,
      order = list(list(8, 'desc'))  # Sort by Days Since descending
    ),
    filter = 'top',
    class = 'compact stripe hover'
  ) %>%
  DT::formatStyle(
    'Status',
    backgroundColor = DT::styleEqual(
      c('Never Inspected', 'Inspection Gap'),
      c('#ffebee', '#fff3e0')
    )
  )
}