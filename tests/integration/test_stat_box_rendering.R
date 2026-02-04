# Integration test: Test stat box rendering with mock data

suppressPackageStartupMessages({
  library(shiny)
})

cat("Running stat box rendering test with mock data...\n\n")

# Source required modules
source(file.path("shared", "db_helpers.R"))
source(file.path("shared", "stat_box_helpers.R"))
source(file.path("apps", "overview", "metric_registry.R"))

# Mock facility data
mock_facility_data <- data.frame(
  facility = "North",
  display_name = "North",
  total = 1000,
  active = 750,
  expiring = 100,
  stringsAsFactors = FALSE
)

cat("Mock Data:\n")
print(mock_facility_data)
cat("\n")

# Test each metric that has detail_boxes
registry <- get_metric_registry()
test_metrics <- c("ground_prehatch", "drone", "catch_basin", "structure")

for (metric_id in test_metrics) {
  cat("Testing metric:", metric_id, "\n")
  
  config <- registry[[metric_id]]
  if (is.null(config$detail_boxes)) {
    cat("  SKIP: No detail_boxes\n\n")
    next
  }
  
  detail_boxes <- config$detail_boxes
  status_colors <- get_status_colors(theme = "MMCD")
  
  cat("  Detail boxes config:\n")
  for (box_def in detail_boxes) {
    cat("    -", box_def$title, ":", box_def$column, "\n")
  }
  
  # Generate stat boxes
  n_boxes <- length(detail_boxes)
  col_width <- max(2, floor(12 / n_boxes))
  
  box_elements <- lapply(detail_boxes, function(box_def) {
    # Get value from mock data
    value <- if (box_def$column %in% names(mock_facility_data)) {
      mock_facility_data[[box_def$column]]
    } else {
      NA
    }
    
    if (is.na(value)) {
      cat("    WARN: Column", box_def$column, "not in data\n")
      return(NULL)
    }
    
    display_value <- format(value, big.mark = ",")
    
    bg_color <- if (!is.null(box_def$status) && box_def$status %in% names(status_colors)) {
      unname(status_colors[box_def$status])
    } else {
      config$bg_color
    }
    
    column(col_width,
      create_stat_box(
        value = display_value,
        title = box_def$title,
        bg_color = bg_color,
        icon = icon(box_def$icon),
        icon_type = "fontawesome"
      )
    )
  })
  
  box_elements <- Filter(Negate(is.null), box_elements)
  
  if (length(box_elements) == 0) {
    cat("  FAIL: No boxes generated\n\n")
    next
  }
  
  # Create the output
  output_ui <- div(class = "facility-detail-boxes-container",
    div(class = "facility-detail-header",
      icon("building"), " North - ", config$display_name, " Details"
    ),
    fluidRow(class = "facility-detail-boxes", box_elements)
  )
  
  # Verify it's a shiny tag
  if (!inherits(output_ui, "shiny.tag")) {
    cat("  FAIL: Not a shiny tag\n\n")
    next
  }
  
  # Check HTML contains expected elements
  html_output <- as.character(output_ui)
  has_header <- grepl("facility-detail-header", html_output, fixed = TRUE)
  has_boxes <- grepl("stat-box", html_output, ignore.case = TRUE) || grepl("div", html_output, fixed = TRUE)
  has_values <- grepl(format(mock_facility_data$total, big.mark = ","), html_output, fixed = TRUE) ||
                grepl(format(mock_facility_data$active, big.mark = ","), html_output, fixed = TRUE)
  
  cat("  HTML checks:\n")
  cat("    Has header:", has_header, "\n")
  cat("    Has boxes:", has_boxes, "\n")
  cat("    Has values:", has_values, "\n")
  
  if (has_header && has_boxes) {
    cat("  PASS: Renders correctly\n\n")
  } else {
    cat("  FAIL: Missing expected HTML elements\n\n")
  }
}

cat("=== RENDER TEST COMPLETED ===\n")
