# =============================================================================
# TEST: ground_prehatch_progress app
# =============================================================================
# Tests for Ground Prehatch Progress app data and display functions
# Uses stub data from tests/stubs/ based on real database schema
# =============================================================================

# Helper to get project root (tests run from tests/apps or project root)
get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

test_that("apply_data_filters works with facility filter", {
  root <- get_project_root()
  source(file.path(root, "apps/ground_prehatch_progress/data_functions.R"), local = TRUE)
  
  # Create sample data similar to what load_raw_data returns (STANDARDIZED format)
  test_data <- list(
    sites = data.frame(
      sitecode = c("020207-001", "700407-010", "021335-005"),
      facility = c("N", "Sj", "N"),
      acres = c(0.50, 0.25, 1.20),
      prehatch = c("PREHATCH", "PREHATCH", "BRIQUET"),
      fosarea = c("0204", "7002", "0206"),
      zone = c("1", "1", "2"),
      stringsAsFactors = FALSE
    ),
    treatments = data.frame(
      sitecode = c("020207-001", "700407-010"),
      facility = c("N", "Sj"),
      inspdate = as.Date(c("2025-04-16", "2025-05-07")),
      acres = c(0.50, 0.25),
      fosarea = c("0204", "7002"),
      zone = c("1", "1"),
      stringsAsFactors = FALSE
    ),
    total_count = 3
  )
  
  # Test filtering by facility
  result <- apply_data_filters(test_data, facility_filter = "N")
  expect_equal(nrow(result$sites), 2)
  expect_true(all(result$sites$facility == "N"))
  
  # Test filtering by zone
  result <- apply_data_filters(test_data, zone_filter = "1")
  expect_equal(nrow(result$sites), 2)
  expect_true(all(result$sites$zone == "1"))
})

test_that("apply_data_filters handles NULL filters correctly", {
  root <- get_project_root()
  source(file.path(root, "apps/ground_prehatch_progress/data_functions.R"), local = TRUE)
  
  test_data <- list(
    sites = data.frame(
      sitecode = c("020207-001", "700407-010"),
      facility = c("N", "Sj"),
      acres = c(0.50, 0.25),
      prehatch = c("PREHATCH", "PREHATCH"),
      fosarea = c("0204", "7002"),
      zone = c("1", "2"),
      stringsAsFactors = FALSE
    ),
    treatments = data.frame(
      sitecode = c("020207-001"),
      facility = c("N"),
      inspdate = as.Date(c("2025-04-16")),
      acres = c(0.50),
      fosarea = c("0204"),
      zone = c("1"),
      stringsAsFactors = FALSE
    ),
    total_count = 2
  )
  
  # With NULL filters, all data should be returned
  result <- apply_data_filters(test_data, 
                               facility_filter = NULL, 
                               foreman_filter = NULL, 
                               zone_filter = NULL)
  
  expect_equal(nrow(result$sites), 2)
  expect_equal(nrow(result$treatments), 1)
})

test_that("filter_ground_data handles filters", {
  root <- get_project_root()
  source(file.path(root, "apps/ground_prehatch_progress/data_functions.R"), local = TRUE)
  
  test_data <- data.frame(
    sitecode = c("020207-001", "700407-010", "021335-005"),
    facility = c("N", "Sj", "N"),
    zone = c("1", "1", "2"),
    fosarea = c("0204", "7002", "0206"),
    total_count = c(10, 20, 15),
    stringsAsFactors = FALSE
  )
  
  # Test zone filter
  result <- filter_ground_data(test_data, zone_filter = "1")
  expect_equal(nrow(result), 2)
  
  # Test facility filter
  result <- filter_ground_data(test_data, facility_filter = "N")
  expect_equal(nrow(result), 2)
})

test_that("display_functions can be sourced without errors", {
  skip_if_not_installed("leaflet")
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/ground_prehatch_progress/display_functions.R"), local = TRUE)
  })
})

test_that("ui_helper can be sourced without errors", {
  root <- get_project_root()
  expect_no_error({
    source(file.path(root, "apps/ground_prehatch_progress/ui_helper.R"), local = TRUE)
  })
})

# =============================================================================
# REGRESSION TESTS: Prevent data column errors
# =============================================================================

test_that("data_functions can be sourced without syntax errors", {
  root <- get_project_root()
  
  # Just check that the file can be parsed without syntax errors
  # Skip actual execution which requires database packages
  expect_no_error({
    parse(file.path(root, "apps/ground_prehatch_progress/data_functions.R"))
  })
})

test_that("get_ground_prehatch_data returns valid structure", {
  root <- get_project_root()
  source(file.path(root, "apps/ground_prehatch_progress/data_functions.R"), local = TRUE)
  
  # Mock the load_raw_data function to avoid database dependency
  mock_load_raw_data <- function(...) {
    list(
      sites = data.frame(
        sitecode = c("020207-001", "700407-010"),
        sectcode = c("0202070", "7004070"),
        facility = c("N", "Sj"),
        zone = c("1", "1"),
        fosarea = c("0204", "7002"),
        acres = c(0.50, 0.25),
        prehatch = c("PREHATCH", "PREHATCH"),
        stringsAsFactors = FALSE
      ),
      treatments = data.frame(
        sitecode = c("020207-001"),
        inspdate = as.Date("2025-04-16"),
        matcode = c("BT"),
        insptime = c("0800"),
        effect_days = c(30),
        stringsAsFactors = FALSE
      ),
      total_count = 2
    )
  }
  
  # Replace the function temporarily
  original_load_raw_data <- load_raw_data
  assign("load_raw_data", mock_load_raw_data, envir = environment())
  
  # Test the function
  result <- get_ground_prehatch_data(zone_filter = c("1", "2"))
  
  # Restore original function
  assign("load_raw_data", original_load_raw_data, envir = environment())
  
  # Check structure
  expect_true(is.data.frame(result))
  if (nrow(result) > 0) {
    expect_true("facility" %in% names(result))
    expect_true("zone" %in% names(result))
    expect_true("prehatch_sites_cnt" %in% names(result))
    expect_true("ph_treated_cnt" %in% names(result))
    expect_true("total_count" %in% names(result))
  }
})

test_that("prehatch status calculation does not reference non-existent columns", {
  root <- get_project_root()
  source(file.path(root, "apps/ground_prehatch_progress/data_functions.R"), local = TRUE)
  
  # Create test treatment data with only valid columns
  treatment_data <- data.frame(
    sitecode = "020207-001",
    inspdate = as.Date("2025-04-16"),
    matcode = "BT",
    insptime = "0800",
    effect_days = 30,
    stringsAsFactors = FALSE
  )
  
  # Test that the prehatch status calculation works with available columns only
  expect_no_error({
    result <- treatment_data %>%
      mutate(
        age = as.numeric(Sys.Date() - inspdate),
        end_of_year = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-30")),
        days_until_eoy = as.numeric(end_of_year - Sys.Date()),
        prehatch_status = case_when(
          age > effect_days ~ "expired",
          age > (effect_days - 14) ~ "expiring", 
          age <= effect_days ~ "treated",
          TRUE ~ "unknown"
        )
      )
  })
  
  # Verify status is calculated correctly
  result <- treatment_data %>%
    mutate(
      age = as.numeric(Sys.Date() - inspdate),
      prehatch_status = case_when(
        age > effect_days ~ "expired",
        age > (effect_days - 14) ~ "expiring", 
        age <= effect_days ~ "treated",
        TRUE ~ "unknown"
      )
    )
  
  expect_true(result$prehatch_status %in% c("treated", "expiring", "expired", "unknown"))
})

test_that("functions do not reference inspection-specific columns", {
  root <- get_project_root()
  
  # Read the data_functions.R file content
  file_content <- readLines(file.path(root, "apps/ground_prehatch_progress/data_functions.R"))
  
  # Check that inspection-specific columns are not referenced
  # These columns should NOT appear in ground prehatch code
  forbidden_columns <- c(
    "last_inspection_date",
    "inspection_action", 
    "inspection_wet",
    "numdip",
    "reinspect"
  )
  
  for (col in forbidden_columns) {
    matches <- grep(col, file_content, value = TRUE)
    # Filter out comment lines and expect no remaining references
    code_matches <- matches[!grepl("^\\s*#", matches)]
    expect_length(code_matches, 0)
    
    # If there are matches, provide detailed info
    if (length(code_matches) > 0) {
      cat("Found forbidden column", col, "in ground prehatch code:\n")
      cat(paste(code_matches, collapse = "\n"))
      cat("\n")
    }
  }
})

test_that("aggregate_data_by_group handles empty data gracefully", {
  root <- get_project_root()
  source(file.path(root, "apps/ground_prehatch_progress/data_functions.R"), local = TRUE)
  
  # Test with empty data frame
  empty_data <- data.frame()
  result <- aggregate_data_by_group(empty_data, group_by = "facility")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
  
  # Test with data frame with correct structure but no rows
  structured_empty <- data.frame(
    facility = character(0),
    zone = character(0),
    prehatch_sites_cnt = numeric(0),
    ph_treated_cnt = numeric(0),
    stringsAsFactors = FALSE
  )
  
  expect_no_error({
    result <- aggregate_data_by_group(structured_empty, group_by = "facility")
  })
})

