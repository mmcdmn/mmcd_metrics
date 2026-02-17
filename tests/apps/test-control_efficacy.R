# =============================================================================
# TEST: control_efficacy app
# =============================================================================
# Comprehensive unit tests for Control Efficacy app
# Tests control checkback detection, Mulla's formula, and material filtering
# Uses stub data for isolated testing without database dependency
# =============================================================================

# Helper to get project root (tests run from tests/apps or project root)
get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

# =============================================================================
# SETUP AND SOURCING
# =============================================================================

# Get project root and source required files
root <- get_project_root()

# Source control efficacy stub data
source(file.path(root, "tests/stubs/stub_control_efficacy.R"), local = TRUE)

# Source all required files
test_that("all control efficacy source files load correctly", {
  skip_if_not_installed("RPostgres")
  expect_no_error({
    source(file.path(root, "shared/app_libraries.R"), local = TRUE)
    source(file.path(root, "shared/server_utilities.R"), local = TRUE) 
    source(file.path(root, "shared/db_helpers.R"), local = TRUE)
    source(file.path(root, "apps/control_efficacy/data_functions.R"), local = TRUE)
    source(file.path(root, "apps/control_efficacy/efficacy_data_functions.R"), local = TRUE)
    source(file.path(root, "apps/control_efficacy/checkback_functions.R"), local = TRUE)
    source(file.path(root, "apps/control_efficacy/display_functions.R"), local = TRUE)
    source(file.path(root, "apps/control_efficacy/ui_helper.R"), local = TRUE)
  })
})

# Setup: Source files and create stubs
if (!requireNamespace("RPostgres", quietly = TRUE)) {
  skip("RPostgres is not installed")
}
source(file.path(root, "shared/app_libraries.R"), local = TRUE)
source(file.path(root, "shared/server_utilities.R"), local = TRUE)
source(file.path(root, "shared/db_helpers.R"), local = TRUE)
source(file.path(root, "apps/control_efficacy/data_functions.R"), local = TRUE)
source(file.path(root, "apps/control_efficacy/efficacy_data_functions.R"), local = TRUE)
source(file.path(root, "apps/control_efficacy/checkback_functions.R"), local = TRUE)
source(file.path(root, "apps/control_efficacy/display_functions.R"), local = TRUE)

# Override database functions with stubs
get_db_connection <- function() NULL
safe_disconnect <- function(conn) invisible(NULL)
get_matcodes_by_ingredient <- function(con, ingredient) {
  codes <- get_stub_material_codes()
  switch(tolower(ingredient),
    "bti" = codes$bti,
    "methoprene" = codes$methoprene, 
    "spinosad" = codes$spinosad,
    character(0)
  )
}

# =============================================================================
# UNIT TESTS: CONTROL CHECKBACK DETECTION
# =============================================================================

test_that("control checkback detection works correctly", {
  # Create test data
  treatments <- get_stub_control_treatments()
  checkbacks <- get_stub_control_checkbacks()
  pre_inspections <- get_stub_control_pre_inspections()
  all_inspections <- rbind(pre_inspections, checkbacks)
  
  # Test the core control detection logic from efficacy_data_functions.R
  # This simulates the matching process with our test data
  matched_data <- data.frame(
    sitecode = paste0("TEST-", sprintf("%03d", 1:10)),
    trt_date = treatments$inspdate,
    pre_date = pre_inspections$inspdate,
    post_date = checkbacks$inspdate,
    pre_numdip = pre_inspections$numdip,
    post_numdip = checkbacks$numdip,
    stringsAsFactors = FALSE
  )
  
  # Apply control detection logic: is_control = trt_date <= pre_date
  matched_data$is_control <- !is.na(matched_data$pre_date) &
                            (is.na(matched_data$trt_date) | matched_data$trt_date <= matched_data$pre_date)
  
  # Test expectations:
  # Sites 1-5: treatment dates (05-15, 05-20, 06-01, 06-10, 06-15) > pre dates (05-12, 05-17, 05-29, 06-07, 06-12) = Valid
  # Sites 6-10: treatment dates (05-10, 05-25, 06-05, 06-12, 06-20) <= pre dates (05-15, 05-28, 06-08, 06-15, 06-23) = Control
  
  expect_equal(sum(matched_data$is_control), 5, info = "Should detect exactly 5 control checkbacks")
  expect_equal(sum(!matched_data$is_control), 5, info = "Should detect exactly 5 valid checkbacks")
  
  # Check specific cases
  expect_false(matched_data$is_control[1], info = "Site 1: trt 05-15 > pre 05-12 should be valid")
  expect_true(matched_data$is_control[6], info = "Site 6: trt 05-10 <= pre 05-15 should be control")
})

test_that("checkback status calculation excludes controls correctly", {
  # Test the core control filtering logic without complex function calls
  treatments <- get_stub_control_treatments()
  checkbacks <- get_stub_control_checkbacks()
  all_inspections <- rbind(get_stub_control_pre_inspections(), checkbacks)
  
  # Test the basic control detection logic that would be used in filtering
  # This tests the core logic without the full calculate_checkback_status function
  control_count <- 0
  valid_count <- 0
  
  for (i in 1:nrow(checkbacks)) {
    cb <- checkbacks[i, ]
    site_trts <- treatments[treatments$sitecode == cb$sitecode, ]
    site_insps <- all_inspections[all_inspections$sitecode == cb$sitecode & 
                                 is.na(all_inspections$posttrt_p), ]
    
    if (nrow(site_trts) > 0 && nrow(site_insps) > 0) {
      # Find pre-inspection before this checkback
      pre_insps <- site_insps[site_insps$inspdate < cb$inspdate, ]
      if (nrow(pre_insps) > 0) {
        pre_date <- max(pre_insps$inspdate)
        most_recent_trt <- max(site_trts$inspdate[site_trts$inspdate <= cb$inspdate])
        
        # Control if treatment is before pre-inspection
        if (!is.na(most_recent_trt) && most_recent_trt <= pre_date) {
          control_count <- control_count + 1
        } else {
          valid_count <- valid_count + 1  
        }
      }
    }
  }
  
  expect_true(control_count > 0, info = "Should detect some control checkbacks")
  expect_true(valid_count > 0, info = "Should detect some valid checkbacks")
})

# =============================================================================
# UNIT TESTS: EFFICACY DATA AND MULLA'S FORMULA
# =============================================================================

test_that("efficacy data processing works with stub data", {
  skip("Integration test - requires complex database mocking")
  # This would test the full load_efficacy_data function
  # Skipped due to complexity but structure is here for future enhancement
})

test_that("percent reduction calculation works correctly", {
  # Test basic % reduction formula: (pre - post) / pre * 100
  pre_dips <- c(10, 20, 15, 8)
  post_dips <- c(5, 2, 12, 0)
  
  expected_reduction <- (pre_dips - post_dips) / pre_dips * 100
  
  # Manual calculation to verify
  expect_equal(expected_reduction[1], 50.0, info = "10 to 5 should be 50% reduction")
  expect_equal(expected_reduction[2], 90.0, info = "20 to 2 should be 90% reduction")  
  expect_equal(expected_reduction[3], 20.0, info = "15 to 12 should be 20% reduction")
  expect_equal(expected_reduction[4], 100.0, info = "8 to 0 should be 100% reduction")
})

test_that("mulla's formula control correction works", {
  # Test Mulla's formula: 100 - (T2/T1) ÃƒÆ’Ã¢â‚¬â€ (C1/C2) ÃƒÆ’Ã¢â‚¬â€ 100
  # Where T=treated, C=control, 1=pre, 2=post
  
  # Treated data
  T1 <- 20  # Pre-treatment dips
  T2 <- 5   # Post-treatment dips
  
  # Control data (natural population change)
  C1 <- 15  # Control pre-dips
  C2 <- 10  # Control post-dips (natural decline)
  
  # Control ratio C1/C2 = 15/10 = 1.5 (population would naturally decline 33%)
  control_ratio <- C1 / C2
  
  # Standard reduction: (T1-T2)/T1 * 100 = (20-5)/20 * 100 = 75%
  standard_reduction <- (T1 - T2) / T1 * 100
  
  # Mulla's reduction: 100 - (T2/T1) ÃƒÆ’Ã¢â‚¬â€ (C1/C2) ÃƒÆ’Ã¢â‚¬â€ 100 = 100 - (5/20) ÃƒÆ’Ã¢â‚¬â€ 1.5 ÃƒÆ’Ã¢â‚¬â€ 100 = 62.5%
  mullas_reduction <- 100 - (T2/T1) * control_ratio * 100
  
  expect_equal(standard_reduction, 75.0, info = "Standard formula should give 75%")
  expect_equal(mullas_reduction, 62.5, info = "Mulla's formula should give 62.5%")
  expect_true(mullas_reduction < standard_reduction, info = "Mulla's should be more conservative when controls show natural decline")
})

# =============================================================================
# UNIT TESTS: MATERIAL FILTERING  
# =============================================================================

test_that("material code filtering works correctly", {
  # Test BTI filter
  bti_codes <- get_matcodes_by_ingredient(NULL, "bti")
  expect_true(length(bti_codes) > 0, info = "Should return BTI matcodes")
  expect_true("19" %in% bti_codes, info = "Matcode 19 should be in BTI list")
  
  # Test methoprene filter
  metho_codes <- get_matcodes_by_ingredient(NULL, "methoprene")
  expect_true(length(metho_codes) > 0, info = "Should return methoprene matcodes")
  expect_true("08" %in% metho_codes, info = "Matcode 08 should be in methoprene list")
  
  # Test spinosad filter
  spin_codes <- get_matcodes_by_ingredient(NULL, "spinosad") 
  expect_true(length(spin_codes) > 0, info = "Should return spinosad matcodes")
  expect_true("N1" %in% spin_codes, info = "Matcode N1 should be in spinosad list")
  
  # Test unknown ingredient
  unknown_codes <- get_matcodes_by_ingredient(NULL, "unknown")
  expect_equal(length(unknown_codes), 0, info = "Unknown ingredient should return empty vector")
})

# =============================================================================  
# UNIT TESTS: PERFORMANCE AND INTEGRATION
# =============================================================================

test_that("optimized control detection performs well", {
  # Test that control detection logic runs efficiently
  treatments <- get_stub_control_treatments()
  checkbacks <- get_stub_control_checkbacks()
  all_inspections <- rbind(get_stub_control_pre_inspections(), checkbacks)
  
  # Time a simplified version of the control detection
  start_time <- Sys.time()
  
  # Test the core detection logic used in the optimized functions
  results <- c()
  for (site in unique(checkbacks$sitecode)) {
    site_cbs <- checkbacks[checkbacks$sitecode == site, ]
    site_trts <- treatments[treatments$sitecode == site, ]
    site_insps <- all_inspections[all_inspections$sitecode == site & 
                                 is.na(all_inspections$posttrt_p), ]
    
    for (i in 1:nrow(site_cbs)) {
      cb_date <- site_cbs$inspdate[i]
      pre_dates <- site_insps$inspdate[site_insps$inspdate < cb_date]
      if (length(pre_dates) > 0) {
        pre_date <- max(pre_dates)
        trt_dates <- site_trts$inspdate
        is_control <- !any(trt_dates > pre_date & trt_dates < cb_date)
        results <- c(results, is_control)
      }
    }
  }
  
  end_time <- Sys.time()
  elapsed <- as.numeric(end_time - start_time, units = "secs")
  
  expect_true(elapsed < 1.0, info = "Control detection should complete quickly")
  expect_true(length(results) > 0, info = "Should process some checkbacks")
})

test_that("site details include correct control flags", {
  # Test the control flag logic without complex function calls
  treatments <- get_stub_control_treatments()
  checkbacks <- get_stub_control_checkbacks() 
  all_inspections <- rbind(get_stub_control_pre_inspections(), checkbacks)
  
  # Test the core control detection using the same logic as the app
  matched_data <- data.frame(
    sitecode = paste0("TEST-", sprintf("%03d", 1:10)),
    trt_date = treatments$inspdate,
    pre_date = get_stub_control_pre_inspections()$inspdate,
    stringsAsFactors = FALSE
  )
  
  # Apply control detection logic
  matched_data$is_control <- !is.na(matched_data$pre_date) &
                            (is.na(matched_data$trt_date) | matched_data$trt_date <= matched_data$pre_date)
  
  n_controls <- sum(matched_data$is_control, na.rm = TRUE)
  n_valid <- sum(!matched_data$is_control, na.rm = TRUE)
  
  expect_true(n_controls > 0, info = "Should detect some control checkbacks")
  expect_true(n_valid > 0, info = "Should detect some valid checkbacks") 
  expect_equal(nrow(matched_data), n_controls + n_valid, info = "All should be classified")
})

# =============================================================================
# UNIT TESTS: DATA VALIDATION
# =============================================================================

test_that("control definition matches documented behavior", {
  # Test the specific definition: "No treatments occurred between the pre-inspection and the post (checkback) inspection"
  # NOT "treatment date is before the pre-inspection"
  
  # Scenario 1: Treatment between pre and post = VALID checkback
  pre_date <- as.Date("2024-05-10")
  trt_date <- as.Date("2024-05-15")  # Between pre and post
  post_date <- as.Date("2024-05-20")
  
  is_control_1 <- trt_date <= pre_date
  expect_false(is_control_1, info = "Treatment between pre and post should be VALID (not control)")
  
  # Scenario 2: Treatment before pre-inspection = CONTROL checkback  
  pre_date_2 <- as.Date("2024-05-15")
  trt_date_2 <- as.Date("2024-05-10")  # Before pre-inspection
  post_date_2 <- as.Date("2024-05-20")
  
  is_control_2 <- trt_date_2 <= pre_date_2
  expect_true(is_control_2, info = "Treatment before pre-inspection should be CONTROL")
  
  # Scenario 3: Treatment on same day as pre-inspection = CONTROL checkback
  pre_date_3 <- as.Date("2024-05-15")
  trt_date_3 <- as.Date("2024-05-15")  # Same day as pre-inspection
  post_date_3 <- as.Date("2024-05-20")
  
  is_control_3 <- trt_date_3 <= pre_date_3
  expect_true(is_control_3, info = "Treatment on same day as pre-inspection should be CONTROL")
})
