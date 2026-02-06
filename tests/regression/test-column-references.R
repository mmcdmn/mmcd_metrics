# =============================================================================
# REGRESSION TEST: Column Reference Validation
# =============================================================================
# Prevents apps from referencing columns that don't exist in their data sources
# This catches issues like ground prehatch apps trying to use inspection columns
# =============================================================================

library(testthat)

# Helper to get project root
get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  if (file.exists("../apps")) return("..")
  stop("Cannot find project root")
}

test_that("ground prehatch does not reference inspection-specific columns", {
  root <- get_project_root()
  
  # Read all R files in ground_prehatch_progress app
  app_path <- file.path(root, "apps/ground_prehatch_progress")
  r_files <- list.files(app_path, pattern = "\\.R$", full.names = TRUE)
  
  # Forbidden columns for ground prehatch (these are inspection-specific)
  forbidden_columns <- c(
    "last_inspection_date",   # Inspection date tracking
    "inspection_action",      # Action codes from inspections  
    "inspection_wet",         # Wet status from inspections
    "numdip",                # Larvae dip counts
    "reinspect",             # Re-inspection flags
    "action.*==.*'9'",       # Inspection action filtering
    "wet.*==.*'0'"           # Dry site inspection logic
  )
  
  for (file_path in r_files) {
    file_content <- readLines(file_path, warn = FALSE)
    file_name <- basename(file_path)
    
    for (col_pattern in forbidden_columns) {
      matches <- grep(col_pattern, file_content, value = TRUE, ignore.case = TRUE)
      
      # Filter out comment lines
      code_matches <- matches[!grepl("^\\s*#", matches)]
      
      if (length(code_matches) > 0) {
        fail(paste0(
          "Found forbidden column/pattern '", col_pattern, "' in ", file_name, ":\n",
          paste(code_matches, collapse = "\n"),
          "\n\nGround prehatch apps should not reference inspection-specific columns."
        ))
      }
    }
  }
  
  # If we get here, all files passed
  expect_true(TRUE, "All ground prehatch files free of forbidden inspection columns")
})

test_that("air sites do not reference ground-specific columns", {
  root <- get_project_root()
  
  # Read air_sites_simple app files  
  app_path <- file.path(root, "apps/air_sites_simple")
  if (!dir.exists(app_path)) {
    skip("air_sites_simple app not found")
  }
  
  r_files <- list.files(app_path, pattern = "\\.R$", full.names = TRUE)
  
  # Forbidden columns for air sites (these are ground-specific)
  forbidden_columns <- c(
    "prehatch_status",        # Ground prehatch treatment status
    "ph_treated_cnt",         # Prehatch treatment counts
    "ph_expiring_cnt",        # Prehatch expiring counts
    "ground_treatments"       # Ground treatment references
  )
  
  for (file_path in r_files) {
    file_content <- readLines(file_path, warn = FALSE)
    file_name <- basename(file_path)
    
    for (col_pattern in forbidden_columns) {
      matches <- grep(col_pattern, file_content, value = TRUE, ignore.case = TRUE)
      
      # Filter out comment lines
      code_matches <- matches[!grepl("^\\s*#", matches)]
      
      if (length(code_matches) > 0) {
        fail(paste0(
          "Found forbidden column/pattern '", col_pattern, "' in ", file_name, ":\n",
          paste(code_matches, collapse = "\n"),
          "\n\nAir sites apps should not reference ground-specific columns."
        ))
      }
    }
  }
  
  expect_true(TRUE, "All air sites files free of forbidden ground columns")
})

test_that("data functions can be parsed without syntax errors", {
  root <- get_project_root()
  
  # Test all data_functions.R files can be parsed
  data_function_files <- list.files(
    file.path(root, "apps"), 
    pattern = "data_functions\\.R$", 
    recursive = TRUE, 
    full.names = TRUE
  )
  
  for (file_path in data_function_files) {
    app_name <- basename(dirname(file_path))
    
    tryCatch({
      parse(file_path)
    }, error = function(e) {
      fail(paste("Syntax error in", app_name, "data_functions.R:", e$message))
    })
  }
  
  expect_gt(length(data_function_files), 0, "Found at least one data_functions.R file")
})