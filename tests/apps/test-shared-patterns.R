# =============================================================================
# TEST: Standardized Data Pipeline Across Treatment Progress Apps
# =============================================================================
# These tests REQUIRE all apps to have IDENTICAL function signatures and
# data formats. If a test fails, the APP MUST BE CHANGED to match the standard.
#
# STANDARD REQUIREMENTS:
# 1. load_raw_data(analysis_date, include_archive, start_year, end_year, include_geometry)
#    -> Returns list with sites and treatments data frames
# 2. apply_data_filters(data, facility_filter, foreman_filter, zone_filter)
#    -> Filters the data list and returns filtered list
# 3. Aggregated data MUST have: total_count, active_count, expiring_count, display_name
#
# If an app doesn't conform, IT MUST BE UPDATED.
# =============================================================================

# Helper to get project root
get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

# Load ALL existing stub data files (includes standardized helpers)
root <- get_project_root()
source(file.path(root, "tests/stubs/stubs.R"))

# =============================================================================
# THE 4 STANDARDIZED TREATMENT PROGRESS APPS
# =============================================================================
# All these apps MUST follow the same patterns. Add new apps here when ready.
STANDARDIZED_APPS <- c("drone", "ground_prehatch_progress", "struct_trt", "catch_basin_status")

# =============================================================================
# REQUIRED FUNCTIONS - Every app MUST have these
# =============================================================================
REQUIRED_FUNCTIONS <- c("load_raw_data", "apply_data_filters")

# =============================================================================
# REQUIRED AGGREGATED COLUMNS - Every app MUST produce these
# =============================================================================
REQUIRED_AGGREGATED_COLUMNS <- c("total_count", "active_count", "expiring_count", "display_name")

# =============================================================================
# FILE STRUCTURE TESTS - All apps must have same file structure
# =============================================================================

test_that("all apps have required file structure", {
  root <- get_project_root()
  required_files <- c("app.R", "data_functions.R", "display_functions.R", 
                      "historical_functions.R", "ui_helper.R")
  
  for (app in STANDARDIZED_APPS) {
    for (file in required_files) {
      file_path <- file.path(root, "apps", app, file)
      expect_true(file.exists(file_path), 
                  info = paste0(app, " is missing required file: ", file))
    }
  }
})

# =============================================================================
# FUNCTION EXISTENCE TESTS - All apps must have load_raw_data
# =============================================================================

test_that("all apps have load_raw_data function", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    # Source the data_functions.R
    env <- new.env()
    source(file.path(root, "apps", app, "data_functions.R"), local = env)
    
    expect_true(exists("load_raw_data", envir = env),
                info = paste0(app, " MUST have load_raw_data function - UPDATE THE APP"))
  }
})

test_that("all apps have apply_data_filters function", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    env <- new.env()
    source(file.path(root, "apps", app, "data_functions.R"), local = env)
    
    expect_true(exists("apply_data_filters", envir = env),
                info = paste0(app, " MUST have apply_data_filters function - UPDATE THE APP"))
  }
})

# =============================================================================
# FUNCTION SIGNATURE TESTS - All apps must have same parameters
# =============================================================================

test_that("all apps load_raw_data accepts analysis_date parameter", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    env <- new.env()
    source(file.path(root, "apps", app, "data_functions.R"), local = env)
    
    if (exists("load_raw_data", envir = env)) {
      args <- names(formals(env$load_raw_data))
      expect_true("analysis_date" %in% args,
                  info = paste0(app, " load_raw_data must accept analysis_date parameter"))
    }
  }
})

test_that("all apps load_raw_data accepts include_archive parameter", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    env <- new.env()
    source(file.path(root, "apps", app, "data_functions.R"), local = env)
    
    if (exists("load_raw_data", envir = env)) {
      args <- names(formals(env$load_raw_data))
      expect_true("include_archive" %in% args,
                  info = paste0(app, " load_raw_data must accept include_archive parameter"))
    }
  }
})

test_that("all apps apply_data_filters accepts required parameters", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    env <- new.env()
    source(file.path(root, "apps", app, "data_functions.R"), local = env)
    
    if (exists("apply_data_filters", envir = env)) {
      args <- names(formals(env$apply_data_filters))
      expect_true("data" %in% args,
                  info = paste0(app, " apply_data_filters must accept data parameter"))
      expect_true("facility_filter" %in% args,
                  info = paste0(app, " apply_data_filters must accept facility_filter parameter"))
      expect_true("foreman_filter" %in% args,
                  info = paste0(app, " apply_data_filters must accept foreman_filter parameter"))
      expect_true("zone_filter" %in% args,
                  info = paste0(app, " apply_data_filters must accept zone_filter parameter"))
    }
  }
})

# =============================================================================
# FILTER FUNCTIONALITY TESTS - All apps must filter data the same way
# =============================================================================

# All apps now return STANDARDIZED format: list(sites, treatments, total_count)
# No helper function needed - just use result$sites or result$treatments directly

test_that("all apps apply_data_filters correctly filters by facility", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    env <- new.env()
    source(file.path(root, "apps", app, "data_functions.R"), local = env)
    
    if (exists("apply_data_filters", envir = env)) {
      # Get stub data for this app (now ALL apps return same format)
      test_data <- get_stub_raw_data_for_app(app)
      
      # Filter by facility "N"
      result <- env$apply_data_filters(test_data, facility_filter = "N")
      
      # All apps return list with sites key
      filtered_data <- result$sites
      
      if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
        expect_true(all(filtered_data$facility == "N"),
                    info = paste0(app, " should only return facility N data"))
      }
    }
  }
})

test_that("all apps apply_data_filters correctly filters by zone", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    env <- new.env()
    source(file.path(root, "apps", app, "data_functions.R"), local = env)
    
    if (exists("apply_data_filters", envir = env)) {
      test_data <- get_stub_raw_data_for_app(app)
      
      # Filter by zone "1"
      result <- env$apply_data_filters(test_data, zone_filter = "1")
      
      # All apps return list with sites key (STANDARDIZED)
      filtered_data <- result$sites
      
      if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
        expect_true(all(filtered_data$zone == "1"),
                    info = paste0(app, " should only return zone 1 data"))
      }
    }
  }
})

test_that("all apps apply_data_filters handles 'all' filter correctly", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    env <- new.env()
    source(file.path(root, "apps", app, "data_functions.R"), local = env)
    
    if (exists("apply_data_filters", envir = env)) {
      test_data <- get_stub_raw_data_for_app(app)
      
      # All apps now return same format - use sites key
      original_count <- nrow(test_data$sites)
      
      # 'all' should return all data
      result <- env$apply_data_filters(test_data, facility_filter = "all")
      
      # All apps return list with sites key (STANDARDIZED)
      filtered_data <- result$sites
      
      if (!is.null(filtered_data)) {
        expect_equal(nrow(filtered_data), original_count,
                     info = paste0(app, " 'all' facility should return all data"))
      }
    }
  }
})

test_that("all apps apply_data_filters handles multiple facilities", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    env <- new.env()
    source(file.path(root, "apps", app, "data_functions.R"), local = env)
    
    if (exists("apply_data_filters", envir = env)) {
      test_data <- get_stub_raw_data_for_app(app)
      
      result <- env$apply_data_filters(test_data, facility_filter = c("N", "Sj"))
      
      # All apps return list with sites key (STANDARDIZED)
      filtered_data <- result$sites
      
      if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
        expect_true(all(filtered_data$facility %in% c("N", "Sj")),
                    info = paste0(app, " should only return N and Sj facilities"))
      }
    }
  }
})

test_that("all apps apply_data_filters handles empty result gracefully", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    env <- new.env()
    source(file.path(root, "apps", app, "data_functions.R"), local = env)
    
    if (exists("apply_data_filters", envir = env)) {
      test_data <- get_stub_raw_data_for_app(app)
      
      # Filter for facility that doesn't exist in stub data
      result <- env$apply_data_filters(test_data, facility_filter = "XX")
      
      # All apps return list with sites key (STANDARDIZED)
      filtered_data <- result$sites
      
      if (!is.null(filtered_data)) {
        expect_equal(nrow(filtered_data), 0,
                     info = paste0(app, " should return empty df for non-existent facility"))
      }
    }
  }
})

test_that("all apps apply_data_filters filters treatments along with sites", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    env <- new.env()
    source(file.path(root, "apps", app, "data_functions.R"), local = env)
    
    if (exists("apply_data_filters", envir = env)) {
      test_data <- get_stub_raw_data_for_app(app)
      
      result <- env$apply_data_filters(test_data, facility_filter = "N")
      
      # ALL apps now return standardized format with treatments key
      filtered_treatments <- result$treatments
      if (!is.null(filtered_treatments) && nrow(filtered_treatments) > 0) {
        expect_true(all(filtered_treatments$facility == "N"),
                    info = paste0(app, " should filter treatments by facility too"))
      }
    }
  }
})

# =============================================================================
# AGGREGATED DATA FORMAT TESTS - All apps must produce same columns
# =============================================================================

test_that("all apps produce aggregated data with total_count column", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    # Check data_functions.R and display_functions.R for column creation
    data_content <- readLines(file.path(root, "apps", app, "data_functions.R"), warn = FALSE)
    display_content <- readLines(file.path(root, "apps", app, "display_functions.R"), warn = FALSE)
    content_text <- paste(c(data_content, display_content), collapse = "\n")
    
    expect_true(grepl("total_count", content_text),
                info = paste0(app, " must produce total_count column in aggregated data"))
  }
})

test_that("all apps produce aggregated data with active_count column", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    data_content <- readLines(file.path(root, "apps", app, "data_functions.R"), warn = FALSE)
    display_content <- readLines(file.path(root, "apps", app, "display_functions.R"), warn = FALSE)
    app_content <- readLines(file.path(root, "apps", app, "app.R"), warn = FALSE)
    content_text <- paste(c(data_content, display_content, app_content), collapse = "\n")
    
    # Allow active_count OR active_acres (for acre-based metrics)
    expect_true(grepl("active_count|active_acres", content_text),
                info = paste0(app, " must produce active_count or active_acres column"))
  }
})

test_that("all apps produce aggregated data with expiring_count column", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    data_content <- readLines(file.path(root, "apps", app, "data_functions.R"), warn = FALSE)
    display_content <- readLines(file.path(root, "apps", app, "display_functions.R"), warn = FALSE)
    app_content <- readLines(file.path(root, "apps", app, "app.R"), warn = FALSE)
    content_text <- paste(c(data_content, display_content, app_content), collapse = "\n")
    
    expect_true(grepl("expiring_count|expiring_acres", content_text),
                info = paste0(app, " must produce expiring_count or expiring_acres column"))
  }
})

test_that("all apps produce aggregated data with display_name column", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    data_content <- readLines(file.path(root, "apps", app, "data_functions.R"), warn = FALSE)
    display_content <- readLines(file.path(root, "apps", app, "display_functions.R"), warn = FALSE)
    content_text <- paste(c(data_content, display_content), collapse = "\n")
    
    expect_true(grepl("display_name", content_text),
                info = paste0(app, " must produce display_name column in aggregated data"))
  }
})

# =============================================================================
# SHARED LIBRARY USAGE TESTS - All apps must use shared helpers
# =============================================================================

test_that("all apps source shared db_helpers", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "app.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    expect_true(grepl("db_helpers.R", content_text, fixed = TRUE),
                info = paste0(app, " must source db_helpers.R"))
  }
})

test_that("all apps source shared stat_box_helpers", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "app.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    expect_true(grepl("stat_box_helpers.R", content_text, fixed = TRUE),
                info = paste0(app, " must source stat_box_helpers.R"))
  }
})

test_that("all apps call set_app_name for logging", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "app.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    expect_true(grepl("set_app_name\\s*\\(", content_text),
                info = paste0(app, " must call set_app_name()"))
  }
})

# =============================================================================
# REACTIVE PATTERN TESTS - All apps must use same reactive patterns
# =============================================================================

test_that("all apps use eventReactive for refresh button data loading", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "app.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    expect_true(grepl("eventReactive.*refresh", content_text, ignore.case = TRUE),
                info = paste0(app, " must use eventReactive for refresh button"))
  }
})

test_that("all apps have current_theme reactive", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "app.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    expect_true(grepl("current_theme\\s*(<-|=)\\s*reactive", content_text),
                info = paste0(app, " must have current_theme reactive"))
  }
})

test_that("all apps update foreman filter when facility changes", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "app.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    expect_true(grepl("observeEvent.*facility", content_text, ignore.case = TRUE),
                info = paste0(app, " must have facility->foreman cascade"))
  }
})

# =============================================================================
# DISPLAY FUNCTION TESTS - All apps must have same display patterns
# =============================================================================

test_that("all apps have chart creation function", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "display_functions.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    expect_true(grepl("create.*chart|create.*plot|ggplot", content_text, ignore.case = TRUE),
                info = paste0(app, " must have chart creation function"))
  }
})

test_that("all apps use plotly for interactive charts", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    display_content <- readLines(file.path(root, "apps", app, "display_functions.R"), warn = FALSE)
    app_content <- readLines(file.path(root, "apps", app, "app.R"), warn = FALSE)
    content_text <- paste(c(display_content, app_content), collapse = "\n")
    
    expect_true(grepl("ggplotly|plotly|plot_ly|renderPlotly", content_text),
                info = paste0(app, " must use plotly for interactive charts"))
  }
})

# =============================================================================
# HISTORICAL FUNCTION TESTS - All apps must have historical capabilities
# =============================================================================

test_that("all apps have historical chart function", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "historical_functions.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    expect_true(grepl("create.*historical|historical.*data|historical.*plot", content_text, ignore.case = TRUE),
                info = paste0(app, " must have historical chart function"))
  }
})

test_that("all apps historical_functions use standardized keys from load_raw_data", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "historical_functions.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    # Should NOT use old app-specific keys
    old_keys_pattern <- "\\$drone_sites|\\$drone_treatments|\\$ground_sites|\\$ground_treatments"
    has_old_keys <- grepl(old_keys_pattern, content_text)
    
    expect_false(has_old_keys,
                 info = paste0(app, " historical_functions.R still uses old app-specific keys instead of standardized $sites/$treatments"))
  }
})

test_that("all apps historical_functions use standardized keys when calling apply_data_filters", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "historical_functions.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    # If apply_data_filters is called, it must use standardized list format
    if (grepl("apply_data_filters", content_text)) {
      # Check it's NOT passing old-style list keys to apply_data_filters
      # Pattern matches: apply_data_filters( ... list(drone_sites = or similar
      old_input_pattern <- "apply_data_filters\\s*\\([^)]*list\\s*\\(\\s*(drone_sites|drone_treatments|ground_sites|ground_treatments)\\s*="
      has_old_input <- grepl(old_input_pattern, content_text)
      
      expect_false(has_old_input,
                   info = paste0(app, " historical_functions.R passes old keys to apply_data_filters - must use list(sites=, treatments=, total_count=)"))
    }
  }
})

test_that("all apps app.R uses standardized keys from apply_data_filters", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "app.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    # Should NOT access old app-specific keys from filtered results
    old_access_pattern <- "filtered\\$drone_sites|filtered\\$drone_treatments|filtered\\$ground_sites|filtered\\$ground_treatments"
    has_old_access <- grepl(old_access_pattern, content_text)
    
    expect_false(has_old_access,
                 info = paste0(app, " app.R still accesses old keys from filtered data - must use $sites/$treatments"))
  }
})

# =============================================================================
# UI HELPER TESTS - All apps must have same UI patterns
# =============================================================================

test_that("all apps have create_help_text function", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "ui_helper.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    expect_true(grepl("create_help_text\\s*(<-|=)\\s*function", content_text),
                info = paste0(app, " must have create_help_text function"))
  }
})

test_that("all apps have refresh button in UI", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "ui_helper.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    expect_true(grepl("actionButton.*refresh", content_text, ignore.case = TRUE),
                info = paste0(app, " must have refresh button"))
  }
})

test_that("all apps have color theme selector", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "ui_helper.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    expect_true(grepl("color_theme", content_text),
                info = paste0(app, " must have color_theme selector"))
  }
})

test_that("all apps use tabsetPanel for navigation", {
  root <- get_project_root()
  
  for (app in STANDARDIZED_APPS) {
    content <- readLines(file.path(root, "apps", app, "ui_helper.R"), warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    expect_true(grepl("tabsetPanel|tabPanel", content_text),
                info = paste0(app, " must use tabs for navigation"))
  }
})
