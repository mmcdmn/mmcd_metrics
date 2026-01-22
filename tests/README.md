# MMCD Metrics Test Suite

## Overview

This folder contains unit tests for the MMCD Metrics dashboard applications using the `testthat` framework.

## Folder Structure

```
tests/
├── testthat.R           # Main test runner
├── test_stubs.R         # Mock/stub functions for isolated testing
├── README.md            # This file
├── shared/              # Tests for shared modules
│   ├── test-color-themes.R
│   ├── test-database-connection.R
│   ├── test-export-helpers.R
│   ├── test-filter-helpers.R
│   ├── test-historical-helpers.R
│   ├── test-lookup-functions.R
│   ├── test-server-utilities.R
│   └── test-stat-box-helpers.R
└── apps/                # Tests for individual apps (TODO)
    ├── struct_trt/
    ├── drone/
    ├── catch_basin_status/
    └── ...
```

## Running Tests

### From Command Line

```bash
cd mmcd_metrics
Rscript tests/testthat.R
```

### From R Console

```r
source("tests/testthat.R")
```

## Test Modes

### Isolated Mode (Default)

Set `TESTING_MODE_ISOLATED <- TRUE` in `testthat.R`

- Uses mock/stub functions from `test_stubs.R`
- No database connection required
- Tests pure function logic only
- Fast and safe for CI/CD

### Integration Mode

Set `TESTING_MODE_ISOLATED <- FALSE` in `testthat.R`

- Uses real database connection
- Requires `.env` file with valid credentials
- Tests full functionality including database queries
- Slower but more comprehensive

## Test Stubs

The `test_stubs.R` file provides mock implementations for database-dependent functions:

| Real Function | Stub Behavior |
|--------------|---------------|
| `get_pool()` | Returns NULL |
| `get_db_connection()` | Returns NULL |
| `get_facility_lookup()` | Returns sample data (E, MO, N, Sj, Sr, Wm, Wp) |
| `get_foremen_lookup()` | Returns sample foremen data |
| `get_facility_choices()` | Returns named vector with facilities |
| `get_foreman_choices()` | Returns named vector with foremen |
| `get_priority_choices()` | Returns RED, YELLOW, BLUE, GREEN, PURPLE |
| `get_virus_target_choices()` | Returns WNV, LAC, JC, EEE, WEE |

**Note**: Stub data is sourced from the actual MMCD database to ensure realistic test scenarios.

## Adding New Tests

### For Shared Modules

1. Create a new file in `tests/shared/` named `test-<module-name>.R`
2. Follow the existing test patterns
3. Use `context()` to group related tests
4. Use `test_that()` for individual test cases

### For Apps

1. Create a new folder in `tests/apps/<app-name>/`
2. Create test files following the pattern `test-<feature>.R`
3. The test runner will automatically source app helper functions

## Test Guidelines

1. **Independence**: Each test should be independent and not rely on state from other tests
2. **Stubs**: If a test requires database access, ensure the stub provides appropriate mock data
3. **Descriptive Names**: Use descriptive test names that explain what's being tested
4. **Edge Cases**: Include tests for edge cases (NULL, empty, invalid inputs)

## Known Limitations

- Some Shiny UI tests require a full Shiny environment to pass
- File export tests may fail in isolated environments without write permissions
- Plotly chart tests validate object creation, not visual output

## Last Updated

2026-01-22 - Initial test suite created with stubs for isolated testing
