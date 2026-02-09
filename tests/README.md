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
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e "testthat::test_file('testthat.R', reporter = 'summary')"
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