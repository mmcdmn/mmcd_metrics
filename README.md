# MMCD Metrics Dashboard

![MMCD Analytics](https://img.shields.io/badge/MMCD-Analytics-blue)
![R Shiny](https://img.shields.io/badge/R-Shiny-276DC3)
![Status](https://img.shields.io/badge/Status-Active-green)

A comprehensive analytics platform for the Metropolitan Mosquito Control District, providing interactive dashboards for mosquito surveillance, treatment analysis, and operational metrics.




## Table of Contents

- [Documentation Resources](#documentation-resources)
- [Common Filter & Grouping Options](#common-filter--grouping-options)
- [Architecture](#architecture)
- [Centralized Shared Resources](#centralized-shared-resources-shared)
- [Applications](documents/applications.md)
- [Setup & Installation](documents/setup.md)
- [Updating Configuration Files](#updating-configuration-files)
- [Git Workflow for Contributors](#git-workflow-for-contributors)
  - [Getting Started - Cloning the Repository](#getting-started---cloning-the-repository)
  - [Branch Strategy](#branch-strategy)
  - [Basic Git Commands](#basic-git-commands)
  - [Typical Workflow for Making Changes](#typical-workflow-for-making-changes)
  - [Common Scenarios](#common-scenarios)
  - [Best Practices](#best-practices)
  - [Handling Merge Conflicts](#handling-merge-conflicts)
- [Important Bug Fixes](#important-bug-fixes)

## Documentation Resources

Comprehensive technical documentation and performance reports:

### Database & Schema
- **[Database Schema Reference](documents/DATABASE_SCHEMA_REFERENCE.md)** - Comprehensive guide to all database tables, columns, relationships, and common join patterns used across applications

### Performance & Optimization
- **[Connection Pooling Performance Report](documents/CONNECTION_POOLING_PERFORMANCE_REPORT.md)** - Before/after analysis of database connection pooling implementation showing 22% reduction in errors and improved reliability
- **[Optimization Analysis](documents/OPTIMIZATION_ANALYSIS.md)** - Comprehensive performance optimization opportunities and implementation strategies
- **[Before and After Data Pooling Tests](documents/before%20and%20after%20data%20pooling%20tests.pdf)** - Visual comparison of stress test results

### Infrastructure & Load Balancing
- **[Load Balancer Architecture](documents/LOAD_BALANCER_ARCHITECTURE.md)** - Complete guide to the OpenResty + Lua + Redis dynamic load-aware routing system, including request queueing, worker distribution, and production deployment architecture

### UI Customization
- **[Color Themes Configuration](documents/COLOR_THEMES_README.md)** - Centralized color theme system where one change in `shared/db_helpers.R` automatically updates colors across all apps
- **[Theme Configuration Guide](documents/THEME_CONFIGURATION.md)** - Detailed theme customization instructions

### Development Resources
- **[Shared Resources README](shared/README.md)** - Documentation for shared utilities, database helpers, geospatial data extraction (Q_to_R), and documentation sync system
- **[User Feedback Backlog](documents/USER_FEEDBACK_BACKLOG.md)** - Feature requests and improvements from user feedback

## Common Filter & Grouping Options

Apps share consistent filter and grouping controls for unified user experience:

**Standard Filters (used when appropriate for each app):**
- **Facility** - Filter by facility location (Sr, Sj, N, E, MO, etc)
- **FOS Area** - Filter by Field Operations Supervisor (foreman) assignment
- **Zone** - P1 only, P2 only, P1+P2 separate, or Combined P1+P2
- **Priority** - Filter by site priority level (RED, YELLOW, BLUE, GREEN, PURPLE)
- **Treatment Type** - Air, Ground, or Drone (note: drone can be subcategorized as ground or air depending on application)
- **Pretend Today Is** - Simulate a different analysis date to see what status/progress would have been on that date (useful for testing, historical analysis, or planning)
- **Days Until Expiring** - For treatment progress apps, set threshold for sites approaching treatment expiration
- **Color Theme** - Choose from 6 themes (MMCD default, IBM, Wong, Tol, Viridis, ColorBrewer) - changes all charts and maps

**Display Metric Options:**
- **Sites vs Acres** - Toggle between count of sites or total acres for all visualizations and summaries

**Group By Options:**
- **All MMCD** - Show aggregate data across entire district
- **Facility** - Group results by facility
- **FOS** - Group results by Field Operations Supervisor
- **Section** - Group results by geographic section (available in some apps)

**How "Pretend Today Is" Works:** This date control allows you to run the analysis as if today were a different date. For example, setting it to a past date shows what the status would have looked like then, while setting it to a future date can help with planning. All status calculations, expiration logic, and "days until expiring" filters use this date instead of the actual current date.
NOTE: "Pretend Today Is" will only work if the query includes the same join with the archive as it does with the current

## Architecture

This platform hosts multiple R Shiny applications in an organized, scalable structure with a **modular architecture** and centralized helper module:

```
mmcd_metrics/
├── shared/
│   ├── db_helpers.R              # Centralized hub for all apps
│   ├── db_pool.R                 # Database connection pooling for performance
│   ├── color_themes.R            # Centralized color definitions
│   ├── stat_box_helpers.R        # Reusable stat box UI components
│   ├── assets/                   # Shared icons and images
│   └── Q_to_R/                   # Geospatial data extraction scripts
│       └── data/                 # Extracted shapefiles and boundaries
├── apps/
│   ├── index.html                # Main landing page
│   ├── about/                    # About page
│   │   └── index.html
│   ├── inspections/              # Inspection coverage analysis (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing and database queries
│   │   ├── display_functions.R   # Visualization and chart generation
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── catch_basin_status/       # Catch basin treatment tracking (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── historical_functions.R # Historical analysis functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── mosquito-monitoring/      # CO2 trap surveillance data
│   │   └── app.R
│   ├── suco_history/             # SUCO surveillance analysis (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing and database queries
│   │   ├── display_functions.R   # Visualization and chart generation
│   │   ├── ui_helpers.R          # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── drone/                    # Comprehensive drone treatment (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── historical_functions.R # Historical data analysis functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── struct_trt/               # Structure treatment (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── historical_functions.R # Historical analysis functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── ground_prehatch_progress/ # Ground prehatch treatment (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── historical_functions.R # Historical analysis functions
│   │   ├── ui_helpers.R          # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── cattail_inspections/      # Cattail inspection tracking (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing and database queries
│   │   ├── display_functions.R   # Visualization and chart generation
│   │   ├── historical_functions.R # Historical analysis functions
│   │   ├── planned_treatment_functions.R # Treatment planning functions
│   │   ├── progress_functions.R  # Progress tracking functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── cattail_treatments/       # Cattail treatment tracking (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing and database queries
│   │   ├── display_functions.R   # Visualization and chart generation
│   │   ├── historical_functions.R # Historical analysis with DOY-based inspection years
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── red_air_legacy/           # Legacy air treatment pipeline
│   │   ├── app.R                 # Main application logic
│   │   ├── air_status_functions.R # Air site status processing
│   │   ├── legacy_air_status_functions.R # Legacy status functions
│   │   ├── optimized_air_status_functions.R # Optimized functions
│   │   ├── flow_testing_functions.R # Flow testing utilities
│   │   └── test_14_day_persistence.R # Testing functions
│   ├── air_sites_simple/         # Air sites simple (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── historical_functions.R # Historical analysis functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── mosquito_surveillance_map/ # Mosquito surveillance mapping
│   │   ├── app.R                 # Main application logic
│   │   └── shp/                  # Shapefile data (geographic boundaries)
│   ├── control_efficacy/         # Air treatment checkback efficacy (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── checkback_functions.R # Checkback analysis functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── trap_survillance_test/    # Trap surveillance test (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── mle_trap_based.R      # MLE calculation functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   ├── section-cards/            # Section Cards (modular structure)
│   │   ├── app.R                 # Main application logic
│   │   ├── data_functions.R      # Data processing functions
│   │   ├── display_functions.R   # Visualization functions
│   │   ├── ui_helper.R           # UI component functions
│   │   ├── NOTES.md              # Technical documentation
│   │   └── NOTES.html            # HTML documentation
│   └── test-app/                 # Test application 
│       └── app.R                 # Main application logic
├── documents/                    # Technical documentation and reports
│   ├── CONNECTION_POOLING_PERFORMANCE_REPORT.md
│   ├── OPTIMIZATION_ANALYSIS.md
│   ├── COLOR_THEMES_README.md
│   ├── THEME_CONFIGURATION.md
│   ├── USER_FEEDBACK_BACKLOG.md
│   └── before and after data pooling tests.pdf
├── dockerfile                    # Docker container configuration
├── index.html                    # Main landing page
├── README.md                     # This file
└── shiny-server.conf            # Shiny Server configuration
```

## Centralized Shared Resources (`shared/`)

All apps source `shared/db_helpers.R` which provides centralized utilities and automatically loads all other shared modules.

**Core Files:**
- **`db_helpers.R`** - Central hub: database connections, facility/FOS lookups, date thresholds, priority choices
- **`color_themes.R`** - **ALL colors in ALL apps come from ONE location!** Change colors here to update everywhere. Supports 6 themes: MMCD (default), IBM, Wong, Tol, Viridis, ColorBrewer. See [Color Themes Configuration](documents/COLOR_THEMES_README.md)
- **`db_pool.R`** - Connection pooling for 22% fewer errors and 81% faster queries. See [Performance Report](documents/CONNECTION_POOLING_PERFORMANCE_REPORT.md)
- **`stat_box_helpers.R`** - Reusable UI components for consistent value boxes

**Usage in apps:**
```r
source("../../shared/db_helpers.R")  # Loads all shared modules
conn <- get_pool()                    # Get connection pool
palette <- get_theme_palette()        # Get current theme colors
```

**Additional Resources:**
- **`assets/`** - Shared icons and images for landing pages
- **`Q_to_R/`** - Geospatial data extraction scripts (PostgreSQL to shapefiles)

See [Shared Resources README](shared/README.md) for complete documentation.

## Documentation

Detailed documentation for setup, deployment, applications, and configuration:

- **[Applications](documents/applications.md)** - Descriptions of all 15 applications with features and modular structure
- **[Setup & Installation](documents/setup.md)** - Installation, deployment, Docker, and AWS instructions
- **[Updating Configuration Files](documents/updating-config.md)** - Step-by-step guide for modifying and deploying config changes

## Git Workflow for Contributors

**Repository**: [https://github.com/mmcdmn/mmcd_metrics](https://github.com/mmcdmn/mmcd_metrics)

> **Access Required**: You must have repository access rights to push changes. Contact the administrator if you need access.

### Getting Started - Cloning the Repository

If you're starting fresh, clone the repository and checkout the dev branch:

```bash
# Clone the repository
git clone https://github.com/mmcdmn/mmcd_metrics.git

# Navigate into the project
cd mmcd_metrics

# Switch to dev branch (where you'll do your work)
git checkout dev

# Verify you're on dev branch
git branch
```

### Branch Strategy

This repository uses a two-branch workflow:

- **`dev`** - Development branch for testing and review
  - All contributors should push their changes here first
  - Used for testing new features and bug fixes
  - Safe environment for experimentation

- **`main`** - Production branch deployed to AWS
  - Protected branch
  - Automatically deployed to AWS App Runner for production use
  - Only stable, tested code should be merged here
  - **Merging to main is done via GitHub Pull Request, not locally**

### Basic Git Commands

#### Check Current Status
See what files you've changed and current branch:
```bash
git status
```

#### Switch Between Branches
```bash
# Switch to dev branch (where you should work)
git checkout dev

# Switch to main branch (to review production code)
git checkout main

# Create and switch to a new feature branch
git checkout -b feature-name
```

#### Stage Your Changes
Add files you want to commit:
```bash
# Add specific file
git add path/to/file.R

# Add all changed files in a directory
git add apps/my-app/

# Add all changed files (use carefully!)
git add .
```

#### Commit Your Changes
Save your changes with a descriptive message:
```bash
git commit -m "Brief description of what you changed"

# Example commit messages:
git commit -m "Fix zone assignment bug in drone app"
git commit -m "Add archive data support to trap surveillance"
git commit -m "Update README with Git workflow instructions"
```

#### Push to Remote Repository
```bash
# Push your changes to dev branch
git push origin dev

# Push changes to a feature branch
git push origin feature-name
```

### Typical Workflow for Making Changes

1. **Start on dev branch**
   ```bash
   git checkout dev
   git pull origin dev  # Get latest changes
   ```

2. **Make your changes**
   - Edit files in your preferred editor
   - Test your changes locally

3. **Stage and commit**
   ```bash
   git status                    # See what changed
   git add apps/my-app/app.R     # Stage your files
   git commit -m "Description of changes"
   ```

4. **Push to dev**
   ```bash
   git push origin dev
   ```

5. **Test on dev environment**
   - Verify your changes work as expected
   - Have others review if needed

6. **Merge to main (when ready for production)**
   
   **Merging is done on GitHub, not locally:**
   
   a. Go to the repository on GitHub: [https://github.com/mmcdmn/mmcd_metrics](https://github.com/mmcdmn/mmcd_metrics)
   
   b. Click "Pull requests" tab
   
   c. Click "New pull request"
   
   d. Set base branch to `main` and compare branch to `dev`
   
   e. Review the changes, add a description
   
   f. Click "Create pull request"
   
   g. Wait for review/approval (if required)
   
   h. Click "Merge pull request" to deploy to production
   
   i. Once merged, the changes automatically deploy to AWS App Runner

### Common Scenarios

#### Pulling Latest Changes
Before starting work, always pull the latest code:
```bash
git fetch
git checkout dev
git pull origin dev
```

#### Viewing Your Changes
See what you've modified:
```bash
git status                    # Files changed
git diff                      # See line-by-line changes
git diff apps/my-app/app.R   # Changes in specific file
```

#### Undoing Changes
If you made a mistake:
```bash
# Discard changes to a specific file
git checkout -- path/to/file.R

# Discard all uncommitted changes (careful!)
git reset --hard
```

#### Viewing Commit History
```bash
git log                       # Full history
git log --oneline             # Compact view
git log --graph --oneline     # Visual branch history
```

### Best Practices

1. **Always work on `dev` first** - Never push directly to `main`
2. **Pull before you push** - Get latest changes to avoid conflicts
3. **Commit often** - Small, focused commits are easier to review
4. **Write clear commit messages** - Future you will thank present you
5. **Test before merging to main** - Production should always be stable
6. **Communicate with team** - Let others know about major changes

### Handling Merge Conflicts

If Git can't automatically merge your changes:

1. **Identify conflicted files**
   ```bash
   git status
   ```

2. **Open conflicted files** - Look for conflict markers:
   ```
   <<<<<<< HEAD
   Your changes
   =======
   Someone else's changes
   >>>>>>> branch-name
   ```

3. **Resolve conflicts** - Edit files to keep the correct code

4. **Mark as resolved and commit**
   ```bash
   git add conflicted-file.R
   git commit -m "Resolve merge conflicts"
   ```

### Getting Help

```bash
git help                     # General help
git help commit             # Help for specific command
git status                  # Shows current state and helpful hints
```

For more information, see the [Git documentation](https://git-scm.com/doc) or contact a team member with Git experience.

---

## Important Bug Fixes

### **Zone Assignment Fix (November 2025)**
**CRITICAL FIX**: Corrected ambiguous JOIN logic in `apps/drone/data_functions.R` that was causing incorrect zone assignments.

**Problem**: Sites were showing wrong zones due to broad pattern matching in SQL JOINs. For example:
- Site `191819-045` was incorrectly showing zone 2 instead of zone 1
- Root cause: JOIN matched both `191819-` (zone 1) and `191819E` (zone 2) sectcodes

**Solution**: Changed from broad OR-based pattern matching to precise sectcode matching:
```sql
-- OLD (incorrect - would match multiple sectcodes)
LEFT JOIN public.gis_sectcode g ON LEFT(sitecode, 6) || '-' = g.sectcode
  OR LEFT(sitecode, 6) || 'N' = g.sectcode
  OR LEFT(sitecode, 6) || 'E' = g.sectcode
  OR LEFT(sitecode, 6) || 'W' = g.sectcode

-- NEW (correct - exact match only)  
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(sitecode,7)
```
