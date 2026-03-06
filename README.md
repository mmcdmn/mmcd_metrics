# MMCD Metrics Dashboard

![MMCD Analytics](https://img.shields.io/badge/MMCD-Analytics-blue)
![R Shiny](https://img.shields.io/badge/R-Shiny-276DC3)
![Status](https://img.shields.io/badge/Status-Active-green)

A comprehensive analytics platform for the Metropolitan Mosquito Control District, providing interactive dashboards for mosquito surveillance, treatment analysis, and operational metrics.

---

## Documentation

| Document | Description |
|----------|-------------|
| **[Architecture](documents/ARCHITECTURE.md)** | Full project directory structure and shared module reference |
| **[Applications](documents/applications.md)** | Detailed descriptions and features for every app |
| **[Setup & Installation](documents/setup.md)** | Installation, deployment, Docker, and AWS instructions |
| **[Updating Configuration](documents/updating-config.md)** | How to modify and deploy config changes |
| **[Database Schema Reference](documents/DATABASE_SCHEMA_REFERENCE.md)** | All database tables, columns, relationships, and join patterns |
| **[Geospatial Data Reference](documents/GEOSPATIAL_DATA.md)** | Shapefiles in Q_to_R/data — what exists, what each app uses |
| **[Connection Pooling Report](documents/CONNECTION_POOLING_PERFORMANCE_REPORT.md)** | Before/after analysis of database connection pooling |
| **[Optimization Analysis](documents/OPTIMIZATION_ANALYSIS.md)** | Performance optimization opportunities and strategies |
| **[Performance Optimization Guide](documents/PERFORMANCE_OPTIMIZATION_GUIDE.md)** | Performance tuning guide |
| **[Load Balancer Architecture](documents/LOAD_BALANCER_ARCHITECTURE.md)** | OpenResty + Lua + Redis dynamic routing system |
| **[Color Themes](documents/COLOR_THEMES_README.md)** | Centralized color theme system (one change updates all apps) |
| **[Theme Configuration](documents/THEME_CONFIGURATION.md)** | Detailed theme customization instructions |
| **[User Feedback Backlog](documents/USER_FEEDBACK_BACKLOG.md)** | Feature requests and improvements from user feedback |
| **[Shared Resources](shared/SHARED_RESOURCES.md)** | Shared utilities, database helpers, cache, and doc sync system |
| **[Overview Framework](apps/overview/OVERVIEW_FRAMEWORK.md)** | Overview dashboard architecture and metric registry pattern |
| **[Geospatial Extraction Scripts](shared/Q_to_R/GEOSPATIAL_EXTRACTION.md)** | How to extract shapefiles from PostgreSQL/PostGIS |
| **[Test Suite](tests/TEST_SUITE.md)** | Test framework, running tests, stubs, and test modes |

---

## Applications

| App | Path | Description | Notes |
|-----|------|-------------|-------|
| Air Inspection Checklist | `/air_inspection_checklist/` | Air site inspection checklists by FOS | [NOTES](apps/air_inspection_checklist/NOTES.md) |
| Air Sites Simple | `/air_sites_simple/` | Air site status and treatment pipeline tracking | [NOTES](apps/air_sites_simple/NOTES.md) |
| Catch Basin Status | `/catch_basin_status/` | Catch basin treatment status across District | [NOTES](apps/catch_basin_status/NOTES.md) |
| Cattail Inspections | `/cattail_inspections/` | Cattail inspection progress tracking against goals | [NOTES](apps/cattail_inspections/NOTES.md) |
| Cattail Treatments | `/cattail_treatments/` | Cattail treatment tracking with DOY-based inspection years | [NOTES](apps/cattail_treatments/NOTES.md) |
| Control Efficacy | `/control_efficacy/` |  Treatment checkback analysis and efficacy monitoring | [NOTES](apps/control_efficacy/NOTES.md) |
| Drone | `/drone/` | Drone treatment tracking with progress and historical analysis | [NOTES](apps/drone/NOTES.md) |
| Ground Prehatch Progress | `/ground_prehatch_progress/` | Ground prehatch treatment progress and performance | [NOTES](apps/ground_prehatch_progress/NOTES.md) |
| Inspections | `/inspections/` | Inspection coverage analysis, wet frequency, larvae thresholds | [NOTES](apps/inspections/NOTES.md) |
| Mosquito Monitoring | `/mosquito-monitoring/` | CO2 trap surveillance with 50+ species | — |
| Mosquito Surveillance Map | `/mosquito_surveillance_map/` | Geographic mapping of surveillance data | — |
| Overview | `/overview/` | Unified overview dashboard with drill-down navigation | [Framework](apps/overview/OVERVIEW_FRAMEWORK.md) |
| Section Cards | `/section-cards/` | Printable field Section cards for section ground/air sits and structure data | [NOTES](apps/section-cards/NOTES.md) |
| Structure Treatment | `/struct_trt/` | Structure treatment tracking with progress and history | [NOTES](apps/struct_trt/NOTES.md) |
| SUCO History | `/suco_history/` | Suco Count historical analysis and mapping with species specific views | [NOTES](apps/suco_history/NOTES.md) |
| Trap Surveillance | `/trap_surveillance/` | Vector Index, MLE, and trap-based spatial interpolation | [NOTES](apps/trap_surveillance/NOTES.md) |
| Trap Surveillance (Test) | `/trap_survillance_test/` | Prototype/test version of trap surveillance | — |
| Test App | `/test-app/` | Admin utilities: cache management, routing status, config viewer | — |

---

## Common Filter & Grouping Options

Apps share consistent filter and grouping controls for unified user experience:

**Standard Filters (used when appropriate for each app):**
- **Facility** - Filter by facility location (Sr, Sj, N, E, MO, etc)
- **FOS Area** - Filter by Field Operations Supervisor (foreman) assignment
- **Zone** - P1 only, P2 only, P1+P2 separate, or Combined P1+P2
- **Priority** - Filter by site priority level (RED, YELLOW, BLUE, GREEN, PURPLE)
- **Treatment Type** - Air, Ground, or Drone
- **Pretend Today Is** - Simulate a different analysis date for historical analysis or planning
- **Days Until Expiring** - Threshold for sites approaching treatment expiration
- **Color Theme** - Choose from 6 themes (MMCD default, IBM, Wong, Tol, Viridis, ColorBrewer)

**Display Metric Options:**
- **Sites vs Acres** - Toggle between count of sites or total acres

**Group By Options:**
- **All MMCD** - Aggregate data across entire district
- **Facility** - Group by facility
- **FOS** - Group by Field Operations Supervisor
- **Section** - Group by geographic section (some apps)

**How "Pretend Today Is" Works:** Runs analysis as if today were a different date. All status calculations, expiration logic, and "days until expiring" filters use this date instead of the actual current date. Only works if the query includes the same join with the archive as it does with the current.

---

## Git Workflow for Contributors

**Repository**: [https://github.com/mmcdmn/mmcd_metrics](https://github.com/mmcdmn/mmcd_metrics)

> **Access Required**: Contact the administrator if you need repository access.

### Branch Strategy

- **`dev`** - Development branch. All contributors push changes here first for testing and review.
- **`main`** - Production branch. Automatically deployed to AWS App Runner. Merge via GitHub Pull Request only.

### Typical Workflow

```bash
# 1. Start on dev branch with latest changes
git checkout dev
git pull origin dev

# 2. Make your changes and test locally

# 3. Stage, commit, push
git status
git add apps/my-app/app.R
git commit -m "Brief description of what you changed"
git push origin dev

# 4. Test on dev, then merge to main via GitHub Pull Request
```

### Quick Reference

| Action | Command |
|--------|---------|
| Check status | `git status` |
| See changes | `git diff` |
| View history | `git log --oneline` |
| Discard file changes | `git checkout -- path/to/file.R` |
| Switch branch | `git checkout dev` |

---

## Documentation Sync

The sync system converts `NOTES.md` files to `NOTES.html` across all apps:

```powershell
cd "c:\Users\datatech\Documents\mmcd_metrics\shared"

# Check what needs updating
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" sync_all_docs.R --status

# Sync changed files
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" sync_all_docs.R

# Force rebuild all
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" sync_all_docs.R --force
```

---

## Running Tests

```powershell
cd "c:\Users\datatech\Documents\mmcd_metrics\tests"
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e "testthat::test_file('testthat.R', reporter = 'summary')"
```

See [Test Suite documentation](tests/TEST_SUITE.md) for details on isolated vs integration modes and test stubs.

---

## Important Bug Fixes

### Zone Assignment Fix (November 2025)
Corrected ambiguous JOIN logic in `apps/drone/data_functions.R` that caused incorrect zone assignments. Sites were matching multiple sectcodes due to broad OR-based pattern matching.

```sql
-- OLD (incorrect)
LEFT JOIN public.gis_sectcode g ON LEFT(sitecode, 6) || '-' = g.sectcode
  OR LEFT(sitecode, 6) || 'N' = g.sectcode
  OR LEFT(sitecode, 6) || 'E' = g.sectcode
  OR LEFT(sitecode, 6) || 'W' = g.sectcode

-- NEW (correct - exact match)
LEFT JOIN public.gis_sectcode g ON g.sectcode = left(sitecode,7)
```
