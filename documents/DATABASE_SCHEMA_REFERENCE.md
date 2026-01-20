# Database Schema Reference

This document provides a comprehensive reference for all database tables used across the MMCD Metrics applications.

## Table of Contents

- [Overview](#overview)
- [Table Organization](#table-organization)
- [Current vs Archive Tables](#current-vs-archive-tables)
- [Core Data Tables](#core-data-tables)
  - [Larvae Inspection/Treatment Tables](#larvae-inspectiontreatment-tables)
  - [Adult Mosquito Surveillance Tables](#adult-mosquito-surveillance-tables)
  - [Virus Testing Tables](#virus-testing-tables)
- [Location Tables](#location-tables)
- [Lookup/Reference Tables](#lookupreference-tables)
- [Geometry/GIS Tables](#geometrygis-tables)
- [Common Join Patterns](#common-join-patterns)
- [Table Relationships](#table-relationships)

---

## Overview

The MMCD database follows a consistent pattern with **current** and **archive** table pairs:
- **Current tables**: Data from the current operational year
- **Archive tables**: Historical data from previous years
- Both table types share identical schemas

### Complete Table Inventory

The database contains the following tables (organized by category):

**Core Data Tables** (with current/archive pairs):
- `dblarv_insptrt_current` / `dblarv_insptrt_archive` - Larval inspections and treatments
- `dbadult_insp_current` / `dbadult_insp_archive` - Adult trap inspections
- `dbadult_species_current` / `dbadult_species_archive` - Adult trap species counts
- `dblarv_sample_current` / `dblarv_sample_archive` - Lab sample metadata (AIR/Ground)
- `dblarv_species_current` / `dblarv_species_archive` - Lab sample species identification

**Virus Testing Tables** (no archive - single tables):
- `dbvirus_pool_test` - Virus test results
- `dbvirus_pool` - Virus pool metadata

**Location Tables**:
- `loc_breeding_sites` - Breeding site locations (AIR, cattail, ground, etc.)
- `loc_cxstruct` - Structure breeding sites (not catch basins)
- `loc_harborage` - Adult mosquito harborage sites (SUCO locations)
- `loc_catchbasin` - Catch basin locations
- `loc_mondaynight` / `loc_mondaynight_active` - Monday night trap network

**Lookup/Reference Tables**:
- `gis_sectcode` - Section codes with zone/facility/FOS assignments (authoritative source)
- `mattype_list_targetdose` - Treatment material codes and effect durations
- `lookup_specieslist` - Mosquito species codes and names
- `employee_list` - Employee information (including FOS/foremen)
- `gis_facility` - Facility codes and names

**Junction/Association Tables**:
- `dblarv_treatment_catchbasin` - Links treatments to catch basins (many-to-many)

**Weather/Environmental Tables**:
- `nws_precip_site_history` - NWS precipitation history data

**Cattail-Specific Tables**:
- `cattail_pctcomplete_base` - Cattail inspection goals by facility/zone

---

## Table Organization

### By Data Type

| Category | Current Table | Archive Table | Purpose |
|----------|---------------|---------------|---------|
| **Larvae Inspections** | `dblarv_insptrt_current` | `dblarv_insptrt_archive` | Ground and air site inspections and treatments |
| **Adult Surveillance** | `dbadult_insp_current` | `dbadult_insp_archive` | Adult mosquito trap inspections (SUCO, CO2, etc.) |
| **Adult Species Data** | `dbadult_species_current` | `dbadult_species_archive` | Species-level counts from adult trap inspections |
| **Virus Testing** | `dbvirus_pool_test` | N/A | Virus test results for mosquito pools |
| **Virus Pool Data** | `dbvirus_pool` | N/A | Pool metadata (size, species, trap association) |

---

## Current vs Archive Tables

### Important Concepts

1. **Year Boundary**: Archive tables contain data from **previous years only**. Current tables contain **current year** data.

2. **UNION ALL Pattern**: Most apps query both tables together to get complete historical data:
   ```sql
   SELECT * FROM dblarv_insptrt_current
   UNION ALL
   SELECT * FROM dblarv_insptrt_archive
   ```

3. **Identical Schema**: Current and archive table pairs have the exact same column structure.

4. **Always Use Archive for History**: When analyzing historical trends or past dates, ALWAYS include archive tables via UNION ALL.

---

## Core Data Tables

### Larvae Inspection/Treatment Tables

#### `dblarv_insptrt_current` / `dblarv_insptrt_archive`

**Purpose**: Records all larvae inspection and treatment events for breeding sites

**Key Columns**:
- `sitecode` (TEXT) - Unique site identifier, first 7 chars match `gis_sectcode.sectcode`
- `inspdate` (DATE) - Date of inspection or treatment
- `action` (TEXT) - Action code:
  - `'1'` - Ground inspect AND treat
  - `'2'` - Ground inspection only
  - `'3'` - Ground treatment
  - `'4'` - Air inspection (inspected on ground for air treatment later)
  - `'9'` - Cattail inspection
  - `'A'` - Aerial (AIR) treatment
  - `'D'` - Drone treatment
  - Others exist for various operational codes
- `matcode` (TEXT) - Material/product code used for treatment (NULL for inspections)
- `wet` (TEXT) - Water depth indicator: numeric value (as text) for depth, or `'A'` for flooded
- `numdip` (INTEGER) - Number of larvae found PER dip
- `acres` (NUMERIC) - Acres treated or inspected
- `zone` (TEXT) - Priority zone: `'1'` (P1) or `'2'` (P2) - **Note**: May be NULL or inconsistent, prefer `gis_sectcode.zone`
- `facility` (TEXT) - Facility code (Sr, Sj, N, E, MO, etc.) - **Note**: Prefer `gis_sectcode.facility` for consistency

**Used By**: 
- Ground Prehatch Progress
- Inspections Coverage
- Drone Treatment
- Catch Basin Status
- Structural Treatment
- Cattail Inspections
- Cattail Treatments
- Air Sites Simple
- Control Efficacy

**Common Filters**:
- Action codes for different operations (`action = '3'` for ground treatments, `action = '2'` for inspections only, `action = '1'` for inspect and treat)
- Date ranges for time-based analysis
- Material codes for specific treatment types
- Wet indicator for water depth assessment

**Important Notes**:
- **Missing zone/facility data**: Not all records have `zone` or `facility`. Always join with `gis_sectcode` for complete coverage.
- **Cattail operations**: Use `action = '9'` for inspections, `action IN ('3', 'A')` for treatments
- **Prehatch treatments**: Join with `mattype_list_targetdose` where `prehatch = 'prehatch'`

---

### Adult Mosquito Surveillance Tables

#### `dbadult_insp_current` / `dbadult_insp_archive`

**Purpose**: Records adult mosquito trap inspections (SUCO, CO2 traps, light traps, etc.)

**Key Columns**:
- `ainspecnum` (TEXT) - Unique adult inspection number (primary key)
- `sampnum_yr` (TEXT) - Sample number for the year (links to virus pool data)
- `sitecode` (TEXT) - Site location code
- `inspdate` (DATE) - Date of trap inspection
- `survtype` (TEXT) - Surveillance type code:
  - `'4'` - Elevated CO2 trap
  - `'5'` - Gravid trap
  - `'6'` - CDC overnight trap
  - `'7'` - SUCO (aspirator)
  - Others for various trap types
- `facility` (TEXT) - Facility code
- `loc_code` (TEXT) - Location code for Monday night trap network
- `network_type` (TEXT) - Network type indicator (for Monday night traps)
- `x` (NUMERIC) - Longitude coordinate
- `y` (NUMERIC) - Latitude coordinate
- `geometry` (GEOMETRY) - PostGIS geometry point

**Used By**:
- Trap Surveillance Test
- SUCO History
- Mosquito Monitoring
- Mosquito Surveillance Map

**Common Filters**:
- `survtype IN ('4', '5', '6')` - Specific trap types
- `survtype = '7'` - SUCO (aspirator) inspections only
- Date ranges for temporal analysis
- Facility for geographic filtering

**Join Patterns**:
```sql
-- Join with species data
LEFT JOIN dbadult_species_current s ON t.ainspecnum = s.ainspecnum

-- Join with Monday night network geometry
LEFT JOIN loc_mondaynight_active a ON t.loc_code = a.loc_code
LEFT JOIN loc_mondaynight n ON n.loc_code = a.loc_code
WHERE a.enddate IS NULL

-- Use COALESCE for geometry
CASE WHEN network_type IS NOT NULL THEN loc_mondaynight.geom 
     ELSE dbadult_insp.geometry END
```

---

#### `dbadult_species_current` / `dbadult_species_archive`

**Purpose**: Species-level mosquito counts from adult trap inspections

**Key Columns**:
- `ainspecnum` (TEXT) - Links to `dbadult_insp_current/archive.ainspecnum`
- `spp` (INTEGER) - Species code (links to `lookup_specieslist.sppcode`)
- `cnt` (INTEGER) - Count of mosquitoes of this species

**Used By**:
- Trap Surveillance Test
- SUCO History
- Mosquito Monitoring

**Join Pattern**:
```sql
-- Get species counts with names
FROM dbadult_insp_current i
LEFT JOIN dbadult_species_current s ON i.ainspecnum = s.ainspecnum
LEFT JOIN lookup_specieslist l ON s.spp = l.sppcode
```

**Important Notes**:
- One-to-many relationship: Each `ainspecnum` can have multiple species records
- Species codes are integers that map to species names via `lookup_specieslist`
- Use `relationship = "many-to-many"` in dplyr joins to avoid warnings

---

### Virus Testing Tables

#### `dbvirus_pool_test`

**Purpose**: Virus test results for mosquito pools (no current/archive split)

**Key Columns**:
- `id` (INTEGER) - Unique test ID
- `poolnum` (TEXT) - Pool number (links to `dbvirus_pool.poolnum`)
- `date` (DATE) - Date of virus test
- `result` (TEXT) - Test result: `'Pos'` (positive), `'Neg'` (negative), `'Inconclusive'`
- `target` (TEXT) - Virus target: `'WNV'` (West Nile Virus), `'EEEV'`, `'LACV'`, etc.
- `status` (TEXT) - Test status
- `method` (TEXT) - Testing method used

**Used By**:
- Trap Surveillance Test
- Control Efficacy (potentially)

**Common Filters**:
- `target = 'WNV'` - West Nile Virus testing only
- `result = 'Pos'` - Positive results only
- Date ranges (typically last 90 days)

---

#### `dbvirus_pool`

**Purpose**: Metadata about mosquito pools submitted for virus testing

**Key Columns**:
- `poolnum` (TEXT) - Unique pool identifier (primary key)
- `sampnum_yr` (TEXT) - Links to `dbadult_insp_current/archive.sampnum_yr`
- `spp_code` (TEXT) - Species code (stored as TEXT, not integer)
- `count` (INTEGER) - Number of mosquitoes in the pool

**Join Pattern**:
```sql
-- Get pool tests with trap inspection data
FROM dbvirus_pool_test t
LEFT JOIN dbvirus_pool p ON t.poolnum = p.poolnum
LEFT JOIN dbadult_insp_current c ON c.sampnum_yr = p.sampnum_yr
```

**Important Notes**:
- `spp_code` is TEXT type (unlike `dbadult_species` where `spp` is INTEGER)
- Multiple pools can come from the same trap inspection (`sampnum_yr`)
- Pool size (`count`) is used in MLE (Maximum Likelihood Estimation) calculations

---

## Location Tables

### `loc_breeding_sites`

**Purpose**: Master list of all breeding sites (ground, air, drone, catch basins, structures, cattails)

**Key Columns**:
- `sitecode` (TEXT) - Unique site identifier (primary key)
- `air_gnd` (TEXT) - Site type: `'A'` (air) or `'G'` (ground)
- `priority` (TEXT) - Priority level: `'RED'`, `'YELLOW'`, `'BLUE'`, `'GREEN'`, `'PURPLE'`
- `drone` (TEXT) - Drone eligible: `'Y'` (yes), `'M'` (maybe), `'N'` (no), or NULL (not eligible)
- `prehatch` (TEXT) - Prehatch site indicator: `'Y'` or NULL
- `acres` (NUMERIC) - Site acreage
- `p1_totsitecount` (INTEGER) - P1 site count goal (cattails)
- `p2_totsitecount` (INTEGER) - P2 site count goal (cattails)
- `status_udw` (TEXT) - Catch basin wet/dry status: `'W'` (wet), `'D'` (dry)
- `enddate` (DATE) - Site closure date (NULL = active site)

**Used By**:
- All apps that work with breeding sites
- Ground Prehatch Progress
- Drone Treatment
- Inspections Coverage
- Catch Basin Status
- Cattail Inspections
- Cattail Treatments
- Section Cards

**Critical Filtering**:
```sql
WHERE enddate IS NULL  -- Only active sites
```

**Common Filters**:
- `air_gnd` for site type
- `drone = 'Y'` for drone-eligible sites
- `prehatch = 'Y'` for prehatch sites (spring season)
- `priority` for priority-based filtering
- `status_udw = 'W'` for wet catch basins

**Join Pattern**:
```sql
FROM loc_breeding_sites b
LEFT JOIN gis_sectcode sc ON LEFT(b.sitecode, 7) = sc.sectcode
WHERE b.enddate IS NULL
```

---

### `loc_cxstruct`

**Purpose**: Catch basin and structure-specific location data

**Key Columns**:
- `sitecode` (TEXT) - Unique site identifier (links to `loc_breeding_sites`)
- `sectcode` (TEXT) - Section code (7 characters, links to `gis_sectcode`)
- `zone` (TEXT) - Zone indicator - **Note**: Not always populated, prefer `gis_sectcode.zone`
- `structure_type` (TEXT) - Type of structure
- `enddate` (DATE) - Structure closure date (NULL = active)

**Used By**:
- Structural Treatment
- Catch Basin Status

**Join Pattern**:
```sql
FROM loc_cxstruct loc
LEFT JOIN gis_sectcode gis ON loc.sectcode = gis.sectcode
WHERE loc.enddate IS NULL
```

**Important Notes**:
- Zone coverage from `loc_cxstruct.zone` is only ~78%
- Always join with `gis_sectcode` for 100% zone coverage
- Must filter `enddate IS NULL` to get active structures only

---

### `loc_harborage`

**Purpose**: Adult mosquito harborage site definitions (SUCO locations)

**Key Columns**:
- `sitecode` (TEXT) - Harborage site code
- `name` (TEXT) - Site name/description
- `startdate` (DATE) - Date site became active
- `enddate` (DATE) - Date site closed (NULL = currently active)

**Used By**:
- SUCO History

**Join Pattern with Date Filtering**:
```sql
LEFT JOIN loc_harborage h ON s.sitecode = h.sitecode
  AND s.inspdate >= h.startdate 
  AND (h.enddate IS NULL OR s.inspdate <= h.enddate)
```

**Important Notes**:
- Use temporal join logic (inspdate between startdate and enddate)
- Sites can be active (`enddate IS NULL`) or historical

---

### `loc_mondaynight` / `loc_mondaynight_active`

**Purpose**: Monday night trap network locations with geometry

**Key Tables**:
- `loc_mondaynight` - All network locations with geometry
- `loc_mondaynight_active` - Active network locations only

**Key Columns**:
- `loc_code` (TEXT) - Location code (links to `dbadult_insp.loc_code`)
- `geom` (GEOMETRY) - PostGIS point geometry
- `enddate` (DATE) - Only in `_active` table (NULL = currently active)

**Used By**:
- Trap Surveillance Test

**Join Pattern**:
```sql
-- Get active network locations with geometry
FROM loc_mondaynight_active a
LEFT JOIN loc_mondaynight n ON n.loc_code = a.loc_code
WHERE a.enddate IS NULL

-- Use network geometry when available, else use inspection geometry
CASE WHEN c.network_type IS NOT NULL THEN l.geom
     ELSE c.geometry END AS geometry
```

---

### `loc_catchbasin`

**Purpose**: Catch basin location data - storm drain structures that can breed mosquitoes

**Key Columns**:
- `gid` (INTEGER) - Primary key, unique catch basin identifier
- `sitecode` (TEXT) - Site code (10+ characters, first 7 chars = sectcode)
- `facility` (TEXT) - Facility code
- `status_udw` (TEXT) - Water status code: `'W'` (Wet), `'D'` (Dry), `'U'` (Unknown)
- `lettergrp` (TEXT) - sub section of section
- `enddate` (DATE) - Date site was deactivated (NULL = currently active)
- `the_geom` (GEOMETRY) - PostGIS point geometry

**Used By**:
- Catch Basin Status

**Join Pattern**:
```sql
-- Get catch basins with zone/facility from gis_sectcode
FROM loc_catchbasin cb
LEFT JOIN gis_sectcode sc ON LEFT(cb.sitecode, 7) = sc.sectcode
WHERE cb.enddate IS NULL AND cb.lettergrp <> 'Z' AND cb.status_udw = 'W'

-- Join with treatments via junction table
JOIN dblarv_treatment_catchbasin jt ON cb.gid = jt.catchbasin_id
JOIN dblarv_insptrt_current trt ON jt.treatment_id = trt.pkey_pg
```

**Important Notes**:
- Use `gid` (not sitecode) to join with `dblarv_treatment_catchbasin`
- Filter `enddate IS NULL` for active basins only
- Filter `lettergrp <> 'Z'` to exclude inactive/special basins
- `status_udw = 'W'` identifies wet (larvicidal) basins

---

## Lookup/Reference Tables

### `gis_sectcode`

**Purpose**: Geographic section codes with facility, FOS area, and zone assignments (authoritative source)

**Key Columns**:
- `sectcode` (TEXT) - 7-character section code (primary key)
- `facility` (TEXT) - Facility code: `'Sr'`, `'Sj'`, `'N'`, `'E'`, `'MO'`
- `fosarea` (TEXT) - Field Operations Supervisor area code (4-digit, links to `employee_list.emp_num`)
- `zone` (TEXT) - Priority zone: `'1'` (P1), `'2'` (P2)

**Used By**: **All apps** - This is the authoritative source for facility, FOS area, and zone

**Standard Join Pattern**:
```sql
-- For breeding sites (sitecode is 10+ chars)
LEFT JOIN gis_sectcode sc ON LEFT(sitecode, 7) = sc.sectcode

-- For inspections (sitecode is 10+ chars)  
LEFT JOIN gis_sectcode sc ON LEFT(i.sitecode, 7) = sc.sectcode

-- For structures (sectcode is already 7 chars)
LEFT JOIN gis_sectcode gis ON loc.sectcode = gis.sectcode
```

**Critical Importance**:
- **100% coverage** for zone, facility, and FOS area
- **Always use this table** instead of relying on zone/facility from other tables
- Many other tables have incomplete or inconsistent zone/facility data

**Common Issues Avoided**:
- Zone assignment bugs (see November 2025 bug fix in README)
- Missing facility codes
- Inconsistent FOS area assignments

---

### `mattype_list_targetdose`

**Purpose**: Material/product codes with treatment parameters

**Key Columns**:
- `matcode` (TEXT) - Material code (primary key, links to `dblarv_insptrt.matcode`)
- `material_name` (TEXT) - Product name
- `effect_days` (INTEGER) - Days treatment remains effective
- `prehatch` (TEXT) - Prehatch indicator: `'prehatch'`, `'briquet'`, or NULL
- `cattail` (BOOLEAN) - Is this for cattail treatment?

**Used By**:
- Ground Prehatch Progress (prehatch materials)
- Structural Treatment (structure materials)
- Cattail Treatments (cattail materials)
- Drone Treatment (effect_days)
- All treatment tracking apps

**Join Pattern**:
```sql
LEFT JOIN mattype_list_targetdose mat ON trt.matcode = mat.matcode

-- Filter for prehatch materials
WHERE mat.prehatch = 'prehatch'

-- Filter for cattail materials
WHERE mat.cattail = TRUE

-- Filter for structure materials
WHERE mat.structure = TRUE
```

**Important Calculations**:
- Treatment expiration: `inspdate + effect_days`
- Active treatment: `current_date <= (inspdate + effect_days)`

---

### `lookup_specieslist`

**Purpose**: Mosquito species reference data

**Key Columns**:
- `sppcode` (INTEGER) - Species code (primary key)
- `genus` (TEXT) - Genus name (e.g., `'Aedes'`, `'Culex'`, `'Anopheles'`)
- `species` (TEXT) - Species name
- `common_name` (TEXT) - Common name

**Used By**:
- SUCO History
- Trap Surveillance Test
- Mosquito Monitoring
- Any app displaying species names

**Join Pattern**:
```sql
-- Join species codes to get names
LEFT JOIN lookup_specieslist l ON s.spp = l.sppcode
```

**Important Notes**:
- `dbadult_species.spp` is INTEGER type
- `dbvirus_pool.spp_code` is TEXT type (need to cast)
- Enhanced species mapping available in `shared/db_helpers.R`

---

### `employee_list`

**Purpose**: Employee information including Field Operations Supervisors (foremen)

**Key Columns**:
- `emp_num` (TEXT) - Employee number (4-digit, links to `gis_sectcode.fosarea`)
- `shortname` (TEXT) - Employee short name/initials
- `facility` (TEXT) - Home facility
- `emp_type` (TEXT) - Employee type (e.g., `'FieldSuper'` for FOS)
- `active` (BOOLEAN) - Is employee currently active

**Used By**:
- Apps with FOS filtering
- Structural Treatment
- Ground Prehatch Progress
- Drone Treatment

**Join Pattern**:
```sql
-- Get foreman names for filtering UI
SELECT emp_num, shortname, facility
FROM employee_list 
WHERE emp_type = 'FieldSuper' AND active = true
  AND facility IN ('N','E','MO','Sr','Sj','Wm','Wp')

-- Join with gis_sectcode to get foreman for a section
LEFT JOIN gis_sectcode sc ON LEFT(sitecode, 7) = sc.sectcode
LEFT JOIN employee_list emp ON sc.fosarea = emp.emp_num
```

---

## Junction/Association Tables

### `dblarv_treatment_catchbasin`

**Purpose**: Junction table linking catch basin treatments to specific catch basins (many-to-many relationship)

**Key Columns**:
- `treatment_id` (INTEGER) - Links to `dblarv_insptrt_current.pkey_pg` (treatment record)
- `catchbasin_id` (INTEGER) - Links to `loc_catchbasin.gid` (catch basin site)
- `status` (TEXT) - Treatment status: `'T'` (Treated), `'S'` (Skipped), or NULL

**Used By**:
- Catch Basin Status

**Join Pattern**:
```sql
-- Get treatments applied to catch basins
FROM dblarv_insptrt_current trt
JOIN dblarv_treatment_catchbasin jt ON trt.pkey_pg = jt.treatment_id
JOIN loc_catchbasin cb ON jt.catchbasin_id = cb.gid

-- Get latest treatment for each catch basin (DISTINCT ON pattern)
SELECT DISTINCT ON (cb.gid) 
  cb.gid, cb.sitecode, trt.inspdate, trt.matcode
FROM loc_catchbasin cb
JOIN dblarv_treatment_catchbasin jt ON cb.gid = jt.catchbasin_id
JOIN dblarv_insptrt_current trt ON jt.treatment_id = trt.pkey_pg
ORDER BY cb.gid, trt.inspdate DESC, trt.insptime DESC
```

**Important Notes**:
- One treatment record can apply to multiple catch basins
- Use this table to link `pkey_pg` (treatment) to `gid` (catch basin)
- Status field tracks planned vs completed treatments

---

## Lab Sample & Species Tables

### `dblarv_sample_current` / `dblarv_sample_archive`

**Purpose**: Lab sample metadata for larval mosquito identification

**Key Columns**:
- `sampnum_yr` (TEXT) - Primary key, sample number with year (e.g., `'12345_25'` where 25 = 2025)
- `redblue` (TEXT) - Red bug presence indicator: `'R'` (Red bugs present), `'B'` (Blue bugs only)
- `missing` (BOOLEAN) - Sample was missing/lost (`TRUE`/`FALSE`)
- `form_type` (TEXT) - Sample form type: `'AIR'`, `'GROUND'`, etc.

**Used By**:
- AIR Sites Simple (red bug tracking)
- Control Efficacy (checkback species analysis)

**Join Pattern**:
```sql
-- Get sample data with species counts
FROM dblarv_sample_current samp
LEFT JOIN dblarv_species_current spp ON samp.sampnum_yr = spp.sampnum_yr

-- For AIR samples only
WHERE samp.form_type = 'AIR' AND samp.missing = FALSE

-- Identify red bugs
SELECT sampnum_yr,
       CASE WHEN redblue = 'R' THEN 1 ELSE 0 END as has_red_bugs
FROM dblarv_sample_current
```

**Current/Archive Pattern**:
```sql
-- Union pattern for historical data
SELECT * FROM dblarv_sample_current WHERE inspdate >= '2020-01-01'
UNION ALL
SELECT * FROM dblarv_sample_archive WHERE inspdate >= '2020-01-01'
```

---

### `dblarv_species_current` / `dblarv_species_archive`

**Purpose**: Species-level larval mosquito counts from lab samples

**Key Columns**:
- `sampnum_yr` (TEXT) - Links to `dblarv_sample_current/archive.sampnum_yr`
- `spp` (TEXT) - Species code (links to `lookup_specieslist.sppcode`)
- `per` (INTEGER) - Count/percentage of this species in sample

**Used By**:
- Control Efficacy (checkback species analysis)

**Join Pattern**:
```sql
-- Get species with names for samples
FROM dblarv_species_current spp
LEFT JOIN lookup_specieslist spec ON spp.spp = CAST(spec.sppcode AS VARCHAR)
LEFT JOIN dblarv_sample_current samp ON spp.sampnum_yr = samp.sampnum_yr

-- Aggregate species for display
SELECT sampnum_yr,
       STRING_AGG(CONCAT(genus, ' ', species, ' (', per, '%)'), ', ') as species_summary
FROM dblarv_species_current
GROUP BY sampnum_yr
```

**Important Notes**:
- One-to-many relationship: Each sample can have multiple species records
- Species codes in this table are TEXT type (unlike `dbadult_species` which uses INTEGER)

---

## Weather/Environmental Tables

### `nws_precip_site_history`

**Purpose**: National Weather Service precipitation history data for rainfall tracking

**Key Columns**:
- `sitecode` (TEXT) - NWS station site code (links to loc_breeding_sites)
- `date` (DATE) - Observation date
- `precip` (NUMERIC) - Precipitation amount (inches)

**Used By**:
- Red AIR Legacy (rainfall criteria for AIR status calculations)

**Join Pattern**:
```sql
-- Get rainfall for last 14 days
SELECT SUM(precip) as total_precip_14d
FROM nws_precip_site_history
WHERE sitecode = 'SITECODE_VALUE'
  AND date >= CURRENT_DATE - INTERVAL '14 days'
  AND date < CURRENT_DATE

-- Get rainfall for specific date range
SELECT date, precip
FROM nws_precip_site_history
WHERE sitecode = 'SITECODE_VALUE'
  AND date BETWEEN '2025-01-01' AND '2025-12-31'
ORDER BY date
```

**Important Notes**:
- Used for environmental triggers in Red AIR status algorithm
- Typically aggregate by site over time windows (7-day, 14-day, 30-day)
- Different NWS sites cover different geographic areas

---

## Cattail-Specific Tables

### `cattail_pctcomplete_base`

**Purpose**: Cattail inspection goal/target data by facility and zone

**Key Columns**:
- `facility` (TEXT) - Facility code
- `p1_totsitecount` (INTEGER) - Total site count goal for Priority 1 (Zone 1)
- `p2_totsitecount` (INTEGER) - Total site count goal for Priority 2 (Zone 2)

**Used By**:
- Cattail Inspections (progress tracking)

**Join Pattern**:
```sql
-- Get goals for progress calculation
FROM cattail_pctcomplete_base goals
WHERE facility = 'FACILITY_CODE'

-- Calculate percent complete
SELECT facility,
       actual_inspections / p1_totsitecount * 100 as p1_pct_complete,
       actual_inspections / p2_totsitecount * 100 as p2_pct_complete
FROM cattail_pctcomplete_base
```

**Important Notes**:
- Static goal/target table (not frequently updated)
- Separate goals for Zone 1 (P1) and Zone 2 (P2)
- Used as denominator for percentage calculations

---

## GIS/Facility Tables

### `gis_facility`

**Purpose**: Facility reference data with abbreviations and full names

**Key Columns**:
- `abbrv` (TEXT) - Facility abbreviation (primary key): `'N'`, `'E'`, `'MO'`, `'Sr'`, `'Sj'`, `'Wm'`, `'Wp'`
- `name` (TEXT) - Full facility name

**Used By**:
- Catch Basin Status (facility filtering)
- Multiple apps for facility lookups

**Join Pattern**:
```sql
-- Get facility names for display
SELECT abbrv, name FROM gis_facility ORDER BY abbrv

-- Cross join for generating all facility/section combinations
FROM gis_facility g
CROSS JOIN gis_sectcode sc
WHERE g.abbrv IN ('N','E','MO','Sr','Sj','Wm','Wp')
```

**Important Notes**:
- Small lookup table (7-8 rows)
- Used for UI dropdowns and facility name display
- `abbrv` matches the `facility` column in other tables

---

## Application-Specific Table Usage

This section shows which tables each Shiny app primarily uses. This is useful for understanding app dependencies and data sources.

| Application | Core Tables | Description |
|-------------|-------------|-------------|
| **AIR Sites Simple** | `dblarv_insptrt_*`, `dblarv_sample_*`, `loc_breeding_sites`, `mattype_list_targetdose` | AIR inspection sites with lab sample tracking and red bug detection |
| **Catch Basin Status** | `dblarv_insptrt_*`, `loc_catchbasin`, `dblarv_treatment_catchbasin`, `mattype_list_targetdose`, `gis_facility`, `gis_sectcode` | Catch basin treatment tracking with expiration status |
| **Cattail Inspections** | `dblarv_insptrt_*`, `loc_breeding_sites`, `cattail_pctcomplete_base`, `gis_sectcode` | Cattail marsh inspection progress (action='9') |
| **Cattail Treatments** | `dblarv_insptrt_*`, `loc_breeding_sites`, `mattype_list_targetdose`, `gis_sectcode` | Cattail treatment tracking with material filtering |
| **Control Efficacy** | `dblarv_insptrt_*`, `dblarv_sample_*`, `dblarv_species_*`, `lookup_specieslist`, `gis_sectcode` | AIR treatment checkback analysis with species identification |
| **Drone** | `dblarv_insptrt_*`, `loc_breeding_sites`, `mattype_list_targetdose`, `gis_sectcode` | Drone application site tracking and treatment history |
| **Ground Prehatch Progress** | `dblarv_insptrt_*`, `loc_breeding_sites`, `mattype_list_targetdose`, `gis_sectcode` | Prehatch treatment progress by zone (mattype filtering) |
| **Inspections** | `dblarv_insptrt_*`, `loc_breeding_sites`, `gis_sectcode` | General breeding site inspection tracking |
| **Mosquito Monitoring** | `dbadult_insp_*`, `dbadult_species_*`, `lookup_specieslist` | Adult trap monitoring and species analysis |
| **Mosquito Surveillance Map** | `dbadult_insp_*`, `loc_mondaynight` | Geographic display of trap network (spatial app) |
| **Red AIR Legacy** | `dblarv_insptrt_*`, `loc_breeding_sites`, `nws_precip_site_history`, `gis_sectcode` | AIR site status based on rainfall and inspection history |
| **Section Cards** | `loc_breeding_sites`, `gis_sectcode` | Section-based site listings for field printing |
| **Structural Treatment** | `dblarv_insptrt_*`, `loc_cxstruct`, `mattype_list_targetdose`, `employee_list`, `gis_sectcode` | Structure treatment tracking (not catch basins) |
| **SUCO History** | `dbadult_insp_*`, `dbadult_species_*`, `loc_harborage`, `lookup_specieslist` | SUCO trap history and species trends (survtype='7') |
| **Trap Surveillance Test** | `dbadult_insp_*`, `dbadult_species_*`, `dbvirus_pool`, `dbvirus_pool_test`, `loc_mondaynight*` | Vector index calculations with virus testing integration |

**Notes**:
- `*` indicates both `_current` and `_archive` tables are used
- Apps typically also use `shared/db_helpers.R` for database connections
- Most apps join with `gis_sectcode` for zone/facility/FOS filtering

---

**Join Pattern**:
```sql
-- Link FOS area to employee names
LEFT JOIN employee_list e ON gis.fosarea = e.emp_num
```

**Important for Filtering**:
- User-facing filters show `shortname` (initials)
- Database queries use `emp_num` (matches `gis_sectcode.fosarea`)

---

## Geometry/GIS Tables

### PostGIS Geometry Columns

Several tables contain PostGIS geometry columns for spatial analysis:

| Table | Geometry Column | Type | SRID | Purpose |
|-------|----------------|------|------|---------|
| `dbadult_insp_current/archive` | `geometry` | POINT | Varies | Trap location coordinates |
| `loc_mondaynight` | `geom` | POINT | Varies | Network trap locations |
| `gis_sectcode` | `geom` (if exists) | POLYGON | Varies | Section boundaries |

**Common Geometry Operations**:
```sql
-- Transform to WGS84 (lat/lon)
ST_X(ST_Transform(geometry, 4326)) as lon
ST_Y(ST_Transform(geometry, 4326)) as lat

-- Check for valid geometry
WHERE geometry IS NOT NULL

-- Distance calculations (for k-NN)
ST_Distance(geom1, geom2)
```

---

## Common Join Patterns

### 1. Breeding Site with Section Data
```sql
FROM loc_breeding_sites b
LEFT JOIN gis_sectcode sc ON LEFT(b.sitecode, 7) = sc.sectcode
WHERE b.enddate IS NULL
```

### 2. Inspection with Treatment Material
```sql
FROM dblarv_insptrt_current i
LEFT JOIN mattype_list_targetdose m ON i.matcode = m.matcode
LEFT JOIN gis_sectcode sc ON LEFT(i.sitecode, 7) = sc.sectcode
```

### 3. Adult Trap with Species Data
```sql
FROM dbadult_insp_current t
LEFT JOIN dbadult_species_current s ON t.ainspecnum = s.ainspecnum
LEFT JOIN lookup_specieslist l ON s.spp = l.sppcode
```

### 4. Virus Testing with Trap Data
```sql
FROM dbvirus_pool_test t
LEFT JOIN dbvirus_pool p ON t.poolnum = p.poolnum
LEFT JOIN dbadult_insp_current c ON c.sampnum_yr = p.sampnum_yr
```

### 5. Structure Treatment with Location
```sql
FROM dblarv_insptrt_current trt
LEFT JOIN mattype_list_targetdose mat ON trt.matcode = mat.matcode
LEFT JOIN loc_cxstruct loc ON trt.sitecode = loc.sitecode
LEFT JOIN gis_sectcode gis ON loc.sectcode = gis.sectcode
```

### 6. Current + Archive Union Pattern
```sql
-- Get complete inspection history
SELECT * FROM dblarv_insptrt_current
WHERE inspdate >= '2024-01-01'
UNION ALL
SELECT * FROM dblarv_insptrt_archive
WHERE inspdate >= '2015-01-01' AND inspdate < '2024-01-01'
```

### 7. Monday Night Network Geometry
```sql
-- Use network geometry when available
LEFT JOIN loc_mondaynight_active a ON c.loc_code = a.loc_code
LEFT JOIN loc_mondaynight n ON n.loc_code = a.loc_code
WHERE a.enddate IS NULL

-- Select appropriate geometry
CASE WHEN c.network_type IS NOT NULL THEN n.geom
     ELSE c.geometry END AS geom
```

---

## Table Relationships

### Entity Relationship Overview

```
loc_breeding_sites (sitecode)
    ├─→ dblarv_insptrt_current/archive (sitecode)
    │       └─→ mattype_list_targetdose (matcode)
    └─→ gis_sectcode (LEFT(sitecode,7) = sectcode)
            └─→ employee_list (fosarea = emp_num)

dbadult_insp_current/archive (ainspecnum)
    ├─→ dbadult_species_current/archive (ainspecnum)
    │       └─→ lookup_specieslist (spp = sppcode)
    ├─→ dbvirus_pool (sampnum_yr)
    │       └─→ dbvirus_pool_test (poolnum)
    └─→ loc_mondaynight (loc_code)
            └─→ loc_mondaynight_active (loc_code)

loc_cxstruct (sectcode)
    └─→ gis_sectcode (sectcode)
```

### Key Relationships

1. **Breeding Site → Inspections/Treatments**: One-to-many
   - One site can have many inspection/treatment records
   - Join on `sitecode`

2. **Inspection → Species Data**: One-to-many
   - One adult trap inspection can have multiple species records
   - Join on `ainspecnum`

3. **Inspection → Virus Pools**: One-to-many
   - One trap inspection (`sampnum_yr`) can produce multiple pools
   - Join on `sampnum_yr`

4. **Pool → Test Results**: One-to-many
   - One pool can have multiple test results (re-testing)
   - Join on `poolnum`

5. **Any Site/Inspection → Section**: Many-to-one
   - Many sites/inspections belong to one geographic section
   - Join on `LEFT(sitecode, 7) = sectcode`

6. **Section → Employee (FOS)**: Many-to-one
   - Many sections assigned to one FOS
   - Join on `fosarea = emp_num`

---
## Application-Specific Table Usage

### Quick Reference: Which Apps Use Which Tables

| Application | Primary Tables | Key Join Pattern |
|------------|---------------|------------------|
| **Ground Prehatch Progress** | `dblarv_insptrt_*`, `loc_breeding_sites`, `mattype_list_targetdose` | Site → Inspection → Material (prehatch) |
| **Inspections Coverage** | `dblarv_insptrt_*`, `loc_breeding_sites`, `gis_sectcode` | Site → Inspections (all actions) |
| **Drone Treatment** | `dblarv_insptrt_*`, `loc_breeding_sites` | Site (drone='Y') → Treatments |
| **Catch Basin Status** | `dblarv_insptrt_*`, `loc_breeding_sites` | Site (status_udw='W') → Treatments |
| **Structural Treatment** | `dblarv_insptrt_*`, `loc_cxstruct`, `mattype_list_targetdose` | Structure → Treatment → Material |
| **SUCO History** | `dbadult_insp_*`, `dbadult_species_*`, `loc_harborage` | Trap → Species (survtype='7') |
| **Trap Surveillance** | `dbadult_insp_*`, `dbadult_species_*`, `dbvirus_*` | Trap → Species + Virus Pools/Tests |
| **Cattail Inspections** | `dblarv_insptrt_*`, `loc_breeding_sites` | Site → Inspection (action='9') |
| **Cattail Treatments** | `dblarv_insptrt_*`, `loc_breeding_sites`, `mattype_list_targetdose` | Site → Treatment (cattail materials) |
| **Control Efficacy** | `dblarv_insptrt_*`, `gis_sectcode` | Air Treatment → Checkback Inspection |
| **Section Cards** | `loc_breeding_sites`, `gis_sectcode` | Site → Section (for printing) |

---

---

## Additional Resources

- **Connection Pooling**: See `shared/db_pool.R` for efficient database connections
- **Helper Functions**: See `shared/db_helpers.R` for common lookup functions
- **App-Specific Notes**: Each app has a `NOTES.md` file with detailed query examples
- **Color Themes**: See `shared/color_themes.R` for facility/FOS color mappings

---

## Document Version

**Last Updated**: December 2025
**Maintained By**: MMCD Development Team

For questions or corrections, please refer to the repository: [https://github.com/mmcdmn/mmcd_metrics](https://github.com/mmcdmn/mmcd_metrics)
