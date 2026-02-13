# Air Inspection Checklist

## Purpose
Checklist view of RED air sites showing whether each site has been inspected within a configurable lookback period. Organized by FOS area and section for easy field verification.

## Features
- Filter by Facility and FOS (FOS choices filter by selected facility)
- Adjustable lookback days (1-7, default 2)
- Show only unfinished (un-inspected) sites
- Employee number and dip count shown for completed inspections
- Red/Blue bug status badges from lab sample results
- URL parameter support: `?facility=N&fos=Rosa&lookback=3`

## URL Parameters
| Parameter | Description | Example |
|-----------|-------------|---------|
| `facility` | Facility short code | `N`, `E`, `Sr` |
| `fos` | FOS shortname(s), comma-separated | `Rosa`, `Rosa,Abbey` |
| `lookback` | Lookback days (1-7) | `3` |

## Data Source
- **Sites**: `loc_breeding_sites` (air_gnd='A', priority='RED')
- **Sections/FOS**: `gis_sectcode` (fosarea maps to employee_list.emp_num)
- **Inspections**: `dblarv_insptrt_current` (actions '2','4')
- **Lab Samples**: `dblarv_sample_current` (redblue column for bug type)

## File Structure
| File | Purpose |
|------|---------|
| `app.R` | Main application entry point, server logic, URL parsing |
| `data_functions.R` | Database queries and data processing |
| `display_functions.R` | Checklist HTML generation and summary boxes |
| `ui_helper.R` | Shiny UI definition |
