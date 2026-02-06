# Section Cards Generator - Technical Notes

## Overview
This utility app generates printable field cards for FOS (Field Operations Supervisor) personnel to use during mosquito breeding site inspections. The cards display site characteristics and provide empty tables for manual data entry in the field.

**Key Features**:
- **Printable Cards**: 6 cards per page, optimized for standard printing
- **Configurable Content**: Select which site fields to display in card header vs data table
- **Filtering**: Filter by facility, zone, FOS area, section, site type, priority
- **Section Grouping**: Option to group cards by section (prevents mixing sections on pages)
- **Download**: Export as standalone HTML file for printing

## Data Sources

### Core Database Tables

#### 1. `loc_breeding_sites` - Breeding Site Registry
**Key Columns**:
- `sitecode` - Unique site identifier (ALWAYS displayed prominently)
- `priority` - Site priority (RED, YELLOW, GREEN, BLUE)
- `acres` - Site acres (capacity)
- `type` - Breeding site type
- `air_gnd` - Treatment type:
  - **'A'** - Air site
  - **'G'** - Ground site
  - **'D'** - Drone site
- `culex` - Culex mosquito presence ('Y' flag)
- `spr_aedes` - Spring Aedes presence ('Y' flag)
- `prehatch` - Prehatch site designation
- `remarks` - Site notes/comments
- `drone` - Drone designation codes ('Y', 'M', 'C')
- `sample` - Sample site designation
- `facility` - Facility code (from breeding site record)
- `enddate` - Site termination date (NULL = active site)

**Critical Filter**: `enddate IS NULL` - Only active sites included

**Usage**: Primary source for all site characteristics displayed on cards

#### 2. `gis_sectcode` - Geographic Section Mapping
**Key Columns**:
- `sectcode` - 7-character section code (e.g., '191819-', '191102N')
- `zone` - Zone designation:
  - **'1'** - P1 (Primary zone)
  - **'2'** - P2 (Secondary zone)
- `facility` - Facility name (Sr, Nr, Er, Wr, etc.) - AUTHORITATIVE
- `fosarea` - FOS area code (4-digit like "0203", "1904")

**Join Pattern**: `LEFT JOIN gis_sectcode g ON g.sectcode = LEFT(b.sitecode, 7)`

**Usage**: Provides facility, zone, and FOS area assignments for filtering and display

#### 3. `employee_list` - FOS Personnel Data
**Key Columns**:
- `emp_num` - Employee number (formats as 4-digit to match fosarea)
- `shortname` - Foreman name (e.g., "Andrew M.")
- `facility` - Facility assignment
- `emp_type` - Employee type (filter: 'FieldSuper')
- `active` - Active status (filter: TRUE)

**FOS Code Format**: `sprintf("%04d", emp_num)` - Convert to 4-digit string

**Usage**: Maps fosarea codes to readable names for display on cards

### Data Collection Strategy

**Single Query Approach**:
```sql
SELECT 
  b.sitecode,
  b.priority,
  b.acres,
  b.type,
  b.air_gnd,
  b.culex,
  b.spr_aedes,
  b.prehatch,
  b.remarks,
  b.drone,
  b.sample,
  b.facility AS site_facility,
  g.sectcode AS section,
  g.zone,
  g.facility,
  g.fosarea
FROM public.loc_breeding_sites b
LEFT JOIN public.gis_sectcode g ON g.sectcode = LEFT(b.sitecode, 7)
WHERE b.enddate IS NULL
ORDER BY b.sitecode
```

**Note**: The query retrieves ALL active sites; filtering is applied client-side in R for responsive UI updates.

## App Functionality

### Card Configuration Panel

**Filters**:
- **Facility**: Filter sites by facility (Sr, Nr, Er, etc.)
- **Zone**: Filter by P1, P2, or All
- **FOS Area**: Filter by specific FOS area (cascades based on facility)
- **Section**: Filter by specific section (cascades based on facility and FOS)
- **Priority**: Filter by priority level (RED, YELLOW, GREEN, BLUE)
- **Site Type**: Filter by Air, Ground, Drone, or All
- **Include Unassigned Sites**: Option to include sites with no FOS area assigned

**Title Fields** (displayed in card header):
- Select which fields appear below sitecode in card header
- Options: priority, acres, facility, zone, fosarea, section, type, air_gnd
- Use Ctrl+Click (Windows) or Cmd+Click (Mac) for multi-select

**Table Columns** (data entry table):
- Select which columns to include in the empty data table
- Options: date, wet_pct, emp_num, num_dip, sample_num, amt, mat
- Columns appear as empty fields for manual field entry
- Use sortable interface to reorder columns (drag handles)

**Table Rows**:
- Number of empty rows in each card's data table (1-10, default 5)
- More rows = more space for multiple inspections/treatments

**Split Cards by Section**:
- When enabled: Each section starts on a new page (no mixing)
- When disabled: Cards fill pages regardless of section boundaries

### Generate Cards Workflow

1. **Click Generate Cards**: Loads complete breeding site data from database
2. **Apply Filters**: Client-side filtering based on selected criteria
3. **Render Cards**: Generates HTML with 6 cards per page
4. **Display**: Shows cards in browser with print-optimized styling

### Print/Export Options

**Browser Print** (Ctrl+P / Cmd+P):
- Prints directly from browser
- Sidebar and buttons automatically hidden via CSS `@media print`
- Cards maintain layout and spacing for field use

**Download HTML**:
- Creates standalone HTML file with embedded CSS
- Can be opened and printed later without app
- Filename format: `section-cards-YYYYMMDD.html`

## Card Layout

### Card Structure (each card ~1/6 page)

**Header Section** (gray background):
- **Sitecode** (large, bold, always displayed)
- **Selected Info Fields** (smaller text, multi-column grid layout):
  - Priority with color coding (RED=#FF0000, YELLOW=#FFD700, GREEN=#00FF00, BLUE=#0000FF)
  - Acres, Type, Air/Ground designation
  - Facility name (full name from lookup)
  - FOS area (name from lookup, not just code)
  - Zone, Section
  - Special flags (Culex=green background, Spring Aedes=yellow)
  - Remarks (full width if selected)

**Data Table** (white background):
- Column headers based on selected table fields
- Empty rows for manual field data entry
- Borders and spacing optimized for handwriting
- Standard columns:
  - **Date**: Inspection/treatment date
  - **Wet %**: Percentage of site that is wet
  - **Emp #**: Employee number
  - **#/Dip**: Larvae count per dip
  - **Sample #**: Lab sample number
  - **Amt**: Material amount applied
  - **Mat**: Material type code

### Page Layout

**Standard Mode** (split_by_section = FALSE):
- 6 cards per page
- 2 columns × 3 rows
- Page breaks after every 6 cards
- Sections may be mixed on pages

**Section-Grouped Mode** (split_by_section = TRUE):
- 6 cards per page within each section
- Each section starts on new page
- No mixing of sections across page boundaries
- Useful for organizing field work by section

## Function Reference

### Data Functions (data_functions.R)

**`get_filter_options(facility_filter, fosarea_filter)`**
- Retrieves unique values for dropdown filters
- Applies cascading filters (facility → FOS → section)
- Returns list with facilities, sections, fosarea_list
- Lightweight query for responsive UI

**`get_breeding_sites_with_sections()`**
- Main data loading function
- Retrieves all active breeding sites with section info
- Uses correct JOIN pattern: `LEFT(sitecode, 7) = sectcode`
- Returns complete dataset for client-side filtering

### Display Functions (display_functions.R)

**`generate_section_cards_html(data, title_fields, table_fields, num_rows, split_by_section)`**
- Main HTML generation function
- Creates complete card layout with CSS
- Handles page breaks and sectioning
- Maps facility codes and FOS codes to names
- Returns HTML string ready for display

**`generate_card_html(row, title_fields, table_fields, num_rows, field_labels, facility_map, fos_map)`**
- Generates HTML for single card
- Applies priority color coding
- Formats special fields (Culex, Spring Aedes)
- Creates empty data table rows
- Returns HTML string for one card

### UI Helper Functions (ui_helper.R)

**`create_field_selector()`**
- Creates complete configuration panel
- Includes all filter inputs
- Creates title field multi-select
- Creates table column selector with reordering
- Returns tagList of UI elements

## File Structure

```
apps/section-cards/
├── app.R                    # Main application UI and server logic
├── data_functions.R         # Data loading and filter options
├── display_functions.R      # HTML generation and card layout
├── ui_helper.R              # UI component creation
└── NOTES.md                 # This file - technical documentation
```

## Notes and Considerations

### Design Philosophy
- **Field-First**: Optimized for printing and manual field data entry
- **Configurable**: FOS personnel can customize cards for their workflow
- **Lightweight**: Fast filtering and rendering for responsive UX

### Print Optimization
- Cards sized to fit 6 per standard letter page
- Font sizes optimized for readability (sitecode=16px, data table=10-12px)
- Black borders for clear field boundaries
- Page breaks prevent card splitting across pages

### Data Entry Fields
- Standard columns cover common inspection/treatment data
- Empty rows allow multiple entries per site (repeat inspections)
- Field workers fill in actual data during site visits
- Cards become physical field records

### Filtering Strategy
- Load complete dataset once (on Generate Cards click)
- Apply filters client-side for instant UI updates
- Cascading filters (facility → FOS → section) improve usability
- "Unassigned" option handles sites with missing FOS assignments

### Section Grouping
- Useful for FOS area organization (each FOS works specific sections)
- Prevents partial section splits across pages
- May result in partially filled last page per section

### Future Enhancements
- Add QR codes for mobile data entry integration
- Include site maps/coordinates for navigation
- Add historical inspection summaries
- Export to PDF directly (currently HTML → browser PDF)

### Known Limitations
- Print quality depends on browser rendering
- No direct PDF generation (requires browser print-to-PDF)
- Client-side filtering may be slow for very large datasets (thousands of sites)
- No mobile-optimized layout (desktop/print focused)
