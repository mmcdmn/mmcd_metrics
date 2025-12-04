# MMCD Color Theme System

## Overview

The MMCD Metrics applications now support multiple color themes/palettes. This system allows for consistent color schemes across all visualizations while providing flexibility for different use cases (accessibility, branding, etc.).

## Available Themes

### 1. **MMCD** (Default)
- **Description**: Custom MMCD palette - balanced and familiar
- **Best for**: General use, current standard across all apps
- **Colors**: Blue, Orange, Green, Red, Purple, Brown, Pink, Gray

### 2. **IBM**
- **Description**: IBM Design Language - modern and professional
- **Best for**: Professional presentations, corporate reports
- **Colors**: IBM Blue, Magenta, Green, Red, Purple, Cyan
- **Reference**: [IBM Design Language](https://www.ibm.com/design/language/)

### 3. **Wong**
- **Description**: Color-blind friendly palette by Bang Wong
- **Best for**: Maximum accessibility, scientific publications
- **Colors**: 8 carefully selected colors distinguishable by all types of color blindness
- **Reference**: Wong, B. (2011). "Points of view: Color blindness." Nature Methods 8, 441.

### 4. **Tol**
- **Description**: Paul Tol's color schemes for scientific visualization
- **Best for**: Scientific papers, data-heavy visualizations
- **Colors**: Indigo, Cyan, Teal, Green, Olive, Sand, Rose, Wine, Purple
- **Reference**: https://personal.sron.nl/~pault/

### 5. **Viridis**
- **Description**: Perceptually uniform color maps
- **Best for**: Continuous data, heat maps, sequential data
- **Colors**: Purple → Blue → Green → Yellow gradient
- **Properties**: Colorblind-safe, print-friendly, perceptually uniform

### 6. **ColorBrewer**
- **Description**: Cynthia Brewer's cartography-inspired palettes
- **Best for**: Maps, general data visualization
- **Colors**: Red, Blue, Green, Purple, Orange, Yellow, Brown, Pink, Gray
- **Reference**: https://colorbrewer2.org/

## Files

### color_themes.R
Contains all color palette definitions and theme management functions:
- `get_theme_palette(theme)` - Get colors for a specific theme
- `get_available_themes()` - List all available themes
- `get_theme_description(theme)` - Get description of a theme
- `generate_distinct_colors(n, theme)` - Generate N distinct colors from a theme

### db_helpers.R
Updated to support theme parameter in color functions:
- `get_facility_base_colors(theme = ...)` - Facility colors
- `get_status_colors(theme = ...)` - Status colors
- `get_status_color_map(theme = ...)` - Status name mapping
- `get_treatment_plan_colors(use_names, theme = ...)` - Treatment plan colors

## Usage

### Setting a Global Theme

```r
# Set theme globally for the session
options(mmcd.color.theme = "Wong")

# Now all color functions will use the Wong palette
facility_colors <- get_facility_base_colors()
status_colors <- get_status_colors()
```

### Using Theme Per Function Call

```r
# Get colors for specific theme without changing global setting
facility_colors_ibm <- get_facility_base_colors(theme = "IBM")
facility_colors_wong <- get_facility_base_colors(theme = "Wong")

# Compare different themes
status_colors_default <- get_status_colors(theme = "MMCD")
status_colors_accessible <- get_status_colors(theme = "Wong")
```

### In Shiny Apps

```r
# In UI - add theme selector
selectInput("color_theme", 
            "Color Theme:",
            choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
            selected = "MMCD")

# In Server - use reactive theme
server <- function(input, output, session) {
  current_theme <- reactive({ input$color_theme })
  
  # Set global option when theme changes
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
  
  # Use in visualizations
  output$myPlot <- renderPlot({
    colors <- get_facility_base_colors(theme = current_theme())
    # ... create plot with colors
  })
}
```

## Test Application

The `apps/test-app/` application demonstrates the theme system:
- View all color palettes for each theme
- Compare facility, status, and treatment colors across themes
- Preview theme palettes (primary, sequential, diverging)
- Test theme switching in real-time

To run:
```r
shiny::runApp("apps/test-app")
```

## Color Palette Structure

Each theme provides:

### Primary Colors
10 distinct colors for categorical data (facilities, categories, etc.)

### Facilities
Specific colors for each MMCD facility (AP, BD, BL, MT, NM, SH, SP, WB)

### Status Colors
Colors for status indicators:
- Complete (green tones)
- Incomplete (yellow/amber tones)
- Not Started (red tones)
- In Progress (blue tones)
- Pending (gray tones)

### Sequential Colors
9 colors for sequential/ordered data (light to dark)

### Diverging Colors
9 colors for diverging data (two contrasting colors meeting at neutral)

## Adding New Themes

To add a new theme, edit `shared/color_themes.R`:

1. Add theme to `get_theme_palette()` function:
```r
NewTheme = list(
  primary = c("#color1", "#color2", ...),
  facilities = c(AP = "#color1", BD = "#color2", ...),
  status = c(Complete = "#color1", Incomplete = "#color2", ...),
  sequential = c("#light", ..., "#dark"),
  diverging = c("#color1", ..., "#neutral", ..., "#color2")
)
```

2. Add description to `get_theme_description()`:
```r
NewTheme = "Description of your theme and when to use it"
```

3. Update `get_available_themes()` return value to include "NewTheme"

## Backwards Compatibility

All existing apps continue to work without modification:
- Default theme is "MMCD" (current implementation)
- All color functions work without theme parameter
- No breaking changes to existing APIs

## Recommendations

### For Accessibility
Use **Wong** or **Tol** themes - designed for color-blind accessibility

### For Professional Presentations
Use **IBM** theme - modern, corporate aesthetic

### For Scientific Publications
Use **Tol** or **Viridis** themes - proven for scientific visualization

### For Maps
Use **ColorBrewer** theme - designed specifically for cartography

### For General Use
Use **MMCD** theme (default) - familiar to all users

## Questions or Issues

Contact the MMCD development team or open an issue in the repository.
