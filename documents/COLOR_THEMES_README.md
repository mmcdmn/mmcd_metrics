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
Specific colors for each MMCD facility (Sr, N, E, etc)

### Status Colors
Workflow-state colors. The keys are exactly:

`active`, `completed`, `planned`, `needs_action`, `in_lab`, `needs_treatment`, `unknown`

Read them with `get_status_colors(theme = ...)`.

### Indicator Colors
Health colors for value boxes: `good`, `warning`, `alert`.

These are separate from Status Colors: status describes *what stage* something
is in, indicators describe *how it is doing*. Read them with
`get_indicator_colors(theme = ...)` - never straight from the palette, because
that function also applies the config override described below.

Wong, Tol and Viridis deliberately avoid a red/green pair here; that is the
entire reason those palettes exist, and a red/green traffic light defeats them.

### Surface Colors
Panel chrome for dark-surfaced components (currently the FOS detail dashboard):

`bg`, `panel`, `border`, `text`, `text_muted`, `text_strong`, `link`, `neutral`, `faint`

### Sequential Colors
9-10 colors for sequential/ordered data (light to dark)

### Sequential Heat
9 colors, light to dark, for heat/abundance ramps (surveillance maps, trap
density). Sample with `colorRampPalette()` when you need a different number of
bins.

### Diverging Colors
9 colors for diverging data (two contrasting colors meeting at neutral)

## Overriding Indicator Colors Globally

`config/app_config.yaml` has a `thresholds.colors` block, commented out by
default. Uncommenting any of `good` / `warning` / `alert` pins that color across
**every** theme and disables theme-driven status coloring for it.

Leave it commented unless you specifically want one fixed set of status colors
everywhere. `tests/shared/test-indicator-colors.R` has a test that fails if the
override is silently left on.

## Adding New Themes

To add a new theme, edit `shared/color_themes.R`:

1. Add theme to `get_theme_palette()` function:
```r
NewTheme = list(
  indicators = c(good = "#color1", warning = "#color2", alert = "#color3"),
  surface = c(bg = "#color1", panel = "#color2", border = "#color3",
              text = "#color4", text_muted = "#color5",
              link = "#color6", neutral = "#color7",
              faint = "#color8", text_strong = "#color9"),
  primary = c("#color1", "#color2", ...),
  facilities = c(E = "#color1", MO = "#color2", N = ..., Sj = ...,
                 Sr = ..., Wm = ..., Wp = ...),
  status = c(active = "#color1", completed = "#color2", planned = ...,
             needs_action = ..., in_lab = ..., needs_treatment = ...,
             unknown = ...),
  sequential = c("#light", ..., "#dark"),
  sequential_heat = c("#light", ..., "#dark"),
  diverging = c("#color1", ..., "#neutral", ..., "#color2")
)
```

2. Add description to `get_theme_description()`:
```r
NewTheme = "Description of your theme and when to use it"
```

3. Update `get_available_themes()` return value to include "NewTheme"

Every dropdown builds its choices from `get_theme_choices()`, so a new theme
appears in all apps automatically - do not hardcode theme lists in app UI code.

`tests/shared/test-indicator-colors.R` checks that every theme defines
`indicators`, `surface` and `sequential_heat`, so a partially-added theme fails
the suite rather than breaking an app at runtime.

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
