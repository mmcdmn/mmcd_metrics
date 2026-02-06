# How to Change the Color Theme for All Apps

## Current Configuration

**All apps currently use: MMCD theme (original colors)**

This is set automatically in `shared/db_helpers.R` when it loads.

## To Change Theme for ALL Apps

### Option 1: Edit db_helpers.R (Permanent Change)

1. Open: `shared/db_helpers.R`
2. Find this line (around line 47):
   ```r
   options(mmcd.color.theme = "MMCD")
   ```
3. Change "MMCD" to your desired theme:
   ```r
   options(mmcd.color.theme = "Wong")    # Color-blind friendly
   options(mmcd.color.theme = "IBM")     # IBM Design Language
   options(mmcd.color.theme = "Tol")     # Scientific visualization
   options(mmcd.color.theme = "Viridis") # Perceptually uniform
   options(mmcd.color.theme = "ColorBrewer") # Cartography-inspired
   ```
4. Save the file
5. Restart any running Shiny apps

**This changes the theme for ALL apps at once!**

### Option 2: Set in R Session (Temporary)

In your R console before running any app:
```r
options(mmcd.color.theme = "Wong")
shiny::runApp("apps/cattail_inspections")
```

### Option 3: Environment Variable (Advanced)

Set an environment variable in your `.Renviron` file:
```
MMCD_COLOR_THEME=Wong
```

Then update `db_helpers.R` line 47 to:
```r
options(mmcd.color.theme = Sys.getenv("MMCD_COLOR_THEME", "MMCD"))
```

## Available Themes

| Theme | Best For | Description |
|-------|----------|-------------|
| **MMCD** (default) | General use | Original MMCD colors - what you're using now |
| **Wong** | Accessibility | Color-blind friendly (8 distinct colors) |
| **IBM** | Professional | IBM Design Language - modern corporate |
| **Tol** | Scientific | Paul Tol's palette - clear and distinct |
| **Viridis** | Continuous data | Perceptually uniform - great for heatmaps |
| **ColorBrewer** | Maps | Cartography-inspired - proven for data viz |

## Testing Themes

Use the **test-app** to preview all themes and their colors:

```r
shiny::runApp("apps/test-app")
```

1. Use the dropdown at the top of the sidebar to switch themes
2. View how each theme affects:
   - Facility colors
   - Status colors (active, in_lab, needs_treatment, etc.)
   - Treatment plan colors
3. Check the "Theme Preview" tab to see all palette colors

## Important Notes

### Color Consistency
- **MMCD theme** = Exact original colors from before theme system
- Changing themes affects ALL apps simultaneously
- All apps load `db_helpers.R` which sets the default theme

### Status Colors (Critical for air_sites_simple)
MMCD theme uses these EXACT original colors:
- `active` (Active Treatment): #187018 (forest green)
- `in_lab` (Needs ID): #5841c0 (purple)
- `needs_treatment`: #FF0000 (red)

These are distinct and will not conflict.

### If Apps Show Wrong Colors
1. Check `shared/db_helpers.R` line ~47 for the theme setting
2. Make sure it says: `options(mmcd.color.theme = "MMCD")`
3. Restart the Shiny app
4. Check the R console for any warning messages

## Reverting to Original Colors

If anything goes wrong, simply set:
```r
options(mmcd.color.theme = "MMCD")
```

This is the default and matches your original implementation exactly.

## Questions?

- See `shared/COLOR_THEMES_README.md` for full documentation
- Test themes in `apps/test-app` before changing globally
- Contact MMCD dev team with any issues
