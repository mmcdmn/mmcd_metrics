# =============================================================================
# MMCD METRICS - COLOR THEME DEFINITIONS
# =============================================================================
# This file contains all color palette/theme definitions used across MMCD apps.
# Color palettes are designed for data visualization accessibility and clarity.
#
# Supported themes:
#   - MMCD (default): Custom MMCD palette
#   - IBM: IBM Design Language colors
#   - Wong: Color-blind friendly palette by Bang Wong
#   - Tol: Paul Tol's color schemes for scientific visualization
#   - Viridis: Perceptually uniform color maps
#   - ColorBrewer: Cynthia Brewer's cartography palettes
# =============================================================================

# =============================================================================
# CORE COLOR PALETTES
# =============================================================================

#' Get Base Color Palette for a Theme
#' 
#' Returns a set of base colors for the specified theme. These are the
#' foundational colors used to generate all other color schemes.
#' 
#' @param theme Character. One of: "MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"
#' @return Named list with color vectors for different purposes
get_theme_palette <- function(theme = "MMCD") {
  
  palettes <- list(
    
    # MMCD Default Theme (Current Implementation)
    MMCD = list(
      primary = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"),
      facilities = c(
        E = "#1f77b4",   # East - Blue
        MO = "#ff7f0e",  # Main Office - Orange
        N = "#2ca02c",   # North - Green
        Sj = "#d62728",  # South Jordan - Red
        Sr = "#9467bd",  # South Rosemount - Purple
        Wm = "#8c564b",  # West Maple Grove - Brown
        Wp = "#e377c2"   # West Plymouth - Pink
      ),
      status = c(
        active = "#187018",         # Forest green for active/in-progress/treatment
        completed = "#4169E1",      # Royal blue for completed
        planned = "#fdb73e",        # Orange for planned/pending
        needs_action = "#FF4500",   # Red-orange for needs inspection
        in_lab = "#5841c0",         # Purple for lab processing
        needs_treatment = "#FF0000", # Pure red for needs treatment
        unknown = "#A9A9A9"         # Dark gray for unknown status
      ),
      sequential = c("#440154", "#482878", "#3E4A89", "#31688E", "#26828E", 
                     "#1F9E89", "#35B779", "#6DCD59", "#B4DE2C", "#FDE724"),  # viridis
      sequential_heat = c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c",
                          "#fc4e2a", "#e31a1c", "#bd0026", "#800026"),  # YlOrRd
      diverging = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf",
                    "#e0f3f8", "#abd9e9", "#74add1", "#4575b4")
    ),
    
    # IBM Design Language Colors
    IBM = list(
      primary = c("#0f62fe", "#ff7eb6", "#42be65", "#fa4d56", "#8a3ffc",
                  "#33b1ff", "#d12771", "#198038", "#ba4e00", "#8a3800"),
      facilities = c(
        E = "#0f62fe",   # East - IBM Blue 60
        MO = "#ff7eb6",  # Main Office - Magenta 40
        N = "#42be65",   # North - Green 50
        Sj = "#fa4d56",  # South Jordan - Red 50
        Sr = "#8a3ffc",  # South Rosemount - Purple 60
        Wm = "#d12771",  # West Maple Grove - Magenta 70
        Wp = "#6f6f6f"   # West Plymouth - Gray 60
      ),
      status = c(
        active = "#0f62fe",        # Blue 60 for active treatment
        completed = "#42be65",     # Green 50 for completed/inspected
        planned = "#878d96",       # Gray 50 for planned
        needs_action = "#f1c21b",  # Yellow 30 for needs action
        in_lab = "#8a3ffc",        # Purple 60 for lab/needs ID
        needs_treatment = "#fa4d56", # Red 50 for needs treatment
        unknown = "#a8a8a8"        # Gray for unknown
      ),
      sequential = c("#edf5ff", "#d0e2ff", "#a6c8ff", "#78a9ff", "#4589ff",
                     "#0f62fe", "#0043ce", "#002d9c", "#001d6c"),
      diverging = c("#da1e28", "#fa4d56", "#ff8389", "#ffb3b8", "#ffd7d9",
                    "#d0e2ff", "#a6c8ff", "#78a9ff", "#4589ff")
    ),
    
    # Wong Color-Blind Friendly Palette
    # Reference: Bang Wong (2011) Nature Methods
    # Note: Original palette includes black, but we avoid it for facilities where possible
    Wong = list(
      primary = c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                  "#0072B2", "#D55E00", "#CC79A7", "#000000"),
      facilities = c(
        E = "#0072B2",   # East - Blue
        MO = "#E69F00",  # Main Office - Orange
        N = "#009E73",   # North - Bluish Green
        Sj = "#D55E00",  # South Jordan - Vermillion (red-orange)
        Sr = "#CC79A7",  # South Rosemount - Reddish Purple
        Wm = "#56B4E9",  # West Maple Grove - Sky Blue
        Wp = "#F0E442"   # West Plymouth - Yellow
      ),
      status = c(
        active = "#0072B2",        # Blue for active treatment
        completed = "#009E73",     # Bluish Green for completed/inspected
        planned = "#CC79A7",       # Reddish Purple for planned
        needs_action = "#D55E00",  # Vermillion for needs action
        in_lab = "#E69F00",        # Orange for lab/needs ID
        needs_treatment = "#D55E00", # Vermillion for needs treatment
        unknown = "#999999"        # Gray for unknown
      ),
      sequential = c("#E8F4F8", "#C9E5F0", "#A2D4E8", "#7AC3E0", "#56B4E9",
                     "#3A9FD5", "#2685BF", "#1A6AA3", "#0072B2"),
      diverging = c("#D55E00", "#E07B26", "#EB984D", "#F5B574", "#FFD29B",
                    "#C9E5F0", "#A2D4E8", "#7AC3E0", "#0072B2")
    ),
    
    # Tol's Color Schemes
    Tol = list(
      primary = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
                  "#DDCC77", "#CC6677", "#882255", "#AA4499"),
      facilities = c(
        E = "#332288",   # East - Indigo
        MO = "#DDCC77",  # Main Office - Sand
        N = "#44AA99",   # North - Teal
        Sj = "#CC6677",  # South Jordan - Rose
        Sr = "#AA4499",  # South Rosemount - Purple
        Wm = "#882255",  # West Maple Grove - Wine
        Wp = "#88CCEE"   # West Plymouth - Cyan
      ),
      status = c(
        active = "#332288",        # Indigo for active treatment
        completed = "#117733",     # Green for completed/inspected
        planned = "#999933",       # Olive for planned
        needs_action = "#CC6677",  # Rose for needs action
        in_lab = "#88CCEE",        # Cyan for lab/needs ID
        needs_treatment = "#CC6677", # Rose for needs treatment
        unknown = "#BBBBBB"        # Gray for unknown
      ),
      sequential = c("#FFFFE5", "#F7FCB9", "#D9F0A3", "#ADDD8E", "#78C679",
                     "#41AB5D", "#238443", "#006837", "#004529"),
      diverging = c("#8E0152", "#C51B7D", "#DE77AE", "#F1B6DA", "#FDE0EF",
                    "#E6F5D0", "#B8E186", "#7FBC41", "#4D9221")
    ),
    
    # Viridis - Perceptually Uniform Sequential
    Viridis = list(
      primary = c("#440154", "#482878", "#3E4A89", "#31688E", "#26828E",
                  "#1F9E89", "#35B779", "#6DCD59", "#B4DE2C", "#FDE724"),
      facilities = c(
        E = "#3E4A89",   # East - Purple-Blue
        MO = "#FDE724",  # Main Office - Yellow
        N = "#35B779",   # North - Green
        Sj = "#B4DE2C",  # South Jordan - Yellow-Green
        Sr = "#440154",  # South Rosemount - Purple
        Wm = "#26828E",  # West Maple Grove - Teal
        Wp = "#6DCD59"   # West Plymouth - Light Green
      ),
      status = c(
        active = "#26828E",        # Teal for active treatment
        completed = "#35B779",     # Green for completed/inspected
        planned = "#31688E",       # Blue for planned
        needs_action = "#FDE724",  # Yellow for needs action
        in_lab = "#6DCD59",        # Light Green for lab/needs ID
        needs_treatment = "#440154", # Purple for needs treatment
        unknown = "#AAAAAA"        # Gray for unknown
      ),
      sequential = c("#440154", "#482878", "#3E4A89", "#31688E", "#26828E",
                     "#1F9E89", "#35B779", "#6DCD59", "#B4DE2C", "#FDE724"),
      diverging = c("#440154", "#482878", "#3E4A89", "#31688E", "#FFFFBF",
                    "#C7E9B4", "#7FBC41", "#35B779", "#1F9E89")
    ),
    
    # ColorBrewer - Set1 (Qualitative) with Extensions
    ColorBrewer = list(
      primary = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                  "#FFFF33", "#A65628", "#F781BF", "#999999"),
      facilities = c(
        E = "#377EB8",   # East - Blue
        MO = "#FF7F00",  # Main Office - Orange
        N = "#4DAF4A",   # North - Green
        Sj = "#E41A1C",  # South Jordan - Red
        Sr = "#984EA3",  # South Rosemount - Purple
        Wm = "#A65628",  # West Maple Grove - Brown
        Wp = "#F781BF"   # West Plymouth - Pink
      ),
      status = c(
        active = "#377EB8",        # Blue for active treatment
        completed = "#4DAF4A",     # Green for completed/inspected
        planned = "#999999",       # Gray for planned
        needs_action = "#FFFF33",  # Yellow for needs action
        in_lab = "#984EA3",        # Purple for lab/needs ID
        needs_treatment = "#E41A1C", # Red for needs treatment
        unknown = "#969696"        # Gray for unknown
      ),
      sequential = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59",
                     "#EF6548", "#D7301F", "#B30000", "#7F0000"),
      diverging = c("#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF",
                    "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850")
    )
  )
  
  # Validate theme
  if (!theme %in% names(palettes)) {
    warning(sprintf("Theme '%s' not found. Using 'MMCD' default.", theme))
    theme <- "MMCD"
  }
  
  return(palettes[[theme]])
}

#' Get Theme Names
#' 
#' Returns a list of all available theme names
#' 
#' @return Character vector of theme names
get_available_themes <- function() {
  return(c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"))
}

#' Get Theme Description
#' 
#' Returns a description of each theme
#' 
#' @param theme Character. Theme name
#' @return Character string with theme description
get_theme_description <- function(theme) {
  descriptions <- list(
    MMCD = "MMCD default color scheme - balanced and familiar",
    IBM = "IBM Design Language - modern and professional",
    Wong = "Color-blind friendly palette - maximum accessibility (8 colors)",
    Tol = "Paul Tol's scientific palette - clear and distinct",
    Viridis = "Perceptually uniform - excellent for continuous data",
    ColorBrewer = "Cartography-inspired - proven for data visualization"
  )
  
  return(descriptions[[theme]] %||% "No description available")
}

#' Generate Distinct Colors with Theme Support
#' 
#' Generates N distinct colors using the specified theme's color palette
#' 
#' @param n Integer. Number of colors to generate
#' @param theme Character. Theme name
#' @return Character vector of hex colors
generate_distinct_colors <- function(n, theme = "MMCD") {
  if (n <= 0) return(character(0))
  
  # Get theme palette
  palette <- get_theme_palette(theme)
  
  # If we need fewer colors than available in primary palette, use those
  if (n <= length(palette$primary)) {
    return(palette$primary[1:n])
  }
  
  # For more colors, use HSV space interpolation based on theme's primary colors
  # Extract hue, saturation, value from theme's first color
  base_rgb <- col2rgb(palette$primary[1]) / 255
  base_hsv <- rgb2hsv(base_rgb[1], base_rgb[2], base_rgb[3])
  
  # Generate evenly spaced hues
  hues <- seq(0, 1, length.out = n + 1)[1:n]
  
  # Use theme-appropriate saturation and value
  colors <- sapply(hues, function(h) {
    hsv(h = h, s = base_hsv[2], v = base_hsv[3])
  })
  
  return(colors)
}

#' Get Consistent Historical Comparison Colors
#' 
#' Returns standardized colors for historical comparison charts (5-Year Avg vs Current Year)
#' 
#' @param theme Character. Theme name (default "MMCD")
#' @return Named list with colors for "5-Year Avg" and current year
get_historical_comparison_colors <- function(theme = "MMCD") {
  palette <- get_theme_palette(theme)
  
  # Use consistent colors across all historical comparison charts
  # Blue for 5-Year Average, Orange for Current Year
  colors <- list(
    "5-Year Avg" = palette$primary[1],  # Blue
    "current_year" = palette$primary[2]  # Orange
  )
  
  return(colors)
}

# NULL coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# EXPORT NOTE
# =============================================================================
# This file is sourced by db_helpers.R
# All functions are available to apps that source db_helpers.R
