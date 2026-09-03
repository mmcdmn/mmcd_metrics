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
      indicators = c(good = "#16a34a", warning = "#eab308", alert = "#dc2626"),
      surface = c(bg = "#0f172a", panel = "#1e293b", border = "#334155",
                  text = "#e2e8f0", text_muted = "#cbd5e1",
                  link = "#60a5fa", neutral = "#64748b",
                  faint = "#94a3b8", text_strong = "#f1f5f9"),
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
      indicators = c(good = "#198038", warning = "#f1c21b", alert = "#fa4d56"),
      surface = c(bg = "#161616", panel = "#262626", border = "#393939",
                  text = "#f4f4f4", text_muted = "#c6c6c6",
                  link = "#78a9ff", neutral = "#6f6f6f",
                  faint = "#a8a8a8", text_strong = "#ffffff"),
      sequential_heat = c("#fcf4d6", "#fddc69", "#f1c21b", "#ff832b", "#fa4d56",
                          "#da1e28", "#a2191f", "#750e13", "#520408"),
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
      indicators = c(good = "#009E73", warning = "#E69F00", alert = "#D55E00"),
      surface = c(bg = "#11212B", panel = "#1B3140", border = "#2A4A5E",
                  text = "#EAF4F8", text_muted = "#B8D4E0",
                  link = "#56B4E9", neutral = "#5A7A8A",
                  faint = "#8FB0C0", text_strong = "#FFFFFF"),
      sequential_heat = c("#FFF7D6", "#F7E08A", "#F0E442", "#F5C242", "#E69F00",
                          "#E07B26", "#D55E00", "#A84700", "#7A3300"),
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
      indicators = c(good = "#117733", warning = "#DDCC77", alert = "#CC6677"),
      surface = c(bg = "#1A1526", panel = "#2A2338", border = "#3E3450",
                  text = "#EDE8F5", text_muted = "#C9BFDC",
                  link = "#88CCEE", neutral = "#6E6288",
                  faint = "#A99CC4", text_strong = "#FFFFFF"),
      sequential_heat = c("#FFFFE5", "#F7F0B9", "#DDCC77", "#E0B769", "#CC9944",
                          "#CC6677", "#AA4455", "#882255", "#661133"),
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
      indicators = c(good = "#35B779", warning = "#FDE724", alert = "#440154"),
      surface = c(bg = "#12122B", panel = "#1E2140", border = "#31385A",
                  text = "#E8EAF6", text_muted = "#C2C8E0",
                  link = "#6DCD59", neutral = "#4A5578",
                  faint = "#8892B8", text_strong = "#FFFFFF"),
      sequential_heat = c("#FDE724", "#B4DE2C", "#6DCD59", "#35B779", "#1F9E89",
                          "#26828E", "#31688E", "#482878", "#440154"),
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
      indicators = c(good = "#4DAF4A", warning = "#FF7F00", alert = "#E41A1C"),
      surface = c(bg = "#1A1A1A", panel = "#2B2B2B", border = "#404040",
                  text = "#F0F0F0", text_muted = "#C8C8C8",
                  link = "#4B94D6", neutral = "#6B6B6B",
                  faint = "#A0A0A0", text_strong = "#FFFFFF"),
      sequential_heat = c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c",
                          "#fc4e2a", "#e31a1c", "#bd0026", "#800026"),
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

#' Labeled theme choices for a selectInput
#'
#' Returns a named character vector suitable for `selectInput(choices = ...)`,
#' built from get_available_themes() so a newly added theme shows up in every
#' app's picker automatically. Previously all 14 pickers hardcoded their own
#' list, which is how the overview ended up silently missing ColorBrewer.
#'
#' @return Named character vector (label -> theme name)
get_theme_choices <- function() {
  themes <- get_available_themes()
  labels <- c(
    MMCD        = "MMCD (Default)",
    IBM         = "IBM Design",
    Wong        = "Color-Blind Friendly",
    Tol         = "Scientific (Color Blind Safe)",
    Viridis     = "Viridis",
    ColorBrewer = "ColorBrewer"
  )
  # Unlabeled themes fall back to their own name rather than dropping out.
  display <- ifelse(themes %in% names(labels), labels[themes], themes)
  stats::setNames(themes, display)
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
  # Blue for 5-Year Average, Active (green) for Current Year
  status <- get_theme_palette(theme)$status
  colors <- list(
    "5-Year Avg" = palette$primary[1],  # Blue
    "current_year" = unname(status["active"])  # Same as active treatment color
  )
  
  return(colors)
}

#' Convert a hex color to an rgba() CSS/plotly string
#'
#' @param hex Hex color (e.g. "#dc2626")
#' @param alpha Opacity 0-1
#' @return "rgba(r,g,b,alpha)" string
hex_to_rgba <- function(hex, alpha = 1) {
  if (is.null(hex) || length(hex) != 1 || is.na(hex) || !nzchar(hex)) {
    hex <- "#dc2626"
  }
  v <- tryCatch(col2rgb(hex)[, 1], error = function(e) c(220, 38, 38))
  sprintf("rgba(%d,%d,%d,%s)", v[1], v[2], v[3], format(alpha, trim = TRUE))
}

#' Pick a readable foreground color for a given background
#'
#' Value boxes paint text directly onto a metric/indicator color. Most of those
#' are dark enough for white text, but some are not — Viridis `warning`
#' (#FDE724) and Wong `#F0E442` are near-yellow and render white text unreadable.
#' Chooses via WCAG relative luminance rather than a hardcoded "#ffffff".
#'
#' @param bg_color Hex background color
#' @param dark Foreground to use on light backgrounds
#' @param light Foreground to use on dark backgrounds
#' @return Hex color string
contrast_text_color <- function(bg_color, dark = "#1a1a1a", light = "#ffffff") {
  if (is.null(bg_color) || length(bg_color) != 1 ||
      is.na(bg_color) || !nzchar(bg_color)) {
    return(light)
  }
  rgb_vals <- tryCatch(col2rgb(bg_color)[, 1] / 255, error = function(e) NULL)
  if (is.null(rgb_vals)) return(light)

  # WCAG 2.x relative luminance
  lin <- ifelse(rgb_vals <= 0.03928,
                rgb_vals / 12.92,
                ((rgb_vals + 0.055) / 1.055) ^ 2.4)
  luminance <- sum(c(0.2126, 0.7152, 0.0722) * lin)

  if (luminance > 0.45) dark else light
}

# NULL coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# EXPORT NOTE
# =============================================================================
# This file is sourced by db_helpers.R
# All functions are available to apps that source db_helpers.R
