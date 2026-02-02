# Overview Dashboard Framework

This folder contains the unified overview dashboard framework for MMCD metrics.
The system is designed with a **single source of truth** approach - you only need
to edit ONE file to add or remove metrics.

## Architecture

```
apps/overview/
├── metric_registry.R     #  SINGLE SOURCE OF TRUTH - Edit this to add/remove metrics
├── data_functions.R      # Data loading for current progress
├── display_functions.R   # Chart creation (plotly)
├── historical_functions.R # Historical data loading
├── dynamic_ui.R          # Auto-generates UI from registry
├── dynamic_server.R      # Auto-generates server logic from registry
├── district/
│   └── app.R             # ~60 lines - just calls build_overview_ui/server
└── facilities/
    └── app.R             # ~80 lines - same pattern with URL param handling
```

## Adding a New Metric

To add a new metric (e.g., "larvicide_ponds"):

### Step 1: Add to the Registry

Open `metric_registry.R` and add an entry to `get_metric_registry()`:

```r
larvicide_ponds = list(
  id = "larvicide_ponds",
  display_name = "Larvicide Ponds",
  short_name = "Ponds",
  icon = "water",
  y_label = "Ponds",
  bg_color = "#9b59b6",
  app_folder = "larvicide_ponds",      # Folder containing data_functions.R
  has_acres = FALSE,                   # Set TRUE if metrics use acres
  historical_enabled = TRUE,           # Set TRUE to show historical charts
  display_metric = "treatments",       # "treatments" or "treatment_acres"
  filter_info = HTML("<b>Filters Applied:</b><br>..."),
  load_params = list(expiring_days = 7)
)
```

### Step 2: Create the Data Functions

Create `apps/larvicide_ponds/data_functions.R` with a `load_raw_data()` function:

```r
load_raw_data <- function(analysis_date, expiring_days = 7, 
                          include_archive = FALSE, 
                          start_year = NULL, end_year = NULL) {
  # Query your database and return:
  list(
    sites = sites_df,        # Must have: site_id, facility, zone
    treatments = trt_df,     # Must have: treatment_date, site_id, facility, zone
    total_count = nrow(sites_df)
  )
}
```

### Step 3: Done!

That's it! The framework will automatically:
- Create UI chart panels for the new metric
- Create stat boxes with the correct color/icon
- Load data using your data_functions.R
- Render charts with the correct labels
- Include historical charts if enabled

## Removing a Metric

Simply delete or comment out the entry in `get_metric_registry()`.

## Chart Types

The framework supports two chart types for historical data:

- **District Overview**: Weekly line charts (single line, all MMCD combined)
- **Facilities Overview**: Yearly stacked bars (grouped by facility)

This is configured in `get_overview_config()` in metric_registry.R.

## Key Functions

| Function | Purpose |
|----------|---------|
| `get_metric_registry()` | Returns all metric configurations |
| `get_active_metrics()` | Returns list of metric IDs to display |
| `get_historical_metrics()` | Returns metrics with historical enabled |
| `build_overview_ui()` | Generates complete UI from registry |
| `build_overview_server()` | Generates complete server logic from registry |

## Benefits of This Approach

1. **Single Source of Truth**: One place to add/remove metrics
2. **No Hardcoding**: Apps don't know about specific metrics
3. **Consistent UI**: All metrics get the same styling automatically
4. **Easy Maintenance**: Change the registry, everything updates
5. **Less Code**: ~60 lines per app instead of ~500+

---

## URL-Based Navigation System

The unified overview app (`unified/app.R`) uses URL parameters for drill-down navigation,
allowing users to share specific views and use browser back/forward buttons.

### URL Structure

```
/overview/?view=<view>&metric=<metrics>&zone=<zone>&facility=<facility>&date=<date>
```

### Parameters

| Parameter | Values | Default | Description |
|-----------|--------|---------|-------------|
| `view` | `district`, `facility`, `fos`, `metric_detail`, `site_detail` | `district` | Aggregation level |
| `metric` | `all` or comma-separated IDs | `all` | Which metrics to show |
| `zone` | `1`, `2`, `1,2`, `separate` | `1,2` | Zone filter |
| `facility` | Facility code or `all` | `all` | Facility filter |
| `fos` | FOS name or `all` | `all` | Field Operations Supervisor filter |
| `site_id` | Site ID or `null` | `null` | Individual site filter |
| `date` | `YYYY-MM-DD` | Today | Analysis date |
| `expiring` | `1-30` | `7` | Expiring days |
| `theme` | `MMCD`, `IBM`, `Wong`, etc. | `MMCD` | Color theme |

### URL Examples

```bash
# District overview - all metrics, all zones
/overview/?view=district

# Facility view for P1 only
/overview/?view=facility&zone=1

# Drill into drone metric only, P1
/overview/?view=metric_detail&metric=drone&zone=1

# Multiple metrics, specific facility
/overview/?view=facility&metric=drone,ground_prehatch&zone=1&facility=SLP

# Full URL with all params
/overview/?view=facility&metric=drone&zone=1&date=2026-02-02&expiring=7&theme=MMCD
```

### Drill-Down Flow

```
District (all metrics, by zone)
  |
  +-> Click P1 bar -> Facility (all metrics, P1 only)
                        |
                        +-> Click facility bar -> FOS (all metrics, P1, selected facility)
                                                    |
                                                    +-> Click FOS bar -> Metric Detail (single metric, P1, facility, FOS)
                                                                          |
                                                                          +-> Click site bar -> Site Detail (single site detail)
```

### Key Files

| File | Purpose |
|------|---------|
| `url_router.R` | URL parsing, building, and navigation helpers |
| `unified/app.R` | Single app that handles all views |

### URL Router Functions

| Function | Purpose |
|----------|---------|
| `parse_url_params()` | Parse query string into structured params |
| `build_drill_down_url()` | Build URL for navigation |
| `build_click_drill_down_url()` | Build URL from bar click event |
| `navigate_to_url()` | Navigate using JavaScript pushState |
| `build_back_url()` | Build URL for back navigation |
| `generate_breadcrumb_ui()` | Create breadcrumb navigation UI |

---