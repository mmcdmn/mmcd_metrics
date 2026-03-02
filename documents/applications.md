# Applications

## Inspections Coverage
- **Path**: `/inspections/`
- **Purpose**: Analyze inspection coverage for all sites across facilities
- **Documentation**: [Technical Notes](../apps/inspections/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with tabbed interface
  - **`data_functions.R`**: Database queries and coverage calculations
  - **`display_functions.R`**: Visualization and chart generation
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Site Analytics**: Total sites, wet frequency analysis, and summary statistics
  - **Larvae Threshold Analysis**: Sites exceeding larvae counts with frequency tracking
  - **Coverage Gaps**: Identify sites with inspection gaps or never inspected
  - Uses both current and archive records for complete history

## Ground Prehatch Progress
- **Path**: `/ground_prehatch_progress/`
- **Purpose**: Track and analyze ground prehatch treatment progress and performance
- **Documentation**: [Technical Notes](../apps/ground_prehatch_progress/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with clean tabbed interface
  - **`data_functions.R`**: Database queries, progress calculations, and performance metrics
  - **`display_functions.R`**: Chart generation, progress visualizations, and dashboard displays
  - **`historical_functions.R`**: Historical trend analysis functions
  - **`ui_helpers.R`**: UI component functions and reusable interface elements
- **Features**:
  - **Progress Overview Tab**: Sites with active/expiring treatments aggregated by facility, FOS, or section
  - **Detailed View Tab**: Individual site details with treatment status and download
  - **Map Tab**: Interactive map with color-coded treatment status markers
  - **Historical Analysis Tab**: Multi-year trends (sites, treatments, or acres by year/week)
  - Prehatch-specific filtering with dry site detection

## Air Sites Simple
- **Path**: `/air_sites_simple/`
- **Purpose**: Monitor air site status and treatment pipeline (no rainfall tracking)
- **Documentation**: [Technical Notes](../apps/air_sites_simple/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with tabbed interface
  - **`data_functions.R`**: Database queries, progress calculations, and performance metrics
  - **`display_functions.R`**: Chart generation, progress visualizations, and dashboard displays
  - **`historical_functions.R`**: Historical trend analysis functions
  - **`ui_helper.R`**: UI component functions and reusable interface elements
- **Features**:
  - Interactive map with color-coded site status dots
  - Treatment lifecycle: Needs Inspection → Under Threshold → Needs Treatment → Active Treatment
  - BTI material effect days tracking
  - Larvae threshold monitoring
  - Red bug detection tracking

## Drone Treatment
- **Path**: `/drone/`
- **Documentation**: [Technical Notes](../apps/drone/NOTES.md)
- **Purpose**: Comprehensive drone treatment tracking with real-time progress and historical analysis
- **Modular Structure**:
  - **`app.R`**: Main application logic with multi-tab interface
  - **`data_functions.R`**: Database queries and treatment calculations
  - **`historical_functions.R`**: Historical trend analysis and data processing functions
  - **`display_functions.R`**: Visualization and chart generation
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Current Progress Tab**: Active treatments and expiring treatments summary
  - **Map Tab**: Leaflet map with color-coded markers and popup details
  - **Historical Trends Tab**: Multi-year trends analyzing sites, treatments, or acres treated
  - **Site Statistics Tab**: Average site size, largest sites, smallest sites analysis
  - Prehatch toggle filter for seasonal analysis

## Air Treatment Checkbacks
- **Path**: `/control_efficacy/`
- **Purpose**: Air treatment checkback analysis and efficacy monitoring
- **Documentation**: [Technical Notes](../apps/control_efficacy/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with tabbed interface
  - **`data_functions.R`**: Database queries and treatment data processing
  - **`display_functions.R`**: Visualization and chart generation
  - **`checkback_functions.R`**: Checkback analysis and efficacy calculations
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Checkback Progress Tab**: Brood completion progress and outstanding checkbacks tracking
  - **Status Tables Tab**: Brood status overview and detailed checkback results with larvae counts
  - **Control Efficacy Tab**: Pre-treatment vs post-treatment dip count efficacy analysis
  - Species composition in checkback samples
  - Treatment-to-checkback timing validation

## Catch Basin Status
- **Path**: `/catch_basin_status/`
- **Purpose**: Monitor catch basin treatment status across facilities
- **Documentation**: [Technical Notes](../apps/catch_basin_status/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with tabbed interface
  - **`data_functions.R`**: Database queries and status calculations
  - **`display_functions.R`**: Chart generation and progress visualizations
  - **`historical_functions.R`**: Historical trend analysis functions
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Status Overview Tab**: High-level summary metrics and treatment status charts
  - **Detailed View Tab**: Granular site-by-site breakdown with last treatment dates
  - **Historical Analysis Tab**: Multi-year treatment trends and patterns
  - Focus on wet catch basins only (status_udw = 'W')
  - Treatment expiration tracking with days until expiring filter

## Structural Treatment
- **Path**: `/struct_trt/`
- **Purpose**: Comprehensive structure treatment tracking with current progress and historical analysis
- **Documentation**: [Technical Notes](../apps/struct_trt/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with dual-tab interface
  - **`data_functions.R`**: Database queries, treatment calculations, and data processing
  - **`display_functions.R`**: Chart generation, progress visualizations, and historical analysis displays
  - **`historical_functions.R`**: Historical trend analysis functions
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Progress Tab**: Real-time active structure treatment monitoring
  - **History Tab**: Multi-year historical trends and patterns
  - Customizable treatment effect_days parameter for expiration calculations
  - Structure type categorization and filtering
  - Proportion of structures under active treatment tracking

## SUCO History
- **Path**: `/suco_history/`
- **Purpose**: Surveillance Count (SUCO) historical analysis dashboard
- **Documentation**: [Technical Notes](../apps/suco_history/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main UI and server logic with tabbed interface
  - **`data_functions.R`**: Database queries, species mapping, spatial data processing, and top locations analysis
  - **`display_functions.R`**: Interactive maps with leaflet, trend charts, and plotly visualizations
  - **`ui_helpers.R`**: UI component functions and interface helpers
- **Features**:
  - **Graph Tab**: Time series visualization of SUCO trends over time
  - **Map Tab**: Spatial distribution with dynamic marker sizing based on counts
  - **Summary Table Tab**: Aggregated statistics grouped by facility or FOS
  - **Detailed Samples Tab**: Raw SUCO inspection records with full details
  - **Top Locations Tab**: Rankings by visit frequency or species 

## Mosquito Surveillance Map
- **Path**: `/mosquito_surveillance_map/`
- **Purpose**: Interactive geographic mapping of mosquito surveillance data
- **Features**:
  - Species filtering and surveillance type selection
  - Temporal analysis with date range controls
  - Visualize mosquito counts across monitoring locations
  - Note: Application takes time to load due to data volume

## Mosquito Monitoring
- **Path**: `/mosquito-monitoring/`
- **Purpose**: CO2 trap mosquito surveillance analysis
- **Features**: 
  - Species-specific analysis with 50+ mosquito species
  - Facility and zone comparisons
  - Interactive time series with hover tooltips

## Cattail Inspections
- **Path**: `/cattail_inspections/`
- **Purpose**: Track cattail inspection progress and site assessments
- **Documentation**: [Technical Notes](../apps/cattail_inspections/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with multi-tab interface
  - **`data_functions.R`**: Database queries, site calculations, and inspection data processing
  - **`display_functions.R`**: Chart generation, progress visualizations, and map displays
  - **`historical_functions.R`**: Historical trend analysis functions
  - **`planned_treatment_functions.R`**: Treatment planning functions
  - **`progress_functions.R`**: Progress tracking functions
  - **`ui_helper.R`**: UI component functions and interface helpers
- **Features**:
  - Progress tracking against goals with historical comparison
  - Single inspection per site counting (most recent only)
  - Site count goals by facility (p1_totsitecount, p2_totsitecount)
  - Wet/dry site status and larvae dip count tracking
  - Action code '9' filtering for cattail inspections only

## Cattail Treatments
- **Path**: `/cattail_treatments/`
- **Purpose**: Track cattail treatment applications and effectiveness with DOY-based inspection year logic
- **Documentation**: [Technical Notes](../apps/cattail_treatments/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic with multi-tab interface (Progress, Historical, Map)
  - **`data_functions.R`**: Database queries, treatment calculations, and data processing
  - **`display_functions.R`**: Chart generation with multiple chart types (line/bar/stacked), progress visualizations
  - **`historical_functions.R`**: Historical analysis with DOY-based inspection year calculation (Fall-Summer seasonal cycles)
  - **`ui_helper.R`**: UI component functions and reusable interface elements
- **Features**:
  - **Progress Tab**: Current treatment status with 3 chart types
  - **Historical Tab**: Multi-year trends using DOY-based inspection years
  - **Inspection Year Logic**: Fall (DOY 244-365) + Summer (DOY 135-213) seasonal cycles
  - Multiple display metrics: Sites Treated, % Treated, Sites Need Treatment
  - Chart type options: Line, grouped bar, stacked bar
  - Action '9' inspections + Action '3'/'A' treatments
  - Cattail material code filtering

## Trap Surveillance Test
- **Path**: `/trap_survillance_test/`
- **Purpose**: Prototype app to compute section vector index using k-nearest traps
- **Documentation**: [Technical Notes](../apps/trap_survillance_test/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic
  - **`data_functions.R`**: Data processing functions
  - **`display_functions.R`**: Visualization functions
  - **`mle_trap_based.R`**: MLE calculation functions using PooledInfRate package
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Trap-based MLE**: Maximum Likelihood Estimation for virus infection risk
  - **k-NN Distance-Weighted Averaging**: Spatial interpolation across sections
  - Population Index, MLE, and Vector Index calculations using PooledInfRate package
  - Interactive leaflet map with section polygons, trap markers, and Vector Index Areas
  - Unified SQL query with CTEs for optimal performance
  - Species filtering for targeted analysis

## Section Cards
- **Path**: `/section-cards/`
- **Purpose**: Demo application for section cards functionality
- **Documentation**: [Technical Notes](../apps/section-cards/NOTES.md)
- **Modular Structure**:
  - **`app.R`**: Main application logic
  - **`data_functions.R`**: Data processing functions
  - **`display_functions.R`**: Card generation and layout functions
  - **`ui_helper.R`**: UI component functions
- **Features**:
  - **Printable Field Cards**: 6 cards per page optimized for standard printing
  - **Configurable Content**: Select which site fields to display in header vs table
  - **Section Grouping**: Option to group cards by section (prevents mixing on pages)
  - **Download as HTML**: Export standalone file for printing
  - Active sites only (enddate IS NULL)

## Test Application
- **Path**: `/test-app/`
- **Purpose**: Admin utilities and testing application for system monitoring
- **Features**:
  - **System Overview**: Redis status, cache key counts, worker mode, platform information
  - **Cache Manager**: Regenerate and clear historical metrics, lookup tables, and tiered caches
  - **Runtime & Routing**: Load balancer status, worker load distribution, nginx access logs
  - **App Config**: Live config viewer for cache TTLs, color thresholds, wiki links
  - **Metric Registry**: View all registered metrics with cache status
  - **Color Reference**: Facility/FOS colors, status indicators, treatment plans, species, theme preview
