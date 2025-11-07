# Trap Surveillance Vector Index - Technical Notes

## Overview
This app calculates a **Vector Index** for each section using a k-Nearest Neighbors (k-NN) inverse-distance weighted algorithm based on trap surveillance data.

## Data Sources

### Tables Used
- **`dbadult_insp_current`** - Current adult mosquito trap inspection records
  - Contains trap locations (x, y coordinates), inspection dates, facility, survey type
  - Multiple inspections per trap location over time
  
- **`dbadult_species_current`** - Species counts for each trap inspection
  - Links to inspections via `ainspecnum`
  - Contains species code (`spp`) and count (`cnt`)
  
- **`lookup_specieslist`** - Species lookup table
  - Maps species codes to genus/species names
  
- **`gis_sectcode`** - Section geometry polygons
  - Stored in EPSG:26915 (UTM Zone 15N)
  - Converted to EPSG:4326 (WGS84 lat/lon) for display

## Trap Types (Survey Types)
The app filters for specific trap types using the `survtype` column:
- **4** - Elevated CO2
- **5** - Gravid Trap  
- **6** - CO2 Overnight

Note: `survtype` is stored as `character varying` in the database, not integer.

## SQL Query Logic

### Step 1: Rank Trap Inspections
```sql
WITH ranked_traps AS (
  SELECT t.ainspecnum, t.facility, t.x, t.y, t.survtype, t.inspdate,
         ROW_NUMBER() OVER (PARTITION BY t.x, t.y, t.facility ORDER BY t.inspdate DESC) as rn
  FROM public.dbadult_insp_current t
  WHERE t.inspdate::date <= [analysis_date]
    AND t.x IS NOT NULL AND t.y IS NOT NULL
    AND t.survtype IN ('4','5','6')
    [AND t.facility IN (...)]  -- optional facility filter
)
```
**Purpose**: Since traps are inspected multiple times per year, we use `ROW_NUMBER()` partitioned by location (`x, y, facility`) and ordered by `inspdate DESC` to identify the most recent inspection at each unique trap location.

### Step 2: Get Latest Traps Only
```sql
latest_traps AS (
  SELECT ainspecnum, facility, x, y, survtype, inspdate
  FROM ranked_traps
  WHERE rn = 1
)
```
**Purpose**: Filter to only the most recent inspection per location (`rn = 1`).

### Step 3: Join Species Counts
```sql
SELECT lt.ainspecnum, lt.facility, lt.x as lon, lt.y as lat, lt.survtype, lt.inspdate::date as inspdate,
       COALESCE(SUM(s.cnt), 0) as species_count
FROM latest_traps lt
LEFT JOIN public.dbadult_species_current s ON lt.ainspecnum = s.ainspecnum
  [AND s.spp IN (...)]  -- optional species filter
GROUP BY lt.ainspecnum, lt.facility, lt.x, lt.y, lt.survtype, lt.inspdate
```
**Purpose**: 
- Join species counts to each trap
- If species filter is active, only count selected species
- `COALESCE(SUM(s.cnt), 0)` ensures traps with no species get count = 0
- GROUP BY required because LEFT JOIN can return multiple rows per trap (one per species)

**Result**: One row per unique trap location with:
- Most recent inspection date
- Species count (filtered by selected species if applicable)
- No duplicate locations

## Vector Index Calculation

### Algorithm: k-Nearest Neighbors with Inverse Distance Weighting

For each section, we:

1. **Find k nearest traps** - Default k=4 (user adjustable 1-10)
   - Calculate distance from section centroid to all trap locations
   - Sort by distance and take top k traps
   
2. **Calculate inverse distance weights**
   ```r
   weights = 1 / distance
   ```
   - Closer traps have higher weights
   - If distance = 0 (trap exactly on section centroid), set to 1e-6 to avoid division by zero

3. **Compute weighted average**
   ```r
   vector_index = sum(weights * species_counts) / sum(weights)
   ```
   - Multiply each trap's species count by its weight
   - Divide by sum of weights to normalize

### Example Calculation
Section with k=4 nearest traps:
- Trap 1: 24km away, 106 mosquitoes → weight = 1/24057 = 0.000042
- Trap 2: 24km away, 76 mosquitoes → weight = 0.000042
- Trap 3: 24km away, 1 mosquito → weight = 0.000042
- Trap 4: 24km away, 90 mosquitoes → weight = 0.000042

```
vector_index = (0.000042 * 106 + 0.000042 * 76 + 0.000042 * 1 + 0.000042 * 90) / (0.000042 * 4)
             = 0.011466 / 0.000168
             = 68.25
```

## Metrics Explained

### Vector Index
**Definition**: A distance-weighted estimate of mosquito abundance for each section based on nearby trap counts.

**Interpretation**:
- Higher values = More mosquitoes in nearby traps (weighted by proximity)
- Lower values = Fewer mosquitoes or far from active traps
- Value of 0 = No mosquitoes in k nearest traps

**Use Case**: Provides a spatial interpolation of mosquito activity across sections, even those without traps, based on nearby trap surveillance data.

### Nearest Trap Total
**Definition**: Simple sum of species counts from the k nearest traps (unweighted).

```r
nearest_trap_count = sum(species_counts from k nearest traps)
```

**Purpose**: 
- Provides context for the vector index
- Shows the raw total count contributing to the calculation
- Helps identify if high vector index is due to a few high-count traps or many moderate-count traps

**Example**: If k=4 and nearest trap counts are [106, 76, 1, 90], then `nearest_trap_total = 273`

## Spatial Operations

### Coordinate Systems
1. **Database Storage**: 
   - Traps: EPSG:4326 (WGS84 lat/lon) stored as x, y
   - Sections: EPSG:26915 (UTM Zone 15N) in `the_geom`

2. **Distance Calculation**: 
   - Transform both to EPSG:3857 (Web Mercator Auxiliary Sphere)
   - Units in meters for accurate distance calculations
   - Use `st_distance()` from sf package

3. **Display**: 
   - Convert back to EPSG:4326 for Leaflet map display

### Why Transform for Distance?
Lat/lon coordinates are angular (degrees), not linear. Direct Euclidean distance in lat/lon is inaccurate. Transforming to a projected coordinate system (EPSG:3857) with meter units provides accurate distances.

## Data Type Issues & Solutions

### Integer64 Problem
PostgreSQL's `SUM()` on integer columns returns `integer64` type in R. When multiplying very small decimals (weights like 0.000042) by integer64 values, precision loss can occur.

**Solution**: Convert to numeric before calculation
```r
counts <- as.numeric(traps_m$species_count[k_idx])
```

### Species Code Handling
Species codes (`sppcode`) are numeric but stored as integers in the database. When building SQL IN clauses, they must NOT be quoted:
```sql
-- Correct
WHERE s.spp IN (1, 2, 3)

-- Incorrect (causes type mismatch)
WHERE s.spp IN ('1', '2', '3')
```

## Performance Optimizations

1. **Single Query Strategy**: Fetch all traps with species counts in one query rather than:
   - Query 1: Get traps
   - Query 2: Get species counts
   - R join operation
   
2. **Filter Early**: Apply facility and species filters in SQL before returning data to R

3. **R-side Spatial Operations**: 
   - After fetching data, all distance calculations happen in R using vectorized sf operations
   - Much faster than making 3000+ distance queries to PostgreSQL

4. **Efficient Ranking**: `ROW_NUMBER()` window function is more efficient than `DISTINCT ON` for getting latest records

## User Controls

- **Analysis Date**: Only include traps inspected on or before this date
- **Species Filter**: Select specific species or "All Species" to include all
- **Trap Types**: Multi-select checkbox for survtypes 4, 5, 6
- **k (Neighbors)**: Number of nearest traps to consider (1-10, default 4)
- **Facility Filter**: Filter traps by facility (E, N, Sj, Sr, Wm, Wp, or All)
- **Refresh Data Button**: Data loads only when clicked (prevents accidental heavy queries)

## Map Layers

### Sections Layer (Yellow-Orange-Red)
- Circle size based on `log1p(vector_index) * 4` - larger for higher indices
- Color intensity from yellow (low) to red (high) based on vector index value
- Popup shows: Section code, Vector Index, Nearest Trap Total

### Traps Layer (Blue)
- Small blue circles (radius = 3)
- Popup shows: Trap ID, Facility, Trap Type, Inspection Date, Species Count
- Species count reflects current filter (e.g., if filtering for Culex pipiens, only shows that species count)

### Layer Control
Toggle sections and traps on/off independently

## Testing

Run `test-sql.R` to validate:
- SQL query returns expected number of rows
- Check for duplicate locations
- Verify distance calculations work correctly
- Review species count distribution

## Future Enhancements

Potential improvements:
- Add date range filter (instead of single date)
- Export results to CSV
- Cache species counts for faster reload
- Display section polygons instead of centroids
- Add time-series animation showing vector index over time
- Include trap density as additional metric
