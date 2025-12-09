# Trap Surveillance Test App - Technical Documentation

## Overview

This Shiny application analyzes mosquito surveillance data to estimate virus infection risk across geographic sections. It implements **trap-based Maximum Likelihood Estimation (MLE)** with **k-nearest neighbor distance-weighted averaging (k-NN DWA)** for spatial interpolation.

---

## Data Sources

### Database Tables

1. **`dbvirus_pool_test`**: Virus test results for individual pools
   - Fields: `poolnum`, `result` (Pos/Neg), `date`, `target` (WNV, etc.), `method`
   
2. **`dbvirus_pool`**: Pool composition metadata
   - Fields: `poolnum`, `sampnum_yr`, `spp_code`, `count` (mosquitoes per pool)
   
3. **`dbadult_insp_current`**: Trap inspection data
   - Fields: `sampnum_yr`, `inspdate`, `survtype` (trap type), `count` (mosquitoes)
   
4. **`loc_mondaynight`**: Trap location coordinates
   - Fields: `sampnum_yr`, `facility`, `geometry` (spatial point)
   
5. **`loc_sect`**: Section boundary polygons
   - Fields: `section` (ID), `geometry` (spatial polygon)

### Data Relationships

```
Pool Test → Pool → Trap Inspection → Trap Location
   (result)   (species)   (count)        (coordinates)
```

### Unified SQL Query 

**Function**: `fetch_unified_surveillance_data()`  
The app uses a **unified SQL query** with Common Table Expressions (CTEs) to fetch all trap and pool data in one database round trip:

```sql
WITH 
-- CTE 1: Latest trap inspections per unique physical location
latest_traps AS (
  SELECT DISTINCT ON (t.x, t.y, t.facility)
    t.ainspecnum, t.facility, t.x, t.y, t.survtype, t.inspdate,
    COALESCE(SUM(s.cnt), 0) as species_count
  FROM public.dbadult_insp_current t
  LEFT JOIN public.dbadult_species_current s 
    ON t.ainspecnum = s.ainspecnum
    AND s.spp IN (...)  -- Species filter
  WHERE t.inspdate::date <= :analysis_date
    AND t.x IS NOT NULL AND t.y IS NOT NULL
    AND t.survtype IN (...)  -- Trap type filter (4, 5, 6)
    AND t.facility IN (...)  -- Facility filter
  GROUP BY t.ainspecnum, t.facility, t.x, t.y, t.survtype, t.inspdate
  ORDER BY t.x, t.y, t.facility, t.inspdate DESC
),

-- CTE 2: Virus pool tests with trap location coordinates
virus_pools AS (
  SELECT 
    t.id AS test_id, t.poolnum, t.result, t.date AS testdate,
    t.target, p.sampnum_yr, p.spp_code, p.count,
    ST_X(ST_Transform(
      CASE WHEN c.network_type IS NOT NULL 
           THEN l.geom 
           ELSE c.geometry 
      END, 4326)) as lon,
    ST_Y(ST_Transform(...)) as lat,
    c.facility
  FROM dbvirus_pool_test t
  LEFT JOIN dbvirus_pool p ON p.poolnum = t.poolnum
  LEFT JOIN dbadult_insp_current c ON c.sampnum_yr = p.sampnum_yr
  LEFT JOIN (
    SELECT a.loc_code, n.geom 
    FROM loc_mondaynight_active a 
    LEFT JOIN loc_mondaynight n ON n.loc_code = a.loc_code
    WHERE a.enddate IS NULL
  ) l ON l.loc_code = c.loc_code
  WHERE t.date >= :analysis_date - INTERVAL ':lookback_days days'
    AND t.date <= :analysis_date
    AND t.target = :virus_target  -- WNV, LAC, EEE
    AND c.survtype IN (...)  -- Trap type filter
    AND c.facility IN (...)  -- Facility filter
    AND p.spp_code IN (...)  -- Species filter
)

-- Union: Return unified result set with sampnum_yr for pool grouping
SELECT 'trap' as data_type, ainspecnum as id, ..., NULL as sampnum_yr FROM latest_traps
UNION ALL
SELECT 'pool' as data_type, poolnum::text as id, ..., sampnum_yr FROM virus_pools
WHERE lon IS NOT NULL AND lat IS NOT NULL;
```



## Methodology

### Two-Stage Trap-Based MLE Calculation

#### Stage 1: Per-Trap MLE Calculation

**Purpose**: Calculate a single MLE for each trap using all pools tested at that trap

**Filters Applied**:
- **Date Range**: Last 90 days from analysis date
- **Virus Target**: WNV, LAC, or EEE (user selectable)
- **Species**: User-selected species codes (or all)
- **Trap Types**: User-selected trap types (4=Elevated CO2, 5=Gravid, 6=CO2 Overnight)

**Process**:
1. Query all pool tests matching filters
2. Join with trap inspection data to filter by `survtype` (trap type)
3. Group pools by `sampnum_yr` (trap ID)
4. For each trap:
   - Extract pool test results: `x` = binary vector (1=Pos, 0=Neg)
   - Extract pool sizes: `m` = mosquitoes per pool
   - Call `PooledInfRate::pooledBin()` directly
   - Calculate MLE with 95% confidence interval
5. Result: One MLE per trap with metadata (num_pools, num_positive, total_mosquitoes)

**Example**:
```
Trap 25-91099 (Elevated CO2, Culex pipiens):
  - Pool 1: 50 mosquitoes, Negative
  - Pool 2: 45 mosquitoes, Negative
  - Pool 3: 52 mosquitoes, Negative
  - Pool 4: 48 mosquitoes, Negative
  
→ MLE = 0 per 1000 (CI: 0.00 - 298.67)
```

#### Stage 2: k-NN Distance-Weighted Averaging for Sections

**Purpose**: Assign infection rate estimates to sections using spatially-weighted interpolation from nearby trap MLEs

**Mathematical Formulation**:

For each section $s$ with centroid location $\mathbf{c}_s$:

1. **Identify k-Nearest Traps**: Find the set of $k$ nearest trap locations $\\{\mathbf{t}_1, \mathbf{t}_2, \ldots, \mathbf{t}_k\\}$

2. **Calculate Distances**: Compute geodesic distances
   
   $$d_i = \text{distance}(\mathbf{c}_s, \mathbf{t}_i) \quad \text{for } i = 1, 2, \ldots, k$$

3. **Inverse Distance Weighting**: Calculate weights using inverse distance squared
   
   $$w_i = \frac{1}{d_i^2}$$

4. **Normalize Weights**: Ensure weights sum to unity
   
   $$\hat{w}_i = \frac{w_i}{\sum_{j=1}^{k} w_j}$$

5. **Weighted Average MLE**: Compute section MLE as weighted mean
   
   $$\text{MLE}_s = \sum_{i=1}^{k} \hat{w}_i \cdot \text{MLE}_{\mathbf{t}_i}$$

   where $\text{MLE}_{\mathbf{t}_i}$ is the trap-level MLE calculated in Stage 1.

**Vector Index Calculation**:

The Vector Index for section $s$ combines population density and infection probability:

$$\text{VI}_s = N_s \times P_s$$

where:

- $N_s$ = Population Index (k-NN inverse-distance-weighted average of trap mosquito counts)
- $P_s = \frac{\text{MLE}_s}{1000}$ (infection probability scaled per 1000 mosquitoes)

**Process**:
1. Calculate centroid for each section polygon
2. For each section centroid:
   - Find k nearest trap locations (default k=4)
   - Calculate geodesic distance (great circle) to each trap
   - Apply inverse distance squared weighting: `weight = 1 / distance²`
   - Normalize weights to sum to 1
   - Calculate section MLE as weighted average: `Σ(trap_MLE × weight)`
3. Aggregate statistics:
   - Count unique trap locations used
   - Sum total pools from all traps
   - Sum positive pools from all traps
   - Track nearest and farthest distances
4. Result: Section-level MLE estimates with spatial smoothing and metadata

**Important**: The section's pool counts represent the **sum of all pools from the k nearest traps**, not individual pool tests at that section location.

**Trap Grouping Options** (to prevent duplicate distance bug):

**Option 1: By Location (Recommended)**
- Groups traps by unique (lon, lat) coordinates first
- Finds k nearest LOCATIONS (not individual traps)
- Uses ALL traps at those k locations in the calculation
- Assigns same distance to all traps at same physical location
- **Why**: Prevents selecting 4 traps all at same coordinates
- **Result**: Proper geographic diversity in k-NN selection

**Option 2: By Individual Trap**
- Original behavior (for comparison)
- Finds k nearest individual traps
- May select multiple traps at same physical location
- **Issue**: Can result in identical nearest/farthest distances

**Example**:
```
Section 123 (centroid at -92.80, 45.25):
  By Location mode:
    Location A (-92.81, 45.24) @ 1200m → Traps: 25-91099 (4 pools), 25-91100 (3 pools)
    Location B (-92.79, 45.26) @ 1500m → Traps: 25-91101 (5 pools)
    Location C (-92.82, 45.23) @ 1800m → Traps: 25-91102 (4 pools)
  
  Section statistics:
    - Traps used: 4
    - Unique locations: 3
    - Total pools: 16 (sum from all 4 traps)
    - Positive pools: 2 (sum from all 4 traps)
  
  Weighted average MLE:
    Section MLE = (MLE_A × 0.45) + (MLE_B × 0.30) + (MLE_C × 0.25)
```

### Distance Weighting Implementation

```r
# Inverse distance squared
weights <- 1 / (distances^2)

# Normalize to sum to 1
weights <- weights / sum(weights)

# Weighted average
section_mle <- sum(trap_mles * weights)
```
---

## Metrics

### 1. Population Index (N)

**Definition**: Estimated mosquito abundance per section

**Calculation**:
- Sum mosquito counts from trap inspections
- Group by trap location (sampnum_yr)
- Apply k-NN DWA to assign counts to sections

**Interpretation**: Higher values = more mosquitoes present

**Use case**: Identify areas with high mosquito populations

---

### 2. Maximum Likelihood Estimate (MLE)

**Definition**: Estimated virus infection rate per 1,000 mosquitoes

**Package**: PooledInfRate (CDC - https://github.com/CDCgov/PooledInfRate)

**Available Methods**:

| Method | Description | When to Use |
|--------|-------------|-------------|
| **Firth** | Bias-corrected MLE | **Default** - Best for small samples |
| **Gart** | Score-based method | Classic pooled testing estimator |
| **MLE** | Standard maximum likelihood | Large samples only |
| **MIR** | Minimum infection rate | Simple ratio: positives/total |

**Scale**: Default 1,000 (results per 1,000 mosquitoes)

**Interpretation**:
- MLE = 5.2 → "Estimated 5.2 infected mosquitoes per 1,000"
- 95% CI = (2.1 - 12.8) → "True rate likely between 2.1 and 12.8 per 1,000"
- MLE = 0 (CI: 0 - 298.67) → "No positives, but could be as high as 298 per 1,000"

**Note**: "95% CI" is the confidence LEVEL, not a percentage result
- It means: "We are 95% confident the true rate is in this range"
- Wider intervals = more uncertainty (fewer pools tested)
- Narrower intervals = more precision (more data)

---

### 3. Vector Index (VI)

**Definition**: Combined risk metric = Population × Infection Rate

**Formula**: 
```r
VI = N × (MLE / 1000)
```

**Calculation**:
1. Calculate Population Index (N) using k-NN DWA
2. Calculate MLE (P) using trap-based k-NN DWA
3. Multiply at section level: VI = N × P

**Interpretation**:
- Units: "Estimated infected mosquitoes per section"
- Higher VI = More mosquitoes AND higher infection rate
- Better risk indicator than either metric alone

**Example**:
```
Section A: N=500 mosquitoes, MLE=10 per 1000
  VI = 500 × (10/1000) = 5 infected mosquitoes
  
Section B: N=100 mosquitoes, MLE=50 per 1000
  VI = 100 × (50/1000) = 5 infected mosquitoes
  
→ Same risk despite different abundance/infection patterns
```

---
## File Structure

### Core Files

| File | Lines | Purpose |
|------|-------|---------|
| `app.R` | 181 | Shiny server logic, reactive data pipeline |
| `ui_helper.R` | 181 | UI layout, input controls, help text |
| `data_functions.R` | 342 | Database queries, Population Index, Vector Index |
| `display_functions.R` | 638 | Leaflet map rendering, popups, themes |
| `mle_trap_based.R` | 359 | Two-stage trap-based MLE calculation |
| `mle_functions.R` | 164 | PooledInfRate wrapper, format normalization |

### Shared Resources

| File | Purpose |
|------|---------|
| `../shared/db_helpers.R` | Database connection management |
| `../shared/color_themes.R` | Color palette definitions |

---

## References

### Scientific Literature

1. **Firth D (1993)**. "Bias reduction of maximum likelihood estimates." *Biometrika* 80(1):27-38.
   - Bias-corrected MLE method (default)

2. **Gart JJ (1991)**. "An application of score methodology: Confidence intervals and tests of fit for one-hit curves." *Biometrics* 47(1):173-182.
   - Score-based pooled testing estimator

3. **Walter SD et al. (1980)**. "Estimation of infection rates in populations of organisms using pools of variable size." *American Journal of Epidemiology* 112(1):124-128.
   - Theory of pooled testing for infection estimation

### Software

- **PooledInfRate Package**: https://github.com/CDCgov/PooledInfRate
  - CDC-maintained R package for pooled infection rate estimation
  - Methods: Firth, Gart, MLE, MIR

- **k-NN Distance Weighting**: Standard geostatistical interpolation
  - Inverse Distance Weighting (IDW)
  - Commonly used in spatial analysis and GIS


## Future Enhancements

### Potential Improvements

1. **Temporal Analysis**: 
   - Time series plots of MLE over weeks/months
   - Animated maps showing risk evolution

2. **Statistical Testing**:
   - Compare MLEs between sections (significance tests)
   - Hotspot detection (spatial clustering)

3. **Predictive Models**:
   - Machine learning to predict high-risk areas
   - Incorporate weather/environmental data

4. **Export Features**:
   - Download section MLE estimates as CSV
   - Export map as PDF for reports

5. **Performance Optimization**:
   - Cache trap MLE calculations
   - Parallel processing for section k-NN

---
