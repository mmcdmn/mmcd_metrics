# Causal Analysis — Mathematical Reference

**Application:** `apps/trap_surveillance` — Causal Analysis tab  
**Inspired by:** Chakravarti, Li, Bartlett, Irwin, Smith (2026). *"A novel approach to determine mosquito trap placement for West Nile virus surveillance."* J. Med. Entomol. 63(2).

---

## Overview

The Causal Analysis pipeline runs four computations in sequence:

1. **Trap Performance Scoring** — composite metric rating each trap's WNV detection value
2. **Spatial Risk Smoothing** — kernel-weighted interpolation of risk across the trap network
3. **Risk Surface & Coverage Assessment** — grid interpolation and per-area coverage grading
4. **Causal Factor Analysis** — Spearman correlations and dose-response curves

All functions are in `apps/trap_surveillance/data_functions.R`.

---

## 1. Trap Performance Scoring

**Function:** `fetch_trap_performance(year)`

### Data Aggregation (SQL)

For each trap location, the query aggregates across all collection weeks for the species group `spp_name = 'Total_Cx_vectors'`:

| Metric | SQL |
|--------|-----|
| `years_active` | `COUNT(DISTINCT year)` |
| `weeks_active` | `COUNT(DISTINCT yrwk)` |
| `total_mosq` | `SUM(total_count)` |
| `avg_per_week` | `AVG(total_count)` |
| `total_pools` | `COUNT(pool_id)` from joined virus pool tables |
| `total_positive` | `SUM(CASE WHEN test_result = 1 THEN 1 ELSE 0 END)` |
| `total_tested` | `COUNT(DISTINCT pool_id)` where test exists |

### Normalization

Each scoring component is normalized by the maximum observed value across all traps, ensuring scores range 0–1:

$$
S^{\mathrm{yield}}_i = \frac{\mathrm{AvgPerWeek}_i}{\max_j(\mathrm{AvgPerWeek}_j)}
$$

$$
S^{\mathrm{testing}}_i = \frac{\mathrm{TotalPools}_i}{\max_j(\mathrm{TotalPools}_j)}
$$

$$
S^{\mathrm{detection}}_i = \frac{\mathrm{PositivityRate}_i}{\max_j(\mathrm{PositivityRate}_j)}
$$

$$
S^{\mathrm{consistency}}_i = \frac{\mathrm{WeeksActive}_i}{\max_j(\mathrm{WeeksActive}_j)}
$$

Where:

$$
\mathrm{PositivityRate}_i = \frac{\mathrm{TotalPositive}_i}{\mathrm{TotalPools}_i} \times 100
$$

### Composite Score

$$
C_i = 0.25 \times S^{\mathrm{yield}}_i + 0.30 \times S^{\mathrm{testing}}_i + 0.30 \times S^{\mathrm{detection}}_i + 0.15 \times S^{\mathrm{consistency}}_i
$$

Weights reflect the paper's finding that testing volume (number of pools) and detection (positivity rate) are the strongest predictors of trap utility for WNV surveillance.

### Performance Tiers

| Tier | Threshold |
|------|-----------|
| High | composite ≥ 0.6 |
| Medium | 0.3 ≤ composite < 0.6 |
| Low | composite < 0.3 |

---

## 2. Spatial Risk Smoothing

**Function:** `compute_spatial_risk(perf_data, bandwidth_km, max_radius_km)`

### Haversine Distance

The great-circle distance between two points on a sphere is computed using the Haversine formula. For traps $i$ and $j$ with coordinates $(\phi_i, \lambda_i)$ and $(\phi_j, \lambda_j)$ in radians:

$$
a = \sin^2\!\left(\frac{\phi_j - \phi_i}{2}\right) + \cos(\phi_i)\,\cos(\phi_j)\,\sin^2\!\left(\frac{\lambda_j - \lambda_i}{2}\right)
$$

$$
d_{ij} = 2\,R_\oplus\,\arcsin\!\left(\sqrt{a}\right)
$$

Where $R_\oplus = 6{,}371.0$ km (mean Earth radius).

### Matérn Covariance (Paper's Approach)

The paper uses a **Matérn spatial covariance** in its GLMM with shape parameter $\nu = 0.16$:

$$
C(d) = \sigma^2\,\frac{2^{1-\nu}}{\Gamma(\nu)}\left(\sqrt{2\nu}\,\frac{d}{\rho}\right)^\nu K_\nu\!\left(\sqrt{2\nu}\,\frac{d}{\rho}\right)
$$

Where:
- $d$ = distance between locations
- $\rho$ = range parameter (practical correlation range)
- $\nu$ = smoothness parameter (paper uses $\nu = 0.16$)
- $K_\nu$ = modified Bessel function of the second kind
- $\Gamma$ = gamma function

### Our Approximation: Exponential Kernel

When $\nu = 0.5$, the Matérn simplifies to the **exponential covariance**:

$$
C(d) = \sigma^2 \exp\!\left(-\frac{d}{\rho}\right)
$$

Since the paper's $\nu = 0.16$ is close to the exponential limit, we use an exponential kernel as a computationally efficient approximation:

$$
w_j = \exp\!\left(-\frac{d_{ij}}{\beta}\right)
$$

Where $\beta$ = `bandwidth_km` (default 3 km). Only neighbors within `max_radius_km` (default 10 km) are included.

### Signal and Spatial Risk

Each trap's **detection signal** weights detection more heavily than testing volume:

$$
\mathrm{Signal}_j = 0.6 \times S^{\mathrm{detection}}_j + 0.4 \times S^{\mathrm{testing}}_j
$$

The **spatial risk** at trap $i$ is the kernel-weighted average of all neighbor signals:

$$
\mathrm{SpatialRisk}_i = \frac{\displaystyle\sum_{j \neq i,\; d_{ij} \leq R} w_j \times \mathrm{Signal}_j}{\displaystyle\sum_{j \neq i,\; d_{ij} \leq R} w_j}
$$

Where $R$ = `max_radius_km`. If no neighbors are within range, $\mathrm{SpatialRisk}_i = 0$.

### Risk Index

The **Risk Index** combines local trap performance with spatial context:

$$
\mathrm{RiskIndex}_i = 0.35 \times S^{\mathrm{yield}}_i + 0.30 \times \mathrm{SpatialRisk}_i + 0.35 \times S^{\mathrm{detection}}_i
$$

### Risk Tiers

| Tier | Threshold |
|------|-----------|
| High Risk | RiskIndex ≥ 0.5 |
| Moderate Risk | 0.2 ≤ RiskIndex < 0.5 |
| Low Risk | RiskIndex < 0.2 |

---

## 3. Risk Surface & Coverage Assessment

### Risk Surface Grid Interpolation

**Function:** `generate_risk_surface(risk_data, grid_res, bandwidth_km)`

A regular latitude/longitude grid is created spanning the trap extent with ~2 km padding:

$$
\lambda_{\mathrm{grid}} \in [\min(\lambda) - 0.02,\; \max(\lambda) + 0.02], \quad \Delta = 0.005° \approx 500 \textrm{ m}
$$

$$
\phi_{\mathrm{grid}} \in [\min(\phi) - 0.02,\; \max(\phi) + 0.02], \quad \Delta = 0.005° \approx 500 \textrm{ m}
$$

For each grid cell at coordinates $(x, y)$, the interpolated risk value uses the same exponential kernel as the trap-level smoothing, but only considers traps within $3\beta$:

$$
R(x, y) = \frac{\displaystyle\sum_{j:\, d_j \leq 3\beta} w_j \times \mathrm{RiskIndex}_j}{\displaystyle\sum_{j:\, d_j \leq 3\beta} w_j}
$$

Where:
- $d_j$ = Haversine distance from grid cell $(x, y)$ to trap $j$
- $w_j = \exp(-d_j / \beta)$
- $\beta$ = `bandwidth_km`

### Coverage Gap Detection

A grid cell is classified as a **coverage gap** if the nearest trap is farther than $2\beta$:

$$
\mathrm{Gap}(x, y) = \begin{cases} 1 & \text{if } \min_j(d_j) > 2\beta \\ 0 & \text{otherwise} \end{cases}
$$

### Area Coverage Grading

**Function:** `compute_area_coverage(risk_data)`

Per-VI-area metrics are aggregated and graded:

| Grade | Criteria |
|-------|----------|
| **Good** | $n \geq 10$ AND $\bar{C} \geq 0.3$ |
| **Adequate** | $n \geq 5$ AND $\bar{C} \geq 0.2$ |
| **Thin** | $n \geq 3$ |
| **Gap** | $n < 3$ |

Where $n$ = number of traps in the area, $\bar{C}$ = mean composite score of all traps in the area.

Additional per-area metrics:
- `avg_risk` = mean RiskIndex
- `positivity_pct` = (total positive / total pools) × 100
- `pct_low` = percentage of traps with Low tier

---

## 4. Causal Factor Analysis

**Function:** `compute_causal_factors(perf_data)`

### Spearman Rank Correlation

For each factor $X_k$ and the composite score $Y$, the Spearman rank correlation is:

$$
r_s = 1 - \frac{6 \sum_{i=1}^{n} d_i^2}{n(n^2 - 1)}
$$

Where $d_i = \mathrm{rank}(X_{k,i}) - \mathrm{rank}(Y_i)$. In practice, computed via `cor.test(method = "spearman")` with exact p-values disabled for large $n$.

**Factors analyzed:**

| Factor | Column |
|--------|--------|
| Pools Collected | `total_pools` |
| Positive Pools | `total_positive` |
| Avg Yield/Week | `avg_per_week` |
| Weeks Active | `weeks_active` |
| Total Mosquitoes | `total_mosq` |

Significance: `***` ($p < 0.001$), `**` ($p < 0.01$), `*` ($p < 0.05$), `ns` ($p \geq 0.05$)

### Dose-Response Curves (ADRF Approximation)

The paper uses **Average Dose-Response Functions (ADRFs)** estimated via generalized propensity score methods. Our approximation uses binned averages:

1. For each factor, create $q$ bins using quantile breaks (up to 10 deciles):

$$
b_k = Q(X,\; k/q), \quad k = 0, 1, \ldots, q
$$

2. Within each bin $B_k = [b_{k-1}, b_k)$, compute the average score and standard error:

$$
\hat{Y}_k = \frac{1}{n_k} \sum_{i \in B_k} Y_i, \quad \mathrm{SE}_k = \frac{s_k}{\sqrt{n_k}}
$$

Where $s_k$ is the standard deviation of $Y$ within bin $k$ and $n_k$ is the bin count.

3. The plot displays raw scatter + binned averages with 95% confidence ribbons:

$$
\mathrm{CI}_k = \hat{Y}_k \pm 1.96 \times \mathrm{SE}_k
$$

### Per-Trap Recommendations

| Condition | Recommendation |
|-----------|---------------|
| Score < 0.15 AND pools = 0 | Consider relocating — no testing data and low yield |
| Score < 0.15 | Review trap placement — consistently underperforming |
| Score < 0.3 AND pools < 3 | Increase pool collection frequency |
| 0.3 ≤ Score < 0.6 | Adequate — maintain current operations |
| Score ≥ 0.6 | High performer — prioritize for continued surveillance |

---

## Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `bandwidth_km` | 3 | Exponential kernel bandwidth (km). Controls spatial smoothing range. |
| `max_radius_km` | 10 | Maximum distance for neighbor search in spatial risk. |
| `grid_res` | 0.005° | Risk surface grid resolution (~500m). |
| Gap threshold | 2 × bandwidth | Grid cells beyond this distance from nearest trap = coverage gap. |
| Interpolation range | 3 × bandwidth | Grid interpolation ignores traps beyond this distance. |

---

## Relationship to the Paper

| Paper Concept | Our Implementation | Difference |
|---------------|-------------------|------------|
| Spatial GLMM with Matérn ($\nu = 0.16$) | Exponential kernel smoothing | Simplified — no random effects or likelihood estimation |
| Sensitivity/specificity scoring | Composite weighted score | Different weighting; no human-case outcome data |
| ADRF via generalized propensity scores | Spearman + binned dose-response | Approximation — no propensity score adjustment |
| 1,500m population buffer | Not available | MMCD lacks geocoded census data integration |
| Trap-year panel data (2004–2018) | Cross-sectional or single-year | Less temporal depth |

---

## Data Flow

```
fetch_trap_performance(year)
  │
  ├── SQL: aggregate abundance + pools + testing per trap
  ├── Normalize scores (0–1) by max
  └── Compute composite score + tier
        │
        ▼
compute_spatial_risk(perf_data, bandwidth_km, max_radius_km)
  │
  ├── Haversine distance matrix
  ├── Exponential kernel weights
  ├── Kernel-weighted spatial risk per trap
  └── Risk index = 0.35×yield + 0.30×spatial + 0.35×detection
        │
        ├──▶ compute_area_coverage(risk_data)
        │     └── Per-VI-area aggregation + coverage grade
        │
        ├──▶ generate_risk_surface(risk_data, grid_res, bandwidth_km)
        │     ├── Lat/lon grid interpolation
        │     └── Gap detection (nearest trap > 2×bandwidth)
        │
        └──▶ compute_causal_factors(risk_data)
              ├── Spearman correlations
              ├── Binned dose-response curves
              └── Per-trap recommendations
```
