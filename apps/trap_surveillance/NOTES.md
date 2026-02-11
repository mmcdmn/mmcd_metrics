# Trap Surveillance Dashboard

## Overview
Displays mosquito vector abundance, MLE/MIR infection rates, and Vector Index by **Vector Index Area** using pre-calculated database views. This is the production version — the legacy `trap_survillance_test` app used in-app KNN calculations; this version reads directly from materialized views.

## Data Sources

| View/Table                           | Purpose                                                                                   |
|--------------------------------------|-------------------------------------------------------------------------------------------|
| `dbadult_mon_nt_co2_forvectorabundance` | Monday night CO₂ trap counts by species, trap location, and yrwk                       |
| `dbvirus_mle_yrwk_area`              | Pre-calculated MLE by week × VI area (all Cx species combined)                            |
| `dbvirus_mle_yrwk_area_spp`          | Pre-calculated MLE by week × VI area × species                                            |
| `dbvirus_mir_yrwk_area`              | Pre-calculated MIR by week × VI area                                                      |
| `dbvirus_mir_yrwk_area_spp`          | Pre-calculated MIR by week × VI area × species                                            |
| `dbvirus_mle_yrwk`                   | District-wide MLE by week (for trend chart)                                               |
| `loc_vectorindexareas_sections_a`    | Section polygons with `viareaa` grouping, dissolved into 12 VI areas                      |

## Metrics

- **Abundance (N):** Average mosquitoes per trap per night for the selected week
- **MLE:** Maximum Likelihood Estimate of infection rate (pre-calculated, Firth method)
- **MIR:** Minimum Infection Rate = (positive pools / total mosquitoes) × 1000
- **Vector Index (VI):** N × P where P = infection rate (MLE or MIR/1000)

## Species
- **Total Cx Vectors** — all Culex combined; uses `dbvirus_mle_yrwk_area` (no species filter)
- **Cx. pipiens (33)** — uses `dbvirus_mle_yrwk_area_spp` with `spp_code = '33'`
- **Cx. restuans (34)** — spp_code = '34'
- **Cx. tarsalis (36)** — spp_code = '36'
- **Cx. restuans/pipiens (372)** — spp_code = '372'

## Key Differences from Legacy App (`trap_survillance_test`)
1. **No KNN calculations** — legacy computed per-section k-NN weighted averages; this uses area-level pre-calculated data
2. **No PooledInfRate dependency** — MLE is pre-calculated in the database
3. **Area-based, not section-based** — displays 12 VI areas instead of ~3,200 individual sections
4. **Much faster** — simple SELECT queries vs. heavy spatial computation
5. **Week-based navigation** — browse by yrwk instead of analysis date

## Files
- `app.R` — Shiny app entry point
- `ui_helper.R` — Dashboard UI layout
- `data_functions.R` — All database queries and data fetching
- `display_functions.R` — Leaflet map rendering
- `NOTES.md` — This file
