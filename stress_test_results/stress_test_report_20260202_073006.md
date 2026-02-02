# MMCD Metrics Stress Test Report
**Generated:** 2026-02-02 07:36:11
**Target:** https://metrics.mmcd.org

---

## Executive Summary

| Metric | Value | Status |
|--------|-------|--------|
| **Overall Score** | 99/100 | Excellent |
| **Total Requests** | 2,610 | |
| **Success Rate** | 100.0% | OK |
| **Avg Response** | 119 ms | OK |
| **95th Percentile** | 137 ms | OK |
| **Max Concurrent Users** | 50 | |

---

## Performance by Endpoint

| Endpoint | Category | Requests | Success % | Avg (ms) | P95 (ms) |
|----------|----------|----------|-----------|----------|----------|
| Drone Sites | heavy | 285 | 100.0% | 132 | 132 |
| Air Sites | medium | 243 | 100.0% | 121 | 132 |
| Structure Treatments | heavy | 269 | 100.0% | 120 | 135 |
| Section Cards | light | 138 | 100.0% | 118 | 138 |
| Catch Basin | medium | 310 | 100.0% | 117 | 135 |
| SUCO History | heavy | 432 | 100.0% | 117 | 137 |
| Ground Prehatch | medium | 287 | 100.0% | 116 | 137 |
| Control Efficacy | light | 136 | 100.0% | 116 | 137 |
| Inspections | medium | 240 | 100.0% | 115 | 139 |
| Cattail Treatments | heavy | 270 | 100.0% | 115 | 131 |

---

## Performance by Concurrency Level

| Users | Requests | Success % | Avg (ms) | P95 (ms) | Req/sec |
|-------|----------|-----------|----------|----------|---------|
| 5 | 50 | 100.0% | 593 | 1793 | 1.7 |
| 10 | 220 | 100.0% | 131 | 145 | 7.3 |
| 15 | 255 | 100.0% | 121 | 137 | 8.0 |
| 20 | 260 | 100.0% | 114 | 127 | 8.6 |
| 25 | 275 | 100.0% | 111 | 120 | 8.8 |
| 30 | 300 | 100.0% | 105 | 113 | 9.4 |
| 35 | 315 | 100.0% | 103 | 110 | 9.6 |
| 40 | 320 | 100.0% | 103 | 113 | 9.6 |
| 45 | 315 | 100.0% | 102 | 107 | 9.7 |
| 50 | 300 | 100.0% | 104 | 110 | 9.5 |

---

## Recommendations

