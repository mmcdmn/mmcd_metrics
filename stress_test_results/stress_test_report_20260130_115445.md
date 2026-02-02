# MMCD Metrics Stress Test Report
**Generated:** 2026-01-30 12:00:51
**Target:** https://metrics.mmcd.org

---

## Executive Summary

| Metric | Value | Status |
|--------|-------|--------|
| **Overall Score** | 99/100 | Excellent |
| **Total Requests** | 2,570 | |
| **Success Rate** | 100.0% | OK |
| **Avg Response** | 121 ms | OK |
| **95th Percentile** | 148 ms | OK |
| **Max Concurrent Users** | 50 | |

---

## Performance by Endpoint

| Endpoint | Category | Requests | Success % | Avg (ms) | P95 (ms) |
|----------|----------|----------|-----------|----------|----------|
| Section Cards | light | 137 | 100.0% | 135 | 150 |
| Air Sites | medium | 297 | 100.0% | 126 | 148 |
| Structure Treatments | heavy | 268 | 100.0% | 123 | 145 |
| Catch Basin | medium | 248 | 100.0% | 121 | 146 |
| Inspections | medium | 261 | 100.0% | 121 | 142 |
| SUCO History | heavy | 412 | 100.0% | 120 | 146 |
| Drone Sites | heavy | 265 | 100.0% | 117 | 147 |
| Cattail Treatments | heavy | 289 | 100.0% | 117 | 147 |
| Control Efficacy | light | 138 | 100.0% | 117 | 149 |
| Ground Prehatch | medium | 255 | 100.0% | 117 | 148 |

---

## Performance by Concurrency Level

| Users | Requests | Success % | Avg (ms) | P95 (ms) | Req/sec |
|-------|----------|-----------|----------|----------|---------|
| 5 | 65 | 100.0% | 489 | 1742 | 2.0 |
| 10 | 210 | 100.0% | 141 | 156 | 6.8 |
| 15 | 240 | 100.0% | 125 | 150 | 7.8 |
| 20 | 260 | 100.0% | 116 | 128 | 8.4 |
| 25 | 275 | 100.0% | 112 | 129 | 8.7 |
| 30 | 270 | 100.0% | 110 | 115 | 8.9 |
| 35 | 315 | 100.0% | 105 | 111 | 9.4 |
| 40 | 320 | 100.0% | 103 | 108 | 9.6 |
| 45 | 315 | 100.0% | 103 | 109 | 9.6 |
| 50 | 300 | 100.0% | 101 | 108 | 9.8 |

---

## Recommendations

