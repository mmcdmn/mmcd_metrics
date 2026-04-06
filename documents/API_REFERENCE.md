# MMCD Metrics API Reference

Base URL: `https://metrics.mmcd.org/v1`

---

## Authentication

Private endpoints require a valid API key. Pass it one of two ways:

| Method | Header |
|--------|--------|
| Bearer token | `Authorization: Bearer <your-key>` |
| API key header | `X-API-Key: <your-key>` |

Public endpoints require no authentication.

---

## Public Endpoints

### GET `/v1/public/health`

Health check — confirms the API is running.

**Parameters:** none

**Response:**
```json
{
  "status": "ok",
  "timestamp": "2026-03-17 14:22:01"
}
```

---

### GET `/v1/public/facilities`

Returns the list of facility codes and labels.

**Parameters:** none

**Response:**
```json
[
  { "label": "All Facilities", "code": "all" },
  { "label": "South Rosemount",  "code": "Sr"  },
  { "label": "East",     "code": "E"  },
  ...
]
```

---

### GET `/v1/public/foremen`

Returns the list of FOS (Field Operations Supervisor) areas and labels.

**Parameters:** none

**Response:**
```json
[
  { "label": "All FOS Areas", "code": "all"  },
  { "label": "Smith",         "code": "1234" },
  ...
]
```

---

### GET `/v1/public/threshold`

Returns the current numdip threshold based on the spring ACT4-P1 date.  
Before the threshold date the numdip threshold is **1**; on/after it becomes **2**. Resets to 1 on January 1.

**Parameters:** none

**Response:**
```json
{
  "threshold": 2,
  "threshold_date": "2026-04-15",
  "as_of": "2026-03-17",
  "year": 2026
}
```

| Field | Type | Description |
|-------|------|-------------|
| `threshold` | integer | Current numdip threshold (1 or 2) |
| `threshold_date` | string | The ACT4-P1 date that triggers the change from 1 to 2 |
| `as_of` | string | Today's date |
| `year` | integer | Current year |

---

## Private Endpoints

### GET `/v1/private/sectcodes`

Section-code reference data. Returns sectcode, facility, FOS area, and zone.

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `facility` | string | No | all | Facility code: `Sr`, `MO`, `E`, `Wp`, `Wm`, `N`, `Sj` |
| `limit` | integer | No | 500 | Row cap, 1–5000 |

**Response:**
```json
{
  "count": 742,
  "data": [
    {
      "sectcode": "190113A",
      "facility": "Sr",
      "fosarea": "5678",
      "zone": "1"
    },
    ...
  ],
  "timestamp": "2026-03-17 14:22:01"
}
```

---

### GET `/v1/private/air-checklist`

Returns air site checklist data: inspection status, active treatments, lab bug status, FOS assignment, priority, and more. This is the same data that powers the Shiny Air Inspection Checklist app.

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `facility` | string | No | all | Facility code: `Sr`, `MO`, `E`, `Wp`, `Wm`, `N`, `Sj` (case-insensitive) |
| `foreman` | string | No | all | FOS shortname (e.g. `Smith`) |
| `zone` | string | No | `1,2` | Zones to include: `1`, `2`, or `1,2` |
| `lookback_days` | integer | No | 2 | How many days back to look for inspections (1–14) |
| `as_of` | string | No | today | Analysis date in `YYYY-MM-DD` format |
| `priority` | string | No | all | Comma-separated priority filter: `RED`, `YELLOW`, `BLUE`, `GREEN`, `PURPLE` |

**Example request:**

```
GET https://metrics.mmcd.org/v1/private/air-checklist?facility=Sr&zone=1,2&priority=YELLOW,RED&lookback_days=2
Authorization: Bearer <your-key>
```

**Response envelope:**
```json
{
  "count": 742,
  "as_of": "2026-03-17",
  "lookback_days": 2,
  "facility_filter": "Sr",
  "foreman_filter": "all",
  "priority_filter": "YELLOW,RED",
  "data": [ ... ],
  "refreshed_at": "2026-03-17 14:22:01"
}
```

**Each row in `data`:**

| Field | Type | Description |
|-------|------|-------------|
| `sitecode` | string | Unique breeding site ID (e.g. `19011301`) |
| `acres` | number | Site acreage |
| `priority` | string | Priority level: RED, YELLOW, BLUE, GREEN, PURPLE |
| `facility` | string | Facility code |
| `zone` | string | Zone number (1 or 2) |
| `fos_name` | string | FOS (Field Operations Supervisor) full name |
| `fosarea` | string | FOS employee number |
| `sectcode` | string | Section code (7 chars, e.g. `1901130`) |
| `township_name` | string | Township name (looked up from first 4 digits of sitecode) |
| `airmap_num` | string | Air map reference number |
| `was_inspected` | boolean | `true` if inspected within lookback window |
| `last_insp_date` | string | Date of most recent inspection (ISO format) |
| `dip_count` | integer | Number of dips taken |
| `pct_wet` | number | Percentage of site that is wet |
| `inspector_name` | string | Name of inspector who did the most recent inspection |
| `sampnum_yr` | string | Sample number (year-based) |
| `remarks` | string | Site remarks (from `loc_breeding_sites`, overridden by card remarks if present) |
| `restricted_area` | boolean | Site is in a restricted area (from cards) |
| `drone` | boolean | Site requires drone access (from `loc_breeding_sites`) |
| `needs_sample` | boolean | Site is flagged for sampling (from `loc_breeding_sites`) |
| `bug_status` | string | Lab result status: `Red Bugs`, `Blue Bugs`, `Pending Lab`, `No Bugs`, or `No Sample` |
| `has_active_treatment` | boolean | Has an unexpired treatment |
| `active_material` | string | Material type of active treatment |
| `active_trt_date` | string | Date of most recent treatment |
| `active_trt_expiry` | string | Expiration date of active treatment |
| `is_prehatch` | boolean | Whether the active treatment is a prehatch material |

---

### GET `/v1/private/claims`

Returns all active site claims from the Redis cache. Claims are stored per-date and looked up across the lookback window. The most recent claim per sitecode wins.

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `lookback_days` | integer | No | 2 | How many days back to retrieve claims (1–14) |

**Response:**
```json
{
  "count": 5,
  "data": [
    {
      "sitecode": "19011301",
      "emp_num": "12345",
      "emp_name": "Eric S.",
      "time": "2026-03-17T10:15:30",
      "claim_date": "2026-03-17"
    },
    ...
  ],
  "as_of": "2026-03-17"
}
```

| Field | Type | Description |
|-------|------|-------------|
| `sitecode` | string | The claimed site ID |
| `emp_num` | string | Employee number of the claimer |
| `emp_name` | string | Display name of the claimer |
| `time` | string | ISO timestamp when the claim was made |
| `claim_date` | string | The date partition the claim is stored under |

---

### POST `/v1/private/claims`

Create or update claims in the Redis cache. Accepts a JSON body with an array of claim objects. Each claim is stored under today's date with a 2-day TTL.

**Request body:**
```json
{
  "claims": [
    { "sitecode": "19011301", "emp_num": "12345", "emp_name": "Eric S." },
    { "sitecode": "19011302", "emp_num": "67890", "emp_name": "Monica W." }
  ]
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `sitecode` | string | Yes | Site to claim (alphanumeric, max 32 chars) |
| `emp_num` | string | Yes | Employee number (alphanumeric, max 32 chars) |
| `emp_name` | string | No | Display name (defaults to emp_num if omitted) |

**Response:**
```json
{
  "saved": 2,
  "errors": null,
  "date": "2026-03-17"
}
```

---

## Error Responses

All errors return a JSON object with an `error` field:

```json
{ "error": "unauthorized: valid API key required" }
```

| HTTP Status | Meaning |
|-------------|---------|
| 401 | Missing or invalid API key (private endpoints) |
| 400 | Invalid parameter value (details in `error` message) |

---

## Usage Examples

### curl
```bash
# Health check
curl https://metrics.mmcd.org/v1/public/health

# Air checklist (with API key)
curl -H "Authorization: Bearer YOUR_KEY" \
  "https://metrics.mmcd.org/v1/private/air-checklist?facility=Sr&priority=RED&lookback_days=2"
```

### Python
```python
import requests

url = "https://metrics.mmcd.org/v1/private/air-checklist"
params = {
    "facility": "Sr",
    "zone": "1,2",
    "priority": "YELLOW,RED",
    "lookback_days": 2,
}
headers = {"Authorization": "Bearer YOUR_KEY"}

resp = requests.get(url, params=params, headers=headers)
data = resp.json()["data"]
print(f"{len(data)} sites returned")
```

### R
```r
library(httr)
library(jsonlite)

resp <- GET(
  "https://metrics.mmcd.org/v1/private/air-checklist",
  query = list(facility = "Sr", zone = "1,2", priority = "YELLOW,RED", lookback_days = 2),
  add_headers(Authorization = "Bearer YOUR_KEY")
)
rows <- content(resp, as = "parsed")$data
```

### Google Apps Script
```javascript
const url = "https://metrics.mmcd.org/v1/private/air-checklist?facility=Sr&priority=RED";
const resp = UrlFetchApp.fetch(url, {
  headers: { "Authorization": "Bearer YOUR_KEY" },
  muteHttpExceptions: true,
});
const data = JSON.parse(resp.getContentText()).data;
Logger.log(data.length + " sites returned");
```

### JavaScript (fetch)
```javascript
const resp = await fetch(
  "https://metrics.mmcd.org/v1/private/air-checklist?facility=Sr&priority=RED",
  { headers: { "Authorization": "Bearer YOUR_KEY" } }
);
const { data } = await resp.json();
console.log(`${data.length} sites returned`);
```

---

## Notes

- **Facility codes are case-insensitive** in requests — the API normalizes to the DB-cased value (e.g. `sr` → `Sr`).
- **Priority values must be uppercase** in the `priority` parameter: `RED`, `YELLOW`, `BLUE`, `GREEN`, `PURPLE`.
- The `lookback_days` parameter on the air-checklist endpoint controls how far back to check for inspections (1–14 days). This is independent of the numdip threshold.
- The `/v1/public/threshold` endpoint returns the current **numdip threshold** (1 or 2) based on the ACT4-P1 spring date. Before that date the threshold is 1; on/after it becomes 2. It resets to 1 on January 1.
- Dates use ISO 8601 format (`YYYY-MM-DD`). The `as_of` parameter defaults to today and cannot be more than 2 years in the past.
