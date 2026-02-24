# MMCD Dashboard — Dynamic Load Balancer Architecture

## Overview

The MMCD Dashboard uses a **dynamic load-aware routing system** to distribute users across multiple Shiny Server worker processes inside a single container. The system replaces traditional hash-based or round-robin nginx routing with an intelligent Lua-based router that tracks live worker load through Redis and assigns new users to the least-loaded worker.

### Technology Stack

| Component | Role |
|-----------|------|
| **OpenResty** (nginx + LuaJIT) | Reverse proxy, runs Lua routing logic on every request |
| **Lua scripts** | Routing decisions, load tracking, session stickiness |
| **Redis** | Shared state store: worker registry, load counters, route mappings, decision log |
| **Shiny Server** (x N workers) | R application processes, each on a unique port |
| **AWS Fargate** (ECS) | Container runtime in production |
| **AWS ALB** | External load balancer that forwards to Fargate tasks |

---

## How It Works: End-to-End Request Flow

### Step 1 — Container Startup (`startup.sh`)

When the container starts:

1. **Redis server** starts on `127.0.0.1:6379` (embedded, in-container)
2. **Shiny Server instances** start on ports 3839, 3840, 3841, ... (one per `SHINY_WORKERS` count)
3. **Worker registration**: `startup.sh` writes each worker to:
   - A file `/etc/nginx/workers.list` (read once by Lua at init)
   - Redis SET `mmcd:workers` via `SADD`
   - Redis key `mmcd:load:<worker>` initialized to `0`
4. **Stale routes cleared**: Any leftover `mmcd:route:*` keys from a previous run are deleted
5. **OpenResty** starts with the Lua routing scripts, reading the worker list into memory

### Step 2 — A User Opens the Dashboard

When a user's browser hits `https://dashboard.mmcd.org/overview/`:

```
Browser → AWS ALB → Fargate Container:3838 (OpenResty) → Shiny Worker (port 3839-3841)
```

The ALB adds the user's real IP to the `X-Forwarded-For` header. OpenResty receives the request and runs the Lua routing script.

### Step 3 — Lua Routing Decision (`dynamic_route.lua`)

The Lua script runs on **every HTTP request** via OpenResty's `access_by_lua_file` directive. Here's the decision tree:

```
Request arrives
    │
    ▼
Extract client IP from X-Forwarded-For 
    │
    ▼
┌─────────────────────────────────────┐
│  L1 CACHE CHECK (nginx shared dict) │  ← Sub-millisecond, in-memory
│  Key: client IP → worker address    │
│  TTL: 30 seconds                    │
└───────────┬─────────────────────────┘
            │
     HIT?  YES → route to cached worker → DONE
            │
           NO
            ▼
┌─────────────────────────────────────┐
│  L2 CACHE CHECK (Redis)             │  ← Sub-millisecond, localhost
│  Key: mmcd:route:<client_ip>        │
│  TTL: 600 seconds (configurable)    │
└───────────┬─────────────────────────┘
            │
     HIT?  YES → cache in L1, route to worker → DONE
            │
           NO (new user or route expired)
            ▼
┌─────────────────────────────────────┐
│  LEAST-LOADED SELECTION             │
│                                     │
│  For each worker in mmcd:workers:   │
│    read mmcd:load:<worker>          │
│                                     │
│  Pick worker(s) with lowest load    │
│  If tie: round-robin among them     │
└───────────┬─────────────────────────┘
            │
            ▼
Store route: Redis SETEX mmcd:route:<ip> = worker (TTL 600s)
Cache in L1 shared dict (TTL 30s)
Log decision to mmcd:route_log
    │
    ▼
Route request to selected worker → DONE
```

### Step 4 — WebSocket Connection (Shiny Session)

After the initial HTTP page load, the browser upgrades to a **WebSocket** connection for real-time Shiny communication. The Lua router detects the `Upgrade: websocket` header and:

1. **INCR** `mmcd:load:<worker>` — increments the load counter for the assigned worker
2. Stores `ngx.ctx.ws_worker` to track which worker owns this connection

When the WebSocket closes (user navigates away, closes tab, or session times out), the `log_by_lua_file` script:

1. **DECR** `mmcd:load:<worker>` — decrements the load counter
2. Floors at 0 to prevent negative counts

This means **load counters reflect live WebSocket connections**, which is the best proxy for "how busy is this worker" since each Shiny session = one WebSocket.

### Step 5 — Route TTL Expiry and Rebalancing

Routes are **sticky for the TTL duration** (default 600 seconds == 10 minutes):

- While a user's route is active, ALL their requests go to the same worker (required for Shiny's stateful sessions)
- When the TTL expires, the next request triggers a fresh least-loaded evaluation
- If the user's original worker is now heavily loaded, they get reassigned to a lighter one

This provides a natural rebalancing mechanism. users whose sessions ended get redistributed on their next visit.

---

## Concrete Example: 3 Users Arrive

### Initial State

```
Workers: 3839 (load=0), 3840 (load=0), 3841 (load=0)
Routes: (none)
```

### User A connects (IP: 203.0.113.10)

1. L1 cache: MISS
2. Redis route: MISS
3. Query loads: 3839=0, 3840=0, 3841=0 → **all tied**
4. Round-robin counter = 1 → picks **3839** (index 1 of sorted list)
5. Store: `mmcd:route:203.0.113.10 = 127.0.0.1:3839` (TTL 600s)
6. Browser opens WebSocket → `mmcd:load:127.0.0.1:3839` incremented to **1**

```
Workers: 3839 (load=1), 3840 (load=0), 3841 (load=0)
Routes: 203.0.113.10 → 3839
```

### User B connects (IP: 198.51.100.20)

1. L1 cache: MISS
2. Redis route: MISS
3. Query loads: 3839=1, 3840=0, 3841=0 → **3840 and 3841 tied at 0**
4. Round-robin counter = 2 → picks **3840** (index 2 of tied candidates)
5. Store: `mmcd:route:198.51.100.20 = 127.0.0.1:3840` (TTL 600s)
6. Browser opens WebSocket → `mmcd:load:127.0.0.1:3840` incremented to **1**

```
Workers: 3839 (load=1), 3840 (load=1), 3841 (load=0)
Routes: 203.0.113.10 → 3839, 198.51.100.20 → 3840
```

### User C connects (IP: 192.0.2.30)

1. L1 cache: MISS
2. Redis route: MISS
3. Query loads: 3839=1, 3840=1, 3841=0 → **3841 is least loaded**
4. Picks **3841** (only candidate with load=0)
5. Store: `mmcd:route:192.0.2.30 = 127.0.0.1:3841` (TTL 600s)
6. Browser opens WebSocket → `mmcd:load:127.0.0.1:3841` incremented to **1**

```
Workers: 3839 (load=1), 3840 (load=1), 3841 (load=1)
Routes: 203.0.113.10 → 3839, 198.51.100.20 → 3840, 192.0.2.30 → 3841
```

**Result: Perfect 1-1-1 distribution across all 3 workers.**

### User A closes browser

1. WebSocket closes → `log_by_lua` decrements `mmcd:load:127.0.0.1:3839` to **0**
2. Route `mmcd:route:203.0.113.10` still exists (TTL hasn't expired)

```
Workers: 3839 (load=0), 3840 (load=1), 3841 (load=1)
```

### User D arrives (IP: 10.0.0.50)

1. Query loads: 3839=0, 3840=1, 3841=1 → **3839 is least loaded**
2. Picks **3839** — the worker that just freed up
3. Load goes to 3839=1, giving us 1-1-1 again

---

## What Each Component Does

### OpenResty (nginx + LuaJIT)

**Role**: Reverse proxy and routing engine

- Listens on port 3838 (the only externally exposed port)
- Runs Lua scripts at specific phases of each request:
  - `init_by_lua_file` — reads worker list once at startup
  - `access_by_lua_file` — routing decision on every request
  - `log_by_lua_file` — cleanup (decrement load) when connections close
- Proxies HTTP and WebSocket traffic to the selected backend worker
- Serves static assets directly (CSS, JS, images)
- Maintains an **L1 in-memory cache** (`lua_shared_dict`) for sub-millisecond route lookups

### Lua Scripts

| Script | Phase | Purpose |
|--------|-------|---------|
| `init_route.lua` | Master startup | Load worker list from file into shared memory |
| `dynamic_route.lua` | Every request | Routing decision: L1→L2→least-loaded selection, WebSocket tracking |
| `log_route.lua` | Request completion | Decrement worker load when WebSocket connections close |

The Lua code runs **inside the nginx event loop**. There is no external process or service call. This adds < 1ms to request latency.

### Redis

**Role**: Shared state store (embedded, localhost:6379)

Redis stores all routing state accessible to both Lua (routing) and R (monitoring):

| Key Pattern | Type | Purpose | TTL |
|-------------|------|---------|-----|
| `mmcd:workers` | SET | Registry of all worker `host:port` addresses | None |
| `mmcd:load:<worker>` | INT | Live WebSocket connection count per worker | None |
| `mmcd:route:<client_ip>` | STRING | Sticky route: which worker a client is assigned to | 600s |
| `mmcd:route_log` | LIST | Last 200 routing decisions (JSON) for monitoring | None |

Redis is also used for **application caching** (separate from routing). For query results, processed data, etc.

### Shiny Server Workers

**Role**: R application runtime

- Each worker is an independent Shiny Server process on its own port
- Workers share the same filesystem and Redis instance
- Each worker can serve **one interactive session at a time** per app (R is single-threaded)
- The load balancer ensures users are spread across workers so no single worker is overwhelmed

### AWS Fargate (Production)

**Role**: Container runtime

- Runs the Docker container with all components inside
- CPU: 4096 units (4 vCPU), Memory: 8192 MB
- Each Fargate task is one container with N Shiny workers + OpenResty + Redis inside
- No EC2 instances to manage — fully serverless container hosting

### AWS ALB (Production)

**Role**: External entry point

- Routes internet traffic to Fargate tasks
- Appends real client IP to `X-Forwarded-For` header
- The Lua router reads `X-Forwarded-For` to identify clients (not `remote_addr`, which would be the ALB's IP)
- Health checks hit the container on port 3838

---

## Why Not Standard Round-Robin or Hash?

### The Problem with Round-Robin

Standard nginx round-robin (`upstream` with default settings) distributes requests evenly across backends. But **Shiny apps are stateful**, meaning a user's session state lives in the worker process that served their first request. If Request 1 goes to Worker A and Request 2 goes to Worker B, the session breaks.

Round-robin is designed for stateless backends (e.g., REST APIs). Shiny needs sticky sessions.

### The Problem with Hash-Based Sticky Sessions

nginx's `ip_hash` or `hash $remote_addr consistent` pins a client to a worker based on their IP hash. This ensures stickiness but has a **birthday problem**:

- With 3 workers and 3 users, there's a **78% chance** at least two users hash to the same worker
- With 5 workers and 5 users, it gets a **52% chance**. Hash distribution is probabilistic, not guaranteed

A real scenario: 2 users stuck on Worker A (each queued behind the other) while Worker C sits idle. Users experience 5-10 second delays waiting for their turn.

### Our Solution: Least-Loaded Assignment

Instead of hashing, we **explicitly assign** each new user to whichever worker currently has the fewest active WebSocket connections:

- **Guaranteed even distribution**: No birthday problem; if workers have loads [2, 0, 1], the next user always goes to the worker with 0
- **Automatic rebalancing**: When users disconnect, load counters drop, and new users fill the freed workers
- **Sticky sessions preserved**: Once assigned, a user stays on their worker for the TTL duration (10 minutes)
- **Sub-millisecond overhead**: L1 cache hits (majority of requests) add < 0.1ms; fresh assignments add < 1ms

---

## Configuration

| Environment Variable | Default | Description |
|---------------------|---------|-------------|
| `SHINY_WORKERS` | `5` | Number of Shiny Server worker processes |
| `ROUTE_TTL_SECONDS` | `600` | How long a user's route sticks (seconds) |
| `ENABLE_NGINX` | `true` | Enable load balancer (set `false` for single-worker mode) |

---

## Fallback Behavior

If Redis becomes unavailable:

1. Lua detects the connection failure (100ms timeout)
2. Falls back to **consistent hash** on client IP bytes (same behavior as old static hash)
3. Logs a warning: `[route] Redis unavailable — using hash fallback`
4. The L1 shared dict cache continues working, so already-routed users are unaffected for 30s
5. When Redis recovers, new routing decisions resume automatically

---

## Monitoring

The **test-app** includes a "Dynamic Routing" tab that reads Redis state directly:

| Panel | Data Source | Shows |
|-------|-------------|-------|
| Worker Load (Live) | `mmcd:load:*` keys | Active WebSocket count per worker |
| Configuration | `workers.list` file + Redis metadata | System config, worker count, route count |
| Active Route Mappings | `mmcd:route:*` keys | Which IP is pinned to which worker, with TTL |
| Routing Decision Log | `mmcd:route_log` list | Last 50 routing decisions with timestamps and load snapshots |
| nginx Access Log | `/var/log/nginx/access.log` | Raw request log with upstream worker shown |

---

## Architecture Diagram

```
```
┌──────────────────────────────────────────────────────────────┐
│                    AWS Fargate Task                          │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐  │
│  │              OpenResty (port 3838)                     │  │
│  │                                                        │  │
│  │  ┌─────────────┐  ┌───────────────────────────────┐    │  │
│  │  │ init_route  │  │    dynamic_route.lua          │    │  │
│  │  │   .lua      │  │                               │    │  │
│  │  │             │  │  1. Extract client IP         │    │  │
│  │  │ Load worker │  │  2. L1 cache check            │    │  │
│  │  │ list at     │  │  3. Redis route check         │    │  │
│  │  │ startup     │  │  4. Least-loaded pick         │    │  │
│  │  │             │  │  5. WebSocket tracking        │    │  │
│  │  └─────────────┘  └──────────┬────────────────────┘    │  │
│  │                              │                         │  │
│  │              ┌───────────────┼───────────────┐         │  │
│  │              │               │               │         │  │
│  │              ▼               ▼               ▼         │  │
│  │     ┌──────────────┐ ┌──────────────┐ ┌──────────────┐ │  │
│  │     │ Shiny Worker │ │ Shiny Worker │ │ Shiny Worker │ │  │
│  │     │  port 3839   │ │  port 3840   │ │  port 3841   │ │  │
│  │     │              │ │              │ │              │ │  │
│  │     │  R process   │ │  R process   │ │  R process   │ │  │
│  │     │ (1 session   │ │ (1 session   │ │ (1 session   │ │  │
│  │     │  at a time)  │ │  at a time)  │ │  at a time)  │ │  │
│  │     └──────────────┘ └──────────────┘ └──────────────┘ │  │
│  │                                                        │  │
│  └────────────────────────────────────────────────────────┘  │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐  │
│  │              Redis (port 6379)                         │  │
│  │                                                        │  │
│  │  mmcd:workers = {3839, 3840, 3841}                     │  │
│  │  mmcd:load:127.0.0.1:3839 = 1                          │  │
│  │  mmcd:load:127.0.0.1:3840 = 1                          │  │
│  │  mmcd:load:127.0.0.1:3841 = 0                          │  │
│  │  mmcd:route:203.0.113.10 = 127.0.0.1:3839 [TTL]        │  │
│  │  mmcd:route:198.51.100.20 = 127.0.0.1:3840 [TTL]       │  │
│  │  mmcd:route_log = [JSON, JSON, ...]                    │  │
│  └────────────────────────────────────────────────────────┘  │
│                                                              │
└──────────────────────────────────────────────────────────────┘
            ▲
            │  port 3838
            │
┌───────────┴──────────────┐
│       AWS ALB            │
│  (X-Forwarded-For: IP)   │
└───────────┬──────────────┘
            │
            ▼
        Internet
        (Users)
```

---

## File Reference

| File | Purpose |
|------|---------|
| `lua/init_route.lua` | Reads `workers.list` into shared memory at OpenResty startup |
| `lua/dynamic_route.lua` | Core routing logic: L1→L2→least-loaded, WebSocket tracking |
| `lua/log_route.lua` | Decrements load counter when WebSocket connections close |
| `nginx.conf` | OpenResty configuration with Lua directives and proxy settings |
| `startup.sh` | Container entrypoint: starts Redis, Shiny workers, registers in Redis, starts OpenResty |
| `dockerfile` | Container image build with OpenResty installation |
| `apps/test-app/app.R` | Monitoring UI: Dynamic Routing tab reads Redis for live metrics |
