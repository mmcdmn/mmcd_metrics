-- =============================================================================
-- MMCD Dashboard - Dynamic Load-Aware Routing
-- =============================================================================
-- Runs on every request (access_by_lua_file).
--
-- Algorithm:
--   1. Get client IP (from X-Forwarded-For or remote_addr)
--   2. Check L1 cache (shared dict) for existing route → fast path
--   3. Check L2 cache (Redis mmcd:route:<ip>) → sticky session
--   4. If no route or expired: query worker loads from Redis, pick least
--      loaded, store route with configurable TTL (default 10min)
--   5. Track WebSocket connections: INCR load on upgrade, DECR on close
--
-- Fallback: If Redis is unavailable, use consistent hash on client IP
--           (same behavior as the old static hash approach).
-- =============================================================================

local redis  = require "resty.redis"
local cjson  = require "cjson.safe"

-- ---- Configuration from shared dicts ----
local config      = ngx.shared.config
local route_cache = ngx.shared.route_cache

local REDIS_HOST    = "127.0.0.1"
local REDIS_PORT    = 6379
local REDIS_TIMEOUT = 100   -- ms — keep fast, fall back on timeout
local ROUTE_TTL     = config:get("route_ttl") or 600  -- seconds
local L1_TTL        = 30    -- seconds — L1 cache lifetime

-- Queue retry detection: when the request is being retried after a 502/503,
-- skip cached routes to allow fresh worker load evaluation.
local is_queue_retry = (tonumber(ngx.req.get_headers()["X-Queue-Retries"]) or 0) > 0

-- ---- Helper: parse worker list from config shared dict ----
local function get_workers()
    local workers_str = config:get("workers")
    if not workers_str then return nil end
    local workers = {}
    for w in workers_str:gmatch("[^\n]+") do
        workers[#workers + 1] = w
    end
    return workers
end

-- ---- Helper: deterministic hash fallback (no Redis) ----
local function fallback_hash(ip, workers)
    if not workers or #workers == 0 then return nil end
    local sum = 0
    for i = 1, #ip do
        sum = sum + string.byte(ip, i)
    end
    return workers[(sum % #workers) + 1]
end

-- ---- Helper: get a pooled Redis connection ----
local function get_redis()
    local red = redis:new()
    red:set_timeouts(REDIS_TIMEOUT, REDIS_TIMEOUT, REDIS_TIMEOUT)
    local ok, err = red:connect(REDIS_HOST, REDIS_PORT)
    if not ok then
        return nil, err
    end
    return red
end

local function release_redis(red)
    if red then red:set_keepalive(10000, 100) end
end

-- =====================================================================
-- MAIN ROUTING LOGIC
-- =====================================================================

-- 1. Determine client identity
local client_ip = ngx.var.http_x_forwarded_for or ngx.var.remote_addr
if client_ip then
    -- Take first IP if comma-separated (ALB appends its own)
    client_ip = client_ip:match("^%s*([^,]+)")
end
if not client_ip or client_ip == "" then
    client_ip = ngx.var.remote_addr or "unknown"
end

local workers = get_workers()
if not workers or #workers == 0 then
    -- No worker list — can't route, let nginx error naturally
    ngx.log(ngx.ERR, "[route] No workers configured — cannot route")
    return
end

-- 2. L1 cache check (shared dict — sub-millisecond)
--    Skip on queue retries to force fresh evaluation
if not is_queue_retry then
    local cached = route_cache:get(client_ip)
    if cached then
        ngx.var.backend = cached
        -- Track WebSocket for load counting
        local upgrade = ngx.var.http_upgrade
        if upgrade and upgrade:lower() == "websocket" then
            local red = get_redis()
            if red then
                red:incr("mmcd:load:" .. cached)
                ngx.ctx.ws_worker = cached
                release_redis(red)
            end
        end
        return
    end
end

-- 3. Redis check (L2 — sub-millisecond for local Redis)
local red, err = get_redis()
if not red then
    -- Redis down → fallback to hash
    ngx.log(ngx.WARN, "[route] Redis unavailable: ", err, " — using hash fallback")
    local target = fallback_hash(client_ip, workers)
    if target then
        ngx.var.backend = target
        route_cache:set(client_ip, target, L1_TTL)
    end
    return
end

local route_key = "mmcd:route:" .. client_ip
local cached_worker = red:get(route_key)

if not is_queue_retry and cached_worker and cached_worker ~= ngx.null then
    -- Existing sticky route still valid (skip on retry to allow rebalancing)
    ngx.var.backend = cached_worker
    route_cache:set(client_ip, cached_worker, L1_TTL)

    -- Track WebSocket
    local upgrade = ngx.var.http_upgrade
    if upgrade and upgrade:lower() == "websocket" then
        red:incr("mmcd:load:" .. cached_worker)
        ngx.ctx.ws_worker = cached_worker
    end

    release_redis(red)
    return
end

-- 4. No cached route — find least-loaded worker
local min_load = math.huge
local candidates = {}

for _, worker in ipairs(workers) do
    local load_val = red:get("mmcd:load:" .. worker)
    local load = tonumber(load_val) or 0
    if load < min_load then
        min_load   = load
        candidates = { worker }
    elseif load == min_load then
        candidates[#candidates + 1] = worker
    end
end

-- Tie-breaking: round-robin among equally-loaded workers
local target
if #candidates > 1 then
    local rr = route_cache:incr("__rr_counter", 1, 0)
    target = candidates[(rr % #candidates) + 1]
elseif #candidates == 1 then
    target = candidates[1]
else
    local rr = route_cache:incr("__rr_counter", 1, 0)
    target = workers[(rr % #workers) + 1]
end

-- 5. Store route with TTL
red:setex(route_key, ROUTE_TTL, target)
route_cache:set(client_ip, target, L1_TTL)
ngx.var.backend = target

-- Track WebSocket
local upgrade = ngx.var.http_upgrade
if upgrade and upgrade:lower() == "websocket" then
    red:incr("mmcd:load:" .. target)
    ngx.ctx.ws_worker = target
end

-- 6. Log routing decision (for monitoring — keep last 200)
local log_entry = cjson.encode({
    time       = ngx.time(),
    time_fmt   = ngx.cookie_time(ngx.time()),
    client_ip  = client_ip,
    target     = target,
    load       = min_load,
    reason     = is_queue_retry and "queue_retry" or "least_loaded",
    all_loads  = (function()
        local t = {}
        for _, w in ipairs(workers) do
            local l = red:get("mmcd:load:" .. w)
            t[w] = tonumber(l) or 0
        end
        return t
    end)()
})
if log_entry then
    red:lpush("mmcd:route_log", log_entry)
    red:ltrim("mmcd:route_log", 0, 199)
end

release_redis(red)
