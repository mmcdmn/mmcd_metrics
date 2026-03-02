-- =============================================================================
-- MMCD Dashboard - Unified Proxy Handler with Request Queue
-- =============================================================================
-- Replaces the old access_by_lua + error_page + @queue chain.
--
-- This single script handles the ENTIRE request lifecycle:
--   1. Route selection (same logic as dynamic_route.lua)
--   2. Proxy the request via ngx.location.capture to /proxy_backend
--   3. If 502/503: sleep, clear cache, re-route, retry (up to MAX_RETRIES)
--   4. If still failing: return auto-refreshing HTML page
--
-- WHY this approach?
--   The old error_page → @queue → ngx.exec() approach chains internal
--   redirects. nginx caps these at 10, and request headers (X-Queue-Retries)
--   don't persist across ngx.exec(). This caused infinite redirect loops
--   and raw 500/503 errors leaking to users.
--
--   By doing the retry loop ENTIRELY in Lua with ngx.location.capture(),
--   we avoid internal redirects completely. The retry counter is a local
--   variable — no header persistence needed.
--
-- Non-blocking: ngx.sleep() yields the Lua coroutine, not the nginx worker.
-- Hundreds of queued requests can wait concurrently.
-- =============================================================================

local redis  = require "resty.redis"
local cjson  = require "cjson.safe"

local config      = ngx.shared.config
local route_cache = ngx.shared.route_cache
local queue_stats = ngx.shared.queue_stats

-- ---- Configuration ----
local REDIS_HOST    = "127.0.0.1"
local REDIS_PORT    = 6379
local REDIS_TIMEOUT = 100
local ROUTE_TTL     = config:get("route_ttl") or 600
local L1_TTL        = 30
local MAX_RETRIES   = config:get("queue_max_retries") or 10
local RETRY_DELAY   = config:get("queue_retry_delay") or 1

-- ---- Helper functions (same as dynamic_route.lua) ----

local function get_workers()
    local workers_str = config:get("workers")
    if not workers_str then return nil end
    local workers = {}
    for w in workers_str:gmatch("[^\n]+") do
        workers[#workers + 1] = w
    end
    return workers
end

local function fallback_hash(ip, workers)
    if not workers or #workers == 0 then return nil end
    local sum = 0
    for i = 1, #ip do
        sum = sum + string.byte(ip, i)
    end
    return workers[(sum % #workers) + 1]
end

local function get_redis()
    local red = redis:new()
    red:set_timeouts(REDIS_TIMEOUT, REDIS_TIMEOUT, REDIS_TIMEOUT)
    local ok, err = red:connect(REDIS_HOST, REDIS_PORT)
    if not ok then return nil, err end
    return red
end

local function release_redis(red)
    if red then red:set_keepalive(10000, 100) end
end

-- ---- Route selection (returns target worker address) ----
local function select_route(client_ip, workers, is_retry)
    -- L1 cache (skip on retry to force fresh evaluation)
    if not is_retry then
        local cached = route_cache:get(client_ip)
        if cached then return cached, "l1_cache" end
    end

    -- Redis L2 cache + least-loaded selection
    local red, err = get_redis()
    if not red then
        ngx.log(ngx.WARN, "[route] Redis unavailable: ", err, " — hash fallback")
        local target = fallback_hash(client_ip, workers)
        if target then route_cache:set(client_ip, target, L1_TTL) end
        return target, "hash_fallback"
    end

    -- Check L2 (sticky route in Redis)
    if not is_retry then
        local route_key = "mmcd:route:" .. client_ip
        local cached_worker = red:get(route_key)
        if cached_worker and cached_worker ~= ngx.null then
            route_cache:set(client_ip, cached_worker, L1_TTL)
            release_redis(red)
            return cached_worker, "l2_cache"
        end
    end

    -- Find least-loaded worker
    local min_load = math.huge
    local candidates = {}
    local all_loads = {}

    for _, worker in ipairs(workers) do
        local load_val = red:get("mmcd:load:" .. worker)
        local load = tonumber(load_val) or 0
        all_loads[worker] = load
        if load < min_load then
            min_load   = load
            candidates = { worker }
        elseif load == min_load then
            candidates[#candidates + 1] = worker
        end
    end

    -- Tie-breaking: round-robin
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

    -- Store route
    local route_key = "mmcd:route:" .. client_ip
    red:setex(route_key, ROUTE_TTL, target)
    route_cache:set(client_ip, target, L1_TTL)

    -- Log routing decision
    local reason = is_retry and "queue_retry" or "least_loaded"
    local log_entry = cjson.encode({
        time      = ngx.time(),
        time_fmt  = ngx.cookie_time(ngx.time()),
        client_ip = client_ip,
        target    = target,
        load      = min_load,
        reason    = reason,
        all_loads = all_loads
    })
    if log_entry then
        red:lpush("mmcd:route_log", log_entry)
        red:ltrim("mmcd:route_log", 0, 199)
    end

    release_redis(red)
    return target, reason
end

-- ---- WebSocket load tracking ----
local function track_ws_connect(target)
    local upgrade = ngx.var.http_upgrade
    if not upgrade or upgrade:lower() ~= "websocket" then return end
    local red = get_redis()
    if red then
        red:incr("mmcd:load:" .. target)
        release_redis(red)
    end
    return target  -- return so we can decrement on close
end

local function track_ws_close(ws_worker)
    if not ws_worker then return end
    local red = get_redis()
    if not red then return end
    local new_load = red:decr("mmcd:load:" .. ws_worker)
    if new_load and tonumber(new_load) < 0 then
        red:set("mmcd:load:" .. ws_worker, 0)
    end
    release_redis(red)
end

-- =====================================================================
-- MAIN REQUEST HANDLING
-- =====================================================================

-- 0. WebSocket requests need direct proxy (capture can't handle upgrades)
--    BUT first, probe the backend via HTTP to ensure the app is loaded.
--    If the initial worker returns 404, try others before exec'ing to @websocket.
local upgrade = ngx.var.http_upgrade
if upgrade and upgrade:lower() == "websocket" then
    -- Extract the base app path from the URI for the probe
    -- e.g. /apps/drone/__sockjs__/... → /apps/drone/
    --       /drone/__sockjs__/...     → /drone/
    local probe_path = ngx.var.request_uri:match("^(/[^_]+/)")
    if not probe_path then
        probe_path = ngx.var.request_uri:match("^(/[^/]+/)")
    end

    if probe_path then
        -- Quick probe to find a worker that has this app loaded
        local ws_workers = get_workers()
        local ws_client_ip = ngx.var.http_x_forwarded_for or ngx.var.remote_addr
        if ws_client_ip then ws_client_ip = ws_client_ip:match("^%s*([^,]+)") end
        if not ws_client_ip or ws_client_ip == "" then ws_client_ip = ngx.var.remote_addr or "unknown" end

        if ws_workers and #ws_workers > 0 then
            -- First try the routed worker
            local ws_target = select_route(ws_client_ip, ws_workers, false)
            if ws_target then
                ngx.req.set_header("X-Backend-Target", ws_target)
                local probe = ngx.location.capture("/proxy_upstream" .. probe_path, {
                    method = ngx.HTTP_HEAD,
                })
                if probe and probe.status and probe.status ~= 404 then
                    ngx.var.backend = ws_target
                    return ngx.exec("@websocket")
                end
                -- First worker returned 404 — try all others
                for _, w in ipairs(ws_workers) do
                    if w ~= ws_target then
                        ngx.req.set_header("X-Backend-Target", w)
                        local p2 = ngx.location.capture("/proxy_upstream" .. probe_path, {
                            method = ngx.HTTP_HEAD,
                        })
                        if p2 and p2.status and p2.status ~= 404 then
                            -- Update sticky route to this working worker
                            route_cache:set(ws_client_ip, w, L1_TTL)
                            local red = get_redis()
                            if red then
                                red:setex("mmcd:route:" .. ws_client_ip, ROUTE_TTL, w)
                                release_redis(red)
                            end
                            ngx.var.backend = w
                            return ngx.exec("@websocket")
                        end
                    end
                end
            end
        end
    end
    -- Fallback: exec to @websocket with whatever route we have
    return ngx.exec("@websocket")
end

-- 1. Determine client IP
local client_ip = ngx.var.http_x_forwarded_for or ngx.var.remote_addr
if client_ip then
    client_ip = client_ip:match("^%s*([^,]+)")
end
if not client_ip or client_ip == "" then
    client_ip = ngx.var.remote_addr or "unknown"
end

local workers = get_workers()
if not workers or #workers == 0 then
    ngx.log(ngx.ERR, "[proxy] No workers configured")
    ngx.status = 503
    ngx.header["Content-Type"] = "text/plain"
    ngx.say("No workers available")
    return
end

-- 2. Route + proxy + retry loop
local ws_worker = nil
local last_status = 0

-- Pre-compute request details (don't change per attempt)
local uri = ngx.var.request_uri
local method = ngx.req.get_method()
ngx.req.read_body()
local body = ngx.req.get_body_data()

local method_map = {
    GET     = ngx.HTTP_GET,
    POST    = ngx.HTTP_POST,
    PUT     = ngx.HTTP_PUT,
    DELETE  = ngx.HTTP_DELETE,
    HEAD    = ngx.HTTP_HEAD,
    OPTIONS = ngx.HTTP_OPTIONS,
    PATCH   = ngx.HTTP_PATCH,
}

for attempt = 0, MAX_RETRIES do
    local is_retry = (attempt > 0)

    if is_retry then
        -- Clear L1 cache so we pick a different/less-loaded worker
        route_cache:delete(client_ip)

        -- Stats
        queue_stats:incr("total_retries", 1, 0)

        -- Add jitter to avoid thundering herd (all retries at same instant)
        local jitter = math.random() * RETRY_DELAY * 0.5  -- 0-50% of delay
        local sleep_time = RETRY_DELAY + jitter

        ngx.log(ngx.WARN, "[queue] Retry ", attempt, "/", MAX_RETRIES,
                " for ", client_ip, " — sleeping ", string.format("%.1f", sleep_time), "s")
        ngx.sleep(sleep_time)
    end

    -- On retries, try ALL workers (shuffled) rather than just the selected one.
    -- This drastically improves success rate when some workers are free and others busy.
    local targets_to_try
    if is_retry then
        -- Shuffle workers to avoid thundering herd all hitting the same worker
        targets_to_try = {}
        for _, w in ipairs(workers) do targets_to_try[#targets_to_try + 1] = w end
        for i = #targets_to_try, 2, -1 do
            local j = math.random(i)
            targets_to_try[i], targets_to_try[j] = targets_to_try[j], targets_to_try[i]
        end
    else
        local target, reason = select_route(client_ip, workers, false)
        if not target then
            ngx.log(ngx.ERR, "[proxy] No target from route selection")
            goto continue
        end
        targets_to_try = { target }
    end

    local all_404_this_attempt = true  -- assume all 404 until proven otherwise

    for _, target in ipairs(targets_to_try) do
        -- Set the backend target as a request header so the subrequest can read it
        ngx.req.set_header("X-Backend-Target", target)

        local res = ngx.location.capture("/proxy_upstream" .. uri, {
            method = method_map[method] or ngx.HTTP_GET,
            body   = body,
            always_forward_body = true,
        })

        if res and res.status and res.status ~= 502 and res.status ~= 503 and res.status ~= 404 then
            -- Success (or a non-retryable client error)
            ngx.status = res.status

            -- Build client-facing base URL for Location header rewriting.
            -- Internal workers (127.0.0.1:PORT) must never leak to the client.
            local client_base_url = (ngx.var.http_x_forwarded_proto or ngx.var.scheme)
                                    .. "://"
                                    .. (ngx.var.http_host or ngx.var.host)

            -- Forward response headers
            for k, v in pairs(res.header) do
                -- Skip hop-by-hop headers that nginx manages
                local lk = k:lower()
                if lk ~= "connection" and lk ~= "transfer-encoding" then
                    -- Rewrite Location headers: replace internal backend
                    -- addresses with the client-facing URL so browsers never
                    -- receive (and cache) redirects to 127.0.0.1:PORT
                    if lk == "location" and type(v) == "string" then
                        v = v:gsub("https?://127%.0%.0%.1:%d+", client_base_url)
                        v = v:gsub("https?://localhost:%d+", client_base_url)
                    end
                    ngx.header[k] = v
                end
            end

            -- Track WebSocket
            ws_worker = track_ws_connect(target)

            -- Update sticky route to the worker that succeeded
            if is_retry then
                route_cache:set(client_ip, target, L1_TTL)
                local red = get_redis()
                if red then
                    red:setex("mmcd:route:" .. client_ip, ROUTE_TTL, target)
                    release_redis(red)
                end
            end

            -- Send response body
            if res.body then
                ngx.print(res.body)
            end
            return
        end

        -- Got 404/502/503 — this worker doesn't have the app or is busy, try next
        if not res or not res.status or res.status ~= 404 then
            all_404_this_attempt = false  -- at least one non-404 response
        end
        last_status = (res and res.status) or 503
    end

    if attempt == 0 then
        queue_stats:incr("total_queued", 1, 0)
    end

    -- SMART 404 HANDLING: If we tried ALL workers (retry attempt) and
    -- every single one returned 404, the resource genuinely doesn't exist.
    -- Stop immediately — don't waste 15+ seconds retrying a dead URL.
    -- This handles: expired sessions, invalid URLs, missing resources.
    if is_retry and all_404_this_attempt and #targets_to_try == #workers then
        ngx.log(ngx.INFO, "[queue] All ", #workers, " workers returned 404 for ",
                uri, " — resource not found, stopping retries")
        break
    end

    ::continue::
end

-- 3. All retries exhausted — return appropriate error page
queue_stats:incr("total_exhausted", 1, 0)

-- If the last status was 404 (resource not found on ALL workers),
-- return a proper 404 page, not the "busy" 503 page.
if last_status == 404 then
    ngx.log(ngx.INFO, "[queue] Resource not found on any worker for ", client_ip, ": ", uri)
    ngx.status = 404
    ngx.header["Content-Type"] = "text/html; charset=utf-8"
    ngx.say([[<!DOCTYPE html>
<html>
<head>
  <title>Page Not Found - MMCD Metrics</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      display: flex; justify-content: center; align-items: center;
      min-height: 80vh; margin: 0; background: #f5f5f5;
    }
    .card {
      background: white; border-radius: 12px; padding: 40px 50px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1); text-align: center;
      max-width: 500px;
    }
    h1   { color: #e74c3c; margin-bottom: 10px; }
    p    { color: #555; line-height: 1.6; }
    a    { color: #2980b9; text-decoration: none; font-weight: 600; }
    a:hover { text-decoration: underline; }
    .sub { font-size: 0.85em; color: #999; margin-top: 15px; }
  </style>
</head>
<body>
<div class="card">
  <h1>Page Not Found</h1>
  <p>The page you requested could not be found.<br>
     Your session may have expired.</p>
  <p><a href="/">&#x2190; Return to Dashboard</a></p>
  <p class="sub">If this keeps happening, try refreshing the app page.</p>
</div>
</body>
</html>]])
    return
end

local wait_time = MAX_RETRIES * RETRY_DELAY

ngx.log(ngx.WARN, "[queue] Exhausted ", MAX_RETRIES, " retries for ", client_ip,
        " after ", wait_time, "s — returning auto-refresh page")

ngx.status = 503
ngx.header["Content-Type"] = "text/html; charset=utf-8"
ngx.header["Retry-After"] = "5"
ngx.say([[<!DOCTYPE html>
<html>
<head>
  <title>Loading - MMCD Metrics</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta http-equiv="refresh" content="5">
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      display: flex; justify-content: center; align-items: center;
      min-height: 80vh; margin: 0; background: #f5f5f5;
    }
    .card {
      background: white; border-radius: 12px; padding: 40px 50px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1); text-align: center;
      max-width: 500px;
    }
    h1   { color: #2980b9; margin-bottom: 10px; }
    p    { color: #555; line-height: 1.6; }
    .sub { font-size: 0.85em; color: #999; margin-top: 15px; }
    .spinner {
      width: 40px; height: 40px; margin: 20px auto;
      border: 4px solid #e0e0e0; border-top: 4px solid #3498db;
      border-radius: 50%; animation: spin 1s linear infinite;
    }
    @keyframes spin { to { transform: rotate(360deg); } }
  </style>
</head>
<body>
<div class="card">
  <h1>&#x23F3; Almost There!</h1>
  <div class="spinner"></div>
  <p>The dashboard is currently handling a lot of traffic.<br>
     Your page will <strong>automatically reload</strong> in a few seconds.</p>
  <p class="sub">Please wait &mdash; retrying automatically&hellip;</p>
</div>
</body>
</html>]])
