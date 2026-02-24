-- =============================================================================
-- MMCD Dashboard - Request Queue Handler
-- =============================================================================
-- Triggered by `error_page 502 503 = @queue` when a Shiny worker returns
-- "Service Unavailable" because it's at capacity.
--
-- Instead of returning an instant 503 to the user, this handler:
--   1. Sleeps for RETRY_DELAY seconds (non-blocking via event loop)
--   2. Clears the route cache so dynamic_route.lua re-evaluates worker loads
--   3. Does an internal redirect back to the original URI for another attempt
--   4. Repeats up to MAX_RETRIES times
--   5. If still failing, returns a friendly "server busy" page
--
-- Non-blocking: ngx.sleep() yields the coroutine, not the nginx worker
-- process. Hundreds of queued requests can wait concurrently without
-- consuming threads.
--
-- Queue stats are tracked in lua_shared_dict "queue_stats" for monitoring.
-- =============================================================================

local config      = ngx.shared.config
local route_cache = ngx.shared.route_cache
local queue_stats = ngx.shared.queue_stats

-- ---- Configuration (set in init_route.lua, overridable via env vars) ----
local MAX_RETRIES  = config:get("queue_max_retries") or 20
local RETRY_DELAY  = config:get("queue_retry_delay") or 2    -- seconds

-- ---- Read retry count from request header ----
local retries = tonumber(ngx.req.get_headers()["X-Queue-Retries"]) or 0

-- ---- Track queue statistics ----
if retries == 0 then
    -- First time entering the queue for this request
    queue_stats:incr("total_queued", 1, 0)
end
queue_stats:incr("total_retries", 1, 0)

-- ---- Check if retries exhausted ----
if retries >= MAX_RETRIES then
    queue_stats:incr("total_exhausted", 1, 0)

    local wait_time = retries * RETRY_DELAY

    ngx.status = 503
    ngx.header["Content-Type"] = "text/html; charset=utf-8"
    ngx.header["Retry-After"] = "10"
    ngx.say([[<!DOCTYPE html>
<html>
<head>
  <title>Server Busy - MMCD Metrics</title>
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
    .sub { font-size: 0.85em; color: #999; margin-top: 15px; }
    .btn {
      display: inline-block; margin-top: 20px; padding: 10px 24px;
      background: #3498db; color: white; border-radius: 6px;
      text-decoration: none; font-weight: 600; cursor: pointer;
      border: none; font-size: 1em;
    }
    .btn:hover { background: #2980b9; }
  </style>
</head>
<body>
<div class="card">
  <h1>&#x23F3; Server Busy</h1>
  <p>All workers are currently at capacity.<br>
     Your request was queued for <strong>]] .. wait_time .. [[ seconds</strong>
     but could not be served.</p>
  <p>Please wait a moment and try again.</p>
  <a class="btn" href="javascript:location.reload()">&#x21bb; Retry Now</a>
  <p class="sub">If this persists, try again in a few minutes.<br>
     The system auto-scales under heavy load.</p>
</div>
</body>
</html>]])
    return
end

-- ---- Increment retry count ----
retries = retries + 1
ngx.req.set_header("X-Queue-Retries", tostring(retries))

-- ---- Clear route cache for this client IP ----
-- Forces dynamic_route.lua to re-evaluate worker loads on the next attempt,
-- potentially picking a less-loaded worker instead of the same busy one.
local client_ip = ngx.var.http_x_forwarded_for or ngx.var.remote_addr
if client_ip then
    client_ip = client_ip:match("^%s*([^,]+)")
end
if client_ip and client_ip ~= "" then
    route_cache:delete(client_ip)
end

ngx.log(ngx.INFO, "[queue] Retry ", retries, "/", MAX_RETRIES,
        " for ", client_ip or "unknown",
        " — sleeping ", RETRY_DELAY, "s before re-routing")

-- ---- Wait before retrying (non-blocking) ----
ngx.sleep(RETRY_DELAY)

-- ---- Internal redirect back to original URI ----
-- Re-enters location / → access_by_lua (fresh route evaluation) → proxy_pass
return ngx.exec(ngx.var.request_uri)
