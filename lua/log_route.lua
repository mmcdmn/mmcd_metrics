-- =============================================================================
-- MMCD Dashboard - Connection Close Tracking
-- =============================================================================
-- Runs at the end of every request (log_by_lua_file).
-- Decrements the load counter when a tracked WebSocket connection closes.
-- WebSocket connections are long-lived (minutes/hours), so the load counter
-- stays incremented for the duration — which is exactly what we want.
--
-- NOTE: cosocket API is NOT available in the log_by_lua* phase, so we
-- defer the Redis call to a 0-delay timer (ngx.timer.at) which runs in
-- a context where cosocket IS available.
-- =============================================================================

local ws_worker = ngx.ctx.ws_worker
if not ws_worker then
    -- Not a tracked WebSocket connection — nothing to do
    return
end

-- Defer Redis work to a timer (cosocket not available in log phase)
local ok, err = ngx.timer.at(0, function(premature)
    if premature then return end

    local redis = require "resty.redis"
    local red = redis:new()
    red:set_timeouts(100, 100, 100)

    local conn_ok, conn_err = red:connect("127.0.0.1", 6379)
    if not conn_ok then
        ngx.log(ngx.WARN, "[route-log] Redis unavailable on WS close: ", conn_err)
        return
    end

    -- Decrement load counter
    local new_load, decr_err = red:decr("mmcd:load:" .. ws_worker)
    if decr_err then
        ngx.log(ngx.WARN, "[route-log] Failed to decr load for ", ws_worker, ": ", decr_err)
    end

    -- Floor at 0 (guard against drift from crashes / missed increments)
    if new_load and tonumber(new_load) < 0 then
        red:set("mmcd:load:" .. ws_worker, 0)
    end

    red:set_keepalive(10000, 100)
end)

if not ok then
    ngx.log(ngx.WARN, "[route-log] Failed to create timer for WS close: ", err)
end
