# =============================================================================
# MMCD METRICS - REDIS DISTRIBUTED CACHE LAYER
# =============================================================================
# Drop-in replacement for file-based .rds caching.
# Uses AWS ElastiCache (Redis) so all Fargate containers share one cache.
#
# ENVIRONMENT VARIABLES:
#   REDIS_HOST     – Redis hostname (optional override)
#   REDIS_PORT     – Port (optional override)
#   REDIS_PASSWORD – AUTH token if required (optional)
#   REDIS_DB       – Database index 0-15 (optional override)
#   REDIS_PREFIX   – Key prefix for namespacing (optional override)
#   CACHE_BACKEND  – "redis" to enable, "file" to fall back (optional override)
#
# USAGE:
#   source("../../shared/redis_cache.R")
#   redis_set("my_key", my_dataframe, ttl = 3600)
#   data <- redis_get("my_key")
#
# DESIGN PRINCIPLES:
#   1. All R objects are serialized with serialize()/unserialize() so any
#      R type (data.frame, list, etc.) round-trips perfectly.
#   2. Every public function has a file-based fallback so local dev works
#      without Redis running.
#   3. The module is sourced once at startup; the connection is lazy-init.
# =============================================================================

# =============================================================================
# CONFIGURATION
# =============================================================================

REDIS_CONFIG <- list(
  # Hardcoded defaults for local shared Redis (Docker network)
  host     = "mmcd-redis",
  port     = 6379L,
  password = "",
  db       = 0L,
  prefix   = "mmcd:",
  backend  = "redis"
)

# =============================================================================
# CONNECTION MANAGEMENT
# =============================================================================

# Lazy-initialised connection (one per R process)
.redis_conn <- new.env(parent = emptyenv())

#' Get (or create) the Redis connection
#' @return A redux redis connection object, or NULL if unavailable
get_redis <- function() {
  # Already connected?
  if (exists("conn", envir = .redis_conn)) {
    conn <- get("conn", envir = .redis_conn)
    # Quick health check
    ok <- tryCatch({ conn$PING(); TRUE }, error = function(e) FALSE)
    if (ok) return(conn)
    # Dead connection – clear and reconnect
    rm("conn", envir = .redis_conn)
  }

  # Not using Redis?
  if (REDIS_CONFIG$backend != "redis") return(NULL)

  # redux must be installed
  if (!requireNamespace("redux", quietly = TRUE)) {
    message("[redis_cache] redux package not installed – falling back to file cache")
    REDIS_CONFIG$backend <<- "file"
    return(NULL)
  }

  tryCatch({
    conn <- redux::hiredis(
      host     = REDIS_CONFIG$host,
      port     = REDIS_CONFIG$port,
      password = if (nchar(REDIS_CONFIG$password) > 0) REDIS_CONFIG$password else NULL,
      db       = REDIS_CONFIG$db
    )
    assign("conn", conn, envir = .redis_conn)
    message(sprintf("[redis_cache] Connected to Redis at %s:%d (db %d)",
                    REDIS_CONFIG$host, REDIS_CONFIG$port, REDIS_CONFIG$db))
    conn
  }, error = function(e) {
    message(sprintf("[redis_cache] Connection failed (%s) – falling back to file cache", e$message))
    REDIS_CONFIG$backend <<- "file"
    NULL
  })
}

#' Check if Redis backend is active
#' @return TRUE if Redis is connected and responding
redis_is_active <- function() {
  if (REDIS_CONFIG$backend != "redis") return(FALSE)
  conn <- get_redis()
  !is.null(conn)
}

#' Build a full Redis key with namespace prefix
#' @param key Short key name (e.g. "catch_basin_5yr")
#' @return Prefixed key (e.g. "mmcd:catch_basin_5yr")
redis_key <- function(key) {
  paste0(REDIS_CONFIG$prefix, key)
}

# =============================================================================
# CORE GET / SET / DELETE
# =============================================================================

#' Store an R object in Redis
#'
#' @param key   Cache key (will be prefixed automatically)
#' @param value Any R object (data.frame, list, vector, …)
#' @param ttl   Time-to-live in seconds. NULL = no expiry. Default 86400 (24h).
#' @return TRUE on success, FALSE on failure
redis_set <- function(key, value, ttl = 86400L) {
  conn <- get_redis()
  if (is.null(conn)) return(FALSE)

  fkey <- redis_key(key)
  raw  <- serialize(value, connection = NULL)   # returns a raw vector

  tryCatch({
    conn$SET(fkey, raw)
    if (!is.null(ttl) && is.numeric(ttl) && ttl > 0) {
      conn$EXPIRE(fkey, as.integer(ttl))
    }
    TRUE
  }, error = function(e) {
    warning(sprintf("[redis_cache] SET failed for '%s': %s", key, e$message))
    FALSE
  })
}

#' Retrieve an R object from Redis
#'
#' @param key Cache key
#' @return The deserialized R object, or NULL if not found / error
redis_get <- function(key) {
  conn <- get_redis()
  if (is.null(conn)) return(NULL)

  fkey <- redis_key(key)

  tryCatch({
    raw <- conn$GET(fkey)
    if (is.null(raw)) return(NULL)
    unserialize(raw)
  }, error = function(e) {
    warning(sprintf("[redis_cache] GET failed for '%s': %s", key, e$message))
    NULL
  })
}

#' Check if a key exists in Redis
#'
#' @param key Cache key
#' @return TRUE / FALSE
redis_exists <- function(key) {
  conn <- get_redis()
  if (is.null(conn)) return(FALSE)

  tryCatch({
    as.logical(conn$EXISTS(redis_key(key)))
  }, error = function(e) FALSE)
}

#' Delete one or more keys
#'
#' @param keys Character vector of cache keys
#' @return Number of keys deleted
redis_del <- function(keys) {
  conn <- get_redis()
  if (is.null(conn)) return(0L)

  fkeys <- vapply(keys, redis_key, character(1), USE.NAMES = FALSE)
  tryCatch({
    as.integer(conn$DEL(fkeys))
  }, error = function(e) 0L)
}

#' List keys matching a pattern
#'
#' @param pattern Glob pattern (default "*" = all keys in namespace)
#' @return Character vector of keys (without prefix)
redis_keys <- function(pattern = "*") {
  conn <- get_redis()
  if (is.null(conn)) return(character(0))

  fpattern <- redis_key(pattern)
  tryCatch({
    raw_keys <- conn$KEYS(fpattern)
    if (length(raw_keys) == 0) return(character(0))
    # Strip prefix
    prefix_len <- nchar(REDIS_CONFIG$prefix)
    vapply(raw_keys, function(k) substring(k, prefix_len + 1), character(1),
           USE.NAMES = FALSE)
  }, error = function(e) character(0))
}

# =============================================================================
# HASH-BASED OPERATIONS (for the historical averages "bundle")
# =============================================================================
# The old cache was one big list:
#   cache$averages$catch_basin_5yr  <- data.frame(...)
#   cache$averages$catch_basin_10yr <- data.frame(...)
#
# In Redis we store each sub-key as a field in a Hash:
#   HSET mmcd:historical_averages  catch_basin_5yr  <serialized df>
#   HSET mmcd:historical_averages  catch_basin_10yr <serialized df>
#
# This lets us read/write individual metrics without loading the entire cache.

HISTORICAL_HASH_KEY  <- "historical_averages"
HISTORICAL_META_KEY  <- "historical_averages_meta"
LOOKUP_HASH_KEY      <- "lookup_cache"

#' Store a metric's data in the historical cache hash
#'
#' @param field  e.g. "catch_basin_5yr"
#' @param value  data.frame
#' @param ttl    TTL for the entire hash (refreshed on every write). Default 24h.
#' @return TRUE/FALSE
redis_hset <- function(hash_key, field, value, ttl = 86400L) {
  conn <- get_redis()
  if (is.null(conn)) return(FALSE)

  fkey <- redis_key(hash_key)
  raw  <- serialize(value, connection = NULL)

  tryCatch({
    conn$HSET(fkey, field, raw)
    if (!is.null(ttl) && is.numeric(ttl) && ttl > 0) {
      conn$EXPIRE(fkey, as.integer(ttl))
    }
    TRUE
  }, error = function(e) {
    warning(sprintf("[redis_cache] HSET failed for '%s.%s': %s", hash_key, field, e$message))
    FALSE
  })
}

#' Get a single field from a hash
#'
#' @param hash_key The hash name
#' @param field    The field name
#' @return Deserialized R object, or NULL
redis_hget <- function(hash_key, field) {
  conn <- get_redis()
  if (is.null(conn)) return(NULL)

  tryCatch({
    raw <- conn$HGET(redis_key(hash_key), field)
    if (is.null(raw)) return(NULL)
    unserialize(raw)
  }, error = function(e) {
    warning(sprintf("[redis_cache] HGET failed for '%s.%s': %s", hash_key, field, e$message))
    NULL
  })
}

#' Get all fields from a hash
#'
#' @param hash_key The hash name
#' @return Named list of deserialized R objects
redis_hgetall <- function(hash_key) {
  conn <- get_redis()
  if (is.null(conn)) return(list())

  tryCatch({
    raw_list <- conn$HGETALL(redis_key(hash_key))
    if (length(raw_list) == 0) return(list())

    # HGETALL returns flat alternating key, value, key, value ...
    n <- length(raw_list)
    if (n %% 2 != 0) return(list())

    result <- list()
    idx <- seq(1, n, by = 2)
    for (i in idx) {
      field_name <- raw_list[[i]]
      if (is.raw(field_name)) field_name <- rawToChar(field_name)
      result[[field_name]] <- tryCatch(
        unserialize(raw_list[[i + 1]]),
        error = function(e) NULL
      )
    }
    result
  }, error = function(e) {
    warning(sprintf("[redis_cache] HGETALL failed for '%s': %s", hash_key, e$message))
    list()
  })
}

#' List all fields in a hash
#'
#' @param hash_key The hash name
#' @return Character vector of field names
redis_hkeys <- function(hash_key) {
  conn <- get_redis()
  if (is.null(conn)) return(character(0))

  tryCatch({
    fields <- conn$HKEYS(redis_key(hash_key))
    if (length(fields) == 0) return(character(0))
    vapply(fields, function(f) {
      if (is.raw(f)) rawToChar(f) else as.character(f)
    }, character(1), USE.NAMES = FALSE)
  }, error = function(e) character(0))
}

#' Delete a field from a hash
#'
#' @param hash_key The hash name
#' @param fields   Character vector of field names to delete
#' @return Number deleted
redis_hdel <- function(hash_key, fields) {
  conn <- get_redis()
  if (is.null(conn)) return(0L)

  tryCatch({
    as.integer(conn$HDEL(redis_key(hash_key), fields))
  }, error = function(e) 0L)
}

# =============================================================================
# HISTORICAL AVERAGES — HIGH-LEVEL API
# (Drop-in replacements for readRDS/saveRDS on the cache file)
# =============================================================================

#' Save the full historical averages cache to Redis
#' Mirrors the structure: list(generated_date, averages = list(...))
#'
#' @param cache List with $generated_date, $zone_filter, $averages
#' @param ttl   TTL in seconds (default 24h)
#' @return TRUE/FALSE
save_historical_cache_redis <- function(cache, ttl = 86400L) {
  if (!redis_is_active()) return(FALSE)

  # Store each metric as a hash field
  for (field_name in names(cache$averages)) {
    redis_hset(HISTORICAL_HASH_KEY, field_name, cache$averages[[field_name]], ttl = ttl)
  }

  # Store metadata separately
  meta <- list(
    generated_date = cache$generated_date,
    generated_by   = cache$generated_by,
    zone_filter    = cache$zone_filter,
    field_count    = length(cache$averages)
  )
  redis_set(HISTORICAL_META_KEY, meta, ttl = ttl)

  message(sprintf("[redis_cache] Historical cache saved: %d fields", length(cache$averages)))
  TRUE
}

#' Load the full historical averages cache from Redis
#' Returns the same list structure as readRDS(cache_file)
#'
#' @return List with $generated_date, $zone_filter, $averages, or NULL
load_historical_cache_redis <- function() {
  if (!redis_is_active()) return(NULL)

  meta <- redis_get(HISTORICAL_META_KEY)
  if (is.null(meta)) return(NULL)

  averages <- redis_hgetall(HISTORICAL_HASH_KEY)

  list(
    generated_date = meta$generated_date,
    generated_by   = meta$generated_by,
    zone_filter    = meta$zone_filter,
    averages       = averages
  )
}

#' Get a single cached average from Redis (fast path)
#'
#' @param metric_id e.g. "catch_basin"
#' @param avg_type  "5yr", "10yr", "yearly_facilities", "yearly_district"
#' @return data.frame or NULL
get_cached_average_redis <- function(metric_id, avg_type = "10yr") {
  field <- paste0(metric_id, "_", avg_type)
  redis_hget(HISTORICAL_HASH_KEY, field)
}

#' Clear historical cache in Redis
clear_historical_cache_redis <- function() {
  redis_del(c(HISTORICAL_HASH_KEY, HISTORICAL_META_KEY))
  message("[redis_cache] Historical cache cleared from Redis")
}

# =============================================================================
# LOOKUP CACHE — HIGH-LEVEL API
# (Drop-in replacement for file-based lookup_cache.rds)
# =============================================================================

#' Get a lookup value from Redis
#'
#' @param key Lookup name (e.g. "facilities")
#' @return The cached value, or NULL
get_lookup_redis <- function(key) {
  redis_hget(LOOKUP_HASH_KEY, key)
}

#' Set a lookup value in Redis
#'
#' @param key   Lookup name
#' @param value Any R object
#' @param ttl   TTL for the entire lookup hash (default 1h)
set_lookup_redis <- function(key, value, ttl = 3600L) {
  redis_hset(LOOKUP_HASH_KEY, key, value, ttl = ttl)
  # Also store timestamp (but don't double-stamp timestamp keys)
  if (!grepl("_timestamp$", key)) {
    redis_hset(LOOKUP_HASH_KEY, paste0(key, "_timestamp"), Sys.time(), ttl = ttl)
  }
}

#' Load entire lookup cache from Redis
#'
#' @return Named list of all lookup data
load_lookup_cache_redis <- function() {
  redis_hgetall(LOOKUP_HASH_KEY)
}

#' Clear lookup cache in Redis
clear_lookup_cache_redis <- function() {
  redis_del(LOOKUP_HASH_KEY)
  message("[redis_cache] Lookup cache cleared from Redis")
}

# =============================================================================
# APP-LEVEL CACHE — HIGH-LEVEL API
# (Replacement for the in-memory .mmcd_app_cache environment)
# =============================================================================

#' Get app-cached data from Redis with TTL check
#'
#' @param key        Cache key
#' @param load_func  Function to load data on miss (optional)
#' @param ttl        TTL in seconds (default 300 = 5 min)
#' @return The cached data, or freshly loaded data, or NULL
get_app_cached_redis <- function(key, load_func = NULL, ttl = 300L) {
  app_key <- paste0("app:", key)

  # Try Redis first
  data <- redis_get(app_key)
  if (!is.null(data)) return(data)

  # Cache miss – load if we can
  if (is.null(load_func)) return(NULL)

  data <- tryCatch(load_func(), error = function(e) {
    warning(sprintf("[redis_cache] Load failed for app key '%s': %s", key, e$message))
    NULL
  })

  if (!is.null(data)) {
    redis_set(app_key, data, ttl = ttl)
  }

  data
}

#' Set app-level cached data in Redis
#'
#' @param key   Cache key
#' @param value R object
#' @param ttl   TTL in seconds (default 300 = 5 min)
set_app_cached_redis <- function(key, value, ttl = 300L) {
  redis_set(paste0("app:", key), value, ttl = ttl)
}

#' Clear all app-level cache entries
clear_app_cache_redis <- function() {
  keys <- redis_keys("app:*")
  if (length(keys) > 0) redis_del(keys)
  message("[redis_cache] App cache cleared from Redis")
}

# =============================================================================
# DIAGNOSTIC / STATUS
# =============================================================================

#' Get comprehensive Redis cache status
#'
#' @return List with connection info, key counts, memory usage
redis_cache_status <- function() {
  status <- list(
    backend   = REDIS_CONFIG$backend,
    connected = FALSE,
    host      = REDIS_CONFIG$host,
    port      = REDIS_CONFIG$port,
    prefix    = REDIS_CONFIG$prefix
  )

  conn <- get_redis()
  if (is.null(conn)) return(status)

  status$connected <- TRUE

  tryCatch({
    info <- conn$INFO("memory")
    # Parse used_memory_human from INFO output
    if (is.character(info) || is.raw(info)) {
      info_text <- if (is.raw(info)) rawToChar(info) else info
      mem_match <- regmatches(info_text, regexpr("used_memory_human:[^\r\n]+", info_text))
      if (length(mem_match) > 0) {
        status$memory_used <- gsub("used_memory_human:", "", mem_match)
      }
    }

    # Count our keys
    all_keys <- redis_keys("*")
    status$total_keys <- length(all_keys)

    # Breakdown
    status$historical_fields <- length(redis_hkeys(HISTORICAL_HASH_KEY))
    status$lookup_fields     <- length(redis_hkeys(LOOKUP_HASH_KEY))
    status$app_keys          <- length(redis_keys("app:*"))

  }, error = function(e) {
    status$error <- e$message
  })

  status
}

#' Print a formatted Redis cache status report
print_redis_status <- function() {
  s <- redis_cache_status()

  cat("\n╔═══════════════════════════════════════╗\n")
  cat("║       MMCD Redis Cache Status         ║\n")
  cat("╠═══════════════════════════════════════╣\n")
  cat(sprintf("║ Backend:    %-25s ║\n", s$backend))
  cat(sprintf("║ Connected:  %-25s ║\n", if (s$connected) "YES" else "NO"))
  cat(sprintf("║ Host:       %-25s ║\n", paste0(s$host, ":", s$port)))

  if (s$connected) {
    cat(sprintf("║ Memory:     %-25s ║\n", s$memory_used %||% "unknown"))
    cat(sprintf("║ Total keys: %-25d ║\n", s$total_keys %||% 0))
    cat("╠═══════════════════════════════════════╣\n")
    cat(sprintf("║ Historical:  %-24d ║\n", s$historical_fields %||% 0))
    cat(sprintf("║ Lookups:     %-24d ║\n", s$lookup_fields %||% 0))
    cat(sprintf("║ App cache:   %-24d ║\n", s$app_keys %||% 0))
  }

  cat("╚═══════════════════════════════════════╝\n")
}

# =============================================================================
# MODULE INIT
# =============================================================================

# Null-coalesce operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

message(sprintf("[redis_cache] Module loaded (backend=%s, host=%s:%d)",
                REDIS_CONFIG$backend, REDIS_CONFIG$host, REDIS_CONFIG$port))
