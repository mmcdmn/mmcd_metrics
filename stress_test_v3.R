# =============================================================================
# MMCD Metrics Stress Test v3 — Load Balancer Concurrency Test
# =============================================================================
# PURPOSE:  Verify the dynamic load balancer distributes users correctly and
#           measure how the dashboard performs under realistic concurrent load.
#
# KEY DIFFERENCES from v2:
#   - Spoofs unique client IPs via X-Forwarded-For so each simulated user
#     gets its own route through the Lua load balancer
#   - Verifies load distribution across workers via Redis (checks that users
#     are actually spread across workers, not all piled on one)
#   - Tests 3 scenarios: single-app burst, multi-app spread, ramp-up
#   - Reads Redis routing state before/after each test to prove distribution
#   - Works against local Docker (localhost:3838) or production
#
# USAGE:
#   source("stress_test_v3.R")
#
#   # Quick test — 10 users, 30s
#   quick_test()
#
#   # Full progressive stress test (ramps from 5 → 50 users)
#   run_stress_test()
#
#   # Single-app burst: 15 users all hitting the same app
#   burst_test("catch_basin_status", num_users = 15)
#
#   # Check load balancer distribution (no load, just routing)
#   verify_distribution(num_users = 20)
# =============================================================================

library(httr2)
library(ggplot2)
library(dplyr)
library(tidyr)

`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# CONFIGURATION
# =============================================================================

CONFIG <- list(
  # ---- Target ----
  # Change to "https://metrics.mmcd.org" for production testing
  base_url = "https://metrics.mmcd.org",

  # ---- Progressive ramp-up ----
  max_users       = 50,
  ramp_step       = 5,
  duration_sec    = 30,     # per level
  cooldown_sec    = 10,      # between levels

  # ---- Thresholds ----
  success_rate_min  = 90,   # % — below this is FAIL
  avg_response_max  = 5000, # ms (queue adds latency, thats ok)
  p95_response_max  = 15000, # ms (queued requests may wait up to 40s)

  # ---- Redis (for verifying distribution) ----
  # Only works when testing against localhost Docker
  redis_via_docker = FALSE,
  container_name   = "mmcd-app",

  # ---- Output ----
  output_dir   = "stress_test_results",
  save_raw     = TRUE
)

# Apps to test — path is relative to base_url/apps/
# weight controls how often each app is selected (higher = more)
APPS <- list(
  list(path = "/apps/catch_basin_status/",       name = "Catch Basin",         weight = 3, category = "heavy"),
  list(path = "/apps/ground_prehatch_progress/",  name = "Ground Prehatch",     weight = 3, category = "heavy"),
  list(path = "/apps/inspections/",               name = "Inspections",         weight = 2, category = "heavy"),
  list(path = "/apps/cattail_treatments/",        name = "Cattail Treatments",  weight = 2, category = "heavy"),
  list(path = "/apps/drone/",                     name = "Drone Sites",         weight = 2, category = "medium"),
  list(path = "/apps/air_sites_simple/",          name = "Air Sites",           weight = 2, category = "medium"),
  list(path = "/apps/control_efficacy/",          name = "Control Efficacy",    weight = 1, category = "light"),
  list(path = "/apps/cattail_inspections/",       name = "Cattail Inspections", weight = 1, category = "light"),
  list(path = "/apps/struct_trt/",                name = "Structure Treatments",weight = 1, category = "light"),
  list(path = "/apps/test-app/",                  name = "Test App",            weight = 1, category = "light")
)

# =============================================================================
# IP SPOOFING
# =============================================================================
# The Lua load balancer reads X-Forwarded-For to identify clients.
# By setting unique XFF values, each simulated "user" gets a separate
# route assignment — exactly what happens in production behind the ALB.

#' Generate a fake IP for user N
fake_ip <- function(user_id) {
  # Use 10.x.x.x range (private, won't collide with real traffic)
  b2 <- (user_id %/% 65536) %% 256
  b3 <- (user_id %/% 256) %% 256
  b4 <- user_id %% 256
  sprintf("10.%d.%d.%d", b2, b3, max(b4, 1))
}

# =============================================================================
# REDIS HELPERS (read routing state from the container)
# =============================================================================

#' Run a redis-cli command inside the Docker container
redis_cmd <- function(cmd) {
  if (!CONFIG$redis_via_docker) return(NULL)
  full <- sprintf("docker exec %s redis-cli %s", CONFIG$container_name, cmd)
  tryCatch(
    trimws(system(full, intern = TRUE, ignore.stderr = TRUE)),
    error = function(e) NULL
  )
}

#' Read current worker loads from Redis
get_worker_loads <- function() {
  workers <- redis_cmd("SMEMBERS mmcd:workers")
  if (is.null(workers) || length(workers) == 0) return(NULL)
  loads <- sapply(workers, function(w) {
    val <- redis_cmd(sprintf("GET mmcd:load:%s", w))
    as.integer(val %||% "0")
  })
  data.frame(worker = workers, load = loads, row.names = NULL, stringsAsFactors = FALSE)
}

#' Count how many routes exist and how they are distributed
get_route_distribution <- function() {
  keys <- redis_cmd('KEYS "mmcd:route:*"')
  if (is.null(keys) || length(keys) == 0 || (length(keys) == 1 && keys == "")) {
    return(data.frame(worker = character(), routes = integer(), stringsAsFactors = FALSE))
  }
  workers <- sapply(keys, function(k) {
    redis_cmd(sprintf("GET %s", k))
  })
  as.data.frame(table(worker = workers), stringsAsFactors = FALSE) |>
    setNames(c("worker", "routes"))
}

#' Flush all route mappings (start fresh)
flush_routes <- function() {
  keys <- redis_cmd('KEYS "mmcd:route:*"')
  if (!is.null(keys) && length(keys) > 0 && keys[1] != "") {
    for (k in keys) redis_cmd(sprintf("DEL %s", k))
  }
  # Reset load counters
  workers <- redis_cmd("SMEMBERS mmcd:workers")
  if (!is.null(workers)) {
    for (w in workers) redis_cmd(sprintf("SET mmcd:load:%s 0", w))
  }
  # Clear the decision log
  redis_cmd("DEL mmcd:route_log")
  invisible(NULL)
}

# =============================================================================
# REQUEST ENGINE
# =============================================================================

#' Build a single request with a spoofed IP
#' Timeout is 60s to allow queue retries (up to 20 x 2s = 40s max wait)
build_request <- function(app, user_id) {
  request(paste0(CONFIG$base_url, app$path)) |>
    req_headers(`X-Forwarded-For` = fake_ip(user_id)) |>
    req_timeout(60) |>
    req_error(is_error = function(resp) FALSE)
}

#' Fire N concurrent requests and collect results
#'
#' @param n         Number of simultaneous requests
#' @param apps      List of app specs (or a single app repeated)
#' @param user_ids  Vector of user IDs (for IP spoofing)
#' @return data.frame of results
fire_batch <- function(n, apps, user_ids) {
  weights <- sapply(apps, function(a) a$weight)

  # Pick an app and build a request for each user
  picks <- sample(seq_along(apps), size = n, replace = TRUE, prob = weights)
  reqs <- lapply(seq_len(n), function(i) build_request(apps[[picks[i]]], user_ids[i]))
  app_info <- apps[picks]

  t0 <- Sys.time()
  resps <- req_perform_parallel(reqs, on_error = "continue")
  batch_wall <- as.numeric(Sys.time() - t0, units = "secs") * 1000

  lapply(seq_len(n), function(i) {
    resp <- resps[[i]]
    ok  <- inherits(resp, "httr2_response")
    code <- if (ok) resp_status(resp) else 0L
    # Detect if request was queued (response time > 2s suggests at least 1 retry)
    was_queued <- (batch_wall / n) > 2000
    data.frame(
      timestamp    = format(t0, "%Y-%m-%d %H:%M:%OS3"),
      user_id      = user_ids[i],
      spoofed_ip   = fake_ip(user_ids[i]),
      app          = app_info[[i]]$name,
      category     = app_info[[i]]$category,
      status       = code,
      success      = ok && code == 200,
      queued       = ok && code == 200 && was_queued,
      wall_ms      = batch_wall,
      per_req_ms   = batch_wall / n,
      error        = if (!ok && inherits(resp, "error")) substr(resp$message, 1, 150) else NA_character_,
      stringsAsFactors = FALSE
    )
  }) |> do.call(what = rbind)
}

# =============================================================================
# TEST SCENARIOS
# =============================================================================

#' Run a load level: N users sending requests for `duration` seconds
run_level <- function(n_users, duration_sec, apps = APPS, verbose = TRUE) {
  if (verbose) {
    cat(sprintf("\n  [%d users] running for %ds ...\n", n_users, duration_sec))
  }

  user_ids <- seq_len(n_users)
  end_time <- Sys.time() + duration_sec
  all <- list()
  batch <- 0L

  while (Sys.time() < end_time) {
    batch <- batch + 1L
    df <- fire_batch(n_users, apps, user_ids)
    df$batch <- batch
    df$n_users <- n_users
    all[[batch]] <- df
    Sys.sleep(0.1)  # brief pause between volleys
  }

  results <- do.call(rbind, all)
  stats   <- summarise_level(results)

  if (verbose) print_level(stats)

  list(results = results, stats = stats,
       should_stop = stats$success_pct < 25 || stats$p95_ms > 45000)
}

#' Summarise one load level
summarise_level <- function(df) {
  list(
    n_users    = df$n_users[1],
    batches    = max(df$batch),
    total_reqs = nrow(df),
    ok         = sum(df$success),
    fail       = sum(!df$success),
    queued     = sum(df$queued, na.rm = TRUE),
    success_pct = mean(df$success) * 100,
    rps         = nrow(df) / max(1, as.numeric(
                    difftime(max(as.POSIXct(df$timestamp)),
                             min(as.POSIXct(df$timestamp)), units = "secs"))),
    avg_ms     = mean(df$per_req_ms, na.rm = TRUE),
    med_ms     = median(df$per_req_ms, na.rm = TRUE),
    p95_ms     = quantile(df$per_req_ms, 0.95, na.rm = TRUE),
    p99_ms     = quantile(df$per_req_ms, 0.99, na.rm = TRUE),
    max_ms     = max(df$per_req_ms, na.rm = TRUE)
  )
}

#' Print level summary
print_level <- function(s) {
  tag <- if (s$success_pct >= CONFIG$success_rate_min) "PASS" else "FAIL"
  cat(sprintf("  [%s] %d users | %d reqs (%d batches) | %.0f%% ok | %.0f rps\n",
              tag, s$n_users, s$total_reqs, s$batches, s$success_pct, s$rps))
  cat(sprintf("         avg=%.0fms  med=%.0fms  p95=%.0fms  p99=%.0fms  max=%.0fms\n",
              s$avg_ms, s$med_ms, s$p95_ms, s$p99_ms, s$max_ms))
  if (s$queued > 0) {
    cat(sprintf("         queued: %d reqs (%.0f%%) waited in queue before being served\n",
                s$queued, s$queued / s$total_reqs * 100))
  }
  if (s$fail > 0) cat(sprintf("         !! %d failures (likely exhausted queue after 40s wait)\n", s$fail))
}

# =============================================================================
# DISTRIBUTION VERIFICATION
# =============================================================================

#' Send 1 request per user and check how the load balancer distributes them
verify_distribution <- function(num_users = 20, verbose = TRUE) {
  cat("\n==============================================================\n")
  cat("  LOAD BALANCER DISTRIBUTION TEST\n")
  cat("==============================================================\n")
  cat(sprintf("  Simulating %d unique users (spoofed X-Forwarded-For)\n", num_users))
  cat(sprintf("  Target: %s\n\n", CONFIG$base_url))

  # Clear existing routes
  if (CONFIG$redis_via_docker) {
    cat("  Flushing existing routes...\n")
    flush_routes()
    Sys.sleep(1)
  }

  # Send one request per user (sequentially so we can track each)
  results <- lapply(seq_len(num_users), function(uid) {
    ip <- fake_ip(uid)
    resp <- tryCatch({
      request(paste0(CONFIG$base_url, "/apps/test-app/")) |>
        req_headers(`X-Forwarded-For` = ip) |>
        req_timeout(30) |>
        req_perform()
    }, error = function(e) NULL)
    ok <- !is.null(resp) && resp_status(resp) == 200
    if (verbose) cat(sprintf("  User %2d (IP %s) -> %s\n", uid, ip, if (ok) "OK" else "FAIL"))
    data.frame(user_id = uid, ip = ip, success = ok, stringsAsFactors = FALSE)
  }) |> do.call(what = rbind)

  cat(sprintf("\n  Sent: %d  |  OK: %d  |  Failed: %d\n",
              nrow(results), sum(results$success), sum(!results$success)))

  # Read routing state
  if (CONFIG$redis_via_docker) {
    Sys.sleep(1)  # let L1 cache settle
    cat("\n  --- Redis Routing State ---\n")

    loads <- get_worker_loads()
    if (!is.null(loads)) {
      cat("\n  Worker Loads (active WebSocket connections):\n")
      for (i in seq_len(nrow(loads))) {
        bar <- paste(rep("#", loads$load[i]), collapse = "")
        cat(sprintf("    %s : %d  %s\n", loads$worker[i], loads$load[i], bar))
      }
    }

    dist <- get_route_distribution()
    if (nrow(dist) > 0) {
      cat("\n  Route Distribution (users per worker):\n")
      total <- sum(dist$routes)
      for (i in seq_len(nrow(dist))) {
        pct <- dist$routes[i] / total * 100
        bar <- paste(rep("█", round(pct / 2)), collapse = "")
        cat(sprintf("    %s : %3d routes (%4.1f%%)  %s\n",
                    dist$worker[i], dist$routes[i], pct, bar))
      }

      # Evenness score (0 = all on one worker, 100 = perfectly even)
      n_workers <- nrow(dist)
      ideal <- total / n_workers
      deviation <- sum(abs(dist$routes - ideal)) / total * 100
      evenness <- max(0, round(100 - deviation * n_workers / 2))
      cat(sprintf("\n  Distribution Score: %d/100", evenness))
      cat(sprintf("  (100 = perfectly even across %d workers)\n", n_workers))

      if (evenness >= 80) {
        cat("  PASS — load balancer is distributing well\n")
      } else if (evenness >= 50) {
        cat("  WARN — uneven distribution, possible issue\n")
      } else {
        cat("  FAIL — severe imbalance, most users on 1 worker\n")
      }
    } else {
      cat("\n  No route keys found (routes may have expired or Redis issue)\n")
    }

    # Show recent routing decisions
    log_raw <- redis_cmd("LRANGE mmcd:route_log 0 4")
    if (!is.null(log_raw) && length(log_raw) > 0 && log_raw[1] != "") {
      cat("\n  Last 5 routing decisions:\n")
      for (entry in log_raw) {
        parsed <- tryCatch(jsonlite::fromJSON(entry), error = function(e) NULL)
        if (!is.null(parsed)) {
          cat(sprintf("    %s -> %s  (reason: %s)\n",
                      parsed$client_ip, parsed$target, parsed$reason))
        }
      }
    }
  }

  cat("\n==============================================================\n")
  invisible(results)
}

# =============================================================================
# BURST TEST — everyone hits the SAME app
# =============================================================================

#' Slam a single app with N concurrent users
burst_test <- function(app_name = "catch_basin_status", num_users = 15, duration_sec = 30) {
  app_match <- Filter(function(a) grepl(app_name, a$name, ignore.case = TRUE) ||
                                   grepl(app_name, a$path, ignore.case = TRUE), APPS)
  if (length(app_match) == 0) {
    cat("App not found. Available:\n")
    cat(paste(" ", sapply(APPS, function(a) a$name), collapse = "\n"), "\n")
    return(invisible(NULL))
  }
  app <- app_match[[1]]

  cat("\n==============================================================\n")
  cat("  SINGLE-APP BURST TEST\n")
  cat("==============================================================\n")
  cat(sprintf("  App:      %s (%s)\n", app$name, app$path))
  cat(sprintf("  Users:    %d concurrent\n", num_users))
  cat(sprintf("  Duration: %ds\n", duration_sec))
  cat(sprintf("  Target:   %s\n", CONFIG$base_url))

  if (CONFIG$redis_via_docker) {
    cat("\n  Flushing routes...\n")
    flush_routes()
    Sys.sleep(1)
  }

  # Run with a single-app list
  single_app <- list(app)
  single_app[[1]]$weight <- 1

  result <- run_level(num_users, duration_sec, apps = single_app, verbose = TRUE)

  # Show distribution after burst
  if (CONFIG$redis_via_docker) {
    cat("\n  --- Post-Burst Distribution ---\n")
    dist <- get_route_distribution()
    if (nrow(dist) > 0) {
      for (i in seq_len(nrow(dist))) {
        cat(sprintf("    %s : %d routes\n", dist$worker[i], dist$routes[i]))
      }
    }
    loads <- get_worker_loads()
    if (!is.null(loads)) {
      cat("  Worker loads: ", paste(sprintf("%s=%d", loads$worker, loads$load), collapse = ", "), "\n")
    }
  }

  cat("==============================================================\n")
  invisible(result)
}

# =============================================================================
# QUICK TEST
# =============================================================================

quick_test <- function(num_users = 10, duration_sec = 30) {
  cat("\n==============================================================\n")
  cat("  QUICK STRESS TEST\n")
  cat("==============================================================\n")
  cat(sprintf("  Target: %s\n", CONFIG$base_url))
  cat(sprintf("  Users:  %d  |  Duration: %ds\n", num_users, duration_sec))

  # Connectivity check
  cat("  Checking connectivity... ")
  resp <- tryCatch(
    request(CONFIG$base_url) |> req_timeout(10) |> req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) {
    cat("FAILED — cannot reach server\n")
    return(invisible(NULL))
  }
  cat("OK\n")

  if (CONFIG$redis_via_docker) {
    cat("  Flushing routes...\n")
    flush_routes()
    Sys.sleep(1)
  }

  result <- run_level(num_users, duration_sec, verbose = TRUE)

  # Distribution check
  if (CONFIG$redis_via_docker) {
    cat("\n  --- Load Balancer Distribution ---\n")
    dist <- get_route_distribution()
    if (nrow(dist) > 0) {
      total <- sum(dist$routes)
      for (i in seq_len(nrow(dist))) {
        pct <- dist$routes[i] / total * 100
        bar <- paste(rep("█", round(pct / 2)), collapse = "")
        cat(sprintf("    %s : %3d routes (%4.1f%%)  %s\n",
                    dist$worker[i], dist$routes[i], pct, bar))
      }
    }
  }

  score <- score_results(result$results)
  cat(sprintf("\n  Overall Score: %d/100\n", score))
  cat("==============================================================\n")
  invisible(result)
}

# =============================================================================
# FULL PROGRESSIVE STRESS TEST
# =============================================================================

run_stress_test <- function(save_baseline = FALSE) {
  cat("\n+--------------------------------------------------------------+\n")
  cat("|  MMCD Stress Test v3 — Load Balancer Concurrency Test        |\n")
  cat("+--------------------------------------------------------------+\n\n")

  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")

  cat("  Configuration:\n")
  cat(sprintf("    Target:       %s\n", CONFIG$base_url))
  cat(sprintf("    Ramp:         %d → %d users (step %d)\n",
              CONFIG$ramp_step, CONFIG$max_users, CONFIG$ramp_step))
  cat(sprintf("    Duration:     %ds per level, %ds cooldown\n",
              CONFIG$duration_sec, CONFIG$cooldown_sec))
  cat(sprintf("    Apps tested:  %d\n", length(APPS)))
  cat(sprintf("    IP spoofing:  X-Forwarded-For (10.x.x.x range)\n"))

  # Connectivity
  cat("\n  Checking connectivity... ")
  resp <- tryCatch(
    request(CONFIG$base_url) |> req_timeout(10) |> req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) {
    cat("FAILED\n")
    return(invisible(NULL))
  }
  cat("OK\n")

  if (CONFIG$redis_via_docker) {
    workers <- redis_cmd("SMEMBERS mmcd:workers")
    cat(sprintf("  Workers detected: %d (%s)\n", length(workers), paste(workers, collapse = ", ")))
    cat("  Flushing routes for clean test...\n")
    flush_routes()
    Sys.sleep(1)
  }

  # ---- Phase 1: Distribution Verification ----
  cat("\n── Phase 1: Distribution Verification ──────────────────────\n")
  verify_distribution(num_users = 15, verbose = FALSE)

  Sys.sleep(CONFIG$cooldown_sec)
  flush_routes()
  Sys.sleep(1)

  # ---- Phase 2: Progressive Ramp-Up ----
  cat("\n── Phase 2: Progressive Ramp-Up ─────────────────────────────\n")

  all_results <- list()
  level_stats <- list()
  n <- CONFIG$ramp_step
  test_start <- Sys.time()

  while (n <= CONFIG$max_users) {
    result <- run_level(n, CONFIG$duration_sec, verbose = TRUE)
    all_results[[length(all_results) + 1]] <- result$results
    level_stats[[length(level_stats) + 1]] <- result$stats

    if (result$should_stop) {
      cat("\n  !! Stopping — severe degradation detected\n")
      break
    }

    if (n < CONFIG$max_users) {
      cat(sprintf("  Cooldown %ds...\n", CONFIG$cooldown_sec))
      Sys.sleep(CONFIG$cooldown_sec)
    }
    n <- n + CONFIG$ramp_step
  }

  total_time <- as.numeric(Sys.time() - test_start, units = "secs")

  # ---- Phase 3: Single-App Burst ----
  cat("\n── Phase 3: Single-App Burst (catch_basin_status x 15) ──────\n")
  flush_routes()
  Sys.sleep(1)
  burst <- run_level(15, min(CONFIG$duration_sec, 20),
                     apps = list(APPS[[1]]), verbose = TRUE)
  all_results[[length(all_results) + 1]] <- burst$results

  # ---- Combine results ----
  final <- do.call(rbind, all_results)

  # ---- Final Distribution ----
  if (CONFIG$redis_via_docker) {
    cat("\n── Final Load Balancer State ────────────────────────────────\n")
    dist <- get_route_distribution()
    if (nrow(dist) > 0) {
      total <- sum(dist$routes)
      for (i in seq_len(nrow(dist))) {
        pct <- dist$routes[i] / total * 100
        bar <- paste(rep("█", round(pct / 2)), collapse = "")
        cat(sprintf("    %s : %3d routes (%4.1f%%)  %s\n",
                    dist$worker[i], dist$routes[i], pct, bar))
      }
    }
    loads <- get_worker_loads()
    if (!is.null(loads)) {
      cat(sprintf("    Active WS: %s\n",
                  paste(sprintf("%s=%d", loads$worker, loads$load), collapse = "  ")))
    }
  }

  # ---- Summary ----
  score <- score_results(final)

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("                       FINAL SUMMARY\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  total_queued <- sum(final$queued, na.rm = TRUE)
  cat(sprintf("  Overall Score:    %d / 100\n", score))
  cat(sprintf("  Total Requests:   %s\n", format(nrow(final), big.mark = ",")))
  cat(sprintf("  Success Rate:     %.1f%%\n", mean(final$success) * 100))
  cat(sprintf("  Queued Requests:  %s (%.1f%% served after waiting in queue)\n",
              format(total_queued, big.mark = ","),
              total_queued / nrow(final) * 100))
  cat(sprintf("  Avg Response:     %.0f ms\n", mean(final$per_req_ms, na.rm = TRUE)))
  cat(sprintf("  95th Percentile:  %.0f ms\n", quantile(final$per_req_ms, 0.95, na.rm = TRUE)))
  cat(sprintf("  Max Concurrent:   %d users tested\n", max(final$n_users)))
  cat(sprintf("  Test Duration:    %.0f seconds\n", total_time))
  cat("═══════════════════════════════════════════════════════════════\n")

  # ---- Save outputs ----
  ensure_dir()

  csv_file <- file.path(CONFIG$output_dir, sprintf("results_%s.csv", ts))
  write.csv(final, csv_file, row.names = FALSE)
  cat(sprintf("  CSV: %s\n", csv_file))

  if (CONFIG$save_raw) {
    rds_file <- file.path(CONFIG$output_dir, sprintf("raw_%s.rds", ts))
    saveRDS(final, rds_file)
  }

  generate_report(final, level_stats, ts)
  generate_plots(final, ts)

  if (save_baseline) {
    base_file <- file.path(CONFIG$output_dir, "baseline.rds")
    saveRDS(final, base_file)
    cat(sprintf("  Baseline saved: %s\n", base_file))
  }

  cat("\n  Done!\n")
  invisible(final)
}

# =============================================================================
# SCORING
# =============================================================================

score_results <- function(df) {
  success_pct <- mean(df$success) * 100
  avg_ms      <- mean(df$per_req_ms, na.rm = TRUE)
  p95_ms      <- quantile(df$per_req_ms, 0.95, na.rm = TRUE)

  s1 <- min(100, success_pct)
  s2 <- max(0, 100 - (avg_ms / CONFIG$avg_response_max * 50))
  s3 <- max(0, 100 - (p95_ms / CONFIG$p95_response_max * 50))

  round(s1 * 0.4 + s2 * 0.3 + s3 * 0.3)
}

# =============================================================================
# OUTPUT HELPERS
# =============================================================================

ensure_dir <- function() {
  if (!dir.exists(CONFIG$output_dir)) dir.create(CONFIG$output_dir, recursive = TRUE)
}

generate_report <- function(results, level_stats, ts) {
  ensure_dir()
  f <- file.path(CONFIG$output_dir, sprintf("report_%s.md", ts))

  by_app <- results |>
    group_by(app, category) |>
    summarise(n = n(), ok = sum(success), pct = mean(success) * 100,
              avg = mean(per_req_ms), p95 = quantile(per_req_ms, 0.95),
              .groups = "drop") |>
    arrange(desc(avg))

  by_level <- results |>
    group_by(n_users) |>
    summarise(n = n(), pct = mean(success) * 100,
              avg = mean(per_req_ms), p95 = quantile(per_req_ms, 0.95),
              rps = n() / max(1, as.numeric(difftime(
                max(as.POSIXct(timestamp)), min(as.POSIXct(timestamp)), units = "secs"))),
              .groups = "drop")

  errors <- results |> filter(!success) |>
    group_by(app, error) |> summarise(n = n(), .groups = "drop") |> arrange(desc(n))

  lines <- c(
    "# MMCD Stress Test v3 Report",
    sprintf("**Date:** %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
    sprintf("**Target:** %s", CONFIG$base_url),
    sprintf("**Score:** %d / 100", score_results(results)),
    "",
    "## Summary",
    "",
    sprintf("| Metric | Value |"),
    sprintf("|--------|-------|"),
    sprintf("| Total Requests | %s |", format(nrow(results), big.mark = ",")),
    sprintf("| Success Rate | %.1f%% |", mean(results$success) * 100),
    sprintf("| Avg Response | %.0f ms |", mean(results$per_req_ms, na.rm = TRUE)),
    sprintf("| P95 Response | %.0f ms |", quantile(results$per_req_ms, 0.95, na.rm = TRUE)),
    sprintf("| Max Users Tested | %d |", max(results$n_users)),
    sprintf("| Unique Spoofed IPs | %d |", length(unique(results$spoofed_ip))),
    sprintf("| Queued Requests | %s (%.1f%%) |",
            format(sum(results$queued, na.rm = TRUE), big.mark = ","),
            sum(results$queued, na.rm = TRUE) / nrow(results) * 100),
    "",
    "## Performance by Concurrency Level",
    "",
    "| Users | Requests | Success | Avg (ms) | P95 (ms) | Req/s |",
    "|-------|----------|---------|----------|----------|-------|"
  )
  for (i in seq_len(nrow(by_level))) {
    r <- by_level[i, ]
    lines <- c(lines, sprintf("| %d | %d | %.1f%% | %.0f | %.0f | %.0f |",
                               r$n_users, r$n, r$pct, r$avg, r$p95, r$rps))
  }

  lines <- c(lines, "",
    "## Performance by App",
    "",
    "| App | Category | Requests | Success | Avg (ms) | P95 (ms) |",
    "|-----|----------|----------|---------|----------|----------|"
  )
  for (i in seq_len(nrow(by_app))) {
    r <- by_app[i, ]
    lines <- c(lines, sprintf("| %s | %s | %d | %.1f%% | %.0f | %.0f |",
                               r$app, r$category, r$n, r$pct, r$avg, r$p95))
  }

  if (nrow(errors) > 0) {
    lines <- c(lines, "", "## Errors", "",
      "| App | Error | Count |",
      "|-----|-------|-------|"
    )
    for (i in seq_len(min(20, nrow(errors)))) {
      r <- errors[i, ]
      lines <- c(lines, sprintf("| %s | %s | %d |", r$app, r$error %||% "unknown", r$n))
    }
  }

  writeLines(lines, f)
  cat(sprintf("  Report: %s\n", f))
  invisible(f)
}

generate_plots <- function(results, ts) {
  ensure_dir()

  # Plot 1: Response time by concurrency level
  p1 <- ggplot(results |> filter(success),
               aes(x = factor(n_users), y = per_req_ms)) +
    geom_boxplot(fill = "#3498db", alpha = 0.7, outlier.size = 0.5) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "#e74c3c") +
    labs(title = "Response Time by Concurrency Level",
         subtitle = "Red diamonds = mean, boxes = quartiles",
         x = "Concurrent Users", y = "Response Time (ms)") +
    theme_minimal(base_size = 12)
  ggsave(file.path(CONFIG$output_dir, sprintf("plot_concurrency_%s.png", ts)),
         p1, width = 10, height = 6, dpi = 150)

  # Plot 2: Success rate trend
  trend <- results |> group_by(n_users) |>
    summarise(pct = mean(success) * 100, .groups = "drop")

  p2 <- ggplot(trend, aes(x = n_users, y = pct)) +
    geom_line(color = "#27ae60", linewidth = 1.2) +
    geom_point(color = "#27ae60", size = 3) +
    geom_hline(yintercept = CONFIG$success_rate_min, linetype = "dashed", color = "#e74c3c") +
    labs(title = "Success Rate vs Concurrent Users",
         x = "Concurrent Users", y = "Success Rate (%)") +
    ylim(0, 100) +
    theme_minimal(base_size = 12)
  ggsave(file.path(CONFIG$output_dir, sprintf("plot_success_%s.png", ts)),
         p2, width = 10, height = 6, dpi = 150)

  # Plot 3: Per-app response times
  by_app <- results |> filter(success) |>
    group_by(app, category) |>
    summarise(avg = mean(per_req_ms), p95 = quantile(per_req_ms, 0.95), .groups = "drop")

  p3 <- ggplot(by_app, aes(x = reorder(app, avg), y = avg, fill = category)) +
    geom_col(alpha = 0.8) +
    geom_errorbar(aes(ymin = avg, ymax = p95), width = 0.3) +
    coord_flip() +
    scale_fill_manual(values = c(heavy = "#e74c3c", medium = "#f39c12", light = "#27ae60")) +
    labs(title = "Response Time by App",
         subtitle = "Bars = avg, error bars extend to P95",
         x = "", y = "Response Time (ms)") +
    theme_minimal(base_size = 12)
  ggsave(file.path(CONFIG$output_dir, sprintf("plot_apps_%s.png", ts)),
         p3, width = 10, height = 7, dpi = 150)

  cat(sprintf("  Plots: %s/plot_*.png\n", CONFIG$output_dir))
}

# =============================================================================
# STARTUP BANNER
# =============================================================================
cat("
+--------------------------------------------------------------+
|  MMCD Stress Test v3 — Load Balancer Concurrency Test        |
+--------------------------------------------------------------+
|                                                              |
|  Commands:                                                   |
|    run_stress_test()           Full test (5 → 50 users)      |
|    quick_test(10, 30)          10 users, 30 seconds          |
|    burst_test(\"Catch Basin\")   15 users → one app            |
|    verify_distribution(20)     Check routing spread           |
|                                                              |
|  How IP spoofing works:                                      |
|    Each simulated user gets a unique X-Forwarded-For header  |
|    (10.0.0.1, 10.0.0.2, ...) so the Lua load balancer sees  |
|    them as different clients and distributes accordingly.    |
|                                                              |
|  Config:                                                     |
|    CONFIG$base_url    = ", CONFIG$base_url, "
|    CONFIG$max_users   = ", CONFIG$max_users, "
|    APPS               = ", length(APPS), " endpoints
|                                                              |
+--------------------------------------------------------------+

")
