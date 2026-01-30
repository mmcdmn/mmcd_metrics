# =============================================================================
# MMCD Metrics Stress Test v2.0
# =============================================================================
# Enhanced stress test with better metrics tracking, baseline comparisons,
# and detailed per-app analysis for optimization progress tracking.
#
# Usage:
#   source("stress_test_v2.R")
#   results <- run_stress_test()
#   
# Or for quick single-level test:
#   results <- quick_test(num_users = 10, duration = 30)
# =============================================================================

library(httr2)
library(parallel)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# =============================================================================
# CONFIGURATION
# =============================================================================

CONFIG <- list(
  # Target URL
  base_url = "https://metrics.mmcd.org",
  
  # Load test parameters
  max_workers = 50,
  ramp_up_step = 5,
  test_duration_sec = 30,
  request_delay_ms = 100,
  cooldown_sec = 5,
  
  # Thresholds for pass/fail
  success_rate_threshold = 95,        # Minimum success rate %
  avg_response_threshold_ms = 2000,   # Max acceptable avg response time
  p95_response_threshold_ms = 5000,   # Max acceptable 95th percentile
  
  # Output settings
  output_dir = "stress_test_results",
  save_raw_data = TRUE
)

# Test endpoints with weights (higher = more frequent)
ENDPOINTS <- list(
  # Heavy apps (more complex queries)
  list(path = "/suco_history/", name = "SUCO History", weight = 3, category = "heavy"),
  list(path = "/drone/", name = "Drone Sites", weight = 2, category = "heavy"),
  list(path = "/struct_trt/", name = "Structure Treatments", weight = 2, category = "heavy"),
  list(path = "/cattail_treatments/", name = "Cattail Treatments", weight = 2, category = "heavy"),
  
  # Medium apps
  list(path = "/air_sites_simple/", name = "Air Sites", weight = 2, category = "medium"),
  list(path = "/ground_prehatch_progress/", name = "Ground Prehatch", weight = 2, category = "medium"),
  list(path = "/inspections/", name = "Inspections", weight = 2, category = "medium"),
  list(path = "/catch_basin_status/", name = "Catch Basin", weight = 2, category = "medium"),
  
  # Light apps
  list(path = "/control_efficacy/", name = "Control Efficacy", weight = 1, category = "light"),
  list(path = "/section-cards/", name = "Section Cards", weight = 1, category = "light")
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Create output directory if needed
ensure_output_dir <- function() {
  if (!dir.exists(CONFIG$output_dir)) {
    dir.create(CONFIG$output_dir, recursive = TRUE)
  }
}

#' Format duration in human readable form
format_duration <- function(seconds) {
  if (seconds < 60) {
    return(sprintf("%.1f seconds", seconds))
  } else if (seconds < 3600) {
    return(sprintf("%.1f minutes", seconds / 60))
  } else {
    return(sprintf("%.1f hours", seconds / 3600))
  }
}

#' Make a single HTTP request and record metrics
make_request <- function(endpoint, worker_id) {
  start_time <- Sys.time()
  
  result <- tryCatch({
    resp <- request(paste0(CONFIG$base_url, endpoint$path)) %>%
      req_timeout(30) %>%
      req_retry(max_tries = 1) %>%
      req_perform()
    
    end_time <- Sys.time()
    response_time <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
    content_length <- as.numeric(resp_headers(resp)$`content-length` %||% 0)
    
    list(
      timestamp = as.character(start_time),
      endpoint = endpoint$name,
      endpoint_category = endpoint$category,
      response_time_ms = response_time,
      status_code = resp_status(resp),
      content_length = content_length,
      success = TRUE,
      error_type = NA_character_,
      error_message = NA_character_,
      worker_id = worker_id
    )
  }, error = function(e) {
    end_time <- Sys.time()
    response_time <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
    
    # Classify error type
    error_type <- if (grepl("timeout", e$message, ignore.case = TRUE)) {
      "timeout"
    } else if (grepl("connection", e$message, ignore.case = TRUE)) {
      "connection"
    } else if (grepl("503|502|504", e$message)) {
      "server_error"
    } else {
      "other"
    }
    
    list(
      timestamp = as.character(start_time),
      endpoint = endpoint$name,
      endpoint_category = endpoint$category,
      response_time_ms = response_time,
      status_code = 0,
      content_length = 0,
      success = FALSE,
      error_type = error_type,
      error_message = substr(as.character(e$message), 1, 200),
      worker_id = worker_id
    )
  })
  
  return(result)
}

#' Worker function that simulates a user session
worker_function <- function(worker_id, duration_sec, endpoints, delay_ms) {
  end_time <- Sys.time() + duration_sec
  worker_results <- list()
  
  # Precompute weights
  weights <- sapply(endpoints, function(e) e$weight)
  
  while (Sys.time() < end_time) {
    # Select random endpoint based on weights
    selected_idx <- sample(seq_along(endpoints), size = 1, prob = weights)
    endpoint <- endpoints[[selected_idx]]
    
    # Make request
    result <- make_request(endpoint, worker_id)
    worker_results[[length(worker_results) + 1]] <- result
    
    # Delay between requests (simulate think time)
    Sys.sleep(delay_ms / 1000)
  }
  
  # Convert to data frame
  do.call(rbind, lapply(worker_results, as.data.frame, stringsAsFactors = FALSE))
}

#' Run load test at a specific concurrency level
run_load_level <- function(num_workers, duration_sec, endpoints, verbose = TRUE) {
  if (verbose) {
    cat(sprintf("\n━━━ Testing with %d concurrent users for %d seconds ━━━\n", 
                num_workers, duration_sec))
  }
  
  # Create cluster (use cores - 1 to leave room for main process)
  num_cores <- min(num_workers, max(1, detectCores() - 1))
  cl <- makeCluster(num_cores)
  
  # Export required objects
  clusterExport(cl, c("make_request", "CONFIG", "endpoints"), 
                envir = environment())
  clusterExport(cl, "%||%", envir = baseenv())
  
  # Load libraries on workers
  clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(httr2)
    })
    # Define %||% if not available
    if (!exists("%||%")) {
      `%||%` <- function(a, b) if (is.null(a)) b else a
    }
  })
  
  # Run workers
  test_start <- Sys.time()
  results_list <- parLapply(cl, seq_len(num_workers), function(id) {
    worker_function(id, duration_sec, endpoints, CONFIG$request_delay_ms)
  })
  test_end <- Sys.time()
  
  stopCluster(cl)
  
  # Combine results
  if (length(results_list) == 0 || all(sapply(results_list, is.null))) {
    return(NULL)
  }
  
  combined <- do.call(rbind, results_list)
  combined$num_workers <- num_workers
  combined$test_duration_actual <- as.numeric(difftime(test_end, test_start, units = "secs"))
  
  # Calculate summary stats
  summary_stats <- calculate_level_stats(combined)
  
  if (verbose) {
    print_level_summary(summary_stats)
  }
  
  list(
    results = combined,
    stats = summary_stats,
    should_stop = summary_stats$success_rate < 50 || summary_stats$p95_response_ms > 15000
  )
}

#' Calculate statistics for a load level
calculate_level_stats <- function(results) {
  total <- nrow(results)
  successful <- sum(results$success)
  
  list(
    num_workers = results$num_workers[1],
    total_requests = total,
    successful_requests = successful,
    failed_requests = total - successful,
    success_rate = (successful / total) * 100,
    requests_per_sec = total / results$test_duration_actual[1],
    
    avg_response_ms = mean(results$response_time_ms, na.rm = TRUE),
    median_response_ms = median(results$response_time_ms, na.rm = TRUE),
    min_response_ms = min(results$response_time_ms, na.rm = TRUE),
    max_response_ms = max(results$response_time_ms, na.rm = TRUE),
    p50_response_ms = quantile(results$response_time_ms, 0.50, na.rm = TRUE),
    p90_response_ms = quantile(results$response_time_ms, 0.90, na.rm = TRUE),
    p95_response_ms = quantile(results$response_time_ms, 0.95, na.rm = TRUE),
    p99_response_ms = quantile(results$response_time_ms, 0.99, na.rm = TRUE),
    
    test_duration = results$test_duration_actual[1]
  )
}

#' Print summary for a load level
print_level_summary <- function(stats) {
  # Status icon
  status_icon <- if (stats$success_rate >= CONFIG$success_rate_threshold) "✅" else "⚠️"
  
  cat(sprintf("\n%s Results (%d workers):\n", status_icon, stats$num_workers))
  cat(sprintf("   Requests: %d total, %d successful (%.1f%%)\n", 
              stats$total_requests, stats$successful_requests, stats$success_rate))
  cat(sprintf("   Throughput: %.1f requests/sec\n", stats$requests_per_sec))
  cat(sprintf("   Response Times: avg=%.0fms, p50=%.0fms, p95=%.0fms, p99=%.0fms\n",
              stats$avg_response_ms, stats$p50_response_ms, 
              stats$p95_response_ms, stats$p99_response_ms))
  
  if (stats$failed_requests > 0) {
    cat(sprintf(" !!!!  %d failed requests\n", stats$failed_requests))
  }
}

# =============================================================================
# ANALYSIS FUNCTIONS
# =============================================================================

#' Analyze results by endpoint
analyze_by_endpoint <- function(results) {
  results %>%
    group_by(endpoint, endpoint_category) %>%
    summarise(
      total_requests = n(),
      successful = sum(success),
      success_rate = mean(success) * 100,
      avg_response_ms = mean(response_time_ms, na.rm = TRUE),
      median_response_ms = median(response_time_ms, na.rm = TRUE),
      p95_response_ms = quantile(response_time_ms, 0.95, na.rm = TRUE),
      p99_response_ms = quantile(response_time_ms, 0.99, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_response_ms))
}

#' Analyze results by concurrency level
analyze_by_concurrency <- function(results) {
  results %>%
    group_by(num_workers) %>%
    summarise(
      total_requests = n(),
      success_rate = mean(success) * 100,
      avg_response_ms = mean(response_time_ms, na.rm = TRUE),
      median_response_ms = median(response_time_ms, na.rm = TRUE),
      p95_response_ms = quantile(response_time_ms, 0.95, na.rm = TRUE),
      requests_per_sec = n() / first(test_duration_actual),
      .groups = "drop"
    )
}

#' Analyze error patterns
analyze_errors <- function(results) {
  results %>%
    filter(!success) %>%
    group_by(endpoint, error_type) %>%
    summarise(
      count = n(),
      avg_response_time = mean(response_time_ms, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(count))
}

#' Calculate overall test score (0-100)
calculate_test_score <- function(results) {
  success_rate <- mean(results$success) * 100
  avg_response <- mean(results$response_time_ms, na.rm = TRUE)
  p95_response <- quantile(results$response_time_ms, 0.95, na.rm = TRUE)
  
  # Score components (each 0-100)
  success_score <- min(100, success_rate)
  response_score <- max(0, 100 - (avg_response / CONFIG$avg_response_threshold_ms * 50))
  p95_score <- max(0, 100 - (p95_response / CONFIG$p95_response_threshold_ms * 50))
  
  # Weighted average
  round((success_score * 0.4) + (response_score * 0.3) + (p95_score * 0.3))
}

# =============================================================================
# REPORTING FUNCTIONS
# =============================================================================

#' Generate comprehensive report
generate_report <- function(results, timestamp) {
  ensure_output_dir()
  
  report_file <- file.path(CONFIG$output_dir, 
                           sprintf("stress_test_report_%s.md", timestamp))
  
  by_endpoint <- analyze_by_endpoint(results)
  by_concurrency <- analyze_by_concurrency(results)
  errors <- analyze_errors(results)
  score <- calculate_test_score(results)
  
  # Generate markdown report
  lines <- c(
    "# MMCD Metrics Stress Test Report",
    sprintf("**Generated:** %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("**Target:** %s", CONFIG$base_url),
    "",
    "---",
    "",
    "## Executive Summary",
    "",
    sprintf("| Metric | Value | Status |"),
    sprintf("|--------|-------|--------|"),
    sprintf("| **Overall Score** | %d/100 | %s |", score, 
            if (score >= 80) "Excellent" else if (score >= 60) " Needs Work" else " Poor"),
    sprintf("| **Total Requests** | %s | |", format(nrow(results), big.mark = ",")),
    sprintf("| **Success Rate** | %.1f%% | %s |", 
            mean(results$success) * 100,
            if (mean(results$success) >= 0.95) "✅" else "⚠️"),
    sprintf("| **Avg Response** | %.0f ms | %s |",
            mean(results$response_time_ms, na.rm = TRUE),
            if (mean(results$response_time_ms, na.rm = TRUE) <= CONFIG$avg_response_threshold_ms) "✅" else "⚠️"),
    sprintf("| **95th Percentile** | %.0f ms | %s |",
            quantile(results$response_time_ms, 0.95, na.rm = TRUE),
            if (quantile(results$response_time_ms, 0.95, na.rm = TRUE) <= CONFIG$p95_response_threshold_ms) "✅" else "⚠️"),
    sprintf("| **Max Concurrent Users** | %d | |", max(results$num_workers)),
    "",
    "---",
    "",
    "## Performance by Endpoint",
    "",
    "| Endpoint | Category | Requests | Success % | Avg (ms) | P95 (ms) |",
    "|----------|----------|----------|-----------|----------|----------|"
  )
  
  for (i in seq_len(nrow(by_endpoint))) {
    row <- by_endpoint[i, ]
    lines <- c(lines, sprintf("| %s | %s | %d | %.1f%% | %.0f | %.0f |",
                               row$endpoint, row$endpoint_category,
                               row$total_requests, row$success_rate,
                               row$avg_response_ms, row$p95_response_ms))
  }
  
  lines <- c(lines, "", "---", "",
             "## Performance by Concurrency Level",
             "",
             "| Users | Requests | Success % | Avg (ms) | P95 (ms) | Req/sec |",
             "|-------|----------|-----------|----------|----------|---------|")
  
  for (i in seq_len(nrow(by_concurrency))) {
    row <- by_concurrency[i, ]
    lines <- c(lines, sprintf("| %d | %d | %.1f%% | %.0f | %.0f | %.1f |",
                               row$num_workers, row$total_requests,
                               row$success_rate, row$avg_response_ms,
                               row$p95_response_ms, row$requests_per_sec))
  }
  
  if (nrow(errors) > 0) {
    lines <- c(lines, "", "---", "",
               "## Error Analysis",
               "",
               "| Endpoint | Error Type | Count |",
               "|----------|------------|-------|")
    
    for (i in seq_len(min(20, nrow(errors)))) {
      row <- errors[i, ]
      lines <- c(lines, sprintf("| %s | %s | %d |",
                                 row$endpoint, row$error_type, row$count))
    }
  }
  
  lines <- c(lines, "", "---", "",
             "## Recommendations",
             "")
  
  # Generate recommendations based on data
  slow_endpoints <- by_endpoint %>% filter(avg_response_ms > 1000)
  if (nrow(slow_endpoints) > 0) {
    lines <- c(lines, sprintf("### Slow Endpoints (>1s avg response)"))
    for (i in seq_len(nrow(slow_endpoints))) {
      lines <- c(lines, sprintf("- **%s**: %.0fms avg - consider caching or query optimization",
                                 slow_endpoints$endpoint[i], slow_endpoints$avg_response_ms[i]))
    }
    lines <- c(lines, "")
  }
  
  failed_endpoints <- by_endpoint %>% filter(success_rate < 99)
  if (nrow(failed_endpoints) > 0) {
    lines <- c(lines, "### Unreliable Endpoints (<99% success)")
    for (i in seq_len(nrow(failed_endpoints))) {
      lines <- c(lines, sprintf("- **%s**: %.1f%% success rate - investigate errors",
                                 failed_endpoints$endpoint[i], failed_endpoints$success_rate[i]))
    }
    lines <- c(lines, "")
  }
  
  writeLines(lines, report_file)
  cat(sprintf(" Report saved: %s\n", report_file))
  
  invisible(report_file)
}

#' Generate comparison report (vs baseline)
generate_comparison_report <- function(current_results, baseline_file, timestamp) {
  if (!file.exists(baseline_file)) {
    cat(" Baseline file not found, skipping comparison\n")
    return(invisible(NULL))
  }
  
  baseline_results <- readRDS(baseline_file)
  
  current_by_endpoint <- analyze_by_endpoint(current_results)
  baseline_by_endpoint <- analyze_by_endpoint(baseline_results)
  
  comparison <- current_by_endpoint %>%
    left_join(baseline_by_endpoint, by = "endpoint", suffix = c("_new", "_old")) %>%
    mutate(
      response_change_pct = (avg_response_ms_new - avg_response_ms_old) / avg_response_ms_old * 100,
      success_change = success_rate_new - success_rate_old
    ) %>%
    select(endpoint, 
           avg_response_ms_old, avg_response_ms_new, response_change_pct,
           success_rate_old, success_rate_new, success_change)
  
  ensure_output_dir()
  comparison_file <- file.path(CONFIG$output_dir, 
                                sprintf("comparison_%s.csv", timestamp))
  write.csv(comparison, comparison_file, row.names = FALSE)
  cat(sprintf(" Comparison saved: %s\n", comparison_file))
  
  # Print summary
  cat("\n Performance Comparison vs Baseline:\n")
  cat("─────────────────────────────────────────\n")
  
  for (i in seq_len(nrow(comparison))) {
    row <- comparison[i, ]
    change_icon <- if (row$response_change_pct < -5) "🟢" 
                   else if (row$response_change_pct > 5) "🔴" 
                   else "🟡"
    cat(sprintf("%s %s: %.0fms → %.0fms (%+.1f%%)\n",
                change_icon, row$endpoint,
                row$avg_response_ms_old, row$avg_response_ms_new,
                row$response_change_pct))
  }
  
  invisible(comparison)
}

#' Generate plots
generate_plots <- function(results, timestamp) {
  ensure_output_dir()
  
  # Plot 1: Response time distribution by concurrency
  p1 <- ggplot(results %>% filter(success), 
               aes(x = factor(num_workers), y = response_time_ms)) +
    geom_boxplot(fill = "#3498db", alpha = 0.7, outlier.size = 0.5) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "#e74c3c") +
    labs(title = "Response Time by Concurrency Level",
         subtitle = "Red diamonds show mean, boxes show quartiles",
         x = "Concurrent Users",
         y = "Response Time (ms)") +
    theme_minimal() +
    theme(text = element_text(size = 11))
  
  ggsave(file.path(CONFIG$output_dir, sprintf("plot_concurrency_%s.png", timestamp)),
         p1, width = 10, height = 6, dpi = 150)
  
  # Plot 2: Success rate trend
  success_trend <- results %>%
    group_by(num_workers) %>%
    summarise(success_rate = mean(success) * 100, .groups = "drop")
  
  p2 <- ggplot(success_trend, aes(x = num_workers, y = success_rate)) +
    geom_line(color = "#27ae60", size = 1.5) +
    geom_point(color = "#27ae60", size = 3) +
    geom_hline(yintercept = 95, linetype = "dashed", color = "#e74c3c", size = 0.8) +
    annotate("text", x = max(success_trend$num_workers), y = 96, 
             label = "95% threshold", hjust = 1, color = "#e74c3c") +
    labs(title = "Success Rate vs Concurrent Users",
         x = "Concurrent Users",
         y = "Success Rate (%)") +
    ylim(0, 100) +
    theme_minimal()
  
  ggsave(file.path(CONFIG$output_dir, sprintf("plot_success_%s.png", timestamp)),
         p2, width = 10, height = 6, dpi = 150)
  
  # Plot 3: Endpoint comparison (horizontal bar)
  by_endpoint <- analyze_by_endpoint(results)
  
  p3 <- ggplot(by_endpoint, 
               aes(x = reorder(endpoint, avg_response_ms), y = avg_response_ms, 
                   fill = endpoint_category)) +
    geom_col(alpha = 0.8) +
    geom_errorbar(aes(ymin = avg_response_ms, ymax = p95_response_ms), 
                  width = 0.2, color = "#2c3e50") +
    coord_flip() +
    scale_fill_manual(values = c("heavy" = "#e74c3c", "medium" = "#f39c12", "light" = "#27ae60")) +
    labs(title = "Response Time by Endpoint",
         subtitle = "Bars show average, error bars extend to 95th percentile",
         x = "",
         y = "Response Time (ms)",
         fill = "Category") +
    theme_minimal()
  
  ggsave(file.path(CONFIG$output_dir, sprintf("plot_endpoints_%s.png", timestamp)),
         p3, width = 10, height = 8, dpi = 150)
  
  # Plot 4: Response time heatmap over time
  results_time <- results %>%
    mutate(minute = floor(as.numeric(difftime(as.POSIXct(timestamp), 
                                               min(as.POSIXct(timestamp)), 
                                               units = "mins"))))
  
  heatmap_data <- results_time %>%
    group_by(endpoint, minute) %>%
    summarise(avg_response = mean(response_time_ms, na.rm = TRUE), .groups = "drop")
  
  p4 <- ggplot(heatmap_data, aes(x = minute, y = endpoint, fill = avg_response)) +
    geom_tile() +
    scale_fill_gradient2(low = "#27ae60", mid = "#f1c40f", high = "#e74c3c",
                         midpoint = 1000) +
    labs(title = "Response Time Heatmap Over Test Duration",
         x = "Minutes into Test",
         y = "",
         fill = "Avg Response (ms)") +
    theme_minimal()
  
  ggsave(file.path(CONFIG$output_dir, sprintf("plot_heatmap_%s.png", timestamp)),
         p4, width = 12, height = 8, dpi = 150)
  
  cat("Plots saved to", CONFIG$output_dir, "\n")
}

# =============================================================================
# MAIN TEST FUNCTIONS
# =============================================================================

#' Run quick test at single concurrency level
quick_test <- function(num_users = 10, duration = 30, verbose = TRUE) {
  cat(" Quick Stress Test\n")
  cat(sprintf("   Target: %s\n", CONFIG$base_url))
  cat(sprintf("   Users: %d, Duration: %ds\n\n", num_users, duration))
  
  # Check connectivity
  tryCatch({
    request(CONFIG$base_url) %>% req_timeout(10) %>% req_perform()
    cat(" Site accessible\n")
  }, error = function(e) {
    cat(" Cannot reach site:", e$message, "\n")
    return(NULL)
  })
  
  result <- run_load_level(num_users, duration, ENDPOINTS, verbose = verbose)
  
  if (!is.null(result$results)) {
    score <- calculate_test_score(result$results)
    cat(sprintf("\n Overall Score: %d/100\n", score))
  }
  
  invisible(result$results)
}

#' Run full progressive stress test
run_stress_test <- function(save_baseline = FALSE, compare_baseline = NULL) {
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║         MMCD Metrics Stress Test v2.0                       ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  cat("Configuration:\n")
  cat(sprintf("  Target URL: %s\n", CONFIG$base_url))
  cat(sprintf("  Max Workers: %d\n", CONFIG$max_workers))
  cat(sprintf("  Ramp-up Step: %d workers\n", CONFIG$ramp_up_step))
  cat(sprintf("  Test Duration: %d sec per level\n", CONFIG$test_duration_sec))
  cat(sprintf("  Endpoints: %d\n", length(ENDPOINTS)))
  cat(sprintf("  Success Threshold: %.0f%%\n", CONFIG$success_rate_threshold))
  cat(sprintf("  Response Threshold: %.0fms avg, %.0fms p95\n\n", 
              CONFIG$avg_response_threshold_ms, CONFIG$p95_response_threshold_ms))
  
  # Check connectivity
  cat(" Checking connectivity...\n")
  tryCatch({
    resp <- request(CONFIG$base_url) %>% req_timeout(10) %>% req_perform()
    cat(" Site accessible\n")
  }, error = function(e) {
    cat(" Cannot reach site:", e$message, "\n")
    cat("Aborting test.\n")
    return(NULL)
  })
  
  Sys.sleep(2)
  
  # Run progressive load test
  all_results <- list()
  num_workers <- CONFIG$ramp_up_step
  
  test_start_time <- Sys.time()
  
  while (num_workers <= CONFIG$max_workers) {
    level_result <- run_load_level(num_workers, CONFIG$test_duration_sec, ENDPOINTS)
    
    if (!is.null(level_result$results)) {
      all_results[[length(all_results) + 1]] <- level_result$results
    }
    
    if (level_result$should_stop) {
      cat("\n Stopping test - performance degradation detected\n")
      break
    }
    
    # Cooldown
    if (num_workers < CONFIG$max_workers) {
      cat(sprintf("  Cooldown %ds before next level...\n", CONFIG$cooldown_sec))
      Sys.sleep(CONFIG$cooldown_sec)
    }
    
    num_workers <- num_workers + CONFIG$ramp_up_step
  }
  
  test_end_time <- Sys.time()
  total_duration <- as.numeric(difftime(test_end_time, test_start_time, units = "secs"))
  
  # Combine all results
  if (length(all_results) == 0) {
    cat(" No results collected\n")
    return(NULL)
  }
  
  final_results <- do.call(rbind, all_results)
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("                     FINAL SUMMARY\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  
  score <- calculate_test_score(final_results)
  cat(sprintf("\n Overall Score: %d/100\n", score))
  cat(sprintf(" Total Requests: %s\n", format(nrow(final_results), big.mark = ",")))
  cat(sprintf(" Success Rate: %.1f%%\n", mean(final_results$success) * 100))
  cat(sprintf(" Avg Response: %.0f ms\n", mean(final_results$response_time_ms, na.rm = TRUE)))
  cat(sprintf(" 95th Percentile: %.0f ms\n", quantile(final_results$response_time_ms, 0.95, na.rm = TRUE)))
  cat(sprintf(" Total Test Duration: %s\n", format_duration(total_duration)))
  
  # Save results
  ensure_output_dir()
  
  if (CONFIG$save_raw_data) {
    data_file <- file.path(CONFIG$output_dir, sprintf("raw_results_%s.rds", timestamp))
    saveRDS(final_results, data_file)
    cat(sprintf("\n Raw data saved: %s\n", data_file))
  }
  
  csv_file <- file.path(CONFIG$output_dir, sprintf("results_%s.csv", timestamp))
  write.csv(final_results, csv_file, row.names = FALSE)
  cat(sprintf(" CSV saved: %s\n", csv_file))
  
  # Generate reports and plots
  generate_report(final_results, timestamp)
  generate_plots(final_results, timestamp)
  
  # Save as baseline if requested
  if (save_baseline) {
    baseline_file <- file.path(CONFIG$output_dir, "baseline.rds")
    saveRDS(final_results, baseline_file)
    cat(sprintf(" Saved as baseline: %s\n", baseline_file))
  }
  
  # Compare to baseline if provided
  if (!is.null(compare_baseline)) {
    generate_comparison_report(final_results, compare_baseline, timestamp)
  } else {
    # Auto-compare to default baseline if it exists
    default_baseline <- file.path(CONFIG$output_dir, "baseline.rds")
    if (file.exists(default_baseline)) {
      generate_comparison_report(final_results, default_baseline, timestamp)
    }
  }
  
  cat("\n Stress test complete!\n")
  
  invisible(final_results)
}

#' Test a single endpoint in isolation
test_endpoint <- function(endpoint_name, num_requests = 50) {
  endpoint <- Filter(function(e) e$name == endpoint_name, ENDPOINTS)
  
  if (length(endpoint) == 0) {
    cat(" Endpoint not found:", endpoint_name, "\n")
    cat("Available endpoints:", paste(sapply(ENDPOINTS, function(e) e$name), collapse = ", "), "\n")
    return(NULL)
  }
  
  endpoint <- endpoint[[1]]
  cat(sprintf(" Testing endpoint: %s (%s)\n", endpoint$name, endpoint$path))
  cat(sprintf("   Making %d sequential requests...\n\n", num_requests))
  
  results <- lapply(seq_len(num_requests), function(i) {
    result <- make_request(endpoint, worker_id = 1)
    cat(sprintf("  [%d/%d] %s - %dms\n", i, num_requests, 
                if (result$success) "✅" else "❌",
                round(result$response_time_ms)))
    as.data.frame(result, stringsAsFactors = FALSE)
  })
  
  combined <- do.call(rbind, results)
  
  cat("\n─────────────────────────────────\n")
  cat(sprintf("Success Rate: %.1f%%\n", mean(combined$success) * 100))
  cat(sprintf("Avg Response: %.0f ms\n", mean(combined$response_time_ms)))
  cat(sprintf("Min/Max: %.0f / %.0f ms\n", 
              min(combined$response_time_ms), max(combined$response_time_ms)))
  cat(sprintf("P50/P95/P99: %.0f / %.0f / %.0f ms\n",
              quantile(combined$response_time_ms, 0.5),
              quantile(combined$response_time_ms, 0.95),
              quantile(combined$response_time_ms, 0.99)))
  
  invisible(combined)
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================
cat("
╔══════════════════════════════════════════════════════════════╗
║  MMCD Stress Test v2.0 Loaded                                ║
╠══════════════════════════════════════════════════════════════╣
║  Commands:                                                   ║
║                                                              ║
║  run_stress_test()              Full progressive test        ║
║  run_stress_test(save_baseline = TRUE)  Save as baseline     ║
║  quick_test(10, 30)             Quick 10-user, 30-sec test   ║
║  test_endpoint(\"SUCO History\")  Test single endpoint         ║
║                                                              ║
║  Results saved to: stress_test_results/                      ║
╚══════════════════════════════════════════════════════════════╝

")
