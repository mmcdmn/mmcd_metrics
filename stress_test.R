# MMCD Metrics Stress Test Script
# Tests deployed application at https://metrics.mmcd.org
# Gradually increases load until failure or timeout

library(httr2)
library(jsonlite)
library(parallel)
library(ggplot2)
library(dplyr)
library(lubridate)

# Configuration
BASE_URL <- "https://metrics.mmcd.org"
MAX_WORKERS <- 50
RAMP_UP_STEP <- 5  # Add this many workers each round
TEST_DURATION_SEC <- 30  # How long each load level runs
REQUEST_DELAY_MS <- 100  # Delay between requests per worker

# Test endpoints - main apps that are likely to be used
ENDPOINTS <- list(
  list(path = "/suco_history/", name = "SUCO History", weight = 3),
  list(path = "/cattail_treatments/", name = "Cattail Treatments", weight = 2),
  list(path = "/struct_trt/", name = "Structure Treatments", weight = 2),
  list(path = "/drone/", name = "Drone Sites", weight = 2),
  list(path = "/air_sites_simple/", name = "Air Sites", weight = 1),
  list(path = "/ground_prehatch_progress/", name = "Ground Prehatch", weight = 1),
  list(path = "/inspections/", name = "Inspections", weight = 1),
  list(path = "/catch_basin_status/", name = "Catch Basin", weight = 1),
  list(path = "/section-cards/", name = "Section Cards", weight = 1),
  list(path = "/control_efficacy/", name = "Control Efficacy", weight = 1)
)

# Initialize results storage
test_results <- data.frame(
  timestamp = as.POSIXct(character()),
  num_workers = integer(),
  endpoint = character(),
  response_time_ms = numeric(),
  status_code = integer(),
  success = logical(),
  error_message = character(),
  stringsAsFactors = FALSE
)

# Function to make a single request
make_request <- function(endpoint, worker_id) {
  start_time <- Sys.time()
  
  result <- tryCatch({
    resp <- request(paste0(BASE_URL, endpoint$path)) %>%
      req_timeout(30) %>%  # 30 second timeout
      req_perform()
    
    end_time <- Sys.time()
    response_time <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
    
    list(
      timestamp = start_time,
      endpoint = endpoint$name,
      response_time_ms = response_time,
      status_code = resp_status(resp),
      success = TRUE,
      error_message = NA
    )
  }, error = function(e) {
    end_time <- Sys.time()
    response_time <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
    
    list(
      timestamp = start_time,
      endpoint = endpoint$name,
      response_time_ms = response_time,
      status_code = 0,
      success = FALSE,
      error_message = as.character(e$message)
    )
  })
  
  return(result)
}

# Function to simulate a single worker
worker_function <- function(worker_id, duration_sec, endpoints) {
  end_time <- Sys.time() + duration_sec
  worker_results <- list()
  request_count <- 0
  
  while (Sys.time() < end_time) {
    # Select endpoint based on weights
    weights <- sapply(endpoints, function(e) e$weight)
    selected <- sample(1:length(endpoints), size = 1, prob = weights)
    endpoint <- endpoints[[selected]]
    
    # Make request
    result <- make_request(endpoint, worker_id)
    result$worker_id <- worker_id
    worker_results[[length(worker_results) + 1]] <- result
    request_count <- request_count + 1
    
    # Small delay between requests
    Sys.sleep(REQUEST_DELAY_MS / 1000)
  }
  
  cat(sprintf("Worker %d completed %d requests\n", worker_id, request_count))
  return(do.call(rbind, lapply(worker_results, as.data.frame)))
}

# Function to run load test at specific concurrency level
run_load_test <- function(num_workers, duration_sec, endpoints) {
  cat(sprintf("\n=== Testing with %d concurrent workers for %d seconds ===\n", 
              num_workers, duration_sec))
  cat(sprintf("Started at: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  
  # Create cluster for parallel execution
  cl <- makeCluster(min(num_workers, detectCores() - 1))
  
  # Export necessary objects to cluster
  clusterExport(cl, c("worker_function", "make_request", "BASE_URL", 
                      "REQUEST_DELAY_MS", "endpoints"), 
                envir = environment())
  
  # Load required libraries on each worker
  clusterEvalQ(cl, {
    library(httr2)
    library(jsonlite)
  })
  
  # Run workers in parallel
  start_time <- Sys.time()
  results_list <- parLapply(cl, 1:num_workers, function(id) {
    worker_function(id, duration_sec, endpoints)
  })
  end_time <- Sys.time()
  
  # Stop cluster
  stopCluster(cl)
  
  # Combine results
  if (length(results_list) > 0) {
    combined_results <- do.call(rbind, results_list)
    combined_results$num_workers <- num_workers
    
    # Calculate statistics
    total_requests <- nrow(combined_results)
    successful_requests <- sum(combined_results$success)
    failed_requests <- total_requests - successful_requests
    avg_response_time <- mean(combined_results$response_time_ms, na.rm = TRUE)
    median_response_time <- median(combined_results$response_time_ms, na.rm = TRUE)
    p95_response_time <- quantile(combined_results$response_time_ms, 0.95, na.rm = TRUE)
    p99_response_time <- quantile(combined_results$response_time_ms, 0.99, na.rm = TRUE)
    success_rate <- (successful_requests / total_requests) * 100
    
    actual_duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
    requests_per_sec <- total_requests / actual_duration
    
    cat(sprintf("\nResults:\n"))
    cat(sprintf("  Total Requests: %d\n", total_requests))
    cat(sprintf("  Successful: %d (%.1f%%)\n", successful_requests, success_rate))
    cat(sprintf("  Failed: %d\n", failed_requests))
    cat(sprintf("  Requests/sec: %.2f\n", requests_per_sec))
    cat(sprintf("  Avg Response Time: %.0f ms\n", avg_response_time))
    cat(sprintf("  Median Response Time: %.0f ms\n", median_response_time))
    cat(sprintf("  95th Percentile: %.0f ms\n", p95_response_time))
    cat(sprintf("  99th Percentile: %.0f ms\n", p99_response_time))
    
    if (failed_requests > 0) {
      cat("\nError Summary:\n")
      error_summary <- combined_results %>%
        filter(!success) %>%
        group_by(error_message) %>%
        summarise(count = n(), .groups = "drop") %>%
        arrange(desc(count))
      print(error_summary)
    }
    
    # Check if we should stop (too many failures)
    if (success_rate < 50) {
      cat("\n⚠️  WARNING: Success rate below 50%. Consider stopping test.\n")
      return(list(results = combined_results, should_stop = TRUE))
    }
    
    if (p95_response_time > 10000) {  # 10 seconds
      cat("\n⚠️  WARNING: 95th percentile response time exceeds 10s. Severe degradation.\n")
      return(list(results = combined_results, should_stop = TRUE))
    }
    
    return(list(results = combined_results, should_stop = FALSE))
  } else {
    cat("No results collected\n")
    return(list(results = NULL, should_stop = TRUE))
  }
}

# Main stress test execution
run_stress_test <- function() {
  cat("╔════════════════════════════════════════════════════════╗\n")
  cat("║   MMCD Metrics Application Stress Test                ║\n")
  cat("║   Target: https://metrics.mmcd.org                     ║\n")
  cat("╚════════════════════════════════════════════════════════╝\n\n")
  
  cat("Configuration:\n")
  cat(sprintf("  Max Workers: %d\n", MAX_WORKERS))
  cat(sprintf("  Ramp-up Step: %d workers\n", RAMP_UP_STEP))
  cat(sprintf("  Test Duration per Level: %d seconds\n", TEST_DURATION_SEC))
  cat(sprintf("  Request Delay: %d ms\n", REQUEST_DELAY_MS))
  cat(sprintf("  Testing %d endpoints\n", length(ENDPOINTS)))
  
  cat("\nTesting endpoints:\n")
  for (endpoint in ENDPOINTS) {
    cat(sprintf("  - %s (weight: %d)\n", endpoint$name, endpoint$weight))
  }
  
  # Check if site is accessible
  cat("\n📡 Checking site accessibility...\n")
  tryCatch({
    resp <- request(BASE_URL) %>% req_perform()
    cat("✓ Site is accessible\n")
  }, error = function(e) {
    cat("✗ Cannot reach site. Error:", e$message, "\n")
    cat("Aborting test.\n")
    return(NULL)
  })
  
  Sys.sleep(2)
  
  # Progressive load test
  all_results <- list()
  num_workers <- RAMP_UP_STEP
  
  while (num_workers <= MAX_WORKERS) {
    test_result <- run_load_test(num_workers, TEST_DURATION_SEC, ENDPOINTS)
    
    if (!is.null(test_result$results)) {
      all_results[[length(all_results) + 1]] <- test_result$results
    }
    
    if (test_result$should_stop) {
      cat("\n🛑 Stopping test due to performance degradation or high failure rate\n")
      break
    }
    
    # Cooldown between test levels
    if (num_workers < MAX_WORKERS) {
      cat(sprintf("\n⏸️  Cooldown for 5 seconds before next level...\n"))
      Sys.sleep(5)
    }
    
    num_workers <- num_workers + RAMP_UP_STEP
  }
  
  # Combine all results
  if (length(all_results) > 0) {
    final_results <- do.call(rbind, all_results)
    
    # Save results
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    results_file <- paste0("stress_test_results_", timestamp, ".csv")
    write.csv(final_results, results_file, row.names = FALSE)
    cat(sprintf("\n💾 Results saved to: %s\n", results_file))
    
    # Generate summary report
    generate_summary_report(final_results, timestamp)
    
    # Generate plots
    generate_plots(final_results, timestamp)
    
    return(final_results)
  }
}

# Generate summary report
generate_summary_report <- function(results, timestamp) {
  report_file <- paste0("stress_test_report_", timestamp, ".txt")
  
  sink(report_file)
  cat("═══════════════════════════════════════════════════════════\n")
  cat("  MMCD Metrics Stress Test - Summary Report\n")
  cat("═══════════════════════════════════════════════════════════\n\n")
  cat(sprintf("Generated: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  cat(sprintf("Target: %s\n\n", BASE_URL))
  
  # Overall statistics
  cat("Overall Statistics:\n")
  cat("-----------------------------------------------------------\n")
  cat(sprintf("Total Requests: %d\n", nrow(results)))
  cat(sprintf("Successful: %d (%.1f%%)\n", 
              sum(results$success), 
              sum(results$success) / nrow(results) * 100))
  cat(sprintf("Failed: %d (%.1f%%)\n", 
              sum(!results$success),
              sum(!results$success) / nrow(results) * 100))
  
  cat("\nResponse Time Statistics (ms):\n")
  cat(sprintf("  Min: %.0f\n", min(results$response_time_ms, na.rm = TRUE)))
  cat(sprintf("  Max: %.0f\n", max(results$response_time_ms, na.rm = TRUE)))
  cat(sprintf("  Mean: %.0f\n", mean(results$response_time_ms, na.rm = TRUE)))
  cat(sprintf("  Median: %.0f\n", median(results$response_time_ms, na.rm = TRUE)))
  cat(sprintf("  95th Percentile: %.0f\n", quantile(results$response_time_ms, 0.95, na.rm = TRUE)))
  cat(sprintf("  99th Percentile: %.0f\n", quantile(results$response_time_ms, 0.99, na.rm = TRUE)))
  
  # By concurrency level
  cat("\n\nPerformance by Concurrency Level:\n")
  cat("-----------------------------------------------------------\n")
  by_workers <- results %>%
    group_by(num_workers) %>%
    summarise(
      total = n(),
      success_rate = sum(success) / n() * 100,
      avg_resp = mean(response_time_ms, na.rm = TRUE),
      p95_resp = quantile(response_time_ms, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
  print(by_workers)
  
  # By endpoint
  cat("\n\nPerformance by Endpoint:\n")
  cat("-----------------------------------------------------------\n")
  by_endpoint <- results %>%
    group_by(endpoint) %>%
    summarise(
      total = n(),
      success_rate = sum(success) / n() * 100,
      avg_resp = mean(response_time_ms, na.rm = TRUE),
      p95_resp = quantile(response_time_ms, 0.95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_resp))
  print(by_endpoint)
  
  # Failure analysis
  if (sum(!results$success) > 0) {
    cat("\n\nFailure Analysis:\n")
    cat("-----------------------------------------------------------\n")
    failure_summary <- results %>%
      filter(!success) %>%
      group_by(error_message, endpoint) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    print(failure_summary)
  }
  
  cat("\n═══════════════════════════════════════════════════════════\n")
  sink()
  
  cat(sprintf("📄 Report saved to: %s\n", report_file))
}

# Generate visualization plots
generate_plots <- function(results, timestamp) {
  # Plot 1: Response time by concurrency
  p1 <- ggplot(results %>% filter(success), 
               aes(x = as.factor(num_workers), y = response_time_ms)) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    labs(title = "Response Time Distribution by Concurrency Level",
         x = "Number of Workers",
         y = "Response Time (ms)") +
    theme_minimal() +
    theme(text = element_text(size = 12))
  
  plot_file_1 <- paste0("plot_response_time_", timestamp, ".png")
  ggsave(plot_file_1, p1, width = 10, height = 6, dpi = 300)
  
  # Plot 2: Success rate by concurrency
  success_by_workers <- results %>%
    group_by(num_workers) %>%
    summarise(success_rate = sum(success) / n() * 100, .groups = "drop")
  
  p2 <- ggplot(success_by_workers, aes(x = num_workers, y = success_rate)) +
    geom_line(color = "darkgreen", size = 1.5) +
    geom_point(color = "darkgreen", size = 3) +
    geom_hline(yintercept = 95, linetype = "dashed", color = "red") +
    labs(title = "Success Rate vs Concurrency Level",
         x = "Number of Workers",
         y = "Success Rate (%)") +
    ylim(0, 100) +
    theme_minimal() +
    theme(text = element_text(size = 12))
  
  plot_file_2 <- paste0("plot_success_rate_", timestamp, ".png")
  ggsave(plot_file_2, p2, width = 10, height = 6, dpi = 300)
  
  # Plot 3: Endpoint comparison
  p3 <- ggplot(results %>% filter(success), 
               aes(x = reorder(endpoint, response_time_ms, FUN = median), 
                   y = response_time_ms)) +
    geom_boxplot(fill = "coral", alpha = 0.7) +
    coord_flip() +
    labs(title = "Response Time by Endpoint",
         x = "Endpoint",
         y = "Response Time (ms)") +
    theme_minimal() +
    theme(text = element_text(size = 12))
  
  plot_file_3 <- paste0("plot_endpoint_comparison_", timestamp, ".png")
  ggsave(plot_file_3, p3, width = 10, height = 8, dpi = 300)
  
  cat(sprintf("📊 Plots saved:\n"))
  cat(sprintf("   - %s\n", plot_file_1))
  cat(sprintf("   - %s\n", plot_file_2))
  cat(sprintf("   - %s\n", plot_file_3))
}

# Run the stress test
cat("\n⚠️  WARNING: This will generate significant load on the production server.\n")
cat("Make sure you have permission to run this test.\n\n")
cat("Press ENTER to continue or Ctrl+C to cancel...\n")
readline()

results <- run_stress_test()

cat("\n✅ Stress test complete!\n")
