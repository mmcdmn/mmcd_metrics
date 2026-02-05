# Integration test: Verify server output binding exists and is correctly wired

cat("Running server output binding test...\n\n")

server_path <- file.path("apps", "overview", "dynamic_server.R")
server_lines <- readLines(server_path, warn = FALSE)
server_text <- paste(server_lines, collapse = "\n")

# Test 1: output$facility_detail_boxes should exist
cat("Test 1: Server output binding\n")
output_pattern <- 'output\\$facility_detail_boxes\\s*<-\\s*renderUI'
has_output <- grepl(output_pattern, server_text)
if (!has_output) {
  stop("FAIL: Missing output$facility_detail_boxes <- renderUI(...)")
}
cat("  PASS: output$facility_detail_boxes binding found\n\n")

# Test 2: Should use input$selected_facility
cat("Test 2: Input dependency\n")
uses_input <- grepl('selected_facility', server_text, fixed = TRUE)
if (!uses_input) {
  stop("FAIL: output$facility_detail_boxes should use input$selected_facility")
}
cat("  PASS: Uses input$selected_facility\n\n")

# Test 3: Should call generate_facility_detail_boxes
cat("Test 3: Generator function call\n")
calls_generator <- grepl('generate_facility_detail_boxes\\s*\\(', server_text)
if (!calls_generator) {
  stop("FAIL: Should call generate_facility_detail_boxes()")
}
cat("  PASS: Calls generate_facility_detail_boxes()\n\n")

# Test 4: Should use metrics_filter
cat("Test 4: Metrics filter usage\n")
uses_filter <- grepl('metrics_filter', server_text, fixed = TRUE)
if (!uses_filter) {
  stop("FAIL: Should use metrics_filter to determine which metric is being viewed")
}
cat("  PASS: Uses metrics_filter\n\n")

# Test 5: Should handle NULL cases
cat("Test 5: NULL handling\n")
handles_null_inputs <- grepl('is\\.null\\(inputs\\)', server_text)
handles_null_filter <- grepl('is\\.null\\(metrics_filter\\)', server_text)
if (!handles_null_inputs || !handles_null_filter) {
  stop("FAIL: Should handle NULL inputs and metrics_filter gracefully")
}
cat("  PASS: Handles NULL cases\n\n")

# Test 6: generate_facility_detail_boxes should exist as function
cat("Test 6: Generator function definition\n")
func_definition <- grepl('generate_facility_detail_boxes\\s*<-\\s*function', server_text)
if (!func_definition) {
  stop("FAIL: generate_facility_detail_boxes function not defined")
}
cat("  PASS: Generator function defined\n\n")

# Test 7: Generator should call load_data_by_facility
cat("Test 7: Data loading\n")
loads_data <- grepl('load_data_by_facility\\s*\\(', server_text)
if (!loads_data) {
  stop("FAIL: Generator should call load_data_by_facility()")
}
cat("  PASS: Calls load_data_by_facility()\n\n")

# Test 8: Generator should use create_stat_box
cat("Test 8: Stat box creation\n")
creates_boxes <- grepl('create_stat_box\\s*\\(', server_text)
if (!creates_boxes) {
  stop("FAIL: Generator should call create_stat_box()")
}
cat("  PASS: Calls create_stat_box()\n\n")

cat("=== ALL SERVER BINDING TESTS PASSED ===\n")
